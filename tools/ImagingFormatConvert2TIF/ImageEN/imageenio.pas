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
File version 1067
Doc revision 1004
*)

unit imageenio;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$R-}
{$Q-}

{$I ie.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ieview,
  ImageEnProc, ExtCtrls, hyiedefs, hyieutils, Dialogs,
  {$ifdef IEHASTYPES} Types, {$endif}
  {$ifdef IEHASUITYPES} System.UITypes, {$endif}
  {$IFDEF IEINCLUDEIEXACQUIRE}
  iexAcquire, ietwain, iexDCIM, iewia,
  {$ENDIF}
  {$IFDEF IEINCLUDEDIRECTSHOW}
  ieds,
  {$ENDIF}
  {$IFDEF IEINCLUDEMEDIAFOUNDATION}
  iemmf,
  {$ENDIF}
  {$ifdef FPC}
  lmetafile,
  {$endif}
  printers;

const
  IEMAXICOIMAGES = 16;

  // Jpeg markers
  JPEG_APP0  = $E0;
  JPEG_APP1  = $E1;
  JPEG_APP2  = $E2;
  JPEG_APP3  = $E3;
  JPEG_APP4  = $E4;
  JPEG_APP5  = $E5;
  JPEG_APP6  = $E6;
  JPEG_APP7  = $E7;
  JPEG_APP8  = $E8;
  JPEG_APP9  = $E9;
  JPEG_APP10 = $EA;
  JPEG_APP11 = $EB;
  JPEG_APP12 = $EC;
  JPEG_APP13 = $ED;
  JPEG_APP14 = $EE;
  JPEG_APP15 = $EF;
  JPEG_COM   = $FE;

  // Standard Twain sizes
  IETW_NONE = 0;
  IETW_A4LETTER = 1;
  IETW_B5LETTER = 2;
  IETW_USLETTER = 3;
  IETW_USLEGAL = 4;
  IETW_A5 = 5;
  IETW_B4 = 6;
  IETW_B6 = 7;
  IETW_USLEDGER = 9;
  IETW_USEXECUTIVE = 10;
  IETW_A3 = 11;
  IETW_B3 = 12;
  IETW_A6 = 13;
  IETW_C4 = 14;
  IETW_C5 = 15;
  IETW_C6 = 16;
  IETW_4A0 = 17;
  IETW_2A0 = 18;
  IETW_A0 = 19;
  IETW_A1 = 20;
  IETW_A2 = 21;
  IETW_A4 = IETW_A4LETTER;
  IETW_A7 = 22;
  IETW_A8 = 23;
  IETW_A9 = 24;
  IETW_A10 = 25;
  IETW_ISOB0 = 26;
  IETW_ISOB1 = 27;
  IETW_ISOB2 = 28;
  IETW_ISOB3 = IETW_B3;
  IETW_ISOB4 = IETW_B4;
  IETW_ISOB5 = 29;
  IETW_ISOB6 = IETW_B6;
  IETW_ISOB7 = 30;
  IETW_ISOB8 = 31;
  IETW_ISOB9 = 32;
  IETW_ISOB10 = 33;
  IETW_JISB0 = 34;
  IETW_JISB1 = 35;
  IETW_JISB2 = 36;
  IETW_JISB3 = 37;
  IETW_JISB4 = 38;
  IETW_JISB5 = IETW_B5LETTER;
  IETW_JISB6 = 39;
  IETW_JISB7 = 40;
  IETW_JISB8 = 41;
  IETW_JISB9 = 42;
  IETW_JISB10 = 43;
  IETW_C0 = 44;
  IETW_C1 = 45;
  IETW_C2 = 46;
  IETW_C3 = 47;
  IETW_C7 = 48;
  IETW_C8 = 49;
  IETW_C9 = 50;
  IETW_C10 = 51;
  IETW_USSTATEMENT = 52;
  IETW_BUSINESSCARD = 53;


  // IPTC items compatible with Adobe PhotoShop
  PhotoShop_IPTC_Records             = 2;

  IPTC_PS_Title                      = 5;
  IPTC_PS_Caption                    = 120;
  IPTC_PS_Keywords                   = 25;
  IPTC_PS_Category                   = 15;
  IPTC_PS_Category_2                 = 20;
  IPTC_PS_City                       = 90;
  IPTC_PS_State_Province             = 95;
  IPTC_PS_Country                    = 101;
  IPTC_PS_Instructions               = 40;
  IPTC_PS_Date_Created               = 55;
  IPTC_PS_Time_Created               = 60;
  IPTC_PS_Byline_1                   = 80;
  IPTC_PS_Byline_2                   = 85;
  IPTC_PS_Country_Code               = 100;
  IPTC_PS_Transmission_Reference     = 103;
  IPTC_PS_Credit                     = 110;
  IPTC_PS_Source                     = 115;
  IPTC_PS_Writer                     = 122;
  IPTC_PS_Edit_Status                = 7;
  IPTC_PS_Urgency                    = 10;
  IPTC_PS_Fixture_Identifier         = 22;
  IPTC_PS_Release_Date               = 30;
  IPTC_PS_Release_Time               = 35;
  IPTC_PS_Reference_Service          = 45;
  IPTC_PS_Reference_Date             = 47;
  IPTC_PS_Reference_Number           = 50;
  IPTC_PS_Originating_Program        = 65;
  IPTC_PS_Program_Version            = 70;
  IPTC_PS_Object_Cycle               = 75;
  IPTC_PS_Copyright_Notice           = 116;
  IPTC_PS_Image_Type                 = 130;


  // Possible values for EXIF_Orientation:
  _exoCorrectOrientation       = 1;     // Image is Orientated Correctly
  _exoNeedsHorizontalFlip      = 2;     // Image is Horizontally Flipped
  _exoNeeds180Rotate           = 3;     // Image is Offset by 180º
  _exoNeedsVerticalFlip        = 4;     // Image is Vertically Flipped
  _exoNeedsHorzAndVertFlip     = 5;     // Image is Flipped Horiz. and Offset 90º CCW
  _exoNeeds90RotateCW          = 6;     // Image is Offset by 90º CCW
  _exoNeedsFlipHorzAnd90Rotate = 7;     // Image is Flipped Horiz. and offset 90º CW
  _exoNeeds270RotateCW         = 8;     // Image is Offset by 90º clockwise
  
type

{!!
<FS>TIETextFormat

<FM>Declaration<FC>
TIETextFormat = (ietfPascal, ietfHex, ietfBase64, ietfASCIIArt);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>ietfPascal<FN></C> <C>
In this case the format is like:<FC>
  const
    'FileName_Extension_Size' = nnnn;
    'FileName_Extension' : array [0 .. 'FileName_Extension_Size' - 1] of byte = ( $xx,$xx );<FN>
This is useful when you want to embed the image inside a .pas file. You can load back the image by writing:<FC>
  ImageEnView1.IO.LoadFromBuffer(FileName_Extension, FileName_Extension_Size, fileformat);<FN>
</C> </R>
<R> <C><FC>ietfHex<FN></C> <C>
Save the image as a sequence of hex values. Example:<FC>
   0xaa, 0xbb, etc...<FN>
</C> </R>
<R> <C><FC>ietfBase64<FN></C> <C>
Save the image encoded with base 64 (the base of Mime64). Example:<FC>
  /9j/4AAQSkZJRgABAQEASABIAAD/2wBDAAYEBQYF...<FN>
</C> </R>
<R> <C><FC>ietfASCIIArt<FN></C> <C>
Save the image as an Ascii art. We suggest you sub-sample the image before save as Ascii art. The <FC>ImageFormat<FN> field of <A TImageEnIO.SaveToText> must be <FC>ioUnknown<FN>.</C> </R>
</TABLE>

!!}
TIETextFormat = (ietfPascal, ietfHex, ietfBase64, ietfASCIIArt);


{!!
<FS>TIEAcquireBitmapEvent

<FM>Declaration<FC>
TIEAcquireBitmapEvent = procedure(Sender: TObject; ABitmap: <A TIEBitmap>; var Handled: boolean) of object;

<FM>Description<FN>
This event is called whenever an image is acquired from a scanner.
<FC>ABitmap<FN> is a TIEBitmap object that contains the acquired image.
Setting Handled to True (default is False) causes ImageEn to ignore this image (i.e. it won't be inserted into the <A TImageEnMView> control, if attached).
!!}
  TIEAcquireBitmapEvent = procedure(Sender: TObject; ABitmap: TIEBitmap; var Handled: boolean) of object;

{!!
<FS>TIEAfterAcquireBitmapEvent

<FM>Declaration<FC>
TIEAfterAcquireBitmapEvent = procedure(Sender: TObject; index: integer) of object;

<FM>Description<FN>
This event fired after an image has been acquired and added to the image list.
<FC>index<FN> specifies the index of the new image in the attached <A TImageEnMView>.
!!}
  TIEAfterAcquireBitmapEvent = procedure(Sender: TObject; index: Integer) of object;


{!!
<FS>TIEJpegTransform

<FM>Declaration<FC>
TIEJpegTransform = (jtNone, jtCut, jtHorizFlip, jtVertFlip, jtTranspose, jtTransverse, jtRotate90, jtRotate180, jtRotate270);

<FM>Description<FN>
Lossless JPEG transformation functions:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>jtNone</C> <C>No transformation</C> </R>
<R> <C>jtCut</C> <C>Crops the image (i.e. only a portion of the image is kept)</C> </R>
<R> <C>jtHorizFlip</C> <C>Mirrors the image horizontally (left-right)</C> </R>
<R> <C>jtVertFlip</C> <C>Mirrors the image vertically (top-bottom)</C> </R>
<R> <C>jtTranspose</C> <C>Transposes the image (across UL-to-LR axis)</C> </R>
<R> <C>jtTransverse</C> <C>Transverse transpose (across UR-to-LL axis)</C> </R>
<R> <C>jtRotate90</C> <C>Rotates the image 90 degrees clockwise</C> </R>
<R> <C>jtRotate180</C> <C>Rotates the image 180 degrees</C> </R>
<R> <C>jtRotate270</C> <C>Rotates the image 270 degrees clockwise (i.e. 90° CCW)</C> </R>
</TABLE>
!!}
  TIEJpegTransform = (jtNone, jtCut, jtHorizFlip, jtVertFlip, jtTranspose, jtTransverse, jtRotate90, jtRotate180, jtRotate270);

{!!
<FS>TIEJpegCopyMarkers

<FM>Declaration<FC>
TIEJpegCopyMarkers = (jcCopyNone, jcCopyComments, jcCopyAll);

<FM>Description<FN>
Specify how the extra markers in a source file should be transfered to the destination file:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>jcCopyNone</C> <C>Copy no extra markers from the source file. This setting suppresses all comments and other excess data present in the source file</C> </R>
<R> <C>jcCopyComments</C> <C>Copy only comment markers. This setting copies only the comments from the source file, discarding all other inessential data</C> </R>
<R> <C>jcCopyAll</C> <C>Copy all extra markers. This setting preserves miscellaneous markers found in the source file, such as JFIF thumbnails and Photoshop settings. In some files these extra markers can be sizable</C> </R>
</TABLE>
!!}
  TIEJpegCopyMarkers = (jcCopyNone, jcCopyComments, jcCopyAll);

  // Header used to save a jpeg inside a Stream
  TStreamJpegHeader = record
    ID: array[0..4] of AnsiChar; // ="JFIF\0"
    dim: integer;                // length of jpeg box
  end;

  // Header used to save a PCX inside a Stream
  PCXSHead = record
    ID: array[0..4] of AnsiChar; // 'PCX2\0'
    dim: integer;
  end;

  // Header used to save a TIFF inside a Stream
  TIFFSHead = record
    ID: array[0..4] of AnsiChar; // 'TIFF\0'
    dim: integer;
  end;

{!!
<FS>TPreviewParams

<FM>Declaration<FC>
type TPreviewParams = set of (ppALL, ppAUTO, ppJPEG, ppTIFF, ppGIF, ppBMP, ppPCX, ppPNG, ppTGA, ppJ2000);

<FM>Description<FN>
Specify which pages are included in the IO Parameters dialog:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ppALL</C> <C>Show all pages</C> </R>
<R> <C>ppAUTO</C> <C>Show the relevant page for the current file type (i.e. as specified in <L TIOParamsVals.FileType>Params.FileType</L>)</C> </R>
<R> <C>ppJPEG</C> <C>Show JPEG parameters</C> </R>
<R> <C>ppTIFF</C> <C>Show TIFF paramaters</C> </R>
<R> <C>ppGIF</C> <C>Show GIF (non-animated) parameters</C> </R>
<R> <C>ppBMP</C> <C>Show BMP parameters</C> </R>
<R> <C>ppPCX</C> <C>Show PCX parameters</C> </R>
<R> <C>ppPNG</C> <C>Show PNG parameters</C> </R>
<R> <C>ppTGA</C> <C>Show TGA parameters</C> </R>
<R> <C>ppJ2000</C> <C>Show JPEG2000 parameters</C> </R>
</TABLE>
!!}
  TPreviewParams = set of (
    ppALL,
    ppAUTO,
    ppJPEG,
    ppTIFF,
    ppGIF,
    ppBMP,
    ppPCX,
    ppPNG,
    ppTGA
{$IFDEF IEINCLUDEJPEG2000}
    , ppJ2000
{$ENDIF}
    );

  // previews properties

{!!
<FS>TIOPreviewsParamsItems

<FM>Declaration<FC>
TIOPreviewsParamsItems = (ioppDefaultLockPreview, ioppApplyButton);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ioppDefaultLockPreview</C> <C>Enable "Lock preview" by default</C> </R>
<R> <C>ioppApplyButton</C> <C>Display an "Apply" button</C> </R>
</TABLE>
!!}
  TIOPreviewsParamsItems = (ioppDefaultLockPreview, ioppApplyButton);

{!!
<FS>TIOPreviewsParams

<FM>Declaration<FC>
TIOPreviewsParams = set of <A TIOPreviewsParamsItems>;   

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ioppDefaultLockPreview</C> <C>Enable "Lock preview" by default</C> </R>
<R> <C>ioppApplyButton</C> <C>Display an "Apply" button</C> </R>
</TABLE>
!!}
  TIOPreviewsParams = set of TIOPreviewsParamsItems;

  // printing properties

{!!
<FS>TIEVerticalPos

<FM>Declaration<FC>
TIEVerticalPos = (ievpTop, ievpCenter, ievpBottom);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ievpTop</C> <C>Image is aligned to the top</C> </R>
<R> <C>ievpCenter</C> <C>Image is vertically centered</C> </R>
<R> <C>ievpBottom</C> <C>Image is aligned to the bottom</C> </R>
</TABLE>
!!}
  TIEVerticalPos = (ievpTop, ievpCenter, ievpBottom);

{!!
<FS>TIEHorizontalPos

<FM>Declaration<FC>
TIEHorizontalPos = (iehpLeft, iehpCenter, iehpRight);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iehpLeft</C> <C>Image is aligned to the left-hand side</C> </R>
<R> <C>iehpCenter</C> <C>Image is horizontally centered</C> </R>
<R> <C>iehpRight</C> <C>Image is aligned to the right-hand side</C> </R>
</TABLE>
!!}
  TIEHorizontalPos = (iehpLeft, iehpCenter, iehpRight);

{!!
<FS>TIESize

<FM>Declaration<FC>
TIESize = (iesNormal, iesFitToPage, iesFitToPageStretch, iesFillPage, iesSpecifiedSize);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iesNormal</C> <C>Determine print size by the original image DPI (i.e. to create a true size copy)</C> </R>
<R> <C>iesFitToPage</C> <C>Stretch the image to fit the page, while respecting its proportions</C> </R>
<R> <C>iesFitToPageStretch</C> <C>Stretch the image to fit the page, ignoring the proportions</C> </R>
<R> <C>iesFillPage</C> <C>Stretch the image to fill the entire page (respecting its proportions), cropping any edge of it so that it aligns with the orientation of the page (i.e. portions of the image may not be printed)</C> </R>
<R> <C>iesSpecifiedSize</C> <C>Print at an absolute size specified by the SpecWidth and SpecHeight parameters</C> </R>
</TABLE>
!!}
  TIESize = (iesNormal, iesFitToPage, iesFitToPageStretch, iesFillPage, iesSpecifiedSize);



{!!
<FS>TIECreateAVIFileResult

<FM>Declaration<FC>
TIECreateAVIFileResult = (ieaviOK, ieaviNOCOMPRESSOR, ieaviMEMORY, ieaviUNSUPPORTED);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieaviOK</C> <C>No error</C> </R>
<R> <C>ieaviNOCOMPRESSOR</C> <C>A suitable compressor cannot be found</C> </R>
<R> <C>ieaviMEMORY</C> <C>There is not enough memory to complete the operation</C> </R>
<R> <C>ieaviUNSUPPORTED</C> <C>Compression is not supported for this type of data</C> </R>
</TABLE>
!!}
TIECreateAVIFileResult = (ieaviOK, ieaviNOCOMPRESSOR, ieaviMEMORY, ieaviUNSUPPORTED);


{!!
<FS>TIOPSCompression

<FM>Declaration<FC>
TIOPSCompression = (ioPS_RLE, ioPS_G4FAX, ioPS_G3FAX2D, ioPS_JPEG);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ioPS_RLE</C> <C>Run length compression</C> </R>
<R> <C>ioPS_G4FAX</C> <C>G4Fax (black & white images only)</C> </R>
<R> <C>ioPS_G3FAX2D</C> <C>G3Fax (black & white images only)</C> </R>
<R> <C>ioPS_JPEG</C> <C>DCT-JPEG (color images only)</C> </R>
</TABLE>
!!}
  TIOPSCompression = (
    ioPS_RLE,
    ioPS_G4FAX,
    ioPS_G3FAX2D,
    ioPS_JPEG
    );

{!!
<FS>TIOPDFCompression

<FM>Declaration<FC>
  TIOPDFCompression = (
    ioPDF_UNCOMPRESSED,
    ioPDF_RLE,
    ioPDF_G4FAX,
    ioPDF_G3FAX2D,
    ioPDF_JPEG,
    ioPDF_LZW
    );

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ioPDF_UNCOMPRESSED</C> <C>No compression</C> </R>
<R> <C>ioPDF_RLE</C> <C>Run length compression</C> </R>
<R> <C>ioPDF_G4FAX</C> <C>G4Fax (black & white images only)</C> </R>
<R> <C>ioPDF_G3FAX2D</C> <C>G3Fax (black & white images only)</C> </R>
<R> <C>ioPDF_JPEG</C> <C>DCT-JPEG (color images only)</C> </R>
<R> <C>ioPDF_LZW</C> <C>LZW compression (color and black/white images)</C> </R>
</TABLE>
!!}
  TIOPDFCompression = (
    ioPDF_UNCOMPRESSED,
    ioPDF_RLE,
    ioPDF_G4FAX,
    ioPDF_G3FAX2D,
    ioPDF_JPEG,
    ioPDF_LZW
    );

{!!
<FS>TIOPDFPaperSize

<FM>Declaration<FC>
TIOPDFPaperSize = (iepA0, iepA1, iepA2, iepA3, iepA4, iepA5, iepA6, iepB5, iepLetter, iepLegal, iepLedger, iepTabloid, iepUnknown);

<FM>Description<FN>
Standard paper sizes that can be specified for <A TIOParamsVals.PS_PaperSize> and <A TIOParamsVals.PDF_PaperSize>.

Note: You can convert <FC>TIOPDFPaperSize<FN> to/from a string using <A IEPaperSizeToStr> and <A IEStrToPaperSize>.

<FM>Example<FC>
procedure TMainForm.FormCreate(Sender: TObject);
var
  a: TPDFPaperSize;
begin
  // Fill combobox with available PDF paper sizes
  cmbPaperSize.Clear;
  for a := Low(TPDFPaperSize) to High(TPDFPaperSize) do
    cmbPaperSize.Items.Add(IEPaperSizeToStr(a));

  // Make "US Letter" the selected one
  cmbPaperSize.ItemIndex := cmbPaperSize.Items.IndexOf(IEPaperSizeToStr(iepLetter));
end;

// Set PDF paper size to user's selection
ImageEnView1.IO.Params.PDF_PaperSize := IEStrToPaperSize(cmbPaperSize.Text);
!!}
  TIOPDFPaperSize = (iepA0, iepA1, iepA2, iepA3, iepA4, iepA5, iepA6, iepB5, iepLetter, iepLegal, iepLedger, iepTabloid, iepUnknown);

{!!
<FS>TIOTIFFCompression

<FM>Declaration<FC>
type TIOTIFFCompression = (ioTIFF_UNCOMPRESSED, ioTIFF_CCITT1D, ioTIFF_G3FAX1D, ioTIFF_G3FAX2D, ioTIFF_G4FAX, ioTIFF_LZW, ioTIFF_OLDJPEG, ioTIFF_JPEG, ioTIFF_PACKBITS, ioTIFF_ZIP, ioTIFF_DEFLATE, ioTIFF_UNKNOWN);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> <H>Supported Pixel Formats</H> <H>Supports Alpha Channel?</H> </R>
<R> <C>ioTIFF_UNCOMPRESSED</C> <C>Uncompressed TIFF</C>  <C>All</C> <C>Yes</C> </R>
<R> <C>ioTIFF_CCITT1D</C> <C>Bilevel Huffman compression</C>  <C>Black/White Only</C> <C>No</C> </R>
<R> <C>ioTIFF_G3FAX1D</C> <C>Bilevel Group 3 CCITT compression, mono-dimensional</C> <C>Black/White Only</C> <C>No</C> </R>
<R> <C>ioTIFF_G3FAX2D</C> <C>Bilevel Group 3 CCITT compression, bi-dimensional</C> <C>Black/White Only</C> <C>No</C> </R>
<R> <C>ioTIFF_G4FAX</C> <C>Bilevel Group 4 CCITT compression, bi-dimensiona</C> <C>Black/White Only</C> <C>No</C> </R>
<R> <C>ioTIFF_LZW</C> <C>LZW compression</C>  <C>All</C> <C>Yes</C> </R>
<R> <C>ioTIFF_OLDJPEG</C> <C>Ver 6.0 JPEG compression (unsupported)</C> <C>True Color Images Only</C> <C>No</C> </R>
<R> <C>ioTIFF_JPEG</C> <C>JPEG compression</C> <C>True Color Images Only</C> <C>No</C> </R>
<R> <C>ioTIFF_PACKBITS</C> <C>RLE compression</C>  <C>All</C> <C>Yes</C> </R>
<R> <C>ioTIFF_ZIP</C> <C>ZIP compression (non-TIFF standard)</C>  <C>All</C> <C>No</C> </R>
<R> <C>ioTIFF_DEFLATE</C> <C>Adobe ZIP compression (non-TIFF standard)</C>  <C>All</C> <C>No</C> </R>
<R> <C>ioTIFF_UNKNOWN</C> <C>Unknown compression</C>  <C></C> <C></C> </R>
</TABLE>

For black/white compressions (ioTIFF_CCITT1D, ioTIFF_G3FAX1D, ioTIFF_G3FAX2D and ioTIFF_G4FAX) make sure that <A TIOParamsVals.BitsPerSample> = 1 and <A TIOParamsVals.SamplesPerPixel> = 1.

<FM>Example<FC>
ImageEnView1.IO.Params.TIFF_Compression := ioTIFF_G4FAX;
ImageEnView1.IO.Params.BitsPerSample := 1;
ImageEnView1.IO.Params.SamplesPerPixel := 1;
ImageEnView1.IO.SaveToFile('D:\output.tif');<FN>

!!}
  TIOTIFFCompression = (
    ioTIFF_UNCOMPRESSED,
    ioTIFF_CCITT1D,
    ioTIFF_G3FAX1D,
    ioTIFF_G3FAX2D,
    ioTIFF_G4FAX,
    ioTIFF_LZW,
    ioTIFF_OLDJPEG,
    ioTIFF_JPEG,
    ioTIFF_PACKBITS,
    ioTIFF_ZIP,
    ioTIFF_DEFLATE,
    ioTIFF_UNKNOWN);
{!!
<FS>TIOTIFFPhotometInterpret

<FM>Declaration<FC>
type TIOTIFFPhotometInterpret = (ioTIFF_WHITEISZERO, ioTIFF_BLACKISZERO, ioTIFF_RGB, ioTIFF_RGBPALETTE, ioTIFF_TRANSPMASK, ioTIFF_CMYK, ioTIFF_YCBCR, ioTIFF_CIELAB);

!!}
  TIOTIFFPhotometInterpret = (
    ioTIFF_WHITEISZERO,
    ioTIFF_BLACKISZERO,
    ioTIFF_RGB,
    ioTIFF_RGBPALETTE,
    ioTIFF_TRANSPMASK,
    ioTIFF_CMYK,
    ioTIFF_YCBCR,
    ioTIFF_CIELAB);

{!!
<FS>TIOJPEGColorspace

<FM>Declaration<FC>
type TIOJPEGColorspace=(ioJPEG_RGB, ioJPEG_GRAYLEV, ioJPEG_YCbCr, ioJPEG_CMYK, ioJPEG_YCbCrK);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ioJPEG_RGB</C> <C>Separate RGB channels</C> </R>
<R> <C>ioJPEG_GRAYLEV</C> <C>Unique intensity channel (gray levels)</C> </R>
<R> <C>ioJPEG_YCbCr</C> <C>Three channels (CCIR Recommendation 601-1)</C> </R>
<R> <C>ioJPEG_CMYK</C> <C>Four channels (Cyan Magenta Yellow Black) - linear conversion</C> </R>
<R> <C>ioJPEG_YCbCrK</C> <C>Four channels (YCbCr and Black)</C> </R>
</TABLE>
!!}
  TIOJPEGColorspace = (
    ioJPEG_RGB,
    ioJPEG_GRAYLEV,
    ioJPEG_YCbCr,
    ioJPEG_CMYK,
    ioJPEG_YCbCrK);

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TIOJ2000ColorSpace

<FM>Declaration<FC>
  TIOJ2000ColorSpace = (
    ioJ2000_GRAYLEV,
    ioJ2000_RGB,
    ioJ2000_YCbCr);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H>
<R> <C>ioJ2000_GRAYLEV</C> <C>Gray scale image</C> </R>
<R> <C>ioJ2000_RGB</C> <C>RGB image</C> </R>
<R> <C>ioJ2000_YcbCr</C> <C>YcbCr image (currently supported only for loading)</C> </R>
</TABLE>
!!}
  TIOJ2000ColorSpace = (
    ioJ2000_GRAYLEV,
    ioJ2000_RGB,
    ioJ2000_YCbCr);

{!!
<FS>TIOJ2000ScalableBy

<FM>Declaration<FC>
  TIOJ2000ScalableBy = (
    ioJ2000_Rate,
    ioJ2000_Resolution);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H>
<R> <C>ioJ2000_Rate</C> <C>Layer-resolution-component-position (LRCP) progressive (i.e. rate scalable)</C> </R>
<R> <C>ioJ2000_Resolution</C> <C>Resolution-layer-component-position (RLCP) progressive (i.e. resolution scalable)</C> </R>
</TABLE>
!!}
  TIOJ2000ScalableBy = (
    ioJ2000_Rate,
    ioJ2000_Resolution);

{$ENDIF}

{!!
<FS>TIOJPEGDctMethod

<FM>Declaration<FC>
type TIOJPEGDctMethod = (ioJPEG_ISLOW, ioJPEG_IFAST, ioJPEG_FLOAT);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H>
<R> <C>ioJPEG_ISLOW</C> <C>Slow but accurate integer algorithm (default)</C> </R>
<R> <C>ioJPEG_IFAST</C> <C>Faster, less accurate integer method</C> </R>
<R> <C>ioJPEG_FLOAT</C> <C>Floating-point method (machine dependent)</C> </R>
</TABLE>
!!}
  TIOJPEGDctMethod = (
    ioJPEG_ISLOW,
    ioJPEG_IFAST,
    ioJPEG_FLOAT);

{!!
<FS>TIOJPEGCromaSubsampling

<FM>Declaration<FC>
type TIOJPEGCromaSubsampling = (ioJPEG_MEDIUM, ioJPEG_HIGH, ioJPEG_NONE);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H>
<R> <C>ioJPEG_MEDIUM</C> <C>4:2:2 croma sub-sampling. Medium quality</C> </R>
<R> <C>ioJPEG_HIGH</C> <C>4:1:1 croma sub-sampling. Default quality for jpegs. Lowest quality</C> </R>
<R> <C>ioJPEG_NONE</C> <C>4:4:4 croma sub-sampling. Highest quality</C> </R>
</TABLE>
!!}
  TIOJPEGCromaSubsampling = (
      ioJPEG_MEDIUM,
      ioJPEG_HIGH,
      ioJPEG_NONE);

{!!
<FS>TIOJPEGScale

<FM>Declaration<FC>
  TIOJPEGScale = (
    ioJPEG_AUTOCALC,
    ioJPEG_FULLSIZE,
    ioJPEG_HALF,
    ioJPEG_QUARTER,
    ioJPEG_EIGHTH);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ioJPEG_AUTOCALC</C> <C>The JPEG Scale is selected automatically to ensure it is larger than your minimum dimensions. You need to specify this using the <A TIOParamsVals.Width> and <A TIOParamsVals.Height> properties</C> </R>
<R> <C>ioJPEG_FULLSIZE</C> <C>The full image is loaded (default). Slowest loading</C> </R>
<R> <C>ioJPEG_HALF</C> <C>Image is loaded at 1/2 size</C> </R>
<R> <C>ioJPEG_QUARTER</C> <C>Image is loaded at 1/4 size</C> </R>
<R> <C>ioJPEG_EIGHTH</C> <C>Image is loaded at 1/8 size. Very fast loading</C> </R>
</TABLE>

<FM>Example<FC>
// Load JPEG as fast as possible while still being larger than the display size
ImageEnView.IO.Params.Width  := ImageEnView1.ClientWidth;
ImageEnView.IO.Params.Height := ImageEnView1.ClientHeight;
ImageEnView.IO.Params.JPEG_Scale := ioJPEG_AUTOCALC;
ImageEnView.IO.LoadFromFile('D:\MyImage.jpeg’);
!!}
  TIOJPEGScale = (
    ioJPEG_AUTOCALC,
    ioJPEG_FULLSIZE,
    ioJPEG_HALF,
    ioJPEG_QUARTER,
    ioJPEG_EIGHTH);

{!!
<FS>TIOBMPVersion

<FM>Declaration<FC>
type TIOBMPVersion = (ioBMP_BM, ioBMP_BM3, ioBMP_BMOS2V1, ioBMP_BMOS2V2);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ioBMP_BM</C> <C>Old Windows bitmap</C> </R>
<R> <C>ioBMP_BM3</C> <C>Windows 3.x or newer bitmap</C> </R>
<R> <C>ioBMP_BMOS2V1</C> <C>IBM OS/2 version 1.x bitmap</C> </R>
<R> <C>ioBMP_BMOS2V2</C> <C>IBM OS/2 version 2.x bitmap</C> </R>
</TABLE>
!!}
  TIOBMPVersion = (
    ioBMP_BM,
    ioBMP_BM3,
    ioBMP_BMOS2V1,
    ioBMP_BMOS2V2);

{!!
<FS>TIOBMPCompression

<FM>Declaration<FC>
type TIOBMPCompression = (ioBMP_UNCOMPRESSED, ioBMP_RLE);

<FM>Description<FN>
ioBMP_UNCOMPRESSED: Uncompressed bitmap
ioBMP_RLE: Compressed bitmap (generally smaller than uncompressed, but not always)
!!}
  TIOBMPCompression = (
    ioBMP_UNCOMPRESSED,
    ioBMP_RLE);

{!!
<FS>TIOPCXCompression

<FM>Declaration<FC>
type TIOPCXCompression = (ioPCX_UNCOMPRESSED, ioPCX_RLE);

<FM>Description<FN>
ioPCX_UNCOMPRESSED: Uncompressed PCX (incompatible with most PCX readers).
ioPCX_RLE: Compressed PCX (standard PCX).
!!}
  TIOPCXCompression = (
    ioPCX_UNCOMPRESSED,
    ioPCX_RLE);

{!!
<FS>TIOPNGFilter

<FM>Declaration<FC>
}
  TIOPNGFilter = (
    ioPNG_FILTER_NONE,
    ioPNG_FILTER_SUB,
    ioPNG_FILTER_PAETH,
    ioPNG_FILTER_UP,
    ioPNG_FILTER_AVG,
    ioPNG_FILTER_ALL);
{!!}

{!!
<FS>TIEGIFAction

<FM>Declaration<FC>
}
  TIEGIFAction = (
    ioGIF_None,
    ioGIF_DontRemove,
    ioGIF_DrawBackground,
    ioGIF_RestorePrev);
{!!}


{!!
<FS>TIOFileType

<FM>Declaration<FC>
TIOFileType = integer;

<FM>Description<FN>
Specifies a file format supported by ImageEn (may be read-write, read-only or write-only).

<TABLE>
<R> <H>Constant</H> <H>Description</H> </R>
<R> <C>ioUnknown</C> <C>Unknown file format</C> </R>
<R> <C>ioTIFF</C> <C>TIFF Bitmap</C> </R>
<R> <C>ioGIF</C> <C>GIF</C> </R>
<R> <C>ioJPEG</C> <C>Jpeg bitmap</C> </R>
<R> <C>ioPCX</C> <C>PaintBrush PCX</C> </R>
<R> <C>ioBMP</C> <C>Windows Bitmap</C> </R>
<R> <C>ioICO</C> <C>Windows Icon</C> </R>
<R> <C>ioCUR</C> <C>Windows Cursor</C> </R>
<R> <C>ioPNG</C> <C>Portable Network Graphics</C> </R>
<R> <C>ioWMF</C> <C>Windows Metafile</C> </R>
<R> <C>ioEMF</C> <C>Enhanced Windows Metafile</C> </R>
<R> <C>ioTGA</C> <C>Targa Bitmap</C> </R>
<R> <C>ioPXM</C> <C>Portable Pixmap, GreyMap, BitMap</C> </R>
<R> <C>ioJP2</C> <C>Jpeg2000</C> </R>
<R> <C>ioJ2K</C> <C>Jpeg2000</C> </R>
<R> <C>ioAVI</C> <C>AVI video</C> </R>
<R> <C>ioWBMP</C> <C>Wireless bitmap</C> </R>
<R> <C>ioPS</C> <C>Postscript</C> </R>
<R> <C>ioPDF</C> <C>Adobe PDF</C> </R>
<R> <C>ioDCX</C> <C>Multipage PCX</C> </R>
<R> <C>ioRAW</C> <C>Digital Camera RAW</C> </R>
<R> <C>ioBMPRAW</C> <C>Bitmap RAW</C> </R>
<R> <C>ioWMV</C> <C>Windows Media</C> </R>
<R> <C>ioMPEG</C> <C>Video MPEG</C> </R>
<R> <C>ioPSD</C> <C>Adobe Photoshop PSD</C> </R>
<R> <C>ioIEV</C> <C>Vectorial objects (<A TImageEnVect.SaveToFileIEV>)</C> </R>
<R> <C>ioLYR</C> <C>Layers (<A TImageEnView.LayersSaveToFile>)</C> </R>
<R> <C>ioALL</C> <C>Combined layers and vectorial objects (<A TImageEnVect.SaveToFileAll>)</C> </R>
<R> <C>ioDICOM</C> <C>DICOM medical imaging</C> </R>
<R> <C>ioHDP</C> <C>Microsoft HD Photo. Requires Windows XP (SP2) with .Net 3.0, Windows Vista or newer</C> </R>
<R> <C>ioRawDLLPlugIn</C> <C>DCRAW external plug-in for Digital Camera Raw file support</C> </R>
<R> <C>ioOtherDLLPlugIns + offset</C> <C>External plugins (e.g. JBIG)</C> </R>
<R> <C>ioMiscDLLPlugIns + offset</C> <C>Misc External plugins (e.g. PCL)</C> </R>
<R> <C>ioUSER + offset</C> <C>User registered file formats</C> </R>
</TABLE>
!!}
  TIOFileType = integer;

const
  // types for TIOFileType
  ioUnknown    = 0;
  ioTIFF       = 1;
  ioGIF        = 2;
  ioJPEG       = 3;
  ioPCX        = 4;
  ioBMP        = 5;
  ioICO        = 6;
  ioCUR        = 7;
  ioPNG        = 8;
  ioWMF        = 9;
  ioEMF        = 10;
  ioTGA        = 11;
  ioPXM        = 12;
  ioJP2        = 13;
  ioJ2K        = 14;
  ioAVI        = 15;
  ioWBMP       = 16;
  ioPS         = 17;
  ioPDF        = 18;
  ioDCX        = 19;
  ioRAW        = 20;
  ioBMPRAW     = 21;
  ioWMV        = 22;
  ioMPEG       = 23;
  ioPSD        = 24;
  ioIEV        = 25;
  ioLYR        = 26;
  ioALL        = 27;
  ioDICOM      = 28;
  ioHDP        = 29;
  ioRAS        = 30; // sun RAS (supported by ievision)
  ioRawDLLPlugIn    = 4096;
  ioOtherDLLPlugIns = ioRawDLLPlugIn + 1;
  ioMiscDLLPlugIns  = 8192;
  ioUSER            = 10000;

type

{!!
<FS>TIOICOSizes

<FM>Declaration<FC>
}
  TIOICOSizes = array[0..IEMAXICOIMAGES - 1] of TSize;
{!!}

{!!
<FS>TIOICOBitCount

<FM>Declaration<FC>
}
  TIOICOBitCount = array[0..IEMAXICOIMAGES - 1] of integer;
{!!}


{!!
<FS>TIOBMPRAWChannelOrder

<FM>Declaration<FC>
}
  TIOBMPRAWChannelOrder=(coRGB, coBGR);
{!!}

{!!
<FS>TIOBMPRAWDataFormat

<FM>Declaration<FC>
TIOBMPRAWDataFormat = (dfBinary, dfTextDecimal, dfTextHex);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>dfBinary<FN></C> <C>Binary raw data</C> </R>
<R> <C><FC>dfTextDecimal<FN></C> <C>Ascii text as decimal values</C> </R>
<R> <C><FC>dfTextHex<FN></C> <C>Ascii text as hexadecimal values</C> </R>
</TABLE>
!!}
TIOBMPRAWDataFormat = (dfBinary, dfTextDecimal, dfTextHex);

{!!
<FS>TIOBMPRAWPlanes

<FM>Declaration<FC>
TIOBMPRAWPlanes = (plInterleaved, plPlanar);

<FM>Description<FN>
plInterleaved channels are B,G,R,B,G,R...
plPlanar channels are BBB.., GGG.., RRR...
!!}
  TIOBMPRAWPlanes = (plInterleaved, plPlanar);

{!!
<FS>TIOByteOrder

<FM>Declaration<FC>
TIOByteOrder = (ioBigEndian, ioLittleEndian);

<FM>Description<FN>
ioBigEndian: Motorola Byte Order
ioLittleEndian: Intel Byte Order
!!}
  TIOByteOrder = (ioBigEndian, ioLittleEndian); //



{!!
<FS>TIEDicomRange

<FM>Declaration<FC>
TIEDicomRange = (iedrAdjust, iedrSetBlackWhite);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>iedrAdjust<FN></C> <C>Adjust pixels values</C> </R>
<R> <C><FC>iedrSetBlackWhite<FN></C> <C>Set TIEBitmap.WhiteValue and TIEBitmap.BlackValue</C> </R>
</TABLE>
!!}
  TIEDicomRange = (iedrAdjust, iedrSetBlackWhite);


{!!
<FS>TIEDicomCompression

<FM>Declaration<FC>
}
  TIEDicomCompression = (iedcUncompressed_Implicit,   // uncompressed, little endian, implicit (IEDICOM_TRANSFERSYNTAX_UNCOMP_LITTLEENDIAN_IMPLICIT)
                         iedcUncompressed,            // uncompressed, little endian, explicit (IEDICOM_TRANSFERSYNTAX_UNCOMP_LITTLEENDIAN_EXPLICIT)
                         iedcUncompressed_BE,         // uncompressed, big endian, explicit (IEDICOM_TRANSFERSYNTAX_UNCOMP_BIGENDIAN_EXPLICIT)
                         iedcRLE,                     // RLE
                         iedcLSJPEG1,                 // lossless jpeg (IEDICOM_TRANSFERSYNTAX_LOSSLESSJPEG1)
                         iedcLSJPEG2,                 // lessless jpeg (IEDICOM_TRANSFERSYNTAX_LOSSLESSJPEG2)
                         iedcJPEG,                    // jpeg 8 bit (IEDICOM_TRANSFERSYNTAX_LOSSYJPEG8BIT)
                         iedcJPEG12Bit,               // jpeg 12 bit (IEDICOM_TRANSFERSYNTAX_LOSSYJPEG12BIT)
                         iedcJPEG2000,                // jpeg 2000 (IEDICOM_TRANSFERSYNTAX_JPEG2000)
                         iedcMPEG);                   // MPEG (IEDICOM_TRANSFERSYNTAX_MPEG)
{!!}



const
  IEEXIFUSERCOMMENTCODE_UNICODE: AnsiString   = #$55#$4E#$49#$43#$4F#$44#$45#$00;
  IEEXIFUSERCOMMENTCODE_ASCII: AnsiString     = #$41#$53#$43#$49#$49#$00#$00#$00;
  IEEXIFUSERCOMMENTCODE_JIS: AnsiString       = #$4A#$49#$53#$00#$00#$00#$00#$00;
  IEEXIFUSERCOMMENTCODE_UNDEFINED: AnsiString = #$00#$00#$00#$00#$00#$00#$00#$00;

type

  TImageEnIO = class;

{!!
<FS>TIOParamsVals

<FM>Description<FN>
The TIOParamsVals object provides access to the properties of all image and video file formats supported by ImageEn, such as compression type, text comments, bits per sample, samples per pixel, etc.
The properties for this class are set when <L TImageEnIO.LoadFromFile>loading an image</L> or can be retrieved (without loading of the image) using <A TIOParamsVals.LoadFromFile> (excluding save only formats, such as PDF, PS, etc).
You can modify these properties before saving your image (naturally this is only relevant to formats which ImageEn can save, which excludes EMF, WMF, CUR, etc).

<FM>See Also<FN>
- <A TImageEnIO.Params>
- <A TImageEnDBView.IOParams>

<FM>Methods and Properties<FN>
<FI>Properties storage handling<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.AdjustGPSCoordinates></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.Assign></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.Create></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.LoadFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.LoadFromStream></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.ResetEXIF></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.ResetInfo></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.SaveToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.SaveToStream></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.SetDefaultParams></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.UpdateEXIFThumbnail></C> </R>
</TABLE>

<FI>Common<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BitsPerSample></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ColorMapCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ColorMap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DefaultICCProfile></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.Dpi></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DpiX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DpiY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EnableAdjustOrientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.FileName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.FileTypeStr></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.FileType></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GetThumbnail></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.Height></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ImageCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ImageIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ImageDelayTime></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ImageEnAnnot></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ImagingAnnot></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.InputICCProfile></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.IsResource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.OutputICCProfile></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.OriginalHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.OriginalWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.SamplesPerPixel></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.Width></C> </R>
</TABLE>

<FI>BMP<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BMP_Compression></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BMP_HandleTransparency></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BMP_Version></C> </R>
</TABLE>

<FI>GIF<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_Action></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_Background></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_Comments></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_DelayTime></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_FlagTranspColor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_ImageCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_ImageIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_Interlaced></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_Ratio></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_RAWLoad></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_TranspColor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_Version></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_WinHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_WinWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_XPos></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.GIF_YPos></C> </R>
</TABLE>

<FI>JPEG<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_ColorSpace></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_DCTMethod></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_CromaSubsampling></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_EnableAdjustOrientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_GetExifThumbnail></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_MarkerList></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_OptimalHuffman></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_OriginalWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_OriginalHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_Progressive></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_Quality></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_Scale_Used></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_Scale></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_Smooth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_WarningCode></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.JPEG_WarningTot></C> </R>
</TABLE>

<FI>JPEG 2000<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.J2000_ColorSpace></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.J2000_Rate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.J2000_ScalableBy></C> </R>
</TABLE>

<FI>PCX<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PCX_Compression></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PCX_Version></C> </R>
</TABLE>

<FI>Adobe PSD<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PSD_HasPremultipliedAlpha></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PSD_LargeDocumentFormat></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PSD_LoadLayers></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PSD_ReplaceLayers></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PSD_SelectLayer></C> </R>
</TABLE>

<FI>HDP<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.HDP_ImageQuality></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.HDP_Lossless></C> </R>
</TABLE>

<FI>TIFF<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_BigTIFF></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_ByteOrder></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_Compression></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_DocumentName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_EnableAdjustOrientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_GetTile></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_FillOrder></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_ImageCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_ImageDescription></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_ImageIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_NewSubfileType></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_JPEGColorSpace></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_JPEGQuality></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_Orientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_PageCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_PageName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_PageNumber></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_PhotometInterpret></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_PhotoshopImageResources></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_PhotoshopImageSourceData></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_PlanarConf></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_SubIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_StripCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_XPos></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_YPos></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TIFF_ZIPCompression></C> </R>
</TABLE>

<FI>CUR<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.CUR_Background></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.CUR_ImageIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.CUR_XHotSpot></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.CUR_YHotSpot></C> </R>
</TABLE>

<FI>DCX<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DCX_ImageIndex></C> </R>
</TABLE>

<FI>ICO<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ICO_Background></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ICO_BitCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ICO_ImageIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.ICO_Sizes></C> </R>
</TABLE>

<FI>PNG<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PNG_Background></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PNG_Compression></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PNG_Filter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PNG_Interlaced></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PNG_TextKeys></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PNG_TextValues></C> </R>
</TABLE>

<FI>TGA<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_AspectRatio></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_Author></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_Background></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_Compressed></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_Date></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_Descriptor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_Gamma></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_GrayLevel></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_ImageName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_XPos></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.TGA_YPos></C> </R>
</TABLE>

<FI>PXM<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PXM_Comments></C> </R>
</TABLE>

<FI>AVI<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.AVI_FrameCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.AVI_FrameDelayTime></C> </R>
</TABLE>

<FI>MediaFile (AVI, MPEG, WMV, etc)<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.MEDIAFILE_FrameCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.MEDIAFILE_FrameDelayTime></C> </R>
</TABLE>

<FI>Dicom<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DICOM_Compression></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DICOM_J2000Rate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DICOM_JPEGQuality></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DICOM_Range></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DICOM_Tags></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.DICOM_WindowCenterOffset></C> </R>
</TABLE>

<FI>RealRAW (not camera RAW)<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BMPRAW_DataFormat></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BMPRAW_ChannelOrder></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BMPRAW_HeaderSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BMPRAW_Planes></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.BMPRAW_RowAlign></C> </R>
</TABLE>

<FI>IPTC<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.IPTC_Info></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.IPTC_Photoshop></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.ReadIPTCField></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.WriteIPTCField></C> </R>
<R> <C_IMG_METHOD> <C><A TIOParamsVals.ClearIPTCField></C> </R>
</TABLE>

<FI>EXIF tags<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ApertureValue></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Artist></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Bitmap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_BrightnessValue></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ColorSpace></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_CompressedBitsPerPixel></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Contrast></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Copyright></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_DateTime></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_DateTime2></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_DateTimeOriginal></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_DateTimeOriginal2></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_DateTimeDigitized></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_DateTimeDigitized2></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_DigitalZoomRatio></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ExifImageHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ExifImageWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ExifVersion></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ExposureBiasValue></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ExposureIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ExposureMode></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ExposureProgram></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ExposureTime></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_FileSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_FlashPixVersion></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Flash></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_FNumber></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_FocalLength></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_FocalLengthIn35mmFilm></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_FocalPlaneResolutionUnit></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_FocalPlaneXResolution></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_FocalPlaneYResolution></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GainControl></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_HasEXIFData></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ImageDescription></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ImageUniqueID></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ISOSpeedRatings></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_LightSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Make></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_MakerNote></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_MaxApertureValue></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_MeteringMode></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Model></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Orientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_PrimaryChromaticities></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ReferenceBlackWhite></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_RelatedSoundFile></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ResolutionUnit></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Saturation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_SceneCaptureType></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_SceneType></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_SensingMethod></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Sharpness></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_ShutterSpeedValue></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_Software></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_SubjectDistanceRange></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_SubjectDistance></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_SubsecTimeDigitized></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_SubsecTimeOriginal></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_SubsecTime></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_UserCommentCode></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_UserComment></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_WhiteBalance></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_WhitePoint></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_XResolution></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_YCbCrCoefficients></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_YCbCrPositioning></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_YResolution></C> </R>
</TABLE>

<FI>EXIF Windows XP tags<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_XPAuthor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_XPComment></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_XPKeywords></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_XPRating></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_XPSubject></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_XPTitle></C> </R>
</TABLE>

<FI>EXIF GPS tags<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSVersionID></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLatitude></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLatitude_Str></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLatitudeRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLatitudeDegrees></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLatitudeMinutes></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLatitudeSeconds></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLongitude></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLongitude_Str></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLongitudeRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLongitudeDegrees></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLongitudeMinutes></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSLongitudeSeconds></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSAltitudeRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSAltitude></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSTimeStampHour></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSTimeStampMinute></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSTimeStampSecond></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSSatellites></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSStatus></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSMeasureMode></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDOP></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSSpeedRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSSpeed></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSTrackRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSTrack></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSImgDirectionRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSImgDirection></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSMapDatum></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestLatitudeRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestLatitudeDegrees></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestLatitudeMinutes></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestLatitudeSeconds></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestLongitudeRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestLongitudeDegrees></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestLongitudeMinutes></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestLongitudeSeconds></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestBearingRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestBearing></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestDistanceRef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDestDistance></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_GPSDateStamp></C> </R>
</TABLE>

<FI>EXIF Interoperability tags<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_InteropIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.EXIF_InteropVersion></C> </R>
</TABLE>


<FI>PostScript<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PS_Compression></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PS_PaperHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PS_PaperWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PS_PaperSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PS_Title></C> </R>
</TABLE>

<FI>Adobe PDF<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_Author></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_Compression></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_Creator></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_Keywords></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_PaperHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_PaperWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_PaperSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_Producer></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_Subject></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.PDF_Title></C> </R>
</TABLE>

<FI>RAW (Camera)<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_AutoAdjustColors></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_BlueScale></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_Bright></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_Camera></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_ExtraParams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_FourColorRGB></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_Gamma></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_GetExifThumbnail></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_HalfSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_QuickInterpolate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_RedScale></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_UseAutoWB></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.RAW_UseCameraWB></C> </R>
</TABLE>

<FI>Adobe XMP info<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.XMP_Info></C> </R>
</TABLE>

<FI>Extra parameters<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOParamsVals.Dict></C> </R>
</TABLE>
!!}
  TIOParamsVals = class
  private
    fImageEnIO: TImageEnIO;
    fDict: TIEDictionary;
    fBitsPerSample: integer;
    fFileName: WideString;
    fSamplesPerPixel: integer;
    fWidth: integer;
    fHeight: integer;
    fDpiX: integer;
    fDpiY: integer;
    fFileType: TIOFileType;
    fImageIndex: Integer;
    fImageCount: Integer;
    fGetThumbnail: Boolean;
    fIsResource: Boolean;
    fEnableAdjustOrientation: Boolean;
    fOriginalWidth: integer;
    fOriginalHeight: integer;
    fTIFF_Compression: TIOTIFFCompression;
    fTIFF_ImageIndex: integer;
    fTIFF_SubIndex: integer; // SubIFD index (-1 read root)
    fTIFF_NewSubfileType: integer;
    fTIFF_PhotometInterpret: TIOTIFFPhotometInterpret;
    fTIFF_PlanarConf: integer;
    fTIFF_XPos: integer;
    fTIFF_YPos: integer;
    fTIFF_GetTile: integer; // -1, load all tiles
    fTIFF_DocumentName: AnsiString;
    fTIFF_ImageDescription: AnsiString;
    fTIFF_PageName: AnsiString;
    fTIFF_PageNumber: integer;
    fTIFF_PageCount: integer;
    fTIFF_Orientation: integer;
    fTIFF_LZWDecompFunc: TTIFFLZWDecompFunc;
    fTIFF_LZWCompFunc: TTIFFLZWCompFunc;
    fTIFF_EnableAdjustOrientation: boolean;
    fTIFF_JPEGQuality: integer;
    fTIFF_JPEGColorSpace: TIOJPEGColorSpace;
    fTIFF_ZIPCompression: integer;  // 0=fast 1=normal (default) 2=max
    fTIFF_StripCount: integer;      // 0=automatic (default), not read from TIFF, but used only in the writer
    fTIFF_ImageCount: integer;
    fTIFF_FillOrder: integer;
    fTIFF_ByteOrder: TIOByteOrder;
    fTIFF_PhotoshopImageResources: TIEArrayOfByte;
    fTIFF_PhotoshopImageSourceData: TIEArrayOfByte;
    fTIFF_BigTIFF: boolean;
    fDCX_ImageIndex: Integer;
    fGIF_Version: AnsiString;
    fGIF_ImageIndex: integer;
    fGIF_XPos: integer;
    fGIF_YPos: integer;
    fGIF_DelayTime: integer;
    fGIF_FlagTranspColor: boolean;
    fGIF_TranspColor: TRGB;
    fGIF_Interlaced: boolean;
    fGIF_WinWidth: integer;
    fGIF_WinHeight: integer;
    fGIF_Background: TRGB;
    fGIF_Ratio: integer;
    fGIF_Comments: TStringList;
    fGIF_Action: TIEGIFAction;
    fGIF_RAWLoad: boolean;
    fGIF_LZWDecompFunc: TGIFLZWDecompFunc;
    fGIF_LZWCompFunc: TGIFLZWCompFunc;
    fGIF_ImageCount: integer;
    fJPEG_ColorSpace: TIOJPEGColorSpace;
    fJPEG_Quality: integer;
    fJPEG_DCTMethod: TIOJPEGDCTMethod;
    fJPEG_CromaSubsampling: TIOJPEGCromaSubsampling;
    fJPEG_OptimalHuffman: boolean;
    fJPEG_Smooth: integer;
    fJPEG_Progressive: boolean;
    fJPEG_Scale: TIOJPEGScale;
    fJPEG_MarkerList: TIEMarkerList;
    fJPEG_Scale_Used: integer;
    fJPEG_WarningTot: integer;
    fJPEG_WarningCode: integer;
    fJPEG_EnableAdjustOrientation: Boolean;
    fJPEG_GetExifThumbnail: Boolean;
{$IFDEF IEINCLUDEJPEG2000}
    fJ2000_ColorSpace: TIOJ2000ColorSpace;
    fJ2000_Rate: double;
    fJ2000_ScalableBy: TIOJ2000ScalableBy;
{$ENDIF}
    fPCX_Version: integer;
    fPCX_Compression: TIOPCXCompression;
    fBMP_Version: TIOBMPVersion;
    fBMP_Compression: TIOBMPCompression;
    fBMP_HandleTransparency: Boolean;
    fICO_ImageIndex: integer;
    fICO_Background: TRGB;
    fCUR_ImageIndex: integer;
    fCUR_XHotSpot: integer;
    fCUR_YHotSpot: integer;
    fCUR_Background: TRGB;
    fPNG_Interlaced: boolean;
    fPNG_Background: TRGB;
    fPNG_Filter: TIOPNGFilter;
    fPNG_Compression: integer;
    fPNG_TextKeys: TStringList;
    fPNG_TextValues: TStringList;
{$ifdef IEINCLUDEDICOM}
    fDICOM_Tags: TIEDicomTags;
    fDICOM_WindowCenterOffset: double;
    fDICOM_Range: TIEDicomRange;
    fDICOM_JPEGQuality: integer;
    fDICOM_J2000Rate: double;
    fDICOM_RescaleIntercept: double;
    fDICOM_RescaleSlope: double;
{$endif}
    fTGA_XPos: integer;
    fTGA_YPos: integer;
    fTGA_Compressed: boolean;
    fTGA_Descriptor: AnsiString;
    fTGA_Author: AnsiString;
    fTGA_Date: TDateTime;
    fTGA_ImageName: AnsiString;
    fTGA_Background: TRGB;
    fTGA_AspectRatio: double;
    fTGA_Gamma: double;
    fTGA_GrayLevel: boolean;
    fIPTC_Info: TIEIPTCInfoList;
    {$ifdef IEINCLUDEIMAGINGANNOT}
    fImagingAnnot: TIEImagingAnnot;
    {$endif}
    fImageEnAnnot: TIEImageEnAnnot;
    fPXM_Comments: TStringList;
    fEXIF_Tags: TList;  // a list of received tags (codes) and a list of tags to write. ONLY FOR NUMERIC TAGS (excluded GPS and non subifd tags).
    fEXIF_HasEXIFData: boolean;
    fEXIF_Orientation: integer;
    fEXIF_Bitmap: TIEBitmap;
    fEXIF_ImageDescription: AnsiString;
    fEXIF_Make: AnsiString;
    fEXIF_Model: AnsiString;
    fEXIF_XResolution: double;
    fEXIF_YResolution: double;
    fEXIF_ResolutionUnit: integer;
    fEXIF_Software: AnsiString;
    fEXIF_Artist: AnsiString;
    fEXIF_DateTime: AnsiString;
    fEXIF_WhitePoint: array[0..1] of double;
    fEXIF_PrimaryChromaticities: array[0..5] of double;
    fEXIF_YCbCrCoefficients: array[0..2] of double;
    fEXIF_YCbCrPositioning: integer;
    fEXIF_ReferenceBlackWhite: array[0..5] of double;
    fEXIF_Copyright: AnsiString;
    fEXIF_ExposureTime: double;
    fEXIF_FNumber: double;
    fEXIF_ExposureProgram: integer;
    fEXIF_ISOSpeedRatings: array[0..1] of integer;
    fEXIF_ExifVersion: AnsiString;
    fEXIF_DateTimeOriginal: AnsiString;
    fEXIF_DateTimeDigitized: AnsiString;
    fEXIF_CompressedBitsPerPixel: double;
    fEXIF_ShutterSpeedValue: double;
    fEXIF_ApertureValue: double;
    fEXIF_BrightnessValue: double;
    fEXIF_ExposureBiasValue: double;
    fEXIF_MaxApertureValue: double;
    fEXIF_SubjectDistance: double;
    fEXIF_MeteringMode: integer;
    fEXIF_LightSource: integer;
    fEXIF_Flash: integer;
    fEXIF_FocalLength: double;
    fEXIF_SubsecTime: AnsiString;
    fEXIF_SubsecTimeOriginal: AnsiString;
    fEXIF_SubsecTimeDigitized: AnsiString;
    fEXIF_FlashPixVersion: AnsiString;
    fEXIF_ColorSpace: integer;
    fEXIF_ExifImageWidth: integer;
    fEXIF_ExifImageHeight: integer;
    fEXIF_RelatedSoundFile: AnsiString;
    fEXIF_FocalPlaneXResolution: double;
    fEXIF_FocalPlaneYResolution: double;
    fEXIF_FocalPlaneResolutionUnit: integer;
    fEXIF_ExposureIndex: double;
    fEXIF_SensingMethod: integer;
    fEXIF_FileSource: integer;
    fEXIF_SceneType: integer;
    fEXIF_UserComment: WideString;
    fEXIF_UserCommentCode: AnsiString;
    fEXIF_MakerNote: TIETagsHandler;
    fEXIF_XPRating: Integer;
    fEXIF_XPTitle: WideString;
    fEXIF_XPComment: WideString;
    fEXIF_XPAuthor: WideString;
    fEXIF_XPKeywords: WideString;
    fEXIF_XPSubject: WideString;
    fEXIF_ExposureMode: Integer;
    fEXIF_WhiteBalance: Integer;
    fEXIF_DigitalZoomRatio: Double;
    fEXIF_FocalLengthIn35mmFilm: Integer;
    fEXIF_SceneCaptureType: Integer;
    fEXIF_GainControl: Integer;
    fEXIF_Contrast: Integer;
    fEXIF_Saturation: Integer;
    fEXIF_Sharpness: Integer;
    fEXIF_SubjectDistanceRange: Integer;
    fEXIF_ImageUniqueID: AnsiString;
    fEXIF_GPSVersionID: AnsiString;
    fEXIF_GPSLatitudeRef: AnsiString;
    fEXIF_GPSLatitudeDegrees: Double;
    fEXIF_GPSLatitudeMinutes: Double;
    fEXIF_GPSLatitudeSeconds: Double;
    fEXIF_GPSLongitudeRef: AnsiString;
    fEXIF_GPSLongitudeDegrees: Double;
    fEXIF_GPSLongitudeMinutes: Double;
    fEXIF_GPSLongitudeSeconds: Double;
    fEXIF_GPSAltitudeRef: AnsiString;
    fEXIF_GPSAltitude: Double;
    fEXIF_GPSTimeStampHour: Double;
    fEXIF_GPSTimeStampMinute: Double;
    fEXIF_GPSTimeStampSecond: Double;
    fEXIF_GPSSatellites: AnsiString;
    fEXIF_GPSStatus: AnsiString;
    fEXIF_GPSMeasureMode: AnsiString;
    fEXIF_GPSDOP: Double;
    fEXIF_GPSSpeedRef: AnsiString;
    fEXIF_GPSSpeed: Double;
    fEXIF_GPSTrackRef: AnsiString;
    fEXIF_GPSTrack: Double;
    fEXIF_GPSImgDirectionRef: AnsiString;
    fEXIF_GPSImgDirection: Double;
    fEXIF_GPSMapDatum: AnsiString;
    fEXIF_GPSDestLatitudeRef: AnsiString;
    fEXIF_GPSDestLatitudeDegrees: Double;
    fEXIF_GPSDestLatitudeMinutes: Double;
    fEXIF_GPSDestLatitudeSeconds: Double;
    fEXIF_GPSDestLongitudeRef: AnsiString;
    fEXIF_GPSDestLongitudeDegrees: Double;
    fEXIF_GPSDestLongitudeMinutes: Double;
    fEXIF_GPSDestLongitudeSeconds: Double;
    fEXIF_GPSDestBearingRef: AnsiString;
    fEXIF_GPSDestBearing: Double;
    fEXIF_GPSDestDistanceRef: AnsiString;
    fEXIF_GPSDestDistance: Double;
    fEXIF_GPSDateStamp: AnsiString;
    fEXIF_InteropIndex: AnsiString;
    fEXIF_InteropVersion: AnsiString;
    fAVI_FrameCount: integer;
    fAVI_FrameDelayTime: double;
    {$ifdef IEINCLUDEDIRECTSHOW}
    fMEDIAFILE_FrameCount: integer;
    fMEDIAFILE_FrameDelayTime: double;
    {$endif}
    fPS_PaperWidth: integer;
    fPS_PaperHeight: integer;
    fPS_Compression: TIOPSCompression;
    fPS_Title: AnsiString;
    fPDF_PaperWidth: integer; // in Adobe PDF points (1 point=1/72 of inch).
    fPDF_PaperHeight: integer; // in Adobe PDF points (1 point=1/72 of inch).
    fPDF_Compression: TIOPDFCompression;
    fPDF_Title: AnsiString;
    fPDF_Author: AnsiString;
    fPDF_Subject: AnsiString;
    fPDF_Keywords: AnsiString;
    fPDF_Creator: AnsiString;
    fPDF_Producer: AnsiString;
    {$ifdef IEINCLUDERAWFORMATS}
    fRAW_HalfSize: Boolean;
    fRAW_Gamma: Double;
    fRAW_Bright: Double;
    fRAW_RedScale: Double;
    fRAW_BlueScale: Double;
    fRAW_QuickInterpolate: Boolean;
    fRAW_UseAutoWB: Boolean;
    fRAW_UseCameraWB: Boolean;
    fRAW_FourColorRGB: Boolean;
    fRAW_Camera: AnsiString;
    fRAW_GetExifThumbnail: Boolean;
    fRAW_AutoAdjustColors: Boolean;
    fRAW_ExtraParams: AnsiString;
    {$endif}
    fBMPRAW_ChannelOrder: TIOBMPRAWChannelOrder;
    fBMPRAW_Planes: TIOBMPRAWPlanes;
    fBMPRAW_RowAlign: Integer;
    fBMPRAW_HeaderSize: Integer;
    fBMPRAW_DataFormat: TIOBMPRAWDataFormat;
    fPSD_LoadLayers: Boolean;
    fPSD_ReplaceLayers: Boolean;
    fPSD_HasPremultipliedAlpha: Boolean;
    fPSD_LargeDocumentFormat: Boolean;
    fPSD_SelectLayer: AnsiString;
    fHDP_ImageQuality: Double;
    fHDP_Lossless: Boolean;
    fXMP_Info: AnsiString;
    function GetFileTypeStr: string;
    procedure SetEXIF_WhitePoint(index: integer; v: double);
    function GetEXIF_WhitePoint(index: integer): double;
    procedure SetEXIF_PrimaryChromaticities(index: integer; v: double);
    function GetEXIF_PrimaryChromaticities(index: integer): double;
    procedure SetEXIF_YCbCrCoefficients(index: integer; v: double);
    function GetEXIF_YCbCrCoefficients(index: integer): double;
    procedure SetEXIF_ReferenceBlackWhite(index: integer; v: double);
    function GetEXIF_ReferenceBlackWhite(index: integer): double;
    procedure SetEXIF_ISOSpeedRatings(index: integer; v: integer);
    function GetEXIF_ISOSpeedRatings(index: integer): integer;
    procedure SetDpi(Value: integer);
    procedure SetTIFF_Orientation(Value: integer);
    procedure SetEXIF_Orientation(Value: integer);
    procedure SetEXIF_XResolution(Value: double);
    procedure SetEXIF_YResolution(Value: double);
    function GetInputICC: TIEICC;
    function GetOutputICC: TIEICC;
    function GetDefaultICC: TIEICC;
    {$ifdef IEINCLUDEIMAGINGANNOT}
    function GetImagingAnnot: TIEImagingAnnot;
    {$endif}
    procedure SetImageIndex(value: Integer);
    procedure SetImageCount(value: Integer);
    procedure SetGetThumbnail(value: Boolean);
    procedure SetIsResource(value: Boolean);
    procedure SetJPEG_GetExifThumbnail(value: Boolean);
    {$ifdef IEINCLUDERAWFORMATS}
    procedure SetRAW_GetExifThumbnail(value: Boolean);
    {$endif}
    procedure SetEnableAdjustOrientation(value: Boolean);
    procedure EXIFTagsAdd(tag: Integer);
    procedure EXIFTagsDel(tag: Integer);
    procedure SetEXIF_ExposureTime(value: Double);
    procedure SetEXIF_FNumber(value: Double);
    procedure SetEXIF_ExposureProgram(value: Integer);
    procedure SetEXIF_CompressedBitsPerPixel(value: Double);
    procedure SetEXIF_ShutterSpeedValue(value: Double);
    procedure SetEXIF_ApertureValue(value: Double);
    procedure SetEXIF_BrightnessValue(value: Double);
    procedure SetEXIF_ExposureBiasValue(value: Double);
    procedure SetEXIF_MaxApertureValue(value: Double);
    procedure SetEXIF_SubjectDistance(value: Double);
    procedure SetEXIF_MeteringMode(value: Integer);
    procedure SetEXIF_LightSource(value: Integer);
    procedure SetEXIF_Flash(value: Integer);
    procedure SetEXIF_FocalLength(value: Double);
    procedure SetEXIF_ColorSpace(value: Integer);
    procedure SetEXIF_ExifImageWidth(value: Integer);
    procedure SetEXIF_ExifImageHeight(value: Integer);
    procedure SetEXIF_FocalPlaneXResolution(value: Double);
    procedure SetEXIF_FocalPlaneYResolution(value: Double);
    procedure SetEXIF_FocalPlaneResolutionUnit(value: Integer);
    procedure SetEXIF_ExposureIndex(value: Double);
    procedure SetEXIF_SensingMethod(value: Integer);
    procedure SetEXIF_FileSource(value: Integer);
    procedure SetEXIF_SceneType(value: Integer);
    procedure SetDpiX(Value: Integer);
    procedure SetDpiY(Value: Integer);
    function GetEXIF_GPSLatitude(): Double;
    procedure SetEXIF_GPSLatitude(value: Double);
    function GetEXIF_GPSLongitude(): Double;
    procedure SetEXIF_GPSLongitude(value: Double);
    function GetEXIF_DateTime2: TDateTime;
    procedure SetEXIF_DateTime2(const Value: TDateTime);
    function GetEXIF_DateTimeOriginal2: TDateTime;
    procedure SetEXIF_DateTimeOriginal2(const Value: TDateTime);
    function GetEXIF_DateTimeDigitized2: TDateTime;
    procedure SetEXIF_DateTimeDigitized2(const Value: TDateTime);
    function GetEXIF_GPSLatitude_Str: string;
    function GetEXIF_GPSLongitude_Str: string;
    function GetIPTC_Photoshop(FieldID: Integer): string;
    procedure SetIPTC_Photoshop(FieldID: Integer; const Value: string);
    function GetImageDelayTime: integer;
    function GetDICOM_Compression(): TIEDicomCompression;
    procedure SetDICOM_Compression(Value: TIEDicomCompression);
    procedure SetXMP_Info(Value: AnsiString);
    function GetPDF_PaperSize : TIOPDFPaperSize;
    procedure SetPDF_PaperSize(const value : TIOPDFPaperSize);
    function GetPS_PaperSize : TIOPDFPaperSize;
    procedure SetPS_PaperSize(const value : TIOPDFPaperSize);
  public
    //
    IsNativePixelFormat: boolean;
    // Read-Only fields
    fColorMap: PRGBROW;
    fColorMapCount: integer;
    // ICO (they are not properties)

{!!
<FS>TIOParamsVals.ICO_Sizes

<FM>Declaration<FC>
ICO_Sizes: <A TIOICOSizes>;

<FM>Description<FN>
An array of TSize structures which specifies the dimensions of all images contained in an icon file.

Note: The last item must specify the size as 0 x 0.

<FM>Example<FC>
// save the current image in 'output.ico', It will contain three images with 64x64 32bit (24bit + alphachannel), 32x32 256 colors and 32x32 16 colors

// 64 x 64 x 32bit
ImageEnView.IO.Params.ICO.BitCount[0] := 32;
ImageEnView.IO.Params.ICO.Sizes[0].cx := 64;
ImageEnView.IO.Params.ICO.Sizes[0].cy := 64;

// 32 x 32 x 8bit
ImageEnView.IO.Params.ICO.BitCount[1] := 8;
ImageEnView.IO.Params.ICO.Sizes[1].cx := 32;
ImageEnView.IO.Params.ICO.Sizes[1].cy := 32;

// 32 x 32 x 4bit
ImageEnView.IO.Params.ICO.BitCount[2] := 4;
ImageEnView.IO.Params.ICO.Sizes[2].cx := 32;
ImageEnView.IO.Params.ICO.Sizes[2].cy := 32;

// I don't want other images
ImageEnView.IO.Params.ICO.BitCount[3] := 0;
ImageEnView.IO.Params.ICO.Sizes[3].cx := 0;
ImageEnView.IO.Params.ICO.Sizes[3].cy := 0;

// save
ImageEnView.IO.SaveToFile('D:\output.ico');
!!}
    ICO_Sizes: TIOICOSizes;

{!!
<FS>TIOParamsVals.ICO_BitCount

<FM>Declaration<FC>
ICO_BitCount: <A TIOICOBitCount>;

<FM>Description<FN>
An array of integers which specifies the the Bitcount of all images contained in an icon file.

The last bitcount must be set to 0.

<FM>Example<FC>
// save the current image in 'output.ico', It will contain three images with 64x64 32bit (24bit + alphachannel), 32x32 256 colors and 32x32 16 colors

// 64 x 64 x 32bit
ImageEnView.IO.Params.ICO.BitCount[0] := 32;
ImageEnView.IO.Params.ICO.Sizes[0].cx := 64;
ImageEnView.IO.Params.ICO.Sizes[0].cy := 64;

// 32 x 32 x 8bit
ImageEnView.IO.Params.ICO.BitCount[1] := 8;
ImageEnView.IO.Params.ICO.Sizes[1].cx := 32;
ImageEnView.IO.Params.ICO.Sizes[1].cy := 32;

// 32 x 32 x 4bit
ImageEnView.IO.Params.ICO.BitCount[2] := 4;
ImageEnView.IO.Params.ICO.Sizes[2].cx := 32;
ImageEnView.IO.Params.ICO.Sizes[2].cy := 32;

// I don't want other images
ImageEnView.IO.Params.ICO.BitCount[3] := 0;
ImageEnView.IO.Params.ICO.Sizes[3].cx := 0;
ImageEnView.IO.Params.ICO.Sizes[3].cy := 0;

// save
ImageEnView.IO.SaveToFile('D:\output.ico');
!!}
    ICO_BitCount: TIOICOBitCount;

    //
    fInputICC: TIEICC;
    fOutputICC: TIEICC;
    fDefaultICC: TIEICC;

    // GENERIC


{!!
<FS>TIOParamsVals.FileName

<FM>Declaration<FC>
property FileName: WideString; (Read-only)

<FM>Description<FN>
Filename of the last loaded or saved image
!!}
    property FileName: WideString read fFileName write fFileName;

    property FileTypeStr: string read GetFileTypeStr;

{!!
<FS>TIOParamsVals.FileType

<FM>Declaration<FC>
property FileType: <A TIOFileType>;

<FM>Description<FN>
The file type of the last loaded or saved image, e.g. after a calling LoadFromFileGIF this property will have the ioGIF value.

<FM>Example<FC>
if OpenImageEnDialog1.Execute then
begin
  ImageEnView1.IO.LoadFromFile(OpenImageEnDialog1.FileName);
  case ImageEnView1.IO.Params.FileType of
    ioTIFF : ShowMessage('You have loaded a TIFF file');
    ioGIF  : ShowMessage('You have loaded a GIF file');
    ...
  end;
end;
!!}
    property FileType: TIOFileType read fFileType write fFileType;

{!!
<FS>TIOParamsVals.BitsPerSample

<FM>Declaration<FC>
property BitsPerSample: integer;

<FM>Description<FN>
Depth, in bits, for each sample.
Allowed values:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>1<FN></C> <C>1 bit black & white</C> </R>
<R> <C><FC>2 to 7<FN></C> <C>Color mapped bitmap (from 4 to 128 colors)</C> </R>
<R> <C><FC>8<FN></C> <C>Color mapped (256 colors), 8 bit gray scale, 24 bit RGB, 48 bit CMYK, 24 bit CIELab</C> </R>
<R> <C><FC>16<FN></C> <C>16 bit gray scale or 48 bit RGB</C> </R>
<R> <C><FC>32<FN></C> <C>32 bit floating point</C> </R>
</TABLE>

See also: <A TIOParamsVals.SamplesPerPixel>

<FM>Example<FC>
// saves 256 colormapped bitmap
ImageEnView1.IO.Params.BitsPerSample := 8;
ImageEnView1.IO.Params.SamplesPerPixel := 1;
ImageEnView1.IO.Params.SaveToFile('D:\Alfa.bmp');
!!}
    property BitsPerSample: integer read fBitsPerSample write fBitsPerSample;

{!!
<FS>TIOParamsVals.SamplesPerPixel

<FM>Declaration<FC>
property SamplesPerPixel: integer;

<FM>Description<FN>
Samples (channels) for each pixel.

Allowed values:
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>1<FN></C> <C>Single channel, colormapped or gray scale image</C> </R>
<R> <C><FC>3<FN></C> <C>Three channels, RGB or CIELab</C> </R>
<R> <C><FC>4<FN></C> <C>Four channels, CMYK or BGRA</C> </R>
</TABLE>

See also: <A TIOParamsVals.BitsPerSample>

<FM>Example<FC>
// saves 256 colormapped bitmap
ImageEnView1.IO.Params.BitsPerSample := 8;
ImageEnView1.IO.Params.SamplesPerPixel := 1;
ImageEnView1.IO.Params.SaveToFile('D:\Alfa.bmp');
!!}
    property SamplesPerPixel: integer read fSamplesPerPixel write fSamplesPerPixel;

{!!
<FS>TIOParamsVals.Width

<FM>Declaration<FC>
property Width: integer;

<FM>Description<FN>
Width of the image in pixels.
!!}
    property Width: integer read fWidth write fWidth;

{!!
<FS>TIOParamsVals.Height

<FM>Declaration<FC>
property Height: integer;

<FM>Description<FN>
Height of the image in pixels.
!!}
    property Height: integer read fHeight write fHeight;

    property DpiX: integer read fDpiX write SetDpiX;
    property DpiY: integer read fDpiY write SetDpiY;

{!!
<FS>TIOParamsVals.Dpi

<FM>Declaration<FC>
property Dpi: Integer;

<FM>Description<FN>
The resolution (dots per inch) of the acquired image. If the horizontal and vertical resolutions aren't equal, Dpi is the horizontal resolution (the same as <A TIOParamsVals.DpiX>).

<FM>See Also<FN>
- <A TIOParamsVals.DpiX>
- <A TIOParamsVals.DpiY>
!!}
    property Dpi: integer read fDpiX write SetDpi;

{!!
<FS>TIOParamsVals.ColorMap

<FM>Declaration<FC>
property ColorMap: <A PRGBROW>; (read-only)

<FM>Description<FN>
Returns the color map of the current image.

Note: Not all image types have a valid colormap (in which case <FC>ColorMap<FN> will be nil).
                                                                            
See also: <A TIOParamsVals.ColorMapCount>

<FM>Example<FC>
// Show the palette of the file "myfile.gif"
var
  fPalDial : TImageEnPaletteDialog;
begin
  ImageEnView1.IO.LoadFromFile('C:\myfile.gif');
  fPalDial := TImageEnPaletteDialog.Create(self);
  fPalDial.SetPalette(ImageEnView1.IO.Params.ColorMap^, ImageEnView1.IO.Params.ColorMapCount);
  fPalDial.Execute;
  fPalDial.free;
end;
!!}
    property ColorMap: PRGBROW read fColorMap;

{!!
<FS>TIOParamsVals.ColorMapCount

<FM>Declaration<FC>
property ColorMapCount: integer; (read-only)

<FM>Description<FN>
Returns the number of entries in the <A TIOParamsVals.ColorMap> array.

See also: <A TIOParamsVals.ColorMap>
!!}
    property ColorMapCount: integer read fColorMapcount;

{!!
<FS>TIOParamsVals.ImageIndex

<FM>Declaration<FC>
property ImageIndex: Integer;

<FM>Description<FN>
Specifies the index of the next page/frame to load. You must set this property before calling read methods like LoadFromFile.
This property is valid for all multi-page file formats (TIFF, GIF, DCX, Dicom, etc..) and sets also other properties like <A TIOParamsVals.TIFF_ImageIndex>, <A TIOParamsVals.GIF_ImageIndex>, etc...

<FM>See Also<FN>
- <A TIOParamsVals.ImageCount>
- <A TImageEnIO.Seek>

<FM>Examples<FC>
// Want the second page of the TIFF
ImageEnView1.IO.Params.ImageIndex := 1;
ImageEnView1.IO.LoadFromFile('C:\input.tif');

// Print all pages of a TIFF
Printer.BeginDoc;
for I := 0 to ImageEnView1.IO.Params.ImageCount - 1 do
begin
  ImageEnView1.IO.Params.ImageIndex := I;
  ImageEnView1.IO.LoadFromFile('C:\input.tif');
  ImageEnView1.IO.PrintImage(Printer.Canvas);
end;
Printer.EndDoc;
!!}
    property ImageIndex: Integer read fImageIndex write SetImageIndex;

{!!
<FS>TIOParamsVals.ImageCount

<FM>Declaration<FC>
property ImageCount: integer

<FM>Description<FN>
Returns the number of pages/frames/images in the current file. Many image file formats can store multiple images, including TIFF, GIF, DCX, ICO and cursor. 

This property is valid for all file formats (i.e. returning 1 for formats that do not support multiple images). It provides generic access to file specific properties such as <A TIOParamsVals.GIF_ImageCount> and <A TIOParamsVals.TIFF_ImageCount>.

You can use <A TImageEnIO.Seek> to load/navigate images within the loaded file.

<FM>See Also<FN>
- <A TIOParamsVals.ImageCount>   
- <A TImageEnIO.Seek>

<FM>Examples<FC>
// Show number of pages in a TIFF
ImageEnView1.IO.LoadFromFile('C:\input.tif');
iPages := ImageEnView1.IO.Params.ImageCount;
ShowMessage('Page Count: ' + IntToStr(iPages)); 

// Print all pages of a TIFF
Printer.BeginDoc;
for I := 0 to ImageEnView1.IO.Params.ImageCount - 1 do
begin
  ImageEnView1.IO.Params.ImageIndex := I;
  ImageEnView1.IO.LoadFromFile('C:\input.tif');
  ImageEnView1.IO.PrintImage(Printer.Canvas);
end;
Printer.EndDoc;
!!}
    property ImageCount: Integer read fImageCount write SetImageCount;
                        
{!!
<FS>TIOParamsVals.ImageDelayTime

<FM>Declaration<FC>
property ImageDelayTime : Integer (Read-only)

<FM>Description<FN>
If the current file supports animation and has multiple frames, then this property provides access to the delay in MS (milliseconds) between images. E.g. if ImageDelayTime = 2000 then each frame should be shown for 2 seconds when animated.

This is a read-only property which provides ready access to one of:
- <A TIOParamsVals.AVI_FrameDelayTime>
- <A TIOParamsVals.GIF_DelayTime>
- <A TIOParamsVals.MEDIAFILE_FrameDelayTime>
- The frame interval <L TIOParamsVals.DICOM_Tags>tag</L> of DICOM files

<FM>Example<FC>
// Set frame load timer to correct interval to animate the image
FrameLoadTimer.Interval := ImageEnView1.IO.Params.ImageDelayTime;
!!}
    property ImageDelayTime : integer read GetImageDelayTime;



{!!
<FS>TIOParamsVals.GetThumbnail

<FM>Declaration<FC>
property GetThumbnail: Boolean;

<FM>Description<FN>
Specifies that the thumbnail for an image will be loaded instead of the full image. A thumbnail is available for images, such as those returned by digital cameras (EXIF Thumbnail).
If enabled and the file does not contain a thumbnail the full image will be automatically loaded instead.

The property sets both:
- <A TIOParamsVals.JPEG_GetExifThumbnail>
- <A TIOParamsVals.RAW_GetExifThumbnail>

Note: Must be set before loading an image

<FM>Example<FC>
// Load only the thumbnail of our image (if it has one)
ImageEnView1.IO.Params.GetThumbnail := true;
ImageEnView1.IO.LoadFromFile('C:\Input.jpg');
!!}
    property GetThumbnail: Boolean read fGetThumbnail write SetGetThumbnail;


{!!
<FS>TIOParamsVals.Dict

<FM>Declaration<FC>
property Dict: <A TIEDictionary>;

<FM>Description<FN>
Contains a dictionary of additional properties.
The key "XMP" will contain a sub dictionary parsed from XMP info.

<FM>Example<FC>
// loads first page of specified PDF rasterizing at 150dpi (assumes ImageMagick and GhostScript are installed)
TIEMiscPluginsImageMagick.RegisterPlugin();
ImageEnView1.IO.Params.ImageIndex := 0;
ImageEnView1.IO.Params.Dict.Insert('PDF:Density', 150);
ImageEnView1.IO.LoadFromFile('mybook.pdf');

// gets XMP doc id
docid := ImageEnView1.IO.Params.Dict.GetString('xapMM:DocumentID', true);
!!}
    property Dict: TIEDictionary read fDict;


{!!
<FS>TIOParamsVals.IsResource

<FM>Declaration<FC>
property IsResource: Boolean;

<FM>Description<FN>
Some formats (BMP, ICO, CUR) do not contain all headers when stored as resources (e.g. in EXE, DLL, etc...). If you set IsResource = true, ImageEn will skip loading of these headers so the image can be loaded correctly.


<FM>Demo<FN>
InputOutput\ResourceLoader


<FM>Example<FC>
// loads resource 143 in "Bitmap" of "explorer.exe" (should be a little Windows logo)
var
  re: TIEResourceExtractor;
  buffer: Pointer;
  bufferLen: Integer;
begin
  re := TIEResourceExtractor.Create('explorer.exe');
  try
    buffer := re.GetBuffer('Bitmap', 'INTRESOURCE:143', bufferLen);
    ImageEnView1.IO.Params.IsResource := true;
    ImageEnView1.IO.LoadFromBuffer(buffer, bufferLen, ioBMP);
  finally
    re.Free;
  end;
end;

!!}
    property IsResource: Boolean read fIsResource write SetIsResource;

{!!
<FS>TIOParamsVals.EnableAdjustOrientation

<FM>Declaration<FC>
property EnableAdjustOrientation: Boolean;

<FM>Description<FN>
If enabled before loading a file which contains EXIF orientation information, the image will be automatically rotated for display (the actual file is not modified).
Orientation information is often found in digital photos from high-end cameras. ImageEn uses the data found in <A TIOParamsVals.EXIF_Orientation> or <A TIOParamsVals.TIFF_Orientation> to determine the correct orientation.

Note: This property also sets <A TIOParamsVals.JPEG_EnableAdjustOrientation> and <A TIOParamsVals.TIFF_EnableAdjustOrientation>.
              
Default: False

<FM>Example<FC>
ImageEnView1.IO.Params.EnableAdjustOrientation := True;
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
!!}
    property EnableAdjustOrientation: Boolean read fEnableAdjustOrientation write SetEnableAdjustOrientation;

    // ICC
    property InputICCProfile: TIEICC read GetInputICC;
    property OutputICCProfile: TIEICC read GetOutputICC;
    property DefaultICCProfile: TIEICC read GetDefaultICC;

    // IPTC

{!!
<FS>TIOParamsVals.IPTC_Info

<FM>Declaration<FC>
property IPTC_Info: <A TIEIPTCInfoList>;

<FM>Description<FN>
A list of all IPTC information contained in a file. View object details at <A TIEIPTCInfoList>.

IPTC records can contains text, objects and images. Applications can read/write information from IPTC_Info using string objects or a memory buffer.
Each IPTC_Info item has a record number and a dataset number. These values specify the type of data contained in the item, according to IPTC - NAA Information Interchange Model Version 4 (See: <L http://www.iptc.org>www.iptc.org</L>).
For JPEG files ImageEn can read/write IPTC fields from the APP13 marker.
ImageEn can also read/write IPTC textual information of PhotoShop (access In Photoshop under "File Info"). View a list <L IPTC items compatible with Adobe PhotoShop>IPTC Photoshop items</L>.

Note: A set of IPTC helper functions are available in <FB>iexMetaHelpers.pas<FN>

<FM>Examples<FC>
// Read the PhotoShop image description
ImageEnView1.IO.LoadFromFile('C:\image.jpg');
Idx := ImageEnView1.IO.Params.IPTC_Info.IndexOf(2, 120);
Caption := ImageEnView1.IO.Params.IPTC_Info.StringItem[idx];

// write the image description
ImageEnView1.IO.Params.IPTC_Info.StringItem[idx] := 'This is the new caption';
ImageEnView1.IO.SaveToFile('D:\image.jpg');


<FM>See Also<FN>
- <L IPTC items compatible with Adobe PhotoShop>Photoshop IPTC consts</L>
- <A TIOParamsVals.IPTC_Photoshop>
- <A TIOParamsVals.ReadIPTCField>
- <A TIOParamsVals.WriteIPTCField>
- <A TIOParamsVals.ClearIPTCField>
!!}
    property IPTC_Info: TIEIPTCInfoList read fIPTC_Info;

    property IPTC_Photoshop[FieldID: Integer]: string read GetIPTC_Photoshop write SetIPTC_Photoshop;

    // Imaging annotations
    {$ifdef IEINCLUDEIMAGINGANNOT}
    property ImagingAnnot: TIEImagingAnnot read GetImagingAnnot;
    {$endif}

{!!
<FS>TIOParamsVals.ImageEnAnnot

<FM>Declaration<FC>
property ImageEnAnnot: <A TIEImageEnAnnot>;

<FM>Description<FN>
Provides access to the <A TImageEnVect> objects loaded (or to be saved) from a TIFF or JPEG.

Using <A TIEImageEnAnnot> object you can create new objects, copy to a <A TImageEnVect> (as vectorial objects), copy from a <A TImageEnVect> (from vectorial objects) or just draw on the bitmap.

<FM>Example<FC>
// Load an image and all annotations from input.tif . This allows the annotations to be edited:
ImageEnVect1.IO.LoadFromFile('C:\input.tif');
ImageEnVect1.IO.Params.ImageEnAnnot.CopyToTImageEnVect();

// Load an image and all annotations from input.tif, but just draw annotation on the image (display only, cannot be edited):
ImageEnVect1.IO.LoadFromFile('C:\input.tif');
ImageEnVect1.IO.Params.ImageEnAnnot.DrawToBitmap( ImageEnVect1.IEBitmap, True );
ImageEnVect1.Update();

// Save an image and all annotations to output.tif
ImageEnVect1.IO.Params.ImageEnAnnot.CopyFromTImageEnVect();
ImageEnVect1.IO.SaveToFile('C:\output.tif');
!!}
    property ImageEnAnnot: TIEImageEnAnnot read fImageEnAnnot;

    // TIFF

{!!
<FS>TIOParamsVals.TIFF_Compression

<FM>Declaration<FC>
property TIFF_Compression: <A TIOTIFFCompression>;

<FM>Description<FN>
Specifies the compression type for TIFF images.

!!}
    property TIFF_Compression: TIOTIFFCompression read fTIFF_Compression write fTIFF_Compression;

{!!
<FS>TIOParamsVals.TIFF_ImageIndex

<FM>Declaration<FC>
property TIFF_ImageIndex: integer;

<FM>Description<FN>
The index (zero-based) of the current page of the loaded TIFF image. You can also use <A TImageEnIO.Seek> to load/navigate images within the loaded file.

Note: You can use <A TIOParamsVals.ImageIndex> for generic access to the ImageIndex (not specific to an image format).

<FM>See Also<FN>
- <A TIOParamsVals.ImageIndex>   
- <A TIOParamsVals.TIFF_ImageCount>
- <A TImageEnIO.Seek>

<FM>Examples<FC>
// Want the second page of the TIFF
ImageEnView1.IO.Params.TIFF_ImageIndex := 1;
ImageEnView1.IO.LoadFromFile('C:\input.tif');

// Print all pages of a TIFF
Printer.BeginDoc;
for I := 0 to ImageEnView1.IO.Params.TIFF_ImageCount - 1 do
begin
  ImageEnView1.IO.Params.TIFF_ImageIndex := I;
  ImageEnView1.IO.LoadFromFile('C:\input.tif');
  ImageEnView1.IO.PrintImage(Printer.Canvas);
end;
Printer.EndDoc;
!!}
    property TIFF_ImageIndex: integer read fTIFF_ImageIndex write SetImageIndex;

{!!
<FS>TIOParamsVals.TIFF_SubIndex

<FM>Declaration<FC>
property TIFF_SubIndex: Integer;

<FM>Description<FN>
A TIFF can contain several pages. You can load image or parameters from a page using <A TIOParamsVals.TIFF_ImageIndex> (or just <A TIOParamsVals.ImageIndex>). Each page can also contain sub-images, so you can load a specific sub-page using <FC>TIFF_SubIndex<FN>.
This is useful, for example, to load an embedded Jpeg from a DNG (camera raw format), which is located in the first page and the second sub-index.

<FM>Example<FC>
// Load the embedded Jpeg from a DNG
ImageEnView1.IO.Params.TIFF_ImageIndex := 0;
ImageEnView1.IO.Params.TIFF_SubIndex := 1;
ImageEnView1.IO.LoadFromFileTIFF('DCS001.DNG');
!!}
    property TIFF_SubIndex: Integer read fTIFF_SubIndex write fTIFF_SubIndex;

{!!
<FS>TIOParamsVals.TIFF_ImageCount

<FM>Declaration<FC>
property TIFF_ImageCount: Integer; (Read-only)

<FM>Description<FN>
Returns the number of images/pages contained in the current TIFF. You can use <A TImageEnIO.Seek> to load/navigate images within the loaded file.

Note: You can use <A TIOParamsVals.ImageCount> for generic access to the image count (not specific to an image format). For non-component access, see <A EnumTiffIm>.

<FM>See Also<FN>
- <A TIOParamsVals.ImageCount>   
- <A TIOParamsVals.TIFF_ImageIndex>
- <A TImageEnIO.Seek>
- <A EnumTiffIm>

<FM>Examples<FC>
// Show number of pages in a TIFF
ImageEnView1.IO.LoadFromFile('C:\input.tif');
iPages := ImageEnView1.IO.Params.ImageCount;
ShowMessage('Page Count: ' + IntToStr(iPages)); 

// Print all pages of a TIFF
Printer.BeginDoc;
for I := 0 to ImageEnView1.IO.Params.ImageCount - 1 do
begin
  ImageEnView1.IO.Params.ImageIndex := I;
  ImageEnView1.IO.LoadFromFile('C:\input.tif');
  ImageEnView1.IO.PrintImage(Printer.Canvas);
end;
Printer.EndDoc;
!!}
    property TIFF_ImageCount: integer read fTIFF_ImageCount write SetImageCount;

{!!
<FS>TIOParamsVals.TIFF_PhotometInterpret

<FM>Declaration<FC>
property TIFF_PhotometInterpret: <A TIOTIFFPhotometInterpret>;

<FM>Description<FN>
The Photometric interpretation of a TIFF image.

!!}
    property TIFF_PhotometInterpret: TIOTIFFPhotometInterpret read fTIFF_PhotometInterpret write fTIFF_PhotometInterpret;

{!!
<FS>TIOParamsVals.TIFF_PlanarConf

<FM>Declaration<FC>
property TIFF_PlanarConf: integer;

<FM>Description<FN>
The Planar configuration of a TIFF image
1: The component values for each pixel are stored contiguously (ex. RGBRGBRGB...)
2: The components are stored in separate component planes (ex. RRRRRR...GGGGGGG.....BBBBBB)
!!}
    property TIFF_PlanarConf: integer read fTIFF_PlanarConf write fTIFF_PlanarConf;

{!!
<FS>TIOParamsVals.TIFF_NewSubfileType

<FM>Declaration<FC>
property TIFF_NewSubfileType: integer;

<FM>Description<FN>
A general indication of the kind of data contained in the current sub-file.

This field is made up of a set of 32 flag bits.

Currently defined values are:
<TABLE>
<R> <H>Bit</H> <H>Description</H> </R>
<R> <C><FC>Bit 0<FN></C> <C>is 1 if the image is a reduced-resolution version of another image in this TIFF file, else the bit is 0.</C> </R>
<R> <C><FC>Bit 1<FN></C> <C>is 1 if the image is a single page of a multi-page image (see the PageNumber field description), else the bit is 0.</C> </R>
<R> <C><FC>Bit 2<FN></C> <C>is 1 if the image defines a transparency mask for another image in this TIFF file. The PhotometricInterpretation value must be 4, designating a transparency mask.</C> </R>
</TABLE>

Default is 0.
!!}
    property TIFF_NewSubfileType: integer read fTIFF_NewSubfileType write fTIFF_NewSubfileType;

{!!
<FS>TIOParamsVals.TIFF_XPos

<FM>Declaration<FC>
property TIFF_XPos: integer;

<FM>Description<FN>
X position of the Top-left of the original scanned image.

!!}
    property TIFF_XPos: integer read fTIFF_XPos write fTIFF_XPos;

{!!
<FS>TIOParamsVals.TIFF_GetTile

<FM>Declaration<FC>
property TIFF_GetTile: integer;

<FM>Description<FN>
Specifies the tile number to load when loading a tiled TIFF file.

-1 (default) means "Load all tiles".
!!}
    property TIFF_GetTile: integer read fTIFF_GetTile write fTIFF_GetTile;

{!!
<FS>TIOParamsVals.TIFF_YPos

<FM>Declaration<FC>
property TIFF_YPos: integer;

<FM>Description<FN>
Y position of the top-left of the original scanned image.

!!}
    property TIFF_YPos: integer read fTIFF_YPos write fTIFF_YPos;

{!!
<FS>TIOParamsVals.TIFF_DocumentName

<FM>Declaration<FC>
property TIFF_DocumentName: AnsiString;

<FM>Description<FN>
Specifies the document name field of a TIFF image.

<FM>Example<FC>
ImageEnView1.IO.Params.TIFF_DocumentName := 'Sanremo';
ImageEnView1.IO.Params.TIFF_ImageDescription := 'The city of the flowers';
ImageEnView1.IO.SaveToFile('D:\Italy.tif');
!!}
    property TIFF_DocumentName: AnsiString read fTIFF_DocumentName write fTIFF_DocumentName;

{!!
<FS>TIOParamsVals.TIFF_ImageDescription

<FM>Declaration<FC>
property TIFF_ImageDescription: AnsiString;

<FM>Description<FN>
Specifiy the image description field of a TIFF image format.

<FM>Example<FC>
ImageEnView1.IO.Params.TIFF_DocumentName := 'Sanremo';
ImageEnView1.IO.Params.TIFF_ImageDescription := 'The city of the flowers';
ImageEnView1.IO.SaveToFile('D:\Italy.tif');
!!}
    property TIFF_ImageDescription: AnsiString read fTIFF_ImageDescription write fTIFF_ImageDescription;

{!!
<FS>TIOParamsVals.TIFF_PageName

<FM>Declaration<FC>
property TIFF_PageName: AnsiString;

<FM>Description<FN>
The page name of a TIFF image.

!!}
    property TIFF_PageName: AnsiString read fTIFF_PageName write fTIFF_PageName;

{!!
<FS>TIOParamsVals.TIFF_PageNumber

<FM>Declaration<FC>
property TIFF_PageNumber: integer;

<FM>Description<FN>
The page number of a TIFF image.

!!}
    property TIFF_PageNumber: integer read fTIFF_PageNumber write fTIFF_PageNumber;

{!!
<FS>TIOParamsVals.TIFF_PageCount

<FM>Declaration<FC>
property TIFF_PageCount: integer;

<FM>Description<FN>
Returns the number of pages in a TIFF file.

!!}
    property TIFF_PageCount: integer read fTIFF_PageCount write fTIFF_PageCount;

{!!
<FS>TIOParamsVals.TIFF_Orientation

<FM>Declaration<FC>
property TIFF_Orientation: Integer;

<FM>Description<FN>
Specifies the orientation of the original image. ImageEn can automatically display an image with the correct  if <A TIOParamsVals.TIFF_EnableAdjustOrientation> is True.

Allowed values:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>_exoCorrectOrientation (1)<FN></C> <C>Image is Orientated Correctly (top left side)</C> </R>
<R> <C><FC>_exoNeedsHorizontalFlip (2)<FN></C> <C>Image is Horizontally Flipped (top right side)</C> </R>
<R> <C><FC>_exoNeeds180Rotate (3)<FN></C> <C>Image is Offset by 180º (bottom right side)</C> </R>
<R> <C><FC>_exoNeedsVerticalFlip (4)<FN></C> <C>Image is Vertically Flipped (bottom left side)</C> </R>
<R> <C><FC>_exoNeedsHorzAndVertFlip (5)<FN></C> <C>Image is Flipped Horiz. and Offset 90º CCW (left side top)</C> </R>
<R> <C><FC>_exoNeeds90RotateCW (6)<FN></C> <C>Image is Offset by 90º CCW (right side top)</C> </R>
<R> <C><FC>_exoNeedsFlipHorzAnd90Rotate (7)<FN></C> <C>Image is Flipped Horiz. and offset 90º CW (right side bottom)</C> </R>
<R> <C><FC>_exoNeeds270RotateCW (8)<FN></C> <C>Image is Offset by 90º clockwise (left side bottom)</C> </R>
</TABLE>

Support for orientations other than 1 is not a baseline TIFF requirement.

Read-only
!!}
    property TIFF_Orientation: integer read fTIFF_Orientation write SetTIFF_Orientation;

{!!
<FS>TIOParamsVals.TIFF_EnableAdjustOrientation

<FM>Declaration<FC>
property TIFF_EnableAdjustOrientation: Boolean;

<FM>Description<FN>
If enabled before loading a file which contains EXIF orientation information, the image will be automatically rotated for display (the actual file is not modified).
Orientation information is often found in digital photos from high-end cameras. ImageEn uses the data found in <A TIOParamsVals.TIFF_Orientation> to determine the correct orientation for TIFF images.

See also: <A TIOParamsVals.EnableAdjustOrientation>

Default: False
!!}



    property TIFF_EnableAdjustOrientation: boolean read fTIFF_EnableAdjustOrientation write fTIFF_EnableAdjustOrientation;

    property TIFF_LZWDecompFunc: TTIFFLZWDecompFunc read fTIFF_LZWDecompFunc write fTIFF_LZWDecompFunc;

    property TIFF_LZWCompFunc: TTIFFLZWCompFunc read fTIFF_LZWCompFunc write fTIFF_LZWCompFunc;

{!!
<FS>TIOParamsVals.TIFF_JPEGQuality

<FM>Declaration<FC>
property TIFF_JPEGQuality: Integer;

<FM>Description<FN>
Specifies the quality factor for a TIFF compressed as JPEG. Range is 1 to 100.
High values improve image quality but require more disk space.
!!}
    property TIFF_JPEGQuality: integer read fTIFF_JPEGQuality write fTIFF_JPEGQuality;

{!!
<FS>TIOParamsVals.TIFF_JPEGColorSpace

<FM>Declaration<FC>
property TIFF_JPEGColorSpace: <A TIOJPEGColorSpace>;

<FM>Description<FN>
Specifies the desired color space for the Jpeg included in a TIFF file.

<FM>Example<FC>
ImageEnView.IO.Params.TIFF_JPEGColorSpace := ioJPEG_RGB;
ImageEnView.IO.Params.TIFF_Compression := ioTIFF_JPEG;
ImageEnView.IO.SaveToFile('D:\output.tif');
!!}
    property TIFF_JPEGColorSpace: TIOJPEGColorSpace read fTIFF_JPEGColorSpace write fTIFF_JPEGColorSpace;

{!!
<FS>TIOParamsVals.TIFF_FillOrder

<FM>Declaration<FC>
property TIFF_FillOrder: integer;

<FM>Description<FN>
Specifies the logical order of bits within a byte.
1: Pixels are arranged within a byte such that pixels with lower column values are stored in the higher-order bits of the byte.
2: Pixels are arranged within a byte such that pixels with lower column values are stored in the lower-order bits of the byte.
!!}
    property TIFF_FillOrder: integer read fTIFF_FillOrder write fTIFF_FillOrder;

{!!
<FS>TIOParamsVals.TIFF_ZIPCompression

<FM>Declaration<FC>
property TIFF_ZIPCompression: Integer;

<FM>Description<FN>
Specifies the compression level used when using ioTIFF_ZIP compression.
0: Fastest
1: Normal (default)
2: Maximum compression
!!}
    property TIFF_ZIPCompression: Integer read fTIFF_ZIPCompression write fTIFF_ZIPCompression;

{!!
<FS>TIOParamsVals.TIFF_StripCount

<FM>Declaration<FC>
property TIFF_StripCount: Integer;

<FM>Description<FN>
Specifies the number of strips to use when creating a TIFF file.
0 (default) means this value is chosen automatically.

Note: TIFF_StripCount is not read when loading a TIFF.

<FM>Example<FC>
// Create a TIFF with a unique strip
ImageEnView1.IO.TIFF_StripCount := 1;
ImageEnView1.IO.SaveToFile('D:\out.tiff');
!!}
    property TIFF_StripCount: Integer read fTIFF_StripCount write fTIFF_StripCount;

{!!
<FS>TIOParamsVals.TIFF_ByteOrder

<FM>Declaration<FC>
property TIFF_ByteOrder: <A TIOByteOrder>;

<FM>Description<FN>
Specifies the byte order of the current TIFF image.

This property is read-only because ImageEn always uses ioLittleEndian when saving TIFF files.

ioBigEndian byte order is used by Motorola, while ioLittleEndian is used by Intel processors.

This function is useful when using <A InsertTIFFImageFile> or <A InsertTIFFImageStream> which merge two tiff images without decoding them, because the created TIFF will be unreadable if the images have different byte orders.
!!}
    property TIFF_ByteOrder: TIOByteOrder read fTIFF_ByteOrder write fTIFF_ByteOrder;


{!!
<FS>TIOParamsVals.TIFF_PhotoshopImageResources

<FM>Declaration<FC>
property TIFF_PhotoshopImageResources: <A TIEArrayOfByte>;

<FM>Description<FN>
Contains Photoshop raw image resources. ImageEn doesn't use this data, but will maintain it when saving.
!!}
    property TIFF_PhotoshopImageResources: TIEArrayOfByte read fTIFF_PhotoshopImageResources write fTIFF_PhotoshopImageResources;

{!!
<FS>TIOParamsVals.TIFF_PhotoshopImageSourceData

<FM>Declaration<FC>
property TIFF_PhotoshopImageSourceData: <A TIEArrayOfByte>;

<FM>Description<FN>
Contains Photoshop raw image source data. ImageEn doesn't use this data, but will maintain it for saving.
!!}
    property TIFF_PhotoshopImageSourceData: TIEArrayOfByte read fTIFF_PhotoshopImageSourceData write fTIFF_PhotoshopImageSourceData;


{!!
<FS>TIOParamsVals.TIFF_BigTIFF

<FM>Declaration<FC>
property TIFF_BigTIFF: boolean;

<FM>Description<FN>
If true, the TIFF is a BigTIFF (>4GB).
!!}
    property TIFF_BigTIFF: boolean read fTIFF_BigTIFF write fTIFF_BigTIFF;



    // GIF

{!!
<FS>TIOParamsVals.GIF_Version

<FM>Declaration<FC>
property GIF_Version: AnsiString;

<FM>Description<FN>
Returns the GIF standard version, either "GIF87a" or "GIF89a".

!!}
    property GIF_Version: AnsiString read fGIF_Version write fGIF_Version;

{!!
<FS>TIOParamsVals.GIF_ImageIndex

<FM>Declaration<FC>
property GIF_ImageIndex: integer;

<FM>Description<FN>
The index (zero-based) of the current image of the loaded GIF image. You can also use <A TImageEnIO.Seek> to load/navigate images within the loaded file.

Note: You can use <A TIOParamsVals.ImageIndex> for generic access to the ImageIndex (not specific to an image format).

<FM>See Also<FN>
- <A TIOParamsVals.ImageIndex>  
- <A TIOParamsVals.GIF_ImageCount>
- <A TImageEnIO.Seek>

<FM>Examples<FC>
// Want the second page of the GIF
ImageEnView1.IO.Params.GIF_ImageIndex := 1;
ImageEnView1.IO.LoadFromFile('C:\Animation.gif');

// Print all images of a GIF
Printer.BeginDoc;
for I := 0 to ImageEnView1.IO.Params.GIF_ImageCount - 1 do
begin
  ImageEnView1.IO.Params.GIF_ImageIndex := I;
  ImageEnView1.IO.LoadFromFile('C:\input.gif');
  ImageEnView1.IO.PrintImage(Printer.Canvas);
end;
Printer.EndDoc;

!!}
    property GIF_ImageIndex: integer read fGIF_ImageIndex write SetImageIndex;

{!!
<FS>TIOParamsVals.GIF_ImageCount

<FM>Declaration<FC>
property GIF_ImageCount: Integer; (Read-only)

<FM>Description<FN>
Returns the number of images contained in the currently loaded GIF. You can use <A TImageEnIO.Seek> to load/navigate images within the loaded file.

Note: You can use <A TIOParamsVals.ImageCount> for generic access to the image count (not specific to an image format). For non-component access, see <A EnumGifIm>.

<FM>See Also<FN>
- <A TIOParamsVals.ImageCount>   
- <A TIOParamsVals.GIF_ImageIndex>
- <A TImageEnIO.Seek>
- <A EnumGifIm>

<FM>Examples<FC>
// Return the frame count of 'animated.gif' without load the GIF
ImageEnView1.IO.ParamsFromFile('C:\Animated.gif');
iImageCount := ImageEnView1.IO.Params.GIF_ImageCount;

// Print all images of a GIF
Printer.BeginDoc;
for I := 0 to ImageEnView1.IO.Params.GIF_ImageCount - 1 do
begin
  ImageEnView1.IO.Params.GIF_ImageIndex := I;
  ImageEnView1.IO.LoadFromFile('C:\input.gif');
  ImageEnView1.IO.PrintImage(Printer.Canvas);
end;
Printer.EndDoc;
!!}
    property GIF_ImageCount: integer read fGIF_ImageCount write SetImageCount;

{!!
<FS>TIOParamsVals.GIF_XPos

<FM>Declaration<FC>
property GIF_XPos: integer;

<FM>Description<FN>
The X coordinate where the top-left of the frame will be shown.

Due to optimization secondary frames of a GIF may not contain all the image data of the original frame, but only what data has changed. This property allows you to draw the subsequent frames at the correct position.

!!}
    property GIF_XPos: integer read fGIF_XPos write fGIF_XPos;

{!!
<FS>TIOParamsVals.GIF_YPos

<FM>Declaration<FC>
property GIF_YPos: integer;

<FM>Description<FN>
The Y coordinate where the top-left of the frame will be shown.

Due to optimization secondary frames of a GIF may not contain all the image data of the original frame, but only what data has changed. This property allows you to draw the subsequent frames at the correct position.
!!}
    property GIF_YPos: integer read fGIF_YPos write fGIF_YPos;

{!!
<FS>TIOParamsVals.GIF_DelayTime

<FM>Declaration<FC>
property GIF_DelayTime: integer;

<FM>Description<FN>
The period (in 1/100th sec) for which the current frame remains shown.
      
<FM>Example<FC>
ImageEnView1.IO.Params.GIF_DelayTime := 10;        // Set current frame to display for 1/10th of a second
!!}
    property GIF_DelayTime: integer read fGIF_DelayTime write fGIF_DelayTime;

{!!
<FS>TIOParamsVals.GIF_FlagTranspColor

<FM>Declaration<FC>
property GIF_FlagTranspColor: boolean;

<FM>Description<FN>
If enabled then the <A TIOParamsVals.GIF_TranspColor> property specifies the transparency color of the image. If false, <A TIOParamsVals.GIF_TranspColor> is not valid.

Note: If the image contains an alpha channel (transparency channel) this property is handled automatically.

<FM>Example<FC>
ImageEnView1.IO.Params.GIF_FlagTranspColor := True;
ImageEnView1.IO.Params.GIF_TranspColor := CreateRGB(0, 0, 0);  // black is the transparent color
!!}
    property GIF_FlagTranspColor: boolean read fGIF_FlagTranspColor write fGIF_FlagTranspColor;

{!!
<FS>TIOParamsVals.GIF_TranspColor

<FM>Declaration<FC>
property GIF_TranspColor: <A TRGB>;

<FM>Description<FN>
Specifies the transparency color.

Notes:
- Only valid if <A TIOParamsVals.GIF_FlagTranspColor> = True
- The specified color should exist within the image

!!}
    property GIF_TranspColor: TRGB read fGIF_TranspColor write fGIF_TranspColor;

{!!
<FS>TIOParamsVals.GIF_Interlaced

<FM>Declaration<FC>
property GIF_Interlaced: boolean;

<FM>Description<FN>
Returns True if the GIF image is interlaced.

!!}
    property GIF_Interlaced: boolean read fGIF_Interlaced write fGIF_Interlaced;

{!!
<FS>TIOParamsVals.GIF_WinWidth

<FM>Declaration<FC>
property GIF_WinWidth: integer;

<FM>Description<FN>
Returns the width of the window where the GIF is shown.

!!}
    property GIF_WinWidth: integer read fGIF_WinWidth write fGIF_WinWidth;

{!!
<FS>TIOParamsVals.GIF_WinHeight

<FM>Declaration<FC>
property GIF_WinHeight: integer;

<FM>Description<FN>                                 
Returns the height of the window where the GIF is shown.

!!}
    property GIF_WinHeight: integer read fGIF_WinHeight write fGIF_WinHeight;

{!!
<FS>TIOParamsVals.GIF_Background

<FM>Declaration<FC>
property GIF_Background: <A TRGB>;

<FM>Description<FN>
Returns the background color of the GIF.

Note: This color should exist in the image.

!!}
    property GIF_Background: TRGB read fGIF_Background write fGIF_Background;

{!!
<FS>TIOParamsVals.GIF_Ratio

<FM>Declaration<FC>
property GIF_Ratio: integer;

<FM>Description<FN>
Returns the aspect ratio of the GIF image, which is calculated as follows:

<FC>Width / Height = (Ratio + 15) / 64<FN>

!!}
    property GIF_Ratio: integer read fGIF_Ratio write fGIF_Ratio;

{!!
<FS>TIOParamsVals.GIF_Comments

<FM>Declaration<FC>
property GIF_Comments: TStringList;

<FM>Description<FN>
Returns the text comments contained in a GIF file.

<FM>Example<FC>
ImageEnIO.Params.Gif_Comments.Clear;
ImageEnIO.Params.GIF_Comments.Add('Hello world!');
ImageEnIO.SaveToFile('D:\output.gif');
!!}
    property GIF_Comments: TStringList read fGIF_Comments;

{!!
<FS>TIOParamsVals.GIF_Action

<FM>Declaration<FC>
property GIF_Action: <A TIEGIFAction>;

<FM>Description<FN>
Specifies how frames are displayed in a multi-frame GIF.
!!}
    property GIF_Action: TIEGIFAction read fGIF_Action write fGIF_Action;

{!!
<FS>TIOParamsVals.GIF_RAWLoad

<FM>Declaration<FC>
property GIF_RAWLoad: boolean;

<FM>Description<FN>
If true ImageEn doesn't process the GIF disposal method (i.e. doesn't merge frames when required).
This property is not applicable to multi-page loading (like TImageEnMIO and TImageEnMView).
If <A TImageEnIO> is attached to TImageEnView, the layer positions (<A TIELayer.PosX> and <A TIELayer.PosY>) are also set. 
!!}
    property GIF_RAWLoad: boolean read fGIF_RAWLoad write fGIF_RAWLoad;

    property GIF_LZWDecompFunc: TGIFLZWDecompFunc read fGIF_LZWDecompFunc write fGIF_LZWDecompFunc;
    property GIF_LZWCompFunc: TGIFLZWCompFunc read fGIF_LZWCompFunc write fGIF_LZWCompFunc;

    // DCX

{!!
<FS>TIOParamsVals.DCX_ImageIndex

<FM>Declaration<FC>
property DCX_ImageIndex: Integer;

<FM>Description<FN>
The index (zero-based) of the current page of the loaded DCX image. You can also use <A TImageEnIO.Seek> to load/navigate images within the loaded file.

Note: You can use <A TIOParamsVals.ImageIndex> for generic access to the ImageIndex (not specific to an image format).

<FM>See Also<FN>
- <A TIOParamsVals.ImageCount>
- <A TImageEnIO.Seek>
- <A EnumDCXIm>

<FM>Examples<FC>
// Load the second page of 'input.dcx'
ImageEnView1.IO.Params.DCX_ImageIndex := 1;
ImageEnView1.IO.LoadFromFile('C:\input.dcx');

// Print all pages of a DCX image
Printer.BeginDoc;
for I := 0 to ImageEnView1.IO.Params.ImageCount - 1 do
begin
  ImageEnView1.IO.Params.DCX_ImageIndex := I;
  ImageEnView1.IO.LoadFromFile('C:\input.dcx');
  ImageEnView1.IO.PrintImage(Printer.Canvas);
end;
Printer.EndDoc;
!!}
    property DCX_ImageIndex: Integer read fDCX_ImageIndex write SetImageIndex;

    // JPEG

{!!
<FS>TIOParamsVals.JPEG_ColorSpace

<FM>Declaration<FC>
property JPEG_ColorSpace: <A TIOJPEGColorSpace>;

<FM>Description<FN>
Specifies the saved/loaded color space. Default is ioJPEG_YCbCr.
!!}
    property JPEG_ColorSpace: TIOJPEGColorSpace read fJPEG_ColorSpace write fJPEG_ColorSpace;

{!!
<FS>TIOParamsVals.JPEG_Quality

<FM>Declaration<FC>
property JPEG_Quality: integer;

<FM>Description<FN>
The Quality factor for the current JPEG image. Range is 1 to 100, though typical range is 60 (low quality) to 95 (high quality).
Higher values will improve image quality but require more disk space.

Note: This property is not stored in a JPEG image, so it is never set when loading an image. Instead you set it when saving a JPEG to specify your desired compression.
It is possible to estimate the original JPEG compression using <A IECalcJpegStreamQuality> or <A IECalcJpegFileQuality>.

<FM>Example<FC>
// load a jpeg and save it using the same compression quality
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
ImageEnView1.IO.Params.JPEG_Quality := IECalcJpegFileQuality('C:\input.jpg');
ImageEnView1.IO.SaveToFile('D:\output.jpg');
!!}
    property JPEG_Quality: integer read fJPEG_Quality write fJPEG_Quality;

{!!
<FS>TIOParamsVals.JPEG_DCTMethod

<FM>Declaration<FC>
property JPEG_DCTMethod: <A TIOJPEGDCTMethod>;

<FM>Description<FN>
Specifies the DCT method of the current JPEG.

!!}
    property JPEG_DCTMethod: TIOJPEGDCTMethod read fJPEG_DCTMethod write fJPEG_DCTMethod;

{!!
<FS>TIOParamsVals.JPEG_CromaSubsampling

<FM>Declaration<FC>
property JPEG_CromaSubsampling: <A TIOJPEGCromaSubsampling>;

<FM>Description<FN>
Specifies the croma sub-sampling, which affects the JPEG quality.

Note: This property is valid only when <A TIOParamsVals.JPEG_ColorSpace> = ioJPEG_YCbCr.
!!}
    property JPEG_CromaSubsampling: TIOJPEGCromaSubsampling read fJPEG_CromaSubsampling write fJPEG_CromaSubsampling;

{!!
<FS>TIOParamsVals.JPEG_OptimalHuffman

<FM>Declaration<FC>
property JPEG_OptimalHuffman: boolean;

<FM>Description<FN>
If enabled, an optimal Huffman table is used by the JPEG compressor (to improve the compression level). If False, a standard table is used.

!!}
    property JPEG_OptimalHuffman: boolean read fJPEG_OptimalHuffman write fJPEG_OptimalHuffman;

{!!
<FS>TIOParamsVals.JPEG_Smooth

<FM>Declaration<FC>
property JPEG_Smooth: integer;

<FM>Description<FN>
The smoothing factor for the JPEG. Range is 0 (No smoothing) to 100 is (max). If JPEG_Smooth is not zero, the JPEG compressor smoothes the image before compressing it. This improves the level of compression.

!!}
    property JPEG_Smooth: integer read fJPEG_Smooth write fJPEG_Smooth;

{!!
<FS>TIOParamsVals.JPEG_Progressive

<FM>Declaration<FC>
property JPEG_Progressive: boolean;

<FM>Description<FN>
Returns true if the current image is a progressive JPEG.

!!}
    property JPEG_Progressive: boolean read fJPEG_Progressive write fJPEG_Progressive;

{!!
<FS>TIOParamsVals.JPEG_Scale

<FM>Declaration<FC>
property JPEG_Scale: <A TIOJPEGScale>;

<FM>Description<FN>
Specifies the size at which to load a JPEG image. It is used to speed up loading when a full size image is not required.

<FM>Example<FC>
// This is the fastest way to load a thumbnailed jpeg of about 100x100 pixels
ImageEnView1.IO.Params.Width := 100;
ImageEnView1.IO.Params.Height := 100;
ImageEnView1.IO.Params.JPEG_Scale := ioJPEG_AUTOCALC;
ImageEnView1.IO.LoadFromFile('C:\myimage.jpg');
!!}
    property JPEG_Scale: TIOJPEGScale read fJPEG_Scale write fJPEG_Scale;


{!!
<FS>TIOParamsVals.JPEG_MarkerList

<FM>Declaration<FC>
property JPEG_MarkerList: <A TIEMarkerList>;

<FM>Description<FN>
Contains a list of of the markers within a JPEG file (from APP0 to APP15 and COM). JPEG markers can contain text, objects and images. Applications can read/write raw markers from <FC>JPEG_MarkerList<FN> using a stream or memory buffer.

<FM>Examples<FC>
// Read the JPEG_COM marker  (idx is integer, Comment is string)
ImageEnView1.IO.LoadFromFile('C:\image.jpg');
Idx := ImageEnView1.IO.Params.JPEG_MarkerList.IndexOf(JPEG_COM);
Comment := ImageEnView1.IO.Params.JPEG_MarkerList.MarkerData[idx];

// Now writes the JPEG_COM marker
Comment := 'This is the new comment';
ImageEnView1.IO.Params.JPEG_MarkerList.SetMarker(idx, JPEG_COM, PAnsiChar(Comment), length(Comment));
ImageEnView1.IO.SaveToFile('D:\image.jpg');
!!}
    property JPEG_MarkerList: TIEMarkerList read fJPEG_MarkerList;

{!!
<FS>TIOParamsVals.JPEG_Scale_Used

<FM>Declaration<FC>
property JPEG_Scale_Used: Integer;

<FM>Description<FN>
Returns the denominator of the scale used to load the current Jpeg image (e.g. if it was loaded at 1/4 size, then 4 will be returned). It is used only when <A TIOParamsVals.JPEG_Scale> is <FC>ioJPEG_AUTOCALC<FN>.

<FM>Example<FC>
ImageEnView1.IO.Params.Width := 100;
ImageEnView1.IO.Params.Height := 100;
ImageEnView1.IO.Params.JPEG_Scale := ioJPEG_AUTOCALC;
ImageEnView1.IO.LoadFromFile('C:\my.jpg');
Case ImageEnView1.IO.Params.JPEG_Scale_Used of
  2: ShowMessage('Half');
  4: ShowMessage('Quarter');
  8: ShowMessage('Eighth');
end;
!!}
    property JPEG_Scale_Used: integer read fJPEG_Scale_Used write fJPEG_Scale_Used;

{!!
<FS>TIOParamsVals.JPEG_WarningTot

<FM>Declaration<FC>
property JPEG_WarningTot: Integer;

<FM>Description<FN>
Returns a count of all warnings encountered while loading the current Jpeg.

Note: You should use the <A TImageEnIO.Aborting> property to see if the image is corrupt.
!!}
    property JPEG_WarningTot: integer read fJPEG_WarningTot write fJPEG_WarningTot;

{!!
<FS>TIOParamsVals.JPEG_WarningCode

<FM>Declaration<FC>
property JPEG_WarningCode: Integer;

<FM>Description<FN>
Returns the last warning code encountered while loading the current Jpeg.


<TABLE>
<R> <H>Value</H> <H>Error</H> <H>Description</H> </R>
<R> <C>0</C> <C><FC>ARITH_NOTIMPL<FN></C> <C>There are legal restrictions on arithmetic coding</C> </R>
<R> <C>1</C> <C><FC>BAD_ALIGN_TYPE<FN></C> <C>ALIGN_TYPE is wrong</C> </R>
<R> <C>2</C> <C><FC>BAD_ALLOC_CHUNK <FN></C> <C>MAX_ALLOC_CHUNK is wrong</C> </R>
<R> <C>3</C> <C><FC>BAD_BUFFER_MODE <FN></C> <C>Bogus buffer control mode</C> </R>
<R> <C>4</C> <C><FC>BAD_COMPONENT_ID <FN></C> <C>Invalid component ID in SOS</C> </R>
<R> <C>5</C> <C><FC>BAD_DCT_COEF <FN></C> <C>DCT coefficient out of range</C> </R>
<R> <C>6</C> <C><FC>BAD_DCTSIZE <FN></C> <C>IDCT output block size not supported</C> </R>
<R> <C>7</C> <C><FC>BAD_HUFF_TABLE <FN></C> <C>Bogus Huffman table definition</C> </R>
<R> <C>8</C> <C><FC>BAD_IN_COLORSPACE <FN></C> <C>Bogus input colorspace</C> </R>
<R> <C>9</C> <C><FC>BAD_J_COLORSPACE <FN></C> <C>Bogus JPEG colorspace</C> </R>
<R> <C>10</C> <C><FC>BAD_LENGTH <FN></C> <C>Bogus marker length</C> </R>
<R> <C>11</C> <C><FC>BAD_LIB_VERSION<FN></C> <C>Wrong JPEG library version</C> </R>
<R> <C>12</C> <C><FC>BAD_MCU_SIZE <FN></C> <C>Sampling factors too large for interleaved scan</C> </R>
<R> <C>13</C> <C><FC>BAD_POOL_ID <FN></C> <C>Invalid memory pool code</C> </R>
<R> <C>14</C> <C><FC>BAD_PRECISION <FN></C> <C>Unsupported JPEG data precision</C> </R>
<R> <C>15</C> <C><FC>BAD_PROGRESSION <FN></C> <C>Invalid progressive parameters</C> </R>
<R> <C>16</C> <C><FC>BAD_PROG_SCRIPT <FN></C> <C>Invalid progressive parameters</C> </R>
<R> <C>17</C> <C><FC>BAD_SAMPLING <FN></C> <C>Bogus sampling factors</C> </R>
<R> <C>18</C> <C><FC>BAD_SCAN_SCRIPT <FN></C> <C>Invalid scan script</C> </R>
<R> <C>19</C> <C><FC>BAD_STATE <FN></C> <C>Improper call to JPEG library</C> </R>
<R> <C>20</C> <C><FC>BAD_STRUCT_SIZE <FN></C> <C>JPEG parameter struct mismatch </C> </R>
<R> <C>21</C> <C><FC>BAD_VIRTUAL_ACCESS <FN></C> <C>Bogus virtual array access</C> </R>
<R> <C>22</C> <C><FC>BUFFER_SIZE <FN></C> <C>Buffer passed to JPEG library is too small</C> </R>
<R> <C>23</C> <C><FC>CANT_SUSPEND <FN></C> <C>Suspension not allowed here</C> </R>
<R> <C>24</C> <C><FC>CCIR601_NOTIMPL <FN></C> <C>CCIR601 sampling not implemented yet</C> </R>
<R> <C>25</C> <C><FC>COMPONENT_COUNT <FN></C> <C>Too many color components </C> </R>
<R> <C>26</C> <C><FC>CONVERSION_NOTIMPL <FN></C> <C>Unsupported color conversion request</C> </R>
<R> <C>27</C> <C><FC>DAC_INDEX <FN></C> <C>Bogus DAC </C> </R>
<R> <C>28</C> <C><FC>DAC_VALUE <FN></C> <C>Bogus DAC </C> </R>
<R> <C>29</C> <C><FC>DHT_INDEX <FN></C> <C>Bogus DHT </C> </R>
<R> <C>30</C> <C><FC>DQT_INDEX <FN></C> <C>Bogus DQT </C> </R>
<R> <C>31</C> <C><FC>EMPTY_IMAGE <FN></C> <C>Empty JPEG image (DNL not supported)</C> </R>
<R> <C>32</C> <C><FC>EMS_READ <FN></C> <C>Read from EMS failed</C> </R>
<R> <C>33</C> <C><FC>EMS_WRITE <FN></C> <C>Write to EMS failed</C> </R>
<R> <C>34</C> <C><FC>EOI_EXPECTED <FN></C> <C>Didn't expect more than one scan</C> </R>
<R> <C>35</C> <C><FC>FILE_READ <FN></C> <C>Input file read error</C> </R>
<R> <C>36</C> <C><FC>FILE_WRITE <FN></C> <C>Output file write error - out of disk space?</C> </R>
<R> <C>37</C> <C><FC>FRACT_SAMPLE_NOTIMPL <FN></C> <C>Fractional sampling not implemented yet</C> </R>
<R> <C>38</C> <C><FC>HUFF_CLEN_OVERFLOW <FN></C> <C>Huffman code size table overflow</C> </R>
<R> <C>39</C> <C><FC>HUFF_MISSING_CODE <FN></C> <C>Missing Huffman code table entry</C> </R>
<R> <C>40</C> <C><FC>IMAGE_TOO_BIG <FN></C> <C>Image too big</C> </R>
<R> <C>41</C> <C><FC>INPUT_EMPTY <FN></C> <C>Empty input file</C> </R>
<R> <C>42</C> <C><FC>INPUT_EOF <FN></C> <C>Premature end of input file</C> </R>
<R> <C>43</C> <C><FC>MISMATCHED_QUANT_TABLE <FN></C> <C>Cannot transcode due to multiple use of quantization table</C> </R>
<R> <C>44</C> <C><FC>MISSING_DATA <FN></C> <C>Scan script does not transmit all data</C> </R>
<R> <C>45</C> <C><FC>MODE_CHANGE <FN></C> <C>Invalid color quantization mode change</C> </R>
<R> <C>46</C> <C><FC>NOTIMPL <FN></C> <C>Not implemented yet</C> </R>
<R> <C>47</C> <C><FC>NOT_COMPILED <FN></C> <C>Requested feature was omitted at compile time</C> </R>
<R> <C>48</C> <C><FC>NO_BACKING_STORE <FN></C> <C>Backing store not supported</C> </R>
<R> <C>49</C> <C><FC>NO_HUFF_TABLE <FN></C> <C>Huffman table was not defined</C> </R>
<R> <C>50</C> <C><FC>NO_IMAGE <FN></C> <C>JPEG datastream contains no image</C> </R>
<R> <C>51</C> <C><FC>NO_QUANT_TABLE <FN></C> <C>Quantization table was not defined</C> </R>
<R> <C>52</C> <C><FC>NO_SOI <FN></C> <C>Not a Jpeg file</C> </R>
<R> <C>53</C> <C><FC>OUT_OF_MEMORY <FN></C> <C>Insufficient memory</C> </R>
<R> <C>54</C> <C><FC>QUANT_COMPONENTS <FN></C> <C>Cannot quantize</C> </R>
<R> <C>55</C> <C><FC>QUANT_FEW_COLORS <FN></C> <C>Cannot quantize</C> </R>
<R> <C>56</C> <C><FC>QUANT_MANY_COLORS <FN></C> <C>Cannot quantize</C> </R>
<R> <C>57</C> <C><FC>SOF_DUPLICATE <FN></C> <C>Invalid Jpeg file structure: two SOF markers</C> </R>
<R> <C>58</C> <C><FC>SOF_NO_SOS <FN></C> <C>Invalid Jpeg file structure: missing SOS marker</C> </R>
<R> <C>59</C> <C><FC>SOF_UNSUPPORTED <FN></C> <C>Unsupported JPEG process</C> </R>
<R> <C>60</C> <C><FC>SOI_DUPLICATE <FN></C> <C>Invalid Jpeg file structure: two SOI markers</C> </R>
<R> <C>61</C> <C><FC>SOS_NO_SOF <FN></C> <C>Invalid Jpeg file structure: SOS before SOF</C> </R>
<R> <C>62</C> <C><FC>TFILE_CREATE <FN></C> <C>Failed to create temporary file</C> </R>
<R> <C>63</C> <C><FC>TFILE_READ <FN></C> <C>Read failed on temporary file</C> </R>
<R> <C>64</C> <C><FC>TFILE_SEEK <FN></C> <C>Seek failed on temporary file</C> </R>
<R> <C>65</C> <C><FC>TFILE_WRITE <FN></C> <C>Write failed on temporary file - out of disk space?</C> </R>
<R> <C>66</C> <C><FC>TOO_LITTLE_DATA <FN></C> <C>Application transferred too few scanlines</C> </R>
<R> <C>67</C> <C><FC>UNKNOWN_MARKER <FN></C> <C>Unsupported marker</C> </R>
<R> <C>68</C> <C><FC>VIRTUAL_BUG <FN></C> <C>Virtual array controller messed up</C> </R>
<R> <C>69</C> <C><FC>WIDTH_OVERFLOW <FN></C> <C>Image too wide for this implementation</C> </R>
<R> <C>70</C> <C><FC>XMS_READ <FN></C> <C>Read from XMS failed</C> </R>
<R> <C>71</C> <C><FC>XMS_WRITE <FN></C> <C>Write to XMS failed</C> </R>
<R> <C>72</C> <C><FC>COPYRIGHT</C> </R>
<R> <C>73</C> <C><FC>VERSION</C> </R>
<R> <C>74</C> <C><FC>16BIT_TABLES <FN></C> <C>Caution: quantization tables are too coarse for baseline JPEG</C> </R>
<R> <C>75</C> <C><FC>ADOBE <FN></C> <C>Adobe APP14 marker</C> </R>
<R> <C>76</C> <C><FC>APP0 <FN></C> <C>Unknown APP0 marker</C> </R>
<R> <C>77</C> <C><FC>APP14 <FN></C> <C>Unknown APP14 marker</C> </R>
<R> <C>78</C> <C><FC>DAC <FN></C> <C>Define Arithmetic Table</C> </R>
<R> <C>79</C> <C><FC>DHT <FN></C> <C>Define Huffman Table</C> </R>
<R> <C>80</C> <C><FC>DQT <FN></C> <C>Define Quantization Table</C> </R>
<R> <C>81</C> <C><FC>DRI <FN></C> <C>Define Restart Interval</C> </R>
<R> <C>82</C> <C><FC>EMS_CLOSE <FN></C> <C>Freed EMS handle
<R> <C>83</C> <C><FC>EMS_OPEN <FN></C> <C>Obtained EMS handle</C> </R>
<R> <C>84</C> <C><FC>EOI <FN></C> <C>End Of Image</C> </R>
<R> <C>85</C> <C><FC>HUFFBITS</C> </R>
<R> <C>86</C> <C><FC>JFIF <FN></C> <C>JFIF APP0 marker </C> </R>
<R> <C>87</C> <C><FC>JFIF_BADTHUMBNAILSIZE <FN></C> <C>Warning: thumbnail image size does not match data length</C> </R>
<R> <C>88</C> <C><FC>JFIF_EXTENSION <FN></C> <C>JFIF extension marker</C> </R>
<R> <C>89</C> <C><FC>JFIF_THUMBNAIL</C> </R>
<R> <C>90</C> <C><FC>MISC_MARKER <FN></C> <C>Miscellaneous marker</C> </R>
<R> <C>91</C> <C><FC>PARMLESS_MARKER <FN></C> <C>Unexpected marker</C> </R>
<R> <C>92</C> <C><FC>QUANTVALS</C> </R>
<R> <C>93</C> <C><FC>QUANT_3_NCOLORS</C> </R>
<R> <C>94</C> <C><FC>QUANT_NCOLORS</C> </R>
<R> <C>95</C> <C><FC>QUANT_SELECTED</C> </R>
<R> <C>96</C> <C><FC>RECOVERY_ACTION</C> </R>
<R> <C>97</C> <C><FC>RST</C> </R>
<R> <C>98</C> <C><FC>SMOOTH_NOTIMPL <FN></C> <C>Smoothing not supported with nonstandard sampling ratios</C> </R>
<R> <C>99</C> <C><FC>SOF</C> </R>
<R> <C>100</C> <C><FC>SOF_COMPONENT</C> </R>
<R> <C>101</C> <C><FC>SOI <FN></C> <C>Start of Image</C> </R>
<R> <C>102</C> <C><FC>SOS <FN></C> <C>Start Of Scan</C> </R>
<R> <C>103</C> <C><FC>SOS_COMPONENT</C> </R>
<R> <C>104</C> <C><FC>SOS_PARAMS</C> </R>
<R> <C>105</C> <C><FC>TFILE_CLOSE <FN></C> <C>Closed temporary file</C> </R>
<R> <C>106</C> <C><FC>TFILE_OPEN <FN></C> <C>Opened temporary file</C> </R>
<R> <C>107</C> <C><FC>THUMB_JPEG <FN></C> <C>JFIF extension marker </C> </R>
<R> <C>108</C> <C><FC>THUMB_PALETTE <FN></C> <C>JFIF extension marker </C> </R>
<R> <C>109</C> <C><FC>THUMB_RGB <FN></C> <C>JFIF extension marker </C> </R>
<R> <C>110</C> <C><FC>UNKNOWN_IDS <FN></C> <C>Unrecognized component IDs, assuming YCbCr</C> </R>
<R> <C>111</C> <C><FC>XMS_CLOSE <FN></C> <C>Freed XMS handle</C> </R>
<R> <C>112</C> <C><FC>XMS_OPEN <FN></C> <C>Obtained XMS handle</C> </R>
<R> <C>113</C> <C><FC>ADOBE_XFORM <FN></C> <C>Unknown Adobe color transform code</C> </R>
<R> <C>114</C> <C><FC>BOGUS_PROGRESSION <FN></C> <C>Inconsistent progression sequence </C> </R>
<R> <C>115</C> <C><FC>EXTRANEOUS_DATA <FN></C> <C>Corrupt JPEG data: extraneous bytes before marker</C> </R>
<R> <C>116</C> <C><FC>HIT_MARKER <FN></C> <C>Corrupt JPEG data: premature end of data segment</C> </R>
<R> <C>117</C> <C><FC>HUFF_BAD_CODE <FN></C> <C>Corrupt JPEG data: bad Huffman code</C> </R>
<R> <C>118</C> <C><FC>JFIF_MAJOR <FN></C> <C>Warning: unknown JFIF revision number</C> </R>
<R> <C>119</C> <C><FC>JPEG_EOF <FN></C> <C>Premature end of Jpeg file</C> </R>
<R> <C>120</C> <C><FC>MUST_RESYNC <FN></C> <C>Corrupt JPEG data </C> </R>
<R> <C>121</C> <C><FC>NOT_SEQUENTIAL <FN></C> <C>Invalid SOS parameters for sequential JPEG</C> </R>
<R> <C>122</C> <C><FC>TOO_MUCH_DATA <FN></C> <C>Application transferred too many scanlines</C> </R>
</TABLE>
!!}
    property JPEG_WarningCode: integer read fJPEG_WarningCode write fJPEG_WarningCode;

{!!
<FS>TIOParamsVals.JPEG_OriginalWidth

<FM>Declaration<FC>
property JPEG_OriginalWidth: Integer;

<FM>Description<FN>
Returns the actual width of a Jpeg image before it was scaled (using <A TIOParamsVals.JPEG_Scale>).

Note: This is the same value as <A TIOParamsVals.OriginalWidth>.

See also: <A TIOParamsVals.JPEG_OriginalHeight>
!!}
    property JPEG_OriginalWidth: integer read fOriginalWidth write fOriginalWidth;

{!!
<FS>TIOParamsVals.JPEG_OriginalHeight

<FM>Declaration<FC>
property JPEG_OriginalHeight: Integer;

<FM>Description<FN>    
Returns the actual height of a Jpeg image before it was scaled (using <A TIOParamsVals.JPEG_Scale>).

Note: This is the same value as <A TIOParamsVals.OriginalHeight>.

See also: <A TIOParamsVals.JPEG_OriginalWidth>
!!}
    property JPEG_OriginalHeight: integer read fOriginalHeight write fOriginalHeight;

{!!
<FS>TIOParamsVals.OriginalWidth

<FM>Declaration<FC>
property OriginalWidth: Integer;

<FM>Description<FN>
Returns the actual width of an image before it was scaled during loading.

<L TIOParamsVals.JPEG_Scale>JPEG</L> and Raw images can be loaded at a reduce size to speed up the loading. This property allows you to access the true size of the image.

See also: <A TIOParamsVals.OriginalHeight>
!!}
    property OriginalWidth: integer read fOriginalWidth write fOriginalWidth;

{!!
<FS>TIOParamsVals.OriginalHeight

<FM>Declaration<FC>
property OriginalHeight: Integer;

<FM>Description<FN>     
Returns the actual height of an image before it was scaled during loading.

<L TIOParamsVals.JPEG_Scale>JPEG</L> and Raw images can be loaded at a reduce size to speed up the loading. This property allows you to access the true size of the image.

See also: <A TIOParamsVals.OriginalWidth>
!!}      
    property OriginalHeight: integer read fOriginalHeight write fOriginalHeight;



{!!
<FS>TIOParamsVals.JPEG_EnableAdjustOrientation

<FM>Declaration<FC>
property JPEG_EnableAdjustOrientation: boolean;

<FM>Description<FN>
If enabled before loading a file which contains EXIF orientation information, the image will be automatically rotated for display (the actual file is not modified).
Orientation information is often found in digital photos from high-end cameras. ImageEn uses the data found in <A TIOParamsVals.EXIF_Orientation> to determine the correct orientation for JPEG images.

See also: <A TIOParamsVals.EnableAdjustOrientation>

<FM>Example<FC>
ImageEnView1.IO.Params.JPEG_EnableAdjustOrientation := True;
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
!!}
    property JPEG_EnableAdjustOrientation: boolean read fJPEG_EnableAdjustOrientation write fJPEG_EnableAdjustOrientation;

{!!
<FS>TIOParamsVals.JPEG_GetExifThumbnail

<FM>Declaration<FC>
property JPEG_GetExifThumbnail: Boolean;

<FM>Description<FN>   
Specifies that the thumbnail for an image will be loaded instead of the full image. A thumbnail is often available for JPEG images from digital cameras (EXIF Thumbnail).
If enabled and the file does not contain a thumbnail the full image will be automatically loaded instead.

See also: <A TIOParamsVals.GetThumbnail>

<FM>Example<FC>
// Load only the thumbnail
ImageEnView1.IO.Params.JPEG_GetExifThumbnail := True;
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
!!}
    property JPEG_GetExifThumbnail: Boolean read fJPEG_GetExifThumbnail write SetJPEG_GetExifThumbnail;

    // JPEG2000
{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TIOParamsVals.J2000_ColorSpace

<FM>Declaration<FC>
property J2000_ColorSpace: <A TIOJ2000ColorSpace>;

<FM>Description<FN>
Specifies the currently loaded/saved color space.

!!}
    property J2000_ColorSpace: TIOJ2000ColorSpace read fJ2000_ColorSpace write fJ2000_ColorSpace;

{!!
<FS>TIOParamsVals.J2000_Rate

<FM>Declaration<FC>
property J2000_Rate : Double;

<FM>Description<FN>
Specifies the compression rate to use when saving a JPEG2000 image. Allowed values are from 0 to 1, where 1 is no compression (lossless mode).

<FM>Example<FC>
// Save without quality loss
ImageEnIO.Params.J2000_Rate := 1;
ImageEnIO.SaveToFile('D:\output.jp2');

// Save with lossy compression
ImageEnView1.IO.Params.J2000_Rate := 0.015
ImageEnView1.IO.SaveToFile('D:\output.jp2');
!!}
    property J2000_Rate: double read fJ2000_Rate write fJ2000_Rate;

{!!
<FS>TIOParamsVals.J2000_ScalableBy

<FM>Declaration<FC>
property J2000_ScalableBy: <A TIOJ2000ScalableBy>;

<FM>Description<FN>
Specifies the JPEG2000 progression order.
!!}
    property J2000_ScalableBy: TIOJ2000ScalableBy read fJ2000_ScalableBy write fJ2000_ScalableBy;

{$ENDIF}

    // PCX

{!!
<FS>TIOParamsVals.PCX_Version

<FM>Declaration<FC>
property PCX_Version: integer;

<FM>Description<FN>
Returns the PCX Version (Default: 5).
!!}
    property PCX_Version: integer read fPCX_Version write fPCX_Version;

{!!
<FS>TIOParamsVals.PCX_Compression

<FM>Declaration<FC>
property PCX_Compression: <A TIOPCXCompression>;

<FM>Description<FN>
Specifies the compression type for a PCX image.
!!}
    property PCX_Compression: TIOPCXCompression read fPCX_Compression write fPCX_Compression;

    // BMP

{!!
<FS>TIOParamsVals.BMP_Version

<FM>Declaration<FC>
property BMP_Version: <A TIOBMPVersion>;

<FM>Description<FN>
Returns the version of the BMP file.

<FM>Example<FC>
// Save a Windows 3.x bitmap
ImageEnView1.IO.Params.BMP_Version := ioBMP_BM3;
ImageEnView1.IO.SaveToFile('D:\alfa.bmp');
!!}
    property BMP_Version: TIOBMPVersion read fBMP_Version write fBMP_Version;

{!!
<FS>TIOParamsVals.BMP_Compression

<FM>Declaration<FC>
property BMP_Compression: <A TIOBMPCompression>;

<FM>Description<FN>
Specifies the compression method when saving a BMP image.

Note: Only 16 or 256 color bitmap can be saved with RLE compression.

<FM>Example<FC>
// Save a compressed bitmap
ImageEnView1.IO.Params.BMP_Compression := ioBMP_RLE;
ImageEnView1.IO.SaveToFile('D:\alfa.bmp');
!!}
    property BMP_Compression: TIOBMPCompression read fBMP_Compression write fBMP_Compression;

{!!
<FS>TIOParamsVals.BMP_HandleTransparency

<FM>Declaration<FC>
property BMP_HandleTransparency: Boolean;

<FM>Description<FN>
BMP files can have up to 32 bits per pixel. This property controls how to interpret the extra byte in the 32 bit word.
When BMP_HandleTransparency is true the extra byte is interpreted as an alpha channel, otherwise it is just ignored.

So if BMP_HandleTransparency = True the image will be displayed in ImageEn with transparency. Whereas if BMP_HandleTransparency = False the transparent color will nto be used.

<FM>Example<FC>
// if BMP_HandleTransparency = true then display with transparency
ImageENVect1.IO.Params.BMP_HandleTransparency := chkBMPHandleTransparency.Checked;
ImageENVect1.IO.LoadFromFile( FilePath );
!!}
    property BMP_HandleTransparency: boolean read fBMP_HandleTransparency write fBMP_HandleTransparency;

    // ICO

{!!
<FS>TIOParamsVals.ICO_ImageIndex

<FM>Declaration<FC>
property ICO_ImageIndex: Integer;

<FM>Description<FN>
The index (zero-based) of the current image of the current icon file. An icon file can contain multiple images of varying sizes and color depths. Use <A TIOParamsVals.ImageCount> to return the number of images the file contains.

Note: You can use <A TIOParamsVals.ImageIndex> for generic access to the ImageIndex (not specific to an image format).

<FM>See Also<FN>
- <A TIOParamsVals.ImageCount>   
- <A EnumICOIm>

<FM>Example<FC>
// Load the second image inside 'alfa.ico'
ImageEnView1.IO.Params.ICO_ImageIndex := 1;
ImageEnView1.IO.LoadFromFile('C:\alfa.ico');
!!}
    property ICO_ImageIndex: integer read fICO_ImageIndex write SetImageIndex;

{!!
<FS>TIOParamsVals.ICO_Background

<FM>Declaration<FC>
property ICO_background: <A TRGB>;

<FM>Description<FN>
Specifies the color that is used for transparency (i.e. becomes the image background) when loading icon files.

Note: This property must be set before loading. It is not used when saving because only the image's alpha channel is used for transparency. To make a color transparent you use <A TImageEnProc.SetTransparentColors> before saving.

<FM>Example<FC>
// Reading
ImageEnView1.IO.Params.ICO_Background := CreateRGB(255, 255, 255);
ImageEnView1.IO.LoadFromFileICO('D:\myicon.ico');

// Writing example
ImageEnView1.Proc.SetTransparentColors(transparent_color, transparent_color, 0);
ImageEnView1.IO.SaveToFileICO('D:\myicon.ico');

!!}
    property ICO_Background: TRGB read fICO_Background write fICO_Background;

    // CUR

{!!
<FS>TIOParamsVals.CUR_ImageIndex

<FM>Declaration<FC>
property CUR_ImageIndex: Integer;

<FM>Description<FN>
The index (zero-based) of the current image/cursor of the current cursor file  (CUR files can contain multiple images). Use <A TIOParamsVals.ImageCount> to return the number of images the file contains.

<FM>Example<FC>
// Load the second cursor inside 'alfa.cur'
ImageEnView1.IO.Params.CUR_ImageIndex := 1;
ImageEnView1.IO.LoadFromFile('C:\alfa.cur');
!!}
    property CUR_ImageIndex: integer read fCUR_ImageIndex write SetImageIndex;

{!!
<FS>TIOParamsVals.CUR_XHotSpot

<FM>Declaration<FC>
property CUR_XHotSpot: Integer;

<FM>Description<FN>
Specifies the horizontal position of the "hot spot". The hot spot is the point of the cursor that activates a click.

See also: <A TIOParamsVals.CUR_YHotSpot>
!!}
    property CUR_XHotSpot: integer read fCUR_XHotSpot write fCUR_XHotSpot;

{!!
<FS>TIOParamsVals.CUR_YHotSpot

<FM>Declaration<FC>
property CUR_YHotSpot: Integer;

<FM>Description<FN>
Specifies the vertical position of the "hot spot". The hot spot is the point of the cursor that activates a click.

See also: <A TIOParamsVals.CUR_XHotSpot>
!!}
    property CUR_YHotSpot: integer read fCUR_YHotSpot write fCUR_YHotSpot;

{!!
<FS>TIOParamsVals.CUR_Background

<FM>Declaration<FC>
property CUR_Background: <A TRGB>;

<FM>Description<FN>
Specifies the color that is used for transparency (i.e. becomes the image background) when loading cursor files.

Note: This property must be set before loading. 

<FM>Example<FC>
ImageEnView1.IO.Params.CUR_Background := CreateRGB(255, 255, 255);
ImageEnView1.IO.LoadFromFileCUR('C:\myicon.cur');
!!}
    property CUR_Background: TRGB read fCUR_Background write fCUR_Background;

{$ifdef IEINCLUDEDICOM}

    // DICOM

{!!
<FS>TIOParamsVals.DICOM_Tags

<FM>Declaration<FC>
property DICOM_Tags: <A TIEDicomTags>;

<FM>Description<FN>
Provides access to the object which contains all the informational tags within a DICOM file.

You can view a list of tags at: <L Dicom Tags Supported by ImageEn>Dicom Tag List</L>

<FM>Demo<FN>
InputOutput\Dicom

<FM>Example<FC>
ImageType := ImageEnView1.IO.Params.DICOM_Tags.GetTagString( tags.IndexOf($0008, $0008) );
PatientName := ImageEnView1.IO.Params.DICOM_Tags.GetTagString( tags.IndexOf($0010, $0010) );
!!}

    property DICOM_Tags: TIEDicomTags read fDICOM_Tags;

{!!
<FS>TIOParamsVals.DICOM_WindowCenterOffset

<FM>Declaration<FC>
property DICOM_WindowCenterOffset: Double;

<FM>Description<FN>
Returns the calculated offset of the window center.
!!}
    property DICOM_WindowCenterOffset: Double read fDICOM_WindowCenterOffset write fDICOM_WindowCenterOffset;

    property DICOM_RescaleIntercept: Double read fDICOM_RescaleIntercept write fDICOM_RescaleIntercept;
    property DICOM_RescaleSlope: Double read fDICOM_RescaleSlope write fDICOM_RescaleSlope;
{!!
<FS>TIOParamsVals.DICOM_Range

<FM>Declaration<FC>
property DICOM_Range: <A TIEDicomRange>;

<FM>Description<FN>
Specifies how to handle the pixels visibility range.
!!}
    property DICOM_Range: TIEDicomRange read fDICOM_Range write fDICOM_Range;

{!!
<FS>TIOParamsVals.DICOM_Compression

<FM>Declaration<FC>
property DICOM_Compression: <A TIEDicomCompression>;

<FM>Description<FN>
The compression used for a DICOM file.
!!}
    property DICOM_Compression: TIEDicomCompression read GetDICOM_Compression write SetDICOM_Compression;

{!!
<FS>TIOParamsVals.DICOM_JPEGQuality

<FM>Declaration<FC>
property DICOM_JPEGQuality: integer;

<FM>Description<FN>
Specifies the quality factor (ranging from 1 to 100) to use when saving a DICOM file as a lossy JPEG.

Higher values improve image quality but require more disk space.
!!}
    property DICOM_JPEGQuality: integer read fDICOM_JPEGQuality write fDICOM_JPEGQuality;

{!!
<FS>TIOParamsVals.DICOM_J2000Rate

<FM>Declaration<FC>
property DICOM_J2000Rate: double;

<FM>Description<FN>
Specifies the compression rate to use when saving a DICOM file as JPEG 2000.
Allowed values are from 0 to 1, where 1 is no compression (lossless mode).
!!}
    property DICOM_J2000Rate: double read fDICOM_J2000Rate write fDICOM_J2000Rate;

{$endif}

    // PNG

{!!
<FS>TIOParamsVals.PNG_Interlaced

<FM>Declaration<FC>
property PNG_Interlaced: Boolean;

<FM>Description<FN>
Returns true if the current PNG image is interlaced.
!!}
    property PNG_Interlaced: boolean read fPNG_Interlaced write fPNG_Interlaced;

{!!
<FS>TIOParamsVals.PNG_Background

<FM>Declaration<FC>
property PNG_Background: <A TRGB>;

<FM>Description<FN>
Specifies the background color of the image.
!!}
    property PNG_Background: TRGB read fPNG_Background write fPNG_Background;

{!!
<FS>TIOParamsVals.PNG_Filter

<FM>Declaration<FC>
property PNG_Filter: <A TIOPNGFilter>;

<FM>Description<FN>
Specifies the filter to use when saving a PNG file. PNG filters can have a significant impact on the file size and encoding (i.e. saving) speed, though, generally the effect on the decoding/loading speed is minimal.

<FM>Example<FC>
// Set best compression
ImageEnView1.IO.Params.PNG_Filter := ioPNG_FILTER_PAETH;
ImageEnView1.IO.Params.PNG_Compression := 9;
// Save PNG
ImageEnView1.IO.SaveToFilePNG('D:\max.png');
!!}
    property PNG_Filter: TIOPNGFilter read fPNG_Filter write fPNG_Filter;

{!!
<FS>TIOParamsVals.PNG_Compression

<FM>Declaration<FC>
property PNG_Compression: Integer;

<FM>Description<FN>
Determines how much time the PNG compressor will spend trying to compress image data when saving a PNG.
Allowed values are from 0 (no compression) to 9 (best compression).

<FM>Example<FC>
// Set best compression
ImageEnView1.IO.Params.PNG_Filter := ioPNG_FILTER_PAETH;
ImageEnView1.IO.Params.PNG_Compression := 9;
// Save PNG
ImageEnView1.IO.SaveToFilePNG('max.png');
!!}
    property PNG_Compression: integer read fPNG_Compression write fPNG_Compression;

{!!
<FS>TIOParamsVals.PNG_TextKeys

<FM>Declaration<FC>
property PNG_TextKeys: TStringList;

<FM>Description<FN>
Contains the keys associated with the list of values (<A TIOParamsVals.PNG_TextValues> property).

Note: PNG_TextKeys and <A TIOParamsVals.PNG_TextValues> must contain the same number of entries. Only uncompressed text is supported.

<FM>Examples<FC>
// Add author and other text info to a PNG file
ImageEnView1.IO.Params.PNG_TextKeys.Add('Author');
ImageEnView1.IO.Params.PNG_TextValues.Add('Letizia');
ImageEnView1.IO.Params.PNG_TextKeys.Add('Subject');
ImageEnView1.IO.Params.PNG_TextValues.Add('Colosseo');
ImageEnView1.IO.SaveToFile('D:\output.png');

// read all text info from a PNG
for i := 0 to ImageEnView1.IO.Params.PNG_TextKeys.Count - 1 do
begin
  key := ImageEnView1.IO.Params.PNG_TextKeys[i];
  value := ImageEnView1.IO.Params.PNG_TextValues[I];
end;
!!}
    property PNG_TextKeys: TStringList read fPNG_TextKeys;

{!!
<FS>TIOParamsVals.PNG_TextValues

<FM>Declaration<FC>
property PNG_TextValues: TStringList;

<FM>Description<FN>
Contains the values associated with a list of keys (<A TIOParamsVals.PNG_TextKeys> property).

Note: <A TIOParamsVals.PNG_TextKeys> and PNG_TextValues must contain the same number of entries. Only uncompressed text is supported.

<FM>Example<FC>

<FM>Examples<FC>
// Add author and other text info to a PNG file
ImageEnView1.IO.Params.PNG_TextKeys.Add('Author');
ImageEnView1.IO.Params.PNG_TextValues.Add('Letizia');
ImageEnView1.IO.Params.PNG_TextKeys.Add('Subject');
ImageEnView1.IO.Params.PNG_TextValues.Add('Colosseo');
ImageEnView1.IO.SaveToFile('D:\output.png');

// read all text info from a PNG
for i := 0 to ImageEnView1.IO.Params.PNG_TextKeys.Count - 1 do
begin
  key := ImageEnView1.IO.Params.PNG_TextKeys[i];
  value := ImageEnView1.IO.Params.PNG_TextValues[I];
end;
!!}
    property PNG_TextValues: TStringList read fPNG_TextValues;

    //// PSD/PSB ////

{!!
<FS>TIOParamsVals.PSD_LoadLayers

<FM>Declaration<FC>
property PSD_LoadLayers: Boolean;

<FM>Description<FN>
When enabled ImageEn will load the separated layers of a PSD file. If False (default), only the merged image is loaded (flattened layers).

<FM>About Adobe PhotoShop Layers<FN>
PSD files are unique among the file types supported by ImageEnIO because the file stores layer information inside of the file including layer positions, layer names, layer dimensions and other unique information along with a merged image containing all the layers flattened onto a background.

When opening PSD files with ImageEnIO the layers can be loaded automatically by setting ImageEnView1.IO.Params.PSD_LoadLayers to true (before loading). If PSD_LoadLayers is True then the merged image is ignored and only the layers are loaded. If PSD_LoadLayers is False then only the merged image is loaded.

You can replace the layers automatically by setting ImageEnView1.IO.Params.PSD_ReplaceLayers to true before loading the file. If PSD_LoadLayers is True then the layers in the PSD file replace any layers already in ImageEnView. If PSD_ReplaceLayers is False then all layers in the PSD file will be added to the layers in ImageEnView. When PSD_ReplaceLayers is True, the content of the PSD file replaces the content of the ImageEnView. For example, assume a PSD file has three layers, and ImageEnView has two layers. This is the situation before loading the PSD file:

ImageEnView, Layer 0: image X
ImageEnView, Layer 1: image Y

Now, you load the PSD file with PSD_ReplaceLayers = True:  The result will be:
ImageEnView, Layer 0 : PSD layer 0
ImageEnView, Layer 1 : PSD layer 1
ImageEnView, Layer 2 : PSD layer 2

If PSD_ReplaceLayers was False, the result will be:
ImageEnView, Layer 0: image X
ImageEnView, Layer 1: image Y
ImageEnView, Layer 2 : PSD layer 0
ImageEnView, Layer 3 : PSD layer 1
ImageEnView, Layer 4 : PSD layer 2

ImageEn usually has a Layer 0 so when you load a PSD file, PSD_LoadLayers should be True and PSD_ReplaceLayers should be True:
ImageEnView1.IO.Params.PSD_LoadLayers := True;
ImageEnView1.IO.Params.PSD_ReplaceLayers := True;
ImageEnView1.IO.LoadFromFilePSD(iFilename);
ImageEnView1.Update;

If both PSD_LoadLayers and PSD_ReplaceLayers is true then the layers displayed by ImageEnView after the file is loaded will be the same as the layers that are in the PSD file. Generally, both of these params should be set to true before opening a PSD file.
If you do not set PSD_ReplaceLayers to True before loading the PSD file, ImageEnView will contain an empty layer 0 after the PSD file is opened.

<FM>Example<FC>
// loads a multilayer PSD and allow user to move and resize layers
ImageEnView1.IO.Params.PSD_LoadLayers := True;
ImageEnView1.IO.LoadFromFile('C:\input.psd');
ImageEnView1.MouseInteract := [miMoveLayers, miResizeLayers];

<FM>See Also<FN>
- <A TIOParamsVals.PSD_ReplaceLayers>
!!}
    property PSD_LoadLayers: Boolean read fPSD_LoadLayers write fPSD_LoadLayers;

{!!
<FS>TIOParamsVals.PSD_ReplaceLayers

<FM>Declaration<FC>
property PSD_ReplaceLayers: Boolean; (Default: False)

<FM>Description<FN>
If enabled, the layers of the current image are replaced by the content of the PSD file, otherwise the PSD file content is appended to the existing layers.

Note: See detail on PSD layer support at: <A TIOParamsVals.PSD_LoadLayers>

<FM>See Also<FN>
- <A TIOParamsVals.PSD_LoadLayers>
!!}
    property PSD_ReplaceLayers: Boolean read fPSD_ReplaceLayers write fPSD_ReplaceLayers;

{!!
<FS>TIOParamsVals.PSD_HasPremultipliedAlpha

<FM>Declaration<FC>
property PSD_HasPremultipliedAlpha: Boolean; (Read-only)

<FM>Description<FN>
Returns <FC>true<FN> if the alpha channel is premultiplied.

!!}
    property PSD_HasPremultipliedAlpha: Boolean read fPSD_HasPremultipliedAlpha;

{!!
<FS>TIOParamsVals.PSD_LargeDocumentFormat

<FM>Declaration<FC>
property PSD_LargeDocumentFormat: Boolean;

<FM>Description<FN>
Returns <FC>true<FN> if the loaded file is a PSB (Large document format) Photoshop file.
You can set PSD_LargeDocumentFormat = true before save to use PSB format instead of PSD (not necessary when using '.psb' extension).
!!}
    property PSD_LargeDocumentFormat: Boolean read fPSD_LargeDocumentFormat write fPSD_LargeDocumentFormat;


{!!
<FS>TIOParamsVals.PSD_SelectLayer

<FM>Declaration<FC>
PSD_SelectLayer: AnsiString;

<FM>Description<FN>
Specifies name of the layer to load. Empty string allows to load all layers.
You should set <A TIOParamsVals.PSD_LoadLayers> =  true.

<FM>Example<FN>
ImageEnView1.IO.Params.PSD_LoadLayers := true;
ImageEnView1.IO.Params.PSD_SelectLayer := 'upperlayer';
ImageEnView1.IO.LoadFromFile('input.psd');
!!}
    property PSD_SelectLayer: AnsiString read fPSD_SelectLayer write fPSD_SelectLayer;


    //// HDP ////

{!!
<FS>TIOParamsVals.HDP_ImageQuality

<FM>Declaration<FC>
property HDP_ImageQuality: Double;

<FM>Description<FN>
Specifies the quality to use when saving an HDP file. 0.0 produces the lowest possible quality, and 1.0 produces the highest quality, which for
Microsoft HD Photo results in mathematically lossless compression. The default value is 0.9.

See also: <A TIOParamsVals.HDP_Lossless>
!!}
    property HDP_ImageQuality: Double read fHDP_ImageQuality write fHDP_ImageQuality;

{!!
<FS>TIOParamsVals.HDP_Lossless

<FM>Declaration<FC>
property HDP_Lossless: Boolean;

<FM>Description<FN>
Enabling this property will create an HDP file with mathematically lossless compression (overriding the <A TIOParamsVals.HDP_ImageQuality> property).
The default value is <FC>false<FN>.
!!}
    property HDP_Lossless: Boolean read fHDP_Lossless write fHDP_Lossless;


    //// TGA ////

{!!
<FS>TIOParamsVals.TGA_XPos

<FM>Declaration<FC>
property TGA_XPos: Integer;

<FM>Description<FN>
Specifies the X coordinate where the top-left of the image will be shown (not used by ImageEn).

!!}
    property TGA_XPos: integer read fTGA_XPos write fTGA_XPos;

{!!
<FS>TIOParamsVals.TGA_YPos

<FM>Declaration<FC>
property TGA_YPos: Integer;

<FM>Description<FN>
Specifies the Y coordinate where the top-left of the image will be shown (not used by ImageEn).

!!}
    property TGA_YPos: integer read fTGA_YPos write fTGA_YPos;

{!!
<FS>TIOParamsVals.TGA_Compressed

<FM>Declaration<FC>
property TGA_Compressed: Boolean;

<FM>Description<FN>
Set to True to compress a TGA image (using RLE compression).

!!}
    property TGA_Compressed: boolean read fTGA_Compressed write fTGA_Compressed;

{!!
<FS>TIOParamsVals.TGA_Descriptor

<FM>Declaration<FC>
property TGA_Descriptor: AnsiString;

<FM>Description<FN>
Contains the description of the current TGA file.
!!}
    property TGA_Descriptor: AnsiString read fTGA_Descriptor write fTGA_Descriptor;

{!!
<FS>TIOParamsVals.TGA_Author

<FM>Declaration<FC>
property TGA_Author: AnsiString;

<FM>Description<FN>
Contains the author name of the current TGA file.
!!}
    property TGA_Author: AnsiString read fTGA_Author write fTGA_Author;

{!!
<FS>TIOParamsVals.TGA_Date

<FM>Declaration<FC>
property TGA_Date: TDateTime;

<FM>Description<FN>
Contains the creation date and time of the current TGA file.
!!}
    property TGA_Date: TDateTime read fTGA_Date write fTGA_Date;

{!!
<FS>TIOParamsVals.TGA_ImageName

<FM>Declaration<FC>
property TGA_ImageName: AnsiString;

<FM>Description<FN>
Contains the image name of the current TGA file.
!!}
    property TGA_ImageName: AnsiString read fTGA_ImageName write fTGA_ImageName;

{!!
<FS>TIOParamsVals.TGA_Background

<FM>Declaration<FC>
property TGA_Background: <A TRGB>;

<FM>Description<FN>
Specifies the background (or transparency) color of the current TGA image.
!!}
    property TGA_Background: TRGB read fTGA_Background write fTGA_Background;

{!!
<FS>TIOParamsVals.TGA_AspectRatio

<FM>Declaration<FC>
property TGA_AspectRatio: Double;

<FM>Description<FN>
Returns the pixel aspect ratio (pixel width/height) of the current TGA image.
!!}
    property TGA_AspectRatio: double read fTGA_AspectRatio write fTGA_AspectRatio;

{!!
<FS>TIOParamsVals.TGA_Gamma

<FM>Declaration<FC>
property TGA_Gamma: Double;

<FM>Description<FN>
Returns the gamma value of the current TGA image.
!!}
    property TGA_Gamma: double read fTGA_Gamma write fTGA_Gamma;

{!!
<FS>TIOParamsVals.TGA_GrayLevel

<FM>Declaration<FC>
property TGA_GrayLevel: Boolean;

<FM>Description<FN>
When set to True, the image will be saved in gray-scale (i.e. without color).
!!}
    property TGA_GrayLevel: boolean read fTGA_GrayLevel write fTGA_GrayLevel;

    // AVI

{!!
<FS>TIOParamsVals.AVI_FrameCount

<FM>Declaration<FC>
property AVI_FrameCount: Integer;

<FM>Description<FN>
Returns the number of frames contained in the current AVI file.
!!}
    property AVI_FrameCount: integer read fAVI_FrameCount write SetImageCount;

{!!
<FS>TIOParamsVals.AVI_FrameDelayTime

<FM>Declaration<FC>
property AVI_FrameDelayTime: Double;

<FM>Description<FN>
Specifies the time (in milliseconds) that the current frame will be shown when animated/playing.
!!}
    property AVI_FrameDelayTime: double read fAVI_FrameDelayTime write fAVI_FrameDelayTime;

    // MEDIAFILE
    {$ifdef IEINCLUDEDIRECTSHOW}

{!!
<FS>TIOParamsVals.MEDIAFILE_FrameCount

<FM>Declaration<FC>
property MEDIAFILE_FrameCount: integer;

<FM>Description<FN>
Returns the number of frames of the current media file (using <A TImageEnIO.OpenMediaFile>).
!!}
    property MEDIAFILE_FrameCount: integer read fMEDIAFILE_FrameCount write SetImageCount;

{!!
<FS>TIOParamsVals.MEDIAFILE_FrameDelayTime

<FM>Declaration<FC>
property MEDIAFILE_FrameDelayTime: double;

<FM>Description<FN>
Specifies the time (in milliseconds) that the current frame will be shown when animated/playing.
!!}
    property MEDIAFILE_FrameDelayTime: double read fMEDIAFILE_FrameDelayTime write fMEDIAFILE_FrameDelayTime;

    {$endif}
    // PXM

{!!
<FS>TIOParamsVals.PXM_Comments

<FM>Declaration<FC>
property PXM_Comments: TStringList;

<FM>Description<FN>
Contains a list of the comments in a PPM, PBM or PGM file.
!!}
    property PXM_Comments: TStringList read fPXM_Comments;

    // PostScript (PS)

{!!
<FS>TIOParamsVals.PS_PaperWidth

<FM>Declaration<FC>
property PS_PaperWidth: Integer

<FM>Description<FN>
Specifies the width of the page in "PostScript points" (1 point = 1/72 of inch).
Default values are width: 595 and height: 842 (A4 format).
Alternatively, you can use <A TIOParamsVals.PS_PaperSize>.

Common values:
<TABLE>
<R> <H>Paper Size</H> <H>PS_PaperWidth</H> <H>PS_PaperHeight</H> </R>
<R> <C>A0</C> <C>2380</C> <C>3368</C> </R>
<R> <C>A1</C> <C>1684</C> <C>2380</C> </R>
<R> <C>A2</C> <C>1190</C> <C>1684</C> </R>
<R> <C>A3</C> <C>842</C> <C>1190</C> </R>
<R> <C>A4</C> <C>595</C> <C>842</C> </R>
<R> <C>A5</C> <C>421</C> <C>595</C> </R>
<R> <C>A6</C> <C>297</C> <C>421</C> </R>
<R> <C>B5</C> <C>501</C> <C>709</C> </R>
<R> <C>US Letter (8.5 x 11")</C> <C>612</C> <C>792</C> </R>
<R> <C>US Legal (8.5 x 14")</C> <C>612</C> <C>1008</C> </R>
<R> <C>US Ledger (17 x 11")</C> <C>1224</C> <C>792</C> </R>
<R> <C>US Tabloid (11 x 17")</C> <C>792</C> <C>1224</C> </R>
</TABLE>

<FM>Example<FC>
// Save using "US Letter" paper size
ImageEnView1.IO.Params.PS_PaperWidth  := 612;
ImageEnView1.IO.Params.PS_PaperHeight := 792;
ImageEnView1.IO.SaveToFile('D:\output.ps');
!!}
    property PS_PaperWidth: integer read fPS_PaperWidth write fPS_PaperWidth;

{!!
<FS>TIOParamsVals.PS_PaperHeight

<FM>Declaration<FC>
property PS_PaperHeight: Integer

<FM>Description<FN>
Specifies the height of the page in "PostScript points" (1 point = 1/72 of inch).
Default values are width: 595 and height: 842 (A4 format).
Alternatively, you can use <A TIOParamsVals.PS_PaperSize>.

Common values:
<TABLE>
<R> <H>Paper Size</H> <H>PS_PaperWidth</H> <H>PS_PaperHeight</H> </R>
<R> <C>A0</C> <C>2380</C> <C>3368</C> </R>
<R> <C>A1</C> <C>1684</C> <C>2380</C> </R>
<R> <C>A2</C> <C>1190</C> <C>1684</C> </R>
<R> <C>A3</C> <C>842</C> <C>1190</C> </R>
<R> <C>A4</C> <C>595</C> <C>842</C> </R>
<R> <C>A5</C> <C>421</C> <C>595</C> </R>
<R> <C>A6</C> <C>297</C> <C>421</C> </R>
<R> <C>B5</C> <C>501</C> <C>709</C> </R>
<R> <C>US Letter (8.5 x 11")</C> <C>612</C> <C>792</C> </R>
<R> <C>US Legal (8.5 x 14")</C> <C>612</C> <C>1008</C> </R>
<R> <C>US Ledger (17 x 11")</C> <C>1224</C> <C>792</C> </R>
<R> <C>US Tabloid (11 x 17")</C> <C>792</C> <C>1224</C> </R>
</TABLE>

<FM>Example<FC>
// Save using "US Letter" paper size
ImageEnView1.IO.Params.PS_PaperWidth  := 612;
ImageEnView1.IO.Params.PS_PaperHeight := 792;
ImageEnView1.IO.SaveToFile('D:\output.ps');
!!}
    property PS_PaperHeight: integer read fPS_paperHeight write fPS_PaperHeight;

    property PS_PaperSize  : TIOPDFPaperSize read GetPS_PaperSize write SetPS_PaperSize;

{!!
<FS>TIOParamsVals.PS_Compression

<FM>Declaration<FC>
property PS_Compression: <A TIOPSCompression>;

<FM>Description<FN>
Specifies the compression filter for a PostScript file.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('D:\input.tif');
ImageEnView1.IO.Params.PS_Compression := ioPS_G4FAX;
ImageEnView1.IO.SaveToFile('D:\output.ps');
!!}
    property PS_Compression: TIOPSCompression read fPS_Compression write fPS_Compression;

{!!
<FS>TIOParamsVals.PS_Title

<FM>Declaration<FC>
property PS_Title: AnsiString;

<FM>Description<FN>
Specifies the title of the PostScript file.
!!}
    property PS_Title: AnsiString read fPS_Title write fPS_Title;

    // PDF

{!!
<FS>TIOParamsVals.PDF_PaperWidth

<FM>Declaration<FC>
property PDF_PaperWidth: Integer

<FM>Description<FN>
Specifies the width of the page in Adobe PDF points (1 point = 1/72 of inch).
Default values are width: 595 and height: 842 (A4 format).
Alternatively, you can use <A TIOParamsVals.PDF_PaperSize>.

Common values:
<TABLE>
<R> <H>Paper Size</H> <H>PDF_PaperWidth</H> <H>PDF_PaperHeight</H> </R>
<R> <C>A0</C> <C>2380</C> <C>3368</C> </R>
<R> <C>A1</C> <C>1684</C> <C>2380</C> </R>
<R> <C>A2</C> <C>1190</C> <C>1684</C> </R>
<R> <C>A3</C> <C>842</C> <C>1190</C> </R>
<R> <C>A4</C> <C>595</C> <C>842</C> </R>
<R> <C>A5</C> <C>421</C> <C>595</C> </R>
<R> <C>A6</C> <C>297</C> <C>421</C> </R>
<R> <C>B5</C> <C>501</C> <C>709</C> </R>
<R> <C>US Letter (8.5 x 11")</C> <C>612</C> <C>792</C> </R>
<R> <C>US Legal (8.5 x 14")</C> <C>612</C> <C>1008</C> </R>
<R> <C>US Ledger (17 x 11")</C> <C>1224</C> <C>792</C> </R>
<R> <C>US Tabloid (11 x 17")</C> <C>792</C> <C>1224</C> </R>
</TABLE>

<FM>Example<FC>
// Save using "US Letter" paper size
ImageEnView1.IO.Params.PDF_PaperWidth  := 612;
ImageEnView1.IO.Params.PDF_PaperHeight := 792;
ImageEnView1.IO.SaveToFile('D:\output.pdf');
!!}
    property PDF_PaperWidth: integer read fPDF_PaperWidth write fPDF_PaperWidth;

{!!
<FS>TIOParamsVals.PDF_PaperHeight

<FM>Declaration<FC>
property PDF_PaperHeight: Integer

<FM>Description<FN>
Specifies the height of the page in Adobe PDF points (1 point = 1/72 of inch).
Default values are width: 595 and height: 842 (A4 format).
Alternatively, you can use <A TIOParamsVals.PDF_PaperSize>.

Common values:
<TABLE>
<R> <H>Paper Size</H> <H>PDF_PaperWidth</H> <H>PDF_PaperHeight</H> </R>
<R> <C>A0</C> <C>2380</C> <C>3368</C> </R>
<R> <C>A1</C> <C>1684</C> <C>2380</C> </R>
<R> <C>A2</C> <C>1190</C> <C>1684</C> </R>
<R> <C>A3</C> <C>842</C> <C>1190</C> </R>
<R> <C>A4</C> <C>595</C> <C>842</C> </R>
<R> <C>A5</C> <C>421</C> <C>595</C> </R>
<R> <C>A6</C> <C>297</C> <C>421</C> </R>
<R> <C>B5</C> <C>501</C> <C>709</C> </R>
<R> <C>US Letter (8.5 x 11")</C> <C>612</C> <C>792</C> </R>
<R> <C>US Legal (8.5 x 14")</C> <C>612</C> <C>1008</C> </R>
<R> <C>US Ledger (17 x 11")</C> <C>1224</C> <C>792</C> </R>
<R> <C>US Tabloid (11 x 17")</C> <C>792</C> <C>1224</C> </R>
</TABLE>

<FM>Example<FC>
// Save using "US Letter" paper size
ImageEnView1.IO.Params.PDF_PaperWidth  := 612;
ImageEnView1.IO.Params.PDF_PaperHeight := 792;
ImageEnView1.IO.SaveToFile('D:\output.pdf');
!!}
    property PDF_PaperHeight: integer read fPDF_paperHeight write fPDF_PaperHeight;

    property PDF_PaperSize : TIOPDFPaperSize read GetPDF_PaperSize write SetPDF_PaperSize;

{!!
<FS>TIOParamsVals.PDF_Compression

<FM>Declaration<FC>
property PDF_Compression: <A TIOPDFCompression>;

<FM>Description<FN>
Specifies the compression filter for an Adobe PDF file.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('D:\input.tif');
ImageEnView1.IO.Params.PDF_Compression := ioPDF_G4FAX;
ImageEnView1.IO.SaveToFile('D:\output.pdf');
!!}
    property PDF_Compression: TIOPDFCompression read fPDF_Compression write fPDF_Compression;

{!!
<FS>TIOParamsVals.PDF_Title

<FM>Declaration<FC>
property PDF_Title: AnsiString;

<FM>Description<FN>
Specifies the title of a PDF document.
!!}
    property PDF_Title: AnsiString read fPDF_Title write fPDF_Title;

{!!
<FS>TIOParamsVals.PDF_Author

<FM>Declaration<FC>
property PDF_Author: AnsiString;

<FM>Description<FN>
Specifies the name of the person who created the document.
!!}
    property PDF_Author: AnsiString read fPDF_Author write fPDF_Author;

{!!
<FS>TIOParamsVals.PDF_Subject

<FM>Declaration<FC>
property PDF_Subject: AnsiString;

<FM>Description<FN>
Specifies the subject of the PDF document.
!!}
    property PDF_Subject: AnsiString read fPDF_Subject write fPDF_Subject;

{!!
<FS>TIOParamsVals.PDF_Keywords

<FM>Declaration<FC>
property PDF_Keywords: AnsiString;

<FM>Description<FN>
Specifies the keywords associated with a PDF document.
!!}
    property PDF_Keywords: AnsiString read fPDF_Keywords write fPDF_Keywords;

{!!
<FS>TIOParamsVals.PDF_Creator

<FM>Declaration<FC>
property PDF_Creator: AnsiString;

<FM>Description<FN>
Specifies the application that created the original document.
!!}
    property PDF_Creator: AnsiString read fPDF_Creator write fPDF_Creator;

{!!
<FS>TIOParamsVals.PDF_Producer

<FM>Declaration<FC>
property PDF_Producer: AnsiString;

<FM>Description<FN>
Specifies the application that converted the image to PDF.
!!}
    property PDF_Producer: AnsiString read fPDF_Producer write fPDF_Producer;

    //// EXIF ////

    property EXIF_Tags: TList read fEXIF_Tags;

{!!
<FS>TIOParamsVals.EXIF_HasEXIFData

<FM>Declaration<FC>
property EXIF_HasEXIFData: Boolean;

<FM>Description<FN>
If True, the loaded image contains EXIF information tags. EXIF data is commonly added by digital cameras to provide meta data on a photo.

Note: If you do not wish to maintain the original EXIF info, set EXIF_HasEXIFData to False before saving.

Important Note: If an image is assigned from a <A TImageEnView> to another, the EXIF tags is NOT automatically assigned.
To maintain EXIF data in the second <A TImageEnView> or <A TImageEnVect>, you must also assign the EXIF Data. This applies to all ImageEn "Display" components including TImageEnView and TImageEnVect.

Typical situations:
1) You want to maintain the EXIF data untouched:
  Do nothing... (default behavior)

2) You want to change some EXIF fields, e.g.
  Params.EXIF_Software := 'ImageEn';
  Params.EXIF_HasEXIFData := true;

3) You want to remove all EXIF data:
  Params.ResetInfo;

<FM>Example<FC>
// In this example, a form named DlgEXIF has a TImageEnView (ImageEnView1) and another form has a TImageEnView (ImageEnView). Both the image and its EXIF data are passed from one to another via assign.

// Assign the image to DlgEXIF
DlgEXIF.ImageEnView1.Assign ( ImageEnView );
// Assign EXIF data to DlgEXIF.ImageEnView1
DlgEXIF.ImageEnView1.IO.Params.Assign ( ImageEnView.IO.Params );
!!}
    property EXIF_HasEXIFData: boolean read fEXIF_HasEXIFData write fEXIF_HasEXIFData;


{!!
<FS>TIOParamsVals.EXIF_Bitmap

<FM>Declaration<FC>
property EXIF_Bitmap: <A TIEBitmap>;

<FM>Description<FN>
Contains a thumbnail of the image, if available - not all formats are supported. 

Note: EXIF_Bitmap is supported only when loading.
!!}
    property EXIF_Bitmap: TIEBitmap read fEXIF_Bitmap write fEXIF_Bitmap;

{!!
<FS>TIOParamsVals.EXIF_ImageDescription

<FM>Declaration<FC>
property EXIF_ImageDescription: AnsiString;

<FM>Description<FN>
Returns a description of the image.
!!}
    property EXIF_ImageDescription: AnsiString read fEXIF_ImageDescription write fEXIF_ImageDescription;

{!!
<FS>TIOParamsVals.EXIF_Make

<FM>Declaration<FC>
property EXIF_Make: AnsiString;

<FM>Description<FN>
The manufacturer of the recording equipment (camera, scanner, video digitizer, etc.) that generated the image.
!!}
    property EXIF_Make: AnsiString read fEXIF_Make write fEXIF_Make;

{!!
<FS>TIOParamsVals.EXIF_Model

<FM>Declaration<FC>
property EXIF_Model: AnsiString;

<FM>Description<FN>
Returns the model number of the camera
!!}
    property EXIF_Model: AnsiString read fEXIF_Model write fEXIF_Model;

{!!
<FS>TIOParamsVals.EXIF_Orientation

<FM>Declaration<FC>
property EXIF_Orientation: Integer;

<FM>Description<FN>
The orientation of the camera relative to the scene, when the image was captured

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>_exoCorrectOrientation (1)<FN></C> <C>Image is Orientated Correctly (top left side)</C> </R>
<R> <C><FC>_exoNeedsHorizontalFlip (2)<FN></C> <C>Image is Horizontally Flipped (top right side)</C> </R>
<R> <C><FC>_exoNeeds180Rotate (3)<FN></C> <C>Image is Offset by 180º (bottom right side)</C> </R>
<R> <C><FC>_exoNeedsVerticalFlip (4)<FN></C> <C>Image is Vertically Flipped (bottom left side)</C> </R>
<R> <C><FC>_exoNeedsHorzAndVertFlip (5)<FN></C> <C>Image is Flipped Horiz. and Offset 90º CCW (left side top)</C> </R>
<R> <C><FC>_exoNeeds90RotateCW (6)<FN></C> <C>Image is Offset by 90º CCW (right side top)</C> </R>
<R> <C><FC>_exoNeedsFlipHorzAnd90Rotate (7)<FN></C> <C>Image is Flipped Horiz. and offset 90º CW (right side bottom)</C> </R>
<R> <C><FC>_exoNeeds270RotateCW (8)<FN></C> <C>Image is Offset by 90º clockwise (left side bottom)</C> </R>
</TABLE>
!!}
    property EXIF_Orientation: integer read fEXIF_Orientation write SetEXIF_Orientation;

{!!
<FS>TIOParamsVals.EXIF_XResolution

<FM>Declaration<FC>
property EXIF_XResolution: Double;

<FM>Description<FN>
Returns the horizontal resolution of the image. The default value is 1/72 inch (one "point"), but it is largely meaningless as personal computers typicaly don't use this value for display/printing.

A string version of <FC>EXIF_XResolution<FN> (in human-readable format) is available in the <L iexMetaHelpers>iexMetaHelpers.pas</L> EXIF helper unit.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_YResolution>
- <A TIOParamsVals.EXIF_ResolutionUnit>
!!}
    property EXIF_XResolution: double read fEXIF_XResolution write SetEXIF_XResolution;

{!!
<FS>TIOParamsVals.EXIF_YResolution

<FM>Declaration<FC>
property EXIF_YResolution: Double;

<FM>Description<FN>
Returns the vertical resolution of the image. The default value is 1/72 inch (one "point"), but it is largely meaningless as personal computers typicaly don't use this value for display/printing.

A string version of <FC>EXIF_YResolution<FN> (in human-readable format) is available in the <L iexMetaHelpers>iexMetaHelpers.pas</L> EXIF helper unit.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_XResolution>
- <A TIOParamsVals.EXIF_ResolutionUnit>
!!}
    property EXIF_YResolution: double read fEXIF_YResolution write SetEXIF_YResolution;

{!!
<FS>TIOParamsVals.EXIF_ResolutionUnit

<FM>Declaration<FC>
property EXIF_ResolutionUnit: Integer;

<FM>Description<FN>
Returns the units of <A TIOParamsVals.EXIF_XResolution> and <A TIOParamsVals.EXIF_YResolution>:
  1: No unit
  2: Inches
  3: Centimeters

Default value: 2 (inches)
!!}
    property EXIF_ResolutionUnit: integer read fEXIF_ResolutionUnit write fEXIF_ResolutionUnit;

{!!
<FS>TIOParamsVals.EXIF_Software

<FM>Declaration<FC>
property EXIF_Software: AnsiString;

<FM>Description<FN>
Returns the version number of the camera firmware (the internal software of the hardware)
!!}
    property EXIF_Software: AnsiString read fEXIF_Software write fEXIF_Software;

{!!
<FS>TIOParamsVals.EXIF_Artist

<FM>Declaration<FC>
property EXIF_Artist: AnsiString;

<FM>Description<FN>
Specifies the EXIF artist.
!!}
    property EXIF_Artist: AnsiString read fEXIF_Artist write fEXIF_Artist;

{!!
<FS>TIOParamsVals.EXIF_DateTime

<FM>Declaration<FC>
property EXIF_DateTime: AnsiString;

<FM>Description<FN>
The date and time that the image was last modified. Data format is "YYYY:MM:DD HH:MM:SS"+0x00, total 20 bytes. If the clock has not been set or the camera doesn't have clock, the field may be filled with spaces. Usually this returns the same value as <A TIOParamsVals.EXIF_DateTimeOriginal>.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_DateTime2>
- <A TIOParamsVals.EXIF_DateTimeOriginal>
!!}
    property EXIF_DateTime: AnsiString read fEXIF_DateTime write fEXIF_DateTime;

{!!
<FS>TIOParamsVals.EXIF_DateTime2

<FM>Declaration<FC>
property EXIF_DateTime2 : TDateTime;

<FM>Description<FN>
The date and time that the image was last modified. It is a TDateTime variant of <A TIOParamsVals.EXIF_DateTime>. If the clock has not been set or the camera doesn't have clock, a zero result will be returned. Usually this returns the same value as <A TIOParamsVals.EXIF_DateTimeOriginal2>.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_DateTime>
- <A TIOParamsVals.EXIF_DateTimeOriginal2>
!!}
    property EXIF_DateTime2: TDateTime read GetEXIF_DateTime2 write SetEXIF_DateTime2;

{!!
<FS>TIOParamsVals.EXIF_WhitePoint

<FM>Declaration<FC>
property EXIF_WhitePoint[index: Integer]: Double;

<FM>Description<FN>
Defines the chromaticity of white points of the image. <FC>index<FN> is in the range 0 to 1.

If the image uses CIE Standard Illumination D65 (known as international standard of "daylight"), the values are '3127/10000, 3290/10000'.

"-1" for both values means "unspecified".
!!}
    property EXIF_WhitePoint[index: integer]: double read GetEXIF_WhitePoint write SetEXIF_WhitePoint;

{!!
<FS>TIOParamsVals.EXIF_PrimaryChromaticities

<FM>Declaration<FC>
property EXIF_PrimaryChromaticities[index: Integer]: Double;

<FM>Description<FN>
Defines the chromaticity of the primaries of the image. <FC>index<FN> is in the range 0 to 5.

If the image uses CCIR Recommendation 709 primaries, the values are '640/1000,330/1000,300/1000,600/1000,150/1000,0/1000'.

"-1" for all values means "unspecified".
!!}
    property EXIF_PrimaryChromaticities[index: integer]: double read GetEXIF_PrimaryChromaticities write SetEXIF_PrimaryChromaticities;

{!!
<FS>TIOParamsVals.EXIF_YCbCrCoefficients

<FM>Declaration<FC>
property EXIF_YCbCrCoefficients[index: Integer]: Double;

<FM>Description<FN>
When the image format is YCbCr, this value shows a constant to translate it to RGB format. <FC>index<FN> is in the range 0 to 2. Usually the values are '0.299/0.587/0.114'.

"-1" for all values means "unspecified".
!!}
    property EXIF_YCbCrCoefficients[index: integer]: double read GetEXIF_YCbCrCoefficients write SetEXIF_YCbCrCoefficients;

{!!
<FS>TIOParamsVals.EXIF_YCbCrPositioning

<FM>Declaration<FC>
property EXIF_YCbCrPositioning: Integer;

<FM>Description<FN>
When the image format is YCbCr and uses "Sub-sampling" (cropping of chroma data which is performed by all cameras), this property defines the chroma sample point of the sub-sampling pixel array.
  0: Unspecified
  1: The center of pixel array.
  2: The datum point.
!!}
    property EXIF_YCbCrPositioning: integer read fEXIF_YCbCrPositioning write fEXIF_YCbCrPositioning;

{!!
<FS>TIOParamsVals.EXIF_ReferenceBlackWhite

<FM>Declaration<FC>
property EXIF_ReferenceBlackWhite[index: Integer]: Double;

<FM>Description<FN>
Returns the reference value of the black point/white point. <FC>index<FN> is in the range 0 to 5.

YCbCr format: The first two values show the black/white of Y, the next two are Cb, and the last two are Cr
RGB format: The first two values show the black/white of R, the next two are G, and the last two are B.

"-1" for all values means "unspecified".
!!}
    property EXIF_ReferenceBlackWhite[index: integer]: double read GetEXIF_ReferenceBlackWhite write SetEXIF_ReferenceBlackWhite;

{!!
<FS>TIOParamsVals.EXIF_Copyright

<FM>Declaration<FC>
property EXIF_Copyright: AnsiString;

<FM>Description<FN>
Returns the copyright information.
!!}
    property EXIF_Copyright: AnsiString read fEXIF_Copyright write fEXIF_Copyright;

{!!
<FS>TIOParamsVals.EXIF_ExposureTime

<FM>Declaration<FC>
property EXIF_ExposureTime: Double;

<FM>Description<FN>
The exposure time of the photo (reciprocal of the shutter speed). Unit is second.

A string version of <FC>EXIF_ExposureTime<FN> (in human-readable format) is available in the <L iexMetaHelpers>iexMetaHelpers.pas</L> EXIF helper unit.
!!}
    property EXIF_ExposureTime: double read fEXIF_ExposureTime write SetEXIF_ExposureTime;

{!!
<FS>TIOParamsVals.EXIF_FNumber

<FM>Declaration<FC>
property EXIF_FNumber: Double;

<FM>Description<FN>
The actual F-number (F-stop) of the lens when the photo was taken.

A string version of <FC>EXIF_FNumber<FN> (in human-readable format) is available in the <L iexMetaHelpers>iexMetaHelpers.pas</L> EXIF helper unit.
!!}
    property EXIF_FNumber: double read fEXIF_FNumber write SetEXIF_FNumber;

{!!
<FS>TIOParamsVals.EXIF_ExposureProgram

<FM>Declaration<FC>
property EXIF_ExposureProgram: Integer;

<FM>Description<FN>
The exposure program used by the camera for the photo:
  1: Manual control
  2: Program normal
  3: Aperture priority
  4: Shutter priority
  5: Program creative (slow program)
  6: Program action (high-speed program)
  7: Portrait mode
  8: Landscape mode
!!}
    property EXIF_ExposureProgram: integer read fEXIF_ExposureProgram write SetEXIF_ExposureProgram;

{!!
<FS>TIOParamsVals.EXIF_ISOSpeedRatings

<FM>Declaration<FC>
property EXIF_ISOSpeedRatings[index: Integer]: Integer;

<FM>Description<FN>
CCD sensitivity equivalent to Ag-Hr film speedrate. <FC>index<FN> is in the range 0 to 1.

"0" for all values means "unspecified".
!!}
    property EXIF_ISOSpeedRatings[index: integer]: integer read GetEXIF_ISOSpeedRatings write SetEXIF_ISOSpeedRatings;

{!!
<FS>TIOParamsVals.EXIF_ExifVersion

<FM>Declaration<FC>
property EXIF_ExifVersion: AnsiString;

<FM>Description<FN>
Returns the EXIF version number. It is stored as 4 bytes of ASCII characters, e.g. if the data is based on Exif V2.1, then the value is "0210". Since the type is "undefined", there is no NULL (0x00) for termination.
!!}
    property EXIF_ExifVersion: AnsiString read fEXIF_ExifVersion write fEXIF_ExifVersion;

{!!
<FS>TIOParamsVals.EXIF_DateTimeOriginal

<FM>Declaration<FC>
property EXIF_DateTimeOriginal: AnsiString;

<FM>Description<FN>
The date and time that the original image was taken. This value should not be modified by your software. The data format is "YYYY:MM:DD HH:MM:SS"+0x00, total 20 bytes. If the clock has not been set or the camera doesn't have a clock, the field may be filled with spaces. In the Exif standard this tag is optional, but it is mandatory for DCF.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_DateTimeOriginal2>
- <A TIOParamsVals.EXIF_DateTime>
!!}
    property EXIF_DateTimeOriginal: AnsiString read fEXIF_DateTimeOriginal write fEXIF_DateTimeOriginal;

{!!
<FS>TIOParamsVals.EXIF_DateTimeOriginal2

<FM>Declaration<FC>
property EXIF_DateTimeOriginal2: TDateTime;

<FM>Description<FN>
The date and time that the original image was taken. This property is a TDateTime variant of <A TIOParamsVals.EXIF_DateTimeOriginal>. This value should not be modified by your software. If the clock has not been set or the camera doesn't have a clock, the property may return zero. In the Exif standard this tag is optional, but it is mandatory for DCF.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_DateTimeOriginal>
- <A TIOParamsVals.EXIF_DateTime2>
!!}
    property EXIF_DateTimeOriginal2: TDateTime read GetEXIF_DateTimeOriginal2 write SetEXIF_DateTimeOriginal2;

{!!
<FS>TIOParamsVals.EXIF_DateTimeDigitized

<FM>Declaration<FC>
property EXIF_DateTimeDigitized: AnsiString;

<FM>Description<FN>
The date and time that the image was digitized. Usually, it will return the same value as <A TIOParamsVals.EXIF_DateTimeOriginal>. The data format is "YYYY:MM:DD HH:MM:SS"+0x00, total 20 bytes. 
If the clock has not been set or the camera doesn't have a clock, the field may be filled with spaces. In the Exif standard this tag is optional, but it is mandatory for DCF.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_DateTimeDigitized2>
- <A TIOParamsVals.EXIF_DateTimeOriginal>
!!}
    property EXIF_DateTimeDigitized: AnsiString read fEXIF_DateTimeDigitized write fEXIF_DateTimeDigitized;

{!!
<FS>TIOParamsVals.EXIF_DateTimeDigitized2

<FM>Declaration<FC>
property EXIF_DateTimeDigitized2: TDateTime;

<FM>Description<FN>
The date and time that the image was digitized. This property is a TDateTime variant of <A TIOParamsVals.EXIF_DateTimeDigitized>. Usually, it will return the same value as <A TIOParamsVals.EXIF_DateTimeOriginal2>.

If the clock has not been set or the camera doesn't have a clock, the property may return zero. In the Exif standard this tag is optional, but it is mandatory for DCF.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_DateTimeDigitized>
- <A TIOParamsVals.EXIF_DateTimeOriginal2>
!!}
    property EXIF_DateTimeDigitized2: TDateTime read GetEXIF_DateTimeDigitized2 write SetEXIF_DateTimeDigitized2;

{!!
<FS>TIOParamsVals.EXIF_CompressedBitsPerPixel

<FM>Declaration<FC>
property EXIF_CompressedBitsPerPixel: Double;

<FM>Description<FN>
The average JPEG compression ratio (rough estimate)
!!}
    property EXIF_CompressedBitsPerPixel: double read fEXIF_CompressedBitsPerPixel write SetEXIF_CompressedBitsPerPixel;

{!!
<FS>TIOParamsVals.EXIF_ShutterSpeedValue

<FM>Declaration<FC>
property EXIF_ShutterSpeedValue: Double;

<FM>Description<FN>
The shutter speed as an APEX value.

To convert this value to a human-readable "Shutter Speed" calculate this value's power of 2, then make it a reciprocal. For example, if EXIF_ShutterSpeedValue is 4, then the shutter speed is 1/16 second.

A string version of <FC>EXIF_ShutterSpeedValue<FN> (in human-readable format) is available in the <L iexMetaHelpers>iexMetaHelpers.pas</L> EXIF helper unit.
!!}
    property EXIF_ShutterSpeedValue: double read fEXIF_ShutterSpeedValue write SetEXIF_ShutterSpeedValue;

{!!
<FS>TIOParamsVals.EXIF_ApertureValue

<FM>Declaration<FC>
property EXIF_ApertureValue: Double;

<FM>Description<FN>
The aperture of the lens when the photo was taken. The unit is APEX.

To convert <FC>EXIF_ApertureValue<FN> to a human-readable F-number (F-stop) calculate this value's power of root 2 (=1.4142). For example, if EXIF_ApertureValue is 5, then the F-number is 1.41425 = F5.6.

A string version of <FC>EXIF_ApertureValue<FN> (in human-readable format) is available in the <L iexMetaHelpers>iexMetaHelpers.pas</L> EXIF helper unit.
!!}
    property EXIF_ApertureValue: double read fEXIF_ApertureValue write SetEXIF_ApertureValue;

{!!
<FS>TIOParamsVals.EXIF_BrightnessValue

<FM>Declaration<FC>
property EXIF_BrightnessValue: Double;

<FM>Description<FN>
The brightness of the photo subject. Unit is APEX.

To calculate Exposure (Ev) from BrigtnessValue (Bv), you must add SensitivityValue(Sv).
  Ev = Bv + Sv   Sv = log2(ISOSpeedRating / 3.125)
  ISO100: Sv = 5, ISO200: Sv = 6, ISO400: Sv = 7, ISO125: Sv = 5.32.
!!}
    property EXIF_BrightnessValue: double read fEXIF_BrightnessValue write SetEXIF_BrightnessValue;

{!!
<FS>TIOParamsVals.EXIF_ExposureBiasValue

<FM>Declaration<FC>
property EXIF_ExposureBiasValue: Double;

<FM>Description<FN>
The exposure bias (compensation) value of the photo. Unit is APEX (EV).
!!}
    property EXIF_ExposureBiasValue: double read fEXIF_ExposureBiasValue write SetEXIF_ExposureBiasValue;

{!!
<FS>TIOParamsVals.EXIF_MaxApertureValue

<FM>Declaration<FC>
property EXIF_MaxApertureValue: Double;

<FM>Description<FN>
Returns the maximum aperture value of the lens.

Convert to an F-number by calculating the power of root 2 (see the process described for <A TIOParamsVals.EXIF_ApertureValue>).

A string version of <FC>EXIF_MaxApertureValue<FN> (in human-readable format) is available in the <L iexMetaHelpers>iexMetaHelpers.pas</L> EXIF helper unit.
!!}
    property EXIF_MaxApertureValue: double read fEXIF_MaxApertureValue write SetEXIF_MaxApertureValue;

{!!
<FS>TIOParamsVals.EXIF_SubjectDistance

<FM>Declaration<FC>
property EXIF_SubjectDistance: Double;

<FM>Description<FN>
Distance to the focus point (in meters).
!!}
    property EXIF_SubjectDistance: double read fEXIF_SubjectDistance write SetEXIF_SubjectDistance;

{!!
<FS>TIOParamsVals.EXIF_MeteringMode

<FM>Declaration<FC>
property EXIF_MeteringMode: Integer;

<FM>Description<FN>
Exposure metering method:
  0: Unknown
  1: Average
  2: Center weighted average
  3: Spot
  4: Multi-spot
  5: Multi-segment
  6: Partial
  255: Other
!!}
    property EXIF_MeteringMode: integer read fEXIF_MeteringMode write SetEXIF_MeteringMode;

{!!
<FS>TIOParamsVals.EXIF_LightSource

<FM>Declaration<FC>
property EXIF_LightSource: Integer;

<FM>Description<FN>
The Light source of the photo, though generally this refers to the white balance setting:
  0: Unknown
  1: Daylight
  2: Fluorescent
  3: Tungsten
  10: Flash
  17: Standard light A
  18: Standard light B
  19: Standard light C
  20: D55
  21: D65
  22: D75
  255: Other
!!}
    property EXIF_LightSource: integer read fEXIF_LightSource write SetEXIF_LightSource;

{!!
<FS>TIOParamsVals.EXIF_Flash

<FM>Declaration<FC>
property EXIF_Flash: Integer;

<FM>Description<FN>
The status of the flash when the photo was taken:
  0: Flash did not fire
  1: Flash fired
  5: Flash fired but strobe return light not detected
  7: Flash fired and strobe return light detected
!!}
    property EXIF_Flash: integer read fEXIF_Flash write SetEXIF_Flash;

{!!
<FS>TIOParamsVals.EXIF_FocalLength

<FM>Declaration<FC>
property EXIF_FocalLength: Double;

<FM>Description<FN>
Returns the focal length of lens used to take image (in millimeters).
!!}
    property EXIF_FocalLength: double read fEXIF_FocalLength write SetEXIF_FocalLength;

{!!
<FS>TIOParamsVals.EXIF_SubsecTime

<FM>Declaration<FC>
property EXIF_SubsecTime: AnsiString;

<FM>Description<FN>
Some digital cameras can take 2 - 30 pictures per second, but the <A TIOParamsVals.EXIF_DateTime>/<A TIOParamsVals.EXIF_DateTimeOriginal>/<A TIOParamsVals.EXIF_DateTimeDigitized> tags cannot record a sub-second time, so <FC>EXIF_SubsecTime<FN> is used to record the detail.

For example, if the DateTime = "1996:09:01 09:15:30" and the SubSecTime = "130", then the precise date and time is "1996:09:01 09:15:30.130"
!!}
    property EXIF_SubsecTime: AnsiString read fEXIF_SubsecTime write fEXIF_SubsecTime;

{!!
<FS>TIOParamsVals.EXIF_SubsecTimeOriginal

<FM>Declaration<FC>
property EXIF_SubsecTimeOriginal: AnsiString;

<FM>Description<FN>
Some digital cameras can take 2 - 30 pictures per second, but the <A TIOParamsVals.EXIF_DateTime>/<A TIOParamsVals.EXIF_DateTimeOriginal>/<A TIOParamsVals.EXIF_DateTimeDigitized> tags cannot record a sub-second time, so <FC>EXIF_SubsecTime<FN> is used to record the detail.

For example, if the DateTimeOriginal = "1996:09:01 09:15:30" and the SubSecTimeOriginal = "130", then the precise original time is "1996:09:01 09:15:30.130"
!!}
    property EXIF_SubsecTimeOriginal: AnsiString read fEXIF_SubsecTimeOriginal write fEXIF_SubsecTimeOriginal;

{!!
<FS>TIOParamsVals.EXIF_SubsecTimeDigitized

<FM>Declaration<FC>
property EXIF_SubsecTimeDigitized: AnsiString;

<FM>Description<FN>
Some digital cameras can take 2 - 30 pictures per second, but the <A TIOParamsVals.EXIF_DateTime>/<A TIOParamsVals.EXIF_DateTimeOriginal>/<A TIOParamsVals.EXIF_DateTimeDigitized> tags cannot record a sub-second time, so <FC>EXIF_SubsecTime<FN> is used to record the detail.

For example, if the DateTimeDigitized = "1996:09:01 09:15:30" and the SubSecTimeDigitized = "130", then the precise digitized time is "1996:09:01 09:15:30.130"
!!}
    property EXIF_SubsecTimeDigitized: AnsiString read fEXIF_SubsecTimeDigitized write fEXIF_SubsecTimeDigitized;

{!!
<FS>TIOParamsVals.EXIF_FlashPixVersion

<FM>Declaration<FC>
property EXIF_FlashPixVersion: AnsiString;

<FM>Description<FN>
Returns the FlashPix version as 4 character string, e.g. if the image data is based on FlashPix format Ver. 1.0, then the value is "0100".
!!}
    property EXIF_FlashPixVersion: AnsiString read fEXIF_FlashPixVersion write fEXIF_FlashPixVersion;

{!!
<FS>TIOParamsVals.EXIF_ColorSpace

<FM>Declaration<FC>
property EXIF_ColorSpace: Integer;

<FM>Description<FN>
Defines the Color Space. DCF image must use the sRGB color space so the value is always "1". If the photo uses another color space, the value is "65535": Uncalibrated.
!!}
    property EXIF_ColorSpace: integer read fEXIF_ColorSpace write SetEXIF_ColorSpace;

{!!
<FS>TIOParamsVals.EXIF_ExifImageWidth

<FM>Declaration<FC>
property EXIF_ExifImageWidth: Integer;

<FM>Description<FN>
Returns the horizontal size of the image (in pixels)
!!}
    property EXIF_ExifImageWidth: integer read fEXIF_ExifImageWidth write SetEXIF_ExifImageWidth;

{!!
<FS>TIOParamsVals.EXIF_ExifImageHeight

<FM>Declaration<FC>
property EXIF_ExifImageHeight: Integer;

<FM>Description<FN>    
Returns the vertical size of the image (in pixels)
!!}
    property EXIF_ExifImageHeight: integer read fEXIF_ExifImageHeight write SetEXIF_ExifImageHeight;

{!!
<FS>TIOParamsVals.EXIF_RelatedSoundFile

<FM>Declaration<FC>
property EXIF_RelatedSoundFile: AnsiString;

<FM>Description<FN>
Returns an audio filename if the source camera recorded audio with the image.
!!}
    property EXIF_RelatedSoundFile: AnsiString read fEXIF_RelatedSoundFile write fEXIF_RelatedSoundFile;

{!!
<FS>TIOParamsVals.EXIF_FocalPlaneXResolution

<FM>Declaration<FC>
property EXIF_FocalPlaneXResolution: Double;

<FM>Description<FN>
Returns the pixel density at CCD's position. With megapixel cameras, when a photo is taken at a lower resolution (e.g. VGA mode), this value is re-sampled by the photo resolution. In such a case, FocalPlaneResolution is not the same as CCD's actual resolution.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_FocalPlaneYResolution>
- <A TIOParamsVals.EXIF_FocalPlaneResolutionUnit>
!!}
    property EXIF_FocalPlaneXResolution: double read fEXIF_FocalPlaneXResolution write SetEXIF_FocalPlaneXResolution;

{!!
<FS>TIOParamsVals.EXIF_FocalPlaneYResolution

<FM>Declaration<FC>
property EXIF_FocalPlaneYResolution: Double;

<FM>Description<FN>
Returns the pixel density at CCD's position. With megapixel cameras, when a photo is taken at a lower resolution (e.g. VGA mode), this value is re-sampled by the photo resolution. In such a case, FocalPlaneResolution is not the same as CCD's actual resolution.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_FocalPlaneXResolution>
- <A TIOParamsVals.EXIF_FocalPlaneResolutionUnit>
!!}
    property EXIF_FocalPlaneYResolution: double read fEXIF_FocalPlaneYResolution write SetEXIF_FocalPlaneYResolution;

{!!
<FS>TIOParamsVals.EXIF_FocalPlaneResolutionUnit

<FM>Declaration<FC>
property EXIF_FocalPlaneResolutionUnit: Integer;

<FM>Description<FN>
The unit of FocalPlaneXResoluton/FocalPlaneYResolution:
  1: No unit
  2: Inches
  3: Centimeters

Note: Some Fujifilm cameras (e.g. FX2700, FX2900, Finepix 4700Z/40i, etc.) return a value of centimeters (3), but the actual resoluion appears to be 8.3mm? (1/3 inch?). This seems to be a bug with Fujifilm cameras. With the Finepix4900Z the value has been changed to inches (2) but it doesn't appear to match an actual value either.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_FocalPlaneXResolution>
- <A TIOParamsVals.EXIF_FocalPlaneYResolution>
!!}
    property EXIF_FocalPlaneResolutionUnit: integer read fEXIF_FocalPlaneResolutionUnit write SetEXIF_FocalPlaneResolutionUnit;

{!!
<FS>TIOParamsVals.EXIF_ExposureIndex

<FM>Declaration<FC>
property EXIF_ExposureIndex: Double;

<FM>Description<FN>
This is the same as <A TIOParamsVals.EXIF_ISOSpeedRatings> (0x8827) but the data type is an unsigned rational.

Only Kodak cameras use this tag in preference to <A TIOParamsVals.EXIF_ISOSpeedRatings>.
!!}
    property EXIF_ExposureIndex: double read fEXIF_ExposureIndex write SetEXIF_ExposureIndex;

{!!
<FS>TIOParamsVals.EXIF_SensingMethod

<FM>Declaration<FC>
property EXIF_SensingMethod: Integer;

<FM>Description<FN>
Returns the type of image sensor unit. "2" means a one-chip color area sensor. Most digital cameras use this type.
!!}
    property EXIF_SensingMethod: integer read fEXIF_SensingMethod write SetEXIF_SensingMethod;

{!!
<FS>TIOParamsVals.EXIF_FileSource

<FM>Declaration<FC>
property EXIF_FileSource: Integer;

<FM>Description<FN>
Returns the image source, e.g. a value of "0x03" means the image source is a digital still camera.
!!}
    property EXIF_FileSource: integer read fEXIF_FileSource write SetEXIF_FileSource;

{!!
<FS>TIOParamsVals.EXIF_SceneType

<FM>Declaration<FC>
property EXIF_SceneType: Integer;

<FM>Description<FN>
Returns the type of scene, e.g. a value of "0x01" means that the image was directly photographed.
!!}
    property EXIF_SceneType: integer read fEXIF_SceneType write SetEXIF_SceneType;

{!!
<FS>TIOParamsVals.EXIF_UserComment

<FM>Declaration<FC>
property EXIF_UserComment: WideString

<FM>Description<FN>
Provides an alternative tag for storing textual data (keywords, comments, etc) with the image instead of <A TIOParamsVals.EXIF_ImageDescription> (which has character code limitations).

Note: You must specify how the string is coded (ASCII or Unicode) using the <A TIOParamsVals.EXIF_UserCommentCode> property.

<FM>Example<FC>
// Write our comment to the file
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
ImageEnView1.IO.Params.EXIF_UserComment := 'Hello World!';
ImageEnView1.IO.Params.EXIF_UserCommentCode := IEEXIFUSERCOMMENTCODE_UNICODE;
ImageEnView1.IO.Params.EXIF_HasEXIFData := true;
ImageEnView1.IO.SaveToFileTIFF('D:\test.tiff');

// Read back the comment
ImageEnView1.IO.LoadFromFileTIFF('D:\test.tiff');
ShowMessage( ImageEnView1.IO.Params.EXIF_UserComment );
<FN>
!!}
    property EXIF_UserComment: WideString read fEXIF_UserComment write fEXIF_UserComment;

{!!
<FS>TIOParamsVals.EXIF_UserCommentCode

<FM>Declaration<FC>
property EXIF_UserCommentCode: AnsiString;

<FM>Description<FN>
Specifies the character code (8 bytes) for the <A TIOParamsVals.EXIF_UserComment> property.

Allowed values:
<FM>Description<FN>
<TABLE>
<R> <H>ImageEn Const</H> <H>Character Code</H> <H>Desgination (8 bytes)</H> <H>References</H> </R>
<R> <C><FC>IEExifUserCommentCode_Unicode<FN></C> <C>Unicode</C> <C><FC>#$55#$4E#$49#$43#$4F#$44#$45#$00<FN></C> <C>Unicode Standard</C> </R>
<R> <C><FC>IEExifUserCommentCode_ASCII<FN></C> <C>ASCII</C> <C><FC>#$41#$53#$43#$49#$49#$00#$00#$00<FN></C> <C>ITU-T T.50 IA5</C> </R>
<R> <C><FC>IEExifUserCommentCode_JIS<FN></C> <C>JIS</C> <C><FC>#$4A#$49#$53#$00#$00#$00#$00#$00<FN></C> <C>JIS X0208-1990</C> </R>
<R> <C><FC>IEExifUserCommentCode_Undefined<FN></C> <C>Undefined</C> <C><FC>#$00#$00#$00#$00#$00#$00#$00#$00<FN></C> <C>Undefined</C> </R>
</TABLE>

Note: All consts are defined in ImageEnIO.pas

<FM>Example<FC>
// Write a comment to file
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
ImageEnView1.IO.Params.EXIF_UserComment := 'Hello World!';
ImageEnView1.IO.Params.EXIF_UserCommentCode := IEEXIFUSERCOMMENTCODE_UNICODE;
ImageEnView1.IO.Params.EXIF_HasEXIFData := true;
ImageEnView1.IO.SaveToFileTIFF('D:\test.tiff');
!!}
    property EXIF_UserCommentCode: AnsiString read fEXIF_UserCommentCode write fEXIF_UserCommentCode;

{!!
<FS>TIOParamsVals.EXIF_MakerNote

<FM>Declaration<FC>
property EXIF_MakerNote: <A TIETagsHandler>;

<FM>Description<FN>
Contains custom tags that are added by camera manufacturers. Unfortunately there is not a standard for this tag, so ImageEn offers a general handler which will read the IFD (if present) of the maker note.

See the InputOutput/EXIF demo for more details.

<FM>Example<FC>
// Read the ISO value for a Canon camera photo
case ImageEnView1.IO.Params.EXIF_MakerNote.GetIntegerIndexed(1, 16) of
  15: ShowMessage('Auto');
  16: ShowMessage ('50');
  17: ShowMessage ('100');
  18: ShowMessage ('200');
  19: ShowMessage ('400');
end;

<FM>Demo<FN>
InputOutput/EXIF
!!}
    property EXIF_MakerNote: TIETagsHandler read fEXIF_MakerNote;

{!!
<FS>TIOParamsVals.EXIF_XPTitle

<FM>Declaration<FC>
property EXIF_XPTitle: WideString;

<FM>Description<FN>
Specifies the Windows XP image title. This is shown in the properties dialog for JPEG and TIFF files under Windows XP. It is still read on newer versions of Windows, but only if a relevant XMP field cannot be found.
!!}
    property EXIF_XPTitle: WideString read fEXIF_XPTitle write fEXIF_XPTitle;

{!!
<FS>TIOParamsVals.EXIF_XPRating

<FM>Declaration<FC>
property EXIF_XPRating: Integer;

<FM>Description<FN>
Specifies the Windows XP image rating. This is shown in the properties dialog for JPEG and TIFF files under Windows XP. It is still read on newer versions of Windows, but only if a relevant XMP field cannot be found.

Allowed values from 0 up to 5. -1 is returned when not available.
!!}
    property EXIF_XPRating: Integer read fEXIF_XPRating write fEXIF_XPRating;

{!!
<FS>TIOParamsVals.EXIF_XPComment

<FM>Declaration<FC>
property EXIF_XPComment: WideString;

<FM>Description<FN>
Specifies the Windows XP image comment. This is shown in the properties dialog for JPEG and TIFF files under Windows XP. It is still read on newer versions of Windows, but only if a relevant XMP field cannot be found.
!!}
    property EXIF_XPComment: WideString read fEXIF_XPComment write fEXIF_XPComment;

{!!
<FS>TIOParamsVals.EXIF_XPAuthor

<FM>Declaration<FC>
property EXIF_XPAuthor: WideString;

<FM>Description<FN>
Specifies the Windows XP image author. This is shown in the properties dialog for JPEG and TIFF files under Windows XP. It is still read on newer versions of Windows, but only if a relevant XMP field cannot be found.
!!}
    property EXIF_XPAuthor: WideString read fEXIF_XPAuthor write fEXIF_XPAuthor;

{!!
<FS>TIOParamsVals.EXIF_XPKeywords

<FM>Declaration<FC>
property EXIF_XPKeywords: WideString;

<FM>Description<FN>
Specifies the Windows XP image keywords. This is shown in the properties dialog for JPEG and TIFF files under Windows XP. It is still read on newer versions of Windows, but only if a relevant XMP field cannot be found.
!!}
    property EXIF_XPKeywords: WideString read fEXIF_XPKeywords write fEXIF_XPKeywords;

{!!
<FS>TIOParamsVals.EXIF_XPSubject

<FM>Declaration<FC>
property EXIF_XPSubject: WideString;

<FM>Description<FN>
Specifies the Windows XP image subject. This is shown in the properties dialog for JPEG and TIFF files under Windows XP. It is still read on newer versions of Windows, but only if a relevant XMP field cannot be found.
!!}
    property EXIF_XPSubject: WideString read fEXIF_XPSubject write fEXIF_XPSubject;

{!!
<FS>TIOParamsVals.EXIF_ExposureMode

<FM>Declaration<FC>
property EXIF_ExposureMode: Integer;

<FM>Description<FN>
Returns the exposure mode that was set when the photo was taken. In auto-bracketing mode, the camera shoots a series of frames of the same scene at different exposure settings.
0: Auto exposure
1: Manual exposure
2: Auto bracketing
!!}
    property EXIF_ExposureMode: Integer read fEXIF_ExposureMode write fEXIF_ExposureMode;

{!!
<FS>TIOParamsVals.EXIF_WhiteBalance

<FM>Declaration<FC>
property EXIF_WhiteBalance: Integer;

<FM>Description<FN>
Returns the white balance mode selected when the photo was taken:
0: Auto white balance
1: Manual white balance
!!}
    property EXIF_WhiteBalance: Integer read fEXIF_WhiteBalance write fEXIF_WhiteBalance;

{!!
<FS>TIOParamsVals.EXIF_DigitalZoomRatio

<FM>Declaration<FC>
property EXIF_DigitalZoomRatio: Double;

<FM>Description<FN>
Returns the digital zoom ratio when the photo was taken.

Note: If the numerator of the recorded value is 0, this indicates that digital zoom was not used.
!!}
    property EXIF_DigitalZoomRatio: Double read fEXIF_DigitalZoomRatio write fEXIF_DigitalZoomRatio;

{!!
<FS>TIOParamsVals.EXIF_FocalLengthIn35mmFilm

<FM>Declaration<FC>
property EXIF_FocalLengthIn35mmFilm: Integer;

<FM>Description<FN>
Returns the equivalent focal length assuming a 35mm film camera, in mm. A value of 0 means the focal length is unknown.

Note: This property differs from the <A TIOParamsVals.EXIF_FocalLength> tag.
!!}
    property EXIF_FocalLengthIn35mmFilm: Integer read fEXIF_FocalLengthIn35mmFilm write fEXIF_FocalLengthIn35mmFilm;

{!!
<FS>TIOParamsVals.EXIF_SceneCaptureType

<FM>Declaration<FC>
property EXIF_SceneCaptureType: Integer;

<FM>Description<FN>
Returns the type of scene that was shot. It may also be used to record the mode in which the photo was taken.

Allowed values:
  0: Standard
  1: Landscape
  2: Portrait
  3: Night scene

Note: This property differs from the <A TIOParamsVals.EXIF_SceneType> tag.
!!}
    property EXIF_SceneCaptureType: Integer read fEXIF_SceneCaptureType write fEXIF_SceneCaptureType;

{!!
<FS>TIOParamsVals.EXIF_GainControl

<FM>Declaration<FC>
property EXIF_GainControl: Integer;

<FM>Description<FN>
Returns the degree of overall image gain adjustment:
  0: None
  1: Low gain up
  2: High gain up
  3: Low gain down
  4: High gain down
!!}
    property EXIF_GainControl: Integer read fEXIF_GainControl write fEXIF_GainControl;

{!!
<FS>TIOParamsVals.EXIF_Contrast

<FM>Declaration<FC>
property EXIF_Contrast: Integer;

<FM>Description<FN>
Returns the direction of contrast processing applied by the camera when the photo was taken:
  0: Normal
  1: Soft
  2: Hard
!!}
    property EXIF_Contrast: Integer read fEXIF_Contrast write fEXIF_Contrast;

{!!
<FS>TIOParamsVals.EXIF_Saturation

<FM>Declaration<FC>
property EXIF_Saturation: Integer;

<FM>Description<FN>
Returns the direction of saturation processing applied by the camera when the photo was taken:
  0: Normal
  1: Low saturation
  2: High saturation
!!}
    property EXIF_Saturation: Integer read fEXIF_Saturation write fEXIF_Saturation;

{!!
<FS>TIOParamsVals.EXIF_Sharpness

<FM>Declaration<FC>
property EXIF_Sharpness: Integer;

<FM>Description<FN>
Returns the direction of sharpness processing applied by the camera when the photo was taken:
  0: Normal
  1: Soft
  2: Hard
!!}
    property EXIF_Sharpness: Integer read fEXIF_Sharpness write fEXIF_Sharpness;

{!!
<FS>TIOParamsVals.EXIF_SubjectDistanceRange

<FM>Declaration<FC>
property EXIF_SubjectDistanceRange: Integer;

<FM>Description<FN>
Returns the distance to the subject:
  0: Unknown
  1: Macro
  2: Close view
  3: Distant view
!!}
    property EXIF_SubjectDistanceRange: Integer read fEXIF_SubjectDistanceRange write fEXIF_SubjectDistanceRange;

{!!
<FS>TIOParamsVals.EXIF_ImageUniqueID

<FM>Declaration<FC>
property EXIF_ImageUniqueID: AnsiString;

<FM>Description<FN>
Returns an identifier assigned uniquely to each image. It is recorded as an ASCII string equivalent of hexadecimal notation with a 128-bit fixed length.
!!}
    property EXIF_ImageUniqueID: AnsiString read fEXIF_ImageUniqueID write fEXIF_ImageUniqueID;

{!!
<FS>TIOParamsVals.EXIF_GPSVersionID

<FM>Declaration<FC>
property EXIF_GPSVersionID: AnsiString;

<FM>Description<FN>
Indicates the version of the GPSInfoIFD tag.
To save GPS info fill this property with '2.2.0.0' string.
!!}
    property EXIF_GPSVersionID: AnsiString read fEXIF_GPSVersionID write fEXIF_GPSVersionID;

{!!
<FS>TIOParamsVals.EXIF_GPSLatitude

<FM>Declaration<FC>
property EXIF_GPSLatitude: Double;

<FM>Description<FN>
Returns the latitude in decimal degrees where the photo was taken (if the camera supports GPS).

Note: This uses the following EXIF properties: <A TIOParamsVals.EXIF_GPSLatitudeDegrees>, <A TIOParamsVals.EXIF_GPSLatitudeMinutes>, <A TIOParamsVals.EXIF_GPSLatitudeSeconds> and <A TIOParamsVals.EXIF_GPSLatitudeRef>

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLatitude_Str>
- <A TIOParamsVals.EXIF_GPSLongitude>
!!}
    property EXIF_GPSLatitude: Double read GetEXIF_GPSLatitude write SetEXIF_GPSLatitude;

{!!
<FS>TIOParamsVals.EXIF_GPSLatitude_Str

<FM>Declaration<FC>
property EXIF_GPSLatitude_Str : string; (Read-only)

<FM>Description<FN>
Returns a human-readable representation of <A TIOParamsVals.EXIF_GPSLongitude>. Returns '' if the image does not contain EXIF GPS data.

Note: This uses the following EXIF properties: <A TIOParamsVals.EXIF_GPSLatitudeDegrees>, <A TIOParamsVals.EXIF_GPSLatitudeMinutes>, <A TIOParamsVals.EXIF_GPSLatitudeSeconds> and <A TIOParamsVals.EXIF_GPSLatitudeRef>

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLatitude>
- <A TIOParamsVals.EXIF_GPSLongitude_Str>
!!}
    property EXIF_GPSLatitude_Str : string read GetEXIF_GPSLatitude_Str;

{!!
<FS>TIOParamsVals.EXIF_GPSLatitudeRef

<FM>Declaration<FC>
property EXIF_GPSLatitudeRef: AnsiString;

<FM>Description<FN>
Indicates whether the latitude is North or South. The ASCII value "N" indicates North latitude, whereas "S" is South latitude.
Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLatitude>
- <A TIOParamsVals.EXIF_GPSLatitude_Str>
!!}
    property EXIF_GPSLatitudeRef: AnsiString read fEXIF_GPSLatitudeRef write fEXIF_GPSLatitudeRef;

{!!
<FS>TIOParamsVals.EXIF_GPSLatitudeDegrees

<FM>Declaration<FC>
property EXIF_GPSLatitudeDegrees: Double;

<FM>Description<FN>
Returns the latitude degrees where the photo was taken (if the camera supports GPS).

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLatitude>
- <A TIOParamsVals.EXIF_GPSLatitude_Str>
!!}
    property EXIF_GPSLatitudeDegrees: Double read fEXIF_GPSLatitudeDegrees write fEXIF_GPSLatitudeDegrees;

{!!
<FS>TIOParamsVals.EXIF_GPSLatitudeMinutes

<FM>Declaration<FC>
property EXIF_GPSLatitudeMinutes: Double;

<FM>Description<FN>
Returns the latitude minutes where the photo was taken (if the camera supports GPS).

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLatitude>
- <A TIOParamsVals.EXIF_GPSLatitude_Str>
!!}
    property EXIF_GPSLatitudeMinutes: Double read fEXIF_GPSLatitudeMinutes write fEXIF_GPSLatitudeMinutes;

{!!
<FS>TIOParamsVals.EXIF_GPSLatitudeSeconds

<FM>Declaration<FC>
property EXIF_GPSLatitudeSeconds: Double;

<FM>Description<FN>
Returns the latitude seconds where the photo was taken (if the camera supports GPS).

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLatitude>
- <A TIOParamsVals.EXIF_GPSLatitude_Str>
!!}
    property EXIF_GPSLatitudeSeconds: Double read fEXIF_GPSLatitudeSeconds write fEXIF_GPSLatitudeSeconds;

{!!
<FS>TIOParamsVals.EXIF_GPSLongitude

<FM>Declaration<FC>
property EXIF_GPSLongitude: Double;

<FM>Description<FN>
Returns the longitude in decimal degrees where the photo was taken (if the camera supports GPS).

Note: This uses the following EXIF properties: <A TIOParamsVals.EXIF_GPSLongitudeDegrees>, <A TIOParamsVals.EXIF_GPSLongitudeMinutes>, <A TIOParamsVals.EXIF_GPSLongitudeSeconds> and <A TIOParamsVals.EXIF_GPSLongitudeRef>

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLongitude_Str>
- <A TIOParamsVals.EXIF_GPSLatitude>
!!}
    property EXIF_GPSLongitude: Double read GetEXIF_GPSLongitude write SetEXIF_GPSLongitude;

{!!
<FS>TIOParamsVals.EXIF_GPSLongitude_Str

<FM>Declaration<FC>
property EXIF_GPSLongitude_Str : string; (Read-only)

<FM>Description<FN>
Returns a human-readable representation of <A TIOParamsVals.EXIF_GPSLongitude>. Returns '' if the image does not contain EXIF GPS data.

Note: This uses the following EXIF properties: <A TIOParamsVals.EXIF_GPSLongitudeDegrees>, <A TIOParamsVals.EXIF_GPSLongitudeMinutes>, <A TIOParamsVals.EXIF_GPSLongitudeSeconds> and <A TIOParamsVals.EXIF_GPSLongitudeRef>

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLongitude>
- <A TIOParamsVals.EXIF_GPSLatitude_Str>
!!}
    property EXIF_GPSLongitude_Str : string read GetEXIF_GPSLongitude_Str;

{!!
<FS>TIOParamsVals.EXIF_GPSLongitudeRef

<FM>Declaration<FC>
property EXIF_GPSLongitudeRef: AnsiString;

<FM>Description<FN>
Indicates whether the longitude is East or West. An ASCII value of "E" indicates East longitude, whereas "W" is West longitude.
Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLongitude>
- <A TIOParamsVals.EXIF_GPSLongitude_Str>
!!}
    property EXIF_GPSLongitudeRef: AnsiString read fEXIF_GPSLongitudeRef write fEXIF_GPSLongitudeRef;

{!!
<FS>TIOParamsVals.EXIF_GPSLongitudeDegrees

<FM>Declaration<FC>
property EXIF_GPSLongitudeDegrees: Double;

<FM>Description<FN>
Returns the longitude degrees where the photo was taken (if the camera supports GPS).

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLongitude>
- <A TIOParamsVals.EXIF_GPSLongitude_Str>
!!}
    property EXIF_GPSLongitudeDegrees: Double read fEXIF_GPSLongitudeDegrees write fEXIF_GPSLongitudeDegrees;

{!!
<FS>TIOParamsVals.EXIF_GPSLongitudeMinutes

<FM>Declaration<FC>
property EXIF_GPSLongitudeMinutes: Double;

<FM>Description<FN>        
Returns the longitude minutes where the photo was taken (if the camera supports GPS).

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLongitude>
- <A TIOParamsVals.EXIF_GPSLongitude_Str>
!!}
    property EXIF_GPSLongitudeMinutes: Double read fEXIF_GPSLongitudeMinutes write fEXIF_GPSLongitudeMinutes;

{!!
<FS>TIOParamsVals.EXIF_GPSLongitudeSeconds

<FM>Declaration<FC>
property EXIF_GPSLongitudeSeconds: Double;

<FM>Description<FN>           
Returns the longitude seconds where the photo was taken (if the camera supports GPS).

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSLongitude>
- <A TIOParamsVals.EXIF_GPSLongitude_Str>
!!}
    property EXIF_GPSLongitudeSeconds: Double read fEXIF_GPSLongitudeSeconds write fEXIF_GPSLongitudeSeconds;




{!!
<FS>TIOParamsVals.EXIF_GPSAltitudeRef

<FM>Declaration<FC>
property EXIF_GPSAltitudeRef: AnsiString;

<FM>Description<FN>
Returns the altitude used as a reference:
- If the reference is sea level and the altitude is above sea level, "0" is returned.
- If the altitude is below sea level, a value of "1" is returned and the altitude is indicated as an absolute value in the <A TIOParamsVals.EXIF_GPSAltitude> tag.

The reference unit is meters.

Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.
!!}
    property EXIF_GPSAltitudeRef: AnsiString read fEXIF_GPSAltitudeRef write fEXIF_GPSAltitudeRef;

{!!
<FS>TIOParamsVals.EXIF_GPSAltitude

<FM>Declaration<FC>
property EXIF_GPSAltitude: Double;

<FM>Description<FN>
Returns the altitude (in meters) based on the reference in <A TIOParamsVals.EXIF_GPSAltitudeRef>.
!!}
    property EXIF_GPSAltitude: Double read fEXIF_GPSAltitude write fEXIF_GPSAltitude;

{!!
<FS>TIOParamsVals.EXIF_GPSTimeStampHour

<FM>Declaration<FC>
property EXIF_GPSTimeStampHour: Double;

<FM>Description<FN>
Returns the hour portion of the GPS time as UTC (Coordinated Universal Time).
!!}
    property EXIF_GPSTimeStampHour: Double read fEXIF_GPSTimeStampHour write fEXIF_GPSTimeStampHour;

{!!
<FS>TIOParamsVals.EXIF_GPSTimeStampMinute

<FM>Declaration<FC>
property EXIF_GPSTimeStampMinute: Double;

<FM>Description<FN>
Returns the minutes portion of the GPS time as UTC (Coordinated Universal Time).
!!}
    property EXIF_GPSTimeStampMinute: Double read fEXIF_GPSTimeStampMinute write fEXIF_GPSTimeStampMinute;

{!!
<FS>TIOParamsVals.EXIF_GPSTimeStampSecond

<FM>Declaration<FC>
property EXIF_GPSTimeStampSecond: Double;

<FM>Description<FN>
Returns the second portion of the GPS time as UTC (Coordinated Universal Time).
!!}
    property EXIF_GPSTimeStampSecond: Double read fEXIF_GPSTimeStampSecond write fEXIF_GPSTimeStampSecond;

{!!
<FS>TIOParamsVals.EXIF_GPSSatellites

<FM>Declaration<FC>
property EXIF_GPSSatellites: AnsiString

<FM>Description<FN>
Returns the GPS satellites used for measurement. This tag can be used to describe the number of satellites, their ID number, angle of elevation, azimuth, SNR and other information in ASCII notation. The format is not defined.
!!}
    property EXIF_GPSSatellites: AnsiString read fEXIF_GPSSatellites write fEXIF_GPSSatellites;

{!!
<FS>TIOParamsVals.EXIF_GPSStatus

<FM>Declaration<FC>
property EXIF_GPSStatus: AnsiString;

<FM>Description<FN>
Returns the status of the GPS receiver when the image was recorded. "A" means measurement is in progress, and "V" means the measurement is interoperability.
!!}
    property EXIF_GPSStatus: AnsiString read fEXIF_GPSStatus write fEXIF_GPSStatus;

{!!
<FS>TIOParamsVals.EXIF_GPSMeasureMode

<FM>Declaration<FC>
property EXIF_GPSMeasureMode: AnsiString;

<FM>Description<FN>
Returns the GPS measurement mode. "2" means two-dimensional measurement and "3" means three-dimensional measurement is in progress.
!!}
    property EXIF_GPSMeasureMode: AnsiString read fEXIF_GPSMeasureMode write fEXIF_GPSMeasureMode;

{!!
<FS>TIOParamsVals.EXIF_GPSDOP

<FM>Declaration<FC>
property EXIF_GPSDOP: Double;

<FM>Description<FN>
Returns the GPS DOP (data degree of precision). An HDOP value is written during two-dimensional measurement, and PDOP during three-dimensional measurement.
!!}
    property EXIF_GPSDOP: Double read fEXIF_GPSDOP write fEXIF_GPSDOP;

{!!
<FS>TIOParamsVals.EXIF_GPSSpeedRef

<FM>Declaration<FC>
property EXIF_GPSSpeedRef: AnsiString;

<FM>Description<FN>
Returns the unit used to express the GPS receiver <L TIOParamsVals.EXIF_GPSSpeed>speed of movement</L>:
  K: Kilometers per hour
  M: Miles per hour
  N: Knots

Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.
!!}
    property EXIF_GPSSpeedRef: AnsiString read fEXIF_GPSSpeedRef write fEXIF_GPSSpeedRef;

{!!
<FS>TIOParamsVals.EXIF_GPSSpeed

<FM>Declaration<FC>
property EXIF_GPSSpeed: Double;

<FM>Description<FN>
Returns the speed of GPS receiver movement.

See also: <A TIOParamsVals.EXIF_GPSSpeedRef>
!!}
    property EXIF_GPSSpeed: Double read fEXIF_GPSSpeed write fEXIF_GPSSpeed;

{!!
<FS>TIOParamsVals.EXIF_GPSTrackRef

<FM>Declaration<FC>
property EXIF_GPSTrackRef: AnsiString;

<FM>Description<FN>
Returns the reference for the direction of <L TIOParamsVals.EXIF_GPSTrack>GPS receiver movement</L>. "T" denotes true direction and "M" is magnetic direction.

Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.
!!}
    property EXIF_GPSTrackRef: AnsiString read fEXIF_GPSTrackRef write fEXIF_GPSTrackRef;

{!!
<FS>TIOParamsVals.EXIF_GPSTrack

<FM>Declaration<FC>
property EXIF_GPSTrack: Double;

<FM>Description<FN>
Returns the direction of GPS receiver movement. The range of values is from 0.00 to 359.99.

See also: <A TIOParamsVals.EXIF_GPSTrackRef>
!!}
    property EXIF_GPSTrack: Double read fEXIF_GPSTrack write fEXIF_GPSTrack;

{!!
<FS>TIOParamsVals.EXIF_GPSImgDirectionRef

<FM>Declaration<FC>
property EXIF_GPSImgDirectionRef: AnsiString;

<FM>Description<FN>
Returns the reference for the <L TIOParamsVals.EXIF_GPSImgDirection>direction of the image</L> when it was captured. "T" denotes true direction and "M" is magnetic direction.

Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.
!!}
    property EXIF_GPSImgDirectionRef: AnsiString read fEXIF_GPSImgDirectionRef write fEXIF_GPSImgDirectionRef;

{!!
<FS>TIOParamsVals.EXIF_GPSImgDirection

<FM>Declaration<FC>
property EXIF_GPSImgDirection: Double;

<FM>Description<FN>
Returns the direction of the image when it was captured. The range of values is from 0.00 to 359.99.

See also: <A TIOParamsVals.EXIF_GPSImgDirectionRef>
!!}
    property EXIF_GPSImgDirection: Double read fEXIF_GPSImgDirection write fEXIF_GPSImgDirection;

{!!
<FS>TIOParamsVals.EXIF_GPSMapDatum

<FM>Declaration<FC>
property EXIF_GPSMapDatum: AnsiString;

<FM>Description<FN>
Returns the geodetic survey data used by the GPS receiver. If the survey data is restricted to Japan, the value of this tag is "TOKYO" or "WGS-84".
!!}
    property EXIF_GPSMapDatum: AnsiString read fEXIF_GPSMapDatum write fEXIF_GPSMapDatum;

{!!
<FS>TIOParamsVals.EXIF_GPSDestLatitudeRef

<FM>Declaration<FC>
property EXIF_GPSDestLatitudeRef: AnsiString;

<FM>Description<FN>
Indicates whether the <L TIOParamsVals.EXIF_GPSDestLatitudeDegrees>latitude of the destination point</L> is North or South latitude. The ASCII value "N" indicates North latitude, whereas "S" is South latitude.
Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSDestLatitudeDegrees>
- <A TIOParamsVals.EXIF_GPSDestLatitudeMinutes>
- <A TIOParamsVals.EXIF_GPSDestLatitudeSeconds>
!!}
    property EXIF_GPSDestLatitudeRef: AnsiString read fEXIF_GPSDestLatitudeRef write fEXIF_GPSDestLatitudeRef;

{!!
<FS>TIOParamsVals.EXIF_GPSDestLatitudeDegrees

<FM>Declaration<FC>
property EXIF_GPSDestLatitudeDegrees: Double;

<FM>Description<FN>
Returns the latitude degrees of the destination point.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSDestLatitudeRef>
- <A TIOParamsVals.EXIF_GPSDestLatitudeMinutes>
- <A TIOParamsVals.EXIF_GPSDestLatitudeSeconds>
!!}
    property EXIF_GPSDestLatitudeDegrees: Double read fEXIF_GPSDestLatitudeDegrees write fEXIF_GPSDestLatitudeDegrees;

{!!
<FS>TIOParamsVals.EXIF_GPSDestLatitudeMinutes

<FM>Declaration<FC>
property EXIF_GPSDestLatitudeMinutes: Double;

<FM>Description<FN>
Returns the latitude minutes of the destination point.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSDestLatitudeRef>
- <A TIOParamsVals.EXIF_GPSDestLatitudeDegrees>
- <A TIOParamsVals.EXIF_GPSDestLatitudeSeconds>
!!}
    property EXIF_GPSDestLatitudeMinutes: Double read fEXIF_GPSDestLatitudeMinutes write fEXIF_GPSDestLatitudeMinutes;

{!!
<FS>TIOParamsVals.EXIF_GPSDestLatitudeSeconds

<FM>Declaration<FC>
property EXIF_GPSDestLatitudeSeconds: Double;

<FM>Description<FN>
Returns the latitude seconds of the destination point.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSDestLatitudeRef>
- <A TIOParamsVals.EXIF_GPSDestLatitudeDegrees>
- <A TIOParamsVals.EXIF_GPSDestLatitudeMinutes>
!!}
    property EXIF_GPSDestLatitudeSeconds: Double read fEXIF_GPSDestLatitudeSeconds write fEXIF_GPSDestLatitudeSeconds;

{!!
<FS>TIOParamsVals.EXIF_GPSDestLongitudeRef

<FM>Declaration<FC>
property EXIF_GPSDestLongitudeRef: AnsiString;

<FM>Description<FN>
Indicates whether the longitude of the destination point</L> is East or West longitude. An ASCII value of "E" indicates East longitude, whereas "W" is West longitude.
Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSDestLongitudeDegrees>
- <A TIOParamsVals.EXIF_GPSDestLongitudeMinutes>
- <A TIOParamsVals.EXIF_GPSDestLongitudeSeconds>
!!}
    property EXIF_GPSDestLongitudeRef: AnsiString read fEXIF_GPSDestLongitudeRef write fEXIF_GPSDestLatitudeRef;

{!!
<FS>TIOParamsVals.EXIF_GPSDestLongitudeDegrees

<FM>Declaration<FC>
property EXIF_GPSDestLongitudeDegrees: Double;

<FM>Description<FN>
Returns the longitude degrees of the destination point.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSDestLongitudeRef>
- <A TIOParamsVals.EXIF_GPSDestLongitudeMinutes>
- <A TIOParamsVals.EXIF_GPSDestLongitudeSeconds>
!!}
    property EXIF_GPSDestLongitudeDegrees: Double read fEXIF_GPSDestLongitudeDegrees write fEXIF_GPSDestLongitudeDegrees;

{!!
<FS>TIOParamsVals.EXIF_GPSDestLongitudeMinutes

<FM>Declaration<FC>
property EXIF_GPSDestLongitudeMinutes: Double;

<FM>Description<FN>
Returns the longitude minutes of the destination point.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSDestLongitudeRef>
- <A TIOParamsVals.EXIF_GPSDestLongitudeDegrees>
- <A TIOParamsVals.EXIF_GPSDestLongitudeSeconds>
!!}
    property EXIF_GPSDestLongitudeMinutes: Double read fEXIF_GPSDestLongitudeMinutes write fEXIF_GPSDestLongitudeMinutes;

{!!
<FS>TIOParamsVals.EXIF_GPSDestLongitudeSeconds

<FM>Declaration<FC>
property EXIF_GPSDestLongitudeSeconds: Double;

<FM>Description<FN>
Returns the longitude seconds of the destination point.

<FM>See Also<FN>
- <A TIOParamsVals.EXIF_GPSDestLongitudeRef>
- <A TIOParamsVals.EXIF_GPSDestLongitudeDegrees>
- <A TIOParamsVals.EXIF_GPSDestLongitudeMinutes>
!!}
    property EXIF_GPSDestLongitudeSeconds: Double read fEXIF_GPSDestLongitudeSeconds write fEXIF_GPSDestLongitudeSeconds;

{!!
<FS>TIOParamsVals.EXIF_GPSDestBearingRef

<FM>Declaration<FC>
property EXIF_GPSDestBearingRef: AnsiString;

<FM>Description<FN>
Returns the reference used for the <L TIOParamsVals.EXIF_GPSDestBearing>bearing to the destination point</L>. "T" denotes true direction, whereas "M" is magnetic direction.
Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.
!!}
    property EXIF_GPSDestBearingRef: AnsiString read fEXIF_GPSDestBearingRef write fEXIF_GPSDestBearingRef;

{!!
<FS>TIOParamsVals.EXIF_GPSDestBearing

<FM>Declaration<FC>
property EXIF_GPSDestBearing: Double;

<FM>Description<FN>
Returns the bearing to the destination point. The range of values is from 0.00 to 359.99.

See also: <A TIOParamsVals.EXIF_GPSDestBearingRef>
!!}
    property EXIF_GPSDestBearing: Double read fEXIF_GPSDestBearing write fEXIF_GPSDestBearing;

{!!
<FS>TIOParamsVals.EXIF_GPSDestDistanceRef

<FM>Declaration<FC>
property EXIF_GPSDestDistanceRef: AnsiString;

<FM>Description<FN>
Returns the unit used to express the <L TIOParamsVals.EXIF_GPSDestDistance>distance to the destination point</L>:
  K: Kilometers
  M: Miles
  N: Knots
Default value is empty string, which means "unknown". An unknown value is not written when saving EXIF values.
!!}
    property EXIF_GPSDestDistanceRef: AnsiString read fEXIF_GPSDestDistanceRef write fEXIF_GPSDestDistanceRef;

{!!
<FS>TIOParamsVals.EXIF_GPSDestDistance

<FM>Declaration<FC>
property EXIF_GPSDestDistance: Double;

<FM>Description<FN>
Returns the distance to destination.

See also: <A TIOParamsVals.EXIF_GPSDestDistanceRef>
!!}
    property EXIF_GPSDestDistance: Double read fEXIF_GPSDestDistance write fEXIF_GPSDestDistance;

{!!
<FS>TIOParamsVals.EXIF_GPSDateStamp

<FM>Declaration<FC>
property EXIF_GPSDateStamp: AnsiString;

<FM>Description<FN>
Returns the GPS date.
!!}
    property EXIF_GPSDateStamp: AnsiString read fEXIF_GPSDateStamp write fEXIF_GPSDateStamp;


    //// Exif-interoperability ////

{!!
<FS>TIOParamsVals.EXIF_InteropIndex

<FM>Declaration<FC>
property EXIF_InteropIndex: AnsiString;

<FM>Description<FN>
Returns the EXIF interoperability index.
!!}
    property EXIF_InteropIndex: AnsiString read fEXIF_InteropIndex write fEXIF_InteropIndex;

{!!
<FS>TIOParamsVals.EXIF_InteropVersion

<FM>Declaration<FC>
property EXIF_InteropVersion: AnsiString;

<FM>Description<FN>
Returns the EXIF interoperability version.
!!}
    property EXIF_InteropVersion: AnsiString read fEXIF_InteropVersion write fEXIF_InteropVersion;


    //// XMP ////

{!!
<FS>TIOParamsVals.XMP_Info

<FM>Declaration<FC>
property XMP_Info: AnsiString;

<FM>Description<FN>
Returns the Adobe XMP info loaded from Jpeg, TIFF and PSD file formats. Adobe XMP is an XML coded string defined by the Adobe XMP Specification (updated to January 2004). ImageEn doesn't parse the XMP-XML string, so to read single tags you will need to write code to parse the XML string.

XMP_Info is also saved back to Jpeg, TIFF and PSD file formats.

You can read parsed values using <A TIOParamsVals.Dict> dictionary, under "XMP" key:
<FC>xmp := ImageEnView.IO.Params.Dict.GetDictionary('XMP');<FN>
!!}
    property XMP_Info: AnsiString read fXMP_Info write SetXMP_Info;

    // RAW Cameras
    {$ifdef IEINCLUDERAWFORMATS}

{!!
<FS>TIOParamsVals.RAW_HalfSize

<FM>Declaration<FC>
property RAW_HalfSize: Boolean;

<FM>Description<FN>
Set to True before loading to retrieve only a half-size image of a Raw image. This will speed up loading.

Default: False

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_HalfSize := True;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_HalfSize: Boolean read fRAW_HalfSize write fRAW_HalfSize;

{!!
<FS>TIOParamsVals.RAW_Gamma

<FM>Declaration<FC>
property RAW_Gamma: Double;

<FM>Description<FN>
Specifies the gamma used when loading a RAW image.

Default: 0.6

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_Gamma := 1;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_Gamma: Double read fRAW_Gamma write fRAW_Gamma;
       

{!!
<FS>TIOParamsVals.RAW_Bright

<FM>Declaration<FC>
property RAW_Bright: Double

<FM>Description<FN>
Specifies the brightness value used when loading a RAW image.

Default: 1.0

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_Bright := 1.5;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_Bright: Double read fRAW_Bright write fRAW_Bright;

{!!
<FS>TIOParamsVals.RAW_RedScale

<FM>Declaration<FC>
property RAW_RedScale: Double

<FM>Description<FN>
Specifies the red multiplier used when loading a RAW image.

Default: 1.0 (Daylight)

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_RedScale := 0.8;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_RedScale: Double read fRAW_RedScale write fRAW_RedScale;

{!!
<FS>TIOParamsVals.RAW_BlueScale

<FM>Declaration<FC>
property RAW_BlueScale: Double

<FM>Description<FN>
Specifies the blue multiplier used when loading a RAW image.

Default: 1.0 (Daylight)

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_BlueScale := 0.8;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_BlueScale: Double read fRAW_BlueScale write fRAW_BlueScale;

{!!
<FS>TIOParamsVals.RAW_QuickInterpolate

<FM>Declaration<FC>
property RAW_QuickInterpolate: Boolean;

<FM>Description<FN>
If True, a quick, low-quality color interpolation is performed.

Default: True

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_QuickInterpolate := False;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_QuickInterpolate: Boolean read fRAW_QuickInterpolate write fRAW_QuickInterpolate;

{!!
<FS>TIOParamsVals.RAW_UseAutoWB

<FM>Declaration<FC>
property RAW_UseAutoWB: Boolean;

<FM>Description<FN>
If True, automatic white balance is performed.

Default: False

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_UseAutoWB := True;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_UseAutoWB: Boolean read fRAW_UseAutoWB write fRAW_UseAutoWB;

{!!
<FS>TIOParamsVals.RAW_ExtraParams

<FM>Declaration<FC>
property RAW_ExtraParams: AnsiString;

<FM>Description<FN>
Specifies extra parameters for external raw plugins (such as Dcraw).

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_ExtraParams := '-H 0 -j';
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_ExtraParams: AnsiString read fRAW_ExtraParams write fRAW_ExtraParams;

{!!
<FS>TIOParamsVals.RAW_UseCameraWB

<FM>Declaration<FC>
property RAW_UseCameraWB: Boolean

<FM>Description<FN>
If True, The camera's white balance is used if possible.

Default: False

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_UseCameraWB := True;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_UseCameraWB: Boolean read fRAW_UseCameraWB write fRAW_UseCameraWB;

{!!
<FS>TIOParamsVals.RAW_FourColorRGB

<FM>Declaration<FC>
property RAW_FourColorRGB: Boolean

<FM>Description<FN>
If True, ImageEn interpolates RGBG as four colors.

Default: False

<FM>Example<FC>
ImageEnView1.IO.Params.RAW_FourColorRGB := True;
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
!!}
    property RAW_FourColorRGB: Boolean read fRAW_FourColorRGB write fRAW_FourColorRGB;

{!!
<FS>TIOParamsVals.RAW_Camera

<FM>Declaration<FC>
property RAW_Camera: AnsiString;

<FM>Description<FN>
Returns the camera description.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\CRW_0001.CRW');
Cameraname := ImageEnView1.IO.Params.RAW_Camera;
!!}
    property RAW_Camera: AnsiString read fRAW_Camera write fRAW_Camera;

{!!
<FS>TIOParamsVals.RAW_GetExifThumbnail

<FM>Declaration<FC>
property RAW_GetExifThumbnail: Boolean;

<FM>Description<FN>
Specifies that the thumbnail for an image will be loaded instead of the full image. A thumbnail is often available for Raw images (EXIF Thumbnail).
If enabled and the file does not contain a thumbnail the full image will be automatically loaded instead.

See also: <A TIOParamsVals.GetThumbnail>

<FM>Example<FC>
// Load only the thumbnail
ImageEnView1.IO.Params.RAW_GetExifThumbnail := True;
ImageEnView1.IO.LoadFromFile('C:\input.crw');
!!}
    property RAW_GetExifThumbnail: Boolean read fRAW_GetExifThumbnail write SetRAW_GetExifThumbnail;

{!!
<FS>TIOParamsVals.RAW_AutoAdjustColors

<FM>Declaration<FC>
property RAW_AutoAdjustColors: Boolean;

<FM>Description<FN>
If True, ImageEn applies an algorithm to adjust image colors.

Note: This works only when the RAW file contains an EXIF thumbnail with correct colors, as it used to calculate image colors.

<FM>Example<FC>
ImageEnView.IO.Params.RAW_AutoAdjustColors := True;
ImageEnView.IO.LoadFromFile('C:\input.crw');

!!}
    property RAW_AutoAdjustColors: Boolean read fRAW_AutoAdjustColors write fRAW_AutoAdjustColors;

    {$endif}

    // Real RAWs

{!!
<FS>TIOParamsVals.BMPRAW_ChannelOrder

<FM>Declaration<FC>
property BMPRAW_ChannelOrder: <A TIOBMPRAWChannelOrder>;

<FM>Description<FN>
Specifies the channels order for a BmpRaw file (a true "raw" format, not the same as a digital camera Raw file). It can be coRGB or coBGR and it is valid only for color images.

<FM>Demo<FN>
InputOutput/RealRAW

<FM>Examples<FC>
// load a RAW image, RGB, interleaved, 8 bit aligned, 1024x768
ImageEnView1.LegacyBitmap := False;
ImageEnView1.IEBitmap.Allocate(1024, 768, ie24RGB);
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plInterleaved;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.Params.BMPRAW_HeaderSize := 0;
ImageEnView1.IO.LoadFromFileBMPRAW('C:\input.dat');

// load a RAW image, CMYK, interleaved, 8 bit aligned, 1024x768
ImageEnView1.LegacyBitmap := False;
ImageEnView1.IEBitmap.Allocate(1024, 768, ieCMYK);
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plInterleaved;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.Params.BMPRAW_HeaderSize := 0;
ImageEnView1.IO.LoadFromFileBMPRAW('C:\input.dat');

// saves current image as RAW image
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plPlanar;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.SaveToFileBMPRAW('C:\output.dat');
!!}
    property BMPRAW_ChannelOrder: TIOBMPRAWChannelOrder read fBMPRAW_ChannelOrder write fBMPRAW_ChannelOrder;

{!!
<FS>TIOParamsVals.BMPRAW_Planes

<FM>Declaration<FC>
property BMPRAW_Planes: <A TIOBMPRAWPlanes>;

<FM>Description<FN>
Specifies how channels of a BmpRaw file are disposed.

<FM>Demo<FN>
InputOutput/RealRAW

<FM>Example<FC>
See example at <A TIOParamsVals.BMPRAW_ChannelOrder>
!!}
    property BMPRAW_Planes: TIOBMPRAWPlanes read fBMPRAW_Planes write fBMPRAW_Planes;

{!!
<FS>TIOParamsVals.BMPRAW_RowAlign

<FM>Declaration<FC>
property BMPRAW_RowAlign: Integer;

<FM>Description<FN>
Specifies the row alignment in bits.

<FM>Demo<FN>
InputOutput/RealRAW

<FM>Example<FC>
See example at <A TIOParamsVals.BMPRAW_ChannelOrder>
!!}
    property BMPRAW_RowAlign: Integer read fBMPRAW_RowAlign write fBMPRAW_RowAlign;

{!!
<FS>TIOParamsVals.BMPRAW_HeaderSize

<FM>Declaration<FC>
property BMPRAW_HeaderSize: Integer;

<FM>Description<FN>
Specifies the header size in bytes. The header will be bypassed.

<FM>Demo<FN>
InputOutput/RealRAW

<FM>Example<FC>
See example at <A TIOParamsVals.BMPRAW_ChannelOrder>
!!}
    property BMPRAW_HeaderSize: Integer read fBMPRAW_HeaderSize write fBMPRAW_HeaderSize;

{!!
<FS>TIOParamsVals.BMPRAW_DataFormat

<FM>Declaration<FC>
property BMPRAW_DataFormat: <A TIOBMPRAWDataFormat>;

<FM>Description<FN>
Specifies the next input/output data format.
ASCII text values must be separated by one or more non-alpha characters (#13, #10, #32...).

<FM>Demo<FN>
InputOutput/RealRAW

<FM>Examples<FC>
// load a RAW image, 16 bit gray scale, ascii-text
ImageEnView1.LegacyBitmap := false;
ImageEnView1.IEBitmap.Allocate(1024, 768, ie16g);
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.Params.BMPRAW_HeaderSize := 0;
ImageEnView1.IO.Params.BMPRAW_DataFormat := dfTextDecimal;
ImageEnView1.IO.LoadFromFileBMPRAW('input.dat');
!!}
    property BMPRAW_DataFormat: TIOBMPRAWDataFormat read fBMPRAW_DataFormat write fBMPRAW_DataFormat;


    /////
    constructor Create(IEIO: TImageEnIO);
    destructor Destroy; override;
    procedure SetDefaultParams;
    procedure Assign(Source: TIOParamsVals);
    procedure AssignCompressionInfo(Source: TIOParamsVals);
    procedure SaveToFile(const FileName: WideString);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: WideString);
    procedure LoadFromStream(Stream: TStream);
    property ImageEnIO: TImageEnIO read fImageEnIO;
    procedure ResetInfo;
    procedure ResetEXIF;
    function GetProperty(const Prop: WideString): WideString;
    procedure SetProperty(Prop, Value: WideString);
    procedure UpdateEXIFThumbnail(Width: integer = 160; Height: integer = -1; ResampleFilter: TResampleFilter = rfTriangle);
    procedure FreeColorMap;
    procedure AdjustGPSCoordinates();

    function ReadIPTCField(iRecNo, iFieldIndex: Integer) : string; overload;
    procedure ReadIPTCField(iRecNo, iFieldIndex: Integer; Dest : TStrings); overload;
    procedure WriteIPTCField(iRecNo, iFieldIndex: Integer; const Value : string); overload;
    procedure WriteIPTCField(iRecNo, iFieldIndex: Integer; ssValues : TStrings); overload;
    procedure ClearIPTCField(iRecNo, iFieldIndex: Integer);
  end;





  // occurs after preview
{!!
<FS>TIEIOPreviewEvent

<FM>Declaration<FC>
}
  TIEIOPreviewEvent = procedure(Sender: TObject; PreviewForm: TForm) of object;
{!!}

{!!
<FS>TIEDoPreviewsEvent

<FM>Declaration<FC>
}
  TIEDoPreviewsEvent = procedure(Sender: TObject; var Handled: boolean) of object;
{!!}

{!!
<FS>TIECSSource

<FM>Declaration<FC>
TIECSSource = (iecsScreen, iecsPrimary, iecsForegroundWindow, iecsForegroundWindowClient);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iecsScreen</C> <C>Captures the entire screen (all screens on a multiple monitor system)</C> </R>
<R> <C>iecsPrimary</C> <C>Captures the screen of the primary monitor on a multiple monitor system (or the whole screen if there is only one monitor)</C> </R>
<R> <C>iecsForegroundWindow</C> <C>Captures the active window</C> </R>
<R> <C>iecsForegroundWindowClient</C> <C>Captures the client area of the active window</C> </R>
</TABLE>
!!}
  TIECSSource = (iecsScreen, iecsPrimary, iecsForegroundWindow, iecsForegroundWindowClient);

{!!
<FS>TIEDialogType

<FM>Declaration<FC>
TIEDialogType = (iedtDialog, iedtMaxi);

<FM>Description<FN>
iedtDialog : Show a dialog window.
iedtMaxi   : Show a maximized window.
!!}
  TIEDialogType = (iedtDialog, iedtMaxi);

{!!
<FS>TIEDialogsMeasureUnit

<FM>Declaration<FC>
TIEDialogsMeasureUnit = (ieduInches, ieduCm, ieduSelectableDefInches, ieduSelectableDefCm);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieduInches</C> <C>Use inches (default)</C> </R>
<R> <C>ieduCm</C> <C>Use centimeters (Cm)</C> </R>
<R> <C>ieduSelectableDefInches</C> <C>Use inches but allow the user to change it</C> </R>
<R> <C>ieduSelectableDefCm</C> <C>Use centimeters but allow the user to change it</C> </R>
</TABLE>
!!}
  TIEDialogsMeasureUnit = (ieduInches, ieduCm, ieduSelectableDefInches, ieduSelectableDefCm);

  // C++Builder doesn't work if we import IEVFW in interface uses
  TAviStreamInfoA_Ex = record
    fccType: DWORD;
    fccHandler: DWORD;
    dwFlags: DWORD; // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority: WORD;
    wLanguage: WORD;
    dwScale: DWORD;
    dwRate: DWORD; // dwRate / dwScale == samples/second
    dwStart: DWORD;
    dwLength: DWORD; // In units above...
    dwInitialFrames: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwQuality: DWORD;
    dwSampleSize: DWORD;
    rcFrame: TRECT;
    dwEditCount: DWORD;
    dwFormatChangeCount: DWORD;
    szName: array[0..63] of AnsiChar;
  end;

  TAviStreamInfoW_Ex = record
    fccType: DWORD;
    fccHandler: DWORD;
    dwFlags: DWORD; // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority: WORD;
    wLanguage: WORD;
    dwScale: DWORD;
    dwRate: DWORD;    // dwRate / dwScale == samples/second
    dwStart: DWORD;
    dwLength: DWORD;  // In units above...
    dwInitialFrames: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwQuality: DWORD;
    dwSampleSize: DWORD;
    rcFrame: TRECT;
    dwEditCount: DWORD;
    dwFormatChangeCount: DWORD;
    szName: array[0..63] of WideChar;
  end;

  TAviStreamInfo_Ex = {$IFDEF UNICODE}TAviStreamInfoW_Ex{$ELSE}TAviStreamInfoA_Ex{$ENDIF};



{!!
<FS>TIOPrintPreviewPosition

<FM>Declaration<FC>
}
  TIOPrintPreviewPosition=(ppTopLeft, ppTop, ppTopRight, ppLeft, ppCenter, ppRight, ppBottomLeft, ppBottom, ppBottomRight);
{!!}

{!!
<FS>TIOPrintPreviewSize

<FM>Declaration<FC>
}
  TIOPrintPreviewSize=(psNormal, psFitToPage, psStretchToPage, psFillPage, psSpecifiedSize);
{!!}


{!!
<FS>TIOPrintPreviewThumbnailStyle

<FM>Declaration<FC>
}
  TIOPrintPreviewThumbnailStyle=(ptFlat, ptSoftShadow, ptBorder);
{!!}


  {$IFDEF IEINCLUDEPRINTDIALOGS}
  
{!!
<FS>TIOPrintPreviewParams

<FM>Description<FN>
This class provides access to the properties of the Print Preview dialog.

<FM>Methods and Properties<FN>
<FI>Storage Handling<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIOPrintPreviewParams.GetProperty></C> </R>
<R> <C_IMG_METHOD> <C><A TIOPrintPreviewParams.LoadFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIOPrintPreviewParams.LoadFromStream></C> </R>
<R> <C_IMG_METHOD> <C><A TIOPrintPreviewParams.SaveToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIOPrintPreviewParams.SaveToStream></C> </R>
<R> <C_IMG_METHOD> <C><A TIOPrintPreviewParams.SetProperty></C> </R>
</TABLE>

<FI>Settings<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.Gamma></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.Height></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.MarginBottom></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.MarginLeft></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.MarginRight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.MarginTop></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.Position></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.Size></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.Width></C> </R>
</TABLE>

<FI>TImageEnMIO.DoPrintPreviewDialog Only<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.DlgWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.DlgHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.PrintThumbnails></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.ThumbnailColumns></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.ThumbnailRows></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.ThumbnailSpacing></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.ThumbnailStyle></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIOPrintPreviewParams.ThumbnailShowText></C> </R>
</TABLE>

!!}
  TIOPrintPreviewParams = class
    private
      fMarginTop: Double;
      fMarginLeft: Double;
      fMarginRight: Double;
      fMarginBottom: Double;
      fPosition: TIOPrintPreviewPosition;
      fSize: TIOPrintPreviewSize;
      fWidth: Double;    // <=0: autocalculated
      fHeight: Double;   // <=0: autocalculated
      fGamma: Double;
      fPrintThumbnails   : Boolean;
      fThumbnailColumns  : Integer;
      fThumbnailRows     : Integer;
      fThumbnailSpacing  : Double;
      fThumbnailStyle    : TIOPrintPreviewThumbnailStyle;
      fThumbnailShowText : Boolean;

      fDlgWidth          : Integer;
      fDlgHeight         : Integer;

    public
      constructor Create;

{!!
<FS>TIOPrintPreviewParams.MarginTop

<FM>Declaration<FC>
property MarginTop: Double;

<FM>Description<FN>
The top margin (unprintable area) of the printed page.
!!}
      property MarginTop: Double read fMarginTop write fMarginTop;

{!!
<FS>TIOPrintPreviewParams.MarginLeft

<FM>Declaration<FC>
property MarginLeft: Double;

<FM>Description<FN>   
The left margin (unprintable area) of the printed page.
!!}
      property MarginLeft: Double read fMarginLeft write fMarginLeft;

{!!
<FS>TIOPrintPreviewParams.MarginRight

<FM>Declaration<FC>
property MarginRight: Double;

<FM>Description<FN>    
The right margin (unprintable area) of the printed page.
!!}
      property MarginRight: Double read fMarginRight write fMarginRight;

{!!
<FS>TIOPrintPreviewParams.MarginBottom

<FM>Declaration<FC>
property MarginBottom: Double;

<FM>Description<FN>     
The bottom margin (unprintable area) of the printed page.
!!}
      property MarginBottom: Double read fMarginBottom write fMarginBottom;

{!!
<FS>TIOPrintPreviewParams.Position

<FM>Declaration<FC>
property Position: <A TIOPrintPreviewPosition>;

<FM>Description<FN>
The alignment of the image on the printed page.
!!}
      property Position: TIOPrintPreviewPosition read fPosition write fPosition;

{!!
<FS>TIOPrintPreviewParams.Size

<FM>Declaration<FC>
property Size: <A TIOPrintPreviewSize>;

<FM>Description<FN> 
The size of the image on the printed page.
!!}
      property Size: TIOPrintPreviewSize read fSize write fSize;

{!!
<FS>TIOPrintPreviewParams.Width

<FM>Declaration<FC>
property Width: Double;

<FM>Description<FN>
Width of the printed image when <A TIOPrintPreviewParams.Size> = psSpecifiedSize.
!!}
      property Width: Double read fWidth write fWidth;

{!!
<FS>TIOPrintPreviewParams.Height

<FM>Declaration<FC>
property Height: Double;

<FM>Description<FN>
Height of the printed image when <A TIOPrintPreviewParams.Size> = psSpecifiedSize.
!!}
      property Height: Double read fHeight write fHeight;

{!!
<FS>TIOPrintPreviewParams.Gamma

<FM>Declaration<FC>
property Gamma: Double;

<FM>Description<FN>
Gamma value.
!!}
      property Gamma: Double read fGamma write fGamma;


{!!
<FS>TIOPrintPreviewParams.PrintThumbnails

<FM>Declaration<FC>
property PrintThumbnails: Boolean;

<FM>Description<FN>
Default to printing of thumbnails (TImageEnMIO.<A TImageEnMIO.DoPrintPreviewDialog> only)
!!}
      property PrintThumbnails: Boolean read fPrintThumbnails write fPrintThumbnails;



{!!
<FS>TIOPrintPreviewParams.ThumbnailColumns

<FM>Declaration<FC>
property ThumbnailColumns: Integer;

<FM>Description<FN>
Number of thumbnail columns per page (TImageEnMIO.<A TImageEnMIO.DoPrintPreviewDialog> only)
!!}
      property ThumbnailColumns: Integer read fThumbnailColumns write fThumbnailColumns;


{!!
<FS>TIOPrintPreviewParams.ThumbnailRows

<FM>Declaration<FC>
property ThumbnailRows: Integer;

<FM>Description<FN>
Number of thumbnail rows per page (TImageEnMIO.<A TImageEnMIO.DoPrintPreviewDialog> only)
!!}
      property ThumbnailRows: Integer read fThumbnailRows write fThumbnailRows;


{!!
<FS>TIOPrintPreviewParams.ThumbnailSpacing

<FM>Declaration<FC>
property ThumbnailSpacing : Double;

<FM>Description<FN>
Spacing between each thumbnail in inches (TImageEnMIO.<A TImageEnMIO.DoPrintPreviewDialog> only)
!!}
      property ThumbnailSpacing : Double read fThumbnailSpacing write fThumbnailSpacing;

      
{!!
<FS>TIOPrintPreviewParams.ThumbnailStyle

<FM>Declaration<FC>       
property ThumbnailStyle: <A TIOPrintPreviewThumbnailStyle>;

<FM>Description<FN>
Style of printed thumbnails (TImageEnMIO.DoPrintPreviewDialog only)
!!}
      property ThumbnailStyle: TIOPrintPreviewThumbnailStyle read fThumbnailStyle write fThumbnailStyle;


{!!
<FS>TIOPrintPreviewParams.ThumbnailShowText

<FM>Declaration<FC>
property ThumbnailShowText: Boolean;

<FM>Description<FN>
Whether the caption from items in a TImageEnMView is included when printing thumbnails (TImageEnMIO.<A TImageEnMIO.DoPrintPreviewDialog> only)
!!}
      property ThumbnailShowText: Boolean read fThumbnailShowText write fThumbnailShowText;

{!!
<FS>TIOPrintPreviewParams.DlgWidth

<FM>Declaration<FC>
property DlgWidth : Integer;

<FM>Description<FN>
The width of the Preview dialog in pixels or zero for default size (TImageEnMIO.<A TImageEnMIO.DoPrintPreviewDialog> only)
!!}
      property DlgWidth : Integer read fDlgWidth write fDlgWidth;

      
{!!
<FS>TIOPrintPreviewParams.DlgHeight

<FM>Declaration<FC>
property DlgHeight : Integer;

<FM>Description<FN>
The Height of the Preview dialog in pixels or zero for default size (TImageEnMIO.<A TImageEnMIO.DoPrintPreviewDialog> only)
!!}
      property DlgHeight : Integer read fDlgHeight write fDlgHeight;

      procedure SaveToFile(const FileName: WideString);
      procedure LoadFromFile(const FileName: WideString);
      procedure SaveToStream(Stream: TStream);
      procedure LoadFromStream(Stream: TStream);
      procedure SetProperty(Prop, Value: WideString);
      function GetProperty(const Prop: WideString): WideString;



  end;
  {$ENDIF}


//////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////
// TImageEnIO


{!!
<FS>TImageEnIO.TIEIOSeekDestination

<FM>Declaration<FC>
TIEIOSeekDestination = (ieioSeekFirst, ieioSeekPrior, ieioSeekNext, ieioSeekLast);

<FM>Description<FN>
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ieioSeekFirst<FN></C> <C>Go to first page</C> </R>
<R> <C><FC>ieioSeekPrior<FN></C> <C>Go to previous page (or first page)</C> </R>
<R> <C><FC>ieioSeekNext<FN></C> <C>Go to next page (or last page)</C> </R>
<R> <C><FC>ieioSeekLast<FN></C> <C>Go to last page</C> </R>
</TABLE>
!!}
  TIEIOSeekDestination = (ieioSeekFirst, ieioSeekPrior, ieioSeekNext, ieioSeekLast);


{!!
<FS>TImageEnIO

<FM>Description<FN>
TImageEnIO provides input/output support to ImageEn:
- <L TImageEnIO.LoadFromFile>Loading</L> and <L TImageEnIO.SaveToFile>saving</L> images
- Access to <L TImageEnIO.Params>image properties and meta-data</L>
- <L TImageEnIO.Acquire>Acquisition</L> of images from cameras and scanners
- <L TImageEnIO.DoPrintPreviewDialog>Printing</L>
- <L TImageEnIO.DShowParams>DirectShow</L> functions for multimedia support

See also: <A TImageEnMIO>, which is made to work with images containing multiple frames (e.g. animated GIFs)


Generally you will not add a TImageEnIO component directly to your project. It is accessed via the following methods:
<FM>1. Using the <A TImageEnView.IO> property of a <A TImageEnView> or <A TImageEnVect><FN>
ImageEnView1.IO.LoadFromFile('C:\MyImage.jpeg');
ImageEnView1.IO.DoPrintPreviewDialog;

<FM>2. Attached to a TBitmap or <A TIEBitmap> in code<FN>
MyBitmap := TBitmap.create;
AnImageEnIO := TImageEnIO.CreateFromBitmap(MyBitmap);
AnImageEnIO.LoadFromFile('C:\input.jpg');
...
AnImageEnIO.Free;
MyBitmap.Free;

<FM>3. Attached to a TImage<FN>
Note: In this case a TImageEnIO would be added to the form and the <A TImageEnIO.AttachedTImage> set appropriately.

<FM>Notes<FN>
- Ensure you do not call any TImageEnIO methods before it is actually attached to an image container (<A TImageEnView>, <A TIEBitmap>, etc)
- Do not attach TImageEnIO to a <A TImageEnMView> component. Only a <A TImageEnMIO> can be attached to a <A TImageEnMView> component.
<FN>

<FM>Methods and Properties<FN>
<FI>Connected component<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.AttachedBitmap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.AttachedIEBitmap></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.AttachedImageEn></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.AttachedTImage></C> </R>
</TABLE>

<FI>Generic Input/Output<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.Aborting></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.AssignParams></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.AutoAdjustDPI></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.Background></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.Bitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CaptureFromScreen></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.ChangeBackground></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.Create></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CreateFromBitmap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.DefaultDitherMethod></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.FilteredAdjustDPI></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.IEBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromBuffer></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileAuto></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileFormat></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromResource></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamFormat></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStream></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromText></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromURL></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.NativePixelFormat></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.Params></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ParamsFromBuffer></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ParamsFromFileFormat></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ParamsFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ParamsFromStreamFormat></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ParamsFromStream></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.ProxyAddress></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.ProxyPassword></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.ProxyUser></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStream></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToText></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.Seek></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.StreamHeaders></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.Update></C> </R></C> </R>
</TABLE>

<FI>Image Acquisition (Twain/WIA)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.Acquire></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SelectAcquireSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.SelectedAcquireSource></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SetAcquireSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.AcquireParams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.DCIMParams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.TwainParams></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.TwainAcquireOpen></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.TwainAcquireClose></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.WIAParams></C> </R>
</TABLE>

<FI>Aynchronous input/output<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.AsyncMode></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.AsyncRunning></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.ThreadsCount></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.WaitThreads></C> </R>
</TABLE>

<FI>DirectShow capture<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.DShowParams></C> </R>
</TABLE>

<FI>Dialogs<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.DialogsMeasureUnit></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.DoPreviews></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ExecuteOpenDialog></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ExecuteSaveDialog></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.PreviewFont></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.PreviewFontEnabled></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.PreviewsParams></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnIO.SimplifiedParamsDialogs></C> </R>
</TABLE>

<FI>Printing<FN>      
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.DoPrintPreviewDialog></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.PreviewPrintImage></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.PrintImagePos></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.PrintImage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.PrintingFilterOnSubsampling></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnIO.PrintPreviewParams></C> </R>
</TABLE>

<FI>JPEG<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InjectJpegEXIFStream></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InjectJpegEXIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InjectJpegIPTCStream></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InjectJpegIPTC></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileJpeg></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamJpeg></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileJpeg></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamJpeg></C> </R>
</TABLE>

<FI>JPEG 2000<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileJ2K></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileJP2></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamJ2K></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamJP2></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileJ2K></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileJP2></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamJ2K></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamJP2></C> </R>
</TABLE>

<FI>GIF<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InsertToFileGIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileGIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamGIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileGIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamGIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ReplaceFileGIF></C> </R>
</TABLE>

<FI>Adobe PSD<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFilePSD></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamPSD></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFilePSD></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamPSD></C> </R>
</TABLE>

<FI>Microsoft HD Photos (HDP)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileHDP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamHDP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileHDP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamHDP></C> </R>
</TABLE>

<FI>TIFF<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InjectTIFFEXIF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InsertToFileTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InsertToStreamTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ReplaceFileTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ReplaceStreamTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileTIFF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamTIFF></C> </R>
</TABLE>

<FI>BMP<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileBMP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamBMP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileBMP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamBMP></C> </R>
</TABLE>

<FI>PNG<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFilePNG></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamPNG></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFilePNG></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamPNG></C> </R>
</TABLE>

<FI>Cursors (CUR)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileCUR></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamCUR></C> </R>
</TABLE>

<FI>ICO<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileICO></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamICO></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileICO></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamICO></C> </R>
</TABLE>

<FI>PCX<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFilePCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamPCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFilePCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamPCX></C> </R>
</TABLE>   

<FI>DCX (Multipage PCX)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileDCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamDCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamDCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileDCX></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.InsertToFileDCX></C> </R>
</TABLE>

<FI>Meta Files (WMF, EMF)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ImportMetafile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.MergeMetaFile></C> </R>
</TABLE>

<FI>Targa (TGA)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileTGA></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamTGA></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileTGA></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamTGA></C> </R>
</TABLE>

<FI>PXM (PPM, PBM, PGM)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFilePXM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamPXM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFilePXM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamPXM></C> </R>
</TABLE>

<FI>AVI Videos<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CloseAVIFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CreateAVIFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.IsOpenAVI></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromAVI></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.OpenAVIFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToAVI></C> </R>
</TABLE>

<FI>WBMP<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileWBMP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamWBMP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileWBMP></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamWBMP></C> </R>
</TABLE>

<FI>PostScript (PS)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ClosePSFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CreatePSFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFilePS></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToPS></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamPS></C> </R>
</TABLE>

<FI>Adobe PDF<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.ClosePDFFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CreatePDFFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFilePDF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToPDF></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamPDF></C> </R>
</TABLE>

<FI>Raw Camera Files<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileRAW></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamRAW></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadJpegFromFileCRW></C> </R>
</TABLE>

<FI>Media Files (AVI, MPEG, WMV..)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.CloseMediaFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.IsOpenMediaFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromMediaFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.OpenMediaFile></C> </R>
</TABLE>

<FI>BmpRaw (a true "raw" format, not the same as a digital camera Raw file)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileBMPRAW></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamBMPRAW></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileBMPRAW></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamBMPRAW></C> </R>
</TABLE>

<FI>DICOM Medical Imaging Format<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromFileDICOM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.LoadFromStreamDICOM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToFileDICOM></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnIO.SaveToStreamDICOM></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TImageEnIO.OnAcquireBitmap></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnIO.OnAcquireClose></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnIO.OnDoPreviews></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnIO.OnFinishWork></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnIO.OnIOPreview></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnIO.OnProgress></C> </R>
</TABLE>
!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TImageEnIO = class(TComponent)

  private

    fBitmap: TBitmap; // refers to the bitmap (if fImageEnView is valid then it is FImageEnView.bitmap)
    fIEBitmap: TIEBitmap; // encapsulates fBitmap if SetBitmap, SetAttachedBitmap, SetAttachedImageEn, SetTImage are called
    fIEBitmapCreated: boolean; // true is fIEBitmap is created by TImageEnIO
    fImageEnView: TIEView; // refers to TIEView (fbitmap=fimageenview.bitmap)
    fImageEnViewBitmapChangeHandle: Pointer; // bitmap change handle (nil=none)
    fTImage: TImage; // refers to TImage
    fBackground: TColor; // valid only if fImageEnview = nil
    fPreviewsParams: TIOPreviewsParams;
    fSimplifiedParamsDialogs: boolean;
    fOnDoPreviews: TIEDoPreviewsEvent;
    fChangeBackground: boolean; // if true change the Background property using the loaded image background
    {$ifdef IEINCLUDEIEXACQUIRE}
    fAcquireParams : TIEAcquireParams;
    fTwainParams: TIETwainParams;
    fDCIMParams : TIEDcimAcquire;
    {$endif}
    fStreamHeaders: boolean; // enable/disable load/save stream headers
    fPreviewFont: TFont;
    fPreviewFontEnabled: Boolean;
    fOnIOPreview: TIEIOPreviewEvent;
    fDialogsMeasureUnit: TIEDialogsMeasureUnit;
    fAutoAdjustDPI: boolean;
    fFilteredAdjustDPI: boolean;
    fAVI_avf: pointer; // PAVIFILE
    fAVI_avs: pointer; // PAVISTREAM
    fAVI_avs1: pointer; // PAVISTREAM
    fAVI_gf: pointer; // PGETFRAME
    fAVI_psi: TAviStreamInfoW_Ex;
    fAVI_popts: pointer; // PAVICOMPRESSOPTIONS
    fAVI_idx: integer;
    {$ifdef IEINCLUDEDIRECTSHOW}
    fOpenMediaFile: TIEDirectShow; // open media file
    fOpenMediaFileRate: Double;
    fOpenMediaFileMul: Integer;
    {$endif}
    fPS_handle: pointer;
    fPS_stream: TIEWideFileStream;
    fPDF_handle: pointer;
    fPDF_stream: TIEWideFileStream;
    fAsyncThreads: TList; // Count > 0 is one or more threads are running
    fAsyncThreadsFinishEvent: THandle;
    fAsyncThreadsCS: TRTLCriticalSection; // protect fAsyncThreads access
    fAsyncMode: boolean; // true if we need async mode
    fPrintingFilterOnSubsampling: TResampleFilter;
    // Twain modeless
    fgrec: pointer;
    // WIA
    {$IFDEF IEINCLUDEIEXACQUIRE}
    fWIA: TIEWia;
    {$ENDIF}
    // DirectShow
    {$IFDEF IEINCLUDEDIRECTSHOW}
    fDShow: TIEDirectShow;
    {$ENDIF}
    // Media Foundation Grabber
    {$IFDEF IEINCLUDEMEDIAFOUNDATION}
    fMediaFoundationSourceReader: TIEMediaFoundationSourceReader;
    {$ENDIF}
    // proxy settings
    fProxyAddress, fProxyUser, fProxyPassword: WideString;
    //
    fDefaultDitherMethod: TIEDitherMethod;
    fResetPrinter: boolean;
    {$IFDEF IEINCLUDEPRINTDIALOGS}
    fPrintPreviewParams: TIOPrintPreviewParams;
    {$ENDIF}
    //
    function IsInsideAsyncThreads: boolean;
    procedure SetAttachedBitmap(atBitmap: TBitmap);
    procedure SetAttachedImageEn(atImageEn: TIEView);
    function GetReBackground: TColor;
    procedure SetReBackground(v: TColor);
    procedure SetPreviewFont(f: TFont);
    procedure SetPreviewFontEnabled(Value: Boolean);
    procedure SetTImage(v: TImage);
    procedure SetIOPreviewParams(v: TIOPreviewsParams);
    function GetIOPreviewParams: TIOPreviewsParams;
    procedure AdjustDPI;
    function GetAsyncRunning: integer;
    function GetThreadsCount: integer;
    procedure SetDefaultDitherMethod(Value: TIEDitherMethod);
    {$IFDEF IEINCLUDEIEXACQUIRE}
    function WiaOnProgress(Percentage: integer): boolean;
    {$ENDIF}
    procedure SetPrintLogFile(v: String);
    function GetPrintLogFile: String;
    procedure UpdateAPP138BimResolution;
    procedure ParamsFromMetaFile(stream: TStream);
    function GetImageEnVersion: String;
    procedure SetImageEnVersion(Value: String);
    procedure SetAborting(Value: Boolean);

  protected

{$IFNDEF IEDEBUG}
    fParams: TIOParamsVals;
{$ENDIF}
    fAborting: boolean;
    fOnIntProgress: TIEProgressEvent; // internal progress event, always assigned to DoIntProgress
    fOnProgress: TIEProgressEvent;
    fOnFinishWork: TNotifyEvent;
    fOnAcquireBitmap: TIEAcquireBitmapEvent;
    fOnAcquireClose: TNotifyEvent;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnBitmapChange(Sender: TObject; destroying: boolean);
    procedure PrintImageEx(PrtCanvas: TCanvas; dpix, dpiy: integer; pagewidth, pageheight: double; MarginLeft, MarginTop, MarginRight, MarginBottom: double; VerticalPos: TIEVerticalPos; HorizontalPos: TIEHorizontalPos; Size: TIESize; SpecWidth, SpecHeight: double; GammaCorrection: double);

{$IFDEF IEINCLUDEJPEG2000}
    function LoadFromStreamJ2000(Stream: TStream): Boolean;
    procedure SaveToStreamJ2000(Stream: TStream; format: integer);
{$ENDIF} // IEINCLUDEJPEG2000
    procedure SetBitmap(bmp: TBitmap);
    procedure SetIEBitmap(bmp: TIEBitmap);
    procedure SetAttachedIEBitmap(bmp: TIEBitmap);
    procedure TWMultiCallBack(Bitmap: TIEBitmap; var IOParams: TObject; ImDpiX, ImDpiY: integer); virtual;
    procedure TWCloseCallBack; virtual;
    procedure DoAcquireBitmap(ABitmap: TIEBitmap; var Handled: boolean); dynamic;
    procedure DoFinishWork; virtual;
    function GetBitmap: TBitmap; virtual;
    function MakeConsistentBitmap(allowedFormats: TIEPixelFormatSet): boolean;
    {$IFDEF IEINCLUDEIEXACQUIRE}
    function GetWIAParams: TIEWia; virtual;
    {$ENDIF}
    {$IFDEF IEINCLUDEDIRECTSHOW}
    function GetDShowParams: TIEDirectShow; virtual;
    {$ENDIF}
    {$IFDEF IEINCLUDEMEDIAFOUNDATION}
    function GetMediaFoundationSourceReader(): TIEMediaFoundationSourceReader;
    {$ENDIF}
    function SyncLoadFromStreamGIF(Stream: TStream): integer;
    procedure SyncLoadFromStreamPCX(Stream: TStream; streamhead: boolean);
    procedure SyncLoadFromStreamDCX(Stream: TStream);
    function SyncLoadFromStreamTIFF(Stream: TStream; streamhead: boolean): integer;
    procedure SyncLoadFromStreamBMP(Stream: TStream);
    procedure SyncSaveToStreamJpeg(Stream: TStream; streamhead: boolean);
    procedure SyncSaveToStreamPS(Stream: TStream);
    {$ifdef IEINCLUDEPDFWRITING}
    procedure SyncSaveToStreamPDF(Stream: TStream);
    {$endif}
    {$ifdef IEINCLUDEDICOM}
    procedure SyncSaveToStreamDICOM(Stream: TStream);
    {$endif}
    procedure SyncSaveToStreamBMP(Stream: TStream);
    procedure SyncSaveToStreamDCX(Stream: TStream);
    {$ifdef IEINCLUDEPSD}
    procedure SyncLoadFromStreamPSD(Stream: TStream);
    {$endif}
    procedure SyncLoadFromStreamCUR(Stream: TStream);
    procedure SyncLoadFromStreamICO(Stream: TStream);
    {$ifdef IEINCLUDEWIC}
    procedure SyncLoadFromStreamHDP(Stream: TStream);
    {$endif}
    {$ifdef IEINCLUDERAWFORMATS}
    procedure SyncLoadFromStreamRAW(Stream: TStream);
    {$endif}
    procedure SyncLoadFromStreamBMPRAW(Stream: TStream);
    procedure SyncSaveToStreamBMPRAW(Stream: TStream);
    procedure CheckDPI;
    procedure SetNativePixelFormat(value: Boolean);
    function GetNativePixelFormat: Boolean;
    procedure DoIntProgress(Sender: TObject; per: integer); virtual;
    {$ifdef IEINCLUDEPSD}
    procedure SyncSaveToStreamPSD(Stream: TStream);
    {$endif}
    {$ifdef IEINCLUDEWIC}
    procedure SyncSaveToStreamHDP(Stream: TStream);
    {$endif}
    function LoadViewerFile(const fileName: WideString; ft: TIOFileType): Boolean;
    function LoadViewerStream(Stream: TStream; ft: TIOFileType): Boolean;
    procedure SetViewerDPI(dpix, dpiy: Integer); virtual;
    {$ifdef IEINCLUDEIEXACQUIRE}
    function GetSelectedAcquireSource : TIEAcquireSource;
    {$endif}
  public

{$IFDEF IEDEBUG}
    fParams: TIOParamsVals;
{$ENDIF}
    constructor Create(Owner: TComponent); override;
    constructor CreateFromBitmap(Bitmap: TIEBitmap); overload;
    constructor CreateFromBitmap(Bitmap: TBitmap); overload;
    destructor Destroy; override;
    property AttachedBitmap: TBitmap read fBitmap write SetAttachedBitmap;
    property AttachedIEBitmap: TIEBitmap read fIEBitmap write SetAttachedIEBitmap;
    procedure Update;
    procedure SyncGetHandle;

{!!
<FS>TImageEnIO.ChangeBackground

<FM>Declaration<FC>
property ChangeBackground: Boolean;

<FM>Description<FN>
When set to True, the background color of the attached <A TImageEnView> will automatically be set to to the image background color.

Default: False

Note: Not all file formats contains background color information.
!!}
    property ChangeBackground: boolean read fChangeBackground write fChangeBackground;

    property ThreadsCount: integer read GetThreadsCount;
    property DefaultDitherMethod: TIEDitherMethod read fDefaultDitherMethod write SetDefaultDitherMethod;
{$IFDEF IEINCLUDEDIALOGIO}
    function DoPreviews(pp: TPreviewParams = [ppAll]): boolean; virtual;
{$ENDIF} // IEINCLUDEDIALOGIO
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property IEBitmap: TIEBitmap read fIEBitmap write SetIEBitmap;


{!!
<FS>TImageEnIO.Params

<FM>Declaration<FC>
property Params: <A TIOParamsVals>;

<FM>Description<FN>
Provides access to the parameters and meta-data of the current image as a <A TIOParamsVals> object. This object contains many format specific properties, such as bits per sample, type of compression, embedded text data, etc. It is filled automatically when loading images or manually using <A TImageEnIO.ParamsFromFile>. The properties can also be set or modified when saving files.

<FM>Example<FC>
// Sets color-mapped 256 colors
ImageEnView1.IO.Params.BitsPerSample := 8;
ImageEnView1.IO.Params.SamplesPerPixel := 1;

// Sets JPEG quality of 90%          
ImageEnView1.IO.Params.JPEG_Quality := 90;
ImageEnView1.IO.SaveToFile('D:\output.jpg');
!!}
    property Params: TIOParamsVals read fParams;

    procedure AssignParams(Source: TObject);
    procedure RecreatedTImageEnViewHandle;

    {$ifdef IEINCLUDERESOURCEEXTRACTOR}
    procedure LoadFromResource(const ModulePath: WideString; const ResourceType: String; const ResourceName: String; Format: TIOFileType);
    {$endif}

    // JPEG
    procedure SaveToFileJpeg(const FileName: WideString);
    function LoadFromFileJpeg(const FileName: WideString): Boolean;
    procedure SaveToStreamJpeg(Stream: TStream);
    function LoadFromStreamJpeg(Stream: TStream): Boolean;
    function InjectJpegIPTC(const FileName: WideString): boolean;
    function InjectJpegIPTCStream(InputStream, OutputStream: TStream): boolean;
    function InjectJpegEXIF(const FileName: WideString): boolean;
    function InjectJpegEXIFStream(InputStream, OutputStream: TStream): boolean;
    // DCX
    function LoadFromFileDCX(const FileName: WideString): Boolean;
    function LoadFromStreamDCX(Stream: TStream): Boolean;
    procedure SaveToStreamDCX(Stream: TStream);
    procedure SaveToFileDCX(const FileName: WideString);
    procedure InsertToFileDCX(const FileName: WideString);
    // JPEG2000
{$IFDEF IEINCLUDEJPEG2000}
    function LoadFromFileJP2(const FileName: WideString): Boolean;
    function LoadFromFileJ2K(const FileName: WideString): Boolean;
    function LoadFromStreamJP2(Stream: TStream): Boolean;
    function LoadFromStreamJ2K(Stream: TStream): Boolean;
    procedure SaveToStreamJP2(Stream: TStream);
    procedure SaveToStreamJ2K(stream: TStream);
    procedure SaveToFileJP2(const FileName: WideString);
    procedure SaveToFileJ2K(const FileName: WideString);
{$ENDIF} // IEINCLUDEJPEG2000
    // GIF
    function LoadFromFileGIF(const FileName: WideString): integer;
    procedure SaveToFileGIF(const FileName: WideString);
    function InsertToFileGIF(const FileName: WideString): integer;
    function LoadFromStreamGIF(Stream: TStream): integer;
    procedure SaveToStreamGIF(Stream: TStream);
    function ReplaceFileGIF(const FileName: WideString): integer;
    // PCX
    procedure SaveToStreamPCX(Stream: TStream);
    function LoadFromStreamPCX(Stream: TStream): Boolean;
    procedure SaveToFilePCX(const FileName: WideString);
    function LoadFromFilePCX(const FileName: WideString): Boolean;
    // TIFF
    function LoadFromStreamTIFF(Stream: TStream): integer;
    procedure SaveToStreamTIFF(Stream: TStream);
    function LoadFromFileTIFF(const FileName: WideString): integer;
    procedure SaveToFileTIFF(const FileName: WideString);
    function InsertToFileTIFF(const FileName: WideString): integer;
    function InsertToStreamTIFF(Stream: TStream): integer;
    function ReplaceFileTIFF(const FileName: WideString): integer;
    function ReplaceStreamTIFF(Stream: TStream): integer;
    {$ifdef IEINCLUDETIFFHANDLER}
    function InjectTIFFEXIF(const FileName: WideString; pageIndex: Integer = 0): boolean; overload;
    function InjectTIFFEXIF(const InputFileName, OutputFileName: WideString; pageIndex: Integer = 0): boolean; overload;
    function InjectTIFFEXIF(InputStream, OutputStream: TStream; pageIndex: Integer = 0): boolean; overload;
    {$endif}
    // BMP
    procedure SaveToStreamBMP(Stream: TStream);
    function LoadFromStreamBMP(Stream: TStream): Boolean;
    procedure SaveToFileBMP(const FileName: WideString);
    function LoadFromFileBMP(const FileName: WideString): Boolean;
    // ICO
    function LoadFromFileICO(const FileName: WideString): Boolean;
    function LoadFromStreamICO(Stream: TStream): Boolean;
    procedure SaveToStreamICO(Stream: TStream);
    procedure SaveToFileICO(const FileName: WideString);
    // CUR
    function LoadFromFileCUR(const FileName: WideString): Boolean;
    function LoadFromStreamCUR(Stream: TStream): Boolean;
    // PNG
    {$IFDEF IEINCLUDEPNG}
    function LoadFromFilePNG(const FileName: WideString): Boolean;
    function LoadFromStreamPNG(Stream: TStream): Boolean;
    procedure SaveToFilePNG(const FileName: WideString);
    procedure SaveToStreamPNG(Stream: TStream);
    {$ENDIF}
    {$ifdef IEINCLUDEDICOM}
    // DICOM
    function LoadFromFileDICOM(const FileName: WideString): Boolean;
    function LoadFromStreamDICOM(Stream: TStream): Boolean;
    procedure SaveToStreamDICOM(Stream: TStream);
    procedure SaveToFileDICOM(const FileName: WideString);
    {$endif}
    // TGA
    function LoadFromFileTGA(const FileName: WideString): Boolean;
    function LoadFromStreamTGA(Stream: TStream): Boolean;
    procedure SaveToFileTGA(const FileName: WideString);
    procedure SaveToStreamTGA(Stream: TStream);
    // METAFILE (WMF, EMF)
    function ImportMetafile(const FileName: WideString; Width: Integer = -1; Height: Integer = -1; WithAlpha: boolean = True): Boolean; overload;
    function ImportMetafile(Stream: TStream; Width: Integer = -1; Height: Integer = -1; WithAlpha: boolean = True): Boolean; overload;
    function ImportMetaFile(meta: TMetaFile; Width: Integer = -1; Height: Integer = -1; WithAlpha: boolean = True): Boolean; overload;
    procedure MergeMetafile(const FileName: WideString; x, y: Integer; Width: Integer = -1; Height: Integer = -1);
    // PXM, PBM, PGM, PPM
    function LoadFromFilePXM(const FileName: WideString): Boolean;
    function LoadFromStreamPXM(Stream: TStream): Boolean;
    procedure SaveToFilePXM(const FileName: WideString);
    procedure SaveToStreamPXM(Stream: TStream);
    // AVI
    function OpenAVIFile(const FileName: WideString): integer;
    procedure CloseAVIFile;
    function LoadFromAVI(FrameIndex: integer): Boolean;
    function CreateAVIFile(const FileName: WideString; rate: Double = 15.0; const codec: AnsiString='DIB '): TIECreateAVIFileResult;
    procedure SaveToAVI;
    function IsOpenAVI: Boolean;
    // Media File
    {$ifdef IEINCLUDEDIRECTSHOW}
    function OpenMediaFile(const FileName: WideString): Integer;
    procedure CloseMediaFile;
    procedure LoadFromMediaFile(FrameIndex: Integer);
    function IsOpenMediaFile: Boolean;
    {$endif}
    // WBMP
    function LoadFromFileWBMP(const FileName: WideString): Boolean;
    function LoadFromStreamWBMP(Stream: TStream): Boolean;
    procedure SaveToFileWBMP(const FileName: WideString);
    procedure SaveToStreamWBMP(Stream: TStream);
    // PostScript (PS)
    procedure CreatePSFile(const FileName: WideString);
    procedure SaveToPS;
    procedure ClosePSFile;
    procedure SaveToStreamPS(Stream: TStream);
    procedure SaveToFilePS(const FileName: WideString);
    // PDF
    {$ifdef IEINCLUDEPDFWRITING}
    procedure CreatePDFFile(const FileName: WideString);
    procedure SaveToPDF;
    procedure ClosePDFFile;
    procedure SaveToStreamPDF(Stream: TStream);
    procedure SaveToFilePDF(const FileName: WideString);
    {$endif}
    // RAW
    {$ifdef IEINCLUDERAWFORMATS}
    function LoadFromStreamRAW(Stream: TStream): Boolean;
    function LoadFromFileRAW(const FileName: WideString): Boolean;
    function LoadJpegFromFileCRW(const FileName: WideString): Boolean;
    {$endif}
    // Real RAW
    function LoadFromStreamBMPRAW(Stream: TStream): Boolean;
    function LoadFromFileBMPRAW(const FileName: WideString): Boolean;
    procedure SaveToStreamBMPRAW(Stream: TStream);
    procedure SaveToFileBMPRAW(const FileName: WideString);
    // PSD
    {$ifdef IEINCLUDEPSD}
    function LoadFromStreamPSD(Stream: TStream): Boolean;
    function LoadFromFilePSD(const FileName: WideString): Boolean;
    procedure SaveToStreamPSD(Stream: TStream);
    procedure SaveToFilePSD(const FileName: WideString);
    {$endif}
    // HDP
    {$ifdef IEINCLUDEWIC}
    function LoadFromStreamHDP(Stream: TStream): Boolean;
    function LoadFromFileHDP(const FileName: WideString): Boolean;
    procedure SaveToStreamHDP(Stream: TStream);
    procedure SaveToFileHDP(const FileName: WideString);
    {$endif}
    // GENERAL
    function LoadFromFile(const FileName: WideString; ImageFormat: TIOFileType = -1): Boolean; overload; dynamic;
    function LoadFromFile(const FileName: WideString; bCheckUnknown: Boolean): Boolean; overload; dynamic; 
    function LoadFromFileAuto(const FileName: WideString): Boolean; dynamic;
    procedure SaveToFile(const FileName: WideString; ImageFormat: TIOFileType = -1); dynamic;
    procedure SaveToText(const FileName: WideString; ImageFormat: TIOFileType = -1; TextFormat: TIETextFormat = ietfASCIIArt); overload; dynamic;
    procedure SaveToText(Stream: TStream; ImageFormat: TIOFileType = -1; TextFormat: TIETextFormat = ietfASCIIArt); overload; dynamic;
    procedure LoadFromText(const FileName: WideString; TextFormat: TIETextFormat = ietfBase64); overload;
    procedure LoadFromText(Stream: TStream; TextFormat: TIETextFormat = ietfBase64); overload;
    function LoadFromFileFormat(const FileName: WideString; FileFormat: TIOFileType): Boolean; dynamic;
    procedure LoadFromBuffer(Buffer: Pointer; BufferSize: Integer; Format: TIOFileType = ioUnknown); dynamic;
    procedure ParamsFromBuffer(Buffer: Pointer; BufferSize: Integer; Format: TIOFileType = ioUnknown); dynamic;

{!!
<FS>TImageEnIO.Aborting

<FM>Declaration<FC>
property Aborting: Boolean;

<FM>Description<FN>
During an operation such as loading or saving, setting Aborting to true will halt the current process.

If set to True while loading, the loaded image will be truncated (i.e. only a portion of the image is loaded).
If set to True while saving, the file will be closed and truncated (i.e. it will be unreadable and should be deleted).

<FC>Aborting<FN> can also be read after loading or saving to determine if an abort has occured.

<FM>Load Error Example<FC>
MyImageEnMView.LoadFromFileGIF('C:\MyGif.gif');
if MyImageEnMView.Aborting then
  ShowMessage('GIF Load Error!');

Which is the same as:
if MyImageEnMView.LoadFromFileGIF('C:\MyGif.gif') = False then
  ShowMessage('GIF Load Error!');

<FM>Abort Loading Example<FC>
// Begin to load image
procedure TForm1.btnLoadClick(Sender: TObject);
begin
  ImageEnView1.IO.LoadFromFile('C:\xyx.bmp');
end;

// Progress of load
procedure TForm1.ImageEnIO1Progress(Sender: TObject; per: Integer);
begin
  ProgressBar1.Position := per;
  Application.ProcessMessages;   // <- important!
end;

// STOP! button
procedure TForm1.btnStopClick(Sender: TObject);
begin
  // Cancel loading
  ImageEnView1.IO.Aborting := True;
end;
!!}
    property Aborting: boolean read fAborting write SetAborting;

    procedure ParamsFromFile(const FileName: WideString); dynamic;
    procedure ParamsFromFileFormat(const FileName: WideString; format: TIOFileType); dynamic;
    procedure ParamsFromStream(Stream: TStream); dynamic;
    procedure ParamsFromStreamFormat(Stream: TStream; format: TIOFileType); dynamic;
    function LoadFromStream(Stream: TStream): Boolean; dynamic;
    function LoadFromStreamFormat(Stream: TStream; FileFormat: TIOFileType): Boolean; dynamic;
    {$IFDEF VIDEO_THUMBNAILS}
    function LoadThumbnailFromExplorer(const FileName : WideString; iDesiredWidth : Integer = 120; iDesiredHeight : Integer = 120) : Boolean;
    {$ENDIF}
    procedure SaveToStream(Stream: TStream; FileType: TIOFileType); dynamic;
    function Seek(Destination: TIEIOSeekDestination; FileName: WideString = ''): Integer;
    {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
    function ExecuteOpenDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                               FilterIndex: Integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                               const Filter : WideString = ''; DefaultFilter : TIOFileType = -1; LimitToFileType : TIOFileType = -1) : WideString; overload;
    function ExecuteOpenDialog(const Title : WideString; DefaultFilter : TIOFileType; LimitToFileType : TIOFileType = -1; AlwaysAnimate : boolean = False) : WideString; overload;
    function ExecuteSaveDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                               FilterIndex: Integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                               const Filter : WideString = ''; DefaultFilter : TIOFileType = -1; LimitToFileType : TIOFileType = -1) : WideString; overload;
    function ExecuteSaveDialog(const Title : WideString; DefaultFilter : TIOFileType; LimitToFileType : TIOFileType = -1; AlwaysAnimate : boolean = False) : WideString; overload;

    {$ENDIF} // IEINCLUDEOPENSAVEDIALOGS
    procedure CaptureFromScreen(Source: TIECSSource = iecsScreen; MouseCursor: TCursor = -1);
    function LoadFromURL(const URL: WideString): Boolean;
    // TWAIN SCANNERS
    {$IFDEF IEINCLUDEIEXACQUIRE}
    function Acquire(bResetParams : Boolean = False) : boolean;
    function SelectAcquireSource(Apis : TIEAcquireApis = [ieaTwain, ieaWIA]): boolean;
    function SetAcquireSource(Api: TIEAcquireApi; Location : Variant) : boolean;
    property SelectedAcquireSource : TIEAcquireSource read GetSelectedAcquireSource;

    function TwainAcquireOpen: boolean;
    procedure TwainAcquireClose;
    function AcquireOpen: boolean;
    procedure AcquireClose;

    // Internal use only
    procedure InitializeAcquireSource(bIncludeWIA : Boolean);
    {$ENDIF} // IEINCLUDEIEXACQUIRE
    // ASYNC WORKS
    property AsyncRunning: integer read GetAsyncRunning;

{!!
<FS>TImageEnIO.AsyncMode

<FM>Declaration<FC>
property AsyncMode: Boolean;

<FM>Description<FN>
Set to True to enable asynchronous input/output operations.

When asynchronous mode is enabled, each input/output method creates a new thread to executes the task then returns without waiting for the task to complete.

Note: Applicable only when TImageEnIO is attached to a <A TIEBitmap> object.

<FM>Example<FC>
// multithread saving
Var
  Bmp: TIEBitmap;
  Io: TImageEnIO;
Begin
  Bmp:= TIEBitmap.Create;
  Io := TImageEnIO.CreateFromBitmap(bmp);

  Io.LoadFromFile('C:\input.bmp');
  Io.AsyncMode := True;  // So it will create a thread for each input/output task
  Io.SaveToFile('D:\i1.jpg');  // thread 1
  Io.SaveToFile('D:\i2.jpg');  // thread 2
  Io.SaveToFile('D:\i3.jpg');  // thread 3

  Io.Free;  // Free method will wait for all tasks to end!
  Bmp.Free;
End;
!!}
    property AsyncMode: boolean read fAsyncMode write fAsyncMode;

    procedure WaitThreads(Aborts: Boolean = false);
    procedure SuspendThreads; {$ifdef IEHASTTHREADSTART} deprecated; {$endif}
    procedure ResumeThreads;  {$ifdef IEHASTTHREADSTART} deprecated; {$endif}

    // TWAIN/WIA scanners
    {$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnIO.AcquireParams

<FM>Declaration<FC>
property AcquireParams : <A TIEAcquireParams>;

<FM>Description<FN>
AcquireParams is a powerful interface that provides generic access to all image acquistion APIs, Twain, WIA and DCIM Retrieval. AcquireParams accesses <A TImageEnIO.TwainParams>, <A TImageEnIO.WIAParams> and <A TImageEnIO.DCIMParams>.

If you have called <A TImageEnIO.SelectAcquireSource> or <A TImageEnIO.Acquire> then you can access common parameters for the device without having to know the <L TIEAcquireApi>API</L>

See the <FC>TIEAcquireParams<FN> object for more details.
!!}
    property AcquireParams: TIEAcquireParams read fAcquireParams;



{!!
<FS>TImageEnIO.TwainParams

<FM>Declaration<FC>
property TwainParams: <A TIETwainParams>;

<FM>Description<FN>
Use the TwainParams property to control acquisition from Twain scanners and cameras. You can enable/disable the standard user interface, set pixel type (Grayscale, RGB...), DPI, etc.

Note: Use TwainParams only when you need access to Twain specific parameters and functionality (when <A TImageEnIO.SelectedAcquireSource>.Api is ieaTwain). For generic access to all image acquisitions sources (Twain, WIA, etc.) use <A TImageEnIO.AcquireParams> instead.

<FM>Example<FC>
// Acquire a black/white (1bit) image from the default Twain device
ImageEnView1.IO.TwainParams.PixelType.CurrentValue := 0;
if ImageEnView1.IO.SetSource(ieaTwain, Default_Device) then
ImageEnView1.IO.Acquire;

<FM>See Also<FN>
- <A TIETwainParams>
- <A TImageEnIO.AcquireParams>
- <A TImageEnIO.SelectedAcquireSource>
!!}
    property TwainParams: TIETwainParams read fTwainParams;


{!!
<FS>TImageEnIO.DCIMParams

<FM>Declaration<FC>
property DCIMParams: <A TIEDcimAcquire>;

<FM>Description<FN>
Use the DCIMParams property to handle retrieval of images from camera cards and connected camera devices (which appear as USB drives).

Note: Use DCIMParams only when you need access to DCIM specific functionality (when <A TImageEnIO.SelectedAcquireSource>.Api is ieaDCIM). For generic access to all image acquisitions sources (Twain, WIA, etc.) use <A TImageEnIO.AcquireParams> instead.
!!}
    property DCIMParams: TIEDcimAcquire read fDCIMParams;


    {$ENDIF}
    {$IFDEF IEINCLUDEIEXACQUIRE}
    property WIAParams: TIEWia read GetWIAParams;
    {$ENDIF}
    // Video capture
    {$IFDEF IEINCLUDEDIRECTSHOW}
    property DShowParams: TIEDirectShow read GetDShowParams;
    {$ENDIF}
    {$IFDEF IEINCLUDEMEDIAFOUNDATION}
    property MediaFoundationSourceReader: TIEMediaFoundationSourceReader read GetMediaFoundationSourceReader;
    {$ENDIF}
    // PRINTING
    procedure PrintImagePos(PrtCanvas: TCanvas; x, y: double; Width, Height: double; GammaCorrection: double = 1);
    procedure PrintImagePosEx(PrtCanvas: TCanvas; dpix, dpiy: integer; x, y: double; Width, Height: double; GammaCorrection: double);
    procedure PrintImage(PrtCanvas: TCanvas = nil; MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1; VerticalPos: TIEVerticalPos = ievpCENTER; HorizontalPos: TIEHorizontalPos = iehpCENTER; Size: TIESize = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0; GammaCorrection: double = 1);
    procedure PreviewPrintImage(DestBitmap: TBitmap; MaxBitmapWidth: integer; MaxBitmapHeight: integer; PrinterObj: TPrinter = nil; MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1; VerticalPos: TIEVerticalPos = ievpCENTER; HorizontalPos: TIEHorizontalPos = iehpCENTER; Size: TIESize = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0; GammaCorrection: double = 1);
    {$IFDEF IEINCLUDEPRINTDIALOGS}
    function DoPrintPreviewDialog(DialogType: TIEDialogType = iedtDialog; const TaskName: WideString = ''; PrintAnnotations: Boolean = False; const Caption: WideString=''): boolean;

{!!
<FS>TImageEnIO.PrintPreviewParams

<FM>Declaration<FC>
property PrintPreviewParams: <A TIOPrintPreviewParams>;

<FM>Description<FN>                    
This property provides access to the parameters of the Print Preview dialog.

Note: All measure units are specified by <A TImageEnIO.DialogsMeasureUnit> property.
!!}
    property PrintPreviewParams: TIOPrintPreviewParams read fPrintPreviewParams;

    {$ENDIF}

{!!
<FS>TImageEnIO.PrintingFilterOnSubsampling

<FM>Declaration<FC>
property PrintingFilterOnSubsampling: <A TResampleFilter>;

<FM>Description<FN>
Specifies a filter when an image needs to be resampled for printing.

Filtering enhances the image quality, but slows down processing.
!!}
    property PrintingFilterOnSubsampling: TResampleFilter read fPrintingFilterOnSubsampling write fPrintingFilterOnSubsampling;

    // proxy settings (used by LoadFromURL and LoadFromFile with http:// prefix

{!!
<FS>TImageEnIO.ProxyAddress

<FM>Declaration<FC>
property ProxyAddress: WideString;

<FM>Description<FN>
Specifies the proxy address and port when using <A TImageEnIO.LoadFromURL> method. The syntax is: 'domain:port'.

<FM>Example<FC>
ImageEnIO.ProxyAddress := '10.2.7.2:8080';
ImageEnIO.LoadFromURL('http://www.imageen.com/image.jpg');
!!}
    property ProxyAddress: WideString read fProxyAddress write fProxyAddress;

{!!
<FS>TImageEnIO.ProxyUser

<FM>Declaration<FC>
property ProxyUser: WideString;

<FM>Description<FN>
Specifies the proxy user id when using <A TImageEnIO.LoadFromURL> method.

<FM>Example<FC>
ImageEnIO.ProxyAddress := '10.2.7.2:8080';
ImageEnIO.ProxyUser := 'testuser';
ImageEnIO.ProxyPassword := 'testpassword';
ImageEnIO.LoadFromURL('http://www.imageen.com/image.jpg');
!!}
    property ProxyUser: WideString read fProxyUser write fProxyUser;

{!!
<FS>TImageEnIO.ProxyPassword

<FM>Declaration<FC>
property ProxyPassword: WideString;

<FM>Description<FN>
Specifies the proxy password when using <A TImageEnIO.LoadFromURL> method.

<FM>Example<FC>
ImageEnIO.ProxyAddress := '10.2.7.2:8080';
ImageEnIO.ProxyUser := 'testuser';
ImageEnIO.ProxyPassword := 'testpassword';
ImageEnIO.LoadFromURL('http://www.imageen.com/image.jpg');
!!}
    property ProxyPassword: WideString read fProxyPassword write fProxyPassword;

    //
    property ResetPrinter: boolean read fResetPrinter write fResetPrinter;
    property PrintLogFile: String read GetPrintLogFile write SetPrintLogFile;
  published
    property AttachedImageEn: TIEView read fImageEnView write SetAttachedImageEn;
    property Background: TColor read GetReBackground write SetReBackground default clBlack;


{!!
<FS>TImageEnIO.OnProgress

<FM>Declaration<FC>
property OnProgress: <A TIEProgressEvent>;

<FM>Description<FN>
OnProgress event is called during input/output operations. It is commonly used to show task progress.

If you are using it to update a progress bar then you can reset it in the <A TImageEnIO.OnFinishWork> event.
         
<FM>Example<FC>
procedure TForm1.ImageEnIO1Progress(Sender: TObject; per: Integer);
begin
  // Show load/save progress
  ProgressBar1.Visible := True;
  ProgressBar1.Position := per;
  Application.ProcessMessages;   // <- important!
end;

procedure TForm1.ImageEnIO1FinishWork(Sender: TObject);
begin
  // Hide the progress bar
  ProgressBar1.Visible := False;
end;

!!}
    property OnProgress: TIEProgressEvent read fOnProgress write fOnProgress;

    property PreviewsParams: TIOPreviewsParams read GetIOPreviewParams write SetIOPreviewParams default [];

{!!
<FS>TImageEnIO.StreamHeaders

<FM>Declaration<FC>
property StreamHeaders: boolean;

<FM>Description<FN>
If True, each SaveToStreamXXX method adds an additional special header as needed for multi-image streams.
When a special header is added, the images saved with SaveToStream*** aren't compatible with LoadFromFile*** methods.

<FM>Example<FC>
// Save ImageEnView1 and ImageEnView2 images in the file, images.dat
// Note: images.dat won't be loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('bmpimages.dat', fmCreate);
  ImageEnView1.IO.StreamHeaders := True;
  ImageEnView1.IO.SaveToStreamJPEG(fs);
  ImageEnView2.IO.StreamHeaders := True;
  ImageEnView2.IO.SaveToStreamJPEG(fs);
  fs.free;
End;

// Save a single image in image.jpg
// image.jpg is loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('image.jpg');
  ImageEnView1.IO.StreamHeaders := False;
  ImageEnView1.IO.SaveToFileJPEG(fs);
End;
!!}
    property StreamHeaders: boolean read fStreamHeaders write fStreamHeaders default false;

    property PreviewFont: TFont read fPreviewFont write SetPreviewFont;
    property PreviewFontEnabled: Boolean read fPreviewFontEnabled write SetPreviewFontEnabled default false;
    property AttachedTImage: TImage read fTImage write SetTImage;

{!!
<FS>TImageEnIO.OnIOPreview

<FM>Declaration<FC>
property OnIOPreview: <A TIEIOPreviewEvent>;

<FM>Description<FN>
OnIOPreview is called before the preview form is displayed (i.e. when calling the <A TImageEnIO.DoPreviews> method).

<FM>Example<FC>
procedure TForm1.ImageEnIO1IOPreview(Sender: TObject; PreviewForm: TForm);
begin
  with (PreviewForm as TfIOPreviews) do
    ProgressBar1.Visible := False;
end;
!!}
    property OnIOPreview: TIEIOPreviewEvent read fOnIOPreview write fOnIOPreview;

{!!
<FS>TImageEnIO.DialogsMeasureUnit

<FM>Declaration<FC>
property DialogsMeasureUnit: <A TIEDialogsMeasureUnit>

<FM>Description<FN>
Specifies the measurement unit used in the <L TImageEnIO.DoPrintPreviewDialog>Print Preview dialog</L>

<FM>Example<FC>
ImageEnView1.IO.DialogsMeasureUnit := ieduCm;
ImageEnView1.IO.DoPrintPreviewDialog(iedtDialog);
!!}
    property DialogsMeasureUnit: TIEDialogsMeasureUnit read fDialogsMeasureUnit write fDialogsMeasureUnit default ieduInches;

{!!
<FS>TImageEnIO.AutoAdjustDPI

<FM>Declaration<FC>
property AutoAdjustDPI: Boolean;

<FM>Description<FN>
When set to True and the current (loaded or scanned) image has a horizontal DPI that does not not equal its vertical DPI, ImageEn will resize the image to make them equal.

Default: False
!!}
    property AutoAdjustDPI: boolean read fAutoAdjustDPI write fAutoAdjustDPI default False;

{!!
<FS>TImageEnIO.FilteredAdjustDPI

<FM>Declaration<FC>
property FilteredAdjustDPI: Boolean;

<FM>Description<FN>
If set to True, ImageEn applies a resampling filter to enhance quality when adjusting an image's DPI.

Note: Only valid when <A TImageEnIO.AutoAdjustDPI> is True. It can slow down the loading process.
!!}
    property FilteredAdjustDPI: boolean read fFilteredAdjustDPI write fFilteredAdjustDPI default false;

{!!
<FS>TImageEnIO.OnFinishWork

TImageEnMView component

<FM>Declaration<FC>
property OnFinishWork: TNotifyEvent;

<FM>Description<FN>
OnFinishWork occurs when an input/output task ends. It is useful for resetting a progress bar or to know when a thread ends in a asynchronous mode.
            
<FM>Example<FC>
procedure TForm1.ImageEnIO1Progress(Sender: TObject; per: Integer);
begin
  // Show load/save progress
  ProgressBar1.Visible := True;
  ProgressBar1.Position := per;
  Application.ProcessMessages;   // <- important!
end;

procedure TForm1.ImageEnIO1FinishWork(Sender: TObject);
begin
  // Hide the progress bar
  ProgressBar1.Visible := False;
end;
!!}
    property OnFinishWork: TNotifyEvent read fOnFinishWork write fOnFinishWork;

{!!
<FS>TImageEnIO.OnAcquireBitmap

<FM>Declaration<FC>
property OnAcquireBitmap: <A TIEAcquireBitmapEvent>;

<FM>Description<FN>
Occurs whenever a new bitmap is acquired during a multi-page acquisition.
!!}
    property OnAcquireBitmap: TIEAcquireBitmapEvent read fOnAcquireBitmap write fOnAcquireBitmap;


{!!
<FS>TImageEnIO.OnAcquireClose

<FM>Declaration<FC>
property OnAcquireClose: TNotifyEvent;

<FM>Description<FN>
Occurs when the user closes the acquire dialog, which was opened using <A TImageEnIO.TwainAcquireOpen>.
!!}
    property OnAcquireClose: TNotifyEvent read fOnAcquireClose write fOnAcquireClose;


{!!
<FS>TImageEnIO.SimplifiedParamsDialogs

<FM>Declaration<FC>
property SimplifiedParamsDialogs: Boolean;

<FM>Description<FN>
When set to true (the default), the File Format Parameters dialog (shown when clicking the "Advanced" button in ImageEn's Save dialog) will show a simplified set of parameters. Set to false to display a fuller set of parameters.

<FM>Example<FC>
The Parameters dialog for TIFF, using SimplifiedParamsDialogs = True:
<IMG help_images\68.bmp>

The Parameters dialog for TIFF, using SimplifiedParamsDialogs = False:
<IMG help_images\69.bmp>
!!}
    property SimplifiedParamsDialogs: boolean read fSimplifiedParamsDialogs write fSimplifiedParamsDialogs default true;

{!!
<FS>TImageEnIO.OnDoPreviews

<FM>Declaration<FC>
property OnDoPreviews: <A TIEDoPreviewsEvent>;

<FM>Description<FN>
OnDoPreviews occurs just before the Format Parameters dialog (shown when clicking the "Advanced" button in ImageEn's Save dialog) shows.

You can disable the dialog box by setting Handled to true.
!!}
    property OnDoPreviews: TIEDoPreviewsEvent read fOnDoPreviews write fOnDoPreviews;

    property NativePixelFormat: boolean read GetNativePixelFormat write SetNativePixelFormat default false;

    property ImageEnVersion: String read GetImageEnVersion write SetImageEnVersion stored false;
  end;

// TImageEnIO
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////


procedure IEInitialize_imageenio;
procedure IEFinalize_imageenio;


function IsKnownFormat(const FileName: WideString; bIncludeVideoFiles : Boolean = False): boolean;
function IsKnownSaveFormat(const FileName: WideString): boolean;
function FindFileFormat(const FileName: WideString; VerifyExtension: boolean = false): TIOFileType;
function FindStreamFormat(Stream: TStream): TIOFileType;
function DeleteGIFIm(const FileName: WideString; idx: integer): integer;
procedure DeleteDCXIm(const FileName: WideString; idx: integer);
function DeleteTIFFIm(const FileName: WideString; idx: integer): integer;
function DeleteTIFFImGroup(const FileName: WideString; Indexes: array of integer): integer;
function EnumGIFIm(const FileName: WideString): integer;
function EnumTIFFIm(const FileName: WideString): integer;
function EnumTIFFStream(Stream: TStream): integer;
function EnumICOIm(const FileName: WideString): integer;
function EnumDCXIm(const FileName: WideString): integer;
function CheckAniGIF(const FileName: WideString): boolean;
procedure IEWriteICOImages(const fileName: WideString; images: array of TObject);
function JpegLosslessTransform(const SourceFile, DestFile: WideString; Transform: TIEJpegTransform; GrayScale: boolean; CopyMarkers: TIEJpegCopyMarkers; CutRect: TRect; UpdateEXIF: Boolean = False): boolean; overload;
function JpegLosslessTransform(const SourceFile, DestFile: WideString; Transform: TIEJpegTransform): boolean; overload;
function JpegLosslessTransform2(const FileName: WideString; Transform: TIEJpegTransform; GrayScale: boolean; CopyMarkers: TIEJpegCopyMarkers; CutRect: TRect; UpdateEXIF: Boolean = False): boolean; overload;
function JpegLosslessTransform2(const FileName: WideString; Transform: TIEJpegTransform): boolean; overload;
function JpegLosslessTransformStream(SourceStream, DestStream: TStream; Transform: TIEJpegTransform; GrayScale: boolean; CopyMarkers: TIEJpegCopyMarkers; CutRect: TRect; UpdateEXIF: Boolean = False): boolean;
procedure ExtractTIFFImageStream(SourceStream, OutStream: TStream; idx: integer);
procedure ExtractTIFFImageFile(const SourceFileName, OutFileName: WideString; idx: integer);
procedure InsertTIFFImageStream(SourceStream, InsertingStream, OutStream: TStream; idx: integer);
procedure InsertTIFFImageFile(const SourceFileName, InsertingFileName, OutFileName: WideString; idx: integer);
function IEAdjustDPI(bmp: TIEBitmap; Params: TIOParamsVals; FilteredAdjustDPI: boolean): TIEBitmap;
function IEGetFileFramesCount(const FileName: WideString): Integer;
function IEFindNumberWithKnownFormat( const Directory: WideString ): integer;
function IEAVISelectCodec(): AnsiString;

function IEAVIGetCodecs: TStringList;
function IEAVIGetCodecsDescription: TStringList;

procedure IEPrintLogWrite(const ss: String);

function IEGetFromURL(const URL: WideString; OutStream: TStream; const ProxyAddress: WideString; const ProxyUser: WideString; const ProxyPassword: WideString; OnProgress: TIEProgressEvent; Sender: TObject; Aborting: pboolean; var FileExt: String): Boolean;


type

{!!
<FS>TIEReadImageStream

<FM>Declaration<FC>
TIEReadImageStream = procedure(Stream: TStream; Bitmap: <A TIEBitmap>; var IOParams: <A TIOParamsVals>; var Progress: <A TProgressRec>; Preview: boolean);

<FM>Description<FN>
The procedure that will load the image in <FC>Stream<FN> into <FC>Bitmap<FN> and fill <FC>IOParams<FN> with meta data.

Use <FC>Progress<FN> to return progress information (e.g. for 35% progress Progress.val := 35 and Progress.tot := 100) and notify of load failure (Progress.Aborting^ := True).
When <FC>Preview<FN> is true, ImageEn is only seeking to fill <FC>IOParams<FN>, not load the image (i.e. you can skip <FC>Bitmap<FN> as it will be ignored.
You will need to reset the stream position.
!!}
TIEReadImageStream = procedure(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);

{!!
<FS>TIEWriteImageStream

<FM>Declaration<FC>
TIEWriteImageStream = procedure(Stream: TStream; Bitmap: <A TIEBitmap>; var IOParams: <A TIOParamsVals>; var Progress: <A TProgressRec>);

<FM>Description<FN>
The procedure that will save the image in <FC>Bitmap<FN> to a <FC>Stream<FN>.

Use <FC>Progress<FN> to return progress information (e.g. for 35% progress Progress.val := 35 and Progress.tot := 100) and notify of save failure (Progress.Aborting^ := True).
!!}
TIEWriteImageStream = procedure(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);

{!!
<FS>TIETryImageStream

<FM>Declaration<FC>
TIETryImageStream = function(Stream: TStream; TryingFormat: <A TIOFileType>): boolean;

<FM>Description<FN>
Return True if <FC>Stream<FN> matches our custom file type.
!!}
TIETryImageStream = function(Stream: TStream; TryingFormat: TIOFileType): boolean;


{!!
<FS>TIEFileFormatInfo

<FM>Declarations<FC>
TIEFileFormatInfo=class
    FileType : <A TIOFileType>;
    FullName : string;
    Extensions :  string;
    SuitableExtension : string;
    InternalFormat : boolean;
    DialogPage : <A TPreviewParams>;
    ReadFunction : <A TIEReadImageStream>;
    WriteFunction : <A TIEWriteImageStream>;
    TryFunction : <A TIETryImageStream>;
end;

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>FileType</C> <C>The file type class, e.g. ioJPEG</C> </R>
<R> <C>FullName</C> <C>A description of the file type, e.g. 'JPEG Bitmap'</C> </R>
<R> <C>Extensions</C> <C>All extensions of this format WITHOUT the period, e.g. 'jpg;jpeg;jpe'</C> </R>
<R> <C>SuitableExtension</C> <C>A single extension that is suitable for this format WITHOUT the period, e.g. 'jpeg'</C> </R>
<R> <C>InternalFormat</C> <C>True for native ImageEn formats, false for custom formats</C> </R>
<R> <C>DialogPage</C> <C>The page that is used to display properties for this type in <A TImageEnIO.DoPreviews></C> </R>
<R> <C>ReadFunction</C> <C>The function used for reading this file type (nil if it cannot be read)</C> </R>
<R> <C>WriteFunction</C> <C>The function used for saving this file type (nil if it is read-only)</C> </R>
<R> <C>TryFunction</C> <C>The function used to determine if a file is of this type (used only when ImageEn is checking for the image type without considering its file extension)</C> </R>
</TABLE>

!!}
  TIEFileFormatInfo = class
    FileType: TIOFileType;
    FullName: string;       // ex 'JPEG Bitmap'
    Extensions: string;     // extensions without '.' ex 'jpg' (ex 'jpg;jpeg;jpe')
    SuitableExtension : string;
    InternalFormat: Boolean;
    DialogPage: TPreviewParams;
    ReadFunction: TIEReadImageStream;
    WriteFunction: TIEWriteImageStream;
    TryFunction: TIETryImageStream;

    constructor Create(); overload;
    constructor Create(FileType: TIOFileType; FullName: string; Extensions: string; SuitableExtension : string; InternalFormat: Boolean; DialogPage: TPreviewParams; ReadFunction: TIEReadImageStream; WriteFunction: TIEWriteImageStream; TryFunction: TIETryImageStream); overload;
  end;


// custom file formats registration functions
function IEFileFormatGetInfo(FileType: TIOFileType): TIEFileFormatInfo;
function IEFileFormatGetInfo2(Extension: string): TIEFileFormatInfo;
function IEFileFormatGetExt(FileType: TIOFileType; idx: integer): string;
function IEFileFormatGetExtCount(FileType: TIOFileType): integer;
procedure IEFileFormatAdd(FileFormatInfo: TIEFileFormatInfo);
procedure IEFileFormatRemove(FileType: TIOFileType);

function GetAllSupportedFileExtensions(bLoadFormats, bSaveFormats : Boolean; bVideoFormats: Boolean = True) : string;
function GetFileExtensionsOfType(FileType: TIOFileType) : string;

function IEExtToFileFormat(ex: String): TIOFileType;
function IEFileIsOfFormat(const sFilename : string; aFormat : TIOFileType) : Boolean;
function IEIsInternalFormat(ex: String): Boolean;

// external dll plugins
function IEAutoLoadIOPlugins : Integer;
function IEAddExtIOPlugin(const FileName : string) : Integer;
function IEIsExtIOPluginLoaded(const FileName : string) : boolean;

// others
procedure IEUpdateGIFStatus;

function IECalcJpegFileQuality(const FileName: WideString): integer;

function IECalcJpegStreamQuality(Stream: TStream): integer;
procedure IEOptimizeGIF(const InputFile, OutputFile: WideString);

function IEPaperSizeToStr(const aSize : TIOPDFPaperSize) : string;
function IEStrToPaperSize(const sSize : string; aDefault  : TIOPDFPaperSize = iepUnknown) : TIOPDFPaperSize;
       
function TidyIPTCStr(const Value: string): string;

type


{!!
<FS>TIEConvertColorFunction

<FM>Declaration<FC>
TIEConvertColorFunction = procedure(InputScanline: pointer; InputColorSpace: <A TIEColorSpace>; OutputScanline: pointer; OutputColorSpace: <A TIEColorSpace>; ImageWidth: integer; IOParams: <A TIOParamsVals>);

<FM>Description<FN>
Specifies the function used to convert from a color space to another.
!!}
  TIEConvertColorFunction = procedure(InputScanline: pointer; InputColorSpace: TIEColorSpace; OutputScanline: pointer; OutputColorSpace: TIEColorSpace; ImageWidth: integer; IOParams: TIOParamsVals);



var

  iegPrintLogFileName: String;
  iegPrintLogFile: textfile;



function IECMYK2RGB(cmyk: TCMYK): TRGB;
function IECMYK2RGBROW(inrow: PCMYK; outrow: PRGB; width: Integer; alphaRow: pinteger = nil): TRGB; overload;
function IECMYK2RGBROW(inrow: PCMYK; outrow: PRGB; width: Integer; alphaRow: pinteger; colorProfile: TIEICC): TRGB; overload;
function IERGB2CMYK(const rgb: TRGB): TCMYK;
procedure IEDefaultConvertColorFunction(InputScanline: pointer; InputColorSpace: TIEColorSpace; OutputScanline: pointer; OutputColorSpace: TIEColorSpace; ImageWidth: integer; IOParams: TIOParamsVals);


const
  // Property strings for TIOPrintPreviewParams.GetProperty/SetProperty   
  PPP_MARGINTOP            = 'MARGINTOP';
  PPP_MARGINLEFT           = 'MARGINLEFT';
  PPP_MARGINRIGHT          = 'MARGINRIGHT';
  PPP_MARGINBOTTOM         = 'MARGINBOTTOM';
  PPP_POSITION             = 'POSITION';
  PPP_SIZE                 = 'SIZE';
  PPP_WIDTH                = 'WIDTH';
  PPP_HEIGHT               = 'HEIGHT';
  PPP_GAMMA                = 'GAMMA';
  PPP_PRINTTHUMBNAILS      = 'PRINTTHUMBNAILS';
  PPP_THUMBNAILCOLUMNS     = 'THUMBNAILCOLUMNS';
  PPP_THUMBNAILROWS        = 'THUMBNAILROWS';
  PPP_THUMBNAILSPACING     = 'THUMBNAILSPACING';
  PPP_THUMBNAILSTYLE       = 'THUMBNAILSTYLE';
  PPP_THUMBNAILSHOWTEXT    = 'THUMBNAILSHOWTEXT';
  PPP_DLGWIDTH             = 'DLGWIDTH';
  PPP_DLGHEIGHT            = 'DLGHEIGHT';

  PPP_Property_Count = 17;
  PPP_Property_List : array[0 .. PPP_Property_Count - 1] of string =
    ( PPP_MARGINTOP            ,
      PPP_MARGINLEFT           ,
      PPP_MARGINRIGHT          ,
      PPP_MARGINBOTTOM         ,
      PPP_POSITION             ,
      PPP_SIZE                 ,
      PPP_WIDTH                ,
      PPP_HEIGHT               ,
      PPP_GAMMA                ,
      PPP_PRINTTHUMBNAILS      ,
      PPP_THUMBNAILCOLUMNS     ,
      PPP_THUMBNAILROWS        ,
      PPP_THUMBNAILSPACING     ,
      PPP_THUMBNAILSTYLE       ,
      PPP_THUMBNAILSHOWTEXT    ,
      PPP_DLGWIDTH             ,
      PPP_DLGHEIGHT);

implementation

uses
  {$ifdef IEUSEVCLZLIB}zlib, {$else}iezlib, {$endif}
  {$IFDEF VIDEO_THUMBNAILS}
    iexShellThumbnails,
  {$ENDIF}
  giffilter, PCXFilter, imscan, tiffilt, jpegfilt, BMPFilt, IOPreviews,
  pngfilt, pngfiltw, neurquant, ietgafil, IEMIO, IEMView, IEOpenSaveDlg, ieprnform1, ieprnform2, IEVfw, iej2000, iepsd, iewic, giflzw, tiflzw, ielcms,
  ieraw, imageenview, ievect, iedicom, iesettings, iemiscplugins;

{$R-}

// Standard Paper Sizes to Adobe point conversion
// Used by TIOParamsVals.PS_PaperSize, TIOParamsVals.PDF_PaperSize, IEStrToPaperSize() and IEPaperSizeToStr()
type
  TIOPDFPaperSizeRec = record
    Size   : TIOPDFPaperSize;
    Name   : String;     // Display name for the paper size
    Width  : Integer;    // Width of the page in Adobe PDF points (1 point = 1/72 of inch).
    Height : Integer;    // Hidth of the page in Adobe PDF points (1 point = 1/72 of inch).
  end;

const
  IOPDFPaperSizes : array[0 .. 11] of TIOPDFPaperSizeRec = (
    (Size: iepA0;      Name: 'A0';         Width: 2380; Height: 3368),
    (Size: iepA1;      Name: 'A1';         Width: 1684; Height: 2380),
    (Size: iepA2;      Name: 'A2';         Width: 1190; Height: 1684),
    (Size: iepA3;      Name: 'A3';         Width: 842;  Height: 1190),
    (Size: iepA4;      Name: 'A4';         Width: 595;  Height: 842),
    (Size: iepA5;      Name: 'A5';         Width: 421;  Height: 595),
    (Size: iepA6;      Name: 'A6';         Width: 297;  Height: 421),
    (Size: iepB5;      Name: 'B5';         Width: 501;  Height: 709),
    (Size: iepLetter;  Name: 'US Letter';  Width: 612;  Height: 792),
    (Size: iepLegal;   Name: 'US Legal';   Width: 612;  Height: 1008),
    (Size: iepLedger;  Name: 'US Ledger';  Width: 1224; Height: 792),
    (Size: iepTabloid; Name: 'US Tabloid'; Width: 792;  Height: 1224)
    );




//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type
  TIEIOMethodTypes = (ieLoadSaveFile, ieLoadSaveFileIF, ieLoadSaveStream, ieLoadSaveFileRetInt, ieLoadSaveFileRetBool, ieLoadSaveFileIFRetBool,
                      ieLoadSaveStreamRetInt, ieLoadSaveStreamRetBool, ieLoadSaveFileFFRetBool, ieLoadSaveStreamFF,
                      ieLoadSaveStreamFTRetBool, ieCaptureFromScreen, ieLoadSaveIndexRetBool, ieImportMetaFileRetBool, ieLoadFromURLRetBool,
                      ieAcquire);

  TIEIOMethodType_LoadSaveFile = procedure(const FileName: WideString) of object;
  TIEIOMethodType_LoadSaveFileIF = procedure(const FileName: WideString; FileType: TIOFileType) of object;
  TIEIOMethodType_LoadSaveStream = procedure(Stream: TStream) of object;
  TIEIOMethodType_LoadSaveFileRetInt = function(const FileName: WideString): integer of object;
  TIEIOMethodType_LoadSaveFileRetBool = function(const FileName: WideString): Boolean of object;
  TIEIOMethodType_LoadSaveFileIFRetBool = function(const FileName: WideString; FileType: TIOFileType): Boolean of object;
  TIEIOMethodType_LoadSaveStreamRetInt = function(Stream: TStream): integer of object;
  TIEIOMethodType_LoadSaveStreamRetBool = function(Stream: TStream): Boolean of object;
  TIEIOMethodType_LoadSaveFileFFRetBool = function(const FileName: WideString; FileFormat: TIOFileType) : Boolean of object;
  TIEIOMethodType_LoadSaveStreamFF = procedure(Stream: TStream; FileFormat: TIOFileType) of object;
  TIEIOMethodType_LoadSaveStreamFTRetBool = function(Stream: TStream; FileFormat: TIOFileType) : Boolean of object;
  TIEIOMethodType_CaptureFromScreen = procedure(Source: TIECSSource; MouseCursor: TCursor) of object;
  TIEIOMethodType_LoadSaveIndexRetBool = function(Index: integer): Boolean of object;
  TIEIOMethodType_ImportMetaFileRetBool = function(const FileName: WideString; Width, Height: integer; WithAlpha: boolean): Boolean of object;
  TIEIOMethodType_LoadFromURLRetBool = function(const URL: WideString) : Boolean of object;
  TIEIOMethodType_Acquire = function(bResetParams : Boolean = False) : boolean of object;


  TIEIOThread = class(TThread)
  private
    fThreadList: TList;
    fMethodType: TIEIOMethodTypes;
    fMethod_LoadSaveFile: TIEIOMethodType_LoadSaveFile;
    fMethod_LoadSaveFileIF: TIEIOMethodType_LoadSaveFileIF;
    fMethod_LoadSaveStream: TIEIOMethodType_LoadSaveStream;
    fMethod_LoadSaveFileRetInt: TIEIOMethodType_LoadSaveFileRetInt;
    fMethod_LoadSaveFileRetBool : TIEIOMethodType_LoadSaveFileRetBool;
    fMethod_LoadSaveFileIFRetBool : TIEIOMethodType_LoadSaveFileIFRetBool;
    fMethod_LoadSaveStreamRetInt: TIEIOMethodType_LoadSaveStreamRetInt;
    fMethod_LoadSaveStreamRetBool: TIEIOMethodType_LoadSaveStreamRetBool;
    fMethod_LoadSaveFileFFRetBool: TIEIOMethodType_LoadSaveFileFFRetBool;
    fMethod_LoadSaveStreamFF: TIEIOMethodType_LoadSaveStreamFF;
    fMethod_LoadSaveStreamFTRetBool: TIEIOMethodType_LoadSaveStreamFTRetBool;
    fMethod_CaptureFromScreen: TIEIOMethodType_CaptureFromScreen;
    fMethod_LoadSaveIndexRetBool: TIEIOMethodType_LoadSaveIndexRetBool;
    fMethod_ImportMetaFileRetBool: TIEIOMethodType_ImportMetaFileRetBool;
    fMethod_LoadFromURLRetBool: TIEIOMethodType_LoadFromURLRetBool;
    fMethod_Acquire: TIEIOMethodType_Acquire;
    p_nf: WideString;
    p_ImageFormat: TIOFileType;
    p_stream: TStream;
    p_fileformat: TIOFileType;
    p_source: TIECSSource;
    p_index: integer;
    p_width, p_height: integer;
    p_withalpha: boolean;
    p_mouseCursor: TCursor;
    p_URL: WideString;
    p_ResetParams: Boolean;
    fThreadID: dword;
    fOwner: TImageEnIO;

    procedure Init;
  public
    procedure Execute; override;
    constructor CreateLoadSaveFile(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFile; const in_nf: WideString);
    constructor CreateLoadSaveFileIF(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileIF; const in_nf: WideString; ImageFormat: TIOFileType);
    constructor CreateLoadSaveStream(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStream; in_Stream: TStream);
    constructor CreateLoadSaveFileRetInt(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileRetInt; const in_nf: WideString);
    constructor CreateLoadSaveFileRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileRetBool; const in_nf: WideString);
    constructor CreateLoadSaveFileIFRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileIFRetBool; const in_nf: WideString; ImageFormat: TIOFileType);
    constructor CreateLoadSaveStreamRetInt(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStreamRetInt; in_Stream: TStream);
    constructor CreateLoadSaveStreamRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStreamRetBool; in_Stream: TStream);
    constructor CreateLoadSaveFileFFRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileFFRetBool; const in_nf: WideString; in_fileformat: TIOFileType);
    constructor CreateLoadSaveStreamFF(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStreamFF; in_Stream: TStream; in_fileformat: TIOFileType);
    constructor CreateLoadSaveStreamFTRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStreamFTRetBool; in_Stream: TStream; in_fileformat: TIOFileType);
    constructor CreateCaptureFromScreen(owner: TImageEnIO; InMethod: TIEIOMethodType_CaptureFromScreen; in_source: TIECSSource; in_mouseCursor: TCursor);
    constructor CreateLoadSaveIndexRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveIndexRetBool; in_index: integer);
    constructor CreateImportMetaFileRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_ImportMetaFileRetBool; const in_nf: WideString; in_width, in_height: integer; in_withalpha: boolean);
    constructor CreateAcquire(owner: TImageEnIO; InMethod: TIEIOMethodType_Acquire; bResetParams: Boolean);
    // dummy is necessary to C++Builder in order to compile
    constructor CreateLoadFromURLRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadFromURLRetBool; const in_URL: WideString; dummy: Double);
    property ThreadID: dword read fThreadID;
  end;


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////








{!!
<FS>TImageEnIO.Create

<FM>Declaration<FC>
constructor Create(Owner: TComponent);

<FM>Description<FN>
Creates a new instance of TImageEnIO. You can set Owner = nil to create an owner-less component.
!!}
constructor TImageEnIO.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  fIEBitmap := TIEBitmap.Create;
  fIEBitmapCreated := true; // we create fIEBitmap
  InitializeCriticalSection(fAsyncThreadsCS);
  fImageEnViewBitmapChangeHandle := nil;
  fOnIOPreview := nil;
  fStreamHeaders := false;
  fParams := TIOParamsVals.Create(self);
  {$IFDEF IEINCLUDEIEXACQUIRE}
  fAcquireParams := TIEAcquireParams.Create(Self);
  fTwainParams := TIETwainParams.Create(Self);
  fDCIMParams  := TIEDcimAcquire.Create(Self);
  {$ENDIF}
  fBitmap := nil;
  fImageEnView := nil;
  fTImage := nil;
  fBackground := clBlack;
  fOnIntProgress := DoIntProgress;
  fOnProgress := nil;
  fOnFinishWork := nil;
  fPreviewsParams := [];
  fPreviewFont := TFont.Create;
  fPreviewFontEnabled := False;
  fAborting := false;
  fDialogsMeasureUnit := ieduInches;
  fAutoAdjustDPI := False;
  fFilteredAdjustDPI := false;
  fAVI_avf := nil;
  fAVI_avs := nil;
  fAVI_avs1 := nil;
  fAVI_gf := nil;
  {$ifdef IEINCLUDEDIRECTSHOW}
  fOpenMediaFile := nil;
  {$endif}
  fAsyncThreads := TList.Create;
  fAsyncThreadsFinishEvent := Windows.CreateEvent(nil, false, false, nil);
  fAsyncMode := false;
  fPrintingFilterOnSubsampling := rfFastLinear;
  fgrec := nil;
  fOnAcquireBitmap := nil;
  fOnAcquireClose := nil;
  SimplifiedParamsDialogs := true;
  fOnDoPreviews := nil;
  fChangeBackground := false;
  ProxyAddress := '';
  ProxyUser := '';
  ProxyPassword := '';
  fDefaultDitherMethod := ieThreshold;
  fResetPrinter := true;
  fPS_handle := nil;
  fPS_stream := nil;
  fPDF_handle := nil;
  fPDF_stream := nil;
  {$IFDEF IEINCLUDEIEXACQUIRE}
  fWIA := nil;
  {$ENDIF}
  {$IFDEF IEINCLUDEDIRECTSHOW}
  fDShow := nil;
  {$ENDIF}
  {$IFDEF IEINCLUDEMEDIAFOUNDATION}
  fMediaFoundationSourceReader := nil;
  {$ENDIF}
  {$IFDEF IEINCLUDEPRINTDIALOGS}
  fPrintPreviewParams := TIOPrintPreviewParams.Create;
  {$ENDIF}
end;

{!!
<FS>TImageEnIO.CreateFromBitmap

<FM>Declaration<FC>
constructor CreateFromBitmap(Bitmap: TIEBitmap);
constructor CreateFromBitmap(Bitmap: TBitmap);

<FM>Description<FN>
Creates a new instance of TImageEnIO, assigning the property <A TImageEnIO.AttachedIEBitmap> or <A TImageEnIO.AttachedBitmap>.

<FM>Example<FC>
with TImageEnIO.CreateFromBitmap(myBitmap) do
begin
  LoadFromFile('C:\input.jpg');
  Free;
end;
!!}
constructor TImageEnIO.CreateFromBitmap(Bitmap: TIEBitmap);
begin
  Create(nil);
  AttachedIEBitmap := Bitmap;
end;

constructor TImageEnIO.CreateFromBitmap(Bitmap: TBitmap);
begin
  Create(nil);
  AttachedBitmap := Bitmap;
end;


{!!
<FS>TImageEnIO.ImageEnVersion

<FM>Declaration<FC>
property ImageEnVersion: String;

<FM>Description<FN>
Returns the ImageEn version as a string.

!!}
function TImageEnIO.GetImageEnVersion: String;
begin
  result := IEMAINVERSION;
end;

procedure TImageEnIO.SetImageEnVersion(Value: String);
begin
  // this is a read-only property, but it must be displayed in object inspector
end;

procedure TImageEnIO.SetNativePixelFormat(value: Boolean);
begin
  if assigned(fParams) then
    fParams.IsNativePixelFormat := value;
end;




{!!
<FS>TImageEnIO.NativePixelFormat

<FM>Declaration<FC>
property NativePixelFormat: Boolean;

<FM>Description<FN>
By default, ImageEn converts all paletted images to 24 bit (true color). Only black/white images are stored in the original format with 1 bit per pixel.
Setting NativePixelFormat to True disables the conversion and uses the image's native format.

Note: <A TImageEnView.LegacyBitmap> must be False to enable <FC>NativePixelFormat<FN>. Also, use of <FC>NativePixelFormat<FN> will prevent execution of some image processing operations.
       
<FM>Example<FN>
If you have a 16 bit gray scale TIFF and you want to work with 16 bit gray scale pixels, you must write this before loading the image:<FC>
ImageEnView1.LegacyBitmap := False;  // Do not use Tbitmap
ImageEnView1.IO.NativePixelFormat := True; // Use the original pixel format

<FN>Now you can read the pixels using:<FC>
word = ImageEnView1.IEBitmap.Pixels_ie16g[x,y];
!!}
function TImageEnIO.GetNativePixelFormat: Boolean;
begin
  if assigned(fParams) then
    result := fParams.IsNativePixelFormat
  else
    result := False;
end;

{!!
<FS>TImageEnIO.WaitThreads

<FM>Declaration<FC>
procedure WaitThreads(Aborts: Boolean = false);

<FM>Description<FN>
Call WaitThreads to wait until all threads complete. If <FC>Aborts<FN> is True, then all threads will be aborted.

<FM>Example<FC>
ImageEnIO.LoadFromFile('C:\input.jpg');
ImageEnIO.AsyncMode := True;             // Enable multithreading
ImageEnIO.SaveToFile('D:\output1.jpg');  // Save in thread 1
ImageEnIO.SaveToFile('D:\output2.jpg');  // Save in thread 2
ImageEnIO.WaitThreads(false);            // Wait until all saving threads terminate
!!}
procedure TImageEnIO.WaitThreads(Aborts: Boolean);
var
  i: Integer;
begin
  repeat
    EnterCriticalSection(fAsyncThreadsCS);
    if Aborts then
      fAborting := true;
    i := fAsyncThreads.Count;
    LeaveCriticalSection(fAsyncThreadsCS);
    if i = 0 then
      break;
    Windows.WaitForSingleObject(fAsyncThreadsFinishEvent, INFINITE);
  until false;
end;

procedure TImageEnIO.SuspendThreads; {$ifdef IEHASTTHREADSTART} deprecated; {$endif}
var
  i: integer;
begin
  EnterCriticalSection(fAsyncThreadsCS);
  try
    for i := 0 to fAsyncThreads.Count - 1 do
    begin
      TThread(fAsyncThreads[i]).Suspend;
      while not TThread(fAsyncThreads[i]).Suspended do
        sleep(0); // give up the remainder of my current time slice
    end;
  finally
    LeaveCriticalSection(fAsyncThreadsCS);
  end;
end;


procedure TImageEnIO.SetAborting(Value: Boolean);
begin
  fAborting := Value;
  {$IFDEF IEINCLUDEIEXACQUIRE}
  fAcquireParams.Aborting := Value;
  fDCIMParams.Aborting := Value;
  {$ENDIF}
end;

procedure TImageEnIO.ResumeThreads; {$ifdef IEHASTTHREADSTART} deprecated; {$endif}
var
  i: integer;
begin
  EnterCriticalSection(fAsyncThreadsCS);
  try
    for i := 0 to fAsyncThreads.Count - 1 do
      if TThread(fAsyncThreads[i]).Suspended then
        TThread(fAsyncThreads[i]).Resume;
  finally
    LeaveCriticalSection(fAsyncThreadsCS);
  end;
end;

destructor TImageEnIO.Destroy;
begin
  // wait threads
  WaitThreads(false);
  FreeAndNil(fAsyncThreads);
  Windows.CloseHandle(fAsyncThreadsFinishEvent);
  //
  {$ifdef IEINCLUDEDIRECTSHOW}
  CloseMediaFile;
  {$endif}
  CloseAVIFile; // works only if AVI is not closed
  ClosePSFile;
  {$ifdef IEINCLUDEPDFWRITING}
  ClosePDFFile;
  {$endif}
  if assigned(fImageEnView) then
    fImageEnView.RemoveBitmapChangeEvent(fImageEnViewBitmapChangeHandle);
  FreeAndNil(fParams);
  {$IFDEF IEINCLUDEIEXACQUIRE}
  FreeAndNil(fAcquireParams);
  FreeAndNil(fTwainParams);
  FreeAndNil(fDCIMParams);
  {$ENDIF}
  {$IFDEF IEINCLUDEPRINTDIALOGS}
  FreeAndNil(fPrintPreviewParams);
  {$ENDIF}
  FreeAndNil(fPreviewFont);
  if fIEBitmapCreated then
    FreeAndNil(fIEBitmap);
  {$IFDEF IEINCLUDEIEXACQUIRE}
  if assigned(fWia) then
    FreeAndNil(fWia);
  {$ENDIF}
  {$IFDEF IEINCLUDEDIRECTSHOW}
  if assigned(fDShow) then
    FreeAndNil(fDShow);
  {$ENDIF}
  {$IFDEF IEINCLUDEMEDIAFOUNDATION}
  FreeAndNil(fMediaFoundationSourceReader);
  {$ENDIF}

  DeleteCriticalSection(fAsyncThreadsCS);

  inherited;
end;

procedure TImageEnIO.RecreatedTImageEnViewHandle;
begin
  {$ifdef IEINCLUDEDIRECTSHOW}
  if assigned(fImageEnView) and assigned(fDShow) and (fDShow.NotifyWindow<>fImageEnView.Handle) then
    fDShow.SetNotifyWindow(fImageEnView.Handle, IEM_NEWFRAME, IEM_EVENT);
  {$endif}

  {$ifdef IEINCLUDEMEDIAFOUNDATION}
  if assigned(fImageEnView) and assigned(fMediaFoundationSourceReader) then
  begin
    fMediaFoundationSourceReader.ClearNotifyReceivers();
    fMediaFoundationSourceReader.PushNotifyReceiver( TIEMediaFoundationReaderWindowNotifyReceiver.Create(fImageEnView.Handle, IEM_MEDIAFOUNDATION) );
  end;
  {$endif}
end;


procedure TImageEnIO.SyncGetHandle;
begin
  if Assigned(fImageEnView) then
    fImageEnView.Handle;
end;


{!!
<FS>TImageEnIO.Update

<FM>Declaration<FC>
procedure Update;

<FM>Description<FN>
If TImageEnIO is attached to a <A TImageEnView> (or inherited) object, Update will call the Update method of the relevant control.
If TImageEnIO is attached to a TBitmap object, Update sets its <FC>Modified<FN> property to True.
!!}
procedure TImageEnIO.Update;
begin
  // remove alpha if attached to fBitmap
  if assigned(fBitmap) then
    fIEBitmap.RemoveAlphaChannel;

  if assigned(fImageEnView) then
  begin
    with fImageEnView do
    begin
      if IsInsideAsyncThreads then
      begin
        if Parent <> nil then
        begin
          {$ifdef IEHASTTHREADSTATICSYNCHRONIZE}
          if not HandleAllocated then // 3.0.2, to avoid deadlocks
            TThread.Synchronize(nil, SyncGetHandle); // 3.0.1, 15/7/2008 - 13:55
          {$endif}
          PostMessage(handle, IEM_UPDATE, 0, 0);
        end;
      end
      else
        Update;
      ImageChange;
    end;
  end
  else
  if assigned(fBitmap) then
    fBitmap.modified := true;
end;


// Get actual background color
function TImageEnIO.GetReBackground: TColor;
begin
  if assigned(fImageEnView) then
    result := fImageEnView.background
  else
    result := fBackground;
end;


{!!
<FS>TImageEnIO.Background

<FM>Declaration<FC>
property Background: TColor;

<FM>Description<FN>
The background color of the image (shown in unoccupied areas if the current image is smaller than the control size).

Note: When TImageEnIO is attached to <A TImageEnView>, the TImageEnIO.Background is equal to <A TImageEnView.Background>.
!!}
procedure TImageEnIO.SetReBackground(v: TColor);
begin
  if assigned(fImageEnView) then
  begin
{$IFDEF IEINCLUDEMULTIVIEW}
    if not (fImageEnView is TImageEnMView) then
{$ENDIF}
      if fChangeBackground then
        fImageEnView.background := v;
  end
  else
    fBackground := v;
end;

function TImageEnIO.IsInsideAsyncThreads: boolean;
var
  i: integer;
  ch: THandle;
begin
  result := false;
  ch := GetCurrentThreadId;
  try
    EnterCriticalSection(fAsyncThreadsCS);
    for i  := 0 to fAsyncThreads.Count - 1 do
      if TIEIOThread(fAsyncThreads.Items[i]).ThreadId = ch then
      begin
        result := true;
        exit;
      end;
  finally
    LeaveCriticalSection(fAsyncThreadsCS);
  end;
end;

function TImageEnIO.MakeConsistentBitmap(allowedFormats: TIEPixelFormatSet): boolean;
begin
  result := false;
  if not assigned(fIEBitmap) then
    exit;
  if assigned(fBitmap) then
    fIEBitmap.EncapsulateTBitmap(fBitmap, false); // synchronize fBitmap with fIEBitmap
  result := fIEBitmap.CheckFormat(allowedFormats, true);
end;

{!!
<FS>TImageEnIO.SaveToFileGif

<FM>Declaration<FC>
procedure SaveToFileGif(const FileName: WideString);

<FM>Description<FN>
Saves the current image to a file in GIF format (89a). The GIF will have a single image and it won't be marked as animated.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
// Save an interlaced, 64 colour Gif.
ImageEnView1.IO.Params.GIF_Interlaced := True;
ImageEnView1.IO.Params.BitsPerSample := 6;
ImageEnView1.IO.Params.SamplesPerPixel := 1;
ImageEnView1.IO.SaveToFileGif('D:\image.gif');

!!}
procedure TImageEnIO.SaveToFileGIF(const FileName: WideString);
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
begin
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileGIF, FileName);
    exit;
  end;

  
  try
    fAborting := true;
    fs := nil;
    try
      fs := TIEWideFileStream.Create(FileName, fmCreate);
      fAborting := false;
      Progress.Aborting := @fAborting;
      if not MakeConsistentBitmap([]) then
        exit;
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      WriteGIFStream(fs, fiebitmap, fParams, Progress);
      fParams.FileName := FileName;
      fParams.FileType := ioGIF;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.InsertToFileGif

<FM>Declaration<FC>
function InsertToFileGif(const FileName: WideString): integer;

<FM>Description<FN>
Inserts a frame into a GIF file at the position specified by <A TIOParamsVals.GIF_ImageIndex> and makes it animated. The file must exist.

<FC>FileName<FN> is the file name including extension.

Returns the number of images inside the specified file.

<FM>Example<FC>
// Create an animated gif with images contained in ImageEn1 and ImageEn2 components:
ImageEnView1.IO.SaveToFileGif('D:\anim.gif');
ImageEnView2.IO.Params.GIF_ImageIndex := 1;
ImageEnView2.IO.InsertToFileGif('D:\anim.gif');

!!}
function TImageEnIO.InsertToFileGIF(const FileName: WideString): integer;
var
  Progress: TProgressRec;
begin                
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetInt(self, InsertToFileGIF, FileName);
    result := -1;
    exit;
  end;

  
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    result := 0;
    if not MakeConsistentBitmap([]) then
      exit;
    if fParams.GIF_WinWidth < (fiebitmap.width + fParams.GIF_XPos) then
      fParams.GIF_WinWidth := fiebitmap.width + fParams.GIF_XPos;
    if fParams.GIF_WinHeight < (fiebitmap.height + fParams.GIF_ypos) then
      fParams.GIF_WinHeight := fiebitmap.height + fParams.GIF_ypos;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    result := _InsertGIFIm(FileName, fIEBitmap, fParams, Progress);
    if not fAborting then
      _GIFMakeAnimate(string(FileName), 0, fParams.GIF_WinWidth, fParams.GIF_WinHeight); // makes it animated
  finally
    DoFinishWork;
  end;
end;


{!!
<FS>TImageEnIO.InsertToFileTIFF

<FM>Declaration<FC>
function InsertToFileTIFF(const FileName: WideString): integer;

<FM>Description<FN>
Insert a frame into a TIFF file at the position specified by <A TIOParamsVals.ImageIndex>. The file must exist.

<FC>FileName<FN> is the file name including extension.

Returns the number of images inside the file.

<FM>Example<FC>
// Create a multi-image TIFF from images into two ImageEnViews
ImageEnView1.IO.SaveToFile('D:\multi.tif');  // create with a single image
ImageEnView2.IO.Params.ImageIndex := 1;      // prepare to insert as second image
ImageEnView2.IO.InsertToFileTIFF('D:\multi.tif');
!!}
function TImageEnIO.InsertToFileTIFF(const FileName: WideString): integer;
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
begin                
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetInt(self, InsertToFileTIFF, FileName);
    result := -1;
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    result := 0;
    if not MakeConsistentBitmap([]) then
      exit;
    fs := nil;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      fs := TIEWideFileStream.Create(FileName, fmOpenReadWrite);
      fAborting := false;
      result := TIFFWriteStream(fs, true, fIEBitmap, fParams, Progress);
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.InsertToStreamTIFF

<FM>Declaration<FC>
function InsertToStreamTIFF(Stream: TStream): integer;

<FM>Description<FN>
Insert a frame in a TIFF stream at position specified by <A TIOParamsVals.ImageIndex>. The stream position must be at the beginning.
Because ImageEn can only write Little-endian TIFF the input file cannot be Big-endian (see <A TIOParamsVals.TIFF_ByteOrder>).

Returns the number of images inside the file.

<FM>Example<FC>
ImageEnView1.IO.Params.ImageIndex := 1;  // prepare to insert as second image
ImageEnView1.IO.InsertToStreamTIFF(outstream);

!!}
function TImageEnIO.InsertToStreamTIFF(Stream: TStream): integer;
var
  Progress: TProgressRec;
begin         
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetInt(self, InsertToStreamTIFF, Stream);
    result := -1;
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    result := 0;
    if not MakeConsistentBitmap([]) then
      exit;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fAborting := false;
    Stream.Position := 0;
    result := TIFFWriteStream(Stream, true, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;


// replace based on TIFF_ImageIndex

{!!
<FS>TImageEnIO.ReplaceFileTIFF

<FM>Declaration<FC>
function ReplaceFileTIFF(const FileName: WideString): Integer;

<FM>Description<FN>
Save the current image into the specified multi-page TIFF file, replacing the image at the index specified by <A TImageEnIO.Params>.<A TIOParamsVals.TIFF_ImageIndex>.
Because ImageEn can only write Little-endian TIFF the input file cannot be Big-endian (see <A TIOParamsVals.TIFF_ByteOrder>).

<FM>Example<FC>
// We have a multipage tiff, named 'multipage.tif'.
// We want to change only the second page (index 1), making it negative.
ImageEnView1.IO.Params.TIFF_ImageIndex := 1;  // select page
ImageEnView1.IO.LoadFromFile('D:\multipage.tif');
ImageEnView1.Proc.Negative;
ImageEnView1.IO.ReplaceFileTIFF('D:\multipage.tif');

!!}
function TImageEnIO.ReplaceFileTIFF(const FileName: WideString): integer;
begin
  DeleteTIFFIm(FileName, fParams.TIFF_ImageIndex);
  result := InsertToFileTIFF(FileName);
end;

// replace based on GIF_ImageIndex

{!!
<FS>TImageEnIO.ReplaceFileGIF

<FM>Declaration<FC>
function ReplaceFileGIF(const FileName: WideString): Integer;

<FM>Description<FN>
Save the current image into the specified multi-page GIF file, replacing the image at the index specified by <A TImageEnIO.Params>.<A TIOParamsVals.GIF_ImageIndex>.

<FM>Example<FC>
// We have a multipage GIF, named 'multipage.gif'.
// We want to change only the second page (index 1), making it negative.
ImageEnView1.IO.Params.GIF_ImageIndex := 1;  // select page
ImageEnView1.IO.LoadFromFile('D:\multipage.gif');
ImageEnView1.Proc.Negative;
ImageEnView1.IO.ReplaceFileTIFF('D:\multipage.gif');

!!}
function TImageEnIO.ReplaceFileGIF(const FileName: WideString): integer;
begin
  DeleteGIFIm(FileName, fParams.GIF_ImageIndex);
  result := InsertToFileGIF(FileName);
end;

{!!
<FS>DeleteGIFIm

<FM>Declaration<FC>
function DeleteGIFIm(const FileName: WideString; idx: Integer): Integer;

<FM>Description<FN>
Removes an image at index, idx (zero-based), from a multi-frame GIF file.

Returns the remaining number of frames that the file contains. If the FileName doesn't exists or doesn't contain images, it returns 0.

<FM>Example<FC>
// Delete the second image in a a multipage GIF, named 'multipage.gif'.
ImageEnView1.IO.DeleteGIFIm('C:\multipage.gif', 1);  

<FM>See Also<FN>
- <A Helper functions>
!!}
// Remove the image idx from the specified GIF
// returns the remaining images count
function DeleteGIFIm(const FileName: WideString; idx: integer): integer;
begin
  result := _DeleteGIFIm(FileName, idx, true);
end;

{!!
<FS>DeleteTIFFIm

<FM>Declaration<FC>
function DeleteTIFFIm(const FileName: WideString; idx: Integer): Integer;

<FM>Description<FN>
Removes an image at index, idx (zero-based), from a multi-frame TIFF file.

Returns the remaining number of frames that the file contains. If the FileName doesn't exists or doesn't contain images, it returns 0.

<FM>Example<FC>
// Delete the second image in a a multipage TIFF, named 'multipage.tif'.
ImageEnView1.IO.DeleteTIFFIm('C:\multipage.tif', 1);   

<FM>See Also<FN>
- <A Helper functions>
!!}
// removes the image idx from the specified tiff
// returns the remained images
function DeleteTIFFIm(const FileName: WideString; idx: integer): integer;
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenReadWrite);
  try
    result := TIFFDeleteImStream(fs, idx);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>DeleteDCXIm

<FM>Declaration<FC>
procedure DeleteDCXIm(const FileName: WideString; idx: integer);

<FM>Description<FN>
Removes an image at index, idx (zero-based), from a multi-frame DCX file.

Returns the remaining number of frames that the file contains. If the FileName doesn't exists or doesn't contain images, it returns 0.

<FM>Example<FC>
// Delete the second image in a a multipage DCX, named 'multipage.dcx'.
ImageEnView1.IO.DeleteDCXIm('C:\multipage.dcx', 1);

<FM>See Also<FN>
- <A Helper functions>
!!}
procedure DeleteDCXIm(const FileName: WideString; idx: integer);
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenReadWrite);
  try
    IEDCXDeleteStream(fs, idx);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>DeleteTIFFImGroup

<FM>Declaration<FC>
function DeleteTIFFImGroup(const FileName: WideString; Indexes: array of integer): Integer;

<FM>Description<FN>
Removes the specified group of pages from the specified file. Indexes is an array of page indexes to remove.

Returns the remaining number of frames that FileName file contains. If the FileName doesn't exists or doesn't contain images, it returns 0.

!!}
function DeleteTIFFImGroup(const FileName: WideString; Indexes: array of integer): integer;
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenReadWrite);
  try
    result := TIFFDeleteImStreamGroup(fs, @Indexes, high(Indexes) + 1);
  finally
    FreeAndNil(fs);
  end;
end;

// Enumerates images in the specified GIF

{!!
<FS>EnumGIFIm

<FM>Declaration<FC>
function EnumGIFIm(const FileName: WideString): Integer;

<FM>Description<FN>
Returns the number of frames (images) that the specified file contains. If the FileName doesn't exist or doesn't contain images, it returns 0.

Note: A GIF can contain multiple images even if it is not marked as animated.
!!}
function EnumGIFIm(const FileName: WideString): integer;
begin
  try
    result := _DeleteGIFIm(FileName, -1, false);
  except
    result := 0;
  end;
end;

{!!
<FS>EnumTIFFIm

<FM>Declaration<FC>
function EnumTIFFIm(const FileName: WideString): Integer;

<FM>Description<FN>
Returns the number of frames (images) that the specified file contains.

If the FileName doesn't exist or doesn't contain images, it returns 0.

!!}
// Enumerates images in the specified TIFF
function EnumTIFFIm(const FileName: WideString): integer;
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := TIFFEnumImages(fs);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>EnumDCXIm

<FM>Declaration<FC>
function EnumDCXIm(const FileName: WideString): Integer;

<FM>Description<FN>
Returns the number of frames (images) that the specified file contains.

If the FileName doesn't exist or doesn't contain images, it returns 0.

<FM>See Also<FN>
- <A Helper functions>

!!}
function EnumDCXIm(const FileName: WideString): integer;
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := IEDCXCountStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>EnumTIFFStream

<FM>Declaration<FC>
function EnumTIFFStream(Stream: TStream): Integer;

<FM>Description<FN>
Returns the number of frames (images) that the TIFF Stream contains.
!!}
function EnumTIFFStream(Stream: TStream): integer;
begin
  result := TIFFEnumImages(Stream);
end;

{!!
<FS>EnumICOIm

<FM>Declaration<FC>
function EnumICOIm(const FileName: WideString): Integer;

<FM>Description<FN>
Returns the number of images inside an ICO file. 

If the FileName doesn't exist or doesn't contain images, it returns 0.
!!}
function EnumICOIm(const FileName: WideString): integer;
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := _EnumICOImStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>CheckAniGIF

<FM>Declaration<FC>
function CheckAniGIF(const FileName: WideString): Boolean;

<FM>Description<FN>
Returns True if the specified GIF file has the animated flag set ("NETSCAPE2.0" string).

If the file doesn't exist or if the image hasn't set the flag, it returns False.
!!}
// Returns True is the specified GIF is animated
function CheckAniGIF(const FileName: WideString): boolean;
begin
  try
    result := _CheckGIFAnimate(string(FileName));
  except
    result := false;
  end;
end;


{!!
<FS>TImageEnIO.SaveToFile

<FM>Declaration<FC>
procedure SaveToFile(const FileName: WideString; ImageFormat: TIOFileType = -1);

<FM>Description<FN>
Saves the current image to Jpeg, Jpeg2000, PNG, TIFF, BMP, WBMP, PS, PDF, PCX, DCX, TGA, PXM, ICO, HDP, GIF, DICOM and any other supported format.
                                         
<FC>FileName<FN> is the file name including extension.
If <FC>FImageFormat<FN> is -1, then SaveToFile detects the file format from the extension of the specified filename.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
ImageEnView1.IO.SaveToFile('D:\rome.jpg');
ImageEnView1.IO.SaveToFile('D:\florence.tif');
ImageEnView1.IO.SaveToFile('D:\venice.gif');

// Saves a 16 color bitmap
ImageEnView1.IO.Params.BitsPerSample  := 4 ;
ImageEnView1.IO.Params.SamplesPerPixel := 1;
ImageEnView1.IO.SaveToFile('D:\Italy.bmp');

// Saves a jpeg into output.dat
ImageEnView1.IO.SaveToFile('D:\output.dat', ioJPEG);
!!}
procedure TImageEnIO.SaveToFile(const FileName: WideString; ImageFormat: TIOFileType = -1);
var
  ex: string;
  fpi: TIEFileFormatInfo;
  fs: TIEWideFileStream;
  Progress: TProgressRec;
begin
  ex := string(IEExtractFileExtW(FileName));
  if FileName = '' then
    exit;
  if (ImageFormat = ioJPEG) or (ex = '.jpg') or (ex = '.jpeg') or (ex = '.jpe') or (ex='.jif') then
    SaveToFileJpeg(FileName)
  {$IFDEF IEINCLUDEJPEG2000}
  else
  if (ImageFormat = ioJP2) or (ex = '.jp2') then
    SaveToFileJP2(FileName)
  else
  if (ImageFormat = ioJ2K) or (ex = '.j2k') or (ex = '.jpc') or (ex = '.j2c') then
    SaveToFileJ2K(FileName)
  {$ENDIF}
  else
  if (ImageFormat = ioPCX) or (ex = '.pcx') then
    SaveToFilePCX(FileName)
  else
  if (ImageFormat = ioDCX) or (ex = '.dcx') then
    SaveToFileDCX(FileName)
  else
  if (ImageFormat = ioGIF) or (ex = '.gif') then
    SaveToFileGIF(FileName)
  else
  if (ImageFormat = ioTIFF) or (ex = '.tif') or (ex = '.tiff') or (ex = '.fax') or (ex = '.g3f') or (ex = '.g3n') then
    SaveToFileTIFF(FileName)
  {$IFDEF IEINCLUDEPNG}
  else
  if (ImageFormat = ioPNG) or (ex = '.png') then
    SaveToFilePNG(FileName)
  {$ENDIF}
  else
  if (ImageFormat = ioBMP) or (ex = '.bmp') or (ex = '.dib') or (ex = '.rle') then
    SaveToFileBMP(FileName)
  else
  if (ImageFormat = ioTGA) or (ex = '.tga') or (ex = '.targa') or (ex = '.vda') or (ex = '.icb') or (ex = '.vst') or (ex = '.win') then
    SaveToFileTGA(FileName)
  else
  if (ImageFormat = ioPXM) or (ex = '.pxm') or (ex = '.pbm') or (ex = '.pgm') or (ex = '.ppm') then
    SaveToFilePXM(FileName)
  else
  if (ImageFormat = ioICO) or (ex = '.ico') then
    SaveToFileICO(FileName)
  else
  if (ImageFormat = ioWBMP) or (ex = '.wbmp') then
    SaveToFileWBMP(FileName)
  else
  if (ImageFormat = ioPS) or (ex = '.ps') or (ex = '.eps') then
    SaveToFilePS(FileName)
  {$ifdef IEINCLUDEPDFWRITING}
  else
  if (ImageFormat = ioPDF) or (ex = '.pdf') then
    SaveToFilePDF(FileName)
  {$endif}
  {$ifdef IEINCLUDEPSD}
  else
  if (ImageFormat = ioPSD) or (ex = '.psd') or (ex = '.psb') then
    SaveToFilePSD(FileName)
  {$endif}
  {$ifdef IEINCLUDEWIC}
  else
  if (ImageFormat = ioHDP) or (ex = '.hdp') or (ex = '.wdp') or (ex = '.jxr') then
    SaveToFileHDP(FileName)
  {$endif}
  {$ifdef IEINCLUDEDICOM}
  else
  if (ImageFormat = ioDICOM) or (ex = '.dcm') or (ex = '.dic') or (ex = '.dicom') or (ex = '.v2') then
    SaveToFileDICOM(FileName)
  {$endif}
  else
  if ((ImageFormat = ioALL) or (ex = '.all')) and (fImageEnView <> nil) then
    (fImageEnView as TImageEnVect).SaveToFileAll(FileName)
  else
  if ((ImageFormat = ioLYR) or (ex = '.lyr')) and (fImageEnView <> nil) then
    (fImageEnView as TImageEnView).LayersSaveToFile(FileName)
  else
  begin
    // try registered file formats
    if ImageFormat > -1 then
      fpi := IEFileFormatGetInfo(ImageFormat)
    else
      fpi := IEFileFormatGetInfo2(ex);
    if assigned(fpi) and assigned(fpi.WriteFunction) then
      with fpi do
      begin        
        // ASYNC SAVING
        if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
        begin
          TIEIOThread.CreateLoadSaveFileIF(self, SaveToFile, FileName, ImageFormat);
          exit;
        end;

        
        try
          fAborting := true; // So that fAborting is True if the file is not found/accessible
          Progress.Aborting := @fAborting;
          if not MakeConsistentBitmap([]) then
            exit;
          if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
            fIEBitmap.PixelFormat := ie24RGB;
          fs := TIEWideFileStream.Create(FileName, fmCreate);
          fAborting := false;
          Progress.fOnProgress := fOnIntProgress;
          Progress.Sender := Self;
          fParams.fFileName := FileName;
          fParams.fFileType := FileType;
          try
            WriteFunction(fs, fIEBitmap, fParams, Progress);
          finally
            FreeAndNil(fs);
          end;
          fParams.FileName := FileName;
          fParams.FileType := fpi.FileType;
        finally
          DoFinishWork;
        end;
      end
    else
      fAborting := True;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamGif

<FM>Declaration<FC>
procedure SaveToStreamGif(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in GIF format.

If <A TImageEnIO.StreamHeaders> property is true, then a special header is added as required for multi-image streams.

<FM>Example<FC>
// Saves images in ImageEnView1 and ImageEnView2 to the file Images.dat
// Note: images.dat isn't loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('C:\images.dat', fmCreate);
  ImageEnView1.IO.StreamHeaders := True;
  ImageEnView1.IO.SaveToStreamGIF(fs);
  ImageEnView2.IO.StreamHeaders := True;
  ImageEnView2.IO.SaveToStreamGIF(fs);
  fs.free;
End;

// Saves a single image to image.gif
// image.gif is loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('image.gif');
  ImageEnView1.IO.StreamHeaders := False;
  ImageEnView1.IO.SaveToFileGIF(fs);
End;

!!}
procedure TImageEnIO.SaveToStreamGIF(Stream: TStream);
var
  Progress: TProgressRec;
begin                                
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamGIF, Stream);
    exit;
  end;

  
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    WriteGIFStream(Stream, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;



{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnIO.Acquire

<FM>Declaration<FC>
function Acquire(bResetParams : Boolean = False) : boolean;

<FM>Description<FN>
Perform a single image acquisition from the <A TImageEnIO.SelectedAcquireSource>, which may be a camera card, Twain or WIA device. <A TImageEnIO.SelectedAcquireSource> is set when the user chooses a source if you have called <A TImageEnIO.SelectAcquireSource> or manually set a source using  <A TImageEnIO.SetAcquireSource>.

If bResetParams is True then <A TIOParamsVals.SetDefaultParams> is called before Acquire to reset the <L TIOParamsVals>IO parameters</L> (if you have already loaded an image from file).

Note: For acquisition of multiple images use <L TImageEnMIO.Acquire>TImageEnMIO.Acquire</L>.

<FM>Examples<FC>
// Prompt the user to choose a scanner source and then acquire
if ImageEnView1.IO.SelectAcquireSource([ieaTwain, ieaWIA, ieaDCIM]) then
  ImageEnView1.IO.Acquire;

// capture from the default WIA device
if ImageEnView1.IO.SetSource(ieaWIA, Default_Device) then
  ImageEnView1.IO.Acquire;

// select the second Twain device and capture
if ImageEnView1.IO.SetSource(ieaTwain, 1) then
  ImageEnView1.IO.Acquire;

// Capture from the Twain device named, CanoScan FB620
if ImageEnView1.IO.SetSource(ieaTwain, 'CanoScan FB620') then
  ImageEnView1.IO.Acquire;

// Retrieve from the camera card listed as H:\ drive
if ImageEnView1.IO.SetSource(ieaDCIM, 'H') then
  ImageEnView1.IO.Acquire;

// Capture without a dialog
ImageEnView1.IO.AcquireParams.VisibleDialog := False;
if ImageEnView1.IO.Acquire then
ImageEnView1.IO.SaveToFile('D:\newimage.jpg'); // save scanned image
!!}
function TImageEnIO.Acquire(bResetParams : Boolean) : boolean;
var
  bHandled: Boolean;
begin
  // calling Acquire after TwainAcquireOpen!!!!
  if assigned(fgrec) then
  begin
    result := true; // there is already a scanner dialog open
    exit;
  end;
  //
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateAcquire(self, Acquire, bResetParams);
    result := true;
    exit;
  end;
  try
    fAborting := false;
    result := false;
    if not MakeConsistentBitmap([]) then
      exit;

    if bResetParams then
      fParams.SetDefaultParams;

    Result := fAcquireParams.Acquire(fIEBitmap, fParams, fOnProgress, NativePixelFormat);

    if Result then
      DoAcquireBitmap(fIEBitmap, bHandled); // Note: bHandled is not honored by this Acquire method

    if Result and (fAcquireParams.SelectedSourceApi = ieaTwain) then
    begin
      if fAutoAdjustDPI then
        AdjustDPI;
      if assigned(fImageEnView) then
      begin
        fImageEnView.dpix := fParams.DpiX;
        fImageEnView.dpiy := fParams.DpiY;
      end;
    end;

    Update;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnIO.SelectAcquireSource

<FM>Declaration<FC>
function SelectAcquireSource(Apis: <A TIEAcquireApis> = [ieaTwain, ieaWIA]) : boolean;

<FM>Description<FN>
Prompts the user with a dialog to select a Twain, WIA or DCIM device. Use Apis to specify which sources are available to the user.

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
if ImageEn1.IO.SelectAcquireSource([ieaTwain, ieaWIA]) then
begin
  ImageEn1.IO.Acquire;
  ImageEn1.IO.SaveToFile('D:\MyImage.jpg');
end;
!!}
function TImageEnIO.SelectAcquireSource(Apis : TIEAcquireApis = [ieaTwain, ieaWIA]): boolean;
// NPC: 16/11/11
begin
  Result := fAcquireParams.SelectSource(Apis);
end;




{!!
<FS>TImageEnIO.SetAcquireSource

<FM>Declaration<FC>
function SetAcquireSource(Api : <A TIEAcquireApi>; Location : Variant) : boolean;

<FM>Description<FN>
Programatically set the selected acquisition source by an API type and device. The selected device will be used for subsequent calls to <A TImageEnIO.Acquire>.
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

<FM>Examples<FC>
// Acquire from the default Twain device
if ImageEnView1.IO.SetAcquireSource(ieaTwain, Default_Device) then
  ImageEnView1.IO.Acquire;

// Select a Twain scanner by name
ImageEnView1.IO.SetAcquireSource(ieaTwain, 'CanoScan FB620');

// Select the second WIA device
ImageEnView1.IO.SetAcquireSource(ieaWIA, 1);

// Acquire from the camera card on H drive
if ImageEnView1.IO.SetAcquireSource(ieaDCIM, 'H') then
  ImageEnView1.IO.Acquire;

// Read and restore a source
var
  sDevice : string;
  ADevice : TIEAcquireSource;
begin
  ...
  // Read the selected device
  sDevice := AcquireSourceToStr(ImageEnView1.IO.SelectedAcquireSource);
  ...
end;

var
  sDevice : string;
  ADevice : TIEAcquireSource;
begin
  ...
  // Restore the device selection
  ADevice := StrToAcquireSource(sDevice);
  ImageEnView1.IO.SetAcquireSource(ADevice.Api, ADevice.Location);
  ...
end;

<FM>See Also<FN>
- <A TImageEnIO.Acquire>
- <A TImageEnIO.SelectedAcquireSource>
!!}
function TImageEnIO.SetAcquireSource(Api: TIEAcquireApi; Location : Variant) : boolean;
// NPC: 16/11/11
begin
  Result := fAcquireParams.SetSource(Api, Location);
end;

{!!
<FS>TImageEnIO.SelectedAcquireSource

<FM>Declaration<FC>
property SelectedAcquireSource : <A TIEAcquireSource>; (Read only)

<FM>Description<FN>
Returns the acquisition source that is currently active due to selection by the user with <A TImageEnIO.SelectAcquireSource> or programatically using <A TImageEnIO.SetAcquireSource>.
A <A TIEAcquireSource> record is returned that provides meta information about the device (SelectedAcquireSource.Name, SelectedAcquireSource.DeviceType) and technical details (SelectedAcquireSource.Api, SelectedAcquireSource.Location).
If no device is selected then SelectedAcquireSource.Api will be ieaNone.

<FM>Examples<FC>
// Display the selected source
if ImageEnView1.IO.SelectedAcquireSource.Api = ieaNone then
  ShowMessage('No device is selected')
else
  ShowMessage('The selected device is ' + ImageEnView1.IO.SelectedAcquireSource.Name);

// Read and restore the selected source
var
  sDevice : string;
  ADevice : TIEAcquireSource;
begin
  ...
  // Read the selected device
  sDevice := AcquireSourceToStr(ImageEnView1.IO.SelectedAcquireSource);
  ...
end;

var
  sDevice : string;
  ADevice : TIEAcquireSource;
begin
  ...
  // Restore the device selection
  ADevice := StrToAcquireSource(sDevice);
  ImageEnView1.IO.SetAcquireSource(ADevice.Api, ADevice.Location);
  ...
end;

<FM>See Also<FN>
- <A TImageEnIO.SelectAcquireSource>
- <A TImageEnIO.SetAcquireSource>
!!}
function TImageEnIO.GetSelectedAcquireSource : TIEAcquireSource;
// NPC: 16/11/11
begin
  Result := fAcquireParams.SelectedSource;
end;


procedure TImageEnIO.InitializeAcquireSource(bIncludeWIA : Boolean);
// NPC: 16/11/11
begin
  AcquireParams.AttachedTwainParams := fTwainParams;
  AcquireParams.AttachedDCIMParams := fDcimParams;
  {$IFDEF IEINCLUDEIEXACQUIRE}
  if bIncludeWIA then
    AcquireParams.AttachedWIAParams := WIAParams;
  {$ENDIF}
end;

{$ENDIF}

function TImageEnIO.SyncLoadFromStreamGIF(Stream: TStream): integer;
var
  p1: int64;
  bmp, merged, prev: TIEBitmap;
  numi, reqidx: integer;
  Progress, Progress2: TProgressRec;
  im: integer;
  tempAlphaChannel: TIEMask;
  dummy1: ppointerarray;
  dummy2, dummy3: pinteger;
  act: TIEGIFAction;
  backx, backy, backw, backh: integer;
  dx, dy: integer;
  OriginalPixelFormat: TIEPixelFormat;
begin
  result := 0;
  bmp := nil;
  merged := nil;
  prev := nil;
  try
    fAborting := False;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;

    if fParams.GIF_RAWLoad then
    begin
      tempAlphaChannel := nil;
      ReadGIFStream(Stream, fIEBitmap, numi, fParams, Progress, False, tempAlphaChannel, false);
      CheckDPI;
      if assigned(tempAlphaChannel) then
      begin
        fIEBitmap.AlphaChannel.CopyFromTIEMask(tempAlphaChannel);
        FreeAndNil(tempAlphaChannel);
      end;
      if assigned(fImageEnView) and (fImageEnView is TImageEnView) then
      begin
        with (fImageEnView as TImageEnView).CurrentLayer do
        begin
          PosX := fParams.GIF_XPos;
          PosY := fParams.GIF_YPos;
        end;
      end;
    end
    else
    begin
      p1 := Stream.Position;
      im := 0;
      merged := TIEBitmap.Create();
      merged.Location := ieMemory;
      act := ioGIF_None;
      backw := 0;
      backh := 0;
      backx := 0;
      backy := 0;
      reqidx := fParams.GIF_ImageIndex;

      if reqidx > 0 then
      begin
        Progress2.fOnProgress := nil;
        Progress2.Sender := nil;
        Progress2.Aborting := @fAborting;
      end
      else
        Progress2 := Progress;

      repeat
        bmp := TIEBitmap.Create();
        Stream.position := p1;
        fParams.GIF_ImageIndex := im;
        tempAlphaChannel := nil;
        ReadGIFStream(Stream, bmp, numi, fParams, Progress2, False, tempAlphaChannel, false);
        OriginalPixelFormat := bmp.PixelFormat;
        CheckDPI;
        dx := imax(fParams.GIF_WinWidth, bmp.Width);
        dy := imax(fParams.GIF_WinHeight, bmp.Height);
        if assigned(tempAlphaChannel) then
        begin
          bmp.AlphaChannel.CopyFromTIEMask(tempAlphaChannel);
          FreeAndNil(tempAlphaChannel);
        end;
        if numi > 1 then
        begin
          if fParams.GIF_Action = ioGIF_RestorePrev then
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
            merged.FillRect(backx, backy, backx + backw - 1, backy + backh - 1, TRGB2TColor(fParams.GIF_Background));
            merged.AlphaChannel.FillRect(backx, backy, backx + backw - 1, backy + backh - 1, 0);
          end;
          if (merged.Width = 0) then
          begin
            merged.Allocate(dx, dy, ie24RGB);
            merged.Fill(TRGB2TColor(fParams.GIF_Background));
            merged.AlphaChannel.Fill(0);
          end;
          if merged.PixelFormat <> ie24RGB then
            merged.PixelFormat := ie24RGB;
          if (dx > merged.Width) or (dy > merged.Height) then
            merged.Resize(dx, dy, TRGB2TColor(fparams.GIF_Background), 255, iehLeft, ievTop);
          dummy1 := nil;
          dummy2 := nil;
          dummy3 := nil;

          bmp.RenderToTIEBitmap(merged, dummy1, dummy2, dummy3, nil, fParams.GIF_XPos, fParams.GIF_YPos, bmp.Width, bmp.Height, 0, 0, bmp.Width, bmp.Height, true, false, 255, rfNone, true, ielNormal);

          bmp.MergeAlphaRectTo(merged, 0, 0, fParams.GIF_XPos, fParams.GIF_YPos, bmp.Width, bmp.Height);
          merged.AlphaChannel.Full := false;

          backw := bmp.Width;
          backh := bmp.Height;
          backx := fParams.GIF_XPos;
          backy := fParams.GIF_YPos;
          FreeAndNil(bmp);
          bmp := merged;
          act := fParams.GIF_Action; // act refers to the action of next image
          fParams.GIF_Action := ioGIF_DrawBackground;
          fParams.GIF_XPos := 0;
          fParams.GIF_YPos := 0;
        end;

        if fAborting then
        begin
          if bmp <> merged then
            FreeAndNil(bmp);
          break;
        end;
        if numi > 0 then
        begin
          Progress.per1 := 100 / numi;
          fIEBitmap.Assign(bmp);
          fIEBitmap.PixelFormat := originalPixelFormat;
        end;
        if bmp <> merged then
          FreeAndNil(bmp)
        else
          bmp := nil;
        with Progress do
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * im));
        if fAborting then
          break;
        inc(im);
      until (im >= numi) or (im - 1 = reqidx);
      if bmp = merged then
        bmp := nil;
      FreeAndNil(merged);
      fParams.GIF_ImageIndex := reqidx;
    end;

    result := numi;

    if fAutoAdjustDPI then
      AdjustDPI;
    if fParams.GIF_FlagTranspColor then
      BackGround := TRGB2TCOLOR(fParams.GIF_TranspColor)
    else
      BackGround := TRGB2TCOLOR(fParams.GIF_Background);

    fParams.FileType := ioGIF;
    fParams.fFileName := '';
    Update;
  finally
    prev.Free();
    bmp.Free();
    merged.Free();
    DoFinishWork;
  end;
end;


{!!
<FS>TImageEnIO.LoadFromFileGIF

<FM>Declaration<FC>
function LoadFromFileGIF(const FileName: WideString): integer;

<FM>Description<FN>
Loads an image from a GIF file (87a, 89a and animated GIF).

<FC>FileName<FN> is the file name including extension.

Returns the number of images contained in GIF file (if this is an animated GIF) or -1 if an error was encountered while loading, such as the file not being GIF format (<A TImageEnIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

Note: If <A TImageEnIO.AsyncMode>=True the result will always be -1

<FM>Example<FC>
// Loads the third frame of an animated gif
ImageEnView1.IO.Params.GIF_ImageIndex := 2;
ImageEnView1.IO.LoadFromFileGIF('D:\anim.gif');
!!}
// return remaining images
function TImageEnIO.LoadFromFileGIF(const FileName: WideString): integer;
var
  fs: TIEWideFileStream;
begin                   
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetInt(self, LoadFromFileGIF, FileName);
    result := -1;
    exit;
  end;

  
  fs := TIEWideFileStream.create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := SyncLoadFromStreamGIF(fs);
    fParams.fFileName := FileName;
  finally
    FreeAndNil(fs);
  end;
end;



{!!
<FS>TImageEnIO.LoadFromStreamGIF

<FM>Declaration<FC>
function LoadFromStreamGIF(Stream: TStream): integer;

<FM>Description<FN>
Loads an image from a stream containing a GIF file. 

Returns the number of images contained in the stream (if this is an animated stream) or -1 if an error was encountered while loading, such as the file not being GIF format (<A TImageEnIO.Aborting> will be true). File access errors will raise an exception. If <A TImageEnIO.AsyncMode>=True the result will always be -1.

Note: If <A TImageEnIO.StreamHeaders> property is True, the stream must have a special header (saved using <A TImageEnIO.SaveToStreamGIF>).

<FM>Example<FC>
// loads a GIF file with LoadfFromStreamGIF
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('C:\myfile.gif', fmOpenRead);
  ImageEnView1.IO.LoadFromStreamGIF(fs);
  fs.free;
End;
!!}
// returns remaining images
function TImageEnIO.LoadFromStreamGIF(Stream: TStream): integer;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin                           
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetInt(self, LoadFromStreamGIF, Stream);
    result := -1;
  end
  else
  begin
    result := SyncLoadFromStreamGIF(Stream);
    if fAborting then
      Result := -1;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromFileDCX

<FM>Declaration<FC>
function LoadFromFileDCX(const FileName: WideString): Boolean;

<FM>Description<FN>
Loads an image from a DCX file.

<FC>FileName<FN> is the file name including extension. Result will be false if the file is not a DCX format (and <A TImageEnIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

Note: You can set the page to load using <A TImageEnIO.Params>.<A TIOParamsVals.DCX_ImageIndex> property.

<FM>Example<FC>
// I want the second page of 'input.dat' (which is a DCX file)
ImageEnView1.IO.Params.DCX_ImageIndex := 1;
ImageEnView1.IO.LoadFromFileDCX('C:\input.dat');
!!}
function TImageEnIO.LoadFromFileDCX(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin            
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileDCX, FileName);
    Result := True;
    exit;
  end;


  fs := TIEWideFileStream.create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamDCX(fs);
    fParams.fFileName := FileName;   
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamDCX

<FM>Declaration<FC>
function LoadFromStreamDCX(Stream: TStream): Boolean;

<FM>Description<FN>
Loads an image from a stream containing a DCX file. The result will be false if an error is encountered, e.g. the file in the stream is not DCX format (<A TImageEnIO.Aborting> will be true).

Note: You can set the page to load using <A TImageEnIO.Params>.<A TIOParamsVals.DCX_ImageIndex> property.
!!}
function TImageEnIO.LoadFromStreamDCX(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin           
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamDCX, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamDCX(Stream);
    Result := Not fAborting;
  end;
end;


procedure TImageEnIO.SyncLoadFromStreamDCX(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    IEDCXReadStream(Stream, fIEBitmap, fParams, Progress, false);
    CheckDPI;
    if fAutoAdjustDPI then
      AdjustDPI;
    fParams.fFileName := '';
    fParams.fFileType := ioDCX;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    Update;
  finally
    DoFinishWork;
  end;
end;


procedure TImageEnIO.SyncLoadFromStreamPCX(Stream: TStream; streamhead: boolean);
var
  SHead: PCXSHead;
  lp1: int64;
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    lp1 := 0;
    if streamhead then
    begin
      // load header
      lp1 := Stream.Position;
      Stream.Read(SHead, sizeof(PCXSHead));
      if IECopy(SHead.id, 1, 3) <> 'PCX' then
      begin
        fAborting := true;
        exit;
      end;
      if SHead.id <> 'PCX2' then
      begin
        Stream.Position := lp1 + 4;
        SHead.dim := Stream.Size;
      end;
    end
    else
      SHead.dim := Stream.Size;
    Progress.Aborting := @fAborting;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    ReadPcxStream(Stream, fIEBitmap, fParams, SHead.dim, Progress, false);
    CheckDPI;
    if fAutoAdjustDPI then
      AdjustDPI;
    fParams.fFileName := '';
    fParams.fFileType := ioPCX;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    Update;
    if streamhead and (SHead.id = 'PCX2') then
      Stream.Position := lp1 + sizeof(SHead) + SHead.dim; // posiziona alla fine del blocco PCX
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamPCX

<FM>Declaration<FC>
function LoadFromStreamPCX(Stream: TStream): Boolean;

<FM>Description<FN> 
Loads an image from a stream containing a PCX file. The result will be false if an error is encountered, e.g. the file in the stream is not PCX format (<A TImageEnIO.Aborting> will be true).

Note: If <A TImageEnIO.StreamHeaders> property is True, the stream must have a special header (saved using <A TImageEnIO.SaveToStreamPCX>).

<FM>Example<FC>
// loads a PCX file with LoadfFromStreamPCX
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('myfile.pcx', fmOpenRead);
  if ImageEnView1.IO.LoadFromStreamPCX(fs) = False then
    ShowMessage('Not a PCX file!');
  fs.free;
End;

!!}
function TImageEnIO.LoadFromStreamPCX(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin               
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamPCX, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamPCX(Stream, fStreamHeaders);
    Result := Not fAborting;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromFilePCX

<FM>Declaration<FC>
function LoadFromFilePCX(const FileName: WideString): Boolean;

<FM>Description<FN>
Loads an image from a PCX file. Result will be false if the file is not PCX format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

<FC>FileName<FN> is the file name including extension.
                                    
<FM>Example<FC>
ImageEnView1.IO.LoadFromFilePCX('C:\alfa.pcx');

!!}
function TImageEnIO.LoadFromFilePCX(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFilePCX, FileName);
    Result := True;
    exit;
  end;

  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamPCX(fs, false);
    fParams.fFileName := FileName;
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
end;

function TImageEnIO.SyncLoadFromStreamTIFF(Stream: TStream; streamhead: boolean): integer;
var
  SHead: TIFFSHead;
  Progress: TProgressRec;
  p0: int64;
  tmpAlphaChannel: TIEMask;
  BufStream: TIEBufferedReadStream;
begin
  BufStream := TIEBufferedReadStream.Create(Stream, 8192, IEGlobalSettings().UseRelativeStreams);
  fAborting := false;
  Progress.Aborting := @fAborting;
  try
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    p0 := 0;
    if streamhead then
    begin
      BufStream.read(SHead, sizeof(SHead));
      p0 := BufStream.Position;
      if SHead.id <> 'TIFF' then
      begin
        fAborting := true;
        result := 0;
        exit;
      end;
    end;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    tmpAlphaChannel := nil;
    TIFFReadStream(fIEBitmap, BufStream, result, fParams, Progress, false, tmpAlphaChannel, not streamhead, false, false, false);
    CheckDPI;
    if assigned(tmpAlphaChannel) then
    begin
      fIEBitmap.AlphaChannel.CopyFromTIEMask(tmpAlphaChannel);
      FreeAndNil(tmpAlphaChannel);
    end;
    if fAutoAdjustDPI then
      AdjustDPI;
    if streamhead then
      BufStream.Position := p0 + SHead.dim;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    fParams.fFileName := '';
    fParams.fFileType := ioTIFF;
    Update;
  finally
    BufStream.Free;
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamTIFF

<FM>Declaration<FC>
function LoadFromStreamTIFF(Stream: TStream): integer;

<FM>Description<FN>                
Loads an image from a stream containing a TIFF file (rev. 6.0, Packbits, LZW, CCITT G.3 and G.4).

Returns the number of images contained in the stream or -1 if an error was encountered while loading, such as the file not being TIFF format (<A TImageEnIO.Aborting> will be true). Result will always be -1 if <A TImageEnIO.AsyncMode>=True.

Note: If <A TImageEnIO.StreamHeaders> property is True, the stream must have a special header (saved using <A TImageEnIO.SaveToStreamTIFF>).

<FM>Example<FC>
// loads a TIFF file with LoadfFromStreamTIFF
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('C:\myfile.tif', fmOpenRead);
  ImageEnView1.IO.LoadFromStreamTIFF(fs);
  fs.free;
End;

!!}
function TImageEnIO.LoadFromStreamTIFF(Stream: TStream): integer;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin                      
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetInt(self, LoadFromStreamTIFF, Stream);
    result := -1;
  end
  else
  begin
    result := SyncLoadFromStreamTIFF(Stream, fStreamHeaders);
    if fAborting then
      Result := -1;
  end;
end;

// returns remaining images

{!!
<FS>TImageEnIO.LoadFromFileTIFF

<FM>Declaration<FC>
function LoadFromFileTIFF(const FileName: WideString): integer:

<FM>Description<FN>
Loads an image from a TIFF file (rev. 6.0, Packbits, LZW, CCITT G.3 and G.4).

<FC>FileName<FN> is the file name including extension.
Returns the number of images contained in TIFF file. Result will be -1 if an error was encountered while loading, such as the file not being TIFF format (<A TImageEnIO.Aborting> will be true). Loading errors due to a file not being available will raise an exception.

Note: If <A TImageEnIO.AsyncMode> = True then the result will always be -1.

<FM>Example<FC>
// Load the second image in the MyImage.tif file
ImageEnView1.IO.Params.TIFF_ImageIndex := 1;
ImageEnView1.IO.LoadFromFileTIFF('D:\MyImage.tif');

!!}
function TImageEnIO.LoadFromFileTIFF(const FileName: WideString): integer;
var
  fs: TIEWideFileStream;
begin                                     
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetInt(self, LoadFromFileTIFF, FileName);
    result := -1;
    exit;
  end;

  
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := SyncLoadFromStreamTIFF(fs, false);
    if fAborting then
      Result := -1;
  finally
    FreeAndNil(fs);
  end;
  fParams.fFileName := FileName;
end;

{!!
<FS>TImageEnIO.LoadFromFileAuto

<FM>Declaration<FC>
function LoadFromFileAuto(const FileName: WideString): Boolean; dynamic;

<FM>Description<FN>
Loads an image from file. Unlike <A TImageEnIO.LoadFromFile> it ignores the file extension, instead it analyzes the file content to determine its format. Result will be false if the file is not a recognized file type (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

Note: This method can load LYR (<A TImageEnView> layers), IEV (<A TImageEnVect> objects) and LYR+IEV formats when <A TImageEnIO.AttachedImageEn> is TImageEnView or TImageEnVect.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFileAuto('C:\input.dat');

ImageEnView1.IO.LoadFromFileAuto('C:\input.tif'); // a tiff or a RAW?

if ImageEnView1.IO.LoadFromFileAuto(sFilename) = False then
  ShowMessage('Not a supported file type!');

<FM>See Also<FN>
- <A TImageEnIO.LoadFromFile>
- <A TImageEnIO.LoadFromFileFormat>
!!}
function TImageEnIO.LoadFromFileAuto(const FileName: WideString): Boolean;
var
  ff: TIOFileType;
begin
  ff := FindFileFormat(FileName, false);
  if ff = ioUnknown then
    Result := LoadFromFile(FileName)
  else
    Result := LoadFromFileFormat(FileName, ff);
end;

function TImageEnIO.LoadViewerFile(const fileName: WideString; ft: TIOFileType): Boolean;
var
  r: TRect;
  tempIE: TImageEnVect;
begin    
  Result := False;
  case ft of
    ioIEV:
      begin
        if assigned(fImageEnView) and (fImageEnView is TImageEnVect) then
          with (fImageEnView as TImageEnVect) do
          begin
            Result := LoadFromFileIEV(fileName);
            r := ObjectsExtents;
            Proc.ImageResize(r.Right, r.Bottom);
            Proc.Fill(CreateRGB(255, 255, 255));
            AlphaChannel.Fill(0);
          end
        else
        begin
          // TImageEnView not associated, renderize image
          tempIE := TImageEnVect.Create(nil);
          try
          tempIE.LegacyBitmap := False;
          Result := tempIE.LoadFromFileIEV(fileName);
          r := TempIE.ObjectsExtents;
          tempIE.Proc.ImageResize(r.Right, r.Bottom);
          tempIE.Proc.Fill(CreateRGB(255, 255, 255));
          tempIE.CopyObjectsToBack();
          IEBitmap.Assign( tempIE.IEBitmap );
          finally
            tempIE.Free;
            DoFinishWork;
          end;
        end;
      end;
    ioLYR:
      begin
        if assigned(fImageEnView) and (fImageEnView is TImageEnView) then
          Result := (fImageEnView as TImageEnView).LayersLoadFromFile(fileName)
        else
        begin
          // TImageEnView not associated, renderize image
          tempIE := TImageEnVect.Create(nil);
          try
          tempIE.LegacyBitmap := False;
          Result := tempIE.LayersLoadFromFile(fileName);
          tempIE.LayersMergeAll;
          IEBitmap.Assign( tempIE.IEBitmap );
          finally
            tempIE.Free;
            DoFinishWork;
          end;
        end;
      end;
    ioALL:
      begin
        if assigned(fImageEnView) and (fImageEnView is TImageEnVect) then
          Result := (fImageEnView as TImageEnVect).LoadFromFileALL(string(fileName))
        else
        begin
          // TImageEnView not associated, renderize image
          tempIE := TImageEnVect.Create(nil);
          try
          tempIE.LegacyBitmap := False;
          Result := tempIE.LoadFromFileALL(string(fileName));
          tempIE.LayersMergeAll;
          tempIE.CopyObjectsToBack();
          IEBitmap.Assign( tempIE.IEBitmap );
          finally
            tempIE.Free;
            DoFinishWork;
          end;
        end;
      end;
  end;
  Params.fFileType := ft;
end;

function TImageEnIO.LoadViewerStream(Stream: TStream; ft: TIOFileType): Boolean;
begin     
  Result := False;
  case ft of
    ioIEV:
      begin
        if assigned(fImageEnView) and (fImageEnView is TImageEnVect) then
          Result := (fImageEnView as TImageEnVect).LoadFromStreamIEV(Stream);
      end;
    ioLYR:
      begin
        if assigned(fImageEnView) and (fImageEnView is TImageEnView) then
          Result := (fImageEnView as TImageEnView).LayersLoadFromStream(Stream);
      end;
    ioALL:
      begin
        if assigned(fImageEnView) and (fImageEnView is TImageEnVect) then
          Result := (fImageEnView as TImageEnVect).LoadFromStreamALL(Stream);
      end;
  end;
  Params.fFileType := ft;
end;


{!!
<FS>TImageEnIO.LoadFromFile

<FM>Declaration<FC>
function LoadFromFile(const FileName: WideString; ImageFormat: TIOFileType = ioUnknown): Boolean; overload;
function LoadFromFile(const FileName: WideString; bCheckUnknown: Boolean): Boolean; overload;

<FM>Description<FN>
Loads an image from the specified file. It recognizes the image format from the filename extension (if <FC>ImageFormat<FN> is not specified).
The source can be also an URL if it has the form 'http://'.

<FC>FileName<FN> is the file name including extension.
<FC>ImageFormat<FN> specifies the image format. If specified as ioUnknown, it will use the file's extension.
<FC>bCheckUnknown<FN> if the file extension is not known or is incorrect (e.g. a GIF file named MyImage.jpg), then loading will be attempted by analyzing the file content (in the same way as <A TImageEnIO.LoadFromFileAuto>)

Result will be false if the file is not a recognized file type (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

Notes:
- By default, this routine will fail for files with an invalid extension (e.g. a GIF file named MyImage.jpg) or unknown extension. To avoid this you can use the <FC>bCheckUnknown<FN> overload or <A TImageEnIO.LoadFromFileAuto>
- Can load LYR (<A TImageEnView> layers), IEV (<A TImageEnVect> objects) and LYR+IEV formats when <A TImageEnIO.AttachedImageEn> is TImageEnView or TImageEnVect.

<FM>Example<FC>
// Load the second image in the MyImage.tif file
ImageEnView1.IO.Params.TIFF_ImageIndex := 1;
ImageEnView1.IO.LoadFromFile('D:\MyImage.tif');

// Load a file even if this file extension is incorrect
ImageEnView1.IO.LoadFromFile(sFilename, True);

<FM>See Also<FN>
- <A TImageEnIO.LoadFromFileAuto>
- <A TImageEnIO.LoadFromFileFormat>
!!}
const
  IO_CHECK_UNKNOWN = -9;

function TImageEnIO.LoadFromFile(const FileName: WideString; ImageFormat: TIOFileType): Boolean;
var
  ex: string;
  fpi: TIEFileFormatInfo;
  fs: TIEWideFileStream;
  Progress: TProgressRec;
  ff: TIOFileType;
begin
  Result := False;
  if not assigned(fIEBitmap) then
    exit;

  if FileName = '' then
  begin
    fAborting := True;
    DoFinishWork;
    exit;
  end;

  if IEGetURLTypeW(FileName) <> ieurlUNKNOWN then
  begin
    Result := LoadFromURL(FileName);
    exit;
  end;

  ex := string(IEExtractFileExtW(FileName));

  if (ex = '.avi') then
  begin
    OpenAVIFile(FileName);
    if fAborting then
      exit;
    LoadFromAVI(fParams.fImageIndex);
    CloseAVIFile;
    Params.fFileName := FileName;
    Params.fFileType := ioAVI;
    Result := True;
    exit;
  end;

  if ImageFormat > ioUnknown then
  begin
    Result := LoadFromFileFormat(FileName, ImageFormat);
    exit;
  end;

  fpi := IEFileFormatGetInfo2(ex);

  {$ifdef IEINCLUDEDIRECTSHOW}
  // IEFileFormatGetInfo2 does not include video files so check if it is one
  if (fpi = nil) and IEFileExtInExtensions(ex, Supported_MPEG_File_Extensions + Supported_WMV_File_Extensions) then
  begin
    OpenMediaFile(FileName);
    if fAborting then
      Exit;
    LoadFromMediaFile(fParams.fImageIndex);
    CloseMediaFile;
    Params.fFileName := FileName;

    if Lowercase(ex) = '.wmv' then
      Params.fFileType := ioWMV
    else
      Params.fFileType := ioMPEG;
    Result := True;
    Exit;
  end;
  {$endif}

  try
    if fpi = nil then
    begin
      fAborting := True;
      DoFinishWork;
      exit; // extension not recognized
    end;

    if fpi.InternalFormat then
      case fpi.FileType of
        ioJPEG  : begin Result := LoadFromFileJpeg(FileName); exit; end;
        {$IFDEF IEINCLUDEJPEG2000}
        ioJP2   : begin Result := LoadFromFileJP2(FileName); exit; end;
        ioJ2K   : begin Result := LoadFromFileJ2K(FileName); exit; end;
        {$ENDIF}
        {$IFDEF IEINCLUDEPNG}
        ioPNG   : begin Result := LoadFromFilePNG(FileName); exit; end;
        {$ENDIF}
        {$ifdef IEINCLUDEDICOM}
        ioDICOM : begin Result := LoadFromFileDICOM(FileName); exit; end;
        {$endif}
        ioTIFF  : begin
                    LoadFromFileTIFF(FileName);
                    Result := Not fAborting;
                    exit;
                  end;
        ioPCX   : begin Result := LoadFromFilePCX(FileName); exit; end;
        ioDCX   : begin Result := LoadFromFileDCX(FileName); exit; end;
        ioGIF   : begin
                    LoadFromFileGIF(FileName); 
                    Result := Not fAborting;
                    exit;
                  end;
        ioWMF,
        ioEMF   : begin Result := ImportMetaFile(FileName, -1, -1, true); exit; end;
        ioBMP   : begin Result := LoadFromFileBMP(FileName); exit; end;
        ioCUR   : begin Result := LoadFromFileCUR(FileName); exit; end;
        ioICO   : begin Result := LoadFromFileICO(FileName); exit; end;
        ioTGA   : begin Result := LoadFromFileTGA(FileName); exit; end;
        ioPXM   : begin Result := LoadFromFilePXM(FileName); exit; end;
        ioWBMP  : begin Result := LoadFromFileWBMP(FileName); exit; end;
        {$ifdef IEINCLUDERAWFORMATS}
        ioRAW   : begin Result := LoadFromFileRAW(FileName); exit; end;
        {$endif}
        {$ifdef IEINCLUDEPSD}
        ioPSD   : begin Result := LoadFromFilePSD(FileName); exit; end;
        {$endif}
        {$ifdef IEINCLUDEWIC}
        ioHDP   : begin Result := LoadFromFileHDP(FileName); exit; end;
        {$endif}
        ioIEV, ioLYR, ioALL: begin Result := LoadViewerFile(FileName, fpi.FileType); exit; end;
      end;

    // try external formats
    Params.fFileName := '';
    Params.fFileType := ioUnknown;
    fParams.ResetInfo;
    if assigned(fpi.ReadFunction) then
      with fpi do
      begin
        // file format supported
        if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
        begin
          TIEIOThread.CreateLoadSaveFileIFRetBool(self, LoadFromFile, FileName, ImageFormat);
          Result := True;
          exit;
        end;
        fAborting := true; // So that fAborting is True if the file is not found/accessible
        Progress.Aborting := @fAborting;
        if not MakeConsistentBitmap([]) then
          exit;
        fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
        fAborting := false;
        Progress.fOnProgress := fOnIntProgress;
        Progress.Sender := Self;
        fParams.fFileName := FileName;
        fParams.fFileType := FileType;
        try
          ReadFunction(fs, fIEBitmap, fParams, Progress, False);
        except
          fAborting := True;
        end;
        if not fAborting then
        begin
          if fAutoAdjustDPI then
            AdjustDPI;
          SetViewerDPI(fParams.DpiX, fParams.DpiY);
        end;
        Result := Not fAborting;
        update;
        FreeAndNil(fs);
        DoFinishWork;
        exit;
      end;

  finally
    // FILE FAILED TO LOAD? TRY GETTING FORMAT FROM CONTENT
    if (Result = False) and (ImageFormat = IO_CHECK_UNKNOWN) then
    begin
      // Get file format from content
      ff := FindFileFormat(FileName, false);
      if (fpi = nil) or (ff <> fpi.FileType) then
      begin
        // File extension is incorrect for the file type!!!
        fAborting := False;
        Result := LoadFromFileFormat( FileName, ff );
      end;
    end;
  end;

  // FAILED?
  if Result = False then
  begin
    fAborting := True;
    DoFinishWork;
  end;
end;

function TImageEnIO.LoadFromFile(const FileName: WideString; bCheckUnknown: Boolean): Boolean;
begin
  Result := LoadFromFile(FileName, IO_CHECK_UNKNOWN);
end;

{!!
<FS>TImageEnIO.LoadFromFileFormat

<FM>Declaration<FC>
function LoadFromFileFormat(const FileName: WideString; FileFormat: <A TIOFileType>): Boolean;

<FM>Description<FN>
Loads an image from file detecting the file format from FileFormat parameter.

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not a recognized file type (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

Note: This method can load LYR (<A TImageEnView> layers), IEV (<A TImageEnVect> objects) and LYR+IEV formats when <A TImageEnIO.AttachedImageEn> is TImageEnView or TImageEnVect.

<FM>Example<FC>
// load ximage.dat as a Jpeg file
ImageEnView1.IO.LoadFromFileFormat('C:\ximage.dat', ioJPEG);

// load layers and vectorial objects
if ImageEnVect1.IO.LoadFromFileFormat('C:\file.all', ioALL) = False then
  ShowMessage('Not an ALL file!');

<FM>See Also<FN>
- <A TImageEnIO.LoadFromFile>
- <A TImageEnIO.LoadFromFileAuto>
!!}
function TImageEnIO.LoadFromFileFormat(const FileName: WideString; FileFormat: TIOFileType): Boolean;
var
  fpi: TIEFileFormatInfo;
  fs: TIEWideFileStream;
  Progress: TProgressRec;
begin
  Result := False;
  fpi := IEFileFormatGetInfo(FileFormat);
  if fpi = nil then
    exit;

  if fpi.InternalFormat then
    case FileFormat of
      ioTIFF  : begin
                  LoadFromFileTIFF(FileName);
                  Result := Not fAborting;
                end;
      ioGIF   : begin
                  LoadFromFileGIF(FileName);        
                  Result := Not fAborting;
                end;
      ioJPEG  : Result := LoadFromFileJpeg(FileName);
      ioPCX   : Result := LoadFromFilePCX(FileName);
      ioDCX   : Result := LoadFromFileDCX(FileName);
      ioBMP   : Result := LoadFromFileBMP(FileName);
      ioICO   : Result := LoadFromFileICO(FileName);
      ioCUR   : Result := LoadFromFileCUR(FileName);
  {$IFDEF IEINCLUDEPNG}
      ioPNG   : Result := LoadFromFilePNG(FileName);
  {$ENDIF}
  {$ifdef IEINCLUDEDICOM}
      ioDICOM : Result := LoadFromFileDICOM(FileName);
  {$endif}
      ioWMF,
      ioEMF   : Result := ImportMetaFile(FileName, -1, -1, true);
      ioTGA   : Result := LoadFromFileTGA(FileName);
      ioPXM   : Result := LoadFromFilePXM(FileName);
      ioWBMP  : Result := LoadFromFIleWBMP(FileName);
  {$IFDEF IEINCLUDEJPEG2000}
      ioJP2   : Result := LoadFromFileJP2(FileName);
      ioJ2K   : Result := LoadFromFileJ2K(FileName);
  {$ENDIF}
  {$ifdef IEINCLUDERAWFORMATS}
      ioRAW   : Result := LoadFromFileRAW(FileName);
  {$endif}
      ioBMPRAW: Result := LoadFromFileBMPRAW(FileName);
      {$ifdef IEINCLUDEPSD}
      ioPSD   : Result := LoadFromFilePSD(FileName);
      {$endif}
      {$ifdef IEINCLUDEWIC}
      ioHDP   : Result := LoadFromFileHDP(FileName);
      {$endif}
      ioIEV,
      ioLYR,
      ioALL: begin Result := LoadViewerFile(FileName, FileFormat); exit; end;
      else
        Result := LoadFromFile(FileName);
    end

  else
    begin
      fParams.ResetInfo;
      Params.fFileName := '';
      Params.fFileType := ioUnknown;
      // try registered file formats
      if assigned(fpi.ReadFunction) then
        with fpi do
        begin
          // file format supported
          if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
          begin
            TIEIOThread.CreateLoadSaveFileFFRetBool(self, LoadFromFileFormat, FileName, FileFormat);
            exit;
          end;
          try
            fAborting := true; // So that fAborting is True if the file is not found/accessible
            Progress.Aborting := @fAborting;
            if not MakeConsistentBitmap([]) then
              exit;
            fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
            fAborting := false;
            try
              Progress.fOnProgress := fOnIntProgress;
              Progress.Sender := Self;
              fParams.fFileName := FileName;
              fParams.fFileType := FileType;
              ReadFunction(fs, fIEBitmap, fParams, Progress, False);
              if fAutoAdjustDPI then
                AdjustDPI;
              SetViewerDPI(fParams.DpiX, fParams.DpiY);
              update;
              Result := Not fAborting;
            finally
              FreeAndNil(fs);
            end;
          finally
            DoFinishWork;
          end;
        end
      else
      begin
        fAborting := True;
        DoFinishWork;
      end;
    end;
end;



{!!
<FS>TImageEnIO.LoadThumbnailFromExplorer

<FM>Declaration<FC>
function LoadThumbnailFromExplorer(const FileName : WideString; iDesiredWidth : Integer = 120; iDesiredHeight : Integer = 120): Boolean;

<FM>Description<FN>
Retrieve the thumbnail for the specified file from Windows Explorer. This should work for any format that displays a thumbnail in Windows Explorer, including images and videos.
Use <FC>iDesiredWidth<FN> and <FC>iDesiredHeight<FN> to specify the size of the thumbnail that you require.

The <A TImageEnIO.Aborting> property will be true  if the load fails (e.g. if there is not a thumbnail for this file type).


<FM>Example<FC>
// Load a thumbnail for a video
if ImageEnView1.IO.LoadThumbnailFromExplorer('D:\MyVideo.wmv') = False then
  ShowMessage('Load Error!');

!!}
{$IFDEF VIDEO_THUMBNAILS}
function TImageEnIO.LoadThumbnailFromExplorer(const FileName : WideString; iDesiredWidth : Integer = 120; iDesiredHeight : Integer = 120): Boolean;
var
  ABitmap : TBitmap;
  ex: string;
begin
  Result := False;
  if not assigned(fIEBitmap) then
    exit;
            
  ABitmap := TBitmap.create;
  try
    if FileName = '' then
    begin
      fAborting := True;
      exit;
    end;

    fAborting := False;
    if (ExtractExplorerThumbnail(FileName, ABitmap, iDesiredWidth, iDesiredHeight) = False) or (ABitmap.Width < 10) then
    begin
      fAborting := True;
      exit;
    end;
                   
    fIEBitmap.CopyFromTBitmap(ABitmap);
    fParams.ResetInfo;
    Params.fFileName := FileName;
    Params.fFileType := FindFileFormat(FileName, false);

    if Params.fFileType = ioUnknown then
    begin
      ex := string(IEExtractFileExtW(FileName));
      if IEFileExtInExtensions(ex, Supported_WMV_File_Extensions) then
        Params.fFileType := ioWMV
      else

      if IEFileExtInExtensions(ex, Supported_MPEG_File_Extensions) then
        Params.fFileType := ioMPEG;
    end;
                 
    Result := Not fAborting;
  finally
    ABitmap.Free;
    DoFinishWork;
  end;
end;
{$ENDIF}

procedure TImageEnIO.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = fImageEnView) and (Operation = opRemove) then
  begin
    fImageEnView.RemoveBitmapChangeEvent(fImageEnViewBitmapChangeHandle);
    fImageEnView := nil;
  end;
  if (AComponent = fTImage) and (Operation = opRemove) then
    fTImage := nil;
end;

procedure TImageEnIO.UpdateAPP138BimResolution;
type
  TPSDResolutionInfo=packed record
    hRes: longint;       // fixed point number: pixels per inch
    hResUnit: word;      // 1=pixels per inch, 2=pixels per centimeter
    WidthUnit: word;     // 1=in, 2=cm, 3=pt, 4=picas, 5=columns
    vRes: longint;       // fixed point number: pixels per inch
    vResUnit: word;      // 1=pixels per inch, 2=pixels per centimeter
    HeightUnit: word;    // 1=in, 2=cm, 3=pt, 4=picas, 5=columns
  end;
  PPSDResolutionInfo=^TPSDResolutionInfo;
var
  idx: Integer;
  data: PAnsiChar;
  len: Integer;
  p: Integer;
  q: Integer;
  ri: PPSDResolutionInfo;
begin
  idx := fParams.JPEG_MarkerList.IndexOf( JPEG_APP13 );
  if idx > - 1 then
  begin
    data := fParams.JPEG_MarkerList.MarkerData[idx];
    len := fParams.JPEG_MarkerList.MarkerLength[idx];
    for p := 0 to len - 1 do
    begin
      if CompareMem(@data[p], PAnsiChar('8BIM'#3#237), 6) then // 0x03 0xED (resolution identifier)
      begin
        inc(data, p+6);
        q := IESwapWord(pword(data)^);  // name length
        inc(data, 2);
        inc(data, q);
        q := IESwapDWord(pdword(data)^);  // structure length
        inc(data, 4);
        if q>=sizeof(TPSDResolutionInfo) then
        begin
          ri := PPSDResolutionInfo(data);
          ri^.hRes := IESwapDWord(fParams.DpiX*65536);
          ri^.vRes := IESwapDWord(fParams.DpiY*65536);
          ri^.hResUnit := IESwapWord(1);    // 1=pixels per inch
          ri^.vResUnit := IESwapWord(1);    // 1=pixels per inch
          ri^.WidthUnit := IESwapWord(1);   // 1=in
          ri^.HeightUnit := IESwapWord(1);  // 1=in
        end;
          break;
      end;
    end;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromFileJpeg

<FM>Declaration<FC>
function LoadFromFileJpeg(const FileName: WideString): Boolean;

<FM>Description<FN>
Loads an image from a JPEG file. Result will be false if the file is not JPEG format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

<FC>FileName<FN> is the file name including extension.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFileJpeg('C:\alfa.jpg');

!!}
function TImageEnIO.LoadFromFileJpeg(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
  Progress: TProgressRec;
  XStream: TIEBufferedReadStream;
begin     
  Result := False;
  
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileJpeg, FileName);
    Result := True;
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    XStream := TIEBufferedReadStream.Create(fs, 65536);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      fIEBitmap.RemoveAlphaChannel;
      ReadJpegStream(XStream, nil, fIEBitmap, fParams, Progress, False, false, true, false, true, true, -1, fParams.IsNativePixelFormat);
      CheckDPI;
      if fAutoAdjustDPI then
        AdjustDPI;
      fParams.fFileName := FileName;
      fParams.fFileType := ioJPEG;
      SetViewerDPI(fParams.DpiX, fParams.DpiY);
      Update;
    finally
      FreeAndNil(XStream);
      FreeAndNil(fs);    
      Result := Not fAborting;
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStream

<FM>Declaration<FC>
procedure SaveToStream(Stream: TStream; FileType: <A TIOFileType>);

<FM>Description<FN>
SaveToStream saves the image to a stream by calling FileType to specify the file format.

If <A TImageEnIO.StreamHeaders> property is True adds an additional special header, needed for multi-image streams.

!!}
procedure TImageEnIO.SaveToStream(Stream: TStream; FileType: TIOFileType);
var
  fpi: TIEFileFormatInfo;
  Progress: TProgressRec;
begin
  case FileType of
    ioTIFF: SaveToStreamTIFF(Stream);
    ioGIF: SaveToStreamGIF(Stream);
    ioJPEG: SaveToStreamJPEG(Stream);
{$IFDEF IEINCLUDEJPEG2000}
    ioJP2: SaveToStreamJP2(Stream);
    ioJ2K: SaveToStreamJ2K(Stream);
{$ENDIF}
    ioPCX: SaveToStreamPCX(Stream);
    ioDCX: SaveToStreamDCX(Stream);
    ioBMP: SaveToStreamBMP(Stream);
{$IFDEF IEINCLUDEPNG}
    ioPNG: SaveToStreamPNG(Stream);
{$ENDIF}
    ioTGA: SaveToStreamTGA(Stream);
    ioPXM: SaveToStreamPXM(Stream);
    ioICO: SaveToStreamICO(Stream);
    ioWBMP: SaveToStreamWBMP(Stream);
    ioPS: SaveToStreamPS(Stream);
    {$ifdef IEINCLUDEPDFWRITING}
    ioPDF: SaveToStreamPDF(Stream);
    {$endif}
    {$ifdef IEINCLUDEPSD}
    ioPSD: SaveToStreamPSD(Stream);
    {$endif}
    {$ifdef IEINCLUDEWIC}
    ioHDP: SaveToStreamHDP(Stream);
    {$endif}
    {$ifdef IEINCLUDEDICOM}
    ioDICOM: SaveToStreamDICOM(Stream);
    {$endif}
  else
    begin
      if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
      begin
        TIEIOThread.CreateLoadSaveStreamFF(self, SaveToStream, Stream, FileType);
        exit;
      end;
      fpi := IEFileFormatGetInfo(FileType);
      if assigned(fpi) and assigned(fpi.WriteFunction) then
        with fpi do
        begin
          try
            fAborting := false;
            Progress.Aborting := @fAborting;
            if not MakeConsistentBitmap([]) then
              exit;
            if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
              fIEBitmap.PixelFormat := ie24RGB;
            Progress.fOnProgress := fOnIntProgress;
            Progress.Sender := Self;
            fParams.fFileType := FileType;
            WriteFunction(Stream, fIEBitmap, fParams, Progress);
          finally
            DoFinishWork;
          end;
        end
      else
        fAborting := True;
    end;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromBuffer

<FM>Declaration<FC>
procedure LoadFromBuffer(Buffer: Pointer; BufferSize: Integer; Format: <A TIOFileType> = ioUnknown);

<FM>Description<FN>
Loads an image from the specified buffer.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Buffer<FN></C> <C>The buffer pointer</C> </R>
<R> <C><FC>BufferSize<FN></C> <C>The buffer length in bytes.</C> </R>
<R> <C><FC>Format<FN></C> <C>Specifies the expected file format. If Format is ioUnknown, then try to find the format automatically</C> </R>
</TABLE>

See also: <A TImageEnIO.ParamsFromBuffer>

<FM>Example<FC>
ImageEnView1.IO.LoadFromBuffer(mybuffer, mybufferlength, ioJPEG);
!!}
procedure TImageEnIO.LoadFromBuffer(Buffer: Pointer; BufferSize: Integer; Format: TIOFileType);
var
  stream: TIEMemStream;
begin
  if (Buffer = nil) or (BufferSize = 0) then
  begin
    fAborting := true;
    exit;
  end;

  stream := TIEMemStream.Create(Buffer, BufferSize);
  try
    if Format = ioUnknown then
      LoadFromStream(stream)
    else
      LoadFromStreamFormat(stream, Format);
  finally
    FreeAndNil(stream);
  end;

end;


{!!
<FS>TImageEnIO.ParamsFromBuffer

<FM>Declaration<FC>
procedure ParamsFromBuffer(Buffer: Pointer; BufferSize: Integer; Format: <A TIOFileType> = ioUnknown);

<FM>Description<FN>
Loads image parameters (but not the actual image) from the specified buffer.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Buffer<FN></C> <C>The buffer pointer</C> </R>
<R> <C><FC>BufferSize<FN></C> <C>The buffer length in bytes.</C> </R>
<R> <C><FC>Format<FN></C> <C>Specifies the expected file format. If Format is ioUnknown, then try to find the format automatically</C> </R>
</TABLE>

See also: <A TImageEnIO.LoadFromBuffer>

<FM>Example<FC>
ImageEnView1.IO.ParamsFromBuffer(mybuffer, mybufferlength, ioJPEG);
!!}
procedure TImageEnIO.ParamsFromBuffer(Buffer: Pointer; BufferSize: Integer; Format: TIOFileType);
var
  stream: TIEMemStream;
begin
  if (Buffer = nil) or (BufferSize = 0) then
  begin
    fAborting := true;
    exit;
  end;

  stream := TIEMemStream.Create(Buffer, BufferSize);
  try
    if Format = ioUnknown then
      ParamsFromStream(stream)
    else
      ParamsFromStreamFormat(stream, Format);
  finally
    FreeAndNil(stream);
  end;

end;


(*
ietfPascal:
  const
    'FileName_Extension_Size' = nnnn;
    'FileName_Extension' : array [0..'FileName_Extension_Size' - 1] of byte = ( $xx,$xx );
ietfHEX
  0xaa,0xbb,etc
ietfBase64
  /9j/4AAQSkZJRgABAQEASABIAAD/2wBDAAYEBQYF....
*)

{!!
<FS>TImageEnIO.SaveToText

<FM>Declaration<FC>
procedure SaveToText(Stream: TStream; ImageFormat: <A TIOFileType>; TextFormat: <A TIETextFormat> = ietfASCIIArt);
procedure SaveToText(const FileName: WideString; ImageFormat: <A TIOFileType>; TextFormat: <A TIETextFormat> = ietfASCIIArt);

<FM>Description<FN>
Saves the current image in the specified text format.

Parameters:

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>FileName<FN></C> <C>Output file name.</C> </R>
<R> <C><FC>Stream<FN></C> <C>Output stream.</C> </R>
<R> <C><FC>ImageFormat<FN></C> <C>Wanted file format.</C> </R>
<R> <C><FC>TextFormat<FN></C> <C>Output text format. See allowed values in <A TIETextFormat>.</C> </R>
</TABLE>

See Also: <A TImageEnIO.LoadFromText>


<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
ImageEnView1.Proc.Resample(128, -1);
ImageEnView1.Io.SaveToText('D:\output.txt', ioUnknown, ietfASCIIArt);
!!}
procedure TImageEnIO.SaveToText(const FileName: WideString; ImageFormat: TIOFileType; TextFormat: TIETextFormat);
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmCreate);
  try
    fParams.FileName := FileName;
    SaveToText(fs, ImageFormat, TextFormat);
  finally
    fs.Free();
  end;
end;

procedure TImageEnIO.SaveToText(Stream: TStream; ImageFormat: TIOFileType; TextFormat: TIETextFormat);
const
  GR: array [0..9] of AnsiChar = 'W@#*+:.,  ';
var
  ss, s1: AnsiString;
  i, j, v: Integer;
  blen: Integer;
  ms: TMemoryStream;
  b: byte;
  px: PRGB;
  RedToGrayCoef, GreenToGrayCoef, BlueToGrayCoef: integer;

  procedure PrepareStream();
  begin
    ss := AnsiString(fParams.FileName);
    i := IEPos('.', ss);
    if i > 0 then
      ss[i] := '_';
    s1 := ss + '_Size';

    ms := TMemoryStream.Create();
    SaveToStream(ms, ImageFormat);
    ms.Position := 0;
    blen := ms.Size;
  end;

begin

  ms := nil;

  try

    case TextFormat of

      ietfPascal:
        begin
          PrepareStream();
          IEStreamWriteLn(Stream, 'const' );
          IEStreamWriteLn(Stream, '  ' + s1 + ' = ' + IEIntToStr(blen) + ';');
          IEStreamWriteLn(Stream, '  ' + ss + ' : array [0..' + s1 + ' - 1] of byte = (' );
          ss := '';
          for i := 0 to blen - 2 do
          begin
            ms.Read(b, 1);
            ss := ss + '$' + IEIntToHex(b, 2) + ',';
            if length(ss) > 70 then
            begin
              IEStreamWriteLn(Stream, '    ' + ss);
              ss := '';
            end;
          end;
          ms.Read(b, 1);
          IEStreamWriteLn(Stream, '    ' + ss + '$' + IEIntToHex(b, 2) + ');');
        end;

      ietfHEX:
        begin
          PrepareStream();
          IEStreamWriteLn(Stream, '\\ Size=' + IEIntToStr(blen));
          ss := '';
          for i := 0 to blen - 2 do
          begin
            ms.Read(b, 1);
            ss := ss + '0x' + IEIntToHex(b, 2) + ',';
            if length(ss) > 70 then
            begin
              IEStreamWriteLn(Stream, ss);
              ss := '';
            end;
          end;
          ms.Read(b, 1);
          IEStreamWriteLn(Stream, ss + '0x' + IEIntToHex(b, 2));
        end;

      ietfBase64:
        begin
          PrepareStream();
          IEEncode64(ms, Stream, 76);
        end;

      ietfASCIIArt:
        begin
          if not MakeConsistentBitmap([ie24RGB]) then
            exit;
          RedToGrayCoef   := IEGlobalSettings().RedToGrayCoef;
          GreenToGrayCoef := IEGlobalSettings().GreenToGrayCoef;
          BlueToGrayCoef  := IEGlobalSettings().BlueToGrayCoef;
          for i := 0 to fIEBitmap.Height - 1 do
          begin
            ss := '';
            px := fIEBitmap.Scanline[i];
            for j := 0 to fIEBitmap.Width - 1 do
            begin
              with px^ do
                v := (r * RedToGrayCoef + g * GreenToGrayCoef + b * BlueToGrayCoef) div 100;
              v := Trunc((v / 255) * 9);
              ss := ss + GR[v];
              inc(px);
            end;
            IEStreamWriteLn(Stream, ss);
          end;
        end;

    end;

  finally
    ms.Free();
  end;
end;



{!!
<FS>TImageEnIO.LoadFromText

<FM>Declaration<FC>
procedure LoadFromText(const FileName: WideString; TextFormat: <A TIETextFormat> = ietfBase64);
procedure LoadFromText(Stream: TStream; TextFormat: <A TIETextFormat> = ietfBase64);

<FM>Description<FN>
Loads image from text.

Parameters:

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>FileName<FN></C> <C>Input file name.</C> </R>
<R> <C><FC>Stream<FN></C> <C>Input stream.</C> </R>
<R> <C><FC>TextFormat<FN></C> <C>Input text format. Only ietfBase64 is currently supported.</C> </R>
</TABLE>

See Also: <A TImageEnIO.SaveToText>


<FM>Example<FC>
// save as base64 (inside there is a jpeg)
ImageEnView1.IO.LoadFromFile('image.jpg');
ImageEnView1.IO.SaveToText('image.base64', ioJpeg, ietfBase64);

// reload back
ImageEnView1.IO.LoadFromText('image.base64', ietfBase64);
!!}
procedure TImageEnIO.LoadFromText(const FileName: WideString; TextFormat: TIETextFormat);
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromText(fs, TextFormat);
  finally
    fs.Free();
  end;
end;

procedure TImageEnIO.LoadFromText(Stream: TStream; TextFormat: TIETextFormat);
var
  ms: TMemoryStream;
begin
  case TextFormat of
    ietfPascal,
    ietfHex,
    ietfASCIIArt:
      raise Exception.create('Unsupported text format');
    ietfBase64:
      begin
        ms := TMemoryStream.Create();
        try
          IEDecode64(Stream, ms);
          ms.Position := 0;
          LoadFromStream(ms);
        finally
          ms.Free();
        end;
      end;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStream

<FM>Declaration<FC>
function LoadFromStream(Stream: TStream): Boolean;

<FM>Description<FN>
Loads an image from a stream by calling <A FindStreamFormat> to detect the file format. The result will be false if an error is encountered, e.g. the file in the stream is not a recognized format (<A TImageEnIO.Aborting> will be true).

If the <A TImageEnIO.StreamHeaders> property is True, the stream must have a special header (saved with <A TImageEnIO.SaveToStream>).

Note: This method can load LYR (<A TImageEnView> layers), IEV (<A TImageEnVect> objects) and LYR+IEV formats when <A TImageEnIO.AttachedImageEn> is TImageEnView or TImageEnVect.

!!}
function TImageEnIO.LoadFromStream(Stream: TStream): Boolean;
var
  sf: TIOFileType;
  lp: int64;
  fpi: TIEFileFormatInfo;
  Progress: TProgressRec;
begin
  Result := False;
  lp := Stream.Position;
  sf := FindStreamFormat(Stream);
  fpi := IEFileFormatGetInfo(sf);

  if fpi = nil then
  begin
    fAborting := true;
    exit;
  end;

  Stream.Position := lp;

  if fpi.InternalFormat then
    case sf of
      ioTIFF  : begin
                  LoadFromStreamTIFF(Stream);
                  Result := Not fAborting;
                end;
      ioGIF   : begin
                  LoadFromStreamGIF(Stream);
                  Result := Not fAborting;
                end;
      ioJPEG  : Result := LoadFromStreamJPEG(Stream);
      ioPCX   : Result := LoadFromStreamPCX(Stream);
      ioDCX   : Result := LoadFromStreamDCX(Stream);
      ioBMP   : Result := LoadFromStreamBMP(Stream);
      ioICO   : Result := LoadFromStreamICO(Stream);
      ioCUR   : Result := LoadFromStreamCUR(Stream);
      {$IFDEF IEINCLUDEPNG}
      ioPNG   : Result := LoadFromStreamPNG(Stream);
      {$ENDIF}
      {$ifdef IEINCLUDEDICOM}
      ioDICOM : Result := LoadFromStreamDICOM(Stream);
      {$endif}
      ioTGA   : Result := LoadFromStreamTGA(Stream);
      ioPXM   : Result := LoadFromStreamPXM(Stream);
      {$IFDEF IEINCLUDEJPEG2000}
      ioJP2   : Result := LoadFromStreamJP2(Stream);
      ioJ2K   : Result := LoadFromStreamJ2K(Stream);
      {$ENDIF}
      {$ifdef IEINCLUDERAWFORMATS}
      ioRAW   : Result := LoadFromStreamRAW(Stream);
      {$endif}
      {$ifdef IEINCLUDEPSD}
      ioPSD   : Result := LoadFromStreamPSD(Stream);
      {$endif}
      {$ifdef IEINCLUDEWIC}
      ioHDP   : Result := LoadFromStreamHDP(Stream);
      {$endif}
      ioIEV, ioLYR, ioALL: begin Result := LoadViewerStream(Stream, sf); exit; end;
      ioWMF, ioEMF: Result := ImportMetafile(Stream);
    end
  else
    begin
      // unknown or user registered file formats
      if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
      begin
        TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStream, Stream);
        Result := True;
        exit;
      end;

      fParams.ResetInfo;
      Params.fFileName := '';
      Params.fFileType := ioUnknown;
      if assigned(fpi) and assigned(fpi.ReadFunction) then
        with fpi do
        begin
          // file format supported
          try
            fAborting := false;
            Progress.Aborting := @fAborting;
            if not MakeConsistentBitmap([]) then
              exit;
            Progress.fOnProgress := fOnIntProgress;
            Progress.Sender := Self;
            fParams.fFileType := FileType;
            ReadFunction(Stream, fIEBitmap, fParams, Progress, False);
            if fAutoAdjustDPI then
              AdjustDPI;
            SetViewerDPI(fParams.DpiX, fParams.DpiY);    
            Result := Not fAborting;
            update;
          finally
            DoFinishWork;
          end;
        end
      else
      begin
        fAborting := True;
        DoFinishWork;
      end;
    end;
end;


{!!
<FS>TImageEnIO.LoadFromStreamFormat

<FM>Declaration<FC>
function LoadFromStreamFormat(Stream: TStream; FileFormat: <A TIOFileType>): Boolean;

<FM>Description<FN>
Loads an image from stream, detecting the file format from the <FC>FileFormat<FN> parameter. The result will be false if an error is encountered, e.g. the file in the stream is not a recognized format (<A TImageEnIO.Aborting> will be true).

If <FC>FileFormat<FN> is <FC>ioUnknown<FN>, then <A TImageEnIO.LoadFromStream> is called (auto-detecting of the file format).

Note: This method can also load LYR (<A TImageEnView> layers), IEV (<A TImageEnVect> objects) and LYR+IEV formats when <A TImageEnIO.AttachedImageEn> is TImageEnView or TImageEnVect.

<FM>Example<FC>
// load a jpeg from the stream stm
if ImageEnView1.IO.LoadFromStreamFormat(stm, ioJPEG) = False then
  ShowMessage('Unknown Format!');

!!}
function TImageEnIO.LoadFromStreamFormat(Stream: TStream; FileFormat: TIOFileType): Boolean;
var
  fpi: TIEFileFormatInfo;
  Progress: TProgressRec;
begin           
  Result := False;
  if FileFormat = ioUnknown then
  begin
    Result := LoadFromStream(Stream);
    exit;
  end;

  fpi := IEFileFormatGetInfo(FileFormat);
  if fpi = nil then
    exit;

  if fpi.InternalFormat then
    case FileFormat of
      ioTIFF  : begin
                  LoadFromStreamTIFF(Stream);
                  Result := Not fAborting;
                end;
      ioGIF   : begin
                  LoadFromStreamGIF(Stream);
                  Result := Not fAborting;
                end;
      ioJPEG  : Result := LoadFromStreamJPEG(Stream);
      ioPCX   : Result := LoadFromStreamPCX(Stream);
      ioDCX   : Result := LoadFromStreamDCX(Stream);
      ioBMP   : Result := LoadFromStreamBMP(Stream);
      ioICO   : Result := LoadFromStreamICO(Stream);
      ioCUR   : Result := LoadFromStreamCUR(Stream);
      {$IFDEF IEINCLUDEPNG}
      ioPNG   : Result := LoadFromStreamPNG(Stream);
      {$ENDIF}
      {$ifdef IEINCLUDEDICOM}
      ioDICOM : Result := LoadFromStreamDICOM(Stream);
      {$endif}
      ioTGA   : Result := LoadFromStreamTGA(Stream);
      ioPXM   : Result := LoadFromStreamPXM(Stream);
      ioWBMP  : Result := LoadFromStreamWBMP(Stream);
      {$IFDEF IEINCLUDEJPEG2000}
      ioJP2   : Result := LoadFromStreamJP2(Stream);
      ioJ2K   : Result := LoadFromStreamJ2K(Stream);
      {$ENDIF}
      {$ifdef IEINCLUDERAWFORMATS}
      ioRAW   : Result := LoadFromStreamRAW(Stream);
      {$endif}
      ioBMPRAW: Result := LoadFromStreamBMPRAW(Stream);
      {$ifdef IEINCLUDEPSD}
      ioPSD   : Result := LoadFromStreamPSD(Stream);
      {$endif}
      {$ifdef IEINCLUDEWIC}
      ioHDP   : Result := LoadFromStreamHDP(Stream);
      {$endif}
      ioIEV, ioLYR, ioALL: begin Result := LoadViewerStream(Stream, FileFormat); exit; end;
      ioWMF, ioEMF: Result := LoadFromStream(Stream);
    end
  else
    begin
      // unknown or user registered file formats
      if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
      begin
        TIEIOThread.CreateLoadSaveStreamFTRetBool(self, LoadFromStreamFormat, Stream, FileFormat);
        Result := True;
        exit;
      end;

      fParams.ResetInfo;
      Params.fFileName := '';
      Params.fFileType := ioUnknown;
      if assigned(fpi) and assigned(fpi.ReadFunction) then
        with fpi do
        begin
          // file format supported
          try
            fAborting := false;
            Progress.Aborting := @fAborting;
            if not MakeConsistentBitmap([]) then
              exit;
            Progress.fOnProgress := fOnIntProgress;
            Progress.Sender := Self;
            fParams.fFileType := FileType;
            ReadFunction(Stream, fIEBitmap, fParams, Progress, False);
            if fAutoAdjustDPI then
              AdjustDPI;
            SetViewerDPI(fParams.DpiX, fParams.DpiY);    
            Result := Not fAborting;
            update;
          finally
            DoFinishWork;
          end;
        end
      else
      begin
        fAborting := True;
        DoFinishWork;
      end;
    end;
end;

{!!
<FS>TImageEnIO.SaveToFilePCX

<FM>Declaration<FC>
procedure SaveToFilePCX(const FileName: WideString);

<FM>Description<FN>
Saves the current image to a file in PCX format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
// Saves a 256 color PCX
ImageEnView1.IO.Params.BitsPerSample := 8;
ImageEnView1.IO.Params.SamplesPerPixel := 1;
ImageEnView1.IO.SaveToFilePCX('D:\image.pcx');

!!}
procedure TImageEnIO.SaveToFilePCX(const FileName: WideString);
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
begin                                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFilePCX, FileName);
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      WritePcxStream(fs, fIEBitmap, fParams, Progress);
      fParams.FileName := FileName;
      fParams.FileType := ioPCX;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromFileTGA

<FM>Declaration<FC>
function LoadFromFileTGA(const FileName: WideString): Boolean;

<FM>Description<FN>
Loads an image from a TGA file.

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not TGA format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.
!!}
function TImageEnIO.LoadFromFileTGA(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
  Progress: TProgressRec;
  tmpAlphaChannel: TIEMask;
begin
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileTGA, FileName);
    Result := True;
    exit;
  end;


  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      fIEBitmap.RemoveAlphaChannel;
      tmpAlphaChannel := nil;
      ReadTGAStream(fs, fIEBitmap, fParams, Progress, false, tmpAlphaChannel, false);
      CheckDPI;
      if assigned(tmpAlphaChannel) then
      begin
        fIEBitmap.AlphaChannel.CopyFromTIEMask(tmpAlphaChannel);
        FreeAndNil(tmpAlphaChannel);
      end;
      if fAutoAdjustDPI then
        AdjustDPI;
      fParams.fFileName := FileName;
      fParams.fFileType := ioTGA;
      SetViewerDPI(fParams.DpiX, fParams.DpiY);
      update;
    finally
      FreeAndNil(fs);
      Result := Not fAborting;
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamTGA

<FM>Declaration<FC>
function LoadFromStreamTGA(Stream: TStream): Boolean;

<FM>Description<FN>   
Loads an image from a stream containing a TGA file. The result will be false if an error is encountered, e.g. the file in the stream is not TGA format (<A TImageEnIO.Aborting> will be true).
!!}
function TImageEnIO.LoadFromStreamTGA(Stream: TStream): Boolean;
var
  Progress: TProgressRec;
  tmpAlphaChannel: TIEMask;
begin
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamTGA, Stream);
    Result := True;
    exit;
  end;
  

  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    tmpAlphaChannel := nil;
    ReadTGAStream(Stream, fIEBitmap, fParams, Progress, false, tmpAlphaChannel, false);
    CheckDPI;
    if assigned(tmpAlphaChannel) then
    begin
      fIEBitmap.AlphaChannel.CopyFromTIEMask(tmpAlphaChannel);
      FreeAndNil(tmpAlphaChannel);
    end;
    if fAutoAdjustDPI then
      AdjustDPI;
    fParams.fFileName := '';
    fParams.fFileType := ioTGA;
    update;
    Result := Not fAborting;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SavetoFileTGA

<FM>Declaration<FC>
procedure SaveToFileTGA(const FileName: WideString);

<FM>Description<FN>
Saves the current image to a file in TGA format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

!!}
procedure TImageEnIO.SaveToFileTGA(const FileName: WideString);
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
  iemask: TIEMask;
begin          
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileTGA, FileName);
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      if fIEBitmap.HasAlphaChannel then
      begin
        iemask := TIEMask.Create;
        fIEBitmap.AlphaChannel.CopyToTIEMask(iemask);
        WriteTGAStream(fs, fIEBitmap, fParams, Progress, iemask);
        FreeAndNil(iemask);
      end
      else
        WriteTGAStream(fs, fIEBitmap, fParams, Progress, nil);
      fParams.FileName := FileName;
      fParams.FileType := ioTGA;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamTGA

<FM>Declaration<FC>
procedure SaveToStreamTGA(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in TGA format.

Stream is the image stream.
!!}
procedure TImageEnIO.SaveToStreamTGA(Stream: TStream);
var
  Progress: TProgressRec;
  iemask: TIEMask;
begin         
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamTGA, Stream);
    exit;
  end;

  
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    if fIEBitmap.HasAlphaChannel then
    begin
      iemask := TIEMask.Create;
      fIEBitmap.AlphaChannel.CopyToTIEMask(iemask);
      WriteTGAStream(Stream, fIEBitmap, fParams, Progress, iemask);
      FreeAndNil(iemask);
    end
    else
      WriteTGAStream(Stream, fIEBitmap, fParams, Progress, nil);
  finally
    DoFinishWork;
  end;
end;

procedure TImageEnIO.SyncSaveToStreamBMP(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    BMPWriteStream(Stream, fIEBitmap, fParams, Progress, fParams.BMP_HandleTransparency);
  finally
    DoFinishWork;
  end;
end;

procedure TImageEnIO.SyncSaveToStreamDCX(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    IEDCXInsertStream(Stream, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamDCX

<FM>Declaration<FC>
procedure SaveToStreamDCX(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in DCX format.

!!}
procedure TImageEnIO.SaveToStreamDCX(Stream: TStream);
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    // ASYNC SAVING
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamDCX, Stream);
  end
  else
  begin
    Stream.Size := 0; // this avoid that this function inserts
    SyncSaveToStreamDCX(Stream);
  end;
end;

{!!
<FS>TImageEnIO.SaveToFileDCX

<FM>Declaration<FC>
procedure SaveToFileDCX(const FileName: WideString);

<FM>Description<FN>
Saves the current image to a file in DCX format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
procedure TImageEnIO.SaveToFileDCX(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin              
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileDCX, FileName);
    exit;
  end;


  fs := nil;
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    SyncSaveToStreamDCX(fs);
    fParams.FileName := FileName;
    fParams.FileType := ioDCX;
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnIO.InsertToFileDCX

<FM>Declaration<FC>
procedure InsertToFileDCX(const FileName: WideString);

<FM>Description<FN>
Inserts the current image into the specified DCX file, at the position specified by <A TImageEnIO.Params>.<A TIOParamsVals.DCX_ImageIndex>.

<FM>Example<FC>
ImageEnView1.IO.Params.DCX_ImageIndex := 1;
ImageEnView1.IO.InsertToFileDCX('D:\multipage.dcx');

!!}
procedure TImageEnIO.InsertToFileDCX(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin        
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, InsertToFileDCX, FileName);
    exit;
  end;

  
  fs := nil;
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmOpenReadWrite);
    SyncSaveToStreamDCX(fs);
  finally
    FreeAndNil(fs);
  end;
end;


{!!
<FS>TImageEnIO.SaveToStreamBMP

<FM>Declaration<FC>
procedure SaveToStreamBMP(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in BMP format.

If <A TImageEnIO.StreamHeaders> property is True, it adds an additional special header as needed for multi-image streams.
                      
<FM>Example<FC>
// Saves ImageEnView1 and ImageEnView2 images in file images.dat
// images.dat isn't loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('bmpimages.dat', fmCreate);
  ImageEnView1.IO.StreamHeaders := True;
  ImageEnView1.IO.SaveToStreamBMP(fs);
  ImageEnView2.IO.StreamHeaders := True;
  ImageEnView2.IO.SaveToStreamBMP(fs);
  fs.free;
End;

// Saves a single image in image.bmp
// image.bmp is loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('image.bmp');
  ImageEnView1.IO.StreamHeaders := False;
  ImageEnView1.IO.SaveToFileBMP(fs);
End;

!!}
procedure TImageEnIO.SaveToStreamBMP(Stream: TStream);
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin         
    // ASYNC SAVING
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamBMP, Stream);
  end
  else
  begin
    SyncSaveToStreamBMP(Stream);
  end;
end;

{!!
<FS>TImageEnIO.SaveToFileBMP

<FM>Declaration<FC>
procedure SaveToFileBMP(const FileName: WideString);

<FM>Description<FN>
Saves the current image to a file in BMP format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
// Saves a 256 color compressed bitmap bitmap
ImageEnView1.IO.Params.BitsPerSample := 8;
ImageEnView1.IO.Params.SamplesPerPixel := 1;
ImageEnView1.IO.Params.BMP_Compression := ioBMP_RLE;
ImageEnView1.IO.SaveToFile('D:\Italy.bmp');
!!}
procedure TImageEnIO.SaveToFileBMP(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin                                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileBMP, FileName);
    exit;
  end;

  
  fs := nil;
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    SyncSaveToStreamBMP(fs);
    fParams.FileName := FileName;
    fParams.FileType := ioBMP;
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnIO.SaveToFileTIFF

<FM>Declaration<FC>
procedure SaveToFileTIFF(const FileName: WideString);

<FM>Description<FN>    
Saves the current image to a file in TIFF format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
ImageEnView1.IO.Params.DocumentName := 'My document';
ImageEnView1.IO.SaveToFileTIFF('C:\image.tiff');

!!}
procedure TImageEnIO.SaveToFileTIFF(const FileName: WideString);
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
begin                      
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileTIFF, FileName);
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      TIFFWriteStream(fs, false, fIEBitmap, fParams, Progress);
      fParams.FileName := FileName;
      fParams.FileType := ioTIFF;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamTIFF

<FM>Declaration<FC>
procedure SaveToStreamTIFF(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in TIFF format.

If <A TImageEnIO.StreamHeaders> property is True, it adds an additional special header as needed for multi-image streams.

<FM>Example<FC>
// Saves ImageEnView1 and ImageEnView2 attached images in file images.dat
// images.dat isn't loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('bmpimages.dat', fmCreate);
  ImageEnView1.IO.StreamHeaders := True;
  ImageEnView1.IO.SaveToStreamTIFF(fs);
  ImageEnView2.IO.StreamHeaders := True;
  ImageEnView2.IO.SaveToStreamTIFF(fs);
  fs.free;
End;

// Saves a single image in image.tif
// image.tif is loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('image.tif');
  ImageEnView1.IO.StreamHeaders := False;
  ImageEnView1.IO.SaveToFileTIFF(fs);
End;

!!}
procedure TImageEnIO.SaveToStreamTIFF(Stream: TStream);
var
  SHead: TIFFSHead;
  lp1, lp2: int64;
  Progress: TProgressRec;
begin                                
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamTIFF, Stream);
    exit;
  end;


  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    lp1 := 0;
    if fStreamHeaders then
    begin
      lp1 := Stream.position;
      Stream.write(SHead, sizeof(SHead)); // create space for TIFF stream header
    end;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    TIFFWriteStream(Stream, false, fIEBitmap, fParams, Progress);
    if fStreamHeaders then
    begin
      // salve HEADER-TIFF-STREAM
      lp2 := Stream.position;
      Stream.position := lp1;
      SHead.id := 'TIFF';
      SHead.dim := lp2 - lp1 - sizeof(SHead);
      Stream.Write(SHead, sizeof(SHead));
      Stream.position := lp2;
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamPCX

<FM>Declaration<FC>
procedure SaveToStreamPCX(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in PCX format.

If <A TImageEnIO.StreamHeaders> property is True, it adds an additional special header as needed for multi-image streams.

<FM>Example<FC>
// Saves ImageEnView1 and ImageEnView2 attached images in file images.dat
// images.dat isn't loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('bmpimages.dat', fmCreate);
  ImageEnView1.IO.StreamHeaders := True;
  ImageEnView1.IO.SaveToStreamPCX(fs);
  ImageEnView2.IO.StreamHeaders := True;
  ImageEnView2.IO.SaveToStreamPCX(fs);
  fs.free;
End;

// Saves a single image in image.pcx
// image.pcx is loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('image.pcx');
  ImageEnView1.IO.StreamHeaders := False;
  ImageEnView1.IO.SaveToFilePCX(fs);
End;

!!}
procedure TImageEnIO.SaveToStreamPCX(Stream: TStream);
var
  SHead: PCXSHead;
  lp1, lp2: int64;
  Progress: TProgressRec;
begin                   
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamPCX, Stream);
    exit;
  end;


  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    lp1 := 0;
    if fStreamHeaders then
    begin
      lp1 := Stream.position;
      Stream.write(SHead, sizeof(SHead)); // lascia spazio per header PCX stream
    end;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    WritePcxStream(Stream, fIEBitmap, fParams, Progress);
    if fStreamHeaders and not fAborting then
    begin
      // salva HEADER-PCX-STREAM
      lp2 := Stream.position;
      Stream.position := lp1;
      SHead.id := 'PCX2'; // PCX2 è PCX con dimensione
      SHead.dim := lp2 - lp1 - sizeof(SHead);
      Stream.Write(SHead, sizeof(SHead));
      Stream.position := lp2;
    end;
  finally
    DoFinishWork;
  end;
end;

// do parameters previews
// return True if user press OK
{$IFDEF IEINCLUDEDIALOGIO}

{!!
<FS>TImageEnIO.DoPreviews

<FM>Declaration<FC>
function DoPreviews(pp: <A TPreviewParams>): boolean;

<FM>Description<FN>
Executes the Previews dialog allowing the user to review and modify the parameters of image file formats (for example, JPEG quality, TIFF compression, etc).

<FC>pp<FN> is the set of image formats parameters to show in the dialog.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\myimage.gif');  // loads a GIF image
ImageEnView1.IO.DoPreviews([ppGIF]); // sets GIF parameters (background, transparency...)
ImageEnView1.IO.SaveToFile('D:\newimage.gif'); // saves some image with new parameters
!!}
function TImageEnIO.DoPreviews(pp: TPreviewParams): boolean;
var
  fIOPreviews: TfIOPreviews;
  Handled: boolean;
begin
  Handled := false;
  if assigned(fOnDoPreviews) then
    fOnDoPreviews(self, Handled);
  if not Handled then
  begin
    result := false;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;

    fIOPreviews := TfIOPreviews.Create(self);

    fIOPreviews.DefaultLockPreview := ioppDefaultLockPreview in PreviewsParams;
    fIOPreviews.btnApply.Visible := ioppApplyButton in PreviewsParams;
    fIOPreviews.fParams := fParams;
    fIOPreviews.fSimplified := fSimplifiedParamsDialogs;
    if fSimplifiedParamsDialogs then
    begin
      fIOPreviews.PageControl1.Height := trunc(IO_Preview_Page_Control_Short_Height / 96 * Screen.PixelsPerInch);
      fIOPreviews.Height := trunc(IO_Preview_Dialog_Short_Height / 96 * Screen.PixelsPerInch);
    end;
    fIOPreviews.fDefaultDitherMethod := fDefaultDitherMethod;

    fIOPreviews.UpdateLanguage();

    if fPreviewFontEnabled then
      fIOPreviews.Font.Assign(fPreviewFont)
    else
      fIOPreviews.Font.Assign(IEGetDefaultDialogFont);

    fIOPreviews.ImageEn1.SetExternalBitmap( fIEBitmap );

    fIOPreviews.ImageEn1.Update;
    fIOPreviews.ImageEn2.Blank;

    if fIOPreviews.SetPreviewParams(pp) then
    begin
      if assigned(fOnIOPreview) then
        fOnIOPreview(self, fIOPreviews);
      result := fIOPreviews.ShowModal = mrOk;
    end;

    fIOPreviews.ImageEn1.SetExternalBitmap( nil );

    fIOPreviews.Release;

    Update;
  end
  else
    result := true; // handled
end;
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIOParamsVals.Create

<FM>Declaration<FC>
constructor Create(IEIO: <A TImageEnIO>);

<FM>Description<FN>
A TIOParamsVals object is automatically created by the TImageEnIO component.
!!}
constructor TIOParamsVals.Create(IEIO: TImageEnIO);
begin
  inherited Create;
  fImageEnIO := IEIO;
  fDict := TIEDictionary.Create();
  fColorMap := nil;
  fEXIF_Bitmap := nil;
  fJPEG_MarkerList := TIEMarkerList.Create;
  fIPTC_Info := TIEIPTCInfoList.Create;
  fPXM_Comments := TStringList.Create;
  fGIF_Comments := TStringList.Create;
  fPNG_TextKeys := TStringList.Create;
  fPNG_TextValues := TStringList.Create;
  {$ifdef IEINCLUDEDICOM}
  fDICOM_Tags := TIEDicomTags.Create;
  {$endif}
  {$ifdef IEINCLUDEIMAGINGANNOT}
  fImagingAnnot := nil;
  {$endif}
  fImageEnAnnot := TIEImageEnAnnot.Create(self);
  fInputICC := nil;
  fOutputICC := nil;
  fDefaultICC := nil;
  fEXIF_MakerNote := TIETagsHandler.Create;
  fEXIF_Tags := TList.Create;
  SetDefaultParams;
end;

destructor TIOParamsVals.Destroy;
begin
  FreeAndNil(fEXIF_Tags);
  FreeAndNil(fEXIF_MakerNote);
  if assigned(fEXIF_Bitmap) then
    FreeAndNil(fEXIF_Bitmap);
  if ColorMap <> nil then
    freemem(ColorMap);
  FreeAndNil(fJPEG_MarkerList);
  FreeAndNil(fIPTC_Info);
  {$ifdef IEINCLUDEIMAGINGANNOT}
  if assigned(fImagingAnnot) then
    FreeAndNil(fImagingAnnot);
  {$endif}
  FreeAndNil(fImageEnAnnot);
  FreeAndNil(fPXM_Comments);
  FreeAndNil(fGIF_Comments);
  FreeAndNil(fPNG_TextKeys);
  FreeAndNil(fPNG_TextValues);
  {$ifdef IEINCLUDEDICOM}
  FreeAndNil(fDICOM_Tags);
  {$endif}
  if assigned(fInputICC) then
    FreeAndNil(fInputICC);
  if assigned(fOutputICC) then
    FreeAndNil(fOutputICC);
  if assigned(fDefaultICC) then
    FreeAndNil(fDefaultICC);
  FreeAndNil(fDict);
  inherited;
end;

procedure TIOParamsVals.FreeColorMap;
begin
  if fColorMap <> nil then
    freemem(fColorMap);
  fColorMap := nil;
  fColorMapCount := 0;
end;

                 

{!!
<FS>TIOParamsVals.ClearIPTCField

<FM>Declaration<FC>
procedure ClearIPTCField(iRecNo, iFieldIndex: Integer);

<FM>Description<FN>
Remove the specified field from the IPTC field list.

<FM>Example<FC>
// Clear all IPTC keywords of a file
AnImageEnIO.Params.ClearIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Keywords);

<FM>See Also<FN>
- <L IPTC items compatible with Adobe PhotoShop>Photoshop IPTC consts</L>
- <A TIOParamsVals.WriteIPTCField>
- <A TIOParamsVals.IPTC_Photoshop>
!!}
procedure TIOParamsVals.ClearIPTCField(iRecNo, iFieldIndex: Integer);
var
  Idx: Integer;
  I: Integer;
begin
  Idx  := IPTC_Info.IndexOf(iRecNo, iFieldIndex);
  if Idx >= 0 then
  begin
    for I  := IPTC_Info.count - 1 downto idx do
    begin
      if (IPTC_Info.RecordNumber[i] = iRecNo) and
         (IPTC_Info.DataSet[i] = iFieldIndex) then
        IPTC_Info.DeleteItem(i);
    end;
  end;
end;

{!!
<FS>TidyIPTCStr

<FM>Declaration<FC>
function TidyIPTCStr(const Value: string): string;

<FM>Description<FN>
Removes the extraneous characters often found in IPTC data, such as null terminators.

<FM>Examples<FC>
sDescription := TidyIPTCStr( AnImageEnIO.Params.IPTC_Info.StringItem[iCaption] );
!!}
function TidyIPTCStr(const Value: string): string;
begin
  Result := Value;

  // Often includes a null terminator
  while (result <> '') and (result[length(result)] = #0) do
    setlength(result, length(result) - 1);

  // Remove #$D which appear in many photoshop descriptions
  result := StringReplace(Result, #$D, ' ', [rfReplaceAll]);
end;

{!!
<FS>TIOParamsVals.ReadIPTCField

<FM>Declaration<FC>
function ReadIPTCField(iRecNo, iFieldIndex: Integer): string;
procedure ReadIPTCField(iRecNo, iFieldIndex: Integer; Dest: TStrings);

<FM>Description<FN>
Return a value from an IPTC field. Fields such as Photoshop's keyword field may have multiple instances. In this case the first method will return a comma separated string, whereas the second will add each instance to the TStrings object

<FM>Example<FC>
// Read the Photoshop description
sDescription := AnImageEnIO.Params.ReadIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Caption);

// Read all the Photoshop keywords (comma-separated):
sKeywords := AnImageEnIO.Params.ReadIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Keywords);

// Add all Photoshop keywords to a Listbox:
AnImageEnIO.Params.ReadIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Keywords, MyListBox.Items);


<FM>See Also<FN>
- <L IPTC items compatible with Adobe PhotoShop>Photoshop IPTC consts</L>
- <A TIOParamsVals.WriteIPTCField> 
- <A TIOParamsVals.IPTC_Photoshop>
!!}
function TIOParamsVals.ReadIPTCField(iRecNo, iFieldIndex: Integer): string;
var
  Idx: Integer;
  I: Integer;
  bExpectingMultiple: Boolean;
begin
  result := '';

  // get the field index
  Idx := IPTC_Info.IndexOf(iRecNo, iFieldIndex);
  if idx = -1 then
    Exit;

  bExpectingMultiple := (iRecNo = PhotoShop_IPTC_Records) and (iFieldIndex = IPTC_PS_KEYWORDS);

  if bExpectingMultiple = False then
  begin
    result := TidyIPTCStr(IPTC_Info.StringItem[idx]);
  end
  else
  begin
    // Have multiple instances of the field
    for I := idx to IPTC_Info.count - 1 do
      if (IPTC_Info.RecordNumber[i] = iRecNo) and
         (IPTC_Info.DataSet[i] = iFieldIndex) then
        Result := Result + TidyIPTCStr(IPTC_Info.StringItem[i]) + ', ';

    if result <> '' then
      SetLength(Result, Length(Result) - 2);  // remove final comma and space
  end;
end;

procedure TIOParamsVals.ReadIPTCField(iRecNo, iFieldIndex: Integer; Dest: TStrings);
var
  Idx: Integer;
  I: Integer;
begin
  Dest.Clear;

  // get the field index
  Idx := IPTC_Info.IndexOf(iRecNo, iFieldIndex);
  if idx = -1 then
    Exit;

  for I := idx to IPTC_Info.count - 1 do
    if (IPTC_Info.RecordNumber[i] = iRecNo) and
       (IPTC_Info.DataSet[i] = iFieldIndex) then
      Dest.Add(TidyIPTCStr(IPTC_Info.StringItem[i]));;
end;



{!!
<FS>TIOParamsVals.WriteIPTCField

<FM>Declaration<FC>
procedure WriteIPTCField(iRecNo, iFieldIndex: Integer; const Value: string);
procedure WriteIPTCField(iRecNo, iFieldIndex: Integer; ssValues: TStrings);

<FM>Description<FN>
Write a value to an IPTC field.

Note: Items that require multiple instances, such as Photoshop's keyword field, should be added using the TStrings overload method.

<FM>Example<FC>
// Write the Photoshop description:
AnImageEnIO.Params.WriteIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Caption, sDescription);

// Write the Photoshop keywords (in a comma-separated string):
ssList := TStringList.create;
ssList.DelimitedText := sKeywords;
AnImageEnIO.Params.WriteIPTCField(PhotoShop_IPTC_Records, IPTC_PS_Keywords, ssList);
ssList.Free;
                                                              
<FM>See Also<FN>
- <L IPTC items compatible with Adobe PhotoShop>Photoshop IPTC consts</L>
- <A TIOParamsVals.ReadIPTCField>     
- <A TIOParamsVals.IPTC_Photoshop>
!!}
procedure TIOParamsVals.WriteIPTCField(iRecNo, iFieldIndex: Integer; const Value: string);
var
  Idx: Integer;
  sKeywords: string;
  ssKeywords: TStringList;
  I: Integer;
begin
  // Special handling for keywords, as they need to be added as multiple instances
  if (iRecNo = PhotoShop_IPTC_Records) and (iFieldIndex = IPTC_PS_Keywords) then
  begin
    ssKeywords := TStringList.Create;
    try
      sKeywords := StringReplace(Value, ',', #13#10, [rfReplaceAll]);
      ssKeywords.Text := sKeywords;
      for I := 0 to ssKeywords.Count - 1 do
        ssKeywords[I] := Trim(ssKeywords[I]);
      WriteIPTCField(iRecNo, iFieldIndex, ssKeywords);
    finally
      ssKeywords.free;
    end;
  end

  ELSE

  // REGULAR FIELDS
  begin
    // get the field index
    Idx := IPTC_Info.IndexOf(iRecNo, iFieldIndex);

    if idx < 0 then
    begin
      // Doesn't exist, so add it
      if Value <> '' then
        IPTC_Info.AddStringItem(iRecNo, iFieldIndex, AnsiString(Value))
    end
    else
    begin
      // Exists...
      if Value = '' then
        IPTC_Info.DeleteItem(Idx)
      else
      if IPTC_Info.StringItem[Idx] <> Value then
        IPTC_Info.StringItem[Idx] := Value;
    end;
  end;
end;

procedure TIOParamsVals.WriteIPTCField(iRecNo, iFieldIndex: Integer; ssValues: TStrings);
var
  Value: string;
  I: Integer;
begin  
  // Clear existing entries
  ClearIPTCField(iRecNo, iFieldIndex);

  // Now add them
  for I := 0 to ssValues.Count - 1 do
  begin
    Value := ssValues[I];
    if Value <> '' then
      IPTC_Info.AddStringItem(iRecNo, iFieldIndex, AnsiString(Value));
  end;
end;




{!!
<FS>TIOParamsVals.IPTC_Photoshop

<FM>Declaration<FC>
property IPTC_Photoshop[FieldID: Integer]: string;

<FM>Description<FN>
Read or write a <L IPTC items compatible with Adobe PhotoShop>Photoshop compatible IPTC field</L>.

Note: This is the same as calling Params.ReadIPTCField(PhotoShop_IPTC_Records, FieldID) or Params.WriteIPTCField(PhotoShop_IPTC_Records, FieldID, Value)

Acceptable values for FieldID:
<TABLE>
<R> <H>Const</H> <H>Description</H> </R>
<R> <C>IPTC_PS_Title (5)</C> <C>Object name</C> </R>
<R> <C>IPTC_PS_Edit_Status (7)</C> <C>Edit status</C> </R>
<R> <C>IPTC_PS_Urgency (10)</C> <C>Urgency</C> </R>
<R> <C>IPTC_PS_Category (15)</C> <C>Category</C> </R>
<R> <C>IPTC_PS_Category_2 (20)</C> <C>Supplemental Category</C> </R>
<R> <C>IPTC_PS_Fixture_Identifier (22)</C> <C>Fixture Identifier</C> </R>
<R> <C>IPTC_PS_Keywords (25)</C> <C>Keywords</C> </R>
<R> <C>IPTC_PS_Release_Date (30)</C> <C>Release Date</C> </R>
<R> <C>IPTC_PS_Release_Time (35)</C> <C>Release Time</C> </R>
<R> <C>IPTC_PS_Instructions (40)</C> <C>Special Instructions</C> </R>
<R> <C>IPTC_PS_Reference_Service (45)</C> <C>Reference Service</C> </R>
<R> <C>IPTC_PS_Reference_Date (47)</C> <C>Reference Date</C> </R>
<R> <C>IPTC_PS_Reference_Number (50)</C> <C>Reference Number</C> </R>
<R> <C>IPTC_PS_Date_Created (55)</C> <C>Date Created</C> </R>
<R> <C>IPTC_PS_Time_Created (60)</C> <C>Time Created</C> </R>
<R> <C>IPTC_PS_Originating_Program (65)</C> <C>Originating Program</C> </R>
<R> <C>IPTC_PS_Program_Version (70)</C> <C>Program Version</C> </R>
<R> <C>IPTC_PS_Object_Cycle (75)</C> <C>Object Cycle</C> </R>
<R> <C>IPTC_PS_Byline_1 (80)</C> <C>By-line</C> </R>
<R> <C>IPTC_PS_Byline_2 (85)</C> <C>By-line Title</C> </R>
<R> <C>IPTC_PS_City (90)</C> <C>City</C> </R>
<R> <C>IPTC_PS_State_Province (95)</C> <C>Province/State</C> </R>
<R> <C>IPTC_PS_Country_Code (100)</C> <C>Country/Primary Location Code</C> </R>
<R> <C>IPTC_PS_Country (101)</C> <C>Country/Primary Location Name</C> </R>
<R> <C>IPTC_PS_Transmission_Reference (103)</C> <C>Original Transmission Reference</C> </R>
<R> <C>IPTC_PS_Credit (110)</C> <C>Credit</C> </R>
<R> <C>IPTC_PS_Source (115)</C> <C>Source</C> </R>
<R> <C>IPTC_PS_Copyright_Notice (116)</C> <C>Copyright Notice</C> </R>
<R> <C>IPTC_PS_Caption (120)</C> <C>Caption/Abstract</C> </R>
<R> <C>IPTC_PS_Writer (122)</C> <C>Writer/Editor</C> </R>
<R> <C>IPTC_PS_Image_Type (130)</C> <C>Image Type</C> </R>
</TABLE>

Notes:
- IPTC_PS_Keywords is stored with multiple instances. This will be returned as a comma-separated string, and can be set as a comma-separated or Linebreak-delimited string (or use the overloaded version of <A TIOParamsVals.WriteIPTCField>)
- IPTC_PS_Caption can contain linebreaks
- All fields have <L IPTC items compatible with Adobe PhotoShop>maximum lengths</L>

<FM>Examples<FC>
// Read the image description (written by PhotoShop)
ImageEnView1.IO.LoadFromFile('C:\image.jpg');
Caption := ImageEnView1.IO.Params.IPTC_Photoshop[IPTC_PS_Caption];

// Write the image description (without resaving JPEG image)
ImageEnView1.IO.Params.IPTC_Photoshop[IPTC_PS_Caption] := 'This is the new caption';
ImageEnView1.IO.InjectJpegIPTC('D:\image.jpg');

// Write keywords
ImageEnView1.IO.Params.IPTC_Photoshop[IPTC_PS_Keywords] := Listbox1.Text;
ImageEnView1.IO.SaveToFile('D:\image.jpg');

<FM>See Also<FN>
- <L TIOParamsVals.IPTC_Info>          
- <L IPTC items compatible with Adobe PhotoShop>Photoshop IPTC consts</L>
- <A TIOParamsVals.ReadIPTCField>
- <A TIOParamsVals.WriteIPTCField>
- <A TIOParamsVals.ClearIPTCField>
!!}
function TIOParamsVals.GetIPTC_Photoshop(FieldID: Integer): string;
begin
  Result := ReadIPTCField(PhotoShop_IPTC_Records, FieldID);
end;


procedure TIOParamsVals.SetIPTC_Photoshop(FieldID: Integer; const Value: string);
begin
  WriteIPTCField(PhotoShop_IPTC_Records, FieldID, Value);
end;


{!!
<FS>TIOParamsVals.ResetEXIF

<FM>Declaration<FC>
procedure ResetEXIF;

<FM>Description<FN>
Resets (nulls) all EXIF fields. This is useful to remove EXIF info from your files.

Note: To remove all meta-data from a JPEG, it is better to call <A TIOParamsVals.ResetInfo> instead.
!!}
procedure TIOParamsVals.ResetEXIF;
var
  q: integer;
begin
  fEXIF_Tags.Clear;
  fEXIF_HasEXIFData := false;
  if assigned(fEXIF_Bitmap) then
    FreeAndNil(fEXIF_Bitmap);
  fEXIF_Bitmap := nil;
  fEXIF_ImageDescription := '';
  fEXIF_Make := '';
  fEXIF_Model := '';
  fEXIF_Orientation := 0;
  fEXIF_XResolution := 0;
  fEXIF_YResolution := 0;
  fEXIF_ResolutionUnit := 0;
  fEXIF_Software := '';
  fEXIF_Artist := '';
  fEXIF_DateTime := '';
  fEXIF_WhitePoint[0] := -1;
  fEXIF_WhitePoint[1] := -1;
  for q := 0 to 5 do
    fEXIF_PrimaryChromaticities[q] := -1;
  for q := 0 to 2 do
    fEXIF_YCbCrCoefficients[q] := -1;
  fEXIF_YCbCrPositioning := -1;
  for q := 0 to 5 do
    fEXIF_ReferenceBlackWhite[q] := -1;
  fEXIF_Copyright := '';
  fEXIF_ExposureTime := -1;
  fEXIF_FNumber := -1;
  fEXIF_ExposureProgram := -1;
  fEXIF_ISOSpeedRatings[0] := 0;
  fEXIF_ISOSpeedRatings[1] := 0;
  fEXIF_ExifVersion := '';
  fEXIF_DateTimeOriginal := '';
  fEXIF_DateTimeDigitized := '';
  fEXIF_CompressedBitsPerPixel := 0;
  fEXIF_ShutterSpeedValue := -1;
  fEXIF_ApertureValue := -1;
  fEXIF_BrightnessValue := -1000;
  fEXIF_ExposureBiasValue := -1000;
  fEXIF_MaxApertureValue := -1000;
  fEXIF_SubjectDistance := -1;
  fEXIF_MeteringMode := -1;
  fEXIF_LightSource := -1;
  fEXIF_Flash := -1;
  fEXIF_FocalLength := -1;
  fEXIF_SubsecTime := '';
  fEXIF_SubsecTimeOriginal := '';
  fEXIF_SubsecTimeDigitized := '';
  fEXIF_FlashPixVersion := '';
  fEXIF_ColorSpace := -1;
  fEXIF_ExifImageWidth := 0;
  fEXIF_ExifImageHeight := 0;
  fEXIF_RelatedSoundFile := '';
  fEXIF_FocalPlaneXResolution := -1;
  fEXIF_FocalPlaneYResolution := -1;
  fEXIF_FocalPlaneResolutionUnit := -1;
  fEXIF_ExposureIndex := -1;
  fEXIF_SensingMethod := -1;
  fEXIF_FileSource := -1;
  fEXIF_SceneType := -1;
  fEXIF_UserComment := '';
  fEXIF_UserCommentCode := '';
  fEXIF_MakerNote.Clear;
  fEXIF_XPRating := -1;
  fEXIF_XPTitle := '';
  fEXIF_XPComment := '';
  fEXIF_XPAuthor := '';
  fEXIF_XPKeywords := '';
  fEXIF_XPSubject := '';
  fEXIF_ExposureMode := -1;
  fEXIF_WhiteBalance := -1;
  fEXIF_DigitalZoomRatio := -1;
  fEXIF_FocalLengthIn35mmFilm := -1;
  fEXIF_SceneCaptureType := -1;
  fEXIF_GainControl := -1;
  fEXIF_Contrast := -1;
  fEXIF_Saturation := -1;
  fEXIF_Sharpness := -1;
  fEXIF_SubjectDistanceRange := -1;
  fEXIF_ImageUniqueID := '';
  fEXIF_GPSVersionID := '';  // ''=indicates that there aren't GPS info to write
  fEXIF_GPSLatitudeRef := '';
  fEXIF_GPSLatitudeDegrees := 0;
  fEXIF_GPSLatitudeMinutes := 0;
  fEXIF_GPSLatitudeSeconds := 0;
  fEXIF_GPSLongitudeRef := '';
  fEXIF_GPSLongitudeDegrees := 0;
  fEXIF_GPSLongitudeMinutes := 0;
  fEXIF_GPSLongitudeSeconds := 0;
  fEXIF_GPSAltitudeRef := '';
  fEXIF_GPSAltitude := 0;
  fEXIF_GPSTimeStampHour := 0;
  fEXIF_GPSTimeStampMinute := 0;
  fEXIF_GPSTimeStampSecond := 0;
  fEXIF_GPSSatellites := '';
  fEXIF_GPSStatus := '';
  fEXIF_GPSMeasureMode := '';
  fEXIF_GPSDOP := 0;
  fEXIF_GPSSpeedRef := '';
  fEXIF_GPSSpeed := 0;
  fEXIF_GPSTrackRef := '';
  fEXIF_GPSTrack := 0;
  fEXIF_GPSImgDirectionRef := '';
  fEXIF_GPSImgDirection := 0;
  fEXIF_GPSMapDatum := '';
  fEXIF_GPSDestLatitudeRef := '';
  fEXIF_GPSDestLatitudeDegrees := 0;
  fEXIF_GPSDestLatitudeMinutes := 0;
  fEXIF_GPSDestLatitudeSeconds := 0;
  fEXIF_GPSDestLongitudeRef := '';
  fEXIF_GPSDestLongitudeDegrees := 0;
  fEXIF_GPSDestLongitudeMinutes := 0;
  fEXIF_GPSDestLongitudeSeconds := 0;
  fEXIF_GPSDestBearingRef := '';
  fEXIF_GPSDestBearing := 0;
  fEXIF_GPSDestDistanceRef := '';
  fEXIF_GPSDestDistance := 0;
  fEXIF_GPSDateStamp := '';
  fEXIF_InteropIndex := '';
  fEXIF_InteropVersion := '';
  // removes Jpeg EXIF tag
  with JPEG_MarkerList do
    for q := 0 to Count-1 do
      if (MarkerType[q]=JPEG_APP1) and CheckEXIFFromStandardBuffer(MarkerData[q], MarkerLength[q]) then
      begin
        DeleteMarker(q);
        break;
      end;
end;



{!!
<FS>TIOParamsVals.ResetInfo

<FM>Declaration<FC>
procedure ResetInfo;

<FM>Description<FN>
Resets IPTC information, imaging annotations, JPEG markers, EXIF, GIF comments, PNG comments, TIFF textual tags, TGA comments, PXM comments and any other loaded textual information. It also removes the input ICC profile.

This can be used to reduce the size of images.

<FM>Example<FC>
ImageEnView1.IO.Params.ResetInfo;
!!}
// reset only info tags (EXIF, IPTC, JPEG_Tags, XMP and comments)
procedure TIOParamsVals.ResetInfo;
begin
  IPTC_Info.Clear;
  {$ifdef IEINCLUDEIMAGINGANNOT}
  if assigned(fImagingAnnot) then
    FreeAndNil(fImagingAnnot);
  {$endif}
  fImageEnAnnot.Clear();
  JPEG_MarkerList.Clear;
  ResetEXIF;
  GIF_Comments.Clear;
  {$ifdef IEINCLUDEDICOM}
  DICOM_Tags.Clear;
  {$endif}
  PNG_TextKeys.Clear;
  PNG_TextValues.Clear;
  TIFF_DocumentName := '';
  TIFF_ImageDescription := '';
  TIFF_PageName := '';
  TGA_Descriptor := '';
  TGA_Author := '';
  TGA_ImageName := '';
  PXM_Comments.Clear;
  XMP_Info := '';
  if assigned(fInputICC) then
    FreeAndNil(fInputICC);
  // do not reset output or Default ICC profile
end;

{!!
<FS>TIOParamsVals.SetDefaultParams

<FM>Declaration<FC>
procedure SetDefaultParams;

<FM>Description<FN>
Resets the parameters of the TIOParamsVals object to their default (start-up values).
!!}
procedure TIOParamsVals.SetDefaultParams;
begin
  fDict.Clear();
  FreeColorMap; // no colormap
  fFileName           := '';
  fFileType           := ioUnknown;
  BitsPerSample       := 8; // 8 BITS x SAMPLE
  SamplesPerPixel     := 3; // 3 SAMPLES x PIXEL (RGB)
  IsNativePixelFOrmat := false;
  if assigned(fImageEnIO) then
  begin
    try
      if assigned(fImageEnIO.Bitmap) then
      begin
        fWidth  := fImageEnIO.Bitmap.Width;
        fHeight := fImageEnIO.Bitmap.Height;
      end;
    except
      // Unexpected error
      fWidth  := 0;
      fHeight := 0;
    end;


    SetDPIX(IEGlobalSettings().DefaultDPIX);
    SetDPIY(IEGlobalSettings().DefaultDPIY);
  end;
  fImageIndex              := 0;
  fImageCount              := 0;
  fGetThumbnail            := false;
  fIsResource              := false;
  fEnableAdjustOrientation := false;
  // GIF
  GIF_Version         := 'GIF89a';
  GIF_ImageIndex      := 0;
  GIF_XPos            := 0;
  GIF_YPos            := 0;
  GIF_DelayTime       := 0;
  GIF_FlagTranspColor := false;
  GIF_TranspColor     := CreateRGB(0, 0, 0);
  GIF_Interlaced      := false;
  GIF_WinWidth        := 0;
  GIF_WinHeight       := 0;
  GIF_Background      := CreateRGB(0, 0, 0);
  GIF_Ratio           := 0;
  GIF_LZWDecompFunc   := IEGlobalSettings().DefGIF_LZWDecompFunc;
  GIF_LZWCompFunc     := IEGlobalSettings().DefGIF_LZWCompFunc;
  fGIF_ImageCount     := 0;
  GIF_Comments.Clear;
  GIF_Action          := ioGIF_DrawBackground;
  GIF_RAWLoad         := false;
  // DCX
  DCX_ImageIndex      := 0;
  // TIFF
  TIFF_Compression              := ioTIFF_UNCOMPRESSED;
  TIFF_PhotometInterpret        := ioTIFF_RGB;
  TIFF_PlanarConf               := 1;
  TIFF_ImageIndex               := 0;
  TIFF_SubIndex                 := -1;
  TIFF_NewSubfileType           := 0;
  TIFF_XPos                     := 0;
  TIFF_YPos                     := 0;
  TIFF_GetTile                  := -1;
  TIFF_DocumentName             := '';
  TIFF_ImageDescription         := '';
  TIFF_PageName                 := '';
  TIFF_PageNumber               := -1;
  TIFF_PageCount                := -1;
  TIFF_Orientation              := 1;
  TIFF_LZWDecompFunc            := IEGlobalSettings().DefTIFF_LZWDecompFunc;
  TIFF_LZWCompFunc              := IEGlobalSettings().DefTIFF_LZWCompFunc;
  fTIFF_ImageCount              := 0;
  fTIFF_EnableAdjustOrientation := false;
  fTIFF_JPEGQuality             := 80;
  fTIFF_JPEGColorSpace          := ioJPEG_YCBCR;
  fTIFF_FillOrder               := 1;
  fTIFF_ZIPCompression          := 1;              // normal (default)
  fTIFF_ByteOrder               := ioLittleEndian; // Intel
  fTIFF_StripCount              := 0;              // 0 = automatic
  fTIFF_BigTIFF                 := false;
  SetLength(fTIFF_PhotoshopImageResources, 0);
  SetLength(fTIFF_PhotoshopImageSourceData, 0);

  // JPEG
  JPEG_ColorSpace              := ioJPEG_YCbCr;
  JPEG_Quality                 := 80;
  JPEG_DCTMethod               := ioJPEG_ISLOW; // 3.0.1
  JPEG_CromaSubsampling        := ioJPEG_HIGH;
  JPEG_OptimalHuffman          := false;
  JPEG_Smooth                  := 0;
  JPEG_Progressive             := false;
  JPEG_Scale                   := ioJPEG_FULLSIZE;
  JPEG_MarkerList.Clear;
  JPEG_Scale_Used              := 1;
  OriginalWidth                := 0;
  OriginalHeight               := 0;
  JPEG_EnableAdjustOrientation := false;
  JPEG_GetExifThumbnail        := false;
  // JPEG2000
{$IFDEF IEINCLUDEJPEG2000}
  fJ2000_ColorSpace := ioJ2000_RGB;
  fJ2000_Rate       := 0.07;
  fJ2000_ScalableBy := ioJ2000_Rate;
{$ENDIF}
  // BMP
  BMP_Compression        := ioBMP_UNCOMPRESSED;
  BMP_Version            := ioBMP_BM3;
  BMP_HandleTransparency := false;
  // PCX
  PCX_Version     := 5;
  PCX_Compression := ioPCX_RLE;
  // ICO
  ICO_ImageIndex := 0;
  ICO_Background := CreateRGB(255, 255, 255);
  FillChar(ICO_Sizes[0], sizeof(TIOICOSizes), 0);
  ICO_Sizes[0].cx := 64;
  ICO_Sizes[0].cy := 64;
  FillChar(ICO_BitCount[0], sizeof(TIOICOBitCount), 0);
  ICO_BitCount[0] := 8;
  // CUR
  CUR_ImageIndex := 0;
  CUR_XHotSpot   := 0;
  CUR_XHotSpot   := 0;
  CUR_Background := CreateRGB(255, 255, 255);
  // PNG
  PNG_Interlaced  := false;
  PNG_Background  := CreateRGB(0, 0, 0);
  PNG_Filter      := ioPNG_FILTER_NONE;
  PNG_Compression := 5;
  PNG_TextKeys.Clear;
  PNG_TextValues.Clear;
  // DICOM
  {$ifdef IEINCLUDEDICOM}
  DICOM_Tags.Clear;
  DICOM_WindowCenterOffset := 0;
  DICOM_Range := iedrAdjust;
  fDICOM_RescaleIntercept := 0;
  fDICOM_RescaleSlope := 1;
  SetDICOM_Compression(iedcUncompressed);
  fDICOM_JPEGQuality := 80;
  fDICOM_J2000Rate := 0.07;
  {$endif}
  // PSD
  PSD_LoadLayers             := false;
  PSD_ReplaceLayers          := true;
  fPSD_HasPremultipliedAlpha := false;
  fPSD_LargeDocumentFormat   := false;
  fPSD_SelectLayer           := '';   
  // HDP
  fHDP_ImageQuality := 0.9;
  fHDP_Lossless     := false;
  // TGA
  TGA_XPos        := 0;
  TGA_YPos        := 0;
  TGA_Compressed  := true;
  TGA_Descriptor  := '';
  TGA_Author      := '';
  TGA_Date        := date;
  TGA_ImageName   := '';
  TGA_Background  := CreateRGB(0, 0, 0);
  TGA_AspectRatio := 1;
  TGA_Gamma       := 2.2;
  TGA_GrayLevel   := false;
  // AVI
  AVI_FrameCount     := 0;
  AVI_FrameDelayTime := 0.0;
  // IPTC
  IPTC_Info.Clear;
  // PXM
  PXM_Comments.Clear;
  // EXIF
  ResetEXIF;
  // PS
  PS_PaperWidth   := 595;
  PS_PaperHeight  := 842;
  PS_Compression  := ioPS_G4FAX;
  PS_Title        := 'No Title';
  // PDF
  PDF_PaperWidth  := 595;
  PDF_PaperHeight := 842;
  PDF_Compression := ioPDF_G4FAX;
  PDF_Title       := '';
  PDF_Author      := '';
  PDF_Subject     := '';
  PDF_Keywords    := '';
  PDF_Creator     := '';
  PDF_Producer    := '';

  {$ifdef IEINCLUDEIMAGINGANNOT}
  // Imaging Annotations
  if assigned(fImagingAnnot) then
    FreeAndNil(fImagingAnnot);
  {$endif}

  // ImageEn Annotations
  fImageEnAnnot.Clear();

  // ICC
  if assigned(fInputICC) then
    FreeAndNil(fInputICC);
  if assigned(fOutputICC) then
    FreeAndNil(fOutputICC);
  if assigned(fDefaultICC) then
    FreeAndNil(fDefaultICC);
  // RAW
  {$ifdef IEINCLUDERAWFORMATS}
  fRAW_HalfSize         := false;
  fRAW_Gamma            := 0.6;
  fRAW_Bright           := 1.0;
  fRAW_RedScale         := 1.0;
  fRAW_BlueScale        := 1.0;
  fRAW_QuickInterpolate := true;
  fRAW_UseAutoWB        := false;
  fRAW_UseCameraWB      := false;
  fRAW_FourColorRGB     := false;
  fRAW_Camera           := '';
  fRAW_GetExifThumbnail := false;
  fRAW_AutoAdjustColors := false;
  fRAW_ExtraParams      := '';
  {$endif}
  // real RAW
  fBMPRAW_ChannelOrder  := coRGB;
  fBMPRAW_Planes        := plInterleaved;
  fBMPRAW_RowAlign      := 8;
  fBMPRAW_HeaderSize    := 0;
  fBMPRAW_DataFormat    := dfBinary;
  // XMP
  fXMP_Info             := '';
end;


{!!
<FS>TIOParamsVals.Assign

<FM>Declaration<FC>
procedure Assign(Source: <A TIOParamsVals>);

<FM>Description<FN>
Copy all image format parameters from <FC>Source<FN> to this object.

<FM>Example<FC>
ImageEnView1.IO.Params.Assign( ImageEnView2.IO.Params );
!!}
procedure TIOParamsVals.Assign(Source: TIOParamsVals);
begin
  fDict.Assign(Source.fDict);
  fFileName := Source.FileName;
  fFileType := Source.FileType;
  BitsPerSample := Source.BitsPerSample;
  SamplesPerPixel := Source.SamplesPerPixel;
  fWidth := Source.Width;
  fHeight := Source.Height;
  DpiX := Source.DpiX;
  DpiY := Source.DpiY;
  fColorMapCount := Source.ColorMapCount;
  IsNativePixelFormat := Source.IsNativePixelFormat;  // 3.0.3
  FreeColorMap;
  if Source.ColorMap <> nil then
  begin
    getmem(fColorMap, ColorMapCount * sizeof(TRGB));
    copymemory(ColorMap, Source.ColorMap, ColorMapCount * sizeof(TRGB));
  end;
  fImageIndex := Source.fImageIndex;
  fImageCount := Source.fImageCount;
  fGetThumbnail := Source.fGetThumbnail;
  fIsResource := Source.fIsResource;
  fEnableAdjustOrientation := Source.fEnableAdjustOrientation;
  OriginalWidth := Source.OriginalWidth;
  OriginalHeight := Source.OriginalHeight;

  // TIFF
  TIFF_Compression := Source.TIFF_Compression;
  TIFF_ImageIndex := Source.TIFF_ImageIndex;
  TIFF_SubIndex := Source.TIFF_SubIndex;
  TIFF_PhotometInterpret := Source.TIFF_PhotometInterpret;
  TIFF_PlanarConf := Source.TIFF_PlanarConf;
  TIFF_NewSubfileType := Source.TIFF_NewSubfileType;
  TIFF_XPos := Source.TIFF_XPos;
  TIFF_YPos := Source.TIFF_YPos;
  TIFF_GetTile := Source.TIFF_GetTile;
  TIFF_DocumentName := Source.TIFF_DocumentName;
  TIFF_ImageDescription := Source.TIFF_ImageDescription;
  TIFF_PageName := Source.TIFF_PageName;
  TIFF_PageNumber := Source.TIFF_PageNumber;
  TIFF_PageCount := Source.TIFF_PageCount;
  TIFF_Orientation := Source.TIFF_Orientation;
  TIFF_LZWDecompFunc := Source.TIFF_LZWDecompFunc;
  TIFF_LZWCompFunc := Source.TIFF_LZWCompFunc;
  fTIFF_ImageCount := Source.TIFF_ImageCount;
  fTIFF_EnableAdjustOrientation := Source.TIFF_EnableAdjustOrientation;
  fTIFF_JPEGQuality := Source.fTIFF_JPEGQuality;
  fTIFF_JPEGColorSpace := Source.fTIFF_JPEGColorSpace;
  fTIFF_FillOrder := Source.fTIFF_FillOrder;
  fTIFF_ZIPCompression := Source.fTIFF_ZIPCompression;
  fTIFF_ByteOrder := Source.fTIFF_ByteOrder;
  fTIFF_StripCount := Source.fTIFF_StripCount;
  fTIFF_PhotoshopImageResources := IECopyArrayOfByte(Source.fTIFF_PhotoshopImageResources);
  fTIFF_PhotoshopImageSourceData := IECopyArrayOfByte(Source.fTIFF_PhotoshopImageSourceData);
  fTIFF_BigTIFF := Source.fTIFF_BigTIFF;

  // GIF
  GIF_Version := Source.GIF_Version;
  GIF_ImageIndex := Source.GIF_ImageIndex;
  GIF_XPos := Source.GIF_XPos;
  GIF_YPos := Source.GIF_YPos;
  GIF_DelayTime := Source.GIF_DelayTime;
  GIF_FlagTranspColor := Source.GIF_FlagTranspColor;
  GIF_TranspColor := Source.GIF_TranspColor;
  GIF_Interlaced := Source.GIF_Interlaced;
  GIF_WinWidth := Source.GIF_WinWidth;
  GIF_WinHeight := Source.GIF_WinHeight;
  GIF_Background := Source.GIF_background;
  GIF_Ratio := Source.GIF_Ratio;
  GIF_LZWDecompFunc := Source.GIF_LZWDecompFunc;
  GIF_LZWCompFunc := Source.GIF_LZWCompFunc;
  fGIF_ImageCount := Source.GIF_ImageCount;
  GIF_Comments.Assign(Source.GIF_Comments);
  GIF_Action := Source.GIF_Action;
  GIF_RAWLoad := Source.GIF_RAWLoad;

  // DCX
  DCX_ImageIndex := Source.DCX_ImageIndex;

  // JPEG
  JPEG_ColorSpace := Source.JPEG_ColorSpace;
  JPEG_Quality := Source.JPEG_Quality;
  JPEG_DCTMethod := Source.JPEG_DCTMethod;
  JPEG_CromaSubsampling := Source.JPEG_CromaSubsampling;
  JPEG_OptimalHuffman := Source.JPEG_OptimalHuffman;
  JPEG_Smooth := Source.JPEG_Smooth;
  JPEG_Progressive := Source.JPEG_Progressive;
  JPEG_Scale := Source.JPEG_Scale;
  JPEG_MarkerList.Assign(Source.JPEG_MarkerList);
  JPEG_Scale_Used := Source.JPEG_Scale_Used;
  JPEG_EnableAdjustOrientation := Source.JPEG_EnableAdjustOrientation;
  JPEG_GetExifThumbnail := Source.JPEG_GetExifThumbnail;

  // JPEG2000
{$IFDEF IEINCLUDEJPEG2000}
  fJ2000_ColorSpace := Source.J2000_ColorSpace;
  fJ2000_Rate := Source.J2000_Rate;
  fJ2000_ScalableBy := Source.J2000_ScalableBy;
{$ENDIF}

  // PCX
  PCX_Version := Source.PCX_Version;
  PCX_Compression := Source.PCX_Compression;

  // BMP
  BMP_Version := Source.BMP_Version;
  BMP_Compression := Source.BMP_Compression;
  BMP_HandleTransparency := Source.BMP_HandleTransparency;

  // ICO
  ICO_ImageIndex := Source.ICO_ImageIndex;
  ICO_Background := Source.ICO_Background;
  move(Source.ICO_Sizes[0], ICO_Sizes[0], sizeof(TIOICOSizes));
  move(Source.ICO_BitCount[0], ICO_BitCount[0], sizeof(TIOICOBitCount));

  // CUR
  CUR_ImageIndex := Source.CUR_ImageIndex;
  CUR_XHotSpot := Source.CUR_XHotSpot;
  CUR_YHotSpot := Source.CUR_YHotSpot;
  CUR_Background := Source.CUR_background;

  // PNG
  PNG_Interlaced := Source.PNG_Interlaced;
  PNG_Background := Source.PNG_Background;
  PNG_Filter := Source.PNG_Filter;
  PNG_Compression := Source.PNG_Compression;
  PNG_TextKeys.Assign( Source.PNG_TextKeys );
  PNG_TextValues.Assign( Source.PNG_TextValues );

  // DICOM
  {$ifdef IEINCLUDEDICOM}
  DICOM_Tags.Assign( Source.DICOM_Tags );
  DICOM_WindowCenterOffset := Source.DICOM_WindowCenterOffset;
  DICOM_Range := Source.DICOM_Range;
  SetDICOM_Compression(Source.DICOM_Compression);
  fDICOM_JPEGQuality := Source.fDICOM_JPEGQuality;
  fDICOM_J2000Rate := Source.fDICOM_J2000Rate;
  fDICOM_RescaleIntercept := Source.fDICOM_RescaleIntercept;
  fDICOM_RescaleSlope := Source.fDICOM_RescaleSlope;
  {$endif}

  // PSD
  PSD_LoadLayers := Source.PSD_LoadLayers;
  PSD_ReplaceLayers := Source.PSD_ReplaceLayers;
  fPSD_HasPremultipliedAlpha := Source.PSD_HasPremultipliedAlpha;
  fPSD_LargeDocumentFormat := Source.PSD_LargeDocumentFormat;
  fPSD_SelectLayer := Source.PSD_SelectLayer;

  // HDP
  fHDP_ImageQuality := Source.fHDP_ImageQuality;
  fHDP_Lossless := Source.fHDP_Lossless;

  // TGA
  TGA_XPos := Source.TGA_XPos;
  TGA_YPos := Source.TGA_YPos;
  TGA_Compressed := Source.TGA_Compressed;
  TGA_Descriptor := Source.TGA_Descriptor;
  TGA_Author := Source.TGA_Author;
  TGA_Date := Source.TGA_Date;
  TGA_ImageName := Source.TGA_ImageName;
  TGA_Background := Source.TGA_Background;
  TGA_AspectRatio := Source.TGA_AspectRatio;
  TGA_Gamma := Source.TGA_Gamma;
  TGA_GrayLevel := Source.TGA_GrayLevel;

  // AVI
  AVI_FrameCount := Source.AVI_FrameCount;
  AVI_FrameDelayTime := Source.AVI_FrameDelayTime;

  // IPTC
  IPTC_Info.Assign(Source.IPTC_Info);

  {$ifdef IEINCLUDEIMAGINGANNOT}
  // Imaging annotations
  if assigned(Source.fImagingAnnot) then
    ImagingAnnot.Assign(Source.ImagingAnnot)
  else
  if assigned(fImagingAnnot) then
    FreeAndNil(fImagingAnnot);
  {$endif}

  // ImageEn annotations
  fImageEnAnnot.Assign(Source.fImageEnAnnot);

  // ICC
  if assigned(Source.fInputICC) then
    InputICCProfile.Assign(Source.InputICCProfile)
  else
  if assigned(fInputICC) then
    FreeAndNil(fInputICC);
  if assigned(Source.fOutputICC) then
    OutputICCProfile.Assign(Source.OutputICCProfile)
  else
  if assigned(fOutputICC) then
    FreeAndNil(fOutputICC);
  if assigned(Source.fDefaultICC) then
    DefaultICCProfile.Assign(Source.DefaultICCProfile)
  else
  if assigned(fDefaultICC) then
    FreeAndNil(fDefaultICC);

  // PXM
  PXM_Comments.Assign(Source.PXM_Comments);

  // EXIF
  IECopyEXIF(Source, self, true);
  fTIFF_Orientation := Source.fTIFF_Orientation;  // because IECopyEXIF will overwrite it

  // PS
  fPS_PaperWidth := Source.fPS_PaperWidth;
  fPS_PaperHeight := Source.fPS_PaperHeight;
  fPS_Compression := Source.fPS_Compression;
  fPS_Title := Source.fPS_Title;

  // PDF
  fPDF_PaperWidth := Source.fPDF_PaperWidth;
  fPDF_PaperHeight := Source.fPDF_PaperHeight;
  fPDF_Compression := Source.fPDF_Compression;
  fPDF_Title := Source.fPDF_Title;
  fPDF_Author := Source.fPDF_Author;
  fPDF_Subject := Source.fPDF_Subject;
  fPDF_Keywords := Source.fPDF_Keywords;
  fPDF_Creator := Source.fPDF_Creator;
  fPDF_Producer := Source.fPDF_Producer;

  // RAW
  {$ifdef IEINCLUDERAWFORMATS}
  fRAW_HalfSize := Source.fRAW_HalfSize;
  fRAW_Gamma := Source.fRAW_Gamma;
  fRAW_Bright := Source.fRAW_Bright;
  fRAW_RedScale := Source.fRAW_RedScale;
  fRAW_BlueScale := Source.fRAW_BlueScale;
  fRAW_QuickInterpolate := Source.fRAW_QuickInterpolate;
  fRAW_UseAutoWB := Source.fRAW_UseAutoWB;
  fRAW_UseCameraWB := Source.fRAW_UseCameraWB;
  fRAW_FourColorRGB := Source.fRAW_FourColorRGB;
  fRAW_Camera := Source.fRAW_Camera;
  fRAW_GetExifThumbnail := Source.fRAW_GetExifThumbnail;
  fRAW_AutoAdjustColors := Source.fRAW_AutoAdjustColors;
  fRAW_ExtraParams := Source.fRAW_ExtraParams;
  {$endif}

  // Real RAW
  fBMPRAW_ChannelOrder := Source.fBMPRAW_ChannelOrder;
  fBMPRAW_Planes := Source.fBMPRAW_Planes;
  fBMPRAW_RowAlign := Source.fBMPRAW_RowAlign;
  fBMPRAW_HeaderSize := Source.fBMPRAW_HeaderSize;
  fBMPRAW_DataFormat := Source.fBMPRAW_DataFormat;

  // XMP
  fXMP_Info := Source.fXMP_Info;
end;

// assign compression parameters
procedure TIOParamsVals.AssignCompressionInfo(Source: TIOParamsVals);
begin
  BitsPerSample := Source.BitsPerSample;
  SamplesPerPixel := Source.SamplesPerPixel;
  // TIFF
  TIFF_Compression := Source.TIFF_Compression;
  TIFF_PhotometInterpret := Source.TIFF_PhotometInterpret;
  TIFF_PlanarConf := Source.TIFF_PlanarConf;
  TIFF_Orientation := Source.TIFF_Orientation;
  TIFF_LZWDecompFunc := Source.TIFF_LZWDecompFunc;
  TIFF_LZWCompFunc := Source.TIFF_LZWCompFunc;
  fTIFF_EnableAdjustOrientation := Source.TIFF_EnableAdjustOrientation;
  fTIFF_JPEGQuality := Source.fTIFF_JPEGQuality;
  fTIFF_JPEGColorSpace := Source.fTIFF_JPEGColorSpace;
  fTIFF_FillOrder := Source.fTIFF_FillOrder;
  fTIFF_ZIPCompression := Source.fTIFF_ZIPCompression;
  fTIFF_StripCount := Source.fTIFF_StripCount;
  // GIF
  GIF_Interlaced := Source.GIF_Interlaced;
  GIF_LZWDecompFunc := Source.GIF_LZWDecompFunc;
  GIF_LZWCompFunc := Source.GIF_LZWCompFunc;
  // JPEG
  JPEG_ColorSpace := Source.JPEG_ColorSpace;
  JPEG_Quality := Source.JPEG_Quality;
  JPEG_DCTMethod := Source.JPEG_DCTMethod;
  JPEG_CromaSubsampling := Source.JPEG_CromaSubsampling;
  JPEG_OptimalHuffman := Source.JPEG_OptimalHuffman;
  JPEG_Smooth := Source.JPEG_Smooth;
  JPEG_Progressive := Source.JPEG_Progressive;
  // JPEG2000
{$IFDEF IEINCLUDEJPEG2000}
  fJ2000_ColorSpace := Source.J2000_ColorSpace;
  fJ2000_Rate := Source.J2000_Rate;
  fJ2000_ScalableBy := Source.J2000_ScalableBy;
{$ENDIF}
  // PCX
  PCX_Version := Source.PCX_Version;
  PCX_Compression := Source.PCX_Compression;
  // BMP
  BMP_Version := Source.BMP_Version;
  BMP_Compression := Source.BMP_Compression;
  BMP_HandleTransparency := Source.BMP_HandleTransparency;
  // ICO
  // CUR
  // PNG
  PNG_Interlaced := Source.PNG_Interlaced;
  PNG_Filter := Source.PNG_Filter;
  PNG_Compression := Source.PNG_Compression;
  // TGA
  TGA_Compressed := Source.TGA_Compressed;
  TGA_GrayLevel := Source.TGA_GrayLevel;
  // AVI
  // IPTC
  // PXM
  // EXIF
  // PS
  PS_Compression := Source.PS_Compression;
  // PDF
  PDF_Compression := Source.PDF_Compression;
  // DICOM
  DICOM_Range := Source.DICOM_Range;
  SetDICOM_Compression(Source.DICOM_Compression);
  fDICOM_JPEGQuality := Source.fDICOM_JPEGQuality;
  fDICOM_J2000Rate := Source.fDICOM_J2000Rate;
end;

procedure TIOParamsVals.SetEXIF_ISOSpeedRatings(index: integer; v: integer);
begin
  fEXIF_ISOSpeedRatings[index] := v;
end;

function TIOParamsVals.GetEXIF_ISOSpeedRatings(index: integer): integer;
begin
  result := fEXIF_ISOSpeedRatings[index];
end;

procedure TIOParamsVals.SetEXIF_ReferenceBlackWhite(index: integer; v: double);
begin
  fEXIF_ReferenceBlackWhite[index] := v;
end;

function TIOParamsVals.GetEXIF_ReferenceBlackWhite(index: integer): double;
begin
  result := fEXIF_ReferenceBlackWhite[index];
end;

procedure TIOParamsVals.SetEXIF_WhitePoint(index: integer; v: double);
begin
  fEXIF_WhitePoint[index] := v;
end;

function TIOParamsVals.GetEXIF_WhitePoint(index: integer): double;
begin
  result := fEXIF_WhitePoint[index];
end;

procedure TIOParamsVals.SetEXIF_PrimaryChromaticities(index: integer; v: double);
begin
  fEXIF_PrimaryChromaticities[index] := v;
end;

function TIOParamsVals.GetEXIF_PrimaryChromaticities(index: integer): double;
begin
  result := fEXIF_PrimaryChromaticities[index];
end;

procedure TIOParamsVals.SetEXIF_YCbCrCoefficients(index: integer; v: double);
begin
  fEXIF_YCbCrCoefficients[index] := v;
end;

function TIOParamsVals.GetEXIF_YCbCrCoefficients(index: integer): double;
begin
  result := fEXIF_YCbCrCoefficients[index];
end;

{!!
<FS>TIOParamsVals.SaveToFile

<FM>Declaration<FC>
procedure SaveToFile(const FileName: WideString);

<FM>Description<FN>
Saves the file format parameters to a custom meta-data file.

<FM>Example<FC>
// Save default parameters
ImageEnView1.IO.Params.TIFF_Compression := ioTIFF_LZW;
ImageEnView1.IO.Params.SaveToFile('D:\default');

...

// Load default parameters
ImageEnView1.IO.Params.LoadFromFile('C:\default');
!!}
procedure TIOParamsVals.SaveToFile(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin
  fs := nil;
  try
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    SaveToStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TIOParamsVals.LoadFromFile

<FM>Declaration<FC>
procedure LoadFromFile(const FileName: WideString);

<FM>Description<FN>
Load file formats parameters from a meta-data file (that was saved using <A TIOParamsVals.SaveToFile>.

<FM>Example<FC>
// Save default parameters
ImageEnView1.IO.Params.TIFF_Compression := ioTIFF_LZW;
ImageEnView1.IO.Params.SaveToFile('D:\default');

...

// Load default parameters
ImageEnView1.IO.Params.LoadFromFile('C:\default');
!!}
procedure TIOParamsVals.LoadFromFile(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin
  fs := nil;
  try
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

procedure TIOParamsVals.SetDpi(Value: integer);
begin
  DpiX := Value;
  DpiY := Value;
end;

{!!
<FS>TIOParamsVals.DpiX

<FM>Declaration<FC>
property DpiX: integer;

<FM>Description<FN>
Specifies the horizontal Dpi (dots per inch) of the image.

<FM>See Also<FN>
- <A TIOParamsVals.Dpi>
- <A TIOParamsVals.DpiY>
!!}
procedure TIOParamsVals.SetDpiX(Value: integer);
begin
  fDpiX := Value;
  fEXIF_XResolution := Value;
  if assigned(fImageEnIO) and assigned(fImageEnIO.fImageEnView) and (fImageEnIO.fImageEnView is TImageEnView) then
  begin
    (fImageEnIO.fImageEnView as TImageEnView).LockUpdate;
    fImageEnIO.fImageEnView.DpiX := Value;
    (fImageEnIO.fImageEnView as TImageEnView).UnLockUpdateEx;
  end;
end;

{!!
<FS>TIOParamsVals.DpiY

<FM>Declaration<FC>
property DpiY: integer;

<FM>Description<FN>
Specifies the vertical Dpi (dots per inch) of the image.

<FM>See Also<FN>
- <A TIOParamsVals.Dpi>
- <A TIOParamsVals.DpiX>
!!}
procedure TIOParamsVals.SetDpiY(Value: integer);
begin
  fDpiY := Value;
  fEXIF_YResolution := Value;
  if assigned(fImageEnIO) and assigned(fImageEnIO.fImageEnView) and (fImageEnIO.fImageEnView is TImageEnView) then
  begin
    (fImageEnIO.fImageEnView as TImageEnView).LockUpdate;
    fImageEnIO.fImageEnView.DpiY := Value;
    (fImageEnIO.fImageEnView as TImageEnView).UnLockUpdateEx;
  end;
end;

procedure TIOParamsVals.SetImageIndex(value: Integer);
begin
  fImageIndex := value;
  fTIFF_ImageIndex := value;
  fDCX_ImageIndex := value;
  fGIF_ImageIndex := value;
  fICO_ImageIndex := value;
  fCUR_ImageIndex := value;
end;


procedure TIOParamsVals.SetEnableAdjustOrientation(value: Boolean);
begin
  fEnableAdjustOrientation := value;
  fTIFF_EnableAdjustOrientation := value;
  fJPEG_EnableAdjustOrientation := value;
end;

procedure TIOParamsVals.SetImageCount(value: Integer);
begin
  fImageCount := value;
  fTIFF_ImageCount := value;
  fAVI_FrameCount := value;
  fGIF_ImageCount := value;
  {$IFDEF IEINCLUDEDIRECTSHOW}
  fMEDIAFILE_FrameCount := value;
  {$ENDIF}
end;

procedure TIOParamsVals.SetGetThumbnail(value: Boolean);
begin
  fGetThumbnail := value;
  fJPEG_GetExifThumbnail := value;
  {$ifdef IEINCLUDERAWFORMATS}
  fRAW_GetExifThumbnail := value;
  {$endif}
end;

procedure TIOParamsVals.SetIsResource(value: Boolean);
begin
  fIsResource := value;
end;

procedure TIOParamsVals.SetJPEG_GetExifThumbnail(value: Boolean);
begin
  fGetThumbnail := value;
  fJPEG_GetExifThumbnail := value;
end;

{$ifdef IEINCLUDERAWFORMATS}
procedure TIOParamsVals.SetRAW_GetExifThumbnail(value: Boolean);
begin
  fRAW_GetExifThumbnail := value;
end;
{$endif}

procedure TIOParamsVals.SetTIFF_Orientation(Value: integer);
begin
  fTIFF_Orientation := Value;
  fEXIF_Orientation := Value;
end;

procedure TIOParamsVals.SetEXIF_Orientation(Value: integer);
begin
  fEXIF_Orientation := Value;
  fTIFF_Orientation := Value;
end;

procedure TIOParamsVals.SetEXIF_XResolution(Value: double);
begin
  SetDpiX(trunc(Value));
  fEXIF_XResolution := Value;  // because SetDpiX assign truncated value
end;

procedure TIOParamsVals.SetEXIF_YResolution(Value: double);
begin
  SetDpiY(trunc(Value));
  fEXIF_YResolution := Value;  // because SetDpiX assign truncated value
end;

function TIOParamsVals.GetImageDelayTime: integer;
var
  iFrameRate : Integer;
begin
  Result := 0;
  case FileType of
    ioAVI : Result := Round(AVI_FrameDelayTime);
    ioGIF : Result := GIF_DelayTime * 10;
    {$ifdef IEINCLUDEDICOM}
    ioDICOM : begin
                //iFrameRate := 0;
                
                // Try Cine-Rate
                iFrameRate := IEStrToIntDef(DICOM_Tags.GetTagString($0018, $0040), 0);
                if iFrameRate > 0 then
                  Result := Round(1000 / iFrameRate);

                if Result = 0 then
                begin
                  // Try Recommended Display Frame Rate
                  iFrameRate := IEStrToIntDef(DICOM_Tags.GetTagString($0008, $2144), 0);
                  if iFrameRate > 0 then
                    Result := Round(1000 / iFrameRate);
                end;
                       
                if Result = 0 then
                begin
                  // Try Frame Time
                  Result := Trunc(IEStrToFloatDefA(DICOM_Tags.GetTagString($0018, $1063), 0));
                end;
              end;
    {$endif}
    {$ifdef IEINCLUDEDIRECTSHOW}
    ioWMV,
    ioMPEG  : Result := Round(MEDIAFILE_FrameDelayTime);
    {$endif}
  end;
end;


{!!
<FS>TIOParamsVals.InputICCProfile

<FM>Declaration<FC>
property InputICCProfile: <A TIEICC>;

<FM>Description<FN>
Provides access to the ICC profile associated with the current image.

If <A TIEImageEnGlobalSettings.EnableCMS> is True and ImageEn has a CMS, the <A TIOParamsVals.InputICCProfile> and <A TIOParamsVals.OutputICCProfile> are applied when you load the image.
!!}
function TIOParamsVals.GetInputICC: TIEICC;
begin
  if not assigned(fInputICC) then
    fInputICC := TIEICC.Create();
  result := fInputICC;
end;

{!!
<FS>TIOParamsVals.DefaultICCProfile

<FM>Declaration<FC>
property DefaultICCProfile: <A TIEICC>;

<FM>Description<FN>
Specifies a default ICC profile associated with the loaded image.

Note: A default ICC profile is used only when the loaded image does not have its own ICC profile.

<FM>Example<FC>
ImageEnView1.IO.Params.DefaultICCProfile.LoadFromFile('C:\Windows\System32\spool\drivers\color\AdobeRGB1998.icc');
ImageEnView1.IO.LoadFromFile('C:\input.tif');
!!}
function TIOParamsVals.GetDefaultICC: TIEICC;
begin
  if not assigned(fDefaultICC) then
    fDefaultICC := TIEICC.Create;
  result := fDefaultICC;
end;

{!!
<FS>TIOParamsVals.OutputICCProfile

<FM>Declaration<FC>
property OutputICCProfile: <A TIEICC>;

<FM>Description<FN>
Specifies the ICC profile associated with the current display system. The default profile is sRGB.

If <A TIEImageEnGlobalSettings.EnableCMS> is True and ImageEn has a CMS, the <A TIOParamsVals.InputICCProfile> and <A TIOParamsVals.OutputICCProfile> are applied when you load the image.
!!}
function TIOParamsVals.GetOutputICC: TIEICC;
begin
  if not assigned(fOutputICC) then
  begin
    fOutputICC := TIEICC.Create;
    fOutputICC.Assign_sRGBProfile;
  end;
  result := fOutputICC;
end;

{!!
<FS>TIOParamsVals.ImagingAnnot

<FM>Declaration<FC>
property ImagingAnnot: <A TIEImagingAnnot>;

<FM>Description<FN>
Provides access to the (Wang) imaging annotations loaded (or to be saved) from a TIFF.

Using the <A TIEImagingAnnot> object you can create new objects, copy to a <A TImageEnVect> (as vectorial objects), copy from a <A TImageEnVect> (from vectorial objects) or just draw onto the bitmap.

<FM>Example<FC>
// Load an image and all annotations from input.tif . This allows the annotations to be edited:
ImageEnVect1.IO.LoadFromFile('C:\input.tif');
ImageEnVect1.IO.Params.ImagingAnnot.CopyToTImageEnVect( ImageEnVect1 );

// Load an image and all annotations from input.tif, but just draw annotation on the image (display only, cannot be edited):
ImageEnVect1.IO.LoadFromFile('C:\input.tif');
ImageEnVect1.IO.Params.ImagingAnnot.DrawToBitmap( ImageEnVect1.IEBitmap, True );
ImageEnVect1.Update;
!!}
{$ifdef IEINCLUDEIMAGINGANNOT}
function TIOParamsVals.GetImagingAnnot: TIEImagingAnnot;
begin
  if not assigned(fImagingAnnot) then
  begin
    fImagingAnnot := TIEImagingAnnot.Create;
    fImagingAnnot.Parent := self;
  end;
  result := fImagingAnnot;
end;
{$endif}

{!!
<FS>TIOParamsVals.SaveToStream

<FM>Declaration<FC>
procedure SaveToStream(Stream: TStream);

<FM>Description<FN>
Saves the file format parameters to stream in a custom meta-data format.
!!}
// the first integer is the version:
// 44: 4.0.2
// 45: 4.0.3
// 46: 4.1.1
// 47: 4.1.1-C
// 48: 4.3.0
// 49: 4.3.1
// 50: 5.0.0
// 51: 5.0.6
// 52: 5.2.0
procedure TIOParamsVals.SaveToStream(Stream: TStream);
const
  VERSION: integer = 52;
var
  i32: integer;
  ms: TMemoryStream;
  ab: boolean;
  pr: TProgressRec;
  dicomCompression: TIEDicomCompression;
begin
  Stream.Write(VERSION, sizeof(integer));

  // Dictionary
  IESaveStringToStreamW(Stream, fDict.Dump());

  // EXIF (embedded in a TIFF). Must be the first loading so next tags can be set correctly
  ms := TMemoryStream.Create;
  try
    ab := false;
    pr.fOnProgress := nil;
    pr.Sender := nil;
    pr.Aborting := @ab;
    TIFFWriteStream(ms, false, nil, self, pr);
    i32 := ms.Size;
    Stream.Write(i32, sizeof(integer));
    IECopyFrom(Stream, ms, 0);
  finally
    ms.Free;
  end;

  // Generics
  IESaveStringToStream(Stream, AnsiString(fFileName));
  Stream.Write(fFileType, sizeof(TIOFileType));
  Stream.Write(fBitsPerSample, sizeof(integer));
  Stream.Write(fSamplesPerPixel, sizeof(integer));
  Stream.Write(fWidth, sizeof(integer));
  Stream.Write(fHeight, sizeof(integer));
  Stream.Write(fDpiX, sizeof(integer));
  Stream.Write(fDpiY, sizeof(integer));
  Stream.Write(fColorMapCount, sizeof(integer));
  if fColorMapCount > 0 then
    Stream.Write(fColorMap^, fColorMapCount * sizeof(TRGB));
  Stream.Write(fImageIndex, sizeof(integer));
  Stream.Write(fImageCount, sizeof(integer));
  Stream.Write(fGetThumbnail, sizeof(boolean));
  Stream.Write(fEnableAdjustOrientation, sizeof(boolean));
  Stream.Write(fIsResource, sizeof(boolean));

  // TIFF
  Stream.Write(fTIFF_Compression, sizeof(TIOTIFFCompression));
  Stream.Write(fTIFF_ImageIndex, sizeof(integer));
  Stream.Write(fTIFF_PhotometInterpret, sizeof(TIOTIFFPhotometInterpret));
  Stream.Write(fTIFF_PlanarConf, sizeof(integer));
  Stream.Write(fTIFF_NewSubfileType, sizeof(integer));
  Stream.Write(fTIFF_XPos, sizeof(integer));
  Stream.Write(fTIFF_YPos, sizeof(integer));
  IESaveStringToStream(Stream, fTIFF_DocumentName);
  IESaveStringToStream(Stream, fTIFF_ImageDescription);
  IESaveStringToStream(Stream, fTIFF_PageName);
  Stream.Write(fTIFF_PageNumber, sizeof(integer));
  Stream.Write(fTIFF_PageCount, sizeof(integer));
  Stream.Write(fTIFF_ImageCount, sizeof(integer));
  Stream.Write(fTIFF_Orientation, sizeof(integer));
  Stream.Write(fTIFF_JPEGQuality, sizeof(integer));
  Stream.Write(fTIFF_JPEGColorSpace, sizeof(TIOJPEGColorSpace));
  Stream.Write(fTIFF_FillOrder, sizeof(integer));
  Stream.Write(fTIFF_ZIPCompression, sizeof(integer));
  Stream.Write(fTIFF_SubIndex, sizeof(integer));
  Stream.Write(fTIFF_ByteOrder, sizeof(TIOByteOrder));
  Stream.Write(fTIFF_GetTile, sizeof(integer));
  Stream.Write(fTIFF_StripCount, sizeof(integer));
  Stream.Write(fTIFF_BigTIFF, sizeof(boolean));
  //
  i32 := length(fTIFF_PhotoshopImageResources);
  Stream.Write(i32, sizeof(integer));
  Stream.Write(fTIFF_PhotoshopImageResources[0], i32);
  //
  i32 := length(fTIFF_PhotoshopImageSourceData);
  Stream.Write(i32, sizeof(integer));
  Stream.Write(fTIFF_PhotoshopImageSourceData[0], i32);

  // GIF
  IESaveStringToStream(Stream, fGIF_Version);
  Stream.Write(fGIF_ImageIndex, sizeof(integer));
  Stream.Write(fGIF_XPos, sizeof(integer));
  Stream.Write(fGIF_YPos, sizeof(integer));
  Stream.Write(fGIF_DelayTime, sizeof(integer));
  Stream.Write(fGIF_FlagTranspColor, sizeof(boolean));
  Stream.Write(fGIF_TranspColor, sizeof(TRGB));
  Stream.Write(fGIF_Interlaced, sizeof(boolean));
  Stream.Write(fGIF_WinWidth, sizeof(integer));
  Stream.Write(fGIF_WinHeight, sizeof(integer));
  Stream.Write(fGIF_Background, sizeof(TRGB));
  Stream.Write(fGIF_Ratio, sizeof(integer));
  Stream.Write(fGIF_ImageCount, sizeof(integer));
  IESaveStringListToStream(Stream, fGIF_Comments);
  Stream.Write(fGIF_Action, sizeof(TIEGIFAction));
  Stream.Write(fGIF_RAWLoad, sizeof(boolean));

  // JPEG
  Stream.Write(fJPEG_ColorSpace, sizeof(TIOJPEGColorSpace));
  Stream.Write(fJPEG_Quality, sizeof(integer));
  Stream.Write(fJPEG_DCTMethod, sizeof(TIOJPEGDCTMethod));
  Stream.Write(fJPEG_OptimalHuffman, sizeof(boolean));
  Stream.Write(fJPEG_Smooth, sizeof(integer));
  Stream.Write(fJPEG_Progressive, sizeof(boolean));
  Stream.Write(fJPEG_Scale, sizeof(TIOJPEGScale));
  fJPEG_MarkerList.SaveToStream(Stream);
  Stream.Write(fOriginalWidth, sizeof(integer));    // since 3.0.2 common to all formats (file position invaried due file compatibility)
  Stream.Write(fOriginalHeight, sizeof(integer));   // since 3.0.2 common to all formats (file position invaried due file compatibility)
  Stream.Write(fJPEG_EnableAdjustOrientation, sizeof(boolean));
  Stream.Write(fJPEG_GetExifThumbnail, sizeof(boolean));
  Stream.Write(fJPEG_CromaSubsampling, sizeof(TIOJPEGCromaSubsampling));

  // JPEG2000
{$IFDEF IEINCLUDEJPEG2000}
  Stream.Write(fJ2000_ColorSpace, sizeof(TIOJ2000ColorSpace));
  Stream.Write(fJ2000_Rate, sizeof(double));
  Stream.Write(fJ2000_ScalableBy, sizeof(TIOJ2000ScalableBy));
{$ENDIF}

  // PCX
  Stream.Write(fPCX_Version, sizeof(integer));
  Stream.Write(fPCX_Compression, sizeof(TIOPCXCompression));

  // BMP
  Stream.Write(fBMP_Version, sizeof(TIOBMPVersion));
  Stream.Write(fBMP_Compression, sizeof(TIOBMPCompression));
  Stream.Write(fBMP_HandleTransparency, sizeof(boolean));

  // ICO
  Stream.Write(fICO_ImageIndex, sizeof(integer));
  Stream.Write(fICO_Background, sizeof(TRGB));
  Stream.Write(ICO_Sizes[0], sizeof(TIOICOSizes));
  Stream.Write(ICO_BitCount[0], sizeof(TIOICOBitCount));

  // CUR
  Stream.Write(fCUR_ImageIndex, sizeof(integer));
  Stream.Write(fCUR_XHotSpot, sizeof(integer));
  Stream.Write(fCUR_YHotSpot, sizeof(integer));
  Stream.Write(fCUR_Background, sizeof(TRGB));

  // PNG
  Stream.Write(fPNG_Interlaced, sizeof(boolean));
  Stream.Write(fPNG_Background, sizeof(TRGB));
  Stream.Write(fPNG_Filter, sizeof(TIOPNGFilter));
  Stream.Write(fPNG_Compression, sizeof(integer));
  IESaveStringListToStream(Stream, fPNG_TextKeys);
  IESaveStringListToStream(Stream, fPNG_TextValues);

  // DICOM
  {$ifdef IEINCLUDEDICOM}
  DICOM_Tags.SaveToStream(Stream);
  Stream.Write(fDICOM_WindowCenterOffset, sizeof(double));
  Stream.Write(fDICOM_Range, sizeof(TIEDicomRange));
  dicomCompression := DICOM_Compression;
  Stream.Write(dicomCompression, sizeof(TIEDicomCompression));
  Stream.Write(fDICOM_JPEGQuality, sizeof(integer));
  Stream.Write(fDICOM_J2000Rate, sizeof(double));
  Stream.Write(fDICOM_RescaleIntercept, sizeof(double));
  Stream.Write(fDICOM_RescaleSlope, sizeof(double));
  {$endif}

  // PSD
  Stream.Write(fPSD_LoadLayers, sizeof(boolean));
  Stream.Write(fPSD_ReplaceLayers, sizeof(boolean));
  Stream.Write(fPSD_HasPremultipliedAlpha, sizeof(boolean));
  Stream.Write(fPSD_LargeDocumentFormat, sizeof(boolean));
  IESaveStringToStream(Stream, fPSD_SelectLayer);

  // HDP
  Stream.Write(fHDP_ImageQuality, sizeof(double));
  Stream.Write(fHDP_Lossless, sizeof(boolean));

  // TGA
  Stream.Write(fTGA_XPos, sizeof(integer));
  Stream.Write(fTGA_YPos, sizeof(integer));
  Stream.Write(fTGA_Compressed, sizeof(boolean));
  IESaveStringToStream(Stream, fTGA_Descriptor);
  IESaveStringToStream(Stream, fTGA_Author);
  Stream.Write(fTGA_Date, sizeof(TDateTime));
  IESaveStringToStream(Stream, fTGA_ImageName);
  Stream.Write(fTGA_Background, sizeof(TRGB));
  Stream.Write(fTGA_AspectRatio, sizeof(double));
  Stream.Write(fTGA_Gamma, sizeof(double));
  Stream.Write(fTGA_GrayLevel, sizeof(boolean));

  // IPTC
  fIPTC_Info.SaveToStream(Stream);

  // PXM
  IESaveStringListToStream(Stream, fPXM_Comments);

  // AVI
  Stream.Write(fAVI_FrameCount, sizeof(integer));
  Stream.Write(fAVI_FrameDelayTIme, sizeof(double));

  // PS
  Stream.Write(fPS_PaperWidth, sizeof(integer));
  Stream.Write(fPS_PaperHeight, sizeof(integer));
  Stream.Write(fPS_Compression, sizeof(TIOPSCompression));
  IESaveStringToStream(Stream, fPS_Title);

  // PDF
  Stream.Write(fPDF_PaperWidth, sizeof(integer));
  Stream.Write(fPDF_PaperHeight, sizeof(integer));
  Stream.Write(fPDF_Compression, sizeof(TIOPDFCompression));
  IESaveStringToStream(Stream, fPDF_Title);
  IESaveStringToStream(Stream, fPDF_Author);
  IESaveStringToStream(Stream, fPDF_Subject);
  IESaveStringToStream(Stream, fPDF_Keywords);
  IESaveStringToStream(Stream, fPDF_Creator);
  IESaveStringToStream(Stream, fPDF_Producer);

  // DCX
  Stream.Write(fDCX_ImageIndex, sizeof(integer));

  {$ifdef IEINCLUDEIMAGINGANNOT}
  // imaging annotations
  ImagingAnnot.SaveToStream(Stream);
  {$endif}

  // ImageEn Annotations
  ImageEnAnnot.SaveToStream(Stream);

  // ICC
  InputICCProfile.SaveToStream(Stream, false);
  OutputICCProfile.SaveToStream(Stream, false);
  DefaultICCProfile.SaveToStream(Stream, false);

  // RAW
  {$ifdef IEINCLUDERAWFORMATS}
  Stream.Write(fRAW_HalfSize, sizeof(boolean));
  Stream.Write(fRAW_Gamma, sizeof(double));
  Stream.Write(fRAW_Bright, sizeof(double));
  Stream.Write(fRAW_RedScale, sizeof(double));
  Stream.Write(fRAW_BlueScale, sizeof(double));
  Stream.Write(fRAW_QuickInterpolate, sizeof(boolean));
  Stream.Write(fRAW_UseAutoWB, sizeof(boolean));
  Stream.Write(fRAW_UseCameraWB, sizeof(boolean));
  Stream.Write(fRAW_FourColorRGB, sizeof(boolean));
  IESaveStringToStream(Stream, fRAW_Camera);
  Stream.Write(fRAW_GetExifThumbnail, sizeof(boolean));
  Stream.Write(fRAW_AutoAdjustColors, sizeof(boolean));
  IESaveStringToStream(Stream, fRAW_ExtraParams);
  {$endif}
  {$ifdef IEINCLUDEDIRECTSHOW}
  Stream.Write(fMEDIAFILE_FrameCount, sizeof(integer));
  Stream.Write(fMEDIAFILE_FrameDelayTime, sizeof(double));
  {$endif}

  // real RAW
  Stream.Write(fBMPRAW_ChannelOrder, sizeof(TIOBMPRAWChannelOrder));
  Stream.Write(fBMPRAW_Planes, sizeof(TIOBMPRAWPlanes));
  Stream.Write(fBMPRAW_RowAlign, sizeof(integer));
  Stream.Write(fBMPRAW_HeaderSize, sizeof(integer));
  Stream.Write(fBMPRAW_DataFormat, sizeof(TIOBMPRAWDataFormat));

end;

{!!
<FS>TIOParamsVals.LoadFromStream

<FM>Declaration<FC>
procedure LoadFromStream(Stream: TStream);

<FM>Description<FN>
Load file formats parameters from a stream that was filled using <A TIOParamsVals.SaveToStream>.
!!}
// read params stream
procedure TIOParamsVals.LoadFromStream(Stream: TStream);
var
  ver, i32: integer;
  b: byte;
  i: integer;
  idummy: integer;
  ss: AnsiString;
  ms: TMemoryStream;
  ab: boolean;
  pr: TProgressRec;
  alpha: TIEMask;
  dicomCompression: TIEDicomCompression;
  ws: WideString;
begin
  Stream.Read(ver, sizeof(integer));

  // Dictionary
  fDict.Clear();
  if ver >= 51 then
  begin
    IELoadStringFromStreamW(Stream, ws);
    fDict.Parse(ws);
  end;

  if ver >= 43 then
  begin
    alpha := nil;
    ms := TMemoryStream.Create;
    try
      Stream.Read(i32, sizeof(integer));
      if i32 > 0 then
        ms.CopyFrom(Stream, i32);
      ms.Position := 0;
      ab := false;
      pr.fOnProgress := nil;
      pr.Sender := nil;
      pr.Aborting := @ab;
      alpha := TIEMask.Create;
      TIFFReadStream(nil, ms, i32, self, pr, true, alpha, false, true, false, false);
    finally
      alpha.Free;
      ms.Free;
    end;
  end;

  // generics
  IELoadStringFromStream(Stream, ss);
  fFileName := WideString(ss);
  if ver < 2 then
  begin
    // in versions 0 and 1, TIOFileType was a byte
    Stream.Read(b, 1);
    fFileType := TIOFileType(b);
  end
  else
    Stream.Read(fFileType, sizeof(TIOFileType));
  Stream.Read(fBitsPerSample, sizeof(integer));
  Stream.Read(fSamplesPerPixel, sizeof(integer));
  Stream.Read(fWidth, sizeof(integer));
  Stream.Read(fHeight, sizeof(integer));
  Stream.Read(fDpiX, sizeof(integer));
  Stream.Read(fDpiY, sizeof(integer));
  FreeColorMap;
  Stream.Read(fColorMapCount, sizeof(integer));
  if fColorMapCount > 0 then
  begin
    getmem(fColorMap, fColorMapCount * sizeof(TRGB));
    Stream.Read(fColorMap^, fColorMapCount * sizeof(TRGB));
  end;
  if ver >= 25 then
  begin
    Stream.Read(fImageIndex, sizeof(integer));
    Stream.Read(fImageCount, sizeof(integer));
    Stream.Read(fGetThumbnail, sizeof(boolean));
  end;
  if ver >= 38 then
    Stream.Read(fEnableAdjustOrientation, sizeof(boolean));

  if ver >= 44 then
    Stream.Read(fIsResource, sizeof(boolean));

  // TIFF
  Stream.Read(fTIFF_Compression, sizeof(TIOTIFFCompression));
  Stream.Read(fTIFF_ImageIndex, sizeof(integer));
  Stream.Read(fTIFF_PhotometInterpret, sizeof(TIOTIFFPhotometInterpret));
  Stream.Read(fTIFF_PlanarConf, sizeof(integer));
  if ver >= 47 then
    Stream.Read(fTIFF_NewSubfileType, sizeof(integer));
  Stream.Read(fTIFF_XPos, sizeof(integer));
  Stream.Read(fTIFF_YPos, sizeof(integer));
  IELoadStringFromStream(Stream, fTIFF_DocumentName);
  IELoadStringFromStream(Stream, fTIFF_ImageDescription);
  IELoadStringFromStream(Stream, fTIFF_PageName);
  Stream.Read(fTIFF_PageNumber, sizeof(integer));
  Stream.Read(fTIFF_PageCount, sizeof(integer));
  if ver >= 4 then
  begin
    Stream.Read(fTIFF_ImageCount, sizeof(integer));
    if ver >= 6 then
      Stream.Read(fTIFF_Orientation, sizeof(integer));
  end;
  if ver >= 10 then
    Stream.Read(fTIFF_JPEGQuality, sizeof(integer));
  if ver >= 16 then
    Stream.Read(fTIFF_JPEGColorSpace, sizeof(TIOJPegColorSpace));
  if ver >= 26 then
    Stream.Read(fTIFF_FillOrder, sizeof(integer));
  if ver >= 30 then
    Stream.Read(fTIFF_ZIPCompression, sizeof(integer));
  if ver >= 31 then
    Stream.Read(fTIFF_SubIndex, sizeof(integer));
  if ver >= 32 then
    Stream.Read(fTIFF_ByteOrder, sizeof(TIOByteOrder));
  if ver >= 40 then
    Stream.Read(fTIFF_GetTile, sizeof(integer));
  if ver >= 46 then
    Stream.Read(fTIFF_StripCount, sizeof(integer));
  if ver >= 50 then
  begin
    Stream.Read(i32, sizeof(integer));
    SetLength(fTIFF_PhotoshopImageResources, i32);
    Stream.Read(fTIFF_PhotoshopImageResources[0], i32);

    Stream.Read(i32, sizeof(integer));
    SetLength(fTIFF_PhotoshopImageSourceData, i32);
    Stream.Read(fTIFF_PhotoshopImageSourceData[0], i32);
  end;
  if ver >= 51 then
    Stream.Read(fTIFF_BigTIFF, sizeof(boolean));

  // GIF
  IELoadStringFromStream(Stream, fGIF_Version);
  Stream.Read(fGIF_ImageIndex, sizeof(integer));
  Stream.Read(fGIF_XPos, sizeof(integer));
  Stream.Read(fGIF_YPos, sizeof(integer));
  Stream.Read(fGIF_DelayTime, sizeof(integer));
  Stream.Read(fGIF_FlagTranspColor, sizeof(boolean));
  Stream.Read(fGIF_TranspColor, sizeof(TRGB));
  Stream.Read(fGIF_Interlaced, sizeof(boolean));
  Stream.Read(fGIF_WinWidth, sizeof(integer));
  Stream.Read(fGIF_WinHeight, sizeof(integer));
  Stream.Read(fGIF_Background, sizeof(TRGB));
  Stream.Read(fGIF_Ratio, sizeof(integer));
  if ver >= 4 then
    Stream.Read(fGIF_ImageCount, sizeof(integer));
  if ver >= 9 then
    IELoadStringListFromStream(Stream, fGIF_Comments);
  if ver >= 12 then
    Stream.Read(fGIF_Action, sizeof(TIEGIFAction));
  if ver >= 49 then
    Stream.Read(fGIF_RAWLoad, sizeof(boolean));

  // JPEG
  Stream.Read(fJPEG_ColorSpace, sizeof(TIOJPEGColorSpace));
  Stream.Read(fJPEG_Quality, sizeof(integer));
  Stream.Read(fJPEG_DCTMethod, sizeof(TIOJPEGDCTMethod));
  Stream.Read(fJPEG_OptimalHuffman, sizeof(boolean));
  Stream.Read(fJPEG_Smooth, sizeof(integer));
  Stream.Read(fJPEG_Progressive, sizeof(boolean));
  if ver >= 1 then
    Stream.Read(fJPEG_Scale, sizeof(TIOJPEGScale));
  if ver >= 4 then
    fJPEG_MarkerList.LoadFromStream(Stream);
  if ver >= 7 then
  begin
    Stream.Read(fOriginalWidth, sizeof(integer));   // 3.0.2: common to all file formats
    Stream.Read(fOriginalHeight, sizeof(integer));  // 3.0.2: common to all file formats
  end;
  if ver >= 20 then
  begin
    Stream.Read(fJPEG_EnableAdjustOrientation, sizeof(boolean));
    Stream.Read(fJPEG_GetExifThumbnail, sizeof(boolean));
  end;
  if ver >= 39 then
    Stream.Read(fJPEG_CromaSubsampling, sizeof(TIOJPEGCromaSubsampling));

  // JPEG2000
{$IFDEF IEINCLUDEJPEG2000}
  if ver >= 8 then
  begin
    Stream.Read(fJ2000_ColorSpace, sizeof(TIOJ2000ColorSpace));
    Stream.Read(fJ2000_Rate, sizeof(double));
    Stream.Read(fJ2000_ScalableBy, sizeof(TIOJ2000ScalableBy));
  end;
{$ENDIF}

  // PCX
  Stream.Read(fPCX_Version, sizeof(integer));
  Stream.Read(fPCX_Compression, sizeof(TIOPCXCompression));

  // BMP
  Stream.Read(fBMP_Version, sizeof(TIOBMPVersion));
  Stream.Read(fBMP_Compression, sizeof(TIOBMPCompression));
  if ver>=28 then
    Stream.Read(fBMP_HandleTransparency, sizeof(boolean));

  // ICO
  if ver = 0 then
  begin
    Stream.Read(idummy, sizeof(integer));
    Stream.Read(idummy, sizeof(integer));
    fICO_ImageIndex := 0;
  end
  else
  begin
    Stream.Read(fICO_ImageIndex, sizeof(integer));
    if ver >= 3 then
      Stream.Read(fICO_Background, sizeof(TRGB));
    if ver >= 11 then
    begin
      Stream.Read(ICO_Sizes[0], sizeof(TIOICOSizes));
      Stream.Read(ICO_BitCount[0], sizeof(TIOICOBitCount));
    end;
  end;

  // CUR
  if ver = 0 then
  begin
    Stream.Read(idummy, sizeof(integer));
    Stream.Read(idummy, sizeof(integer));
    fCUR_ImageIndex := 0;
    fCUR_XHotSpot := 0;
    fCUR_YHotSpot := 0;
  end
  else
  begin
    Stream.Read(fCUR_ImageIndex, sizeof(integer));
    Stream.Read(fCUR_XHotSpot, sizeof(integer));
    Stream.Read(fCUR_YHotSpot, sizeof(integer));
    if ver >= 3 then
      Stream.Read(fCUR_Background, sizeof(TRGB));
  end;

  // PNG
  Stream.Read(fPNG_Interlaced, sizeof(boolean));
  Stream.Read(fPNG_Background, sizeof(TRGB));
  Stream.Read(fPNG_Filter, sizeof(TIOPNGFilter));
  Stream.Read(fPNG_Compression, sizeof(integer));
  if ver >= 23 then
  begin
    IELoadStringListFromStream(Stream, fPNG_TextKeys);
    IELoadStringListFromStream(Stream, fPNG_TextValues);
  end;

  // DICOM
  {$ifdef IEINCLUDEDICOM}
  if ver >= 37 then
    DICOM_Tags.LoadFromStream(Stream);
  if ver >= 48 then
  begin
    Stream.Read(fDICOM_WindowCenterOffset, sizeof(double));
    Stream.Read(fDICOM_Range, sizeof(TIEDicomRange));
  end;
  if ver >= 50 then
  begin
    Stream.Read(dicomCompression, sizeof(TIEDicomCompression));
    SetDICOM_Compression(dicomCompression);
    Stream.Read(fDICOM_JPEGQuality, sizeof(integer));
    Stream.Read(fDICOM_J2000Rate, sizeof(double));
  end;
  if ver >= 52 then
  begin
    Stream.Read(fDICOM_RescaleIntercept, sizeof(double));
    Stream.Read(fDICOM_RescaleSlope, sizeof(double));
  end;
  {$endif}

  // PSD
  if ver >=27 then
  begin
    Stream.Read(fPSD_LoadLayers, sizeof(boolean));
    if ver >= 34 then
      Stream.Read(fPSD_ReplaceLayers, sizeof(boolean));
    if ver >= 36 then
      Stream.Read(fPSD_HasPremultipliedAlpha, sizeof(boolean));
    if ver >= 51 then
    begin
      Stream.Read(fPSD_LargeDocumentFormat, sizeof(boolean));
      IELoadStringFromStream(Stream, fPSD_SelectLayer);
    end;
  end;

  // HDP
  if ver >=41 then
  begin
    Stream.Read(fHDP_ImageQuality, sizeof(double));
    Stream.Read(fHDP_Lossless, sizeof(boolean));
  end;

  // TGA
  if ver >= 3 then
  begin
    Stream.Read(fTGA_XPos, sizeof(integer));
    Stream.Read(fTGA_YPos, sizeof(integer));
    Stream.Read(fTGA_Compressed, sizeof(boolean));
    IELoadStringFromStream(Stream, fTGA_Descriptor);
    IELoadStringFromStream(Stream, fTGA_Author);
    Stream.Read(fTGA_Date, sizeof(TDateTime));
    IELoadStringFromStream(Stream, fTGA_ImageName);
    Stream.Read(fTGA_Background, sizeof(TRGB));
    Stream.Read(fTGA_AspectRatio, sizeof(double));
    Stream.Read(fTGA_Gamma, sizeof(double));
    Stream.Read(fTGA_GrayLevel, sizeof(boolean));
  end;

  // IPTC
  if ver >= 4 then
    fIPTC_Info.LoadFromStream(Stream);

  // PXM
  if ver >= 5 then
    IELoadStringListFromStream(Stream, fPXM_Comments);

  // AVI
  if ver >= 6 then
  begin
    Stream.Read(fAVI_FrameCount, sizeof(integer));
    if ver >= 45 then
      Stream.Read(fAVI_FrameDelayTime, sizeof(double))
    else
    begin
      Stream.Read(i, sizeof(integer));
      fAVI_FrameDelayTime := i;
    end;
  end;

  // PS
  if ver >= 14 then
  begin
    Stream.Read(fPS_PaperWidth, sizeof(integer));
    Stream.Read(fPS_PaperHeight, sizeof(integer));
    Stream.Read(fPS_Compression, sizeof(TIOPSCompression));
    IELoadStringFromStream(Stream, fPS_Title);
  end;

  // PDF
  if ver >= 14 then
  begin
    Stream.Read(fPDF_PaperWidth, sizeof(integer));
    Stream.Read(fPDF_PaperHeight, sizeof(integer));
    Stream.Read(fPDF_Compression, sizeof(TIOPDFCompression));
    if ver >= 15 then
    begin
      IELoadStringFromStream(Stream, fPDF_Title);
      IELoadStringFromStream(Stream, fPDF_Author);
      IELoadStringFromStream(Stream, fPDF_Subject);
      IELoadStringFromStream(Stream, fPDF_Keywords);
      IELoadStringFromStream(Stream, fPDF_Creator);
      IELoadStringFromStream(Stream, fPDF_Producer);
    end;
  end;

  // DCX
  if ver>=18 then
    Stream.Read(fDCX_ImageIndex, sizeof(integer));

  {$ifdef IEINCLUDEIMAGINGANNOT}
  // Imagning annotations
  if ver >= 13 then
    ImagingAnnot.LoadFromStream(Stream);
  {$endif}

  // ImageEn annotations
  if ver >= 51 then
    ImageEnAnnot.LoadFromStream(Stream);

  // ICC
  if ver >=17 then
  begin
    InputICCProfile.LoadFromStream(Stream, false);
    OutputICCProfile.LoadFromStream(Stream, false);
    if ver >=33 then
      DefaultICCProfile.LoadFromStream(Stream, false);
  end;

  // RAW
  if ver>=18 then
  begin
    {$ifdef IEINCLUDERAWFORMATS}
    Stream.Read(fRAW_HalfSize, sizeof(boolean));
    Stream.Read(fRAW_Gamma, sizeof(double));
    Stream.Read(fRAW_Bright, sizeof(double));
    Stream.Read(fRAW_RedScale, sizeof(double));
    Stream.Read(fRAW_BlueScale, sizeof(double));
    Stream.Read(fRAW_QuickInterpolate, sizeof(boolean));
    Stream.Read(fRAW_UseAutoWB, sizeof(boolean));
    Stream.Read(fRAW_UseCameraWB, sizeof(boolean));
    Stream.Read(fRAW_FourColorRGB, sizeof(boolean));
    IELoadStringFromStream(Stream, fRAW_Camera);
    Stream.Read(fRAW_GetExifThumbnail, sizeof(boolean));
    if ver>=22 then
      Stream.Read(fRAW_AutoAdjustColors, sizeof(boolean));
    if ver>=35 then
      IELoadStringFromStream(Stream, fRAW_ExtraParams);
    {$endif}
  end;

  // Media File
  {$ifdef IEINCLUDEDIRECTSHOW}
  if ver>=21 then
  begin
    Stream.Read(fMEDIAFILE_FrameCount, sizeof(integer));
    if ver >= 45 then
      Stream.Read(fMEDIAFILE_FrameDelayTime, sizeof(double))
    else
    begin
      Stream.Read(i, sizeof(integer));
      fMEDIAFILE_FrameDelayTime := i;
    end;
  end;
  {$endif}

  // Real RAW
  if ver>=23 then
  begin
    Stream.Read(fBMPRAW_ChannelOrder, sizeof(TIOBMPRAWChannelOrder));
    Stream.Read(fBMPRAW_Planes, sizeof(TIOBMPRAWPlanes));
    Stream.Read(fBMPRAW_RowAlign, sizeof(integer));
    Stream.Read(fBMPRAW_HeaderSize, sizeof(integer));
    if ver>=42 then
      Stream.Read(fBMPRAW_DataFormat, sizeof(TIOBMPRAWDataFormat));
  end;
end;



{!!
<FS>TIOParamsVals.FileTypeStr

<FM>Declaration<FC>
property FileTypeStr: String; (Read-only)

<FM>Description<FN>
Returns a textual description of the <L TIOParamsVals.FileType>current file type</L> and other file format specific properties.

For example, if <A TIOParamsVals.FileType> is ioTIFF and <A TIOParamsVals.TIFF_Compression> is ioTIFF_G4FAX, FileTypeStr returns the string: 'TIFF Bitmap (TIFF)  Group 4 Fax'.

<FM>Example<FC>
if OpenImageEnDialog1.Execute then
begin
  ImageEnView1.IO.LoadFromFile(OpenImageEnDialog1.FileName);
  ShowMessage( ImageEnView1.IO.Params.FileTypeStr );
end;
!!}
// returns the type of last loaded file
function TIOParamsVals.GetFileTypeStr: string;
var
  fi: TIEFileFormatInfo;
begin
  // main
  fi := IEFileFormatGetInfo(fFileType);
  if assigned(fi) then
    with fi do
      result := FullName + ' (' + UpperCase(SuitableExtension) + ')'
  else
    result := '';
  // sub formats
  case fFileType of
    ioTIFF:
      case TIFF_Compression of
        ioTIFF_UNCOMPRESSED:  result := result + ' Uncompressed';
        ioTIFF_CCITT1D:       result := result + ' Huffman';
        ioTIFF_G3FAX1D:       result := result + ' Group 3 Fax';
        ioTIFF_G3FAX2D:       result := result + ' Group 3 Fax 2D';
        ioTIFF_G4FAX:         result := result + ' Group 4 Fax';
        ioTIFF_LZW:           result := result + ' LZW';
        ioTIFF_OLDJPEG:       result := result + ' Jpeg v.5';
        ioTIFF_JPEG:          result := result + ' Jpeg';
        ioTIFF_PACKBITS:      result := result + ' PackBits RLE';
        ioTIFF_ZIP:           result := result + ' ZIP';
        ioTIFF_DEFLATE:       result := result+' Deflate';
      end;
    ioJPEG:
      case JPEG_ColorSpace of
        ioJPEG_GRAYLEV: result := result + ' Gray level';
        ioJPEG_YCbCr:   result := result + ' YCbCr';
        ioJPEG_CMYK:    result := result + ' CMYK';
        ioJPEG_YCbCrK:  result := result + ' YCbCrK';
      end;
{$IFDEF IEINCLUDEJPEG2000}
    ioJP2,
      ioJ2k:
      case fJ2000_ColorSpace of
        ioJ2000_GRAYLEV:  result := result + ' Gray level';
        ioJ2000_RGB:      result := result + ' RGB';
        ioJ2000_YCbCr:    result := result + ' YCbCr';
      end;
{$ENDIF}
    ioPCX:
      if PCX_Compression = ioPCX_UNCOMPRESSED then
        result := result + ' Uncompressed';
    ioBMP:
      case BMP_Version of
        ioBMP_BM:       result := result + ' ver.1';
        ioBMP_BM3:      result := result + ' ver.3';
        ioBMP_BMOS2V1:  result := result + ' OS/2 v.1';
        ioBMP_BMOS2V2:  result := result + ' OS/2 v.2';
      end;
    ioGIF:
      if GIF_Interlaced then
        result := result + ' Interlaced';
{$IFDEF IEINCLUDEPNG}
    ioPNG:
      if PNG_Interlaced then
        result := result + ' Interlaced';
{$ENDIF}
    ioTGA:
      if TGA_Compressed then
        result := result + ' Compressed'
      else
        result := result + ' Uncompressed';
{$ifdef IEINCLUDERAWFORMATS}
    ioRAW:
      result := result+' ('+string(RAW_Camera)+')';
{$endif}
  end;

end;



function TIOParamsVals.GetProperty(const Prop: WideString): WideString;
var
  ss : WideString;
  q : integer;
begin
  ss := UpperCase(IERemoveCtrlCharsW(Prop));
  if ss = 'FILENAME' then
    result := FileName
  else
  if ss = 'FILETYPESTR' then
    result := WideString(FileTypeStr)
  else
  if ss = 'FILETYPE' then
    result := IntToStr(FileType)
  else
  if ss = 'BITSPERSAMPLE' then
    result := IntToStr(BitsPerSample)
  else
  if ss = 'SAMPLESPERPIXEL' then
    result := IntToStr(SamplesPerPixel)
  else
  if ss = 'WIDTH' then
    result := IntToStr(Width)
  else
  if ss = 'HEIGHT' then
    result := IntToStr(Height)
  else
  if ss = 'DPIX' then
    result := IntToStr(DpiX)
  else
  if ss = 'DPIY' then
    result := IntToStr(DpiY)
  else 
  if Copy(ss, 1, 12)='COLORMAPITEM' then
  begin
    q := StrToIntDef(Copy(ss, 13, length(ss)), 0);
    result := IERGB2StrW(ColorMap^[q]);
  end
  else
  if ss = 'COLORMAPCOUNT' then
    result := IntToStr(ColorMapCount)
  else
  if ss = 'ISNATIVEPIXELFORMAT' then
    result := IEBool2StrW(IsNativePixelFormat)
  else
  if ss = 'ENABLEADJUSTORIENTATION' then
    result := IEBool2StrW(EnableAdjustOrientation)

  else
  if ss = 'ABORTING' then
    result := IEBool2StrW(fImageEnIO.Aborting)

  else
  if ss = 'TIFF_COMPRESSIONSTR' then
  begin
    case TIFF_Compression of
      ioTIFF_UNCOMPRESSED: result := 'UNCOMPRESSED';
      ioTIFF_CCITT1D:      result := 'CCITT1D';
      ioTIFF_G3FAX1D:      result := 'G3FAX1D';
      ioTIFF_G3FAX2D:      result := 'G3FAX2D';
      ioTIFF_G4FAX:        result := 'G4FAX';
      ioTIFF_LZW:          result := 'LZW';
      ioTIFF_OLDJPEG:      result := 'OLDJPEG';
      ioTIFF_JPEG:         result := 'JPEG';
      ioTIFF_PACKBITS:     result := 'PACKBITS';
      ioTIFF_ZIP:          result := 'ZIP';
      ioTIFF_DEFLATE:      result := 'DEFLATE';
      else
        result := 'UNKNOWN';
    end;
  end
  else
  if ss = 'TIFF_COMPRESSION' then
    result := IntToStr(integer(TIFF_Compression))
  else
  if ss = 'TIFF_IMAGEINDEX' then
    result := IntToStr(TIFF_ImageIndex)
  else
  if ss = 'TIFF_SUBINDEX' then
    result := IntToStr(TIFF_SubIndex)
  else
  if ss = 'TIFF_IMAGECOUNT' then
    result := IntToStr(TIFF_ImageCount)
  else
  if ss = 'TIFF_JPEGQUALITY' then
    result := IntToStr(TIFF_JpegQuality)
  else
  if ss = 'TIFF_ZIPCOMPRESSION' then
    result := IntToStr(TIFF_ZIPCompression)
  else
  if ss = 'TIFF_STRIPCOUNT' then
    result := IntToStr(TIFF_StripCount)
  else
  if ss = 'TIFF_PHOTOMETINTEPRETSTR' then
  begin
    case TIFF_PhotometInterpret of
      ioTIFF_WHITEISZERO: result := 'WB';
      ioTIFF_BLACKISZERO: result := 'BW';
      ioTIFF_RGB:         result := 'RGB';
      ioTIFF_RGBPALETTE:  result := 'RGBPAL';
      ioTIFF_TRANSPMASK:  result := 'TRANSPMASK';
      ioTIFF_CMYK:        result := 'CMYK';
      ioTIFF_YCBCR:       result := 'YCBCR';
      ioTIFF_CIELAB:      result := 'CIELAB';
    end;
  end
  else
  if ss = 'TIFF_PHOTOMETINTEPRET' then
    result := IntToStr(integer(TIFF_PhotometInterpret))
  else
  if ss = 'TIFF_PLANARCONF' then
    result := IntToStr(TIFF_PlanarConf)
  else
  if ss = 'TIFF_NEWSUBFILETYPE' then
    result := IntToStr(TIFF_NewSubfileType)
  else
  if ss = 'TIFF_XPOS' then
    result := IntToStr(TIFF_XPos)
  else
  if ss = 'TIFF_YPOS' then
    result := IntToStr(TIFF_YPos)
  else
  if ss = 'TIFF_GETTILE' then
    result := IntToStr(TIFF_GetTile)
  else
  if ss = 'TIFF_DOCUMENTNAME' then
    result := WideString(TIFF_DocumentName)
  else
  if ss = 'TIFF_IMAGEDESCRIPTION' then
    result := WideString(TIFF_ImageDescription)
  else
  if ss = 'TIFF_PAGENAME' then
    result := WideString(TIFF_PageName)
  else
  if ss = 'TIFF_PAGENUMBER' then
    result := IntToStr(TIFF_PageNumber)
  else
  if ss = 'TIFF_PAGECOUNT' then
    result := IntToStr(TIFF_PageCount)
  else
  if ss = 'TIFF_JPEGCOLORSPACE' then
    result := IntToStr(integer(TIFF_JPEGColorSpace))
  else
  if ss = 'TIFF_FILLORDER' then
    result := IntToStr(TIFF_FillOrder)
  else
  if ss = 'TIFF_ORIENTATION' then
    result := IntToStr(TIFF_Orientation)
  else
  if ss = 'TIFF_ENABLEADJUSTORIENTATION' then
    result := IEBool2StrW(TIFF_EnableAdjustOrientation)
  else
  if ss = 'TIFF_BYTEORDER' then
    result := IntToStr(integer(TIFF_ByteOrder))

  else
  if ss = 'DCX_IMAGEINDEX' then
    result := IntToStr(DCX_ImageIndex)

  else
  if ss = 'AVI_FRAMECOUNT' then
    result := IntToStr(AVI_FrameCount)
  else
  if ss = 'AVI_FRAMEDELAYTIME' then
    result := IEFloatToStrW(AVI_FrameDelayTime)

  else
  if ss = 'GIF_VERSION' then
    result := WideString(GIF_Version)
  else
  if ss = 'GIF_IMAGEINDEX' then
    result := IntToStr(GIF_ImageIndex)
  else
  if ss = 'GIF_IMAGECOUNT' then
    result := IntToStr(GIF_ImageCount)
  else
  if ss = 'GIF_XPOS' then
    result := IntToStr(GIF_XPos)
  else
  if ss = 'GIF_YPOS' then
    result := IntToStr(GIF_YPos)
  else
  if ss = 'GIF_DELAYTIME' then
    result := IntToStr(GIF_DelayTime)
  else
  if ss = 'GIF_TRANSPARENT' then
    result := IEBool2StrW(GIF_FlagTranspColor)
  else
  if ss = 'GIF_TRANSPCOLOR' then
    result := IERGB2StrW(GIF_TranspColor)
  else
  if ss = 'GIF_INTERLACED' then
    result := IEBool2StrW(GIF_Interlaced)
  else
  if ss = 'GIF_WINWIDTH' then
    result := IntToStr(GIF_WinWidth)
  else
  if ss = 'GIF_WINHEIGHT' then
    result := IntToStr(GIF_WinHeight)
  else
  if ss = 'GIF_BACKGROUND' then
    result := IERGB2StrW(GIF_Background)
  else
  if ss = 'GIF_RATIO' then
    result := IntToStr(GIF_Ratio)

  else
  if ss = 'JPEG_COLORSPACE' then
    result := IntToStr(integer(JPEG_ColorSpace))
  else
  if ss = 'JPEG_QUALITY' then
    result := IntToStr(JPEG_Quality)
  else
  if ss = 'JPEG_DCTMETHOD' then
    result := IntToStr(integer(JPEG_DCTMethod))
  else
  if ss = 'JPEG_OPTIMALHUFFMAN' then
    result := IEBool2StrW(JPEG_OptimalHuffman)
  else
  if ss = 'JPEG_SMOOTH' then
    result := IntToStr(JPEG_Smooth)
  else
  if ss = 'JPEG_PROGRESSIVE' then
    result := IEBool2StrW(JPEG_Progressive)
  else
  if ss = 'JPEG_SCALE_USED' then
    result := IntToStr(JPEG_Scale_Used)
  else
  if ss = 'JPEG_SCALE' then
    result := IntToStr(integer(JPEG_Scale))
  else
  if ss = 'JPEG_ENABLEADJUSTORIENTATION' then
    result := IEBool2StrW(JPEG_EnableAdjustOrientation)
  else
  if ss = 'JPEG_GETEXIFTHUMBNAIL' then
    result := IEBool2StrW(JPEG_GetExifThumbnail)
  else
  if ss = 'JPEG_WARNINGTOT' then
    result := IntToStr(JPEG_WarningTot)
  else
  if ss = 'JPEG_WARNINGCODE' then
    result := IntToStr(JPEG_WarningCode)
  else
  if ss = 'JPEG_ORIGINALWIDTH' then
    result := IntToStr(OriginalWidth)
  else
  if ss = 'JPEG_ORIGINALHEIGHT' then
    result := IntToStr(OriginalHeight)
  else
  if ss = 'ORIGINALWIDTH' then
    result := IntToStr(OriginalWidth)
  else
  if ss = 'ORIGINALHEIGHT' then
    result := IntToStr(OriginalHeight)
  else
  if ss = 'JPEG_CROMASUBSAMPLING' then
    result := IntToStr(integer(JPEG_CromaSubsampling))

  else
  if ss = 'PCX_VERSION' then
    result := IntToStr(PCX_Version)
  else
  if ss = 'PCX_COMPRESSION' then
    result := IntToStr(integer(PCX_Compression))

  else
  if ss = 'BMP_VERSION' then
    result := IntToStr(integer(BMP_Version))
  else
  if ss = 'BMP_COMPRESSION' then
    result := IntToStr(integer(BMP_Compression))
  else
  if ss = 'BMP_HANDLETRANSPARENCY' then
    result := IEBool2StrW(BMP_HandleTransparency)

  else
  if ss = 'BMPRAW_CHANNELORDER' then
    result := IntToStr(integer(BMPRAW_ChannelOrder))
  else
  if ss = 'BMPRAW_PLANES' then
    result := IntToStr(integer(BMPRAW_Planes))
  else
  if ss = 'BMPRAW_ROWALIGN' then
    result := IntToStr(BMPRAW_RowAlign)
  else
  if ss = 'BMPRAW_HEADERSIZE' then
    result := IntToStr(BMPRAW_HeaderSize)
  else
  if ss = 'BMPRAW_DATAFORMAT' then
    result := IntToStr(integer(BMPRAW_DataFormat))

  else
  if ss = 'ICO_IMAGEINDEX' then
    result := IntToStr(ICO_ImageIndex)
  else
  if ss = 'ICO_BACKGROUND' then
    result := IERGB2StrW(ICO_Background)

  else
  if ss = 'CUR_IMAGEINDEX' then
    result := IntToStr(CUR_ImageIndex)
  else
  if ss = 'CUR_XHOTSPOT' then
    result := IntToStr(CUR_XHotSpot)
  else
  if ss = 'CUR_YHOTSPOT' then
    result := IntToStr(CUR_YHotSpot)
  else
  if ss = 'CUR_BACKGROUND' then
    result := IERGB2StrW(CUR_Background)

  else
  if ss = 'PNG_INTERLACED' then
    result := IEBool2StrW(PNG_Interlaced)
  else
  if ss = 'PNG_BACKGROUND' then
    result := IERGB2StrW(PNG_Background)
  else
  if ss = 'PNG_FILTER' then
    result := IntToStr(integer(PNG_Filter))
  else
  if ss = 'PNG_COMPRESSION' then
    result := IntToStr(PNG_Compression)

  else
  if ss = 'PSD_LOADLAYERS' then
    result := IEBool2StrW(PSD_LoadLayers)
  else
  if ss = 'PSD_REPLACELAYERS' then
    result := IEBool2StrW(PSD_ReplaceLayers)
  else
  if ss = 'PSD_HASPREMULTIPLIEDALPHA' then
    result := IEBool2StrW(PSD_HasPremultipliedAlpha)

  else
  if ss = 'HDP_IMAGEQUALITY' then
    result := IEFloatToStrW(HDP_ImageQuality)
  else
  if ss = 'HDP_LOSSLESS' then
    result := IEBool2StrW(HDP_Lossless)

  else
  if ss = 'TGA_XPOS' then
    result := IntToStr(TGA_XPos)
  else
  if ss = 'TGA_YPOS' then
    result := IntToStr(TGA_YPos)
  else
  if ss = 'TGA_COMPRESSED' then
    result := IEBool2StrW(TGA_Compressed)
  else
  if ss = 'TGA_DESCRIPTOR' then
    result := WideString(TGA_Descriptor)
  else
  if ss = 'TGA_AUTHOR' then
    result := WideString(TGA_Author)
  else
  if ss = 'TGA_DATE' then
    result := datetostr(TGA_Date)
  else
  if ss = 'TGA_IMAGENAME' then
    result := WideString(TGA_ImageName)
  else
  if ss = 'TGA_BACKGROUND' then
    result := IERGB2StrW(TGA_Background)
  else
  if ss = 'TGA_ASPECTRATIO' then
    result := IEFloatToStrW(TGA_AspectRatio)
  else
  if ss = 'TGA_GAMMA' then
    result := IEFloatToStrW(TGA_Gamma)
  else
  if ss = 'TGA_GRAYLEVEL' then
    result := IEBool2StrW(TGA_GrayLevel)

  {$ifdef IEINCLUDEJPEG2000}
  else
  if ss = 'J2000_COLORSPACE' then
    result := IntToStr(integer(J2000_ColorSpace))
  else
  if ss = 'J2000_RATE' then
    result := IEFloatToStrW(J2000_Rate)
  else
  if ss = 'J2000_SCALABLEBY' then
    result := IntToStr(integer(J2000_ScalableBy))
  {$endif}

  else
  if ss = 'PS_PAPERWIDTH' then
    result := IntToStr(PS_PaperWidth)
  else
  if ss = 'PS_PAPERHEIGHT' then
    result := IntToStr(PS_PaperHeight)
  else
  if ss = 'PS_COMPRESSION' then
    result := IntToStr(integer(PS_Compression))
  else
  if ss = 'PS_TITLE' then
    result := WideString(PS_Title)
  else
  if ss = 'PDF_PAPERWIDTH' then
    result := IntToStr(PDF_PaperWidth)
  else
  if ss = 'PDF_PAPERHEIGHT' then
    result := IntToStr(PDF_PaperHeight)
  else
  if ss = 'PDF_COMPRESSION' then
    result := IntToStr(integer(PDF_Compression))
  else
  if ss = 'PDF_TITLE' then
    result := WideString(PDF_Title)
  else
  if ss = 'PDF_AUTHOR' then
    result := WideString(PDF_Author)
  else
  if ss = 'PDF_SUBJECT' then
    result := WideString(PDF_Subject)
  else
  if ss = 'PDF_KEYWORDS' then
    result := WideString(PDF_Keywords)
  else
  if ss = 'PDF_CREATOR' then
    result := WideString(PDF_Creator)
  else
  if ss = 'PDF_PRODUCER' then
    result := WideString(PDF_Producer)

  else
  if ss = 'EXIF_HASEXIF' then
    result := IEBool2StrW(EXIF_HasEXIFData)
  else
  if ss = 'EXIF_IMAGEDESCRIPTION' then
    result := WideString(EXIF_ImageDescription)
  else
  if ss = 'EXIF_MAKE' then
    result := WideString(EXIF_Make)
  else
  if ss = 'EXIF_MODEL' then
    result := WideString(EXIF_Model)
  else
  if ss = 'EXIF_ORIENTATION' then
    result := IntToStr(EXIF_Orientation)
  else
  if ss = 'EXIF_XRESOLUTION' then
    result := IEFloatToStrW(EXIF_XResolution)
  else
  if ss = 'EXIF_YRESOLUTION' then
    result := IEFloatToStrW(EXIF_YResolution)
  else
  if ss = 'EXIF_RESOLUTIONUNIT' then
    result := IntToStr(EXIF_ResolutionUnit)
  else
  if ss = 'EXIF_SOFTWARE' then
    result := WideString(EXIF_Software)
  else
  if ss = 'EXIF_DATETIME' then
    result := WideString(EXIF_Datetime)
  else
  if ss = 'EXIF_COPYRIGHT' then
    result := WideString(EXIF_Copyright)
  else
  if ss = 'EXIF_EXPOSURETIME' then
    result := IEFloatToStrW(EXIF_ExposureTime)
  else
  if ss = 'EXIF_FNUMBER' then
    result := IEFloatToStrW(EXIF_FNumber)
  else
  if ss = 'EXIF_EXPOSUREPROGRAM' then
    result := IntToStr(EXIF_ExposureProgram)
  else
  if ss = 'EXIF_EXIFVERSION' then
    result := WideString(EXIF_EXIFVersion)
  else
  if ss = 'EXIF_DATETIMEORIGINAL' then
    result := WideString(EXIF_DateTimeOriginal)
  else
  if ss = 'EXIF_DATETIMEDIGITIZED' then
    result := WideString(EXIF_DateTimeDigitized)
  else
  if ss = 'EXIF_COMPRESSEDBITSPERPIXEL' then
    result := IEFloatToStrW(EXIF_CompressedBitsPerPixel)
  else
  if ss = 'EXIF_SHUTTERSPEEDVALUE' then
    result := IEFloatToStrW(EXIF_ShutterSpeedValue)
  else
  if ss = 'EXIF_APERTUREVALUE' then
    result := IEFloatToStrW(EXIF_ApertureValue)
  else
  if ss = 'EXIF_BRIGHTNESSVALUE' then
    result := IEFloatToStrW(EXIF_BrightNessValue)
  else
  if ss = 'EXIF_EXPOSUREBIASVALUE' then
    result := IEFloatToStrW(EXIF_ExposureBiasValue)
  else
  if ss = 'EXIF_MAXAPERTUREVALUE' then
    result := IEFloatToStrW(EXIF_MaxApertureValue)
  else
  if ss = 'EXIF_SUBJECTDISTANCE' then
    result := IEFloatToStrW(EXIF_SubjectDistance)
  else
  if ss = 'EXIF_METERINGMODE' then
    result := IntToStr(EXIF_MeteringMode)
  else
  if ss = 'EXIF_LIGHTSOURCE' then
    result := IntToStr(EXIF_LightSource)
  else
  if ss = 'EXIF_FLASH' then
    result := IntToStr(EXIF_Flash)
  else
  if ss = 'EXIF_FOCALLENGTH' then
    result := IEFloatToStrW(EXIF_FocalLength)
  else
  if ss = 'EXIF_SUBSECTIME' then
    result := WideString(EXIF_SubsecTime)
  else
  if ss = 'EXIF_SUBSECTIMEORIGINAL' then
    result := WideString(EXIF_SubsecTimeOriginal)
  else
  if ss = 'EXIF_SUBSECTIMEDIGITIZED' then
    result := WideString(EXIF_SubsecTimeDigitized)
  else
  if ss = 'EXIF_FLASHPIXVERSION' then
    result := WideString(EXIF_FlashPixVersion)
  else
  if ss = 'EXIF_COLORSPACE' then
    result := IntToStr(EXIF_ColorSpace)
  else
  if ss = 'EXIF_EXIFIMAGEWIDTH' then
    result := IntToStr(EXIF_EXIFImageWidth)
  else
  if ss = 'EXIF_EXIFIMAGEHEIGHT' then
    result := IntToStr(EXIF_EXIFImageHeight)
  else
  if ss = 'EXIF_RELATEDSOUNDFILE' then
    result := WideString(EXIF_RelatedSoundFile)
  else
  if ss = 'EXIF_FOCALPLANEXRESOLUTION' then
    result := IEFloatToStrW(EXIF_FocalPlaneXResolution)
  else
  if ss = 'EXIF_FOCALPLANEYRESOLUTION' then
    result := IEFloatToStrW(EXIF_FocalPlaneYResolution)
  else
  if ss = 'EXIF_FOCALPLANERESOLUTIONUNIT' then
    result := IntToStr(EXIF_FocalPlaneResolutionUnit)
  else
  if ss = 'EXIF_EXPOSUREINDEX' then
    result := IEFloatToStrW(EXIF_ExposureIndex)
  else
  if ss = 'EXIF_SENSINGMETHOD' then
    result := IntToStr(EXIF_SensingMethod)
  else
  if ss = 'EXIF_FILESOURCE' then
    result := IntToStr(EXIF_FileSource)
  else
  if ss = 'EXIF_SCENETYPE' then
    result := IntToStr(EXIF_SceneType)
  else
  if ss = 'EXIF_USERCOMMENT' then
    result := EXIF_UserComment
  else
  if ss = 'EXIF_USERCOMMENTCODE' then
    result := WideString(EXIF_UserCommentCode)
  else
  if ss = 'EXIF_EXPOSUREMODE' then
    result := IntToStr(fEXIF_ExposureMode)
  else
  if ss = 'EXIF_WHITEBALANCE' then
    result := IntToStr(fEXIF_WhiteBalance)
  else
  if ss = 'EXIF_DIGITALZOOMRATIO' then
    result := IEFloatToStrW(fEXIF_DigitalZoomRatio)
  else
  if ss = 'EXIF_FOCALLENGTHIN35MMFILM' then
    result := IntToStr(fEXIF_FocalLengthIn35mmFilm)
  else
  if ss = 'EXIF_SCENECAPTURETYPE' then
    result := IntToStr(fEXIF_SceneCaptureType)
  else
  if ss = 'EXIF_GAINCONTROL' then
    result := IntToStr(fEXIF_GainControl)
  else
  if ss = 'EXIF_CONTRAST' then
    result := IntToStr(fEXIF_Contrast)
  else
  if ss = 'EXIF_SATURATION' then
    result := IntToStr(fEXIF_Saturation)
  else
  if ss = 'EXIF_SHARPNESS' then
    result := IntToStr(fEXIF_Sharpness)
  else
  if ss = 'EXIF_SUBJECTDISTANCERANGE' then
    result := IntToStr(fEXIF_SubjectDistanceRange)
  else
  if ss = 'EXIF_IMAGEUNIQUEID' then
    result := WideString(fEXIF_ImageUniqueID)
  else
  if ss = 'EXIF_ARTIST' then
    result := WideString(fEXIF_Artist)
  else
  if ss = 'EXIF_ISOSPEEDRATINGS0' then
    result := IntToStr(EXIF_ISOSpeedRatings[0])
  else
  if ss = 'EXIF_ISOSPEEDRATINGS1' then
    result := IntToStr(EXIF_ISOSpeedRatings[1])
  else
  if ss = 'EXIF_XPRATING' then
    result := IntToStr(fEXIF_XPRating)
  else
  if ss = 'EXIF_XPTITLE' then
    result := fEXIF_XPTitle
  else
  if ss = 'EXIF_XPCOMMENT' then
    result := fEXIF_XPComment
  else
  if ss = 'EXIF_XPAUTHOR' then
    result := fEXIF_XPAuthor
  else
  if ss = 'EXIF_XPKEYWORDS' then
    result := fEXIF_XPKeywords
  else
  if ss = 'EXIF_XPSUBJECT' then
    result := fEXIF_XPSubject
  else
  if ss = 'EXIF_WHITEPOINT0' then
    result := IEFloatToStrW(fEXIF_WhitePoint[0])
  else
  if ss = 'EXIF_WHITEPOINT1' then
    result := IEFloatToStrW(fEXIF_WhitePoint[1])
  else
  if ss = 'EXIF_YCBCRCOEFFICIENTS0' then
    result := IEFloatToStrW(fEXIF_YCbCrCoefficients[0])
  else
  if ss = 'EXIF_YCBCRCOEFFICIENTS1' then
    result := IEFloatToStrW(fEXIF_YCbCrCoefficients[1])
  else
  if ss = 'EXIF_YCBCRCOEFFICIENTS2' then
    result := IEFloatToStrW(fEXIF_YCbCrCoefficients[2])
  else
  if ss = 'EXIF_YCBCRPOSITIONING' then
    result := IntToStr(EXIF_YCbCrPositioning)
  else
  if ss = 'EXIF_GPSVERSIONID' then
    result := WideString(fEXIF_GPSVersionID)
  else
  if ss = 'EXIF_GPSLATITUDE' then
    result := IEFloatToStrW(EXIF_GPSLatitude)
  else
  if ss = 'EXIF_GPSLATITUDEREF' then
    result := WideString(fEXIF_GPSLatitudeRef)
  else
  if ss = 'EXIF_GPSLATITUDEDEGREES' then
    result := IEFloatToStrW(fEXIF_GPSLatitudeDegrees)
  else
  if ss = 'EXIF_GPSLATITUDEMINUTES' then
    result := IEFloatToStrW(fEXIF_GPSLatitudeMinutes)
  else
  if ss = 'EXIF_GPSLATITUDESECONDS' then
    result := IEFloatToStrW(fEXIF_GPSLatitudeSeconds)
  else
  if ss = 'EXIF_GPSLONGITUDE' then
    result := IEFloatToStrW(EXIF_GPSLongitude)
  else
  if ss = 'EXIF_GPSLONGITUDEREF' then
    result := WideString(fEXIF_GPSLONGITUDEREF)
  else
  if ss = 'EXIF_GPSLONGITUDEDEGREES' then
    result := IEFloatToStrW(fEXIF_GPSLongitudeDegrees)
  else
  if ss = 'EXIF_GPSLONGITUDEMINUTES' then
    result := IEFloatToStrW(fEXIF_GPSLongitudeMinutes)
  else
  if ss = 'EXIF_GPSLONGITUDESECONDS' then
    result := IEFloatToStrW(fEXIF_GPSLongitudeSeconds)
  else
  if ss = 'EXIF_GPSALTITUDEREF' then
    result := WideString(fEXIF_GPSAltitudeRef)
  else
  if ss = 'EXIF_GPSALTITUDE' then
    result := IEFloatToStrW(fEXIF_GPSAltitude)
  else
  if ss = 'EXIF_GPSTIMESTAMPHOUR' then
    result := IEFloatToStrW(fEXIF_GPSTimeStampHour)
  else
  if ss = 'EXIF_GPSTIMESTAMPMINUTE' then
    result := IEFloatToStrW(fEXIF_GPSTimeStampMinute)
  else
  if ss = 'EXIF_GPSTIMESTAMPSECOND' then
    result := IEFloatToStrW(fEXIF_GPSTimeStampSecond)
  else
  if ss = 'EXIF_GPSSATELLITES' then
    result := WideString(fEXIF_GPSSatellites)
  else
  if ss = 'EXIF_GPSSTATUS' then
    result := WideString(fEXIF_GPSStatus)
  else
  if ss = 'EXIF_GPSMEASUREMODE' then
    result := WideString(fEXIF_GPSMeasureMode)
  else
  if ss = 'EXIF_GPSDOP' then
    result := IEFloatToStrW(fEXIF_GPSDOP)
  else
  if ss = 'EXIF_GPSSPEEDREF' then
    result := WideString(fEXIF_GPSSpeedRef)
  else
  if ss = 'EXIF_GPSSPEED' then
    result := IEFloatToStrW(fEXIF_GPSSpeed)
  else
  if ss = 'EXIF_GPSTRACKREF' then
    result := WideString(fEXIF_GPSTrackRef)
  else
  if ss = 'EXIF_GPSTRACK' then
    result := IEFloatToStrW(fEXIF_GPSTrack)
  else
  if ss = 'EXIF_GPSIMGDIRECTIONREF' then
    result := WideString(fEXIF_GPSImgDirectionRef)
  else
  if ss = 'EXIF_GPSIMGDIRECTION' then
    result := IEFloatToStrW(fEXIF_GPSImgDirection)
  else
  if ss = 'EXIF_GPSMAPDATUM' then
    result := WideString(fEXIF_GPSMapDatum)
  else
  if ss = 'EXIF_GPSDESTLATITUDEREF' then
    result := WideString(fEXIF_GPSDestLatitudeRef)
  else
  if ss = 'EXIF_GPSDESTLATITUDEDEGREES' then
    result := IEFloatToStrW(fEXIF_GPSDestLatitudeDegrees)
  else
  if ss = 'EXIF_GPSDESTLATITUDEMINUTES' then
    result := IEFloatToStrW(fEXIF_GPSDestLatitudeMinutes)
  else
  if ss = 'EXIF_GPSDESTLATITUDESECONDS' then
    result := IEFloatToStrW(fEXIF_GPSDestLatitudeSeconds)
  else
  if ss = 'EXIF_GPSDESTLONGITUDEREF' then
    result := WideString(fEXIF_GPSDestLongitudeRef)
  else
  if ss = 'EXIF_GPSDESTLONGITUDEDEGREES' then
    result := IEFloatToStrW(fEXIF_GPSDestLongitudeDegrees)
  else
  if ss = 'EXIF_GPSDESTLONGITUDEMINUTES' then
    result := IEFloatToStrW(fEXIF_GPSDestLongitudeMinutes)
  else
  if ss = 'EXIF_GPSDESTLONGITUDESECONDS' then
    result := IEFloatToStrW(fEXIF_GPSDestLongitudeSeconds)
  else
  if ss = 'EXIF_GPSDESTBEARINGREF' then
    result := WideString(fEXIF_GPSDestBearingRef)
  else
  if ss = 'EXIF_GPSDESTBEARING' then
    result := IEFloatToStrW(fEXIF_GPSDestBearing)
  else
  if ss = 'EXIF_GPSDESTDISTANCEREF' then
    result := WideString(fEXIF_GPSDestDistanceRef)
  else
  if ss = 'EXIF_GPSDESTDISTANCE' then
    result := IEFloatToStrW(fEXIF_GPSDestDistance)
  else
  if ss = 'EXIF_GPSDATESTAMP' then
    result := WideString(fEXIF_GPSDateStamp)

  {$ifdef IEINCLUDERAWFORMATS}
  else
  if ss = 'RAW_HALFSIZE' then
    result := IEBool2StrW(RAW_HalfSize)
  else
  if ss = 'RAW_GAMMA' then
    result := IEFloatToStrW(RAW_Gamma)
  else
  if ss = 'RAW_BRIGHT' then
    result := IEFloatToStrW(RAW_Bright)
  else
  if ss = 'RAW_REDSCALE' then
    result := IEFloatToStrW(RAW_RedScale)
  else
  if ss = 'RAW_BLUESCALE' then
    result := IEFloatToStrW(RAW_BlueScale)
  else
  if ss = 'RAW_QUICKINTERPOLATE' then
    result := IEBool2StrW(RAW_QuickInterpolate)
  else
  if ss = 'RAW_USEAUTOWB' then
    result := IEBool2StrW(RAW_UseAutoWB)
  else
  if ss = 'RAW_USECAMERAWB' then
    result := IEBool2StrW(RAW_UseCameraWB)
  else
  if ss = 'RAW_FOURCOLORRGB' then
    result := IEBool2StrW(RAW_FourColorRGB)
  else
  if ss = 'RAW_CAMERA' then
    result := WideString(RAW_Camera)
  else
  if ss = 'RAW_GETEXIFTHUMBNAIL' then
    result := IEBool2StrW(RAW_GetExifThumbnail)
  else
  if ss = 'RAW_AUTOADJUSTCOLORS' then
    result := IEBool2StrW(RAW_AutoAdjustColors)
  else
  if ss = 'RAW_EXTRAPARAMS' then
    result := WideString(RAW_ExtraParams)
  {$endif}

  {$IFDEF IEINCLUDEDIRECTSHOW}
  else
  if ss = 'MEDIAFILE_FRAMECOUNT' then
    result := IntToStr(MEDIAFILE_FrameCount)
  else
  if ss = 'MEDIAFILE_FRAMEDELAYTIME' then
    result := IEFloatToStrW(MEDIAFILE_FrameDelayTime)
  {$ENDIF}

  else
  if ss = 'IMAGEINDEX' then
    result := IntToStr(ImageIndex)
  else
  if ss = 'IMAGECOUNT' then
    result := IntToStr(ImageCount)
  else
  if ss = 'GETTHUMBNAIL' then
    result := IEBool2StrW(GetThumbnail)
  else
  if ss = 'ISRESOURCE' then
    result := IEBool2StrW(IsResource)

  else
  if ss = 'XMP_INFO' then
    result := WideString(XMP_Info)

  else
    result := 'INVALID PROPERTY';
end;




procedure TIOParamsVals.SetProperty(Prop, Value: WideString);
var
  ss : WideString;
  q : integer;
begin
  ss := UpperCase(IERemoveCtrlCharsW(Prop));
  Value := IERemoveCtrlCharsW(Value);
  if ss = 'BITSPERSAMPLE' then
    BitsPerSample := StrToIntDef(Value, 8)
  else
  if ss = 'SAMPLESPERPIXEL' then
    SamplesPerPixel := StrToIntDef(Value, 8)
  else
  if ss = 'WIDTH' then
    Width := StrToIntDef(Value, 1)
  else
  if ss = 'HEIGHT' then
    Height := StrToIntDef(Value, 1)
  else
  if ss = 'DPIX' then
    DpiX := StrToIntDef(Value, 1)
  else
  if ss = 'DPIY' then
    DpiY := StrToIntDef(Value, 1)
  else
  if Copy(ss, 1, 12)='COLORMAPITEM' then
  begin
    q := StrToIntDef(Copy(ss, 13, length(ss)), 0);
    ColorMap^[q] := IEStr2RGBW(Value);
  end
  // else
  // if ss = 'COLORMAPCOUNT' then
  // readonly
  else
  if ss = 'ISNATIVEPIXELFORMAT' then
    IsNativePixelFormat := IEStr2BoolW(value)
  else
  if ss = 'ENABLEADJUSTORIENTATION' then
    EnableAdjustOrientation := IEStr2BoolW(value)

  else
  if ss = 'ABORTING' then
    fImageEnIO.Aborting := IEStr2BoolW(value)

  else
  if ss = 'TIFF_COMPRESSION' then
    TIFF_Compression := TIOTIFFCompression(StrToIntDef(value, 0))
  else
  if ss = 'TIFF_IMAGEINDEX' then
    TIFF_ImageIndex := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_SUBINDEX' then
    TIFF_SubIndex := StrToIntDef(value, 0)
  //else
  // if ss = 'TIFF_IMAGECOUNT' then
  // readonly
  else
  if ss = 'TIFF_PHOTOMETINTEPRET' then
    TIFF_PhotometInterpret := TIOTIFFPhotometInterpret(StrToIntDef(value, 0))
  else
  if ss = 'TIFF_PLANARCONF' then
    TIFF_PlanarConf := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_NEWSUBFILETYPE' then
    TIFF_NewSubfileType := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_XPOS' then
    TIFF_XPos := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_YPOS' then
    TIFF_YPos := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_GETTILE' then
    TIFF_GetTile := StrToIntDef(value, -1)
  else
  if ss = 'TIFF_JPEGQUALITY' then
    TIFF_JPEGQuality := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_ZIPCOMPRESSION' then
    TIFF_ZIPCompression := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_STRIPCOUNT' then
    TIFF_StripCount := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_DOCUMENTNAME' then
    TIFF_DocumentName := AnsiString(value)
  else
  if ss = 'TIFF_IMAGEDESCRIPTION' then
    TIFF_ImageDescription := AnsiString(value)
  else
  if ss = 'TIFF_PAGENAME' then
    TIFF_PageName := AnsiString(value)
  else
  if ss = 'TIFF_PAGENUMBER' then
    TIFF_PageNumber := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_PAGECOUNT' then
    TIFF_PageCount := StrToIntDef(value, 0)
  else
  if ss = 'TIFF_JPEGCOLORSPACE' then
    TIFF_JPEGColorSpace := TIOJPEGColorSpace(StrToIntDef(value, 0))
  else
  if ss = 'TIFF_FILLORDER' then
    TIFF_FillOrder := StrToIntDef(value, 1)
  else
  if ss = 'TIFF_ORIENTATION' then
    TIFF_Orientation := StrToIntDef(value, 1)
  else
  if ss = 'TIFF_ENABLEADJUSTORIENTATION' then
    TIFF_EnableAdjustOrientation := IEStr2BoolW(value)
  else
  if ss = 'TIFF_BYTEORDER' then
    TIFF_ByteOrder := TIOByteOrder(StrToIntDef(value, 1))

  else
  if ss = 'DCX_IMAGEINDEX' then
    DCX_ImageIndex := StrToIntDef(value, 0)

  else
  if ss = 'AVI_FRAMECOUNT' then
    AVI_FrameCount := StrToIntDef(value, 0)
  else
  if ss = 'AVI_FRAMEDELAYTIME' then
    AVI_FrameDelayTime := IEStrToFloatDefW(value, 0)

  else
  if ss = 'GIF_VERSION' then
    GIF_Version := AnsiString(value)
  else
  if ss = 'GIF_IMAGEINDEX' then
    GIF_ImageIndex := StrToIntDef(value, 0)
  // else
  // if ss = 'GIF_IMAGECOUNT' then
  // read-only
  else
  if ss = 'GIF_XPOS' then
    GIF_XPos := StrToIntDef(value, 0)
  else
  if ss = 'GIF_YPOS' then
    GIF_YPos := StrToIntDef(value, 0)
  else
  if ss = 'GIF_DELAYTIME' then
      GIF_DelayTime := StrToIntDef(value, 0)
  else
  if ss = 'GIF_TRANSPARENT' then
     GIF_FlagTranspColor := IEStr2BoolW(value)
  else
  if ss = 'GIF_TRANSPCOLOR' then
    GIF_TranspColor := IEStr2RGBW(value)
  else
  if ss = 'GIF_INTERLACED' then
    GIF_Interlaced := IEStr2BoolW(value)
  else
  if ss = 'GIF_WINWIDTH' then
    GIF_WinWidth := StrToIntDef(value, 0)
  else
  if ss = 'GIF_WINHEIGHT' then
    GIF_WinHeight := StrToIntDef(value, 0)
  else
  if ss = 'GIF_BACKGROUND' then
    GIF_Background := IEStr2RGBW(value)
  else
  if ss = 'GIF_RATIO' then
    GIF_Ratio := StrToIntDef(value, 0)

  else
  if ss = 'JPEG_COLORSPACE' then
    JPEG_ColorSpace := TIOJPEGColorSpace(StrToIntDef(value, 0))
  else
  if ss = 'JPEG_QUALITY' then
    JPEG_Quality := StrToIntDef(value, 0)
  else
  if ss = 'JPEG_DCTMETHOD' then
    JPEG_DCTMethod := TIOJPEGDCTMethod(StrToIntDef(value, 0))
  else
  if ss = 'JPEG_OPTIMALHUFFMAN' then
    JPEG_OptimalHuffman := IEStr2BoolW(value)
  else
  if ss = 'JPEG_SMOOTH' then
    JPEG_Smooth := StrToIntDef(value, 0)
  else
  if ss = 'JPEG_PROGRESSIVE' then
    JPEG_Progressive := IEStr2BoolW(value)
  else
  if ss = 'JPEG_SCALE_USED' then
    JPEG_Scale_Used := StrToIntDef(value, 0)
  else
  if ss = 'JPEG_SCALE' then
    JPEG_Scale := TIOJPEGScale(StrToIntDef(value, 0))
  else
  if ss = 'JPEG_ENABLEADJUSTORIENTATION' then
    JPEG_EnableAdjustOrientation := IEStr2BoolW(value)
  else
  if ss = 'JPEG_GETEXIFTHUMBNAIL' then
    JPEG_GetExifThumbnail := IEStr2BoolW(value)
  else
  if ss = 'JPEG_WARNINGTOT' then
    JPEG_WarningTot := StrToIntDef(value, 0)
  else
  if ss = 'JPEG_WARNINGCODE' then
    JPEG_WarningCode := StrToIntDef(value, 0)
  else
  if ss = 'JPEG_ORIGINALWIDTH' then
    OriginalWidth := StrToIntDef(value, 0)
  else
  if ss = 'JPEG_ORIGINALHEIGHT' then
    OriginalHeight := StrToIntDef(value, 0)
  else
  if ss = 'ORIGINALWIDTH' then
    OriginalWidth := StrToIntDef(value, 0)
  else
  if ss = 'ORIGINALHEIGHT' then
    OriginalHeight := StrToIntDef(value, 0)
  else
  if ss = 'JPEG_CROMASUBSAMPLING' then
    JPEG_CromaSubsampling := TIOJPEGCROMASUBSAMPLING(StrToIntDef(value, 1))

  else
  if ss = 'PCX_VERSION' then
    PCX_Version := StrToIntDef(value, 0)
  else
  if ss = 'PCX_COMPRESSION' then
    PCX_Compression := TIOPCXCompression(StrToIntDef(value, 0))

  else
  if ss = 'BMP_VERSION' then
    BMP_Version := TIOBMPVersion(StrToIntDef(value, 0))
  else
  if ss = 'BMP_COMPRESSION' then
    BMP_Compression := TIOBMPCompression(StrToIntDef(value, 0))
  else
  if ss = 'BMP_HANDLETRANSPARENCY' then
    BMP_HandleTransparency := IEStr2BoolW(value)

  else
  if ss = 'BMPRAW_CHANNELORDER' then
    BMPRAW_ChannelOrder := TIOBMPRAWChannelOrder(StrToIntDef(value, 0))
  else
  if ss = 'BMPRAW_PLANES' then
    BMPRAW_Planes := TIOBMPRAWPlanes(StrToIntDef(value, 0))
  else
  if ss = 'BMPRAW_ROWALIGN' then
    BMPRAW_RowAlign := StrToIntDef(value, 8)
  else
  if ss = 'BMPRAW_HEADERSIZE' then
    BMPRAW_HeaderSize := StrToIntDef(value, 0)
  else
  if ss = 'BMPRAW_DATAFORMAT' then
    BMPRAW_DataFormat := TIOBMPRAWDataFormat(StrToIntDef(value, 0))

  else
  if ss = 'ICO_IMAGEINDEX' then
    ICO_ImageIndex := StrToIntDef(value, 0)
  else
  if ss = 'ICO_BACKGROUND' then
    ICO_Background := IEStr2RGBW(value)

  else
  if ss = 'CUR_IMAGEINDEX' then
    CUR_ImageIndex := StrToIntDef(value, 0)
  else
  if ss = 'CUR_XHOTSPOT' then
    CUR_XHotSpot := StrToIntDef(value, 0)
  else
  if ss = 'CUR_YHOTSPOT' then
    CUR_YHotSpot := StrToIntDef(value, 0)
  else
  if ss = 'CUR_BACKGROUND' then
    CUR_Background := IEStr2RGBW(value)

  else
  if ss = 'PNG_INTERLACED' then
    PNG_Interlaced := IEStr2BoolW(value)
  else
  if ss = 'PNG_BACKGROUND' then
    PNG_Background := IEStr2RGBW(value)
  else
  if ss = 'PNG_FILTER' then
    PNG_Filter := TIOPNGFilter(StrToIntDef(value, 0))
  else
  if ss = 'PNG_COMPRESSION' then
    PNG_Compression := StrToIntDef(value, 1)

  else
  if ss = 'PSD_LOADLAYERS' then
    PSD_LoadLayers := IEStr2BoolW(value)
  else
  if ss = 'PSD_REPLACELAYERS' then
    PSD_ReplaceLayers := IEStr2BoolW(value)
  // else
  // if ss = 'PSD_HASPREMULTIPLIEDALPHA' then
  // readonly, PSD_HasPremultipliedAlpha := IEStr2BoolW(value)
  else
  if ss = 'HDP_IMAGEQUALITY' then
    HDP_ImageQuality := IEStrToFloatDefW(value, 0.9)
  else
  if ss = 'HDP_LOSSLESS' then
    HDP_Lossless := IEStr2BoolW(value)

  else
  if ss = 'TGA_XPOS' then
    TGA_XPos := StrToIntDef(value, 0)
  else
  if ss = 'TGA_YPOS' then
    TGA_YPos := StrToIntDef(value, 0)
  else
  if ss = 'TGA_COMPRESSED' then
    TGA_Compressed := IEStr2BoolW(value)
  else
  if ss = 'TGA_DESCRIPTOR' then
    TGA_Descriptor := AnsiString(value)
  else
  if ss = 'TGA_AUTHOR' then
    TGA_Author := AnsiString(value)
  else
  if ss = 'TGA_DATE' then
  try
    TGA_Date := strtodate(value)
  except
  end
  else
  if ss = 'TGA_IMAGENAME' then
    TGA_ImageName := AnsiString(value)
  else
  if ss = 'TGA_BACKGROUND' then
    TGA_Background := IEStr2RGBW(value)
  else
  if ss = 'TGA_ASPECTRATIO' then
  try
    TGA_AspectRatio := IEStrToFloatDefW(value, 0)
  except
  end
  else
  if ss = 'TGA_GAMMA' then
  try
    TGA_Gamma := IEStrToFloatDefW(value, 0)
  except
  end
  else
  if ss = 'TGA_GRAYLEVEL' then
    TGA_GrayLevel := IEStr2BoolW(value)

    {$ifdef IEINCLUDEJPEG2000}
  else
  if ss = 'J2000_COLORSPACE' then
    J2000_ColorSpace := TIOJ2000ColorSpace(StrToIntDef(value, 0))
  else
  if ss = 'J2000_RATE' then
  try
    J2000_Rate := IEStrToFloatDefW(value, 0)
  except
  end
  else
  if ss = 'J2000_SCALABLEBY' then
    J2000_ScalableBy := TIOJ2000ScalableBy(StrToIntDef(value, 0))
    {$endif}

  else
  if ss = 'PS_PAPERWIDTH' then
    PS_PaperWidth := StrToIntDef(value, 595)
  else
  if ss = 'PS_PAPERHEIGHT' then
    PS_PaperHeight := StrToIntDef(value, 842)
  else
  if ss = 'PS_COMPRESSION' then
    PS_Compression := TIOPSCompression(StrToIntDef(value, 0))
  else
  if ss = 'PS_TITLE' then
    PS_Title := AnsiString(value)
  else
  if ss = 'PDF_PAPERWIDTH' then
    PDF_PaperWidth := StrToIntDef(value, 595)
  else
  if ss = 'PDF_PAPERHEIGHT' then
    PDF_PaperHeight := StrToIntDef(value, 842)
  else
  if ss = 'PDF_COMPRESSION' then
    PDF_Compression := TIOPDFCompression(StrToIntDef(value, 0))
  else
  if ss = 'PDF_TITLE' then
    PDF_Title := AnsiString(value)
  else
  if ss = 'PDF_AUTHOR' then
    PDF_Author := AnsiString(value)
  else
  if ss = 'PDF_SUBJECT' then
    PDF_Subject := AnsiString(value)
  else
  if ss = 'PDF_KEYWORDS' then
    PDF_Keywords := AnsiString(value)
  else
  if ss = 'PDF_CREATOR' then
    PDF_Creator := AnsiString(value)
  else
  if ss = 'PDF_PRODUCER' then
    PDF_Producer := AnsiString(value)

  else
  if ss = 'EXIF_HASEXIF' then
    EXIF_HasEXIFData := IEStr2BoolW(value)
  else
  if ss = 'EXIF_IMAGEDESCRIPTION' then
    EXIF_ImageDescription := AnsiString(value)
  else
  if ss = 'EXIF_MAKE' then
    EXIF_Make := AnsiString(value)
  else
  if ss = 'EXIF_MODEL' then
    EXIF_Model := AnsiString(value)
  else
  if ss = 'EXIF_ORIENTATION' then
    EXIF_Orientation := StrToIntDef(value, 0)
  else
  if ss = 'EXIF_XRESOLUTION' then
  try
    EXIF_XResolution := IEStrToFloatDefW(value, 0);
  except
  end
  else
  if ss = 'EXIF_YRESOLUTION' then
  try
    EXIF_YResolution := IEStrToFloatDefW(value, 0);
  except
  end
  else
  if ss = 'EXIF_RESOLUTIONUNIT' then
    EXIF_ResolutionUnit := StrToIntDef(value, 0)
  else
  if ss = 'EXIF_SOFTWARE' then
    EXIF_Software := AnsiString(value)
  else
  if ss = 'EXIF_DATETIME' then
    EXIF_Datetime := AnsiString(value)
  else
  if ss = 'EXIF_COPYRIGHT' then
    EXIF_Copyright := AnsiString(value)
  else
  if ss = 'EXIF_EXPOSURETIME' then
  try
    EXIF_ExposureTime := IEStrToFloatDefW(value, 0)
  except
  end
  else
  if ss = 'EXIF_FNUMBER' then
  try
    EXIF_FNumber := IEStrToFloatDefW(value, -1)
  except
  end
  else
  if ss = 'EXIF_EXPOSUREPROGRAM' then
    EXIF_ExposureProgram := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_EXIFVERSION' then
    EXIF_EXIFVersion := AnsiString(value)
  else
  if ss = 'EXIF_DATETIMEORIGINAL' then
    EXIF_DateTimeOriginal := AnsiString(value)
  else
  if ss = 'EXIF_DATETIMEDIGITIZED' then
    EXIF_DateTimeDigitized := AnsiString(value)
  else
  if ss = 'EXIF_COMPRESSEDBITSPERPIXEL' then
  try
    EXIF_CompressedBitsPerPixel := IEStrToFloatDefW(value, 0)
  except
  end
  else
  if ss = 'EXIF_SHUTTERSPEEDVALUE' then
  try
    EXIF_ShutterSpeedValue := IEStrToFloatDefW(value, -1);
  except
  end
  else
  if ss = 'EXIF_APERTUREVALUE' then
  try
    EXIF_ApertureValue := IEStrToFloatDefW(value, -1);
  except
  end
  else
  if ss = 'EXIF_BRIGHTNESSVALUE' then
  try
    EXIF_BrightNessValue := IEStrToFloatDefW(value, -1000);
  except
  end
  else
  if ss = 'EXIF_EXPOSUREBIASVALUE' then
  try
    EXIF_ExposureBiasValue := IEStrToFloatDefW(value, -1000);
  except
  end
  else
  if ss = 'EXIF_MAXAPERTUREVALUE' then
  try
    EXIF_MaxApertureValue := IEStrToFloatDefW(value, -1000);
  except
  end
  else
  if ss = 'EXIF_SUBJECTDISTANCE' then
  try
    EXIF_SubjectDistance := IEStrToFloatDefW(value, -1);
  except
  end
  else
  if ss = 'EXIF_METERINGMODE' then
    EXIF_MeteringMode := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_LIGHTSOURCE' then
    EXIF_LightSource := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_FLASH' then
    EXIF_Flash := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_FOCALLENGTH' then
  try
    EXIF_FocalLength := IEStrToFloatDefW(value, -1)
  except
  end
  else
  if ss = 'EXIF_SUBSECTIME' then
    EXIF_SubsecTime := AnsiString(value)
  else
  if ss = 'EXIF_SUBSECTIMEORIGINAL' then
    EXIF_SubsecTimeOriginal := AnsiString(value)
  else
  if ss = 'EXIF_SUBSECTIMEDIGITIZED' then
    EXIF_SubsecTimeDigitized := AnsiString(value)
  else
  if ss = 'EXIF_FLASHPIXVERSION' then
    EXIF_FlashPixVersion := AnsiString(value)
  else
  if ss = 'EXIF_COLORSPACE' then
    EXIF_ColorSpace := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_EXIFIMAGEWIDTH' then
    EXIF_EXIFImageWidth := StrToIntDef(value, 0)
  else
  if ss = 'EXIF_EXIFIMAGEHEIGHT' then
    EXIF_EXIFImageHeight := StrToIntDef(value, 0)
  else
  if ss = 'EXIF_RELATEDSOUNDFILE' then
    EXIF_RelatedSoundFile := AnsiString(value)
  else
  if ss = 'EXIF_FOCALPLANEXRESOLUTION' then
  try
    EXIF_FocalPlaneXResolution := IEStrToFloatDefW(value, -1)
  except
  end
  else
  if ss = 'EXIF_FOCALPLANEYRESOLUTION' then
  try
    EXIF_FocalPlaneYResolution := IEStrToFloatDefW(value, -1)
  except
  end
  else
  if ss = 'EXIF_FOCALPLANERESOLUTIONUNIT' then
    EXIF_FocalPlaneResolutionUnit := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_EXPOSUREINDEX' then
  try
    EXIF_ExposureIndex := IEStrToFloatDefW(value, -1)
  except
  end
  else
  if ss = 'EXIF_SENSINGMETHOD' then
    EXIF_SensingMethod := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_FILESOURCE' then
    EXIF_FileSource := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_SCENETYPE' then
    EXIF_SceneType := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_USERCOMMENT' then
    EXIF_UserComment := value
  else
  if ss = 'EXIF_USERCOMMENTCODE' then
    EXIF_UserCommentCode := AnsiString(value)

  else
  if ss = 'EXIF_EXPOSUREMODE' then
    fEXIF_ExposureMode := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_WHITEBALANCE' then
    fEXIF_WhiteBalance := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_DIGITALZOOMRATIO' then
    fEXIF_DigitalZoomRatio := IEStrToFloatDefW(value, -1)
  else
  if ss = 'EXIF_FOCALLENGTHIN35MMFILM' then
    fEXIF_FocalLengthIn35mmFilm := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_SCENECAPTURETYPE' then
    fEXIF_SceneCaptureType := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_GAINCONTROL' then
    fEXIF_GainControl := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_CONTRAST' then
    fEXIF_Contrast := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_SATURATION' then
    fEXIF_Saturation := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_SHARPNESS' then
    fEXIF_Sharpness := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_SUBJECTDISTANCERANGE' then
    fEXIF_SubjectDistanceRange := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_IMAGEUNIQUEID' then
    fEXIF_ImageUniqueID := AnsiString(value)

  else
  if ss = 'EXIF_GPSVERSIONID' then
    fEXIF_GPSVersionID := AnsiString(value)
  else
  if ss = 'EXIF_GPSLATITUDE' then
    EXIF_GPSLatitude := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSLATITUDEREF' then
    fEXIF_GPSLatitudeRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSLATITUDEDEGREES' then
    fEXIF_GPSLatitudeDegrees := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSLATITUDEMINUTES' then
    fEXIF_GPSLatitudeMinutes := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSLATITUDESECONDS' then
    fEXIF_GPSLatitudeSeconds := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSLONGITUDE' then
    EXIF_GPSLongitude := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSLONGITUDEREF' then
    fEXIF_GPSLONGITUDEREF := AnsiString(value)
  else
  if ss = 'EXIF_GPSLONGITUDEDEGREES' then
    fEXIF_GPSLongitudeDegrees := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSLONGITUDEMINUTES' then
    fEXIF_GPSLongitudeMinutes := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSLONGITUDESECONDS' then
    fEXIF_GPSLongitudeSeconds := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSALTITUDEREF' then
    fEXIF_GPSAltitudeRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSALTITUDE' then
    fEXIF_GPSAltitude := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSTIMESTAMPHOUR' then
    fEXIF_GPSTimeStampHour := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSTIMESTAMPMINUTE' then
    fEXIF_GPSTimeStampMinute := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSTIMESTAMPSECOND' then
    fEXIF_GPSTimeStampSecond := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSSATELLITES' then
    fEXIF_GPSSatellites := AnsiString(value)
  else
  if ss = 'EXIF_GPSSTATUS' then
    fEXIF_GPSStatus := AnsiString(value)
  else
  if ss = 'EXIF_GPSMEASUREMODE' then
    fEXIF_GPSMeasureMode := AnsiString(value)
  else
  if ss = 'EXIF_GPSDOP' then
    fEXIF_GPSDOP := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSSPEEDREF' then
    fEXIF_GPSSpeedRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSSPEED' then
    fEXIF_GPSSpeed := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSTRACKREF' then
    fEXIF_GPSTrackRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSTRACK' then
    fEXIF_GPSTrack := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSIMGDIRECTIONREF' then
    fEXIF_GPSImgDirectionRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSIMGDIRECTION' then
    fEXIF_GPSImgDirection := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSMAPDATUM' then
    fEXIF_GPSMapDatum := AnsiString(value)
  else
  if ss = 'EXIF_GPSDESTLATITUDEREF' then
    fEXIF_GPSDestLatitudeRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSDESTLATITUDEDEGREES' then
    fEXIF_GPSDestLatitudeDegrees := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSDESTLATITUDEMINUTES' then
    fEXIF_GPSDestLatitudeMinutes := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSDESTLATITUDESECONDS' then
    fEXIF_GPSDestLatitudeSeconds := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSDESTLONGITUDEREF' then
    fEXIF_GPSDestLongitudeRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSDESTLONGITUDEDEGREES' then
    fEXIF_GPSDestLongitudeDegrees := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSDESTLONGITUDEMINUTES' then
    fEXIF_GPSDestLongitudeMinutes := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSDESTLONGITUDESECONDS' then
    fEXIF_GPSDestLongitudeSeconds := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSDESTBEARINGREF' then
    fEXIF_GPSDestBearingRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSDESTBEARING' then
    fEXIF_GPSDestBearing := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSDESTDISTANCEREF' then
    fEXIF_GPSDestDistanceRef := AnsiString(value)
  else
  if ss = 'EXIF_GPSDESTDISTANCE' then
    fEXIF_GPSDestDistance := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_GPSDATESTAMP' then
    fEXIF_GPSDateStamp := AnsiString(value)
  else
  if ss = 'EXIF_ISOSPEEDRATINGS0' then
    fEXIF_ISOSpeedRatings[0] := StrToIntDef(value, 0)
  else
  if ss = 'EXIF_ISOSPEEDRATINGS1' then
    fEXIF_ISOSpeedRatings[1] := StrToIntDef(value, 0)
  else
  if ss = 'EXIF_ARTIST' then
    fEXIF_Artist := AnsiString(value)
  else
  if ss = 'EXIF_XPRATING' then
    fEXIF_XPRating := StrToIntDef(value, -1)
  else
  if ss = 'EXIF_XPTITLE' then
    fEXIF_XPTitle := value
  else
  if ss = 'EXIF_XPCOMMENT' then
    fEXIF_XPComment := value
  else
  if ss = 'EXIF_XPAUTHOR' then
    fEXIF_XPAuthor := value
  else
  if ss = 'EXIF_XPKEYWORDS' then
    fEXIF_XPKeywords := value
  else
  if ss = 'EXIF_XPSUBJECT' then
    fEXIF_XPSubject := value
  else
  if ss = 'EXIF_WHITEPOINT0' then
    fEXIF_WhitePoint[0] := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_WHITEPOINT1' then
    fEXIF_WhitePoint[1] := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_YCBCRCOEFFICIENTS0' then
    fEXIF_YCbCrCoefficients[0] := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_YCBCRCOEFFICIENTS1' then
    fEXIF_YCbCrCoefficients[1] := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_YCBCRCOEFFICIENTS2' then
    fEXIF_YCbCrCoefficients[2] := IEStrToFloatDefW(value, 0)
  else
  if ss = 'EXIF_YCBCRPOSITIONING' then
    EXIF_YCbCrPositioning := StrToIntDef(value, 0)


   {$ifdef IEINCLUDERAWFORMATS}
  else
  if ss = 'RAW_HALFSIZE' then
    RAW_HalfSize := IEStr2BoolW(value)
  else
  if ss = 'RAW_GAMMA' then
    RAW_Gamma := IEStrToFloatDefW(value, 0)
  else
  if ss = 'RAW_BRIGHT' then
    RAW_Bright := IEStrToFloatDefW(value, 0)
  else
  if ss = 'RAW_REDSCALE' then
    RAW_RedScale := IEStrToFloatDefW(value, 0)
  else
  if ss = 'RAW_BLUESCALE' then
    RAW_BlueScale := IEStrToFloatDefW(value, 0)
  else
  if ss = 'RAW_QUICKINTERPOLATE' then
    RAW_QuickInterpolate := IEStr2BoolW(value)
  else
  if ss = 'RAW_USEAUTOWB' then
    RAW_UseAutoWB := IEStr2BoolW(value)
  else
  if ss = 'RAW_USECAMERAWB' then
    RAW_UseCameraWB := IEStr2BoolW(value)
  else
  if ss = 'RAW_FOURCOLORRGB' then
    RAW_FourColorRGB := IEStr2BoolW(value)
  else
  if ss = 'RAW_CAMERA' then
    RAW_Camera := AnsiString(value)
  else
  if ss = 'RAW_GETEXIFTHUMBNAIL' then
    RAW_GetExifThumbnail := IEStr2BoolW(value)
  else
  if ss = 'RAW_AUTOADJUSTCOLORS' then
    RAW_AutoAdjustColors := IEStr2BoolW(value)
  else
  if ss = 'RAW_EXTRAPARAMS' then
    RAW_ExtraParams := AnsiString(value)
   {$endif}

  {$IFDEF IEINCLUDEDIRECTSHOW}
  else
  if ss = 'MEDIAFILE_FRAMECOUNT' then
    MEDIAFILE_FrameCount := StrToIntDef(value, 0)
  else
  if ss = 'MEDIAFILE_FRAMEDELAYTIME' then
    MEDIAFILE_FrameDelayTime := IEStrToFloatDefW(value, 0)
  {$ENDIF}

  else
  if ss = 'IMAGEINDEX' then
    ImageIndex := StrToIntDef(value, 0)
  else
  if ss = 'IMAGECOUNT' then
    ImageCount := StrToIntDef(value, 0)
  else
  if ss = 'GETTHUMBNAIL' then
    GetThumbnail := IEStr2BoolW(value)
  else
  if ss = 'ISRESOURCE' then
    IsResource := IEStr2BoolW(value)

  else
  if ss = 'XMP_INFO' then
    XMP_Info := AnsiString(value);
end;


{!!
<FS>TIOParamsVals.UpdateEXIFThumbnail

<FM>Declaration<FC>
procedure UpdateEXIFThumbnail(Width: integer = 160; Height: integer = -1; ResampleFilter: <A TResampleFilter> = rfTriangle);

<FM>Description<FN>
Updates the <A TIOParamsVals.EXIF_Bitmap> property with the content of current image.

You should call this method just before saving a JPEG so the thumbnail is consistent with the saved image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Width<FN></C> <C>Width of the thumbnail.</C> </R>
<R> <C><FC>Height<FN></C> <C>Height of the thumbnail.</C> </R>
<R> <C><FC>ResampleFilter<FN></C> <C>Interpolation filter to use when resampling.</C> </R>
</TABLE>

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
ImageEnView1.Proc.Negative;
ImageEnView1.IO.Params.UpdateEXIFThumbnail;
ImageEnView1.IO.SaveToFile('D:\output.jpg');
!!}
procedure TIOParamsVals.UpdateEXIFThumbnail(Width: integer = 160; Height: integer = -1; ResampleFilter: TResampleFilter = rfTriangle);
var
  proc: TImageEnProc;
begin
  if fEXIF_Bitmap = nil then
    fEXIF_Bitmap := TIEBitmap.Create;
  proc := TImageEnProc.CreateFromBitmap(fImageEnIO.IEBitmap);
  proc.AutoUndo := false;
  try
    proc.ResampleTo(fEXIF_Bitmap, Width, Height, ResampleFilter);
  finally
    proc.free;
  end;
end;

procedure TIOParamsVals.EXIFTagsAdd(tag: Integer);
begin
  if fEXIF_Tags.IndexOf(pointer(tag))<0 then
    fEXIF_Tags.Add(pointer(tag));
end;

procedure TIOParamsVals.EXIFTagsDel(tag: Integer);
var
  p: Integer;
begin
  p := fEXIF_Tags.IndexOf(pointer(tag));
  if p > -1 then
    fEXIF_Tags.Delete(p);
end;

procedure TIOParamsVals.SetEXIF_ExposureTime(value: Double);
begin
  fEXIF_ExposureTime := value;
  if value <> -1 then
    EXIFTagsAdd($829A)
  else
    EXIFTagsDel($829A);
end;

procedure TIOParamsVals.SetEXIF_FNumber(value: Double);
begin
  fEXIF_FNumber := value;
  if value <> -1 then
    EXIFTagsAdd($829D)
  else
    EXIFTagsDel($829D);
end;

procedure TIOParamsVals.SetEXIF_ExposureProgram(value: Integer);
begin
  fEXIF_ExposureProgram := value;
  if value <> -1 then
    EXIFTagsAdd($8822)
  else
    EXIFTagsDel($8822);
end;

procedure TIOParamsVals.SetEXIF_CompressedBitsPerPixel(value: Double);
begin
  fEXIF_CompressedBitsPerPixel := value;
  if value <> 0 then
    EXIFTagsAdd($9102)
  else
    EXIFTagsDel($9102);
end;

procedure TIOParamsVals.SetEXIF_DateTime2(const Value: TDateTime);
begin
  EXIF_DateTime := AnsiString(DateTimeToEXIFDate(Value));
end;

procedure TIOParamsVals.SetEXIF_DateTimeDigitized2(const Value: TDateTime);
begin
  EXIF_DateTimeDigitized := AnsiString(DateTimeToEXIFDate(Value));
end;

procedure TIOParamsVals.SetEXIF_DateTimeOriginal2(const Value: TDateTime);
begin
  EXIF_DateTimeOriginal := AnsiString(DateTimeToEXIFDate(Value));
end;

procedure TIOParamsVals.SetEXIF_ShutterSpeedValue(value: Double);
begin
  fEXIF_ShutterSpeedValue := value;
  if value <> -1 then
    EXIFTagsAdd($9201)
  else
    EXIFTagsDel($9201);
end;

procedure TIOParamsVals.SetEXIF_ApertureValue(value: Double);
begin
  fEXIF_ApertureValue := value;
  if value <> -1 then
    EXIFTagsAdd($9202)
  else
    EXIFTagsDel($9202);
end;

procedure TIOParamsVals.SetEXIF_BrightnessValue(value: Double);
begin
  fEXIF_BrightnessValue := value;
  if value <> -1000 then
    EXIFTagsAdd($9203)
  else
    EXIFTagsDel($9203);
end;

procedure TIOParamsVals.SetEXIF_ExposureBiasValue(value: Double);
begin
  fEXIF_ExposureBiasValue := value;
  if value <> -1000 then
    EXIFTagsAdd($9204)
  else
    EXIFTagsDel($9204);
end;

procedure TIOParamsVals.SetEXIF_MaxApertureValue(value: Double);
begin
  fEXIF_MaxApertureValue := value;
  if value <> -1000 then
    EXIFTagsAdd($9205)
  else
    EXIFTagsDel($9205);
end;

procedure TIOParamsVals.SetEXIF_SubjectDistance(value: Double);
begin
  fEXIF_SubjectDistance := value;
  if value <> -1 then
    EXIFTagsAdd($9206)
  else
    EXIFTagsDel($9206);
end;

procedure TIOParamsVals.SetEXIF_MeteringMode(value: Integer);
begin
  fEXIF_MeteringMode := value;
  if value <> -1 then
    EXIFTagsAdd($9207)
  else
    EXIFTagsDel($9207);
end;

procedure TIOParamsVals.SetEXIF_LightSource(value: Integer);
begin
  fEXIF_LightSource := value;
  if value <> -1 then
    EXIFTagsAdd($9208)
  else
    EXIFTagsDel($9208);
end;

procedure TIOParamsVals.SetEXIF_Flash(value: Integer);
begin
  fEXIF_Flash := value;
  if value <> -1 then
    EXIFTagsAdd($9209)
  else
    EXIFTagsDel($9209);
end;

procedure TIOParamsVals.SetEXIF_FocalLength(value: Double);
begin
  fEXIF_FocalLength := value;
  if value <> -1 then
    EXIFTagsAdd($920A)
  else
    EXIFTagsDel($920A);
end;

procedure TIOParamsVals.SetEXIF_ColorSpace(value: Integer);
begin
  fEXIF_ColorSpace := value;
  if value <> -1 then
    EXIFTagsAdd($A001)
  else
    EXIFTagsDel($A001);
end;

procedure TIOParamsVals.SetEXIF_ExifImageWidth(value: Integer);
begin
  fEXIF_ExifImageWidth := value;
  if value <> 0 then
    EXIFTagsAdd($A002)
  else
    EXIFTagsDel($A002);
end;

procedure TIOParamsVals.SetEXIF_ExifImageHeight(value: Integer);
begin
  fEXIF_ExifImageHeight := value;
  if value <> 0 then
    EXIFTagsAdd($A003)
  else
    EXIFTagsDel($A003);
end;

procedure TIOParamsVals.SetEXIF_FocalPlaneXResolution(value: Double);
begin
  fEXIF_FocalPlaneXResolution := value;
  if value <> -1 then
    EXIFTagsAdd($A20E)
  else
    EXIFTagsDel($A20E);
end;

procedure TIOParamsVals.SetEXIF_FocalPlaneYResolution(value: Double);
begin
  fEXIF_FocalPlaneYResolution := value;
  if value <> -1 then
    EXIFTagsAdd($A20F)
  else
    EXIFTagsDel($A20F);
end;

procedure TIOParamsVals.SetEXIF_FocalPlaneResolutionUnit(value: Integer);
begin
  fEXIF_FocalPlaneResolutionUnit := value;
  if value <> -1 then
    EXIFTagsAdd($A210)
  else
    EXIFTagsDel($A210);
end;

procedure TIOParamsVals.SetEXIF_ExposureIndex(value: Double);
begin
  fEXIF_ExposureIndex := value;
  if value <> -1 then
    EXIFTagsAdd($A215)
  else
    EXIFTagsDel($A215);
end;

procedure TIOParamsVals.SetEXIF_SensingMethod(value: Integer);
begin
  fEXIF_SensingMethod := value;
  if value <> -1 then
    EXIFTagsAdd($A217)
  else
    EXIFTagsDel($A217);
end;

procedure TIOParamsVals.SetEXIF_FileSource(value: Integer);
begin
  fEXIF_FileSource := value;
  if value <> -1 then
    EXIFTagsAdd($A300)
  else
    EXIFTagsDel($A300);
end;

procedure TIOParamsVals.SetEXIF_SceneType(value: Integer);
begin
  fEXIF_SceneType := value;
  if value <> -1 then
    EXIFTagsAdd($A301)
  else
    EXIFTagsDel($A301);
end;

function TIOParamsVals.GetEXIF_DateTime2: TDateTime;
begin
  Result := EXIFDateToDateTime(string(EXIF_DateTime));
end;

function TIOParamsVals.GetEXIF_DateTimeDigitized2: TDateTime;
begin
  Result := EXIFDateToDateTime(string(EXIF_DateTimeDigitized));
end;

function TIOParamsVals.GetEXIF_DateTimeOriginal2: TDateTime;
begin
  Result := EXIFDateToDateTime(string(EXIF_DateTimeOriginal));
end;

{!!
<FS>TIOParamsVals.AdjustGPSCoordinates

<FM>Declaration<FC>
procedure AdjustGPSCoordinates();

<FM>Description<FN>
Adjusts GPS (ie <A TIOParamsVals.EXIF_GPSLongitudeDegrees>, <A TIOParamsVals.EXIF_GPSLongitudeMinutes>, etc...), to fit into integer values (when possible).
This method is automatically called loading EXIF-GPS tags.   
!!}
procedure TIOParamsVals.AdjustGPSCoordinates();

  procedure AdjustOne(dir: AnsiString; var degrees: Double; var minutes: Double; var seconds: Double; var ref: AnsiString);
  var
    v: Double;
  begin
    v := IEGPSConvertDMSToDegDec(degrees, minutes, seconds, ref);
    IEGPSConvertDegDecToDMS(dir, v, degrees, minutes, seconds, ref);
  end;

begin
  AdjustOne('SN', fEXIF_GPSLatitudeDegrees, fEXIF_GPSLatitudeMinutes, fEXIF_GPSLatitudeSeconds, fEXIF_GPSLatitudeRef);
  AdjustOne('WE', fEXIF_GPSLongitudeDegrees, fEXIF_GPSLongitudeMinutes, fEXIF_GPSLongitudeSeconds, fEXIF_GPSLongitudeRef);
  AdjustOne('SN', fEXIF_GPSDestLatitudeDegrees, fEXIF_GPSDestLatitudeMinutes, fEXIF_GPSDestLatitudeSeconds, fEXIF_GPSDestLatitudeRef);
  AdjustOne('WE', fEXIF_GPSDestLongitudeDegrees, fEXIF_GPSDestLongitudeMinutes, fEXIF_GPSDestLongitudeSeconds, fEXIF_GPSDestLongitudeRef);
end;

function LatLongStr(const dGPSDegrees, dGPSMinutes, dGPSSeconds :  Double; const sGPSReference :  string) : string;

  // remove the null terminator from a string
  function RemoveNull(Value : string) : string;
  begin
    result := trim(Value);
    if (result <> '') and
       (result[length(result)] = #0) then
         SetLength(result, length(result) - 1);
      result := trim(result);
  end;

var
  iOutDegrees :  Integer;
  iOutMinutes :  Integer;
  dOutSeconds :  Double;
begin
  Result  := '';
  if (dGPSDegrees <> 0) or (RemoveNull(sGPSReference)<>'') then
  begin
    iOutDegrees  := Trunc(dGPSDegrees);
    iOutMinutes  := Trunc(dGPSMinutes);
    dOutSeconds  := dGPSSeconds;
    if dOutSeconds = 0 then
      dOutSeconds  := Frac(dGPSMinutes) * 60;

    result := IntToStr(iOutDegrees) + '° ' +
             IntToStr(iOutMinutes) + ''' ' +
             IEFloatToFormatString(dOutSeconds, 2, True) + '" ' +
             Uppercase(RemoveNull(sGPSReference));
  end;
end;
              
function TIOParamsVals.GetEXIF_GPSLatitude_Str: string;
begin
  Result := string( LatLongStr(EXIF_GPSLatitudeDegrees, EXIF_GPSLatitudeMinutes, EXIF_GPSLatitudeSeconds, string(EXIF_GPSLatitudeRef)) );
end;

procedure TIOParamsVals.SetEXIF_GPSLatitude(value: Double);
begin
  IEGPSConvertDegDecToDMS('SN', value, fEXIF_GPSLatitudeDegrees, fEXIF_GPSLatitudeMinutes, fEXIF_GPSLatitudeSeconds, fEXIF_GPSLatitudeRef);
end;

function TIOParamsVals.GetEXIF_GPSLatitude(): Double;
begin
  result := IEGPSConvertDMSToDegDec(EXIF_GPSLatitudeDegrees, EXIF_GPSLatitudeMinutes, EXIF_GPSLatitudeSeconds, EXIF_GPSLatitudeRef);
end;

function TIOParamsVals.GetEXIF_GPSLongitude(): Double;
begin
  result := IEGPSConvertDMSToDegDec(EXIF_GPSLongitudeDegrees, EXIF_GPSLongitudeMinutes, EXIF_GPSLongitudeSeconds, EXIF_GPSLongitudeRef);
end;

function TIOParamsVals.GetEXIF_GPSLongitude_Str: string;
begin
  Result := string( LatLongStr(EXIF_GPSLongitudeDegrees, EXIF_GPSLongitudeMinutes, EXIF_GPSLongitudeSeconds, string(EXIF_GPSLongitudeRef)) );
end;


procedure TIOParamsVals.SetEXIF_GPSLongitude(value: Double);
begin
  IEGPSConvertDegDecToDMS('WE', value, fEXIF_GPSLongitudeDegrees, fEXIF_GPSLongitudeMinutes, fEXIF_GPSLongitudeSeconds, fEXIF_GPSLongitudeRef);
end;



function DICOMCompression2Str(Compression: TIEDicomCompression): AnsiString;
{ Note, do not use a conversion like this! Will cause AV on startup C++ apps with RunTime packages disabled
const
  DICOM_TS2STR: array [TIEDicomCompression] of ^AnsiString = (
    @IEDICOM_TRANSFERSYNTAX_UNCOMP_LITTLEENDIAN_IMPLICIT,
    @IEDICOM_TRANSFERSYNTAX_UNCOMP_LITTLEENDIAN_EXPLICIT,
    @IEDICOM_TRANSFERSYNTAX_UNCOMP_BIGENDIAN_EXPLICIT,
    @IEDICOM_TRANSFERSYNTAX_RLE,
    @IEDICOM_TRANSFERSYNTAX_LOSSLESSJPEG1,
    @IEDICOM_TRANSFERSYNTAX_LOSSLESSJPEG2,
    @IEDICOM_TRANSFERSYNTAX_LOSSYJPEG8BIT,
    @IEDICOM_TRANSFERSYNTAX_LOSSYJPEG12BIT,
    @IEDICOM_TRANSFERSYNTAX_JPEG2000,
    @IEDICOM_TRANSFERSYNTAX_MPEG); }
begin
  case Compression of
    iedcUncompressed_Implicit:
      result := IEDICOM_TRANSFERSYNTAX_UNCOMP_LITTLEENDIAN_IMPLICIT;
    iedcUncompressed:
      result := IEDICOM_TRANSFERSYNTAX_UNCOMP_LITTLEENDIAN_EXPLICIT;
    iedcUncompressed_BE:
      result := IEDICOM_TRANSFERSYNTAX_UNCOMP_BIGENDIAN_EXPLICIT;
    iedcRLE:
      result := IEDICOM_TRANSFERSYNTAX_RLE;
    iedcLSJPEG1:
      result := IEDICOM_TRANSFERSYNTAX_LOSSLESSJPEG1;
    iedcLSJPEG2:
      result := IEDICOM_TRANSFERSYNTAX_LOSSLESSJPEG2;
    iedcJPEG:
      result := IEDICOM_TRANSFERSYNTAX_LOSSYJPEG8BIT;
    iedcJPEG12Bit:
      result := IEDICOM_TRANSFERSYNTAX_LOSSYJPEG12BIT;
    iedcJPEG2000:
      result := IEDICOM_TRANSFERSYNTAX_JPEG2000;
    iedcMPEG:
      result := IEDICOM_TRANSFERSYNTAX_MPEG;
    else
      result := '';
  end;
end;


function TIOParamsVals.GetDICOM_Compression(): TIEDicomCompression;
var
  transfSyntax: AnsiString;
begin
  transfSyntax := IETrim(DICOM_Tags.GetTagString($0002, $0010));
  for result := iedcUncompressed_Implicit to iedcMPEG do
    if DICOMCompression2Str(result) = transfSyntax then
      exit;
  result := iedcUncompressed; // the default
end;


procedure TIOParamsVals.SetDICOM_Compression(Value: TIEDicomCompression);
begin
  DICOM_Tags.SetTagString($0002, $0010, DICOMCompression2Str(Value));
end;


procedure TIOParamsVals.SetXMP_Info(Value: AnsiString);
var
  xmpDict: TIEDictionary;
begin
  fXMP_Info := Value;
  xmpDict := TIEDictionary.Create();
  try
    xmpDict.Parse(WideString(fXMP_Info));
  finally
    if not xmpDict.IsEmpty() then
      Dict.Insert('XMP', xmpDict)
    else
      xmpDict.Free();
  end;
end;


{!!
<FS>TIOParamsVals.PS_PaperSize

<FM>Declaration<FC>
property PS_PaperSize : <A TIOPDFPaperSize>

<FM>Description<FN>
Provides a quick way to set <A TIOParamsVals.PS_PaperWidth> and <A TIOParamsVals.PS_PaperHeight> or interpret their current values.

<FM>Example<FC>
// Save using "US Letter" paper size
ImageEnView1.IO.Params.PS_PaperSize := iepLetter;
ImageEnView1.IO.SaveToFile('D:\output.ps');

// Which is the same as...
ImageEnView1.IO.Params.PS_PaperWidth  := 612;
ImageEnView1.IO.Params.PS_PaperHeight := 792;
ImageEnView1.IO.SaveToFile('D:\output.ps');  

<FM>See Also<FN>
- <A IEPaperSizeToStr>
- <A IEStrToPaperSize>
!!}
function TIOParamsVals.GetPS_PaperSize : TIOPDFPaperSize;
var
  I: Integer;
begin
  Result := iepUnknown;
  for I := Low(IOPDFPaperSizes) to High(IOPDFPaperSizes) do
    if (PS_PaperWidth  = IOPDFPaperSizes[I].Width) and
       (PS_PaperHeight = IOPDFPaperSizes[I].Height) then
    begin
      Result := IOPDFPaperSizes[I].Size;
      exit;
    end;
end;

procedure TIOParamsVals.SetPS_PaperSize(const value : TIOPDFPaperSize);
var
  I: Integer;
begin
  for I := Low(IOPDFPaperSizes) to High(IOPDFPaperSizes) do
    if IOPDFPaperSizes[I].Size = Value then
    begin
      PS_PaperWidth  := IOPDFPaperSizes[I].Width;
      PS_PaperHeight := IOPDFPaperSizes[I].Height;
      exit;
    end;
end;

{!!
<FS>TIOParamsVals.PDF_PaperSize

<FM>Declaration<FC>
property PDF_PaperSize : <A TIOPDFPaperSize>

<FM>Description<FN>
Provides a quick way to set <A TIOParamsVals.PDF_PaperWidth> and <A TIOParamsVals.PDF_PaperHeight> or interpret their current values.

<FM>Example<FC>
// Save using "US Letter" paper size
ImageEnView1.IO.Params.PDF_PaperSize := iepLetter;
ImageEnView1.IO.SaveToFile('D:\output.pdf');

// Which is the same as...
ImageEnView1.IO.Params.PDF_PaperWidth  := 612;
ImageEnView1.IO.Params.PDF_PaperHeight := 792;
ImageEnView1.IO.SaveToFile('D:\output.pdf');

<FM>See Also<FN>
- <A IEPaperSizeToStr>
- <A IEStrToPaperSize>
!!}
function TIOParamsVals.GetPDF_PaperSize : TIOPDFPaperSize;
var
  I: Integer;
begin
  Result := iepUnknown;
  for I := Low(IOPDFPaperSizes) to High(IOPDFPaperSizes) do
    if (PDF_PaperWidth  = IOPDFPaperSizes[I].Width) and
       (PDF_PaperHeight = IOPDFPaperSizes[I].Height) then
    begin
      Result := IOPDFPaperSizes[I].Size;
      exit;
    end;
end;

procedure TIOParamsVals.SetPDF_PaperSize(const value : TIOPDFPaperSize);
var
  I: Integer;
begin
  for I := Low(IOPDFPaperSizes) to High(IOPDFPaperSizes) do
    if IOPDFPaperSizes[I].Size = Value then
    begin
      PDF_PaperWidth  := IOPDFPaperSizes[I].Width;
      PDF_PaperHeight := IOPDFPaperSizes[I].Height;
      exit;
    end;
end;


/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>IEPaperSizeToStr

<FM>Declaration<FC>
function IEPaperSizeToStr(const ASize : TIOPDFPaperSize) : string;

<FM>Description<FN>
Convert a <A TIOPDFPaperSize> value to a human-readible string. <A TIOPDFPaperSize> values can be specified for <A TIOParamsVals.PS_PaperSize> and <A TIOParamsVals.PDF_PaperSize>.

See also: <A IEStrToPaperSize>

<FM>Example 1<FC>
Caption := IEPaperSizeToStr(iepLetter); // Caption becomes 'US Letter';

<FM>Example 2<FC>
procedure TMainForm.FormCreate(Sender: TObject);
var
  a: TIOPDFPaperSize;
begin
  // Fill combobox with available PDF paper sizes
  cmbPaperSize.Clear;
  for a := Low(TIOPDFPaperSize) to High(TIOPDFPaperSize) do
    cmbPaperSize.Items.Add(IEPaperSizeToStr(a));

  // Make "US Letter" the selected one
  cmbPaperSize.ItemIndex := cmbPaperSize.Items.IndexOf(IEPaperSizeToStr(iepLetter));
end;

// Set PDF paper size to user's selection
ImageEnView1.IO.Params.PDF_PaperSize := IEStrToPaperSize(cmbPaperSize.Text);
!!}
function IEPaperSizeToStr(const ASize : TIOPDFPaperSize) : string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(IOPDFPaperSizes) to High(IOPDFPaperSizes) do
    if IOPDFPaperSizes[I].Size = ASize then
    begin
      Result := IOPDFPaperSizes[I].Name;
      exit;
    end;
end;

{!!
<FS>IEStrToPaperSize

<FM>Declaration<FC>
function IEStrToPaperSize(const sSize : string; aDefault  : TIOPDFPaperSize = iepUnknown) : TIOPDFPaperSize;

<FM>Description<FN>
Converts a paper size name (e.g. as returned by <A IEPaperSizeToStr>) to a <A TIOPDFPaperSize> value. <A TIOPDFPaperSize> values are used by <A TIOParamsVals.PS_PaperSize> and <A TIOParamsVals.PDF_PaperSize>.

See also: <A IEPaperSizeToStr>

<FM>Example<FC>
procedure TMainForm.FormCreate(Sender: TObject);
var
  a: TIOPDFPaperSize;
begin
  // Fill combobox with available PDF paper sizes
  cmbPaperSize.Clear;
  for a := Low(TIOPDFPaperSize) to High(TIOPDFPaperSize) do
    cmbPaperSize.Items.Add(IEPaperSizeToStr(a));

  // Make "US Letter" the selected one
  cmbPaperSize.ItemIndex := cmbPaperSize.Items.IndexOf(IEPaperSizeToStr(iepLetter));
end;

// Set PDF paper size to user's selection
ImageEnView1.IO.Params.PDF_PaperSize := IEStrToPaperSize(cmbPaperSize.Text);
!!}
function IEStrToPaperSize(const sSize : string; aDefault  : TIOPDFPaperSize = iepUnknown) : TIOPDFPaperSize;
var
  I: Integer;
begin
  Result := aDefault;
  for I := Low(IOPDFPaperSizes) to High(IOPDFPaperSizes) do
    if SameText(IOPDFPaperSizes[I].Name, sSize) then
    begin
      Result := IOPDFPaperSizes[I].Size;
      exit;
    end;
end;




/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////



{!!
<FS>TImageEnIO.LoadFromStreamJpeg

<FM>Declaration<FC>
function LoadFromStreamJpeg(Stream: TStream): Boolean;

<FM>Description<FN>
Loads an image from a stream containing a JPEG file. The result will be false if an error is encountered, e.g. the file in the stream is not JPEG format (<A TImageEnIO.Aborting> will be true).

Note: If <A TImageEnIO.StreamHeaders> property is True, the stream must have a special header (saved with <A TImageEnIO.SaveToStreamJPEG>).

<FM>Example<FC>
// loads a JPEG file with LoadfFromStreamJPEG
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('C:\myfile.jpg', fmOpenRead);
  ImageEnView1.IO.LoadFromStreamJPG(fs);
  fs.free;
End;
!!}
// at the beginning could be a TStreamJpegHeader structure
function TImageEnIO.LoadFromStreamJpeg(Stream: TStream): Boolean;
var
  sh: TStreamJpegHeader;
  lp: int64;
  Progress: TProgressRec;
begin           
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamJpeg, Stream);
    Result := True;
  end;


  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    lp := Stream.Position;
    Stream.Read(sh, sizeof(sh)); // carica header TStreamJpegHeader
    if sh.ID <> 'JFIF' then
      // stream header don't exist, reset to beginning
      Stream.Position := lp;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    ReadJpegStream(Stream, nil, fIEBitmap, fParams, Progress, False, false, true, false, true, true, -1, fParams.IsNativePixelFormat);
    CheckDPI;
    if sh.ID = 'JFIF' then
      Stream.Position := lp + sizeof(sh) + sh.dim;
    if fAutoAdjustDPI then
      AdjustDPI;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    fParams.fFileName := '';
    fParams.fFileType := ioJPEG;
    Update;
    Result := Not fAborting;
  finally
    DoFinishWork;
  end;
end;

procedure TImageEnIO.SyncSaveToStreamJpeg(Stream: TStream; streamhead: boolean);
var
  sh: TStreamJpegHeader;
  lp, lp2: int64;
  Progress: TProgressRec;
  buf: pointer;
  lbuf, mi, i: integer;
begin                                     
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    lp := 0;
    if streamhead then
    begin
      lp := Stream.position; // save initial position
      Stream.write(sh, sizeof(sh)); // leaves spaces for the header
    end;

    // update APP13 - 8BIM dpi info (Photoshop) resolution. This is overwritten if IPTC is written.
    UpdateAPP138BimResolution;

    // update APP13 marker with IPTC_Info
    if (fParams.IPTC_Info.UserChanged) or ( (fParams.IPTC_Info.Count > 0) and (fParams.JPEG_MarkerList.Count = 0) ) then
    begin
      fParams.IPTC_Info.SaveToStandardBuffer(buf, lbuf, true);
      mi := fParams.JPEG_MarkerList.IndexOf(JPEG_APP13);
      if buf <> nil then
      begin
        // replace or add IPTC marker
        if mi >= 0 then
          fParams.JPEG_MarkerList.SetMarker(mi, JPEG_APP13, buf, lbuf)
        else
          fParams.JPEG_MarkerList.AddMarker(JPEG_APP13, buf, lbuf);
        freemem(buf);
      end
      else
      if mi >= 0 then
        // remove IPTC marker
        fParams.JPEG_MarkerList.DeleteMarker(mi);
    end;

    // Exif info
    if fParams.EXIF_HasExifData then
    begin
      // save in temporary buffer
      SaveEXIFToStandardBuffer(fParams, buf, lbuf, true);
      // remove previous EXIF data
      with fParams.JPEG_MarkerList do
        for i := 0 to Count - 1 do
          if (MarkerType[i] = JPEG_APP1) and CheckEXIFFromStandardBuffer(MarkerData[i], MarkerLength[i]) then
          begin
            DeleteMarker(i);
            break;
          end;
      // save in APP1
      fParams.JPEG_MarkerList.AddMarker(JPEG_APP1, buf, lbuf);
      freemem(buf);
    end;

    // XMP
    // remove previous XMP data
    with fParams.JPEG_MarkerList do
      for i := 0 to Count - 1 do
        if (MarkerType[i] = JPEG_APP1) and (IEFindXMPFromJpegTag(MarkerData[i], MarkerLength[i]) <> nil) then
        begin
          DeleteMarker(i);
          break;
        end;
    if fParams.XMP_Info <> '' then
    begin
      // save in temporary buffer
      IESaveXMPToJpegTag(fParams, buf, lbuf);
      // save in APP1
      fParams.JPEG_MarkerList.AddMarker(JPEG_APP1, buf, lbuf);
      freemem(buf);
    end;

    // ImageEn annotations (TImageEnVect objects)
    // remove previous ImageEn Annot data
    with fParams.JPEG_MarkerList do
      for i := 0 to Count - 1do
        if (MarkerType[i] = JPEG_APP1) and TIEImageEnAnnot.BufferContainsImageEnAnnot(MarkerData[i], MarkerLength[i]) then
        begin
          DeleteMarker(i);
          break;
        end;
    if not fParams.ImageEnAnnot.IsEmpty() then
    begin
      // save in temporary buffer
      fParams.ImageEnAnnot.SaveToBuffer(buf, lbuf);
      // save in APP1
      fParams.JPEG_MarkerList.AddMarker(JPEG_APP1, buf, lbuf);
      freemem(buf);
    end;

    // save jpeg
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    WriteJpegStream(Stream, fIEBitmap, fParams, Progress);
    if streamhead then
    begin
      lp2 := Stream.position; // saves final position
      Stream.position := lp; // return to initial position
      sh.ID := 'JFIF';
      sh.dim := lp2 - lp - sizeof(sh);
      Stream.Write(sh, sizeof(sh)); // Saves header TStreamJpegHeader
      Stream.position := lp2; // return to final position
    end;
  finally
    DoFinishWork;
  end;
end;


{!!
<FS>TImageEnIO.SaveToStreamJpeg

<FM>Declaration<FC>
procedure SaveToStreamJpeg(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in JPEG format.

If <A TImageEnIO.StreamHeaders> property is True, it adds an additional special header as needed for multi-image streams.


<FM>Example<FC>
// Saves ImageEnView1 and ImageEnView2 attached images in file images.dat
// images.dat isn't loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('bmpimages.dat', fmCreate);
  ImageEnView1.IO.StreamHeaders := True;
  ImageEnView1.IO.SaveToStreamJPEG(fs);
  ImageEnView2.IO.StreamHeaders := True;
  ImageEnView2.IO.SaveToStreamJPEG(fs);
  fs.free;
End;

// Saves a single image in image.jpg
// image.jpg is loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('image.jpg');
  ImageEnView1.IO.StreamHeaders := False;
  ImageEnView1.IO.SaveToFileJPEG(fs);
End;

!!}
procedure TImageEnIO.SaveToStreamJpeg(Stream: TStream);
begin                                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamJpeg, Stream);
    exit;
  end;

  SyncSaveToStreamJpeg(Stream, fStreamHeaders);
end;

{!!
<FS>TImageEnIO.SaveToFileJpeg

<FM>Declaration<FC>
procedure SaveToFileJpeg(const FileName: WideString);

<FM>Description<FN>
Saves the current image to a file in JPEG format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
// Saves a gray levels, progressive jpeg image with quality 70
ImageEnView1.IO.Params.JPEG_ColorSpace := ioJPEG_GRAYLEV;
ImageEnView1.IO.Params.JPEG_Quality := 70;
ImageEnView1.IO.Params.JPEG_Progressive := True;
ImageEnView1.IO.SaveToFileJpeg('C:\image.jpg');

!!}
procedure TImageEnIO.SaveToFileJpeg(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileJpeg, FileName);
    exit;
  end;

  fs := TIEWideFileStream.Create(FileName, fmCreate);
  try
    SyncSaveToStreamJpeg(fs, false);
    fParams.FileName := FileName;
    fParams.FileType := ioJPEG;
  finally
    fs.Free();
  end;
end;

{!!
<FS>TImageEnIO.PreviewFont

<FM>Declaration<FC>
property PreviewFont: TFont;

<FM>Description<FN>
If <A TImageEnIO.PreviewFontEnabled> is set to True then <FC>PreviewFont<FN> specifies the font used in the <A TImageEnIO.DoPreviews> dialog.

Note: You should ensure the size of the font matches the length of the dialog controls.

<FM>Example<FC>
ImageEnView1.IO.PreviewFont.Name := 'MS Times New Roman';
ImageEnView1.IO.PreviewFontEnabled := True;
ImageEnView1.IO.DoPreviews([ppAll]);
!!}
procedure TImageEnIO.SetPreviewFont(f: TFont);
begin
  fPreviewFont.assign(f);
end;



{!!
<FS>TImageEnIO.PreviewFontEnabled

<FM>Declaration<FC>
property PreviewFontEnabled: Boolean;

<FM>Description<FN>
When set to True then you can use <A TImageEnIO.PreviewFont> to specify a custom font for the <A TImageEnIO.DoPreviews> dialogs.

<FM>Example<FC>
ImageEnView1.IO.PreviewFont.Name := 'MS Times New Roman';
ImageEnView1.IO.PreviewFontEnabled := True;     
ImageEnView1.IO.DoPreviews([ppAll]);
!!}
procedure TImageEnIO.SetPreviewFontEnabled(Value : Boolean);
begin
  fPreviewFontEnabled := Value;
end;


procedure TImageEnIO.SyncLoadFromStreamBMP(Stream: TStream);
var
  Progress: TProgressRec;
  tmpAlphaChannel: TIEMask;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    tmpAlphaChannel := nil;
    BMPReadStream(Stream, fIEBitmap, 0, fParams, Progress, false, false, tmpAlphaChannel, not fParams.BMP_HandleTransparency);
    CheckDPI;
    if assigned(tmpAlphaChannel) then
    begin
      fIEBitmap.AlphaChannel.CopyFromTIEMask(tmpAlphaChannel);
      FreeAndNil(tmpAlphaChannel);
    end;
    if fAutoAdjustDPI then
      AdjustDPI;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    fParams.fFileName := '';
    fParams.fFileType := ioBMP;
    update;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamBMP

<FM>Declaration<FC>
function LoadFromStreamBMP(Stream: TStream): Boolean;

<FM>Description<FN>     
Loads an image from a stream containing a BMP file. The result will be false if an error is encountered, e.g. the file in the stream is not BMP format (<A TImageEnIO.Aborting> will be true).

Note: If <A TImageEnIO.StreamHeaders> property is True, the stream must have a special header (saved with <A TImageEnIO.SaveToStreamBMP>).
      
<FM>Example<FC>
// loads a BMP file with LoadfFromStreamBMP
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('C:\myfile.bmp', fmOpenRead);
  ImageEnView1.IO.LoadFromStreamBMP(fs);
  fs.free;
End;

!!}
function TImageEnIO.LoadFromStreamBMP(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin                    
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamBMP, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamBMP(Stream); 
    Result := Not fAborting;
  end;
end;



{!!
<FS>TImageEnIO.LoadFromFileBMP

<FM>Declaration<FC>
function LoadFromFileBMP(const FileName: WideString): Boolean;

<FM>Description<FN>     
Loads an image from a BMP file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not BMP format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFileBMP('C:\myimage.bmp');

!!}
function TImageEnIO.LoadFromFileBMP(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin             
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileBMP, FileName);
    Result := True;
    exit;
  end;

  
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamBMP(fs);
    fParams.fFileName := FileName;
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
end;




{!!
<FS>TImageEnIO.AssignParams

<FM>Declaration<FC>
procedure AssignParams(Source: TObject);

<FM>Description<FN>
Assign the file format parameters from another TImageEnIO component or <A TIOParamsVals> object.
!!}
// Copy only Params
procedure TImageEnIO.AssignParams(Source: TObject);
begin
  if Source is TImageEnIO then
    fParams.assign((Source as TImageEnIO).Params)
  else
  if Source is TIOParamsVals then
    fParams.assign(Source as TIOParamsVals);
end;

{!!
<FS>IsKnownFormat

<FM>Declaration<FC>
function IsKnownFormat(const FileName : WideString; bIncludeVideoFiles : Boolean = False) : boolean;

<FM>Description<FN>
Returns <FC>true<FN> if the specified filename is a supported file format.

By default, this only includes image formats. Set bIncludeVideoFiles to true to include AVI, MPEG and WMV

Note: This method only checks that file extension is recognized (e.g. .JPEG of image.jpeg). To examine the content of the image to test it can be read use <A FindFileFormat>

See also: <A IsKnownSaveFormat>

<FM>Example<FC>
If IsKnownFormat('C:\test.fax') then
  ShowMessage('ok, I can load it');
!!}
function IsKnownFormat(const FileName: WideString; bIncludeVideoFiles : Boolean = False): boolean;
var
  fpi: TIEFileFormatInfo;
  sExt: string;
begin
  sExt := Lowercase(string(IEExtractFileExtW(FileName)));

  if IEFileExtInExtensions(sExt, Supported_Video_File_Extensions) then
  begin
    Result := bIncludeVideoFiles;
  end
  else
  begin
    fpi := IEFileFormatGetInfo2(sExt);
    result := assigned(fpi) and (@fpi.ReadFunction <> nil);
  end;
end;

{!!
<FS>IsKnownSaveFormat

<FM>Declaration<FC>
function IsKnownSaveFormat(const FileName : WideString) : boolean;

<FM>Description<FN>
Returns <FC>true<FN> if the specified filename is a file format that ImageEn supports saving (by checking its file extension).

Note: There are a variety of formats (such as WMF) which ImageEn can load but not save.

See also: <A IsKnownFormat>

<FM>Example<FC>
If IsKnownSaveFormat('test.dcm') then
  ShowMessage('ok, I can save it');
!!}
function IsKnownSaveFormat(const FileName: WideString): boolean;
var
  fpi: TIEFileFormatInfo;
  sExt: string;
begin
  sExt := Lowercase(string(IEExtractFileExtW(FileName)));
  fpi := IEFileFormatGetInfo2(sExt);
  result := assigned(fpi) and (@fpi.WriteFunction <> nil);
end;


{!!
<FS>FindFileFormat

<FM>Declaration<FC>
function FindFileFormat(const FileName: WideString; VerifyExtension: Boolean): <A TIOFileType>;

<FM>Description<FN>
Returns the file format of the specified file by reading the file and trying to recognize the file header.

If <FC>VerifyExtension<FN> is False, then <FC>FindFileFormat<FN> function doesn't use the file extension to determine the format.

<FC>FineFileFormat<FN> uses <A FindStreamFormat> to detect image format from file header.

<FM>Example<FC>
var
  ss: TIOFileType;
begin
  ss := FindFileFormat('C:\myfile.dat', false);
  if ss = ioGIF then
    ImageEnView1.IO.LoadFromFileGIF('C:\myfile.dat')
  else
  if ss = ioJPEG then
    ImageEnView1.IO.LoadFromFileJpeg('C:\myfile.dat');
end;
!!}
// Recognizes: JPG, GIF, PCX, DCX, BMP, TIF, PNG, ICO, CUR, TGA, PXM, JP2, JPC, J2C, J2K, RAW, PSD, HDP
// supports registered file formats
function FindFileFormat(const FileName: WideString; VerifyExtension: boolean): TIOFileType;
var
  fs: TIEWideFileStream;
  fpi: TIEFileFormatInfo;
begin
  result := ioUnknown;
  if (FileName='') or not IEFileExistsW(FileName) then
    exit;
  // verify stream
  try
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      result := FindStreamFormat(fs);
    finally
      FreeAndNil(fs);
    end;
  except
  end;

  fpi := IEFileFormatGetInfo2(string(IEExtractFileExtW(FileName)));

  // raw formats cannot be confused with TIFF, then we must check the extension
  if (fpi <> nil) and (fpi.FileType = ioRAW) and (result = ioTIFF) then
    result := ioRAW;

  if (fpi <> nil) and not fpi.InternalFormat then
    result := fpi.FileType
  else
  if VerifyExtension and (result <> ioUnknown) then
  begin
    // verify extension
    if assigned(fpi) then
    begin
      if (result <> ioICO) and (result <> ioCUR) then
      begin
        if result <> fpi.FileType then
          result := ioUnknown;
      end
      else
        result := fpi.FileType;
    end
    else
      result := ioUnknown;
  end;
end;

function AVITryStream(Stream: TStream): boolean;
var
  l: int64;
  s1, s2: array[0..3] of AnsiChar;
begin
  l := Stream.Position;
  Stream.Read(s1[0], 4);
  Stream.Seek(4, soCurrent);
  Stream.Read(s2[0], 4);
  result := (s1 = 'RIFF') and (s2 = 'AVI ');
  Stream.Position := l;
end;

{!!
<FS>FindStreamFormat

<FM>Declaration<FC>
function FindStreamFormat(Stream: TStream): <A TIOFileType>;

<FM>Description<FN>
Returns the format of the file found in a stream by reading the file header.

This function can recognize the following file formats:
<TABLE>
<R> <H>Return value</H> <H>File format</H> </R>
<R> <C>ioGIF</C> <C>GIF</C> </R>
<R> <C>ioBMP</C> <C>BMP</C> </R>
<R> <C>ioPCX</C> <C>PCX</C> </R>
<R> <C>ioTIFF</C> <C>TIFF</C> </R>
<R> <C>ioPNG</C> <C>PNG</C> </R>
<R> <C>ioDICOM</C> <C>DICOM</C> </R>
<R> <C>ioICO</C> <C>ICO</C> </R>
<R> <C>ioCUR</C> <C>CUR</C> </R>
<R> <C>ioTGA</C> <C>TGA</C> </R>
<R> <C>ioPXM</C> <C>PXM</C> </R>
<R> <C>ioJPEG</C> <C>JPEG</C> </R>
<R> <C>ioJP2, ioJ2K</C> <C>JPEG2000</C> </R>
<R> <C>ioAVI</C> <C>AVI</C> </R>
<R> <C>ioDCX</C> <C>DCX</C> </R>
<R> <C>ioWMF</C> <C>WMF</C> </R>
<R> <C>ioEMF</C> <C>EMF</C> </R>
<R> <C>ioPSD</C> <C>PSD</C> </R>
<R> <C>ioIEV</C> <C>IEV (ImageEn vectorial objects)</C> </R>
<R> <C>ioLYR</C> <C>LYR (ImageEn layers)</C> </R>
<R> <C>ioALL</C> <C>ALL (ImageEn vectorial objects and layers)</C> </R>
<R> <C>ioRAW</C> <C>RAW (when dcraw plugin is not installed)</C> </R>
<R> <C>ioHDP</C> <C>Microsoft HD Photo</C> </R>
<R> <C>ioRawDLLPlugIn</C> <C>DCRAW external plug-in for Digital Camera Raw file support</C> </R>
<R> <C>ioOtherDLLPlugIns + offset</C> <C>External plugins (e.g. JBIG)</C> </R>
<R> <C>ioUSER + offset</C> <C>User registered file formats</C> </R>
</TABLE>

!!}
function FindStreamFormat(Stream: TStream): TIOFileType;
var
  Size: integer;
  HeaderJpegStream: TStreamJpegHeader;
  HeaderGIF: TGIFHeader;
  HeaderPcx: PCXSHead;
  HeaderPcx2: TPcxHeader;
  HeaderBmp: TBITMAPFILEHEADER;
  HeaderTIFF: TIFFSHead;
  id: array[0..3] of AnsiChar;
  lp: int64;
  q: integer;
begin
  result := ioUnknown;
  try
    lp := Stream.Position;
    Size := Stream.Size;

    try

      // try jpeg (with extra header)
      if Size > sizeof(TStreamJpegHeader) then
      begin
        Stream.Read(HeaderJpegStream, sizeof(HeaderJpegStream));
        Stream.Position := lp;
        if HeaderJpegStream.ID = 'JFIF' then
        begin
          result := ioJPEG;
          exit;
        end;
      end;

      // try GIF (no extra header)
      if Size > sizeof(TGIFHeader) then
      begin
        Stream.Read(HeaderGIF, sizeof(HeaderGIF));
        Stream.Position := lp;
        if (headergif.id[0] = 'G') and (headergif.id[1] = 'I') and (headergif.id[2] = 'F') then
        begin
          result := ioGIF;
          exit;
        end;
      end;

      // try PCX (with extra header - ver.2)
      if Size > sizeof(PCXSHead) then
      begin
        Stream.Read(HeaderPCX, sizeof(HeaderPCX));
        Stream.Position := lp;
        if HeaderPCX.id = 'PCX2' then
        begin
          result := ioPCX;
          exit;
        end;
      end;

      // try PCX (with extra header - ver.1)
      if Size > 4 then
      begin
        Stream.Read(id, 4);
        Stream.Position := lp;
        if id = 'PCX' then
        begin
          result := ioPCX;
          exit;
        end;
      end;

      // try BMP (no extra header)
      if Size > sizeof(HeaderBmp) then
      begin
        Stream.Read(HeaderBmp, sizeof(HeaderBmp));
        Stream.Position := lp;
        if HeaderBmp.bfType = 19778 then
        begin
          result := ioBMP;
          exit;
        end;
      end;

      // try PCX (no extra header)
      if Size > sizeof(TPcxHeader) then
      begin
        Stream.Read(HeaderPcx2, sizeof(HeaderPcx2));
        Stream.Position := lp;
        if (HeaderPcx2.Manufacturer = $0A) and (HeaderPcx2.Version <= 5) then
        begin
          result := ioPCX;
          exit;
        end;
      end;

      // try TIFF (with extra header)
      if Size > sizeof(TIFFSHead) then
      begin
        Stream.Read(HeaderTIFF, sizeof(HeaderTIFF));
        Stream.Position := lp;
        if HeaderTIFF.ID = 'TIFF' then
        begin
          result := ioTIFF;
          exit;
        end;
      end;

      {$ifdef IEINCLUDEWIC}
      // try HDP (Microsoft PhotoHD)
      if IsHDPStream(Stream) then
      begin
        result := ioHDP;
        exit;
      end;
      {$endif}

      // try TIFF (no extra header)
      if IsTIFFStream(Stream) and not IsDNGStream(Stream) then
      begin
        result := ioTIFF;
        exit;
      end;

      // try PNG
  {$IFDEF IEINCLUDEPNG}
      if Size > 8 then
      begin
        if IsPNGStream(Stream) then
        begin
          result := ioPNG;
          exit;
        end;
      end;
  {$ENDIF}

      // try DICOM
  {$ifdef IEINCLUDEDICOM}
      if IEDicomTryStream(Stream) then
      begin
        result := ioDICOM;
        exit;
      end;
  {$endif}

      // try ICO
      if IcoTryStream(Stream) then
      begin
        result := ioICO;
        exit;
      end;

      // try CUR
      if CurTryStream(Stream) then
      begin
        result := ioCUR;
        exit;
      end;

      // try TGA
      if TryTGA(Stream) then
      begin
        result := ioTGA;
        exit;
      end;

      // try PXM
      if tryPXM(Stream) then
      begin
        result := ioPXM;
        exit;
      end;

  {$IFDEF IEINCLUDEJPEG2000}
      // try jp2
      if J2KTryStreamJP2(Stream) then
      begin
        result := ioJP2;
        exit;
      end;
      // try j2k, jpc, j2c
      if J2KTryStreamJ2K(Stream) then
      begin
        result := ioJ2K;
        exit;
      end;
  {$ENDIF}

      // try AVI
      if AVITryStream(Stream) then
      begin
        result := ioAVI;
        exit;
      end;

      // try DCX
      if IEDCXTryStream(Stream) then
      begin
        result := ioDCX;
        exit;
      end;

      // try WMF
      if IEWMFTryStream(Stream) then
      begin
        result := ioWMF;
        exit;
      end;

      // try EMF
      if IEEMFTryStream(Stream) then
      begin
        result := ioEMF;
        exit;
      end;

      {$ifdef IEINCLUDEPSD}

      // try PSD
      if IETryPSD(Stream) then
      begin
        result := ioPSD;
        exit;
      end;

      {$endif}

      // try IEV
      if IETryIEV(Stream) then
      begin
        result := ioIEV;
        exit;
      end;

      // try LYR
      if IETryLYR(Stream) then
      begin
        result := ioLYR;
        exit;
      end;

      // try ALL (LYR+IEV)
      if IETryALL(Stream) then
      begin
        result := ioALL;
        exit;
      end;

      // try jpeg (without extra header)
      if JpegTryStream(Stream, true) >= 0 then
      begin
        result := ioJPEG;
        exit;
      end;

      // try user registered file formats
      for q := 0 to IEGlobalSettings().FileFormats.Count - 1 do
        with TIEFileFormatInfo(IEGlobalSettings().FileFormats[q]) do
          if assigned(TryFunction) then
          begin
            Stream.Position := lp;
            if TryFunction(Stream, FileType) then
            begin
              result := FileType;
              break;
            end;
          end;
      Stream.Position := lp;
      if result <> ioUnknown then
        exit;

      // try RAW
      {$ifdef IEINCLUDERAWFORMATS}
      if IERAWTryStream(Stream) then
      begin
        result := ioRAW;
        exit;
      end;
      {$endif}

    finally
      Stream.Position := lp;
    end;

  except
  end;
end;

// reset fBitmap (called from RegisterBitmapChangeEvent)
procedure TImageEnIO.OnBitmapChange(Sender: TObject; destroying: boolean);
begin
  if destroying then
  begin
    fImageEnView := nil;
  end
  else
  if assigned(fImageEnView) then
  begin
    if assigned(fIEBitmap) then
    begin
      fIEBitmap := fImageEnView.IEBitmap;
      fBitmap := nil; // both fBitmap and fIEBitmap aren't allowed if not encapsulated
    end
    else
    if assigned(fBitmap) then
    begin
      fBitmap := fImageEnView.Bitmap;
      if fIEBitmapCreated then
        fIEBitmap.EncapsulateTBitmap(fBitmap, true)
    end;

    fParams.DpiX := fImageEnView.DpiX;
    fParams.DpiY := fImageEnView.DpiY;
  end;
end;

{$IFDEF IEINCLUDEPNG}

{!!
<FS>TImageEnIO.LoadFromFilePNG

<FM>Declaration<FC>
function LoadFromFilePNG(const FileName: WideString): Boolean;

<FM>Description<FN>       
Loads an image from a PNG file.

<FC>FileName<FN> is the file name including extension.
 Result will be false if the file is not PNG format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

!!}
function TImageEnIO.LoadFromFilePNG(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
  Progress: TProgressRec;
begin
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFilePNG, FileName);
    Result := True;
    exit;
  end;
  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      fParams.PNG_Background := tcolor2trgb(Background);
      fIEBitmap.RemoveAlphaChannel;
      ReadPNGStream(fs, fIEBitmap, fParams, Progress, false);
      CheckDPI;
      if fAutoAdjustDPI then
        AdjustDPI;
      fParams.fFileName := FileName;
      fParams.fFileType := ioPNG;
      SetViewerDPI(fParams.DpiX, fParams.DpiY);
      BackGround := TRGB2TCOLOR(fParams.PNG_Background);
      Update();
    finally
      FreeAndNil(fs);  
      Result := Not fAborting;
    end;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEPNG}

{!!
<FS>TImageEnIO.LoadFromStreamPNG

<FM>Declaration<FC>
function LoadFromStreamPNG(Stream: TStream): Boolean;

<FM>Description<FN>        
Loads an image from a stream containing a PNG file. The result will be false if an error is encountered, e.g. the file in the stream is not PNG format (<A TImageEnIO.Aborting> will be true).

!!}
function TImageEnIO.LoadFromStreamPNG(Stream: TStream): Boolean;
var
  Progress: TProgressRec;
begin        
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamPNG, Stream);
    Result := True;
    exit;
  end;

  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    ReadPNGStream(Stream, fIEBitmap, fParams, Progress, false);
    CheckDPI;
    if fAutoAdjustDPI then
      AdjustDPI;
    fParams.fFileName := '';
    fParams.fFileType := ioPNG;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    BackGround := TRGB2TCOLOR(fParams.PNG_Background);
    update;
    Result := Not fAborting;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEPNG}

{!!
<FS>TImageEnIO.SaveToFilePNG

<FM>Declaration<FC>
procedure SaveToFilePNG(const FileName: WideString);

<FM>Description<FN>
Saves the current image to a file in PNG format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
// Set best compression
ImageEnView1.IO.Params.PNG_Filter := ioPNG_FILTER_PAETH;
ImageEnView1.IO.Params.PNG_Compression := 9;
// Save PNG
ImageEnView1.IO.SaveToFilePNG('D:\max.png');

!!}
procedure TImageEnIO.SaveToFilePNG(const FileName: WideString);
var
  fs: TIEWideFileStream;
  Progress: TProgressRec;
  iemask: TIEMask;
begin         
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFilePNG, FileName);
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    try
      if fIEBitmap.HasAlphaChannel then
      begin
        iemask := TIEMask.Create();
        fIEBitmap.AlphaChannel.CopyToTIEMask(iemask);
        WritePNGStream(fs, fIEBitmap, fParams, Progress, iemask);
        FreeAndNil(iemask);
      end
      else
        WritePNGStream(fs, fIEBitmap, fParams, Progress, nil);
      fParams.FileName := FileName;
      fParams.FileType := ioPNG;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEPNG}

{!!
<FS>TImageEnIO.SaveToStreamPNG

<FM>Declaration<FC>
procedure SaveToStreamPNG(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in PNG format.

<FM>Example<FC>
// Set best compression
ImageEnView1.IO.Params.PNG_Filter := ioPNG_FILTER_PAETH;
ImageEnView1.IO.Params.PNG_Compression := 9;
// Save PNG
ImageEnView1.IO.SaveToStreamPNG(Stream);

!!}
procedure TImageEnIO.SaveToStreamPNG(Stream: TStream);
var
  Progress: TProgressRec;
  iemask: TIEMask;
begin          
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamPNG, Stream);
    exit;
  end;

  
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    if fIEBitmap.HasAlphaChannel then
    begin
      iemask := TIEMask.Create;
      fIEBitmap.AlphaChannel.CopyToTIEMask(iemask);
      WritePNGStream(Stream, fIEBitmap, fParams, Progress, iemask);
      FreeAndNil(iemask);
    end
    else
      WritePNGStream(Stream, fIEBitmap, fParams, Progress, nil);
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$ifdef IEINCLUDEDICOM}

{!!
<FS>TImageEnIO.LoadFromFileDICOM

<FM>Declaration<FC>
function LoadFromFileDICOM(const FileName: WideString): Boolean;

<FM>Description<FN>        
Loads an image from a DICOM file. This method is necessary for DICOM files that don't have an extension or a valid DICOM header, but you know to be DICOM format.

<FC>FileName<FN> is the file name including extension.
 Result will be false if the file is not DICOM format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

Note: DICOM parameters are stored in <A TIOParamsVals.DICOM_Tags>.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFileDICOM('heart.dcm');

Load the second image of a multiple image DICOM
ImageEnView1.IO.Params.ImageIndex := 1;
ImageEnView1.IO.LoadFromFileDICOM('D:\MyImage.dcm');
!!}
function TImageEnIO.LoadFromFileDICOM(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
  Progress: TProgressRec;
begin         
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileDICOM, FileName);
    Result := True;
    exit;
  end;
  

  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      fIEBitmap.RemoveAlphaChannel;
      IEDicomRead(fs, fParams, fIEBitmap, Progress, false);
      fParams.fFileName := FileName;
      fParams.fFileType := ioDICOM;
      Update;
    finally
      FreeAndNil(fs);       
      Result := Not fAborting;
    end;
  finally
    DoFinishWork;
  end;
end;


{!!
<FS>TImageEnIO.LoadFromStreamDICOM

<FM>Declaration<FC>
function LoadFromStreamDICOM(Stream: TStream): Boolean;

<FM>Description<FN>         
Loads an image from a stream containing a DICOM file. The result will be false if an error is encountered, e.g. the file in the stream is not DICOM format (<A TImageEnIO.Aborting> will be true).

Note: DICOM parameters are stored in <A TIOParamsVals.DICOM_Tags>.
!!}
function TImageEnIO.LoadFromStreamDICOM(Stream: TStream): Boolean;
var
  Progress: TProgressRec;
begin   
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamDICOM, Stream);
    Result := True;
    exit;
  end;

  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    IEDicomRead(Stream, fParams, fIEBitmap, Progress, false);
    fParams.fFileName := '';
    fParams.fFileType := ioDICOM;
    Update;
    Result := Not fAborting;
  finally
    DoFinishWork;
  end;
end;

procedure TImageEnIO.SyncSaveToStreamDICOM(Stream: TStream);
var
  Progress: TProgressRec;
  context: pointer;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;

    if not MakeConsistentBitmap([]) then
      exit;
    case fIEBitmap.PixelFormat of
      ie1g:
        begin
          fParams.BitsPerSample   := 1;
          fParams.SamplesPerPixel := 1;
        end;
      ie8g:
        begin
          fParams.BitsPerSample   := 8;
          fParams.SamplesPerPixel := 1;
        end;
      ie16g:
        begin
          fParams.BitsPerSample   := 16;
          fParams.SamplesPerPixel := 1;
        end;
      ie24RGB:
        begin
          fParams.BitsPerSample   := 8;
          fParams.SamplesPerPixel := 3;
        end;
      else
        raise EIEException.Create('DICOM saving: unsupported pixel format');
    end;
    fParams.Width  := fIEBitmap.Width;
    fParams.Height := fIEBitmap.Height;

    context := IEDicomWrite_init(Stream, fParams, 1, Progress);
    IEDicomWrite_addImage(context, fIEBitmap);
    IEDicomWrite_finalize(context);
  finally
    DoFinishWork;
  end;
end;


{!!
<FS>TImageEnIO.SaveToStreamDicom

<FM>Declaration<FC>
procedure SaveToStreamDicom(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in DICOM format.

If <A TImageEnIO.StreamHeaders> property is true, then a special header is added as required for multi-image streams.

<FM>Example<FC>
// Saves images in ImageEnView1 and ImageEnView2 to the file Images.dat
// Note: images.dat isn't loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('C:\images.dat', fmCreate);
  ImageEnView1.IO.StreamHeaders := True;
  ImageEnView1.IO.SaveToStreamDicom(fs);
  ImageEnView2.IO.StreamHeaders := True;
  ImageEnView2.IO.SaveToStreamDicom(fs);
  fs.free;
End;

// Saves a single image to image.Dicom
// image.Dicom is loadable with LoadFromFileXXX methods
var
  fs: TFileStream;
Begin
  fs := TFileStream.Create('image.Dicom');
  ImageEnView1.IO.StreamHeaders := False;
  ImageEnView1.IO.SaveToFileDicom(fs);
End;

!!}

procedure TImageEnIO.SaveToStreamDICOM(Stream: TStream);
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin                                   
    // ASYNC SAVING
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamDICOM, Stream);
  end
  else
  begin
    SyncSaveToStreamDICOM(Stream);
  end;  
end;

{!!
<FS>TImageEnIO.SaveToFileDicom

<FM>Declaration<FC>
procedure SaveToFileDicom(const FileName: WideString);

<FM>Description<FN>                                         
Saves the current image to a file in Dicom medical imaging format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
ImageEnView1.IO.SaveToFileDicom('D:\image.dicom');

!!}

procedure TImageEnIO.SaveToFileDICOM(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin                                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileDICOM, FileName);
    exit;
  end;

  
  fs := TIEWideFileStream.Create(FileName, fmCreate);
  try
    SyncSaveToStreamDICOM(fs);
    fParams.FileName := FileName;
    fParams.FileType := ioDICOM;
  finally
    FreeAndNil(fs);
  end;
end;


{$endif}  // IEINCLUDEDICOM


{!!
<FS>TImageEnIO.PreviewsParams

<FM>Declaration<FC>
property PreviewsParams: <A TIOPreviewsParams>;

<FM>Description<FN>
Specifies the features of the <L TImageEnIO.DoPreviews>File Format Parameters dialog</L>.

<FM>Example<FC>
ImageEnView1.IO.PreviewsParams := [ioppDefaultLockPreview];
ImageEnView1.IO.DoPreviews([ppTIFF]);
!!}
procedure TImageEnIO.SetIOPreviewParams(v: TIOPreviewsParams);
begin
  fPreviewsParams := v;
end;

function TImageEnIO.GetIOPreviewParams: TIOPreviewsParams;
begin
  result := fPreviewsParams;
end;

{!!
<FS>TImageEnIO.LoadFromFileICO

<FM>Declaration<FC>
function LoadFromFileICO(const FileName: WideString): Boolean;

<FM>Description<FN>          
Loads an image from an icon file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not ICO format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFileICO('C:\gates.ico');

!!}
function TImageEnIO.LoadFromFileICO(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileICO, FileName);
    Result := True;
    exit;
  end;
  

  fAborting := true; // So that fAborting is True if the file is not found/accessible
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamICO(fs);
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
  fParams.fFileName := FileName;
end;


procedure TImageEnIO.SyncLoadFromStreamICO(Stream: TStream);
var
  Progress: TProgressRec;
  tmpAlphaChannel: TIEMask;
begin
  tmpAlphaChannel := nil;
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    ICOReadStream(Stream, fIEBitmap, fParams, false, Progress, tmpAlphaChannel, false);
    CheckDPI;
    if assigned(tmpAlphaChannel) then
      fIEBitmap.AlphaChannel.CopyFromTIEMask(tmpAlphaChannel);
    if fAutoAdjustDPI then
      AdjustDPI;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    fParams.fFileName := '';
    fParams.fFileType := ioICO;
    Update;
  finally
    tmpAlphaChannel.Free;
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamICO

<FM>Declaration<FC>
function LoadFromStreamICO(Stream: TStream): Boolean;

<FM>Description<FN>      
Loads an image from a stream containing an icon file. The result will be false if an error is encountered, e.g. the file in the stream is not ICO format (<A TImageEnIO.Aborting> will be true).
!!}
function TImageEnIO.LoadFromStreamICO(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin     
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamICO, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamICO(Stream); 
    Result := Not fAborting;
  end;
end;



procedure TImageEnIO.SyncLoadFromStreamCUR(Stream: TStream);
var
  Progress: TProgressRec;
  tmpAlphaChannel: TIEMask;
begin
  tmpAlphaChannel := nil;
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    CURReadStream(Stream, fIEBitmap, fParams, false, Progress, tmpAlphaChannel, false);
    CheckDPI;
    if assigned(tmpAlphaChannel) then
      fIEBitmap.AlphaChannel.CopyFromTIEMask(tmpAlphaChannel);
    if fAutoAdjustDPI then
      AdjustDPI;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    fParams.fFileName := '';
    fParams.fFileType := ioCUR;
    Update;
  finally
    tmpAlphaChannel.Free;
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamCUR

<FM>Declaration<FC>
function LoadFromStreamCUR(Stream: TStream): Boolean;

<FM>Description<FN>   
Loads an image from a stream containing a cursor file. The result will be false if an error is encountered, e.g. the file in the stream is not CUR format (<A TImageEnIO.Aborting> will be true).

!!}
function TImageEnIO.LoadFromStreamCUR(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamCUR, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamCUR(Stream); 
    Result := Not fAborting;
  end;
end;



{!!
<FS>TImageEnIO.LoadFromFileCUR

<FM>Declaration<FC>
function LoadFromFileCUR(const FileName: WideString): Boolean;

<FM>Description<FN>              
Loads an image from a cursor file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not ICO format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFileCUR('C:\gates.cur');

!!}
function TImageEnIO.LoadFromFileCUR(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin        
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileCUR, FileName);
    Result := True;
    exit;
  end;

  
  fAborting := true; // So that fAborting is True if the file is not found/accessible
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamCUR(fs);
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
  fParams.fFileName := FileName;
end;

{!!
<FS>TImageEnIO.ImportMetafile

<FM>Declaration<FC>
function ImportMetafile(const FileName: WideString; Width, Height: Integer; WithAlpha: Boolean): Boolean;
function ImportMetafile(Stream: TStream; Width: Integer = -1; Height: Integer = -1; WithAlpha: boolean = True): Boolean;
function ImportMetafile(meta: TMetaFile; Width, Height: Integer; WithAlpha: Boolean): Boolean;

<FM>Description<FN>
Imports a WMF or EMF vectorial image. If the file does not represent a valid image format, ImportMetafile raises an <FC>EInvalidGraphic<FN> exception.

<FC>Width<FN> and <FC>Height<FN> specify the image size (the rectangle where the vectorial image will be painted).
To maintain the image aspect ratio set only one size, assigning -1 to the other, e.g. ImportMetafile('axi.emf', -1, 500);

If <FC>WithAlpha<FN> is true then ImportMetaFile creates an alpha channel, making only the metafile content visible.

Note: ImportMetafile converts the vectorial image to a raster image, so you must limit the rasterized image sizes. The maximum allowed size is stored in the public field named <A TIEImageEnGlobalSettings.MaxImageEMFSize>.

<FM>Example<FC>
// import vectorial image 'alfa.wmf', resizing to 500xHHH (HHH is autocalculated)
ImageEnView1.IO.ImportMetafile('C:\alfa.wmf', 500, -1, true);
!!}
// If Width or Height is -1 then it is auto-calculated maintain aspect ratio
// Import... because this is a vectorial image
// SHOULD NOT BE IN THREADS BECAUSE USE TCANVAS AND TMETAFILE!!
function TImageEnIO.ImportMetaFile(meta: TMetaFile; Width, Height: Integer; WithAlpha: Boolean): Boolean;
var
  dib: TIEDibBitmap;
  dibcanvas: TCanvas;
  x, y: integer;
  px: pbyte;
  p_rgb: PRGB;
  w, h: integer;
begin                
  Result := False;
  fAborting := false;
  if (meta.width = 0) or (meta.height = 0) then
  begin
    fAborting := true;
    exit;
  end;
  if (Width = 0) or (Height = 0) then
    exit;
  if not MakeConsistentBitmap([]) then
    exit;
  fParams.ResetInfo;

  if (Width > 0) or (Height > 0) then   // 3.0.4
  begin
    if (Width > 0) and (Height > 0) then  // 3.0.4
    begin
      meta.Width := Width;
      meta.Height := Height;
    end
    else
    begin
      if Width < 0 then
      begin
        meta.Width := (meta.Width * Height) div meta.Height;
        meta.Height := Height;
      end
      else
      if Height < 0 then
      begin
        meta.Height := (meta.Height * Width) div meta.Width;
        meta.Width := Width;
      end;
    end;
  end;
  if (meta.width > IEGlobalSettings().MaxImageEMFSize) or (meta.height > IEGlobalSettings().MaxImageEMFSize) then
  begin
    if meta.Height > meta.Width then
    begin
      meta.Width := (meta.Width * IEGlobalSettings().MaxImageEMFSize) div meta.Height;
      meta.Height := IEGlobalSettings().MaxImageEMFSize;
    end
    else
    begin
      meta.Height := (meta.Height * IEGlobalSettings().MaxImageEMFSize) div meta.Width;
      meta.Width := IEGlobalSettings().MaxImageEMFSize;
    end;
  end;

  dib := TIEDibBitmap.Create;
  dibcanvas := TCanvas.Create;
  dib.AllocateBits(meta.Width, meta.Height, 24);
  dibcanvas.Handle := dib.HDC;
  fParams.fFileName := '';
  if meta.Enhanced then
    fParams.fFileType := ioEMF
  else
    fParams.fFileType := ioWMF;
  fParams.BitsPerSample := 8;
  fParams.SamplesPerPixel := 3;
  fParams.fWidth := dib.Width;
  fParams.fHeight := dib.Height;
  fParams.fOriginalWidth := dib.Width;
  fParams.fOriginalHeight := dib.Height;
  fParams.DpiX := IEGlobalSettings().DefaultDPIX;
  fParams.DpiY := IEGlobalSettings().DefaultDPIY;
  fParams.FreeColorMap;
  SetViewerDPI(fParams.DpiX, fParams.DpiY);
  dibCanvas.Brush.Color := GetReBackground;
  dibcanvas.Brush.Style := bsSolid;
  dibCanvas.FillRect(rect(0, 0, dib.width, dib.height));
  dibCanvas.Draw(0, 0, meta);
  fIEBitmap.CopyFromTDibBitmap(dib);
  FreeAndNil(dibcanvas);
  FreeAndNil(dib);
  // alpha channel
  fIEBitmap.RemoveAlphaChannel;
  if WithAlpha then
  begin
    dib := TIEDibBitmap.Create;
    dibcanvas := TCanvas.Create;
    dib.AllocateBits(meta.Width, meta.Height, 24);
    dibcanvas.Handle := dib.HDC;
    dibCanvas.Brush.Color := $00010203; // transparent color (we hope it isn't in the image)
    dibcanvas.Brush.Style := bsSolid;
    dibCanvas.FillRect(rect(0, 0, dib.width, dib.height));
    dibcanvas.Draw(0, 0, meta);
    w := fIEBitmap.AlphaChannel.Width;
    h := fIEBitmap.AlphaChannel.Height;
    for y := 0 to h - 1 do
    begin
      px := fIEBitmap.AlphaChannel.Scanline[y];
      p_rgb := dib.Scanline[y];
      for x := 0 to w - 1 do
      begin
        with p_rgb^ do
          if (b = $01) and (g = $02) and (r = $03) then
            px^ := 0
          else
            px^ := 255;
        inc(p_rgb);
        inc(px);
      end;
    end;
    fIEBitmap.AlphaChannel.SyncFull;
    FreeAndNil(dibcanvas);
    FreeAndNil(dib);
  end;      
  Result := Not fAborting;
  Update;
end;

procedure TImageEnIO.ParamsFromMetaFile(stream: TStream);
var
  meta: TMetaFile;
begin
  meta := TMetaFile.Create;
  meta.LoadFromStream(stream);
  try
    fParams.fFileName := '';
    if meta.Enhanced then
      fParams.fFileType := ioEMF
    else
      fParams.fFileType := ioWMF;
    fParams.BitsPerSample := 8;
    fParams.SamplesPerPixel := 3;
    fParams.fWidth := meta.Width;
    fParams.fHeight := meta.Height;
    fParams.fOriginalWidth := meta.Width;
    fParams.fOriginalHeight := meta.Height;
    fParams.DpiX := IEGlobalSettings().DefaultDPIX;
    fParams.DpiY := IEGlobalSettings().DefaultDPIY;
    fParams.FreeColorMap;
  finally
    meta.Free;
  end;
end;

function TImageEnIO.ImportMetafile(const FileName: WideString; Width, Height: integer; WithAlpha: boolean): Boolean;
var
  meta: TMetafile;
begin                                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateImportMetaFileRetBool(self, ImportMetaFile, FileName, Width, Height, WithAlpha);
    Result := True;
    exit;
  end;

  
  try
    meta := TMetaFile.Create;
    try
      meta.LoadFromFile(FileName);
      Result := ImportMetaFile(meta, Width, Height, WithAlpha);
      fParams.fFileName := FileName;
    finally
      FreeAndNil(meta);
    end;
  finally
    DoFinishWork;
  end;
end;

function TImageEnIO.ImportMetafile(Stream: TStream; Width: Integer = -1; Height: Integer = -1; WithAlpha: boolean = True): Boolean;
var
  meta: TMetafile;
begin
  try
    meta := TMetaFile.Create;
    try
      meta.LoadFromStream(Stream);
      Result := ImportMetaFile(meta, Width, Height, WithAlpha);
      fParams.fFileName := '';
    finally
      FreeAndNil(meta);
    end;
  finally
    DoFinishWork;
  end;
end;


{!!
<FS>TImageEnIO.ParamsFromFileFormat

<FM>Declaration<FC>
procedure ParamsFromFileFormat(const FileName: WideString; format: <A TIOFileType>);

<FM>Description<FN>
Fills <A TImageEnIO.Params> by reading the image without actually loading the image into the component.

<FC>FileName<FN> is the file name with full path.
<FC>Format<FN> is the file format that the stream or file contains (you cannot specify <FC>ioUnknown<FN>).

<FM>Examples<FC>
ImageEnView1.IO.ParamsFromFileFormat('C:\alfa.jpg', ioJPEG);

ImageEnView1.IO.ParamsFromFileFormat('C:\alfa.dat', ioJPEG);
!!}
procedure TImageEnIO.ParamsFromFileFormat(const FileName: WideString; format: TIOFileType);
var
  idummy: integer;
  fpi: TIEFileFormatInfo;
  fs: TIEWideFileStream;
  nullpr: TProgressRec;
  tempAlphaChannel: TIEMask;
  BufStream: TIEBufferedReadStream;
begin
  fs := nil;
  BufStream := nil;
  try
    fParams.fFileType := format;
    fParams.ResetInfo;
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    with nullpr do
    begin
      Aborting := @fAborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    tempAlphaChannel := nil;
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    BufStream := TIEBufferedReadStream.Create(fs, 8192, IEGlobalSettings().UseRelativeStreams);
    fAborting := false;
    case fParams.fFileType of
      ioTIFF: TIFFReadStream(fIEBitmap, BufStream, idummy, fParams, nullpr, True, tempAlphaChannel, true, true, false, false);
      ioGIF: ReadGIFStream(BufStream, fIEBitmap, idummy, fParams, nullpr, True, tempAlphaChannel, true);
      ioJPEG: ReadJpegStream(BufStream, nil, fIEBitmap, fParams, nullpr, True, false, true, false, true, true, -1, fParams.IsNativePixelFormat);
      ioICO: ICOReadStream(BufStream, fIEBitmap, fParams, True, nullpr, tempAlphaChannel, true);
      ioCUR: CURReadStream(BufStream, fIEBitmap, fParams, True, nullpr, tempAlphaChannel, true);
      ioPCX: ReadPcxStream(BufStream, fIEBitmap, fParams, BufStream.size, nullpr, True);
      ioDCX: IEDCXReadStream(BufStream, fIEBitmap, fParams, nullpr, True);
      ioBMP: BMPReadStream(BufStream, fIEBitmap, BufStream.size, fParams, nullpr, True, false, tempAlphaChannel, true);
      {$IFDEF IEINCLUDEPNG}
      ioPNG: ReadPNGStream(BufStream, fIEBitmap, fParams, nullpr, true);
      {$ENDIF}
      {$ifdef IEINCLUDEDICOM}
      ioDICOM: IEDicomRead(BufStream, fParams, fIEBitmap, nullpr, true);
      {$endif}
      ioTGA: ReadTGAStream(BufStream, fIEBitmap, fParams, nullpr, true, tempAlphaChannel, true);
      ioPXM: PXMReadStream(BufStream, fIEBitmap, fParams, nullpr, true);
      ioWBMP: WBMPReadStream(BufStream, fIEBitmap, fParams, nullpr, true);
      {$IFDEF IEINCLUDEJPEG2000}
      ioJP2: J2KReadStream(BufStream, fIEBitmap, fParams, nullpr, true);
      ioJ2K: J2KReadStream(BufStream, fIEBitmap, fParams, nullpr, true);
      {$ENDIF}
      {$ifdef IEINCLUDERAWFORMATS}
      ioRAW: IEReadCameraRAWStream(BufStream, fIEBitmap, fParams, nullpr, true);
      {$endif}
      {$ifdef IEINCLUDEPSD}
      ioPSD: IEReadPSD(BufStream, nil, fParams, nullpr, false, nil);
      {$endif}
      {$ifdef IEINCLUDEWIC}
      ioHDP: IEHDPRead(BufStream, fIEBitmap, fParams, nullpr, true);
      {$endif}
      ioWMF, ioEMF: ParamsFromMetaFile(BufStream);
    else
      begin
        fpi := IEFileFormatGetInfo(fParams.fFileType);
        if assigned(fpi) then
          with fpi do
            if assigned(ReadFunction) then
            begin
              ReadFunction(BufStream, fIEBitmap, fParams, nullpr, true);
            end;
      end;
    end;
  except
    fParams.fFileType := ioUnknown;
  end;
  if fAborting then
    fParams.fFileType := ioUnknown;
  fParams.fFileName := FileName;
  BufStream.Free;
  fs.Free;
  DoFinishWork;
end;


{!!
<FS>TImageEnIO.ParamsFromFile

<FM>Declaration<FC>
procedure ParamsFromFile(const FileName: WideString);

<FM>Description<FN>
Reads the <L TImageEnIO.Params>image parameters</L> without actually loading the image (and without changing the current image).
                                                                                                                        
The image type is automatically determined from The file header and filename extension. If image format is unknown the <A TImageEnIO.Params>.<A TIOParamsVals.FileType> will be given an <FC>ioUnknown<FN> value.

<FM>See Also<FN>
- <A TImageEnIO.ParamsFromStreamFormat>
- <A TImageEnIO.ParamsFromFileFormat>

<FM>Example<FC>
// Display the bits per sample of an image without loading it
ImageEnView1.IO.ParamsFromFile('C:\alfa.bmp');
Label1.Caption := 'alfa.bmp has ' + inttostr(ImageEnView1.IO.Params.BitsPerSample) + ' bits per sample';
!!}
// Fills Params reading the specified file, but the contained image
procedure TImageEnIO.ParamsFromFile(const FileName: WideString);
begin
  try
    ParamsFromFileFormat(FileName, FindFileFormat(FileName, true));
  except
    fParams.fFileType := ioUnknown;
  end;
end;

{!!
<FS>TImageEnIO.ParamsFromStreamFormat

<FM>Declaration<FC>
procedure ParamsFromStreamFormat(Stream: TStream; format: <A TIOFileType>);

<FM>Description<FN>
Fills <A TImageEnIO.Params> when reading an image from Stream, but don't load the image into the component.

<FC>Stream<FN> is a TStream that contains the image.
<FC>Format<FN> is the file format that the stream or file contains (you cannot specify <FC>ioUnknown<FN>).
!!}
// Fills Params reading Stream, but doesn't load the image
// format specifies the file format
// The Stream position changes
procedure TImageEnIO.ParamsFromStreamFormat(Stream: TStream; format: TIOFileType);
var
  idummy: integer;
  fpi: TIEFileFormatInfo;
  nullpr: TProgressRec;
  tempAlphaChannel: TIEMask;
  BufStream: TIEBufferedReadStream;
begin
  BufStream := TIEBufferedReadStream.Create(Stream, 8192, IEGlobalSettings().UseRelativeStreams);
  try
    fParams.fFileType := format;
    fParams.ResetInfo;
    fAborting := false;
    with nullpr do
    begin
      Aborting := @fAborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    tempAlphaChannel := nil;
    case fParams.fFileType of
      ioTIFF: TIFFReadStream(fIEBitmap, BufStream, idummy, fParams, nullpr, True, tempAlphaChannel, true, true, false, false);
      ioGIF: ReadGIFStream(BufStream, fIEBitmap, idummy, fParams, nullpr, True, tempAlphaChannel, true);
      ioJPEG: ReadJpegStream(BufStream, nil, fIEBitmap, fParams, nullpr, True, false, true, false, true, true, -1, fParams.IsNativePixelFormat);
      ioICO: ICOReadStream(BufStream, fIEBitmap, fParams, True, nullpr, tempAlphaChannel, true);
      ioCUR: CURReadStream(BufStream, fIEBitmap, fParams, True, nullpr, tempAlphaChannel, true);
      ioPCX: ReadPcxStream(BufStream, fIEBitmap, fParams, BufStream.size, nullpr, True);
      ioDCX: IEDCXReadStream(BufStream, fIEBitmap, fParams, nullpr, True);
      ioBMP: BMPReadStream(BufStream, fIEBitmap, BufStream.size, fParams, nullpr, True, false, tempAlphaChannel, true);
      {$IFDEF IEINCLUDEPNG}
      ioPNG: ReadPNGStream(BufStream, fIEBitmap, fParams, nullpr, true);
      {$ENDIF}
      {$ifdef IEINCLUDEDICOM}
      ioDICOM: IEDicomRead(BufStream, fParams, fIEBitmap, nullpr, true);
      {$endif}
      ioTGA: ReadTGAStream(BufStream, fIEBitmap, fParams, nullpr, True, tempAlphaChannel, true);
      ioPXM: PXMReadStream(BufStream, fIEBitmap, fParams, nullpr, True);
      ioWBMP: WBMPReadStream(BufStream, fIEBitmap, fParams, nullpr, True);
      {$IFDEF IEINCLUDEJPEG2000}
      ioJP2: J2KReadStream(BufStream, fIEBitmap, fParams, nullpr, True);
      ioJ2K: J2KReadStream(BufStream, fIEBitmap, fParams, nullpr, True);
      {$ENDIF}
      {$ifdef IEINCLUDERAWFORMATS}
      ioRAW: IEReadCameraRAWStream(BufStream, fIEBitmap, fParams, nullpr, true);
      {$endif}
      {$ifdef IEINCLUDEPSD}
      ioPSD: IEReadPSD(BufStream, nil, fParams, nullpr, false, nil);
      {$endif}
      {$ifdef IEINCLUDEWIC}
      ioHDP: IEHDPRead(BufStream, nil, fParams, nullpr, false);
      {$endif}
      ioWMF, ioEMF: ParamsFromMetaFile(BufStream);
    else
      begin
        fpi := IEFileFormatGetInfo(fParams.fFileType);
        if assigned(fpi) then
          with fpi do
            if assigned(ReadFunction) then
              ReadFunction(BufStream, fIEBitmap, fParams, nullpr, true);
      end;
    end;
  except
    fParams.fFileType := ioUnknown;
  end;
  BufStream.Free;
  if fAborting then
    fParams.fFileType := ioUnknown;
  DoFinishWork;
end;

{!!
<FS>TImageEnIO.ParamsFromStream

<FM>Declaration<FC>
procedure ParamsFromStream(Stream: TStream);

<FM>Description<FN>
Reads the <L TImageEnIO.Params>image properties</L> without loading the image (and without changing the current image).

The image type is automatically determined from The file header and filename extension. If image format is unknown the <A TImageEnIO.Params>.<A TIOParamsVals.FileType> has ioUnknown value.
!!}
procedure TImageEnIO.ParamsFromStream(Stream: TStream);
begin
  try
    ParamsFromStreamFormat(Stream, FindStreamFormat(Stream));
  except
    fParams.fFileType := ioUnknown;
  end;
end;


/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
// Registered File formats support
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>IEFileFormatGetInfo

<FM>Declaration<FC>
function IEFileFormatGetInfo(FileType: <A TIOFileType>): <A TIEFileFormatInfo>;

<FM>Description<FN>
Returns a TIEFileFormatInfo object which contains details about the specified file format.

See also: <A IEFileFormatGetInfo2>

<FM>Example<FC>
// Get a suitable file extension for saving a JPEG
sExt := IEFileFormatGetInfo(ioJPEG).SuitableExtension;  // returns 'jpeg'

// Get a description of JPEG files
sFormatName := IEFileFormatGetInfo(ioJPEG).FullName;  // returns 'JPEG Bitmap'

!!}
// ret nil if FileType doesn't exists
function IEFileFormatGetInfo(FileType: TIOFileType): TIEFileFormatInfo;
var
  q: integer;
begin
  result := nil;
  if FileType = ioUnknown then
    exit;
  
  for q := 0 to IEGlobalSettings().FileFormats.Count - 1 do
  begin
    result := TIEFileFormatInfo(IEGlobalSettings().FileFormats[q]);
    if result.FileType = FileType then
      exit;
  end;
end;

// ret extension count
function IEFileFormatGetExtCount(FileType: TIOFileType): integer;
begin
  result := 0;
  while IEFileFormatGetExt(FileType, result) <> '' do
    inc(result);
end;

// ret extension
// example: for Extensions='jpg;jpeg' idx=1 is 'jpeg'
function IEFileFormatGetExt(FileType: TIOFileType; idx: integer): string;
var
  fi: TIEFileFormatInfo;
  ss: string;
  //
  function ExtractNext: string;
  var
    q: integer;
  begin
    q := Pos(';', ss);
    if q = 0 then
    begin
      result := ss;
      ss := '';
    end
    else
    begin
      result := Copy(ss, 1, q - 1);
      ss := Copy(ss, q + 1, length(ss) - q);
    end;
  end;
  //
var
  i: integer;
begin
  fi := IEFileFormatGetInfo(FileType);
  if assigned(fi) then
  begin
    ss := fi.Extensions;
    i := 0;
    while length(ss) > 0 do
    begin
      result := ExtractNext;
      if i = idx then
        exit;
      inc(i);
    end;
  end;
  result := '';
end;

{!!
<FS>IEFileFormatGetInfo2

<FM>Declaration<FC>
function IEFileFormatGetInfo2(Extension: string): <A TIEFileFormatInfo>;

<FM>Description<FN>
Returns a TIEFileFormatInfo object which contains details about the specified file format. The format is specified by its extension (example '.gif' or 'gif').

See also: <A IEFileFormatGetInfo>

<FM>Example<FC>
// Get a description of .JPEG files
sFormatName := IEFileFormatGetInfo2('.JPEG').FullName;  // returns 'JPEG Bitmap'
!!}
// ret nil if Extension doesn't exists (accept '.xxx' or 'xxx')
function IEFileFormatGetInfo2(Extension: string): TIEFileFormatInfo;
var
  q, i, c: integer;
begin
  Extension := LowerCase(Extension);
  if (Length(Extension) > 0) and (Extension[1] = '.') then
    Delete(Extension, 1, 1);

  for q := 0 to IEGlobalSettings().FileFormats.Count - 1 do
  begin
    result := TIEFileFormatInfo(IEGlobalSettings().FileFormats[q]);
    c := IEFileFormatGetExtCount(result.FileType);
    for i := 0 to c - 1 do
      if LowerCase(IEFileFormatGetExt(result.FileType, i)) = Extension then
        exit;
  end;
  result := nil;
end;


{!!
<FS>GetAllSupportedFileExtensions

<FM>Declaration<FC>
function GetAllSupportedFileExtensions(bLoadFormats, bSaveFormats : Boolean; bVideoFormats: Boolean = True) : string

<FM>Description<FN>
Returns the extensions of all file formats that are supported by ImageEn in the format "*.jpeg;*.jpg;*.gif;...'

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>bLoadFormats<FN></C> <C>Include formats that ImageEn can load</C> </R>
<R> <C><FC>bSaveFormats<FN></C> <C>Include formats that ImageEn can save</C> </R>
<R> <C><FC>bVideoFormats<FN></C> <C>Include video formats that ImageEn supports</C> </R>
</TABLE>

<FM>Example<FC>
// Create a file filter for JPEG, GIF and all supported files types
FilterComboBox1.Filter := 'JPEG Files|' + GetFileExtensionsOfType(ioJPEG) + '|' +
                          'GIF Files|' + GetFileExtensionsOfType(ioGIF) + '|' +
                          'All Images|' + GetAllSupportedFileExtensions(True, False, False)  + '|' +
                          'All Files|*.*';

!!}
function GetAllSupportedFileExtensions(bLoadFormats, bSaveFormats : Boolean; bVideoFormats: Boolean = True) : string;
var
  q: integer;
begin
  result := '';

  for q := 0 to IEGlobalSettings().FileFormats.Count - 1 do
    with TIEFileFormatInfo(IEGlobalSettings().FileFormats[q]) do
    begin
      if (Extensions <> '') and ((bSaveFormats and (@WriteFunction <> nil)) or (bLoadFormats and (@ReadFunction <> nil))) then
        Result := Result + GetFileExtensionsOfType(FileType) + ';';
    end;

  // Video files
  if bVideoFormats then
  begin
    {$ifdef IEINCLUDEDIRECTSHOW}
    if bLoadFormats then
      result := result + Supported_Video_File_Extensions
    else
    {$endif}
      result := result + Supported_AVI_File_Extensions;
  end;

  // Remove trailing semicolon
  if Result <> '' then
    SetLength(Result, Length(Result) - 1);
end;


{!!
<FS>GetFileExtensionsOfType

<FM>Declaration<FC>
function GetFileExtensionsOfType(FileType: <A TIOFileType>) : string;

<FM>Description<FN>
Returns the extensions of an ImageEn file type in the format "*.jpeg;*.jpg'

<FM>Example<FC>
// Create a file filter for JPEG, GIF and all supported files types
FilterComboBox1.Filter := 'JPEG Files|' + GetFileExtensionsOfType(ioJPEG) + '|' +
                          'GIF Files|' + GetFileExtensionsOfType(ioGIF) + '|' +
                          'All Images|' + GetAllSupportedFileExtensions(True, False, False)  + '|' +
                          'All Files|*.*';
!!}
function GetFileExtensionsOfType(FileType: TIOFileType) : string;
var
  i, cc: integer;
begin
  result := '';

  cc := IEFileFormatGetExtCount(FileType);
  for i := 0 to cc - 1 do
    Result := Result + '*.' + LowerCase(IEFileFormatGetExt(FileType, i)) + ';';

  // Remove trailing semicolon
  if Result <> '' then
    SetLength(Result, Length(Result) - 1);
end;


{!!
<FS>IEFileFormatAdd

<FM>Declaration<FC>
procedure IEFileFormatAdd(FileFormatInfo: <A TIEFileFormatInfo>);

<FM>Description<FN>
Adds a new file format to ImageEn. The object <FC>FileFormatInfo<FN> specifies file format information and read/write functions.

Note: You do not need to free the <FC>FileFormatInfo<FN> object.

<FM>Example<FC>
var
  FileFormatInfo: TIEFileFormatInfo;
begin
  FileFormatInfo := TIEFileFormatInfo.Create;
  with FileFormatInfo do
  begin
    FileType := ioUNC;
    FullName := 'Uncompressed Bitmap';
    Extensions := 'unc;ucp';
    SuitableExtension := 'unc';  // Optional. It is automatically set if not specified
    InternalFormat := False;
    DialogPage := [];
    ReadFunction := ReadUNC;
    WriteFunction := WriteUNC;
    TryFunction := TryUNC;
  end;
  IEFileFormatADD(FileFormatInfo);
end;
!!}
procedure IEFileFormatAdd(FileFormatInfo: TIEFileFormatInfo);
var
  sSuitableExtension: string;
  iPos: Integer;
begin
  if FileFormatInfo.SuitableExtension = '' then
  begin
    sSuitableExtension := FileFormatInfo.Extensions;
    iPos := Pos(';', sSuitableExtension);
    if iPos > 0 then
      delete(sSuitableExtension, iPos, Length(sSuitableExtension) - iPos + 1);
    FileFormatInfo.SuitableExtension := sSuitableExtension;
  end;
  
  IEGlobalSettings().FileFormats.Add(FileFormatInfo);
end;

{!!
<FS>IEFileFormatRemove

<FM>Declaration<FC>
procedure IEFileFormatRemove(FileType: <A TIOFileType>);

<FM>Description<FN>
Removes a file format added using <A IEFileFormatAdd>.
!!}
procedure IEFileFormatRemove(FileType: TIOFileType);
var
  r: TIEFileFormatInfo;
begin
  r := IEFileFormatGetInfo(FileType);
  if assigned(r) then
  begin
    IEGlobalSettings().FileFormats.Remove(r);
    r.Free;
  end;
end;

procedure DumpReadImageStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
begin
end;

procedure DumpWriteImageStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
begin
end;

function DumpTryImageStream(Stream: TStream; TryingFormat: TIOFileType): boolean;
begin
  result := false;
end;

// Alloc TIEImageEnGlobalSettings.FileFormats global variable and embedded file formats
procedure IEInitFileFormats;
var
  fi: TIEFileFormatInfo;
  L3118: TImageEnIO;
begin
  // So that the linker includes all obj files, avoiding the Delphi error, "Internal Error L3118"
  L3118 := TImageEnIO.Create(nil);
  FreeAndNil(L3118);

  IEGlobalSettings().FileFormats := TList.Create;

  // TIFF
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioTIFF;
    FullName := 'TIFF Bitmap';
    Extensions := 'TIF;TIFF;FAX;G3N;G3F;XIF';
    SuitableExtension := 'tiff';
    DialogPage := [ppTIFF];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // GIF
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioGIF;
    FullName := 'CompuServe Bitmap';
    Extensions := 'GIF';             
    SuitableExtension := 'gif';
    DialogPage := [ppGIF];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // JPEG
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioJPEG;
    FullName := 'JPEG Bitmap';
    Extensions := 'JPG;JPEG;JPE;JIF';  
    SuitableExtension := 'jpeg';
    DialogPage := [ppJPEG];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // PCX
  with fi do
  begin
    fi := TIEFileFormatInfo.Create;
    FileType := ioPCX;
    FullName := 'PaintBrush';
    Extensions := 'PCX';         
    SuitableExtension := 'pcx';
    DialogPage := [ppPCX];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // BMP
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioBMP;
    FullName := 'Windows Bitmap';
    Extensions := 'BMP;DIB;RLE';
    SuitableExtension := 'bmp';
    DialogPage := [ppBMP];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // BMPRAW
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioBMPRAW;
    FullName := 'Raw Bitmap';
    Extensions := ''; // in this way this format will not be included in open/save dialog    
    SuitableExtension := '';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // ICO
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioICO;
    FullName := 'Windows Icon';
    Extensions := 'ICO';      
    SuitableExtension := 'ico';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // CUR
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioCUR;
    FullName := 'Windows Cursor';
    Extensions := 'CUR';       
    SuitableExtension := 'cur';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := nil; // cannot write
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;


  {$IFDEF IEINCLUDEPNG}
  // PNG
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioPNG;
    FullName := 'Portable Network Graphics';
    Extensions := 'PNG';        
    SuitableExtension := 'png';
    DialogPage := [ppPNG];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;
  {$ENDIF}

  {$ifdef IEINCLUDEDICOM}
  // DICOM
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioDICOM;
    FullName := 'DICOM Bitmap';
    Extensions := 'DCM;DIC;DICOM;V2';  
    SuitableExtension := 'dicom';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;
  {$endif}

  // WMF
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioWMF;
    FullName := 'Windows Metafile';
    Extensions := 'WMF';             
    SuitableExtension := 'wmf';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := nil; // cannot write
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // EMF
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioEMF;
    FullName := 'Enhanced Windows Metafile';
    Extensions := 'EMF';              
    SuitableExtension := 'emf';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := nil; // cannot write
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // TGA
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioTGA;
    FullName := 'Targa Bitmap';
    Extensions := 'TGA;TARGA;VDA;ICB;VST;PIX';      
    SuitableExtension := 'targa';
    DialogPage := [ppTGA];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // PXM
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioPXM;
    FullName := 'Portable Pixmap, GrayMap, BitMap';
    Extensions := 'PXM;PPM;PGM;PBM';      
    SuitableExtension := 'pxm';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // WBMP
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioWBMP;
    FullName := 'Wireless Bitmap';
    Extensions := 'WBMP';            
    SuitableExtension := 'wbmp';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  {$IFDEF IEINCLUDEJPEG2000}
  // JP2
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioJP2;
    FullName := 'JPEG2000';
    Extensions := 'JP2';      
    SuitableExtension := 'jp2';
    DialogPage := [ppJ2000];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;
  // J2K
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioJ2K;
    FullName := 'JPEG2000 Code Stream';
    Extensions := 'J2K;JPC;J2C';       
    SuitableExtension := 'j2k';
    DialogPage := [ppJ2000];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;
  {$ENDIF}

  // PostScript (PS)
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioPS;
    FullName := 'PostScript Level 2';
    Extensions := 'PS;EPS';      
    SuitableExtension := 'eps';
    DialogPage := [];
    ReadFunction := nil;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // Adobe PDF
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioPDF;
    FullName := 'Adobe PDF';
    Extensions := 'PDF';   
    SuitableExtension := 'pdf';
    DialogPage := [];
    ReadFunction := nil;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // DCX
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioDCX;
    FullName := 'Multipage PCX';
    Extensions := 'DCX';   
    SuitableExtension := 'dcx';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // RAW
  {$ifdef IEINCLUDERAWFORMATS}
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioRAW;
    FullName := 'Camera RAW';
    Extensions := 'CRW;CR2;NEF;RAW;PEF;RAF;X3F;BAY;ORF;SRF;MRW;DCR;SR2';   
    SuitableExtension := 'raw';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := nil;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;
  {$endif}

  // PSD
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioPSD;
    FullName := 'Photoshop PSD';
    Extensions := 'PSD;PSB';            
    SuitableExtension := 'psd';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // IEV
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioIEV;
    FullName := 'Vectorial objects';
    Extensions := 'IEV';         
    SuitableExtension := 'iev';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // LYR
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioLYR;
    FullName := 'Layers';
    Extensions := 'LYR';     
    SuitableExtension := 'lyr';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // ALL
  fi := TIEFileFormatInfo.Create;
  with fi do
  begin
    FileType := ioALL;
    FullName := 'Layers and objects';
    Extensions := 'ALL';   
    SuitableExtension := 'all';
    DialogPage := [];
    ReadFunction := DumpReadImageStream;
    WriteFunction := DumpWriteImageStream;
    TryFunction := DumpTryimageStream;
    InternalFormat := True;
    IEFileFormatAdd(fi);
  end;

  // HDP
  {$ifdef IEINCLUDEWIC}
  if IEWICAvailable() then
  begin
    fi := TIEFileFormatInfo.Create;
    with fi do
    begin
      FileType := ioHDP;
      FullName := 'Microsoft HD Photo';
      Extensions := 'WDP;HDP;JXR';   
      SuitableExtension := 'hdp';
      DialogPage := [];
      ReadFunction := DumpReadImageStream;
      WriteFunction := DumpWriteImageStream;
      TryFunction := DumpTryimageStream;
      InternalFormat := true;
      IEFileFormatAdd(fi);
    end;
  end;
  {$endif}

end;

// update GIF WriteFunction and ReadFunction regarding to DefGIF_LZWCOMPFUNC and DefGIF_LZWDECOMPFUNC
procedure IEUpdateGIFStatus;
var
  fi: TIEFileFormatInfo;
begin
  fi := IEFileFormatGetInfo(ioGIF);
  if assigned(fi) then
  begin
    if assigned(IEGlobalSettings().DefGIF_LZWDECOMPFUNC) then
      fi.ReadFunction := DumpReadImageStream
    else
      fi.ReadFunction := nil;
    if assigned(IEGlobalSettings().DefGIF_LZWCOMPFUNC) then
      fi.WriteFunction := DumpWriteImageStream
    else
      fi.WriteFunction := nil;
  end;
end;

// Free TIEImageEnGlobalSettings.FileFormats global variable
procedure IEFreeFileFormats;
var
  q: integer;
begin
  for q := 0 to IEGlobalSettings().FileFormats.Count - 1 do
    TIEFileFormatInfo(IEGlobalSettings().FileFormats[q]).Free;
  IEGlobalSettings().FileFormats.Free();
  IEGlobalSettings().FileFormats := nil;
end;

/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnIO.LoadFromFilePXM

<FM>Declaration<FC>
function LoadFromFilePXM(const FileName: WideString): Boolean;

<FM>Description<FN>
Loads an image from a PBM, PGM or PPM file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not a recognized file type (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.
!!}
function TImageEnIO.LoadFromFilePXM(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
  Progress: TProgressRec;
begin           
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFilePXM, FileName);
    Result := True;
    exit;
  end;

  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      fIEBitmap.RemoveAlphaChannel;
      PXMReadStream(fs, fIEBitmap, fParams, Progress, false);
      CheckDPI;
      if fAutoAdjustDPI then
        AdjustDPI;
      fParams.fFileName := FileName;
      fParams.fFileType := ioPXM;
      update;
    finally
      FreeAndNil(fs);  
      Result := Not fAborting;
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamPXM

<FM>Declaration<FC>
function LoadFromStreamPXM(Stream: TStream): Boolean;

<FM>Description<FN>    
Loads an image from a stream containing a PBM, PGM or PPM file. The result will be false if an error is encountered, e.g. the file in the stream is not a recognized format (<A TImageEnIO.Aborting> will be true).
!!}
function TImageEnIO.LoadFromStreamPXM(Stream: TStream): Boolean;
var
  Progress: TProgressRec;
begin              
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamPXM, Stream);
    Result := True;
  end;

  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    PXMReadStream(Stream, fIEBitmap, fParams, Progress, false);
    CheckDPI;
    if fAutoAdjustDPI then
      AdjustDPI;
    fParams.fFileName := '';
    fParams.fFileType := ioPXM;   
    Result := Not fAborting;
    update;
    Result := Not fAborting;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToFilePXM

<FM>Declaration<FC>
procedure SaveToFilePXM(const FileName: WideString);

<FM>Description<FN>     
Saves the current image to a file in PBM, PGM or PPM format.

<FC>FileName<FN> is the file name including extension.

If the PBM (Portable Bitmap) contains only 1 bpp images (black/white), then the values must be Params.BitsPerPixel = 1 and Params.SamplesPerPixel = 1.
If the PGM (Portable Graymap) contains only 8 bpp images (gray scale), then the values must be Params.BitsPerPixel = 8 and Params.SamplesPerPixel = 1.
If the PPM (Portable Pixmap) contains only 24 bpp images (true color), then the values must be Params.BitsPerPixel = 8 and Params.SamplesPerPixel = 3.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.
!!}
procedure TImageEnIO.SaveToFilePXM(const FileName: WideString);
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
begin                                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFilePXM, FileName);
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) and (fIEBitmap.PixelFormat<>ie48RGB) then
      fIEBitmap.PixelFormat := ie24RGB;
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      PXMWriteStream(fs, fIEBitmap, fParams, Progress);
      fParams.FileName := FileName;
      fParams.FileType := ioPXM;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamPXM

<FM>Declaration<FC>
procedure SaveToStreamPXM(Stream: TStream);

<FM>Description<FN>                                        
Saves the current image to a stream in PBM, PGM or PPM format.

If the PBM (Portable Bitmap) contains only 1 bpp images (black/white), then the values must be Params.BitsPerPixel=1 and Params.SamplesPerPixel=1.
If the PGM (Portable Graymap) contains only 8 bpp images (gray scale), then the values must be Params.BitsPerPixel=8 and Params.SamplesPerPixel=1.
If the PPM (Portable Pixmap) contains only 24 bpp images (true color), then the values must be Params.BitsPerPixel=8 and Params.SamplesPerPixel=3.

!!}
procedure TImageEnIO.SaveToStreamPXM(Stream: TStream);
var
  Progress: TProgressRec;
begin                                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamPXM, Stream);
    exit;
  end;

  
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    PXMWriteStream(Stream, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;

procedure PrintImagePos_func1(x, y: dword; pixel: pointer; UserData: pointer);
begin
  with PRGB(pixel)^ do
  begin
    r := pbytearray(UserData)^[r];
    g := pbytearray(UserData)^[g];
    b := pbytearray(UserData)^[b];
  end;
end;

// dpix, dpiy cannot be 0
procedure TImageEnIO.PrintImagePosEx(PrtCanvas: TCanvas; dpix, dpiy: integer; x, y: double; Width, Height: double; GammaCorrection: double);
var
  px, py, pWidth, pHeight: integer;
  zz: double;
begin
  IEPrintLogWrite('TImageEnIO.PrintImagePosEx: begin');
  if not MakeConsistentBitmap([]) then
    exit;
  //
  px := trunc(x * dpix);
  py := trunc(y * dpiy);
  pWidth := trunc(Width * dpix);
  pHeight := trunc(Height * dpiy);
  zz := pWidth / fIEBitmap.Width;
  //
  if (fPrintingFilterOnSubsampling<>rfNone) and (zz < 1) then
  begin
    IEPrintLogWrite('TImageEnIO.PrintImagePosEx: calling RenderToCanvas with subsampling filter');
    fIEBitmap.RenderToCanvas(PrtCanvas, px, py, pWidth, pHeight, fPrintingFilterOnSubsampling, GammaCorrection);
  end
  else
  begin
    IEPrintLogWrite('TImageEnIO.PrintImagePosEx: calling RenderToCanvas without subsampling filter');
    fIEBitmap.RenderToCanvas(PrtCanvas, px, py, pWidth, pHeight, rfNone, GammaCorrection);
  end;
  IEPrintLogWrite('TImageEnIO.PrintImagePosEx: end');
end;



{!!
<FS>TImageEnIO.PrintImagePos

<FM>Declaration<FC>
procedure PrintImagePos(PrtCanvas: TCanvas; x, y: double; Width, Height: double; GammaCorrection: double = 1);

<FM>Description<FN>
Prints the current image at a specified absolute position and size.

<FC>PrtCanvas<FN> is the printing canvas. The application can set this parameter from Printer.Canvas.
<FC>x, y<FN> is the top-left starting point in inches.
<FC>Width, Height<FN> is the image size in inches.
<FC>GammaCorrection<FN> is the gamma correction value. Use a value of 1.0 to disable gamma correction.

<FM>Example<FC>
// print the image two inches from the top and left of the page, and at a width and height of 10 inches (the image will be stretched)
Printer.BeginDoc;
ImageEnView1.IO.PrintImagePos(Printer.Canvas, 2, 2, 10, 10);
Printer.EndDoc;
!!}
// if width = 0 and/or height = 0, PrintImagePos autocalculates for normal size
procedure TImageEnIO.PrintImagePos(PrtCanvas: TCanvas; x, y: double; Width, Height: double; GammaCorrection: double);
var
  dpix, dpiy: integer;
begin
  dpix := GetDeviceCaps(PrtCanvas.Handle, LOGPIXELSX);
  dpiy := GetDeviceCaps(PrtCanvas.Handle, LOGPIXELSY);
  PrintImagePosEx(PrtCanvas, dpix, dpiy, x, y, Width, Height, GammaCorrection);
end;

// dpix, dpiy are dpi of destination device, cannot be 0
procedure TImageEnIO.PrintImageEx(PrtCanvas: TCanvas; dpix, dpiy: integer; pagewidth, pageheight: double; MarginLeft, MarginTop, MarginRight, MarginBottom: double; VerticalPos: TIEVerticalPos; HorizontalPos: TIEHorizontalPos; Size: TIESize; SpecWidth, SpecHeight: double; GammaCorrection: double);
var
  x, y, width, height: double;
  z: double;
  bitmapwidth, bitmapheight: double; // in inches
  imgdpix, imgdpiy: integer;
begin
  IEPrintLogWrite('TImageEnIO.PrintImageEx: begin');
  if not MakeConsistentBitmap([]) then
    exit;
  if assigned(fImageEnView) then
  begin
    imgdpix := fImageEnView.DpiX;
    imgdpiy := fImageEnView.DpiY;
  end
  else
  begin
    imgdpix := Params.DpiX;
    imgdpiy := Params.DpiY;
  end;
  if assigned(fTImage) then
  begin
    if (fTImage.Picture.Bitmap.PixelFormat <> pf1bit) and (fTImage.Picture.Bitmap.PixelFormat <> pf24bit) then
      fTImage.Picture.Bitmap.PixelFormat := pf24bit;
  end;

  if imgdpix <= 1 then
    imgdpix := dpix;
  if imgdpiy <= 1 then
    imgdpiy := dpiy;

  IEPrintLogWrite('TImageEnIO.PrintImageEx: setting page sizes');
  pagewidth := pagewidth - (MarginLeft + MarginRight);
  pageheight := pageheight - (MarginTop + MarginBottom);
  width := 0;
  height := 0;
  x := 0;
  y := 0;
  case Size of
    iesNORMAL:
      begin
        width := fIEBitmap.Width / imgdpix;
        height := fIEBitmap.Height / imgdpiy;
      end;
    iesFITTOPAGE:
      begin
        bitmapwidth := fIEBitmap.Width / dpix;
        bitmapheight := fIEBitmap.Height / dpiy;
        z := dmin(pagewidth / bitmapwidth, pageheight / bitmapheight);
        width := bitmapwidth * z;
        height := bitmapheight * z;
      end;
    iesFITTOPAGESTRETCH:
      begin
        width := pagewidth;
        height := pageheight;
      end; 
    iesFILLPAGE:
      begin
        bitmapwidth := fIEBitmap.Width / dpix;
        bitmapheight := fIEBitmap.Height / dpiy;
        z := dmax(pagewidth / bitmapwidth, pageheight / bitmapheight);
        width := bitmapwidth * z;
        height := bitmapheight * z;
      end;
    iesSPECIFIEDSIZE:
      begin
        width := SpecWidth;
        height := SpecHeight;
      end;
  end;
  case HorizontalPos of
    iehpLEFT:
      x := MarginLeft;
    iehpCENTER:
      x := MarginLeft + (pagewidth - width) / 2;
    iehpRIGHT:
      x := MarginLeft + pagewidth - width;
  end;
  case VerticalPos of
    ievpTOP:
      y := MarginTop;
    ievpCENTER:
      y := MarginTop + (pageheight - height) / 2;
    ievpBOTTOM:
      y := MarginTop + pageheight - height;
  end;
  PrintImagePosEx(PrtCanvas, dpix, dpiy, x, y, width, height, GammaCorrection);
  IEPrintLogWrite('TImageEnIO.PrintImageEx: end');
end;

{!!
<FS>TImageEnIO.PrintImage

<FM>Declaration<FC>
procedure PrintImage(PrtCanvas: TCanvas = nil; MarginLeft: double = 1; MarginTop: double = 1; MarginRight: double = 1; MarginBottom: double = 1; VerticalPos: <A TIEVerticalPos> = ievpCENTER; HorizontalPos: <A TIEHorizontalPos> = iehpCENTER; Size: <A TIESize> = iesFITTOPAGE; SpecWidth: double = 0; SpecHeight: double = 0; GammaCorrection: double = 1);

<FM>Description<FN>
Print the current image by specifying margins, vertical position, horizontal position and size.

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
</TABLE>


<FM>Example<FC>
// Print the image in the center of the page at the original size
Printer.BeginDoc;
ImageEnView1.IO.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesNORMAL, 0, 0, 1);
Printer.EndDoc;

// Print the image in the center of the page stretched to page dimensions (respecting the proportions)
Printer.BeginDoc;
ImageEnView1.IO.PrintImage(Printer.Canvas, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesFITTOPAGE, 0, 0, 1);
Printer.EndDoc;

// Print all pages of a TIFF
Printer.BeginDoc;
for I := 0 to ImageEnView1.IO.Params.ImageCount - 1 do
begin
  ImageEnView1.IO.Params.ImageIndex := I;
  ImageEnView1.IO.LoadFromFile('C:\input.tif');
  ImageEnView1.IO.PrintImage(Printer.Canvas);
end;
Printer.EndDoc;
!!}
procedure TImageEnIO.PrintImage(PrtCanvas: TCanvas; MarginLeft, MarginTop, MarginRight, MarginBottom: double; VerticalPos: TIEVerticalPos; HorizontalPos: TIEHorizontalPos; Size: TIESize; SpecWidth, SpecHeight: double; GammaCorrection: double);
var
  dpix, dpiy: integer;
  pagewidth, pageheight: double;
begin
  IEPrintLogWrite('TImageEnIO.PrintImage: begin');
  if PrtCanvas = nil then
    PrtCanvas := Printer.Canvas;
  dpix := GetDeviceCaps(PrtCanvas.Handle, LOGPIXELSX);
  dpiy := GetDeviceCaps(PrtCanvas.Handle, LOGPIXELSY);
  pagewidth := GetDeviceCaps(PrtCanvas.Handle, 8) / dpix; // HORZRES
  pageheight := GetDeviceCaps(PrtCanvas.Handle, 10) / dpiy; // VERTRES
  PrintImageEx(PrtCanvas, dpix, dpiy, pagewidth, pageheight, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, Size, SpecWidth, SpecHeight, GammaCorrection);
  IEPrintLogWrite('TImageEnIO.PrintImage: end');
end;



{!!
<FS>TImageEnIO.PreviewPrintImage

<FM>Declaration<FC>
procedure PreviewPrintImage(DestBitmap: TBitmap; MaxBitmapWidth: Integer; MaxBitmapHeight: Integer; PrinterObj: TPrinter = nil; MarginLeft: Double = 1; MarginTop: Double = 1; MarginRight: Double = 1; MarginBottom: Double = 1; VerticalPos: <A TIEVerticalPos> = ievpCENTER; HorizontalPos: <A TIEHorizontalPos> = iehpCENTER; Size: <A TIESize> = iesFITTOPAGE; SpecWidth: Double = 0; SpecHeight: Double = 0; GammaCorrection: Double = 1);

<FM>Description<FN>
Draws a preview of the printing of the current image by specifying margins, vertical position, horizontal position and size.
It also paints a rectangle over the image to show the margin limits.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>DestBitmap<FN></C> <C>Destination bitmap where to paint the preview. Resizes DestBitmap as needed.</C> </R>
<R> <C><FC>MaxBitmapWidth<FN></C> <C>Maximum destination bitmap width. Preview will be stretched to match this size.</C> </R>
<R> <C><FC>MaxBitmapHeight<FN></C> <C>Maximum destination bitmap height. Preview will be stretched to match this size.</C> </R>
<R> <C><FC>Printer<FN></C> <C>This is the Printer object. PreviewPrintImage need it to know printer settings as orientation or page sizes.</C> </R>
<R> <C><FC>MarginLeft<FN></C> <C>Left page margin in inches. Specying zero causes no margin to be used.</C> </R>
<R> <C><FC>MarginTop<FN></C> <C>Top page margin in inches. Specying zero causes no margin to be used.</C> </R>
<R> <C><FC>MarginRight<FN></C> <C>Right page margin in inches. Specying zero causes no margin to be used.</C> </R>
<R> <C><FC>MarginBottom<FN></C> <C>Bottom page margin in inches. Specying zero causes no margin to be used.</C> </R>
<R> <C><FC>VerticalPos<FN></C> <C>Determines how the image vertically aligns within the page.</C> </R>
<R> <C><FC>HorizontalPos<FN></C> <C>Determines how the image horizontally aligns within the page.</C> </R>
<R> <C><FC>Size<FN></C> <C>Determines the size of the image.</C> </R>
<R> <C><FC>SpecWidth<FN></C> <C>Specifies the absolute width of the image in inches. Valid only when Size=iesSPECIFIEDSIZE.</C> </R>
<R> <C><FC>SpecHeight<FN></C> <C>Specifies the absolute height of the image in inches. Valid only when Size=iesSPECIFIEDSIZE.</C> </R>
<R> <C><FC>GammaCorrection<FN></C> <C>The gamma correction value, use 1.0 to disable gamma correction.</C> </R>
</TABLE>


<FM>Example<FC>
// Paint and display the preview in ImageEnView2 component.
ImageEnView1.IO.PreviewPrintImage(ImageEnView2.Bitmap, ImageEnView2.Width, ImageEnView2.Height, Printer, 0, 0, 0, 0, ievpCENTER, iehpCENTER, iesFITTOPAGE , 0 , 0 , 1);
ImageEnView2.Update;

!!}
procedure TImageEnIO.PreviewPrintImage(DestBitmap: TBitmap; MaxBitmapWidth, MaxBitmapHeight: integer; PrinterObj: TPrinter;
                                       MarginLeft, MarginTop, MarginRight, MarginBottom: double;
                                       VerticalPos: TIEVerticalPos; HorizontalPos: TIEHorizontalPos; Size: TIESize;
                                       SpecWidth, SpecHeight: double; GammaCorrection: double);
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
  if (dpix <= 0) or (dpiy <= 0) then
    exit;
  PrintImageEx(DestBitmap.Canvas, dpix, dpiy, PageWidth / dpix, PageHeight / dpiy, MarginLeft, MarginTop, MarginRight, MarginBottom, VerticalPos, HorizontalPos, Size, SpecWidth, SpecHeight, GammaCorrection);
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
<FS>TImageEnIO.InjectJpegIPTC

<FM>Declaration<FC>
function InjectJpegIPTC(const FileName: WideString): Boolean;

<FM>Description<FN>
Replaces the IPTC information in the specified JPEG file with the current IPTC (in <A TImageEnIO.Params>) without loading or modifying the original image.

The method returns False if the operation could not be performed.

<FM>Example<FC>
// copy the IPTC info (not the image) from file one.jpg to the inside of file two.jpg, without loading any images
ImageEnView1.IO.ParamsFromFile('D:\one.jpg');
ImageEnView1.IO.InjectJpegIPTC('D:\two.jpg');

// removes IPTC info from image.jpg
ImageEnView1.IO.Params.IPTC_Info.Clear;
ImageEnView1.IO.InjectJpegIPTC('D:\image.jpg');

!!}
function TImageEnIO.InjectJpegIPTC(const FileName: WideString): boolean;
var
  fr: TMemoryStream;
  fm: TMemoryStream;
  Progress: TProgressRec;
begin
  fAborting := false;
  Progress.Aborting := @fAborting;
  Progress.fOnProgress := fOnIntProgress;
  Progress.Sender := Self;
  fr := TMemorystream.Create;
  fm := TMemoryStream.Create;
  try
    fr.LoadFromFile(FileName);
    fr.Position := 0;
    fm.Size := fr.Size;
    result := IEJpegInjectIPTC(fr, fm, fParams.IPTC_Info, Progress);
    fm.Size := fm.Position;
    if result then
      fm.SaveToFile(FileName);
  finally
    FreeAndNil(fr);
    FreeAndNil(fm);
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.InjectJpegIPTCStream

<FM>Declaration<FC>
function InjectJpegIPTCStream(InputStream, OutputStream: TStream): Boolean;

<FM>Description<FN>  
Replaces the IPTC information of the JPEG file in the InputStream stream with the current IPTC (in <A TImageEnIO.Params>) without loading or modifying the original image.

<FC>OutputStream<FN> contains the modified stream.

The method returns False if the operation could not be performed.
!!}
function TImageEnIO.InjectJpegIPTCStream(InputStream, OutputStream: TStream): boolean;
var
  Progress: TProgressRec;
begin
  Progress.fOnProgress := nil;
  result := IEJpegInjectIPTC(InputStream, OutputStream, fParams.IPTC_Info, Progress);
end;

{!!
<FS>TImageEnIO.InjectJpegEXIF

<FM>Declaration<FC>
function InjectJpegEXIF(const FileName: WideString): Boolean;

<FM>Description<FN>    
Replaces the EXIF information in the specified JPEG file with the current EXIF (in <A TImageEnIO.Params>) without loading or modifying the original image.

The method returns False if the operation could not be performed.

<FM>Example<FC>
// copy the EXIF info (not the image) from file one.jpg inside two.jpg, without loading any image
ImageEnView1.IO.ParamsFromFile('D:\one.jpg');
ImageEnView1.IO.InjectJpegEXIF('D:\two.jpg');

// removes EXIF info from image.jpg
ImageEnView1.IO.Params.EXIF_HasExifData := false;
ImageEnView1.IO.InjectJpegEXIF('D:\image.jpg');

!!}
function TImageEnIO.InjectJpegEXIF(const FileName: WideString): boolean;
var
  fr: TMemoryStream;
  fm: TMemoryStream;
  Progress: TProgressRec;
begin
  fAborting := false;
  Progress.Aborting := @fAborting;
  Progress.fOnProgress := fOnIntProgress;
  Progress.Sender := Self;
  fr := TMemoryStream.Create;
  fm := TMemoryStream.Create;
  try
    fr.LoadFromFile(FileName);
    fr.Position := 0;
    fm.Size := fr.Size;
    result := IEJpegInjectEXIF(fr, fm, fParams, Progress);
    fm.Size := fm.Position;
    if result then
      fm.SaveToFile(FileName);
  finally
    FreeAndNil(fr);
    FreeAndNil(fm);
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.InjectJpegEXIFStream

<FM>Declaration<FC>
function InjectJpegEXIFStream(InputStream, OutputStream: TStream): Boolean;

<FM>Description<FN>
Replaces the EXIF information in the JPEG file in InputStream stream with the current EXIF (in <A TImageEnIO.Params>) without loading or modifying the original image.

<FC>OutputStream<FN> contains the modified stream.

The method returns False if the operation could not be performed.
!!}
function TImageEnIO.InjectJpegEXIFStream(InputStream, OutputStream: TStream): boolean;
var
  Progress: TProgressRec;
begin
  Progress.fOnProgress := nil;
  result := IEJpegInjectEXIF(InputStream, OutputStream, fParams, Progress);
end;




{!!
<FS>TImageEnIO.InjectTIFFEXIF

<FM>Declaration<FC>
function InjectTIFFEXIF(const FileName: WideString; pageIndex: Integer): boolean;
function InjectTIFFEXIF(const InputFileName, OutputFileName: WideString; pageIndex: Integer): boolean;
function InjectTIFFEXIF(InputStream, OutputStream: TStream; pageIndex: Integer): boolean;

<FM>Description<FN>      
Replaces the EXIF information in the specified TIFF or InputStream stream with the current EXIF (in <A TImageEnIO.Params>) without loading or modifying the original image.

<FC>OutputStream<FN> contains the modified stream.

The method returns <FC>false<FN> if the operation could not be performed.

Note: InjectTIFFEXIF works only with TIFF and Microsoft HD Photo file formats.
!!}
{$ifdef IEINCLUDETIFFHANDLER}
function TImageEnIO.InjectTIFFEXIF(const FileName: WideString; pageIndex: Integer): boolean;
begin
  result := IEInjectTIFFEXIF(nil, nil, FileName, FileName, pageIndex, fParams);
end;

function TImageEnIO.InjectTIFFEXIF(const InputFileName, OutputFileName: WideString; pageIndex: Integer = 0): boolean;
begin
  result := IEInjectTIFFEXIF(nil, nil, InputFileName, OutputFileName, pageIndex, fParams);
end;

function TImageEnIO.InjectTIFFEXIF(InputStream, OutputStream: TStream; pageIndex: Integer): boolean;
begin
  result := IEInjectTIFFEXIF(InputStream, OutputStream, '', '', pageIndex, fParams);
end;
{$endif}



{$IFDEF IEINCLUDEOPENSAVEDIALOGS}

{!!
<FS>TImageEnIO.ExecuteOpenDialog

<FM>Declaration<FC>
function ExecuteOpenDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                           FilterIndex: Integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                           const Filter : WideString = ''; DefaultFilter : <A TIOFileType> = -1; LimitToFileType : <A TIOFileType> = -1) : WideString; overload;
function ExecuteOpenDialog(const Title : WideString; DefaultFilter : <A TIOFileType>; LimitToFileType : <A TIOFileType> = -1; AlwaysAnimate : boolean = False) : WideString; overload;

<FM>Description<FN>
The ExecuteOpenDialog shows and executes the open dialog. It encapsulates the <A TOpenImageEnDialog> component.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>InitialDir</C> <C>Folder displayed on opening (leave as '' for no default)</C> </R>
<R> <C>InitialFileName</C> <C>Default file name with extension (leave as '' for no default)</C> </R>
<R> <C>AlwaysAnimate</C> <C>Enable to animate GIF and AVI (without user needing to click the play button). Default is False</C> </R>
<R> <C>FilterIndex</C> <C>The index of the default selected item in the filter. Default is 0.
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
<R> <C>LimitToFileType</C> <C>Limits the filter to a specified <L TIOFileType>ImageEn file type</L>, plus "All Supported Types" and "All Files" (only relevant if Filter is not set). Default is -1</C> </R>
</TABLE>

Returns a null string ('') if the user clicks Cancel.


<FM>Examples<FC>
// Prompt to load a file into an ImageEnView
sFilename := ImageEnView1.IO.ExecuteOpenDialog;
if sFilename <> '' then
  ImageEnView1.IO.LoadFromFile(sFileName);

// Prompt to load an image, defaulting to JPEG format (second overloaded method)
sFilename := ImageEnView1.IO.ExecuteOpenDialog('Select an Image', ioJPEG);
if sFilename <> '' then
  ImageEnView1.IO.LoadFromFile(sFileName);

// Prompt to load an image, forcing GIF format (second overloaded method)
sFilename := ImageEnView1.IO.ExecuteOpenDialog('Select an Image', -1, ioGIF);
if sFilename <> '' then
  ImageEnView1.IO.LoadFromFile(sFileName);
!!}
function TImageEnIO.ExecuteOpenDialog(const Title : WideString; DefaultFilter : TIOFileType; LimitToFileType : TIOFileType = -1; AlwaysAnimate : boolean = False) : WideString;
begin
  Result := ExecuteOpenDialog('', '', AlwaysAnimate, 0, '', Title, '', DefaultFilter, LimitToFileType);
end;

function TImageEnIO.ExecuteOpenDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                                      FilterIndex: Integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                                      const Filter : WideString = ''; DefaultFilter : TIOFileType = -1; LimitToFileType : TIOFileType = -1) : WideString;
var
  fOpenImageEnDialog: TOpenImageEnDialog;
begin
  result := '';
  fOpenImageEnDialog := TOpenImageEnDialog.Create(self);
  try
    fOpenImageEnDialog.InitialDir := string(InitialDir);
    fOpenImageEnDialog.FileName := InitialFileName;
    {$IFDEF IEINCLUDEMULTIVIEW}
    fOpenImageEnDialog.AlwaysAnimate := AlwaysAnimate;
    {$ENDIF}
    fOpenImageEnDialog.Options := fOpenImageEnDialog.Options + [OFENABLESIZING];
    if Filter <> '' then
    begin
      fOpenImageEnDialog.AutoSetFilter := false;
      fOpenImageEnDialog.Filter := Filter;
    end;
    fOpenImageEnDialog.AutoSetFilterFileType := LimitToFileType;
    fOpenImageEnDialog.PreviewBorderStyle := IepsSoftShadow;     
    fOpenImageEnDialog.FilterDefault := DefaultFilter;
    fOpenImageEnDialog.FilterIndex := FilterIndex;
    fOpenImageEnDialog.AutoAdjustDPI := AutoAdjustDPI;
    fOpenImageEnDialog.FilteredAdjustDPI := FilteredAdjustDPI;
    fOpenImageEnDialog.ExtendedFilters := string(ExtendedFilters);
    if Title<>'' then
      fOpenImageEnDialog.Title := string(Title);
    if fOpenImageEnDialog.Execute then
    begin
      Params.ImageIndex := fOpenImageEnDialog.SelectedFrame;
      result := fOpenImageEnDialog.FileNameW;
    end;
  finally
    FreeAndNil(fOpenImageEnDialog);
  end;
end;
{$ENDIF} // IEINCLUDEOPENSAVEDIALOGS

{$IFDEF IEINCLUDEOPENSAVEDIALOGS}

{!!
<FS>TImageEnIO.ExecuteSaveDialog

<FM>Declaration<FC>
function ExecuteSaveDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                           FilterIndex: Integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                           const Filter : WideString = ''; DefaultFilter : <A TIOFileType> = -1; LimitToFileType : <A TIOFileType> = -1) : WideString;overload;
function ExecuteSaveDialog(const Title : WideString; DefaultFilter : <A TIOFileType>; LimitToFileType : <A TIOFileType> = -1; AlwaysAnimate : boolean = False) : WideString; overload;

<FM>Description<FN>
The ExecuteSaveDialog shows and executes the save dialog. It encapsulates the <A TSaveImageEnDialog> component.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C>InitialDir</C> <C>Folder displayed on opening (leave as '' for no default)</C> </R>
<R> <C>InitialFileName</C> <C>Default file name with extension (leave as '' for no default)</C> </R>
<R> <C>AlwaysAnimate</C> <C>Enable to animate GIF and AVI (without user needing to click the play button). Default is False</C> </R>
<R> <C>FilterIndex</C> <C>The index of the default selected item in the filter. Default is 0.
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
<R> <C>LimitToFileType</C> <C>Limits the filter to a specified <L TIOFileType>ImageEn file type. Default is -1</L></C> </R>
</TABLE>
      
Returns a null string ('') if the user clicks Cancel.

<FM>Examples<FC>
// Prompt to save a file in an ImageEnView
sFilename := ImageEnView1.IO.ExecuteSaveDialog;
if sFilename <> '' then
  ImageEnView1.IO.SaveToFile(sFileName)

// Prompt to save an image, defaulting to JPEG format (second overloaded method)
sFilename := ImageEnView1.IO.ExecuteOpenDialog('Save your Image', ioJPEG);
if sFilename <> '' then
  ImageEnView1.IO.SaveToFile(sFileName)

// Prompt to save an image, forcing GIF format (second overloaded method)
sFilename := ImageEnView1.IO.ExecuteOpenDialog('Save your Image', -1, ioGIF);
if sFilename <> '' then
  ImageEnView1.IO.SaveToFile(sFileName)

!!}
function TImageEnIO.ExecuteSaveDialog(const Title : WideString; DefaultFilter : TIOFileType; LimitToFileType : TIOFileType = -1; AlwaysAnimate : boolean = False) : WideString; 
begin
  Result := ExecuteSaveDialog('', '', AlwaysAnimate, 0, '', Title, '', DefaultFilter, LimitToFileType);
end;

function TImageEnIO.ExecuteSaveDialog(const InitialDir : WideString = ''; const InitialFileName : WideString = ''; AlwaysAnimate : boolean = False;
                                      FilterIndex: Integer = 0; const ExtendedFilters : WideString = ''; const Title : WideString = '';
                                      const Filter : WideString = ''; DefaultFilter : TIOFileType = -1; LimitToFileType : TIOFileType = -1) : WideString;
var
  fSaveImageEnDialog: TSaveImageEnDialog;
begin
  result := '';
  fSaveImageEnDialog := TSaveImageEnDialog.create(self);
  try
    fSaveImageEnDialog.InitialDir := string(InitialDir);
    fSaveImageEnDialog.FileName := InitialFileName;
    {$IFDEF IEINCLUDEMULTIVIEW}
    fSaveImageEnDialog.AlwaysAnimate := AlwaysAnimate;
    {$ENDIF}
    fSaveImageEnDialog.Options := fSaveImageEnDialog.Options + [OFENABLESIZING];           
    if Filter<>'' then
    begin
      fSaveImageEnDialog.AutoSetFilter := false;
      fSaveImageEnDialog.Filter := Filter;
    end;
    fSaveImageEnDialog.AutoSetFilterFileType := LimitToFileType;
    fSaveImageEnDialog.FilterDefault := DefaultFilter;
    fSaveImageEnDialog.AttachedImageEnIO := self;
    fSaveImageEnDialog.PreviewBorderStyle := IepsSoftShadow;
    fSaveImageEnDialog.FilterIndex := FilterIndex;
    fSaveImageEnDialog.AutoAdjustDPI := AutoAdjustDPI;
    fSaveImageEnDialog.FilteredAdjustDPI := FilteredAdjustDPI;
    fSaveImageEnDialog.ExtendedFilters := string(ExtendedFilters);
    fSaveImageEnDialog.ShowFormats := iesfImagesOnly;
    if Title<>'' then
      fSaveImageEnDialog.Title := string(Title);
    if fSaveImageEnDialog.Execute then
      result := fSaveImageEnDialog.FileNameW;
  finally
    FreeAndNil(fSaveImageEnDialog);
  end;
end;
{$ENDIF} // IEINCLUDEOPENSAVEDIALOGS

{!!
<FS>TImageEnIO.CaptureFromScreen

<FM>Declaration<FC>
procedure CaptureFromScreen(Source: <A TIECSSource>; MouseCursor: TCursor = -1);

<FM>Description<FN>
Captures the current desktop or active window.

MouseCursor defines the cursor to draw because ImageEn cannot get it from Windows. Use -1 for none.

<FM>Example<FC>
// Save the current desktop to 'screen.png'
ImageEnView1.io.CaptureFromScreen(iecsScreen, -1);
ImageEnView1.io.SaveToFile('D:\screen.png');
!!}
procedure TImageEnIO.CaptureFromScreen(Source: TIECSSource; MouseCursor: TCursor);
var
  hwin, hdc: THandle;
  x, y, w, h: integer;
  rc: TRect;
  vdsk: boolean;
  pt: TPoint;
  xbmp: TIEDibBitmap;
  plc: TWINDOWPLACEMENT;
  hCursor: THandle;
  MousePt: TPoint;
  ClientLeftTop: TPoint;
  IconInfo: TIconInfo;
begin                                     
  // ASYNC MODE
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateCaptureFromScreen(self, CaptureFromScreen, Source, MouseCursor);
    exit;
  end;

  if not MakeConsistentBitmap([]) then
    exit;

  getcursorpos(MousePt);
  hCursor := Screen.Cursors[-2];
  GetIconInfo(hCursor, IconInfo);
  ClientLeftTop := Point(0, 0);
  vdsk := (IEGlobalSettings().OpSys <> ieosWin95) and (IEGlobalSettings().OpSys <> ieosWinNT4);
  hwin := 0;
  w := 0;
  h := 0;
  x := 0;
  y := 0;
  case Source of
  
    iecsScreen:
      begin
        hwin := GetDesktopWindow;
        if vdsk then
        begin
          x := GetSystemMetrics(76); // SM_XVIRTUALSCREEN
          y := GetSystemMetrics(77); // SM_YVIRTUALSCREEN
          w := GetSystemMetrics(78); // SM_CXVIRTUALSCREEN
          h := GetSystemMetrics(79); // SM_CYVIRTUALSCREEN
        end
        else
        begin
          // OLDER VERSIONS OF WINDOWS
          w := Screen.Width;
          h := Screen.Height;
        end;
        dec(MousePt.x, integer(IconInfo.xHotspot));
        dec(MousePt.y, integer(IconInfo.yHotspot));
        Windows.ScreenToClient(hwin, MousePt);
      end;

    iecsPrimary:
      begin
        hwin := GetDesktopWindow;
        if vdsk then
        begin
          x := 0;
          y := 0;
          w := GetSystemMetrics(0);  // SM_CXSCREEN
          h := GetSystemMetrics(1);  // SM_CYSCREEN
        end
        else
        begin
          // OLDER VERSIONS OF WINDOWS
          w := Screen.Width;
          h := Screen.Height;
        end;
        dec(MousePt.x, integer(IconInfo.xHotspot));
        dec(MousePt.y, integer(IconInfo.yHotspot));
        Windows.ScreenToClient(hwin, MousePt);
      end;

    iecsForegroundWindow:
      begin
        hwin := GetForegroundWindow;
        if hwin <> 0 then
        begin
          plc.length := sizeof(TWINDOWPLACEMENT);
          GetWindowPlacement(hwin, @plc);
          if GetWindowRect(hwin, rc) then
          begin
            if plc.showCmd = SW_SHOWMAXIMIZED then
            begin
              if rc.left < 0 then
                x := -rc.left;
              if rc.top < 0 then
                y := -rc.top;
              w := rc.Right - x;
              h := rc.Bottom - y;
              if rc.left < 0 then
                rc.left := 0;
              if rc.top < 0 then
                rc.top := 0;
            end
            else
            begin
              w := rc.Right - rc.Left;
              h := rc.Bottom - rc.Top;
            end;
            Windows.ClientToScreen(hwin, ClientLeftTop);
            dec(MousePt.x, -(ClientLeftTop.x - rc.left) + integer(IconInfo.xHotspot));
            dec(MousePt.y, -(ClientLeftTop.y - rc.top) + integer(IconInfo.yHotspot));
            Windows.ScreenToClient(hwin, MousePt);
          end
          else
            hwin := 0;
        end;
      end;

    iecsForegroundWindowClient:
      begin
        hwin := GetForegroundWindow;
        if hwin <> 0 then
        begin
          if GetClientRect(hwin, rc) then
          begin
            pt.x := rc.Left;
            pt.y := rc.Top;
            Windows.ClientToScreen(hwin, pt);
            x := pt.x;
            y := pt.y;
            pt.x := rc.Right;
            pt.y := rc.Bottom;
            Windows.ClientToScreen(hwin, pt);
            w := pt.x - x;
            h := pt.y - y;
            hwin := GetDesktopWindow;
            dec(MousePt.x, integer(IconInfo.xHotspot) + x);
            dec(MousePt.y, integer(IconInfo.yHotspot) + y);
          end
          else
            hwin := 0;
        end;
      end;
  end;

  if hwin <> 0 then
  begin
    xbmp := TIEDibBitmap.Create;
    xbmp.AllocateBits(w, h, 24);
    hdc := GetWindowDC(hwin);
    if hdc <> 0 then
    begin
      BitBlt(xbmp.HDC, 0, 0, w, h, hdc, x, y, SRCCOPY);
      if MouseCursor <> -1 then
        DrawIconEx(xbmp.HDC, MousePt.x, MousePt.y, hCursor, 0, 0, 0, 0, DI_DEFAULTSIZE or DI_NORMAL);
      fIEBitmap.Allocate(w, h, ie24RGB);
      for y := 0 to h - 1 do
        CopyMemory(fIEBitmap.Scanline[y], xbmp.Scanline[y], fIEBitmap.RowLen);
      FreeAndNil(xbmp);
      ReleaseDC(hwin, hdc);
      fParams.DpiX := IEGlobalSettings().SystemDPIX;
      fParams.DpiY := IEGlobalSettings().SystemDPIY;
      Update;
    end;
  end;
  DoFinishWork;
end;

{$IFDEF IEINCLUDEPRINTDIALOGS}

constructor TIOPrintPreviewParams.Create;
begin
  inherited Create;

  fMarginTop         := 1;
  fMarginLeft        := 1;
  fMarginRight       := 1;
  fMarginBottom      := 1;
  fPosition          := ppCenter;
  fSize              := psFitToPage;
  fWidth             := -1;  // default auto-calculated
  fHeight            := -1;  // default auto-calculated
  fGamma             := 1;

  fPrintThumbnails   := False;
  fThumbnailColumns  := 4;
  fThumbnailRows     := 5;
  fThumbnailSpacing  := 0.1;
  fThumbnailStyle    := ptSoftShadow;
  fThumbnailShowText := True;
  fDlgWidth          := 0;
  fDlgHeight         := 0;
end;

{!!
<FS>TIOPrintPreviewParams.SaveToFile

<FM>Declaration<FC>
procedure SaveToFile(const FileName: WideString);

<FM>Description<FN>
Save all settings of the Print Preview dialog to file.
!!}
procedure TIOPrintPreviewParams.SaveToFile(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmCreate);
  SaveToStream(fs);
  FreeAndNil(fs);
end;

{!!
<FS>TIOPrintPreviewParams.LoadFromFile

<FM>Declaration<FC>
procedure LoadFromFile(const FileName: WideString);

<FM>Description<FN>
Load all settings of the Print Preview dialog from file.
!!}
procedure TIOPrintPreviewParams.LoadFromFile(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TIOPrintPreviewParams.SaveToStream

<FM>Declaration<FC>
procedure SaveToStream(Stream: TStream);

<FM>Description<FN>
Save all settings of the Print Preview dialog to a stream.
!!}
procedure TIOPrintPreviewParams.SaveToStream(Stream: TStream);
begin
  Stream.Write(fMarginTop, sizeof(double));
  Stream.Write(fMarginLeft, sizeof(double));
  Stream.Write(fMarginRight, sizeof(double));
  Stream.Write(fMarginBottom, sizeof(double));
  Stream.Write(fPosition, sizeof(TIOPrintPreviewPosition));
  Stream.Write(fSize, sizeof(TIOPrintPreviewSize));
  Stream.Write(fWidth, sizeof(double));
  Stream.Write(fHeight, sizeof(double));
  Stream.Write(fGamma, sizeof(double));

  Stream.Write(fPrintThumbnails, sizeof(Boolean));
  Stream.Write(fThumbnailColumns, sizeof(Integer));
  Stream.Write(fThumbnailRows, sizeof(Integer));
  Stream.Write(fThumbnailSpacing, sizeof(double));
  Stream.Write(fThumbnailStyle, sizeof(TIOPrintPreviewThumbnailStyle));
  Stream.Write(fThumbnailShowText, sizeof(Boolean));
  Stream.Write(fDlgWidth, sizeof(Integer));
  Stream.Write(fDlgHeight, sizeof(Integer));
end;

{!!
<FS>TIOPrintPreviewParams.LoadFromStream

<FM>Declaration<FC>
procedure LoadFromStream(Stream: TStream);

<FM>Description<FN>
Loads all settings of the Print Preview dialog from a stream.
!!}
procedure TIOPrintPreviewParams.LoadFromStream(Stream: TStream);
begin
  Stream.Read(fMarginTop, sizeof(double));
  Stream.Read(fMarginLeft, sizeof(double));
  Stream.Read(fMarginRight, sizeof(double));
  Stream.Read(fMarginBottom, sizeof(double));
  Stream.Read(fPosition, sizeof(TIOPrintPreviewPosition));
  Stream.Read(fSize, sizeof(TIOPrintPreviewSize));
  Stream.Read(fWidth, sizeof(double));
  Stream.Read(fHeight, sizeof(double));
  Stream.Read(fGamma, sizeof(double));

  Stream.Read(fPrintThumbnails, sizeof(Boolean));
  Stream.Read(fThumbnailColumns, sizeof(Integer));
  Stream.Read(fThumbnailRows, sizeof(Integer));   
  Stream.Read(fThumbnailSpacing, sizeof(double));
  Stream.Read(fThumbnailStyle, sizeof(TIOPrintPreviewThumbnailStyle));
  Stream.Read(fThumbnailShowText, sizeof(Boolean));
  Stream.Read(fDlgWidth, sizeof(Integer));
  Stream.Read(fDlgHeight, sizeof(Integer));
end;

{!!
<FS>TIOPrintPreviewParams.GetProperty

<FM>Declaration<FC>
function GetProperty(const Prop: WideString): WideString;

<FM>Description<FN>
Read a property of the Print Preview dialog using its string representation.
!!}
function TIOPrintPreviewParams.GetProperty(const Prop: WideString): WideString;
var
  ss: WideString;
begin
  ss := UpperCase(IERemoveCtrlCharsW(Prop));
  if ss = PPP_MARGINTOP then
    result := IEFloatToStrW(fMarginTop)
  else
  if ss = PPP_MARGINLEFT then
    result := IEFloatToStrW(fMarginLeft)
  else
  if ss = PPP_MARGINRIGHT then
    result := IEFloatToStrW(fMarginRight)
  else
  if ss = PPP_MARGINBOTTOM then
    result := IEFloatToStrW(fMarginBottom)
  else
  if ss = PPP_POSITION then
    result := IntToStr(integer(fPosition))
  else
  if ss = PPP_SIZE then
    result := IntToStr(integer(fSize))
  else
  if ss = PPP_WIDTH then
    result := IEFloatToStrW(fWidth)
  else
  if ss = PPP_HEIGHT then
    result := IEFloatToStrW(fHeight)
  else
  if ss = PPP_GAMMA then
    result := IEFloatToStrW(fGamma)
  else
  if ss = PPP_PRINTTHUMBNAILS then
    Result := IEBool2StrW(fPrintThumbnails)
  else
  if ss = PPP_THUMBNAILCOLUMNS then
    Result := IntToStr(fThumbnailColumns)
  else
  if ss = PPP_THUMBNAILROWS then
    Result := IntToStr(fThumbnailRows)
  else
  if ss = PPP_THUMBNAILSPACING then
    Result := IEFloatToStrW(fThumbnailSpacing)
  else
  if ss = PPP_THUMBNAILSTYLE then
    Result := IntToStr(Integer(fThumbnailStyle))
  else
  if ss = PPP_THUMBNAILSHOWTEXT then
    Result := IEBool2StrW(fThumbnailShowText)
  else
  if ss = PPP_DLGWIDTH then
    Result := IntToStr(fDlgWidth)
  else
  if ss = PPP_DLGHEIGHT then
    Result := IntToStr(fDlgHeight);
end;

{!!
<FS>TIOPrintPreviewParams.SetProperty

<FM>Declaration<FC>
procedure SetProperty(Prop, Value: WideString);

<FM>Description<FN>   
Write a property of the Print Preview dialog using its string representation.
!!}
procedure TIOPrintPreviewParams.SetProperty(Prop, Value: WideString);
var
  ss: WideString;
begin
  ss := UpperCase(IERemoveCtrlCharsW(Prop));
  Value := IERemoveCtrlCharsW(Value);
  if ss = PPP_MARGINLEFT then
    fMarginLeft := IEStrToFloatDefW(value, 0)
  else
  if ss = PPP_MARGINRIGHT then
    fMarginRight := IEStrToFloatDefW(value, 0)
  else
  if ss = PPP_MARGINTOP then
    fMarginTop := IEStrToFloatDefW(value, 0)
  else
  if ss = PPP_MARGINBOTTOM then
    fMarginBottom := IEStrToFloatDefW(value, 0)
  else
  if ss = PPP_POSITION then
    fPosition := TIOPrintPreviewPosition(StrToIntDef(value, 0))
  else
  if ss = PPP_SIZE then
    fSize := TIOPrintPreviewSize(StrToIntDef(value, 0))
  else
  if ss = PPP_WIDTH then
    fWidth := IEStrToFloatDefW(value, 0)
  else
  if ss = PPP_HEIGHT then
    fHeight := IEStrToFloatDefW(value, 0)
  else
  if ss = PPP_GAMMA then
    fGamma := IEStrToFloatDefW(value, 0)
  else
  if ss = PPP_PRINTTHUMBNAILS then
    fPrintThumbnails := IEStr2BoolW(value)
  else
  if ss = PPP_THUMBNAILCOLUMNS then
    fThumbnailColumns := StrToIntDef(value, 4)
  else
  if ss = PPP_THUMBNAILROWS then
    fThumbnailRows := StrToIntDef(value, 5)
  else
  if ss = PPP_THUMBNAILSPACING then
    fThumbnailSpacing := IEStrToFloatDefW(value, 0)
  else
  if ss = PPP_THUMBNAILSTYLE then
    fThumbnailStyle := TIOPrintPreviewThumbnailStyle(StrToIntDef(value, 0))
  else
  if ss = PPP_THUMBNAILSHOWTEXT then
    fThumbnailShowText := IEStr2BoolW(value)
  else
  if ss = PPP_DLGWIDTH then
    fDlgWidth := StrToIntDef(value, 0)
  else
  if ss = PPP_DLGHEIGHT then
    fDlgHeight := StrToIntDef(value, 0);
end;


{!!
<FS>TImageEnIO.DoPrintPreviewDialog

<FM>Declaration<FC>
function DoPrintPreviewDialog(DialogType: <A TIEDialogType> = iedtDialog; const TaskName: WideString = ''; PrintAnnotations: Boolean = false; const Caption: WideString = ''): Boolean;

<FM>Description<FN>
Displays a print preview dialog of the type specified by <FC>DialogType<FN>.

<FC>TaskName<FN> determines the text that appears listed in the Print Manager and on network header pages.
If <FC>PrintAnnotation<FN> is true and the image contains Imaging Annotations they will be printed out.
<FC>Caption<FN> specifies the caption of preview dialog.

Note: The language used in the dialog is controlled by <A TIEImageEnGlobalSettings.MsgLanguage>. The styling can also be adjusted using <A TIEImageEnGlobalSettings.UseButtonGlyphsInDialogs>

<FM>Example<FC>
ImageEnView1.IO.DoPrintPreviewDialog(iedtDialog);
<IMG help_images\56.bmp>

ImageEnView1.IO.DoPrintPreviewDialog(iedtMaxi);
<IMG help_images\57.bmp>

<FM>See Also<FN>
- <A TImageEnIO.PrintPreviewParams>
- <A TImageEnIO.DialogsMeasureUnit>
!!}
function TImageEnIO.DoPrintPreviewDialog(DialogType: TIEDialogType; const TaskName: WideString; PrintAnnotations: Boolean; const Caption: WideString): boolean;
var
  fieprnform1: tfieprnform1;
  fieprnform2: tfieprnform2;
begin
  if fResetPrinter then
    try
      IEResetPrinter();
    except
       MessageDlg('The Print Preview could not be displayed because a printer has not been configured on this computer.', mtError, [mbOK], 0);
       result := false;
       exit;
    end;

  result := false;
  if not MakeConsistentBitmap([]) then
    exit;
  if (fIEBitmap.Width <= 1) and (fIEBitmap.Height <= 1) then
    exit;
  case DialogType of
    iedtDialog:
      begin
        fieprnform2 := tfieprnform2.Create(self);
        try
          fieprnform2.io := self;
          fieprnform2.fDialogsMeasureUnit := fDialogsMeasureUnit;
          fieprnform2.UpdateLanguage();

          if fPreviewFontEnabled then
            fieprnform2.Font.Assign(fPreviewFont)
          else
            fieprnform2.Font.Assign(IEGetDefaultDialogFont);

          fieprnform2.fTaskName := TaskName;
          fieprnform2.fPrintPreviewParams := fPrintPreviewParams;
          fieprnform2.PrintAnnotations := PrintAnnotations;
          if Caption <> '' then
            fieprnform2.Caption := Caption
          else
            fieprnform2.Caption := iemsg(IEMSG_PRINT);
          if assigned(fOnIOPreview) then
            fOnIOPreview(self, fieprnform2);
          result := fieprnform2.ShowModal = mrOk;
        finally
          fieprnform2.Release;
        end;
      end;
    iedtMaxi:
      begin
        fieprnform1 := tfieprnform1.Create(self);
        try
          fieprnform1.io := self;
          fieprnform1.fDialogsMeasureUnit := fDialogsMeasureUnit;
          fieprnform1.UpdateLanguage();

          if fPreviewFontEnabled then
            fieprnform1.Font.Assign(fPreviewFont)
          else
            fieprnform1.Font.Assign(IEGetDefaultDialogFont);

          fieprnform1.fTaskName := TaskName;
          fieprnform1.Left := 0;
          fieprnform1.Top := 0;
          fieprnform1.fPrintPreviewParams := fPrintPreviewParams;
          fieprnform1.PrintAnnotations := PrintAnnotations;
          if Caption <> '' then
            fieprnform1.Caption := Caption
          else
            fieprnform1.Caption := iemsg(IEMSG_PRINT);
          if assigned(fOnIOPreview) then
            fOnIOPreview(self, fieprnform1);
          result := fieprnform1.ShowModal = mrOk;
        finally
          fieprnform1.Release;
        end;
      end;
  end;
end;

{$ENDIF}

function IEAdjustDPI(bmp: TIEBitmap; Params: TIOParamsVals; FilteredAdjustDPI: boolean): TIEBitmap;
var
  new_w, new_h: integer;
begin
  result := bmp;
  with Params do
    if (DpiX <> DpiY) and (DpiX > 0) and (DpiY > 0) and (bmp.Width > 0) and (bmp.Height > 0) then
    begin
      result := TIEBitmap.Create;
      if Width > Height then
      begin
        new_h := trunc((Height / DpiY) * DpiX);
        new_w := Width;
        DpiY := DpiX;
        Height := new_h;
      end
      else
      begin
        new_w := trunc((Width / DpiX) * DpiY);
        new_h := Height;
        DpiX := DpiY;
        Width := new_w;
      end;
      if FilteredAdjustDPI then
      begin
        result.Allocate(new_w, new_h, ie24RGB);
        if bmp.PixelFormat <> ie24RGB then
          bmp.PixelFormat := ie24RGB;
        _ResampleEx(bmp, result, nil, IEGlobalSettings().DefaultResampleFilter, nil, nil);
      end
      else
      begin
        result.Allocate(new_w, new_h, bmp.PixelFormat);
        _IEBmpStretchEx(bmp, result, nil, nil);
      end;
    end;
end;

procedure TImageEnIO.AdjustDPI;
var
  new_w, new_h: integer;
  newbmp, oldalpha: TIEBitmap;
begin
  with fParams do
  begin
    if not MakeConsistentBitmap([]) then
      exit;
    if (DpiX <> DpiY) and (DpiX > 0) and (DpiY > 0) and (fIEBitmap.Width > 0) and (fIEBitmap.Height > 0) and (fHeight > 0) and (fWidth > 0) then
    begin
      newbmp := TIEBitmap.Create;
      if fWidth > fHeight then
      begin
        new_h := trunc((fHeight / fDpiY) * fDpiX);
        new_w := fWidth;
        fDpiY := fDpiX;
        fHeight := new_h;
      end
      else
      begin
        new_w := trunc((fWidth / fDpiX) * fDpiY);
        new_h := fHeight;
        fDpiX := fDpiY;
        fWidth := new_w;
      end;
      if fIEBitmap.HasAlphaChannel then
      begin
        oldalpha := TIEBitmap.Create;
        oldalpha.assign(fIEBitmap.AlphaChannel);
      end
      else
        oldalpha := nil;
      if fFilteredAdjustDPI then
      begin
        newbmp.Allocate(new_w, new_h, ie24RGB);
        if fIEBitmap.PixelFormat <> ie24RGB then
          fIEBitmap.PixelFormat := ie24RGB;
        _ResampleEx(fIEBitmap, newbmp, nil, IEGlobalSettings().DefaultResampleFilter, nil, nil);
      end
      else
      begin
        newbmp.Allocate(new_w, new_h, fIEBitmap.PixelFormat);
        _IEBmpStretchEx(fIEBitmap, newbmp, nil, nil);
      end;
      fIEBitmap.Assign(newbmp);
      FreeAndNil(newbmp);
      // stretch alpha
      if assigned(oldalpha) then
      begin
        _IEBmpStretchEx(oldalpha, fIEBitmap.AlphaChannel, nil, nil);
        FreeAndNil(oldalpha);
      end;
    end;
  end;
end;

{!!
<FS>JpegLosslessTransformStream

<FM>Declaration<FC>
function JpegLosslessTransformStream(SourceStream, DestStream: TStream; Transform: <A TIEJpegTransform>; GrayScale: Boolean; CopyMarkers: <A TIEJpegCopyMarkers>; CutRect: Trect; UpdateEXIF: Boolean = False): Boolean;

<FM>Description<FN>
Losslessly rotates, flips or crops (cuts) a JPEG in a stream.

<FM>Description<FN>
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>SourceStream<FN></C> <C>A TStream object that contains the jpeg source stream</C> </R>
<R> <C><FC>DestStream<FN></C> <C>A TStream object that will receive the jpeg result stream</C> </R>
<R> <C><FC>Transform<FN></C> <C>The <L TIEJpegTransform>transformation</L> to perform</C> </R>
<R> <C><FC>GrayScale<FN></C> <C>True force grayscale output</C> </R>
<R> <C><FC>CopyMarkers<FN></C> <C>How <L TIEJpegCopyMarkers>comments and markers</L> (e.g. IPTC info) should be copied</C> </R>
<R> <C><FC>CutRect<FN></C> <C>Specifies the rectangle to retain when <FC>Transform = jtCut<FN></C> </R>
<R> <C><FC>UpdateEXIF<FN></C> <C>Updates the EXIF thumbnail and orientation tags</C> </R>
</TABLE>

Returns false if the operation failed.

<FM>Lossless Information<FN>
Whenever you save a JPEG by regular methods it needs to re-encoded, so after multiple re-saves the quality can become quite degraded. Lossless JPEG operations, on the other hand, work by rearranging the compressed data, without ever fully decoding the image, therefore there is no degration in quality.

Note, however, that lossless operations can only work on full DCT blocks, so when cutting/cropping the resulting image may contain at least a part of the area outside <FC>CutRect<FN> depending on the need to align with to the nearest DCT block boundary (or multiple blocks, depending on the sampling factors).

!!}
function JpegLosslessTransformStream(SourceStream, DestStream: TStream; Transform: TIEJpegTransform; GrayScale: boolean; CopyMarkers: TIEJpegCopyMarkers; CutRect: TRect; UpdateEXIF: Boolean): boolean;
var
  xp: TProgressRec;
  ab: boolean;
begin
  ab := false;
  xp.Aborting := @ab;
  IEJpegLosslessTransform(SourceStream, DestStream, xp, integer(Transform), GrayScale, integer(CopyMarkers), CutRect, UpdateEXIF);
  result := not ab;
end;

{!!
<FS>JpegLosslessTransform

<FM>Declaration<FC>
function JpegLosslessTransform(const SourceFile, DestFile: WideString; Transform: <A TIEJpegTransform>; GrayScale: Boolean; CopyMarkers: <A TIEJpegCopyMarkers>; CutRect: TRect; UpdateEXIF: Boolean = False): Boolean;
function JpegLosslessTransform(const SourceFile, DestFile: WideString; Transform: <A TIEJpegTransform>): boolean; overload;

<FM>Description<FN>
Losslessly rotates, flips or crops (cuts) a JPEG file.

<FM>Description<FN>
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>SourceFile<FN></C> <C>The full path and filename of the source file</C> </R>
<R> <C><FC>DestFile<FN></C> <C>The full path and filename for the resulting file</C> </R>
<R> <C><FC>Transform<FN></C> <C>The <L TIEJpegTransform>transformation</L> to perform</C> </R>
<R> <C><FC>GrayScale<FN></C> <C>True force grayscale output</C> </R>
<R> <C><FC>CopyMarkers<FN></C> <C>How <L TIEJpegCopyMarkers>comments and markers</L> (e.g. IPTC info) should be copied</C> </R>
<R> <C><FC>CutRect<FN></C> <C>Specifies the rectangle to retain when <FC>Transform = jtCut<FN></C> </R>
<R> <C><FC>UpdateEXIF<FN></C> <C>Updates the EXIF thumbnail and orientation tags</C> </R>
</TABLE>

Returns false if the operation failed.

See also: <A JpegLosslessTransform2> which works with a single file.

<FM>Lossless Information<FN>
Whenever you save a JPEG by regular methods it needs to re-encoded, so after multiple re-saves the quality can become quite degraded. Lossless JPEG operations, on the other hand, work by rearranging the compressed data, without ever fully decoding the image, therefore there is no degration in quality.

Note, however, that lossless operations can only work on full DCT blocks, so when cutting/cropping the resulting image may contain at least a part of the area outside <FC>CutRect<FN> depending on the need to align with to the nearest DCT block boundary (or multiple blocks, depending on the sampling factors).

<FM>Example<FC>
// Rotate input.jpg 90° clockwise and save to output.jpg
JpegLosslessTransform('D:\input.jpg', 'D:\output.jpg', jtRotate90, false, jcCopyAll, Rect(0, 0, 0, 0));
!!}
function JpegLosslessTransform(const SourceFile, DestFile: WideString; Transform: TIEJpegTransform; GrayScale: boolean; CopyMarkers: TIEJpegCopyMarkers; CutRect: TRect; UpdateEXIF: Boolean): boolean;
var
  fr, fw: TIEWideFileStream;
begin
  try
    fr := TIEWideFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  except
    result := false;
    exit;
  end;
  try
    fw := TIEWideFileStream.Create(DestFile, fmCreate);
  except
    FreeAndNil(fr);
    result := false;
    exit;
  end;
  result := JpegLosslessTransformStream(fr, fw, Transform, GrayScale, CopyMarkers, CutRect, UpdateEXIF);
  FreeAndNil(fw);
  FreeAndNil(fr);
end;

function JpegLosslessTransform(const SourceFile, DestFile: WideString; Transform: TIEJpegTransform): boolean; overload;
begin
  result := JpegLosslessTransform(SourceFile, DestFile, Transform, false, jcCopyAll, Rect(0, 0, 0, 0), true);
end;

{!!
<FS>JpegLosslessTransform2

<FM>Declaration<FC>
function JpegLosslessTransform2(const FileName: WideString; Transform: <A TIEJpegTransform>; GrayScale: boolean; CopyMarkers: <A TIEJpegCopyMarkers>; CutRect: TRect; UpdateEXIF: Boolean = False): boolean; overload;
function JpegLosslessTransform2(const FileName: WideString; Transform: <A TIEJpegTransform>): boolean; overload;

<FM>Description<FN>
Losslessly rotates, flips or crops (cuts) a JPEG file. Same as <A JpegLosslessTransform>, but the source file is overwritten with the transformed image.

<FM>Description<FN>
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>FileName<FN></C> <C>The full path and filename of the file to transform</C> </R>
<R> <C><FC>Transform<FN></C> <C>The <L TIEJpegTransform>transformation</L> to perform</C> </R>
<R> <C><FC>GrayScale<FN></C> <C>True force grayscale output</C> </R>
<R> <C><FC>CopyMarkers<FN></C> <C>How <L TIEJpegCopyMarkers>comments and markers</L> (e.g. IPTC info) should be copied</C> </R>
<R> <C><FC>CutRect<FN></C> <C>Specifies the rectangle to retain when <FC>Transform = jtCut<FN></C> </R>
<R> <C><FC>UpdateEXIF<FN></C> <C>Updates the EXIF thumbnail and orientation tags</C> </R>
</TABLE>

Returns false if the operation failed.

<FM>Lossless Information<FN>
Whenever you save a JPEG by regular methods it needs to re-encoded, so after multiple re-saves the quality can become quite degraded. Lossless JPEG operations, on the other hand, work by rearranging the compressed data, without ever fully decoding the image, therefore there is no degration in quality.

Note, however, that lossless operations can only work on full DCT blocks, so when cutting/cropping the resulting image may contain at least a part of the area outside <FC>CutRect<FN> depending on the need to align with to the nearest DCT block boundary (or multiple blocks, depending on the sampling factors).

<FM>Example<FC>
// Rotate MyImage.jpg 90° clockwise
JpegLosslessTransform('D:\MyImage.jpg', jtRotate90);
!!}
function JpegLosslessTransform2(const FileName: WideString; Transform: TIEJpegTransform; GrayScale: boolean; CopyMarkers: TIEJpegCopyMarkers; CutRect: TRect; UpdateEXIF: Boolean): boolean;
var
  i: Integer;
  SrcFileName: WideString;
begin
  i := 0;
  repeat
    SrcFileName := FileName+IntToStr(i);
    inc(i);
  until not IEFileExistsW(SrcFileName);
  MoveFileW(PWideChar(FileName), PWideChar(SrcFileName));
  result := JpegLosslessTransform(SrcFileName, FileName, Transform, GrayScale, CopyMarkers, CutRect, UpdateEXIF);
  if not result then
  begin
    if IEFileExistsW(FileName) then
      DeleteFileW(PWideChar(FileName));
    MoveFileW(PWideChar(SrcFileName), PWideChar(FileName));
  end
  else
  DeleteFileW(PWideChar(SrcFileName));
end;


function JpegLosslessTransform2(const FileName: WideString; Transform: TIEJpegTransform): boolean;
begin
  result := JpegLosslessTransform2(FileName, Transform, False, jcCopyAll, Rect(0, 0, 0, 0), true);
end;

              




// return frame count
{!!
<FS>TImageEnIO.OpenAVIFile

<FM>Declaration<FC>
function OpenAVIFile(const FileName: WideString): Integer;

<FM>Description<FN>
Opens an AVI file without reading any frames. The result is the the number of frames contained in the AVI.

<FM>Example<FC>
// Save the first AVI frame to 'frame1.jpg', second frame to 'frame2.jpg'
ImageEnView1.IO.OpenAVIFile( 'C:\film.avi' );

ImageEnView1.IO.LoadFromAVI( 0 );
ImageEnView1.IO.SaveToFile('D:\frame1.jpg');

ImageEnView1.IO.LoadFromAVI( 1 );
ImageEnView1.IO.SaveToFile('D:\frame2.jpg');
...
ImageEnView1.IO.CloseAVIFile;

<FM>See Also<FN>
- <A TImageEnIO.CloseAVIFile>
- <A TImageEnIO.LoadFromAVI>
- <A TImageEnIO.IsOpenAVI>

!!}
function TImageEnIO.OpenAVIFile(const FileName: WideString): integer;
var
  psi: TAviStreamInfoW;
begin
  fAborting := false;
  if fAVI_avf <> nil then
    CloseAVIFile();
  result := 0;
  fParams.AVI_FrameCount := 0;
  try
    fParams.fFileName := FileName;
    fParams.FileType := ioUnknown;
    if not gAVIFILEinit then
    begin
      AVIFileInit();
      gAVIFILEinit := true;
    end;
    if AVIFileOpenW(PAVIFILE(fAVI_avf), PWideChar(FileName), OF_READ, nil) <> 0 then
    begin
      fAborting := True;
      exit;
    end;
    if AVIFileGetStream(PAVIFILE(fAVI_avf), PAVISTREAM(fAVI_avs), streamtypeVIDEO, 0) <> 0 then
    begin
      AVIFileRelease(fAVI_avf);
      fAborting := True;
      exit;
    end;
    if AVIStreamInfoW(PAVISTREAM(fAVI_avs), psi, sizeof(TAVISTREAMINFOW)) <> 0 then
    begin
      AVIFileRelease(fAVI_avf);
      AVIStreamRelease(fAVI_avs);
      fAborting := True;
      exit;
    end;
    move(psi, fAVI_psi, sizeof(TAviStreamInfoW_Ex));
    fAVI_gf := AVIStreamGetFrameOpen(fAVI_avs, nil);
    if fAVI_gf = nil then
    begin
      AVIFileRelease(fAVI_avf);
      AVIStreamRelease(fAVI_avs);
      fAborting := True;
      exit;
    end;
    result := fAVI_psi.dwLength;
    fParams.AVI_FrameCount := fAVI_psi.dwLength;
    fParams.fAVI_FrameDelayTime := 1000 / (fAVI_psi.dwRate/fAVI_psi.dwScale);
  finally
    if fAborting then
    begin
      fAVI_avf := nil;
      fAVI_avs := nil;
      fAVI_gf := nil;
    end;
  end;
end;

{!!
<FS>TImageEnIO.CloseAVIFile

<FM>Declaration<FC>
procedure CloseAVIFile;

<FM>Description<FN>
Closes an AVI file that was opened with <A TImageEnIO.OpenAVIFile> or <A TImageEnIO.CreateAVIFile>.

<FM>Example<FC>
// Save the first AVI frame to 'frame1.jpg', second frame to 'frame2.jpg'
ImageEnView1.IO.OpenAVIFile( 'C:\film.avi' );

ImageEnView1.IO.LoadFromAVI( 0 );
ImageEnView1.IO.SaveToFile('D:\frame1.jpg');

ImageEnView1.IO.LoadFromAVI( 1 );
ImageEnView1.IO.SaveToFile('D:\frame2.jpg');
...
ImageEnView1.IO.CloseAVIFile;
!!}
procedure TImageEnIO.CloseAVIFile;
begin
  if fAVI_avs1 <> nil then
  begin
    // save
    AVISaveOptionsFree(1, PAVICOMPRESSOPTIONS(fAVI_popts));
    freemem(fAVI_popts);
    if fAVI_avs <> nil then
      AVIStreamRelease(fAVI_avs);
    AVIStreamRelease(fAVI_avs1);
    AVIFileRelease(fAVI_avf);
    fAVI_popts := nil;
    fAVI_avs := nil;
    fAVI_avs1 := nil;
    fAVI_avf := nil;
  end
  else
  begin
    // load
    if fAVI_avf = nil then
      exit;
    AVIStreamGetFrameClose(fAVI_gf);
    AVIStreamRelease(fAVI_avs);
    AVIFileRelease(fAVI_avf);
    fAVI_avf := nil;
    fAVI_avs := nil;
    fAVI_gf := nil;
  end;
end;

{!!
<FS>TImageEnIO.IsOpenAVI

<FM>Declaration<FC>
function IsOpenAVI: Boolean;

<FM>Description<FN>
Returns True when <A TImageEnIO.OpenAVIFile> has a currently open file.

!!}
function TImageEnIO.IsOpenAVI: Boolean;
begin
  result := (fAVI_avf <> nil) and (fAVI_avs <> nil) and (fAVI_gf <> nil);
end;


{!!
<FS>TImageEnIO.LoadFromAVI

<FM>Declaration<FC>
function LoadFromAVI(FrameIndex: Integer): Boolean;

<FM>Description<FN>
Reads the frame of index, <FC>FrameIndex<FN>, opened with <A TImageEnIO.OpenAVIFile>. Result will be false if the file is not a recognized file type (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

<FM>Example<FC>
// Save first AVI frame to 'frame1.jpg', second frame to 'frame2.jpg' 
ImageEnView1.IO.OpenAVIFile( 'C:\film.avi' );

ImageEnView1.IO.LoadFromAVI( 0 );
ImageEnView1.IO.SaveToFile('D:\frame1.jpg');

ImageEnView1.IO.LoadFromAVI( 1 );
ImageEnView1.IO.SaveToFile('D:\frame2.jpg');
...
ImageEnView1.IO.CloseAVIFile;
!!}
function TImageEnIO.LoadFromAVI(FrameIndex: integer): Boolean;
var
  pt: pointer;
  bitcount: integer;
  dib: TIEDibBitmap;
begin
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveIndexRetBool(self, LoadFromAVI, FrameIndex);
    exit;
  end;

  try
    fAborting := false;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fAVI_avf = nil) or (fAVI_avs = nil) or (fAVI_gf = nil) then
      exit;
    if dword(FrameIndex) >= fAVI_psi.dwLength then
      exit;
    pt := AVIStreamGetFrame(fAVI_gf, FrameIndex);
    fParams.AVI_FrameCount := fAVI_psi.dwLength;
    fParams.fAVI_FrameDelayTime := 1000 / (fAVI_psi.dwRate/fAVI_psi.dwScale);
    dib := TIEDibBitmap.create;
    if pt <> nil then
    begin
      bitcount := _IECopyDIB2Bitmap2Ex(uint64(pt), dib, nil, true); // uses drawdibdraw
      fIEBitmap.CopyFromTDibBitmap(dib);
      case BitCount of
        1:
          begin
            fParams.BitsPerSample   := 1;
            fParams.SamplesPerPixel := 1;
          end;
        4:
          begin
            fParams.BitsPerSample   := 4;
            fParams.SamplesPerPixel := 1;
          end;
        8:
          begin
            fParams.BitsPerSample   := 8;
            fParams.SamplesPerPixel := 1;
          end;
        15:
          begin
            fParams.BitsPerSample   := 5;
            fParams.SamplesPerPixel := 3;
          end;
        16, 24, 32:
          begin
            fParams.BitsPerSample   := 8;
            fParams.SamplesPerPixel := 3;
          end;
      end;
      fParams.DpiX := IEGlobalSettings().DefaultDPIX;
      fParams.DpiY := IEGlobalSettings().DefaultDPIY;
      fParams.Width  := dib.Width;
      fParams.Height := dib.Height;
      fParams.OriginalWidth  := dib.Width;
      fParams.OriginalHeight := dib.Height;
      fParams.FreeColorMap;
      Update;            
      Result := Not fAborting;
    end;
    FreeAndNil(dib);
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.CreateAVIFile

<FM>Declaration<FC>
function CreateAVIFile(const FileName: WideString; rate: Integer; const codec: AnsiString): TIECreateAVIFileResult;

<FM>Description<FN>
Creates a new, empty AVI file.

<FM>FileName<FN>: Full destination path
<FM>Rate<FN>: Specifies the number of frames per second
<FM>Code<FN>: The compression codec to use (must be installed on system) as a four characters string.
For example:
'cvid' : cinepak by Radius
'msvc' : Microsoft Video 1
'mp42' : Microsoft MPEG4 V2

More codecs are listed at <L http://www.fourcc.org>www.fourcc.org</L> or by searching for registered fourcc codes and wave formats on <L http://msdn.microsoft.com>MSDN</L>

You can save each frame to the created AVI file using the <A TImageEnIO.SaveToAVI> method.

Finally, call <A TImageEnIO.CloseAVIFile> to close the file.

<FM>Example<FC>
ImageEnView.IO.CreateAVIFile('D:\output.avi', 20, 'DIB ');

// save frame 0
ImageEnView.IO.LoadFromFile('C:\frame0.jpg');
ImageEnView.IO.Params.BitsPerSample := 8;
ImageEnView.IO.Params.SamplesPerPixel := 1;
ImageEnView.IO.SaveToAVI;

// save frame 1
ImageEnView.IO.LoadFromFile('C:\frame1.jpg');
ImageEnView.IO.Params.BitsPerSample := 8;
ImageEnView.IO.Params.SamplesPerPixel := 1;
ImageEnView.IO.SaveToAVI;

// close the file
ImageEnView.IO.CloseAVIFile;
!!}
function TImageEnIO.CreateAVIFile(const FileName: WideString; rate: Double; const codec: AnsiString): TIECreateAVIFileResult;
var
  psi: TAviStreamInfoW;
  hr: HResult;
  num, den: integer;
begin
  fAborting := False;
  if not gAVIFILEinit then
  begin
    AVIFileInit;
    gAVIFILEinit := true;
  end;
  if IEFileExistsW(FileName) then
    DeleteFileW(PWideChar(FileName));
  if AVIFileOpenW(PAVIFILE(fAVI_avf), PWideChar(FileName), OF_WRITE or OF_CREATE, nil) <> 0 then
    raise EInvalidGraphic.Create('unable to create AVI file');
  ZeroMemory(@psi, sizeof(psi));
  psi.fccType := streamtypeVIDEO;
  IEDecimalToFraction(rate, num, den);
  psi.dwScale := den;
  psi.dwRate := num;
  psi.dwQuality := $FFFFFFFF;
  fAVI_popts := allocmem(sizeof(TAVICOMPRESSOPTIONS));
  if (fParams.BitsPerSample = 8) and (fParams.SamplesPerPixel = 1) then
    psi.dwSuggestedBufferSize := IEBitmapRowLen(fParams.Width, 8, 32) * fParams.Height
  else
    psi.dwSuggestedBufferSize := IEBitmapRowLen(fParams.Width, 24, 32) * fParams.Height;
  psi.rcFrame := rect(0, 0, fParams.Width, fParams.Height);
  AVIFileCreateStreamW(PAVIFILE(fAVI_avf), PAVISTREAM(fAVI_avs1), psi);

  if codec = '' then
  begin
    if not AVISaveOptions(0, 0, 1, @fAVI_avs1, @fAVI_popts) then
    begin
      AVIStreamRelease(fAVI_avs1);
      fAVI_avs1 := nil;
      AVIFileRelease(fAVI_avf);
      fAVI_avf := nil;
      freemem(fAVI_popts);
      fAborting := True;
      result := ieaviNOCOMPRESSOR;
      exit; // EXIT POINT
    end;
  end
  else
  begin
    PAVICOMPRESSOPTIONS(fAVI_popts)^.fccType := streamtypeVIDEO;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.fccHandler := pinteger(PAnsiChar(codec))^;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.dwKeyFrameEvery := 0;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.dwQuality := $FFFFFFFF;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.dwBytesPerSecond := 0;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.dwFlags := 0;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.lpFormat := nil;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.cbFormat := 0;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.lpParms := nil;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.cbParms := 0;
    PAVICOMPRESSOPTIONS(fAVI_popts)^.dwInterleaveEvery := 0;
  end;
  hr := AVIMakeCompressedStream(PAVISTREAM(fAVI_avs), PAVISTREAM(fAVI_avs1), fAVI_popts, nil);

  if hr = AVIERR_NOCOMPRESSOR then
    result := ieaviNOCOMPRESSOR
  else
  if hr = AVIERR_MEMORY then
    result := ieaviMEMORY
  else
  if hr = AVIERR_UNSUPPORTED then
    result := ieaviUNSUPPORTED
  else
    result := ieaviOK;

  fAborting := result <> ieaviOK;

  fAVI_idx := 0;
end;

{!!
<FS>TImageEnIO.SaveToAVI

<FM>Declaration<FC>
procedure SaveToAVI;

<FM>Description<FN>
Saves the current image to an AVI file opened using <A TImageEnIO.CreateAVIFile>.

It is recommended that you set <A TImageEnIO.Params>.<A TIOParamsVals.BitsPerSample> and <A TImageEnIO.Params>.<A TIOParamsVals.SamplesPerPixel> before calling <A TImageEnIO.SaveToAVI>, because AVI requires all images to have the same color depth.

<FM>Example<FC>
ImageEnView.IO.CreateAVIFile('output.avi', 20, 'DIB ');

// save frame 0
ImageEnView.IO.LoadFromFile('C:\frame0.jpg');
ImageEnView.IO.Params.BitsPerSample := 8;
ImageEnView.IO.Params.SamplesPerPixel := 1;
ImageEnView.IO.SaveToAVI();

// save frame 1
ImageEnView.IO.LoadFromFile('C:\frame1.jpg');
ImageEnView.IO.Params.BitsPerSample := 8;
ImageEnView.IO.Params.SamplesPerPixel := 1;
ImageEnView.IO.SaveToAVI();

// close the file
ImageEnView.IO.CloseAVIFile();
!!}                            
procedure TImageEnIO.SaveToAVI;
type
  TBitmapInfoHeader256Ex = packed record
    biSize: DWORD;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
    Palette: array[0..255] of TRGBQUAD;
  end;
  PBitmapInfoHeader256Ex = ^TBitmapInfoHeader256Ex;
var
  he: TBitmapInfoHeader256Ex;
  i: integer;
  sw, sx: integer;
  bmp: TIEBitmap;
begin

  if (fAVI_avs = nil) or (not MakeConsistentBitmap([])) then
    exit;
  if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
    fIEBitmap.PixelFormat := ie24RGB;

  bmp := TIEBitmap.Create();
  try
    bmp.Location := ieMemory;
    bmp.Assign(fIEBitmap);

    // set video format
    he.biSize := sizeof(TBitmapInfoHeader);
    he.biWidth := bmp.Width;
    he.biHeight := bmp.Height;
    he.biPlanes := 1;
    he.biCompression := BI_RGB;
    he.biXPelsPerMeter := 0;
    he.biYPelsPerMeter := 0;
    he.biClrImportant := 0;
    if (fParams.fSamplesPerPixel = 1) and (fParams.fBitsPerSample = 8) then
    begin
      // 256 colors
      he.biClrUsed := 256;
      bmp.PixelFormat := ie8p;
      he.biSizeImage := bmp.RowLen * bmp.Height;
      he.biBitCount := 8;
      for i := 0 to 255 do
      begin
        he.Palette[i].rgbRed   := bmp.Palette[i].r;
        he.Palette[i].rgbGreen := bmp.Palette[i].g;
        he.Palette[i].rgbBlue  := bmp.Palette[i].b;
        he.Palette[i].rgbReserved := 0;
      end;
      AVIStreamSetFormat(fAVI_avs, fAVI_idx, @he, sizeof(TBITMAPINFOHEADER) + 256 * sizeof(TRGBQUAD));
    end
    else
    if bmp.HasAlphaChannel then
    begin
      // 32bit RGBA
      bmp.PixelFormat := IE32RGB;
      bmp.SynchronizeRGBA(false); // alpha to RGBA
      he.biClrUsed := 0;
      he.biBitCount := 32;
      //he.biCompression := IEBI_RGBA;  // <- don't work with it!
      he.biSizeImage := bmp.RowLen * bmp.Height;
      if fAVI_idx = 0 then
        AVIStreamSetFormat(fAVI_avs, 0, @he, sizeof(TBITMAPINFOHEADER));
    end
    else
    begin
      // 24 bit
      he.biClrUsed := 0;
      he.biBitCount := 24;
      he.biSizeImage := bmp.RowLen * bmp.Height;
      if fAVI_idx = 0 then
        AVIStreamSetFormat(fAVI_avs, 0, @he, sizeof(TBITMAPINFOHEADER));
    end;
    sw := 0;
    sx := 0;
    AVIStreamWrite(fAVI_avs, fAVI_idx, 1, bmp.Scanline[bmp.Height - 1], he.biSizeImage, AVIIF_KEYFRAME, @sx, @sw);
  finally
    FreeAndNil(bmp);
  end;
  inc(fAVI_idx);
end;       





{!!
<FS>TImageEnIO.MergeMetaFile

<FM>Declaration<FC>
procedure MergeMetafile(const FileName: WideString; x, y, Width, Height: Integer);

<FM>Description<FN>
Loads a metafile and draws it at the x, y position using sizes of Width and Height.

If Width or Height is -1 the other is auto-calculated to maintain the aspect ratio.

<FM>Example<FC>
ImageEnView1.io.LoadFromFile('C:\backgroundimage.jpg');
ImageEnView1.io.MergeMetaFile('C:\overimage.wmf', 100, 100, 120, -1);

!!}
// If Width or Height is -1 then it is auto-calculated maintain aspect ratio
// Import... because this is a vectorial image
// Doesn't set IO params
// DON't USE INSIDE A THREAD
procedure TImageEnIO.MergeMetafile(const FileName: WideString; x, y, Width, Height: integer);
const
  MAXDIMS = 16000;
var
  meta: TMetafile;
  dib: TIEDibBitmap;
  dibcanvas: TCanvas;
begin
  try
    fAborting := false;
    if (Width = 0) or (Height = 0) then
      exit;
    if not MakeConsistentBitmap([]) then
      exit;
    meta := tmetafile.create;
    try
      meta.LoadFromFile(FileName);
      if (Width >= 0) or (Height >= 0) then
      begin
        if (Width >= 0) and (Height >= 0) then
        begin
          meta.Width := Width;
          meta.Height := Height;
        end
        else
        begin
          if Width < 0 then
          begin
            meta.Width := (meta.Width * Height) div meta.Height;
            meta.Height := Height;
          end
          else
          if Height < 0 then
          begin
            meta.Height := (meta.Height * Width) div meta.Width;
            meta.Width := Width;
          end;
        end;
      end;
      if (meta.width = 0) or (meta.height = 0) then
      begin
        fAborting := true;
        exit; // finally will be executed (meta.free)
      end;
      if (meta.width > MAXDIMS) or (meta.height > MAXDIMS) then
      begin
        if meta.Height > meta.Width then
        begin
          meta.Width := (meta.Width * MAXDIMS) div meta.Height;
          meta.Height := MAXDIMS;
        end
        else
        begin
          meta.Height := (meta.Height * MAXDIMS) div meta.Width;
          meta.Width := MAXDIMS;
        end;
      end;
      meta.transparent := true;
      dib := TIEDibBitmap.Create;
      dibcanvas := TCanvas.Create;
      dib.AllocateBits(meta.Width - 1, meta.Height - 1, 24);
      fIEBitmap.CopyToTDibBitmap(dib, x, y, dib.Width, dib.Height);
      dibcanvas.Handle := dib.HDC;
      dibCanvas.Draw(0, 0, meta);
      fIEBitmap.MergeFromTDibBitmap(dib, x, y);
      FreeAndNil(dibcanvas);
      FreeAndNil(dib);
    finally
      FreeAndNil(meta);
    end;
    update;
  finally
    DoFinishWork;
  end;
end;

// doesn't set fParams.fFileType
{$IFDEF IEINCLUDEJPEG2000}

function TImageEnIO.LoadFromStreamJ2000(Stream: TStream): Boolean;
var
  Progress: TProgressRec;
begin                 
  Result := False;
  fAborting := false;
  Progress.Aborting := @fAborting;
  if not MakeConsistentBitmap([]) then
    exit;
  fParams.ResetInfo;
  Progress.fOnProgress := fOnIntProgress;
  Progress.Sender := Self;
  fIEBitmap.RemoveAlphaChannel;
  J2KReadStream(Stream, fIEBitmap, fParams, Progress, False);
  CheckDPI;
  if fAutoAdjustDPI then
    AdjustDPI;
  fParams.fFileName := '';
  SetViewerDPI(fParams.DpiX, fParams.DpiY);  
  Result := Not fAborting;
end;
{$ENDIF}

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TImageEnIO.LoadFromStreamJP2

<FM>Declaration<FC>
function LoadFromStreamJP2(Stream: TStream): Boolean;

<FM>Description<FN>    
Loads an image from a stream containing a Jpeg2000 (JP2) file. The result will be false if an error is encountered, e.g. the file in the stream is not JPEG2000 format (<A TImageEnIO.Aborting> will be true).

!!}
function TImageEnIO.LoadFromStreamJP2(Stream: TStream): Boolean;
begin
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamJP2, Stream);
    Result := True;
    exit;
  end;

  try
    Result := LoadFromStreamJ2000(Stream);
    fParams.fFileType := ioJP2;
    update;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TImageEnIO.LoadFromStreamJ2K

<FM>Declaration<FC>
function LoadFromStreamJ2K(Stream: TStream): Boolean;

<FM>Description<FN>
Loads an image from a stream containing a Jpeg2000 (J2K code stream) file. The result will be false if an error is encountered, e.g. the file in the stream is not JPEG2000 format (<A TImageEnIO.Aborting> will be true).

!!}
function TImageEnIO.LoadFromStreamJ2K(Stream: TStream): Boolean;
begin
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamJ2K, Stream);
    Result := True;
    exit;
  end;

  try
    Result := LoadFromStreamJ2000(Stream);
    fParams.fFileType := ioJ2K;
    update;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TImageEnIO.LoadFromFileJ2K

<FM>Declaration<FC>
function LoadFromFileJ2K(const FileName: WideString): Boolean;

<FM>Description<FN>     
Loads an image from a Jpeg2000 (J2K code stream) file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not a JPEG2000 (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.
!!}
function TImageEnIO.LoadFromFileJ2K(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin          
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileJ2K, FileName);
    Result := True;
    exit;
  end;

  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    fAborting := false;
    try
      Result := LoadFromStreamJ2000(fs);
      fParams.fFileName := FileName;
      fParams.fFileType := ioJ2K;
    finally
      FreeAndNil(fs);
    end;
    update;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TImageEnIO.LoadFromFileJP2

<FM>Declaration<FC>
function LoadFromFileJP2(const FileName: WideString): Boolean;

<FM>Description<FN>         
Loads an image from a Jpeg2000 (JP2) file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not JPEG2000 format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.
!!}
function TImageEnIO.LoadFromFileJP2(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin            
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileJP2, FileName);
    Result := True;
    exit;
  end;

  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    fAborting := false;
    try
      Result := LoadFromStreamJ2000(fs);
      fParams.fFileName := FileName;
      fParams.fFileType := ioJP2;
    finally
      FreeAndNil(fs);
    end;
    update;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}


{$IFDEF IEINCLUDEJPEG2000}
procedure TImageEnIO.SaveToStreamJ2000(Stream: TStream; format: integer);
var
  Progress: TProgressRec;
begin
  fAborting := false;
  Progress.Aborting := @fAborting;
  if not MakeConsistentBitmap([ie1g, ie8g, ie16g, ie24RGB]) then
    exit;
  Progress.fOnProgress := fOnIntProgress;
  Progress.Sender := Self;
  J2KWriteStream(Stream, fIEBitmap, fParams, Progress, format);
end;
{$ENDIF}

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TImageEnIO.SaveToStreamJP2

<FM>Declaration<FC>
procedure SaveToStreamJP2(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in Jpeg2000 (JP2) format.
!!}
procedure TImageEnIO.SaveToStreamJP2(Stream: TStream);
begin                                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamJP2, Stream);
    exit;
  end;

  
  try
    SaveToStreamJ2000(Stream, 0);
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TImageEnIO.SaveToStreamJ2K

<FM>Declaration<FC>
procedure SaveToStreamJ2K(Stream: TStream);

<FM>Description<FN>                    
Saves the current image to a stream in Jpeg2000 (J2K code stream) format.
!!}
procedure TImageEnIO.SaveToStreamJ2K(stream: TStream);
begin               
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamJ2K, Stream);
    exit;
  end;

  try
    SaveToStreamJ2000(Stream, 1);
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TImageEnIO.SaveToFileJP2

<FM>Declaration<FC>
procedure SaveToFileJP2(const FileName: WideString);

<FM>Description<FN>           
Saves the current image to a file in Jpeg2000 (JP2) format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

!!}
procedure TImageEnIO.SaveToFileJP2(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin        
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileJP2, FileName);
    exit;
  end;

  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    try
      SaveToStreamJ2000(fs, 0);
      fParams.FileName := FileName;
      fParams.FileType := ioJP2;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{$IFDEF IEINCLUDEJPEG2000}

{!!
<FS>TImageEnIO.SaveToFileJ2K

<FM>Declaration<FC>
procedure SaveToFileJ2K(const FileName: WideString);

<FM>Description<FN>     
Saves the current image to a file in Jpeg2000 (J2K code stream) format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

!!}
procedure TImageEnIO.SaveToFileJ2K(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin              
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileJ2K, FileName);
    exit;
  end;

  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    try
      SaveToStreamJ2000(fs, 1);
      fParams.FileName := FileName;
      fParams.FileType := ioJ2K;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;
{$ENDIF}

{!!
<FS>TImageEnIO.IEBitmap

<FM>Declaration<FC>
property IEBitmap: <A TIEBitmap>

<FM>Description<FN>
Contains the attached <A TIEBitmap> object (same value as the <A TImageEnIO.AttachedIEBitmap> property).
!!}
procedure TImageEnIO.SetIEBitmap(bmp: TIEBitmap);
begin
  fBitmap := nil;
  if fIEBitmapCreated then
    FreeAndNil(fIEBitmap);
  fIEBitmapCreated := false;
  fIEBitmap := bmp;
end;

{!!
<FS>TImageEnIO.AttachedIEBitmap

<FM>Declaration<FC>
property AttachedIEBitmap: <A TIEBitmap>

<FM>Description<FN>
Contains the attached <A TIEBitmap> object (same value as the <A TImageEnIO.IEBitmap> property).

<FM>Example<FC>
// multithread saving
Var
  Bmp: TIEBitmap;
  Io: TImageEnIO;
Begin
  Bmp := TIEBitmap.Create;
  Io := TImageEnIO.CreateFromBitmap(bmp);

  Io.LoadFromFile('C:\input.bmp');
  Io.AsyncMode := True;  // this makes a thread foreach input/output task
  Io.SaveToFile('D:\i1.jpg');  // thread 1
  Io.SaveToFile('D:\i2.jpg');  // thread 2
  Io.SaveToFile('D:\i3.jpg');  // thread 3

  Io.Free;  // free method waits for all tasks end!
  Bmp.Free;
End;
!!}
procedure TImageEnIO.SetAttachedIEBitmap(bmp: TIEBitmap);
begin
  if assigned(fImageEnView) then
    fImageEnView.RemoveBitmapChangeEvent(fImageEnViewBitmapChangeHandle); // remove previous if exists
  if (not assigned(bmp)) and (assigned(fImageEnView) or assigned(fTImage)) then
    exit; // error
  SetIEBitmap(bmp);
  if assigned(bmp) then
  begin
    fImageEnView := nil;
    fTImage := nil;
  end;
end;

procedure TImageEnIO.SetBitmap(bmp: TBitmap);
begin
  fBitmap := bmp;
  fIEBitmap.EncapsulateTBitmap(fBitmap, true);
end;

{!!
<FS>TImageEnIO.AttachedBitmap

<FM>Declaration<FC>
property AttachedBitmap: TBitmap;

<FM>Description<FN>
Use this property to attach a TImageEnIO to a normal TBitmap object to provide wider format support and functionality.

This property is mutually exclusive with <A TImageEnIO.AttachedTImage> and <A TImageEnIO.AttachedImageEn>.

<FM>Example<FC>
var
  bmp: TBitmap;
Begin
  bmp := TBitmap.Create;
  ImageEnIO1.AttachedBitmap := bmp;
  ImageEnIO1.LoadFromFile('C:\alfa.tif');
  ...
End;
!!}
procedure TImageEnIO.SetAttachedBitmap(atBitmap: TBitmap);
begin
  if assigned(fImageEnView) then
    fImageEnView.RemoveBitmapChangeEvent(fImageEnViewBitmapChangeHandle); // remove previous if exists
  if (not assigned(atBitmap)) and (assigned(fImageEnView) or assigned(fTImage)) then
    exit; // error
  fBitmap := atBitmap;
  if assigned(fBitmap) then
    fIEBitmap.EncapsulateTBitmap(fBitmap, true);
  if assigned(fBitmap) then
  begin
    fImageEnView := nil;
    fTImage := nil;
  end;
end;



{!!
<FS>TImageEnIO.AttachedImageEn

<FM>Declaration<FC>
property AttachedImageEn: <A TIEView>;

<FM>Description<FN>
Use this property to attach a TImageEnIO to a <A TImageEnView>, <A TImageEnDBView>, <A TImageEnVect> or any other inherited component.

This property is mutually exclusive with <A TImageEnIO.AttachedTImage> and <A TImageEnIO.AttachedBitmap>.

Note: TIEView is the base class of <A TImageEnView> and <A TImageEnMView>.

<FM>Example<FC>
ImageEnIO1.AttachedImageEn := ImageEnView1;
!!}
procedure TImageEnIO.SetAttachedImageEn(atImageEn: TIEView);
begin
  if assigned(fImageEnView) then
    fImageEnView.RemoveBitmapChangeEvent(fImageEnViewBitmapChangeHandle); // remove previous if exists
  fImageEnView := atImageEn;
  if assigned(fImageEnView) then
  begin // fImageEnView now could be nil
    if fIEBitmapCreated then
    begin
      fIEBitmapCreated := false;
      FreeAndNil(fIEBitmap);
    end;
    fBitmap := fImageEnView.Bitmap;
    fIEBitmap := fImageEnView.IEBitmap;
    if assigned(fIEBitmap) then
      // use TIEBitmap
      fBitmap := nil // both fBitmap and fIEBitmap not allowed
    else
    begin
      // use TBitmap
      fIEBitmapCreated := true;
      fIEBitmap := TIEBitmap.Create;
      fIEBitmap.EncapsulateTBitmap(fBitmap, true);
    end;
    fImageEnView.FreeNotification(self);
    fTImage := nil;
    fImageEnViewBitmapChangeHandle := fImageEnView.RegisterBitmapChangeEvent(OnBitmapChange);
  end
  else
  begin
    fIEBitmap := TIEBitmap.Create;
    fIEBitmapCreated := true; // we create fIEBitmap
  end;
end;

{!!
<FS>TImageEnIO.AttachedTImage

<FM>Declaration<FC>
property AttachedTImage: TImage;

<FM>Description<FN>
Use this property to attach a TImageEnIO to a TImage (to widen its format support and functionality).

This property is mutually exclusive with <A TImageEnIO.AttachedImageEn> and <A TImageEnIO.AttachedBitmap>.

<FM>Example<FC>
ImageEnIO1.AttachedTImage := Image1;
!!}
procedure TImageEnIO.SetTImage(v: TImage);
begin
  if assigned(fImageEnView) then
    fImageEnView.RemoveBitmapChangeEvent(fImageEnViewBitmapChangeHandle); // remove previous if exists
  fTImage := v;
  if assigned(fTImage) then
  begin
    fBitmap := fTImage.Picture.Bitmap;
    fIEBitmap.EncapsulateTBitmap(fBitmap, true);
    fTImage.FreeNotification(self);
    fImageEnView := nil;
  end
  else
    fIEBitmap.FreeImage(true);
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEIOThread

{!!
<FS>TImageEnIO.AsyncRunning

<FM>Declaration<FC>
property AsyncRunning: Integer;

<FM>Description<FN>
Returns the number of threads that are running.

See also: <A TImageEnIO.AsyncMode>

<FM>Example<FC>
// multithread saving
Var
  Bmp: TIEBitmap;
  Io: TImageEnIO;
Begin
  Bmp := TIEBitmap.Create;
  Io := TImageEnIO.CreateFromBitmap(bmp);

  Io.LoadFromFile('C:\input.bmp');
  Io.AsyncMode := True;  // this makes a thread foreach input/output task
  Io.SaveToFile('D:\i1.jpg');  // thread-1
  Io.SaveToFile('D:\i2.jpg');  // thread-2
  Io.SaveToFile('D:\i3.jpg');  // thread-3

  // waits all threads finish
  Io.WaitThreads();
...
End;
!!}
function TImageEnIO.GetAsyncRunning: integer;
begin
  EnterCriticalSection(fAsyncThreadsCS);
  result := fAsyncThreads.Count;
  LeaveCriticalSection(fAsyncThreadsCS);
end;

procedure TIEIOThread.Init;
begin
  fThreadList := fOwner.fAsyncThreads;
  EnterCriticalSection(fOwner.fAsyncThreadsCS);
  fThreadList.Add(self);
  Windows.ResetEvent(fOwner.fAsyncThreadsFinishEvent);
  LeaveCriticalSection(fOwner.fAsyncThreadsCS);
  {$ifndef IEHASAFTERCONSTRUCTION}
  // this Delphi version hasn't AfterConstruction. We create threads in suspended mode, then resume
  Resume;
  {$endif}
end;

{$ifdef IEHASAFTERCONSTRUCTION}
const TIEIOTHREAD_STARTMODE = false;  // start in AfterConstruction (don't need Resume)
{$else}
const TIEIOTHREAD_STARTMODE = true;   // start suspended (need Resume)
{$endif}

constructor TIEIOThread.CreateLoadSaveFile(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFile; const in_nf: WideString);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveFile;
  fMethod_LoadSaveFile := InMethod;
  p_nf := in_nf;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveFileIF(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileIF; const in_nf: WideString; ImageFormat: TIOFileType);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveFileIF;
  fMethod_LoadSaveFileIF := InMethod;
  p_nf := in_nf;
  p_ImageFormat := ImageFormat;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveStream(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStream; in_Stream: TStream);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveStream;
  fMethod_LoadSaveStream := InMethod;
  p_stream := in_Stream;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveStreamRetInt(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStreamRetInt; in_Stream: TStream);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveStreamRetInt;
  fMethod_LoadSaveStreamRetInt := InMethod;
  p_stream := in_Stream;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveStreamRetBool(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStreamRetBool; in_Stream: TStream);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveStreamRetBool;
  fMethod_LoadSaveStreamRetBool := InMethod;
  p_stream := in_Stream;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveFileRetInt(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileRetInt; const in_nf: WideString);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveFileRetInt;
  fMethod_LoadSaveFileRetInt := InMethod;
  p_nf := in_nf;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveFileRetBool(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileRetBool; const in_nf: WideString);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveFileRetBool;
  fMethod_LoadSaveFileRetBool := InMethod;
  p_nf := in_nf;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveFileIFRetBool(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileIFRetBool; const in_nf: WideString; ImageFormat: TIOFileType);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveFileIFRetBool;
  fMethod_LoadSaveFileIFRetBool := InMethod;
  p_nf := in_nf;  
  p_ImageFormat := ImageFormat;
  Init;
end;


constructor TIEIOThread.CreateLoadSaveFileFFRetBool(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveFileFFRetBool; const in_nf: WideString; in_fileformat: TIOFileType);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveFileFFRetBool;
  fMethod_LoadSaveFileFFRetBool := InMethod;
  p_nf := in_nf;
  p_fileformat := in_fileformat;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveStreamFF(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStreamFF; in_Stream: TStream; in_fileformat: TIOFileType);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveStreamFF;
  fMethod_LoadSaveStreamFF := InMethod;
  p_stream := in_stream;
  p_fileformat := in_fileformat;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveStreamFTRetBool(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveStreamFTRetBool; in_Stream: TStream; in_fileformat: TIOFileType);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveStreamFTRetBool;
  fMethod_LoadSaveStreamFTRetBool := InMethod;
  p_stream := in_stream;
  p_fileformat := in_fileformat;
  Init;
end;

constructor TIEIOThread.CreateCaptureFromScreen(Owner: TImageEnIO; InMethod: TIEIOMethodType_CaptureFromScreen; in_source: TIECSSource; in_mouseCursor: TCursor);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieCaptureFromScreen;
  fMethod_CaptureFromScreen := InMethod;
  p_source := in_source;
  p_mouseCursor := in_mouseCursor;
  Init;
end;

constructor TIEIOThread.CreateLoadSaveIndexRetBool(Owner: TImageEnIO; InMethod: TIEIOMethodType_LoadSaveIndexRetBool; in_index: integer);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadSaveIndexRetBool;
  fMethod_LoadSaveIndexRetBool := InMethod;
  p_index := in_index;
  Init;
end;

constructor TIEIOThread.CreateImportMetaFileRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_ImportMetaFileRetBool; const in_nf: WideString; in_width, in_height: integer; in_withalpha: boolean);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieImportMetaFileRetBool;
  fMethod_ImportMetaFileRetBool := InMethod;
  p_nf := in_nf;
  p_width := in_width;
  p_height := in_height;
  p_withalpha := in_withalpha;
  Init;
end;

constructor TIEIOThread.CreateLoadFromURLRetBool(owner: TImageEnIO; InMethod: TIEIOMethodType_LoadFromURLRetBool; const in_URL: WideString; dummy: Double);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieLoadFromURLRetBool;
  fMethod_LoadFromURLRetBool := InMethod;
  p_URL := in_URL;
  Init;
end;

constructor TIEIOThread.CreateAcquire(owner: TImageEnIO; InMethod: TIEIOMethodType_Acquire; bResetParams: Boolean);
begin
  inherited Create(TIEIOTHREAD_STARTMODE);
  fOwner := Owner;
  fMethodType := ieAcquire;
  fMethod_Acquire := InMethod;
  p_ResetParams := bResetParams;
  Init;
end;

procedure TIEIOThread.Execute;
begin
  fThreadID := GetCurrentThreadID;
  try
    case fMethodType of
      ieLoadSaveFile              : fMethod_LoadSaveFile(p_nf);
      ieLoadSaveFileIF            : fMethod_LoadSaveFileIF(p_nf, p_ImageFormat);
      ieLoadSaveStream            : fMethod_LoadSaveStream(p_stream);
      ieLoadSaveFileRetInt        : fMethod_LoadSaveFileRetInt(p_nf);
      ieLoadSaveFileRetBool       : fMethod_LoadSaveFileRetBool(p_nf);
      ieLoadSaveFileIFRetBool     : fMethod_LoadSaveFileIFRetBool(p_nf,  p_ImageFormat);
      ieLoadSaveStreamRetInt      : fMethod_LoadSaveStreamRetInt(p_stream);
      ieLoadSaveStreamRetBool     : fMethod_LoadSaveStreamRetBool(p_stream);
      ieLoadSaveFileFFRetBool     : fMethod_LoadSaveFileFFRetBool(p_nf, p_fileformat);
      ieLoadSaveStreamFF          : fMethod_LoadSaveStreamFF(p_stream, p_FileFormat);
      ieLoadSaveStreamFTRetBool   : fMethod_LoadSaveStreamFTRetBool(p_stream, p_FileFormat);
      ieCaptureFromScreen         : fMethod_CaptureFromScreen(p_Source, p_mouseCursor);
      ieLoadSaveIndexRetBool      : fMethod_LoadSaveIndexRetBool(p_index);
      ieImportMetaFileRetBool     : fMethod_ImportMetaFileRetBool(p_nf, p_width, p_height, p_withalpha);
      ieLoadFromURLRetBool        : fMethod_LoadFromURLRetBool(p_URL);
      ieAcquire                   : fMethod_Acquire(p_ResetParams);
    end;
  except
    fOwner.Aborting := true;
  end;
  FreeOnTerminate := true;
  EnterCriticalSection(fOwner.fAsyncThreadsCS);
  fThreadList.Remove(self);
  if fThreadList.Count = 0 then
    Windows.SetEvent(fOwner.fAsyncThreadsFinishEvent);
  LeaveCriticalSection(fOwner.fAsyncThreadsCS);
end;



// end of TIEIOThread
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>ExtractTIFFImageStream

<FM>Declaration<FC>
procedure ExtractTIFFImageStream(SourceStream, OutStream: TStream; idx: Integer);

<FM>Description<FN>
Extracts the page, idx, (starting at 0) from SourceStream and saves it to OutStream.
It doesn't remove the page from source file; also it doesn't decompress the image resulting in a very quick process.

<FM>Example<FC>
// extract first page
fr := TFileStream.Create('input_multipage.tif', fmOpenRead);
fw := TFileStream.Create('page0.tif', fmCreate);
ExtractTIFFImageFile( fr , fw, 0 );
fw.free;
fr.free;
!!}
procedure ExtractTIFFImageStream(SourceStream, OutStream: TStream; idx: integer);
begin
  TIFFExtractImStream(SourceStream, idx, OutStream);
end;

{!!
<FS>ExtractTIFFImageFile

<FM>Declaration<FC>
procedure ExtractTIFFImageFile(const SourceFileName, OutFileName: WideString; idx: Integer);

<FM>Description<FN>
Extracts the page, idx, (starting at 0) from SourceFileName and saves it to OutFileName.
It doesn't remove the page from source file; also it doesn't decompress the image resulting in a very quick process.

<FM>Example<FC>
ExtractTIFFImageFile( 'input_multipage.tif' , 'page0.tif', 0 );  //  extract first page

!!}
procedure ExtractTIFFImageFile(const SourceFileName, OutFileName: WideString; idx: integer);
var
  SourceStream, OutStream: TIEWideFileStream;
begin
  SourceStream := TIEWideFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
  OutStream := nil;
  try
    OutStream := TIEWideFileStream.Create(OutFileName, fmCreate);
    TIFFExtractImStream(SourceStream, idx, OutStream);
  finally
    FreeAndNil(OutStream);
  end;
  FreeAndNil(SourceStream);
end;

{!!
<FS>InsertTIFFImageStream

<FM>Declaration<FC>
procedure InsertTIFFImageStream(SourceStream, InsertingStream, OutStream: TStream; idx: Integer);

<FM>Description<FN>
Inserts the TIFF stream <FC>InsertingStream<FN> into <FC>SourceStream<FN>, saving the result to OutStream. <FC>Idx<FN> is the page where to insert the file.

Note: Both TIFFs must have the same byte order. You can check it by reading the image parameters and checking <A TIOParamsVals.TIFF_ByteOrder> property.

It is a fast operation because both source and destination images are uncompressed.
!!}
procedure InsertTIFFImageStream(SourceStream, InsertingStream, OutStream: TStream; idx: integer);
begin
  TIFFInsertImStream(SourceStream, InsertingStream, idx, OutStream);
end;

{!!
<FS>InsertTIFFImageFile

<FM>Declaration<FC>
procedure InsertTIFFImageFile(const SourceFileName, InsertingFileName, OutFileName: WideString; idx: Integer);

<FM>Description<FN>
Inserts the file <FC>InsertingFileName<FN> into <FC>SourceFileName<FN>, saving the result to <FC>OutFileName<FN>. <FC>Idx<FN> is the page where to insert the file.

Note: Both TIFFs must have the same byte order. You can check it reading image parameters and checking <A TIOParamsVals.TIFF_ByteOrder> property.  

It is a fast operation because both sources and destination images are uncompressed.

<FM>Example<FC>
// this inserts pagetoinsert.tif in old.tif as first page and save all to new.tif
InsertingTIFFImageFile( 'old.tif' , 'pagetoinsert.tif', 'new.tif', 0 );
!!}
// InsertingFileName is the page to insert
procedure InsertTIFFImageFile(const SourceFileName, InsertingFileName, OutFileName: WideString; idx: integer);
var
  SourceStream, InsertingStream, OutStream: TIEWideFileStream;
begin
  SourceStream := TIEWideFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
  InsertingStream := nil;
  OutStream := nil;
  try
    InsertingStream := TIEWideFileStream.Create(InsertingFileName, fmOpenRead or fmShareDenyWrite);
    try
      OutStream := TIEWideFileStream.Create(OutFileName, fmCreate);
      TIFFInsertImStream(SourceStream, InsertingStream, idx, OutStream);
    finally
      FreeAndNil(OutStream);
    end;
  finally
    FreeAndNil(InsertingStream);
  end;
  FreeAndNil(SourceStream);
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// load from url

type
  HINTERNET = Pointer;
  INTERNET_PORT = Word;
  LPINTERNET_BUFFERS=^INTERNET_BUFFERS;
  INTERNET_BUFFERS = record
    dwStructSize: DWORD;
    Next: LPINTERNET_BUFFERS;
    lpcszHeader: PAnsiChar;
    dwHeadersLength: DWORD;
    dwHeadersTotal: DWORD;
    lpvBuffer: Pointer;
    dwBufferLength: DWORD;
    dwBufferTotal: DWORD;
    dwOffsetLow: DWORD;
    dwOffsetHigh: DWORD;
  end;


  TInternetCloseHandle = function(hInet: HINTERNET): BOOL; stdcall;
  TInternetOpen = function(lpszAgent: PAnsiChar; dwAccessType: DWORD; lpszProxy, lpszProxyBypass: PAnsiChar; dwFlags: DWORD): HINTERNET; stdcall;
  TInternetConnect = function(hInet: HINTERNET; lpszServerName: PAnsiChar; nServerPort: INTERNET_PORT; lpszUsername: PAnsiChar; lpszPassword: PAnsiChar; dwService: DWORD; dwFlags: DWORD; dwContext: DWORD): HINTERNET; stdcall;
  THttpOpenRequest = function(hConnect: HINTERNET; lpszVerb: PAnsiChar; lpszObjectName: PAnsiChar; lpszVersion: PAnsiChar; lpszReferrer: PAnsiChar; lplpszAcceptTypes: PAnsiChar; dwFlags: DWORD; dwContext: DWORD): HINTERNET; stdcall;
  THttpSendRequest = function(hRequest: HINTERNET; lpszHeaders: PAnsiChar; dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD): BOOL; stdcall;
  TInternetReadFile = function(hFile: HINTERNET; lpBuffer: Pointer; dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
  TInternetReadFileEx = function(hFile: HINTERNET; lpBuffersOut: LPINTERNET_BUFFERS; dwFlags: DWORD; dwContext: DWORD): BOOL; stdcall;
  THttpQueryInfo = function(hRequest: HINTERNET; dwInfoLevel: DWORD; lpvBuffer: Pointer; lpdwBufferLength: PDWORD; lpdwIndex: PDWORD): BOOL; stdcall;
  TInternetQueryOption = function(hInternet: HINTERNET; dwOption: DWORD; lpBuffer: Pointer; var lpdwBufferLength: DWORD): BOOL; stdcall;
  TInternetSetOption = function(hInternet: HINTERNET; dwOption: DWORD; lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
  TInternetGetLastResponseInfo = function(lpdwError: PDWORD; lpszBuffer: PAnsiChar; lpdwBufferLength: PDWORD): BOOL; stdcall;
  TFtpOpenFile = function(hConnect: HINTERNET; lpszFileName: PAnsiChar; dwAccess: DWORD; dwFlags: DWORD; dwContext: PDWORD): HINTERNET; stdcall;



const
  INTERNET_OPEN_TYPE_DIRECT                = 1;
  INTERNET_SERVICE_FTP                     = 1;
  INTERNET_SERVICE_HTTP                    = 3;
  INTERNET_OPEN_TYPE_PROXY                 = 3;
  INTERNET_FLAG_NO_CACHE_WRITE             = $04000000;
  INTERNET_FLAG_DONT_CACHE                 = INTERNET_FLAG_NO_CACHE_WRITE;
  INTERNET_FLAG_KEEP_CONNECTION            = $00400000;
  INTERNET_FLAG_RELOAD                     = $80000000;
  HTTP_QUERY_CONTENT_LENGTH                = 5;
  IRF_NO_WAIT                              = 8;
  INTERNET_FLAG_ASYNC                      = $10000000;
  INTERNET_FLAG_SECURE                     = $00800000;
  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP    = $00008000; // ex: https:// to http://
  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS   = $00004000; // ex: http:// to https://
  INTERNET_FLAG_IGNORE_CERT_DATE_INVALID   = $00002000; // expired X509 Cert.
  INTERNET_FLAG_IGNORE_CERT_CN_INVALID     = $00001000; // bad common name in X509 Cert.
  INTERNET_OPEN_TYPE_PRECONFIG             = 0;
  INTERNET_FLAG_NO_AUTH                    = $00040000;
  INTERNET_OPTION_SECURITY_FLAGS           = 31;
  SECURITY_FLAG_IGNORE_REVOCATION          = $00000080;
  SECURITY_FLAG_IGNORE_UNKNOWN_CA          = $00000100;
  SECURITY_FLAG_IGNORE_WRONG_USAGE         = $00000200;
  HTTP_QUERY_CONTENT_TYPE                  = 1;
  FTP_TRANSFER_TYPE_BINARY                 = $00000002;


var
  wininet: THandle;
  InternetOpen: TInternetOpen;
  InternetCloseHandle: TInternetCloseHandle;
  InternetConnect: TInternetConnect;
  HttpOpenRequest: THttpOpenRequest;
  HttpSendRequest: THttpSendRequest;
  HttpQueryInfo: THttpQueryInfo;
  InternetReadFile: TInternetReadFile;
  InternetReadFileEx: TInternetReadFileEx;
  InternetQueryOption: TInternetQueryOption;
  InternetSetOption: TInternetSetOption;
  InternetGetLastResponseInfo: TInternetGetLastResponseInfo;
  FtpOpenFile : TFtpOpenFile;

procedure IEInitWinINet;
begin
  if wininet = 0 then
  begin
    // try to load the wininet.dll dynamic library
    wininet := LoadLibrary('wininet.dll');
    if wininet <> 0 then
    begin
      InternetOpen := GetProcAddress(wininet, 'InternetOpenA');
      InternetCloseHandle := GetProcAddress(wininet, 'InternetCloseHandle');
      InternetConnect := GetProcAddress(wininet, 'InternetConnectA');
      HttpOpenRequest := GetProcAddress(wininet, 'HttpOpenRequestA');
      HttpSendRequest := GetProcAddress(wininet, 'HttpSendRequestA');
      HttpQueryInfo := GetProcAddress(wininet, 'HttpQueryInfoA');
      InternetReadFile := GetProcAddress(wininet, 'InternetReadFile');
      InternetReadFileEx := GetProcAddress(wininet, 'InternetReadFileExA');
      InternetQueryOption := GetProcAddress(wininet, 'InternetQueryOptionA');
      InternetSetOption := GetProcAddress(wininet, 'InternetSetOptionA');
      InternetGetLastResponseInfo := GetProcAddress(wininet, 'InternetGetLastResponseInfoA');
      FtpOpenFile := GetProcAddress(wininet, 'FtpOpenFileA');
     end;
  end;
end;

procedure IEFreeWinINet;
begin
  if wininet <> 0 then
  begin
    FreeLibrary(wininet);
    wininet := 0;
  end;
end;


// URL: 'http://domain[:port]/resource'  (example 'http://www.imageen.com/test.jpg' )
// URL: 'https://domain[:port]/resource'
// URL: 'ftp://user:password@domain[:port]/resource' (example 'ftp://user:password@ftp.imageen.com/Pictures/test.jpg')
// ProxyAddress: 'domain:port' (example '10.2.7.2:8080' )
function IEGetFromURL(const URL: WideString; OutStream: TStream; const ProxyAddress: WideString; const ProxyUser: WideString; const ProxyPassword: WideString; OnProgress: TIEProgressEvent; Sender: TObject; Aborting: pboolean; var FileExt: String): Boolean;
var
  hint: HINTERNET;
  hcon: HINTERNET;
  hreq: HINTERNET;
  buf: PAnsiChar;
  Host, Page: AnsiString;
  Port, i, j: integer;
  ib: INTERNET_BUFFERS;
  t1: dword;
  ConnType: TIEURLType;
  Service: dword;
  flags: cardinal;
  OptionsBuffer, OptionsBufferLen: DWORD;
  bx: cardinal;
  filetype: AnsiString;
  userid, password: AnsiString;
  AURL: AnsiString;

  procedure FreeHandles;
  begin
    if hreq <> nil then
      InternetCloseHandle(hreq);
    if hcon <> nil then
      InternetCloseHandle(hcon);
    if hint <> nil then
      internetCloseHandle(hint);
  end;

begin
  result := false;

  ConnType := IEGetURLTypeW(URL);
  AURL := AnsiString(URL);

  // remove protocol part and set service type
  case ConnType of
    ieurlHTTP:
      begin
        delete(AURL, 1, 7);
        Service := INTERNET_SERVICE_HTTP;
      end;
    ieurlHTTPS:
      begin
        delete(AURL, 1, 8);
        Service := INTERNET_SERVICE_HTTP;
      end;
    ieurlFTP:
      begin
        delete(AURL, 1, 6);
        Service := INTERNET_SERVICE_FTP;
      end;
    else
      exit;
  end;

  userid   := '';
  password := '';

  // extract userid/password, Host, Port and Resource (Page)
  i := IEPos('/', AURL);
  if i < 1 then
    exit;
  Host := IECopy(AURL, 1, i - 1);
  Page := IECopy(AURL, i, length(AURL));
  Port := 0;
  i := IEPos('@', Host);
  if i > 0 then
  begin
    // userid/password
    j := IEPos(':',Host);
    if j = 0 then
      exit;
    userid   := IECopy(Host, 1, j-1);
    password := IECopy(Host, j+1, i-j-1);
    Host     := IECopy(Host, i+1, length(Host));
  end;
  i := IEPos(':', Host);
  if i > 0 then
  begin
    Port := IEStrToIntDef(IECopy(Host, i + 1, length(Host)), 80);
    Host := IECopy(Host, 1, i - 1);
  end
  else
  begin
    // default port
    case ConnType of
      ieurlHTTP:  Port := 80;
      ieurlHTTPS: Port := 443;
      ieurlFTP:   Port := 21;
    end;
  end;

  hreq := nil;
  hcon := nil;
  hint := nil;
  buf := nil;

  try

    if ProxyAddress = '' then
    begin
      hint := InternetOpen('Mozilla/4.*', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
      hcon := InternetConnect(hint, PAnsiChar(AnsiString(Host)), Port, PAnsiChar(AnsiString(userid)), PAnsiChar(AnsiString(password)), Service, 0, 0);
    end
    else
    begin
      hint := InternetOpen('Mozilla/4.*', INTERNET_OPEN_TYPE_PROXY, PAnsiChar(AnsiString(ProxyAddress)), nil, 0);
      hcon := InternetConnect(hint, PAnsiChar(AnsiString(Host)), Port, PAnsiChar(AnsiString(ProxyUser)), PAnsiChar(AnsiString(ProxyPassword)), Service, 0, 0);
    end;

    if hcon = nil then
      exit;

    if (ConnType = ieurlHTTP) or (ConnType = ieurlHTTPS) then
    begin

      flags := INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD;
      if ConnType=ieurlHTTPS then
        flags := flags or INTERNET_FLAG_SECURE or INTERNET_FLAG_IGNORE_CERT_CN_INVALID or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP or
                        INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
      hreq := HttpOpenRequest(hcon, 'GET', PAnsiChar(AnsiString(Page)), 'HTTP/1.1', nil, nil, flags, 0);
      if hreq = nil then
        exit;

      OptionsBufferLen := sizeof(OptionsBuffer);
      InternetQueryOption(hreq, INTERNET_OPTION_SECURITY_FLAGS, @OptionsBuffer, OptionsBufferLen);
      OptionsBuffer := OptionsBuffer or SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_REVOCATION or SECURITY_FLAG_IGNORE_WRONG_USAGE;
      InternetSetOption(hreq, INTERNET_OPTION_SECURITY_FLAGS, @OptionsBuffer, sizeof(OptionsBuffer));

      getmem(buf, 65536);

      HttpSendRequest(hreq, nil, 0, nil, 0);

      // get file type
      fileExt := '';
      t1 := 65536;
      if HttpQueryInfo(hreq, HTTP_QUERY_CONTENT_TYPE, buf, @t1, nil) then
      begin
        PAnsiChar(buf)[t1] := #0;
        fileType := AnsiString(PAnsiChar(buf));
        t1 := IEPos('/', fileType);
        if t1 > 0 then
          fileExt := string(IECopy(fileType, t1+1, length(fileType)));
      end;

      // get file length
      t1 := 65536;
      if HttpQueryInfo(hreq, HTTP_QUERY_CONTENT_LENGTH, buf, @t1, nil) then
      begin
        PAnsiChar(buf)[t1] := #0;
        t1 := IEStrToIntDef(PAnsiChar(buf), 0);
      end
      else
        t1 := 0;
    end
    else
    if (ConnType=ieurlFTP) then
    begin
      Page := IECopy(Page, 2, length(Page));

      hreq := FtpOpenFile(hcon, PAnsiChar(Page), GENERIC_READ, FTP_TRANSFER_TYPE_BINARY, nil);
      if hreq = nil then
        exit;

      fileExt := string(IECopy(IEExtractFileExtA(Page), 2, 10));
      t1 := 0;
      getmem(buf, 65536);
    end;

    if t1 <> 0 then
    begin
      // we know the size, can load in async mode
      fillchar(ib, sizeof(ib), 0);
      ib.dwStructSize := sizeof(ib);
      repeat
        ib.lpvBuffer := buf;
        ib.dwBufferLength := 65536;
        if not InternetReadFileEx(hreq, @ib, IRF_NO_WAIT, 0) then
          break;
        if ib.dwBufferLength = 0 then
          break
        else
        begin
          OutStream.Write(pbyte(ib.lpvBuffer)^, ib.dwBufferLength);
          if assigned(OnProgress) then
            OnProgress(Sender, trunc(100 / t1 * OutStream.Size));
        end;
      until (dword(OutStream.Size)>=t1) or (assigned(Aborting) and Aborting^);
    end
    else
    begin
      // sync mode
      repeat
        InternetReadFile(hreq, buf, 1024, bx);
        OutStream.Write(buf^, bx);
      until (bx = 0) or (assigned(Aborting) and Aborting^);
    end;

  finally
    if buf <> nil then
      freemem(buf);
    FreeHandles;
  end;

  result := true;
end;


{!!
<FS>TImageEnIO.LoadFromURL

<FM>Declaration<FC>
function LoadFromURL(const URL: WideString): Boolean;

<FM>Description<FN>
Loads the image from the internet using the HTTP or FTP protocol.

Password authentication for HTTP is NOT supported, while it is necessary for FTP (even if connecting to an anonymous server).

<FC>URL<FN> must be in the format:
'http://domain[:port]/resource'
'https://domain[:port]/resource'
'ftp://user:password@domain[:port]/resource'

It is possible to set proxy parameters using <A TImageEnIO.ProxyAddress>, <A TImageEnIO.ProxyUser> and <A TImageEnIO.ProxyPassword> properties.

<FM>Example<FC>
// load from standard port 80
ImageEnView.IO.LoadFromURL('http://www.imageen.com/image.jpg');

// load from port 8080
ImageEnView.IO.LoadFromURL('http://www.imageen.com:8080/image.jpg');

// load from FTP
ImageEnView.IO.LoadFromURL('ftp://space:shuttle@ftp.imageen.com/Pictures/test.jpg')
!!}
function TImageEnIO.LoadFromURL(const URL: WideString): Boolean;
var
  ms: TMemoryStream;
  FileExt: string;
begin
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadFromURLRetBool(self, LoadFromURL, URL, 0);
    Result := True;
    exit;
  end;

  fAborting := false;

  ms := TMemoryStream.Create;
  try
    if not IEGetFromURL(URL, ms, fProxyAddress, fProxyUser, fProxyPassword, fOnIntProgress, self, @fAborting, FileExt) then
    begin
      fAborting := true;
      DoFinishWork;
    end
    else
    begin
      ms.Position := 0;
      Result := LoadFromStream(ms); // this handles other tasks, such as IEBitmap sync, OnFinishWork, Update
    end;
  finally
    FreeAndNil(ms);
  end;
end;

// load from url
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////


procedure TImageEnIO.DoAcquireBitmap(ABitmap: TIEBitmap; var Handled: boolean);
begin
  if assigned(fOnAcquireBitmap) then
    fOnAcquireBitmap(self, ABitmap, Handled);
end;

procedure TImageEnIO.TWMultiCallBack(Bitmap: TIEBitmap; var IOParams: TObject; ImDpiX, ImDpiY: integer);
var
  bHandled: boolean; // Flag indicating whether the user has handled the acquired bitmap themselves
begin
  if not MakeConsistentBitmap([]) then
    exit;
  bHandled := false;
  DoAcquireBitmap(Bitmap, bHandled);
  if not bHandled then
  begin
    IOParams := fParams;
    fIEBitmap.Assign(Bitmap);
    if fAutoAdjustDPI then
      AdjustDPI;
    Update;
  end;
end;

procedure TImageEnIO.TWCloseCallBack;
begin
  fgrec := nil;
  if assigned(fOnAcquireClose) then
    fOnAcquireClose(self);
end;

{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnIO.TwainAcquireOpen

<FM>Declaration<FC>
function TwainAcquireOpen : boolean;

<FM>Description<FN>
Opens a connection to the selected scanner. It commonly used to perform a modeless acquisition from a Twain device. The result is false if it fails.

Whenever ImageEn gets an image, the <A TImageEnIO.OnAcquireBitmap> event occurs.

See also: <A TImageEnIO.TwainAcquireClose>

<FM>Example<FC>
ImageEnIO.TwainAcquireOpen;
...
ImageEnIO.TwainAcquireClose;

!!}
function TImageEnIO.TwainAcquireOpen: boolean;
begin
  if (not assigned(fgrec)) and assigned(fImageEnView) then
  begin
    fAborting := false;
    fTwainParams.FreeResources;
    fTwainParams.LastError := 0;
    fTwainParams.LastErrorStr := '';
    fgrec := IETWAINAcquireOpen(TWCloseCallBack, TWMultiCallBack, fTwainParams, @fTwainParams.TwainShared, fParams, fImageEnView, NativePixelFormat);
    result := fgrec <> nil;
  end
  else
    result := false;
end;

// Legacy interface to TwainAcquireOpen
function TImageEnIO.AcquireOpen: boolean;
begin
  result := TwainAcquireOpen;
end;

{$ENDIF}

{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnIO.TwainAcquireClose

<FM>Declaration<FC>
procedure TwainAcquireClose;

<FM>Description<FN>
Closes a connection to a Twain device, which was opened with <A TImageEnIO.TwainAcquireOpen>.

<FM>Example<FC>
ImageEnIO.TwainAcquireOpen;
...
ImageEnIO.TwainAcquireClose;

!!}
procedure TImageEnIO.TwainAcquireClose;
begin
  if fgrec <> nil then
  begin
    IETWAINAcquireClose(fgrec);
    fgrec := nil;
  end;
end;

// Legacy interface to TwainAcquireClose
procedure TImageEnIO.AcquireClose; 
begin
  TwainAcquireClose;
end;
{$ENDIF}


////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnIO.ThreadsCount

<FM>Declaration<FC>
property ThreadsCount: Integer;

<FM>Description<FN>
Returns the count of currently active threads. Valid only if <A TImageEnIO.AsyncMode> is True.

!!}
function TImageEnIO.GetThreadsCount: integer;
begin
  EnterCriticalSection(fAsyncThreadsCS);
  result := fAsyncThreads.Count;
  LeaveCriticalSection(fAsyncThreadsCS);
end;

{!!
<FS>TImageEnIO.DefaultDitherMethod

<FM>Declaration<FC>
property DefaultDitherMethod: <A TIEDitherMethod>;

<FM>Description<FN>
Specifies the dithering method to apply when a color image needs to be converted to black/white.

Default: <FC>ieThreshold<FN>
!!}
procedure TImageEnIO.SetDefaultDitherMethod(Value: TIEDitherMethod);
begin
  if assigned(fIEBitmap) then
    fIEBitmap.DefaultDitherMethod := Value;
  fDefaultDitherMethod := Value;
end;

{!!
<FS>TImageEnIO.Bitmap

<FM>Declaration<FC>
property Bitmap: TBitmap; (Read-only)

<FM>Description<FN>
References the bitmap contained in TImageEnIO, <A TImageEnView> or <A TImageEnMView>.

<FM>Example<FC>
// Use the standard LoadFromFile method of TBitmap
ImageEnView1.IO.Bitmap.LoadFromFile('C:\my.bmp');
ImageEnView1.IO.Bitmap.PixelFormat := pf24bit;
ImageEnView1.Update;
!!}
function TImageEnIO.GetBitmap: TBitmap;
begin
  if assigned(fIEBitmap) and (fIEBitmap.Location = ieTBitmap) then
    result := fIEBitmap.VclBitmap
  else
    result := fBitmap;
end;

{!!
<FS>TImageEnIO.SaveToStreamICO

<FM>Declaration<FC>
procedure SaveToStreamICO(Stream: TStream);

<FM>Description<FN>
Saves the current image to a stream in ICO format. ICO format allows storing multiple images with several resolutions and sizes.
ImageEn creates for you the sub-images to save; you have only to set the <A TImageEnIO.Params>.<A TIOParamsVals.ICO_BitCount> and Params.<A TIOParamsVals.ICO_Sizes> arrays.

To control each image separately, use the <A IEWriteICOImages> function.

<FM>Example<FC>
// save the current image in 'output.ico', It will contain three images with 64x64 32bit (24bit +alphachannel), 32x32 256 colors and 32x32 16 colors

// 64x64 x32bit
ImageEnView.IO.Params.ICO.BitCount[0] := 32;
ImageEnView.IO.Params.ICO.Sizes[0].cx := 64;
ImageEnView.IO.Params.ICO.Sizes[0].cy := 64;

// 32x32 x8bit
ImageEnView.IO.Params.ICO.BitCount[1] := 8;
ImageEnView.IO.Params.ICO.Sizes[1].cx := 32;
ImageEnView.IO.Params.ICO.Sizes[1].cy := 32;

// 32x32 x4bit
ImageEnView.IO.Params.ICO.BitCount[2] := 4;
ImageEnView.IO.Params.ICO.Sizes[2].cx := 32;
ImageEnView.IO.Params.ICO.Sizes[2].cy := 32;

// I don't want other images
ImageEnView.IO.Params.ICO.BitCount[3] := 0;
ImageEnView.IO.Params.ICO.Sizes[3].cx := 0;
ImageEnView.IO.Params.ICO.Sizes[3].cy := 0;

// save
ImageEnView.IO.SaveToFile('D:\output.ico');

!!}
procedure TImageEnIO.SaveToStreamICO(Stream: TStream);
var
  Progress: TProgressRec;
  icount: integer;
begin        
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamICO, Stream);
    exit;
  end;

  
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    icount := 0;
    while (icount <= high(Params.ICO_Sizes)) and (Params.ICO_Sizes[icount].cx > 0) do
      inc(icount);
    ICOWriteStream(Stream, fIEBitmap, fParams, Progress, slice(Params.ICO_Sizes, icount), slice(Params.ICO_BitCount, icount));
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToFileICO

<FM>Declaration<FC>
procedure SaveToFileICO(const FileName: WideString);

<FM>Description<FN>               
Saves the current image to a file in ICO format. ICO format allows storing multiple images with different resolutions and sizes.

<FC>FileName<FN> is the file name including extension.

ImageEn creates for you the sub-images to save, you have only to set the <A TImageEnIO.Params>.<A TIOParamsVals.ICO_BitCount> and Params.<A TIOParamsVals.ICO_Sizes> arrays.

To control each image separately, use the <A IEWriteICOImages> function.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

<FM>Example<FC>
// save the current image in 'output.ico', It will contain three images with 64x64 32bit (24bit +alphachannel), 32x32 256 colors and 32x32 16 colors

// 64x64 x32bit
ImageEnView.IO.Params.ICO.BitCount[0] := 32;
ImageEnView.IO.Params.ICO.Sizes[0].cx := 64;
ImageEnView.IO.Params.ICO.Sizes[0].cy := 64;

// 32x32 x8bit
ImageEnView.IO.Params.ICO.BitCount[1] := 8;
ImageEnView.IO.Params.ICO.Sizes[1].cx := 32;
ImageEnView.IO.Params.ICO.Sizes[1].cy := 32;

// 32x32 x4bit
ImageEnView.IO.Params.ICO.BitCount[2] := 4;
ImageEnView.IO.Params.ICO.Sizes[2].cx := 32;
ImageEnView.IO.Params.ICO.Sizes[2].cy := 32;

// I don't want other images
ImageEnView.IO.Params.ICO.BitCount[3] := 0;
ImageEnView.IO.Params.ICO.Sizes[3].cx := 0;
ImageEnView.IO.Params.ICO.Sizes[3].cy := 0;

// save
ImageEnView.IO.SaveToFile('D:\output.ico');
!!}
procedure TImageEnIO.SaveToFileICO(const FileName: WideString);
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
  icount: integer;
begin                 
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileICO, FileName);
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      icount := 0;
      while (icount <= high(Params.ICO_Sizes)) and (Params.ICO_Sizes[icount].cx > 0) do
        inc(icount);
      ICOWriteStream(fs, fIEBitmap, fParams, Progress, slice(Params.ICO_Sizes, icount), slice(Params.ICO_BitCount, icount));
      fParams.FileName := FileName;
      fParams.FileType := ioICO;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;



{!!
<FS>IEWriteICOImages

<FM>Declaration<FC>
procedure IEWriteICOImages(const fileName: WideString; images: array of TObject);

<FM>Description<FN>
Provides an alternate method to save ICO files (instead of using <A TImageEnIO.SaveToFile> or <A TImageEnIO.SaveToFileICO>). It allows you to specify the origin of each frame of ICO to be saved.

IEWriteICOImages doesn't look at <A TIOParamsVals.ICO_Sizes> and <A TIOParamsVals.ICO_BitCount>, but only <A TImageEnView>.<A TImageEnView.IO>.<A TImageEnIO.Params>.<A TIOParamsVals.BitsPerSample> and TImageEnView.IO.Params.<A TIOParamsVals.SamplesPerPixel>.

<FM>Example<FC>
// Suppose we have three images, each in a TImageEnView component (e.g. ImageEnView1, ImageEnView2, ImageEnView3)
// we want to create an icon of these three images at 32 bit, 8 bit and 4 bit.

// 32 bit (24 for colors and 8 for alpha channel)
ImageEnView1.IO.Params.BitsPerSample := 4;
ImageEnView1.IO.Params.SamplesPerPixel := 3;

// 8 bit (256 colors)
ImageEnView2.IO.Params.BitsPerSample := 8;
ImageEnView2.IO.Params.SamplesPerPixel := 1;

// 4 bit (16 colors)
ImageEnView3.IO.Params.BitsPerSample := 4;
ImageEnView3.IO.Params.SamplesPerPixel := 1;

// save all inside a single ICO
IEWriteICOImages('output.ico', [ImageEnView1, ImageEnView2, ImageEnView3]);

!!}
procedure IEWriteICOImages(const fileName: WideString; images: array of TObject);
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
  fAborting: boolean;
begin
  fAborting := false;
  Progress.Aborting := @fAborting;
  Progress.fOnProgress := nil;
  Progress.Sender := nil;
  fs := TIEWideFileStream.Create(fileName, fmCreate);
  try
    ICOWriteStream2(fs, images, Progress);
  finally
    FreeAndNil(fs);
  end;
end;

{$IFDEF IEINCLUDEIEXACQUIRE}

{!!
<FS>TImageEnIO.WIAParams

<FM>Declaration<FC>
property WIAParams: <A TIEWia>;

<FM>Description<FN>
Provides access to WIA parameters and allows you to show dialogs and control WIA devices.

Note: Use WIAParams only when you need access to WIA specific functionality (when <A TImageEnIO.SelectedAcquireSource>.Api is ieaWIA). For generic access to all image acquisitions sources (Twain, WIA, etc.) use <A TImageEnIO.AcquireParams> instead.

<FM>Examples<FC>
// select first device, set black/white (with threshold) and capture
ImageEnView.IO.WIAParams.SetItemProperty(WIA_IPA_DATATYPE, WIA_DATA_THRESHOLD, nil);
if ImageEnView1.IO.SetSource(ieaWIA, Default_Device) then
  ImageEnView1.IO.Acquire;

// set dpi = 150 then capture
ImageEnView.IO.WIAParams.SetItemProperty(WIA_IPS_XRES, 150, nil);
ImageEnView.IO.WIAParams.SetItemProperty(WIA_IPS_YRES, 150, nil);
if ImageEnView1.IO.SetSource(ieaWIA, Default_Device) then
  ImageEnView1.IO.Acquire;

<FM>See Also<FN>
- <A TIEWia>
- <A TImageEnIO.AcquireParams>
- <A TImageEnIO.SelectedAcquireSource>

!!}
function TImageEnIO.GetWIAParams: TIEWia;
begin
  if not assigned(fWIA) then
  begin
    fWIA := TIEWia.Create(self);
    fWIA.OnProgress := WiaOnProgress;
  end;
  result := fWIA;
end;

function TImageEnIO.WiaOnProgress(Percentage: integer): boolean;
begin
  fOnIntProgress(self, Percentage); // fOnIntProgress is always assigned
  result := not fAborting;
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnIO.LoadFromFileWBMP

<FM>Declaration<FC>
function LoadFromFileWBMP(const FileName: WideString): Boolean;

<FM>Description<FN>       
Loads an image from a WBMP (Wireless bitmap) file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not WBMP format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.
!!}
function TImageEnIO.LoadFromFileWBMP(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
  Progress: TProgressRec;
begin                
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileWBMP, FileName);
    Result := True;
    exit;
  end;

  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      fIEBitmap.RemoveAlphaChannel;
      WBMPReadStream(fs, fIEBitmap, fParams, Progress, false);
      CheckDPI;
      fParams.fFileName := FileName;
      fParams.fFileType := ioWBMP;
      update;
    finally
      FreeAndNil(fs);  
      Result := Not fAborting;
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamWBMP

<FM>Declaration<FC>
function LoadFromStreamWBMP(Stream: TStream): Boolean;

<FM>Description<FN>                
Loads an image from a stream containing a WBMP (Wireless bitmap) file. The result will be false if an error is encountered, e.g. the file in the stream is not WBMP format (<A TImageEnIO.Aborting> will be true).

!!}
function TImageEnIO.LoadFromStreamWBMP(Stream: TStream): Boolean;
var
  Progress: TProgressRec;
begin            
  Result := False;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamWBMP, Stream);
    Result := True;
    exit;
  end;

  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    WBMPReadStream(Stream, fIEBitmap, fParams, Progress, false);
    CheckDPI;
    fParams.fFileName := '';
    fParams.fFileType := ioWBMP;
    update;
    Result := Not fAborting;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToFileWBMP

<FM>Declaration<FC>
procedure SaveToFileWBMP(const FileName: WideString);

<FM>Description<FN>      
Saves the current image to a file in WBMP (Wireless bitmap) format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

!!}
procedure TImageEnIO.SaveToFileWBMP(const FileName: WideString);
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
begin                                           
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileWBMP, FileName);
    exit;
  end;

  
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    fAborting := false;
    try
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      WBMPWriteStream(fs, fIEBitmap, fParams, Progress);
      fParams.FileName := FileName;
      fParams.FileType := ioWBMP;
    finally
      FreeAndNil(fs);
    end;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamWBMP

<FM>Declaration<FC>
procedure SaveToStreamWBMP(Stream: TStream);

<FM>Description<FN>                    
Saves the current image to a stream in WBMP (Wireless bitmap) format.
!!}
procedure TImageEnIO.SaveToStreamWBMP(Stream: TStream);
var
  Progress: TProgressRec;
begin
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamWBMP, Stream);
    exit;
  end;

  
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    WBMPWriteStream(Stream, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// CMYK/RGB conversions


var
  CMYKProfile: TIEICC;
  SRGBProfile: TIEICC;

function InitCMYKConversion: Boolean;
begin
  result := True;
  if CMYKProfile = nil then
  begin
    CMYKProfile := TIEICC.Create(false);
    CMYKProfile.Assign_CMYKProfile();
    if CMYKProfile.IsValid then
    begin
      SRGBProfile := TIEICC.Create(false);
      SRGBProfile.Assign_sRGBProfile();
      CMYKProfile.InitTransform(SRGBProfile, integer(iecmsCMYK), integer(iecmsBGR), 0, 0);
      SRGBProfile.InitTransform(CMYKProfile, integer(iecmsBGR), integer(iecmsCMYK), 0, 0);
    end
    else
      result := False;
  end;
end;

// CMYK to RGB
function IECMYK2RGB(cmyk: TCMYK): TRGB;
var
  src: TIECMSCOLOR;
  dst: TIECMSCOLOR;
begin
  if IEGlobalSettings().UseCMYKProfile and InitCMYKConversion then
  begin
    src.cmyk.cyan    := (255 - cmyk.c) * 257;
    src.cmyk.magenta := (255 - cmyk.m) * 257;
    src.cmyk.yellow  := (255 - cmyk.y) * 257;
    src.cmyk.black   := (255 - cmyk.k) * 257;
    IE_TranslateColors(CMYKProfile.MSTransform, @src, 1, IE_CS2IF[integer(iecmsCMYK)], @dst, IE_CS2IF[integer(iecmsBGR)]);
    result.r := dst.rgb.red shr 8;
    result.g := dst.rgb.green shr 8;
    result.b := dst.rgb.blue shr 8;
  end
  else
  begin
    with cmyk, result do
    begin
      r := k * c div 255;
      g := k * m div 255;
      b := k * y div 255;
    end;
  end;
end;


// colorProfile must be initialized with the destination profile using TIEICC.InitTransform call
function IECMYK2RGBROW(inrow: PCMYK; outrow: PRGB; width: Integer; alphaRow: pinteger; colorProfile: TIEICC): TRGB;
var
  src, buf_src: PIECMSCOLOR;
  dst, buf_dst: PIECMSCOLOR;
  i: integer;
begin
  getmem(buf_src, sizeof(TIECMSCOLOR) * width);
  src := buf_src;
  for i := 1 to width do
  begin
    src^.cmyk.cyan    := (255 - inrow^.c) * 257;
    src^.cmyk.magenta := (255 - inrow^.m) * 257;
    src^.cmyk.yellow  := (255 - inrow^.y) * 257;
    src^.cmyk.black   := (255 - inrow^.k) * 257;
    inc(src);
    inc(inrow);
  end;
  getmem(buf_dst, sizeof(TIECMSCOLOR) * width);
  dst := buf_dst;
  IE_TranslateColors(colorProfile.MSTransform, buf_src, width, IE_CS2IF[integer(iecmsCMYK)], buf_dst, IE_CS2IF[integer(iecmsBGR)]);
  if alphaRow = nil then
  begin
    // no alpha channel
    for i := 1 to width do
    begin
      outrow^.r := dst^.rgb.red shr 8;
      outrow^.g := dst^.rgb.green shr 8;
      outrow^.b := dst^.rgb.blue shr 8;
      inc(dst);
      inc(outrow);
    end;
  end
  else
  begin
    // with alpha channel
    for i := 1 to width do
    begin
      outrow^.r := alphaRow^ * (dst^.rgb.red shr 8 - outrow^.r) shr 18 + outrow^.r;
      outrow^.g := alphaRow^ * (dst^.rgb.green shr 8 - outrow^.g) shr 18 + outrow^.g;
      outrow^.b := alphaRow^ * (dst^.rgb.blue shr 8 - outrow^.b) shr 18 + outrow^.b;
      inc(dst);
      inc(outrow);
      inc(alphaRow);
    end;
  end;
  freemem(buf_dst);
  freemem(buf_src);
end;



// CMYK to RGB, a whole row
// alpha is shift-left 10
function IECMYK2RGBROW(inrow: PCMYK; outrow: PRGB; width: Integer; alphaRow: pinteger = nil): TRGB;
var
  i: integer;
begin
  if IEGlobalSettings().UseCMYKProfile and InitCMYKConversion then
  begin
    IECMYK2RGBROW(inrow, outrow, width, alphaRow, CMYKProfile);
  end
  else
  begin
    if alphaRow = nil then
    begin
      // no alpha channel
      for i := 1 to width do
      begin
        outrow^.r := inrow^.k * inrow^.c div 255;
        outrow^.g := inrow^.k * inrow^.m div 255;
        outrow^.b := inrow^.k * inrow^.y div 255;
        inc(inrow);
        inc(outrow);
      end;
    end
    else
    begin
      // with alpha channel
      for i := 1 to width do
      begin
        outrow^.r := alphaRow^ * (inrow^.k * inrow^.c div 255 - outrow^.r) shr 18 + outrow^.r;
        outrow^.g := alphaRow^ * (inrow^.k * inrow^.m div 255 - outrow^.g) shr 18 + outrow^.g;
        outrow^.b := alphaRow^ * (inrow^.k * inrow^.y div 255 - outrow^.b) shr 18 + outrow^.b;
        inc(inrow);
        inc(outrow);
        inc(alphaRow);
      end;
    end;
  end;
end;


// RGB to CMYK
function IERGB2CMYK(const rgb: TRGB): TCMYK;
var
  src: TIECMSCOLOR;
  dst: TIECMSCOLOR;
begin
  if IEGlobalSettings().UseCMYKProfile and InitCMYKConversion then
  begin
    src.rgb.red   := rgb.r * 257;
    src.rgb.green := rgb.g * 257;
    src.rgb.blue  := rgb.b * 257;
    IE_TranslateColors(SRGBProfile.MSTransform, @src, 1, IE_CS2IF[integer(iecmsBGR)], @dst, IE_CS2IF[integer(iecmsCMYK)]);
    result.c := 255 - dst.cmyk.cyan shr 8;
    result.m := 255 - dst.cmyk.magenta shr 8;
    result.y := 255 - dst.cmyk.yellow shr 8;
    result.k := 255 - dst.cmyk.black shr 8;
  end
  else
    with rgb, result do
    begin
      K := imax(r, imax(g, b));
      if K = 0 then
      begin
        C := 255;
        M := 255;
        Y := 255;
      end
      else
      begin
        C := r * 255 div K;
        M := g * 255 div K;
        Y := b * 255 div K;
      end;
    end;
end;

// end of CMYK/RGB conversions
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


procedure IEDefaultConvertColorFunction(InputScanline: pointer; InputColorSpace: TIEColorSpace; OutputScanline: pointer; OutputColorSpace: TIEColorSpace; ImageWidth: integer; IOParams: TIOParamsVals);
{$ifdef IEINCLUDECMS}
const
  CCTOCMS: array [iecmsRGB..iecmsYCBCR] of integer = (TYPE_RGB_8, TYPE_BGR_8, TYPE_CMYK_8_REV, TYPE_CMYKcm_8, TYPE_Lab_8, TYPE_GRAY_8, TYPE_RGB_16, TYPE_RGB_16_SE, TYPE_YCbCr_8);
{$endif}
var
  xlab: PCIELAB;
  xcmyk: PCMYK;
  xbgr: PRGB;
  xgray8: pbyte;
  xrgb48: PRGB48;
  xycbcr: PYCBCR;
  i: integer;
  InputProfile: TIEICC;
  RedToGrayCoef, GreenToGrayCoef, BlueToGrayCoef: integer;
begin
  InputProfile := nil;
  if assigned(IOParams) then
  begin
    if assigned(IOParams.fDefaultICC) and ((IOParams.fInputICC = nil) or (not IOParams.fInputICC.IsValid))  then
      InputProfile := IOParams.fDefaultICC
    else
      InputProfile := IOParams.fInputICC;
  end;
  {$ifdef IEINCLUDECMS}
  if IEGlobalSettings().EnableCMS and (IOParams <> nil) and assigned(InputProfile) and InputProfile.IsValid and not InputProfile.IsApplied and IOParams.OutputICCProfile.IsValid then
  begin
    // use CMS
    if InputProfile.Transform(IOParams.OutputICCProfile, CCTOCMS[InputColorSpace], CCTOCMS[OutputColorSpace], INTENT_PERCEPTUAL, 0 , InputScanline, OutputScanline, ImageWidth) then
      exit; // sucessful
  end;
  {$else}
  // use mscms
  if IEGlobalSettings().EnableCMS and
     (IOParams <> nil) and
     assigned(InputProfile) and
     InputProfile.IsValid and
     not InputProfile.IsApplied and
     IOParams.OutputICCProfile.IsValid and
     InputProfile.CheckTransform(integer(InputColorSpace)) and
     InputProfile.Transform(IOParams.OutputICCProfile, integer(InputColorSpace), integer(OutputColorSpace), 0, 0, InputScanline, OutputScanline, ImageWidth) then
  begin
     exit; // sucessful
  end;
  {$endif}

  case InputColorSpace of

    iecmsGray8:
      case OutputColorSpace of
        iecmsBGR:
          begin
            // Gray8->BGR
            _ConvRow1To24(InputScanline, OutputScanline, ImageWidth);
          end;
      end;

    iecmsRGB:
      case OutputColorSpace of
        iecmsBGR:
          begin
            // RGB->BGR
            _CopyBGR_RGB(OutputScanline, InputScanline, ImageWidth);
          end;
      end;

    iecmsRGB48:
      case OutputColorSpace of
        iecmsBGR:
          begin
            // RGB48->BGR
            xrgb48 := PRGB48(InputScanline);
            xbgr := PRGB(OutputScanline);
            for i := 0 to ImageWidth - 1 do
            begin
              xbgr^.r := xrgb48^.r shr 8;
              xbgr^.g := xrgb48^.g shr 8;
              xbgr^.b := xrgb48^.b shr 8;
              inc(xbgr);
              inc(xrgb48);
            end;
          end;
      end;

    iecmsRGB48_SE:
      case OutputColorSpace of
        iecmsBGR:
          begin
            // RGB48->BGR
            xrgb48 := PRGB48(InputScanline);
            xbgr := PRGB(OutputScanline);
            for i := 0 to ImageWidth - 1 do
            begin
              xbgr^.r := xrgb48^.r and $FF;
              xbgr^.g := xrgb48^.g and $FF;
              xbgr^.b := xrgb48^.b and $FF;
              inc(xbgr);
              inc(xrgb48);
            end;
          end;
      end;

    iecmsBGR:
      case OutputColorSpace of
        iecmsCMYK:
          begin
            // BGR->CMYK
            xbgr := PRGB(InputScanline);
            xcmyk := PCMYK(OutputScanline);
            for i := 0 to ImageWidth - 1 do
            begin
              xcmyk^ := IERGB2CMYK(xbgr^);
              inc(xbgr);
              inc(xcmyk);
            end;
          end;
        iecmsCIELab:
          begin
            // BGR->CIELab
            xbgr := PRGB(InputScanline);
            xlab := PCIELAB(OutputScanline);
            for i := 0 to ImageWidth - 1 do
            begin
              xlab^ := IERGB2CIELAB(xbgr^);
              inc(xbgr);
              inc(xlab);
            end;
          end;
        iecmsGray8:
          begin
            // BGR->Gray8
            RedToGrayCoef   := IEGlobalSettings().RedToGrayCoef;
            GreenToGrayCoef := IEGlobalSettings().GreenToGrayCoef;
            BlueToGrayCoef  := IEGlobalSettings().BlueToGrayCoef;
            xbgr := PRGB(InputScanline);
            xgray8 := pbyte(OutputScanline);
            for i := 0 to ImageWidth  - 1 do
            begin
              with xbgr^ do
                xgray8^ := (r * RedToGrayCoef + g * GreenToGrayCoef + b * BlueToGrayCoef) div 100;
              inc(xbgr);
              inc(xgray8);
            end;
          end;
        iecmsBGR:
          begin
            // BGR->BGR
            copymemory(OutputScanline, InputScanline, ImageWidth*3);
          end;
      end;

    iecmsCMYK:
      case OutputColorSpace of
        iecmsBGR:
          begin
            // CMYK->BGR
            (*
            xcmyk := PCMYK(InputScanline);
            xbgr := OutputScanline;
            for i := 0 to ImageWidth - 1 do
            begin
              xbgr^ := IECMYK2RGB(xcmyk^);
              inc(xbgr);
              inc(xcmyk);
            end;
            //*)
            IECMYK2RGBROW(PCMYK(InputScanline), OutputScanline, ImageWidth);
          end;
      end;

    iecmsCMYK6:
      case OutputColorSpace of
        iecmsBGR:
          begin
            // CMYK->BGR
            xcmyk := PCMYK(InputScanline);
            xbgr := OutputScanline;
            for i := 0 to ImageWidth - 1 do
            begin
              with xcmyk^ do
              begin
                c := 255-c;
                m := 255-m;
                y := 255-y;
                k := 255-k;
              end;
              xbgr^ := IECMYK2RGB(xcmyk^);
              inc(xbgr);
              inc(xcmyk);
              inc(pbyte(xcmyk), 2);
            end;
          end;
      end;

    iecmsCIELab:
      case OutputColorSpace of
        iecmsBGR:
          begin
            // ieCIELab->BGR
            xlab := PCIELAB(InputScanline);
            xbgr := OutputScanline;
            for i := 0 to ImageWidth - 1 do
            begin
              xbgr^ := IECIELAB2RGB(xlab^);
              inc(xlab);
              inc(xbgr);
            end;
          end;
      end;

    iecmsYCbCr:
      case OutputColorSpace of
        iecmsBGR:
          begin
            // ieYCbCr->BGR
            xycbcr := PYCBCR(InputScanline);
            xbgr := OutputScanline;
            for i := 0 to ImageWidth - 1 do
            begin
              IEYCbCr2RGB(xbgr^, xycbcr^.y, xycbcr^.Cb, xycbcr^.Cr);
              inc(xycbcr);
              inc(xbgr);
            end;
          end;
      end;

  end;
end;

{!!
<FS>TImageEnIO.CreatePSFile

<FM>Declaration<FC>
procedure CreatePSFile(const FileName: WideString);

<FM>Description<FN>
Creates a new, empty PostScript file of the specified filename. You can add pages using <A TImageEnIO.SaveToPS> and when complete, close the file using <A TImageEnIO.ClosePSFile>.

Note: All images in the resulting PostScript file will be aligned to the upper-left corner of the paper.

<FM>Example<FC>
ImageEnView1.IO.CreatePSFile( 'D:\output.ps' );

// load and save page 1
ImageEnView1.IO.LoadFromFile('C:\page1.tiff');
ImageEnView1.IO.Params.PS_Compression := ioPS_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPS;

// load and save page 2
ImageEnView1.IO.LoadFromFile('C:\page2.tiff');
ImageEnView1.IO.Params.PS_Compression := ioPS_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPS;

...Other pages...

// close PS file
ImageEnView1.IO.ClosePSFile;

!!}
procedure TImageEnIO.CreatePSFile(const FileName: WideString);
begin
  fAborting := False;
  fPS_stream := TIEWideFileStream.Create(FileName, fmCreate);
  fPS_handle := IEPostScriptCreate(fPS_stream, fParams);
end;

{!!
<FS>TImageEnIO.SaveToPS

<FM>Declaration<FC>
procedure SaveToPS;

<FM>Description<FN>
Saves the current image to a PostScript file opened using <A TImageEnIO.CreatePSFile>.

Note: All images in the resulting PostScript file will be aligned to the upper-left corner of the paper.

<FM>Example<FC>
ImageEnView1.IO.CreatePSFile( 'D:\output.ps' );

// load and save page 1
ImageEnView1.IO.LoadFromFile('C:\page1.tiff');
ImageEnView1.IO.Params.PS_Compression := ioPS_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPS;

// load and save page 2
ImageEnView1.IO.LoadFromFile('C:\page2.tiff');
ImageEnView1.IO.Params.PS_Compression := ioPS_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPS;

...Other pages...

// close PS file
ImageEnView1.IO.ClosePSFile;

!!}
procedure TImageEnIO.SaveToPS;
var
  Progress: TProgressRec;
begin
  if not MakeConsistentBitmap([]) then
    exit;
  if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
    fIEBitmap.PixelFormat := ie24RGB;
  fAborting := false;
  Progress.Aborting := @fAborting;
  Progress.fOnProgress := fOnIntProgress;
  Progress.Sender := Self;
  IEPostScriptSave(fPS_handle, fPS_stream, fIEBitmap, fParams, Progress);
end;

{!!
<FS>TImageEnIO.ClosePSFile

<FM>Declaration<FC>
procedure ClosePSFile;

<FM>Description<FN>
Closes a PostScript file opened using <A TImageEnIO.CreatePSFile>.

<FM>Example<FC>
ImageEnView1.IO.CreatePSFile( 'output.ps' );

// load and save page 1
ImageEnView1.IO.LoadFromFile('C:\page1.tiff');
ImageEnView1.IO.Params.PS_Compression := ioPS_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPS;

// load and save page 2
ImageEnView1.IO.LoadFromFile('C:\page2.tiff');
ImageEnView1.IO.Params.PS_Compression := ioPS_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPS;

...Other pages...

// close PS file
ImageEnView1.IO.ClosePSFile;

<FM>See Also<FN>
- <A TImageEnIO.CreatePSFile>
- <A TImageEnIO.SaveToPS>
!!}
procedure TImageEnIO.ClosePSFile;
begin
  if fPS_handle <> nil then
    IEPostScriptClose(fPS_handle, fPS_stream);
  if assigned(fPS_stream) then
    FreeAndNil(fPS_stream);
  fPS_stream := nil;
  fPS_handle := nil;
end;

procedure TImageEnIO.SyncSaveToStreamPS(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    IEPostScriptSaveOneStep(Stream, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamPS

<FM>Declaration<FC>
procedure SaveToStreamPS(Stream: TStream);

<FM>Description<FN>                  
Saves the current image to a stream in PostScript format. The resulting file will have only one page.
!!}
procedure TImageEnIO.SaveToStreamPS(Stream: TStream);
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin                  
    // ASYNC SAVING
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamPS, Stream);
  end
  else
  begin
    SyncSaveToStreamPS(Stream);
  end;
end;

{!!
<FS>TImageEnIO.SaveToFilePS

<FM>Declaration<FC>
procedure SaveToFilePS(const FileName: WideString);

<FM>Description<FN>    
Saves the current image to a file in PostScript format.

<FC>FileName<FN> is the file name including extension.

If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

Note: The created file will have only one page. See <A TImageEnIO.CreatePSFile> to create multi-page PS files.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('D:\input.bmp');
ImageEnView1.IO.SaveToFilePS('D:\output.ps');

!!}
procedure TImageEnIO.SaveToFilePS(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin            
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFilePS, FileName);
    exit;
  end;

  
  fs := TIEWideFileStream.Create(FileName, fmCreate);
  try
    SyncSaveToStreamPS(fs);
    fParams.FileName := FileName;
    fParams.FileType := ioPS;
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnIO.CreatePDFFile

<FM>Declaration<FC>
procedure CreatePDFFile(const FileName: WideString);

<FM>Description<FN>       
Creates a new, empty Adobe PDF file of the specified filename. You can add pages using <A TImageEnIO.SaveToPDF> and when complete, close the file using <A TImageEnIO.ClosePDFFile>.

Note: All images in the resulting PDF file will be aligned to the upper-left corner of the paper.

<FM>Example<FC>
ImageEnView1.IO.CreatePDFFile( 'D:\output.pdf' );

// load and save page 1
ImageEnView1.IO.LoadFromFile('C:\page1.tiff');
ImageEnView1.IO.Params.PDF_Compression := ioPDF_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPDF;

// load and save page 2
ImageEnView1.IO.LoadFromFile('C:\page2.tiff');
ImageEnView1.IO.Params.PDF_Compression := ioPDF_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPDF;

...Other pages...

// close PDF file
ImageEnView1.IO.ClosePDFFile;
!!}
{$ifdef IEINCLUDEPDFWRITING}
procedure TImageEnIO.CreatePDFFile(const FileName: WideString);
begin
  fAborting := False;
  fPDF_stream := TIEWideFileStream.Create(FileName, fmCreate);
  fPDF_handle := IEPDFCreate(fParams);
end;
{$endif}

{!!
<FS>TImageEnIO.SaveToPDF

<FM>Declaration<FC>
procedure SaveToPDF;

<FM>Description<FN>             
Saves the current image to the active Adobe PDF file, opened using <A TImageEnIO.CreatePDFFile>.

Note: All images in the resulting PDF file will be aligned to the upper-left corner of the paper.

<FM>Example<FC>
ImageEnView1.IO.CreatePDFFile( 'D:\output.pdf' );

// load and save page 1
ImageEnView1.IO.LoadFromFile('C:\page1.tiff');
ImageEnView1.IO.Params.PDF_Compression := ioPDF_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPDF;

// load and save page 2
ImageEnView1.IO.LoadFromFile('C:\page2.tiff');
ImageEnView1.IO.Params.PDF_Compression := ioPDF_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPDF;

...Other pages...

// close PDF file
ImageEnView1.IO.ClosePDFFile;
!!}
{$ifdef IEINCLUDEPDFWRITING}
procedure TImageEnIO.SaveToPDF;
var
  Progress: TProgressRec;
begin
  if not MakeConsistentBitmap([]) then
    exit;
  if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
    fIEBitmap.PixelFormat := ie24RGB;
  fAborting := false;
  Progress.Aborting := @fAborting;
  Progress.fOnProgress := fOnIntProgress;
  Progress.Sender := Self;
  IEPDFSave(fPDF_handle, fIEBitmap, fParams, Progress);
end;
{$endif}

{!!
<FS>TImageEnIO.ClosePDFFile

<FM>Declaration<FC>
procedure ClosePDFFile;

<FM>Description<FN>     
Closes the active Adobe PDF file, opened using <A TImageEnIO.CreatePDFFile>.

<FM>Example<FC>
ImageEnView1.IO.CreatePDFFile( 'D:\output.pdf' );

// load and save page 1
ImageEnView1.IO.LoadFromFile('C:\page1.tiff');
ImageEnView1.IO.Params.PDF_Compression := ioPDF_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPDF;

// load and save page 2
ImageEnView1.IO.LoadFromFile('C:\page2.tiff');
ImageEnView1.IO.Params.PDF_Compression := ioPDF_G4FAX;  // G4Fax compression
ImageEnView1.IO.SaveToPDF;

...Other pages...

// close PDF file
ImageEnView1.IO.ClosePDFFile;

<FM>See Also<FN>
- <A TImageEnIO.CreatePDFFile>
- <A TImageEnIO.SaveToPDF>
!!}
{$ifdef IEINCLUDEPDFWRITING}
procedure TImageEnIO.ClosePDFFile;
begin
  if fPDF_handle <> nil then
    IEPDFClose(fPDF_handle, fPDF_stream, fParams);
  if assigned(fPDF_stream) then
    FreeAndNil(fPDF_stream);
  fPDF_stream := nil;
  fPDF_handle := nil;
end;
{$endif}

{$ifdef IEINCLUDEPDFWRITING}
procedure TImageEnIO.SyncSaveToStreamPDF(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    if (fIEBitmap.pixelformat <> ie24RGB) and (fIEBitmap.PixelFormat <> ie1g) then
      fIEBitmap.PixelFormat := ie24RGB;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    IEPDFSaveOneStep(Stream, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;
{$endif}

{!!
<FS>TImageEnIO.SaveToStreamPDF

<FM>Declaration<FC>
procedure SaveToStreamPDF(Stream: TStream);

<FM>Description<FN>           
Saves the current image to a stream in Adobe PDF format. The resulting file will have only one page.
!!}
{$ifdef IEINCLUDEPDFWRITING}
procedure TImageEnIO.SaveToStreamPDF(Stream: TStream);
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    // ASYNC SAVING
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamPDF, Stream);
  end
  else
  begin
    SyncSaveToStreamPDF(Stream);
  end;
end;
{$endif}

{!!
<FS>TImageEnIO.SaveToFilePDF

<FM>Declaration<FC>
procedure SaveToFilePDF(const FileName: WideString);

<FM>Description<FN>    
Saves the current image to a file in Adobe PDF format.

<FC>FileName<FN> is the file name including extension.

If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

Note: The created file will have only one page. See <A TImageEnIO.CreatePDFFile> to create multi-page PDF files.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\input.bmp');
ImageEnView1.IO.SaveToFilePDF('D:\output.pdf');
!!}
{$ifdef IEINCLUDEPDFWRITING}
procedure TImageEnIO.SaveToFilePDF(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin                     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFilePDF, FileName);
    exit;
  end;

  
  fs := TIEWideFileStream.Create(FileName, fmCreate);
  try
    SyncSaveToStreamPDF(fs);
    fParams.FileName := FileName;
    fParams.FileType := ioPDF;
  finally
    FreeAndNil(fs);
  end;
end;
{$endif}

{$IFDEF IEINCLUDEDIRECTSHOW}



{!!
<FS>TImageEnIO.DShowParams

<FM>Declaration<FC>
property DShowParams: <A TIEDirectShow>;

<FM>Description<FN>
DShowParams is a TIEDirectShow instance which allows control of some Direct Show features, such as video capture, audio capture, multimedia files capture as well video rendering, and multimedia file writing.

<FM>Demos<FN>
VideoCapture\DirectShow1
VideoCapture\DirectShow2
VideoCapture\MotionDetectior
VideoCapture\VideoEffects
Display\VideoPlayer

<FM>Examples<FC>
Capture Skeleton

// Fills ComboBox1 with the names of all video capture inputs
ComboBox1.Items.Assign( ImageEnView1.IO.DShowParams.VideoInputs );

// Select the first video input available
ImageEnView1.IO.DShowParams.SetVideoInput( 0 );

// Enables frame grabbing
// ImageEnView1.OnDShowNewFrame event occurs for each frame acquired
ImageEnView1.IO.DShowParams.EnableSampleGrabber := True;

// connect to the video input: this begins video capture
ImageEnView1.IO.DShowParams.Connect;

...

// Disconnect the video input
ImageEnView1.IO.DShowParams.Disconnect;

Capture a frame

// We are inside the OnDShowNewFrame event:
procedure Tfmain.ImageEnView1DShowNewFrame(Sender: TObject);
begin
   // copy current sample to ImageEnView bitmap
   ImageEnView1.IO.DShowParams.GetSample(ImageEnView1.IEBitmap);
   // refresh ImageEnView1
   ImageEnView1.Update;
end;

Setting video capture size

// We'd like to acquire 640x480 frames
ImageEnView1.IO.DShowParams.SetCurrentVideoFormat( 640, 480, '' );

Select a TV Tuner channel

// We want channel 15
ImageEnView1.IO.DShowParams.TunerChannel := 15;

Showing capture card dialogs

// Show video dialog
ImageEnView1.IO.DShowParams.ShowPropertyPages(iepVideoInput, ietFilter);

// Show video source dialog
ImageEnView1.IO.DShowParams.ShowPropertyPages(iepVideoInputSource, ietFilter);

// Show tuner dialog
ImageEnView1.IO.DShowParams.ShowPropertyPages(iepTuner, ietFilter);

// Show format dialog
ImageEnView1.IO.DShowParams.ShowPropertyPages(iepVideoInput, ietOutput);

Read from a multimedia file, and save with another compression

ImageEnView1.IO.DShowParams.FileInput := 'input.avi';
ImageEnView1.IO.DShowParams.FileOutput := 'output.avi';
ImageEnView1.IO.DShowParams.SetVideoCodec( ... );
ImageEnView1.IO.DShowParams.SetAudioCodec( ... );

ImageEnView1.IO.DShowParams.EnableSampleGrabber := True;
ImageEnView1.IO.DShowParams.Connect;

See the \VideoCapture\DirectShow1\ and \VideoCapture\DirectShow2\ demos for more info.
!!}
function TImageEnIO.GetDShowParams: TIEDirectShow;
begin
  if not assigned(fDShow) then
  begin
    fDShow := TIEDirectShow.Create;
    if assigned(fImageEnView) then
    begin
      fDShow.SetNotifyWindow(fImageEnView.Handle, IEM_NEWFRAME, IEM_EVENT)
    end;
  end;
  result := fDShow;
end;
{$ENDIF}


{$IFDEF IEINCLUDEMEDIAFOUNDATION}
{!!
<FS>TImageEnIO.MediaFoundationSourceReader

<FM>Declaration<FC>
property MediaFoundationSourceReader: <A TIEMediaFoundationSourceReader>;

<FM>Description<FN>
MediaFoundationSourceReader is a TIEMediaFoundationSourceReader instance which allows capture samples from webcams, multimedia files and URLs.

This is a minimal setup to capture from a webcam (the first webcam, using first proposed media type):

<FC>
ImageEnView1.IO.MediaFoundationSourceReader.SetVideoInput(0);                   // select first video input (first webcam)
ImageEnView1.IO.MediaFoundationSourceReader.SetSelectedStreams('Video', true);  // enable first video stream
ImageEnView1.IO.MediaFoundationSourceReader.SelectMediaType('Video', 0);        // select first media type of the first video stream
ImageEnView1.IO.MediaFoundationSourceReader.StartCapture();                     // start capture

// handler for TImageEnView.OnMediaFoundatioNotify event
procedure TForm1.ImageEnVect1MediaFoundationNotify(Sender, MediaFoundationObject: TObject; NotifyType: TIEMediaFountationNotifyType);
var
  sample: TIEMFReceivedSample;
begin
  if NotifyType = iemfnFRAME then // is this a frame?
  begin
    sample := ImageEnView1.IO.MediaFoundationSourceReader.GetNextSample();  // retrieve frame sample
    try
      sample.DecodeSample(ImageEnView1.IEBitmap); // convert frame sample to bitmap
      ImageEnView1.Update();                      // update TImageEnView to show the new bitmap
    finally
      sample.Free();                              // free the sample
    end;
  end;
end;
<FN>

Look at VideoCapture\MediaFoundation demos for usage samples.
!!}
function TImageEnIO.GetMediaFoundationSourceReader(): TIEMediaFoundationSourceReader;
begin
  if not assigned(fMediaFoundationSourceReader) then
  begin
    fMediaFoundationSourceReader := TIEMediaFoundationSourceReader.Create();
    if assigned(fImageEnView) then
      fMediaFoundationSourceReader.PushNotifyReceiver( TIEMediaFoundationReaderWindowNotifyReceiver.Create(fImageEnView.Handle, IEM_MEDIAFOUNDATION) );
  end;
  result := fMediaFoundationSourceReader;
end;
{$ENDIF}


procedure TImageEnIO.CheckDPI;
begin
  if fParams.DpiX < 2 then
    fParams.DpiX := IEGlobalSettings().DefaultDPIX;
  if fParams.DpiY < 2 then
    fParams.DpiY := IEGlobalSettings().DefaultDPIY;
end;

{!!
<FS>IECalcJpegFileQuality

<FM>Declaration<FC>
function IECalcJpegFileQuality(const FileName: WideString): Integer;

<FM>Description<FN>
Estimates the quality that was used for saving a Jpeg file.

The returned value can then be used for the <A TIOParamsVals.JPEG_Quality> property (to maintain a similar quality level).

<FM>Example<FC>
ImageEnView1.LoadFromFile('C:\input.jpg');
// do image processing here...
ImageEnView1.IO.Params.JPEG_Quality := IECalcJpegFileQuality('C:\input.jpg');
ImageEnView1.IO.SaveToFile('D:\output.jpg');

!!}
function IECalcJpegFileQuality(const FileName: WideString): integer;
var
  fs: TIEWideFileStream;
begin
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := IECalcJpegStreamQuality(fs);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>IECalcJpegStreamQuality

<FM>Declaration<FC>
function IECalcJpegStreamQuality(Stream: TStream): Integer;

<FM>Description<FN>
Estimates the quality that was used for saving a Jpeg file.

The returned value can then be used for the <A TIOParamsVals.JPEG_Quality> property (to maintain a similar quality level).

<FM>Example<FC>
ImageEnView1.LoadFromFile('C:\input.jpg');
// do image processing here...
ImageEnView1.IO.Params.JPEG_Quality := IECalcJpegFileQuality('C:\input.jpg');
ImageEnView1.IO.SaveToFile('D:\output.jpg');
!!}
function IECalcJpegStreamQuality(Stream: TStream): integer;
var
  qt: Pointer;
  QTables: pintegerarray;
  QTablesCount: integer;
  i: integer;
begin
  QTablesCount := IEGetJpegQuality(Stream, qt);
  QTables := qt;
  // returns the average of all qtables
  result := 0;
  for i := 0 to QTablesCount - 1 do
    result := result + QTables[i];
  result := result div QTablesCount;
  //
  freemem(qt);
end;

{$ifdef IEINCLUDERAWFORMATS}
procedure TImageEnIO.SyncLoadFromStreamRAW(Stream: TStream);
var
  Progress: TProgressRec;
  ff: TIOFileType;
begin
  ff := IEExtToFileFormat('RAW');
  if ff = ioRAW then
  begin
    // use internal implementation
    try
      fAborting := false;
      Progress.Aborting := @fAborting;
      if not MakeConsistentBitmap([]) then
        exit;
      fParams.ResetInfo;
      Progress.fOnProgress := fOnIntProgress;
      Progress.Sender := Self;
      fIEBitmap.RemoveAlphaChannel;
      IEReadCameraRAWStream(Stream, fIEBitmap, fParams, Progress, false);
      CheckDPI;
      if fAutoAdjustDPI then
        AdjustDPI;
      fParams.fFileName := '';
      fParams.fFileType := ioRAW;
      SetViewerDPI(fParams.DpiX, fParams.DpiY);
      Update;
    finally
      DoFinishWork;
    end;
  end
  else
    // use external plugin if available
    LoadFromStreamFormat(Stream, ff);
end;
{$endif}

{$ifdef IEINCLUDERAWFORMATS}
{!!
<FS>TImageEnIO.LoadFromStreamRAW

<FM>Declaration<FC>
function LoadFromStreamRAW(Stream: TStream): Boolean;

<FM>Description<FN>
Loads an image from a stream containing a digital camera Raw file. The result will be false if an error is encountered, e.g. the file in the stream is not a RAW format (<A TImageEnIO.Aborting> will be true).

See also: <L List of supported Camera RAW formats>Supported Camera Raw Formats</L>
!!}
function TImageEnIO.LoadFromStreamRAW(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamRAW, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamRAW(Stream); 
    Result := Not fAborting;
  end;
end;
{$endif}

{$ifdef IEINCLUDERAWFORMATS}
{!!
<FS>TImageEnIO.LoadFromFileRAW

<FM>Declaration<FC>
function LoadFromFileRAW(const FileName: WideString): Boolean;

<FM>Description<FN>        
Loads an image from a Digital Camera Raw file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not a camera RAW format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

See also: <L List of supported Camera RAW formats>Supported Camera RAW formats</L>
!!}
function TImageEnIO.LoadFromFileRAW(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileRAW, FileName);
    Result := True;
    exit;
  end;

  fs := TIEWideFileStream.create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamRAW(fs);
    fParams.fFileName := FileName; 
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
end;
{$endif}

{$ifdef IEINCLUDEDIRECTSHOW}
{!!
<FS>TImageEnIO.OpenMediaFile

<FM>Declaration<FC>
function OpenMediaFile(const FileName: WideString): Integer;

<FM>Description<FN>
Opens a video file using DirectShow. Supported file types are AVI, MPEG, WMV and other DirectX supported formats.

This method will open the file, but doesn't actually get an image. To get a frame use the <A TImageEnIO.LoadFromMediaFile> method.

To close the media file use <A TImageEnIO.CloseMediaFile>.

<FM>Example<FC>
// Save frame 10 as 'frame10.jpg'
ImageEnView1.IO.OpenMediaFile('C:\film.mpeg');
ImageEnView1.IO.LoadFromMediaFile(10);
ImageEnView1.IO.SaveToFile('D:\frame10.jpg');
ImageEnView1.IO.CloseMediaFile;
!!}
function TImageEnIO.OpenMediaFile(const FileName: WideString): Integer;
var
  l: int64;
  ww, hh: Integer;
  fmt: AnsiString;
  avgtime, ltime: int64;
begin
  result := 0;
  if assigned(fOpenMediaFile) then
    CloseMediaFile;
  fAborting := True;
  fOpenMediaFile := TIEDirectShow.Create;

  fOpenMediaFile.FileInput := AnsiString(FileName);
  fOpenMediaFile.EnableSampleGrabber := True;
  fOpenMediaFile.Connect;

  fOpenMediaFile.Pause;

  fOpenMediaFile.TimeFormat := TfTime;
  ltime := fOpenMediaFile.Duration;
  if (ltime = 0) then
    exit;
  fOpenMediaFile.TimeFormat := TfFrame;
  l := fOpenMediaFile.Duration;
  avgtime := fOpenMediaFile.GetAverageTimePerFrame;

  if (l=ltime) and (avgtime <> 0) then
  begin
    l := l div avgtime;
    fOpenMediaFileMul := avgtime;
  end
  else
    fOpenMediaFileMul := 1;
  fOpenMediaFileRate := (ltime/10000000) / l*1000;

  fAborting := False;

  fParams.MEDIAFILE_FrameCount := l;
  fParams.FileType := ioUnknown;
  fParams.FileName := FileName; // this is important for TImageEnMView

  fOpenMediaFile.GetCurrentVideoFormat(ww, hh, fmt);
  fParams.Width := ww;
  fParams.Height := hh;
  fParams.OriginalWidth := ww;
  fParams.OriginalHeight := hh;

  result := l;
end;

{!!
<FS>TImageEnIO.CloseMediaFile

<FM>Declaration<FC>
procedure CloseMediaFile;

<FM>Description<FN>
Close a video file that was opened with <A TImageEnIO.OpenMediaFile>.

<FM>Example<FC>
// Save frame 10 as 'frame10.jpg'
ImageEnView1.IO.OpenMediaFile('C:\film.mpeg');
ImageEnView1.IO.LoadFromMediaFile(10);
ImageEnView1.IO.SaveToFile('D:\frame10.jpg');
ImageEnView1.IO.CloseMediaFile;

!!}
procedure TImageEnIO.CloseMediaFile;
begin
  if assigned(fOpenMediaFile) then
  begin
    fOpenMediaFile.Disconnect;
    FreeAndNil(fOpenMediaFile);
  end;
end;

{!!
<FS>TImageEnIO.LoadFromMediaFile

<FM>Declaration<FC>
procedure LoadFromMediaFile(FrameIndex: Integer);

<FM>Description<FN>
Loads the specified frame from a media file that was opened using <A TImageEnIO.OpenMediaFile>.

The first frame has index 0.

<FM>Example<FC>
// Save frame 10 as 'frame10.jpg'
ImageEnView1.IO.OpenMediaFile('C:\film.mpeg');
ImageEnView1.IO.LoadFromMediaFile(10);
ImageEnView1.IO.SaveToFile('D:\frame10.jpg');
ImageEnView1.IO.CloseMediaFile;

!!}
procedure TImageEnIO.LoadFromMediaFile(FrameIndex: Integer);
begin
  if assigned(fOpenMediaFile) then
  begin
    fParams.TIFF_ImageIndex := FrameIndex;
    fParams.GIF_ImageIndex := FrameIndex;
    fParams.DCX_ImageIndex := FrameIndex;

    fOpenMediaFile.position := int64(FrameIndex) * int64(fOpenMediaFileMul);

    fOpenMediaFile.GetSample( fIEBitmap );
    fParams.fMEDIAFILE_FrameDelayTime := fOpenMediaFileRate;
    Update;
  end;
end;

{!!
<FS>TImageEnIO.IsOpenMediaFile

<FM>Declaration<FC>
function IsOpenMediaFile: Boolean;

<FM>Description<FN>
Returns True if <A TImageEnIO.OpenMediaFile> has a currently open file.
!!}
function TImageEnIO.IsOpenMediaFile: Boolean;
begin
  result := assigned(fOpenMediaFile);
end;

{$endif}

{!!
<FS>IEGetFileFramesCount

<FM>Declaration<FC>
function IEGetFileFramesCount(const FileName: WideString): Integer;

<FM>Description<FN>
A generic method to get the number of frames in a multi-page images and supported videos (GIF, TIFF, AVI, MPEG...).

Note: It will work with single page files too (such as JPEG) by returning 1.

<FM>See Also<FN>
- <A Helper functions>
!!}
function IEGetFileFramesCount(const FileName: WideString): Integer;
var
  fi: TIEFileFormatInfo;
  io: TImageEnIO;
begin
  fi := IEFileFormatGetInfo2(string(IEExtractFileExtW(FileName)));
  if assigned(fi) and (fi.FileType<>ioUnknown) and (fi.FileType<>ioAVI) then
  begin
    case fi.FileType of
      ioGIF:    result := EnumGifIm(FileName);
      ioTIFF:   result := EnumTIFFIm(FileName);
      ioICO:    result := EnumICOIm(FileName);
      ioDCX:    result := EnumDCXIm(FileName);
      {$ifdef IEINCLUDEDICOM}
      ioDICOM:  result := IEDicomImageCount(string(FileName));
      {$endif}
      {$ifdef IEINCLUDEWIC}
      ioHDP:    result := IEHDPFrameCount(FileName);
      {$endif}
      ioPDF:    result := IEPDFFrameCount(FileName);
      {$ifdef IEINCLUDEMISCPLUGINS}
      iomscPDF: result := IEPDFFrameCount(FileName);
      {$endif}
      else
        result := 1;  // a single page file (jpeg, bmp...)
    end;
  end
  else
  begin
    // AVI or multimedia files (Mpeg,...)
    io := TImageEnIO.Create(nil);
    try
    {$ifdef IEINCLUDEDIRECTSHOW}
      result := io.OpenMediaFile(FileName);
      io.CloseMediaFile;
    {$else}
    // only AVI supported
      result := io.OpenAVIFile(FileName);
      io.CloseAVIFile;
    {$endif}
    finally
      FreeAndNil(io);
    end;
  end;
end;

{!!
<FS>IEExtToFileFormat

<FM>Declaration<FC>
function IEExtToFileFormat(ex: String): <A TIOFileType>;

<FM>Description<FN>
Converts a file extension to a <A TIOFileType> type.

<FM>See Also<FN>
- <A Helper functions>
!!}
function IEExtToFileFormat(ex: String): TIOFileType;
var
  fpi: TIEFileFormatInfo;
begin
  fpi := IEFileFormatGetInfo2( LowerCase(ex) );
  if assigned(fpi) then
    result := fpi.FileType
  else
  if IEFileExtInExtensions(ex, Supported_MPEG_File_Extensions) then
    result := ioMPEG
  else
  if IEFileExtInExtensions(ex, Supported_WMV_File_Extensions) then
    result := ioWMV
  else
    result := ioUnknown;
end;


{!!
<FS>IEFileIsOfFormat

<FM>Declaration<FC>
function IEFileIsOfFormat(const sFilename : string; aFormat : TIOFileType) : Boolean;

<FM>Description<FN>
Returns true if the file has the extension of the specified type.

For example: IEFileIsOfFormat('C:\File.avi', ioAVI) would return true

<FM>See Also<FN>
- <A Helper functions>
!!}
function IEFileIsOfFormat(const sFilename : string; aFormat : TIOFileType) : Boolean;
var
  Ex : string;
  c: integer;
  i: Integer;
begin
  if aFormat = ioAVI then
    result := IEFilenameInExtensions(sFilename, Supported_AVI_File_Extensions)
  else
  if aFormat = ioMPEG then
    result := IEFilenameInExtensions(sFilename, Supported_MPEG_File_Extensions)
  else
  if aFormat = ioWMV then
    result := IEFilenameInExtensions(sFilename, Supported_WMV_File_Extensions)
  else
  begin        
    Result := False;
    Ex := IEExtractFileExtS(sFilename, False);
    c := IEFileFormatGetExtCount(aFormat);
    for i := 0 to c - 1 do
    begin
      if LowerCase(IEFileFormatGetExt(aFormat, i)) = Ex then
      begin
        Result := True;
        exit;
      end;
    end;
  end;
end;


{!!
<FS>IEIsInternalFormat

<FM>Declaration<FC>
function IEIsInternalFormat(ex: String): Boolean;

<FM>Description<FN>
Returns true if the specified extension is recognized as internal file format (i.e. a format that ImageEn natively supports).

<FM>See Also<FN>
- <A Helper functions>
!!}
function IEIsInternalFormat(ex: String): Boolean;
var
  fpi: TIEFileFormatInfo;
begin
  fpi := IEFileFormatGetInfo2( LowerCase(ex) );
  result := assigned(fpi) and fpi.InternalFormat;
end;

procedure TImageEnIO.SyncLoadFromStreamBMPRAW(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    IERealRAWReadStream(Stream, fIEBitmap, fParams, Progress);
    fParams.fFileName := '';
    fParams.fFileType := ioBMPRAW;
    Update;
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamBMPRAW

<FM>Declaration<FC>
function LoadFromStreamBMPRAW(Stream: TStream): Boolean;

<FM>Description<FN>     
Loads an image from a stream containing a BmpRaw file. The result will be false if an error is encountered, e.g. the file in the stream is not BmpRaw format (<A TImageEnIO.Aborting> will be true).

Note: This is not the same as a digital camera Raw file, but a true "raw" format (named "BmpRaw" in ImageEn) where you can specify the channel order, placement, alignment, size, etc.
   
<FM>Demo<FN>
InputOutput/RealRAW

<FM>Examples<FC>
// load a RAW image, RGB, interleaved, 8 bit aligned, 1024x768
ImageEnView1.LegacyBitmap := False;
ImageEnView1.IEBitmap.Allocate(1024, 768, ie24RGB);
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plInterleaved;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.Params.BMPRAW_HeaderSize := 0;
ImageEnView1.IO.LoadFromStreamBMPRAW( stream );

// load a RAW image, CMYK, interleaved, 8 bit aligned, 1024x768
ImageEnView1.LegacyBitmap := False;
ImageEnView1.IEBitmap.Allocate(1024, 768, ieCMYK);
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plInterleaved;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.Params.BMPRAW_HeaderSize := 0;
ImageEnView1.IO.LoadFromStreamBMPRAW( stream );

!!}
function TImageEnIO.LoadFromStreamBMPRAW(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamBMPRAW, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamBMPRAW(Stream); 
    Result := Not fAborting;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromFileBMPRAW

<FM>Declaration<FC>
function LoadFromFileBMPRAW(const FileName: WideString): Boolean;

<FM>Description<FN>         
Loads an image from a BmpRaw file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not BmpRaw (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.
       
Note: This is not the same as a digital camera Raw file, but a true "raw" format (named "BmpRaw" in ImageEn) where you can specify the channel order, placement, alignment, size, etc.
                  
<FM>Demo<FN>
InputOutput/RealRAW

<FM>Examples<FC>
// load a RAW image, RGB, interleaved, 8 bit aligned, 1024x768
ImageEnView1.LegacyBitmap := False;
ImageEnView1.IEBitmap.Allocate(1024, 768, ie24RGB);
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plInterleaved;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.Params.BMPRAW_HeaderSize := 0;
ImageEnView1.IO.LoadFromFileBMPRAW('C:\input.dat');

// load a RAW image, CMYK, interleaved, 8 bit aligned, 1024x768
ImageEnView1.LegacyBitmap := False;
ImageEnView1.IEBitmap.Allocate(1024, 768, ieCMYK);
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plInterleaved;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.Params.BMPRAW_HeaderSize := 0;
ImageEnView1.IO.LoadFromFileBMPRAW('C:\input.dat');
!!}
function TImageEnIO.LoadFromFileBMPRAW(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin             
  Result := False;

  if FileName='' then
    exit;

  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileBMPRAW, FileName);
    Result := True;
    exit;
  end;

  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamBMPRAW(fs);
    fParams.fFileName := FileName;  
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
end;

procedure TImageEnIO.SyncSaveToStreamBMPRAW(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    IERealRAWWriteStream(Stream, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamBMPRAW

<FM>Declaration<FC>
procedure SaveToStreamBMPRAW(Stream: TStream);

<FM>Description<FN>              
Saves the current image to a stream in BmpRaw format.

Note: This is not the same as a digital camera Raw file, but a true "raw" format (named "BmpRaw" in ImageEn) where you can specify the channel order, placement, alignment, size, etc.

<FM>Demo<FN>
InputOutput/RealRAW

<FM>Example<FC>
// saves current image as RAW image
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plPlanar;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.SaveToStreamBMPRAW(stream);
!!}
procedure TImageEnIO.SaveToStreamBMPRAW(Stream: TStream);
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    // ASYNC SAVING
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamBMPRAW, Stream);
  end
  else
  begin
    SyncSaveToStreamBMPRAW(Stream);
  end;
end;

{!!
<FS>TImageEnIO.SaveToFileBMPRAW

<FM>Declaration<FC>
procedure SaveToFileBMPRAW(const FileName: WideString);

<FM>Description<FN>      
Saves the current image to a file in BmpRaw format.

<FC>FileName<FN> is the file name including extension.

If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

Note: This is not the same as a digital camera Raw file, but a true "raw" format (named "BmpRaw" in ImageEn) where you can specify the channel order, placement, alignment, size, etc.                           

<FM>Demo<FN>
InputOutput/RealRAW

<FM>Example<FC>
// saves current image as RAW image
ImageEnView1.IO.Params.BMPRAW_ChannelOrder := coRGB;
ImageEnView1.IO.Params.BMPRAW_Planes := plPlanar;
ImageEnView1.IO.Params.BMPRAW_RowAlign := 8;
ImageEnView1.IO.SaveToFileBMPRAW('C:\output.dat');

!!}
procedure TImageEnIO.SaveToFileBMPRAW(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin
  if FileName='' then
    exit;
    
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileBMPRAW, FileName);
    exit;
  end;

  
  fs := nil;
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    SyncSaveToStreamBMPRAW(fs);
    fParams.FileName := FileName;
    fParams.FileType := ioBMPRAW;
  finally
    FreeAndNil(fs);
  end;
end;

{$ifdef IEINCLUDERAWFORMATS}
{!!
<FS>TImageEnIO.LoadJpegFromFileCRW

<FM>Declaration<FC>
function LoadJpegFromFileCRW(const FileName: WideString): Boolean;

<FM>Description<FN>
Extract and load the large Jpeg encapsulated inside a CRW (canon RAW) file. Result will be false if the file is not a recognized file type (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

<FM>Example<FC>
ImageEnView1.IO.LoadJpegFromFileCRW('C:\input.crw');

!!}
function TImageEnIO.LoadJpegFromFileCRW(const FileName: WideString): Boolean;
var
  Progress: TProgressRec;
  fs: TIEWideFileStream;
begin            
  Result := False;
  if FileName='' then
    exit;
  fs := TIEWideFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    IECRWGetJpeg(fIEBitmap, fs);
    fParams.fFileName := FileName;
    fParams.fFileType := ioRAW;
    Update;
  finally
    FreeAndNil(fs);
    DoFinishWork;    
    Result := Not fAborting;
  end;
end;
{$endif}


{!!
<FS>IEAVISelectCodec

<FM>Declaration<FC>
function IEAVISelectCodec: AnsiString;

<FM>Description<FN>
Displays the Windows Codec Selection dialog. If the user selects a code it is returned as a FourCC codec string. Otherwise '' is returned.

This can then be used in methods such as <A TImageEnIO.CreateAVIFile>.
!!}
function IEAVISelectCodec(): AnsiString;
var
  AVI_avf: pointer;
  AVI_avs1: pointer;
  AVI_popts: pointer;
  psi: TAviStreamInfo;
  tempfilename: String;
begin
  if not gAVIFILEinit then
  begin
    AVIFileInit;
    gAVIFILEinit := true;
  end;
  AVI_avs1 := nil;
  AVI_avf := nil;
  AVI_popts := nil;
  tempfilename := IEGetTempFileName('avitemp', IEGlobalSettings().DefTEMPPATH)+'.avi';
  if IEFileExists(tempfilename) then
    DeleteFile(tempfilename);
  if AVIFileOpen(PAVIFILE(AVI_avf), PChar(tempfilename), OF_WRITE or OF_CREATE, nil) <> 0 then
    raise EInvalidGraphic.Create('unable to create AVI file');
  try
    ZeroMemory(@psi, sizeof(psi));
    psi.fccType := streamtypeVIDEO;
    psi.dwScale := 1;
    psi.dwRate := 15;
    psi.dwQuality := $FFFFFFFF;
    AVI_popts := AllocMem(sizeof(TAVICOMPRESSOPTIONS));
    psi.dwSuggestedBufferSize := 0;
    psi.rcFrame := rect(0, 0, 640, 480);
    AVIFileCreateStream(PAVIFILE(AVI_avf), PAVISTREAM(AVI_avs1), psi);
    if AVISaveOptions(0, 0, 1, @AVI_avs1, @AVI_popts) then
      result := PAnsiChar(@PAVICOMPRESSOPTIONS(AVI_popts)^.fccHandler)
    else
      result := '';
  finally
    if AVI_popts <> nil then
    begin
      AVISaveOptionsFree(1, PAVICOMPRESSOPTIONS(AVI_popts));
      freemem(AVI_popts);
    end;
    if AVI_avs1 <> nil then
      AVIStreamRelease(AVI_avs1);
    if AVI_avf <> nil then
      AVIFileRelease(AVI_avf);
    if IEFileExists(tempfilename) then
      DeleteFile(tempfilename);
  end;
end;

type
TICINFO = packed record
  dwSize: DWORD;
  fccType: DWORD;
  fccHandler: DWORD;
  dwFlags: DWORD;
  dwVersion: DWORD;
  dwVersionICM: DWORD;
  szName: array [0..16 - 1] of WCHAR;
  szDescription: array [0..128 - 1] of WCHAR;
  szDriver: array [0..128 - 1] of WCHAR;
end;
PICINFO = ^TICINFO;

function ICInfo(fccType: DWORD; fccHandler: DWORD; lpicinfo: PICINFO): Boolean; stdcall; external 'MsVfW32.dll' name 'ICInfo';
function ICGetInfo(hic: THandle; lpicinfo: PICINFO; cb: DWORD): LRESULT; stdcall; external 'MsVfW32.dll' name 'ICGetInfo';
function ICOpen(fccType: DWORD; fccHandler: DWORD; wMode: UINT): THandle; stdcall; external 'MsVfW32.dll' name 'ICOpen';
function ICClose(hic: THandle): LRESULT; stdcall; external 'MsVfW32.dll' name 'ICClose';

{!!
<FS>IEAVIGetCodecs

<FM>Declaration<FC>
function IEAVIGetCodecs: TStringList;

<FM>Description<FN>
Returns a list of Windows codec as FourCC codec strings. These can then be used for methods such as <A TImageEnIO.CreateAVIFile>.
To obtain a description of codecs use <A IEAVIGetCodecsDescription>.
!!}
function IEAVIGetCodecs: TStringList;
var
  i: Integer;
  ic: TICINFO;
  fourcc: AnsiString;
begin
  result := TStringList.Create;
  FillChar(ic, sizeof(ic), 0);
  SetLength(fourcc, 4);
  i := 0;
  while true do
  begin
    if not ICInfo(0, i, @ic) then
      break;
    Move(ic.fccHandler, fourcc[1], 4);
    result.Add( string(fourcc) );
    inc(i);
  end;
end;

{!!
<FS>IEAVIGetCodecsDescription

<FM>Declaration<FC>
function IEAVIGetCodecsDescription: TStringList;

<FM>Description<FN>
Returns a list of Windows codec descriptions. It will align with FourCC codec strings returned by <A IEAVIGetCodecs>.
!!}
function IEAVIGetCodecsDescription: TStringList;
var
  i: Integer;
  ic: TICINFO;
  hic: THandle;
begin
  result := TStringList.Create;
  FillChar(ic, sizeof(ic), 0);
  i := 0;
  while true do
  begin
    if not ICInfo(0, i, @ic) then
      break;
    hic := ICOpen(ic.fccType, ic.fccHandler, 4);
    ICGetInfo(hic, @ic, sizeof(TICINFO));
    result.Add( ic.szDescription );
    ICClose(hic);
    inc(i);
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// input/output plugins

const

// IEX_GetInfo/IEX_SetInfo: available after IEX_ExecuteRead
IEX_IMAGEWIDTH            = $00;
IEX_IMAGEHEIGHT           = $01;
IEX_PIXELFORMAT           = $02;     // IEX_1G, IEX_8G etc
IEX_FORMATDESCRIPTOR      = $03;     // could be the same of IEX_PLUGINDESCRIPTOR plus specific file format info
IEX_IMAGEDATA             = $04;     // RGB data. R,G,B are interlaced and the alignment is 8 bit. First channel is Red.
IEX_IMAGEPALETTE          = $05;     // an array of 256x8 bit RGB triplets, only when format is IEX_8P
IEX_IMAGEDATELENGTH       = $06;     // length of IEX_IMAGEDATA buffer
IEX_ORIGINALIMAGEWIDTH    = $07;
IEX_ORIGINALIMAGEHEIGHT   = $08;

// IEX_GetInfo/IEX_SetInfo: always available after IEX_Initialize
IEX_PLUGINCAPABILITY      = $0101;     // IEX_FILEREADER and/or IEX_FILEWRITER and/or IEX_MULTITHREAD
IEX_PLUGINDESCRIPTOR      = $0102;     // ex. "Camera RAW"
IEX_FILEEXTENSIONS        = $0103;     // ex. "CRW;CR2;NEF;RAW;PEF;RAF;X3F;BAY;ORF;SRF;MRW;DCR"
IEX_AUTOSEARCHEXIF        = $0104;     // plugin provides this value. You should use only with IEX_GetInfo. If true ImageEn will search automatically for EXIF in file
IEX_FORMATSCOUNT          = $0105;     // >1 if the plugin supports multiple formats

// IEX_GetInfo/IEX_SetInfo: values for IEX_CAPABILITY
IEX_FILEREADER            = $0001;
IEX_FILEWRITER            = $0010;
IEX_MULTITHREAD           = $0100;

// IEX_GetInfo/IEX_SetInfo: values for IEX_PIXELFORMAT
IEX_INVALID  = 0;
IEX_1G       = 1;
IEX_8P       = 2;
IEX_8G       = 3;
IEX_16G      = 4;
IEX_24RGB    = 5;
IEX_32F      = 6;
IEX_CMYK     = 7;
IEX_48RGB    = 8;

type

// callbacks (stdcall)
TIEX_Progress = procedure(percentage: Integer; UserData: Pointer); stdcall;
TIEX_Read = function(buffer: Pointer; bufferLength: Integer; UserData: Pointer): Integer; stdcall;
TIEX_Write = function(buffer: Pointer; bufferLength: Integer; UserData: Pointer): Integer; stdcall;
TIEX_PositionSet = procedure(position: Integer; UserData: Pointer); stdcall;
TIEX_PositionGet = function(UserData: Pointer): Integer; stdcall;
TIEX_Length = function(UserData: Pointer): Integer; stdcall;
TIEX_GetParameter = procedure(Param: PAnsiChar; value: PAnsiChar; UserData: Pointer); stdcall;
TIEX_SetParameter = procedure(Param: PAnsiChar; value: PAnsiChar; UserData: Pointer); stdcall;

// callbacks (cdecl)
TIEX_Progress_cdecl = procedure(percentage: Integer; UserData: Pointer); cdecl;
TIEX_Read_cdecl = function(buffer: Pointer; bufferLength: Integer; UserData: Pointer): Integer; cdecl;
TIEX_Write_cdecl = function(buffer: Pointer; bufferLength: Integer; UserData: Pointer): Integer; cdecl;
TIEX_PositionSet_cdecl = procedure(position: Integer; UserData: Pointer); cdecl;
TIEX_PositionGet_cdecl = function(UserData: Pointer): Integer; cdecl;
TIEX_Length_cdecl = function(UserData: Pointer): Integer; cdecl;
TIEX_GetParameter_cdecl = procedure(Param: PAnsiChar; value: PAnsiChar; UserData: Pointer); cdecl;
TIEX_SetParameter_cdecl = procedure(Param: PAnsiChar; value: PAnsiChar; UserData: Pointer); cdecl;


// dll exported (stdcall)
TIEX_GetInfo = function(handle: Pointer; info: Integer): Pointer; stdcall;
TIEX_SetInfo = procedure(handle: Pointer; info: Integer; value: Pointer); stdcall;
TIEX_ExecuteRead = procedure(handle: Pointer; parametersOnly: longbool); stdcall;
TIEX_ExecuteWrite = procedure(handle: Pointer); stdcall;
TIEX_ExecuteTry = function(handle: Pointer): longbool; stdcall;
TIEX_AddParameter = procedure(handle: Pointer; param: PAnsiChar); stdcall;
TIEX_SetCallBacks = procedure(handle: Pointer; progressfun: TIEX_Progress; readfun: TIEX_Read; writefun: TIEX_Write; posset: TIEX_PositionSet; posget: TIEX_PositionGet; lenfun: TIEX_Length; getparam: TIEX_GetParameter; setparam: TIEX_SetParameter; userData: Pointer ); stdcall;
TIEX_Initialize = function(format: Integer): Pointer; stdcall;
TIEX_Finalize = procedure(handle: Pointer); stdcall;

// dll exported (cdecl)
TIEX_GetInfo_cdecl = function(handle: Pointer; info: Integer): Pointer; cdecl;
TIEX_SetInfo_cdecl = procedure(handle: Pointer; info: Integer; value: Pointer); cdecl;
TIEX_ExecuteRead_cdecl = procedure(handle: Pointer; parametersOnly: longbool); cdecl;
TIEX_ExecuteWrite_cdecl = procedure(handle: Pointer); cdecl;
TIEX_ExecuteTry_cdecl = function(handle: Pointer): longbool; cdecl;
TIEX_AddParameter_cdecl = procedure(handle: Pointer; param: PAnsiChar); cdecl;
TIEX_SetCallBacks_cdecl = procedure(handle: Pointer; progressfun: TIEX_Progress_cdecl; readfun: TIEX_Read_cdecl; writefun: TIEX_Write_cdecl; posset: TIEX_PositionSet_cdecl; posget: TIEX_PositionGet_cdecl; lenfun: TIEX_Length_cdecl; getparam: TIEX_GetParameter_cdecl; setparam: TIEX_SetParameter_cdecl; userData: Pointer ); cdecl;
TIEX_Initialize_cdecl = function(format: Integer): Pointer; cdecl;
TIEX_Finalize_cdecl = procedure(handle: Pointer); cdecl;


TIEIOPlugin=record
  libhandle: THandle;
  // stdcall
  IEX_ExecuteRead_std: TIEX_ExecuteRead;
  IEX_ExecuteWrite_std: TIEX_ExecuteWrite;
  IEX_ExecuteTry_std: TIEX_ExecuteTry;
  IEX_AddParameter_std: TIEX_AddParameter;
  IEX_SetCallBacks_std: TIEX_SetCallBacks;
  IEX_Initialize_std: TIEX_Initialize;
  IEX_Finalize_std: TIEX_Finalize;
  IEX_GetInfo_std: TIEX_GetInfo;
  IEX_SetInfo_std: TIEX_SetInfo;
  // cdecl
  IEX_ExecuteRead_cdecl: TIEX_ExecuteRead_cdecl;
  IEX_ExecuteWrite_cdecl: TIEX_ExecuteWrite_cdecl;
  IEX_ExecuteTry_cdecl: TIEX_ExecuteTry_cdecl;
  IEX_AddParameter_cdecl: TIEX_AddParameter_cdecl;
  IEX_SetCallBacks_cdecl: TIEX_SetCallBacks_cdecl;
  IEX_Initialize_cdecl: TIEX_Initialize_cdecl;
  IEX_Finalize_cdecl: TIEX_Finalize_cdecl;
  IEX_GetInfo_cdecl: TIEX_GetInfo_cdecl;
  IEX_SetInfo_cdecl: TIEX_SetInfo_cdecl;

  FileType: TIOFileType;
  PluginsCS: TRTLCriticalSection; // protect non multithreaded plugin
  Capability: Integer;
  pluginFormat: Integer; // the index of format inside the plugin
  DLLFileName: String;
  use_cdecl: Boolean;
end;
PIEIOPlugin=^TIEIOPlugin;

var
  ioplugins: TList;


function IEGetIOPlugin(fileType: TIOFileType): PIEIOPlugin;
var
  i: Integer;
begin
  for i := 0 to ioplugins.Count - 1 do
  begin
    result := PIEIOPlugin( ioplugins[i] );
    if result^.FileType=fileType then
      exit;
  end;
  result := nil;
end;

type
  TIECallbackRecord = record
    OnProgress:       TIEProgressEvent;
    OnProgressSender: TObject;
    Stream:           TStream;
    params:           TIOParamsVals;
  end;
  PIECallBackRecord = ^TIECallBackRecord;


procedure CallBack_Progress_std(percentage: Integer; userdata: Pointer); stdcall;
begin
  with PIECallBackRecord(userdata)^ do
    if assigned(OnProgress) then
      OnProgress( OnProgressSender, percentage );
end;

procedure CallBack_Progress_cdecl(percentage: Integer; userdata: Pointer); cdecl;
begin
  CallBack_Progress_std(percentage, userdata);
end;

function CallBack_Read_std(buffer: Pointer; bufferLength: Integer; userdata: Pointer): Integer; stdcall;
begin
  result := PIECallBackRecord(userdata)^.Stream.Read(pbyte(buffer)^, bufferLength);
end;

function CallBack_Read_cdecl(buffer: Pointer; bufferLength: Integer; userdata: Pointer): Integer; cdecl;
begin
  result := CallBack_Read_std(buffer, bufferLength, userdata);
end;

function CallBack_Write_std(buffer: Pointer; bufferLength: Integer; userdata: Pointer): Integer; stdcall;
begin
  result := PIECallBackRecord(userdata)^.Stream.Write(pbyte(buffer)^, bufferLength);
end;

function CallBack_Write_cdecl(buffer: Pointer; bufferLength: Integer; userdata: Pointer): Integer; cdecl;
begin
  result := CallBack_Write_std(buffer, bufferLength, userdata);
end;

procedure CallBack_PositionSet_std(position: Integer; UserData: Pointer); stdcall;
begin
  PIECallBackRecord(userdata)^.Stream.Position := position;
end;

procedure CallBack_PositionSet_cdecl(position: Integer; UserData: Pointer); cdecl;
begin
  CallBack_PositionSet_std(position, UserData);
end;

function CallBack_PositionGet_std(UserData: Pointer): Integer; stdcall;
begin
  result := PIECallBackRecord(userdata)^.Stream.Position;
end;

function CallBack_PositionGet_cdecl(UserData: Pointer): Integer; cdecl;
begin
  result := CallBack_PositionGet_std(UserData);
end;

function CallBack_Length_std(UserData: Pointer): Integer; stdcall;
begin
  result := PIECallBackRecord(userdata)^.Stream.Size;
end;

function CallBack_Length_cdecl(UserData: Pointer): Integer; cdecl;
begin
  result := CallBack_Length_std(UserData);
end;

procedure CallBack_GetParameter_std(Param: PAnsiChar; Value: PAnsiChar; UserData: Pointer); stdcall;
begin
  if assigned( PIECallBackRecord(userdata)^.Params ) then
    IEStrCopy( Value, PAnsiChar(AnsiString(PIECallBackRecord(userdata)^.Params.GetProperty(WideString(Param)))) );
end;

procedure CallBack_GetParameter_cdecl(Param: PAnsiChar; Value: PAnsiChar; UserData: Pointer); cdecl;
begin
  CallBack_GetParameter_std(Param, Value, UserData);
end;

procedure CallBack_SetParameter_std(Param: PAnsiChar; Value: PAnsiChar; UserData: Pointer); stdcall;
begin
  if assigned( PIECallBackRecord(userdata)^.Params ) then
    PIECallBackRecord(userdata)^.Params.SetProperty(WideString(Param), WideString(Value));
end;

procedure CallBack_SetParameter_cdecl(Param: PAnsiChar; Value: PAnsiChar; UserData: Pointer); cdecl;
begin
  CallBack_SetParameter_std(Param, Value, UserData);
end;




function IEX_GetInfo(p: PIEIOPlugin; handle: Pointer; info: Integer): Pointer;
begin
  if p^.use_cdecl then
    result := p^.IEX_GetInfo_cdecl(handle, info)
  else
    result := p^.IEX_GetInfo_std(handle, info);
end;

procedure IEX_SetInfo(p: PIEIOPlugin; handle: Pointer; info: Integer; value: Pointer);
begin
  if p^.use_cdecl then
    p^.IEX_SetInfo_cdecl(handle, info, value)
  else
    p^.IEX_SetInfo_std(handle, info, value);
end;

procedure IEX_ExecuteRead(p: PIEIOPlugin; handle: Pointer; parametersOnly: longbool);
begin
  if p^.use_cdecl then
    p^.IEX_ExecuteRead_cdecl(handle, parametersOnly)
  else
    p^.IEX_ExecuteRead_std(handle, parametersOnly);
end;

procedure IEX_ExecuteWrite(p: PIEIOPlugin; handle: Pointer);
begin
  if p^.use_cdecl then
    p^.IEX_ExecuteWrite_cdecl(handle)
  else
    p^.IEX_ExecuteWrite_std(handle);
end;

function IEX_ExecuteTry(p: PIEIOPlugin; handle: Pointer): longbool;
begin
  if p^.use_cdecl then
    result := p^.IEX_ExecuteTry_cdecl(handle)
  else
    result := p^.IEX_ExecuteTry_std(handle);
end;

procedure IEX_AddParameter(p: PIEIOPlugin; handle: Pointer; param: PAnsiChar);
begin
  if p^.use_cdecl then
    p^.IEX_AddParameter_cdecl(handle, param)
  else
    p^.IEX_AddParameter_std(handle, param);
end;

procedure IEX_SetCallBacks(p: PIEIOPlugin; handle: Pointer; userData: Pointer);
begin
  if p^.use_cdecl then
    p^.IEX_SetCallBacks_cdecl(handle, CallBack_Progress_cdecl, CallBack_Read_cdecl, CallBack_Write_cdecl, CallBack_PositionSet_cdecl, CallBack_PositionGet_cdecl, CallBack_Length_cdecl, CallBack_GetParameter_cdecl, CallBack_SetParameter_cdecl, userData)
  else
    p^.IEX_SetCallBacks_std(handle, CallBack_Progress_std, CallBack_Read_std, CallBack_Write_std, CallBack_PositionSet_std, CallBack_PositionGet_std, CallBack_Length_std, CallBack_GetParameter_std, CallBack_SetParameter_std, userData)
end;

function IEX_Initialize(p: PIEIOPlugin; format: Integer): Pointer;
begin
  if p^.use_cdecl then
    result := p^.IEX_Initialize_cdecl(format)
  else
    result := p^.IEX_Initialize_std(format);
end;

procedure IEX_Finalize(p: PIEIOPlugin; handle: Pointer);
begin
  if p^.use_cdecl then
    p^.IEX_Finalize_cdecl(handle)
  else
    p^.IEX_Finalize_std(handle);
end;





// invert swap low-high bytes (only for 48bit RGB images)
procedure BGRHL(row: PRGB48; width: Integer);
var
  i: Integer;
begin
  for i := 0 to width - 1 do
  begin
    with row^ do
    begin
      r := IESwapWord(r);
      g := IESwapWord(g);
      b := IESwapWord(b);
    end;
    inc(row);
  end;
end;


function IsRawDLLFilename(const sDLLFileName : string) : boolean;
begin
  Result := pos('dcrawlib.dll', Lowercase(ExtractFilename(sDLLFilename))) > 0;
end;


procedure IEPlgInREAD(InputStream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
type
  plongbool = ^longbool;
var
  p: PIEIOPlugin;
  rec: TIECallbackRecord;
  handle: pointer;
  srcformat: integer;
  width, height, row: integer;
  image: pbyte;
  imagelen: integer;
  rl: integer;
  tempBitmap: TIEBitmap;
  q: integer;
  Stream: TIEBufferedReadStream;
  dd: double;
  tempio: TImageEnIO;
  memstream: TIEMemStream;
  {$ifdef IEINCLUDERAWFORMATS}
  lextra: AnsiString;
  {$endif}
begin
  p := IEGetIOPlugin( IOParams.FileType );
  if p = nil then
    exit;

  if IOParams.GetThumbnail then
  begin
    // try to load the (EXIF?) thumbnail
    if IOParams.EXIF_Bitmap <> nil then
      IOParams.EXIF_Bitmap.FreeImage(true);
    IOParams.GetThumbnail := false;
    IEPlgInREAD(InputStream, Bitmap, IOParams, Progress, true);
    IOParams.GetThumbnail := true;
    if assigned(IOParams.EXIF_Bitmap) and not IOParams.EXIF_Bitmap.IsEmpty then
    begin
      // success, exit
      Bitmap.Assign( IOParams.EXIF_Bitmap );
      exit;
    end;
    {$ifdef IEINCLUDERAWFORMATS}
    if IsRawDLLFilename(p^.DLLFileName) then
    begin
      // try to load with "-e" option, if raw
      lextra := IOParams.RAW_ExtraParams;
      IOParams.RAW_ExtraParams := '-e';
      IOParams.GetThumbnail := false;
      IEPlgInREAD(InputStream, Bitmap, IOParams, Progress, false);
      IOParams.GetThumbnail := true;
      IOParams.RAW_ExtraParams := lextra;
      if not Progress.Aborting^ then
        exit;
    end;
    {$endif}
    // continue loading the full image
    InputStream.Position := 0;
  end;

  if (p^.Capability and IEX_MULTITHREAD) = 0 then
    EnterCriticalSection(p^.PluginsCS);

  if Preview then
    Stream := TIEBufferedReadStream.Create(InputStream, 8192, IEGlobalSettings().UseRelativeStreams)
  else
    Stream := TIEBufferedReadStream.Create(InputStream, 1024 * 1024, IEGlobalSettings().UseRelativeStreams);


  rec.OnProgress := Progress.fOnProgress;
  rec.OnProgressSender := Progress.Sender;
  rec.Stream := Stream;
  rec.params := IOParams;
  Progress.Aborting^ := false;

  handle := IEX_Initialize(p, p^.pluginFormat);
  IEX_SetCallBacks(p, handle, @rec);

  try

    if plongbool(IEX_GetInfo(p, handle, IEX_AUTOSEARCHEXIF))^ then
    begin
      // ImageEn will search EXIF info inside the file
      Stream.Position := 0;
      q := IESearchEXIFInfo(Stream);
      if q >= 0 then
      begin
        Stream.Position := q;
        if IELoadEXIFFromTIFF(Stream, IOParams, true) then
        begin
          // use dpi of the Exif
          if IOParams.EXIF_ResolutionUnit = 3 then
            dd := CM_per_Inch
          else
            dd := 1;
          IOParams.DpiX := trunc(IOParams.EXIF_XResolution * dd);
          IOParams.DpiY := trunc(IOParams.EXIF_YResolution * dd);
        end;
      end
      else
      begin
        // is this a CRW? (with CIFF instead of EXIF?)
        Stream.Position := 0;
        {$ifdef IEINCLUDERAWFORMATS}
        IECRWGetCIFFAsExif(Stream, IOParams);
        {$endif}
      end;

      // look for IPTC
      Stream.Position := 0;
      tempio := TImageEnIO.Create(nil);
      try
        tempio.ParamsFromStreamFormat(Stream, ioTIFF);
        IOParams.IPTC_Info.Assign( tempio.Params.IPTC_Info );
        if not IOParams.EXIF_HasEXIFData and tempio.Params.EXIF_HasEXIFData then
          IECopyEXIF(tempio.Params, IOParams, true);
      finally
        tempio.Free();
      end;

      // reset position
      Stream.Position := 0;
    end;

    IEX_ExecuteRead(p, handle, Preview);

    width  := pinteger(IEX_GetInfo(p, handle, IEX_IMAGEWIDTH))^;
    height := pinteger(IEX_GetInfo(p, handle, IEX_IMAGEHEIGHT))^;
    srcformat := pinteger(IEX_GetInfo(p, handle, IEX_PIXELFORMAT))^;
    image := IEX_GetInfo(p, handle, IEX_IMAGEDATA);

    // set ioparams
    IOParams.Width  := width;
    IOParams.Height := height;
    if assigned(IEX_GetInfo(p, handle, IEX_ORIGINALIMAGEWIDTH)) then
    begin
      IOParams.OriginalWidth  := pinteger(IEX_GetInfo(p, handle, IEX_ORIGINALIMAGEWIDTH))^;
      IOParams.OriginalHeight := pinteger(IEX_GetInfo(p, handle, IEX_ORIGINALIMAGEHEIGHT))^;
    end
    else
    begin
      IOParams.OriginalWidth  := width;
      IOParams.OriginalHeight := height;
    end;
    case srcformat of
      IEX_1G     : begin IOParams.BitsPerSample :=  1; IOParams.SamplesPerPixel := 1; end;
      IEX_8G     : begin IOParams.BitsPerSample :=  8; IOParams.SamplesPerPixel := 1; end;
      IEX_8P     : begin IOParams.BitsPerSample :=  8; IOParams.SamplesPerPixel := 1; end;
      IEX_16G    : begin IOParams.BitsPerSample := 16; IOParams.SamplesPerPixel := 1; end;
      IEX_24RGB  : begin IOParams.BitsPerSample :=  8; IOParams.SamplesPerPixel := 3; end;
      IEX_48RGB  : begin IOParams.BitsPerSample := 16; IOParams.SamplesPerPixel := 3; end;
      IEX_32F    : begin IOParams.BitsPerSample := 32; IOParams.SamplesPerPixel := 1; end;
      IEX_CMYK   : begin IOParams.BitsPerSample :=  8; IOParams.SamplesPerPixel := 4; end;
    end;
    IOParams.FreeColorMap();

    if (image <> nil) and (width <> 0) and (height <> 0) and not Preview then
    begin

      tempBitmap := nil;

      if IOParams.IsNativePixelFormat then
      begin
        // native pixel format
        Bitmap.Allocate(width, height, TIEPixelFormat(srcformat));
      end
      else
      begin
        // convert to ie1g or ie24RGB
        if (srcformat = IEX_1G) then
          Bitmap.Allocate(width, height, ie1g)
        else
        if srcFormat = IEX_24RGB then
          Bitmap.Allocate(width, height, ie24RGB)
        else
        begin
          Bitmap.Allocate(width, height, ie24RGB);
          tempBitmap := TIEBitmap.Create();
          tempBitmap.Allocate(width, height, TIEPixelFormat(srcformat));
        end;
      end;

      for row := 0 to height - 1 do
      begin

        if tempBitmap = nil then
        begin
          // native pixel format of ie1g or ie24RGB
          rl := IEBitmapRowLen(width, Bitmap.BitCount, 8);
          CopyMemory( Bitmap.Scanline[row], image, rl );
          if Bitmap.PixelFormat = ie24RGB then
            _BGR2RGB( Bitmap.Scanline[row], width );
          if Bitmap.PixelFOrmat = ie48RGB then
            BGRHL( Bitmap.Scanline[row], width );
          inc(image, rl);
        end
        else
        begin
          // non-nativepixelformat or other pixel formats to convert to ie1g or ie24RGB
          rl := IEBitmapRowLen(width, tempBitmap.BitCount, 8);
          CopyMemory( tempBitmap.Scanline[row], image, rl );
          inc(image, rl);
        end;

      end;

      if tempBitmap <> nil then
      begin
        Bitmap.CopyAndConvertFormat(tempBitmap);
        FreeAndNil(tempBitmap);
      end;

      if IOParams.EXIF_HasEXIFData and IOParams.EnableAdjustOrientation then
      begin
        IEAdjustEXIFOrientation(Bitmap, IOParams.EXIF_Orientation);
        IOParams.EXIF_Orientation := 1; // 3.0.3
      end;

    end;

    if (image <> nil) and not Preview and (srcFormat = IEX_INVALID) then
    begin
      // try to load using known format (in case it is jpeg thumbnail)
      memstream := nil;
      tempio := TImageEnIO.CreateFromBitmap(Bitmap);
      try
        imagelen := pinteger(IEX_GetInfo(p, handle, IEX_IMAGEDATELENGTH))^;
        memstream := TIEMemStream.Create(image, imagelen);
        tempio.LoadFromStream(memstream);
        IOParams.Assign(tempio.Params);
      finally
        memstream.Free();
        tempio.Free();
      end;
    end;

    if (image = nil) and not Preview then
      Progress.Aborting^ := true;

  finally
    IEX_Finalize(p, handle);

    if Stream <> nil then
      FreeAndNil(Stream);
    if (p^.Capability and IEX_MULTITHREAD) = 0 then
      LeaveCriticalSection(p^.PluginsCS);
  end;

end;


function IEPlgInTRY(Stream: TStream; TryingFormat: TIOFileType): boolean;
var
  p: PIEIOPlugin;
  rec: TIECallbackRecord;
  handle: pointer;
begin
  p := IEGetIOPlugin( TryingFormat );
  if p = nil then
  begin
    result := false;
    exit;
  end;

  rec.OnProgress := nil;
  rec.OnProgressSender := nil;
  rec.Stream := Stream;
  rec.params := nil;

  if (p^.Capability and IEX_MULTITHREAD) = 0 then
    EnterCriticalSection(p^.PluginsCS);

  handle := IEX_Initialize(p, p^.pluginFormat);

  IEX_SetCallBacks(p, handle, @rec);

  try

    result := IEX_ExecuteTry(p, handle);

  finally
    IEX_Finalize(p, handle);

    if (p^.Capability and IEX_MULTITHREAD) = 0 then
      LeaveCriticalSection(p^.PluginsCS);
  end;

end;


procedure IEPlgInWRITE(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  p: PIEIOPlugin;
  rec: TIECallbackRecord;
  handle: pointer;
  i, rl, row: integer;
  mem: pointer;
  pd: pbyte;
begin
  p := IEGetIOPlugin( IOParams.FileType );
  if p = nil then
    exit;

  mem := nil;

  rec.OnProgress := Progress.fOnProgress;
  rec.OnProgressSender := Progress.Sender;
  rec.Stream := Stream;
  rec.params := IOParams;
  Progress.Aborting^ := false;

  if (p^.Capability and IEX_MULTITHREAD) = 0 then
    EnterCriticalSection(p^.PluginsCS);

  handle := IEX_Initialize(p, p^.pluginFormat);

  IEX_SetCallBacks(p, handle, @rec);

  i := Bitmap.Width;
  IEX_SetInfo(p, handle, IEX_IMAGEWIDTH, @i);
  i := Bitmap.Height;
  IEX_SetInfo(p, handle, IEX_IMAGEHEIGHT, @i);

  i := integer(Bitmap.PixelFormat);
  IEX_SetInfo(p, handle, IEX_PIXELFORMAT, @i);
  rl := IEBitmapRowLen(Bitmap.Width, Bitmap.BitCount, 8); // 8 bit aligned

  try

    getmem(mem, rl*Bitmap.Height);

    pd := mem;
    for row := 0 to Bitmap.Height - 1 do
    begin
      CopyMemory(pd, Bitmap.Scanline[row], rl);
      if Bitmap.PixelFormat = ie24RGB then
        _BGR2RGB( PRGB(pd), Bitmap.Width );
      inc(pd, rl);
    end;

    IEX_SetInfo(p, handle, IEX_IMAGEDATA, mem);
    IEX_ExecuteWrite(p, handle);

  finally
    freemem(mem);
    IEX_Finalize(p, handle);
  if (p^.Capability and IEX_MULTITHREAD) = 0 then
    LeaveCriticalSection(p^.PluginsCS);
  end;

end;


// return true is the specified plugins is already loaded
// does not compare file path
function IEIsExtIOPluginLoaded(const FileName: String): Boolean;
var
  p: PIEIOPlugin;
  i: integer;
begin
  result := false;
  for i := 0 to ioplugins.Count - 1 do
  begin
    p := ioplugins[i];
    if ExtractFileName(p^.DLLFileName) = ExtractFileName(string(FileName)) then
    begin
      result := true;
      break;
    end;
  end;
end;



{!!
<FS>IEAutoLoadIOPlugins

<FM>Declaration<FC>
function IEAutoLoadIOPlugins : Integer;

<FM>Description<FN>
Checks the application folder for any of the following plug-ins: dcrawlib.dll, jbiglib.dll and imagemagick.dll and automatically adds them by calling <A IEAddExtIOPlugin>.

Also checks for an installed version of ImageMagick (and optionally Ghostscript) using <A TIEMiscPluginsImageMagick>.

Note: If Dcraw.dll is loaded then internal Raw support will be disabled.

Result is the number of plug-ins that were loaded, or -1 in case of unexpected error.

<FM>Example<FC>
procedure TMainForm.FormCreate(Sender: TObject);
begin
  IEAutoLoadIOPlugins;
end;

<FM>See Also<FN>
- <A IEAddExtIOPlugin>
- <A Helper functions>

!!}
function IEAutoLoadIOPlugins : Integer;
const
  Dcrawlib_dll    = 'dcrawlib.dll';
  Jbiglib_dll     = 'jbiglib.dll';
  Imagemagick_dll = 'imagemagick.dll';
var
  sAppFolder: string;
begin
  Result := 0;
  try
    sAppFolder := IncludeTrailingBackslash(ExtractFilePath(Application.Exename));
    if IEFileExists(sAppFolder + Dcrawlib_dll) then
    begin
      IEFileFormatRemove(ioRAW);
      IEAddExtIOPlugIn(sAppFolder + Dcrawlib_dll);
      inc(Result);
    end;
    if IEFileExists(sAppFolder + Jbiglib_dll) then
    begin
      IEAddExtIOPlugIn(sAppFolder + Jbiglib_dll);
      inc(Result);
    end;      
    if IEFileExists(sAppFolder + Imagemagick_dll) then
    begin
      IEAddExtIOPlugIn(sAppFolder + Imagemagick_dll);
      inc(Result);
    end;
    {$ifdef IEINCLUDEMISCPLUGINS}
    if TIEMiscPluginsImageMagick.IsAvailable then
    begin
      TIEMiscPluginsImageMagick.RegisterPlugin;
      inc(Result);
    end;
    {$endif}
  except
    Result := -1;
  end;
end;


{!!
<FS>IEAddExtIOPlugin

<FM>Declaration<FC>
function IEAddExtIOPlugin(const FileName : string) : Integer;

<FM>Description<FN>
Adds support for extra file formats using the ImageEn input/output plug-ins (DLL's) that can be downloaded from the Registered Users download page.

Available plug-ins allow you to load and save JBIG, load camera RAW (using DCRAW) and other formats. All include source code if you need help to you write your own plug-ins.

Result is the FileType for the added formats, e.g. ioRawDLLPlugIn (for DCRAW), ioOtherDLLPlugIns, ioOtherDLLPlugIns + 1, etc. This can be used by methods such as <A TImageEnIO.LoadFromStreamFormat>.

<FM>Example<FC>
IEFileFormatRemove(ioRAW);
IEAddExtIOPlugIn('dcrawlib.dll');
IEAddExtIOPlugIn('jbiglib.dll');
IEAddExtIOPlugIn('imagemagick.dll');

<FN>The first instruction is needed only when you add dcrawlib.dll , because ImageEn already has an internal implementation of RAW format. In this way you disable the internal implementation.
A plugin can include more than one file format. For example, imagemagick.dll adds PCD, DICOM, FITS and many others.
Please read license terms for each plugin downloaded from imageen.com web site.

<FM>See Also<FN>
- <A IEAutoLoadIOPlugins>
- <A Helper functions>

!!}
function IEAddExtIOPlugin(const FileName: String): Integer;
var
  h: THandle;
  p: PIEIOPlugin;
  FileFormatInfo: TIEFileFormatInfo;
  formatsCount: integer;
  formatIndex: integer;
  done: boolean;
  ph: pointer;
  iPos: Integer;
  sExtensions: string;
  sSuitableExtension: string;
begin
  result := 0;

  if IEIsExtIOPluginLoaded(FileName) then
    exit; // already loaded

  formatIndex := 0;
  repeat

    done := true;
    h := LoadLibrary(PChar(FileName));
    if h <> 0 then
    begin

      new( p );

      p^.use_cdecl := GetProcAddress(h, 'IEX_UseCDECL') <> nil;  // this is a flag not a function!

      if p^.use_cdecl then
      begin
        p^.IEX_ExecuteRead_cdecl := GetProcAddress(h, 'IEX_ExecuteRead');
        p^.IEX_ExecuteWrite_cdecl := GetProcAddress(h, 'IEX_ExecuteWrite');
        p^.IEX_ExecuteTry_cdecl := GetProcAddress(h, 'IEX_ExecuteTry');
        p^.IEX_AddParameter_cdecl := GetProcAddress(h, 'IEX_AddParameter');
        p^.IEX_SetCallBacks_cdecl := GetProcAddress(h, 'IEX_SetCallBacks');
        p^.IEX_Initialize_cdecl := GetProcAddress(h, 'IEX_Initialize');
        p^.IEX_Finalize_cdecl := GetProcAddress(h, 'IEX_Finalize');
        p^.IEX_GetInfo_cdecl := GetProcAddress(h, 'IEX_GetInfo');
        p^.IEX_SetInfo_cdecl := GetProcAddress(h, 'IEX_SetInfo');
      end
      else
      begin
        // stdcall
        p^.IEX_ExecuteRead_std := GetProcAddress(h, 'IEX_ExecuteRead');
        p^.IEX_ExecuteWrite_std := GetProcAddress(h, 'IEX_ExecuteWrite');
        p^.IEX_ExecuteTry_std := GetProcAddress(h, 'IEX_ExecuteTry');
        p^.IEX_AddParameter_std := GetProcAddress(h, 'IEX_AddParameter');
        p^.IEX_SetCallBacks_std := GetProcAddress(h, 'IEX_SetCallBacks');
        p^.IEX_Initialize_std := GetProcAddress(h, 'IEX_Initialize');
        p^.IEX_Finalize_std := GetProcAddress(h, 'IEX_Finalize');
        p^.IEX_GetInfo_std := GetProcAddress(h, 'IEX_GetInfo');
        p^.IEX_SetInfo_std := GetProcAddress(h, 'IEX_SetInfo');
      end;

      if ((@p^.IEX_ExecuteRead_std = nil) or (@p^.IEX_ExecuteWrite_std = nil) or (@p^.IEX_AddParameter_std = nil) or (@p^.IEX_SetCallBacks_std = nil) or
          (@p^.IEX_Initialize_std = nil) or (@p^.IEX_Finalize_std = nil) or (@p^.IEX_GetInfo_std = nil) or (@p^.IEX_SetInfo_std = nil) or (@p^.IEX_ExecuteTry_std = nil)) and
         ((@p^.IEX_ExecuteRead_cdecl = nil) or (@p^.IEX_ExecuteWrite_cdecl = nil) or (@p^.IEX_AddParameter_cdecl = nil) or (@p^.IEX_SetCallBacks_cdecl = nil) or
          (@p^.IEX_Initialize_cdecl = nil) or (@p^.IEX_Finalize_cdecl = nil) or (@p^.IEX_GetInfo_cdecl = nil) or (@p^.IEX_SetInfo_cdecl = nil) or (@p^.IEX_ExecuteTry_cdecl = nil)) then
      begin
        // this is not a plugin dll
        FreeLibrary(h);
        dispose( p );
      end
      else
      begin
        // this is OK
        ph := IEX_Initialize(p, formatIndex);
        formatsCount := pinteger(IEX_GetInfo(p, ph, IEX_FORMATSCOUNT))^;
        p^.libhandle := h;
        p^.DLLFileName := FileName;
        if IsRawDLLFilename(FileName) then   
          p^.FileType := ioRawDLLPlugIn
        else
          p^.FileType := ioOtherDLLPlugIns + ioplugins.Count;
        p^.pluginFormat := formatIndex;
        FileFormatInfo := TIEFileFormatInfo.Create();
        FileFormatInfo.FileType := p^.FileType;
        FileFormatInfo.FullName := string(PAnsiChar(IEX_GetInfo(p, ph, IEX_PLUGINDESCRIPTOR))); // plugins can send only ansichars

        sExtensions := string(PAnsiChar(IEX_GetInfo(p, ph, IEX_FILEEXTENSIONS))); // plugins can send only ansichars
        FileFormatInfo.Extensions := sExtensions;

        sSuitableExtension := sExtensions;
        iPos := Pos(';', sSuitableExtension);
        if iPos > 0 then
          delete(sSuitableExtension, iPos, Length(sSuitableExtension) - iPos + 1);
        FileFormatInfo.SuitableExtension := sSuitableExtension;

        FileFormatInfo.DialogPage := [];
        FileFormatInfo.InternalFormat := false;
        p^.Capability := pinteger( IEX_GetInfo(p, ph, IEX_PLUGINCAPABILITY) )^;
        if (p^.Capability and IEX_FILEREADER) <> 0 then
          FileFormatInfo.ReadFunction := IEPlgInREAD;
        if (p^.Capability and IEX_FILEREADER) <> 0 then
          FileFormatInfo.TryFunction := IEPlgInTRY;
        if (p^.Capability and IEX_FILEWRITER) <> 0 then
          FileFormatInfo.WriteFunction := IEPlgInWRITE;
        if (p^.Capability and IEX_MULTITHREAD) = 0 then
          InitializeCriticalSection(p^.PluginsCS);
        ioplugins.Add( p );
        IEFileFormatAdd( FileFormatInfo );
        result := p^.FileType;
        inc(formatIndex);
        done := formatIndex = formatsCount;
        IEX_Finalize(p, ph);
      end;

    end;

  until done;
end;


procedure IEUnLoadIOPlugins;
var
  i: integer;
  p: PIEIOPlugin;
begin
  for i := 0 to ioplugins.Count - 1 do
  begin
    p := ioplugins[i];
    IEFileFormatRemove( p^.FileType );
    if (p^.Capability and IEX_MULTITHREAD) = 0 then
      DeleteCriticalSection(p^.PluginsCS);
    FreeLibrary( p^.libhandle );
    dispose( p );
  end;
  ioplugins.Clear();
end;

// input/output plugins
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>IEOptimizeGIF

<FM>Declaration<FC>
procedure IEOptimizeGIF(const InputFile, OutputFile: WideString);

<FM>Description<FN>
Optimizes a GIF animation (or multi-page GIF) by detecting differences between each frame and removing any data that is duplicated. The resulting file will only include differences between frames and can be significantly smaller.

<FM>Example<FC>
ImageEnMView1.MIO.SaveToFile('D:\temp.gif');
IEOptimizeGIF('D:\temp.gif', 'D:\output.gif');

!!}
procedure IEOptimizeGIF(const InputFile, OutputFile: WideString);
var
  i: Integer;
  ie: TImageEnView;
  prev: TIEBitmap;
  x, y, w, h: Integer;
  p1, p2: PRGB;
  x1, y1, x2, y2: Integer;
  a1, a2: pbyte;
  ielist: TList;
begin
  ielist := TList.Create;
  prev := TIEBitmap.Create;
  i := 0;
  repeat
    ie := TImageEnView.Create(nil);
    ielist.Add(ie);
    ie.IO.Params.GIF_ImageIndex := i;
    ie.IO.LoadFromFileGIF(InputFile);

    if i = 0 then
    begin
      // this is the first frame, save as is
      ie.IO.Params.GIF_Action := ioGIF_DontRemove;
      prev.Assign( ie.IEBitmap );
    end
    else
    begin
      // this is not first frame, check differences with "prev" bitmap
      x1 := 1000000;
      y1 := 1000000;
      x2 := 0;
      y2 := 0;
      w := imin(prev.Width, ie.IEBitmap.Width);
      h := imin(prev.Height, ie.IEBitmap.Height);
      y := 0;
      while y<h do
      begin
        p1 := prev.Scanline[y];
        a1 := prev.AlphaChannel.Scanline[y];
        p2 := ie.IEBitmap.Scanline[y];
        a2 := ie.IEBitmap.AlphaChannel.Scanline[y];
        for x := 0 to w - 1 do
        begin
          if (p1^.r<>p2^.r) or (p1^.g<>p2^.g) or (p1^.b<>p2^.b) then
          begin
            if x < x1 then
              x1 := x;
            if x > x2 then
              x2 := x;
            if y < y1 then
              y1 := y;
            if y > y2 then
              y2 := y;
          end;
          if (a2^ = 0) and (a1^=255) then
          begin
            x1 := 1000000;
            y1 := 1000000;
            x2 := 0;
            y2 := 0;
            y := h;
            break;
          end;
          inc(p1);
          inc(p2);
          inc(a1);
          inc(a2);
        end;
        inc(y);
      end;
      prev.Assign( ie.IEBitmap );
      if (x1 <> 1000000) and (y1 <> 1000000) and (x2 <> 0) and (y2 <> 0) then
      begin
        ie.Proc.Crop(x1, y1, x2, y2);
        ie.IO.Params.GIF_XPos := x1;
        ie.IO.Params.GIF_YPos := y1;
        ie.IO.Params.GIF_WinWidth  := prev.Width;
        ie.IO.Params.GIF_WinHeight := prev.Height;
        ie.IO.Params.GIF_Action := ioGIF_DontRemove;
      end
      else
        ie.IO.Params.GIF_Action := ioGIF_DrawBackground;
    end;

    inc(i);
  until i=ie.IO.Params.GIF_ImageCount;

  for i := 0 to ielist.Count - 1 do
  begin
    ie := ielist[i];
    if i < ielist.Count - 1 then
      ie.IO.Params.GIF_Action := TImageEnView(ielist[i + 1]).IO.Params.GIF_Action;
    if i = 0 then
      ie.IO.SaveToFileGIF(OutputFile)
    else
      ie.IO.InsertToFileGIF(OutputFile);
    FreeAndNil(ie);
  end;

  FreeAndNil(prev);
  FreeAndNil(ielist);
end;

procedure TImageEnIO.DoIntProgress(Sender: TObject; per: integer);
begin
  if assigned(fOnProgress) then
  begin
    if assigned(fImageEnView) and (fImageEnView.Parent <> nil) and IsInsideAsyncThreads then
    begin
      // we are inside a thread, so send a message to the parent TImageEnView component, if exists
      {$ifdef IEHASTTHREADSTATICSYNCHRONIZE}
      if not fImageEnView.HandleAllocated then // 3.0.2, to avoid deadlocks
        TThread.Synchronize(nil, SyncGetHandle); // 3.0.1, 15/7/2008 - 13:55
      {$endif}
      PostMessage( fImageEnView.Handle, IEM_PROGRESS, per, 0);
    end
    else
      fOnProgress(Sender, per);
  end;
end;

procedure TImageEnIO.DoFinishWork;
begin
  fOnIntProgress(self, 100);  // fOnIntProgress is always assigned
  if assigned(fOnFinishWork) then
  begin
    if assigned(fImageEnView) and (fImageEnView.Parent <> nil) and IsInsideAsyncThreads then
    begin
      // we are inside a thread, so send a message to the parent TImageEnView component, if exists
      {$ifdef IEHASTTHREADSTATICSYNCHRONIZE}
      if not fImageEnView.HandleAllocated then // 3.0.2, to avoid deadlocks
        TThread.Synchronize(nil, SyncGetHandle); // 3.0.1, 15/7/2008 - 13:55
      {$endif}
      PostMessage( fImageEnView.Handle, IEM_FINISHWORK, 0, 0);
    end
    else
      fOnFinishWork(self);
  end;
end;

{!!
<FS>TImageEnIO.LoadFromStreamPSD

<FM>Declaration<FC>
function LoadFromStreamPSD(Stream: TStream): Boolean;

<FM>Description<FN>    
Loads an image from a stream containing an Adobe PSD file. The result will be false if an error is encountered, e.g. the file in the stream is not PSD format (<A TImageEnIO.Aborting> will be true).

PSD files can contain multiple layers. To load separated layers:<FC>
ImageEnView1.IO.Params.PSD_LoadLayers := True;
ImageEnView1.IO.LoadFromStreamPSD(stream);<FN>

To load only the merged/flattened image (default method):<FC>
ImageEnView1.IO.Params.PSD_LoadLayers := False;
ImageEnView1.IO.LoadFromStreamPSD(stream);<FN>
!!}
{$ifdef IEINCLUDEPSD}
function TImageEnIO.LoadFromStreamPSD(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin           
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamPSD, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamPSD(Stream); 
    Result := Not fAborting;
  end;  
end;
{$endif}

{!!
<FS>TImageEnIO.LoadFromFilePSD

<FM>Declaration<FC>
function LoadFromFilePSD(const FileName: WideString): Boolean;

<FM>Description<FN>                
Loads an image from an Adobe PSD file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not PSD format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.


PSD files can contain multiple layers. To load separated layers:<FC>
ImageEnView1.IO.Params.PSD_LoadLayers := True;
ImageEnView1.IO.LoadFromFilePSD(filename);<FN>

To load only the merged/flattened image (default option):<FC>
ImageEnView1.IO.Params.PSD_LoadLayers := False;
ImageEnView1.IO.LoadFromFilePSD(filename);<FN>

<FM>Example<FC>
ImageEnView1.IO.Params.PSD_LoadLayers := True;
ImageEnView1.IO.LoadFromFilePSD('C:\input.psd');
!!}
{$ifdef IEINCLUDEPSD}
function TImageEnIO.LoadFromFilePSD(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin                
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFilePSD, FileName);
    Result := True;
    exit;
  end;

  fs := TIEWideFileStream.create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamPSD(fs);
    fParams.fFileName := FileName;    
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
end;
{$endif}

{$ifdef IEINCLUDEPSD}
procedure TImageEnIO.SyncLoadFromStreamPSD(Stream: TStream);
var
  Progress: TProgressRec;
  layers: TList;
  i, l: Integer;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;

    if Params.PSD_LoadLayers and Params.PSD_ReplaceLayers and assigned(fImageEnView) and (fImageEnView is TImageEnView) then
      // working with TImageEnView, remove all layers
      (fImageEnView as TImageEnView).LayersClear;

    fIEBitmap.RemoveAlphaChannel;
    layers := TList.Create;
    IEReadPSD(Stream, fIEBitmap, fParams, Progress, fParams.PSD_LoadLayers, layers);

    if Params.PSD_LoadLayers and assigned(fImageEnView) and (fImageEnView is TImageEnView) then
    begin
      // working with TImageEnView
      if layers.Count > 0 then
      begin
        if Params.PSD_ReplaceLayers then
          l := 0
        else
          l := (fImageEnView as TImageEnView).LayersAdd(1, 1);

        // create a transparent layer 0 when the first layer is moved
        with TIELayer(layers[0]) do
          if (PosX <> 0) or (PosY <> 0) then
          begin
            (fImageEnView as TImageEnView).Layers[l].SetDefaults;
            (fImageEnView as TImageEnView).Layers[l].Locked := True;
            (fImageEnView as TImageEnView).Layers[l].Bitmap.Allocate(fParams.Width, fParams.Height);
            (fImageEnView as TImageEnView).Layers[l].Bitmap.AlphaChannel.Fill(0);
            (fImageEnView as TImageEnView).LayersAdd(1, 1);
            inc(l);
          end;

        for i := 0 to layers.Count - 1 do
        begin
          if i > 0 then
            l := (fImageEnView as TImageEnView).LayersAdd(1, 1);
          if (fImageEnView as TImageEnView).LegacyBitmap then
            (fImageEnView as TImageEnView).Layers[l].Assign( TIELayer(layers[i]) )  // TIELayer.Swap is not possible when using LegacyBitmap (causes memory leak)
          else
            (fImageEnView as TImageEnView).Layers[l].Swap( TIELayer(layers[i]) );
        end;
      end;
    end
    else

    if (layers.Count = 1) and not fParams.GetThumbnail then
    begin
      // One layer or TImageEnIO
      fIEBitmap.AlphaChannel.Assign( TIELayer(layers[0]).Bitmap.AlphaChannel );
      if (fIEBitmap.Width <> fIEBitmap.AlphaChannel.Width) or (fIEBitmap.Height <> fIEBitmap.AlphaChannel.Height) then
      begin
        // an alpha channel can be moved and of different size
        fIEBitmap.AlphaChannel.Resize(fIEBitmap.AlphaChannel.Width+TIELayer(layers[0]).PosX,
                                      fIEBitmap.AlphaChannel.Height+TIELayer(layers[0]).PosY, 0, 0, iehRight, ievBottom);
        fIEBitmap.AlphaChannel.Resize(fIEBitmap.Width, fIEBitmap.Height, 0, 0, iehLeft, ievTop);
      end;
    end;

    for i := 0 to layers.count - 1 do
      TIELayer(layers[i]).Free();
    layers.Free();

    CheckDPI;
    if fAutoAdjustDPI then
      AdjustDPI;
    fParams.fFileName := '';
    fParams.fFileType := ioPSD;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    Update;
  finally
    DoFinishWork;
  end;
end;
{$endif}

{$ifdef IEINCLUDEPSD}
procedure TImageEnIO.SyncSaveToStreamPSD(Stream: TStream);
var
  Progress: TProgressRec;
  merged, view: TImageEnView;
  layers: TList;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    if assigned(fImageEnView) and (fImageEnView is TImageEnView) then
      view := (fImageEnView as TImageEnView)
    else
      view := nil;
    if (view <> nil) then
    begin
      merged := TImageEnView.Create(nil);
      merged.LegacyBitmap := False;
      view.LayersDrawTo(merged.IEBitmap);
      merged.IEBitmap.Location := ieMemory;
      merged.IEBitmap.PixelFormat := view.Layers[0].Bitmap.PixelFormat;
      layers := view.LayersList;
      IEWritePSD(Stream, fParams, Progress, merged.IEBitmap, layers);
      merged.free;
    end
    else
    begin
      layers := TList.Create;
      IEWritePSD(Stream, fParams, Progress, fIEBitmap, layers);
      layers.free;
    end;
  finally
    DoFinishWork;
  end;
end;
{$endif}

{!!
<FS>TImageEnIO.SaveToStreamPSD

<FM>Declaration<FC>
procedure SaveToStreamPSD(Stream: TStream);

<FM>Description<FN>             
Saves the current image to a stream in PSD format. PSD supports multiple layers, so it can store position and other useful info (like layer name). PSD also have a merged representation of the image to fast preview the merged result.
!!}
{$ifdef IEINCLUDEPSD}
procedure TImageEnIO.SaveToStreamPSD(Stream: TStream);
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin                    
    // ASYNC SAVING
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamPSD, Stream);
  end
  else
  begin
    SyncSaveToStreamPSD(Stream);
  end;
end;
{$endif}

{!!
<FS>TImageEnIO.SaveToFilePSD

<FM>Declaration<FC>
procedure SaveToFilePSD(const FileName: WideString);

<FM>Description<FN>               
Saves the current image or all layers to a file in PSD format. PSD supports multiple layers, so it can store position and other useful info (like layer name).
PSD files also have a merged representation of the image to fast preview the merged result.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

!!}
{$ifdef IEINCLUDEPSD}
procedure TImageEnIO.SaveToFilePSD(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin     
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFilePSD, FileName);
    exit;
  end;

  
  fs := nil;
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fParams.PSD_LargeDocumentFormat := IEFilenameInExtensions(string(FileName), '*.psb'); // automatically selec large document format for PSB
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    SyncSaveToStreamPSD(fs);
    fParams.FileName := FileName;
    fParams.FileType := ioPSD;
  finally
    FreeAndNil(fs);
  end;
end;
{$endif}

{!!
<FS>IEFindNumberWithKnownFormat

<FM>Declaration<FC>
function IEFindNumberWithKnownFormat( const Directory: WideString ): integer;

<FM>Description<FN>
Returns the number of images in a specified folder that are of a format supported by ImageEn.
!!}
// Returns the number of Images with KnownFormat in a Folder
function IEFindNumberWithKnownFormat( const Directory: WideString ): integer;
var
 Found: integer;
 SR: TSearchRec;
 FPath: TFileName;
begin
  Result := 0;
  Found := FindFirst ( Directory + '\*.*', $00000020, SR ); // $00000020=faArchive
  if Found = 0 then
  begin
    while Found = 0 do
    begin
      FPath := Directory + '\' + SR.Name;
      if IsKnownFormat ( FPath ) and ( SR.Attr and $10 = 0 ) then
        inc ( Result );
      Found := FindNext ( SR );
    end;
    FindClose ( SR );
  end;
end;

procedure TImageEnIO.SetPrintLogFile(v: String);
begin
  if iegPrintLogFileName <> '' then
    CloseFile(iegPrintLogFile);
  iegPrintLogFileName := v;
  if v <> '' then
  begin
    AssignFile(iegPrintLogFile, iegPrintLogFileName);
    Rewrite(iegPrintLogFile);
  end;
end;

function TImageEnIO.GetPrintLogFile: String;
begin
  result := iegPrintLogFileName;
end;


procedure IEPrintLogWrite(const ss: String);
begin
  if iegPrintLogFileName <> '' then
  begin
    closefile(iegPrintLogFile);
    assignfile(iegPrintLogFile, string(iegPrintLogFileName));
    append(iegPrintLogFile);
    WriteLn(iegPrintLogFile, datetostr(date) + ' ' + timetostr(time) + ' : ' + ss);
    Flush(iegPrintLogFile);
  end;
end;


{!!
<FS>TImageEnIO.ReplaceStreamTIFF

<FM>Declaration<FC>
function ReplaceStreamTIFF(Stream: TStream): integer;

<FM>Description<FN>
Save the current image into the specified multi-page TIFF file, replacing the image at the index specified by <A TImageEnIO.Params>.<A TIOParamsVals.TIFF_ImageIndex>.

Note: The Stream position must be at the beginning of the TIFF content.
!!}
function TImageEnIO.ReplaceStreamTIFF(Stream: TStream): integer;
var
  lpos: int64;
begin
  lpos := Stream.Position;
  TIFFDeleteImStream(Stream, fParams.TIFF_ImageIndex);
  Stream.Position := lpos;
  result := InsertToStreamTIFF(Stream);
end;

// 3.0.2
procedure TImageEnIO.SetViewerDPI(dpix, dpiy: Integer);
begin
  if assigned(fImageEnView) and (fImageEnView is TImageEnView) then
    with fImageEnView as TImageEnView do
    begin
      LockUpdate;
      SetDpi(fParams.dpix, fParams.dpiy);
      UnlockUpdateEx; // do not call Update
    end;
end;

{$ifdef IEINCLUDEWIC}
{!!
<FS>TImageEnIO.LoadFromStreamHDP

<FM>Declaration<FC>
function LoadFromStreamHDP(Stream: TStream): Boolean;

<FM>Description<FN>
Loads an image from a stream containing a Microsoft HD Photo file. The result will be false if an error is encountered, e.g. the file in the stream is not HDP format (<A TImageEnIO.Aborting> will be true).

Requires: Windows XP (SP2) with .Net 3.0, Windows Vista or later.
!!}
function TImageEnIO.LoadFromStreamHDP(Stream: TStream): Boolean;
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin                            
    // ASYNC LOADING
    TIEIOThread.CreateLoadSaveStreamRetBool(self, LoadFromStreamHDP, Stream);
    Result := True;
  end
  else
  begin
    SyncLoadFromStreamHDP(Stream); 
    Result := Not fAborting;
  end;
end;

{!!
<FS>TImageEnIO.LoadFromFileHDP

<FM>Declaration<FC>
function LoadFromFileHDP(const FileName: WideString): Boolean;

<FM>Description<FN>         
Loads an image from a Microsoft HD Photo file. 

<FC>FileName<FN> is the file name including extension.
Result will be false if the file is not HDP format (and <A TImageEnIO.Aborting> will be false). Loading errors due to a file not being available will raise an exception.

Requires: Windows XP (SP2) with .Net 3.0, Windows Vista or later.
!!}
function TImageEnIO.LoadFromFileHDP(const FileName: WideString): Boolean;
var
  fs: TIEWideFileStream;
begin                
  // ASYNC LOADING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFileRetBool(self, LoadFromFileHDP, FileName);
    Result := True;
    exit;
  end;

  
  fs := TIEWideFileStream.create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SyncLoadFromStreamHDP(fs);
    fParams.fFileName := FileName;          
    Result := Not fAborting;
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnIO.SaveToStreamHDP

<FM>Declaration<FC>
procedure SaveToStreamHDP(Stream: TStream);

<FM>Description<FN>      
Saves the current image to a stream in Microsoft HD Photo file format.

Requires: Windows XP (SP2) with .Net 3.0, Windows Vista or higher.
!!}
procedure TImageEnIO.SaveToStreamHDP(Stream: TStream);
begin
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin         
    // ASYNC SAVING
    TIEIOThread.CreateLoadSaveStream(self, SaveToStreamHDP, Stream);
  end
  else
  begin
    SyncSaveToStreamHDP(Stream);
  end;
end;

{!!
<FS>TImageEnIO.SaveToFileHDP

<FM>Declaration<FC>
procedure SaveToFileHDP(const FileName: WideString);

<FM>Description<FN>     
Saves the current image to a file in Microsoft HD Photo format.

<FC>FileName<FN> is the file name including extension.

Note: If an internal save error is encountered <A TImageEnIO.Aborting> will return true. Saving issues due to insufficient write permissions and disk write failures will raise an exception.

Requires: Windows XP (SP2) with .Net 3.0, Windows Vista or higher.
!!}
procedure TImageEnIO.SaveToFileHDP(const FileName: WideString);
var
  fs: TIEWideFileStream;
begin                       
  // ASYNC SAVING
  if (not fIEBitmapCreated) and fAsyncMode and (not IsInsideAsyncThreads) then
  begin
    TIEIOThread.CreateLoadSaveFile(self, SaveToFileHDP, FileName);
    exit;
  end;

  
  fs := nil;
  try
    fAborting := true; // So that fAborting is True if the file is not found/accessible
    fs := TIEWideFileStream.Create(FileName, fmCreate);
    SyncSaveToStreamHDP(fs);
    fParams.FileName := FileName;
    fParams.FileType := ioHDP;
  finally
    FreeAndNil(fs);
  end;
end;

procedure TImageEnIO.SyncLoadFromStreamHDP(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    fParams.ResetInfo;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    fIEBitmap.RemoveAlphaChannel;
    IEHDPRead(Stream, fIEBitmap, fParams, Progress, false);
    CheckDPI;
    if fAutoAdjustDPI then
      AdjustDPI;
    fParams.fFileName := '';
    fParams.fFileType := ioHDP;
    SetViewerDPI(fParams.DpiX, fParams.DpiY);
    Update;
  finally
    DoFinishWork;
  end;
end;

procedure TImageEnIO.SyncSaveToStreamHDP(Stream: TStream);
var
  Progress: TProgressRec;
begin
  try
    fAborting := false;
    Progress.Aborting := @fAborting;
    if not MakeConsistentBitmap([]) then
      exit;
    Progress.fOnProgress := fOnIntProgress;
    Progress.Sender := Self;
    IEHDPWrite(Stream, fIEBitmap, fParams, Progress);
  finally
    DoFinishWork;
  end;
end;

{$endif}  // HDP






{$ifdef IEINCLUDERESOURCEEXTRACTOR}


{!!
<FS>TImageEnIO.LoadFromResource

<FM>Declaration<FC>
procedure LoadFromResource(const ModulePath: WideString; const ResourceType: String; const ResourceName: String; Format: TIOFileType);

<FM>Description<FN>
Loads the specified image resource from PE files like EXE, DLL, OCX, ICL, BPL, etc.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ModulePath<FN></C> <C>Specifies the path and filename of PE module.</C> </R>
<R> <C><FC>ResourceType<FN></C> <C>Resource type as string (ie 'Bitmap', 'Cursor').</C> </R>
<R> <C><FC>ResourceName<FN></C> <C>Resource name as string (ie 'INTRESOURCE:100', 'Hand').</C> </R>
<R> <C><FC>Format<FN></C> <C>Specifies the expected file format. If Format is ioUnknown, then try to find the format automatically. ioUnknown should fail for BMP, ICO and CUR resources.</C> </R>
</TABLE>

See also: <A TIEResourceExtractor>

<FM>Example<FC>
// Load resource 143, in "Bitmap"s, from "explorer.exe" (should be a small Windows logo)
ImageEnView1.IO.LoadFromResource('explorer.exe', 'Bitmap', 'INTRESOURCE:143', ioBMP);

!!}
procedure TImageEnIO.LoadFromResource(const ModulePath: WideString; const ResourceType: String; const ResourceName: String; Format: TIOFileType);
var
  re: TIEResourceExtractor;
  buffer: Pointer;
  bufferLen: Integer;
begin
  re := TIEResourceExtractor.Create(ModulePath);
  try
    buffer := re.GetBuffer(AnsiString(ResourceType), AnsiString(ResourceName), bufferLen);
    fParams.IsResource := true;
    LoadFromBuffer(buffer, bufferLen, format);
  finally
    fParams.IsResource := false;
    re.Free;
  end;
end;

{$endif}  // IEINCLUDERESOURCEEXTRACTOR




{!!
<FS>TImageEnIO.Seek

<FM>Declaration<FC>
function Seek(Page : <A TImageEnIO.TIEIOSeekDestination>; FileName : WideString = ''): Integer;

<FM>Description<FN>
Load the specified page from the last loaded multipage file, including TIFF, GIF, AVI and other video types.
This method uses <A TImageEnIO.LoadFromFile> to load the file (hence the file name must be provided), unless it is part of <A TImageEnDBView> or <A TImageEnDBVect> where LoadPicture is called instead.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Page<FN></C> <C>Page to load.</C> </R>
<R> <C><FC>FileName<FN></C> <C>Optional file name. If empty will use the last loaded file name.</C> </R>
</TABLE>

Returns the loaded page index.

<FM>Example<FC>
// load first page
ImageEnView1.IO.Seek(ieioSeekFirst, 'multipage.tiff');

// load next page
ImageEnView1.IO.Seek(ieioSeekNext);
!!}
function TImageEnIO.Seek(Destination: TIEIOSeekDestination; FileName: WideString = ''): Integer;
var
  fn: WideString;
begin
  result := 0;
  case Destination of
    ieioSeekFirst:
      result := 0;
    ieioSeekPrior:
      result := imax(0, fParams.ImageIndex - 1);
    ieioSeekNext:
      result := imin(fParams.ImageCount - 1, fParams.ImageIndex + 1);
    ieioSeekLast:
      result := fParams.ImageCount - 1;
  end;

  if FileName = '' then
    fn := fParams.FileName
  else
    fn := FileName;

  {$ifdef IESUPPORTINTERFACESUPPORT}
  if assigned(fImageEnView) and Supports(fImageEnView, IIELoadPicture) then
  begin
    fParams.ImageIndex := result;
    (fImageEnView as IIELoadPicture).LoadPicture();
  end
  else
  {$endif}
  begin
    if (FileName = '') and (result = fParams.ImageIndex) then
      exit; // nothing to load
    fParams.ImageIndex := result;
    LoadFromFile(fn);
  end;
end;



constructor TIEFileFormatInfo.Create();
begin
end;

constructor TIEFileFormatInfo.Create(FileType: TIOFileType; FullName: string; Extensions: string; SuitableExtension : string; InternalFormat: Boolean; DialogPage: TPreviewParams; ReadFunction: TIEReadImageStream; WriteFunction: TIEWriteImageStream; TryFunction: TIETryImageStream);
begin
  self.FileType          := FileType;
  self.FullName          := FullName;
  self.Extensions        := Extensions;
  self.SuitableExtension := SuitableExtension;
  self.InternalFormat    := InternalFormat;
  self.DialogPage        := DialogPage;
  self.ReadFunction      := ReadFunction;
  self.WriteFunction     := WriteFunction;
  self.TryFunction       := TryFunction
end;



procedure IEInitialize_imageenio;
begin
  IEGlobalSettings().DefGIF_LZWDECOMPFUNC := GIFLZWDecompress;
  IEGlobalSettings().DefGIF_LZWCOMPFUNC := GIFLZWCompress;
  IEGlobalSettings().DefTIFF_LZWDECOMPFUNC := TIFFLZWDecompress;
  IEGlobalSettings().DefTIFF_LZWCOMPFUNC := TIFFLZWCompress;
  IEInitFileFormats;
  gAVIFILEinit := false;
  {$IFDEF IEINCLUDEIEXACQUIRE}
  iegTwainLogName := '';
  {$ENDIF}
  wininet := 0;
  IEInitWinINet;
  {$IFDEF IEREGISTERTPICTUREFORMATS}
  IERegisterFormats;
  {$ENDIF}
  ioplugins := TList.Create();
  CMYKProfile := nil;
end;

procedure IEFinalize_imageenio;
begin
  FreeAndNil(CMYKProfile);
  FreeAndNil(SRGBProfile);
  {$IFDEF IEREGISTERTPICTUREFORMATS}
  IEUnregisterFormats;
  {$ENDIF}
  IEUnLoadIOPlugins;
  IEFreeFileFormats;
  if gAVIFILEinit then
    AviFileExit;
  IEFreeWinINet;
  {$IFDEF IEINCLUDEIEXACQUIRE}
  if iegTwainLogName <> '' then
    closefile(iegTwainLogFile);
  {$ENDIF}
  FreeAndNil(ioplugins);
end;



end.



