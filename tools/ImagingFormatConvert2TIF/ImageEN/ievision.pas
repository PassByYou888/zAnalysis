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


unit ievision;

{$R-}
{$Q-}
{$Z4}

{$I ie.inc}


{$ifdef IEVISION}


interface


uses
  Windows, SysUtils, Classes;


resourcestring

  IERS_IEVISIONNOTFOUND = 'ievision.dll, ievision32.dll, ievision64.dll, ielib.dll or ielib64.dll not found';


const

{!!
<FS>IEVision Embedded Classifiers

<FM>Declaration<FC>
}
IEVC_EYE                      = ':EYE';
IEVC_EYE_TREE_EYE_GLASSES     = ':EYETREEEYEGLASSES';
IEVC_FRONTAL_FACE_ALT         = ':FRONTALFACEALT';
IEVC_FRONTAL_FACE_ALT_2       = ':FRONTALFACEALT2';
IEVC_FRONTAL_FACE_ALT_TREE    = ':FRONTALFACEALTTREE';
IEVC_FRONTAL_FACE_DEFAULT     = ':FRONTALFACEDEFAULT';
IEVC_FULL_BODY                = ':FULLBODY';
IEVC_LOWER_BODY               = ':LOWERBODY';
IEVC_PROFILE_FACE             = ':PROFILEFACE';
IEVC_UPPER_BODY               = ':UPPERBODY';
IEVC_LEFT_EYE_2_SPLITS        = ':LEFTEYE2SPLITS';
IEVC_MCS_EYE_PAIR_BIG         = ':MCSEYEPAIRBIG';
IEVC_MCS_EYE_PAIR_SMALL       = ':MCSEYEPAIRSMALL';
IEVC_MCS_LEFT_EYE             = ':MCSLEFTEYE';
IEVC_MCS_MOUTH                = ':MCSMOUTH';
IEVC_MCS_NOSE                 = ':MCSNOSE';
IEVC_MCS_RIGHT_EYE            = ':MCSRIGHTEYE';
IEVC_MCS_UPPER_BODY           = ':MCSUPPERBODY';
IEVC_RIGHT_EYE_2_SPLITS       = ':RIGHTEYE2SPLITS';
IEVC_MCS_LEFT_EAR             = ':MCSLEFTEAR';
IEVC_MCS_RIGHT_EAR            = ':MCSRIGHTEAR';
{!!}

type


  ///////////////////////////////////////////////////
  // primitive types

  uint8_t  = byte;
  uint16_t = word;
  uint32_t = longword;
  int8_t   = shortint;
  int16_t  = smallint;
  int32_t  = integer;
  int64_t  = int64;
  bool32   = longbool;

  uint8_p  = ^uint8_t;
  uint16_p = ^uint16_t;
  uint32_p = ^uint32_t;
  int8_p   = ^int8_t;
  int16_p  = ^int16_t;
  int32_p  = ^int32_t;
  int64_p  = ^int64_t;
  bool32_p = ^bool32;

  float_p  = ^single;
  double_p = ^double;


  ///////////////////////////////////////////////////
  // enums

  {$WARNINGS OFF}



{!!
<FS>TIEVisionLibraryInfo

<FM>Declaration<FC>
}
  TIEVisionLibraryInfo = (
    ievLIBNAME           = 0,
    ievLIBVERSION        = 1,
    ievLIBPLATFORM       = 2,
    ievLIBFILENAME       = 3,
    ievISTRIAL           = 4
  );
{!!}



{!!
<FS>TIEVisionChannelFormat

<FM>Declaration<FC>
}
  TIEVisionChannelFormat = (
    ievUINT8   = 0,         // 8 bit unsigned
    ievSINT8   = 1,         // 8 bit signed
    ievUINT16  = 2,         // 16 bit unsigned
    ievSINT16  = 3,         // 16 bit signed
    ievSINT32  = 4,         // 32 bit signed
    ievFLOAT32 = 5,         // 32 bit floating point
    ievFLOAT64 = 6,         // 64 bit floating point
    ievUINT1   = $A0000001  // 1 bit per pixel
  );
{!!}


{!!
<FS>TIEVisionCvtColorCode

<FM>Declaration<FC>
}
  TIEVisionCvtColorCode = (
    ievBGR2BGRA    =  0,
    ievRGB2RGBA    =  0,
    ievBGRA2BGR    =  1,
    ievRGBA2RGB    =  1,
    ievBGR2RGBA    =  2,
    ievRGB2BGRA    =  2,
    ievRGBA2BGR    =  3,
    ievBGRA2RGB    =  3,
    ievBGR2RGB     =  4,
    ievRGB2BGR     =  4,
    ievBGRA2RGBA   =  5,
    ievRGBA2BGRA   =  5,
    ievBGR2GRAY    =  6,
    ievRGB2GRAY    =  7,
    ievGRAY2BGR    =  8,
    ievGRAY2RGB    =  8,
    ievGRAY2BGRA   =  9,
    ievGRAY2RGBA   =  9,
    ievBGRA2GRAY   = 10,
    ievRGBA2GRAY   = 11,
    ievBGR2BGR565  = 12,
    ievRGB2BGR565  = 13,
    ievBGR5652BGR  = 14,
    ievBGR5652RGB  = 15,
    ievBGRA2BGR565 = 16,
    ievRGBA2BGR565 = 17,
    ievBGR5652BGRA = 18,
    ievBGR5652RGBA = 19,
    ievGRAY2BGR565 = 20,
    ievBGR5652GRAY = 21,
    ievBGR2BGR555  = 22,
    ievRGB2BGR555  = 23,
    ievBGR5552BGR  = 24,
    ievBGR5552RGB  = 25,
    ievBGRA2BGR555 = 26,
    ievRGBA2BGR555 = 27,
    ievBGR5552BGRA = 28,
    ievBGR5552RGBA = 29,
    ievGRAY2BGR555 = 30,
    ievBGR5552GRAY = 31,
    ievBGR2XYZ     = 32,
    ievRGB2XYZ     = 33,
    ievXYZ2BGR     = 34,
    ievXYZ2RGB     = 35,
    ievBGR2YCrCb   = 36,
    ievRGB2YCrCb   = 37,
    ievYCrCb2BGR   = 38,
    ievYCrCb2RGB   = 39,
    ievBGR2HSV     = 40,
    ievRGB2HSV     = 41,
    ievRGB2Lab     = 45,
    ievBGR2Lab     = 44,
    ievBayerBG2BGR = 46,
    ievBayerGB2BGR = 47,
    ievBayerRG2BGR = 48,
    ievBayerGR2BGR = 49,
    ievBayerBG2RGB = 48,
    ievBayerGB2RGB = 49,
    ievBayerRG2RGB = 46,
    ievBayerGR2RGB = 47,
    ievBGR2Luv     = 50,
    ievRGB2Luv     = 51,
    ievBGR2HLS     = 52,
    ievRGB2HLS     = 53,
    ievHSV2BGR     = 54,
    ievHSV2RGB     = 55,
    ievLab2BGR     = 56,
    ievLab2RGB     = 57,
    ievLuv2BGR     = 58,
    ievLuv2RGB     = 59,
    ievHLS2BGR     = 60,
    ievHLS2RGB     = 61
  );
{!!}


{!!
<FS>TIEVisionInterpolation

<FM>Declaration<FC>
}
  TIEVisionInterpolation = (
    ievNEAREST  = 0,
    ievLINEAR   = 1,
    ievCUBIC    = 2,
    ievAREA     = 3,
    ievLANCZOS4 = 4
  );
{!!}


{!!
<FS>TIEVisionFlipMode

<FM>Declaration<FC>
}
  TIEVisionFlipMode = (
    ievX_AXIS    =  0,  // flip X axis
    ievY_AXIS    =  1,  // flip Y axis
    ievXY_AXISES = -1   // flip X and Y axis
  );
{!!}


{!!
<FS>TIEVisionFileStreamMode

<FM>Declaration<FC>
}
  TIEVisionFileStreamMode = (
    ievREAD      = 0,   // read only
    ievCREATE    = 1,   // create file
    ievREADWRITE = 2    // read and write
  );
{!!}


{!!
<FS>TIEVisionSeekOffset

<FM>Declaration<FC>
}
  TIEVisionSeekOffset = (
    ievSET = 0,   // set absolute position
    ievCUR = 1,   // set from current position
    ievEND = 2    // set from the end
  );
{!!}


{!!
<FS>TIEVisionFileFormat

<FM>Declaration<FC>
}
  TIEVisionFileFormat = (
    ievTIFF = 1,    // TIFF
    ievJPEG = 3,    // JPEG
    ievBMP  = 5,    // BMP
    ievPNG  = 8,    // PNG
    ievPXM  = 12,   // PXM
    ievJ2K  = 14,   // JPEG2000
    ievRAS  = 30,   // Sun Rasters
    ievHEX  = 100   // ASCII HEX digits, separating planes
  );
{!!}


{!!
<FS>TIEVisionHaarInvert

<FM>Declaration<FC>
}
  TIEVisionHaarInvert = (
    ievNOINVERT      = 0,           // do not invert samples
    ievINVERT        = 1,           // always invert samples
    ievRANDOM_INVERT = $7FFFFFFF    // random invert samples
  );
{!!}


{!!
<FS>TIEVisionHaarMode

<FM>Declaration<FC>
}
  TIEVisionHaarMode = (
    ievBASIC = 0,
    ievCORE  = 1,
    ievALL   = 2
  );
{!!}


{!!
<FS>TIEVisionHaarBoostType

<FM>Declaration<FC>
}
  TIEVisionHaarBoostType = (
    ievDISCRETE_ADABOOST = 0,
    ievREAL_ADABOOST     = 1,
    ievLOGITBOOST        = 2,
    ievGENTLE_ADABOOST   = 3
  );
{!!}


{!!
<FS>TIEVisionHaarStumpError

<FM>Declaration<FC>
}
  TIEVisionHaarStumpError = (
    ievMISCLASSIFICATION_ERROR = 0,
    ievGINI_ERROR              = 1,
    ievENTROPY_ERROR           = 2
  );
{!!}


{!!
<FS>TIEVisionPropertyTreeFormat

<FM>Declaration<FC>
}
  TIEVisionPropertyTreeFormat = (
    ievXML  = 0,
    ievINFO = 1,
    ievINI  = 2,
    ievJSON = 3
  );
{!!}


{!!
<FS>TIEVisionBorderType

<FM>Declaration<FC>
}
  TIEVisionBorderType = (
    ievBORDER_CONSTANT   = 0,
    ievBORDER_REPLICATE  = 1,
    ievBORDER_REFLECT    = 2,
    ievBORDER_WRAP       = 3
  );
{!!}


{!!
<FS>TIEVisionSmoothType

<FM>Declaration<FC>
}
  TIEVisionSmoothType = (
    ievBLUR_NO_SCALE = 0,
    ievBLUR          = 1,
    ievGAUSSIAN      = 2,
    ievMEDIAN        = 3,
    ievBILATERAL     = 4
  );

{!!}



{!!
<FS>TIEVisionHistogramType

<FM>Declaration<FC>
}
  TIEVisionHistogramType = (
    ievARRAY   = 0,
    ievSPARSE  = 1
  );
{!!}


{!!
<FS>TIEVisionSortOrder

<FM>Declaration<FC>
}
  TIEVisionSortOrder = (
    ievASCENDING  = 0,
    ievDESCENDING = 1
  );
{!!}


{!!
<FS>TIEVisionCovarFlags

<FM>Declaration<FC>
}
  TIEVisionCovarFlags = (
    ievSCRAMBLED = 0,
    ievNORMAL    = 1,
    ievUSE_AVG   = 2,
    ievSCALE     = 4,
    ievROWS      = 8,
    ievCOLS      = 16
  );
{!!}


{!!
<FS>TIEVisionCmpOp

<FM>Declaration<FC>
}
  TIEVisionCmpOp = (
    ievEQUAL         = 0,
    ievGREATER       = 1,
    ievGREATER_EQUAL = 2,
    ievLESS          = 3,
    ievLESS_EQUAL    = 4,
    ievNOT_EQUAL     = 5
  );
{!!}


{!!
<FS>TIEVisionDCTFlags

<FM>Declaration<FC>
}
  TIEVisionDCTFlags = (
    ievDCT_FORWARD = 0,
    ievDCT_INVERSE = 1,
    ievDCT_ROWS    = 4
  );
{!!}


{!!
<FS>TIEVisionDFTFlags

<FM>Declaration<FC>
}
  TIEVisionDFTFlags = (
    ievDFT_FORWARD       = 0,
    ievDFT_INVERSE       = 1,
    ievDFT_SCALE         = 2,
    ievDFT_INVERSE_SCALE = 3,
    ievDFT_ROWS          = 4
  );
{!!}


{!!
<FS>TIEVisionGEMMFlags

<FM>Declaration<FC>
}
  TIEVisionGEMMFlags = (
    ievGEMM_NONE = 0,
    ievGEMM_A_T  = 1,
    ievGEMM_B_T  = 2,
    ievGEMM_C_T  = 4
  );
{!!}


{!!
<FS>TIEVisionInvertMethod

<FM>Declaration<FC>
}
  TIEVisionInvertMethod = (
    ievLU      = 0,
    ievSVD     = 1,
    ievSVD_SYM = 2
  );
{!!}


{!!
<FS>TIEVisionReduceOp

<FM>Declaration<FC>
}
  TIEVisionReduceOp = (
    ievREDUCE_SUM = 0,
    ievREDUCE_AVG = 1,
    ievREDUCE_MAX = 2,
    ievREDUCE_MIN = 3
  );
{!!}


{!!
<FS>TIEVisionSolveMethod

<FM>Declaration<FC>
}
  TIEVisionSolveMethod = (
    ievSOLVE_LU      = 0,
    ievSOLVE_SVD     = 1,
    ievSOLVE_SVD_SYM = 2
  );
{!!}


{!!
<FS>TIEVisionSVBFlags

<FM>Declaration<FC>
}
  TIEVisionSVBFlags = (
    ievSVB_NONE     = 0,
    ievSVB_MODIFY_A = 1,
    ievSVB_U_T      = 2,
    ievSVB_V_T      = 4
  );
{!!}


{!!
<FS>TIEVisionSVDFlags

<FM>Declaration<FC>
}
  TIEVisionSVDFlags = (
    ievSVD_NONE     = 0,
    ievSVD_MODIFY_A = 1,
    ievSVD_U_T      = 2,
    ievSVD_V_T      = 4
  );
{!!}


{!!
<FS>TIEVisionContourRetrMode

<FM>Declaration<FC>
}
  TIEVisionContourRetrMode = (
    ievEXTERNAL = 0,
    ievLIST     = 1,
    ievCCOMP    = 2,
    ievTREE     = 3
  );
{!!}


{!!
<FS>TIEVisionContourApproxMethod

<FM>Declaration<FC>
}
  TIEVisionContourApproxMethod = (
    ievCHAIN_CODE             = 0,
    ievCHAIN_APPROX_NONE      = 1,
    ievCHAIN_APPROX_SIMPLE    = 2,
    ievCHAIN_APPROX_TC89_L1   = 3,
    ievCHAIN_APPROX_TC89_KCOS = 4,
    ievLINK_RUNS              = 5
  );
{!!}


{!!
<FS>TIEVisionInpaintMethod

<FM>Declaration<FC>
}
  TIEVisionInpaintMethod = (
    ievINPAINT_NS    = 0,
    ievINPAINT_TELEA = 1
  );
{!!}


{!!
<FS>TIEVisionOCRPageSegmentationMode

<FM>Declaration<FC>
}
  TIEVisionOCRPageSegmentationMode = (
  ievOCROSD_ONLY               = 0,  // Orientation and script detection only.
  ievOCRAUTO_OSD               = 1,  // Automatic page segmentation with orientation and script detection. (OSD)
  ievOCRAUTO_ONLY              = 2,  // Automatic page segmentation, but no OSD, or OCR.
  ievOCRAUTO                   = 3,  // Fully automatic page segmentation, but no OSD.
  ievOCRSINGLE_COLUMN          = 4,  // Assume a single column of text of variable sizes.
  ievOCRSINGLE_BLOCK_VERT_TEXT = 5,  // Assume a single uniform block of vertically aligned text.
  ievOCRSINGLE_BLOCK           = 6,  // Assume a single uniform block of text. (Default.)
  ievOCRSINGLE_LINE            = 7,  // Treat the image as a single text line.
  ievOCRSINGLE_WORD            = 8,  // Treat the image as a single word.
  ievOCRCIRCLE_WORD            = 9,  // Treat the image as a single word in a circle.
  ievOCRSINGLE_CHAR            = 10  // Treat the image as a single character.
  );
{!!}


{!!
<FS>TIEVisionOCRScriptDirection

<FM>Declaration<FC>
}
TIEVisionOCRScriptDirection = (
  ievOCRDIR_NEUTRAL       = 0,  // Text contains only neutral characters.
  ievOCRDIR_LEFT_TO_RIGHT = 1,  // Text contains no Right-to-Left characters.
  ievOCRDIR_RIGHT_TO_LEFT = 2,  // Text contains no Left-to-Right characters.
  ievOCRDIR_MIX           = 3   // Text contains a mixture of left-to-right
  );
{!!}


{!!
<FS>TIEVisionOCREngine

<FM>Declaration<FC>
TIEVisionOCREngine = (
  ievOCRFAST     = 0,
  ievOCRCUBE     = 1,
  ievOCRACCURATE = 2
  );

<FM>Description<FN>
The engine(s) used for OCR recognition:
<TABLE>
<R> <H>Type</H> <H>Description</H> </R>
<R> <C><FC>ievOCRFAST<FN></C> <C>Use only the base OCR engine for faster processing</C> </R>
<R> <C><FC>ievOCRCUBE<FN></C> <C>Use the Cube OCR engine for better accuracy (slower)</C> </R>
<R> <C><FC>ievOCRACCURATE<FN></C> <C>Use both the base and Cube OCR engines for best accuracy</C> </R>
</TABLE>

Note: If you use the base OCR engine (<FC>ievOCRFAST<FN>) you will only require the *.trained data support file, e.g. eng.traineddata for processing of an English language document.
Use of <FC>ievOCRCUBE<FN> and <FC>ievOCRACCURATE<FN> require you to also enclude the *.cube.* files. e.g. eng.cube.word-freq, eng.cube.size, etc.

!!}
  TIEVisionOCREngine = (
    ievOCRFAST     = 0,   // don't use cube, fastest engine
    ievOCRCUBE     = 1,   // use cube only, better accuracy, but slower
    ievOCRACCURATE = 2    // use base and cube engine, best accuracy
    );


{!!
<FS>TIEVisionTermCriteriaType

<FM>Declaration<FC>
}
  type TIEVisionTermCriteriaType = int32_t;
  const ievITER   = 1;
  const ievNUMBER = 1;
  const ievEPS    = 2;
{!!}


{!!
<FS>TIEVisionMulSpectrumsFlags

<FM>Declaration<FC>
}
  type TIEVisionMulSpectrumsFlags = int32_t;
  const ievMUL_ROWS = 4;
  const ievMUL_CONJ = 8;
{!!}


{!!
<FS>TIEVisionHaarDetectObjectsFlags

<FM>Declaration<FC>
}
  type TIEVisionHaarDetectObjectsFlags = int32_t;
  const ievNONE                = 0;
  const ievDO_CANNY_PRUNING    = 1;
  const ievSCALE_IMAGE         = 2;
  const ievFIND_BIGGEST_OBJECT = 4;
  const ievDO_ROUGH_SEARCH     = 8;
{!!}

  {$WARNINGS ON}


type

  ///////////////////////////////////////////////////
  // structures

{!!
<FS>TIEVisionRect

<FM>Declaration<FC>
}
  TIEVisionRect = packed record
    x: int32_t;
    y: int32_t;
    width: int32_t;
    height: int32_t;
  end;
{!!}


{!!
<FS>PIEVisionRect

<FM>Declaration<FC>
PIEVisionRect = ^<A TIEVisionRect>;
!!}
  PIEVisionRect = ^TIEVisionRect;


{!!
<FS>TIEVisionSize

<FM>Declaration<FC>
}
  TIEVisionSize = packed record
    width:  int32_t;
    height: int32_t;
  end;
{!!}


{!!
<FS>TIEVisionSize2f

<FM>Declaration<FC>
}
  TIEVisionSize2f = packed record
    width:  single;
    height: single;
  end;
{!!}


{!!
<FS>TIEVisionPoint

<FM>Declaration<FC>
}
  TIEVisionPoint = packed record
    x: int32_t;
    y: int32_t;
  end;
{!!}


{!!
<FS>TIEVisionPoint2f

<FM>Declaration<FC>
}
  TIEVisionPoint2f = packed record
    x: single;
    y: single;
  end;
{!!}


{!!
<FS>TIEVisionScalar

<FM>Declaration<FC>
}
  TIEVisionScalar = packed record
    val: array [0..3] of double;
  end;
{!!}


{!!
<FS>TIEVisionFloatPair

<FM>Declaration<FC>
}
  TIEVisionFloatPair = packed record
    first:  single;
    second: single;
  end;
{!!}


{!!
<FS>TIEVisionTermCriteria

<FM>Declaration<FC>
TIEVisionTermCriteria = packed record
  ctype:   <A TIEVisionTermCriteriaType>;
  maxIter: int32_t;
  epsilon: double;
end;
!!}
  TIEVisionTermCriteria = packed record
    ctype:   TIEVisionTermCriteriaType;
    maxIter: int32_t;
    epsilon: double;
  end;


{!!
<FS>TIEVisionBox2D

<FM>Declaration<FC>
}
  TIEVisionBox2D = packed record
    centerX, centerY: single;
    width, height:   single;
    angle: single;
  end;
{!!}



{!!
<FS>TIEVisionRotatedRect

<FM>Declaration<FC>
TIEVisionRotatedRect = packed record
  center: <A TIEVisionPoint2f>;  // mass center of the rectangle
  size:   <A TIEVisionSize2f>;   // size
  angle:  single;                // rotation angle in degrees
end;
!!}
  TIEVisionRotatedRect = packed record
    center: TIEVisionPoint2f;  // mass center of the rectangle
    size:   TIEVisionSize2f;   // size
    angle:  single;            // rotation angle in degrees
  end;



{!!
<FS>TIEVisionRGB8

<FM>Declaration<FC>
}
  TIEVisionRGB8 = packed record
    r: uint8_t;
    g: uint8_t;
    b: uint8_t;
  end;
{!!}



{!!
<FS>PIEVisionRGB8

<FM>Declaration<FC>
PIEVisionRGB8 = ^<A TIEVisionRGB8>;
!!}
  PIEVisionRGB8 = ^TIEVisionRGB8;


{!!
<FS>TIEVisionRGB16

<FM>Declaration<FC>
}
  TIEVisionRGB16 = packed record
    r: uint16_t;
    g: uint16_t;
    b: uint16_t;
  end;
{!!}


{!!
<FS>PIEVisionRGB16

<FM>Declaration<FC>
PIEVisionRGB16 = ^<A TIEVisionRGB16>;
!!}
  PIEVisionRGB16 = ^TIEVisionRGB16;


{!!
<FS>TIEVisionBGR8

<FM>Declaration<FC>
}
  TIEVisionBGR8 = packed record
    b: uint8_t;
    g: uint8_t;
    r: uint8_t;
  end;
{!!}


{!!
<FS>PIEVisionBGR8

<FM>Declaration<FC>
PIEVisionBGR8 = ^<A TIEVisionBGR8>;
!!}
  PIEVisionBGR8 = ^TIEVisionBGR8;


{!!
<FS>TIEVisionBGR16

<FM>Declaration<FC>
}
  TIEVisionBGR16 = packed record
    b: uint16_t;
    g: uint16_t;
    r: uint16_t;
  end;
{!!}


{!!
<FS>PIEVisionBGR16

<FM>Declaration<FC>
PIEVisionBGR16 = ^<A TIEVisionBGR16>;
!!}
  PIEVisionBGR16 = ^TIEVisionBGR16;


{!!
<FS>TIEVisionOCRBox

<FM>Declaration<FC>
TIEVisionOCRBox = packed record
  text: uint32_t;
  cost: single;
  rect: <A TIEVisionRect>;
end;
!!}
  TIEVisionOCRBox = packed record
    text: uint32_t;
    cost: single;
    rect: TIEVisionRect;
  end;


{!!
<FS>PIEVisionOCRBox

<FM>Declaration<FC>
PIEVisionOCRBox = ^<A TIEVisionOCRBox>;
!!}
  PIEVisionOCRBox = ^TIEVisionOCRBox;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionBase

<FM>Declaration<FC>
TIEVisionBase = interface(IInterface)

<FM>Description<FN>
This is the base interface for each IEVision object.
It handles reference counting and automatic disposing.
!!}
  // must be stdcall
  TIEVisionBase = interface(IInterface)
    procedure dispose(); stdcall;
    function getRefCount(): int32_t; stdcall;
    procedure internal_destructor; cdecl; // don't use!
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionString

<FM>Declaration<FC>
TIEVisionString = interface(<A TIEVisionBase>)

<FM>Description<FN>
An object that exposes this interface can store an ANSI string.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionString.append></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionString.at></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionString.c_str></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionString.length></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionString.operatorASSIGN></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionString.operatorEQUAL></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionString.saveToFile></C> </R>
</TABLE>

!!}
  TIEVisionString = interface(TIEVisionBase)

{!!
<FS>TIEVisionString.length

<FM>Declaration<FC>
function length(): int32_t; safecall;

<FM>Description<FN>
Returns the string length.
!!}
    function length(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionString.operatorASSIGN

<FM>Declaration<FC>
procedure operatorASSIGN(src: <A TIEVisionString>); safecall;

<FM>Description<FN>
Replaces current string with the content of source string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>The source string.</C> </R>
</TABLE>


<FM>Example<FC>
string1.operatorASSIGN(string2);  // equiv. to "string1 := string2"
!!}
    procedure operatorASSIGN(src: TIEVisionString; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionString.operatorEQUAL

<FM>Declaration<FC>
function operatorEQUAL(src: <A TIEVisionString>): bool32; safecall;

<FM>Description<FN>
Compares current string with the content of source string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>The source string.</C> </R>
</TABLE>


<FM>Example<FC>
string1.operatorEQUAL(string2);  // equiv. to "string1 = string2"
!!}
    function operatorEQUAL(src: TIEVisionString; wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionString.at

<FM>Declaration<FC>
function at(pos: int32_t): AnsiChar; safecall;

<FM>Description<FN>
Returns character at specified position.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Character index (0=first character).</C> </R>
</TABLE>


<FM>Example<FC>
c = string1.at(0);  // equiv. to "c := string1[0]"
!!}
    function at(pos: int32_t; wantExceptions: bool32 = false): AnsiChar; safecall;

{!!
<FS>TIEVisionString.append

<FM>Declaration<FC>
procedure append(src: <A TIEVisionString>); overload; safecall;
procedure append(src: PAnsiChar); overload; safecall;

<FM>Description<FN>
Appends a string or ANSI string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>String to append.</C> </R>
</TABLE>


<FM>Example<FC>
string1.append(string2);  // equiv. to string1 := string1 + string2;
string1.append('hello');  // equiv. to string1 := string1 + 'hello';
!!}
    procedure append(src: TIEVisionString; wantExceptions: bool32 = false); overload; safecall;
    procedure append(src: PAnsiChar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionString.c_str

<FM>Declaration<FC>
function c_str(): PAnsiChar; safecall;

<FM>Description<FN>
Returns a zero terminated string.

<FM>Example<FC>
ShowMessage( AnsiString(string1.c_str()) );
!!}
    function c_str(wantExceptions: bool32 = false): PAnsiChar; safecall;

{!!
<FS>TIEVisionString.saveToFile

<FM>Declaration<FC>
procedure saveToFile(filename: PAnsiChar); safecall;

<FM>Description<FN>
Saves string content to a file.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>filename<FN></C> <C>The output filename with path.</C> </R>
</TABLE>


<FM>Example<FC>
string1.saveToFile('output.txt');
!!}
    procedure saveToFile(filename: PAnsiChar; wantExceptions: bool32 = false); safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionWString

<FM>Declaration<FC>
TIEVisionWString = interface(<A TIEVisionBase>)

<FM>Description<FN>
An object that exposes this interface can store a wide string.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionWString.append></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionWString.at></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionWString.c_str></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionWString.convertFromUTF8></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionWString.length></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionWString.operatorASSIGN></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionWString.operatorEQUAL></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionWString.saveToFile></C> </R>
</TABLE>
!!}
  TIEVisionWString = interface(TIEVisionBase)

{!!
<FS>TIEVisionWString.length

<FM>Declaration<FC>
function length(): int32_t; safecall;

<FM>Description<FN>
Returns the string length.
!!}
    function length(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionWString.operatorASSIGN

<FM>Declaration<FC>
procedure operatorASSIGN(src: <A TIEVisionWString>); safecall;

<FM>Description<FN>
Replaces current string with the content of source string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>The source string.</C> </R>
</TABLE>


<FM>Example<FC>
string1.operatorASSIGN(string2);  // equiv. to "string1 := string2"
!!}
    procedure operatorASSIGN(src: TIEVisionWString; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionWString.operatorEQUAL

<FM>Declaration<FC>
function operatorEQUAL(src: <A TIEVisionWString>): bool32; safecall;

<FM>Description<FN>
Compares current string with the content of source string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>The source string.</C> </R>
</TABLE>


<FM>Example<FC>
string1.operatorEQUAL(string2);  // equiv. to "string1 = string2"
!!}
    function operatorEQUAL(src: TIEVisionWString; wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionWString.at

<FM>Declaration<FC>
function at(pos: int32_t): WideChar; safecall;

<FM>Description<FN>
Returns character at specified position.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Character index (0=first character).</C> </R>
</TABLE>


<FM>Example<FC>
c = string1.at(0);  // equiv. to "c := string1[0]"
!!}
    function at(pos: int32_t; wantExceptions: bool32 = false): WideChar; safecall;

{!!
<FS>TIEVisionWString.append

<FM>Declaration<FC>
procedure append(src: <A TIEVisionWString>); overload; safecall;
procedure append(src: PWideChar); overload; safecall;

<FM>Description<FN>
Appends a string or wide string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>String to append.</C> </R>
</TABLE>


<FM>Example<FC>
string1.append(string2);  // equiv. to string1 := string1 + string2;
string1.append('hello');  // equiv. to string1 := string1 + 'hello';
!!}
    procedure append(src: TIEVisionWString; wantExceptions: bool32 = false); overload; safecall;
    procedure append(src: PWideChar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionWString.c_str

<FM>Declaration<FC>
function c_str(): PWideChar; safecall;

<FM>Description<FN>
Returns a zero terminated string.

<FM>Example<FC>
ShowMessage( WideString(string1.c_str()) );
!!}
    function c_str(wantExceptions: bool32 = false): PWideChar; safecall;

{!!
<FS>TIEVisionWString.saveToFile

<FM>Declaration<FC>
procedure saveToFile(filename: PAnsiChar); safecall;

<FM>Description<FN>
Saves string content to a file as UTF-16 (little endian).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>filename<FN></C> <C>The output filename with path.</C> </R>
</TABLE>


<FM>Example<FC>
string1.saveToFile('output.txt');
!!}
    procedure saveToFile(filename: PAnsiChar; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionWString.convertFromUTF8

<FM>Declaration<FC>
procedure convertFromUTF8(src: PAnsiChar); safecall;

<FM>Description<FN>
Converts UTF8 zero terminated string to UTF16.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Source UTF8 string.</C> </R>
</TABLE>
!!}
    procedure convertFromUTF8(src: PAnsiChar; wantExceptions: bool32 = false); safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionBaseVector

<FM>Declaration<FC>
TIEVisionBaseVector = interface(<A TIEVisionBase>)

<FM>Description<FN>
An object that exposes this interface can store a generic vector. This is the base class for <A TIEVisionVectorString>, <A TIEVisionVectorInt32>, etc...

<FM>See Also<FN>
- <A TIEVisionVectorString>
- <A TIEVisionVectorInt32>
- <A TIEVisionVectorByte>
- <A TIEVisionVectorFloatPair>
- <A TIEVisionVectorPoint>
- <A TIEVisionVectorRect>
- <A TIEVisionVectorDouble>
- <A TIEVisionVectorOCRBox>
- <A TIEVisionVectorImageRef>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.size></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.operatorASSIGN></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.max_size></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.resize></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.capacity></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.empty></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.reserve></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.pop_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.erase></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.swap></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.clear></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.sort></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBaseVector.data></C> </R>
</TABLE>
!!}
  TIEVisionBaseVector = interface(TIEVisionBase)

{!!
<FS>TIEVisionBaseVector.size

<FM>Declaration<FC>
function size(): int32_t; safecall;

<FM>Description<FN>
Represents the number of items in the vector.
!!}
    function size(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionBaseVector.operatorASSIGN

<FM>Declaration<FC>
procedure operatorASSIGN(src: <A TIEVisionBaseVector>); safecall;

<FM>Description<FN>
Replaces current vector with the source vector.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>The source vector.</C> </R>
</TABLE>
!!}
    procedure operatorASSIGN(src: TIEVisionBaseVector; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionBaseVector.max_size

<FM>Declaration<FC>
function max_size(): uint32_t; safecall;

<FM>Description<FN>
Returns the maximum number of items that this vector can contain (checks free memory).
!!}
    function max_size(wantExceptions: bool32 = false): uint32_t; safecall;

{!!
<FS>TIEVisionBaseVector.resize

<FM>Declaration<FC>
procedure resize(sz: uint32_t); safecall;

<FM>Description<FN>
Resizes the vector to the specified size.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>sz<FN></C> <C>The new vector size.</C> </R>
</TABLE>
!!}
    procedure resize(sz: uint32_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionBaseVector.capacity

<FM>Declaration<FC>
function capacity(): uint32_t; safecall;

<FM>Description<FN>
Returns the size of allocated storage for the container.
!!}
    function capacity(wantExceptions: bool32 = false): uint32_t; safecall;

{!!
<FS>TIEVisionBaseVector.empty

<FM>Declaration<FC>
function empty(): bool32; safecall;

<FM>Description<FN>
Returns true if the vector is empty.
!!}
    function empty(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionBaseVector.reserve

<FM>Declaration<FC>
procedure reserve(n: uint32_t); safecall;

<FM>Description<FN>
Reserves a minimum length of storage for this vector, allocating space if necessary.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>n<FN></C> <C>The minimum length of storage to be allocated for the vector.</C> </R>
</TABLE>
!!}
    procedure reserve(n: uint32_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionBaseVector.pop_back

<FM>Declaration<FC>
procedure pop_back(); safecall;

<FM>Description<FN>
Removes the last element.
!!}
    procedure pop_back(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionBaseVector.erase

<FM>Declaration<FC>
function erase(position: uint32_t): uint32_t; overload; safecall;
function erase(first: uint32_t; last: uint32_t): uint32_t; overload; safecall;

<FM>Description<FN>
Removes specified item or a range of items.
Returns the first element remaining beyond any elements removed.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>position<FN></C> <C>The item index to remove.</C> </R>
<R> <C><FC>first<FN></C> <C>Begin of range to erase.</C> </R>
<R> <C><FC>last<FN></C> <C>End of range (not included) to erase.</C> </R>
</TABLE>


<FM>Example<FC>
// removes item 0 (first item)
vector1.erase(0);

// removes item 0 up to 3 (included)
vector1.erase(0, 4);
!!}
    function erase(position: uint32_t; wantExceptions: bool32 = false): uint32_t; overload; safecall;
    function erase(first: uint32_t; last: uint32_t; wantExceptions: bool32 = false): uint32_t; overload; safecall;

{!!
<FS>TIEVisionBaseVector.swap

<FM>Declaration<FC>
procedure swap(vec: <A TIEVisionBaseVector>); safecall;

<FM>Description<FN>
Swaps the content of this vector with the specified one.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>vec<FN></C> <C>Other vector to swap.</C> </R>
</TABLE>
!!}
    procedure swap(vec: TIEVisionBaseVector; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionBaseVector.clear

<FM>Declaration<FC>
procedure clear(); safecall;

<FM>Description<FN>
Removes all elements.
!!}
    procedure clear(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionBaseVector.sort

<FM>Declaration<FC>
procedure sort(order: <A TIEVisionSortOrder> = ievASCENDING); safecall;

<FM>Description<FN>
Sorts elements of the vector.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>order<FN></C> <C>Sorting order.</C> </R>
</TABLE>


<FM>Example<FC>
vector1.sort(ievASCENDING);
!!}
    procedure sort(order: TIEVisionSortOrder = ievASCENDING; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionBaseVector.data

<FM>Declaration<FC>
function data(): pointer; safecall;

<FM>Description<FN>
Returns the raw pointer to the vector.
!!}
    function data(wantExceptions: bool32 = false): pointer; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionVectorString

<FM>Declaration<FC>
TIEVisionVectorString = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of strings (<A TIEVisionString>).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorString.getString></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorString.getCString></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorString.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorString.setString></C> </R>
</TABLE>
!!}
  TIEVisionVectorString = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorString.getString

<FM>Declaration<FC>
function getString(pos: int32_t): <A TIEVisionString>; safecall;

<FM>Description<FN>
Returns the string at specified index.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Index of string to return (0=first string).</C> </R>
</TABLE>
!!}
    function getString(pos: int32_t; wantExceptions: bool32 = false): TIEVisionString; safecall;

{!!
<FS>TIEVisionVectorString.getCString

<FM>Declaration<FC>
function getCString(pos: int32_t): PAnsiChar; safecall;

<FM>Description<FN>
Returns an ANSI zero terminated string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Index of string to return (0=first string).</C> </R>
</TABLE>


<FM>Example<FC>
ShowMessage(AnsiString(vector1.getCString(0)));
!!}
    function getCString(pos: int32_t; wantExceptions: bool32 = false): PAnsiChar; safecall;

{!!
<FS>TIEVisionVectorString.push_back

<FM>Declaration<FC>
procedure push_back(str: <A TIEVisionString>); overload; safecall;
procedure push_back(str: PAnsiChar); overload; safecall;

<FM>Description<FN>
Appends a string object or an ANSI string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>str<FN></C> <C>String to append.</C> </R>
</TABLE>


<FM>Example<FC>
vector1.push_back("hello");
!!}
    procedure push_back(str: TIEVisionString; wantExceptions: bool32 = false); overload; safecall;
    procedure push_back(str: PAnsiChar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionVectorString.setString

<FM>Declaration<FC>
procedure setString(pos: int32_t; value: <A TIEVisionString>); overload; safecall;
procedure setString(pos: int32_t; value: PAnsiChar); overload; safecall;

<FM>Description<FN>
Replaces/Sets an item with the specified string object or ANSI stirng.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index to set.</C> </R>
<R> <C><FC>value<FN></C> <C>String object or ANSI string.</C> </R>
</TABLE>


<FM>Example<FC>
i := vector1.push_back('before');
vector1.setString(i, 'replaced');
!!}
    procedure setString(pos: int32_t; value: TIEVisionString; wantExceptions: bool32 = false); overload; safecall;
    procedure setString(pos: int32_t; value: PAnsiChar; wantExceptions: bool32 = false); overload; safecall;

  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionVectorInt32

<FM>Declaration<FC>
TIEVisionVectorInt32 = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of integers (32 bit).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorInt32.getInt32></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorInt32.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorInt32.setInt32></C> </R>
</TABLE>
!!}
  TIEVisionVectorInt32 = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorInt32.getInt32

<FM>Declaration<FC>
function getInt32(pos: int32_t): int32_t; safecall;

<FM>Description<FN>
Returns the specified item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index (0 = first item).</C> </R>
</TABLE>
!!}
    function getInt32(pos: int32_t; wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionVectorInt32.push_back

<FM>Declaration<FC>
procedure push_back(value: int32_t); safecall;

<FM>Description<FN>
Appends a new integer.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Integer to add.</C> </R>
</TABLE>
!!}
    procedure push_back(value: int32_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorInt32.setInt32

<FM>Declaration<FC>
procedure setInt32(pos: int32_t; value: int32_t); safecall;

<FM>Description<FN>
Sets the item value.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index to set.</C> </R>
<R> <C><FC>value<FN></C> <C>Integer value.</C> </R>
</TABLE>


<FM>Example<FC>
intvector.setInt32(0, 123); // like: intvector[0] :=  123
!!}
    procedure setInt32(pos: int32_t; value: int32_t; wantExceptions: bool32 = false); safecall;

  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionVectorByte

<FM>Declaration<FC>
TIEVisionVectorByte = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of bytes (unsigned 8 bit).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorByte.getUInt8></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorByte.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorByte.setUInt8></C> </R>
</TABLE>
!!}
  TIEVisionVectorByte = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorByte.getUInt8

<FM>Declaration<FC>
function getUInt8(pos: int32_t): uint8_t; safecall;

<FM>Description<FN>
Returns value of the specified item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index (0 = first item).</C> </R>
</TABLE>
!!}
    function getUInt8(pos: int32_t; wantExceptions: bool32 = false): uint8_t; safecall;

{!!
<FS>TIEVisionVectorByte.push_back

<FM>Declaration<FC>
procedure push_back(value: uint8_t); safecall;

<FM>Description<FN>
Appends a new byte.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Byte value to add.</C> </R>
</TABLE>
!!}
    procedure push_back(value: uint8_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorByte.setUInt8

<FM>Declaration<FC>
procedure setUInt8(pos: int32_t; value: uint8_t); safecall;

<FM>Description<FN>

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index to set.</C> </R>
<R> <C><FC>value<FN></C> <C>Value to set.</C> </R>
</TABLE>
!!}
    procedure setUInt8(pos: int32_t; value: uint8_t; wantExceptions: bool32 = false); safecall;

  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionVectorFloatPair

<FM>Declaration<FC>
TIEVisionVectorFloatPair = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of float pairs (two 32 bit floats, <A TIEVisionFloatPair>).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorFloatPair.getFloatPair></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorFloatPair.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorFloatPair.setFloatPair></C> </R>
</TABLE>
!!}
  TIEVisionVectorFloatPair = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorFloatPair.getFloatPair

<FM>Declaration<FC>
function getFloatPair(pos: int32_t): <A TIEVisionFloatPair>; safecall;

<FM>Description<FN>
Returns the float pair at the specified index.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index to retrieve.</C> </R>
</TABLE>
!!}
    function getFloatPair(pos: int32_t; wantExceptions: bool32 = false): TIEVisionFloatPair; safecall;

{!!
<FS>TIEVisionVectorFloatPair.push_back

<FM>Declaration<FC>
procedure push_back(value: <A TIEVisionFloatPair>); safecall;

<FM>Description<FN>
Appends a new float pair value.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Float pair to add.</C> </R>
</TABLE>


<FM>Example<FC>
vector1.push_back( IEVisionFloatPair(1.2, 1.3) );
!!}
    procedure push_back(value: TIEVisionFloatPair; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorFloatPair.setFloatPair

<FM>Declaration<FC>
procedure setFloatPair(pos: int32_t; value: <A TIEVisionFloatPair>); safecall;

<FM>Description<FN>
Sets a new value for the specified item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index to set (0 = first item).</C> </R>
<R> <C><FC>value<FN></C> <C>Item value.</C> </R>
</TABLE>


<FM>Example<FC>
vector1.setFloatPair(0, IEVisionFloatPair(1.2, 1.3) );
!!}
    procedure setFloatPair(pos: int32_t; value: TIEVisionFloatPair; wantExceptions: bool32 = false); safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionVectorPoint

<FM>Declaration<FC>
TIEVisionVectorPoint = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of points (<A TIEVisionPoint>).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorPoint.getPoint></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorPoint.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorPoint.setPoint></C> </R>
</TABLE>
!!}
  TIEVisionVectorPoint = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorPoint.getPoint

<FM>Declaration<FC>
function getPoint(pos: int32_t): <A TIEVisionPoint>; safecall;

<FM>Description<FN>
Returns the point at specified position.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index.</C> </R>
</TABLE>
!!}
    function getPoint(pos: int32_t; wantExceptions: bool32 = false): TIEVisionPoint; safecall;

{!!
<FS>TIEVisionVectorPoint.push_back

<FM>Declaration<FC>
procedure push_back(value: <A TIEVisionPoint>); safecall;

<FM>Description<FN>
Appends a new point to the vector.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Point to add.</C> </R>
</TABLE>


<FM>Example<FC>
vector1.push_back( IEVisionPoint(10, 20) );
!!}
    procedure push_back(value: TIEVisionPoint; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorPoint.setPoint

<FM>Declaration<FC>
procedure setPoint(pos: int32_t; value: <A TIEVisionPoint>); safecall;

<FM>Description<FN>
Replaces/Sets an item value.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index.</C> </R>
<R> <C><FC>value<FN></C> <C>Value of the item.</C> </R>
</TABLE>


<FM>Example<FC>
vector1.setPoint(0, IEVisionPoint(10, 20));
!!}
    procedure setPoint(pos: int32_t; value: TIEVisionPoint; wantExceptions: bool32 = false); safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionVectorRect

<FM>Declaration<FC>
TIEVisionVectorRect = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of rectangles (<A TIEVisionRect>).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorRect.getRect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorRect.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorRect.setRect></C> </R>
</TABLE>
!!}
  TIEVisionVectorRect = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorRect.getRect

<FM>Declaration<FC>
function getRect(pos: int32_t): <A TIEVisionRect>; safecall;

<FM>Description<FN>
Returns the specified item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index (0 = first item).</C> </R>
</TABLE>
!!}
    function getRect(pos: int32_t; wantExceptions: bool32 = false): TIEVisionRect; safecall;

{!!
<FS>TIEVisionVectorRect.push_back

<FM>Declaration<FC>
procedure push_back(value: <A TIEVisionRect>); safecall;

<FM>Description<FN>
Appends a new item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>The rectangle to append.</C> </R>
</TABLE>


<FM>Example<FC>
vector1.push_back(IEVisionRect(0, 0, 200, 200));
!!}
    procedure push_back(value: TIEVisionRect; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorRect.setRect

<FM>Declaration<FC>
procedure setRect(pos: int32_t; value: <A TIEVisionRect>); safecall;

<FM>Description<FN>
Replaces/Sets an item value.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index (0 = first item).</C> </R>
<R> <C><FC>value<FN></C> <C>Value to set.</C> </R>
</TABLE>


<FM>Example<FC>
vector1.setRect(IEVisionRect(0, 0, 200, 200));
!!}
    procedure setRect(pos: int32_t; value: TIEVisionRect; wantExceptions: bool32 = false); safecall;

  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionVectorDouble

<FM>Declaration<FC>
TIEVisionVectorDouble = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of doubles (64 bit floating point).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorDouble.getDouble></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorDouble.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorDouble.setDouble></C> </R>
</TABLE>
!!}
  TIEVisionVectorDouble = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorDouble.getDouble

<FM>Declaration<FC>
function getDouble(pos: int32_t): double; safecall;

<FM>Description<FN>
Returns the specified item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index.</C> </R>
</TABLE>
!!}
    function getDouble(pos: int32_t; wantExceptions: bool32 = false): double; safecall;

{!!
<FS>TIEVisionVectorDouble.push_back

<FM>Declaration<FC>
procedure push_back(value: double); safecall;

<FM>Description<FN>
Appends a new item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Value to add.</C> </R>
</TABLE>
!!}
    procedure push_back(value: double; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorDouble.setDouble

<FM>Declaration<FC>
procedure setDouble(pos: int32_t; value: double); safecall;

<FM>Description<FN>
Replaces/Sets a value at specified position.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index.</C> </R>
<R> <C><FC>value<FN></C> <C>Value to set.</C> </R>
</TABLE>
!!}
    procedure setDouble(pos: int32_t; value: double; wantExceptions: bool32 = false); safecall;

  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionVectorOCRBox

<FM>Declaration<FC>
TIEVisionVectorOCRBox = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of OCR boxes (<A TIEVisionOCRBox>).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorOCRBox.getOCRBox></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorOCRBox.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorOCRBox.setOCRBox></C> </R>
</TABLE>

!!}
  TIEVisionVectorOCRBox = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorOCRBox.getOCRBox

<FM>Declaration<FC>
function getOCRBox(pos: int32_t): <A TIEVisionOCRBox>; safecall;

<FM>Description<FN>
Returns the specified item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index.</C> </R>
</TABLE>
!!}
    function getOCRBox(pos: int32_t; wantExceptions: bool32 = false): TIEVisionOCRBox; safecall;

{!!
<FS>TIEVisionVectorOCRBox.push_back

<FM>Declaration<FC>
procedure push_back(value: <A TIEVisionOCRBox>); safecall;

<FM>Description<FN>
Appends a new item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Item to append.</C> </R>
</TABLE>
!!}
    procedure push_back(value: TIEVisionOCRBox; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorOCRBox.setOCRBox

<FM>Declaration<FC>
procedure setOCRBox(pos: int32_t; value: <A TIEVisionOCRBox>); safecall;

<FM>Description<FN>
Replaces/Sets the spcecified item.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index.</C> </R>
<R> <C><FC>value<FN></C> <C>Value to set.</C> </R>
</TABLE>
!!}
    procedure setOCRBox(pos: int32_t; value: TIEVisionOCRBox; wantExceptions: bool32 = false); safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionOCRWordBox

<FM>Declaration<FC>
TIEVisionOCRWordBox = interface(<A TIEVisionBase>)

<FM>Description<FN>
An object that exposes this interface can store an ocr word box (a box that contains a word).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionOCRWordBox.getBox></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionOCRWordBox.getText></C> </R>
</TABLE>

!!}
  TIEVisionOCRWordBox = interface(TIEVisionBase)

{!!
<FS>TIEVisionOCRWordBox.getText

<FM>Declaration<FC>
function getText(): <A TIEVisionWString>; safecall;

<FM>Description<FN>
Returns the text associated to this box.
!!}
    function getText(wantExceptions: bool32 = false): TIEVisionWString; safecall;

{!!
<FS>TIEVisionOCRWordBox.getBox

<FM>Declaration<FC>
function getBox(): <A TIEVisionRect>; safecall;

<FM>Description<FN>
Returns the bounding rectangle of this box.
!!}
    function getBox(wantExceptions: bool32 = false): TIEVisionRect; safecall;

{!!
<FS>TIEVisionOCRWordBox.getConfidence

<FM>Declaration<FC>
function getConfidence(): single; safecall;

<FM>Description<FN>
Returns confidence (as percentage) of this word.
!!}
    function getConfidence(wantExceptions: bool32 = false): single; safecall;

{!!
<FS>TIEVisionOCRWordBox.isBold

<FM>Declaration<FC>
function isBold(): bool32; safecall;

<FM>Description<FN>
Returns true if current word has Bold attribute.
!!}
    function isBold(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionOCRWordBox.isItalic

<FM>Declaration<FC>
function isItalic(): bool32; safecall;

<FM>Description<FN>
Returns true if current word has Italic attribute.
!!}
    function isItalic(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionOCRWordBox.isUnderlined

<FM>Declaration<FC>
function isUnderlined(): bool32; safecall;

<FM>Description<FN>
Returns true if current word has Underlined attribute.
!!}
    function isUnderlined(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionOCRWordBox.isMonospace

<FM>Declaration<FC>
function isMonospace(): bool32; safecall;

<FM>Description<FN>
Returns true if current word is monospaced.
!!}
    function isMonospace(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionOCRWordBox.isSerif

<FM>Declaration<FC>
function isSerif(): bool32; safecall;

<FM>Description<FN>
Returns true if current word is Serif.
!!}
    function isSerif(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionOCRWordBox.isSmallCaps

<FM>Declaration<FC>
function isSmallCaps(): bool32; safecall;

<FM>Description<FN>
Returns true if current word is smallcaps.
!!}
    function isSmallCaps(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionOCRWordBox.getPointSize

<FM>Declaration<FC>
function getPointSize(): int32_t; safecall;

<FM>Description<FN>
Returns point size in printers points (1/72 inch).
!!}
    function getPointSize(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionOCRWordBox.getLanguage

<FM>Declaration<FC>
function getLanguage(): <A TIEVisionString>; safecall;

<FM>Description<FN>
Returns language code (ie 'eng') used to recognize this word.
!!}
    function getLanguage(wantExceptions: bool32 = false): TIEVisionString; safecall;

{!!
<FS>TIEVisionOCRWordBox.getScriptDirection

<FM>Declaration<FC>
function getScriptDirection(): <A TIEVisionOCRScriptDirection>; safecall;

<FM>Description<FN>
Return the overall directionality of this word.
!!}
    function getScriptDirection(wantExceptions: bool32 = false): TIEVisionOCRScriptDirection; safecall;

  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  TIEVisionImage = interface;   // forward declaration


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionVectorImageRef

<FM>Declaration<FC>
TIEVisionVectorImageRef = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of image references (<A TIEVisionImage>).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorImageRef.getImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorImageRef.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorImageRef.setImage></C> </R>
</TABLE>
!!}
  TIEVisionVectorImageRef = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorImageRef.getImage

<FM>Declaration<FC>
function getImage(pos: int32_t): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Returns the image reference at specified index.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Image index.</C> </R>
</TABLE>
!!}
    function getImage(pos: int32_t; wantExceptions: bool32 = false): TIEVisionImage; safecall;

{!!
<FS>TIEVisionVectorImageRef.push_back

<FM>Declaration<FC>
procedure push_back(value: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Appends a new image reference.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Image reference to set.</C> </R>
</TABLE>


<FM>Example<FC>
imagelist.push_back( image1 );
!!}
    procedure push_back(value: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorImageRef.setImage

<FM>Declaration<FC>
procedure setImage(pos: int32_t; value: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Replaces/Set the image reference.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Image index.</C> </R>
<R> <C><FC>value<FN></C> <C>Image reference to set.</C> </R>
</TABLE>


<FM>Example<FC>
imagelist.setImage( image1 );
!!}
    procedure setImage(pos: int32_t; value: TIEVisionImage; wantExceptions: bool32 = false); safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionVectorObjRef

<FM>Declaration<FC>
TIEVisionVectorObjRef = interface(<A TIEVisionBaseVector>)

<FM>Description<FN>
An object that exposes this interface can store a vector of object references (inherited from <A TIEVisionBase>).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorObjRef.getObj></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorObjRef.push_back></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionVectorObjRef.setObj></C> </R>
</TABLE>
!!}
  TIEVisionVectorObjRef = interface(TIEVisionBaseVector)

{!!
<FS>TIEVisionVectorObjRef.getObj

<FM>Declaration<FC>
function getObj(pos: int32_t): <A TIEVisionBase>; safecall;

<FM>Description<FN>
Returns the object at specified index.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Object index.</C> </R>
</TABLE>
!!}
    function getObj(pos: int32_t; wantExceptions: bool32 = false): TIEVisionBase; safecall;

{!!
<FS>TIEVisionVectorObjRef.push_back

<FM>Declaration<FC>
procedure push_back(value: <A TIEVisionBase>); safecall;

<FM>Description<FN>
Appends a new object reference.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Object reference to add.</C> </R>
</TABLE>
!!}
    procedure push_back(value: TIEVisionBase; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionVectorObjRef.setObj

<FM>Declaration<FC>
procedure setObj(pos: int32_t; value: <A TIEVisionBase>); safecall;

<FM>Description<FN>
Replaces/Sets an object reference.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>pos<FN></C> <C>Item index.</C> </R>
<R> <C><FC>value<FN></C> <C>Object reference to set.</C> </R>
</TABLE>
!!}
    procedure setObj(pos: int32_t; value: TIEVisionBase; wantExceptions: bool32 = false); safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionStream

<FM>Declaration<FC>
TIEVisionStream = interface(<A TIEVisionBase>)

<FM>Description<FN>
This is the base interface for all objects with streaming features.

<FM>See Also<FN>
- <A TIEVisionFileStream>
- <A TIEVisionTempFileStream>
- <A TIEVisionMemoryStream>
- <A TIEVisionExistingMemoryStream>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.eof></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.loadFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.read></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.saveToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.seek></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.silent_getc></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.silent_read></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.silent_write></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.size></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.tell></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionStream.write></C> </R>
</TABLE>
!!}
  TIEVisionStream = interface(TIEVisionBase)

{!!
<FS>TIEVisionStream.seek

<FM>Declaration<FC>
procedure seek(offset: int64; whence: <A TIEVisionSeekOffset>); safecall;

<FM>Description<FN>
Moves stream to the specified position.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>offset<FN></C> <C>Absolute or relative offset.</C> </R>
<R> <C><FC>whence<FN></C> <C>Where the offset starts.</C> </R>
</TABLE>


<FM>Example<FC>
stream1.seek(0, ievSET);  // reset position to zero
!!}
    procedure seek(offset: int64; whence: TIEVisionSeekOffset; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionStream.tell

<FM>Declaration<FC>
function tell(): int64; safecall;

<FM>Description<FN>
Returns current stream position.
!!}
    function tell(wantExceptions: bool32 = false): int64; safecall;

{!!
<FS>TIEVisionStream.silent_read

<FM>Declaration<FC>
function silent_read(ptr: pointer; size: int64): int64; safecall;

<FM>Description<FN>
Reads buffer from stream without raise exceptions on fail.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ptr<FN></C> <C>Pointer to destination buffer.</C> </R>
<R> <C><FC>size<FN></C> <C>Number of bytes to read.</C> </R>
</TABLE>
!!}
    function silent_read(ptr: pointer; size: int64; wantExceptions: bool32 = false): int64; safecall;

{!!
<FS>TIEVisionStream.silent_write

<FM>Declaration<FC>
function silent_write(ptr: pointer; size: int64): int64; safecall;

<FM>Description<FN>
Writes buffer to stream without raise exceptions on fail.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ptr<FN></C> <C>Point to source buffer.</C> </R>
<R> <C><FC>size<FN></C> <C>Number of bytes to write.</C> </R>
</TABLE>
!!}
function silent_write(ptr: pointer; size: int64; wantExceptions: bool32 = false): int64; safecall;

{!!
<FS>TIEVisionStream.silent_getc

<FM>Declaration<FC>
function silent_getc(): int32_t; safecall;

<FM>Description<FN>
Reads a char (8 bit) from stream.
!!}
function silent_getc(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionStream.eof

<FM>Declaration<FC>
function eof(): bool32; safecall;

<FM>Description<FN>
Tests whether the file position is at the end of a file.
!!}
function eof(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionStream.saveToFile

<FM>Declaration<FC>
procedure saveToFile(filename: PAnsiChar); overload; safecall;
procedure saveToFile(filename: PWideChar); overload; safecall;

<FM>Description<FN>
Saves the stream to the specified file.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>filename<FN></C> <C>Destination path and filename.</C> </R>
</TABLE>


<FM>Example<FC>
stream1.saveToFile('output.dat');
!!}
procedure saveToFile(filename: PAnsiChar; wantExceptions: bool32 = false); overload; safecall;
procedure saveToFile(filename: PWideChar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionStream.loadFromFile

<FM>Declaration<FC>
procedure loadFromFile(filename: PAnsiChar); overload; safecall;
procedure loadFromFile(filename: PWideChar); overload; safecall;

<FM>Description<FN>
Loads the stream from the specified file.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>filename<FN></C> <C>Source path and filename.</C> </R>
</TABLE>


<FM>Example<FC>
stream1.loadFromFile('input.dat');
!!}
procedure loadFromFile(filename: PAnsiChar; wantExceptions: bool32 = false); overload; safecall;
procedure loadFromFile(filename: PWideChar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionStream.read

<FM>Declaration<FC>
procedure read(ptr: pointer; size: int64); safecall;

<FM>Description<FN>
Reads buffer from stream.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ptr<FN></C> <C>Pointer to destination buffer.</C> </R>
<R> <C><FC>size<FN></C> <C>Number of bytes to read.</C> </R>
</TABLE>
!!}
    procedure read(ptr: pointer; size: int64; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionStream.write

<FM>Declaration<FC>
procedure write(ptr: pointer; size: int64); safecall;

<FM>Description<FN>
Writes buffer to stream.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ptr<FN></C> <C>Pointer to source buffer.</C> </R>
<R> <C><FC>XXX<FN></C> <C>Number of bytes to write.</C> </R>
</TABLE>
!!}
    procedure write(ptr: pointer; size: int64; wantExceptions: bool32 = false); safecall;

    // for internal use
    procedure iostream; cdecl;

{!!
<FS>TIEVisionStream.size

<FM>Declaration<FC>
function size(): int64; safecall;

<FM>Description<FN>
Returns the stream size.
!!}
    function size(wantExceptions: bool32 = false): int64; safecall;

    // for internal use
    procedure convertToCPlusPlus(outputStream: TIEVisionStream; className: PAnsiChar; compressed: bool32; wantExceptions: bool32 = false); safecall;

  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionFileStream

<FM>Declaration<FC>
TIEVisionFileStream = interface(<A TIEVisionStream>)

<FM>Description<FN>
This is the base interface for all objects with file streaming features.
This interface allows you to open/create files and access them using a streaming interface.

<FM>See Also<FN>
- <A TIEVisionFileStream>
- <A TIEVisionTempFileStream>
- <A TIEVisionMemoryStream>
- <A TIEVisionExistingMemoryStream>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionFileStream.close></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionFileStream.open></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionFileStream.open></C> </R>
</TABLE>
!!}
  TIEVisionFileStream = interface(TIEVisionStream)

{!!
<FS>TIEVisionFileStream.close

<FM>Declaration<FC>
procedure close(); safecall;

<FM>Description<FN>
Closes the file.
!!}
    procedure close(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionFileStream.open

<FM>Declaration<FC>
procedure open(filename: PAnsiChar; mode: <A TIEVisionFileStreamMode>); overload; safecall;
procedure open(filename: PWideChar; mode: <A TIEVisionFileStreamMode>); overload; safecall;

<FM>Description<FN>
Opens/Creates a file.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>filename<FN></C> <C>Path and name of the file to open.</C> </R>
<R> <C><FC>mode<FN></C> <C>Opening mode.</C> </R>
</TABLE>


<FM>Example<FC>
// opens 'input.jpg' in read only mode
filestream := IEVisionLib.createFileStream();
filestream.open('input.jpg', ievREAD);

// creates 'output.jpg'
filestream := IEVisionLib.createFileStream();
filestream.open('output.jpg', ievCREATE);
!!}
    procedure open(filename: PAnsiChar; mode: TIEVisionFileStreamMode; wantExceptions: bool32 = false); overload; safecall;
    procedure open(filename: PWideChar; mode: TIEVisionFileStreamMode; wantExceptions: bool32 = false); overload; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionTempFileStream

<FM>Declaration<FC>
TIEVisionTempFileStream = interface(<A TIEVisionFileStream>)

<FM>Description<FN>
This is the base interface for all objects with temporary file streaming features.
This interface allows you to create a temporary file that is removed when the class is disposed. The file name is chosen automatically.

<FM>See Also<FN>
- <A TIEVisionFileStream>
- <A TIEVisionTempFileStream>
- <A TIEVisionMemoryStream>
- <A TIEVisionExistingMemoryStream>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionTempFileStream.getFilename></C> </R>
</TABLE>

!!}
  TIEVisionTempFileStream = interface(TIEVisionFileStream)

{!!
<FS>TIEVisionTempFileStream.getFilename

<FM>Declaration<FC>
function getFilename(): PAnsiChar; safecall;

<FM>Description<FN>
Returns the automatically chosen file name.
!!}
    function getFilename(wantExceptions: bool32 = false): PAnsiChar; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionMemoryStream

<FM>Declaration<FC>
TIEVisionMemoryStream = interface(<A TIEVisionStream>)

<FM>Description<FN>
This is the base interface for all objects with memory streaming features.
This interface allows you to create a memory stream.

<FM>See Also<FN>
- <A TIEVisionFileStream>
- <A TIEVisionTempFileStream>
- <A TIEVisionMemoryStream>
- <A TIEVisionExistingMemoryStream>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionMemoryStream.clear></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMemoryStream.reserve></C> </R>
</TABLE>
!!}
  TIEVisionMemoryStream = interface(TIEVisionStream)

{!!
<FS>TIEVisionMemoryStream.clear

<FM>Declaration<FC>
procedure clear(); safecall;

<FM>Description<FN>
Empties the memory stream.
!!}
    procedure clear(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMemoryStream.reserve

<FM>Declaration<FC>
procedure reserve(count: int32_t); safecall;

<FM>Description<FN>
Allocates the specified number of bytes. The actual size of the stream doesn't change.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>count<FN></C> <C>Number of bytes to allocate.</C> </R>
</TABLE>
!!}
    procedure reserve(count: int32_t; wantExceptions: bool32 = false); safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionExistingMemoryStream

<FM>Declaration<FC>
TIEVisionExistingMemoryStream = interface(<A TIEVisionStream>)

<FM>Description<FN>
This is the base interface for all objects with existing memory streaming features.
This interface allows you to create a memory stream from existing stream.

<FM>See Also<FN>
- <A TIEVisionFileStream>
- <A TIEVisionTempFileStream>
- <A TIEVisionMemoryStream>
- <A TIEVisionExistingMemoryStream>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionExistingMemoryStream.reset></C> </R>
</TABLE>
  
!!}
  TIEVisionExistingMemoryStream = interface(TIEVisionStream)

{!!
<FS>TIEVisionExistingMemoryStream.reset

<FM>Declaration<FC>
procedure reset(existingBuffer: pointer; existingBufferSize: int32_t); safecall;

<FM>Description<FN>
Sets a new buffer for the memory stream.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>existingBuffer<FN></C> <C>Memory buffer pointer.</C> </R>
<R> <C><FC>existingBufferSize<FN></C> <C>Memory buffer size in bytes.</C> </R>
</TABLE>
!!}
    procedure reset(existingBuffer: pointer; existingBufferSize: int32_t; wantExceptions: bool32 = false); safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionTempFileName

<FM>Declaration<FC>
TIEVisionTempFileName = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to create temporary file name. The file name is chosen automatically.

<FM>See Also<FN>
- <A TIEVisionTempFileName>
- <A TIEVisionTempDirName>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionTempFileName.c_str></C> </R>
</TABLE>
  
!!}
  TIEVisionTempFileName = interface(TIEVisionBase)

{!!
<FS>TIEVisionTempFileName.c_str

<FM>Declaration<FC>
function c_str(): PAnsiChar; safecall;

<FM>Description<FN>
Returns the automatically chosen file name.
!!}
    function c_str(wantExceptions: bool32 = false): PAnsiChar; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionTempDirName

<FM>Declaration<FC>
TIEVisionTempDirName = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to create temporary directory name. The directory name is chosen automatically.

<FM>See Also<FN>
- <A TIEVisionTempFileName>
- <A TIEVisionTempDirName>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionTempDirName.c_str></C> </R>
</TABLE>
  
!!}
  TIEVisionTempDirName = interface(TIEVisionBase)

{!!
<FS>TIEVisionTempDirName.c_str

<FM>Declaration<FC>
function c_str(): PAnsiChar; safecall;

<FM>Description<FN>
Returns the automatically chosen file name.
!!}
    function c_str(wantExceptions: bool32 = false): PAnsiChar; safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionPropertyTree

<FM>Declaration<FC>
TIEVisionPropertyTree = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to create a property tree.
A property tree contains several types of data (doubles, ints, other sub property trees, etc...) associating a key for each value.
Keys are structured as paths (ie. "image.size.width") separated by periods.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.clear></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.erase></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.exists></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.getDouble></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.getInt32></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.getPropertyTree></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.getRect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.getString></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.load></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.operatorASSIGN></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.operatorEQUAL></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.putDouble></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.putInt32></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.putPropertyTree></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.putRect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.putString></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.save></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.size></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.sort></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionPropertyTree.swap></C> </R>
</TABLE>
!!}
  TIEVisionPropertyTree = interface(TIEVisionBase)

{!!
<FS>TIEVisionPropertyTree.size

<FM>Declaration<FC>
function size(): int32_t; safecall;

<FM>Description<FN>
Returns the number of properties
!!}
    function size(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionPropertyTree.clear

<FM>Declaration<FC>
procedure clear(); safecall;

<FM>Description<FN>
Removes all properties.
!!}
    procedure clear(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.save

<FM>Declaration<FC>
procedure save(stream: <A TIEVisionStream>; format: <A TIEVisionPropertyTreeFormat> = ievXML); overload; safecall;
procedure save(filename: PAnsiChar; format: <A TIEVisionPropertyTreeFormat> = ievXML); overload; safecall;

<FM>Description<FN>
Saves the entire property tree to file or stream, using the specified format.

<FM>See Also<FN>
- <A TIEVisionPropertyTree.load>

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>stream<FN></C> <C>Stream to write.</C> </R>
<R> <C><FC>format<FN></C> <C>Format of the output.</C> </R>
<R> <C><FC>filename<FN></C> <C>Filename to write.</C> </R>
</TABLE>
!!}
    procedure save(stream: TIEVisionStream; format: TIEVisionPropertyTreeFormat = ievXML; wantExceptions: bool32 = false); overload; safecall;
    procedure save(filename: PAnsiChar; format: TIEVisionPropertyTreeFormat = ievXML; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionPropertyTree.load

<FM>Declaration<FC>
procedure load(stream: <A TIEVisionStream>; format: <A TIEVisionPropertyTreeFormat> = ievXML); overload; safecall;
procedure load(filename: PAnsiChar; format: <A TIEVisionPropertyTreeFormat> = ievXML); overload; safecall;


<FM>Description<FN>
Loads a property tree from file or stream, using the specified format.

<FM>See Also<FN>
- <A TIEVisionPropertyTree.save>

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>stream<FN></C> <C>Stream to read.</C> </R>
<R> <C><FC>format<FN></C> <C>Format of the input.</C> </R>
<R> <C><FC>filename<FN></C> <C>Filename to read.</C> </R>
</TABLE>
!!}
    procedure load(stream: TIEVisionStream; format: TIEVisionPropertyTreeFormat = ievXML; wantExceptions: bool32 = false); overload; safecall;
    procedure load(filename: PAnsiChar; format: TIEVisionPropertyTreeFormat = ievXML; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionPropertyTree.operatorEQUAL

<FM>Declaration<FC>
function operatorEQUAL(rhs: <A TIEVisionPropertyTree>): bool32; safecall;

<FM>Description<FN>
Compares this property tree with the specified one.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rhs<FN></C> <C>The other property tree to compare.</C> </R>
</TABLE>
!!}
    function operatorEQUAL(rhs: TIEVisionPropertyTree; wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionPropertyTree.operatorASSIGN

<FM>Declaration<FC>
procedure operatorASSIGN(rhs: TIEVisionPropertyTree); safecall;

<FM>Description<FN>
Clones the content of specified parameter.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rhs<FN></C> <C>Property tree to clone.</C> </R>
</TABLE>
!!}
    procedure operatorASSIGN(rhs: TIEVisionPropertyTree; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.swap

<FM>Declaration<FC>
procedure swap(rhs: <A TIEVisionPropertyTree>); safecall;

<FM>Description<FN>
Swaps this property tree with the specified one.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ros<FN></C> <C>The other property tree to swap.</C> </R>
</TABLE>
!!}
    procedure swap(rhs: TIEVisionPropertyTree; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.exists

<FM>Declaration<FC>
function exists(keypath: PAnsiChar): bool32; safecall;

<FM>Description<FN>
Returns true if the key exists.
A key path is a list of words separated by periods (ie: "image.size.width").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to find.</C> </R>
</TABLE>


<FM>Example<FC>
if propertytree.exists('image.size.width') then
....
!!}
    function exists(keypath: PAnsiChar; wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionPropertyTree.erase

<FM>Declaration<FC>
procedure erase(keypath: PAnsiChar); safecall;

<FM>Description<FN>
Removes the specified key.
A key path is a list of words separated by periods (ie: "image.size.width").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to erase.</C> </R>
</TABLE>


<FM>Example<FC>
propertytree.erase('image.size.width');
!!}
    procedure erase(keypath: PAnsiChar; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.sort

<FM>Declaration<FC>
procedure sort(); safecall;

<FM>Description<FN>
Sorts all keys of the property tree.
!!}
    procedure sort(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.getPropertyTree

<FM>Declaration<FC>
function getPropertyTree(keypath: PAnsiChar): <A TIEVisionPropertyTree>; safecall;

<FM>Description<FN>
Retrieves value of the specified key. The value must be of <A TIEVisionPropertyTree> interface type.
A key path is a list of words separated by periods (ie: "image.size.width").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to retrieve.</C> </R>
</TABLE>


<FM>Example<FC>
propertytree2 := propertytree1.getPropertyTree('image.properties');
!!}
    function getPropertyTree(keypath: PAnsiChar; wantExceptions: bool32 = false): TIEVisionPropertyTree; safecall;

{!!
<FS>TIEVisionPropertyTree.putPropertyTree

<FM>Declaration<FC>
procedure putPropertyTree(keypath: PAnsiChar; propertyTree: <A TIEVisionPropertyTree>); safecall;

<FM>Description<FN>
Puts a property tree into this property tree, using the specified key path.
A key path is a list of words separated by periods (ie: "image.size.width").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to put.</C> </R>
<R> <C><FC>propertyTree<FN></C> <C>Value to put.</C> </R>
</TABLE>
!!}
    procedure putPropertyTree(keypath: PAnsiChar; propertyTree: TIEVisionPropertyTree; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.putString

<FM>Declaration<FC>
procedure putString(keypath: PAnsiChar; value: PAnsiChar); overload; safecall;
procedure putString(keypath: PAnsiChar; value: <A TIEVisionString>); overload; safecall;

<FM>Description<FN>
Puts an ANSI string or string object into this property tree, using the specified key path.
A key path is a list of words separated by periods (ie: "image.size.width").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to put.</C> </R>
<R> <C><FC>value<FN></C> <C>Value to put.</C> </R>
</TABLE>


<FM>Example<FC>
propertytree1.putString('image.size.width', 640);
propertytree1.putString('image.size.height', 480);
!!}
    procedure putString(keypath: PAnsiChar; value: PAnsiChar; wantExceptions: bool32 = false); overload; safecall;
    procedure putString(keypath: PAnsiChar; value: TIEVisionString; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionPropertyTree.getString

<FM>Declaration<FC>
function getString(keypath: PAnsiChar): <A TIEVisionString>; safecall;

<FM>Description<FN>
Retrieves value of the specified key. The value must be of <A TIEVisionString> interface type.
A key path is a list of words separated by periods (ie: "image.property.caption").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to get.</C> </R>
</TABLE>


<FM>Example<FC>
ShowMessage( AnsiString( propertytree1.getString('image.property.caption').c_str() ) );
!!}
    function getString(keypath: PAnsiChar; wantExceptions: bool32 = false): TIEVisionString; safecall;

{!!
<FS>TIEVisionPropertyTree.putInt32

<FM>Declaration<FC>
procedure putInt32(keypath: PAnsiChar; value: int32_t); safecall;

<FM>Description<FN>
Puts an integer (32 bit) into this property tree, using the specified key path.
A key path is a list of words separated by periods (ie: "image.size.width").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to put.</C> </R>
<R> <C><FC>value<FN></C> <C>Value to put.</C> </R>
</TABLE>


<FM>Example<FC>
propertytree1.putInt32('image.size.width', 640);
!!}
    procedure putInt32(keypath: PAnsiChar; value: int32_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.getInt32

<FM>Declaration<FC>
function getInt32(keypath: PAnsiChar): int32_t; safecall;

<FM>Description<FN>
Retrieves value of the specified key. The value must be an integer (32 bit).
A key path is a list of words separated by periods (ie: "image.size.width").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to get.</C> </R>
</TABLE>


<FM>Example<FC>
width := propertytree1.getInt32('image.size.width');
!!}
    function getInt32(keypath: PAnsiChar; wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionPropertyTree.putDouble

<FM>Declaration<FC>
procedure putDouble(keypath: PAnsiChar; value: double); safecall;

<FM>Description<FN>
Puts a double (64 bit floating point) into this property tree, using the specified key path.
A key path is a list of words separated by periods (ie: "image.property.EXIF_FNumber").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to put.</C> </R>
<R> <C><FC>value<FN></C> <C>Value to put.</C> </R>
</TABLE>


<FM>Example<FC>
propertytree.putDouble('image.property.EXIF_FNumber', 2.8);
!!}
    procedure putDouble(keypath: PAnsiChar; value: double; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.getDouble

<FM>Declaration<FC>
function getDouble(keypath: PAnsiChar): double; safecall;

<FM>Description<FN>
Retrieves value of the specified key. The value must be a double (64 bit floating point).
A key path is a list of words separated by periods (ie: "image.property.EXIF.FNumber").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to get.</C> </R>
</TABLE>


<FM>Example<FC>
width := propertytree1.getDouble('image.property.EXIF.FNumber');
!!}
    function getDouble(keypath: PAnsiChar; wantExceptions: bool32 = false): double; safecall;

{!!
<FS>TIEVisionPropertyTree.putRect

<FM>Declaration<FC>
procedure putRect(keypath: PAnsiChar; const rect: <A TIEVisionRect>); safecall;

<FM>Description<FN>
Puts a rectangle (<A TIEVisionRect>) into this property tree, using the specified key path.
A key path is a list of words separated by periods (ie: "image.ROI").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to put.</C> </R>
<R> <C><FC>value<FN></C> <C>Value to put.</C> </R>
</TABLE>


<FM>Example<FC>
propertytree.putRect('image.ROI', IEVisionRect(10, 10, 200, 200));
!!}
    procedure putRect(keypath: PAnsiChar; const rect: TIEVisionRect; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionPropertyTree.getRect

<FM>Declaration<FC>
function getRect(keypath: PAnsiChar): <A TIEVisionRect>; safecall;

<FM>Description<FN>
Retrieves value of the specified key. The value must be a <A TIEVisionRect> interface.
A key path is a list of words separated by periods (ie: "image.ROI").

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>keypath<FN></C> <C>Path of the key to get.</C> </R>
</TABLE>


<FM>Example<FC>
rect := propertytree1.getRect('image.ROI');
!!}
    function getRect(keypath: PAnsiChar; wantExceptions: bool32 = false): TIEVisionRect; safecall;

  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionImage

<FM>Declaration<FC>
TIEVisionImage = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to create raster images or matrices.
It is possible to perform basic input/output operations and several image processing tasks.
It is possible also to interface TIEVisionImage to ImageEn <a TIEBitmap>, sharing the same content.

<FM>Example<FC>
// creates empty image
image := IEVisionLib.createImage();

// creates an image from "input.jpg", resize to 100x100 and save as "output.jpg"
image := IEVisionLib.createImage('input.jpg');
image.resize(100, 100); // default interpolation = linear
image.save('output.jpg');

// creates an image of 1000x1000x24 bit
image := IEVisionLib.createImage(1000, 1000, ievUINT8, 3);

// creates an image from ImageEn <A TImageEnView> (shareing the content)
ImageEnView1.IEBitmap.Origin := ieboTOPLEFT;
image := IEVisionLib.createImage(ImageEnView1.IEBitmap.Width, ImageEnView1.IEBitmap.Height,
                                 ievUINT8, 3, ImageEnView1.IEBitmap.Rowlen,
                                 ImageEnView1.IEBitmap.ScanLine[0]);

// same of previous code
image := ImageEnView1.IEBitmap.GetIEVisionImage();

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.addWeighted></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.avg></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.avgSdv></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.camShift></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.cmp></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.convertScale></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.copy></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.copyFrom></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.countNonZero></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.create></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.createAlphaMask></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.DCT></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.determinant></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.DFT></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.dotProduct></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.eigenVV></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.equalizeHistogram></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.filter2D></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.findContours></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.flip></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getAlphaMask></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getChannelFormat></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getChannels></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getColumn></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getColumns></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getDiagonal></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getHeight></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getMetadata></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getRect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getRow></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getRowLen></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getRows></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getScanline></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.getWidth></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.inpaint></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.inRange></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.integral></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.invert></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.isAllWhite></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.isEmpty></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.LUT></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.makeBorder></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.max></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.merge></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.min></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.minMax></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.mulSpectrums></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.mulTransposed></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.norm></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.operatorASSIGN></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.opNot></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.perspectiveTransform></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.pixel_BGR8></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.pixel_float32></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.pixel_float64></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.pixel_RGB8></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.pixel_uint16></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.pixel_uint8></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.pyrDown></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.pyrUp></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.reduce></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.repeatImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.resize></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.save></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.scaleAdd></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.setIdentity></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.setValue></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.setZero></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.share></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.smooth></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.splitPlanes></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.swap></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImage.transpose></C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionLibrary.createImage>
!!}
  TIEVisionImage = interface(TIEVisionBase)

{!!
<FS>TIEVisionImage.create

<FM>Declaration<FC>
procedure create(width: int32_t; height: int32_t; channelFormat: <A TIEVisionChannelFormat>; channels: int32_t); safecall;

<FM>Description<FN>
Allocates (if necessary) an image with the specified parameters.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>width<FN></C> <C>Image width.</C> </R>
<R> <C><FC>height<FN></C> <C>Image height.</C> </R>
<R> <C><FC>channelFormat<FN></C> <C>Type of channels (8 bit, 8 bit unsigned, 16 bit, etc...).</C> </R>
<R> <C><FC>channels<FN></C> <C>Number of channels (1 = gray scale, 3 = color).</C> </R>
</TABLE>


<FM>Example<FC>
// create empty image
image := IEVisionLib.createImage();
// allocate 1000x1000 RGB image
image.create(1000, 1000, ievUINT8, 3);
!!}
    procedure create(width: int32_t; height: int32_t; channelFormat: TIEVisionChannelFormat; channels: int32_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.swap

<FM>Declaration<FC>
procedure swap(secondImage: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Swaps the content of specified image and this image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>secondImage<FN></C> <C>The other image to swap.</C> </R>
</TABLE>


<FM>Example<FC>
// swap content of image1 and image2
image1.swap(image2);
!!}
    procedure swap(secondImage: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.operatorASSIGN

<FM>Declaration<FC>
procedure operatorASSIGN(src: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Clones the source image replacing the old content.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Source image to clone.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionImage.share>


<FM>Example<FC>
// copies the entire content of image1 to image2
image2.operatorASSIGN(image1);
!!}
    procedure operatorASSIGN(src: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.share

<FM>Declaration<FC>
procedure share(src: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Shares the content of source image with this image, replacing the old content.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Image to share.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionImage.operatorASSIGN>


<FM>Example<FC>
// sares the entire content of image1 with image2
image2.share(image1);
!!}
    procedure share(src: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.getWidth

<FM>Declaration<FC>
function getWidth(): int32_t; safecall;

<FM>Description<FN>
Returns current image width.

<FM>See Also<FN>
- <A TIEVisionImage.getHeight>

!!}
    function getWidth(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionImage.getHeight

<FM>Declaration<FC>
function getHeight(): int32_t; safecall;

<FM>Description<FN>
Returns current image height.

<FM>See Also<FN>
- <A TIEVisionImage.getWidth>
!!}
    function getHeight(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionImage.getChannels

<FM>Declaration<FC>
function getChannels(): int32_t; safecall;

<FM>Description<FN>
Returns the number of channels of current image.
When 1 the image is gray scale. When >=3 the image is colored image (ie RGB).

<FM>See Also<FN>
- <A TIEVisionImage.getChannelFormat>
!!}
    function getChannels(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionImage.getChannelFormat

<FM>Declaration<FC>
function getChannelFormat(): <A TIEVisionChannelFormat>; safecall;

<FM>Description<FN>
Returns the type of channels.

<FM>See Also<FN>
- <A TIEVisionImage.getChannels>
!!}
    function getChannelFormat(wantExceptions: bool32 = false): TIEVisionChannelFormat; safecall;

{!!
<FS>TIEVisionImage.getRowLen

<FM>Declaration<FC>
function getRowLen(): int32_t; safecall;

<FM>Description<FN>
Returns the length, in bytes, of a single row.
!!}
    function getRowLen(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionImage.isEmpty

<FM>Declaration<FC>
function isEmpty(): bool32; safecall;

<FM>Description<FN>
Returns true if the object doesn't contain an image.
!!}
    function isEmpty(wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionImage.getScanline

<FM>Declaration<FC>
function getScanline(row: int32_t): pointer; safecall;

<FM>Description<FN>
Returns a pointer to the specified row.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>row<FN></C> <C>Row index to retrieve.</C> </R>
</TABLE>
!!}
    function getScanline(row: int32_t; wantExceptions: bool32 = false): pointer; safecall;

{!!
<FS>TIEVisionImage.pixel_uint8

<FM>Declaration<FC>
function pixel_uint8(row: int32_t; col: int32_t): uint8_p; safecall;

<FM>Description<FN>
Returns the pixel value at the specified position. Channel format must be 8 bit unsigned, single channel.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>row<FN></C> <C>Vertial position.</C> </R>
<R> <C><FC>col<FN></C> <C>Horizontal position.</C> </R>
</TABLE>


<FM>Example<FC>
// read value of pixel at 10, 10
value := image1.pixel_uint8(10, 10)^;
!!}
    function pixel_uint8(row: int32_t; col: int32_t; wantExceptions: bool32 = false): uint8_p; safecall;

{!!
<FS>TIEVisionImage.pixel_uint16

<FM>Declaration<FC>
function pixel_uint16(row: int32_t; col: int32_t): uint16_p; safecall;

<FM>Description<FN>
Returns the pixel value at the specified position. Channel format must be 16 bit unsigned, single channel.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>row<FN></C> <C>Vertial position.</C> </R>
<R> <C><FC>col<FN></C> <C>Horizontal position.</C> </R>
</TABLE>


<FM>Example<FC>
// read value of pixel at 10, 10
value := image1.pixel_uint16(10, 10)^;
!!}
    function pixel_uint16(row: int32_t; col: int32_t; wantExceptions: bool32 = false): uint16_p; safecall;

{!!
<FS>TIEVisionImage.pixel_float32

<FM>Declaration<FC>
function pixel_float32(row: int32_t; col: int32_t): float_p; safecall;

<FM>Description<FN>
Returns the pixel value at the specified position. Channel format must be 32 bit floating point, single channel.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>row<FN></C> <C>Vertial position.</C> </R>
<R> <C><FC>col<FN></C> <C>Horizontal position.</C> </R>
</TABLE>
!!}
    function pixel_float32(row: int32_t; col: int32_t; wantExceptions: bool32 = false): float_p; safecall;

{!!
<FS>TIEVisionImage.pixel_float64

<FM>Declaration<FC>
function pixel_float64(row: int32_t; col: int32_t): double_p; safecall;

<FM>Description<FN>
Returns the pixel value at the specified position. Channel format must be 64 bit floating point, single channel.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>row<FN></C> <C>Vertial position.</C> </R>
<R> <C><FC>col<FN></C> <C>Horizontal position.</C> </R>
</TABLE>
!!}
    function pixel_float64(row: int32_t; col: int32_t; wantExceptions: bool32 = false): double_p; safecall;

{!!
<FS>TIEVisionImage.pixel_RGB8

<FM>Declaration<FC>
function pixel_RGB8(row: int32_t; col: int32_t): <A PIEVisionRGB8>; safecall;

<FM>Description<FN>
Returns the pixel value at the specified position. Channel format must be 8 bit unsigned, three channels (diposed as R-G-B).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>row<FN></C> <C>Vertial position.</C> </R>
<R> <C><FC>col<FN></C> <C>Horizontal position.</C> </R>
</TABLE>
!!}
    function pixel_RGB8(row: int32_t; col: int32_t; wantExceptions: bool32 = false): PIEVisionRGB8; safecall;

{!!
<FS>TIEVisionImage.pixel_BGR8

<FM>Declaration<FC>
function pixel_BGR8(row: int32_t; col: int32_t): <A PIEVisionBGR8>; safecall;

<FM>Description<FN>
Returns the pixel value at the specified position. Channel format must be 8 bit unsigned, three channels (diposed as B-G-R).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>row<FN></C> <C>Vertial position.</C> </R>
<R> <C><FC>col<FN></C> <C>Horizontal position.</C> </R>
</TABLE>
!!}
    function pixel_BGR8(row: int32_t; col: int32_t; wantExceptions: bool32 = false): PIEVisionBGR8; safecall;

{!!
<FS>TIEVisionImage.getRect

<FM>Declaration<FC>
function getRect(rect: TIEVisionRect): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Creates a new TIEVisionImage object that contains the specified region. No copy is performed, only the actual area is shared among the objects.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rect<FN></C> <C>Region to share.</C> </R>
</TABLE>


<FM>Example<FC>
ROI_image := image1.getRect(IEVisionRect(10, 10, 200, 200));
!!}
    function getRect(rect: TIEVisionRect; wantExceptions: bool32 = false): TIEVisionImage; safecall;

{!!
<FS>TIEVisionImage.getColumn

<FM>Declaration<FC>
function getColumn(col: int32_t): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Creates a new TIEVisionImage object that contains the specified column. No copy is performed, only the actual area is shared among the objects.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>col<FN></C> <C>Column to share.</C> </R>
</TABLE>


<FM>Example<FC>
ROI_image := image1.getColumn(5);
!!}
    function getColumn(col: int32_t; wantExceptions: bool32 = false): TIEVisionImage; safecall;

{!!
<FS>TIEVisionImage.getColumns

<FM>Declaration<FC>
function getColumns(startCol: int32_t; endCol: int32_t): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Creates a new TIEVisionImage object that contains the specified range of columns. No copy is performed, only the actual area is shared among the objects.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>startCol<FN></C> <C>First column to share.</C> </R>
<R> <C><FC>endCol<FN></C> <C>Last column (not included) to share.</C> </R>
</TABLE>


<FM>Example<FC>
ROI_image := image1.getColumns(10, 100);
!!}
    function getColumns(startCol: int32_t; endCol: int32_t; wantExceptions: bool32 = false): TIEVisionImage; safecall;
    
{!!
<FS>TIEVisionImage.getRow

<FM>Declaration<FC>
function getRow(row: int32_t): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Creates a new TIEVisionImage object that contains the specified single row. No copy is performed, only the actual area is shared among the objects.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>row<FN></C> <C>Row to share.</C> </R>
</TABLE>


<FM>Example<FC>
ROI_image := image1.getRow(25);
!!}
    function getRow(row: int32_t; wantExceptions: bool32 = false): TIEVisionImage; safecall;

{!!
<FS>TIEVisionImage.getRows

<FM>Declaration<FC>
function getRows(startRow: int32_t; endRow: int32_t): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Creates a new TIEVisionImage object that contains the specified range of rows. No copy is performed, only the actual area is shared among the objects.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>startRow<FN></C> <C>First row to share.</C> </R>
<R> <C><FC>endRow<FN></C> <C>Last row (not included) to share.</C> </R>
</TABLE>


<FM>Example<FC>
ROI_image := image1.getRows(10, 100);
!!}
    function getRows(startRow: int32_t; endRow: int32_t; wantExceptions: bool32): TIEVisionImage; safecall;

{!!
<FS>TIEVisionImage.getDiagonal

<FM>Declaration<FC>
function getDiagonal(diag: int32_t): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Creates a new TIEVisionImage object that contains the specified diagonal. No copy is performed, only the actual area is shared among the objects.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>diag<FN></C> <C>Index of the diagonal, with the following values: d=0 is the main diagonal, d>0 is a diagonal from the lower half. For example, d=1 means the diagonal is set imme- diately below the main one. d<0 is a diagonal from the upper half. For example, d=1 means the diagonal is set imme- diately above the main one.</C> </R>
</TABLE>
!!}
    function getDiagonal(diag: int32_t; wantExceptions: bool32 = false): TIEVisionImage; safecall;

{!!
<FS>TIEVisionImage.getAlphaMask

<FM>Declaration<FC>
function getAlphaMask(): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Returns current alpha mask.

<FM>See Also<FN>
- <A TIEVisionImage.createAlphaMask>


<FM>Example<FC>
image1.createAlphaMask();
alpha := image1.getAlphaMask();
!!}
    function getAlphaMask(wantExceptions: bool32 = false): TIEVisionImage; safecall;

{!!
<FS>TIEVisionImage.createAlphaMask

<FM>Declaration<FC>
procedure createAlphaMask(); safecall;

<FM>Description<FN>
Creates the alpha mask.

<FM>See Also<FN>
- <A TIEVisionImage.getAlphaMask>
!!}
    procedure createAlphaMask(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.save

<FM>Declaration<FC>
procedure save(filename: PAnsiChar); overload; safecall;
procedure save(filename: PAnsiChar; fileType: <A TIEVisionFileFormat>); overload; safecall;

<FM>Description<FN>
Saves current image to the specified file.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>filename<FN></C> <C>Filename of output file.</C> </R>
<R> <C><FC>fileType<FN></C> <C>File format of the file.</C> </R>
</TABLE>


<FM>Example<FC>
// save to output.jpg, as jpeg
image1.save('output.jpg');

// save to output.dat, as jpeg
image1.save('output.dat', ievJPEG);
!!}
    procedure save(filename: PAnsiChar; wantExceptions: bool32 = false); overload; safecall;
    procedure save(filename: PAnsiChar; fileType: TIEVisionFileFormat; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.getMetadata

<FM>Declaration<FC>
function getMetadata(): <A TIEVisionPropertyTree>; safecall;

<FM>Description<FN>
Returns a property tree with all meta properties of the image.
!!}
    function getMetadata(wantExceptions: bool32 = false): TIEVisionPropertyTree; safecall;

{!!
<FS>TIEVisionImage.resize

<FM>Declaration<FC>
function resize(dst: <A TIEVisionImage>; interpolation: <A TIEVisionInterpolation> = ievLINEAR): <A TIEVisionImage>; overload; safecall;
function resize(newWidth: int32_t; newHeight: int32_t; interpolation: <A TIEVisionInterpolation> = ievLINEAR): <A TIEVisionImage>; overload; safecall;

<FM>Description<FN>
Resizes (resamples) current image to the specified size, using an interpolation filter.
First overload stores the resized image in the specified destination object. Uses destination size as resize parameters. Source image remains untouched. Returns the destination object.
Second overload resizes the image inplace, returning itself.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the resized image.</C> </R>
<R> <C><FC>interpolation<FN></C> <C>Interpolation filter.</C> </R>
<R> <C><FC>newWidth<FN></C> <C>New image width. If less than 0, this is autocalculated.</C> </R>
<R> <C><FC>newHeight<FN></C> <C>New image height. If less than 0, this is autocalculated.</C> </R>
</TABLE>


<FM>Example<FC>
// resizes image1 to 1000x1000, using CUBIC interpolation
image1.resize(1000, 1000, ievCUBIC);

// resizes image1 to the size of image2 (1000x1000)
image2.create(1000, 1000, ievUINT8, 3);
image1.resize(image2, ievCUBIC);
!!}
    function resize(dst: TIEVisionImage; interpolation: TIEVisionInterpolation = ievLINEAR; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    function resize(newWidth: int32_t; newHeight: int32_t; interpolation: TIEVisionInterpolation = ievLINEAR; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;

{!!
<FS>TIEVisionImage.convertColor

<FM>Declaration<FC>
function convertColor(dst: TIEVisionImage; code: <A TIEVisionCvtColorCode>): <A TIEVisionImage>; overload; safecall;
function convertColor(code: <A TIEVisionCvtColorCode>): <A TIEVisionImage>; overload; safecall;

<FM>Description<FN>
Converts from a color space to another color space.
First overload stores the resulting image into specified destination. Returns the destination object.
Second overload stores the resulting image inplace. Returns itself.

Channels format and channels count are set automatically.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the destination image.</C> </R>
<R> <C><FC>code<FN></C> <C>Specifies source and destination color space.</C> </R>
</TABLE>


<FM>Example<FC>
// Converts BGR image to gray scale
image.convertColor(ievBGR2GRAY);
!!}
    function convertColor(dst: TIEVisionImage; code: TIEVisionCvtColorCode; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    function convertColor(code: TIEVisionCvtColorCode; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    
{!!
<FS>TIEVisionImage.flip

<FM>Declaration<FC>
function flip(dst: <A TIEVisionImage>; flipMode: <A TIEVisionFlipMode>): <A TIEVisionImage>; overload; safecall;
function flip(flipMode: <A TIEVisionFlipMode>): <A TIEVisionImage>; overload; safecall;

<FM>Description<FN>
Flips current image.
First overload stores the resulting image into specified destination. Returns the destination object.
Second overload stores the resulting image inplace. Returns itself.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the destination image.</C> </R>
<R> <C><FC>flipMode<FN></C> <C>Specifies the axis (X, Y or XY).</C> </R>
</TABLE>


<FM>Example<FC>
// horizontal flip
image1.flip(ievX_AXIS);
!!}
    function flip(dst: TIEVisionImage; flipMode: TIEVisionFlipMode; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    function flip(flipMode: TIEVisionFlipMode; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;

{!!
<FS>TIEVisionImage.equalizeHistogram

<FM>Declaration<FC>
procedure equalizeHistogram(dst: <A TIEVisionImage>); overload; safecall;
procedure equalizeHistogram(); overload; safecall;

<FM>Description<FN>
Equalizes the histogram.
First overload places the result into destination object.
Second overload stores the resulting image inplace.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the destination image.</C> </R>
</TABLE>


<FM>Example<FC>
image1.equalizeHistogram();
!!}
    procedure equalizeHistogram(dst: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure equalizeHistogram(wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.makeBorder

<FM>Declaration<FC>
procedure makeBorder(dst: <A TIEVisionImage>; top, bottom, left, right: int32_t; borderType: <A TIEVisionBorderType>; value: <A TIEVisionScalar>); overload; safecall;
procedure makeBorder(top, bottom, left, right: int32_t; borderType: <A TIEVisionBorderType>; value: <A TIEVisionScalar>); overload; safecall;

<FM>Description<FN>
Forms a border around the image.
First overload places the result into destination object.
Second overload stores the resulting image inplace.

Resulting image size will be width = old_width + left + right, height = old_height + top + bottom.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the destination image.</C> </R>
<R> <C><FC>top<FN></C> <C>Specifies how many pixels on the top to add.</C> </R>
<R> <C><FC>bottom<FN></C> <C>Specifies how many pixels on the bottom to add.</C> </R>
<R> <C><FC>left<FN></C> <C>Specifies how many pixels on the left to add.</C> </R>
<R> <C><FC>right<FN></C> <C>Specifies how many pixels on the right to add.</C> </R>
<R> <C><FC>borderType<FN></C> <C>Border type.</C> </R>
<R> <C><FC>value<FN></C> <C>Border value if borderType is ievBORDER_CONSTANT.</C> </R>
</TABLE>
!!}
    procedure makeBorder(dst: TIEVisionImage; top, bottom, left, right: int32_t; borderType: TIEVisionBorderType; value: TIEVisionScalar; wantExceptions: bool32 = false); overload; safecall;
    procedure makeBorder(top, bottom, left, right: int32_t; borderType: TIEVisionBorderType; value: TIEVisionScalar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.smooth

<FM>Declaration<FC>
procedure smooth(dst: <A TIEVisionImage>; smoothType: <A TIEVisionSmoothType> = ievGAUSSIAN; param1: int32_t = 3; param2: int32_t = 0; param3: double = 0.0; param4: double = 0.0); overload; safecall;
procedure smooth(smoothType: <A TIEVisionSmoothType> = ievGAUSSIAN; param1: int32_t = 3; param2: int32_t = 0; param3: double = 0.0; param4: double = 0.0); overload; safecall;

<FM>Description<FN>
Smooths the image.
First overload places the result into destination object.
Second overload stores the resulting image inplace.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the destination image.</C> </R>
<R> <C><FC>smoothType<FN></C> <C>Type of the smoothing.</C> </R>
<R> <C><FC>param1<FN></C> <C>Parameter 1. Depends by smoothType.</C> </R>
<R> <C><FC>param2<FN></C> <C>Parameter 2. Depends by smoothType.</C> </R>
<R> <C><FC>param3<FN></C> <C>Parameter 3. Depends by smoothType.</C> </R>
<R> <C><FC>param4<FN></C> <C>Parameter 4. Depends by smoothType.</C> </R>
</TABLE>

<FC>smoothType<FN> can be: 
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ievBLUR_NO_SCALE<FN></C> <C>Linear convolution with param1 x param2 box kernel (all 1's). To smooth different pixels with different-size box kernels, you can use the integral image that is computed using <A TIEVisionImage.integral>.</C> </R>
<R> <C><FC>ievBLUR<FN></C> <C>Linear convolution with param1 x param2 box kernel (all 1's) with subsequent scaling by 1 / (param1 * param2).</C> </R>
<R> <C><FC>ievGAUSSIAN<FN></C> <C>Linear convolution with a param1 x param2 Gaussian kernel.</C> </R>
<R> <C><FC>ievMEDIAN<FN></C> <C>Median filter with a param1 x param1 square aperture.</C> </R>
<R> <C><FC>ievBILATERAL<FN></C> <C>Bilateral filter with a param1 x param1 square aperture, color sigma= param3 and spatial sigma= param4. If param1=0, the aperture square side is set to round(param4*1.5)*2+1.</C> </R>
</TABLE>
!!}
    procedure smooth(dst: TIEVisionImage; smoothType: TIEVisionSmoothType = ievGAUSSIAN; param1: int32_t = 3; param2: int32_t = 0; param3: double = 0.0; param4: double = 0.0; wantExceptions: bool32 = false); overload; safecall;
    procedure smooth(smoothType: TIEVisionSmoothType = ievGAUSSIAN; param1: int32_t = 3; param2: int32_t = 0; param3: double = 0.0; param4: double = 0.0; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.filter2D

<FM>Declaration<FC>
procedure filter2D(dst: <A TIEVisionImage>; kernel: <A TIEVisionImage>; anchor: <A TIEVisionPoint>); overload; safecall;
procedure filter2D(kernel: <A TIEVisionImage>; anchor: <A TIEVisionPoint>); overload; safecall;

<FM>Description<FN>
Convolves an image with the kernel.
First overload places the result into destination object.
Second overload stores the resulting image inplace.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the destination image.</C> </R>
<R> <C><FC>kernel<FN></C> <C>Convolution kernel (or rather a correlation kernel), a single-channel floating point matrix. To apply different kernels to different channels, split the image into separate color planes using <A TIEVisionImage.splitPlanes> and process them individually.</C> </R>
<R> <C><FC>anchor<FN></C> <C>Anchor of the kernel that indicates the relative position of a filtered point within the kernel. The anchor should lie within the kernel. The special default value (-1, -1) means that the anchor is at the kernel center.</C> </R>
</TABLE>
!!}
    procedure filter2D(dst: TIEVisionImage; kernel: TIEVisionImage; anchor: TIEVisionPoint; wantExceptions: bool32 = false); overload; safecall;
    procedure filter2D(kernel: TIEVisionImage; anchor: TIEVisionPoint; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.integral

<FM>Declaration<FC>
procedure integral(sum: <A TIEVisionImage>); overload; safecall;
procedure integral(sum: <A TIEVisionImage>; sqsum: <A TIEVisionImage>; tilted_sum: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Calculates the integral of an image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>sum<FN></C> <C>Integral image as (W + 1) x (H + 1) , 32-bit integer or floating-point (32f or 64f).</C> </R>
<R> <C><FC>sqsum<FN></C> <C>Integral image for squared pixel values. It is (W + 1) x (H + 1), double-precision floating-point (64f) image (matrix).</C> </R>
<R> <C><FC>tilted_sum<FN></C> <C>Integral for the image rotated by 45 degrees. It is (W + 1) x (H + 1) image (matrix) with the same data type as sum.</C> </R>
</TABLE>
!!}
    procedure integral(sum: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure integral(sum: TIEVisionImage; sqsum: TIEVisionImage; tilted_sum: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.pyrDown

<FM>Declaration<FC>
procedure pyrDown(dst: <A TIEVisionImage>); overload; safecall;
procedure pyrDown(); overload; safecall;

<FM>Description<FN>
Smoothes an image and downsamples it.
First overload places the result into destination object.
Second overload stores the resulting image inplace.

Size of the destination image is computed as new_width = (width+1)/2, new_height = (height+1)/2).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the destination image.</C> </R>
</TABLE>
!!}
    procedure pyrDown(dst: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure pyrDown(wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.pyrUp

<FM>Declaration<FC>
procedure pyrUp(dst: <A TIEVisionImage>); overload; safecall;
procedure pyrUp(); overload; safecall;

<FM>Description<FN>
Upsamples an image and then smoothes it.
First overload places the result into destination object.
Second overload stores the resulting image inplace.

Size of the destination image is computed as new_width = width*2, new_height = height*2

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dst<FN></C> <C>Container for the destination image.</C> </R>
</TABLE>
!!}
    procedure pyrUp(dst: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure pyrUp(wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.splitPlanes

<FM>Declaration<FC>
procedure splitPlanes(plane0: <A TIEVisionImage>); overload; safecall;
procedure splitPlanes(plane0: <A TIEVisionImage>; plane1: <A TIEVisionImage>); overload; safecall;
procedure splitPlanes(plane0: <A TIEVisionImage>; plane1: <A TIEVisionImage>; plane2: <A TIEVisionImage>); overload; safecall;
procedure splitPlanes(plane0: <A TIEVisionImage>; plane1: <A TIEVisionImage>; plane2: <A TIEVisionImage>; plane3: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Divides a multi-channel image (matrix) into several single-channel images.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>plane0<FN></C> <C>Container for plane 0.</C> </R>
<R> <C><FC>plane1<FN></C> <C>Container for plane 1.</C> </R>
<R> <C><FC>plane2<FN></C> <C>Container for plane 2.</C> </R>
<R> <C><FC>plane3<FN></C> <C>Container for plane 3.</C> </R>
</TABLE>


<FM>Example<FC>
blue := IEVisionLib.createImage();
green := IEVisionLib.createImage();
red := IEVisionLib.createImage();
image1.splitPlanes(blue, green, red);
!!}
    procedure splitPlanes(plane0: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure splitPlanes(plane0: TIEVisionImage; plane1: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure splitPlanes(plane0: TIEVisionImage; plane1: TIEVisionImage; plane2: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure splitPlanes(plane0: TIEVisionImage; plane1: TIEVisionImage; plane2: TIEVisionImage; plane3: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.minMax

<FM>Declaration<FC>
procedure minMax(out minVal: double; out maxVal: double); overload; safecall;
procedure minMax(out minVal: double; out maxVal: double; out minLoc: <A TIEVisionPoint>; out maxLoc: <A TIEVisionPoint>); overload; safecall;
procedure minMax(out minVal: double; out maxVal: double; out minLoc: <A TIEVisionPoint>; out maxLoc: <A TIEVisionPoint>; mask: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Finds the global minimum and maximum in a whole image or sub-image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>minVal<FN></C> <C>Output for minimum value.</C> </R>
<R> <C><FC>maxVal<FN></C> <C>Output for maximum value.</C> </R>
<R> <C><FC>minLoc<FN></C> <C>Minimum value location.</C> </R>
<R> <C><FC>maxLoc<FN></C> <C>Maximum value location.</C> </R>
</TABLE>
!!}
    procedure minMax(out minVal: double; out maxVal: double; wantExceptions: bool32 = false); overload; safecall;
    procedure minMax(out minVal: double; out maxVal: double; out minLoc: TIEVisionPoint; out maxLoc: TIEVisionPoint; wantExceptions: bool32 = false); overload; safecall;
    procedure minMax(out minVal: double; out maxVal: double; out minLoc: TIEVisionPoint; out maxLoc: TIEVisionPoint; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.camShift

<FM>Declaration<FC>
procedure camShift(var window: <A TIEVisionRect>; criteria: <A TIEVisionTermCriteria>; out area: double; out box: <A TIEVisionBox2D>); overload; safecall;
function camShift(var window: <A TIEVisionRect>; criteria: <A TIEVisionTermCriteria>): <A TIEVisionRotatedRect>; overload; safecall;

<FM>Description<FN>
Finds an object center, size, and orientation.
First overload outputs area as double and bounding box as <A TIEVisionBox2D>.
Second overload returns a <A TIEVisionRotatedRect> record.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>window<FN></C> <C>Initial search window.</C> </R>
<R> <C><FC>criteria<FN></C> <C>Search stop criteria.</C> </R>
<R> <C><FC>area<FN></C> <C>Area of found box.</C> </R>
<R> <C><FC>box<FN></C> <C>Found box.</C> </R>
</TABLE>

<FM>Demos<FN>
IEVision\TrackObjects
IEVision\TrackObjects_LowLevel
!!}
    procedure camShift(var window: TIEVisionRect; criteria: TIEVisionTermCriteria; out area: double; out box: TIEVisionBox2D; wantExceptions: bool32 = false); overload; safecall;
    function camShift(var window: TIEVisionRect; criteria: TIEVisionTermCriteria; wantExceptions: bool32 = false): TIEVisionRotatedRect; overload; safecall;

{!!
<FS>TIEVisionImage.inRange

<FM>Declaration<FC>
procedure inRange(lower: <A TIEVisionScalar>; upper: <A TIEVisionScalar>; dest: <A TIEVisionImage>); overload; safecall;
procedure inRange(lower: <A TIEVisionImage>; upper: <A TIEVisionImage>; dest: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Checks if image pixels lie between the specified values.
First overload gets a range of two scalar values.
Second overload gets an matrix (image) of lower and upper values.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lower<FN></C> <C>Lower value or matrix of values.</C> </R>
<R> <C><FC>upper<FN></C> <C>Upper value or matrix of value.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container used to store results. Contains 255 if related pixel is inside the range.</C> </R>
</TABLE>
!!}
    procedure inRange(lower: TIEVisionScalar; upper: TIEVisionScalar; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure inRange(lower: TIEVisionImage; upper: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.convertScale

<FM>Declaration<FC>
function convertScale(dest: <A TIEVisionImage>; scale: double = 1.0; shift: double = 0.0): <A TIEVisionImage>; overload; safecall;
function convertScale(channelFormat: <A TIEVisionChannelFormat>; scale: double = 1.0; shift: double = 0.0): <A TIEVisionImage>; overload; safecall;

<FM>Description<FN>
Converts an image to another datatype with optional scaling.
First overload stores the resulting image into destination object. It returns the destination image.
Second overload stores the resulting image inplace. It returns itself.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container used to store results.</C> </R>
<R> <C><FC>scale<FN></C> <C>Scale factor (1.0 = no scale).</C> </R>
<R> <C><FC>shift<FN></C> <C>Delta added to the scaled values.</C> </R>
<R> <C><FC>channelFormat<FN></C> <C>Used to change the channel format.</C> </R>
</TABLE>
!!}
    function convertScale(dest: TIEVisionImage; scale: double = 1.0; shift: double = 0.0; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    function convertScale(channelFormat: TIEVisionChannelFormat; scale: double = 1.0; shift: double = 0.0; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;

{!!
<FS>TIEVisionImage.countNonZero

<FM>Declaration<FC>
function countNonZero(): int32_t; safecall;

<FM>Description<FN>
Counts non-zero image pixels.
!!}
    function countNonZero(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionImage.isAllWhite

<FM>Declaration<FC>
function isAllWhite(): bool32; safecall;

<FM>Description<FN>
Returns true if the image contains all whites (255 values).
!!}
    function isAllWhite(wantExceptions: bool32): bool32; safecall;

{!!
<FS>TIEVisionImage.addWeighted

<FM>Declaration<FC>
procedure addWeighted(alpha: double; rhs: <A TIEVisionImage>; beta: double; gamma: double; dest: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Computes the weighted sum of two images, executing: dest = this*alpha + rhs*beta + gamma

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>alpha<FN></C> <C>Weight for the first image pixels.</C> </R>
<R> <C><FC>rhs<FN></C> <C>Second source image of the same size and channel number.</C> </R>
<R> <C><FC>beta<FN></C> <C>Weight for the second image pixels.</C> </R>
<R> <C><FC>gamma<FN></C> <C>Scalar added to each sum.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
</TABLE>
!!}
    procedure addWeighted(alpha: double; rhs: TIEVisionImage; beta: double; gamma: double; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.avg

<FM>Declaration<FC>
function avg(mask: <A TIEVisionImage>): <A TIEVisionScalar>; overload; safecall;
function avg(): <A TIEVisionScalar>; overload; safecall;

<FM>Description<FN>
Calculates an average (mean) of image pixels.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>mask<FN></C> <C>Operation mask (processes only pixels with mask > 0).</C> </R>
</TABLE>
!!}
    function avg(mask: TIEVisionImage; wantExceptions: bool32 = false): TIEVisionScalar; overload; safecall;
    function avg(wantExceptions: bool32 = false): TIEVisionScalar; overload; safecall;

{!!
<FS>TIEVisionImage.avgSdv

<FM>Declaration<FC>
procedure avgSdv(out mean: <A TIEVisionScalar>; out stdDev: <A TIEVisionScalar>; mask: <A TIEVisionImage>); overload; safecall;
procedure avgSdv(out mean: <A TIEVisionScalar>; out stdDev: <A TIEVisionScalar>); overload; safecall;

<FM>Description<FN>
Calculates a mean and standard deviation of image pixels.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>mean<FN></C> <C>Outputs calculated mean.</C> </R>
<R> <C><FC>stdDev<FN></C> <C>Output calculated standard deviation.</C> </R>
<R> <C><FC>mask<FN></C> <C>Operation mask (processes only pixels with mask > 0).</C> </R>
</TABLE>
!!}
    procedure avgSdv(out mean: TIEVisionScalar; out stdDev: TIEVisionScalar; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure avgSdv(out mean: TIEVisionScalar; out stdDev: TIEVisionScalar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.cmp

<FM>Declaration<FC>
procedure cmp(rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; cmpOp: <A TIEVisionCmpOp>); overload; safecall;
procedure cmp(rhs: double; dest: <A TIEVisionImage>; cmpOp: <A TIEVisionCmpOp>); overload; safecall;

<FM>Description<FN>
Performs the per-element comparison of two images or an image and scalar value.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rhs<FN></C> <C>Right-side image to compare.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the results.</C> </R>
<R> <C><FC>cmpOp<FN></C> <C>Compare operation.</C> </R>
</TABLE>
!!}
    procedure cmp(rhs: TIEVisionImage; dest: TIEVisionImage; cmpOp: TIEVisionCmpOp; wantExceptions: bool32 = false); overload; safecall;
    procedure cmp(rhs: double; dest: TIEVisionImage; cmpOp: TIEVisionCmpOp; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.copy

<FM>Declaration<FC>
procedure copy(dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure copy(dest: <A TIEVisionImage>); overload; safecall;
procedure copy(const srcRect: <A TIEVisionRect>; dest: <A TIEVisionImage>); overload; safecall;
function copy(const srcRect: <A TIEVisionRect>): <A TIEVisionImage>; overload; safecall;
procedure copy(const srcRect: <A TIEVisionRect>; const dstRect: <A TIEVisionRect>; dest: <A TIEVisionImage>); overload; safecall;
procedure copy(const srcRect: <A TIEVisionRect>; destPtr: pointer; rowLen: int32_t); overload; safecall;

<FM>Description<FN>
Copies current image to the specified destination.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>XXX</C> </R>
<R> <C><FC>mask<FN></C> <C>XXX</C> </R>
<R> <C><FC>srcRect<FN></C> <C>XXX</C> </R>
<R> <C><FC>dstRect<FN></C> <C>XXX</C> </R>
<R> <C><FC>destPtr<FN></C> <C>XXX</C> </R>
<R> <C><FC>rowLen<FN></C> <C>XXX</C> </R>
</TABLE>


<FM>Example<FC>
// overload nr.1: copies image1 to destination, using specified mask (only pixels with mask > 0)
destination := IEVisionLib.createImage();
image1.copy(destination, mask);

// overload nr.2: copies image1 to destination
destination := IEVisionLib.createImage();
image1.copy(destination);

// overload nr.3: copies specified rectangle of image1 to destination
destination := IEVisionLib.createImage();
image1.copy(IEVisionRect(10, 10, 200, 200), destination);

// overload nr.4: creates a new image from the specified rectangle
destination := image1.copy(IEVisionRect(10, 10, 200, 200));

// overload nr.5: copies specified rectangle of image1 to specified rectangle of destination
destination := IEVisionLib.createImage();
image1.copy(IEVisionRect(10, 10, 200, 200), IEVisionRect(50, 50, 200, 200), destination);

// overload nr.6: copies specified rectangle to destination buffer
image1.copy(IEVisionRect(10, 10, 200, 200), destPointer, destRowLen);
!!}
    procedure copy(dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure copy(dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure copy(const srcRect: TIEVisionRect; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    function copy(const srcRect: TIEVisionRect; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    procedure copy(const srcRect: TIEVisionRect; const dstRect: TIEVisionRect; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure copy(const srcRect: TIEVisionRect; destPtr: pointer; rowLen: int32_t; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.copyFrom

<FM>Declaration<FC>
procedure copyFrom(srcChannelFormat: <A TIEVisionChannelFormat>; srcChannels: int32_t; srcRowLen: int32_t; srcData: pointer); safecall;

<FM>Description<FN>
Copies image from memory buffer.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>srcChannelFormat<FN></C> <C>Source image channel format.</C> </R>
<R> <C><FC>srcChannels<FN></C> <C>Source image number of channels.</C> </R>
<R> <C><FC>srcRowLen<FN></C> <C>Source image row length.</C> </R>
<R> <C><FC>srcData<FN></C> <C>Source image buffer.</C> </R>
</TABLE>


<FM>Example<FC>
// copies RGB image from ImageEnView1 (TImageEnView)
image1 := IEVisionLib.createImage();
image1.copyFrom(ievUINT8, 3, ImageEnView1.IEBitmap.RowLen, ImageEnView1.IEBitmap.Scanline[ImageEnView1.IEBitmap.Height-1]);
image1.flip(ievY_AXIS);
!!}
    procedure copyFrom(srcChannelFormat: TIEVisionChannelFormat; srcChannels: int32_t; srcRowLen: int32_t; srcData: pointer; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.setZero

<FM>Declaration<FC>
procedure setZero(); safecall;

<FM>Description<FN>
Sets all pixels to zero.
!!}
    procedure setZero(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.DCT

<FM>Declaration<FC>
procedure DCT(dest: <A TIEVisionImage>; flags: <A TIEVisionDCTFlags>); safecall;

<FM>Description<FN>
Performs a forward or inverse discrete Cosine transform of the image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
<R> <C><FC>flags<FN></C> <C>Transformation flags.</C> </R>
</TABLE>
!!}
    procedure DCT(dest: TIEVisionImage; flags: TIEVisionDCTFlags; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.DFT

<FM>Declaration<FC>
procedure DFT(dest: <A TIEVisionImage>; flags: <A TIEVisionDFTFlags>; nonZeroRows: int32_t = 0); safecall;

<FM>Description<FN>
Performs a forward or inverse Discrete Fourier transform of a 1D or 2D floating-point image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
<R> <C><FC>flags<FN></C> <C>Transformation flags.</C> </R>
<R> <C><FC>nonZeroRows<FN></C> <C>When the parameter is not zero, the function assumes that only the first nonzeroRows rows of the input image (ievDFT_INVERSE is not set) or only the first nonzeroRows of the output image (ievDFT_INVERSE is set) contain non-zeros.</C> </R>
</TABLE>
!!}
    procedure DFT(dest: TIEVisionImage; flags: TIEVisionDFTFlags; nonZeroRows: int32_t = 0; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.determinant

<FM>Declaration<FC>
function determinant(): double; safecall;

<FM>Description<FN>
Returns the determinant of a square floating-point image (matrix).
!!}
    function determinant(wantExceptions: bool32 = false): double; safecall;

{!!
<FS>TIEVisionImage.dotProduct

<FM>Declaration<FC>
function dotProduct(rhs: <A TIEVisionImage>): double; safecall;

<FM>Description<FN>
Calculates the dot product in Euclidian metrics.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image.</C> </R>
</TABLE>
!!}
    function dotProduct(rhs: TIEVisionImage; wantExceptions: bool32 = false): double; safecall;

{!!
<FS>TIEVisionImage.eigenVV

<FM>Declaration<FC>
procedure eigenVV(evects: <A TIEVisionImage>; evals: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Computes eigenvalues and eigenvectors of a symmetric image (matrix).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>evects<FN></C> <C>Output container for the eigen vectors.</C> </R>
<R> <C><FC>evals<FN></C> <C>Output container for the eigen values.</C> </R>
</TABLE>
!!}
    procedure eigenVV(evects: TIEVisionImage; evals: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.invert

<FM>Declaration<FC>
function invert(dest: <A TIEVisionImage>; invertMethod: <A TIEVisionInvertMethod>): double; safecall;

<FM>Description<FN>
Finds the inverse or pseudo-inverse of an image (matrix).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
<R> <C><FC>invertMethod<FN></C> <C>Inversion method.</C> </R>
</TABLE>
!!}
    function invert(dest: TIEVisionImage; invertMethod: TIEVisionInvertMethod; wantExceptions: bool32 = false): double; safecall;

{!!
<FS>TIEVisionImage.LUT

<FM>Declaration<FC>
procedure LUT(dest: <A TIEVisionImage>; lut: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Performs a look-up table transform of an image. Source must be an image of 8-bit pixels.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
<R> <C><FC>lut<FN></C> <C>Look-up table of 256 elements. In case of multi-channel source image, the table should either have a single channel (in this case the same table is used for all channels) or the same number of channels as in the source image.</C> </R>
</TABLE>
!!}
    procedure LUT(dest: TIEVisionImage; lut: TIEVisionImage; wantExceptions: bool32 = false); safecall;
    
{!!
<FS>TIEVisionImage.max

<FM>Declaration<FC>
procedure max(source1: <A TIEVisionImage>; source2: <A TIEVisionImage>); overload; safecall;
procedure max(source1: <A TIEVisionImage>; source2: double); overload; safecall;

<FM>Description<FN>
Calculates per-element maximum of two image or an image and a scalar.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>source1<FN></C> <C>First source image.</C> </R>
<R> <C><FC>source2<FN></C> <C>Second source image or scalar value.</C> </R>
</TABLE>
!!}
    procedure max(source1: TIEVisionImage; source2: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure max(source1: TIEVisionImage; source2: double; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.merge

<FM>Declaration<FC>
procedure merge(channel1: <A TIEVisionImage>; channel2: <A TIEVisionImage>; channel3: <A TIEVisionImage>); overload; safecall;
procedure merge(channel1: <A TIEVisionImage>; channel2: <A TIEVisionImage>; channel3: <A TIEVisionImage>; channel4: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Merges specified channels and places the result in this object.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>channel1<FN></C> <C>Channel 1.</C> </R>
<R> <C><FC>channel2<FN></C> <C>Channel 2.</C> </R>
<R> <C><FC>channel3<FN></C> <C>Channel 3.</C> </R>
<R> <C><FC>channel4<FN></C> <C>Channel 4.</C> </R>
</TABLE>


<FM>Example<FC>
image1.merge(channel_Blue, channel_Green, channel_Red);
!!}
    procedure merge(channel1: TIEVisionImage; channel2: TIEVisionImage; channel3: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure merge(channel1: TIEVisionImage; channel2: TIEVisionImage; channel3: TIEVisionImage; channel4: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.min

<FM>Declaration<FC>
procedure min(source1: <A TIEVisionImage>; source2: <A TIEVisionImage>); overload; safecall;
procedure min(source1: <A TIEVisionImage>; source2: double); overload; safecall;

<FM>Description<FN>
Calculates per-element minimum of two image or an image and a scalar.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>source1<FN></C> <C>First source image.</C> </R>
<R> <C><FC>source2<FN></C> <C>Second source image or scalar value.</C> </R>
</TABLE>
!!}
    procedure min(source1: TIEVisionImage; source2: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure min(source1: TIEVisionImage; source2: double; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.mulSpectrums

<FM>Declaration<FC>
procedure mulSpectrums(rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; flags: <A TIEVisionMulSpectrumsFlags>); safecall;

<FM>Description<FN>
Performs the per-element multiplication of two Fourier spectrums.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
<R> <C><FC>flags<FN></C> <C>Operation flags.</C> </R>
</TABLE>
!!}
    procedure mulSpectrums(rhs: TIEVisionImage; dest: TIEVisionImage; flags: TIEVisionMulSpectrumsFlags; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.mulTransposed

<FM>Declaration<FC>
procedure mulTransposed(dest: <A TIEVisionImage>; order: bool32; delta: <A TIEVisionImage>; scale: double = 1.0); overload; safecall;
procedure mulTransposed(dest: <A TIEVisionImage>; order: bool32; scale: double = 1.0); overload; safecall;

<FM>Description<FN>
Calculates the product of an image (matrix) and its transposition.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
<R> <C><FC>order<FN></C> <C>If true performs: st = scale(src - delta)T (src - delta), otherwise performs: dst = scale(src - delta)(src - delta)T</C> </R>
<R> <C><FC>delta<FN></C> <C>Optional delta matrix subtracted from src before the multiplication.</C> </R>
<R> <C><FC>scale<FN></C> <C>Optional scale factor for the matrix product.</C> </R>
</TABLE>
!!}
    procedure mulTransposed(dest: TIEVisionImage; order: bool32; delta: TIEVisionImage; scale: double = 1.0; wantExceptions: bool32 = false); overload; safecall;
    procedure mulTransposed(dest: TIEVisionImage; order: bool32; scale: double = 1.0; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.norm

<FM>Declaration<FC>
function norm(): double; overload; safecall;
function norm(mask: <A TIEVisionImage>): double; overload; safecall;

<FM>Description<FN>
Calculates an absolute image norm, an absolute difference norm, or a relative difference norm.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>mask<FN></C> <C>Operation mask.</C> </R>
</TABLE>
!!}
    function norm(wantExceptions: bool32 = false): double; overload; safecall;
    function norm(mask: TIEVisionImage; wantExceptions: bool32 = false): double; overload; safecall;

{!!
<FS>TIEVisionImage.opNot

<FM>Declaration<FC>
procedure opNot(dest: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Inverts every bit of an image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
</TABLE>
!!}
    procedure opNot(dest: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.perspectiveTransform

<FM>Declaration<FC>
procedure perspectiveTransform(dest: <A TIEVisionImage>; matrix: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Performs the perspective image (matrix) transformation of vectors.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
<R> <C><FC>matrix<FN></C> <C>3x3 or 4x4 floating-point transformation matrix.</C> </R>
</TABLE>
!!}
    procedure perspectiveTransform(dest: TIEVisionImage; matrix: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.reduce

<FM>Declaration<FC>
procedure reduce(dest: <A TIEVisionImage>; dim: int32_t = -1; op: <A TIEVisionReduceOp> = ievREDUCE_SUM); safecall;

<FM>Description<FN>
Reduces an image (matrix) to a vector.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image.</C> </R>
<R> <C><FC>dim<FN></C> <C>Dimension index along which the matrix is reduced. 0 means that the matrix is reduced to a single row. 1 means that the matrix is reduced to a single column.</C> </R>
<R> <C><FC>op<FN></C> <C>Reduction operation.</C> </R>
</TABLE>
!!}
    procedure reduce(dest: TIEVisionImage; dim: int32_t = -1; op: TIEVisionReduceOp = ievREDUCE_SUM; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.repeatImage

<FM>Declaration<FC>
procedure repeatImage(ny, nx: int32_t; dest: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Fills the destination image with repeated copies of the source image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ny<FN></C> <C>Flag to specify how many times the source is repeated along the vertical axis.</C> </R>
<R> <C><FC>nx<FN></C> <C>Flag to specify how many times the source is repeated along the horizontal axis.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for resuling image.</C> </R>
</TABLE>
!!}
    procedure repeatImage(ny, nx: int32_t; dest: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.scaleAdd

<FM>Declaration<FC>
procedure scaleAdd(scale: double; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Calculates the sum of a scaled image and another image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>scale<FN></C> <C>Scale factor for left-hand side image (this).</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for resulting image.</C> </R>
</TABLE>
!!}
    procedure scaleAdd(scale: double; rhs: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.setValue

<FM>Declaration<FC>
procedure setValue(value: <A TIEVisionScalar>; mask: <A TIEVisionImage>); overload; safecall;
procedure setValue(value: <A TIEVisionScalar>); overload; safecall;

<FM>Description<FN>
Sets all or some of the image pixels to the specified value.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Scalar value to set.</C> </R>
<R> <C><FC>mask<FN></C> <C>Operation mask (set only when > 0).</C> </R>
</TABLE>


<FM>Example<FC>
// fills with BGR = (200, 100, 0)
image1.setValue(IEVisionScalar(200, 100, 0));
!!}
    procedure setValue(value: TIEVisionScalar; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure setValue(value: TIEVisionScalar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionImage.setIdentity

<FM>Declaration<FC>
procedure setIdentity(); safecall;

<FM>Description<FN>
Initializes a scaled identity image (matrix).
!!}
    procedure setIdentity(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.transpose

<FM>Declaration<FC>
procedure transpose(); safecall;

<FM>Description<FN>
Transposes an image (matrix).
!!}
    procedure transpose(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImage.findContours

<FM>Declaration<FC>
function findContours(mode: <A TIEVisionContourRetrMode>; method: <A TIEVisionContourApproxMethod>; offset: <A TIEVisionPoint>): <A TIEVisionVectorObjRef>; safecall;

<FM>Description<FN>
Finds contours.
Image must be ievUINT8, 1 channel. Result will be a vector of vector of points (TIEVisionVectorObjRef will contain TIEVision VectorPoint objects).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>mode<FN></C> <C>Contour retrieval mode.</C> </R>
<R> <C><FC>method<FN></C> <C>Contour approximation method.</C> </R>
<R> <C><FC>offset<FN></C> <C>Optional offset by which every contour point is shifted.</C> </R>
</TABLE>
!!}
    function findContours(mode: TIEVisionContourRetrMode; method: TIEVisionContourApproxMethod; offset: TIEVisionPoint; wantExceptions: bool32 = false): TIEVisionVectorObjRef; safecall;

{!!
<FS>TIEVisionImage.inpaint

<FM>Declaration<FC>
procedure inpaint(mask: <A TIEVisionImage>; range: double; method: <A TIEVisionInpaintMethod> = ievINPAINT_NS); overload; safecall;
procedure inpaint(brushWidth: int32_t; brushHeight: int32_t; const subimageRect: <A TIEVisionRect>; range: double; method: <A TIEVisionInpaintMethod> = ievINPAINT_NS); overload; safecall;

<FM>Description<FN>
Restores the selected region in an image using the region neighborhood.
First overload needs a mask to identify the area to be inpainted.
Second overload builds automatically the mask using specified brush size.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>mask<FN></C> <C>Inpainting mask, 8-bit 1-channel image. Non-zero pixels indicate the area that needs to be inpainted.</C> </R>
<R> <C><FC>range<FN></C> <C>Radius of a circlular neighborhood of each point inpainted that is considered by the algorithm.</C> </R>
<R> <C><FC>method<FN></C> <C> Inpainting method</C> </R>
<R> <C><FC>brushWidth<FN></C> <C>Width of area to inpaint.</C> </R>
<R> <C><FC>brushHeight<FN></C> <C>Height of area to inpaint.</C> </R>
<R> <C><FC>subimageRect<FN></C> <C>Rectangle of area of interest (which includes the area to inpaint).</C> </R>
</TABLE>


<FM>Demo<FN>
IEVision\Inpaint_Brush
IEVision\Inpaint_Selection
!!}
    procedure inpaint(mask: TIEVisionImage; range: double; method: TIEVisionInpaintMethod = ievINPAINT_NS; wantExceptions: bool32 = false); overload; safecall;
    procedure inpaint(brushWidth: int32_t; brushHeight: int32_t; const subimageRect: TIEVisionRect; range: double; method: TIEVisionInpaintMethod = ievINPAINT_NS; wantExceptions: bool32 = false); overload; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionHistogram

<FM>Declaration<FC>
TIEVisionHistogram = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to handle an histogram.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.calc></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.calcBackProject></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.clear></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.getValue></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.minMax></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.normalize></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.operatorASSIGN></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.size></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHistogram.swap></C> </R>
</TABLE>
!!}
  TIEVisionHistogram = interface(TIEVisionBase)

{!!
<FS>TIEVisionHistogram.getValue

<FM>Declaration<FC>
function getValue(index0: int32_t): single; overload; safecall;
function getValue(index0: int32_t; index1: int32_t): single; overload; safecall;
function getValue(index0: int32_t; index1: int32_t; index2: int32_t): single; overload; safecall;

<FM>Description<FN>
Returns an item of the histogram.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>index0<FN></C> <C>Index of dimension 0.</C> </R>
<R> <C><FC>index1<FN></C> <C>Index of dimension 1.</C> </R>
<R> <C><FC>index2<FN></C> <C>Index of dimension 2.</C> </R>
</TABLE>
!!}
    function getValue(index0: int32_t; wantExceptions: bool32 = false): single; overload; safecall;
    function getValue(index0: int32_t; index1: int32_t; wantExceptions: bool32 = false): single; overload; safecall;
    function getValue(index0: int32_t; index1: int32_t; index2: int32_t; wantExceptions: bool32 = false): single; overload; safecall;

{!!
<FS>TIEVisionHistogram.size

<FM>Declaration<FC>
function size(index: int32_t): int32_t; safecall;

<FM>Description<FN>
Returns the size of specified dimension.
!!}
    function size(index: int32_t; wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionHistogram.clear

<FM>Declaration<FC>
procedure clear(); safecall;

<FM>Description<FN>
Clears the histogram.
!!}
    procedure clear(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionHistogram.swap

<FM>Declaration<FC>
procedure swap(secondHist: <A TIEVisionHistogram>); safecall;

<FM>Description<FN>
Exchanges the content of two histograms.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>secondHist<FN></C> <C>Other object to exchange.</C> </R>
</TABLE>
!!}
    procedure swap(secondHist: TIEVisionHistogram; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionHistogram.operatorASSIGN

<FM>Declaration<FC>
procedure operatorASSIGN(src: <A TIEVisionHistogram>); safecall;

<FM>Description<FN>
Replaces current content with the content of specified object.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Object to assign to.</C> </R>
</TABLE>
!!}
    procedure operatorASSIGN(src: TIEVisionHistogram; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionHistogram.normalize

<FM>Declaration<FC>
procedure normalize(factor: double); safecall;

<FM>Description<FN>
Normalizes the histogram.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>factor<FN></C> <C>Normalization factor.</C> </R>
</TABLE>
!!}
    procedure normalize(factor: double; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionHistogram.calc

<FM>Declaration<FC>
procedure calc(image: <A TIEVisionImage>; accumulate: bool32 = false); overload; safecall;
procedure calc(image: <A TIEVisionImage>; accumulate: bool32; mask: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Populates histogram with the content of the specified image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Source image.</C> </R>
<R> <C><FC>accumulate<FN></C> <C>If it is set, the histogram is not cleared in the beginning when it is allocated.</C> </R>
<R> <C><FC>mask<FN></C> <C>Operational mask.</C> </R>
</TABLE>
!!}
    procedure calc(image: TIEVisionImage; accumulate: bool32 = false; wantExceptions: bool32 = false); overload; safecall;
    procedure calc(image: TIEVisionImage; accumulate: bool32; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionHistogram.calcBackProject

<FM>Declaration<FC>
procedure calcBackProject(image: <A TIEVisionImage>; backProject: TIEVisionImage); safecall;

<FM>Description<FN>
Calculates the back projection of a histogram.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Source image.</C> </R>
<R> <C><FC>backProject<FN></C> <C>Destination image.</C> </R>
</TABLE>
!!}
    procedure calcBackProject(image: TIEVisionImage; backProject: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionHistogram.minMax

<FM>Declaration<FC>
procedure minMax(out vmin: single; out vmax: single); safecall;

<FM>Description<FN>
Returns minimum and maximum histogram values.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>vmin<FN></C> <C>Minimum value.</C> </R>
<R> <C><FC>vmax<FN></C> <C>Maximum value.</C> </R>
</TABLE>
!!}
    procedure minMax(out vmin: single; out vmax: single; wantExceptions: bool32 = false); safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionCascadeClassifier

<FM>Declaration<FC>
TIEVisionCascadeClassifier = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to handle a cascade classifier.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionCascadeClassifier.detectObjects></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionCascadeClassifier.empty></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionCascadeClassifier.load></C> </R>
</TABLE>
!!}
  TIEVisionCascadeClassifier = interface(TIEVisionBase)

{!!
<FS>TIEVisionCascadeClassifier.load

<FM>Declaration<FC>
procedure load(stream: <A TIEVisionStream>); overload; safecall;
procedure load(filename: PAnsiChar); overload; safecall;

<FM>Description<FN>
Loads a cascade classifier from file or stream.
It is possible also to load an internal detector using following strings:
":EYE"
":EYETREEEYEGLASSES"
":FRONTALFACEALT"
":FRONTALFACEALT2"
":FRONTALFACEALTTREE"
":FRONTALFACEDEFAULT"
":FULLBODY"
":LOWERBODY"
":PROFILEFACE"
":UPPERBODY"
":LEFTEYE2SPLITS"
":MCSEYEPAIRBIG"
":MCSEYEPAIRSMALL"
":MCSLEFTEYE"
":MCSMOUTH"
":MCSNOSE"
":MCSRIGHTEYE"
":MCSUPPERBODY"
":RIGHTEYE2SPLITS"
":MCSLEFTEAR"
":MCSRIGHTEAR"

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>stream<FN></C> <C>Input stream.</C> </R>
<R> <C><FC>filename<FN></C> <C>Input filename or internal detector string.</C> </R>
</TABLE>

<FM>Demo<FN>
IEVision\FaceDetection
IEVision\FaceDetection_LowLevel
IEVision\GetFaces
IEVision\TrackObjects
IEVision\TrackObjects_LowLevel
!!}
    procedure load(stream: TIEVisionStream; wantExceptions: bool32 = false); overload; safecall;
    procedure load(filename: PAnsiChar; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionCascadeClassifier.detectObjects

<FM>Declaration<FC>
function detectObjects(image: <A TIEVisionImage>; scaleFactor: double; minNeighbors: int32_t; flags: <A TIEVisionHaarDetectObjectsFlags>; minSize: <A TIEVisionSize>; maxSize: <A TIEVisionSize>): <A TIEVisionVectorRect>; safecall;

<FM>Description<FN>
Detects objects of different sizes in the input image. The detected objects are returned as a list of rectangles.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Image where to find the objects.</C> </R>
<R> <C><FC>scaleFactor<FN></C> <C>Parameter specifying how much the image size is reduced at each image scale.</C> </R>
<R> <C><FC>minNeighbors<FN></C> <C>Parameter specifying how many neighbors each candiate rectangle should have to retain it.</C> </R>
<R> <C><FC>flags<FN></C> <C>Detection flags.</C> </R>
<R> <C><FC>minSize<FN></C> <C>Minimum possible object size. Objects smaller than that are ignored.</C> </R>
<R> <C><FC>maxSize<FN></C> <C>Maximum possible object size. Objects larger than that are ignored.</C> </R>
</TABLE>

<FM>Demos<FN>
IEVision\FaceDetection
IEVision\FaceDetection_LowLevel
IEVision\GetFaces
IEVision\TrackObjects
IEVision\TrackObjects_LowLevel
!!}
    function detectObjects(image: TIEVisionImage; scaleFactor: double;
                           minNeighbors: int32_t; flags: TIEVisionHaarDetectObjectsFlags;
                           minSize: TIEVisionSize; maxSize: TIEVisionSize; wantExceptions: bool32 = false): TIEVisionVectorRect; safecall;

{!!
<FS>TIEVisionCascadeClassifier.empty

<FM>Declaration<FC>
function empty(): bool32; safecall;

<FM>Description<FN>
Returns true if the cascade classifier is empty (it isn't loaded).
!!}
    function empty(wantExceptions: bool32 = false): bool32; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionImageList

<FM>Declaration<FC>
TIEVisionImageList = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to handle a list of images.
This is a base interface that is inherited by <A TIEVisionMemoryImageList>, <A TIEVisionMemorySharedImageList> and <A TIEVisionTempFileImageList>.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionImageList.clear></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImageList.getImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImageList.insert></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImageList.pushBack></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImageList.remove></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImageList.setImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionImageList.size></C> </R>
</TABLE>
!!}
  TIEVisionImageList = interface(TIEVisionBase)

{!!
<FS>TIEVisionImageList.insert

<FM>Declaration<FC>
function insert(position: int32_t; image: <A TIEVisionImage>): int32_t; overload; safecall;
function insert(position: int32_t; filename: PAnsiChar): int32_t; overload; safecall;

<FM>Description<FN>
Inserts an image into the specified index.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>position<FN></C> <C>Destination index.</C> </R>
<R> <C><FC>image<FN></C> <C>Image to insert.</C> </R>
<R> <C><FC>filename<FN></C> <C>Filename of the image to load and insert.</C> </R>
</TABLE>
!!}
    function insert(position: int32_t; image: TIEVisionImage; wantExceptions: bool32 = false): int32_t; overload; safecall;
    function insert(position: int32_t; filename: PAnsiChar; wantExceptions: bool32 = false): int32_t; overload; safecall;

{!!
<FS>TIEVisionImageList.remove

<FM>Declaration<FC>
procedure remove(position: int32_t); safecall;

<FM>Description<FN>
Removes the image at specified index.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>position<FN></C> <C>Index of image to remove.</C> </R>
</TABLE>
!!}
    procedure remove(position: int32_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImageList.clear

<FM>Declaration<FC>
procedure clear(); safecall;

<FM>Description<FN>
Removes all images in the list.
!!}
    procedure clear(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImageList.getImage

<FM>Declaration<FC>
function getImage(position: int32_t): <A TIEVisionImage>; safecall;

<FM>Description<FN>
Returns the image at specified index.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>position<FN></C> <C>Index of image to retrieve.</C> </R>
</TABLE>
!!}
    function getImage(position: int32_t; wantExceptions: bool32 = false): TIEVisionImage; safecall;

{!!
<FS>TIEVisionImageList.size

<FM>Declaration<FC>
function size(): int32_t; safecall;

<FM>Description<FN>
Returns number of images in the list.
!!}
    function size(wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionImageList.setImage

<FM>Declaration<FC>
procedure setImage(position: int32_t; image: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Replaces image at the index with the specified one.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>position<FN></C> <C>Index of image to replace.</C> </R>
<R> <C><FC>image<FN></C> <C>Image to set.</C> </R>
</TABLE>
!!}
    procedure setImage(position: int32_t; image: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionImageList.pushBack

<FM>Declaration<FC>
function pushBack(image: <A TIEVisionImage>): int32_t; overload; safecall;
function pushBack(filename: PAnsiChar): int32_t; overload; safecall;

<FM>Description<FN>
Appends the specified image to the end of list.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Image to set.</C> </R>
<R> <C><FC>filename<FN></C> <C>Filename of the image to set.</C> </R>
</TABLE>
!!}
    function pushBack(image: TIEVisionImage; wantExceptions: bool32 = false): int32_t; overload; safecall;
    function pushBack(filename: PAnsiChar; wantExceptions: bool32 = false): int32_t; overload; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionMemoryImageList

<FM>Declaration<FC>
TIEVisionMemoryImageList = interface(<A TIEVisionImageList>)

<FM>Description<FN>
This interface allows you to handle a list of images stored in memory.
!!}
  TIEVisionMemoryImageList = interface(TIEVisionImageList)
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionMemorySharedImageList

<FM>Declaration<FC>
TIEVisionMemorySharedImageList = interface(<A TIEVisionImageList>)

<FM>Description<FN>
This interface allows you to handle a list of images stored in memory as shared object (the content is shared with the source images).
!!}
  TIEVisionMemorySharedImageList = interface(TIEVisionImageList)
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionTempFileImageList

<FM>Declaration<FC>
TIEVisionTempFileImageList = interface(<A TIEVisionImageList>)

<FM>Description<FN>
This interface allows you to handle a list of images stored on disk and removed when the object is destroyed.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionTempFileImageList.getFilename></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionTempFileImageList.insertExistingFile></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionTempFileImageList.pushBackExistingFile></C> </R>
</TABLE>
!!}
  TIEVisionTempFileImageList = interface(TIEVisionImageList)

{!!
<FS>TIEVisionTempFileImageList.insertExistingFile

<FM>Declaration<FC>
function insertExistingFile(position: int32_t; filename: PAnsiChar; deleteOnDestroy: bool32 = true): int32_t; safecall;

<FM>Description<FN>
Inserts an existing file to the list.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>position<FN></C> <C>Inserting position.</C> </R>
<R> <C><FC>filename<FN></C> <C>Filename of existing file.</C> </R>
<R> <C><FC>deleteOnDestroy<FN></C> <C>Flag to delete the file on destroy.</C> </R>
</TABLE>
!!}
    function insertExistingFile(position: int32_t; filename: PAnsiChar; deleteOnDestroy: bool32 = true; wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionTempFileImageList.pushBackExistingFile

<FM>Declaration<FC>
function pushBackExistingFile(filename: PAnsiChar; deleteOnDestroy: bool32 = true): int32_t; safecall;

<FM>Description<FN>
Appends an existing file to the list.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Filename of existing file.<FN></C> <C>XXX</C> </R>
<R> <C><FC>deleteOnDestroy<FN></C> <C>Flag to delete the file on destroy.</C> </R>
</TABLE>
!!}
    function pushBackExistingFile(filename: PAnsiChar; deleteOnDestroy: bool32 = true; wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionTempFileImageList.getFilename

<FM>Declaration<FC>
function getFilename(position: int32_t): PAnsiChar; safecall;

<FM>Description<FN>
Returns the filename assigned to the specified image index.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>position<FN></C> <C>Index of the image filename to retrieve.</C> </R>
</TABLE>
!!}
    function getFilename(position: int32_t; wantExceptions: bool32 = false): PAnsiChar; safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionHaarTraining

<FM>Declaration<FC>
TIEVisionHaarTraining = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to create training samples and learn an Haar cascade classifier to recognize new objects.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionHaarTraining.createCascadeClassifier></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHaarTraining.createTrainingSamples></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionHaarTraining.trainCascadeClassifier></C> </R>
</TABLE>
!!}
  TIEVisionHaarTraining = interface(TIEVisionBase)

{!!
<FS>TIEVisionHaarTraining.createTrainingSamples

<FM>Declaration<FC>
procedure createTrainingSamples(dstSamplesFilename: PAnsiChar; sampleImage: <A TIEVisionImage>; backgroundImageList: <A TIEVisionImageList>; samplesCount: int32_t; bgColor: int32_t; bgThreshold: int32_t = 80; invert: <A TIEVisionHaarInvert> = ievNOINVERT; maxIntensityDeviation: int32_t = 40; maxXAngle: double = 1.1; maxYAngle: double = 1.1; maxZAngle: double = 0.5; width: int32_t = 24; height: int32_t = 24); overload; safecall;
procedure createTrainingSamples(dstSamplesFilename: PAnsiChar; srcImageList: <A TIEVisionImageList>; width: int32_t = 24; height: int32_t = 24); overload; safecall;

<FM>Description<FN>
Creates a file with a set of training sample images.

First overload gets a single positive image and a list of backgrounds (negative samples) and mixes them to build the training samples file.

Second overload gets a list of images specifying a set of positive rectangles for each image and builds the training samples file.
Each image in srcImageList must contain bounding box of the objects to train, under the "HaarTraining.Objects" tree. The path is, for example:
"HaarTraining.Objects.Marisa" contains a Rect object.
"HaarTraining.Objects.Bill" contains a Rect object.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dstSamplesFilename<FN></C> <C>The resulting training samples file.</C> </R>
<R> <C><FC>sampleImage<FN></C> <C>A single positive image.</C> </R>
<R> <C><FC>backgroundImageList<FN></C> <C>A set of negative images.</C> </R>
<R> <C><FC>samplesCount<FN></C> <C>Number of samples to generate.</C> </R>
<R> <C><FC>bgColor<FN></C> <C>Background color.</C> </R>
<R> <C><FC>bgThreshold<FN></C> <C>Background threshold.</C> </R>
<R> <C><FC>invert<FN></C> <C>Randomly invert images.</C> </R>
<R> <C><FC>maxIntensityDeviation<FN></C> <C>Maximum intensity deviation.</C> </R>
<R> <C><FC>maxXAngle<FN></C> <C>Random rotation maximum X angle.</C> </R>
<R> <C><FC>maxYAngle<FN></C> <C>Random rotation maximum Y angle.</C> </R>
<R> <C><FC>maxZAngle<FN></C> <C>Random rotation maximum Z angle.</C> </R>
<R> <C><FC>width<FN></C> <C>Sample width.</C> </R>
<R> <C><FC>height<FN></C> <C>Sample height.</C> </R>
<R> <C><FC>srcImageList<FN></C> <C>A set of images containing rectangles with positive samples.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionHaarTraining.trainCascadeClassifier>
!!}
    procedure createTrainingSamples(dstSamplesFilename: PAnsiChar; sampleImage: TIEVisionImage;
                                    backgroundImageList: TIEVisionImageList; samplesCount: int32_t; bgColor: int32_t; bgThreshold: int32_t = 80; invert: TIEVisionHaarInvert = ievNOINVERT;
                                    maxIntensityDeviation: int32_t = 40; maxXAngle: double = 1.1; maxYAngle: double = 1.1; maxZAngle: double = 0.5;
                                    width: int32_t = 24; height: int32_t = 24; wantExceptions: bool32 = false); overload; safecall;
    procedure createTrainingSamples(dstSamplesFilename: PAnsiChar; srcImageList: TIEVisionImageList; width: int32_t = 24; height: int32_t = 24; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionHaarTraining.trainCascadeClassifier

<FM>Declaration<FC>
procedure trainCascadeClassifier(dstFilename: PAnsiChar; srcSamplesFilename: PAnsiChar; backgroundImageList: <A TIEVisionImageList>; treeCascade: bool32 = true; stagesCount: int32_t = 14; numPrecalculated: int32_t = 200; splitsCount: int32_t = 1; minHitRate: double = 0.995; maxFalseAlarm: double = 0.5; weightFraction: double = 0.95; haarMode: <A TIEVisionHaarMode> = ievBASIC; verticalSymmetry: bool32 = false; equalWeights: bool32 = true; width: int32_t = 24; height: int32_t = 24; boostType: <A TIEVisionHaarBoostType> = ievGENTLE_ADABOOST; stumpError: <A TIEVisionHaarStumpError> = ievMISCLASSIFICATION_ERROR; maxTreeSplits: int32_t = 0; minPos: int32_t = 500); safecall;


<FM>Description<FN>
Trains an Haar cascade classifier with the specified training samples file (generated with <A TIEVisionHaarTraining.createTrainingSamples>) and saves the classifier to file.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dstFilename<FN></C> <C>Filename to store trained Haar Classifier.</C> </R>
<R> <C><FC>srcSamplesFilename<FN></C> <C>Training samples file.</C> </R>
<R> <C><FC>backgroundImageList<FN></C> <C>A set of negative samples.</C> </R>
<R> <C><FC>treeCascade<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>stagesCount<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>numPrecalculated<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>splitsCount<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>minHitRate<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>maxFalseAlarm<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>weightFraction<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>haarMode<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>verticalSymmetry<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>equalWeights<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>width<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>height<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>boostType<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>stumpError<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>maxTreeSplits<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>minPos<FN></C> <C>Undocumented.</C> </R>
</TABLE>
!!}
    procedure trainCascadeClassifier(dstFilename: PAnsiChar; srcSamplesFilename: PAnsiChar; backgroundImageList: TIEVisionImageList;
                                     treeCascade: bool32 = true; stagesCount: int32_t = 14; numPrecalculated: int32_t = 200;
                                     splitsCount: int32_t = 1; minHitRate: double = 0.995; maxFalseAlarm: double = 0.5; weightFraction: double = 0.95;
                                     haarMode: TIEVisionHaarMode = ievBASIC; verticalSymmetry: bool32 = false; equalWeights: bool32 = true;
                                     width: int32_t = 24; height: int32_t = 24; boostType: TIEVisionHaarBoostType = ievGENTLE_ADABOOST;
                                     stumpError: TIEVisionHaarStumpError = ievMISCLASSIFICATION_ERROR;
                                     maxTreeSplits: int32_t = 0; minPos: int32_t = 500; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionHaarTraining.createCascadeClassifier

<FM>Declaration<FC>
function createCascadeClassifier(sampleImage: <A TIEVisionImage>; backgroundImageList: <A TIEVisionImageList>; samplesCount: int32_t; treeCascade: bool32 = true; bgColor: int32_t = 0; bgThreshold: int32_t = 80; invert: <A TIEVisionHaarInvert> = ievNOINVERT; maxIntensityDeviation: int32_t = 40; maxXAngle: double = 1.1; maxYAngle: double = 1.1; maxZAngle: double = 0.5; width: int32_t = 24; height: int32_t = 24; stagesCount: int32_t = 14; numPrecalculated: int32_t = 200; splitsCount: int32_t = 1; minHitRate: double = 0.995; maxFalseAlarm: double = 0.5; weightFraction: double = 0.95; haarMode: <A TIEVisionHaarMode> = ievBASIC; verticalSymmetry: bool32 = false; equalWeights: bool32 = true; boostType: <A TIEVisionHaarBoostType> = ievGENTLE_ADABOOST; stumpError: <A TIEVisionHaarStumpError> = ievMISCLASSIFICATION_ERROR; maxTreeSplits: int32_t = 0; minPos: int32_t = 500): <A TIEVisionCascadeClassifier>; safecall;

<FM>Description<FN>
Trains an Haar cascade classifier with the specified sample (positive) image and background samples (negative) and returns a Cascade classifier object ready to reconize the trained object.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>sampleImage<FN></C> <C>One positive sample image.</C> </R>
<R> <C><FC>backgroundImageList<FN></C> <C>A list of negative sample images.</C> </R>
<R> <C><FC>samplesCount<FN></C> <C>Number of samples to generate.</C> </R>
<R> <C><FC>treeCascade<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>bgColor<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>bgThreshold<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>invert<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>maxIntensityDeviation<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>maxXAngle<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>maxYAngle<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>maxZAngle<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>width<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>height<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>stagesCount<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>numPrecalculated<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>splitsCount<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>minHitRate<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>maxFalseAlarm<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>weightFraction<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>haarMode<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>verticalSymmetry<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>equalWeights<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>boostType<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>stumpError<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>maxTreeSplits<FN></C> <C>Undocumented.</C> </R>
<R> <C><FC>minPos<FN></C> <C>Undocumented.</C> </R>
</TABLE>
!!}
    function createCascadeClassifier(sampleImage: TIEVisionImage; backgroundImageList: TIEVisionImageList;
                                     samplesCount: int32_t; treeCascade: bool32 = true; bgColor: int32_t = 0; bgThreshold: int32_t = 80;
                                     invert: TIEVisionHaarInvert = ievNOINVERT;
                                     maxIntensityDeviation: int32_t = 40; maxXAngle: double = 1.1; maxYAngle: double = 1.1;
                                     maxZAngle: double = 0.5; width: int32_t = 24; height: int32_t = 24; stagesCount: int32_t = 14;
                                     numPrecalculated: int32_t = 200; splitsCount: int32_t = 1; minHitRate: double = 0.995;
                                     maxFalseAlarm: double = 0.5; weightFraction: double = 0.95; haarMode: TIEVisionHaarMode = ievBASIC;
                                     verticalSymmetry: bool32 = false; equalWeights: bool32 = true;
                                     boostType: TIEVisionHaarBoostType = ievGENTLE_ADABOOST;
                                     stumpError: TIEVisionHaarStumpError = ievMISCLASSIFICATION_ERROR;
                                     maxTreeSplits: int32_t = 0; minPos: int32_t = 500; wantExceptions: bool32 = false): TIEVisionCascadeClassifier; safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionInputOutput

<FM>Declaration<FC>
TIEVisionInputOutput = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface contains a set of helper input/output functions.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionInputOutput.zcompress></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionInputOutput.zdecompress></C> </R>
</TABLE>
!!}
  TIEVisionInputOutput = interface(TIEVisionBase)

{!!
<FS>TIEVisionInputOutput.zcompress

<FM>Declaration<FC>
procedure zcompress(inputStream: <A TIEVisionStream>; outputStream: <A TIEVisionStream>; compressionLevel: int32_t = -1); safecall;

<FM>Description<FN>
Compresses an input stream to output stream using zlib.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>inputStream<FN></C> <C>Input stream to compress.</C> </R>
<R> <C><FC>outputStream<FN></C> <C>Output stream (compressed).</C> </R>
<R> <C><FC>compressionLevel<FN></C> <C>Optional compression level. See below:</C> </R>
</TABLE>

Compression levels:
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>-1<FN></C> <C>Default compression.</C> </R>
<R> <C><FC>0<FN></C> <C>No compression.</C> </R>
<R> <C><FC>1<FN></C> <C>Best speed.</C> </R>
<R> <C><FC>2..8<FN></C> <C>Compromises between speed and compression.</C> </R>
<R> <C><FC>9<FN></C> <C>Best compression.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionInputOutput.zdecompress>
!!}
    procedure zcompress(inputStream: TIEVisionStream; outputStream: TIEVisionStream; compressionLevel: int32_t = -1; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionInputOutput.zdecompress

<FM>Declaration<FC>
procedure zdecompress(inputStream: <A TIEVisionStream>; outputStream: <A TIEVisionStream>); safecall;

<FM>Description<FN>
Decompresses a zlib stream.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>inputStream<FN></C> <C>Input stream to decompress.</C> </R>
<R> <C><FC>outputStream<FN></C> <C>Resulting decompressed stream.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionInputOutput.zdecompress>
!!}
    procedure zdecompress(inputStream: TIEVisionStream; outputStream: TIEVisionStream; wantExceptions: bool32 = false); safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionOptimizerFunction

<FM>Declaration<FC>
TIEVisionOptimizerFunction = function(userData: pointer; variables: <A TIEVisionVectorDouble>): double; stdcall;

<FM>Description<FN>
Callback used in <A TIEVisionMath.optimizeWith_NelderMeadSimplexMethod> optimization function.
The optimization algorithm will try to minimize this function.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>userData<FN></C> <C>User data (maybe a pointer to an application object).</C> </R>
<R> <C><FC>variables<FN></C> <C>Variables of the function to minimize.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionMath.optimizeWith_NelderMeadSimplexMethod>

<FM>Demo<FN>
IEVision\TrackObjects_LowLevel
!!}
  TIEVisionOptimizerFunction = function(userData: pointer; variables: TIEVisionVectorDouble): double; stdcall; // must be stdcall


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionMath

<FM>Declaration<FC>
TIEVisionMath = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface contains a set of mathematical and logical operations with images and matrices.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.absDiff></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.add></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.boundingRect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.calcCovarMatrix></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.cartToPolar></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.contourArea></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.exp></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.genericMultiply></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.log></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.mahalonobisDistance></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.mul></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.opAnd></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.opDiv></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.opOr></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.optimizeWith_NelderMeadSimplexMethod></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.opXor></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.pow></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.rectContains></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.rectIntersect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.rectTestIntersect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.rectUnion></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.solve></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.solveCubic></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.sub></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.trace></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.transform></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionMath.transpose></C> </R>
</TABLE>
!!}
  TIEVisionMath = interface(TIEVisionBase)

{!!
<FS>TIEVisionMath.optimizeWith_NelderMeadSimplexMethod

<FM>Declaration<FC>
function optimizeWith_NelderMeadSimplexMethod(func: <A TIEVisionOptimizerFunction>; funcUserData: pointer; solutions: <A TIEVisionVectorDouble>; out optimalValue: double; length: double = 1.0; timeout: int32_t = 1000; eps: double = 1.0e-20): bool32; safecall;

<FM>Description<FN>
Optimizes (minimizes) the specified function using Nelder Mead Simplex method.
Returns true on success.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>func<FN></C> <C>Function to optimize (minimize).</C> </R>
<R> <C><FC>funcUserData<FN></C> <C>A user pointer sent to function to optimize.</C> </R>
<R> <C><FC>solutions<FN></C> <C>Initial veriables and output minimizing variables.</C> </R>
<R> <C><FC>optimalValue<FN></C> <C>Optimal value.</C> </R>
<R> <C><FC>length<FN></C> <C>Initial length of the simplex.</C> </R>
<R> <C><FC>timeout<FN></C> <C>Maximum number of iterations.</C> </R>
<R> <C><FC>eps<FN></C> <C>Small real number to test convergence.</C> </R>
</TABLE>

<FM>Demo<FN>
IEVision\TrackObjects_LowLevel

<FM>Example<FC>
// finds values (x0=vars[0] and x1=vars[1]) that minimze (x0-1)*(x0-1) + (x1-6)*(x1-6)
function optFunc(userData: pointer; vars: TIEVisionVectorDouble): double; stdcall;
var
  x0, x1: double;
begin
  x0 := vars.getDouble(0);
  x1 := vars.getDouble(1);
  result := (x0-1)*(x0-1) + (x1-6)*(x1-6);
end;

....
  // vector of input and output variables
  vec := IEVisionLib.createVectorDouble();
  // initial values
  vec.push_back(-1.2);
  vec.push_back(1.0);
  // find minimizing values
  if not IEVisionLib.createMath().optimizeWith_NelderMeadSimplexMethod(@optFunc, nil, vec, optValue, 1.0, 1000, 1e-20) then
  begin
    x0 := vec.getDouble(0); // should be "1"
    x1 := vec.getDouble(1); // should be "6"
    // ...and optValue shoud be "0"
  end;
....

!!}
    function optimizeWith_NelderMeadSimplexMethod(func: TIEVisionOptimizerFunction; funcUserData: pointer; solutions: TIEVisionVectorDouble; out optimalValue: double; length: double = 1.0; timeout: int32_t = 1000; eps: double = 1.0e-20; wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionMath.calcCovarMatrix

<FM>Declaration<FC>
procedure calcCovarMatrix(images: <A TIEVisionVectorImageRef>; covMat: <A TIEVisionImage>; avg: <A TIEVisionImage>; flags: <A TIEVisionCovarFlags>); safecall;

<FM>Description<FN>
Calculates covariant matrix of specified images (matrices).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>images<FN></C> <C>A list of images of the same size.</C> </R>
<R> <C><FC>covMat<FN></C> <C>Output covariant matrix.</C> </R>
<R> <C><FC>avg<FN></C> <C>Average of all input matrices.</C> </R>
<R> <C><FC>flags<FN></C> <C>Oprational flags.</C> </R>
</TABLE>
!!}
    procedure calcCovarMatrix(images: TIEVisionVectorImageRef; covMat: TIEVisionImage; avg: TIEVisionImage; flags: TIEVisionCovarFlags; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.cartToPolar

<FM>Declaration<FC>
procedure cartToPolar(x: <A TIEVisionImage>; y: <A TIEVisionImage>; magnitude: <A TIEVisionImage>; angle: <A TIEVisionImage>; angleInDegrees: bool32 = false); safecall;

<FM>Description<FN>
Calculates the magnitude and angle of 2D vectors.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>x<FN></C> <C>Array of x-coordinates. This must be a single-precision or double-precision floating-point array.</C> </R>
<R> <C><FC>y<FN></C> <C>Array of y-coordinates that must have the same size and same type as x.</C> </R>
<R> <C><FC>magnitude<FN></C> <C>Destination array of magnitudes of the same size and type as x .</C> </R>
<R> <C><FC>angle<FN></C> <C>Destination array of angles that has the same size and type as x. The angles are measured in radians (from 0 to 2*Pi) or in degrees (0 to 360 degrees).</C> </R>
<R> <C><FC>angleInDegrees<FN></C> <C>Flag indicating whether the angles are measured in radians, which is the default mode, or in degrees.</C> </R>
</TABLE>
!!}
    procedure cartToPolar(x: TIEVisionImage; y: TIEVisionImage; magnitude: TIEVisionImage; angle: TIEVisionImage; angleInDegrees: bool32 = false; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.genericMultiply

<FM>Declaration<FC>
procedure genericMultiply(src1: <A TIEVisionImage>; src2: <A TIEVisionImage>; alpha: double; src3: <A TIEVisionImage>; beta: double; dest: <A TIEVisionImage>; flags: <A TIEVisionGEMMFlags> = ievGEMM_NONE); safecall;

<FM>Description<FN>
Performs generalized matrix multiplication.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src1<FN></C> <C>First multiplied input matrix that should have 32 bit float point (1 channel), 64 bit float point (1 channel) , 32 bit float point (2 channels), or 64 bit float point (2 channels) type.</C> </R>
<R> <C><FC>src2<FN></C> <C>Second multiplied input matrix of the same type as src1 .</C> </R>
<R> <C><FC>alpha<FN></C> <C>Weight of the matrix product.</C> </R>
<R> <C><FC>src3<FN></C> <C>Third optional delta matrix added to the matrix product. It should have the same type as src1 and src2.</C> </R>
<R> <C><FC>beta<FN></C> <C>Weight of src3.</C> </R>
<R> <C><FC>dest<FN></C> <C>Destination matrix. It has the proper size and the same type as input matrices.</C> </R>
<R> <C><FC>flags<FN></C> <C>Operation flag.</C> </R>
</TABLE>
!!}
    procedure genericMultiply(src1: TIEVisionImage; src2: TIEVisionImage; alpha: double; src3: TIEVisionImage; beta: double; dest: TIEVisionImage; flags: TIEVisionGEMMFlags = ievGEMM_NONE; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.mahalonobisDistance

<FM>Declaration<FC>
function mahalonobisDistance(vec1: <A TIEVisionImage>; vec2: <A TIEVisionImage>; invCovMatrix: <A TIEVisionImage>): double; safecall;

<FM>Description<FN>
Calculates the Mahalanobis distance between two vectors.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>vec1<FN></C> <C>First 1D source vector.</C> </R>
<R> <C><FC>vec2<FN></C> <C>Second 1D source vector.</C> </R>
<R> <C><FC>invCovMatrix<FN></C> <C>Inverse covariance matrix.</C> </R>
</TABLE>
!!}
    function mahalonobisDistance(vec1: TIEVisionImage; vec2: TIEVisionImage; invCovMatrix: TIEVisionImage; wantExceptions: bool32 = false): double; safecall;

{!!
<FS>TIEVisionMath.solve

<FM>Declaration<FC>
function solve(source1: <A TIEVisionImage>; source2: <A TIEVisionImage>; dest: <A TIEVisionImage>; method: <A TIEVisionSolveMethod>): int32_t; safecall;

<FM>Description<FN>
Solves one or more linear systems or least-squares problems.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>source1<FN></C> <C>Input matrix on the left-hand side of the system.</C> </R>
<R> <C><FC>source2<FN></C> <C>Input matrix on the right-hand side of the system.</C> </R>
<R> <C><FC>dest<FN></C> <C>Output solution.</C> </R>
<R> <C><FC>method<FN></C> <C>Solution (matrix inversion) method.</C> </R>
</TABLE>
!!}
    function solve(source1: TIEVisionImage; source2: TIEVisionImage; dest: TIEVisionImage; method: TIEVisionSolveMethod; wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionMath.solveCubic

<FM>Declaration<FC>
procedure solveCubic(coeffs: <A TIEVisionImage>; roots: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Finds the real roots of a cubic equation.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>coeffs<FN></C> <C>Equation coefficients, an array of 3 or 4 elements.</C> </R>
<R> <C><FC>roots<FN></C> <C>Destination array of real roots that has 1 or 3 elements.</C> </R>
</TABLE>
!!}
    procedure solveCubic(coeffs: TIEVisionImage; roots: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.sub

<FM>Declaration<FC>
procedure sub(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure sub(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>); overload; safecall;
procedure sub(lhs: <A TIEVisionScalar>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure sub(lhs: <A TIEVisionScalar>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>); overload; safecall;
procedure sub(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure sub(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Subtracts left-hand side image (matrix) or scalar to the right-hand side image (matrix) or scalar and puts resulting image (matrix) in the specified destination.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lhs<FN></C> <C>Left-hand side image or scalar.</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image or scalar.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (or matrix).</C> </R>
<R> <C><FC>mask<FN></C> <C>Optional operation mask.</C> </R>
</TABLE>
!!}
    procedure sub(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure sub(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure sub(lhs: TIEVisionScalar; rhs: TIEVisionImage; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure sub(lhs: TIEVisionScalar; rhs: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure sub(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure sub(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionMath.add

<FM>Declaration<FC>
procedure add(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure add(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>); overload; safecall;
procedure add(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure add(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Adds left-hand side iamge (matrix) or scalar to the right-hand side image (matrix) or scalar and puts resulting image (matrix) in the specified destination.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lhs<FN></C> <C>Left-hand side image or scalar.</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image or scalar.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (or matrix).</C> </R>
<R> <C><FC>mask<FN></C> <C>Optional operation mask.</C> </R>
</TABLE>
!!}
    procedure add(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure add(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure add(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure add(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionMath.absDiff

<FM>Declaration<FC>
procedure absDiff(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>); overload; safecall;
procedure absDiff(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Calculates absolute difference of left-hand side image (matrix) and right-hand side image (matrix) or scalar and puts resulting image (matrix) in the specified destination.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lhs<FN></C> <C>Left-hand side image (matrix).</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image (matrix) or scalar.</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (or matrix).</C> </R>
</TABLE>
!!}
    procedure absDiff(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure absDiff(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionMath.opAnd

<FM>Declaration<FC>
procedure opAnd(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>); overload; safecall;
procedure opAnd(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure opAnd(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure opAnd(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Performs bitwise and of left-hand side image (matrix) and right-hand side image (matrix) or scalar and puts resulting image (matrix) in the specified destination.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lhs<FN></C> <C>Left-hand side image (matrix).</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image or scalar (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (or matrix).</C> </R>
<R> <C><FC>mask<FN></C> <C>Operation mask.</C> </R>
</TABLE>
!!}
    procedure opAnd(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opAnd(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opAnd(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opAnd(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionMath.opDiv

<FM>Declaration<FC>
procedure opDiv(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; scale: double = 1.0); safecall;

<FM>Description<FN>
Divides left-hand side image (matrix) with right-side image (matrix) and puts resulting image (matrix) in the specified destination.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lhs<FN></C> <C>Left-hand side image (matrix).</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (or matrix).</C> </R>
<R> <C><FC>scale<FN></C> <C>Optional scalar multiplier.</C> </R>
</TABLE>
!!}
    procedure opDiv(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; scale: double = 1.0; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.exp

<FM>Declaration<FC>
procedure exp(src: <A TIEVisionImage>; dest: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Calculates the exponent of every image element.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Source image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (matrix).</C> </R>
</TABLE>
!!}
    procedure exp(src: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.log

<FM>Declaration<FC>
procedure log(src: <A TIEVisionImage>; dest: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Calculates the natural logarithm of every image element.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Source image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (matrix).</C> </R>
</TABLE>

!!}
    procedure log(src: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.mul

<FM>Declaration<FC>
procedure mul(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; scale: double = 1.0); safecall;

<FM>Description<FN>
Calculates the per-element scaled product of two images.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lhs<FN></C> <C>Left-hand side image (matrix).</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (matrix).</C> </R>
<R> <C><FC>scale<FN></C> <C>Optional scalar multiplier.</C> </R>
</TABLE>
!!}
    procedure mul(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; scale: double = 1.0; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.opOr

<FM>Declaration<FC>
procedure opOr(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure opOr(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>); overload; safecall;
procedure opOr(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure opOr(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Calculates the per-element bit-wise disjunction of two images or an image and a scalar.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lhs<FN></C> <C>Left-hand side image (matrix).</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (matrix).</C> </R>
<R> <C><FC>mask<FN></C> <C>Optional operation mask.</C> </R>
</TABLE>
!!}
    procedure opOr(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opOr(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opOr(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opOr(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionMath.pow

<FM>Declaration<FC>
procedure pow(src: <A TIEVisionImage>; dest: <A TIEVisionImage>; power: double); safecall;

<FM>Description<FN>
Raises every image element to a power.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Source image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (matrix).</C> </R>
<R> <C><FC>power<FN></C> <C>Exponent of power.</C> </R>
</TABLE>
!!}
    procedure pow(src: TIEVisionImage; dest: TIEVisionImage; power: double; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.trace

<FM>Declaration<FC>
function trace(src: <A TIEVisionImage>): <A TIEVisionScalar>; safecall;

<FM>Description<FN>
Returns the trace of a matrix.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Source image.</C> </R>
</TABLE>
!!}
    function trace(src: TIEVisionImage; wantExceptions: bool32 = false): TIEVisionScalar; safecall;

{!!
<FS>TIEVisionMath.transform

<FM>Declaration<FC>
procedure transform(src: <A TIEVisionImage>; dest: <A TIEVisionImage>; transfMatrix: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Performs the matrix transformation of every image element.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Source image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (matrix).</C> </R>
<R> <C><FC>transfMatrix<FN></C> <C>Transformation 2x2 or 2x3 floating-point matrix.</C> </R>
</TABLE>
!!}
    procedure transform(src: TIEVisionImage; dest: TIEVisionImage; transfMatrix: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionMath.transpose

<FM>Declaration<FC>
procedure transpose(src: <A TIEVisionImage>; dest: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Transposes a matrix.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Source image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (matrix).</C> </R>
</TABLE>
!!}
    procedure transpose(src: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionMath.opXor

<FM>Declaration<FC>
procedure opXor(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure opXor(lhs: <A TIEVisionImage>; rhs: <A TIEVisionImage>; dest: <A TIEVisionImage>); overload; safecall;
procedure opXor(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>; mask: <A TIEVisionImage>); overload; safecall;
procedure opXor(lhs: <A TIEVisionImage>; rhs: <A TIEVisionScalar>; dest: <A TIEVisionImage>); overload; safecall;

<FM>Description<FN>
Calculates the per-element bit-wise "exclusive or" operation on two images (matrix) or an image (matrix) and a scalar.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>lhs<FN></C> <C>Left-hand side image (matrix).</C> </R>
<R> <C><FC>rhs<FN></C> <C>Right-hand side image (matrix).</C> </R>
<R> <C><FC>dest<FN></C> <C>Container for the resulting image (matrix).</C> </R>
<R> <C><FC>mask<FN></C> <C>Operation mask.</C> </R>
</TABLE>
!!}
    procedure opXor(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opXor(lhs: TIEVisionImage; rhs: TIEVisionImage; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opXor(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; mask: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;
    procedure opXor(lhs: TIEVisionImage; rhs: TIEVisionScalar; dest: TIEVisionImage; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionMath.boundingRect

<FM>Declaration<FC>
function boundingRect(points: <A TIEVisionVectorPoint>): <A TIEVisionRect>; safecall;

<FM>Description<FN>
Calculates the up-right bounding rectangle of a point set.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>points<FN></C> <C>Point set.</C> </R>
</TABLE>
!!}
    function boundingRect(points: TIEVisionVectorPoint; wantExceptions: bool32 = false): TIEVisionRect; safecall;

{!!
<FS>TIEVisionMath.contourArea

<FM>Declaration<FC>
function contourArea(points: <A TIEVisionVectorPoint>): double; safecall;

<FM>Description<FN>
Calculates a contour area.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>points<FN></C> <C>Contour vertices.</C> </R>
</TABLE>
!!}
    function contourArea(points: TIEVisionVectorPoint; wantExceptions: bool32 = false): double; safecall;

{!!
<FS>TIEVisionMath.rectIntersect

<FM>Declaration<FC>
function rectIntersect(const rect1: <A TIEVisionRect>; const rect2: <A TIEVisionRect>): <A TIEVisionRect>; safecall;

<FM>Description<FN>
Returns the intersection of two rectangles.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rect1<FN></C> <C>First rectangle.</C> </R>
<R> <C><FC>rect2<FN></C> <C>Second rectangle.</C> </R>
</TABLE>
!!}
    function rectIntersect(const rect1: TIEVisionRect; const rect2: TIEVisionRect; wantExceptions: bool32 = false): TIEVisionRect; safecall;

{!!
<FS>TIEVisionMath.rectTestIntersect

<FM>Declaration<FC>
function rectTestIntersect(const rect1: <A TIEVisionRect>; const rect2: <A TIEVisionRect>): bool32; safecall;

<FM>Description<FN>
Tests if two rectangles intersect.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rect1<FN></C> <C>First rectangle.</C> </R>
<R> <C><FC>rect2<FN></C> <C>Second rectangle.</C> </R>
</TABLE>
!!}
    function rectTestIntersect(const rect1: TIEVisionRect; const rect2: TIEVisionRect; wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionMath.rectUnion

<FM>Declaration<FC>
function rectUnion(const rect1: <A TIEVisionRect>; const rect2: <A TIEVisionRect>): <A TIEVisionRect>; safecall;

<FM>Description<FN>
Returns the union of two rectangles.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rect1<FN></C> <C>First rectangle.</C> </R>
<R> <C><FC>rect2<FN></C> <C>Second rectangle.</C> </R>
</TABLE>
!!}
    function rectUnion(const rect1: TIEVisionRect; const rect2: TIEVisionRect; wantExceptions: bool32 = false): TIEVisionRect; safecall;

{!!
<FS>TIEVisionMath.rectContains

<FM>Declaration<FC>
function rectContains(const rect: <A TIEVisionRect>; const point: <A TIEVisionPoint>): bool32; safecall;

<FM>Description<FN>
Tests if a point is inside a rectangle.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>rect<FN></C> <C>Rectangle.</C> </R>
<R> <C><FC>point<FN></C> <C>Point to test.</C> </R>
</TABLE>
!!}
    function rectContains(const rect: TIEVisionRect; const point: TIEVisionPoint; wantExceptions: bool32 = false): bool32; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionDrawing

<FM>Declaration<FC>
TIEVisionDrawing = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface contains a set of drawing primitives, to paint over a <A TIEVisionImage> objects.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionDrawing.circle></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionDrawing.drawContours></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionDrawing.ellipse></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionDrawing.rectangle></C> </R>
</TABLE>
!!}
  TIEVisionDrawing = interface(TIEVisionBase)

{!!
<FS>TIEVisionDrawing.rectangle

<FM>Declaration<FC>
procedure rectangle(image: <A TIEVisionImage>; pt1: <A TIEVisionPoint>; pt2: <A TIEVisionPoint>; const color: <A TIEVisionScalar>; thickness: int32_t = 1; linetype: int32_t = 8; shift: int32_t = 0); overload; safecall;
procedure rectangle(image: <A TIEVisionImage>; const rect: <A TIEVisionRect>; const color: <A TIEVisionScalar>; thickness: int32_t = 1; linetype: int32_t = 8; shift: int32_t = 0); overload; safecall;

<FM>Description<FN>
Draws a rectangle.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Destination image.</C> </R>
<R> <C><FC>pt1<FN></C> <C>Top-left rectangle point.</C> </R>
<R> <C><FC>pt2<FN></C> <C>Bottom-right rectangle point.</C> </R>
<R> <C><FC>color<FN></C> <C>Rectangle color.</C> </R>
<R> <C><FC>thickness<FN></C> <C>Line width.</C> </R>
<R> <C><FC>linetype<FN></C> <C>Line type.</C> </R>
<R> <C><FC>shift<FN></C> <C>Thickness of lines that make up the rectangle. Negative values mean that the function has to draw a filled rectangle.</C> </R>
<R> <C><FC>rect<FN></C> <C>Number of fractional bits in the point coordinates.</C> </R>
</TABLE>
!!}
    procedure rectangle(image: TIEVisionImage; pt1: TIEVisionPoint; pt2: TIEVisionPoint; const color: TIEVisionScalar; thickness: int32_t = 1; linetype: int32_t = 8; shift: int32_t = 0; wantExceptions: bool32 = false); overload; safecall;
    procedure rectangle(image: TIEVisionImage; const rect: TIEVisionRect; const color: TIEVisionScalar; thickness: int32_t = 1; linetype: int32_t = 8; shift: int32_t = 0; wantExceptions: bool32 = false); overload; safecall;

{!!
<FS>TIEVisionDrawing.circle

<FM>Declaration<FC>
procedure circle(image: <A TIEVisionImage>; center: <A TIEVisionPoint>; radius: int32_t; color: <A TIEVisionScalar>; thickness: int32_t = 1; linetype: int32_t = 8; shift: int32_t = 0); safecall;

<FM>Description<FN>
Draw a circle.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Destination image.</C> </R>
<R> <C><FC>center<FN></C> <C>Center of the circle.</C> </R>
<R> <C><FC>radius<FN></C> <C>Radius of the circle.</C> </R>
<R> <C><FC>color<FN></C> <C>Rectangle color.</C> </R>
<R> <C><FC>thickness<FN></C> <C>Line width.</C> </R>
<R> <C><FC>linetype<FN></C> <C>Line type.</C> </R>
<R> <C><FC>shift<FN></C> <C>Thickness of lines that make up the rectangle. Negative values mean that the function has to draw a filled circle.</C> </R>
</TABLE>
!!}
    procedure circle(image: TIEVisionImage; center: TIEVisionPoint; radius: int32_t; color: TIEVisionScalar; thickness: int32_t = 1; linetype: int32_t = 8; shift: int32_t = 0; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionDrawing.ellipse

<FM>Declaration<FC>
procedure ellipse(image: <A TIEVisionImage>; center: <A TIEVisionPoint>; axes: <A TIEVisionSize>; angle: double; startAngle: double; endAngle: double; color: <A TIEVisionScalar>; thickness: int32_t = 1; linetype: int32_t = 8; shift: int32_t = 0); safecall;

<FM>Description<FN>
Draws an ellipse.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Destination image.</C> </R>
<R> <C><FC>center<FN></C> <C>Center of the ellipse.</C> </R>
<R> <C><FC>axes<FN></C> <C>Length of the ellipse axes.</C> </R>
<R> <C><FC>angle<FN></C> <C>Ellipse rotation angle in degrees.</C> </R>
<R> <C><FC>startAngle<FN></C> <C>Starting angle of the elliptic arc in degrees.</C> </R>
<R> <C><FC>endAngle<FN></C> <C>Ending angle of the elliptic arc in degrees.</C> </R>
<R> <C><FC>color<FN></C> <C>Rectangle color.</C> </R>
<R> <C><FC>thickness<FN></C> <C>Line width.</C> </R>
<R> <C><FC>linetype<FN></C> <C>Line type.</C> </R>
<R> <C><FC>shift<FN></C> <C>Thickness of lines that make up the rectangle. Negative values mean that the function has to draw a filled ellipse.</C> </R>
</TABLE>
!!}
    procedure ellipse(image: TIEVisionImage; center: TIEVisionPoint; axes: TIEVisionSize; angle: double; startAngle: double; endAngle: double; color: TIEVisionScalar; thickness: int32_t = 1; linetype: int32_t = 8; shift: int32_t = 0; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionDrawing.drawContours

<FM>Declaration<FC>
procedure drawContours(image: <A TIEVisionImage>; contours: <A TIEVisionVectorObjRef>; const color: <A TIEVisionScalar>; contourIdx: int32_t = -1; thickness: int32_t = 1; linetype: int32_t = 8); safecall;

<FM>Description<FN>
Draws a contour.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Destination image.</C> </R>
<R> <C><FC>contours<FN></C> <C>A vector of vector of points. (TIEVisionVectorObjRef must contain <A TIEVisionVectorPoint> objects).</C> </R>
<R> <C><FC>color<FN></C> <C>Rectangle color.</C> </R>
<R> <C><FC>thickness<FN></C> <C>Line width.</C> </R>
<R> <C><FC>linetype<FN></C> <C>Line type.</C> </R>
<R> <C><FC>shift<FN></C> <C>Thickness of lines that make up the rectangle. Negative values mean that the function has to draw a filled polygon.</C> </R>
</TABLE>
!!}
    procedure drawContours(image: TIEVisionImage; contours: TIEVisionVectorObjRef; const color: TIEVisionScalar; contourIdx: int32_t = -1; thickness: int32_t = 1; linetype: int32_t = 8; wantExceptions: bool32 = false); safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionObjectTracker

<FM>Declaration<FC>
TIEVisionObjectTracker = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to track an objected given its initial position.
Tracking is performed using Camshift algorithm.

<FM>Demo<FN>
IEVision\TrackObjects

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectTracker.locateNewPosition></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectTracker.setInitialPosition></C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionLibrary.createObjectTracker>
!!}
  TIEVisionObjectTracker = interface(TIEVisionBase)

{!!
<FS>TIEVisionObjectTracker.setInitialPosition

<FM>Declaration<FC>
procedure setInitialPosition(image: <A TIEVisionImage>; const window: <A TIEVisionRect>); safecall;

<FM>Description<FN>
Sets the initial object position.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Source image (where the object stays).</C> </R>
<R> <C><FC>window<FN></C> <C>Coordinates of rectangle containing the object.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionObjectTracker.locateNewPosition>

<FM>Demo<FN>
IEVision\TrackObjects
!!}
    procedure setInitialPosition(image: TIEVisionImage; const window: TIEVisionRect; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionObjectTracker.locateNewPosition

<FM>Declaration<FC>
function locateNewPosition(image: <A TIEVisionImage>; out backProjectedImage: <A TIEVisionImage>): <A TIEVisionRotatedRect>; safecall;

<FM>Description<FN>
Locates the new position of the object. Before call locateNewPosition you must call <A TIEVisionObjectTracker.setInitialPosition>.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Source image.</C> </R>
<R> <C><FC>backProjectedImage<FN></C> <C>Image resulting of backprojection of object histogram.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionObjectTracker.setInitialPosition>

<FM>Demo<FN>
IEVision\TrackObjects
!!}
    function locateNewPosition(image: TIEVisionImage; out backProjectedImage: TIEVisionImage; wantExceptions: bool32 = false): TIEVisionRotatedRect; safecall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionObjectsFinder

<FM>Declaration<FC>
TIEVisionObjectsFinder = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to find an object or a set of different objects.
It is possible to load more than one classifier (object detector). Each classifier is executed on a different thread.

<FM>Demo<FN>
IEVision\FaceDetection
IEVision\GetFaces

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.addClassifier></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.classifierExists></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.findIn></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.getClassifier></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.getFoundRect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.getFoundRectCount></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.mergeAllRects></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.mergeRects></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.removeAllClassifiers></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.removeClassifier></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.setDivisor></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.setHaarFlags></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.setHaarMinNeighbors></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionObjectsFinder.setHaarScaleFactor></C> </R>
</TABLE>

<FM>See Also<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createObjectsFinder></C> </R>
<R> <C_IMG_CLASS> <C><A IEVision Embedded Classifiers></C> </R>
</TABLE>

<FM>Example<FC>
var
  objectsFinder: TIEVisionObjectsFinder;
  rects: TIEVisionVectorRect;
...
// load two face detectors
objectsFinder := IEVisionLib.createObjectsFinder();
objectsFinder.addClassifier('face detector 1', IEVisionLib.createCascadeClassifier(IEVC_FRONTAL_FACE_ALT_TREE));
objectsFinder.addClassifier('face detector 2', IEVisionLib.createCascadeClassifier(IEVC_FRONTAL_FACE_DEFAULT));

// detect objects
objectsFinder.findIn(ImageEnView1.IEBitmap.GetIEVisionImage());

// merge intersecting rectangles of all searched objects
rects := objectsFinder.mergeAllRects();

// loop among rectangles
for i := 0 to rects.size-1 do
begin
  ImageEnView1.ObjPenWidth[-1] := 2;
  with rects.getRect(i) do
  begin
    ... do something with the rectangle coordinates and size
  end;
end;

!!}
  TIEVisionObjectsFinder = interface(TIEVisionBase)

{!!
<FS>TIEVisionObjectsFinder.addClassifier

<FM>Declaration<FC>
procedure addClassifier(name: PAnsiChar; classifier: <A TIEVisionBase>); safecall;

<FM>Description<FN>
Adds a new classifier (object detector) to the classifiers set.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>name<FN></C> <C>Name of the classifier. This name is used only for the user convenience.</C> </R>
<R> <C><FC>classifier<FN></C> <C>Actual object detector.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionObjectsFinder.removeClassifier>
- <A IEVision Embedded Classifiers>

<FM>Example<FC>
// load two face detectors
objectsFinder := IEVisionLib.createObjectsFinder();
objectsFinder.addClassifier('face detector 1', IEVisionLib.createCascadeClassifier(IEVC_FRONTAL_FACE_ALT_TREE);
objectsFinder.addClassifier('face detector 2', IEVisionLib.createCascadeClassifier(IEVC_FRONTAL_FACE_DEFAULT));

!!}
    procedure addClassifier(name: PAnsiChar; classifier: TIEVisionBase; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionObjectsFinder.getClassifier

<FM>Declaration<FC>
function getClassifier(name: PAnsiChar): <A TIEVisionBase>; safecall;

<FM>Description<FN>
Returns the classifier added with this name.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>name<FN></C> <C>Classifier name to find.</C> </R>
</TABLE>
!!}
    function getClassifier(name: PAnsiChar; wantExceptions: bool32 = false): TIEVisionBase; safecall;

{!!
<FS>TIEVisionObjectsFinder.findIn

<FM>Declaration<FC>
procedure findIn(image: <A TIEVisionImage>); safecall;

<FM>Description<FN>
Loops among all added classifiers to find objects inside the specified image.
Objects found are returned by <A TIEVisionObjectsFinder.getFoundRectCount> and <A TIEVisionObjectsFinder.getFoundRect>.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Image where to find the objects.</C> </R>
</TABLE>


<FM>Example<FC>
// detect objects
objectsFinder.findIn(ImageEnView1.IEBitmap.GetIEVisionImage());
!!}
    procedure findIn(image: TIEVisionImage; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionObjectsFinder.getFoundRectCount

<FM>Declaration<FC>
function getFoundRectCount(name: PAnsiChar): int32_t; safecall;

<FM>Description<FN>
Returns number of found objects for the specified object detector.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>name<FN></C> <C>Object detector.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionObjectsFinder.findIn>
- <A TIEVisionObjectsFinder.getFoundRect>
- <A TIEVisionObjectsFinder.mergeRects>
- <A TIEVisionObjectsFinder.mergeAllRects>

<FM>Example<FC>
objectsFinder.findIn(image);
foundRects := objectsFinder.getFoundRectCount('face detector 1');
!!}
    function getFoundRectCount(name: PAnsiChar; wantExceptions: bool32 = false): int32_t; safecall;

{!!
<FS>TIEVisionObjectsFinder.getFoundRect

<FM>Declaration<FC>
function getFoundRect(name: PAnsiChar; index: int32_t): <A TIEVisionRect>; safecall;

<FM>Description<FN>
Returns the nth object found for the specified object detector.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>name<FN></C> <C>Name of object detector.</C> </R>
<R> <C><FC>index<FN></C> <C>Index of found rectangle.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionObjectsFinder.findIn>
- <A TIEVisionObjectsFinder.getFoundRectCount>
- <A TIEVisionObjectsFinder.mergeRects>
- <A TIEVisionObjectsFinder.mergeAllRects>

<FM>Example<FC>
objectsFinder.findIn(image);
foundRects := objectsFinder.getFoundRectCount('face detector 1');
for i := 0 to foundRects-1 do
  with GetFoundRect('face detector 1', i) do
  begin
    // do something with x, y and width, height
  end;
!!}
    function getFoundRect(name: PAnsiChar; index: int32_t; wantExceptions: bool32 = false): TIEVisionRect; safecall;

{!!
<FS>TIEVisionObjectsFinder.mergeRects

<FM>Declaration<FC>
function mergeRects(name1: PAnsiChar; name2: PAnsiChar): <A TIEVisionVectorRect>; safecall;

<FM>Description<FN>
Merges rectangles that intersect of the specified objects detectors.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>name1<FN></C> <C>Name of an object detector.</C> </R>
<R> <C><FC>name2<FN></C> <C>Name of an object detector.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionObjectsFinder.findIn>
- <A TIEVisionObjectsFinder.getFoundRect>
- <A TIEVisionObjectsFinder.getFoundRectCount>
- <A TIEVisionObjectsFinder.mergeAllRects>

<FM>Example<FC>
// detect objects
objectsFinder.findIn(image);

// merge intersecting rectangles of "face1" and "face2" detectors
rects := objectsFinder.mergeRects("face1", "face2");

// loop among rectangles
for i := 0 to rects.size-1 do
begin
  ImageEnView1.ObjPenWidth[-1] := 2;
  with rects.getRect(i) do
  begin
    ... do something with the rectangle coordinates and size
  end;
end;
!!}
    function mergeRects(name1: PAnsiChar; name2: PAnsiChar; wantExceptions: bool32 = false): TIEVisionVectorRect; safecall;

{!!
<FS>TIEVisionObjectsFinder.mergeAllRects

<FM>Declaration<FC>
function mergeAllRects(): <A TIEVisionVectorRect>; safecall;

<FM>Description<FN>
Merges rectangles that intersect of all object detectors.

<FM>See Also<FN>
- <A TIEVisionObjectsFinder.findIn>
- <A TIEVisionObjectsFinder.getFoundRect>
- <A TIEVisionObjectsFinder.getFoundRectCount>
- <A TIEVisionObjectsFinder.mergeRects>

<FM>Example<FC>
// detect objects
objectsFinder.findIn(image);

// merge intersecting rectangles
rects := objectsFinder.mergeAllRects();

// loop among rectangles
for i := 0 to rects.size-1 do
begin
  ImageEnView1.ObjPenWidth[-1] := 2;
  with rects.getRect(i) do
  begin
    ... do something with the rectangle coordinates and size
  end;
end;
!!}
    function mergeAllRects(wantExceptions: bool32 = false): TIEVisionVectorRect; safecall;

{!!
<FS>TIEVisionObjectsFinder.removeAllClassifiers

<FM>Declaration<FC>
procedure removeAllClassifiers(); safecall;

<FM>Description<FN>
Removes all added classifiers.

<FM>See Also<FN>
- <A TIEVisionObjectsFinder.addClassifier>
- <A TIEVisionObjectsFinder.removeClassifier>
!!}
    procedure removeAllClassifiers(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionObjectsFinder.classifierExists

<FM>Declaration<FC>
function classifierExists(name: PAnsiChar): bool32; safecall;

<FM>Description<FN>
Tests if a classifier has been loaded.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>name<FN></C> <C>Name of classifier to test presence.</C> </R>
</TABLE>
!!}
    function classifierExists(name: PAnsiChar; wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionObjectsFinder.removeClassifier

<FM>Declaration<FC>
procedure removeClassifier(name: PAnsiChar); safecall;

<FM>Description<FN>
Removes the specified classifier.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>name<FN></C> <C>Classifier to remove.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEVisionObjectsFinder.addClassifier>
!!}
    procedure removeClassifier(name: PAnsiChar; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionObjectsFinder.setDivisor

<FM>Declaration<FC>
procedure setDivisor(value: int32_t); safecall;

<FM>Description<FN>
Sets the divisor value. A temporary image is created subsampling the source image by the divisor factor.
Object detector will look for the object inside the temporary image, instead of full image.
Default value is 3 (divide source image by 3). For fine (but slow) detection sets to 1.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Divisor value.</C> </R>
</TABLE>


<FM>Example<FC>
objectfinder.setDivisor(1);
objectsFinder.findIn(image);
!!}
    procedure setDivisor(value: int32_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionObjectsFinder.setHaarScaleFactor

<FM>Declaration<FC>
procedure setHaarScaleFactor(value: double); safecall;

<FM>Description<FN>
Scale factor specifies how much the image size is reduced at each image scale.
Default value is 1.1. This setting is specific of Haar Classifier.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Scale factor value.</C> </R>
</TABLE>
!!}
    procedure setHaarScaleFactor(value: double; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionObjectsFinder.setHaarMinNeighbors

<FM>Declaration<FC>
procedure setHaarMinNeighbors(value: int32_t); safecall;

<FM>Description<FN>
Haar minimum neighbors specifies how many neighbors each candiate rectangle should have to retain it.
Default value is 3. This setting is specific of Haar Classifier.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Minimum neighbors value.</C> </R>
</TABLE>


<FM>Example<FC>
objectsFinder.setHaarMinNeighbors(4);
objectsFinder.findIn(image);
!!}
    procedure setHaarMinNeighbors(value: int32_t; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionObjectsFinder.setHaarFlags

<FM>Declaration<FC>
procedure setHaarFlags(value: <A TIEVisionHaarDetectObjectsFlags>); safecall;

<FM>Description<FN>
Specifies Haar classifier flags.
Default value is ievDO_CANNY_PRUNING. This setting is specific of Haar Classifier.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Haar flags.</C> </R>
</TABLE>
!!}
    procedure setHaarFlags(value: TIEVisionHaarDetectObjectsFlags; wantExceptions: bool32 = false); safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionOCR

<FM>Declaration<FC>
TIEVisionOCR = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to perform basic OCR (Optical Character Recognition) in English and <L TIEVisionLanguages>35 other languages</L>.
It is possible to recognize a specific image area or the whole image.
For recognition it means:
- gets the ANSI or Unicode text from raster image
- estimates the document orientation (text angle)
- gets each recognized character position (bounding box)
- gets text regions (to reproduce text layout)

To support OCR language specify the path of the language file when calling IEVisionLib.createOCR (several language files are available).

Only one single instance of this object can exist in your application.

<FM>Demo<FN>
IEVision\OCR
IEVision\OCRwithLayout

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionOCR.clearAdaptiveInfo></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionOCR.getBoxes></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionOCR.getRegions></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionOCR.getTextAngle></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionOCR.isWordValid></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionOCR.recognize></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionOCR.setSegmentationMode></C> </R>
</TABLE>      

<FM>See Also<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createOCR></C> </R>
<R> <C_IMG_CLASS> <C><A TIEVisionLanguages></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionLanguageCodeToName></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionLanguageNameToCode></C> </R>
<R> <C_IMG_GLOBMETHOD> <C><A IEVisionGetLanguagesInFolder></C> </R>
</TABLE>

<FM>Example<FC>
// perform OCR
str := IEVisionLib.createOCR(IEOCRLanguageList[OCR_English_language].Code).recognize(ImageEnView1.IEBitmap.GetIEVisionImage(), IEVisionRect(0, 0, 0, 0)).c_str();
!!}
  TIEVisionOCR = interface(TIEVisionBase)

{!!
<FS>TIEVisionOCR.recognize

<FM>Declaration<FC>
function recognize(image: <A TIEVisionImage>; const rect: <A TIEVisionRect>): <A TIEVisionWString>; safecall;

<FM>Description<FN>
Performs OCR on the specified area of the image returning an ANSI string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Source image.</C> </R>
<R> <C><FC>rect<FN></C> <C>Rectangle of interest. Setting (0, 0, 0, 0) means "entire image".</C> </R>
</TABLE>


<FM>Example 1<FC>

// perform OCR
OCR := IEVisionLib.createOCR(IEOCRLanguageList[OCR_English_language].Code);
str := OCR.recognize(ImageEnView1.IEBitmap.GetIEVisionImage(), IEVisionRect(0, 0, 0, 0)).c_str();

               
<FM>Example 2<FC>

Allow switching of language: 

// Set language based on selection
Case LanguageRadioGroup.ItemIndex of
  0 : OCR := IEVisionLib.createOCR(IEOCRLanguageList[OCR_English_language].Code);
  1 : OCR := IEVisionLib.createOCR(IEOCRLanguageList[OCR_French_language].Code);
  2 : OCR := IEVisionLib.createOCR(IEOCRLanguageList[OCR_German_language].Code);
End;

// Perform OCR
str := OCR.recognize(ImageEnView1.IEBitmap.GetIEVisionImage(), IEVisionRect(0, 0, 0, 0)).c_str();

// Reset OCR object
OCR := nil;

!!}
    function recognize(image: TIEVisionImage; const rect: TIEVisionRect; wantExceptions: bool32 = false): TIEVisionWString; safecall;

{!!
<FS>TIEVisionOCR.clearAdaptiveInfo

<FM>Declaration<FC>
procedure clearAdaptiveInfo(); safecall;

<FM>Description<FN>
Removes adaptive information. Call this method whenever a new page or document must be processed.


<FM>Example<FC>
// perform OCR
OCR := IEVisionLib.createOCR(IEOCRLanguageList[OCR_English_language].Code); // english language
// page 0
str_page0 := OCR.recognize(image_page0, IEVisionRect(0, 0, 0, 0));
// page 1
OCR.clearAdaptiveInfo();
str_page1 := OCR.recognize(image_page1, IEVisionRect(0, 0, 0, 0));
// page 2
OCR.clearAdaptiveInfo();
str_page2 := OCR.recognize(image_page2, IEVisionRect(0, 0, 0, 0));
!!}
    procedure clearAdaptiveInfo(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionOCR.isWordValid

<FM>Declaration<FC>
function isWordValid(word: PAnsiChar): bool32; safecall;

<FM>Description<FN>
Tests if the specified word is valid for currently loaded language.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>word<FN></C> <C>Word to test.</C> </R>
</TABLE>
!!}
    function isWordValid(word: PAnsiChar; wantExceptions: bool32 = false): bool32; safecall;

{!!
<FS>TIEVisionOCR.getBoxes

<FM>Declaration<FC>
function getBoxes(): <A TIEVisionVectorOCRBox>; safecall;

<FM>Description<FN>
Returns a rectangle position and size for each recognized character.


<FM>Example<FC>
// Draw rectangles around recognized characters
procedure TMainForm.Button2Click(Sender: TObject);
var
  boxes: TIEVisionVectorOCRBox;
  i: integer;
begin
  ImageEnView1.IEBitmap.Canvas.Pen.Color := clRed;
  ImageEnView1.IEBitmap.Canvas.Brush.Style := bsClear;

  boxes := m_OCR.getBoxes();
  for i := 0 to boxes.size()-1 do
    with boxes.getOCRBox(i) do
      ImageEnView1.IEBitmap.Canvas.Rectangle(rect.x, rect.y, rect.x+rect.width, rect.y+rect.height);

  ImageEnView1.Update();
end;
!!}
    function getBoxes(wantExceptions: bool32 = false): TIEVisionVectorOCRBox; safecall;

{!!
<FS>TIEVisionOCR.getTextAngle

<FM>Declaration<FC>
function getTextAngle(): single; overload; safecall;
function getTextAngle(image: <A TIEVisionImage>): single; overload; safecall;

<FM>Description<FN>
Returns text angle (orientation) in radians.
First overload takes last processed image as input.
Second overload takes the specified image as input.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Source image.</C> </R>
</TABLE>


<FM>Example<FC>
// detect orientation
angle := OCR.getTextAngle(image) * 180 / PI;
ImageEnView1.IEBitmap.Origin := ieboBOTTOMLEFT;
ImageEnView1.Proc.Rotate(-angle);
!!}
    function getTextAngle(wantExceptions: bool32 = false): single; overload; safecall;
    function getTextAngle(image: TIEVisionImage; wantExceptions: bool32 = false): single; overload; safecall;

{!!
<FS>TIEVisionOCR.getRegions

<FM>Declaration<FC>
function getRegions(textOnly: bool32 = false): <A TIEVisionVectorRect>; overload; safecall;
function getRegions(image: <A TIEVisionImage>; textOnly: bool32 = false): <A TIEVisionVectorRect>; overload; safecall;

<FM>Description<FN>
Detects text regions (layout). In order to get document layout applications must set <A TIEVisionOCR.setSegmentationMode> to ievOCRAUTO.
First overload takes last processed image as input.
Second overload takes the specified image as input.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Source image.</C> </R>
<R> <C><FC>textOnly<FN></C> <C>If true then returns only text regions.</C> </R>
</TABLE>


<FM>Example<FC>
OCR.setSegmentationMode(ievOCRAUTO);
regions := OCR.getRegions(image);
for i := 0 to regions.size()-1 do
  with regions.getRect(i) do
  begin
    str := m_OCR.recognize(image, IEVisionRect(x, y, width, height));
  end;
!!}
    function getRegions(textOnly: bool32 = false; wantExceptions: bool32 = false): TIEVisionVectorRect; overload; safecall;
    function getRegions(image: TIEVisionImage; textOnly: bool32 = false; wantExceptions: bool32 = false): TIEVisionVectorRect; overload; safecall;

{!!
<FS>TIEVisionOCR.setSegmentationMode

<FM>Declaration<FC>
procedure setSegmentationMode(value: <A TIEVisionOCRPageSegmentationMode>); safecall;

<FM>Description<FN>
Specifies segmentation mode.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>value<FN></C> <C>Segmentation mode to set.</C> </R>
</TABLE>


<FM>Example<FC>
OCR.setSegmentationMode(ievOCRAUTO);
!!}
    procedure setSegmentationMode(value: TIEVisionOCRPageSegmentationMode; wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionOCR.getWordBoxes

<FM>Declaration<FC>
function getWordBoxes(): <A TIEVisionVectorObjRef>; safecall;

<FM>Description<FN>
Extracts words as Unicode text and their bounding boxes.
Returns a list of <A TIEVisionOCRWordBox> objects.

<FM>Example<FC>
var
  boxes: TIEVisionVectorObjRef;
  box: TIEVisionOCRWordBox;
  i: integer;
begin
  ImageEnView1.IEBitmap.Canvas.Pen.Color := clBlue;
  ImageEnView1.IEBitmap.Canvas.Brush.Style := bsClear;

  boxes := m_OCR.getWordBoxes();
  for i := 0 to boxes.size() - 1 do
  begin
    box := TIEVisionOCRWordBox( boxes.getObj(i) );
    with box.getBox() do
      ImageEnView1.IEBitmap.Canvas.Rectangle(x, y, x + width, y + height);
  end;

  ImageEnVect1.Update();
end;
!!}
    function getWordBoxes(wantExceptions: bool32 = false): TIEVisionVectorObjRef; safecall;
  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



{!!
<FS>TIEVisionPeopleDetector

<FM>Declaration<FC>
TIEVisionPeopleDetector = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to perform people (body) detection.

<FM>Demo<FN>
IEVision\GetPeople

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionPeopleDetector.detect></C> </R>
</TABLE>

<FM>See Also<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createPeopleDetector></C> </R>
</TABLE>

<FM>Example<FC>
detector := IEVisionLib.createPeopleDetector();
found_rectangles := detector.detect(ImageEnView1.IEBitmap.GetIEVisionImage());

for i := 0 to found_rectangles.size()-1 do
  with found_rectangles.getRect(i) do
  begin
    ImageEnView1.IEBitmap.Canvas.Brush.Style := bsClear;
    ImageEnView1.IEBitmap.Canvas.Pen.Color := clRed;
    ImageEnView1.IEBitmap.Canvas.Rectangle(x, y, x+width, y+height);
  end;
ImageEnView1.Update();
!!}
TIEVisionPeopleDetector = interface(TIEVisionBase)

{!!
<FS>TIEVisionPeopleDetector.detect

<FM>Declaration<FC>
function detect(image: <A TIEVisionImage>): <A TIEVisionVectorRect>; safecall;

<FM>Description<FN>
Finds all people's bodies in the specified image.
Returns a list of rectangles for each body found.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Image where to search for bodies.</C> </R>
</TABLE>
!!}
  function detect(image: TIEVisionImage; wantExceptions: bool32 = false): TIEVisionVectorRect; safecall;

end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionBarCodeSymbol

<FM>Declaration<FC>
TIEVisionBarCodeSymbol = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface represents a bar code detected symbol.

<FM>Demo<FN>
IEVision\BarCode
IEVision\BarCodeCam


<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionBarCodeSymbol.getBoundingBox></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBarCodeSymbol.getData></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBarCodeSymbol.getSymbolType></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionBarCodeSymbol.getXML></C> </R>
</TABLE>


<FM>Example<FC>
var
  symbols: TIEVisionVectorObjRef;
  s: TIEVisionBarCodeSymbol;
  i: integer;
begin
  symbols := IEVisionLib.createBarCodeScanner().scan(ImageEnView1.IEBitmap.GetIEVisionImage(), IEVisionRect(0, 0, 0, 0));
  for i := 0 to symbols.size() - 1 do
  begin
    s := TIEVisionBarCodeSymbol( symbols.getObj(i) );
    Memo1.Lines.Add('type = ' + s.getSymbolType().c_str());
    Memo1.Lines.Add('data = ' + s.getData().c_str());
    with s.getBoundingBox() do
      Memo1.Lines.Add('rect = ' + inttostr(x) + ' ' + inttostr(y) + ' ' + inttostr(width) + ' ' + inttostr(height));
  end;
end;
!!}
TIEVisionBarCodeSymbol = interface(TIEVisionBase)

{!!
<FS>TIEVisionBarCodeSymbol.getSymbolType

<FM>Declaration<FC>
function getSymbolType(wantExceptions: bool32 = false): <A TIEVisionWString>; safecall;

<FM>Description<FN>
Returns the bar code type. It can be one of the following strings:

<TABLE>
<R> <H>Bar code type string</H> </R>
<R> <C><FC>"EAN-8"<FN></C> </R>
<R> <C><FC>"UPC-E"<FN></C> </R>
<R> <C><FC>"ISBN-10"<FN></C> </R>
<R> <C><FC>"UPC-A"<FN></C> </R>
<R> <C><FC>"EAN-13"<FN></C> </R>
<R> <C><FC>"ISBN-13"<FN></C> </R>
<R> <C><FC>"I2/5"<FN></C> </R>
<R> <C><FC>"CODE-39"<FN></C> </R>
<R> <C><FC>"CODE-128"<FN></C> </R>
<R> <C><FC>"PDF417" (still not supported)<FN></C> </R>
<R> <C><FC>"QR-Code"<FN></C> </R>
</TABLE>
!!}
  function getSymbolType(wantExceptions: bool32 = false): TIEVisionWString; safecall;

{!!
<FS>TIEVisionBarCodeSymbol.getData

<FM>Declaration<FC>
function getData(wantExceptions: bool32 = false): <A TIEVisionWString>; safecall;

<FM>Description<FN>
Returns decoded bar code symbols.
!!}
  function getData(wantExceptions: bool32 = false): TIEVisionWString; safecall;

{!!
<FS>TIEVisionBarCodeSymbol.getBoundingBox

<FM>Declaration<FC>
function getBoundingBox(wantExceptions: bool32 = false): <A TIEVisionRect>; safecall;

<FM>Description<FN>
Returns decoded bar code bounding rectangle.
Bounding box could be not always available (i.e. for I2/5 bar code type).
!!}
  function getBoundingBox(wantExceptions: bool32 = false): TIEVisionRect; safecall;

{!!
<FS>TIEVisionBarCodeSymbol.getXML

<FM>Declaration<FC>
function getXML(wantExceptions: bool32 = false): <A TIEVisionWString>; safecall;

<FM>Description<FN>
Returns decoded bar code info as XML string.
!!}
  function getXML(wantExceptions: bool32 = false): TIEVisionWString; safecall;

end;




////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TIEVisionBarCodeScanner

<FM>Declaration<FC>
TIEVisionBarCodeScanner = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to read bar codes (EAN-13/UPC-A, UPC-E, EAN-8, Code 128, Code 39, Interleaved 2 of 5 and QR Code).

<FM>Demo<FN>
IEVision\BarCode
IEVision\BarCodeCam

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionBarCodeScanner.scan></C> </R>
</TABLE>

<FM>See Also<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createBarCodeScanner></C> </R>
</TABLE>

<FM>Example<FC>
var
  symbols: TIEVisionVectorObjRef;
  s: TIEVisionBarCodeSymbol;
  i: integer;
begin
  symbols := IEVisionLib.createBarCodeScanner().scan(ImageEnView1.IEBitmap.GetIEVisionImage(), IEVisionRect(0, 0, 0, 0));
  for i := 0 to symbols.size() - 1 do
  begin
    s := TIEVisionBarCodeSymbol( symbols.getObj(i) );
    Memo1.Lines.Add('type = ' + s.getSymbolType().c_str());
    Memo1.Lines.Add('data = ' + s.getData().c_str());
    with s.getBoundingBox() do
      Memo1.Lines.Add('rect = ' + inttostr(x) + ' ' + inttostr(y) + ' ' + inttostr(width) + ' ' + inttostr(height));
  end;
end;
!!}
TIEVisionBarCodeScanner = interface(TIEVisionBase)

{!!
<FS>TIEVisionBarCodeScanner.scan

<FM>Declaration<FC>
function scan(image: <A TIEVisionImage>; const rect: <A TIEVisionRect>; wantExceptions: bool32 = false): <A TIEVisionVectorObjRef>; safecall;

<FM>Description<FN>
Look for a scan code inside the specified image. Only 24 bit RGB and 8 bit gray scale images are supported.
Returns a list of symbols (bar codes) found as <A TIEVisionBarCodeSymbol> objects.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>image<FN></C> <C>Image where to search for barcodes.</C> </R>
<R> <C><FC>rect<FN></C> <C>Rectangle of the image where to search for. Setting (0, 0, 0, 0) means the whole image.</C> </R>
</TABLE>

<FM>Example<FC>
var
  symbols: TIEVisionVectorObjRef;
  s: TIEVisionBarCodeSymbol;
  i: integer;
begin
  symbols := IEVisionLib.createBarCodeScanner().scan(ImageEnView1.IEBitmap.GetIEVisionImage(), IEVisionRect(0, 0, 0, 0));
  for i := 0 to symbols.size() - 1 do
  begin
    s := TIEVisionBarCodeSymbol( symbols.getObj(i) );
    Memo1.Lines.Add('type = ' + s.getSymbolType().c_str());
    Memo1.Lines.Add('data = ' + s.getData().c_str());
    with s.getBoundingBox() do
      Memo1.Lines.Add('rect = ' + inttostr(x) + ' ' + inttostr(y) + ' ' + inttostr(width) + ' ' + inttostr(height));
  end;
end;
!!}
  function scan(image: TIEVisionImage; const rect: TIEVisionRect; wantExceptions: bool32 = false): TIEVisionVectorObjRef; safecall;

end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TIEVisionLibrary

<FM>Declaration<FC>
TIEVisionLibrary = interface(<A TIEVisionBase>)

<FM>Description<FN>
This interface allows you to create IEVision objects.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createBarCodeScanner></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createCascadeClassifier></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createDrawing></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createExistingMemoryStream></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createFileStream></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createHaarTraining></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createHistogram></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createInputOutput></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createMath></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createMemoryImageList></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createMemorySharedImageList></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createMemoryStream></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createObjectsFinder></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createObjectTracker></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createOCR></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createPeopleDetector></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createPropertyTree></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createString></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createWString></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createTempDirName></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createTempFileImageList></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createTempFileName></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createTempFileStream></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createVectorByte></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createVectorDouble></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createVectorFloatPair></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createVectorImageRef></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createVectorInt32></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createVectorPoint></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createVectorRect></C> </R>
<R> <C_IMG_METHOD> <C><A TIEVisionLibrary.createVectorString></C> </R>
</TABLE>

<FM>Example<FC>
image := IEVisionLib.createImage(1000, 1000, ievUINT8, 3);
!!}
  TIEVisionLibrary = interface(TIEVisionBase)
    procedure debug(wantExceptions: bool32 = false); safecall;

{!!
<FS>TIEVisionLibrary.createHaarTraining

<FM>Declaration<FC>
function createHaarTraining(): <A TIEVisionHaarTraining>; safecall;

<FM>Description<FN>
Creates a new Haar training object.
!!}
    function createHaarTraining(wantExceptions: bool32 = false): TIEVisionHaarTraining; safecall;

{!!
<FS>TIEVisionLibrary.createInputOutput

<FM>Declaration<FC>
function createInputOutput(): <A TIEVisionInputOutput>; safecall;

<FM>Description<FN>
Creates a new input/output object.
!!}
    function createInputOutput(wantExceptions: bool32 = false): TIEVisionInputOutput; safecall;

{!!
<FS>TIEVisionLibrary.createMath

<FM>Declaration<FC>
function createMath(): <A TIEVisionMath>; safecall;

<FM>Description<FN>
Creates a new math object.
!!}
    function createMath(wantExceptions: bool32 = false): TIEVisionMath; safecall;

{!!
<FS>TIEVisionLibrary.createDrawing

<FM>Declaration<FC>
function createDrawing(): <A TIEVisionDrawing>; safecall;

<FM>Description<FN>
Creates a new drawing object.
!!}
    function createDrawing(wantExceptions: bool32 = false): TIEVisionDrawing; safecall;

{!!
<FS>TIEVisionLibrary.createImage

<FM>Declaration<FC>
function createImage(width: int32_t; height: int32_t; channelFormat: <A TIEVisionChannelFormat>; channels: int32_t): <A TIEVisionImage>; overload; safecall;
function createImage(c: <A TIEVisionImage>): <A TIEVisionImage>; overload; safecall;
function createImage(): <A TIEVisionImage>; overload; safecall;
function createImage(width: int32_t; height: int32_t; channelFormat: <A TIEVisionChannelFormat>; channels: int32_t; rowlen: int32_t; data: pointer): <A TIEVisionImage>; overload; safecall;
function createImage(filename: PAnsiChar): <A TIEVisionImage>; overload; safecall;

<FM>Description<FN>
First overload creates a new image object from specified sizes and format.
Second overload creates a new image object from a copy of specified object.
Third overload creates a new empty image object.
Fourth overload creates a new image from the specified buffer (shares the same content). The buffer will be not released on destroy.
Fifth overload creates a new image from the specified file. Currently supported file formats are: TIFF, JPEG, BMP, PNG, PXM, J2K (jpeg2000), RAS.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>width<FN></C> <C>Image width.</C> </R>
<R> <C><FC>height<FN></C> <C>Image height.</C> </R>
<R> <C><FC>channelFormat<FN></C> <C>Channel format.</C> </R>
<R> <C><FC>channels<FN></C> <C>Number of channels.</C> </R>
<R> <C><FC>c<FN></C> <C>Source image to copy.</C> </R>
<R> <C><FC>rowlen<FN></C> <C>Row length in bytes.</C> </R>
<R> <C><FC>data<FN></C> <C>Raw data buffer.</C> </R>
<R> <C><FC>filename<FN></C> <C>Filename to load.</C> </R>
</TABLE>


<FM>Example<FC>
// create 1000x1000 RGB image
image := IEVisionLib.createImage(1000, 1000, ievUINT8, 3);

// load 'input.jpg'
image := IEVisionLib.createImage('input.jpeg');

// share ImageEnView1 bitmap
ImageEnView1.IEBitmap.Origin := ieboTOPLEFT;
image := IEVisionLib.createImage(ImageEnView1.IEBitmap.Width, ImageEnView1.IEBitmap.Height, 
                                 ievUINT8, 3, ImageEnView1.IEBitmap.Rowlen, 
                                 ImageEnView1.IEBitmap.ScanLine[0]);

// the same result of previous code
image := ImageEnView1.IEBitmap.GetIEVisionImage();
!!}
    function createImage(width: int32_t; height: int32_t; channelFormat: TIEVisionChannelFormat; channels: int32_t; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    function createImage(c: TIEVisionImage; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    function createImage(wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    function createImage(width: int32_t; height: int32_t; channelFormat: TIEVisionChannelFormat; channels: int32_t; rowlen: int32_t; data: pointer; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;
    function createImage(filename: PAnsiChar; wantExceptions: bool32 = false): TIEVisionImage; overload; safecall;

{!!
<FS>TIEVisionLibrary.createCascadeClassifier

<FM>Declaration<FC>
function createCascadeClassifier(): <A TIEVisionCascadeClassifier>; overload; safecall;
function createCascadeClassifier(c: <A TIEVisionCascadeClassifier>): <A TIEVisionCascadeClassifier>; overload; safecall;
function createCascadeClassifier(filename: PAnsiChar): <A TIEVisionCascadeClassifier>; overload; safecall;

<FM>Description<FN>
First overload creates a new empty cascade classifier.
Second overload creates a cascade classifier from a copy of specified object.
Third overload creates a cascade classifier from file or embedded classifier. Embedded classifiers have ':' prefix and can be: 
<TABLE>
<R> <H>Const</H> <H>Classifier</H> </R>
<R> <C><FC>IEVC_EYE  <FN></C> <C>":EYE"</C> 
<R> <C><FC>IEVC_EYE_TREE_EYE_GLASSES<FN></C> <C>":EYETREEEYEGLASSES"</C> 
<R> <C><FC>IEVC_FRONTAL_FACE_ALT    <FN></C> <C>":FRONTALFACEALT"</C> 
<R> <C><FC>IEVC_FRONTAL_FACE_ALT_2  <FN></C> <C>":FRONTALFACEALT2"</C> 
<R> <C><FC>IEVC_FRONTAL_FACE_ALT_TREE    <FN></C> <C>":FRONTALFACEALTTREE"</C> 
<R> <C><FC>IEVC_FRONTAL_FACE_DEFAULT<FN></C> <C>":FRONTALFACEDEFAULT"</C> 
<R> <C><FC>IEVC_FULL_BODY <FN></C> <C>":FULLBODY"</C> 
<R> <C><FC>IEVC_LOWER_BODY<FN></C> <C>":LOWERBODY"</C> 
<R> <C><FC>IEVC_PROFILE_FACE   <FN></C> <C>":PROFILEFACE"</C> 
<R> <C><FC>IEVC_UPPER_BODY<FN></C> <C>":UPPERBODY"</C> 
<R> <C><FC>IEVC_LEFT_EYE_2_SPLITS   <FN></C> <C>":LEFTEYE2SPLITS"</C> 
<R> <C><FC>IEVC_MCS_EYE_PAIR_BIG    <FN></C> <C>":MCSEYEPAIRBIG"</C> 
<R> <C><FC>IEVC_MCS_EYE_PAIR_SMALL  <FN></C> <C>":MCSEYEPAIRSMALL"</C> 
<R> <C><FC>IEVC_MCS_LEFT_EYE   <FN></C> <C>":MCSLEFTEYE"</C> 
<R> <C><FC>IEVC_MCS_MOUTH <FN></C> <C>":MCSMOUTH"</C> 
<R> <C><FC>IEVC_MCS_NOSE  <FN></C> <C>":MCSNOSE"</C>
<R> <C><FC>IEVC_MCS_RIGHT_EYE  <FN></C> <C>":MCSRIGHTEYE"</C> 
<R> <C><FC>IEVC_MCS_UPPER_BODY <FN></C> <C>":MCSUPPERBODY"</C> 
<R> <C><FC>IEVC_RIGHT_EYE_2_SPLITS  <FN></C> <C>":RIGHTEYE2SPLITS"</C>
<R> <C><FC>IEVC_MCS_LEFT_EAR   <FN></C> <C>":MCSLEFTEAR"</C> 
<R> <C><FC>IEVC_MCS_RIGHT_EAR  <FN></C> <C>":MCSRIGHTEAR"</C> 
</R>
</TABLE>

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>c<FN></C> <C>Source classifier to copy.</C> </R>
<R> <C><FC>filename<FN></C> <C>Filename of classifier to load (or one of the <L IEVision Embedded Classifiers>embedded classifiers</L>).</C> </R>
</TABLE>
                
<FM>Example<FC>
// load embedded eye detector
classifier := IEVisionLib.createCascadeClassifier(IEVC_EYE);

<FM>See Also<FN>
- <A IEVision Embedded Classifiers>
!!}
    function createCascadeClassifier(wantExceptions: bool32 = false): TIEVisionCascadeClassifier; overload; safecall;
    function createCascadeClassifier(c: TIEVisionCascadeClassifier; wantExceptions: bool32 = false): TIEVisionCascadeClassifier; overload; safecall;
    function createCascadeClassifier(filename: PAnsiChar; wantExceptions: bool32 = false): TIEVisionCascadeClassifier; overload; safecall;

{!!
<FS>TIEVisionLibrary.createFileStream

<FM>Declaration<FC>
function createFileStream(filename: PAnsiChar; mode: <A TIEVisionFileStreamMode>): <A TIEVisionFileStream>; overload; safecall;
function createFileStream(filename: PWideChar; mode: <A TIEVisionFileStreamMode>): <A TIEVisionFileStream>; overload; safecall;
function createFileStream(): <A TIEVisionFileStream>; overload; safecall;

<FM>Description<FN>
First two overloads create a file stream using specified mode.
Third overload create an unopened file stream.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>filename<FN></C> <C>ANSI or Unicode file name.</C> </R>
<R> <C><FC>mode<FN></C> <C>File mode.</C> </R>
</TABLE>


<FM>Example<FC>
// open "input.dat" as readonly file
filestream := createFileStream('input.dat', ievREAD);

// create "output.dat"
filestream := createFileStream('output.dat', ievCREATE);

// open "input.dat" as readonly file
filestream := createFileStream();
filestream.open('input.dat', ievREAD);
!!}
    function createFileStream(filename: PAnsiChar; mode: TIEVisionFileStreamMode; wantExceptions: bool32 = false): TIEVisionFileStream; overload; safecall;
    function createFileStream(filename: PWideChar; mode: TIEVisionFileStreamMode; wantExceptions: bool32 = false): TIEVisionFileStream; overload; safecall;
    function createFileStream(wantExceptions: bool32 = false): TIEVisionFileStream; overload; safecall;

{!!
<FS>TIEVisionLibrary.createMemoryStream

<FM>Declaration<FC>
function createMemoryStream(): <A TIEVisionMemoryStream>; safecall;

<FM>Description<FN>
Creates a memory stream.
!!}
    function createMemoryStream(wantExceptions: bool32 = false): TIEVisionMemoryStream; safecall;

{!!
<FS>TIEVisionLibrary.createExistingMemoryStream

<FM>Declaration<FC>
function createExistingMemoryStream(existingBuffer: pointer; existingBufferSize: int32_t): <A TIEVisionExistingMemoryStream>; safecall;

<FM>Description<FN>
Creates a readonly memory stream from an existing buffer.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>existingBuffer<FN></C> <C>Pointer to the existing buffer.</C> </R>
<R> <C><FC>existingBufferSize<FN></C> <C>Size of the existing buffer.</C> </R>
</TABLE>
!!}
    function createExistingMemoryStream(existingBuffer: pointer; existingBufferSize: int32_t; wantExceptions: bool32 = false): TIEVisionExistingMemoryStream; safecall;

{!!
<FS>TIEVisionLibrary.createTempFileStream

<FM>Declaration<FC>
function createTempFileStream(deleteOnDestroy: bool32 = true): <A TIEVisionTempFileStream>; overload; safecall;
function createTempFileStream(filename: PAnsiChar; deleteOnDestroy: bool32 = true): <A TIEVisionTempFileStream>; overload; safecall;

<FM>Description<FN>
Creates a temporary file stream.
First overload choices automatically the file name.
Second overload allows you to set a custom file name.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>deleteOnDestroy<FN></C> <C>If true the file will be delete on object disposing.</C> </R>
<R> <C><FC>filename<FN></C> <C>Filename of temporary file.</C> </R>
</TABLE>
!!}
    function createTempFileStream(deleteOnDestroy: bool32 = true; wantExceptions: bool32 = false): TIEVisionTempFileStream; overload; safecall;
    function createTempFileStream(filename: PAnsiChar; deleteOnDestroy: bool32 = true; wantExceptions: bool32 = false): TIEVisionTempFileStream; overload; safecall;

{!!
<FS>TIEVisionLibrary.createMemoryImageList

<FM>Declaration<FC>
function createMemoryImageList(): <A TIEVisionMemoryImageList>; safecall;

<FM>Description<FN>
Creates a memory image list.
!!}
    function createMemoryImageList(wantExceptions: bool32 = false): TIEVisionMemoryImageList; safecall;

{!!
<FS>TIEVisionLibrary.createMemorySharedImageList

<FM>Declaration<FC>
function createMemorySharedImageList(): <A TIEVisionMemorySharedImageList>; safecall;

<FM>Description<FN>
Creates a shared image list.
!!}
    function createMemorySharedImageList(wantExceptions: bool32 = false): TIEVisionMemorySharedImageList; safecall;

{!!
<FS>TIEVisionLibrary.createTempFileImageList

<FM>Declaration<FC>
function createTempFileImageList(): <A TIEVisionTempFileImageList>; safecall;

<FM>Description<FN>
Creates a temporary file image list.
!!}
    function createTempFileImageList(wantExceptions: bool32 = false): TIEVisionTempFileImageList; safecall;

{!!
<FS>TIEVisionLibrary.createString

<FM>Declaration<FC>
function createString(): <A TIEVisionString>; overload; safecall;
function createString(src: <A TIEVisionString>): <A TIEVisionString>; overload; safecall;
function createString(src: PAnsiChar): <A TIEVisionString>; overload; safecall;

<FM>Description<FN>
Creates an ANSI string object.
First overload creates an empty string.
Second overload creates a clone of source string object.
Third overload creates a string from the specified ANSI string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>String object or ANSI string pointer to copy.</C> </R>
</TABLE>
!!}
    function createString(wantExceptions: bool32 = false): TIEVisionString; overload; safecall;
    function createString(src: TIEVisionString; wantExceptions: bool32 = false): TIEVisionString; overload; safecall;
    function createString(src: PAnsiChar; wantExceptions: bool32 = false): TIEVisionString; overload; safecall;

{!!
<FS>TIEVisionLibrary.createWString

<FM>Declaration<FC>
function createWString(): <A TIEVisionWString>; overload; safecall;
function createWString(src: <A TIEVisionWString>): <A TIEVisionWString>; overload; safecall;
function createWString(src: PWideChar): <A TIEVisionWString>; overload; safecall;

<FM>Description<FN>
Creates a wide string object.
First overload creates an empty string.
Second overload creates a clone of source string object.
Third overload creates a string from the specified wide string.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>String object or wide string pointer to copy.</C> </R>
</TABLE>
!!}
    function createWString(wantExceptions: bool32 = false): TIEVisionWString; overload; safecall;
    function createWString(src: TIEVisionWString; wantExceptions: bool32 = false): TIEVisionWString; overload; safecall;
    function createWString(src: PWideChar; wantExceptions: bool32 = false): TIEVisionWString; overload; safecall;

{!!
<FS>TIEVisionLibrary.createVectorString

<FM>Declaration<FC>
function createVectorString(): <A TIEVisionVectorString>; overload; safecall;
function createVectorString(src: <A TIEVisionVectorString>): <A TIEVisionVectorString>; overload; safecall;

<FM>Description<FN>
Creates a vector of strings.
Second overload creates a clone of specified vector of strings.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Vector to clone.</C> </R>
</TABLE>
!!}
    function createVectorString(wantExceptions: bool32 = false): TIEVisionVectorString; overload; safecall;
    function createVectorString(src: TIEVisionVectorString; wantExceptions: bool32 = false): TIEVisionVectorString; overload; safecall;

{!!
<FS>TIEVisionLibrary.createPropertyTree

<FM>Declaration<FC>
function createPropertyTree(): <A TIEVisionPropertyTree>; overload; safecall;
function createPropertyTree(src: <A TIEVisionPropertyTree>): <A TIEVisionPropertyTree>; overload; safecall;

<FM>Description<FN>
Creates a property tree object.
Second overload creates a copy of specified property tree object.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Property tree to copy.</C> </R>
</TABLE>
!!}
    function createPropertyTree(wantExceptions: bool32 = false): TIEVisionPropertyTree; overload; safecall;
    function createPropertyTree(src: TIEVisionPropertyTree; wantExceptions: bool32 = false): TIEVisionPropertyTree; overload; safecall;

{!!
<FS>TIEVisionLibrary.createTempFileName

<FM>Declaration<FC>
function createTempFileName(deleteOnDestroy: bool32 = true): <A TIEVisionTempFileName>; overload; safecall;
function createTempFileName(filename: PAnsiChar; deleteOnDestroy: bool32 = true): <A TIEVisionTempFileName>; overload; safecall;

<FM>Description<FN>
Creates a temporary file name.
Second overload allows you to specify the file name.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>deleteOnDestroy<FN></C> <C>Automatically delete the file on object destroy.</C> </R>
<R> <C><FC>filename<FN></C> <C>Specify filename manually.</C> </R>
</TABLE>
!!}
    function createTempFileName(deleteOnDestroy: bool32 = true; wantExceptions: bool32 = false): TIEVisionTempFileName; overload; safecall;
    function createTempFileName(filename: PAnsiChar; deleteOnDestroy: bool32 = true; wantExceptions: bool32 = false): TIEVisionTempFileName; overload; safecall;

{!!
<FS>TIEVisionLibrary.createTempDirName

<FM>Declaration<FC>
function createTempDirName(deleteOnDestroy: bool32 = true): <A TIEVisionTempDirName>; overload; safecall;
function createTempDirName(dirname: PAnsiChar; deleteOnDestroy: bool32 = true): <A TIEVisionTempDirName>; overload; safecall;

<FM>Description<FN>
Creates a temporary directory name.
Second overload allows you to specify the directory name.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>deleteOnDestroy<FN></C> <C>Automatically delete the directory on object destroy.</C> </R>
<R> <C><FC>dirname<FN></C> <C>Specify dirname manually.</C> </R>
</TABLE>
!!}
    function createTempDirName(deleteOnDestroy: bool32 = true; wantExceptions: bool32 = false): TIEVisionTempDirName; overload; safecall;
    function createTempDirName(dirname: PAnsiChar; deleteOnDestroy: bool32 = true; wantExceptions: bool32 = false): TIEVisionTempDirName; overload; safecall;

{!!
<FS>TIEVisionLibrary.createHistogram

<FM>Declaration<FC>
function createHistogram(sizes: <A TIEVisionVectorInt32>; histType: <A TIEVisionHistogramType>; ranges: <A TIEVisionVectorFloatPair>; uniform: bool32 = true): <A TIEVisionHistogram>; overload; safecall;
function createHistogram(bins: int32_t = 256; histType: <A TIEVisionHistogramType> = ievARRAY; rangeMin: single = 0.0; rangeMax: single = 255.0; uniform: bool32 = true): <A TIEVisionHistogram>; overload; safecall;

<FM>Description<FN>
Creates an empty histogram object.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>sizes<FN></C> <C>Number of histogram sizes.</C> </R>
<R> <C><FC>histType<FN></C> <C>Histogram type.</C> </R>
<R> <C><FC>ranges<FN></C> <C>Vector of ranges (for each size).</C> </R>
<R> <C><FC>uniform<FN></C> <C>Uniform histogram.</C> </R>
<R> <C><FC>bins<FN></C> <C>Number of bins.</C> </R>
<R> <C><FC>rangeMin<FN></C> <C>Range start.</C> </R>
<R> <C><FC>rangeMax<FN></C> <C>Range end.</C> </R>
</TABLE>
!!}
    function createHistogram(sizes: TIEVisionVectorInt32; histType: TIEVisionHistogramType; ranges: TIEVisionVectorFloatPair; uniform: bool32 = true; wantExceptions: bool32 = false): TIEVisionHistogram; overload; safecall;
    function createHistogram(bins: int32_t = 256; histType: TIEVisionHistogramType = ievARRAY; rangeMin: single = 0.0; rangeMax: single = 255.0; uniform: bool32 = true; wantExceptions: bool32 = false): TIEVisionHistogram; overload; safecall;

{!!
<FS>TIEVisionLibrary.createVectorInt32

<FM>Declaration<FC>
function createVectorInt32(): <A TIEVisionVectorInt32>; overload; safecall;
function createVectorInt32(src: <A TIEVisionVectorInt32>): <A TIEVisionVectorInt32>; overload; safecall;

<FM>Description<FN>
Creates a vector of integers (signed 32 bit).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Vector to copy.</C> </R>
</TABLE>
!!}
    function createVectorInt32(wantExceptions: bool32 = false): TIEVisionVectorInt32; overload; safecall;
    function createVectorInt32(src: TIEVisionVectorInt32; wantExceptions: bool32 = false): TIEVisionVectorInt32; overload; safecall;

{!!
<FS>TIEVisionLibrary.createVectorFloatPair

<FM>Declaration<FC>
function createVectorFloatPair(): <A TIEVisionVectorFloatPair>; overload; safecall;
function createVectorFloatPair(src: <A TIEVisionVectorFloatPair>): <A TIEVisionVectorFloatPair>; overload; safecall;

<FM>Description<FN>
Creates a vector of float pairs (two 32 bit floating point values).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Vector to copy.</C> </R>
</TABLE>
!!}
    function createVectorFloatPair(wantExceptions: bool32 = false): TIEVisionVectorFloatPair; overload; safecall;
    function createVectorFloatPair(src: TIEVisionVectorFloatPair; wantExceptions: bool32 = false): TIEVisionVectorFloatPair; overload; safecall;

{!!
<FS>TIEVisionLibrary.createVectorPoint

<FM>Declaration<FC>
function createVectorPoint(): <A TIEVisionVectorPoint>; overload; safecall;
function createVectorPoint(src: <A TIEVisionVectorPoint>): <A TIEVisionVectorPoint>; overload; safecall;

<FM>Description<FN>
Creates a vector of points.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Vector to copy.</C> </R>
</TABLE>
!!}
    function createVectorPoint(wantExceptions: bool32 = false): TIEVisionVectorPoint; overload; safecall;
    function createVectorPoint(src: TIEVisionVectorPoint; wantExceptions: bool32 = false): TIEVisionVectorPoint; overload; safecall;

{!!
<FS>TIEVisionLibrary.createVectorRect

<FM>Declaration<FC>
function createVectorRect(): <A TIEVisionVectorRect>; overload; safecall;
function createVectorRect(src: <A TIEVisionVectorRect>): <A TIEVisionVectorRect>; overload; safecall;

<FM>Description<FN>
Creates a vector of rectangles.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Vector to copy.</C> </R>
</TABLE>
!!}
    function createVectorRect(wantExceptions: bool32 = false): TIEVisionVectorRect; overload; safecall;
    function createVectorRect(src: TIEVisionVectorRect; wantExceptions: bool32 = false): TIEVisionVectorRect; overload; safecall;

{!!
<FS>TIEVisionLibrary.createVectorByte

<FM>Declaration<FC>
function createVectorByte(): <A TIEVisionVectorByte>; overload; safecall;
function createVectorByte(src: <A TIEVisionVectorByte>): <A TIEVisionVectorByte>; overload; safecall;

<FM>Description<FN>
Creates a vector of bytes (8 bit unsigned).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Vector to copy.</C> </R>
</TABLE>
!!}
    function createVectorByte(wantExceptions: bool32 = false): TIEVisionVectorByte; overload; safecall;
    function createVectorByte(src: TIEVisionVectorByte; wantExceptions: bool32 = false): TIEVisionVectorByte; overload; safecall;

{!!
<FS>TIEVisionLibrary.createVectorDouble

<FM>Declaration<FC>
function createVectorDouble(): <A TIEVisionVectorDouble>; overload; safecall;
function createVectorDouble(src: <A TIEVisionVectorDouble>): <A TIEVisionVectorDouble>; overload; safecall;

<FM>Description<FN>
Creates a vector of floating point values (64 bit).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Vector to copy.</C> </R>
</TABLE>
!!}
    function createVectorDouble(wantExceptions: bool32 = false): TIEVisionVectorDouble; overload; safecall;
    function createVectorDouble(src: TIEVisionVectorDouble; wantExceptions: bool32 = false): TIEVisionVectorDouble; overload; safecall;

{!!
<FS>TIEVisionLibrary.createVectorImageRef

<FM>Declaration<FC>
function createVectorImageRef(): <A TIEVisionVectorImageRef>; overload; safecall;
function createVectorImageRef(src: <A TIEVisionVectorImageRef>): <A TIEVisionVectorImageRef>; overload; safecall;

<FM>Description<FN>
Creates a vector of <A TIEVisionImage> references.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>src<FN></C> <C>Vector to copy.</C> </R>
</TABLE>
!!}
    function createVectorImageRef(wantExceptions: bool32 = false): TIEVisionVectorImageRef; overload; safecall;
    function createVectorImageRef(src: TIEVisionVectorImageRef; wantExceptions: bool32 = false): TIEVisionVectorImageRef; overload; safecall;

{!!
<FS>TIEVisionLibrary.createObjectTracker

<FM>Declaration<FC>
function createObjectTracker(): <A TIEVisionObjectTracker>; safecall;

<FM>Description<FN>
Creates an object tracker.
!!}
    function createObjectTracker(wantExceptions: bool32 = false): TIEVisionObjectTracker; safecall;

{!!
<FS>TIEVisionLibrary.createObjectsFinder

<FM>Declaration<FC>
function createObjectsFinder(): <A TIEVisionObjectsFinder>; safecall;

<FM>Description<FN>
Creates an object finder.
!!}
    function createObjectsFinder(wantExceptions: bool32 = false): TIEVisionObjectsFinder; safecall;

{!!
<FS>TIEVisionLibrary.createOCR

<FM>Declaration<FC>
function createOCR(language: PAnsiChar = nil; engine: <A TIEVisionOCREngine> = ievOCRFAST): <A TIEVisionOCR>; overload; safecall;
function createOCR(path: PAnsiChar; language: PAnsiChar; engine: <A TIEVisionOCREngine> = ievOCRFAST): <A TIEVisionOCR>; overload; safecall;
function createOCR(path: PAnsiChar; languages: <A TIEVisionVectorString>; engine: <A TIEVisionOCREngine> = ievOCRFAST): <A TIEVisionOCR>; overload; safecall;

<FM>Description<FN>
Creates an OCR object for the specified language.
<L TIEVisionLanguages>Many languages</L> are available as separate files.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>language<FN></C> <C>Language code of OCR recognition (ex. 'eng', 'fra').</C> </R>
<R> <C><FC>path<FN></C> <C>Path to language data files.</C> </R>
<R> <C><FC>engine<FN></C> <C>OCR engine to use.</C> </R>
<R> <C><FC>languages<FN></C> <C>A list of languages.</C> </R>
</TABLE>


<FM>Example<FC>
OCR := IEVisionLib.createOCR(IEOCRLanguageList[OCR_English_language].Code);

OR

sLanguage := 'fra'; // French
if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + '.traineddata' + sLanguage) = False then
  raise Exception.create('Language file not found');
OCR := IEVisionLib.createOCR(sLanguage);

OR (multiple languages)

var langs: TIEVisionVectorString;
langs := IEVisionLib.createVectorString();
langs.push_back(IEOCRLanguageList[OCR_English_language].Code);  // load English
langs.push_back(IEOCRLanguageList[OCR_Italian_language].Code);  // load Italian
m_OCR := IEVisionLib.createOCR('', langs, TIEVisionOCREngine(ComboBox1.ItemIndex));
!!}
    function createOCR(language: PAnsiChar = nil; engine: TIEVisionOCREngine = ievOCRFAST; wantExceptions: bool32 = false): TIEVisionOCR; overload; safecall;
    function createOCR(path: PAnsiChar; language: PAnsiChar; engine: TIEVisionOCREngine = ievOCRFAST; wantExceptions: bool32 = false): TIEVisionOCR; overload; safecall;
    function createOCR(path: PAnsiChar; languages: TIEVisionVectorString; engine: TIEVisionOCREngine = ievOCRFAST; wantExceptions: bool32 = false): TIEVisionOCR; overload; safecall;

{!!
<FS>TIEVisionLibrary.createPeopleDetector

<FM>Declaration<FC>
function createPeopleDetector(wantExceptions: bool32 = false): <A TIEVisionPeopleDetector>; safecall;

<FM>Description<FN>
Creates a People Detector object.


<FM>Example<FC>
objectsFinder := IEVisionLib.createObjectsFinder();
objectsFinder.addClassifier('People Detector', IEVisionLib.createPeopleDetector());
!!}
    function createPeopleDetector(wantExceptions: bool32 = false): TIEVisionPeopleDetector; safecall;

{!!
<FS>TIEVisionLibrary.createBarCodeScanner

<FM>Declaration<FC>
function createBarCodeScanner(wantExceptions: bool32 = false): <A TIEVisionBarCodeScanner>; safecall;

<FM>Description<FN>
Creates a bar code scanner object.


<FM>Example<FC>
var
  symbols: TIEVisionVectorObjRef;
  s: TIEVisionBarCodeSymbol;
  i: integer;
begin
  symbols := IEVisionLib.createBarCodeScanner().scan(ImageEnView1.IEBitmap.GetIEVisionImage(), IEVisionRect(0, 0, 0, 0));
  for i := 0 to symbols.size() - 1 do
  begin
    s := TIEVisionBarCodeSymbol( symbols.getObj(i) );
    Memo1.Lines.Add('type = ' + s.getSymbolType().c_str());
    Memo1.Lines.Add('data = ' + s.getData().c_str());
    with s.getBoundingBox() do
      Memo1.Lines.Add('rect = ' + inttostr(x) + ' ' + inttostr(y) + ' ' + inttostr(width) + ' ' + inttostr(height));
  end;
end;
!!}
    function createBarCodeScanner(wantExceptions: bool32 = false): TIEVisionBarCodeScanner; safecall;

{!!
<FS>TIEVisionLibrary.getLibraryInfo

<FM>Declaration<FC>
function getLibraryInfo(info: <A TIEVisionLibraryInfo>; wantExceptions: bool32 = false): <A TIEVisionString>; safecall;

<FM>Description<FN>
Returns some library infos (version, platform, etc).


<FM>Example<FC>
ShowMessage( Format('%s %s %s bit (filename: "%s", is trial: %s)', [
    IEVisionLib().getLibraryInfo(ievLIBNAME).c_str(), 
    IEVisionLib().getLibraryInfo(ievLIBVERSION).c_str(), 
    IEVisionLib().getLibraryInfo(ievLIBPLATFORM).c_str(), 
    IEVisionLib().getLibraryInfo(ievLIBFILENAME).c_str(), 
    IEVisionLib().getLibraryInfo(ievISTRIAL).c_str()]) );
!!}
    function getLibraryInfo(info: TIEVisionLibraryInfo; wantExceptions: bool32 = false): TIEVisionString; safecall;

  end;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



  procedure IEInitialize_ievision(const sDLLPath: WideString = ''; reinitialize: boolean = true);
  procedure IEFinalize_ievision();


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



{!!
<FS>IEVisionAvailable

<FM>Declaration<FC>
function IEVisionAvailable(): boolean;

<FM>Description<FN>
Returns true if ievision is available and loaded.


<FM>Example<FC>
if not IEVisionAvailable() then
begin
  ShowMessage('This application requires ievision.dll plugin. Please download it from www.imageen.com');
  Application.Terminate;
  exit;
end;
!!}
  function IEVisionAvailable(): boolean;

{!!
<FS>IEVisionSize

<FM>Declaration<FC>
function IEVisionSize(width, height: int32_t): <A TIEVisionSize>;

<FM>Description<FN>
Returns a <A TIEVisionSize> record initialized with specified parameters.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>width<FN></C> <C>Width</C> </R>
<R> <C><FC>height<FN></C> <C>Height</C> </R>
</TABLE>
!!}
  function IEVisionSize(width, height: int32_t): TIEVisionSize;

{!!
<FS>IEVisionRect

<FM>Declaration<FC>
function IEVisionRect(x, y, width, height: int32_t): <A TIEVisionRect>;

<FM>Description<FN>
Returns a <A TIEVisionRect> record initialized with specified parameters.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>x<FN></C> <C>Top-left horizontal coordinate.</C> </R>
<R> <C><FC>y<FN></C> <C>Top-left vertical coordinate.</C> </R>
<R> <C><FC>width<FN></C> <C>Width.</C> </R>
<R> <C><FC>height<FN></C> <C>Height.</C> </R>
</TABLE>
!!}
  function IEVisionRect(x, y, width, height: int32_t): TIEVisionRect;

{!!
<FS>IEVisionPoint

<FM>Declaration<FC>
function IEVisionPoint(x, y: int32_t): <A TIEVisionPoint>;

<FM>Description<FN>
Returns a <A TIEVisionPoint> record initialized with specified parameters.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>x<FN></C> <C>Top-left horizontal coordinate.</C> </R>
<R> <C><FC>y<FN></C> <C>Top-left vertical coordinate.</C> </R>
</TABLE>
!!}
  function IEVisionPoint(x, y: int32_t): TIEVisionPoint;

{!!
<FS>IEVisionScalar

<FM>Declaration<FC>
function IEVisionScalar(val0: double; val1: double = 0.0; val2: double = 0.0; val3: double = 0.0): <A TIEVisionScalar>;

<FM>Description<FN>
Returns a <A TIEVisionScalar> record initialized with specified parameters.
This structure can be also used to specify BGR colors: IEVisionScalar(0, 0, 255) = full red.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>val0<FN></C> <C>First scalar value.</C> </R>
<R> <C><FC>val1<FN></C> <C>Second scalar value.</C> </R>
<R> <C><FC>val2<FN></C> <C>Third scalar value.</C> </R>
<R> <C><FC>val3<FN></C> <C>Fourth scalar value.</C> </R>
</TABLE>
!!}
  function IEVisionScalar(val0: double; val1: double = 0.0; val2: double = 0.0; val3: double = 0.0): TIEVisionScalar;

{!!
<FS>IEVisionFloatPair

<FM>Declaration<FC>
function IEVisionFloatPair(first: double; second: double): <A TIEVisionFloatPair>;

<FM>Description<FN>
Returns a <A TIEVisionFloatPair> record initialized with specified parameters.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>first<FN></C> <C>First floating point value.</C> </R>
<R> <C><FC>second<FN></C> <C>Second floating point value.</C> </R>
</TABLE>
!!}
  function IEVisionFloatPair(first: double; second: double): TIEVisionFloatPair;

{!!
<FS>IEVisionTermCriteria

<FM>Declaration<FC>
function IEVisionTermCriteria(ctype: <A TIEVisionTermCriteriaType>; maxIter: int32_t; epsilon: double): <A TIEVisionTermCriteria>;

<FM>Description<FN>
Returns a <A TIEVisionTermCriteria> record initialized with specified parameters.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>ctype<FN></C> <C>Type of the termination criteria.</C> </R>
<R> <C><FC>maxIter<FN></C> <C>Maximum number of iterations.</C> </R>
<R> <C><FC>epsilon<FN></C> <C>Required accuracy.</C> </R>
</TABLE>
!!}
  function IEVisionTermCriteria(ctype: TIEVisionTermCriteriaType; maxIter: int32_t; epsilon: double): TIEVisionTermCriteria;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function IEVisionLanguageCodeToName(const sLangCode : string) : string;

function IEVisionLanguageNameToCode(const sLangName : string) : string;

function IEVisionGetLanguagesInFolder(ssDest : TStrings; const sFolder : string; bDisplayName : Boolean = False) : Boolean;


{!!
<FS>TIEOCRLanguages

<FM>Declaration<FC>
}
type
  TIEOCRLanguages = (
    OCR_Ancient_Greek_Language,
    OCR_Esperanto_Alternative_Language,
    OCR_English_Language,
    OCR_Ukrainian_Language,
    OCR_Turkish_Language,
    OCR_Thai_Language,
    OCR_Tagalog_Language,
    OCR_Telugu_Language,
    OCR_Tamil_Language,
    OCR_Swedish_Language,
    OCR_Swahili_Language,
    OCR_Serbian_Latin_Language,
    OCR_Albanian_Language,
    OCR_Spanish_Old_Language,
    OCR_Spanish_Language,
    OCR_Slovenian_Language,
    OCR_Slovakian_Language,
    OCR_Romanian_Language,
    OCR_Portuguese_Language,
    OCR_Polish_Language,
    OCR_Norwegian_Language,
    OCR_Dutch_Language,
    OCR_Malay_Language,
    OCR_Maltese_Language,
    OCR_Macedonian_Language,
    OCR_Malayalam_Language,
    OCR_Lithuanian_Language,
    OCR_Latvian_Language,
    OCR_Korean_Language,
    OCR_Kannada_Language,
    OCR_Italian_Old_Language,
    OCR_Italian_Language,
    OCR_Icelandic_Language,
    OCR_Indonesian_Language,
    OCR_Cherokee_Language,
    OCR_Hungarian_Language,
    OCR_Croatian_Language,
    OCR_Hindi_Language,
    OCR_Hebrew_Language,
    OCR_Galician_Language,
    OCR_Middle_French_1400_1600_Language,
    OCR_Frankish_Language,
    OCR_French_Language,
    OCR_Finnish_Language,
    OCR_Basque_Language,
    OCR_Estonian_Language,
    OCR_Esperanto_Language,
    OCR_Middle_English_1100_1500_Language,
    OCR_Greek_Language,
    OCR_German_Language,
    OCR_Danish_Language,
    OCR_Czech_Language,
    OCR_Catalan_Language,
    OCR_Bulgarian_Language,
    OCR_Bengali_Language,
    OCR_Belarusian_Language,
    OCR_Azerbaijani_Language,
    OCR_Arabic_Language,
    OCR_Afrikaans_Language,
    OCR_Japanese_Language,
    OCR_Chinese_Simplified_Language,
    OCR_Chinese_Traditional_Language,
    OCR_Russian_Language,
    OCR_Vietnamese_Language
  );
{!!}


{!!
<FS>TIEVisionLanguages

<FM>Declaration<FC>
}
type
  TIEVisionLanguages = record
    Code        : PAnsiChar;
    DisplayName : string;
  end;

const
  IEOCRLanguageList: array[TIEOCRLanguages] of TIEVisionLanguages = (
    (Code: 'grc';	DisplayName: 'Ancient Greek'),
    (Code: 'epo_alt';	DisplayName: 'Esperanto Alternative'),
    (Code: 'eng';	DisplayName: 'English'),
    (Code: 'ukr';	DisplayName: 'Ukrainian'),
    (Code: 'tur';	DisplayName: 'Turkish'),
    (Code: 'tha';	DisplayName: 'Thai'),
    (Code: 'tgl';	DisplayName: 'Tagalog'),
    (Code: 'tel';	DisplayName: 'Telugu'),
    (Code: 'tam';	DisplayName: 'Tamil'),
    (Code: 'swe';	DisplayName: 'Swedish'),
    (Code: 'swa';	DisplayName: 'Swahili'),
    (Code: 'srp';	DisplayName: 'Serbian (Latin)'),
    (Code: 'sqi';	DisplayName: 'Albanian'),
    (Code: 'spa_old';	DisplayName: 'Spanish (Old)'),
    (Code: 'spa';	DisplayName: 'Spanish'),
    (Code: 'slv';	DisplayName: 'Slovenian'),
    (Code: 'slk';	DisplayName: 'Slovakian'),
    (Code: 'ron';	DisplayName: 'Romanian'),
    (Code: 'por';	DisplayName: 'Portuguese'),
    (Code: 'pol';	DisplayName: 'Polish'),
    (Code: 'nor';	DisplayName: 'Norwegian'),
    (Code: 'nld';	DisplayName: 'Dutch'),
    (Code: 'msa';	DisplayName: 'Malay'),
    (Code: 'mlt';	DisplayName: 'Maltese'),
    (Code: 'mkd';	DisplayName: 'Macedonian'),
    (Code: 'mal';	DisplayName: 'Malayalam'),
    (Code: 'lit';	DisplayName: 'Lithuanian'),
    (Code: 'lav';	DisplayName: 'Latvian'),
    (Code: 'kor';	DisplayName: 'Korean'),
    (Code: 'kan';	DisplayName: 'Kannada'),
    (Code: 'ita_old';	DisplayName: 'Italian (Old)'),
    (Code: 'ita';	DisplayName: 'Italian'),
    (Code: 'isl';	DisplayName: 'Icelandic'),
    (Code: 'ind';	DisplayName: 'Indonesian'),
    (Code: 'chr';	DisplayName: 'Cherokee'),
    (Code: 'hun';	DisplayName: 'Hungarian'),
    (Code: 'hrv';	DisplayName: 'Croatian'),
    (Code: 'hin';	DisplayName: 'Hindi'),
    (Code: 'heb';	DisplayName: 'Hebrew'),
    (Code: 'glg';	DisplayName: 'Galician'),
    (Code: 'frm';	DisplayName: 'Middle French (ca. 1400-1600)'),
    (Code: 'frk';	DisplayName: 'Frankish'),
    (Code: 'fra';	DisplayName: 'French'),
    (Code: 'fin';	DisplayName: 'Finnish'),
    (Code: 'eus';	DisplayName: 'Basque'),
    (Code: 'est';	DisplayName: 'Estonian'),
    (Code: 'epo';	DisplayName: 'Esperanto'),
    (Code: 'enm';	DisplayName: 'Middle English (1100-1500)'),
    (Code: 'ell';	DisplayName: 'Greek'),
    (Code: 'deu';	DisplayName: 'German'),
    (Code: 'dan';	DisplayName: 'Danish'),
    (Code: 'ces';	DisplayName: 'Czech'),
    (Code: 'cat';	DisplayName: 'Catalan'),
    (Code: 'bul';	DisplayName: 'Bulgarian'),
    (Code: 'ben';	DisplayName: 'Bengali'),
    (Code: 'bel';	DisplayName: 'Belarusian'),
    (Code: 'aze';	DisplayName: 'Azerbaijani'),
    (Code: 'ara';	DisplayName: 'Arabic'),
    (Code: 'afr';	DisplayName: 'Afrikaans'),
    (Code: 'jpn';	DisplayName: 'Japanese'),
    (Code: 'chi_sim';	DisplayName: 'Chinese (Simplified)'),
    (Code: 'chi_tra';	DisplayName: 'Chinese (Traditional)'),
    (Code: 'rus';	DisplayName: 'Russian'),
    (Code: 'vie';	DisplayName: 'Vietnamese')
    );
{!!}

function LanguageExistsInFolder(aLanguage : TIEOCRLanguages; const sFolder : string) : Boolean; overload;
function LanguageExistsInFolder(const sLangCode : string; const sFolder : string) : Boolean; overload;

function CubeEngineExistsInFolder(aLanguage : TIEOCRLanguages; const sFolder : string) : Boolean; overload;
function CubeEngineExistsInFolder(const sLangCode : string; const sFolder : string) : Boolean; overload;

const
  IEV_OCR_Language_Data_Ext = '.traineddata';



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>IEVisionLib

<FM>Declaration<FC>
function IEVisionLib(): <A TIEVisionLibrary>;

<FM>Description<FN>
When <A IEVisionAvailable> is true then this function returns a pre-built <A TIEVisionLibrary> interface which allows you to create all other IEVision objects.


<FM>Example<FC>
// creates empty image
image := IEVisionLib().createImage();
!!}
function IEVisionLib(): TIEVisionLibrary;





////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type

  TIEVisionCustomStreamProvider = interface(IUnknown)
    function size(): int64; safecall;
    procedure seek(offset: int64; whence: TIEVisionSeekOffset); safecall;
    function tell(): int64; safecall;
    function silent_read(ptr: pointer; size: int64): int64; safecall;
    function silent_write(ptr: pointer; size: int64): int64; safecall;
    function silent_getc(): int32_t; safecall;
    function eof(): bool32; safecall;
  end;


  TIEVisionCustomStream = interface(TIEVisionStream)
    procedure reset(provider: TIEVisionCustomStreamProvider; wantExceptions: bool32 = false); safecall;     // don't free "provider" object!
  end;


  TIELibJPEGErrorManager = interface(TIEVisionBase)
    function getNumWarnings(wantExceptions: bool32 = false): int32_t; safecall;
    function getErrMsgCode(wantExceptions: bool32 = false): int32_t; safecall;
  end;

  TIELibJPEGMarker = interface(TIEVisionBase)
    function getID(wantExceptions: bool32 = false): uint8_t; safecall;
    function getData(wantExceptions: bool32 = false): pointer; safecall;
    function getLength(wantExceptions: bool32 = false): uint32_t; safecall;
    function next(wantExceptions: bool32 = false): TIELibJPEGMarker; safecall;
  end;

  TIELibJPEGDecompressor = interface(TIEVisionBase)
    procedure setErrorManager(errorManager: TIELibJPEGErrorManager; wantExceptions: bool32 = false); safecall;
    procedure createDecompress(wantExceptions: bool32 = false); safecall;
    procedure destroyDecompress(wantExceptions: bool32 = false); safecall;
    procedure setupReadStream(stream: TIEVisionStream; bufferSize: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure readHeader(requireImage: bool32; wantExceptions: bool32 = false); safecall;
    procedure saveMarkers(markerCode: int32_t; lengthLimit: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure startDecompress(wantExceptions: bool32 = false); safecall;
    function getOutputWidth(wantExceptions: bool32 = false): uint32_t; safecall;
    function getOutputHeight(wantExceptions: bool32 = false): uint32_t; safecall;
    function getOutputComponents(wantExceptions: bool32 = false): uint32_t; safecall;
    function getOutputScanline(wantExceptions: bool32 = false): uint32_t; safecall;
    function readScanlines(scanlines: pointer; maxLines: uint32_t; wantExceptions: bool32 = false): uint32_t; safecall;
    procedure finishDecompress(wantExceptions: bool32 = false); safecall;
    procedure setOutColorSpace(colorSpace: uint32_t; wantExceptions: bool32 = false); safecall;
    function getOutColorSpace(wantExceptions: bool32 = false): uint32_t; safecall;
    function getJpegColorSpace(wantExceptions: bool32 = false): uint32_t; safecall;
    procedure setDCTMethod(DCTMethod: uint32_t; wantExceptions: bool32 = false); safecall;
    function getDensityUnit(wantExceptions: bool32 = false): uint8_t; safecall;
    function getXDensity(wantExceptions: bool32 = false): uint16_t; safecall;
    function getYDensity(wantExceptions: bool32 = false): uint16_t; safecall;
    function getProgressiveMode(wantExceptions: bool32 = false): bool32; safecall;
    function getImageWidth(wantExceptions: bool32 = false): uint32_t; safecall;
    function getImageHeight(wantExceptions: bool32 = false): uint32_t; safecall;
    procedure setScale(num: uint32_t; den: uint32_t; wantExceptions: bool32 = false); safecall;
    function getMarkerList(wantExceptions: bool32 = false): TIELibJPEGMarker; safecall;
  end;

  TIELibJPEGCompressor = interface(TIEVisionBase)
    procedure setErrorManager(errorManager: TIELibJPEGErrorManager; wantExceptions: bool32 = false); safecall;
    procedure setupWriteStream(stream: TIEVisionStream; bufferSize: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure createCompress(wantExceptions: bool32 = false); safecall;
    procedure destroyCompress(wantExceptions: bool32 = false); safecall;
    procedure setImageWidth(imageWidth: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure setImageHeight(imageHeight: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure setInputComponents(inputComponents: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure setInColorSpace(colorSpace: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure setColorSpace(colorSpace: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure setDefaults(wantExceptions: bool32 = false); safecall;
    procedure setDensityUnit(densityUnit: uint8_t; wantExceptions: bool32 = false); safecall;
    procedure setXDensity(XDensity: uint16_t; wantExceptions: bool32 = false); safecall;
    procedure setYDensity(YDensity: uint16_t; wantExceptions: bool32 = false); safecall;
    procedure setDCTMethod(DCTMethod: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure setOptimizeCoding(optimizeCoding: bool32; wantExceptions: bool32 = false); safecall;
    procedure setSmoothingFactor(smoothingFactor: int32_t; wantExceptions: bool32 = false); safecall;
    procedure setQuality(quality: int32_t; baseline: bool32; wantExceptions: bool32 = false); safecall;
    procedure setSampleFactor(componentIndex: int32_t; H: int32_t; V: int32_t; wantExceptions: bool32 = false); safecall;
    procedure setWriteJFIFHeader(writeJFIFHeader: bool32; wantExceptions: bool32 = false); safecall;
    procedure simpleProgression(wantExceptions: bool32 = false); safecall;
    procedure startCompress(writeAllTables: bool32; wantExceptions: bool32 = false); safecall;
    procedure writeMarker(markerID: int32_t; markerPtr: pointer; markerLength: uint32_t; wantExceptions: bool32 = false); safecall;
    function getNextScanlineIndex(wantExceptions: bool32 = false): uint32_t; safecall;
    function writeScanlines(scanlines: pointer; maxLines: uint32_t; wantExceptions: bool32 = false): uint32_t; safecall;
    procedure finishCompress(wantExceptions: bool32 = false); safecall;
  end;

  TIELibJPEGTransform = interface(TIEVisionBase)
    procedure requestWorkspace(source: TIELibJPEGDecompressor; wantExceptions: bool32 = false); safecall;
    function adjustParameters(source: TIELibJPEGDecompressor; dest: TIELibJPEGCompressor; sourceCoefArrays: pointer; wantExceptions: bool32 = false): pointer; safecall;
    procedure executeTransformation(source: TIELibJPEGDecompressor; dest: TIELibJPEGCompressor; sourceCoefArrays: pointer; wantExceptions: bool32 = false); safecall;
  end;

  TIELibJPEGCopy = interface(TIEVisionBase)
    procedure copyMarkersSetup(source: TIELibJPEGDecompressor; copyOption: int32_t; wantExceptions: bool32 = false); safecall;
    function readCoefficients(source: TIELibJPEGDecompressor; wantExceptions: bool32 = false): pointer; safecall;
    procedure copyCriticalParameters(source: TIELibJPEGDecompressor; dest: TIELibJPEGCompressor; wantExceptions: bool32 = false); safecall;
    procedure writeCoefficients(dest: TIELibJPEGCompressor; destCoefArrays: pointer; wantExceptions: bool32 = false); safecall;
    procedure copyMarkersExecute(source: TIELibJPEGDecompressor; dest: TIELibJPEGCompressor; copyOption: int32_t; wantExceptions: bool32 = false); safecall;
  end;

  TIELibPNGText = interface(TIEVisionBase)
    function getKey(wantExceptions: bool32 = false): PAnsiChar; safecall;
    function getText(wantExceptions: bool32 = false): PAnsiChar; safecall;
    function getNext(wantExceptions: bool32 = false): TIELibPNGText; safecall;
  end;

  TIELibPNGDecompressor = interface(TIEVisionBase)
    function isValid(wantExceptions: bool32 = false): bool32; safecall;
    procedure setReadFunction(ioPtr: pointer; readDataFunction: pointer; wantExceptions: bool32 = false); safecall;
    procedure readInfo(wantExceptions: bool32 = false); safecall;
    function getIHDR(out width: uint32_t; out height: uint32_t; out bitDepth: int32_t; out colorType: int32_t; out interlaceType: int32_t; out compressionType: int32_t; out filterType: int32_t; wantExceptions: bool32 = false): uint32_t; safecall;
    function getText(out outPngText: TIELibPNGText; wantExceptions: bool32 = false): uint32_t; safecall;
    function getBackground(defaultValue: TIEVisionBGR8; wantExceptions: bool32 = false): TIEVisionBGR8; safecall;
    procedure setExpand(wantExceptions: bool32 = false); safecall;
    procedure setStrip16(wantExceptions: bool32 = false); safecall;
    procedure setPacking(wantExceptions: bool32 = false); safecall;
    procedure setGrayToRGB(wantExceptions: bool32 = false); safecall;
    procedure setBGR(wantExceptions: bool32 = false); safecall;
    procedure setSwap(wantExceptions: bool32 = false); safecall;
    procedure setTRNStoAlpha(wantExceptions: bool32 = false); safecall;
    function setInterlaceHandling(wantExceptions: bool32 = false): int32_t; safecall;
    procedure readUpdateInfo(wantExceptions: bool32 = false); safecall;
    function getXPixelsPerMeter(wantExceptions: bool32 = false): uint32_t; safecall;
    function getYPixelsPerMeter(wantExceptions: bool32 = false): uint32_t; safecall;
    function getPalette(palette: pointer; out numPalette: int32_t; wantExceptions: bool32 = false): uint32_t; safecall;
    function getInterlaceType(wantExceptions: bool32 = false): uint8_t; safecall;
    function getChannels(wantExceptions: bool32 = false): uint8_t; safecall;
    procedure readRows(row: pointer; displayRow: pointer; numRows: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure readEnd(wantExceptions: bool32 = false); safecall;
    function getTRNS(trans: pointer; out numTrans: int32_t; wantExceptions: bool32 = false): uint32_t; safecall;
  end;

  TIELibPNGTextList = interface(TIEVisionBase)
    procedure assign(index: int32_t; compression: int32_t; key: PAnsiChar; text: PAnsiChar; wantExceptions: bool32 = false); safecall;
  end;

  TIELibPNGCompressor = interface(TIEVisionBase)
    function isValid(wantExceptions: bool32 = false): bool32; safecall;
    procedure setWriteFunction(ioPtr: pointer; writeFunc: pointer; flushFunc: pointer; wantExceptions: bool32 = false); safecall;
    procedure setPalette(palette: pointer; numPalette: int32_t; wantExceptions: bool32 = false); safecall;
    procedure setTRNS(trans: pbyte; numTrans: int32_t; wantExceptions: bool32 = false); safecall;
    procedure setIHDR(width: uint32_t; height: uint32_t; bitDepth: int32_t; colorType: int32_t; interlaceType: int32_t; compressionType: int32_t; filterType: int32_t; wantExceptions: bool32 = false); safecall;
    procedure setPHYS(resX: uint32_t; resY: uint32_t; unitType: int32_t; wantExceptions: bool32 = false); safecall;
    procedure setFilter(method: int32_t; filters: int32_t; wantExceptions: bool32 = false); safecall;
    procedure setBackground(colorValue: TIEVisionBGR8; colorIndex: uint8_t; wantExceptions: bool32 = false); safecall;
    procedure setCompressionLevel(level: int32_t; wantExceptions: bool32 = false); safecall;
    procedure setBGR(wantExceptions: bool32 = false); safecall;
    procedure writeInfo(wantExceptions: bool32 = false); safecall;
    function setInterlaceHandling(wantExceptions: bool32 = false): int32_t; safecall;
    procedure writeRows(row: pointer; numRows: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure writeEnd(wantExceptions: bool32 = false); safecall;
    procedure setText(textList: TIELibPNGTextList; wantExceptions: bool32 = false); safecall;
  end;

  TIELibJP2KComponentParamsList = interface(TIEVisionBase)
    procedure assign(index: int32_t; tlx: uint32_t; tly: uint32_t; hstep: uint32_t; vstep: uint32_t; width: uint32_t; height: uint32_t; prec: uint16_t; sgnd: int32_t; wantExceptions: bool32 = false); safecall;
  end;

  TIELibJP2KMatrix = interface(TIEVisionBase)
    function getValue(i: int32_t; j: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
  end;

  TIELibJP2KImage = interface(TIEVisionBase)
    function getWidth(wantExceptions: bool32 = false): int32_t; safecall;
    function getHeight(wantExceptions: bool32 = false): int32_t; safecall;
    function getColorSpace(wantExceptions: bool32 = false): int32_t; safecall;
    function getComponentByType(component: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
    function getComponentPrecision(index: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
    function getNumComponents(wantExceptions: bool32 = false): int32_t; safecall;
    function getComponentHeight(index: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
    function getComponentWidth(index: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
    function readComponent(index: uint16_t; x: int32_t; y: int32_t; width: int32_t; height: int32_t; data: TIELibJP2KMatrix; wantExceptions: bool32 = false): int32_t; safecall;
    function getComponentTopLeftY(index: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
    function getComponentTopLeftX(index: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
    function getComponentVStep(index: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
    function getComponentHStep(index: int32_t; wantExceptions: bool32 = false): int32_t; safecall;
    procedure encode(stream: TIEVisionStream; format: int32_t; options: PAnsiChar; wantExceptions: bool32 = false); safecall;
    procedure setComponentType(index: int32_t; component: int32_t; wantExceptions: bool32 = true); safecall;
    procedure writeComponentSample(index: int32_t; x: int32_t; y: int32_t; v: uint32_t; wantExceptions: bool32 = false); safecall;
    procedure writeRowRGB8(width: int32_t; rowIndex: int32_t; bgr8Array: pbyte; alphaArray: pbyte; colors: int32_t; wantExceptions: bool32 = false); safecall;
    procedure readLinearBGR8(blueMatrix: TIELibJP2KMatrix; greenMatrix: TIELibJP2KMatrix; redMatrix: TIELibJP2KMatrix; rowIndex: int32_t; bluePrec: int32_t; greenPrec: int32_t; redPrec: int32_t; destBGR8: pbyte; width: int32_t; wantExceptions: bool32 = true); safecall;
  end;

  TIELibDCRAWTextInfo = (
    ievCOLOR_DESCRIPTION = 0,
    ievIMAGE_DESCRIPTION = 1,
    ievMAKE              = 2,
    ievMODEL             = 3,
    ievMODEL2            = 4,
    ievARTIST            = 5
  );

  TIELibDCRAWIntInfo = (
    ievWIDTH             = 0,
    ievHEIGHT            = 1,
    ievORIGINAL_WIDTH    = 2,
    ievORIGINAL_HEIGHT   = 3,
    ievPIXELFORMAT       = 4  // its type is TIELibDCRAWPixelFormat
  );

  TIELibDCRAWPixelFormat = (
    ievRGB24 = 0,
    ievRGB48 = 1
  );


  TIELibDCRAWDecoder = interface(TIEVisionBase)
    function getTextInfo(info: TIELibDCRAWTextInfo; wantExceptions: bool32 = false): TIEVisionString; safecall;
    function getIntInfo(info: TIELibDCRAWIntInfo; wantExceptions: bool32 = false): int32_t; safecall;
    procedure getRow(rowIndex: uint32_t; destBuffer: pointer; wantExceptions: bool32 = false); safecall;
    function imageLoaded(wantExceptions: bool32 = false): longbool; safecall;
  end;


  TIELib = interface(TIEVisionBase)
    procedure debug(wantExceptions: bool32 = false); safecall;

    function createString(wantExceptions: bool32 = false): TIEVisionString; overload; safecall;
    function createString(src: TIEVisionString; wantExceptions: bool32 = false): TIEVisionString; overload; safecall;
    function createString(src: PAnsiChar; wantExceptions: bool32 = false): TIEVisionString; overload; safecall;

    function createVectorString(wantExceptions: bool32 = false): TIEVisionVectorString; overload; safecall;
    function createVectorString(src: TIEVisionVectorString; wantExceptions: bool32 = false): TIEVisionVectorString; overload; safecall;

    function createCustomStream(provider: TIEVisionCustomStreamProvider; wantExceptions: bool32 = false): TIEVisionCustomStream; safecall;
    function createJPEGErrorManager(wantExceptions: bool32 = false): TIELibJPEGErrorManager; safecall;
    function createJPEGDecompressor(wantExceptions: bool32 = false): TIELibJPEGDecompressor; safecall;
    function createJPEGCompressor(wantExceptions: bool32 = false): TIELibJPEGCompressor; safecall;
    function createJPEGTransform(transform: int32_t; trim: bool32; forceGrayscale: bool32; xoffs: uint32_t; yoffs: uint32_t; newWidth: uint32_t; newHeight: uint32_t; wantExceptions: bool32 = false): TIELibJPEGTransform; safecall;
    function createJPEGCopy(wantExceptions: bool32 = false): TIELibJPEGCopy; safecall;

    function createPNGDecompressor(errorPtr: pointer; errorFunc: pointer; warnFunc: pointer; wantExceptions: bool32 = false): TIELibPNGDecompressor; safecall;
    function PNGGetIOPtr(pngPtr: pointer; wantExceptions: bool32 = false): pointer; safecall;
    function PNGGetErrorPtr(pngPtr: pointer; wantExceptions: bool32 = false): pointer; safecall;
    function PNGSigCmp(sig: pointer; start: uint32_t; numToCheck: uint32_t; wantExceptions: bool32 = false): int32_t; safecall;

    function createPNGTextList(size: uint32_t; wantExceptions: bool32 = false): TIELibPNGTextList; safecall;
    function createPNGCompressor(errorPtr: pointer; errorFunc: pointer; warnFunc: pointer; wantExceptions: bool32 = false): TIELibPNGCompressor; safecall;

    procedure JP2KInitialize(wantExceptions: bool32 = false); safecall;
    procedure JP2KFinalize(wantExceptions: bool32 = false); safecall;
    function createJP2KImage(stream: TIEVisionStream; wantExceptions: bool32 = false): TIELibJP2KImage; overload; safecall;
    function createJP2KImage(numComponents: uint16_t; parameters: TIELibJP2KComponentParamsList; colorModel: int32_t; wantExceptions: bool32 = false): TIELibJP2KImage; overload; safecall;
    function createJP2KComponentParamsList(size: int32_t; wantExceptions: bool32 = false): TIELibJP2KComponentParamsList; safecall;
    function createJP2KMatrix(numRows: int32_t; numCols: int32_t; wantExceptions: bool32 = false): TIELibJP2KMatrix; safecall;

    function createDCRAWDecoder(inStream: TIEVisionStream; parameters: TIEVisionVectorString; progressCallback: pointer; progressUserData: pointer; wantExceptions: bool32 = false): TIELibDCRAWDecoder; safecall;

  end;




procedure IEInitialize_ielib(const sDLLPath: WideString = ''; reinitialize: boolean = true);
procedure IEFinalize_ielib();
function IELibAvailable(): boolean;
function IELib(): TIELib;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


const
  IELIB_32BIT_DLL_FILENAME1    = 'ielib.dll';
  IELIB_32BIT_DLL_FILENAME2    = 'ielib32.dll';
  IELIB_64BIT_DLL_FILENAME     = 'ielib64.dll';
  IEVISION_32BIT_DLL_FILENAME1 = 'ievision.dll';
  IEVISION_32BIT_DLL_FILENAME2 = 'ievision32.dll';
  IEVISION_64BIT_DLL_FILENAME  = 'ievision64.dll';



implementation



var
  VisionHandle: THandle          = 0;
  IEVisionLib_: TIEVisionLibrary = nil;
  IEVisionCS:   TRTLCriticalSection;
  iev_Library_create: procedure(out result: TIEVisionLibrary); cdecl;


function IEVisionLib(): TIEVisionLibrary;
begin
  if (VisionHandle = 0) or (IEVisionLib_ = nil) then   // check also IEVisionLib_ because IEInitialize_ievision could still run in another thread
    IEInitialize_ievision('', false);
  result := IEVisionLib_;
end;


{!!
<FS>IEVisionAvailable

<FM>Declaration<FC>
function IEVisionAvailable(): boolean;

<FM>Description<FN>
Checks if the ievision.dll is available. When it returns true you can use any IEVision function and class.

<FM>Example<FC>
if not IEVisionAvailable() then
begin
  ShowMessage('This application requires ievision.dll plugin. Please download it from www.imageen.com');
  Application.Terminate;
  exit;
end;
!!}
function IEVisionAvailable(): boolean;
begin
  if (VisionHandle = 0) or (IEVisionLib_ = nil) then   // check also IEVisionLib_ because IEInitialize_ievision could still run in another thread
    IEInitialize_ievision('', false);
  result := (VisionHandle <> 0) and (IEVisionLib_ <> nil);
end;


procedure IEInitialize_ievision(const sDLLPath: WideString = ''; reinitialize: boolean = true);
  procedure TryLoad(const dllname: WideString);
  begin
    if VisionHandle = 0 then
    begin
      VisionHandle := LoadLibraryW(PWideChar(dllname));
      if (VisionHandle = 0) and IsLibrary then // if this is a DLL try to load from dll path             
        VisionHandle := LoadLibraryW(PWideChar( ExtractFilePath(GetModuleName(HInstance)) + dllname ));
      if VisionHandle <> 0 then
      begin
        @iev_Library_create := GetProcAddress(VisionHandle, 'iev_Library_create');

        // initialization
        if @iev_Library_create <> nil then
          iev_Library_create(IEVisionLib_)
        else
        begin
          FreeLibrary(VisionHandle);
          VisionHandle := 0;
        end;
      end;
    end;
  end;
  
begin
  EnterCriticalSection(IEVisionCS);
  try
    if reinitialize then
      IEFinalize_ievision();
    if sDLLPath = '' then
    begin
      TryLoad(IEVISION_32BIT_DLL_FILENAME1);
      TryLoad(IEVISION_32BIT_DLL_FILENAME2);
      TryLoad(IEVISION_64BIT_DLL_FILENAME);
      TryLoad(IELIB_32BIT_DLL_FILENAME1);
      TryLoad(IELIB_32BIT_DLL_FILENAME2);
      TryLoad(IELIB_64BIT_DLL_FILENAME);
    end
    else
      TryLoad(sDLLPath);
  finally
    LeaveCriticalSection(IEVisionCS);
  end;
end;

procedure IEFinalize_ievision();
begin
  if VisionHandle <> 0 then
  begin
    IEVisionLib_ := nil;
    FreeLibrary(VisionHandle);
    VisionHandle := 0;
  end;
end;


///////


var
  IELibHandle: THandle = 0;
  IELib_:      TIELib  = nil;
  iev_IELib_create: procedure(out result: TIELib); cdecl;


function IELib(): TIELib;
begin
  if (IELibHandle = 0) or (IELib_ = nil) then // check also IELib_ because IEInitialize_ielib could still run in another thread
    IEInitialize_ielib('', false);
  result := IELib_;
end;


{!!
<FS>IELibAvailable

<FM>Declaration<FC>
function IELibAvailable() : Boolean;

<FM>Description<FN>
Returns true if the DLL's required by ImageEn are available (e.g. ielib64.dll nn a 64bit ImageEn application. Also initializes the library.

<FM>Example<FC>
procedure TMainForm.FormShow(Sender: TObject);
begin
  if not IELibAvailable() = False then
    MessageDlg(format('The installation is incomplete. Please reinstall %s', [Application_Name]), mtError, [mbOK], 0);
end;
!!}
function IELibAvailable() : Boolean;
begin
  if (IELibHandle = 0) or (IELib_ = nil) then // check also IELib_ because IEInitialize_ielib could still run in another thread
    IEInitialize_ielib('', false);
  result := (IELibHandle <> 0) and (IELib_ <> nil);
end;


procedure IEInitialize_ielib(const sDLLPath: WideString = ''; reinitialize: boolean = true);
  procedure TryLoad(const dllname: WideString);
  begin
    if IELibHandle = 0 then
    begin
      IELibHandle := LoadLibraryW(PWideChar(dllname));
      if (IELibHandle = 0) and IsLibrary then // if this is a DLL try to load from dll path
        IELibHandle := LoadLibraryW(PWideChar( ExtractFilePath(GetModuleName(HInstance)) + dllname ));
      if IELibHandle <> 0 then
      begin
        @iev_IELib_create := GetProcAddress(IELibHandle, 'iev_IELib_create');

        // initialization
        if @iev_IELib_create <> nil then
        begin
          iev_IELib_create(IELib_);
        end
        else
        begin
          FreeLibrary(IELibHandle);
          IELibHandle := 0;
        end;
      end;
    end;
  end;
begin
  EnterCriticalSection(IEVisionCS);
  try
    if reinitialize then
      IEFinalize_ielib();
    if sDLLPath = '' then
    begin
      TryLoad(IEVISION_32BIT_DLL_FILENAME1);
      TryLoad(IEVISION_32BIT_DLL_FILENAME2);
      TryLoad(IEVISION_64BIT_DLL_FILENAME);
      TryLoad(IELIB_32BIT_DLL_FILENAME1);
      TryLoad(IELIB_32BIT_DLL_FILENAME2);
      TryLoad(IELIB_64BIT_DLL_FILENAME);
    end
    else
      TryLoad(sDLLPath);
  finally
    LeaveCriticalSection(IEVisionCS);
  end;
end;

procedure IEFinalize_ielib();
begin
  if IELibHandle <> 0 then
  begin
    IELib_ := nil;
    FreeLibrary(IELibHandle);
    IELibHandle := 0;
  end;
end;


/////////////////////////////////////////////////////////////////////////
// Utilities

function IEVisionSize(width, height: int32_t): TIEVisionSize;
begin
  result.width := width;
  result.height := height;
end;

function IEVisionRect(x, y, width, height: int32_t): TIEVisionRect; overload;
begin
  result.x := x;
  result.y := y;
  result.width := width;
  result.height := height;
end;

function IEVisionPoint(x, y: int32_t): TIEVisionPoint;
begin
  result.x := x;
  result.y := y;
end;

function IEVisionScalar(val0: double; val1: double = 0.0; val2: double = 0.0; val3: double = 0.0): TIEVisionScalar;
begin
  result.val[0] := val0;
  result.val[1] := val1;
  result.val[2] := val2;
  result.val[3] := val3;
end;

function IEVisionFloatPair(first: double; second: double): TIEVisionFloatPair;
begin
  result.first := first;
  result.second := second;
end;

function IEVisionTermCriteria(ctype: TIEVisionTermCriteriaType; maxIter: int32_t; epsilon: double): TIEVisionTermCriteria;
begin
  result.ctype := ctype;
  result.maxIter := maxIter;
  result.epsilon := epsilon;
end;

{!!
<FS>IEVisionLanguageCodeToName

<FM>Declaration<FC>
function IEVisionLanguageCodeToName(const sLangCode : string) : string;

<FM>Description<FN>
Converts a language code (which can be used for loading an OCR language file) to a display name.

E.g. IEVisionLanguageCodeToName('eng') would retun 'English'.

<FM>See Also<FN>
- <A TIEOCRLanguages>
- <A IEVisionLanguageNameToCode>
- <A IEVisionGetLanguagesInFolder>
!!}
function IEVisionLanguageCodeToName(const sLangCode : string) : string;
var
  Lng : TIEOCRLanguages;
begin
  Result := '';
  for Lng := Low(IEOCRLanguageList) to High(IEOCRLanguageList) do
    if SameText(string(AnsiString(IEOCRLanguageList[Lng].Code)), sLangCode) then
    begin
      Result := IEOCRLanguageList[Lng].DisplayName;
      Break;
    end;
end;



{!!
<FS>IEVisionLanguageNameToCode

<FM>Declaration<FC>
function IEVisionLanguageNameToCode(const sLangName : string) : string;

<FM>Description<FN>
Returns the language code (which can be used for loading an OCR language file) from a display name (filled using A TIEOCRLanguages>).

E.g. IEVisionLanguageNameToCode('English') would retun 'eng'.

<FM>Example<FC>
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Fill our Combobox with available languages
  IEVisionGetLanguagesInFolder(cmbLanguage.Items, ExtractFilePath(Application.ExeName), True);

  // Default to English
  cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(IEOCRLanguageList[OCR_English_language].DisplayName);
end;

procedure TMainForm.btnRecognizeClick(Sender: TObject);
var
  sLangCode: string;
begin
  // Get selected language code
  sLangCode := IEVisionLanguageNameToCode(cmbLanguage.Text);

  // create OCR object
  m_OCR := IEVisionLib.createOCR(PAnsiChar(ExtractFilePath(Application.ExeName)), PAnsiChar(sLangCode), ievOCRFAST);
  ...
end;

<FM>See Also<FN>
- <A TIEOCRLanguages>
- <A IEVisionLanguageCodeToName>
- <A IEVisionGetLanguagesInFolder>
!!}
function IEVisionLanguageNameToCode(const sLangName : string) : string;
var
  Lng : TIEOCRLanguages;
begin
  Result := '';
  for Lng := Low(IEOCRLanguageList) to High(IEOCRLanguageList) do
    if SameText(IEOCRLanguageList[Lng].DisplayName, sLangName) then
    begin
      Result := string(AnsiString(IEOCRLanguageList[Lng].Code));
      Break;
    end;
end;


{!!
<FS>IEVisionGetLanguagesInFolder

<FM>Declaration<FC>
function IEVisionGetLanguagesInFolder(ssDest : TStrings; const sFolder : string; bDisplayName : Boolean = False) : Boolean;

<FM>Description<FN>
Finds all the language files in a folder and fills ssDest with their display name or language code.

Language files of IEVision will be named with a language code followed by extensions such as traineddata (IEV_OCR_Language_Data_Ext), word-freq, params, size, etc.

Result is false if no language files are found.

<FM>Example<FC>
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Fill our Combobox with available languages
  IEVisionGetLanguagesInFolder(cmbLanguage.Items, ExtractFilePath(Application.ExeName), True);

  // Default to English
  cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(IEOCRLanguageList[OCR_English_language].DisplayName);
end;

procedure TMainForm.btnRecognizeClick(Sender: TObject);
var
  sLangCode: string;
begin
  // Get selected language code
  sLangCode := IEVisionLanguageNameToCode(cmbLanguage.Text);

  // create OCR object
  m_OCR := IEVisionLib.createOCR(PAnsiChar(ExtractFilePath(Application.ExeName)), PAnsiChar(sLangCode), ievOCRFAST);
  ...
end;

<FM>See Also<FN>
- <A TIEOCRLanguages>
- <A LanguageExistsInFolder>
- <A IEVisionLanguageCodeToName>
- <A IEVisionLanguageNameToCode>
!!}
function IEVisionGetLanguagesInFolder(ssDest : TStrings; const sFolder : string; bDisplayName : Boolean = False) : Boolean;
var
  Lng : TIEOCRLanguages;
begin
  Result := False;
  ssDest.BeginUpdate;
  ssDest.Clear;
  for Lng := Low(IEOCRLanguageList) to High(IEOCRLanguageList) do
    if LanguageExistsInFolder(string(AnsiString(IEOCRLanguageList[Lng].Code)), sFolder) then
    begin
      if bDisplayName then
        ssDest.Add(IEOCRLanguageList[Lng].DisplayName)
      else
        ssDest.Add(string(AnsiString(IEOCRLanguageList[Lng].Code)));
      Result := True;
    end;
  ssDest.EndUpdate;
end;


{!!
<FS>LanguageExistsInFolder

<FM>Declaration<FC>
function LanguageExistsInFolder(aLanguage : TIEOCRLanguages; const sFolder : string) : Boolean; overload;
function LanguageExistsInFolder(const sLangCode : string; const sFolder : string) : Boolean; overload;

<FM>Description<FN>
Returns true if the language file for the specified language is found in a folder.

Language files of IEVision will be named with a language code followed by extensions such as traineddata (IEV_OCR_Language_Data_Ext), word-freq, params, size, etc.

<FM>Examples<FC>
if LanguageExistsInFolder(IEVisionLanguageNameToCode('English'), ExtractFilePath(Application.ExeName)) = False then
  MessageDlg('The English language OCR files could not be found. Please reinstall.', mtError, [mbOK], 0);

if LanguageExistsInFolder(OCR_English_language, ExtractFilePath(Application.ExeName)) = False then
  MessageDlg('The English language OCR files could not be found. Please reinstall.', mtError, [mbOK], 0);

<FM>See Also<FN>
- <A TIEOCRLanguages>
- <A CubeEngineExistsInFolder>
- <A IEVisionGetLanguagesInFolder>
- <A IEVisionLanguageCodeToName>
- <A IEVisionLanguageNameToCode>
!!}
function LanguageExistsInFolder(aLanguage : TIEOCRLanguages; const sFolder : string) : Boolean;
begin
  result := LanguageExistsInFolder(string(AnsiString(IEOCRLanguageList[aLanguage].Code)), sFolder);
end;

function LanguageExistsInFolder(const sLangCode : string; const sFolder : string) : Boolean;
var
  sFilename: string;
begin
  sFilename := IncludeTrailingBackSlash(sFolder) + sLangCode + IEV_OCR_Language_Data_Ext;
  Result := FileExists(sFilename);
end;

{!!
<FS>CubeEngineExistsInFolder

<FM>Declaration<FC>
function CubeEngineExistsInFolder(aLanguage : TIEOCRLanguages; const sFolder : string) : Boolean; overload;
function CubeEngineExistsInFolder(const sLangCode : string; const sFolder : string) : Boolean; overload;

<FM>Description<FN>
Returns true if the support files for the Cube OCR engine of a specified language is found in a folder.

When <L TIEVisionLibrary.createOCR>creating the OCR engine</L> you must specify which OCR engine to use, base, Cube, or both.

If you use the base OCR engine (<FC>ievOCRFAST<FN>) you will only require the *.trained data support file, e.g. eng.traineddata for processing of an English language document. Use of <FC>ievOCRCUBE<FN> and <FC>ievOCRACCURATE<FN> require you to also enclude the *.cube.* files. e.g. eng.cube.word-freq, eng.cube.size, etc.

All existing cube trainings have the following files:

    XXX.cube.fold
    XXX.cube.lm
    XXX.cube.nn
    XXX.cube.params
    XXX.cube.size
    XXX.cube.word-freq

Some also have these files:

    XXX.cube.bigrams
    XXX.cube-word-dawg
    XXX.cube-unicharset
    XXX.tesseract_cube.nn

<FM>Example<FC>
if (SelectedOCREngine in [ievOCRCUBE, ievOCRACCURATE]) and (CubeEngineExistsInFolder(OCR_English_language, ExtractFilePath(Application.ExeName)) = False) then
  MessageDlg('Files for the selected OCR engine could not be found. Please reinstall.', mtError, [mbOK], 0);

<FM>See Also<FN>
- <A TIEOCRLanguages>
- <A IEVisionGetLanguagesInFolder>
- <A IEVisionLanguageCodeToName>
- <A IEVisionLanguageNameToCode>
!!}
function CubeEngineExistsInFolder(aLanguage : TIEOCRLanguages; const sFolder : string) : Boolean;
begin
  result := CubeEngineExistsInFolder(string(AnsiString(IEOCRLanguageList[aLanguage].Code)), sFolder);
end;

function CubeEngineExistsInFolder(const sLangCode : string; const sFolder : string) : Boolean;
const
  IEV_Cube_OCR_Engine_Ext_1 = '.cube.size';
  IEV_Cube_OCR_Engine_Ext_2 = '.cube.word-freq';
var
  sFilename1, sFilename2 : string;
begin
  sFilename1 := IncludeTrailingBackSlash(sFolder) + sLangCode + IEV_Cube_OCR_Engine_Ext_1;
  sFilename2 := IncludeTrailingBackSlash(sFolder) + sLangCode + IEV_Cube_OCR_Engine_Ext_2;
  Result := FileExists(sFilename1) and FileExists(sFilename2);
end;


initialization
  InitializeCriticalSection(IEVisionCS);

finalization
  DeleteCriticalSection(IEVisionCS);



{$else} // IEVISION

interface

implementation

{$endif}  // IEVISION


end.
