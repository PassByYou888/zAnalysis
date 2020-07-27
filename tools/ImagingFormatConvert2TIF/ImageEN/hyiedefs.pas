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
File version 1028
*)


unit hyiedefs;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, Messages, Classes, SysUtils, Graphics;



const
  // version details
  IEMAINVER             = 520;
  IEMAINVERSION: string = '5.2.0';
  IEMAINDATEDD          = 20;
  IEMAINDATEMM          = 10;
  IEMAINDATEYY          = 2014;

  IEM_UPDATE   = WM_USER + 7000; // message to TImageEnView for Update

  // messages from TImageEnIO.DShowParams
  IEM_NEWFRAME = WM_USER + 7001;
  IEM_EVENT    = WM_USER + 7002;

  // async OnProgress/OnFinishWork messages
  IEM_PROGRESS   = WM_USER + 7003;
  IEM_FINISHWORK = WM_USER + 7004;

  IEWM_MOUSEHWHEEL = $020E;

  // Auto-scrolling in IEMView
  IEM_AUTOSCROLL = WM_User + 7005;

  // messages from TImageEnIO.MediaFoundation
  IEM_MEDIAFOUNDATION = WM_USER + 7006;


  IEPrint_Preview_Margin_Color = clSilver;


  Supported_AVI_File_Extensions = '*.avi;';
  {$ifdef IEINCLUDEDIRECTSHOW}
  Supported_MPEG_File_Extensions  = '*.mpe;*.mpg;*.mpeg;';
  Supported_WMV_File_Extensions   = '*.wmv;';
  {$ELSE}
  Supported_MPEG_File_Extensions  = '';
  Supported_WMV_File_Extensions   = '';
  {$ENDIF}
  Supported_Video_File_Extensions = Supported_AVI_File_Extensions + Supported_MPEG_File_Extensions + Supported_WMV_File_Extensions;

  ALL_KNOWN_EXPLORER_VIDEO_FORMATS = '*.AVI;*.MPG;*.MPE;*.MPEG;*.WMV;*.ASF;*.IVF;*.WM;*.MP4;*.MOV;*.QT;*.RM;*.M2TS;*.MTS;*.MOD;';

  Default_GIF_Animation_Delay_MS = 100; // 100 milliseconds: Default delay for animated images that do not have an explicit delay interval - in line with most browsers

  CM_per_Inch = 2.54;                   // For imperial to metric conversion

// defines IECMYKPROFILE const
{$WARNINGS OFF}
{$I table1.inc}
{$WARNINGS ON}

{$IFDEF UNICODE}
 IEDLLWNameExt = 'W';
{$ELSE}
 IEDLLWNameExt = 'A';
{$ENDIF}

{$ifdef IEOLDSEEKDEF}
 soCurrent = soFromCurrent;
 soBeginning = soFromBeginning;
 soEnd = soFromEnd;
{$endif}

type

{!!
<FS>EIEException

<FM>Declaration<FC>
EIEException = class(Exception);

<FM>Description<FC>
Generic ImageEn exception
!!} 
  EIEException = class(Exception);

{!!
<FS>TIEAntialiasMode

<FM>Declaration<FC>
TIEAntialiasMode = (ierNone, ierFast, ierBilinear, ierBicubic);

<FM>Description<FN>
Specifies the anti-aliasing algorithm that is used to improve quality when rotating images:
  ierNone : No anti-aliasing (lowest quality)
  ierFast : Fast but lower quality
  ierBilinear : Bilinear, high quality
  ierBicubic : Bicubic, highest quality
!!}
  TIEAntialiasMode = (ierNone, ierFast, ierBilinear, ierBicubic);

{!!
<FS>TFlipDir
              
<FM>Declaration<FC>
}
  TFlipDir = (fdHorizontal, fdVertical);
{!!}


{!!
<FS>TIEMagnifyStyle

<FM>Declaration<FC>
}
  TIEMagnifyStyle = (iemRectangle, iemEllipse);
{!!}


{!!
<FS>TIEMagnifySource

<FM>Declaration<FC>
TIEMagnifySource = (iemBackgroundLayer, iemCanvas);

<FM>Description<FN>
iemBackgroundLayer: gets data from the background layer.
iemCanvas: gets data from the painted background.

To magnify the layer 0 image by getting the pixels directly from the bitmap, use iemBackgroundLayer.
!!}
  TIEMagnifySource = (iemBackgroundLayer, iemCanvas);


{!!
<FS>TIELayerMagnification

<FM>Declaration<FC>
  TIELayerMagnification = record
    Enabled: boolean;
    Rate: double;
    Style: <A TIEMagnifyStyle>;
    Source: <A TIEMagnifySource>;
  end;

<FM>Description<FN>

<TABLE>
<R> <H>Field</H> <H>Description</H> </R>
<R> <C>Enabled</C> <C>If true this is a magnify layer. The bitmap of the layer is filled with the background zoomed according to Rate field. Valid only if <A TImageEnView.LayersSync>=False. This property doesn't apply to layer 0</C> </R>
<R> <C>Rate</C> <C>The rate of the magnify layer (magnification). Allowed values greather than 1. Valid only if Enabled=True and <A TImageEnView.LayersSync>=False. This property doesn't apply to layer 0</C> </R>
<R> <C>Style</C> <C>The magnify layer shape. Valid only if Enabled=True and <A TImageEnView.LayersSync>=False. This property doesn't apply to layer 0</C> </R>
<R> <C>Source</C> <C>The magnify layer source. </C> </R>
</TABLE>
!!}
  TIELayerMagnification = record
    Enabled: boolean;
    Rate: double; // must be >= 1
    Style: TIEMagnifyStyle;
    Source: TIEMagnifySource;
  end;


{!!
<FS>TIEColorSpace

<FM>Declaration<FC>
}
  TIEColorSpace = (iecmsRGB, iecmsBGR, iecmsCMYK, iecmsCMYK6, iecmsCIELab, iecmsGray8, iecmsRGB48, iecmsRGB48_SE, iecmsYCBCR);
{!!}



{!!
<FS>TIEFileDragDropActions

<FM>Declaration<FC>
TIEFileDragDropActions = set of (ieddCopy, ieddMove);

<FM>Description<FN>
Used by <A TImageEnFolderMView.AutoDragFiles> and <A TImageEnFolderMView.AutoDropFiles>.
!!}
  TIEFileDragDropActions = set of (iedaCopy, iedaMove);

  

{$ifdef FPC}
  TCMWantSpecialKey = TWMKey;
{$endif}


PBitmapInfoHeader256 = ^TBitmapInfoHeader256;

{!!
<FS>PPointArray

<FM>Declaration<FC>
PPointArray = ^<A TPointArray>;
!!}
  PPointArray = ^TPointArray;

{!!
<FS>PIEPointArray

<FM>Declaration<FC>
PIEPointArray = ^<A TIEPointArray>;
!!}
  PIEPointArray = ^TIEPointArray;

{!!
<FS>PPointerArray

<FM>Declaration<FC>
PPointerArray = ^<A TPointerArray>;
!!}
  PPointerArray = ^TPointerArray;


{!!
<FS>PDoubleArray

<FM>Declaration<FC>
PDoubleArray = ^<A TDoubleArray>;
!!}
  PDoubleArray = ^TDoubleArray;

{!!
<FS>PDWordArray

<FM>Declaration<FC>
PDWordArray = ^<A TDWordArray>;
!!}
  PDWordArray = ^TDWordArray;


{!!
<FS>PSingleArray

<FM>Declaration<FC>
PSingleArray = ^<A TSingleArray>;
!!}
  PSingleArray = ^TSingleArray;


{!!
<FS>PRGBA

<FM>Declaration<FC>
PRGBA = ^<A TRGBA>;
!!}
  PRGBA = ^TRGBA;



{!!
<FS>PIERGBAPalette

<FM>Declaration<FC>
PIERGBAPalette = ^<A TIERGBAPalette>;
!!}
  PIERGBAPalette = ^TIERGBAPalette;


{!!
<FS>PRGB

<FM>Declaration<FC>
type PRGB = ^<A TRGB>;
!!}
  PRGB = ^TRGB;


{!!
<FS>PIERGBPalette

<FM>Declaration<FC>
PIERGBPalette = ^<A TIERGBPalette>;
!!}
  PIERGBPalette = ^TIERGBPalette;


{!!
<FS>PCMYK

<FM>Declaration<FC>
PCMYK = ^<A TCMYK>;
!!}
  PCMYK = ^TCMYK;


{!!
<FS>PCIELAB

<FM>Declaration<FC>
PCIELAB = ^<A TCIELAB>;
!!}
  PCIELAB = ^TCIELAB;


{!!
<FS>PCIELABROW

<FM>Declaration<FC>
PCIELABROW = ^<A TCIELABROW>;
!!}
  PCIELABROW = ^TCIELABROW;


{!!
<FS>PYCBCR

<FM>Declaration<FC>
PYCBCR = ^<A TYCBCR>;
!!}
  PYCBCR = ^TYCBCR;


{!!
<FS>PCMYKROW

<FM>Declaration<FC>
PCMYKROW = ^<A TCMYKROW>;
!!}
  PCMYKROW = ^TCMYKROW;


{!!
<FS>PRGBROW

<FM>Declaration<FC>
type PRGBROW = ^<A RGBROW>;
!!}
  PRGBROW = ^RGBROW;

{!!
<FS>PRGB32ROW

<FM>Declaration<FC>
type PRGB32ROW = ^<A RGB32ROW>;
!!}
  PRGB32ROW = ^RGB32ROW;

{!!
<FS>PRGB48ROW

<FM>Declaration<FC>
PRGB48ROW = ^<A TRGB48ROW>;
!!}
  PRGB48ROW = ^TRGB48ROW;

{!!
<FS>PBYTEROW

<FM>Declaration<FC>
PBYTEROW = ^<A TBYTEROW>;
!!}
  PBYTEROW = ^TBYTEROW;

{!!
<FS>PBYTEROWS

<FM>Declaration<FC>
PBYTEROWS = ^<A TBYTEROWS>;
!!}
  PBYTEROWS = ^TBYTEROWS;

{!!
<FS>pIntegerArray

<FM>Declaration<FC>
pIntegerArray = ^<A IntegerArray>;
!!}
  pIntegerArray = ^IntegerArray;

{!!
<FS>pint64array

<FM>Declaration<FC>
pint64array = ^<A int64array>;
!!}
  pint64array = ^int64array;

{!!
<FS>PPRGBArray

<FM>Declaration<FC>
type PPRGBArray = ^<A PRGBArray>;
!!}
  PPRGBArray = ^PRGBArray;

{!!
<FS>pboolean

<FM>Declaration<FC>
}
  pboolean = ^boolean;
{!!}

{!!
<FS>TIEArrayOfDouble

<FM>Declaration<FC>
}
  TIEArrayOfDouble = array of double;
{!!}


{!!
<FS>PProgressRec

<FM>Declaration<FC>
PProgressRec = ^<A TProgressRec>;
!!}
  PProgressRec = ^TProgressRec;

  PIEDouble = ^double;


{$ifndef IEHASUINT64}
  uint64 = int64;
{$endif}
  

  // DRAWDIB TYPES
  hDrawDib = THandle;
  TDrawDibTime = record
    timeCount: LongInt;       // see below
    timeDraw: LongInt;        // time to draw bitmaps
    timeDecompress: LongInt;  // time to decompress bitmaps
    timeDither: LongInt;      // time to dither bitmaps
    timeStretch: LongInt;     // time to stretch bitmaps
    timeBlt: LongInt;         // time to transfer bitmaps (BitBlt)
    timeSetDIBits: LongInt;   // time to transfer bitmaps (SetDIBits)
  end;

  // TBitmapInfoHeader + palette 256 colors
  TBitmapInfoHeader256 = packed record
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
    Palette: array[0..1] of TRGBQUAD;
  end;

{!!
<FS>TPointArray

<FM>Declaration<FC>
}
  TPointArray = array[0..Maxint div 16] of Windows.TPoint;
{!!}


  // Why this? Because C++Builder doesn't want TPointArray or PPointArray for variable declarations (not function members)
{!!
<FS>TIEPoint

<FM>Declaration<FC>
}
  TIEPoint = record
    X: Longint;
    Y: Longint;
  end;
{!!}


{!!
<FS>TIE2DPoint

<FM>Declaration<FC>
}
  TIE2DPoint = record
    X: double;
    Y: double;
  end;
{!!}


{!!
<FS>TIE2DPointArray

<FM>Declaration<FC>
}
  TIE2DPointArray = array of TIE2DPoint;
{!!}


{!!
<FS>TIE3DPoint

<FM>Declaration<FC>
}
  TIE3DPoint = record
    x: double;
    y: double;
    z: double;
  end;
{!!}


{!!
<FS>TIEPointArray

<FM>Declaration<FC>
TIEPointArray = array[0..Maxint div 16] of <A TIEPoint>;
!!}
  TIEPointArray = array[0..Maxint div 16] of TIEPoint;


{!!
<FS>TIEQuadCoords

<FM>Declaration<FC>
}
TIEQuadCoords = record
  x0, y0: integer;
  x1, y1: integer;
  x2, y2: integer;
  x3, y3: integer;
end;
{!!}

{!!
<FS>TPointerArray

<FM>Declaration<FC>
}
  TPointerArray = array[0..maxint div 16] of pointer;
{!!}


{!!
<FS>TDoubleArray

<FM>Declaration<FC>
TDoubleArray = array[0..maxint div 16] of double;
!!}
  TDoubleArray = array[0..maxint div 16] of double;


{!!
<FS>TDWordArray

<FM>Declaration<FC>
TDWordArray = array[0..maxint div 16] of dword;
!!}
  TDWordArray = array[0..maxint div 16] of dword;


{!!
<FS>TSingleArray

<FM>Declaration<FC>
TSingleArray = array[0..maxint div 16] of single;
!!}
  TSingleArray = array[0..maxint div 16] of single;


{!!
<FS>TIESmallIntArray

<FM>Declaration<FC>
TIESmallIntArray = array[0..maxint div 16] of SmallInt;
!!}
  TIESmallIntArray = array[0..maxint div 16] of SmallInt;

{!!
<FS>PIESmallIntArray

<FM>Declaration<FC>
PIESmallIntArray = ^TIESmallIntArray;
!!}
  PIESmallIntArray = ^TIESmallIntArray;


{!!
<FS>TRGBA

<FM>Declaration<FC>
}
  TRGBA = packed record
    b: byte;
    g: byte;
    r: byte;
    a: byte;
  end;
{!!}

{!!
<FS>TRGB48

<FM>Declaration<FC>
}
  TRGB48 = packed record
    r: word;
    g: word;
    b: word;
  end;
{!!}


{!!
<FS>TRGBA48

<FM>Declaration<FC>
}
  TRGBA48 = packed record
    r: word;
    g: word;
    b: word;
    a: word;
  end;
{!!}


PRGBA48 = ^TRGBA48;

{!!
<FS>PRGB48

<FM>Declaration<FC>
PRGB48 = ^<A TRGB48>;
!!}
  PRGB48 = ^TRGB48;

  TIELine = record
    P, Q: TPoint;
  end;
  TIELineArray = array[0..8192] of TIELine;
  PIELineArray = ^TIELineArray;

{!!
<FS>TIERGBAPalette

<FM>Declaration<FC>
TIERGBAPalette = array[0..maxint div 16] of <A TRGBA>;
!!}
  TIERGBAPalette = array[0..maxint div 16] of TRGBA;

{!!
<FS>TRGB

<FM>Declaration<FC>
}
  TRGB = packed record
    b: byte;
    g: byte;
    r: byte;
  end;
{!!}


{!!
<FS>TIEArrayOfTRGB

<FM>Declaration<FC>
}
  TIEArrayOfTRGB = array of TRGB;
{!!}


{!!
<FS>TIERGBPalette

<FM>Declaration<FC>
TIERGBPalette = array[0..maxint div 16] of <A TRGB>;
!!}
  TIERGBPalette = array[0..maxint div 16] of TRGB;


{!!
<FS>TCMYK

<FM>Declaration<FC>
}
  TCMYK = packed record
    c: byte;
    m: byte;
    y: byte;
    k: byte;
  end;
{!!}

{!!
<FS>TCMYK16

<FM>Declaration<FC>
}
  TCMYK16 = packed record
    c: word;
    m: word;
    y: word;
    k: word;
  end;
{!!}

PCMYK16 = ^TCMYK16;


{!!
<FS>TCIELab

<FM>Declaration<FC>
}
  TCIELab = packed record
    l: byte;
    a: shortint;
    b: shortint;
  end;
{!!}

{!!
<FS>TYCbCr

<FM>Declaration<FC>
}
  TYCbCr = packed record
    y: byte;
    Cb: byte;
    Cr: byte;
  end;
{!!}


{!!
<FS>TCMYKROW

<FM>Declaration<FC>
TCMYKROW   = array[0..Maxint div 16] of <A TCMYK>;

!!}
  TCMYKROW   = array[0..Maxint div 16] of TCMYK;


{!!
<FS>TRGB48ROW

<FM>Declaration<FC>
TRGB48ROW  = array[0..Maxint div 16] of <A TRGB48>;
!!}
  TRGB48ROW  = array[0..Maxint div 16] of TRGB48;


{!!
<FS>TCIELABROW

<FM>Declaration<FC>
TCIELABROW = array[0..Maxint div 16] of <A TCIELAB>;
!!}
  TCIELABROW = array[0..Maxint div 16] of TCIELAB;



{!!
<FS>RGBROW

<FM>Declaration<FC>
type RGBROW = array[0 . . Maxint div 16] of <A TRGB>;
!!}
  RGBROW = array[0..Maxint div 16] of TRGB;

{!!
<FS>RGB32ROW

<FM>Declaration<FC>
type RGB32ROW = array[0 . . Maxint div 16] of <A TRGBA>;
!!}
  RGB32ROW = array[0..Maxint div 16] of TRGBA;


  // row of bytes
{!!
<FS>TBYTEROW

<FM>Declaration<FC>
}
  TBYTEROW = array[0..Maxint div 16] of byte;
{!!}


  // array of byte rows
{!!
<FS>TBYTEROWS

<FM>Declaration<FC>
}
  TBYTEROWS = array[0..Maxint div 16] of PBYTEROW;
{!!}


{!!
<FS>IntegerArray

<FM>Declaration<FC>
}
  IntegerArray = array[0..MaxInt div 16] of integer;
{!!}

{!!
<FS>int64array

<FM>Declaration<FC>
}
  int64array = array[0..MaxInt div 16] of int64;
{!!}


  // pointer tp array of pointers to TRGB (scanline rows)
{!!
<FS>PRGBArray

<FM>Declaration<FC>
type PRGBArray = array[0 . . Maxint div 16] of pRGB;
!!}
  PRGBArray = array[0..Maxint div 16] of pRGB;


{!!
<FS>TIEArrayOfByte

<FM>Declaration<FC>
}
  TIEArrayOfByte = array of byte;
{!!}


{!!
<FS>TIEArrayOfInteger

<FM>Declaration<FC>
}
  TIEArrayOfInteger = array of integer;
{!!}


{!!
<FS>TIEArrayOfDWord

<FM>Declaration<FC>
}
  TIEArrayOfDWord = array of dword;
{!!}



{!!
<FS>TIEProgressEvent

<FM>Declaration<FC>
type TIEProgressEvent = procedure(Sender: TObject; per: integer) of object;

<FM>Description<FN>
<FC>per<FN> is a value from 0 to 100 that indicates the percentage progress made.
!!}
  // Notify progress (per=percentage)
  TIEProgressEvent = procedure(Sender: TObject; per: integer) of object;


{!!
<FS>TIEJob

<FM>Declaration<FC>
}
  TIEJob = (iejNOTHING, // no job / end of work
    iejGENERALPROCESSING, // generic processing
    iejVIDEOCAP_CONNECTING // Video capture - connecting...
    );
{!!}

{!!
<FS>TIEJobEvent

<FM>Declaration<FC>
type TIEJobEvent = procedure(Sender: TObject; job: <A TIEJob>; per: integer) of object;

<FM>Description<FN>
A TIEJobEvent is called during ImageEn processing.

<FC>job<FN> is the job type in progress.
<FC>per<FN> is percentage of performed work related to job.

The <FC>job<FN> parameter can be one of these values:

iejNOTHING : No job / end of work
iejGENERALPROCESSING : generic job
iejVIDEOCAP_CONNECTING : video capture - connecting to video capture card
!!}
  TIEJobEvent = procedure(Sender: TObject; job: TIEJob; per: integer) of object;

{!!
<FS>TProgressRec

<FM>Declaration<FC>
}
  // helper structure for OnProgress
  TProgressRec = record
    fOnProgress: TIEProgressEvent;
    Sender: TObject;
    val: integer; // counter (per=trunc(per1*val))
    tot: integer;
    per1: double; // (100/maxval)
    per2: double;
    Aborting: pboolean;
  end;
{!!}


{!!
<FS>TResampleFilter

<FM>Declaration<FC>
TResampleFilter = (rfNone, rfTriangle, rfHermite, rfBell, rfBSpline, rfLanczos3, rfMitchell, rfNearest, rfLinear, rfFastLinear, rfBilinear, rfBicubic, rfProjectBW, rfProjectWB, rfWICNearestNeighbor, rfWICLinear, rfWICCubic, rfWICFant);

<FM>Description<FN>
If you need the best quality we suggest: rfHermite, rfBell, rfBSpline, rfLanczos3, rfMitchell, rfNearest, rfBilinear, rfBicubic

If you need speed we suggest: rfTriangle, rfLinear, rfFastLinear, rfWICNearestNeighbor, rfWICLinear, rfWICCubic, rfWICFant

For projects (white on black or black on white) we suggest: rfProjectBW and rfProjectWB

You can view the effect of the various filters in the FullApps\PhotoEn3 demo (see Tools > Zoom Properties)
!!}
  TResampleFilter = (rfNone, rfTriangle, rfHermite, rfBell, rfBSpline, rfLanczos3,
                     rfMitchell, rfNearest, rfLinear, rfFastLinear, rfBilinear,
                     rfBicubic, rfProjectBW, rfProjectWB, rfWICNearestNeighbor,
                     rfWICLinear, rfWICCubic, rfWICFant);


  THYIEGraphicHeader = record
    Count: Word; { Fixed at 1 }
    HType: Word; { Fixed at $0100 }
    Size: Longint; { Size not including header }
  end;

{!!
<FS>TMsgLanguage

<FM>Declaration<FC>
Type TMsgLanguage = (msSystem, msEnglish, msItalian, msGerman, msSpanish, msFrench, msPortuguese, msGreek, msRussian, msDutch, msSwedish, msPolish, msJapanese, msCzech, msFinnish, msFarsi, msChinese, msChineseTraditional, msDanish, msTurkish, msKorean, msHungarian, msArabic, msSerbian, msChineseTraditionalBig5, msNorwegian, msUser);

See also:
- <A LanguageToStr>
!!}
  TMsgLanguage = (
    msSystem
{$IFDEF IESUPPORTENGLISH}
    , msEnglish
{$ENDIF}
{$IFDEF IESUPPORTITALIAN}
    , msItalian
{$ENDIF}
{$IFDEF IESUPPORTGERMAN}
    , msGerman
{$ENDIF}
{$IFDEF IESUPPORTSPANISH}
    , msSpanish
{$ENDIF}
{$IFDEF IESUPPORTFRENCH}
    , msFrench
{$ENDIF}
{$IFDEF IESUPPORTPORTUGUESE}
    , msPortuguese
{$ENDIF}
{$IFDEF IESUPPORTGREEK}
    , msGreek
{$ENDIF}
{$IFDEF IESUPPORTRUSSIAN}
    , msRussian
{$ENDIF}
{$IFDEF IESUPPORTDUTCH}
    , msDutch
{$ENDIF}
{$IFDEF IESUPPORTSWEDISH}
    , msSwedish
{$ENDIF}
{$IFDEF IESUPPORTPOLISH}
    , msPolish
{$ENDIF}
{$IFDEF IESUPPORTJAPANESE}
    , msJapanese
{$ENDIF}
{$IFDEF IESUPPORTCZECH}
    , msCzech
{$ENDIF}
{$IFDEF IESUPPORTFINNISH}
    , msFinnish
{$ENDIF}
{$IFDEF IESUPPORTFARSI}
    , msFarsi
{$ENDIF}
{$IFDEF IESUPPORTCHINESE}
    , msChinese
    , msChineseTraditional
    , msChineseTraditionalBig5
{$ENDIF}
{$IFDEF IESUPPORTDANISH}
    , msDanish
{$ENDIF}
{$IFDEF IESUPPORTTURKISH}
    , msTurkish
{$ENDIF}
{$IFDEF IESUPPORTKOREAN}
    , msKorean
{$ENDIF}
{$IFDEF IESUPPORTHUNGARIAN}
    , msHungarian
{$ENDIF}
{$IFDEF IESUPPORTARABIC}
    , msArabic
{$ENDIF}
{$IFDEF IESUPPORTSERBIAN}
    , msSerbian
{$ENDIF}
{$IFDEF IESUPPORTNORWEGIAN}
    , msNorwegian
{$ENDIF}
{$IFDEF IESUPPORTUSER}
    , msUser
{$ENDIF}
    );

{!!
<FS>TMsgLanguageWords

<FM>Declaration<FC>
}
  TMsgLanguageWords = (IEMSG_PREVIEW, IEMSG_SOURCE, IEMSG_RESULT, IEMSG_OK, IEMSG_CANCEL,
    IEMSG_LOCKPREVIEW, IEMSG_COPYRESULTTOSOURCE, IEMSG_CONTRAST, IEMSG_BRIGHTNESS,
    IEMSG_HUE, IEMSG_SATURATION, IEMSG_VALUE, IEMSG_BASECOLOR, IEMSG_NEWCOLOR,
    IEMSG_LUMINOSITY, IEMSG_RED, IEMSG_GREEN, IEMSG_BLUE, IEMSG_FILTERVALUES, IEMSG_PRESETS,
    IEMSG_DIVISOR, IEMSG_LOAD, IEMSG_SAVE, IEMSG_EQUALIZATION, IEMSG_THRESHOLD, IEMSG_EQUALIZE,
    IEMSG_HISTOGRAM, IEMSG_GRAY, IEMSG_LIGHT, IEMSG_LEFT, IEMSG_TOP, IEMSG_WIDTH, IEMSG_HEIGHT,
    IEMSG_COLOR, IEMSG_SOURCEIMAGEQUANTITY, IEMSG_LENS, IEMSG_REFRACTION, IEMSG_PARAMETERSPREVIEW,
    IEMSG_QUALITY, IEMSG_DCTMETHOD, IEMSG_SMOOTHINGFACTOR, IEMSG_GRAYSCALE, IEMSG_OPTIMALHUFFMAN,
    IEMSG_ORIGINALSIZE, IEMSG_COMPRESSEDSIZE, IEMSG_ADVANCED, IEMSG_PROGRESSIVE, IEMSG_COMPRESSION,
    IEMSG_IMAGEINDEX, IEMSG_PHOTOMETRIC, IEMSG_SCANNDEDDOCUMENTINFO, IEMSG_NAME, IEMSG_DESCRIPTION,
    IEMSG_PAGENAME, IEMSG_PAGENUMBER, IEMSG_OF, IEMSG_HORIZPOSITIONINCH, IEMSG_VERTPOSITIONINCH,
    IEMSG_COLORS, IEMSG_TRANSPARENT, IEMSG_TRANSPARENTCOLOR, IEMSG_INTERLACED, IEMSG_BACKGROUND,
    IEMSG_HORIZPOSITION, IEMSG_VERTPOSITION, IEMSG_DELAYTIME, IEMSG_FILTER, IEMSG_WAVE,
    IEMSG_AMPLITUDE, IEMSG_WAVELENGTH, IEMSG_PHASE, IEMSG_REFLECTIVE, IEMSG_USERFILTER,
    IEMSG_MORPHFILTER, IEMSG_WINDOWSIZE, IEMSG_MAXIMUM, IEMSG_MINIMUM, IEMSG_OPEN,
    IEMSG_CLOSE, IEMSG_ROTATE, IEMSG_FLIP, IEMSG_FLIPHOR, IEMSG_FLIPVER,
    IEMSG_FREQUENCYDOMAINIMAGE, IEMSG_SELECTTHEREGIONTOCLEAR, IEMSG_CLEAR, IEMSG_RESET,
    IEMSG_ANIMATE, IEMSG_LOADFILTER, IEMSG_SAVEFILTER, IEMSG_BUMPMAP, IEMSG_PRINT, IEMSG_MARGINS, IEMSG_INCHES,
    IEMSG_RIGHT, IEMSG_BOTTOM, IEMSG_POSITION, IEMSG_SIZE, IEMSG_NORMAL, IEMSG_FITTOPAGE, IEMSG_STRETCHTOPAGE,
    IEMSG_SPECIFIEDSIZE, IEMSG_GAMMACORRECTION, IEMSG_VALUE2, IEMSG_PRINTSETUP, IEMSG_LEFTMARGIN, IEMSG_TOPMARGIN,
    IEMSG_RIGHTMARGIN, IEMSG_BOTTOMMARGIN, IEMSG_LOCATIONSIZE, IEMSG_TOPLEFT, IEMSG_TOPCENTER, IEMSG_TOPRIGHT,
    IEMSG_CENTERLEFT, IEMSG_CENTER, IEMSG_CENTERRIGHT, IEMSG_BOTTOMLEFT, IEMSG_BOTTOMCENTER, IEMSG_BOTTOMRIGHT,
    IEMSG_CLOSE2, IEMSG_APPLY, IEMSG_MEASUREUNITS, IEMSG_UNITS, IEMSG_RATE,
    IEMSG_ALLGRAPHICS, IEMSG_VIDEOFORWINDOWS, IEMSG_FILE, IEMSG_MEM, IEMSG_LOCKPREVIEWHINT,
    IEMSG_PRINTALL, IEMSG_PRINTSELECTED, IEMSG_ALLCOMMONGRAPHICFILES, IEMSG_ALLFILES,
    IEMSG_HSV, IEMSG_HSL, IEMSG_RGB, IEMSG_FFT, IEMSG_SHARPEN, IEMSG_CHANNELS,
    IEMSG_PIXELS, IEMSG_FRAMES, IEMSG_BIT, IEMSG_BYTE, IEMSG_DPI, IEMSG_KB, IEMSG_FIT, IEMSG_OTHER,
    IEMSG_COLUMNS, IEMSG_ROWS, IEMSG_STYLE, IEMSG_SPACING, IEMSG_SELECTACQUIREDEVICE, IEMSG_SELECTANACQUISITIONDEVICE,
    IEMSG_Page_X_of_X, IEMSG_X_Pages, IEMSG_Previous, IEMSG_Next, IEMSG_THUMBNAILS,
    IEMsg_Acquire,
    IEMsg_AddFromFile,
    IEMsg_AddLayer,
    IEMsg_AdjustColors,
    IEMsg_AutoEqualizeColors,
    IEMsg_AutoRotateDisplay,
    IEMsg_AutoShrink,
    IEMsg_AutoStretch,
    IEMsg_BringForward,
    IEMsg_BringToFront,
    IEMsg_CircularSelect,
    IEMsg_ConvertToGray,
    IEMsg_CopyImage,
    IEMsg_CopySelection,
    IEMsg_Copy,
    IEMsg_CropImageToObjects,
    IEMsg_CropToSelection,
    IEMsg_CropTransparency,
    IEMsg_CutSelection,
    IEMsg_Cut,
    IEMsg_DeleteAll,
    IEMsg_Delete,
    IEMsg_DeselectAll,
    IEMsg_Deselect,
    IEMsg_EditPolyline,
    IEMsg_FirstFrame,
    IEMsg_FitImageToHeight,
    IEMsg_FitImageToWidth,
    IEMsg_FitImage,
    IEMsg_FlipHorizontal,
    IEMsg_FlipVertical,
    IEMsg_ImageEffects,
    IEMsg_ImageSaveProperties,
    IEMsg_InsertAngle,
    IEMsg_InsertEllipse,
    IEMsg_InsertImage,
    IEMsg_InsertLabelWithLine,
    IEMsg_InsertLine,
    IEMsg_InsertMemo,
    IEMsg_InsertPolyline,
    IEMsg_InsertRectangle,
    IEMsg_InsertRuler,
    IEMsg_InsertText,
    IEMsg_LassoSelect,
    IEMsg_LastFrame,
    IEMsg_LoadFileAsLayer,
    IEMsg_LoopPlayback,
    IEMsg_MeasureArea,
    IEMsg_MeasureDistance,
    IEMsg_MeasureLength,
    IEMsg_MergeAllLayers,
    IEMsg_MergeAllToBackground,
    IEMsg_MergeToBackground,
    IEMsg_MoveLayers,
    IEMsg_Negative,
    IEMsg_NextFrame,
    IEMsg_PasteAsLayer,
    IEMsg_Paste,
    IEMsg_PolygonSelect,
    IEMsg_PreviousFrame,
    IEMsg_PrintAllThumbnails,
    IEMsg_PrintPreview,
    IEMsg_PrintThumbnails,
    IEMsg_PrintToPage,
    IEMsg_RectangularSelect,
    IEMsg_Redo,
    IEMsg_RemoveLayer,
    IEMsg_RemoveRedEyes,
    IEMsg_ResizeLayers,
    IEMsg_Rotate180,
    IEMsg_RotateLayers,
    IEMsg_RotateLeft,
    IEMsg_RotateRight,
    IEMsg_SaveAs,
    IEMsg_SaveProperties,
    IEMsg_ScrollImage,
    IEMsg_ScrollToCursor,
    IEMsg_SelectAcquisitionSource,
    IEMsg_SelectAll,
    IEMsg_SelectByColor,
    IEMsg_Select,
    IEMsg_SendBackward,
    IEMsg_SendToBack,
    IEMsg_SingleFrameOnly,
    IEMsg_Undo,
    IEMsg_Unstamp,
    IEMsg_ZoomIn,
    IEMsg_ZoomOut,
    IEMsg_ZoomToFullSize,
    IEMsg_ZoomToSelection,
    IEMsg_Zoom,
    IEMsg_AddALineObject,
    IEMsg_AddAMultiLineTextObject,
    IEMsg_AddANewLabelObjectWithALine,
    IEMsg_AddANewLayerToThisImage,
    IEMsg_AddARulerObject,
    IEMsg_AddATextObject,
    IEMsg_AddAnAngleMeasurementObject,
    IEMsg_AddAnEllipicalObject,
    IEMsg_AddAnImageObject,
    IEMsg_AddAnImageToTheGridFromFile,
    IEMsg_AddAnRectangularObject,
    IEMsg_ApplyASharpeningFilterToTheImage,
    IEMsg_AutomaticallyDisplayImageWithTheCorrectOrientation,
    IEMsg_AutomaticallyDisplayImagesWithTheCorrectOrientation,
    IEMsg_BringTheSelectedLayerToTheFrontOfAllOthers,
    IEMsg_BringTheSelectedObjectToTheFrontOfAllOtherObjects,
    IEMsg_ClearAllImages,
    IEMsg_ClearTheWindow,
    IEMsg_ClearThisImage,
    IEMsg_ClearYourSelection,
    IEMsg_ClickTheImageAndDragTheMouseToScroll,
    IEMsg_ContinouslyLoopThePlaybackOfAGIFOrAVIFile,
    IEMsg_CopyImageToTheClipboard,
    IEMsg_CopyTheCurrentImageOrSelectionToTheClipboard,
    IEMsg_CopyTheCurrentImageToTheClipboardAndRemoveIt,
    IEMsg_CopyTheCurrentImageToTheClipboard,
    IEMsg_CopyTheCurrentSelectionToTheClipboardAndRemoveItFromTheImage,
    IEMsg_CopyTheCurrentSelectionToTheClipboard,
    IEMsg_CopyTheSelectedObjectToTheClipboard,
    IEMsg_DeselectAllObjects,
    IEMsg_DisplayAPreviewOfThisImageForPrinting,
    IEMsg_DisplayAPreviewOfYourPrinting,
    IEMsg_DisplayLargeImagesAtTheWindowSize,
    IEMsg_DisplayOnlyTheActiveFrame,
    IEMsg_DisplaySmallImagesAtTheWindowSize,
    IEMsg_DisplayTheAnimationOfAGIFOrAVIFile,
    IEMsg_DisplayTheFirstFrameOfThisImage,
    IEMsg_DisplayTheImageAtFullSize,
    IEMsg_DisplayTheImageAtTheHeightOfTheWindow,
    IEMsg_DisplayTheImageAtTheSizeOfTheWindow,
    IEMsg_DisplayTheImageAtTheWidthOfTheWindow,
    IEMsg_DisplayTheImageLarger,
    IEMsg_DisplayTheImageSmaller,
    IEMsg_DisplayTheLastFrameOfThisImage,
    IEMsg_DisplayTheNextFrameOfThisImage,
    IEMsg_DisplayThePriorFrameOfThisImage,
    IEMsg_DynamicallyMeasureADistance,
    IEMsg_EditPointsOfAPolyline,
    IEMsg_EqualizesTheColorHistogramForTheSelectedRegion,
    IEMsg_FlipTheImageFromLeftToRight,
    IEMsg_FlipTheImageFromTopToBottom,
    IEMsg_FlipTheSelectedImageFromLeftToRight,
    IEMsg_FlipTheSelectedImageFromTopToBottom,
    IEMsg_FreeHandPaintAnOpenPolygon,
    IEMsg_InvertTheColorsOfTheImage,
    IEMsg_LeftClickTheImageToZoomInRightClickToZoomOut,
    IEMsg_LoadAnImageFromFile,
    IEMsg_MatchTheSizeOfTheBackgroundImageToAllOfTheObjectsItContains,
    IEMsg_MeasureARectangularArea,
    IEMsg_MeasureTheDistanceBetweenTwoPoints,
    IEMsg_MergeAllObjectsWithTheBackgroundLayer,
    IEMsg_MergeTheSelectedObjectWithTheBackgroundLayer,
    IEMsg_MergesAllLayersOfTheImageIntoASingleOne,
    IEMsg_MoveTheMouseToScrollTheImage,
    IEMsg_MoveTheSelectedLayerBackward,
    IEMsg_MoveTheSelectedLayerForward,
    IEMsg_MoveTheSelectedObjectBackward,
    IEMsg_MoveTheSelectedObjectForward,
    IEMsg_MoveTheSelectedObjectToTheClipboard,
    IEMsg_PasteAnImageFromTheClipboard,
    IEMsg_PasteObjectFromTheClipboard,
    IEMsg_PasteTheContentOfTheClipboardAsANewLayer,
    IEMsg_PerformColorEnhancementFunctionsOnTheImage,
    IEMsg_PerformColorEnhancementFunctionsOnTheSelectedImage,
    IEMsg_PerformAnIrregularSelectionOfYourImageByClickingTheMouse,
    IEMsg_PerformAnIrregularSelectionOfYourImageByDraggingTheMouse,
    IEMsg_PerformEffectsOnTheImage,
    IEMsg_PerformEffectsOnTheSelectedImage,
    IEMsg_PlaybackTheseFramesInSequence,
    IEMsg_PositionTheSelectedLayerBehindAllOthers,
    IEMsg_PositionTheSelectedObjectBehindAllOtherObjects,
    IEMsg_PrintAllImagesOfTheGridAsASheetOfThumbnails,
    IEMsg_PrintTheSelectedImageAtItsOriginalSize,
    IEMsg_PrintTheSelectedImageToFitThePage,
    IEMsg_PrintTheSelectedImagesAsASheetOfThumbnails,
    IEMsg_PrintThisImageAtItsOriginalSize,
    IEMsg_PrintThisImageToFitThePage,
    IEMsg_PromptForAnImageFileToLoadAsANewLayer,
    IEMsg_RedoTheLastActionThatWasUndone,
    IEMsg_ReduceTheColorsOfTheImageToGrayscale,
    IEMsg_RemoveAllObjectsFromTheImage,
    IEMsg_RemoveAllPartsOfTheImageOutsideTheCurrentSelection,
    IEMsg_RemoveTheRedEyeEffectFromTheSelection,
    IEMsg_RemoveTheSelectedImage,
    IEMsg_RemoveTheSelectedLayerFromTheImage,
    IEMsg_RemoveTheSelectedObjectFromTheImage,
    IEMsg_RemoveTheTransparentBordersFromTheSelectedLayer,
    IEMsg_RestartPlaybackAfterItCompletes,
    IEMsg_RetrieveAnImageFromACameraOrScanner,
    IEMsg_RetrieveImagesFromACameraOrScanner,
    IEMsg_RotateTheImage180Clockwise,
    IEMsg_RotateTheImage90Clockwise,
    IEMsg_RotateTheImage90CounterClockwise,
    IEMsg_RotateTheSelectedImage180Clockwise,
    IEMsg_RotateTheSelectedImage90Clockwise,
    IEMsg_RotateTheSelectedImage90CounterClockwise,
    IEMsg_RotateTheSelectedObject180Clockwise,
    IEMsg_RotateTheSelectedObject90Clockwise,
    IEMsg_RotateTheSelectedObject90CounterClockwise,
    IEMsg_SaveChangesToThisImageToFile,
    IEMsg_SaveThisImageToANewFilename,
    IEMsg_SelectACircularAreaOfYourImage,
    IEMsg_SelectAPortionOfYourImageOfASimilarColor,
    IEMsg_SelectARectangularAreaOfYourImage,
    IEMsg_SelectAllImagesInTheGrid,
    IEMsg_SelectAllObjectsOfImage,
    IEMsg_SelectAnAreaOfTheImageToZoomInto,
    IEMsg_SelectOrResizeAnObject,
    IEMsg_SelectTheCameraOrScannerToAcquireImagesFrom,
    IEMsg_SelectTheFirstFrame,
    IEMsg_SelectTheLastFrame,
    IEMsg_SelectTheNextFrame,
    IEMsg_SelectThePreviousFrame,
    IEMsg_SingleClickingWillNotInsertANewObject,
    IEMsg_SpecifyAdvancedPropertiesForAllImages,
    IEMsg_SpecifyAdvancedPropertiesForTheSelectedImage,
    IEMsg_SpecifyAdvancedPropertiesForThisImage,
    IEMsg_UndoTheLastAction,
    IEMsg_UndoTheLastEdit,
    IEMsg_UseTheMouseToMoveImageLayers,
    IEMsg_UseTheMouseToResizeImageLayers,
    IEMsg_UseTheMouseToRotateImageLayers,
    IEMsg_ZoomImage                 ,
    IEMsg_DisplayImageAtCustomZoom  ,
    IEMsg_CustomRotate              ,
    IEMsg_RotateImageByACustomAngle ,
    IEMsg_ResizeImage               ,
    IEMsg_SpecifyANewSizeForTheImage,
    IEMSG_RESIZE,
    IEMSG_CURRENT,
    IEMSG_NEW,
    IEMSG_SCALE,
    IEMSG_MAINTAINASPECTRATIO,
    IEMSG_Folder,
    IEMSG_Dimensions,
    IEMSG_Type,
    IEMSG_Created,
    IEMSG_Modified                               ,
    IEMsg_CopyFiles                              ,
    IEMsg_CopyFilesToClipboard                   ,
    IEMsg_CopyTheSelectedFilesToANewFolder       ,
    IEMsg_CopyTheSelectedFilesToTheClipboard     ,
    IEMsg_CutFilesToClipboard                    ,
    IEMsg_CutTheSelectedFilesToTheClipboard      ,
    IEMsg_DeleteFiles                            ,
    IEMsg_DeleteTheSelectedFilesFromTheFolder    ,
    IEMsg_GoUp                                   ,
    IEMsg_MoveFiles                              ,
    IEMsg_MoveTheSelectedFilesToANewFolder       ,
    IEMsg_OpenFile                               ,
    IEMsg_OpenTheParentOfTheCurrentFolder        ,
    IEMsg_OpenTheSelectedFileInTheDefaultViewer  ,
    IEMsg_PasteFilesFromClipboard                ,
    IEMsg_PasteFilesFromTheClipboardToThisFolder ,
    IEMsg_Refresh                                ,
    IEMsg_RefreshTheFileListing                  ,
    IEMsg_WhereDoYouWantToMoveTheseFiles         ,
    IEMsg_WhereDoYouWantToCopyTheseFiles         ,
    IEMsg_XIsNotAValidFolder                     ,
    IEMsg_RenameFile                             ,
    IEMsg_SpecifyANewNameForTheSelectedFile      ,
    IEMsg_SpecifyANewNameForX                    ,
    IEMsg_SelectAFolderToOpen                    ,
    IEMsg_SelectFolder                           ,
    IEMsg_CreateFolder                           ,
    IEMsg_CreateANewFolderAtTheCurrentLocation   ,
    IEMsg_SpecifyTheNameOfYourNewFolder          ,
    IEMsg_SoftShadow                             ,
    IEMsg_AddSoftShadow                          ,
    IEMsg_Radius                                 ,
    IEMsg_Offset                                 ,
    IEMsg_All_Glow                               );

{!!}

type
  TIELANGUAGECHARINFO = record
    CharSet: TFontCharset;
    FontName: string;  // empty string means predefined
    CodePage: word;
  end;

const

  IELANGUAGECHARINFO: array [TMsgLanguage] of TIELANGUAGECHARINFO = (
      (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 0)
    {$IFDEF IESUPPORTENGLISH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTITALIAN}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTGERMAN}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTSPANISH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTFRENCH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTPORTUGUESE}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTGREEK}
    , (CharSet: GREEK_CHARSET; FontName: ''; CodePage: 1253)
    {$ENDIF}
    {$IFDEF IESUPPORTRUSSIAN}
    , (CharSet: RUSSIAN_CHARSET; FontName: ''; CodePage: 1251)
    {$ENDIF}
    {$IFDEF IESUPPORTDUTCH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTSWEDISH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTPOLISH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1250)
    {$ENDIF}
    {$IFDEF IESUPPORTJAPANESE}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 932)
    {$ENDIF}
    {$IFDEF IESUPPORTCZECH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1250)
    {$ENDIF}
    {$IFDEF IESUPPORTFINNISH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTFARSI}
    , (CharSet: ARABIC_CHARSET; FontName: 'Tahoma'; CodePage: 1256)
    {$ENDIF}
    {$IFDEF IESUPPORTCHINESE}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 936) // Vhinese
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 950) // Chinese Trad.   
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 950) // Chinese Trad. Big5
    {$ENDIF}
    {$IFDEF IESUPPORTDANISH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    {$IFDEF IESUPPORTTURKISH}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1254)
    {$ENDIF}
    {$IFDEF IESUPPORTKOREAN}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 949)
    {$ENDIF}
    {$IFDEF IESUPPORTHUNGARIAN}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1250)
    {$ENDIF}
    {$IFDEF IESUPPORTARABIC}
    , (CharSet: DEFAULT_CHARSET; FontName: 'Tahoma'; CodePage: 1256)
    {$ENDIF}
    {$IFDEF IESUPPORTSERBIAN}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1250)
    {$ENDIF}
    {$IFDEF IESUPPORTNORWEGIAN}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252) 
    {$ENDIF}
    {$IFDEF IESUPPORTUSER}
    , (CharSet: DEFAULT_CHARSET; FontName: ''; CodePage: 1252)
    {$ENDIF}
    );


  IESELBREAK = $FFFFF; // selection break code

  // TIFF tags
  IETIFFTAG_TRANSFERFUNC  = 301;
  IETIFFTAG_COLORMAP      = 320;
  IETIFFTAG_SUBIFD        = 330;
  IETIFFTAG_XMP           = 700;
  IETIFFTAG_WANGIMAGING   = 32932;
  IETIFFTAG_COPYRIGHT     = 33432;
  IETIFFTAG_IPTC          = 33723;
  IETIFFTAG_PHOTOSHOP     = 34377;
  IETIFFTAG_EXIFIFD       = 34665;
  IETIFFTAG_ICC           = 34675;
  IETIFFTAG_EXIFGPSIFD    = 34853;
  IETIFFTAG_EPSTANDARD    = 37398;
  IETIFFTAG_INTEROPIFD    = 40965;
  IETIFFTAG_DNGVERSION    = 50706;

  // EXIF tags
  IETIFFTAG_EXIFMAKERNOTE = $927C;

  // TIFF types
  IETIFFTYPE_BYTE       = 1;
  IETIFFTYPE_ASCII      = 2;
  IETIFFTYPE_SHORT      = 3;
  IETIFFTYPE_LONG       = 4;
  IETIFFTYPE_RATIONAL   = 5;
  IETIFFTYPE_SBYTE      = 6;
  IETIFFTYPE_UNDEFINED  = 7;
  IETIFFTYPE_SSHORT     = 8;
  IETIFFTYPE_SLONG      = 9;
  IETIFFTYPE_SRATIONAL  = 10;
  IETIFFTYPE_FLOAT      = 11;
  IETIFFTYPE_DOUBLE     = 12;
  IETIFFTYPE_IFDPOINTER = 13;  // 4 bytes pointer to IFD (the same of LONG)
  IETIFFTYPE_UNICODE    = 14;
  IETIFFTYPE_COMPLEX    = 15;
  IETIFFTYPE_LONG8      = 16;  // BIGTIFF uint64
  IETIFFTYPE_SLONG8     = 17;  // BIGTIFF int64
  IETIFFTYPE_IFD8       = 18;  // BIGTIFF 8 bytes pointer to IFD

  // YUV formats
  IEBI_IYU1 = $31555949;
  IEBI_IYU2 = $32555949;
  IEBI_UYVY = $59565955;
  IEBI_UYNV = $564E5955;
  IEBI_cyuv = $76757963;
  IEBI_YUY2 = $32595559;
  IEBI_YUNV = $564E5559;
  IEBI_YVYU = $55595659;
  IEBI_Y41P = $50313459;
  IEBI_Y211 = $31313259;
  IEBI_Y41T = $54313459;
  IEBI_Y42T = $54323459;
  IEBI_CLJR = $524A4C43;
  IEBI_YVU9 = $39555659;
  IEBI_IF09 = $39304649;
  IEBI_YV12 = $32315659;
  IEBI_I420 = $30323449;
  IEBI_IYUV = $56555949;
  IEBI_CLPL = $4C504C43;

  function LanguageToStr(ALanguage : TMsgLanguage) : string;

var

  ieMessages: array[TMsgLanguage] of array[TMsgLanguageWords] of string;

  gAVIFILEinit: boolean; // true if AVIFILE library has been initialized




{!!
<FS>iegDialogsBackground

<FM>Declaration<FC>
iegDialogsBackground: (iedbDefault, iedbPaper, iedbMetal);

<FM>Description<FN>
Specifies the default dialogs (image processing, printing, ..) background style. This is not the image background, but actually the dialog background.
!!}
  iegDialogsBackground: (iedbDefault, iedbPaper, iedbMetal);


type

  IIESequentialStream = interface(IUnknown)
    ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
    function Read(pv: pointer; cb: dword; pcbRead: PDWORD): HResult; stdcall;
    function Write(pv: pointer; cb: dword; pcbWritten: PDWORD): HResult; stdcall;
  end;

  IE_STATSTG = record
  end;

  IIEStream = interface(IIESequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function Seek(dlibMove: int64; dwOrigin: dword; var plibNewPosition: int64): HResult; stdcall;
    function SetSize(libNewSize: int64): HResult; stdcall;
    function CopyTo(pstm: IIEStream; cb: int64; var pcbRead: int64; var pcbWritten: int64): HResult; stdcall;
    function Commit(grfCommitFlags: dword): HResult; stdcall;
    function Revert(): HResult; stdcall;
    function LockRegion(libOffset: int64; cb: int64; dwLockType: dword): HResult; stdcall;
    function UnlockRegion(libOffset: int64; cb: int64; dwLockType: dword): HResult; stdcall;
    function Stat(var pstatstg: IE_STATSTG; grfStatFlag: dword): HResult; stdcall;
    function Clone(var ppstm: IIEStream): HResult; stdcall;
  end;


{!!
<FS>TIERectangle

<FM>Declaration<FC>
TIERectangle = packed record
  x, y: integer;
  width, height: integer;
end;

<FM>Description<FN>
Describes a rectangle in terms of top-left, top-right coordinates and sizes.

See also:
- <A IERectangle>
!!}
  TIERectangle = packed record
    x, y: integer;
    width, height: integer;
  end;


{!!
<FS>TIEBackgroundStyle

<FM>Declaration<FC>
TIEBackgroundStyle = (iebsSolid, iebsHorizontal, iebsVertical, iebsFDiagonal, iebsBDiagonal, iebsCross, iebsDiagCross, iebsChessboard, iebsDiagonals, iebsCropped, iebsCropShadow, iebsGradient, iebsSoftShadow, iebsPhotoLike);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iebsCropped</C> <C>A 3D border is shown around the image (instead of around the edge of the ImageEnView). Best used with ImageEnView.BorderStyle=bsNone and <A TImageEnView.Center>=True</C> </R>
<R> <C>iebsCropShadow</C> <C>A solid shadow is drawn around the image (See also: iebsSoftShadow)</C> </R>
<R> <C>iebsChessboard</C> <C>Background has chessboard pattern</C> </R>
<R> <C>iebsDiagonals</C> <C>Background has diagonal pattern</C> </R>
<R> <C>iebsSolid</C> <C>Background is a solid color (see <A TImageEnView.Background>)</C> </R>
<R> <C>iebsHorizontal</C> <C>Same as bsHorizontal of TBrushStyle</C> </R>
<R> <C>iebsVertical</C> <C>Same as bsVertical of TBrushStyle</C> </R>
<R> <C>iebsFDiagonal</C> <C>Same as bsFDiagonal of TBrushStyle</C> </R>
<R> <C>iebsBDiagonal</C> <C>Same as bsBDiagonal of TBrushStyle</C> </R>
<R> <C>iebsCross</C> <C>Same as bsCross of TBrushStyle</C> </R>
<R> <C>iebsDiagCross</C> <C>Same as bsDiagCross of TBrushStyle</C> </R>
<R> <C>iebsGradient</C> <C>Background is a gradient of two colors (specified with <A TImageEnView.Background> and <A TImageEnView.GradientEndColor>)</C> </R>
<R> <C>iebsSoftShadow</C> <C>A soft (gaussian) shadow is drawn around the image</C> </R>
<R> <C>iebsPhotoLike</C> <C>A chessboard is drawn inside the image with a black border around</C> </R>
</TABLE>
!!}
  TIEBackgroundStyle = (iebsSolid, iebsHorizontal, iebsVertical, iebsFDiagonal, iebsBDiagonal, iebsCross, iebsDiagCross, iebsChessboard, iebsDiagonals, iebsCropped, iebsCropShadow, iebsGradient, iebsSoftShadow, iebsPhotoLike);


{!!
<FS>TIEMemoShortCut

<FM>Declaration<FC>
}
  TIEMemoShortCut = (iesLEFTALIGN, iesCENTERALIGN, iesRIGHTALIGN, iesJUSTIFIED, iesCOPY, iesCUT, iesPASTE, iesFONTSELECT, iesBOLD, iesITALIC, iesUNDERLINE, iesBACKCOLORSELECT);
{!!}

{!!
<FS>TIEMemoShortCuts

<FM>Declaration<FC>
TIEMemoShortCuts = array [<A TIEMemoShortCut>] of TShortCut;
!!}
  TIEMemoShortCuts = array [TIEMemoShortCut] of TShortCut;


{!!
<FS>TIEUnits

<FM>Declaration<FC>
type TIEUnits = (ieuPIXELS, ieuINCHES, ieuKM, ieuMETERS, ieuCENTIMETERS, ieuMILLIMETERS, ieuMICRONS, ieuNANOMETERS, ieuFEET, ieuYARDS, ieuMILES);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ieuPIXELS</C> <C>Pixels</C> </R>
<R> <C>ieuINCHES</C> <C>Inches</C> </R>
<R> <C>ieuKM</C> <C>Kilometers</C> </R>
<R> <C>ieuMETERS</C> <C>Meters</C> </R>
<R> <C>ieuCENTIMETERS</C> <C>Centimeters</C> </R>
<R> <C>ieuMILLIMETERS</C> <C>Millimeters</C> </R>
<R> <C>ieuMICRONS</C> <C>Microns</C> </R>
<R> <C>ieuNANOMETERS</C> <C>Nanometers</C> </R>
<R> <C>ieuFEET</C> <C>Feet</C> </R>
<R> <C>ieuYARDS</C> <C>Yards</C> </R>
<R> <C>ieuMILES</C> <C>Miles</C> </R>
</TABLE>
!!}
  TIEUnits = (ieuPIXELS, ieuINCHES, ieuKM, ieuMETERS, ieuCENTIMETERS, ieuMILLIMETERS, ieuMICRONS, ieuNANOMETERS, ieuFEET, ieuYARDS, ieuMILES);

{!!
<FS>TIEMeasureUnits

<FM>Declaration<FC>
}
  TIEMeasureUnits = array [TIEUnits] of string;
{!!}


  TGIFLZWCompFunc    = procedure(Stream: TStream; Height, Width: integer; Interlaced: boolean; FData: PAnsiChar; BitsPerPixel: integer);
  TGIFLZWDecompFunc  = procedure(Stream: TStream; Height, Width: integer; Interlaced: boolean; FData: PAnsiChar);
  TTIFFLZWDecompFunc = function(CompBuf: pbyte; LineSize: integer; var Id: pointer; FillOrder: integer): pbyte;
  TTIFFLZWCompFunc   = procedure(indata: pbyte; inputlen: integer; outstream: TStream; var id: pointer);


{!!
<FS>IIELanguageUpdatable

<FM>Declaration<FC>
IIELanguageUpdatable = interface
  procedure UpdateLanguage();
end;

<FM>Description<FN>
Each component (TComponent descendant) that implements UpdateLanguage method of this interface will be called whenever <A TIEImageEnGlobalSettings.MsgLanguage> changes.
!!}
IIELanguageUpdatable = interface
  ['{6274D2D8-3E4C-43F2-8157-229558BFB2F0}']
  procedure UpdateLanguage();
end;


{!!
<FS>TIEMediaFountationNotifyType

<FM>Declaration<FC>
}
TIEMediaFountationNotifyType = (iemfnFRAME, iemfnSTARTINGCAPTURE, iemfnENDOFSTREAM);
{!!}



////////////////////////////////////////////////////////////////////////////////
// Windows CMS

  type TIECMSGRAY = word;

  type TIECMSRGBCOLOR = record
    red:   WORD;
    green: WORD;
    blue:  WORD;
  end;

  type TIECMSCMYKCOLOR = record
    cyan:    WORD;
    magenta: WORD;
    yellow:  WORD;
    black:   WORD;
  end;

  type TIECMSXYZCOLOR = record
    X: WORD;
    Y: WORD;
    Z: WORD;
  end;

  type TIECMSYxyCOLOR = record
    Y:  WORD;
    x:  WORD;
    y_: WORD;
  end;

  type TIECMSLabCOLOR = record
    L: WORD;
    a: WORD;
    b: WORD;
  end;

  type TIECMSGENERIC3CHANNEL = record
    ch1: WORD;
    ch2: WORD;
    ch3: WORD;
  end;

  type TIECMSNAMEDCOLOR = record
    dwIndex: DWORD;
  end;

  const IECMSMAX_COLOR_CHANNELS = 8;   // maximum number of HiFi color channels

  type TIECMSHiFiCOLOR = record
    channel: array [0..IECMSMAX_COLOR_CHANNELS - 1] of byte;
  end;
  
  type TIECMSAlignment = record
    reserved1: DWORD;
    reserved2: pointer;
  end;

  TIECMSCOLOR = record
    case Integer of
      0: (gray:   TIECMSGRAY);
      1: (rgb:    TIECMSRGBCOLOR);
      2: (cmyk:   TIECMSCMYKCOLOR);
      3: (XYZ:    TIECMSXYZCOLOR);
      4: (Yxy:    TIECMSYxyCOLOR);
      5: (Lab:    TIECMSLabCOLOR);
      6: (gen3ch: TIECMSGENERIC3CHANNEL);
      7: (named:  TIECMSNAMEDCOLOR);
      8: (hifi:   TIECMSHiFiCOLOR);
      9: (align:  TIECMSAlignment);
    end;

  PIECMSCOLOR = ^TIECMSCOLOR;


  TIEPOINTL = packed record
    x: integer;
    y: integer;
  end;

  TIEDeviceModeW = packed record
    dmDeviceName: array[0..CCHDEVICENAME - 1] of WideChar;
    dmSpecVersion: Word;
    dmDriverVersion: Word;
    dmSize: Word;
    dmDriverExtra: Word;
    dmFields: DWORD;

    dmPosition: TIEPOINTL;
    dmDisplayOrientation: DWORD;
    dmDisplayFixedOutput: DWORD;

    dmColor: SHORT;
    dmDuplex: SHORT;
    dmYResolution: SHORT;
    dmTTOption: SHORT;
    dmCollate: SHORT;
    dmFormName: array[0..CCHFORMNAME - 1] of WideChar;
    dmLogPixels: Word;
    dmBitsPerPel: DWORD;
    dmPelsWidth: DWORD;
    dmPelsHeight: DWORD;
    dmDisplayFlags: DWORD;
    dmDisplayFrequency: DWORD;
    dmICMMethod: DWORD;
    dmICMIntent: DWORD;
    dmMediaType: DWORD;
    dmDitherType: DWORD;
    dmICCManufacturer: DWORD;
    dmICCModel: DWORD;
    dmPanningWidth: DWORD;
    dmPanningHeight: DWORD;
  end;



  ////////////////////////////////////////////////////////////////////
  // Gestures

const

  // Messages
  IEWM_GESTURENOTIFY    = $011A;
  IEWM_GESTURE          = $0119;
  IEWM_TOUCH            = $0240;
  IEWM_TABLET_DEFBASE   = $02C0;
  IEWM_TABLET_FLICK     = IEWM_TABLET_DEFBASE + 11;

  // Gesture IDs
  IEGID_BEGIN        = 1;
  IEGID_END          = 2;
  IEGID_ZOOM         = 3;
  IEGID_PAN          = 4;
  IEGID_ROTATE       = 5;
  IEGID_TWOFINGERTAP = 6;
  IEGID_PRESSANDTAP  = 7;
  IEGID_ROLLOVER     = IEGID_PRESSANDTAP;

  // Gesture flags - GESTUREINFO.dwFlags
  IEGF_BEGIN         = $00000001;
  IEGF_INERTIA       = $00000002;
  IEGF_END           = $00000004;

  // gesture configuration flags - set GESTURECONFIG.dwID to zero
  IEGC_ALLGESTURES                          = $00000001;
  IEGC_ZOOM                                 = $00000001;
  IEGC_PAN                                  = $00000001;
  IEGC_PAN_WITH_SINGLE_FINGER_VERTICALLY    = $00000002;
  IEGC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY  = $00000004;
  IEGC_PAN_WITH_GUTTER                      = $00000008;
  IEGC_PAN_WITH_INERTIA                     = $00000010;
  IEGC_ROTATE                               = $00000001;
  IEGC_TWOFINGERTAP                         = $00000001;
  IEGC_PRESSANDTAP                          = $00000001;
  IEGC_ROLLOVER                             = IEGC_PRESSANDTAP;


type
  TIEGestureNotifyStruct = packed record
    cbSize:       UINT;
    dwFlags:      DWORD;
    hwndTarget:   HWND;
    ptsLocation:  TSmallPoint;
    dwInstanceID: DWORD;
  end;
  PIEGestureNotifyStruct = ^TIEGestureNotifyStruct;

  TIEWMGestureNotify = packed record
    Msg:          DWORD;
    Unused:       WPARAM;
    NotifyStruct: PIEGestureNotifyStruct;
    Result:       integer;
  end;

  TIEGESTUREINFO = record // 8 byte alignment
    cbSize:       DWORD;
    dwFlags:      DWORD;
    dwID:         DWORD;
    hwndTarget:   HWND;
    ptsLocation:  TSmallPoint;
    dwInstanceID: DWORD;
    dwSequenceID: DWORD;
    ullArguments: uint64;
    cbExtraArgs:  DWORD;
  end;
  PIEGESTUREINFO = ^TIEGESTUREINFO;

  HGESTUREINFO = THandle;

  TIEGESTURECONFIG = packed record
    dwID:    DWORD;
    dwWant:  DWORD;
    dwBlock: DWORD;
  end;
  PIEGESTURECONFIG = ^TIEGESTURECONFIG;

  // Gestures
  ////////////////////////////////////////////////////////////////////


procedure IEInitialize_hyiedefs;
procedure IEFinalize_hyiedefs;


implementation

uses hyieutils, imageenio, imageenview, imageenproc;

          
{!!
<FS>LanguageToStr

<FM>Declaration<FC>
function LanguageToStr(ALanguage : <A TMsgLanguage>) : string;

<FM>Description<FN>
Converts a <A TMsgLanguage> to string, e.g. msEnglish would return 'English', msItalian returns 'Italian', etc.

<FM>Example<FC>
// Create all language menu items
procedure TMainForm.CreateLanguageMenuItems;
var
  aLang : TMsgLanguage;
  NewItem: TMenuItem;
begin
  mnuLanguage.Clear;
  for aLang := Low(TMsgLanguage) to High(TMsgLanguage) do
  begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := LanguageToStr(aLang);
    NewItem.Checked := IEGlobalSettings.MsgLanguage = aLang;
    NewItem.Tag := ord(aLang);
    NewItem.OnClick := LanguageClick;
    mnuLanguage.Add(NewItem)
  end;
end;  
        
// Handle clicking of a language item
procedure TMainForm.LanguageClick(Sender: TObject);
var
  aLang : TMsgLanguage;
  I: Integer;
begin
  for I := 0 to mnuLanguage.Count - 1 do
    mnuLanguage.Items[I].Checked := False;
  aLang := TMsgLanguage(TMenuItem(Sender).Tag);
  IEGlobalSettings.MsgLanguage := aLang;
  TMenuItem(Sender).Checked := True;
end;
!!}
function LanguageToStr(ALanguage : TMsgLanguage) : string;
begin
  Result := '';

  case ALanguage of
    msSystem         : Result := 'System Default';
    {$IFDEF IESUPPORTENGLISH}
    msEnglish        : Result := 'English';
    {$ENDIF}
    {$IFDEF IESUPPORTITALIAN}
    msItalian        : Result := 'Italian';
    {$ENDIF}
    {$IFDEF IESUPPORTGERMAN}
    msGerman         : Result := 'German';
    {$ENDIF}
    {$IFDEF IESUPPORTSPANISH}
    msSpanish        : Result := 'Spanish';
    {$ENDIF}
    {$IFDEF IESUPPORTFRENCH}
    msFrench         : Result := 'French';
    {$ENDIF}
    {$IFDEF IESUPPORTPORTUGUESE}
    msPortuguese     : Result := 'Portuguese';
    {$ENDIF}
    {$IFDEF IESUPPORTGREEK}
    msGreek          : Result := 'Greek';
    {$ENDIF}
    {$IFDEF IESUPPORTRUSSIAN}
    msRussian        : Result := 'Russian';
    {$ENDIF}
    {$IFDEF IESUPPORTDUTCH}
    msDutch          : Result := 'Dutch';
    {$ENDIF}
    {$IFDEF IESUPPORTSWEDISH}
    msSwedish        : Result := 'Swedish';
    {$ENDIF}
    {$IFDEF IESUPPORTPOLISH}
    msPolish         : Result := 'Polish';
    {$ENDIF}
    {$IFDEF IESUPPORTJAPANESE}
    msJapanese       : Result := 'Japanese';
    {$ENDIF}
    {$IFDEF IESUPPORTCZECH}
    msCzech          : Result := 'Czech';
    {$ENDIF}
    {$IFDEF IESUPPORTFINNISH}
    msFinnish        : Result := 'Finnish';
    {$ENDIF}
    {$IFDEF IESUPPORTFARSI}
    msFarsi          : Result := 'Farsi';
    {$ENDIF}
    {$IFDEF IESUPPORTCHINESE}
    msChinese                : Result := 'Chinese (Simplified)';
    msChineseTraditional     : Result := 'Chinese (Traditional)';
    msChineseTraditionalBig5 : Result := 'Chinese (Big5)';
    {$ENDIF}
    {$IFDEF IESUPPORTDANISH}
    msDanish         : Result := 'Danish';
    {$ENDIF}
    {$IFDEF IESUPPORTTURKISH}
    msTurkish        : Result := 'Turkish';
    {$ENDIF}
    {$IFDEF IESUPPORTKOREAN}
    msKorean         : Result := 'Korean';
    {$ENDIF}
    {$IFDEF IESUPPORTHUNGARIAN}
    msHungarian      : Result := 'Hungarian';
    {$ENDIF}
    {$IFDEF IESUPPORTARABIC}
    msArabic         : Result := 'Arabic';
    {$ENDIF}
    {$IFDEF IESUPPORTSERBIAN}
    msSerbian        : Result := 'Serbian';
    {$ENDIF}
    {$IFDEF IESUPPORTNORWEGIAN}
    msNorwegian      : Result := 'Norwegian';
    {$ENDIF}
    {$IFDEF IESUPPORTUSER}
    msUser           : Result := 'Custom';
    {$ENDIF}
  end;      
end;

procedure IEInitialize_hyiedefs;
begin
  iegDialogsBackground := iedbDefault;
end;

procedure IEFinalize_hyiedefs;
begin
end;



end.
