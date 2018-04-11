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
File version 1005
*)


unit iesettings;


{$R-}
{$Q-}

{$I ie.inc}


interface

uses Windows, Classes, SysUtils, Graphics, hyiedefs, hyieutils, imageenio;


///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
// TIEImageEnGlobalSettings

type


{!!
<FS>TIEEXIFInteroperabilityIndexUsage

<FM>Declaration<FC>
}
TIEEXIFInteroperabilityIndexUsage = (ieiiNone, ieiiApplyOnly, ieiiApplyAndReset);
{!!}

{!!
<FS>TIEWordTransitionParams

<FM>Declaration<FC>
TIEWordTransitionParams = class
  Word     : string;
  FontName : string;
  Style    : TFontStyles;
  Quality  : Word;
end;

<FM>Description<FN>
Properties for customizing the <L TIETransitionType>"Word" transition effects</L>:

- iettRandomBoxesWithWord
- iettRandomWord *
- iettExpandingWord *
- iettWordWipeOut *
- iettWordWipeIn *
- iettWordWipeInAndOut *
- iettWordHalfSweep
- iettWordFullSweep
- iettWordFullExpandingSweep

<TABLE>
<R> <H>Property</H> <H>Description</H> </R>
<R> <C>Word</C> <C>The text that is displayed during the transition. It can be a single character, e.g. a letter or a symbol from Wingdings, or a whole word</C> </R>
<R> <C>FontName</C> <C>The font to use to display the text, e.g. "Arial" or "Wingdings". Ensure you specify a font that is available on the destination system (default: "Arial")</C> </R>
<R> <C>Style</C> <C>The style that is used to display the text, can include fsBold, fsItalic, fsUnderline and/or fsStrikeOut (default: [fsBold])</C> </R>
<R> <C>Quality</C> <C>The smoothness of the text. The value specifies the height in pixels of the bitmap used to create each letter. A high value will give performance issues on some systems, whereas a low value will give the letter a blocky apperance. Practical range is 20 - 2000; default is 200</C> </R>
</TABLE>

Note: Effects with an asterisk will cycle through each specified letter, displaying one at a time. If you wish to display a whole (only a short one is recommended) then enclose it in quotations, e.g. "HI!"
Other effects will always display the full word.

<FM>Example<FC>
// Display snowflakes in random positions
IEGlobalSettings().WordTransitionParams.Word := 'T';  // Snowflake in the Wingdings font
IEGlobalSettings().WordTransitionParams.FontName := 'Wingdings';
IEGlobalSettings().WordTransitionParams.Style := [fsBold];

ImageEnView1.PrepareTransition;
ImageEnView1.IO.LoadFromFile(GetImageOfIndex(FCurrentImage));
ImageEnView1.RunTransition(iettRandomWord, 2000);

// Transition, showing each letter of the word "ImageEn" in turn
IEGlobalSettings().WordTransitionParams.Word := 'ImageEn';
ImageEnView1.PrepareTransition;
ImageEnView1.IO.LoadFromFile(GetImageOfIndex(FCurrentImage));
ImageEnView1.RunTransition(iettWordWipeInAndOut, 5000);

// Transition wiping the screen with the word "GO!"
IEGlobalSettings().WordTransitionParams.Word := '"GO!"'; // Enclose in quotes to avoid cyling. Small words are best
ImageEnView1.PrepareTransition;
ImageEnView1.IO.LoadFromFile(GetImageOfIndex(FCurrentImage));
ImageEnView1.RunTransition(iettWordWipeInAndOut, 5000);
!!}
  TIEWordTransitionParams = class
    Word     : string;
    FontName : string;
    Style    : TFontStyles;
    Quality  : Word;
  end;


{!!
<FS>TIEImageEnGlobalSettings

<FM>Description<FN>
Contains almost all ImageEn shared settings.

<A IEGlobalSettings> returns the instance of this object.


<FM>Example<FC>
// Change the language to Italian
IEGlobalSettings().MsgLanguage := msItalian;


<FM>Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.ApplyColorProfileOnRendering></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.AutoFragmentBitmap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.AutoLocateOnDisk></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.BlueToGrayCoef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.BorderX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.BorderY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.ColorReductionAlgorithm></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.ColorReductionQuality></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.ConvertColorFunction></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.DefaultCoresCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.DefaultDialogFont></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.DefaultDPIX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.DefaultDPIY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.DefaultPreviewsZoomFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.DefDialogCenter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.DefMinFileSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.DefTempPath></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.EdgeX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.EdgeY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.EnableCMS></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.EXIFInteroperabilityIndexUsage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.GreenToGrayCoef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.GridPen></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.HScrollHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.IsInsideTwain></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.IsRemoteSession></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.MaxImageEMFSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.MeasureUnits></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.MemoShortCuts></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.MinZoomDisplayGrid></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.MMX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.ModelessSelectTwainSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.MsgLanguage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.MViewExplorerThumbnailExts></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.ObjectsTIFFTag></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.OpSys></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.PanZoomQualityFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.PreviewAdditionalMultipageExts></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.PreviewImageBackgroundColor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.PreviewImageBackgroundStyle></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.PrintDialogMarginsIncrement></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.PrintDialogMarginsMinValue></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.RedToGrayCoef></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.ReleaseTwainResources></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.SelectionGridColor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.SystemColors></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.SystemDPIX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.SystemDPIY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.TransitionsDrawAlternative></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.UnicodeOS></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.UseButtonGlyphsInDialogs></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.UseCMYKProfile></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.UseDefaultFileExists></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.UseGDIPlus></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.UseRelativeStreams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.VScrollWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
</TABLE>
!!}
TIEImageEnGlobalSettings = class

  private
    fMsgLanguage: TMsgLanguage;
    fDefaultCoresCount: integer;
    fOpSys: TIEOpSys;
    fUnicodeOS: boolean;
    fDefaultPreviewsZoomFilter: TResampleFilter;
    fDefaultResampleFilter: TResampleFilter;
    fDefaultRotateAntiAlias : TIEAntialiasMode;
    fDefDialogCenter: TIEDialogCenter;
    fDefaultDialogFont: TFont;
    fDefMinFileSize: int64;
    fAutoLocateOnDisk: boolean;
    fAutoFragmentBitmap: boolean;
    fUseGDIPlus: boolean;
    fUseButtonGlyphsInDialogs: boolean;
    fPreviewImageBackgroundStyle: TIEBackgroundStyle;
    fPreviewImageBackgroundColor: TColor;
    fPreviewAdditionalMultipageExts: string;
    fSystemColors: integer;
    fIsRemoteSession: boolean;
    fSystemDPIX: integer;
    fSystemDPIY: integer;
    fDefaultDPIX: integer;
    fDefaultDPIY: integer;
    fMMX: boolean;
    fEdgeX: integer;
    fEdgeY: integer;
    fBorderX: integer;
    fBorderY: integer;
    fVScrollWidth: integer;
    fHScrollHeight: integer;
    fMinZoomDisplayGrid: integer;
    fGridPen: TPen;
    fSelectionGridColor: TColor;
    fMViewExplorerThumbnailExts: string;
    fDefGIF_LZWDECOMPFUNC: TGIFLZWDecompFunc;
    fDefGIF_LZWCOMPFUNC: TGIFLZWCompFunc;
    fDefTIFF_LZWDECOMPFUNC: TTIFFLZWDecompFunc;
    fDefTIFF_LZWCOMPFUNC: TTIFFLZWCompFunc;
    fFileFormats: TList;
    fUseCMYKProfile: boolean;
    fUseDefaultFileExists: boolean;
    fMaxImageEMFSize: integer;
    fDefTempPath: string;
    fConvertColorFunction: TIEConvertColorFunction;
    fEnableCMS: boolean;
    fColorReductionAlgorithm: integer;
    fColorReductionQuality: integer;
    fObjectsTIFFTag: integer;
    fUseRelativeStreams: boolean;
    fPanZoomQualityFilter: TResampleFilter;
    fRedToGrayCoef: integer;
    fGreenToGrayCoef: integer;
    fBlueToGrayCoef: integer;
    fEXIFInteroperabilityIndexUsage: TIEEXIFInteroperabilityIndexUsage;
    fPrintDialogMarginsIncrement: double;
    fPrintDialogMarginsMinValue: double;
    fWordTransitionParams: TIEWordTransitionParams;
    fTransitionsDrawAlternative : Boolean;
    fApplyColorProfileOnRendering: Boolean;
    fReleaseTwainResources: Boolean;
    fModelessSelectTwainSource: Boolean;
    fIsInsideTwain: Boolean;

    procedure SetMsgLanguage(value: TMsgLanguage);

  public
    // Not surfaced: Custom colors shown in PromptForColor dialog
    ColorDialogCustomColors : string;

{!!
<FS>TIEImageEnGlobalSettings.MemoShortCuts

<FM>Declaration<FC>
MemoShortCuts: <A TIEMemoShortCuts>;

<FM>Description<FN>
Contains all key shortcuts used in Memo objects. Defaults are:

    MemoShortCuts[iesLEFTALIGN]       := ShortCut(Word('L'), [ssCtrl]);
    MemoShortCuts[iesCENTERALIGN]     := ShortCut(Word('E'), [ssCtrl]);
    MemoShortCuts[iesRIGHTALIGN]      := ShortCut(Word('R'), [ssCtrl]);
    MemoShortCuts[iesJUSTIFIED]       := ShortCut(Word('J'), [ssCtrl]);
    MemoShortCuts[iesCOPY]            := ShortCut(Word('C'), [ssCtrl]);
    MemoShortCuts[iesCUT]             := ShortCut(Word('X'), [ssCtrl]);
    MemoShortCuts[iesPASTE]           := ShortCut(Word('V'), [ssCtrl]);
    MemoShortCuts[iesFONTSELECT]      := ShortCut(Word('F'), [ssCtrl]);
    MemoShortCuts[iesBOLD]            := ShortCut(Word('B'), [ssCtrl]);
    MemoShortCuts[iesITALIC]          := ShortCut(Word('I'), [ssCtrl]);
    MemoShortCuts[iesUNDERLINE]       := ShortCut(Word('U'), [ssCtrl]);
    MemoShortCuts[iesBACKCOLORSELECT] := ShortCut(Word('G'), [ssCtrl]);

Change one of above lines to set your custom short cut.

The following table specifies the default key map:
<TABLE>
<R> <H>Shortcut</H> <H>Function</H> </R>
<R> <C>F2</C> <C>Increase font size</C> </R>
<R> <C>F1</C> <C>Decrease font size</C> </R>
<R> <C>CTRL - L</C> <C>Left align</C> </R>
<R> <C>CTRL - E</C> <C>Center align</C> </R>
<R> <C>CTRL - R</C> <C>Right align</C> </R>
<R> <C>CTRL - J</C> <C>Justified</C> </R>
<R> <C>CTRL - C</C> <C>Copy</C> </R>
<R> <C>CTRL - X</C> <C>Cut</C> </R>
<R> <C>CTRL - V</C> <C>Paste</C> </R>
<R> <C>CTRL - F</C> <C>Open font dialog</C> </R>
<R> <C>CTRL - B</C> <C>Bold</C> </R>
<R> <C>CTRL - I</C> <C>Italic</C> </R>
<R> <C>CTRL - U</C> <C>Underline</C> </R>
<R> <C>CTRL - G</C> <C>Set background color (open dialog)</C> </R>
</TABLE>
!!}
    MemoShortCuts: TIEMemoShortCuts;

{!!
<FS>TIEImageEnGlobalSettings.MeasureUnits

<FM>Declaration<FC>
ieMeasureUnits: <A TIEMeasureUnits> = ('pixels', 'inches', 'km', 'mt', 'cm', 'mm', 'microns', 'nanometers', 'feet', 'yards', 'miles');

<FM>Description<FN>
Specifies how measure units are displayed as strings.

<FM>Example<FC>
IEGlobalSettings().MeasureUnits[ieuNANOMETERS] := 'uM';
!!}
    MeasureUnits: TIEMeasureUnits;

    constructor Create(); // do not use directly! Marked public only to avoid compatibility problems with old C++Builder versions
    destructor Destroy; override;

    procedure DestroySingletonInstance();

{!!
<FS>TIEImageEnGlobalSettings.MsgLanguage

<FM>Declaration<FC>
property MsgLanguage: <A TMsgLanguage>;

<FM>Description<FN>
This property sets the language for ImageEn dialogs and messages.

Note: The UpdateLanguage method of <A IIELanguageUpdatable> interface is called whenever MsgLanguage is updated.

<FM>Example<FC>
IEGlobalSettings().MsgLanguage := msItalian;
!!}
    property MsgLanguage: TMsgLanguage read fMsgLanguage write SetMsgLanguage;

{!!
<FS>TIEImageEnGlobalSettings.DefaultCoresCount

<FM>Declaration<FC>
property DefaultCoresCount: integer;

<FM>Description<FN>
Specifies the total amount of processor's cores to use.
-1 (default) means to get this value from operating system.

See also: <A IEGetCoresCount>.
!!}
  property DefaultCoresCount: integer read fDefaultCoresCount write fDefaultCoresCount;

{!!
<FS>TIEImageEnGlobalSettings.OpSys

<FM>Declaration<FC>
property OpSys: <A TIEOpSys>;

<FM>Description<FN>
Contains the detected operating system.
!!}
  property OpSys: TIEOpSys read fOpSys write fOpSys;

{!!
<FS>TIEImageEnGlobalSettings.TransitionsDrawAlternative

<FM>Declaration<FC>
property TransitionsDrawAlternative: boolean; (Default: False)

<FM>Description<FN>
Set to True to use an alternative drawing style for some transitions, in some cases this will improve performance on lower spec systems, but usually it is only an aesthetic change.

The specific effect upon each transition is as follows:
<TABLE>
<R> <H>Transition</H> <H>Effect of TransitionsDrawAlternative</H> </R>
<R> <C>iettCubeRotateFromLeft             
iettCubeRotateFromRight            
iettCubeRotateFromTop              
iettCubeRotateFromBottom           
iettCubeRotateFromLeft2
iettCubeRotateFromRight2           
iettCubeRotateFromTop2             
iettCubeRotateFromBottom2</C> <C>Setting to true will disable darkening of partially shown cube faces</C> </R>
<R> <C>iettPageFlip         
iettReversePageFlip</C> <C>Setting to true will disable darkening of partially shown pages</C> </R>
<R> <C>iettPageFlip2
iettReversePageFlip2</C> <C>Setting to true will disable drawing of a centre line</C> </R>
<R> <C>iettRandomBoxes</C> <C>Set to true for double-size boxes</C> </R>
<R> <C>iettSoftWipeFromLeft  
iettSoftWipeFromRight 
iettSoftWipeFromTop   
iettSoftWipeFromBottom</C> <C>Set to true for extra soft blending</C> </R>
<R> <C>iettSweepClockwise              
iettSweepCounterClockwise
iettFullSweepClockwise
iettExpandingSweepClockwise</C> <C>When set to true it rotates through a completely black state</C> </R>
<R> <C>iettRandomPuzzlePieces</C> <C>Set to true for half-sized puzzle pieces</C> </R>
<R> <C>iettRandomBoxesWithWord
iettRandomBigBoxes</C> <C>Set to true for double-size boxes</C> </R>
<R> <C>iettTriangularWipe
iettWordHalfSweep
iettWordFullSweep</C> <C>Set to true for anti-clockwise rotation</C> </R>
<R> <C>iettShreddedFromLeft                      
iettShreddedFromRight                     
iettShreddedFromTop                       
iettShreddedFromBottom                    
iettShreddedFromTopAndLeft                
iettShreddedFromTopAndRight               
iettShreddedFromBottomAndLeft             
iettShreddedFromBottomAndRight            
iettShreddedFromHorizonAndLeft            
iettShreddedFromHorizonAndRight           
iettShreddedFromTopAndVerticalCenter      
iettShreddedFromBottomAndVerticalCenter   
iettShreddedFromCenter                    
iettShreddedToCenter                      
iettShreddedInToHorizon                   
iettShreddedInToVerticalCenter            
iettShreddedOutFromHorizon                
iettShreddedOutFromVerticalCenter</C> <C>Set to true for larger shreds</C> </R>
<R> <C>iettBarsInFromLeft
iettBarsInFromRight
iettBarsFromTop
iettBarsFromBottom
iettBarsLeftThenRight
iettBarsRightThenLeft
iettBarsTopThenBottom
iettBarsBottomThenTop
iettBarsFrombothSides
iettBarsFromTopAndBottom
iettCrisscrossWipeFromTopLeft
iettCrisscrossWipeFromTopRight
iettCrisscrossWipeFromBottomLeft
iettCrisscrossWipeFromBottomRight
iettCrisscrossWipeBounceFromTopLeft
iettCrisscrossWipeBounceFromTopRight
iettCrisscrossWipeBounceFromBottomLeft
iettCrisscrossWipeBounceFromBottomRight
iettCrisscrossWipeFromLeftRightAndTop
iettCrisscrossWipeFromLeftRightAndBottom
iettCrisscrossWipeFromLeftTopAndBottom
iettCrisscrossWipeFromTopLeftRightAndBottom
iettCrisscrossWipeFromRightTopAndBottom
iettCrisscrossWipeFromBottomLeftTopRight
iettRectanglesFromTheLeft
iettRectanglesFromTheRight
iettRectanglesFromTheTop
iettRectanglesFromTheBottom</C> <C>Set to true for wider bars</C> </R>
<R> <C>iettSpeckledWipeFromLeft   
iettSpeckledWipeFromRight  
iettSpeckledWipeFromTop    
iettSpeckledWipeFromBottom </C> <C>Set to true for smaller speckles</C> </R>
<R> <C>iettExpandingExplosions
iettExpandingLightningBolts
iettExplosionWipeOut
iettExplosionWipeIn
iettExplosionWipeInAndOut</C> <C>Change to horizontally flip the shape</C> </R>
<R> <C>iettHeartWipeInAndOut
iettRandomHearts
iettExpandingHearts
iettHeartWipeOut
iettHeartWipeIn  </C> <C>Set to true for "Double hearts" instead of a single one</C> </R>
<R> <C>iettLeftRight2                   
iettRightLeft2
iettUpDown2
iettDownUp2</C> <C>If false, the wipe line is red. If true, a wider gray line is used</C> </R>
<R> <C>iettRandomPoints</C> <C>Set to true for "static" rather than dots</C> </R>
<R> <C>iettExpandingTriangles</C> <C>Set to true to invert the triangle</C> </R>
<R> <C>iettExpandFromLeft    
iettExpandFromRight   
iettExpandFromTop     
iettExpandFromBottom</C> <C>Setting to true will disable darkening of the image being covered</C> </R>
<R> <C>iettRandomWord
iettExpandingWord     
iettWordWipeOut
iettWordWipeIn
iettWordWipeInAndOut</C> <C>Set to true for larger words (i.e. greater coverage)</C> </R>
<R> <C>iettZigzagWipeFromHorizon
iettZigzagWipeFromVerticalCenter
iettZigzagWipeToHorizon
iettZigzagWipeToVerticalCenter</C> <C>Set to true for bigger teeth</C> </R>
<R> <C>iettPacmanFromLeft
iettPacmanFromRight  
iettPacman3Row       
iettPacman4Row       
iettPacman2SimRow    
iettPacman4SimRow
iettPacman6SimRow</C> <C>Setting to true will disable drawing of "Pills"</C> </R>
</TABLE>
!!}
  property TransitionsDrawAlternative: boolean read fTransitionsDrawAlternative write fTransitionsDrawAlternative;

{!!
<FS>TIEImageEnGlobalSettings.UnicodeOS

<FM>Declaration<FC>
property UnicodeOS: boolean;

<FM>Description<FN>
Returns true if the system has a Unicode operating system, otherwise returns false (i.e. Win98/WinME).
!!}
  property UnicodeOS: boolean read fUnicodeOS write fUnicodeOS;

{!!
<FS>TIEImageEnGlobalSettings.DefaultPreviewsZoomFilter

<FM>Declaration<FC>
property DefaultPreviewsZoomFilter: <A TResampleFilter>;

<FM>Description<FN>
Default zoom (resampling) filter for previews. Default is rfFastLinear.
!!}
  property DefaultPreviewsZoomFilter: TResampleFilter read fDefaultPreviewsZoomFilter write fDefaultPreviewsZoomFilter;

{!!
<FS>TIEImageEnGlobalSettings.DefaultResampleFilter

<FM>Declaration<FC>
property DefaultResampleFilter: <A TResampleFilter>;

<FM>Description<FN>
The default resampling filter is used when no filter is specified and no other default resampling filters are used, e.g. when using <A TImageEnProc.DoPreviews>([peResize]).
Default is rfFastLinear.
!!}
  property DefaultResampleFilter: TResampleFilter read fDefaultResampleFilter write fDefaultResampleFilter;

{!!
<FS>TIEImageEnGlobalSettings.DefaultRotateAntiAlias

<FM>Declaration<FC>
property DefaultRotateAntiAlias: <A TIEAntialiasMode>;

<FM>Description<FN>
Used when rotating images and an <L TIEAntialiasMode>anti-alias algorithm</L> cannot be specified, e.g. when using <A TImageEnProc.DoPreviews>([peRotate]).
Default is ierFast.
!!}
  property DefaultRotateAntiAlias: TIEAntialiasMode read fDefaultRotateAntiAlias write fDefaultRotateAntiAlias;

{!!
<FS>TIEImageEnGlobalSettings.DefDialogCenter

<FM>Declaration<FC>
property DefDialogCenter: <A TIEDialogCenter>;

<FM>Description<FN>
Specify a function called when open/save dialogs needs to be centered.
!!}
  property DefDialogCenter: TIEDialogCenter read fDefDialogCenter write fDefDialogCenter;

{!!
<FS>TIEImageEnGlobalSettings.DefaultDialogFont

<FM>Declaration<FC>
property DefaultDialogFont: TFont;

<FM>Description<FN>
Don't use directly as it may be unitialized, use IEGetDefaultDialogFont().
!!}
  property DefaultDialogFont: TFont read fDefaultDialogFont write fDefaultDialogFont;

{!!
<FS>TIEImageEnGlobalSettings.DefMinFileSize

<FM>Declaration<FC>
property DefMinFileSize: int64;

<FM>Description<FN>
Specifies the default value for <A TIEBitmap.MinFileSize> property.
<A TIEBitmap.MinFileSize> specifies the minimum memory needed by the image to allow use of memory mapped file.
If the memory needed by the image is less than MinFileSize, the image will be stored in memory (also if the Location is ieFile). If the global variable DefMinFileSize is not -1, it overlaps the property MinFileSize value.
!!}
  property DefMinFileSize: int64 read fDefMinFileSize write fDefMinFileSize;

{!!
<FS>TIEImageEnGlobalSettings.AutoLocateOnDisk

<FM>Declaration<FC>
property AutoLocateOnDisk: boolean;

<FM>Description<FN>
If true (default) TIEBitmap will allocate the image on disk if it fails to allocate on memory.
!!}
  property AutoLocateOnDisk: boolean read fAutoLocateOnDisk write fAutoLocateOnDisk;

{!!
<FS>TIEImageEnGlobalSettings.AutoFragmentBitmap

<FM>Declaration<FC>
property AutoFragmentBitmap: boolean;

<FM>Description<FN>
If true (default) TIEBitmap will try to allocate chunks of memory instead of a single buffer to contain the image.
This is useful when memory is fragmented.
!!}
  property AutoFragmentBitmap: boolean read fAutoFragmentBitmap write fAutoFragmentBitmap;

{!!
<FS>TIEImageEnGlobalSettings.UseGDIPlus

<FM>Declaration<FC>
property UseGDIPlus: boolean;

<FM>Description<FN>
If True ImageEn used GDIPlus instead of GDI, when available.
!!}
  property UseGDIPlus: boolean read fUseGDIPlus write fUseGDIPlus;

{!!
<FS>TIEImageEnGlobalSettings.PreviewImageBackgroundStyle

<FM>Declaration<FC>
property PreviewImageBackgroundStyle: <A TIEBackgroundStyle>;

<FM>Description<FN>
Specify the background style for image preview in open/save dialogs.
!!}
  property PreviewImageBackgroundStyle: TIEBackgroundStyle read fPreviewImageBackgroundStyle write fPreviewImageBackgroundStyle;

{!!
<FS>TIEImageEnGlobalSettings.PreviewImageBackgroundColor

<FM>Declaration<FC>
property PreviewImageBackgroundColor: TColor;

<FM>Description<FN>
Specify the background color for image preview in open/save dialogs.
!!}
  property PreviewImageBackgroundColor: TColor read fPreviewImageBackgroundColor write fPreviewImageBackgroundColor;

{!!
<FS>TIEImageEnGlobalSettings.PreviewAdditionalMultipageExts

<FM>Declaration<FC>
property PreviewAdditionalMultipageExts: string;

<FM>Description<FN>
Additional multipage extensions (ex. 'mpg,mpeg,wmv,avi') for open/save dialogs.
!!}
  property PreviewAdditionalMultipageExts: string read fPreviewAdditionalMultipageExts write fPreviewAdditionalMultipageExts;

{!!
<FS>TIEImageEnGlobalSettings.SystemColors

<FM>Declaration<FC>
property SystemColors: integer;

<FM>Description<FN>
Display bits per pixel.
!!}
  property SystemColors: integer read fSystemColors write fSystemColors;

{!!
<FS>TIEImageEnGlobalSettings.IsRemoteSession

<FM>Declaration<FC>
property IsRemoteSession: boolean;

<FM>Description<FN>
true =this is a RDP session
!!}
  property IsRemoteSession: boolean read fIsRemoteSession write fIsRemoteSession;

{!!
<FS>TIEImageEnGlobalSettings.SystemDPIX

<FM>Declaration<FC>
property SystemDPIX: integer;

<FM>Description<FN>
System DPIX.
!!}
  property SystemDPIX: integer read fSystemDPIX write fSystemDPIX;

{!!
<FS>TIEImageEnGlobalSettings.SystemDPIY

<FM>Declaration<FC>
property SystemDPIY: integer;

<FM>Description<FN>
System DPIY.
!!}
  property SystemDPIY: integer read fSystemDPIY write fSystemDPIY;


{!!
<FS>TIEImageEnGlobalSettings.WordTransitionParams

<FM>Declaration<FC>
property WordTransitionParams: <A TIEWordTransitionParams>;

<FM>Description<FN>
Properties for customizing the <L TIETransitionType>"Word" transition effects</L> such as iettRandomWord and iettExpandingWord.

<TABLE>
<R> <H>Property</H> <H>Description</H> </R>
<R> <C>Word</C> <C>The text that is displayed during the transition. It can be a single character, e.g. a letter or a symbol from Wingdings or a whole word. If it is a word then each letter of the word is displayed in turn. To display a whole word without cycling, enclose it in quotes (default: 'A')</C> </R>
<R> <C>FontName</C> <C>The font to use to display the text, e.g. "Arial" or "Wingdings". Ensure you specify a font that is available on the destination system (default: "Arial")</C> </R>
<R> <C>Style</C> <C>The style that is used to display the text, can include fsBold, fsItalic, fsUnderline and/or fsStrikeOut (default: [fsBold])</C> </R>
<R> <C>Quality</C> <C>The smoothness of the text. The value specifies the height in pixels of the bitmap used to create each letter. A high value will give performance issues on some systems, whereas a low value will give the letter a blocky apperance. Practical range is 20 - 2000; default is 200</C> </R>
</TABLE>

<FM>Example<FC>
// Display snowflakes in random positions
IEGlobalSettings().WordTransitionParams.Word := 'T';  // Snowflake in the Wingdings font
IEGlobalSettings().WordTransitionParams.FontName := 'Wingdings';
IEGlobalSettings().WordTransitionParams.Style := [fsBold];

ImageEnView1.PrepareTransition;
ImageEnView1.IO.LoadFromFile(GetImageOfIndex(FCurrentImage));
ImageEnView1.RunTransition(iettRandomWord, 2000);

// Transition, showing each letter of the word "ImageEn" in turn
IEGlobalSettings().WordTransitionParams.Word := 'ImageEn';
ImageEnView1.PrepareTransition;
ImageEnView1.IO.LoadFromFile(GetImageOfIndex(FCurrentImage));
ImageEnView1.RunTransition(iettWordWipeInAndOut, 5000);

// Transition wiping the screen with the word "GO!"
IEGlobalSettings().WordTransitionParams.Word := '"GO!"'; // Enclose in quotes to avoid cyling. Small words are best
ImageEnView1.PrepareTransition;
ImageEnView1.IO.LoadFromFile(GetImageOfIndex(FCurrentImage));
ImageEnView1.RunTransition(iettWordWipeInAndOut, 5000);
!!}
  property WordTransitionParams: TIEWordTransitionParams read fWordTransitionParams write fWordTransitionParams;

{!!
<FS>TIEImageEnGlobalSettings.DefaultDPIX

<FM>Declaration<FC>
property DefaultDPIX: integer;

<FM>Description<FN>
Specify the default DPI X value assigned when a loaded image doesn't contain this information. Default value is 300 for both X and Y.
!!}
  property DefaultDPIX: integer read fDefaultDPIX write fDefaultDPIX;

{!!
<FS>TIEImageEnGlobalSettings.DefaultDPIY

<FM>Declaration<FC>
property DefaultDPIY: integer;

<FM>Description<FN>
Specify the default DPI Y value assigned when a loaded image doesn't contain this information. Default value is 300 for both X and Y.
!!}
  property DefaultDPIY: integer read fDefaultDPIY write fDefaultDPIY;

{!!
<FS>TIEImageEnGlobalSettings.MMX

<FM>Declaration<FC>
property MMX: boolean;

<FM>Description<FN>
true = CPU supports MMX.
!!}
  property MMX: boolean read fMMX write fMMX;

{!!
<FS>TIEImageEnGlobalSettings.EdgeX

<FM>Declaration<FC>
property EdgeX: integer;

<FM>Description<FN>
System border horizontal edge (3d).
!!}
  property EdgeX: integer read fEdgeX write fEdgeX;

{!!
<FS>TIEImageEnGlobalSettings.EdgeY

<FM>Declaration<FC>
property EdgeY: integer;

<FM>Description<FN>
System border vertical edge (3d).
!!}
  property EdgeY: integer read fEdgeY write fEdgeY;

{!!
<FS>TIEImageEnGlobalSettings.BorderX

<FM>Declaration<FC>
property BorderX: integer;

<FM>Description<FN>
System border horizontal edge.
!!}
  property BorderX: integer read fBorderX write fBorderX;

{!!
<FS>TIEImageEnGlobalSettings.BorderY

<FM>Declaration<FC>
property BorderY: integer;

<FM>Description<FN>
System border vertical edge.
!!}
  property BorderY: integer read fBorderY write fBorderY;

{!!
<FS>TIEImageEnGlobalSettings.VScrollWidth

<FM>Declaration<FC>
property VScrollWidth: integer;

<FM>Description<FN>
System scrollbar width.
!!}
  property VScrollWidth: integer read fVScrollWidth write fVScrollWidth;

{!!
<FS>TIEImageEnGlobalSettings.HScrollHeight

<FM>Declaration<FC>
property HScrollHeight: integer;

<FM>Description<FN>
System scrollbar height.
!!}
  property HScrollHeight: integer read fHScrollHeight write fHScrollHeight;

{!!
<FS>TIEImageEnGlobalSettings.MinZoomDisplayGrid

<FM>Declaration<FC>
property MinZoomDisplayGrid: integer;

<FM>Description<FN>
Specifies the minimum value of zoom to display a grid (when <A TImageEnView.DisplayGrid> is True). Default value is 400.
!!}
  property MinZoomDisplayGrid: integer read fMinZoomDisplayGrid write fMinZoomDisplayGrid;

{!!
<FS>TIEImageEnGlobalSettings.GridPen

<FM>Declaration<FC>
property GridPen: TPen

<FM>Description<FN>
Specifies the pen used to draw the grid.
!!}
  property GridPen: TPen read fGridPen write fGridPen;


{!!
<FS>TIEImageEnGlobalSettings.SelectionGridColor

<FM>Declaration<FC>
property SelectionGridColor: TColor; (Default: $00A0A0A0)

<FM>Description<FN>
Specifies the color of the selection grid drawn by use of <A TImageEnView.SelectionGridSize>, <A TImageEnView.SelectionGridWidth> or <A TImageEnView.SelectionGridHeight>
!!}
  property SelectionGridColor: TColor read fSelectionGridColor write fSelectionGridColor;

{!!
<FS>TIEImageEnGlobalSettings.MViewExplorerThumbnailExts

<FM>Declaration<FC>
property MViewExplorerThumbnailExts: string;

<FM>Description<FN>
A list of file extensions delimited by semi-colons for which ImageEnMView will retrieve thumbnails from Windows Explorer. This can be any format that displays a thumbnail in Explorer, including images and videos.

<FM>Example<FC>
IEGlobalSettings().MViewExplorerThumbnailExts := '*.AVI;*.MPG;*.MPE;*.MPEG;*.WMV;*.ASF;*.IVF;*.WM;*.MP4;*.MOV;*.QT;*.RM;*.M2TS;*.MTS;*.MOD;';

!!}
  // Extensions for which thumbnails will be loaded from Explorer. Defaults to ALL_KNOWN_EXPLORER_FORMATS
  property MViewExplorerThumbnailExts: string read fMViewExplorerThumbnailExts write fMViewExplorerThumbnailExts;

  property DefGIF_LZWDECOMPFUNC: TGIFLZWDecompFunc read fDefGIF_LZWDECOMPFUNC write fDefGIF_LZWDECOMPFUNC;
  property DefGIF_LZWCOMPFUNC: TGIFLZWCompFunc read fDefGIF_LZWCOMPFUNC write fDefGIF_LZWCOMPFUNC;
  property DefTIFF_LZWDECOMPFUNC: TTIFFLZWDecompFunc read fDefTIFF_LZWDECOMPFUNC write fDefTIFF_LZWDECOMPFUNC;
  property DefTIFF_LZWCOMPFUNC: TTIFFLZWCompFunc read fDefTIFF_LZWCOMPFUNC write fDefTIFF_LZWCOMPFUNC;

  property FileFormats: TList read fFileFormats write fFileFormats;

{!!
<FS>TIEImageEnGlobalSettings.UseButtonGlyphsInDialogs

<FM>Declaration<FC>
property UseButtonGlyphsInDialogs: boolean;

<FM>Description<FN>
When true ImageEn's dialogs will show a glyph (image) on buttons. When false the buttons only show text.

Note: This applies to <A TImageEnIO.DoPrintPreviewDialog>, <A TImageEnMIO.DoPrintPreviewDialog>, <A TImageEnIO.SelectAcquireSource>, <A TImageEnMIO.SelectAcquireSource>, <A TImageEnProc.DoPreviews> and clicking the Advanced button in <A TSaveImageEnDialog>

<FM>Example<FC>
IEGlobalSettings().UseButtonGlyphsInDialogs := True;
ImageEnView1.IO.DoPrintPreviewDialog;
!!}
  property UseButtonGlyphsInDialogs: boolean read fUseButtonGlyphsInDialogs write fUseButtonGlyphsInDialogs;

{!!
<FS>TIEImageEnGlobalSettings.UseCMYKProfile

<FM>Declaration<FC>
property UseCMYKProfile: boolean;

<FM>Description<FN>
When true ImageEn uses an internal ICC profile to convert from CMYK to RGB and viceversa.
!!}
  property UseCMYKProfile: boolean read fUseCMYKProfile write fUseCMYKProfile;

{!!
<FS>TIEImageEnGlobalSettings.UseDefaultFileExists

<FM>Declaration<FC>
property UseDefaultFileExists: boolean;

<FM>Description<FN>
When true (default is false since version 3.0.3) ImageEn uses FileExists function. Otherwise uses an different way which doesn't need "listing" Windows rights.
!!}
  property UseDefaultFileExists: boolean read fUseDefaultFileExists write fUseDefaultFileExists;

{!!
<FS>TIEImageEnGlobalSettings.MaxImageEMFSize

<FM>Declaration<FC>
property MaxImageEMFSize: integer;

<FM>Description<FN>
Specifies the maximum width or height of imported EMF/WMF image.

<FM>Example<FC>
// we want maximum 1024x??? imported image
IEGlobalSettings().MaxImageEMFSize := 1024;
ImageEnView1.IO.LoadFromFile('input.wmf');
!!}
  property MaxImageEMFSize: integer read fMaxImageEMFSize write fMaxImageEMFSize;

{!!
<FS>TIEImageEnGlobalSettings.DefTempPath

<FM>Declaration<FC>
property DefTempPath: string;

<FM>Description<FN>
The DefTempPath specifies the directory where ImageEn stores temporary files.
The default value (empty string) is equal to default system temporary directory.

Applications should set DefTempPath inside 'initialization' block, before that the ImageEn components are created.
For <A TImageEnMView> component, you can change DefTempPath at any time, just call after the <A TImageEnMView.Clear> method.

<FM>Example<FC>
Initialization
  IEGlobalSettings().DefTempPath := 'G:\temp';

  ...or...

// this set ImageEnMView1 to disk G and ImageEnMView2 to disk H: 
IEGlobalSettings().DefTempPath := 'G:\';
ImageEnMView1.Clear;
IEGlobalSettings().DefTempPath := 'H:\';
ImageEnMView2.Clear;
!!}
  property DefTempPath: string read fDefTempPath write fDefTempPath;

{!!
<FS>TIEImageEnGlobalSettings.ConvertColorFunction

<FM>Declaration<FC>
property ConvertColorFunction: <A TIEConvertColorFunction>;

<FM>Description<FN>
Specifies the function used to convert from a color space to another.
!!}
  property ConvertColorFunction: TIEConvertColorFunction read fConvertColorFunction write fConvertColorFunction;

{!!
<FS>TIEImageEnGlobalSettings.EnableCMS

<FM>Declaration<FC>
property EnableCMS: boolean;

<FM>Description<FN>
Enables a Color Management System. Default CMS is provided by Windows.
Default values is <FC>false<FN>: this means that no color profile is applied.
See: <A Color Management System>.
!!}
  property EnableCMS: boolean read fEnableCMS write fEnableCMS;

{!!
<FS>TIEImageEnGlobalSettings.ColorReductionAlgorithm

<FM>Declaration<FC>
property ColorReductionAlgorithm: integer;

<FM>Description<FN>
Specifies the algorithm used when converting from true color (24 bit) to color mapped images.
Color reduction algorithm are used, for example, when you save to a GIF.
-1 = automatically select (default)
0 = Kohonen algorithm (See also: <A TIEImageEnGlobalSettings.ColorReductionQuality>)
1 = Median cut
!!}
  property ColorReductionAlgorithm: integer read fColorReductionAlgorithm write fColorReductionAlgorithm;

{!!
<FS>TIEImageEnGlobalSettings.ColorReductionQuality

<FM>Declaration<FC>
property ColorReductionQuality: integer;

<FM>Description<FN>
When <A TIEImageEnGlobalSettings.ColorReductionAlgorithm> is 0, this field specifies the quality. 0=minimum quality, 100=maximum quality.
-1 means "automatically calcualted" (default)
!!}
  property ColorReductionQuality: integer read fColorReductionQuality write fColorReductionQuality;

{!!
<FS>TIEImageEnGlobalSettings.ObjectsTIFFTag

<FM>Declaration<FC>
property ObjectsTIFFTag: integer;

<FM>Description<FN>
Specifies the TIFF tag used to read/write embedded vectorial objects. The default value is 40101.
!!}
  property ObjectsTIFFTag: integer read fObjectsTIFFTag write fObjectsTIFFTag;

{!!
<FS>TIEImageEnGlobalSettings.UseRelativeStreams

<FM>Declaration<FC>
property UseRelativeStreams: boolean;

<FM>Description<FN>
For default ImageEn uses absolute streams. This means that the image must be contained from position zero of the stream.
Setting UseRelativeStreams=true, you can put the image in any offset of the stream.
Default is False.
!!}
  property UseRelativeStreams: boolean read fUseRelativeStreams write fUseRelativeStreams;

{!!
<FS>TIEImageEnGlobalSettings.PanZoomQualityFilter

<FM>Declaration<FC>
property PanZoomQualityFilter: <A TResampleFilter> = rfNone;

<FM>Description<FN>
Specifies zoom filter for iettPanZoom transition effect.
!!}
  property PanZoomQualityFilter: TResampleFilter read fPanZoomQualityFilter write fPanZoomQualityFilter;

{!!
<FS>TIEImageEnGlobalSettings.RedToGrayCoef

<FM>Declaration<FC>
property RedToGrayCoef: integer;

<FM>Description<FN>
Specify the red coefficient used to convert from color to gray scale. Default values are 21, 71 and 8. The conversion formula is:
<FC>gray := (Red * <A TIEImageEnGlobalSettings.RedToGrayCoef> + Green * <A TIEImageEnGlobalSettings.GreenToGrayCoef> + Blue * <A TIEImageEnGlobalSettings.BlueToGrayCoef>) div 100;
!!}
  property RedToGrayCoef: integer read fRedToGrayCoef write fRedToGrayCoef;

{!!
<FS>TIEImageEnGlobalSettings.GreenToGrayCoef

<FM>Declaration<FC>
property GreenToGrayCoef: integer;

<FM>Description<FN>
Specify the green coefficient used to convert from color to gray scale. Default values are 21, 71 and 8. The conversion formula is:
<FC>gray := (Red * <A TIEImageEnGlobalSettings.RedToGrayCoef> + Green * <A TIEImageEnGlobalSettings.GreenToGrayCoef> + Blue * <A TIEImageEnGlobalSettings.BlueToGrayCoef>) div 100;
!!}
  property GreenToGrayCoef: integer read fGreenToGrayCoef write fGreenToGrayCoef;

{!!
<FS>TIEImageEnGlobalSettings.BlueToGrayCoef

<FM>Declaration<FC>
property BlueToGrayCoef: integer;

<FM>Description<FN>
Specify the blue coefficient used to convert from color to gray scale. Default values are 21, 71 and 8. The conversion formula is:
<FC>gray := (Red * <A TIEImageEnGlobalSettings.RedToGrayCoef> + Green * <A TIEImageEnGlobalSettings.GreenToGrayCoef> + Blue * <A TIEImageEnGlobalSettings.BlueToGrayCoef>) div 100;
!!}
  property BlueToGrayCoef: integer read fBlueToGrayCoef write fBlueToGrayCoef;


{!!
<FS>TIEImageEnGlobalSettings.EXIFInteroperabilityIndexUsage

<FM>Declaration<FC>
property EXIFInteroperabilityIndexUsage: <A TIEEXIFInteroperabilityIndexUsage>;

<FM>Description<FN>
When this property is ieiiApplyOnly or ieiiApplyAndReset and EnableCMS is True then EXIF interoperability index (<A TIOParamsVals.EXIF_InteropIndex>) is applied.
If this property contains ieiiApplyAndReset then after applied EXIF_InteropIndex is reset (emptied).

When EXIFInteroperabilityIndexUsage is ieiiApplyAndReset then ImageEn follows these rules:
- If EXIF_InteropIndex = 'R03' and there isn't an embedded profile then the AdobeRGB1998 profile is applied. Content of EXIF_InteropIndex field is emptied.
- If EXIF_InteropIndex = 'R03' and there is an embedded profile then the embedded profile is applied. Content of EXIF_InteropIndex field is emptied.
- If EXIF_InteropIndex <> 'R03' and there isn't an embedded profile no profile is applied. Content of EXIF_InteropIndex remains untouched.
- If EXIF_InteropIndex <> 'R03' and there is an embedded profile then the embedded profile is applied. Content of EXIF_InteropIndex field is emptied.

When this property is ieiiNone then EXIF_InteropIndex is read but not modified or applied in any way.

The default is ieiiApplyAndReset.
!!}
  property EXIFInteroperabilityIndexUsage: TIEEXIFInteroperabilityIndexUsage read fEXIFInteroperabilityIndexUsage write fEXIFInteroperabilityIndexUsage;

{!!
<FS>TIEImageEnGlobalSettings.PrintDialogMarginsIncrement

<FM>Declaration<FC>
property PrintDialogMarginsIncrement: double;

<FM>Description<FN>
Specifies the floating point increment/decrement for Left, Top, Right and Bottom margins.

The default is 0.01.
!!}
  property PrintDialogMarginsIncrement: double read fPrintDialogMarginsIncrement write fPrintDialogMarginsIncrement;

{!!
<FS>TIEImageEnGlobalSettings.PrintDialogMarginsMinValue

<FM>Declaration<FC>
property PrintDialogMarginsMinValue: double;

<FM>Description<FN>
Specifies the floating point allowed minimal value for Left, Top, Right and Bottom margins.

The default is 0.
!!}
  property PrintDialogMarginsMinValue: double read fPrintDialogMarginsMinValue write fPrintDialogMarginsMinValue;

{!!
<FS>TIEImageEnGlobalSettings.ApplyColorProfileOnRendering

<FM>Declaration<FC>
property ApplyColorProfileOnRendering: Boolean;

<FM>Description<FN>
Applies <A TIEBitmap.ColorProfile> when the image is displayed.

The default is True.
!!}
  property ApplyColorProfileOnRendering: Boolean read fApplyColorProfileOnRendering write fApplyColorProfileOnRendering;

{!!
<FS>TIEImageEnGlobalSettings.ReleaseTwainResources

<FM>Declaration<FC>
property ReleaseTwainResources: Boolean;

<FM>Description<FN>
Forces to free Twain resources when not used by ImageEn.

The default is False.
!!}
  property ReleaseTwainResources: Boolean read fReleaseTwainResources write fReleaseTwainResources;

{!!
<FS>TIEImageEnGlobalSettings.ModelessSelectTwainSource

<FM>Declaration<FC>
property ModelessSelectTwainSource: Boolean;

<FM>Description<FN>
If True the Twain source selection dialog is modeless (can lose focus). If False the selection dialog is modal.

The default is False.
!!}
  property ModelessSelectTwainSource: Boolean read fModelessSelectTwainSource write fModelessSelectTwainSource;

{!!
<FS>TIEImageEnGlobalSettings.IsInsideTwain

<FM>Declaration<FC>
property IsInsideTwain: Boolean;

<FM>Description<FN>
If True this thread is inside Twain handling function (maybe handling an event).
!!}
  property IsInsideTwain: Boolean read fIsInsideTwain write fIsInsideTwain;

end;


//////////////////////////////////////////////////////////////////////////////

{!!
<FS>IEGlobalSettings

<FM>Declaration<FC>
function IEGlobalSettings(): <A TIEImageEnGlobalSettings>;

<FM>Description<FN>
Returns the instance of TIEImageEnGlobalSettings object.
!!}
function IEGlobalSettings(): TIEImageEnGlobalSettings;


//////////////////////////////////////////////////////////////////////////////


implementation

uses Menus, Forms;



///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
// TIEImageEnGlobalSettings

var
  ieGlobalSettings_: TIEImageEnGlobalSettings = nil;


function IEGlobalSettings(): TIEImageEnGlobalSettings;
begin
  if not assigned(ieGlobalSettings_) then
    ieGlobalSettings_ := TIEImageEnGlobalSettings.Create();
  result := ieGlobalSettings_;
end;

destructor TIEImageEnGlobalSettings.Destroy;
begin
  FreeAndNil(fWordTransitionParams);
  inherited;
end;

procedure TIEImageEnGlobalSettings.DestroySingletonInstance();
begin
  ieGlobalSettings_.Free();
  ieGlobalSettings_ := nil;
end;

constructor TIEImageEnGlobalSettings.Create();
var
  hdc: THANDLE;
begin
  fMsgLanguage                    := msSystem;
  fDefaultCoresCount              := -1;
  fOpSys                          := IEGetOpSys();
  fUnicodeOS                      := (fOpSys > ieosWinME);
  fDefaultPreviewsZoomFilter      := rfFastLinear;
  fDefaultResampleFilter          := rfFastLinear;
  fDefaultRotateAntiAlias         := ierFast;
  fDefDialogCenter                := nil;
  fDefaultDialogFont              := nil;
  fDefMinFileSize                 := -1;
  fAutoLocateOnDisk               := true;
  fAutoFragmentBitmap             := true;
  fUseGDIPlus                     := true;
  fUseButtonGlyphsInDialogs       := False;
  fPreviewImageBackgroundStyle    := iebsSolid;
  fPreviewImageBackgroundColor    := clBtnFace;
  fPreviewAdditionalMultipageExts := 'mpg,mpeg,wmv,avi';
  // init memo shortcuts
  MemoShortCuts[iesLEFTALIGN]       := ShortCut(Word('L'), [ssCtrl]);
  MemoShortCuts[iesCENTERALIGN]     := ShortCut(Word('E'), [ssCtrl]);
  MemoShortCuts[iesRIGHTALIGN]      := ShortCut(Word('R'), [ssCtrl]);
  MemoShortCuts[iesJUSTIFIED]       := ShortCut(Word('J'), [ssCtrl]);
  MemoShortCuts[iesCOPY]            := ShortCut(Word('C'), [ssCtrl]);
  MemoShortCuts[iesCUT]             := ShortCut(Word('X'), [ssCtrl]);
  MemoShortCuts[iesPASTE]           := ShortCut(Word('V'), [ssCtrl]);
  MemoShortCuts[iesFONTSELECT]      := ShortCut(Word('F'), [ssCtrl]);
  MemoShortCuts[iesBOLD]            := ShortCut(Word('B'), [ssCtrl]);
  MemoShortCuts[iesITALIC]          := ShortCut(Word('I'), [ssCtrl]);
  MemoShortCuts[iesUNDERLINE]       := ShortCut(Word('U'), [ssCtrl]);
  MemoShortCuts[iesBACKCOLORSELECT] := ShortCut(Word('G'), [ssCtrl]);
  // measure units
  MeasureUnits[ieuPIXELS]      := 'pixels';
  MeasureUnits[ieuINCHES]      := 'inches';
  MeasureUnits[ieuKM]          := 'km';
  MeasureUnits[ieuMETERS]      := 'mt';
  MeasureUnits[ieuCENTIMETERS] := 'cm';
  MeasureUnits[ieuMILLIMETERS] := 'mm';
  MeasureUnits[ieuMICRONS]     := 'microns';
  MeasureUnits[ieuNANOMETERS]  := 'nanometers';
  MeasureUnits[ieuFEET]        := 'feet';
  MeasureUnits[ieuYARDS]       := 'yards';
  MeasureUnits[ieuMILES]       := 'miles';

  // bits per pixel of display and DPI
  hdc := getdc(0);
  fSystemColors := GetDeviceCaps(hdc, BITSPIXEL);
  fSystemDPIX := GetDeviceCaps(hdc, LOGPIXELSX);
  fSystemDPIY := GetDeviceCaps(hdc, LOGPIXELSY);
  releasedc(0, hdc);

  fDefaultDPIX := 300;
  fDefaultDPIY := 300;

  fIsRemoteSession := IEIsRemoteSession();
  fMMX := IEMMXSupported;

  fEdgeX := GetSystemMetrics(SM_CXEDGE);
  fEdgeY := GetSystemMetrics(SM_CYEDGE);
  fBorderX := GetSystemMetrics(SM_CXBORDER);
  fBorderY := GetSystemMetrics(SM_CYBORDER);
  fVScrollWidth := GetSystemMetrics(SM_CYHSCROLL);
  fHScrollHeight := GetSystemMetrics(SM_CXVSCROLL);

  fMinZoomDisplayGrid := 400;
  fGridPen := TPen.Create;
  fSelectionGridColor := $00A0A0A0;

  fMViewExplorerThumbnailExts := ALL_KNOWN_EXPLORER_VIDEO_FORMATS;

  fUseCMYKProfile := true;
  fUseDefaultFileExists := false;
  fMaxImageEMFSize := 8000;
  fDefTEMPPATH := '';
  fConvertColorFunction := IEDefaultConvertColorFunction;
  fEnableCMS := false;
  fColorReductionAlgorithm := -1;
  fColorReductionQuality := -1;
  fObjectsTIFFTag := 40101;
  fUseRelativeStreams := false;
  fPanZoomQualityFilter := rfNone;

  fRedToGrayCoef   := 21;
  fGreenToGrayCoef := 71;
  fBlueToGrayCoef  := 8;

  EXIFInteroperabilityIndexUsage := ieiiApplyAndReset;

  fPrintDialogMarginsIncrement := 0.01;
  fPrintDialogMarginsMinValue  := 0;

  fWordTransitionParams := TIEWordTransitionParams.create;
  fWordTransitionParams.Word     := 'A';
  fWordTransitionParams.FontName := 'Arial';
  fWordTransitionParams.Style    := [fsBold];
  fWordTransitionParams.Quality  := 200;

  fTransitionsDrawAlternative := False;

  fApplyColorProfileOnRendering := True;

  fReleaseTwainResources := False;
  fModelessSelectTwainSource := False;
  fIsInsideTwain := False;
end;


procedure UpdateImageEnActionsLanguage(Component: TComponent);
var
 i: integer;
begin
  {$IFDEF Delphi6orNewer}
  if Supports(Component, IIELanguageUpdatable) then
    (Component as IIELanguageUpdatable).UpdateLanguage();
  {$ENDIF}
  for i := 0 to Component.ComponentCount - 1 do
    UpdateImageEnActionsLanguage(Component.Components[i]);
end;

procedure TIEImageEnGlobalSettings.SetMsgLanguage(value: TMsgLanguage);
begin
  fMsgLanguage := value;
  UpdateImageEnActionsLanguage(Application);
end;


///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////


end.
