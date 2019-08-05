// ie.inc

(*
File version 1026
*)


{.$define IEDEBUG}


{$A+} // like A8, aligned on quad word boundaries
{$T-} // result of the @ operator is always an untyped pointer
{$B-} // short-circuit Boolean expression evaluation
{$R-} // range check OFF
{$Q-} // overflow check OFF

//////////////////////////////////////////////////////////////
// Specify the modules to include
//////////////////////////////////////////////////////////////

// Support for multi-colum/thumbnail view components (TImageEnMIO, TImageEnMView and TImageEnFolderMView)
{$define IEINCLUDEMULTIVIEW}

// Support acquisition via WIA and Twain devices
{$define IEINCLUDEIEXACQUIRE}

// Support for database components (TImageEnDBVect and TImageEnDBView)
{$define IEINCLUDEDB}

// Support for the image processing dialog (Image effects and color adjustment)
{$define IEINCLUDEDIALOGIP}

// Support for the input/output dialog (shown when clicking the "Advanced" button in the save dialog)
{$define IEINCLUDEDIALOGIO}

// Support for open/save dialogs
{$define IEINCLUDEOPENSAVEDIALOGS}

// Support for print dialogs
{$define IEINCLUDEPRINTDIALOGS}

// WIC - Windows Imaging Component support. Requires IEINCLUDEWIA and IEINCLUDEDIRECTSHOW
{$define IEINCLUDEWIC}

// DirectShow support (requires IEINCLUDEWIA)
{$define IEINCLUDEDIRECTSHOW}

// Media Foundation support (requires IEINCLUDEWIA)
{$define IEINCLUDEMEDIAFOUNDATION}

// Includes Color Management System (LCMS) - Default: disabled
{$define IEINCLUDECMS}

// Includes over 150 extra transition effects
{$define IEINCLUDEEXTRATRANSITIONS}

// Include preset images (used to add special border effects in TImageEnMView
{$define IEINCLUDEPRESETIMAGES}

// register QuickReport ImageEn components - Default: disabled
{.$define IEREGISTERQR}

// Use TIEBufferedReadStream instead of TStream to read files
{$define IEUSEBUFFEREDSTREAM}

// Include TIENeurNet class - Default: disabled
{.$define IEINCLUDENEURALNET}

// Support for TIEResourceExtractor class
{$define IEINCLUDERESOURCEEXTRACTOR}

// Support for TIEHashStream class (and TIEBitmap.GetHash)
{$define IEINCLUDEHASHSTREAM}

// Support for TIEImagingAnnot (and related services)
{$define IEINCLUDEIMAGINGANNOT}

// Support for TIETIFFHandler (and related services)
{$define IEINCLUDETIFFHANDLER}

// Support for FFT processing
{$define IEINCLUDEFFT}

// Support for RFB (VNC client) protocol
{$define IERFBPROTOCOL}

// Use GDI+
{$define IEUSEGDIPLUS}

// Support for TIESlippyMap
{$define IEINCLUDESLIPPYMAP}

// Retrieve thumnails for video files from Windows Explorer in TImageEnMView
// Also adds support for large Icons
{$DEFINE VIDEO_THUMBNAILS}

// Include a set of ImageEn TAction descendents for rapid UI development
{$DEFINE IEINCLUDEACTIONS}

// Include TIEEquirectangularRenderer
{$define IEINCLUDEEQUIRECTANGULARRENDERER}

// Make ImageEn support VCL theming
{.$define SupportVclThemes}


//////////////////////////////////////////////////////////////
// Format support
//////////////////////////////////////////////////////////////

// Include support for Camera Raw formats
{$define IEINCLUDERAWFORMATS}

// Support for DICOM file format
{$define IEINCLUDEDICOM}

// Support for PDF writing
{$define IEINCLUDEPDFWRITING}

// Support for PSD file format
{$define IEINCLUDEPSD}

// Include support for Jpeg2000
{$define IEINCLUDEJPEG2000}

// Include support for PNG  (requires IEINCLUDEZLIB under Delphi)
{$define IEINCLUDEPNG}

// ZLib support (required by some formats, such as PNG)
{$define IEINCLUDEZLIB}

// Misc plugins support (iemiscplugins unit, TIEMiscPluginsImageMagick class, etc..)
{$DEFINE IEINCLUDEMISCPLUGINS}


//////////////////////////////////////////////////////////////
// Deprecated modules
//////////////////////////////////////////////////////////////

// Include support for legacy methods and components which are outdated or superceded
{$DEFINE IEINCLUDEDEPRECATEDMETHODS}

// Support "Video for Windows" video capture (TImageEnVideoCap and TImageEnVideoView) - DEPRECATED
// Note: VFW is deprecated and not supported by newer versions of Windows
// See also: "IEINCLUDEDEPRECATEDMETHODS" define
{$define IEINCLUDEVIDEOCAPTURE}

// Support for the TImageEn component - DEPRECATED
// Note: The TImageEn component is only included for legacy applications. TImageEnView is now used instead of TImageEn
// See also: "IEINCLUDEDEPRECATEDMETHODS" define
{$define IEINCLUDETIMAGEEN}


//////////////////////////////////////////////////////////////
// Conditional modules
//////////////////////////////////////////////////////////////

{$ifndef IEINCLUDEDEPRECATEDMETHODS}
  {$undef IEINCLUDEVIDEOCAPTURE}
  {$undef IEINCLUDETIMAGEEN}
{$endif}

{$ifndef IEINCLUDEPNG}
  {$undef IEINCLUDEPRESETIMAGES}
  {$undef IEINCLUDEZLIB}
{$endif}

{$ifndef IEINCLUDEZLIB}
  {$undef IEINCLUDEPNG}
  {$undef IEINCLUDEPRESETIMAGES}
{$endif}


//////////////////////////////////////////////////////////////
// Supported languages
//////////////////////////////////////////////////////////////

{.$define IEUSESTRINGRESOURCEONLY}   // leave only English

{$ifndef IEUSESTRINGRESOURCEONLY}
  {$define IESUPPORTENGLISH}          // English
  {$define IESUPPORTITALIAN}          // Italian
  {$define IESUPPORTGERMAN}           // German
  {$define IESUPPORTSPANISH}          // Spanish
  {$define IESUPPORTFRENCH}           // French
  {$define IESUPPORTPORTUGUESE}       // Portuguese
  {$define IESUPPORTGREEK}            // Greek
  {$define IESUPPORTRUSSIAN}          // Russian
  {$define IESUPPORTDUTCH}            // Dutch
  {$define IESUPPORTSWEDISH}          // Swedish
  {$define IESUPPORTPOLISH}           // Polish
  {$define IESUPPORTJAPANESE}         // Japanese
  {$define IESUPPORTCZECH}            // Czech
  {$define IESUPPORTFINNISH}          // Finnish
  {$define IESUPPORTFARSI}            // Farsi
  {$define IESUPPORTCHINESE}          // Chinese and Chinese Traditional
  {$define IESUPPORTDANISH}           // Danish
  {$define IESUPPORTTURKISH}          // Turkish
  {$define IESUPPORTKOREAN}           // Korean
  {$define IESUPPORTHUNGARIAN}        // Hungarian
  {$define IESUPPORTARABIC}           // Arabic
  {$define IESUPPORTSERBIAN}          // Serbian
  {$define IESUPPORTNORWEGIAN}        // Norwegian
  {$define IESUPPORTUSER}             // User--
{$else}
  {$define IESUPPORTENGLISH}
{$endif}




//////////////////////////////////////////////////////////////
// Component Behavior
//////////////////////////////////////////////////////////////

{$define IETWAINTASKWINDOWS}                // Enhance Twain compatibility
{.$define IEREGISTERTPICTUREFORMATS}        // Register ImageEn file formats in the VCL (e.g. so they can be accessed using a TImage)
{.$define THEMED_BORDERS}                   // TImageEnView, TImageEnMView and TImageEnVect will be drawn with a theme border


//////////////////////////////////////////////////////////////
// detect Borland compiler version
//////////////////////////////////////////////////////////////

// C++Builder 5 or Delphi 5
{$ifdef VER130}
  {$define Delphi5orNewer}
  {$define Delphi7orOlder}
  {$define Delphi2006orOlder}
  {$define IEHASINT64}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEINCLUDEFLATSB}         // Use FlatSB unit
  {$define IESUPPORTOVERLOAD}
  {$define IEHASTHREADSBUG}
  {$define IEHASFREEANDNIL}
  {$define IEOLDSEEKDEF}
  {$ifndef BCB}
    {$define IEUSEFILTEDIT}
    {$define IEUSEDSGNINTF}
    {$define IEREGISTERPROPERTYEDITOR}
  {$else}
    {$define IEHASNOTWINDOWSALPHABLEND}
  {$endif}
  {$define IEUSEASM}

  {$undef IEINCLUDEMISCPLUGINS}
  {$undef IEINCLUDEMEDIAFOUNDATION}
  {$undef SupportVclThemes}
{$endif}

// C++Builder 6 or Delphi 6
{$ifdef VER140}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orOlder}
  {$define Delphi2006orOlder}
  {$define IEHASINT64}
  {$define IEHASANSISTRINGBUG}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEINCLUDEFLATSB}         // Use FlatSB unit
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IESUPPORTDEPRECATED}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASTYPES}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEUSEASM}
  {$ifndef BCB}
    {$define IEVISION}
    {$define IEUSEFILTEDIT}
    //{$define IEUSEDSGNINTF}
    //{$define IEREGISTERPROPERTYEDITOR}
  {$else}
    {$define IEHASNOTWINDOWSALPHABLEND}
  {$endif}

  {$undef IEINCLUDEMISCPLUGINS}
  {$undef IEINCLUDEMEDIAFOUNDATION}
  {$undef SupportVclThemes}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi 7
{$ifdef VER150}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi7orOlder}
  {$define Delphi2006orOlder}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASTYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEVISION}
  {$define IEHASUTF8ENCODEDECODE}

  {$undef SupportVclThemes}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi 2005
{$ifdef VER170}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orOlder}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASUTF8ENCODEDECODE}

  {$undef SupportVclThemes}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// C++Builder 2006 or Delphi 2006
{$ifdef VER180}
  {$ifndef VER185}
    {$define Delphi5orNewer}
    {$define Delphi6orNewer}
    {$define Delphi7orNewer}
    {$define Delphi2005orNewer}
    {$define Delphi2006orNewer}
    {$define Delphi2006orOlder}
    {$define IEFIXPREVIEWS}
    {$define IEFIXUPDATE}
    {$define IEFIXOSDIALOGS}
    {$define IESUPPORTDEFPARAMS}
    {$define IESUPPORTANCHORS}
    {$define IEMOUSEWHEELEVENTS}
    {$define IEHASINT64}
    {$define IEHASUINT64}
    {$define IEHASNATIVEINT}
    {$define IEINCLUDEFLATSB}     // Use FlatSB unit
    {$define IESUPPORTINLINE}
    {$define IESUPPORTOVERLOAD}
    {$define IENEWVARIANTS}
    {$define IEHASFREEANDNIL}
    {$define IEMMXSUPPORTED}
    {$define IEHASTTHREADSTATICSYNCHRONIZE}
    {$define IEHASCONSTENUM}
    {$define IEHASAFTERCONSTRUCTION}
    {$define IEHASACTIVEFORMHANDLE}
    {$define IEHASTYPES}
    {$define IESUPPORTDEPRECATED}
    {$define IESUPPORTINTERFACESUPPORT}
    {$define IEHASFOCUSSTATE}
    {$define IEHASOPENOPTIONSEX}
    {$define IEHASNEEDSBORDERPAINT}
    {$define IEHASPARENTBACKGROUND}
    {$define IEUSEDESIGNINTF}
    {$define IEUSEFILTEDIT}
    {$define IEREGISTERPROPERTYEDITOR}
    {$define IEUSEASM}
    {$define IEVISION}
    {$define IEHASPOSEX}
    {$define IEHASSTRUTILS}
    {$define IEHASRECORDASCLASSES}
    {$define IEHASUTF8ENCODEDECODE}

    {$undef SupportVclThemes}

    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN UNIT_PLATFORM OFF}
  {$endif}
{$endif}


// Delphi 2007 or C++Builder 2007
{$ifdef VER185}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUTF8ENCODEDECODE}

  {$undef SupportVclThemes}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi 2009 or C++Builder 2009
{$ifdef VER200}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEHASUTF8ENCODEDECODE}

  {$undef SupportVclThemes}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi 2010 or C++Builder 2010
{$ifdef VER210}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEHASUTF8ENCODEDECODE}

  {$undef SupportVclThemes}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}



// Delphi XE or C++Builder XE
{$ifdef VER220}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$undef SupportVclThemes}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}


// Delphi XE2 or C++Builder XE2
{$ifdef VER230}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}



// Delphi XE3 or C++Builder XE3
{$ifdef VER240}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}




// Delphi XE4 or C++ Builder XE4
{$ifdef VER250}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}





// Delphi XE5 or C++ Builder XE5
{$ifdef VER260}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define DelphiXE5orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}



// Delphi XE6 or C++ Builder XE6
{$ifdef VER270}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define DelphiXE5orNewer}
  {$define DelphiXE6orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi XE7 or C++ Builder XE7
{$ifdef VER280}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define DelphiXE5orNewer}
  {$define DelphiXE6orNewer}
  {$define DelphiXE7orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi XE8 or C++ Builder XE8
{$ifdef VER290}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define DelphiXE5orNewer}
  {$define DelphiXE6orNewer}
  {$define DelphiXE7orNewer}
  {$define DelphiXE8orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi XE10 or C++ Builder XE10
{$ifdef VER300}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define DelphiXE5orNewer}
  {$define DelphiXE6orNewer}
  {$define DelphiXE7orNewer}
  {$define DelphiXE8orNewer}
  {$define DelphiXE10orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi XE10 or C++ Builder XE10
{$ifdef VER310}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define DelphiXE5orNewer}
  {$define DelphiXE6orNewer}
  {$define DelphiXE7orNewer}
  {$define DelphiXE8orNewer}
  {$define DelphiXE10orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi XE10 or C++ Builder XE10
{$ifdef VER320}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define DelphiXE5orNewer}
  {$define DelphiXE6orNewer}
  {$define DelphiXE7orNewer}
  {$define DelphiXE8orNewer}
  {$define DelphiXE10orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// Delphi XE10 or C++ Builder XE10
{$ifdef VER330}
  {$define Delphi5orNewer}
  {$define Delphi6orNewer}
  {$define Delphi7orNewer}
  {$define Delphi2005orNewer}
  {$define Delphi2006orNewer}
  {$define Delphi2007orNewer}
  {$define Delphi2009orNewer}
  {$define Delphi2010orNewer}
  {$define DelphiXEorNewer}
  {$define DelphiXE2orNewer}
  {$define DelphiXE3orNewer}
  {$define DelphiXE4orNewer}
  {$define DelphiXE5orNewer}
  {$define DelphiXE6orNewer}
  {$define DelphiXE7orNewer}
  {$define DelphiXE8orNewer}
  {$define DelphiXE10orNewer}
  {$define IEFIXPREVIEWS}
  {$define IEFIXUPDATE}
  {$define IEFIXOSDIALOGS}
  {$define IESUPPORTDEFPARAMS}
  {$define IESUPPORTANCHORS}
  {$define IEMOUSEWHEELEVENTS}
  {$define IEHASINT64}
  {$define IEHASUINT64}
  {$define IEHASNATIVEINT}
  {$define IEINCLUDEFLATSB}     // Use FlatSB unit
  {$define IESUPPORTINLINE}
  {$define IESUPPORTOVERLOAD}
  {$define IENEWVARIANTS}
  {$define IEHASFREEANDNIL}
  {$define IEMMXSUPPORTED}
  {$define IEHASTTHREADSTATICSYNCHRONIZE}
  {$define IEHASTTHREADSTART}
  {$define IEHASCONSTENUM}
  {$define IEHASAFTERCONSTRUCTION}
  {$define IEHASACTIVEFORMHANDLE}
  {$define IEHASTYPES}
  {$define IEHASUITYPES}
  {$define IESUPPORTDEPRECATED}
  {$define IESUPPORTINTERFACESUPPORT}
  {$define IEHASFORMATSETTINGS}
  {$define IEHASFOCUSSTATE}
  {$define IEHASOPENOPTIONSEX}
  {$define IEHASNEEDSBORDERPAINT}
  {$define IEHASPARENTBACKGROUND}
  {$define IEUSENEWSENDMESSAGETIMEOUT}
  {$define IEUSEDESIGNINTF}
  {$define IEUSEFILTEDIT}
  {$define IEREGISTERPROPERTYEDITOR}
  {$define IEUSEASM}
  {$define IEVISION}
  {$define IEHASPOSEX}
  {$define IEHASSTRUTILS}
  {$define IEHASRECORDASCLASSES}
  {$define IEHASUNICODEWARNS}
  {$define IEUSEVCLZLIB}
  {$define IEHASTOUCH}
  {$define IEHASPLATFORMATTRIBUTE}
  {$define IEHASONGESTURE}
  {$define IEHASUTF8ENCODEDECODE}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

//////////////////////////////////////////////////////////////
// 64 bit related
//////////////////////////////////////////////////////////////

// test
{.$undef IEUSEASM}
{.$define IEUSEDLLJPEGLIB}
{.$define IEUSEDLLPNGLIB}
{.$define IEUSEDLLJPEG2000LIB}
{.$define IEUSEDLLRAWLIB}

{$ifdef WIN64}
  {$undef IEUSEASM}
  {$define IEUSEDLLJPEGLIB}     // Use DLL Jpeg library (otherwise use linked jpeg library)
  {$define IEUSEDLLPNGLIB}      // Use DLL PNG library (otherwise use linked png library)
  {$define IEUSEDLLJPEG2000LIB} // Use DLL Jpeg2000 (Jasper) library (otherwise use linked jasper library)
  {$define IEUSEDLLRAWLIB}      // Use DLL RAW library (otherwise use embedded RAW library)
  {$undef IEUSEDESIGNINTF}
  {$undef IEUSEFILTEDIT}
  {$undef IEREGISTERPROPERTYEDITOR}
{$endif}


//////////////////////////////////////////////////////////////
// C++ Builder related
//////////////////////////////////////////////////////////////

{$ifdef BCB}
  {$ObjExportAll On}
{$endif}


//////////////////////////////////////////////////////////////
// Target selection
//////////////////////////////////////////////////////////////

// SPECIALS
{.$define OCXVERSION}
{.$define IEDOTNETVERSION}

{$ifdef IEDOTNETVERSION}
  {$define OCXVERSION}
{$endif}

{$ifdef OCXVERSION}
  {$undef IEINCLUDEDB}
{$endif}



