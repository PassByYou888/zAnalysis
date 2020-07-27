{------------------------------------------------------------------------------}
{                                                                              }
{  ImageEn Registry and Ini File Helper Functions                              }
{  Requires: Delphi 2005 or newer                                              }
{                                                                              }
{  Nigel Cross                                                                 }
{  Xequte Software                                                             }
{  nigel@xequte.com                                                            }
{  http://www.xequte.com                                                       }
{                                                                              }
{  © Xequte Software 2011-2014                                                 }
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
File version 1003
*)

unit iexRegistryFunctions;

interface   

{$I ie.inc}




uses
  Windows, Graphics, Classes,
  {$ifdef IEHASTYPES} Types, {$endif} 
  {$IFDEF IEINCLUDEIEXACQUIRE} iexAcquire, {$ENDIF}
  ImageEnProc, hyiedefs, ImageEnIO, ieMIO, ImageEnView, IEMView;


{$IFDEF Delphi2005orNewer}
type
  // TImageEnIO Helper Functions
  TImageEnIOHelper = class helper for TImageEnIO
  private
  public
    function LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
    function SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;     
    function LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
    function SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
  end;
  
  // TImageEnMIO Helper Functions
  TImageEnMIOHelper = class helper for TImageEnMIO
  private
  public
    function LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
    function SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;   
    function LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
    function SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
  end;

  // TIPDialogParams Helper Functions
  TIPDialogParamsHelper = class helper for TIPDialogParams
  private
  public
    function LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
    function SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;   
    function LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
    function SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
  end;

  // TIOPrintPreviewParams Helper Functions
  TIOPrintPreviewParamsHelper = class helper for TIOPrintPreviewParams
  private
  public
    function LoadFromRegistry(const sKey : string; const sPrefix : string = 'IEV'; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
    function SaveToRegistry(const sKey : string; const sPrefix : string = 'IEV'; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;   
    function LoadFromIniFile(const sFilename : string; const sPrefix : string = 'IEV'; sSection : string = ''): boolean;
    function SaveToIniFile(const sFilename : string; const sPrefix : string = 'IEV'; sSection : string = ''): boolean;
  end;

  // TImageEnProc Helper Functions
  TImageEnProcHelper = class helper for TImageEnProc
  private
  public
    function LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
    function SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;  
    function LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
    function SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
  end;

  // TImageEnView Helper Functions
  TImageEnViewHelper = class helper for TImageEnView
  private
  public
    function LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
    function SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;   
    function LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
    function SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
  end;

  // TImageEnMView Helper Functions
  TImageEnMViewHelper = class helper for TImageEnMView
  private
  public
    function LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
    function SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;     
    function LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
    function SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
  end;
                    
  {$IFDEF IEINCLUDEIEXACQUIRE}
  // TIEAcquireParams Helper Functions
  TIEAcquireParamsHelper = class helper for TIEAcquireParams
  private
  public
    function LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
    function SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;     
    function LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
    function SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
  end;
  {$ENDIF}

{$ENDIF}


implementation

uses
  Registry, IniFiles, SysUtils, hyieutils, iesettings, iexFolderMView;

const
  // Registry Values
  IE_Registry_Language                   = 'IELanguage';
  IE_Registry_DialogsMeasureUnit         = 'IEDialogsMeasureUnit';
  IE_Registry_IPP_Prefix                 = 'IEIPP';
  IE_Registry_PPP_Prefix                 = 'PPP';
  IE_Registry_AutoAdjustDPI              = 'IEAutoAdjustDPI';
  IE_Registry_IO_JPEG_Quality            = 'IEIOJPEGQuality';
  IE_Registry_IO_EnableAdjustOrientation = 'IEIOEnableAdjustOrientation';
  IE_Registry_SelectedAcquireSource      = 'IESelectedAcquireSource';
  IE_Registry_IEVAutoShrink              = 'IEVAutoShrink';
  IE_Registry_IEVAutoStretch             = 'IEVAutoStretch';
  IE_Registry_IEVCenter                  = 'IEVCenter';
  IE_Registry_IEVBackgroundStyle         = 'IEVBackgroundStyle';
  IE_Registry_IEVBackground              = 'IEVBackground';
  IE_Registry_IEVGradientEndColor        = 'IEVGradientEndColor';
  IE_Registry_IEVMouseInteract_Prefix    = 'IEVMouseInteract';
  IE_Registry_IEVZoomFilter              = 'IEVZoomFilter';
  IE_Registry_IEVPlayLoop                = 'IEVPlayLoop';
  IE_Registry_IEMHorizBorder             = 'HorizBorder';
  IE_Registry_IEMVertBorder              = 'VertBorder';
  IE_Registry_IEMThumbWidth              = 'ThumbWidth';
  IE_Registry_IEMThumbHeight             = 'ThumbHeight';
  IE_Registry_IEMEnableAdjustOrientation = 'IEMEnableAdjustOrientation';
  IE_Registry_IEMBackgroundStyle         = 'IEMBackgroundStyle';
  IE_Registry_IEMBackground              = 'IEMBackground';
  IE_Registry_IEMGradientEndColor        = 'IEMGradientEndColor';
  IE_Registry_IEMResampleFilter          = 'IEMResampleFilter';
  IE_Registry_IEMPlayLoop                = 'IEMPlayLoop';
  IE_Registry_IEFExclusionMask           = 'IEFExclusionMask';
  IE_Registry_IEFFileTypes               = 'IEFFileTypes';
  IE_Registry_IEFFileTypesMask           = 'IEFFileTypesMask';
  IE_Registry_IEFFolder                  = 'IEFFolder';
  IE_Registry_IEFFolderOptions_Hidden    = 'IEFFolderOptionsHidden';
  IE_Registry_IEFFolderOptions_Folders   = 'IEFFolderOptionsFolders';
  IE_Registry_IEFSortAscending           = 'IEFSortAscending';
  IE_Registry_IEFSortOrder               = 'IEFSortOrder';
  IE_Registry_IEFShowThumbnailHint       = 'IEFShowThumbnailHint';
  IE_Registry_IEFDefaultBottomText       = 'IEFDefaultBottomText';
  IE_Registry_IEFDefaultInfoText         = 'IEFDefaultInfoText';
  IE_Registry_IEFDefaultTopText          = 'IEFDefaultTopText';
  IE_Registry_SelectedSource             = 'SelectedSource';
  IE_Registry_VisibleDialog              = 'VisibleDialog';
  IE_Registry_AcquireFrameEnabled        = 'AcquireFrameEnabled';
  IE_Registry_AcquireFrameBottom         = 'AcquireFrameBottom';
  IE_Registry_AcquireFrameLeft           = 'AcquireFrameLeft';
  IE_Registry_AcquireFrameRight          = 'AcquireFrameRight';
  IE_Registry_AcquireFrameTop            = 'AcquireFrameTop';
  IE_Registry_AutoFeed                   = 'AutoFeed';
  IE_Registry_BitDepth                   = 'BitDepth';
  IE_Registry_Brightness                 = 'Brightness';
  IE_Registry_Contrast                   = 'Contrast';
  IE_Registry_DuplexEnabled              = 'DuplexEnabled';
  IE_Registry_FeederEnabled              = 'FeederEnabled';
  IE_Registry_Orientation                = 'Orientation';
  IE_Registry_PixelType                  = 'PixelType';
  IE_Registry_Rotation                   = 'Rotation';
  IE_Registry_Threshold                  = 'Threshold';
  IE_Registry_XResolution                = 'XResolution';
  IE_Registry_YResolution                = 'YResolution';

  // Ini File Sections
  ImageEnIO_Default_Section              = 'IeIOProperties';
  ImageEnMIO_Default_Section             = 'IeMIOProperties';
  IPDialogParams_Default_Section         = 'IeIPParameters';
  IOPrintPreviewParams_Default_Section   = 'IePrintParameters';
  ImageEnProc_Default_Section            = 'IeProcProperties';
  ImageEnView_Default_Section            = 'IeViewProperties';
  ImageEnMView_Default_Section           = 'IeMViewProperties';
  AcquireParams_Default_Section          = 'IeAcquireProperties';





{$IFDEF Delphi2005orNewer}

function TImageEnIOHelper.LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
  sData: string;      
  ADevice : TIEAcquireSource;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_READ;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, FALSE) = False then
        exit;

      // AutoAdjustDPI
      sData := WRegistry.ReadString(IE_Registry_AutoAdjustDPI);
      if sData <> '' then
        AutoAdjustDPI := IEStr2BoolS(sData);

      // Params.JPEG_Quality
      sData := WRegistry.ReadString(IE_Registry_IO_JPEG_Quality);
      if sData <> '' then
        Params.JPEG_Quality := StrToIntDef(sData, 75);

      // Params.EnableAdjustOrientation
      sData := WRegistry.ReadString(IE_Registry_IO_EnableAdjustOrientation);
      if sData <> '' then
        Params.EnableAdjustOrientation := IEStr2BoolS(sData);

      {$IFDEF IEINCLUDEIEXACQUIRE}
      // SelectedAcquireSource
      sData := WRegistry.ReadString(IE_Registry_SelectedAcquireSource);
      if sData <> '' then
      begin
        ADevice := StrToAcquireSource(sData);
        SetAcquireSource(ADevice.Api, ADevice.Location);
      end;
      {$ENDIF}

      // MsgLanguage
      sData := WRegistry.ReadString(IE_Registry_Language);
      if sData <> '' then
        IEGlobalSettings().MsgLanguage := TMsgLanguage(StrToIntDef(sData, 0));

      // DialogsMeasureUnit
      sData := WRegistry.ReadString(IE_Registry_DialogsMeasureUnit);
      if sData <> '' then
        DialogsMeasureUnit := TIEDialogsMeasureUnit(StrToIntDef(sData, 0));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;
    
    // Print Preview Params
    if PrintPreviewParams.LoadFromRegistry(sKey, 'IEV', aHKEY) = False then
      raise ERegistryException.create('Param Load Error');
  except
    Result := False;
  end;
end;

function TImageEnIOHelper.SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_ALL_ACCESS;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, True) = False then
        raise ERegistryException.create('Open Error');

      // AutoAdjustDPI
      WRegistry.WriteString(IE_Registry_AutoAdjustDPI, IEBool2StrS(AutoAdjustDPI));

      // Params.JPEG_Quality
      WRegistry.WriteString(IE_Registry_IO_JPEG_Quality, IntToStr(Params.JPEG_Quality));

      // Params.EnableAdjustOrientation
      WRegistry.WriteString(IE_Registry_IO_EnableAdjustOrientation, IEBool2StrS(Params.EnableAdjustOrientation));
                           
      {$IFDEF IEINCLUDEIEXACQUIRE}
      // SelectedAcquireSource
      WRegistry.WriteString(IE_Registry_SelectedAcquireSource, AcquireSourceToStr(SelectedAcquireSource));
      {$ENDIF}
                         
      // MsgLanguage
      WRegistry.WriteString(IE_Registry_Language, IntToStr(Integer(IEGlobalSettings().MsgLanguage)));
      
      // DialogsMeasureUnit
      WRegistry.WriteString(IE_Registry_DialogsMeasureUnit, IntToStr(Integer(DialogsMeasureUnit)));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;

    // Print Preview Params
    if PrintPreviewParams.SaveToRegistry(sKey, 'IEV', aHKEY) = False then
      raise ERegistryException.create('Param Save Error');
  except
    Result := False;
  end;
end;


function TImageEnIOHelper.LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
  sData: string;      
  ADevice : TIEAcquireSource;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnIO_Default_Section;

      // AutoAdjustDPI
      sData := aIniFile.ReadString(sSection, IE_Registry_AutoAdjustDPI, '');
      if sData <> '' then
        AutoAdjustDPI := IEStr2BoolS(sData);

      // Params.JPEG_Quality
      sData := aIniFile.ReadString(sSection, IE_Registry_IO_JPEG_Quality, '');
      if sData <> '' then
        Params.JPEG_Quality := StrToIntDef(sData, 75);

      // Params.EnableAdjustOrientation
      sData := aIniFile.ReadString(sSection, IE_Registry_IO_EnableAdjustOrientation, '');
      if sData <> '' then
        Params.EnableAdjustOrientation := IEStr2BoolS(sData);

      {$IFDEF IEINCLUDEIEXACQUIRE}
      // SelectedAcquireSource
      sData := aIniFile.ReadString(sSection, IE_Registry_SelectedAcquireSource, '');
      if sData <> '' then
      begin
        ADevice := StrToAcquireSource(sData);
        SetAcquireSource(ADevice.Api, ADevice.Location);
      end;
      {$ENDIF}

      // MsgLanguage
      sData := aIniFile.ReadString(sSection, IE_Registry_Language, '');
      if sData <> '' then
        IEGlobalSettings().MsgLanguage := TMsgLanguage(StrToIntDef(sData, 0));

      // DialogsMeasureUnit
      sData := aIniFile.ReadString(sSection, IE_Registry_DialogsMeasureUnit, '');
      if sData <> '' then
        DialogsMeasureUnit := TIEDialogsMeasureUnit(StrToIntDef(sData, 0));

    finally
      aIniFile.free;
    end;
    
    // Print Preview Params
    if PrintPreviewParams.LoadFromIniFile(sFilename, 'IEV', sSection, ) = False then
      raise ERegistryException.create('Param Load Error');
  except
    Result := False;
  end;
end;

function TImageEnIOHelper.SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnIO_Default_Section;

      // AutoAdjustDPI
      aIniFile.WriteString(sSection, IE_Registry_AutoAdjustDPI, IEBool2StrS(AutoAdjustDPI));

      // Params.JPEG_Quality
      aIniFile.WriteString(sSection, IE_Registry_IO_JPEG_Quality, IntToStr(Params.JPEG_Quality));

      // Params.EnableAdjustOrientation
      aIniFile.WriteString(sSection, IE_Registry_IO_EnableAdjustOrientation, IEBool2StrS(Params.EnableAdjustOrientation));
                           
      {$IFDEF IEINCLUDEIEXACQUIRE}
      // SelectedAcquireSource
      aIniFile.WriteString(sSection, IE_Registry_SelectedAcquireSource, AcquireSourceToStr(SelectedAcquireSource));
      {$ENDIF}
                         
      // MsgLanguage
      aIniFile.WriteString(sSection, IE_Registry_Language, IntToStr(Integer(IEGlobalSettings().MsgLanguage)));

      // DialogsMeasureUnit
      aIniFile.WriteString(sSection, IE_Registry_DialogsMeasureUnit, IntToStr(Integer(DialogsMeasureUnit)));

      aIniFile.UpdateFile;
    finally
      aIniFile.free;
    end;

    // Print Preview Params
    if PrintPreviewParams.SaveToIniFile(sFilename, 'IEV', sSection, ) = False then
      raise ERegistryException.create('Param Save Error');
  except
    Result := False;
  end;
end;

            
// TImageEnMIOHelper Helper Functions

function TImageEnMIOHelper.LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
  sData: string;      
  ADevice : TIEAcquireSource;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_READ;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, FALSE) = False then
        exit;

      // AutoAdjustDPI
      sData := WRegistry.ReadString(IE_Registry_AutoAdjustDPI);
      if sData <> '' then
        AutoAdjustDPI := IEStr2BoolS(sData);

      {$IFDEF IEINCLUDEIEXACQUIRE}
      // SelectedAcquireSource
      sData := WRegistry.ReadString(IE_Registry_SelectedAcquireSource);
      if sData <> '' then
      begin
        ADevice := StrToAcquireSource(sData);
        SetAcquireSource(ADevice.Api, ADevice.Location);
      end;
      {$ENDIF}

      // MsgLanguage
      sData := WRegistry.ReadString(IE_Registry_Language);
      if sData <> '' then
        IEGlobalSettings().MsgLanguage := TMsgLanguage(StrToIntDef(sData, 0));

      // DialogsMeasureUnit
      sData := WRegistry.ReadString(IE_Registry_DialogsMeasureUnit);
      if sData <> '' then
        DialogsMeasureUnit := TIEDialogsMeasureUnit(StrToIntDef(sData, 0));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;
    
    // Print Preview Params
    if PrintPreviewParams.LoadFromRegistry(sKey, 'IEM', aHKEY) = False then
      raise ERegistryException.create('Param Load Error');
  except
    Result := False;
  end;
end;

function TImageEnMIOHelper.SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_ALL_ACCESS;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, True) = False then
        raise ERegistryException.create('Open Error');

      // AutoAdjustDPI
      WRegistry.WriteString(IE_Registry_AutoAdjustDPI, IEBool2StrS(AutoAdjustDPI));
                           
      {$IFDEF IEINCLUDEIEXACQUIRE}
      // SelectedAcquireSource
      WRegistry.WriteString(IE_Registry_SelectedAcquireSource, AcquireSourceToStr(SelectedAcquireSource));
      {$ENDIF}
                         
      // MsgLanguage
      WRegistry.WriteString(IE_Registry_Language, IntToStr(Integer(IEGlobalSettings().MsgLanguage)));
      
      // DialogsMeasureUnit
      WRegistry.WriteString(IE_Registry_DialogsMeasureUnit, IntToStr(Integer(DialogsMeasureUnit)));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;
                 
    // Print Preview Params
    if PrintPreviewParams.SaveToRegistry(sKey, 'IEM', aHKEY) = False then
      raise ERegistryException.create('Param Save Error');
  except
    Result := False;
  end;
end;


function TImageEnMIOHelper.LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
  sData: string;      
  ADevice : TIEAcquireSource;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnMIO_Default_Section;

      // AutoAdjustDPI
      sData := aIniFile.ReadString(sSection, IE_Registry_AutoAdjustDPI, '');
      if sData <> '' then
        AutoAdjustDPI := IEStr2BoolS(sData);

      {$IFDEF IEINCLUDEIEXACQUIRE}
      // SelectedAcquireSource
      sData := aIniFile.ReadString(sSection, IE_Registry_SelectedAcquireSource, '');
      if sData <> '' then
      begin
        ADevice := StrToAcquireSource(sData);
        SetAcquireSource(ADevice.Api, ADevice.Location);
      end;
      {$ENDIF}

      // MsgLanguage
      sData := aIniFile.ReadString(sSection, IE_Registry_Language, '');
      if sData <> '' then
        IEGlobalSettings().MsgLanguage := TMsgLanguage(StrToIntDef(sData, 0));

      // DialogsMeasureUnit
      sData := aIniFile.ReadString(sSection, IE_Registry_DialogsMeasureUnit, '');
      if sData <> '' then
        DialogsMeasureUnit := TIEDialogsMeasureUnit(StrToIntDef(sData, 0));

    finally
      aIniFile.free;
    end;
    
    // Print Preview Params
    if PrintPreviewParams.LoadFromIniFile(sFilename, 'IEM', sSection, ) = False then
      raise ERegistryException.create('Param Load Error');
  except
    Result := False;
  end;
end;

function TImageEnMIOHelper.SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnMIO_Default_Section;

      // AutoAdjustDPI
      aIniFile.WriteString(sSection, IE_Registry_AutoAdjustDPI, IEBool2StrS(AutoAdjustDPI));
                           
      {$IFDEF IEINCLUDEIEXACQUIRE}
      // SelectedAcquireSource
      aIniFile.WriteString(sSection, IE_Registry_SelectedAcquireSource, AcquireSourceToStr(SelectedAcquireSource));
      {$ENDIF}
                         
      // MsgLanguage
      aIniFile.WriteString(sSection, IE_Registry_Language, IntToStr(Integer(IEGlobalSettings().MsgLanguage)));
      
      // DialogsMeasureUnit
      aIniFile.WriteString(sSection, IE_Registry_DialogsMeasureUnit, IntToStr(Integer(DialogsMeasureUnit)));

      aIniFile.UpdateFile;
    finally
      aIniFile.free;
    end;
                 
    // Print Preview Params
    if PrintPreviewParams.SaveToIniFile(sFilename, 'IEM', sSection, ) = False then
      raise ERegistryException.create('Param Save Error');
  except
    Result := False;
  end;
end;



// TIPDialogParams Helper Functions

function TIPDialogParamsHelper.LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
  sData: string;
  i: Integer;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_READ;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, FALSE) = False then
        exit;

      for i := Low(IPP_Property_List) to High(IPP_Property_List) do
      begin
        sData := WRegistry.ReadString(IE_Registry_IPP_Prefix + IPP_Property_List[i]);
        if sData <> '' then
          SetProperty(IPP_Property_List[i], sData);
      end;
      
      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;
  except
    Result := False;
  end;
end;

function TIPDialogParamsHelper.SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var          
  WRegistry : TRegistry;
  i: Integer;
begin        
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_ALL_ACCESS;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, True) = False then
        raise ERegistryException.create('Open Error');

      for i := Low(IPP_Property_List) to High(IPP_Property_List) do
        WRegistry.WriteString(IE_Registry_IPP_Prefix + IPP_Property_List[i], GetProperty(IPP_Property_List[i]));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;
  except
    Result := False;
  end;
end;


function TIPDialogParamsHelper.LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
  sData: string;
  i: Integer;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := IPDialogParams_Default_Section;

      for i := Low(IPP_Property_List) to High(IPP_Property_List) do
      begin
        sData := aIniFile.ReadString(sSection, IE_Registry_IPP_Prefix + IPP_Property_List[i], '');
        if sData <> '' then
          SetProperty(IPP_Property_List[i], sData);
      end;
      
    finally
      aIniFile.free;
    end;
  except
    Result := False;
  end;
end;

function TIPDialogParamsHelper.SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
var          
  aIniFile : TMemIniFile;
  i: Integer;
begin        
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := IPDialogParams_Default_Section;

      for i := Low(IPP_Property_List) to High(IPP_Property_List) do
        aIniFile.WriteString(sSection, IE_Registry_IPP_Prefix + IPP_Property_List[i], GetProperty(IPP_Property_List[i]));

      aIniFile.UpdateFile;
    finally
      aIniFile.free;
    end;
  except
    Result := False;
  end;
end;


// TIOPrintPreviewParams Helper Functions

function TIOPrintPreviewParamsHelper.LoadFromRegistry(const sKey : string; const sPrefix : string = 'IEV'; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
  sData: string;
  i: Integer;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_READ;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, FALSE) = False then
        exit;

      for i := Low(PPP_Property_List) to High(PPP_Property_List) do
      begin
        sData := WRegistry.ReadString(sPrefix + IE_Registry_PPP_Prefix + PPP_Property_List[i]);
        if sData <> '' then
          SetProperty(PPP_Property_List[i], sData);
      end;
      
      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;
  except
    Result := False;
  end;
end;

function TIOPrintPreviewParamsHelper.SaveToRegistry(const sKey : string; const sPrefix : string = 'IEV'; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var          
  WRegistry : TRegistry;
  i: Integer;
begin        
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_ALL_ACCESS;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, True) = False then
        raise ERegistryException.create('Open Error');

      for i := Low(PPP_Property_List) to High(PPP_Property_List) do
        WRegistry.WriteString(sPrefix + IE_Registry_PPP_Prefix + PPP_Property_List[i], GetProperty(PPP_Property_List[i]));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;
  except
    Result := False;
  end;
end;


function TIOPrintPreviewParamsHelper.LoadFromIniFile(const sFilename : string; const sPrefix : string = 'IEV'; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
  sData: string;
  i: Integer;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := IOPrintPreviewParams_Default_Section;

      for i := Low(PPP_Property_List) to High(PPP_Property_List) do
      begin
        sData := aIniFile.ReadString(sSection, sPrefix + IE_Registry_PPP_Prefix + PPP_Property_List[i], '');
        if sData <> '' then
          SetProperty(PPP_Property_List[i], sData);
      end;
      
    finally
      aIniFile.free;
    end;
  except
    Result := False;
  end;
end;

function TIOPrintPreviewParamsHelper.SaveToIniFile(const sFilename : string; const sPrefix : string = 'IEV'; sSection : string = ''): boolean;
var          
  aIniFile : TMemIniFile;
  i: Integer;
begin        
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := IOPrintPreviewParams_Default_Section;

      for i := Low(PPP_Property_List) to High(PPP_Property_List) do
        aIniFile.WriteString(sSection, sPrefix + IE_Registry_PPP_Prefix + PPP_Property_List[i], GetProperty(PPP_Property_List[i]));

      aIniFile.UpdateFile;
    finally
      aIniFile.free;
    end;
  except
    Result := False;
  end;
end;


// TImageEnProc Helper Functions

function TImageEnProcHelper.LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
  sData: string;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_READ;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, FALSE) = False then
        exit;

      sData := WRegistry.ReadString(IE_Registry_Language);
      if sData <> '' then
        IEGlobalSettings().MsgLanguage := TMsgLanguage(StrToIntDef(sData, 0));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;
    
    // Image Processing Params
    if IPDialogParams.LoadFromRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('Param Load Error');
  except
    Result := False;
  end;
end;

function TImageEnProcHelper.SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_ALL_ACCESS;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, True) = False then
        raise ERegistryException.create('Open Error');

      WRegistry.WriteString(IE_Registry_Language, IntToStr(Integer(IEGlobalSettings().MsgLanguage)));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;

    // Image Processing Params
    if IPDialogParams.SaveToRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('Param Save Error');
  except
    Result := False;
  end;
end;


function TImageEnProcHelper.LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
  sData: string;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnProc_Default_Section;

      sData := aIniFile.ReadString(sSection, IE_Registry_Language, '');
      if sData <> '' then
        IEGlobalSettings().MsgLanguage := TMsgLanguage(StrToIntDef(sData, 0));

    finally
      aIniFile.free;
    end;
    
    // Image Processing Params
    if IPDialogParams.LoadFromIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('Param Load Error');
  except
    Result := False;
  end;
end;

function TImageEnProcHelper.SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnProc_Default_Section;

      aIniFile.WriteString(sSection, IE_Registry_Language, IntToStr(Integer(IEGlobalSettings().MsgLanguage)));

      aIniFile.UpdateFile;
    finally
      aIniFile.free;
    end;

    // Image Processing Params
    if IPDialogParams.SaveToIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('Param Save Error');
  except
    Result := False;
  end;
end;


// TImageEnView Helper Functions

function TImageEnViewHelper.LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
  sData: string;
  enum : TIEMouseInteractItems;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_READ;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, FALSE) = False then
        exit;

      // AutoShrink
      sData := WRegistry.ReadString(IE_Registry_IEVAutoShrink);
      if sData <> '' then
        AutoShrink := IEStr2BoolS(sData);

      // AutoStretch
      sData := WRegistry.ReadString(IE_Registry_IEVAutoStretch);
      if sData <> '' then
        AutoStretch := IEStr2BoolS(sData);

      // Center
      sData := WRegistry.ReadString(IE_Registry_IEVCenter);
      if sData <> '' then
        Center := IEStr2BoolS(sData);

      // PlayLoop
      sData := WRegistry.ReadString(IE_Registry_IEVPlayLoop);
      if sData <> '' then
        PlayLoop := IEStr2BoolS(sData);

      // Background
      sData := WRegistry.ReadString(IE_Registry_IEVBackgroundStyle);
      if sData <> '' then
      begin
        BackgroundStyle  := TIEBackgroundStyle(StrToIntDef(sData, 0));
        Background       := IEStr2ColorDefS(WRegistry.ReadString(IE_Registry_IEVBackground), clBlack);
        GradientEndColor := IEStr2ColorDefS(WRegistry.ReadString(IE_Registry_IEVGradientEndColor), clGray);
      end;

      // MouseInteract
      for enum in [Low(TIEMouseInteractItems) .. High(TIEMouseInteractItems)] do
      begin
        sData := WRegistry.ReadString(IE_Registry_IEVMouseInteract_Prefix + IntToStr(Integer(enum)));
        if sData <> '' then
        begin
          if IEStr2BoolS(sData) then
            MouseInteract := MouseInteract + [enum]
          else
            MouseInteract := MouseInteract - [enum];
        end;
      end;

      // ZoomFilter
      sData := WRegistry.ReadString(IE_Registry_IEVZoomFilter);
      if sData <> '' then
        ZoomFilter := TResampleFilter(StrToIntDef(sData, 0));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;

    // PROC
    if Proc.LoadFromRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('Proc Load Error');

    // IO
    if IO.LoadFromRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('IO Load Error');
  except
    Result := False;
  end;
end;

function TImageEnViewHelper.SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;   
  enum : TIEMouseInteractItems;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_ALL_ACCESS;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, True) = False then
        raise ERegistryException.create('Open Error');

      // AutoShrink
      WRegistry.WriteString(IE_Registry_IEVAutoShrink, IEBool2StrS(AutoShrink));

      // AutoStretch
      WRegistry.WriteString(IE_Registry_IEVAutoStretch, IEBool2StrS(AutoStretch));

      // Center
      WRegistry.WriteString(IE_Registry_IEVCenter, IEBool2StrS(Center));

      // PlayLoop
      WRegistry.WriteString(IE_Registry_IEVPlayLoop, IEBool2StrS(PlayLoop));

      // Background
      WRegistry.WriteString(IE_Registry_IEVBackgroundStyle, IntToStr(Integer(BackgroundStyle)));
      WRegistry.WriteString(IE_Registry_IEVBackground, ColorToString(Background));
      WRegistry.WriteString(IE_Registry_IEVGradientEndColor, ColorToString(GradientEndColor));

      // MouseInteract
      for enum in [Low(TIEMouseInteractItems) .. High(TIEMouseInteractItems)] do
        WRegistry.WriteString(IE_Registry_IEVMouseInteract_Prefix + IntToStr(Integer(enum)), IEBool2StrS(enum in MouseInteract));

      // ZoomFilter
      WRegistry.WriteString(IE_Registry_IEVZoomFilter, IntToStr(Integer(ZoomFilter)));


      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;

    // PROC
    if Proc.SaveToRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('Proc Save Error');

    // IO
    if IO.SaveToRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('IO Save Error');
  except
    Result := False;
  end;
end;


function TImageEnViewHelper.LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
  sData: string;
  enum : TIEMouseInteractItems;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnView_Default_Section;

      // AutoShrink
      sData := aIniFile.ReadString(sSection, IE_Registry_IEVAutoShrink, '');
      if sData <> '' then
        AutoShrink := IEStr2BoolS(sData);

      // AutoStretch
      sData := aIniFile.ReadString(sSection, IE_Registry_IEVAutoStretch, '');
      if sData <> '' then
        AutoStretch := IEStr2BoolS(sData);

      // Center
      sData := aIniFile.ReadString(sSection, IE_Registry_IEVCenter, '');
      if sData <> '' then
        Center := IEStr2BoolS(sData);

      // PlayLoop
      sData := aIniFile.ReadString(sSection, IE_Registry_IEVPlayLoop, '');
      if sData <> '' then
        PlayLoop := IEStr2BoolS(sData);

      // Background
      sData := aIniFile.ReadString(sSection, IE_Registry_IEVBackgroundStyle, '');
      if sData <> '' then
      begin
        BackgroundStyle  := TIEBackgroundStyle(StrToIntDef(sData, 0));
        Background       := IEStr2ColorDefS(aIniFile.ReadString(sSection, IE_Registry_IEVBackground, ''), clBlack);
        GradientEndColor := IEStr2ColorDefS(aIniFile.ReadString(sSection, IE_Registry_IEVGradientEndColor, ''), clGray);
      end;

      // MouseInteract
      for enum in [Low(TIEMouseInteractItems) .. High(TIEMouseInteractItems)] do
      begin
        sData := aIniFile.ReadString(sSection, IE_Registry_IEVMouseInteract_Prefix + IntToStr(Integer(enum)), '');
        if sData <> '' then
        begin
          if IEStr2BoolS(sData) then
            MouseInteract := MouseInteract + [enum]
          else
            MouseInteract := MouseInteract - [enum];
        end;
      end;

      // ZoomFilter
      sData := aIniFile.ReadString(sSection, IE_Registry_IEVZoomFilter, '');
      if sData <> '' then
        ZoomFilter := TResampleFilter(StrToIntDef(sData, 0));

    finally
      aIniFile.free;
    end;

    // PROC
    if Proc.LoadFromIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('Proc Load Error');

    // IO
    if IO.LoadFromIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('IO Load Error');
  except
    Result := False;
  end;
end;

function TImageEnViewHelper.SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;   
  enum : TIEMouseInteractItems;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnView_Default_Section;

      // AutoShrink
      aIniFile.WriteString(sSection, IE_Registry_IEVAutoShrink, IEBool2StrS(AutoShrink));

      // AutoStretch
      aIniFile.WriteString(sSection, IE_Registry_IEVAutoStretch, IEBool2StrS(AutoStretch));

      // Center
      aIniFile.WriteString(sSection, IE_Registry_IEVCenter, IEBool2StrS(Center));

      // PlayLoop
      aIniFile.WriteString(sSection, IE_Registry_IEVPlayLoop, IEBool2StrS(PlayLoop));

      // Background
      aIniFile.WriteString(sSection, IE_Registry_IEVBackgroundStyle, IntToStr(Integer(BackgroundStyle)));
      aIniFile.WriteString(sSection, IE_Registry_IEVBackground, ColorToString(Background));
      aIniFile.WriteString(sSection, IE_Registry_IEVGradientEndColor, ColorToString(GradientEndColor));

      // MouseInteract
      for enum in [Low(TIEMouseInteractItems) .. High(TIEMouseInteractItems)] do
        aIniFile.WriteString(sSection, IE_Registry_IEVMouseInteract_Prefix + IntToStr(Integer(enum)), IEBool2StrS(enum in MouseInteract));

      // ZoomFilter
      aIniFile.WriteString(sSection, IE_Registry_IEVZoomFilter, IntToStr(Integer(ZoomFilter)));


      aIniFile.UpdateFile;
    finally
      aIniFile.free;
    end;

    // PROC
    if Proc.SaveToIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('Proc Save Error');

    // IO
    if IO.SaveToIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('IO Save Error');
  except
    Result := False;
  end;
end;





// TImageEnMView Helper Functions

function TImageEnMViewHelper.LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
  sData: string;
begin
  Result := True;
  try
    LockUpdate;
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_READ;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, FALSE) = False then
        exit;

      // EnableAdjustOrientation
      sData := WRegistry.ReadString(IE_Registry_IEMEnableAdjustOrientation);
      if sData <> '' then
        EnableAdjustOrientation := IEStr2BoolS(sData);

      // Borders
      HorizBorder := StrToIntDef(WRegistry.ReadString(IE_Registry_IEMHorizBorder), HorizBorder);
      VertBorder  := StrToIntDef(WRegistry.ReadString(IE_Registry_IEMVertBorder ), VertBorder );

      // Thumb Size
      ThumbWidth  := StrToIntDef(WRegistry.ReadString(IE_Registry_IEMThumbWidth ), ThumbWidth );
      ThumbHeight := StrToIntDef(WRegistry.ReadString(IE_Registry_IEMThumbHeight), ThumbHeight);

      // PlayLoop
      sData := WRegistry.ReadString(IE_Registry_IEMPlayLoop);
      if sData <> '' then
        PlayLoop := IEStr2BoolS(sData);

      // Background
      sData := WRegistry.ReadString(IE_Registry_IEMBackgroundStyle);
      if sData <> '' then
      begin
        BackgroundStyle  := TIEBackgroundStyle(StrToIntDef(sData, 0));
        Background       := IEStr2ColorDefS(WRegistry.ReadString(IE_Registry_IEMBackground), clBlack);
        GradientEndColor := IEStr2ColorDefS(WRegistry.ReadString(IE_Registry_IEMGradientEndColor), clGray);
      end;

      // ThumbnailResampleFilter
      sData := WRegistry.ReadString(IE_Registry_IEMResampleFilter);
      if sData <> '' then
        ThumbnailResampleFilter := TResampleFilter(StrToIntDef(sData, 0));

      if Self is TImageEnFolderMView then
        With Self as TImageEnFolderMView do
        begin
          // Folder display
          if WRegistry.ReadString(IE_Registry_IEFFileTypes) <> '' then
          begin
            Folder := WRegistry.ReadString(IE_Registry_IEFFolder);
            FileTypes := TIEFolderFileTypes(StrToIntDef( WRegistry.ReadString(IE_Registry_IEFFileTypes), ord(FileTypes)));
            FileTypesMask := WRegistry.ReadString(IE_Registry_IEFFileTypesMask);
            ExclusionMask := WRegistry.ReadString(IE_Registry_IEFExclusionMask);                       
            ShowHiddenFiles := IEStr2BoolS(WRegistry.ReadString(IE_Registry_IEFFolderOptions_Hidden));
            ShowFolders     := IEStr2BoolS(WRegistry.ReadString(IE_Registry_IEFFolderOptions_Folders));
          end;


          // Sorting
          SortOrder := TIEImageEnMViewSortBy(StrToIntDef(WRegistry.ReadString(IE_Registry_IEFSortOrder), ord(SortOrder)));
          sData := WRegistry.ReadString(IE_Registry_IEFSortAscending);
          if sData <> '' then
            SortAscending := IEStr2BoolS(sData);


          // Text
          DefaultBottomText := TIEImageEnMViewDefaultText(StrToIntDef(WRegistry.ReadString(IE_Registry_IEFDefaultBottomText) , ord(DefaultBottomText)));
          DefaultInfoText   := TIEImageEnMViewDefaultText(StrToIntDef(WRegistry.ReadString(IE_Registry_IEFDefaultInfoText  ) , ord(DefaultInfoText  )));
          DefaultTopText    := TIEImageEnMViewDefaultText(StrToIntDef(WRegistry.ReadString(IE_Registry_IEFDefaultTopText   ) , ord(DefaultTopText   )));

          // Other
          sData := WRegistry.ReadString(IE_Registry_IEFShowThumbnailHint);
          if sData <> '' then
            ShowThumbnailHint := IEStr2BoolS(sData);
        end;

      WRegistry.CloseKey;
    finally
      WRegistry.free;
      UnlockUpdate;
    end;

    // PROC
    if Proc.LoadFromRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('Proc Load Error');

    // IO
    if MIO.LoadFromRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('IO Load Error');
  except
    Result := False;
  end;
end;




function TImageEnMViewHelper.SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;   
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_ALL_ACCESS;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, True) = False then
        raise ERegistryException.create('Open Error');

      // EnableAdjustOrientation
      WRegistry.WriteString(IE_Registry_IEMEnableAdjustOrientation, IEBool2StrS(EnableAdjustOrientation));

      // Borders
      WRegistry.WriteString(IE_Registry_IEMHorizBorder, IntToStr(HorizBorder));
      WRegistry.WriteString(IE_Registry_IEMVertBorder , IntToStr(VertBorder ));

      // Thumb Size
      WRegistry.WriteString(IE_Registry_IEMThumbWidth , IntToStr(ThumbWidth ));
      WRegistry.WriteString(IE_Registry_IEMThumbHeight, IntToStr(ThumbHeight));

      // PlayLoop
      WRegistry.WriteString(IE_Registry_IEMPlayLoop, IEBool2StrS(PlayLoop));

      // Background
      WRegistry.WriteString(IE_Registry_IEMBackgroundStyle, IntToStr(Integer(BackgroundStyle)));
      WRegistry.WriteString(IE_Registry_IEMBackground, ColorToString(Background));
      WRegistry.WriteString(IE_Registry_IEMGradientEndColor, ColorToString(GradientEndColor));

      // ThumbnailResampleFilter
      WRegistry.WriteString(IE_Registry_IEMResampleFilter, IntToStr(Integer(ThumbnailResampleFilter)));

      if Self is TImageEnFolderMView then
        With Self as TImageEnFolderMView do
        begin
          // Folder display
          WRegistry.WriteString(IE_Registry_IEFFolder, Folder);
          WRegistry.WriteString(IE_Registry_IEFFileTypes, IntToStr(ord(FileTypes)));
          WRegistry.WriteString(IE_Registry_IEFFileTypesMask, FileTypesMask);
          WRegistry.WriteString(IE_Registry_IEFExclusionMask, ExclusionMask);
          WRegistry.WriteString(IE_Registry_IEFFolderOptions_Hidden, IEBool2StrS(ShowHiddenFiles));
          WRegistry.WriteString(IE_Registry_IEFFolderOptions_Folders, IEBool2StrS(ShowFolders));

          // Sorting
          WRegistry.WriteString(IE_Registry_IEFSortAscending, IEBool2StrS(SortAscending));
          WRegistry.WriteString(IE_Registry_IEFSortOrder, IntToStr(ord(SortOrder)));

          // Text
          WRegistry.WriteString(IE_Registry_IEFDefaultBottomText, IntToStr(ord(DefaultBottomText)));
          WRegistry.WriteString(IE_Registry_IEFDefaultInfoText  , IntToStr(ord(DefaultInfoText)));
          WRegistry.WriteString(IE_Registry_IEFDefaultTopText   , IntToStr(ord(DefaultTopText)));

          // Other
          WRegistry.WriteString(IE_Registry_IEFShowThumbnailHint, IEBool2StrS(ShowThumbnailHint));
        end;

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;

    // PROC
    if Proc.SaveToRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('Proc Save Error');

    // IO
    if MIO.SaveToRegistry(sKey, aHKEY) = False then
      raise ERegistryException.create('IO Save Error');
  except
    Result := False;
  end;
end;

function TImageEnMViewHelper.LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
  sData: string;
begin
  Result := True;
  try
    LockUpdate;
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnMView_Default_Section;

      // EnableAdjustOrientation
      sData := aIniFile.ReadString(sSection, IE_Registry_IEMEnableAdjustOrientation, '');
      if sData <> '' then
        EnableAdjustOrientation := IEStr2BoolS(sData);

      // Borders
      HorizBorder := StrToIntDef(aIniFile.ReadString(sSection, IE_Registry_IEMHorizBorder, ''), HorizBorder);
      VertBorder  := StrToIntDef(aIniFile.ReadString(sSection, IE_Registry_IEMVertBorder, '' ), VertBorder );

      // Thumb Size
      ThumbWidth  := StrToIntDef(aIniFile.ReadString(sSection, IE_Registry_IEMThumbWidth, '' ), ThumbWidth );
      ThumbHeight := StrToIntDef(aIniFile.ReadString(sSection, IE_Registry_IEMThumbHeight, ''), ThumbHeight);

      // PlayLoop
      sData := aIniFile.ReadString(sSection, IE_Registry_IEMPlayLoop, '');
      if sData <> '' then
        PlayLoop := IEStr2BoolS(sData);

      // Background
      sData := aIniFile.ReadString(sSection, IE_Registry_IEMBackgroundStyle, '');
      if sData <> '' then
      begin
        BackgroundStyle  := TIEBackgroundStyle(StrToIntDef(sData, 0));
        Background       := IEStr2ColorDefS(aIniFile.ReadString(sSection, IE_Registry_IEMBackground, ''), clBlack);
        GradientEndColor := IEStr2ColorDefS(aIniFile.ReadString(sSection, IE_Registry_IEMGradientEndColor, ''), clGray);
      end;

      // ThumbnailResampleFilter
      sData := aIniFile.ReadString(sSection, IE_Registry_IEMResampleFilter, '');
      if sData <> '' then
        ThumbnailResampleFilter := TResampleFilter(StrToIntDef(sData, 0));

      if Self is TImageEnFolderMView then
        With Self as TImageEnFolderMView do
        begin
          // Folder display
          if aIniFile.ReadString(sSection, IE_Registry_IEFFileTypes, '') <> '' then
          begin
            Folder := aIniFile.ReadString(sSection, IE_Registry_IEFFolder, '');
            FileTypes := TIEFolderFileTypes(StrToIntDef( aIniFile.ReadString(sSection, IE_Registry_IEFFileTypes, ''), ord(FileTypes)));
            FileTypesMask := aIniFile.ReadString(sSection, IE_Registry_IEFFileTypesMask, '');
            ExclusionMask := aIniFile.ReadString(sSection, IE_Registry_IEFExclusionMask, '');                       
            ShowHiddenFiles := IEStr2BoolS(aIniFile.ReadString(sSection, IE_Registry_IEFFolderOptions_Hidden, ''));
            ShowFolders     := IEStr2BoolS(aIniFile.ReadString(sSection, IE_Registry_IEFFolderOptions_Folders, ''));
          end;


          // Sorting
          SortOrder := TIEImageEnMViewSortBy(StrToIntDef(aIniFile.ReadString(sSection, IE_Registry_IEFSortOrder, ''), ord(SortOrder)));
          sData := aIniFile.ReadString(sSection, IE_Registry_IEFSortAscending, '');
          if sData <> '' then
            SortAscending := IEStr2BoolS(sData);


          // Text
          DefaultBottomText := TIEImageEnMViewDefaultText(StrToIntDef(aIniFile.ReadString(sSection, IE_Registry_IEFDefaultBottomText, '') , ord(DefaultBottomText)));
          DefaultInfoText   := TIEImageEnMViewDefaultText(StrToIntDef(aIniFile.ReadString(sSection, IE_Registry_IEFDefaultInfoText  , '') , ord(DefaultInfoText  )));
          DefaultTopText    := TIEImageEnMViewDefaultText(StrToIntDef(aIniFile.ReadString(sSection, IE_Registry_IEFDefaultTopText   , '') , ord(DefaultTopText   )));

          // Other
          sData := aIniFile.ReadString(sSection, IE_Registry_IEFShowThumbnailHint, '');
          if sData <> '' then
            ShowThumbnailHint := IEStr2BoolS(sData);
        end;

    finally
      aIniFile.free;
      UnlockUpdate;
    end;

    // PROC
    if Proc.LoadFromIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('Proc Load Error');

    // IO
    if MIO.LoadFromIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('IO Load Error');
  except
    Result := False;
  end;
end;




function TImageEnMViewHelper.SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;   
begin                           
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := ImageEnMView_Default_Section;

      // EnableAdjustOrientation
      aIniFile.WriteString(sSection, IE_Registry_IEMEnableAdjustOrientation, IEBool2StrS(EnableAdjustOrientation));

      // Borders
      aIniFile.WriteString(sSection, IE_Registry_IEMHorizBorder, IntToStr(HorizBorder));
      aIniFile.WriteString(sSection, IE_Registry_IEMVertBorder , IntToStr(VertBorder ));

      // Thumb Size
      aIniFile.WriteString(sSection, IE_Registry_IEMThumbWidth , IntToStr(ThumbWidth ));
      aIniFile.WriteString(sSection, IE_Registry_IEMThumbHeight, IntToStr(ThumbHeight));

      // PlayLoop
      aIniFile.WriteString(sSection, IE_Registry_IEMPlayLoop, IEBool2StrS(PlayLoop));

      // Background
      aIniFile.WriteString(sSection, IE_Registry_IEMBackgroundStyle, IntToStr(Integer(BackgroundStyle)));
      aIniFile.WriteString(sSection, IE_Registry_IEMBackground, ColorToString(Background));
      aIniFile.WriteString(sSection, IE_Registry_IEMGradientEndColor, ColorToString(GradientEndColor));

      // ThumbnailResampleFilter
      aIniFile.WriteString(sSection, IE_Registry_IEMResampleFilter, IntToStr(Integer(ThumbnailResampleFilter)));

      if Self is TImageEnFolderMView then
        With Self as TImageEnFolderMView do
        begin
          // Folder display
          aIniFile.WriteString(sSection, IE_Registry_IEFFolder, Folder);
          aIniFile.WriteString(sSection, IE_Registry_IEFFileTypes, IntToStr(ord(FileTypes)));
          aIniFile.WriteString(sSection, IE_Registry_IEFFileTypesMask, FileTypesMask);
          aIniFile.WriteString(sSection, IE_Registry_IEFExclusionMask, ExclusionMask);
          aIniFile.WriteString(sSection, IE_Registry_IEFFolderOptions_Hidden, IEBool2StrS(ShowHiddenFiles));
          aIniFile.WriteString(sSection, IE_Registry_IEFFolderOptions_Folders, IEBool2StrS(ShowFolders));

          // Sorting
          aIniFile.WriteString(sSection, IE_Registry_IEFSortAscending, IEBool2StrS(SortAscending));
          aIniFile.WriteString(sSection, IE_Registry_IEFSortOrder, IntToStr(ord(SortOrder)));

          // Text
          aIniFile.WriteString(sSection, IE_Registry_IEFDefaultBottomText, IntToStr(ord(DefaultBottomText)));
          aIniFile.WriteString(sSection, IE_Registry_IEFDefaultInfoText  , IntToStr(ord(DefaultInfoText)));
          aIniFile.WriteString(sSection, IE_Registry_IEFDefaultTopText   , IntToStr(ord(DefaultTopText)));

          // Other
          aIniFile.WriteString(sSection, IE_Registry_IEFShowThumbnailHint, IEBool2StrS(ShowThumbnailHint));
        end;

      aIniFile.UpdateFile;
    finally
      aIniFile.free;
    end;

    // PROC
    if Proc.SaveToIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('Proc Save Error');

    // IO
    if MIO.SaveToIniFile(sFilename, sSection) = False then
      raise ERegistryException.create('IO Save Error');
  except
    Result := False;
  end;
end;




{$IFDEF IEINCLUDEIEXACQUIRE}
function TIEAcquireParamsHelper.LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
  sData: string;
  ADevice : TIEAcquireSource;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_READ;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, FALSE) = False then
        exit;

      // SelectedSource
      sData := WRegistry.ReadString(IE_Registry_SelectedSource);
      if sData <> '' then
      begin
        ADevice := StrToAcquireSource(sData);
        SetSource(ADevice.Api, ADevice.Location);
      end;

      // VisibleDialog
      sData := WRegistry.ReadString(IE_Registry_VisibleDialog);
      if sData <> '' then
        VisibleDialog := IEStr2BoolS(sData);

      // AcquireFrameEnabled
      sData := WRegistry.ReadString(IE_Registry_AcquireFrameEnabled);
      if sData <> '' then
        AcquireFrameEnabled := IEStr2BoolS(sData);

      // AcquireFrameBottom
      sData := WRegistry.ReadString(IE_Registry_AcquireFrameBottom);
      if sData <> '' then
        AcquireFrameBottom := StrToFloatDef(sData, 2);

      // AcquireFrameLeft
      sData := WRegistry.ReadString(IE_Registry_AcquireFrameLeft);
      if sData <> '' then
        AcquireFrameLeft := StrToFloatDef(sData, 2);

      // AcquireFrameRight
      sData := WRegistry.ReadString(IE_Registry_AcquireFrameRight);
      if sData <> '' then
        AcquireFrameRight := StrToFloatDef(sData, 2);

      // AcquireFrameTop
      sData := WRegistry.ReadString(IE_Registry_AcquireFrameTop);
      if sData <> '' then
        AcquireFrameTop := StrToFloatDef(sData, 2);

      // AutoFeed
      sData := WRegistry.ReadString(IE_Registry_AutoFeed);
      if sData <> '' then
        AutoFeed := IEStr2BoolS(sData);

      // BitDepth
      sData := WRegistry.ReadString(IE_Registry_BitDepth);
      if sData <> '' then
        BitDepth := StrToIntDef(sData, 2);

      // Brightness
      sData := WRegistry.ReadString(IE_Registry_Brightness);
      if sData <> '' then
        Brightness := StrToFloatDef(sData, 1000);

      // Contrast
      sData := WRegistry.ReadString(IE_Registry_Contrast);
      if sData <> '' then
        Contrast := StrToFloatDef(sData, 1000);

      // DuplexEnabled
      sData := WRegistry.ReadString(IE_Registry_DuplexEnabled);
      if sData <> '' then
        DuplexEnabled := IEStr2BoolS(sData);

      // FeederEnabled
      sData := WRegistry.ReadString(IE_Registry_FeederEnabled);
      if sData <> '' then
        FeederEnabled := IEStr2BoolS(sData);

      // Orientation
      sData := WRegistry.ReadString(IE_Registry_Orientation);
      if sData <> '' then
        Orientation := TIEAcquireOrientation(StrToIntDef(sData, 0));

      // PixelType
      sData := WRegistry.ReadString(IE_Registry_PixelType);
      if sData <> '' then
        PixelType := TIEAcquirePixelType(StrToIntDef(sData, 0));

      // Rotation
      sData := WRegistry.ReadString(IE_Registry_Rotation);
      if sData <> '' then
        Rotation := TIEAcquireRotation(StrToIntDef(sData, 0));

      // Threshold
      sData := WRegistry.ReadString(IE_Registry_Threshold);
      if sData <> '' then
        Threshold := StrToFloatDef(sData, 125);

      // XResolution
      sData := WRegistry.ReadString(IE_Registry_XResolution);
      if sData <> '' then
        XResolution := StrToFloatDef(sData, 100);

      // YResolution
      sData := WRegistry.ReadString(IE_Registry_YResolution);
      if sData <> '' then
        YResolution := StrToFloatDef(sData, 100);

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;

  except
    Result := False;
  end;
end;

function TIEAcquireParamsHelper.SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
var
  WRegistry : TRegistry;
begin
  Result := True;
  try
    WRegistry := TRegistry.Create;
    try
      WRegistry.Access := KEY_ALL_ACCESS;
      WRegistry.Rootkey := aHKEY;
      if WRegistry.Openkey(sKey, True) = False then
        raise ERegistryException.create('Open Error');

      // SelectedSource
      WRegistry.WriteString(IE_Registry_SelectedSource, AcquireSourceToStr(SelectedSource));

      // VisibleDialog
      WRegistry.WriteString(IE_Registry_VisibleDialog, IEBool2StrS(VisibleDialog));

      // AcquireFrameEnabled
      WRegistry.WriteString(IE_Registry_AcquireFrameEnabled, IEBool2StrS(AcquireFrameEnabled));

      // AcquireFrameBottom
      WRegistry.WriteString(IE_Registry_AcquireFrameBottom, FloatToStr(AcquireFrameBottom));

      // AcquireFrameLeft
      WRegistry.WriteString(IE_Registry_AcquireFrameLeft, FloatToStr(AcquireFrameLeft));

      // AcquireFrameRight
      WRegistry.WriteString(IE_Registry_AcquireFrameRight, FloatToStr(AcquireFrameRight));

      // AcquireFrameTop
      WRegistry.WriteString(IE_Registry_AcquireFrameTop, FloatToStr(AcquireFrameTop));

      // AutoFeed
      WRegistry.WriteString(IE_Registry_AutoFeed, IEBool2StrS(AutoFeed));

      // BitDepth
      WRegistry.WriteString(IE_Registry_BitDepth, IntToStr(BitDepth));

      // Brightness
      WRegistry.WriteString(IE_Registry_Brightness, FloatToStr(Brightness));

      // Contrast
      WRegistry.WriteString(IE_Registry_Contrast, FloatToStr(Contrast));

      // DuplexEnabled
      WRegistry.WriteString(IE_Registry_DuplexEnabled, IEBool2StrS(DuplexEnabled));

      // FeederEnabled
      WRegistry.WriteString(IE_Registry_FeederEnabled, IEBool2StrS(FeederEnabled));

      // Orientation
      WRegistry.WriteString(IE_Registry_Orientation, IntToStr(ord(Orientation)));

      // PixelType
      WRegistry.WriteString(IE_Registry_PixelType, IntToStr(ord(PixelType)));

      // Rotation
      WRegistry.WriteString(IE_Registry_Rotation, IntToStr(ord(Rotation)));

      // Threshold
      WRegistry.WriteString(IE_Registry_Threshold, FloatToStr(Threshold));

      // XResolution
      WRegistry.WriteString(IE_Registry_XResolution, FloatToStr(XResolution));

      // YResolution
      WRegistry.WriteString(IE_Registry_YResolution, FloatToStr(YResolution));

      WRegistry.CloseKey;
    finally
      WRegistry.free;
    end;

  except
    Result := False;
  end;
end;


function TIEAcquireParamsHelper.LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
  sData: string;      
  ADevice : TIEAcquireSource;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := AcquireParams_Default_Section;

      // SelectedSource
      sData := aIniFile.ReadString(sSection, IE_Registry_SelectedSource, '');
      if sData <> '' then
      begin
        ADevice := StrToAcquireSource(sData);
        SetSource(ADevice.Api, ADevice.Location);
      end;

      // VisibleDialog
      sData := aIniFile.ReadString(sSection, IE_Registry_VisibleDialog, '');
      if sData <> '' then
        VisibleDialog := IEStr2BoolS(sData);

      // AcquireFrameEnabled
      sData := aIniFile.ReadString(sSection, IE_Registry_AcquireFrameEnabled, '');
      if sData <> '' then
        AcquireFrameEnabled := IEStr2BoolS(sData);

      // AcquireFrameBottom
      sData := aIniFile.ReadString(sSection, IE_Registry_AcquireFrameBottom, '');
      if sData <> '' then
        AcquireFrameBottom := StrToFloatDef(sData, 2);

      // AcquireFrameLeft
      sData := aIniFile.ReadString(sSection, IE_Registry_AcquireFrameLeft, '');
      if sData <> '' then
        AcquireFrameLeft := StrToFloatDef(sData, 2);

      // AcquireFrameRight
      sData := aIniFile.ReadString(sSection, IE_Registry_AcquireFrameRight, '');
      if sData <> '' then
        AcquireFrameRight := StrToFloatDef(sData, 2);

      // AcquireFrameTop
      sData := aIniFile.ReadString(sSection, IE_Registry_AcquireFrameTop, '');
      if sData <> '' then
        AcquireFrameTop := StrToFloatDef(sData, 2);

      // AutoFeed
      sData := aIniFile.ReadString(sSection, IE_Registry_AutoFeed, '');
      if sData <> '' then
        AutoFeed := IEStr2BoolS(sData);

      // BitDepth
      sData := aIniFile.ReadString(sSection, IE_Registry_BitDepth, '');
      if sData <> '' then
        BitDepth := StrToIntDef(sData, 2);

      // Brightness
      sData := aIniFile.ReadString(sSection, IE_Registry_Brightness, '');
      if sData <> '' then
        Brightness := StrToFloatDef(sData, 1000);

      // Contrast
      sData := aIniFile.ReadString(sSection, IE_Registry_Contrast, '');
      if sData <> '' then
        Contrast := StrToFloatDef(sData, 1000);

      // DuplexEnabled
      sData := aIniFile.ReadString(sSection, IE_Registry_DuplexEnabled, '');
      if sData <> '' then
        DuplexEnabled := IEStr2BoolS(sData);

      // FeederEnabled
      sData := aIniFile.ReadString(sSection, IE_Registry_FeederEnabled, '');
      if sData <> '' then
        FeederEnabled := IEStr2BoolS(sData);

      // Orientation
      sData := aIniFile.ReadString(sSection, IE_Registry_Orientation, '');
      if sData <> '' then
        Orientation := TIEAcquireOrientation(StrToIntDef(sData, 0));

      // PixelType
      sData := aIniFile.ReadString(sSection, IE_Registry_PixelType, '');
      if sData <> '' then
        PixelType := TIEAcquirePixelType(StrToIntDef(sData, 0));

      // Rotation
      sData := aIniFile.ReadString(sSection, IE_Registry_Rotation, '');
      if sData <> '' then
        Rotation := TIEAcquireRotation(StrToIntDef(sData, 0));

      // Threshold
      sData := aIniFile.ReadString(sSection, IE_Registry_Threshold, '');
      if sData <> '' then
        Threshold := StrToFloatDef(sData, 125);

      // XResolution
      sData := aIniFile.ReadString(sSection, IE_Registry_XResolution, '');
      if sData <> '' then
        XResolution := StrToFloatDef(sData, 100);

      // YResolution
      sData := aIniFile.ReadString(sSection, IE_Registry_YResolution, '');
      if sData <> '' then
        YResolution := StrToFloatDef(sData, 100);
    finally
      aIniFile.free;
    end;
    
  except
    Result := False;
  end;
end;

function TIEAcquireParamsHelper.SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;
var
  aIniFile : TMemIniFile;
begin
  Result := True;
  try
    aIniFile := TMemIniFile.Create(sFilename);
    try
      if sSection = '' then
        sSection := AcquireParams_Default_Section;

      // SelectedSource
      aIniFile.WriteString(sSection, IE_Registry_SelectedSource, AcquireSourceToStr(SelectedSource));

      // VisibleDialog
      aIniFile.WriteString(sSection, IE_Registry_VisibleDialog, IEBool2StrS(VisibleDialog));

      // AcquireFrameEnabled
      aIniFile.WriteString(sSection, IE_Registry_AcquireFrameEnabled, IEBool2StrS(AcquireFrameEnabled));

      // AcquireFrameBottom
      aIniFile.WriteString(sSection, IE_Registry_AcquireFrameBottom, FloatToStr(AcquireFrameBottom));

      // AcquireFrameLeft
      aIniFile.WriteString(sSection, IE_Registry_AcquireFrameLeft, FloatToStr(AcquireFrameLeft));

      // AcquireFrameRight
      aIniFile.WriteString(sSection, IE_Registry_AcquireFrameRight, FloatToStr(AcquireFrameRight));

      // AcquireFrameTop
      aIniFile.WriteString(sSection, IE_Registry_AcquireFrameTop, FloatToStr(AcquireFrameTop));

      // AutoFeed
      aIniFile.WriteString(sSection, IE_Registry_AutoFeed, IEBool2StrS(AutoFeed));

      // BitDepth
      aIniFile.WriteString(sSection, IE_Registry_BitDepth, IntToStr(BitDepth));

      // Brightness
      aIniFile.WriteString(sSection, IE_Registry_Brightness, FloatToStr(Brightness));

      // Contrast
      aIniFile.WriteString(sSection, IE_Registry_Contrast, FloatToStr(Contrast));

      // DuplexEnabled
      aIniFile.WriteString(sSection, IE_Registry_DuplexEnabled, IEBool2StrS(DuplexEnabled));

      // FeederEnabled
      aIniFile.WriteString(sSection, IE_Registry_FeederEnabled, IEBool2StrS(FeederEnabled));

      // Orientation
      aIniFile.WriteString(sSection, IE_Registry_Orientation, IntToStr(ord(Orientation)));

      // PixelType
      aIniFile.WriteString(sSection, IE_Registry_PixelType, IntToStr(ord(PixelType)));

      // Rotation
      aIniFile.WriteString(sSection, IE_Registry_Rotation, IntToStr(ord(Rotation)));

      // Threshold
      aIniFile.WriteString(sSection, IE_Registry_Threshold, FloatToStr(Threshold));

      // XResolution
      aIniFile.WriteString(sSection, IE_Registry_XResolution, FloatToStr(XResolution));

      // YResolution
      aIniFile.WriteString(sSection, IE_Registry_YResolution, FloatToStr(YResolution));

      aIniFile.UpdateFile;
    finally
      aIniFile.free;
    end;

  except
    Result := False;
  end;
end;
{$ENDIF}


{$ENDIF}

{!!
<FS>iexRegistryFunctions

<FN>iexRegistryFunctions.pas adds the ability to save key properties of <A TImageEnIO>, <A TImageEnMIO>, <A TIPDialogParams>, <A TIOPrintPreviewParams>, <A TImageEnProc>, <A TImageEnView> and <A TImageEnMView> to the registry or an ini file.

<FM>Declaration<FC>
  function LoadFromRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
  function SaveToRegistry(const sKey : string; aHKEY: HKEY = HKEY_CURRENT_USER): boolean;
  function LoadFromIniFile(const sFilename : string; sSection : string = ''): boolean;
  function SaveToIniFile(const sFilename : string; sSection : string = ''): boolean;

<FM>Description<FN>
Saves common user settings for the component to the registry path specified by sKey, or the ini file specified by sFilename.

Note: If you call ImageEnMView1.SaveToRegistry you do <FB>not<FN> need to call ImageEnMView1.Proc.SaveToRegistry, ImageEnMView1.IO.SaveToRegistry, etc, as this will be done automatically.

<TABLE>
<R> <H>Component</H> <H>Properties Saved</H> </R>
<R> <C><A TImageEnView></C> <C><A TImageEnView.AutoShrink>, <A TImageEnView.AutoStretch>, <A TImageEnView.Center>, <A TImageEnView.MouseInteract>, <A TImageEnView.PlayLoop>, <A TImageEnView.BackgroundStyle>, <A TImageEnView.Background>, <A TImageEnView.GradientEndColor>, <A TImageEnView.ZoomFilter>, <L TImageEnProc>TImageEnProc properties (See below)</L> and <L TImageEnIO>TImageEnIO properties (See below)</L></C> </R>
<R> <C><A TImageEnMView></C> <C><A TImageEnMView.EnableAdjustOrientation>, <A TImageEnMView.HorizBorder>, <A TImageEnMView.VertBorder>, <A TImageEnMView.ThumbWidth>, <A TImageEnMView.ThumbHeight>, <A TImageEnMView.PlayLoop>, <A TImageEnMView.BackgroundStyle>, <A TImageEnMView.Background>, <A TImageEnMView.GradientEndColor>, <A TImageEnMView.ThumbnailResampleFilter>, <L TImageEnProc>TImageEnProc properties (See below)</L> and <L TImageEnMIO>TImageEnMIO properties (See below)</L></C> </R>
<R> <C><A TImageEnFolderMView></C> <C><A TImageEnFolderMView.ExclusionMask>, <A TImageEnFolderMView.FileTypes>, <A TImageEnFolderMView.FileTypesMask>, <A TImageEnFolderMView.Folder>, <A TImageEnFolderMView.ShowFolders>, <A TImageEnFolderMView.ShowHiddenFiles>, <A TImageEnFolderMView.SortAscending>, <A TImageEnFolderMView.SortOrder>, <A TImageEnFolderMView.ShowThumbnailHint>, <A TImageEnFolderMView.DefaultBottomText>, <A TImageEnFolderMView.DefaultInfoText>, <A TImageEnFolderMView.DefaultTopText>, <L TImageEnMView>TImageEnMView properties (See above)</L> , <L TImageEnProc>TImageEnProc properties (See below)</L> and <L TImageEnMIO>TImageEnMIO properties (See below)</L></C> </R>
<R> <C><A TImageEnIO></C> <C><A TImageEnIO.AutoAdjustDPI>, <A TIOParamsVals.JPEG_Quality>, <A TIOParamsVals.EnableAdjustOrientation>, <A TImageEnIO.SelectedAcquireSource>, <A TImageEnIO.DialogsMeasureUnit>, <A TIEImageEnGlobalSettings.MsgLanguage> and <L TImageEnIO.PrintPreviewParams>all print preview dialog properties</L></C> </R>
<R> <C><A TImageEnMIO></C> <C><A TImageEnMIO.AutoAdjustDPI>, <A TImageEnMIO.SelectedAcquireSource>, <A TImageEnMIO.DialogsMeasureUnit>, <A TIEImageEnGlobalSettings.MsgLanguage> and <L TImageEnMIO.PrintPreviewParams>all print preview dialog properties</L></C> </R>
<R> <C><A TImageEnProc></C> <C><A TIEImageEnGlobalSettings.MsgLanguage> and <L TImageEnProc.IPDialogParams>all image processing dialog properties</L></C> </R>
<R> <C><A TIEAcquireParams></C> <C>All</L></C> </R>
</TABLE>

<FM>Example<FC>
uses
  iexRegistryFunctions;
...

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Load settings of our ImageEnView from the registry
  ImageEnView1.LoadFromRegistry('Software\MyCompany\MySoftware');
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Save settings of our ImageEnView to the registry
  ImageEnView1.SaveToRegistry('Software\MyCompany\MySoftware');
end;

!!}

end.