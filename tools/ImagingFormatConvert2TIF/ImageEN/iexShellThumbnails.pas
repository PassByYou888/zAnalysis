{------------------------------------------------------------------------------}
{                                                                              }
{  Retrieve thumbnails from Windows (as displayed in Explorer)                 }
{                                                                              }
{  Nigel Cross                                                                 }
{  Xequte Software                                                             }
{  nigel@xequte.com                                                            }
{  http://www.xequte.com                                                       }
{                                                                              }
{  © Xequte Software 2009-2014                                                 }
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
File version 1002
*)

// VIDEO_THUMBNAILS
// Define VIDEO_THUMBNAILS in ie.inc to allow ImageEn to display thumbnails for videos in TImageEnMView

unit iexShellThumbnails;

interface
  
uses
  Windows, SysUtils, Graphics, hyieutils;
                              
{$I ie.inc}


{$IFDEF VIDEO_THUMBNAILS}
             

type
  {$EXTERNALSYM SIIGBF}
  SIIGBF = Integer;

  {$EXTERNALSYM IShellItemImageFactory}
  IShellItemImageFactory = interface(IUnknown)
    ['{BCC18B79-BA16-442F-80C4-8A59C30C463B}']
    function GetImage(size: TSize; flags: SIIGBF; out phbm: HBITMAP): HRESULT; stdcall;
  end;

const
  SIIGBF_RESIZETOFIT = $00000000;
  SIIGBF_BIGGERSIZEOK = $00000001;
  SIIGBF_MEMORYONLY = $00000002;
  SIIGBF_ICONONLY = $00000004;
  SIIGBF_THUMBNAILONLY = $00000008;
  SIIGBF_INCACHEONLY = $00000010;

  IID_IExtractImage2: TGUID = '{953BB1EE-93B4-11D1-98A3-00C04FB687DA}';

  Min_Thumbnail_Size = 52;
  Max_Thumbnail_Size = 256;

  IEIFLAG_ASYNC = $001; // ask the extractor if it supports ASYNC extract
  // (free threaded)
  IEIFLAG_CACHE = $002; // returned from the extractor if it does NOT cache
  // the thumbnail
  IEIFLAG_ASPECT = $004; // passed to the extractor to beg it to render to
  // the aspect ratio of the supplied rect
  IEIFLAG_OFFLINE = $008; // if the extractor shouldn't hit the net to get
  // any content needs for the rendering
  IEIFLAG_GLEAM = $010; // does the image have a gleam? this will be
  // returned if it does
  IEIFLAG_SCREEN = $020; // render as if for the screen  (this is exlusive
  // with IEIFLAG_ASPECT )
  IEIFLAG_ORIGSIZE = $040; // render to the approx size passed, but crop if
  // neccessary
  IEIFLAG_NOSTAMP = $080; // returned from the extractor if it does NOT want
  // an icon stamp on the thumbnail
  IEIFLAG_NOBORDER = $100; // returned from the extractor if it does NOT want
  // an a border around the thumbnail
  IEIFLAG_QUALITY = $200; // passed to the Extract method to indicate that
  // a slower, higher quality image is desired,
  // re-compute the thumbnail

type
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE_UUID("953BB1EE-93B4-11D1-98A3-00C04FB687DA", IExtractImage2)'}
  IRunnableTask = interface
    ['{85788D00-6807-11D0-B810-00C04FD706EC}']
    function Run: HResult; stdcall;
    function Kill(fWait: BOOL): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
    function IsRunning: Longint; stdcall;
  end;

  IExtractImage = interface
    ['{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}']
    function GetLocation(pszwPathBuffer: PWideChar; cch: DWord;
      var dwPriority: DWord; var rgSize: TSize; dwRecClrDepth: DWord;
      var dwFlags: DWord): HResult; stdcall;
    function Extract(var hBmpThumb: HBITMAP): HResult; stdcall;
  end;

  IExtractImage2 = interface(IExtractImage)
    ['{953BB1EE-93B4-11D1-98A3-00C04FB687DA}']
    function GetDateStamp(var pDateStamp: TFileTime): HResult; stdcall;
  end;    
{$ENDIF}




{$IFDEF VIDEO_THUMBNAILS}
// Retrieve the Windows thumnail for the specified file into a bitmap
// Result is true if it succeeds
function ExtractExplorerThumbnail(const sFileName : string; Bitmap : TBitmap; iDesiredWidth : Integer = 120; iDesiredHeight : Integer = 120) : Boolean;
{$ENDIF}

// Returns true if the file has an extension listed in IEGlobalSettings().MViewExplorerThumbnailExts
function UseThumbnailFromExplorer(const sFileName : string) : Boolean;

        
{$IFDEF VIDEO_THUMBNAILS}
// Return icons of larger size (48x28, 256x256, etc) using GetImageListSH()
Procedure IEGetLargeFileIcon(const sFilename : String; var aIcon : TIcon; SHIL_FLAG : Cardinal);

// Return 256x256 icon using GetImageListSH(SHIL_JUMBO). Note: File types without a large icon will return a much smaller one
procedure IEGetJumboFileIcon(const sFilename : string; DestBitmap : TIEBitmap);
{$ENDIF}


{$IFDEF VIDEO_THUMBNAILS}
const
  SHIL_LARGE     = $00;  // The image size is normally 32x32 pixels. However, if the Use large icons option is selected from the Effects section of the Appearance tab in Display Properties, the image is 48x48 pixels.
  SHIL_SMALL     = $01;  // These images are the Shell standard small icon size of 16x16, but the size can be customized by the user.
  SHIL_EXTRALARGE= $02;  // These images are the Shell standard extra-large icon size. This is typically 48x48, but the size can be customized by the user.
  SHIL_SYSSMALL  = $03;  // These images are the size specified by GetSystemMetrics called with SM_CXSMICON and GetSystemMetrics called with SM_CYSMICON.
  SHIL_JUMBO     = $04;  // Windows Vista and later. The image is normally 256x256 pixels.
  IID_IImageList: TGUID= '{46EB5926-582E-4017-9FDF-E8998DAA0950}';
{$ENDIF}


implementation   

{$IFDEF VIDEO_THUMBNAILS}
uses
  ShellApi, Commctrl, bmpfilt, 
  ShlObj, ActiveX, ComObj, iesettings;
  
const
  Shell_32_Dll = 'shell32.dll';
var
  Shell32Lib: HModule;
  _SHGetImageList: function (iImageList: integer; const riid: TGUID; var ppv: Pointer): hResult; stdcall; 
  SHJumboImageList : HIMAGELIST;
  {$IFNDEF Delphi2007orNewer}
  _SHCreateItemFromParsingName: function(pszPath: LPCWSTR; const pbc: IBindCtx; const riid: TIID; out ppv): HResult; stdcall;
  {$ENDIF}
{$ENDIF}


{$IFDEF VIDEO_THUMBNAILS}
{$IFNDEF Delphi2007orNewer}      
// Delphi 7 lacks this function
function SHCreateItemFromParsingName(pszPath: LPCWSTR; const pbc: IBindCtx; const riid: TIID; out ppv): HResult;
begin
  if Assigned(_SHCreateItemFromParsingName) then
    Result := _SHCreateItemFromParsingName(pszPath, pbc, riid, ppv)
  else
  begin
    Shell32Lib := GetModuleHandle(Shell_32_Dll);
    Result := E_NOTIMPL;
    if Shell32Lib > 0 then
    begin
      _SHCreateItemFromParsingName := GetProcAddress(Shell32Lib, 'SHCreateItemFromParsingName');
      if Assigned(_SHCreateItemFromParsingName) then
        Result := _SHCreateItemFromParsingName(pszPath, pbc, riid, ppv);
    end;
  end;
end;
{$ENDIF}
{$ENDIF}


// Returns true if the file has an extension listed in IEGlobalSettings().MViewExplorerThumbnailExts
function UseThumbnailFromExplorer(const sFileName : string) : Boolean;
begin        
  {$IFDEF VIDEO_THUMBNAILS}
  Result := False;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
    Result := IEFilenameInExtensions(sFileName, IEGlobalSettings().MViewExplorerThumbnailExts);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;





{$IFDEF VIDEO_THUMBNAILS}
function ExtractExplorerThumbnail(const sFileName : string; Bitmap : TBitmap; iDesiredWidth : Integer = 120; iDesiredHeight : Integer = 120) : Boolean;
var
  ImageFactory: IShellItemImageFactory;
  hRes: HResult;          
  BmpHandle: HBITMAP;  
  ExtractImage: IExtractImage;
  ExtractImage2: IExtractImage2;    
  ASize: TSize;       
  RunnableTask: IRunnableTask;      
  ShellFolder, DesktopShellFolder: IShellFolder;      
  PIDL: PItemIDList;
  Eaten: DWord;                  
  Attribute, Priority: DWord;     
  TempFileName : Widestring;        
  Flags: DWord;    
  Colordepth: Cardinal; 
  lRes: HResult;            
  Buff: array [0 .. MAX_PATH * 4] of WideChar;
begin
  Result := False;
  Try
    OleCheck(SHGetDesktopFolder(DesktopShellFolder));
    OleCheck(DesktopShellFolder.ParseDisplayName(0, nil, StringToOleStr(ExtractFilePath(sFilename)), Eaten, PIDL, Attribute));
    OleCheck(DesktopShellFolder.BindToObject(PIDL, nil, IID_IShellFolder, Pointer(ShellFolder)));
    CoTaskMemFree(PIDL);  

    ASize.cx := iDesiredWidth;
    ASize.cy := iDesiredHeight;

    if IEIsWindowsVistaOrNewer then
    begin
      TempFileName := WideString(sFilename);
      hRes := SHCreateItemFromParsingName(PWideChar(TempFileName), nil, IShellItemImageFactory, ImageFactory);
      if Succeeded(hRes) then
      begin
        ASize.cx := iDesiredWidth;
        ASize.cy := iDesiredHeight;
        hRes := ImageFactory.GetImage(ASize, SIIGBF_THUMBNAILONLY, BmpHandle);
        if Succeeded(hRes) then
        begin
          Bitmap.Handle := BmpHandle;
          Bitmap.PixelFormat := pf24bit;
          Result := true;
        end;
      end;
    end
    else
    // XP
    begin
      OleCheck(ShellFolder.ParseDisplayName(0, nil, StringToOleStr(ExtractFileName(sFilename)), Eaten, PIDL, Attribute));
      ShellFolder.GetUIObjectOf(0, 1, PIDL, IExtractImage, nil, ExtractImage);
      CoTaskMemFree(PIDL);
      if Assigned(ExtractImage) then
      begin
        if ExtractImage.QueryInterface(IID_IExtractImage2, Pointer(ExtractImage2)) = E_NOINTERFACE then
          ExtractImage2 := nil;
        RunnableTask := nil;
        Priority := 0;
        Flags := IEIFLAG_SCREEN or IEIFLAG_OFFLINE or IEIFLAG_ORIGSIZE or IEIFLAG_QUALITY;
        Colordepth := 32;
        lRes := ExtractImage.GetLocation(Buff, MAX_PATH, Priority, ASize, Colordepth, Flags);
        if (lRes = NOERROR) or (lRes = E_PENDING) then
        begin
          if lRes = E_PENDING then
            if ExtractImage.QueryInterface(IRunnableTask, RunnableTask) <> S_OK then
              RunnableTask := nil;

          if Succeeded(ExtractImage.Extract(BmpHandle)) then
          begin
            Bitmap.Handle := BmpHandle;      
            Bitmap.PixelFormat := pf24bit;
            Result := True;
          end;
        end;
      end;
    end;

  except
    // UNEXPECTED ERROR
  end;
end;
{$ENDIF}





{$IFDEF VIDEO_THUMBNAILS}
function GetImageListSH(SHIL_FLAG : Cardinal): HIMAGELIST;
begin
  Result := 0;

  if (Assigned(_SHGetImageList) = False) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    Shell32Lib := GetModuleHandle(Shell_32_Dll);
    if Shell32Lib > 0 then
    _SHGetImageList := GetProcAddress(Shell32Lib, PChar(727));
  end;

  if Assigned(_SHGetImageList) then
    _SHGetImageList(SHIL_FLAG, IID_IImageList, Pointer(Result));
end;
{$ENDIF}


{$IFDEF VIDEO_THUMBNAILS}
Procedure IEGetLargeFileIcon(const sFilename : String; var aIcon : TIcon; SHIL_FLAG : Cardinal);
var
  aImgList    : HIMAGELIST;
  SFI         : TSHFileInfo;
Begin
  // Get the index of the imagelist
  FillChar(SFI, SizeOf(TShFileInfo), 0);
  SHGetFileInfo(PChar(sFilename), FILE_ATTRIBUTE_NORMAL, SFI,
               SizeOf( TSHFileInfo ), SHGFI_ICON or SHGFI_LARGEICON);

  if not Assigned(aIcon) then
    aIcon := TIcon.Create;

  // Get the imagelist
  aImgList := GetImageListSH(SHIL_FLAG);

  // Extract the icon handle
  aIcon.Handle := ImageList_GetIcon(aImgList, SFI.iIcon, ILD_IMAGE);
End;   
{$ENDIF}

             
{$IFDEF VIDEO_THUMBNAILS}
procedure IEGetJumboFileIcon(const sFilename : string; DestBitmap : TIEBitmap);
var
  SFI         : TSHFileInfo;
  icon        : HIcon;
Begin
  // Get the index of the imagelist
  FillChar(SFI, SizeOf(TShFileInfo), 0);
  SHGetFileInfo(PChar(sFilename), FILE_ATTRIBUTE_NORMAL, SFI,
               SizeOf( TSHFileInfo ), SHGFI_ICON or SHGFI_LARGEICON);

  // Get the imagelist
  if SHJumboImageList = 0 then
    SHJumboImageList := GetImageListSH(SHIL_JUMBO);

  // Extract the icon handle
  icon := ImageList_GetIcon(SHJumboImageList, SFI.iIcon, ILD_IMAGE);

  Try
    IEConvertIconToBitmap(icon, DestBitmap, True); 
  except
    IEGetFileIcon(sFilename, DestBitmap);
  end;
End;
{$ENDIF}


end.
