(*
File version 1002
*)

unit ievfw;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows;

const
  streamtypeVIDEO = $73646976; // DWORD( 'v', 'i', 'd', 's' )
  streamtypeAUDIO = $73647561; // DWORD( 'a', 'u', 'd', 's' )
  streamtypeMIDI = $7364696D; // DWORD( 'm', 'i', 'd', 's' )
  streamtypeTEXT = $73747874; // DWORD( 't', 'x', 't', 's' )
  comptypeDIB = $20424944; // DWORD( 'D', 'I', 'B', ' ' )

  AVIIF_KEYFRAME = $10;

type

  PCLSID = PGUID;
  TIID = TGUID;

  IUnknown = class
  public
    function QueryInterface(const iid: TIID; var obj): HResult; virtual; stdcall; abstract;
    function AddRef: Longint; virtual; stdcall; abstract;
    function Release: Longint; virtual; stdcall; abstract;
  end;

  LONG = Longint;
  PVOID = Pointer;

  PAviStreamInfoA = ^TAviStreamInfoA;
  TAviStreamInfoA = record
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
 
  PAviStreamInfoW = ^TAviStreamInfoW;
  TAviStreamInfoW = record
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
    szName: array[0..63] of WideChar;
  end;
  TAviStreamInfo = {$IFDEF UNICODE}TAviStreamInfoW{$ELSE}TAviStreamInfoA{$ENDIF};

const
  AVISTREAMINFO_DISABLED = $00000001;
  AVISTREAMINFO_FORMATCHANGES = $00010000;

type
  PAviFileInfoA = ^TAviFileInfoA;
  TAviFileInfoA = record
    dwMaxBytesPerSec: DWORD; // max. transfer rate
    dwFlags: DWORD; // the ever-present flags
    dwCaps: DWORD;
    dwStreams: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwScale: DWORD;
    dwRate: DWORD; // dwRate / dwScale == samples/second
    dwLength: DWORD;
    dwEditCount: DWORD;
    // descriptive string for file type?
    szFileType: array[0..63] of AnsiChar;
  end;
  TAviFileInfo = TAviFileInfoA;

  PAviFileInfoW = ^TAviFileInfoW;
  TAviFileInfoW = record
    dwMaxBytesPerSec: DWORD; // max. transfer rate
    dwFlags: DWORD; // the ever-present flags
    dwCaps: DWORD;
    dwStreams: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwScale: DWORD;
    dwRate: DWORD; // dwRate / dwScale == samples/second
    dwLength: DWORD;
    dwEditCount: DWORD;
    // descriptive string for file type?
    szFileType: array[0..63] of WideChar;
  end;

const
  // Flags for dwFlags
  AVIFILEINFO_HASINDEX = $00000010;
  AVIFILEINFO_MUSTUSEINDEX = $00000020;
  AVIFILEINFO_ISINTERLEAVED = $00000100;
  AVIFILEINFO_WASCAPTUREFILE = $00010000;
  AVIFILEINFO_COPYRIGHTED = $00020000;

  // Flags for dwCaps
  AVIFILECAPS_CANREAD = $00000001;
  AVIFILECAPS_CANWRITE = $00000002;
  AVIFILECAPS_ALLKEYFRAMES = $00000010;
  AVIFILECAPS_NOCOMPRESSION = $00000020;

  // defines for uiFlags
  ICMF_CHOOSE_KEYFRAME = $0001; // show KeyFrame Every box
  ICMF_CHOOSE_DATARATE = $0002; // show DataRate box
  ICMF_CHOOSE_PREVIEW = $0004; // allow expanded preview dialog
  ICMF_CHOOSE_ALLCOMPRESSORS = $0008; // don't only show those that

type
  TAVISaveCallBack = function(Percentage: Integer): Boolean stdcall;
  AVISaveCallBack = ^TAVISaveCallBack;

type
  PAviCompressOptions = ^TAviCompressOptions;
  TAviCompressOptions = record
    fccType: DWORD; // stream type, for consistency
    fccHandler: DWORD; // compressor
    dwKeyFrameEvery: DWORD; // keyframe rate
    dwQuality: DWORD; // compress quality 0-10,000
    dwBytesPerSecond: DWORD; // bytes per second
    dwFlags: DWORD; // flags... see aviopts.h
    lpFormat: PVOID; // save format
    cbFormat: DWORD;
    lpParms: PVOID; // compressor options
    cbParms: DWORD;
    dwInterleaveEvery: DWORD; // for non-video streams only
  end;

  //
  // Defines for the dwFlags field of the AVICOMPRESSOPTIONS struct
  // Each of these flags determines if the appropriate field in the structure
  // (dwInterleaveEvery, dwBytesPerSecond, and dwKeyFrameEvery) is payed
  // attention to.  See the autodoc in avisave.c for details.
  //
const
  AVICOMPRESSF_INTERLEAVE = $00000001; // interleave
  AVICOMPRESSF_DATARATE = $00000002; // use a data rate
  AVICOMPRESSF_KEYFRAMES = $00000004; // use keyframes
  AVICOMPRESSF_VALID = $00000008; // has valid data?

  // #include "aviiface.h": All necessary stuff from "aviiface.h" follows

type
  // IAVIStream interface
  IAVIStream = class(IUnknown)
    function Create(lParam1, lParam2: LPARAM): HResult; virtual; stdcall; abstract;
    function Info(var psi: TAVIStreamInfoW; lSize: LONG): HResult; virtual; stdcall; abstract;
    function FindSample(lPos, lFlags: LONG): LONG; virtual; stdcall; abstract;
    function ReadFormat(lPos: LONG; lpFormat: PVOID; var lpcbFormat: LONG): HResult; virtual; stdcall; abstract;
    function SetFormat(lPos: LONG; lpFormat: PVOID; lpcbFormat: LONG): HResult; virtual; stdcall; abstract;
    function Read(lStart, lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; var plBytes: LONG; var plSamples: LONG): HResult; virtual; stdcall; abstract;
    function Write(lStart, lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; dwFlags: DWORD; var plSampWritten: LONG; var plBytesWritten: LONG): HResult; virtual; stdcall; abstract;
    function Delete(lStart, lSamples: LONG): HResult; virtual; stdcall; abstract;
    function ReadData(fcc: DWORD; lp: PVOID; var lpcb: LONG): HResult; virtual; stdcall; abstract;
    function WriteData(fcc: DWORD; lp: PVOID; cb: LONG): HResult; virtual; stdcall; abstract;
    function SetInfo(var lpInfo: TAVIStreamInfoW; cbInfo: LONG): HResult; virtual; stdcall; abstract;
  end;
  PAVIStream = ^IAVIStream;

  // IAVIEditStream interface
  IAVIEditStream = class(IUnknown)
    function Info(var pfi: TAVIFileInfoW; lSize: LONG): HResult; virtual; stdcall; abstract;
    function Cut(var plStart: LONG; var plLength: Long; var ppResult: PAVISTREAM): HResult; virtual; stdcall; abstract;
    function Copy(var plLength: Long; var ppResult: PAVISTREAM): HResult; virtual; stdcall; abstract;
    function Paste(var plLength: Long; pstream: PAVISTREAM; lStart: LONG; lEnd: LONG): HResult; virtual; stdcall; abstract;
    function Clone(var ppResult: PAVISTREAM): HResult; virtual; stdcall; abstract;
    function SetInfo(var lpInfo: TAVIStreamInfoW; cbInfo: LONG): HResult; virtual; stdcall; abstract;
  end;
  PAVIEditStream = ^IAVIEditStream;

  // IAVIFile interface
  IAVIFile = class(IUnknown)
    function Open(szFile: LPCSTR; mode: UINT): HResult; virtual; stdcall; abstract;
    function Info(var pfi: TAviFileInfo; lSize: LONG): HResult; virtual; stdcall; abstract;
    function GetStream(var ppStream: PAVISTREAM; fccType: DWORD; lParam: LONG): HResult; virtual; stdcall; abstract;
    function CreateStream(var ppStream: PAVISTREAM; psi: TAVISTREAMINFO): HResult; virtual; stdcall; abstract;
    function Save(szFile: LPCSTR; var lpOptions: TAVICOMPRESSOPTIONS; lpfnCallback: AVISAVECALLBACK): HResult; virtual; stdcall; abstract;
    function WriteData(ckid: DWORD; lpData: PVOID; cbData: LONG): HResult; virtual; stdcall; abstract;
    function ReadData(ckid: DWORD; lpData: PVOID; var lpcbData: LONG): HResult; virtual; stdcall; abstract;
    function EndRecord: HResult; virtual; stdcall; abstract;
  end;
  PAVIFile = ^IAVIFile;


  // IGetFrame interface
type
  IGetFrame = class(IUnknown)
    function GetFrame(lPos: LONG): PVOID; virtual; stdcall; abstract;
    function SetFormat(lpbi: PBITMAPINFOHEADER; lpBits: PVOID; x: Integer; y: Integer; dx: Integer; dy: Integer): HResult; virtual; stdcall; abstract;
  end;
  PGetFrame = ^IGetFrame;

const
  // AVI interface IDs
  IID_IAVIFile: TGUID = (
    D1: $00020020; D2: $0; D3: $0; D4: ($C0, $0, $0, $0, $0, $0, $0, $46));
  IID_IAVIStream: TGUID = (
    D1: $00020021; D2: $0; D3: $0; D4: ($C0, $0, $0, $0, $0, $0, $0, $46));
  IID_IAVIStreaming: TGUID = (
    D1: $00020022; D2: $0; D3: $0; D4: ($C0, $0, $0, $0, $0, $0, $0, $46));
  IID_IGetFrame: TGUID = (
    D1: $00020023; D2: $0; D3: $0; D4: ($C0, $0, $0, $0, $0, $0, $0, $46));
  IID_IAVIEditStream: TGUID = (
    D1: $00020024; D2: $0; D3: $0; D4: ($C0, $0, $0, $0, $0, $0, $0, $46));

  // AVI class IDs
  CLSID_AVISimpleUnMarshal: TGUID = (
    D1: $00020009; D2: $0; D3: $0; D4: ($C0, $0, $0, $0, $0, $0, $0, $46));
  CLSID_AVIFile: TGUID = (
    D1: $00020000; D2: $0; D3: $0; D4: ($C0, $0, $0, $0, $0, $0, $0, $46));

  AVIFILEHANDLER_CANREAD = $0001;
  AVIFILEHANDLER_CANWRITE = $0002;
  AVIFILEHANDLER_CANACCEPTNONRGB = $0004;

  //
  // functions
  //

procedure AVIFileInit; stdcall;
procedure AVIFileExit; stdcall;
function AVIFileAddRef(pfile: PAVIFILE): ULONG; stdcall;
function AVIFileRelease(pfile: PAVIFILE): ULONG; stdcall;

function AVIFileOpen(var ppfile: PAVIFile; szFile: LPCTSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;
function AVIFileOpenW(var ppfile: PAVIFile; szFile: PWideChar; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;
function AVIFileInfo(pfile: PAVIFile; var pfi: TAVIFileInfo; lSize: Long): HResult; stdcall;

function AVIFileGetStream(pfile: PAVIFile; var ppavi: PAVISTREAM; fccType: DWORD; lParam: LONG): HResult; stdcall;
function AVIFileCreateStream(pfile: PAVIFile; var ppavi: PAVISTREAM; var psi: TAVIStreamInfo): HResult; stdcall;
function AVIFileCreateStreamW(pfile: PAVIFile; var ppavi: PAVISTREAM; var psi: TAVIStreamInfoW): HResult; stdcall;

function AVIFileWriteData(pfile: PAVIFile; ckid: DWORD; lpData: PVOID; cbData: LONG): HResult; stdcall;
function AVIFileReadData(ckid: DWORD; lpData: PVOID; var lpcbData: LONG): HResult; stdcall;
function AVIFileEndRecord(pfile: PAVIFile): HResult; stdcall;

function AVIStreamAddRef(pavi: PAVISTREAM): ULONG; stdcall;
function AVIStreamRelease(pavi: PAVISTREAM): ULONG; stdcall;

function AVIStreamInfo(pavi: PAVISTREAM; var psi: TAVISTREAMINFO; lSize: LONG): HResult; stdcall;
function AVIStreamInfoW(pavi: PAVISTREAM; var psi: TAVISTREAMINFOW; lSize: LONG): HResult; stdcall;
function AVIStreamFindSample(pavi: PAVISTREAM; lPos: LONG; lFlags: LONG): LONG; stdcall;
function AVIStreamReadFormat(pavi: PAVISTREAM; lPos: LONG; lpFormat: PVOID; var lpcbFormat: LONG): HResult; stdcall;
function AVIStreamSetFormat(pavi: PAVIStream; lPos: LONG; lpFormat: PVOID; cbFormat: LONG): HResult; stdcall;

function AVIStreamReadData(pavi: PAVIStream; fcc: DWORD; lp: PVOID; var lpcb: LONG): HResult; stdcall;
function AVIStreamWriteData(pavi: PAVIStream; fcc: DWORD; lp: PVOID; cb: LONG): HResult; stdcall;

function AVIStreamRead(pavi: PAVIStream; lStart: LONG; lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; var plBytes: LONG; var plSamples: LONG): HResult; stdcall;

function AVIStreamWrite(pavi: PAVIStream; lStart, lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; dwFlags: DWORD; plSampWritten: pointer; plBytesWritten: pointer): HResult; stdcall;

function AVIStreamStart(pavi: PAVIStream): LONG; stdcall;
function AVIStreamLength(pavi: PAVIStream): LONG; stdcall;
function AVIStreamTimeToSample(pavi: PAVIStream; lTime: LONG): LONG; stdcall;
function AVIStreamSampleToTime(pavi: PAVIStream; lSample: LONG): LONG; stdcall;
function AVIStreamBeginStreaming(pavi: PAVIStream; lStart: LONG; lEnd: LONG; lRate: LONG): LONG; stdcall;
function AVIStreamEndStreaming(pavi: PAVIStream): LONG; stdcall;

//
// helper functions for using IGetFrame
//
function AVIStreamGetFrameOpen(pavi: PAVISTREAM; lpbiWanted: PBITMAPINFOHEADER): PGETFRAME; stdcall;
function AVIStreamGetFrame(pg: PGETFRAME; lPos: LONG): PVOID; stdcall;
function AVIStreamGetFrameClose(pg: PGETFRAME): HResult; stdcall;

// Shortcut function
function AVIStreamOpenFromFile(var ppavi: PAVISTREAM; szFile: LPCTSTR; fccType: DWORD; lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall;
function AVIStreamOpenFromFileW(var ppavi: PAVISTREAM; szFile: PWideChar; fccType: DWORD; lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall;

// Use to create disembodied streams
function AVIStreamCreate(var ppavi: PAVISTREAM; lParam1: LONG; lParam2: LONG; pclsidHandler: PCLSID): HResult; stdcall;

function CreateEditableStream(var ppsEditable: PAVISTREAM; psSource: PAVISTREAM): HResult; stdcall;

function EditStreamSetInfo(pavi: PAVISTREAM; lpinfo: PAVISTREAMINFOA; cbinfo: LONG): HResult; stdcall;

function AVIMakeCompressedStream(var ppsCompressed: PAVISTREAM; psSource: PAVISTREAM; lpOptions: PAVICOMPRESSOPTIONS; pclsid: PCLSID): HResult; stdcall;

const
  //
  // flags for AVIStreamFindSample
  //
  FIND_DIR = $0000000F; // direction
  FIND_NEXT = $00000001; // go forward
  FIND_PREV = $00000004; // go backward

  FIND_TYPE = $000000F0; // type mask
  FIND_KEY = $00000010; // find key frame.
  FIND_ANY = $00000020; // find any (non-empty) sample
  FIND_FORMAT = $00000040; // find format change

  FIND_RET = $0000F000; // return mask
  FIND_POS = $00000000; // return logical position
  FIND_LENGTH = $00001000; // return logical size
  FIND_OFFSET = $00002000; // return physical position
  FIND_SIZE = $00003000; // return physical size
  FIND_INDEX = $00004000; // return physical index position

function AVISaveV(szFile: LPCTSTR; pclsidHandler: PCLSID;
  lpfnCallback: AVISAVECALLBACK; nStreams: INTEGER;
  var ppavi: PAVISTREAM; var plpOptions: PAVICOMPRESSOPTIONS): HResult; stdcall;

function AVISaveOptions(hwnd: HWND; uiFlags: UINT; nStreams: INTEGER;
  pavi: pointer; plpOptions: pointer): Boolean; stdcall;

function AVISaveOptionsFree(nStreams: INTEGER; var plpOptions: PAVICOMPRESSOPTIONS): Boolean; stdcall;

function mmioFOURCC(cc: AnsiString): integer;

const
  AVIERR_OK = 0;
  AVIIF_LIST = $01;
  AVIIF_TWOCC = $02;
  AVIERR_NOCOMPRESSOR: HResult= HResult($80044071);
  AVIERR_MEMORY: HResult = HResult($80044067);
  AVIERR_UNSUPPORTED: HResult = HResult($80044065);


implementation

uses hyiedefs;

{$R-}

procedure AVIFileInit; stdcall; external 'avifil32.dll' name 'AVIFileInit';

procedure AVIFileExit; stdcall; external 'avifil32.dll' name 'AVIFileExit';
function AVIFileAddRef(pfile: PAVIFILE): ULONG;
  external 'avifil32.dll' name 'AVIFileAddRef';
function AVIFileRelease(pfile: PAVIFILE): ULONG;
  external 'avifil32.dll' name 'AVIFileRelease';
function AVIFileOpen(var ppfile: PAVIFILE; szFile: LPCTSTR; uMode: UINT; lpHandler: PCLSID): HResult; external 'avifil32.dll' name 'AVIFileOpen' + IEDLLWNameExt;
function AVIFileOpenW(var ppfile: PAVIFILE; szFile: PWideChar; uMode: UINT; lpHandler: PCLSID): HResult; external 'avifil32.dll' name 'AVIFileOpenW';

function AVIFileInfo(pfile: PAVIFile; var pfi: TAVIFileInfo;
  lSize: Long): HResult; stdcall;
  external 'avifil32.dll' name 'AVIFileInfo';
function AVIFileGetStream(pfile: PAVIFile; var ppavi: PAVIStream;
  fccType: DWORD; lParam: LONG): HResult;
  external 'avifil32.dll' name 'AVIFileGetStream';

function AVIFileCreateStream(pfile: PAVIFile; var ppavi: PAVIStream; var psi: TAviStreamInfo): HResult; external 'avifil32.dll' name 'AVIFileCreateStream' + IEDLLWNameExt;
function AVIFileCreateStreamW(pfile: PAVIFile; var ppavi: PAVIStream; var psi: TAviStreamInfoW): HResult; external 'avifil32.dll' name 'AVIFileCreateStreamW';
function AVIFileWriteData(pfile: PAVIFile; ckid: DWORD; lpData: PVOID;
  cbData: LONG): HResult;
  external 'avifil32.dll' name 'AVIFileWriteData';
function AVIFileReadData(ckid: DWORD; lpData: PVOID;
  var lpcbData: LONG): HResult;
  external 'avifil32.dll' name 'AVIFileReadData';
function AVIFileEndRecord(pfile: PAVIFile): HResult;
  external 'avifil32.dll' name 'AVIFileEndRecord';
function AVIStreamAddRef(pavi: PAVISTREAM): ULONG;
  external 'avifil32.dll' name 'AVIStreamAddRef';
function AVIStreamRelease(pavi: PAVISTREAM): ULONG;
  external 'avifil32.dll' name 'AVIStreamRelease';
function AVIStreamInfo(pavi: PAVISTREAM; var psi: TAVISTREAMINFO; lSize: LONG): HResult; external 'avifil32.dll' name 'AVIStreamInfo' + IEDLLWNameExt;
function AVIStreamInfoW(pavi: PAVISTREAM; var psi: TAVISTREAMINFOW; lSize: LONG): HResult; external 'avifil32.dll' name 'AVIStreamInfoW';
function AVIStreamFindSample(pavi: PAVISTREAM; lPos: LONG; lFlags: LONG): LONG;
  external 'avifil32.dll' name 'AVIStreamFindSample';
function AVIStreamReadFormat(pavi: PAVISTREAM; lPos: LONG; lpFormat: PVOID; var lpcbFormat: LONG): HResult;
  external 'avifil32.dll' name 'AVIStreamReadFormat';
function AVIStreamSetFormat(pavi: PAVIStream; lPos: LONG; lpFormat: PVOID;
  cbFormat: LONG): HResult;
  external 'avifil32.dll' name 'AVIStreamSetFormat';
function AVIStreamReadData(pavi: PAVIStream; fcc: DWORD; lp: PVOID; var lpcb: LONG): HResult;
  external 'avifil32.dll' name 'AVIStreamReadData';
function AVIStreamWriteData(pavi: PAVIStream; fcc: DWORD; lp: PVOID; cb: LONG): HResult;
  external 'avifil32.dll' name 'AVIStreamWriteData';
function AVIStreamRead(pavi: PAVIStream; lStart: LONG; lSamples: LONG; lpBuffer: PVOID;
  cbBuffer: LONG; var plBytes: LONG; var plSamples: LONG): HResult;
  external 'avifil32.dll' name 'AVIStreamRead';
function AVIStreamWrite(pavi: PAVIStream; lStart, lSamples: LONG;
  lpBuffer: PVOID; cbBuffer: LONG; dwFlags: DWORD;
  plSampWritten: pointer; plBytesWritten: pointer): HResult;
  external 'avifil32.dll' name 'AVIStreamWrite';
function AVIStreamStart(pavi: PAVIStream): LONG;
  external 'avifil32.dll' name 'AVIStreamStart';
function AVIStreamLength(pavi: PAVIStream): LONG;
  external 'avifil32.dll' name 'AVIStreamLength';
function AVIStreamTimeToSample(pavi: PAVIStream; lTime: LONG): LONG;
  external 'avifil32.dll' name 'AVIStreamTimeToSample';
function AVIStreamSampleToTime(pavi: PAVIStream; lSample: LONG): LONG;
  external 'avifil32.dll' name 'AVIStreamSampleToTime';

function AVIStreamBeginStreaming(pavi: PAVIStream; lStart: LONG; lEnd: LONG; lRate: LONG): LONG; stdcall;
  external 'avifil32.dll' name 'AVIStreamBeginStreaming';
function AVIStreamEndStreaming(pavi: PAVIStream): LONG;
  external 'avifil32.dll' name 'AVIStreamEndStreaming';

function AVIStreamGetFrameOpen(pavi: PAVISTREAM; lpbiWanted: PBITMAPINFOHEADER): PGETFRAME;
  external 'avifil32.dll' name 'AVIStreamGetFrameOpen';
function AVIStreamGetFrame(pg: PGETFRAME; lPos: LONG): PVOID;
  external 'avifil32.dll' name 'AVIStreamGetFrame';
function AVIStreamGetFrameClose(pg: PGETFRAME): HResult;
  external 'avifil32.dll' name 'AVIStreamGetFrameClose';
function AVIStreamOpenFromFile(var ppavi: PAVISTREAM; szFile: LPCTSTR; fccType: DWORD; lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult;  external 'avifil32.dll' name 'AVIStreamOpenFromFile' + IEDLLWNameExt;
function AVIStreamOpenFromFileW(var ppavi: PAVISTREAM; szFile: PWideChar; fccType: DWORD; lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult;  external 'avifil32.dll' name 'AVIStreamOpenFromFileW';
function AVIStreamCreate(var ppavi: PAVISTREAM; lParam1: LONG; lParam2: LONG; pclsidHandler: PCLSID): HResult;
  external 'avifil32.dll' name 'AVIStreamCreate';

function AVISaveV(szFile: LPCTSTR; pclsidHandler: PCLSID;
  lpfnCallback: AVISAVECALLBACK; nStreams: INTEGER;
  var ppavi: PAVISTREAM; var plpOptions: PAVICOMPRESSOPTIONS): HResult; stdcall;
  external 'avifil32.dll' name 'AVISaveV'+IEDLLWNameExt;

function AVISaveOptions(hwnd: HWND; uiFlags: UINT; nStreams: INTEGER;
  pavi: pointer; plpOptions: pointer): Boolean; stdcall;
  external 'avifil32.dll' name 'AVISaveOptions';

function AVISaveOptionsFree(nStreams: INTEGER; var plpOptions: PAVICOMPRESSOPTIONS): Boolean; stdcall;
  external 'avifil32.dll' name 'AVISaveOptionsFree';

function CreateEditableStream(var ppsEditable: PAVISTREAM; psSource: PAVISTREAM): HResult; stdcall;
  external 'avifil32.dll' name 'CreateEditableStream';

function EditStreamSetInfo(pavi: PAVISTREAM; lpinfo: PAVISTREAMINFOA; cbinfo: LONG): HResult; stdcall;
  external 'avifil32.dll' name 'EditStreamSetInfo';

function AVIMakeCompressedStream(var ppsCompressed: PAVISTREAM; psSource: PAVISTREAM; lpOptions: PAVICOMPRESSOPTIONS; pclsid: PCLSID): HResult; stdcall;
  external 'avifil32.dll' name 'AVIMakeCompressedStream';

function mmioFOURCC(cc: AnsiString): integer;
begin
  result := ord(cc[1]) or (ord(cc[2]) shl 8) or (ord(cc[3]) shl 16) or (ord(cc[4]) shl 24);
end;


end.
