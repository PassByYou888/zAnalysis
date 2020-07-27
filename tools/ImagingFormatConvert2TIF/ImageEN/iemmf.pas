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
File version 1000
*)


unit iemmf;


{$R-}
{$Q-}

{$I ie.inc}


{$ifdef IEINCLUDEMEDIAFOUNDATION}


interface


uses Windows, Classes, Sysutils, Graphics, ActiveX, Contnrs,
     {$ifdef IEHASTYPES} Types, {$endif}
     {$ifdef IEHASUITYPES} System.UITypes, {$endif}
     hyieutils, hyiedefs, iewia, imageenproc, dialogs;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Windows Media Foundation interfaces

type IE_IMFAttributes = interface(IUnknown)
  ['{2cd2d921-c447-44a7-a13c-4adabfc247e3}']
  function GetItem(const guidKey: TGUID; var pValue: PROPVARIANT): HRESULT; stdcall;
  function GetItemType(const guidKey: TGUID; out pType: DWORD): HRESULT; stdcall;
  function CompareItem(const guidKey: TGUID; const Value: PROPVARIANT; out pbResult: longbool): HRESULT; stdcall;
  function Compare(pTheirs: IE_IMFAttributes; MatchType: DWORD; out pbResult: longbool): HRESULT; stdcall;
  function GetUINT32(const guidKey: TGUID; out punValue: DWORD): HRESULT; stdcall;
  function GetUINT64(const guidKey: TGUID; out punValue: uint64): HRESULT; stdcall;
  function GetDouble(const guidKey: TGUID; out pfValue: double): HRESULT; stdcall;
  function GetGUID(const guidKey: TGUID; out pguidValue: TGUID): HRESULT; stdcall;
  function GetStringLength(const guidKey: TGUID; out pcchLength: DWORD): HRESULT; stdcall;
  function GetString(const guidKey: TGUID; pwszValue: PWideChar; cchBufSize: DWORD; var pcchLength: DWORD): HRESULT; stdcall;
  function GetAllocatedString(const guidKey: TGUID; out ppwszValue: PWideChar; out pcchLength: DWORD): HRESULT; stdcall;
  function GetBlobSize(const guidKey: TGUID; out pcbBlobSize: DWORD): HRESULT; stdcall;
  function GetBlob(const guidKey: TGUID; pBuf: pbyte; cbBufSize: DWORD; var pcbBlobSize: DWORD): HRESULT; stdcall;
  function GetAllocatedBlob(const guidKey: TGUID; out ppBuf: pbyte; out pcbSize: DWORD): HRESULT; stdcall;
  function GetUnknown(const guidKey: TGUID; const riid: TGUID; out ppv): HRESULT; stdcall;
  function SetItem(const guidKey: TGUID; const Value: PROPVARIANT): HRESULT; stdcall;
  function DeleteItem(const guidKey: TGUID): HRESULT; stdcall;
  function DeleteAllItems(): HRESULT; stdcall;
  function SetUINT32(const guidKey: TGUID; unValue: DWORD): HRESULT; stdcall;
  function SetUINT64(const guidKey: TGUID; unValue: uint64): HRESULT; stdcall;
  function SetDouble(const guidKey: TGUID; fValue: double): HRESULT; stdcall;
  function SetGUID(const guidKey: TGUID; const guidValue: TGUID): HRESULT; stdcall;
  function SetString(const guidKey: TGUID; wszValue: PWideChar): HRESULT; stdcall;
  function SetBlob(const guidKey: TGUID; pBuf: pbyte; cbBufSize: DWORD): HRESULT; stdcall;
  function SetUnknown(const guidKey: TGUID; pUnknown: IUnknown): HRESULT; stdcall;
  function LockStore(): HRESULT; stdcall;
  function UnlockStore(): HRESULT; stdcall;
  function GetCount(out pcItems: DWORD): HRESULT; stdcall;
  function GetItemByIndex(unIndex: DWORD; out pguidKey: TGUID; var pValue: PROPVARIANT): HRESULT; stdcall;
  function CopyAllItems(pDest: IE_IMFAttributes): HRESULT; stdcall;
end;


type IE_IMFActivate = interface(IE_IMFAttributes)
  ['{7FEE9E9A-4A89-47a6-899C-B6A53A70FB67}']
  function ActivateObject(const riid: TGUID; out ppv): HRESULT; stdcall;
  function ShutdownObject(): HRESULT; stdcall;
  function DetachObject(): HRESULT; stdcall;
end;


type IE_IMFMediaBuffer = interface(IUnknown)
  ['{045FA593-8799-42b8-BC8D-8968C6453507}']
  function Lock(out ppbBuffer: pbyte; out pcbMaxLength: DWORD; out pcbCurrentLength: DWORD): HRESULT; stdcall;
  function Unlock(): HRESULT; stdcall;
  function GetCurrentLength(out pcbCurrentLength: DWORD): HRESULT; stdcall;
  function SetCurrentLength(cbCurrentLength: DWORD): HRESULT; stdcall;
  function GetMaxLength(out pcbMaxLength: DWORD): HRESULT; stdcall;
end;


const IE_IMF2DBuffer_GUID: TGUID = '{7DC9D5F9-9ED9-44ec-9BBF-0600BB589FBB}';

type IE_IMF2DBuffer = interface(IUnknown)
  ['{7DC9D5F9-9ED9-44ec-9BBF-0600BB589FBB}']
  function Lock2D(out pbScanline0: pbyte; out plPitch: integer): HRESULT; stdcall;
  function Unlock2D(): HRESULT; stdcall;
  function GetScanline0AndPitch(out pbScanline0: pbyte; out plPitch: integer): HRESULT; stdcall;
  function IsContiguousFormat(out pfIsContiguous: longbool): HRESULT; stdcall;
  function GetContiguousLength(out pcbLength: DWORD): HRESULT; stdcall;
  function ContiguousCopyTo(pbDestBuffer: pbyte; cbDestBuffer: DWORD): HRESULT; stdcall;
  function ContiguousCopyFrom(pbSrcBuffer: pbyte; cbSrcBuffer: DWORD): HRESULT; stdcall;
end;


type IE_IMFSample = interface(IE_IMFAttributes)
  ['{c40a00f2-b93a-4d80-ae8c-5a1c634f58e4}']
  function GetSampleFlags(out pdwSampleFlags: DWORD): HRESULT; stdcall;
  function SetSampleFlags(dwSampleFlags: DWORD): HRESULT; stdcall;
  function GetSampleTime(out phnsSampleTime: int64): HRESULT; stdcall;
  function SetSampleTime(hnsSampleTime: int64): HRESULT; stdcall;
  function GetSampleDuration(out phnsSampleDuration: int64): HRESULT; stdcall;
  function SetSampleDuration(hnsSampleDuration: int64): HRESULT; stdcall;
  function GetBufferCount(out pdwBufferCount: DWORD): HRESULT; stdcall;
  function GetBufferByIndex(dwIndex: DWORD; out IE_IMFMediaBuffer): HRESULT; stdcall;
  function ConvertToContiguousBuffer(out ppBuffer: IE_IMFMediaBuffer): HRESULT; stdcall;
  function AddBuffer(pBuffer: IE_IMFMediaBuffer): HRESULT; stdcall;
  function RemoveBufferByIndex(dwIndex: DWORD): HRESULT; stdcall;
  function RemoveAllBuffers(): HRESULT; stdcall;
  function GetTotalLength(out pcbTotalLength: DWORD): HRESULT; stdcall;
  function CopyToBuffer(pBuffer: IE_IMFMediaBuffer): HRESULT; stdcall;
end;


type IE_IMFMediaEvent = interface(IE_IMFAttributes)
  ['{DF598932-F10C-4E39-BBA2-C308F101DAA3}']
  function GetType(out pmet: DWORD): HRESULT; stdcall;
  function GetExtendedType(out pguidExtendedType: TGUID): HRESULT; stdcall;
  function GetStatus(out phrStatus: HRESULT): HRESULT; stdcall;
  function GetValue(out pvValue: PROPVARIANT): HRESULT; stdcall;
end;


type IE_IMFSourceReaderCallback = interface(IUnknown)
  ['{deec8d99-fa1d-4d82-84c2-2c8969944867}']
  function OnReadSample(hrStatus: HRESULT; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample): HRESULT; stdcall;
  function OnFlush(dwStreamIndex: DWORD): HRESULT; stdcall;
  function OnEvent(dwStreamIndex: DWORD; pEvent: IE_IMFMediaEvent): HRESULT; stdcall;
end;


type IE_IMFAsyncResult = interface(IUnknown)
  ['{ac6b7889-0740-4d51-8619-905994a55cc6}']
  function GetState(out ppunkState: IUnknown): HRESULT; stdcall;
  function GetStatus(): HRESULT; stdcall;
  function SetStatus(hrStatus: HRESULT): HRESULT; stdcall;
  function GetObject(out ppObject: IUnknown): HRESULT; stdcall;
  function GetStateNoAddRef(): IUnknown;
end;


type IE_IMFAsyncCallback = interface(IUnknown)
  ['{a27003cf-2354-4f2a-8d6a-ab7cff15437e}']
  function GetParameters(out pdwFlags: DWORD; out pdwQueue: DWORD): HRESULT; stdcall;
  function Invoke(pAsyncResult: IE_IMFAsyncResult): HRESULT; stdcall;
end;


type IE_IMFMediaEventGenerator = interface(IUnknown)
  ['{2CD0BD52-BCD5-4B89-B62C-EADC0C031E7D}']
  function GetEvent(dwFlags: DWORD; out ppEvent: IE_IMFMediaEvent): HRESULT; stdcall;
  function BeginGetEvent(pCallback: IE_IMFAsyncCallback; punkState: IUnknown): HRESULT; stdcall;
  function EndGetEvent(pResult: IE_IMFAsyncResult; out ppEvent: IE_IMFMediaEvent): HRESULT; stdcall;
  function QueueEvent(met: DWORD; const guidExtendedType: TGUID; hrStatus: HRESULT; const pvValue: PROPVARIANT): HRESULT; stdcall;
end;


type IE_IMFMediaType = interface(IE_IMFAttributes)
  ['{44ae0fa8-ea31-4109-8d2e-4cae4997c555}']
  function GetMajorType(out pguidMajorType: TGUID): HRESULT; stdcall;
  function IsCompressedFormat(out pfCompressed: longbool): HRESULT; stdcall;
  function IsEqual(pIMediaType: IE_IMFMediaType; out pdwFlags: DWORD): HRESULT; stdcall;
  function GetRepresentation(guidRepresentation: TGUID; out ppvRepresentation: pointer): HRESULT; stdcall;
  function FreeRepresentation(guidRepresentation: TGUID; pvRepresentation: pointer): HRESULT; stdcall;
end;


type IE_IMFMediaTypeHandler = interface(IUnknown)
  ['{e93dcf6c-4b07-4e1e-8123-aa16ed6eadf5}']
  function IsMediaTypeSupported(pMediaType: IE_IMFMediaType; out ppMediaType: IE_IMFMediaType): HRESULT; stdcall;
  function GetMediaTypeCount(out pdwTypeCount: DWORD): HRESULT; stdcall;
  function GetMediaTypeByIndex(dwIndex: DWORD; out ppType: IE_IMFMediaType): HRESULT; stdcall;
  function SetCurrentMediaType(pMediaType: IE_IMFMediaType): HRESULT; stdcall;
  function GetCurrentMediaType(out ppMediaType: IE_IMFMediaType): HRESULT; stdcall;
  function GetMajorType(out pguidMajorType: TGUID): HRESULT; stdcall;
end;


type IE_IMFStreamDescriptor = interface(IE_IMFAttributes)
  ['{56c03d9c-9dbb-45f5-ab4b-d80f47c05938}']
  function GetStreamIdentifier(out pdwStreamIdentifier: DWORD): HRESULT; stdcall;
  function GetMediaTypeHandler(out ppMediaTypeHandler: IE_IMFMediaTypeHandler): HRESULT; stdcall;
end;


type IE_IMFPresentationDescriptor = interface(IE_IMFAttributes)
  ['{03cb2711-24d7-4db6-a17f-f3a7a479a536}']
  function GetStreamDescriptorCount(out pdwDescriptorCount: DWORD): HRESULT; stdcall;
  function GetStreamDescriptorByIndex(dwIndex: DWORD; out pfSelected: longbool; out ppDescriptor: IE_IMFStreamDescriptor): HRESULT; stdcall;
  function SelectStream(dwDescriptorIndex: DWORD): HRESULT; stdcall;
  function DeselectStream(dwDescriptorIndex: DWORD): HRESULT; stdcall;
  function Clone(out ppPresentationDescriptor: IE_IMFPresentationDescriptor): HRESULT; stdcall;
end;


const IE_IMFMediaSource_GUID: TGUID = '{279a808d-aec7-40c8-9c6b-a6b492c78a66}';

type IE_IMFMediaSource = interface(IE_IMFMediaEventGenerator)
  ['{279a808d-aec7-40c8-9c6b-a6b492c78a66}']
  function GetCharacteristics(out pdwCharacteristics: DWORD): HRESULT; stdcall;
  function CreatePresentationDescriptor(out ppPresentationDescriptor: IE_IMFPresentationDescriptor): HRESULT; stdcall;
  function Start(pPresentationDescriptor: IE_IMFPresentationDescriptor; const pguidTimeFormat: TGUID; const pvarStartPosition: PROPVARIANT): HRESULT; stdcall;
  function Stop(): HRESULT; stdcall;
  function Pause(): HRESULT; stdcall;
  function Shutdown(): HRESULT; stdcall;
end;


type IE_IMFSourceReader = interface(IUnknown)
  ['{70ae66f2-c809-4e4f-8915-bdcb406b7993}']
  function GetStreamSelection(dwStreamIndex: DWORD; out pfSelected: longbool): HRESULT; stdcall;
  function SetStreamSelection(dwStreamIndex: DWORD; fSelected: longbool): HRESULT; stdcall;
  function GetNativeMediaType(dwStreamIndex: DWORD; dwMediaTypeIndex: DWORD; out ppMediaType: IE_IMFMediaType): HRESULT; stdcall;
  function GetCurrentMediaType(dwStreamIndex: DWORD; out ppMediaType: IE_IMFMediaType): HRESULT; stdcall;
  function SetCurrentMediaType(dwStreamIndex: DWORD; pdwReserved: PDWORD; pMediaType: IE_IMFMediaType): HRESULT; stdcall;
  function SetCurrentPosition(const guidTimeFormat: TGUID; const varPosition: PROPVARIANT): HRESULT; stdcall;
  function ReadSample(dwStreamIndex: DWORD; dwControlFlags: DWORD; out pdwActualStreamIndex: DWORD; out pdwStreamFlags: DWORD; out pllTimestamp: int64; var ppSample: IE_IMFSample): HRESULT; stdcall;  // do not set ppSample as 'out'!
  function Flush(dwStreamIndex: DWORD): HRESULT; stdcall;
  function GetServiceForStream(dwStreamIndex: DWORD; const guidService: TGUID; const riid: TGUID; out ppvObject): HRESULT; stdcall;
  function GetPresentationAttribute(dwStreamIndex: DWORD; const guidAttribute: TGUID; out pvarAttribute: PROPVARIANT): HRESULT; stdcall;
end;


type IE_IMFByteStream = interface(IUnknown)
  ['{ad4c1b00-4bf7-422f-9175-756693d9130d}']
  function GetCapabilities(out pdwCapabilities: DWORD): HRESULT; stdcall;
  function GetLength(out pqwLength: uint64): HRESULT; stdcall;
  function SetLength(qwLength: uint64): HRESULT; stdcall;
  function GetCurrentPosition(out pqwPosition: uint64): HRESULT; stdcall;
  function SetCurrentPosition(qwPosition: uint64): HRESULT; stdcall;
  function IsEndOfStream(out pfEndOfStream: longbool): HRESULT; stdcall;
  function Read(pb: pbyte; cb: DWORD; out pcbRead: DWORD): HRESULT; stdcall;
  function BeginRead(pb: pbyte; cb: DWORD; pCallback: IE_IMFAsyncCallback; punkState: IUnknown): HRESULT; stdcall;
  function EndRead(pResult: IE_IMFAsyncResult; out pcbRead: DWORD): HRESULT; stdcall;
  function Write(pb: pbyte; cb: DWORD; out pcbWritten: DWORD): HRESULT; stdcall;
  function BeginWrite(pb: pbyte; cb: DWORD; pCallback: IE_IMFAsyncCallback; punkState: IUnknown): HRESULT; stdcall;
  function EndWrite(pResult: IE_IMFAsyncResult; out pcbWritten: DWORD): HRESULT; stdcall;
  function Seek(SeekOrigin: DWORD; llSeekOffset: int64; dwSeekFlags: DWORD; out pqwCurrentPosition: uint64): HRESULT; stdcall;
  function Flush(): HRESULT; stdcall;
  function Close(): HRESULT; stdcall;
end;


type IE_IMFStreamSink = interface(IE_IMFMediaEventGenerator)
  ['{0A97B3CF-8E7C-4a3d-8F8C-0C843DC247FB}']
  function GetMediaSink({out ppMediaSink: IE_IMFMediaSink}): HRESULT; stdcall;
  function GetIdentifier(out pdwIdentifier: DWORD): HRESULT; stdcall;
  function GetMediaTypeHandler(out ppHandler: IE_IMFMediaTypeHandler): HRESULT; stdcall;
  function ProcessSample(pSample: IE_IMFSample): HRESULT; stdcall;
  function PlaceMarker(eMarkerType: DWORD; const pvarMarkerValue: PROPVARIANT; const pvarContextValue: PROPVARIANT): HRESULT; stdcall;
  function Flush(): HRESULT; stdcall;
end;


type IE_IMFClock = interface(IUnknown)
  ['{2eb1e945-18b8-4139-9b1a-d5d584818530}']
  function GetClockCharacteristics(out pdwCharacteristics: DWORD): HRESULT; stdcall;
  function GetCorrelatedTime(dwReserved: DWORD; out pllClockTime: int64; out phnsSystemTime: int64): HRESULT; stdcall;
  function GetContinuityKey(out pdwContinuityKey: DWORD): HRESULT; stdcall;
  function GetState(dwReserved: DWORD; out peClockState: DWORD): HRESULT; stdcall;
  function GetProperties({out pClockProperties: MFCLOCK_PROPERTIES}): HRESULT; stdcall;
end;


const IE_IMFPresentationTimeSource_GUID: TGUID = '{7FF12CCE-F76F-41c2-863B-1666C8E5E139}';

type IE_IMFPresentationTimeSource = interface(IE_IMFClock)
  ['{7FF12CCE-F76F-41c2-863B-1666C8E5E139}']
  function GetUnderlyingClock(out ppClock: IE_IMFClock): HRESULT; stdcall;
end;


type IE_IMFClockStateSink = interface(IUnknown)
  ['{F6696E82-74F7-4f3d-A178-8A5E09C3659F}']
  function OnClockStart(hnsSystemTime: int64; llClockStartOffset: int64): HRESULT; stdcall;
  function OnClockStop(hnsSystemTime: int64): HRESULT; stdcall;
  function OnClockPause(hnsSystemTime: int64): HRESULT; stdcall;
  function OnClockRestart(hnsSystemTime: int64): HRESULT; stdcall;
  function OnClockSetRate(hnsSystemTime: int64; flRate: single): HRESULT; stdcall;
end;


type IE_IMFPresentationClock = interface(IE_IMFClock)
  ['{868CE85C-8EA9-4f55-AB82-B009A910A805}']
  function SetTimeSource(pTimeSource: IE_IMFPresentationTimeSource): HRESULT; stdcall;
  function GetTimeSource(out ppTimeSource: IE_IMFPresentationTimeSource): HRESULT; stdcall;
  function GetTime(out phnsClockTime: int64): HRESULT; stdcall;
  function AddClockStateSink(pStateSink: IE_IMFClockStateSink): HRESULT; stdcall;
  function RemoveClockStateSink(pStateSink: IE_IMFClockStateSink): HRESULT; stdcall;
  function Start(llClockStartOffset: int64 ): HRESULT; stdcall;
  function Stop(): HRESULT; stdcall;
  function Pause(): HRESULT; stdcall;
end;


type IE_IMFMediaSink = interface(IUnknown)
  ['{6ef2a660-47c0-4666-b13d-cbb717f2fa2c}']
  function GetCharacteristics(out pdwCharacteristics: DWORD): HRESULT; stdcall;
  function AddStreamSink(dwStreamSinkIdentifier: DWORD; pMediaType: IE_IMFMediaType; out ppStreamSink: IE_IMFStreamSink): HRESULT; stdcall;
  function RemoveStreamSink(dwStreamSinkIdentifier: DWORD): HRESULT; stdcall;
  function GetStreamSinkCount(out pcStreamSinkCount: DWORD): HRESULT; stdcall;
  function GetStreamSinkByIndex(dwIndex: DWORD; out ppStreamSink: IE_IMFStreamSink): HRESULT; stdcall;
  function GetStreamSinkById(dwStreamSinkIdentifier: DWORD; out ppStreamSink: IE_IMFStreamSink): HRESULT; stdcall;
  function SetPresentationClock(pPresentationClock: IE_IMFPresentationClock): HRESULT; stdcall;
  function GetPresentationClock(out ppPresentationClock: IE_IMFPresentationClock): HRESULT; stdcall;
  function Shutdown(): HRESULT; stdcall;
end;


type IE_IMFCollection = interface(IUnknown)
  ['{5BC8A76B-869A-46a3-9B03-FA218A66AEBE}']
  function GetElementCount(out pcElements: DWORD): HRESULT; stdcall;
  function GetElement(dwElementIndex: DWORD; out ppUnkElement: IUnknown): HRESULT; stdcall;
  function AddElement(pUnkElement: IUnknown): HRESULT; stdcall;
  function RemoveElement(dwElementIndex: DWORD; out ppUnkElement: IUnknown): HRESULT; stdcall;
  function InsertElementAt(dwIndex: DWORD; pUnknown: IUnknown): HRESULT; stdcall;
  function RemoveAllElements(): HRESULT; stdcall;
end;


type IE_MFT_INPUT_STREAM_INFO = packed record
  hnsMaxLatency: int64;
  dwFlags: DWORD;
  cbSize: DWORD;
  cbMaxLookahead: DWORD;
  cbAlignment: DWORD;
end;


type IE_MFT_OUTPUT_STREAM_INFO = packed record
  dwFlags: DWORD;
  cbSize: DWORD;
  cbAlignment: DWORD;
end;


type IE_MFT_OUTPUT_DATA_BUFFER = packed record
  dwStreamID: DWORD;
  pSample: IE_IMFSample;
  dwStatus: DWORD;
  pEvents: IE_IMFCollection;
end;
type PIE_MFT_OUTPUT_DATA_BUFFER = ^IE_MFT_OUTPUT_DATA_BUFFER;


const IE_IMFTransform_GUID: TGUID = '{bf94c121-5b05-4e6f-8000-ba598961414d}';

type IE_IMFTransform = interface(IUnknown)
  ['{bf94c121-5b05-4e6f-8000-ba598961414d}']
  function GetStreamLimits(out pdwInputMinimum: DWORD; out pdwInputMaximum: DWORD; out pdwOutputMinimum: DWORD; out pdwOutputMaximum: DWORD): HRESULT; stdcall;
  function GetStreamCount(out pcInputStreams: DWORD; out pcOutputStreams: DWORD): HRESULT; stdcall;
  function GetStreamIDs(dwInputIDArraySize: DWORD; out pdwInputIDs: DWORD; dwOutputIDArraySize: DWORD; out pdwOutputIDs: DWORD): HRESULT; stdcall;
  function GetInputStreamInfo(dwInputStreamID: DWORD; out pStreamInfo: IE_MFT_INPUT_STREAM_INFO): HRESULT; stdcall;
  function GetOutputStreamInfo(dwOutputStreamID: DWORD; out pStreamInfo: IE_MFT_OUTPUT_STREAM_INFO): HRESULT; stdcall;
  function GetAttributes(out pAttributes: IE_IMFAttributes): HRESULT; stdcall;
  function GetInputStreamAttributes(dwInputStreamID: DWORD; out pAttributes: IE_IMFAttributes): HRESULT; stdcall;
  function GetOutputStreamAttributes(dwOutputStreamID: DWORD; out pAttributes: IE_IMFAttributes): HRESULT; stdcall;
  function DeleteInputStream(dwStreamID: DWORD): HRESULT; stdcall;
  function AddInputStreams(cStreams: DWORD; adwStreamIDs: PDWORD): HRESULT; stdcall;
  function GetInputAvailableType(dwInputStreamID: DWORD; dwTypeIndex: DWORD; out ppType: IE_IMFMediaType): HRESULT; stdcall;
  function GetOutputAvailableType(dwOutputStreamID: DWORD; dwTypeIndex: DWORD; out ppType: IE_IMFMediaType): HRESULT; stdcall;
  function SetInputType(dwInputStreamID: DWORD; pType: IE_IMFMediaType; dwFlags: DWORD): HRESULT; stdcall;
  function SetOutputType(dwOutputStreamID: DWORD; pType: IE_IMFMediaType; dwFlags: DWORD): HRESULT; stdcall;
  function GetInputCurrentType(dwInputStreamID: DWORD; out ppType: IE_IMFMediaType): HRESULT; stdcall;
  function GetOutputCurrentType(dwOutputStreamID: DWORD; out ppType: IE_IMFMediaType): HRESULT; stdcall;
  function GetInputStatus(dwInputStreamID: DWORD; out pdwFlags: DWORD): HRESULT; stdcall;
  function GetOutputStatus(out pdwFlags: DWORD): HRESULT; stdcall;
  function SetOutputBounds(hnsLowerBound: int64; hnsUpperBound: int64): HRESULT; stdcall;
  function ProcessEvent(dwInputStreamID: DWORD; pEvent: IE_IMFMediaEvent): HRESULT; stdcall;
  function ProcessMessage(eMessage: DWORD; ulParam: LPARAM): HRESULT; stdcall;
  function ProcessInput(dwInputStreamID: DWORD; pSample: IE_IMFSample; dwFlags: DWORD): HRESULT; stdcall;
  function ProcessOutput(dwFlags: DWORD; cOutputBufferCount: DWORD; pOutputSamples: PIE_MFT_OUTPUT_DATA_BUFFER; var pdwStatus: DWORD): HRESULT; stdcall;
end;


const IE_IWMResamplerProps_GUID: TGUID = '{E7E9984F-F09F-4da4-903F-6E2E0EFE56B5}';

type IE_IWMResamplerProps = interface(IUnknown)
  ['{E7E9984F-F09F-4da4-903F-6E2E0EFE56B5}']
  function SetHalfFilterLength(lhalfFilterLen: integer): HRESULT; stdcall;
  function SetUserChannelMtx(userChannelMtx: psingle): HRESULT; stdcall;
end;


const IE_IMFSourceReaderEx_GUID: TGUID = '{7b981cf0-560e-4116-9875-b099895f23d7}';

// Windows 8 only!
type IE_IMFSourceReaderEx = interface(IE_IMFSourceReader)
  ['{7b981cf0-560e-4116-9875-b099895f23d7}']
  function SetNativeMediaType(dwStreamIndex: DWORD; pMediaType: IE_IMFMediaType; out pdwStreamFlags: DWORD): HRESULT; stdcall;
  function AddTransformForStream(dwStreamIndex: DWORD; pTransformOrActivate: IUnknown): HRESULT; stdcall;
  function RemoveAllTransformsForStream(dwStreamIndex: DWORD): HRESULT; stdcall;
  function GetTransformForStream(dwStreamIndex: DWORD; dwTransformIndex: DWORD; out pGuidCategory: TGUID; out ppTransform: IE_IMFTransform): HRESULT; stdcall;
end;


type IE_MFRECT = packed record
  left:   integer;
  top:    integer;
  right:  integer;
  bottom: integer;
end;


const IE_IMFVideoProcessorControl_GUID: TGUID = '{A3F675D5-6119-4f7f-A100-1D8B280F0EFB}';

// Windows 8 only!
type IE_IMFVideoProcessorControl = interface(IUnknown)
  ['{A3F675D5-6119-4f7f-A100-1D8B280F0EFB}']
  function SetBorderColor(): HRESULT; stdcall;
  function SetSourceRectangle(var pSrcRect: IE_MFRECT): HRESULT; stdcall;
  function SetDestinationRectangle(var pDstRect: IE_MFRECT): HRESULT; stdcall;
  function SetMirror(eMirror: DWORD): HRESULT; stdcall;
  function SetRotation(eRotation: DWORD): HRESULT; stdcall;
  function SetConstrictionSize(): HRESULT; stdcall;
end;


// Windows Media Foundation interfaces
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMMFDeviceList

type TIEMFDeviceList = class
  private
    m_devices: PPointerArray; // array of IE_IMFActivate objects
    m_devicesCount: DWORD;
    m_populated: boolean;
    m_names: TStringList;     // a copy of all device names as a TStringList object
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Clear();
    procedure Populate();
    property Populated: boolean read m_populated;

    function GetCount(): integer;
    function GetName(index: integer): WideString;
    function GetDevice(index: integer): IE_IMFActivate;
    function GetNames(): TStringList;
end;

// TIEMFDeviceList
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// IIEMFCallbackHandler

type IIEMFCallbackHandler = interface(IInterface)
  function OnReadSample(hrStatus: HRESULT; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample): HRESULT;
  function OnFlush(dwStreamIndex: DWORD): HRESULT;
  function OnEvent(dwStreamIndex: DWORD; pEvent: IE_IMFMediaEvent): HRESULT;
end;

// IIEMFCallbackHandler
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMFReceivedSample

{!!
<FS>TIEMFReceivedSample

<FM>Description<FN>
Represents a Media Foundation sample (video or audio).

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMFReceivedSample.DecodeSample></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMFReceivedSample.MediaType></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMFReceivedSample.Sample></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMFReceivedSample.StreamIndex></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMFReceivedSample.StreamFlags></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMFReceivedSample.StreamType></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMFReceivedSample.TimeStamp></C> </R>
</TABLE>
!!}
type TIEMFReceivedSample = class
  public

{!!
<FS>TIEMFReceivedSample.Sample

<FM>Declaration<FC>
Sample: IE_IMFSample;

<FM>Description<FN>
Contains the Media Foundation sample interface. See the <L http://msdn.microsoft.com/en-us/library/windows/desktop/ms702192(v=vs.85).aspx>MSDN documentation</L> for more info.
!!}
    Sample: IE_IMFSample;

{!!
<FS>TIEMFReceivedSample.StreamIndex

<FM>Declaration<FC>
StreamIndex: DWORD;

<FM>Description<FN>
Contains the index of stream from where this sample comes.
!!}
    StreamIndex: DWORD;

{!!
<FS>TIEMFReceivedSample.StreamFlags

<FM>Declaration<FC>
StreamFlags: DWORD;

<FM>Description<FN>
Media Foundation sample flags. See the <L http://msdn.microsoft.com/en-us/library/windows/desktop/dd375773(v=vs.85).aspx>MSDN documentation</L> for more info.
!!}
    StreamFlags: DWORD;

{!!
<FS>TIEMFReceivedSample.TimeStamp

<FM>Declaration<FC>
TimeStamp: int64;

<FM>Description<FN>
The time stamp of the sample in 100 nanosecond units (There are 1 billion nanoseconds to a second).

<FM>See Also<FN>
- <A IEMediaFoundationTimeToStr>
- <A IEMediaFoundationTimeToSec>
!!}
    TimeStamp: int64;

{!!
<FS>TIEMFReceivedSample.MediaType

<FM>Declaration<FC>
MediaType: IE_IMFMediaType;

<FM>Description<FN>
Media Foundation sample type. See the <L http://msdn.microsoft.com/en-us/library/ms704850(v=vs.85).aspx>MSDN documentation</L> for more info.
!!}
    MediaType: IE_IMFMediaType;

{!!
<FS>TIEMFReceivedSample.StreamType

<FM>Declaration<FC>
StreamType: WideString;

<FM>Description<FN>
Returns a string representing the type of the stream. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType>.
!!}
    StreamType: WideString;

    constructor Create(sample: IE_IMFSample; streamIndex: DWORD; streamFlags: DWORD; timeStamp: int64; mediaType: IE_IMFMediaType);
    destructor Destroy(); override;
    function DecodeSample(destBitmap: TIEBitmap): boolean;
end;

// TIEMFReceivedSample
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationSourceReaderCallback

TIEMediaFoundationSourceReaderCallbackEventType = (mfrceONREADSAMPLE, mfrceONFLUSH, mfrceONEVENT);

TIEMediaFoundationSourceReaderCallbackEvent = function(event: TIEMediaFoundationSourceReaderCallbackEventType; hrStatus: HRESULT; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; pEvent: IE_IMFMediaEvent): HRESULT of object;

type TIEMediaFoundationSourceReaderCallback = class(TInterfacedObject, IE_IMFSourceReaderCallback)
  private
    m_onCallBack: TIEMediaFoundationSourceReaderCallbackEvent;
  public
    constructor Create(OnCallBack: TIEMediaFoundationSourceReaderCallbackEvent);
  protected
    // IIEMFCallBackHandler implementation
    function OnReadSample(hrStatus: HRESULT; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample): HRESULT; stdcall;
    function OnFlush(dwStreamIndex: DWORD): HRESULT; stdcall;
    function OnEvent(dwStreamIndex: DWORD; pEvent: IE_IMFMediaEvent): HRESULT; stdcall;
end;

// TIEMediaFoundationSourceReaderCallback
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationVideoSampleDecoder

{!!
<FS>TIEMediaFoundationVideoSampleDecoder

<FM>Declaration<FC>
type TIEMediaFoundationVideoSampleDecoder = class
  public
    function GetSubType(): WideString; virtual; abstract;
    function Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: <A TIEBitmap>): boolean; virtual; abstract;
end;

<FM>Description<FN>
Applications can add new decoders by using <A IEMediaFoundationGetVideoSampleDecoders> and implementing this abstract class.
!!}
type TIEMediaFoundationVideoSampleDecoder = class
  public
    function GetSubType(): WideString; virtual; abstract;
    function Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean; virtual; abstract;
end;

// TIEMediaFoundationVideoSampleDecoder
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// IIEMediaFoundationReaderNotifyReceiver

{!!
<FS>IIEMediaFoundationReaderNotifyReceiver

<FM>Declaration<FC>
}
type IIEMediaFoundationReaderNotifyReceiver = interface
  ['{70E06CBA-1727-402B-857A-CC3679EDDC26}']
  procedure ReceiveNotify(sender: TObject; notifyType: TIEMediaFountationNotifyType; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; mediaType: IE_IMFMediaType; pEvent: IE_IMFMediaEvent);
end;
{!!}

// IIEMediaFoundationReaderNotifyReceiver
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationReaderWindowNotifyReceiver
// Just an helper for notifications sent by Windows messages

type TIEMediaFoundationReaderWindowNotifyReceiver = class(TInterfacedObject, IIEMediaFoundationReaderNotifyReceiver)
  private
    m_notifyWindow: THandle;
    m_notifyMessage: DWORD;
  public
    constructor Create(notifyWindow: THandle; notifyMessage: DWORD);
    destructor Destroy(); override;
    procedure ReceiveNotify(sender: TObject; notifyType: TIEMediaFountationNotifyType; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; mediaType: IE_IMFMediaType; pEvent: IE_IMFMediaEvent);
end;

// TIEMediaFoundationReaderWindowNotifyReceiver
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationAudioResampler

type TIEMediaFoundationAudioResampler = class
  private
    m_transform: IE_IMFTransform;
    m_outputBufferSize: DWORD;
  public
    constructor Create();
    destructor Destroy(); override;

    function SetInputMediaType(mediaType: IE_IMFMediaType): boolean;
    function SetOutputMediaType(mediaType: IE_IMFMediaType): boolean;

    procedure Start();
    procedure Stop();

    function PushSample(sample: IE_IMFSample): boolean;
    function GetSample(): IE_IMFSample;
end;

// TIEMediaFoundationAudioResampler
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationAudioRenderer

type TIEMediaFoundationAudioRendererRole = (
  iemfarrECONSOLE        = 0,
  iemfarrEMULTIMEDIA     = 1,
  iemfarrECOMMUNICATIONS = 2);


{!!
<FS>TIEMediaFoundationAudioRenderer

<FM>Declaration<FC>
type TIEMediaFoundationAudioRenderer = class(TInterfacedObject, IIEMediaFoundationReaderNotifyReceiver);

<FM>Description<FN>
A wrapper for the Media Foundation audio renderer.

Applications can add a <FC>TIEMediaFoundationAudioRenderer<FN> as a notification receiver (see <A TIEMediaFoundationSourceReader.PushNotifyReceiver> method) in order to automatically render the audio stream.

Notes:
- Using TIEMediaFoundationAudioRenderer with <A TIEMediaFoundationSourceReader> may produce audio out of synchronization.
- The Audio renderer supports only PCM or Float media types, so applications should set the appropriate media type (see example).

<FM>Example<FC>
// Add the audio renderer
audioStreamindex := ImageEnView1.IO.MediaFoundationSourceReader.IndexOfFirstStream(mmf_AUDIO_STREAM);
ImageEnView1.IO.MediaFoundationSourceReader.SetSelectedStreams(audioStreamIndex, true);
ImageEnView1.IO.MediaFoundationSourceReader.SetMediaTypeAudio(audioStreamIndex, 'PCM');
ImageEnView1.IO.MediaFoundationSourceReader.PushNotifyReceiver( TIEMediaFoundationAudioRenderer.Create(audioStreamIndex) );
ImageEnView1.IO.MediaFoundationSourceReader.StartCapture();

// Remove the audio renderer
ImageEnView1.IO.MediaFoundationSourceReader.StopCapture();
ImageEnView1.IO.MediaFoundationSourceReader.PopNotifyReceiver();

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.PopNotifyReceiver>
- <A TIEMediaFoundationSourceReader.PushNotifyReceiver>
!!}
type TIEMediaFoundationAudioRenderer = class(TInterfacedObject, IIEMediaFoundationReaderNotifyReceiver)
  private
    m_mediaSink: IE_IMFMediaSink;
    m_streamSink: IE_IMFStreamSink;
    m_presentationClock: IE_IMFPresentationClock;
    m_streamIndex: DWORD;
    m_resampler: TIEMediaFoundationAudioResampler;
  public
    constructor Create(streamIndex: DWORD; role: TIEMediaFoundationAudioRendererRole = iemfarrEMULTIMEDIA);
    destructor Destroy(); override;

    function SetMediaType(mediaType: IE_IMFMediaType): boolean;
    procedure ReceiveNotify(sender: TObject; notifyType: TIEMediaFountationNotifyType; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; mediaType: IE_IMFMediaType; pEvent: IE_IMFMediaEvent);
end;

// TIEMediaFoundationAudioRenderer
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationVideoProcessor

{!!
<FS>TIEMediaFoundationVideoProcessorMirror

<FM>Declaration<FC>
type TIEMediaFoundationVideoProcessorMirror = (mfpmNone = 0, mfpmHorizontal = 1, mfpmVertical = 2);

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>mfpmNone<FN></C> <C>Do not flip the image.</C> </R>
<R> <C><FC>mfpmHorizontal<FN></C> <C>Flip the image horizontally.</C> </R>
<R> <C><FC>mfpmVertical<FN></C> <C>Flip the image vertically.</C> </R>
</TABLE>
!!}
type TIEMediaFoundationVideoProcessorMirror = (mfpmNone = 0, mfpmHorizontal = 1, mfpmVertical = 2);

{!!
<FS>TIEMediaFoundationVideoProcessorRotation

<FM>Declaration<FC>
type TIEMediaFoundationVideoProcessorRotation = (mfprNone = 0, mfprNormal = 1);

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>mfprNone<FN></C> <C>Do not rotate the image.</C> </R>
<R> <C><FC>mfprNormal<FN></C> <C>Rotate the image to the correct viewing orientation.</C> </R>
</TABLE>
!!}
type TIEMediaFoundationVideoProcessorRotation = (mfprNone = 0, mfprNormal = 1);

{!!
<FS>TIEMediaFoundationVideoProcessor

<FM>Declaration<FC>
type TIEMediaFoundationVideoProcessor = class

<FM>Description<FN>
A wrapper for the Media Foundation Video Processor.

<FM>Example<FC>
// setup horizontal flip and automatic rotation
ImageEnView1.IO.MediaFoundationSourceReader.VideoProcessor.SetMirror(mfpmHorizontal);
ImageEnView1.IO.MediaFoundationSourceReader.VideoProcessor.SetRotation(mfprNormal);
ImageEnView1.IO.MediaFoundationSourceReader.StartCapture();

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.VideoProcessor>

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationVideoProcessor.IsAvailable></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationVideoProcessor.SetDestinationRectangle></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationVideoProcessor.SetMirror></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationVideoProcessor.SetRotation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationVideoProcessor.SetSourceRectangle></C> </R>
</TABLE>
!!}
type TIEMediaFoundationVideoProcessor = class
  private
    m_transform: IE_IMFTransform;
    m_control: IE_IMFVideoProcessorControl;
    m_outputBufferSize: DWORD;
    m_started: boolean;
    m_mediaBuffer: IE_IMFMediaBuffer;
    function GetIsAvailable(): boolean;
  protected
    function SetInputMediaType(mediaType: IE_IMFMediaType): boolean;
    function SetOutputMediaType(mediaType: IE_IMFMediaType): boolean;
    function GetOutputMediaType(): IE_IMFMediaType;

    procedure Start();
    procedure Stop();
    property Started: boolean read m_started;

    function PushSample(sample: IE_IMFSample): boolean; overload;
    function PushSample(buffer: pointer; bufferLen: integer): boolean; overload;
    function GetSample(): IE_IMFSample; overload;
    procedure GetSample(destBuffer: pointer); overload;
  public
    constructor Create();
    destructor Destroy(); override;
    property IsAvailable: boolean read GetIsAvailable;

    procedure SetSourceRectangle(rect: TRect);
    procedure SetDestinationRectangle(rect: TRect);
    procedure SetMirror(mirror: TIEMediaFoundationVideoProcessorMirror);
    procedure SetRotation(rotation: TIEMediaFoundationVideoProcessorRotation);
end;

// TIEMediaFoundationVideoProcessor
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationSourceReader
//
// Using some Delphi versions (ie Delphi 7) combined with some Windows versions (ie Windows 8.1 64bit) it could be
// necessary to add a "OS Exception", otherwise you will get an exception exiting the application (like "Application-defined exception...").
// This happens only inside the IDE and you can avoid it adding an OS Exception (Debugger Options) with
// range $C000000D-$C000000D, with "User Program" and "Run Handlded" options enabled.


{!!
<FS>TIEMediaFoundationSourceReader

<FM>Description<FN>
Encapsulates Microsoft Media Foundation source reader object, allowing the capture of video and audio samples from a source.

Presently TIEMediaFoundationSourceReader can capture from video capture devices (webcams), files (all formats supported by Windows Media Player) and URLs.

To setup <FC>TIEMediaFoundationSourceReader<FN>, applications must specify a video/file source, a video/audio stream and a media type. Then call GetNextSample() to read samples (images) from the source.
<A TImageEnView> and <A TImageEnIO> embed a TIEMediaFoundationSourceReader object to simplify usage.

See the VideoCapture\MediaFoundation demos for usage samples.

TIEMediaFoundationSourceReader is available in Windows Vista and newer. We have tested to confirm compatibility with Windows 7 and Windows 8.1.


<IMG help_images\IEMediaFoundation_Grabbing.gif>

<FM>Example<FC>
// This is a minimal setup to capture from a webcam (the first webcam, using first proposed media type):

ImageEnView1.IO.MediaFoundationSourceReader.SetVideoInput(0);                   // select first video input (first webcam)
ImageEnView1.IO.MediaFoundationSourceReader.SetSelectedStreams('Video', true);  // enable first video stream
ImageEnView1.IO.MediaFoundationSourceReader.SelectMediaType(mmf_VIDEO_STREAM, 0);        // select first media type of the first video stream
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

<FM>Methods and Properties<FN>
<FI>Media Foundation Availability<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationSourceReader.IsAvailable></C> </R>
</TABLE>

<FI>Events/Frame Notification<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.ClearNotifyReceivers></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.PopNotifyReceiver></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.PushNotifyReceiver></C> </R>
</TABLE>

<FI>Video Input Information<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.UpdateVideoInputs></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationSourceReader.VideoInputs></C> </R>
</TABLE>

<FI>Video Input Selection<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SetVideoInput></C> </R>
</TABLE>

<FI>File Input Selection<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SetFileInput></C> </R>
</TABLE>

<FI>URL Input Selection<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SetURLInput></C> </R>
</TABLE>

<FI>Stream Information<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.IndexOfFirstStream></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationSourceReader.StreamCount></C> </R>
</TABLE>

<FI>Stream Selection<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SetSelectedStreams></C> </R>
</TABLE>

<FI>Media Type Information<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.GetMediaType></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.GetMediaTypesCount></C> </R>
</TABLE>

<FI>Media Type Selection (for native media type selection)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SelectMediaType></C> </R>
</TABLE>

<FI>Media Type Setting (for custom media types)<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SetMediaTypeAudio></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SetMediaTypeCustom></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SetMediaTypeVideo></C> </R>
</TABLE>

<FI>Current Media Type Info<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.GetCurrentMediaType></C> </R>
</TABLE>

<FI>Seeking<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationSourceReader.Duration></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.SetPosition></C> </R>
</TABLE>

<FI>Capture Control<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationSourceReader.Capturing></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.Flush></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.PauseCapture></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.ResumeCapture></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.StartCapture></C> </R>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.StopCapture></C> </R>
</TABLE>

<FI>Sample Capture<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.GetNextSample></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEMediaFoundationSourceReader.SamplesBufferSize></C> </R>
</TABLE>

<FI>Video Processing<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEMediaFoundationSourceReader.VideoProcessor></C> </R>
</TABLE>
!!}
type TIEMediaFoundationSourceReader = class
  private
    m_lock: TRTLCriticalSection;
    m_isAvailable: boolean;
    m_videoInputs: TIEMFDeviceList;
    m_notifyReceivers: TInterfaceList;   // a list of IIEMediaFoundationReaderNotifyReceiver interfaces
    m_capturing: boolean;
    m_receivedSamples: TObjectList;      // samples received on async mode (list of TIEMFReceivedSample objects);
    m_firstTimeStamp: int64;
    m_delayFramePost: boolean;
    m_frameRequested: integer;           // increased by ReadSample() and decreased by SourceReaderCallback()
    m_source: IE_IMFMediaSource;
    m_sourceReader: IE_IMFSourceReader;
    m_streams: TObjectList;              // each m_streams[] item contains another TObjectList, which finally contains a TIEDictionary object with the mediatype attributes
    m_selectedMediaType: TObjectList;    // each m_selectedMediaType contains a TIEDictionary object with the current mediatype (maybe not one of m_streams[][] mediatypes)
    m_selectedActivate: IE_IMFActivate;  // set by SetSource()
    m_duration: int64;
    m_videoProcessor: TIEMediaFoundationVideoProcessor; // video processor (Win8 only)
    m_samplesBufferSize: integer;

  protected
    procedure PopulateStreams();
    procedure PopulateSelectedMediaType();
    function GetStreamCount(): integer;
    function GetDuration(): int64;
    function GetVideoInputs(): TStringList;
    procedure CheckVideoInputsPopulated();
    procedure CheckVideoInputIndex(index: integer);
    function IsAsyncMode(): boolean;
    function AddReceivedSample(sample: TIEMFReceivedSample): TIEMFReceivedSample;
    function PopReceivedSample(streamIndex: DWORD): TIEMFReceivedSample;
    procedure ClearReceivedSamples();
    function SourceReaderCallback(event: TIEMediaFoundationSourceReaderCallbackEventType; hrStatus: HRESULT; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; pEvent: IE_IMFMediaEvent): HRESULT;
    function ReadSample(streamIndex: DWORD): boolean; overload;
    function ReadSample(streamType: WideString): TIEMFReceivedSample; overload;
    procedure DrainSamples();
    function SetInput(activate: IE_IMFActivate): boolean;
    procedure SendNotify(notifyType: TIEMediaFountationNotifyType; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; mediaType: IE_IMFMediaType; pEvent: IE_IMFMediaEvent);
    function GetCurrentMediaTypeIntf(streamIndex: integer): IE_IMFMediaType; overload;
    function GetCurrentMediaTypeIntf(streamType: WideString): IE_IMFMediaType; overload;
    function GetCapturing(): boolean;
    procedure DoVideoProcessing(var mediaType: IE_IMFMediaType; var sample: IE_IMFSample);
    procedure SetupVideoProcessing();
    procedure FinalizeVideoProcessing();
    function GetVideoProcessor(): TIEMediaFoundationVideoProcessor;
    procedure SetSamplesBufferSize(value: integer);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure Lock();
    procedure Unlock();

    // Media foundation availability
{!!
<FS>TIEMediaFoundationSourceReader.IsAvailable

<FM>Declaration<FC>
property IsAvailable: boolean;

<FM>Description<FN>
Returns True if Media Foundation is available.

TIEMediaFoundationSourceReader has been tested to work with Windows 7 and Windows 8.1 operating systems.
!!}
    property IsAvailable: boolean read m_isAvailable;

    // Events/frames notification
    procedure PushNotifyReceiver(notifyReceiver: IIEMediaFoundationReaderNotifyReceiver);
    procedure PopNotifyReceiver();
    procedure ClearNotifyReceivers();

    // Video inputs info
    property VideoInputs: TStringList read GetVideoInputs;
    procedure UpdateVideoInputs();

    // Video inputs selection
    function SetVideoInput(index: integer): boolean; overload;
    function SetVideoInput(name: WideString): boolean; overload;

    // File input selection
    function SetFileInput(filename: WideString): boolean;

    // URL input selection
    function SetURLInput(URL: WideString): boolean;

    // Streams info
    property StreamCount: integer read GetStreamCount;
    function IndexOfFirstStream(streamType: WideString): integer;
    function GetStreamType(streamIndex: integer): WideString;

    // Streams selection
    procedure SetSelectedStreams(streamIndex: integer; selected: boolean); overload;
    procedure SetSelectedStreams(streamType: WideString; selected: boolean); overload;

    // Media types info
    function GetMediaTypesCount(streamIndex: integer): integer; overload;
    function GetMediaTypesCount(streamType: WideString): integer; overload;
    function GetMediaType(streamIndex: integer; mediaTypeIndex: integer): TIEDictionary; overload;
    function GetMediaType(streamType: WideString; mediaTypeIndex: integer): TIEDictionary; overload;

    // Media types selection (for native media types selection)
    function SelectMediaType(streamIndex: integer; mediaTypeIndex: integer): boolean; overload;
    function SelectMediaType(streamType: WideString; mediaTypeIndex: integer): boolean; overload;

    // Media type setting (for custom media types)
    function SetMediaTypeCustom(streamIndex: integer; jsonDescription: WideString): boolean; overload;
    function SetMediaTypeCustom(streamType: WideString; jsonDescription: WideString): boolean; overload;
    function SetMediaTypeVideo(streamIndex: integer; subTypeStr: WideString; frameWidth: integer = 0; frameHeight: integer = 0; frameRate: double = 0.0; videoLighting: WideString = ''): boolean; overload;
    function SetMediaTypeVideo(subTypeStr: WideString; frameWidth: integer = 0; frameHeight: integer = 0; frameRate: double = 0.0; videoLighting: WideString = ''): boolean; overload;
    function SetMediaTypeAudio(streamIndex: integer; subTypeStr: WideString): boolean; overload;
    function SetMediaTypeAudio(subTypeStr: WideString): boolean; overload;

    // Current media type info
    function GetCurrentMediaType(streamIndex: integer): TIEDictionary; overload;
    function GetCurrentMediaType(streamType: WideString): TIEDictionary; overload;

    // Seeking
    procedure SetPosition(position: int64);
    property Duration: int64 read m_duration;

    // Capture control
    function StartCapture(): boolean;
    function PauseCapture(): boolean;
    procedure ResumeCapture();
    procedure StopCapture();
    procedure Flush();
    property Capturing: boolean read GetCapturing;

    // Sample capture
    function GetNextSample(): TIEMFReceivedSample;
    property SamplesBufferSize: integer read m_samplesBufferSize write SetSamplesBufferSize;

    // Additional processing
    property VideoProcessor: TIEMediaFoundationVideoProcessor read GetVideoProcessor;

end;

// TIEMediaFoundationSourceReader
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


const
  IEMAJORTYPE_DICT_KEY:                  WideString = 'MajorType';                  // contains MF_MT_MAJOR_TYPE (string)
  IESUBTYPE_DICT_KEY:                    WideString = 'SubType';                    // contains MF_MT_SUBTYPE (string)
  IECOMPRESSED_DICT_KEY:                 WideString = 'Compressed';                 // contains MF_MT_COMPRESSED (boolean)
  IEAVGBITRATE_DICT_KEY:                 WideString = 'AvgBitrate';                 // contains MF_MT_AVG_BITRATE (integer)
  IEDEFAULTSTRIDE_DICT_KEY:              WideString = 'DefaultStride';              // contains MF_MT_DEFAULT_STRIDE (integer)
  IEFRAMERATE_DICT_KEY:                  WideString = 'FrameRate';                  // contains MF_MT_FRAME_RATE (double)
  IEFRAMERATEMAX_DICT_KEY:               WideString = 'FrameRateMax';               // contains MF_MT_FRAME_RATE_RANGE_MAX (double)
  IEFRAMERATEMIN_DICT_KEY:               WideString = 'FrameRateMin';               // contains MF_MT_FRAME_RATE_RANGE_MIN (double)
  IEFRAMEWIDTH_DICT_KEY:                 WideString = 'FrameWidth';                 // contains MF_MT_FRAME_SIZE (high dword) (integer)
  IEFRAMEHEIGHT_DICT_KEY:                WideString = 'FrameHeight';                // contains MF_MT_FRAME_SIZE (low dword) (integer)
  IEINTERLACEMODE_DICT_KEY:              WideString = 'InterlaceMode';              // contains MF_MT_INTERLACE_MODE (string)
  IEVIDEOLIGHTING_DICT_KEY:              WideString = 'VideoLighting';              // contains MF_MT_VIDEO_LIGHTING (string)
  IEAUDIOBITSPERSAMPLE_DICT_KEY:         WideString = 'AudioBitsPerSample';         // contains MF_MT_AUDIO_BITS_PER_SAMPLE (integer)
  IEAUDIOFLOATSAMPLESPERSECOND_DICT_KEY: WideString = 'AudioFloatSamplesPerSecond'; // contains MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND (double)
  IEAUDIONUMCHANNELS_DICT_KEY:           WideString = 'AudioNumChannels';           // contains MF_MT_AUDIO_NUM_CHANNELS (integer)
  IEAUDIOSAMPLESPERSECOND_DICT_KEY:      WideString = 'AudioSamplesPerSecond';      // contains MF_MT_AUDIO_SAMPLES_PER_SECOND (integer)
  IEAUDIOBLOCKALIGNMENT_DICT_KEY:        WideString = 'AudioBlockAlignment';        // contains MF_MT_AUDIO_BLOCK_ALIGNMENT (integer)
  IEALLSAMPLESINDEPENDENT_DICT_KEY:      WideString = 'AllSamplesIndependent';      // contains MF_MT_ALL_SAMPLES_INDEPENDENT (boolean)
  IEAUDIOPREFERWAVEFORMATEX_DICT_KEY:    WideString = 'AudioPreferWaveFormatEx';    // contains MF_MT_AUDIO_PREFER_WAVEFORMATEX (boolean)
  IEAUDIOAVGBYTESPERSECOND_DICT_KEY:     WideString = 'AudioAvgBytesPerSecond';     // contains MF_MT_AUDIO_AVG_BYTES_PER_SECOND (integer)
  IEVIDEOROTATION_DICT_KEY:              WideString = 'VideoRotation';              // contains MF_MT_VIDEO_ROTATION (integer)


// helpers
function IEMediaFoundationGetVideoSampleDecoders(): TObjectList;
function IEMediaFoundationTimeToStr(time: int64): string;
function IEMediaFoundationTimeToSec(time: int64): Double;
function IESecToMediaFoundationTime(sec: Double): int64;

const
  // STREAM TYPES
  mmf_ANY_STREAM          = 'Any';
  mmf_VIDEO_STREAM        = 'Video';
  mmf_AUDIO_STREAM        = 'Audio';
  mmf_PROTECTED_STREAM    = 'Protected';
  mmf_SAMI_STREAM         = 'SAMI';
  mmf_SCRIPT_STREAM       = 'Script';
  mmf_IMAGE_STREAM        = 'Image';
  mmf_HTML_STREAM         = 'HTML';
  mmf_BINARY_STREAM       = 'Binary';
  mmf_FILETRANSFER_STREAM = 'FileTransfer';

  // VIDEO STREAM FORMATS
  mmf_VideoFormat_RGB8             = 'RGB8';
  mmf_VideoFormat_RGB555           = 'RGB555';
  mmf_VideoFormat_RGB565           = 'RGB565';
  mmf_VideoFormat_RGB24            = 'RGB24';
  mmf_VideoFormat_RGB32            = 'RGB32';
  mmf_VideoFormat_ARGB32           = 'ARGB32';

  // AUDIO STREAM FORMATS
  mmf_AudioFormat_PCM              = 'PCM';
  mmf_AudioFormat_Float            = 'Float';
  mmf_AudioFormat_DTS              = 'DTS';
  mmf_AudioFormat_Dolby_AC3_SPDIF  = 'Dolby_AC3_SPDIF';
  mmf_AudioFormat_DRM              = 'DRM';
  mmf_AudioFormat_WMAudioV8        = 'WMAudioV8';
  mmf_AudioFormat_WMAudioV9        = 'WMAudioV9';
  mmf_AudioFormat_WMAudio_Lossless = 'WMAudio_Lossless';
  mmf_AudioFormat_WMASPDIF         = 'WMASPDIF';
  mmf_AudioFormat_MSP1             = 'MSP1';
  mmf_AudioFormat_MP3              = 'MP3';
  mmf_AudioFormat_MPEG             = 'MPEG';
  mmf_AudioFormat_AAC              = 'AAC';
  mmf_AudioFormat_ADTS             = 'ADTS';


implementation

uses TypInfo;



const
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE:                      TGUID = (D1: $c60ac5fe; D2: $252a; D3: $478f; D4: ($a0, $ef, $bc, $8f, $a5, $f7, $ca, $d3));
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID:          TGUID = (D1: $8ac3587a; D2: $4ae7; D3: $42d8; D4: ($99, $e0, $0a, $60, $13, $ee, $f9, $0f));
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID:          TGUID = (D1: $14dd9a1c; D2: $7cff; D3: $41be; D4: ($b1, $b9, $ba, $1a, $c6, $ec, $b5, $71));
  MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME:                    TGUID = (D1: $60d0e559; D2: $52f8; D3: $4fa2; D4: ($bb, $ce, $ac, $db, $34, $a8, $ec, $1));
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK: TGUID = (D1: $58f0aad8; D2: $22bf; D3: $4f8a; D4: ($bb, $3d, $d2, $c4, $97, $8c, $6e, $2f));
  MF_SOURCE_READER_ASYNC_CALLBACK:                         TGUID = (D1: $1e3dbeac; D2: $bb43; D3: $4c35; D4: ($b5, $07, $cd, $64, $44, $64, $c9, $65));
  MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING:                TGUID = (D1: $fb394f3d; D2: $ccf1; D3: $42ee; D4: ($bb, $b3, $f9, $b8, $45, $d5, $68, $1d));
  MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS:                 TGUID = (D1: $a634a91c; D2: $822b; D3: $41b9; D4: ($a4, $94, $4d, $e4, $64, $36, $12, $b0));
  MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ROLE:               TGUID = (D1: $6ba644ff; D2: $27c5; D3: $4d02; D4: ($98, $87, $c2, $86, $19, $fd, $b9, $1b));

  IID_IUnknown:                TGUID = '{00000000-0000-0000-C000-000000000046}';
  CLSID_CResamplerMediaObject: TGUID = '{f447b69e-1884-4a7e-8055-346f74d6edb3}';
  CLSID_VideoProcessorMFT:     TGUID = '{88753b26-5b24-49bd-b2e7-0c445c78c982}';


  // Media Type Attributes
  MF_MT_MAJOR_TYPE:                     TGUID = '{48eba18e-f8c9-4687-bf11-0a74c9f96a8f}'; // {GUID}
  MF_MT_SUBTYPE:                        TGUID = '{f7e34c9a-42e8-4714-b74b-cb29d72c35e5}'; // {GUID}
  MF_MT_COMPRESSED:                     TGUID = '{3afd0cee-18f2-4ba5-a110-8bea502e1f92}'; // {UINT32 (BOOL)}
  MF_MT_FIXED_SIZE_SAMPLES:             TGUID = '{b8ebefaf-b718-4e04-b0a9-116775e3321b}'; // {UINT32 (BOOL)}
  MF_MT_SAMPLE_SIZE:                    TGUID = '{dad3ab78-1990-408b-bce2-eba673dacc10}'; // {UINT32}
  MF_MT_AVG_BITRATE:                    TGUID = '{20332624-fb0d-4d9e-bd0d-cbf6786c102e}'; // {UINT32}
  MF_MT_DEFAULT_STRIDE:                 TGUID = '{644b4e48-1e02-4516-b0eb-c01ca9d49ac6}'; // {UINT32 (INT32)} // in bytes
  MF_MT_FRAME_RATE:                     TGUID = '{c459a2e8-3d2c-4e44-b132-fee5156c7bb0}'; // {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_RATE_RANGE_MAX:           TGUID = '{E3371D41-B4CF-4a05-BD4E-20B88BB2C4D6}'; // {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_RATE_RANGE_MIN:           TGUID = '{D2E7558C-DC1F-403f-9A72-D28BB1EB3B5E}'; // {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_SIZE:                     TGUID = '{1652c33d-d6b2-4012-b834-72030849a37d}'; // {UINT64 (HI32(Width),LO32(Height))}
  MF_MT_INTERLACE_MODE:                 TGUID = '{e2724bb8-e676-4806-b4b2-a8d6efb44ccd}'; // {UINT32 (oneof MFVideoInterlaceMode)}
  MF_MT_VIDEO_LIGHTING:                 TGUID = '{53a0529c-890b-4216-8bf9-599367ad6d20}'; // {UINT32 (oneof MFVideoLighting)}
  MF_MT_AUDIO_BITS_PER_SAMPLE:          TGUID = '{f2deb57f-40fa-4764-aa33-ed4f2d1ff669}'; // {UINT32}
  MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND: TGUID = '{fb3b724a-cfb5-4319-aefe-6e42b2406132}'; // {double}
  MF_MT_AUDIO_NUM_CHANNELS:             TGUID = '{37e48bf5-645e-4c5b-89de-ada9e29b696a}'; // {UINT32}
  MF_MT_AUDIO_SAMPLES_PER_SECOND:       TGUID = '{5faeeae7-0290-4c31-9e8a-c534f68d9dba}'; // {UINT32}
  MF_MT_AUDIO_BLOCK_ALIGNMENT:          TGUID = '{322de230-9eeb-43bd-ab7a-ff412251541d}'; // {UINT32}
  MF_MT_ALL_SAMPLES_INDEPENDENT:        TGUID = '{c9173739-5e56-461c-b713-46fb995cb95f}'; // {UINT32 (BOOL)}
  MF_MT_AUDIO_PREFER_WAVEFORMATEX:      TGUID = '{a901aaba-e037-458a-bdf6-545be2074042}'; // {UINT32 (BOOL)}
  MF_MT_AUDIO_AVG_BYTES_PER_SECOND:     TGUID = '{1aab75c8-cfef-451c-ab95-ac034b8e1731}'; // {UINT32}
  MF_MT_VIDEO_ROTATION:                 TGUID = '{C380465D-2271-428C-9B83-ECEA3B4A85C1}'; // {UINT32}

  // Major types
  MFMediaType_Default:      TGUID = (D1: $81A412E6; D2: $8103; D3: $4B06; D4: ($85, $7F, $18, $62, $78, $10, $24, $AC));
  MFMediaType_Audio:        TGUID = (D1: $73647561; D2: $0000; D3: $0010; D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  MFMediaType_Video:        TGUID = (D1: $73646976; D2: $0000; D3: $0010; D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  MFMediaType_Protected:    TGUID = (D1: $7b4b6fe6; D2: $9d04; D3: $4494; D4: ($be, $14, $7e, $0b, $d0, $76, $c8, $e4));
  MFMediaType_SAMI:         TGUID = (D1: $e69669a0; D2: $3dcd; D3: $40cb; D4: ($9e, $2e, $37, $08, $38, $7c, $06, $16));
  MFMediaType_Script:       TGUID = (D1: $72178C22; D2: $E45B; D3: $11D5; D4: ($BC, $2A, $00, $B0, $D0, $F3, $F4, $AB));
  MFMediaType_Image:        TGUID = (D1: $72178C23; D2: $E45B; D3: $11D5; D4: ($BC, $2A, $00, $B0, $D0, $F3, $F4, $AB));
  MFMediaType_HTML:         TGUID = (D1: $72178C24; D2: $E45B; D3: $11D5; D4: ($BC, $2A, $00, $B0, $D0, $F3, $F4, $AB));
  MFMediaType_Binary:       TGUID = (D1: $72178C25; D2: $E45B; D3: $11D5; D4: ($BC, $2A, $00, $B0, $D0, $F3, $F4, $AB));
  MFMediaType_FileTransfer: TGUID = (D1: $72178C26; D2: $E45B; D3: $11D5; D4: ($BC, $2A, $00, $B0, $D0, $F3, $F4, $AB));

  // Presentation Descriptor Attributes
  MF_PD_DURATION:           TGUID = (D1: $6c990d33; D2: $bb8e; D3: $477a; D4: ($85, $98, $0d, $5d, $96, $fc, $d8, $8a));

  // Direct3D formats (used to build complete video subtype GUIDs)
  D3DFMT_R8G8B8   = 20;
  D3DFMT_A8R8G8B8 = 21;
  D3DFMT_X8R8G8B8 = 22;
  D3DFMT_R5G6B5   = 23;
  D3DFMT_X1R5G5B5 = 24;
  D3DFMT_P8       = 41;

  // Wave formats (used to build complete audio subtype GUIDs)
  WAVE_FORMAT_PCM              = $0001;
  WAVE_FORMAT_IEEE_FLOAT       = $0003;
  WAVE_FORMAT_DTS              = $0008;
  WAVE_FORMAT_DOLBY_AC3_SPDIF  = $0092;
  WAVE_FORMAT_DRM              = $0009;
  WAVE_FORMAT_WMAUDIO2         = $0161;
  WAVE_FORMAT_WMAUDIO3         = $0162;
  WAVE_FORMAT_WMAUDIO_LOSSLESS = $0163;
  WAVE_FORMAT_WMASPDIF         = $0164;
  WAVE_FORMAT_WMAVOICE9        = $000A;
  WAVE_FORMAT_MPEGLAYER3       = $0055;
  WAVE_FORMAT_MPEG             = $0050;
  WAVE_FORMAT_MPEG_HEAAC       = $1610;
  WAVE_FORMAT_MPEG_ADTS_AAC    = $1600;

  // Video Subtype GUIDs (use IEConvertVideoSubTypeToString() and IEConvertStringToVideoSubType() for the other formats)
  MFVideoFormat_Base: TGUID = (D1: $00000000; D2: $0000; D3: $0010; D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));

  // Audio Subtype GUIDs (use IEConvertAudioSubTypeToString() and IEConvertStringToAudioSubType() for the other formats)
  MFAudioFormat_Base: TGUID = (D1: $00000000; D2: $0000; D3: $0010; D4: ($80, $00, $00, $aa, $00, $38, $9b, $71));

  // IMFSourceReader::ReadSample parameter dwStreamIndex values:
  MF_SOURCE_READER_FIRST_VIDEO_STREAM = $FFFFFFFC;
  MF_SOURCE_READER_FIRST_AUDIO_STREAM = $FFFFFFFD;
  MF_SOURCE_READER_ANY_STREAM         = $FFFFFFFE;
  MF_SOURCE_READER_ALL_STREAMS        = $FFFFFFFE;

  // IMFSourceReader::GetPresentationAttribute dwStreamIndex values:
  MF_SOURCE_READER_MEDIASOURCE        = $FFFFFFFF;

  // IMFSourceReader::ReadSample parameter dwControlFlags values:
  MF_SOURCE_READER_CONTROLF_DRAIN     = $00000001;

  // IMFSourceReader::ReadSample parameter pdwStreamFlags values:
  MF_SOURCE_READERF_ERROR                    = $00000001;
  MF_SOURCE_READERF_ENDOFSTREAM              = $00000002;
  MF_SOURCE_READERF_NEWSTREAM                = $00000004;
  MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED   = $00000010;
  MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED  = $00000020;
  MF_SOURCE_READERF_STREAMTICK               = $00000100;
  MF_SOURCE_READERF_ALLEFFECTSREMOVED        = $00000200;

  // MFVideoLighting values
  MFVideoLighting_Unknown     = 0;
  MFVideoLighting_bright      = 1;
  MFVideoLighting_office      = 2;
  MFVideoLighting_dim         = 3;
  MFVideoLighting_dark        = 4;
  MFVideoLighting_Last        = 5;
  MFVideoLighting_ForceDWORD  = $7FFFFFFF;

  // MFVideoInterlaceMode  values
  MFVideoInterlace_Unknown                      = 0;
  MFVideoInterlace_Progressive                  = 2;
  MFVideoInterlace_FieldInterleavedUpperFirst   = 3;
  MFVideoInterlace_FieldInterleavedLowerFirst   = 4;
  MFVideoInterlace_FieldSingleUpper             = 5;
  MFVideoInterlace_FieldSingleLower             = 6;
  MFVideoInterlace_MixedInterlaceOrProgressive  = 7;

  // Other consts
  MFSTARTUP_FULL              = 0;
  MF_SDK_VERSION              = $0002;
  MF_API_VERSION              = $0070;
  MF_VERSION                  = (MF_SDK_VERSION shl 16) or MF_API_VERSION;
                     
  // MF_FILE_ACCESSMODE values
  MF_ACCESSMODE_READ       = 1;
  MF_ACCESSMODE_WRITE      = 2;
  MF_ACCESSMODE_READWRITE  = 3;

  // MF_FILE_OPENMODE values
  MF_OPENMODE_FAIL_IF_NOT_EXIST  = 0;
  MF_OPENMODE_FAIL_IF_EXIST      = 1;
  MF_OPENMODE_RESET_IF_EXIST     = 2;
  MF_OPENMODE_APPEND_IF_EXIST    = 3;
  MF_OPENMODE_DELETE_IF_EXIST    = 4;

  // MF_FILE_FLAGS values
  MF_FILEFLAGS_NONE                 = 0;
  MF_FILEFLAGS_NOBUFFERING          = $1;
  MF_FILEFLAGS_ALLOW_WRITE_SHARING  = $2;

  // MFT_MESSAGE_TYPE values
  MFT_MESSAGE_COMMAND_FLUSH               = 0;
  MFT_MESSAGE_COMMAND_DRAIN               = $1;
  MFT_MESSAGE_SET_D3D_MANAGER             = $2;
  MFT_MESSAGE_DROP_SAMPLES                = $3;
  MFT_MESSAGE_NOTIFY_BEGIN_STREAMING      = $10000000;
  MFT_MESSAGE_NOTIFY_END_STREAMING        = $10000001;
  MFT_MESSAGE_NOTIFY_END_OF_STREAM        = $10000002;
  MFT_MESSAGE_NOTIFY_START_OF_STREAM      = $10000003;
  MFT_MESSAGE_COMMAND_MARKER              = $20000000;

  // Errors
  MF_E_TRANSFORM_NEED_MORE_INPUT: DWORD = $C00D6D72;

  // Video Lighting types
  MFVideoLighting_Bright_STR = 'Bright';
  MFVideoLighting_Office_STR = 'Office';
  MFVideoLighting_Dim_STR    = 'Dim';
  MFVideoLighting_Dark_STR   = 'Dark';

  // Interlace Modes
  MFVideoInterlace_Progressive_STR                 = 'Progressive';
  MFVideoInterlace_FieldInterleavedUpperFirst_STR  = 'FieldInterleavedUpperFirst';
  MFVideoInterlace_FieldInterleavedLowerFirst_STR  = 'FieldInterleavedLowerFirst';
  MFVideoInterlace_FieldSingleUpper_STR            = 'FieldSingleUpper';
  MFVideoInterlace_FieldSingleLower_STR            = 'FieldSingleLower';
  MFVideoInterlace_MixedInterlaceOrProgressive_STR = 'MixedInterlaceOrProgressive';

type
  MF_FILE_ACCESSMODE = DWORD;
  MF_FILE_OPENMODE   = DWORD;
  MF_FILE_FLAGS      = DWORD;


var
  // Windows Media Foundation libraries
  mfplat_lib:      THandle = 0;
  mf_lib:          THandle = 0;
  mfreadwrite_lib: THandle = 0;

  
var
  // video sample decoders (a list of TIEMediaFoundationVideoSampleDecoder implementations)
  videoSampleDecoders: TObjectList = nil;


var
  MFStartup:                           function(Version: DWORD; dwFlags: DWORD = MFSTARTUP_FULL): HRESULT; stdcall;
  MFShutdown:                          function(): HRESULT; stdcall;
  MFCreateAttributes:                  function(out ppMFAttributes: IE_IMFAttributes; cInitialSize: dword): HRESULT; stdcall;
  MFEnumDeviceSources:                 function(pAttributes: IE_IMFAttributes; out pppSourceActivate: PPointerArray; out pcSourceActivate: DWORD): HRESULT; stdcall;
  MFCreateSourceReaderFromMediaSource: function(pMediaSource: IE_IMFMediaSource; pAttributes: IE_IMFAttributes; out ppSourceReader: IE_IMFSourceReader): HRESULT; stdcall;
  MFCreateMediaType:                   function(out ppMFType: IE_IMFMediaType): HRESULT; stdcall;
  MFGetStrideForBitmapInfoHeader:      function(format: DWORD; dwWidth: DWORD; out pStride: integer): HRESULT; stdcall;
  MFCreateFile:                        function(AccessMode: MF_FILE_ACCESSMODE; OpenMode: MF_FILE_OPENMODE; fFlags: MF_FILE_FLAGS; pwszFileURL: PWideChar; out ppIByteStream: IE_IMFByteStream): HRESULT; stdcall;
  MFCreateSourceReaderFromByteStream:  function(pByteStream: IE_IMFByteStream; pAttributes: IE_IMFAttributes; out ppSourceReader: IE_IMFSourceReader): HRESULT; stdcall;
  MFGetSystemTime:                     function(): int64; stdcall;
  MFCreateSourceReaderFromURL:         function(pwszURL: PWideChar; pAttributes: IE_IMFAttributes; out ppSourceReader: IE_IMFSourceReader): HRESULT; stdcall;
  MFCreateAudioRenderer:               function(pAudioAttributes: IE_IMFAttributes; out ppSink: IE_IMFMediaSink): HRESULT; stdcall;
  MFCreatePresentationClock:           function(out ppPresentationClock: IE_IMFPresentationClock): HRESULT; stdcall;
  //MFCreateSystemTimeSource:            function(out ppSystemTimeSource: IE_IMFPresentationTimeSource): HRESULT; stdcall;
  MFCreateSample:                      function(out ppIMFSample: IE_IMFSample): HRESULT; stdcall;
  MFCreateMemoryBuffer:                function(cbMaxLength: DWORD; out ppBuffer: IE_IMFMediaBuffer): HRESULT; stdcall;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers


{!!
<FS>IEMediaFoundationGetVideoSampleDecoders

<FM>Declaration<FC>
function IEMediaFoundationGetVideoSampleDecoders(): TObjectList;;

<FM>Description<FN>
Applications add new decoders by using this function and implementing the <A TIEMediaFoundationVideoSampleDecoder> abstract class.

<FM>Example<FC>
IEMediaFoundationGetVideoSampleDecoders().Add(TIEMediaFoundationVideoSampleDecoder_YOURFORMAT.Create());
!!}
function IEMediaFoundationGetVideoSampleDecoders(): TObjectList;
begin
  if videoSampleDecoders = nil then
    videoSampleDecoders := TObjectList.Create();
  result := videoSampleDecoders;
end;


procedure IEMediaFoundationSetupDefaultVideoSampleDecoders(); forward;


function IEMFLoadLibrary(): boolean;
begin
  if mfplat_lib = 0 then
  begin
    mfplat_lib := LoadLibrary('Mfplat.dll');
    if mfplat_lib <> 0 then
    begin
      MFStartup                      := GetProcAddress(mfplat_lib, 'MFStartup');
      MFShutdown                     := GetProcAddress(mfplat_lib, 'MFShutdown');
      MFCreateAttributes             := GetProcAddress(mfplat_lib, 'MFCreateAttributes');
      MFCreateMediaType              := GetProcAddress(mfplat_lib, 'MFCreateMediaType');
      MFGetStrideForBitmapInfoHeader := GetProcAddress(mfplat_lib, 'MFGetStrideForBitmapInfoHeader');
      MFCreateFile                   := GetProcAddress(mfplat_lib, 'MFCreateFile');
      MFGetSystemTime                := GetProcAddress(mfplat_lib, 'MFGetSystemTime');
      MFCreateSample                 := GetProcAddress(mfplat_lib, 'MFCreateSample');
      MFCreateMemoryBuffer           := GetProcAddress(mfplat_lib, 'MFCreateMemoryBuffer');
    end;

    mf_lib := LoadLibrary('Mf.dll');
    if mf_lib <> 0 then
    begin
      MFEnumDeviceSources       := GetProcAddress(mf_lib, 'MFEnumDeviceSources');
      MFCreateAudioRenderer     := GetProcAddress(mf_lib, 'MFCreateAudioRenderer');
      MFCreatePresentationClock := GetProcAddress(mf_lib, 'MFCreatePresentationClock');
    end;

    mfreadwrite_lib := LoadLibrary('Mfreadwrite.dll');
    if mfreadwrite_lib <> 0 then
    begin
      MFCreateSourceReaderFromMediaSource := GetProcAddress(mfreadwrite_lib, 'MFCreateSourceReaderFromMediaSource');
      MFCreateSourceReaderFromByteStream  := GetProcAddress(mfreadwrite_lib, 'MFCreateSourceReaderFromByteStream');
      MFCreateSourceReaderFromURL         := GetProcAddress(mfreadwrite_lib, 'MFCreateSourceReaderFromURL');
    end;

    IEMediaFoundationSetupDefaultVideoSampleDecoders();
  end;

  result := (mfplat_lib <> 0) and (mf_lib <> 0) and (mfreadwrite_lib <> 0);
end;


procedure IEMFUnloadLibrary();
begin
  if mfreadwrite_lib <> 0 then
    FreeLibrary(mfreadwrite_lib);

  if mf_lib <> 0 then
    FreeLibrary(mf_lib);

  if mfplat_lib <> 0 then
    FreeLibrary(mfplat_lib);

  FreeAndNil(videoSampleDecoders);
end;


function IEMFStartup(): boolean;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);
  result := IEMFLoadLibrary() and assigned(MFStartup) and SUCCEEDED( MFStartup(MF_VERSION) );
end;


procedure IEMFShutdown();
begin
  if (mfplat_lib <> 0) and (mf_lib <> 0) and (mfreadwrite_lib <> 0) and assigned(MFShutdown) then
    MFShutdown();
  CoUninitialize();
end;


function IEGetStringFromAllocatedString(attributes: IE_IMFAttributes; const guidKey: TGUID): WideString;
var
  val: PWideChar;
  len: DWORD;
begin
  attributes.GetAllocatedString(MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME, val, len);
  SetLength(result, len);
  CopyMemory(@result[1], val, len * 2);
  CoTaskMemFree(val);
end;


function IEConvertMajorTypeToString(const majorTypeGuid: TGUID): WideString;
begin
  if CompareGUID(majorTypeGuid, MFMediaType_Audio) then
    result := mmf_AUDIO_STREAM
  else
  if CompareGUID(majorTypeGuid, MFMediaType_Video) then
    result := mmf_VIDEO_STREAM
  else
  if CompareGUID(majorTypeGuid, MFMediaType_Protected) then
    result := mmf_PROTECTED_STREAM
  else
  if CompareGUID(majorTypeGuid, MFMediaType_SAMI) then
    result := mmf_SAMI_STREAM
  else
  if CompareGUID(majorTypeGuid, MFMediaType_Script) then
    result := mmf_SCRIPT_STREAM
  else
  if CompareGUID(majorTypeGuid, MFMediaType_Image) then
    result := mmf_IMAGE_STREAM
  else
  if CompareGUID(majorTypeGuid, MFMediaType_HTML) then
    result := mmf_HTML_STREAM
  else
  if CompareGUID(majorTypeGuid, MFMediaType_Binary) then
    result := mmf_BINARY_STREAM
  else
  if CompareGUID(majorTypeGuid, MFMediaType_FileTransfer) then
    result := mmf_FILETRANSFER_STREAM
  else
    result := 'Unknown';
end;


function IEConvertStringToMajorType(value: WideString): TGUID;
begin
  if value = mmf_AUDIO_STREAM then
    result := MFMediaType_Audio
  else
  if value = mmf_VIDEO_STREAM then
    result := MFMediaType_Video
  else
  if value = mmf_PROTECTED_STREAM then
    result := MFMediaType_Protected
  else
  if value = mmf_SAMI_STREAM then
    result := MFMediaType_SAMI
  else
  if value = mmf_SCRIPT_STREAM then
    result := MFMediaType_Script
  else
  if value = mmf_IMAGE_STREAM then
    result := MFMediaType_Image
  else
  if value = mmf_HTML_STREAM then
    result := MFMediaType_HTML
  else
  if value = mmf_BINARY_STREAM then
    result := MFMediaType_Binary
  else
  if value = mmf_FILETRANSFER_STREAM then
    result := MFMediaType_FileTransfer
  else
    result := MFMediaType_Default
end;


// ex: MFVideoFormat_RGB8 -> 'RGB8',  MFAudioFormat_PCM -> 'PCM', etc..
function IEConvertSubTypeToString(const videoSubTypeGuid: TGUID): WideString;
var
  astr: AnsiString;
begin
  result := '';
  if CompareMem(@videoSubTypeGuid.D2, @MFVideoFormat_Base.D2, sizeof(TGUID) - 4) then // compare only from D2 (do not compare D1)
  begin
    case videoSubTypeGuid.D1 of
      D3DFMT_P8                    : result := mmf_VideoFormat_RGB8;
      D3DFMT_X1R5G5B5              : result := mmf_VideoFormat_RGB555;
      D3DFMT_R5G6B5                : result := mmf_VideoFormat_RGB565;
      D3DFMT_R8G8B8                : result := mmf_VideoFormat_RGB24;
      D3DFMT_X8R8G8B8              : result := mmf_VideoFormat_RGB32;
      D3DFMT_A8R8G8B8              : result := mmf_VideoFormat_ARGB32;
      WAVE_FORMAT_PCM              : result := mmf_AudioFormat_PCM;
      WAVE_FORMAT_IEEE_FLOAT       : result := mmf_AudioFormat_Float;
      WAVE_FORMAT_DTS              : result := mmf_AudioFormat_DTS;
      WAVE_FORMAT_DOLBY_AC3_SPDIF  : result := mmf_AudioFormat_Dolby_AC3_SPDIF;
      WAVE_FORMAT_DRM              : result := mmf_AudioFormat_DRM;
      WAVE_FORMAT_WMAUDIO2         : result := mmf_AudioFormat_WMAudioV8;
      WAVE_FORMAT_WMAUDIO3         : result := mmf_AudioFormat_WMAudioV9;
      WAVE_FORMAT_WMAUDIO_LOSSLESS : result := mmf_AudioFormat_WMAudio_Lossless;
      WAVE_FORMAT_WMASPDIF         : result := mmf_AudioFormat_WMASPDIF;
      WAVE_FORMAT_WMAVOICE9        : result := mmf_AudioFormat_MSP1;
      WAVE_FORMAT_MPEGLAYER3       : result := mmf_AudioFormat_MP3;
      WAVE_FORMAT_MPEG             : result := mmf_AudioFormat_MPEG;
      WAVE_FORMAT_MPEG_HEAAC       : result := mmf_AudioFormat_AAC;
      WAVE_FORMAT_MPEG_ADTS_AAC    : result := mmf_AudioFormat_ADTS;
      else
      begin
        // Extract fourcc code (other cases like MFVideoFormat_AI44...)
        // these are: 'AI44', 'AYUV', 'YUY2', 'YVYU', 'YVU9', 'UYVY', 'NV11', 'NV12', 'YV12', 'I420', 'IYUV', 'Y210', 'Y216', 'Y410', 'Y416',
        //            'Y41P', 'Y41T', 'Y42T', 'P210', 'P216', 'P010', 'P016', 'v210', 'v216', 'v410', 'MP43', 'MP4S', 'M4S2', 'MP4V', 'WMV1',
        //            'WMV2', 'WMV3', 'WVC1', 'MSS1', 'MSS2', 'MPG1', 'dvsl', 'dvsd', 'dvhd', 'dv25', 'dv50', 'dvh1', 'dvc ', 'H264', 'MJPG'
        SetLength(astr, 4);
        CopyMemory(@astr[1], @videoSubTypeGuid.D1, 4);
        result := WideString(astr);
      end;
    end;
  end;
end;


function IEConvertStringToSubType(value: WideString): TGUID;
var
  astr: AnsiString;
begin
  CopyMemory(@result.D2, @MFVideoFormat_Base.D2, sizeof(TGUID) - 4);  // copy from D2

  // VIDEO

  // MFVideoFormat_RGB8
  if value = mmf_VideoFormat_RGB8 then
    result.D1 := D3DFMT_P8
  // MFVideoFormat_RGB555
  else
  if value = mmf_VideoFormat_RGB555 then
    result.D1 := D3DFMT_X1R5G5B5
  // MFVideoFormat_RGB565
  else
  if value = mmf_VideoFormat_RGB565 then
    result.D1 := D3DFMT_R5G6B5
  // MFVideoFormat_RGB24
  else
  if value = mmf_VideoFormat_RGB24 then
    result.D1 := D3DFMT_R8G8B8
  // MFVideoFormat_RGB32
  else
  if value = mmf_VideoFormat_RGB32 then
    result.D1 := D3DFMT_X8R8G8B8
  // MFVideoFormat_ARGB32
  else
  if value = mmf_VideoFormat_ARGB32 then
    result.D1 := D3DFMT_A8R8G8B8

  // AUDIO

  // MFAudioFormat_PCM
  else
  if value = mmf_AudioFormat_PCM then
    result.D1 := WAVE_FORMAT_PCM
  // MFAudioFormat_Float
  else
  if value = mmf_AudioFormat_Float then
    result.D1 := WAVE_FORMAT_IEEE_FLOAT
  // MFAudioFormat_DTS
  else
  if value = mmf_AudioFormat_DTS then
    result.D1 := WAVE_FORMAT_DTS
  // MFAudioFormat_Dolby_AC3_SPDIF
  else
  if value = mmf_AudioFormat_Dolby_AC3_SPDIF then
    result.D1 := WAVE_FORMAT_DOLBY_AC3_SPDIF
  // MFAudioFormat_DRM
  else
  if value = mmf_AudioFormat_DRM then
    result.D1 := WAVE_FORMAT_DRM
  // MFAudioFormat_WMAudioV8
  else
  if value = mmf_AudioFormat_WMAudioV8 then
    result.D1 := WAVE_FORMAT_WMAUDIO2
  // MFAudioFormat_WMAudioV9
  else
  if value = mmf_AudioFormat_WMAudioV9 then
    result.D1 := WAVE_FORMAT_WMAUDIO3
  // MFAudioFormat_WMAudio_Lossless
  else
  if value = mmf_AudioFormat_WMAudio_Lossless then
    result.D1 := WAVE_FORMAT_WMAUDIO_LOSSLESS
  // MFAudioFormat_WMASPDIF
  else
  if value = mmf_AudioFormat_WMASPDIF then
    result.D1 := WAVE_FORMAT_WMASPDIF
  // MFAudioFormat_MSP1
  else
  if value = mmf_AudioFormat_MSP1 then
    result.D1 := WAVE_FORMAT_WMAVOICE9
  // MFAudioFormat_MP3
  else
  if value = mmf_AudioFormat_MP3 then
    result.D1 := WAVE_FORMAT_MPEGLAYER3
  // MFAudioFormat_MPEG
  else
  if value = mmf_AudioFormat_MPEG then
    result.D1 := WAVE_FORMAT_MPEG
  // MFAudioFormat_AAC
  else
  if value = mmf_AudioFormat_AAC then
    result.D1 := WAVE_FORMAT_MPEG_HEAAC
  // MFAudioFormat_ADTS
  else
  if value = mmf_AudioFormat_ADTS then
    result.D1 := WAVE_FORMAT_MPEG_ADTS_AAC

  // fourcc code (like MFVideoFormat_AI44...)
  else
  begin
    astr := AnsiString(value);
    CopyMemory(@result.D1, @astr[1], 4);
  end;
end;


function IEGetMediaTypeFrameSize(mediaType: IE_IMFMediaType; out width: DWORD; out height: DWORD): boolean;
var
  u64: uint64;
begin
  width  := 0;
  height := 0;
  result := SUCCEEDED(mediaType.GetUINT64(MF_MT_FRAME_SIZE, u64));
  if result then
  begin
    width  := u64 shr 32;
    height := u64 and $FFFFFFFF;
  end;
end;


function IEGetMediaTypeFrameStride(mediaType: IE_IMFMediaType; const subType: TGUID; width: DWORD): integer;
var
  u32: DWORD;
begin
  result := 0;  // invalid stride
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_DEFAULT_STRIDE, u32)) then
    result := integer(u32)
  else
  if SUCCEEDED(MFGetStrideForBitmapInfoHeader(subType.D1, width, result)) then
    result := {abs}(result) // on windows 8 MFGetStrideForBitmapInfoHeader with RGB32, even when it should not be
  else
    result := 0;
end;


function IEConvertVideoLightingToString(videoLighting: DWORD): WideString;
begin
  case videoLighting of
    MFVideoLighting_bright : result := MFVideoLighting_Bright_STR;
    MFVideoLighting_office : result := MFVideoLighting_Office_STR;
    MFVideoLighting_dim    : result := MFVideoLighting_Dim_STR;
    MFVideoLighting_dark   : result := MFVideoLighting_Dark_STR;
    else
      result := 'Unknown';
  end;
end;


function IEConvertStringToVideoLighting(value: WideString): DWORD;
begin
  if value = MFVideoLighting_Bright_STR then
    result := MFVideoLighting_bright
  else
  if value = MFVideoLighting_Office_STR then
    result := MFVideoLighting_office
  else
  if value = MFVideoLighting_Dim_STR then
    result := MFVideoLighting_dim
  else
  if value = MFVideoLighting_Dark_STR then
    result := MFVideoLighting_dark
  else
    result := MFVideoLighting_Unknown;
end;


function IEConvertInterlaceModeToString(interlaceMode: DWORD): WideString;
begin
  case interlaceMode of
    MFVideoInterlace_Progressive                 : result := MFVideoInterlace_Progressive_STR;
    MFVideoInterlace_FieldInterleavedUpperFirst  : result := MFVideoInterlace_FieldInterleavedUpperFirst_STR;
    MFVideoInterlace_FieldInterleavedLowerFirst  : result := MFVideoInterlace_FieldInterleavedLowerFirst_STR;
    MFVideoInterlace_FieldSingleUpper            : result := MFVideoInterlace_FieldSingleUpper_STR;
    MFVideoInterlace_FieldSingleLower            : result := MFVideoInterlace_FieldSingleLower_STR;
    MFVideoInterlace_MixedInterlaceOrProgressive : result := MFVideoInterlace_MixedInterlaceOrProgressive_STR;
    else
      result := 'Unknown';
  end;
end;


function IEConvertStringToInterlaceMode(value: WideString): DWORD;
begin
  if value = MFVideoInterlace_Progressive_STR then
    result := MFVideoInterlace_Progressive
  else
  if value = MFVideoInterlace_FieldInterleavedUpperFirst_STR then
    result := MFVideoInterlace_FieldInterleavedUpperFirst
  else
  if value = MFVideoInterlace_FieldInterleavedLowerFirst_STR then
    result := MFVideoInterlace_FieldInterleavedLowerFirst
  else
  if value = MFVideoInterlace_FieldSingleUpper_STR then
    result := MFVideoInterlace_FieldSingleUpper
  else
  if value = MFVideoInterlace_FieldSingleLower_STR then
    result := MFVideoInterlace_FieldSingleLower
  else
  if value = MFVideoInterlace_MixedInterlaceOrProgressive_STR then
    result := MFVideoInterlace_MixedInterlaceOrProgressive
  else
    result := MFVideoInterlace_Unknown;
end;


function IECreateDictionaryFromMediaType(mediaType: IE_IMFMediaType): TIEDictionary;
var
  majType, subType: TGUID;
  u32: DWORD;
  u64: uint64;
  f64: double;
  width, height: DWORD;
begin
  result := TIEDictionary.Create();

  // MF_MT_MAJOR_TYPE -> IEMAJORTYPE_DICT_KEY
  if not SUCCEEDED(mediaType.GetGUID(MF_MT_MAJOR_TYPE, majType)) then
    exit;
  result.Insert(IEMAJORTYPE_DICT_KEY, IEConvertMajorTypeToString(majType));

  // MF_MT_SUBTYPE -> IESUBTYPE_DICT_KEY
  if SUCCEEDED(mediaType.GetGUID(MF_MT_SUBTYPE, subType)) then
    result.Insert(IESUBTYPE_DICT_KEY, IEConvertSubTypeToString(subType));

  // MF_MT_COMPRESSED -> IECOMPRESSED_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_COMPRESSED, u32)) then
    result.Insert(IECOMPRESSED_DICT_KEY, boolean(u32));

  // MF_MT_AVG_BITRATE -> IEAVGBITRATE_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_AVG_BITRATE, u32)) then
    result.Insert(IEAVGBITRATE_DICT_KEY, u32);

  // MF_MT_FRAME_SIZE -> (IEFRAMEWIDTH_DICT_KEY, IEFRAMEHEIGHT_DICT_KEY)
  if IEGetMediaTypeFrameSize(mediaType, width, height) then
  begin
    result.Insert(IEFRAMEWIDTH_DICT_KEY, width);
    result.Insert(IEFRAMEHEIGHT_DICT_KEY, height);
  end;

  // MF_MT_DEFAULT_STRIDE -> IEDEFAULTSTRIDE_DICT_KEY
  result.Insert(IEDEFAULTSTRIDE_DICT_KEY, IEGetMediaTypeFrameStride(mediaType, subType, width));

  // MF_MT_FRAME_RATE -> IEFRAMERATE_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT64(MF_MT_FRAME_RATE, u64)) then
    result.Insert(IEFRAMERATE_DICT_KEY, (u64 shr 32) / (u64 and $FFFFFFFF) );

  // MF_MT_FRAME_RATE_RANGE_MAX -> IEFRAMERATEMAX_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT64(MF_MT_FRAME_RATE_RANGE_MAX, u64)) then
    result.Insert(IEFRAMERATEMAX_DICT_KEY, (u64 shr 32) / (u64 and $FFFFFFFF) );

  // MF_MT_FRAME_RATE_RANGE_MIN -> IEFRAMERATEMIN_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT64(MF_MT_FRAME_RATE_RANGE_MIN, u64)) then
    result.Insert(IEFRAMERATEMIN_DICT_KEY, (u64 shr 32) / (u64 and $FFFFFFFF) );

  // MF_MT_INTERLACE_MODE -> IEINTERLACEMODE_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_INTERLACE_MODE, u32)) then
    result.Insert(IEINTERLACEMODE_DICT_KEY, IEConvertInterlaceModeToString(u32));

  // MF_MT_VIDEO_LIGHTING -> IEVIDEOLIGHTING_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_VIDEO_LIGHTING, u32)) then
    result.Insert(IEVIDEOLIGHTING_DICT_KEY, IEConvertVideoLightingToString(u32));

  // MF_MT_AUDIO_BITS_PER_SAMPLE -> IEAUDIOBITSPERSAMPLE_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE, u32)) then
    result.Insert(IEAUDIOBITSPERSAMPLE_DICT_KEY, u32);

  // MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND -> IEAUDIOFLOATSAMPLESPERSECOND
  if SUCCEEDED(mediaType.GetDouble(MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND, f64)) then
    result.Insert(IEAUDIOFLOATSAMPLESPERSECOND_DICT_KEY, f64);

  // MF_MT_AUDIO_NUM_CHANNELS -> IEAUDIONUMCHANNELS_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS, u32)) then
    result.Insert(IEAUDIONUMCHANNELS_DICT_KEY, u32);

  // MF_MT_AUDIO_SAMPLES_PER_SECOND -> IEAUDIOSAMPLESPERSECOND_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, u32)) then
    result.Insert(IEAUDIOSAMPLESPERSECOND_DICT_KEY, u32);

  // MF_MT_AUDIO_BLOCK_ALIGNMENT -> IEAUDIOBLOCKALIGNMENT_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT, u32)) then
    result.Insert(IEAUDIOBLOCKALIGNMENT_DICT_KEY, u32);

  // MF_MT_ALL_SAMPLES_INDEPENDENT -> IEALLSAMPLESINDEPENDENT_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT, u32)) then
    result.Insert(IEALLSAMPLESINDEPENDENT_DICT_KEY, boolean(u32));

  // MF_MT_AUDIO_PREFER_WAVEFORMATEX -> IEAUDIOPREFERWAVEFORMATEX_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_AUDIO_PREFER_WAVEFORMATEX, u32)) then
    result.Insert(IEAUDIOPREFERWAVEFORMATEX_DICT_KEY, boolean(u32));

  // MF_MT_AUDIO_AVG_BYTES_PER_SECOND -> IEAUDIOAVGBYTESPERSECOND_DICT_KEY
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND, u32)) then
    result.Insert(IEAUDIOAVGBYTESPERSECOND_DICT_KEY, u32);

  // MF_MT_VIDEO_ROTATION -> IEVIDEOROTATION_DICT_KEY (win8 only)
  if SUCCEEDED(mediaType.GetUINT32(MF_MT_VIDEO_ROTATION, u32)) then
    result.Insert(IEVIDEOROTATION_DICT_KEY, u32);
end;


// not all IECreateDictionaryFromMediaType() keys are supported
function IECreateMediaTypeFromDictionary(dict: TIEDictionary): IE_IMFMediaType; overload;
var
  num, den, w, h: integer;
begin
  result := nil;
  MFCreateMediaType(result);

  // IEMAJORTYPE_DICT_KEY -> MF_MT_MAJOR_TYPE
  if dict.HasKey(IEMAJORTYPE_DICT_KEY) then
    result.SetGUID(MF_MT_MAJOR_TYPE, IEConvertStringToMajorType(dict.GetString(IEMAJORTYPE_DICT_KEY)));

  // IESUBTYPE_DICT_KEY -> MF_MT_SUBTYPE
  if dict.HasKey(IESUBTYPE_DICT_KEY) then
    result.SetGUID(MF_MT_SUBTYPE, IEConvertStringToSubType(dict.GetString(IESUBTYPE_DICT_KEY)));

  // IECOMPRESSED_DICT_KEY -> MF_MT_COMPRESSED
  if dict.HasKey(IECOMPRESSED_DICT_KEY) then
    result.SetUINT32(MF_MT_COMPRESSED, DWORD(dict.GetBoolean(IECOMPRESSED_DICT_KEY)));

  // IEFRAMERATE_DICT_KEY -> MF_MT_FRAME_RATE
  if dict.HasKey(IEFRAMERATE_DICT_KEY) then
  begin
    IEDecimalToFraction(dict.GetDouble(IEFRAMERATE_DICT_KEY), num, den);
    result.SetUINT64(MF_MT_FRAME_RATE, (uint64(num) shl 32) or uint64(den));
  end;

  // (IEFRAMEWIDTH_DICT_KEY, IEFRAMEHEIGHT_DICT_KEY) -> MF_MT_FRAME_SIZE
  if dict.HasKey(IEFRAMEWIDTH_DICT_KEY) and dict.HasKey(IEFRAMEHEIGHT_DICT_KEY) then
  begin
    w := dict.GetInteger(IEFRAMEWIDTH_DICT_KEY);
    h := dict.GetInteger(IEFRAMEHEIGHT_DICT_KEY);
    result.SetUINT64(MF_MT_FRAME_SIZE, (uint64(w) shl 32) or uint64(h));
  end;

  // IEINTERLACEMODE_DICT_KEY -> MF_MT_INTERLACE_MODE
  if dict.HasKey(IEINTERLACEMODE_DICT_KEY) then
    result.SetUINT32(MF_MT_INTERLACE_MODE, IEConvertStringToInterlaceMode(dict.GetString(IEINTERLACEMODE_DICT_KEY)));

  // IEVIDEOLIGHTING_DICT_KEY -> MF_MT_VIDEO_LIGHTING
  if dict.HasKey(IEVIDEOLIGHTING_DICT_KEY) then
    result.SetUINT32(MF_MT_VIDEO_LIGHTING, IEConvertStringToVideoLighting(dict.GetString(IEVIDEOLIGHTING_DICT_KEY)));

  // IEAUDIOSAMPLESPERSECOND_DICT_KEY -> MF_MT_AUDIO_SAMPLES_PER_SECOND
  if dict.HasKey(IEAUDIOSAMPLESPERSECOND_DICT_KEY) then
    result.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, dict.GetInteger(IEAUDIOSAMPLESPERSECOND_DICT_KEY));

  // IEAUDIOBITSPERSAMPLE_DICT_KEY -> MF_MT_AUDIO_BITS_PER_SAMPLE
  if dict.HasKey(IEAUDIOBITSPERSAMPLE_DICT_KEY) then
    result.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE, dict.GetInteger(IEAUDIOBITSPERSAMPLE_DICT_KEY));

  // IEAUDIONUMCHANNELS_DICT_KEY -> MF_MT_AUDIO_NUM_CHANNELS
  if dict.HasKey(IEAUDIONUMCHANNELS_DICT_KEY) then
    result.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS, dict.GetInteger(IEAUDIONUMCHANNELS_DICT_KEY));

  // IEAUDIOBLOCKALIGNMENT_DICT_KEY -> MF_MT_AUDIO_BLOCK_ALIGNMENT
  if dict.HasKey(IEAUDIOBLOCKALIGNMENT_DICT_KEY) then
    result.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT, dict.GetInteger(IEAUDIOBLOCKALIGNMENT_DICT_KEY));

  // IEALLSAMPLESINDEPENDENT_DICT_KEY -> MF_MT_ALL_SAMPLES_INDEPENDENT
  if dict.HasKey(IEALLSAMPLESINDEPENDENT_DICT_KEY) then
    result.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT, DWORD(dict.GetBoolean(IEALLSAMPLESINDEPENDENT_DICT_KEY)));

  // IEAUDIOPREFERWAVEFORMATEX_DICT_KEY -> MF_MT_AUDIO_PREFER_WAVEFORMATEX
  if dict.HasKey(IEAUDIOPREFERWAVEFORMATEX_DICT_KEY) then
    result.SetUINT32(MF_MT_AUDIO_PREFER_WAVEFORMATEX, DWORD(dict.GetBoolean(IEAUDIOPREFERWAVEFORMATEX_DICT_KEY)));

  // MF_MT_AUDIO_AVG_BYTES_PER_SECOND -> IEAUDIOAVGBYTESPERSECOND_DICT_KEY
  if dict.HasKey(IEAUDIOAVGBYTESPERSECOND_DICT_KEY) then
    result.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND, dict.GetInteger(IEAUDIOAVGBYTESPERSECOND_DICT_KEY));

  // IEVIDEOROTATION_DICT_KEY -> MF_MT_VIDEO_ROTATION (win8 only)
  if dict.HasKey(IEVIDEOROTATION_DICT_KEY) then
    result.SetUINT32(MF_MT_VIDEO_ROTATION, dict.GetInteger(IEVIDEOROTATION_DICT_KEY));
end;


function IECreateMediaTypeFromDictionary(dictStr: WideString): IE_IMFMediaType; overload;
var
  dict: TIEDictionary;
begin
  dict := TIEDictionary.Create();
  try
    dict.Parse(dictStr);
    result := IECreateMediaTypeFromDictionary(dict);
  finally
    dict.Free;
  end;
end;


{!!
<FS>IEMediaFoundationTimeToStr

<FM>Declaration<FC>
function IEMediaFoundationTimeToStr(time: int64): string;

<FM>Description<FN>
Converts Media Foundation times (100 nanosecond units) to their string representation (HH:MM:SS:DD).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>time<FN></C> <C>The Media Foundation time in 100 nanosecond units</C> </R>
</TABLE>

<FM>Example<FC>
// Gets the source duration as a 'HH:MM:SS:DD' string
durationStr := IEMediaFoundationTimeToStr(ImageEnView1.IO.MediaFoundationSourceReader.Duration);

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.Duration>
- <A IEMediaFoundationTimeToSec>
!!}
function IEMediaFoundationTimeToStr(time: int64): string;
var
  tot: int64;
  hh, mm, ss, ds: integer;
begin
  tot := time div 100000;
  hh := tot div 360000;
  mm := (tot - (hh * 360000)) div 6000;
  ss := (tot - (hh * 360000) - (mm * 6000)) div 100;
  ds := (tot - (hh * 360000) - (mm * 6000) - (ss * 100));
  result := Format('%.2d:%.2d:%.2d:%.2d', [hh, mm, ss, ds]);
end;
     
const
  Nanoseconds_per_Second = Int64(1000000000);

{!!
<FS>IEMediaFoundationTimeToSec

<FM>Declaration<FC>
function IEMediaFoundationTimeToSec(time: int64): Double;

<FM>Description<FN>
Converts Media Foundation times (100 nanosecond units) to seconds (There are 1 billion nanoseconds to a second).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>time<FN></C> <C>The Media Foundation time in 100 nanosecond units</C> </R>
</TABLE>

<FM>Example<FC>
// Display length in seconds
if IEMediaFoundationTimeToSec(ImageEnView1.IO.MediaFoundationSourceReader.Duration) < 1 then
  lblLength.Caption := '< 1 Sec.'
else
  lblLength.Caption := format('%d Sec.', [Round(IEMediaFoundationTimeToSec(ImageEnView1.IO.MediaFoundationSourceReader.Duration))]);

<FM>See Also<FN>
- <A IESecToMediaFoundationTime>
- <A IEMediaFoundationTimeToStr>
!!}
function IEMediaFoundationTimeToSec(time: int64): Double;
begin
  result := time / Nanoseconds_per_Second;
end;


{!!
<FS>IESecToMediaFoundationTime

<FM>Declaration<FC>
function IESecToMediaFoundationTime(sec: Double): int64;

<FM>Description<FN>
Converts a value in seconds to Media Foundation times (100 nanosecond units).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>time<FN></C> <C>The Media Foundation time in 100 nanosecond units</C> </R>
</TABLE>

<FM>Example<FC>
// Navigate ten seconds into the sample
ImageEnView1.IO.MediaFoundationSourceReader.SetPosition( IESecToMediaFoundationTime(10) );

<FM>See Also<FN>
- <A IEMediaFoundationTimeToSec>
!!}
function IESecToMediaFoundationTime(sec: Double): int64;
begin
  result := Round(sec * Nanoseconds_per_Second);
end;

       
function IEGetMediaTypeMajorTypeStr(mediaType: IE_IMFMediaType): WideString;
var
  majType: TGUID;
begin
  if assigned(mediaType) then
  begin
    mediaType.GetGUID(MF_MT_MAJOR_TYPE, majType);
    result := IEConvertMajorTypeToString(majType);
  end
  else
    result := '';
end;


// Helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMFDeviceList

constructor TIEMFDeviceList.Create();
begin
  inherited;
  m_devicesCount := 0;
  m_devices      := nil;
  m_populated    := false;
  m_names        := nil;
end;


destructor TIEMFDeviceList.Destroy();
begin
  Clear();
  inherited;
end;


procedure TIEMFDeviceList.Clear();
var
  i: integer;
begin
  for i := 0 to m_devicesCount - 1 do
  begin
    IE_IMFActivate(m_devices[i])._Release();
    m_devices[i] := nil;
  end;
  CoTaskMemFree(m_devices);

  m_devicesCount := 0;
  m_devices := nil;
  FreeAndNil(m_names);
end;


procedure TIEMFDeviceList.Populate();
var
  attributes: IE_IMFAttributes;
begin
  Clear();

  attributes := nil;
  MFCreateAttributes(attributes, 1);
  attributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE, MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

  m_populated := SUCCEEDED( MFEnumDeviceSources(attributes, m_devices, m_devicesCount) );
end;


// Populate() must be called before
function TIEMFDeviceList.GetCount(): integer;
begin
  result := m_devicesCount;
end;


// Populate() must be called before
function TIEMFDeviceList.GetName(index: integer): WideString;
begin
  result := IEGetStringFromAllocatedString(IE_IMFActivate(m_devices[index]), MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME);
end;


// Populate() must be called before
function TIEMFDeviceList.GetDevice(index: integer): IE_IMFActivate;
begin
  result := IE_IMFActivate(m_devices[index]); // this increments IE_IMFActivate reference count
end;


// Populate() must be called before
function TIEMFDeviceList.GetNames(): TStringList;
var
  i: integer;
begin
  if m_names = nil then
  begin
    m_names := TStringList.Create();
    for i := 0 to m_devicesCount - 1 do
      m_names.Add(GetName(i));
  end;
  result := m_names;
end;

// TIEMFDeviceList
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMFReceivedSample

constructor TIEMFReceivedSample.Create(sample: IE_IMFSample; streamIndex: DWORD; streamFlags: DWORD; timeStamp: int64; mediaType: IE_IMFMediaType);
begin
  inherited Create();
  self.Sample      := sample;
  self.StreamIndex := streamIndex;
  self.StreamFlags := streamFlags;
  self.TimeStamp   := timeStamp;
  self.MediaType   := mediaType;
  self.StreamType  := IEGetMediaTypeMajorTypeStr(mediaType);
end;


destructor TIEMFReceivedSample.Destroy();
begin
  Sample := nil;
  inherited;
end;


{!!
<FS>TIEMFReceivedSample.DecodeSample

<FM>Declaration<FC>
function DecodeSample(destBitmap: <A TIEBitmap>): boolean;

<FM>Description<FN>
Decodes a video sample, filling the specified bitmap with the converted data.

Presently the following conversions are possible:
RGB24 -> RGB24 (copy only)
RGB32 -> RGB24
YUY2  -> RGB24
I420  -> RGB24
NV12  -> RGB24

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>destBitmap<FN></C> <C>The destination bitmap for the decoded video sample</C> </R>
</TABLE>

Returns True on success. Returns False if the sample is not a video sample or if no decoder is found.

<FM>Example 1<FC>
// Handler for TImageEnView.OnMediaFoundatioNotify event
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

Applications add new decoders by using the <A IEMediaFoundationGetVideoSampleDecoders> function and implementing the <A TIEMediaFoundationVideoSampleDecoder> abstract class.

<FM>Example 2<FC>
IEMediaFoundationGetVideoSampleDecoders().Add(TIEMediaFoundationVideoSampleDecoder_YOURFORMAT.Create());
!!}
function TIEMFReceivedSample.DecodeSample(destBitmap: TIEBitmap): boolean;
var
  mediaBuffer: IE_IMFMediaBuffer;
  buffer: pbyte;
  bufferLen, maxBufferLen: DWORD;
  frameWidth, frameHeight: DWORD;
  frameStride: integer;
  frameFormat: WideString;
  i: integer;
  subType: TGUID;
begin
  result := false;

  if StreamType <> mmf_VIDEO_STREAM then
    exit;

  mediaBuffer := nil;
  if assigned(Sample) then
    Sample.ConvertToContiguousBuffer(mediaBuffer);

  if assigned(mediaBuffer) then
  begin
    MediaType.GetGUID(MF_MT_SUBTYPE, subType);
    frameFormat := IEConvertSubTypeToString(subType);
    IEGetMediaTypeFrameSize(MediaType, frameWidth, frameHeight);
    frameStride := IEGetMediaTypeFrameStride(MediaType, subType, frameWidth);

    if frameStride = 0 then
      exit;

    if not SUCCEEDED(mediaBuffer.Lock(buffer, maxBufferLen, bufferLen)) then
      exit;

    try

      for i := 0 to IEMediaFoundationGetVideoSampleDecoders().Count - 1 do
        if (IEMediaFoundationGetVideoSampleDecoders()[i] as TIEMediaFoundationVideoSampleDecoder).GetSubType() = frameFormat then
        begin
          result := (IEMediaFoundationGetVideoSampleDecoders()[i] as TIEMediaFoundationVideoSampleDecoder).Decode(frameWidth, frameHeight, frameStride, buffer, bufferLen, destBitmap);
          break;
        end;

    finally

      mediaBuffer.Unlock();

    end;
  end;
end;

// TIEMFReceivedSample
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationVideoSampleDecoder_YUY2

type TIEMediaFoundationVideoSampleDecoder_YUY2 = class(TIEMediaFoundationVideoSampleDecoder)
  public
    function GetSubType(): WideString; override;
    function Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean; override;
end;

function TIEMediaFoundationVideoSampleDecoder_YUY2.GetSubType(): WideString;
begin
  result := 'YUY2';
end;

function TIEMediaFoundationVideoSampleDecoder_YUY2.Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean;
begin
  destBitmap.Allocate(width, height, ie24RGB);
  _CopyYUY2ToBitmap(buffer, destBitmap, stride > 0);
  result := true;
end;

// TIEMediaFoundationVideoSampleDecoder_YUY2
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationVideoSampleDecoder_RGB24

type TIEMediaFoundationVideoSampleDecoder_RGB24 = class(TIEMediaFoundationVideoSampleDecoder)
  public
    function GetSubType(): WideString; override;
    function Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean; override;
end;

function TIEMediaFoundationVideoSampleDecoder_RGB24.GetSubType(): WideString;
begin
  result := 'RGB24';
end;

function TIEMediaFoundationVideoSampleDecoder_RGB24.Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean;
var
  i: integer;
begin
  destBitmap.Allocate(width, height, ie24RGB);
  if stride < 0 then
    for i := height - 1 downto 0 do
    begin
      CopyMemory(destBitmap.ScanLine[i], PRGB(buffer), width * 3);
      inc(pbyte(buffer), -stride);
    end
  else
    for i := 0 to height - 1 do
    begin
      CopyMemory(destBitmap.ScanLine[i], PRGB(buffer), width * 3);
      inc(pbyte(buffer), stride);
    end;
  result := true;
end;

// TIEMediaFoundationVideoSampleDecoder_RGB24
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationVideoSampleDecoder_RGB32

type TIEMediaFoundationVideoSampleDecoder_RGB32 = class(TIEMediaFoundationVideoSampleDecoder)
  public
    function GetSubType(): WideString; override;
    function Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean; override;
end;

function TIEMediaFoundationVideoSampleDecoder_RGB32.GetSubType(): WideString;
begin
  result := 'RGB32';
end;

function TIEMediaFoundationVideoSampleDecoder_RGB32.Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean;
var
  i: integer;
  procedure CopyRow();
  var
    j: integer;
    p_bgra: PRGBA;
    p_bgr: PRGBA;
    wm2: integer;
  begin
    p_bgra := buffer;
    p_bgr := destBitmap.ScanLine[i];
    wm2 := width - 2; // -2 = avoid to copy last pixel
    for j := 0 to wm2 do
    begin
      p_bgr^ := p_bgra^;
      inc(PRGB(p_bgr));
      inc(p_bgra);
    end;
    // last pixel
    p_bgr^.b := p_bgra^.b;
    p_bgr^.g := p_bgra^.g;
    p_bgr^.r := p_bgra^.r;
  end;
begin
  destBitmap.Allocate(width, height, ie24RGB);
  if stride < 0 then
    for i := height - 1 downto 0 do
    begin
      CopyRow();
      inc(pbyte(buffer), -stride);
    end
  else
    for i := 0 to height - 1 do
    begin
      CopyRow();
      inc(pbyte(buffer), stride);
    end;
  result := true;
end;

// TIEMediaFoundationVideoSampleDecoder_RGB32
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationVideoSampleDecoder_I420

type TIEMediaFoundationVideoSampleDecoder_I420 = class(TIEMediaFoundationVideoSampleDecoder)
  public
    function GetSubType(): WideString; override;
    function Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean; override;
end;

function TIEMediaFoundationVideoSampleDecoder_I420.GetSubType(): WideString;
begin
  result := 'I420';
end;

function TIEMediaFoundationVideoSampleDecoder_I420.Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean;
begin
  destBitmap.Allocate(width, height, ie24RGB);
  _CopyI420ToBitmap(buffer, destBitmap, stride > 0);
  result := true;
end;

// TIEMediaFoundationVideoSampleDecoder_I420
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationVideoSampleDecoder_NV12

type TIEMediaFoundationVideoSampleDecoder_NV12 = class(TIEMediaFoundationVideoSampleDecoder)
  public
    function GetSubType(): WideString; override;
    function Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean; override;
end;

function TIEMediaFoundationVideoSampleDecoder_NV12.GetSubType(): WideString;
begin
  result := 'NV12';
end;

function TIEMediaFoundationVideoSampleDecoder_NV12.Decode(width: DWORD; height: DWORD; stride: integer; buffer: pointer; bufferLen: DWORD; destBitmap: TIEBitmap): boolean;
begin
  destBitmap.Allocate(width, height, ie24RGB);
  _CopyNV12ToBitmap(buffer, destBitmap, stride > 0);
  result := true;
end;

// TIEMediaFoundationVideoSampleDecoder_NV12
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationReaderWindowNotifyReceiver

constructor TIEMediaFoundationReaderWindowNotifyReceiver.Create(notifyWindow: THandle; notifyMessage: DWORD);
begin
  inherited Create();
  m_notifyWindow  := notifyWindow;
  m_notifyMessage := notifyMessage;
end;


destructor TIEMediaFoundationReaderWindowNotifyReceiver.Destroy();
begin
  inherited;
end;


procedure TIEMediaFoundationReaderWindowNotifyReceiver.ReceiveNotify(sender: TObject; notifyType: TIEMediaFountationNotifyType; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; mediaType: IE_IMFMediaType; pEvent: IE_IMFMediaEvent);
begin
  PostMessage(m_notifyWindow, m_notifyMessage, WPARAM(notifyType), LPARAM(pointer(sender)));
end;

// TIEMediaFoundationReaderWindowNotifyReceiver
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationSourceReaderCallback

constructor TIEMediaFoundationSourceReaderCallback.Create(OnCallBack: TIEMediaFoundationSourceReaderCallbackEvent);
begin
  m_onCallBack := OnCallBack;
end;


function TIEMediaFoundationSourceReaderCallback.OnReadSample(hrStatus: HRESULT; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample): HRESULT;
begin
  result := m_onCallBack(mfrceONREADSAMPLE, hrStatus, dwStreamIndex, dwStreamFlags, llTimestamp, pSample, nil);
end;


function TIEMediaFoundationSourceReaderCallback.OnFlush(dwStreamIndex: DWORD): HRESULT;
begin
  result := m_onCallBack(mfrceONFLUSH, S_OK, dwStreamIndex, 0, 0, nil, nil);
end;


function TIEMediaFoundationSourceReaderCallback.OnEvent(dwStreamIndex: DWORD; pEvent: IE_IMFMediaEvent): HRESULT;
begin
  result := m_onCallBack(mfrceONEVENT, S_OK, dwStreamIndex, 0, 0, nil, pEvent);
end;


// TIEMediaFoundationSourceReaderCallback
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationSourceReader

constructor TIEMediaFoundationSourceReader.Create();
begin
  inherited Create();

  m_source            := nil;
  m_sourceReader      := nil;
  m_selectedActivate  := nil;
  m_streams           := nil;
  m_selectedMediaType := nil;
  m_duration          := 0;
  m_videoInputs       := nil;
  m_notifyReceivers   := nil;
  m_capturing         := false;
  m_receivedSamples   := nil;
  m_delayFramePost    := false;
  m_frameRequested    := 0;
  m_videoProcessor    := nil;
  m_samplesBufferSize := 30;

  m_IsAvailable := IEMFStartup();

  if m_IsAvailable then
  begin
    InitializeCriticalSection(m_lock);

    m_streams           := TObjectList.Create();
    m_selectedMediaType := TObjectList.Create();
    m_videoInputs       := TIEMFDeviceList.Create();
    m_notifyReceivers   := TInterfaceList.Create();
    m_receivedSamples   := TObjectList.Create();
  end;
end;


destructor TIEMediaFoundationSourceReader.Destroy();
begin
  if m_IsAvailable then
  begin
    PauseCapture();
    Flush();

    FreeAndNil(m_selectedMediaType);
    FreeAndNil(m_streams);

    FreeAndNil(m_notifyReceivers);
    m_selectedActivate := nil;
    m_source           := nil;
    m_sourceReader     := nil;

    FreeAndNil(m_videoInputs);

    ClearReceivedSamples();
    FreeAndNil(m_receivedSamples);

    FreeAndNil(m_videoProcessor);

    IEMFShutdown();

    DeleteCriticalSection(m_lock);
  end;
  inherited;
end;


procedure TIEMediaFoundationSourceReader.Lock();
begin
  EnterCriticalSection(m_lock);
end;


procedure TIEMediaFoundationSourceReader.Unlock();
begin
  LeaveCriticalSection(m_lock);
end;


function TIEMediaFoundationSourceReader.SetInput(activate: IE_IMFActivate): boolean;
var
  attributes: IE_IMFAttributes;
begin
  result := false;

  m_sourceReader := nil;
  m_source := nil;
  m_streams.Clear();

  m_selectedActivate := activate;

  // create media source
  if not SUCCEEDED(activate.ActivateObject(IE_IMFMediaSource_GUID, m_source)) then
    exit;
  activate.DetachObject();  // this allows to reuse the activate object

  // setup source reader attributes
  attributes := nil;
  MFCreateAttributes(attributes, 3);
  // support callback?
  if IsAsyncMode() then
    attributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK, TIEMediaFoundationSourceReaderCallback.Create(SourceReaderCallback));
  // this is needed in order to convert among mediatypes automatically
  attributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING, DWORD(true));
  // enable hardware transforms
  attributes.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS, DWORD(true));

  // create source reader
  result := SUCCEEDED(MFCreateSourceReaderFromMediaSource(m_source, attributes, m_sourceReader));

  if result then
  begin
    m_duration := GetDuration();
    PopulateStreams();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.SetVideoInput

<FM>Declaration<FC>
function SetVideoInput(index: Integer): boolean;
function SetVideoInput(name: WideString): boolean;

<FM>Description<FN>
Select one of the <A TIEMediaFoundationSourceReader.VideoInputs> devices as the video source.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>index<FN></C> <C>Index of the video input device. Ranges from 0 up to <A TIEMediaFoundationSourceReader.VideoInputs>.Count - 1</C> </R>
<R> <C><FC>name<FN></C> <C>Name of the video input device. Must be one of <A TIEMediaFoundationSourceReader.VideoInputs> values</C> </R>
</TABLE>

<FM>Example<FC>
// Select first video input (first webcam)
ImageEnView1.IO.MediaFoundationSourceReader.SetVideoInput(0);

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.VideoInputs>
- <A TIEMediaFoundationSourceReader.SetFileInput>
- <A TIEMediaFoundationSourceReader.SetURLInput>
!!}
function TIEMediaFoundationSourceReader.SetVideoInput(index: integer): boolean;
begin
  CheckVideoInputIndex(index);
  m_delayFramePost := false;
  result := SetInput(m_videoInputs.GetDevice(index));
end;


function TIEMediaFoundationSourceReader.SetVideoInput(name: WideString): boolean;
begin
  result := SetVideoInput(m_videoInputs.getNames().IndexOf(name));
end;


{!!
<FS>TIEMediaFoundationSourceReader.SetFileInput

<FM>Declaration<FC>
function SetFileInput(filename: WideString): boolean;

<FM>Description<FN>
Specifies a file as the video/audio source.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>filename<FN></C> <C>Full path of the input file</C> </R>
</TABLE>

<FM>Example<FC>
// Set 'input.mp4' as the video/audio source
ImageEnView1.IO.MediaFoundationSourceReader.SetFileInput('D:\input.mp4');

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.SetVideoInput>
- <A TIEMediaFoundationSourceReader.SetURLInput>
!!}
function TIEMediaFoundationSourceReader.SetFileInput(filename: WideString): boolean;
var
  byteStream: IE_IMFByteStream;
  attributes: IE_IMFAttributes;
begin
  result := false;

  m_sourceReader := nil;
  m_source := nil;
  m_streams.Clear();

  m_selectedActivate := nil;
  m_delayFramePost   := true;

  byteStream := nil;
  if not SUCCEEDED(MFCreateFile(MF_ACCESSMODE_READ, MF_OPENMODE_FAIL_IF_NOT_EXIST, MF_FILEFLAGS_NONE, @filename[1], byteStream)) then
    exit;

  // setup source reader attributes
  attributes := nil;
  MFCreateAttributes(attributes, 3);
  // support callback?
  if IsAsyncMode() then
    attributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK, TIEMediaFoundationSourceReaderCallback.Create(SourceReaderCallback));
  // this is needed in order to convert among mediatypes automatically
  attributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING, DWORD(true));
  // enable hardware transforms
  attributes.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS, DWORD(true));

  // create source reader
  result := SUCCEEDED(MFCreateSourceReaderFromByteStream(byteStream, attributes, m_sourceReader));

  if result then
  begin
    m_duration := GetDuration();
    PopulateStreams();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.SetURLInput

<FM>Declaration<FC>
function SetURLInput(URL: WideString): boolean;

<FM>Description<FN>
Specifies a URL as the video/audio source.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>URL<FN></C> <C>Web link to a video/audio source</C> </R>
</TABLE>

<FM>Example<FC>
ImageEnView1.IO.MediaFoundationSourceReader.SetURLInput('http://avideos.5min.com//29/5181029/518102846_4.mp4');

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.SetVideoInput>
- <A TIEMediaFoundationSourceReader.SetFileInput>
!!}
function TIEMediaFoundationSourceReader.SetURLInput(URL: WideString): boolean;
var
  attributes: IE_IMFAttributes;
begin
  m_sourceReader := nil;
  m_source := nil;
  m_streams.Clear();

  m_selectedActivate := nil;
  m_delayFramePost   := true;

  // setup source reader attributes
  attributes := nil;
  MFCreateAttributes(attributes, 3);
  // support callback?
  if IsAsyncMode() then
    attributes.SetUnknown(MF_SOURCE_READER_ASYNC_CALLBACK, TIEMediaFoundationSourceReaderCallback.Create(SourceReaderCallback));
  // this is needed in order to convert among mediatypes automatically
  attributes.SetUINT32(MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING, DWORD(true));
  // enable hardware transforms
  attributes.SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS, DWORD(true));

  // create source reader
  result := SUCCEEDED(MFCreateSourceReaderFromURL(@URL[1], attributes, m_sourceReader));

  if result then
  begin
    m_duration := GetDuration();
    PopulateStreams();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.Duration

<FM>Declaration<FC>
property Duration: int64;

<FM>Description<FN>
Returns the duration of a source media file, in 100 nanosecond units (There are 1 billion nanoseconds to a second).

<FM>Example 1<FC>
// Get the source duration as a 'HH:MM:SS:DD' string
duration := IEMediaFoundationTimeToStr(ImageEnView1.IO.MediaFoundationSourceReader.Duration);

<FM>Example 2<FC>
// Display length in seconds
if IEMediaFoundationTimeToSec(ImageEnView1.IO.MediaFoundationSourceReader.Duration) < 1 then
  lblLength.Caption := '< 1 Sec.'
else
  lblLength.Caption := format('%d Sec.', [Round(IEMediaFoundationTimeToSec(ImageEnView1.IO.MediaFoundationSourceReader.Duration))]);

<FM>See Also<FN>
- <A IEMediaFoundationTimeToStr>
- <A IEMediaFoundationTimeToSec>
!!}
function TIEMediaFoundationSourceReader.GetDuration(): int64;
var
  prop: PROPVARIANT;
begin
  result := 0;
  if assigned(m_sourceReader) and SUCCEEDED(m_sourceReader.GetPresentationAttribute(MF_SOURCE_READER_MEDIASOURCE, MF_PD_DURATION, prop)) then
    result := prop.uhVal.QuadPart;
end;


procedure TIEMediaFoundationSourceReader.PopulateStreams();
var
  streamIndex: integer;
  mediaTypeIndex: integer;
  mediaType: IE_IMFMediaType;
  mediaTypes: TObjectList;
begin
  m_streams.Clear();
  streamIndex := 0;
  mediaTypeIndex := 0;
  while SUCCEEDED( m_sourceReader.GetNativeMediaType(streamIndex, mediaTypeIndex, mediaType) ) do
  begin
    mediaTypes := TObjectList.Create();
    m_streams.Add(mediaTypes);  // add new stream
    mediaTypes.Add(IECreateDictionaryFromMediaType(mediaType));  // add first mediatype to the stream
    inc(mediaTypeIndex);
    while SUCCEEDED( m_sourceReader.GetNativeMediaType(streamIndex, mediaTypeIndex, mediaType) ) do
    begin
      mediaTypes.Add(IECreateDictionaryFromMediaType(mediaType));  // add other mediatypes to the stream
      inc(mediaTypeIndex);
    end;
    inc(streamIndex);
    mediaTypeIndex := 0;
  end;
end;


// assumes m_streams is populated (to get number of streams)
procedure TIEMediaFoundationSourceReader.PopulateSelectedMediaType();
var
  i: integer;
  mediaType: IE_IMFMediaType;
begin
  m_selectedMediaType.Clear();
  for i := 0 to StreamCount - 1 do
  begin
    m_sourceReader.GetCurrentMediaType(i, mediaType);
    m_selectedMediaType.Add(IECreateDictionaryFromMediaType(mediaType));
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.GetCurrentMediaType

<FM>Declaration<FC>
function GetCurrentMediaType(streamIndex: integer): <A TIEDictionary>;
function GetCurrentMediaType(streamType: WideString): <A TIEDictionary>;

<FM>Description<FN>
Returns the actual media type for the specified stream.
Applications should call GetCurrentMediaType to check the actual selected media type after a call to SetMediaTypeXXX or SelectMediaType.
See <A TIEMediaFoundationSourceReader.GetMediaType> for the list of keys in the resulting dictionary.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Only the first stream of this type will be considered. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
</TABLE>

<FM>Example<FC>
// Get the actual video format (ie MJPEG may be converted to YUY2)
with ImageEnView1.IO.MediaFoundationSourceReader.GetCurrentMediaType(mmf_VIDEO_STREAM) do
begin
  Label4.Caption := Format('Actual video format: %d x %d   %.1f fps   %s',
           [GetInteger(IEFRAMEWIDTH_DICT_KEY),    // width
            GetInteger(IEFRAMEHEIGHT_DICT_KEY),   // height
            GetDouble(IEFRAMERATE_DICT_KEY),      // frame rate
            GetString(IESUBTYPE_DICT_KEY)         // subtype = color space
            ]);
end;

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.GetMediaType>
- <A TIEMediaFoundationSourceReader.GetMediaTypesCount>
- <A TIEMediaFoundationSourceReader.SelectMediaType>
- <A TIEMediaFoundationSourceReader.SetMediaTypeAudio>
- <A TIEMediaFoundationSourceReader.SetMediaTypeCustom>
- <A TIEMediaFoundationSourceReader.SetMediaTypeVideo>
!!}
function TIEMediaFoundationSourceReader.GetCurrentMediaType(streamIndex: integer): TIEDictionary;
begin
  result := nil;
  if streamIndex < m_selectedMediaType.Count then
    result := m_selectedMediaType[streamIndex] as TIEDictionary;
end;


function TIEMediaFoundationSourceReader.GetCurrentMediaType(streamType: WideString): TIEDictionary;
var
  streamIndex: integer;
begin
  result := nil;
  streamIndex := IndexOfFirstStream(streamType);
  if streamIndex > -1 then
    result := GetCurrentMediaType(streamIndex);
end;


function TIEMediaFoundationSourceReader.GetCurrentMediaTypeIntf(streamIndex: integer): IE_IMFMediaType;
begin
  result := nil;
  m_sourceReader.GetCurrentMediaType(streamIndex, result);
end;


function TIEMediaFoundationSourceReader.GetCurrentMediaTypeIntf(streamType: WideString): IE_IMFMediaType;
var
  streamIndex: integer;
begin
  result := nil;
  streamIndex := IndexOfFirstStream(streamType);
  if streamIndex > -1 then
    m_sourceReader.GetCurrentMediaType(streamIndex, result);
end;


{!!
<FS>TIEMediaFoundationSourceReader.StreamCount

<FM>Declaration<FC>
property StreamCount: integer;

<FM>Description<FN>
Returns the number of streams for the selected source.

<FM>Example<FC>
var
  i: integer;
  mediaType: TIEDictionary;
begin
  // Fill streams listbox
  ListBoxStreams.Clear();
  with ImageEnView1.IO.MediaFoundationSourceReader do
  begin
    for i := 0 to StreamCount - 1 do
    begin
      mediaType := GetMediaType(i, 0);
      ListBoxStreams.Items.Add(Format('%s', [mediaType.GetString(IEMAJORTYPE_DICT_KEY)]));
    end;
  end;
end;

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.GetStreamType>
- <A TIEMediaFoundationSourceReader.SetSelectedStreams>
!!}
function TIEMediaFoundationSourceReader.GetStreamCount(): integer;
begin
  result := m_streams.Count;
end;


{!!
<FS>TIEMediaFoundationSourceReader.GetStreamType

<FM>Declaration<FC>
function GetStreamType(streamIndex: integer): WideString;

<FM>Description<FN>
Returns a string representing the type of the specified stream.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
</TABLE>

Stream type can be one of the following values:
<TABLE>
<R> <H>Return value</H> <H>Description</H>  <H>Const</H> </R>
<R> <C><FC>'Audio'<FN></C> <C>Audio stream</C> <FC>mmf_AUDIO_STREAM<FN> </R>
<R> <C><FC>'Video'<FN></C> <C>Video stream</C> <FC>mmf_VIDEO_STREAM<FN> </R>
<R> <C><FC>'Protected'<FN></C> <C>Protected media</C> <FC>mmf_PROTECTED_STREAM<FN> </R>
<R> <C><FC>'SAMI'<FN></C> <C>Synchronized Accessible Media Interchange (SAMI) captions</C> <FC>mmf_SAMI_STREAM<FN> </R>
<R> <C><FC>'Script'<FN></C> <C>Script stream</C> <FC>mmf_SCRIPT_STREAM<FN> </R>
<R> <C><FC>'Image'<FN></C> <C>Still image stream</C> <FC>mmf_IMAGE_STREAM<FN> </R>
<R> <C><FC>'HTML'<FN></C> <C>HTML stream</C> <FC>mmf_HTML_STREAM<FN> </R>
<R> <C><FC>'Binary'<FN></C> <C>Binary stream</C> <FC>mmf_BINARY_STREAM<FN> </R>
<R> <C><FC>'FileTransfer'<FN></C> <C>A stream that contains data files</C> <FC><FN> </R>
<R> <C><FC>'Unknown'<FN></C> <C>None of the above</C> <FC>mmf_FILETRANSFER_STREAM<FN> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.StreamCount>
!!}
function TIEMediaFoundationSourceReader.GetStreamType(streamIndex: integer): WideString;
begin
  result := GetMediaType(streamIndex, 0).GetString(IEMAJORTYPE_DICT_KEY);
end;


{!!
<FS>TIEMediaFoundationSourceReader.IndexOfFirstStream

<FM>Declaration<FC>
function IndexOfFirstStream(streamType: WideString): integer;

<FM>Description<FN>
Returns the index of first stream of the specified type, or -1 if the stream type is not found.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
</TABLE>

<FM>Example<FC>
// Search for audio stream and enables audio renderer for that stream
audioStreamIndex := ImageEnView1.IO.MediaFoundationSourceReader.IndexOfFirstStream(mmf_AUDIO_STREAM);
ImageEnView1.IO.MediaFoundationSourceReader.SetSelectedStreams(audioStreamIndex, true);
ImageEnView1.IO.MediaFoundationSourceReader.SetMediaTypeAudio(audioStreamIndex, 'PCM');
ImageEnView1.IO.MediaFoundationSourceReader.PushNotifyReceiver( TIEMediaFoundationAudioRenderer.Create(audioStreamIndex) );

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.GetStreamType>
- <A TIEMediaFoundationSourceReader.StreamCount>
!!}
// ret -1 = not found
function TIEMediaFoundationSourceReader.IndexOfFirstStream(streamType: WideString): integer;
begin
  for result := 0 to StreamCount - 1 do
    if SameText(GetStreamType(result), streamType) then
      exit; // found
  result := -1; // not found
end;


{!!
<FS>TIEMediaFoundationSourceReader.GetMediaTypesCount

<FM>Declaration<FC>
function GetMediaTypesCount(streamIndex: integer): integer;
function GetMediaTypesCount(streamType: WideString): integer;

<FM>Description<FN>
Returns the number of media types for the specified stream.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Only the first stream of this type will be considered. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.StreamCount>
- <A TIEMediaFoundationSourceReader.GetMediaType>
!!}
function TIEMediaFoundationSourceReader.GetMediaTypesCount(streamIndex: integer): integer;
begin
  result := (m_streams[streamIndex] as TObjectList).Count;
end;


// get "first" stream of specified type
function TIEMediaFoundationSourceReader.GetMediaTypesCount(streamType: WideString): integer;
var
  streamIndex: integer;
begin
  result := 0;
  streamIndex := IndexOfFirstStream(streamType);
  if streamIndex > -1 then
    result := GetMediaTypesCount(streamIndex);
end;


{!!
<FS>TIEMediaFoundationSourceReader.GetMediaType

<FM>Declaration<FC>
function GetMediaType(streamIndex: integer; mediaTypeIndex: integer): <A TIEDictionary>;
function GetMediaType(streamType: WideString; mediaTypeIndex: integer): <A TIEDictionary>;

<FM>Description<FN>
Returns the media type for the specified stream and media type index.
Each source stream can support several media types (ie. frame size, frame format, etc..) so GetMediaType provides details about them.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Only the first stream of this type will be considered. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
<R> <C><FC>mediaTypeIndex<FN></C> <C>Index of the media type, in the range of 0 to <A TIEMediaFoundationSourceReader.GetMediaTypesCount> - 1</C> </R>
</TABLE>

The resulting object is a dictionary which contains following media type attributes:
<TABLE>
<R> <H>Dictionary key</H> <H>Type</H> <H>Description</H> </R>
<R> <C><FC>IEMAJORTYPE_DICT_KEY<FN></C> <C>string</C> <C>Contains major stream type. See <A TIEMediaFoundationSourceReader.GetStreamType> for a list of stream types. The Media Foundation Type is MF_MT_MAJOR_TYPE</C> </R>
<R> <C><FC>IESUBTYPE_DICT_KEY<FN></C> <C>string</C> <C>Contains the media sub type. See below for a list of common values. The Media Foundation Type is MF_MT_SUBTYPE</C> </R>
<R> <C><FC>IECOMPRESSED_DICT_KEY<FN></C> <C>boolean</C> <C>If this attribute is True, the media type is a compressed format. The Media Foundation Type is MF_MT_COMPRESSED</C> </R>
<R> <C><FC>IEAVGBITRATE_DICT_KEY<FN></C> <C>integer</C> <C>Approximate data rate of the video stream, in bits per second. The Media Foundation Type is MF_MT_AVG_BITRATE</C> </R>
<R> <C><FC>IEDEFAULTSTRIDE_DICT_KEY<FN></C> <C>integer</C> <C>The number of bytes needed to go from one row of pixels to the next. The Media Foundation Type is MF_MT_DEFAULT_STRIDE</C> </R>
<R> <C><FC>IEFRAMERATE_DICT_KEY<FN></C> <C>double</C> <C>Frame rate of a video media type, in frames per second. The Media Foundation Type is MF_MT_FRAME_RATE</C> </R>
<R> <C><FC>IEFRAMERATEMAX_DICT_KEY<FN></C> <C>double</C> <C>The maximum frame rate that is supported by a video capture device, in frames per second. The Media Foundation Type is MF_MT_FRAME_RATE_RANGE_MAX</C> </R>
<R> <C><FC>IEFRAMERATEMIN_DICT_KEY<FN></C> <C>double</C> <C>The minimum frame rate that is supported by a video capture device, in frames per second. The Media Foundation Type is MF_MT_FRAME_RATE_RANGE_MIN</C> </R>
<R> <C><FC>IEFRAMEWIDTH_DICT_KEY<FN></C> <C>integer</C> <C>Width of a video frame, in pixels. The Media Foundation Type is MF_MT_FRAME_SIZE (high dword)</C> </R>
<R> <C><FC>IEFRAMEHEIGHT_DICT_KEY<FN></C> <C>integer</C> <C>Height of a video frame, in pixels. The Media Foundation Type is MF_MT_FRAME_SIZE (low dword)</C> </R>
<R> <C><FC>IEINTERLACEMODE_DICT_KEY<FN></C> <C>string</C> <C>Describes how the frames in a video media type are interlaced. The Media Foundation Type is MF_MT_INTERLACE_MODE</C> </R>
<R> <C><FC>IEVIDEOLIGHTING_DICT_KEY<FN></C> <C>string</C> <C>Specifies the optimal lighting conditions for a video media type. Allowed values are: 'Bright', 'Office', 'Dim' and 'Dark'. The Media Foundation Type is MF_MT_VIDEO_LIGHTING</C> </R>
<R> <C><FC>IEAUDIOBITSPERSAMPLE_DICT_KEY<FN></C> <C>integer</C> <C>Number of bits per audio sample in an audio media type. The Media Foundation Type is MF_MT_AUDIO_BITS_PER_SAMPLE</C> </R>
<R> <C><FC>IEAUDIOFLOATSAMPLESPERSECOND_DICT_KEY<FN></C> <C>double</C> <C>Number of audio samples per second. The Media Foundation Type is MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND</C> </R>
<R> <C><FC>IEAUDIONUMCHANNELS_DICT_KEY<FN></C> <C>integer</C> <C>Number of audio channels. The Media Foundation Type is MF_MT_AUDIO_NUM_CHANNELS</C> </R>
<R> <C><FC>IEAUDIOSAMPLESPERSECOND_DICT_KEY<FN></C> <C>integer</C> <C>Number of audio samples per second. The Media Foundation Type is MF_MT_AUDIO_SAMPLES_PER_SECOND</C> </R>
<R> <C><FC>IEAUDIOBLOCKALIGNMENT_DICT_KEY<FN></C> <C>integer</C> <C>Block alignment, in bytes. For PCM audio formats, the block alignment is equal to the number of audio channels multiplied by the number of bytes per audio sample. The Media Foundation Type is MF_MT_AUDIO_BLOCK_ALIGNMENT</C> </R>
<R> <C><FC>IEALLSAMPLESINDEPENDENT_DICT_KEY<FN></C> <C>boolean</C> <C>Specifies for a media type whether each sample is independent of the other samples. The Media Foundation Type is MF_MT_ALL_SAMPLES_INDEPENDENT</C> </R>
<R> <C><FC>IEAUDIOAVGBYTESPERSECOND_DICT_KEY<FN></C> <C>integer</C> <C>Average number of bytes per second. The Media Foundation Type is MF_MT_AUDIO_AVG_BYTES_PER_SECOND</C> </R>
</TABLE>

The Video media sub types (IESUBTYPE_DICT_KEY) are: 'RGB8', 'RGB555', 'RGB565', 'RGB24', 'RGB32', 'ARGB32', 'AI44', 'AYUV', 'YUY2', 'YVYU', 'YVU9', 'UYVY', 'NV11', 'NV12', 'YV12', 'I420', 'IYUV', 'Y210', 'Y216',
'Y410', 'Y416', 'Y41P', 'Y41T', 'Y42T', 'P210', 'P216', 'P010', 'P016', 'v210', 'v216', 'v410', 'MP43', 'MP4S', 'M4S2', 'MP4V', 'WMV1', 'WMV2', 'WMV3', 'WVC1', 'MSS1', 'MSS2',
'MPG1', 'dvsl', 'dvsd', 'dvhd', 'dv25', 'dv50', 'dvh1', 'dvc ', 'H264', 'MJPG'.

The Audio media sub types (IESUBTYPE_DICT_KEY) are: 'PCM', 'Float', 'DTS', 'Dolby_AC3_SPDIF', 'DRM', 'WMAudioV8', 'WMAudioV9', 'WMAudio_Lossless', 'WMASPDIF', 'MSP1', 'MP3', 'MPEG', 'AAC', 'ADTS'.

The interlace modes (IEINTERLACEMODE_DICT_KEY) are: 'Progressive', 'FieldInterleavedUpperFirst', 'FieldInterleavedLowerFirst', 'FieldSingleUpper', 'FieldSingleLower', 'MixedInterlaceOrProgressive'.

<FM>Example<FC>
// Fill supported formats listbox
ListBox1.Clear();
with ImageEnView1.IO.MediaFoundationSourceReader do
begin
  SetVideoInput(ComboBox1.ItemIndex);
  for i := 0 to GetMediaTypesCount(mmf_VIDEO_STREAM) - 1 do
  begin
    mediaType := GetMediaType(mmf_VIDEO_STREAM, i);
    ListBox1.Items.Add(Format('%d x %d   %.1f fps (min fps: %.1f max fps: %.1f)   %s',
          [mediaType.GetInteger(IEFRAMEWIDTH_DICT_KEY),    // width
           mediaType.GetInteger(IEFRAMEHEIGHT_DICT_KEY),   // height
           mediaType.GetDouble(IEFRAMERATE_DICT_KEY),      // default frame rate
           mediaType.GetDouble(IEFRAMERATEMIN_DICT_KEY),   // minimum frame rate
           mediaType.GetDouble(IEFRAMERATEMAX_DICT_KEY),   // maximum frame rate
           mediaType.GetString(IESUBTYPE_DICT_KEY)         // subtype = color space
           ]));
  end;
end;

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.StreamCount>
- <A TIEMediaFoundationSourceReader.GetMediaTypesCount>
!!}
function TIEMediaFoundationSourceReader.GetMediaType(streamIndex: integer; mediaTypeIndex: integer): TIEDictionary;
begin
  result := (m_streams[streamIndex] as TObjectList)[mediaTypeIndex] as TIEDictionary;
end;


// get mediatype of "first" stream of specified type
function TIEMediaFoundationSourceReader.GetMediaType(streamType: WideString; mediaTypeIndex: integer): TIEDictionary;
var
  streamIndex: integer;
begin
  result := nil;
  streamIndex := IndexOfFirstStream(streamType);
  if streamIndex > -1 then
    result := GetMediaType(streamIndex, mediaTypeIndex);
end;


{!!
<FS>TIEMediaFoundationSourceReader.SetSelectedStreams

<FM>Declaration<FC>
procedure SetSelectedStreams(streamIndex: integer; selected: boolean);
procedure SetSelectedStreams(streamType: WideString; selected: boolean);

<FM>Description<FN>
Specifies which streams are enabled. An enabled stream sends samples to the receiver (hence to the <A TImageEnView.OnMediaFoundationNotify> event).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Only the first stream of this type will be considered. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
<R> <C><FC>selected<FN></C> <C>Must be True in order to select the stream</C> </R>
</TABLE>

<FM>Example<FC>
// Enable both audio and video streams (first streams of these types)
ImageEnView1.IO.MediaFoundationSourceReader.SetSelectedStreams(mmf_AUDIO_STREAM, true);
ImageEnView1.IO.MediaFoundationSourceReader.SetSelectedStreams(mmf_VIDEO_STREAM, true);

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.GetStreamType>
- <A TIEMediaFoundationSourceReader.StreamCount>
!!}
procedure TIEMediaFoundationSourceReader.SetSelectedStreams(streamIndex: integer; selected: boolean);
begin
  m_sourceReader.SetStreamSelection(streamIndex, selected);
end;


procedure TIEMediaFoundationSourceReader.SetSelectedStreams(streamType: WideString; selected: boolean);
var
  streamIndex: integer;
begin
  streamIndex := IndexOfFirstStream(streamType);
  if streamIndex > -1 then
    SetSelectedStreams(streamIndex, selected);
end;


{!!
<FS>TIEMediaFoundationSourceReader.SelectMediaType

<FM>Declaration<FC>
function SelectMediaType(streamIndex: integer; mediaTypeIndex: integer): boolean;
function SelectMediaType(streamType: WideString; mediaTypeIndex: integer): boolean;

<FM>Description<FN>
Select one of the native media types of the specified stream.
If the media type is not natively supported by ImageEn a compatible one is automatically selected (e.g. 'MJPG->YUY2').
Returns False if the media type is not accepted (or a suitable conversion is not available).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Only the first stream of this type will be considered. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
<R> <C><FC>mediaTypeIndex<FN></C> <C>Index of native media type to select</C> </R>
</TABLE>

<FM>Example<FC>
ImageEnView1.IO.MediaFoundationSourceReader.SelectMediaType(mmf_VIDEO_STREAM, ListBox1.ItemIndex);

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.GetMediaType>
- <A TIEMediaFoundationSourceReader.GetMediaTypesCount>
- <A TIEMediaFoundationSourceReader.SetMediaTypeAudio>
- <A TIEMediaFoundationSourceReader.SetMediaTypeCustom>
- <A TIEMediaFoundationSourceReader.SetMediaTypeVideo>
- <A TIEMediaFoundationSourceReader.GetCurrentMediaType>
!!}
function TIEMediaFoundationSourceReader.SelectMediaType(streamIndex: integer; mediaTypeIndex: integer): boolean;
var
  mediaType: IE_IMFMediaType;
  mediaTypeDict: TIEDictionary;
  subTypeStr: WideString;
  i: integer;
begin
  result := false;
  Lock();
  mediaTypeDict := nil;
  try
    mediaType := nil;
    m_sourceReader.GetNativeMediaType(streamIndex, mediaTypeIndex, mediaType);
    mediaTypeDict := IECreateDictionaryFromMediaType(mediaType);
    if mediaTypeDict.GetString(IEMAJORTYPE_DICT_KEY) = mmf_VIDEO_STREAM then
    begin
      // check direct match with supported subtypes
      for i := 0 to IEMediaFoundationGetVideoSampleDecoders().Count - 1 do
      begin
        if (IEMediaFoundationGetVideoSampleDecoders()[i] as TIEMediaFoundationVideoSampleDecoder).GetSubType() = mediaTypeDict.GetString(IESUBTYPE_DICT_KEY) then
        begin
          // can use directly this media type
          result := SUCCEEDED(m_sourceReader.SetCurrentMediaType(streamIndex, nil, mediaType));
          break;
        end;
      end;
      if not result then
      begin
        // try conversion to supported subtypes
        for i := 0 to IEMediaFoundationGetVideoSampleDecoders().Count - 1 do
        begin
          subTypeStr := (IEMediaFoundationGetVideoSampleDecoders()[i] as TIEMediaFoundationVideoSampleDecoder).GetSubType();
          mediaType.SetGUID(MF_MT_SUBTYPE, IEConvertStringToSubType(subTypeStr));
          result := SUCCEEDED(m_sourceReader.SetCurrentMediaType(streamIndex, nil, mediaType));
          if result then
            break;  // subtype conversion accepted!
        end;
      end;
    end
    else
      result := SUCCEEDED(m_sourceReader.SetCurrentMediaType(streamIndex, nil, mediaType));
    PopulateSelectedMediaType();
  finally
    mediaTypeDict.Free();
    Unlock();
  end;
end;


function TIEMediaFoundationSourceReader.SelectMediaType(streamType: WideString; mediaTypeIndex: integer): boolean;
var
  streamIndex: integer;
begin
  result := false;
  streamIndex := IndexOfFirstStream(streamType);
  if streamIndex > -1 then
    result := SelectMediaType(streamIndex, mediaTypeIndex);
end;


{!!
<FS>TIEMediaFoundationSourceReader.SetMediaTypeCustom

<FM>Declaration<FC>
function SetMediaTypeCustom(streamIndex: integer; jsonDescription: WideString): boolean;
function SetMediaTypeCustom(streamType: WideString; jsonDescription: WideString): boolean;

<FM>Description<FN>
Creates and selects a new audio/video media type. Media Foundation will provide a decoder to convert from native media type.
Returns False if the media type is not accepted (or a suitable conversion is not available).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Only the first stream of this type will be considered. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
<R> <C><FC>jsonDescription<FN></C> <C>JSON description of the media type</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.GetMediaType>
- <A TIEMediaFoundationSourceReader.GetMediaTypesCount>
- <A TIEMediaFoundationSourceReader.SetMediaTypeVideo>
- <A TIEMediaFoundationSourceReader.SetMediaTypeAudio>
- <A TIEMediaFoundationSourceReader.SelectMediaType>
- <A TIEMediaFoundationSourceReader.GetCurrentMediaType>
!!}
function TIEMediaFoundationSourceReader.SetMediaTypeCustom(streamIndex: integer; jsonDescription: WideString): boolean;
var
  newMediaTypeDict: TIEDictionary;
begin
  newMediaTypeDict := TIEDictionary.Create();
  Lock();
  try
    newMediaTypeDict.Parse(jsonDescription);

    // copy majortype from first native mediatype (maybe overwrite what specified in jsonDescription!)
    newMediaTypeDict.Insert(IEMAJORTYPE_DICT_KEY, GetMediaType(streamIndex, 0).GetString(IEMAJORTYPE_DICT_KEY));

    // creates actual media type from dictionary and set it as current mediatype for specified stream
    result := SUCCEEDED( m_sourceReader.SetCurrentMediaType(streamIndex, nil, IECreateMediaTypeFromDictionary(newMediaTypeDict)) );

    PopulateSelectedMediaType();

  finally
    Unlock();
    newMediaTypeDict.Free();
  end;
end;


// set mediatype of "first" stream of specified type
function TIEMediaFoundationSourceReader.SetMediaTypeCustom(streamType: WideString; jsonDescription: WideString): boolean;
var
  streamIndex: integer;
begin
  result := false;
  streamIndex := IndexOfFirstStream(streamType);
  if streamIndex > -1 then
    result := SetMediaTypeCustom(streamIndex, jsonDescription);
end;


{!!
<FS>TIEMediaFoundationSourceReader.SetMediaTypeVideo

<FM>Declaration<FC>
function SetMediaTypeVideo(streamIndex: integer; subTypeStr: WideString; frameWidth: integer; frameHeight: integer; frameRate: double; videoLighting: WideString): boolean;
function SetMediaTypeVideo(subTypeStr: WideString; frameWidth: integer; frameHeight: integer; frameRate: double; videoLighting: WideString): boolean;

<FM>Description<FN>
Creates and selects a new video media type. Media Foundation will provide a decoder to convert from native media type.
Returns False if the media type is not accepted (or a suitable conversion is not available).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Only the first stream of this type will be considered. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
<R> <C><FC>subTypeStr<FN></C> <C>Specifies the video subtype</C> </R>
<R> <C><FC>frameWidth<FN></C> <C>Width of a video frame, in pixels</C> </R>
<R> <C><FC>frameHeight<FN></C> <C>Height of a video frame, in pixels</C> </R>
<R> <C><FC>frameRate<FN></C> <C>Frame rate of a video media type, in frames per second</C> </R>
<R> <C><FC>videoLighting<FN></C> <C>Specifies the optimal lighting conditions for a video media type. Allowed values are: 'Bright', 'Office', 'Dim' and 'Dark'</C> </R>
</TABLE>

The Video media sub types (IESUBTYPE_DICT_KEY) are: 'RGB8', 'RGB555', 'RGB565', 'RGB24', 'RGB32', 'ARGB32', 'AI44', 'AYUV', 'YUY2', 'YVYU', 'YVU9', 'UYVY', 'NV11', 'NV12', 'YV12', 'I420', 'IYUV', 'Y210', 'Y216',
'Y410', 'Y416', 'Y41P', 'Y41T', 'Y42T', 'P210', 'P216', 'P010', 'P016', 'v210', 'v216', 'v410', 'MP43', 'MP4S', 'M4S2', 'MP4V', 'WMV1', 'WMV2', 'WMV3', 'WVC1', 'MSS1', 'MSS2',
'MPG1', 'dvsl', 'dvsd', 'dvhd', 'dv25', 'dv50', 'dvh1', 'dvc ', 'H264', 'MJPG'.

<FM>Example<FC>
ImageEnView1.IO.MediaFoundationSourceReader.SetMediaTypeVideo('RGB32', 640, 480, 30);

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.GetMediaType>
- <A TIEMediaFoundationSourceReader.GetMediaTypesCount>
- <A TIEMediaFoundationSourceReader.SetMediaTypeCustom>
- <A TIEMediaFoundationSourceReader.SetMediaTypeAudio>
- <A TIEMediaFoundationSourceReader.SelectMediaType>
- <A TIEMediaFoundationSourceReader.GetCurrentMediaType>
!!}
function TIEMediaFoundationSourceReader.SetMediaTypeVideo(streamIndex: integer; subTypeStr: WideString; frameWidth: integer; frameHeight: integer; frameRate: double; videoLighting: WideString): boolean;
var
  newMediaTypeDict: TIEDictionary;
begin
  newMediaTypeDict := TIEDictionary.Create();
  try
    newMediaTypeDict.Insert(IEMAJORTYPE_DICT_KEY, mmf_VIDEO_STREAM);

    // set subtype
    newMediaTypeDict.Insert(IESUBTYPE_DICT_KEY, subTypeStr);

    // set frameWidth and frameHeight
    if frameWidth <> 0 then
      newMediaTypeDict.Insert(IEFRAMEWIDTH_DICT_KEY, frameWidth);
    if frameHeight <> 0 then
      newMediaTypeDict.Insert(IEFRAMEHEIGHT_DICT_KEY, frameHeight);

    // set framerate
    if frameRate > 0.0 then
      newMediaTypeDict.Insert(IEFRAMERATE_DICT_KEY, frameRate);

    // set video lighting
    if videoLighting <> '' then
      newMediaTypeDict.Insert(IEVIDEOLIGHTING_DICT_KEY, videoLighting);

    result := SetMediaTypeCustom(streamIndex, newMediaTypeDict.Dump());

  finally
    newMediaTypeDict.Free();
  end;
end;


// set mediatype of "first" video stream
function TIEMediaFoundationSourceReader.SetMediaTypeVideo(subTypeStr: WideString; frameWidth: integer; frameHeight: integer; frameRate: double; videoLighting: WideString): boolean;
var
  streamIndex: integer;
begin
  result := false;
  streamIndex := IndexOfFirstStream(mmf_VIDEO_STREAM);
  if streamIndex > -1 then
    result := SetMediaTypeVideo(streamIndex, subTypeStr, frameWidth, frameHeight, frameRate, videoLighting);
end;


{!!
<FS>TIEMediaFoundationSourceReader.SetMediaTypeAudio

<FM>Declaration<FC>
function SetMediaTypeAudio(streamIndex: integer; subTypeStr: WideString): boolean;
function SetMediaTypeAudio(subTypeStr: WideString): boolean;

<FM>Description<FN>
Creates and selects a new audio media type. Media Foundation will provide a decoder to convert from native media type.
Returns False if the media type is not accepted (or a suitable conversion is not available).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>streamIndex<FN></C> <C>Index of the stream, in the range of 0 to <A TIEMediaFoundationSourceReader.StreamCount> - 1</C> </R>
<R> <C><FC>streamType<FN></C> <C>A string representing the stream type. Only the first stream of this type will be considered. Can be any one of the values accepted by <A TIEMediaFoundationSourceReader.GetStreamType></C> </R>
<R> <C><FC>subTypeStr<FN></C> <C>Specifies the audio subtype</C> </R>
</TABLE>

Audio subtypes are: 'PCM', 'Float', 'DTS', 'Dolby_AC3_SPDIF', 'DRM', 'WMAudioV8', 'WMAudioV9', 'WMAudio_Lossless', 'WMASPDIF', 'MSP1', 'MP3', 'MPEG', 'AAC', 'ADTS'

<FM>Example<FC>
audioStreamIndex := ImageEnView1.IO.MediaFoundationSourceReader.IndexOfFirstStream(mmf_AUDIO_STREAM);
ImageEnView1.IO.MediaFoundationSourceReader.SetSelectedStreams(audioStreamIndex, true);
ImageEnView1.IO.MediaFoundationSourceReader.SetMediaTypeAudio(audioStreamIndex, 'PCM');
ImageEnView1.IO.MediaFoundationSourceReader.PushNotifyReceiver( TIEMediaFoundationAudioRenderer.Create(audioStreamIndex) );

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.GetMediaType>
- <A TIEMediaFoundationSourceReader.GetMediaTypesCount>
- <A TIEMediaFoundationSourceReader.SetMediaTypeCustom>
- <A TIEMediaFoundationSourceReader.SetMediaTypeVideo>
- <A TIEMediaFoundationSourceReader.SelectMediaType>
- <A TIEMediaFoundationSourceReader.GetCurrentMediaType>
!!}
function TIEMediaFoundationSourceReader.SetMediaTypeAudio(streamIndex: integer; subTypeStr: WideString): boolean;
var
  newMediaTypeDict: TIEDictionary;
begin
  newMediaTypeDict := TIEDictionary.Create();
  try
    newMediaTypeDict.Insert(IEMAJORTYPE_DICT_KEY, mmf_AUDIO_STREAM);

    // set subtype
    newMediaTypeDict.Insert(IESUBTYPE_DICT_KEY, subTypeStr);

    result := SetMediaTypeCustom(streamIndex, newMediaTypeDict.Dump());

  finally
    newMediaTypeDict.Free();
  end;
end;


function TIEMediaFoundationSourceReader.SetMediaTypeAudio(subTypeStr: WideString): boolean;
var
  streamIndex: integer;
begin
  result := false;
  streamIndex := IndexOfFirstStream(mmf_AUDIO_STREAM);
  if streamIndex > -1 then
    result := SetMediaTypeVideo(streamIndex, subTypeStr);
end;


procedure TIEMediaFoundationSourceReader.DrainSamples();
var
  actualStreamIndex: DWORD;
  streamFlags: DWORD;
  timeStamp: int64;
  sample: IE_IMFSample;
begin
  Lock();
  try
    repeat
      sample := nil;
      m_sourceReader.ReadSample(MF_SOURCE_READER_ANY_STREAM, MF_SOURCE_READER_CONTROLF_DRAIN, actualStreamIndex, streamFlags, timeStamp, sample);
    until sample = nil;
  finally
    Unlock();
  end;
end;


// discard a sample for sync mode or request a sample for async mode
function TIEMediaFoundationSourceReader.ReadSample(streamIndex: DWORD): boolean;
var
  actualStreamIndex: ^DWORD;
  streamFlags: ^DWORD;
  timeStamp: ^int64;
  sample: ^IE_IMFSample;
  flags: DWORD;
begin
  actualStreamIndex := nil;
  streamFlags := nil;
  timeStamp := nil;
  sample := nil;
  flags := 0;

  Lock();
  try
    result := SUCCEEDED(m_sourceReader.ReadSample(streamIndex, flags, actualStreamIndex^, streamFlags^, timeStamp^, sample^));
    if result and IsAsyncMode() then
      inc(m_frameRequested);
  finally
    Unlock();
  end;
end;


// streamType: 'Audio' or 'Video' or 'Any' or index ex: '0', '1'
function TIEMediaFoundationSourceReader.ReadSample(streamType: WideString): TIEMFReceivedSample;
var
  streamIndex: DWORD;
  streamFlags: DWORD;
  actualStreamIndex: DWORD;
  timeStamp: int64;
  sample: IE_IMFSample;
  flags: DWORD;
  hr: HRESULT;
  mediaType: IE_IMFMediaType;
begin
  if streamType = mmf_AUDIO_STREAM then
    streamIndex := MF_SOURCE_READER_FIRST_AUDIO_STREAM
  else
  if streamType = mmf_VIDEO_STREAM then
    streamIndex := MF_SOURCE_READER_FIRST_VIDEO_STREAM
  else
  if streamType = mmf_ANY_STREAM then
    streamIndex := MF_SOURCE_READER_ANY_STREAM
  else
    streamIndex := IEStrToIntDef(AnsiString(streamType), 0);
  flags := 0;
  sample := nil;
  hr := m_sourceReader.ReadSample(streamindex, flags, actualStreamIndex, streamFlags, timeStamp, sample);
  if SUCCEEDED(hr) and ((streamFlags and MF_SOURCE_READERF_ERROR) = 0) then
  begin
    mediaType := nil;
    m_sourceReader.GetCurrentMediaType(actualStreamIndex, mediaType);
    DoVideoProcessing(mediaType, sample);
    result := TIEMFReceivedSample.Create(sample, actualStreamIndex, streamFlags, timeStamp, mediaType);
  end
  else
    result := TIEMFReceivedSample.Create(nil, streamIndex, 0, 0, nil);  // return empty sample container
end;


{!!
<FS>TIEMediaFoundationSourceReader.Flush

<FM>Declaration<FC>
procedure Flush();

<FM>Description<FN>
Discards all received samples. After this the next call to <A TIEMediaFoundationSourceReader.GetNextSample> will fail.
!!}
procedure TIEMediaFoundationSourceReader.Flush();
begin
  ClearReceivedSamples();
end;


{!!
<FS>TIEMediaFoundationSourceReader.SetPosition

<FM>Declaration<FC>
procedure SetPosition(position: int64);

<FM>Description<FN>
Seeks a new position in the media source.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>position<FN></C><C>The Media Foundation position in 100 nanosecond units</C> </R>
</TABLE>

<FM>Example 1<FC>
var
  duration: int64;
begin
  duration := ImageEnView1.IO.MediaFoundationSourceReader.Duration;
  ImageEnView1.IO.MediaFoundationSourceReader.SetPosition( trunc(duration / ScrollBarPosition.Max * ScrollBarPosition.Position) );
end;

<FM>Example 2<FC>
// Move one minute into the sample
ImageEnView1.IO.MediaFoundationSourceReader.SetPosition( IESecToMediaFoundationTime(60) );

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.Duration>  
- <A IESecToMediaFoundationTime>
!!}                      
procedure TIEMediaFoundationSourceReader.SetPosition(position: int64);
var
  lcap: boolean;
  varPosition: PROPVARIANT;
begin
  // SetCapture works only if no frame has been requested, so we must wait
  lcap := PauseCapture();
  Flush();
  varPosition.vt             := VT_I8;
  varPosition.uhVal.QuadPart := position;
  m_sourceReader.SetCurrentPosition(GUID_NULL, varPosition);
  if lcap then
    ResumeCapture();
end;


{!!
<FS>TIEMediaFoundationSourceReader.PushNotifyReceiver

<FM>Declaration<FC>
procedure PushNotifyReceiver(notifyReceiver: <A IIEMediaFoundationReaderNotifyReceiver>);

<FM>Description<FN>
Adds a notify receiver. A notify receiver receives notifications like "start capturing", "new frame", "end of stream".

Applications can use this method to add an audio renderer before capture begins.

Note: <A TImageEnView> is setup as receiver of TIEMediaFoundationSourceReader in order to deliver <A TImageEnView.OnMediaFoundationNotify> events.

<FM>Example<FC>
// Add the audio renderer
audioStreamindex := ImageEnView1.IO.MediaFoundationSourceReader.IndexOfFirstStream(mmf_AUDIO_STREAM);
ImageEnView1.IO.MediaFoundationSourceReader.PushNotifyReceiver( TIEMediaFoundationAudioRenderer.Create(audioStreamIndex) );
ImageEnView1.IO.MediaFoundationSourceReader.StartCapture();

// Remove the audio renderer
ImageEnView1.IO.MediaFoundationSourceReader.StopCapture();
ImageEnView1.IO.MediaFoundationSourceReader.PopNotifyReceiver();

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.PopNotifyReceiver>
- <A TIEMediaFoundationSourceReader.PushNotifyReceiver>
- <A TIEMediaFoundationAudioRenderer>
!!}
procedure TIEMediaFoundationSourceReader.PushNotifyReceiver(notifyReceiver: IIEMediaFoundationReaderNotifyReceiver);
begin
  Lock();
  try
    if m_notifyReceivers.IndexOf(notifyReceiver) = -1 then
      m_notifyReceivers.Add(notifyReceiver);
  finally
    Unlock();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.PopNotifyReceiver

<FM>Declaration<FC>
procedure PopNotifyReceiver();

<FM>Description<FN>
Removes the last added notify receiver.

Applications can use this method to remove the audio renderer after capture ends.

<FM>Example<FC>
// Add the audio renderer
audioStreamindex := ImageEnView1.IO.MediaFoundationSourceReader.IndexOfFirstStream(mmf_AUDIO_STREAM);
ImageEnView1.IO.MediaFoundationSourceReader.PushNotifyReceiver( TIEMediaFoundationAudioRenderer.Create(audioStreamIndex) );
ImageEnView1.IO.MediaFoundationSourceReader.StartCapture();

// Remove the audio renderer
ImageEnView1.IO.MediaFoundationSourceReader.StopCapture();
ImageEnView1.IO.MediaFoundationSourceReader.PopNotifyReceiver();

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.ClearNotifyReceivers>
- <A TIEMediaFoundationSourceReader.PushNotifyReceiver>
- <A TIEMediaFoundationAudioRenderer>
!!}
procedure TIEMediaFoundationSourceReader.PopNotifyReceiver();
begin
  Lock();
  try
    if m_notifyReceivers.Count > 0 then
      m_notifyReceivers.Delete(m_notifyReceivers.Count - 1);
  finally
    Unlock();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.ClearNotifyReceivers

<FM>Declaration<FC>
procedure ClearNotifyReceivers();

<FM>Description<FN>
Removes all notify receivers.

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.PopNotifyReceiver>
- <A TIEMediaFoundationSourceReader.PushNotifyReceiver>
!!}
procedure TIEMediaFoundationSourceReader.ClearNotifyReceivers();
begin
  Lock();
  try
    m_notifyReceivers.Clear();
  finally
    Unlock();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.UpdateVideoInputs

<FM>Declaration<FC>
procedure UpdateVideoInputs();

<FM>Description<FN>
Reloads the video input list.

This method is automatically called the first time the <A TIEMediaFoundationSourceReader.VideoInputs> property is read. Applications should call UpdateVideoInputs() whenever a new device is added or removed.

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.VideoInputs>
!!}
procedure TIEMediaFoundationSourceReader.UpdateVideoInputs();
begin
  m_videoInputs.Populate();
end;


procedure TIEMediaFoundationSourceReader.CheckVideoInputsPopulated();
begin
  if not m_videoInputs.Populated then
    UpdateVideoInputs();
end;


procedure TIEMediaFoundationSourceReader.CheckVideoInputIndex(index: integer);
begin
  CheckVideoInputsPopulated();
  if (Index < 0) or (Index >= m_videoInputs.GetCount()) then
    raise Exception.Create('Invalid video input index');
end;


{!!
<FS>TIEMediaFoundationSourceReader.VideoInputs

<FM>Declaration<FC>
property VideoInputs: TStringList;

<FM>Description<FN>
Contains a list of video input device names.

Applications can refresh this list by calling <A TIEMediaFoundationSourceReader.UpdateVideoInputs>.

<FM>Example<FC>
// Fill ComboBoxVideoInputs with the list of video inputs
ComboBoxVideoInputs.Items.Assign(ImageEnView1.IO.MediaFoundationSourceReader.VideoInputs);

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.UpdateVideoInputs>
!!}
function TIEMediaFoundationSourceReader.GetVideoInputs(): TStringList;
begin
  CheckVideoInputsPopulated();
  result := m_videoInputs.GetNames();
end;


function TIEMediaFoundationSourceReader.IsAsyncMode(): boolean;
begin
  result := m_notifyReceivers.Count > 0;
end;


{!!
<FS>TIEMediaFoundationSourceReader.StartCapture

<FM>Declaration<FC>
function StartCapture(): boolean;

<FM>Description<FN>
Starts capturing of the sample. Applications can stop capturing by calling <A TIEMediaFoundationSourceReader.StopCapture>.
Returns True on success.

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.Capturing>
- <A TIEMediaFoundationSourceReader.PauseCapture>
- <A TIEMediaFoundationSourceReader.ResumeCapture>
- <A TIEMediaFoundationSourceReader.StartCapture>
- <A TIEMediaFoundationSourceReader.StopCapture>
!!}
function TIEMediaFoundationSourceReader.StartCapture(): boolean;
begin
  SetupVideoProcessing();
  m_firstTimeStamp := -1;
  SetPosition(0);
  m_capturing := true;
  result := ReadSample(MF_SOURCE_READER_ANY_STREAM);
end;


{!!
<FS>TIEMediaFoundationSourceReader.StopCapture

<FM>Declaration<FC>
procedure StopCapture();

<FM>Description<FN>
Terminates capturing of the sample. Applications can start capturing by calling <A TIEMediaFoundationSourceReader.StartCapture>.

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.Capturing>
- <A TIEMediaFoundationSourceReader.PauseCapture>
- <A TIEMediaFoundationSourceReader.ResumeCapture>
- <A TIEMediaFoundationSourceReader.StartCapture>
- <A TIEMediaFoundationSourceReader.StopCapture>
!!}
procedure TIEMediaFoundationSourceReader.StopCapture();
begin
  PauseCapture();
  Flush();
  FinalizeVideoProcessing();

  Lock();
  try
    if assigned(m_selectedActivate) then
      SetInput(m_selectedActivate);
  finally
    Unlock();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.ResumeCapture

<FM>Declaration<FC>
procedure ResumeCapture();

<FM>Description<FN>
Resumes capturing of the sample. Call <A TIEMediaFoundationSourceReader.PauseCapture> to pause capturing.

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.Capturing>
- <A TIEMediaFoundationSourceReader.PauseCapture>
- <A TIEMediaFoundationSourceReader.ResumeCapture>
- <A TIEMediaFoundationSourceReader.StartCapture>
- <A TIEMediaFoundationSourceReader.StopCapture>
!!}
procedure TIEMediaFoundationSourceReader.ResumeCapture();
begin
  Lock();
  try
    m_firstTimeStamp := -1;
    m_capturing := true;
    ReadSample(MF_SOURCE_READER_ANY_STREAM);
  finally
    Unlock();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.Capturing

<FM>Declaration<FC>
property Capturing: boolean;

<FM>Description<FN>
Returns True if capturing is underway.
Capturing is True after <A TIEMediaFoundationSourceReader.ResumeCapture> or <A TIEMediaFoundationSourceReader.StartCapture> is called.
Capturing is False after <A TIEMediaFoundationSourceReader.PauseCapture> or <A TIEMediaFoundationSourceReader.StopCapture> is called.

This property is read-only.

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.PauseCapture>
- <A TIEMediaFoundationSourceReader.ResumeCapture>
- <A TIEMediaFoundationSourceReader.StartCapture>
- <A TIEMediaFoundationSourceReader.StopCapture>
!!}
function TIEMediaFoundationSourceReader.GetCapturing(): boolean;
begin
  Lock();
  result := m_capturing;
  Unlock();
end;


{!!
<FS>TIEMediaFoundationSourceReader.PauseCapture

<FM>Declaration<FC>
function PauseCapture(): boolean;

<FM>Description<FN>
Suspends capturing of sample. Call <A TIEMediaFoundationSourceReader.ResumeCapture> to restart capturing.
Returns the capture state before pausing (True = was capturing, False = was not capturing).

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.Capturing>
- <A TIEMediaFoundationSourceReader.PauseCapture>
- <A TIEMediaFoundationSourceReader.ResumeCapture>
- <A TIEMediaFoundationSourceReader.StartCapture>
- <A TIEMediaFoundationSourceReader.StopCapture>
!!}
// pause capture and wait for ReadSample() finishes and flush
// ret. True if was on Capture state before pause
function TIEMediaFoundationSourceReader.PauseCapture(): boolean;
const
  TIMEOUT = 1000;
var
  waitTime: DWORD;
begin
  // pause capture
  Lock();
  result := m_capturing;
  m_capturing := false;
  Unlock();

  // wait for ReadSample() completes (callback responds)
  waitTime := GetTickCount();
  while GetTickCount() - waitTime < TIMEOUT do
  begin
    Lock();
    try
      if m_frameRequested = 0 then
        break;
    finally
      Unlock();
    end;
  end;
end;


function TIEMediaFoundationSourceReader.AddReceivedSample(sample: TIEMFReceivedSample): TIEMFReceivedSample;
begin
  Lock();
  try
    m_receivedSamples.Add(sample);
    result := sample;
  finally
    Unlock();
  end;
end;


function TIEMediaFoundationSourceReader.PopReceivedSample(streamIndex: DWORD): TIEMFReceivedSample;
var
  i: integer;
begin
  Lock();
  try
    result := nil;
    if m_receivedSamples.Count > 0 then
    begin
      for i := 0 to m_receivedSamples.Count - 1 do
        if (streamIndex = MF_SOURCE_READER_ANY_STREAM) or ((m_receivedSamples[i] as TIEMFReceivedSample).StreamIndex = streamIndex) then
        begin
          result := m_receivedSamples[i] as TIEMFReceivedSample;
          m_receivedSamples.Extract(result);  // detach from objects list
          break;
        end;
    end;
    if result = nil then
      result := TIEMFReceivedSample.Create(nil, streamIndex, 0, 0, nil);  // return empty sample container
  finally
    Unlock();
  end;
end;


procedure TIEMediaFoundationSourceReader.ClearReceivedSamples();
begin
  Lock();
  try
    m_receivedSamples.Clear();
  finally
    Unlock();
  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.GetNextSample

<FM>Declaration<FC>
function GetNextSample(): <A TIEMFReceivedSample>;

<FM>Description<FN>
Retrieve the next sample from the samples buffer.

Note: Applications should free the received sample.

<FM>Example<FC>
// Handler for TImageEnView.OnMediaFoundatioNotify event
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

<FM>See Also<FN>
- <A TIEMediaFoundationSourceReader.Capturing>
- <A TIEMediaFoundationSourceReader.StartCapture>
- <A TIEMediaFoundationSourceReader.StopCapture>
!!}
// read a sample from samples list (m_receivedSamples) on async mode or directly on sync mode
// applications must free returned TIEMFReceivedSample object
function TIEMediaFoundationSourceReader.GetNextSample(): TIEMFReceivedSample;
begin
  if IsAsyncMode() then
    // Async mode
    result := PopReceivedSample(MF_SOURCE_READER_ANY_STREAM)
  else
    // Sync mode
    result := ReadSample(mmf_ANY_STREAM);
end;


{!!
<FS>TIEMediaFoundationSourceReader.SamplesBufferSize

<FM>Declaration<FC>
property SamplesBufferSize: integer;

<FM>Description<FN>
Specifies how many samples can be stored waiting to be displayed or processed.

Default value is 30.
!!}
procedure TIEMediaFoundationSourceReader.SetSamplesBufferSize(value: integer);
begin
  Lock();
  m_samplesBufferSize := value;
  UnLock();
end;


procedure TIEMediaFoundationSourceReader.SendNotify(notifyType: TIEMediaFountationNotifyType; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; mediaType: IE_IMFMediaType; pEvent: IE_IMFMediaEvent);
var
  i: integer;
begin
  for i := 0 to m_notifyReceivers.Count - 1 do
    IIEMediaFoundationReaderNotifyReceiver(m_notifyReceivers[i]).ReceiveNotify(self, notifyType, dwStreamIndex, dwStreamFlags, llTimestamp, pSample, mediaType, pEvent);
end;
             

function TIEMediaFoundationSourceReader.SourceReaderCallback(event: TIEMediaFoundationSourceReaderCallbackEventType; hrStatus: HRESULT; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; pEvent: IE_IMFMediaEvent): HRESULT;
var
  timeDiff: int64;
  systemTime: int64;
  mediaType: IE_IMFMediaType;
begin
  result := S_OK;

  case event of

    mfrceONREADSAMPLE:
      begin

        if not Capturing then
          exit;

        mediaType := GetCurrentMediaTypeIntf(dwStreamIndex);

        // wait for the sample time
        systemTime := MFGetSystemTime();
        if (m_firstTimeStamp = -1) then
        begin
          m_firstTimeStamp := systemTime - llTimestamp;
          // send start capture notify
          SendNotify(iemfnSTARTINGCAPTURE, dwStreamIndex, dwStreamFlags, llTimestamp, pSample, mediaType, pEvent);
        end;

        if m_delayFramePost and (GetStreamType(dwStreamIndex) = mmf_VIDEO_STREAM) then
        begin
          timeDiff := llTimestamp - (systemTime - m_firstTimeStamp);
          if timeDiff > 0 then
            sleep(timeDiff div 10000);
        end;

        Lock();
        try
          dec(m_frameRequested);
          if Capturing then
          begin

            // add sample to the samples list
            if assigned(pSample) and (m_receivedSamples.Count < m_samplesBufferSize) then
            begin
              DoVideoProcessing(mediaType, pSample);
              AddReceivedSample(TIEMFReceivedSample.Create(pSample, dwStreamIndex, dwStreamFlags, llTimestamp, mediaType));
              // send frame notify
              SendNotify(iemfnFRAME, dwStreamIndex, dwStreamFlags, llTimestamp, pSample, mediaType, pEvent);
            end;

            // notify end of stream
            if (dwStreamFlags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0 then
              SendNotify(iemfnENDOFSTREAM, dwStreamIndex, dwStreamFlags, llTimestamp, pSample, mediaType, pEvent);

            // request another sample
            if ((dwStreamFlags and MF_SOURCE_READERF_ERROR) = 0) and ((dwStreamFlags and MF_SOURCE_READERF_ENDOFSTREAM) = 0) then
              ReadSample(MF_SOURCE_READER_ANY_STREAM);
          end;

        finally
          Unlock();
        end;
        
      end;

    mfrceONFLUSH:
      begin
        // nothing to do
      end;

    mfrceONEVENT:
      begin
        // nothing to do
      end;

  end;
end;


{!!
<FS>TIEMediaFoundationSourceReader.VideoProcessor

<FM>Declaration<FC>
property VideoProcessor: <A TIEMediaFoundationVideoProcessor>;

<FM>Description<FN>
Use this property to setup the Media Foundation Video Processor. Applications can set rotations, mirrors, source and destination rectangles.

This property is read-only.
Only for Windows 8 or Windows 2012 Server.

<FM>Example<FC>
// setup horizontal flip and automatic rotation
ImageEnView1.IO.MediaFoundationSourceReader.VideoProcessor.SetMirror(mfpmHorizontal);
ImageEnView1.IO.MediaFoundationSourceReader.VideoProcessor.SetRotation(mfprNormal);
!!}
function TIEMediaFoundationSourceReader.GetVideoProcessor(): TIEMediaFoundationVideoProcessor;
begin
  if not assigned(m_videoProcessor) then
    m_videoProcessor := TIEMediaFoundationVideoProcessor.Create();
  result := m_videoProcessor;
end;


procedure TIEMediaFoundationSourceReader.SetupVideoProcessing();
var
  mt: TIEDictionary;
begin
  if assigned(m_videoProcessor) and m_videoProcessor.IsAvailable then
  begin
    // RGB32 is supported by the video processor
    mt := GetCurrentMediaType(mmf_VIDEO_STREAM);
    mt.Insert(IESUBTYPE_DICT_KEY, mmf_VideoFormat_RGB32);
    SetMediaTypeCustom(mmf_VIDEO_STREAM, mt.Dump());
  end;
end;


procedure TIEMediaFoundationSourceReader.FinalizeVideoProcessing();
begin
  if assigned(m_videoProcessor) and m_videoProcessor.IsAvailable then
    m_videoProcessor.Stop();
end;


procedure TIEMediaFoundationSourceReader.DoVideoProcessing(var mediaType: IE_IMFMediaType; var sample: IE_IMFSample);
var
  mediaRotation: DWORD;
  wantedRotation: DWORD;
begin
  if assigned(m_videoProcessor) and m_videoProcessor.IsAvailable and (IEGetMediaTypeMajorTypeStr(mediaType) = mmf_VIDEO_STREAM) then
  begin
    if not m_videoProcessor.Started then
    begin
      m_videoProcessor.SetInputMediaType(mediaType);
      //mediaType.SetGUID(MF_MT_SUBTYPE, IEConvertStringToSubType(mmf_VideoFormat_RGB24));  // output of video processor will be RGB24 (for faster TIEBitmap conversion) (NOT WORK ON SOME WEBCAMS!!)
      m_videoProcessor.SetOutputMediaType(mediaType);
      m_videoProcessor.Start();
    end;

    mediaRotation := 0;
    mediaType.GetUINT32(MF_MT_VIDEO_ROTATION, mediaRotation);
    wantedRotation := 360 - IEGetDisplayOrientation();
    if wantedRotation <> mediaRotation then
    begin
      mediaType.SetUINT32(MF_MT_VIDEO_ROTATION, wantedRotation);
      m_videoProcessor.SetInputMediaType(mediaType);
    end;
    mediaType := m_videoProcessor.GetOutputMediaType();

    m_videoProcessor.PushSample(sample);
    sample := m_videoProcessor.GetSample();
  end;
end;


// TIEMediaFoundationSourceReader
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationAudioResampler

constructor TIEMediaFoundationAudioResampler.Create();
var
  props: IE_IWMResamplerProps;
begin
  inherited Create();

  m_outputBufferSize := 0;

  m_transform := nil;
  CoCreateInstance(CLSID_CResamplerMediaObject, nil, CLSCTX_INPROC_SERVER, IE_IMFTransform_GUID, m_transform);
  if not assigned(m_transform) then
    exit;

  // set conversion quality
  props := nil;
  m_transform.QueryInterface(IE_IWMResamplerProps_GUID, props);
  props.SetHalfFilterLength(60);
end;


destructor TIEMediaFoundationAudioResampler.Destroy();
begin
  Stop();
  m_transform := nil;
  inherited;
end;


function TIEMediaFoundationAudioResampler.SetInputMediaType(mediaType: IE_IMFMediaType): boolean;
begin
  result := SUCCEEDED(m_transform.SetInputType(0, mediaType, 0));
end;


function TIEMediaFoundationAudioResampler.SetOutputMediaType(mediaType: IE_IMFMediaType): boolean;
var
  streamInfo: IE_MFT_OUTPUT_STREAM_INFO;
  samplesPerSecond: DWORD;
  numChannels: DWORD;
begin
  result := SUCCEEDED(m_transform.SetOutputType(0, mediaType, 0));

  m_transform.GetOutputStreamInfo(0, streamInfo);
  mediaType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, samplesPerSecond);
  mediaType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS, numChannels);
  m_outputBufferSize := streamInfo.cbSize * samplesPerSecond;
end;


procedure TIEMediaFoundationAudioResampler.Start();
begin
  m_transform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH, 0);
  m_transform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING, 0);
  m_transform.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM, 0);
end;


procedure TIEMediaFoundationAudioResampler.Stop();
begin
  m_transform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_OF_STREAM, 0);
  m_transform.ProcessMessage(MFT_MESSAGE_COMMAND_DRAIN, 0);
  m_transform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_STREAMING, 0);
end;


function TIEMediaFoundationAudioResampler.PushSample(sample: IE_IMFSample): boolean;
var
  hr: HRESULT;
begin
  hr := m_transform.ProcessInput(0, sample, 0);
  result := SUCCEEDED(hr);
end;


function TIEMediaFoundationAudioResampler.GetSample(): IE_IMFSample;
var
  outputDataBuffer: IE_MFT_OUTPUT_DATA_BUFFER;
  status: DWORD;
  hr: HRESULT;
  mediaBuffer: IE_IMFMediaBuffer;
begin
  result := nil;

  MFCreateSample(result);
  MFCreateMemoryBuffer(m_outputBufferSize, mediaBuffer);
  result.AddBuffer(mediaBuffer);
  outputDataBuffer.dwStreamID := 0;
  outputDataBuffer.pSample    := result;
  outputDataBuffer.dwStatus   := 0;
  outputDataBuffer.pEvents    := nil;

  hr := m_transform.ProcessOutput(0, 1, @outputDataBuffer, status);
  if DWORD(hr) = MF_E_TRANSFORM_NEED_MORE_INPUT then
  begin
    result := nil;
  end;
end;


// TIEMediaFoundationAudioResampler
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationAudioRenderer

constructor TIEMediaFoundationAudioRenderer.Create(streamIndex: DWORD; role: TIEMediaFoundationAudioRendererRole);
var
  attributes: IE_IMFAttributes;
  timeSource: IE_IMFPresentationTimeSource;
begin
  inherited Create();

  m_streamIndex := streamIndex;
  m_resampler   := nil;

  if IEMFStartup() then
  begin
    attributes := nil;
    MFCreateAttributes(attributes, 1);
    attributes.SetUINT32(MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ROLE, DWORD(0));

    m_mediaSink := nil;
    MFCreateAudioRenderer(attributes, m_mediaSink);

    m_streamSink := nil;
    m_mediaSink.GetStreamSinkByIndex(0, m_streamSink);

    timeSource := nil;
    m_mediaSink.QueryInterface(IE_IMFPresentationTimeSource_GUID, timeSource);

    m_presentationClock := nil;
    MFCreatePresentationClock(m_presentationClock);
    m_presentationClock.SetTimeSource(timeSource);

    m_mediaSink.SetPresentationClock(m_presentationClock);
  end;
end;


destructor TIEMediaFoundationAudioRenderer.Destroy();
begin
  FreeAndNil(m_resampler);

  m_streamSink := nil;
  m_mediaSink := nil;
  m_presentationClock := nil;

  IEMFShutdown();

  inherited;
end;


// Audio renderer supports only PCM or Float media types
function TIEMediaFoundationAudioRenderer.SetMediaType(mediaType: IE_IMFMediaType): boolean;
var
  mediaTypeHandler: IE_IMFMediaTypeHandler;
  hr: HRESULT;
  samplesPerSecond: DWORD;
begin
  FreeAndNil(m_resampler);

  mediaType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, samplesPerSecond);
  if samplesPerSecond <> 44100 then
  begin
    // a resampler is necessary
    m_resampler := TIEMediaFoundationAudioResampler.Create();
    m_resampler.SetInputMediaType(mediaType);
    mediaType := IECreateMediaTypeFromDictionary('{"MajorType":"Audio", "SubType":"PCM", "AudioSamplesPerSecond":44100, "AudioBitsPerSample":16, "AudioNumChannels":2, "AudioBlockAlignment":4, "AudioAvgBytesPerSecond":176400}');
    m_resampler.SetOutputMediaType(mediaType);
  end;

  mediaTypeHandler := nil;
  m_streamSink.GetMediaTypeHandler(mediaTypeHandler);
  hr := mediaTypeHandler.SetCurrentMediaType(mediaType);
  result := SUCCEEDED(hr);
end;


procedure TIEMediaFoundationAudioRenderer.ReceiveNotify(sender: TObject; notifyType: TIEMediaFountationNotifyType; dwStreamIndex: DWORD; dwStreamFlags: DWORD; llTimestamp: int64; pSample: IE_IMFSample; mediaType: IE_IMFMediaType; pEvent: IE_IMFMediaEvent);
var
  rSample: IE_IMFSample;
begin
  if dwStreamIndex = m_streamIndex then
  begin
    case notifyType of
      iemfnSTARTINGCAPTURE:
        begin
          // setup audio renderer media type
          SetMediaType(mediaType);
          // start clock
          m_presentationClock.Start(llTimestamp);
          // start resampler if exists
          if assigned(m_resampler) then
            m_resampler.Start();
        end;
      iemfnFRAME:
        begin
          if assigned(m_resampler) then
          begin
            m_resampler.PushSample(pSample);
            rSample := m_resampler.GetSample();
            if assigned(rSample) then
              m_streamSink.ProcessSample(rSample);
          end
          else
            m_streamSink.ProcessSample(pSample);
        end;
      iemfnENDOFSTREAM:
        begin
        end;
    end;
  end;
end;


// TIEMediaFoundationAudioRenderer
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEMediaFoundationVideoProcessor
// Windows 8 only!!

constructor TIEMediaFoundationVideoProcessor.Create();
begin
  inherited;

  if IEMFStartup() then
  begin
    m_started := false;

    m_transform := nil;
    CoCreateInstance(CLSID_VideoProcessorMFT, nil, CLSCTX_INPROC_SERVER, IE_IMFTransform_GUID, m_transform);
    if not assigned(m_transform) then
      exit;
    m_control := nil;
    m_transform.QueryInterface(IE_IMFVideoProcessorControl_GUID, m_control);

    m_mediaBuffer := nil;
  end;
end;


destructor TIEMediaFoundationVideoProcessor.Destroy();
begin
  if IsAvailable then
    Stop();

  m_control   := nil;
  m_transform := nil;
  m_mediaBuffer := nil;

  IEMFShutdown();

  inherited;
end;


{!!
<FS>TIEMediaFoundationVideoProcessor.IsAvailable

<FM>Declaration<FC>
property IsAvailable: boolean;

<FM>Description<FN>
This property is true when Media Foundation Video Processor is available. This happens from Windows 8 or Windows 2012 Server.
!!}
function TIEMediaFoundationVideoProcessor.GetIsAvailable(): boolean;
begin
  result := m_control <> nil;
end;


{!!
<FS>TIEMediaFoundationVideoProcessor.SetSourceRectangle

<FM>Declaration<FC>
procedure SetSourceRectangle(rect: TRect);

<FM>Description<FN>
Sets the source rectangle. The source rectangle is the portion of the input frame that is blitted to the destination surface.
!!}
procedure TIEMediaFoundationVideoProcessor.SetSourceRectangle(rect: TRect);
var
  r: IE_MFRECT;
begin
  if IsAvailable then
  begin
    r.left   := rect.Left;
    r.top    := rect.Top;
    r.right  := rect.Right;
    r.bottom := rect.Bottom;
    m_control.SetSourceRectangle(r);
  end;
end;


{!!
<FS>TIEMediaFoundationVideoProcessor.SetDestinationRectangle

<FM>Declaration<FC>
procedure SetDestinationRectangle(rect: TRect);

<FM>Description<FN>
Sets the destination rectangle. The destination rectangle is the portion of the output surface where the source rectangle is blitted.
!!}
procedure TIEMediaFoundationVideoProcessor.SetDestinationRectangle(rect: TRect);
var
  r: IE_MFRECT;
begin
  if IsAvailable then
  begin
    r.left   := rect.Left;
    r.top    := rect.Top;
    r.right  := rect.Right;
    r.bottom := rect.Bottom;
    m_control.SetDestinationRectangle(r);
  end;
end;


{!!
<FS>TIEMediaFoundationVideoProcessor.SetMirror

<FM>Declaration<FC>
procedure SetMirror(mirror: <A TIEMediaFoundationVideoProcessorMirror>);

<FM>Description<FN>
Specifies whether to flip the video image.
!!}
procedure TIEMediaFoundationVideoProcessor.SetMirror(mirror: TIEMediaFoundationVideoProcessorMirror);
begin
  if IsAvailable then
    m_control.SetMirror(DWORD(mirror));
end;


{!!
<FS>TIEMediaFoundationVideoProcessor.SetRotation

<FM>Declaration<FC>
procedure SetRotation(rotation: <A TIEMediaFoundationVideoProcessorRotation>);

<FM>Description<FN>
Specifies whether to rotate the video to the correct orientation.
!!}
procedure TIEMediaFoundationVideoProcessor.SetRotation(rotation: TIEMediaFoundationVideoProcessorRotation);
begin
  if IsAvailable then
    m_control.SetRotation(DWORD(rotation));
end;


function TIEMediaFoundationVideoProcessor.SetInputMediaType(mediaType: IE_IMFMediaType): boolean;
begin
  result := SUCCEEDED(m_transform.SetInputType(0, mediaType, 0));
end;


function TIEMediaFoundationVideoProcessor.GetOutputMediaType(): IE_IMFMediaType;
begin
  result := nil;
  m_transform.GetOutputCurrentType(0, result);
end;


function TIEMediaFoundationVideoProcessor.SetOutputMediaType(mediaType: IE_IMFMediaType): boolean;
var
  streamInfo: IE_MFT_OUTPUT_STREAM_INFO;
begin
  result := SUCCEEDED(m_transform.SetOutputType(0, mediaType, 0));

  m_transform.GetOutputStreamInfo(0, streamInfo);
  m_outputBufferSize := streamInfo.cbSize;
end;


procedure TIEMediaFoundationVideoProcessor.Start();
begin
  m_transform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH, 0);
  m_transform.ProcessMessage(MFT_MESSAGE_NOTIFY_BEGIN_STREAMING, 0);
  m_transform.ProcessMessage(MFT_MESSAGE_NOTIFY_START_OF_STREAM, 0);
  m_started := true;
  m_mediaBuffer := nil;
end;


procedure TIEMediaFoundationVideoProcessor.Stop();
begin
  m_transform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_OF_STREAM, 0);
  m_transform.ProcessMessage(MFT_MESSAGE_COMMAND_DRAIN, 0);
  m_transform.ProcessMessage(MFT_MESSAGE_NOTIFY_END_STREAMING, 0);
  m_started := false;
  m_mediaBuffer := nil;
end;


function TIEMediaFoundationVideoProcessor.PushSample(sample: IE_IMFSample): boolean;
var
  hr: HRESULT;
begin
  hr := m_transform.ProcessInput(0, sample, 0);
  result := SUCCEEDED(hr);
end;


function TIEMediaFoundationVideoProcessor.PushSample(buffer: pointer; bufferLen: integer): boolean;
var
  sample: IE_IMFSample;
  maxLen, curLen: DWORD;
  destBuffer: pbyte;
begin
  MFCreateSample(sample);
  if m_mediaBuffer = nil then
    MFCreateMemoryBuffer(bufferLen, m_mediaBuffer);
  m_mediaBuffer.SetCurrentLength(bufferLen);
  m_mediaBuffer.Lock(destBuffer, maxLen, curLen);
  CopyMemory(destBuffer, buffer, bufferLen);
  m_mediaBuffer.Unlock();
  sample.AddBuffer(m_mediaBuffer);
  result := PushSample(sample);
end;


function TIEMediaFoundationVideoProcessor.GetSample(): IE_IMFSample;
var
  outputDataBuffer: IE_MFT_OUTPUT_DATA_BUFFER;
  status: DWORD;
  hr: HRESULT;
begin
  result := nil;

  MFCreateSample(result);
  if m_mediaBuffer = nil then
    MFCreateMemoryBuffer(m_outputBufferSize, m_mediaBuffer);
  result.AddBuffer(m_mediaBuffer);
  outputDataBuffer.dwStreamID := 0;
  outputDataBuffer.pSample    := result;
  outputDataBuffer.dwStatus   := 0;
  outputDataBuffer.pEvents    := nil;

  hr := m_transform.ProcessOutput(0, 1, @outputDataBuffer, status);
  if DWORD(hr) = MF_E_TRANSFORM_NEED_MORE_INPUT then
  begin
    result := nil;
    m_mediaBuffer := nil;
  end;
  m_transform.ProcessMessage(MFT_MESSAGE_COMMAND_FLUSH, 0);
end;


procedure TIEMediaFoundationVideoProcessor.GetSample(destBuffer: pointer);
var
  sample: IE_IMFSample;
  mediaBuffer: IE_IMFMediaBuffer;
  srcBuffer: pbyte;
  maxLen, curLen: DWORD;
begin
  sample := GetSample();
  if assigned(sample) then
  begin
    sample.ConvertToContiguousBuffer(mediaBuffer);
    mediaBuffer.Lock(srcBuffer, maxLen, curLen);
    CopyMemory(destBuffer, srcBuffer, maxLen);
    mediaBuffer.Unlock();
  end;
end;


// TIEMediaFoundationVideoProcessor
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Initialization / finalization


procedure IEMediaFoundationSetupDefaultVideoSampleDecoders();
begin
  IEMediaFoundationGetVideoSampleDecoders().Add(TIEMediaFoundationVideoSampleDecoder_RGB24.Create());
  IEMediaFoundationGetVideoSampleDecoders().Add(TIEMediaFoundationVideoSampleDecoder_YUY2.Create());
  IEMediaFoundationGetVideoSampleDecoders().Add(TIEMediaFoundationVideoSampleDecoder_I420.Create());
  IEMediaFoundationGetVideoSampleDecoders().Add(TIEMediaFoundationVideoSampleDecoder_NV12.Create());
  IEMediaFoundationGetVideoSampleDecoders().Add(TIEMediaFoundationVideoSampleDecoder_RGB32.Create());
end;


initialization
  // nothing to do
  

finalization
  IEMFUnloadLibrary();


// Initialization / finalization
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////



{$else} // not IEINCLUDEMEDIAFOUNDATION

interface

implementation

{$endif}

end.
