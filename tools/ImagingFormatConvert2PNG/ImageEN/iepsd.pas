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
File version 1004
*)

unit iepsd;

{$R-}
{$Q-}

{$I ie.inc}

interface

{$ifdef IEINCLUDEPSD}

uses Windows, Graphics, classes, sysutils, ImageEnProc, ImageEnIO, hyiedefs, hyieutils;

procedure IEReadPSD(Stream: TStream; MergedBitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; LoadLayers: boolean; layers: TList);
procedure IEWritePSD(Stream: TStream; var IOParams: TIOParamsVals; var Progress: TProgressRec; mergedImage: TIEBitmap; layers: TList);
function IETryPSD(Stream: TStream): boolean;

{$endif}

implementation

{$ifdef IEINCLUDEPSD}

uses imageenview, jpegfilt, iesettings;



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PSD Read



const
  MAXLAYERNAME = 1024;

type

TPSDHeader = packed record
  Signature: array [0..3] of AnsiChar; // must be '8BPS'
  Version : word;                      // must be 1
  Reserved: array [0..5] of byte;      // must be 0
  Channels: word;                      // 1 to 24
  Rows: longint;                       // 1 to 30000
  Columns: longint;                    // 1 to 30000
  Depth: word;                         // 1, 8, 16
  Mode: word;                          // 0=bitmap, 1=grayscale, 2=indexed, 3=RGB, 4=CMYK, 7=mutlichannel, 8=duotone, 9=lab
end;

TColorMapChannel = array [0..255] of byte;

TColorMap = array of TColorMapChannel;


TPSDResolutionInfo = packed record
  hRes: longint;       // fixed point number: pixels per inch
  hResUnit: word;      // 1=pixels per inch, 2=pixels per centimeter
  WidthUnit: word;     // 1=in, 2=cm, 3=pt, 4=picas, 5=columns
  vRes: longint;       // fixed point number: pixels per inch
  vResUnit: word;      // 1=pixels per inch, 2=pixels per centimeter
  HeightUnit: word;    // 1=in, 2=cm, 3=pt, 4=picas, 5=columns
end;
PPSDResolutionInfo = ^TPSDResolutionInfo;

TPSDThumbnailInfo = packed record
  format: longint;         // 1=jpeg 0=raw
  width: longint;          // thumbnail width
  height: longint;         // thumbnail height
  widthbytes: longint;     // rowlen aligned
  size: longint;           // uncompressed size
  compressedsize: longint; // compressed size
  bitspixel: word;         // bits per pixel (24)
  planes: word;            // number of planes (1)
end;
PPSDThumbnailInfo = ^TPSDThumbnailInfo;

TPSDReaderContext = class
  Stream: TStream;
  IOParams: TIOParamsVals;
  header: TPSDHeader;
  colormap: TColorMap;
  transpindex: integer;  // index of transparency when mode=indexed (2)
  layers: TList;
  LoadLayers: boolean;
  XProgress: TProgressRec;
  MergedBitmap: TIEBitmap;
  thumbnailLoaded: boolean;
  hasPremultipliedAlpha: boolean;
  hasICC: boolean;
end;

const
  MAGIK:    array [0..3] of AnsiChar = '8BPS';
  RESMAGIK: array [0..3] of AnsiChar = '8BIM';

procedure ReadImageData(Stream: TStream; Bitmap: TIEBitmap; width, height, depth, mode: integer; colormap: TColorMap; transpindex: integer; compression: smallint; sizes: TIEArrayOfDWord; var cursize: integer; channel: integer; progress: PProgressRec = nil); forward;

// get 16 bit signed value
function GetSmallint(Stream: TStream): smallint;
begin
  Stream.Read(result, 2);
  result := IESwapWord(result);
end;

// get 32 bit signed value
function GetLongint(Stream: TStream): longint;
begin
  Stream.Read(result, 4);
  result := IESwapDWord(result);
end;

// get 64 bit signed value
function GetInt64(Stream: TStream): int64;
begin
  Stream.Read(result, 8);
  result := IESwapInt64(result);
end;

function GetByte(Stream: TStream): byte;
begin
  Stream.Read(result, 1);
end;

function GetWord(Stream: TStream): word;
begin
  Stream.Read(result, 2);
  result := IESwapWord(result);
end;

function GetDWord(Stream: TStream): dword;
begin
  Stream.Read(result, 4);
  result := IESwapDWord(result);
end;

function IETryPSD(Stream: TStream): boolean;
var
  lpos: int64;
  header: TPSDHeader;
begin
  result := false;
  lpos := Stream.Position;
  try
    Stream.Read(header, sizeof(TPSDHeader));
    if not CompareMem(@header.Signature, @MAGIK, 4) then
      exit;
    with header do
    begin
      Version  := IESwapWord(Version);  // 1 = PSD, 2 = PSB
      Channels := IESwapWord(Channels);
      Rows     := IESwapDWord(Rows);
      Columns  := IESwapDWord(Columns);
      Depth    := IESwapWord(Depth);
      Mode     := IESwapWord(Mode);
      if ((Version <> 1) and (Version <> 2)) or (Channels < 1) or (Channels > 24) or (Depth < 1) or (Depth > 16) or (Mode > 9) then
        exit;
    end;
    result := true;
  finally
    Stream.Position := lpos;
  end;
end;

// read Color mode data section
procedure ReadColorMap(context: TPSDReaderContext);
var
  colormaplen: longint;
begin
  with context do
  begin
    colormaplen := GetLongint(Stream);
    if colormaplen = 768 then
    begin
      // load color map
      SetLength(colormap, 3);
      Stream.Read(colormap[0][0], 768);
    end
    else
    begin
      SetLength(colormap, 0);
      Stream.Seek(colormaplen, soCurrent);
    end;
  end;
end;

procedure ReadResource(context: TPSDReaderContext; ID: word; Data: pbyte; Size: longint);
var
  resinfo: PPSDResolutionInfo;
  thumbinfo: PPSDThumbnailInfo;
  ms: TIEMemStream;
  dummyParams: TIOParamsVals;
  dummyProgress: TProgressRec;
  dummyAbort: boolean;
  ss: AnsiString;
begin
  with context do
    case ID of
      $03ED:  // Resolution information
        begin
          if Size >= sizeof(TPSDResolutionInfo) then
          begin
            resinfo := PPSDResolutionInfo(Data);
            with resinfo^ do
            begin
              hRes := IESwapDWord(hRes);
              vRes := IESwapDWord(vRes);
              IOParams.DpiX := trunc(hRes / 65536);
              IOParams.DpiY := trunc(vRes / 65536);
            end;
          end;
        end;
      $0417:  // Transparency index (Photoshop 6.0)
        begin
          transpindex := IESwapWord(pword(Data)^);
        end;
      $0404:  // IPTC-NAA
        begin
          IOParams.IPTC_Info.LoadFromStandardBuffer(Data, Size);
        end;
      $040F:  // ICC Profile (Photoshop 5.0)
        begin
          IOParams.InputICCProfile.LoadFromBuffer(Data, Size);
          hasICC := true;
        end;
      $040C:  // Thumbnail (Photoshop 5.0)
        begin
          if Size >= sizeof(TPSDThumbnailInfo) then
          begin
            thumbinfo := PPSDThumbnailInfo(Data);
            with thumbinfo^ do
            begin
              format         := IESwapDWord(format);
              width          := IESwapDWord(width);
              height         := IESwapDWord(height);
              widthbytes     := IESwapDWord(widthbytes);
              size           := IESwapDWord(size);
              compressedsize := IESwapDWord(compressedsize);
              bitspixel      := IESwapWord(bitspixel);
              planes         := IESwapWord(planes);
            end;
            inc(data, 28);
            ms := TIEMemStream.Create(Data, Size-28);
            if not assigned(IOParams.EXIF_Bitmap) then
              IOParams.EXIF_Bitmap := TIEBitmap.Create();
            dummyParams := TIOParamsVals.Create(nil);
            try
              dummyProgress.fOnProgress := nil;
              dummyProgress.Sender      := nil;
              dummyProgress.Aborting    := @dummyAbort;
              dummyAbort := false;
              ReadJPegStream(ms, nil, IOParams.EXIF_Bitmap, dummyParams, dummyProgress, false, false, false, false, true, true, -1, dummyParams.IsNativePixelFormat);
            finally
              dummyParams.Free();
              ms.free;
            end;
            if IOParams.GetThumbnail and (MergedBitmap<>nil) then
            begin
              LoadLayers := false;
              MergedBitmap.Assign( IOParams.EXIF_Bitmap );
              thumbnailLoaded := true;
            end;
          end;
        end;
      $0424:  // XMP
        begin
          SetLength( ss, Size );
          Move( Data^, ss[1], Size );
          IOParams.XMP_Info := ss;
        end;
    end;
end;

// Image resources section
procedure ReadImageResources(context: TPSDReaderContext);
var
  resourceslen: longint;
  Signature: array [0..3] of AnsiChar;
  ID: smallint;
  Name: AnsiString;
  NameLen: smallint;
  Size: longint;
  Data: array of byte;
  StreamSize: int64;
begin
  with context do
  begin
    resourceslen := GetLongint(Stream);
    StreamSize := Stream.Size;
    // load known image resources
    while resourceslen > 0 do
    begin
      if (Stream.Position and $1) <> 0 then
      begin
        Stream.Seek(1, soCurrent);
        dec(resourceslen);
        if resourceslen = 0 then
          break;
      end;
      Stream.Read(Signature[0], 4);
      dec(resourceslen, 4);
      if not CompareMem(@Signature, @RESMAGIK, 4) then
        break;
      ID := GetSmallint(Stream);
      NameLen := GetSmallint(Stream);
      SetLength(Name, NameLen);
      Stream.Read(Name[1], NameLen);
      Size := GetLongint(Stream);
      if (Size <= 0) or (Size > StreamSize) then
      begin
        dec(resourceslen, 2 + 2 + NameLen + 4);
        break;
      end;
      SetLength(Data, Size);
      Stream.Read(Data[0], Size);
      ReadResource(context, ID, @Data[0], Size);
      dec(resourceslen, 2 + 2 + NameLen + 4 + Size);
    end;
    Stream.Seek(resourceslen, soCurrent); // bypass unknown bytes
  end;
end;

type

  TAdjustment = record
    Signature: array [0..3] of AnsiChar; // always 8BIM
    Key: array [0..3] of AnsiChar;
    Length: longint;
    data: pbyte;
  end;
  TAdjustmentArray = array [0..MaxInt div 32] of TAdjustment;
  PAdjustmentArray = ^TAdjustmentArray;

  TSmallIntArray = array [0..MaxInt div 4] of SmallInt;
  TLongIntArray  = array [0..MaxInt div 8] of LongInt;

  PSmallIntArray = ^TSmallIntArray;
  PLongIntArray  = ^TLongIntArray;

  TTempLayerData = record
    ChannelID: PSmallIntArray;
    ChannelLen: PInt64Array;
    layerTop, layerLeft, layerBottom, layerRight: longint;
    numberChannels: smallint;
    BlendMode: array [0..3] of AnsiChar;
    Opacity: byte;
    Clipping: byte;
    Flags: byte;
    layermask_Size: longint;
    layermask_Top: longint;
    layermask_Left: longint;
    layermask_Bottom: longint;
    layermask_Right: longint;
    layermask_DefaultColor: byte;
    layermask_Flags: byte;
    Name: array [0..MAXLAYERNAME-1] of AnsiChar;
    adjustmentCount: integer;
    adjustment: PAdjustmentArray;
  end;
  PTempLayerData = ^TTempLayerData;
  TArrayOfTempLayerData = array [0..MaxInt div 262144] of TTempLayerData;
  PArrayOfTempLayerData = ^TArrayOfTempLayerData;


// Read layer and mask information section
procedure ReadLayerAndMaskInfo(context: TPSDReaderContext);
var
  lpos1: int64;
  layerslen: int64;
  layersinfolen: int64;
  li: longint;
  b: byte;
  layerscount: smallint;
  i, j, c, k: integer;
  compression: smallint;
  cursize: integer;
  sizes: TIEArrayOfDWord;
  width, height: integer;
  ExtraDataSize: longint;
  LayersTemp: PArrayOfTempLayerData;
  layer: TIELayer;
  globalmaskLength: longint;
  //globalmaskOverlayColorSpace: smallint;
  globalmaskColorComponents: array [0..3] of smallint;
  //globalmaskOpacity: smallint;
  //globalmaskKind: byte;
begin
  with context do
  begin
    if header.Version = 2 then
      // PSB, read as 64 bit length
      layerslen := GetInt64(Stream)
    else
      // PSD, read as 32 bit length
      layerslen := GetLongint(Stream);

    lpos1 := Stream.Position;
    
    if layerslen > 0 then
    begin

      if header.Version = 2 then
        // PSB, read as 64 bit length
        layersinfolen := GetInt64(Stream)
      else
        // PSD, read as 32 bit length
        layersinfolen := GetLongint(Stream);

      layerscount := GetSmallint(Stream);
      hasPremultipliedAlpha := layerscount < 0;
      layerscount := abs(layerscount);
      if ((layerscount > 1) and not LoadLayers) or (layers = nil) then
      begin
        // bypass layers
        if header.Version = 2 then
          // PSB, subtract 8 bytes
          Stream.Seek(layerslen - 8 - 2, soCurrent)
        else
          // PSD, subtract 4 bytes
          Stream.Seek(layerslen - 4 - 2, soCurrent);
        exit;
      end;
      LayersTemp := AllocMem( sizeof(TTempLayerData) * layerscount );
      for i := 0 to layerscount - 1 do
      begin
        // load layer parameters
        LayersTemp[i].layerTop    := GetLongint(Stream);
        LayersTemp[i].layerLeft   := GetLongint(Stream);
        LayersTemp[i].layerBottom := GetLongint(Stream);
        LayersTemp[i].layerRight  := GetLongint(Stream);
        LayersTemp[i].numberChannels := GetSmallint(Stream);
        getmem(LayersTemp[i].ChannelID, sizeof(Smallint) * LayersTemp[i].numberChannels);
        getmem(LayersTemp[i].ChannelLen, sizeof(int64) * LayersTemp[i].numberChannels);
        for c := 0 to LayersTemp[i].numberChannels - 1 do
        begin
          LayersTemp[i].ChannelID[c]  := GetSmallint(Stream);
          if header.Version = 2 then
            // PSB, read length as 64 bit
            LayersTemp[i].ChannelLen[c] := GetInt64(Stream)
          else
            // PSD, read length as 32 bit
            LayersTemp[i].ChannelLen[c] := GetLongint(Stream);
        end;
        Stream.Seek(4, soCurrent); // bypass blend mode signature (always 8BIM)
        Stream.Read(LayersTemp[i].BlendMode[0], 4);
        LayersTemp[i].Opacity  := GetByte(Stream);
        LayersTemp[i].Clipping := GetByte(Stream);
        LayersTemp[i].Flags    := GetByte(Stream);
        Stream.Seek(1, soCurrent);  // bypass filler
        ExtraDataSize := GetLongint(Stream);
        // layer mask / adjustment layer data
        LayersTemp[i].layermask_Size := GetLongint(Stream);
        if LayersTemp[i].layermask_Size > 0 then
        begin
          LayersTemp[i].layermask_Top          := GetLongint(Stream);
          LayersTemp[i].layermask_Left         := GetLongint(Stream);
          LayersTemp[i].layermask_Bottom       := GetLongint(Stream);
          LayersTemp[i].layermask_Right        := GetLongint(Stream);
          LayersTemp[i].layermask_DefaultColor := GetByte(Stream);
          LayersTemp[i].layermask_Flags := GetByte(Stream);
          Stream.Seek(2, soCurrent);  // bypass padding
        end;
        dec(ExtraDataSize, LayersTemp[i].layermask_Size + 4);
        // layer blending ranges data (bypass)
        li := GetLongint(Stream);
        dec(ExtraDataSize, li + 4);
        Stream.Seek(li, soCurrent);
        // layer name
        b := GetByte(Stream);
        Stream.Read(LayersTemp[i].Name[0], imin(b, MAXLAYERNAME - 1));
        LayersTemp[i].Name[b] := #0;
        dec(ExtraDataSize, 1 + b);
        while (ExtraDataSize and $3) <> 0 do
        begin
          Stream.Seek(1, soCurrent);  // pad to multiple of 4 bytes
          dec(ExtraDataSize);
        end;
        // adjustment layer info tags
        c := 0;
        while ExtraDataSize > 0 do
        begin
          inc(LayersTemp[i].adjustmentCount);
          ReallocMem(LayersTemp[i].adjustment, LayersTemp[i].adjustmentCount * sizeof(TAdjustment) );
          Stream.Read(LayersTemp[i].adjustment[c].Signature, 4); // Signature '8BIM'
          Stream.Read(LayersTemp[i].adjustment[c].Key, 4);       // Key
          LayersTemp[i].adjustment[c].Length := GetLongint(Stream); // length
          getmem(LayersTemp[i].adjustment[c].Data, LayersTemp[i].adjustment[c].Length);
          Stream.Read(LayersTemp[i].adjustment[c].Data^, LayersTemp[i].adjustment[c].Length);
          dec(ExtraDataSize, 4 + 4 + 4 + LayersTemp[i].adjustment[c].Length);
          inc(c);
        end;
        // bypass extradata (should not be present)
        Stream.Seek(ExtraDataSize, soCurrent);
        // fill TIELayer data
        layer := TIELayer.Create(nil, nil, false);
        layers.Add(layer);
        layer.PosX := LayersTemp[i].layerLeft;
        layer.PosY := LayersTemp[i].layerTop;
        //layer.Transparency := LayersTemp[i].Opacity;  <-- old behavior
        layer.Opacity := LayersTemp[i].Opacity / 255;
        layer.Name := LayersTemp[i].Name;
        layer.Visible := (LayersTemp[i].Flags and $2) = 0;
        layer.Cropped := (LayersTemp[i].Clipping = 0);
      end;

      XProgress.val := 0;
      XProgress.tot := 0;
      XProgress.per1 := 0.0;
      for i := 0 to layerscount - 1 do
        inc(XProgress.tot, imax(LayersTemp[i].layerBottom - LayersTemp[i].layerTop, LayersTemp[i].layermask_Bottom - LayersTemp[i].layermask_Top) * LayersTemp[i].numberChannels);

      k := 0; // index inside layers[] list
      for i := 0 to layerscount - 1 do
      begin
        // load layer image
        for c := 0 to LayersTemp[i].numberChannels - 1 do
        begin

          if (IOParams.PSD_SelectLayer <> '') and (LayersTemp[i].Name <> IOParams.PSD_SelectLayer) then
          begin
            // bypass channel (then by pass layer)
            Stream.Seek(LayersTemp[i].ChannelLen[c], soCurrent);
          end
          else
          begin

            if (IOParams.PSD_SelectLayer <> '') and (LayersTemp[i].Name = IOParams.PSD_SelectLayer) then
            begin
              // XProgress.tot is invalid now, recalculate for this layer only
              XProgress.tot := imax(LayersTemp[i].layerBottom - LayersTemp[i].layerTop, LayersTemp[i].layermask_Bottom - LayersTemp[i].layermask_Top) * LayersTemp[i].numberChannels;
            end;

            // load channel
            if LayersTemp[i].ChannelID[c] = -2 then
            begin
              // this is the layer mask
              layer := TIELayer.Create(nil, nil, false);
              layers.Insert(k + 1, layer);
              layer.PosX    := LayersTemp[i].layermask_Left;
              layer.PosY    := LayersTemp[i].layermask_Top;
              layer.IsMask  := true;
              layer.Visible := false;
              width  := LayersTemp[i].layermask_Right - LayersTemp[i].layermask_Left;
              height := LayersTemp[i].layermask_Bottom - LayersTemp[i].layermask_Top;
              compression := GetSmallint(Stream);
              SetLength(sizes, height);
              if compression = 1 then
              begin
                // RLE compression, read row lengths
                for j := 0 to height - 1 do
                  if header.Version = 2 then
                    // PSB, each row length is 32 bit
                    sizes[j] := GetDWord(Stream)
                  else
                    // PSD, each row length is 16 bit
                    sizes[j] := GetWord(Stream);
              end
              else
                // uncompressed, calculate row lengths
                for j := 0 to height - 1 do
                  sizes[j] := IEBitmapRowLen(width, header.Depth, 8);
              cursize := 0;
              ReadImageData(Stream, layer.Bitmap, width, height, 8, 1, nil, 0, compression, sizes, cursize, 0, @XProgress);
            end
            else
            begin
              // this is a channel or alpha channel
              if TIELayer(layers[k]).IsMask then
                inc(k);
              layer  := TIELayer(layers[k]);
              width  := LayersTemp[i].layerRight - LayersTemp[i].layerLeft;
              height := LayersTemp[i].layerBottom - LayersTemp[i].layerTop;
              compression := GetSmallint(Stream);
              SetLength(sizes, height);
              if compression = 1 then
              begin
                // RLE compression, read row lengths
                for j := 0 to height - 1 do
                  if header.Version = 2 then
                    // PSB, each row length is 32 bit
                    sizes[j] := GetDWord(Stream)
                  else
                    // PSD, each row length is 16 bit
                    sizes[j] := GetWord(Stream);
              end
              else
                for j := 0 to height - 1 do
                  sizes[j] := IEBitmapRowLen(width, header.Depth, 8);
              cursize := 0;
              ReadImageData(Stream, layer.Bitmap, width, height, header.Depth, header.mode, colormap, transpindex, compression, sizes, cursize, LayersTemp[i].ChannelID[c], @XProgress);
            end;
          end;
        end;
        inc(k);

      end;

      // free mem
      for i := 0 to layerscount - 1 do
      begin
        freemem(LayersTemp[i].ChannelID);
        freemem(LayersTemp[i].ChannelLen);
        for j := 0 to LayersTemp[i].adjustmentCount - 1 do
          freemem( LayersTemp[i].adjustment[j].data );
        freemem( LayersTemp[i].adjustment );
      end;
      freemem(LayersTemp);

      // remove empty layers
      i := 0;
      while i < layers.Count do
        if TIELayer(layers[i]).Bitmap.IsEmpty() then
        begin
          TIELayer(layers[i]).Free();
          layers.Delete(i);
        end
        else
          inc(i);

      // go at global layer mask info
      Stream.Position := lpos1 + layersinfolen + 4;
      globalmaskLength := GetLongint(Stream);
      if globalmaskLength > 0 then
      begin
        (*globalmaskOverlayColorSpace := *)GetSmallint(Stream);
        for i := 0 to 3 do
          globalmaskColorComponents[i] := GetSmallint(Stream);
        (*globalmaskOpacity := *)GetSmallint(Stream);
        (*globalmaskKind := *)GetByte(Stream);
        GetByte(Stream);  // by pass
      end;

      // go to at end of layer and mask info
      Stream.Position := lpos1 + layerslen;
    end;

    // remove zero size layers
    if layers <> nil then
    begin
      i := 0;
      while i < layers.Count do
        if (TIELayer(layers[i]).Bitmap.Width = 0) or (TIELayer(layers[i]).Bitmap.Height = 0) then
        begin
          TIELayer(layers[i]).Free;
          layers.Delete(i);
          if (i < layers.Count - 1) and TIELayer(layers[i]).IsMask then
          begin
            // remove also the layer mask
            TIELayer(layers[i]).Free;
            layers.Delete(i);
          end;
        end
        else
          inc(i);
    end;
  end;
end;

// blocksize: the size of block to read from Stream
// outbuf: output uncompressed buffer
procedure ReadRow(Stream: TStream; compression: word; outbuf: pbyte; sizes: TIEArrayOfDWord; var cursize: integer; maxOutbufLen: dword);
var
  i, j: dword;
  rp: byte;
  src: array of byte;
  dst: pbyte;
  blocksize: dword;
begin
  blocksize := sizes[cursize];
  case compression of
    0:  // RAW data
      begin
        Stream.Read(outbuf^, blocksize);
      end;
    1:  // RLE
      begin
        SetLength(src, blocksize);
        Stream.Read(src[0], blocksize);
        dst := outbuf;
        j := 0;
        i := 0;
        while i < blocksize do
        begin
          if (src[i] and $80) <> 0 then
          begin
            // repeater
            rp := (not src[i]) + 2;
            inc(i);
            if j + rp > maxOutbufLen then
              rp := maxOutbufLen - j;
            inc(j, rp);
            while rp > 0 do
            begin
              dst^ := src[i];
              inc(dst);
              dec(rp);
            end;
            inc(i);
          end
          else
          begin
            // copy
            rp := src[i] + 1;
            inc(i);
            if j + rp > maxOutbufLen then
              rp := maxOutbufLen - j;
            inc(j, rp);
            while rp > 0 do
            begin
              dst^ := src[i];
              inc(dst);
              inc(i);
              dec(rp);
            end;
          end;
        end;
      end;
  end;
  {
  according to Adobe specifiations row lengths should not be odd, but this seems in conflict with actual files
  if (blocksize and $1) <> 0 then
    Stream.Seek(1, soCurrent);
  }
  inc(cursize);
end;

procedure CopyRow8(bitmap: TIEBitmap; row: integer; channel: integer; channelcount: integer; width: integer; inrow: pbyte);
var
  pb1, pb2: pbyte;
  i: integer;
begin
  pb1 := Bitmap.Scanline[row];
  inc(pb1, channel);
  pb2 := inrow;
  for i := 0 to width - 1 do
  begin
    pb1^ := pb2^;
    inc(pb1, channelcount);
    inc(pb2);
  end;
end;

procedure CopyRow16(bitmap: TIEBitmap; row: integer; channel: integer; channelcount: integer; width: integer; inrow: pbyte);
var
  pw1, pw2: pword;
  i: integer;
begin
  pw1 := Bitmap.Scanline[row];
  inc(pw1, channel);
  pw2 := pword(inrow);
  for i := 0 to width - 1 do
  begin
    pw1^ := IESwapWord(pw2^);
    inc(pw1, channelcount);
    inc(pw2);
  end;
end;

procedure CopyRow16to8(bitmap: TIEBitmap; row: integer; channel: integer; channelcount: integer; width: integer; inrow: pbyte);
var
  pb1: pbyte;
  pw2: pword;
  i: integer;
begin
  pb1 := Bitmap.Scanline[row];
  inc(pb1, channel);
  pw2 := pword(inrow);
  for i := 0 to width - 1 do
  begin
    pb1^ := pw2^ and $FF; // it is the same of "pb1^ := IESwapWord(pw2^) shr 8"
    inc(pb1, channelcount);
    inc(pw2);
  end;
end;

procedure AdjustLab8(bitmap: TIEBitmap; row: integer; channel: integer; channelcount: integer; width: integer);
var
  p: pshortint;
  i: integer;
begin
  p := Bitmap.Scanline[row];
  inc(p, channel);
  for i := 0 to width - 1 do
  begin
    p^ := pbyte(p)^ - 128;
    inc(p, channelcount);
  end;
end;

procedure AdjustPaletteTransp(Bitmap: TIEBitmap; row: integer; transpindex: integer; width: integer);
var
  i: integer;
  p, a: pbyte;
begin
  if transpindex > -1 then
  begin
    p := Bitmap.Scanline[row];
    a := Bitmap.AlphaChannel.Scanline[row];
    for i := 0 to width - 1 do
    begin
      if p^ = transpindex then
        a^ := 0
      else
        a^ := 255;
      inc(p);
      inc(a);
    end;
  end;
end;

procedure LoadAlpha(Stream: TStream; AlphaBitmap: TIEBitmap; width, height, depth: integer; compression: integer; sizes: TIEArrayOfDWord; var cursize: integer; rowbuf: pbyte; maxRowbufLen: dword);
var
  i: integer;
begin
  for i := 0 to height - 1 do
  begin
    ReadRow(Stream, compression, rowbuf, sizes, cursize, maxRowbufLen);
    case depth of
      8:  CopyRow8(AlphaBitmap, i, 0, 1, width, rowbuf);
      16: CopyRow16to8(AlphaBitmap, i, 0, 1, width, rowbuf);
    end;
  end;
  AlphaBitmap.Full := false;
end;


// read image data section
procedure ReadImageData(Stream: TStream; Bitmap: TIEBitmap; width, height, depth, mode: integer; colormap: TColorMap; transpindex: integer; compression: smallint; sizes: TIEArrayOfDWord; var cursize: integer; channel: integer; progress: PProgressRec);

  procedure DoProgress();
  begin
    if assigned(progress) and assigned(progress^.fOnProgress) then
    begin
      progress^.per2 := progress^.val / progress^.tot * 100;
      if trunc(progress^.per1) <> trunc(progress^.per2) then
      begin
        progress^.per1 := progress^.per2;
        progress^.fOnProgress(progress^.Sender, trunc(progress^.per1));
      end;
      inc(progress^.val);
    end;
  end;

var
  rowbuf: TIEArrayOfByte;
  i: integer;
begin

  SetLength(rowbuf, IEBitmapRowLen(width, depth, 8));

  case depth of

    1:  // depth = 1
      case mode of
        0:  // bitmap
          begin
            Bitmap.Allocate(width, height, ie1g);
            if channel = 0 then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow8(Bitmap, i, 0, 1, width div 8, @rowbuf[0]);
                _NegativeBuffer(Bitmap.Scanline[i], width div 8);
                DoProgress();
              end;
          end;
      end;

    8:  // depth = 8
      case mode of
        1, 8, 7:  // Gray Scale or duotone or multichannel (not very well supported)
          begin
            Bitmap.Allocate(width, height, ie8g);
            if channel = 0 then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow8(Bitmap, i, 0, 1, width, @rowbuf[0]);
                DoProgress();
              end;
            if (channel = 1) or (channel = -1) then
              LoadAlpha(Stream, Bitmap.AlphaChannel, width, height, depth, compression, sizes, cursize, @rowbuf[0], length(rowbuf));
          end;
        2:  // Indexed
          begin
            Bitmap.Allocate(width, height, ie8p);
            if channel = 0 then
            begin
              for i := 0 to 255 do
                Bitmap.Palette[i] := CreateRGB(colormap[0][i], colormap[1][i], colormap[2][i]);
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow8(Bitmap, i, 0, 1, width, @rowbuf[0]);
                AdjustPaletteTransp(Bitmap, i, transpindex, width);
                DoProgress();
              end;
            end;
          end;
        3:  // RGB
          begin
            Bitmap.Allocate(width, height, ie24RGB);
            if (channel >= 0) and (channel < 3) then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow8(Bitmap, i, 2 - channel, 3, width, @rowbuf[0]);
                DoProgress();
              end;
            if (channel = 3) or (channel = -1) then
              LoadAlpha(Stream, Bitmap.AlphaChannel, width, height, depth, compression, sizes, cursize, @rowbuf[0], length(rowbuf));
          end;
        4:  // CMYK
          begin
            Bitmap.Allocate(width, height, ieCMYK);
            if (channel >= 0) and (channel < 4) then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow8(Bitmap, i, channel, 4, width, @rowbuf[0]);
                DoProgress();
              end;
            if (channel = 4) or (channel = -1) then
              LoadAlpha(Stream, Bitmap.AlphaChannel, width, height, depth, compression, sizes, cursize, @rowbuf[0], length(rowbuf));
          end;
        9:  // Lab
          begin
            Bitmap.Allocate(width, height, ieCIELab);
            if (channel >= 0) and (channel < 3) then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow8(Bitmap, i, channel, 3, width, @rowbuf[0]);
                if (channel = 1) or (channel = 2) then
                  AdjustLab8(Bitmap, i, channel, 3, width);
                DoProgress();
              end;
            if (channel = 3) or (channel = -1) then
              LoadAlpha(Stream, Bitmap.AlphaChannel, width, height, depth, compression, sizes, cursize, @rowbuf[0], length(rowbuf));
          end;
      end;

    16: // depth = 16
      case mode of
        1, 8:  // Gray Scale or duotone
          begin
            Bitmap.Allocate(width, height, ie16g);
            if channel = 0 then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow16(Bitmap, i, 0, 1, width, @rowbuf[0]);
                DoProgress();
              end;
            if (channel = 1) or (channel = -1) then
              LoadAlpha(Stream, Bitmap.AlphaChannel, width, height, depth, compression, sizes, cursize, @rowbuf[0], length(rowbuf));
          end;
        3:  // RGB
          begin
            Bitmap.Allocate(width, height, ie48RGB);
            if (channel >= 0) and (channel < 3) then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow16(Bitmap, i, channel, 3, width, @rowbuf[0]);
                DoProgress();
              end;
            if (channel = 3) or (channel = -1) then
              LoadAlpha(Stream, Bitmap.AlphaChannel, width, height, depth, compression, sizes, cursize, @rowbuf[0], length(rowbuf));
          end;
        4:  // CMYK
          begin
            Bitmap.Allocate(width, height, ieCMYK);
            if (channel >= 0) and (channel < 4) then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow16to8(Bitmap, i, channel, 4, width, @rowbuf[0]);
                DoProgress();
              end;
            if (channel = 4) or (channel = -1) then
              LoadAlpha(Stream, Bitmap.AlphaChannel, width, height, depth, compression, sizes, cursize, @rowbuf[0], length(rowbuf));
          end;
        9:  // Lab
          begin
            Bitmap.Allocate(width, height, ieCIELab);
            if (channel >= 0) and (channel < 3) then
              for i := 0 to height - 1 do
              begin
                ReadRow(Stream, compression, @rowbuf[0], sizes, cursize, length(rowbuf));
                CopyRow16to8(Bitmap, i, channel, 3, width, @rowbuf[0]);
                if (channel = 1) or (channel = 2) then
                  AdjustLab8(Bitmap, i, channel, 3, width);
                DoProgress();
              end;
            if (channel = 3) or (channel = -1) then
              LoadAlpha(Stream, Bitmap.AlphaChannel, width, height, depth, compression, sizes, cursize, @rowbuf[0], length(rowbuf));
          end;
      end;
  end;
end;

// output is a list of TIELayer (output must be a created list)
// note: to do "Preview" set boths LoadLayers and LoadMergedImage to False
procedure IEReadPSD(Stream: TStream; MergedBitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; LoadLayers: boolean; layers: TList);
var
  context: TPSDReaderContext;
  compression: smallint;
  cursize: integer;
  sizes: TIEArrayOfDWord;
  i: integer;
begin

  context := TPSDReaderContext.Create();

  try

    context.Stream       := Stream;
    context.IOParams     := IOParams;
    context.Layers       := layers;
    context.LoadLayers   := LoadLayers;
    context.XProgress    := Progress;
    context.MergedBitmap := MergedBitmap;
    context.hasICC       := false;

    with context do
    begin

      thumbnailLoaded := false;

      if assigned(XProgress.fOnProgress) then
        XProgress.fOnProgress(XProgress.Sender, 0);

      Stream.Read(header, sizeof(TPSDHeader));
      with header do
      begin
        Version  := IESwapWord(Version);
        Channels := IESwapWord(Channels);
        Rows     := IESwapDWord(Rows);
        Columns  := IESwapDWord(Columns);
        Depth    := IESwapWord(Depth);
        Mode     := IESwapWord(Mode);
        if not CompareMem(@Signature, @MAGIK, 4) or ((Version <> 1) and (Version <> 2)) or (Channels < 1) or (Channels > 24) or (Depth < 1) or (Depth > 16) or (Mode > 9) then
        begin
          Progress.Aborting^ := true;
          exit;
        end;
      end;

      if assigned(MergedBitmap) and MergedBitmap.EncapsulatedFromTBitmap then
        MergedBitmap := TIEBitmap.Create;

      IOParams.ImageCount      := 1;
      IOParams.Width           := header.Columns;
      IOParams.Height          := header.Rows;
      IOParams.OriginalWidth   := header.Columns;
      IOParams.OriginalHeight  := header.Rows;
      IOParams.BitsPerSample   := header.Depth;
      IOParams.SamplesPerPixel := header.Channels;
      IOParams.DpiX            := 96;  // waiting for actual parameters
      IOParams.DpiY            := 96;  // waiting for actual parameters

      // Read Color mode data section
      ReadColorMap(context);

      // Read Image resources section
      transpindex := -1;  // no transp index
      ReadImageResources(context);

      // Read Layer and mask information section

      ReadLayerAndMaskInfo(context);

      // Read image data section (merged image)
      if ((not LoadLayers) or (layers = nil) or (layers.Count = 0)) and assigned(MergedBitmap) and (not IOParams.GetThumbnail or not thumbnailLoaded) and (IOParams.PSD_SelectLayer = '') then
      begin
        compression := GetSmallint(Stream);
        SetLength(sizes, header.Channels * header.Rows);
        if compression = 1 then
        begin
          // RLE compression, read row lengths
          for i := 0 to header.Channels * header.Rows - 1 do
            if header.Version = 2 then
              // PSB, each row length is 32 bit
              sizes[i] := GetDWord(Stream)
            else
              // PSD, each row length is 16 bit
              sizes[i] := GetWord(Stream);
        end
        else
          // uncompressed, calculate row lengths
          for i := 0 to header.Channels * header.Rows - 1 do
            sizes[i] := IEBitmapRowLen(header.Columns, header.Depth, 8);
        cursize := 0;
        Progress.val := 0;
        Progress.tot := header.Channels * header.Rows;
        Progress.per1 := 0.0;
        for i := 0 to header.Channels - 1 do
          ReadImageData(Stream, MergedBitmap, header.Columns, header.Rows, header.Depth, header.mode, colormap, transpindex, compression, sizes, cursize, i, @Progress);
      end;

      // process NativePixelFormat
      if (layers <> nil) then
      begin
        for i := 0 to layers.Count - 1 do
          with TIELayer(layers[i]) do
          begin
            if (Bitmap.PixelFormat <> ie1g) and not IsMask then
            begin
              if IOParams.IsNativePixelFormat then
              begin
                if assigned(IOParams) and assigned(IOParams.InputICCProfile) then
                  Bitmap.ColorProfile.Assign(IOParams.InputICCProfile);
              end
              else
              begin
                if hasICC and
                   IEGlobalSettings().EnableCMS and
                   (IOParams <> nil) and
                   assigned(IOParams.InputICCProfile) and
                   IOParams.InputICCProfile.IsValid and
                   not IOParams.InputICCProfile.IsApplied and
                   IOParams.OutputICCProfile.IsValid then
                begin
                  IOParams.InputICCProfile.ConvertBitmap(Bitmap, ie24RGB, IOParams.OutputICCProfile);
                end
                else
                begin
                  Bitmap.PixelFormat := ie24RGB;
                end;
              end;
            end;
          end;
      end;
      if assigned(MergedBitmap) then
        if (MergedBitmap.PixelFormat <> ie1g) then
        begin
          if IOParams.IsNativePixelFormat then
          begin
            if assigned(IOParams) and assigned(IOParams.InputICCProfile) then
              MergedBitmap.ColorProfile.Assign(IOParams.InputICCProfile);
          end
          else
          begin
            if hasICC and
               IEGlobalSettings().EnableCMS and
               (IOParams <> nil) and
               assigned(IOParams.InputICCProfile) and
               IOParams.InputICCProfile.IsValid and
               not IOParams.InputICCProfile.IsApplied and
               IOParams.OutputICCProfile.IsValid then
            begin
              IOParams.InputICCProfile.ConvertBitmap(MergedBitmap, ie24RGB, IOParams.OutputICCProfile);
            end
            else
            begin
              MergedBitmap.PixelFormat := ie24RGB;
            end;
          end;
        end;
    end;

    if context.MergedBitmap <> MergedBitmap then
    begin
      MergedBitmap.Assign(context.MergedBitmap);
      FreeAndNil(context.MergedBitmap);
    end;

  finally
    context.Free();
  end;

end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PSD Write

type

TPSDWriterContext = record
  Stream: TStream;
  IOParams: TIOParamsVals;
  layers: TList;
  mergedImage: TIEBitmap;
  mode: integer;
  depth: integer;
  Progress: TProgressRec;
end;


const
  PIXELFORMAT2MODE: array [TIEPixelFormat] of integer = (1000, 0, 2, 1, 1, 3, 1000, 4, 3, 9, 3);


// put 16 bit signed value
procedure PutSmallint(Stream: TStream; value: smallint);
begin
  value := IESwapWord(value);
  Stream.Write(value, 2);
end;

// put 32 bit signed value
procedure PutLongint(Stream: TStream; value: longint);
begin
  value := IESwapDWord(value);
  Stream.Write(value, 4);
end;

// put 64 bit signed value
procedure PutInt64(Stream: TStream; value: int64);
begin
  value := IESwapInt64(value);
  Stream.Write(value, 8);
end;

procedure PutByte(Stream: TStream; value: byte);
begin
  Stream.Write(value, 1);
end;

// put 16 bit signed value at specified position, restoring the previous one when exits
procedure PutSmallintAt(Stream: TStream; position: int64; value: smallint);
var
  prev: int64;
begin
  prev := Stream.Position;
  Stream.Position := position;
  value := IESwapWord(value);
  Stream.Write(value, 2);
  Stream.Position := prev;
end;

// put 32 bit signed value at specified position, restoring the previous one when exits
procedure PutLongintAt(Stream: TStream; position: int64; value: longint);
var
  prev: int64;
begin
  prev := Stream.Position;
  Stream.Position := position;
  value := IESwapDWord(value);
  Stream.Write(value, 4);
  Stream.Position := prev;
end;

// put 64 bit signed value at specified position, restoring the previous one when exits
procedure PutInt64At(Stream: TStream; position: int64; value: int64);
var
  prev: int64;
begin
  prev := Stream.Position;
  Stream.Position := position;
  value := IESwapInt64(value);
  Stream.Write(value, 8);
  Stream.Position := prev;
end;

procedure WriteAt(Stream: TStream; position: int64; const value; len: integer);
var
  prev: int64;
begin
  prev := Stream.Position;
  Stream.Position := position;
  Stream.Write(value, len);
  Stream.Position := prev;
end;


procedure WriteColorMap(var context: TPSDWriterContext);
var
  colormap: TColorMap;
  i: integer;
begin
  with context do
  begin

    if mergedImage.PixelFormat = ie8p then
    begin
      // 256 values palette
      PutLongint(Stream, 768);
      SetLength(colormap, 3);
      for i := 0 to 255 do
      begin
        colormap[0][i] := mergedImage.Palette[i].r;
        colormap[1][i] := mergedImage.Palette[i].g;
        colormap[2][i] := mergedImage.Palette[i].b;
      end;
      Stream.Write(colormap[0][0], 768);
    end
    else
    begin
      // empty section
      PutLongint(Stream, 0);
    end;

  end;
end;

procedure WriteResource(Stream: TStream; ID: smallint; name: AnsiString; data: pointer; size: integer);
begin
  // align position
  if (Stream.Position and $1) <> 0 then
    PutByte(Stream, 0);
  // 8BIM
  Stream.Write(RESMAGIK[0], 4);
  // ID
  PutSmallint(Stream, ID);
  // name
  PutSmallint(Stream, length(name));
  Stream.Write(name[1], length(name));
  // data
  PutLongint(Stream, size);
  Stream.Write(pbyte(data)^, size);
end;

procedure WriteThumbnailToBuffer(var context: TPSDWriterContext; var buf: pointer; var buflen: integer);
var
  ms: TMemoryStream;
  thumbinfo: TPSDThumbnailInfo;
  dummyParams: TIOParamsVals;
  dummyProgress: TProgressRec;
  dummyAbort: boolean;
begin
  with context do
  begin
    ms := TMemoryStream.Create();
    // write empty header (because compressedsize is still not available)
    FillChar(thumbinfo, sizeof(TPSDThumbnailInfo), 0);
    ms.Write(thumbinfo, sizeof(TPSDThumbnailInfo));

    // write jpeg image
    dummyParams := TIOParamsVals.Create(nil);
    dummyAbort := false;
    dummyProgress.fOnProgress := nil;
    dummyProgress.Sender := nil;
    dummyProgress.Aborting := @dummyAbort;
    WriteJpegStream(ms, IOParams.EXIF_Bitmap, dummyParams, dummyProgress);
    dummyParams.Free();

    // write actual header
    ms.Position := 0;
    thumbinfo.format     := IESwapDWord(1);
    thumbinfo.width      := IESwapDWord(IOParams.EXIF_Bitmap.Width);
    thumbinfo.height     := IESwapDWord(IOParams.EXIF_Bitmap.Height);
    thumbinfo.widthbytes := IESwapDWord(IOParams.EXIF_Bitmap.Width * 3);
    thumbinfo.size       := IESwapDWord(IOParams.EXIF_Bitmap.Width * IOParams.EXIF_Bitmap.Height * 3);
    thumbinfo.compressedsize := ms.Size - sizeof(TPSDThumbnailInfo);
    thumbinfo.bitspixel  := 24;
    thumbinfo.planes     := 1;
    ms.Write(thumbinfo, sizeof(TPSDThumbnailInfo));

    // copy memory stream to memory buffer
    buflen := ms.Size;
    getmem(buf, buflen);
    copymemory(buf, ms.Memory, buflen);
    ms.Free();
  end;
end;

procedure WriteImageResources(var context: TPSDWriterContext);
var
  resinfo: TPSDResolutionInfo;
  ww: word;
  buf: pointer;
  buflen: integer;
  sizepos: int64;
begin
  with context do
  begin
    sizepos := Stream.Position;
    PutLongint(Stream, 0); // image resources size (now zero)

    // $03ED - Resolution information
    resinfo.hRes       := IESwapDWord(IOParams.DpiX * 65536);
    resinfo.vRes       := IESwapDWord(IOParams.DpiY * 65536);
    resinfo.hResUnit   := IESwapWord(1);    // 1=pixels per inch
    resinfo.vResUnit   := IESwapWord(1);    // 1=pixels per inch
    resinfo.WidthUnit  := IESwapWord(1);    // 1=in
    resinfo.HeightUnit := IESwapWord(1);    // 1=in
    WriteResource(Stream, $03ED, '', @resinfo, sizeof(TPSDResolutionInfo));

    // $0417 - Transparency index (Photoshop 6.0)
    // We uses index 255 for alpha channel. Colors that has the same index should be reindex to another similar index color.
    if mergedImage.PixelFormat = ie8p then
    begin
      ww := IESwapWord(255);
      WriteResource(Stream, $0417, '', @ww, sizeof(word));
    end;

    // $0404 - IPTC NAA
    IOParams.IPTC_Info.SaveToStandardBuffer(buf, buflen, false);
    try
      if buflen > 0 then
        WriteResource(Stream, $0404, '', buf, buflen);
    finally
      freemem(buf);
    end;

    // $040F - ICC Profile (Photoshop 5.0)
    if IOParams.InputICCProfile.RawLength > 0 then
      WriteResource(Stream, $040F, '', IOParams.InputICCProfile.Raw, IOParams.InputICCProfile.RawLength);

    // $040C - Thumbnail (Photoshop 5.0)
    if (IOParams.EXIF_Bitmap <> nil) and not IOParams.EXIF_Bitmap.IsEmpty then
    begin
      WriteThumbnailToBuffer(context, buf, buflen);
      try
        WriteResource(Stream, $040C, '', buf, buflen);
      finally
        freemem(buf);
      end;
    end;

    // $0424 - XMP
    if IOParams.XMP_Info <> '' then
      WriteResource(Stream, $0424, '', PAnsiChar(IOParams.XMP_Info), length(IOParams.XMP_Info)); // 3.0.1

    // align position
    if (Stream.Position and $1) <> 0 then
      PutByte(Stream, 0);

    PutLongintAt(Stream, sizepos, Stream.Size - sizepos - 4);

  end;
end;

// outData must be already allocated (inLen*3)
procedure CompressBytes(inData: pbytearray; inLen: integer; outData: pbyte; var outLen: integer);
var
  n, rl: integer;
  si: shortint;
  bp: integer;
  procedure SavB();
  var
    qq: integer;
  begin
    // writes absolute bytes from bp to n-1
    qq := n - bp;
    if qq > 0 then
    begin
      // more bytes
      si := qq - 1;
      outData^ := si; inc(outData);                      // SafeStreamWrite(Stream, Aborting, si, 1);
      move(inData[bp], outData^, qq); inc(outData, qq);  // SafeStreamWrite(Stream, Aborting, pbyte(@inData[bp])^, qq);
      inc(outLen, qq + 1);
    end;
  end;
begin
  outLen := 0;
  n := 0; // n is the initial position of the first group to compress
  bp := 0;
  while n < inLen do
  begin
    // look for equal bytes
    rl := 1;
    while ((n + rl) < inLen) and (inData[n] = inData[n + rl]) and (rl < 128) do
      inc(rl);
    if rl > 3 then
    begin
      SavB(); // write absolute bytes from bp to n-1
      // replicates bytes
      si := -1 * (rl - 1);
      outData^ := si; inc(outData);         // SafeStreamWrite(Stream, Aborting, si, 1);
      outData^ := inData[n]; inc(outData);  // SafeStreamWrite(Stream, Aborting, inData[n], 1);
      inc(outLen, 2);
      inc(n, rl);
      bp := n;
    end
    else
    if (n - bp) = 128 then
    begin
      SavB();
      bp := n;
    end
    else
      inc(n);
  end;
  SavB(); // writes absolute bytes from bp to n-1
end;

type
  // specify saved sizes for each channel for each layer
  TSizes = array [0..10000000] of array [0..5] of int64;
  PSizes = ^TSizes;

// sizes can be null (in this case sizesIdx is unused)
// layerMask can be null
// if sizes is null supposes we are writing merged
// if we are writing merged and compressed then all channels lengths are grouped, while writing layers they are separated for each channel
procedure WritePixelData(var context: TPSDWriterContext; bitmap: TIEBitmap; layerMask: TIEBitmap; sizes: PSizes; sizesIdx: integer; doprogress: boolean);
var
  row, col: integer;
  pb, rb: pbyte;
  pw, wb: pword;
  pos1, pos2: int64;
  i, k: integer;
  rl: integer;
  channelCount: integer;
  bitmapWidth, bitmapHeight: integer;
  bitmapPixelFormat: TIEPixelFormat;
  rowbuf, cmpbuf: pbyte;
  actlen, cmplen: integer;

  procedure WriteRow();
  begin
    with context do
    begin
      CompressBytes(pbytearray(rowbuf), actlen, cmpbuf, cmplen);
      Stream.Write(cmpbuf^, cmplen);

      // write row length
      if IOParams.PSD_LargeDocumentFormat then
      begin
        // PSB, 32 bit row length
        if sizes <> nil then
          PutLongintAt(Stream, pos2 + row * 4, cmplen)
        else
          PutLongintAt(Stream, pos2 + i * bitmapHeight * 4 + row * 4, cmplen);
      end
      else
      begin
        // PSD, 16 bit row length
        if sizes <> nil then
          PutSmallintAt(Stream, pos2 + row * 2, cmplen)
        else
          PutSmallintAt(Stream, pos2 + i * bitmapHeight * 2 + row * 2, cmplen);
      end;
    end;
  end;

begin
  with context do
  begin

    rl := IEBitmapRowLen(Bitmap.Width, depth, 8); // output rowlen
    getmem(rowbuf, Bitmap.Width * 4);
    getmem(cmpbuf, Bitmap.Width * 4);

    try

      k := 0;

      if (sizes <> nil) and bitmap.HasAlphaChannel then
      begin
        // put alpha channel
        pos1 := Stream.Position;
        PutSmallint(Stream, 1);  // compression

        // put blank row lengths
        pos2 := Stream.Position;
        if IOParams.PSD_LargeDocumentFormat then
          // PSB, 32 bit row length
          Stream.Seek(Bitmap.AlphaChannel.Height * 4, soCurrent)
        else
          // PSD, 16 bit row length
          Stream.Seek(Bitmap.AlphaChannel.Height * 2, soCurrent);
        
        // write values
        for row := 0 to Bitmap.AlphaChannel.Height - 1 do
        begin
          pb := Bitmap.AlphaChannel.Scanline[row];
          actlen := 0;
          case depth of
            8:
              begin
                actlen := Bitmap.AlphaChannel.Width;
                move(pb^, rowbuf^, actlen);
              end;
            16:
              begin
                wb := pword(rowbuf);
                for col := 0 to Bitmap.AlphaChannel.Width - 1 do
                begin
                  wb^ := IESwapWord(pb^ * 257);
                  inc(wb);
                  inc(pb);
                  inc(actlen, 2);
                end;
              end;
          end;
          WriteRow();
        end;

        // write channel size
        if IOParams.PSD_LargeDocumentFormat then
          // PSB, 64 bit size
          PutInt64At(Stream, sizes[sizesIdx][k], Stream.Size - pos1)   // included compression tag
        else
          // PSD, 32 bit size
          PutLongintAt(Stream, sizes[sizesIdx][k], Stream.Size - pos1);  // included compression tag

        inc(k);
      end;

      // put color channels
      channelCount      := bitmap.ChannelCount;
      bitmapWidth       := bitmap.Width;
      bitmapHeight      := bitmap.Height;
      bitmapPixelFormat := bitmap.PixelFormat;
      if sizes = nil then
      begin
        PutSmallint(Stream, 1);  // compression (for merged, out of channels loop)

        // put blank row lengths
        pos2 := Stream.Position;
        if IOParams.PSD_LargeDocumentFormat then
          // PSB, 32 bit row length
          Stream.Seek(BitmapHeight * channelCount * 4, soCurrent)
        else
          // PSD, 16 bit row length
          Stream.Seek(BitmapHeight * channelCount * 2, soCurrent);
      end;
      for i := 0 to channelCount - 1 do
      begin
        pos1 := Stream.Position;
        if sizes <> nil then
        begin
          PutSmallint(Stream, 1);  // compression (for layers)

          // put blank row lengths
          pos2 := Stream.Position;
          if IOParams.PSD_LargeDocumentFormat then
            // PSB, 32 bit row length
            Stream.Seek(BitmapHeight * 4, soCurrent)
          else
            // PSd, 16 bit row length
            Stream.Seek(BitmapHeight * 2, soCurrent);
        end;
        for row := 0 to bitmapHeight - 1 do
        begin
          pb := Bitmap.Scanline[row];
          pw := pword(pb);
          if bitmapPixelFormat = ie24RGB then
            inc(pb, 2 - i)
          else
            inc(pb, i);
          inc(pw, i);
          case depth of
            1:
              begin
                actlen := rl;
                move(pb^, rowbuf^, actlen);
              end;
            8:
              begin
                rb := rowbuf;
                for col := 0 to bitmapWidth - 1 do
                begin
                  rb^ := pb^;
                  inc(pb, channelCount);
                  inc(rb);
                end;
                actlen := rl;
              end;
            16:
              begin
                wb := pword(rowbuf);
                for col := 0 to bitmapWidth - 1 do
                begin
                  wb^ := IESwapWord(pw^);
                  inc(pw, channelCount);
                  inc(wb);
                end;
                actlen := rl;
              end;
          end;
          WriteRow();
        end;
        // write channel size
        if sizes <> nil then
        begin
          if IOParams.PSD_LargeDocumentFormat then
            // PSB, 64 bit size
            PutInt64At(Stream, sizes[sizesIdx][k], Stream.Size - pos1)  // included compression tag
          else
            // PSD, 32 bit size
            PutLongintAt(Stream, sizes[sizesIdx][k], Stream.Size - pos1);   // included compression tag
        end;

        inc(k);
        if doProgress and assigned(Progress.fOnProgress) then
          Progress.fOnProgress(Progress.Sender, trunc(i / channelCount * 100));
      end;

      // put layer mask (always 8 bit?)
      if layerMask <> nil then
      begin
        pos1 := Stream.Position;
        PutSmallint(Stream, 1);  // compression

        // put blank row lengths
        pos2 := Stream.Position;
        if IOParams.PSD_LargeDocumentFormat then
          // PSB, 32 bit row length
          Stream.Seek(layerMask.Height * 4, soCurrent)
        else
          // PSD, 16 bit row length
          Stream.Seek(layerMask.Height * 2, soCurrent);

        // write values
        for row := 0 to layerMask.Height - 1 do
        begin
          pb := layerMask.Scanline[row];
          actlen := layerMask.Width;
          move(pb^, rowbuf^, actlen);
          WriteRow();
        end;
        
        // write channel size
        if sizes <> nil then
        begin
          if IOParams.PSD_LargeDocumentFormat then
            // PSB, 64 bit size
            PutInt64At(Stream, sizes[sizesIdx][k], Stream.Size - pos1)   // included compression tag
          else
            // PSD, 32 bit size
            PutLongintAt(Stream, sizes[sizesIdx][k], Stream.Size - pos1);  // included compression tag
        end;
      end;

    finally
      freemem(cmpbuf);
      freemem(rowbuf);
    end;

  end;  // end of with context
end;


procedure WriteLayers(var context: TPSDWriterContext);
var
  sizepos: int64;
  i, j, k: integer;
  lyr, msk: TIELayer;
  sizes: PSizes;  // array of lsizes structure to store channels sizes for each layer
  layermaskcount: integer; // number of layer masks
  pos1: int64;
  extradatasize: integer;
  chcount: integer;
  tempLayer: TIELayer; // used if layers.Count=0
begin
  with context do
  begin
    sizepos := Stream.Position;

    // dummy size value
    if IOParams.PSD_LargeDocumentFormat then
      // PSB, 64 bit size
      PutInt64(Stream, 0)
    else
      // PSB, 32 bit size
      PutLongint(Stream, 0);

    if layers.Count = 0 then
    begin
      tempLayer := TIELayer.Create(nil, MergedImage, true);
      tempLayer.FreeBitmapOnDestroy := false;
      layers.Add(tempLayer);
    end
    else
      tempLayer := nil;

    // count how much layer mask are present
    layermaskcount := 0;
    for i := 0 to layers.Count - 1 do
      if TIELayer(layers[i]).IsMask then
        inc(layermaskcount);

    PutSmallint(Stream, layers.Count - layermaskcount); // layers count

    getmem(sizes, sizeof(int64) * 6 * layers.Count);

    try

      for i := 0 to layers.Count - 1 do
      begin
        lyr := TIELayer(layers[i]);

        if not lyr.IsMask then
        begin

          PutLongint(Stream, lyr.PosY); // Layer top
          PutLongint(Stream, lyr.PosX); // Layer left
          PutLongint(Stream, lyr.PosY + lyr.Bitmap.Height);   // Layer bottom
          PutLongint(Stream, lyr.PosX+lyr.Bitmap.Width);      // Layer right

          // channels count
          chcount := lyr.Bitmap.ChannelCount;
          if lyr.Bitmap.HasAlphaChannel then
            inc(chcount);
          if (i < layers.Count - 1) and (TIELayer(layers[i + 1]).IsMask) then
            inc(chcount);
          PutSmallint(Stream, chcount);

          // channel length info
          k := 0;
          if lyr.Bitmap.HasAlphaChannel then
          begin
            // transparency mask
            PutSmallint(Stream, -1);  // -1 = transparency mask
            sizes[i][k] := Stream.Position;
            inc(k);
            // dummy size
            if IOParams.PSD_LargeDocumentFormat then
              // PSB
              PutInt64(Stream, 0)
            else
              // PSD
              PutLongint(Stream, 0);
          end;
          for j := 0 to lyr.Bitmap.ChannelCount - 1 do
          begin
            // color channels
            PutSmallint(Stream, j);
            sizes[i][k] := Stream.Position;
            inc(k);
            // dummy size
            if IOParams.PSD_LargeDocumentFormat then
              // PSB
              PutInt64(Stream, 0)
            else
              // PSD
              PutLongint(Stream, 0);
          end;
          if (i < layers.Count - 1) and (TIELayer(layers[i + 1]).IsMask) then
          begin
            // layer mask
            PutSmallint(Stream, -2);
            sizes[i][k] := Stream.Position;
            //inc(k);
            // dummy size
            if IOParams.PSD_LargeDocumentFormat then
              // PSB
              PutInt64(Stream, 0)
            else
              // PSD
              PutLongint(Stream, 0);
          end;

          Stream.Write(AnsiString('8BIM'), 4); // Blend mode signature
          Stream.Write(AnsiString('norm'), 4); // temporary: blend mode key

          // Opacity
          //PutByte(Stream, lyr.Transparency);  <-- old behavior
          PutByte(Stream, trunc(lyr.Opacity * 255));

          //PutByte(Stream, integer(not lyr.Cropped));  // Clipping
          PutByte(Stream, 0);  // it seems that photoshop doesn't support "1"!

          // Flags
          if lyr.Visible then
            PutByte(Stream, 0)
          else
            PutByte(Stream, $2);

          PutByte(Stream, 0);  // Filler

          // extra data size
          pos1 := Stream.Position;
          PutLongint(Stream, 0); // Extra data size (dummy value)
          extradatasize := 0;

          // Layer mask data
          inc(extradatasize, 4);
          if (i < layers.Count - 1) and (TIELayer(layers[i + 1]).IsMask) then
          begin
            PutLongint(Stream, 20); // Layer mask data size
            msk := TIELayer(layers[i + 1]);
            PutLongint(Stream, msk.PosY); // top
            PutLongint(Stream, msk.PosX); // left
            PutLongint(Stream, msk.PosY + msk.Bitmap.Height);   // bottom
            PutLongint(Stream, msk.PosX + msk.Bitmap.Width);    // right
            PutByte(Stream, 0); // default color
            PutByte(Stream, 0); // flags
            PutSmallint(Stream, 0); // padding (zeros)
            inc(extradatasize, 20);
          end
          else
            PutLongint(Stream, 0); // Layer mask data size (no layer mask)

          // Layer blending ranges (not used)
          PutLongint(Stream, 0); // zero size
          inc(extradatasize, 4);

          // Layer name
          PutByte(Stream, length(lyr.Name)); // name size
          Stream.Write(lyr.Name[1], length(lyr.Name)); // name
          inc(extradatasize, 1 + length(lyr.Name));
          // pad to multiple of 4 bytes
          while (ExtraDataSize and $3) <> 0 do
          begin
            PutByte(Stream, 1);
            inc(ExtraDataSize);
          end;

          // adjustment layer info tags (not used)
          // just blank because this is tagged

          // actual extra data asize
          PutLongintAt(Stream, pos1, extradatasize);
        end;

      end;  // for each layer loop

      // write pixel data
      for i := 0 to layers.Count - 1 do
      begin
        lyr := TIELayer(layers[i]);
        if not lyr.IsMask then
        begin
          if (i < layers.Count - 1) and (TIELayer(layers[i + 1]).IsMask) then
            WritePixelData(context, lyr.Bitmap, TIELayer(layers[i + 1]).Bitmap, sizes, i, false)
          else
            WritePixelData(context, lyr.Bitmap, nil, sizes, i, false);
        end;
        if assigned(Progress.fOnProgress) then
          Progress.fOnProgress(Progress.Sender, trunc(i / layers.Count * 100));
      end;

    finally
      freemem(sizes);
      if tempLayer <> nil then
      begin
        layers.Clear();
        tempLayer.Free();
      end;
    end;

    if IOParams.PSD_LargeDocumentFormat then
      // PSB, 64 bit size
      PutInt64At(Stream, sizepos, Stream.Size - sizepos - 8)
    else
      // PSD, 32 bit size
      PutLongintAt(Stream, sizepos, Stream.Size - sizepos - 4);
  end;
end;

procedure WriteGlobalMask(var context: TPSDWriterContext);
var
  sizepos: int64;
begin
  with context do
  begin
    sizepos := Stream.Position;
    PutLongint(Stream, 0); // dummy size value

    PutLongintAt(Stream, sizepos, Stream.Size - sizepos - 4);
  end;
end;

// Write layer and mask information section
procedure WriteLayerAndMaskInfo(var context: TPSDWriterContext);
var
  sizepos: int64;
begin
  with context do
  begin
    sizepos := Stream.Position;

    // dummy size value
    if IOParams.PSD_LargeDocumentFormat then
      // PSB, 64 bit size
      PutInt64(Stream, 0)
    else
      // PSB, 32 bit size
      PutLongint(Stream, 0);

    if context.layers.Count > 0 then
    begin
      WriteLayers(context);
      WriteGlobalMask(context);
      if IOParams.PSD_LargeDocumentFormat then
        // PSB, 64 bit size
        PutInt64At(Stream, sizepos, Stream.Size - sizepos - 8)
      else
        // PSD, 32 bit size
        PutLongintAt(Stream, sizepos, Stream.Size - sizepos - 4);
    end;
  end;
end;

// in PSD layers have the same size of related bitmap
// in PSD layers have all the same pixel format
// this function make ImageEn layers compatible with PSD
procedure MakeLayersPSDCompatible(mergedImage: TIEBitmap; layers: TList);
var
  mode: integer;
  depth: integer;
  i: integer;
  proc: TImageEnProc;
begin
  mode  := -1;
  depth := -1;
  if mergedImage <> nil then
  begin
    mode  := PIXELFORMAT2MODE[mergedImage.PixelFormat];
    depth := mergedImage.BitCount div mergedImage.ChannelCount;
  end;
  if mode = -1 then
  begin
    with TIELayer(layers[0]) do
    begin
      mode  := PIXELFORMAT2MODE[Bitmap.PixelFormat];
      depth := Bitmap.BitCount div Bitmap.ChannelCount;
    end;
  end;
  if (mode = -1) or (mode = 1000) then
    exit;
  proc := TImageEnProc.Create(nil);
  proc.AutoUndo := false;
  try
    for i := 0 to layers.Count - 1 do
    begin
      // change pixel format (if this is not a layer mask)
      with TIELayer(layers[i]) do
      begin
        if not IsMask then
          case mode of
            0:  // black/white
              begin
                depth := 1;
                if Bitmap.PixelFormat <> ie1g then
                  Bitmap.PixelFormat := ie1g;
              end;
            1:  // gray scale
              begin
                if (depth = 8) and (Bitmap.PixelFormat <> ie8g) then
                  Bitmap.PixelFormat := ie8g
                else
                if (depth = 16) and (Bitmap.PixelFormat <> ie16g) then
                  Bitmap.PixelFormat := ie16g;
              end;
            2:  // indexed
              begin
                depth := 8;
                if Bitmap.PixelFormat <> ie8p then
                  Bitmap.PixelFormat := ie8p;
              end;
            3:  // RGB
              begin
                if (depth = 8) and (Bitmap.PixelFormat <> ie24RGB) then
                  Bitmap.PixelFormat := ie24RGB
                else
                if (depth = 16) and (Bitmap.PixelFormat <> ie48RGB) then
                  Bitmap.PixelFormat := ie48RGB;
              end;
            4:  // CMYK
              begin
                depth := 8;
                if Bitmap.PixelFormat <> ieCMYK then
                  Bitmap.PixelFormat := ieCMYK;
              end;
            9:  // Lab
              begin
                depth := 8;
                if Bitmap.PixelFormat <> ieCIELab then
                  Bitmap.PixelFormat := ieCIELab;
              end;
          end;
        // now resize
        if (Width <> Bitmap.Width) or (Height <> Bitmap.Height) then
        begin
          proc.AttachedIEBitmap := Bitmap;
          if UseResampleFilter then
            proc.Resample(Width, Height, ResampleFilter)
          else
            proc.Resample(Width, Height, rfNone);
        end;

        Bitmap.AlphaChannel;  // each layer needs to have an alpha mask
      end;
    end;
  finally
    proc.Free();
  end;
end;

// layers cannot be "nil". If you don't want save layers, just leave its size (Count) = 0
// mergedImage must be always present
procedure IEWritePSD(Stream: TStream; var IOParams: TIOParamsVals; var Progress: TProgressRec; mergedImage: TIEBitmap; layers: TList);
var
  context: TPSDWriterContext;
  header: TPSDHeader;
begin

  MakeLayersPSDCompatible(mergedImage, layers);

  context.Stream       := Stream;
  context.IOParams     := IOParams;
  context.layers       := layers;
  context.mergedImage  := mergedImage;
  context.Progress     := Progress;
  with context do
  begin

    mode  := PIXELFORMAT2MODE[mergedImage.PixelFormat];
    depth := mergedImage.BitCount div mergedImage.ChannelCount;

    // prepare header
    Move(MAGIK[0], header.Signature[0], 4);
    if IOParams.PSD_LargeDocumentFormat then
      // PSB (version = 2)
      header.Version := IESwapWord(2)
    else
      // PSD (version = 1)
      header.Version := IESwapWord(1);
    FillChar(header.Reserved[0], 5, 0);
    header.Channels := IESwapWord(mergedImage.ChannelCount);
    header.Rows     := IESwapDWord(mergedImage.Height);
    header.Columns  := IESwapDWord(mergedImage.Width);
    header.Depth    := IESwapWord(depth);
    header.Mode     := IESwapWord(mode);
    if header.Mode = 1000 then
    begin
      // pixel format not supported by PSD
      Progress.Aborting^ := true;
      exit;
    end;

    // write header
    Stream.Write(header, sizeof(TPSDHeader));

    // write color mode data section (palette)
    WriteColorMap(context);

    // write image resources section
    WriteImageResources(context);

    // write Layer and mask information section
    WriteLayerAndMaskInfo(context);

    // write pixel data (merged image)
    WritePixelData(context, mergedImage, nil, nil, 0, (layers.Count > 0));

  end;
end;

{$endif}  // IEINCLUDEPSD

end.
