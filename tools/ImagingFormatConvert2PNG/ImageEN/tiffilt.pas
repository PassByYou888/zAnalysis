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
File version 1031
*)

unit tiffilt;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, Graphics, classes, sysutils, ImageEnProc, ImageEnIO, hyiedefs, hyieutils;

// TIFF image load/save
procedure TIFFReadStream(Bitmap: TIEBitmap; Stream: TStream; var numi: integer; IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; var AlphaChannel: TIEMask; TranslateBase: boolean; IgnoreAlpha: boolean; IsExifThumb: boolean; IsEXIFData: boolean; ProvidedHeader: PTIFFHeader = nil);
function TIFFWriteStream(OStream: TStream; Ins: boolean; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec): integer;
function TIFFEnumImages(Stream: TStream): integer;
function TIFFDeleteImStream(Stream: TStream; idx: integer): integer;
function TIFFDeleteImStreamGroup(Stream: TStream; idxlist: pintegerarray; idxcount: integer): integer;
procedure TIFFExtractImStream(Stream: TStream; idx: integer; OutStream: TStream);
procedure TIFFInsertImStream(Stream: TStream; ToInsert: TStream; idx: integer; OutStream: TStream); overload;
function TIFFInsertImStream(Stream: TStream; ToInsert: TStream; idx: integer; OutStream: TStream; internal: boolean): integer; overload;
function IsTIFFStream(fs: TStream): boolean;
function IsDNGStream(fs: TStream): boolean;
function IsHDPStream(fs: TStream): boolean;
{$ifdef IEINCLUDETIFFHANDLER}
function IEInjectTIFFEXIF(InputStream, OutputStream: TStream; const InputFileName, OutputFileName: WideString; pageIndex: integer; IOParams: TIOParamsVals): boolean;
{$endif}

function TIFFReadHeader(Stream: TStream; ProvidedHeader: PTIFFHeader; out LittleEndian: boolean; out BigTIFF: boolean; out DataPosSize: integer; out IFDPosition: int64): boolean;
function TIFFLoadTags(Stream: TStream; var numi: integer; ImageIndex: integer; IFD: TIETIFFIFDReader): boolean;



implementation

uses  {$ifdef IEUSEVCLZLIB}zlib, {$else}iezlib, {$endif}
      giffilter, tifccitt, imageenview, ieview, jpegfilt, neurquant, iesettings;

{$R-}




 //////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
//**********************************************************************************//
//* READ TIFF                                                                      *//
//**********************************************************************************//
////////////////////////////////////////////////////////////////////////////////////
 //////////////////////////////////////////////////////////////////////////////////


const
  IEMAXEXTRASAMPLES = 10;

type

  TTIFFReaderContext = class
    ImageWidth, ImageHeight: integer;
    SamplesPerPixel: integer;
    BitsPerSample: integer;
    RowsPerStrip: integer;
    TileWidth, TileLength: integer;
    PhotometricInterpretation: word;
    PlanarConfiguration: word;
    Orientation: word;
    Compression: word;
    SampleFormat: word;
    StripOffsets: pint64array;
    StripOffsets_Num: integer;     // number of items in StripOffsets
    StripByteCounts: pint64array;
    StripByteCounts_Num: integer;  // number of items in StripByteCounts
    TileOffsets: pint64Array;
    TileOffsets_Num: integer;      // number of items in TileOffsets
    TileByteCounts: pint64Array;
    TileByteCounts_Num: integer;   // number of items in TileByteCounts
    ColorMap: PRGBROW;             // Colormap (dim=2^BitsPerSample)
    ColorMap_Num: integer;         // number of entries in Colormap (2^BitsPerSample)
    TransferFunction: PTIFFRGBArray;
    Transferfunction_Num: integer; // number of items in TransferFunction
    Predictor: integer;
    JPEGTables: pointer;           // compression tables (JPEG 7)
    JPEGTablesSize: integer;       // size in bytes of JPEGTables
    T4Options: integer;
    T6Options: integer;
    FillOrder: integer;
    Software: AnsiString;
    YCbCrSubSampling: array [0..1] of integer;
    // OLD Jpeg fields
    JPEGProc: integer;
    JPEGInterchangeFormat: integer;
    JPEGInterchangeFormatLength: integer;
    JPEGRestartInterval: integer;
    JPEGLosslessPredictors: array[0..6] of integer;
    JPEGPointTransforms: array[0..6] of integer;
    JPEGQTables: array[0..6] of integer;
    JPEGDCTables: array[0..6] of integer;
    JPEGACTables: array[0..6] of integer;
    // no tiff fields
    LZWDecompFunc: TTIFFLZWDecompFunc;
    LittleEndian: boolean;
    RefParams: TIOParamsVals;
    // Alpha
    AlphaChannel: TIEMask;
    IgnoreAlpha: boolean;
    ExtraSamples: integer;
    ExtraSamplesCount: integer;
    ExtraSamplesVal: array[0..IEMAXEXTRASAMPLES-1] of integer;
    // IFDs
    MainIFD: TIETIFFIFDReader;
    ExifIFD: TIETIFFIFDReader;
    GpsIFD: TIETIFFIFDReader;
    InteropIFD: TIETIFFIFDReader;

    constructor Create();
    destructor Destroy(); override;

    procedure ReadStream(Bitmap: TIEBitmap; Stream: TStream; var numi: integer; IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; var AlphaChannel: TIEMask; TranslateBase: boolean; IgnoreAlpha: boolean; IsExifThumb: boolean; IsEXIFData: boolean; ProvidedHeader: PTIFFHeader = nil);
  end;


procedure Decompress1(context: TTIFFReaderContext; outbuf: TIEBitmap; baserow: integer; basecol: integer; xbuf: pbyte; sz: integer; Width, Height: integer; var Progress: TProgressRec); forward;
procedure Decompress2(context: TTIFFReaderContext; outbuf: TIEBitmap; baserow: integer; xbufn: array of pbyte; szn: array of integer; Width, Height: integer; var Progress: TProgressRec); forward;
procedure Strips2Bitmap(IFD: TIETIFFIFDReader; context: TTIFFReaderContext; var Bitmap: TIEBitmap; var Progress: TProgressRec); forward;
procedure Tiles2Bitmap(IFD: TIETIFFIFDReader; context: TTIFFReaderContext; var Bitmap: TIEBitmap; var Progress: TProgressRec); forward;




// returns: true = ok, false = fail
function TIFFReadHeader(Stream: TStream; ProvidedHeader: PTIFFHeader; out LittleEndian: boolean; out BigTIFF: boolean; out DataPosSize: integer; out IFDPosition: int64): boolean;
var
  HeaderByteOrder: word;
  HeaderVersion: word;
begin
  if ProvidedHeader <> nil then
  begin
    HeaderByteOrder := ProvidedHeader^.Id;
    LittleEndian    := HeaderByteOrder = $4949;
    //HeaderVersion   := IECSwapWord(ProvidedHeader^.Ver, not LittleEndian);
    IFDPosition     := IECSwapDWord(ProvidedHeader^.PosIFD, not LittleEndian);
    BigTIFF         := false;
    DataPosSize     := 4;
  end
  else
  begin
    Stream.Read(HeaderByteOrder, sizeof(word));
    LittleEndian := HeaderByteOrder = $4949;
    HeaderVersion := IEStreamReadWord(Stream, not LittleEndian);
    if HeaderVersion = 43 then
    begin
      // BigTIFF
      BigTIFF := true;
      DataPosSize := 8;
      if IEStreamReadWord(Stream, not LittleEndian) <> 8 then // Bytesize of offsets (always 8)
      begin
        result := false;
        exit;
      end;
      if IEStreamReadWord(Stream, not LittleEndian) <> 0 then // always 0
      begin
        result := false;
        exit;
      end;
      IFDPosition := IEStreamReadInt64(Stream, not LittleEndian);  // Offset to first IFD
    end
    else
    begin
      // classic TIFF
      BigTIFF := false;
      DataPosSize := 4;
      IFDPosition := IEStreamReadDWord(Stream, not LittleEndian);  // Offset to first IFD
    end;
  end;

  // simple header check
  result := ((HeaderByteOrder = $4949) or (HeaderByteOrder = $4D4D)) and ((IFDPosition > 0) or (ProvidedHeader <> nil)) and (IFDPosition < Stream.Size);
end;



/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
// TTIFFReaderContext


constructor TTIFFReaderContext.Create();
begin
  inherited;
  MainIFD    := TIETIFFIFDReader.Create();
  ExifIFD    := TIETIFFIFDReader.Create();
  GpsIFD     := TIETIFFIFDReader.Create();
  InteropIFD := TIETIFFIFDReader.Create();
  // note: other object fields are always initialized to 0
end;


destructor TTIFFReaderContext.Destroy();
begin
  if StripOffsets_Num > 0 then
    freemem(Stripoffsets);
  if StripByteCounts_Num > 0 then
    freemem(StripByteCounts);
  if TileOffsets_Num > 0 then
    freemem(TileOffsets);
  if TileByteCounts_Num > 0 then
    freemem(TileByteCounts);
  if ColorMap_Num > 0 then
    freemem(ColorMap);
  if TransferFunction_Num > 0 then
    freemem(TransferFunction);
  if JPEGTables <> nil then
    freemem(JPEGTables);

  MainIFD.Free();
  ExifIFD.Free();
  GpsIFD.Free();
  InteropIFD.Free();

  inherited;
end;


// TTIFFReaderContext
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////


procedure ReadEXIFMakerNote(IFD: TIETIFFIFDReader; NTag: integer; tagsHandler: TIETagsHandler);
var
  t: integer;
  dpos, dnum: int64;
begin
  tagsHandler.Clear();
  t := IFD.FindTAG(NTag);
  if t >= 0 then
  begin
    dpos := IFD.GetDataPos(t);
    dnum := IFD.GetDataNum(t);
    if (IFD.StreamBase + dpos + dnum <= IFD.Stream.Size) and (dnum > 0) then
    begin
      IFD.Stream.Seek(IFD.StreamBase + dpos, soBeginning);
      tagsHandler.ReadFromStream(IFD.Stream, dnum, IFD.LittleEndian, IFD.Stream.Position);
    end;
  end;
end;

{$ifdef IEINCLUDEIMAGINGANNOT}
procedure LoadWang(IFD: TIETIFFIFDReader; Params: TIOParamsVals);
var
  t: integer;
  buf: array of byte;
  dpos, dnum: int64;
begin
  t := IFD.FindTAG(IETIFFTAG_WANGIMAGING);
  if t >= 0 then
  begin
    dpos := IFD.GetDataPos(t);
    dnum := IFD.GetDataNum(t);
    IFD.Stream.Seek(IFD.StreamBase + dpos, soBeginning);
    SetLength(buf, dnum);
    IFD.Stream.Read(buf[0], dnum);
    Params.ImagingAnnot.LoadFromStandardBuffer(@buf[0], dnum);
  end;
end;
{$endif}


procedure LoadImageEnAnnot(IFD: TIETIFFIFDReader; Params: TIOParamsVals);
var
  t : integer;
  buf: array of byte;
  dpos, dnum: int64;
begin
  t := IFD.FindTAG(IEGlobalSettings().ObjectsTIFFTag);
  if t >= 0 then
  begin
    dpos := IFD.GetDataPos(t);
    dnum := IFD.GetDataNum(t);
    IFD.Stream.Seek(IFD.StreamBase + dpos, soBeginning);
    SetLength(buf, dnum);
    IFD.Stream.Read(buf[0], dnum);
    Params.ImageEnAnnot.LoadFromBuffer(@buf[0], dnum);
  end;
end;


procedure LoadICC(IFD: TIETIFFIFDReader; Params: TIOParamsVals);
var
  t: integer;
  buf: array of byte;
  dpos, dnum: int64;
begin
  if assigned(Params) then
  begin
    t := IFD.FindTAG(IETIFFTAG_ICC);
    if t >= 0 then
    begin
      dpos := IFD.GetDataPos(t);
      dnum := IFD.GetDataNum(t);
      IFD.Stream.Seek(IFD.StreamBase + dpos, soBeginning);
      SetLength(buf, dnum);
      IFD.Stream.Read(buf[0], dnum);
      Params.InputICCProfile.LoadFromBuffer(@buf[0], dnum);
    end;
  end;
end;

function convVersionIDtoStr(id: AnsiString): AnsiString;
var
  i: integer;
begin
  if id = '' then
    id := #2#2#0#0;
  result := '';
  for i := 1 to length(id) do
    result := result + IEIntToStr(ord(id[i])) + '.';
  result := IECopy(result, 1, length(result) - 1);
end;

function convVersionStrtoID(const str: AnsiString): AnsiString;
var
  i, p: integer;
begin
  result := '';
  p := 1;
  for i := 1 to length(str) do
  begin
    if (str[i] = '.') then
    begin
      result := result + AnsiChar(IEStrToIntDef(IECopy(str, p, i - p), 0));
      p := i + 1;
    end;
  end;
  result := result + AnsiChar(IEStrToIntDef(IECopy(str, p, length(str) - p + 1), 0));
  while length(result) < 4 do
    result := result + #0;
  SetLength(result, 4);
end;

// read colormap
procedure ReadColorMap(IFD: TIETIFFIFDReader; context: TTIFFReaderContext);
var
  t, q: integer;
  max: integer;
begin
  context.ColorMap_Num := 0;
  t := IFD.FindTAG(IETIFFTAG_COLORMAP);
  if t >= 0 then
    with context do
    begin
      ColorMap_Num := 1 shl BitsPerSample;

      max := 0;
      for q := 0 to ColorMap_Num * 3 - 1 do
        max := imax( IFD.GetItem(t, q), max );

      getmem(ColorMap, ColorMap_Num * sizeof(TRGB));
      for q := 0 to ColorMap_Num - 1 do
      begin
        if max>255 then
        begin
          ColorMap^[q].R := IFD.GetItem(t, q) shr 8;
          ColorMap^[q].G := IFD.GetItem(t, ColorMap_Num + q) shr 8;
          ColorMap^[q].B := IFD.GetItem(t, (ColorMap_Num * 2) + q) shr 8;
        end
        else
        begin
          ColorMap^[q].R := IFD.GetItem(t, q) ;
          ColorMap^[q].G := IFD.GetItem(t, ColorMap_Num + q) ;
          ColorMap^[q].B := IFD.GetItem(t, (ColorMap_Num * 2) + q) ;
        end;
      end;
    end;
end;


procedure TIFFReadExtraSamples(IFD: TIETIFFIFDReader; context: TTIFFReaderContext);
var
  t, i: integer;
  dnum: int64;
begin
  context.ExtraSamplesCount := 0;
  t := IFD.FindTAG(338);
  if t >= 0 then
  begin
    dnum := IFD.GetDataNum(t);
    with context do
    begin
      ExtraSamplesCount := imin( dnum, IEMAXEXTRASAMPLES );
      for i := 0 to ExtraSamplesCount - 1 do
        ExtraSamplesVal[i] := IFD.GetItem(t, i);
    end;
  end;
end;

// read TransferFunction
procedure ReadTransferFunction(IFD: TIETIFFIFDReader; context: TTIFFReaderContext);
var
  t, q: integer;
begin
  context.TransferFunction_Num := 0;
  t := IFD.FindTAG(IETIFFTAG_TRANSFERFUNC);
  if t >= 0 then
    with context do
    begin
      TransferFunction_Num := 1 shl BitsPerSample;
      getmem(TransferFunction, TransferFunction_Num * sizeof(TTIFFColor));
      for q := 0 to TransferFunction_Num - 1 do
      begin
        TransferFunction^[q].R := IFD.GetItem(t, q * 3);
        TransferFunction^[q].G := IFD.GetItem(t, q * 3 + 1);
        TransferFunction^[q].B := IFD.GetItem(t, q * 3 + 2);
      end;
    end;
end;


function ReadBitsPerSample(IFD: TIETIFFIFDReader; context: TTIFFReaderContext): boolean;
var
  t: integer;
  w: word;
  q: integer;
  dnum: int64;
begin
  result := true;
  context.BitsPerSample := 1; // default
  t := IFD.FindTAG(258);
  if t >= 0 then
  begin
    dnum := IFD.GetDataNum(t);
    if dnum = 1 then
    begin
      // one value
      context.BitsPerSample := IFD.GetItem(t, 0);
    end
    else
    begin
      context.BitsPerSample := -1;
      for q := 0 to dnum - 1 do
      begin
        w := IFD.GetItem(t, q);
        if (context.BitsPerSample <> -1) and (context.BitsPerSample <> w) then
          result := false;
        context.BitsPerSample := w;
      end;
    end;
  end;
end;

// read tiles
procedure Tiles2Bitmap(IFD: TIETIFFIFDReader; context: TTIFFReaderContext; var Bitmap: TIEBitmap; var Progress: TProgressRec);
var
  q, sz: integer;
  buf: pbyte;
  row, col, basecol: integer;
begin
  with context do
  begin
    row := 0;
    col := 0;
    Progress.per1 := 100 / ImageHeight;
    Progress.per2 := 100 / TileOffsets_Num;
    Progress.val := 0;
    basecol := 0;
    if PlanarConfiguration = 1 then
    begin
      Bitmap.Width  := Bitmap.Width + TileWidth;
      Bitmap.Height := Bitmap.Height + TileLength;
      if RefParams.TIFF_GetTile = -1 then
        q := 0
      else
        q := RefParams.TIFF_GetTile;
      while q < TileOffsets_Num do
      begin
        IFD.Stream.Seek(IFD.StreamBase + TileOffsets^[q], soBeginning);
        if TileByteCounts_Num > q then
          sz := TileByteCounts^[q]
        else
          sz := 0;
        if sz = 0 then
          sz := IFD.Stream.Size - uint64(TileOffsets^[q]);

        getmem(buf, imax(sz, TileWidth * TileLength * 4));

        try

          IFD.Stream.Read(buf^, sz);
          if bitmap.pixelformat = ie24RGB then
            basecol := col * 3
          else
          if bitmap.pixelformat = ie1g then
            basecol := (col shr 3)
          else
          if (bitmap.pixelformat = ie8g) or (bitmap.PixelFormat = ie8p) then
            basecol := col
          else
          if (bitmap.pixelformat = ie16g) then
            basecol := col * 2
          else
          begin
            Progress.Aborting^ := true;
            exit;
          end;
          Decompress1(context, Bitmap, row, basecol, buf, sz, TileWidth, TileLength, Progress); // compress strip (buffer)

        finally
          freemem(buf);
        end;

        inc(col, TileWidth);
        if col >= ImageWidth then
        begin
          col := 0;
          inc(row, TileLength);
        end;

        if Progress.Aborting^ then
          break;

        if RefParams.TIFF_GetTile>-1 then
          break;

        inc(q);
      end;
      Bitmap.Width  := ImageWidth;
      Bitmap.Height := Imageheight;
    end
    else
    if PlanarConfiguration = 2 then
    begin
      // not supported
      Progress.Aborting^ := true;
      exit;
    end;
  end;
end;


// read strips
procedure Strips2Bitmap(IFD: TIETIFFIFDReader; context: TTIFFReaderContext; var Bitmap: TIEBitmap; var Progress: TProgressRec);
var
  q, w, e, c, sz, szt, i: integer;
  szn: array [0..IEMAXEXTRASAMPLES-1] of integer;
  buf: array of byte;
  bufn, bufv: array [0..IEMAXEXTRASAMPLES-1] of pbyte;
  row: integer;
begin
  with context do
  begin
    row := 0;
    Progress.per1 := 100 / ImageHeight;
    Progress.per2 := 100 / StripOffsets_Num;
    Progress.val := 0;
    if PlanarConfiguration = 1 then
    begin
      // consecutive channels

      for q := 0 to StripOffsets_Num - 1 do
      begin
        IFD.Stream.Seek(IFD.StreamBase + StripOffsets^[q], soBeginning);

        if StripByteCounts_Num > q then
          sz := StripByteCounts^[q]
        else
          sz := 0;

        szt := RowsPerStrip * IEBitmapRowLen(ImageWidth, SamplesPerPixel * BitsPerSample, 8); // 3.1.2 (case 26 MAR 2010, 05:51)

        if (Compression = 1) and (szt > sz) then
          sz := szt;

        if sz = 0 then
          sz := IFD.Stream.size - uint64(StripOffsets^[q]);

        if sz > 0 then
        begin

          if (Compression = 1) and (RowsPerStrip = ImageHeight) and (BitsPerSample = 8) then
          begin
            // to avoid to allocate the full strip (useful when one strip contains the whole image)
            sz := SamplesPerPixel * ImageWidth * (BitsPerSample div 8);
            SetLength(buf, sz);
            for i := 0 to RowsPerStrip-1 do
            begin
              IFD.Stream.Read(buf[0], sz);
              Decompress1(context, bitmap, row+i, 0, @buf[0], sz, ImageWidth, 1, Progress); // decompress row
            end;
          end
          else
          begin
            SetLength(buf, sz);
            szt := sz;
            if szt > IFD.Stream.Size then
              szt := IFD.Stream.size - uint64(StripOffsets^[q]);

            szt := IFD.Stream.Read(buf[0], szt);
            Decompress1(context, bitmap, row, 0, @buf[0], szt, ImageWidth, imin(RowsPerStrip, ImageHeight - row), Progress); // decompress strip (buffer)
          end;
        end;

        inc(row, RowsPerStrip);

        if Progress.Aborting^ then
          break;
      end;
    end
    else
    if PlanarConfiguration = 2 then
    begin
      // channels are on separate strips
      q := 0;
      e := StripOffsets_Num div context.SamplesPerPixel;
      while q < e do
      begin
        for c := 0 to context.SamplesPerPixel - 1 do
          bufn[c] := nil;

        try
          for c := 0 to context.SamplesPerPixel - 1 do
          begin
            // channel "c"
            szn[c] := StripByteCounts^[q + c*e];
            getmem(bufn[c], szn[c]);
            bufv[c] := bufn[c]; // bufv is actually sent to Decompress2, hence it can change pointers inside the array
            IFD.Stream.Seek(IFD.StreamBase + StripOffsets^[q + c*e], soBeginning);
            szn[c] := IFD.Stream.Read(bufn[c]^, szn[c]);
            if context.FillOrder = 2 then
              for i := 0 to szn[c]-1 do
                ReverseBitsB( pbytearray(bufn[c])[i] );
          end;
          inc(q);

          w := imin(RowsPerStrip, ImageHeight - row);
          Decompress2(context, bitmap, row, bufv, szn, ImageWidth, w, Progress); // decompress strip (buffer)

        finally
          for c := 0 to context.SamplesPerPixel - 1 do
            freemem(bufn[c]);
        end;

        inc(row, RowsPerStrip);

        if Progress.Aborting^ then
          break;
      end;
    end;
  end;
end;


procedure PerformPredictor(context: TTIFFReaderContext; buf: pbyte; Width: integer; OneChannel: boolean = false);
var
  ra1, ra2, ra3, ra4, rp1, rp2, rp3, rp4: pbyte;
  dra1, dra2, dra3, drp1, drp2, drp3: pword;
  ra, rp: array of pbyte;
  z, v, i: integer;
begin
  if (context.Predictor = 2) and ((context.Compression = 5) or (context.Compression = 8) or (context.Compression = 32946)) then
  begin
    // Predictor
    if (context.BitsPerSample = 8) and ((context.SamplesPerPixel = 1) or OneChannel) then
    begin
      // 8 bits per sample - 1 sample per pixel
      ra1 := @(pbytearray(buf)^[1]);
      rp1 := buf;
      for z := 1 to Width - 1 do
      begin
        inc(ra1^, rp1^);
        inc(ra1);
        inc(rp1);
      end;
    end
    else
    if (context.BitsPerSample = 16) and ((context.SamplesPerPixel = 1) or OneChannel) then
    begin
      // 16 bits per sample - 1 sample per pixel
      dra1 := @(pwordarray(buf)^[1]);
      drp1 := pword(buf);
      for z := 1 to Width - 1 do
      begin
        inc(dra1^, drp1^);
        inc(dra1);
        inc(drp1);
      end;
    end
    else
    if (context.BitsPerSample = 8) and (context.SamplesPerPixel = 3) then
    begin
      // 8 bits per sample - 3 samples per pixel
      ra1 := @(pbytearray(buf)^[3]);
      ra2 := @(pbytearray(buf)^[4]);
      ra3 := @(pbytearray(buf)^[5]);
      rp1 := buf;
      rp2 := @(pbytearray(buf)^[1]);
      rp3 := @(pbytearray(buf)^[2]);
      for z := 1 to width - 1 do
      begin
        inc(ra1^, rp1^);
        inc(ra1, 3);
        inc(rp1, 3);
        inc(ra2^, rp2^);
        inc(ra2, 3);
        inc(rp2, 3);
        inc(ra3^, rp3^);
        inc(ra3, 3);
        inc(rp3, 3);
      end;
    end
    else
    if (context.BitsPerSample = 8) and (context.SamplesPerPixel = 2) then
    begin
      // 8 bits per sample - 2 samples per pixel
      ra1 := @(pbytearray(buf)^[2]);
      ra2 := @(pbytearray(buf)^[3]);
      rp1 := buf;
      rp2 := @(pbytearray(buf)^[1]);
      for z := 1 to width - 1 do
      begin
        inc(ra1^, rp1^);
        inc(ra1, 2);
        inc(rp1, 2);
        inc(ra2^, rp2^);
        inc(ra2, 2);
        inc(rp2, 2);
      end;
    end
    else
    if (context.BitsPerSample = 16) and (context.SamplesPerPixel = 3) then
    begin
      // 16 bits per sample - 3 samples per pixel
      dra1 := @(pwordarray(buf)^[3]);
      dra2 := @(pwordarray(buf)^[4]);
      dra3 := @(pwordarray(buf)^[5]);
      drp1 := pword(buf);
      drp2 := @(pwordarray(buf)^[1]);
      drp3 := @(pwordarray(buf)^[2]);
      for z := 1 to width - 1 do
      begin
        inc(dra1^, drp1^);
        inc(dra1, 3);
        inc(drp1, 3);
        inc(dra2^, drp2^);
        inc(dra2, 3);
        inc(drp2, 3);
        inc(dra3^, drp3^);
        inc(dra3, 3);
        inc(drp3, 3);
      end;
    end
    else
    if (context.BitsPerSample = 8) and (context.SamplesPerPixel = 4) then
    begin
      // 8 bits per sample - 4 samples per pixel
      ra1 := @(pbytearray(buf)^[4]);
      ra2 := @(pbytearray(buf)^[5]);
      ra3 := @(pbytearray(buf)^[6]);
      ra4 := @(pbytearray(buf)^[7]);
      rp1 := buf;
      rp2 := @(pbytearray(buf)^[1]);
      rp3 := @(pbytearray(buf)^[2]);
      rp4 := @(pbytearray(buf)^[3]);
      for z := 1 to width - 1 do
      begin
        inc(ra1^, rp1^);
        inc(ra1, 4);
        inc(rp1, 4);
        inc(ra2^, rp2^);
        inc(ra2, 4);
        inc(rp2, 4);
        inc(ra3^, rp3^);
        inc(ra3, 4);
        inc(rp3, 4);
        inc(ra4^, rp4^);
        inc(ra4, 4);
        inc(rp4, 4);
      end;
    end
    else
    if (context.BitsPerSample = 8) and (context.SamplesPerPixel > 4) then
    begin
      // 8 bits per sample - >4 samples per pixel
      v := context.SamplesPerPixel;
      SetLength(ra, v);
      SetLength(rp, v);
      for i := 0 to v-1 do
      begin
        ra[i] := @(pbytearray(buf)^[ v + i ]);
        rp[i] := @(pbytearray(buf)^[ i ]);
      end;
      for z := 1 to width - 1 do
        for i := 0 to v - 1 do
        begin
          inc(ra[i]^, rp[i]^);
          inc(ra[i], v);
          inc(rp[i], v);
        end;
    end;
  end;
end;

// Decompress a row
// buf= output buffer (if BitsPerSample>=8 and there isn't compression then it link to the input buffer)
// xbuf= input buffer (current position)
// xbuflen = length of xbuf
// Width= row size
// brow = row size in bytes (decompressed, packed)
// LZW = LZW decompressor Id
// predbuf = buffer of previous line for CCITT 2D decompression and ZIP compression
// CCITTposb = position of next bit to read for CCITT decompression (intput/output)
// return false if aborting
function GetNextLine(curline: integer; var buf, xbuf: pbyte; xbuflen: integer; context: TTIFFReaderContext; Width, brow: integer; var LzwId: pointer; predbuf: pbyte; var CCITTposb: integer; var zipbuf: pbyte; var rlepos: integer): boolean;
var
  z, v, v2, z2, cw: integer;
  buf2: pbyte;
  ra1: pbyte;
  {$ifdef IEINCLUDEZLIB}
  i: integer;
  {$endif}
begin
  result := true;

  case context.Compression of

    1: // NO COMPRESSION
      begin
        if context.BitsperSample >= 8 then
          buf := xbuf
        else
          CopyMemory(buf, xbuf, brow);
        if (context.FillOrder=2) and (context.BitsPerSample<=8) then // 3.0.3
        begin
          buf2 := buf;
          for z := 0 to brow-1 do
          begin
            ReverseBitsB(buf2^);
            inc(buf2);
          end;
        end;
        inc(xbuf, brow);
      end;
    2: // HUFFMAN (TIFF: CCITT 1D)
      inc(xbuf, CCITTHuffmanGetLine(buf, xbuf, xbuflen, Width, context.FillOrder));

    3:
      if (context.T4Options and 1) = 0 then
        // CCITT 3 - 1D (TIFF: Group 3 Fax, or T.4)
        CCITTposb := _CCITTHuffmanGetLine(buf, xbuf, xbuflen, Width, CCITTposb, context.FillOrder)
      else
        // CCITT 3 - 2D (TIFF: Group 3 Fax, or T.4 - 2D)
        CCITTposb := CCITT3_2D_GetLine(buf, xbuf, xbuflen, Width, predbuf, CCITTposb, context.FillOrder, true);

    4:
      // CCITT 4 (TIFF: Group 4 Fax, or T.6)
      CCITTposb := CCITT3_2D_GetLine(buf, xbuf, xbuflen, Width, predbuf, CCITTposb, context.FillOrder, (context.T4Options and $4) <> 0);

    5: // LZW
      begin
        buf2 := context.LZWDecompFunc(xbuf, brow, LzwId, context.FillOrder);
        if buf2 <> nil then
          CopyMemory(buf, buf2, brow)
        else
          // aborting
          result := false;
      end;

    8, 32946:  // Deflate or ZIP
      begin
        {$ifdef IEINCLUDEZLIB}
        if zipbuf = nil then
          ZDecompress(xbuf, xbuflen, pointer(zipbuf), i, 0);
        buf2 := pbyte(zipbuf); inc(buf2, curline*brow);
        CopyMemory(buf, buf2, brow);
        {$endif}
      end;

    32773: // RLE PACKBITS
      begin
{$WARNINGS OFF}
        buf2 := buf;
        z := 0; // reading position
        v := brow;
        cw := 0;
        while (z<v) and (rlepos<xbuflen) do
        begin
          if context.FillOrder=2 then
            ReverseBitsB(xbuf^);
          if (shortint(xbuf^) >= 0) and (shortint(xbuf^) <= 127) then
          begin
            // read next shortint(xbuf^)+1 bytes
            v2 := shortint(xbuf^);
            inc(xbuf);
            inc(rlepos);
            for z2 := 0 to v2 do
            begin // it isn't v2-1, because I have first removed v2+1
              if context.FillOrder=2 then
                ReverseBitsB(xbuf^);
              if cw < brow then
                buf2^ := xbuf^;
              inc(buf2);
              inc(xbuf);
              inc(rlepos);
              inc(z);
              inc(cw);
            end;
          end
          else
          if (shortint(xbuf^) >= -127) and (shortint(xbuf^) <= -1) then
          begin
            // repeat next byte for abs(shortint(xbuf^))+1 times
            v2 := -1 * (shortint(xbuf^)); // see up because there isn't the "+1"
            inc(xbuf);
            inc(rlepos);
            if context.FillOrder=2 then
              ReverseBitsB(xbuf^);
            for z2 := 0 to v2 do
            begin
              if cw < brow then
                buf2^ := xbuf^;
              inc(buf2);
              inc(z);
              inc(cw);
            end;
            inc(xbuf);
            inc(rlepos);
          end
          else
          begin
            inc(xbuf);
            inc(rlepos);
          end;
        end;
{$WARNINGS ON}
      end;

  end;

  if context.BitsPerSample = 4 then
  begin
    // unpack nibble to byte
    v := brow * 2 - 1;
    v2 := brow - 1;
    while v >= 0 do
    begin
      pbytearray(buf)^[v] := pbytearray(buf)^[v2] and $0F;
      dec(v);
      pbytearray(buf)^[v] := (pbytearray(buf)^[v2] and $F0) shr 4;
      dec(v);
      dec(v2);
    end;
  end
  else
  if (context.BitsPerSample > 1) and (context.BitsPerSample < 8) and ((context.Compression < 2) or (context.Compression > 4)) then
  begin
    // unpack groups of BitsPerSample bits in a byte
    buf2 := allocmem(width);
    ra1 := buf2;
    v := 0;
    for z := 0 to width * context.BitsPerSample - 1 do
    begin
      if _GetPixelbw(buf, z) <> 0 then
        ra1^ := ra1^ or (1 shl (context.BitsPerSample - 1 - v));
      inc(v);
      if v = context.BitsPerSample then
      begin
        v := 0;
        inc(ra1);
      end;
    end;
    copymemory(buf, buf2, width);
    freemem(buf2);
  end;

  if (context.BitsPerSample = 4) and (context.PhotometricInterpretation <= 1) then
  begin
    // image (4 bit, 16 levels) gray scale , convert to 8 bit
    buf2 := buf;
    z := 0;
    v := brow * 2;
    while z < v do
    begin
      buf2^ := buf2^ * 17;
      inc(buf2);
      inc(z);
    end;
  end;
  if (context.BitsPerSample = 2) and (context.PhotometricInterpretation <= 1) then
  begin
    // image (2 bit, 4 levels) gray scale , convert to 8 bit
    buf2 := buf;
    z := 0;
    v := brow * 4;
    while z < v do
    begin
      buf2^ := buf2^ * 85;
      inc(buf2);
      inc(z);
    end;
  end;
  if (context.BitsPerSample = 7) and (context.PhotometricInterpretation <= 1) then
  begin
    // image (7 bit, 128 levels) gray scale , convert to 8 bit
    buf2 := buf;
    z := 0;
    v := brow;
    while z < v do
    begin
      buf2^ := buf2^ * 2;
      inc(buf2);
      inc(z);
    end;
  end;
end;


// decompress buffer (PlanarConfiguration=1)
// baserow = first row to fill in outbuf (y coordinate of the subimage)
// basecol = first col x 3 to fill in outbuf (x coordinate X 3 of the subimage)
// xbuf = compressed data
// sz = length of compressed data
// Width = width of the sub image (in pixel)
// Height = height of the sub image (in pixel)
procedure Decompress1(context: TTIFFReaderContext; outbuf: TIEBitmap; baserow: integer; basecol: integer; xbuf: pbyte; sz: integer; Width, Height: integer; var Progress: TProgressRec);
var
  q, w, i, e, j: integer;
  px: PRGB;
  pw, pw2: pword;
  lxbuf, buf, zbuf, predbuf, pxx, pb, buf1, palpha: pbyte;
  wbuf: pword;
  brow: integer; // dimensione in byte di una riga (decompressa, non scompattata)
  CCITTposb: integer;
  inv: boolean;
  LzwId: pointer;
  hasalpha: boolean;
  ms, tbs: TMemoryStream;
  tmpBMP: TIEBitmap;
  nullpr: TProgressRec;
  raw: boolean;
  ldpix, ldpiy, lwidth, lheight, lowidth, loheight, limcount: integer;
  px_cmyk: PCMYK;
  px_rgb48: PRGB48;
  ycbcr, px_ycbcr: PYCBCR;
  pba: pbytearray;
  zipbuf: pbyte;
  rlepos: integer;
  lper: integer;
  alpha: double;
begin
  predbuf := nil;
  zipbuf := nil;
  rlepos := 0;

  lper := -1;

  // calc brow
  if context.PlanarConfiguration = 1 then
    brow := trunc(Width * context.SamplesPerPixel * (context.BitsPerSample / 8))
  else
    brow := trunc(Width * (context.BitsPerSample / 8));
  if (brow / (context.BitsPerSample / 8) < Width) then
    inc(brow);

  LzwId := nil; // initialize Id of LZW compressor

  // jpeg compression (DRAFT TIFF Technical Note #2), without jpeg tables
  if (context.Compression = 7) and (context.JPEGTables = nil) then
  begin
    tmpBMP := nil;
    ms := TMemoryStream.Create();
    try
      ms.Write(xbuf^, sz);
      ms.Position := 0;
      tmpBMP := TIEBitmap.Create();
      tmpBMP.Allocate(Width, Height, outbuf.PixelFormat);
      if (context.StripOffsets_Num = 1) or (context.TileOffsets_Num = 1) then
        nullpr := Progress
      else
      begin
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per2 * val)<>lper) then
          begin
            lper := trunc(per2 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        nullpr.fOnProgress := nil;
        nullpr.Aborting := Progress.Aborting;
      end;
      //raw := context.PhotometricInterpretation=2;  // saved as native RGB
      raw := false;
      // save dpi and size because ReadJPegStream overwrite them
      ldpix    := context.RefParams.DpiX;
      ldpiy    := context.RefParams.DpiY;
      lwidth   := context.RefParams.Width;
      lheight  := context.RefParams.Height;
      lowidth  := context.RefParams.OriginalWidth;
      loheight := context.RefParams.OriginalHeight;
      limcount := context.RefParams.ImageCount;
      ReadJPegStream(ms, nil, tmpBMP, context.RefParams, nullpr, false, raw, false, true, false, true, -1, context.RefParams.IsNativePixelFormat);
      context.RefParams.ImageCount     := limcount;
      context.RefParams.DpiX           := ldpix;
      context.RefParams.DpiY           := ldpiy;
      context.RefParams.Width          := lwidth;
      context.RefParams.Height         := lheight;
      context.RefParams.OriginalWidth  := lowidth;
      context.RefParams.OriginalHeight := loheight;
      if not nullpr.Aborting^ then
      begin
        if raw then
          for q := 0 to tmpBMP.Height - 1 do
          begin
            px := tmpBMP.Scanline[q];
            _BGR2RGB(px, tmpBMP.Width);
          end;
        tmpBMP.CopyRectTo(outbuf, 0, 0, basecol div outbuf.ChannelCount, baserow, Width, Height);
      end;
    finally
      FreeAndNil(tmpBMP);
      FreeAndNil(ms);
    end;
  end

  // jpeg compression (DRAFT TIFF Technical Note #2), with jpeg tables
  else
  if (context.Compression = 7) and (context.JPEGTables <> nil) then
  begin
    ms := TMemoryStream.Create;
    try
      tbs := nil;
      tmpBMP := nil;
      ms.Write(xbuf^, sz);
      ms.Position := 0;
      tbs := TMemoryStream.Create;
      tbs.Write(pbyte(context.JPEGTables)^, context.JPEGTablesSize);
      tbs.Position := 0;
      tmpBMP := TIEBitmap.Create;
      tmpBMP.Allocate(Width, Height, outbuf.PixelFormat);
      if (context.StripOffsets_Num = 1) or (context.TileOffsets_Num = 1) then
        nullpr := Progress
      else
      begin
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per2 * val)<>lper) then
          begin
            lper := trunc(per2 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        nullpr.fOnProgress := nil;
        nullpr.Aborting := Progress.Aborting;
      end;
      raw := context.PhotometricInterpretation = 2; // saved as native RGB
      try
        // save dpi and size because ReadJPegStream overwrite them
        ldpix := context.RefParams.DpiX;
        ldpiy := context.RefParams.DpiY;
        lwidth := context.RefParams.Width;
        lheight := context.RefParams.Height;
        lowidth := context.RefParams.OriginalWidth;
        loheight := context.RefParams.OriginalHeight;
        limcount := context.RefParams.ImageCount;
        ReadJPegStream(ms, tbs, tmpBMP, context.RefParams, nullpr, false, raw, false, true, false, true, -1, context.RefParams.IsNativePixelFormat);
        context.RefParams.ImageCount := limcount;
        context.RefParams.DpiX := ldpix;
        context.RefParams.DpiY := ldpiy;
        context.RefParams.Width := lwidth;
        context.RefParams.Height := lheight;
        context.RefParams.OriginalWidth := lowidth;
        context.RefParams.OriginalHeight := loheight;
      except
      end;
      if not nullpr.Aborting^ then
      begin
        if raw then
          for q := 0 to tmpBMP.Height - 1 do
          begin
            px := tmpBMP.Scanline[q];
            _BGR2RGB(px, tmpBMP.Width);
          end;
        tmpBMP.CopyRectTo(outbuf, 0, 0, basecol div outbuf.ChannelCount, baserow, Width, Height);
      end;
    finally
      FreeAndNil(tmpBMP);
      FreeAndNil(ms);
      FreeAndNil(tbs);
    end;
  end

  // without ColorMap, RGB and 24/32 bit (8 per pixel)
  else
  if (context.PhotometricInterpretation = 2) and
    ((context.SamplesPerPixel = 3) or (context.SamplesPerPixel = 4)) and
    (context.BitsPerSample = 8) then
  begin
    hasalpha := ( ((context.SamplesPerPixel = 4) and (context.ExtraSamples = 2)) or
                  ((context.SamplesPerPixel = 4) and (context.PhotometricInterpretation = 2))
                ) and (not context.IgnoreAlpha);
    if hasalpha then
      context.AlphaChannel.Full := false;
    if (context.Compression <> 1) then
      getmem(buf, Width * context.SamplesPerPixel);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        zbuf := buf;
        px := outbuf.Scanline[baserow + q];
        inc(pbyte(px), basecol);
        if hasalpha then
        begin
          // Has Alpha Channel (3.0.3)
          pb := context.AlphaChannel.ScanLine[baserow + q];
          inc(pb, basecol);
          for w := 0 to Width - 1 do
          begin
            px^ := PRGB(zbuf)^;
            bswap(px^.r, px^.b);
            inc(zbuf, 3);
            pb^ := zbuf^; // alpha channel
            inc(pb);
            inc(zbuf);
            inc(px);
          end;
        end
        else
        begin
          // No Alpha Channel
          IEGlobalSettings().ConvertColorFunction(zbuf, iecmsRGB, px, iecmsBGR, Width, context.RefParams);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // Error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // without ColorMap, RGB 24 + extra samples (only alpha is handled)
  else
  if (context.PhotometricInterpretation = 2) and
    (context.SamplesPerPixel > 3) and (context.ExtraSamplesCount>0) and
    (context.BitsPerSample = 8) then
  begin
    hasalpha := false;
    if not context.IgnoreAlpha then
      for q := 0 to context.ExtraSamplesCount-1 do
        if (context.ExtraSamplesVal[q] = 1) or (context.ExtraSamplesVal[q] = 2) then
          hasalpha := true;
    if hasalpha then
      context.AlphaChannel.Full := false;
    if (context.Compression <> 1) then
      getmem(buf, Width * context.SamplesPerPixel);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        zbuf := buf;
        px := outbuf.Scanline[baserow + q];
        inc(pbyte(px), basecol);
        if hasalpha then
        begin
          // Has Alpha Channel
          for w := 0 to Width - 1 do
          begin
            px^ := PRGB(zbuf)^;
            bswap(px^.r, px^.b);
            inc(zbuf, 3);
            for j := 0 to context.ExtraSamplesCount-1 do
            begin
              if context.ExtraSamplesVal[j] = 1 then
              begin
                // premultiplied alpha
                context.AlphaChannel.SetPixel(w, q + BaseRow, zbuf^);
                alpha := dmax( zbuf^ / 255, 1/255 );
                px^.r := trunc(px^.r / alpha);
                px^.g := trunc(px^.g / alpha);
                px^.b := trunc(px^.b / alpha);
              end
              else
              if context.ExtraSamplesVal[j] = 2 then
              begin
                // unassociated alpha
                context.AlphaChannel.SetPixel(w, q + BaseRow, zbuf^);
              end;
              inc(zbuf);
            end;
            inc(px);
          end;
        end
        else
        begin
          // No Alpha Channel
          //IEConvertColorFunction(zbuf, iecmsRGB, px, iecmsBGR, Width, context.RefParams);
          for w := 0 to Width - 1 do
          begin
            px^ := PRGB(zbuf)^;
            bswap(px^.r, px^.b);
            inc(zbuf, 3);
            for j := 0 to context.ExtraSamplesCount-1 do
              inc(zbuf);
            inc(px);
          end;
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // Error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // without ColorMap, RGB and 48 bit (16 per sample)
  else
  if (context.PhotometricInterpretation = 2) and (context.SamplesPerPixel = 3) and (context.BitsPerSample = 16) then
  begin
    if (context.Compression <> 1) then
      getmem(buf, Width * context.SamplesPerPixel * 2);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        pw := pword(buf);

        if outbuf.PixelFormat=ie48RGB then
        begin

          // native pixel format
          px_rgb48 := outbuf.Scanline[baserow+q];
          inc(px_rgb48, basecol div context.SamplesPerPixel);
          if context.LittleEndian then
            // LittleEndian
            for w := 0 to Width-1 do
            begin
              px_rgb48^.r := pw^; inc(pw);
              px_rgb48^.g := pw^; inc(pw);
              px_rgb48^.b := pw^; inc(pw);
              inc(px_rgb48);
            end
          else
            // BigEndian
            for w := 0 to Width-1 do
            begin
              px_rgb48^.r := IESwapWord(pw^); inc(pw);
              px_rgb48^.g := IESwapWord(pw^); inc(pw);
              px_rgb48^.b := IESwapWord(pw^); inc(pw);
              inc(px_rgb48);
            end;

        end
        else
        begin

          // convert to 24 bit
          px := outbuf.Scanline[baserow + q];
          inc(pbyte(px), basecol);
          if not context.LittleEndian then
            // BigEndian Format
            IEGlobalSettings().ConvertColorFunction(pw, iecmsRGB48_SE, px, iecmsBGR, Width, context.RefParams)
          else
            // LittleEndian format
            IEGlobalSettings().ConvertColorFunction(pw, iecmsRGB48, px, iecmsBGR, Width, context.RefParams);

        end;

        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // Error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // without ColorMap, RGB and 48 bit (16 per sample) + 1 extra channel
  else
  if (context.PhotometricInterpretation = 2) and (context.SamplesPerPixel = 4) and (context.BitsPerSample = 16) then
  begin
    if (context.Compression <> 1) then
      getmem(buf, Width * context.SamplesPerPixel * 2);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        pw := pword(buf);

        if outbuf.PixelFormat=ie48RGB then
        begin

          // native pixel format
          px_rgb48 := outbuf.Scanline[baserow+q];
          inc(px_rgb48, basecol div context.SamplesPerPixel);
          if context.LittleEndian then
            // LittleEndian
            for w := 0 to Width-1 do
            begin
              px_rgb48^.r := pw^; inc(pw);
              px_rgb48^.g := pw^; inc(pw);
              px_rgb48^.b := pw^; inc(pw);
              inc(pw);  // discard extra channel
              inc(px_rgb48);
            end
          else
            // BigEndian
            for w := 0 to Width-1 do
            begin
              px_rgb48^.r := IESwapWord(pw^); inc(pw);
              px_rgb48^.g := IESwapWord(pw^); inc(pw);
              px_rgb48^.b := IESwapWord(pw^); inc(pw);
              inc(pw);  // discard extra channel
              inc(px_rgb48);
            end;

        end
        else
        begin

          // convert to 24 bit
          px := outbuf.Scanline[baserow + q];
          inc(pbyte(px), basecol);

          // discard extra channel
          px_rgb48 := PRGB48(pw);
          pw2 := pw;
          for w := 0 to Width-1 do
          begin
            px_rgb48^.r := pw2^; inc(pw2);
            px_rgb48^.g := pw2^; inc(pw2);
            px_rgb48^.b := pw2^; inc(pw2);
            inc(pw2);  // the extra channel
            inc(px_rgb48);
          end;

          if not context.LittleEndian then
            // BigEndian Format
            IEGlobalSettings().ConvertColorFunction(pw, iecmsRGB48_SE, px, iecmsBGR, Width, context.RefParams)
          else
            // LittleEndian format
            IEGlobalSettings().ConvertColorFunction(pw, iecmsRGB48, px, iecmsBGR, Width, context.RefParams);

        end;

        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // Error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // without ColorMap, RGB and 16 bit
  else
  if (context.PhotometricInterpretation = 2) and (context.SamplesPerPixel = 1) and (context.BitsPerSample = 16) then
  begin
    if (context.Compression <> 1) then
      getmem(buf, Width * context.SamplesPerPixel);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        zbuf := buf;
        px := outbuf.Scanline[baserow + q];
        inc(pbyte(px), basecol);
        for w := 0 to Width - 1 do
        begin
          px^.r := (pword(zbuf)^ shr 10) shl 3;
          px^.g := ((pword(zbuf)^ shr 5) and $1F) shl 3;
          px^.b := (pword(zbuf)^ and $1F) shl 3;
          inc(zbuf, 2);
          inc(px);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // Error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // with RGB ColorMap (1 bit per pixel)
  else
  if (context.PhotometricInterpretation = 3) and (context.BitsPerSample = 1) and (context.SamplesPerPixel=1) then
  begin
    getmem(buf, Width*2);
    getmem(predbuf, brow); // previous line for CCITT 2D
    try
      with context do
      begin
        fillmemory(predbuf, brow, 255); // initialize
        CCITTposb := 0;
        for q := 0 to Height - 1 do
        begin
          px := outbuf.scanline[baserow + q];
          inc(pbyte(px), basecol);
          if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
          begin
            PerformPredictor(context, buf, Width);
            // RGB colormap
            if outbuf.PixelFormat = ie8p then
            begin
              for i := 0 to Width - 1 do
              begin
                if _GetPixelbw(buf, i)<>0 then
                  pbyte(px)^ := 1
                else
                  pbyte(px)^ := 0;
                inc(pbyte(px));
              end;
            end
            else
            begin
              for i := 0 to Width - 1 do
              begin
                if _GetPixelbw(buf, i)<>0 then
                  px^ := ColorMap^[1]
                else
                  px^ := ColorMap^[0];
                inc(px);
              end;
            end;
            // OnProgress
            with Progress do
            begin
              inc(val);
              if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
              begin
                lper := trunc(per1 * val);
                fOnProgress(Sender, lper);
              end;
            end;
            if Progress.Aborting^ then
              break;
          end
          else
          begin
            // error detected
            Progress.Aborting^ := True;
            break;
          end;
        end;
      end;
    finally
      freemem(predbuf);
      freemem(buf);
    end;
  end

  // with RGB ColorMap
  else
  if (context.PhotometricInterpretation = 3) and (context.BitsPerSample <= 8) then
  begin
    if (context.SamplesPerPixel = 2) and (context.BitsPerSample = 8) then
    begin
      // with alpha channel
      if ((context.Compression <> 1)) then
        getmem(buf, Width * 2);
      with context do
        for q := 0 to Height - 1 do
        begin
          px := outbuf.scanline[baserow + q];
          inc(pbyte(px), basecol);
          if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
          begin
            PerformPredictor(context, buf, Width);
            zbuf := buf;
            // RGB colormap
            for i := 0 to Width - 1 do
            begin
              px^ := ColorMap^[zbuf^];
              inc(zbuf);
              context.AlphaChannel.SetPixel(i, q + BaseRow, 255 - zbuf^);
              inc(zbuf); //
              inc(px);
            end;
            // OnProgress
            with Progress do
            begin
              inc(val);
              if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
              begin
                lper := trunc(per1 * val);
                fOnProgress(Sender, lper);
              end;
            end;
            if Progress.Aborting^ then
              break;
          end
          else
          begin
            // error detected
            Progress.Aborting^ := True;
            break;
          end;
        end;
      if ((context.Compression <> 1)) then
        freemem(buf);
    end
    else
    begin
      // without alpha channel
      if ((context.Compression <> 1)) or (context.BitsperSample < 8) then
        getmem(buf, Width*2);
      with context do
        for q := 0 to Height - 1 do
        begin
          px := outbuf.scanline[baserow + q];
          inc(pbyte(px), basecol);
          if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
          begin
            PerformPredictor(context, buf, Width);
            zbuf := buf;
            // RGB colormap
            if outbuf.PixelFormat = ie8p then
            begin
              for i := 0 to Width - 1 do
              begin
                pbyte(px)^ := zbuf^;
                inc(zbuf);
                inc(pbyte(px));
              end;
            end
            else
            begin
              for i := 0 to Width - 1 do
              begin
                px^ := ColorMap^[zbuf^];
                inc(zbuf);
                inc(px);
              end;
            end;
            // OnProgress
            with Progress do
            begin
              inc(val);
              if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
              begin
                lper := trunc(per1 * val);
                fOnProgress(Sender, lper);
              end;
            end;
            if Progress.Aborting^ then
              break;
          end
          else
          begin
            // error detected
            Progress.Aborting^ := True;
            break;
          end;
        end;
      if ((context.Compression <> 1)) or (context.BitsperSample < 8) then
        freemem(buf);
    end;
  end

  // gray levels (8 bit)
  else
  if (context.PhotometricInterpretation <= 1) and (context.SamplesPerPixel = 1) and
    ((context.BitsPerSample = 8) or (context.BitsPerSample = 4) or (context.BitsPerSample=2) or (context.BitsPerSample=7)) then
  begin
    if (context.Compression <> 1) or (context.BitsPerSample = 4) or (context.BitsPerSample=2) or (context.BitsPerSample=7) then
      getmem(buf, Width+4); // 3.0.0
    for q := 0 to Height - 1 do
    begin
      px := outbuf.scanline[baserow + q];
      inc(pbyte(px), basecol);
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        zbuf := buf;
        if outbuf.PixelFormat = ie8g then
        begin
          for w := 0 to Width - 1 do
          begin
            pbyte(px)^ := zbuf^;
            inc(zbuf);
            inc(pbyte(px));
          end;
        end
        else
        begin
          if context.PhotometricInterpretation = 0 then
            for w := 0 to Width - 1 do
            begin
              i := 255 - zbuf^;
              px^.r := i;
              px^.g := i;
              px^.b := i;
              inc(zbuf);
              inc(px);
            end
          else
            for w := 0 to Width - 1 do
            begin
              px^.r := zbuf^;
              px^.g := zbuf^;
              px^.b := zbuf^;
              inc(zbuf);
              inc(px);
            end;
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) or (context.BitsPerSample = 4) or (context.BitsPerSample=2) or (context.BitsPerSample=7) then
      freemem(buf);
  end

  // gray levels (8 bit) with SamplesPerPixel>=2 bytes
  else
  if (context.PhotometricInterpretation <= 1) and (context.SamplesPerPixel > 1) and ((context.BitsPerSample = 8)) then
  begin
    hasalpha := (not context.IgnoreAlpha);
    if hasalpha then
      context.AlphaChannel.Full := false;

    if ((context.Compression <> 1)) then
      getmem(buf, Width * context.SamplesPerPixel);

    getmem(pxx, Width * context.SamplesPerPixel);

    try

      for q := 0 to Height - 1 do
      begin
        px := outbuf.Scanline[baserow + q];
        inc(pbyte(px), basecol);
        if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
        begin
          PerformPredictor(context, buf, Width);
          zbuf := buf;
          if context.PhotometricInterpretation = 0 then
          begin
            for w := 0 to Width - 1 do
            begin
              i := 255 - zbuf^;
              px^.r := i;
              px^.g := i;
              px^.b := i;
              inc(zbuf);
              if hasalpha then
                context.AlphaChannel.SetPixel(w, q + BaseRow, zbuf^);
              for i := 1 to context.SamplesPerPixel-1 do
                inc(zbuf);
              inc(px);
            end
          end
          else
          begin
            pb := pxx;
            for w := 0 to Width - 1 do
            begin
              pb^ := zbuf^;
              inc(zbuf);
              if hasalpha then
                context.AlphaChannel.SetPixel(w, q + BaseRow, zbuf^);
              for i := 1 to context.SamplesPerPixel-1 do
                inc(zbuf);
              inc(pb);
            end;
            IEGlobalSettings().ConvertColorFunction(pxx, iecmsGray8, px, iecmsBGR, Width, context.RefParams);
          end;
          // OnProgress
          with Progress do
          begin
            inc(val);
            if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
            begin
              lper := trunc(per1 * val);
              fOnProgress(Sender, lper);
            end;
          end;
          if Progress.Aborting^ then
            break;
        end
        else
        begin
          // error detected
          Progress.Aborting^ := True;
          break;
        end;
      end;

    finally
      freemem(pxx);
      if (context.Compression <> 1)then
        freemem(buf);
    end;
    
  end

  // gray levels (16 bit)
  else
  if (context.PhotometricInterpretation <= 1) and (context.SamplesPerPixel = 1) and (context.BitsPerSample = 16) then
  begin
    if ((context.Compression <> 1)) then
      getmem(buf, Width * 2);
    for q := 0 to Height - 1 do
    begin
      px := outbuf.scanline[baserow + q];
      inc(pbyte(px), basecol);
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        wbuf := pword(buf);
        // special case
        if IECopy(context.Software, 1, 15) = 'Look@Molli v1.1' then
          e := 4
        else
          e := 8;
        //
        if outbuf.PixelFormat = ie16g then
        begin
          // native pixel format
          if context.PhotometricInterpretation = 0 then
            for w := 0 to Width - 1 do
            begin
              if not context.LittleEndian then
                wbuf^ := ((wbuf^ shr 8) and $00FF) or ((wbuf^ shl 8) and $FF00);  // swapWord
              pword(px)^ := 65535 - wbuf^;
              inc(wbuf);
              inc(pword(px));
            end
          else
          begin
            for w := 0 to Width - 1 do
            begin
              if not context.LittleEndian then
                wbuf^ := ((wbuf^ shr 8) and $00FF) or ((wbuf^ shl 8) and $FF00);  // swapWord
              pword(px)^ := wbuf^;
              if context.SampleFormat = 2 then  // two's complement signed integer data?
              begin
                // convert from -32768..32767 range to 0..65535
                pword(px)^ := 32768 + PSmallInt(px)^;
              end;
              inc(wbuf);
              inc(pword(px));
            end;
          end
        end
        else
        begin
          // convert to BGR
          if context.PhotometricInterpretation = 0 then
            for w := 0 to Width - 1 do
            begin
              if not context.LittleEndian then
                wbuf^ := ((wbuf^ shr 8) and $00FF) or ((wbuf^ shl 8) and $FF00);  // swapWord
              i := (65535 - wbuf^) shr e;
              px^.r := i;
              px^.g := i;
              px^.b := i;
              inc(wbuf);
              inc(px);
            end
          else
            for w := 0 to Width - 1 do
            begin
              if not context.LittleEndian then
                wbuf^ := ((wbuf^ shr 8) and $00FF) or ((wbuf^ shl 8) and $FF00);  // swapWord
              if context.SampleFormat = 2 then  // two's complement signed integer data?
              begin
                // convert from -32768..32767 range to 0..65535
                wbuf^ := 32768 + PSmallInt(wbuf)^;
              end;
              i := wbuf^ shr e;
              px^.r := i;
              px^.g := i;
              px^.b := i;
              inc(wbuf);
              inc(px);
            end;
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if context.Compression <> 1 then
      freemem(buf);
  end

  // gray levels (12 bit, packed) -> converted to 16 bit
  else
  if (context.PhotometricInterpretation <= 1) and (context.SamplesPerPixel = 1) and (context.BitsPerSample = 12) then
  begin
    if ((context.Compression <> 1)) then
      getmem(buf, Width * 2);
    for q := 0 to Height - 1 do
    begin
      px := outbuf.scanline[baserow + q];
      inc(pbyte(px), basecol);
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        w := 0;
        if outbuf.PixelFormat = ie16g then
        begin
          if context.PhotometricInterpretation = 0 then
          begin
            // todo
          end
          else
          begin
            while w < Width do
            begin
              // 8 + 4
              i := buf^ shl 4;  // get 8 bit
              inc(buf);
              i := i or ((buf^ and $F0) shr 4); // get 4 bit
              i := i shl 4; // 12 bit to 16 bit
              pword(px)^ := i;
              inc(w);
              inc(pword(px));
              if w = Width then
                break;
              // 4 + 8
              i := (buf^ and $0F) shl 8;  // get 4 bit
              inc(buf);
              i := i or buf^; // get 8 bit
              inc(buf);
              i := i shl 4; // 12 bit to 16 bit
              pword(px)^ := i;
              inc(w);
              inc(pword(px));
            end;
          end;
        end
        else
        begin
          if context.PhotometricInterpretation = 0 then
          begin
            // todo
          end
          else
          begin
            while w < Width do
            begin
              // 8 + 4
              i := buf^ shl 4;  // get 8 bit
              inc(buf);
              i := i or ((buf^ and $F0) shr 4); // get 4 bit
              i := i shr 4; // convert 12 bit to 8 bit
              px^.b := i;
              px^.g := i;
              px^.r := i;
              inc(w);
              inc(px);
              if w = Width then
                break;
              // 4 + 8
              i := (buf^ and $0F) shl 8;  // get 4 bit
              inc(buf);
              i := i or buf^; // get 8 bit
              inc(buf);
              i := i shr 4; // convert 12 bit to 8 bit
              px^.b := i;
              px^.g := i;
              px^.r := i;
              inc(w);
              inc(px);
            end;
          end;
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if context.Compression <> 1 then
      freemem(buf);
  end

  // Black/White
  else
  if (context.PhotometricInterpretation <= 1) and (context.SamplesPerPixel = 1) and (context.BitsPerSample = 1) then
  begin
    lxbuf := xbuf;
    getmem(buf, Width);
    getmem(predbuf, brow); // previous line for CCITT 2D
    try
      FillMemory(predbuf, brow, 255); // initialize
      CCITTposb := 0;
      if ((context.PhotometricInterpretation = 0) and ((context.Compression = 1) or (context.Compression >= 5))) or
        ((context.PhotometricInterpretation = 1) and ((context.Compression = 3) or (context.Compression = 4))) then
        inv := true
      else
        inv := false;
      for q := 0 to Height - 1 do
      begin
        px := outbuf.ScanLine[baserow + q];
        inc(pbyte(px), basecol);
        if (uint64(xbuf)-uint64(lxbuf)) >= sz then
          break;  // exceeded to read input data
        if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
        begin
          PerformPredictor(context, buf, Width);
          zbuf := buf;
          pxx := pbyte(px);
          case inv of
            true:
              for w := 0 to brow - 1 do
              begin
                pxx^ := not zbuf^;
                inc(zbuf);
                inc(pxx);
              end;
            false:
              CopyMemory(pxx, zbuf, brow);
          end;
          // OnProgress
          with Progress do
          begin
            inc(val);
            if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
            begin
              lper := trunc(per1 * val);
              fOnProgress(Sender, lper);
            end;
          end;
          if Progress.Aborting^ then
            break;
        end
        else
        begin
          // error detected
          Progress.Aborting^ := True;
          break;
        end;
      end;
    finally
      freemem(predbuf);
      freemem(buf);
    end;
  end

  // CMYK
  else
  if (context.PhotometricInterpretation = 5) and (context.SamplesPerPixel = 4) and (context.BitsPerSample = 8) then
  begin
    if (context.Compression <> 1) then
      getmem(buf, Width * 4);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        if outbuf.PixelFormat=ieCMYK then
        begin
          // native CMYK format
          px_cmyk := outbuf.scanline[baserow+q];
          inc(px_cmyk, basecol div 3);
          pb := buf;
          for w := 0 to Width-1 do
          begin
            px_cmyk^.c := 255-pb^; inc(pb);
            px_cmyk^.m := 255-pb^; inc(pb);
            px_cmyk^.y := 255-pb^; inc(pb);
            px_cmyk^.k := 255-pb^; inc(pb);
            inc(px_cmyk);
          end;
        end
        else
        begin
          // convert to 24bit
          px := outbuf.Scanline[baserow + q];
          inc(pbyte(px), basecol);
          // invert CMYK values, because IEConvertColorFunction wants normal values
          pb := buf;
          for w := 0 to Width- 1 do
          begin
            pb^ := 255 - pb^; inc(pb);
            pb^ := 255 - pb^; inc(pb);
            pb^ := 255 - pb^; inc(pb);
            pb^ := 255 - pb^; inc(pb);
          end;
          IEGlobalSettings().ConvertColorFunction(buf, iecmsCMYK, px, iecmsBGR, Width, context.RefParams);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // CMYK
  else
  if (context.PhotometricInterpretation = 5) and (context.SamplesPerPixel >= 5) and (context.BitsPerSample = 8) then
  begin
    hasalpha := (context.SamplesPerPixel > 4) and (context.ExtraSamples > 0) and not context.IgnoreAlpha;
    if hasalpha then
      context.AlphaChannel.Full := false;

    if (context.Compression <> 1) then
      getmem(buf, Width * context.SamplesPerPixel);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        if outbuf.PixelFormat=ieCMYK then
        begin
          // native CMYK format
          px_cmyk := outbuf.scanline[baserow+q];
          inc(px_cmyk, basecol div 3);
          if hasalpha then
          begin
            palpha := context.AlphaChannel.ScanLine[baserow + q];
            inc(palpha, basecol);
          end
          else
            palpha := nil;
          pb := buf;
          for w := 0 to Width-1 do
          begin
            px_cmyk^.c := 255-pb^; inc(pb);
            px_cmyk^.m := 255-pb^; inc(pb);
            px_cmyk^.y := 255-pb^; inc(pb);
            px_cmyk^.k := 255-pb^; inc(pb);
            if hasalpha then
            begin
              palpha^ := pb^; inc(palpha); inc(pb);
              for e := 2 to context.SamplesPerPixel-4 do
                inc(pb);
            end
            else
              for e := 1 to context.SamplesPerPixel-4 do
                inc(pb);
            inc(px_cmyk);
          end;
        end
        else
        begin
          // convert to 24bit
          px := outbuf.scanline[baserow + q];
          inc(pbyte(px), basecol);
          if hasalpha then
          begin
            palpha := context.AlphaChannel.ScanLine[baserow + q];
            inc(palpha, basecol);
          end
          else
            palpha := nil;
          // invert CMYK values, because IEConvertColorFunction wants normal values
          pb := buf;
          px_cmyk := PCMYK(buf);
          for w := 0 to Width- 1 do
          begin
            px_cmyk^.c := 255-pb^; inc(pb);
            px_cmyk^.m := 255-pb^; inc(pb);
            px_cmyk^.y := 255-pb^; inc(pb);
            px_cmyk^.k := 255-pb^; inc(pb);
            if hasalpha then
            begin
              palpha^ := pb^; inc(palpha); inc(pb);
              for e := 2 to context.SamplesPerPixel-4 do
                inc(pb);
            end
            else
              for e := 1 to context.SamplesPerPixel-4 do
                inc(pb);
            inc(px_cmyk);
          end;
          IEGlobalSettings().ConvertColorFunction(buf, iecmsCMYK, px, iecmsBGR, Width, context.RefParams);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // CMYK
  else
  if (context.PhotometricInterpretation = 5) and (context.SamplesPerPixel = 4) and (context.BitsPerSample = 16) then
  begin
    if (context.Compression <> 1) then
      getmem(buf, Width * 8);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        if not context.LittleEndian then
          IEChangeEndiannessWordArray(pword(buf), Width * 4);
        PerformPredictor(context, buf, Width);
        if outbuf.PixelFormat=ieCMYK then
        begin
          // native CMYK format
          px_cmyk := outbuf.scanline[baserow+q];
          inc(px_cmyk, basecol div 4);
          pw := pword(buf);
          for w := 0 to Width-1 do
          begin
            px_cmyk^.c := 255-pw^ shr 8; inc(pw);
            px_cmyk^.m := 255-pw^ shr 8; inc(pw);
            px_cmyk^.y := 255-pw^ shr 8; inc(pw);
            px_cmyk^.k := 255-pw^ shr 8; inc(pw);
            inc(px_cmyk);
          end;
        end
        else
        begin
          // convert to 24bit
          px := outbuf.scanline[baserow + q];
          inc(pbyte(px), basecol);
          // invert and shift right CMYK values, because IEConvertColorFunction wants normal values
          pw := pword(buf);
          getmem(buf1, Width*4);
          pb := pbyte(buf1);
          for w := 0 to Width- 1 do
          begin
            pb^ := 255 - pw^ shr 8; inc(pb); inc(pw);
            pb^ := 255 - pw^ shr 8; inc(pb); inc(pw);
            pb^ := 255 - pw^ shr 8; inc(pb); inc(pw);
            pb^ := 255 - pw^ shr 8; inc(pb); inc(pw);
          end;
          IEGlobalSettings().ConvertColorFunction(buf1, iecmsCMYK, px, iecmsBGR, Width, context.RefParams);
          freemem(buf1);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // CIE L*a*b*
  else
  if (context.PhotometricInterpretation = 8) and (context.SamplesPerPixel = 3) and (context.BitsPerSample = 8) then
  begin
    if (context.Compression <> 1) then
      getmem(buf, Width * 3);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);
        //lab := PIELAB(buf);
        px := outbuf.scanline[baserow + q];
        inc(pbyte(px), basecol);
        IEGlobalSettings().ConvertColorFunction(buf, iecmsCIELab, px, iecmsBGR, Width, context.RefParams);
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
  end

  // YCbCr - YCbCrSubSampling = 2, 1
  else
  if (context.PhotometricInterpretation = 6) and (context.SamplesPerPixel = 3) and
    (context.BitsPerSample = 8) and (context.YCbCrSubSampling[0]=2) and (context.YCbCrSubSampling[1]=1) then
  begin
    brow := Width*2;
    if (context.Compression <> 1) then
      getmem(buf, Width * 3);
    getmem(ycbcr, Width*3);
    for q := 0 to Height - 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf, rlepos) then
      begin
        PerformPredictor(context, buf, Width);

        // convert YCbCr: 2: 1 to 1: 1
        pba := pbytearray(buf);
        px_ycbcr := ycbcr;
        w := 0;
        while w<width do
        begin
          px_ycbcr^.y := pba[0];
          px_ycbcr^.Cb := pba[2];
          px_ycbcr^.Cr := pba[3];
          inc(px_ycbcr);
          px_ycbcr^.y := pba[1];
          px_ycbcr^.Cb := pba[2];
          px_ycbcr^.Cr := pba[3];
          inc(px_ycbcr);
          inc(pbyte(pba), 4);
          inc(w, 2);
        end;

        px := outbuf.scanline[baserow + q];
        inc(pbyte(px), basecol);
        IEGlobalSettings().ConvertColorFunction(ycbcr, iecmsYCbCr, px, iecmsBGR, Width, context.RefParams);
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) and (trunc(per1 * val)<>lper) then
          begin
            lper := trunc(per1 * val);
            fOnProgress(Sender, lper);
          end;
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
    freemem(ycbcr);
  end

  // YCbCr - YCbCrSubSampling = 2, 2
  else
  if (context.PhotometricInterpretation = 6) and (context.SamplesPerPixel = 3) and
    (context.BitsPerSample = 8) and (context.YCbCrSubSampling[0]=2) and (context.YCbCrSubSampling[1]=2) then
  begin

    (* not still working
    brow := Width*3;
    if (context.Compression <> 1) then
      getmem(buf, Width * 3);
    getmem(ycbcr, Width*3);
    for q := 0 to Height- 1 do
    begin
      if GetNextLine(q, buf, xbuf, sz, context, Width, brow, LzwId, predbuf, CCITTposb, zipbuf) then
      begin
        PerformPredictor(context, buf, Width);

        // convert YCbCr: 2: 2 to 1: 1

       pba := pbytearray(buf);
        px_ycbcr := ycbcr;
        w := 0;
        while w<width do
        begin
          px_ycbcr^.y := pba[2];
          px_ycbcr^.Cb := 128;
          px_ycbcr^.Cr := 128;
          inc(px_ycbcr);
          px_ycbcr^.y := pba[3];
          px_ycbcr^.Cb := 128;
          px_ycbcr^.Cr := 128;
          inc(px_ycbcr);

          inc(pbyte(pba), 6);
          inc(w, 2);
        end;
        px := outbuf.scanline[baserow + q];
        inc(pbyte(px), basecol);
        IEConvertColorFunction(ycbcr, iecmsYCbCr, px, iecmsBGR, Width, context.RefParams);

        pba := pbytearray(buf);
        px_ycbcr := ycbcr;
        w := 0;
        while w<width do
        begin
          px_ycbcr^.y := pba[2];
          px_ycbcr^.Cb := 128;
          px_ycbcr^.Cr := 128;
          inc(px_ycbcr);
          px_ycbcr^.y := pba[3];
          px_ycbcr^.Cb := 128;
          px_ycbcr^.Cr := 128;
          inc(px_ycbcr);

          inc(pbyte(pba), 6);
          inc(w, 2);
        end;
        px := outbuf.scanline[baserow + q*2+1];
        inc(pbyte(px), basecol);
        IEConvertColorFunction(ycbcr, iecmsYCbCr, px, iecmsBGR, Width, context.RefParams);

        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
        if Progress.Aborting^ then
          break;
      end
      else
      begin
        // error detected
        Progress.Aborting^ := True;
        break;
      end;
    end;
    if (context.Compression <> 1) then
      freemem(buf);
    freemem(ycbcr);
    *)
  end;

  if (context.Compression = 5) and (LzwId<>nil) then
    // free LZW compressor
    context.LZWDecompFunc(nil, 0, LzwId, context.FillOrder);
  if zipbuf<>nil then
    freemem(zipbuf); // zip buffer
end;

// decompress buffer (PlanarConfiguration=2)
procedure Decompress2(context: TTIFFReaderContext; outbuf: TIEBitmap; baserow: integer; xbufn: array of pbyte; szn: array of integer; Width, Height: integer; var Progress: TProgressRec);
var
  q, w, c: integer;
  px: PRGB;
  bufv, bufn, zbufn: array [0..IEMAXEXTRASAMPLES-1] of pbyte;
  //pw_srcn: array [0..IEMAXEXTRASAMPLES-1] of pword;
  predbuf: pbyte;
  LZWn: array [0..IEMAXEXTRASAMPLES-1] of pointer;
  brow: integer; // size in byte of one row (decompressed, not compacted)
  CCITTposb: integer;
  zipbufn: array [0..IEMAXEXTRASAMPLES-1] of pbyte;
  rleposn: array [0..IEMAXEXTRASAMPLES-1] of integer;
  cmyk_buf: ^TCMYKROW;
  px_cmyk: PCMYK;
  //px_srcRGB48, px_dstRGB48: PRGB48;
  RGB48_src: array of TRGB48;
  px_byte: pbyte;
  px_srcword, px_dstword: pword;
  byte_src: array of byte;
begin
  predbuf := nil;
  cmyk_buf := nil;
  for c := 0 to IEMAXEXTRASAMPLES-1 do
  begin
    zipbufn[c] := nil;
    rleposn[c] := 0;
    LZWn[c] := nil;
    bufn[c] := nil;
    bufv[c] := nil;
  end;

  // calc brow
  brow := trunc(Width * (context.BitsPerSample / 8));
  if (context.BitsPerSample = 4) and (Width and 1 <> 0) then
    inc(brow);

  try

    if (context.PhotometricInterpretation = 3) and (context.SamplesPerPixel = 1) and (context.BitsPerSample = 8) then
    begin
      // with ColorMap, RGB 24 bit (8bits per pixel)
      if context.Compression <> 1 then
      begin
        getmem(bufv[0], Width);
        bufn[0] := bufv[0];
      end;
      for q := 0 to Height - 1 do
      begin

        if not GetNextLine(q, bufn[0], xbufn[0], szn[0], context, Width, brow, LZWn[0], predbuf, CCITTposb, zipbufn[0], rleposn[0]) then
        begin
          Progress.Aborting^ := True;
          exit; // memory released in "finally"
        end;
        zbufn[0] := bufn[0];

        if outbuf.PixelFormat = ie8p then
        begin
          px_byte := outbuf.Scanline[baserow + q];
          for w := 0 to Width - 1 do
          begin
            px_byte^ := zbufn[0]^;
            inc(zbufn[0]);
            inc(px_byte);
          end;
          PerformPredictor(context, outbuf.scanline[baserow + q], Width);
        end
        else
        begin
          px := outbuf.Scanline[baserow + q];
          for w := 0 to Width - 1 do
          begin
            px^.r := context.ColorMap[zbufn[0]^].r;
            px^.g := context.ColorMap[zbufn[0]^].g;
            px^.b := context.ColorMap[zbufn[0]^].b;
            inc(zbufn[0]);
            inc(px);
          end;
          PerformPredictor(context, outbuf.scanline[baserow + q], Width);
        end;

        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
        if Progress.Aborting^ then
          break;
      end;
    end

    else
    if (context.PhotometricInterpretation = 2) and (context.SamplesPerPixel = 3) and (context.BitsPerSample = 8) then
    begin
      // without ColorMap, RGB 24 bit (8bits per pixel)
      if context.Compression <> 1 then
        for c := 0 to context.SamplesPerPixel - 1 do
        begin
          getmem(bufv[c], Width);
          bufn[c] := bufv[c];
        end;
      for q := 0 to Height - 1 do
      begin

        for c := 0 to context.SamplesPerPixel - 1 do
        begin
          if not GetNextLine(q, bufn[c], xbufn[c], szn[c], context, Width, brow, LZWn[c], predbuf, CCITTposb, zipbufn[c], rleposn[c]) then
          begin
            Progress.Aborting^ := True;
            exit; // memory released in "finally"
          end;
          zbufn[c] := bufn[c];
        end;

        px := outbuf.Scanline[baserow + q];
        for w := 0 to Width - 1 do
        begin
          px^.r := zbufn[0]^;
          inc(zbufn[0]);
          px^.g := zbufn[1]^;
          inc(zbufn[1]);
          px^.b := zbufn[2]^;
          inc(zbufn[2]);
          inc(px);
        end;
        PerformPredictor(context, outbuf.scanline[baserow+q], Width);

        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
        if Progress.Aborting^ then
          break;
      end;
    end

    else
    if (context.PhotometricInterpretation = 2) and (context.SamplesPerPixel = 3) and (context.BitsPerSample = 16) then
    begin
      // without ColorMap, RGB - 48 bit (16bits per pixel)
      if context.Compression <> 1 then
        SetLength(byte_src, Width * 2);
      SetLength(RGB48_src, Width);
      for q := 0 to Height - 1 do
      begin

        for c := 0 to context.SamplesPerPixel - 1 do
        begin
          px_byte := @byte_src[0];
          if not GetNextLine(q, px_byte, xbufn[c], szn[c], context, Width, brow, LZWn[c], predbuf, CCITTposb, zipbufn[c], rleposn[c]) then
          begin
            Progress.Aborting^ := True;
            exit;
          end;
          PerformPredictor(context, px_byte, Width);
          px_srcword := pword(px_byte);
          px_dstword := @RGB48_src[0];
          inc(px_dstword, c);
          for w := 0 to Width - 1 do
          begin
            px_dstword^ := px_srcword^;
            inc(px_srcword);
            inc(px_dstword, 3);
          end;
        end;

        if outbuf.PixelFormat = ie48RGB then
        begin
          CopyMemory(outbuf.Scanline[baserow + q], @RGB48_src[0], Width * sizeof(TRGB48));
        end
        else
        if outbuf.PixelFormat = ie24RGB then
        begin
          IEGlobalSettings().ConvertColorFunction(@RGB48_src[0], iecmsRGB48, outbuf.Scanline[baserow + q], iecmsBGR, Width, context.RefParams);
        end;

        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
        if Progress.Aborting^ then
          break;
      end;
    end

    else
    if (context.PhotometricInterpretation = 5) and (context.SamplesPerPixel >= 4) and (context.BitsPerSample = 8) then
    begin
      // CMYK
      if (context.Compression <> 1) then
        for c := 0 to context.SamplesPerPixel-1 do
        begin
          getmem(bufv[c], Width * context.SamplesPerPixel);
          bufn[c] := bufv[c];
        end;

      getmem(cmyk_buf, sizeof(TCMYK)*Width);

      for q := 0 to Height - 1 do
      begin

        for c := 0 to context.SamplesPerPixel-1 do
        begin
          if not GetNextLine(q, bufn[c], xbufn[c], szn[c], context, Width, brow, LZWn[c], predbuf, CCITTposb, zipbufn[c], rleposn[c]) then
          begin
            Progress.Aborting^ := True;
            exit; // memory released in "finally"
          end;
          zbufn[c] := bufn[c];
          PerformPredictor(context, bufn[c], Width, true);
        end;

        if outbuf.PixelFormat=ieCMYK then
        begin
          // native CMYK format
          px_cmyk := outbuf.Scanline[baserow + q];
          for w := 0 to Width-1 do
          begin
            px_cmyk^.c := 255 - zbufn[0]^; inc(zbufn[0]);
            px_cmyk^.m := 255 - zbufn[1]^; inc(zbufn[1]);
            px_cmyk^.y := 255 - zbufn[2]^; inc(zbufn[2]);
            px_cmyk^.k := 255 - zbufn[3]^; inc(zbufn[3]);
            inc(px_cmyk);
          end;
        end
        else
        begin
          // convert to 24bit
          px := outbuf.scanline[baserow + q];
          // invert CMYK values, because IEConvertColorFunction wants normal values
          for w := 0 to Width- 1 do
          begin
            cmyk_buf[w].c := 255 - zbufn[0]^; inc(zbufn[0]);
            cmyk_buf[w].m := 255 - zbufn[1]^; inc(zbufn[1]);
            cmyk_buf[w].y := 255 - zbufn[2]^; inc(zbufn[2]);
            cmyk_buf[w].k := 255 - zbufn[3]^; inc(zbufn[3]);
          end;
          IEGlobalSettings().ConvertColorFunction(cmyk_buf, iecmsCMYK, px, iecmsBGR, Width, context.RefParams);
        end;

        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
        if Progress.Aborting^ then
          break;

      end;
    end

  finally

    for c := 0 to IEMAXEXTRASAMPLES-1 do
    begin
      if bufv[c]<>nil then
        freemem(bufv[c]);
      if (context.Compression=5) and (LZWn[c]<>nil) then
        context.LZWDecompFunc(nil, 0, LZWn[c], context.FillOrder);
      if zipbufn[c]<>nil then
        freemem(zipbufn[c]);
    end;
    if cmyk_buf<>nil then
      freemem(cmyk_buf);

  end;

end;


procedure LoadSimpleJpegV6(IFD: TIETIFFIFDReader; context: TTIFFReaderContext; var Bitmap: TIEBitmap; var Progress: TProgressRec);
var
  ms: TMemoryStream;
  ioparams: TIOParamsVals;
  l: integer;
  tlr: integer;
  i: integer;
  tmpBMP: TIEBitmap;
  nullpr: TProgressRec;
  w: word;
begin
  ioparams := TIOParamsVals.Create(nil);
  ms := TMemoryStream.Create;

  try

    if (context.StripByteCounts <> nil) then
    begin

      Bitmap.Allocate(context.ImageWidth, context.ImageHeight, ie24RGB);
      tmpBMP := TIEBitmap.Create;
      try
        for i := 0 to context.StripOffsets_Num - 1 do
        begin
          ms.Clear;
          IFD.Stream.Seek(IFD.StreamBase + context.JPEGInterchangeFormat, soBeginning);
          if context.JPEGInterchangeFormatLength > 0 then
            IECopyFrom(ms, IFD.Stream, context.JPEGInterchangeFormatLength);
          if i>0 then
          begin
            IFD.Stream.Seek(IFD.StreamBase + context.StripOffsets^[0], soBeginning);
            IFD.Stream.Seek(2, soCurrent);
            IFD.Stream.Read(w, 2); w := IESwapWord(w);
            IFD.Stream.Seek(-4, soCurrent);
            if w + 2 > 0 then
              IECopyFrom(ms, IFD.Stream, w + 2);
          end;
          IFD.Stream.Seek(IFD.StreamBase + context.StripOffsets^[i], soBeginning);
          l := i64min( context.StripByteCounts[i] , IFD.Stream.Size - IFD.Stream.Position );
          if l > 0 then
            IECopyFrom(ms, IFD.Stream, l);
          ms.position := 0;
          if context.StripOffsets_Num>1 then
          begin
            nullpr.fOnProgress := nil;
            nullpr.Aborting := Progress.Aborting;
          end
          else
            nullpr := Progress;
          tlr := ReadJpegStream(ms, nil, tmpBMP, ioparams, nullpr, false, false, false, false, true, false, context.RowsPerStrip, ioparams.IsNativePixelFormat);
          if (tlr > (context.RowsPerStrip div 2)) and Progress.Aborting^ then
            Progress.Aborting^ := false;  // >50% read, acceptable (2.3.1 -  ref: tif\8907_1_0_Problem_greyscaleJPEG_image.tif)
          tmpBMP.CopyRectTo(Bitmap, 0, 0, 0, i * context.RowsPerStrip, context.ImageWidth, context.RowsPerStrip);
          with Progress do
            if assigned(fOnProgress) then
              fOnProgress(Sender, trunc( i / context.StripOffsets_Num * 100 ));
        end;
      finally
        tmpBMP.free;
      end;

    end
    else
    begin

      if (context.JPEGInterchangeFormat = 0) and (context.StripOffsets <> nil) then
        // sometimes jpeg 6 is included in a strip instead of JPEGInterchangeFormat tag
        IFD.Stream.Seek(IFD.StreamBase + context.StripOffsets^[0], soBeginning)
      else
        IFD.Stream.Seek(IFD.StreamBase + context.JPEGInterchangeFormat, soBeginning);
      l := IEGetJpegLength(IFD.Stream); // sometimes JPEGInterchangeFormatLength is invalid and not cover the entire jpeg, then IEGetJpegLength is needed
      if (l >= 0) and (IFD.Stream.Position+ l <= IFD.Stream.Size) then
      begin
        if l > 0 then
          IECopyFrom(ms, IFD.Stream, l);
        ms.position := 0;
        ReadJpegStream(ms, nil, Bitmap, ioparams, Progress, false, false, false, false, true, true, -1, ioparams.IsNativePixelFormat);
      end;

    end;

  finally
    FreeAndNil(ms);
    FreeAndNil(ioparams);
  end;
end;


procedure LoadIPTC(IFD: TIETIFFIFDReader; context: TTIFFReaderContext; Params: TIOParamsVals);
var
  data: array of byte;
  t: integer;
begin
  Params.IPTC_Info.Clear();
  t := IFD.FindTAG(IETIFFTAG_IPTC);
  if t >= 0 then
  begin
    SetLength(data, IFD.GetDataLengthInBytes(t));
    IFD.Stream.Seek(IFD.StreamBase + IFD.GetDataPos(t), soBeginning);
    IFD.Stream.Read(data[0], length(data));
    Params.IPTC_Info.LoadFromStandardBuffer(@data[0], length(data));
  end;
end;


procedure LoadXMP(IFD: TIETIFFIFDReader; context: TTIFFReaderContext; Params: TIOParamsVals);
var
  t: integer;
  info: AnsiString;
  dpos, dnum: int64;
  dtyp: word;
begin
  Params.XMP_Info := '';
  t := IFD.FindTAG(IETIFFTAG_XMP);
  if (t >= 0) then
  begin
    dnum := IFD.GetDataNum(t);
    dpos := IFD.GetDataPos(t);
    dtyp := IFD.GetDataType(t);
    if ((dtyp = IETIFFTYPE_BYTE) or (dtyp = IETIFFTYPE_UNDEFINED)) then
    begin
      IFD.Stream.Seek(IFD.StreamBase + dpos, soBeginning);
      SetLength(info, dnum);
      IFD.Stream.Read(info[1], dnum);
      Params.XMP_Info := info;
    end;
  end;
end;


procedure ReadEXIFUserComment(IFD: TIETIFFIFDReader; Params: TIOParamsVals);
var
  tempAnsiString: AnsiString;
  tempWideString: WideString;
  wlen: integer;
begin
  tempAnsiString := IFD.ReadString($9286, false);
  wlen := length(tempAnsiString) - 8;
  if wlen > 0 then
  begin
    Params.EXIF_UserCommentCode := IECopy(tempAnsiString, 1, 8);
    if Params.EXIF_UserCommentCode = IEEXIFUSERCOMMENTCODE_UNICODE then
    begin
      // IEEXIFUSERCOMMENTCODE_UNICODE
      SetLength(tempWideString, IECeil(wlen / 2));
      CopyMemory(@tempWideString[1], @tempAnsiString[9], wlen);
      Params.EXIF_UserComment := tempWideString;
    end
    else
    if Params.EXIF_UserCommentCode = IEEXIFUSERCOMMENTCODE_ASCII then
    begin
      // IEEXIFUSERCOMMENTCODE_ASCII
      Params.EXIF_UserComment := WideString(IECopy(tempAnsiString, 9, wlen));
    end
    else
    begin
      // IEEXIFUSERCOMMENTCODE_JIS, IEEXIFUSERCOMMENTCODE_UNDEFINED
      Params.EXIF_UserComment := WideString(IECopy(tempAnsiString, 9, wlen));
    end;
  end
  else
  begin
    Params.EXIF_UserCommentCode := #$55#$4E#$49#$43#$4F#$44#$45#$00;  // default UNICODE
    Params.EXIF_UserComment := ''; // default empty
  end;

end;


procedure TIFFReadStream(Bitmap: TIEBitmap; Stream: TStream; var numi: integer; IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; var AlphaChannel: TIEMask; TranslateBase: boolean; IgnoreAlpha: boolean; IsExifThumb: boolean; IsEXIFData: boolean; ProvidedHeader: PTIFFHeader = nil);
var
  context: TTIFFReaderContext;
begin
  context := TTIFFReaderContext.Create();
  try
    context.ReadStream(Bitmap, Stream, numi, IOParams, Progress, Preview, AlphaChannel, TranslateBase, IgnoreAlpha, IsExifThumb, IsEXIFData, ProvidedHeader);
  finally
    context.Free();
  end;
end;

// read a TIFF stream
// Bitmap: bitmap to write
// numi: images count (output)
procedure TTIFFReaderContext.ReadStream(Bitmap: TIEBitmap; Stream: TStream; var numi: integer; IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; var AlphaChannel: TIEMask; TranslateBase: boolean; IgnoreAlpha: boolean; IsExifThumb: boolean; IsEXIFData: boolean; ProvidedHeader: PTIFFHeader = nil);
var
  PosIFD: int64;
  q, w: integer;
  dd: double;
  laccess: TIEDataAccess;
  imageAllocationOk: boolean;
begin
  numi := 0;
  try
    RefParams := IOParams;

    // setup input stream
    MainIFD.Stream := Stream;
    if TranslateBase then
      MainIFD.StreamBase := Stream.Position
    else
      MainIFD.StreamBase := 0;

    // read header
    if not TIFFReadHeader(Stream, ProvidedHeader, MainIFD.LittleEndian, MainIFD.IsBigTIFF, MainIFD.DataPosSize, PosIFD) then
    begin
      Progress.Aborting^ := True;
      exit;
    end;

    LittleEndian := MainIFD.LittleEndian;

    // read main IFD (of the selected image)
    q := IOParams.TIFF_ImageIndex;
    if not MainIFD.ReadIFD(q, PosIFD, numi) then
      exit;

    // Sub-IFD (loaded only on request, when TIFF_SubIndex > -1), replaces the main-IFD
    if IOParams.TIFF_SubIndex > -1 then
    begin
      PosIFD := MainIFD.ReadInteger(IETIFFTAG_SUBIFD, IOParams.TIFF_SubIndex, 0);
      if PosIFD > 0 then
      begin
        // replaces MainIFD
        MainIFD.ReadIFD(0, PosIFD, q);
      end;
    end;

    // EXIF-IFD
    PosIFD := MainIFD.ReadInteger(IETIFFTAG_EXIFIFD, 0, 0);
    if PosIFD > 0 then
    begin
      ExifIFD.LittleEndian := MainIFD.LittleEndian;
      ExifIFD.Stream       := MainIFD.Stream;
      ExifIFD.StreamBase   := IEIFI(isEXIFData, MainIFD.StreamBase, 0);
      ExifIFD.IsBigTIFF    := false;
      ExifIFD.DataPosSize  := 4;
      ExifIFD.ReadIFD(0, PosIFD, q);
      IOParams.EXIF_HasEXIFData := true;
    end
    else
    if isEXIFData then
      ExifIFD.Assign(MainIFD);

    // Get GPS-EXIF data
    PosIFD := MainIFD.ReadInteger(IETIFFTAG_EXIFGPSIFD, 0, 0);
    if PosIFD > 0 then
    begin
      GpsIFD.LittleEndian := MainIFD.LittleEndian;
      GpsIFD.Stream       := MainIFD.Stream;
      GpsIFD.StreamBase   := 0;
      GpsIFD.IsBigTIFF    := false;
      GpsIFD.DataPosSize  := 4;
      GpsIFD.ReadIFD(0, PosIFD, q);
    end;

    // Get Interoperability IFD
    PosIFD := ExifIFD.ReadInteger(IETIFFTAG_INTEROPIFD, 0, 0);
    if PosIFD > 0 then
    begin
      InteropIFD.LittleEndian := ExifIFD.LittleEndian;
      InteropIFD.Stream       := ExifIFD.Stream;
      InteropIFD.StreamBase   := 0;
      InteropIFD.IsBigTIFF    := false;
      InteropIFD.DataPosSize  := 4;
      InteropIFD.ReadIFD(0, PosIFD, q);
    end;

    // Decode TAGS
    TileWidth  := MainIFD.ReadInteger(322, 0, -1);
    TileLength := MainIFD.ReadInteger(323, 0, -1);
    if IOParams.TIFF_GetTile=-1 then
    begin
      ImageWidth  := MainIFD.ReadInteger(256, 0, 0);
      ImageHeight := MainIFD.ReadInteger(257, 0, 0);
    end
    else
    begin
      ImageWidth  := TileWidth;
      ImageHeight := TileLength;
    end;
    SamplesPerPixel := MainIFD.ReadInteger(277, 0, 1);
    if not ReadBitsPerSample(MainIFD, self) then
    begin
      Progress.Aborting^ := True;
      exit;
    end;
    RowsPerStrip := imin( MainIFD.ReadInteger(278, 0, ImageHeight), ImageHeight);
    if (RowsPerStrip = -1) or (RowsPerStrip = 0) then
      RowsPerStrip := ImageHeight;
    SampleFormat                := MainIFD.ReadInteger(339, 0, 1);
    PhotometricInterpretation   := MainIFD.ReadInteger(262, 0, 2);
    PlanarConfiguration         := MainIFD.ReadInteger(284, 0, 1);
    Orientation                 := MainIFD.ReadInteger(274, 0, 1);
    Compression                 := MainIFD.ReadInteger(259, 0, 1);
    Predictor                   := MainIFD.ReadInteger(317, 0, 1);
    T4Options                   := MainIFD.ReadInteger(292, 0, 0);
    T6Options                   := MainIFD.ReadInteger(293, 0, 0);
    FillOrder                   := MainIFD.ReadInteger(266, 0, 1);
    if (FillOrder <> 1) and (FillOrder <> 2) then
      FillOrder := 1;  // some tiffs have FillOrder<>[1, 2]
    Software                    := MainIFD.ReadString(305);
    JPEGProc                    := MainIFD.ReadInteger(512, 0, 0);
    JPEGInterchangeFormat       := MainIFD.ReadInteger(513, 0, 0);
    JPEGInterchangeFormatLength := MainIFD.ReadInteger(514, 0, 0);
    JPEGRestartInterval         := MainIFD.ReadInteger(515, 0, 0);
    w := imin(SamplesPerPixel, 7);
    for q := 0 to w - 1 do
    begin
      JPEGLosslessPredictors[q] := MainIFD.ReadInteger(517, q, 0);
      JPEGPointTransforms[q]    := MainIFD.ReadInteger(518, q, 0);
      JPEGQTables[q]            := MainIFD.ReadInteger(519, q, 0);
      JPEGDCTables[q]           := MainIFD.ReadInteger(520, q, 0);
      JPEGACTables[q]           := MainIFD.ReadInteger(521, q, 0);
    end;
    ExtraSamples := MainIFD.ReadInteger(338, 0, 0);
    TIFFReadExtraSamples(MainIFD, self);

    YCbCrSubSampling[0] := MainIFD.ReadInteger(530, 0, 0);
    if YCbCrSubSampling[0] = 0 then
      YCbCrSubSampling[0] := 2;

    YCbCrSubSampling[1] := MainIFD.ReadInteger(530, 1, 0);
    if YCbCrSubSampling[1] = 0 then
      YCbCrSubSampling[1] := 2;

    StripOffsets_Num    := MainIFD.ReadArrayIntegers(StripOffsets, 273);
    StripByteCounts_Num := MainIFD.ReadArrayIntegers(StripByteCounts, 279);
    TileOffsets_Num     := MainIFD.ReadArrayIntegers(TileOffsets, 324);
    TileByteCounts_Num  := MainIFD.ReadArrayIntegers(TileByteCounts, 325);
    
    // fix wrong files of ACDSee 3.1
    if (SamplesPerPixel = 1) and (BitsPerSample = 1) and (Compression > 0) and (Compression < 5) and (PhotometricInterpretation = 2) then
      PhotometricInterpretation := 0;

    JPEGTables := MainIFD.ReadRawData(347, JPEGTablesSize); // for Compression=7

    ReadColorMap(MainIFD, self);
    ReadTransferFunction(MainIFD, self);

    // IPTC
    LoadIPTC(MainIFD, self, IOParams);

    // XMP
    LoadXMP(MainIFD, self, IOParams);

    // Photoshop image resources information
    IOParams.TIFF_PhotoshopImageResources := MainIFD.ReadRawDataAsArrayOfByte(IETIFFTAG_PHOTOSHOP);

    // Photoshop "ImageSourceData"
    IOParams.TIFF_PhotoshopImageSourceData := MainIFD.ReadRawDataAsArrayOfByte(37724);

    {$ifdef IEINCLUDEIMAGINGANNOT}
    // Wang annotations
    LoadWang(MainIFD, IOParams);
    {$endif}

    // ImageEn annotations
    LoadImageEnAnnot(MainIFD, IOParams);

    // ICC profile
    LoadICC(MainIFD, IOParams);

    // set TIOParamsVals parameters
    with IOParams do
    begin
      BitsPerSample   := self.BitsPerSample;
      SamplesPerPixel := self.SamplesPerPixel;
      Width           := self.ImageWidth;
      Height          := self.ImageHeight;
      OriginalWidth   := self.ImageWidth;
      OriginalHeight  := self.ImageHeight;
      TIFF_BigTIFF    := MainIFD.IsBigTIFF;
      if self.LittleEndian then
        TIFF_ByteOrder := ioLittleEndian
      else
        TIFF_ByteOrder := ioBigEndian;
      TIFF_ImageCount  := numi;
      TIFF_Orientation := self.Orientation;
      case self.Compression of
        1: TIFF_Compression := ioTIFF_UNCOMPRESSED;
        2: TIFF_Compression := ioTIFF_CCITT1D;
        3: if self.T4Options and 1 = 0 then
             TIFF_Compression := ioTIFF_G3FAX1D
           else
             TIFF_Compression := ioTIFF_G3FAX2D;
        4:     TIFF_Compression := ioTIFF_G4FAX;
        5:     TIFF_Compression := ioTIFF_LZW;
        6:     TIFF_Compression := ioTIFF_OLDJPEG;
        7:     TIFF_Compression := ioTIFF_JPEG;
        32773: TIFF_Compression := ioTIFF_PACKBITS;
        32946: TIFF_Compression := ioTIFF_ZIP;
        8:     TIFF_Compression := ioTIFF_DEFLATE;
      else
        TIFF_Compression := ioTIFF_UNKNOWN;
      end;
      case self.PhotometricInterpretation of
        0: TIFF_PhotometInterpret := ioTIFF_WHITEISZERO;
        1: TIFF_PhotometInterpret := ioTIFF_BLACKISZERO;
        2: TIFF_PhotometInterpret := ioTIFF_RGB;
        3: TIFF_PhotometInterpret := ioTIFF_RGBPALETTE;
        4: TIFF_PhotometInterpret := ioTIFF_TRANSPMASK;
        5: TIFF_PhotometInterpret := ioTIFF_CMYK;
        6: TIFF_PhotometInterpret := ioTIFF_YCBCR;
        8: TIFF_PhotometInterpret := ioTIFF_CIELAB;
      end;
      TIFF_PlanarConf       := self.PlanarConfiguration;
      TIFF_DocumentName     := MainIFD.ReadString(269);
      TIFF_ImageDescription := MainIFD.ReadString(270);
      TIFF_PageName         := MainIFD.ReadString(285);
      TIFF_PageNumber       := MainIFD.ReadInteger(297, 0, 0);
      TIFF_PageCount        := MainIFD.ReadInteger(297, 1, 0);
      TIFF_FillOrder        := self.FillOrder;
      TIFF_NewSubfileType   := MainIFD.ReadInteger(254, 0, 0);

      // EXIF (not in sub IFD)
      EXIF_ImageDescription := TIFF_ImageDescription;
      EXIF_Make             := MainIFD.ReadString(271);
      EXIF_Model            := MainIFD.ReadString(272);
      EXIF_DateTime         := MainIFD.ReadString(306);
      EXIF_Orientation      := self.Orientation;
      EXIF_XResolution      := MainIFD.ReadRational(282, 0, IEGlobalSettings().DefaultDPIX);
      EXIF_YResolution      := MainIFD.ReadRational(283, 0, IEGlobalSettings().DefaultDPIY);
      EXIF_ResolutionUnit   := MainIFD.ReadInteger(296, 0, 2);
      EXIF_Software         := self.Software;
      EXIF_XPRating         := MainIFD.ReadInteger($4746, 0, -1);
      EXIF_XPTitle          := MainIFD.ReadWideString($9C9B);
      EXIF_XPComment        := MainIFD.ReadWideString($9C9C);
      EXIF_XPAuthor         := MainIFD.ReadWideString($9C9D);
      EXIF_XPKeywords       := MainIFD.ReadWideString($9C9E);
      EXIF_XPSubject        := MainIFD.ReadWideString($9C9F);
      EXIF_Artist           := MainIFD.ReadString(315);
      EXIF_WhitePoint[0]    := MainIFD.ReadRational(318, 0, -1);
      EXIF_WhitePoint[1]    := MainIFD.ReadRational(318, 1, -1);
      EXIF_YCbCrPositioning := MainIFD.ReadInteger(531, 0, -1);
      for q := 0 to 5 do
        EXIF_PrimaryChromaticities[q] := MainIFD.ReadRational(319, q, -1);
      for q := 0 to 2 do
        EXIF_YCbCrCoefficients[q]     := MainIFD.ReadRational(529, q, -1);
      for q := 0 to 5 do
        EXIF_ReferenceBlackWhite[q]   := MainIFD.ReadRational(532, q, -1);
      EXIF_Copyright := MainIFD.ReadString(IETIFFTAG_COPYRIGHT);

      // EXIF (in sub IFD)
      EXIF_ExposureTime             := ExifIFD.ReadRational($829A, 0, -1);
      EXIF_FNumber                  := ExifIFD.ReadRational($829D, 0, -1);
      EXIF_ExposureProgram          := ExifIFD.ReadInteger($8822, 0, -1);
      EXIF_ISOSpeedRatings[0]       := ExifIFD.ReadInteger($8827, 0, 0);
      EXIF_ISOSpeedRatings[1]       := ExifIFD.ReadInteger($8827, 1, 0);
      EXIF_ExifVersion              := ExifIFD.ReadString($9000);
      EXIF_DateTimeOriginal         := ExifIFD.ReadString($9003);
      EXIF_DateTimeDigitized        := ExifIFD.ReadString($9004);
      EXIF_CompressedBitsPerPixel   := ExifIFD.ReadRational($9102, 0, 0);
      EXIF_ShutterSpeedValue        := ExifIFD.ReadRational($9201, 0, -1);
      EXIF_ApertureValue            := ExifIFD.ReadRational($9202, 0, -1);
      EXIF_BrightnessValue          := ExifIFD.ReadRational($9203, 0, -1000);
      EXIF_ExposureBiasValue        := ExifIFD.ReadRational($9204, 0, -1000);
      EXIF_MaxApertureValue         := ExifIFD.ReadRational($9205, 0, -1000);
      EXIF_SubjectDistance          := ExifIFD.ReadRational($9206, 0, -1);
      EXIF_MeteringMode             := ExifIFD.ReadInteger($9207, 0, -1);
      EXIF_LightSource              := ExifIFD.ReadInteger($9208, 0, -1);
      EXIF_Flash                    := ExifIFD.ReadInteger($9209, 0, -1);
      EXIF_FocalLength              := ExifIFD.ReadRational($920A, 0, -1);
      EXIF_SubsecTime               := ExifIFD.ReadString($9290);
      EXIF_SubsecTimeOriginal       := ExifIFD.ReadString($9291);
      EXIF_SubsecTimeDigitized      := ExifIFD.ReadString($9292);
      EXIF_FlashPixVersion          := ExifIFD.ReadString($A000);
      EXIF_ColorSpace               := ExifIFD.ReadInteger($A001, 0, -1);
      EXIF_ExifImageWidth           := ExifIFD.ReadInteger($A002, 0, 0);
      EXIF_ExifImageHeight          := ExifIFD.ReadInteger($A003, 0, 0);
      EXIF_RelatedSoundFile         := ExifIFD.ReadString($A004);
      EXIF_FocalPlaneXResolution    := ExifIFD.ReadRational($A20E, 0, -1);
      EXIF_FocalPlaneYResolution    := ExifIFD.ReadRational($A20F, 0, -1);
      EXIF_FocalPlaneResolutionUnit := ExifIFD.ReadInteger($A210, 0, -1);
      EXIF_ExposureIndex            := ExifIFD.ReadRational($A215, 0, -1);
      EXIF_SensingMethod            := ExifIFD.ReadInteger($A217, 0, -1);
      EXIF_FileSource               := ExifIFD.ReadInteger($A300, 0, -1);
      EXIF_SceneType                := ExifIFD.ReadInteger($A301, 0, -1);
      EXIF_ExposureMode             := ExifIFD.ReadInteger($A402, 0, -1);
      EXIF_WhiteBalance             := ExifIFD.ReadInteger($A403, 0, -1);
      EXIF_DigitalZoomRatio         := ExifIFD.ReadRational($A404, 0, -1);
      EXIF_FocalLengthIn35mmFilm    := ExifIFD.ReadInteger($A405, 0, -1);
      EXIF_SceneCaptureType         := ExifIFD.ReadInteger($A406, 0, -1);
      EXIF_GainControl              := ExifIFD.ReadInteger($A407, 0, -1);
      EXIF_Contrast                 := ExifIFD.ReadInteger($A408, 0, -1);
      EXIF_Saturation               := ExifIFD.ReadInteger($A409, 0, -1);
      EXIF_Sharpness                := ExifIFD.ReadInteger($A40A, 0, -1);
      EXIF_SubjectDistanceRange     := ExifIFD.ReadInteger($A40C, 0, -1);
      EXIF_ImageUniqueID            := ExifIFD.ReadString($A420);

      ReadEXIFUserComment(ExifIFD, IOParams);   // tag $9286

      ReadEXIFMakerNote(ExifIFD, IETIFFTAG_EXIFMAKERNOTE, EXIF_MakerNote);

      // GPS-EXIF
      if GpsIFD.NumTags > 0 then
      begin
        EXIF_GPSVersionID            := convVersionIDtoStr(GpsIFD.ReadString($0));
        EXIF_GPSLatitudeRef          := GpsIFD.ReadString($1);
        EXIF_GPSLatitudeDegrees      := GpsIFD.ReadRational($2, 0, 0);
        EXIF_GPSLatitudeMinutes      := GpsIFD.ReadRational($2, 1, 0);
        EXIF_GPSLatitudeSeconds      := GpsIFD.ReadRational($2, 2, 0);
        EXIF_GPSLongitudeRef         := GpsIFD.ReadString($3);
        EXIF_GPSLongitudeDegrees     := GpsIFD.ReadRational($4, 0, 0);
        EXIF_GPSLongitudeMinutes     := GpsIFD.ReadRational($4, 1, 0);
        EXIF_GPSLongitudeSeconds     := GpsIFD.ReadRational($4, 2, 0);
        EXIF_GPSAltitudeRef          := IEIntToStr(GpsIFD.ReadInteger($5, 0, 0));
        EXIF_GPSAltitude             := GpsIFD.ReadRational($6, 0, 0);
        EXIF_GPSTimeStampHour        := GpsIFD.ReadRational($7, 0, 0);
        EXIF_GPSTimeStampMinute      := GpsIFD.ReadRational($7, 1, 0);
        EXIF_GPSTimeStampSecond      := GpsIFD.ReadRational($7, 2, 0);
        EXIF_GPSSatellites           := GpsIFD.ReadString($8);
        EXIF_GPSStatus               := GpsIFD.ReadString($9);
        EXIF_GPSMeasureMode          := GpsIFD.ReadString($A);
        EXIF_GPSDOP                  := GpsIFD.ReadRational($B, 0, 0);
        EXIF_GPSSpeedRef             := GpsIFD.ReadString($C);
        EXIF_GPSSpeed                := GpsIFD.ReadRational($D, 0, 0);
        EXIF_GPSTrackRef             := GpsIFD.ReadString($E);
        EXIF_GPSTrack                := GpsIFD.ReadRational($F, 0, 0);
        EXIF_GPSImgDirectionRef      := GpsIFD.ReadString($10);
        EXIF_GPSImgDirection         := GpsIFD.ReadRational($11, 0, 0);
        EXIF_GPSMapDatum             := GpsIFD.ReadString($12);
        EXIF_GPSDestLatitudeRef      := GpsIFD.ReadString($13);
        EXIF_GPSDestLatitudeDegrees  := GpsIFD.ReadRational($14, 0, 0);
        EXIF_GPSDestLatitudeMinutes  := GpsIFD.ReadRational($14, 1, 0);
        EXIF_GPSDestLatitudeSeconds  := GpsIFD.ReadRational($14, 2, 0);
        EXIF_GPSDestLongitudeRef     := GpsIFD.ReadString($15);
        EXIF_GPSDestLongitudeDegrees := GpsIFD.ReadRational($16, 0, 0);
        EXIF_GPSDestLongitudeMinutes := GpsIFD.ReadRational($16, 1, 0);
        EXIF_GPSDestLongitudeSeconds := GpsIFD.ReadRational($16, 2, 0);
        EXIF_GPSDestBearingRef       := GpsIFD.ReadString($17);
        EXIF_GPSDestBearing          := GpsIFD.ReadRational($18, 0, 0);
        EXIF_GPSDestDistanceRef      := GpsIFD.ReadString($19);
        EXIF_GPSDestDistance         := GpsIFD.ReadRational($1A, 0, 0);
        EXIF_GPSDateStamp            := GpsIFD.ReadString($1D);
        AdjustGPSCoordinates();
      end;

      // Interoperability IFD
      EXIF_InteropIndex            := InteropIFD.ReadString($0001);
      EXIF_InteropVersion          := InteropIFD.ReadString($0002);

      // Colormap
      FreeColorMap;
      if self.ColorMap_Num > 0 then
      begin
        fColorMapCount := self.ColorMap_Num;
        getmem(fColorMap, self.ColorMap_Num * sizeof(TRGB));
        CopyMemory(ColorMap, self.ColorMap, self.ColorMap_Num * sizeof(TRGB));
      end;
      // resolution unit and dpi
      if EXIF_ResolutionUnit = 3 then
        dd := CM_per_Inch
      else
        dd := 1;
      DpiX := round(EXIF_XResolution * dd);
      DpiY := round(EXIF_YResolution * dd);
      if DpiX = 0 then
        DpiX := IEGlobalSettings().DefaultDPIX;
      if DpiY = 0 then
        DpiY := IEGlobalSettings().DefaultDPIY;
      EXIF_XResolution := EXIF_XResolution * dd; // (re-set because DpiX=V assigns also EXIF_?Resolution)
      EXIF_YResolution := EXIF_YResolution * dd; // (re-set because DpiX=V assigns also EXIF_?Resolution)
      TIFF_XPos := trunc(MainIFD.ReadRational(286, 0, 0) * dd);
      TIFF_YPos := trunc(MainIFD.ReadRational(287, 0, 0) * dd);
      if ((Width = 0) or (Height = 0)) and (not IsExifThumb) then
      begin
        Progress.Aborting^ := True;
        exit;
      end;
    end;

    if Compression = 5 then
    begin
      // verify that exists a LZW function for decompression
      if assigned(IOParams.TIFF_LZWDecompFunc) then
        LZWDecompFunc := IOParams.TIFF_LZWDecompFunc
      else
      if assigned(IEGlobalSettings().DefTIFF_LZWDECOMPFUNC) then
        LZWDecompFunc := IEGlobalSettings().DefTIFF_LZWDECOMPFUNC
      else
      begin
        Progress.Aborting^ := True;
        exit;
      end;
    end;
    self.IgnoreAlpha := IgnoreAlpha;
    if not Preview then
    begin
      // Load the image
      if not self.IgnoreAlpha then
      begin
        if (((SamplesPerPixel = 4) and (ExtraSamples = 2)) or
          ((SamplesPerPixel > 3) and (ExtraSamplesCount>0)) or
          ((SamplesPerPixel = 2) and (BitsPerSample = 8)) or
          ((SamplesPerPixel > 1) and (ExtraSamplesCount>0) and (ExtraSamplesVal[0] = 1)) or
          ((SamplesPerPixel = 4) and (PhotometricInterpretation = 2)))
          and (not assigned(AlphaChannel)) then
          // alpha required
          AlphaChannel := TIEMask.Create;
        if assigned(AlphaChannel) then
        begin
          AlphaChannel.AllocateBits(ImageWidth, ImageHeight, 8);
          AlphaChannel.Fill(255);
        end
        else
          self.IgnoreAlpha := true;
      end;
      self.AlphaChannel := AlphaChannel;
      laccess := Bitmap.Access;
      Bitmap.Access := [iedWrite]; // write only

      if ((ImageWidth > 400) or (ImageHeight > 400)) and IsExifThumb then
      begin
        Progress.Aborting^ := True;
        exit;
      end;

      if ((Compression = 6) or ((JPEGInterchangeFormat <> 0) and (Compression <> 5) and (Compression <> 7))) and (JPEGProc < 2) then
      begin
        LoadSimpleJpegV6(MainIFD, self, Bitmap, Progress);
      end
      else
      begin
        if (ImageWidth = 0) or (ImageHeight = 0) then
        begin
          Progress.Aborting^ := True;
          exit;
        end;

        // adjust PhotometricInterpretation to 1 (black is zero) if SamplesPerPixel=1 and BitsPerSample=1 and PhotoMetricinterpretation = 2 (RGB)
        if (BitsPerSample = 1) and (SamplesPerPixel = 1) and (PhotometricInterpretation = 2) then
        begin
          PhotometricInterpretation := 1;
          IOParams.TIFF_PhotometInterpret := ioTIFF_BLACKISZERO;
        end;

        // allocate destination bitmap
        if (BitsPerSample = 1) and (SamplesPerPixel = 1) and (PhotometricInterpretation <> 3) then
          // black/white (1 bit gray scale)
          imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ie1g)
        else
        if IOParams.IsNativePixelFormat then
        begin
          // native pixel formats
          if (BitsPerSample = 8) and (SamplesPerPixel = 1) and (PhotometricInterpretation = 3) then
          begin
            // 8 bit palette
            imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ie8p);
            for q := 0 to IOParams.ColorMapCount - 1 do
              Bitmap.Palette[q] := IOParams.ColorMap[q];
          end
          else
          if (BitsPerSample = 8) and (SamplesPerPixel = 1) and (PhotometricInterpretation < 2) then
            // 8 bit gray scale
            imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ie8g)
          else
          if (BitsPerSample = 16) and (SamplesPerPixel = 1) and (PhotometricInterpretation < 2) then
            // 16 bit gray scale
            imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ie16g)
          else
          if (BitsPerSample = 12) and (SamplesPerPixel = 1) and (PhotometricInterpretation < 2) then
            // 12 bit gray scale (convert to 16 bit)
            imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ie16g)
          else
          if (BitsPerSample = 8) and (SamplesPerPixel >= 4) and (PhotometricInterpretation = 5) then
            // CMYK->x*8
            imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ieCMYK)
          else
          if (BitsPerSample = 16) and (SamplesPerPixel = 4) and (PhotometricInterpretation = 5) then
            // CMYK-4*16
            imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ieCMYK)
          else
          if (BitsPerSample = 16) and (SamplesPerPixel = 3) and (PhotometricInterpretation = 2) then
            // 48 bit RGB
            imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ie48RGB)
          else
            // otherwise 24 bit RGB
            imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ie24RGB);
        end
        else
          // 24 bit RGB
          imageAllocationOk := Bitmap.Allocate(ImageWidth, ImageHeight, ie24RGB);

        if not imageAllocationOk then
        begin
          Progress.Aborting^ := True;
          exit;
        end;

        if IsExifThumb then
          Bitmap.Fill(0);

        if (TileWidth > 0) or (TileLength > 0) then
        begin
          // tiled
          if TileOffsets_Num > 0 then
            Tiles2Bitmap(MainIFD, self, Bitmap, Progress)
          else
          if StripOffsets_Num > 0 then
          begin
            // old tiff, uses Strips instead of Tiles...
            TileOffsets_Num    := MainIFD.ReadArrayIntegers(TileOffsets, 273);
            TileByteCounts_Num := MainIFD.ReadArrayIntegers(TileByteCounts, 279);
            Tiles2Bitmap(MainIFD, self, Bitmap, Progress)
          end;
        end
        else
        begin
          // stripped
          if StripOffsets_Num > 0 then
            Strips2Bitmap(MainIFD, self, Bitmap, Progress)
          else
          begin
            Progress.Aborting^ := True;
            exit;
          end;
        end;
      end;
      Bitmap.Access := laccess;
      // adjust orientation if <>1
      if IOParams.TIFF_EnableAdjustOrientation then
      begin
        IEAdjustEXIFOrientation(Bitmap, Orientation);
        Orientation := 1;
      end;
    end;
  finally

    // ICC
    if assigned(IOParams) and assigned(IOParams.fInputICC) then
    begin
      if IOParams.InputICCProfile.IsTransforming then
        IOParams.InputICCProfile.FreeTransform()
      else
      if assigned(Bitmap) and not Preview then
        Bitmap.ColorProfile.Assign(IOParams.InputICCProfile);
    end;

  end;
end;



 //////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
//**********************************************************************************//
//* WRITE TIFF                                                                     *//
//**********************************************************************************//
////////////////////////////////////////////////////////////////////////////////////
 //////////////////////////////////////////////////////////////////////////////////

type


  TTIFFWriterContext = class
    LZWCompFunc: TTIFFLZWCompFunc; // LZW compression function
    Compression: integer;
    T4Options: integer;
    FillOrder: integer;
    Photomet: TIOTIFFPhotometInterpret;
    Predictor: integer;
    qt: TIEQuantizer;
    BitsPerSample: integer;
    jpegquality: integer; // jpeg quality
    jpegcolorspace: TIOJPEGColorSpace;
    RefParams: TIOParamsVals;
    hasalpha: boolean;

    constructor Create();
    destructor Destroy(); override;
  end;


constructor TTIFFWriterContext.Create();
begin
  inherited;
end;


destructor TTIFFWriterContext.Destroy();
begin
  if qt <> nil then
    FreeAndNil(qt);
  inherited;
end;


procedure WriteEXIFMakerNote(IFD: TIETIFFIFDWriter; Stream: TStream; tagid: integer; tagsHandler: TIETagsHandler; var Aborting: boolean);
var
  datapos: dword;
begin
  if (tagsHandler.Data.Size < 12) and assigned(tagsHandler.UnparsedData) and (tagsHandler.UnparsedDataLength > 0) then
  begin
    // IMPORTANT: We don't save unparsed EXIF_MakerNote data because it can contain pointers to previous
    //            file positions, making it anyway unreadable.
    (*
    // unparsed tags, write raw data
    IEStreamWordAlign(Stream, Aborting);
    IFD.AddTag(tagid, IETIFFTYPE_UNDEFINED, tagsHandler.UnparsedDataLength, Stream.Position);
    SafeStreamWrite(Stream, Aborting, pbyte(tagsHandler.UnparsedData)^, tagsHandler.UnparsedDataLength);
    *)

  end
  else
  if (tagsHandler.Data.Size > 12) then
  begin
    datapos := IEStreamWordAlign(stream, Aborting);
    IFD.AddTag(tagid, IETIFFTYPE_UNDEFINED, tagsHandler.WriteToStream(Stream, 0), datapos);
  end;

end;


// writes tag EXIF user comment
procedure WriteEXIFUserComment(IFD: TIETIFFIFDWriter; Stream: TStream; EXIF_UserCommentCode: AnsiString; EXIF_UserComment: WideString; var Aborting: boolean);
var
  strLength: integer;
  zerow: dword;
  ansiTemp: AnsiString;
  datanum, datapos: dword;
begin

  if EXIF_UserCommentCode = '' then
    EXIF_UserCommentCode := IEEXIFUSERCOMMENTCODE_UNICODE;

  strLength := length(EXIF_UserComment);
  if (strLength = 0) or (length(EXIF_UserCommentCode) <> 8) then
    exit;

  datapos := IEStreamWordAlign(Stream, Aborting);
  zerow := 0;

  if EXIF_UserCommentCode = IEEXIFUSERCOMMENTCODE_UNICODE then
  begin
    // IEEXIFUSERCOMMENTCODE_UNICODE
    datanum := 8 + 2 * strLength + 2;
    SafeStreamWrite(Stream, Aborting, EXIF_UserCommentCode[1], 8);
    SafeStreamWrite(Stream, Aborting, EXIF_UserComment[1], 2 * strLength);
    SafeStreamWrite(Stream, Aborting, zerow, 2);
  end
  else
  if EXIF_UserCommentCode = IEEXIFUSERCOMMENTCODE_ASCII then
  begin
    // IEEXIFUSERCOMMENTCODE_ASCII
    datanum := 8 + strLength + 1;
    SafeStreamWrite(Stream, Aborting, EXIF_UserCommentCode[1], 8);
    ansiTemp := AnsiString(EXIF_UserComment);
    SafeStreamWrite(Stream, Aborting, ansiTemp[1], strLength);
    SafeStreamWrite(Stream, Aborting, zerow, 1);
  end
  else
  begin
    // IEEXIFUSERCOMMENTCODE_JIS, IEEXIFUSERCOMMENTCODE_UNDEFINED
    datanum := strLength * 2 + length(EXIF_UserCommentCode);
    SafeStreamWrite(Stream, Aborting, EXIF_UserCommentCode[1], length(EXIF_UserCommentCode));
    SafeStreamWrite(Stream, Aborting, EXIF_UserComment[1], strLength * 2);
  end;

  IFD.AddTag($9286, IETIFFTYPE_UNDEFINED, datanum, datapos);
end;


procedure WriteIPTC(IFD: TIETIFFIFDWriter; Stream: TStream; Params: TIOParamsVals; var Aborting: boolean);
var
  buf: pointer;
  buflen: integer;
  datanum: dword;
begin
  Params.IPTC_Info.SaveToStandardBuffer(buf, buflen, false);
  try
    if buf <> nil then
    begin
      datanum := buflen div 4;
      if (buflen and 7) <> 0 then
        inc(datanum);
      IFD.AddTag(IETIFFTAG_IPTC, IETIFFTYPE_LONG, datanum, IEStreamWordAlign(Stream, Aborting));
      SafeStreamWrite(Stream, Aborting, pbyte(buf)^, buflen);
    end;
  finally
    freemem(buf);
  end;
end;


procedure WriteXMP(IFD: TIETIFFIFDWriter; Stream: TStream; Params: TIOParamsVals; var Aborting: boolean);
begin
  if Params.XMP_Info <> '' then
  begin
    IFD.AddTag(IETIFFTAG_XMP, IETIFFTYPE_BYTE, length(Params.XMP_Info), IEStreamWordAlign(Stream, Aborting));
    SafeStreamWrite(Stream, Aborting, Params.XMP_Info[1], length(Params.XMP_Info));
  end;
end;


procedure WriteICC(IFD: TIETIFFIFDWriter; Stream: TStream; Params: TIOParamsVals; var Aborting: boolean);
begin
  if assigned(Params.InputICCProfile) and Params.InputICCProfile.IsValid and not Params.InputICCProfile.IsApplied then
  begin
    IFD.AddTag(IETIFFTAG_ICC, IETIFFTYPE_UNDEFINED, Params.InputICCProfile.RawLength, IEStreamWordAlign(Stream, Aborting));
    Params.InputICCProfile.SaveToStream(Stream, true);
  end;
end;

{$ifdef IEINCLUDEIMAGINGANNOT}
procedure WriteWang(IFD: TIETIFFIFDWriter; Stream: TStream; Params: TIOParamsVals; var Aborting: boolean);
var
  buf: pointer;
  buflen: integer;
begin
  Params.ImagingAnnot.SaveToStandardBuffer(buf, buflen);
  try
    if buf <> nil then
    begin
      IFD.AddTag(IETIFFTAG_WANGIMAGING, IETIFFTYPE_BYTE, buflen, IEStreamWordAlign(Stream, Aborting));
      SafeStreamWrite(Stream, Aborting, pbyte(buf)^, buflen);
    end;
  finally
    freemem(buf);
  end;
end;
{$endif}


procedure WriteImageEnAnnot(IFD: TIETIFFIFDWriter; Stream: TStream; Params: TIOParamsVals; var Aborting: boolean);
var
  buf: pointer;
  buflen: integer;
begin
  Params.ImageEnAnnot.SaveToBuffer(buf, buflen);
  try
    if buf <> nil then
    begin
      IFD.AddTag(IEGlobalSettings().ObjectsTIFFTag, IETIFFTYPE_BYTE, buflen, IEStreamWordAlign(Stream, Aborting));
      SafeStreamWrite(Stream, Aborting, pbyte(buf)^, buflen);
    end;
  finally
    freemem(buf);
  end;
end;


// Writes rowbuf (of sz bytes) in Stream, PackBits compressed

procedure _WritePackBits(rowbuf: pbyte; sz: integer; Stream: TStream; var Aborting: boolean);
var
  pa: pbytearray;
  n, rl: integer;
  si: shortint;
  bp: integer;
  procedure SavB;
  var
    qq: integer;
  begin
    // writes absolute bytes from bp to n-1
    qq := n - bp;
    if qq > 0 then
    begin
      // more bytes
      si := qq - 1;
      SafeStreamWrite(Stream, Aborting, si, 1);
      SafeStreamWrite(Stream, Aborting, pbyte(@pa^[bp])^, qq);
    end;
  end;
begin
  pa := pbytearray(rowbuf);
  n := 0; // n is the initial position of the first group to compress
  bp := 0;
  while n < sz do
  begin
    // look for equal bytes
    rl := 1;
    while ((n + rl) < sz) and (pa^[n] = pa^[n + rl]) and (rl < 128) do
      inc(rl);
    if rl > 3 then
    begin
      SavB; // write absolute bytes from bp to n-1
      // replicates bytes
      si := -1 * (rl - 1);
      SafeStreamWrite(Stream, Aborting, si, 1);
      SafeStreamWrite(Stream, Aborting, pa^[n], 1);
      inc(n, rl);
      bp := n;
    end
    else
    if (n - bp) = 128 then
    begin
      SavB;
      bp := n;
    end
    else
      inc(n);
  end;
  SavB; // writes absolute bytes from bp to n-1
end;


// old jpeg compression
procedure WriteOldJpeg(Stream: TStream; WBitmap: TIEBitmap; IFD: TIETIFFIFDWriter; context: TTIFFWriterContext; var Progress: TProgressRec);
var
  iop: TIOParamsVals;
  ps1, ps2: integer;
  ms: TMemoryStream;
  pw: pword;
begin
  iop := TIOParamsVals.Create(nil);
  ms := TMemoryStream.Create();
  try
    iop.JPEG_Quality := context.jpegquality;
    iop.JPEG_ColorSpace := context.jpegcolorspace;
    WriteJPegStream(ms, WBitmap, iop, Progress);

    ps1 := Stream.Position;
    IECopyFrom(Stream, ms, 0);

    IFD.WriteSingleLong(514, ms.Size);  // JPEGInterchangeFormatLength
    IFD.WriteSingleLong(513, ps1);      // JPEGInterchangeFormat

    // search for SOS marker
    ps2 := 0;
    pw := ms.Memory;
    while true do
    begin
      if pw^ = $DAFF then
        break;
      inc(pbyte(pw));
      inc(ps2);
    end;

    IFD.WriteSingleLong(278, WBitmap.Height); // RowsPerStrip
    IFD.WriteSingleLong(279, ms.Size - ps2); // StripByteCounts
    IFD.WriteSingleLong(273, ps1 + ps2); // StripOffsets
    IFD.WriteSingleShort(512, 1); // JPEGProc
    IFD.WriteMultiShort(Stream, 530, [2, 2], Progress.Aborting^);

  finally
    ms.Free();
    iop.Free();
  end;
end;


// jpeg compression (DRAFT TIFF Technical Note #2)
procedure WriteStripJpeg(Stream: TStream; Bitmap: TIEBitmap; context: TTIFFWriterContext; var Progress: TProgressRec);
var
  iop: TIOParamsVals;
begin
  iop := TIOParamsVals.Create(nil);
  try
    iop.JPEG_Quality := context.jpegquality;
    iop.JPEG_ColorSpace := context.jpegcolorspace;
    WriteJPegStream(Stream, Bitmap, iop, Progress);
  finally
    iop.Free();
  end;
end;


procedure WriteStrip(Stream: TStream; Bitmap: TIEBitmap; IFD: TIETIFFIFDWriter; context: TTIFFWriterContext; var Progress: TProgressRec);
const
  MAXSTRIPDIM = 512 * 1024; // 512 K
var
  row, q, ww, dbit, sbit: integer;
  rowbuf, tmpbuf: array of byte;
  bufb, pb: pbyte;
  bufw: pword;
  inrow, buf1, buf2: PRGB;
  inrow_48, buf1_48, buf2_48: PRGB48;
  buf1_cmyk, buf2_cmyk: PCMYK;
  inrow_alpha, buf2_alpha: pbyte;
  bwr, bb: byte; // byte to write
  bwrl: integer; // remaining bits in bwr to write
  p_byte, predline: pbyte; // predline allocated by compressing function
  Samples: integer; // Samples per pixel
  lzwid: pointer;
  bitmapwidth1, bitmapheight1: integer;
  StripsPerImage: integer;
  RowsPerStrip: integer;
  striprow: integer; // current row of strip
  stripidx: integer; // current strip
  pos_ar: array of dword;
  siz_ar: array of dword;
  p_word: pword;
  zstream: TZCompressionStream;
  rl: integer;
  bitmapWidth: integer;

  procedure FinalizeCompressors;
  begin
    if (context.Compression = 3) and (context.T4Options = 0) then
      // finalize G3FAX1D
      CCITTHuffmanPutLineG3(nil, 0, Stream, bwr, bwrl, Progress.Aborting^, context.FillOrder);
    if (context.Compression = 3) and (context.T4Options = 1) then
      // finalize G3FAX2D
      CCITTHuffmanPutLineG32D(nil, 0, Stream, bwr, bwrl, predline, Progress.Aborting^, context.FillOrder);
    if (context.Compression = 4) then
      // finalize G4FAX
      CCITTHuffmanPutLineG4(nil, 0, Stream, bwr, bwrl, predline, Progress.Aborting^, context.FillOrder);
    if context.Compression = 5 then
      // finalize LZW
      context.LZWCompFunc(nil, 0, Stream, lzwid);
    if (context.Compression = 32946) or (context.Compression = 8) then
      // finalize zip
      zstream.free;
  end;

begin
  if (context.Compression = 7) then
  begin
    StripsPerImage := 1;
    RowsPerStrip := Bitmap.Height;
  end
  else
  if (context.Compression = 32946) or (context.Compression = 8) then
  begin
    StripsPerImage := 1;
    RowsPerStrip := Bitmap.Height;
    case context.RefParams.TIFF_ZIPCompression of
      0: zstream := TZCompressionStream.Create(Stream, zcFastest, 15);
      1: zstream := TZCompressionStream.Create(Stream, zcDefault, 15);
      2: zstream := TZCompressionStream.Create(Stream, zcMax, 15);
    end;
  end
  else
  begin
    if context.RefParams.TIFF_StripCount = 0 then
      StripsPerImage := imax((Bitmap.Height * Bitmap.RowLen) div MAXSTRIPDIM, 1)
    else
      StripsPerImage := context.RefParams.TIFF_StripCount;
    RowsPerStrip := Bitmap.Height div StripsPerImage;
    if frac(Bitmap.Height / StripsPerImage) > 0 then
      inc(StripsPerImage);
  end;
  SetLength(pos_ar, StripsPerImage * 2);
  SetLength(siz_ar, StripsPerImage * 2);
  StripsPerImage := 0; // above values was only an estimation. Now calculates the actual value.
  case context.Photomet of
    ioTIFF_WHITEISZERO,
    ioTIFF_BLACKISZERO,
    ioTIFF_RGBPALETTE,
    ioTIFF_TRANSPMASK: Samples := 1;
    ioTIFF_CMYK:       Samples := 4;
    else
      Samples := 3; // RGB, CIELab, etc...
  end;
  if bitmap.HasAlphaChannel and not bitmap.AlphaChannel.Full and ((samples = 3) or (samples = 4)) and ((context.Compression = 1) or (context.Compression = 32773) or (context.Compression = 5)) then
  begin
    context.hasalpha := true;
    inc(Samples);
  end;
  bitmapwidth1  := bitmap.width - 1;
  bitmapheight1 := bitmap.height - 1;
  lzwid := nil;
  if context.Compression = 7 then
  begin
    pos_ar[0] := Stream.Position;
    WriteStripJpeg(Stream, Bitmap, context, Progress);
    siz_ar[0] := Stream.Position - int64(pos_ar[0]);
    StripsPerImage := 1;
  end
  else
  begin
    case Bitmap.PixelFormat of

      ie8p, ie8g, ie16g:
        begin

          if context.Compression = 7 then
            exit;
          ww := (Samples * context.BitsPerSample * Bitmap.Width);
          if (ww and 7) <> 0 then
            ww := (ww div 8) + 1
          else
            ww := ww div 8;
          Progress.per1 := 100 / Bitmap.Height;
          SetLength(rowbuf, (Bitmap.Width * Samples) * imax(1, context.BitsPerSample div 8) + 4);
          striprow := 0;
          stripidx := 0;
          pos_ar[0] := Stream.Position;
          for row := 0 to BitmapHeight1 do
          begin
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * row));

            // PALETTE or GRAYSCALE
            if context.BitsPerSample = 8 then
            begin
              // 8 bits per pixel
              p_byte := Bitmap.Scanline[row];
              bufb := @rowbuf[0];
              for q := 0 to BitmapWidth1 do
              begin
                bufb^ := p_byte^;
                inc(p_byte);
                inc(bufb);
              end;
            end
            else
            if context.BitsPerSample = 16 then
            begin
              // 16 bits per pixel (grayscale)
              p_word := Bitmap.Scanline[row];
              bufw := pword(@rowbuf[0]);
              for q := 0 to BitmapWidth1 do
              begin
                bufw^ := p_word^;
                inc(p_word);
                inc(bufw);
              end;
            end
            else
            begin
              // 7, 6, 5, 4, 3, 2 bits per pixel
              // compact pixels in bytes
              p_byte := Bitmap.Scanline[row];
              dbit := 0; // dest bit (0..7)
              bufb := @rowbuf[0]; // dest buffer
              for q := 0 to BitmapWidth1 do
              begin
                bb := p_byte^;
                for sbit := 0 to context.BitsPerSample - 1 do
                begin
                  if (bb and (1 shl (context.BitsPerSample - 1 - sbit))) <> 0 then
                    // write 1
                    bufb^ := bufb^ or iebitmask1[dbit]
                  else
                    // write 0
                    bufb^ := bufb^ and not iebitmask1[dbit];
                  inc(dbit);
                  if dbit = 8 then
                  begin
                    dbit := 0;
                    inc(bufb);
                  end;
                end;
                inc(p_byte);
              end;
            end;
            // from here in rowbuf there is the row do compress and write
            case context.Compression of
              1:
                // NO COMPRESSION
                SafeStreamWrite(Stream, Progress.Aborting^, rowbuf[0], ww);
              5:
                // LZW
                context.LZWCompFunc(@rowbuf[0], ww, Stream, lzwid);
              32773:
                // PACKBITS
                _WritePackBits(@rowbuf[0], ww, Stream, Progress.Aborting^);
              32946:
                // zip
                SafeStreamWrite(zstream, Progress.Aborting^, rowbuf[0], ww);
            end;
            if Progress.Aborting^ then
              break;

            inc(striprow);
            if (striprow = RowsPerStrip) or (row = BitmapHeight1) then
            begin
              FinalizeCompressors;
              lzwid := nil;
              //
              siz_ar[stripidx] := Stream.Position - int64(pos_ar[stripidx]);
              StripsPerImage := stripidx + 1;
              striprow := 0;
              if row < BitmapHeight1 then
              begin
                // this isn't the last one
                inc(stripidx);
                pos_ar[stripidx] := Stream.Position;
              end;
            end;

          end; // of row for
        end;

      ie24RGB:
        ///////////// COLOR IMAGES
        begin
          if context.Compression = 7 then
            exit;
          // RGB/CMYK/CIELAB/PALETTE
          if (Samples < 3) and (context.Photomet <> ioTIFF_RGBPALETTE) and (context.Photomet <> ioTIFF_BLACKISZERO) then
            Samples := 3;
          ww := (Samples * context.BitsPerSample * Bitmap.Width);
          if (ww and 7) <> 0 then
            ww := (ww div 8) + 1
          else
            ww := ww div 8;
          Progress.per1 := 100 / Bitmap.Height;
          SetLength(rowbuf, (Bitmap.Width * Samples) * imax(1, context.BitsPerSample div 8) + 4);
          striprow := 0;
          stripidx := 0;
          pos_ar[0] := Stream.Position;
          for row := 0 to BitmapHeight1 do
          begin
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * row));
            // prepare buffer to write
            if context.Photomet = ioTIFF_CMYK then
            begin
              // CMYK
              inrow := Bitmap.Scanline[row];
              // from BGR to CMYK
              IEGlobalSettings().ConvertColorFunction(inrow, iecmsBGR, @rowbuf[0], iecmsCMYK, Bitmap.Width, context.RefParams);
              pb := @rowbuf[0];
              rl := Bitmap.Width * 4;
              for q := 0 to rl - 1 do
              begin
                pb^ := 255 - pb^;
                inc(pb);
              end;
              // insert alpha channel
              if context.hasalpha then
              begin
                SetLength(tmpbuf, length(rowbuf));
                inrow_alpha := Bitmap.AlphaChannel.ScanLine[row];
                bitmapWidth := Bitmap.Width;
                for q := 0 to bitmapWidth - 1 do
                begin
                  tmpbuf[q * 5 + 0] := rowbuf[q * 4 + 0];
                  tmpbuf[q * 5 + 1] := rowbuf[q * 4 + 1];
                  tmpbuf[q * 5 + 2] := rowbuf[q * 4 + 2];
                  tmpbuf[q * 5 + 3] := rowbuf[q * 4 + 3];
                  tmpbuf[q * 5 + 4] := inrow_alpha^;
                  inc(inrow_alpha);
                end;
                rowbuf := tmpbuf;
              end;
            end
            else
            if context.Photomet = ioTIFF_CIELAB then
            begin
              // CIELAB
              inrow := Bitmap.Scanline[row];
              IEGlobalSettings().ConvertColorFunction(inrow, iecmsBGR, @rowbuf[0], iecmsCIELab, Bitmap.Width, context.RefParams);
            end
            else
            if (context.Photomet = ioTIFF_RGBPALETTE) or (context.Photomet = ioTIFF_BLACKISZERO) then
            begin
              // PALETTE or GRAYSCALE
              inrow := PRGB(Bitmap.Scanline[row]);
              if context.BitsPerSample = 8 then
              begin
                // 8 bits per pixel
                bufb := @rowbuf[0];
                for q := 0 to BitmapWidth1 do
                begin
                  bufb^ := context.qt.RGBIndex[inrow^];
                  inc(inrow);
                  inc(bufb);
                end;
              end
              else
              if context.BitsPerSample = 16 then
              begin
                // 16 bits per pixel (grayscale)
                bufw := pword(@rowbuf[0]);
                for q := 0 to BitmapWidth1 do
                begin
                  bufw^ := context.qt.RGBIndex[inrow^] *257;
                  inc(inrow);
                  inc(bufw);
                end;
              end
              else
              begin
                // 7, 6, 5, 4, 3, 2 bits per pixel
                // compact pixels in bytes
                dbit := 0; // dest bit (0..7)
                bufb := @rowbuf[0]; // dest buffer
                for q := 0 to BitmapWidth1 do
                begin
                  bb := context.qt.RGBIndex[inrow^];
                  for sbit := 0 to context.BitsPerSample - 1 do
                  begin
                    if (bb and (1 shl (context.BitsPerSample - 1 - sbit))) <> 0 then
                      // write 1
                      bufb^ := bufb^ or iebitmask1[dbit]
                    else
                      // write 0
                      bufb^ := bufb^ and not iebitmask1[dbit];
                    inc(dbit);
                    if dbit = 8 then
                    begin
                      dbit := 0;
                      inc(bufb);
                    end;
                  end;
                  inc(inrow);
                end;
              end;
            end
            else
            begin
              // RGB
              if context.Predictor = 2 then
              begin
                // Predictor, BGR to RGB
                buf1 := PRGB(uint64(@rowbuf[0]) + BitmapWidth1 * 3);
                inrow := PRGB(uint64(Bitmap.Scanline[row]) + (BitmapWidth1) * 3);
                buf2 := inrow;
                dec(buf2);
                for q := BitmapWidth1 downto 1 do
                begin
                  buf1^.r := inrow^.b - buf2^.b;
                  buf1^.g := inrow^.g - buf2^.g;
                  buf1^.b := inrow^.r - buf2^.r;
                  dec(buf1);
                  dec(inrow);
                  dec(buf2);
                end;
                buf1^.r := inrow^.b;
                buf1^.g := inrow^.g;
                buf1^.b := inrow^.r;
              end
              else
              if context.hasalpha then
              begin
                // RGB with alpha channel and no predictor
                inrow := PRGB(Bitmap.Scanline[row]);
                bufb := pbyte(@rowbuf[0]);
                pb := Bitmap.AlphaChannel.Scanline[row];
                for q := 0 to BitmapWidth1 do
                begin
                  (*
                  bufb^ := (inrow^.r * pb^) div 255; inc(bufb);
                  bufb^ := (inrow^.g * pb^) div 255; inc(bufb);
                  bufb^ := (inrow^.b * pb^) div 255; inc(bufb);
                  *)
                  bufb^ := inrow^.r; inc(bufb);
                  bufb^ := inrow^.g; inc(bufb);
                  bufb^ := inrow^.b; inc(bufb);
                  bufb^ := pb^; inc(bufb);
                  inc(inrow);
                  inc(pb);
                end;
              end
              else
              begin
                // No predictor, from BGR to RGB
                inrow := PRGB(Bitmap.Scanline[row]);
                buf1 := PRGB(@rowbuf[0]);
                CopyMemory(@rowbuf[0], inrow, Bitmap.Width * Samples);
                for q := 0 to BitmapWidth1 do
                begin
                  bswap(buf1.r, buf1.b);
                  inc(buf1);
                end;
              end;
            end;
            // from here in rowbuf there is the row do compress and write
            case context.Compression of
              1:
                // NO COMPRESSION
                SafeStreamWrite(Stream, Progress.Aborting^, rowbuf[0], ww);
              5:
                // LZW
                context.LZWCompFunc(@rowbuf[0], ww, Stream, lzwid);
              32773:
                // PACKBITS
                _WritePackBits(@rowbuf[0], ww, Stream, Progress.Aborting^);
              32946:
                // zip
                SafeStreamWrite(zstream, Progress.Aborting^, rowbuf[0], ww);
            end;
            if Progress.Aborting^ then
              break;

            inc(striprow);
            if (striprow = RowsPerStrip) or (row = BitmapHeight1) then
            begin
              FinalizeCompressors;
              lzwid := nil;
              //
              siz_ar[stripidx] := Stream.Position - int64(pos_ar[stripidx]);
              StripsPerImage := stripidx + 1;
              striprow := 0;
              if row < BitmapHeight1 then
              begin
                // this isn't the last one
                inc(stripidx);
                pos_ar[stripidx] := Stream.Position;
              end;
            end;

          end; // of row for
        end;


      ie48RGB:
        ///////////// 48 bit color images
        begin
          if context.Compression = 7 then
            exit;
          // RGB 48
          Samples := 3;
          ww := (Samples * context.BitsPerSample * Bitmap.Width);
          if (ww and 7) <> 0 then
            ww := (ww div 8) + 1
          else
            ww := ww div 8;
          Progress.per1 := 100 / Bitmap.Height;
          SetLength(rowbuf, (Bitmap.Width * Samples) * imax(1, context.BitsPerSample div 8) + 4);
          striprow := 0;
          stripidx := 0;
          pos_ar[0] := Stream.Position;
          for row := 0 to BitmapHeight1 do
          begin
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * row));
            // prepare buffer to write
            if context.Predictor = 2 then
            begin
              // Predictor, BGR to RGB
              buf1_48 := PRGB48(uint64(@rowbuf[0]) + BitmapWidth1 * 6);
              inrow_48 := PRGB48(uint64(Bitmap.Scanline[row]) + (BitmapWidth1) * 6);
              buf2_48 := inrow_48;
              dec(buf2_48);
              for q := BitmapWidth1 downto 1 do
              begin
                buf1_48^.b := inrow_48^.b - buf2_48^.b;
                buf1_48^.g := inrow_48^.g - buf2_48^.g;
                buf1_48^.r := inrow_48^.r - buf2_48^.r;
                dec(buf1_48);
                dec(inrow_48);
                dec(buf2_48);
              end;
              buf1_48^.b := inrow_48^.b;
              buf1_48^.g := inrow_48^.g;
              buf1_48^.r := inrow_48^.r;
            end
            else
            begin
              // No predictor, from BGR to RGB
              CopyMemory(@rowbuf[0], Bitmap.Scanline[row], Bitmap.Width * Samples *2);
            end;
            // from here in rowbuf there is the row do compress and write
            case context.Compression of
              1:
                // NO COMPRESSION
                SafeStreamWrite(Stream, Progress.Aborting^, rowbuf[0], ww);
              5:
                // LZW
                context.LZWCompFunc(@rowbuf[0], ww, Stream, lzwid);
              32773:
                // PACKBITS
                _WritePackBits(@rowbuf[0], ww, Stream, Progress.Aborting^);
              32946:
                // zip
                SafeStreamWrite(zstream, Progress.Aborting^, rowbuf[0], ww);
            end;
            if Progress.Aborting^ then
              break;

            inc(striprow);
            if (striprow = RowsPerStrip) or (row = BitmapHeight1) then
            begin
              FinalizeCompressors;
              lzwid := nil;
              //
              siz_ar[stripidx] := Stream.Position - int64(pos_ar[stripidx]);
              StripsPerImage := stripidx + 1;
              striprow := 0;
              if row < BitmapHeight1 then
              begin
                // this isn't the last one
                inc(stripidx);
                pos_ar[stripidx] := Stream.Position;
              end;
            end;

          end; // of row for
        end;

      ieCMYK:
        ///////////// CMYK
        begin
          if context.Compression = 7 then
            exit;
          // CMYK
          Samples := 4;
          if context.hasalpha then
            inc(Samples);
          ww := (Samples * context.BitsPerSample * Bitmap.Width);
          if (ww and 7) <> 0 then
            ww := (ww div 8) + 1
          else
            ww := ww div 8;
          Progress.per1 := 100 / Bitmap.Height;
          SetLength(rowbuf, (Bitmap.Width * Samples) * imax(1, context.BitsPerSample div 8) + 4);
          striprow := 0;
          stripidx := 0;
          pos_ar[0] := Stream.Position;
          for row := 0 to BitmapHeight1 do
          begin
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * row));
            // prepare buffer to write (Predictor must be 1)
            if context.hasalpha then
            begin
              // with alpha channel
              buf1_cmyk  := PCMYK(@rowbuf[0]);
              buf2_cmyk  := Bitmap.Scanline[row];
              buf2_alpha := Bitmap.AlphaChannel.Scanline[row];
              for q := 0 to BitmapWidth1 do
              begin
                buf1_cmyk^.c := 255 - buf2_cmyk^.c;
                buf1_cmyk^.m := 255 - buf2_cmyk^.m;
                buf1_cmyk^.y := 255 - buf2_cmyk^.y;
                buf1_cmyk^.k := 255 - buf2_cmyk^.k;
                inc(buf1_cmyk);
                inc(buf2_cmyk);
                pbyte(buf1_cmyk)^ := buf2_alpha^;
                inc(pbyte(buf1_cmyk));
                inc(buf2_alpha);
              end;
            end
            else
            begin
              // no alpha channel
              buf1_cmyk := PCMYK(@rowbuf[0]);
              buf2_cmyk := Bitmap.Scanline[row];
              for q := 0 to BitmapWidth1 do
              begin
                buf1_cmyk^.c := 255 - buf2_cmyk^.c;
                buf1_cmyk^.m := 255 - buf2_cmyk^.m;
                buf1_cmyk^.y := 255 - buf2_cmyk^.y;
                buf1_cmyk^.k := 255 - buf2_cmyk^.k;
                inc(buf1_cmyk);
                inc(buf2_cmyk);
              end;
            end;
            // from here in rowbuf there is the row do compress and write
            case context.Compression of
              1:
                // NO COMPRESSION
                SafeStreamWrite(Stream, Progress.Aborting^, rowbuf[0], ww);
              5:
                // LZW
                context.LZWCompFunc(@rowbuf[0], ww, Stream, lzwid);
              32773:
                // PACKBITS
                _WritePackBits(@rowbuf[0], ww, Stream, Progress.Aborting^);
              32946:
                // zip
                SafeStreamWrite(zstream, Progress.Aborting^, rowbuf[0], ww);
            end;
            if Progress.Aborting^ then
              break;

            inc(striprow);
            if (striprow = RowsPerStrip) or (row = BitmapHeight1) then
            begin
              FinalizeCompressors;
              lzwid := nil;
              //
              siz_ar[stripidx] := Stream.Position - int64(pos_ar[stripidx]);
              StripsPerImage := stripidx + 1;
              striprow := 0;
              if row < BitmapHeight1 then
              begin
                // this isn't the last one
                inc(stripidx);
                pos_ar[stripidx] := Stream.Position;
              end;
            end;

          end; // of row for
        end;


      ie1g:
        ///////////// B/W IMAGES
        begin
          Progress.per1 := 100 / Bitmap.Height;
          // calculates row length in bytes
          ww := Bitmap.Width div 8;
          if (Bitmap.Width mod 8) <> 0 then
            inc(ww);
          //
          striprow := 0;
          stripidx := 0;
          pos_ar[0] := Stream.Position;
          bwrl := 0;
          bwr := 0;
          predline := nil;
          for row := 0 to BitmapHeight1 do
          begin
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * row));
            //
            case context.Compression of
              1:
                // NO COMPRESSION
                SafeStreamWrite(Stream, Progress.Aborting^, pbyte(Bitmap.Scanline[row])^, ww);
              2:
                // CCITT HUFFMAN 1D
                CCITTHuffmanPutLine(pbyte(Bitmap.Scanline[row]), Bitmap.Width, Stream, Progress.Aborting^, context.FillOrder);
              3:
                if context.T4Options = 0 then
                  // CCITT G3FAX1D
                  CCITTHuffmanPutLineG3(pbyte(Bitmap.Scanline[row]), Bitmap.Width, Stream, bwr, bwrl, Progress.Aborting^, context.FillOrder)
                else
                  // CCITT G3FAX2D
                  CCITTHuffmanPutLineG32D(pbyte(Bitmap.Scanline[row]), Bitmap.Width, Stream, bwr, bwrl, predline, Progress.Aborting^, context.FillOrder);
              4:
                // CCITT G4FAX
                CCITTHuffmanPutLineG4(pbyte(Bitmap.Scanline[row]), Bitmap.Width, Stream, bwr, bwrl, predline, Progress.Aborting^, context.FillOrder);
              5:
                // LZW
                context.LZWCompFunc(pbyte(Bitmap.Scanline[row]), ww, Stream, lzwid);
              32773:
                // PACKBITS
                _WritePackBits(pbyte(Bitmap.Scanline[row]), ww, Stream, Progress.Aborting^);
              32946:
                // zip
                SafeStreamWrite(zstream, Progress.Aborting^, pbyte(Bitmap.Scanline[row])^, ww);
            end;
            if Progress.Aborting^ then
              break;

            inc(striprow);
            if (striprow = RowsPerStrip) or (row = BitmapHeight1) then
            begin
              FinalizeCompressors;
              bwrl := 0;
              bwr := 0;
              predline := nil;
              lzwid := nil;
              //
              siz_ar[stripidx] := Stream.Position - int64(pos_ar[stripidx]);
              StripsPerImage := stripidx + 1;
              striprow := 0;
              if row < BitmapHeight1 then
              begin
                // this isn't the last one
                inc(stripidx);
                pos_ar[stripidx] := Stream.Position;
              end;
            end;

          end;
        end;

    end; // end PixelFormat case
  end; // end if (jpeg compression)
  IFD.WriteSingleLong(278, RowsPerStrip); // RowsPerStrip
  if StripsPerImage = 1 then
  begin
    IFD.WriteSingleLong(273, pos_ar[0]); // StripOffsets (array)
    IFD.WriteSingleLong(279, siz_ar[0]); // StripByteCounts (array)
  end
  else
  begin
    IFD.WriteMultiLongEx(Stream, 273, pos_ar, StripsPerImage, Progress.Aborting^);
    IFD.WriteMultiLongEx(Stream, 279, siz_ar, StripsPerImage, Progress.Aborting^);
  end;
end;


procedure RelocateIFD(Stream: TStream; IFDPosition: int64; offset: int64; TagsStream: TStream; littleEndian: boolean = true);
var
  oldpos: int64;
  w: word;
  IFD: array of TTIFFTAG;
  i: integer;
  makerNotesRelocator: TIETagsHandlerRelocator;
begin
  oldpos := Stream.Position;

  Stream.Position := IFDPosition;
  Stream.Read(w, 2);
  SetLength(IFD, w);
  Stream.Read(IFD[0], length(IFD) * sizeof(TTIFFTAG));

  for i := 0 to high(IFD) do
  begin
    if (IFD[i].IdTag = IETIFFTAG_EXIFMAKERNOTE) then
    begin
      makerNotesRelocator := TIETagsHandlerRelocator.Create(TagsStream, IFD[i].DataPos, offset, littleEndian);
      try
        makerNotesRelocator.Relocate();
      finally
        makerNotesRelocator.Free();
      end;
      inc(IFD[i].DataPos, offset);
    end
    else
    if IETIFFCalcTagSize(IFD[i].DataType) * IFD[i].DataNum > 4 then
    begin
      inc(IFD[i].DataPos, offset);
    end
    else
    if (IFD[i].IdTag = IETIFFTAG_EXIFIFD) or
       (IFD[i].IdTag = IETIFFTAG_EXIFGPSIFD) or
       (IFD[i].IdTag = IETIFFTAG_INTEROPIFD) then
    begin
      RelocateIFD(TagsStream, IFD[i].DataPos, offset, TagsStream, littleEndian);
      inc(IFD[i].DataPos, offset);
    end;
  end;

  Stream.Position := IFDPosition + 2;
  Stream.Write(IFD[0], length(IFD) * sizeof(TTIFFTAG));

  Stream.Position := oldpos;
end;


procedure WriteExifBlock(parentIFD: TIETIFFIFDWriter; OStream: TStream; var IOParams: TIOParamsVals; var Aborting: boolean); forward;


// Ins: true insert an image of index IOParams.TIFF_ImageIndex, false saves as unique image
// If Ins is True, Stream must be open as fmOpenReadWrite
// returns the number of images inside the file (always 1 if Ins=false)
// note: Bitmap can be nil. If it is nil write only parameters (for EXIF inside Jpeg)
function TIFFWriteStream(OStream: TStream; Ins: boolean; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec): integer;
var
  tifhead: TTIFFHEADER;
  mainIFD: TIETIFFIFDWriter; // the main IFD
  i: integer;
  BasePos: int64;
  PosIFD: int64;
  WBitmap: TIEBitmap;
  BackCol, ForeCol: TRGB;
  context: TTIFFWriterContext;
  inv1bit: boolean;
  numi: integer;
  nt: word;
  WPosIFD: int64; // where to write position of new IFD
  SPosIFD: int64; // position of IFD to connect to the new IFD
  nullpr: TProgressRec;
  GlobalColorMap: array[0..255] of TRGB;
  wcolormap: array[0..255 * 3] of word;
  ncol: integer;
  laccess: TIEDataAccess;
  ms: TMemoryStream;
  XStream: TStream;
  ofs: int64;
  dw: dword;
  ww: word;
  apos: int64;
  samples: integer;
begin
  mainIFD := nil;
  wbitmap := nil;
  ncol := 0;
  ms := nil;
  context := TTIFFWriterContext.Create();
  try
    context.RefParams := IOParams;
    mainIFD := TIETIFFIFDWriter.Create();

    with nullpr do
    begin
      Aborting := Progress.Aborting;
      fOnProgress := nil;
      Sender := nil;
    end;
    wbitmap := bitmap;
    if wbitmap <> nil then
    begin
      // adjust wrong combinations
      if (wbitmap.PixelFormat = ie1g) and (IOParams.TIFF_PhotoMetInterpret = ioTIFF_RGB) and (IOParams.SamplesPerPixel = 1) and (IOParams.BitsPerSample = 1) then
        IOParams.TIFF_PhotometInterpret := ioTIFF_BLACKISZERO;

      if (wbitmap.PixelFormat = ie24RGB) and (IOParams.SamplesPerPixel = 1) and (IOParams.BitsPerSample = 1) then
        IOParams.TIFF_PhotometInterpret := ioTIFF_BLACKISZERO;

      if (wbitmap.PixelFormat = ie24RGB) and (IOParams.SamplesPerPixel <= 2) and (IOParams.BitsPerSample = 8) and not wbitmap.HasAlphaChannel then
      begin
        IOParams.TIFF_PhotometInterpret := ioTIFF_RGBPALETTE;
        IOParams.SamplesPerPixel := 1;
      end;

      if (wbitmap.PixelFormat = ie24RGB) and (IOParams.SamplesPerPixel <= 2) and (IOParams.BitsPerSample = 8) and wbitmap.HasAlphaChannel then
      begin
        // alpha supported only on 24 bit RGB - uncompressed
        IOParams.TIFF_PhotometInterpret := ioTIFF_RGB;
        IOParams.SamplesPerPixel        := 4;
        IOParams.BitsPerSample          := 8;
        IOParams.TIFF_Compression       := ioTIFF_UNCOMPRESSED;
      end;

      // convert bitmap when necessary
      if (IOParams.BitsPerSample = 1) and
         (IOParams.SamplesPerPixel = 1) and
         ((IOParams.TIFF_PhotometInterpret = ioTIFF_WHITEISZERO) or (IOParams.TIFF_PhotometInterpret = ioTIFF_BLACKISZERO)) then
      begin
        // save to black/white
        if Bitmap.PixelFormat <> ie1g then
        begin
          // Converts to 1 bit
          WBitmap := _ConvertTo1bitEx(Bitmap, BackCol, ForeCol);
          if WBitmap = nil then
          begin
            // impossible to convert to 1 bit, converts from color to black/white
            // 3.0.0
            WBitmap := TIEBitmap.Create(Bitmap.Width, Bitmap.Height, ie1g);
            WBitmap.CopyAndConvertFormat(Bitmap);
          end;
        end;
      end
      else
      begin
        // save in true color
        if Bitmap.PixelFormat = ie1g then
        begin
          // Converts to 24 bit
          WBitmap := TIEBitmap.Create();
          WBitmap.Assign(Bitmap);
          WBitmap.PixelFormat := ie24RGB;
        end;
      end;
      if (IOParams.SamplesPerPixel = 1) and
         ((IOParams.BitsPerSample <= 8) or (IOParams.BitsPerSample = 16)) and
         ((IOParams.BitsPerSample > 1) or (IOParams.TIFF_PhotometInterpret = ioTIFF_RGBPALETTE)) then
      begin
        // paletted image
        ncol := imin(1 shl IOParams.BitsPerSample, 256);
        context.qt := TIEQuantizer.Create(wBitmap, GlobalColorMap, ncol);
        if context.qt.grayScale then
          IOParams.TIFF_PhotometInterpret := ioTIFF_BLACKISZERO
        else
        begin
          if (IOParams.TIFF_Compression <> ioTIFF_LZW) and (IOParams.TIFF_Compression <> ioTIFF_PACKBITS) and (IOParams.TIFF_Compression <> ioTIFF_ZIP) then
            IOParams.TIFF_Compression := ioTIFF_UNCOMPRESSED;
          IOParams.TIFF_PhotometInterpret := ioTIFF_RGBPALETTE;
          if IOParams.BitsPerSample = 16 then
            IOParams.BitsPerSample := 8; // color image cannot be 16 bit gray scale
        end;
      end;
      if (IOParams.TIFF_PhotometInterpret = ioTIFF_RGBPALETTE) and (IOParams.SamplesPerPixel > 1) then
        IOParams.TIFF_PhotometInterpret := ioTIFF_RGB;
      if WBitmap.PixelFormat = ie48RGB then
      begin
        IOParams.SamplesPerPixel := 3;
        IOParams.BitsPerSample   := 16;
        IOParams.TIFF_PhotometInterpret := ioTIFF_RGB;
      end;
      if (IOParams.BitsPerSample > 8) and (IOParams.SamplesPerPixel <> 1) then
      begin
        IOParams.SamplesPerPixel := 3;
        IOParams.BitsPerSample   := 8;
        IOParams.TIFF_PhotometInterpret := ioTIFF_RGB;
      end;
      if ((IOParams.TIFF_PhotometInterpret = ioTIFF_BLACKISZERO) or (IOParams.TIFF_PhotometInterpret = ioTIFF_WHITEISZERO)) and
        (IOParams.BitsPerSample = 8) and (IOParams.SamplesPerPixel = 3) then
        IOParams.TIFF_PhotometInterpret := ioTIFF_RGB;
      if (IOParams.TIFF_PhotoMetInterpret = ioTIFF_RGB) and (IOParams.SamplesPerPixel = 1) and (IOParams.BitsPerSample = 1) then
        IOParams.TIFF_PhotometInterpret := ioTIFF_BLACKISZERO;
      if (IOParams.TIFF_PhotoMetInterpret = ioTIFF_YCBCR) and (IOParams.TIFF_Compression <> ioTIFF_JPEG) then
        IOParams.TIFF_PhotoMetInterpret := ioTIFF_RGB;
      if (IOParams.TIFF_PhotoMetInterpret = ioTIFF_RGB) and (IOParams.SamplesPerPixel > 3) then
        IOParams.SamplesPerPixel := 3;
      if WBitmap.PixelFormat = ie48RGB then
      begin
        IOParams.SamplesPerPixel := 3;
        IOParams.BitsPerSample   := 16;
        IOParams.TIFF_PhotometInterpret := ioTIFF_RGB;
      end;
      if WBitmap.PixelFormat = ieCMYK then
      begin
        IOParams.SamplesPerPixel := 4;
        IOParams.BitsPerSample := 8;
        IOParams.TIFF_PhotometInterpret := ioTIFF_CMYK;
      end;
      //
      context.T4Options := 0;
      case IOParams.TIFF_PhotometInterpret of
        ioTIFF_WHITEISZERO: context.Photomet := ioTIFF_BLACKISZERO;
        ioTIFF_BLACKISZERO: context.Photomet := ioTIFF_BLACKISZERO;
        ioTIFF_RGBPALETTE:  context.Photomet := ioTIFF_RGBPALETTE;
        ioTIFF_RGB:         context.Photomet := ioTIFF_RGB;
        ioTIFF_TRANSPMASK:  context.Photomet := ioTIFF_RGB;
        ioTIFF_CMYK:
          begin
            context.Photomet := ioTIFF_CMYK;
            IOParams.SamplesPerPixel := 4;
          end;
        ioTIFF_YCBCR:  context.Photomet := ioTIFF_RGB;
        ioTIFF_CIELAB: context.Photomet := ioTIFF_CIELAB;
      else
        context.Photomet := ioTIFF_RGB;
      end;
      inv1bit := false;
      case IOParams.TIFF_Compression of
        ioTIFF_LZW:
          begin
            context.Compression := 5; // LZW
            context.Predictor   := 2;
            if assigned(IOParams.TIFF_LZWCompFunc) then
              context.LZWCompFunc := IOParams.TIFF_LZWCompFunc
            else
            if assigned(IEGlobalSettings().DefTIFF_LZWCOMPFUNC) then
              context.LZWCompFunc := IEGlobalSettings().DefTIFF_LZWCOMPFUNC
            else
            begin
              context.Compression := 1;
              context.Predictor   := 1;
            end;
          end;
        ioTIFF_PACKBITS:
          context.Compression := 32773; // Packbits
        ioTIFF_CCITT1D:
          begin
            context.Compression := 2; // CCITT1D
            context.Photomet    := ioTIFF_WHITEISZERO;
            inv1bit := true;
          end;
        ioTIFF_G3FAX1D:
          begin
            context.Compression := 3; // G3FAX1D
            context.Photomet    := ioTIFF_WHITEISZERO;
            inv1bit := true;
          end;
        ioTIFF_G3FAX2D:
          begin
            context.Compression := 3; // G3FAX2D
            context.T4Options   := 1;
            context.Photomet    := ioTIFF_WHITEISZERO;
            inv1bit := true;
          end;
        ioTIFF_G4FAX:
          begin
            context.Compression := 4; // G4FAX
            context.Photomet    := ioTIFF_WHITEISZERO;
            inv1bit := true;
          end;
        ioTIFF_JPEG, ioTIFF_OLDJPEG:
          begin
            if IOParams.TIFF_Compression = ioTIFF_JPEG then
              context.Compression := 7   // new jpeg
            else
              context.Compression := 6;  // old jpeg
            context.jpegquality    := IOParams.TIFF_JPEGQuality;
            context.jpegcolorspace := IOParams.TIFF_JPEGColorSpace;
            case context.jpegcolorspace of
              ioJPEG_RGB:     context.Photomet := ioTIFF_RGB;
              ioJPEG_GRAYLEV: context.Photomet := ioTIFF_BLACKISZERO;
              ioJPEG_YCbCr:   context.Photomet := ioTIFF_YCBCR;
              ioJPEG_CMYK:    context.Photomet := ioTIFF_CMYK;
              ioJPEG_YCbCrK:
                begin
                  context.Photomet       := ioTIFF_RGB;
                  context.jpegcolorspace := ioJPEG_RGB;
                end;
            end;
          end;
        ioTIFF_ZIP, ioTIFF_Deflate:
          begin
            context.Compression := 32946;
          end;
        else
          context.Compression := 1; // no compression
      end;
      context.FillOrder := IOParams.TIFF_FillOrder;
      if (WBitmap.PixelFormat <> ie1g) and (context.Compression > 1) and (context.Compression < 5) then
      begin
        context.Compression := 1; // no compression
        inv1bit := false;
      end;
      if (IOParams.BitsPerSample <> 8) or ((IOParams.SamplesPerPixel <> 1) and (IOParams.SamplesPerPixel <> 3)) or (context.PhotoMet = ioTIFF_RGBPALETTE) or ((IOParams.BitsPerSample > 1) and (context.PhotoMet = ioTIFF_BLACKISZERO)) or (WBitmap.HasAlphaChannel) then
        context.Predictor := 1;

      if inv1bit then
      begin
        if wbitmap = bitmap then
        begin
          wbitmap := TIEBitmap.Create();
          wbitmap.Assign(bitmap);
        end;
        _Negative1BitEx(wbitmap);
      end;
    end; // end of WBitmap<>nil

    BasePos := OStream.Position;
    XStream := OStream;

    WPosIFD := 0;
    SPosIFD := 0;
    if Ins then
    begin
      // insert as TIFF_ImageIndex image
      OStream.Read(tifhead, sizeof(TTIFFHEADER) - 4); // read header minus posifd
      if tifhead.Id <> $4949 then
      begin
        Progress.Aborting^ := true;
        result := 0;
        exit;
      end;
      numi := 0;
      repeat
        apos := OStream.Position;

        OStream.Read(dw, 4);
        PosIFD := dw;
        
        if (numi = IOParams.TIFF_ImageIndex) or
          ((PosIFD = 0) and (numi < IOParams.TIFF_ImageIndex)) then
        begin
          WPosIFD := apos;
          SPosIFD := PosIFD;
        end;
        if (PosIFD = 0) then
          break;
        OStream.Position := PosIFD;
        OStream.Read(nt, 2);
        OStream.Seek(nt * sizeof(TTIFFTAG), soCurrent);
        inc(numi);
      until false;
      result := numi + 1;
      OStream.Seek(0, soEnd); // write from the end
    end
    else
    begin
      SafeStreamWrite(OStream, Progress.Aborting^, tifhead, sizeof(TTIFFHEADER)); // writes an empty header
      result := 1;
      if Bitmap = nil then
      begin
        // we can put IFD before actual tags (common case: writing EXIF)
        ms := TMemoryStream.Create();
        XStream := ms;
      end
    end;

    // exif
    if IOParams.EXIF_HasEXIFData then
    begin
      // this also write imagedescription, dpix, dpiy and dpi unit
      WriteExifBlock(mainIFD, XStream, IOParams, Progress.Aborting^);
    end
    else
    begin
      // WriteExifBlock not called, then we must write this
      mainIFD.WriteString(XStream, 270, IOParams.TIFF_ImageDescription, Progress.Aborting^);
      mainIFD.WriteSingleRational(XStream, 282, IOParams.DpiX, Progress.Aborting^); // dpix
      mainIFD.WriteSingleRational(XStream, 283, IOParams.DpiY, Progress.Aborting^); // dpiy
      mainIFD.WriteSingleShort(296, 2); // inches units
    end;

    if WBitmap <> nil then
    begin
      mainIFD.WriteSingleLong(256, WBitmap.Width); // ImageWidth
      mainIFD.WriteSingleLong(257, WBitmap.Height); // ImageHeight
      case WBitmap.PixelFormat of
        ie1g:
          // BitsPerSample: 1 bit x sample
          mainIFD.WriteSingleShort(258, 1);
        ie8g:
          mainIFD.WriteSingleShort(258, 8);
        ie8p:
          mainIFD.WriteSingleShort(258, 8);
        ie16g:
          mainIFD.WriteSingleShort(258, 16);
        ie48RGB:
          // RGB 48 bit
          mainIFD.WriteMultiShort(XStream, 258, [16, 16, 16], Progress.Aborting^);
        ieCMYK:
          // CMYK
          mainIFD.WriteMultiShort(XStream, 258, [8, 8, 8, 8], Progress.Aborting^);
        ie24RGB:
          begin
            case context.Photomet of
              ioTIFF_CMYK:        mainIFD.WriteMultiShort(XStream, 258, [8, 8, 8, 8], Progress.Aborting^); // CMYK
              ioTIFF_RGBPALETTE:  mainIFD.WriteSingleShort(258, IOParams.BitsPerSample); // RGBPALETTE
              ioTIFF_BLACKISZERO: mainIFD.WriteSingleShort(258, IOParams.BitsPerSample); // RGBPALETTE
            else
              mainIFD.WriteMultiShort(XStream, 258, [8, 8, 8], Progress.Aborting^); // RGB/YCBCR
            end;
          end;
      end;
      mainIFD.WriteSingleShort(259, context.Compression); // Compression
      if WBitmap.pixelformat = ie1g then
      begin
        case context.Photomet of
          ioTIFF_WHITEISZERO: mainIFD.WriteSingleShort(262, 0); // PhotometricInterpretation=0 (0=white)
          ioTIFF_BLACKISZERO: mainIFD.WriteSingleShort(262, 1); // PhotometricInterpretation=1 (0=black)
        end;
      end
      else
      begin
        case context.Photomet of
          ioTIFF_CMYK:        mainIFD.WriteSingleShort(262, 5); // CMYK
          ioTIFF_CIELAB:      mainIFD.WriteSingleShort(262, 8); // CIELAB
          ioTIFF_RGBPALETTE:  mainIFD.WriteSingleShort(262, 3); // RGBPAlette
          ioTIFF_BLACKISZERO: mainIFD.WriteSingleShort(262, 1); // PhotometricInterpretation=1 (0=black)
          ioTIFF_YCBCR:       mainIFD.WriteSingleShort(262, 6); // YCBCR
        else
          mainIFD.WriteSingleShort(262, 2); // PhotometricInterpretation=2 (RGB)
        end;
      end;
      mainIFD.WriteString(XStream, 269, IOParams.TIFF_DocumentName, Progress.Aborting^);
      // some fax programs require to send default and other parameters to work
      if (context.Compression = 2) or (context.Compression = 3) or (context.Compression = 4) then
      begin
        mainIFD.WriteSingleShort(266, context.FillOrder); // FillOrder
        mainIFD.WriteSingleShort(284, 1); // Planar configuration
        mainIFD.WriteSingleShort(327, 0); // CleanFaxData (0=no incorrect lines)
      end;

      context.BitsPerSample := IOParams.BitsPerSample;

      // write image
      laccess := WBitmap.Access;
      WBitmap.Access := [iedRead];

      if context.Compression = 6 then
        WriteOldJpeg(XStream, WBitmap, mainIFD, context, Progress)
      else
        WriteStrip(XStream, WBitmap, mainIFD, context, Progress);

      WBitmap.Access := laccess;
    end;

    if not Progress.Aborting^ then
    begin
      if WBitmap <> nil then
      begin

        if WBitmap.pixelformat = ie1g then
          samples := 1
        else
          case context.Photomet of
            ioTIFF_CMYK:        samples := 4; // CMYK 4 sample x pixel
            ioTIFF_RGBPALETTE:  samples := 1; // RGBPALETTE, 1 sample x pixel
            ioTIFF_BLACKISZERO: samples := 1; // GRAYSCALE, 1 sample x pixel
            else
              samples := 3;
          end;
        if context.hasalpha then
        begin
          inc(samples);
          mainIFD.WriteMultiShort(XStream, 338, [1], Progress.Aborting^); // extra sample is alpha channel
        end;
        mainIFD.WriteSingleShort(277, samples);

        if context.Predictor = 2 then
          mainIFD.WriteSingleShort(317, 2); // Predictor
        mainIFD.WriteString(XStream, 285, IOParams.TIFF_PageName, Progress.Aborting^);
        if IOParams.TIFF_XPos <> 0 then
          mainIFD.WriteSingleRational(XStream, 286, IOParams.TIFF_XPos, Progress.Aborting^);
        if IOParams.TIFF_YPos <> 0 then
          mainIFD.WriteSingleRational(XStream, 287, IOParams.TIFF_YPos, Progress.Aborting^);

        if (context.Compression = 3) then
          mainIFD.WriteSingleLong(292, context.T4Options)
        else
        if (context.Compression = 4) then
          mainIFD.WriteSingleLong(292, 0);

        if (context.Compression = 3) or (context.Compression = 4) then
          mainIFD.WriteSingleLong(293, 0); // T6Options

        // NewSubfileType
        mainIFD.WriteSingleLong(254, IOParams.TIFF_NewSubfileType);

        // Page number
        if (IOParams.TIFF_PageNumber > -1) or (IOParams.TIFF_PageCount > -1) then
          mainIFD.WriteMultiShort(XStream, 297, [IOParams.TIFF_PageNumber, IOParams.TIFF_PageCount], Progress.Aborting^);

        // IPTC
        WriteIPTC(mainIFD, XStream, IOParams, Progress.Aborting^);

        // XMP
        WriteXMP(mainIFD, XStream, IOParams, Progress.Aborting^);

        {$ifdef IEINCLUDEIMAGINGANNOT}
        // Wang Imaging
        WriteWang(mainIFD, XStream, IOParams, Progress.Aborting^);
        {$endif}

        // ImageEn annotations
        WriteImageEnannot(mainIFD, XStream, IOParams, Progress.Aborting^);

        // ICC
        WriteICC(mainIFD, XStream, IOParams, Progress.Aborting^);

        // Photoshop image resources information
        mainIFD.WriteArrayOfByte(XStream, IETIFFTAG_PHOTOSHOP, 1, IOParams.TIFF_PhotoshopImageResources, Progress.Aborting^);

        // Photoshop "ImageSourceData"
        mainIFD.WriteArrayOfByte(XStream, 37724, 7, IOParams.TIFF_PhotoshopImageSourceData, Progress.Aborting^);

        // colormap
        if context.Photomet = ioTIFF_RGBPALETTE then
        begin
          for i := 0 to ncol - 1 do
          begin
            wcolormap[i]            := GlobalColorMap[i].r * 257;
            wcolormap[i + ncol]     := GlobalColorMap[i].g * 257;
            wcolormap[i + ncol * 2] := GlobalColorMap[i].b * 257;
          end;
          mainIFD.WriteMultiShort(XStream, IETIFFTAG_COLORMAP, slice(wcolormap, ncol * 3), Progress.Aborting^);
        end;
      end;

      apos := IEStreamWordAlign(OStream, Progress.Aborting^);

      if Ins then
      begin
        // insert image
        OStream.Position := WPosIFD;
        dw := apos;
        SafeStreamWrite(OStream, Progress.Aborting^, dw, 4);
      end
      else
      begin
        // write header
        OStream.Position := BasePos;
        tifhead.Id     := $4949;
        tifhead.Ver    := 42;
        tifhead.PosIFD := apos;
        SafeStreamWrite(OStream, Progress.Aborting^, tifhead, sizeof(TTIFFHEADER));
      end;

      // write IFD
      OStream.Position := apos;
      ofs := OStream.Position + 2 + mainIFD.Count * sizeof(TTIFFTAG) + 4;
      ww := mainIFD.Count;
      SafeStreamWrite(OStream, Progress.Aborting^, ww, 2); // tags count
      mainIFD.ReorderTags();
      for i := 0 to mainIFD.Count - 1 do
        SafeStreamWrite(OStream, Progress.Aborting^, mainIFD.Tag[i]^, sizeof(TTIFFTAG));
      if Ins then
      begin
        // insert image
        dw := SPosIFD;
        SafeStreamWrite(OStream, Progress.Aborting^, dw, 4);
      end
      else
      begin
        dw := 0;
        SafeStreamWrite(OStream, Progress.Aborting^, dw, 4); // next IFD (null)
      end;
      if assigned(ms) then
      begin
        RelocateIFD(OStream, apos, ofs, ms);
        SafeStreamWrite(OStream, Progress.Aborting^, pbyte(ms.Memory)^, ms.Size); // write tags (when IFD precedes tags)
      end;
    end; // end of aborting
  finally
    mainIFD.Free();
    ms.Free();
    if (WBitmap <> nil) and (wbitmap <> bitmap) then
      FreeAndNil(WBitmap);
    context.Free();
  end;
end;


procedure WriteExifInteropBlock(parentIFD: TIETIFFIFDWriter; Stream: TStream; var IOParams: TIOParamsVals; var Aborting: boolean);
var
  InteropIFD: TIETIFFIFDWriter;
  q: integer;
  tw: word;
  w: dword;
begin
  InteropIFD := nil;
  try
    InteropIFD := TIETIFFIFDWriter.Create();
    InteropIFD.WriteString(Stream, $0001, IOParams.EXIF_InteropIndex, Aborting);
    InteropIFD.WriteMiniString($0002, IOParams.EXIF_InteropVersion);

    w := Stream.Position;
    if (w and 1) <> 0 then
    begin
      inc(w); // align to word
      q := 0;
      SafeStreamWrite(Stream, Aborting, q, 1); // write an align byte
    end;

    // write IFD
    Stream.Position := w;
    tw := InteropIFD.Count;
    SafeStreamWrite(Stream, Aborting, tw, 2); // tags count
    InteropIFD.ReorderTags();
    for q := 0 to InteropIFD.Count - 1 do
      SafeStreamWrite(Stream, Aborting, InteropIFD.Tag[q]^, sizeof(TTIFFTAG));
    q := 0;
    SafeStreamWrite(Stream, Aborting, q, 4); // next IFD (null)
  finally
    InteropIFD.Free();
  end;
  // write EXIF-Interop tag (point to IFD)
  parentIFD.AddTag(IETIFFTAG_INTEROPIFD, IETIFFTYPE_LONG, 1, w); // w already aligned
end;


procedure WriteExifGPSBlock(parentIFD: TIETIFFIFDWriter; OStream: TStream; var IOParams: TIOParamsVals; var Aborting: boolean);
var
  GPSifd: TIETIFFIFDWriter;
  q: integer;
  tw: word;
  w: dword;
  ms: TMemoryStream;
  ofs: dword;
begin
  GPSifd := nil;
  ms := nil;
  try
    GPSifd := TIETIFFIFDWriter.Create();
    ms := TMemoryStream.Create();
    with IOParams do
    begin
      GPSifd.WriteMiniByteString($0, convVersionStrtoID(EXIF_GPSVersionID));

      if EXIF_GPSLatitudeRef <> '' then
      begin
        GPSifd.WriteMiniString($1, EXIF_GPSLatitudeRef);
        GPSifd.WriteMultiRational(ms, $2, [
          EXIF_GPSLatitudeDegrees,
          EXIF_GPSLatitudeMinutes,
          EXIF_GPSLatitudeSeconds], Aborting);
      end;

      if EXIF_GPSLongitudeRef <> '' then
      begin
        GPSifd.WriteMiniString($3, EXIF_GPSLongitudeRef);
        GPSifd.WriteMultiRational(ms, $4, [
          EXIF_GPSLongitudeDegrees,
          EXIF_GPSLongitudeMinutes,
          EXIF_GPSLongitudeSeconds], Aborting);
      end;

      if EXIF_GPSAltitudeRef <> '' then
      begin
        GPSifd.WriteSingleByte($5, IEStrToIntDef(EXIF_GPSAltitudeRef, 0));
        GPSifd.WriteSingleRational(ms, $6, EXIF_GPSAltitude, Aborting);
      end;

      GPSifd.WriteMultiRational(ms, $7, [
        EXIF_GPSTimeStampHour,
        EXIF_GPSTimeStampMinute,
        EXIF_GPSTimeStampSecond], Aborting);

      GPSifd.WriteString(ms, $8, EXIF_GPSSatellites, Aborting);
      GPSifd.WriteMiniString($9, EXIF_GPSStatus);
      GPSifd.WriteMiniString($A, EXIF_GPSMeasureMode);
      GPSifd.WriteSingleRational(ms, $B, EXIF_GPSDOP, Aborting);

      if EXIF_GPSSpeedRef <> '' then
      begin
        GPSifd.WriteMiniString($C, EXIF_GPSSpeedRef);
        GPSifd.WriteSingleRational(ms, $D, EXIF_GPSSpeed, Aborting);
      end;

      if EXIF_GPSTrackRef <> '' then
      begin
        GPSifd.WriteMiniString($E, EXIF_GPSTrackRef);
        GPSifd.WriteSingleRational(ms, $F, EXIF_GPSTrack, Aborting);
      end;

      if EXIF_GPSImgDirectionRef <> '' then
      begin
        GPSifd.WriteMiniString($10, EXIF_GPSImgDirectionRef);
        GPSifd.WriteSingleRational(ms, $11, EXIF_GPSImgDirection, Aborting);
      end;

      GPSifd.WriteString(ms, $12, EXIF_GPSMapDatum, Aborting);

      if EXIF_GPSDestLatitudeRef <> '' then
      begin
        GPSifd.WriteMiniString($13, EXIF_GPSDestLatitudeRef);
        GPSifd.WriteMultiRational(ms, $14, [
          EXIF_GPSDestLatitudeDegrees,
          EXIF_GPSDestLatitudeMinutes,
          EXIF_GPSDestLatitudeSeconds], Aborting);
      end;

      if EXIF_GPSDestLongitudeRef <> '' then
      begin
        GPSifd.WriteMiniString($15, EXIF_GPSDestLongitudeRef);
        GPSifd.WriteMultiRational(ms, $16, [
          EXIF_GPSDestLongitudeDegrees,
          EXIF_GPSDestLongitudeMinutes,
          EXIF_GPSDestLongitudeSeconds], Aborting);
      end;

      if EXIF_GPSDestBearingRef <> '' then
      begin
        GPSifd.WriteMiniString($17, EXIF_GPSDestBearingRef);
        GPSifd.WriteSingleRational(ms, $18, EXIF_GPSDestBearing, Aborting);
      end;

      if EXIF_GPSDestDistanceRef <> '' then
      begin
        GPSifd.WriteMiniString($19, EXIF_GPSDestDistanceRef);
        GPSifd.WriteSingleRational(ms, $1A, EXIF_GPSDestDistance, Aborting);
      end;

      GPSifd.WriteString(ms, $1D, EXIF_GPSDateStamp, Aborting);
    end;

    // align to word
    w := OStream.Position;
    if (w and 1) <> 0 then
    begin
      inc(w);
      q := 0;
      SafeStreamWrite(OStream, Aborting, q, 1); // write an align byte
    end;

    // write IFD
    ofs := OStream.Position + 2 + GPSifd.Count * sizeof(TTIFFTAG) + 4;
    tw := GPSifd.Count;
    SafeStreamWrite(OStream, Aborting, tw, 2); // tags count
    GPSifd.ReorderTags();
    for q := 0 to GPSifd.Count - 1 do
      SafeStreamWrite(OStream, Aborting, GPSifd.Tag[q]^, sizeof(TTIFFTAG));
    q := 0;
    SafeStreamWrite(OStream, Aborting, q, 4); // next IFD (null)
    RelocateIFD(OStream, w, ofs, ms);
    SafeStreamWrite(OStream, Aborting, pbyte(ms.Memory)^, ms.Size); // write tags
  finally
    GPSifd.Free();
    ms.Free();
  end;
  // write EXIF-GPS tag (point to IFD)
  parentIFD.AddTag(IETIFFTAG_EXIFGPSIFD, IETIFFTYPE_LONG, 1, w);  // w already aligned
end;


procedure WriteExifBlock(parentIFD: TIETIFFIFDWriter; OStream: TStream; var IOParams: TIOParamsVals; var Aborting: boolean);
var
  EXIFifd: TIETIFFIFDWriter;
  q, w: integer;
  tw: word;
  ms: TMemoryStream;
  ofs: dword;
begin
  // tags in parentIFD
  with IOParams do
  begin
    if EXIF_ImageDescription <> '' then
      parentIFD.WriteString(OStream, 270, EXIF_ImageDescription, Aborting);
    if EXIF_XResolution <> 0 then
      parentIFD.WriteSingleRational(OStream, 282, EXIF_XResolution, Aborting); // dpix
    if EXIF_YResolution <> 0 then
      parentIFD.WriteSingleRational(OStream, 283, EXIF_YResolution, Aborting); // dpiy
    parentIFD.WriteSingleShort(274, TIFF_Orientation); // Orientation
    parentIFD.WriteSingleShort(296, 2); // inches units
    if EXIF_Software <> '' then
      parentIFD.WriteString(OStream, 305, EXIF_Software, Aborting);

    if EXIF_XPRating > -1 then
      parentIFD.WriteSingleShort($4746, EXIF_XPRating);
    if EXIF_XPTitle <> '' then
      parentIFD.WriteWideString(OStream, $9C9B, EXIF_XPTitle, Aborting);
    if EXIF_XPComment <> '' then
      parentIFD.WriteWideString(OStream, $9C9C, EXIF_XPComment, Aborting);
    if EXIF_XPAuthor <> '' then
      parentIFD.WriteWideString(OStream, $9C9D, EXIF_XPAuthor, Aborting);
    if EXIF_XPKeywords <> '' then
      parentIFD.WriteWideString(OStream, $9C9E, EXIF_XPKeywords, Aborting);
    if EXIF_XPSubject <> '' then
      parentIFD.WriteWideString(OStream, $9C9F, EXIF_XPSubject, Aborting);

    if EXIF_Artist <> '' then
      parentIFD.WriteString(OStream, 315, EXIF_Artist, Aborting);
    if EXIF_Make <> '' then
      parentIFD.WriteString(OStream, 271, EXIF_Make, Aborting);
    if EXIF_Model <> '' then
      parentIFD.WriteString(OStream, 272, EXIF_Model, Aborting);
    if EXIF_DateTime <> '' then
      parentIFD.WriteString(OStream, 306, EXIF_DateTime, Aborting);
    if (EXIF_WhitePoint[0] <> -1) or (EXIF_WhitePoint[1] <> -1) then
      parentIFD.WriteMultiRational(OStream, 318, [EXIF_WhitePoint[0], EXIF_WhitePoint[1]], Aborting);
    if EXIF_YCbCrPositioning <> 0 then
      parentIFD.WriteSingleShort(531, EXIF_YCbCrPositioning);
    if (EXIF_PrimaryChromaticities[0] <> -1) or (EXIF_PrimaryChromaticities[1] <> -1) or (EXIF_PrimaryChromaticities[2] <> -1) or
       (EXIF_PrimaryChromaticities[3] <> -1) or (EXIF_PrimaryChromaticities[4] <> -1) or (EXIF_PrimaryChromaticities[5] <> -1) then
      parentIFD.WriteMultiRational(OStream, 319, [
          EXIF_PrimaryChromaticities[0],
          EXIF_PrimaryChromaticities[1],
          EXIF_PrimaryChromaticities[2],
          EXIF_PrimaryChromaticities[3],
          EXIF_PrimaryChromaticities[4],
          EXIF_PrimaryChromaticities[5] ], Aborting);
    if (EXIF_YCbCrCoefficients[0] <> -1) or (EXIF_YCbCrCoefficients[1] <> -1) or (EXIF_YCbCrCoefficients[2] <> -1) then
      parentIFD.WriteMultiRational(OStream, 529, [
          EXIF_YCbCrCoefficients[0],
          EXIF_YCbCrCoefficients[1],
          EXIF_YCbCrCoefficients[2] ], Aborting);
    if (EXIF_ReferenceBlackWhite[0] <> -1) or (EXIF_ReferenceBlackWhite[1] <> -1) or (EXIF_ReferenceBlackWhite[2] <> -1) or
       (EXIF_ReferenceBlackWhite[3] <> -1) or (EXIF_ReferenceBlackWhite[4] <> -1) or (EXIF_ReferenceBlackWhite[5] <> -1) then
      parentIFD.WriteMultiRational(OStream, 532, [
          EXIF_ReferenceBlackWhite[0],
          EXIF_ReferenceBlackWhite[1],
          EXIF_ReferenceBlackWhite[2],
          EXIF_ReferenceBlackWhite[3],
          EXIF_ReferenceBlackWhite[4],
          EXIF_ReferenceBlackWhite[5] ], Aborting);
    if EXIF_Copyright <> '' then
      parentIFD.WriteString(OStream, IETIFFTAG_COPYRIGHT, EXIF_Copyright, Aborting);
  end;

  EXIFifd := nil;
  ms := nil;
  try
    EXIFifd := TIETIFFIFDWriter.Create();
    ms := TMemoryStream.Create();
    // tags in EXIFifd
    with IOParams do
    begin
      EXIFifd.WriteSingleRational(ms, $829A, EXIF_ExposureTime, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $829D, EXIF_FNumber, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleShort($8822, EXIF_ExposureProgram, EXIF_Tags);
      if EXIF_ISOSpeedRatings[1] <> 0 then
        EXIFifd.WriteMultiShort(ms, $8827, [EXIF_ISOSpeedRatings[0], EXIF_ISOSpeedRatings[1]], Aborting)
      else
      if EXIF_ISOSpeedRatings[0] <> 0 then
        EXIFifd.WriteSingleShort($8827, EXIF_ISOSpeedRatings[0]);
      EXIFifd.WriteMiniString($9000, EXIF_ExifVersion);
      if EXIF_DateTimeOriginal <> '' then
        EXIFifd.WriteString(ms, $9003, EXIF_DateTimeOriginal, Aborting);
      if EXIF_DateTimeDigitized <> '' then
        EXIFifd.WriteString(ms, $9004, EXIF_DateTimeDigitized, Aborting);
      EXIFifd.WriteSingleRational(ms, $9102, EXIF_CompressedBitsPerPixel, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $9201, EXIF_ShutterSpeedValue, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $9202, EXIF_ApertureValue, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $9203, EXIF_BrightnessValue, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $9204, EXIF_ExposureBiasValue, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $9205, EXIF_MaxApertureValue, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $9206, EXIF_SubjectDistance, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleShort($9207, EXIF_MeteringMode, EXIF_Tags);
      EXIFifd.WriteSingleShort($9208, EXIF_LightSource, EXIF_Tags);
      EXIFifd.WriteSingleShort($9209, EXIF_Flash, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $920A, EXIF_FocalLength, Aborting, EXIF_Tags);
      if EXIF_SubsecTime <> '' then
        EXIFifd.WriteString(ms, $9290, EXIF_SubsecTime, Aborting);
      if EXIF_SubsecTimeOriginal <> '' then
        EXIFifd.WriteString(ms, $9291, EXIF_SubsecTimeOriginal, Aborting);
      if EXIF_SubsecTimeDigitized <> '' then
        EXIFifd.WriteString(ms, $9292, EXIF_SubsecTimeDigitized, Aborting);
      if EXIF_FlashPixVersion <> '' then
        EXIFifd.WriteMiniString($A000, EXIF_FlashPixVersion);
      EXIFifd.WriteSingleShort($A001, EXIF_ColorSpace, EXIF_Tags);
      EXIFifd.WriteSingleShort($A002, EXIF_ExifImageWidth, EXIF_Tags); // could be also LONG
      EXIFifd.WriteSingleShort($A003, EXIF_ExifImageHeight, EXIF_Tags); // could be also LONG
      if EXIF_RelatedSoundFile <> '' then
        EXIFifd.WriteString(ms, $A004, EXIF_RelatedSoundFile, Aborting);
      EXIFifd.WriteSingleRational(ms, $A20E, EXIF_FocalPlaneXResolution, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $A20F, EXIF_FocalPlaneYResolution, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleShort($A210, EXIF_FocalPlaneResolutionUnit, EXIF_Tags);
      EXIFifd.WriteSingleRational(ms, $A215, EXIF_ExposureIndex, Aborting, EXIF_Tags);
      EXIFifd.WriteSingleShort($A217, EXIF_SensingMethod, EXIF_Tags);
      EXIFifd.WriteSingleUndefined($A300, EXIF_FileSource, EXIF_Tags);
      EXIFifd.WriteSingleUndefined($A301, EXIF_SceneType, EXIF_Tags);
      if EXIF_ExposureMode <> -1 then
        EXIFifd.WriteSingleShort($A402, EXIF_ExposureMode);
      if EXIF_WhiteBalance <> -1 then
        EXIFifd.WriteSingleShort($A403, EXIF_WhiteBalance);
      if EXIF_DigitalZoomRatio <> -1 then
        EXIFifd.WriteSingleRational(ms, $A404, EXIF_DigitalZoomRatio, Aborting);
      if EXIF_FocalLengthIn35mmFilm <> -1 then
        EXIFifd.WriteSingleShort($A405, EXIF_FocalLengthIn35mmFilm);
      if EXIF_SceneCaptureType <> -1 then
        EXIFifd.WriteSingleShort($A406, EXIF_SceneCaptureType);
      if EXIF_GainControl <> -1 then
        EXIFifd.WriteSingleShort($A407, EXIF_GainControl);
      if EXIF_Contrast <> -1 then
        EXIFifd.WriteSingleShort($A408, EXIF_Contrast);
      if EXIF_Saturation <> -1 then
        EXIFifd.WriteSingleShort($A409, EXIF_Saturation);
      if EXIF_Sharpness <> -1 then
        EXIFifd.WriteSingleShort($A40A, EXIF_Sharpness);
      if EXIF_SubjectDistanceRange <> -1 then
        EXIFifd.WriteSingleShort($A40C, EXIF_SubjectDistanceRange);
      if EXIF_ImageUniqueID <> '' then
        EXIFifd.WriteString(ms, $A420, EXIF_ImageUniqueID, Aborting);

      WriteEXIFUserComment(EXIFifd, ms, EXIF_UserCommentCode, EXIF_UserComment, Aborting);  // tag $9286

      WriteEXIFMakerNote(EXIFifd, ms, IETIFFTAG_EXIFMAKERNOTE, EXIF_MakerNote, Aborting);

      if EXIF_InteropIndex <> '' then
        WriteExifInteropBlock(EXIFifd, ms, IOParams, Aborting);
    end;

    // align to word
    w := OStream.Position;
    if (w and 1) <> 0 then
    begin
      inc(w);
      q := 0;
      SafeStreamWrite(OStream, Aborting, q, 1); // write an align byte
    end;

    // write IFD
    ofs := OStream.Position + 2 + EXIFifd.Count * sizeof(TTIFFTAG) + 4;
    tw := EXIFifd.Count;
    SafeStreamWrite(OStream, Aborting, tw, 2); // tags count
    EXIFifd.ReorderTags();
    for q := 0 to EXIFifd.Count - 1 do
      SafeStreamWrite(OStream, Aborting, EXIFifd.Tag[q]^, sizeof(TTIFFTAG));
    q := 0;
    SafeStreamWrite(OStream, Aborting, q, 4); // next IFD (null)
    RelocateIFD(OStream, w, ofs, ms);
    SafeStreamWrite(OStream, Aborting, pbyte(ms.Memory)^, ms.Size); // write tags
  finally
    EXIFifd.Free();
    ms.Free();
  end;

  // write EXIF tag (point to IFD)
  parentIFD.AddTag(IETIFFTAG_EXIFIFD, IETIFFTYPE_LONG, 1, w); // w is already aligned
  if IOParams.EXIF_GPSVersionID <> '' then
    WriteExifGPSBlock(parentIFD, OStream, IOParams, Aborting);
end;


 //////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
//**********************************************************************************//
//* TIFF Utilities                                                                 *//
//**********************************************************************************//
////////////////////////////////////////////////////////////////////////////////////
 //////////////////////////////////////////////////////////////////////////////////



// return images count
function TIFFEnumImages(Stream: TStream): integer;
var
  pr: TProgressRec;
  tempAlphaChannel: TIEMask;
  ab: boolean;
  params: TIOParamsVals;
begin
  tempAlphaChannel := nil;
  ab := false;
  pr.Aborting := @ab;
  pr.fOnProgress := nil;
  params := TIOParamsVals.Create(nil);
  try
    TIFFReadStream(nil, Stream, result, params, pr, true, tempAlphaChannel, true, true, false, false); // result is inside...
  finally
    params.Free();
  end;
end;


function TIFFDeleteImStream(Stream: TStream; idx: integer): integer;
begin
  result := TIFFDeleteImStreamGroup(Stream, @idx, 1);
end;

function TIFFDeleteImStreamGroup(Stream: TStream; idxlist: pintegerarray; idxcount: integer): integer;
var
  TIFFHeader: TTIFFHeader;
  PosIFD, t, q: integer;
  LittleEndian: boolean;
  numi, ii: integer;
  os: TMemoryStream;
  IFD: PIFD;
  nt, ww: word;
  wp, bp, lp1, lp2, lp3, lp4: integer;
  sz, sz2: integer;
  ia1, ia2: pintegerarray;
  wa1, wa2: pwordarray;
  xIdTag: word; // tag identifier
  xDataType, lxDataType: word; // data type
  xDataNum: integer; // data count
  xDataPos: integer; // data position
  idx: integer;
begin
  result := 0;
  // read header (minus IFD position)
  bp := Stream.Position;
  Stream.read(TIFFHeader, sizeof(TTIFFHeader) - 4); // doesn't read IFD position
  if (TIFFHeader.Id <> $4949) and (TIFFHeader.id <> $4D4D) then
    exit;
  LittleEndian := TIFFHeader.Id = $4949;
  // write header (minus IFD position)
  os := TMemoryStream.Create;
  os.Size := Stream.Size;
  os.Write(TIFFHeader, sizeof(TIFFHeader) - 4);
  // IFD read loop
  numi := 0;
  idx := 0;
  wp := os.Position;
  PosIFD := 0;
  os.Write(PosIFD, 4); // blank space for IFD position
  lp4 := 0; // watch-dog for auto-looping IFD
  repeat
    Stream.Read(PosIFD, 4); // read IFD position
    if (PosIFD = 0) or (lp4 = PosIFD) then
      break; // end of images
    lp4 := PosIFD;
    PosIFD := IECSwapDWord(PosIFD, not LittleEndian);
    Stream.Position := PosIFD; // go to the IFD
    Stream.read(nt, 2); // read tags count
    nt := IECSwapWord(nt, not LittleEndian);
    // read tags
    getmem(IFD, nt * sizeof(TTIFFTAG));
    Stream.read(pbyte(IFD)^, sizeof(TTIFFTAG) * nt);
    lp3 := Stream.Position; // save reading position
    //
    if (idx < idxcount) and (idxlist[idx] = numi) then
      inc(idx)
    else
    begin
      // write tags
      ia1 := nil;
      ia2 := nil;
      wa1 := nil;
      lxDataType := 0;
      // search for StripByteCount or TileByteCount (we need them now)
      for t := 0 to nt - 1 do
        with IFD^[t] do
        begin
          xIdTag := IECSwapWord(IdTag, not LittleEndian);
          if (xIdTag = 279) or (xIdTag = 325) then
          begin
            xDataType := IECSwapWord(DataType, not LittleEndian);
            xDataNum := IECSwapDWord(DataNum, not LittleEndian);
            xDataPos := IECSwapDWord(DataPos, not LittleEndian);
            sz := IETIFFCalcTagSize(xDataType) * xDataNum;
            lxDataType := xDataType;
            getmem(ia1, sz);
            wa1 := pwordarray(ia1);
            if sz > 4 then
            begin
              Stream.Position := xDataPos;
              Stream.Read(ia1^, sz);
            end
            else
              CopyMemory(ia1, @DataPos, sz);
          end;
        end;
      for t := 0 to nt - 1 do
        with IFD^[t] do
        begin
          xIdTag := IECSwapWord(IdTag, not LittleEndian);
          xDataType := IECSwapWord(DataType, not LittleEndian);
          xDataNum := IECSwapDWord(DataNum, not LittleEndian);
          xDataPos := IECSwapDWord(DataPos, not LittleEndian);
          sz := IETIFFCalcTagSize(xDataType) * xDataNum;
          if (xIdTag = 273) or (xIdTag = 324) then
          begin
            // we are reading StripOffsets or TileOffsets
            getmem(ia2, sz);
            wa2 := pwordarray(ia2);
            if sz > 4 then
            begin
              Stream.Position := xDataPos;
              Stream.Read(ia2^, sz);
            end
            else
              CopyMemory(ia2, @DataPos, sz);
            // write data referenced by array
            for q := 0 to xDataNum - 1 do
            begin
              if xDataType = IETIFFTYPE_SHORT then
              begin
                // SHORT
                Stream.Position := IECSwapWord(wa2^[q], not LittleEndian);
                ww := os.Position;
                wa2^[q] := IECSwapWord(ww, not LittleEndian);
              end
              else
              begin
                // LONG
                Stream.Position := IECSwapDWord(ia2^[q], not LittleEndian);
                ia2^[q] := IECSwapDWord(os.Position, not LittleEndian);
              end;
              if lxDataType = IETIFFTYPE_SHORT then
              begin
                sz2 := IECSwapWord(wa1^[q], not LittleEndian);
                if sz2 > 0 then
                  IECopyFrom(os, Stream, sz2) // SHORT
              end
              else
              begin
                sz2 := IECSwapDWord(ia1^[q], not LittleEndian);
                if sz2 > 0 then
                  IECopyFrom(os, Stream, sz2); // LONG
              end;
            end;
            // write array
            if sz > 4 then
            begin
              DataPos := IECSwapDWord(os.Position, not LittleEndian);
              os.Write(ia2^, sz);
            end
            else
              CopyMemory(@DataPos, ia2, sz);
          end
          else
          if (sz > 4) or (xDataType = IETIFFTYPE_ASCII) then
          begin
            // DataPos now point to an area of the file (it can be ASCII, too)
            DataPos := IECSwapDWord(os.Position, not LittleEndian);
            Stream.Position := xDataPos;
            if Stream.Position < os.Size then
            begin
              if sz > 0 then
                IECopyFrom(os, Stream, sz);
            end
            else
              os.Position := os.Position+sz;
          end;
        end;
      freemem(ia2);
      freemem(ia1);
      // write IFD
      lp1 := os.Position; // save IFD position
      ww := IECSwapWord(nt, not LittleEndian);
      os.Write(ww, 2); // write tags count
      os.Write(IFD^, nt * sizeof(TTIFFTAG));
      lp2 := os.Position; // save position of next reading
      // write IFD position
      os.Position := wp;
      ii := IECSwapDWord(lp1, not LittleEndian);
      os.Write(ii, 4);
      os.Position := lp2; // point to next byte to reading
      wp := lp2;
      PosIFD := 0;
      os.Write(PosIFD, 4); // write blank space for IFD position
      //
      inc(result);
    end;
    // free tags
    freemem(IFD);
    inc(numi);
    // point to the next byte to read
    Stream.Position := lp3;
  until false;
  // write final IFD position (0)
  lp1 := os.Position;
  os.Position := wp;
  PosIFD := 0;
  os.Write(PosIFD, 4);
  // write "os" to Stream
  Stream.size := bp;
  os.Position := 0;
  if lp1 + 1 > 0 then
    IECopyFrom(Stream, os, lp1 + 1);
  FreeAndNil(os);
end;

function TIFFLoadTags(Stream: TStream; var numi: integer; ImageIndex: integer; IFD: TIETIFFIFDReader): boolean;
var
  TIFFHeader: TTIFFHeader;
  PosIFD: integer;
begin
  result := false;
  IFD.Clear();
  // read header
  IFD.Stream := Stream;
  IFD.StreamBase := 0;
  Stream.read(TIFFHeader, sizeof(TTIFFHeader));
  if (TIFFHeader.Id <> $4949) and (TIFFHeader.id <> $4D4D) then
    exit;
  IFD.LittleEndian := TIFFHeader.Id = $4949;
  if not IFD.LittleEndian then
    TIFFHeader.PosIFD := IESwapDWord(TIFFHeader.PosIFD); // converts to LittleEndian
  // read main IFD (of the selected image)
  IFD.IFD := nil;
  numi := 0;
  PosIFD := TIFFHeader.PosIFD;
  if PosIFD = 0 then
    exit;
  if not IFD.ReadIFD(ImageIndex, TIFFHeader.PosIFD, numi) then
    exit;
  result := true;
end;


// extract a TIFF from a multipage TIFF
procedure TIFFExtractImStream(Stream: TStream; idx: integer; OutStream: TStream);
var
  TIFFHeader: TTIFFHeader;
  PosIFD: int64;
  t: integer;
  LittleEndian: boolean;
  numi, ii: integer;
  IFD: PIFD;
  nt, ww: word;
  wp, lp1, lp2, lp3, lp4: int64;
  sz: integer;
  t279, t325, t514: pointer;
  xIdTag: word;                 // tag identifier
  xDataType, lxDataType: word;  // data type
  xDataNum: integer;            // data count
  xDataPos: dword;              // data position

  procedure ReadByteCountTag(ntag: integer; var ptr: pointer);
  begin
    with IFD^[ntag] do
    begin
      xDataType := IECSwapWord(DataType, not LittleEndian);
      xDataNum := IECSwapDWord(DataNum, not LittleEndian);
      xDataPos := IECSwapDWord(DataPos, not LittleEndian);
      sz := IETIFFCalcTagSize(xDataType) * xDataNum;
      lxDataType := xDataType;
      getmem(ptr, sz);
      try
        if sz > 4 then
        begin
          Stream.Position := xDataPos;
          Stream.Read(pbyte(ptr)^, sz);
        end
        else
          CopyMemory(ptr, @DataPos, sz);
      except
        FreeAndNil(ptr);
        raise;
      end;
    end;
  end;

  procedure ProcessOffsetsTag(ntag: integer; var ByteCountTag: pointer);
  var
    OffsetsTag: pointer;
    q: integer;
    pw: pwordarray;
    pd: pdwordarray;
    sz2: integer;
  begin
    with IFD^[ntag] do
    begin
      getmem(OffsetsTag, sz);
      try
        if sz > 4 then
        begin
          Stream.Position := xDataPos;
          Stream.Read(pbyte(OffsetsTag)^, sz);
        end
        else
          CopyMemory(OffsetsTag, @DataPos, sz);
        // write data referenced by array
        for q := 0 to xDataNum - 1 do
        begin
          if xDataType = IETIFFTYPE_SHORT then
          begin
            // SHORT
            pw := pwordarray(OffsetsTag);
            Stream.Position := IECSwapWord(pw^[q], not LittleEndian);
            ww := OutStream.Position;
            pw^[q] := IECSwapWord(ww, not LittleEndian);
          end
          else
          begin
            // LONG
            pd := pdwordarray(OffsetsTag);
            Stream.Position := IECSwapDWord(pd^[q], not LittleEndian);
            pd^[q] := IECSwapDWord(OutStream.Position, not LittleEndian);
          end;
          if (ByteCountTag=nil) then
          begin
            if Stream.Size-Stream.Position > 0 then
              IECopyFrom(OutStream, Stream, Stream.Size-Stream.Position);
          end
          else
          begin
            if lxDataType = IETIFFTYPE_SHORT then
            begin
              pw := pwordarray(ByteCountTag);
              sz2 := IECSwapWord(pw^[q], not LittleEndian);
              if sz2 > 0 then
                IECopyFrom(OutStream, Stream, sz2); // SHORT
            end
            else
            begin
              pd := pdwordarray(ByteCountTag);
              sz2 := IECSwapDWord(pd^[q], not LittleEndian);
              if sz2 > 0 then
                IECopyFrom(OutStream, Stream, sz2); // LONG
            end;
          end;
        end;
        // write array
        if sz > 4 then
        begin
          DataPos := IECSwapDWord(OutStream.Position, not LittleEndian);
          OutStream.Write(pbyte(OffsetsTag)^, sz);
        end
        else
          CopyMemory(@DataPos, OffsetsTag, sz);
      finally
        freemem(OffsetsTag);
      end;
    end;
  end;

begin
  Stream.Position := 0;
  // read header (minus IFD position)
  Stream.read(TIFFHeader, sizeof(TTIFFHeader) - 4); // doesn't read IFD position
  if (TIFFHeader.Id <> $4949) and (TIFFHeader.id <> $4D4D) then
    exit;
  LittleEndian := TIFFHeader.Id = $4949;
  // write header (minus IFD position)
  OutStream.Write(TIFFHeader, sizeof(TIFFHeader) - 4);
  // IFD read loop
  numi := 0;
  wp := OutStream.Position;
  PosIFD := 0;
  OutStream.Write(PosIFD, 4); // blank space for IFD position
  lp4 := 0; // watch-dog for auto-looping IFD
  repeat

    Stream.Read(PosIFD, 4); // read IFD position
    if (PosIFD = 0) or (lp4 = PosIFD) then
      break; // end of images
    lp4 := PosIFD;
    PosIFD := IECSwapDWord(PosIFD, not LittleEndian);
    Stream.Position := PosIFD; // go to the IFD
    Stream.read(nt, 2); // read tags count
    nt := IECSwapWord(nt, not LittleEndian);

    getmem(IFD, nt * sizeof(TTIFFTAG));

    try
      // read tags
      Stream.read(pbyte(IFD)^, sizeof(TTIFFTAG) * nt);
      lp3 := Stream.Position; // save reading position

      if numi = idx then
      begin
        // write tags
        t279 := nil;
        t325 := nil;
        t514 := nil;
        lxDataType := 0;

        try
          // search for byteCounts tags (we need them now)
          for t := 0 to nt - 1 do
            case IECSwapWord(IFD^[t].IdTag, not LittleEndian) of
              279: ReadByteCountTag(t, t279);
              325: ReadByteCountTag(t, t325);
              514: ReadByteCountTag(t, t514);
            end;

          for t := 0 to nt - 1 do
            with IFD^[t] do
            begin
              xIdTag := IECSwapWord(IdTag, not LittleEndian);
              xDataType := IECSwapWord(DataType, not LittleEndian);
              xDataNum := IECSwapDWord(DataNum, not LittleEndian);
              xDataPos := IECSwapDWord(DataPos, not LittleEndian);
              sz := IETIFFCalcTagSize(xDataType) * xDataNum;
              case xIdTag of
                273: ProcessOffsetsTag(t, t279);
                324: ProcessOffsetsTag(t, t325);
                513: ProcessOffsetsTag(t, t514);
                else
                if (sz > 4) then
                begin
                  // DataPos now point to an area of the file (it can be ASCII, too)
                  DataPos := IECSwapDWord(OutStream.Position, not LittleEndian);
                  Stream.Position := xDataPos;
                  if sz > 0 then
                    IECopyFrom(OutStream, Stream, sz);
                end;
              end;
            end;
        finally
          freemem(t279);
          freemem(t325);
          freemem(t514);
        end;
      
        // write IFD
        lp1 := OutStream.Position; // save IFD position
        ww := IECSwapWord(nt, not LittleEndian);
        OutStream.Write(ww, 2); // write tags count
        OutStream.Write(IFD^, nt * sizeof(TTIFFTAG));
        lp2 := OutStream.Position; // save position of next reading
        // write IFD position
        OutStream.Position := wp;
        ii := IECSwapDWord(lp1, not LittleEndian);
        OutStream.Write(ii, 4);
        OutStream.Position := lp2; // point to next byte to reading
        wp := lp2;
        PosIFD := 0;
        OutStream.Write(PosIFD, 4); // write blank space for IFD position
      end;

    finally
      // free tags
      freemem(IFD);
    end;
    
    inc(numi);

    // point to the next byte to read
    Stream.Position := lp3;

  until false;

  // write final IFD position (0)
  OutStream.Position := wp;
  PosIFD := 0;
  OutStream.Write(PosIFD, 4);
end;
//*)


function TIFFInsertImStream(Stream: TStream; ToInsert: TStream; idx: integer; OutStream: TStream; internal: boolean): integer;
var
  TIFFHeader: TTIFFHeader;
  PosIFD, t, q: integer;
  numi, ii: integer;
  IFD: PIFD;
  nt, ww: word;
  wp, lp1, lp2, lp3, lp4: integer;
  sz, sz2: integer;
  ia1, ia2: pintegerarray;
  wa1, wa2: pwordarray;
  xIdTag: word; // tag identifier
  xDataType, lxDataType: word; // data type
  xDataNum: integer; // data count
  xDataPos: integer; // data position
  LittleEndian: boolean;
begin
  result := 0;
  Stream.Position := 0;
  // read header (minus IFD position)
  Stream.read(TIFFHeader, sizeof(TTIFFHeader) - 4); // doesn't read IFD position
  if (TIFFHeader.Id <> $4949) and (TIFFHeader.id <> $4D4D) then
    exit;
  LittleEndian := TIFFHeader.Id = $4949;
  if not Internal then
  begin
    // write header (minus IFD position)
    OutStream.Write(TIFFHeader, sizeof(TIFFHeader) - 4);
  end;
  // IFD read loop
  numi := 0;
  wp := OutStream.Position;
  PosIFD := 0;
  OutStream.Write(PosIFD, 4); // blank space for IFD position
  lp4 := 0; // watch-dog for auto-looping IFD
  repeat
    //
    if numi = idx then
    begin
      // insert ToInsert here
      OutStream.Position := OutStream.Position - 4;
      wp := TIFFInsertImStream(ToInsert, nil, -1, OutStream, true);
      inc(numi);
    end
    else
    begin
      Stream.Read(PosIFD, 4); // read IFD position
      if (PosIFD = 0) or (lp4 = PosIFD) then
        break; // end of images
      lp4 := PosIFD;
      PosIFD := IECSwapDWord(PosIFD, not LittleEndian);
      Stream.Position := PosIFD; // go to the IFD
      Stream.read(nt, 2); // read tags count
      nt := IECSwapWord(nt, not LittleEndian);
      // read tags
      getmem(IFD, nt * sizeof(TTIFFTAG));
      Stream.read(pbyte(IFD)^, sizeof(TTIFFTAG) * nt);
      lp3 := Stream.Position; // save reading position
      // write tags
      ia1 := nil;
      ia2 := nil;
      wa1 := nil;
      lxDataType := 0;
      // search for StripByteCount or TileByteCount (we need them now)
      for t := 0 to nt - 1 do
        with IFD^[t] do
        begin
          xIdTag := IECSwapWord(IdTag, not LittleEndian);
          if (xIdTag = 279) or (xIdTag = 325) then
          begin
            xDataType := IECSwapWord(DataType, not LittleEndian);
            xDataNum := IECSwapDWord(DataNum, not LittleEndian);
            xDataPos := IECSwapDWord(DataPos, not LittleEndian);
            sz := IETIFFCalcTagSize(xDataType) * xDataNum;
            lxDataType := xDataType;
            getmem(ia1, sz);
            wa1 := pwordarray(ia1);
            if sz > 4 then
            begin
              Stream.Position := xDataPos;
              Stream.Read(ia1^, sz);
            end
            else
              CopyMemory(ia1, @DataPos, sz);
          end;
        end;
      for t := 0 to nt - 1 do
        with IFD^[t] do
        begin
          xIdTag := IECSwapWord(IdTag, not LittleEndian);
          xDataType := IECSwapWord(DataType, not LittleEndian);
          xDataNum := IECSwapDWord(DataNum, not LittleEndian);
          xDataPos := IECSwapDWord(DataPos, not LittleEndian);
          sz := IETIFFCalcTagSize(xDataType) * xDataNum;
          if (xIdTag = 273) or (xIdTag = 324) then
          begin
            // we are reading StripOffsets or TileOffsets
            getmem(ia2, sz);
            wa2 := pwordarray(ia2);
            if sz > 4 then
            begin
              Stream.Position := xDataPos;
              Stream.Read(ia2^, sz);
            end
            else
              CopyMemory(ia2, @DataPos, sz);
            // write data referenced by array
            for q := 0 to xDataNum - 1 do
            begin
              if xDataType = IETIFFTYPE_SHORT then
              begin
                // SHORT
                Stream.Position := IECSwapWord(wa2^[q], not LittleEndian);
                ww := OutStream.Position;
                wa2^[q] := IECSwapWord(ww, not LittleEndian);
              end
              else
              begin
                // LONG
                Stream.Position := IECSwapDWord(ia2^[q], not LittleEndian);
                ia2^[q] := IECSwapDWord(OutStream.Position, not LittleEndian);
              end;
              if lxDataType = IETIFFTYPE_SHORT then
              begin
                sz2 := IECSwapWord(wa1^[q], not LittleEndian);
                if sz2 > 0 then
                  IECopyFrom(OutStream, Stream, sz2); // SHORT
              end
              else
              begin
                sz2 := IECSwapDWord(ia1^[q], not LittleEndian);
                if sz2 > 0 then
                  IECopyFrom(OutStream, Stream, sz2); // LONG
              end;
            end;
            // write array
            if sz > 4 then
            begin
              DataPos := IECSwapDWord(OutStream.Position, not LittleEndian);
              OutStream.Write(ia2^, sz);
            end
            else
              CopyMemory(@DataPos, ia2, sz);
          end
          else
          if xIdTag = 254 then
          begin
            // correct NewSubfileType
            DataPos := IECSwapDWord(xDataPos or 2, not LittleEndian); // 2 means this is a single page of a multipage image
          end
          else
          if (sz > 4) then
          begin
            // DataPos now points to an area of the file (it can be ASCII, too)
            DataPos := IECSwapDWord(OutStream.Position, not LittleEndian);
            Stream.Position := xDataPos;
            if sz > 0 then
              IECopyFrom(OutStream, Stream, sz);
          end;
        end;
      freemem(ia2);
      freemem(ia1);
      // write IFD
      lp1 := OutStream.Position; // save IFD position
      ww := IECSwapWord(nt, not LittleEndian);
      OutStream.Write(ww, 2); // write tags count
      OutStream.Write(IFD^, nt * sizeof(TTIFFTAG));
      lp2 := OutStream.Position; // save position of next reading
      // write IFD position
      OutStream.Position := wp;
      ii := IECSwapDWord(lp1, not LittleEndian);
      OutStream.Write(ii, 4);
      OutStream.Position := lp2; // point to next byte to write
      wp := lp2;
      PosIFD := 0;
      OutStream.Write(PosIFD, 4); // write blank space for IFD position
      // free tags
      freemem(IFD);
      inc(numi);
      // point to the next byte to read
      Stream.Position := lp3;
    end;
  until false;
  if not Internal then
  begin
    // write final IFD position (0)
    OutStream.Position := wp;
    PosIFD := 0;
    OutStream.Write(PosIFD, 4);
  end;
  result := wp;
end;


procedure TIFFInsertImStream(Stream: TStream; ToInsert: TStream; idx: integer; OutStream: TStream);
begin
  TIFFInsertImStream(Stream, ToInsert, idx, OutStream, false);
end;

// find DNG or TIFF-EP raw encoded image
function IsDNGStream(fs: TStream): boolean;
var
  lp: int64;
  IFD: TIETIFFIFDReader;
  TIFFHeader: TTIFFHeader;
  numi: integer;
  i: integer;
begin
  lp := fs.Position;
  result := false;
  IFD := TIETIFFIFDReader.Create();
  try
    IFD.Stream     := fs;
    IFD.StreamBase := 0;
    fs.read(TIFFHeader, sizeof(TTIFFHeader));
    if (TIFFHeader.Id <> $4949) and (TIFFHeader.id <> $4D4D) then
      exit;
    IFD.LittleEndian := TIFFHeader.Id = $4949;
    if not IFD.LittleEndian then
      TIFFHeader.PosIFD := IESwapDWord(TIFFHeader.PosIFD); // converts to LittleEndian header
    IFD.IFD := nil;
    numi := 0;
    if not IFD.ReadIFD(0, TIFFHeader.PosIFD, numi) then
      exit;
    // check for DNGVersion tag
    i := IFD.FindTAG(IETIFFTAG_DNGVERSION);
    if (i > -1) and (IFD.IFD[i].DataType = IETIFFTYPE_BYTE) and (IFD.IFD[i].DataNum = IETIFFTYPE_LONG) then
    begin
      result := true;
      exit;
    end;
    // check for photometricInterpretation=32803 or 34892
    i := IFD.FindTAG(262);
    if (i > -1) and (IFD.IFD[i].DataType = IETIFFTYPE_SHORT) and (IFD.IFD[i].DataNum = 1) and ((IFD.ReadInteger(262, 0, 0) = 32803) or (IFD.ReadInteger(262, 0, 0) = 34892)) then
    begin
      result := true;
      exit;
    end;
    //
    if (IFD.FindTAG($014A) > -1) and (IFD.FindTAG($9216) > -1) then
    begin
      // Has SubIFD and TIFF/EPStandardID, now check if it is a thumbnail
      if (IFD.ReadInteger(256, 0, 0) < 200) and (IFD.ReadInteger(257, 0, 0) < 200) then
      begin
        result := true;
        exit;
      end;
    end;
  finally
    IFD.Free();
    fs.Position := lp;
  end;
end;


function IsTIFFStream(fs: TStream): boolean;
var
  IFD: TIETIFFIFDReader;
  lp: int64;
  numi: integer;
  BufStream: TIEBufferedReadStream;
  LittleEndian: boolean;
  BigTIFF: boolean;
  DataPosSize: integer;
  IFDPosition: int64;
begin
  result := false;
  lp := fs.Position;
  BufStream := TIEBufferedReadStream.Create(fs, 1024, IEGlobalSettings().UseRelativeStreams);
  IFD := TIETIFFIFDReader.Create();
  try
    if BufStream.Size > 20 then
    begin
      result := TIFFReadHeader(BufStream, nil, LittleEndian, BigTIFF, DataPosSize, IFDPosition);
      if not result then
        exit; // fail

      // check some tags
      IFD.Stream       := BufStream;
      IFD.StreamBase   := 0;
      IFD.LittleEndian := LittleEndian;
      IFD.IsBigTIFF    := BigTIFF;
      IFD.DataPosSize  := DataPosSize;
      numi := 0;
      if (IFD.ReadIFD(0, IFDPosition, numi) = false) // has a valid IFD?
         or (IFD.FindTAG(256) = -1)  // has ImageWidth?
         or (IFD.FindTAG(257) = -1)  // has ImageLength?
      then
        result := false
    end;
  finally
    IFD.Free();
    BufStream.Free();
    fs.Position := lp;
  end;
end;

// Try Microsoft PhotoHD 1.0
function IsHDPStream(fs: TStream): boolean;
var
  HeaderTIFF: TTIFFHeader;
  IFD: TIETIFFIFDReader;
  lp: int64;
  numi: integer;
  BufStream: TIEBufferedReadStream;
begin
  result := false;
  lp := fs.Position;
  BufStream := TIEBufferedReadStream.Create(fs, 1024, IEGlobalSettings().UseRelativeStreams);
  IFD := TIETIFFIFDReader.Create(); 
  try
    if BufStream.Size > sizeof(TTIFFHeader) then
    begin
      BufStream.Read(HeaderTIFF, sizeof(HeaderTIFF));
      BufStream.Position := lp;
      result := HeaderTIFF.Id=$4949;
      if not result then
        exit;
      // check some tags
      BufStream.Position := BufStream.Position-1;
      IFD.Stream := BufStream;
      IFD.StreamBase := 0;
      IFD.LittleEndian := true;
      numi := 0;
      if (IFD.ReadIFD(0, HeaderTIFF.PosIFD, numi) = false) // has a valid IFD?
         or (IFD.FindTAG(48256) = -1)  // has ImageWidth?
         or (IFD.FindTAG(48257) = -1)  // has ImageLength?
         or (IFD.FindTAG(48129) = -1)  // has PixelFormat?
         or (IFD.FindTAG(48320) = -1)  // has ImageOffset?
         or (IFD.FindTAG(48321) = -1)  // has ImageByteCount?
      then
        result := false
    end;
  finally
    IFD.Free();
    BufStream.Free;
    fs.Position := lp;
  end;
end;



// if InputStream=nil, then load from InputFileName
// if OutputStream=nil, then save to OutputFileName
// InputStream=OutputStream allowed
{$ifdef IEINCLUDETIFFHANDLER}
function IEInjectTIFFEXIF(InputStream, OutputStream: TStream; const InputFileName, OutputFileName: WideString; pageIndex: integer; IOParams: TIOParamsVals): boolean;
const
  EXIFTags: array [0..22] of integer = (
          271, 272, 306, 274, 282, 283, 296, 305, 315, 318, 531, 319, 529, 532, IETIFFTAG_COPYRIGHT // TIFF 6 standard
          , $4746, $9C9B, $9C9C, $9C9D, $9C9E, $9C9F                                                 // XP specific
          , IETIFFTAG_EXIFIFD                                                                        // EXIF sub ifd
          , IETIFFTAG_EXIFGPSIFD                                                                     // EXIF-GPS
          );
var
  target: TIETIFFHandler;
  source_tags: TIETIFFHandler;
  tempTiffStream: TIEMemStream;
  i: integer;
  nullpr: TProgressRec;
  fAbort: boolean;
  buffer: pointer;
  bufferLength: integer;
  lp: int64;
begin
  result := true;

  fAbort := false;
  with nullpr do
  begin
    Aborting := @fAbort;
    fOnProgress := nil;
    Sender := nil;
  end;

  tempTiffStream := nil;
  source_tags := nil;
  target := nil;
  buffer := nil;
  lp := 0;

  try
    SaveEXIFToStandardBuffer(IOParams, buffer, bufferLength, false);
    tempTiffStream := TIEMemStream.Create(buffer, bufferLength);
    tempTiffStream.Position := 0;
    source_tags := TIETIFFHandler.Create(tempTiffStream);

    if InputStream=nil then
      target := TIETIFFHandler.Create(InputFileName)
    else
    begin
      lp := InputStream.Position; // save input stream position, to allow InputStream=OutputStream
      target := TIETIFFHandler.Create(InputStream);
    end;

    for i := 0 to High(EXIFTags) do
      target.CopyTag(0, source_tags.FindTag(0, EXIFTags[i]), source_tags, pageIndex);

    if OutputStream=nil then
      target.WriteFile(OutputFileName)
    else
    begin
      if InputStream=OutputStream then
        OutputStream.Position := lp;
      target.WriteStream(OutputStream);
    end;

  finally
    if buffer<>nil then
      freemem(buffer);
    source_tags.Free;
    target.Free;
    tempTiffStream.Free;
  end;
end;
{$endif}



end.
