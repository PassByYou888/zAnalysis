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

unit pcxfilter;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, Graphics, classes, sysutils, ImageEnProc, ImageEnIO, hyiedefs, hyieutils;

type
  TPcxHeader = packed record
    Manufacturer: Byte;
    Version: Byte;
    Encoding: Byte;
    Bits_Per_Pixel: Byte;
    Xmin, Ymin: Word;
    Xmax, Ymax: Word;
    Hres, Vres: Word;
    Palette: array[0..15] of TRGB;
    Reserved: Byte;
    Colour_Planes: Byte;
    Bytes_Per_Line: Word;
    Palette_Type: Word; // 0=256colors/true_color
    Filler: array[0..57] of Byte;
  end;

procedure ReadPcxStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; fsDim: integer; var Progress: TProgressRec; Preview: boolean);
procedure WritePcxStream(fs: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);

procedure IEDCXReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
procedure IEDCXInsertStream(fs: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
procedure IEDCXDeleteStream(fs: TStream; idx: integer);
function IEDCXCountStream(fs: TStream): integer;
function IEDCXTryStream(fs: TStream): boolean;

implementation

uses ImageEnView, NeurQuant, ieview, iesettings;

{$R-}


// fsDim = size of pcx block (necessary because Palette is stored at the end of image)
procedure ReadPcxStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; fsDim: integer; var Progress: TProgressRec; Preview: boolean);
var
  imbuf: array of byte; // encoded image
  impos: integer; // reading position
  ph: TPcxHeader;
  cols, rows, col, row: integer;
  rowbuf: array of byte;
  buf2: array of byte;
  pal256: array[0..255] of TRGB;
  ppx, ppx2: pRGB;
  qq, ww: integer;
  lp1: int64;     // start stream position
  lr: integer;    // remaining repetitions previous row
  lbb: byte;      // value of remaining repetitions of previous row
  per, lper: integer;
  // fill rowbuf with previous row
  procedure GetLine;
  var
    q, rp: integer;
    bb: byte;
  begin
    case ph.Encoding of
      0: // no compression
        begin
          CopyMemory(@rowbuf[0], @imbuf[impos], ph.Bytes_Per_Line);
          inc(impos, ph.Bytes_per_Line);
        end;
      1: // RLE compression
        begin
          q := 0;
          while q < ph.Bytes_Per_Line do
          begin
            if lr > 0 then
            begin
              // repetitions previsous row
              rp := lr;
              if (q + rp) >= (ph.Bytes_per_line) then
              begin
                lr := q + rp - ph.bytes_per_line;
                rp := rp - lr;
              end
              else
                lr := 0;
              fillmemory(@rowbuf[q], rp, lbb);
              inc(q, rp);
            end
            else
            begin
              bb := imbuf[impos];
              inc(impos);
              if (bb and $C0) = $C0 then
              begin
                // encoded sequence
                rp := bb and $3F; // repetitions
                bb := imbuf[impos];  // value to repeat
                inc(impos);
                if (q + rp) >= (ph.Bytes_per_line) then
                begin
                  lr := q + rp - ph.bytes_per_line;
                  rp := rp - lr;
                  lbb := bb;
                end;
                fillmemory(@rowbuf[q], rp, bb);
                inc(q, rp);
              end
              else
              begin
                rowbuf[q] := bb;
                inc(q);
              end;
            end;
          end;
        end;
    end;
  end;
begin
  lper := -1;
  lp1 := fs.Position;
  // read TPcxHeader header
  fs.read(ph, sizeof(TPcxHeader));
  if (ph.Manufacturer <> $0A) or
    (ph.bits_per_pixel > 8) or
    ((ph.encoding <> 0) and (ph.encoding <> 1)) then
  begin
    // not supported
    Progress.Aborting^ := True;
    exit;
  end;
  cols := ph.Xmax - ph.Xmin + 1;
  rows := ph.Ymax - ph.Ymin + 1;

  IOParams.ImageCount := 1;
  IOParams.DpiX := ph.Hres;
  IOParams.DpiY := ph.Vres;
  if (IOParams.DpiX = 0) or (IOParams.DpiX = 0) then
  begin
    IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
    IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
  end;

  try
    // read color table
    if (ph.Colour_Planes = 1) and (ph.bits_per_pixel > 4) then
    begin
      fs.Position := lp1 + fsDim - 256 * sizeof(TRGB);
      fs.Read(pal256[0], 256 * sizeof(TRGB));
      fs.Position := lp1 + sizeof(TPcxHeader);
    end;
    // swap R and B in both colormaps
    for qq := 0 to 15 do
      bswap(ph.palette[qq].r, ph.palette[qq].b);
    for qq := 0 to 255 do
      bswap(pal256[qq].r, pal256[qq].b);
    SetLength(imbuf, fsDim - sizeof(TPcxHeader) + 8);
    fs.Read(imbuf[0], fsDim - sizeof(TPcxHeader)); // re-read all (also palette)
    impos := 0;
    // set IOParams
    with IOParams do
    begin
      BitsPerSample := ph.Bits_Per_Pixel;
      SamplesPerPixel := ph.Colour_Planes;
      Width  := cols;
      Height := rows;
      OriginalWidth  := cols;
      OriginalHeight := rows;
      PCX_Version := ph.Version;
      if ColorMap <> nil then
      begin
        freemem(ColorMap);
        fColorMap := nil;
        fColorMapCount := 0;
      end;
      if ph.bits_per_pixel = 8 then
      begin
        fColorMapCount := 256;
        getmem(fColorMap, 256 * sizeof(TRGB));
        CopyMemory(ColorMap, @pal256, 256 * sizeof(TRGB));
      end;
      if (ph.bits_per_pixel = 1) and (ph.Colour_Planes = 4) then
      begin
        fColorMapCount := 16;
        getmem(fColorMap, ColorMapCount * sizeof(TRGB));
        CopyMemory(ColorMap, @ph.palette, 16 * sizeof(TRGB));
        BitsPerSample := 4;
        SamplesPerPixel := 1;
      end
      else
      if ph.bits_per_pixel < 8 then
      begin
        fColorMapCount := (1 shl ph.Bits_Per_Pixel);
        getmem(fColorMap, ColorMapCount * sizeof(TRGB));
        CopyMemory(ColorMap, @ph.palette, (1 shl ph.Bits_Per_Pixel) * sizeof(TRGB));
      end;
    end;
    if not Preview then
    begin
      // read image
      if (ph.Bits_Per_Pixel = 1) and (ph.Colour_Planes = 1) then
        Bitmap.Allocate(cols, rows, ie1g)
      else
        Bitmap.Allocate(cols, rows, ie24RGB);
      SetLength(rowbuf, ph.Bytes_Per_Line * ph.Colour_Planes * 2 + 32); // double size to workaround an old bug
      Progress.per1 := 100 / rows;
      lr := 0;
      for row := 0 to rows - 1 do
      begin
        // OnProgress
        with Progress do
          if assigned(fOnProgress) then
          begin
            per := trunc(per1 * row);
            if per<>lper then
            begin
              lper := per;
              fOnProgress(Sender, per);
            end;
          end;
        if Progress.Aborting^ then
          break;

        ppx := Bitmap.ScanLine[row];
        case ph.Colour_Planes of
          1: // palette
            begin
              if ph.bits_per_pixel = 8 then
              begin
                // 256 colors
                GetLine;
                for col := 0 to cols - 1 do
                begin
                  ppx^ := pal256[rowbuf[col]];
                  inc(ppx);
                end;
              end
              else
              if ph.bits_per_pixel = 4 then
              begin
                // 16 colors (4bit)
                GetLine;
                ww := (cols shr 1);
                if (cols and 1) <> 0 then
                  inc(ww);
                for col := 0 to ww - 1 do
                begin
                  qq := rowbuf[col] shr 4;
                  ppx^ := ph.Palette[qq];
                  inc(ppx);
                  qq := rowbuf[col] and $0F;
                  ppx^ := ph.Palette[qq];
                  inc(ppx);
                end;
              end
              else
              if ph.bits_per_pixel = 1 then
              begin
                // 2 colors (1 bit)
                GetLine;
                ww := cols shr 3;
                if (cols and 7) <> 0 then
                  inc(ww);
                CopyMemory(pbyte(ppx), @rowbuf[0], ww);
              end;
            end;
          3: // true color
            begin
              GetLine;
              ppx2 := ppx;
              for col := 0 to cols - 1 do
              begin
                ppx2^.r := rowbuf[col];
                inc(ppx2);
              end;
              GetLine;
              ppx2 := ppx;
              for col := 0 to cols - 1 do
              begin
                ppx2^.g := rowbuf[col];
                inc(ppx2);
              end;
              GetLine;
              ppx2 := ppx;
              for col := 0 to cols - 1 do
              begin
                ppx2^.b := rowbuf[col];
                inc(ppx2);
              end;
            end;
          4: // four planes per pixel
            begin
              SetLength(buf2, cols);
              ZeroMemory(@buf2[0], cols);
              // bit 0
              GetLine;
              for col := 0 to cols - 1 do
                if _GetPixelbw(@rowbuf[0], col) <> 0 then
                  buf2[col] := 1;
              // bit 1
              GetLine;
              for col := 0 to cols - 1 do
                if _GetPixelbw(@rowbuf[0], col) <> 0 then
                  buf2[col] := buf2[col] or 2;
              // bit 2
              GetLine;
              for col := 0 to cols - 1 do
                if _GetPixelbw(@rowbuf[0], col) <> 0 then
                  buf2[col] := buf2[col] or 4;
              // bit 3
              GetLine;
              for col := 0 to cols - 1 do
              begin
                if _GetPixelbw(@rowbuf[0], col) <> 0 then
                  buf2[col] := buf2[col] or 8;
                ppx^ := ph.Palette[buf2[col]];
                inc(ppx);
              end;
            end;
        end;
      end;
    end; // endif not preview
  except
    Progress.Aborting^ := true;
  end;
end;


// rowbuf: output buffer
// pbx: input buffer (row to encode)
// Width: row byte length (not rounded to a even value)
// c: offset when reading pbx (channel)
// m: multipling offset reading pbx (3 for 24bit)
// return: read byte
function CompressRLE(rowbuf: pbytearray; pbx: pbytearray; Width: integer; c, m: integer): integer;
var
  rb, col: integer;
  bb, rp: byte;
begin
  col := 0;
  rb := 0;
  while col < width do
  begin
    bb := pbx^[col * m + c];
    rp := 1;
    while ((rp + col) < width) and (pbx^[(col + rp) * m + c] = bb) and (rp < 63) do
      inc(rp);
    if (rp > 1) or (bb > 191) then
    begin
      // encoded sequence (rp=repetitions)
      rowbuf[rb] := $C0 or rp;
      inc(rb);
      rowbuf[rb] := bb;
    end
    else
    begin
      rowbuf[rb] := bb;
    end;
    inc(rb);
    inc(col, rp);
  end;
  if (width and 1) <> 0 then
  begin
    rowbuf[rb] := 0;
    inc(rb);
  end;
  result := rb;
end;


procedure WritePcxStream(fs: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  ph: TPcxHeader;
  qt: TIEQuantizer;
  xBitsPixel: integer;
  FreeW: boolean; // if true we have to free WBitmap
  BackCol, ForeCol: TRGB;
  NCol: integer;
  WBitmap: TIEBitmap;
  Palette256: array[0..255] of TRGB;
  pbx: pbytearray;
  row, bcol, col: integer;
  rowbuf: pbytearray;
  rowbuf2: pbytearray;
  rb, c: integer;
  nullpr: TProgressRec;
  bitmapwidth1, bitmapheight1: integer;
  bb: byte;
begin
  with nullpr do
  begin
    Aborting := Progress.Aborting;
    fOnProgress := nil;
    Sender := nil;
  end;
  FreeW := false;
  if IOParams.SamplesPerPixel > 3 then
    IOParams.SamplesPerPixel := 3;  // 3.0.3
  if (IOParams.BitsPerSample = 1) then
  begin
    // save to black/white
    xBitsPixel := 1;
    if Bitmap.pixelformat = ie1g then
      WBitmap := Bitmap
    else
    begin
      // Convert to 1 bit
      WBitmap := _ConvertTo1bitEx(Bitmap, BackCol, ForeCol);
      if WBitmap = nil then
      begin
        // impossible to convert to 1 bit, converts from color to black/white
        // 3.0.0
        WBitmap := TIEBitmap.Create(Bitmap.Width, Bitmap.Height, ie1g);
        WBitmap.CopyAndConvertFormat(Bitmap);
      end;
      FreeW := true;
    end;
  end
  else
  begin
    // save in true color
    xBitsPixel := IOParams.BitsPerSample * IOParams.SamplesPerPixel;
    if Bitmap.PixelFormat = ie24RGB then
      WBitmap := Bitmap
    else
    begin
      // Convert to 24 bit
      WBitmap := TIEBitmap.Create;
      WBitmap.Assign(Bitmap);
      WBitmap.PixelFormat := ie24RGB;
      FreeW := true;
    end;
  end;
  NCol := 1 shl xBitsPixel;
  qt := nil;
  // prepare header
  zeromemory(@ph, sizeof(TPcxHeader));
  with ph do
  begin
    Manufacturer := $0A;
    Version := IOParams.PCX_Version;
    if IOParams.PCX_Compression = ioPCX_UNCOMPRESSED then
      Encoding := 0   // Uncompressed
    else
      Encoding := 1;  // RLE
    Xmin := 0;
    Ymin := 0;
    Xmax := wbitmap.Width - 1;
    Ymax := wbitmap.Height - 1;
    Hres := IOParams.DpiX;
    Vres := IOparams.DpiY;
    Palette_Type := 1;
    case xBitsPixel of
      1:
        begin
          Bits_Per_Pixel := 1;
          Colour_Planes := 1;
          Bytes_Per_Line := wbitmap.width div 8;
          if (wbitmap.width mod 8) <> 0 then
            inc(Bytes_Per_Line);
          Palette[0] := BackCol;
          Palette[1] := ForeCol;
          _RGB2BGR(Palette, 2);
        end;
      4:
        begin
          Bits_Per_Pixel := 4;
          Colour_Planes := 1;
          Bytes_Per_Line := wbitmap.width div 2;
          if (wbitmap.width mod 2) <> 0 then
            inc(Bytes_Per_Line);
          qt := TIEQuantizer.Create(wBitmap, Palette, NCol);
          _RGB2BGR(Palette, 16);
        end;
      8:
        begin
          Bits_Per_Pixel := 8;
          Colour_Planes := 1;
          Bytes_Per_Line := wbitmap.width;
          qt := TIEQuantizer.Create(wBitmap, Palette256, NCol);
          _RGB2BGR(Palette256, 256);
        end;
      24:
        begin
          Bits_Per_Pixel := 8;
          Colour_Planes := 3;
          Bytes_Per_Line := wbitmap.width;
          Palette_Type := 0;
        end;
    end;
    if (Bytes_Per_Line and 1) <> 0 then
      inc(Bytes_Per_Line);
  end;
  // write header
  SafeStreamWrite(fs, Progress.Aborting^, ph, sizeof(TPcxHeader));
  // compress and write the image
  getmem(rowbuf, wbitmap.width * 3 + 8);  // compressed buffer to write
  getmem(rowbuf2, wbitmap.width + 8);     // to compress buffer (for 8, 4, 1 bitperpixel)
  Progress.per1 := 100 / wBitmap.Height;
  bitmapheight1 := wbitmap.height - 1;
  bitmapwidth1 := wbitmap.width - 1;
  for row := 0 to bitmapheight1 do
  begin
    // OnProgress
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * row));
    if Progress.Aborting^ then
      break;
    //
    pbx := wbitmap.ScanLine[row];
    case xBitsPixel of
      24:
        begin
          for c := 2 downto 0 do
          begin
            if ph.Encoding = 1 then
              rb := CompressRLE(rowbuf, pbx, wbitmap.width, c, 3)
            else
            begin
              for col := 0 to bitmapwidth1 do
                rowbuf^[col] := pbx^[col * 3 + c];
              rb := ph.Bytes_Per_Line;
            end;
            SafeStreamWrite(fs, Progress.Aborting^, rowbuf^, rb);
          end;
        end;
      8:
        begin
          for col := 0 to bitmapWidth1 do
            rowbuf2[col] := qt.RGBIndex[PRGBROW(pbx)^[col]];
          if ph.encoding = 1 then
          begin
            rb := CompressRLE(rowbuf, rowbuf2, wbitmap.width, 0, 1);
            SafeStreamWrite(fs, Progress.Aborting^, rowbuf^, rb);
          end
          else
            SafeStreamWrite(fs, Progress.Aborting^, rowbuf2^, ph.Bytes_Per_Line);
        end;
      4:
        begin
          col := 0;
          bcol := 0;
          while col < wbitmap.width do
          begin
            rowbuf2[bcol] := qt.RGBIndex[PRGBROW(pbx)^[col]] shl 4;
            inc(col);
            if col = wbitmap.width then
            begin
              inc(bcol);
              break;
            end;
            rowbuf2[bcol] := rowbuf2[bcol] or qt.RGBIndex[PRGBROW(pbx)^[col]];
            inc(col);
            inc(bcol);
          end; // at the end bcol is the number of bytes used by the row
          if ph.encoding = 1 then
          begin
            rb := CompressRLE(rowbuf, rowbuf2, bcol, 0, 1);
            SafeStreamWrite(fs, Progress.Aborting^, rowbuf^, rb);
          end
          else
            SafeStreamWrite(fs, Progress.Aborting^, rowbuf2^, ph.Bytes_Per_Line);
        end;
      1:
        begin
          col := wbitmap.width shr 3;
          if (wbitmap.width mod 8) <> 0 then
            inc(col);
          if ph.encoding = 1 then
          begin
            rb := CompressRLE(rowbuf, pbx, col, 0, 1);
            SafeStreamWrite(fs, Progress.Aborting^, rowbuf^, rb);
          end
          else
            SafeStreamWrite(fs, Progress.Aborting^, pbx^, ph.Bytes_Per_Line);
        end;
    end;
  end;
  //
  freemem(rowbuf);
  freemem(rowbuf2);
  //
  if (xBitsPixel = 8) and not Progress.Aborting^ then
  begin
    bb := 12;
    fs.Write(bb, 1);
    // write colormap 256
    SafeStreamWrite(fs, Progress.Aborting^, palette256, 3 * 256);
  end;
  //
  if FreeW then
    FreeAndNil(WBitmap);
  if assigned(qt) then
    FreeAndNil(qt);
end;

function IEDCXReadOffsets(fs: TStream; var count: integer): pdwordarray;
begin
  getmem(result, 1024*sizeof(integer));
  count := 0;
  while count<1024 do
  begin
    fs.Read(result[count], sizeof(integer));
    if result[count]=0 then
      break;
    inc(count);
  end;
end;

function IEDCXTryStream(fs: TStream): boolean;
var
  l: int64;
  v, i: integer;
  table: pdwordarray;
  tablec: integer;
  fssize: int64;
begin
  l := fs.Position;
  fs.Read(v, sizeof(integer));

  // check magic number
  result := (v = 987654321);

  // check offsets
  if result then
  begin
    fssize := fs.Size;
    table := IEDCXReadOffsets(fs, tablec);
    for i := 0 to tablec-1 do
      if (table[i]>=fssize) then
      begin
        result := false;
        break;
      end;
    freemem(table);
  end;

  fs.Position := l;
end;

function IEDCXCountStream(fs: TStream): integer;
var
  table: pdwordarray;
  l: int64;
  v: integer;
begin
  l := fs.Position;
  fs.Read(v, sizeof(integer)); // magic number
  table := IEDCXReadOffsets(fs, result);
  freemem(table);
  fs.Position := l;
end;

function IEDCXGetPCXSizes(fs: TStream; table: pdwordarray; tablecount: integer): pintegerarray;
var
  i, j: integer;
  m: dword;
begin
  // extract block sizes
  getmem(result, tablecount*sizeof(integer));
  for i := 0 to tablecount-1 do
  begin
    m := dword(fs.Size)-table[i];
    for j := 0 to tablecount-1 do
      if (i<>j) and (table[j]>table[i]) and ((table[j]-table[i])<m) then
        m := table[j]-table[i];
    result[i] := m;
  end;
end;

procedure IEDCXReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
var
  table: pdwordarray;
  tablecount: integer;
  sizes: pintegerarray;
  v: integer;
begin
  if not IEDCXTryStream(fs) then
  begin
    Progress.Aborting^ := true;
    exit;
  end;

  // read magic number (already verified by IEDCXTryStream)
  fs.Read(v, sizeof(integer));

  // read header
  table := IEDCXReadOffsets(fs, tablecount);

  IOParams.DCX_ImageIndex := ilimit(IOParams.DCX_ImageIndex, 0, tablecount-1);
  fs.Position := table[IOParams.DCX_ImageIndex];

  sizes := IEDCXGetPCXSizes(fs, table, tablecount);

  ReadPcxStream(fs, Bitmap, IOParams, sizes[IOParams.DCX_ImageIndex], Progress, Preview);

  IOParams.ImageCount := TableCount;

  freemem(table);
  freemem(sizes);
end;

procedure IEDCXInsertStream(fs: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  table: pdwordarray;
  tablecount: integer;
  sizes: pintegerarray;
  i: integer;
  tempstream: TMemoryStream;
  newtable: pdwordarray;
  ii: integer;
begin

  if fs.Size>0 then
  begin

    if not IEDCXTryStream(fs) then
    begin
      Progress.Aborting^ := true;
      exit;
    end;

    // read magic number (already verified by IEDCXTryStream)
    fs.Read(i, sizeof(integer));
    // read header
    table := IEDCXReadOffsets(fs, tablecount);

  end
  else
  begin
    tablecount := 0;
    table := nil;
  end;

  sizes := IEDCXGetPCXSizes(fs, table, tablecount);

  tempstream := TMemoryStream.Create;

  // write magic number
  i := 987654321; tempstream.Write(i, sizeof(integer));

  newtable := allocmem(sizeof(dword)*(tablecount+2));

  try

    // write not valid table
    tempstream.Write(newtable^, sizeof(dword)*(tablecount+2));

    i := 0;
    ii := 0;
    repeat

      if IOParams.DCX_ImageIndex=i then
      begin
        newtable[ii] := tempstream.position;
        inc(ii);
        WritePcxStream(tempstream, bitmap, IOParams, Progress);
      end;

      if i < tablecount then
      begin
        newtable[ii] := tempstream.position;
        inc(ii);
        fs.Position := table[i];
        if sizes[i] > 0 then
          IECopyFrom(tempstream, fs, sizes[i]);
      end
      else
        break;

      inc(i);

    until false;

    // write valid table
    tempstream.Position := 4;
    tempstream.Write(newtable^, sizeof(dword)*(tablecount+2));

    fs.Size := 0;
    IECopyFrom(fs, tempstream, 0);

  finally
    FreeAndNil(tempstream);
    freemem(newtable);
    freemem(sizes);
    freemem(table);
  end;
end;

procedure IEDCXDeleteStream(fs: TStream; idx: integer);
var
  table: pdwordarray;
  tablecount: integer;
  sizes: pintegerarray;
  i: integer;
  tempstream: TMemoryStream;
  newtable: pdwordarray;
  ii: integer;
begin

  if not IEDCXTryStream(fs) then
    exit;

  // read magic number (already verified by IEDCXTryStream)
  fs.Read(i, sizeof(integer));
  // read header
  table := IEDCXReadOffsets(fs, tablecount);

  idx := ilimit(idx, 0, tablecount-1);

  sizes := IEDCXGetPCXSizes(fs, table, tablecount);

  tempstream := TMemoryStream.Create;

  // write magic number
  i := 987654321; tempstream.Write(i, sizeof(integer));

  newtable := allocmem(sizeof(dword)*(tablecount)); // includes terminator (zero)

  try

    // write not valid table
    tempstream.Write(newtable^, sizeof(dword)*(tablecount));

    i := 0;
    ii := 0;

    for i := 0 to tablecount-1 do
      if idx<>i then
      begin
        newtable[ii] := tempstream.position;
        inc(ii);
        fs.Position := table[i];
        if sizes[i] > 0 then
          IECopyFrom(tempstream, fs, sizes[i]);
      end;

    // write valid table
    tempstream.Position := 4;
    tempstream.Write(newtable^, sizeof(dword)*(tablecount));

    fs.Size := 0;
    IECopyFrom(fs, tempstream, 0);
  finally
    FreeAndNil(tempstream);
    freemem(newtable);
    freemem(sizes);
    freemem(table);
  end;
end;


end.
