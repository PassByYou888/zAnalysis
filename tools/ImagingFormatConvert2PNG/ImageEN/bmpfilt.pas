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
File version 1010
*)



unit bmpfilt;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, Graphics, classes, sysutils, ImageEnProc, ImageEnIO, hyiedefs, hyieutils;

// BMP
procedure BMPReadStream(fs: TStream; Bitmap: TIEBitmap; BlockDim: integer; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; MissingFileHead: boolean; var AlphaChannel: TIEMask; IgnoreAlpha: boolean);
procedure BMPWriteStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Save32BitAsAlpha: boolean);

// Real RAW
procedure IERealRAWReadStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
procedure IERealRAWWriteStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);

// ICO
function ICOTryStream(fs: TStream): boolean;
procedure ICOReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; Preview: boolean; var Progress: TProgressRec; var AlphaChannel: TIEMask; IgnoreAlpha: boolean; bExceptionOnInvalidSize : Boolean = False);
procedure ICOWriteStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; sizes: array of TSize; bitcounts: array of integer);
procedure ICOWriteStream2(fs: TStream; ielist: array of TObject; var Progress: TProgressRec);
function _EnumICOImStream(fs: TStream): integer;

// ICO helpers
function IESaveIconToStream(Stream: TStream; icon: HICON): boolean;
function IEGetFileIcon(const filename: string): HICON; overload;
procedure IEGetFileIcon(const filename: string; DestBitmap: TIEBitmap); overload;
procedure IEConvertIconToBitmap(icon: HICON; DestBitmap: TIEBitmap; bExceptionOnInvalidSize : Boolean = False);


// CUR
function CURTryStream(fs: TStream): boolean;
procedure CURReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; Preview: boolean; var Progress: TProgressRec; var AlphaChannel: TIEMask; IgnoreAlpha: boolean);

// PXM
procedure PXMReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
procedure PXMWriteStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
function TryPXM(fs: TStream): boolean;

// WBMP
procedure WBMPReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
procedure WBMPWriteStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);

// PostScript (PS)
function IEPostScriptCreate(fs: TStream; var IOParams: TIOParamsVals): pointer;
procedure IEPostScriptClose(handle: pointer; fs: TStream);
procedure IEPostScriptSave(handle: pointer; fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
procedure IEPostScriptSaveOneStep(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);

// PDF
{$ifdef IEINCLUDEPDFWRITING}
function IEPDFCreate(var IOParams: TIOParamsVals): pointer;
procedure IEPDFSave(handle: pointer; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
procedure IEPDFClose(handle: pointer; fs: TStream; var IOParams: TIOParamsVals);
procedure IEPDFSaveOneStep(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
{$endif}

// others
function IEWMFTryStream(Stream: TStream): boolean;
function IEEMFTryStream(Stream: TStream): boolean;

const

  IEBI_RGB  = $32424752;  // 1, 4, 8, 16, 24, 32  Alias for BI_RGB
  IEBI_RLE8 = $38454C52;  // 8  Alias for BI_RLE8
  IEBI_RLE  = $34454C52;  // 4  Alias for BI_RLE4
  IEBI_RAW  = $32776173;  // "raw, uncompressed RGB bitmaps"
  IEBI_RGBA = $41424752;  // 16, 32  Raw RGB with alpha. Sample precision and packing is arbitrary and determined using bit masks for each component, as for BI_BITFIELDS.
  IEBI_RGBT = $54424752;  // 16, 32  Raw RGB with a transparency field. Layout is as for BI_RGB at 16 and 32 bits per pixel but the msb in each pixel indicates whether the pixel is transparent or not.

implementation

uses neurquant, imageenview, ieview, tifccitt, jpegfilt, pngfilt, shellapi, iesettings;

{$R-}


type
  // Bitmap infoheader + OS2 2.x extensions
  TBITMAPINFOHEADER2 = packed record
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
    // os2 part
    biUnits: word;
    biReserved: word;
    biRecording: word;
    biRendering: word;
    biSize1: dword;
    biSize2: dword;
    biColorencoding: dword;
    biIdentifier: dword;
  end;

procedure DecompRLE4_to24(Bitmap: TIEBitmap; bits2: pbyte; xImageDim: integer; ColorMap: PRGBROW; var Progress: TProgressRec; inverter: integer);
var
  y, q, w, xx: integer;
  ww: integer;
  px: PRGB;
  procedure WriteLo;
  begin
    if ww < Bitmap.Width then
    begin
      px^ := ColorMap^[bits2^ shr 4];
      inc(px);
      inc(ww);
    end;
  end;
  procedure WriteHi;
  begin
    if ww < Bitmap.Width then
    begin
      px^ := ColorMap^[bits2^ and $0F];
      inc(px);
      inc(ww);
    end;
  end;
begin
  y := Bitmap.height - 1;
  px := Bitmap.scanline[abs(inverter - y)];
  q := 0;
  ww := 0;
  while q < xImageDim do
  begin
    if bits2^ = 0 then
    begin
      // escape
      inc(bits2);
      inc(q);
      case bits2^ of
        0:
          begin
            // eol
            dec(y);
            if y < 0 then
              break;
            w := imin(imax(0, abs(inverter - y)), Bitmap.Height - 1);
            px := Bitmap.scanline[w];
            ww := 0;
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * (Bitmap.height - y)));
            if Progress.Aborting^ then
              break;
          end;
        1: break; // eof
        2:
          begin
            // delta
            inc(bits2);
            inc(q);
            w := bits2^;
            inc(bits2);
            inc(q);
            dec(y, bits2^);
            px := Bitmap.scanline[abs(inverter - y)];
            inc(px, w);
          end;
      else
        begin
          // absolute packet
          xx := bits2^;
          for w := 0 to (xx shr 1) - 1 do
          begin
            inc(bits2);
            inc(q);
            WriteLo;
            WriteHi;
          end;
          if xx and 1 <> 0 then
          begin
            inc(bits2);
            inc(q);
            WriteLo;
          end;
          xx := (xx shr 1) + (xx and 1);
          if xx and 1 <> 0 then
          begin
            inc(bits2);
            inc(q);
          end;
        end;
      end;
      inc(bits2);
      inc(q);
    end
    else
    begin
      // run length
      xx := bits2^;
      inc(bits2);
      inc(q);
      for w := 0 to (xx shr 1) - 1 do
      begin
        WriteLo;
        WriteHi;
      end;
      if xx and 1 <> 0 then
        WriteLo;
      inc(bits2);
      inc(q);
    end;
  end;
end;

procedure DecompRLE4_to8(Bitmap: TIEBitmap; bits2: pbyte; xImageDim: integer; ColorMap: PRGBROW; var Progress: TProgressRec; inverter: integer);
var
  y, q, w, xx: integer;
  px: pbyte;
  ww: integer;
  procedure WriteLo;
  begin
    if ww < Bitmap.Width then
    begin
      px^ := bits2^ shr 4;
      inc(px);
      inc(ww);
    end;
  end;
  procedure WriteHi;
  begin
    if ww < Bitmap.Width then
    begin
      px^ := bits2^ and $0F;
      inc(px);
      inc(ww);
    end;
  end;
begin
  y := Bitmap.height - 1;
  px := Bitmap.scanline[abs(inverter - y)];
  q := 0;
  ww := 0;
  while q < xImageDim do
  begin
    if bits2^ = 0 then
    begin
      // escape
      inc(bits2);
      inc(q);
      case bits2^ of
        0:
          begin
            // eol
            dec(y);
            if y < 0 then
              break;
            w := imin(imax(0, abs(inverter - y)), Bitmap.Height - 1);
            px := Bitmap.scanline[w];
            ww := 0;
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * (Bitmap.height - y)));
            if Progress.Aborting^ then
              break;
          end;
        1: break; // eof
        2:
          begin
            // delta
            inc(bits2);
            w := bits2^;
            inc(q);
            inc(bits2);
            dec(y, bits2^);
            inc(q);
            px := Bitmap.scanline[abs(inverter - y)];
            inc(px, w);
          end;
      else
        begin
          // absolute packet
          xx := bits2^;
          for w := 0 to (xx shr 1) - 1 do
          begin
            inc(bits2);
            inc(q);
            WriteLo;
            WriteHi;
          end;
          if xx and 1 <> 0 then
          begin
            inc(bits2);
            inc(q);
            WriteLo;
          end;
          xx := (xx shr 1) + (xx and 1);
          if xx and 1 <> 0 then
          begin
            inc(bits2);
            inc(q);
          end;
        end;
      end;
      inc(bits2);
      inc(q);
    end
    else
    begin
      // run length
      xx := bits2^;
      inc(bits2);
      inc(q);
      for w := 0 to (xx shr 1) - 1 do
      begin
        WriteLo;
        WriteHi;
      end;
      if xx and 1 <> 0 then
        WriteLo;
      inc(bits2);
      inc(q);
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////

procedure DecompRLE8_to24(Bitmap: TIEBitmap; bits2: pbyte; xImageDim: integer; ColorMap: PRGBROW; var Progress: TProgressRec; inverter: integer);
var
  y, q, w, xx, x: integer;
  px: PRGB;
begin
  y := Bitmap.height - 1; // vertical position (inverted)
  px := Bitmap.scanline[abs(inverter - y)];
  x := 0; // horizontal position
  for q := 0 to xImageDim - 1 do
  begin
    if bits2^ = 0 then
    begin
      // escape
      inc(bits2);
      case bits2^ of
        0:
          begin
            // eol
            dec(y);
            if y < 0 then
              break;
            px := Bitmap.scanline[abs(inverter - y)];
            x := 0;
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * (Bitmap.height - y)));
            if Progress.Aborting^ then
              break;
          end;
        1: break; // eof
        2:
          begin
            // delta
            inc(bits2);
            w := bits2^;
            inc(bits2);
            dec(y, bits2^);
            px := Bitmap.scanline[abs(inverter - y)];
            inc(x, w);
            if x < Bitmap.Width then
              inc(px, w);
          end;
      else
        begin
          // packet
          xx := bits2^ - 1;
          for w := 0 to xx do
          begin
            inc(bits2);
            px^ := ColorMap^[bits2^];
            inc(x);
            if x < Bitmap.Width then
              inc(px);
          end;
          if xx and 1 = 0 then
            inc(bits2);
        end;
      end;
      inc(bits2);
    end
    else
    begin
      // run length
      xx := bits2^ - 1;
      inc(bits2);
      for w := 0 to xx do
      begin
        px^ := ColorMap^[bits2^];
        inc(x);
        if x < Bitmap.Width then
          inc(px);
      end;
      inc(bits2);
    end;
  end;
end;

procedure DecompRLE8_to8(Bitmap: TIEBitmap; bits2: pbyte; xImageDim: integer; ColorMap: PRGBROW; var Progress: TProgressRec; inverter: integer);
var
  y, q, w, xx, x: integer;
  px: pbyte;
begin
  y := Bitmap.height - 1; // vertical position (inverted)
  px := Bitmap.scanline[abs(inverter - y)];
  x := 0; // horizontal position
  for q := 0 to xImageDim - 1 do
  begin
    if bits2^ = 0 then
    begin
      // escape
      inc(bits2);
      case bits2^ of
        0:
          begin
            // eol
            dec(y);
            if y < 0 then
              break;
            px := Bitmap.scanline[abs(inverter - y)];
            x := 0;
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * (Bitmap.height - y)));
            if Progress.Aborting^ then
              break;
          end;
        1: break; // eof
        2:
          begin
            // delta
            inc(bits2);
            w := bits2^;
            inc(bits2);
            dec(y, bits2^);
            px := Bitmap.scanline[abs(inverter - y)];
            inc(x, w);
            if x < Bitmap.Width then
              inc(px, w);
          end;
      else
        begin
          // packet
          xx := bits2^ - 1;
          for w := 0 to xx do
          begin
            inc(bits2);
            px^ := bits2^;
            inc(x);
            if x < Bitmap.Width then
              inc(px);
          end;
          if xx and 1 = 0 then
            inc(bits2);
        end;
      end;
      inc(bits2);
    end
    else
    begin
      // run length
      xx := bits2^ - 1;
      inc(bits2);
      for w := 0 to xx do
      begin
        px^ := bits2^;
        inc(x);
        if x < Bitmap.Width then
          inc(px);
      end;
      inc(bits2);
    end;
  end;
end;

type
  TColorMap4 = array[0..255] of TRGBQUAD;

procedure BMPReadStream(fs: TStream; Bitmap: TIEBitmap; BlockDim: integer; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; MissingFileHead: boolean; var AlphaChannel: TIEMask; IgnoreAlpha: boolean);
var
  FileHead: TBITMAPFILEHEADER;
  InfoHead: ^TBITMAPINFOHEADER2;
  CoreHead: ^TBITMAPCOREHEADER;
  dm: integer; // size of structure next to BITMAPFILEHEADER
  p0: int64;
  xBitCount: integer;
  xCompression: integer;
  xImageDim: integer; // image size (can be 0 for non compressed images)
  ColorMap4: ^TColorMap4;
  BitFields: array[0..2] of dword;
  q, w, x, y: integer;
  lw: integer;
  px: PRGB;
  px_byte, bits, bits2: pbyte;
  px_word: pword;
  gbitcount, rbitcount, bbitcount: integer;
  rshift, gshift, bshift: integer;
  wbits: pword;
  bitmapwidth1, bitmapheight1, inverter: integer;
  pf: TIEPixelFormat;
  allzeroalpha: boolean;
  lper, per: integer;

  function DoProgress: boolean;
  begin
    with Progress do
    begin
      per := trunc(per1 * (BitmapHeight1 - y));
      if assigned(fOnProgress) and (lper<>per) then
        fOnProgress(Sender, per);
      lper := per;
      result := not Aborting^;
    end;
  end;

begin
{.$WARNINGS OFF}
  getmem(ColorMap4, 256 * sizeof(TRGBQUAD));
  InfoHead := AllocMem(sizeof(TBITMAPINFOHEADER2)); // zero init
  CoreHead := AllocMem(sizeof(TBITMAPCOREHEADER));  // zero init
  try
    xImageDim := 0;
    p0 := fs.Position;
    if IOParams.IsResource or MissingFileHead then
    begin
      FileHead.bfType := 19778;
      FileHead.bfSize := fs.Size;
      FileHead.bfReserved1 := 0;
      FileHead.bfReserved2 := 0;
      FileHead.bfOffBits := 0;
    end
    else
      fs.Read(FileHead, sizeof(TBITMAPFILEHEADER));
    if (FileHead.bfSize > 0) and (BlockDim <= 0) then
      BlockDim := FileHead.bfSize;
    if FileHead.bfType <> 19778 then
    begin
      Progress.Aborting^ := true;
      exit;
    end;
    fs.Read(dm, sizeof(dm)); // read size of next header
    fs.Seek(-4, soCurrent);
    // if present remove old colormap
    IOParams.FreeColorMap;
    // read headers
    if dm = sizeof(TBITMAPCOREHEADER) then
    begin
      // read BITMAPCOREHEADER (OS2 v1.x)
      fs.Read(CoreHead^, dm);
      IOParams.BMP_Version := ioBMP_BMOS2V1;
      if InfoHead^.biHeight < 0 then
      begin
        InfoHead^.biHeight := -InfoHead^.biHeight;
        inverter := InfoHead^.biHeight - 1;
      end
      else
        inverter := 0;
      IOParams.Width  := CoreHead^.bcWidth;
      IOParams.Height := CoreHead^.bcHeight;
      IOParams.OriginalWidth  := CoreHead^.bcWidth;
      IOParams.OriginalHeight := CoreHead^.bcHeight;
      xBitCount := CoreHead^.bcBitCount;
      xCompression := BI_RGB;
      IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
      IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
      // read colormap
      if xBitCount <= 8 then
      begin
        w := 1 shl xBitCount;
        IOParams.fColorMapCount := w;
        IOParams.fColorMap := allocmem(w * sizeof(TRGB));
        fs.Read(IOParams.ColorMap^, w * sizeof(TRGB));
      end;
    end
    else
    begin
      // read BITMAPINFOHEADER
      FillChar(InfoHead^, sizeof(TBITMAPINFOHEADER2), 0);
      fs.Read(InfoHead^, imin(sizeof(TBITMAPINFOHEADER2), dm));
      if dm > sizeof(TBITMAPINFOHEADER2) then
        fs.Seek(dm - 40, soCurrent); // bypass extra data
      if dm = 64 then
        IOParams.BMP_Version := ioBMP_BMOS2V2
      else
      if dm = 40 then
        IOParams.BMP_Version := ioBMP_BM3
      else
        IOParams.BMP_Version := ioBMP_BM;
      if InfoHead^.biHeight < 0 then
      begin
        InfoHead^.biHeight := -InfoHead^.biHeight;
        inverter := InfoHead^.biHeight - 1;
      end
      else
        inverter := 0;
      IOParams.Width := InfoHead^.biWidth;
      IOParams.Height := InfoHead^.biHeight;
      IOParams.OriginalWidth := InfoHead^.biWidth;
      IOParams.OriginalHeight := InfoHead^.biHeight;
      xBitCount := InfoHead^.biBitCount;
      xCompression := InfoHead^.biCompression;
      xImageDim := InfoHead^.biSizeImage;
      IOParams.DpiX := round((InfoHead^.biXPelsPerMeter / 100) * CM_per_Inch);
      if IOParams.DpiX = 0 then
        IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
      IOParams.DpiY := round((InfoHead^.biYPelsPerMeter / 100) * CM_per_Inch);
      if IOParams.DpiY = 0 then
        IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
      // read colormap
      if InfoHead^.biClrUsed > 256 then
        InfoHead^.biClrUsed := 0;
      if (InfoHead^.biClrUsed = 0) and (xBitCount <= 8) then
        InfoHead^.biClrUsed := 1 shl xBitCount; // default
      if (InfoHead^.biClrUsed > 0) and (xBitCount <= 8) then
      begin
        fs.Read(ColorMap4^, InfoHead^.biClrUsed * sizeof(TRGBQUAD));
        IOParams.fColorMapCount := InfoHead^.biClrUsed;
        IOParams.fColorMap := allocmem(InfoHead^.biClrUsed * sizeof(TRGB));
        for q := 0 to InfoHead^.biClrUsed - 1 do
        begin
          IOParams.ColorMap[q].r := ColorMap4^[q].rgbRed;
          IOParams.ColorMap[q].g := ColorMap4^[q].rgbGreen;
          IOParams.ColorMap[q].b := ColorMap4^[q].rgbBlue;
        end;
      end;
      // read bitfields
      if xCompression = BI_BITFIELDS then
        fs.Read(BitFields, 12);
    end;
    IOParams.ImageCount := 1;
    if (xCompression = BI_RGB) or (xCompression = BI_BITFIELDS) then
      IOParams.BMP_Compression := ioBMP_UNCOMPRESSED;
    if (xCompression = BI_RLE4) or (xCompression = BI_RLE8) then
      IOParams.BMP_Compression := ioBMP_RLE;
    // read bitmap
    if FileHead.bfOffBits > 0 then
      fs.Position := p0 + FileHead.bfOffBits; // go to bitmap start
    if BlockDim > 0 then
      xImageDim := BlockDim - (fs.position - p0);
    if (IOParams.Width = 0) or (IOParams.Height = 0) then
      Preview := true;
    if not Preview then
    begin
      if xBitCount = 1 then
        pf := ie1g
      else
      if (xBitCount = 4) and IOParams.IsNativePixelFormat then
        pf := ie8p
      else
      if (xBitCount = 8) and IOParams.IsNativePixelFormat then
        pf := ie8p
      else
      if (xBitCount = 16) and IOParams.IsNativePixelFormat then
        pf := ie16g
      else
        pf := ie24RGB;
      if not Bitmap.Allocate(IOParams.Width, IOParams.Height, pf) then
      begin
        Progress.Aborting^ := true;
        exit;
      end;
      Progress.per1 := 100 / Bitmap.Height;
      lw := (((Bitmap.Width * xBitCount) + 31) shr 5) shl 2; // row byte length
    end
    else
      lw := 0; // prevents warnings
    case xBitCount of
      1:
        begin // 1 bit per pixel
          IOParams.BitsPerSample := 1;
          IOParams.SamplesPerPixel := 1;
          if not Preview then
          begin
            if xCompression = BI_RGB then
            begin
              bitmapheight1 := Bitmap.Height - 1;
              for y := BitmapHeight1 downto 0 do
              begin
                if not DoProgress then
                  break;
                fs.read(pbyte(Bitmap.Scanline[abs(inverter - y)])^, lw);
              end;
              if (IOParams.fColorMapCount = 2) and
                equalrgb(IOParams.fColorMap^[0], creatergb(255, 255, 255)) and
                equalrgb(IOParams.fColorMap^[1], creatergb(0, 0, 0)) then
                _Negative1BitEx(Bitmap);
            end;
          end; // endif not preview
        end;
      4:
        begin // 4 bit per pixel
          IOParams.BitsPerSample := 4;
          IOParams.SamplesPerPixel := 1;
          if not Preview then
          begin
            if IOParams.IsNativePixelFormat then
              // set color map
              for y := 0 to IOParams.ColorMapCount - 1 do
                Bitmap.Palette[y] := IOParams.ColorMap[y];
            if xCompression = BI_RGB then
            begin
              getmem(bits, lw + 32);
              bitmapheight1 := Bitmap.Height - 1;
              bitmapwidth1 := Bitmap.Width - 1;
              for y := bitmapheight1 downto 0 do
              begin
                if not DoProgress then
                  break;
                fs.Read(bits^, lw);
                bits2 := bits;
                if IOParams.IsNativePixelFormat then
                begin
                  // native format
                  Bitmap.PaletteUsed := 16;
                  px_byte := Bitmap.Scanline[abs(inverter - y)];
                  for x := 0 to bitmapwidth1 do
                  begin
                    if (x and 1) = 0 then
                      px_byte^ := bits2^ shr 4
                    else
                    begin
                      px_byte^ := bits2^ and $0F;
                      inc(bits2);
                    end;
                    inc(px_byte);
                  end;
                end
                else
                begin
                  // 4 bit to 24 bit
                  px := Bitmap.Scanline[abs(inverter - y)];
                  for x := 0 to bitmapwidth1 do
                  begin
                    if (x and 1) = 0 then
                      px^ := IOParams.ColorMap^[bits2^ shr 4]
                    else
                    begin
                      px^ := IOParams.ColorMap^[bits2^ and $0F];
                      inc(bits2);
                    end;
                    inc(px);
                  end;
                end;
              end;
              freemem(bits);
            end
            else
            if xCompression = BI_RLE4 then
            begin
              // RLE4 compression
              getmem(bits, xImageDim);
              fs.Read(bits^, xImageDim);
              if IOParams.IsNativePixelFormat then
              begin
                // native format
                Bitmap.PaletteUsed := 16;
                DecompRLE4_to8(Bitmap, bits, xImageDim, IOParams.ColorMap, Progress, inverter);
              end
              else
                // 4 bit to 24 bit
                DecompRLE4_to24(Bitmap, bits, xImageDim, IOParams.ColorMap, Progress, inverter);
              freemem(bits);
            end;
          end; // endif not preview
        end;
      8:
        begin // 8 bit per pixel
          IOParams.BitsPerSample := 8;
          IOParams.SamplesPerPixel := 1;
          if not Preview then
          begin
            if IOParams.IsNativePixelFormat then
              // set color map
              for y := 0 to IOParams.ColorMapCount - 1 do
                Bitmap.Palette[y] := IOParams.ColorMap[y];
            if xCompression = BI_RLE8 then
            begin
              // RLE8 compression
              getmem(bits, xImageDim);
              fs.Read(bits^, xImageDim);
              if IOParams.IsNativePixelFormat then
              begin
                // native format
                Bitmap.PaletteUsed := 256;
                DecompRLE8_to8(Bitmap, bits, xImageDim, IOParams.ColorMap, Progress, inverter);
              end
              else
                // 8 bit to 24 bit
                DecompRLE8_to24(Bitmap, bits, xImageDim, IOParams.ColorMap, Progress, inverter);
              freemem(bits);
            end
            else
            if xCompression = BI_RGB then
            begin
              // no compression
              getmem(bits, lw);
              try
                bitmapheight1 := Bitmap.Height - 1;
                bitmapwidth1 := Bitmap.Width - 1;
                for y := bitmapheight1 downto 0 do
                begin
                  fs.Read(bits^, lw);
                  if not DoProgress then
                    break;
                  bits2 := bits;
                  if IOParams.IsNativePixelFormat then
                  begin
                    // native format
                    Bitmap.PaletteUsed := 256;
                    CopyMemory(Bitmap.Scanline[abs(inverter - y)], bits2, bitmap.Width);
                  end
                  else
                  begin
                    // 8 bit to 24 bit
                    px := Bitmap.Scanline[abs(inverter - y)];
                    for x := 0 to bitmapwidth1 do
                    begin
                      px^ := IOParams.ColorMap^[bits2^];
                      inc(bits2);
                      inc(px);
                    end;
                  end;
                end;
              finally
                freemem(bits);
              end;
            end;
          end; // endif not Preview
        end;
      16:
        begin // 16 bit per pixel
          IOParams.BitsPerSample := 5;
          IOParams.SamplesPerPixel := 3;
          if not Preview then
          begin
            getmem(bits, lw); // alloc one row
            try
              if xCompression = BI_RGB then
              begin
                // 5-5-5 pixel format
                BitFields[0] := $7C00;
                BitFields[1] := $03E0;
                BitFields[2] := $001F;
              end; // otherwise it is BI_BITFIELDS , values already loaded in BitFields
              rbitcount := _GetBitCount(BitFields[0]);
              gbitcount := _GetBitCount(BitFields[1]);
              bbitcount := _GetBitCount(BitFields[2]);
              if (rbitcount = 32) or (gbitcount = 32) or (bbitcount = 32) then
              begin
                // handled as 16 bit gray scale
                bitmapheight1 := Bitmap.Height - 1;
                bitmapwidth1 := Bitmap.Width - 1;
                for y := bitmapheight1 downto 0 do
                begin
                  if not DoProgress then
                    break;
                  fs.Read(bits^, lw); // load a row
                  wbits := pword(bits);
                  case Bitmap.PixelFormat of
                    ie16g:
                      begin
                        px_word := Bitmap.Scanline[abs(inverter - y)];
                        for x := 0 to bitmapwidth1 do
                        begin
                          px_word^ := wbits^;
                          inc(px_word);
                          inc(wbits);
                        end;
                      end;
                    ie24RGB:
                      begin
                        px := Bitmap.Scanline[abs(inverter - y)];
                        for x := 0 to bitmapwidth1 do
                        begin
                          px^.r := wbits^ shr 8;
                          px^.g := px^.r;
                          px^.b := px^.r;
                          inc(px);
                          inc(wbits);
                        end;
                      end;
                  end;
                end;
              end
              else
              begin
                rshift := (gbitCount + bbitCount) - (8 - rbitCount);
                gshift := bbitCount - (8 - gbitCount);
                bshift := 8 - bbitCount;
                bitmapheight1 := Bitmap.Height - 1;
                bitmapwidth1 := Bitmap.Width - 1;
                for y := bitmapheight1 downto 0 do
                begin
                  if not DoProgress then
                    break;
                  fs.Read(bits^, lw); // load a row
                  px := Bitmap.Scanline[abs(inverter - y)];
                  wbits := pword(bits);
                  for x := 0 to bitmapwidth1 do
                  begin
                    px^.r := (wbits^ and BitFields[0]) shr rshift;
                    px^.g := (wbits^ and BitFields[1]) shr gshift;
                    px^.b := (wbits^ and BitFields[2]) shl bshift;
                    inc(px);
                    inc(wbits);
                  end;
                end;
              end;
            finally
              freemem(bits);
            end;
          end; // endif not preview
        end;
      24:
        begin // 24 bit per pixel
          IOParams.BitsPerSample := 8;
          IOParams.SamplesPerPixel := 3;
          if not Preview then
          begin
            bitmapheight1 := Bitmap.Height - 1;
            lper := -1;
            for y := BitmapHeight1 downto 0 do
            begin
              if not DoProgress then
                break;
              fs.read(pbyte(Bitmap.Scanline[abs(inverter - y)])^, lw);
            end;
          end; // endif not Preview
        end;
      32:
        begin // 32 bit per pixel
          IOParams.BitsPerSample := 8;
          IOParams.SamplesPerPixel := 4;
          if not Preview then
          begin
            if not IgnoreAlpha then
            begin
              if not assigned(AlphaChannel) then
                AlphaChannel := TIEMask.Create;
              AlphaChannel.AllocateBits(Bitmap.Width, Bitmap.Height, 8);
              AlphaChannel.Fill(255);
            end;
            allzeroalpha := true;
            getmem(bits, lw);
            bitmapheight1 := Bitmap.Height - 1;
            bitmapwidth1 := Bitmap.Width - 1;
            for y := bitmapheight1 downto 0 do
            begin
              if not DoProgress then
                break;
              px := Bitmap.Scanline[abs(inverter - y)];
              fs.read(bits^, lw);
              bits2 := bits;
              for x := 0 to bitmapwidth1 do
              begin
                px^.b := bits2^;
                inc(bits2);
                px^.g := bits2^;
                inc(bits2);
                px^.r := bits2^;
                inc(bits2);
                if not IgnoreAlpha then
                begin
                  AlphaChannel.SetPixel(x, y, bits2^);
                  if allzeroalpha then
                    allzeroalpha := bits2^=0;
                end;
                inc(bits2);
                inc(px);
              end;
            end;
            if not IgnoreAlpha and (AlphaChannel.Full or allzeroalpha) then
              FreeAndNil(AlphaChannel);
            freemem(bits);
          end; // endif not preview
        end;
    end;
  finally
    freemem(ColorMap4);
    freemem(InfoHead);
    freemem(CoreHead);
  end;
{.$WARNINGS ON}
end;

// compress a row to RLE4
// px: uncompressed buffer
// Width: number of bytes of px
// rowbuf: compressed bufer (output)
// rest. size of compressed buffer
// note: each byte of px contains a pixel (nibble)

function CompressRLE4row(px: pbytearray; Width: integer; rowbuf: pbyte): integer;
var
  p1, p2: integer;
  pb: integer;
  q: integer;
  basbuf: pbyte;
  procedure WAbs;
  var
    q, w: integer;
  begin
    while p1 - pb > 0 do
    begin
      q := imin(p1 - pb, 255); // byte count (p1 not included)
      if q = 1 then
      begin
        // only one, code as runlength
        rowbuf^ := 1;
        inc(rowbuf);
        rowbuf^ := px^[pb] shl 4;
        inc(rowbuf);
      end
      else
      if q = 2 then
      begin
        // two, again runlength
        rowbuf^ := 2;
        inc(rowbuf);
        rowbuf^ := (px^[pb] shl 4) or px^[pb + 1];
        inc(rowbuf);
      end
      else
      begin
        // they are 3 o more, code as absolute
        rowbuf^ := 0;
        inc(rowbuf); // ESC
        rowbuf^ := q;
        inc(rowbuf);
        for w := 0 to (q shr 1) - 1 do
        begin
          rowbuf^ := (px^[pb + w * 2] shl 4) or px^[pb + w * 2 + 1];
          inc(rowbuf);
        end;
        if q and 1 <> 0 then
        begin
          rowbuf^ := px^[pb] shl 4;
          inc(rowbuf);
        end;
        w := (q shr 1) + (q and 1);
        if w and 1 <> 0 then
          inc(rowbuf);
      end;
      inc(pb, q);
    end;
  end;
begin
  basbuf := rowbuf;
  p1 := 0;
  pb := 0;
  while p1 < Width do
  begin
    if p1 - pb > 255 then
    begin
      WAbs;
      //pb := 0;
    end;
    if px^[p1] = px^[p1 + 1] then
    begin
      // * found at least 2 equals
        // write previsous bytes as absolute (from pb)
      if pb < p1 then
        WAbs;
      // look for other equal bytes
      p2 := p1 + 2;
      while (p2 < Width) and (px^[p1] = px^[p2]) and (p2 - p1 < 255) do
        inc(p2);
      // write p2-p1 (p2 not included) times same byte
      q := p2 - p1;
      rowbuf^ := q;
      inc(rowbuf);
      rowbuf^ := (px^[p1] shl 4) or px^[p1];
      inc(rowbuf);
      pb := p2;
      p1 := pb;
    end
    else
      inc(p1);
  end;
  if pb < p1 then
    WAbs;
  rowbuf^ := 0;
  inc(rowbuf); // ESC
  rowbuf^ := 0; // EOL
  result := uint64(rowbuf) - uint64(basbuf) + 1;
end;

// returns size of compressed buffer
// px: uncompressed buffer
// Width: number of bytes of px
// rowbuf: compressed bufer (output)
// compress a row to RLE4

function CompressRLE8row(px: pbytearray; Width: integer; rowbuf: pbyte): integer;
var
  p1, p2: integer;
  pb: integer;
  q: integer;
  basbuf: pbyte;
  procedure WAbs;
  var
    q, w: integer;
  begin
    while p1 - pb > 0 do
    begin
      q := imin(p1 - pb, 255); // byte count (p1 not included)
      if q = 1 then
      begin
        // one byte, encoded as runlength
        rowbuf^ := 1;
        inc(rowbuf);
        rowbuf^ := px^[pb];
        inc(rowbuf);
      end
      else
      if q = 2 then
      begin
        // two bytes, again runlength
        rowbuf^ := 1;
        inc(rowbuf);
        rowbuf^ := px^[pb];
        inc(rowbuf);
        rowbuf^ := 1;
        inc(rowbuf);
        rowbuf^ := px^[pb + 1];
        inc(rowbuf);
      end
      else
      begin
        // three or more, encoded as absolute
        rowbuf^ := 0;
        inc(rowbuf); // ESC
        rowbuf^ := q;
        inc(rowbuf);
        for w := 0 to q - 1 do
        begin
          rowbuf^ := px^[pb + w];
          inc(rowbuf);
        end;
        if q and 1 <> 0 then
          inc(rowbuf);
      end;
      inc(pb, q);
    end;
  end;
begin
  basbuf := rowbuf;
  p1 := 0;
  pb := 0;
  while p1 < Width do
  begin
    if p1 - pb > 255 then
    begin
      WAbs;
      //pb := 0;
    end;
    if px^[p1] = px^[p1 + 1] then
    begin
      // * found at least two equals
      // write previous bytes as absolute (from pb)
      if pb < p1 then
        WAbs;
      // look for other equals byte
      p2 := p1 + 2;
      while (p2 < Width) and (px^[p1] = px^[p2]) and (p2 - p1 < 255) do
        inc(p2);
      // write p2-p1 (p2 not included) equal bytes
      q := p2 - p1;
      rowbuf^ := q;
      inc(rowbuf);
      rowbuf^ := px^[p1];
      inc(rowbuf);
      pb := p2;
      p1 := pb;
    end
    else
      inc(p1);
  end;
  if pb < p1 then
    WAbs;
  rowbuf^ := 0;
  inc(rowbuf); // ESC
  rowbuf^ := 0; // EOL
  result := uint64(rowbuf) - uint64(basbuf) + 1;
end;


// note: output bitcount will be only 1, 4, 8, 24, 32
procedure BMPWriteStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Save32BitAsAlpha: boolean);
var
  FileHead: TBITMAPFILEHEADER;
  InfoHead: TBITMAPINFOHEADER2;
  CoreHead: TBITMAPCOREHEADER;
  xBitCount: integer;
  qt: TIEQuantizer;
  zz, q, w, row: integer;
  p0: int64; // begin of file position
  p1: int64; // begin of image position
  p2: int64; // final file position
  px: PRGB;
  px_byte, rowbuf: pbyte;
  buf: pbytearray;
  wo: word;
  lw: integer;
  BWBitmap: TIEBitmap;
  nullpr: TProgressRec;
  bitmapheight1, bitmapwidth1: integer;
  lper, per: integer;

  // write colormap in quad word (nc=number of colors, fs=stream, IOParams)
  procedure WColorMap4(nc: integer);
  var
    q: integer;
    QRGB: TRGBQUAD;
  begin
    for q := 0 to nc - 1 do
    begin
      qrgb.rgbBlue := IOParams.ColorMap^[q].b;
      qrgb.rgbGreen := IOParams.ColorMap^[q].g;
      qrgb.rgbRed := IOParams.ColorMap^[q].r;
      qrgb.rgbReserved := 0;
      fs.write(qrgb, 4);
    end;
  end;

begin
  with nullpr do
  begin
    Aborting := Progress.Aborting;
    fOnProgress := nil;
    Sender := nil;
  end;

  p0 := fs.position;
  p1 := 0;

  with IOParams do
  begin
    if (BitsPerSample = 1) or (BitsPerSample = 4) then
      xBitCount := BitsPerSample
    else
    if SamplesPerPixel = 1 then
      xBitCount := 8
    else
    begin
      if Bitmap.HasAlphaChannel then
        xBitCount := 32
      else
        xBitCount := 24;
    end;
  end;

  // write header
  fillchar(FileHead, sizeof(FileHead), 0);
  FileHead.bfType := 19778;
  SafeStreamWrite(fs, Progress.Aborting^, FileHead, sizeof(FileHead));
  if IOParams.BMP_Version = ioBMP_BMOS2V1 then
  begin
    // OS/2 v1.x
    CoreHead.bcSize := sizeof(TBITMAPCOREHEADER);
    CoreHead.bcWidth := Bitmap.Width;
    CoreHead.bcHeight := Bitmap.Height;
    CoreHead.bcPlanes := 1;
    SafeStreamWrite(fs, Progress.Aborting^, CoreHead, CoreHead.bcSize);
  end
  else
  begin
    // Win3 - OS/2 v2.x
    fillchar(InfoHead, sizeof(InfoHead), 0);
    case IOParams.BMP_Version of
      ioBMP_BM: InfoHead.biSize := 40;
      ioBMP_BM3: InfoHead.biSize := 40;
      ioBMP_BMOS2V2: InfoHead.biSize := 64;
    end;
    InfoHead.biWidth := Bitmap.Width;
    InfoHead.biHeight := Bitmap.Height;
    InfoHead.biPlanes := 1;
    InfoHead.biCompression := BI_RGB; // default UNCOMPRESSED
    if (IOParams.BMP_Compression = ioBMP_RLE) then
    begin
      if xBitCount = 4 then
        InfoHead.biCompression := BI_RLE4;
      if xBitCount = 8 then
        InfoHead.biCompression := BI_RLE8;
    end;
    InfoHead.biXPelsPerMeter := round((IOParams.DpiX / CM_per_Inch) * 100);
    InfoHead.biYPelsPerMeter := round((IOParams.DpiY / CM_per_Inch) * 100);
    if xBitCount <= 8 then
      InfoHead.biClrUsed := 1 shl xBitCount;
    InfoHead.biClrImportant := 1 shl xBitCount;
    // omitted parameters for OS/2 v2 due documentation leak
    SafeStreamWrite(fs, Progress.Aborting^, InfoHead, InfoHead.biSize);
  end;
  //
  Progress.per1 := 100 / Bitmap.Height;
  lw := (((Bitmap.Width * xBitCount) + 31) shr 5) shl 2; // row length in bytes

  // estimate file size, to speed up saving
  //fs.Size := fs.Size+ lw*(Bitmap.Height);

  //
  if (xBitCount = 4) or (xBitCount = 8) then
  begin
    //////////////////////////////////////
    // 4 e 8 bits per pixel with colormap
    BWBitmap := nil;
    qt := nil;
    rowbuf := nil;
    buf := nil;
    try
      if Bitmap.PixelFormat = ie1g then
      begin
        // from 1 bit to 24 bit
        BWBitmap := TIEBitmap.Create;
        BWBitmap.Assign(Bitmap);
        BWBitmap.PixelFormat := ie24RGB;
      end
      else
        BWBitmap := Bitmap;

      // color map
      zz := 1 shl xBitCount;
      IOParams.FreeColorMap;
      getmem(IOParams.fColorMap, zz * 3);
      IOParams.fColorMapCount := zz;
      qt := TIEQuantizer.Create(BWBitmap, IOParams.ColorMap^, zz);
      if IOParams.BMP_Version = ioBMP_BMOS2V1 then
        // write colormap as TRGB (RGBTRIPLE)
        SafeStreamWrite(fs, Progress.Aborting^, IOParams.ColorMap^, zz * 3)
      else
        // write colormap as RGBQUAD
        WColorMap4(zz);
      p1 := fs.Position;

      // write BWBitmap
      getmem(rowbuf, BWBitmap.Width * 10); // compressed buffer (it could be learger than decompressed!)
      getmem(buf, imax(BWBitmap.Width, lw)); // compressed buffer
      bitmapheight1 := BWBitmap.Height - 1;
      bitmapwidth1 := BWBitmap.Width - 1;
      for row := BitmapHeight1 downto 0 do
      begin
        // OnProgress
        with Progress do
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * (BitmapHeight1 - row)));
        if Progress.Aborting^ then
          break;
        //
        if (IOParams.BMP_Compression = ioBMP_RLE) and (IOParams.BMP_Version <> ioBMP_BMOS2V1) then
        begin
          // RLE COMPRESSION
          if (BWBitmap.PixelFormat = ie8p) or (BWBitmap.PixelFormat = ie8g) then
          begin
            // native format
            CopyMemory(buf, BWBitmap.Scanline[row], BWBitmap.Width);
          end
          else
          begin
            // color subsample or other formats
            px := BWBitmap.Scanline[row];
            for q := 0 to BitmapWidth1 do
            begin
              buf[q] := qt.RGBIndex[px^];
              inc(px);
            end;
          end;
          if xBitCount = 4 then
            zz := CompressRLE4row(pointer(buf), BWBitmap.Width, rowbuf) // RLE4
          else
            zz := CompressRLE8row(pointer(buf), BWBitmap.Width, rowbuf); // RLE8
          SafeStreamWrite(fs, Progress.Aborting^, rowbuf^, zz);
        end
        else
        begin
          // NO COMPRESSION
          if xBitCount = 8 then
          begin
            // 8 bit per pixel
            if (BWBitmap.PixelFormat = ie8p) or (BWBitmap.PixelFormat = ie8g) then
            begin
              // native format
              CopyMemory(buf, BWBitmap.Scanline[row], BWBitmap.Width);
            end
            else
            begin
              // color subsample or other formats
              px := BWBitmap.Scanline[row];
              for q := 0 to BitmapWidth1 do
              begin
                buf[q] := qt.RGBIndex[px^];
                inc(px);
              end;
            end;
            SafeStreamWrite(fs, Progress.Aborting^, buf^, lw);
          end
          else
          begin
            // 4 bit per pixel
            if BWBitmap.PixelFormat = ie8p then
            begin
              // native format
              px_byte := BWBitmap.Scanline[row];
              q := 0;
              while q < (BWBitmap.Width shr 1) do
              begin
                buf[q] := px_byte^ shl 4;
                inc(px_byte);
                buf[q] := buf[q] or px_byte^;
                inc(px_byte);
                inc(q);
              end;
              if BWBitmap.width and 1 <> 0 then
                buf[q] := px_byte^ shl 4;
            end
            else
            begin
              // color subsample or other formats
              px := BWBitmap.Scanline[row];
              q := 0;
              while q < (BWBitmap.Width shr 1) do
              begin
                buf[q] := qt.RGBIndex[px^] shl 4;
                inc(px);
                buf[q] := buf[q] or qt.RGBIndex[px^];
                inc(px);
                inc(q);
              end;
              if BWBitmap.width and 1 <> 0 then
                buf[q] := qt.RGBIndex[px^] shl 4;
            end;
            SafeStreamWrite(fs, Progress.Aborting^, buf^, lw);
          end;
        end;
      end;
      if IOParams.BMP_Compression = ioBMP_RLE then
      begin
        // write ESC+EOF
        wo := $0001;
        SafeStreamWrite(fs, Progress.Aborting^, wo, 2);
      end;
    finally
      freemem(buf);
      freemem(rowbuf);
      FreeAndNil(qt);
      if (BWBitmap <> Bitmap) and assigned(BWBitmap) then
        FreeAndNil(BWBitmap);
    end;
  end;
  if xBitCount = 1 then
  begin
    //////////////////////////////////////
    // 1 bit per pixel (with colormap, but it isn't used...)
    IOParams.FreeColorMap;
    IOParams.fColorMapCount := 2;
    getmem(IOParams.fColorMap, 2 * sizeof(TRGB));
    if Bitmap.PixelFormat = ie1g then
    begin
      // the Bitmap is already bilevel
      BWBitmap := Bitmap;
      // note: following values could be taked by Bitmap
      IOParams.ColorMap^[0] := CreateRGB(0, 0, 0);
      IOParams.ColorMap^[1] := CreateRGB(255, 255, 255);
    end
    else
    begin
      // to convert in bilevel
      BWBitmap := _ConvertTo1bitEx(Bitmap, IOParams.ColorMap^[0], IOParams.ColorMap^[1]);
      if BWBitmap = nil then
      begin
        // impossible to convert in 1 bit, convert in ordered dither
        BWBitmap := TIEBitmap.Create;
        BWBitmap.Assign(Bitmap);
        BWBitmap.PixelFormat := ie1g; // user ordered conversion
      end;
    end;
    if BWBitmap <> nil then
    begin
      // write colormap
      if IOParams.BMP_Version = ioBMP_BMOS2V1 then
        SafeStreamWrite(fs, Progress.Aborting^, IOParams.ColorMap^, 2 * sizeof(TRGB))
      else
        WColorMap4(2);
      // write bitmap
      p1 := fs.position;
      bitmapheight1 := bitmap.height - 1;
      lper := -1;
      for q := bitmapheight1 downto 0 do
      begin
        // OnProgress
        with Progress do
          if assigned(fOnProgress) then
          begin
            per := trunc(per1 * (bitmapheight1 - q));
            if lper<>per then
            begin
              fOnProgress(Sender, per);
              lper := per;
            end;
          end;
        if Progress.Aborting^ then
          break;
        //
        px := BWBitmap.Scanline[q];
        SafeStreamWrite(fs, Progress.Aborting^, px^, lw);
      end;
      //
      if Bitmap.PixelFormat <> ie1g then
        FreeAndNil(BWBitmap);
    end;
  end;
  if xBitCount = 24 then
  begin
    // 24 bit per pixel (includes 16 and 24 bitcount)
    if bitmap.pixelformat = ie1g then
    begin
      // convert 1 bit to 24 bit
      BWBitmap := TIEBitmap.Create;
      BWBitmap.Assign(Bitmap);
      BWBitmap.PixelFormat := ie24RGB;
    end
    else
      BWBitmap := Bitmap;
    //
    p1 := fs.position;
    bitmapheight1 := BWBitmap.height - 1;
    for q := bitmapheight1 downto 0 do
    begin
      // OnProgress
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * (bitmapheight1 - q)));
      if Progress.Aborting^ then
        break;

      px := BWBitmap.Scanline[q];
      SafeStreamWrite(fs, Progress.Aborting^, px^, lw);
    end;
    if BWBitmap <> Bitmap then
      FreeAndNil(BWBitmap);
  end;

  if xBitCount = 32 then
  begin
    // 32 bit per pixel (8 bits used by alpha channel)
    BWBitmap := Bitmap;

    getmem(rowbuf, lw);
    p1 := fs.position;
    bitmapheight1 := BWBitmap.height - 1;
    for q := bitmapheight1 downto 0 do
    begin
      // OnProgress
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * (bitmapheight1 - q)));
      if Progress.Aborting^ then
        break;
      //
      px_byte := rowbuf;
      px := BWBitmap.Scanline[q];
      for w := 0 to BWBitmap.width-1 do
      begin
        px_byte^ := px^.b; 
        inc(px_byte);
        px_byte^ := px^.g; 
        inc(px_byte);
        px_byte^ := px^.r; 
        inc(px_byte);
        if Save32BitAsAlpha then
          px_byte^ := BWBitmap.Alpha[w, q]
        else
          px_byte^ := 255;
        inc(px_byte);
        inc(px);
      end;
      SafeStreamWrite(fs, Progress.Aborting^, rowbuf^, lw);
    end;
    freemem(rowbuf);
  end;

  if not Progress.Aborting^ then
  begin
    // write updated headers
    p2 := fs.position;
    FileHead.bfSize := p2 - p0;
    FileHead.bfOffBits := p1 - p0;
    fs.position := p0;
    SafeStreamWrite(fs, Progress.Aborting^, FileHead, sizeof(FileHead));
    if IOParams.BMP_Version = ioBMP_BMOS2V1 then
    begin
      CoreHead.bcBitCount := xBitCount;
      SafeStreamWrite(fs, Progress.Aborting^, CoreHead, CoreHead.bcSize);
    end
    else
    begin
      InfoHead.biSizeImage := p2 - p1;
      InfoHead.biBitCount := xBitCount;
      SafeStreamWrite(fs, Progress.Aborting^, InfoHead, InfoHead.biSize);
    end;
    fs.position := p2; // go to at end of file
  end;

  //fs.Size := fs.Position;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////
// ICO

type

  TICONDIR = packed record
    idReserved: WORD;
    idType: WORD;
    idCount: WORD;
  end;

  TICONDIRENTRY = packed record
    bWidth: BYTE;
    bHeight: BYTE;
    bColorCount: BYTE;
    bReserved: BYTE;
    wPlanes: WORD;
    wBitCount: WORD;
    dwBytesInRes: DWORD;
    dwImageOffset: DWORD;
  end;

// restores stream position
function ICOTryStream(fs: TStream): boolean;
var
  IconDir: TICONDIR;
  IconEntry: TICONDIRENTRY;
  q: integer;
  lp: int64;
begin
{.$WARNINGS OFF}
  lp := fs.Position;
  result := false;
  if fs.Size > (sizeof(TICONDIR) + sizeof(TICONDIRENTRY)) then
  begin
    fs.Read(IconDir, sizeof(TICONDIR));
    if (IconDir.idReserved <> 0) or (IconDir.idType <> 1) or (IconDir.idCount = 0) then
    begin
      fs.Position := lp;
      exit; // FALSE
    end;
    for q := 0 to IconDir.idCount - 1 do
    begin
      fs.Read(IconEntry, sizeof(TICONDIRENTRY));
      with IconEntry do
        if (bReserved <> 0) or (dwBytesInRes = 0) or (int64(dwImageOffset) < fs.Position) then
        begin
          fs.Position := lp;
          exit; // FALSE
        end;
    end;
    result := true; // OK!
    fs.Position := lp;
  end;
{.$WARNINGS ON}
end;

function _EnumICOImStream(fs: TStream): integer;
var
  lp: int64;
  IconDir: TICONDIR;
begin
  lp := fs.Position;
  fs.Read(IconDir, sizeof(TICONDIR));
  result := IconDir.idCount;
  fs.Position := lp;
end;


// bExceptionOnInvalidSize: Handle icons returned by GetImageListSH(SHIL_JUMBO). Files without full size icons will only return a tiny icon in the top-left corner
procedure ICOReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; Preview: boolean; var Progress: TProgressRec; var AlphaChannel: TIEMask; IgnoreAlpha: boolean; bExceptionOnInvalidSize : Boolean = False);
var
  IconDir: TICONDIR;
  IconEntry: TICONDIRENTRY;
  InfoHead: TBITMAPINFOHEADER;
  p0, w: int64;
  i, q, x, y, lw, ll: integer;
  xBitsPerSample: integer;
  ColorMap4: array[0..255] of TRGBQUAD;
  bits, bits2: pbyte;
  px: PRGB;
  pxb: pbyte;
  colorcount: integer;
  height, width: integer;
  amap, andmap, pix: pbyte;
  lw1: integer;
  v1, v2: boolean;
  bitmapwidth1, bitmapheight1: integer;
  rgba, xrgba: PRGBA;
  rgb: PRGB;
  bAllZero: boolean;
  bSizeValid: Boolean;
begin
{.$WARNINGS OFF}
  bSizeValid := False;
  p0 := fs.Position;

  FillChar(IconDir, sizeof(TICONDIR), 0);
  FillChar(IconEntry, sizeof(TICONDIRENTRY), 0);

  if not IOParams.IsResource then
  begin
    fs.Read(IconDir, sizeof(TICONDIR));
    if IconDir.idReserved = 19778 then
    begin
      // this is a bmp renamed!
      fs.position := p0;
      BMPReadStream(fs, Bitmap, fs.size, IOParams, Progress, Preview, false, AlphaChannel, IgnoreAlpha);
      exit;
    end;

    if (IOParams.ICO_ImageIndex >= IconDir.idCount) or (IOParams.ICO_ImageIndex < 0) then
      exit; // Invalid ICO_ImageIndex

    IOParams.ImageCount := IconDir.idCount;

    // read all icon entries (to fill IOParams properties)
    w := fs.Position;
    FillChar(IOParams.ICO_Sizes[0], sizeof(TIOICOSizes), 0);
    q := 0;
    while q < IconDir.idCount do
    begin
      if q = IEMAXICOIMAGES then
        break;
      fs.Read(IconEntry, sizeof(TICONDIRENTRY));
      if IconEntry.bWidth > 0 then
        bSizeValid := True;
      IOParams.ICO_Sizes[q].cx := IEIFI(IconEntry.bWidth <> 0, IconEntry.bWidth, 256);
      IOParams.ICO_Sizes[q].cy := IEIFI(IconEntry.bHeight <> 0, IconEntry.bHeight, 256);
      if IconEntry.wBitCount <> 0 then
        IOParams.ICO_BitCount[q] := IconEntry.wBitCount
      else
      if IconEntry.bColorCount <> 0 then
        IOParams.ICO_BitCount[q] := _NColToBitsPixel(IconEntry.bColorCount)
      else
        IOParams.ICO_BitCount[q] := 24;
      inc(q); // 3.0.2, to avoid infinite loop when all frames are invalid
    end;
    fs.Position := w;

    // read the requested icon entry
    q := 0;
    while q < IconDir.idCount do
    begin
      fs.Read(IconEntry, sizeof(TICONDIRENTRY));
      if q = IOParams.ICO_ImageIndex then
        break; // Found ICO_ImageIndex
      inc(q); // 3.0.2, to avoid infinite loop when all frames are invalid
    end;

    if IconEntry.dwImageOffset + SizeOf(TBITMAPINFOHEADER) >= dword(fs.Size) then
    begin
      Progress.Aborting^ := true;
      exit;
    end;

    fs.Position := p0 + IconEntry.dwImageOffset;
  end;

  // read BITMAPINFOHEADER
  FillChar(InfoHead, sizeof(InfoHead), 0);
  fs.Read(InfoHead, sizeof(TBITMAPINFOHEADER));

  {$ifdef IEINCLUDEPNG}
  // detect PNG
  if (pbytearray(@InfoHead)[0]=$89) and (pbytearray(@InfoHead)[1]=$50) and (pbytearray(@InfoHead)[2]=$4E) and
     (pbytearray(@InfoHead)[3]=$47) and (pbytearray(@InfoHead)[4]=$0D) and (pbytearray(@InfoHead)[5]=$0A) and
     (pbytearray(@InfoHead)[6]=$1A) and (pbytearray(@InfoHead)[7]=$0A) then
  begin
    fs.Seek(-sizeof(TBITMAPINFOHEADER), soCurrent);
    ReadPNGStream(fs, Bitmap, IOParams, Progress, false);
    exit;
  end;
  {$endif}

  if IOParams.IsResource then
  begin
    IOParams.ICO_ImageIndex := 0;
    IOParams.ICO_Sizes[0].cx := InfoHead.biWidth;
    IOParams.ICO_Sizes[0].cy := InfoHead.biHeight div 2;
    IconEntry.bWidth := InfoHead.biWidth;
    IconEntry.bHeight := InfoHead.biHeight div 2;
    bSizeValid := True;
  end;

  if InfoHead.biXPelsPerMeter > 0 then
    IOParams.DpiX := round((InfoHead.biXPelsPerMeter / 100) * CM_per_Inch)
  else
    IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
  if InfoHead.biYPelsPerMeter > 0 then
    IOParams.DpiY := round((InfoHead.biYPelsPerMeter / 100) * CM_per_Inch)
  else
    IOParams.DpiY := IEGlobalSettings().DefaultDPIY;

  xBitsPerSample := 0;
  if IconEntry.wBitCount > 0 then
    // use BitCount of IconEntry
    xBitsPerSample := IconEntry.wBitCount;
  if InfoHead.biBitCount > 0 then
    // use BitCount of BITMAPINFOHEADER
    xBitsPerSample := InfoHead.biBitCount;
  if xBitsPerSample = 0 then
  begin
    // uses ColorCount of IconEntry, otherwise the ICO is not valid
    if IconEntry.bColorCount > 0 then
      xBitsPerSample := _NColToBitsPixel(IconEntry.bColorCount)
    else
    begin
      Progress.Aborting^ := true;
      exit;
    end;
  end;

  colorcount := 1 shl xBitsPerSample;
  if xBitsPerSample <= 8 then
  begin
    IOParams.BitsPerSample := xBitsPerSample;
    IOParams.SamplesPerPixel := 1;
  end
  else
  begin
    IOParams.BitsPerSample := 8;
    if xBitsPerSample = 32 then
      IOParams.SamplesPerPixel := 4
    else
      IOParams.SamplesPerPixel := 3;
  end;

  Width  := IconEntry.bWidth;
  Height := IconEntry.bHeight;
  if Width = 0 then
    Width := InfoHead.biWidth;
  if Height = 0 then
    Height := InfoHead.biHeight shr 1;
  if (IconEntry.bWidth <> InfoHead.biwidth) and (IconEntry.bWidth <> 0) and (InfoHead.biWidth <> 0)
    and (InfoHead.biwidth < 2048) and (InfoHead.biheight < 2048) then
  begin
    Width := InfoHead.biWidth;
    Height := InfoHead.biHeight shr 1;
  end;

  lw := (((width * xBitsPerSample) + 31) shr 5) shl 2;
  lw1 := ((width + 31) shr 5) shl 2;

  // read colormap (ColorCount is the color number of colormap. It could be not as specified in xBitsPerSample
  IOParams.FreeColorMap;
  if xBitsPerSample <= 8 then
  begin
    fs.Read(ColorMap4, sizeof(TRGBQUAD) * ColorCount);
    IOParams.fColorMapCount := ColorCount;
    IOParams.fColorMap := allocmem(ColorCount * sizeof(TRGB));
    for q := 0 to ColorCount - 1 do
    begin
      IOParams.ColorMap[q].r := ColorMap4[q].rgbRed;
      IOParams.ColorMap[q].g := ColorMap4[q].rgbGreen;
      IOParams.ColorMap[q].b := ColorMap4[q].rgbBlue;
    end;
  end;

  IOParams.Width := Width;
  IOParams.Height := Height;
  IOParams.OriginalWidth := Width;
  IOParams.OriginalHeight := Height;

  if Preview then
    exit; // PREVIEW ONLY

  if IOParams.IsNativePixelFormat then
    // use native pixel formats
    case xBitsPerSample of
      1:  Bitmap.Allocate(Width, Height, ie1g);
      4, 8:
        begin
          Bitmap.Allocate(Width, Height, ie8p);
          Bitmap.PaletteUsed := ColorCount;
          for i := 0 to ColorCount-1 do
            Bitmap.Palette[i] := IOParams.ColorMap[i];
        end;
      24, 32: Bitmap.Allocate(Width, Height, ie24RGB);
      else raise Exception.Create('Unsupported pixel format');
    end
  else
    // convert bitspersample<>1 and <>24 to RGB 24 bit
    case xBitsPerSample of
      1:   Bitmap.Allocate(Width, Height, ie1g);
      else Bitmap.Allocate(Width, Height, ie24RGB);
    end;

  if not IgnoreAlpha then
  begin
    if not assigned(AlphaChannel) then
      AlphaChannel := TIEMask.Create;
    AlphaChannel.AllocateBits(Width, Height, 8);
    AlphaChannel.Fill(255);
    AlphaChannel.Full := false;
  end;

  // read bitmap
  getmem(andmap, lw1 * Bitmap.Height);
  try
    amap := andmap;
    case xBitsPerSample of
      1:
        begin // 1 bit per pixel
          bitmapheight1 := Bitmap.Height - 1;
          for y := BitmapHeight1 downto 0 do
            fs.read(pbyte(Bitmap.Scanline[y])^, lw);
          // and map
          fs.read(andmap^, lw1 * Bitmap.Height);
          if not IgnoreAlpha then
          begin
            bitmapwidth1 := Bitmap.Width - 1;
            for y := BitmapHeight1 downto 0 do
            begin
              for x := 0 to BitmapWidth1 do
              begin
                if _GetPixelbw(amap, x) <> 0 then
                  AlphaChannel.SetPixel(x, y, 0);
              end;
              inc(amap, lw1);
            end;
          end;
          amap := andmap;
          v2 := EqualRGB(IOParams.ICO_Background, CreateRGB(255, 255, 255));
          bitmapwidth1 := Bitmap.Width - 1;
          for y := BitmapHeight1 downto 0 do
          begin
            pix := Bitmap.Scanline[y];
            for x := 0 to BitmapWidth1 do
            begin
              if _GetPixelbw(amap, x) <> 0 then
              begin
                v1 := _GetPixelbw(pix, x) <> 0;
                if v1 xor v2 then
                  _SetPixelbw(pix, x, 1)
                else
                  _SetPixelbw(pix, x, 0);
              end;
            end;
            inc(amap, lw1);
          end;
        end;
      4:
        begin // 4 bit per pixel
          w := Bitmap.Width shr 1;
          if (Bitmap.Width and 1) <> 0 then
            inc(w);
          getmem(bits, lw * Bitmap.Height);
          try
            fs.Read(bits^, lw * Bitmap.Height);
            fs.Read(andmap^, lw1 * Bitmap.Height);
            bitmapheight1 := Bitmap.Height - 1;
            if not IgnoreAlpha then
            begin
              for y := BitmapHeight1 downto 0 do
              begin
                px := Bitmap.Scanline[y];
                pxb := pbyte(px);
                bits2 := bits;
                inc(bits2, lw * (Bitmapheight1 - y));
                for x := 0 to w - 1 do
                begin
                  if _GetPixelbw(amap, x * 2) <> 0 then
                    AlphaChannel.SetPixel(x * 2, y, 0);
                  if _GetPixelbw(amap, x * 2 + 1) <> 0 then
                    AlphaChannel.SetPixel(x * 2 + 1, y, 0);
                  if IOParams.IsNativePixelFormat then
                  begin
                    pxb^ := bits2^ shr 4;
                    inc(pxb);
                    pxb^ := bits2^ and $0F;
                    inc(pxb);
                  end
                  else
                  begin
                    if _GetPixelbw(amap, x * 2) = 0 then
                      px^ := IOParams.ColorMap^[bits2^ shr 4]
                    else
                      px^ := IOParams.ICO_Background;
                    inc(px);
                    if _GetPixelbw(amap, x * 2 + 1) = 0 then
                      px^ := IOParams.ColorMap^[bits2^ and $0F]
                    else
                      px^ := IOParams.ICO_Background;
                    inc(px);
                  end;
                  inc(bits2);
                end;
                inc(amap, lw1);
              end;
            end
            else
            begin
              for y := BitmapHeight1 downto 0 do
              begin
                px := Bitmap.Scanline[y];
                pxb := pbyte(px);
                bits2 := bits;
                inc(bits2, lw * (Bitmapheight1 - y));
                for x := 0 to w - 1 do
                begin
                  if IOParams.IsNativePixelFormat then
                  begin
                    pxb^ := bits2^ shr 4;
                    inc(pxb);
                    pxb^ := bits2^ and $0F;
                  end
                  else
                  begin
                    if _GetPixelbw(amap, x * 2) = 0 then
                      px^ := IOParams.ColorMap^[bits2^ shr 4]
                    else
                      px^ := IOParams.ICO_Background;
                    inc(px);
                    if _GetPixelbw(amap, x * 2 + 1) = 0 then
                      px^ := IOParams.ColorMap^[bits2^ and $0F]
                    else
                      px^ := IOParams.ICO_Background;
                    inc(px);
                  end;
                  inc(bits2);
                end;
                inc(amap, lw1);
              end;
            end;
          finally
            freemem(bits);
          end;
        end;
      8:
        begin // 8 bit per pixel
          ll := lw * Bitmap.height;
          getmem(bits, ll);
          try
            fs.Read(bits^, ll);
            fs.Read(andmap^, lw1 * Bitmap.Height);
            bitmapheight1 := Bitmap.Height - 1;
            bitmapwidth1 := Bitmap.Width - 1;
            if not IgnoreAlpha then
            begin
              for y := BitmapHeight1 downto 0 do
              begin
                px := Bitmap.Scanline[y];
                pxb := pbyte(px);
                bits2 := bits;
                inc(bits2, lw * (Bitmap.height - 1 - y));
                for x := 0 to BitmapWidth1 do
                begin
                  if _GetPixelbw(amap, x) <> 0 then
                    AlphaChannel.SetPixel(x, y, 0);
                  if IOParams.IsNativePixelFormat then
                  begin
                    pxb^ := bits2^;
                    inc(pxb);
                  end
                  else
                  begin
                    if _GetPixelbw(amap, x) = 0 then
                      px^ := IOParams.ColorMap^[bits2^]
                    else
                      px^ := IOParams.ICO_Background;
                    inc(px);
                  end;
                  inc(bits2);
                end;
                inc(amap, lw1);
              end;
            end
            else
            begin
              for y := BitmapHeight1 downto 0 do
              begin
                px := Bitmap.Scanline[y];
                pxb := pbyte(px);
                bits2 := bits;
                inc(bits2, lw * (Bitmap.height - 1 - y));
                for x := 0 to BitmapWidth1 do
                begin
                  if IOParams.IsNativePixelFormat then
                  begin
                    pxb^ := bits2^;
                    inc(pxb);
                  end
                  else
                  begin
                    if _GetPixelbw(amap, x) = 0 then
                      px^ := IOParams.ColorMap^[bits2^]
                    else
                      px^ := IOParams.ICO_Background;
                    inc(px);
                  end;
                  inc(bits2);
                end;
                inc(amap, lw1);
              end;
            end;
          finally
            freemem(bits);
          end;
        end;
      24:
        begin // 24 bit per pixel
          for y := Bitmap.Height - 1 downto 0 do
            fs.read(pbyte(Bitmap.Scanline[y])^, lw);
          if (fs.Size-fs.Position >= lw1 * Bitmap.Height) then
          begin
            fs.Read(andmap^, lw1 * Bitmap.Height);
            amap := andmap;
            for y := Bitmap.Height-1 downto 0 do
            begin
              for x := 0 to Bitmap.width-1 do
              begin
                if _GetPixelbw(amap, x) <> 0 then
                  AlphaChannel.SetPixel(x, y, 0)
                else
                  AlphaChannel.SetPixel(x, y, 255);
              end;
              inc(amap, lw1);
            end;
          end;
        end;
      32:
        begin // 32 bit
          getmem(rgba, sizeof(TRGBA) * Bitmap.Width);
          try
            bAllZero := true;
            for y := Bitmap.Height - 1 downto 0 do
            begin
              fs.read(rgba^, sizeof(TRGBA) * Bitmap.Width);
              rgb := Bitmap.Scanline[y];
              amap := AlphaChannel.Scanline[y];
              xrgba := rgba;
              for x := 0 to Bitmap.Width - 1 do
              begin
                rgb^.r := xrgba^.r;
                rgb^.g := xrgba^.g;
                rgb^.b := xrgba^.b;
                amap^  := xrgba^.a;
                if bAllZero and (amap^ > 0) then
                begin
                  bAllZero := false;

                  // "Jumbo" Icons from Windows may simply contain a small icon in the top-left corner if a full 256x256 icon is not available 
                  if bExceptionOnInvalidSize and (bSizeValid = False) and (Bitmap.Height = 256) and (y < 66)  then
                    raise EIEException.create('Not valid 256x256 Icon');
                end;
                inc(rgb);
                inc(xrgba);
                inc(amap);
              end;
            end;
          finally
            freemem(rgba);
          end;

          if (fs.Size-fs.Position >= lw1 * Bitmap.Height) and bAllZero then
          begin
            fs.Read(andmap^, lw1 * Bitmap.Height);
            amap := andmap;
            for y := Bitmap.Height-1 downto 0 do
            begin
              for x := 0 to Bitmap.width-1 do
              begin
                if _GetPixelbw(amap, x) <> 0 then
                  AlphaChannel.SetPixel(x, y, 0)
                else
                  AlphaChannel.SetPixel(x, y, 255);
              end;
              inc(amap, lw1);
            end;
          end;

        end;
    end; // case
  finally
    freemem(andmap);
  end;

  // verify alpha channel
  if not IgnoreAlpha then
  begin
    AlphaChannel.SyncFull;
    if AlphaChannel.Full then
      FreeAndNil(AlphaChannel);
  end;
  //
{.$WARNINGS ON}
end;

procedure ICOWriteStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; sizes: array of TSize; bitcounts: array of integer);
var
  IconDir: TICONDIR;
  IconEntry: TICONDIRENTRY;
  InfoHead: TBITMAPINFOHEADER;
  i, j, row, col, l: integer;
  p0: int64; // entries position
  p1: TList; // dwBytesInRes
  p2: TList; // dwImageOffset
  p3: int64;
  qt: TIEQuantizer;
  ie: TImageEnView;
  px, buf, xbuf: pbyte;
  p_rgb: PRGB;
  ColorMap4: TColorMap4;
  icount: integer;
  bc: TList;
  SamplesPerPixel, BitsPerSample: integer;
  rgba, xrgba: PRGBA;
begin

  // icon directory
  icount := high(sizes) + 1;
  IconDir.idReserved := 0;
  IconDir.idType := 1;
  IconDir.idCount := icount;
  fs.Write(IconDir, sizeof(TICONDIR));

  Progress.per1 := 100 / icount;

  // bypass directory entries
  p0 := fs.Position;
  fs.Seek(sizeof(TICONDIRENTRY) * icount, soCurrent);

  p1 := TList.Create;
  p2 := TList.Create;
  bc := TList.Create;
  ie := TImageEnView.Create(nil);

  try

  IOParams.FreeColorMap;

  if (Bitmap.PixelFormat <> ie24RGB) then
  begin
    // suppose ie1g
    IOParams.BitsPerSample := 1;
    IOParams.SamplesPerPixel := 1;
  end;

  // write pixmaps
  for i := 0 to high(sizes) do
  begin
    // OnProgress
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * i));
    if Progress.Aborting^ then
      break;

    ie.IEBitmap.Assign(Bitmap);
    if ie.IEBitmap.PixelFormat <> ie1g then
      ie.Proc.Resample(sizes[i].cx, sizes[i].cy, IEGlobalSettings().DefaultResampleFilter)
    else
      ie.Proc.Resample(sizes[i].cx, sizes[i].cy, rfNone);
    ie.Update;
    p3 := fs.Position;
    p2.Add(pointer(p3)); // save pixmap position
    InfoHead.biSize := 40;
    InfoHead.biWidth := ie.IEBitmap.Width;
    InfoHead.biHeight := ie.IEBitmap.Height * 2;
    InfoHead.biCompression := 0;
    InfoHead.biClrUsed := 0;
    InfoHead.biClrImportant := 0;
    InfoHead.biXPelsPerMeter := round((IOParams.DpiX / CM_per_Inch) * 100);
    InfoHead.biYPelsPerMeter := round((IOParams.DpiY / CM_per_Inch) * 100);

    case bitcounts[i] of
      0:
        begin
          SamplesPerPixel := IOParams.SamplesPerPixel;
          BitsPerSample := IOParams.BitsPerSample;
        end;
      1:
        begin
          SamplesPerPixel := 1;
          BitsPerSample := 1;
        end;
      4:
        begin
          SamplesPerPixel := 1;
          BitsPerSample := 4;
        end;
      8:
        begin
          SamplesPerPixel := 1;
          BitsPerSample := 8;
        end;
      32:
        begin
          SamplesPerPixel := 4;
          BitsPerSample := 8;
        end;
    else
      begin
        SamplesPerPixel := 3;
        BitsPerSample := 8;
      end;
    end;

    // save xor map
    if (SamplesPerPixel = 3) and (BitsPerSample = 8) then
    begin
      // 24 bit (suppose PixelFormat=ie24RGB)
      bc.Add(pointer(24));
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 24;
      InfoHead.biSizeImage := 0; //(ie.IEBitmap.Height*ie.IEBitmap.RowLen) + (ie.IEBitmap.Width*ie.IEBitmap.Height div 8);
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      for row := ie.IEBitmap.Height - 1 downto 0 do
      begin
        px := ie.IEBitmap.Scanline[row];
        fs.Write(px^, ie.IEBitmap.RowLen);
      end;
    end
    else
    if (SamplesPerPixel = 4) and (BitsPerSample = 8) then
    begin
      // 32 bit (suppose PixelFormat=ie24RGB)
      bc.Add(pointer(32));
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 32;
      InfoHead.biSizeImage := 0; //(ie.IEBitmap.Height*ie.IEBitmap.RowLen) + (ie.IEBitmap.Width*ie.IEBitmap.Height div 8);
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      getmem(rgba, sizeof(TRGBA) * ie.IEBitmap.Width);
      try
        for row := ie.IEBitmap.Height - 1 downto 0 do
        begin
          p_rgb := ie.IEBitmap.Scanline[row];
          px := ie.IEBitmap.AlphaCHannel.Scanline[row];
          xrgba := rgba;
          for col := 0 to ie.IEBitmap.Width - 1 do
          begin
            xrgba^.r := p_rgb^.r;
            xrgba^.g := p_rgb^.g;
            xrgba^.b := p_rgb^.b;
            xrgba^.a := px^;
            inc(p_rgb);
            inc(px);
            inc(xrgba);
          end;
          fs.Write(rgba^, sizeof(TRGBA) * ie.IEBitmap.Width);
        end;
      finally
        freemem(rgba);
      end;
    end
    else
    if (SamplesPerPixel = 1) and (BitsPerSample = 1) then
    begin
      // 1 bit (suppose PixelFormat=ie1g)
      bc.Add(pointer(1));
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 1;
      InfoHead.biSizeImage := (ie.IEBitmap.Height * ie.IEBitmap.RowLen) + (ie.IEBitmap.Width * ie.IEBitmap.Height div 8);
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      ColorMap4[0].rgbBlue := 0;
      ColorMap4[0].rgbGreen := 0;
      ColorMap4[0].rgbRed := 0;
      ColorMap4[0].rgbReserved := 0;
      ColorMap4[1].rgbBlue := 255;
      ColorMap4[1].rgbGreen := 255;
      ColorMap4[1].rgbRed := 255;
      ColorMap4[1].rgbReserved := 0;
      fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 2);
      for row := ie.IEBitmap.Height - 1 downto 0 do
      begin
        px := ie.IEBitmap.Scanline[row];
        fs.Write(px^, ie.IEBitmap.RowLen);
      end;
    end
    else
    if (SamplesPerPixel = 1) and (BitsPerSample = 4) then
    begin
      // 4 bit (suppose PixelFormat=ie24RGB)
      bc.Add(pointer(4));
      l := IEBitmapRowLen(ie.IEBitmap.Width, 4, 32);
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 4;
      InfoHead.biSizeImage := (ie.IEBitmap.Height * l) + (ie.IEBitmap.Width * ie.IEBitmap.Height div 8);
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      case ie.IEBitmap.PixelFormat of
        ie24RGB:
          begin
            IOParams.FreeColorMap;
            getmem(IOParams.fColorMap, 16 * sizeof(TRGB));
            IOParams.fColorMapCount := 16;
            qt := TIEQuantizer.Create(ie.IEBitmap, IOParams.ColorMap^, 15);
            for j := 1 to 15 do
            begin
              ColorMap4[j].rgbBlue := IOParams.ColorMap[j - 1].b;
              ColorMap4[j].rgbGreen := IOParams.ColorMap[j - 1].g;
              ColorMap4[j].rgbRed := IOParams.ColorMap[j - 1].r;
              ColorMap4[j].rgbReserved := 0;
            end;
            // index 0 reserved for transparency
            ColorMap4[0].rgbBlue := 0;
            ColorMap4[0].rgbGreen := 0;
            ColorMap4[0].rgbRed := 0;
            ColorMap4[0].rgbReserved := 0;
            getmem(buf, l);
            try
              fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 16);
              for row := ie.IEBitmap.Height - 1 downto 0 do
              begin
                p_rgb := ie.IEBitmap.Scanline[row];
                xbuf := buf;
                col := 0;
                while col < ie.IEBitmap.Width do
                begin
                  if ie.IEBitmap.HasAlphaChannel and (ie.IEBitmap.Alpha[col, row] < 255) then
                    xbuf^ := 0
                  else
                    xbuf^ := (qt.RGBIndex[p_rgb^] + 1) shl 4;
                  inc(col);
                  inc(p_rgb);

                  if ie.IEBitmap.HasAlphaChannel and (ie.IEBitmap.Alpha[col, row] < 255) then
                    xbuf^ := xbuf^ // just to put something
                  else
                    xbuf^ := xbuf^ or (qt.RGBIndex[p_rgb^] + 1);
                  inc(col);
                  inc(p_rgb);

                  inc(xbuf);
                end;
                fs.Write(buf^, l);
              end;
            finally
              freemem(buf);
              FreeAndNil(qt);
            end;
          end;
        ie8p:
          begin
            for j := 0 to 15 do
            begin
              ColorMap4[j].rgbBlue := ie.IEBitmap.Palette[j].b;
              ColorMap4[j].rgbGreen := ie.IEBitmap.Palette[j].g;
              ColorMap4[j].rgbRed := ie.IEBitmap.Palette[j].r;
              ColorMap4[j].rgbReserved := 0;
            end;
            fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 16);
            getmem(buf, l);
            try
              for row := ie.IEBitmap.Height - 1 downto 0 do
              begin
                px := ie.IEBitmap.Scanline[row];
                xbuf := buf;
                col := 0;
                while col < ie.IEBitmap.Width do
                begin
                  xbuf^ := px^ shl 4;
                  inc(col);
                  inc(px);

                  xbuf^ := xbuf^ or px^;
                  inc(col);
                  inc(px);

                  inc(xbuf);
                end;
                fs.Write(buf^, l);
              end;
            finally
              freemem(buf);
            end;
          end;
      end;
    end
    else
    begin
      // 8 bit
      bc.Add(pointer(8));
      l := IEBitmapRowLen(ie.IEBitmap.Width, 8, 32);
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 8;
      InfoHead.biSizeImage := (ie.IEBitmap.Height * l) + (ie.IEBitmap.Width * ie.IEBitmap.Height div 8);
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      case ie.IEBitmap.PixelFormat of
        ie24RGB:
          begin
            getmem(IOParams.fColorMap, 256 * sizeof(TRGB));
            IOParams.fColorMapCount := 256;
            buf := nil;
            qt := TIEQuantizer.Create(ie.IEBitmap, IOParams.ColorMap^, 255);
            try
              for j := 1 to 255 do
              begin
                ColorMap4[j].rgbBlue := IOParams.ColorMap[j - 1].b;
                ColorMap4[j].rgbGreen := IOParams.ColorMap[j - 1].g;
                ColorMap4[j].rgbRed := IOParams.ColorMap[j - 1].r;
                ColorMap4[j].rgbReserved := 0;
              end;
              // index 0 reserved for transparency
              ColorMap4[0].rgbBlue := 0;
              ColorMap4[0].rgbGreen := 0;
              ColorMap4[0].rgbRed := 0;
              ColorMap4[0].rgbReserved := 0;
              //
              fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 256);
              getmem(buf, l);
              for row := ie.IEBitmap.Height - 1 downto 0 do
              begin
                p_rgb := ie.IEBitmap.Scanline[row];
                xbuf := buf;
                for col := 0 to ie.IEBitmap.Width - 1 do
                begin
                  if ie.IEBitmap.HasAlphaChannel and (ie.IEBitmap.Alpha[col, row] < 255) then
                    xbuf^ := 0
                  else
                    xbuf^ := qt.RGBIndex[p_rgb^] + 1;
                  inc(p_rgb);
                  inc(xbuf);
                end;
                fs.Write(buf^, l);
              end;
            finally
              freemem(buf);
              FreeAndNil(qt);
            end;
          end;
        ie8p:
          begin
            for j := 0 to 255 do
            begin
              ColorMap4[j].rgbBlue := ie.IEBitmap.Palette[j].b;
              ColorMap4[j].rgbGreen := ie.IEBitmap.Palette[j].g;
              ColorMap4[j].rgbRed := ie.IEBitmap.Palette[j].r;
              ColorMap4[j].rgbReserved := 0;
            end;
            fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 256);
            for row := ie.IEBitmap.Height - 1 downto 0 do
              fs.Write(pbyte(ie.IEBitmap.Scanline[row])^, l);
          end;
      end;
    end;
    // save and-map
    l := IEBitmapRowLen(ie.IEBitmap.Width, 1, 32);
    getmem(buf, l);
    try
      if ie.IEBitmap.HasAlphaChannel then
      begin
        for row := ie.IEBitmap.Height - 1 downto 0 do
        begin
          fillchar(buf^, l, 0);
          px := ie.IEBitmap.AlphaChannel.ScanLine[row];
          for col := 0 to ie.IEBitmap.Width - 1 do
          begin
            if px^ < 255 then
              _SetPixelbw(buf, col, 1);
            inc(px);
          end;
          fs.Write(buf^, l);
        end;
      end
      else
      begin
        // empty
        fillchar(buf^, l, 0);
        for row := ie.IEBitmap.Height - 1 downto 0 do
          fs.Write(buf^, l);
      end;
    finally
      freemem(buf);
    end;

    p1.Add(pointer(fs.Position - p3)); // save pixmap length
  end;

  // write icon entries
  p3 := fs.Position; // end position
  fs.Position := p0;
  for i := 0 to high(sizes) do
  begin
    IconEntry.bWidth := sizes[i].cx;
    IconEntry.bHeight := sizes[i].cy;
    if (integer(bc[i]) = 24) or (integer(bc[i]) = 32) then
    begin
      IconEntry.bColorCount := 0;
      IconEntry.wPlanes := 1;
      IconEntry.wBitCount := integer(bc[i]);
    end
    else
    begin
      IconEntry.bColorCount := imin((1 shl integer(bc[i])), 255);
      IconEntry.wPlanes := 0;
      IconEntry.wBitCount := 0;
    end;
    IconEntry.bReserved := 0;
    IconEntry.dwBytesInRes := integer(p1[i]);
    IconEntry.dwImageOffset := integer(p2[i]);
    fs.Write(IconEntry, sizeof(TICONDIRENTRY));
  end;
  fs.Position := p3;

  finally
    ie.Free;
    p1.Free;
    p2.Free;
    bc.Free;
  end;

end;

// accept following BitsPerSample and SamplesPerPixel:
(*
             SamplesPerPixel := 1;
               BitsPerSample := 1;

             SamplesPerPixel := 1;
               BitsPerSample := 4;

             SamplesPerPixel := 1;
               BitsPerSample := 8;

             SamplesPerPixel := 4;
               BitsPerSample := 8;

             SamplesPerPixel := 3;
               BitsPerSample := 8;
*)

procedure ICOWriteStream2(fs: TStream; ielist: array of TObject; var Progress: TProgressRec);
var
  IconDir: TICONDIR;
  IconEntry: TICONDIRENTRY;
  InfoHead: TBITMAPINFOHEADER;
  i, j, row, col, l: integer;
  p0: int64; // entries position
  p1: TList; // dwBytesInRes
  p2: TList; // dwImageOffset
  p3: int64;
  qt: TIEQuantizer;
  px, buf, xbuf: pbyte;
  p_rgb: PRGB;
  ColorMap4: TColorMap4;
  icount: integer;
  bc: TList;
  SamplesPerPixel, BitsPerSample: integer;
  rgba, xrgba: PRGBA;
  Bitmap: TIEBitmap;
  IOParams: TIOParamsVals;
begin
  // icon directory
  icount := high(ielist) + 1;
  IconDir.idReserved := 0;
  IconDir.idType := 1;
  IconDir.idCount := icount;
  fs.Write(IconDir, sizeof(TICONDIR));
  //
  Progress.per1 := 100 / icount;
  // bypass directory entries
  p0 := fs.Position;
  fs.Seek(sizeof(TICONDIRENTRY) * icount, soCurrent);
  //
  p1 := TList.Create;
  p2 := TList.Create;
  bc := TList.Create;
  // write pixmaps
  for i := 0 to icount - 1 do
  begin
    Bitmap := TImageEnView(ielist[i]).IEBitmap;
    IOParams := TImageEnView(ielist[i]).IO.Params;
    if (Bitmap.PixelFormat <> ie24RGB) and (Bitmap.PixelFormat<>ie8p) then
    begin
      // suppose ie1g
      IOParams.BitsPerSample := 1;
      IOParams.SamplesPerPixel := 1;
    end;
    IOParams.FreeColorMap;
    // OnProgress
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * i));
    if Progress.Aborting^ then
      break;
    //
    p3 := fs.Position;
    p2.Add(pointer(p3)); // save pixmap position
    InfoHead.biSize := 40;
    InfoHead.biWidth := Bitmap.Width;
    InfoHead.biHeight := Bitmap.Height * 2;
    InfoHead.biCompression := 0;
    InfoHead.biClrUsed := 0;
    InfoHead.biClrImportant := 0;
    InfoHead.biXPelsPerMeter := round((IOParams.DpiX / CM_per_Inch) * 100);
    InfoHead.biYPelsPerMeter := round((IOParams.DpiY / CM_per_Inch) * 100);
    SamplesPerPixel := IOParams.SamplesPerPixel;
    BitsPerSample := IOParams.BitsPerSample;
    // save xor map
    if (SamplesPerPixel = 3) and (BitsPerSample = 8) then
    begin
      // 24 bit (suppose PixelFormat=ie24RGB)
      bc.Add(pointer(24));
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 24;
      InfoHead.biSizeImage := Bitmap.Height * Bitmap.RowLen;
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      for row := Bitmap.Height - 1 downto 0 do
      begin
        px := Bitmap.Scanline[row];
        fs.Write(px^, Bitmap.RowLen);
      end;
    end
    else
    if (SamplesPerPixel = 4) and (BitsPerSample = 8) then
    begin
      // 32 bit (suppose PixelFormat=ie24RGB)
      bc.Add(pointer(32));
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 32;
      InfoHead.biSizeImage := 0;
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      getmem(rgba, sizeof(TRGBA) * Bitmap.Width);
      for row := Bitmap.Height - 1 downto 0 do
      begin
        p_rgb := Bitmap.Scanline[row];
        px := Bitmap.AlphaCHannel.Scanline[row];
        xrgba := rgba;
        for col := 0 to Bitmap.Width - 1 do
        begin
          xrgba^.r := p_rgb^.r;
          xrgba^.g := p_rgb^.g;
          xrgba^.b := p_rgb^.b;
          xrgba^.a := px^;
          inc(p_rgb);
          inc(px);
          inc(xrgba);
        end;
        fs.Write(rgba^, sizeof(TRGBA) * Bitmap.Width);
      end;
      freemem(rgba);
    end
    else
    if (SamplesPerPixel = 1) and (BitsPerSample = 1) then
    begin
      // 1 bit (suppose PixelFormat=ie1g)
      bc.Add(pointer(1));
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 1;
      InfoHead.biSizeImage := (Bitmap.Height * Bitmap.RowLen) + (Bitmap.Width * Bitmap.Height div 8);
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      ColorMap4[0].rgbBlue := 0;
      ColorMap4[0].rgbGreen := 0;
      ColorMap4[0].rgbRed := 0;
      ColorMap4[0].rgbReserved := 0;
      ColorMap4[1].rgbBlue := 255;
      ColorMap4[1].rgbGreen := 255;
      ColorMap4[1].rgbRed := 255;
      ColorMap4[1].rgbReserved := 0;
      fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 2);
      for row := Bitmap.Height - 1 downto 0 do
      begin
        px := Bitmap.Scanline[row];
        fs.Write(px^, Bitmap.RowLen);
      end;
    end
    else
    if (SamplesPerPixel = 1) and (BitsPerSample = 4) then
    begin
      // 4 bit
      bc.Add(pointer(4));
      l := IEBitmapRowLen(Bitmap.Width, 4, 32);
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 4;
      InfoHead.biSizeImage := (Bitmap.Height * l) + (Bitmap.Width * Bitmap.Height div 8);
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      IOParams.FreeColorMap;
      case Bitmap.PixelFormat of
        ie24RGB:
          begin
            getmem(IOParams.fColorMap, 16 * sizeof(TRGB));
            IOParams.fColorMapCount := 16;
            qt := TIEQuantizer.Create(Bitmap, IOParams.ColorMap^, 16);
            getmem(buf, l);
            try
              for j := 0 to 15 do
              begin
                ColorMap4[j].rgbBlue := IOParams.ColorMap[j].b;
                ColorMap4[j].rgbGreen := IOParams.ColorMap[j].g;
                ColorMap4[j].rgbRed := IOParams.ColorMap[j].r;
                ColorMap4[j].rgbReserved := 0;
              end;
              fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 16);
              for row := Bitmap.Height - 1 downto 0 do
              begin
                p_rgb := Bitmap.Scanline[row];
                xbuf := buf;
                col := 0;
                while col < Bitmap.Width do
                begin
                  xbuf^ := qt.RGBIndex[p_rgb^] shl 4;
                  inc(col);
                  inc(p_rgb);

                  xbuf^ := xbuf^ or qt.RGBIndex[p_rgb^];
                  inc(col);
                  inc(p_rgb);

                  inc(xbuf);
                end;
                fs.Write(buf^, l);
              end;
            finally
              freemem(buf);
              FreeAndNil(qt);
            end;
          end;
        ie8p:
          begin
            for j := 0 to 15 do
            begin
              ColorMap4[j].rgbBlue := Bitmap.Palette[j].b;
              ColorMap4[j].rgbGreen := Bitmap.Palette[j].g;
              ColorMap4[j].rgbRed := Bitmap.Palette[j].r;
              ColorMap4[j].rgbReserved := 0;
            end;
            fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 16);
            getmem(buf, l);
            try
              for row := Bitmap.Height - 1 downto 0 do
              begin
                px := Bitmap.Scanline[row];
                xbuf := buf;
                col := 0;
                while col < Bitmap.Width do
                begin
                  xbuf^ := px^ shl 4;
                  inc(col);
                  inc(px);

                  xbuf^ := xbuf^ or px^;
                  inc(col);
                  inc(px);

                  inc(xbuf);
                end;
                fs.Write(buf^, l);
              end;
            finally
              freemem(buf);
            end;
          end;
      end
    end
    else
    begin
      // 8 bit
      bc.Add(pointer(8));
      l := IEBitmapRowLen(Bitmap.Width, 8, 32);
      InfoHead.biPlanes := 1;
      InfoHead.biBitCount := 8;
      InfoHead.biSizeImage := (Bitmap.Height * l) + (Bitmap.Width * Bitmap.Height div 8);
      fs.Write(InfoHead, sizeof(TBITMAPINFOHEADER));
      case Bitmap.PixelFormat of
        ie24RGB:
          begin
            IOParams.FreeColorMap;
            getmem(IOParams.fColorMap, 256 * sizeof(TRGB));
            IOParams.fColorMapCount := 256;
            qt := TIEQuantizer.Create(Bitmap, IOParams.ColorMap^, 256);
            for j := 0 to 255 do
            begin
              ColorMap4[j].rgbBlue := IOParams.ColorMap[j].b;
              ColorMap4[j].rgbGreen := IOParams.ColorMap[j].g;
              ColorMap4[j].rgbRed := IOParams.ColorMap[j].r;
              ColorMap4[j].rgbReserved := 0;
            end;
            fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 256);
            getmem(buf, l);
            for row := Bitmap.Height - 1 downto 0 do
            begin
              p_rgb := Bitmap.Scanline[row];
              xbuf := buf;
              for col := 0 to Bitmap.Width - 1 do
              begin
                xbuf^ := qt.RGBIndex[p_rgb^];
                inc(p_rgb);
                inc(xbuf);
              end;
              fs.Write(buf^, l);
            end;
            freemem(buf);
            FreeAndNil(qt);
          end;
        ie8p:
          begin
            for j := 0 to 255 do
            begin
              ColorMap4[j].rgbBlue := Bitmap.Palette[j].b;
              ColorMap4[j].rgbGreen := Bitmap.Palette[j].g;
              ColorMap4[j].rgbRed := Bitmap.Palette[j].r;
              ColorMap4[j].rgbReserved := 0;
            end;
            fs.Write(ColorMap4[0], sizeof(TRGBQUAD) * 256);
            for row := Bitmap.Height - 1 downto 0 do
              fs.Write(pbyte(Bitmap.Scanline[row])^, l);
          end;
      end;
    end;
    // save and-map
    l := IEBitmapRowLen(Bitmap.width, 1, 32);
    getmem(buf, l);
    if Bitmap.HasAlphaChannel then
    begin
      for row := Bitmap.Height - 1 downto 0 do
      begin
        fillchar(buf^, l, 0);
        px := Bitmap.AlphaChannel.ScanLine[row];
        for col := 0 to Bitmap.Width - 1 do
        begin
          if px^ < 255 then
            _SetPixelbw(buf, col, 1);
          inc(px);
        end;
        fs.Write(buf^, l);
      end;
    end
    else
    begin
      // empty
      fillchar(buf^, l, 0);
      for row := Bitmap.Height - 1 downto 0 do
        fs.Write(buf^, l);
    end;
    freemem(buf);
    //
    p1.Add(pointer(fs.Position - p3)); // save pixmap length
  end;
  // write icon entries
  p3 := fs.Position; // end position
  fs.Position := p0;
  for i := 0 to icount - 1 do
  begin
    IconEntry.bWidth := TImageEnView(ielist[i]).IEBitmap.Width;
    IconEntry.bHeight := TImageEnView(ielist[i]).IEBitmap.Height;
    if (integer(bc[i]) = 24) or (integer(bc[i]) = 32) then
    begin
      IconEntry.bColorCount := 0;
      IconEntry.wPlanes := 1;
      IconEntry.wBitCount := integer(bc[i]);
    end
    else
    begin
      IconEntry.bColorCount := imin((1 shl integer(bc[i])), 255);
      IconEntry.wPlanes := 0;
      IconEntry.wBitCount := 0;
    end;
    IconEntry.bReserved := 0;
    IconEntry.dwBytesInRes := integer(p1[i]);
    IconEntry.dwImageOffset := integer(p2[i]);
    fs.Write(IconEntry, sizeof(TICONDIRENTRY));
  end;
  fs.Position := p3;
  //
  FreeAndNil(p1);
  FreeAndNil(p2);
  FreeAndNil(bc);
end;

// ICO
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ICO export

{$ifndef TCursorOrIcon}
type
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;
{$endif}
{$ifndef TIconRec}
  TIconRec = packed record
    Width: Byte;
    Height: Byte;
    Colors: Word;
    Reserved1: Word;
    Reserved2: Word;
    DIBSize: Longint;
    DIBOffset: Longint;
  end;
{$endif}

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader; Colors: Integer);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
  if Bytes = 0 then exit
  else
  if (Bytes >= (sizeof(DS.dsbm) + sizeof(DS.dsbmih))) and (DS.dsbmih.biSize >= DWORD(sizeof(DS.dsbmih))) then
    BI := DS.dsbmih
  else
  begin
    FillChar(BI, sizeof(BI), 0);
    with BI, DS.dsbm do
    begin
      biSize := SizeOf(BI);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case Colors of
    2: BI.biBitCount := 1;
    3..16:
      begin
        BI.biBitCount := 4;
        BI.biClrUsed := Colors;
      end;
    17..256:
      begin
        BI.biBitCount := 32;
        BI.biClrUsed := Colors;
      end;
  else
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;
  if BI.biClrImportant > BI.biClrUsed then
    BI.biClrImportant := BI.biClrUsed;
  if BI.biSizeImage = 0 then
    BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Abs(BI.biHeight);
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD; var ImageSize: DWORD; Colors: Integer);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI, Colors);
  if BI.biBitCount > 8 then
  begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end
  else
    if BI.biClrUsed = 0 then
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl BI.biBitCount)
    else
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * BI.biClrUsed;
  ImageSize := BI.biSizeImage;
end;

function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits; Colors: Integer): Boolean;
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), Colors);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight, @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
  finally
    if OldPal <> 0 then
      SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

procedure WriteIcon(Stream: TStream; Icon: HICON);
var
  IconInfo: TIconInfo;
  MonoInfoSize, ColorInfoSize: DWORD;
  MonoBitsSize, ColorBitsSize: DWORD;
  MonoInfo, MonoBits, ColorInfo, ColorBits: Pointer;
  CI: TCursorOrIcon;
  List: TIconRec;
begin
  FillChar(CI, SizeOf(CI), 0);
  FillChar(List, SizeOf(List), 0);
  if not (GetIconInfo(Icon, IconInfo)) then
    exit;
  try
    InternalGetDIBSizes(IconInfo.hbmMask, MonoInfoSize, MonoBitsSize, 2);
    InternalGetDIBSizes(IconInfo.hbmColor, ColorInfoSize, ColorBitsSize, 256);
    MonoInfo := nil;
    MonoBits := nil;
    ColorInfo := nil;
    ColorBits := nil;
    try
      MonoInfo := AllocMem(MonoInfoSize);
      MonoBits := AllocMem(MonoBitsSize);
      ColorInfo := AllocMem(ColorInfoSize);
      ColorBits := AllocMem(ColorBitsSize);
      InternalGetDIB(IconInfo.hbmMask, 0, MonoInfo^, MonoBits^, 2);
      InternalGetDIB(IconInfo.hbmColor, 0, ColorInfo^, ColorBits^, 256);
      with CI do
      begin
        CI.wType := 1;
        CI.Count := 1;
      end;
      Stream.Write(CI, SizeOf(CI));
      with List, PBitmapInfoHeader(ColorInfo)^ do
      begin
        Width := biWidth;
        Height := biHeight;
        Colors := biPlanes * biBitCount;
        DIBSize := ColorInfoSize + ColorBitsSize + MonoBitsSize;
        DIBOffset := SizeOf(CI) + SizeOf(List);
      end;
      Stream.Write(List, SizeOf(List));
      with PBitmapInfoHeader(ColorInfo)^ do
        Inc(biHeight, biHeight);
      Stream.Write(ColorInfo^, ColorInfoSize);
      Stream.Write(ColorBits^, ColorBitsSize);
      Stream.Write(MonoBits^, MonoBitsSize);
    finally
      FreeMem(ColorInfo, ColorInfoSize);
      FreeMem(ColorBits, ColorBitsSize);
      FreeMem(MonoInfo, MonoInfoSize);
      FreeMem(MonoBits, MonoBitsSize);
    end;
  finally
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
  end;
end;

function IESaveIconToStream(Stream: TStream; icon: HICON): boolean;
begin
  try
    WriteIcon(Stream, icon);
    result := true;
  except
    result := false;
  end;
end;

procedure IEConvertIconToBitmap(icon: HICON; DestBitmap: TIEBitmap; bExceptionOnInvalidSize : Boolean = False);
var
  ms: TMemoryStream;
  Progress: TProgressRec;
  tmpAlphaChannel: TIEMask;
  aborting: boolean;
  Params: TIOParamsVals;
begin
  ms := TMemoryStream.Create;
  IESaveIconToStream(ms, icon);
  ms.Position := 0;
  aborting := false;
  Progress.Aborting := @aborting;
  Progress.fOnProgress := nil;
  Progress.Sender := nil;
  DestBitmap.RemoveAlphaChannel();
  tmpAlphaChannel := nil;
  Params := TIOParamsVals.Create(nil);
  try
    ICOReadStream(ms, DestBitmap, Params, false, Progress, tmpAlphaChannel, false, bExceptionOnInvalidSize);
    if assigned(tmpAlphaChannel) then
      DestBitmap.AlphaChannel.CopyFromTIEMask(tmpAlphaChannel);
  finally
    tmpAlphaChannel.Free;
    Params.Free;
    ms.Free;
  end;
end;

function IEGetFileIcon(const filename: string): HICON;
var
  shfi: TShFileInfo;
begin
  try
    FillChar(shfi, SizeOf(TShFileInfo), 0);
    ShGetFileInfo(PChar(filename), 0, shfi, SizeOf(TShFileInfo), SHGFI_ICON or SHGFI_LARGEICON);
    Result := shfi.hIcon;
  except
    Result := 0;
  end;
end;

procedure IEGetFileIcon(const filename : string; DestBitmap: TIEBitmap);
var
  icon: HICON;
begin
  icon := IEGetFileIcon(filename);
  IEConvertIconToBitmap(icon, DestBitmap);
  DestroyIcon(icon);
end;

// ICO export
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// CUR readubg

type

  TCURSORDIR = packed record
    cdReserved: WORD;
    cdType: WORD;
    cdCount: WORD;
  end;

  TCURSORDIRENTRY = packed record
    bWidth: BYTE;
    bHeight: BYTE;
    bColorCount: BYTE;
    bReserved: BYTE;
    wXHotspot: WORD;
    wYHotspot: WORD;
    lBytesInRes: DWORD;
    dwImageOffset: DWORD;
  end;

  TCURSORDIR_RES = packed record
    wReserved: WORD;
    wResID: WORD;
    wNumImages: WORD;
  end;

  TCURSORDIRENTRY_RES = packed record
    wWidth: WORD;
    wHeight: WORD;
    wPlanes: WORD;
    wBitCount: WORD;
    dwBytesInImage: DWORD;
    wID: WORD;
  end;

// restores stream position
function CURTryStream(fs: TStream): boolean;
var
  CurDir: TCURSORDIR;
  CurEntry: TCURSORDIRENTRY;
  q: integer;
  lp: int64;
begin
{.$WARNINGS OFF}
  lp := fs.Position;
  result := false;
  if fs.Size > (sizeof(TCURSORDIR) + sizeof(TCURSORDIRENTRY)) then
  begin
    fs.Read(CurDir, sizeof(TCURSORDIR));
    if (CurDir.cdReserved <> 0) or (CurDir.cdType <> 2) or (CurDir.cdCount = 0) then
    begin
      fs.Position := lp;
      exit; // FALSE
    end;
    for q := 0 to CurDir.cdCount - 1 do
    begin
      fs.Read(CurEntry, sizeof(TCURSORDIRENTRY));
      with CurEntry do
        if (bReserved <> 0) or (lBytesInRes = 0) or (int64(dwImageOffset) < fs.Position) then
        begin
          fs.Position := lp;
          exit; // FALSE
        end;
    end;
    result := true; // OK!
    fs.Position := lp;
  end;
{.$WARNINGS ON}
end;

procedure CURReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; Preview: boolean; var Progress: TProgressRec; var AlphaChannel: TIEMask; IgnoreAlpha: boolean);
var
  CurDir: TCURSORDIR;
  CurEntry: TCURSORDIRENTRY;
  InfoHead: TBITMAPINFOHEADER;
  p0: int64;
  q, x, y, lw, w: integer;
  xBitsPerSample: integer;
  ColorMap4: array[0..255] of TRGBQUAD;
  bits, bits2: pbyte;
  px: PRGB;
  colorcount: int64;
  amap, andmap, pix: pbyte;
  lw1: integer;
  v1, v2: boolean;
  height, width: integer;
  bitmapwidth1, bitmapheight1: integer;
  ww: word;
  rgba, xrgba: PRGBA;
  rgb: PRGB;
  allzero: boolean;
begin
{.$WARNINGS OFF}
  p0 := fs.Position;

  // default values in case of CurEntry.bWidth=0 or CurEntry.bHeight=0
  Width  := 256;
  Height := 256;

  FillChar(CurDir, sizeof(TCURSORDIR), 0);
  FillChar(CurEntry, sizeof(TCURSORDIRENTRY), 0);

  if IOParams.IsResource then
  begin
    // a RT_CURSOR starts with X-HotSpot and Y-HotSpot, then BITMAPINFOHEADER
    fs.Read(ww, 2);
    IOParams.CUR_XHotSpot := ww;
    fs.Read(ww, 2);
    IOParams.CUR_YHotSpot := ww;
  end
  else
  begin
    // has Cursor directory and entries
    fs.Read(CurDir, sizeof(TCURSORDIR));
    if (IOParams.CUR_ImageIndex >= CurDir.cdCount) or (IOParams.CUR_ImageIndex < 0) then
      exit; // Invalid CUR_ImageIndex

    IOParams.ImageCount := CurDir.cdCount;

    for q := 0 to CurDir.cdCount - 1 do
    begin
      fs.Read(CurEntry, sizeof(TCURSORDIRENTRY));
      if q = IOParams.CUR_ImageIndex then
        break; // Found CUR_ImageIndex
    end;

    if CurEntry.bWidth<>0 then
      Width := CurEntry.bWidth;
    if CurEntry.bHeight<>0 then
      Height := CurEntry.bHeight;

    IOParams.CUR_XHotSpot := CurEntry.wXHotspot;
    IOParams.CUR_YHotSpot := CurEntry.wYHotspot;

    fs.Position := p0 + CurEntry.dwImageOffset;
  end;

  // read BITMAPINFOHEADER
  FillChar(InfoHead, sizeof(InfoHead), 0);
  fs.Read(InfoHead, sizeof(TBITMAPINFOHEADER));

  if IOParams.IsResource then
  begin
    // han't Cursor directory and entries (it could be a RT_CURSOR resource)
    InfoHead.biHeight := InfoHead.biHeight div 2;
    Width := InfoHead.biWidth;
    Height := InfoHead.biHeight;
  end;

  IOParams.Width  := Width;
  IOParams.Height := Height;
  IOParams.OriginalWidth  := Width;
  IOParams.OriginalHeight := Height;
  IOParams.SamplesPerPixel := 1;

  if InfoHead.biXPelsPerMeter > 0 then
    IOParams.DpiX := round((InfoHead.biXPelsPerMeter / 100) * CM_per_Inch)
  else
    IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
  if InfoHead.biYPelsPerMeter > 0 then
    IOParams.DpiY := round((InfoHead.biYPelsPerMeter / 100) * CM_per_Inch)
  else
    IOParams.DpiY := IEGlobalSettings().DefaultDPIY;

  xBitsPerSample := 0;
  if InfoHead.biBitCount > 0 then
    // uses BitCount of BITMAPINFOHEADER
    xBitsPerSample := InfoHead.biBitCount;
  if xBitsPerSample = 0 then
  begin
    // uses ColorCount of CurEntry, otherwise the CUR is not valid
    if CurEntry.bColorCount > 0 then
      xBitsPerSample := _NColToBitsPixel(CurEntry.bColorCount)
    else
    begin
      Progress.Aborting^ := true;
      exit;
    end;
  end;
  if CurEntry.bColorCount = 0 then
    // ColorCount depends by BitCount (xBitsPerSample)
    colorcount := int64(1) shl xBitsPerSample
  else
  begin
    if CurEntry.bColorCount = 255 then
      colorcount := 256
    else
      colorcount := CurEntry.bColorCount;
  end;
  IOParams.BitsPerSample := xBitsPerSample;

  // read colormap (ColorCount is the color count of colormap. it could be not associated with xBitsPerSample
  if ColorCount<=256 then
  begin
    fs.Read(ColorMap4, sizeof(TRGBQUAD) * ColorCount);
    IOParams.FreeColorMap;
    IOParams.fColorMapCount := ColorCount;
    IOParams.fColorMap := allocmem(ColorCount * sizeof(TRGB));
    for q := 0 to ColorCount - 1 do
    begin
      IOParams.ColorMap[q].r := ColorMap4[q].rgbRed;
      IOParams.ColorMap[q].g := ColorMap4[q].rgbGreen;
      IOParams.ColorMap[q].b := ColorMap4[q].rgbBlue;
    end;
  end;

  if Preview then
    exit; // PREVIEW ONLY

  if ColorCount = 2 then
    Bitmap.Allocate(Width, Height, ie1g)
  else
    Bitmap.Allocate(Width, Height, ie24RGB);

  if not IgnoreAlpha then
  begin
    if not assigned(AlphaChannel) then
      AlphaChannel := TIEMask.Create;
    AlphaChannel.AllocateBits(Width, Height, 8);
    AlphaChannel.Fill(255);
  end;

  lw := (((width * xBitsPerSample) + 31) shr 5) shl 2;
  lw1 := ((width + 31) shr 5) shl 2;

  // read bitmap
  getmem(andmap, lw1 * Bitmap.Height);
  try
    amap := andmap;
    if not IgnoreAlpha then
      AlphaChannel.Full := false;
    case xBitsPerSample of
      1:
        begin // 1 bit per pixel
          bitmapheight1 := Bitmap.Height - 1;
          bitmapwidth1 := Bitmap.Width - 1;
          for y := Bitmap.Height - 1 downto 0 do
            fs.read(pbyte(Bitmap.Scanline[y])^, lw);
          fs.read(andmap^, lw1 * Bitmap.Height);
          if not IgnoreAlpha then
          begin
            for y := BitmapHeight1 downto 0 do
            begin
              for x := 0 to BitmapWidth1 do
              begin
                if _GetPixelbw(amap, x) <> 0 then
                  AlphaChannel.SetPixel(x, y, 0);
              end;
              inc(amap, lw1);
            end;
          end;
          amap := andmap;
          v2 := EqualRGB(IOParams.CUR_Background, CreateRGB(255, 255, 255));
          for y := BitmapHeight1 downto 0 do
          begin
            pix := Bitmap.Scanline[y];
            for x := 0 to BitmapWidth1 do
            begin
              if _GetPixelbw(amap, x) <> 0 then
              begin
                v1 := _GetPixelbw(pix, x) <> 0;
                if v1 xor v2 then
                  _SetPixelbw(pix, x, 1)
                else
                  _SetPixelbw(pix, x, 0);
              end;
            end;
            inc(amap, lw1);
          end;
        end;
      4:
        begin // 4 bit per pixel
          w := Bitmap.Width shr 1;
          if (Bitmap.Width and 1) <> 0 then
            inc(w);
          getmem(bits, lw * Bitmap.Height);
          fs.Read(bits^, lw * Bitmap.Height);
          fs.Read(andmap^, lw1 * Bitmap.Height);
          bitmapheight1 := Bitmap.Height - 1;
          if not IgnoreAlpha then
          begin
            for y := BitmapHeight1 downto 0 do
            begin
              px := Bitmap.Scanline[y];
              bits2 := bits;
              inc(bits2, lw * (Bitmapheight1 - y));
              for x := 0 to w - 1 do
              begin
                if _GetPixelbw(amap, x * 2) <> 0 then
                  AlphaChannel.SetPixel(x * 2, y, 0);
                px^ := IOParams.ColorMap^[bits2^ shr 4];
                inc(px);
                if _GetPixelbw(amap, x * 2 + 1) <> 0 then
                  AlphaChannel.SetPixel(x * 2 + 1, y, 0);
                if _GetPixelbw(amap, x * 2 + 1) = 0 then
                  px^ := IOParams.ColorMap^[bits2^ and $0F]
                else
                  px^ := IOParams.CUR_Background;
                inc(px);
                inc(bits2);
              end;
              inc(amap, lw1);
            end;
          end
          else
          begin
            for y := BitmapHeight1 downto 0 do
            begin
              px := Bitmap.Scanline[y];
              bits2 := bits;
              inc(bits2, lw * (Bitmapheight1 - y));
              for x := 0 to w - 1 do
              begin
                if _GetPixelbw(amap, x * 2) = 0 then
                  px^ := IOParams.ColorMap^[bits2^ shr 4]
                else
                  px^ := IOParams.CUR_Background;
                inc(px);
                if _GetPixelbw(amap, x * 2 + 1) = 0 then
                  px^ := IOParams.ColorMap^[bits2^ and $0F]
                else
                  px^ := IOParams.CUR_Background;
                inc(px);
                inc(bits2);
              end;
              inc(amap, lw1);
            end;
          end;
          freemem(bits);
        end;
      8:
        begin // 8 bit per pixel
          getmem(bits, lw * Bitmap.height);
          fs.Read(bits^, lw * Bitmap.height);
          fs.Read(andmap^, lw1 * Bitmap.Height);
          bitmapheight1 := Bitmap.Height - 1;
          bitmapwidth1 := Bitmap.Width - 1;
          if not IgnoreAlpha then
          begin
            for y := BitmapHeight1 downto 0 do
            begin
              px := Bitmap.Scanline[y];
              bits2 := bits;
              inc(bits2, lw * (Bitmap.height - 1 - y));
              for x := 0 to BitmapWidth1 do
              begin
                if _GetPixelbw(amap, x) <> 0 then
                  AlphaChannel.SetPixel(x, y, 0);
                if _GetPixelbw(amap, x) = 0 then
                  px^ := IOParams.ColorMap^[bits2^]
                else
                  px^ := IOParams.CUR_Background;
                inc(bits2);
                inc(px);
              end;
              inc(amap, lw1);
            end;
          end
          else
          begin
            for y := BitmapHeight1 downto 0 do
            begin
              px := Bitmap.Scanline[y];
              bits2 := bits;
              inc(bits2, lw * (Bitmapheight1 - y));
              for x := 0 to BitmapWidth1 do
              begin
                if _GetPixelbw(amap, x) = 0 then
                  px^ := IOParams.ColorMap^[bits2^]
                else
                  px^ := IOParams.CUR_Background;
                inc(bits2);
                inc(px);
              end;
              inc(amap, lw1);
            end;
          end;
          freemem(bits);
        end;
      24:
        begin // 24 bit per pixel
          for y := Bitmap.Height - 1 downto 0 do
            fs.read(pbyte(Bitmap.Scanline[y])^, lw);
          if (fs.Size-fs.Position >= lw1 * Bitmap.Height) then
          begin
            fs.Read(andmap^, lw1 * Bitmap.Height);
            amap := andmap;
            for y := Bitmap.Height-1 downto 0 do
            begin
              for x := 0 to Bitmap.width-1 do
              begin
                if _GetPixelbw(amap, x) <> 0 then
                  AlphaChannel.SetPixel(x, y, 0)
                else
                  AlphaChannel.SetPixel(x, y, 255);
              end;
              inc(amap, lw1);
            end;
          end;
        end;
      32:
        begin // 32 bit
          getmem(rgba, sizeof(TRGBA) * Bitmap.Width);
          try
            allzero := true;
            for y := Bitmap.Height - 1 downto 0 do
            begin
              fs.read(rgba^, sizeof(TRGBA) * Bitmap.Width);
              rgb := Bitmap.Scanline[y];
              amap := AlphaChannel.Scanline[y];
              xrgba := rgba;
              for x := 0 to Bitmap.Width - 1 do
              begin
                rgb^.r := xrgba^.r;
                rgb^.g := xrgba^.g;
                rgb^.b := xrgba^.b;
                amap^  := xrgba^.a;
                if amap^>0 then
                  allzero := false;
                inc(rgb);
                inc(xrgba);
                inc(amap);
              end;
            end;
          finally
            freemem(rgba);
          end;

          if (fs.Size-fs.Position >= lw1 * Bitmap.Height) and allzero then
          begin
            fs.Read(andmap^, lw1 * Bitmap.Height);
            amap := andmap;
            for y := Bitmap.Height-1 downto 0 do
            begin
              for x := 0 to Bitmap.width-1 do
              begin
                if _GetPixelbw(amap, x) <> 0 then
                  AlphaChannel.SetPixel(x, y, 0)
                else
                  AlphaChannel.SetPixel(x, y, 255);
              end;
              inc(amap, lw1);
            end;
          end;

        end;
    end; // case
  finally
    freemem(andmap);
  end;
{.$WARNINGS ON}
end;

// CUR reading
///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////





///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////

// bypass separators and return an item (an item can also be a comment)
// in the comments re-insert a # character

function PBMReadItem(fs: TStream): AnsiString;
var
  c: AnsiChar;
begin
  result := '';
  repeat
    if fs.Read(c, 1) < 1 then
      break;
    if c = '#' then
    begin
      // comment
      repeat
        if fs.Read(c, 1) < 1 then
          break;
        if (c = #$0A) or (c = #$0D) then
          break;
        result := result + c;
      until false;
      result := '#' + IETrim(result);
      break;
    end
    else
    if ((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z')) then
    begin
      // item
      result := result + c;
      repeat
        if fs.Read(c, 1) < 1 then
          break;
        if ((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z')) then
          result := result + c
        else
          break;
      until false;
      break;
    end;
  until false;
end;

// return true if the stream is PXM

function TryPXM(fs: TStream): boolean;
var
  ss: AnsiString;
  magic: AnsiString;
  width, height, vmax: integer;
  x: integer;
  sp: int64;
begin
  sp := fs.Position;
  x := 0;
  magic := '';
  width := 0;
  height := 0;
  vmax := 0;
  repeat
    ss := PBMReadItem(fs);
    if IECopy(ss, 1, 1) = '#' then
      ss := ss
    else
    if magic = '' then
    begin
      magic := ss;
      if (magic = 'P1') or (magic = 'P4') then
        vmax := 1;
      if (magic <> 'P1') and (magic <> 'P2') and (magic <> 'P3') and (magic <> 'P4') and
        (magic <> 'P5') and (magic <> 'P6') then
        break;
    end
    else
    if width = 0 then
      width := IEStrToIntDef(ss, -1)
    else
    if height = 0 then
    begin
      height := IEStrToIntDef(ss, -1);
      if vmax > 0 then
        break;
    end
    else
    if vmax = 0 then
    begin
      break;
    end;
    inc(x);
  until x > 500;
  fs.Position := sp;
  result := (width > 0) and (height > 0);
end;

procedure PXMReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
var
  ss: AnsiString;
  magic: AnsiString;
  width, height, vmax: integer;
  x, y, rl: integer;
  buf, p: pbyte;
  wbuf, wp, p16: pword;
  rgb: PRGB;
  rgb48: PRGB48;
  c: AnsiChar;
  bb: byte;
  fsSize: integer;
begin
  magic := '';
  width := 0;
  height := 0;
  vmax := 0;
  IOParams.PXM_Comments.Clear;
  fsSize := fs.Size;
  repeat
    ss := PBMReadItem(fs);
    if IECopy(ss, 1, 1) = '#' then
      IOParams.PXM_Comments.Add(string(IECopy(ss, 2, length(ss))))
    else
    if magic = '' then
    begin
      magic := ss;
      if (magic = 'P1') or (magic = 'P4') then
        vmax := 1;
    end
    else
    if width = 0 then
      width := IEStrToIntDef(ss, -1)
    else
    if height = 0 then
    begin
      height := IEStrToIntDef(ss, -1);
      if vmax > 0 then
        break;
    end
    else
    if vmax = 0 then
    begin
      vmax := IEStrToIntDef(ss, -1);
      break;
    end;
  until fs.Position >= fsSize;
  if (width < 1) or (height < 1) then
  begin
    Progress.Aborting^ := true;
    exit;
  end;
  // if present remove old colormap
  IOParams.FreeColorMap;
  //
  IOParams.Width  := width;
  IOParams.Height := height;
  IOParams.OriginalWidth  := width;
  IOParams.OriginalHeight := height;
  IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
  IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
  //
  Progress.per1 := 100 / height;
  Progress.val := 0;
  if magic = 'P1' then
  begin
    // PBM (1bit) - ASCII
    IOParams.BitsPerSample := 1;
    IOParams.SamplesPerPixel := 1;
    if Preview then
      exit;
    bitmap.Allocate(Width, Height, ie1g);
    x := 0;
    y := 0;
    p := bitmap.scanline[0];
    repeat
      if fs.Read(c, 1) < 1 then
        break;
      if c = '0' then
      begin
        _SetPixelbw(p, x, 1);
        inc(x);
      end
      else
      if c = '1' then
      begin
        _SetPixelbw(p, x, 0);
        inc(x);
      end;
      if x = width then
      begin
        x := 0;
        inc(y);
        if y = height then
          break;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
        p := bitmap.scanline[y];
      end;
    until false;
  end
  else
  if magic = 'P2' then
  begin
    // PGM (8bit) - ASCII
    IOParams.BitsPerSample := 8;
    IOParams.SamplesPerPixel := 1;
    if Preview then
      exit;
    bitmap.Allocate(Width, Height, ie24RGB);
    for y := 0 to height - 1 do
    begin
      rgb := bitmap.scanline[y];
      for x := 0 to width - 1 do
      begin
        ss := PBMReadItem(fs);
        bb := trunc((IEStrToIntDef(ss, 0) / vmax) * 255);
        with rgb^ do
        begin
          r := bb;
          g := bb;
          b := bb;
        end;
        inc(rgb);
      end;
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
  end
  else
  if magic = 'P3' then
  begin
    // PPM (24bit) - ASCII
    IOParams.BitsPerSample := 8;
    IOParams.SamplesPerPixel := 3;
    if Preview then
      exit;
    bitmap.Allocate(Width, Height, ie24RGB);
    for y := 0 to height - 1 do
    begin
      rgb := bitmap.scanline[y];
      for x := 0 to width - 1 do
      begin
        with rgb^ do
        begin
          ss := PBMReadItem(fs);
          r := trunc((IEStrToIntDef(ss, 0) / vmax) * 255);
          ss := PBMReadItem(fs);
          g := trunc((IEStrToIntDef(ss, 0) / vmax) * 255);
          ss := PBMReadItem(fs);
          b := trunc((IEStrToIntDef(ss, 0) / vmax) * 255);
        end;
        inc(rgb);
      end;
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
  end
  else
  if magic = 'P4' then
  begin
    // PBM (1bit) - RAWBITS
    IOParams.BitsPerSample := 1;
    IOParams.SamplesPerPixel := 1;
    if Preview then
      exit;
    bitmap.Allocate(Width, Height, ie1g);
    rl := width shr 3;
    if (width and $7) <> 0 then
      inc(rl);
    for y := 0 to height - 1 do
    begin
      p := bitmap.scanline[y];
      fs.Read(p^, rl);
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
    _Negative1BitEx(Bitmap);
  end
  else
  if (magic = 'P5') and (vmax<=255) then // 3.0.4
  begin
    // PGM (8bit) - RAWBITS
    IOParams.BitsPerSample := 8;
    IOParams.SamplesPerPixel := 1;
    if Preview then
      exit;
    bitmap.Allocate(Width, Height, ie24RGB);
    getmem(buf, width);
    for y := 0 to height - 1 do
    begin
      rgb := bitmap.scanline[y];
      p := buf;
      fs.Read(p^, width);
      if vmax < 255 then
        for x := 0 to width - 1 do
        begin
          p^ := trunc((p^ / vmax) * 255);
          inc(p); // 3.0.4
        end;
      for x := 0 to width - 1 do
      begin
        with rgb^ do
        begin
          r := p^;
          g := p^;
          b := p^;
        end;
        inc(rgb);
        inc(p);
      end;
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
    freemem(buf);
  end
  else
  if (magic = 'P5') and (vmax<=65535) then // 3.0.4
  begin
    // PGM (16bit) - RAWBITS
    IOParams.BitsPerSample := 16;
    IOParams.SamplesPerPixel := 1;
    if Preview then
      exit;
    if IOParams.IsNativePixelFormat then
    begin
      bitmap.Allocate(Width, Height, ie16g);
      for y := 0 to height - 1 do
      begin
        p16 := bitmap.scanline[y];
        fs.Read(p16^, width*2);
        for x := 0 to width - 1 do
        begin
          p16^ := trunc((IESwapWord(p16^) / vmax) * 65535);
          inc(p16);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
      end;
    end
    else
    begin
      bitmap.Allocate(Width, Height, ie24RGB);
      getmem(wbuf, width*2);
      for y := 0 to height - 1 do
      begin
        rgb := bitmap.scanline[y];
        wp := wbuf;
        fs.Read(wp^, width*2);
        for x := 0 to width - 1 do
        begin
          wp^ := trunc((IESwapWord(wp^) / vmax) * 255);
          inc(wp);
        end;
        wp := wbuf;
        for x := 0 to width - 1 do
        begin
          with rgb^ do
          begin
            r := wp^;
            g := wp^;
            b := wp^;
          end;
          inc(rgb);
          inc(wp);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
      end;
      freemem(wbuf);
    end;
  end
  else
  if (magic = 'P6') and (vmax<256) then
  begin
    // PPM (24bit) - RAWBITS
    IOParams.BitsPerSample := 8;
    IOParams.SamplesPerPixel := 3;
    if Preview then
      exit;
    bitmap.Allocate(Width, Height, ie24RGB);
    for y := 0 to height - 1 do
    begin
      rgb := bitmap.scanline[y];
      fs.Read(rgb^, width * 3);
      for x := 0 to width - 1 do
      begin
        bswap(rgb^.r, rgb^.b);
        inc(rgb);
      end;
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
  end
  else
  if (magic = 'P6') and (vmax<65536) then
  begin
    // PPM (48bit) - RAWBITS
    IOParams.BitsPerSample := 16;
    IOParams.SamplesPerPixel := 3;
    if Preview then
      exit;
    if IOParams.IsNativePixelFormat then
    begin
      // native pixel format
      bitmap.Allocate(Width, Height, ie48RGB);
      for y := 0 to height - 1 do
      begin
        rgb48 := bitmap.scanline[y];
        fs.Read(rgb48^, width * 6);
        for x := 0 to width - 1 do
        begin
          with rgb48^ do
          begin
            r := IESwapWord(r);
            g := IESwapWord(g);
            b := IESwapWord(b);
          end;
          inc(rgb48);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
      end;
    end
    else
    begin
      // convert 48 bit to 24 bit
      bitmap.Allocate(Width, Height, ie24RGB);
      getmem(buf, width*6);
      for y := 0 to height - 1 do
      begin
        rgb := bitmap.scanline[y];
        fs.Read(buf^, width * 6);
        rgb48 := PRGB48(buf);
        for x := 0 to width - 1 do
        begin
          with rgb48^ do
          begin
            rgb^.r := IESwapWord(r) shr 8;
            rgb^.g := IESwapWord(g) shr 8;
            rgb^.b := IESwapWord(b) shr 8;
          end;
          inc(rgb);
          inc(rgb48);
        end;
        // OnProgress
        with Progress do
        begin
          inc(val);
          if assigned(fOnProgress) then
            fOnProgress(Sender, trunc(per1 * val));
        end;
      end;
      freemem(buf);
    end;
  end
  else
  begin
    Progress.Aborting^ := true;
    exit;
  end;
end;

// writes only rawbits (P4, P5, P6)

procedure PXMWriteStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  rl, width, height: integer;
  y, x, q: integer;
  ss, cm: AnsiString;
  p, buf: pbyte;
  px, xbuf: PRGB;
  px48, dx48: PRGB48;
begin
  width := Bitmap.Width;
  height := Bitmap.Height;
  Progress.per1 := 100 / height;
  Progress.val := 0;
  // comments
  cm := '';
  for q := 0 to IOParams.PXM_Comments.Count - 1 do
    cm := #$0A'# ' + AnsiString(IOParams.PXM_Comments[q]) + #$0A;
  //
  if ((IOParams.BitsPerSample = 1) and (IOParams.SamplesPerPixel = 1)) or (Bitmap.PixelFormat = ie1g) then
  begin
    // P4 (PBM - 1 bit - RAW)
    ss := 'P4 ' + cm + IEIntToStr(width) + ' ' + IEIntToStr(height) + #$0A;
    SafeStreamWrite(fs, Progress.Aborting^, ss[1], length(ss));
    rl := width shr 3;
    if (width and $7) <> 0 then
      inc(rl);
    getmem(buf, rl);
    for y := 0 to height - 1 do
    begin
      copymemory(buf, bitmap.scanline[y], rl);
      p := buf;
      for x := 0 to rl - 1 do
      begin
        p^ := not p^;
        inc(p);
      end;
      SafeStreamWrite(fs, Progress.Aborting^, buf^, rl);
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
    freemem(buf);
  end
  else
  if (IOParams.BitsPerSample = 8) and (IOParams.SamplesPerPixel = 1) then
  begin
    // P5 (PGM 8 bit - RAWBITS)
    ss := 'P5 ' + cm + IEIntToStr(width) + ' ' + IEIntToStr(height) + ' 255' + #$0A;
    SafeStreamWrite(fs, Progress.Aborting^, ss[1], length(ss));
    getmem(buf, width);
    for y := 0 to height - 1 do
    begin
      px := bitmap.scanline[y];
      p := buf;
      IEDefaultConvertColorFunction(px, iecmsBGR, p, iecmsGray8, width, IOParams);
      SafeStreamWrite(fs, Progress.Aborting^, buf^, width);
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
    freemem(buf);
  end
  else
  if (IOParams.BitsPerSample=16) and (IOParams.SamplesPerPixel=3) and (Bitmap.PixelFormat=ie24RGB) then
  begin
    // P6 (PPM 48 bit - RAWBITS) - converting from ie24RGB
    ss := 'P6 ' + cm + IEIntToStr(width) + ' ' + IEIntToStr(height) + ' 65535' + #$0A;
    SafeStreamWrite(fs, Progress.Aborting^, ss[1], length(ss));
    getmem(xbuf, width * 6);
    for y := 0 to height - 1 do
    begin
      px48 := PRGB48(xbuf);
      px := bitmap.Scanline[y];
      for x := 0 to width - 1 do
      begin
        px48^.r := IESwapWord(px^.r *257);
        px48^.g := IESwapWord(px^.g *257);
        px48^.b := IESwapWord(px^.b *257);
        inc(px48);
        inc(px);
      end;
      SafeStreamWrite(fs, Progress.Aborting^, xbuf^, width * 6);
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
    freemem(xbuf);
  end
  else
  if (Bitmap.PixelFormat=ie48RGB) then
  begin
    // P6 (PPM 48 bit - RAWBITS)
    ss := 'P6 ' + cm + IEIntToStr(width) + ' ' + IEIntToStr(height) + ' 65535' + #$0A;
    SafeStreamWrite(fs, Progress.Aborting^, ss[1], length(ss));
    getmem(xbuf, width*6);
    for y := 0 to height - 1 do
    begin
      px48 := bitmap.scanline[y];
      dx48 := PRGB48(xbuf);
      for x := 0 to width-1 do
      begin
        dx48^.r := IESwapWord(px48^.r);
        dx48^.g := IESwapWord(px48^.g);
        dx48^.b := IESwapWord(px48^.b);
        inc(px48);
        inc(dx48);
      end;
      SafeStreamWrite(fs, Progress.Aborting^, xbuf^, width * 6);
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
    freemem(xbuf);
  end
  else
  if ((IOParams.BitsPerSample = 8) and (IOParams.SamplesPerPixel = 3)) or (Bitmap.PixelFormat = ie24RGB) then
  begin
    // P6 (PPM 24 bit - RAWBITS)
    ss := 'P6 ' + cm + IEIntToStr(width) + ' ' + IEIntToStr(height) + ' 255' + #$0A;
    SafeStreamWrite(fs, Progress.Aborting^, ss[1], length(ss));
    getmem(xbuf, width * 3);
    for y := 0 to height - 1 do
    begin
      copymemory(xbuf, bitmap.scanline[y], width * 3);
      px := xbuf;
      for x := 0 to width - 1 do
      begin
        bswap(px^.r, px^.b);
        inc(px);
      end;
      SafeStreamWrite(fs, Progress.Aborting^, xbuf^, width * 3);
      // OnProgress
      with Progress do
      begin
        inc(val);
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * val));
      end;
    end;
    freemem(xbuf);
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// WBMP

type
  TWBMPHeader = packed record
    typeField: byte;
    EndOfExt: byte;
    Width: byte;
    Height: byte;
  end;

function ParseUINTVar(AStream : TStream) : Cardinal;
var
  UintVar      : Cardinal;
  WillContinue : Boolean;
  B            : Byte;
  I            : Integer;
begin
  i := 1;
  UintVar := 0;
  with AStream do
  repeat
    Read(b, 1);
    if (b and $80) = $80 then
      WillContinue := True
    else
      WillContinue := False;
    UintVar := (UintVar shl 7) + (b and $7f);
    inc(i);
  until (not WillContinue) or (i > 4);
  Result := UintVar;
end;

// only type 0, uncompressed, black/white, 255x255

procedure WBMPReadStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
var
  header: TWBMPHeader;
  y, ll: integer;
  px: pbyte;
begin
  // read header
  header.typeField := ParseUINTVar(fs);
  header.EndOfExt := ParseUINTVar(fs);
  header.Width := ParseUINTVar(fs);
  header.Height := ParseUINTVar(fs);
  if (header.typeField <> 0) or (header.EndOfExt <> 0) or (header.Width =0) or (header.Height =0) then
  begin
    Progress.Aborting^ := true;
    exit;
  end;
  // assign parameters
  IOParams.BitsPerSample := 1;
  IOParams.SamplesPerPixel := 1;
  IOParams.Width  := header.Width;
  IOParams.Height := header.Height;
  IOParams.OriginalWidth  := header.Width;
  IOParams.OriginalHeight := header.Height;
  IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
  IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
  // if present remove old colormap
  IOParams.FreeColorMap;
  // load bitmap
  if not Preview then
  begin
    Progress.per1 := 100 / header.height;
    Bitmap.Allocate(header.Width, header.Height, ie1g);
    ll := header.Width div 8;
    if (header.Width and $7) <> 0 then
      inc(ll);
    for y := 0 to Bitmap.Height - 1 do
    begin
      px := Bitmap.Scanline[y];
      fs.Read(px^, ll);
      // OnProgress
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * y));
    end;
  end;
end;

// only type 0, uncompressed, black/white

procedure WBMPWriteStream(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  header: TWBMPHeader;
  wbitmap: TIEBitmap;
  y, ll: integer;
  px: pbyte;
begin
  // convert to ie1g
  if Bitmap.PixelFormat <> ie1g then
  begin
    wbitmap := TIEBitmap.Create;
    wbitmap.Assign(Bitmap);
    wbitmap.PixelFormat := ie1g;
  end
  else
    wbitmap := Bitmap;
  // resize
  if (wbitmap.Width > 255) or (wbitmap.Height > 255) then
  begin
    if wbitmap.Width > wbitmap.Height then
      wbitmap.Resize(255, trunc(wbitmap.Height * 255 / wbitmap.Width), 0, 255, iehLeft, ievTop)
    else
      wbitmap.Resize(trunc(wbitmap.Width * 255 / wbitmap.Height), 255, 0, 255, iehLeft, ievTop);
  end;
  // write header
  header.typeField := 0;
  header.EndOfExt := 0;
  header.Width := wbitmap.Width;
  header.Height := wbitmap.Height;
  fs.Write(header, sizeof(TWBMPHeader));
  // write image
  Progress.per1 := 100 / wbitmap.height;
  ll := header.Width div 8;
  if (header.Width and $7) <> 0 then
    inc(ll);
  for y := 0 to wbitmap.Height do
  begin
    px := wbitmap.Scanline[y];
    fs.Write(px^, ll);
    // OnProgress
    with Progress do
      if assigned(fOnProgress) then
        fOnProgress(Sender, trunc(per1 * y));
  end;
  // free wbitmap
  if wbitmap <> Bitmap then
    FreeAndNil(wbitmap);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PostScript (PS)

// Write an image (this isn't a page or a full PS!)
// returns ASCII characters wrote

function IEPostScriptWriteImage(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec): integer;
var
  px: pbyte;
  row, pb: pbyte;
  pc: PAnsiChar;
  compressedrow: pbyte;
  asciiout: PAnsiChar;
  i, l: integer;
  asciilen: integer;
  ms: tmemorystream;
  procedure WriteStrip24(offset: integer);
  var
    j: integer;
    bitmapWidth: integer;
  begin
    px := Bitmap.scanline[i];
    inc(pbyte(px), offset);
    pb := row;
    bitmapWidth := bitmap.Width;
    for j := 0 to bitmapWidth - 1 do
    begin
      pb^ := px^;
      inc(pb);
      inc(px, 3);
    end;
    l := IEPSRunLengthEncode(PByteArray(row), bitmap.width, PByteArray(compressedrow));
    pb := compressedrow;
    pc := asciiout;
    l := IEASCII85EncodeBlock(pb, l, pc, asciilen);
    fs.Write(asciiout^, l);
  end;
  procedure Write1;
  var
    j: integer;
  begin
    pb := row;
    l := bitmap.width div 8;
    if (bitmap.width and 7) <> 0 then
      inc(l);
    for j := 0 to bitmap.height - 1 do
    begin
      copymemory(pb, bitmap.scanline[j], l);
      inc(pb, l);
      // OnProgress
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * j));
    end;
    l := IEPSRunLengthEncode(pbytearray(row), l * bitmap.height, pbytearray(compressedrow));
    pb := compressedrow;
    pc := asciiout;
    l := IEASCII85EncodeBlock(pb, l, pc, asciilen);
    fs.Write(asciiout^, l);
  end;
  procedure Write1CCITT;
  var
    ms: tmemorystream;
    j: integer;
    bwr: byte;
    bwrl: integer;
    predline: pbyte;
  begin
    ms := tmemorystream.create;
    bwrl := 0;
    bwr := 0;
    predline := nil;
    for j := 0 to bitmap.height - 1 do
    begin
      case IOParams.PS_Compression of
        ioPS_G4FAX:
          CCITTHuffmanPutLineG4(pbyte(Bitmap.Scanline[j]), Bitmap.Width, ms, bwr, bwrl, predline, Progress.Aborting^, 1);
        ioPS_G3FAX2D:
          CCITTHuffmanPutLineG32D(pbyte(Bitmap.Scanline[j]), Bitmap.Width, ms, bwr, bwrl, predline, Progress.Aborting^, 1);
      end;
    end;
    case IOParams.PS_Compression of
      ioPS_G4FAX:
        CCITTHuffmanPutLineG4(nil, 0, ms, bwr, bwrl, predline, Progress.Aborting^, 1);
      ioPS_G3FAX2D:
        CCITTHuffmanPutLineG32D(nil, 0, ms, bwr, bwrl, predline, Progress.Aborting^, 1);
    end;
    l := ms.Size;
    // round to 4 bytes (not really, but set to zero other bytes)
    bwr := 0;
    while (ms.size mod 4) <> 0 do
      ms.Write(bwr, 1);
    //
    pb := ms.Memory;
    pc := asciiout;
    l := IEASCII85EncodeBlock(pb, l, pc, asciilen);
    fs.Write(asciiout^, l);
    FreeAndNil(ms);
  end;
begin
  asciilen := 0;
  Progress.per1 := 100 / bitmap.height;

  if (ioparams.SamplesPerPixel = 1) and (ioparams.BitsPerSample > 1) then
  begin
    ioparams.SamplesPerPixel := 3;
    ioparams.BitsPerSample := 8;
  end;
  if ioparams.SamplesPerPixel > 3 then
    ioparams.SamplesPerPixel := 3;

  case ioparams.SamplesPerPixel of
    1:
      begin
        case ioparams.BitsPerSample of
          1:
            begin
              // black/white
              l := Bitmap.RowLen * 2 * bitmap.height;
              getmem(compressedrow, l);
              getmem(asciiout, l * 2);
              getmem(row, l);

              if (IOParams.PS_Compression = ioPS_RLE) then
                Write1 // uncompressed or rle
              else
                Write1CCITT; // all CCITT

              freemem(asciiout);
              freemem(row);
              freemem(compressedrow);
            end;
          8:
            begin
              // gray scale
            end;
        end;
      end;
    3:
      begin
        if IOParams.PS_Compression = ioPS_JPEG then
        begin
          ms := TMemoryStream.Create;
          WriteJpegStream(ms, Bitmap, IOParams, Progress);
          getmem(asciiout, l * 2);
          pb := ms.Memory;
          pc := asciiout;
          l := ms.Size;
          l := IEASCII85EncodeBlock(pb, l, pc, asciilen);
          FreeAndNil(ms);
          fs.Write(asciiout^, l);
          freemem(asciiout);
        end
        else
        begin
          // RLW
          getmem(compressedrow, Bitmap.RowLen * 4);
          getmem(asciiout, bitmap.width * 4);
          getmem(row, bitmap.width + 4);
          for i := 0 to bitmap.height - 1 do
          begin
            WriteStrip24(2); // red
            WriteStrip24(1); // green
            WriteStrip24(0); // blue
            // OnProgress
            with Progress do
              if assigned(fOnProgress) then
                fOnProgress(Sender, trunc(per1 * i));
          end;
          freemem(asciiout);
          freemem(row);
          freemem(compressedrow);
        end;
      end;
  end;

  result := asciilen;
end;

// write a single page with a single image (this is not a full PS, but a just a page)
// for A4 PaperWidth=595, PaperHeight=842

procedure IEPostScriptWritePage(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Page: integer; PaperWidth, PaperHeight: integer);
var
  ms: tmemorystream;
  asciilen: integer;
  w, h: integer;
begin
  IEWriteStrLn(fs, '%%Page: ' + IEIntToStr(Page) + ' ' + IEIntToStr(Page));
  IEWriteStrLn(fs, '0 ' + IEIntToStr(PaperHeight) + ' translate');
  IEGetFitResampleSize(bitmap.Width, bitmap.height, PaperWidth, PaperHeight, w, h);
  IEWriteStrLn(fs, IEIntToStr(w) + ' -' + IEIntToStr(h) + ' scale');
  IEWriteStrLn(fs, IEIntToStr(bitmap.width) + ' ' + IEIntToStr(bitmap.height) + ' ' + IEIntToStr(ioparams.BitsPerSample));
  IEWriteStrLn(fs, '[ ' + IEIntToStr(bitmap.width) + ' 0 0 ' + IEIntToStr(bitmap.height) + ' 0 0 ]');

  if Bitmap.PixelFormat=ie1g then
  begin
    IOParams.BitsPerSample := 1;
    IOParams.SamplesPerPixel := 1;
  end
  else
  if Bitmap.PixelFormat=ie24RGB then
  begin
    IOParams.BitsPerSample := 8;
    IOParams.SamplesPerPixel := 3;
  end;

  ms := tmemorystream.create;
  ms.size := 8*1024*1024;
  asciilen := IEPostScriptWriteImage(ms, Bitmap, IOParams, Progress);
  ms.size := ms.position;

  if ioparams.SamplesPerPixel = 3 then
  begin
    // color image
    if ioparams.PS_Compression = ioPS_JPEG then
    begin
      IEWriteStrLn(fs, 'currentfile /ASCII85Decode filter << >> /DCTDecode filter');
      IEWriteStrLn(fs, 'false 3');
      IEWriteStrLn(fs, '%%BeginData:      ' + IEIntToStr(asciilen) + ' ASCII Bytes');
      IEWriteStrLn(fs, 'colorimage');
    end
    else
    begin
      IEWriteStrLn(fs, '/rstr ' + IEIntToStr(bitmap.width) + ' string def');
      IEWriteStrLn(fs, '/gstr ' + IEIntToStr(bitmap.width) + ' string def');
      IEWriteStrLn(fs, '/bstr ' + IEIntToStr(bitmap.width) + ' string def');
      IEWriteStrLn(fs, '{currentfile /ASCII85Decode filter /RunLengthDecode filter rstr readstring pop}');
      IEWriteStrLn(fs, '{currentfile /ASCII85Decode filter /RunLengthDecode filter gstr readstring pop}');
      IEWriteStrLn(fs, '{currentfile /ASCII85Decode filter /RunLengthDecode filter bstr readstring pop}');
      IEWriteStrLn(fs, 'true 3');
      IEWriteStrLn(fs, '%%BeginData:      ' + IEIntToStr(asciilen) + ' ASCII Bytes');
      IEWriteStrLn(fs, 'colorimage');
    end;
  end
  else
  begin
    // black/white image
    case IOParams.PS_Compression of
      ioPS_RLE:
        IEWriteStrLn(fs, 'currentfile /ASCII85Decode filter /RunLengthDecode filter');
      ioPS_G4FAX:
        IEWriteStrLn(fs, 'currentfile /ASCII85Decode filter << /K -1 /BlackIs1 true /Columns ' + IEIntToStr(bitmap.width) + ' /Rows ' + IEIntToStr(bitmap.height) + ' /EndOfLine false /Uncompressed true /EncodedByteAlign false /EndOfBlock false >> /CCITTFaxDecode filter');
      ioPS_G3FAX2D:
        IEWriteStrLn(fs, 'currentfile /ASCII85Decode filter << /K 1 /BlackIs1 true /Columns ' + IEIntToStr(bitmap.width) + ' /Rows ' + IEIntToStr(bitmap.height) + ' /EndOfLine false /Uncompressed true /EncodedByteAlign false /EndOfBlock false >> /CCITTFaxDecode filter');
    end;
    IEWriteStrLn(fs, '%%BeginData:      ' + IEIntToStr(asciilen) + ' ASCII Bytes');
    IEWriteStrLn(fs, 'image');
  end;
  IECopyFrom(fs, ms, 0);
  FreeAndNil(ms);

  IEWriteStrLn(fs, '%%EndData');
  IEWriteStrLn(fs, 'showpage');
end;

type
  TPSRec = record
    pagpos: integer; // position of %%Pages tag
    wripos: integer; // position of next byte to write
  end;
  PPSRec = ^TPSRec;

function padr(v: integer; l: integer): AnsiString;
begin
  result := IEIntToStr(v);
  while length(result) < l do
    result := result + ' ';
end;

function IEPostScriptCreate(fs: TStream; var IOParams: TIOParamsVals): pointer;
var
  sr: PPSRec;
begin
  new(sr);
  result := sr;
  IEWriteStrLn(fs, '%!PS-Adobe-3.0');
  IEWriteStrLn(fs, '%%Creator: ImageEn');
  IEWriteStrLn(fs, '%%Title: ' + IOParams.PS_Title);
  IEWriteStrLn(fs, '%%CreationDate: ' + AnsiString(formatdatetime('ddd mmm dd hh:nn:ss yyyy', date + time)));
  IEWriteStrLn(fs, '%%DocumentData: Clean7Bit');
  IEWriteStrLn(fs, '%%LanguageLevel: 2');
  sr^.pagpos := fs.Position;
  IEWriteStrLn(fs, '%%Pages: ' + padr(0, 6));
  IEWriteStrLn(fs, '%%BoundingBox: 0 0 ' + IEIntToStr(IOParams.PS_PaperWidth - 1) + ' ' + IEIntToStr(IOParams.PS_PaperHeight - 1));
  IEWriteStrLn(fs, '%%EndComments');
  //IEWriteStrLn(fs, '%%BeginProlog');  // 3.0.3
  //IEWriteStrLn(fs, '10 dict begin');  // 3.0.3
  //IEWriteStrLn(fs, '%%EndProlog');    // 3.0.3
  sr^.wripos := fs.Position;
end;

procedure IEPostScriptClose(handle: pointer; fs: TStream);
var
  sr: PPSRec;
begin
  if handle <> nil then
  begin
    sr := PPSRec(handle);
    fs.Position := sr^.wripos;
    IEWriteStrLn(fs, '%%Trailer');
    //IEWriteStrLn(fs, 'end');  // 3.0.3
    IEWriteStrLn(fs, '%%EOF');
    dispose(sr);
  end;
end;

// %%Pages: 000000
// 123456789012345

procedure IEPostScriptSave(handle: pointer; fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  i: integer;
  sr: PPSRec;
  ss: AnsiString;
begin
  sr := PPSRec(handle);
  // increase page number
  fs.Position := sr^.pagpos;
  SetLength(ss, 15);
  fs.Read(ss[1], 15);
  i := IEStrToIntDef(IETrim(IECopy(ss, 10, 5)), 1) + 1;
  fs.Position := sr^.pagpos;
  IEWriteStrLn(fs, '%%Pages: ' + padr(i, 6));
  // write page
  fs.Position := sr^.wripos;
  IEPostScriptWritePage(fs, Bitmap, IOParams, Progress, i, IOParams.PS_PaperWidth, IOParams.PS_PaperHeight);
  sr^.wripos := fs.Position;
end;

// write a full PS with a single image

procedure IEPostScriptSaveOneStep(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  h: pointer;
begin
  h := IEPostScriptCreate(fs, IOParams);
  IEPostScriptSave(h, fs, Bitmap, IOParams, Progress);
  IEPostScriptClose(h, fs);
end;

// End of PostScript
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// PDF

{$ifdef IEINCLUDEPDFWRITING}

type
  TPDFRec = record
    objs: TList;
    pages: TList;
    images: TList;
    contents: TList;
    resources: TIEPDFDictionaryObject;
    catalog: TIEPDFDictionaryObject;
  end;
  PPDFRec = ^TPDFRec;

function IEPDFCreate(var IOParams: TIOParamsVals): pointer;
var
  rec: PPDFRec;
  ar1: TIEPDFArrayObject;
begin
  new(rec);
  result := rec;
  with rec^ do
  begin

    objs := TList.Create;
    pages := TList.Create;
    images := TList.Create;
    contents := TList.Create;

    catalog := iepdf_AddCatalog(objs);

    // resources
    resources := TIEPDFDictionaryObject.Create;
    resources.DontFree := true;
    ar1 := TIEPDFArrayObject.Create;
    ar1.Items.Add(TIEPDFNameObject.Create('PDF'));
    ar1.Items.Add(TIEPDFNameObject.Create('ImageB'));
    resources.items.AddObject('ProcSet', ar1);
    resources.items.AddObject('XObject', TIEPDFDictionaryObject.Create);
    iepdf_AddIndirectObject(objs, resources);

  end;
end;

procedure IEPDFSave(handle: pointer; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  rec: PPDFRec;
  image: TIEPDFStreamObject;
  i, rl, l: integer;
  pd: pbyte;
  contentstr: AnsiString;
  content: TIEPDFStreamObject;
  di1, di2: TIEPDFDictionaryObject;
  re1: TIEPDFRefObject;
  w, h: integer;
  pagenum: integer;
  buf1, buf2: pbyte;
  ar1: TIEPDFArrayObject;
  ms: tmemorystream;
  bwr: byte;
  bwrl: integer;
  predline: pbyte;
  mediabox: TIEPDFArrayObject;
  lzwid: pointer;
  procedure CCITTCommonCodeBegin;
  begin
    ms := tmemorystream.create;
    bwrl := 0;
    bwr := 0;
    predline := nil;
  end;
  procedure CCITTCommonCodeEnd(K: integer);
  begin
    l := ms.Size;
    // round to 4 bytes
    bwr := 0;
    while (ms.size mod 4) <> 0 do
      ms.Write(bwr, 1);
    image := TIEPDFStreamObject.CreateCopy(ms.Memory, l);
    FreeAndNil(ms);
    image.dict.items.Addobject('Filter', TIEPDFNameObject.Create('CCITTFaxDecode'));
    di2 := TIEPDFDictionaryObject.Create;
    di2.items.AddObject('K', TIEPDFNumericObject.Create(K));
    di2.items.AddObject('BlackIs1', TIEPDFBooleanObject.Create(true));
    di2.items.AddObject('Columns', TIEPDFNumericObject.Create(bitmap.width));
    di2.items.AddObject('Rows', TIEPDFNumericObject.Create(bitmap.height));
    di2.items.AddObject('EndOfLine', TIEPDFBooleanObject.Create(false));
    di2.items.AddObject('Uncompressed', TIEPDFBooleanObject.Create(true));
    di2.items.AddObject('EncodedByteAlign', TIEPDFBooleanObject.Create(false));
    di2.items.AddObject('EndOfBlock', TIEPDFBooleanObject.Create(false));
    image.dict.items.AddObject('DecodeParms', di2);
  end;
begin
  rec := PPDFRec(handle);
  with rec^ do
  begin
    // image stream
    image := nil;
    if Bitmap.PixelFormat=ie1g then
    begin
      IOParams.BitsPerSample := 1;
      IOParams.SamplesPerPixel := 1;
    end
    else
    if Bitmap.PixelFormat=ie24RGB then
    begin
      IOParams.BitsPerSample := 8;
      IOParams.SamplesPerPixel := 3;
    end;
    if (IOParams.BitsPerSample = 1) and (IOParams.SamplesPerPixel = 1) then
    begin
      // black/white
      rl := bitmap.Width div 8;
      if (bitmap.Width and 7) <> 0 then
        inc(rl);
      if IOParams.PDF_Compression=ioPDF_JPEG then
        IOParams.PDF_Compression := ioPDF_G3FAX2D;
      case IOParams.PDF_Compression of
        ioPDF_UNCOMPRESSED:
          begin
            image := TIEPDFStreamObject.Create;
            image.length := rl * bitmap.height;
            getmem(image.data, image.length);
            pd := image.data;
            for i := 0 to bitmap.height - 1 do
            begin
              copymemory(pd, bitmap.scanline[i], rl);
              inc(pd, rl);
            end;
          end;
        ioPDF_RLE:
          begin
            getmem(buf1, rl * bitmap.height);
            pd := buf1;
            for i := 0 to bitmap.height - 1 do
            begin
              copymemory(pd, bitmap.scanline[i], rl);
              inc(pd, rl);
            end;
            getmem(buf2, rl * 2 * bitmap.height);
            l := IEPSRunLengthEncode(pbytearray(buf1), rl * bitmap.height, pbytearray(buf2));
            image := TIEPDFStreamObject.CreateCopy(buf2, l);
            freemem(buf2);
            freemem(buf1);
            ar1 := TIEPDFArrayObject.Create;
            ar1.Items.Add(TIEPDFNameObject.Create('RunLengthDecode'));
            image.dict.items.AddObject('Filter', ar1);
          end;
        ioPDF_G4FAX:
          begin
            CCITTCommonCodeBegin;
            for i := 0 to bitmap.height - 1 do
              CCITTHuffmanPutLineG4(pbyte(Bitmap.Scanline[i]), Bitmap.Width, ms, bwr, bwrl, predline, Progress.Aborting^, 1);
            CCITTHuffmanPutLineG4(nil, 0, ms, bwr, bwrl, predline, Progress.Aborting^, 1);
            CCITTCommonCodeEnd(-1);
          end;
        ioPDF_G3FAX2D:
          begin
            CCITTCommonCodeBegin;
            for i := 0 to bitmap.height - 1 do
              CCITTHuffmanPutLineG32D(pbyte(Bitmap.Scanline[i]), Bitmap.Width, ms, bwr, bwrl, predline, Progress.Aborting^, 1);
            CCITTHuffmanPutLineG32D(nil, 0, ms, bwr, bwrl, predline, Progress.Aborting^, 1);
            CCITTCommonCodeEnd(1);
          end;
        ioPDF_LZW:
          begin
            ms := TMemoryStream.Create;
            lzwid := nil;
            for i := 0 to bitmap.Height-1 do
              IOParams.TIFF_LZWCompFunc(bitmap.Scanline[i], IEBitmapRowLen(bitmap.Width, 1, 8), ms, lzwid);
            IOParams.TIFF_LZWCompFunc(nil, 0, ms, lzwid);
            image := TIEPDFStreamObject.CreateCopy(ms.Memory, ms.Size);
            image.dict.items.Addobject('Filter', TIEPDFNameObject.Create('LZWDecode'));
            FreeAndNil(ms);
          end;
      end;
      image.dict.items.AddObject('ColorSpace', TIEPDFNameObject.Create('DeviceGray'));
      image.dict.items.AddObject('BitsPerComponent', TIEPDFNumericObject.Create(1));
    end
    else
    begin
      // 24 bit
      if IOParams.PDF_Compression = ioPDF_LZW then
      begin
        // LZW
        ms := TMemoryStream.Create;
        lzwid := nil;
        getmem(pd, bitmap.Width*3);
        for i := 0 to bitmap.Height-1 do
        begin
          _CopyBGR_RGB(PRGB(pd), PRGB(bitmap.Scanline[i]), bitmap.Width);
          IOParams.TIFF_LZWCompFunc(pd, bitmap.Width*3, ms, lzwid);
        end;
        freemem(pd);
        IOParams.TIFF_LZWCompFunc(nil, 0, ms, lzwid);
        image := TIEPDFStreamObject.CreateCopy(ms.Memory, ms.Size);
        image.dict.items.Addobject('Filter', TIEPDFNameObject.Create('LZWDecode'));
        FreeAndNil(ms);
        image.dict.items.AddObject('ColorSpace', TIEPDFNameObject.Create('DeviceRGB'));
      end
      else
      if IOParams.PDF_Compression = ioPDF_JPEG then
      begin
        // JPEG (DCT)
        ms := TMemoryStream.Create;
        WriteJpegStream(ms, Bitmap, IOParams, Progress);
        image := TIEPDFStreamObject.CreateCopy(ms.Memory, ms.Size);
        image.dict.items.Addobject('Filter', TIEPDFNameObject.Create('DCTDecode'));
        FreeAndNil(ms);
        if IOParams.JPEG_ColorSpace=ioJPEG_GRAYLEV then
          image.dict.items.AddObject('ColorSpace', TIEPDFNameObject.Create('DeviceGray'))
        else
          image.dict.items.AddObject('ColorSpace', TIEPDFNameObject.Create('DeviceRGB'));
      end
      else
      begin
        // uncompressed
        rl := bitmap.Width * 3;
        image := TIEPDFStreamObject.Create;
        image.length := rl * bitmap.height;
        getmem(image.data, image.length);
        pd := image.data;
        for i := 0 to bitmap.height - 1 do
        begin
          copymemory(pd, bitmap.scanline[i], rl);
          _BGR2RGB(PRGB(pd), bitmap.Width);
          inc(pd, rl);
        end;
        image.dict.items.AddObject('ColorSpace', TIEPDFNameObject.Create('DeviceRGB'));
      end;
      image.dict.items.AddObject('BitsPerComponent', TIEPDFNumericObject.Create(8));
    end;

    image.dict.items.InsertObject(0, 'Type', TIEPDFNameObject.Create('XObject'));
    image.dict.items.InsertObject(1, 'Subtype', TIEPDFNameObject.Create('Image'));
    image.dict.items.AddObject('Width', TIEPDFNumericObject.Create(bitmap.width));
    image.dict.items.AddObject('Height', TIEPDFNumericObject.Create(bitmap.height));
    iepdf_AddIndirectObject(objs, image);
    images.Add(image);
    image.FlushToCache();

    // content stream
    IEGetFitResampleSize(bitmap.Width, bitmap.height, IOParams.PDF_PaperWidth, IOParams.PDF_PaperHeight, w, h);
    pagenum := images.Count;
    contentstr := 'q' + #10
      + IEIntToStr(w) + ' ' + IEIntToStr(0) + ' ' + IEIntToStr(0) + ' ' + IEIntToStr(h) + ' ' + IEIntToStr(0) + ' ' + IEIntToStr(IOParams.PDF_paperheight - h) + ' cm' + #10
      + '/lm' + IEIntToStr(pagenum) + ' Do' + #10
      + 'Q';
    content := TIEPDFStreamObject.CreateCopy(PAnsiChar(contentstr), length(contentstr));
    iepdf_AddIndirectObject(objs, content);
    contents.Add(content);

    // add image resource
    i := resources.Items.IndexOf('XObject');
    di1 := TIEPDFDictionaryObject(resources.Items.Objects[i]);
    re1 := TIEPDFRefObject.Create(image.Index, 0);
    di1.items.AddObject('lm' + IntToStr(pagenum), re1);

    // media box
    mediabox := TIEPDFArrayObject.Create;
    mediabox.items.Add(TIEPDFNumericObject.Create(0));
    mediabox.items.Add(TIEPDFNumericObject.Create(0));
    mediabox.items.Add(TIEPDFNumericObject.Create(IOParams.PDF_PaperWidth));
    mediabox.items.Add(TIEPDFNumericObject.Create(IOParams.PDF_PaperHeight));

    iepdf_AddPage(objs, pages, resources, mediabox, content.Index);

  end;
end;

procedure IEPDFClose(handle: pointer; fs: TStream; var IOParams: TIOParamsVals);
var
  rec: PPDFRec;
  info: TIEPDFDictionaryObject;
  page_tree: TIEPDFDictionaryObject;
begin
  if handle <> nil then
  begin
    rec := PPDFRec(handle);
    with rec^ do
    begin

      // page tree
      page_tree := iepdf_AddPageTree(objs, pages);

      // info dictionary
      info := TIEPDFDictionaryObject.Create;
      if IOParams.PDF_Title <> '' then
        info.items.AddObject('Title', TIEPDFStringObject.Create(IOParams.PDF_Title));
      if IOParams.PDF_Author <> '' then
        info.items.AddObject('Author', TIEPDFStringObject.Create(IOParams.PDF_Author));
      if IOParams.PDF_Subject <> '' then
        info.items.AddObject('Subject', TIEPDFStringObject.Create(IOParams.PDF_Subject));
      if IOParams.PDF_Keywords <> '' then
        info.items.AddObject('Keywords', TIEPDFStringObject.Create(IOParams.PDF_Keywords));
      if IOParams.PDF_Creator <> '' then
        info.items.AddObject('Creator', TIEPDFStringObject.Create(IOParams.PDF_Creator));
      if IOParams.PDF_Producer <> '' then
        info.items.AddObject('Producer', TIEPDFStringObject.Create(IOParams.PDF_Producer));
      info.items.AddObject('CreationDate', TIEPDFStringObject.Create(AnsiString('D:' + formatdatetime('yyyymmddhhmmss', date + time))));

      iepdf_AddIndirectObject(objs, info);

      iepdf_WriteHeader(fs);
      iepdf_WriteIndirectObjects(fs, objs);
      iepdf_WriteFooter(fs, objs, info);

      // free pages
      while pages.Count > 0 do
      begin
        TObject(pages[pages.Count - 1]).Free;
        pages.Delete(pages.Count - 1);
      end;
      FreeAndNil(pages);

      // free indirect objects
      FreeAndNil(resources);
      FreeAndNil(info);

      while contents.Count > 0 do
      begin
        TObject(contents[contents.Count - 1]).Free;
        contents.Delete(contents.Count - 1);
      end;
      FreeAndNil(contents);

      while images.Count > 0 do
      begin
        TObject(images[images.Count - 1]).Free;
        images.Delete(images.Count - 1);
      end;
      FreeAndNil(images);

      FreeAndNil(page_tree);
      FreeAndNil(catalog);

      FreeAndNil(objs);
    end;
    dispose( rec );
  end;
end;

// write a full PDF with a single image

procedure IEPDFSaveOneStep(fs: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  h: pointer;
begin
  h := IEPDFCreate(IOParams);
  IEPDFSave(h, Bitmap, IOParams, Progress);
  IEPDFClose(h, fs, IOParams);
end;

{$endif}  // IEINCLUDEPDFWRITING

// End of PDF
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function IEEMFTryStream(Stream: TStream): boolean;
var
  lp: int64;
  header: TEnhMetaHeader;
begin
  lp := Stream.Position;
  Stream.Read(header, sizeof(header));
  if (header.iType=EMR_HEADER) and (header.dSignature=ENHMETA_SIGNATURE) then
    result := true
  else
    result := false;
  Stream.Position := lp;
end;

type
TPlaceableMetaHeader=record
  Key: DWORD;           // Magic number (always 9AC6CDD7h)
  Handle: WORD;         // Metafile HANDLE number (always 0)
  Left: SHORTINT;       // Left coordinate in metafile units
  Top: SHORTINT;        // Top coordinate in metafile units
  Right: SHORTINT;      // Right coordinate in metafile units
  Bottom: SHORTINT;     // Bottom coordinate in metafile units
  Inch: WORD;           // Number of metafile units per inch
  Reserved: DWORD;      // Reserved (always 0)
  Checksum: WORD;       // Checksum value for previous 10 WORDs
end;

function IEWMFTryStream(Stream: TStream): boolean;
var
  lp: int64;
  aldus: TPlaceableMetaHeader;
  header: TMetaHeader;
begin
  lp := Stream.Position;
  // try Aldus placeable metafiles
  Stream.Read(aldus, sizeof(aldus));
  if aldus.Key<>$9AC6CDD7 then
    Stream.Position := lp
  else
    Stream.Position := lp+22;
  // metafile header
  Stream.Read(header, sizeof(header));
  if (header.mtType=1) and (header.mtHeaderSize=9) and (header.mtNoParameters=0) then
    result := true
  else
    result := false;
  Stream.Position := lp;
end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Real RAW support (this is a true raw: "real" distinguish by Camera RAW)


type TDataType = (dt8BIT, dt16BIT, dtFLOAT);


const PixelFormat2DataType: array [TIEPixelFormat] of TDataType = (dt8BIT, dt8BIT, dt8BIT, dt8BIT, dt16BIT, dt8BIT, dtFLOAT, dt8BIT, dt16BIT, dt8BIT, dt8BIT);


function IsDigit(c: AnsiChar): boolean;
begin
  result := (c>='0') and (c<='9');
end;

function IsHexDigit(c: AnsiChar): boolean;
begin
  result := ((c>='0') and (c<='9')) or ((c>='A') and (c<='F'));
end;

procedure DiscardNonDigits(Stream: TStream);
var
  c: AnsiChar;
begin
  while (Stream.Read(c, 1) = 1) and not IsDigit(c) do;
  Stream.Seek(-1, soCurrent);
end;

procedure DiscardNonHexDigits(Stream: TStream);
var
  c: AnsiChar;
begin
  while (Stream.Read(c, 1) = 1) and not IsHexDigit(c) do;
  Stream.Seek(-1, soCurrent);
end;

function GetDigits(Stream: TStream): AnsiString;
const
  MAXLEN = 35;
var
  c: AnsiChar;
  l: integer;
begin
  SetLength(result, MAXLEN);
  l := 1;
  while (Stream.Read(c, 1) = 1) and IsDigit(c) and (l<MAXLEN) do
  begin
    result[l] := c;
    inc(l);
  end;
  SetLength(result, l-1);
end;

function GetHexDigits(Stream: TStream): AnsiString;
const
  MAXLEN = 35;
var
  c: AnsiChar;
  l: integer;
begin
  SetLength(result, MAXLEN);
  l := 1;
  while (Stream.Read(c, 1) = 1) and IsHexDigit(c) and (l<MAXLEN) do
  begin
    result[l] := c;
    inc(l);
  end;
  SetLength(result, l-1);
end;

procedure PutValue(v: AnsiString; var buf: pointer; DataType: TDataType); overload;
begin
  case DataType of
    dt8BIT:
      begin
        pbyte(buf)^ := IEStrToIntDef(v, 0);
        inc(pbyte(buf));
      end;
    dt16BIT:
      begin
        pword(buf)^ := IEStrToIntDef(v, 0);
        inc(pword(buf));
      end;
    dtFLOAT:
      begin
        psingle(buf)^ := IEStrToFloatDefA(v, 0.0);
        inc(psingle(buf));
      end;
  end;
end;

procedure PutValue(v: integer; var buf: pointer; DataType: TDataType); overload;
begin
  case DataType of
    dt8BIT:
      begin
        pbyte(buf)^ := v;
        inc(pbyte(buf));
      end;
    dt16BIT:
      begin
        pword(buf)^ := v;
        inc(pword(buf));
      end;
    dtFLOAT:
      raise Exception.Create('Format unsupported');
  end;
end;

function HexStrToInt(s: AnsiString): integer;
var
  c: AnsiChar;
  i, bit, val: integer;
begin
  s := IELowerCase(IETrim(s));
  result := 0;
  bit := 0;
  for i := length(s) downto 1 do
  begin
    c := s[i];
    if IsDigit(c) then
      val := ord(c) - 48
    else
      val := ord(c) - 87;
    inc(result, (1 shl bit) * val);
    inc(bit, 4);
  end;
end;

// IOParams.BMPRAW_ChannelOrder (coRGB, coBGR)
// IOParams.BMPRAW_Planes (plInterleaved, plPlanar)
// IOParams.BMPRAW_RowAlign (alignment in bits: 8, 16, 32...)
// IOParams.BMPRAW_HeaderSize
// IOParams.BMPRAW_DataFormat (dfBinary, dfTextDecimal, dfTextHex)
// - do not look at BitsPerSample and SamplesPerPixel but read Bitmap.PixelFormat. Also read Bitmap.Width and Height
//   to know the incoming bitmap size.
// - ie8p palette is not supported
procedure IERealRAWReadStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  rl: integer;
  lper: integer;
  fs: TIEBufferedReadStream;

  procedure DoProgress(p: integer);
  begin
    if p<>lper then
      with Progress do
        if assigned(fOnProgress) then
        begin
          fOnProgress(Sender, p);
          lper := p;
        end;
  end;

  procedure ReadRow(buf: pointer; count: integer; bytes: integer; DataType: TDataType; Channels: integer);
  var
    i, j: integer;
  begin
    case IOParams.BMPRAW_DataFormat of
      dfBinary:
        fs.Read(buf^, bytes);
      dfTextDecimal:
        for i := 0 to count-1 do
          for j := 1 to Channels do
          begin
            DiscardNonDigits(fs);
            PutValue(GetDigits(fs), buf, DataType);
          end;
      dfTextHex:
        for i := 0 to count-1 do
          for j := 1 to Channels do
          begin
            DiscardNonHexDigits(fs);
            PutValue(HexStrToInt(GetHexDigits(fs)), buf, DataType);
          end;
    end;
  end;

  procedure ReadDirect;
  var
    row: integer;
  begin
    for row := 0 to Bitmap.Height-1 do
    begin
      ReadRow(Bitmap.Scanline[row], Bitmap.Width, imin(Bitmap.RowLen, rl), PixelFormat2DataType[Bitmap.PixelFormat], Bitmap.ChannelCount);
      if rl-Bitmap.RowLen > 0 then
        fs.Seek( rl-Bitmap.RowLen, soCurrent );
      DoProgress(trunc(row*Progress.per1));
    end;
  end;

  procedure ReadPlanar8(choff: integer; channels: integer);
  var
    row, col: integer;
    buf, src, dst: pbyte;
    channelrl: integer;
  begin
    channelrl := IEBitmapRowLen(Bitmap.Width, 8, IOParams.BMPRAW_RowAlign);
    getmem(buf, 8*Bitmap.Width);
    for row := 0 to Bitmap.height-1 do
    begin
      ReadRow(buf, Bitmap.Width, channelrl, dt8BIT, 1);
      src := buf;
      dst := Bitmap.Scanline[row];
      inc(dst, choff);
      for col := 0 to Bitmap.Width-1 do
      begin
        dst^ := src^;
        inc(dst, channels);
        inc(src);
      end;
    end;
    freemem(buf);
  end;

  procedure ReadPlanar16(choff: integer; channels: integer);
  var
    row, col: integer;
    buf, src, dst: pword;
    channelrl: integer;
  begin
    channelrl := IEBitmapRowLen(Bitmap.Width, 16, IOParams.BMPRAW_RowAlign);
    getmem(buf, 16*Bitmap.Width);
    for row := 0 to Bitmap.height-1 do
    begin
      ReadRow(buf, Bitmap.Width, channelrl, dt16BIT, 1);
      src := buf;
      dst := Bitmap.Scanline[row];
      inc(dst, choff);
      for col := 0 to Bitmap.Width-1 do
      begin
        dst^ := src^;
        inc(dst, channels);
        inc(src);
      end;
    end;
    freemem(buf);
  end;

  procedure ExchangeRGB8;
  var
    row: integer;
  begin
    for row := 0 to Bitmap.Height-1 do
      _BGR2RGB( Bitmap.Scanline[row], Bitmap.Width );
  end;

  procedure ExchangeRGB16;
  var
    row: integer;
  begin
    for row := 0 to Bitmap.Height-1 do
      _BGR2RGB48( Bitmap.Scanline[row], Bitmap.Width );
  end;

begin
  fs := TIEBufferedReadStream.Create(Stream, 8192);
  try
    lper := -1;
    Progress.per1 := 100 / Bitmap.Height;
    fs.Seek(IOParams.BMPRAW_HeaderSize, soCurrent);
    rl := IEBitmapRowLen(Bitmap.Width, Bitmap.BitCount, IOParams.BMPRAW_RowAlign);
    case Bitmap.PixelFormat of
      ie1g,     // gray scale (black/white)
      ie8p,     // color (palette) (PALETTE NOT SUPPORTED)
      ie8g,     // gray scale (256 levels)
      ie16g,    // gray scale (65536 levels)
      ie32f:    // float point values, 32 bit - Single in Pascal - gray scale
        ReadDirect;
      ie24RGB:  // RGB 24 bit (8 bit per channel)
        begin
          case IOParams.BMPRAW_Planes of
            plInterleaved:
              ReadDirect;
            plPlanar:
              begin
                DoProgress(0);
                ReadPlanar8(0, 3);
                DoProgress(33);
                ReadPlanar8(1, 3);
                DoProgress(33);
                ReadPlanar8(2, 3);
                DoProgress(100);
              end;
          end;
          if IOParams.BMPRAW_ChannelOrder=coRGB then
            ExchangeRGB8;
        end;
      ieCMYK:   // CMYK (reversed 8 bit values)
        case IOParams.BMPRAW_Planes of
          plInterleaved:
            ReadDirect;
          plPlanar:
            begin
              DoProgress(0);
              ReadPlanar8(0, 4);
              DoProgress(25);
              ReadPlanar8(1, 4);
              DoProgress(50);
              ReadPlanar8(2, 4);
              DoProgress(75);
              ReadPlanar8(3, 4);
              DoProgress(100);
            end;
        end;
      ie48RGB:   // RGB 48 bit (16 bit per channel)
        begin
          case IOParams.BMPRAW_Planes of
            plInterleaved:
              ReadDirect;
            plPlanar:
            begin
              DoProgress(0);
              ReadPlanar16(0, 3);
              DoProgress(33);
              ReadPlanar16(1, 3);
              DoProgress(66);
              ReadPlanar16(2, 3);
              DoProgress(100);
            end;
          end;
          if IOParams.BMPRAW_ChannelOrder=coRGB then
            ExchangeRGB16;
        end;
    end;
  finally
    fs.Free;
  end;
end;

// IOParams.BMPRAW_ChannelOrder (coRGB, coBGR)
// IOParams.BMPRAW_Planes (plInterleaved, plPlanar)
// IOParams.BMPRAW_RowAlign (alignment in bits: 8, 16, 32...)
// IOParams.BMPRAW_DataFormat (dfBinary, dfTextDecimal, dfTextHex)
// - do not look at BitsPerSample and SamplesPerPixel but read Bitmap.PixelFormat.
// - ie8p palette is not supported
procedure IERealRAWWriteStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  rl: integer;
  lper: integer;
  fs: TIEBufferedWriteStream;

  procedure DoProgress(p: integer);
  begin
    if lper<>p then
      with Progress do
        if assigned(fOnProgress) then
        begin
          fOnProgress(Sender, p);
          lper := p;
        end;
  end;

  function GetValue(var buf: pointer; DataType: TDataType; hex: boolean): AnsiString;
  begin
    case DataType of
      dt8BIT:
        begin
          if hex then
            result := IEIntToHex(pbyte(buf)^, 4)
          else
            result := IEIntToStr(pbyte(buf)^);
          inc(pbyte(buf));
        end;
      dt16BIT:
        begin
          if hex then
            result := IEIntToHex(pword(buf)^, 4)
          else
            result := IEIntToStr(pword(buf)^);
          inc(pword(buf));
        end;
      dtFLOAT:
        begin
          result := IEFloatToStrA(psingle(buf)^);
          inc(psingle(buf));
        end;
    end;
  end;

  procedure WriteRow(buf: pointer; count: integer; bytes: integer; DataType: TDataType; Channels: integer);
  const
    sep: AnsiChar = #10;
  var
    s: AnsiString;
    i, j: integer;
  begin
    case IOParams.BMPRAW_DataFormat of
      dfBinary:
        fs.Write(pbyte(buf)^, bytes);
      dfTextDecimal:
        for i := 0 to count-1 do
          for j := 1 to Channels do
          begin
            s := GetValue(buf, DataType, false);
            fs.Write(s[1], length(s));
            fs.Write(sep, 1);
          end;
      dfTextHex:
        for i := 0 to count-1 do
          for j := 1 to Channels do
          begin
            s := GetValue(buf, DataType, true);
            fs.Write(s[1], length(s));
            fs.Write(sep, 1);
          end;
    end;
  end;

  procedure WriteDirect;
  var
    row: integer;
  begin
    for row := 0 to Bitmap.Height-1 do
    begin
      WriteRow(Bitmap.Scanline[row], Bitmap.Width, imin(Bitmap.RowLen, rl), PixelFormat2DataType[Bitmap.PixelFormat], Bitmap.ChannelCount);
      if rl-Bitmap.RowLen > 0 then
        fs.Seek( rl-Bitmap.RowLen, soCurrent );
      DoProgress(trunc(row*Progress.per1));
    end;
  end;

  procedure WritePlanar8(choff: integer; channels: integer);
  var
    row, col: integer;
    buf, src, dst: pbyte;
    channelrl: integer;
  begin
    channelrl := IEBitmapRowLen(Bitmap.Width, 8, IOParams.BMPRAW_RowAlign);
    getmem(buf, 8*Bitmap.Width);
    for row := 0 to Bitmap.height-1 do
    begin
      dst := buf;
      src := Bitmap.Scanline[row];
      inc(src, choff);
      for col := 0 to Bitmap.Width-1 do
      begin
        dst^ := src^;
        inc(src, channels);
        inc(dst);
      end;
      WriteRow(buf, Bitmap.Width, channelrl, dt8BIT, 1);
    end;
    freemem(buf);
  end;

  procedure WritePlanar16(choff: integer; channels: integer);
  var
    row, col: integer;
    buf, src, dst: pword;
    channelrl: integer;
  begin
    channelrl := IEBitmapRowLen(Bitmap.Width, 16, IOParams.BMPRAW_RowAlign);
    getmem(buf, 16*Bitmap.Width);
    for row := 0 to Bitmap.height-1 do
    begin
      dst := buf;
      src := Bitmap.Scanline[row];
      inc(src, choff);
      for col := 0 to Bitmap.Width-1 do
      begin
        dst^ := src^;
        inc(src, channels);
        inc(dst);
      end;
      WriteRow(buf, Bitmap.Width, channelrl, dt16BIT, 1);
    end;
    freemem(buf);
  end;

  procedure ExchangeRGB8;
  var
    row: integer;
  begin
    for row := 0 to Bitmap.Height-1 do
      _BGR2RGB( Bitmap.Scanline[row], Bitmap.Width );
  end;

  procedure ExchangeRGB16;
  var
    row: integer;
  begin
    for row := 0 to Bitmap.Height-1 do
      _BGR2RGB48( Bitmap.Scanline[row], Bitmap.Width );
  end;

begin
  fs := TIEBufferedWriteStream.Create(Stream, 8192);
  lper := -1;
  Progress.per1 := 100 / Bitmap.Height;
  rl := IEBitmapRowLen(Bitmap.Width, Bitmap.BitCount, IOParams.BMPRAW_RowAlign);  // output row length
  case Bitmap.PixelFormat of
    ie1g,     // gray scale (black/white)
    ie8p,     // color (palette)
    ie8g,     // gray scale (256 levels)
    ie16g,    // gray scale (65536 levels)
    ie32f:    // float point values, 32 bit - Single in Pascal - gray scale
      WriteDirect;
    ie24RGB:  // RGB 24 bit (8 bit per channel)
      begin
        case IOParams.BMPRAW_Planes of
          plInterleaved:
            begin
              if IOParams.BMPRAW_ChannelOrder=coRGB then
                ExchangeRGB8;
              WriteDirect;
              if IOParams.BMPRAW_ChannelOrder=coRGB then
                ExchangeRGB8;
            end;
          plPlanar:
            begin
              if IOParams.BMPRAW_ChannelOrder=coRGB then
              begin
                DoProgress(0);
                WritePlanar8(2, 3);
                DoProgress(33);
                WritePlanar8(1, 3);
                DoProgress(66);
                WritePlanar8(0, 3);
                DoProgress(100);
              end
              else
              begin
                DoProgress(0);
                WritePlanar8(0, 3);
                DoProgress(33);
                WritePlanar8(1, 3);
                DoProgress(66);
                WritePlanar8(2, 3);
                DoProgress(100);
              end;
            end;
        end;
      end;
    ieCMYK:   // CMYK (reversed 8 bit values)
      case IOParams.BMPRAW_Planes of
        plInterleaved:
          WriteDirect;
        plPlanar:
          begin
            DoProgress(0);
            WritePlanar8(0, 4);
            DoProgress(25);
            WritePlanar8(1, 4);
            DoProgress(50);
            WritePlanar8(2, 4);
            DoProgress(75);
            WritePlanar8(3, 4);
            DoProgress(100);
          end;
      end;
    ie48RGB:   // RGB 48 bit (16 bit per channel)
      begin
        case IOParams.BMPRAW_Planes of
          plInterleaved:
            begin
              if IOParams.BMPRAW_ChannelOrder=coRGB then
                ExchangeRGB16;
              WriteDirect;
              if IOParams.BMPRAW_ChannelOrder=coRGB then
                ExchangeRGB16;
            end;
          plPlanar:
            if IOParams.BMPRAW_ChannelOrder=coRGB then
            begin
              DoProgress(0);
              WritePlanar16(2, 3);
              DoProgress(33);
              WritePlanar16(1, 3);
              DoProgress(66);
              WritePlanar16(0, 3);
              DoProgress(100);
            end
            else
            begin
              DoProgress(0);
              WritePlanar16(0, 3);
              DoProgress(33);
              WritePlanar16(1, 3);
              DoProgress(66);
              WritePlanar16(2, 3);
              DoProgress(100);
            end;
        end;
      end;
  end;
  fs.Free;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



end.
