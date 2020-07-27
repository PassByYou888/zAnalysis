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

unit giffilter;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, Graphics, classes, sysutils, ImageEn, ImageEnIO, ImageEnProc, hyiedefs, hyieutils;

type

  // main header / screen descriptor
  TGIFHeader = packed record
    // GIF magic
    id: array[0..5] of AnsiChar;
    // screen descriptor
    WinWidth: word;   // window width
    WinHeight: word;  // window height
    Flags: byte;      // Flags
    Background: byte; // Background
    Ratio: byte;      // Aspect ratio   dx/dy = (Ratio+15)/64
  end;

procedure ReadGIFStream(fs: TStream; OutBitmap: TIEBitmap; var numi: integer; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; var AlphaChannel: TIEMask; IgnoreAlpha: boolean);
procedure WriteGIFStream(fs: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
function _InsertGIFIm(FileName: string; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec): integer;
function _InsertGIFImStream(fs: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec): integer;
procedure _GIFMakeAnimate(const FileName: string; iters: word; windx, windy: integer);
function _DeleteGIFIm(FileName: string; idx: integer; wr: boolean): integer;
function _CheckGIFAnimate(const FileName: string): boolean;

implementation

uses NeurQuant, ImageEnView, ieview, iesettings;

{$R-}

type
  // image descriptor
  TGIFImDes = packed record
    PosX: word;   // X position
    PosY: word;   // Y position
    Width: word;  // width
    Height: word; // height
    Flags: byte;  // Flags
  end;

  // Graphic control extension
  TGIFGCE = packed record
    Flags: byte;        // Flags
    DelayTime: word;    // wait time (1/100 seconds)
    TranspColor: byte;  // transparent color
  end;

// makes a gif animated
procedure _GIFMakeAnimate(const FileName: string; iters: word; windx, windy: integer);
var
  fs: TFileStream;
  fm: TMemoryStream;
  head: TGIFHeader;
  // info in FLAGS of screen descriptor
  headDimGlobColormap: integer; // size of global color map
  headGlobalColormap: boolean;  // does global color map exist?
  //
  bb: byte;
  ps: int64;
  buf: string[255];
begin
  fs := TFileStream.create(FileName, fmOpenRead or fmShareDenyWrite); // INPUT
  fm := TMemoryStream.Create; // OUTPUT
  try
    try
      // read header and check GIF validity
      fs.read(head, sizeof(TGIFHeader));
      if (head.id[0] <> 'G') or (head.id[1] <> 'I') or (head.id[2] <> 'F') then
        exit;
      // SAVE HEADER
      head.id[3] := '8';
      head.id[4] := '9';
      head.id[5] := 'a';
      head.WinWidth  := imax(windx, head.WinWidth);
      head.WinHeight := imax(windy, head.WinHeight);
      fm.Write(head, sizeof(TGIFHeader));
      // Decode FLAGS of screen descriptor
      headDimGlobColormap := (head.Flags and $07);
      headGlobalColormap := (head.Flags and $80) <> 0;
      // Load, if exists, global colormap
      if headGlobalColormap then
        IECopyFrom(fm, fs, 3 * (2 shl headDimGlobColormap));
      // Check if animated GIF
      ps := fs.Position; // save position
      fs.read(bb, 1);
      if bb = $21 then
      begin
        // look for extension
        fs.read(bb, 1);
        if bb = $FF then
        begin
          // found comment label
          fs.read(buf[0], 1); // len
          fs.read(buf[1], ord(buf[0]));
          if buf = 'NETSCAPE2.0' then
            // already animated, bypass extension
            inc(ps, 19);
        end;
      end;
      fs.position := ps;
      // write application extension (for animations)
      bb := $21;
      fm.Write(bb, 1); // extension code
      bb := $FF;
      fm.Write(bb, 1); // comment label
      bb := 11;
      fm.Write(bb, 1); // label length
      fm.Write('NETSCAPE2.0', 11); // !!
      bb := 3;
      fm.Write(bb, 1); // subblock length
      bb := 1;
      fm.Write(bb, 1); //
      fm.Write(iters, 2); // iterations count
      bb := 0;
      fm.Write(bb, 1); // end
      // save remaining part
      if fs.size - fs.position > 0 then
        IECopyFrom(fm, fs, fs.size - fs.position);
    finally
      FreeAndNil(fs);
    end;
    fm.SaveToFile(FileName);
  finally
    FreeAndNil(fm);
  end;
end;

procedure SafeCopyFrom(Dest, Source: TStream; len: integer);
begin
  IECopyFrom(Dest, Source, i64min(len, Source.size - Source.position));
end;

// Remove image idx from a GIF
// The GIF file must exist
// return number of images inside the GIF
// wr: if true rewrite the file (wr=false is useful to read image count)
function _DeleteGIFIm(FileName: string; idx: integer; wr: boolean): integer;
var
  fs: TFileStream;
  fm: TMemoryStream;
  head: TGIFHeader;
  imdesc: TGIFImDes;
  bb: byte;
  // data in FLAGS of screen descriptor
  headDimGlobColormap: integer; // size of global colormap
  headGlobalColormap: boolean;  // does global colormap exist?
  // data in FLAGS of image descriptor
  imDimLocalColormap: integer;  // size of local colormap
  imLocalColormap: boolean;     // does local colormap exist?
  // estensioni
  ExtType: byte;  // extension type
  ExtLen: byte;   // extension size
  //
  numi: integer;  // image count
  xnumi: integer; // image count - the removed one
begin
  result := 0;
  numi := 0;
  xnumi := 0;
  fm := TMemoryStream.Create; // OUTPUT
  fs := TFileStream.create(FileName, fmOpenRead or fmShareDenyWrite); // INPUT
  try
    // read header and check GIF validity
    fs.read(head, sizeof(TGIFHeader));
    if (head.id[0] <> 'G') or (head.id[1] <> 'I') or (head.id[2] <> 'F') then
    begin
      result := 0;
      exit;
    end;
    // SAVE HEADER
    head.id[3] := '8';
    head.id[4] := '9';
    head.id[5] := 'a';
    fm.Write(head, sizeof(TGIFHeader));
    // decode FLAGS of screen descriptor
    headDimGlobColormap := (head.Flags and $07);
    headGlobalColormap := (head.Flags and $80) <> 0;
    // Load and save, if exists, the global colormap
    if headGlobalColormap then
      SafeCopyFrom(fm, fs, 3 * (2 shl headDimGlobColormap));
    //
    repeat
      fs.read(bb, 1);
      if fs.Position >= fs.size then // fs.size is acceptable, $3B should allow to exit
        bb := $3B;
      case bb of
        $2C: // image descriptor
          begin
            fs.read(imdesc, sizeof(TGIFImDes));
            // decode FLAGS of image descriptor
            imDimLocalColormap := (imdesc.Flags and $07);
            imLocalColormap := (imdesc.Flags and $80) <> 0;
            if numi <> idx then
            begin
              inc(xnumi);
              // SAVE IMAGE DESCRIPTOR
              fm.Write(bb, 1);
              fm.Write(imdesc, sizeof(TGIFImDes));
              // load and save, if exists, local colormap
              if imLocalColormap then
                SafeCopyFrom(fm, fs, 3 * (2 shl imDimLocalColormap));
              // load and save image data
              fs.read(bb, 1);   // read bit lzw size
              fm.Write(bb, 1);  // write bit lzw size
              repeat
                fs.read(bb, 1);   // read block size
                fm.Write(bb, 1);  // write block size
                if fs.position + bb > fs.size then
                  exit;
                if bb > 0 then
                  SafeCopyFrom(fm, fs, bb);
              until (bb = 0) or (fs.position >= fs.size);
            end
            else
            begin
              if imLocalColormap then
                // bypass colormap
                fs.Seek(3 * (2 shl imDimLocalColormap), soCurrent);
              // bypass image
              fs.read(bb, 1);   // bit size
              repeat
                fs.read(bb, 1); // block size
                fs.Seek(bb, soCurrent);
              until (bb = 0) or (fs.position >= fs.size);
            end;
            inc(numi);
          end;
        $21: // Extensions
          begin
            fs.read(ExtType, 1); // extension type
            if (ExtType = $F9) and (idx = numi) then
            begin
              // graphic control extension of image to delete, bypass it!
              repeat
                fs.Read(ExtLen, 1); // extension size (0=end)
                fs.Seek(ExtLen, soCurrent); // bypass
              until (ExtLen = 0) or (fs.Position >= fs.size);
            end
            else
            begin
              bb := $21;
              fm.Write(bb, 1);      // save exnteion marker
              fm.Write(ExtType, 1); // save extension type
              repeat
                fs.read(ExtLen, 1); // extension size (0=end)
                fm.Write(ExtLen, 1);
                if ExtLen > 0 then
                begin
                  if (ExtLen + fs.position) > fs.size then
                  begin
                    if fs.size - fs.position > 0 then
                      SafeCopyFrom(fm, fs, fs.size - fs.position);
                  end
                  else
                  begin
                    if ExtLen > 0 then
                      SafeCopyFrom(fm, fs, ExtLen);
                  end;
                end;
              until (ExtLen = 0) or (fs.Position >= fs.size);
            end;
          end;
        $3B: // end of GIF
          begin
            bb := $3B;
            fm.Write(bb, 1);
            break;
          end;
      end; // end of CASE
    until false;
  finally
    FreeAndNil(fs);
    if wr then
      fm.SaveToFile(FileName);
    FreeAndNil(fm);
    result := xnumi;
  end;
end;

// returns True if the GIF is animated (NETSCAPE...)
function _CheckGIFAnimate(const FileName: string): boolean;
var
  fs: TFileStream;
  head: TGIFHeader;
  // data in FLAGS of screen descriptor
  headDimGlobColormap: integer; // size of global colormap
  headGlobalColormap: boolean;  // does global colormap exist?
  //
  bb: byte;
  buf: string[255];
begin
  fs := TFileStream.create(FileName, fmOpenRead or fmShareDenyWrite); // INPUT
  try
    // read header and check GIF validity
    fs.read(head, sizeof(TGIFHeader));
    if (head.id[0] <> 'G') or (head.id[1] <> 'I') or (head.id[2] <> 'F') then
    begin
      result := false;
      exit;
    end;
    // decode field FLAGS of screen descriptor
    headDimGlobColormap := (head.Flags and $07);
    headGlobalColormap := (head.Flags and $80) <> 0;
    // bypass, if present, global colormap
    if headGlobalColormap then
      fs.Seek(3 * (2 shl headDimGlobColormap), soCurrent);
    // check if animated GIF
    result := false;
    fs.read(bb, 1);
    if bb = $21 then
    begin
      // extension found
      fs.read(bb, 1);
      if bb = $FF then
      begin
        // found comment label
        fs.read(buf[0], 1); // len
        fs.read(buf[1], ord(buf[0]));
        if buf = 'NETSCAPE2.0' then
          // ok
          result := true;
      end;
    end;
  finally
    FreeAndNil(fs);
  end;
end;


// Read a GIF from stream
// Outbitmap: output bitmap
// numi (output) : number of images in this GIF
// Leave the stream position at the end of GIF block
procedure ReadGIFStream(fs: TStream; OutBitmap: TIEBitmap; var numi: integer; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; var AlphaChannel: TIEMask; IgnoreAlpha: boolean);
var
  idx, ii: integer;
  head: TGIFHeader;
  imdesc: TGIFImDes;
  GIFGCE: TGIFGCE;
  bb: byte;
  fdata: pByteArray;
  fdataptr: pbyte;
  qq: int64;
  q, x, y: integer;
  ppx: pRGB;
  px_byte, bx, px: pbyte;
  IsTrueColor: boolean;
  pc: PAnsiChar;
  // fields in FLAGS of screen descriptor
  headDimGlobColormap: integer; // size of global Colormap
  headGlobalColormap: boolean;  // does global colormap exists?
  // Colormap globale
  GlobalColorMap: array[0..255] of TRGB;  // warning, swapped RGB!
  LocalColorMap: array[0..255] of TRGB;   // warning, swapper RGB!
  // fields in FLAGS of image descriptor
  imDimLocalColormap: integer;  // size of local colormap
  imInterlLocal: boolean;       // interlaced image = true
  imLocalColormap: boolean;     // does local colormap exists?
  // extensions
  ExtType: byte;  // extension type
  ExtLen: byte;   // extension length
  //
  fssize: int64;
  doalpha: boolean;
begin
  numi := 0;
  if assigned(Progress.fOnProgress) then
    Progress.fOnProgress(Progress.Sender, 0);
  if Progress.Aborting^ then
    exit;
  fssize := fs.Size;
  fillchar(GIFGCE, sizeof(GIFGCE), 0);
  idx := IOParams.GIF_ImageIndex;
  // Read header and check GIF validity
  fs.read(head, sizeof(TGIFHeader));
  if (head.id[0] <> 'G') or (head.id[1] <> 'I') or (head.id[2] <> 'F') then
  begin
    Progress.Aborting^ := True;
    exit;
  end;
  IOParams.GIF_Version := AnsiString(head.id);  // 3.0.1

  IOParams.GIF_WinWidth := head.WinWidth;
  IOParams.GIF_WinHeight := head.WinHeight;
  IOParams.GIF_Ratio := head.Ratio;
  IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
  IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
  IOParams.SamplesPerPixel := 1;

  // Decode FLAGS of screen descriptor
  headDimGlobColormap := (head.Flags and $07);
  headGlobalColormap := (head.Flags and $80) <> 0;
  // Load, if possible, global colormap
  if headGlobalColormap then
  begin
    fs.read(GlobalColorMap, 3 * (2 shl headDimGlobColormap));
    // swap colomap colors (rgb->bgr)
    for q := 0 to (2 shl headDimGlobColormap) - 1 do
      bswap(GlobalColorMap[q].r, GlobalColorMap[q].b);
    IOParams.GIF_Background := GlobalColorMap[head.background];
  end;
  //
  if fs.Position >= fssize then
    Progress.Aborting^ := true;
  fdata := nil; // prevents warning
  numi := 0;
  repeat
    if Progress.Aborting^ then
      break;
    fs.read(bb, 1);
    if fs.Position >= fssize then
      bb := $3B;
    case bb of
      $2C: // image descriptor
        begin
          fs.read(imdesc, sizeof(TGIFImDes));
          // Decode FLAGS of image descriptor
          imDimLocalColormap := (imdesc.Flags and $07);
          imInterlLocal := (imdesc.Flags and $40) <> 0;
          imLocalColormap := (imdesc.Flags and $80) <> 0;
          // Load, if present, local colormap
          if imLocalColormap then
            fs.read(LocalColorMap, 3 * (2 shl imDimLocalColormap));
          if numi = idx then
          begin
            if assigned(Progress.fOnProgress) then
              Progress.fOnProgress(Progress.Sender, 30);
            if Progress.Aborting^ then
              break;
            //
            if imLocalColormap then
            begin
              // swap colormap colors (rgb->bgr)
              for q := 0 to (2 shl imDimLocalColormap) - 1 do
                bswap(LocalColorMap[q].r, LocalColorMap[q].b);
              IOParams.GIF_TranspColor := LocalColorMap[GIFGCE.TranspColor];
            end
            else
              IOParams.GIF_TranspColor := GlobalColorMap[GIFGCE.TranspColor];
            // Fill GIFInfo record
            IOParams.GIF_XPos := imdesc.PosX;
            IOParams.GIF_YPos := imdesc.PosY;
            IOParams.Width  := imdesc.Width;
            IOParams.Height := imdesc.Height;
            IOParams.OriginalWidth  := imdesc.Width;
            IOParams.OriginalHeight := imdesc.Height;
            IOParams.GIF_DelayTime := GIFGCE.DelayTime;
            IOParams.GIF_FlagTranspColor := (GIFGCE.Flags and $01) <> 0;
            IOParams.GIF_Interlaced := imInterlLocal;
            IOParams.GIF_Action := TIEGIFAction((GIFGCE.Flags and $1C) shr 2);
            if Preview then
            begin
              // bypass image (Preview/Parameters only)
              fs.read(bb, 1);   // bit size
              repeat
                fs.read(bb, 1); // block size
                fs.Seek(bb, soCurrent);
              until (bb = 0) or (fs.position >= fssize);
            end
            else
            begin
              // decode image
              IESilentGetMem(FData, imdesc.Width * imdesc.Height);
              if fData=nil then
              begin
                break;
              end;
              qq := fs.position;
              if assigned(IOParams.GIF_LZWDecompFunc) then
                IOParams.GIF_LZWDecompFunc(fs, imdesc.Height, imdesc.Width, imInterlLocal, PAnsiChar(FData))
              else
              if assigned(IEGlobalSettings().DefGIF_LZWDECOMPFUNC) then
                IEGlobalSettings().DefGIF_LZWDECOMPFUNC(fs, imdesc.Height, imdesc.Width, imInterlLocal, PAnsiChar(FData));
              if qq = fs.position then
              begin
                freemem(FData);
                Progress.Aborting^ := True;
                break;
              end;
              if assigned(Progress.fOnProgress) then
                Progress.fOnProgress(Progress.Sender, 70);
              if Progress.Aborting^ then
              begin
                freemem(FData);
                break;
              end;
            end;
            if IOParams.ColorMap <> nil then
              freemem(IOParams.ColorMap);
            if imLocalColormap then
              // Use local colormap
              with IOParams do
              begin
                BitsPerSample := imDimLocalColormap + 1;
                fColorMapCount := (1 shl BitsPerSample);
                getmem(fColorMap, ColorMapCount * sizeof(TRGB));
                CopyMemory(ColorMap, @LocalColorMap, (1 shl BitsPerSample) * sizeof(TRGB));
              end
            else
              // use global colormap
              with IOParams do
              begin
                BitsPerSample := headDimGlobColormap + 1;
                fColorMapCount := (1 shl BitsPerSample);
                getmem(fColorMap, ColorMapCount * sizeof(TRGB));
                CopyMemory(ColorMap, @GlobalColorMap, (1 shl BitsPerSample) * sizeof(TRGB));
              end;
            if not Preview then
            begin

              if (IOParams.BitsPerSample = 1) and (IOParams.ColorMapCount = 2) then
                IsTrueColor := not (EqualRGB(IOParams.ColorMap[0], CreateRGB(0, 0, 0)) and EqualRGB(IOParams.ColorMap[1], CreateRGB(255, 255, 255)))
              else
                IsTrueColor := IOParams.BitsPerSample <> 1;

              // this has been included to recognize some special kind of corrupted GIFs
              if IsTrueColor and ((imdesc.Width>65000) or (imdesc.Height>65000)) and (fssize<fs.Position+2000) then
              begin
                Progress.Aborting^ := true;
                break;
              end;

              if IsTrueColor then
              begin
                if IOParams.IsNativePixelFormat then
                  OutBitmap.Allocate(imdesc.Width, imdesc.Height, ie8p)
                else
                  OutBitmap.Allocate(imdesc.Width, imdesc.Height, ie24RGB);
              end
              else
                OutBitmap.Allocate(imdesc.Width, imdesc.Height, ie1g);
              doalpha := (not IgnoreAlpha) and (IOParams.GIF_FlagTranspColor);
              if doalpha then
              begin
                if not assigned(AlphaChannel) then
                  AlphaChannel := TIEMask.Create;
                AlphaChannel.AllocateBits(imdesc.Width, imdesc.Height, 8);
                AlphaChannel.Fill(255);
              end;
              // converts FData to OutBitmap
              if IsTrueColor then
              begin
                if IOParams.IsNativePixelFormat then
                begin
                  // native format
                  for y := 0 to IOParams.ColorMapCount - 1 do
                    OutBitmap.Palette[y] := IOParams.ColorMap[y];
                  OutBitmap.PaletteUsed := 1 shl IOParams.BitsPerSample;
                  for y := 0 to imdesc.Height - 1 do
                  begin
                    px_byte := OutBitmap.ScanLine[y];
                    fdataptr := @FData[y * imdesc.Width];
                    for x := 0 to imdesc.Width - 1 do
                    begin
                      ii := fdataptr^;
                      px_byte^ := ii;
                      inc(px_byte);
                      if doalpha and (GIFGCE.TranspColor = ii) then
                        AlphaChannel.SetPixel(x, y, 0);
                      inc(fdataptr);
                    end
                  end;
                end
                else
                begin
                  // convert to 24 bit per pixel
                  for y := 0 to imdesc.Height - 1 do
                  begin
                    ppx := OutBitmap.ScanLine[y];
                    fdataptr := @FData[y * imdesc.Width];
                    for x := 0 to imdesc.Width - 1 do
                    begin
                      ii := fdataptr^;
                      ppx^ := IOParams.ColorMap^[ii];
                      inc(ppx);
                      if doalpha and (GIFGCE.TranspColor = ii) then
                        AlphaChannel.SetPixel(x, y, 0);
                      inc(fdataptr);
                    end
                  end;
                end;
              end
              else
              begin
                // B/W
                for y := 0 to imdesc.Height - 1 do
                begin
                  bx := OutBitmap.ScanLine[y];
                  px := @fdata[y * imdesc.width];
                  x := 0;
                  while x < imdesc.Width - 1 do
                  begin
                    bx^ := (px^ shl 7);
                    inc(px);
                    bx^ := bx^ or (px^ shl 6);
                    inc(px);
                    bx^ := bx^ or (px^ shl 5);
                    inc(px);
                    bx^ := bx^ or (px^ shl 4);
                    inc(px);
                    bx^ := bx^ or (px^ shl 3);
                    inc(px);
                    bx^ := bx^ or (px^ shl 2);
                    inc(px);
                    bx^ := bx^ or (px^ shl 1);
                    inc(px);
                    bx^ := bx^ or (px^ shl 0);
                    inc(px);
                    inc(bx);
                    inc(x, 8);
                  end;
                  // alpha
                  px := @fdata[y * imdesc.width];
                  for x := 0 to imdesc.Width - 1 do
                  begin
                    if doalpha and (GIFGCE.TranspColor = px^) then
                      AlphaChannel.SetPixel(x, y, 0);
                    inc(px);
                  end;
                end;
              end;
              freemem(FData);

              // verify alpha channel
              if doalpha then
              begin
                AlphaChannel.SyncFull;
                if AlphaChannel.Full then
                  FreeAndNil(AlphaChannel);
              end;

            end; // endif not preview
          end
          else
          begin
            // bypass image
            fs.read(bb, 1);   // bit size
            repeat
              fs.read(bb, 1); // block size
              fs.Seek(bb, soCurrent);
            until (bb = 0) or (fs.position >= fssize);
          end;
          inc(numi);
        end;
      $21: // Extensions
        begin
          fs.read(ExtType, 1); // extension type
          if (ExtType = $F9) and (numi = idx) then
          begin
            // load GCE (Graphic Control Extension)
            fs.read(ExtLen, 1);
            fs.read(GIFGCE, sizeof(TGIFGCE));
            fs.read(ExtLen, 1);
          end
          else
          if (ExtType = $FE) then
          begin
            // load Comment Extension
            fs.read(ExtLen, 1);
            while (ExtLen > 0) and (fs.Position < fsSize) do
            begin
              getmem(pc, ExtLen + 1);
              fs.read(pc^, ExtLen);
              pc[ExtLen] := #0;
              if IEStrLen(pc) > 0 then
                IOParams.GIF_Comments.Add(string(pc));
              freemem(pc);
              fs.read(ExtLen, 1); // comment length (0=end)
            end;
          end
          else
            // bypass extensions
            repeat
              fs.read(ExtLen, 1); // extension length (0=end)
              fs.seek(ExtLen, soCurrent); // bypass extensions
            until (ExtLen = 0) or (fs.Position >= fssize);
        end;
      $3B: // end GIF
        break;
    end; // end CASE
  until false;
  IOParams.GIF_ImageCount := numi;
  if assigned(Progress.fOnProgress) and not Progress.Aborting^ then
    Progress.fOnProgress(Progress.Sender, 100);
end;



// Write a GIF
// Always write colormap as global
procedure WriteGIFStream(fs: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec);
var
  q: integer;
  head: TGIFHeader;
  imdesc: TGIFImDes;
  gce: TGIFGCE;
  GlobalColorMap: array[0..255] of TRGB; // warning, RGB swapped!
  fdata: pByteArray;
  px_byte, fd: pbyte;
  bb: byte;
  ss: AnsiString;
  WBitmap: TIEBitmap;
  FreeW: boolean; // If true we must free WBitmap
  BackCol, ForeCol: TRGB;
  NCol: integer;
  row, col: integer;
  ppx: pRGB;
  qt: TIEQuantizer;
  pb: pbyte;
  xBitsPixel: integer;
  nullpr: TProgressRec;
  bitmapwidth1, bitmapheight1: integer;
begin
  if assigned(Progress.fOnProgress) then
    Progress.fOnProgress(Progress.Sender, 0);
  if Progress.Aborting^ then
    exit;
  with nullpr do
  begin
    Aborting := Progress.Aborting;
    fOnProgress := nil;
    Sender := nil;
  end;
  FreeW := false;
  if (IOParams.BitsPerSample = 1) then
  begin
    // Required to save as monochrome
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
    // Required to save as color
    xBitsPixel := IOParams.BitsPerSample;
    if Bitmap.PixelFormat = ie1g then
    begin
      // Convert to 24 bit
      WBitmap := TIEBitmap.Create;
      WBitmap.Assign(Bitmap);
      WBitmap.PixelFormat := ie24RGB;
      FreeW := true;
    end
    else
      WBitmap := Bitmap;
  end;
  NCol := 1 shl xBitsPixel;
  if bitmap.HasAlphaChannel then
  begin
    dec(NCol);
    IOParams.GIF_FlagTranspColor := true;
  end;
  // From here it writes only WBitmap, it can be pf24bit or pf1bit. From here IOParams.BitsPerSample and
  // IOParams.SamplesPerPIxel are ignored
  //
  // prepare global colormap and FData
  qt := nil;
  getmem(FData, wbitmap.Width * wbitmap.Height + 9);
  bitmapheight1 := wbitmap.height - 1;
  bitmapwidth1 := wbitmap.width - 1;
  if WBitmap.pixelformat <> ie1g then
  begin
    // colors
    qt := TIEQuantizer.Create(wBitmap, GlobalColorMap, NCol);
    for row := 0 to bitmapheight1 do
    begin
      if WBitmap.PixelFormat = ie8p then
      begin
        // native format
        px_byte := wbitmap.ScanLine[row];
        for col := 0 to bitmapwidth1 do
        begin
          if bitmap.HasAlphaCHannel and (bitmap.alpha[col, row] < 255) then
            FData[col + row * wbitmap.Width] := NCol
          else
            FData[col + row * wbitmap.Width] := px_byte^;
          inc(px_byte);
        end;
      end
      else
      begin
        // subsample 24 bit
        ppx := wbitmap.ScanLine[row];
        for col := 0 to bitmapwidth1 do
        begin
          if bitmap.HasAlphaCHannel and (bitmap.alpha[col, row] < 255) then
            FData[col + row * wbitmap.Width] := NCol
          else
            FData[col + row * wbitmap.Width] := qt.RGBIndex[ppx^];
          inc(ppx);
        end;
      end;
    end;
  end
  else
  begin
    // black & white
    GlobalColorMap[0] := CreateRGB(0, 0, 0);
    GlobalColorMap[1] := CreateRGB(255, 255, 255);
    for row := 0 to bitmapheight1 do
    begin
      pb := wbitmap.scanline[row];
      fd := @fdata[row * wbitmap.width];
      col := 0;
      while col < wbitmap.width do
      begin
        fd^ := (pb^ and $80) shr 7;
        inc(fd);
        fd^ := (pb^ and $40) shr 6;
        inc(fd);
        fd^ := (pb^ and $20) shr 5;
        inc(fd);
        fd^ := (pb^ and $10) shr 4;
        inc(fd);
        fd^ := (pb^ and $08) shr 3;
        inc(fd);
        fd^ := (pb^ and $04) shr 2;
        inc(fd);
        fd^ := (pb^ and $02) shr 1;
        inc(fd);
        fd^ := (pb^ and $01);
        inc(fd);
        inc(pb);
        inc(col, 8);
      end;
    end;
  end;
  // invert R and B
  for q := 0 to NCol - 1 do
    bswap(GlobalColorMap[q].r, GlobalColorMap[q].b);
  //
  if assigned(Progress.fOnProgress) then
    Progress.fOnProgress(Progress.Sender, 30);
  if Progress.Aborting^ then
  begin
    if FreeW then
      FreeAndNil(WBitmap);
    if assigned(qt) then
      FreeAndNil(qt);
    freemem(FData);
    exit;
  end;
  // prepare header and write it
  CopyMemory(@head.id[0], PAnsiChar(IOParams.GIF_Version), 6);
  head.WinWidth := wbitmap.width;
  head.WinHeight := wbitmap.height;
  head.Flags := (xBitsPixel - 1) or ((xBitsPixel - 1) shl 4) or $80;
  if wbitmap.pixelformat = ie24RGB then
    head.Background := qt.RGBIndex[IOParams.GIF_Background]
  else
    head.background := _GetSimilColor(GlobalColorMap, NCol, IOParams.GIF_Background);
  head.Ratio := 0;
  SafeStreamWrite(fs, Progress.Aborting^, head, sizeof(TGIFHeader));
  // write global colormap
  if bitmap.HasAlphaChannel then
    SafeStreamWrite(fs, Progress.Aborting^, GlobalColorMap, 3 * (NCol + 1))
  else
    SafeStreamWrite(fs, Progress.Aborting^, GlobalColorMap, 3 * NCol);
  // prepare and write Graphic Control Extension (GCE)
  gce.Flags := ord(IOParams.GIF_FlagTranspColor);
  gce.Flags := gce.Flags or (integer(IOParams.GIF_Action) shl 2);
  gce.DelayTime := IOParams.GIF_DelayTime;
  if bitmap.HasAlphaChannel then
  begin
    gce.TranspColor := NCol;
  end
  else
  begin
    if wbitmap.pixelformat = ie24RGB then
      gce.TranspColor := qt.RGBIndex[IOParams.GIF_TranspColor]
    else
      gce.TranspColor := _GetSimilColor(GlobalColorMap, NCol, IOParams.GIF_TranspColor);
  end;
  bb := $21;
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1); // mark extension
  bb := $F9;
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1); // mark as graphic control extension
  bb := sizeof(TGIFGCE);
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1); // extension size
  SafeStreamWrite(fs, Progress.Aborting^, gce, sizeof(TGIFGCE)); // extension
  bb := 0;
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1); // end of extension
  // prepare image descriptor and write it
  // (no local colormap)
  imdesc.PosX := IOParams.GIF_XPos;
  imdesc.PosY := IOParams.GIF_YPos;
  imdesc.Width := wbitmap.width;
  imdesc.Height := wbitmap.height;
  imdesc.Flags := (ord(IOParams.GIF_Interlaced) shl 6);
  bb := $2C; // image descriptor identifier
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1);
  SafeStreamWrite(fs, Progress.Aborting^, imdesc, sizeof(TGIFImDes));
  //
  if assigned(Progress.fOnProgress) then
    Progress.fOnProgress(Progress.Sender, 70);
  if Progress.Aborting^ then
  begin
    if FreeW then
      FreeAndNil(WBitmap);
    if assigned(qt) then
      FreeAndNil(qt);
    freemem(FData);
    exit;
  end;
  // encode (compress) and write the image
  if assigned(IOParams.GIF_LZWCompFunc) then
    IOParams.GIF_LZWCompFunc(fs, wbitmap.Height, wbitmap.Width, IOParams.GIF_Interlaced, PAnsiChar(FData), xBitsPixel)
  else
  if assigned(IEGlobalSettings().DefGIF_LZWCOMPFUNC) then
    IEGlobalSettings().DefGIF_LZWCOMPFUNC(fs, wbitmap.Height, wbitmap.Width, IOParams.GIF_Interlaced, PAnsiChar(FData), xBitsPixel)
  else
  begin
    if FreeW then
      FreeAndNil(WBitmap);
    if assigned(qt) then
      FreeAndNil(qt);
    freemem(FData);
    Progress.Aborting^ := True;
    exit;
  end;
  if FreeW then
    FreeAndNil(WBitmap);
  if assigned(qt) then
    FreeAndNil(qt);
  freemem(FData);
  // insert comments
  bb := $21;
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1);
  bb := $FE;
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1);
  for q := 0 to IOParams.GIF_Comments.Count - 1 do
  begin
    bb := length(IOParams.GIF_Comments[q]);
    SafeStreamWrite(fs, Progress.Aborting^, bb, 1);
    ss := AnsiString(IOParams.GIF_Comments[q]);
    SafeStreamWrite(fs, Progress.Aborting^, ss[1], bb);
  end;
  bb := $0;
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1);
  //
  bb := $3B; // end of GIF marker
  SafeStreamWrite(fs, Progress.Aborting^, bb, 1);
  if assigned(Progress.fOnProgress) then
    Progress.fOnProgress(Progress.Sender, 100);
end;


// Insert an image in a GIF file at "idx" position
// when idx=0 insert at the beginning, while if idx is greater than stored images insert at the ending.
// File GIF must exists.
// Save palette in global colormap if only one image exists, otherwise each image has own colormap.
// Return resulting image count.
function _InsertGIFIm(FileName: string; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec): integer;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenReadWrite);
  result := _InsertGIFImStream(fs, bitmap, IOParams, Progress);
  FreeAndNil(fs);
end;

// Insert an image in a GIF file at "idx" position
// when idx=0 insert at the beginning, while if idx is greater than stored images insert at the ending.
// The stream must already contain a GIF.
// Save palette in global colormap if only one image exists, otherwise each image has own colormap.
// Return resulting image count.
function _InsertGIFImStream(fs: TStream; bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec): integer;
var
  p1: int64;
  idx: integer;
  fm: TMemoryStream;
  head: TGIFHeader;
  imdesc: TGIFImDes;
  gce: TGIFGCE;
  bb: byte;
  px_byte, fd: pbyte;
  fdata: pByteArray;
  // Fields in FLAGS of screen descriptor
  headDimGlobColormap: integer; // Size of global colormap
  headGlobalColormap: boolean;  // Foes global colormap exists?
  // Colormap globale
  LocalColorMap: array[0..255] of TRGB; // Warning, swapped RGB!
  // Fields in FLAGS of image descriptor
  imDimLocalColormap: integer;  // Size of local colormap
  imLocalColormap: boolean;     // Does local colormap exists?
  // Extensions
  ExtType: byte;  // extension type
  ExtLen: byte;   // extension size
  //
  numi: integer;  // image count
  //
  FreeW: boolean; // free WBitmap if true
  xBitsPixel: integer;
  WBitmap: TIEBitmap;
  BackCol, ForeCol: TRGB;
  NCol: integer;
  nullpr: TProgressRec;
  //
  procedure InsertImage;
  var
    qt: TIEQuantizer;
    ppx: pRGB;
    pb: pbyte;
    row, col, q: integer;
  begin
    qt := nil;
    getmem(FData, wbitmap.Width * wbitmap.Height);
    if WBitmap.pixelformat <> ie1g then
    begin
      // colors
      qt := TIEQuantizer.Create(wBitmap, LocalColorMap, NCol);
      for row := 0 to wbitmap.height - 1 do
      begin
        if wbitmap.PixelFormat = ie8p then
        begin
          // native format
          px_byte := wbitmap.ScanLine[row];
          for col := 0 to wbitmap.width - 1 do
          begin
            if bitmap.HasAlphaCHannel and (bitmap.alpha[col, row] < 255) then
              FData[col + row * wbitmap.Width] := NCol
            else
              FData[col + row * wbitmap.Width] := px_byte^;
            inc(px_byte);
          end;
        end
        else
        begin
          // subsample 24 bit
          ppx := wbitmap.ScanLine[row];
          for col := 0 to wbitmap.width - 1 do
          begin
            if bitmap.HasAlphaCHannel and (bitmap.alpha[col, row] < 255) then
              FData[col + row * wbitmap.Width] := NCol
            else
              FData[col + row * wbitmap.Width] := qt.RGBIndex[ppx^];
            inc(ppx);
          end;
        end;
      end;
    end
    else
    begin
      // black & white
      LocalColorMap[0] := CreateRGB(0, 0, 0);
      LocalColorMap[1] := CreateRGB(255, 255, 255);
      for row := 0 to wbitmap.height - 1 do
      begin
        pb := wbitmap.scanline[row];
        fd := @fdata[row * wbitmap.width];
        col := 0;
        while col < wbitmap.width do
        begin
          fd^ := (pb^ and $80) shr 7;
          inc(fd);
          fd^ := (pb^ and $40) shr 6;
          inc(fd);
          fd^ := (pb^ and $20) shr 5;
          inc(fd);
          fd^ := (pb^ and $10) shr 4;
          inc(fd);
          fd^ := (pb^ and $08) shr 3;
          inc(fd);
          fd^ := (pb^ and $04) shr 2;
          inc(fd);
          fd^ := (pb^ and $02) shr 1;
          inc(fd);
          fd^ := (pb^ and $01);
          inc(fd);
          inc(pb);
          inc(col, 8);
        end;
      end;
    end;
    // swap R and B
    for q := 0 to NCol - 1 do
      bswap(LocalColorMap[q].r, LocalColorMap[q].b);
    // prepare and write Graphic Control Extension (GCE)
    gce.Flags := ord(IOParams.GIF_FlagTranspColor);
    gce.Flags := gce.Flags or (integer(IOParams.GIF_Action) shl 2);
    gce.DelayTime := IOParams.GIF_DelayTime;
    if bitmap.HasAlphaChannel then
    begin
      gce.TranspColor := NCol;
    end
    else
    begin
      if wbitmap.pixelformat = ie24RGB then
        gce.TranspColor := qt.RGBIndex[IOParams.GIF_TranspColor]
      else
        gce.TranspColor := _GetSimilColor(LocalColorMap, NCol, IOParams.GIF_TranspColor);
    end;
    bb := $21;
    fm.write(bb, 1); // mark extension
    bb := $F9;
    fm.write(bb, 1); // mark as graphic control extension
    bb := sizeof(TGIFGCE);
    fm.write(bb, 1); // extension size
    fm.write(gce, sizeof(TGIFGCE)); // extension
    bb := 0;
    fm.write(bb, 1); // end of extension
    // prepare and write image descriptor
    // local colormap
    imdesc.PosX := IOParams.GIF_XPos;
    imdesc.PosY := IOParams.GIF_YPos;
    imdesc.Width := wbitmap.width;
    imdesc.Height := wbitmap.height;
    imdesc.Flags := (xBitsPixel - 1) or (ord(IOParams.GIF_Interlaced) shl 6) or $80;
    // write image descriptor
    bb := $2C; // image descritpro marker
    fm.write(bb, 1);
    fm.write(imdesc, sizeof(TGIFImDes));
    // write local colormap
    if bitmap.HasAlphaChannel then
      fm.write(LocalColorMap, 3 * (NCol + 1))
    else
      fm.write(LocalColorMap, 3 * NCol);
    // encode and write image
    if assigned(IOParams.GIF_LZWCompFunc) then
      IOParams.GIF_LZWCompFunc(fm, wbitmap.Height, wbitmap.Width, IOParams.GIF_Interlaced, PAnsiChar(FData), xBitsPixel)
    else
    if assigned(IEGlobalSettings().DefGIF_LZWCOMPFUNC) then
      IEGlobalSettings().DefGIF_LZWCOMPFUNC(fm, wbitmap.Height, wbitmap.Width, IOParams.GIF_Interlaced, PAnsiChar(FData), xBitsPixel)
    else
    begin
      freemem(FData);
      Progress.Aborting^ := False;
    end;
    freemem(FData);
    if assigned(qt) then
      FreeAndNil(qt);
  end;
begin
  result := 0;
  if assigned(Progress.fOnProgress) then
    Progress.fOnProgress(Progress.Sender, 0);
  if Progress.Aborting^ then
    exit;
  //
  with nullpr do
  begin
    Aborting := Progress.Aborting;
    fOnProgress := nil;
    Sender := nil;
  end;
  fm := TMemoryStream.Create; // OUTPUT
  p1 := fs.Position;
  idx := IOParams.GIF_ImageIndex;
  // Read header and check GIF validity
  fs.read(head, sizeof(TGIFHeader));
  if (head.id[0] <> 'G') or (head.id[1] <> 'I') or (head.id[2] <> 'F') then
  begin
    FreeAndNil(fs);
    result := 0;
    Progress.Aborting^ := False;
    exit;
  end;
  // save header
  head.id[3] := '8';
  head.id[4] := '9';
  head.id[5] := 'a';
  fm.Write(head, sizeof(TGIFHeader));
  // Decode FLAGS of screen descriptor
  headDimGlobColormap := (head.Flags and $07);
  headGlobalColormap := (head.Flags and $80) <> 0;
  // Read and Write, if present, global colormap
  if headGlobalColormap then
    IECopyFrom(fm, fs, 3 * (2 shl headDimGlobColormap));
  // create WBitmap
  FreeW := false;
  if (IOParams.BitsPerSample = 1) then
  begin
    // Required to save as monochrome
    xBitsPixel := 1;
    if Bitmap.pixelformat = ie1g then
      WBitmap := Bitmap
    else
    begin
      // Convert to 1 bit
      WBitmap := _ConvertTo1bitEx(Bitmap, BackCol, ForeCol);
      if WBitmap = nil then
      begin
        // failed to convert to 1 bit, save as true color
        WBitmap := Bitmap;
        xBitsPixel := 8;
      end
      else
        FreeW := true;
    end;
  end
  else
  begin
    // Required to save as colors
    xBitsPixel := IOParams.BitsPerSample;
    if Bitmap.PixelFormat = ie1g then
    begin
      // Convert to 24 bit
      WBitmap := TIEBitmap.Create;
      WBitmap.Assign(Bitmap);
      WBitmap.PixelFormat := ie24RGB;
      FreeW := true;
    end
    else
      WBitmap := Bitmap;
  end;
  NCol := 1 shl xBitsPixel;
  if bitmap.HasAlphaChannel then
  begin
    dec(NCol);
    IOParams.GIF_FlagTranspColor := true;
  end;
  //
  if assigned(Progress.fOnProgress) then
    Progress.fOnProgress(Progress.Sender, 30);
  if Progress.Aborting^ then
  begin
    if FreeW then
      FreeAndNil(WBitmap);
    FreeAndNil(fm);
    exit;
  end;
  //
  numi := 0;
  repeat
    if Progress.Aborting^ then
      break;
    fs.read(bb, 1);
    if fs.Position >= fs.size then // fs.size is accettable,  $3B should come out
      bb := $3B;
    case bb of
      $2C: // image descriptor
        begin
          fs.read(imdesc, sizeof(TGIFImDes));
          // write image descriptor
          fm.write(bb, 1);
          fm.write(imdesc, sizeof(TGIFImDes));
          // Decode FLAGS of image descriptor
          imDimLocalColormap := (imdesc.Flags and $07);
          imLocalColormap := (imdesc.Flags and $80) <> 0;
          // Read and Write, if present, the local colormap
          if imLocalColormap then
            IECopyFrom(fm, fs, 3 * (2 shl imDimLocalColormap));
          // Read and Write image data
          fs.read(bb, 1);   // read lzw bit size
          fm.write(bb, 1);  // write lzw bit size
          repeat
            fs.read(bb, 1);   // read block size
            fm.write(bb, 1);  // write block size
            if fs.position + bb > fs.size then
            begin
              Progress.Aborting^ := True;
              break;
            end;
            if bb > 0 then
              IECopyFrom(fm, fs, bb);
          until (bb = 0) or (fs.position >= fs.size);
          //
          inc(numi);
          if idx = numi then
          begin
            if assigned(Progress.fOnProgress) then
              Progress.fOnProgress(Progress.Sender, 70);
            if Progress.Aborting^ then
            begin
              if FreeW then
                FreeAndNil(WBitmap);
              FreeAndNil(fm);
              exit;
            end;
            // idx is the first image to insert
            InsertImage;
            idx := -1; // mark as inserted
            inc(numi);
          end;
        end;
      $21: // Estensioni
        begin
          fs.read(ExtType, 1); // extension type
          if (ExtType = $F9) and (idx = numi) then
          begin
            // idx is the first image
            InsertImage;
            idx := -1; // mark as inserted
            inc(numi);
          end;
          bb := $21;
          fm.write(bb, 1);      // save extension marker
          fm.write(ExtType, 1); // save extension type
          repeat
            fs.read(ExtLen, 1); // extension length (0=end)
            fm.write(ExtLen, 1);
            if ExtLen > 0 then
            begin
              if (ExtLen + fs.position) > fs.size then
              begin
                if fs.size - fs.position > 0 then
                  IECopyFrom(fm, fs, fs.size - fs.position);
              end
              else
              begin
                if ExtLen > 0 then
                  IECopyFrom(fm, fs, ExtLen);
              end;
            end;
          until (ExtLen = 0) or (fs.Position >= fs.size);
        end;
      $3B: // end of GIF
        begin
          if idx = numi then
          begin
            // the GIF was empty
            InsertImage;
            inc(numi);
          end;
          bb := $3B;
          fm.write(bb, 1);
          break;
        end;
    end; // end of CASE
  until false;
  if FreeW then
    FreeAndNil(WBitmap);
  fs.position := p1;
  IECopyFrom(fs, fm, 0);
  fs.Size := fm.size;
  FreeAndNil(fm);
  result := numi;
  if assigned(Progress.fOnProgress) and not Progress.Aborting^ then
    Progress.fOnProgress(Progress.Sender, 100);
end;

end.
