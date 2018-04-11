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

unit ietgafil;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, Classes, Graphics, SysUtils, ImageEnIO, hyiedefs, hyieutils;

procedure ReadTGAStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; var AlphaChannel: TIEMask; IgnoreAlpha: boolean);
procedure WriteTGAStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; AlphaChannel: TIEMask);
function TryTGA(Stream: TStream): boolean;

implementation

uses ImageEnProc, neurquant, ImageEnView, ieview, iesettings;

{$R-}

type
  TGAHeader = packed record
    IdentSize: Byte;      // length of Identifier String
    ColorMaptype: Byte;   // 0 = no map
    Imagetype: Byte;      // image type
    ColorMapStart: Word;  // index of first color map entry
    ColorMapLength: Word; // number of entries in color map
    ColorMapBits: Byte;   // size of color map entry (15, 16, 24, 32)
    XStart: Word;         // x origin of image
    YStart: Word;         // y origin of image
    Width: Word;          // width of image
    Height: Word;         // height of image
    Bits: Byte;           // pixel size (8, 16, 24, 32)
    Descriptor: Byte;     // image descriptor
  end;

  TGAFooter = packed record
    ExtensionArea: dword;
    DeveloperDir: dword;
    Signature: array[0..17] of AnsiChar; // must be 'TRUEVISION-XFILE.\0'
  end;

  TGAExtension = packed record
    ExtSize: word;
    AuthorName: array[0..40] of AnsiChar;
    AuthorComments: array[0..323] of AnsiChar;
    DateTime: array[0..5] of word;
    JobName: array[0..40] of AnsiChar;
    JobTime: array[0..2] of word;
    SoftwareID: array[0..40] of AnsiChar;
    SoftwareVer: array[0..2] of byte;
    KeyColor: array[0..3] of byte;
    AspectRatio: array[0..1] of word;
    Gamma: array[0..1] of word;
    ColorCorrection: dword;
    PostageStamp: dword;
    ScanLine: dword;
    AttributesType: byte;
  end;

  TRC = record
    IndexData: array[0..8192 - 1] of Byte;
    Palette256: array[0..255] of TRGB;
    alpha256: array [0..255] of byte;
    hasalpha256: boolean;
    TempArrayD: PBYTEROW;
    //TempArrayD2: PBYTEROW;
    TempArrayDBIg: PBYTEROW;
    TempArrayAlpha: PBYTEROW;
    CompRow: PBYTEROW;
    Index1: Word;
    Index2: Word;
    Newtype: boolean;
    Footer: TGAFooter;
    Extension: TGAExtension;
    sbase: integer;
    RemSize, RemCode: integer;
  end;
  PRC = ^TRC;



function TryTGA(Stream: TStream): boolean;
var
  TGAHead: TGAHeader;
  c: AnsiChar;
  B: Byte;
  sp: int64;
begin
  sp := Stream.Position;
  // Read Targa Header
  Stream.Read(TGAHead, Sizeof(TGAHeader));
  if (TGAHead.Imagetype in [1, 2, 3, 9, 10, 11]) and (TGAHead.Bits in [1, 4, 8, 16, 24, 32]) and
    (TGAHead.ColorMaptype < 2) and (TGAHead.Width>0) and (TGAHead.Height>0) then
  begin
    result := true;
    Stream.Position := sp;
    Stream.Read(c, 1);
    if c = 'P' then
    begin
      Stream.Read(b, 1);
      if (b - 48 > 01) and (b - 48 < 7) then
      begin
        Stream.Read(c, 1);
        if (c <> ' ') and (c <> '#') and (c <> #$0A) then
          result := false;
      end;
    end;
  end
  else
    result := false;
  Stream.Position := sp;
end;

procedure SetUpMaskGrayPalette(var rc: TRC);
var
  J: integer;
begin
  for J := 0 to 255 do
    with rc.Palette256[J] do
    begin
      r := J;
      g := J;
      b := J;
    end;
end;

procedure MakeGenPalette(var rc: TRC);
var
  X: integer;
  R, G, B: Word;
begin
  with rc do
  begin
    X := 0;
    for R := 0 to 7 do
      for G := 0 to 7 do
        for B := 0 to 3 do
        begin
          Palette256[X].r := ((R + 1) * 8 - 1) * 4;
          Palette256[X].g := ((G + 1) * 8 - 1) * 4;
          Palette256[X].b := ((B + 1) * 16 - 1) * 4;
          Inc(X);
        end;
  end;
end;



procedure ReadTGAStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean; var AlphaChannel: TIEMask; IgnoreAlpha: boolean);
var
  rc: PRC;
  Width: Word;
  Height: Word;
  BitsPerPixel: SmallInt;
  Compressed: Boolean;
  TGAHead: TGAHeader;
  FileOk: Boolean;
  //
  procedure FileGetMore;
  var
    NumRead: integer;
  begin
    with rc^ do
    begin
      NumRead := Stream.Size - Stream.Position;
      //FillChar(IndexData, 8192, 0);
      if NumRead < 8192 then
      begin
        Stream.Read(IndexData, NumRead);
        Index1 := NumRead;
      end
      else
      begin
        Stream.Read(IndexData, 8192);
        Index1 := 8192;
      end;
      Index2 := 0;
    end;
  end;
  //
  procedure FastGetBytes(var Ptr1; NumBytes: Word);
  var
    X: Integer;
  begin
    with rc^ do
    begin
      // if we have enough the block it! Otherwise do one at a time!
      if Index1 < NumBytes then
      begin
        if Index1 = 0 then
          FileGetMore;
        for X := 0 to NumBytes - 1 do
        begin
          TBYTEROW(Ptr1)[X] := IndexData[Index2];
          Inc(Index2);
          Dec(Index1);
          if Index1 = 0 then
            FileGetMore;
        end;
      end
      else
      begin
        // Block it fast!
        Move(IndexData[Index2], TBYTEROW(Ptr1)[0], NumBytes);
        Index2 := Index2 + Numbytes;
        Index1 := Index1 - NumBytes;
      end;
    end;
  end;
  //
  function FastGetByte: Byte;
  begin
    with rc^ do
    begin
      if Index1 = 0 then
        FileGetMore;
      FastGetByte := IndexData[Index2];
      Inc(Index2);
      Dec(Index1);
    end;
  end;
  //
  function FastGetWord: Word;
  begin
    FastGetWord := Word(FastGetByte) + Word(FastGetByte) * 256;
  end;
  //
  procedure ReadTGAFileHeader(var FileOk: Boolean;
    var Width: Word; var Height: Word; var BitsPerPixel: SmallInt;
    var Compressed: Boolean);
  var
    W1: Word;
    I: integer;
    ss: AnsiString;
  begin
    with rc^ do
    begin
      // Read Targa footer (if exists)
      Stream.Seek(-sizeof(TGAFooter), soEnd);
      Stream.Read(Footer, sizeof(TGAFooter));
      NewType := Footer.Signature = 'TRUEVISION-XFILE.';
      // default values
      IOParams.TGA_Author := '';
      IOParams.TGA_Date := date;
      IOParams.TGA_ImageName := '';
      IOParams.TGA_Background := CreateRGB(0, 0, 0);
      IOParams.TGA_AspectRatio := 1;
      IOParams.TGA_Gamma := 2.2;
      if NewType then
        with Extension do
        begin
          Stream.Position := sbase + integer(Footer.ExtensionArea);
          Stream.Read(Extension, sizeof(TGAExtension));
          IOParams.TGA_Author := AuthorName;
          try
            if (DateTime[0] <> 0) and (DateTime[1] <> 0) and (DateTime[2] <> 0) and
              (DateTime[2] > 0) and (DateTime[2] < 2500) and
              (DateTime[0] > 0) and (DateTime[0] < 13) and
              (DateTime[1] > 0) and (DateTime[1] < 32) and
              (DateTime[3] < 24) and
              (DateTime[4] < 60) and
              (DateTime[5] < 60) then
              IOParams.TGA_Date := EncodeDate(DateTime[2], DateTime[0], DateTime[1]) +
                EncodeTime(DateTime[3], DateTime[4], DateTime[5], 0);
          except
          end;
          IOParams.TGA_ImageName := JobName;
          IOParams.TGA_Background := CreateRGB(KeyColor[1], KeyColor[2], KeyColor[3]);
          if (AspectRatio[0] <> 0) and (AspectRatio[1] <> 0) then
            IOParams.TGA_AspectRatio := AspectRatio[0] / AspectRatio[1];
          if (Gamma[0] <> 0) and (Gamma[1] <> 0) then
            IOParams.TGA_Gamma := Gamma[0] / Gamma[1];
        end;
      Stream.Position := sbase;
      // Read Targa Header
      FastGetBytes(TGAHead, Sizeof(TGAHeader));
      IOParams.TGA_XPos := TGAHead.XStart;
      IOParams.TGA_YPos := TGAHead.YStart;
      FileOk := (TGAHead.Imagetype in [1, 2, 3, 9, 10, 11]) and (TGAHead.Bits in [1, 4, 8, 16, 24, 32]);
      if FileOk then
      begin
        Width := TGAHead.Width;
        Height := TGAHead.Height;
        BitsPerPixel := TGAHead.Bits;
        SetLength(ss, TGAHead.IdentSize);
        FastGetBytes(ss[1], TGAHead.IdentSize);
        IOParams.TGA_Descriptor := ss;
        // Read in colormap
        MakeGenPalette(rc^);
        if TGAHead.ColorMaptype = 1 then
        begin
          case TGAHead.ColorMapBits of
            15, 16:
              for I := TGAHead.ColorMapStart to TGAHead.ColorMapStart + TGAHead.ColorMapLength - 1 do
              begin
                W1 := FastGetWord;
                Palette256[I].r := (((W1 shr 10) and $1F) shl 1) * 4;
                Palette256[I].g := (((W1 shr 5) and $1F) shl 1) * 4;
                Palette256[I].b := (((W1 shr 0) and $1F) shl 1) * 4;
              end;
            24:
              for I := TGAHead.ColorMapStart to TGAHead.ColorMapStart + TGAHead.ColorMapLength - 1 do
              begin
                Palette256[I].b := FastGetByte;
                Palette256[I].g := FastGetByte;
                Palette256[I].r := FastGetByte;
              end;
            32:
              begin
                hasalpha256 := true;
                for I := TGAHead.ColorMapStart to TGAHead.ColorMapStart + TGAHead.ColorMapLength - 1 do
                begin
                  Palette256[I].b := FastGetByte;
                  Palette256[I].g := FastGetByte;
                  Palette256[I].r := FastGetByte;
                  alpha256[I] := FastGetByte;
                end;
              end;
          end;
          IOParams.FreeColorMap;
          IOParams.fColorMapCount := TGAHead.ColorMapLength;
          IOParams.fColorMap := allocmem(TGAHead.ColorMapLength * sizeof(TRGB));
          move(Palette256[0], IOParams.fcolorMap^[0], TGAHead.ColorMapLength * sizeof(TRGB));
          IOParams.TGA_GrayLevel := false;
        end
        else
        if BitsPerPixel = 8 then
        begin
          // gray level image (8bpp but without colormap)
          SetUpMaskGrayPalette(rc^);
          IOParams.TGA_GrayLevel := true;
        end
        else
        if BitsPerPixel = 1 then
        begin
          // bilevel image
          Palette256[0] := CreateRGB(0, 0, 0);
          Palette256[1] := CreateRGB(255, 255, 255);
        end;
        Compressed := TGAHead.Imagetype in [9, 10, 11];
        IOParams.TGA_Compressed := Compressed;
      end;
    end;
  end;
  //
const
  MaskTable: array[0..7] of Byte = (128, 64, 32, 16, 8, 4, 2, 1);
var
  II: Word;
  LineBytes: Word;
  StartLine, IncLine, I: SmallInt;
  Ptr1: Pointer;
  //
  procedure PixelSwapArray(var TempArrayD; Wide: Word);
  var
    W, X, Y, Z: integer;
    Byte1, Byte2, Byte3: Byte;
  begin
    // Should I do 1 byte pixel or 3 byte pixels
    case BitsPerPixel of
      8:
        begin
          Y := Wide shr 1;
          Z := Wide - 1;
          for X := 0 to Y - 1 do
          begin
            Byte1 := TBYTEROW(TempArrayD)[X];
            TBYTEROW(TempArrayD)[X] := TBYTEROW(TempArrayD)[Z];
            TBYTEROW(TempArrayD)[Z] := Byte1;
            Dec(Z);
          end;
        end;
      24:
        begin
          Y := (Wide div 3) div 2;
          Z := Wide - 3;
          W := 0;
          for X := 0 to Y - 1 do
          begin
            Byte1 := TBYTEROW(TempArrayD)[W + 0];
            Byte2 := TBYTEROW(TempArrayD)[W + 1];
            Byte3 := TBYTEROW(TempArrayD)[W + 2];
            TBYTEROW(TempArrayD)[W + 0] := TBYTEROW(TempArrayD)[Z + 0];
            TBYTEROW(TempArrayD)[W + 1] := TBYTEROW(TempArrayD)[Z + 1];
            TBYTEROW(TempArrayD)[W + 2] := TBYTEROW(TempArrayD)[Z + 2];
            TBYTEROW(TempArrayD)[Z + 0] := Byte1;
            TBYTEROW(TempArrayD)[Z + 1] := Byte2;
            TBYTEROW(TempArrayD)[Z + 2] := Byte3;
            dec(Z, 3);
            inc(W, 3);
          end;
        end;
      32:
        begin
          Y := Wide shr 3;
          Z := Wide - 4;
          W := 0;
          for X := 0 to Y - 1 do
          begin
            Byte1 := TBYTEROW(TempArrayD)[W + 0];
            Byte2 := TBYTEROW(TempArrayD)[W + 1];
            Byte3 := TBYTEROW(TempArrayD)[W + 2];
            TBYTEROW(TempArrayD)[W + 0] := TBYTEROW(TempArrayD)[Z + 0];
            TBYTEROW(TempArrayD)[W + 1] := TBYTEROW(TempArrayD)[Z + 1];
            TBYTEROW(TempArrayD)[W + 2] := TBYTEROW(TempArrayD)[Z + 2];
            TBYTEROW(TempArrayD)[Z + 0] := Byte1;
            TBYTEROW(TempArrayD)[Z + 1] := Byte2;
            TBYTEROW(TempArrayD)[Z + 2] := Byte3;
            Z := Z - 4;
            W := W + 4;
          end;
        end;
    end;
  end;
  //
  procedure TGAReverse(var TempArrayD: TBYTEROW);
  begin
    if TGAHead.Descriptor and $10 <> 0 then
      PixelSwapArray(TempArrayD, LineBytes);
  end;
  //
  procedure TGA16_ANY_U(var Z: integer; var TempArrayD; Width: Word);
  var
    W1: Word;
    I: integer;
    R, G, B: Byte;
  begin
    for I := 0 to Width - 1 do
    begin
      W1 := FastGetWord;
      R := ((W1 shr 10) and $1F) shl 3;
      G := ((W1 shr 5) and $1F) shl 3;
      B := ((W1 shr 0) and $1F) shl 3;
      TBYTEROW(TempArrayD)[Z + 0] := B;
      TBYTEROW(TempArrayD)[Z + 1] := G;
      TBYTEROW(TempArrayD)[Z + 2] := R;
      inc(z, 3);
    end;
  end;
  //
  procedure TGA24_ANY_U(var Z: integer; Flag: Byte; var TempArrayD; Width: Word; TempArrayAlpha: PBYTEROW);
  var
    I: integer;
  begin
    for I := 0 to Width - 1 do
    begin
      TBYTEROW(TempArrayD)[Z + 0] := FastGetByte;
      TBYTEROW(TempArrayD)[Z + 1] := FastGetByte;
      TBYTEROW(TempArrayD)[Z + 2] := FastGetByte;
      if Flag = 1 then
      begin
        if TempArrayAlpha <> nil then
          TempArrayAlpha[Z div 3] := FastGetByte
        else
          FastGetByte;
      end;
      inc(Z, 3);
    end;
  end;
  //
  procedure ReadTGALine;
  var
    Size, LineSize: integer;
    W1: Word;
    Z: integer;
    R, G, B, B1: Byte;
    procedure do8;
    var
      I: integer;
    begin
      with rc^ do
        for I := 0 to Size - 1 do
        begin
          TempArrayD^[Z] := ((R shl 5) + (G shl 6) + (B * 12)) div 108;
          Inc(Z);
        end;
    end;
    procedure do24Raw;
    var
      I, Z: integer;
    begin
      with rc^ do
      begin
        Z := 0;
        for I := 0 to Width - 1 do
        begin
          B1 := FastGetByte;
          TempArrayD^[Z + 0] := Palette256[B1].b;
          TempArrayD^[Z + 1] := Palette256[B1].g;
          TempArrayD^[Z + 2] := Palette256[B1].r;
          if (TempArrayAlpha<>nil) and hasalpha256 then
            TempArrayAlpha[Z div 3] := alpha256[B1];
          inc(Z, 3);
        end;
      end;
    end;
    procedure do24RawPart;
    var
      I: integer;
    begin
      with rc^ do
        for I := 0 to Size - 1 do
        begin
          B1 := FastGetByte;
          TempArrayD^[Z + 0] := Palette256[B1].b;
          TempArrayD^[Z + 1] := Palette256[B1].g;
          TempArrayD^[Z + 2] := Palette256[B1].r;
          if (TempArrayAlpha<>nil) and hasalpha256 then
            TempArrayAlpha[Z div 3] := alpha256[B1];
          inc(Z, 3);
        end;
    end;
    procedure do24Fill(B1: Byte);
    var
      I: integer;
      R, G, B: Byte;
    begin
      with rc^ do
      begin
        R := Palette256[B1].r;
        G := Palette256[B1].g;
        B := Palette256[B1].b;
        for I := 0 to Size - 1 do
        begin
          TempArrayD^[Z + 0] := B;
          TempArrayD^[Z + 1] := G;
          TempArrayD^[Z + 2] := R;
          if (TempArrayAlpha<>nil) and hasalpha256 then
            TempArrayAlpha[Z div 3] := alpha256[B1];
          inc(Z, 3);
        end;
      end;
    end;
    procedure do24;
    var
      I: integer;
    begin
      with rc^ do
        for I := 0 to Size - 1 do
        begin
          TempArrayD^[Z + 0] := B;
          TempArrayD^[Z + 1] := G;
          TempArrayD^[Z + 2] := R;
          inc(Z, 3);
        end;
    end;
  var
    col, q: integer;
  begin
    // ReadTGALine
    with rc^ do
    begin
      if BitsPerPixel = 1 then
        LineSize := (Width + 7) shr 3
      else
        LineSize := Width;
      // Uncompressed Lines
      if TGAHead.Imagetype in [1, 2, 3] then
        case BitsPerPixel of
          1: FastGetBytes(TempArrayD^[0], LineBytes);
          8: do24Raw;
          16:
            begin
              Z := 0;
              TGA16_ANY_U(Z, TempArrayD^[0], Width);
            end;
          24:
            begin
              Z := 0;
              TGA24_ANY_U(Z, 0, TempArrayD^[0], Width, TempArrayAlpha);
            end;
          32:
            begin
              Z := 0;
              TGA24_ANY_U(Z, 1, TempArrayD^[0], Width, TempArrayAlpha);
            end;
        end
      else
      begin
        // Compressed Lines
        Z := 0;
        col := 0;
        repeat
          if RemCode>-1 then
          begin
            B1 := RemCode;
            Size := RemSize;
            RemCode := -1;
          end
          else
          begin
            B1 := FastGetByte;
            Size := (B1 and $7F) + 1;
          end;
          if Size+col>LineSize then
          begin
            RemSize := (Size+Col)-LineSize;
            RemCode := B1;
            Size := LineSize-col;
          end;
          if (B1 and $80) <> 0 then
          begin
            // Run length packet
            case BitsPerPixel of
              1, 8:
                begin
                  B1 := FastGetByte;
                  do24Fill(B1);
                end;
              16:
                begin
                  W1 := FastGetWord;
                  R := ((W1 shr 10) and $1F) shl 3;
                  G := ((W1 shr 5) and $1F) shl 3;
                  B := ((W1 shr 0) and $1F) shl 3;
                  do24;
                end;
              24, 32:
                begin
                  B := FastGetByte;
                  G := FastGetByte;
                  R := FastGetByte;
                  if BitsPerPixel = 32 then
                  begin
                    B1 := FastGetByte;
                    if TempArrayAlpha <> nil then
                      for q := col to col+Size-1 do
                        TempArrayAlpha[q] := B1;
                  end;
                  do24;
                end;
            end;
          end
          else
            // Single bytes
            case BitsPerPixel of
              1, 8:
                do24RawPart;
              16:
                TGA16_ANY_U(Z, TempArrayD^[0], Size);
              24:
                TGA24_ANY_U(Z, 0, TempArrayD^[0], Size, TempArrayAlpha);
              32:
                TGA24_ANY_U(Z, 1, TempArrayD^[0], Size, TempArrayAlpha);
            end;
          inc(col, Size);
        until col >= LineSize;
      end;
    end;
  end;
  //
begin
  new(rc);
  try
    zeromemory(rc, sizeof(TRC));
    with rc^ do
    begin
      // init alpha
      for i := 0 to 255 do
        alpha256[i] := 255;
      hasalpha256 := false;

      // Read Targa Stream
      sbase := Stream.Position;
      Index1 := 0;
      Index2 := 0;
      FileOk := true;
      ReadTgaFileHeader(FileOK, Width, Height, BitsPerPixel, Compressed);
      if FileOK then
      begin
        IOParams.Width := Width;
        IOParams.Height := Height;
        IOParams.OriginalWidth := Width;
        IOParams.OriginalHeight := Height;
        IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
        IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
        IOParams.ImageCount := 1;
        case BitsPerPixel of
          1:
            begin
              IOParams.BitsPerSample := 1;
              IOParams.SamplesPerPixel := 1;
            end;
          8:
            begin
              IOParams.BitsPerSample := 8;
              IOParams.SamplesPerPixel := 1;
            end;
          16:
            begin
              IOParams.BitsPerSample := 5;
              IOParams.SamplesPerPixel := 3;
            end;
          24:
            begin
              IOParams.BitsPerSample := 8;
              IOParams.SamplesPerPixel := 3;
            end;
          32:
            begin
              IOParams.BitsPerSample := 8;
              IOParams.SamplesPerPixel := 4;
            end;
        end;
        if not Preview then
        begin
          Progress.per1 := 100 / Height;
          Progress.val := 0;
          if BitsPerPixel = 1 then
          begin
            Bitmap.Allocate(Width, Height, ie1g);
            LineBytes := (Width + 7) shr 3;
          end
          else
          begin
            Bitmap.Allocate(Width, Height, ie24RGB);
            LineBytes := Width * 3;
          end;
          TempArrayD := nil;
          TempArrayAlpha := nil;
          try
            GetMem(TempArrayD, LineBytes);
            if (not IgnoreAlpha) and ( ((BitsPerPixel = 32) (*and ((TGAHead.Descriptor and 8) <> 0)*)) or (TGAHead.ColorMapBits=32) ) then
            begin
              if not assigned(AlphaChannel) then
                AlphaChannel := TIEMask.Create;
              AlphaChannel.AllocateBits(Width, Height, 8);
              AlphaChannel.Fill(255);
              getmem(TempArrayAlpha, Width);
              FillChar(TempArrayAlpha^, Width, 255);
              AlphaChannel.Full := false;
            end
            else
              TempArrayAlpha := nil;
            if ((ord(TGAHead.Descriptor) and 32) <> 32) and ((ord(TGAHead.Descriptor) and 16) <> 16) then
            begin
              StartLine := Height - 1;
              IncLine := -1;
            end
            else
            begin
              StartLine := 0;
              IncLine := 1;
            end;
            RemCode := -1;
            I := StartLine;
            II := 0;
            if TGAHead.Imagetype in [1, 2, 3, 9, 10, 11] then
              repeat
                ReadTGALine;
                TGAReverse(TempArrayD^);
                Ptr1 := BitMap.ScanLine[I];
                // Copy the data
                Move(TempArrayD^, Ptr1^, LineBytes);
                // copy alpha
                if TempArrayAlpha <> nil then
                  copymemory(AlphaChannel.Scanline[I], TempArrayAlpha, Width);
                Inc(II);
                I := I + IncLine;
                with Progress do
                begin
                  inc(val);
                  if assigned(fOnProgress) then
                    fOnProgress(Sender, trunc(per1 * val));
                end;
              until (II >= Height) or (Progress.Aborting^=True)
            else
              Progress.Aborting^ := True;
          finally
            FreeMem(TempArrayD);
            if TempArrayAlpha <> nil then
              FreeMem(TempArrayAlpha);
          end;
        end; // not preview
      end
      else
        Progress.Aborting^ := True;
    end;
  finally
    dispose(rc);
  end;
  if assigned(AlphaChannel) then
  begin
    AlphaChannel.SyncRect;
    if AlphaChannel.IsEmpty then
      FreeAndNil(AlphaChannel);
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////

procedure WriteTGAStream(Stream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; AlphaChannel: TIEMask);
var
  rc: PRC;
  TGAHead: TGAHeader;
  OutputWidth: integer;
  OutputHeight: integer;
  DestBitsPerPixel: integer;
  OrigBitsPerPixel: integer;
  NewLine: PBYTEROW;
  XBitmap: TIEBitmap;
  qt: TIEQuantizer;
  nullpr: TProgressRec;
  //
  procedure TGAWriteHeader;
  begin
    TGAHead.IdentSize := Length(IOParams.TGA_Descriptor) + 1;
    TGAHead.Bits := DestBitsPerPixel;
    TGAHead.Descriptor := 0;
    case DestBitsPerPixel of
      1:
        begin
          // bilevel image
          TGAHead.ColorMaptype := 0;
          if IOParams.TGA_Compressed then
            TGAHead.Imagetype := 11
          else
            TGAHead.Imagetype := 3;
          TGAHead.ColorMapStart := 0;
          TGAHead.ColorMapLength := 0;
          TGAHead.ColorMapBits := 0;
        end;
      4, 8:
        begin
          if (DestBitsPerPixel = 8) and IOParams.TGA_GrayLevel then
          begin
            // gray scaled image
            TGAHead.ColorMaptype := 0;
            if IOParams.TGA_Compressed then
              TGAHead.Imagetype := 11
            else
              TGAHead.Imagetype := 3;
            TGAHead.ColorMapStart := 0;
            TGAHead.ColorMapLength := 1 shl DestBitsPerPixel;
            TGAHead.ColorMapBits := 24;
          end
          else
          begin
            // colormapped image
            TGAHead.ColorMaptype := 1;
            if IOParams.TGA_Compressed then
              TGAHead.Imagetype := 9
            else
              TGAHead.Imagetype := 1;
            TGAHead.ColorMapStart := 0;
            TGAHead.ColorMapLength := 1 shl DestBitsPerPixel;
            TGAHead.ColorMapBits := 24;
          end;
        end;
      24:
        begin
          // true color image
          TGAHead.ColorMaptype := 0;
          if IOParams.TGA_Compressed then
            TGAHead.Imagetype := 10
          else
            TGAHead.Imagetype := 2;
          TGAHead.ColorMapStart := 0;
          TGAHead.ColorMapLength := 0;
          TGAHead.ColorMapBits := 0;
        end;
      32:
        begin
          // true color image with alpha (AlphaChannel must be valid and not empty)
          TGAHead.ColorMaptype := 0;
          if IOParams.TGA_Compressed then
            TGAHead.Imagetype := 10
          else
            TGAHead.Imagetype := 2;
          TGAHead.ColorMapStart := 0;
          TGAHead.ColorMapLength := 0;
          TGAHead.ColorMapBits := 0;
          TGAHead.Descriptor := TGAHead.Descriptor or $8; // alpha channel with 8 bit
        end;
    end;
    TGAHead.XStart := IOParams.TGA_XPos;
    TGAHead.YStart := IOParams.TGA_YPos;
    TGAHead.Width := OutputWidth;
    TGAHead.Height := OutputHeight;
    TGAHead.Descriptor := TGAHead.Descriptor or $20; // isn't IOParams.TGA_Descriptor!!
    SafeStreamWrite(Stream, Progress.Aborting^, TGAHead, Sizeof(TGAHead));
    SafeStreamWrite(Stream, Progress.Aborting^, PAnsiChar(IOParams.TGA_Descriptor)^, Length(IOParams.TGA_Descriptor) + 1);
    if TGAHead.ColorMaptype = 1 then
      SafeStreamWrite(Stream, Progress.Aborting^, rc^.Palette256[0], (1 shl DestBitsPerPixel) * 3);
  end;
  //
  // convert to gray TempArrayDBIG (TBYTEROW)
  // only for 8bpp
  procedure TGAConvertToGray;
  var
    i: integer;
  begin
    for i := 0 to OutputWidth - 1 do
      with rc^, Palette256[TempArrayDBIG^[i]] do
        TempArrayDBIG^[i] := _RGBToGray(CreateRGB(r, g, b));
  end;
  //
// compress TempArrayDBIG (TBYTEROW) and saves it in CompRow and then in the stream
  // implemented only for 8 and 24 bit
  procedure TGACompress;
  var
    p, bwidth: integer;
    l8: byte;
    l24: TRGB;
    l: byte;
    warr: pbyte;
  begin
    with rc^ do
    begin
      warr := pbyte(@CompRow[0]);
      p := 0;
      case DestBitsPerPixel of
        8: // encode 8 bit row (1 byte)
          begin
            bwidth := OutputWidth;
            repeat
              l8 := TempArrayDBIG^[p];
              inc(p);
              l := 1;
              while (p < bwidth) and (l8 = TempArrayDBIG^[p]) and (l < 128) do
              begin
                inc(p);
                inc(l);
              end;
              if l > 1 then
              begin
                // encode as run-length packet
                warr^ := $80 or (l - 1);
                inc(warr);
                warr^ := l8;
                inc(warr);
              end
              else
              begin
                // encode as raw packet
                dec(p);
                l := 0;
                while (p < bwidth - 1) and (TempArrayDBIG^[p] <> TemparrayDBIG^[p + 1]) and (l < 127) do
                begin
                  inc(p);
                  inc(l);
                end;
                if p = bwidth - 1 then
                begin
                  inc(p);
                  inc(l);
                end;
                warr^ := l - 1;
                inc(warr);
                CopyMemory(warr, @TempArrayDBIG^[p - l], l);
                inc(warr, l);
              end;
            until p >= bwidth;
          end;
        24: // encode 24 bit row (3 byte)
          begin
            bwidth := OutputWidth * 3;
            repeat
              l24 := PRGB(@TempArrayDBIG^[p])^;
              inc(p, 3);
              l := 1;
              while (p < bwidth) and equalrgb(l24, PRGB(@TempArrayDBIG^[p])^) and (l < 128) do
              begin
                inc(p, 3);
                inc(l);
              end;
              if l > 1 then
              begin
                // encode as run-length packet
                warr^ := $80 or (l - 1);
                inc(warr);
                PRGB(warr)^ := l24;
                inc(warr, 3);
              end
              else
              begin
                // encode as raw packet
                dec(p, 3);
                l := 0;
                while (p < bwidth - 3) and (not equalrgb(PRGB(@TempArrayDBIG^[p])^, PRGB(@TemparrayDBIG^[p + 3])^)) and (l < 127) do
                begin
                  inc(p, 3);
                  inc(l);
                end;
                if p = bwidth - 3 then
                begin
                  inc(p, 3);
                  inc(l);
                end;
                warr^ := l - 1;
                inc(warr);
                CopyMemory(warr, @TempArrayDBIG^[p - l * 3], l * 3);
                inc(warr, l * 3);
              end;
            until p >= bwidth;
          end;
      end;
      SafeStreamWrite(Stream, Progress.Aborting^, CompRow^[0], uint64(warr) - uint64(@CompRow[0]));
    end;
  end;
  //
  procedure TGAWriteBody;
  var
    i, l, x: integer;
    oarr, px, al, sp: pbyte;
  begin
    with rc^ do
    begin
      i := 0;
      Progress.per1 := 100 / OutputHeight;
      Progress.val := 0;

      if DestBitsPerPixel = 32 then
        getmem(oarr, XBitmap.Width * 4)
      else
        oarr := nil;

      try

        repeat
          TempArrayD := XBitMap.ScanLine[i];
          if DestBitsPerPixel = 32 then
            l := _ConvertXBitsToYBits(TempArrayD^, TempArrayDBIG^, OrigBitsPerPixel, 24, OutputWidth, Palette256, qt)
          else
            l := _ConvertXBitsToYBits(TempArrayD^, TempArrayDBIG^, OrigBitsPerPixel, DestBitsPerPixel, OutputWidth, Palette256, qt);
          if IOParams.TGA_GrayLevel and (DestBitsPerPixel = 8) then
            TGAConvertToGray;
          if IOParams.TGA_Compressed then
            TGACompress
          else
          begin
            if DestBitsPerPixel = 32 then
            begin
              // add and save alpha channel
              al := AlphaChannel.Scanline[i];
              px := oarr;
              sp := pbyte(TempArrayDBIG);
              for x := 0 to XBitmap.Width - 1 do
              begin
                px^ := sp^;
                inc(px);
                inc(sp);
                px^ := sp^;
                inc(px);
                inc(sp);
                px^ := sp^;
                inc(px);
                inc(sp);
                px^ := al^;
                inc(px);
                inc(al);
              end;
              SafeStreamWrite(Stream, Progress.Aborting^, oarr^, 4 * XBitmap.Width);
            end
            else
              SafeStreamWrite(Stream, Progress.Aborting^, TempArrayDBIG^[0], l);
          end;
          inc(i);
          with Progress do
          begin
            inc(val);
            if assigned(fOnProgress) then
              fOnProgress(Sender, trunc(per1 * val));
          end;
        until (i >= OutputHeight);

      finally
        if DestBitsPerPixel = 32 then
          freemem(oarr);
      end;

    end;
  end;
  //
  procedure TGAWriteExtension;
  var
    ms: word;
  begin
    with rc^, Extension do
    begin
      zeromemory(@Extension, sizeof(TGAExtension));
      IEStrCopy(AuthorName, PAnsiChar(IOParams.TGA_Author));
      IEStrCopy(JobName, PAnsiChar(IOParams.TGA_ImageName));
      DecodeDate(IOParams.TGA_Date, DateTime[2], DateTime[0], DateTime[1]);
      DecodeTime(IOParams.TGA_Date, DateTime[3], DateTime[4], DateTime[5], ms);
      KeyColor[0] := 0;
      KeyColor[1] := IOParams.TGA_Background.r;
      KeyColor[2] := IOParams.TGA_Background.g;
      KeyColor[3] := IOParams.TGA_Background.b;
      AspectRatio[0] := trunc(IOParams.TGA_AspectRatio) * 10000;
      AspectRatio[1] := 10000;
      Gamma[0] := trunc(IOParams.TGA_Gamma) * 10000;
      Gamma[1] := 10000;
      SafeStreamWrite(Stream, Progress.Aborting^, Extension, sizeof(TGAExtension));
      if DestBitsPerPixel = 32 then
        AttributesType := 0;
    end;
  end;
  //
  procedure TGAWriteFooter;
  begin
    with rc^ do
    begin
      Footer.Signature := 'TRUEVISION-XFILE.' + AnsiChar(0);
      Footer.DeveloperDir := 0;
      Footer.ExtensionArea := sbase + Stream.Position;
      TGAWriteExtension;
      SafeStreamWrite(Stream, Progress.Aborting^, Footer, sizeof(TGAFooter));
    end;
  end;
  //
var
  rgb1, rgb2: TRGB;
begin
  if (Bitmap.PixelFormat <> ie24RGB) and (Bitmap.PixelFormat <> ie1g) then
    exit;
  with nullpr do
  begin
    Aborting := Progress.Aborting;
    fOnProgress := nil;
    Sender := nil;
  end;
  XBitmap := Bitmap;
  qt := nil;
  new(rc);
  zeromemory(rc, sizeof(TRC));

  try

    with rc^ do
    begin
      // Write TARGA Stream.
      sbase := Stream.Position;
      Index1 := 0;
      Index2 := 0;
      if Bitmap.PixelFormat = ie24RGB then
        OrigBitsPerPixel := 24
      else
        OrigBitsPerPixel := 1;
      if IOParams.SamplesPerPixel = 1 then
      begin
        case IOParams.BitsPerSample of
          1:
            begin
              if OrigBitsPerPixel = 24 then
              begin
                XBitmap := _ConvertTo1bitEx(Bitmap, rgb1, rgb2);
                if XBitmap = nil then
                begin
                  // impossible to convert to 1 bit, converts from color to black/white
                  // 3.0.0
                  XBitmap := TIEBitmap.Create(Bitmap.Width, Bitmap.Height, ie1g);
                  XBitmap.CopyAndConvertFormat(Bitmap);
                end;
                OrigBitsPerPixel := 1;
              end;
              DestBitsPerPixel := 1;
              Palette256[0] := CreateRGB(0, 0, 0);
              Palette256[1] := CreateRGB(255, 255, 255);
              IOParams.TGA_Compressed := false;
            end;
          4:
            begin
              if OrigBitsPerPixel = 24 then
              begin
                IOParams.FreeColorMap;
                IOParams.fColorMapCount := 16;
                getmem(IOParams.fColorMap, 16 * 3);
                qt := TIEQuantizer.Create(Bitmap, IOParams.ColorMap^, 16);
                copymemory(@Palette256[0], IOparams.ColorMap, 16 * 3);
              end;
              DestBitsPerPixel := 4;
              IOParams.TGA_Compressed := false;
            end;
          8:
            begin
              if OrigBitsPerPixel = 24 then
              begin
                IOParams.FreeColorMap;
                IOParams.fColorMapCount := 256;
                getmem(IOParams.fColorMap, 256 * 3);
                qt := TIEQuantizer.Create(Bitmap, IOParams.ColorMap^, 256);
                copymemory(@Palette256[0], IOparams.ColorMap, 256 * 3);
              end;
              DestBitsPerPixel := 8;
            end;
        end;
      end
      else
      begin
        if assigned(AlphaChannel) then
        begin
          DestBitsPerPixel := 32;
          IOParams.TGA_Compressed := false; // alpha+compression not supported
        end
        else
          DestBitsPerPixel := 24;
      end;
      OutputWidth := XBitmap.Width;
      OutputHeight := XBitmap.Height;
      GetMem(NewLine, OutputWidth * 3);
      GetMem(TempArrayDBig, OutputWidth * 3);
      if IOParams.TGA_Compressed then
        GetMem(CompRow, OutputWidth * 3 * 3);
      try
        TGAWriteHeader;
        TGAWriteBody;
        TGAWriteFooter;
      finally
        FreeMem(TempArrayDBig);
        FreeMem(NewLine);
        if IOParams.TGA_Compressed then
          freemem(CompRow);
      end;
    end;

  finally

    dispose(rc);
    if XBitmap <> Bitmap then
      FreeAndNil(XBitmap);
    if assigned(qt) then
      FreeAndNil(qt);

  end;
end;

end.
