(*
   dcraw.c -- Dave Coffin's raw photo decoder
   Copyright 1997-2004 by Dave Coffin, dcoffin a cybercom o net

   This is a portable ANSI C program to convert raw image files from
   any digital camera into PPM format.  TIFF and CIFF parsing are
   based upon public specifications, but no such documentation is
   available for the raw sensor data, so writing this program has
   been an immense effort.

   This code is freely licensed for all uses, commercial and
   otherwise.  Comments, questions, and encouragement are welcome.

   $Revision: 1.215 $
   $Date: 2004/11/05 06:48:50 $

 *)

(*
File version 1006
*)
 
{!!
<FS>List of supported Camera RAW formats<FN>

The following is a list of formats supported in the ImageEn's internal RAW implementation.

A free plug-in is available from the Registered Users download page which is more frequently updated.

Canon PowerShot 600
Canon PowerShot A5
Canon PowerShot A5 Zoom
Canon PowerShot A50
Canon PowerShot Pro70
Canon PowerShot Pro90 IS
Canon PowerShot G1
Canon PowerShot G2
Canon PowerShot G3
Canon PowerShot G5
Canon PowerShot G6
Canon PowerShot S30
Canon PowerShot S40
Canon PowerShot S45
Canon PowerShot S50
Canon PowerShot S70
Canon PowerShot Pro1
Canon EOS D30
Canon EOS D60
Canon EOS 10D
Canon EOS 20D
Canon EOS 300D
Canon EOS DIGITAL REBEL
Canon EOS Kiss Digital
Canon EOS D2000C
Canon EOS-1D
Canon EOS-1DS
Canon EOS-1D Mark II
Canon EOS-1Ds Mark II
Casio QV-2000UX
Casio QV-3000EX
Casio QV-3500EX
Casio QV-4000
Casio QV-5700
Casio Exlim Pro 600
Contax N DIGITAL
Creative PC-CAM 600
Creo Leaf Valeo 22
Fuji FinePix S2Pro
Fuji FinePix S5000
Fuji FinePix S7000
Fuji FinePix E550
Fuji FinePix F700
Fuji FinePix S20Pro
Imacon Ixpress
Kodak DC20 (see Oliver Hartman's page)
Kodak DC25 (see Jun-ichiro Itoh's page)
Kodak DC40 (aka "Logitech Fotoman Pixtura")
Kodak DC50
Kodak DC120 (also try kdc2tiff)
Kodak DCS315C
Kodak DCS330C
Kodak DCS420
Kodak DCS460
Kodak DCS460A
Kodak DCS520C
Kodak DCS560C
Kodak DCS620C
Kodak DCS620X
Kodak DCS660C
Kodak DCS660M
Kodak DCS720X
Kodak DCS760C
Kodak DCS760M
Kodak EOSDCS1
Kodak EOSDCS3B
Kodak NC2000F
Kodak ProBack
Kodak PB645C
Kodak PB645H
Kodak PB645M
Kodak DCS Pro 14n
Kodak DCS Pro 14nx
Kodak DCS Pro SLR/c
Kodak DCS Pro SLR/n
Konica KD-400Z
Konica KD-510Z
Leica Digilux 2
Minolta DiMAGE 5
Minolta DiMAGE 7
Minolta DiMAGE 7i
Minolta DiMAGE 7Hi
Minolta DiMAGE A1
Minolta DiMAGE A2
Minolta DiMAGE G400
Minolta DiMAGE G500
Minolta DiMAGE G600
Minolta DiMAGE Z2
Nikon D1
Nikon D1H
Nikon D1X
Nikon D100
Nikon D2H
Nikon D70
Nikon E950 ("DIAG RAW" hack)
Nikon E990 ("DIAG RAW" hack)
Nikon E995 ("DIAG RAW" hack)
Nikon E2100 ("DIAG RAW" hack)
Nikon E2500 ("DIAG RAW" hack)
Nikon E4300 ("DIAG RAW" hack)
Nikon E4500 ("DIAG RAW" hack)
Nikon E5000
Nikon E5400
Nikon E5700
Nikon E8700
Nikon E8800
Olympus C5050Z
Olympus C5060WZ
Olympus C8080WZ
Olympus E-1
Olympus E-10
Olympus E-20
Panasonic DMC-LC1
Pentax *ist D
Pentax Optio S
Pentax Optio S4
Phase One LightPhase
Phase One H10
Phase One H20
Phase One H25
Rollei d530flex
Sigma SD9
Sigma SD10
Sinar 12582980-byte
Sony DSC-F828

!!}

unit ieraw;


{$I ie.inc}


interface


{$ifdef IEINCLUDERAWFORMATS}

uses SysUtils, Windows, Classes, hyiedefs, hyieutils, imageenproc, imageenio, ieview;


procedure IEReadCameraRAWStream(InputStream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
function IERAWTryStream(Stream: TStream): boolean;

function IECRWGetCIFFAsExif(Stream: TStream; var IOParams: TIOParamsVals): boolean;
function IECRWGetJpeg(Bitmap: TIEBitmap; Stream: TStream): boolean;


type
  EIERAWException = class(Exception);

{$endif}



implementation

uses Math, ievision, iesettings;



{$ifdef IEINCLUDERAWFORMATS}

{$WARNINGS OFF}


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// embedded dcraw porting

{$ifndef IEUSEDLLRAWLIB}

const LONG_BIT = (8 * sizeof (integer));

type

  pdecode=^decode;
  decode=record
    branch: array [0..1] of pdecode;
    leaf: integer;
  end;

  tword4array=array [0..maxint div 16] of array [0..3] of word;
  pword4array=^tword4array;

  tint64array=array [0..maxint div 16] of int64;
  pint64array=^tint64array;

  PRec=^TRec;

  TIELoadRaw=procedure(rec: PRec);

  TRec=record
    image: pword4array; // 'ushort (*image)[4]'
    ifp: TStream;
    order: word;
    make, model, model2: array [0..63] of AnsiChar;
    timestamp: ttimestamp;
    data_offset, curve_offset, curve_length: integer;
    tiff_data_compression, kodak_data_compression: integer;
    raw_height, raw_width, top_margin, left_margin: integer;
    height, width, colors, black, rgb_max: integer;
    iheight, iwidth, shrink: integer;
    is_canon, is_cmy, is_foveon, use_coeff, trim, flip, xmag, ymag: integer;
    zero_after_ff: integer;
    filters: dword;
    white: array [0..7] of array [0..7] of word;
    load_raw: TIELoadRaw;
    gamma_val, bright, red_scale, blue_scale: single;
    four_color_rgb, document_mode, quick_interpolate: integer;
    use_auto_wb, use_camera_wb, use_secondary: integer;
    camera_red, camera_blue: single;
    pre_mul: array [0..3] of single;
    coeff: array [0..2] of array [0..3] of single;
    histogram: array [0..$2000-1] of integer;
    jpeg_buffer: array [0..4096-1] of AnsiChar;
    pad: array [0..128-1] of dword;
    decrypt_p: dword;
    getbits_bitbuf: dword;
    getbits_vbits: integer;
    make_decoder_leaf: integer;
    radc_token_s: pintegerarray;
    radc_token_dstart: array [0..18-1] of pdecode;
    radc_token_dindex: pdecode;
    vng_interpolate_cp: pinteger;
    first_decode: array [0..2047] of decode;
    second_decode: pdecode;
    free_decode: pdecode;
    needrot45: boolean;
    xprogress: TProgressRec;
  end;



procedure InitRec(var rec: TRec);
begin
  with rec do
  begin
    getbits_bitbuf := 0;
    getbits_vbits := 0;
    make_decoder_leaf := 0;
    radc_token_s := nil;
    fillchar(radc_token_dstart, sizeof(radc_token_dstart), 0);
    radc_token_dindex := nil;
    vng_interpolate_cp := nil;
  end;
end;

function strcasecmp(Str1, Str2: PAnsiChar): integer;
begin
  result := stricomp(Str1, Str2);
end;

function strcmp(Str1, Str2: PAnsiChar): integer;
begin
  result := strcomp(Str1, Str2);
end;

function strncmp(Str1, Str2: PAnsiChar; n: integer): integer;
begin
  result := strlcomp(Str1, Str2, n);
end;

function ftell(s: TStream): integer;
begin
  result := s.Position;
end;

function fgetc(s: TStream): byte;
begin
  s.Read(result, 1);
end;

(*
   In order to inline this calculation, I make the risky
   assumption that all filter patterns can be described
   by a repeating pattern of eight rows and two columns

   Return values are either 0/1/2/3 = G/M/C/Y or 0/1/2/3 = R/G1/B/G2
 *)
function FC(var rec: TRec; row, col: dword): integer;
begin
  with rec do
    result := (filters shr (((row shl 1 and 14) + (col and 1)) shl 1) and 3);
end;

function BAYER(var rec: TRec; row, col: dword): pword;
begin
  with rec do
    //result := @(image[(row shr shrink)*iwidth + (col shr shrink)][FC(row, col)]);
    result := @(image[(row shr shrink)*iwidth + (col shr shrink)][ (filters shr (((row shl 1 and 14) + (col and 1)) shl 1) and 3) ]);
end;

(*
   PowerShot 600 uses 0xe1e4e1e4:

	  0 1 2 3 4 5
	0 G M G M G M
	1 C Y C Y C Y
	2 M G M G M G
	3 C Y C Y C Y

   PowerShot A5 uses 0x1e4e1e4e:

	  0 1 2 3 4 5
	0 C Y C Y C Y
	1 G M G M G M
	2 C Y C Y C Y
	3 M G M G M G

   PowerShot A50 uses 0x1b4e4b1e:

	  0 1 2 3 4 5
	0 C Y C Y C Y
	1 M G M G M G
	2 Y C Y C Y C
	3 G M G M G M
	4 C Y C Y C Y
	5 G M G M G M
	6 Y C Y C Y C
	7 M G M G M G

   PowerShot Pro70 uses 0x1e4b4e1b:

	  0 1 2 3 4 5
	0 Y C Y C Y C
	1 M G M G M G
	2 C Y C Y C Y
	3 G M G M G M
	4 Y C Y C Y C
	5 G M G M G M
	6 C Y C Y C Y
	7 M G M G M G

   PowerShots Pro90 and G1 use 0xb4b4b4b4:

	  0 1 2 3 4 5
	0 G M G M G M
	1 Y C Y C Y C

   All RGB cameras use one of these Bayer grids:

	0x16161616:	0x61616161:	0x49494949:	0x94949494:

	  0 1 2 3 4 5	  0 1 2 3 4 5	  0 1 2 3 4 5	  0 1 2 3 4 5
	0 B G B G B G	0 G R G R G R	0 G B G B G B	0 R G R G R G
	1 G R G R G R	1 B G B G B G	1 R G R G R G	1 G B G B G B
	2 B G B G B G	2 G R G R G R	2 G B G B G B	2 R G R G R G
	3 G R G R G R	3 B G B G B G	3 R G R G R G	3 G B G B G B

 *)

function memcmp(buf1, buf2: pbyte; count: integer): integer;
begin
  if count = 0 then
    result := 0
  else
  begin
    dec(count);
    while (count > 0) and (buf1^ = buf2^) do
    begin
      inc(buf1);
      inc(buf2);
      dec(count);
    end;
    result := buf1^ - buf2^;
  end;
end;

function memmem (haystack: PAnsiChar; haystacklen: integer; needle: PAnsiChar; needlelen: integer): PAnsiChar;
var
  c: DWORD;
begin
  for c := DWORD(haystack) to DWORD(haystack) + DWORD(haystacklen) - DWORD(needlelen) do
    if (memcmp (pbyte(c), pbyte(needle), needlelen)=0) then
    begin
      result := PAnsiChar(c);
      exit;
    end;
  result := nil;
end;

procedure merror (ptr: pointer; where: PAnsiChar);
begin
  if (ptr<>nil) then
    exit;
  raise Exception.Create(': Out of memory in '+where);
end;

(*
   Get a 2-byte integer, making no assumptions about CPU byte order.
   Nor should we assume that the compiler evaluates left-to-right.
 *)
function fget2 (var rec: TRec; f: TStream): word;
var
  a, b: byte;
begin
  with rec do
  begin
    f.Read(a, 1);
    f.Read(b, 1);
    if (order = $4949) then    (* "II" means little-endian *)
      result := a + (b shl 8)
    else        (* "MM" means big-endian *)
      result := (a shl 8) + b;
  end;
end;

(*
   Same for a 4-byte integer.
 *)
function fget4 (var rec: TRec; f: TStream): integer;
var
  a, b, c, d: byte;
begin
  with rec do
  begin
    f.Read(a, 1);
    f.Read(b, 1);
    f.Read(c, 1);
    f.Read(d, 1);
    if (order = $4949) then
      result := a + (b shl 8) + (c shl 16) + (d shl 24)
    else
      result := (a shl 24) + (b shl 16) + (c shl 8) + d;
  end;
end;

procedure canon_600_load_raw(rec: PRec);
var
  data: array [0..1119] of byte;
  dp: pbytearray;
  pixel: array [0..895] of word;
  pix: pwordarray;
  irow, orow, col: integer;
begin
  with rec^ do
  begin
    orow := 0;
    xprogress.per1 := 100 / height / 2;
    for irow := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * irow));
          
      ifp.Read(data[0], 1120); //fread (data, 1120, 1, ifp);
      dp := @data;
      pix := @pixel;
      while int64(DWORD(dp)) < int64(DWORD(@data))+1120 do
      begin
        pix[0] := (dp[0] shl 2) + (dp[1] shr 6    );
        pix[1] := (dp[2] shl 2) + (dp[1] shr 4 and 3);
        pix[2] := (dp[3] shl 2) + (dp[1] shr 2 and 3);
        pix[3] := (dp[4] shl 2) + (dp[1]      and 3);
        pix[4] := (dp[5] shl 2) + (dp[9]      and 3);
        pix[5] := (dp[6] shl 2) + (dp[9] shr 2 and 3);
        pix[6] := (dp[7] shl 2) + (dp[9] shr 4 and 3);
        pix[7] := (dp[8] shl 2) + (dp[9] shr 6    );
        inc(pbyte(dp), 10);
        inc(pword(pix), 8);
      end;
      for col := 0 to width-1 do
        BAYER(rec^, orow, col)^ := pixel[col] shl 4;
      for col := width to 896-1 do
        inc(black, pixel[col]);
      inc(orow, 2);
      if (orow > height) then
        orow := 1;
    end;
    black := ( black shl 4) div ((896 - width) * height);
  end;
end;

procedure canon_a5_load_raw(rec: PRec);
var
  data: array [0..1940-1] of byte;
  dp: pbytearray;
  pixel: array [0..1552-1] of word;
  pix: pwordarray;
  row, col: integer;
begin
  with rec^ do
  begin
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(data[0], raw_width * 10 div 8); //  fread (data, raw_width * 10 / 8, 1, ifp);
      dp := @data;
      pix := @pixel;
      while (int64(DWORD(pix)) < int64(DWORD(@pixel))+raw_width*sizeof(word)) do
      begin
        pix[0] := (dp[1] shl 2) + (dp[0] shr 6);
        pix[1] := (dp[0] shl 4) + (dp[3] shr 4);
        pix[2] := (dp[3] shl 6) + (dp[2] shr 2);
        pix[3] := (dp[2] shl 8) + (dp[5]     );
        pix[4] := (dp[4] shl 2) + (dp[7] shr 6);
        pix[5] := (dp[7] shl 4) + (dp[6] shr 4);
        pix[6] := (dp[6] shl 6) + (dp[9] shr 2);
        pix[7] := (dp[9] shl 8) + (dp[8]     );
        inc(pbyte(dp), 10);
        inc(pword(pix), 8);
      end;
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := (pixel[col] and $3ff) shl 4;
      for col := width to raw_width-1 do
        inc(black , pixel[col] and $3ff);
    end;
    if (raw_width > width) then
      black := ( black shl 4) div ((raw_width - width) * height);
  end;
end;

(*
   getbits(-1) initializes the buffer
   getbits(n) where 0 <= n <= 25 returns an n-bit integer
 *)
function getbits (var rec: TRec; nbits: integer): dword;
var
  c, ret: dword;
begin
  with rec do
  begin
    if (nbits = 0) then
    begin
      result := 0;
      exit;
    end;
    if (nbits = -1) then
    begin
      getbits_vbits := 0;
      getbits_bitbuf := 0;
      ret := 0;
    end
    else
    begin
      ret := getbits_bitbuf shl (LONG_BIT - getbits_vbits) shr (LONG_BIT - nbits);
      dec(getbits_vbits , nbits);
    end;
    while (getbits_vbits < LONG_BIT - 7) do
    begin
      c := fgetc(ifp);
      getbits_bitbuf := (getbits_bitbuf shl 8) + c;
      if (c = $ff) and (zero_after_ff<>0) then
        fgetc(ifp);
      inc(getbits_vbits , 8);
    end;
    result := ret;
  end;
end;

procedure memset(P: Pointer; B: Byte; count: Integer);
begin
  FillChar(P^, count, B);
end;

procedure init_decoder(var rec: TRec);
begin
  with rec do
  begin
    memset (@first_decode, 0, sizeof(first_decode));
    free_decode := @first_decode;
  end;
end;

(*
   Construct a decode tree according the specification in *source.
   The first 16 bytes specify how many codes should be 1-bit, 2-bit
   3-bit, etc.  Bytes after that are the leaf values.

   For example, if the source is

    { 0,1,4,2,3,1,2,0,0,0,0,0,0,0,0,0,
      0x04,0x03,0x05,0x06,0x02,0x07,0x01,0x08,0x09,0x00,0x0a,0x0b,0xff  },

   then the code is

	00		0x04
	010		0x03
	011		0x05
	100		0x06
	101		0x02
	1100		0x07
	1101		0x01
	11100		0x08
	11101		0x09
	11110		0x00
	111110		0x0a
	1111110		0x0b
	1111111		0xff
 *)
function make_decoder (var rec: TRec; source: pbytearray; level: integer): pbyte;
var
  cur: pdecode;
  i, next: integer;
begin
  with rec do
  begin
    if (level=0) then
      make_decoder_leaf := 0;
    cur := free_decode;
    inc(free_decode);
    if (int64(DWORD(free_decode)) > int64(DWORD(@first_decode))+2048*sizeof(decode)) then
      raise Exception.Create(': decoder table overflow');

    i := 0;
    next := 0;
    while (i <= make_decoder_leaf) and (next < 16) do
    begin
      inc(i , source[next]);
      inc(next);
    end;
    if (i > make_decoder_leaf) then
    begin
      if (level < next) then
      begin
        cur^.branch[0] := free_decode;
        make_decoder (rec, source, level+1);
        cur^.branch[1] := free_decode;
        make_decoder (rec, source, level+1);
      end
      else
      begin
        cur^.leaf := source[16 + make_decoder_leaf];
        inc(make_decoder_leaf);
      end;
    end;
    result := pbyte( int64(DWORD(source)) + 16 + make_decoder_leaf );
  end;
end;

procedure crw_init_tables (var rec: TRec; table: dword);
const
  first_tree:array [0..3-1] of array [0..29-1] of byte = (
    ( 0,1,4,2,3,1,2,0,0,0,0,0,0,0,0,0,
      $04,$03,$05,$06,$02,$07,$01,$08,$09,$00,$0a,$0b,$ff  ),

    ( 0,2,2,3,1,1,1,1,2,0,0,0,0,0,0,0,
      $03,$02,$04,$01,$05,$00,$06,$07,$09,$08,$0a,$0b,$ff  ),

    ( 0,0,6,3,1,1,2,0,0,0,0,0,0,0,0,0,
      $06,$05,$07,$04,$08,$03,$09,$02,$00,$0a,$01,$0b,$ff  )
  );

  second_tree:array [0..3-1] of array [0..180-1] of byte = (
    ( 0,2,2,2,1,4,2,1,2,5,1,1,0,0,0,139,
      $03,$04,$02,$05,$01,$06,$07,$08,
      $12,$13,$11,$14,$09,$15,$22,$00,$21,$16,$0a,$f0,
      $23,$17,$24,$31,$32,$18,$19,$33,$25,$41,$34,$42,
      $35,$51,$36,$37,$38,$29,$79,$26,$1a,$39,$56,$57,
      $28,$27,$52,$55,$58,$43,$76,$59,$77,$54,$61,$f9,
      $71,$78,$75,$96,$97,$49,$b7,$53,$d7,$74,$b6,$98,
      $47,$48,$95,$69,$99,$91,$fa,$b8,$68,$b5,$b9,$d6,
      $f7,$d8,$67,$46,$45,$94,$89,$f8,$81,$d5,$f6,$b4,
      $88,$b1,$2a,$44,$72,$d9,$87,$66,$d4,$f5,$3a,$a7,
      $73,$a9,$a8,$86,$62,$c7,$65,$c8,$c9,$a1,$f4,$d1,
      $e9,$5a,$92,$85,$a6,$e7,$93,$e8,$c1,$c6,$7a,$64,
      $e1,$4a,$6a,$e6,$b3,$f1,$d3,$a5,$8a,$b2,$9a,$ba,
      $84,$a4,$63,$e5,$c5,$f3,$d2,$c4,$82,$aa,$da,$e4,
      $f2,$ca,$83,$a3,$a2,$c3,$ea,$c2,$e2,$e3,$ff,$ff  ),

    ( 0,2,2,1,4,1,4,1,3,3,1,0,0,0,0,140,
      $02,$03,$01,$04,$05,$12,$11,$06,
      $13,$07,$08,$14,$22,$09,$21,$00,$23,$15,$31,$32,
      $0a,$16,$f0,$24,$33,$41,$42,$19,$17,$25,$18,$51,
      $34,$43,$52,$29,$35,$61,$39,$71,$62,$36,$53,$26,
      $38,$1a,$37,$81,$27,$91,$79,$55,$45,$28,$72,$59,
      $a1,$b1,$44,$69,$54,$58,$d1,$fa,$57,$e1,$f1,$b9,
      $49,$47,$63,$6a,$f9,$56,$46,$a8,$2a,$4a,$78,$99,
      $3a,$75,$74,$86,$65,$c1,$76,$b6,$96,$d6,$89,$85,
      $c9,$f5,$95,$b4,$c7,$f7,$8a,$97,$b8,$73,$b7,$d8,
      $d9,$87,$a7,$7a,$48,$82,$84,$ea,$f4,$a6,$c5,$5a,
      $94,$a4,$c6,$92,$c3,$68,$b5,$c8,$e4,$e5,$e6,$e9,
      $a2,$a3,$e3,$c2,$66,$67,$93,$aa,$d4,$d5,$e7,$f8,
      $88,$9a,$d7,$77,$c4,$64,$e2,$98,$a5,$ca,$da,$e8,
      $f3,$f6,$a9,$b2,$b3,$f2,$d2,$83,$ba,$d3,$ff,$ff  ),

    ( 0,0,6,2,1,3,3,2,5,1,2,2,8,10,0,117,
      $04,$05,$03,$06,$02,$07,$01,$08,
      $09,$12,$13,$14,$11,$15,$0a,$16,$17,$f0,$00,$22,
      $21,$18,$23,$19,$24,$32,$31,$25,$33,$38,$37,$34,
      $35,$36,$39,$79,$57,$58,$59,$28,$56,$78,$27,$41,
      $29,$77,$26,$42,$76,$99,$1a,$55,$98,$97,$f9,$48,
      $54,$96,$89,$47,$b7,$49,$fa,$75,$68,$b6,$67,$69,
      $b9,$b8,$d8,$52,$d7,$88,$b5,$74,$51,$46,$d9,$f8,
      $3a,$d6,$87,$45,$7a,$95,$d5,$f6,$86,$b4,$a9,$94,
      $53,$2a,$a8,$43,$f5,$f7,$d4,$66,$a7,$5a,$44,$8a,
      $c9,$e8,$c8,$e7,$9a,$6a,$73,$4a,$61,$c7,$f4,$c6,
      $65,$e9,$72,$e6,$71,$91,$93,$a6,$da,$92,$85,$62,
      $f3,$c5,$b2,$a4,$84,$ba,$64,$a5,$b3,$d2,$81,$e5,
      $d3,$aa,$c4,$ca,$f2,$b1,$e4,$d1,$83,$63,$ea,$c3,
      $e2,$82,$f1,$a3,$c2,$a1,$c1,$e3,$a2,$e1,$ff,$ff  )
  );
begin
  with rec do
  begin
    if (table > 2) then
      table := 2;
    init_decoder(rec);
    make_decoder ( rec, @first_tree[table], 0);
    second_decode := free_decode;
    make_decoder ( rec, @second_tree[table], 0);
  end;
end;

(*
   Return 0 if the image starts with compressed data,
   1 if it starts with uncompressed low-order bits.

   In Canon compressed data, 0xff is always followed by 0x00.
 *)
function canon_has_lowbits(var rec: TRec): integer;
var
  test: array [0..$4000-1] of byte;
  ret, i: integer;
begin
  with rec do
  begin
    ret := 1;
    ifp.Position := 0;//fseek (ifp, 0, SEEK_SET);
    ifp.Read(test[0], sizeof(test));//fread (test, 1, sizeof test, ifp);
    for i := 540 to sizeof(test) - 2 do
      if (test[i] = $ff) then
      begin
        if (test[i+1]<>0) then
        begin
          result := 1;
          exit;
        end;
        ret := 0;
      end;
    result := ret;
  end;
end;

procedure canon_compressed_load_raw(rec: PRec);
var
  pixel: pwordarray;
  prow: pword;
  lowbits, shift, i, row, r, col, save, val: integer;
  irow, icol: dword;
  decode, dindex: pdecode;
  block, leaf, len, diff, carry, pnum : integer ;
  diffbuf: array [0..64-1] of integer;
  base: array [0..2-1] of integer;
  c: byte;
  bblack: int64;

begin
  with rec^ do
  begin
    carry := 0;
    pnum := 0;
    bblack := 0;

    pixel := allocmem (raw_width*8 * sizeof(word));
    merror (pixel, 'canon_compressed_load_raw()');
    lowbits := canon_has_lowbits(rec^);
    shift := 4 - lowbits*2;
    ifp.Position := 540 + lowbits*raw_height*raw_width div 4; //fseek (ifp, 540 + lowbits*raw_height*raw_width/4, SEEK_SET);
    zero_after_ff := 1;
    getbits(rec^, -1);
    row := 0;

    xprogress.per1 := 100 / raw_height / 2;

    while (row < raw_height ) do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      for block := 0 to (raw_width shr 3)-1 do
      begin
        memset (@diffbuf, 0, sizeof(diffbuf));
        decode := @first_decode;
        i := 0;
        while i<64 do
        begin
          dindex := decode;
          while ( dindex^.branch[0]<>nil ) do
            dindex := dindex^.branch[getbits(rec^, 1)];
          leaf := dindex^.leaf;
          decode := second_decode;
          if (leaf = 0) and (i<>0) then
            break;
          if (leaf = $ff) then
          begin
            inc(i);
            continue;
          end;
          inc(i , leaf shr 4);
          len := leaf and 15;
          if (len = 0) then
          begin
            inc(i);
            continue;
          end;
          diff := getbits(rec^, len);
          if ((diff and (1 shl (len-1))) = 0) then
            dec(diff , (1 shl len) - 1);
          if (i < 64) then
            diffbuf[i] := diff;
          inc(i);
        end;
        inc(diffbuf[0] , carry);
        carry := diffbuf[0];
        for i := 0 to 64-1 do
        begin
          if (pnum mod raw_width = 0) then
          begin
            base[1] := 512;
            base[0] := base[1];
          end;
          inc(pnum);
          inc(base[i and 1] , diffbuf[i]);
          pixel[(block shl 6) + i] := ( base[i and 1] );
        end;
      end;
      if (lowbits<>0) then
      begin
        save := ftell(ifp);			(* Don't lose our place *)
        ifp.Position := 26 + row*raw_width div 4; //fseek (ifp, 26 + row*raw_width/4, SEEK_SET);
        prow := pword(pixel);
        for i := 0 to raw_width*2-1 do
        begin
          c := fgetc(ifp);
          r := 0;
          while ( r < 8 ) do
          begin
            val := (prow^ shl 2) + ((c shr r) and 3);
            if (raw_width = 2672) and (val < 512) then
              inc(val , 2);
            prow^ := val;
            inc(r, 2);
            inc(prow);
          end;
        end;
        ifp.Position := save; //fseek (ifp, save, SEEK_SET);
      end;
      for r := 0 to 8-1 do
      begin

        irow := row - top_margin + r;

        if (dword(irow) >= dword(height)) or (irow<0) then
          continue;

        for col := 0 to raw_width-1 do
        begin

          icol := col - left_margin;

          if (dword(icol) < dword(width)) and (icol>=0) then
            //BAYER(irow, icol)^ := pixel[r*raw_width+col] shl shift
            image[(irow shr shrink)*iwidth + (icol shr shrink)][ (filters shr (((irow shl 1 and 14) + (icol and 1)) shl 1) and 3) ]
              :=
            pixel[r*raw_width+col] shl shift
          else
            inc(bblack , pixel[r*raw_width+col] );
        end;
      end;
      inc(row , 8);
    end;
    freemem(pixel);
    if (raw_width > width) then
      black := (bblack shl shift) div ((raw_width - width) * height);
  end;
end;

procedure kodak_curve (var rec: TRec; curve: pwordarray);
var
  i, entries, tag, len, val: integer;
begin
  with rec do
  begin
    for i := 0 to $1000-1 do
      curve[i] := i;
    if strcasecmp(make, 'KODAK')<>0 then
      exit;
    if (curve_offset<>0) then
    begin
      ifp.Position := 12; //fseek (ifp, 12, SEEK_SET);
      entries := fget2(rec, ifp);
      while entries<>0 do
      begin
        dec(entries);
        tag  := fget2(rec, ifp);
        fget2(rec, ifp);
        len  := fget4(rec, ifp);
        val  := fget4(rec, ifp);
        if (tag = $90d) then
        begin
          curve_offset := val;
          curve_length := len;
        end;
      end;
    end;
    if (curve_offset<>0) then
    begin
      ifp.Position := curve_offset; //fseek (ifp, curve_offset, SEEK_SET);
      i := 0;
      while i<curve_length do
      begin
        curve[i] := fget2(rec, ifp);
        inc(i);
      end;
      while i < $1000 do
      begin
        curve[i] := curve[i-1];
        inc(i);
      end;
      rgb_max := curve[i-1] shl 2;
    end;
    ifp.Position := data_offset; // fseek (ifp, data_offset, SEEK_SET);
  end;
end;



(*
   Not a full implementation of Lossless JPEG, 
   just enough to decode Canon and Kodak images.
 *)
procedure lossless_jpeg_load_raw(rec: PRec);
var
  tag, len, jhigh, jwide, jrow, jcol, jidx, diff, i, row, col: integer;
  data: array [0..256-1] of byte;
  dp: pbyte;
  vpred, hpred: array [0..1] of integer;
  dstart: array [0..2-1] of pdecode;
  dindex: pdecode;
  curve: array [0..$1000-1] of word;
  bblack: int64;
  min: integer;
begin
  with rec^ do
  begin
    jhigh := 0;
    jwide := 0;
    vpred[0] := $800;
    vpred[1] := $800;
    bblack := 0;
    min := maxint;

    kodak_curve(rec^, @curve);
    order := $4d4d;
    if (fget2(rec^, ifp) <> $ffd8) then
      exit;
    repeat
      tag := fget2(rec^, ifp);
      len := fget2(rec^, ifp) - 2;
      if (tag <= $ff00) or (len > 255) then
        exit;
      ifp.Read(data[0], len); //fread (data, 1, len, ifp);
      case (tag) of
        $ffc3:
          begin
            jhigh := (data[1] shl 8) + data[2];
            jwide := ((data[3] shl 8) + data[4])*2;
          end;
        $ffc4:
          begin
            init_decoder(rec^);
            dstart[1] := free_decode;
            dstart[0] := dstart[1];
            dp := @data;
            while ( int64(DWORD(dp)) < int64(DWORD(@data))+len) and (dp^ < 2 ) do
            begin
              dstart[dp^] := free_decode;
              inc(dp);
              dp := make_decoder (rec^, pbytearray(dp), 0);
            end;
          end;
       end;
    until (tag = $ffda);

    xprogress.per1 := 100 / jhigh / 2;

    zero_after_ff := 1;
    getbits(rec^, -1);
    for jrow := 0 to jhigh-1 do
    begin
      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * jrow));
      for jcol := 0 to jwide-1 do
      begin
        dindex := dstart[jcol and 1];
        while ( dindex^.branch[0]<>nil ) do
          dindex := dindex^.branch[getbits(rec^, 1)];
        len := dindex^.leaf;
        diff := getbits(rec^, len);
        if ((diff and (1 shl (len-1))) = 0) then
          dec(diff , (1 shl len) - 1 );
        if (jcol < 2) then
        begin
          inc(vpred[jcol] , diff);
          hpred[jcol] := vpred[jcol];
        end
        else
          inc( hpred[jcol and 1] , diff );
        diff := hpred[jcol and 1];
        if (diff < 0) then
          diff := 0;
        if (diff > $fff) then
          diff := $fff;
        jidx := jrow*jwide + jcol;
        if (raw_width = 5108) then
        begin
          i := jidx div (1680*jhigh);
          if (i < 2) then
          begin
            row := jidx div 1680 mod jhigh;
            col := jidx mod 1680 + i*1680;
          end
          else
          begin
            dec( jidx , 2*1680*jhigh );
            row := jidx div 1748;
            col := jidx mod 1748 + 2*1680;
          end;
        end
        else
        begin
          row := jidx div raw_width;
          col := jidx mod raw_width;
        end;
        if ( (row-top_margin) >= height) then
          continue;
        if ( (col-left_margin) < width) then
        begin

          if (row-top_margin>=0) and (col-left_margin>=0) then
            BAYER(rec^, row-top_margin, col-left_margin)^ := curve[diff] shl 2;

          if (min > curve[diff]) then
            min := curve[diff];
        end
        else
          inc(bblack , curve[diff] );
      end;
    end;
    if (raw_width > width) then
      black := (bblack shl 2) div ((raw_width - width) * height);
    if strcasecmp(make, 'KODAK')=0 then
      black := min shl 2;
  end;
end;

procedure nikon_compressed_load_raw(rec: PRec);
const
  nikon_tree:array [0..28] of byte = (
    0,1,5,1,1,1,1,1,1,2,0,0,0,0,0,0,
    5,4,3,6,2,7,1,0,8,9,11,10,12
  );
var
  vpred: array [0..4-1] of integer;
  hpred: array [0..2-1] of integer;
  csize, row, col, i, len, diff: integer;
  curve: pwordarray;
  dindex: pdecode;
begin
  with rec^ do
  begin
    init_decoder(rec^);
    make_decoder (rec^, @nikon_tree, 0);

    ifp.Position := curve_offset; //fseek (ifp, curve_offset, SEEK_SET);
    for i := 0 to 4-1 do
      vpred[i] := fget2(rec^, ifp);
    csize := fget2(rec^, ifp);
    curve := allocmem(csize * sizeof(word));
    merror (curve, 'nikon_compressed_load_raw()');
    for i := 0 to csize-1 do
      curve[i] := fget2(rec^, ifp);

    ifp.Position := data_offset; //fseek (ifp, data_offset, SEEK_SET);
    getbits(rec^, -1);

    xprogress.per1 := 100 / height / 2;

    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      for col := 0 to raw_width-1 do
      begin
        dindex := @first_decode;
        while (dindex^.branch[0]<>nil) do
          dindex := dindex^.branch[getbits(rec^, 1)];
        len := dindex^.leaf;
        diff := getbits(rec^, len);
        if ((diff and (1 shl (len-1))) = 0) then
          dec( diff , (1 shl len) - 1 );
        if (col < 2) then
        begin
          i := 2*(row and 1) + (col and 1);
          inc(vpred[i] , diff);
          hpred[col] := vpred[i];
        end
        else
          inc( hpred[col and 1] , diff );
        if ( (col-left_margin) >= width) then
          continue;
        diff := hpred[col and 1];
        if (diff < 0) then
          diff := 0;
        if (diff >= csize) then
          diff := csize-1;
        BAYER(rec^, row, col-left_margin)^ := curve[diff] shl 2;
      end;
    end;
    freemem(curve);
  end;
end;

procedure nikon_load_raw(rec: PRec);
var
  irow, row, col, i: integer;
begin
  with rec^ do
  begin
    getbits(rec^, -1);
    xprogress.per1 := 100 / height / 2;
    for irow := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      row := irow;
      if (model[0] = 'E') then
      begin
        row := irow * 2 mod height + irow div (height div 2);
        if (row = 1) and (IEStrToIntDef(AnsiString(PAnsiChar(@model[1])), 0) < 5000) then
        begin
          ifp.Position := ifp.Size; //fseek (ifp, 0, SEEK_END);
          ifp.Position := ifp.Position div 2; //fseek (ifp, ftell(ifp)/2, SEEK_SET);
          getbits(rec^, -1);
        end;
      end;
      for col := 0 to raw_width-1 do
      begin
        i := getbits(rec^, 12);
        if ((col-left_margin) < width) and ((col-left_margin)>=0) then
          BAYER(rec^, row, col-left_margin)^ := i shl 2;
        if (tiff_data_compression = 34713) and ((col mod 10) = 9) then
          getbits(rec^, 8);
      end;
    end;
  end;
end;

(*
   Figure out if a NEF file is compressed.  These fancy heuristics
   are only needed for the D100, thanks to a bug in some cameras
   that tags all images as "compressed".
 *)
function nikon_is_compressed(var rec: TRec): integer;
var
  test: array [0..256-1] of byte;
  i: integer;
begin
  with rec do
  begin
    if (tiff_data_compression <> 34713) then
    begin
      result := 0;
      exit;
    end;
    if strcmp(model, 'D100')<>0 then
    begin
      result := 1;
      exit;
    end;
    ifp.Position := data_offset; //fseek (ifp, data_offset, SEEK_SET);
    ifp.Read(test[0], 256); //fread (test, 1, 256, ifp);
    i := 15;
    while (i < 256) do
    begin
      if (test[i]<>0) then
      begin
        result := 1;
        exit;
      end;
      inc(i, 16);
    end;
    result := 0;
  end;
end;

(*
   Returns 1 for a Coolpix 990, 0 for a Coolpix 995.
 *)
function nikon_e990(var rec: TRec): integer;
const
  often: array [0..3] of byte = ( $00, $55, $aa, $ff );
var
  i: integer;
  histo: array [0..256-1] of integer;
  _c: byte;
begin
  with rec do
  begin
    memset (@histo, 0, sizeof(histo));
    ifp.Position := 2064*1540*3 div 4; //fseek (ifp, 2064*1540*3/4, SEEK_SET);
    for i := 0 to 2000-1 do
    begin
      ifp.Read(_c, 1); //fgetc(ifp)
      inc(histo[_c]);
    end;
    for i := 0 to 4-1 do
      if (histo[often[i]] > 400) then
      begin
        result := 1;
        exit;
      end;
    result := 0;
  end;
end;

(*
   Returns 1 for a Coolpix 2100, 0 for anything else.
 *)
function nikon_e2100(var rec: TRec): integer;
var
  t: array [0..12-1] of byte;
  i: integer;
begin
  with rec do
  begin
    ifp.Position := 0; //fseek (ifp, 0, SEEK_SET);
    for i := 0 to 1024-1 do
    begin
      ifp.Read(t[0], 12);  //fread (t, 1, 12, ifp);
      if (((t[2] and t[4] and t[7] and t[9]) shr 4 and t[1] and t[6] and t[8] and t[11] and 3) <> 3) then
      begin
        result := 0;
        exit;
      end;
    end;
    result := 1;
  end;
end;

(*
   Separates a Minolta DiMAGE Z2 from a Nikon E4300.
 *)
function minolta_z2(var rec: TRec): integer;
var
  i: integer;
  tail: array [0..424-1] of AnsiChar;
begin
  with rec do
  begin
    ifp.Position := ifp.Size-sizeof(tail); //fseek (ifp, -sizeof tail, SEEK_END);
    ifp.Read(tail[0], sizeof(tail)); //fread (tail, 1, sizeof tail, ifp);
    for i := 0 to sizeof(tail)-1 do
      if (tail[i]<>#0) then
      begin
        result := 1;
        exit;
      end;
    result := 0;
  end;
end;

procedure nikon_e2100_load_raw(rec: PRec);
var
  data: array [0..3432-1] of byte;
  dp: pbytearray;
  pixel: array [0..2288-1] of word;
  pix: pwordarray;
  row, col: integer;
begin
  with rec^ do
  begin
    xprogress.per1 := 100 / height / 2;
    row := 0;
    while (row <= height) do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      if (row = height) then
      begin
        ifp.Position := ifp.Position+IEIFI(width=1616 , 8792, 424); //fseek (ifp, width==1616 ? 8792: 424, SEEK_CUR);
        row := 1;
      end;
      ifp.Read(data[0], width*3 div 2); //fread (data, 1, width*3/2, ifp);
      dp := @data;
      pix := @pixel;
      while ( int64(DWORD(pix)) < int64(DWORD(@pixel))+width*sizeof(word) ) do
      begin
        pix[0] := (dp[2] shr 4) + (dp[ 3] shl 4);
        pix[1] := (dp[2] shl 8) +  dp[ 1];
        pix[2] := (dp[7] shr 4) + (dp[ 0] shl 4);
        pix[3] := (dp[7] shl 8) +  dp[ 6];
        pix[4] := (dp[4] shr 4) + (dp[ 5] shl 4);
        pix[5] := (dp[4] shl 8) +  dp[11];
        pix[6] := (dp[9] shr 4) + (dp[10] shl 4);
        pix[7] := (dp[9] shl 8) +  dp[ 8];
        inc(pbyte(dp), 12);
        inc(pword(pix), 8);
      end;
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := (pixel[col] and $fff) shl 2;
      inc(row, 2);
    end;
  end;
end;

procedure nikon_e950_load_raw(rec: PRec);
var
  irow, row, col: integer;
begin
  with rec^ do
  begin
    getbits(rec^, -1);
    xprogress.per1 := 100 / height / 2;
    for irow := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * irow));

      row := irow * 2 mod height;
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := getbits(rec^, 10) shl 4;
      col := 28;
      while ( col<>0 ) do
      begin
        getbits(rec^, 8);
        dec(col);
      end;
    end;
  end;
end;

function ntohs(netshort: word): word;
begin
  //result := swap(netshort);
  result := IESwapWord(netshort);
end;

function htons(netshort: word): word;
begin
  //result := swap(netshort);
  result := IESwapWord(netshort);
end;


function htonl(netlong: integer): integer;
begin
  result := IESwapDWord(netlong);
end;

(*
   The Fuji Super CCD is just a Bayer grid rotated 45 degrees.
 *)
procedure fuji_s2_load_raw(rec: PRec);
var
  pixel: array [0..2944-1] of word;
  row, col, r, c: integer;
begin
  with rec^ do
  begin
    ifp.position := ifp.position+(2944*24+32)*2; //fseek (ifp, (2944*24+32)*2, SEEK_CUR);
    xprogress.per1 := 100 / 2144 / 2;
    for row := 0 to 2144-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(pixel[0], 2*2944); //fread (pixel, 2, 2944, ifp);
      for col := 0 to 2880-1 do
      begin
        r := row + ((col+1) shr 1);
        c := 2143 - row + (col shr 1);

        //BAYER(rec^, r, c)^ := ntohs(pixel[col]) shl 2;
        image[(r shr shrink)*iwidth + (c shr shrink)][ (filters shr (((r shl 1 and 14) + (c and 1)) shl 1) and 3) ]
          := ntohs(pixel[col]) shl 2;
          
      end;
    end;
    needrot45 := true;
  end;
end;

procedure swab( src: PAnsiChar; dest: PAnsiChar; nbytes: integer );
var
  b1, b2: AnsiChar;
begin
  while (nbytes > 1) do
  begin
    b1 := src^; inc(src);
    b2 := src^; inc(src);
    dest^ := b2; inc(dest);
    dest^ := b1; inc(dest);
    dec(nbytes , 2);
  end;
end;


procedure fuji_common_load_raw (var rec: TRec; ncol: integer; icol: integer; nrow: integer);
var
  pixel: array [0..2048-1] of word;
  row, col, r, c: integer;
begin
  with rec do
  begin
    xprogress.per1 := 100 / nrow / 2;
    for row := 0 to nrow-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(pixel[0], 2*ncol); //fread (pixel, 2, ncol, ifp);
      if (ntohs($aa55) = $aa55)	then(* data is little-endian *)
        swab (PAnsiChar(@pixel), PAnsiChar(@pixel), ncol*2);
      for col := 0 to icol do
      begin
        r := icol - col + (row shr 1);
        c := col + ((row+1) shr 1);
        BAYER(rec, r, c)^ := pixel[col] shl 2;
      end;
    end;
    needrot45 := true;
  end;
end;

procedure fuji_s5000_load_raw(rec: PRec);
begin
  with rec^ do
  begin
    ifp.position := ifp.position+ (1472*4+24)*2; //fseek (ifp, (1472*4+24)*2, SEEK_CUR);
    fuji_common_load_raw (rec^, 1472, 1423, 2152);
  end;
end;

procedure fuji_s7000_load_raw(rec: PRec);
begin
  fuji_common_load_raw (rec^, 2048, 2047, 3080);
end;

(*
   The Fuji Super CCD SR has two photodiodes for each pixel.
   The secondary has about 1/16 the sensitivity of the primary, 
   but this ratio may vary.
 *)
procedure fuji_f700_load_raw(rec: PRec);
var
  pixel: array [0..2944-1] of word;
  row, col, r, c, val: integer;
begin
  with rec^ do
  begin
    xprogress.per1 := 100 / 2168 / 2;
    for row := 0 to 2168-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.read(pixel[0], 2*2944); //fread (pixel, 2, 2944, ifp);
      if (ntohs($aa55) = $aa55)	then(* data is little-endian *)
        swab (PAnsiChar(@pixel), PAnsiChar(@pixel), 2944*2);
      for col := 0 to 1440-1 do
      begin
        r := 1439 - col + (row shr 1);
        c := col + ((row+1) shr 1);
        val := pixel[col+16 + use_secondary*1472];
        BAYER(rec^, r, c)^ := val;
      end;
    end;
    needrot45 := true;
  end;
end;

procedure rollei_load_raw(rec: PRec);
var
  pixel: array [0..10-1] of byte;
  iten, isix, i, buffer, row, col: dword;
  todo : array [0..16-1] of dword;
  qq: integer;
begin
  with rec^ do
  begin
    iten := 0;
    buffer := 0;

    isix := raw_width * raw_height * 5 div 8;
    while true do
    begin
      qq := ifp.Read(pixel[0], 10); //fread (pixel, 1, 10, ifp)
      if qq<>10 then
        break;
      i := 0;
      while ( i < 10) do
      begin
        todo[i]   := iten;
        inc(iten);
        todo[i+1] := pixel[i] shl 8 or pixel[i+1];
        buffer    := pixel[i] shr 2 or buffer shl 6;
        inc(i, 2);
      end;
      while ( i < 16 ) do
      begin
        todo[i]   := isix;
        inc(isix);
        todo[i+1] := buffer shr (14-i)*5;
        inc(i, 2);
      end;
      i := 0;
      while ( i < 16 ) do
      begin
        row := todo[i] div raw_width - top_margin;
        col := todo[i] mod raw_width - left_margin;
        if (row < height) and (col < width) then
          BAYER(rec^, row, col)^ := (todo[i+1] and $3ff) shl 4;
        inc(i, 2);
      end;
    end;
  end;
end;

procedure phase_one_load_raw(rec: PRec);
var
  row, col, a, b: integer;
  pixel: array [0..4134-1] of word;
  akey, bkey: word;
begin
  with rec^ do
  begin
    ifp.Position := ifp.Position+ 8; //fseek (ifp, 8, SEEK_CUR);
    ifp.Position := ifp.Position+ fget4(rec^, ifp) + 296; //fseek (ifp, fget4(ifp) + 296, SEEK_CUR);
    akey := fget2(rec^, ifp);
    bkey := fget2(rec^, ifp);
    ifp.Position := data_offset + 12 + top_margin*raw_width*2; //fseek (ifp, data_offset + 12 + top_margin*raw_width*2, SEEK_SET);
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(pixel[0], 2*raw_width); //fread (pixel, 2, raw_width, ifp);
      col := 0;
      while ( col < raw_width ) do
      begin
        a := ntohs(pixel[col+0]) xor akey;
        b := ntohs(pixel[col+1]) xor bkey;
        pixel[col+0] := (b and $aaaa) or (a and $5555);
        pixel[col+1] := (a and $aaaa) or (b and $5555);
        inc(col, 2);
      end;
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := pixel[col+left_margin];
    end;
  end;
end;

procedure ixpress_load_raw(rec: PRec);
var
  pixel: array [0..4090-1] of word;
  row, col: integer;
begin
  with rec^ do
  begin
    ifp.Position := 304 + 6*2*4090; //fseek (ifp, 304 + 6*2*4090, SEEK_SET);
    xprogress.per1 := 100 / height / 2;
    row := height;
    dec(row);
    while ( row >= 0 ) do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * (height-row)));

      ifp.Read(pixel[0], 2*4090); //fread (pixel, 2, 4090, ifp);
      if (ntohs($aa55) = $aa55)	then (* data is little-endian *)
        swab (PAnsiChar(@pixel), PAnsiChar(@pixel), 4090*2);
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := pixel[width-1-col];
      dec(row);
    end;
  end;
end;

(* For this function only, raw_width is in bytes, not pixels! *)
procedure packed_12_load_raw(rec: PRec);
var
  row, col: integer;
begin
  with rec^ do
  begin
    getbits(rec^, -1);
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := getbits(rec^, 12) shl 2;
      for col := width*3 div 2 to raw_width-1 do
        getbits(rec^, 8);
    end;
  end;
end;

procedure unpacked_load_raw (var rec: TRec; xorder: integer; rsh: integer);
var
  pixel: pwordarray;
  row, col: integer;
begin
  with rec do
  begin
    pixel := allocmem(raw_width * sizeof(word));
    merror (pixel, 'unpacked_load_raw()');
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(pixel[0], 2*raw_width); //fread (pixel, 2, raw_width, ifp);

      if (xorder <> ntohs($55aa)) then
        swab (PAnsiChar(pixel), PAnsiChar(pixel), width*2);

      for col := 0 to width-1 do
        BAYER(rec, row, col)^ := pixel[col] shl 8 shr (8+rsh);
    end;
    freemem(pixel);
  end;
end;

procedure be_16_load_raw(rec: PRec);		(* "be" = "big-endian" *)
begin
  unpacked_load_raw (rec^, $55aa, 0);
end;

procedure be_high_12_load_raw(rec: PRec);
begin
  unpacked_load_raw (rec^, $55aa, 2);
end;

procedure be_low_12_load_raw(rec: PRec);
begin
  unpacked_load_raw (rec^, $55aa, -2);
end;

procedure be_low_10_load_raw(rec: PRec);
begin
  unpacked_load_raw (rec^, $55aa, -4);
end;

procedure le_high_12_load_raw(rec: PRec);	(* "le" = "little-endian" *)
begin
  unpacked_load_raw (rec^, $aa55, 2);
end;

procedure olympus_cseries_load_raw(rec: PRec);
var
  irow, row, col: integer;
begin
  with rec^ do
  begin
    xprogress.per1 := 100 / height / 2;
    for irow := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * irow));

      row := irow * 2 mod height + irow div (height div 2);
      if (row < 2) then
      begin
        ifp.Position := data_offset - row*(-width*height*3 div 4 and -2048); //fseek (ifp, data_offset - row*(-width*height*3/4 & -2048), SEEK_SET);
        getbits(rec^, -1);
      end;
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := getbits(rec^, 12) shl 2;
    end;
  end;
end;

procedure eight_bit_load_raw(rec: PRec);
var
  pixel: pbytearray;
  row, col: integer;
begin
  with rec^ do
  begin
    pixel := allocmem(raw_width * sizeof(byte));
    merror (pixel, 'eight_bit_load_raw()');
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(pixel[0], raw_width); //fread (pixel, 1, raw_width, ifp);
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := pixel[col] shl 6;
    end;
    freemem (pixel);
  end;
end;

procedure casio_qv5700_load_raw(rec: PRec);
var
  data: array [0..3232-1] of byte;
  dp: pbytearray;
  pixel: array [0..2576-1] of word;
  pix: pwordarray;
  row, col: integer;
begin
  with rec^ do
  begin
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(data[0], 3232); //fread (data, 1, 3232, ifp);
      dp := @data;
      pix := @pixel;
      while ( int64(DWORD(dp)) < int64(DWORD(@data))+3220 ) do
      begin
        pix[0] := (dp[0] shl 2) + (dp[1] shr 6);
        pix[1] := (dp[1] shl 4) + (dp[2] shr 4);
        pix[2] := (dp[2] shl 6) + (dp[3] shr 2);
        pix[3] := (dp[3] shl 8) + (dp[4]     );
        inc(dp, 5);
        inc(pix, 4);
      end;
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := (pixel[col] and $3ff) shl 4;
    end;
  end;
end;

procedure nucore_load_raw(rec: PRec);
var
  data: pbytearray;
  dp: pbytearray;
  irow, row, col: integer;
begin
  with rec^ do
  begin
    data := allocmem(width * 2);
    merror (data, 'nucore_load_raw()');
    xprogress.per1 := 100 / height / 2;
    for irow := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * irow));

      ifp.Read(data^, 2*width); //fread (data, 2, width, ifp);
      if (model[0] = 'B') and (width = 2598) then
        row := height - 1 - irow div 2 - height div 2 * (irow and 1)
      else
        row := irow;
      dp := data;
      for col := 0 to width-1 do
      begin
        BAYER(rec^, row, col)^ := (dp[0] shl 2) + (dp[1] shl 10);
        inc(pbyte(dp), 2);
      end;
    end;
    freemem(data);
  end;
end;

function make_decoder_int (var rec: TRec; source: pintegerarray; level: integer): pintegerarray;
var
  cur: pdecode;
begin
  with rec do
  begin
    cur := free_decode; inc(free_decode);
    if (level < source[0]) then
    begin
      cur^.branch[0] := free_decode;
      source := make_decoder_int (rec, source, level+1);
      cur^.branch[1] := free_decode;
      source := make_decoder_int (rec, source, level+1);
    end
    else
    begin
      cur^.leaf := source[1];
      inc(pinteger(source) , 2);
    end;
    result := source;
  end;
end;

function radc_token (var rec: TRec; tree: integer): integer;
const
  source:array [0..259] of integer = (
    1,1, 2,3, 3,4, 4,2, 5,7, 6,5, 7,6, 7,8,
    1,0, 2,1, 3,3, 4,4, 5,2, 6,7, 7,6, 8,5, 8,8,
    2,1, 2,3, 3,0, 3,2, 3,4, 4,6, 5,5, 6,7, 6,8,
    2,0, 2,1, 2,3, 3,2, 4,4, 5,6, 6,7, 7,5, 7,8,
    2,1, 2,4, 3,0, 3,2, 3,3, 4,7, 5,5, 6,6, 6,8,
    2,3, 3,1, 3,2, 3,4, 3,5, 3,6, 4,7, 5,0, 5,8,
    2,3, 2,6, 3,0, 3,1, 4,4, 4,5, 4,7, 5,2, 5,8,
    2,4, 2,7, 3,3, 3,6, 4,1, 4,2, 4,5, 5,0, 5,8,
    2,6, 3,1, 3,3, 3,5, 3,7, 3,8, 4,0, 5,2, 5,4,
    2,0, 2,1, 3,2, 3,3, 4,4, 4,5, 5,6, 5,7, 4,8,
    1,0, 2,2, 2,-2,
    1,-3, 1,3,
    2,-17, 2,-5, 2,5, 2,17,
    2,-7, 2,2, 2,9, 2,18,
    2,-18, 2,-9, 2,-2, 2,7,
    2,-28, 2,28, 3,-49, 3,-9, 3,9, 4,49, 5,-79, 5,79,
    2,-1, 2,13, 2,26, 3,39, 4,-16, 5,55, 6,-37, 6,76,
    2,-26, 2,-13, 2,1, 3,-39, 4,16, 5,-55, 6,-76, 6,37
  );
var
  t: integer;
begin
  with rec do
  begin
    if (free_decode = @first_decode) then
    begin
      radc_token_s := @source;
      for t := 0 to 18-1 do
      begin
        radc_token_dstart[t] := free_decode;
        radc_token_s := make_decoder_int (rec, radc_token_s, 0);
      end;
    end;
    if (tree = 18) then
    begin
      if (model[2] = '4') then
      begin
        result := (getbits(rec, 5) shl 3) + 4;	(* DC40 *)
        exit;
      end
      else
      begin
        result := (getbits(rec, 6) shl 2) + 2;	(* DC50 *)
        exit;
      end;
    end;
    radc_token_dindex := radc_token_dstart[tree];
    while( radc_token_dindex^.branch[0]<>nil ) do
      radc_token_dindex := radc_token_dindex^.branch[getbits(rec, 1)];
    result := radc_token_dindex^.leaf;
  end;
end;

procedure memcpy(dest, source: Pointer; count: Integer);
begin
  Move(source^, dest^, count);
end;

// replacement of "!x" where "x" is "int"
function IENOT(x: integer): integer;
begin
  result := integer(not boolean(x));
end;

//#define PREDICTOR (c ? (buf[c][y-1][x] + buf[c][y][x+1]) / 2 : (buf[c][y-1][x+1] + 2*buf[c][y-1][x] + buf[c][y][x+1]) / 4)

procedure kodak_radc_load_raw(rec: PRec);
var
  row, col, tree, nreps, rep, step, i, c, s, r, x, y, val: integer;
  last: array [0..3-1] of smallint;
  mul: array [0..3-1] of smallint;
  buf: array [0..3-1] of array [0..3-1] of array [0..386-1] of smallint;

  function PREDICTOR: integer;
  begin
    result := IEIFI(c<>0 , (buf[c][y-1][x] + buf[c][y][x+1]) div 2 , (buf[c][y-1][x+1] + 2*buf[c][y-1][x] + buf[c][y][x+1]) div 4);
  end;

begin
  with rec^ do
  begin
    last[0] := 16;
    last[1] := 16;
    last[2] := 16;

    init_decoder(rec^);
    getbits(rec^, -1);
    for i := 0 to sizeof(buf) div sizeof(smallint)-1 do
      buf[0][0][i] := 2048;
    xprogress.per1 := 100 / height / 2;
    row := 0;
    while (row < height) do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      for i := 0 to 3-1 do
        mul[i] := getbits(rec^, 6);
      for c := 0 to 3-1 do
      begin
        val := (($1000000 div last[c] + $7ff) shr 12) * mul[c];
        s := IEIFI(val > 65564 , 10, 12);
        x := not (-1 shl (s-1));
        val := val shl (12-s);
        for i := 0 to sizeof(buf[0]) div sizeof(smallint)-1 do
          buf[c][0][i] := (buf[c][0][i] * val + x) shr s;
        last[c] := mul[c];
        for r := 0 to IENOT(c) do
        begin
          buf[c][2][width div 2] := mul[c] shl 7;
          buf[c][1][width div 2] := buf[c][2][width div 2];
          tree := 1; col := width div 2;
          while ( col > 0 ) do
          begin
            tree := radc_token(rec^, tree);
            if (tree<>0) then
            begin
              dec(col , 2);
              if (tree = 8) then
                for y := 1 to 3-1 do
                  for x := col+1 downto col do
                    buf[c][y][x] := radc_token(rec^, tree+10) * mul[c]
              else
                for y := 1 to 3-1 do
                  for x := col+1 downto col do
                    buf[c][y][x] := radc_token(rec^, tree+10) * 16 + PREDICTOR;
            end
            else
              repeat
                nreps := IEIFI( (col > 2) , radc_token(rec^, 9) + 1 , 1 );
                rep := 0;
                while ( rep < 8) and (rep < nreps) and (col > 0 ) do
                begin
                  dec(col , 2);
                  for y := 1 to 3-1 do
                    for x := col+1 downto col do
                      buf[c][y][x] := PREDICTOR;
                  if (rep and 1)<>0 then
                  begin
                    step := radc_token(rec^, 10) shl 4;
                    for y := 1 to 3-1 do
                      for x := col+1 downto col do
                        inc(buf[c][y][x] , step);
                  end;
                  inc(rep);
                end;
              until (nreps <> 9);
          end;
          for y := 0 to 2-1 do
            for x := 0 to width div 2 do
            begin
              val := (buf[c][y+1][x] shl 4) div mul[c];
              if (val < 0) then
                val := 0;
              if (c<>0) then
                BAYER(rec^, row+y*2+c-1, x*2+2-c)^ := val
              else
                BAYER(rec^, row+r*2+y, x*2+y)^ := val;
            end;
          memcpy( pointer(int64(DWORD(@buf[c][0]))+IENOT(c)*sizeof(smallint)) , @buf[c][2] , 386-2*IENOT(c) );
        end;
      end;
      for y := row to row+4-1 do
        for x := 0 to width-1 do
          if ((x+y) and 1)<>0 then
          begin
            val := (BAYER(rec^, y, x)^-2048)*2 + (BAYER(rec^, y, x-1)^+BAYER(rec^, y, x+1)^) div 2;
            if (val < 0) then
              val := 0;
            BAYER(rec^, y, x)^ := val;
          end;
      inc(row, 4);
    end;
  end;
end;

(*
function fill_input_buffer(var rec: TRec; cinfo: pointer): LongBool;
var
  nbytes: integer;
begin
  with rec do
  begin
    nbytes := ifp.Read(jpeg_buffer[0], 4096); //nbytes = fread (jpeg_buffer, 1, 4096, ifp);
    swab (PAnsiChar(@jpeg_buffer), PAnsiChar(@jpeg_buffer), nbytes);
    IEJPEG_Decomp_SetNextInput(cinfo, @jpeg_buffer, nbytes);
    result := TRUE;
  end;
end;

procedure kodak_jpeg_load_raw(rec: PRec);
type
  t3bytes=array [0..2] of byte;
  p3bytes=^t3bytes;
  t3bytesarray=array [0..maxint div 16] of p3bytes;
  p3bytesarray=^t3bytesarray;
  JSAMPLE_ARRAY = array[0..(MaxInt div SIZEOF(byte)) - 1] of byte;
  JSAMPROW = ^JSAMPLE_ARRAY;
  JSAMPROW_ARRAY = array[0..(MaxInt div SIZEOF(JSAMPROW)) - 1] of JSAMPROW;
  JSAMPARRAY = ^JSAMPROW_ARRAY;
var
  cinfo: pointer ;
  jerr: pointer;
  buf: JSAMPARRAY ;
  pixel: p3bytesarray; // 'byte (*pixel)[3]';
  row, col: integer;
  output_width, output_height, output_components, output_scanline: integer;
begin
  with rec^ do
  begin
    cinfo := IEJPEG_Decomp_AllocDecompStruct();
    jerr := IEJPEG_CreateErrorManager(nil);
    IEJPEG_Decomp_SetErrorManager(cinfo, jerr);
    //cinfo.err := jpeg_std_error (@jerr);  // inside IEJPEG_CreateErrMgr
    IEJPEG_Decomp_CreateDecompress(cinfo);
    IEJPEG_Decomp_SetupReadStream(cinfo, ifp, nil); //jpeg_stdio_src (@cinfo, ifp);
    IEJPEG_Decomp_SetFillInputBuffer(cinfo, @fill_input_buffer); //cinfo.src^.fill_input_buffer := fill_input_buffer;
    IEJPEG_Decomp_ReadHeader(cinfo, true); //jpeg_read_header (@cinfo, TRUE);
    IEJPEG_Decomp_StartDecompress(cinfo); // jpeg_start_decompress (@cinfo);
    output_width := IEJPEG_Decomp_GetOutputWidth(cinfo);
    output_height := IEJPEG_Decomp_GetOutputHeight(cinfo);
    output_components := IEJPEG_Decomp_GetOutputComponents(cinfo);
    if ((output_width <> width  ) or (output_height*2 <> height ) or (output_components <> 3)) then
    begin
      IEJPEG_Decomp_DestroyDecompress(cinfo); //jpeg_destroy_decompress (@cinfo);
      raise Exception.Create(': incorrect JPEG dimensions');
    end;
    buf := IEJPEG_Decomp_AllocSArray(cinfo, 1, width*3, 1); //buf := cinfo.mem^.alloc_sarray(j_common_ptr( @cinfo), JPOOL_IMAGE, width*3, 1);

    xprogress.per1 := 100 / output_height / 2;

    output_scanline := IEJPEG_Decomp_GetOutputScanline(cinfo);
    while (output_scanline < output_height) do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * output_scanline));

      row := output_scanline * 2;
      IEJPEG_Decomp_ReadScanlines(cinfo, buf, 1); //jpeg_read_scanlines (@cinfo, buf, 1);
      pixel := pointer( buf[0] );
      col := 0;
      while (col < width) do
      begin
        BAYER(rec^, row+0, col+0)^ := pixel[col+0][1] shl 6;
        BAYER(rec^, row+1, col+1)^ := pixel[col+1][1] shl 6;
        BAYER(rec^, row+0, col+1)^ := (pixel[col][0] + pixel[col+1][0]) shl 5;
        BAYER(rec^, row+1, col+0)^ := (pixel[col][2] + pixel[col+1][2]) shl 5;
        inc(col, 2);
      end;
    end;
    IEJPEG_Decomp_FinishDecompress(cinfo); //jpeg_finish_decompress (&cinfo);
    IEJPEG_Decomp_DestroyDecompress(cinfo); //jpeg_destroy_decompress (&cinfo);

    IEJPEG_Decomp_CleanupReadStream(cinfo);

    IEJPEG_Decomp_FreeDecompStruct(cinfo);
    IEJPEG_FreeErrorManager(jerr);
  end;
end;
*)

procedure kodak_dc120_load_raw(rec: PRec);
const
  mul: array [0..4-1] of integer = ( 162, 192, 187,  92 );
  add: array [0..4-1] of integer = (   0, 636, 424, 212 );
var
  pixel: array [0..848-1] of byte;
  row, shift, col: integer;
begin
  with rec^ do
  begin
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(pixel[0], 848); //fread (pixel, 848, 1, ifp);
      shift := row * mul[row and 3] + add[row and 3];
      for col := 0 to width-1 do
        BAYER(rec^, row, col)^ := pixel[(col + shift) mod 848] shl 6;
    end;
  end;
end;

procedure kodak_dc20_coeff (var rec: TRec; juice: single);
const
  my_coeff:array [0..3-1] of array [0..4-1] of single=
  ( (  2.25,  0.75, -1.75, -0.25 ),
    ( -0.25,  0.75,  0.75, -0.25 ),
    ( -0.25, -1.75,  0.75,  2.25 ) );
  flat:array [0..3-1] of array [0..4-1] of single=
  ( (  1, 0,   0,   0 ),
    (  0, 0.5, 0.5, 0 ),
    (  0, 0,   0,   1 ) );
var
  r, g: integer;
begin
  with rec do
  begin
    for r := 0 to 3-1 do
      for g := 0 to 4-1 do
        coeff[r][g] := my_coeff[r][g] * juice + flat[r][g] * (1-juice);
    use_coeff := 1;
  end;
end;

procedure kodak_easy_load_raw(rec: PRec);
var
  pixel: pbytearray;
  curve: array [0..$1000-1] of word;
  row, col, icol: dword;
begin
  with rec^ do
  begin
    kodak_curve (rec^, @curve);
    if (raw_width > width) then
      black := 0;
    pixel := allocmem(raw_width * sizeof(byte));
    merror (pixel, 'kodak_easy_load_raw()');
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(pixel[0], raw_width); //fread (pixel, 1, raw_width, ifp);
      for col := 0 to raw_width-1 do
      begin
        icol := col - left_margin;
        if (icol < width) then
          BAYER(rec^, row, icol)^ := curve[pixel[col]] shl 2
        else
          inc(black , curve[pixel[col]]);
      end;
    end;
    if (raw_width > width) then
      black := ( black shl 2) div ((raw_width - width) * height);
    if strncmp(model, 'DC2', 3)=0 then
      black := 0;
    freemem(pixel);
  end;
end;

procedure kodak_compressed_load_raw(rec: PRec);
var
  c: byte;
  blen: array [0..256-1] of byte;
  raw: array [0..6-1] of word;
  curve: array [0..$1000-1] of word;
  row, col, len, save, i, israw, bits: dword;
  pred: array [0..2-1] of dword;
  bitbuf: int64;
  diff: integer;
begin
  with rec^ do
  begin
    israw := 0;
    bits := 0;
    bitbuf := 0;

    kodak_curve (rec^, @curve);
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      for col := 0 to width-1 do
      begin
        if ((col and 255) = 0) then
        begin		(* Get the bit-lengths of the *)
          len := width - col;		(* next 256 pixel values      *)
          if (len > 256) then
            len := 256;
          save := ftell(ifp);
          i := 0;
          israw := 0;
          while ( i < len ) do
          begin
            c := fgetc(ifp);

            blen[i+0] := c and 15;
            if blen[i+0]>12 then
              israw := 1
            else
            begin
              blen[i+1] := c shr 4;
              if blen[i+1]>12 then
                israw := 1;
            end;

            inc(i, 2);
          end;
          bitbuf := 0;
          bits := 0;
          pred[0] := 0;
          pred[1] := 0;
          if (len mod 8 = 4) then
          begin
            bitbuf  := fgetc(ifp) shl 8;
            inc(bitbuf , fgetc(ifp) );
            bits := 16;
          end;
          if (israw<>0) then
            ifp.Position := save; //fseek (ifp, save, SEEK_SET);
        end;

        if (israw<>0) then			(* If the data is not compressed *)
        begin
          case (col and 7) of
            0: 
              begin
                ifp.Read(raw[0], 2*6); // fread (raw, 2, 6, ifp);
                for i := 0 to 6-1 do
                  raw[i] := ntohs(raw[i]);
                diff := raw[0] shr 12 shl 8 or raw[2] shr 12 shl 4 or raw[4] shr 12;
              end;
            1: 
              diff := raw[1] shr 12 shl 8 or raw[3] shr 12 shl 4 or raw[5] shr 12;
            else
              diff := raw[(col and 7) - 2] and $fff;
          end;
        end
        else
        begin				(* If the data is compressed *)
          len := blen[col and 255];		(* Number of bits for this pixel *)
          if (bits < len) then		(* Got enough bits in the buffer? *)
          begin
            i := 0;
            while (i < 32) do
            begin
              inc(bitbuf , int64(fgetc(ifp)) shl (bits+(i xor 8)));
              inc(i, 8);
            end;
            inc(bits , 32);
          end;
          diff := bitbuf and ($ffff shr (16-len));  (* Pull bits from buffer *)
          bitbuf := bitbuf shr len;
          dec(bits , len);
          if ((diff and (1 shl (len-1))) = 0) then
            dec(diff , (1 shl len) - 1);
          inc( pred[col and 1] , diff);
          diff := pred[col and 1];
        end;
        BAYER(rec^, row, col)^ := curve[diff] shl 2;
      end;
    end;
  end;
end;

procedure kodak_yuv_load_raw(rec: PRec);
var
  c: byte;
  blen: array [0..384-1] of byte;
  row, col, len, bits: dword;
  bitbuf: int64;
  i, li, si, diff, cb, cr: integer ;
  y: array [0..4-1] of integer;
  six: array [0..6-1] of integer;
  rgb: array [0..3-1] of integer;
  ip: pwordarray;
  curve: array [0..$1000-1] of word;
begin
  with rec^ do
  begin
    bits := 0;
    bitbuf := 0;
    li := 0;
    cb := 0;
    cr := 0;

    kodak_curve (rec^, @curve);
    xprogress.per1 := 100 / height / 2;
    row := 0;
    while ( row < height ) do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      col := 0;
      while ( col < width ) do
      begin
        if ((col and 127) = 0) then
        begin
          len := (width - col + 1) * 3 and -4;
          if (len > 384) then
            len := 384;
          i := 0;
          while ( i < len ) do
          begin
            c := fgetc(ifp);
            blen[i] := c and 15; inc(i);
            blen[i] := c shr 4; inc(i);
          end;
          li := 0;
          bitbuf := 0;
          bits := 0;
          y[1] := 0;
          y[3] := 0;
          cb := 0;
          cr := 0;
          if (len mod 8 = 4) then
          begin
            bitbuf  := fgetc(ifp) shl 8;
            inc( bitbuf , fgetc(ifp) );
            bits := 16;
          end;
        end;
        for si := 0 to 6-1 do
        begin
          len := blen[li]; inc(li);
          if (bits < len) then
          begin
            i := 0;
            while ( i < 32 ) do
            begin
              inc( bitbuf , fgetc(ifp) shl (bits+(i xor 8)) );
              inc(i, 8);
            end;
            inc(bits , 32 );
          end;
          diff := bitbuf and ($ffff shr (16-len));
          bitbuf := bitbuf shr len;
          dec(bits , len);
          if ((diff and (1 shl (len-1))) = 0) then
            dec( diff , (1 shl len) - 1 );
          six[si] := diff;
        end;
        y[0] := six[0] + y[1];
        y[1] := six[1] + y[0];
        y[2] := six[2] + y[3];
        y[3] := six[3] + y[2];
        inc( cb  , six[4] );
        inc( cr  , six[5] );
        for i := 0 to 4-1 do
        begin
          ip := pwordarray( @image[ (row+(i shr 1))*width + col+(i and 1) ][0] );
          rgb[0] := y[i] + cr;
          rgb[1] := y[i];
          rgb[2] := y[i] + cb;
          for c := 0 to 3-1 do
            if (rgb[c] > 0) then
              ip[c] := curve[rgb[c]] shl 2;
        end;
        inc(col, 2);
      end;
      inc(row, 2);
    end;
  end;
end;

procedure sony_decrypt (var rec: TRec; data: pdword; len: integer; start: integer; key: integer);
begin
  with rec do
  begin
    if (start<>0) then
    begin
      decrypt_p := 0;
      while( decrypt_p<4 ) do
      begin
        key := key * 48828125 + 1;
        pad[decrypt_p] := key ;
        inc(decrypt_p);
      end;
      pad[3] := pad[3] shl 1 or (pad[0] xor pad[2]) shr 31;
      decrypt_p := 4;
      while (decrypt_p<127) do
      begin
        pad[decrypt_p] := (pad[decrypt_p-4] xor pad[decrypt_p-2]) shl 1 or (pad[decrypt_p-3] xor pad[decrypt_p-1]) shr 31;
        inc(decrypt_p);
      end;
      decrypt_p := 0;
      while (decrypt_p<127) do
      begin
        pad[decrypt_p] := htonl(pad[decrypt_p]);
        inc(decrypt_p);
      end;
    end;
    while (len<>0) do
    begin
      dec(len);
      pad[decrypt_p and 127] := pad[(decrypt_p+1) and 127] xor pad[(decrypt_p+65) and 127];
      data^ := data^ xor pad[decrypt_p and 127] ;
      inc(decrypt_p);
      inc(data);
    end;
  end;
end;

procedure sony_load_raw(rec: PRec);
var
  head: array [0..40-1] of byte;
  pixel: array [0..3360-1] of word;
  i, key, row, col, icol: dword;
  bblack: int64;
begin
  with rec^ do
  begin
    bblack := 0;

    ifp.Position := 200896;  // fseek (ifp, 200896, SEEK_SET);
    ifp.Position := ifp.Position+  fgetc(ifp)*4 - 1; // fseek (ifp, (unsigned) fgetc(ifp)*4 - 1, SEEK_CUR);
    order := $4d4d;
    key := fget4(rec^, ifp);
    ifp.position := 164600; // fseek (ifp, 164600, SEEK_SET);
    ifp.Read(head[0], 40); // fread (head, 1, 40, ifp);
    sony_decrypt (rec^, @head, 10, 1, key);
    i := 26;
    while ( i > 22 ) do
    begin
      dec(i);
      key := key shl 8 or head[i];
    end;
    ifp.Position := 862144; // fseek (ifp, 862144, SEEK_SET);
    xprogress.per1 := 100 / height / 2;
    for row := 0 to height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      ifp.Read(pixel[0], 2*raw_width); // fread (pixel, 2, raw_width, ifp);
      sony_decrypt (rec^, @pixel, raw_width div 2, IENOT(row), key);
      for col := 0 to 3343-1 do
      begin
        icol := col-left_margin;
        if (icol < width) then
          BAYER(rec^, row, icol)^ := ntohs(pixel[col])
        else
          inc(bblack , ntohs(pixel[col]));
      end;
    end;
    black := bblack div ((3343 - width) * height);
  end;
end;

procedure sony_rgbe_coeff(var rec: TRec);
const
  my_coeff:array [0..3-1] of array [0..4-1] of single =
  ( (  1.321918,  0.000000,  0.149829, -0.471747 ),
    ( -0.288764,  1.129213, -0.486517,  0.646067 ),
    (  0.061336, -0.199343,  1.138007,  0.000000 ) );
var
  r, g: integer;
begin
  with rec do
  begin
    for r := 0 to 3-1 do
      for g := 0 to 4-1 do
        coeff[r][g] := my_coeff[r][g];
    use_coeff := 1;
  end;
end;

procedure foveon_decoder (var rec: TRec; huff: pdwordarray; code: dword);
var
  cur: pdecode;
  i, len: integer;
begin
  with rec do
  begin
    cur := free_decode; inc(free_decode);
    if (int64(DWORD(free_decode)) > int64(DWORD(@first_decode))+2048*sizeof(decode)) then
      raise Exception.Create(': decoder table overflow');
    if (code<>0) then
    begin
      for i := 0 to 1024-1 do
        if (huff[i] = code) then
        begin
          cur^.leaf := i;
          exit;
        end;
    end;
    len := code shr 27;
    if (len > 26) then
      exit;
    code := (len+1) shl 27 or (code and $3ffffff) shl 1;

    cur^.branch[0] := free_decode;
    foveon_decoder (rec, huff, code);
    cur^.branch[1] := free_decode;
    foveon_decoder (rec, huff, code+1);
  end;
end;

procedure foveon_load_raw(rec: PRec);
var
  dindex: pdecode;
  diff: array [0..1024-1] of smallint;
  pred: array [0..3-1] of smallint;
  huff: array [0..1024-1] of dword;
  bitbuf: dword;
  row, col, bit, c, i: integer;
begin
  with rec^ do
  begin
    bitbuf := 0;
    bit := -1;

    ifp.Position := 260; //fseek (ifp, 260, SEEK_SET);
    for i := 0 to 1024-1 do
      diff[i] := fget2(rec^, ifp);
    for i := 0 to 1024-1 do
      huff[i] := fget4(rec^, ifp);

    init_decoder(rec^);
    foveon_decoder (rec^, @huff, 0);

    xprogress.per1 := 100 / raw_height / 2;

    for row := 0 to raw_height-1 do
    begin

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * row));

      memset (@pred, 0, sizeof(pred));
      if (bit=0) then
        fget4(rec^, ifp);
      bit := 0;
      for col := 0 to raw_width-1 do
      begin
        for c := 0 to 3-1 do
        begin
          dindex := @first_decode;
          while ( dindex^.branch[0]<>nil ) do
          begin
            bit := (bit-1) and 31;
            if (bit = 31) then
              for i := 0 to 4-1 do
                bitbuf := (bitbuf shl 8) + fgetc(ifp);
            dindex := dindex^.branch[bitbuf shr bit and 1];
          end;
          inc( pred[c] , diff[dindex^.leaf] );
        end;
        if ((row-top_margin)  >= height) or ((col-left_margin) >= width) or ((col-left_margin) <0) or ((row-top_margin) <0) then
          continue;
        for c := 0 to 3-1 do
          if (pred[c] > 0) then
            if (row-top_margin)*width+(col-left_margin)>0 then
              image[(row-top_margin)*width+(col-left_margin)][c] := pred[c];
      end;
    end;
  end;
end;

function apply_curve (i: integer; curve: pintegerarray): integer;
begin
  if (i <= -curve[0]) then
    result := -curve[curve[0]]-1
  else
  if (i < 0) then
    result := -curve[1-i]
  else
  if (i < curve[0]) then
    result := curve[1+i]
  else
    result := curve[curve[0]]+1;
end;

// is like "v shr t", but works with negative values
function nshr(v: integer; t: integer): integer;
begin
  if v<0 then
    result := -((-v) shr t)
  else
    result := v shr t;
end;

procedure foveon_interpolate(var rec: TRec);
const
  mul:array [0..3-1] of single = ( 1.0321, 1.0, 1.1124 );
  weight:array [0..3-1] of array [0..3-1] of array [0..3-1] of integer =
  ( ( (   4141,  37726,  11265  ),
      ( -30437,  16066, -41102  ),
      (    326,   -413,    362  ) ),
    ( (   1770,  -1316,   3480  ),
      (  -2139,    213,  -4998  ),
      (  -2381,   3496,  -2008  ) ),
    ( (  -3838, -24025, -12968  ),
      (  20144, -12195,  30272  ),
      (   -631,  -2025,    822  ) ) );
  curve1:array [0..73-1] of integer = ( 72,
     0,1,2,2,3,4,5,6,6,7,8,9,9,10,11,11,12,13,13,14,14,
    15,16,16,17,17,18,18,18,19,19,20,20,20,21,21,21,22,
    22,22,23,23,23,23,23,24,24,24,24,24,25,25,25,25,25,
    25,25,25,26,26,26,26,26,26,26,26,26,26,26,26,26,26 );
  curve2:array [0..21-1] of integer = ( 20,
    0,1,1,2,3,3,4,4,5,5,6,6,6,7,7,7,7,7,7,7 );
  curve3:array [0..73-1] of integer = ( 72,
     0,1,1,2,2,3,4,4,5,5,6,6,7,7,8,8,8,9,9,10,10,10,10,
    11,11,11,12,12,12,12,12,12,13,13,13,13,13,13,13,13,
    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14 );
  curve4:array [0..37-1] of integer = ( 36,
    0,1,1,2,3,3,4,4,5,6,6,7,7,7,8,8,9,9,9,10,10,10,
    11,11,11,11,11,12,12,12,12,12,12,12,12,12 );
  curve5:array [0..111-1] of integer = ( 110,
    0,1,1,2,3,3,4,5,6,6,7,7,8,9,9,10,11,11,12,12,13,13,
    14,14,15,15,16,16,17,17,18,18,18,19,19,19,20,20,20,
    21,21,21,21,22,22,22,22,22,23,23,23,23,23,24,24,24,24,
    24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,26,26,
    26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,
    26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26 );
  curves:array [0..3-1] of pintegerarray = ( @curve3, @curve4, @curve5 );
  trans:array [0..3-1] of array [0..3-1] of integer =
  ( (   7576,  -2933,   1279  ),
    ( -11594,  29911, -12394  ),
    (   4000, -18850,  20772  ) );
type
  tword3array=array [0..maxint div 16] of array [0..2] of word;
  pword3array=^tword3array;
  tint3array=array [0..maxint div 16] of array [0..2] of integer;
  pint3array=^tint3array;
var
  pix: pwordarray;
  prev: array [0..3-1] of word;
  xshrink: pword3array; // ushort (*shrink)[3];
  row, col, c, i, j, diff, sum: integer;
  ipix: array [0..3-1] of integer;
  work: array [0..3-1] of array [0..3-1] of integer;
  total: array [0..4-1] of integer;
  smrow: array [0..7-1] of pint3array; //int (*smrow[7])[3], 
  smlast, smred, smred_p: integer;
  hood: array [0..7-1] of integer;
  min, max: integer;
  pix1, pix2: pword;
begin

  with rec do
  begin
    smred_p := 0;

    (* Sharpen all colors *)
    for row := 0 to height-1 do
    begin
      pix := pwordarray(@image[row*width]);
      memcpy (@prev[0], pix, sizeof(prev));
      for col := 0 to width-1 do
      begin
        for c := 0 to 3-1 do
        begin
          diff := pix[c] - prev[c];
          prev[c] := pix[c];

          ipix[c] := pix[c] + nshr((diff + (diff*diff shr 14)) * $3333 , 14);

        end;
        for c := 0 to 3-1 do
        begin
          work[0][c] := ipix[c]*ipix[c] shr 14;
          work[2][c] := ipix[c]*work[0][c] shr 14;
          work[1][2-c] := ipix[(c+1) mod 3] * ipix[(c+2) mod 3] shr 14;
        end;
        for c := 0 to 3-1 do
        begin
          sum := 0;
          for i := 0 to 3-1 do
            for j := 0 to 3-1 do
              inc(sum , weight[c][i][j] * work[i][j] );

          //ipix[c] := trunc( (ipix[c] + (sum shr 14)) * mul[c] );
          ipix[c] := trunc( (ipix[c] + nshr(sum , 14)) * mul[c] );

          if (ipix[c] < 0) then
            ipix[c] := 0;
          if (ipix[c] > 32000) then
            ipix[c] := 32000;
          pix[c] := ipix[c];
        end;
        inc( pword(pix) , 4 );
      end;
    end;

    (* Array for 5x5 Gaussian averaging of red values *)
    smrow[6] := allocmem(width*5 * sizeof(integer)*3);
    merror (smrow[6], 'foveon_interpolate()');
    for i := 0 to 5-1 do
      smrow[i] := pointer( int64(DWORD(smrow[6])) + i*width*sizeof(integer)*3 );

    (* Sharpen the reds against these Gaussian averages *)
    smlast := -1;
    for row := 2 to height-2-1 do
    begin
      while (smlast < row+2) do
      begin
        for i := 0 to 6-1 do
          smrow[(i+5) mod 6] := smrow[i];
        inc(smlast);
        pix := pwordarray(@image[smlast*width+2]);
        for col := 2 to width-2-1 do
        begin
          pix1 := pword(pix); dec(pix1, 4);
          pix2 := pword(pix); dec(pix2, 8);

          smrow[4][col][0] := (pix[0]*6 + (pix1^+pix[4])*4 + pix2^+pix[8] + 8) shr 4;

          inc(pword(pix) , 4);
        end;
      end;
      pix := pwordarray(@image[row*width+2]);
      for col := 2 to width-2-1 do
      begin
        smred := nshr( (smrow[2][col][0]*6 + (smrow[1][col][0]+smrow[3][col][0])*4 + smrow[0][col][0]+smrow[4][col][0] + 8) , 4 );
        if (col = 2) then
          smred_p := smred;
        i := pix[0] + nshr( (pix[0] - nshr((smred*7 + smred_p) , 3)) , 2 );
        if (i < 0) then
          i := 0;
        if (i > 10000) then
          i := 10000;
        pix[0] := i;
        smred_p := smred;
        inc( pword(pix) , 4);
      end;
    end;

    (* Limit each color value to the range of its neighbors *)
    hood[0] := 4;
    for i := 0 to 3-1 do
    begin
      hood[i+1] := (i-width-1)*4;
      hood[i+4] := (i+width-1)*4;
    end;
    for row := 1 to height-1-1 do
    begin
      pix := pwordarray(@image[row*width+1]);
      pix1 := pword(pix); dec(pix1, 4);
      memcpy (@prev[0], pix1, sizeof(prev));
      for col := 1 to width-1-1 do
      begin
        for c := 0 to 3-1 do
        begin
          max := prev[c];
          min := max;
          for i := 0 to 7-1 do
          begin
            j := pix[hood[i]];
            if (min > j) then
              min := j;
            if (max < j) then
              max := j;
          end;
          prev[c] := pix[0];
          if (pix[0] < min) then
            pix[0] := min;
          if (pix[0] > max) then
            pix[0] := max;
          inc(pword(pix));
        end;
        inc(pword(pix));
      end;
    end;

  (*
     Because photons that miss one detector often hit another, 
     the sum R+G+B is much less noisy than the individual colors.
     So smooth the hues without smoothing the total.
   *)
    smlast := -1;
    for row := 2 to height-2-1 do
    begin
      while (smlast < row+2) do
      begin
        for i := 0 to 6-1 do
          smrow[(i+5) mod 6] := smrow[i];
        inc(smlast);
        pix := pwordarray(@image[smlast*width+2][0]);
        for col := 2 to width-2-1 do
        begin
          for c := 0 to 3-1 do
            smrow[4][col][c] := pix[c-8]+pix[c-4]+pix[c]+pix[c+4]+pix[c+8];
          inc(pword(pix) , 4);
        end;
      end;
      pix := pwordarray(@image[row*width+2][0]);
      for col := 2 to width-2-1 do
      begin
        total[3] := 1500;
        sum := 60;
        for c := 0 to 3-1 do
        begin
          total[c] := 0;
          for i := 0 to 5-1 do
            inc( total[c] , smrow[i][col][c] );
          inc( total[3] , total[c] );
          inc( sum , pix[c]);
        end;
        j := (sum shl 16) div total[3];
        for c := 0 to 3-1 do
        begin
          i := apply_curve ((nshr(total[c] * j , 16)) - pix[c], @curve1);
          inc( i , pix[c] - 13 - integer(c=1) );
          ipix[c] := i - apply_curve (i, @curve2);
        end;
        sum := nshr( (ipix[0]+ipix[1]+ipix[1]+ipix[2]) , 2 );
        for c := 0 to 3-1 do
        begin
          i := ipix[c] - apply_curve (ipix[c] - sum, @curve2);
          if (i < 0) then
            i := 0;
          pix[c] := i;
        end;
        inc( pword(pix) , 4);
      end;
    end;

    (* Translate the image to a different colorspace *)
    pix := pwordarray(@image[0]);
    while ( int64(DWORD(pix)) < int64(DWORD(@image[height*width])) ) do
    begin
      for c := 0 to 3-1 do
      begin
        i := 0;
        for j := 0 to 3-1 do
          inc(i , trans[c][j] * pix[j]);
        i := nshr( (i+$1000) , 13 );
        if (i < 0) then
          i := 0;
        if (i > 24000) then
          i := 24000;
        ipix[c] := i;
      end;
      for c := 0 to 3-1 do
        pix[c] := ipix[c];
      inc(pword(pix), 4);
    end;

    (* Smooth the image bottom-to-top and save at 1/4 scale *)
    xshrink := allocmem((width div 4) * (height div 4) * sizeof(word)*3);
    merror (xshrink, 'foveon_interpolate()');
    row := height div 4;
    while ( row<>0 ) do
    begin
      dec(row);
      for col := 0 to width div 4-1 do
      begin
        ipix[0] := 0;
        ipix[1] := 0;
        ipix[2] := 0;
        for i := 0 to 4-1 do
          for j := 0 to 4-1 do
            for c := 0 to 3-1 do
              inc( ipix[c] , image[(row*4+i)*width+col*4+j][c] );
        for c := 0 to 3-1 do
          if (row+2 > height div 4) then
            xshrink[row*(width div 4)+col][c] := ipix[c] shr 4
          else
            xshrink[row*(width div 4)+col][c] := nshr( (xshrink[(row+1)*(width div 4)+col][c]*1840 + ipix[c]*141) , 12);
      end;
    end;

    (* From the 1/4-scale image, smooth right-to-left *)
    for row := 0 to (height and not 3)-1 do
    begin
      ipix[0] := 0;
      ipix[1] := 0;
      ipix[2] := 0;
      if ((row and 3) = 0) then
      begin
        col := width and not 3;
        while ( col<>0 ) do
        begin
          dec(col);
          for c := 0 to 3-1 do
          begin
            ipix[c] := nshr( (xshrink[(row div 4)*(width div 4)+col div 4][c]*1485 + ipix[c]*6707) , 13 );
            smrow[0][col][c] := ipix[c] ;
          end;
        end;
      end;

      (* Then smooth left-to-right *)
      ipix[0] := 0;
      ipix[1] := 0;
      ipix[2] := 0;
      for col := 0 to (width and not 3)-1 do
        for c := 0 to 3-1 do
        begin
          ipix[c] := nshr( (smrow[0][col][c]*1485 + ipix[c]*6707) , 13 );
          smrow[1][col][c] := ipix[c] ;
        end;

      (* Smooth top-to-bottom *)
      if (row = 0) then
        memcpy (smrow[2], smrow[1], sizeof(integer)*3 * width)
      else
        for col := 0 to (width and not 3)-1 do
          for c := 0 to 3-1 do
            smrow[2][col][c] := nshr( (smrow[2][col][c]*6707 + smrow[1][col][c]*1485) , 13 );

      (* Adjust the chroma toward the smooth values *)
      for col := 0 to (width and not 3)-1 do
      begin
        i := 60;
        j := 60;
        for c := 0 to 3-1 do
        begin
          inc( i , smrow[2][col][c] );
          inc( j , image[row*width+col][c] );
        end;
        j := (j shl 16) div i;
        sum := 0;
        for c := 0 to 3-1 do
        begin
          i := nshr(smrow[2][col][c] * j , 16) - image[row*width+col][c];
          ipix[c] := apply_curve (i, curves[c]);
          inc( sum , ipix[c] );
        end;
        sum := nshr( sum , 3);
        for c := 0 to 3-1 do
        begin
          i := image[row*width+col][c] + ipix[c] - sum;
          if (i < 0) then
            i := 0;
          image[row*width+col][c] := i;
        end;
      end;

    end;
    freemem(xshrink);
    freemem(smrow[6]);
  end;
end;

const
  MinSingle        =     1.5e-45;
  MaxSingle        =     3.4e+38;
  MinDouble        =     5.0e-324;
  MaxDouble        =     1.7e+308;


procedure scale_colors(var rec: TRec);
var
  row, col, c, val: integer;
  min, max, count: array [0..4-1] of integer;
  sum: array [0..4-1] of double;
  dmin, dmax: double;
begin
  with rec do
  begin
    dec( rgb_max , black );
    if (use_auto_wb<>0) or ((use_camera_wb<>0) and (camera_red = -1)) then
    begin
      for c := 0 to 4-1 do
      begin
        min[c] := maxint;
        max[c] := 0;
        count[c] := 0;
        sum[c] := 0;
      end;
      for row := 0 to height-1 do
        for col := 0 to width-1 do
          for c := 0 to colors-1 do
          begin
            val := image[row*width+col][c];
            if (val=0) then
              continue;
            if (min[c] > val) then
              min[c] := val;
            if (max[c] < val) then
              max[c] := val;
            dec( val , black );
            if (val > rgb_max-100) then
              continue;
            if (val < 0) then
              val := 0;
            sum[c] := sum[c]+val;
            inc(count[c]);
          end;
      dmax := 0;
      for c := 0 to colors-1 do
      begin
        sum[c] := sum[c] / count[c];
        if (dmax < sum[c]) then
          dmax := sum[c];
      end;
      for c := 0 to colors-1 do
        pre_mul[c] := dmax / sum[c];
    end;
    if (use_camera_wb<>0) and (camera_red <> -1) then
    begin
      for c := 0 to 4-1 do
      begin
        sum[c] := 0;
        count[c] := 0;
      end;
      for row := 0 to 8-1 do
        for col := 0 to 8-1 do
        begin

          //c := FC(row, col);
          c := (filters shr (((row shl 1 and 14) + (col and 1)) shl 1) and 3);

          val := white[row][col] - black;
          if (val > 0) then
            sum[c] := sum[c] + val;
          inc(count[c]);
        end;
      dmin := maxdouble;
      dmax := 0;
      for c := 0 to colors-1 do
      begin
        sum[c] := sum[c] / count[c];
        if (dmin > sum[c]) then
          dmin := sum[c];
        if (dmax < sum[c]) then
          dmax := sum[c];
      end;
      if (dmin > 0) then
        for c := 0 to colors-1 do
          pre_mul[c] := dmax / sum[c]
      else
        if (camera_red<>0) and (camera_blue<>0) then
        begin
          pre_mul[0] := camera_red;
          pre_mul[2] := camera_blue;
          pre_mul[3] := 1.0;
          pre_mul[1] := pre_mul[3];
        end
        else
          ;//raise Exception.Create(': Cannot use camera white balance.');
    end;
    if (use_coeff=0) then
    begin
      pre_mul[0] := pre_mul[0] * red_scale;
      pre_mul[2] := pre_mul[2] * blue_scale;
    end;
    for row := 0 to height-1 do
      for col := 0 to width-1 do
        for c := 0 to colors-1 do
        begin
          val := image[row*width+col][c];
          if (val=0) then
            continue;
          dec( val , black );
          val := trunc( val * pre_mul[c] );
          if (val < 0) then
            val := 0;
          if (val > rgb_max) then
            val := rgb_max;
          image[row*width+col][c] := val;
        end;
  end;
end;

(*
   This algorithm is officially called:

   "Interpolation using a Threshold-based variable number of gradients"

   described in http://www-ise.stanford.edu/~tingchen/algodep/vargra.html

   I've extended the basic idea to work with non-Bayer filter arrays.
   Gradients are numbered clockwise from NW=0 to W=7.
 *)
procedure vng_interpolate(var rec: TRec);
const
  terms:array [0..384-1] of integer = (
    -2,-2,+0,-1,0,$01, -2,-2,+0,+0,1,$01, -2,-1,-1,+0,0,$01,
    -2,-1,+0,-1,0,$02, -2,-1,+0,+0,0,$03, -2,-1,+0,+1,1,$01,
    -2,+0,+0,-1,0,$06, -2,+0,+0,+0,1,$02, -2,+0,+0,+1,0,$03,
    -2,+1,-1,+0,0,$04, -2,+1,+0,-1,1,$04, -2,+1,+0,+0,0,$06,
    -2,+1,+0,+1,0,$02, -2,+2,+0,+0,1,$04, -2,+2,+0,+1,0,$04,
    -1,-2,-1,+0,0,$80, -1,-2,+0,-1,0,$01, -1,-2,+1,-1,0,$01,
    -1,-2,+1,+0,1,$01, -1,-1,-1,+1,0,$88, -1,-1,+1,-2,0,$40,
    -1,-1,+1,-1,0,$22, -1,-1,+1,+0,0,$33, -1,-1,+1,+1,1,$11,
    -1,+0,-1,+2,0,$08, -1,+0,+0,-1,0,$44, -1,+0,+0,+1,0,$11,
    -1,+0,+1,-2,1,$40, -1,+0,+1,-1,0,$66, -1,+0,+1,+0,1,$22,
    -1,+0,+1,+1,0,$33, -1,+0,+1,+2,1,$10, -1,+1,+1,-1,1,$44,
    -1,+1,+1,+0,0,$66, -1,+1,+1,+1,0,$22, -1,+1,+1,+2,0,$10,
    -1,+2,+0,+1,0,$04, -1,+2,+1,+0,1,$04, -1,+2,+1,+1,0,$04,
    +0,-2,+0,+0,1,$80, +0,-1,+0,+1,1,$88, +0,-1,+1,-2,0,$40,
    +0,-1,+1,+0,0,$11, +0,-1,+2,-2,0,$40, +0,-1,+2,-1,0,$20,
    +0,-1,+2,+0,0,$30, +0,-1,+2,+1,1,$10, +0,+0,+0,+2,1,$08,
    +0,+0,+2,-2,1,$40, +0,+0,+2,-1,0,$60, +0,+0,+2,+0,1,$20,
    +0,+0,+2,+1,0,$30, +0,+0,+2,+2,1,$10, +0,+1,+1,+0,0,$44,
    +0,+1,+1,+2,0,$10, +0,+1,+2,-1,1,$40, +0,+1,+2,+0,0,$60,
    +0,+1,+2,+1,0,$20, +0,+1,+2,+2,0,$10, +1,-2,+1,+0,0,$80,
    +1,-1,+1,+1,0,$88, +1,+0,+1,+2,0,$08, +1,+0,+2,-1,0,$40,
    +1,+0,+2,+1,0,$10
  );
  chood:array [0..16-1] of integer= ( -1,-1, -1,0, -1,+1, 0,+1, +1,+1, +1,0, +1,-1, 0,-1 );
type
  tword4array=array [0..maxint div 16] of array [0..3] of word;
  pword4array=^tword4array;
var
  brow: array [0..5-1] of pword4array; // word (*brow[5])[4];
  pix: pwordarray;
  code: array [0..8-1] of array [0..2-1] of array [0..320-1] of integer;
  ip: pinteger;
  ip1: pinteger;
  gval: array [0..8-1] of integer;
  gmin, gmax: integer;
  sum: array [0..4-1] of integer;
  row, col, shift, x, y, x1, x2, y1, y2, t, weight, grads, color, diag: integer;
  g, diff, thold, num, c: integer;
begin
  with rec do
  begin
    for row := 0 to 8-1 do		(* Precalculate for bilinear *)
    begin
      for col := 1 to 3-1 do
      begin
        ip := @code[row][col and 1];
        memset (@sum, 0, sizeof(sum));
        for y := -1 to 1 do
          for x := -1 to 1 do
          begin
            shift := integer(y=0) + integer(x=0);
            if (shift = 2) then
              continue;

            //color := FC(row+y, col+x);
            color := (filters shr ((((row+y) shl 1 and 14) + ((col+x) and 1)) shl 1) and 3);

            ip^ := (width*y + x)*4 + color; inc(ip);
            ip^ := shift; inc(ip);
            ip^ := color; inc(ip);
            inc( sum[color] , 1 shl shift);
          end;
        for c := 0 to colors-1 do
          if (c<>(filters shr (((row shl 1 and 14) + (col and 1)) shl 1) and 3)) then
          begin
            ip^ := c; inc(ip);
            ip^ := sum[c]; inc(ip);
          end;
      end;
    end;
    for row := 1 to height-1-1 do	(* Do bilinear interpolation *)
    begin
      for col := 1 to width-1-1 do
      begin
        pix := pwordarray(@image[row*width+col][0]);
        ip := @code[row and 7][col and 1];
        memset (@sum, 0, sizeof(sum));
        g := 8;
        while ( g<>0 ) do
        begin
          dec(g);
          diff := pix[ip^]; inc(ip);
          diff := diff shl ip^; inc(ip);
          sum[ip^] := sum[ip^] + diff; inc(ip);
        end;
        g := colors;
        while true do
        begin
          dec(g);
          if g=0 then
            break;
          c := ip^; inc(ip);
          pix[c] := sum[c] div ip^; inc(ip);
        end;
      end;
    end;
    if (quick_interpolate<>0) then
      exit;

    for row := 0 to 8-1 do		(* Precalculate for VNG *)
    begin
      for col := 0 to 2-1 do
      begin
        ip := @code[row][col];
        vng_interpolate_cp := @terms;
        for t := 0 to 64-1 do
        begin
          y1 := vng_interpolate_cp^; inc(vng_interpolate_cp);
          x1 := vng_interpolate_cp^; inc(vng_interpolate_cp);
          y2 := vng_interpolate_cp^; inc(vng_interpolate_cp);
          x2 := vng_interpolate_cp^; inc(vng_interpolate_cp);
          weight := vng_interpolate_cp^; inc(vng_interpolate_cp);
          grads := vng_interpolate_cp^; inc(vng_interpolate_cp);
          color := FC(rec, row+y1, col+x1);
          if (FC(rec, row+y2, col+x2) <> color) then
            continue;
          diag := IEIFI( (FC(rec, row, col+1) = color) and (FC(rec, row+1, col) = color) , 2, 1);
          if (abs(y1-y2) = diag) and (abs(x1-x2) = diag) then
            continue;
          ip^ := (y1*width + x1)*4 + color; inc(ip);
          ip^ := (y2*width + x2)*4 + color; inc(ip);
          ip^ := weight; inc(ip);
          for g := 0 to 8-1 do
            if (grads and (1 shl g))<>0 then
            begin
              ip^ := g;
              inc(ip);
            end;
          ip^ := -1; inc(ip);
        end;
        ip^ := maxint; inc(ip);
        vng_interpolate_cp := @chood;
        for g := 0 to 8-1 do
        begin
          y := vng_interpolate_cp^; inc(vng_interpolate_cp);
          x := vng_interpolate_cp^; inc(vng_interpolate_cp);
          ip^ := (y*width + x) * 4; inc(ip);
          color := FC(rec, row, col);
          if (FC(rec, row+y, col+x) <> color) and (FC(rec, row+y*2, col+x*2) = color) then
          begin
            ip^ := (y*width + x) * 8 + color;
            inc(ip);
          end
          else
          begin
            ip^ := 0;
            inc(ip);
          end;
        end;
      end;
    end;
    brow[4] := allocmem (width*3 * sizeof(word)*4); // brow[4] = calloc (width*3, sizeof **brow);
    merror (brow[4], 'vng_interpolate()');
    for row := 0 to 3-1 do
      brow[row] := pointer( int64(DWORD(brow[4])) + row*width *sizeof(word)*4 ); // brow[row] = brow[4] + row*width;
    row := 2;
    while( row<height-2 ) do 		(* Do VNG interpolation *)
    begin
      for col := 2 to width-2-1 do
      begin
        pix := pwordarray(@image[row*width+col]);
        ip := @code[row and 7][col and 1];
        memset (@gval, 0, sizeof(gval));
        while (true) do		(* Calculate gradients *)
        begin
          g := ip^;
          if g=maxint then
            break;

          diff := pix[g] - pix[pintegerarray(ip)[1]];
          num := diff shr 31;
          diff := ((diff xor num) - num) shl pintegerarray(ip)[2];
          inc( gval[pintegerarray(ip)[3]] , diff );

          inc( ip , 5);
          ip1 := ip; dec(ip1);
          g := ip1^;
          if (g = -1) then
            continue;

          inc( gval[g] , diff);

          while (true) do
          begin
            g := ip^; inc(ip);
            if g=-1 then
              break;
            inc(gval[g] , diff);
          end;

        end;
        inc(ip);
        gmax := gval[0];
        gmin := gmax;			(* Choose a threshold *)
        for g := 1 to 8-1 do
        begin
          if (gmin > gval[g]) then
            gmin := gval[g];
          if (gmax < gval[g]) then
            gmax := gval[g];
        end;
        if (gmax = 0) then
        begin
          memcpy (@brow[2][col], pix, sizeof(word)*4);  // memcpy (brow[2][col], pix, sizeof *image);
          continue;
        end;
        thold := gmin + (gmax shr 1);
        memset (@sum, 0, sizeof(sum));
        color := FC(rec, row, col);
        num := 0;
        for g := 0 to 8-1 do		(* Average the neighbors *)
        begin
          if (gval[g] <= thold) then
          begin
            for c := 0 to colors-1 do
              if (c = color) and (pintegerarray(ip)[1]<>0) then
                inc( sum[c] , (pix[c] + pix[pintegerarray(ip)[1]]) shr 1)
              else
                inc( sum[c] , pix[ip^ + c] );
            inc(num);
          end;
          inc(ip, 2);
        end;
        for c := 0 to colors-1 do		(* Save to buffer *)
        begin
          t := pix[color];
          if (c <> color) then
          begin
            inc( t , (sum[c] - sum[color]) div num );
            if (t < 0) then
              t := 0;
            if (t > rgb_max) then
              t := rgb_max;
          end;
          brow[2][col][c] := t;
        end;
      end;
      if (row > 3) then				(* Write buffer to image *)
        memcpy (@image[(row-2)*width+2][0], pointer(int64(DWORD(brow[0]))+2*sizeof(word)*4), (width-4)*sizeof(word)*4);
      for g := 0 to 4-1 do
        brow[(g-1) and 3] := brow[g];
      inc(row);
    end;
    memcpy (@image[(row-2)*width+2][0], pointer(int64(DWORD(brow[0]))+2*sizeof(word)*4), (width-4)*sizeof(word)*4);
    memcpy (@image[(row-1)*width+2][0], pointer(int64(DWORD(brow[1]))+2*sizeof(word)*4), (width-4)*sizeof(word)*4);
    freemem(brow[4]);
  end;
end;

procedure tiff_parse_subifd (var rec: TRec; base: integer);
var
  entries, tag, xtype, len, val, save: integer;
begin
  with rec do
  begin
    entries := fget2(rec, ifp);
    while (entries<>0) do
    begin
      dec(entries);
      tag  := fget2(rec, ifp);
      xtype := fget2(rec, ifp);
      len  := fget4(rec, ifp);
      if (xtype = 3) and (len < 3) then
      begin
        val := fget2(rec, ifp);  fget2(rec, ifp);
      end
      else
        val := fget4(rec, ifp);
      case (tag) of
        $100: 		(* ImageWidth *)
          raw_width := val;
        $101: 		(* ImageHeight *)
          raw_height := val;
        $102: 		(* Bits per sample *)
          ;
        $103: 		(* Compression *)
          tiff_data_compression := val;
        $106: 		(* Kodak color format *)
          kodak_data_compression := val;
        $111: 		(* StripOffset *)
          if (len = 1) then
            data_offset := val
          else
          begin
            save := ftell(ifp);
            ifp.Position := val+base; //fseek (ifp, val+base, SEEK_SET);
            data_offset := fget4(rec, ifp);
            ifp.Position := save; //fseek (ifp, save, SEEK_SET);
          end;
        $115: 		(* SamplesPerRow *)
          ;
        $116: 		(* RowsPerStrip *)
          ;
        $117: 		(* StripByteCounts *)
          ;
        $123: 
          begin
            curve_offset := val;
            curve_length := len;
          end;
      end;
    end;
  end;
end;

procedure nef_parse_makernote(var rec: TRec);
var
  base, offset, entries, tag, xtype, val, save: integer;
  sorder: smallint;
  buf: array [0..10-1] of AnsiChar;
begin
  with rec do
  begin
    base := 0;

  (*
     The MakerNote might have its own TIFF header (possibly with
     its own byte-order!), or it might just be a table.
   *)
    sorder := order;
    ifp.Read(buf[0], 10); //fread (buf, 1, 10, ifp);
    if (strcmp (buf, 'Nikon')=0) then	(* starts with "Nikon\0\2\0\0\0" ? *)
    begin
      base := ftell(ifp);
      order := fget2(rec, ifp);		(* might differ from file-wide byteorder *)
      fget2(rec, ifp);		(* should be 42 decimal *)
      offset := fget4(rec, ifp);
      ifp.Position := ifp.Position+ (offset-8); // fseek (ifp, offset-8, SEEK_CUR);
    end
    else
      if (strcmp (buf, 'OLYMP')=0) then
        ifp.Position := ifp.Position+ (-2) //fseek (ifp, -2, SEEK_CUR);
      else
        ifp.Position := ifp.Position+ (-10); //fseek (ifp, -10, SEEK_CUR);

    entries := fget2(rec, ifp);
    while (entries<>0) do
    begin
      dec(entries);
      tag  := fget2(rec, ifp);
      xtype := fget2(rec, ifp);
      fget4(rec, ifp);
      if (xtype = 3) then            (* short int *)
      begin
        val := fget2(rec, ifp);  fget2(rec, ifp);
      end
      else
        val := fget4(rec, ifp);
      save := ftell(ifp);
      if (tag = $c) then
      begin
        ifp.Position := base+val; //fseek (ifp, base+val, SEEK_SET);
        camera_red  := fget4(rec, ifp);
        camera_red  := camera_red / fget4(rec, ifp);
        camera_blue := fget4(rec, ifp);
        camera_blue := camera_blue / fget4(rec, ifp);
      end;
      if (tag = $8c) then
        curve_offset := base+val + 2112;
      if (tag = $96) then
        curve_offset := base+val + 2;
      if (tag = $97) then
      begin
        if (strcmp(model, 'NIKON D100 ')=0) then
        begin
          ifp.Position := base+val + 72; //fseek (ifp, base+val + 72, SEEK_SET);
          camera_red  := fget2(rec, ifp) / 256.0;
          camera_blue := fget2(rec, ifp) / 256.0;
        end
        else
          if (strcmp(model, 'NIKON D2H')=0) then
          begin
            ifp.Position := base+val + 10; //fseek (ifp, base+val + 10, SEEK_SET);
            camera_red  := fget2(rec, ifp);
            camera_red  := camera_red / fget2(rec, ifp);
            camera_blue := fget2(rec, ifp);
            camera_blue := fget2(rec, ifp) / camera_blue;
          end
          else
            if (strcmp(model, 'NIKON D70')=0) then
            begin
              ifp.Position := base+val + 20; //fseek (ifp, base+val + 20, SEEK_SET);
              camera_red  := fget2(rec, ifp);
              camera_red  := camera_red / fget2(rec, ifp);
              camera_blue := fget2(rec, ifp);
              camera_blue := camera_blue / fget2(rec, ifp);
            end;
      end;
      if (tag = $1017) then		(* Olympus *)
        camera_red  := val / 256.0;
      if (tag = $1018) then
        camera_blue := val / 256.0;
      ifp.Position := save; //fseek (ifp, save, SEEK_SET);
    end;
    order := sorder;
   end;
end;

(*
   Since the TIFF DateTime string has no timezone information,
   assume that the camera's clock was set to Universal Time.
 *)
 (*
procedure get_timestamp;
var
  tm t;
  time_t ts;
begin

  if (fscanf (ifp, "%d:%d:%d %d:%d:%d", &t.tm_year, &t.tm_mon,
	&t.tm_mday, &t.tm_hour, &t.tm_min, &t.tm_sec) != 6)
    return;
  t.tm_year -= 1900;
  t.tm_mon -= 1;
  putenv ("TZ=UTC");		// Remove this to assume local time
  if ((ts = mktime(&t)) > 0)
    timestamp = ts;
end;
*)
// yyyy:mm:dd hh:mm:ss
// 1234567890123456789
procedure get_timestamp(var rec: TRec);
var
  t: array [0..18] of AnsiChar;
(*
  year, month, day, hour, min, sec: integer;
  dt: tdatetime;
  *)
begin
  with rec do
  begin
    ifp.Read(t[0], 19);
    (*
    year := IEStrToIntDef(t[1], 0);
    month := IEStrToIntDef(t[6], 0);
    day := IEStrToIntDef(t[9], 0);
    hour := IEStrToIntDef(t[12], 0);
    min := IEStrToIntDef(t[15], 0);
    sec := IEStrToIntDef(t[18], 0);
    dt := EncodeDate(year, month, day) + EncodeTime(hour, min, sec, 0);
    timestamp := DateTimeToTimeStamp(dt);
    *)
  end;
end;

procedure nef_parse_exif (var rec: TRec; base: integer);
var
  entries, tag, val, save: integer;
begin
  with rec do
  begin
    entries := fget2(rec, ifp);
    while (entries<>0) do
    begin
      dec(entries);
      tag  := fget2(rec, ifp);
      fget2(rec, ifp);
      fget4(rec, ifp);
      val  := fget4(rec, ifp);
      save := ftell(ifp);
      ifp.Position := val+base; //fseek (ifp, val+base, SEEK_SET);
      if (tag = $9003) or (tag = $9004) then
        get_timestamp(rec);
      if (tag = IETIFFTAG_EXIFMAKERNOTE) and	((strncmp(make, 'NIKON', 5)=0) or (strncmp(make, 'OLYMPUS', 7)=0)) then
        nef_parse_makernote(rec);
      ifp.Position := save; //fseek (ifp, save, SEEK_SET);
    end;
  end;
end;

(*
   Parse a TIFF file looking for camera model and decompress offsets.
 *)
procedure parse_tiff (var rec: TRec; base: integer);
const
  flip_map: array [0..7] of integer = ( 0, 1, 3, 2, 4, 6, 7, 5 );
var
  doff, entries, tag, xtype, len, val, save: integer;
  software: array [0..64-1] of AnsiChar;
  wide, high, cr2_offset, offset: integer;
begin
  with rec do
  begin
    wide := 0;
    high := 0;
    cr2_offset := 0;
    offset := 0;

    ifp.Position := base; // fseek (ifp, base, SEEK_SET);
    order := fget2(rec, ifp);
    fget2(rec, ifp);		(* Should be 42 for standard TIFF *)
    doff := fget4(rec, ifp);
    while (doff<>0) and (doff+base<ifp.Size) do
    begin
      ifp.position := doff+base; //fseek (ifp, doff+base, SEEK_SET);
      entries := fget2(rec, ifp);
      while (entries<>0) do
      begin
        dec(entries);
        tag  := fget2(rec, ifp);
        xtype := fget2(rec, ifp);
        len  := fget4(rec, ifp);
        if (xtype = 3) and (len < 3) then
        begin
          val := fget2(rec, ifp);  fget2(rec, ifp);
        end
        else
          val := fget4(rec, ifp);
        save := ftell(ifp);
        ifp.Position := val+base; //fseek (ifp, val+base, SEEK_SET);
        case (tag) of
          $11: 
            camera_red  := val / 256.0;
          $12: 
            camera_blue := val / 256.0;
          $100: 			(* ImageWidth *)
            wide := val;
          $101: 			(* ImageHeight *)
            high := val;
          $10f: 			(* Make tag *)
            ifp.Read(make[0], 64); //fgets (make, 64, ifp);
          $110: 			(* Model tag *)
            ifp.Read(model[0], 64); //fgets (model, 64, ifp);
          $111: 			(* StripOffset *)
            begin
              cr2_offset := val;
              offset := fget4(rec, ifp);
            end;
          $112: 			(* Rotation *)
            flip := flip_map[(val-1) and 7];
          $123: 
            begin
              curve_offset := val;
              curve_length := len;
            end;
          $827d: 			(* Model2 tag *)
            ifp.Read(model2[0], 64); //fgets (model2, 64, ifp);
          $131: 			(* Software tag *)
            begin
              ifp.Read(software[0], 64); //fgets (software, 64, ifp);
              if (strncmp(software, 'Adobe', 5)=0) then
                make[0] := #0;
            end;
          $132: 			(* DateTime tag *)
            get_timestamp(rec);
          $144: 
            begin
              IEStrCopy(make, 'Leaf');
              raw_width  := wide;
              raw_height := high;
              if (len > 1) then
                data_offset := fget4(rec, ifp)
              else
                data_offset := val;
            end;
          $14a: 			(* SubIFD tag *)
            begin
              if (len > 2) and (strcmp(make, 'Kodak')=0) then
                  len := 2;
              if (len > 1) then
                while (len<>0) do
                begin
                  dec(len);
                  ifp.Position := val+base; //fseek (ifp, val+base, SEEK_SET);
                  ifp.Position := fget4(rec, ifp)+base; // fseek (ifp, fget4(ifp)+base, SEEK_SET);
                  tiff_parse_subifd(rec, base);
                  inc(val , 4);
                end
              else
                tiff_parse_subifd(rec, base);
            end;
          $8769: 			(* Nikon EXIF tag *)
            nef_parse_exif(rec, base);
        end;
        ifp.Position := save; //fseek (ifp, save, SEEK_SET);

      end;
      doff := fget4(rec, ifp);
    end;
    if (strncmp(make, 'OLYMPUS', 7)=0) then
    begin
      make[7] := #0;
      raw_width := wide;
      raw_height := - (-high and -2);
      data_offset := offset;
    end;
    if (strcmp(make, 'Canon')=0) and (strcmp(model, 'EOS D2000C')<>0) then
      data_offset := cr2_offset;

    if (make[0] = #0) and (wide = 680) and (high = 680) then
    begin
      IEStrCopy(make, 'Imacon');
      IEStrCopy(model, 'Ixpress');
    end;
  end;
end;

(*
   CIFF block $1030 contains an 8x8 white sample.
   Load this into white[][] for use in scale_colors().
 *)
procedure ciff_block_1030(var rec: TRec);
const
  key: array [0..1] of word = ( $410, $45f3 );
var
  i, bpp, row, col, vbits: integer;
  bitbuf: dword;
begin
  with rec do
  begin
    vbits := 0;
    bitbuf := 0;

    fget2(rec, ifp);
    if (fget4(rec, ifp) <> $80008) then
      exit;
    if (fget4(rec, ifp) = 0) then
      exit;
    bpp := fget2(rec, ifp);
    if (bpp <> 10) and (bpp <> 12) then
      exit;
    i := 0;
    for row := 0 to 8-1 do
      for col := 0 to 8-1 do
      begin
        if (vbits < bpp) then
        begin
          bitbuf := bitbuf shl 16 or (fget2(rec, ifp) xor key[i and 1]); inc(i);
          inc(vbits , 16);
        end;
        white[row][col] := 	bitbuf shl (LONG_BIT - vbits) shr (LONG_BIT - bpp) shl (14-bpp);
        dec(vbits , bpp);
      end;
  end;
end;

(*
   Parse the CIFF structure looking for two pieces of information:
   The camera model, and the decode table number.
 *)
procedure parse_ciff (var rec: TRec; offset: integer; length: integer);
const
  remap:array [0..5] of integer= ( 1,2,3,4,5,1 );
  remap_10d:array [0..9] of integer= ( 0,1,3,4,5,6,0,0,2,8 );
var
  tboff, nrecs, i, xtype, len, roff, aoff, save, wbi, q: integer;
label
  common;
begin
  with rec do
  begin
    wbi := -1;

    ifp.Position := offset+length-4;  // fseek (ifp, offset+length-4, SEEK_SET);
    tboff := fget4(rec, ifp) + offset;
    ifp.Position := tboff; //fseek (ifp, tboff, SEEK_SET);
    nrecs := fget2(rec, ifp);
    for i := 0 to nrecs - 1 do
    begin
      xtype := fget2(rec, ifp);
      len  := fget4(rec, ifp);
      roff := fget4(rec, ifp);
      aoff := offset + roff;
      save := ftell(ifp);
      if (xtype = $080a) then		(* Get the camera make and model *)
      begin
        ifp.Position := aoff;
        ifp.Read(make[0], 64);
        ifp.position := aoff + IEStrLen(make) + 1;
        ifp.Read(model[0], 64);
      end;
      if (xtype = $102a) then 		(* Find the White Balance index *)
      begin
        ifp.Position := aoff + 14; //fseek (ifp, aoff+14, SEEK_SET);	(* 0=auto, 1=daylight, 2=cloudy ... *)
        wbi := fget2(rec, ifp);
        if ((((strcmp(model, 'Canon EOS DIGITAL REBEL') = 0) or (strcmp(model, 'Canon EOS 300D DIGITAL') = 0))) and (wbi = 6)) then
          inc(wbi);
      end;
      if (xtype = $102c) then		(* Get white balance (G2) *)
      begin
        if ((strcmp(model, 'Canon PowerShot G1')=0) or (strcmp(model, 'Canon PowerShot Pro90 IS')=0)) then
        begin
          ifp.Position := aoff+120; //fseek (ifp, aoff+120, SEEK_SET);
          white[0][1] := fget2(rec, ifp) shl 4;
          white[0][0] := fget2(rec, ifp) shl 4;
          white[1][0] := fget2(rec, ifp) shl 4;
          white[1][1] := fget2(rec, ifp) shl 4;
        end
        else
        begin
          ifp.position := aoff+100; //fseek (ifp, aoff+100, SEEK_SET);
          if (wbi = 6) and (fget4(rec, ifp) = 0) then
            wbi := 15
          else
          begin
            ifp.Position := aoff+100; //fseek (ifp, aoff+100, SEEK_SET);
            goto common;
          end;
        end;
      end;
      if (xtype = $0032) then	(* Get white balance (D30 & G3) *)
      begin
        if (strcmp(model, 'Canon EOS D30')=0) then
        begin
          ifp.Position := aoff+72; //fseek (ifp, aoff+72, SEEK_SET);
  common: 
          camera_red   := fget2(rec, ifp);
          if camera_red<>0 then
            camera_red   := fget2(rec, ifp) / camera_red;
          camera_blue  := fget2(rec, ifp);
          q := fget2(rec, ifp);
          if q<>0 then
            camera_blue  := camera_blue / q;
        end
        else
        begin
          ifp.Position := aoff+80 + IEIFI(wbi < 6 , remap[wbi]*8 , 0); //fseek (ifp, aoff+80 + (wbi < 6 ? remap[wbi]*8 : 0), SEEK_SET);
          if (camera_red=0) then
            goto common;
        end;
      end;
      if (xtype = $10a9) then		(* Get white balance (D60) *)
      begin
        if (strcmp(model, 'Canon EOS 10D')=0) then
          wbi := remap_10d[wbi];
        ifp.position := aoff+2 + wbi*8; //fseek (ifp, aoff+2 + wbi*8, SEEK_SET);
        camera_red  := fget2(rec, ifp);
        camera_red  := camera_red / fget2(rec, ifp);
        camera_blue := fget2(rec, ifp);
        camera_blue := fget2(rec, ifp) / camera_blue;
      end;
      if (xtype = $1030) and (wbi > 14) then	(* Get white sample *)
      begin
        ifp.position := aoff; //fseek (ifp, aoff, SEEK_SET);
        ciff_block_1030(rec);
      end;
      if (xtype = $1031) then 		(* Get the raw width and height *)
      begin
        ifp.Position := aoff+2; //fseek (ifp, aoff+2, SEEK_SET);
        raw_width  := fget2(rec, ifp);
        raw_height := fget2(rec, ifp);
      end;
      if (xtype = $180e) then		(* Get the timestamp *)
      begin
        ifp.position := aoff; //fseek (ifp, aoff, SEEK_SET);
        timestamp.Time := fget4(rec, ifp);
      end;
      if (xtype = $580e) then
        timestamp.Time := len;
      if (xtype = $1810) then		(* Get the rotation *)
      begin
        ifp.Position := aoff+12; //fseek (ifp, aoff+12, SEEK_SET);
        case (fget4(rec, ifp)) of
          270:  flip := 5;
          180:  flip := 3;
          90:  flip := 6;
        end;
      end;
      if (xtype = $1835) then		(* Get the decoder table *)
      begin
        ifp.Position := aoff; //fseek (ifp, aoff, SEEK_SET);
        crw_init_tables (rec, fget4(rec, ifp));
      end;
      if (xtype shr 8 = $28) or (xtype shr 8 = $30) then	(* Get sub-tables *)
        parse_ciff(rec, aoff, len);
      ifp.Position := save; //fseek (ifp, save, SEEK_SET);
    end;
    if (wbi = 0) and (strcmp(model, 'Canon EOS D30')=0) then
      camera_red := -1;			(* Use my auto WB for this photo *)
  end;
end;

function strchr(s: PAnsiChar; c: integer): PAnsiChar;
begin
  result := IEStrScan(s, AnsiChar(c));
end;

procedure parse_rollei(var rec: TRec);
var
  line: array [0..128-1] of AnsiChar;
  val: PAnsiChar;
  tx, ty: integer;
  (*
  tm t;
  time_t ts;
  *)
  //day, month, year: integer;
begin
  with rec do
  begin
    tx := 0;
    ty := 0;

    ifp.Position := 0; //fseek (ifp, 0, SEEK_SET);
    repeat
      ifp.Read(line[0], 128); //fgets (line, 128, ifp);
      val := strchr(line, ord('='));
      if (val<>nil) then
      begin
        val^ := #0; inc(val);
      end
      else
        val := line + IEStrLen(line);
      if (strcmp(line, 'DAT')=0) then
      begin
        //sscanf (val, "%d.%d.%d", &t.tm_mday, &t.tm_mon, &t.tm_year);
      end;
      if (strcmp(line, 'TIM')=0) then
      begin
        //sscanf (val, "%d: %d: %d", &t.tm_hour, &t.tm_min, &t.tm_sec);
      end;
      if (strcmp(line, 'HDR')=0) then
        data_offset := IEStrToIntDef(AnsiString(val), 0);
      if (strcmp(line, 'X  ')=0) then
        raw_width := IEStrToIntDef(AnsiString(val), 0);
      if (strcmp(line, 'Y  ')=0) then
        raw_height := IEStrToIntDef(AnsiString(val), 0);
      if (strcmp(line, 'TX ')=0) then
        tx := IEStrToIntDef(AnsiString(val), 0);
      if (strcmp(line, 'TY ')=0) then
        ty := IEStrToIntDef(AnsiString(val), 0);
    until (strncmp(line, 'EOHD', 4)=0);
    (*
    t.tm_year -= 1900;
    t.tm_mon -= 1;
    putenv ("TZ=");
    if ((ts = mktime(&t)) > 0)
      timestamp = ts;
      *)
    inc( data_offset , tx * ty * 2 );
    IEStrCopy(make, 'Rollei');
    IEStrCopy(model, 'd530flex');
  end;
end;

procedure parse_foveon(var rec: TRec);
var
  buf, bp, np: PAnsiChar;
  off1, off2, len, i: integer;
begin
  with rec do
  begin
    order := $4949;			(* Little-endian *)
    ifp.position := ifp.Size-4;//fseek (ifp, -4, SEEK_END);
    off2 := fget4(rec, ifp);
    ifp.Position := off2; //fseek (ifp, off2, SEEK_SET);
    while (fget4(rec, ifp) <> $464d4143)	do (* Search for "CAMF" *)
      if ifp.Position>=ifp.Size then
        exit;
    off1 := fget4(rec, ifp);
    ifp.Position := off1+8; //fseek (ifp, off1+8, SEEK_SET);
    inc( off1 , (fget4(rec, ifp)+3) * 8 );
    len := (off2 - off1) div 2;
    ifp.position := off1; //fseek (ifp, off1, SEEK_SET);
    buf := allocmem (len);
    merror (buf, 'parse_foveon()');
    for i := 0 to len-1 do 		(* Convert Unicode to ASCII *)
      buf[i] := AnsiChar(fget2(rec, ifp));
    bp := buf;
    while ( bp < buf+len ) do
    begin
      np := bp + IEStrLen(bp) + 1;
      if (strcmp(bp, 'CAMMANUF')=0) then
        IEStrCopy(make, np);
      if (strcmp(bp, 'CAMMODEL')=0) then
        IEStrCopy(model, np);
      if (strcmp(bp, 'TIME')=0) then
        timestamp.Time := IEStrToIntDef(AnsiString(np), 0);
      bp := np;
    end;
    ifp.Position := 248; //fseek (ifp, 248, SEEK_SET);
    raw_width  := fget4(rec, ifp);
    raw_height := fget4(rec, ifp);
    freemem(buf);
  end;
end;

procedure foveon_coeff(var rec: TRec);
const
  foveon:array [0..3-1] of array [0..3-1] of single = (
    (  2.0343955, -0.727533, -0.3067457 ),
    ( -0.2287194,  1.231793, -0.0028293 ),
    ( -0.0086152, -0.153336,  1.1617814 )
  );
var
  i, j: integer;
begin
  with rec do
  begin
    for i := 0 to 3-1 do
      for j := 0 to 3-1 do
        coeff[i][j] := foveon[i][j] * pre_mul[i];
    use_coeff := 1;
  end;
end;

(*
   The grass is always greener in my PowerShot G2 when this
   function is called.  Use at your own risk!
 *)
procedure canon_rgb_coeff (var rec: TRec; juice: single);
const
  my_coeff: array [0..3-1] of array [0..3-1] of single =
  ( (  1.116187, -0.107427, -0.008760 ),
    ( -1.551374,  4.157144, -1.605770 ),
    (  0.090939, -0.399727,  1.308788 ) );
var
  i, j: integer;
begin
  with rec do
  begin
    for i := 0 to 3-1 do
      for j := 0 to 3-1 do
        coeff[i][j] := my_coeff[i][j] * juice + integer(i=j) * (1-juice);
    use_coeff := 1;
  end;
end;

procedure nikon_e950_coeff(var rec: TRec);
const
  my_coeff:array [0..3-1] of array [0..4-1] of single=
  ( ( -1.936280,  1.800443, -1.448486,  2.584324 ),
    (  1.405365, -0.524955, -0.289090,  0.408680 ),
    ( -1.204965,  1.082304,  2.941367, -1.818705 ) );
var
  r, g: integer;
begin
  with rec do
  begin
    for r := 0 to 3-1 do
      for g := 0 to 4-1 do
        coeff[r][g] := my_coeff[r][g];
    use_coeff := 1;
  end;
end;

(*
   Given a matrix that converts RGB to GMCY, create a matrix to do
   the opposite.  Only square matrices can be inverted, so I create
   four 3x3 matrices by omitting a different GMCY color in each one.
   The final coeff[][] matrix is the sum of these four.
 *)
procedure gmcy_coeff(var rec: TRec);
const
  gmcy: array [0..4-1] of array [0..3-1] of single = (
(*    red  green  blue			   *)
    ( 0.11, 0.86, 0.08 ),	(* green   *)
    ( 0.50, 0.29, 0.51 ),	(* magenta *)
    ( 0.11, 0.92, 0.75 ),	(* cyan    *)
    ( 0.81, 0.98, 0.08 )	(* yellow  *)
  );
var
  invert: array [0..3-1] of array [0..6-1] of double;
  num: double;
  ignore, i, j, k, r, g: integer;
begin
  with rec do
  begin
    memset (@coeff, 0, sizeof(coeff));
    for ignore := 0 to 4-1 do
    begin
      for j := 0 to 3-1 do
      begin
        g := IEIFI((j < ignore) , j , j+1);
        for r := 0 to 3-1 do
        begin
          invert[j][r] := gmcy[g][r];	(* 3x3 matrix to invert *)
          invert[j][r+3] := integer(r = j);	(* Identity matrix	*)
        end;
      end;
      for j := 0 to 3-1 do
      begin
        num := invert[j][j];		(* Normalize this row	*)
        for i := 0 to 6-1 do
          invert[j][i] := invert[j][i] / num;
        for k := 0 to 3-1 do		(* Subtract it from the other rows *)
        begin
          if (k=j) then
            continue;
          num := invert[k][j];
          for i := 0 to 6-1 do
            invert[k][i] := invert[k][i] - invert[j][i] * num;
        end;
      end;
      for j := 0 to 3-1 do		(* Add the result to coeff[][] *)
      begin
        g := IEIFI((j < ignore) , j , j+1);
        for r := 0 to 3-1 do
          coeff[r][g] := coeff[r][g] + invert[r][j+3];
      end;
    end;
    for r := 0 to 3-1 do		(* Normalize such that: 		*)
    begin
      num := 0;
      for g := 0 to 4-1 do		(* (1, 1, 1, 1) x coeff = (1, 1, 1) *)
        num := num + coeff[r][g];
      for g := 0 to 4-1 do
        coeff[r][g] := coeff[r][g] / num;
    end;
    use_coeff := 1;
  end;
end;

function strstr(s1, s2: PAnsiChar): PAnsiChar;
begin
  result := strpos(s1, s2);
end;

function memmove(s1, s2: pointer; n: integer): pointer;
begin
  move(pbyte(s2)^, pbyte(s1)^, n);
  result := s1;
end;

(*
   Identify which camera created this file, and set global variables
   accordingly.  Return nonzero if the file cannot be decoded.
 *)
function identify(var rec: TRec): integer;
type
  ttable=record
    fsize: integer;
    make: array [0..12-1] of AnsiChar;
    model: array [0..16-1] of AnsiChar;
  end;
const
  table:array [0..15] of ttable = (
    (    fsize:62464; make:'Kodak';    model:'DC20' ),
    (   fsize:124928; make:'Kodak';    model:'DC20' ),
    (  fsize:2465792; make:'NIKON';    model:'E950' ),
    (  fsize:2940928; make:'NIKON';    model:'E2100' ),
    (  fsize:4771840; make:'NIKON';    model:'E990' ),
    (  fsize:5865472; make:'NIKON';    model:'E4500' ),
    (  fsize:5869568; make:'NIKON';    model:'E4300' ),
    (   fsize:787456; make:'Creative'; model:'PC-CAM 600' ),
    (  fsize:1976352; make:'Casio';    model:'QV-2000UX' ),
    (  fsize:3217760; make:'Casio';    model:'QV-3*00EX' ),
    (  fsize:6218368; make:'Casio';    model:'QV-5700' ),
    (  fsize:7684000; make:'Casio';    model:'QV-4000' ),
    (  fsize:9313536; make:'Casio';    model:'EX-P600' ),
    (  fsize:4841984; make:'Pentax';   model:'Optio S' ),
    (  fsize:6114240; make:'Pentax';   model:'Optio S4' ),
    ( fsize:12582980; make:'Sinar';    model:'' ) );
  corp:array [0..6] of PAnsiChar =
    ( 'Canon', 'NIKON', 'Kodak', 'PENTAX', 'MINOLTA', 'Minolta', 'Konica' );
var
  head: array [0..32-1] of AnsiChar;
  c: PAnsiChar;
  hlen, fsize, i: dword;
label
  nucore, coolpix, fuji_s7000, konica_400z, 
  konica_510z;
begin

  with rec do
  begin
  (*  What format is this file?  Set make[] if we recognize it. *)

    raw_height := 0;
    raw_width := 0;
    flip := 0;
    make[0] := #0;
    model[0] := #0;
    model2[0] := #0;
    memset (@white, 0, sizeof(white));
    camera_red := 0;
    camera_blue := 0;
    timestamp.Time := 0;
    timestamp.Date := 0;
    data_offset := 0;
    curve_offset := 0;
    tiff_data_compression := 0;
    zero_after_ff := 0;

    order := fget2(rec, ifp);
    hlen := fget4(rec, ifp);
    ifp.Position := 0; //fseek (ifp, 0, SEEK_SET);
    ifp.Read(head[0], 32); //fread (head, 1, 32, ifp);
    ifp.Position := ifp.Size;//fseek (ifp, 0, SEEK_END);
    fsize := ftell(ifp);
    c := memmem (head, 32, 'MMMMRawT', 8);
    if (c<>nil) then
    begin
      IEStrCopy(make, 'Phase One');
      data_offset := c - head;
      ifp.Position := data_offset + 8;  //fseek (ifp, data_offset + 8, SEEK_SET);
      ifp.Position := ifp.Position+ fget4(rec, ifp) + 136; //fseek (ifp, fget4(ifp) + 136, SEEK_CUR);
      raw_width := fget4(rec, ifp);
      ifp.Position := ifp.position+ 12; //fseek (ifp, 12, SEEK_CUR);
      raw_height := fget4(rec, ifp);
    end
    else
      if (order = $4949) or (order = $4d4d) then
      begin
        if (memcmp (pointer(int64(DWORD(@head))+6), pbyte(PAnsiChar('HEAPCCDR')), 8)=0) then
        begin
          data_offset := hlen;
          parse_ciff (rec, hlen, fsize - hlen);
        end
        else
        begin
          parse_tiff(rec, 0);
          if (strncmp(make, 'NIKON', 5)=0) and (raw_width = 0) then
            make[0] := #0;
        end;
      end
      else
        if (memcmp (@head, pbyte(PAnsiChar(#0'MRM')), 4)=0) then
        begin
          parse_tiff(rec, 48);
          ifp.Position := 4; //fseek (ifp, 4, SEEK_SET);
          data_offset := fget4(rec, ifp) + 8;
          ifp.Position := 24; //fseek (ifp, 24, SEEK_SET);
          raw_height := fget2(rec, ifp);
          raw_width  := fget2(rec, ifp);
          ifp.Position := 12; //fseek (ifp, 12, SEEK_SET);			(* PRD *)
          ifp.Position := ifp.Position+ fget4(rec, ifp) +  4; //fseek (ifp, fget4(ifp) +  4, SEEK_CUR);	(* TTW *)
          ifp.Position := ifp.Position+ fget4(rec, ifp) + 12; //fseek (ifp, fget4(ifp) + 12, SEEK_CUR);	(* WBG *)
          camera_red  := fget2(rec, ifp);
          camera_red  := camera_red / fget2(rec, ifp);
          camera_blue := fget2(rec, ifp);
          camera_blue := fget2(rec, ifp) / camera_blue;
        end
        else
          if (memcmp (@head, pbyte(PAnsiChar(#$ff#$d8#$ff#$e1)), 4)=0) and (memcmp (pointer(int64(DWORD(@head))+6), pbyte(PAnsiChar('Exif')), 4)=0) then
          begin
            ifp.Position := 4; //fseek (ifp, 4, SEEK_SET);
            ifp.Position := 4 + fget2(rec, ifp); //fseek (ifp, 4 + fget2(ifp), SEEK_SET);
            if (fgetc(ifp) <> $ff) then
              parse_tiff(rec, 12);
          end
          else
            if (memcmp (@head, pbyte(PAnsiChar('BM')), 2)=0) then
            begin
              data_offset := $1000;
              order := $4949;
              ifp.Position := 38; // fseek (ifp, 38, SEEK_SET);
              if (fget4(rec, ifp) = 2834) and (fget4(rec, ifp) = 2834) then
              begin
                IEStrCopy(model, 'BMQ');
                goto nucore;
              end;
            end
            else
              if (memcmp (@head, pbyte(PAnsiChar('BR')), 2)=0) then
              begin
                IEStrCopy(model, 'RAW');
  nucore: 
                IEStrCopy(make, 'Nucore');
                order := $4949;
                ifp.Position := 10; //fseek (ifp, 10, SEEK_SET);
                inc( data_offset , fget4(rec, ifp) );
                fget4(rec, ifp);
                raw_width  := fget4(rec, ifp);
                raw_height := fget4(rec, ifp);
                if (model[0] = 'B') and (raw_width = 2597) then
                begin
                  inc(raw_width);
                  dec(data_offset , $1000);
                end;
              end
              else
                if (memcmp (pointer(int64(DWORD(@head))+25), pbyte(PAnsiChar('ARECOYK')), 7)=0) then
                begin
                  IEStrCopy(make, 'CONTAX');
                  IEStrCopy(model, 'N DIGITAL');
                end
                else
                  if (memcmp (@head, pbyte(PAnsiChar('FUJIFILM')), 8)=0) then
                  begin
                    ifp.Position := 84; //fseek (ifp, 84, SEEK_SET);
                    parse_tiff (rec, fget4(rec, ifp)+12);
                    order := $4d4d;
                    ifp.Position := 100; //fseek (ifp, 100, SEEK_SET);
                    data_offset := fget4(rec, ifp);
                  end
                  else
                    if (memcmp (@head, pbyte(PAnsiChar('DSC-Image')), 9)=0) then
                      parse_rollei(rec)
                    else
                      if (memcmp (@head, pbyte(PAnsiChar('FOVb')), 4)=0) then
                        parse_foveon(rec)
                      else
                        for i := 0 to sizeof(table) div sizeof(ttable) do
                          if (fsize = table[i].fsize) then
                          begin
                            IEStrCopy(@make,  table[i].make );
                            IEStrCopy(@model, table[i].model);
                          end;

    for i := 0 to sizeof(corp) div sizeof(PAnsiChar) -1 do
      if (strstr (make, corp[i])<>nil) then		(* Simplify company names *)
        IEStrCopy(make, corp[i]);
    if (strncmp (make, 'KODAK', 5)=0) then
    begin
      model[16] := #0;
      make[16] := model[16];
    end;
    c := make + IEStrLen(make);		(* Remove trailing spaces *)
    dec(c);
    while (c^ = ' ') do
    begin
      c^ := #0;
      dec(c);
    end;
    c := model + IEStrLen(model);
    dec(c);
    while (c^ = ' ') do
    begin
      c^ := #0;
      dec(c);
    end;
    i := IEStrLen(make);			(* Remove make from model *)
    if (strncmp (model, make, i)=0) then
    begin
      inc(i);
      memmove (@model, model+i, 64-i);
    end;{
    else
      inc(i);}

    if (make[0] = #0) then
    begin
      //fprintf (stderr, "%s: unsupported file format.\n", ifname);
      result := 1;
      exit;
    end;

  (*  File format is OK.  Do we support this camera? *)
  (*  Start with some useful defaults: 		   *)

    load_raw := nil;
    height := raw_height;
    width  := raw_width;
    left_margin := 0;
    top_margin := left_margin;
    colors := 3;
    filters := $94949494;
    black := 0;
    is_cmy := 0;
    is_foveon := 0;
    use_coeff := 0;
    pre_mul[0] := 1;
    pre_mul[1] := 1;
    pre_mul[2] := 1;
    pre_mul[3] := 1;
    xmag := 1;
    ymag := 1;
    rgb_max := $4000;

  (*  We'll try to decode anything from Canon or Nikon. *)

    is_canon := integer(strcmp(make, 'Canon')=0);
    if (is_canon<>0 ) then
    begin
      if (memcmp (pointer(int64(DWORD(@head))+6), pbyte(PAnsiChar('HEAPCCDR')), 8)<>0) then
      begin
        filters := $61616161;
        load_raw := lossless_jpeg_load_raw;
      end
      else
        if (raw_width<>0) then
          load_raw := canon_compressed_load_raw;
    end;
    if (strcmp(make, 'NIKON')=0) then
      if nikon_is_compressed(rec)<>0 then
        load_raw := nikon_compressed_load_raw
      else
        load_raw := nikon_load_raw ;

    if (strcmp(model, 'PowerShot 600')=0) then
    begin
      height := 613;
      width  := 854;
      colors := 4;
      filters := $e1e4e1e4;
      load_raw := canon_600_load_raw;
      pre_mul[0] := 1.137;
      pre_mul[1] := 1.257;
    end
    else
    if (strcmp(model, 'PowerShot A5')=0) or (strcmp(model, 'PowerShot A5 Zoom')=0) then
    begin
      height := 776;
      width  := 960;
      raw_width := 992;
      colors := 4;
      filters := $1e4e1e4e;
      load_raw := canon_a5_load_raw;
      pre_mul[0] := 1.5842;
      pre_mul[1] := 1.2966;
      pre_mul[2] := 1.0419;
    end
    else
    if (strcmp(model, 'PowerShot A50')=0) then
    begin
      height :=  968;
      width  := 1290;
      raw_width := 1320;
      colors := 4;
      filters := $1b4e4b1e;
      load_raw := canon_a5_load_raw;
      pre_mul[0] := 1.750;
      pre_mul[1] := 1.381;
      pre_mul[3] := 1.182;
    end
    else
    if (strcmp(model, 'PowerShot Pro70')=0) then
    begin
      height := 1024;
      width  := 1552;
      colors := 4;
      filters := $1e4b4e1b;
      load_raw := canon_a5_load_raw;
      pre_mul[0] := 1.389;
      pre_mul[1] := 1.343;
      pre_mul[3] := 1.034;
    end
    else
    if (strcmp(model, 'PowerShot Pro90 IS')=0) then
    begin
      width  := 1896;
      colors := 4;
      filters := $b4b4b4b4;
      pre_mul[0] := 1.496;
      pre_mul[1] := 1.509;
      pre_mul[3] := 1.009;
    end
    else
    if (is_canon<>0) and (raw_width = 2144) then
    begin
      height := 1550;
      width  := 2088;
      top_margin  := 8;
      left_margin := 4;
      if (strcmp(model, 'PowerShot G1')=0) then
      begin
        colors := 4;
        filters := $b4b4b4b4;
        pre_mul[0] := 1.446;
        pre_mul[1] := 1.405;
        pre_mul[2] := 1.016;
      end
      else
      begin
        pre_mul[0] := 1.785;
        pre_mul[2] := 1.266;
      end;
    end
    else
    if (is_canon<>0) and (raw_width = 2224) then
    begin
      height := 1448;
      width  := 2176;
      top_margin  := 6;
      left_margin := 48;
      pre_mul[0] := 1.592;
      pre_mul[2] := 1.261;
    end
    else
    if (is_canon<>0) and (raw_width = 2376) then
    begin
      height := 1720;
      width  := 2312;
      top_margin  := 6;
      left_margin := 12;
      (*
  #ifdef CUSTOM
      if (write_fun == write_ppm)		// Pro users may not want my matrix
        canon_rgb_coeff (0.1);
  #endif
      *)
      if (strcmp(model, 'PowerShot G2')=0) or (strcmp(model, 'PowerShot S40')=0) then
      begin
        pre_mul[0] := 1.965;
        pre_mul[2] := 1.208;
      end
      else
      begin				(* G3 and S45 *)
        pre_mul[0] := 1.855;
        pre_mul[2] := 1.339;
      end;
    end
    else
    if (is_canon<>0) and (raw_width = 2672) then
    begin
      height := 1960;
      width  := 2616;
      top_margin  := 6;
      left_margin := 12;
      pre_mul[0] := 1.895;
      pre_mul[2] := 1.403;
    end
    else
    if (is_canon<>0) and (raw_width = 3152) then
    begin
      height := 2056;
      width  := 3088;
      top_margin  := 12;
      left_margin := 64;
      pre_mul[0] := 2.242;
      pre_mul[2] := 1.245;
      if (strcmp(model, 'EOS Kiss Digital')=0) then
      begin
        pre_mul[0] := 1.882;
        pre_mul[2] := 1.094;
      end;
      rgb_max := 16000;
    end
    else
    if (is_canon<>0) and (raw_width = 3160) then
    begin
      height := 2328;
      width  := 3112;
      top_margin  := 12;
      left_margin := 44;
      pre_mul[0] := 1.85;
      pre_mul[2] := 1.53;
    end
    else
    if (is_canon<>0) and (raw_width = 3344) then
    begin
      height := 2472;
      width  := 3288;
      top_margin  := 6;
      left_margin := 4;
      pre_mul[0] := 1.621;
      pre_mul[2] := 1.528;
    end
    else
    if (strcmp(model, 'EOS-1D')=0) then
    begin
      height := 1662;
      width  := 2496;
      data_offset := 288912;
      pre_mul[0] := 1.976;
      pre_mul[2] := 1.282;
    end
    else
    if (strcmp(model, 'EOS-1DS')=0) then
    begin
      height := 2718;
      width  := 4082;
      data_offset := 289168;
      pre_mul[0] := 1.66;
      pre_mul[2] := 1.13;
      rgb_max := 14464;
    end
    else
    if (strcmp(model, 'EOS-1D Mark II')=0) or (strcmp(model, 'EOS 20D')=0) then
    begin
      raw_height := 2360;
      raw_width  := 3596;
      top_margin  := 12;
      left_margin := 74;
      height := raw_height - top_margin;
      width  := raw_width - left_margin;
      filters := $94949494;
      pre_mul[0] := 1.95;
      pre_mul[2] := 1.36;
    end
    else
    if (strcmp(model, 'EOS-1Ds Mark II')=0) then
    begin
      raw_height := 3349;
      raw_width  := 5108;
      top_margin  := 13;
      left_margin := 98;
      height := raw_height - top_margin;
      width  := raw_width - left_margin;
      filters := $94949494;
      pre_mul[0] := 1.609;
      pre_mul[2] := 1.848;
      rgb_max := $3a00;
    end
    else
    if (strcmp(model, 'EOS D2000C')=0) then
    begin
      black := 800;
      pre_mul[2] := 1.25;
    end
    else
    if (strcmp(model, 'D1')=0) then
    begin
      filters := $16161616;
      pre_mul[0] := 0.838;
      pre_mul[2] := 1.095;
    end
    else
    if (strcmp(model, 'D1H')=0) then
    begin
      filters := $16161616;
      pre_mul[0] := 2.301;
      pre_mul[2] := 1.129;
    end
    else
    if (strcmp(model, 'D1X')=0) then
    begin
      width  := 4024;
      filters := $16161616;
      ymag := 2;
      pre_mul[0] := 1.910;
      pre_mul[2] := 1.220;
    end
    else
    if (strcmp(model, 'D100')=0) then
    begin
      if (tiff_data_compression = 34713) and (@load_raw = @nikon_load_raw) then
      begin
        width := width + 3;
        raw_width := width + 3;
      end;
      filters := $61616161;
      pre_mul[0] := 2.374;
      pre_mul[2] := 1.677;
      rgb_max := 15632;
    end
    else
    if (strcmp(model, 'D2H')=0) then
    begin
      width  := 2482;
      left_margin := 6;
      filters := $49494949;
      pre_mul[0] := 2.8;
      pre_mul[2] := 1.2;
    end
    else
    if (strcmp(model, 'D70')=0) then
    begin
      filters := $16161616;
      pre_mul[0] := 2.043;
      pre_mul[2] := 1.625;
    end
    else
    if (strcmp(model, 'E950')=0) then
    begin
      height := 1203;
      width  := 1616;
      filters := $4b4b4b4b;
      colors := 4;
      load_raw := nikon_e950_load_raw;
      nikon_e950_coeff(rec);
      pre_mul[0] := 1.18193;
      pre_mul[2] := 1.16452;
      pre_mul[3] := 1.17250;
    end
    else
    if (strcmp(model, 'E990')=0) then
    begin
      height := 1540;
      width  := 2064;
      colors := 4;
      if (nikon_e990(rec)<>0) then
      begin
        filters := $b4b4b4b4;
        nikon_e950_coeff(rec);
        pre_mul[0] := 1.196;
        pre_mul[1] := 1.246;
        pre_mul[2] := 1.018;
      end
      else
      begin
        IEStrCopy(model, 'E995');
        filters := $e1e1e1e1;
        pre_mul[0] := 1.253;
        pre_mul[1] := 1.178;
        pre_mul[3] := 1.035;
      end;
    end
    else
    if (strcmp(model, 'E2100')=0) then
    begin
      width := 1616;
      if (nikon_e2100(rec)<>0) then
      begin
        height := 1206;
        load_raw := nikon_e2100_load_raw;
        pre_mul[0] := 1.945;
        pre_mul[2] := 1.040;
      end
      else
      begin
        IEStrCopy(model, 'E2500');
        height := 1204;
        filters := $4b4b4b4b;
        goto coolpix;
      end;
    end
    else
    if (strcmp(model, 'E4300')=0) then
    begin
      height := 1710;
      width  := 2288;
      filters := $16161616;
      if (minolta_z2(rec)<>0) then
      begin
        IEStrCopy(make, 'Minolta');
        IEStrCopy(model, 'DiMAGE Z2');
        load_raw := nikon_e2100_load_raw;
      end;
    end
    else
    if (strcmp(model, 'E4500')=0) then
    begin
      height := 1708;
      width  := 2288;
      filters := $b4b4b4b4;
      goto coolpix;
    end
    else
    if (strcmp(model, 'E5000')=0) or (strcmp(model, 'E5700')=0) then
    begin
      filters := $b4b4b4b4;
  coolpix: 
      colors := 4;
      pre_mul[0] := 1.300;
      pre_mul[1] := 1.300;
      pre_mul[3] := 1.148;
    end
    else
    if (strcmp(model, 'E5400')=0) then
    begin
      filters := $16161616;
      pre_mul[0] := 1.700;
      pre_mul[2] := 1.344;
    end
    else
    if (strcmp(model, 'E8700')=0) or (strcmp(model, 'E8800')=0) then
    begin
      filters := $16161616;
      pre_mul[0] := 2.131;
      pre_mul[2] := 1.300;
    end
    else
    if (strcmp(model, 'FinePixS2Pro')=0) then
    begin
      height := 3584;
      width  := 3583;
      filters := $61616161;
      load_raw := fuji_s2_load_raw;
      black := 512;
      pre_mul[0] := 1.424;
      pre_mul[2] := 1.718;
    end
    else
    if (strcmp(model, 'FinePix S5000')=0) then
    begin
      height := 2499;
      width  := 2500;
      filters := $49494949;
      load_raw := fuji_s5000_load_raw;
      pre_mul[0] := 1.639;
      pre_mul[2] := 1.438;
      rgb_max := $f800;
    end
    else
    if (strcmp(model, 'FinePix S7000')=0) then
    begin
      pre_mul[0] := 1.62;
      pre_mul[2] := 1.38;
      goto fuji_s7000;
    end
    else
    if (strcmp(model, 'FinePix E550')=0) then
    begin
      pre_mul[0] := 1.45;
      pre_mul[2] := 1.25;
  fuji_s7000: 
      height := 3587;
      width  := 3588;
      filters := $49494949;
      load_raw := fuji_s7000_load_raw;
      rgb_max := $f800;
    end
    else
    if (strcmp(model, 'FinePix F700')=0) or (strcmp(model, 'FinePix S20Pro')=0) then
    begin
      height := 2523;
      width  := 2524;
      filters := $49494949;
      load_raw := fuji_f700_load_raw;
      pre_mul[0] := 1.639;
      pre_mul[2] := 1.438;
      rgb_max := $3e00;
    end
    else
    if (strcmp(model, 'Digital Camera KD-400Z')=0) then
    begin
      height := 1712;
      width  := 2312;
      raw_width := 2336;
      data_offset := 4034;
      ifp.Position := 2032; //fseek (ifp, 2032, SEEK_SET);
      goto konica_400z;
    end
    else
    if (strcmp(model, 'Digital Camera KD-510Z')=0) then
    begin
      data_offset := 4032;
      ifp.Position := 2032; //fseek (ifp, 2032, SEEK_SET);
      goto konica_510z;
    end
    else
    if (strcasecmp(make, 'MINOLTA')=0) then
    begin
      load_raw := be_low_12_load_raw;
      rgb_max := 15860;
      if (strncmp(model, 'DiMAGE A', 8)=0) then
      begin
        load_raw := packed_12_load_raw;
        rgb_max := IEIFI( model[8] = '1' , 15916, 16380);
      end
      else
      if (strncmp(model, 'DiMAGE G', 8)=0) then
      begin
        if (model[8] = '4') then
        begin
          data_offset := 5056;
          ifp.Position := 2078; //fseek (ifp, 2078, SEEK_SET);
          height := 1716;
          width  := 2304;
        end
        else
        if (model[8] = '5') then
        begin
          data_offset := 4016;
          ifp.Position := 1936; //fseek (ifp, 1936, SEEK_SET);
  konica_510z: 
          height := 1956;
          width  := 2607;
          raw_width := 2624;
        end
        else
        if (model[8] = '6') then
        begin
          data_offset := 4032;
          ifp.Position := 2030; //fseek (ifp, 2030, SEEK_SET);
          height := 2136;
          width  := 2848;
        end;
        filters := $61616161;
  konica_400z: 
        load_raw := be_low_10_load_raw;
        rgb_max := 15856;
        order := $4d4d;
        camera_red   := fget2(rec, ifp);
        camera_blue  := fget2(rec, ifp);
        camera_red   := camera_red / fget2(rec, ifp);
        camera_blue  := camera_blue / fget2(rec, ifp);
      end;
      pre_mul[0] := 1.42;
      pre_mul[2] := 1.25;
    end
    else
    if (strcmp(model, '*ist D')=0) then
    begin
      height := 2024;
      width  := 3040;
      data_offset := $10000;
      load_raw := be_low_12_load_raw;
      pre_mul[0] := 1.76;
      pre_mul[1] := 1.07;
    end
    else
    if (strcmp(model, 'Optio S')=0) then
    begin
      height := 1544;
      width  := 2068;
      raw_width := 3136;
      load_raw := packed_12_load_raw;
      pre_mul[0] := 1.506;
      pre_mul[2] := 1.152;
    end
    else
    if (strcmp(model, 'Optio S4')=0) then
    begin
      height := 1737;
      width  := 2324;
      raw_width := 3520;
      load_raw := packed_12_load_raw;
      pre_mul[0] := 1.308;
      pre_mul[2] := 1.275;
    end
    else
    if (strcmp(make, 'Phase One')=0) then
    begin
      case (raw_height) of
        2060:
          begin
            IEStrCopy(model, 'LightPhase');
            height := 2048;
            width  := 3080;
            top_margin  := 5;
            left_margin := 22;
            pre_mul[0] := 1.331;
            pre_mul[2] := 1.154;
          end;
        2682:
          begin
            IEStrCopy(model, 'H10');
            height := 2672;
            width  := 4012;
            top_margin  := 5;
            left_margin := 26;
          end;
        4128:
          begin
            IEStrCopy(model, 'H20');
            height := 4098;
            width  := 4098;
            top_margin  := 20;
            left_margin := 26;
            pre_mul[0] := 1.963;
            pre_mul[2] := 1.430;
          end;
        5488:
          begin
            IEStrCopy(model, 'H25');
            height := 5458;
            width  := 4098;
            top_margin  := 20;
            left_margin := 26;
            pre_mul[0] := 2.80;
            pre_mul[2] := 1.20;
          end;
      end;
      filters := IEIFI( (top_margin and 1)<>0 , $94949494 , $49494949);
      load_raw := phase_one_load_raw;
      rgb_max := $ffff;
    end
    else
    if (strcmp(model, 'Ixpress')=0) then
    begin
      height := 4084;
      width  := 4080;
      filters := $49494949;
      load_raw := ixpress_load_raw;
      pre_mul[0] := 1.963;
      pre_mul[2] := 1.430;
      rgb_max := $ffff;
    end
    else
    if (strcmp(make, 'Sinar')=0) and (memcmp(@head, pbyte(PAnsiChar('8BPS')), 4)=0) then
    begin
      ifp.Position := 14; //fseek (ifp, 14, SEEK_SET);
      height := fget4(rec, ifp);
      width  := fget4(rec, ifp);
      filters := $61616161;
      data_offset := 68;
      load_raw := be_16_load_raw;
      rgb_max := $ffff;
    end
    else
    if (strcmp(make, 'Leaf')=0) then
    begin
      if (height > width) then
        filters := $16161616;
      load_raw := be_16_load_raw;
      pre_mul[0] := 1.1629;
      pre_mul[2] := 1.3556;
      rgb_max := $ffff;
    end
    else
    if (strcmp(model, 'DIGILUX 2')=0) or (strcmp(model, 'DMC-LC1')=0) then
    begin
      height := 1928;
      width  := 2568;
      data_offset := 1024;
      load_raw := le_high_12_load_raw;
      pre_mul[0] := 1.883;
      pre_mul[2] := 1.367;
    end
    else
    if (strcmp(model, 'E-1')=0) then
    begin
      filters := $61616161;
      load_raw := le_high_12_load_raw;
      pre_mul[0] := 1.57;
      pre_mul[2] := 1.48;
    end
    else
    if (strcmp(model, 'E-10')=0) then
    begin
      load_raw := be_high_12_load_raw;
      pre_mul[0] := 1.43;
      pre_mul[2] := 1.77;
    end
    else
    if (strncmp(model, 'E-20', 4)=0) then
    begin
      load_raw := be_high_12_load_raw;
      black := 640;
      pre_mul[0] := 1.43;
      pre_mul[2] := 1.77;
    end
    else
    if (strcmp(model, 'C5050Z')=0) then
    begin
      filters := $16161616;
      load_raw := olympus_cseries_load_raw;
      pre_mul[0] := 1.533;
      pre_mul[2] := 1.880;
    end
    else
    if (strcmp(model, 'C5060WZ')=0) then
    begin
      load_raw := olympus_cseries_load_raw;
      pre_mul[0] := 2.285;
      pre_mul[2] := 1.023;
    end
    else
    if (strcmp(model, 'C8080WZ')=0) then
    begin
      filters := $16161616;
      load_raw := olympus_cseries_load_raw;
      pre_mul[0] := 2.335;
      pre_mul[2] := 1.323;
    end
    else
    if (strcmp(model, 'N DIGITAL')=0) then
    begin
      height := 2047;
      width  := 3072;
      filters := $61616161;
      data_offset := $1a00;
      load_raw := packed_12_load_raw;
      pre_mul[0] := 1.366;
      pre_mul[2] := 1.251;
    end
    else
    if (strcmp(model, 'DSC-F828')=0) then
    begin
      height := 2460;
      width := 3288;
      raw_width := 3360;
      left_margin := 5;
      load_raw := sony_load_raw;
      sony_rgbe_coeff(rec);
      filters := $b4b4b4b4;
      colors := 4;
      pre_mul[0] := 1.512;
      pre_mul[1] := 1.020;
      pre_mul[2] := 1.405;
    end
    else
    if (strcasecmp(make, 'KODAK')=0) then
    begin
      filters := $61616161;
      if (strcmp(model, 'NC2000F')=0) then
      begin
        width := width - 4;
        left_margin := 1;
        curve_length := 176;
        pre_mul[0] := 1.509;
        pre_mul[2] := 2.686;
      end
      else
      if (strcmp(model, 'EOSDCS3B')=0) then
      begin
        dec( width , 4 );
        left_margin := 2;
        pre_mul[0] := 1.629;
        pre_mul[2] := 2.767;
      end
      else
      if (strcmp(model, 'EOSDCS1')=0) then
      begin
        dec( width , 4 );
        left_margin := 2;
        pre_mul[0] := 1.386;
        pre_mul[2] := 2.405;
      end
      else
      if (strcmp(model, 'DCS315C')=0) then
      begin
        black := 32;
        pre_mul[1] := 1.068;
        pre_mul[2] := 1.036;
      end
      else
      if (strcmp(model, 'DCS330C')=0) then
      begin
        black := 32;
        pre_mul[1] := 1.012;
        pre_mul[2] := 1.804;
      end
      else
      if (strcmp(model, 'DCS420')=0) then
      begin
        dec( width , 4 );
        left_margin := 2;
        pre_mul[0] := 1.327;
        pre_mul[2] := 2.074;
      end
      else
      if (strcmp(model, 'DCS460')=0) then
      begin
        dec( width , 4 );
        left_margin := 2;
        pre_mul[0] := 1.724;
        pre_mul[2] := 2.411;
      end
      else
      if (strcmp(model, 'DCS460A')=0) then
      begin
        dec( width , 4 );
        left_margin := 2;
        colors := 1;
        filters := 0;
      end
      else
      if (strcmp(model, 'DCS520C')=0) then
      begin
        black := 720;
        pre_mul[0] := 1.006;
        pre_mul[2] := 1.858;
      end
      else
      if (strcmp(model, 'DCS560C')=0) then
      begin
        black := 750;
        pre_mul[1] := 1.053;
        pre_mul[2] := 1.703;
      end
      else
      if (strcmp(model, 'DCS620C')=0) then
      begin
        black := 720;
        pre_mul[1] := 1.002;
        pre_mul[2] := 1.818;
      end
      else
      if (strcmp(model, 'DCS62$')=0) then
      begin
        black := 740;
        pre_mul[0] := 1.486;
        pre_mul[2] := 1.280;
        is_cmy := 1;
      end
      else
      if (strcmp(model, 'DCS660C')=0) then
      begin
        black := 855;
        pre_mul[0] := 1.156;
        pre_mul[2] := 1.626;
      end
      else
      if (strcmp(model, 'DCS660M')=0) then
      begin
        black := 855;
        colors := 1;
        filters := 0;
      end
      else
      if (strcmp(model, 'DCS72$')=0) then
      begin
        pre_mul[0] := 1.35;
        pre_mul[2] := 1.18;
        is_cmy := 1;
      end
      else
      if (strcmp(model, 'DCS760C')=0) then
      begin
        pre_mul[0] := 1.06;
        pre_mul[2] := 1.72;
      end
      else
      if (strcmp(model, 'DCS760M')=0) then
      begin
        colors := 1;
        filters := 0;
      end
      else
      if (strcmp(model, 'ProBack')=0) then
      begin
        pre_mul[0] := 1.06;
        pre_mul[2] := 1.385;
      end
      else
      if (strncmp(model2, 'PB645C', 6)=0) then
      begin
        pre_mul[0] := 1.0497;
        pre_mul[2] := 1.3306;
      end
      else
      if (strncmp(model2, 'PB645H', 6)=0) then
      begin
        pre_mul[0] := 1.2010;
        pre_mul[2] := 1.5061;
      end
      else
      if (strncmp(model2, 'PB645M', 6)=0) then
      begin
        pre_mul[0] := 1.01755;
        pre_mul[2] := 1.5424;
      end
      else
      if (strcasecmp(model, 'DCS Pro 14n')=0) then
      begin
        pre_mul[1] := 1.0323;
        pre_mul[2] := 1.258;
      end
      else
      if (strcasecmp(model, 'DCS Pro 14nx')=0) then
      begin
        pre_mul[0] := 1.336;
        pre_mul[2] := 1.3155;
      end
      else
      if (strcasecmp(model, 'DCS Pro SLR/c')=0) then
      begin
        pre_mul[0] := 1.425;
        pre_mul[2] := 1.293;
      end
      else
      if (strcasecmp(model, 'DCS Pro SLR/n')=0) then
      begin
        pre_mul[0] := 1.324;
        pre_mul[2] := 1.483;
      end;
      case (tiff_data_compression) of
        0, 				(* No compression *)
        1: 
          load_raw := kodak_easy_load_raw;
        7: 				(* Lossless JPEG *)
          load_raw := lossless_jpeg_load_raw;
        32867: 
          ;
        65000: 			(* Kodak DCR compression *)
          begin
            if (kodak_data_compression = 32803) then
              load_raw := kodak_compressed_load_raw
            else
            begin
              load_raw := kodak_yuv_load_raw;
              height := (height+1) and -2;
              width  := (width +1) and -2;
              filters := 0;
            end;
          end;
        else
          begin
            //fprintf (stderr, '%s: %s %s uses unsupported compression method %d.\n', ifname, make, model, tiff_data_compression);
            result := 1;
            exit;
          end;
      end;
      if (strcmp(model, 'DC20')=0) then
      begin
        height := 242;
        if (fsize < 100000) then
        begin
          width := 249;
          raw_width := 256;
        end
        else
        begin
          width := 501;
          raw_width := 512;
        end;
        data_offset := raw_width + 1;
        colors := 4;
        filters := $8d8d8d8d;
        kodak_dc20_coeff (rec, 0.5);
        pre_mul[1] := 1.179;
        pre_mul[2] := 1.209;
        pre_mul[3] := 1.036;
        load_raw := kodak_easy_load_raw;
      end
      else
      if (strstr(model, 'DC25')<>nil) then
      begin
        IEStrCopy(model, 'DC25');
        height := 242;
        if (fsize < 100000) then
        begin
          width := 249;
          raw_width := 256;
          data_offset := 15681;
        end
        else
        begin
          width := 501;
          raw_width := 512;
          data_offset := 15937;
        end;
        colors := 4;
        filters := $b4b4b4b4;
        load_raw := kodak_easy_load_raw;
      end
      else
      if (strcmp(model, 'Digital Camera 40')=0) then
      begin
        IEStrCopy(model, 'DC40');
        height := 512;
        width := 768;
        data_offset := 1152;
        load_raw := kodak_radc_load_raw;
      end
      else
      if (strstr(model, 'DC50')<>nil) then
      begin
        IEStrCopy(model, 'DC50');
        height := 512;
        width := 768;
        data_offset := 19712;
        load_raw := kodak_radc_load_raw;
      end
      else
      if (strstr(model, 'DC120')<>nil) then
      begin
        IEStrCopy(model, 'DC120');
        height := 976;
        width := 848;

        (*
        if (tiff_data_compression = 7) then
          load_raw := kodak_jpeg_load_raw
        else
          load_raw := kodak_dc120_load_raw;
          *)
        if (tiff_data_compression <> 7) then
          load_raw := kodak_dc120_load_raw;
          
      end;
    end
    else
    if (strcmp(make, 'Rollei')=0) then
    begin
      case (raw_width) of
        1316:
          begin
            height := 1030;
            width  := 1300;
            top_margin  := 1;
            left_margin := 6;
          end;
        2568:
          begin
            height := 1960;
            width  := 2560;
            top_margin  := 2;
            left_margin := 8;
          end;
      end;
      filters := $16161616;
      load_raw := rollei_load_raw;
      pre_mul[0] := 1.8;
      pre_mul[2] := 1.3;
    end
    else
    if (strcmp(make, 'SIGMA')=0) then
    begin
      case (raw_height) of
        763:
          begin
            height :=  756;
            top_margin :=  2;
          end;
        1531:
          begin
            height := 1514;
            top_margin :=  7;
          end;
      end;
      case (raw_width) of
        1152:
          begin
            width := 1136;
            left_margin :=  8;
          end;
        2304:
          begin
            width := 2271;
            left_margin := 17;
          end;
      end;
      if (height*2 < width) then
        ymag := 2;
      filters := 0;
      load_raw := foveon_load_raw;
      is_foveon := 1;
      pre_mul[0] := 1.179;
      pre_mul[2] := 0.713;
      if (strcmp(model, 'SD10')=0) then
      begin
        pre_mul[0] := pre_mul[0] * 2.07;
        pre_mul[2] := pre_mul[2] * 2.30;
      end;
      foveon_coeff(rec);
      rgb_max := 5600;
    end
    else
    if (strcmp(model, 'PC-CAM 600')=0) then
    begin
      height := 768;
      width := 1024;
      data_offset := width;
      filters := $49494949;
      load_raw := eight_bit_load_raw;
      pre_mul[0] := 1.14;
      pre_mul[2] := 2.73;
    end
    else
    if (strcmp(model, 'QV-2000UX')=0) then
    begin
      height := 1208;
      width  := 1632;
      data_offset := width * 2;
      load_raw := eight_bit_load_raw;
    end
    else
    if (strcmp(model, 'QV-3*00EX')=0) then
    begin
      height := 1546;
      width  := 2070;
      raw_width := 2080;
      load_raw := eight_bit_load_raw;
    end
    else
    if (strcmp(model, 'QV-4000')=0) then
    begin
      height := 1700;
      width  := 2260;
      load_raw := be_high_12_load_raw;
    end
    else
    if (strcmp(model, 'QV-5700')=0) then
    begin
      height := 1924;
      width  := 2576;
      load_raw := casio_qv5700_load_raw;
    end
    else
    if (strcmp(model, 'EX-P600')=0) then
    begin
      height := 2142;
      width  := 2844;
      raw_width := 4288;
      load_raw := packed_12_load_raw;
      pre_mul[0] := 2.356;
      pre_mul[1] := 1.069;
    end
    else
    if (strcmp(make, 'Nucore')=0) then
    begin
      filters := $61616161;
      load_raw := nucore_load_raw;
    end;
    if (@load_raw=nil) or (height=0) then
    begin
      //fprintf (stderr, '%s: %s %s is not yet supported.\n', ifname, make, model);
      result := 1;
      exit;
    end;
    if (raw_height=0) then
      raw_height := height;
    if (raw_width=0 ) then
      raw_width  := width;
    if (colors = 4) and (use_coeff=0) then
      gmcy_coeff(rec);
    if (use_coeff<>0) then		 (* Apply user-selected color balance *)
      for i := 0 to colors-1 do
      begin
        coeff[0][i] := coeff[0][i] * red_scale;
        coeff[2][i] := coeff[2][i] * blue_scale;
      end;
    if (four_color_rgb<>0) and (filters<>0) and (colors = 3) then
    begin
      i := 0;
      while ( i < 32) do
      begin
        if ((filters shr i and 15) = 9) then
          filters := filters or (2 shl i);
        if ((filters shr i and 15) = 6) then
          filters := filters or (8 shl i);
        inc(i, 4);
      end;
      inc(colors);
      pre_mul[3] := pre_mul[1];
      if (use_coeff<>0) then
        for i := 0 to 3-1 do
        begin
          coeff[i][1] := coeff[i][1] / 2;
          coeff[i][3] := coeff[i][1];
        end;
    end;
    ifp.Position := data_offset; //fseek (ifp, data_offset, SEEK_SET);
    result := 0;
  end;
end;

(*
   Convert the entire image to RGB colorspace and build a histogram.
 *)
procedure convert_to_rgb(var rec: TRec);
var
  row, col, r, g, c: integer;
  img: pwordarray;
  rgb: array [0..3-1] of double;
  mag: double;
  v1: double;
label
  norgb;
begin

  with rec do
  begin
    c := 0;

    if (document_mode<>0) then
      colors := 1;
    memset (@histogram, 0, sizeof(histogram));
    for row := trim to height-trim-1 do
      for col := trim to width-trim-1 do
      begin
        img := pwordarray(@image[row*width+col]);
        if (document_mode<>0) then
          //c := FC(row, col);
          c := (filters shr (((row shl 1 and 14) + (col and 1)) shl 1) and 3);
        if (colors = 4) and (use_coeff=0)	then (* Recombine the greens *)
          img[1] := (img[1] + img[3]) shr 1;
        if (colors = 1) then			(* RGB from grayscale *)
          for r := 0 to 3-1 do
            rgb[r] := img[c]
        else
        if (use_coeff<>0) then		(* RGB from GMCY or Foveon *)
        begin
          for r := 0 to 3-1 do
          begin
            rgb[r] := 0;
            for g := 0 to colors-1 do
              rgb[r] := rgb[r] + (img[g] * coeff[r][g]);
          end;
        end
        else
        if (is_cmy<>0) then		(* RGB from CMY *)
        begin
          rgb[0] := img[0] + img[1] - img[2];
          rgb[1] := img[1] + img[2] - img[0];
          rgb[2] := img[2] + img[0] - img[1];
        end
        else				(* RGB from RGB (easy) *)
          goto norgb;
        for r := 0 to 3-1 do
        begin
          if (rgb[r] < 0) then
            rgb[r] := 0;
          if (rgb[r] > rgb_max) then
            rgb[r] := rgb_max;
          img[r] := trunc(rgb[r]);
        end;
  norgb: 
        (*
        if (@write_fun = @write_bitmap) then
        begin
        *)
          mag := 0;
          for r := 0 to 3-1 do
          begin
            v1 := img[r];
            mag :=  mag + int(v1*v1);
          end;
          mag := sqrt(mag)/2;
          if (mag > $ffff) then
            mag := $ffff;
          img[3] := trunc(mag);
          inc( histogram[img[3] shr 3] );
        //end;
      end;
  end;
end;

procedure fujirot16(bitmap: TIEBitmap);
type
  value=word;
  tword3=array [0..2] of value;
  pword3=^tword3;
  tword3array=array [0..maxint div 16] of tword3;
  pword3array=^tword3array;
var
  newbitmap: TIEBitmap;
  xin, mid, pix, xout: pword3array; // word (*in)[3], (*mid)[3], (*pix)[3], (*out)[3];
  pix1, pix2, pix3, pix4: pword3;
  iwide, ihigh, owide, ohigh: integer;
  i, irow, icol, orow, ocol: word;
  q, w: integer;
begin
  newbitmap := TIEBitmap.Create;

  iwide := bitmap.width;
  ihigh := bitmap.Height;

  q := 0;
  w := 0;

  xin := allocmem(iwide * sizeof(value)*3);
  copymemory(xin, bitmap.scanline[q], iwide*sizeof(value)*3); inc(q);

  for i := 0 to iwide-1 do
    if (xin[i][0]<>0) or (xin[i][1]<>0) or (xin[i][2]<>0) then
      break;
  ohigh := (iwide - i) * 2 - 4;
  i := iwide;
  while (true) do
  begin
    dec(i);
    if i=0 then
      break;
    if (xin[i][0]<>0) or (xin[i][1]<>0) or (xin[i][2]<>0) then
      break;
  end;
  owide := i;
  mid := allocmem (ohigh * owide * sizeof(value)*3+10);
  for irow := 0 to ihigh-1 do
  begin
    for icol := 0 to iwide-1 do
    begin
      orow :=  irow + icol - word(owide) + 5; //orow := imax(0, orow);
      ocol := (icol - irow + word(owide) - 1) div 2; //ocol := imax(0, ocol);
      if (orow < ohigh) and (ocol < owide) then
      begin
        pix1 := @mid[orow*owide+ocol];
        pix2 := @xin[icol];
        pix1^ := pix2^;
	      //for i := 0 to 3-1 do
	        //mid[orow*owide+ocol][i] := ntohs(xin[icol][i]);
      end;
    end;
    if q<bitmap.height then
      copymemory(xin, bitmap.scanline[q], iwide*sizeof(value)*3);
    inc(q);
  end;
  freemem(xin);
  xout := allocmem(2*owide * sizeof(value)*3);
  newbitmap.allocate(owide*2, ohigh, ie48RGB);
  for orow := 0+1 to ohigh-1 do
  begin
    for ocol := 0+1 to owide*2-1 do
    begin
      pix := pointer( int64(DWORD(mid)) + (orow*owide + ocol div 2)*sizeof(value)*3 );
      if ((orow+ocol) and 1)<>0 then
      begin
        if (orow-1 < ohigh-2) and (ocol-1 < owide*2-2) then
	        for i := 0 to 3-1 do
          begin
	          //xout[ocol][i] := htons ( ( pix[-owide][i] + pix[0-(orow&1)][i] + pix[ owide][i] + pix[1-(orow&1)][i] ) >> 2);
            pix1 := pword3(pix); inc(pix1, -owide);
            pix2 := pword3(pix); inc(pix2, 0-(orow and 1));
            pix3 := pword3(pix); inc(pix3, owide);
            pix4 := pword3(pix); inc(pix4, 1-(orow and 1));
            xout[ocol][i] := ( pix1[i] + pix2[i] + pix3[i] + pix4[i] ) shr 2;
          end;
      end
      else
        xout[ocol] := pix[0];
	      //for i := 0 to 3-1 do
	        //xout[ocol][i] := htons(pix[0][i]);
    end;
    copymemory(newbitmap.scanline[w], xout, 2*owide* 3*sizeof(value)); inc(w);

  end;
  freemem(mid);
  freemem(xout);

  bitmap.Assign(newbitmap);
  FreeAndNil(newbitmap);
end;


procedure fujirot8(bitmap: TIEBitmap);
type
  value=byte;
  tbyte3=array [0..2] of value;
  pbyte3=^tbyte3;
  tbyte3array=array [0..maxint div 16] of tbyte3;
  pbyte3array=^tbyte3array;
var
  newbitmap: TIEBitmap;
  xin, mid, pix, xout: pbyte3array; // byte (*in)[3], (*mid)[3], (*pix)[3], (*out)[3];
  pix1, pix2, pix3, pix4: pbyte3;
  iwide, ihigh, owide, ohigh: integer;
  i, irow, icol, orow, ocol: word;
  q, w: integer;
begin
  newbitmap := TIEBitmap.Create;

  iwide := bitmap.width;
  ihigh := bitmap.Height;

  q := 0;
  w := 0;

  xin := allocmem(iwide * sizeof(value)*3);
  copymemory(xin, bitmap.scanline[q], iwide*sizeof(value)*3); inc(q);

  for i := 0 to iwide-1 do
    if (xin[i][0]<>0) or (xin[i][1]<>0) or (xin[i][2]<>0) then
      break;
  ohigh := (iwide - i) * 2 - 4;
  i := iwide;
  while (true) do
  begin
    dec(i);
    if i=0 then
      break;
    if (xin[i][0]<>0) or (xin[i][1]<>0) or (xin[i][2]<>0) then
      break;
  end;
  owide := i;
  mid := allocmem (ohigh * owide * sizeof(value)*3+10);
  for irow := 0 to ihigh-1 do
  begin
    for icol := 0 to iwide-1 do
    begin
      orow :=  irow + icol - word(owide) + 5; //orow := imax(0, orow);
      ocol := (icol - irow + word(owide) - 1) div 2; //ocol := imax(0, ocol);
      if (orow < ohigh) and (ocol < owide) then
      begin
        pix1 := @mid[orow*owide+ocol];
        pix2 := @xin[icol];
        pix1^ := pix2^;
	      //for i := 0 to 3-1 do
	        //mid[orow*owide+ocol][i] := ntohs(xin[icol][i]);
      end;
    end;
    if q<bitmap.height then
      copymemory(xin, bitmap.scanline[q], iwide*sizeof(value)*3);
    inc(q);
  end;
  freemem(xin);
  xout := allocmem(2*owide * sizeof(value)*3);
  newbitmap.allocate(owide*2, ohigh, ie24RGB);
  for orow := 0+1 to ohigh-1 do
  begin
    for ocol := 0+1 to owide*2-1 do
    begin
      pix := pointer( int64(DWORD(mid)) + (orow*owide + ocol div 2)*sizeof(value)*3 );
      if ((orow+ocol) and 1)<>0 then
      begin
        if (orow-1 < ohigh-2) and (ocol-1 < owide*2-2) then
	        for i := 0 to 3-1 do
          begin
	          //xout[ocol][i] := htons ( ( pix[-owide][i] + pix[0-(orow&1)][i] + pix[ owide][i] + pix[1-(orow&1)][i] ) >> 2);
            pix1 := pbyte3(pix); inc(pix1, -owide);
            pix2 := pbyte3(pix); inc(pix2, 0-(orow and 1));
            pix3 := pbyte3(pix); inc(pix3, owide);
            pix4 := pbyte3(pix); inc(pix4, 1-(orow and 1));
            xout[ocol][i] := ( pix1[i] + pix2[i] + pix3[i] + pix4[i] ) shr 2;
          end;
      end
      else
        xout[ocol] := pix[0];
	      //for i := 0 to 3-1 do
	        //xout[ocol][i] := htons(pix[0][i]);
    end;
    copymemory(newbitmap.scanline[w], xout, 2*owide* 3*sizeof(value)); inc(w);

  end;
  freemem(mid);
  freemem(xout);

  bitmap.Assign(newbitmap);
  FreeAndNil(newbitmap);
end;

procedure IEReadCameraRAWStream(InputStream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
type
  tbyte3array = array [0..maxint div 16] of array [0..2] of byte;
  pbyte3array = ^tbyte3array;
  tword3array = array [0..maxint div 16] of array [0..2] of word;
  pword3array = ^tword3array;
var
  row, col, i, c, val, v, total: integer;
  max, mul, dd: double;
  scale: pdoublearray; //array [0..$10000-1] of double;
  rgb: pwordarray;
  ppm: pbyte3array; //'uchar (*ppm)[3]';
  ppmw: pword3array;
  q: integer;
  half_size: integer;
  rec: TRec;
  rgb48, pw: PRGB48;
  Stream: TIEBufferedReadStream;
begin

  Stream := nil;

  if IOParams.RAW_GetExifThumbnail then
  begin
    // try to load the EXIF thumbnail
    if IOParams.EXIF_Bitmap<>nil then
      IOParams.EXIF_Bitmap.FreeImage(true);
    IOParams.RAW_GetExifThumbnail := false;
    IEReadCameraRAWStream(InputStream, Bitmap, IOParams, Progress, true);
    IOParams.RAW_GetExifThumbnail := true;
    if assigned(IOParams.EXIF_Bitmap) and not IOParams.EXIF_Bitmap.IsEmpty then
    begin
      Bitmap.Assign( IOParams.EXIF_Bitmap );
      exit;
    end;
    InputStream.Position := 0;
  end;

  Stream := TIEBufferedReadStream.Create(InputStream, 65536, IEGlobalSettings().UseRelativeStreams);

  try

    // try to load EXIF
    Stream.Position := 0;
    q := IESearchEXIFInfo(Stream);
    if q >= 0 then
    begin
      Stream.Position := q;
      if IELoadEXIFFromTIFF(Stream, IOParams, true) then
      begin
        // use dpi of the Exif
        if IOParams.EXIF_ResolutionUnit = 3 then
          dd := CM_per_Inch
        else
          dd := 1;
        IOParams.DpiX := trunc(IOParams.EXIF_XResolution * dd);
        IOParams.DpiY := trunc(IOParams.EXIF_YResolution * dd);
      end;
    end
    else
    begin
      // is this a CRW? (with CIFF instead of EXIF?)
      Stream.Position := 0;
      IECRWGetCIFFAsExif(Stream, IOParams);
    end;
    Stream.Position := 0;

    with rec do
    begin

      InitRec(rec);

      gamma_val := IOParams.RAW_Gamma;
      bright := IOParams.RAW_Bright;
      red_scale := IOParams.RAW_RedScale;
      blue_scale := IOParams.RAW_BlueScale;
      four_color_rgb := integer(IOParams.RAW_FourColorRGB);
      document_mode := 0;
      quick_interpolate := integer(IOParams.RAW_QuickInterpolate);
      use_auto_wb := integer(IOParams.RAW_UseAutoWB);
      use_camera_wb := integer(IOParams.RAW_UseCameraWB);
      use_secondary := 0;

      if IOParams.RAW_AutoAdjustColors and assigned(IOParams.EXIF_Bitmap) and not IOParams.EXIF_Bitmap.IsEmpty then
        bright := 0.5;

      needrot45 := false;
      xprogress := progress;

      Stream.Position := 0;
      ifp := Stream;

      if identify(rec)<>0 then
      begin
        Progress.Aborting^ := true;
        exit;
      end;

      getmem(scale, sizeof(double)*$10000);

      try

      IOParams.RAW_Camera := AnsiString(make) + ' ' + AnsiString(model) + ' ' + AnsiString(model2);
      IOParams.DpiX := IEGlobalSettings().DefaultDPIX;
      IOParams.DpiY := IEGlobalSettings().DefaultDPIY;
      IOParams.FreeColorMap;
      IOParams.Width :=  width-2;
      IOParams.Height := height-2;
      IOParams.OriginalWidth  := IOParams.Width;
      IOParams.OriginalHeight := IOParams.Height;

      if not Preview then
      begin

        half_size := integer(IOParams.RAW_HalfSize);
        shrink := integer( boolean(half_size) and boolean(filters) );

        iheight := (height + shrink) shr shrink;
        iwidth  := (width  + shrink) shr shrink;

        image := allocmem(iheight * iwidth * 4*sizeof(word));

        load_raw(@rec);

        height := iheight;
        width  := iwidth;

        quick_interpolate := 1;

        if (is_foveon<>0) then
          foveon_interpolate(rec)
        else
          scale_colors(rec);

        with xprogress do
          if assigned(fOnProgress) then
            fOnProgress(Sender, 70);

        if (shrink<>0) then
          filters := 0;
        trim := 0;
        if (filters<>0) and (document_mode=0) then
        begin
          trim := 1;
          vng_interpolate(rec);
        end;

        convert_to_rgb(rec);

        with xprogress do
          if assigned(fOnProgress) then
            fOnProgress(Sender, 85);

        IOParams.ImageCount := 1;

        if IOParams.IsNativePixelFormat then
        begin
          // 16 bit per pixel (ie48RGB)

          IOParams.BitsPerSample := 16;
          IOParams.SamplesPerPixel := 3;
          i := trunc( width * height * IEIFD(strcmp(make, 'FUJIFILM')<>0 , 0.01 , 0.005) );
          total := 0;
          val := $2000;
          while true do
          begin
            dec(val);
            if val = 0 then
              break;
            inc( total , histogram[val] );
            if (total > i) then
              break;
          end;
          max := val shl 4;

          if max=0 then max := 1;
          mul := bright * 442 / max;
          scale[0] := 0;
          for i := 1 to $10000-1 do
            scale[i] := mul * Power(i*2/max, gamma_val-1) /255*65535;

          bitmap.Allocate(xmag*(width-trim*2) , ymag*(height-trim*2), ie48RGB);

          ppmw := allocmem((width-trim*2) * 6);
          merror (ppmw, 'write_ppm16()');

          q := 0;
          for row := trim to height-trim-1 do
          begin
            for col := trim to width-trim-1 do
            begin
              rgb := pwordarray(@image[row*width+col]);
              for c := 0 to 3-1 do
              begin
                v := trunc( rgb[c] * scale[rgb[3]] );
                if (v > $ffff) then
                  v := $ffff;
                //ppmw[col-trim][c] := htons(v);
                ppmw[col-trim][c] := v;
              end;
            end;
            rgb48 := bitmap.scanline[q];
            pw := PRGB48(ppmw);
            for col := 0 to bitmap.width-1 do
            begin
              rgb48^ := pw^;
              inc(rgb48);
              inc(pw);
            end;
            inc(q);
          end;
          freemem(ppmw);
          freemem(image);

          with xprogress do
            if assigned(fOnProgress) then
              fOnProgress(Sender, 90);

          if needrot45 then
            fujirot16(bitmap);

        end
        else
        begin
          // 8 bit per pixel (ie24RGB)

          IOParams.BitsPerSample := 8;
          IOParams.SamplesPerPixel := 3;
          i := trunc( width * height * IEIFD(strcmp(make, 'FUJIFILM')<>0 , 0.01 , 0.005) );
          total := 0;
          val := $2000;
          while true do
          begin
            dec(val);
            if val = 0 then
              break;
            inc( total , histogram[val] );
            if (total > i) then
              break;
          end;
          max := val shl 4;

          bitmap.Allocate(xmag*(width-trim*2), ymag*(height-trim*2), ie24RGB);

          ppm := allocmem ((width-trim*2) * 3*xmag +10);
          merror (ppm, 'write_ppm()');
          if max = 0 then
            max := 1;
          mul := bright * 442 / max;
          scale[0] := 0;
          for i := 1 to $10000-1 do
            scale[i] := mul * Power(i*2/max, gamma_val-1);

          q := 0;
          for row := trim to height-trim-1 do
          begin
            for col := trim to width-trim-1 do
            begin
              rgb := pwordarray(@image[row*width+col]);
              for c := 0 to 3-1 do
              begin
                val := trunc( rgb[c] * scale[rgb[3]] );
                if (val > 255) then
                  val := 255;
                for i := 0 to xmag-1 do
                  ppm[xmag*(col-trim)+i][c] := val;
              end;
            end;
            _CopyBGR_RGB(bitmap.Scanline[q], PRGB(@pbytearray(ppm)[0]), (width-trim*2) * xmag);
            inc(q);
          end;
          freemem(ppm);
          freemem(image);

          with xprogress do
            if assigned(fOnProgress) then
              fOnProgress(Sender, 90);

          if needrot45 then
            fujirot8(bitmap);

        end;

      end;  // if not preview

      if IOParams.RAW_AutoAdjustColors and assigned(IOParams.EXIF_Bitmap) and not IOParams.EXIF_Bitmap.IsEmpty then
        IEAdjustColors(IOParams.EXIF_Bitmap, Bitmap);

      with xprogress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, 100);

      finally
        freemem(scale);
      end;

    end;

  finally
    if Stream<>nil then
      FreeAndNil(Stream);
  end;

end;

function IERAWTryStream(Stream: TStream): boolean;
var
  rec: TRec;
  lp: int64;
begin
  lp := Stream.Position;
  InitRec(rec);
  rec.ifp := Stream;
  result := identify(rec) = 0;
  Stream.Position := lp;
end;

{$endif} // IEUSEDLLRAWLIB


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////




///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// dll dcraw (inside ielib.dll or ievision.dll) library wrappers


{$ifdef IEUSEDLLRAWLIB}

type
  TIECallbackRecord = record
    OnProgress:       TIEProgressEvent;
    OnProgressSender: TObject;
    Stream:           TStream;
    params:           TIOParamsVals;
    aborting:         pboolean;
  end;
  PIECallBackRecord = ^TIECallBackRecord;

function ProgressCallback(ptr: pointer; per: integer): bool32; stdcall;
begin
  with PIECallBackRecord(ptr)^ do
  begin
    if assigned(OnProgress) then
      OnProgress( OnProgressSender, per );
    result := not aborting^;
  end;
end;

procedure IEReadCameraRAWStream(InputStream: TStream; Bitmap: TIEBitmap; var IOParams: TIOParamsVals; var Progress: TProgressRec; Preview: boolean);
var
  lextra: AnsiString;
  bufStream: TIEBufferedReadStream;
  dcraw: TIELibDCRAWDecoder;
  params: TIEVisionVectorString;
  i, j: integer;
  tempio: TImageEnIO;
  rec: TIECallbackRecord;
  width, height: integer;
  pixelFormat: TIELibDCRAWPixelFormat;
begin
  if IOParams.GetThumbnail then
  begin
    // try to load the (EXIF?) thumbnail
    if IOParams.EXIF_Bitmap <> nil then
      IOParams.EXIF_Bitmap.FreeImage(true);
    IOParams.GetThumbnail := false;
    IEReadCameraRAWStream(InputStream, Bitmap, IOParams, Progress, true);
    IOParams.GetThumbnail := true;
    if assigned(IOParams.EXIF_Bitmap) and not IOParams.EXIF_Bitmap.IsEmpty then
    begin
      // success, exit
      Bitmap.Assign( IOParams.EXIF_Bitmap );
      exit;
    end;
    // try to load with "-e" option, if raw
    lextra := IOParams.RAW_ExtraParams;
    IOParams.RAW_ExtraParams := '-e';
    IOParams.GetThumbnail := false;
    IEReadCameraRAWStream(InputStream, Bitmap, IOParams, Progress, false);
    IOParams.GetThumbnail := true;
    IOParams.RAW_ExtraParams := lextra;
    if not Progress.Aborting^ then
      exit; // success, exit
    // continue loading the full image
    InputStream.Position := 0;
  end;

  bufStream := TIEBufferedReadStream.Create(InputStream, 8 * 1024 * 1024);

  rec.OnProgress       := Progress.fOnProgress;
  rec.OnProgressSender := Progress.Sender;
  rec.Stream           := bufStream;
  rec.params           := IOParams;
  rec.aborting         := Progress.Aborting;
  Progress.Aborting^ := false;

  try

    // ImageEn will search EXIF info inside the file
    bufStream.Position := 0;
    i := IESearchEXIFInfo(bufStream);
    if i >= 0 then
    begin
      bufStream.Position := i;
      if IELoadEXIFFromTIFF(bufStream, IOParams, true) then
      begin
        // use dpi of the Exif
        IOParams.DpiX := trunc(IOParams.EXIF_XResolution * IEIFD(IOParams.EXIF_ResolutionUnit = 3, CM_per_Inch, 1.0));
        IOParams.DpiY := trunc(IOParams.EXIF_YResolution * IEIFD(IOParams.EXIF_ResolutionUnit = 3, CM_per_Inch, 1.0));
      end;
    end
    else
    begin
      // is this a CRW? (with CIFF instead of EXIF?)
      bufStream.Position := 0;
      IECRWGetCIFFAsExif(bufStream, IOParams);
    end;

    // look for IPTC
    bufStream.Position := 0;
    tempio := TImageEnIO.Create(nil);
    try
      tempio.ParamsFromStreamFormat(bufStream, ioTIFF);
      IOParams.IPTC_Info.Assign( tempio.Params.IPTC_Info );
      if not IOParams.EXIF_HasEXIFData and tempio.Params.EXIF_HasEXIFData then
        IECopyEXIF(tempio.Params, IOParams, true);
    finally
      tempio.Free();
    end;

    if not IELibAvailable() then
      raise EIERAWException.Create(IERS_IEVISIONNOTFOUND);

    // reset position
    bufStream.Position := 0;

    // setup dcraw parameters
    params := IELib.createVectorString();
    params.push_back('par.000');
    if Preview then
      params.push_back('-i')
    else
    begin
      if IOParams.IsNativePixelFormat then
        params.push_back('-4');
      if IOParams.RAW_HalfSize then
        params.push_back('-h');
      if IOParams.RAW_QuickInterpolate then
      begin
        params.push_back('-q');
        params.push_back('0');
      end;
      if IOParams.RAW_UseAutoWB then
        params.push_back('-a');
      if IOParams.RAW_UseCameraWB then
        params.push_back('-w');
      if IOParams.RAW_Bright <> 1.0 then
      begin
        params.push_back('-b');
        params.push_back(PAnsiChar(AnsiString(IEFloatToStrA(IOParams.RAW_Bright))));
      end;
      if IOParams.RAW_ExtraParams <> '' then
      begin
        j := 1;
        for i := 1 to length(IOParams.RAW_ExtraParams) + 1 do // +1 to allow to consider string ending as space (see below IF)
          if (i = length(IOParams.RAW_ExtraParams) + 1) or (IOParams.RAW_ExtraParams[i] = ' ') then
          begin
            params.push_back( PAnsiChar(AnsiString(IECopy(IOParams.RAW_ExtraParams, j, i - j))) );
            j := i + 1;
          end;
      end;
    end;
    params.push_back('par.111');

    // decode
    dcraw := IELib.createDCRAWDecoder(IELib.createCustomStream(TIEVCLStreamProvider.Create(bufStream)), params, @ProgressCallback, @rec);

    // retrieve parameters
    width  := dcraw.getIntInfo(ievWIDTH);
    height := dcraw.getIntInfo(ievHEIGHT);
    pixelFormat := TIELibDCRAWPixelFormat(dcraw.getIntInfo(ievPIXELFORMAT));
    IOParams.Width  := width;
    IOParams.Height := height;
    IOParams.OriginalWidth  := dcraw.getIntInfo(ievORIGINAL_WIDTH);
    IOParams.OriginalHeight := dcraw.getIntInfo(ievORIGINAL_HEIGHT);
    case pixelFormat of
      ievRGB24: begin IOParams.BitsPerSample :=  8; IOParams.SamplesPerPixel := 3; end;
      ievRGB48: begin IOParams.BitsPerSample := 16; IOParams.SamplesPerPixel := 3; end;
    end;
    IOParams.FreeColorMap();

    if not Preview and dcraw.imageLoaded() then
    begin
      // copy dcraw image to Bitmap
      case pixelFormat of
        ievRGB24:
          begin
            Bitmap.Allocate(width, height, ie24RGB);
            for i := 0 to height - 1 do
              dcraw.getRow(i, Bitmap.ScanLine[i]);
          end;
        ievRGB48:
          begin
            Bitmap.Allocate(width, height, ie48RGB);
            for i := 0 to height - 1 do
              dcraw.getRow(i, Bitmap.ScanLine[i]);
          end;
      end;
    end
    else
    if not Preview then
      Progress.Aborting^ := true;

  finally
    bufStream.Free();
  end;
end;


function IERAWTryStream(Stream: TStream): boolean;
var
  dcraw: TIELibDCRAWDecoder;
  bufStream: TIEBufferedReadStream;
  params: TIEVisionVectorString;
begin
  if not IELibAvailable() then
    raise EIERAWException.Create(IERS_IEVISIONNOTFOUND);

  params := IELib.createVectorString();
  params.push_back('par.000');
  params.push_back('-i');      // identify only
  params.push_back('par.111');

  bufStream := TIEBufferedReadStream.Create(Stream, 1 * 1024 * 1024);

  try
    dcraw := IELib.createDCRAWDecoder(IELib.createCustomStream(TIEVCLStreamProvider.Create(bufStream)), params, nil, nil);
    result := (dcraw.getIntInfo(ievWIDTH) <> 0) and (dcraw.getIntInfo(ievHEIGHT) <> 0);

  finally
    bufstream.Free();
  end;

end;


{$endif}  // IEUSEDLLRAWLIB





const

  // following consts comes from ciff.h of Peter Galbavy, then only for the consts part it is valid "Copyright (c) 2001 Peter Galbavy.  All rights reserved."

  // type codes
  CIFF_TC_ST_MASK      =$C000;
  CIFF_TC_DT_MASK      =$3800;
  CIFF_TC_ID_MASK      =$07FF;

  CIFF_TC_ST_HEAP      =$0000;
  CIFF_TC_ST_RECORD    =$4000;

  CIFF_TC_DT_BYTE      =$0000;
  CIFF_TC_DT_ASCII     =$0800;
  CIFF_TC_DT_WORD      =$1000;
  CIFF_TC_DT_DWORD     =$1800;
  CIFF_TC_DT_BYTE2     =$2000;
  CIFF_TC_DT_HEAP1     =$2800;
  CIFF_TC_DT_HEAP2     =$3000;

  CIFF_TC_ID_WILDCARD  =$ffff;
  CIFF_TC_ID_NULL      =$0000;
  CIFF_TC_ID_FREE      =$0001;
  CIFF_TC_ID_EXUSED    =$0002;

  // BYTE types
  CIFF_TC_ID_CANONUNK32    = (CIFF_TC_DT_BYTE or $0032);
  CIFF_TC_ID_CANONUNK36    = (CIFF_TC_DT_BYTE or $0036);

  CIFF_TC_ID_CANONRAW      = (CIFF_TC_DT_BYTE2 or $0005);
  CIFF_TC_ID_CANONJPEG1    = (CIFF_TC_DT_BYTE2 or $0007);
  CIFF_TC_ID_CANONJPEG2    = (CIFF_TC_DT_BYTE2 or $0008);

  // ASCII types
  CIFF_TC_ID_DESCRIPTION   = (CIFF_TC_DT_ASCII or $0005);
  CIFF_TC_ID_MODELNAME     = (CIFF_TC_DT_ASCII or $000a);
  CIFF_TC_ID_FIRMWARE      = (CIFF_TC_DT_ASCII or $000b);
  CIFF_TC_ID_COMPONENT     = (CIFF_TC_DT_ASCII or $000c);
  CIFF_TC_ID_ROMOPMODE     = (CIFF_TC_DT_ASCII or $000d);
  CIFF_TC_ID_OWNERNAME     = (CIFF_TC_DT_ASCII or $0010);
  CIFF_TC_ID_IMAGENAME     = (CIFF_TC_DT_ASCII or $0015);
  CIFF_TC_ID_IMAGEFILE     = (CIFF_TC_DT_ASCII or $0016);
  CIFF_TC_ID_THUMBNAIL     = (CIFF_TC_DT_ASCII or $0017);

  // WORD types
  CIFF_TC_ID_TARGETIMG     = (CIFF_TC_DT_WORD or $000a);
  CIFF_TC_ID_SR_RELMETH    = (CIFF_TC_DT_WORD or $0010);
  CIFF_TC_ID_SR_RELTIME    = (CIFF_TC_DT_WORD or $0011);
  CIFF_TC_ID_RELEASE       = (CIFF_TC_DT_WORD or $0016);
  CIFF_TC_ID_BODYSENSE     = (CIFF_TC_DT_WORD or $001c);

  CIFF_TC_ID_CANONUNK28    = (CIFF_TC_DT_WORD or $0028);
  CIFF_TC_ID_CANONUNK29    = (CIFF_TC_DT_WORD or $0029);
  CIFF_TC_ID_CANONUNK2A    = (CIFF_TC_DT_WORD or $002a);
  CIFF_TC_ID_CANONUNK2C    = (CIFF_TC_DT_WORD or $002c);
  CIFF_TC_ID_CANONUNK2D    = (CIFF_TC_DT_WORD or $002d);
  CIFF_TC_ID_CANONUNK30    = (CIFF_TC_DT_WORD or $0030);
  CIFF_TC_ID_CANON_SENSOR  = (CIFF_TC_DT_WORD or $0031);
  CIFF_TC_ID_CANON_CFN     = (CIFF_TC_DT_WORD or $0033);

  CIFF_TC_ID_CANONUNKA8    = (CIFF_TC_DT_WORD or $00a8); // D60
  CIFF_TC_ID_CANON_WB2     = (CIFF_TC_DT_WORD or $00a9); // D60 WB
  CIFF_TC_ID_CANONUNKAA    = (CIFF_TC_DT_WORD or $00aa); // D60
  CIFF_TC_ID_CANONUNKC0    = (CIFF_TC_DT_WORD or $00c0); // D60
  CIFF_TC_ID_CANONUNKC1    = (CIFF_TC_DT_WORD or $00c1); // D60
  CIFF_TC_ID_CANONUNKC2    = (CIFF_TC_DT_WORD or $00c2); // D60

  // DWORD types
  CIFF_TC_ID_IMAGEFORMAT   = (CIFF_TC_DT_DWORD or $0003);
  CIFF_TC_ID_RECORDID      = (CIFF_TC_DT_DWORD or $0004);
  CIFF_TC_ID_SELFTIMER     = (CIFF_TC_DT_DWORD or $0006);
  CIFF_TC_ID_SR_TARGETDST  = (CIFF_TC_DT_DWORD or $0007);
  CIFF_TC_ID_BODYID        = (CIFF_TC_DT_DWORD or $000b);
  CIFF_TC_ID_CAPTURETIME   = (CIFF_TC_DT_DWORD or $000e);
  CIFF_TC_ID_IMAGESPEC     = (CIFF_TC_DT_DWORD or $0010);
  CIFF_TC_ID_SR_EF         = (CIFF_TC_DT_DWORD or $0013);
  CIFF_TC_ID_MI_EV         = (CIFF_TC_DT_DWORD or $0014);
  CIFF_TC_ID_SERIALNUMBER  = (CIFF_TC_DT_DWORD or $0017);
  CIFF_TC_ID_SR_EXPOSURE   = (CIFF_TC_DT_DWORD or $0018);

  CIFF_TC_ID_CANONUNK34    = (CIFF_TC_DT_DWORD or $0034);

  CIFF_TC_ID_CANONUNK7F    = (CIFF_TC_DT_DWORD or $007f); // G2

  // from David Coffin's code
  CIFF_TC_ID_DECODETABLE   = (CIFF_TC_DT_DWORD or $0035);

  // HEAP1 types
  CIFF_TC_ID_IMAGEDESC     = (CIFF_TC_DT_HEAP1 or $0004);
  CIFF_TC_ID_CAMERAOBJECT  = (CIFF_TC_DT_HEAP1 or $0007);

  // HEAP2 types
  CIFF_TC_ID_SHOOTINGREC   = (CIFF_TC_DT_HEAP2 or $0002);
  CIFF_TC_ID_MEASUREDINFO  = (CIFF_TC_DT_HEAP2 or $0003);
  CIFF_TC_ID_CAMERASPEC    = (CIFF_TC_DT_HEAP2 or $0004);
  CIFF_TC_ID_IMAGEPROPS    = (CIFF_TC_DT_HEAP2 or $000a);
  CIFF_TC_ID_CANONRAWPROPS = (CIFF_TC_DT_HEAP2 or $000b);

  // Canon Specific Values

  CIFF_CANON_ISO_OFFSET    = $0004;  // in CIFF_TC_ID_CANONUNK2A
  CIFF_CANON_ISO_100       = 160;
  CIFF_CANON_ISO_200       = 192;
  CIFF_CANON_ISO_400       = 224;
  CIFF_CANON_ISO_800       = 256;
  CIFF_CANON_ISO_1000      = 268;
  CIFF_CANON_ISO_1600      = 288;

  CIFF_CANON_WB_OFFSET      = $000e;  // in CIFF_TC_ID_CANONUNK2A
  CIFF_CANON_WB_AUTO        = 0;
  CIFF_CANON_WB_DAYLIGHT    = 1;
  CIFF_CANON_WB_CLOUDY      = 2;
  CIFF_CANON_WB_TUNGSTEN    = 3;
  CIFF_CANON_WB_FLOURESCENT = 4;
  CIFF_CANON_WB_FLASH       = 5;
  CIFF_CANON_WB_PRESET      = 6;

  CIFF_CANON_SUBJ_DIST    = $0028;  // in CIFF_TC_ID_CANONUNK2A

// get CIFF info and thumbnails from CRW, and copy them to Exif tags
// return true if successful
function IECRWGetCIFFAsExif(Stream: TStream; var IOParams: TIOParamsVals): boolean;
type
  CRWHeader=packed record
    Id: word;                    // II for Intel, MM for Motorola
    ciffpos: dword;              // start of CIFF block
    magic: array [0..7] of AnsiChar; // must be 'HEAPCCDR'
  end;
var
  header: CRWHeader;
  inv: boolean;
  thumbpos: integer;
  thumbwidth, thumbheight: integer;
  makernotesdata: TList;
  makernoteslen: TList;
  makernotesid: TList;

  function GetWord: word;
  begin
    Stream.Read(result, sizeof(word));
    result := IECSwapWord(result, inv);
  end;
  function GetDWord: dword;
  begin
    Stream.Read(result, sizeof(dword));
    result := IECSwapDWord(result, inv);
  end;
  procedure CheckJpegThumbnail(len: integer);
  var
    io: TImageEnIO;
    p: integer;
  begin
    p := Stream.Position;
    io := TImageEnIO.Create(nil);
    try
      io.ParamsFromStreamFormat(Stream, ioJpeg);
      if (thumbwidth>io.Params.Width) and (thumbheight>io.Params.Height) then
      begin
        thumbpos := p;
        thumbwidth := io.Params.Width;
        thumbheight := io.Params.Height;
      end;
    finally
      FreeAndNil(io);
    end;
  end;
  procedure LoadJpegThumbnail;
  var
    io: TImageEnIO;
  begin
    if not assigned(ioparams.EXIF_Bitmap) then
      ioparams.EXIF_Bitmap := TIEBitmap.Create;
    ioparams.EXIF_Bitmap.FreeImage(true);
    if thumbpos>0 then
    begin
      io := TImageEnIO.CreateFromBitmap(ioparams.EXIF_Bitmap);
      try
        Stream.Position := thumbpos;
        io.LoadFromStreamJpeg(Stream);
      finally
        FreeAndNil(io);
      end;
    end;
  end;
  // read until reach #0 or len
  function ReadString(len: integer): AnsiString;
  var
    c: AnsiChar;
  begin
    result := '';
    repeat
      Stream.Read(c, 1);
      if c=#0 then
        break;
      result := result+c;
    until length(result)>=len;
  end;
  procedure ReadMakerNote(id: integer; len: integer);
  var
    buf: pbyte;
  begin
    makernoteslen.Add(pointer(len));
    getmem(buf, len*sizeof(word));
    Stream.Read(buf^, len*sizeof(word));
    makernotesdata.Add(buf);
    makernotesid.Add(pointer(id));
  end;
  procedure WriteAndFreeAllMakerNotes;
  var
    i: integer;
    tag: TTIFFTag;
    datapos: integer;
    tagpos: integer;
    w: word;
  begin
    w := makernotesid.Count;
    IOParams.EXIF_MakerNote.Clear;
    IOParams.EXIF_MakerNote.Data.Write(w, sizeof(word));
    tagpos := IOParams.EXIF_MakerNote.Data.Position;
    datapos := tagpos+makernotesid.Count*sizeof(TTIFFTag);
    for i := 0 to makernotesid.Count-1 do
    begin
      tag.IdTag := integer(makernotesid[i]);
      tag.DataType := 3;
      tag.DataNum := integer(makernoteslen[i]);
      tag.DataPos := datapos;
      IOParams.EXIF_MakerNote.Data.Write(tag, sizeof(TTIFFTag));
      tagpos := IOParams.EXIF_MakerNote.Data.Position;
      IOParams.EXIF_MakerNote.Data.Position := datapos;
      IOParams.EXIF_MakerNote.Data.Write( pbyte(makernotesdata[i])^, tag.DataNum*sizeof(word) );
      freemem(pointer(makernotesdata[i]));
      datapos := IOParams.EXIF_MakerNote.Data.Position;
      IOParams.EXIF_MakerNote.Data.Position := tagpos;
    end;
    IOParams.EXIF_MakerNote.Update;
  end;

  procedure ParseCIFF(ciffpos: integer; cifflen: integer);
  var
    recnum: word;
    i, npos: integer;
    tagtype: word;
    taglen: dword;
    tagpos: dword;
    temp: AnsiString;
    //temp2: word;
    //temp3: dword;
    //temp4: dword;
    //temp5: dword;
    streamSize: int64;
  begin
    streamSize := Stream.Size;
    Stream.Position := ciffpos+cifflen-4;
    if Stream.Position>=streamSize then
      exit;
    Stream.Position := GetDWord+ciffpos;
    if Stream.Position>=streamSize then
      exit;
    recnum := GetWord;
    if recnum > 256 then // added in 2.2.2 (b) to avoid infinite loop in raw\P1000490.RAW
      recnum := 256;
    for i := 0 to recnum-1 do
    begin
      tagtype := GetWord;
      taglen := GetDWord;
      tagpos := GetDWord+ciffpos;
      npos := Stream.Position;
      if ((tagtype and CIFF_TC_ST_MASK) = CIFF_TC_ST_RECORD) then
      begin
        // data in taglen and tagpos
        taglen := 8;
        tagpos := npos-8;
      end;
      Stream.Position := tagpos;
      if Stream.Position>=streamSize then
        exit;

      //temp5 := temp4;
      //temp4 := temp3;
      //temp3 := temp2;
      //temp2 := tagtype and CIFF_TC_ID_MASK;

      case (tagtype and not CIFF_TC_ST_MASK) of
        CIFF_TC_ID_CAMERAOBJECT,
        CIFF_TC_ID_SHOOTINGREC,
        CIFF_TC_ID_MEASUREDINFO,
        CIFF_TC_ID_CAMERASPEC,
        CIFF_TC_ID_IMAGEPROPS,
        CIFF_TC_ID_CANONRAWPROPS,
        CIFF_TC_DT_HEAP1,
        CIFF_TC_DT_HEAP2:
          ParseCIFF(tagpos, taglen);

        CIFF_TC_ID_CANONJPEG1:
          // it could be the thumbnail
          CheckJpegThumbnail(taglen);
        CIFF_TC_ID_CANONJPEG2:
          // it could be the thumbnail
          CheckJpegThumbnail(taglen);
        CIFF_TC_ID_DESCRIPTION:
          IOParams.EXIF_ImageDescription := ReadString(taglen);
        CIFF_TC_ID_MODELNAME:
          begin
            IOParams.EXIF_Make := ReadString(taglen);
            IOParams.EXIF_Model := ReadString(taglen);
          end;
        CIFF_TC_ID_FIRMWARE:
          IOParams.EXIF_Software := ReadString(taglen);
        CIFF_TC_ID_COMPONENT:
          temp := ReadString(taglen);
        CIFF_TC_ID_ROMOPMODE:
          temp := ReadString(taglen); // Rom Operation Mode (no EXIF)
        CIFF_TC_ID_OWNERNAME:
          temp := ReadString(taglen);
        CIFF_TC_ID_IMAGEFILE:
          temp := ReadString(taglen); // name of the file (CRW) (no EXIF)
        CIFF_TC_ID_THUMBNAIL:
          temp := ReadString(taglen); // name of the thumbnail file (THM) (no EXIF)


        CIFF_TC_ID_TARGETIMG:
          temp := temp;
        CIFF_TC_ID_SR_RELMETH:
          temp := temp;
        CIFF_TC_ID_SR_RELTIME:
          temp := temp;
        CIFF_TC_ID_RELEASE:
          temp := temp;
        CIFF_TC_ID_BODYSENSE:
          temp := temp;
        CIFF_TC_ID_CANON_SENSOR:
          temp := temp;
        CIFF_TC_ID_CANON_WB2:
          temp := temp;
        CIFF_TC_ID_IMAGEFORMAT:
          temp := temp;
        CIFF_TC_ID_RECORDID:
          temp := temp;
        CIFF_TC_ID_SELFTIMER:
          temp := temp;
        CIFF_TC_ID_SR_TARGETDST:
          temp := temp;
        CIFF_TC_ID_BODYID:
          temp := temp;
        CIFF_TC_ID_CAPTURETIME:
          temp := temp;
        CIFF_TC_ID_SR_EF:
          temp := temp;
        CIFF_TC_ID_MI_EV:
          temp := temp;
        CIFF_TC_ID_SERIALNUMBER:
          temp := temp;
        CIFF_TC_ID_SR_EXPOSURE:
          temp := temp;
        CIFF_TC_ID_DECODETABLE:
          temp := temp;


        CIFF_TC_ID_IMAGESPEC:
          begin
            IOParams.EXIF_ExifImageWidth := GetDWord;
            IOParams.EXIF_ExifImageHeight := GetDWord;
          end;

        CIFF_TC_ID_IMAGEDESC:
          temp := temp;

        CIFF_TC_ID_CANONUNK36:
          temp := temp;

        CIFF_TC_ID_CANONUNK2D:
          // Maker note tag 1
          ReadMakerNote(1, taglen);
        CIFF_TC_ID_CANONUNK2A:
          // Maker note tag 4
          ReadMakerNote(4, taglen);


        CIFF_TC_ID_CANON_CFN:
          temp := temp;

      end;

      if (tagtype and CIFF_TC_DT_MASK)=CIFF_TC_DT_ASCII then
      begin
        temp := ReadString(taglen);
      end;

      Stream.Position := npos;
    end;
  end;

begin
  makernotesdata := TList.Create;
  makernoteslen := TList.Create;
  makernotesid := TList.Create;
  try
    thumbwidth := maxint-1;
    thumbheight := maxint-1;
    thumbpos := -1;
    result := false;
    Stream.Read(header, sizeof(CRWHeader));
    if (header.Id <> $4949) and (header.id <> $4D4D) and (header.magic<>'HEAPCCDR') then
      exit; // fail
    inv := header.Id=$4D4D;
    header.ciffpos := IECSwapDWord(header.ciffpos, inv);
    ParseCIFF( header.ciffpos , Stream.Size-header.ciffpos );
    LoadJpegThumbnail;
    WriteAndFreeAllMakerNotes;
  finally
    FreeAndNil(makernotesdata);
    FreeAndNil(makernoteslen);
    FreeAndNil(makernotesid);
  end;
end;

// get jpeg image from a CRW
// return true if successful
function IECRWGetJpeg(Bitmap: TIEBitmap; Stream: TStream): boolean;
type
  CRWHeader=packed record
    Id: word;                    // II for Intel, MM for Motorola
    ciffpos: dword;              // start of CIFF block
    magic: array [0..7] of AnsiChar; // must be 'HEAPCCDR'
  end;
var
  header: CRWHeader;
  inv: boolean;
  jpegpos: integer;
  jpegwidth, jpegheight: integer;

  function GetWord: word;
  begin
    Stream.Read(result, sizeof(word));
    result := IECSwapWord(result, inv);
  end;
  function GetDWord: dword;
  begin
    Stream.Read(result, sizeof(dword));
    result := IECSwapDWord(result, inv);
  end;
  procedure CheckJpeg(len: integer);
  var
    io: TImageEnIO;
    p: integer;
  begin
    p := Stream.Position;
    io := TImageEnIO.Create(nil);
    try
      io.ParamsFromStreamFormat(Stream, ioJpeg);
      if (jpegwidth<io.Params.Width) and (jpegheight<io.Params.Height) then
      begin
        jpegpos := p;
        jpegwidth := io.Params.Width;
        jpegheight := io.Params.Height;
      end;
    finally
      FreeAndNil(io);
    end;
  end;
  procedure LoadJpeg;
  var
    io: TImageEnIO;
  begin
    if jpegpos>0 then
    begin
      io := TImageEnIO.CreateFromBitmap(Bitmap);
      try
        Stream.Position := jpegpos;
        io.LoadFromStreamJpeg(Stream);
      finally
        FreeAndNil(io);
      end;
    end;
  end;
  procedure ParseCIFF(ciffpos: integer; cifflen: integer);
  var
    recnum: word;
    i, npos: integer;
    tagtype: word;
    taglen: dword;
    tagpos: dword;
    //temp: AnsiString;
    //temp2: word;
    //temp3: dword;
    //temp4: dword;
    //temp5: dword;
  begin
    Stream.Position := ciffpos+cifflen-4;
    Stream.Position := GetDWord+ciffpos;
    recnum := GetWord;
    for i := 0 to recnum-1 do
    begin
      tagtype := GetWord;
      taglen := GetDWord;
      tagpos := GetDWord+ciffpos;
      npos := Stream.Position;
      if ((tagtype and CIFF_TC_ST_MASK) = CIFF_TC_ST_RECORD) then
      begin
        // data in taglen and tagpos
        taglen := 8;
        tagpos := npos-8;
      end;
      Stream.Position := tagpos;

      //temp5 := temp4;
      //temp4 := temp3;
      //temp3 := temp2;
      //temp2 := tagtype and CIFF_TC_ID_MASK;

      case (tagtype and not CIFF_TC_ST_MASK) of
        CIFF_TC_ID_CAMERAOBJECT,
        CIFF_TC_ID_SHOOTINGREC,
        CIFF_TC_ID_MEASUREDINFO,
        CIFF_TC_ID_CAMERASPEC,
        CIFF_TC_ID_IMAGEPROPS,
        CIFF_TC_ID_CANONRAWPROPS,
        CIFF_TC_DT_HEAP1,
        CIFF_TC_DT_HEAP2:
            ParseCIFF(tagpos, taglen);

        CIFF_TC_ID_CANONJPEG1:
          // it could be the jpeg
          CheckJpeg(taglen);
        CIFF_TC_ID_CANONJPEG2:
          // it could be the jpeg
          CheckJpeg(taglen);

      end;

      Stream.Position := npos;
    end;
  end;

begin
  jpegwidth := 0;
  jpegheight := 0;
  jpegpos := -1;
  result := false;
  Stream.Read(header, sizeof(CRWHeader));
  if (header.Id <> $4949) and (header.id <> $4D4D) and (header.magic<>'HEAPCCDR') then
    exit; // fail
  inv := header.Id=$4D4D;
  header.ciffpos := IECSwapDWord(header.ciffpos, inv);
  ParseCIFF( header.ciffpos , Stream.Size-header.ciffpos );
  LoadJpeg;
end;


{$endif}  // IEINCLUDERAWFORMATS

end.
