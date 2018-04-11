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
File version 1006
*)

unit iefft;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$R-}
{$Q-}

{$I ie.inc}

interface

{$ifdef IEINCLUDEFFT}

uses Windows, Graphics, Classes, hyiedefs, hyieutils;

type

  TIEsinglearray = array[0..16384] of single;
  PIEsinglearray = ^TIEsinglearray;

  TIECOMPLEX_IMAGE = array[0..16384] of PIEsinglearray;
  PIECOMPLEX_IMAGE = ^TIECOMPLEX_IMAGE;

  PIEdouble = ^double;
  PIEinteger = ^integer;
  PIEsingle = ^single;
  PIElongint = ^longint;

  TIEComplexColor = packed record
    // red channel
    real_Red: PIEsingle;
    imag_Red: PIEsingle;
    // blue channel
    real_Blue: PIEsingle;
    imag_Blue: PIEsingle;
    // green channel
    real_Green: PIEsingle;
    imag_Green: PIEsingle;
    // gray scale
    imag_gray: PIEsingle;
    real_gray: PIEsingle;
  end;

  TIEComplex = packed record
    real: single;
    imag: single;
  end;

{!!
<FS>TIEFtImageType

<FM>Declaration<FC>
}
  TIEFtImageType = (ieitUnknown, ieitRGB, ieitGrayscale);
{!!}

  tdwordarray = array[0..maxint div 16] of dword;
  pdwordarray = ^tdwordarray;

  tlongintarray = array[0..maxint div 16] of longint;
  plongintarray = ^tlongintarray;

  TIEFtImage = class
  private
    sintabpt: PIEdouble;
    bittabpt: PIEinteger;
    powers: PIEinteger;
    numpts: integer;
    nn: integer;
    direction: single;
    scalef: double;
    FFTN: integer;
    NORMALIZE: boolean;
    //
    fftr: PIECOMPLEX_IMAGE; // FT of R
    fftg: PIECOMPLEX_IMAGE; // FT of G
    fftb: PIECOMPLEX_IMAGE; // FT of B
    fftgray: PIECOMPLEX_IMAGE; // FT of gray scale
    fOX, fOY: integer; // origins of fOrigBitmap inside fBitmap
    fOrigBitmapWidth, fOrigBitmapHeight: integer; // original input bitmap sizes
    fOnProgress: TIEProgressEvent;
    fImageType: TIEFtImageType;
    //
    //procedure image_fft(image: TBitmap; var output: PIECOMPLEX_IMAGE; ch: integer);
    procedure image_fftoc(image: TIEBitmap; var output: PIECOMPLEX_IMAGE; ch: integer; var Progress: TProgressRec);
    procedure fftx(image: TIEBitmap; oc: integer; var output: PIECOMPLEX_IMAGE; ch: integer; var Progress: TProgressRec);
    procedure image_fftinv(image: PIECOMPLEX_IMAGE; var output: PIECOMPLEX_IMAGE; var Progress: TProgressRec);
    procedure pairsort(arr: PIEsinglearray; iarr: pdwordarray; n: integer);
    procedure fqsort(arr: PIEsinglearray; iarr: pdwordarray; l: integer; r: integer);
    //procedure image_fftinvoc(image: PIECOMPLEX_IMAGE; var output: PIECOMPLEX_IMAGE);
    procedure fftinvx(image: PIECOMPLEX_IMAGE; oc: integer; var output: PIECOMPLEX_IMAGE; var Progress: TProgressRec);
    procedure fft2d(image: PIECOMPLEX_IMAGE; direction: single; var Progress: TProgressRec);
    procedure filt_orig(xarray: PIECOMPLEX_IMAGE);
    procedure realtoint(fim: PIECOMPLEX_IMAGE; H: plongintarray);
    procedure fft(data: PIEsinglearray; dir: single);
    procedure _fft(tseries: PIEsinglearray; level: integer; chunk: integer);
    procedure fftinit(nopts: integer);
    function bitrev(bits: integer): integer;
    function newcomplex(im: TIEBitmap; ch: integer): PIECOMPLEX_IMAGE;
    function dupcomplex(im: PIECOMPLEX_IMAGE): PIECOMPLEX_IMAGE;
    procedure filt_toint(oimage: PIECOMPLEX_IMAGE; output: TIEBitmap; ch: integer);
    function GetComplexImage(x, y: integer): TIEComplexColor;
  protected
    procedure FreeAll;
  public
    constructor Create;
    destructor Destroy; override;
    // transformations
    procedure BuildFT(fOrigBitmap: TIEBitmap; ImageType: TIEFtImageType); // direct Fourier transf
    procedure BuildBitmap(Bitmap: TIEBitmap); // inverse Fourier transf
    procedure GetFTImage(Bitmap: TIEBitmap); // display an image of Fourier transf
    class procedure CalcFFTImageSize(ImageWidth, ImageHeight: integer; var FFTImageWidthHeight: integer);
    class procedure CalcSuitableSourceSize(OrigWidth, OrigHeight: integer; var NewWidth: integer; var NewHeight: integer);
    // processing transformed image
    procedure HiPass(radius: integer);
    procedure LoPass(radius: integer);
    procedure ClearZone(x1, y1, x2, y2: integer; vreal: single=0; vimag: single=0); overload;
    procedure ClearZone(mask: TIEMask; vreal: single=0; vimag: single=0); overload;
    // complex image access
    property ComplexPixel[x, y: integer]: TIEComplexColor read GetComplexImage;
    property Imagetype: TIEFtImageType read fImageType;
    property ComplexWidth: integer read FFTN;
    property ComplexHeight: integer read FFTN;
    procedure Assign(Source: TIEFtImage);
    // events
    property OnProgress: TIEProgressEvent read fOnProgress write fOnProgress;
  end;

{$endif}

implementation

{$ifdef IEINCLUDEFFT}

uses SysUtils, math, imageenproc, iesettings;

{$R-}

procedure freecomplex(var x: PIECOMPLEX_IMAGE);
begin
  if (x <> nil) then
  begin
    freemem(x[0]);
    freemem(x);
    x := nil;
  end;
end;

constructor TIEFtImage.Create;
begin
  inherited Create;
  sintabpt := nil;
  bittabpt := nil;
  powers := nil;
  scalef := 0.0;
  FFTN := 0;
  NORMALIZE := false;
  //
  fftr := nil;
  fftg := nil;
  fftb := nil;
  fftgray := nil;
  fOnProgress := nil;
  fImageType := ieitUnknown;
end;

procedure TIEFtImage.FreeAll;
begin
  if fftr <> nil then
    freecomplex(fftr);
  if fftg <> nil then
    freecomplex(fftg);
  if fftb <> nil then
    freecomplex(fftb);
  if fftgray <> nil then
    freecomplex(fftgray);
end;

destructor TIEFtImage.Destroy;
begin
  FreeAll;
  if (sintabpt <> nil) then
    freemem(sintabpt);
  if (bittabpt <> nil) then
    freemem(bittabpt);
  if (powers <> nil) then
    freemem(powers);
  //
  inherited;
end;

procedure TIEFtImage.Assign(Source: TIEFtImage);
begin
  FreeAll;
  FFTN := Source.FFTN;
  NORMALIZE := Source.NORMALIZE;
  fftinit(FFTN);
  fOX := Source.fOX;
  fOY := Source.fOY;
  fOrigBitmapWidth := Source.fOrigBitmapWidth;
  fOrigBitmapHeight := Source.fOrigBitmapHeight;
  fOnProgress := Source.fOnProgress;
  fImageType := Source.fImageType;
  if assigned(Source.fftr) then
    fftr := dupcomplex(Source.fftr);
  if assigned(Source.fftg) then
    fftg := dupcomplex(Source.fftg);
  if assigned(Source.fftb) then
    fftb := dupcomplex(Source.fftb);
  if assigned(Source.fftgray) then
    fftgray := dupcomplex(Source.fftgray);
end;

class procedure TIEFtImage.CalcSuitableSourceSize(OrigWidth, OrigHeight: integer; var NewWidth: integer; var NewHeight: integer);
begin
  if OrigWidth > OrigHeight then
  begin
    NewWidth  := imax(OrigWidth div 3, 128);
    NewHeight := (OrigHeight * NewWidth) div OrigWidth;
  end
  else
  begin
    NewHeight := imax(OrigHeight div 3, 128);
    NewWidth  := (OrigWidth * NewHeight) div OrigHeight;
  end;
end;


class procedure TIEFtImage.CalcFFTImageSize(ImageWidth, ImageHeight: integer; var FFTImageWidthHeight: integer);
var
  i: integer;
begin
  FFTImageWidthHeight := imax(ImageWidth, ImageHeight);
  i := 0;
  while (1 shl i) < FFTImageWidthHeight do
    inc(i);
  FFTImageWidthHeight := 1 shl i;
end;

// converts from bitmap to FT
// channel:
// 0=b 1=g 2=r 3=rgb 4=grayscale
procedure TIEFtImage.BuildFT(fOrigBitmap: TIEBitmap; ImageType: TIEFtImageType);
var
  fBitmap: TIEBitmap;
  ww: integer;
  Progress: TProgressRec;
begin
  fImageType := ImageType;
  Progress.fOnProgress := fOnProgress;
  Progress.Sender := Self;
  if fftr <> nil then
    freecomplex(fftr);
  if fftg <> nil then
    freecomplex(fftg);
  if fftb <> nil then
    freecomplex(fftb);
  if fftgray <> nil then
    freecomplex(fftgray);

  fOrigBitmapWidth := fOrigBitmap.Width;
  fOrigBitmapHeight := fOrigBitmap.Height;

  CalcFFTImageSize(fOrigBitmapWidth, fOrigBitmapHeight, ww);

  fBitmap := TIEBitmap.Create;
  fBitmap.Allocate(ww, ww, fOrigBitmap.PixelFormat);
  fBitmap.Fill(0);
  fOX := (ww - fOrigBitmapWidth) div 2;
  fOY := (ww - fOrigBitmapHeight) div 2;
  fOrigBitmap.CopyRectTo(fBitmap, 0, 0, fOX, fOY, fOrigBitmap.Width, fOrigBitmap.Height);
  //
  normalize := true;
  case fImageType of
    ieitRGB: // RGB
      begin
        Progress.tot := 3;
        Progress.val := 0;
        image_fftoc(fBitmap, fftr, 2, Progress);
        image_fftoc(fBitmap, fftg, 1, Progress);
        image_fftoc(fBitmap, fftb, 0, Progress);
      end;
    ieitGrayScale: // GRAYSCALE
      begin
        Progress.tot := 1;
        Progress.val := 0;
        image_fftoc(fBitmap, fftgray, 3, Progress);
      end;
  end;
  normalize := false;
  FreeAndNil(fBitmap);
end;

// Bitmap must be created

procedure TIEFtImage.GetFTImage(Bitmap: TIEBitmap);
begin
  Bitmap.Allocate(FFTN, FFTN, ie24RGB);
  case fImageType of
    ieitRGB:
      begin
        // RGB
        filt_toint(fftr, Bitmap, 2);
        filt_toint(fftg, Bitmap, 1);
        filt_toint(fftb, Bitmap, 0);
      end;
    ieitGrayscale:
      // gray scale
      filt_toint(fftgray, Bitmap, 3);
  end;
end;

// converts from FT to bitmap
// Bitmap.PixleFormat will be ie24RGB

procedure TIEFtImage.BuildBitmap(Bitmap: TIEBitmap);
var
  offtr, offtg, offtb: PIECOMPLEX_IMAGE;
  x, y: integer;
  rgb: PRGB;
  Progress: TProgressRec;
  bitmapheight1, bitmapwidth1: integer;
begin
  Progress.fOnProgress := fOnProgress;
  Progress.Sender := Self;
  Bitmap.Allocate(fOrigBitmapWidth, fOrigBitmapHeight, ie24RGB);
  case fImageType of
    ieitRGB:
      begin
        // RGB
        Progress.tot := 3;
        Progress.val := 0;
        image_fftinv(fftr, offtr, Progress);
        realtoint(offtr, nil);
        image_fftinv(fftg, offtg, Progress);
        realtoint(offtg, nil);
        image_fftinv(fftb, offtb, Progress);
        realtoint(offtb, nil);
        bitmapheight1 := Bitmap.Height - 1;
        bitmapwidth1 := Bitmap.Width - 1;
        for y := 0 to BitmapHeight1 do
        begin
          rgb := Bitmap.Scanline[y];
          for x := 0 to BitmapWidth1 do
          begin
            rgb^.r := trunc(offtr^[y + fOY]^[x + fOX]);
            rgb^.g := trunc(offtg^[y + fOY]^[x + fOX]);
            rgb^.b := trunc(offtb^[y + fOY]^[x + fOX]);
            inc(rgb);
          end;
        end;
        freecomplex(offtr);
        freecomplex(offtg);
        freecomplex(offtb);
      end;
    ieitGrayscale:
      begin
        // gray scale
        Progress.tot := 1;
        Progress.val := 0;
        image_fftinv(fftgray, offtg, Progress);
        realtoint(offtg, nil);
        bitmapheight1 := Bitmap.Height - 1;
        bitmapwidth1 := Bitmap.Width - 1;
        for y := 0 to BitmapHeight1 do
        begin
          rgb := Bitmap.Scanline[y];
          for x := 0 to BitmapWidth1 do
          begin
            rgb^.r := trunc(offtg^[y + fOY]^[x + fOX]);
            rgb^.g := rgb^.r;
            rgb^.b := rgb^.r;
            inc(rgb);
          end;
        end;
        freecomplex(offtg);
      end;
  end;
end;

(*
// ch: channel of im = 0: B 1: G 2: R 3: grayscale
procedure TIEFtImage.image_fft(image: TIEBitmap; var output: PIECOMPLEX_IMAGE; ch: integer);
begin
 fftx(image, 0, output, ch);
end;
*)

// ch: channel of im = 0:B 1:G 2:R 3:grayscale

procedure TIEFtImage.image_fftoc(image: TIEBitmap; var output: PIECOMPLEX_IMAGE; ch: integer; var Progress: TProgressRec);
begin
  fftx(image, 1, output, ch, Progress);
end;

// Do a 2D FFT on a set of BYTE-type pixels
// ch: channel of im = 0:B 1:G 2:R 3:grayscale

procedure TIEFtImage.fftx(image: TIEBitmap; oc: integer; var output: PIECOMPLEX_IMAGE; ch: integer; var Progress: TProgressRec);
var
  n: integer;
  cim: PIECOMPLEX_IMAGE;
begin
  n := image.Width;
  if (FFTN <> n) then
    fftinit(n);
  FFTN := n;
  cim := newcomplex(image, ch);
  if (oc <> 0) then
    filt_orig(cim);
  fft2d(cim, -1.0, Progress);
  output := cim;
end;

procedure TIEFtImage.image_fftinv(image: PIECOMPLEX_IMAGE; var output: PIECOMPLEX_IMAGE; var Progress: TProgressRec);
begin
  fftinvx(image, 0, output, Progress);
end;

(*
procedure TIEFtImage.image_fftinvoc(image: PIECOMPLEX_IMAGE; var output: PIECOMPLEX_IMAGE);
begin
 fftinvx(image, 1, output);
end;
*)

procedure TIEFtImage.fftinvx(image: PIECOMPLEX_IMAGE; oc: integer; var output: PIECOMPLEX_IMAGE; var Progress: TProgressRec);
var
  cim: PIECOMPLEX_IMAGE;
begin
  cim := dupcomplex(image);
  if (oc <> 0) then
    filt_orig(cim);
  fft2d(cim, 1.0, Progress);
  output := cim;
end;

// Complex product: res = c1*c2
(*
procedure cprod(c1r: single; c1i: single; c2r: PIEsingle; c2i: PIEsingle);
var
 real, imag: single;
begin
 real := c1r*(c2r^) - c1i*(c2i^);
 imag := c1r*(c2i^) + (c2r^)*c1i;
 c2r^ := real;
   c2i^ := imag;
end;
*)

// abs value squared: res = |c1|**2

function pix_cnorm(cr: single; ci: single): single;
begin
  result := cr * cr + ci * ci;
end;

// res = c1/c2 ... Complex
(*
procedure cdiv(c1r: single; c1i: single; c2r: PIEsingle; c2i: PIEsingle);
var
 z, real, imag: single;
begin
   z := ((c2r^)*(c2r^) + (c2i^)*(c2i^));
 if (z <> 0.0) then
 begin
  real := (c1r*(c2r^) + c1i*(c2i^))/z;
  imag := ((c2r^)*c1i - c1r*(c2i^))/z;
  c2r^ := real;
  c2i^ := imag;
 end
 else
 begin
   c2r^ := 0.0;
   c2i^ := 0.0;
 end;
end;
*)

//  fft2d -- Calls `fft' to perform a 2 dimensional Fast Fourier Trans-
//           form on an N x N array of complex data. (N must be defined).

procedure TIEFtImage.fft2d(image: PIECOMPLEX_IMAGE; direction: single; var Progress: TProgressRec);
var
  temp: piesinglearray;
  i, j: integer;
begin
  Progress.per1 := 100 / (FFTN * Progress.Tot);
  getmem(temp, (FFTN * 2 + 1) * 4);
  try
    for i := 0 to FFTN - 1 do
      fft(image^[i], direction);
    for i := 0 to FFTN - 1 do
    begin
      for j := 0 to FFTN - 1 do
      begin
        temp^[j] := image^[j]^[i];
        temp^[j + FFTN] := image^[j]^[i + FFTN];
      end;
      fft(temp, direction);
      for j := 0 to FFTN - 1 do
      begin
        image^[j]^[i] := temp^[j];
        image^[j]^[i + FFTN] := temp^[j + FFTN];
      end;
      with Progress do
        if assigned(fOnProgress) then
          fOnProgress(Sender, trunc(per1 * i + per1 * Progress.Val * FFTN));
    end;
  finally
    freemem(temp);
  end;
  inc(Progress.Val);
end;

//  Fixorig -- Modifies the input data so that after being Fourier trans-
//             formed the origin (f = 0) lies at the center of the array.

procedure TIEFtImage.filt_orig(xarray: PIECOMPLEX_IMAGE);
var
  i, j: integer;
begin
  for i := 0 to FFTN - 1 do
    for j := 0 to FFTN - 1 do
      if ((i + j) mod 2) <> 0 then
      begin
        xarray^[i]^[j] := -xarray^[i]^[j];
        xarray^[i]^[j + FFTN] := -xarray^[i]^[j + FFTN];
      end;
end;

// Convert a REAL/COMPLEX image into an integer one for display
// ch: channel to fill in output (0=B, 1=G, 2=R,  3=RGB)

procedure TIEFtImage.filt_toint(oimage: PIECOMPLEX_IMAGE; output: TIEBitmap; ch: integer);
var
  xmax, xmin, x, xdif: single;
  i, j, n: integer;
  xx: byte;
  rgb: pbytearray;
  pix: pbyte;
  image: PIECOMPLEX_IMAGE;
begin
  image := dupcomplex(oimage);
  n := FFTN;
  xmax := -1.0E20;
  xmin := -xmax;
  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
    begin
      x := sqrt(pix_cnorm(image^[i]^[j], image^[i]^[j + n]));
      if (x > 0.0) then
        x := ln(sqrt(x))
      else
        x := 0.0;
      if (x > xmax) then
        xmax := x;
      if (x < xmin) then
        xmin := x;
      image^[i]^[j] := x;
      image^[i]^[j + FFTN] := 0.0;
    end;
  end;
  if ((xmin <= 0.00001) and (xmin >= -0.00001)) then
    xmin := 0.000000;
  if (xmax - xmin) = 0 then
    xdif := 0
  else
    xdif := 255 / (xmax - xmin);
  for i := 0 to n - 1 do
  begin
    if ch < 3 then
    begin
      // fill R, G or B
      rgb := output.ScanLine[i];
      for j := 0 to n - 1 do
      begin
        rgb^[ch] := blimit(trunc((image^[i]^[j] - xmin) * xdif));
        inc(prgb(rgb));
      end
    end
    else
    begin
      // fill RGB
      pix := output.ScanLine[i];
      for j := 0 to n - 1 do
      begin
        xx := blimit(trunc((image^[i]^[j] - xmin) * xdif));
        pix^ := xx;
        inc(pix);
        pix^ := xx;
        inc(pix);
        pix^ := xx;
        inc(pix);
      end;
    end;
  end;
  freecomplex(image);
end;

// Scale a floating point image to the range 0-255

procedure TIEFtImage.realtoint(fim: PIECOMPLEX_IMAGE; H: plongintarray);
var
  data: PIEsinglearray;
  dij: pdwordarray;
  i, j, k: integer;
  ii, jj: dword;
  flast, xmax, xmin, xd: single;
  nr, nc: integer;
  N: longint;
begin
  flast := 0.0;
  nr := FFTN;
  nc := FFTN;
  N := nr * nc;
  if (H = nil) then
  begin
    xmin := abs(fim^[0]^[0]);
    xmax := xmin;
    for i := 0 to nr - 1 do
      for j := 0 to nc - 1 do
      begin
        fim^[i]^[j] := abs(fim^[i]^[j]);
        if (xmax < fim^[i]^[j]) then
          xmax := fim^[i]^[j];
        if (xmin > fim^[i]^[j]) then
          xmin := fim^[i]^[j];
      end;
    if (xmax - xmin) = 0 then
      xd := 0
    else
      xd := 255 / (xmax - xmin);
    for i := 0 to nr - 1 do
      for j := 0 to nc - 1 do
        fim^[i]^[j] := (fim^[i]^[j] - xmin) * xd;
  end
  else
  begin
    k := 0;
    getmem(data, N * sizeof(single));
    getmem(dij, N * sizeof(dword));
    for i := 0 to nr - 1 do
      for j := 0 to nc - 1 do
      begin
        data[k] := abs(fim^[i]^[j]);
        dij[k] := (i shl 10) or j;
        inc(k);
      end;
    pairsort(data, dij, N);
    j := 0;
    k := 0;
    for i := 0 to 255 do
    begin
      if (k <= 0) then
        k := 0;
      while (k < H[i]) and (j < nr * nc) do
      begin
        ii := dij[j];
        jj := ii and 1023;
        ii := (ii shr 10) and 1023;
        flast := data[j];
        fim[ii][jj] := i;
        inc(k);
        inc(j);
      end;
      k := 0;
      while (data[j] = flast) do
      begin
        ii := dij[j];
        jj := ii mod 1024;
        ii := trunc(ii / 1024);
        fim[ii][jj] := i;
        inc(j);
        inc(k);
      end;
    end;
    freemem(data);
    freemem(dij);
    for i := 0 to nr - 1 do
      for j := 0 to nc - 1 do
        fim^[i]^[j] := abs(fim^[i]^[j]);
  end;
end;


procedure TIEFtImage.fft(data: PIEsinglearray; dir: single);
var
  tempval: TIEComplex;
  i: integer;
  ba: PIEinteger;
begin
  direction := dir;
  if (direction <> 1.0) then
    direction := -1.0;
  _fft(data, 1, 0);
  ba := bittabpt;
  for i := 0 to numpts - 1 do
  begin
    if (ba^ <= i) then
      with tempval do
      begin
        real := data^[i];
        imag := data^[i + FFTN];
        data^[i] := data[ba^];
        data^[i + FFTN] := data[ba^ + FFTN];
        data^[ba^] := real;
        data^[ba^ + FFTN] := imag;
      end;
    inc(ba);
  end;
  for i := 0 to numpts - 1 do
  begin
    data^[i] := data^[i] * scalef;
    data^[i + FFTN] := data^[i + FFTN] * scalef;
  end;
end;

//  _fft -- A recursive FFT routine.

procedure TIEFtImage._fft(tseries: PIEsinglearray; level: integer; chunk: integer);
var
  nodes, i: integer;
  sinindx, cosindx: integer;
  dual1, dual2: integer;
  dual1val, dual2val, dualprod, wp: TIEComplex;
begin
  nodes := (PIEinteger(uint64(powers) + (nn - level) * 4))^;
  sinindx := (PIEinteger(uint64(bittabpt) + (chunk div nodes) * 4))^;
  cosindx := (sinindx + numpts div 4) mod numpts;
  wp.real := (PIEdouble(uint64(sintabpt) + cosindx * 8))^;
  wp.imag := direction * (PIEdouble(uint64(sintabpt) + sinindx * 8))^;
  for i := 0 to nodes - 1 do
  begin
    dual1 := chunk + i;
    dual2 := dual1 + nodes;
    with dual2val do
    begin
      real := tseries^[dual2];
      imag := tseries^[dual2 + FFTN];
      dualprod.real := real * wp.real - imag * wp.imag;
      dualprod.imag := real * wp.imag + imag * wp.real;
    end;
    with dual1val do
    begin
      real := tseries^[dual1];
      imag := tseries^[dual1 + FFTN];
      tseries^[dual1] := real + dualprod.real;
      tseries^[dual1 + FFTN] := imag + dualprod.imag;
      tseries^[dual2] := real - dualprod.real;
      tseries^[dual2 + FFTN] := imag - dualprod.imag;
    end;
  end;
  if (level < nn) then
  begin
    _fft(tseries, level + 1, chunk);
    _fft(tseries, level + 1, chunk + nodes);
  end;
end;

procedure TIEFtImage.fftinit(nopts: integer);
var
  i: integer;
  po: PIEinteger;
  si: PIEdouble;
  bi: PIEinteger;
begin
  numpts := nopts;
  nn := trunc((ln(numpts) / ln(2.0)) + 0.5);
  scalef := 1.0 / sqrt(numpts);
  if (sintabpt <> nil) then
    freemem(sintabpt);
  if (bittabpt <> nil) then
    freemem(bittabpt);
  if (powers <> nil) then
    freemem(powers);
  getmem(sintabpt, nopts * sizeof(double));
  getmem(bittabpt, nopts * sizeof(integer));
  getmem(powers, (nn + 1) * sizeof(integer));
  po := powers;
  for i := 0 to nn do
  begin
    po^ := trunc(power(2.0, i) + 0.5);
    inc(po);
  end;
  si := sintabpt;
  bi := bittabpt;
  for i := 0 to numpts - 1 do
  begin
    si^ := sin(6.283185307179587 * i / numpts);
    bi^ := bitrev(i);
    inc(si);
    inc(bi);
  end;
end;

function TIEFtImage.bitrev(bits: integer): integer;
var
  i, lookmask, setmask, tempbit: integer;
begin
  lookmask := 1;
  setmask := numpts;
  setmask := setmask shr 1;
  tempbit := 0;
  for i := 0 to nn - 1 do
  begin
    if ((bits and lookmask) = lookmask) then
      tempbit := tempbit or setmask;
    lookmask := lookmask shl 1;
    setmask := setmask shr 1;
  end;
  result := tempbit;
end;

// ch: channel of im = 0: B 1: G 2: R 3: grayscale

function TIEFtImage.newcomplex(im: TIEBitmap; ch: integer): PIECOMPLEX_IMAGE;
var
  i, j: integer;
  x: PIECOMPLEX_IMAGE;
  y: PIEsinglearray;
  xmax: single;
  pix: pbytearray;
  RedToGrayCoef, GreenToGrayCoef, BlueToGrayCoef: integer;
begin
  RedToGrayCoef   := IEGlobalSettings().RedToGrayCoef;
  GreenToGrayCoef := IEGlobalSettings().GreenToGrayCoef;
  BlueToGrayCoef  := IEGlobalSettings().BlueToGrayCoef;
  getmem(x, (im.Height * sizeof(PIEsinglearray)));
  getmem(y, (sizeof(single) * im.Height * im.Width * 2));
  for i := 0 to im.Height - 1 do
    x^[i] := @(y^[i * FFTN * 2]);
  xmax := 1;
  for i := 0 to im.Height - 1 do
  begin
    pix := im.Scanline[i];
    if ch = 3 then
    begin
      // convert to grayscale
      for j := 0 to im.Width - 1 do
      begin
        with PRGB(pix)^ do
          x^[i]^[j] := (r * RedToGrayCoef + g * GreenToGrayCoef + b * BlueToGrayCoef) div 100;
        x^[i]^[j + im.Width] := 0.0;
        if (x^[i]^[j] > xmax) then
          xmax := x^[i]^[j];
        inc(PRGB(pix));
      end;
    end
    else
    begin
      // rgb
      for j := 0 to im.Width - 1 do
      begin
        x^[i]^[j] := pix^[ch];
        x^[i]^[j + im.Width] := 0.0;
        if (x^[i]^[j] > xmax) then
          xmax := x^[i]^[j];
        inc(pbyte(pix), 3);
      end;
    end;
  end;
  if NORMALIZE then
    for i := 0 to im.Height - 1 do
      for j := 0 to im.Width - 1 do
        x^[i]^[j] := x^[i]^[j] / xmax;
  result := x;
end;

function TIEFtImage.dupcomplex(im: PIECOMPLEX_IMAGE): PIECOMPLEX_IMAGE;
var
  i: integer;
  x: PIECOMPLEX_IMAGE;
  y: PIEsinglearray;
begin
  getmem(x, (FFTN * sizeof(PIEsinglearray)));
  getmem(y, (sizeof(single) * FFTN * FFTN * 2));
  x^[0] := y;
  for i := 1 to FFTN - 1 do
    x^[i] := @(y^[i * 2 * FFTN]);
  for i := 0 to FFTN - 1 do
    move(im^[i]^[0], x^[i]^[0], 2 * FFTN * sizeof(single));
  result := x;
end;

procedure TIEFtImage.pairsort(arr: PIEsinglearray; iarr: pdwordarray; n: integer);
begin
  fqsort(arr, iarr, 0, n - 1);
end;

procedure TIEFtImage.fqsort(arr: PIEsinglearray; iarr: pdwordarray; l: integer; r: integer);
var
  i, j: integer;
  k: dword;
  x, w: single;
begin
  i := l;
  j := r;
  x := arr^[(l + r) div 2];
  repeat
    while (arr^[i] < x) do
      inc(i);
    while (x < arr^[j]) do
      dec(j);
    if (i <= j) then
    begin
      w := arr^[i];
      arr^[i] := arr^[j];
      arr^[j] := w;
      k := iarr^[i];
      iarr^[i] := iarr^[j];
      iarr^[j] := k;
      inc(i);
      dec(j);
    end;
  until not (i <= j);
  if (l < j) then
    fqsort(arr, iarr, l, j);
  if (i < r) then
    fqsort(arr, iarr, i, r);
end;

function TIEFtImage.GetComplexImage(x, y: integer): TIEComplexColor;
begin
  zeromemory(@result, sizeof(TIEComplexColor));
  with result do
  begin
    if assigned(fftr) then
    begin
      real_Red := @fftr^[y]^[x];
      imag_Red := @fftr^[y]^[x + FFTN];
    end;
    if assigned(fftg) then
    begin
      real_Green := @fftg^[y]^[x];
      imag_Green := @fftg^[y]^[x + FFTN];
    end;
    if assigned(fftb) then
    begin
      real_Blue := @fftb^[y]^[x];
      imag_Blue := @fftb^[y]^[x + FFTN];
    end;
    if assigned(fftgray) then
    begin
      real_Gray := @fftgray^[y]^[x];
      imag_Gray := @fftgray^[y]^[x + FFTN];
    end;
  end;
end;

procedure TIEFtImage.HiPass(radius: integer);
var
  cc, i, j, dist, c1: integer;
begin
  cc := FFTN div 2;
  for i := 0 to FFTN - 1 do
  begin
    c1 := (i - cc) * (i - cc);
    for j := 0 to FFTN - 1 do
    begin
      dist := trunc(sqrt((c1 + (j - cc) * (j - cc))));
      if (dist <= radius) then
        with ComplexPixel[j, i] do
          case fImageType of
            ieitRGB:
              begin
                real_Red^ := 0;
                imag_Red^ := 0;
                real_Green^ := 0;
                imag_Green^ := 0;
                real_Blue^ := 0;
                imag_Blue^ := 0;
              end;
            ieitGrayscale:
              begin
                real_Gray^ := 0;
                imag_Gray^ := 0;
              end;
          end;
    end;
  end;
end;

procedure TIEFtImage.LoPass(radius: integer);
var
  cc, i, j, dist, c1: integer;
begin
  cc := FFTN div 2;
  for i := 0 to FFTN - 1 do
  begin
    c1 := (i - cc) * (i - cc);
    for j := 0 to FFTN - 1 do
    begin
      dist := trunc(sqrt((c1 + (j - cc) * (j - cc))));
      if (dist > radius) then
        with ComplexPixel[j, i] do
          case fImageType of
            ieitRGB:
              begin
                real_Red^ := 0;
                imag_Red^ := 0;
                real_Green^ := 0;
                imag_Green^ := 0;
                real_Blue^ := 0;
                imag_Blue^ := 0;
              end;
            ieitGrayscale:
              begin
                real_Gray^ := 0;
                imag_Gray^ := 0;
              end;
          end;
    end;
  end;
end;

// set complex region x1,y1...x2,y2
procedure TIEFtImage.ClearZone(x1, y1, x2, y2: integer; vreal, vimag: single);
var
  x, y: integer;
begin
  for x := x1 to x2 do
    for y := y1 to y2 do
      with ComplexPixel[x, y] do
        case fImageType of
          ieitRGB:
            begin
              real_Red^ := vreal;
              imag_Red^ := vimag;
              real_Green^ := vreal;
              imag_Green^ := vimag;
              real_Blue^ := vreal;
              imag_Blue^ := vimag;
            end;
          ieitGrayscale:
            begin
              real_Gray^ := vreal;
              imag_Gray^ := vimag;
            end;
        end;
end;

procedure TIEFtImage.ClearZone(mask: TIEMask; vreal: single=0; vimag: single=0);
var
  x, y: integer;
begin
  for x := mask.X1 to mask.X2 do
    for y := mask.Y1 to mask.Y2 do
      if mask.GetPixel(x, y)>0 then
        with ComplexPixel[x, y] do
          case fImageType of
            ieitRGB:
              begin
                real_Red^ := vreal;
                imag_Red^ := vimag;
                real_Green^ := vreal;
                imag_Green^ := vimag;
                real_Blue^ := vreal;
                imag_Blue^ := vimag;
              end;
            ieitGrayscale:
              begin
                real_Gray^ := vreal;
                imag_Gray^ := vimag;
              end;
          end;
end;

{$endif}  // IEINCLUDEFFT

end.
