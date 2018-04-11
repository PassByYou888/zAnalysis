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

unit QRImageEn;

interface

{$I ie.inc}

uses Windows, Messages, classes, Graphics, ImageEnView, ImageEnio,
     hyiedefs, QuickRpt, hyieutils, ieview;

type
  TQRImageEn = class(TQRPrintable)
   private
    fCenter: boolean;	// centra immagine se più piccola della client
    fFit: boolean;
    fZoom: integer;
    fViewX: integer;
    fViewY: integer;
    fBitmapInfoHeader256: TBitmapInfoHeader256;	// usato internamente da PaintTo e DrawTo
    fHDrawDib: HDRAWDIB;		// per disegno su schermo
    fShadowSize: integer;
    fShadowType: TIEShadowType;
    fGammaCorrection: double;
    fBitmap: TBitmap;				// riferimento alla bitmap (se fImageEnView è valido vale FImageEnView.bitmap)
    fImageEnView: TIEView;		// riferimento a TIEView (fbitmap=fimageenview.bitmap)
    BitmapChangeHandle: pointer;  // bitmap change index (-1=nothing)
    fImageBorder: boolean;      // per bordo a filo immagine
    fImageShadow: boolean;		// ombra sul bordo dell'immagine (false=ombra sul frame)
    procedure FitZoom(bmpww, bmphh, ww, hh: integer);
    procedure SetAttachedBitmap(atBitmap: TBitmap);
    procedure SetAttachedImageEn(atImageEn: TIEView);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnBitmapChange(Sender: TObject; destroying: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Print(OfsX, OfsY : integer); override;
    property AttachedBitmap: TBitmap read fBitmap write SetAttachedBitmap;
    property Bitmap: TBitmap read fBitmap write fBitmap;
  published
    property Fit: boolean read fFit write fFit default true;
    property Center: boolean read fCenter write fCenter default true;
    property ShadowSize: integer read fShadowSize write fShadowSize default 8;
    property ShadowType: TIEShadowType read fShadowType write fShadowType default iestSmooth2;
    property GammaCorrection: double read fGammaCorrection write fGammaCorrection;
    property AttachedImageEn: TIEView read fImageEnView write SetAttachedImageEn;
    property ImageBorder: boolean read fImageBorder write fImageBorder default true;
    property ImageShadow: boolean read fImageShadow write fImageShadow default true;
   end;

{$ifndef IEREGISTERQR}
procedure Register;
{$endif}

implementation

uses Math, QRDBImageEn, ImageEnProc;


{$ifndef IEREGISTERQR}
procedure Register;
begin
  RegisterComponents('ImageEn', [TQRImageEn]);
end;
{$endif}

/////////////////////////////////////////////////////////////////////////////////////
constructor TQRImageEn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ZeroMemory(@fBitmapInfoHeader256, sizeof(TBitmapInfoHeader256));
  fZoom := 100;
  fCenter := True;
  fFit := True;
  fViewX := 0;
  fViewY := 0;
  fHDrawDib := IEDrawDibOpen;
  fShadowSize := 8;
  fShadowType := iestSmooth2;
  fGammaCorrection := 1;
  fBitmap := nil;
  fImageEnView := nil;
  fImageBorder := true;
  fImageShadow := true;
  BitmapChangeHandle := nil;
end;

/////////////////////////////////////////////////////////////////////////////////////
destructor TQRImageEn.Destroy;
begin
  IEDrawDibClose(fHDrawDib);
  if assigned(fImageEnView) then
     fImageEnView.RemoveBitmapChangeEvent(BitmapChangeHandle);
  inherited Destroy;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRImageEn.Paint;
begin
  with Canvas do
  begin
    Pen.Color := Frame.Color;
    Pen.Width := Frame.Width;
    Pen.Style := Frame.Style;
    MoveTo(0, 0);
    LineTo(width, height);
    MoveTo(0, height);
    LineTo(width, 0);
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure DoGammaCorrection(Gamma: double; fBitmap: TBitmap);
const
  Inv255 = 1.0 / 255;
var
  i, x, y: Integer;
  px: PRGB;
  InvGamma: double;
  lut: array [0..255] of byte;
begin
  InvGamma := 1.0/Gamma;
  // Build LUT
  for i := 0 to 255 do
     lut[i] := blimit(round(255*Power(i*Inv255, InvGamma)));
  //
  for y := 0 to fBitmap.Height-1 do
  begin
    px := fBitmap.Scanline[y];
    for x := 0 to fBitmap.Width-1 do
    begin
      with px^ do
      begin
        r := lut[r];
        g := lut[g];
        b := lut[b];
      end;
      inc(px);
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRImageEn.Print(OfsX, OfsY : integer);
var
  XBitmap: TBitmap;
  CalcLeft, CalcTop, CalcRight, CalcBottom : integer;
  fOffX, fOffY: integer;  // inizio visualizzazione (solo per imm. centrate)
  fExtX, fExtY: integer;  // estensione visualizzazione
  fZZWW, fZZHH: integer;  // dimensioni bitmap zoommata
  ww, hh: integer;  // width & height
  o1x, o1y, o2x, o2y: integer;
  rr: double;
begin
  if not assigned(fBitmap) then
     exit;
  XBitmap := TBitmap.Create;
  IECopyBitmap(fBitmap, XBitmap);
  if (XBitmap.Width>0) and (XBitmap.Height>0) then
  begin
    if fGammaCorrection<>1 then
      DoGammaCorrection(fGammaCorrection, XBitmap);
    with ParentReport.QRPrinter do
    begin
      CalcLeft := XPos(OfsX + Size.Left)+1;
      if Frame.DrawLeft then
        inc(CalcLeft, Frame.Width);
      CalcTop := YPos(OfsY + Size.Top)+1;
      if Frame.DrawTop then
        inc(CalcTop, Frame.Width);
      CalcRight := XPos(OfsX + Size.Left + Size.Width)-1;
      if Frame.DrawRight then
        dec(CalcRight, Frame.Width);
      CalcBottom := YPos(OfsY + Size.Top + Size.Height)-1;
      if Frame.DrawBottom then
        dec(CalcBottom, Frame.Width);
      ww := CalcRight-CalcLeft+1;
      hh := CalcBottom-CalcTop+1;
      //
      if fFit then
        FitZoom(XBitmap.Width, XBitmap.Height, ww, hh)
      else
        fZoom := 100;
      fZZWW := round(fzoom*(Xbitmap.width/100));   // nuova width della bitmap
      fZZHH := round(fzoom*(Xbitmap.height/100));  // nuova height della bitmap
      fOffX := 0; fOffY := 0;
      fExtX := imin(fZZWW, ww);  			 // width destinazione bitmap
      fExtY := imin(fZZHH, hh);			 // height destinazione bitmap
      if fCenter then
      begin
        // centra immagine
        if fExtx<ww then
          fOffX := (ww-fExtx) div 2;
        if fExty<hh then
          fOffY := (hh-fExty) div 2;
      end;
      //
      o1x := 0; o2x := 0; o1y := 0; o2y := 0;
      if fZZWW<>0 then
      begin
        rr := Xbitmap.width/fZZWW;
        o1x := round(fViewX*rr);
        o2x := round(fExtx*rr);
        if (o1x+o2x)>XBitmap.Width then
          dec(o2x);
      end;
      if fZZHH<>0 then
      begin
        rr := Xbitmap.height/fZZHH;
        o1y := round(fViewY*rr);
        o2y := round(fExty*rr);
        if (o1y+o2y)>XBitmap.Height then
          dec(o2y);
      end;
      with fBitmapInfoHeader256 do
      begin
        biSize := sizeof(TBitmapInfoHeader);
        biWidth := Xbitmap.width;
        biHeight := Xbitmap.height;
        biPlanes := 1;
        if Xbitmap.pixelformat=pf1bit then
        begin
          biBitCount := 1;
          // assegna colori bianco e nero (la Palette[0] è tutta zero...)
          Palette[1].rgbRed := 255;
          Palette[1].rgbGreen := 255;
          Palette[1].rgbBlue := 255;
        end
        else
          biBitCount := 24;
        biCompression := BI_RGB;
      end;
      inc(fOffX, CalcLeft);
      inc(fOffY, CalcTop);
      IEDrawDibDraw(fHDrawDib, canvas.handle, fOffX, fOffY, fExtx, fExty, PBitmapInfoHeader(@fBitmapInfoHeader256)^, Xbitmap.ScanLine[Xbitmap.height-1], o1x, o1y, o2x, o2y, 0);
      with Canvas do
      begin
        Pen.Color := Frame.Color;
        Pen.Width := Frame.Width;
        Pen.Style := Frame.Style;
        // shadow
        if fShadowSize>0 then
        begin
          Brush.Color := clBlack;
          Brush.Style := bsSolid;
          if fImageShadow then
          begin
            // ombra sull'immagine
            IERightShadow(Canvas, nil, fOffX+fExtx, fOffY, fOffX+fExtx+fShadowSize+2, fOffY+fExty+fShadowSize, fShadowType, clWhite);
            IEBottomShadow(Canvas, nil, fOffX, fOffY+fExty, fOffX+fExtx+fShadowSize, fOffY+fExty+fShadowSize+1, fShadowType, clWhite);
          end
          else
          begin
            // ombra sul frame
            IERightShadow(Canvas, nil, CalcRight+2, CalcTop, CalcRight+2+fShadowSize, CalcBottom+fShadowSize, fShadowType, clWhite);
            IEBottomShadow(Canvas, nil, CalcLeft, CalcBottom+1, CalcRight+fShadowSize, CalcBottom+1+fShadowSize, fShadowType, clWhite);
          end;
          if fImageBorder then
          begin
            // bordo sull'immagine
            moveto(fOffX, fOffY);
            lineto(fOffX+fExtx-1, fOffY);
            lineto(fOffX+fExtx-1, fOffY+fExty-1);
            lineto(fOffX, fOffY+fExty-1);
            lineto(fOffX, fOffY);
          end;
        end;
      end;
    end;
  end;
  XBitmap.free;
  if (not fImageBorder) and (not fImageShadow) then
    inherited;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRImageEn.FitZoom(bmpww, bmphh, ww, hh: integer);
begin
  if (bmpww<>0) and (bmphh<>0) then
    fZoom := imin( trunc(ww/(bmpww/100)), trunc(hh/(bmphh/100)) );
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRImageEn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent=fImageEnView) and (Operation=opRemove) then
  begin
    fImageEnView.RemoveBitmapChangeEvent(BitmapChangeHandle);
     fImageEnView := nil;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRImageEn.SetAttachedBitmap(atBitmap: TBitmap);
begin
  if assigned(fImageEnView) then
    fImageEnView.RemoveBitmapChangeEvent(BitmapChangeHandle);  // rimuove precedente, se c'è
  if (not assigned(atBitmap)) and assigned(fImageEnView) then
    exit;  // situazione senza significato
  fBitmap := atBitmap;
  if assigned(fBitmap) then
    fImageEnView := nil;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRImageEn.SetAttachedImageEn(atImageEn: TIEView);
begin
  if assigned(fImageEnView) then
    fImageEnView.RemoveBitmapChangeEvent(BitmapChangeHandle);  // rimuove precedente, se c'è
  fImageEnView := atImageEn;
  if assigned(fImageEnView) then
  begin // fImageEnView ora potrebbe anche essere nil
    fBitmap := fImageEnView.Bitmap;
    fImageEnView.FreeNotification(self);
    BitmapChangeHandle := fImageEnView.RegisterBitmapChangeEvent(OnBitmapChange);
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
// riassegna fBitmap (chiamato dal meccanismo RegisterBitmapChangeEvent)
procedure TQRImageEn.OnBitmapChange(Sender: TObject; destroying: boolean);
begin
  if destroying then
    fImageEnView := nil
  else
  if assigned(fImageEnView) then
    fBitmap := fImageEnView.Bitmap;
end;


end.

