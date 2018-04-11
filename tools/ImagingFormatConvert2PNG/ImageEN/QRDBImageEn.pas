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
File version 1001
*)


unit QRDBImageEn;

{$I ie.inc}

{$ifdef IEINCLUDEDB}

interface


uses Windows, Messages, classes, Graphics, Db, dbctrls, ImageEnView, ImageEnio,
     hyiedefs, DBImageEn, QuickRpt, QRImageEn, ievect, hyieutils, ieview;

type

  TQRDBImageEn = class(TQRPrintable)
     private
         FAutoDisplay: Boolean;
         FDataLink: TFieldDataLink;
         FPictureLoaded: Boolean;
         fDataFieldImageFormat: TDataFieldImageFormat;
         fStreamHeaders: boolean;  // attiva/disattiva caricamento header negli streams
         fCenter: boolean;  // centra immagine se più piccola della client
         fFit: boolean;
         fZoom: integer;
         fViewX: integer;
         fViewY: integer;
         fBitmapInfoHeader256: TBitmapInfoHeader256;  // usato internamente da PaintTo e DrawTo
         fHDrawDib: HDRAWDIB;    // per disegno su schermo
         fShadowSize: integer;
         fShadowType: TIEShadowType;
         fGammaCorrection: double;
         fImageBorder: boolean;      // per bordo a filo immagine
         fImageShadow: boolean;    // ombra sul bordo dell'immagine (false=ombra sul frame)
         fPrintObjects: boolean;
         fAbsolutePath: string;
         procedure SetAutoDisplay(Value: Boolean);
         function GetDataField: string;
         function GetField: TField;
         procedure SetDataField(const Value: string);
         procedure FitZoom(bmpww, bmphh, ww, hh: integer);
         procedure SetDataSource(Value: TDataSource);
         function GetDataSource: TDataSource;
      protected
         procedure Notification(AComponent: TComponent;Operation: TOperation); override;
         procedure DataChange(Sender: TObject); virtual;
         function GetDataFieldImageFormat: TDataFieldImageFormat; virtual;
         procedure LoadPictureEx(ffImageEnIO: TImageEnIO; ffImageEnVect: TImageEnVect);
         procedure SetAbsolutePath(const v: string);
      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Paint; override;
         procedure LoadPicture; virtual;
         function LoadedFieldImageFormat: TDataFieldImageFormat; virtual;
         property PictureLoaded: boolean read fPictureLoaded;
         property Field: TField read GetField;
         procedure Print(OfsX, OfsY : integer); override;
      published
        property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
        property DataField: string read GetDataField write SetDataField;
        property DataFieldImageFormat: TDataFieldImageFormat read GetDataFieldImageFormat default ifBitmap;
        property StreamHeaders: boolean read fStreamHeaders write fStreamHeaders default true;
        property Fit: boolean read fFit write fFit default true;
        property Center: boolean read fCenter write fCenter default true;
        property ShadowSize: integer read fShadowSize write fShadowSize default 8;
        property DataSource: TDataSource read GetDataSource write SetDataSource;
        property ShadowType: TIEShadowType read fShadowType write fShadowType default iestSmooth2;
        property GammaCorrection: double read fGammaCorrection write fGammaCorrection;
        property ImageBorder: boolean read fImageBorder write fImageBorder default true;
        property ImageShadow: boolean read fImageShadow write fImageShadow default true;
        property PrintObjects: boolean read fPrintObjects write fPrintObjects default false;
        property AbsolutePath: string read fAbsolutePath write SetAbsolutePath;
   end;

{$ifndef IEREGISTERQR}
procedure Register;
{$endif}

implementation

uses Math, dbtables, sysutils, imageenproc;


{$ifndef IEREGISTERQR}
procedure Register;
begin
  RegisterComponents('ImageEn', [TQRDBImageEn]);
end;
{$endif}

/////////////////////////////////////////////////////////////////////////////////////
procedure TQrDBImageEn.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then
      LoadPicture;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
constructor TQRDBImageEn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ZeroMemory(@fBitmapInfoHeader256, sizeof(TBitmapInfoHeader256));
  fStreamHeaders := true;
  fDataFieldImageFormat := ifBitmap;
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  fZoom := 100;
  fCenter := True;
  fFit := True;
  fViewX := 0;
  fViewY := 0;
  fHDrawDib := IEDrawDibOpen;
  fShadowSize := 8;
  fShadowType := iestSmooth2;
  fGammaCorrection := 1;
  fImageBorder := true;
  fImageShadow := true;
  fPrintObjects := false;
  fAbsolutePath := '';
end;

/////////////////////////////////////////////////////////////////////////////////////
destructor TQRDBImageEn.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  IEDrawDibClose(fHDrawDib);
  inherited Destroy;
end;

/////////////////////////////////////////////////////////////////////////////////////
function TQRDBImageEn.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRDBImageEn.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRDBImageEn.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRDBImageEn.DataChange(Sender: TObject);
begin
  FPictureLoaded := False;
  if not assigned(fDataLink) then exit;
  if not assigned(fDataLink.DataSource) then exit;
  if not assigned(fDataLink.DataSource.DataSet) then exit;
  if not FDataLink.DataSource.DataSet.Active then exit;
  if FAutoDisplay then LoadPicture;
end;

/////////////////////////////////////////////////////////////////////////////////////
// Carica immagine da fdatalink.field senza controllare fPictureLoaded
// non assegna fDataFieldImageFormat
procedure TQRDBImageEn.LoadPictureEx(ffImageEnIO: TImageEnIO; ffImageEnVect: TImageEnVect);
var
  BlobStream: TStream;
  ifm: TDataFieldImageFormat;
  ss: string;
begin
  ffImageEnIO.StreamHeaders := fStreamHeaders or fPrintObjects;
  if (FDataLink.Field is TBlobField) and ((FDataLink.Field as TBlobField).BlobSize>0) then
  begin
     BlobStream := nil;
     ifm := LoadedFieldImageFormat;
     try
      BlobStream := FDataLink.DataSource.DataSet.CreateBlobStream(TBlobField(FDataLink.Field), bmRead);
      if assigned(ffImageEnIO.Bitmap) then
      begin
         case ifm of
            ifBitmap: ffImageEnIO.LoadFromStreamBMP(BlobStream);
            ifJpeg: ffImageEnIO.LoadFromStreamJpeg(BlobStream);
            ifGif: ffImageEnIO.LoadFromStreamGif(BlobStream);
            ifPCX: ffImageEnIO.LoadFromStreamPCX(BlobStream);
            ifTIFF: ffImageEnIO.LoadFromStreamTIFF(BlobStream);
            {$ifdef IEINCLUDEPNG}
            ifPNG: ffImageEnIO.LoadFromStreamPNG(BlobStream);
            {$endif}
            ifTGA: ffImageEnIO.LoadFromStreamTGA(BlobStream);
            ifPXM: ffImageEnIO.LoadFromStreamPXM(BlobStream);
            ifICO: ffImageEnIO.LoadFromStreamICO(BlobStream);
            {$ifdef IEINCLUDEJPEG2000}
            ifJP2: ffImageEnIO.LoadFromStreamJP2(BlobStream);
            ifJ2K: ffImageEnIO.LoadFromStreamJ2K(BlobStream);
            {$endif}
            ifWBMP: ffImageEnIO.LoadFromStreamWBMP(BlobStream);
         end;
      end
      else
        ffImageEnIO.ParamsFromStream(BlobStream);
      if fPrintObjects then
      begin
        ffImageEnVect.RemoveAllObjects;
        ffImageEnVect.LoadFromStreamIEV(BlobStream);
      end;
    finally
      BlobStream.Free;
    end;
  end
  else
  if (FDataLink.Field is TStringField) then
  begin
    ss := TStringField(FDataLink.Field).Value;
    if (ss<>'') and (fileexists(fAbsolutePath+ss)) then
    begin
      ss := fAbsolutePath+ss;
      ifm := LoadedFieldImageFormat;
      case ifm of
        ifBitmap: ffImageEnIO.LoadFromFileBMP(ss);
        ifJpeg: ffImageEnIO.LoadFromFileJpeg(ss);
        ifGif: ffImageEnIO.LoadFromFileGif(ss);
        ifPCX: ffImageEnIO.LoadFromFilePCX(ss);
        ifTIFF: ffImageEnIO.LoadFromFileTIFF(ss);
        {$ifdef IEINCLUDEPNG}
        ifPNG: ffImageEnIO.LoadFromFilePNG(ss);
        {$endif}
        ifTGA: ffImageEnIO.LoadFromFileTGA(ss);
        ifPXM: ffImageEnIO.LoadFromFilePXM(ss);
        ifICO: ffImageEnIO.LoadFromFileICO(ss);
        {$ifdef IEINCLUDEJPEG2000}
        ifJP2: ffImageEnIO.LoadFromFileJP2(ss);
        ifJ2K: ffImageEnIO.LoadFromFileJ2K(ss);
        {$endif}
        ifWBMP: ffImageEnIO.LoadFromFileWBMP(ss);
      end;
      if fPrintObjects then
      begin
        ffImageEnVect.RemoveAllObjects;
        if fileexists(ss+'.iev') then
          ffImageEnVEct.LoadFromFileIEV(ss+'.iev');
      end;
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
// Carica immagine da field
procedure TQRDBImageEn.LoadPicture;
begin
  if (not FPictureLoaded) and (FDataLink.Field is TBlobField) and ((FDataLink.Field as TBlobField).BlobSize>0) then
    fDataFieldImageFormat := LoadedFieldImageFormat;
end;

/////////////////////////////////////////////////////////////////////////////////////
// restituisce il formato dell'immagine memorizzata in fDataLink.Field
function TQRDBImageEn.LoadedFieldImageFormat: TDataFieldImageFormat;
var
  BlobStream: TStream;
  ss: string;
begin
  result := ifUnknown;
  if not FAutoDisplay then
     exit;
  if FDataLink.Field is TBlobField then
  begin
     BlobStream := nil;
    try
      BlobStream := FDataLink.DataSource.DataSet.CreateBlobStream(TBlobField(FDataLink.Field), bmRead);
      case FindStreamFormat(BlobStream) of
        ioBMP:  result := ifBitmap;
        ioJPEG: result := ifJpeg;
        ioGIF:  result := ifGIF;
        ioPCX:  result := ifPCX;
        ioTIFF: result := ifTIFF;
         {$ifdef IEINCLUDEPNG}
        ioPNG:  result := ifPNG;
         {$endif}
         ioTGA:  result := ifTGA;
         ioPXM:  result := ifPXM;
         ioICO:  result := ifICO;
         ioJP2:  result := ifJP2;
         ioJ2K:  result := ifJ2K;
         ioWBMP: result := ifWBMP;
      end;
    finally
      BlobStream.free;
    end;
  end
  else
  if FDataLink.Field is TStringField then
  begin
    ss := TStringField(FDataLink.Field).Value;
    if (ss<>'') and (fileexists(fAbsolutePath+ss)) then
    begin
      case FindFileFormat(fAbsolutePath+ss, false) of
        ioBMP:  result := ifBitmap;
        ioJPEG: result := ifJpeg;
        ioGIF:  result := ifGIF;
        ioPCX:  result := ifPCX;
        ioTIFF: result := ifTIFF;
        {$ifdef IEINCLUDEPNG}
        ioPNG:  result := ifPNG;
        {$endif}
        ioTGA:  result := ifTGA;
        ioPXM:  result := ifPXM;
        ioICO:  result := ifICO;
        ioJP2:  result := ifJP2;
        ioJ2K:  result := ifJ2K;
        ioWBMP: result := ifWBMP;
      end;
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////////
function TQRDBImageEn.GetDataFieldImageFormat: TDataFieldImageFormat;
begin
  result := fDataFieldImageFormat;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRDBImageEn.Paint;
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
function TQRDBImageEn.GetField: TField;
begin
  Result := FDataLink.Field;
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
procedure TQRDBImageEn.Print(OfsX, OfsY : integer);
var
  fImageEnIO: TImageEnIO;
  fBitmap: TBitmap;
  fImageEnVect: TImageEnVect;
  CalcLeft, CalcTop, CalcRight, CalcBottom : integer;
  fOffX, fOffY: integer;  // inizio visualizzazione (solo per imm. centrate)
  fExtX, fExtY: integer;  // estensione visualizzazione
  fZZWW, fZZHH: integer;  // dimensioni bitmap zoommata
  ww, hh: integer;  // width & height
  o1x, o1y, o2x, o2y: integer;
  rr: double;
begin
   if assigned(fDataLink.DataSource) and FDataLink.DataSource.DataSet.Active then
  begin
     fImageEnVect := TImageEnVect.Create(self);
    fImageEnVect.Parent := self;
     fBitmap := fImageEnVect.Bitmap;
     fImageEnIO := TImageEnIO.Create(self);
    fImageEnIO.AttachedBitmap := fBitmap;
    LoadPictureEx(fImageEnIO, fImageEnVect);
    if fPrintObjects then
    begin
      fImageEnVect.Update;
      fImageEnVect.CopyObjectsToBack(true);
    end;
    if (fBitmap.Width>0) and (fBitmap.Height>0) then
    begin
      if fGammaCorrection<>1 then
        DoGammaCorrection(fGammaCorrection, fBitmap);
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
          FitZoom(fBitmap.Width, fBitmap.Height, ww, hh)
        else
          fZoom := 100;
        fZZWW := round(fzoom*(fbitmap.width/100));   // nuova width della bitmap
        fZZHH := round(fzoom*(fbitmap.height/100)); // nuova height della bitmap
        fOffX := 0; fOffY := 0;
        fExtX := imin(fZZWW, ww);  			 // width destinazione bitmap
        fExtY := imin(fZZHH, hh);				 // height destinazione bitmap
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
          rr := fbitmap.width/fZZWW;
          o1x := round(fViewX*rr);
          o2x := round(fExtx*rr);
          if (o1x+o2x)>fBitmap.Width then
            dec(o2x);
        end;
        if fZZHH<>0 then
        begin
          rr := fbitmap.height/fZZHH;
          o1y := round(fViewY*rr);
          o2y := round(fExty*rr);
          if (o1y+o2y)>fBitmap.Height then
            dec(o2y);
        end;
        with fBitmapInfoHeader256 do
        begin
          biSize := sizeof(TBitmapInfoHeader);
          biWidth := fbitmap.width;
          biHeight := fbitmap.height;
          biPlanes := 1;
          if fbitmap.pixelformat=pf1bit then
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
        IEDrawDibDraw(fHDrawDib, canvas.handle, fOffX, fOffY, fExtx, fExty, PBitmapInfoHeader(@fBitmapInfoHeader256)^, fbitmap.ScanLine[fbitmap.height-1], 
                   o1x, o1y, o2x, o2y, 0);
      end;
      with Canvas do
      begin
        Pen.Color := Frame.Color;
        Pen.Width := Frame.Width;
        Pen.Style := Frame.Style;
        // shadow
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
    fImageEnIO.free;
    fImageEnVect.free;
  end;
  if (not fImageBorder) and (not fImageShadow) then
    inherited;
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRDBImageEn.FitZoom(bmpww, bmphh, ww, hh: integer);
begin
  if (bmpww<>0) and (bmphh<>0) then
    fZoom := imin( trunc(ww/(bmpww/100)), trunc(hh/(bmphh/100)) );
end;

/////////////////////////////////////////////////////////////////////////////////////
procedure TQRDBImageEn.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

/////////////////////////////////////////////////////////////////////////////////////
function TQRDBImageEn.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TQRDBImageEn.SetAbsolutePath(const v: string);
begin
  fAbsolutePath := v;
  FPictureLoaded := false;
  LoadPicture;
end;


{$else}

interface
implementation

{$endif}


end.
