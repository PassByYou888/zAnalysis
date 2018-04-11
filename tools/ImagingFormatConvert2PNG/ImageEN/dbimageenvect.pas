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

unit dbimageenvect;

{$R-}
{$Q-}

{$I ie.inc}

{$IFDEF IEINCLUDEDB}

interface

uses Windows, Messages, classes, Graphics, Db, dbctrls, ImageEnView, ImageEnio, dbimageen, ievect, hyiedefs, hyieutils;

type

{!!
<FS>TImageEnDBVect

<FM>Description<FN>
TImageEnDBVect is a descendant of <A TImageEnVect>, but it can connected to a TDataset object to store/load images (BMP, PCX, GIF, JPEG, TIFF, PNG and others) and vectorial objects in Blob field.

It is similar to the TDBImage component of Delphi.
You can specify particular file format parameters through <A TImageEnDBVect.IOParams> property, or executing <A TImageEnDBVect.DoIOPreview> method.
You can attach a <A TImageEnProc> component for image processing the image contained in the Blob.

Also see methods and properties of <A TImageEnVect>.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnDBView.AbsolutePath></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.AutoDisplay></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.DataField></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.DataFieldImageFormat></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.DataSource></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnDBView.DoIOPreview></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnDBView.Field></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnDBView.IOParams></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.IOPreviewsParams></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.JpegQuality></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnDBView.LoadedFieldImageFormat></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnDBView.LoadPicture></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.PreviewFont></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.PreviewFontEnabled></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.ReadOnly></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TImageEnDBVect.OnUnableToLoadImage></C> </R>
</TABLE>

!!}
  TImageEnDBVect = class(TImageEnVect, IIELoadPicture)
  private
    FAutoDisplay: Boolean;
    FDataLink: TFieldDataLink;
    FPictureLoaded: Boolean;
    fDataFieldImageFormat: TDataFieldImageFormat;
    fDoImageChange: boolean; // se true viene eseguita ImageChange
    fAbsolutePath: string;
    fIsInsideDbCtrl: boolean;
    fOnUnableToLoadImage: TUnableToLoadImageEvent;
    procedure SetAutoDisplay(Value: Boolean);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure SetJpegQuality(q: integer);
    function GetJpegQuality: integer;
    function GetIOParams: TIOParamsVals;
    function GetIOPreviewsParams: TIOPreviewsParams;
    procedure SetIOPreviewsParams(v: TIOPreviewsParams);
    procedure SetPreviewFont(f: TFont);
    function GetPreviewFont: TFont;
    procedure SetPreviewFontEnabled(Value: Boolean);
    function GetPreviewFontEnabled: Boolean;
    procedure SetAbsolutePath(const v: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DataChange(Sender: TObject); virtual;
    procedure UpdateData(Sender: TObject); virtual;
    function GetDataFieldImageFormat: TDataFieldImageFormat; virtual;
    procedure SetDataFieldImageFormat(v: TDataFieldImageFormat); virtual;
    procedure LoadPictureEx(ffImageEnIO: TImageEnIO);
    procedure DoVectorialChanged; override;
    function InsideDBCtrl: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintToEx(ABitmap: TIEBitmap; ABitmapScanline: ppointerarray; UpdRect: PRect; drawBackground: boolean; drawGadgets: boolean); override;
    procedure Paint; override;
    procedure ImageChange; override;
    property Field: TField read GetField;
    procedure LoadPicture; virtual;
    function LoadedFieldImageFormat(): TDataFieldImageFormat; virtual;
    property IOParams: TIOParamsVals read GetIOParams;
{$IFDEF IEINCLUDEDIALOGIO}
    function DoIOPreview: boolean;
{$ENDIF}
    property PictureLoaded: boolean read fPictureLoaded;
    property AbsolutePath: string read fAbsolutePath write SetAbsolutePath;
  published

{!!
<FS>TImageEnDBVect.AutoDisplay

<FM>Declaration<FC>
property AutoDisplay: Boolean;

<FM>Description<FN>
AutoDisplay determines whether to automatically display the contents of a graphic BLOB in the database image control.

If AutoDisplay is True (the default value), the image automatically displays new data when the underlying BLOB field changes (such as when moving to a new record).
If AutoDisplay is False, the image clears whenever the underlying BLOB field changes. To display the data, the user can double-click on the control or select it and press Enter. In addition, calling the LoadPicture method ensures that the control is showing data.
Change the value of AutoDisplay to False if the automatic loading of BLOB fields seems to take too long.
!!}
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;

    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property DataFieldImageFormat: TDataFieldImageFormat read GetDataFieldImageFormat write SetDataFieldImageFormat default ifBitmap;
    property JpegQuality: integer read GetJpegQuality write SetJpegQuality default 80;
    property IOPreviewsParams: TIOPreviewsParams read GetIOPreviewsParams write SetIOPreviewsParams default [];
    property PreviewFont: TFont read GetPreviewFont write SetPreviewFont;
    property PreviewFontEnabled: Boolean read GetPreviewFontEnabled write SetPreviewFontEnabled default false;
    property IsInsideDbCtrl: boolean read fIsInsideDbCtrl write fIsInsideDbCtrl default false;

{!!
<FS>TImageEnDBVect.OnUnableToLoadImage

<FM>Declaration<FC>
property OnUnableToLoadImage: <A TUnableToLoadImageEvent>;

<FM>Description<FN>
Occurs when TImageEnDBVect or TImageEnDBView fails to load an image from blob field.
!!}
    property OnUnableToLoadImage: TUnableToLoadImageEvent read fOnUnableToLoadImage write fOnUnableToLoadImage;

  end;


implementation

uses
  controls, giffilter, ImageEn, sysutils;

{$R-}



procedure TImageEnDBVect.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then
      LoadPicture;
  end;
end;



constructor TImageEnDBVect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetImageEnIO; // creates fImageEnIO;
  fAbsolutePath := '';
  fDataFieldImageFormat := ifBitmap;
  FAutoDisplay := True;
  fDoImageChange := true;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  SetJpegQuality(80);
  fIsInsideDbCtrl := false;
  fOnUnableToLoadImage := nil;
end;



destructor TImageEnDBVect.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;



{!!
<FS>TImageEnDBVect.DataSource

<FM>Declaration<FC>
property DataSource: TDataSource;

<FM>Description<FN>
DataSource links the image control to a dataset.

Use DataSource to link the image control to a dataset in which the data can be found.
To fully specify a database field for the image control, both the dataset and a field within that dataset must be defined.
Use the <A TImageEnDBVect.DataField> property to specify the particular field within the dataset.
!!}
function TImageEnDBVect.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;



procedure TImageEnDBVect.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;



{!!
<FS>TImageEnDBVect.DataField

<FM>Declaration<FC>
property DataField: string;

<FM>Description<FN>
DataField specifies the field from which the database image displays data.

Use DataField to bind the image control to a field in the dataset.
To fully specify a database field, both the dataset and the field within that dataset must be defined.
The <A TImageEnDBVect.DataSource> property of the image control specifies the dataset which contains the DataField. DataField should specify a graphic field.
!!}
function TImageEnDBVect.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;



procedure TImageEnDBVect.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;



{!!
<FS>TImageEnDBVect.ReadOnly

<FM>Declaration<FC>
property ReadOnly: Boolean;

<FM>Description<FN>
ReadOnly determines whether the user can change the contents of the field using the image control.

!!}
function TImageEnDBVect.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;



procedure TImageEnDBVect.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;



{!!
<FS>TImageEnDBVect.Field

<FM>Declaration<FC>
property Field: TField;

<FM>Description<FN>
Field is the TField component the database image is linked to.

Read-only

!!}
function TImageEnDBVect.GetField: TField;
begin
  Result := FDataLink.Field;
end;



procedure TImageEnDBVect.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;



procedure TImageEnDBVect.DataChange(Sender: TObject);
begin
  Clear;
  RemoveAllObjects;
  FPictureLoaded := False;
  if (not assigned(fDataLink.DataSource)) or (not assigned(FDataLink.DataSource.DataSet)) or (not FDataLink.DataSource.DataSet.Active) then
    exit;
  if FAutoDisplay then
    LoadPicture;
end;



procedure TImageEnDBVect.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  LoadPicture;
  inherited;
end;



procedure TImageEnDBVect.ImageChange;
begin
  inherited;
  if fDoImageChange then
  begin
    FDataLink.Modified;
    FPictureLoaded := True;
    Invalidate;
  end;
end;



procedure TImageEnDBVect.DoVectorialChanged;
begin
  ImageChange;
end;



procedure TImageEnDBVect.SetDataFieldImageFormat(v: TDataFieldImageFormat);
begin
  fDataFieldImageFormat := v;
  ImageChange;
end;


// save the image
procedure TImageEnDBVect.UpdateData(Sender: TObject);
var
  ms: TMemoryStream;
  ss: string;
begin
  if FDataLink.Field is TBlobField then
  begin
    fImageEnIO.StreamHeaders := (fDataFieldImageFormat <> ifTIFFwAnnot);  // ifTIFFwAnnot doesn't need StreamHeaders
    ms := TMemoryStream.Create();
    try
      case fDataFieldImageFormat of
        ifBitmap:     fImageEnIO.SaveToStreamBMP(ms);
        ifJpeg:       fImageEnIO.SaveToStreamJpeg(ms);
        ifGIF:        fImageEnIO.SaveToStreamGIF(ms);
        ifPCX:        fImageEnIO.SaveToStreamPCX(ms);
        ifTIFF,
        ifTIFFwAnnot:
          begin
            IO.Params.ImageEnAnnot.CopyFromTImageEnVect();
            fImageEnIO.SaveToStreamTIFF(ms);
          end;
{$IFDEF IEINCLUDEPNG}
        ifPNG:        fImageEnIO.SaveToStreamPNG(ms);
{$ENDIF}
        ifTGA:        fImageEnIO.SaveToStreamTGA(ms);
        ifPXM:        fImageEnIO.SaveToStreamPXM(ms);
        ifICO:        fImageEnIO.SaveToStreamICO(ms);
{$IFDEF IEINCLUDEJPEG2000}
        ifJP2:        fImageEnIO.SaveToStreamJP2(ms);
        ifJ2K:        fImageEnIO.SaveToStreamJ2K(ms);
{$ENDIF}
        ifWBMP:       fImageEnIO.SaveToStreamWBMP(ms);
      end;
      if fDataFieldImageFormat <> ifTIFFwAnnot then
        SaveToStreamIEV(ms);
      ms.position := 0;
      (fdatalink.field as tblobfield).LoadFromStream(ms);
    finally
      ms.Free();
    end;
  end
  else
  if FDataLink.Field is TStringField then
  begin
    // path
    fImageEnIO.StreamHeaders := false;
    ss := string(TStringField(FDataLink.Field).Value);
    if (ss <> '') then
    begin
      ss := fAbsolutePath + ss;
      case fDataFieldImageFormat of
        ifBitmap:     fImageEnIO.SaveToFileBMP(ss);
        ifJpeg:       fImageEnIO.SaveToFileJpeg(ss);
        ifGIF:        fImageEnIO.SaveToFileGIF(ss);
        ifPCX:        fImageEnIO.SaveToFilePCX(ss);
        ifTIFF,
        ifTIFFwAnnot:
          begin
            IO.Params.ImageEnAnnot.CopyFromTImageEnVect();
            fImageEnIO.SaveToFileTIFF(ss);
          end;
{$IFDEF IEINCLUDEPNG}
        ifPNG:        fImageEnIO.SaveToFilePNG(ss);
{$ENDIF}
        ifTGA:        fImageEnIO.SaveToFileTGA(ss);
        ifPXM:        fImageEnIO.SaveToFilePXM(ss);
        ifICO:        fImageEnIO.SaveToFileICO(ss);
{$IFDEF IEINCLUDEJPEG2000}
        ifJP2:        fImageEnIO.SaveToFileJP2(ss);
        ifJ2K:        fImageEnIO.SaveToFileJ2K(ss);
{$ENDIF}
        ifWBMP:       fImageEnIO.SaveToFileWBMP(ss);
      end;
      if fDataFieldImageFormat <> ifTIFFwAnnot then
        SaveToFileIEV(ss + '.iev');
    end;
  end;
end;



// Load image from fdatalink.field without check fPictureLoaded
// Doesn't assign fDataFieldImageFormat
procedure TImageEnDBVect.LoadPictureEx(ffImageEnIO: TImageEnIO);
var
  ms: TMemoryStream;
  ifm: TDataFieldImageFormat;
  bg: THYIEGraphicHeader;
  ss: string;
  bSuccess: boolean;
begin
  bSuccess := false;
  try
  if (FDataLink.Field is TBlobField) and ((FDataLink.Field as TBlobField).BlobSize > 0) then
  begin
    fDoImageChange := false;
    ifm := LoadedFieldImageFormat();
    ffImageEnIO.StreamHeaders := (ifm <> ifTIFFwAnnot);  // ifTIFFwAnnot doesn't need StreamHeaders
    ms := TMemoryStream.Create();
    try
      (fdatalink.field as tblobfield).SaveToStream(ms);
      ms.position := 0;
      if ifm = ifUnknown then
      begin
        // try paradox graphic
        ms.read(bg, sizeof(THYIEGraphicHeader));
        if (bg.Count = 1) and (bg.HType = $0100) then
          ifm := ifBitmap
        else
          ms.position := 0;
      end;
      case ifm of
        ifBitmap:     ffImageEnIO.LoadFromStreamBMP(ms);
        ifJpeg:       ffImageEnIO.LoadFromStreamJpeg(ms);
        ifGIF:        ffImageEnIO.LoadFromStreamGIF(ms);
        ifPCX:        ffImageEnIO.LoadFromStreamPCX(ms);
        ifTIFF,
        ifTIFFwAnnot: ffImageEnIO.LoadFromStreamTIFF(ms);
{$IFDEF IEINCLUDEPNG}
        ifPNG:        ffImageEnIO.LoadFromStreamPNG(ms);
{$ENDIF}
        ifTGA:        ffImageEnIO.LoadFromStreamTGA(ms);
        ifPXM:        ffImageEnIO.LoadFromStreamPXM(ms);
        ifICO:        ffImageEnIO.LoadFromStreamICO(ms);
{$IFDEF IEINCLUDEJPEG2000}
        ifJP2:        ffImageEnIO.LoadFromStreamJP2(ms);
        ifJ2K:        ffImageEnIO.LoadFromStreamJ2K(ms);
{$ENDIF}
        ifWBMP:       ffImageEnIO.LoadFromStreamWBMP(ms);
        else
          Clear;
      end;
      bSuccess := not ffImageEnIO.Aborting;
      RemoveAllObjects;
      if ifm = ifTIFFwAnnot then
        IO.Params.ImageEnAnnot.CopyToTImageEnVect()
      else
        LoadFromStreamIEV(ms);
    finally
      ms.Free();
      fDoImageChange := true;
    end;
  end
  else
  if (FDataLink.Field is TStringField) then
  begin
    ffImageEnIO.StreamHeaders := false;
    ss := string(TStringField(FDataLink.Field).Value);
    if (ss <> '') and (IEFileExists(fAbsolutePath + ss)) then
    begin
      ss := fAbsolutePath + ss;
      ifm := LoadedFieldImageFormat;
      case ifm of
        ifBitmap:     ffImageEnIO.LoadFromFileBMP(ss);
        ifJpeg:       ffImageEnIO.LoadFromFileJpeg(ss);
        ifGIF:        ffImageEnIO.LoadFromFileGIF(ss);
        ifPCX:        ffImageEnIO.LoadFromFilePCX(ss);
        ifTIFF,
        ifTIFFwAnnot: ffImageEnIO.LoadFromFileTIFF(ss);
{$IFDEF IEINCLUDEPNG}
        ifPNG:        ffImageEnIO.LoadFromFilePNG(ss);
{$ENDIF}
        ifTGA:        ffImageEnIO.LoadFromFileTGA(ss);
        ifPXM:        ffImageEnIO.LoadFromFilePXM(ss);
        ifICO:        ffImageEnIO.LoadFromFileICO(ss);
{$IFDEF IEINCLUDEJPEG2000}
        ifJP2:        ffImageEnIO.LoadFromFileJP2(ss);
        ifJ2K:        ffImageEnIO.LoadFromFileJ2K(ss);
{$ENDIF}
        ifWBMP:       ffImageEnIO.LoadFromFileWBMP(ss);
        else
          Clear;
      end;
      bSuccess := not ffImageEnIO.Aborting;
      RemoveAllObjects;
      if ifm = ifTIFFwAnnot then
        IO.Params.ImageEnAnnot.CopyToTImageEnVect()
      else if IEFileExists(ss + '.iev') then
        LoadFromFileIEV(ss + '.iev');
      fDoImageChange := true;
    end;
  end;
  except
    bSuccess := false;
  end;
  if (not bSuccess) and assigned(fOnUnableToLoadImage) then
    fOnUnableToLoadImage(self, FDataLink.Field);
end;


{!!
<FS>TImageEnDBVect.LoadPicture

<FM>Declaration<FC>
procedure LoadPicture;

<FM>Description<FN>
LoadPicture loads the image stored in the field into the database image control.

!!}
procedure TImageEnDBVect.LoadPicture;
begin
  if (not FPictureLoaded) and
    (((FDataLink.Field is TBlobField) and ((FDataLink.Field as TBlobField).BlobSize > 0)) or
    (FDataLink.Field is TStringField)) then
  begin
    LoadPictureEx(fImageEnIO);
  end;
end;


{!!
<FS>TImageEnDBVect.LoadedFieldImageFormat

<FM>Declaration<FC>
function LoadedFieldImageFormat: <A TDataFieldImageFormat>;

<FM>Description<FN>
LoadedFieldImageFormat gets the image format stored in the Blob field (loads directly from blob).

If you change <A TImageEnDBVect.DataFieldImageFormat>, to store a several image formats, LoadedFieldImageFormat maintains the original value.

!!}
function TImageEnDBVect.LoadedFieldImageFormat(): TDataFieldImageFormat;
var
  ms: TMemoryStream;
  ss: string;
begin
  result := ifUnknown;
  if not FAutoDisplay then
    exit;
  if FDataLink.Field is TBlobField then
  begin
    ms := TMemoryStream.create();
    try
      (fdatalink.field as tblobfield).SaveToStream(ms);
      ms.position := 0;
      case FindStreamFormat(ms) of
        ioBMP:  result := ifBitmap;
        ioJPEG: result := ifJpeg;
        ioGIF:  result := ifGIF;
        ioPCX:  result := ifPCX;
        ioTIFF:
          begin
            // check if this TIFF contains embedded IEV objects
            if TIEImageEnAnnot.TIFFContainsImageEnAnnot(ms, 0) then
              result := ifTIFFwAnnot
            else
              result := ifTIFF;
          end;
{$IFDEF IEINCLUDEPNG}
        ioPNG: result := ifPNG;
{$ENDIF}
        ioTGA: result := ifTGA;
        ioPXM: result := ifPXM;
        ioICO: result := ifICO;
{$IFDEF IEINCLUDEJPEG2000}
        ioJP2: result := ifJP2;
        ioJ2K: result := ifJ2K;
{$ENDIF}
        ioWBMP: result := ifWBMP;
      end;
    finally
      ms.Free();
    end;
  end
  else
  if FDataLink.Field is TStringField then
  begin
    ss := string(TStringField(FDataLink.Field).Value);
    if (ss <> '') and (IEFileExists(fAbsolutePath + ss)) then
    begin
      case FindFileFormat(fAbsolutePath + ss, false) of
        ioBMP:  result := ifBitmap;
        ioJPEG: result := ifJpeg;
        ioGIF:  result := ifGIF;
        ioPCX:  result := ifPCX;
        ioTIFF:
          begin
            // check if this TIFF contains embedded IEV objects
            if TIEImageEnAnnot.TIFFContainsImageEnAnnot(WideString(fAbsolutePath + ss), 0) then
              result := ifTIFFwAnnot
            else
              result := ifTIFF;
          end;
{$IFDEF IEINCLUDEPNG}
        ioPNG: result := ifPNG;
{$ENDIF}
        ioTGA: result := ifTGA;
        ioPXM: result := ifPXM;
        ioICO: result := ifICO;
{$IFDEF IEINCLUDEJPEG2000}
        ioJP2: result := ifJP2;
        ioJ2K: result := ifJ2K;
{$ENDIF}
        ioWBMP: result := ifWBMP;
      end;
    end;
  end;
end;



procedure TImageEnDBVect.SetJpegQuality(q: integer);
begin
  IOParams.JPEG_Quality := q;
end;



{!!
<FS>TImageEnDBVect.JpegQuality

<FM>Declaration<FC>
property JpegQuality: integer;

<FM>Description<FN>
JpegQuality is the stored image quality factor, from 1 to 100. The higher the value, the better the image quality and the larger resultant memory required.

This is the same as TIOParamsVas.<A TIOParamsVals.JPEG_Quality>.

!!}
function TImageEnDBVect.GetJpegQuality: integer;
begin
  result := IOParams.JPEG_Quality;
end;



{!!
<FS>TImageEnDBVect.IOParams

<FM>Declaration<FC>
property IOParams: <A TIOParamsVals>;

<FM>Description<FN>
IOParams allow you to set or get all file format parameters such as bits per pixel or type of compression.

Read-only

<FM>Example<FC>
Table1.Edit;
ImageEnDBView1.IOParams.BMP_Compression := ioBMP_RLE;
Table1.Post;
!!}
function TImageEnDBVect.GetIOParams: TIOParamsVals;
begin
  result := fImageEnIO.Params;
end;


{$IFDEF IEINCLUDEDIALOGIO}

{!!
<FS>TImageEnDBVect.DoIOPreview

<FM>Declaration<FC>
function DoIOPreview: boolean;

<FM>Description<FN>
DoIOPreview executes the IOPreviews dialog. This dialog gets/sets the parameters of image file formats.
The dialog shown depends upon the <A TImageEnDBVect.DataFieldImageFormat> property.

!!}
function TImageEnDBVect.DoIOPreview: boolean;
var
  pp: TPreviewParams;
begin
  case fDataFieldImageFormat of
    ifBitmap:     pp := [ppBMP];
    ifJpeg:       pp := [ppJPEG];
    ifGIF:        pp := [ppGIF];
    ifPCX:        pp := [ppPCX];
    ifTIFF,
    ifTIFFwAnnot: pp := [ppTIFF];
    ifPNG:        pp := [ppPNG];
    ifTGA:        pp := [ppTGA];
  else
    begin
      result := false;
      exit;
    end;
  end;
  result := fImageEnIO.DoPreviews(pp);
end;
{$ENDIF}



procedure TImageEnDBVect.SetIOPreviewsParams(v: TIOPreviewsParams);
begin
  fImageEnIO.PreviewsParams := v;
end;



{!!
<FS>TImageEnDBVect.IOPreviewsParams

<FM>Declaration<FC>
property IOPreviewsParams: <A TIOPreviewsParams>;

<FM>Description<FN>
The IOPreviewsParams property contains some features that input/output preview dialogs will present. Currently only ioppDefaultLockPreview is allowed.

Set ioppDefaultLockPreview to set down the "Lock preview" button when the dialog is showed.

!!}
function TImageEnDBVect.GetIOPreviewsParams: TIOPreviewsParams;
begin
  result := fImageEnIO.PreviewsParams;
end;




{!!
<FS>TImageEnDBVect.PreviewFont

<FM>Declaration<FC>
property PreviewFont: TFont;

<FM>Description<FN>
If <A TImageEnDBVect.PreviewFontEnabled> is set to True then PreviewFont specifies the font used in the IOPreviews dialog. Ensure the size of font matches the labels length.

<FM>Example<FC>
ImageEnDBVect1.PreviewFont.Name := 'MS Times New Roman';
ImageEnDBVect1.PreviewFontEnabled := True;
ImageEnDBVect1.DoPreviews([peAll]);
!!}
function TImageEnDBVect.GetPreviewFont: TFont;
begin
  result := fImageEnIO.PreviewFont;
end;  

procedure TImageEnDBVect.SetPreviewFont(f: TFont);
begin
  fImageEnIO.PreviewFont := f;
end;



{!!
<FS>TImageEnDBVect.PreviewFontEnabled

<FM>Declaration<FC>
property PreviewFontEnabled: TFont;

<FM>Description<FN>
By default (when PreviewFontEnabled = False) IO Preview dialogs are displayed using the system font (e.g. Tahoma or Segoe UI).
If you set PreviewFontEnabled to True then you can use <A TImageEnDBVect.PreviewFont> to specify a custom font for the IO Preview dialogs.

<FM>Example<FC>
ImageEnDBView1.PreviewFont.Name := 'MS Times New Roman';
ImageEnDBView1.PreviewFontEnabled := False;
ImageEnDBView1.DoPreviews([peAll]);
!!}
function TImageEnDBVect.GetPreviewFontEnabled: Boolean;
begin
  result := fImageEnIO.PreviewFontEnabled;
end;

procedure TImageEnDBVect.SetPreviewFontEnabled(value : Boolean);
begin
  fImageEnIO.PreviewFontEnabled := value;
end;



{!!
<FS>TImageEnDBVect.DataFieldImageFormat

<FM>Declaration<FC>
property DataFieldImageFormat: <A TDataFieldImageFormat>;

<FM>Description<FN>
DataFieldImageFormat sets the image format to save in the Blob field or path reference.

See the <A TImageEnDBVect.IOParams> property for specific image format parameters.
!!}
function TImageEnDBVect.GetDataFieldImageFormat: TDataFieldImageFormat;
begin
  result := fDataFieldImageFormat;
end;

function TImageEnDBVect.InsideDBCtrl: boolean;
var
  parent: TControl;
begin
  result := false;
  parent := self;
  while (parent <> nil) do
  begin
    if parent.ClassName = 'TDBCtrlGrid' then
    begin
      result := true;
      break;
    end;
    if parent = parent.parent then
      break;
    parent := parent.parent;
  end;
end;

procedure TImageEnDBVect.PaintToEx(ABitmap: TIEBitmap; ABitmapScanline: ppointerarray; UpdRect: PRect; drawBackground: boolean; drawGadgets: boolean);
var
  ie: TImageEnVect;
  bmp: TIEBitmap;
begin
  if (not (csDesigning in ComponentState)) and assigned(fDataLink.DataSource) and assigned(fDataLink.DataSource.DataSet) and
    FDataLink.DataSource.DataSet.Active and (fIsInsideDBCtrl or InsideDbCtrl) then
  begin
    // we are in TDBGrid
    bmp := TIEBitmap.Create;
    bmp.assign(IEBitmap);
    fUpdateInvalidate := false;
    ie := TImageEnVect.Create(nil);
    LoadPictureEx(ie.IO);
    if (ie.IEBitmap.Width = 0) or (ie.IEBitmap.Height = 0) then
    begin
      IEBitmap.Resize(1, 1, Background, 255, iehLeft, ievTop);
      IEBitmap.Fill(Background);
    end
    else
      IEBitmap.Assign(ie.IEBitmap);
    Update;
    inherited;
    IEBitmap.Assign(bmp);
    ie.Free();
    bmp.Free();
    fUpdateInvalidate := true;
  end
  else
    inherited;
end;



procedure TImageEnDBVect.Paint;
begin
  fDBToDraw := true;
  inherited;
end;



{!!
<FS>TImageEnDBVect.AbsolutePath

<FM>Declaration<FC>
property AbsolutePath: string;

<FM>Description<FN>
The AbsolutePath property sets/gets the base path where the stored images are when <A TImageEnDBVect.DataField> points to a string field.

The final path is calculated by concatenation of AbsolutePath and string field.

The default value is an empty string. If the AbsolutePath property is empty, then the string field should be an absolute path.
!!}
procedure TImageEnDBVect.SetAbsolutePath(const v: string);
begin
  fAbsolutePath := v;
  FPictureLoaded := false;
  LoadPicture;
end;


{$ELSE} // {$ifdef IEINCLUDEDB}

interface
implementation

{$ENDIF}


end.


