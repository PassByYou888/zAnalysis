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

unit dbimageen;

{$R-}
{$Q-}

{$I ie.inc}


{$IFDEF IEINCLUDEDB}



interface

uses Windows, Messages, classes, Graphics, Db, dbctrls, ImageEnView, ImageEnio, hyiedefs, hyieutils;

type

{!!
<FS>TDataFieldImageFormat

<FM>Declaration<FC>
TDataFieldImageFormat = (ifBitmap, ifJpeg, ifGIF, ifPCX, ifTIFF, ifTIFFwAnnot, ifPNG, ifTGA, ifPXM, ifICO, ifJP2, ifJ2K, ifWBMP, ifUnknown);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>ifBitmap<FN></C> <C>BMP format</C> </R>
<R> <C><FC>ifJpeg<FN></C> <C>Jpeg format</C> </R>
<R> <C><FC>ifGIF<FN></C> <C>GIF format</C> </R>
<R> <C><FC>ifPCX<FN></C> <C>PCX format</C> </R>
<R> <C><FC>ifTIFF<FN></C> <C>TIFF format</C> </R>
<R> <C><FC>ifTIFFwAnnot<FN></C>TIFF format with embedded TImageEnVect annotations (works only with <A TImageEnDBVect>). Doesn't use stream headers.<C>  </C> </R>
<R> <C><FC>ifPNG<FN></C> <C>PNG format</C> </R>
<R> <C><FC>ifTGA<FN></C> <C>TGA format</C> </R>
<R> <C><FC>ifPXM<FN></C> <C>PXM format</C> </R>
<R> <C><FC>ifICO<FN></C> <C>ICO format</C> </R>
<R> <C><FC>ifJP2<FN></C> <C>JPEG2000 format</C> </R>
<R> <C><FC>ifJ2K<FN></C> <C>JPEG2000 format</C> </R>
<R> <C><FC>ifWBMP<FN></C> <C>WBMP format</C> </R>
<R> <C><FC>ifUnknown<FN></C> <C>Unknown format</C> </R>
</TABLE>
!!}
  TDataFieldImageFormat = (ifBitmap, ifJpeg, ifGIF, ifPCX, ifTIFF, ifTIFFwAnnot, ifPNG, ifTGA, ifPXM, ifICO, ifJP2, ifJ2K, ifWBMP, ifUnknown);


{!!
<FS>TUnableToLoadImageEvent

<FM>Declaration<FC>
}
  TUnableToLoadImageEvent = procedure(Sender: TObject; Field: TField) of object;
{!!}


{!!
<FS>TIEBeforeLoadImageEvent

<FM>Declaration<FC>
}
  TIEBeforeLoadImageEvent = procedure(Sender: TObject; Field: TField) of object;
{!!}


{!!
<FS>TImageEnDBView

<FM>Description<FN>
TImageEnDBView is a descendant of <A TImageEnView>, but it can be connected to a TDataset object to store/load images (BMP, PCX, GIF, JPEG, TIFF, PNG, PXM and others) into Blob fields or into path reference (string) fields.

TImageEnDBView works simialrly to Delphi TDBImage components.

You can specify particular file format parameters through <A TImageEnDBView.IOParams> property, or by executing the <A TImageEnDBView.DoIOPreview> method.

You can attach a <A TImageEnProc> component for processing the image contained in the Blob.

Also see methods and properties of <A TImageEnView>.


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
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.StreamHeaders></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnDBView.UseMemoryStream></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TImageEnDBView.OnUnableToLoadImage></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnDBView.OnBeforeLoadImage></C> </R>
</TABLE>
!!}
  TImageEnDBView = class(TImageEnView, IIELoadPicture)
  private
    FAutoDisplay: Boolean;
    FDataLink: TFieldDataLink;
    FPictureLoaded: Boolean;
    fDataFieldImageFormat: TDataFieldImageFormat;
    fStreamHeaders: boolean; // enable/disable load and save of file formats headers
    fDoImageChange: boolean; // se true viene eseguita ImageChange
    fAbsolutePath: string;
    fIsInsideDbCtrl: boolean;
    fOnUnableToLoadImage: TUnableToLoadImageEvent;
    fOnBeforeLoadImage: TIEBeforeLoadImageEvent;
    fUseMemoryStream: boolean;
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
    function InsideDBCtrl: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintToEx(ABitmap: TIEBitmap; ABitmapScanline: ppointerarray; UpdRect: PRect; drawBackground: boolean; drawGadgets: boolean); override;
    procedure Paint; override;
    procedure ImageChange; override;
    property Field: TField read GetField;
    procedure LoadPicture(); virtual;
    function LoadedFieldImageFormat(): TDataFieldImageFormat; virtual;
    property IOParams: TIOParamsVals read GetIOParams;
{$IFDEF IEINCLUDEDIALOGIO}
    function DoIOPreview: boolean;
{$ENDIF}
    property PictureLoaded: boolean read fPictureLoaded;
    property AbsolutePath: string read fAbsolutePath write SetAbsolutePath;
  published
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property DataFieldImageFormat: TDataFieldImageFormat read GetDataFieldImageFormat write SetDataFieldImageFormat default ifBitmap;
    property JpegQuality: integer read GetJpegQuality write SetJpegQuality default 80;
    property IOPreviewsParams: TIOPreviewsParams read GetIOPreviewsParams write SetIOPreviewsParams default [];
    property PreviewFont: TFont read GetPreviewFont write SetPreviewFont;
    property PreviewFontEnabled: Boolean read GetPreviewFontEnabled write SetPreviewFontEnabled default false;

{!!
<FS>TImageEnDBView.UseMemoryStream

<FM>Declaration<FC>
property UseMemoryStream: boolean

<FM>Description<FN>
If True (default), TImageEnDBView will use a temporary memory stream to retrieve data from blob stream.
Otherwise will use CreateBlobStream.
!!}
    property UseMemoryStream: boolean read fUseMemoryStream write fUseMemoryStream default true;

{!!
<FS>TImageEnDBView.StreamHeaders

<FM>Declaration<FC>
property StreamHeaders: boolean

<FM>Description<FN>
If True, TImageEnDBView adds an additional header before standard image format.

To read older ImageEn data fields, leave this property as True.
To read data field from other programs, set this property to False (default value).
!!}
    property StreamHeaders: boolean read fStreamHeaders write fStreamHeaders default false;

    property IsInsideDbCtrl: boolean read fIsInsideDbCtrl write fIsInsideDbCtrl default false;

{!!
<FS>TImageEnDBView.OnUnableToLoadImage

<FM>Declaration<FC>
property OnUnableToLoadImage: <A TUnableToLoadImageEvent>;

<FM>Description<FN>
Occurs when <A TImageEnDBVect> or TImageEnDBView fails to load an image from blob field.
!!}
    property OnUnableToLoadImage: TUnableToLoadImageEvent read fOnUnableToLoadImage write fOnUnableToLoadImage;


{!!
<FS>TImageEnDBView.OnBeforeLoadImage

<FM>Declaration<FC>
property OnBeforeLoadImage: <A TIEBeforeLoadImageEvent>;

<FM>Description<FN>
Occurs immediately before an image is loaded.
!!}
    property OnBeforeLoadImage: TIEBeforeLoadImageEvent read fOnBeforeLoadImage write fOnBeforeLoadImage;

  end;



function IETDataFieldImageFormat_To_TIOFileType(imageformat: TDataFieldImageFormat): TIOFileType;
function IETIOFileType_To_TDataFieldImageFormat(filetype: TIOFileType): TDataFieldImageFormat;


implementation

uses
  controls, giffilter, ImageEn, sysutils;

{$R-}




function IETDataFieldImageFormat_To_TIOFileType(imageformat: TDataFieldImageFormat): TIOFileType;
const
  DATAFIELDIMAGEFORMAT_TO_TIOIMAGEFORMAT: array [TDataFieldImageFormat] of TIOFileType = (ioBMP, ioJPEG, ioGIF, ioPCX, ioTIFF, ioTIFF, ioPNG, ioTGA, ioPXM, ioICO, ioJP2, ioJ2K, ioWBMP, ioUnknown);
begin
  result := DATAFIELDIMAGEFORMAT_TO_TIOIMAGEFORMAT[imageformat];
end;


function IETIOFileType_To_TDataFieldImageFormat(filetype: TIOFileType): TDataFieldImageFormat;
begin
  case filetype of
    ioBMP:  result := ifBitmap;
    ioJPEG: result := ifJpeg;
    ioGIF:  result := ifGIF;
    ioPCX:  result := ifPCX;
    ioTIFF: result := ifTIFF;
    {$IFDEF IEINCLUDEPNG}
    ioPNG:  result := ifPNG;
    {$ENDIF}
    ioTGA:  result := ifTGA;
    ioPXM:  result := ifPXM;
    ioICO:  result := ifICO;
    {$IFDEF IEINCLUDEJPEG2000}
    ioJP2:  result := ifJP2;
    ioJ2K:  result := ifJ2K;
    {$ENDIF}
    ioWBMP: result := ifWBMP;
    else
      result := ifUnknown;
  end;
end;




{!!
<FS>TImageEnDBView.AutoDisplay

<FM>Declaration<FC>
property AutoDisplay: Boolean;

<FM>Description<FN>
AutoDisplay determines whether to automatically display the contents of a graphic BLOB in the database image control.

If AutoDisplay is True (the default value), the image automatically displays new data when the underlying BLOB field changes (such as when moving to a new record).
If AutoDisplay is False, the image clears whenever the underlying BLOB field changes.

To display the data, the user can double-click on the control or select it and press Enter. In addition, calling the LoadPicture method ensures that the control is showing data.
Change the value of AutoDisplay to False if the automatic loading of BLOB fields seems to take too long.
!!}
procedure TImageEnDBView.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then
      LoadPicture;
  end;
end;



constructor TImageEnDBView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetImageEnIO; // creates fImageEnIO;
  fAbsolutePath := '';
  fStreamHeaders := false;
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
  fOnBeforeLoadImage := nil;
  fUseMemoryStream := true;
end;



destructor TImageEnDBView.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;



{!!
<FS>TImageEnDBView.DataSource

<FM>Declaration<FC>
property DataSource: TDataSource;

<FM>Description<FN>
DataSource links the image control to a dataset.

Use DataSource to link the image control to a dataset in which the data can be found.
To fully specify a database field for the image control, both the dataset and a field within that dataset must be defined.
Use the <A TImageEnDBView.DataField> property to specify the particular field within the dataset.
!!}
function TImageEnDBView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TImageEnDBView.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;


{!!
<FS>TImageEnDBView.DataField

<FM>Declaration<FC>
property DataField: string;

<FM>Description<FN>
DataField specifies the field from which the database image displays data.

Use DataField to bind the image control to a field in the dataset.
To fully specify a database field, both the dataset and the field within that dataset must be defined.
The <A TImageEnDBView.DataSource> property of the image control specifies the dataset which contains the DataField. DataField should specify a graphic field.

!!}
function TImageEnDBView.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;


procedure TImageEnDBView.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;


{!!
<FS>TImageEnDBView.ReadOnly

<FM>Declaration<FC>
property ReadOnly: Boolean;

<FM>Description<FN>
ReadOnly determines whether the user can change the contents of the field using the image control.

!!}
function TImageEnDBView.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;


procedure TImageEnDBView.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;


{!!
<FS>TImageEnDBView.Field

<FM>Declaration<FC>
property Field: TField;

<FM>Description<FN>
Field is the TField component the database image is linked to.

Read-only
!!}
function TImageEnDBView.GetField: TField;
begin
  Result := FDataLink.Field;
end;


procedure TImageEnDBView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;


procedure TImageEnDBView.DataChange(Sender: TObject);
begin
  Clear();
  FPictureLoaded := False;
  if (not assigned(fDataLink.DataSource)) or (not assigned(FDataLink.DataSource.DataSet)) or (not FDataLink.DataSource.DataSet.Active) then
    exit;
  if FAutoDisplay then
    LoadPicture;
end;


procedure TImageEnDBView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  LoadPicture();
  inherited;
end;


procedure TImageEnDBView.ImageChange;
begin
  inherited;
  if fDoImageChange then
  begin
    FDataLink.Modified;
    FPictureLoaded := True;
    Invalidate();
  end;
end;


procedure TImageEnDBView.SetDataFieldImageFormat(v: TDataFieldImageFormat);
begin
  fDataFieldImageFormat := v;
  ImageChange;
end;


// Save the image
procedure TImageEnDBView.UpdateData(Sender: TObject);
var
  stream: TStream;
  ss: string;
begin
  fImageEnIO.StreamHeaders := fStreamHeaders;
  if FDataLink.Field is TBlobField then
  begin
    // blob
    if fUseMemoryStream then
    begin
      // upload the full file using a temporary memory stream
      stream := TMemoryStream.create;
      try
        fImageEnIO.SaveToStream(stream, IETDataFieldImageFormat_To_TIOFileType(fDataFieldImageFormat));
        stream.position := 0;
        (FDatalink.Field as TBlobField).LoadFromStream(stream);
      finally
        stream.Free();
      end;
    end
    else
    begin
      // direct save using CreateBlobStream
      stream := FDatalink.DataSet.CreateBlobStream(FDatalink.Field, bmWrite);
      try
        fImageEnIO.SaveToStream(stream, IETDataFieldImageFormat_To_TIOFileType(fDataFieldImageFormat));
      finally
        stream.Free();
      end;
    end
  end
  else
  if FDataLink.Field is TStringField then
  begin
    // path
    ss := string(TStringField(FDataLink.Field).Value);
    if (ss <> '') then
    begin
      ss := fAbsolutePath + ss;
      fImageEnIO.SaveToFile(ss, IETDataFieldImageFormat_To_TIOFileType(fDataFieldImageFormat));
    end;
  end;
end;


// Load image from fdatalink.field without check fPictureLoaded
// Doesn't assign fDataFieldImageFormat
procedure TImageEnDBView.LoadPictureEx(ffImageEnIO: TImageEnIO);
var
  stream: TStream;
  ifm: TDataFieldImageFormat;
  bg: THYIEGraphicHeader;
  ss: string;
  success: boolean;
begin
  if assigned(fOnBeforeLoadImage) then
    fOnBeforeLoadImage(self, FDataLink.Field);
    
  success := false;
  try
    ffImageEnIO.StreamHeaders := fStreamHeaders;
    if (FDataLink.Field is TBlobField) and ((FDataLink.Field as TBlobField).BlobSize > 0) then
    begin
      fDoImageChange := false;
      ifm := LoadedFieldImageFormat();
      stream := nil;
      try
        if fUseMemoryStream then
        begin
          // download image into temporary memory stream
          stream := TMemoryStream.Create;
          (FDatalink.Field as TBlobField).SaveToStream(stream);
          stream.Position := 0;
        end
        else
        begin
          // directly download the image
          stream := FDatalink.DataSet.CreateBlobStream(FDatalink.Field, bmRead);
        end;
        if ifm = ifUnknown then
        begin
          // try paradox graphic
          stream.read(bg, sizeof(THYIEGraphicHeader));
          if (bg.Count = 1) and (bg.HType = $0100) then
            ifm := ifBitmap
          else
            stream.position := 0;
        end;
        ffImageEnIO.LoadFromStreamFormat(stream, IETDataFieldImageFormat_To_TIOFileType(ifm));
        if ffImageEnIO.Aborting then
          Clear();
        success := not ffImageEnIO.Aborting;
      finally
        stream.Free();
        fDoImageChange := true;
      end;
    end
    else
    if (FDataLink.Field is TStringField) then
    begin
      ss := string(TStringField(FDataLink.Field).Value);
      if (ss <> '') and (IEFileExists(fAbsolutePath + ss)) then
      begin
        ss := fAbsolutePath + ss;
        ifm := LoadedFieldImageFormat;
        ffImageEnIO.LoadFromFile(ss, IETDataFieldImageFormat_To_TIOFileType(ifm));
        if ffImageEnIO.Aborting then
          Clear();
        success := not ffImageEnIO.Aborting;
        fDoImageChange := true;
      end;
    end;
  except
    success := false;
  end;
  if (not success) and assigned(fOnUnableToLoadImage) then
    fOnUnableToLoadImage(self, FDataLink.Field);
end;


{!!
<FS>TImageEnDBView.LoadPicture

<FM>Declaration<FC>
procedure LoadPicture;

<FM>Description<FN>
LoadPicture loads the image stored in the field into the database image control.
!!}
// Load image from fdatalink.field
procedure TImageEnDBView.LoadPicture;
begin
  if (not FPictureLoaded) and
    (((FDataLink.Field is TBlobField) and ((FDataLink.Field as TBlobField).BlobSize > 0)) or
    (FDataLink.Field is TStringField)) then
  begin
    LoadPictureEx(fImageEnIO);
  end;
end;


{!!
<FS>TImageEnDBView.LoadedFieldImageFormat

<FM>Declaration<FC>
function LoadedFieldImageFormat(): <A TDataFieldImageFormat>;

<FM>Description<FN>
LoadedFieldImageFormat gets the image format stored in the Blob field (load directly from blob).

If you change <A TImageEnDBView.DataFieldImageFormat>, to store as several image formats, LoadedFieldImageFormat maintains the original value.
!!}
function TImageEnDBView.LoadedFieldImageFormat(): TDataFieldImageFormat;
var
  stream: TStream;
  ss: string;
begin
  result := ifUnknown;
  if not FAutoDisplay then
    exit;
  if FDataLink.Field is TBlobField then
  begin
    stream := nil;
    try
      if fUseMemoryStream then
      begin
        stream := TMemoryStream.create;
        (FDatalink.Field as TBlobField).SaveToStream(stream);
        stream.Position := 0;
      end
      else
        stream := FDatalink.DataSet.CreateBlobStream(FDatalink.Field, bmRead);
      result := IETIOFileType_To_TDataFieldImageFormat( FindStreamFormat(stream) );
    finally
      stream.Free();
    end;
  end
  else
  if FDataLink.Field is TStringField then
  begin
    ss := string(TStringField(FDataLink.Field).Value);
    if (ss <> '') and (IEFileExists(fAbsolutePath + ss)) then
      result := IETIOFileType_To_TDataFieldImageFormat( FindFileFormat(fAbsolutePath + ss, false) );
  end;
end;


procedure TImageEnDBView.SetJpegQuality(q: integer);
begin
  IOParams.JPEG_Quality := q;
end;


{!!
<FS>TImageEnDBView.JpegQuality

<FM>Declaration<FC>
property JpegQuality: integer;

<FM>Description<FN>
JpegQuality sets the quality factor, from 1 to 100. The higher the value, the better the image quality and the larger resultant memory needed.

This is the example of <A TImageEnDBView.IOParams>.<A TIOParamsVals.JPEG_Quality>.

!!}
function TImageEnDBView.GetJpegQuality: integer;
begin
  result := IOParams.JPEG_Quality;
end;


{!!
<FS>TImageEnDBView.IOParams

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
function TImageEnDBView.GetIOParams: TIOParamsVals;
begin
  result := fImageEnIO.Params;
end;


{$IFDEF IEINCLUDEDIALOGIO}

{!!
<FS>TImageEnDBView.DoIOPreview

<FM>Declaration<FC>
function DoIOPreview: boolean;

<FM>Description<FN>
This function executes the IOPreviews dialog. This dialog gets/sets the parameters of image file formats.
The dialog shown depends on the <A TImageEnDBView.DataFieldImageFormat> property.

!!}
function TImageEnDBView.DoIOPreview: boolean;
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


procedure TImageEnDBView.SetIOPreviewsParams(v: TIOPreviewsParams);
begin
  fImageEnIO.PreviewsParams := v;
end;


{!!
<FS>TImageEnDBView.IOPreviewsParams

<FM>Declaration<FC>
property IOPreviewsParams: <A TIOPreviewsParams>;

<FM>Description<FN>
This property contains some features that the input/output preview dialog will have.
!!}
function TImageEnDBView.GetIOPreviewsParams: TIOPreviewsParams;
begin
  result := fImageEnIO.PreviewsParams;
end;



{!!
<FS>TImageEnDBView.PreviewFont

<FM>Declaration<FC>
property PreviewFont: TFont;

<FM>Description<FN>
If <A TImageEnDBView.PreviewFontEnabled> is set to True then PreviewFont specifies the font used in the IOPreviews dialog. Ensure the size of font matches the labels length.

<FM>Example<FC>
ImageEnDBView1.PreviewFont.Name := 'MS Times New Roman';
ImageEnDBView1.PreviewFontEnabled := True;
ImageEnDBView1.DoPreviews([peAll]);
!!}
function TImageEnDBView.GetPreviewFont: TFont;
begin
  result := fImageEnIO.PreviewFont;
end;

procedure TImageEnDBView.SetPreviewFont(f: TFont);
begin
  fImageEnIO.PreviewFont := f;
end;


{!!
<FS>TImageEnDBView.PreviewFontEnabled

<FM>Declaration<FC>
property PreviewFontEnabled: TFont;

<FM>Description<FN>
By default (when PreviewFontEnabled = False) IO Preview dialogs are displayed using the system font (e.g. Tahoma or Segoe UI).
If you set PreviewFontEnabled to True then you can use <A TImageEnDBView.PreviewFont> to specify a custom font for the IO Preview dialogs.

<FM>Example<FC>
ImageEnDBView1.PreviewFont.Name := 'MS Times New Roman';
ImageEnDBView1.PreviewFontEnabled := False;
ImageEnDBView1.DoPreviews([peAll]);
!!}
function TImageEnDBView.GetPreviewFontEnabled: Boolean;
begin
  result := fImageEnIO.PreviewFontEnabled;
end;

procedure TImageEnDBView.SetPreviewFontEnabled(value : Boolean);
begin
  fImageEnIO.PreviewFontEnabled := value;
end;


{!!
<FS>TImageEnDBView.DataFieldImageFormat

<FM>Declaration<FC>
property DataFieldImageFormat: <A TDataFieldImageFormat>;

<FM>Description<FN>
Sets the image format to save in the Blob field or path reference.

See the <A TImageEnDBView.IOParams> property for specific image format parameters.

!!}
function TImageEnDBView.GetDataFieldImageFormat: TDataFieldImageFormat;
begin
  result := fDataFieldImageFormat;
end;


function TImageEnDBView.InsideDBCtrl: boolean;
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


procedure TImageEnDBView.PaintToEx(ABitmap: TIEBitmap; ABitmapScanline: ppointerarray; UpdRect: PRect; drawBackground: boolean; drawGadgets: boolean);
var
  ie: TImageEnView;
  bmp: TIEBitmap;
begin
  if (not (csDesigning in ComponentState)) and assigned(fDataLink.DataSource) and assigned(fDataLink.DataSource.DataSet) and
    FDataLink.DataSource.DataSet.Active and (fIsInsideDBCtrl or InsideDbCtrl) then
  begin
    // we are inside TDBGrid
    bmp := TIEBitmap.Create;
    bmp.Assign(IEBitmap);
    fUpdateInvalidate := false;
    ie := TImageEnView.Create(nil);
    try
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
    finally
      FreeAndNil(ie);
      FreeAndNil(bmp);
    end;
    fUpdateInvalidate := true;
  end
  else
    inherited;
end;


procedure TImageEnDBView.Paint;
begin
  fDBToDraw := true;
  inherited;
end;


{!!
<FS>TImageEnDBView.AbsolutePath

<FM>Declaration<FC>
property AbsolutePath: string;

<FM>Description<FN>
The AbsolutePath property sets/gets the base path where the stored images are when <A TImageEnDBView.DataField> points to a string field.

The final path is calculated by concatenation of AbsolutePath and string field.

The default value is an empty string. If the AbsolutePath property is empty, then the string field should be an absolute path.
!!}
procedure TImageEnDBView.SetAbsolutePath(const v: string);
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





