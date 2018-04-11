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
File version 1009
*)


unit ieanimation;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses
  Windows, Messages, SysUtils, Classes,
  {$ifdef IEHASTYPES} Types, {$endif}
  {$ifdef IEHASUITYPES} System.UITypes, {$endif}
  Graphics, hyiedefs, hyieutils;


type

  ///////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////
  // TIEAnimation


{!!
<FS>TIEAnimationGetImageInfoEvent

<FM>Declaration<FC>
TIEAnimationGetImageInfoEvent = procedure(Sender: TObject; imageIndex: integer; isVisible: boolean; var imageWidth: integer; var imageHeight: integer; var text: WideString) of object;

<FM>Description<FN>
Event called when only the image info is needed (not the whole image).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Sender<FN></C> <C>The caller, a subclass of <A TIEAnimation> class.</C> </R>
<R> <C><FC>imageIndex<FN></C> <C>Index of the required image info.</C> </R>
<R> <C><FC>isVisible<FN></C> <C>Is True if the required image could be not visible (hence you can provide estimated values).</C> </R>
<R> <C><FC>imageWidth<FN></C> <C>Return value for image width.</C> </R>
<R> <C><FC>imageHeight<FN></C> <C>Return value for image height</C> </R>
<R> <C><FC>text<FN></C> <C>Return value for image textual info (ie a filename).</C> </R>
</TABLE>
!!}
  TIEAnimationGetImageInfoEvent = procedure(Sender: TObject; imageIndex: integer; isVisible: boolean; var imageWidth: integer; var imageHeight: integer; var text: WideString) of object;


{!!
<FS>TIEAnimationGetImageEvent

<FM>Declaration<FC>
TIEAnimationGetImageEvent = procedure(Sender: TObject; imageIndex: integer; var image: <A TIEBitmap>; var text: WideString) of object;

<FM>Description<FN>
Event called when the whole image is needed.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Sender<FN></C> <C>The caller, a subclass of <A TIEAnimation> class.</C> </R>
<R> <C><FC>imageIndex<FN></C> <C>Index of the required image.</C> </R>
<R> <C><FC>image<FN></C> <C>Return value for image bitmap.</C> </R>
<R> <C><FC>text<FN></C> <C>Return value for image textual info (ie a filename).</C> </R>
</TABLE>
!!}
  TIEAnimationGetImageEvent = procedure(Sender: TObject; imageIndex: integer; var image: TIEBitmap; var text: WideString) of object;


{!!
<FS>TIEAnimationReleaseImageEvent

<FM>Declaration<FC>
TIEAnimationReleaseImageEvent = procedure(Sender: TObject; imageIndex: integer; var image: <A TIEBitmap>) of object;

<FM>Description<FN>
Event called when the image can be released.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Sender<FN></C> <C>The caller, a subclass of <A TIEAnimation> class.</C> </R>
<R> <C><FC>imageIndex<FN></C> <C>Index of the image to release.</C> </R>
<R> <C><FC>image<FN></C> <C>The bitmap to release.</C> </R>
</TABLE>
!!}
  TIEAnimationReleaseImageEvent = procedure(Sender: TObject; imageIndex: integer; var image: TIEBitmap) of object;


{!!
<FS>TIEAnimationImageInfo

<FM>Declaration<FC>
TIEAnimationImageInfo = class;

<FM>Description<FN>
Objects of this class contain the animation state of each image like positions and rotations, other than timing info.

<FM>Fields<FN>
}
  TIEAnimationImageInfo = class
  public
    startTime    : dword;    // when transition was started
    endTime      : dword;    // when transition must ends

    startAlpha   : integer;  // the starting alpha (0..255)
    endAlpha     : integer;  // the ending alpha (0..255)
    lastAlpha    : integer;  // last calculated alpha (0..255)

    startAngleX  : double;   // the starting angle X in degrees
    endAngleX    : double;   // the ending angle X in degrees
    lastAngleX   : double;   // last calculated angle X in degrees

    startAngleY  : double;   // the starting angle Y in degrees
    endAngleY    : double;   // the ending angle Y in degrees
    lastAngleY   : double;   // last calculated angle Y in degrees

    startCenterX : integer;  // the starting X position
    endCenterX   : integer;  // the ending X position
    lastCenterX  : integer;  // last calculated X position

    startCenterY : integer;  // the starting Y position
    endCenterY   : integer;  // the ending Y position
    lastCenterY  : integer;  // last calculated Y position

    startWidth   : integer;  // the starting width
    endWidth     : integer;  // the ending width
    lastWidth    : integer;  // last calculated width

    startHeight  : integer;  // the starting height
    endHeight    : integer;  // the ending height
    lastHeight   : integer;  // last calculated height

    lastCoords   : TIEQuadCoords; // last calculated coordinates
  end;
{!!}


{!!
<FS>TIEAnimation

<FM>Declaration<FC>
TIEAnimation = class;

<FM>Description<FN>
This is the base abstract class for animation classes like coverflow-like (<A TIEHorizontalFlow>) and circular flow (<A TIECircularFlow>) or any user custom animation classes.

<FM>Methods and Properties<FN>
<FI>Protected<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIEAnimation.DoGetImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.DoGetImageInfo></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.DoReleaseImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.drawImage> virtual</C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.PaintScrollBar></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.PaintText></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.SetEndValues> virtual</C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.SetInitialValues> virtual</C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.SetStartEndValues></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.SetStartValues> virtual</C> </R>
</TABLE>

<FI>Public<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C>constructor <A TIEAnimation.Create></C> </R>
<R> <C_IMG_METHOD> <C>destructor <A TIEAnimation.Destroy></C> </R>
</TABLE>

<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.AnimDuration></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.CurrentImage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.BorderPen></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.Depth></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.DeleteImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.Display> virtual abstract</C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.FindImageAt></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.Font></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.FramesZoomFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.ImageCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.Images></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.InsertImage></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.IsInsideScrollbar></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.IsInsideScrollbarSlider></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.LastFrameZoomFilter></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.MoveScrollbarSliderTo></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.NeedRefresh></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.RestartAnimation></C> </R>
<R> <C_IMG_METHOD> <C><A TIEAnimation.SetupEvents></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.ShadowAlphaMax></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.ShadowAlphaMin></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.ShowBorder></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.ShowScrollbar></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.ShowText></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.ViewHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEAnimation.ViewWidth></C> </R>
</TABLE>


<FM>Demos<FN>
Display\ManualFlow
!!}
  TIEAnimation = class
    private
      m_getImageInfo: TIEAnimationGetImageInfoEvent;
      m_getImage: TIEAnimationGetImageEvent;
      m_releaseImage: TIEAnimationReleaseImageEvent;
      m_images: array of TIEAnimationImageInfo;
      m_currentImage: integer;
      m_viewWidth: integer;
      m_viewHeight: integer;
      m_animDuration: dword; // in ms
      m_depth: integer;
      m_needRefresh: boolean;
      m_showBorder: boolean;
      m_borderPen: TPen;
      m_shadowAlphaMin: integer;
      m_shadowAlphaMax: integer;
      m_lastScrollbarSlideRect: TIERectangle;
      m_lastScrollbarBoundRect: TIERectangle;
      m_showScrollbar: boolean;
      m_showText: boolean;
      m_lastFrameZoomFilter: TResampleFilter;
      m_FramesZoomFilter: TResampleFilter;
      m_canvasCriticalSection: TRTLCriticalSection;
      fFont : TFont;

      procedure SetCurrentImage(value: integer);
      procedure SetViewWidth(value: integer);
      procedure SetViewHeight(value: integer);
      procedure SetDepth(value: integer);
      function GetImageCount(): integer;
      procedure SetImageCount(value: integer);
      procedure SetShowBorder(value: boolean);
      procedure SetShadowAlphaMin(value: integer);
      procedure SetShadowAlphaMax(value: integer);
      procedure SetShowScrollbar(value: boolean);
      procedure FreeImageInfo(first, last: integer);
      function GetImages(idx: integer): TIEAnimationImageInfo;
      procedure PaintBorder(i: integer; dest: TIEBitmap);
      procedure SetShowText(value: boolean);
      function isVisible(const coords: TIEQuadCoords): boolean; virtual;
      procedure SetFont(const Value: TFont);

    protected

      procedure SetStartEndValues();
      procedure SetInitialValues(first, last: integer); virtual;
      procedure SetStartValues(); virtual;
      procedure SetEndValues(); virtual;
      function drawImage(dest: TIEBitmap; imageIndex: integer): boolean; virtual;
      procedure DoGetImageInfo(imageIndex: integer; isVisible: boolean; var imageWidth: integer; var imageHeight: integer; var text: WideString);
      procedure DoGetImage(imageIndex: integer; var image: TIEBitmap; var text: WideString);
      procedure DoReleaseImage(imageIndex: integer; var image: TIEBitmap);
      procedure PaintScrollBar(dest: TIEBitmap);
      procedure PaintText(posY: integer; dest: TIEBitmap);

    public

      constructor Create();
      destructor Destroy(); override;
      procedure SetupEvents(getImage: TIEAnimationGetImageEvent; releaseImage: TIEAnimationReleaseImageEvent = nil; getImageInfo: TIEAnimationGetImageInfoEvent = nil);
      procedure RestartAnimation();

      procedure Display(dest: TIEBitmap); virtual; abstract;
      function FindImageAt(X, Y: integer): integer;
      procedure MoveScrollbarSliderTo(X: integer);
      function IsInsideScrollbarSlider(X, Y: integer): boolean;
      function IsInsideScrollbar(X, Y: integer): boolean;
      procedure DeleteImage(imageIndex: integer);
      procedure InsertImage(imageIndex: integer);

      property ViewWidth: integer read m_viewWidth write SetViewWidth;
      property ViewHeight: integer read m_viewHeight write SetViewHeight;
      property Depth: integer read m_depth write SetDepth;
      property ShowBorder: boolean read m_showBorder write SetShowBorder;
      property ShadowAlphaMin: integer read m_shadowAlphaMin write SetShadowAlphaMin;
      property ShadowAlphaMax: integer read m_shadowAlphaMax write SetShadowAlphaMax;
      property ImageCount: integer read GetImageCount write SetImageCount;
      property Images[idx: integer]: TIEAnimationImageInfo read GetImages;
      property CurrentImage: integer read m_currentImage write SetCurrentImage;
      property ShowScrollbar: boolean read m_showScrollbar write SetShowScrollbar;
      property ShowText: boolean read m_showText write SetShowText;
      property Font: TFont read fFont write SetFont;

{!!
<FS>TIEAnimation.BorderPen

<FM>Declaration<FC>
property BorderPen: TPen;

<FM>Description<FN>
Specifies pen to use when painting the frames border. A frame border is painted when <A TIEAnimation.ShowBorder> is true.
!!}
      property BorderPen: TPen read m_borderPen;

{!!
<FS>TIEAnimation.LastFrameZoomFilter

<FM>Declaration<FC>
property LastFrameZoomFilter: <A TResampleFilter>;

<FM>Description<FN>
Specifies the interpolation filter to apply when the last frame is showed (when the animation ends).
Default is rfFastLinear.
!!}
      property LastFrameZoomFilter: TResampleFilter read m_lastFrameZoomFilter write m_lastFrameZoomFilter;

{!!
<FS>TIEAnimation.FramesZoomFilter

<FM>Declaration<FC>
property FramesZoomFilter: <A TResampleFilter>;

<FM>Description<FN>
Specifies the interpolation filter to apply when the animation runs.
Default is rfNone.
!!}
      property FramesZoomFilter: TResampleFilter read m_FramesZoomFilter write m_FramesZoomFilter;

{!!
<FS>TIEAnimation.AnimDuration

<FM>Declaration<FC>
property AnimDuration: dword;

<FM>Description<FN>
Specifies the animation duration in milliseconds. Default is 800ms.
!!}
      property AnimDuration: dword read m_animDuration write m_animDuration;

{!!
<FS>TIEAnimation.NeedRefresh

<FM>Declaration<FC>
property NeedRefresh: boolean;

<FM>Description<FN>
Applications will read this value to know if a call to <A TIEAnimation.Display> is necessary.
This property is True when animation is running, and False when the last frame has been painted.
NeedRefresh can be also True just after a property has been changed.

<FM>Example<FC>
// repaint frame (if necessary)
procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if m_animation.NeedRefresh then
  begin
    ImageEnView1.IEBitmap.Fill(clBlack);
    ImageEnView1.IEBitmap.AlphaChannel.Fill(255);
    m_animation.Display(ImageEnView1.IEBitmap);
    ImageEnView1.Update();
  end;
end;
!!}
      property NeedRefresh: boolean read m_needRefresh write m_needRefresh;
  end;



  ///////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////
  // TIEHorizontalFlow

{!!
<FS>TIEHorizontalFlow

<FM>Declaration<FC>
TIEHorizontalFlow = class(<A TIEAnimation>);

<FM>Description<FN>
Implements a coverflow-like animation.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEHorizontalFlow.CurrentImageZoom></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEHorizontalFlow.HorizontalDistance></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEHorizontalFlow.ImagesHorizontalPercentage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEHorizontalFlow.ImagesVerticalPercentage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEHorizontalFlow.ImagesZoom></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEHorizontalFlow.RotateAngle></C> </R>
</TABLE>
Also inherits all properties and methods of <A TIEAnimation>

<FM>Demos<FN>
Display\ManualFlow
Display\MviewFlow
!!}
  TIEHorizontalFlow = class(TIEAnimation)
    private
      m_horizontalDistance: integer;
      m_imagesHorizontalPercentage: double;  // in %
      m_imagesVerticalPercentage: double;    // in %
      m_rotateAngle: double;                 // in degrees
      m_imagesZoom: double;                  // 0..1
      m_currentImageZoom: double;            // 0..1

      procedure SetHorizontalDistance(value: integer);
      procedure SetImagesHorizontalPercentage(value: double);
      procedure SetImagesVerticalPercentage(value: double);
      procedure SetRotateAngle(value: double);
      procedure SetImagesZoom(value: double);
      procedure SetCurrentImageZoom(value: double);

    protected
      procedure SetInitialValues(first, last: integer); override;
      procedure SetStartValues(); override;
      procedure SetEndValues(); override;

    public
      constructor Create();
      destructor Destroy(); override;

      procedure Display(dest: TIEBitmap); override;

      property HorizontalDistance: integer read m_horizontalDistance write SetHorizontalDistance;
      property ImagesHorizontalPercentage: double read m_imagesHorizontalPercentage write SetImagesHorizontalPercentage;
      property ImagesVerticalPercentage: double read m_imagesVerticalPercentage write SetImagesVerticalPercentage;
      property RotateAngle: double read m_rotateAngle write SetRotateAngle;
      property ImagesZoom: double read m_imagesZoom write SetImagesZoom;
      property CurrentImageZoom: double read m_currentImageZoom write SetCurrentImageZoom;
  end;


  ///////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////
  // TIECircularFlow

{!!
<FS>TIECircularFlow

<FM>Declaration<FC>
TIECircularFlow = class(<A TIEAnimation>);

<FM>Description<FN>
Implements a circular animation.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIECircularFlow.CurrentImageZoom></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIECircularFlow.EllipseAngle></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIECircularFlow.ImagesSizePercentage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIECircularFlow.ImagesZoom></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIECircularFlow.VisibleImages></C> </R>
</TABLE>
Also inherits all properties and methods of <A TIEAnimation>

<FM>Demos<FN>
Display\ManualFlow
Display\MviewFlow
!!}
  TIECircularFlow = class(TIEAnimation)
    private
      m_imagesSizePercentage: double;  // in %
      m_imagesZoom: double;            // 0..1
      m_currentImageZoom: double;      // 0..1
      m_visibleImages: integer;
      m_ellipseAngle: double;          // in degrees

    protected
      procedure SetInitialValues(first, last: integer); override;
      procedure SetStartValues(); override;
      procedure SetEndValues(); override;
      procedure SetImagesSizePercentage(value: double);
      procedure SetImagesZoom(value: double);
      procedure SetCurrentImageZoom(value: double);
      procedure SetVisibleImages(value: integer);
      procedure SetEllipseAngle(value: double);

    public
      constructor Create();
      destructor Destroy(); override;

      procedure Display(dest: TIEBitmap); override;

      property ImagesSizePercentage: double read m_imagesSizePercentage write SetImagesSizePercentage;
      property ImagesZoom: double read m_imagesZoom write SetImagesZoom;
      property CurrentImageZoom: double read m_currentImageZoom write SetCurrentImageZoom;
      property VisibleImages: integer read m_visibleImages write SetVisibleImages;
      property EllipseAngle: double read m_ellipseAngle write SetEllipseAngle;
  end;




implementation


uses iegdiplus, imageenio, imageenproc;





///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
// TIEAnimation


{!!
<FS>TIEAnimation.Create

<FM>Declaration<FC>
constructor Create();

<FM>Description<FN>
Creates the TIEAnimation object. You should call this constructor on your subclass.
!!}
constructor TIEAnimation.Create();
begin
  inherited Create();

  IEGDIPLoadLibrary();

  // default values for properties
  m_animDuration        := 800;
  m_depth               := 500;
  m_showBorder          := false;
  m_shadowAlphaMin      := 0;
  m_shadowAlphaMax      := 40;
  m_showScrollbar       := true;
  m_showText            := true;
  m_lastFrameZoomFilter := rfFastLinear;
  m_FramesZoomFilter    := rfNone;

  m_viewWidth           := 0;
  m_viewHeight          := 0;
  m_currentImage        := 0;

  m_borderPen           := TPen.Create();
  m_borderPen.Style     := psSolid;
  m_borderPen.Mode      := pmCopy;
  m_borderPen.Color     := clWhite;
  m_borderPen.Width     := 2;

  SetLength(m_images, 0);

  fFont := TFont.Create;
  fFont.Charset := DEFAULT_CHARSET;
  fFont.Color   := clWhite;
  fFont.Height  := 18;
  fFont.Name    := 'Arial';
  fFont.Style   := [fsBold];

  InitializeCriticalSection(m_canvasCriticalSection);
end;


{!!
<FS>TIEAnimation.SetupEvents

<FM>Declaration<FC>
procedure SetupEvents(getImage: <A TIEAnimationGetImageEvent>; releaseImage: <A TIEAnimationReleaseImageEvent> = nil; getImageInfo: <A TIEAnimationGetImageInfoEvent> = nil);

<FM>Description<FN>
Setup events necessary to show images.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>getImage<FN></C> <C>The event called whenever a new image needs to be drawn. Cannot be nil.</C> </R>
<R> <C><FC>releaseImage<FN></C> <C>The event called whenever an image can be released. Can be nil.</C> </R>
<R> <C><FC>getImageInfo<FN></C> <C>The event called whenever only image info are needed. Can be nil.</C> </R>
</TABLE>
!!}
procedure TIEAnimation.SetupEvents(getImage: TIEAnimationGetImageEvent; releaseImage: TIEAnimationReleaseImageEvent; getImageInfo: TIEAnimationGetImageInfoEvent);
begin
  m_getImageInfo        := getImageInfo;
  m_getImage            := getImage;
  m_releaseImage        := releaseImage;
  SetInitialValues(0, ImageCount-1);
  SetEndValues();
end;


function TIEAnimation.GetImageCount(): integer;
begin
  result := length(m_images);
end;


{!!
<FS>TIEAnimation.ImageCount

<FM>Declaration<FC>
property ImageCount: integer;

<FM>Description<FN>
Specifies the number of images that TIEAnimation should handle. This value could not match the number of visible images.
Setting this property reinitializes the animation and set the current image to 0.
!!}
procedure TIEAnimation.SetImageCount(value: integer);
var
  i: integer;
begin
  FreeImageInfo(0, length(m_images)-1);
  SetLength(m_images, value);
  for i := 0 to length(m_images)-1 do
    m_images[i] := TIEAnimationImageInfo.Create();
  m_currentImage := 0;
  SetInitialValues(0, length(m_images)-1);
  SetEndValues();
end;


{!!
<FS>TIEAnimation.DeleteImage

<FM>Declaration<FC>
procedure DeleteImage(imageIndex: integer);

<FM>Description<FN>
Removes the specified image.
!!}
procedure TIEAnimation.DeleteImage(imageIndex: integer);
begin
  FreeImageInfo(imageIndex, imageIndex);
  m_currentImage := imin(m_currentImage, length(m_images)-1);
  SetStartEndValues();
end;


{!!
<FS>TIEAnimation.InsertImage

<FM>Declaration<FC>
procedure InsertImage(imageIndex: integer);

<FM>Description<FN>
Inserts a new image at specified position.
!!}
procedure TIEAnimation.InsertImage(imageIndex: integer);
var
  i: integer;
begin
  SetLength(m_images, length(m_images)+1);
  for i := length(m_images)-1 downto imageIndex+1 do
    m_images[i] := m_images[i-1];
  m_images[imageIndex] := TIEAnimationImageInfo.Create();
  m_currentImage := imageIndex;
  SetInitialValues(imageIndex, imageIndex);
  SetStartEndValues();
end;


procedure TIEAnimation.FreeImageInfo(first, last: integer);
var
  i: integer;
  count: integer;
begin
  count := last - first + 1;
  for i := first to last do
    m_images[i].Free();
  for i := last+1 to length(m_images)-1 do
  begin
    m_images[first] := m_images[i];
    inc(first);
  end;
  SetLength(m_images, length(m_images)-count);
end;


{!!
<FS>TIEAnimation.Destroy

<FM>Declaration<FC>
destructor Destroy(); override;

<FM>Description<FN>
Destroys the TIEAnimation object. You should call this destructor on your subclass.
!!}
destructor TIEAnimation.Destroy();
begin
  FreeImageInfo(0, length(m_images)-1);
  DeleteCriticalSection(m_canvasCriticalSection);
  FreeAndNil(m_borderPen);
  IEGDIPUnLoadLibrary();
  inherited;
end;


{!!
<FS>TIEAnimation.SetInitialValues

<FM>Declaration<FC>
procedure SetInitialValues(first, last: integer); virtual;

<FM>Description<FN>
Sets initial animation values (image position, rotation, etc). This is called one time when the object is initialized.
This method need to be overloaded to implement animations.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>first<FN></C> <C>Index of first image to set values.</C> </R>
<R> <C><FC>last<FN></C> <C>Index of last image to set values..</C> </R>
</TABLE>
!!}
procedure TIEAnimation.SetInitialValues(first, last: integer);
var
  i: integer;
begin
  for i := first to last do
  begin
    Images[i].startTime    := GetTickCount();
    Images[i].startAlpha   := 255;
    Images[i].startAngleX  := 0.0;
    Images[i].startAngleY  := 0.0;
    Images[i].startCenterX := 0;
    Images[i].startCenterY := 0;
    Images[i].startWidth   := 0;
    Images[i].startHeight  := 0;
  end;
  NeedRefresh := true;
end;


{!!
<FS>TIEAnimation.SetStartValues

<FM>Declaration<FC>
procedure SetStartValues(); virtual;

<FM>Description<FN>
Sets starting values (image position, rotation, etc) whenever the animation is about to begin.
This method needs to be overloaded to implement animations.
!!}
procedure TIEAnimation.SetStartValues();
var
  i: integer;
begin
  for i := 0 to ImageCount-1 do
  begin
    Images[i].startTime    := GetTickCount();
    Images[i].startAlpha   := Images[i].lastAlpha;
    Images[i].startAngleX  := Images[i].lastAngleX;
    Images[i].startAngleY  := Images[i].lastAngleY;
    Images[i].startCenterX := Images[i].lastCenterX;
    Images[i].startCenterY := Images[i].lastCenterY;
    Images[i].startWidth   := Images[i].lastWidth;
    Images[i].startHeight  := Images[i].lastHeight;
  end;
  NeedRefresh := true;
end;


{!!
<FS>TIEAnimation.SetEndValues

<FM>Declaration<FC>
procedure SetEndValues(); virtual;

<FM>Description<FN>
Sets ending values (image position, rotation, etc) whenever the animation is about to begin.
This method needs to be overloaded to implement animations.
!!}
procedure TIEAnimation.SetEndValues();
var
  i: integer;
begin
  for i := 0 to length(m_images)-1 do
    Images[i].endTime := Images[i].startTime + AnimDuration;
  NeedRefresh := true;
end;

 
{!!
<FS>TIEAnimation.Font

<FM>Declaration<FC>
property Font: TFont;

<FM>Description<FN>
The font of the text if <A TIEAnimation.ShowText> is enabled.

Default values are: Height: 18; Style: [fsBold]; Color: clWhite; Name: 'Arial'
        
<FM>Example<FC>
ImageEnMView1.Animation.Font.Color  := clRed;
ImageEnMView1.Animation.Font.Height := 30;
ImageEnMView1.Animation.Font.Name   := 'Tahoma';
ImageEnMView1.Animation.Font..Style := [fsBold];
ImageEnMView1.Update;
!!}

procedure TIEAnimation.SetFont(const Value: TFont);
begin
  fFont := Value;
  NeedRefresh := true;
end;

{!!
<FS>TIEAnimation.RestartAnimation

<FM>Declaration<FC>
procedure RestartAnimation();

<FM>Description<FN>
Restarts the animation.
!!}
procedure TIEAnimation.RestartAnimation();
begin
  SetStartEndValues();
end;


{!!
<FS>TIEAnimation.Images

<FM>Declaration<FC>
property Images[index: integer]: <A TIEAnimationImageInfo>;

<FM>Description<FN>
Get or set the animation parameters for the image of the specified index.

<FM>Example<FC>
animation.Images[0].startAngleX := 45;
!!}
function TIEAnimation.GetImages(idx: integer): TIEAnimationImageInfo;
begin
  result := m_images[idx];
end;


function TIEAnimation.isVisible(const coords: TIEQuadCoords): boolean;
begin
  result := IEPointInRect(coords.x0, coords.y0, 0, 0, m_viewWidth-1, m_viewHeight-1) or IEPointInRect(coords.x1, coords.y1, 0, 0, m_viewWidth-1, m_viewHeight-1) or
            IEPointInRect(coords.x2, coords.y2, 0, 0, m_viewWidth-1, m_viewHeight-1) or IEPointInRect(coords.x3, coords.y3, 0, 0, m_viewWidth-1, m_viewHeight-1);
end;



{!!
<FS>TIEAnimation.Display

<FM>Declaration<FC>
procedure Display(dest: <A TIEBitmap>); virtual; abstract;

<FM>Description<FN>
This method must be implemented by the subclass. It should loop among images calling <A TIEAnimation.drawImage> for each image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>The destination bitmap.</C> </R>
</TABLE>
!!}



{!!
<FS>TIEAnimation.drawImage

<FM>Declaration<FC>
function drawImage(dest: <A TIEBitmap>; imageIndex: integer): boolean; virtual;

<FM>Description<FN>
Draws the specified image. This method reads the timers to draw the image correctly (at calculated position, rotation, etc...).
Returns true when the image doesn't need to be drawn again (the animation is finished).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>The destination bitmap. Must be at least <A TIEAnimation.ViewWidth> and <A TIEAnimation.ViewHeight> pixels.</C> </R>
<R> <C><FC>imageIndex<FN></C> <C>The image index to draw.</C> </R>
</TABLE>
!!}
function TIEAnimation.drawImage(dest: TIEBitmap; imageIndex: integer): boolean;
var
  image: TIEAnimationImageInfo;
  thisStep: double; // 0..1
  bmp, abmp: TIEBitmap;
  lastCenterX, lastCenterY: integer;
  lastAngleX, lastAngleY: double;
  lastWidth, lastHeight: integer;
  lastAlpha: integer;
  text: WideString;
begin
  image := m_images[imageIndex];

  thisStep := (GetTickCount()-image.startTime) / (image.endTime-image.startTime); // linear 0..1
  if thisStep < 0.0 then thisStep := 0.0;
  if thisStep > 1.0 then thisStep := 1.0;
  thisStep := thisStep*thisStep*thisStep + 1.0*3.0*thisStep*thisStep*(1.0-thisStep) + 0.1*3.0*thisStep*(1.0-thisStep)*(1.0-thisStep);  // bezier-bicubic
  if thisStep >= 1.0 then
  begin
    // go directly to ending values
    image.startAlpha   := image.endAlpha;
    image.startAngleX  := image.endAngleX;
    image.startAngleY  := image.endAngleY;
    image.startCenterX := image.endCenterX;
    image.startCenterY := image.endCenterY;
    image.startWidth   := image.endWidth;
    image.startHeight  := image.endHeight;
  end;

  lastAlpha   := trunc( image.startAlpha + (image.endAlpha - image.startAlpha) * thisStep );
  lastCenterX := trunc( image.startCenterX + (image.endCenterX - image.startCenterX) * thisStep );
  lastCenterY := trunc( image.startCenterY + (image.endCenterY - image.startCenterY) * thisStep );
  lastAngleX  := image.startAngleX + (image.endAngleX - image.startAngleX) * thisStep;
  lastAngleY  := image.startAngleY + (image.endAngleY - image.startAngleY) * thisStep;
  lastWidth   := trunc( image.startWidth + (image.endWidth - image.startWidth) * thisStep );
  lastHeight  := trunc( image.startHeight + (image.endHeight - image.startHeight) * thisStep );

  result := (lastCenterX <> image.endCenterX) or
            (lastCenterY <> image.endCenterY) or
            (lastAngleX  <> image.endAngleX)  or
            (lastAngleY  <> image.endAngleY)  or
            (lastWidth   <> image.endWidth)   or
            (lastHeight  <> image.endHeight)  or
            (lastAlpha   <> image.endAlpha);

  image.lastAlpha   := lastAlpha;
  image.lastCenterX := lastCenterX;
  image.lastCenterY := lastCenterY;
  image.lastAngleX  := lastAngleX;
  image.lastAngleY  := lastAngleY;
  image.lastWidth   := lastWidth;
  image.lastHeight  := lastHeight;

  if (image.lastWidth = 0) or (image.lastHeight = 0) then
    exit;

  // calculate image rect
  IEProjectBitmap1(nil, nil, 
                   image.lastCenterX, image.lastCenterY, // centerDstX, centerDstY
                   image.lastWidth, image.lastHeight,    // dstWidth, dstHeight
                   0, 0,                                 // translateX, translateY
                   m_depth,                              // depth
                   image.lastAngleX, image.lastAngleY,   // rotateX, rotateY
                   m_shadowAlphaMin, m_shadowAlphaMax,   // specularAlphaMin, specularAlphaMax
                   image.lastCoords,                     // output coordinates
                   ieovoCALCRECTONLY, true, 
                   image.lastAlpha);
  if isVisible(image.lastCoords) and (image.lastWidth > 0) and (image.lastHeight > 0) then
  begin
    bmp := nil;
    DoGetImage(imageIndex, bmp, text);
    abmp := TIEBitmap.Create(image.lastWidth, image.lastHeight, bmp.PixelFormat);
    if result then
      _IEResampleIEBitmap(bmp, abmp, m_framesZoomFilter, nil, nil)
    else
      _IEResampleIEBitmap(bmp, abmp, m_lastFrameZoomFilter, nil, nil);
    abmp.PixelFormat := ie24RGB;
    IEProjectBitmap1(abmp, dest,
                     0, 0,                                // centerDstX, centerDstY
                     0, 0,                                // dstWidth, dstHeight
                     0, 0,                                // translateX, translateY
                     0.0,                                 // depth
                     0.0, 0.0,                            // rotateX, rotateY
                     m_shadowAlphaMin, m_shadowAlphaMax,  // specularAlphaMin, specularAlphaMax
                     image.lastCoords,                    // output coordinates
                     ieovoDRAWONLY, true,
                     image.lastAlpha);
    abmp.Free();
    DoReleaseImage(imageIndex, bmp);

    // draw border
    if m_showBorder then
      PaintBorder(imageIndex, dest);
  end;
end;



{!!
<FS>TIEAnimation.FindImageAt

<FM>Declaration<FC>
function FindImageAt(X, Y: integer): integer;

<FM>Description<FN>
Returns the index of the image that is under the specified coordinates, or -1 if there is no image.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>X<FN></C> <C>Horizontal coordinate.</C> </R>
<R> <C><FC>Y<FN></C> <C>Vertical coordinate.</C> </R>
</TABLE>
!!}
function TIEAnimation.FindImageAt(X, Y: integer): integer;
  function IsInsideImage(i: integer): boolean;
  begin
    if (i >= length(m_images)) or (i < 0) then
      result := false
    else
      with m_images[i].lastCoords do
        result := IEISPointInPoly(X, Y, [Point(x0, y0), Point(x1, y1), Point(x2, y2), Point(x3, y3)]);
  end;
var
  i: integer;
begin
  // look for current image
  if IsInsideImage(m_currentImage) then
  begin
    result := m_currentImage;
    exit;
  end
  else
  begin
    // look on the left
    for i := m_currentImage-1 downto 0 do
      if IsInsideImage(i) then
      begin
        result := i;
        exit;
      end;
    // look on the right
    for i := m_currentImage+1 to length(m_images)-1 do
      if IsInsideImage(i) then
      begin
        result := i;
        exit;
      end;
  end;
  result := -1;
end;

// thread safe
procedure TIEAnimation.PaintBorder(i: integer; dest: TIEBitmap);
begin
  if assigned(m_borderPen) then
  begin
    EnterCriticalSection(m_canvasCriticalSection);
    with TIECanvas.Create(dest.Canvas) do
    begin
      Pen.Style := m_borderPen.Style;
      Pen.Mode  := m_borderPen.Mode;
      Pen.Color := m_borderPen.Color;
      Pen.Width := m_borderPen.Width;
      Brush.style := bsClear;
      with m_images[i].lastCoords do
      begin
        MoveTo(x0, y0);
        LineTo(x1, y1);
        LineTo(x2, y2);
        LineTo(x3, y3);
        LineTo(x0, y0);
      end;
      Free();
    end;
    LeaveCriticalSection(m_canvasCriticalSection);
  end;
end;


{!!
<FS>TIEAnimation.PaintText

<FM>Declaration<FC>
procedure PaintText(posY: integer; dest: <A TIEBitmap>);

<FM>Description<FN>
Paints the text (ie filename) at the specified row. You should call this method inside <A TIEAnimation.Display> method.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>posY<FN></C> <C>Vertical position where to draw the text.</C> </R>
<R> <C><FC>dest<FN></C> <C>The destination bitmap.</C> </R>
</TABLE>
!!}
procedure TIEAnimation.PaintText(posY: integer; dest: TIEBitmap);
const
  Default_Font_Height = 18; // Fixed font height used prior to v5.0.7
var
  posX: integer;
  text: WideString;
  iw, ih: integer;
  iTextOffsetY: Integer;
begin
  if m_showText then
  begin
    with TIECanvas.Create(dest.Canvas) do
    begin
      Brush.Style := bsClear; 
      Font.Assign(fFont);

      DoGetImageInfo(CurrentImage, true, iw, ih, text);
      posX := Images[CurrentImage].endCenterX - TextWidth(text) div 2;

      // Adjust text position for increased font size, while maintaining legacy support
      iTextOffsetY := (Default_Font_Height - fFont.Height) div 2;

      TextOut(posX, posY + iTextOffsetY, text);
      Free();
    end;
  end;
end;


{!!
<FS>TIEAnimation.PaintScrollBar

<FM>Declaration<FC>
procedure PaintScrollBar(dest: <A TIEBitmap>);

<FM>Description<FN>
Paints the scroll bar. You should call this method inside <A TIEAnimation.Display> method.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>dest<FN></C> <C>The destination bitmap.</C> </R>
</TABLE>
!!}
procedure TIEAnimation.PaintScrollBar(dest: TIEBitmap);
const
  VERTGAP    = 5;   // gap from bottom border
  XPER       = 0.8; // 80% of the width
  HEIGHT     = 16;  // height in pixels
  RX         = 12;  // round width
  RY         = 12;  // round height
  BUTTONW    = 18;  // buttons width
  MINSLIDERW = 8;   // minimum slider width
var
  boundwidth: integer;
  sliderWidth, asliderWidth: double;
begin
  if m_showScrollbar then
  begin
    with TIECanvas.Create(dest.Canvas) do
    begin
      // paint brush and border
      Brush.Style := bsSolid;
      Brush.Color := clWhite;
      Brush.Transparency := 64;
      Pen.Color := clWhite;
      Pen.Style := psSolid;
      Pen.Mode := pmCopy;
      Pen.Width := 1;
      Pen.Transparency := 128;
      boundwidth := trunc(ViewWidth * XPER); // width of the scrollbar
      m_lastScrollbarBoundRect.x := (ViewWidth - boundwidth) div 2;
      m_lastScrollbarBoundRect.y := ViewHeight - HEIGHT - VERTGAP;
      m_lastScrollbarBoundRect.width := boundwidth;
      m_lastScrollbarBoundRect.height := HEIGHT;
      with m_lastScrollbarBoundRect do
        RoundRect(x, y, x+width, y+height, RX, RY);

      // paint slider
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      Brush.Transparency := 128;
      Pen.Width := 1;
      Pen.Transparency := 128;
      sliderWidth := boundwidth / ImageCount;
      asliderWidth := dmax(MINSLIDERW, sliderWidth);
      m_lastScrollbarSlideRect.x := m_lastScrollbarBoundRect.x + trunc(CurrentImage * sliderWidth);
      if m_lastScrollbarSlideRect.x + asliderWidth > m_lastScrollbarBoundRect.x + m_lastScrollbarBoundRect.width then
        m_lastScrollbarSlideRect.x := trunc(m_lastScrollbarBoundRect.x + m_lastScrollbarBoundRect.width - asliderWidth);
      m_lastScrollbarSlideRect.y := m_lastScrollbarBoundRect.y + 1;
      m_lastScrollbarSlideRect.width := trunc(asliderWidth);
      m_lastScrollbarSlideRect.height := m_lastScrollbarBoundRect.height - 1;
      with m_lastScrollbarSlideRect do
        RoundRect(x, y, x+width, y+height, RX, RY);

      Free();
    end;
  end;
end;


{!!
<FS>TIEAnimation.MoveScrollbarSliderTo

<FM>Declaration<FC>
procedure MoveScrollbarSliderTo(X: integer);

<FM>Description<FN>
Moves the scrollbar slider to the specified horizontal position.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>X<FN></C> <C>Horizontal slider position.</C> </R>
</TABLE>

<FM>Example<FC>
procedure TMainForm.ImageEnView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // directly move to the clicked scrollbar position
  if m_animation.IsInsideScrollbar(X, Y) then
    m_animation.MoveScrollbarSliderTo(X);
end;
!!}
procedure TIEAnimation.MoveScrollbarSliderTo(X: integer);
begin
  CurrentImage := round((X - m_lastScrollbarBoundRect.x - m_lastScrollbarSlideRect.width / 2) / (m_lastScrollbarBoundRect.width / ImageCount));
end;


{!!
<FS>TIEAnimation.IsInsideScrollbarSlider

<FM>Declaration<FC>
function IsInsideScrollbarSlider(X, Y: integer): boolean;

<FM>Description<FN>
Returns true if the specified coordinates are inside the scrollbar slider.
This function is useful to start dragging the scrollbar slider.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>X<FN></C> <C>Horizontal coordinate.</C> </R>
<R> <C><FC>Y<FN></C> <C>Vertical coordinate.</C> </R>
</TABLE>

<FM>See Also<FN>
- <A TIEAnimation.MoveScrollbarSliderTo>
!!}
function TIEAnimation.IsInsideScrollbarSlider(X, Y: integer): boolean;
begin
  result := IEPointInRect(X, Y, 
                          m_lastScrollbarSlideRect.x, 
                          m_lastScrollbarSlideRect.y, 
                          m_lastScrollbarSlideRect.x + m_lastScrollbarSlideRect.width, 
                          m_lastScrollbarSlideRect.y + m_lastScrollbarSlideRect.height);
end;


{!!
<FS>TIEAnimation.IsInsideScrollbar

<FM>Declaration<FC>
function IsInsideScrollbar(X, Y: integer): boolean;

<FM>Description<FN>
Returns true if the specified coordinates are inside the scrollbar.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>X<FN></C> <C>Horizontal coordinate.</C> </R>
<R> <C><FC>Y<FN></C> <C>Vertical coordinate.</C> </R>
</TABLE>
!!}
function TIEAnimation.IsInsideScrollbar(X, Y: integer): boolean;
begin
  result := IEPointInRect(X, Y,
                          m_lastScrollbarBoundRect.x,
                          m_lastScrollbarBoundRect.y,
                          m_lastScrollbarBoundRect.x + m_lastScrollbarBoundRect.width,
                          m_lastScrollbarBoundRect.y + m_lastScrollbarBoundRect.height);
end;


{!!
<FS>TIEAnimation.SetStartEndValues

<FM>Declaration<FC>
procedure SetStartEndValues();

<FM>Description<FN>
This method just calls <A TIEAnimation.SetStartValues> and <A TIEAnimation.SetEndValues>.
!!}
procedure TIEAnimation.SetStartEndValues();
begin
  SetStartValues();
  SetEndValues();
end;


{!!
<FS>TIEAnimation.CurrentImage

<FM>Declaration<FC>
property CurrentImage: integer;

<FM>Description<FN>
Specifies current image index. Current image is often the most visible or the central image.
!!}
procedure TIEAnimation.SetCurrentImage(value: integer);
begin
  if (value <> m_currentImage) then
  begin
    m_currentImage := imin(imax(0, value), length(m_images)-1);
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEAnimation.ViewWidth

<FM>Declaration<FC>
property ViewWidth: integer;

<FM>Description<FN>
Specifies the width of area reserved for the animation.

<FM>See Also<FN>
- <A TIEAnimation.ViewHeight>

<FM>Example<FC>
animation.ViewWidth := ImageEnView.IEBitmap.Width;
animation.ViewHeight := ImageEnView.IEBitmap.Height;
!!}
procedure TIEAnimation.SetViewWidth(value: integer);
begin
  if (value <> m_viewWidth) and (value > 0) then
  begin
    m_viewWidth := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEAnimation.ViewHeight

<FM>Declaration<FC>
property ViewHeight: integer;

<FM>Description<FN>
Specifies the height of area reserved for the animation.

<FM>See Also<FN>
- <A TIEAnimation.ViewWidth>

<FM>Example<FC>
animation.ViewWidth := ImageEnView.IEBitmap.Width;
animation.ViewHeight := ImageEnView.IEBitmap.Height;
!!}
procedure TIEAnimation.SetViewHeight(value: integer);
begin
  if (value <> m_viewHeight) and (value > 0) then
  begin
    m_viewHeight := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEAnimation.Depth

<FM>Declaration<FC>
property Depth: integer;

<FM>Description<FN>
Specifies the 3D projection depth. Default is 500.
!!}
procedure TIEAnimation.SetDepth(value: integer);
begin
  if (value <> m_depth) and (value > 0) then
  begin
    m_depth := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEAnimation.ShowBorder

<FM>Declaration<FC>
property ShowBorder: boolean;

<FM>Description<FN>
If true a border will be painted around all images.

See also: <A TIEAnimation.BorderPen>.
!!}
procedure TIEAnimation.SetShowBorder(value: boolean);
begin
  if (value <> m_showBorder) then
  begin
    m_showBorder := value;
    m_needRefresh := true;
  end;
end;


{!!
<FS>TIEAnimation.ShadowAlphaMin

<FM>Declaration<FC>
property ShadowAlphaMin: integer;

<FM>Description<FN>
Specifies the minimum alpha value for the shadow. Default is 0.
!!}
procedure TIEAnimation.SetShadowAlphaMin(value: integer);
begin
  if (value <> m_shadowAlphaMin) then
  begin
    m_shadowAlphaMin := value;
    m_needRefresh := true;
  end;
end;


{!!
<FS>TIEAnimation.ShadowAlphaMax

<FM>Declaration<FC>
property ShadowAlphaMax: integer;

<FM>Description<FN>
Specifies the maximum alpha value for the shadow. Default is 40.
!!}
procedure TIEAnimation.SetShadowAlphaMax(value: integer);
begin
  if (value <> m_shadowAlphaMax) then
  begin
    m_shadowAlphaMax := value;
    m_needRefresh := true;
  end;
end;


{!!
<FS>TIEAnimation.ShowScrollbar

<FM>Declaration<FC>
property ShowScrollbar: boolean;

<FM>Description<FN>
If true the scrollbar will be painted.
!!}
procedure TIEAnimation.SetShowScrollbar(value: boolean);
begin
  if (value <> m_showScrollbar) then
  begin
    m_showScrollbar := value;
    NeedRefresh := true;
  end;
end;


{!!
<FS>TIEAnimation.ShowText

<FM>Declaration<FC>
property ShowText: boolean;

<FM>Description<FN>
Whether the current image text will be painted. Styling of the text is specified using <A TIEAnimation.Font>
!!}
procedure TIEAnimation.SetShowText(value: boolean);
begin
  if (value <> m_showText) then
  begin
    m_showText := value;
    NeedRefresh := true;
  end;
end;


{!!
<FS>TIEAnimation.DoGetImageInfo

<FM>Declaration<FC>
procedure DoGetImageInfo(imageIndex: integer; isVisible: boolean; var imageWidth: integer; var imageHeight: integer; var text: WideString);

<FM>Description<FN>
Calls the <A TIEAnimationGetImageInfoEvent> event, if assigned on creation. If no event was assigned just calls the <A TIEAnimationGetImageEvent> event.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>imageIndex<FN></C> <C>Index of the required image info.</C> </R>
<R> <C><FC>isVisible<FN></C> <C>Is True if the required image could be not visible (hence you can provide estimated values).</C> </R>
<R> <C><FC>imageWidth<FN></C> <C>Return value for image width.</C> </R>
<R> <C><FC>imageHeight<FN></C> <C>Return value for image height</C> </R>
<R> <C><FC>text<FN></C> <C>Return value for image textual info (ie a filename).</C> </R>
</TABLE>
!!}
procedure TIEAnimation.DoGetImageInfo(imageIndex: integer; isVisible: boolean; var imageWidth: integer; var imageHeight: integer; var text: WideString);
var
  image: TIEBitmap;
begin
  if assigned(m_getImageInfo) then
    m_getImageInfo(self, imageindex, isVisible, imageWidth, imageHeight, text)
  else
  begin
    DoGetImage(imageIndex, image, text);
    imageWidth  := image.Width;
    imageHeight := image.Height;
    DoReleaseImage(imageIndex, image);
  end;
end;


{!!
<FS>TIEAnimation.DoGetImage

<FM>Declaration<FC>
procedure DoGetImage(imageIndex: integer; var image: <A TIEBitmap>; var text: WideString);

<FM>Description<FN>
Calls the <A TIEAnimationGetImageEvent> event, assigned on creation.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>imageIndex<FN></C> <C>Index of the required image.</C> </R>
<R> <C><FC>image<FN></C> <C>Return value for image bitmap.</C> </R>
<R> <C><FC>text<FN></C> <C>Return value for image textual info (ie a filename).</C> </R>
</TABLE>
!!}
procedure TIEAnimation.DoGetImage(imageIndex: integer; var image: TIEBitmap; var text: WideString);
begin
  if assigned(m_getImage) then
    m_getImage(self, imageIndex, image, text)
  else
    raise Exception.Create('GetImageEvent not assigned');
end;


{!!
<FS>TIEAnimation.DoReleaseImage

<FM>Declaration<FC>
procedure DoReleaseImage(imageIndex: integer; var image: TIEBitmap);

<FM>Description<FN>
Calls the <A TIEAnimationReleaseImageEvent> event, if assigned on creation.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>imageIndex<FN></C> <C>Index of the image to release.</C> </R>
<R> <C><FC>image<FN></C> <C>The bitmap to release.</C> </R>
</TABLE>
!!}
procedure TIEAnimation.DoReleaseImage(imageIndex: integer; var image: TIEBitmap);
begin
  if assigned(m_releaseImage) then
    m_releaseImage(self, imageIndex, image);
end;




///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
// TIEHorizontalFlow

constructor TIEHorizontalFlow.Create();
begin
  // default values for properties
  m_horizontalDistance         := 80;
  m_imagesHorizontalPercentage := 40;
  m_imagesVerticalPercentage   := 80;
  m_rotateAngle                := 40;
  m_imagesZoom                 := 0.7;
  m_currentImageZoom           := 1.0;

  inherited;
end;

destructor TIEHorizontalFlow.Destroy();
begin
  inherited;
end;

procedure TIEHorizontalFlow.SetInitialValues(first, last: integer);
var
  i: integer;
begin
  inherited;
  for i := first to last do
  begin
    Images[i].startAlpha   := 0;
    Images[i].startAngleX  := 0.0;
    Images[i].startAngleY  := m_rotateAngle;
    Images[i].startCenterX := ViewWidth div 2 + i * m_horizontalDistance;
    Images[i].startCenterY := trunc(ViewHeight * m_imagesVerticalPercentage / 100);
    Images[i].startWidth   := 0;
    Images[i].startHeight  := 0;
  end;
end;

procedure TIEHorizontalFlow.SetStartValues();
begin
  inherited;
end;

procedure TIEHorizontalFlow.SetEndValues();
var
  i: integer;
  viewCenterX: integer;
  width, height, imWidth, imHeight, w, h: integer;
  filename: WideString;
  isVis: boolean;
  endx: integer;
begin
  inherited;
  if ImageCount > 0 then
  begin
    viewCenterX := ViewWidth div 2;
    width := trunc(ViewWidth * m_imagesHorizontalPercentage / 100);
    height := trunc(ViewHeight * m_imagesVerticalPercentage / 100);

    // images on the left
    for i := 0 to CurrentImage-1 do
    begin
      endx := viewCenterX - ((CurrentImage-i) * m_horizontalDistance) - width div 2;

      isVis := endx + m_horizontalDistance >= 0;
      DoGetImageInfo(i, isVis, imWidth, imHeight, filename);
      IEGetFitResampleSize(imWidth, imHeight, width, height, w, h);
      w := trunc(w * m_imagesZoom);
      h := trunc(h * m_imagesZoom);

      Images[i].endAlpha   := 255;
      Images[i].endAngleX  := 0.0;
      Images[i].endAngleY  := -m_rotateAngle;
      Images[i].endCenterX := endx;
      Images[i].endCenterY := height - h div 2;
      Images[i].endWidth   := w;
      Images[i].endheight  := h;
    end;

    // images on the right
    for i := CurrentImage+1 to ImageCount-1 do
    begin
      endx := viewCenterX + ((i-CurrentImage) * m_horizontalDistance) + width div 2;

      isVis := endx - m_horizontalDistance <= ViewWidth;
      DoGetImageInfo(i, isVis, imWidth, imHeight, filename);
      IEGetFitResampleSize(imWidth, imHeight, width, height, w, h);
      w := trunc(w * m_imagesZoom);
      h := trunc(h * m_imagesZoom);

      Images[i].endAlpha   := 255;
      Images[i].endAngleX  := 0.0;
      Images[i].endAngleY  := m_rotateAngle;
      Images[i].endCenterX := endx;
      Images[i].endCenterY := height - h div 2;
      Images[i].endWidth   := w;
      Images[i].endheight  := h;
    end;

    // image at the center (CurrentImage)
    DoGetImageInfo(CurrentImage, true, imWidth, imHeight, filename);
    IEGetFitResampleSize(imWidth, imHeight, width, height, w, h);
    w := trunc(w * m_currentImageZoom);
    h := trunc(h * m_currentImageZoom);
    Images[CurrentImage].endAlpha   := 255;
    Images[CurrentImage].endAngleX  := 0.0;
    Images[CurrentImage].endAngleY  := 0.0;
    Images[CurrentImage].endCenterX := ViewCenterX;
    Images[CurrentImage].endCenterY := height - h div 2;
    Images[CurrentImage].endWidth   := w;
    Images[CurrentImage].endHeight  := h;
  end;
end;


type TIEHorizontalFlowThreadSide = (iecfLEFT, iecfRIGHT);

type TIEHorizontalFlowThread = class(TThread)
private
  m_horizontalflow: TIEHorizontalFlow;
  m_side: TIEHorizontalFlowThreadSide;
  m_needrefresh: boolean;
  m_dest: TIEBitmap;
public
  procedure Execute; override;
  constructor Create(horizontalflow: TIEHorizontalFlow; side: TIEHorizontalFlowThreadSide; dest: TIEBitmap);
end;

constructor TIEHorizontalFlowThread.Create(horizontalflow: TIEHorizontalFlow; side: TIEHorizontalFlowThreadSide; dest: TIEBitmap);
begin
  inherited Create(true);
  m_horizontalflow := horizontalflow;
  m_side           := side;
  m_needrefresh    := false;
  m_dest           := dest;
end;

procedure TIEHorizontalFlowThread.Execute;
var
  i: integer;
begin
  if m_side = iecfLEFT then
  begin
    // images on the left
    for i := 0 to m_horizontalflow.CurrentImage-1 do
      m_needRefresh := m_horizontalflow.drawImage(m_dest, i) or m_needRefresh;
  end
  else
  begin
    // images on the right
    for i := m_horizontalflow.ImageCount-1 downto m_horizontalflow.CurrentImage+1 do
      m_needRefresh := m_horizontalflow.drawImage(m_dest, i) or m_needRefresh;
  end;
end;

procedure TIEHorizontalFlow.Display(dest: TIEBitmap);
var
  threads: TIEThreadPool;
begin
  NeedRefresh := false;

  if ImageCount > 0 then
  begin
    threads := TIEThreadPool.Create();
    threads.Add( TIEHorizontalFlowThread.Create(self, iecfLEFT, dest) );
    threads.Add( TIEHorizontalFlowThread.Create(self, iecfRIGHT, dest) );
    threads.WaitFor();
    NeedRefresh := (threads[0] as TIEHorizontalFlowThread).m_needrefresh or (threads[1] as TIEHorizontalFlowThread).m_needrefresh;
    threads.Free();

    // central image
    NeedRefresh := drawImage(dest, CurrentImage) or NeedRefresh;

    // draw filename of central image
    PaintText(ViewHeight div 2 + trunc(ViewHeight * m_imagesVerticalPercentage / 100) div 2, dest);

    // draw scrollbar
    PaintScrollBar(dest);
  end;
end;


{!!
<FS>TIEHorizontalFlow.HorizontalDistance

<FM>Declaration<FC>
property HorizontalDistance: integer;

<FM>Description<FN>
Specifies the horizontal distance among images, in pixels. Default is 80.
!!}
procedure TIEHorizontalFlow.SetHorizontalDistance(value: integer);
begin
  if (value <> m_horizontalDistance) and (value > 0) then
  begin
    m_horizontalDistance := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEHorizontalFlow.ImagesHorizontalPercentage

<FM>Declaration<FC>
property ImagesHorizontalPercentage: double;

<FM>Description<FN>
Specifies the percentage (1..100) of view-width used by a single image. Default is 40.
!!}
procedure TIEHorizontalFlow.SetImagesHorizontalPercentage(value: double);
begin
  if (value <> m_imagesHorizontalPercentage) and (value > 0) then
  begin
    m_imagesHorizontalPercentage := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEHorizontalFlow.ImagesVerticalPercentage

<FM>Declaration<FC>
property ImagesVerticalPercentage: double;

<FM>Description<FN>
Specifies the percentage (1..100) of view-height used by a single image. Default is 80.
!!}
procedure TIEHorizontalFlow.SetImagesVerticalPercentage(value: double);
begin
  if (value <> m_imagesVerticalPercentage) and (value > 0) then
  begin
    m_imagesVerticalPercentage := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEHorizontalFlow.RotateAngle

<FM>Declaration<FC>
property RotateAngle: double;

<FM>Description<FN>
Specifies the non-central images rotation in degrees (0..359). Default is 40 degrees.
!!}
procedure TIEHorizontalFlow.SetRotateAngle(value: double);
begin
  if (value <> m_rotateAngle) then
  begin
    m_rotateAngle := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEHorizontalFlow.ImagesZoom

<FM>Declaration<FC>
property ImagesZoom: double;

<FM>Description<FN>
Specifies the zoom (0..1) for non-central images. Default is 0.7.
!!}
procedure TIEHorizontalFlow.SetImagesZoom(value: double);
begin
  if (value <> m_imagesZoom) then
  begin
    m_imagesZoom := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIEHorizontalFlow.CurrentImageZoom

<FM>Declaration<FC>
property CurrentImageZoom: double;

<FM>Description<FN>
Specifies the zoom (0..1) for central image. Default is 1.0.
!!}
procedure TIEHorizontalFlow.SetCurrentImageZoom(value: double);
begin
  if (value <> m_currentImageZoom) then
  begin
    m_currentImageZoom := value;
    SetStartEndValues();
  end;
end;



///////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////
// TIECircularFlow

constructor TIECircularFlow.Create();
begin
  // default parameters
  m_imagesSizePercentage := 40;
  m_imagesZoom           := 0.2;
  m_currentImageZoom     := 1.0;
  m_visibleImages        := 15;
  m_ellipseAngle         := 0;

  inherited;
end;

destructor TIECircularFlow.Destroy();
begin
  inherited;
end;

procedure TIECircularFlow.SetInitialValues(first, last: integer);
var
  i: integer;
begin
  inherited;
  for i := first to last do
  begin
    Images[i].startAlpha   := 0;
    Images[i].startAngleX  := 0.0;
    Images[i].startAngleY  := 0.0;
    Images[i].startCenterX := ViewWidth div 2;
    Images[i].startCenterY := ViewHeight div 2;
    Images[i].startWidth   := 0;
    Images[i].startHeight  := 0;
  end;
end;

procedure TIECircularFlow.SetStartValues();
var
  i: integer;
begin
  inherited;
  for i := 0 to ImageCount-1 do
  begin
    Images[i].startAlpha   := Images[i].lastAlpha;
    Images[i].startAngleX  := 0.0;
    Images[i].startAngleY  := Images[i].lastAngleY;
    Images[i].startCenterX := Images[i].lastCenterX;
    Images[i].startCenterY := Images[i].lastCenterY;
    Images[i].startWidth   := Images[i].lastWidth;
    Images[i].startHeight  := Images[i].lastHeight;
  end;
end;

procedure TIECircularFlow.SetEndValues();
var
  i, a, iindex: integer;
  viewCenterX, viewCenterY: integer;
  alpha, beta, sinbeta, cosbeta, sinalpha, cosalpha: double;
  ellipseX, ellipseY: integer;
  x, y: integer;
  width, height, imWidth, imHeight, w, h: integer;
  filename: WideString;
  steps: integer;
  m_ellipseRatio: double;
  semimaj, semimin: double;
begin
  inherited;
  if ImageCount > 0 then
  begin
    m_ellipseRatio := 0.7;

    viewCenterX := ViewWidth div 2;
    viewCenterY := ViewHeight div 2;
    width := trunc(ViewWidth * m_imagesSizePercentage / 100);
    height := trunc(ViewHeight * m_imagesSizePercentage / 100);
    steps := imin(ImageCount, m_visibleImages);
    semimaj := m_ellipseRatio * imin(ViewWidth-width, ViewHeight-height);
    semimin := (1.0-m_ellipseRatio) * imin(ViewWidth-width, ViewHeight-height);

    for i := 0 to ImageCount-1 do
    begin
      Images[i].endAlpha   := 0;
      Images[i].endAngleX  := 0.0;
      Images[i].endAngleY  := 0.0;
      Images[i].endCenterX := viewCenterX;
      Images[i].endCenterY := viewCenterY;
      Images[i].endWidth   := 0;
      Images[i].endheight  := 0;
    end;

    // build ellipse points
    ellipseX := viewCenterX;
    ellipseY := viewCenterY;
    beta := -m_ellipseAngle * (PI / 180);
    sinbeta := sin(beta);
    cosbeta := cos(beta);
    a := 90;
    iindex := CurrentImage;
    for i := 0 to STEPS-1 do
    begin
      alpha := a * (PI / 180) ;
      sinalpha := sin(alpha);
      cosalpha := cos(alpha);
      x := round( ellipseX + (semimaj * cosalpha * cosbeta - semimin * sinalpha * sinbeta) );
      y := round( ellipseY + (semimaj * cosalpha * sinbeta + semimin * sinalpha * cosbeta) );

      DoGetImageInfo(iindex, true, imWidth, imHeight, filename);
      IEGetFitResampleSize(imWidth, imHeight, width, height, w, h);
      if (iindex <> CurrentImage) then
      begin
        w := trunc(w * m_imagesZoom);
        h := trunc(h * m_imagesZoom);
        Images[iindex].endAlpha   := 255;
        Images[iindex].endAngleX  := 0.0;
        Images[iindex].endAngleY  := 0.0;
        Images[iindex].endCenterX := x;
        Images[iindex].endCenterY := y;
        Images[iindex].endWidth   := round(w * (1.5+sinalpha));
        Images[iindex].endheight  := round(h * (1.5+sinalpha));
      end
      else
      begin
        w := trunc(w * m_currentImageZoom);
        h := trunc(h * m_currentImageZoom);
        Images[CurrentImage].endAlpha   := 255;
        Images[CurrentImage].endAngleX  := 0.0;
        Images[CurrentImage].endAngleY  := 0.0;
        Images[CurrentImage].endCenterX := viewCenterX;
        Images[CurrentImage].endCenterY := y - h div 4;
        Images[CurrentImage].endWidth   := w;
        Images[CurrentImage].endheight  := h;
      end;

      dec(a, 360 div STEPS);
      inc(iindex);
      if iindex >= ImageCount then
        iindex := 0;

    end;
  end;
end;


procedure TIECircularFlow.Display(dest: TIEBitmap);
var
  i: integer;
begin
  NeedRefresh := false;

  if ImageCount > 0 then
  begin
    for i := ImageCount-1 downto 0 do
    begin
      if i <> CurrentImage then
        NeedRefresh := drawImage(dest, i) or NeedRefresh;
    end;

    // central image
    NeedRefresh := drawImage(dest, CurrentImage) or NeedRefresh;

    // draw filename of central image
    //PaintText(ViewHeight div 2 - 50, dest);
    PaintText(ViewHeight div 2 + trunc(ViewHeight * m_imagesSizePercentage / 100), dest);

    // draw scrollbar
    PaintScrollBar(dest);
  end;
end;


{!!
<FS>TIECircularFlow.ImagesSizePercentage

<FM>Declaration<FC>
property ImagesSizePercentage: double;

<FM>Description<FN>
Specifies the percentage (1..100) of view size used for images. Default is 40.
!!}
procedure TIECircularFlow.SetImagesSizePercentage(value: double);
begin
  if (value <> m_imagesSizePercentage) and (value > 0) then
  begin
    m_imagesSizePercentage := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIECircularFlow.ImagesZoom

<FM>Declaration<FC>
property ImagesZoom: double;

<FM>Description<FN>
Specifies the zoom (0..1) for non-central images. Default is 0.2.
!!}
procedure TIECircularFlow.SetImagesZoom(value: double);
begin
  if (value <> m_imagesZoom) then
  begin
    m_imagesZoom := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIECircularFlow.CurrentImageZoom

<FM>Declaration<FC>
property CurrentImageZoom: double;

<FM>Description<FN>
Specifies the zoom (0..1) for central image. Default is 1.0.
!!}
procedure TIECircularFlow.SetCurrentImageZoom(value: double);
begin
  if (value <> m_currentImageZoom) then
  begin
    m_currentImageZoom := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIECircularFlow.VisibleImages

<FM>Declaration<FC>
property VisibleImages: integer;

<FM>Description<FN>
Specifies the number of visible images. Deault is 15.
!!}
procedure TIECircularFlow.SetVisibleImages(value: integer);
begin
  if (value <> m_visibleImages) then
  begin
    m_visibleImages := value;
    SetStartEndValues();
  end;
end;


{!!
<FS>TIECircularFlow.EllipseAngle

<FM>Declaration<FC>
property EllipseAngle: double;

<FM>Description<FN>
Specifies the ellipse (the curve where images are placed) angle in degrees. Default is 0 degrees.
!!}
procedure TIECircularFlow.SetEllipseAngle(value: double);
begin
  if (value <> m_ellipseAngle) then
  begin
    m_ellipseAngle := value;
    SetStartEndValues();
  end;
end;




end.
