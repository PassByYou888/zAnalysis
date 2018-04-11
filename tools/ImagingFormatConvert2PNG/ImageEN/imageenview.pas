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
File version 1053
Doc revision 1004
*)

unit imageenview;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


{$R-}
{$Q-}


interface

{$I ie.inc}


{$ifdef IESUPPORTDEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$endif}


uses
  Windows, Messages, Graphics, Controls, Forms, Classes, SysUtils, StdCtrls, ExtCtrls, hyiedefs, hyieutils, imageenio, imageenproc,
  ieview, iexTransitions

{$ifdef FPC}
  ,lmessages
{$endif}

  ;

type

{!!
<FS>TIEGrip

<FM>Declaration<FC>
}
  TIEGrip = (ieNone, ieTopLeft, ieTopRight, ieBottomRight, ieBottomLeft, ieLeftSide, ieRightSide, ieTopSide, ieBottomSide, ieRotationCenter);
{!!}

{!!
<FS>TIEAlignment

<FM>Declaration<FC>
}
  TIEAlignment = (iejLeft, iejRight, iejCenter, iejJustify);
{!!}


{!!
<FS>TViewChangeEvent

<FM>Declaration<FC>
TViewChangeEvent = procedure(Sender: TObject; Change: integer) of object;

<FM>Description<FN>
If Change = 0 the event is <A TImageEnView.ViewX> or <A TImageEnView.ViewY> modification.
If Change = 1 the event is <A TImageEnView.Zoom> modification.

Note: Zoom could also modify ViewX/Y.

!!}
  TViewChangeEvent = procedure(Sender: TObject; Change: integer) of object;



{!!
<FS>TIEMediaFoundationNotifyEvent

<FM>Declaration<FC>
TIEMediaFoundationNotifyEvent = procedure(Sender: TObject; MediaFoundationObject: TObject; NotifyType: <A TIEMediaFountationNotifyType>) of object;

<FM>Description<FN>
<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Sender<FN></C> <C>Sender object. For <A TImageEnView.OnMediaFoundationNotify> this is a <A TImageEnView> object</C> </R>
<R> <C><FC>MediaFoundationObject<FN></C> <C>Media foundation object. This could be a <A TIEMediaFoundationSourceReader> object</C> </R>
<R> <C><FC>NotifyType<FN></C> <C>Notification type</C> </R>
</TABLE>
!!}
  TIEMediaFoundationNotifyEvent = procedure(Sender: TObject; MediaFoundationObject: TObject; NotifyType: TIEMediaFountationNotifyType) of object;


{!!
<FS>TIESmoothTask

<FM>Declaration<FC>
}
  TIESmoothTask = (iestScroll, iestZoom);
{!!}


{!!
<FS>TIEFinishSmoothTaskEvent

<FM>Declaration<FC>
TIEFinishSmoothTaskEvent = procedure(Sender: TObject; Task: <A TIESmoothTask>) of object;

<FM>Description<FN>
Event called when a smooth task has finished (smooth scroll, smooth zoom, etc...).
!!}
  TIEFinishSmoothTaskEvent = procedure(Sender: TObject; Task: TIESmoothTask) of object;


{!!
<FS>TViewChangingEvent

<FM>Declaration<FC>
type TViewChangingEvent = procedure(Sender: TObject; Change: integer; newValue: Double) of object;

<FM>Description<FN>

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Change<FN> = 0<FN></C> <C><A TImageEnView.ViewX> modification.</C> </R>
<R> <C><FC>Change<FN> = 1<FN></C> <C><A TImageEnView.ViewY> modification.</C> </R>
<R> <C><FC>Change<FN> = 2<FN></C> <C><A TImageEnView.Zoom> modification.</C> </R>
</TABLE>

<FC>newValue<FN> contains the new value that will be set.

Note: <FC>Zoom<FN> could also modify <FC>ViewX/Y<FN>.
!!}
  TViewChangingEvent = procedure(Sender: TObject; Change: integer; newValue: double) of object;


{!!
<FS>TIEMouseInResizingGripEvent

<FM>Declaration<FC>
TIEMouseInResizingGripEvent = procedure(Sender: TObject; Grip: <A TIEGrip>) of object;
!!}
  TIEMouseInResizingGripEvent = procedure(Sender: TObject; Grip: TIEGrip) of object;

{!!
<FS>TIEZoomEvent

<FM>Declaration<FC>
}
  TIEZoomEvent = procedure(Sender: TObject; var NewZoom: double) of object;
{!!}

{!!
<FS>TIESpecialKeyEvent

<FM>Declaration<FC>
}
  TIESpecialKeyEvent = procedure(Sender: TObject; CharCode: word; Shift: TShiftState; var Handled: Boolean) of object;
{!!}

  TIEScrollCommand = (iescPosition, iescBottom, iescTop, iescLineDown, iescLineUp, iescPageDown, iescPageUp);


{!!
<FS>TIEOnDrawCanvas

<FM>Declaration<FC>
}
  TIEOnDrawCanvas = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect) of object;
{!!}


{!!
<FS>TIEOnDrawBackground

<FM>Declaration<FC>
}
  TIEOnDrawBackground = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; var Handled: boolean) of object;
{!!}


{!!
<FS>TIEOnDrawPolygon

<FM>Declaration<FC>
}
  TIEOnDrawPolygon = procedure(Sender: TObject; polygon: Integer; point: Integer; Canvas: TCanvas; x, y: Integer) of object;
{!!}


{!!
<FS>TIEWallpaperStyle

<FM>Declaration<FC>
TIEWallpaperStyle = (iewoNormal, iewoStretch, iewoTile);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iewoNormal</C> <C>Draw the wallpaper image at the top left</C> </R>
<R> <C>iewoStretch</C> <C>Stretch the wallpaper to the ImageEnView size</C> </R>
<R> <C>iewoTile</C> <C>Tile the wallpaper over the background</C> </R>
</TABLE>
                                                          
<FM>Example<FN>
// Tile a bitmap over the background
ImageEnView1.WallPaperStyle := iewoTile
ImageEnView1.WallPaper.LoadFromFile('D:\MyWallpaper.bmp');
!!}
  TIEWallPaperStyle = (iewoNormal, iewoStretch, iewoTile);

{!!
<FS>TIEMouseInteractItems

<FM>Declaration<FC>
TIEMouseInteractItems = (miZoom, miScroll, miSelect, miSelectPolygon, miSelectCircle, miSelectZoom, miSelectMagicWand, miSelectLasso, miMoveLayers, miResizeLayers, miRotateLayers, miMovingScroll );

<FM>Description<FN>
What actions does the mouse perform:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>miZoom</C> <C>A left mouse click zooms into the image. A right click zooms out</C> </R>
<R> <C>miScroll</C> <C>The image can be navigated by clicking with the left mouse button and dragging</C> </R>
<R> <C>miSelect</C> <C>A rectangular area can be selected. Click and move the mouse to select the rectangle. miSelect excludes miScroll and miSelectXXX. Simultaneously holding down the Shift key allows the selection of multiple regions. The Alt Key forces the selection to maintain its aspect ratio</C> </R>
<R> <C>miSelectZoom</C> <C>If the user selects an area, it will be automatically zoomed into (and become the new display view). Click and move the mouse to select the zoom rectangle</C> </R>
<R> <C>miSelectPolygon</C> <C>A polygonal area can be selected. Click (and release) the left mouse button at each point of the polygon, or click and hold the left button and move to specify continuous irregular lines. Simultaneously holding down the Shift key allows the selection of multiple regions</C> </R>
<R> <C>miSelectCircle</C> <C>A circular/elliptical area can be selected. Click the left button and move the mouse (holding the Alt key will force a circular selection). Simultaneously holding down the Shift key allows the selection of multiple regions</C> </R>
<R> <C>miSelectMagicWand</C> <C>Selects an irregular region of similar colors. Click the left mouse button upon a pixel and similar colors surrounding it are selected (see <A TImageEnView.MagicWandTolerance>). Simultaneously holding down the Shift key allows the selection of multiple regions</C> </R>
<R> <C>miSelectLasso</C> <C>A polygonal area can be selected by continous movement. Click the left button and move the mouse to select a continuous irregular region</C> </R>
<R> <C>miMoveLayers</C> <C>User can move layers. <A TImageEnView.LayersSync> must be False</C> </R>
<R> <C>miResizeLayers</C> <C>User can resize layers using grips. <A TImageEnView.LayersSync> must be False</C> </R>
<R> <C>miRotateLayers</C> <C>User can rotate layers using grips. <A TImageEnView.LayersSync> must be False</C> </R>
<R> <C>miMovingScroll</C> <C>Moving the mouse over the image (without requiring any clicking) will cause the image to automatically scroll (pan) (see <A TImageEnView.SmoothScrollValue>)</C> </R>
</TABLE>
!!}
  TIEMouseInteractItems = (
    miZoom,               // Click-sx zoom-in / click-dx zoom-out
    miScroll,             // Hand navigation
    miSelect,             // Rectangular selection
    miSelectPolygon,      // Polygonal selection
    miSelectCircle,       // Circular selection
    miSelectZoom,         // Zoom in rectangle
    miSelectMagicWand,    // Magic Wand selection
    miSelectLasso,        // Polygonal selection
    miMoveLayers,         // Move layers
    miResizeLayers,       // Resize layers
    miRotateLayers,       // Rotate layers
    miMovingScroll        // Smooth moving scroll
    );


{!!
<FS>TIEMouseInteract

<FM>Declaration<FC>
type TIEMouseInteract = set of <A TIEMouseInteractItems>;
!!}
  TIEMouseInteract = set of TIEMouseInteractItems;



{!!
<FS>TIEGestureOptions

<FM>Description<FN>
Options for ImageEn gesture support.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Enabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Inertia></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Multiplier></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapValues></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapDelta></C> </R>
</TABLE>
!!}
  TIEGestureOptions = class
    private
      fDisables: array of TIEGestureOptions;
      fEnabled: boolean;
      fInertia: boolean;
      fMultiplier: double;
      fSnapValues: boolean;
      fSnapDelta: double;

      procedure SetEnabled(Value: boolean);
      procedure AddDisables(Value: TIEGestureOptions);

    public

{!!
<FS>TIEGestureOptions.Enabled

<FM>Declaration<FC>
property Enabled: boolean;

<FM>Description<FN>
Enables/disables the gesture.
!!}
      property Enabled: boolean read fEnabled write SetEnabled;

{!!
<FS>TIEGestureOptions.Inertia

<FM>Declaration<FC>
property Inertia: boolean;

<FM>Description<FN>
Whether the gesture supports "intertia". When inertia is enabled and a gesture is performed with sufficient speed then it will continue for a time even after the actual movement ends.
!!}
      property Inertia: boolean read fInertia write fInertia;

{!!
<FS>TIEGestureOptions.Multiplier

<FM>Declaration<FC>
property Multiplier: double;

<FM>Description<FN>
Specifies a multiplier for the controlled viewer property.
!!}
      property Multiplier: double read fMultiplier write fMultiplier;

{!!
<FS>TIEGestureOptions.SnapValues

<FM>Declaration<FC>
property SnapValues: boolean;

<FM>Description<FN>
If enabled, then values will change only in amounts specified by the <A TIEGestureOptions.SnapDelta>. For example, when performing a small rotation of less than a <A TIEGestureOptions.SnapDelta> of 90 degrees, it will "snap" to 90.
!!}
      property SnapValues: boolean read fSnapValues write fSnapValues;

{!!
<FS>TIEGestureOptions.SnapDelta

<FM>Declaration<FC>
property SnapDelta: double;

<FM>Description<FN>
Specifies the amount to snap values by when <A TIEGestureOptions.SnapValues> is enabled.
!!}
      property SnapDelta: double read fSnapDelta write fSnapDelta;

  end;


{!!
<FS>TIEGesturePanOptions

<FM>Description<FN>
Specufy options for the Zoom gesture.

Note: Inherites from the <A TIEGestureOptions> class.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEGesturePanOptions.BoundingBox> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Enabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Inertia></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Multiplier></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapValues></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapDelta></C> </R>
</TABLE>
!!}
  TIEGesturePanOptions = class(TIEGestureOptions)
    private
      fBoundingBox: TRect;

    public

{!!
<FS>TIEGesturePanOptions.BoundingBox

<FM>Declaration<FC>
property BoundingBox: TRect;

<FM>Description<FN>
Contains the minimum/maximum values for the left, top and right and bottom.
!!}
      property BoundingBox: TRect read fBoundingBox write fBoundingBox;

  end;


{!!
<FS>TIEGestureZoomOptions

<FM>Description<FN>
Specifies options for the Zoom gesture.

Note: Inherites from the <A TIEGestureOptions> class.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEGestureZoomOptions.Max> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureZoomOptions.Min> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Enabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Inertia></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Multiplier></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapValues></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapDelta></C> </R>
</TABLE>
!!}
  TIEGestureZoomOptions = class(TIEGestureOptions)
    private
      fMax: double;
      fMin: double;

    public

{!!
<FS>TIEGestureZoomOptions.Max

<FM>Declaration<FC>
property Max: double;

<FM>Description<FN>
Specify the maximum allowed Zoom.
!!}
      property Max: double read fMax write fMax;

{!!
<FS>TIEGestureZoomOptions.Min

<FM>Declaration<FC>
property Min: double;

<FM>Description<FN>
Specify the minimum allowed Zoom.
!!}
      property Min: double read fMin write fMin;
  end;


{!!
<FS>TIEGestureLayerMoveOptions

<FM>Description<FN>
Specifies options for the layer movement gesture.

Note: Inherites from the <A TIEGestureOptions> class.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEGestureLayerMoveOptions.BoundingBox> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Enabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Inertia></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Multiplier></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapValues></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapDelta></C> </R>
</TABLE>
!!}
  TIEGestureLayerMoveOptions = class(TIEGestureOptions)
    private
      fBoundingBox: TRect;

    public

{!!
<FS>TIEGestureLayerMoveOptions.BoundingBox

<FM>Declaration<FC>
property BoundingBox: TRect;

<FM>Description<FN>
Contains the minimum/maximum values for the left, top and right and bottom.
!!}
      property BoundingBox: TRect read fBoundingBox write fBoundingBox;

  end;


{!!
<FS>TIEGestureLayerRotateOptions

<FM>Description<FN>
Specifies options for the layer rotation gesture.

Note: Inherites from the <A TIEGestureOptions> class.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEGestureLayerRotateOptions.Min> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureLayerRotateOptions.Max> <IMG help_images\Star.gif></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Enabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Inertia></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.Multiplier></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapValues></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGestureOptions.SnapDelta></C> </R>
</TABLE>
!!}
  TIEGestureLayerRotateOptions = class(TIEGestureOptions)
    private
      fMax: double;
      fMin: double;

    public

{!!
<FS>TIEGestureLayerRotateOptions.Max

<FM>Declaration<FC>
property Max: double;

<FM>Description<FN>
Specify the maximum allowed Zoom.
!!}
      property Max: double read fMax write fMax;

{!!
<FS>TIEGestureLayerRotateOptions.Min

<FM>Declaration<FC>
property Min: double;

<FM>Description<FN>
Specify the minimum allowed Zoom.
!!}
      property Min: double read fMin write fMin;
  end;


{!!
<FS>TIEViewerGestures

<FM>Description<FN>
TImageEnView supports native Windows 7 and Windows 8 gestures, allowing pan, zoom, layer rotation and movement.

This class contains the properties to configure the gestures.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEViewerGestures.Enabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEViewerGestures.LayerMove></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEViewerGestures.LayerRotate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEViewerGestures.Pan></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEViewerGestures.Zoom></C> </R>
</TABLE>
!!}
  TIEViewerGestures = class
    private
      fPan: TIEGesturePanOptions;
      fZoom: TIEGestureZoomOptions;
      fLayerRotate: TIEGestureLayerRotateOptions;
      fLayerMove: TIEGestureLayerMoveOptions;

      function GetEnabled(): boolean;

    public
      constructor Create();
      destructor Destroy(); override;

{!!
<FS>TIEViewerGestures.Enabled

<FM>Declaration<FC>
property Enabled: boolean; (Read-only)

<FM>Description<FN>
Returns true whenever at least one gesture is enabled.

!!}
      property Enabled: boolean read GetEnabled;

{!!
<FS>TIEViewerGestures.Pan

<FM>Declaration<FC>
property Pan: <A TIEGesturePanOptions>;

<FM>Description<FN>
Allows enabling and configuration of the Pan (image scrolling) gesture. The Windows gesture to "Pan with intertia" is performed on the action of "dragging 1 or 2 fingers".

Note: The <L TIEViewerGestures.Pan>Pan</L> and <L TIEViewerGestures.LayerMove>layer movement</L> gestures are mutually exclusive (i.e. you cannot use both).
!!}
      property Pan: TIEGesturePanOptions read fPan;

{!!
<FS>TIEViewerGestures.Zoom

<FM>Declaration<FC>
property Zoom: <A TIEGestureZoomOptions>;

<FM>Description<FN>
Allows enabling and configuration of the Zoom gesture. The Windows gesture of "Zoom" is performed on the action of "moving two fingers apart/towards each other".
!!}
      property Zoom: TIEGestureZoomOptions read fZoom;

{!!
<FS>TIEViewerGestures.LayerRotate

<FM>Declaration<FC>
property LayerRotate: <A TIEGestureLayerRotateOptions>;

<FM>Description<FN>
Allows enabling and configuration of the Layer Rotation gesture. The Windows gesture of "Rotate" is performed on the action of "moving two fingers in opposing directions or using one finger to pivot around another".
!!}
      property LayerRotate: TIEGestureLayerRotateOptions read fLayerRotate;

{!!
<FS>TIEViewerGestures.LayerMove

<FM>Declaration<FC>
property LayerMove: <A TIEGestureLayerMoveOptions>;

<FM>Description<FN>
Allows enabling and configuration of the Layer Movement gesture. The Windows gesture of "Pan with intertia" is performed on the action of "dragging 1 or 2 fingers".

Note: The <L TIEViewerGestures.Pan>Pan</L> and <L TIEViewerGestures.LayerMove>layer movement</L> gestures are mutually exclusive (i.e. you cannot use both).
!!}
      property LayerMove: TIEGestureLayerMoveOptions read fLayerMove;
  end;



{!!
<FS>TIENavigatorOptions

<FM>Declaration<FC>
TIENavigatorOptions = set of (ienoMouseWheelZoom, ienoMarkOuter);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>ienoMouseWheelZoom<FN></C> <C>Zoom is possible with mouse-wheel</C> </R>
<R> <C><FC>ienoMarkOuter<FN></C> <C>Marks as grayed non visible area</C> </R>
</TABLE>
!!}
TIENavigatorOptions = set of (ienoMouseWheelZoom, ienoMarkOuter);


{!!
<FS>TIESelectionBase

<FM>Declaration<FC>
}
  TIESelectionBase = (
    iesbClientArea, // Select coordinates over client area (as they appear in the current view)
    iesbBitmap      // Select coordinates over bitmap (as they relate to the bitmap regardless of view or zoom)
    );
{!!}


{!!
<FS>TIESelectionOptions

<FM>Declaration<FC>
TIESelectionOptions = set of (
      iesoAnimated,
      iesoSizeable,
      iesoMoveable,
      iesoFilled,
      iesoCutBorders,
      iesoMarkOuter,
      iesoCanScroll,
      iesoSelectTranspLayers,
      iesoRightButtonSelectLayers,
      iesoRightButtonTerminatePolySelect,
      iesoDisableOneClickDeselect,
      iesoDisableNewSelection);


<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iesoAnimated</C> <C>Enable animation of the selection</C> </R>
<R> <C>iesoSizeable</C> <C>Allow the user to resize the selection (with grips)</C> </R>
<R> <C>iesoMoveable</C> <C>Allow the user to move the selection (with grips)</C> </R>
<R> <C>iesoFilled</C> <C>The image within the selection is displayed with inverted colors</C> </R>
<R> <C>iesoCutBorders</C> <C>The user is allowed to drag the selection outside the borders (the selection borders will be cut)</C> </R>
<R> <C>iesoMarkOuter</C> <C>Areas of the image outside the selection are shown as grayed or with alpha blending (see <A TImageEnView.SetSelectionMarkOuterStyle>)</C> </R>
<R> <C>iesoCanScroll</C> <C>Automatically scroll the image when selecting outside the visible area</C> </R>
<R> <C>iesoSelectTranspLayers</C> <C>Allow transparent areas of a layer to be selectable</C> </R>
<R> <C>iesoRightButtonSelectLayers</C> <C>Allow selection of layers with the right button (other than left button)</C> </R>
<R> <C>iesoRightButtonTerminatePolySelect</C> <C>Allow polygon selection to be cancelled with the right mouse button</C> </R>
<R> <C>iesoDisableOneClickDeselect</C> <C>Prevent the selection from being cleared when clicking outside the selection</C> </R>
<R> <C>iesoDisableNewSelection</C> <C>Once a selection has been made (programatically using <A TImageEnView.Select> or by the user using <A TImageEnView.MouseInteract>) then the user cannot make a new selection (though they can still move or resize the selection if iesoSizeable or iesoMoveable is specified)</C> </R>
</TABLE>
!!}
  TIESelectionOptions = set of (
        iesoAnimated,
        iesoSizeable,
        iesoMoveable,
        iesoFilled,
        iesoCutBorders,
        iesoMarkOuter,
        iesoCanScroll,
        iesoSelectTranspLayers,
        iesoRightButtonSelectLayers,
        iesoRightButtonTerminatePolySelect,
        iesoDisableOneClickDeselect,
        iesoDisableNewSelection
        );

{!!
<FS>TIESelOp

<FM>Declaration<FC>
TIESelOp = (iespReplace, iespAdd);

<FM>Description<FN>
iespReplace: replaces all previous selections.
iespAdd: adds a new selection.
!!}
  TIESelOp = (iespReplace, iespAdd);

{!!
<FS>TIEMagicWandMode

<FM>Declaration<FC>
TIEMagicWandMode=(iewInclusive, iewExclusive, iewGlobal);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iewInclusive</C> <C>the selection is a closed polygon</C> </R>
<R> <C>iewExclusive</C> <C>the selection includes only points that match the initial pixel, similar to a flood fill</C> </R>
<R> <C>iewGlobal</C> <C>the selection includes all points that match the initial pixel, examining all pixels of the image</C> </R>
</TABLE>
!!}
  TIEMagicWandMode = (iewInclusive, iewExclusive, iewGlobal);

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TIEVSoftShadow

{!!
<FS>TIEVSoftShadow

<FM>Description<FN>
This class specifies how a shadow will be rendered.

<FM>Fields<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEVSoftShadow.Enabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEVSoftShadow.Intensity></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEVSoftShadow.OffsetX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEVSoftShadow.OffsetY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEVSoftShadow.Radius></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEVSoftShadow.ShadowColor></C> </R>
</TABLE>
!!}
  TIEVSoftShadow = class

{!!
<FS>TIEVSoftShadow.Enabled

<FM>Declaration<FC>
Enabled: Boolean

<FM>Description<FN>
Enable/disable the shadow.
!!}
    Enabled: boolean;

{!!
<FS>TIEVSoftShadow.Radius

<FM>Declaration<FC>
Radius: Double

<FM>Description<FN>
The radius of the shadow (which controls its blur)
!!}
    Radius: double;

{!!
<FS>TIEVSoftShadow.OffsetX

<FM>Declaration<FC>
OffsetX: Integer

<FM>Description<FN>
The horizontal relative position of the shadow.
!!}
    OffsetX: integer;

{!!
<FS>TIEVSoftShadow.OffsetY

<FM>Declaration<FC>
OffsetY: Integer

<FM>Description<FN>
The vertical relative position of the shadow.
!!}
    OffsetY: integer;

{!!
<FS>TIEVSoftShadow.Intensity

<FM>Declaration<FC>
Intensity: Integer

<FM>Description<FN>
This is the shadow intensity, with values in the range 0 and 100 (maximum). Default is 100.
!!}
    Intensity: integer;

{!!
<FS>TIEVSoftShadow.ShadowColor

<FM>Declaration<FC>
ShadowColor: <A TRGB>;

<FM>Description<FN>
The shadow color. The default is 0,0,0 (black).
!!}
    ShadowColor: TRGB;

  end;

// TIEVSoftShadow
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////




{!!
<FS>TIEDrawingInfo

<FM>Declaration<FC>
}
  TIEDrawingInfo = record
    XDst: integer;       // Destination x  (output canvas)
    YDst: integer;       // Destination y  (output canvas)
    WidthDst: integer;   // Destination width (output canvas)
    HeightDst: integer;  // Destination height (output canvas)
    XSrc: integer;       // Source x (input bitmap)
    YSrc: integer;       // Source y (input bitmap)
    WidthSrc: integer;   // Source width (input bitmap)
    HeightSrc: integer;  // Source height (input bitmap)
    RotatedDest: array [0..3] of TPoint; // destination rotated points (output canvas)
  end;
{!!}


{!!
<FS>TIEDrawLayerEvent

<FM>Declaration<FC>
}
  TIEDrawLayerEvent = procedure(Sender: TObject; dest: TIEBitmap; layerIndex: Integer) of object;
{!!}


{!!
<FS>TIEDrawLayerBoxEvent

<FM>Declaration<FC>
}
  TIEDrawLayerBoxEvent = procedure (Sender: TObject; ABitmap: TBitmap; layer: integer) of object;
{!!}

{!!
<FS>TIEDrawLayerGrip

<FM>Declaration<FC>
}
  TIEDrawLayerGrip = procedure (Sender: TObject; ABitmap: TBitmap; layer: integer; grip: Integer; rect: TRect) of object;
{!!}

{!!
<FS>TIELayer

<FM>Description<FN>
TImageEnView supports multiple layers, allowing the creation of a single image from multiple source images (which can be resized, rotated, moved, etc).

This class contains a single layer of <A TImageEnView> object. A layer has a TIEBitmap, position, size, transparency and other properties.
You should create new layers using <A TImageEnView.LayersAdd>.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIELayer.Assign></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Bitmap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.ClientAreaBox></C> </R>
<R> <C_IMG_METHOD> <C><A TIELayer.ConvXBmp2Scr></C> </R>
<R> <C_IMG_METHOD> <C><A TIELayer.ConvXScr2Bmp></C> </R>
<R> <C_IMG_METHOD> <C><A TIELayer.ConvYBmp2Scr></C> </R>
<R> <C_IMG_METHOD> <C><A TIELayer.ConvYScr2Bmp></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Cropped></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.DrawingInfo></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.DrawOuter></C> </R>
<R> <C_IMG_METHOD> <C><A TIELayer.GetIndex></C> </R>
<R> <C_IMG_METHOD> <C><A TIELayer.GetLayerMask></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Height></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.IsMask></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Locked></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Magnify></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Name></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Opacity></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Operation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.PosX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.PosY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.ResampleFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Rotate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.RotateCenterX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.RotateCenterY></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Selectable></C> </R>
<R> <C_IMG_METHOD> <C><A TIELayer.Swap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Transparency></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.UserData></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.UserDataLen></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.UseResampleFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Visible></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.VisibleBox></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIELayer.Width></C> </R>
</TABLE>
!!}
  TIELayer = class
  private
    ResizedWidth, ResizedHeight: integer; // 0=original sizes
    fRotate: Double;
    fCachedLayerMask: TIEBitmap;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetRotate(v: Double);
  public
    fOwner: TObject; // must be TImageEnView

{!!
<FS>TIELayer.Bitmap

<FM>Declaration<FC>
property Bitmap: <A TIEBitmap>;

<FM>Description<FN>
Contains the image and alpha channel of the layer.
!!}
    Bitmap: TIEBitmap;

{!!
<FS>TIELayer.Visible

<FM>Declaration<FC>
property Visible: Boolean;

<FM>Description<FN>
Set to True to make the layer visible, or False to hide it.
!!}
    Visible: boolean; // if True the layer is visible

{!!
<FS>TIELayer.VisibleBox

<FM>Declaration<FC>
property VisibleBox: boolean;

<FM>Description<FN>
Display a box around the layer.
!!}
    VisibleBox: boolean;

{!!
<FS>TIELayer.Transparency

<FM>Declaration<FC>
property Transparency: integer;

<FM>Description<FN>
Set the overall transparency of the layer in the range from 0 (fully transparent) to 255 (totally opaque).
Use Opacity instead of Transparency when the image has an alpha channel.

See also: <A TIELayer.Opacity>.
!!}
    Transparency: integer; // 255=opaque, 0=transparent

{!!
<FS>TIELayer.Opacity

<FM>Declaration<FC>
property Opacity: double;

<FM>Description<FN>
Set the overall opacity of the layer in the range from 0 (fully transparent) to 1.0 (totally opaque).
Use Opacity instead of Transparency when the image has an alpha channel.

See also: <A TIELayer.Transparency>.
!!}
    Opacity: double;

{!!
<FS>TIELayer.PosX

<FM>Declaration<FC>
property PosX: Integer;

<FM>Description<FN>
Specifies the horizontal position of the layer relative to the background image (layer 0). 
Note: Valid only when <A TImageEnView.LayersSync>=False.
!!}
    PosX: integer;

{!!
<FS>TIELayer.PosY

<FM>Declaration<FC>
property PosY: Integer;

<FM>Description<FN>
Specifies the vertical position of the layer relative to the background image (layer 0).
Note: Valid only when <A TImageEnView.LayersSync>=False.
!!}
    PosY: integer;

{!!
<FS>TIELayer.ClientAreaBox

<FM>Declaration<FC>
property ClientAreaBox: TRect; (Read-Only)

<FM>Description<FN>
Returns the drawing area of the layer. The coordinates are relative to the component client area.
Note: Valid only if <A TImageEnView.LayersSync>=False.
!!}
    ClientAreaBox: TRect;

{!!
<FS>TIELayer.Cropped

<FM>Declaration<FC>
property Cropped: Boolean;

<FM>Description<FN>
When true any part of the layer that is outside the background image (layer 0) area will be cut off.
Note: Valid only if <A TImageenView.LayersSync>=False.
!!}
    Cropped: boolean;

{!!
<FS>TIELayer.Locked

<FM>Declaration<FC>
property Locked: Boolean;

<FM>Description<FN>
When False the user can move or resize the layer. Also enables display of the layer border and resizing grips.
Note: Valid only if <A TImageEnView.LayersSync>=False.
!!}
    Locked: boolean;

{!!
<FS>TIELayer.Selectable

<FM>Declaration<FC>
property Selectable: boolean;

<FM>Description<FN>
If true (default) the layer is selectable by the user.
!!}
    Selectable: boolean;

{!!
<FS>TIELayer.Magnify

<FM>Declaration<FC>
property Magnify: <A TIELayerMagnification>

<FM>Description<FN>
Set properties if the layer is a magnification layer.
!!}
    Magnify: TIELayerMagnification;

{!!
<FS>TIELayer.Operation

<FM>Declaration<FC>
property Operation: <A TIERenderOperation>;

<FM>Description<FN>
Specifies the color operation to execute between this layer and the background layer (default: ielNormal).
!!}
    Operation: TIERenderOperation;

{!!
<FS>TIELayer.UserData

<FM>Declaration<FC>
property UserData: pointer;

<FM>Description<FN>
Contains a pointer to user data buffer. This can be used for your own purposes to store information about the layer.

See also: <A TIELayer.UserDataLen>.
!!}
    UserData: pointer;

{!!
<FS>TIELayer.UserDataLen

<FM>Declaration<FC>
property UserDataLen: Integer;

<FM>Description<FN>
Specifies the length of UserData buffer.
If this value is greater than zero then when you save the layer (LayersSaveTo..) the content of <A TIELayer.UserData> buffer is saved (and restored with LayersLoadFrom..).
!!}
    UserDataLen: Integer; // used when save. If >0 then saves UserData content. Also UserData will be freed on destroy

    FreeBitmapOnDestroy: boolean;

{!!
<FS>TIELayer.ResampleFilter

<FM>Declaration<FC>
property ResampleFilter: <A TResampleFilter>;

<FM>Description<FN>
Actived only when <A TIELayer.UseResampleFilter> is true this property specifies a resample filter to use instead of default one (<A TImageEnView.ZoomFilter>) to improve the quality of images.
!!}
    ResampleFilter: TResampleFilter;

{!!
<FS>TIELayer.UseResampleFilter

<FM>Declaration<FC>
property UseResampleFilter: boolean;

<FM>Description<FN>
This property active the value specified in <A TIELayer.ResampleFilter>.
!!}
    UseResampleFilter: Boolean;  // if true uses this ResampleFilter instead of TImageEnView.ZoomFilter

{!!
<FS>TIELayer.Name

<FM>Declaration<FC>
property Name: AnsiString;

<FM>Description<FN>
Contains the layer name. Only applications use this field.
!!}
    Name: AnsiString;

{!!
<FS>TIELayer.IsMask

<FM>Declaration<FC>
property IsMask: Boolean;

<FM>Description<FN>
If True this is a layer mask. A layer mask contains a gray scale image (ie8g) which is connected to the upper layer.
The layer mask specifies where the upper layer is visible (255=fully visible).
A layer mask should be invisible (<A TIELayer.Visible>=False).
!!}
    IsMask: Boolean; // This is the "layer mask" of previous layer

{!!
<FS>TIELayer.DrawingInfo

<FM>Declaration<FC>
property DrawingInfo: <A TIEDrawingInfo>; (read-only)

<FM>Description<FN>
Specifies the source and destination rectangle recorded on last drawing.
!!}
    DrawingInfo: TIEDrawingInfo; // filled by PaintToEx

{!!
<FS>TIELayer.DrawOuter

<FM>Declaration<FC>
property DrawOuter: Boolean;

<FM>Description<FN>
If True unselected layers are drawn as "grayed". The selected layer is drawn normally.
!!}
    DrawOuter: Boolean;

    GUID: TGuid; // Unique identifier for this layer

{!!
<FS>TIELayer.RotateCenterX

<FM>Declaration<FC>
property RotateCenterX: Double;

<FM>Description<FN>
Specifies the horizontal rotation center, in percentage of layer size.
0.5 means center (default), while 0 is left side and 1 is right side.
Also allowed are values less than 0 and greater than 1 (e.g. 2 would rotate at one width to the right of the image).
This value (and <A TIELayer.RotateCenterY>) is modified when user moves the central grip, when layers rotation is active.

See also: <A TIELayer.Rotate>.
!!}
    RotateCenterX: Double; // in percentage. 0.5=center

{!!
<FS>TIELayer.RotateCenterY

<FM>Declaration<FC>
property RotateCenterY: Double;

<FM>Description<FN>
Specifies the vertical rotation center, in percentage of layer size.
0.5 means center (default), while 0 is top side and 1 is bottom side.
Also allowed are values less than 0 and greater than 1 (e.g. 2 would rotate at one height below the image).
This value (and <A TIELayer.RotateCenterX>) is modified when user moves the central grip, when layers rotation is active.

See also: <A TIELayer.Rotate>.
!!}
    RotateCenterY: Double; // in percentage. 0.5=center

{!!
<FS>TIELayer.Rotate

<FM>Declaration<FC>
property Rotate: Double;

<FM>Description<FN>
Specifies the layer rotation angle in degrees.
It is not allowed Rotation and Resize in the same time.

To allow resize after a layer rotation, it must be effected, i.e. actually rotate the bitmap.
Conversely to allow rotation after a resize, the resizing must be effected, actually resizing the bitmap.
To effect layer rotation call <A TImageEnView.LayersFixRotations>.
To effect layer resizing call <A TImageEnView.LayersFixSizes>.
Finally, multiple rotations can cause the bitmap to add an external, transparent border. To remove this border call <A TImageEnView.LayersFixBorders>.

Note: When rotation is handled by <A TImageEnView.MouseInteract> (setting miRotateLayers), you don't need to call LayersFixRotations, LayersFixSizes and LayersFixBorders.

<FM>Demo<FN>
ImageEditing\Layers
ImageEditing\RotateLayers
!!}
    property Rotate: Double read fRotate write SetRotate;

{!!
<FS>TIELayer.Width

<FM>Declaration<FC>
property Width: Integer;

<FM>Description<FN>
Specifies the layer size (bitmap size).
Note: Valid only if <A TImageEnView.LayersSync>=False.
!!}
    property Width: integer read GetWidth write ResizedWidth; // displayed width of the layer

{!!
<FS>TIELayer.Height

<FM>Declaration<FC>
property Height: Integer;

<FM>Description<FN>
Specifies the layer size (bitmap size).
Note: Valid only if <A TImageEnView.LayersSync>=False.
!!}
    property Height: integer read GetHeight write ResizedHeight; // displayed height of the layer

    constructor Create(Owner: TObject; TemplateBitmap: TIEBitmap; JustAssign: boolean);
    destructor Destroy; override;
    function ConvXScr2Bmp(x: Integer): Integer;
    function ConvYScr2Bmp(y: Integer): Integer;
    function ConvXBmp2Scr(x: Integer): Integer;
    function ConvYBmp2Scr(y: Integer): Integer;
    procedure Assign(Source: TIELayer);
    procedure Swap(OtherLayer: TIELayer);
    procedure SetDefaults;
    procedure SaveInfo(Stream: TStream; saveBitmap: Boolean);
    procedure LoadInfo(Stream: TStream);
    function GetLayerMask(): TIELayer;
    function GetIndex(): integer;
  end;

{!!
<FS>TIELayerEvent

<FM>Declaration<FC>
TIELayerEvent = (ielSelected, ielMoved, ielResized, ielMoving, ielResizing, ielRotating, ielRotated, ielBeginResizing, ielBeginMoving);

<FM>Description<FN>

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>ielSelected</C> <C>user selected the layer</C> </R>
<R> <C>ielMoved</C> <C>user moved the layer</C> </R>
<R> <C>ielResized</C> <C>user resized the layer</C> </R>
<R> <C>ielMoving</C> <C>user is moving the layer</C> </R>
<R> <C>ielResizing</C> <C>user is resizing the layer</C> </R>
<R> <C>ielRotating</C> <C>user is rotating the layer</C> </R>
<R> <C>ielRotated</C> <C>user rotated the layer</C> </R>
<R> <C>ielBeginResizing</C> <C>user began to resize the layer (click down)</C> </R>
<R> <C>ielBeginMoving</C> <C>user began to move the layer (click down)</C> </R>
</TABLE>
!!}
  TIELayerEvent = (ielSelected, ielMoved, ielResized, ielMoving, ielResizing, ielRotating, ielRotated, ielBeginResizing, ielBeginMoving);

{!!
<FS>TIELayerNotify

<FM>Declaration<FC>
TIELayerNotify = procedure(Sender: TObject; layer: integer; event: <A TIELayerEvent>) of object;

<FM>Description<FN>
<FC>layer<FN> is the layer index that is selected, moved or resized.
<FC>event<FN> specifies the event type.
!!}
  TIELayerNotify = procedure(Sender: TObject; layer: integer; event: TIELayerEvent) of object;


{!!
<FS>TIEVirtualKeyEvent

<FM>Declaration<FC>
TIEVirtualKeyEvent = procedure(Sender: TObject; VirtualKey: Dword; KeyData: Dword) of object;

<FM>Description<FN>
Event type for <A TImageEnView.OnVirtualKey> event.

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>VirtualKey</C> <C>Specifies the virtual key code (the same as wParam of WM_KEYWODN, WM_KEYUP, WM_SYSKEYDOWN and WM_SYSKEYUP)</C> </R>
<R> <C>KeyData</C> <C>Specifies additional info about the key (the same as lParam of WM_KEYWODN, WM_KEYUP, WM_SYSKEYDOWN and WM_SYSKEYUP)</C> </R>
<R> <C>KeyDown</C> <C>This is true on keydown, false on keyup</C> </R>
</TABLE>
!!}
  TIEVirtualKeyEvent = procedure(Sender: TObject; VirtualKey: Dword; KeyData: Dword; KeyDown: Boolean) of object;

{!!
<FS>TIESetCursorEvent

<FM>Declaration<FC>
TIESetCursorEvent = procedure(Sender: TObject; var Cursor: TCursor) of object;

<FM>Description<FN>
Used by the <A TImageEnView.OnSetCursor> event.
!!}
  TIESetCursorEvent = procedure(Sender: TObject; var Cursor: TCursor) of object;


{!!
<FS>TIEGestureFlags

<FM>Declaration<FC>
}
  TIEGestureFlags = set of (iegfBegin, iegfInertia, iegfEnd);
{!!}


{!!
<FS>TIEGestureID

<FM>Declaration<FC>
}
  TIEGestureID = (iegiBegin, iegiEnd, iegiZoom, iegiPan, iegiRotate, iegiTwoFingerTap, iegiPressAndTap);
{!!}


{!!
<FS>TIEImageEnGestureEvent

<FM>Declaration<FC>
TIEImageEnGestureEvent = procedure(Sender: TObject; Flags: <A TIEGestureFlags>; ID: <A TIEGestureID>; Location: TSmallPoint; Value: integer; InertiaX: integer; InertiaY: integer; PressAndTapDistance: integer; var Handled: boolean) of object;

<FM>Description<FN>
Used by the <A TImageEnView.OnImageEnGesture> event.
!!}
  TIEImageEnGestureEvent = procedure(Sender: TObject; Flags: TIEGestureFlags; ID: TIEGestureID; Location: TSmallPoint; Value: integer; InertiaX: integer; InertiaY: integer; PressAndTapDistance: integer; var Handled: boolean) of object;


{!!
<FS>TIERSOptions

<FM>Declaration<FC>
TIERSOptions = (iersNone, iersMoveToAdapt, iersSyncLayers);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iersNone</C> <C>no adaption</C> </R>
<R> <C>iersMoveToAdapt</C> <C>move the selection to fit inside the new size</C> </R>
<R> <C>iersSyncLayers</C> <C>used to maintain the selection in the same position among layers</C> </R>
</TABLE>
!!}
  TIERSOptions = (iersNone, iersMoveToAdapt, iersSyncLayers);

{!!
<FS>TIESoftCropMode

<FM>Declaration<FC>
  TIESoftCropMode = (iesfNone, iesfAlphaBlend, iesfGrid, iesfAdd);

<FM>Description<FN>
Portion of layers that are outside the background layer are shown as:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iesfNone</C> <C>Normal</C> </R>
<R> <C>iesfAlphaBlend</C> <C>Partially transparent (to the level specified by <A TImageEnView.SoftCropValue>)</C> </R>
<R> <C>iesfGrid</C> <C>Grayed (Grid matrix pattern)</C> </R>
<R> <C>iesfAdd</C> <C>Color shifted/washed out (to the level specified by <A TImageEnView.SoftCropValue>)</C> </R>
</TABLE>
!!}
  TIESoftCropMode = (iesfNone, iesfAlphaBlend, iesfGrid, iesfAdd);


{!!
<FS>TIEGripShape

<FM>Declaration<FC>
}
TIEGripShape = (iegsBox, iegsCircle);
{!!}


{!!
<FS>TIEUpdateReason

<FM>Declaration<FC>
}
TIEUpdateReason = (ieurDefault, ieurScrolled, ieurZoomed, ieurSelectionChanged, ieurComponentStuffChanged);
{!!}



{!!
<FS>TIELayersResizeAspectRatio

<FM>Declaration<FC>
TIELayersResizeAspectRatio = (iearDisabled, iearALTKey, iearAlways, iearAlwaysOnCornerGrip);

<FM>Description<FN>
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iearDisabled</C> <C>Doesn't maintain the aspect ratio even when pressing ALT or setting <A TImageEnView.ForceALTKey>)</C> </R>
<R> <C>iearALTKey</C> <C>Maintains the aspect ratio only when pressing ALT or setting <A TImageEnView.ForceALTKey></C> </R>
<R> <C>iearAlways</C> <C>The aspect ratio is always maintained (using any grips)</C> </R>
<R> <C>iearAlwaysOnCornerGrip</C> <C>The aspect ratio is always maintained if the corner grips are used</C> </R>
</TABLE>
!!}
TIELayersResizeAspectRatio = (iearDisabled, iearALTKey, iearAlways, iearAlwaysOnCornerGrip);




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TImageEnView

{!!
<FS>TImageEnView

<FM>Declaration<FC>
TImageEnView = class(<A TIEView>);

<FM>Description<FN>
TImageEnView is an image container, viewer and editor. It supports zooming, scrolling, selections and layers.

TImageEnView encapsulates a <A TImageEnIO> (<A TImageEnView.IO> property) for loading/saving/acquisition and a <A TImageEnProc> (<A TImageEnView.Proc> property) component for image editing/processing (you do not need to add extra TImageEnIO and TImageEnProc components to your form).

For rapid UI development a full set of <L TImageEnView Actions>actions</L> is also available.


<IMG help_images\IEView_Component.gif>

<FM>Example<FC>
// Setup
with ImageEnView1 do
begin                                                                        
  LegacyBitmap  := False;               // Support bigger files and multithreading
  AutoShrink    := True;                // Make the image fit within the window
  BorderStyle   := bsNone;              // Normally don't require a 3D border
  ZoomFilter    := rfFastLinear;        // Use better quality drawing when the image is not 100%, Use rfFastLinear for best speed or rfLanczos3 for best quality
  MouseInteract := [miSelect];          // What action should the mouse do?
end;

// Display an image
ImageEnView1.IO.LoadFromFile('C:\MyImage.jpg');

// Rotate the image            
ImageEnView1.Proc.Rotate(270, True);

<FM>Methods and Properties<FN>
<FI>General<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.AutoShrink></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.AutoStretch></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.Center></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.CenterImage></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.DisplayGrid></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.DisplayGridLyr></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.DelayTimer></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.DelayZoomFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.DelayZoomTime></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ExtentX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ExtentY></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.Fit></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.FitToHeight></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.FitToWidth></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.GetIdealZoom></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.GetMaxViewXY></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.GetRenderRectangles></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.IdealComponentHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.IdealComponentWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ImageHorizAlignment></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ImageVertAlignment></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LoadState></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LockPaint></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LockPaintCount></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LockUpdate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.OffScreenPaint></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.OffsetX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.OffsetY></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.PaintRect></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.ResetState></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SaveState></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetViewXY></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetViewXYSmooth></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetZoomSmooth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SmoothScrollValue></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SmoothZoomValue></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.Stretch></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.UnLockPaint></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.UnLockUpdate></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.UnLockUpdateEx></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.Update></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.UpdateNoPaint></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.UpdateReason></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.UpdateRect></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ViewX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ViewY></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.VisibleBitmapRect></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.XBmp2Scr></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.XScr2Bmp></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.YBmp2Scr></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.YScr2Bmp></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.Zoom></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.ZoomAt></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.ZoomIn></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.ZoomFilter></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.ZoomOut></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.ZoomSelection></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ZoomX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ZoomY></C> </R>
</TABLE>

<FI>Background<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.BackgroundStyle></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.Background></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.GradientEndColor></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetChessboardStyle></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.WallpaperStyle></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.Wallpaper></C> </R>
</TABLE>

<FI>Scrollbars<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.FlatScrollBars></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.HScrollBarParams></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ScrollBarsAlwaysVisible></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.ScrollBars></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.VScrollBarParams></C> </R>
</TABLE>

<FI>Image Processing<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.Proc></C> </R>
</TABLE>

<FI>Input/Output<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.IO></C> </R>
</TABLE>

<FI>Bitmap<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIEView.DpiX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEView.DpiY></C> </R>
<R> <C_IMG_METHOD> <C><A TIEView.SetDPI></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.Bitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.Blank></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.ChangeResolution></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.Clear></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.CopyFromPolygon></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.CopyToPolygon></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.DrawTo></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.IEBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.IsEmpty></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.IsEmpty2></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.LegacyBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetExternalBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetSelectedPixelsColor></C> </R>
</TABLE>

<FI>User Actions<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.EnableShiftKey></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.EnableInteractionHints></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ForceALTkey></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.Gestures></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.AutoFixRotationBorders></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.MagicWandMaxFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.MagicWandMode></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.MagicWandTolerance></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.MouseInteract></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.MouseScrollRate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.MouseWheelParams></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetInteractionHint></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.ZoomSelectionAspectRatio></C> </R>
</TABLE>

<FI>Selections<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnView.AddSelBreak></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.AddSelPoint></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.AssignSelTo></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.CopySelectionToBitmap></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.CopySelectionToIEBitmap></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.DelayDisplaySelection></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.DelLastSelPoint></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.DeSelect></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.DiscardSavedSelection></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.EndSelect></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.GetSelectionGripStyle></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.InvertSelection></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.IsPointInsideSelection></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LoadSelectionFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LoadSelectionFromStream></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.MakeSelectionFeather></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.MergeSelectionFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.MergeSelectionFromStream></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.MoveSelection></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.PolySel></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.PolySelCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.PolySelPoints></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.RestoreSelection></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SavedSelectionsCount></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SaveSelection></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SaveSelectionToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SaveSelectionToStream></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelColor1></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelColor2></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.Select></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SelectColors></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SelectCustom></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.Selected></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectedRect></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SelectEllipse></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionAbsHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionAbsWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionAspectRatio></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.SelectionBase></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionGridHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionGridSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionGridWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionIntensity></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionMask></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelectionMaskDepth></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.SelectionOptions></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SelectMagicWand></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SelectRoundRect></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelX1></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelX2></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelY1></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SelY2></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetSelectionGripStyle></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetSelectionMarkOuterStyle></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.VisibleSelection></C> </R>
</TABLE>

<FI>Alpha Channel (Transparency)<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.AlphaChannel></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.CopyToBitmapWithAlpha></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.EnableAlphaChannel></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.HasAlphaChannel></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.RemoveAlphaChannel></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetAlphaRangePixelsColor></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetSelectedAreaAlpha></C> </R>
</TABLE>

<FI>Layers<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.CurrentLayer></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.FindLayerAt></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.Layers></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersAdd></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersAutoUndo></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersClear></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersCopyToAlpha></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersCount></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersCreateFromAlpha></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersCreateFromClipboard></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersCreateFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersCreateFromSelection></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersCreateFromText></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersCurrent></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersDrawBox></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersDrawTo></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersFixBorders></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersFixRotations></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersFixSizes></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersInsert></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersLoadFromFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersLoadFromStream></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersMerge></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersMergeAll></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersMergeTo></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersMove></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersRemove></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersResizeAspectRatio></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersRotateStep></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersRotationAntialias></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersRotationFilter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersRotationUseFilterOnPreview></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersRotationDelayFilterOnPreview></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersSaveToFile></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.LayersSaveToStream></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersSelectConstrains></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.LayersSync></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.MaxLayerHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.MaxLayerWidth></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetLayersBoxStyle></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetLayersGripStyle></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SoftCrop></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.SoftCropValue></C> </R>
</TABLE>

<FI>Animations and Transitions<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnView.AbortTransition></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.PrepareTransition></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.RunTransition></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.TransitionRunning></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.TransitionTiming></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.Playing></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.PlayLoop></C> </R>
</TABLE>

<FI>Navigator<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.IsNavigator></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.SetNavigator></C> </R>
</TABLE>

<FI>Animated Polygons<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnView.AnimPolygonAddPt></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.AnimPolygonClear></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.AnimPolygonDel></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.AnimPolygonNew></C> </R>
</TABLE>

<FI>Other<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TImageEnView.Assign></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.AutoCursors></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.Cursor></C> </R>
<R> <C_IMG_METHOD> <C><A TIEView.GetCanvas></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.BackBuffer></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.BeginPostFrames></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.DoubleBuffer></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TImageEnView.DrawVersion></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.EndPostFrames></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.GetGripAt></C> </R>
<R> <C_IMG_PROPERTY> <C><A TImageEnView.HighlightedPixel></C> </R>
<R> <C_IMG_METHOD> <C><A TImageEnView.MoveContentTo></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnAcquireBitmap></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnBeforeSelectionChange></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDrawBackBuffer></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDrawBackground></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDrawCanvas></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDrawLayer></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDrawLayerBox></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDrawLayerGrip></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDrawPolygon></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDShowEvent></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnDShowNewFrame></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnFinishSmoothTask></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnFinishWork></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnImageChange></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnImageEnGesture></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnLayerNotify></C> </R>
<R> <C_IMG_EVENT> <C><A TIEView.OnMouseEnter></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnMouseInResizingGrip></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnMouseInSel></C> </R>
<R> <C_IMG_EVENT> <C><A TIEView.OnMouseLeave></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnPaint></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnProgress></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnSaveUndo></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnSelectionChange></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnSelectionChanging></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnSetCursor></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnSpecialKey></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnTransitionPaint></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnTransitionStep></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnTransitionStop></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnViewChange></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnViewChanging></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnVirtualKey></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnZoomIn></C> </R>
<R> <C_IMG_EVENT> <C><A TImageEnView.OnZoomOut></C> </R>
</TABLE>
!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TImageEnView = class(TIEView)
  private

    fLegacyBitmap: boolean;           // false=use TIEBitmap, true=use TBitmap
    fUseDrawDibDraw: boolean;         // for same behavior of old versions set it to True
    fLCursor: TCursor;                // this is default cursor
    fCursor: TCursor;                 // this is current cursor
    fWasScreenCursor : TCursor;       // Store the screen's cursor for special handling on mouse down
    fOnImageChange: TNotifyEvent;
    fOnViewChange: TViewChangeEvent;
    fOnImageEnGesture: TIEImageEnGestureEvent;
    fOnFinishSmoothTask: TIEFinishSmoothTaskEvent;
    fOnViewChanging: TViewChangingEvent;
    fImageHorizAlignment: TIEHAlign;
    fImageVertAlignment: TIEVAlign;
    fZoomFilter: TResampleFilter;
    fSaveZoomFilter: TResampleFilter; // saved zoomfilter from mousedown to mouseup
    fDelayZoomFilter: boolean;        // if true enables fStable, delay zoom filter
    fBackgroundStyle: TIEBackgroundStyle;
    fMagicWandMaxFilter: boolean;     // if true apply a max filter to the selection
    fMagicWandTolerance: integer;     // 0..255 tolerance
    fMagicWandMode: TIEMagicWandMode;
    fRXScroll, fRYScroll: double;
    fDisplayGrid: boolean;
    fDisplayGridLyr: integer; // layer where to display the grid (-1 = current, >= 0 specific layer)
    fHighlightedPixel: TPoint;
    fVScrollBarVisible, fHScrollBarVisible: boolean;
    fAniCounter: integer;
    fOnPaint: TNotifyEvent;
    fOnMouseInResizingGrip: TIEMouseInResizingGripEvent;
    fOnZoomIn, fOnZoomOut: TIEZoomEvent;
    fDelayDisplaySelection: boolean;
    fOnProgress: TIEProgressEvent;
    fOnFinishWork: TNotifyEvent;
    fOnAcquireBitmap: TIEAcquireBitmapEvent;
    fFullUpdateRequest: boolean;    // true when Paint requires to repaint full image without cliprect, Paint event set to False
    fBlockPaint: boolean;
    fRectMoving: boolean;           // true if we are moving a rectangle
    fDrawVersion: boolean;
    fOnDrawBackBuffer: TNotifyEvent;
    fOnLayerNotify: TIELayerNotify;
    fSelectionAspectRatio: double; // -1 auto aspect , 0 = absolute (fSelectionAbsWidth, fSelectionAbsHeight), >0 specify aspect
    fSelectionAbsWidth: integer;
    fSelectionAbsHeight: integer;
    fOnSpecialKey: TIESpecialKeyEvent;
    fNavigator: TImageEnView;
    fNavigatorOptions: TIENavigatorOptions;
    fNavigatorInside: Boolean;
    fIsNavigator: Boolean;
    fNavigatorBackBuffer: TIEBitmap;
    fNavigatorActualRect: TRect;
    fUpdateInsideUpdate: Boolean;  // true when Update call itself
    fInsideUpdate: Boolean;        // true when it is inside Update
    fDelayTimer: Integer;
    fLyrResizingClientAreaBox: TRect;  // ClientAreaBox when we begon resizing a layer
    fLyrResizingRotatedBox: array [0..3] of TPoint;  // box coordinates when we begon resizing a rotated layer
    fZoomSelectionAspectRatio: Boolean;
    fMouseScrollRate: Double;
    fOnDrawPolygon: TIEOnDrawPolygon;
    fLayersRotationFilter: TIEAntialiasMode;
    fLayersRotationAntialias: Boolean;
    fLayersRotationUseFilterOnPreview: Boolean;
    fLayersRotationDelayFilterOnPreview: Boolean;
    fUpdate_MaskCache: Integer;  // -1=update all, >-1 update only specified layer
    fLayersSelectConstrains: Boolean;
    fSelectionGridWidth: Integer;
    fSelectionGridHeight: Integer;
    fMarkOuterAlpha: Integer;
    fMarkOuterColor: TColor;
    fLayersResizeAspectRatio: TIELayersResizeAspectRatio;
    fLayersResizingAR: Double; // used when resizing layers
    fOnSetCursor: TIESetCursorEvent;
    //
    fLutLastZoomX: double;  // last zoom used by CreateCoordConvLUT
    fLutLastZoomY: double;  // last zoom used by CreateCoordConvLUT
    fLutLastFRX: integer;   // last frx used by CreateCoordConvLUT
    fLutLastFRY: integer;   // last fry used by CreateCoordConvLUT
    fLutLastMaxLayerWidth: integer;   // fMaxLayerWidth used by CreateCoordConvLUT
    fLutLastMaxLayerHeight: integer;  // fMaxLayerHeight used by CreateCoordConvLUT
    // handscroll
    fHSVX1, fHSVY1: integer; // view in mouse down
    //
    fo1x, fo1y, fo2x, fo2y: integer;
    frx, fry: integer;
    //
    fBitmapWidth, fBitmapHeight: integer;         // monitoring bitmap size
    // transition effects
    fTransition: TIETransitionEffects;            // effect engine
    fTransitionEffect: TIETransitionType;         // transition type
    fTransitionDuration: integer;                 // transition duration ms
    fMinBitmapSize: Integer;                       // min bitmap size to make it displyed
    //
    {$IFDEF IEINCLUDEDIRECTSHOW}
    fOnDShowNewFrame: TNotifyEvent;
    fOnDShowEvent: TNotifyEvent;
    {$ENDIF}
    {$IFDEF IEINCLUDEMEDIAFOUNDATION}
    fOnMediaFoundationNotify: TIEMediaFoundationNotifyEvent;
    {$ENDIF}
    fFlatScrollBars: Boolean;
    fOnDrawBackground: TIEOnDrawBackground;
    fOnDrawCanvas: TIEOnDrawCanvas;
    fOldHandle: THandle;
    fOnDrawLayer: TIEDrawLayerEvent;
    fOnDrawLayerBox: TIEDrawLayerBoxEvent;
    fOnDrawLayerGrip: TIEDrawLayerGrip;
    fVisibleSelection: Boolean;
    fOffscreenPaint: Boolean;
    fFirstTimeSetCursor: Boolean;
    fLayersRotateStep: Integer;
    fAutoFixRotationBorders: Boolean;  // true = allowed to call LayersFixBorders automatically
    // Wall paper
    fWallPaper: TPicture;
    fWallPaperStyle: TIEWallPaperStyle;
    // setpixel replacement (due the Vista setpixel bug)
    fDrawPixelBitmap: TBitmap;
    fDrawPixelPtr: PRGB;
    // softcrop
    fSoftCrop: TIESoftCropMode;
    fSoftCropValue: Integer;
    // hints
    fEnableInteractionHints: Boolean;
    fInteractionHint: String;
    fInteractionHintX, fInteractionHintY: Integer;
    fInteractionHintMinText: String;
    // PostFrames support fields
    fPostFrames: TList; // a list of TIEPostFramesTarget objects
    
    // Play
    fPlaying: boolean;                  // true=play actived
    fPlayTimer: integer;                // timer for playback of animated files (0=not allocated)
    fPlayLoop: boolean;                 // when True executes in loop

    procedure RemovePostFrames(index: Integer);
    //
    procedure AniPolyFunc(Sender: TObject; UseLockPaint: boolean);
    procedure TimerEvent(Sender: TObject);
    procedure SetViewX(v: integer);
    procedure SetViewY(v: integer);
    procedure SetZoom(v: double);
    procedure SetZoomX(v: double);
    procedure SetZoomY(v: double);
    function GetSelX1: integer;
    function GetSelY1: integer;
    function GetSelX2: integer;
    function GetSelY2: integer;
    procedure SetCenter(value: boolean);
    function GetCenter(): Boolean;
    procedure SetImageHorizAlignment(value: TIEHAlign);
    procedure SetImageVertAlignment(value: TIEVAlign);
    function GetMouseInteract: TIEMouseInteract;
    procedure SetBackgroundStyle(v: TIEBackgroundStyle);
    procedure DoSelectionChange;
    procedure DoSelectionChanging;
    procedure DoBeforeSelectionChange;
    procedure DoMouseInResizingGrip(Grip: TIEGrip);
    procedure DoMouseInSel;
    procedure DoZoomIn(var NewZoom: double);
    procedure DoZoomOut(var NewZoom: double);
    procedure SetSelectionOptions(v: TIESelectionOptions);
    function GetSelectedRect(): TIERectangle;
    function GetResizingGrip(x, y: integer; Shift: TShiftState): TIEGrip;
    function GetMovingGrip(x, y: integer; Shift: TShiftState): integer;
    function SelectResizeEx(grip: TIEGrip; newx, newy: integer; aspectratio: boolean): TIEGrip;
    function SelectMoveEx(fMoving, ox, oy: integer; cutBorders: Boolean): integer;
    function GetScrollBarsAlwaysVisible: boolean;
    procedure SetScrollBarsAlwaysVisible(v: boolean);
    procedure SetDisplayGrid(v: boolean);
    procedure SetDisplayGridLyr(v: integer);
    function GetTransitionRunning: boolean;
    function GetLayersCount: integer;
    function GetLayer(idx: integer): TIELayer;
    procedure PaintSelection(OutBitmap: TBitmap);
    procedure PaintGrid(OutBitmap: TBitmap);
    procedure SetCursor(Value: TCursor);
    procedure SetDelayTimer(Value: integer);
    function GetDelayTimer: integer;
    procedure SetGradientEndColor(Value: TColor);
    procedure DoDrawVersion;
    procedure SetDrawVersion(v: boolean);
    procedure SetSelectionMaskDepth(value: integer);
    procedure MouseResizeLayer(clientlx, cliently: integer; AltPressed: Boolean);
    {$IFDEF IEINCLUDEDIRECTSHOW}
    procedure DShowNewFrame(var Message: TMessage); message IEM_NEWFRAME;
    procedure DShowEvent(var Message: TMessage); message IEM_EVENT;
    {$ENDIF}
    {$IFDEF IEINCLUDEMEDIAFOUNDATION}
    procedure MediaFoundationNotify(var Message: TMessage); message IEM_MEDIAFOUNDATION;
    {$ENDIF}
    procedure MouseSelectCircle(x, y: integer; Shift: TShiftState);
    procedure MouseSelectRectangle(x, y: integer; Shift: TShiftState);
    procedure SetSelectionAspectRatio(value: Double);
    function GetSavedSelectionsCount: Integer;
    {$ifdef IEINCLUDEFLATSB}
    procedure SetFlatScrollBars(Value: Boolean);
    {$endif}
    procedure SwapSelectionBase;
    procedure NavigatorSelectionChanging(Sender: TObject);
    procedure NavigatorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SetNavigatorRect;
    function GetImageEnVersion: String;
    procedure SetImageEnVersion(Value: String);
    {$ifdef IEDOTNETVERSION}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$endif}
    procedure WMGestureNotify(var Msg: TIEWMGestureNotify); message IEWM_GESTURENOTIFY;
    procedure WMGesture(var Msg: TMessage); message IEWM_GESTURE;
    procedure SetOnTransitionStop(value: TNotifyEvent);
    function GetOnTransitionStop: TNotifyEvent;
    procedure SetOnTransitionStep(value: TIETransitionStepEvent);
    function GetOnTransitionStep: TIETransitionStepEvent;
    procedure SetTransitionTiming(value: TIETransitionTiming);
    function GetTransitionTiming: TIETransitionTiming;
    procedure SetWallPaper(Value: TPicture);
    procedure SetWallPaperStyle(Value: TIEWallPaperStyle);
    function AdjustRectToAspectRatio(ARect: TRect): TRect; // Returns an adjusted rect taking into account the aspect ratio of the image and display window
    procedure DoOnDrawPolygon(polygon: Integer; point: Integer; Canvas: TCanvas; x, y: Integer);
    function GetBackBuffer: TBitmap;
    procedure SetOnSaveUndo(value: TIESaveUndoEvent);
    function GetOnSaveUndo: TIESaveUndoEvent;
    procedure ZoomSelectionEx(AspectRatio: Boolean; FireDoZoomIn: Boolean);
    procedure SetSoftCrop(v: TIESoftCropMode);
    procedure SetSoftCropValue(v: Integer);
    procedure SetHighlightedPixel(v: TPoint);
    procedure SetOnTransitionPaint(const Value: TIEOnTransitionPaint);
    function GetOnTransitionPaint: TIEOnTransitionPaint;
    procedure SetSmoothScrollValue(v: Integer);
    procedure SetSmoothZoomValue(v: Integer);
    procedure SetupAniPolyTimer;
    procedure SetupTransition;
    procedure SetupDrawPixelBitmap;
    procedure TerminatePolySelection(shift: Boolean; removeLastPoint: Boolean);
    procedure DrawMarkOutNavigator(const rc: TRect);
    procedure SetHighlightedPixelColor(c: TColor);
    function GetSelectionGridSize(): Integer;
    procedure SetSelectionGridSize(value: Integer);
    procedure SetPlaying(const v: boolean);
    procedure StartPlayTimer;
    procedure StopPlayTimer;
    procedure DoImageEnGestureEvent(const GInfo: TIEGESTUREINFO; var Handled: boolean); virtual;
    function PerformRotateSnap(Value: double): double;
    function PerformLayerMoveSnap(Value: double): double;
    function PerformZoomSnap(Value: double): double;

  protected

    // view and selections stabilization
    fStableReset: integer;  // initial fStable value (also available as public property)
    fStable2Reset: integer; // initial fStable2 value
    fStable: integer;       // stability counter (0=stable) , decremented each AniPolyFunc
    // setted by SetZoom, SetView## (used for ZoomFilter)
    fStable2: integer;      // secondary stablity counter (for selections)
    //
    fUpdateReason: TIEUpdateReason;
    fDBToDraw: boolean;   // if true draw fTBitmap
    fTBitmap: TIEBitmap;  // back buffer for DoubleBuffer
    fViewX, fViewY: integer;
    fZoomX: double;       // zoom percentage (starting at 1)
    fZoomY: double;       // zoom percentage (starting at 1)
    fZoomD100X: double;   // fZoomX/100
    f100DZoomX: double;   // 100/fZoomX
    fZoomD100Y: double;   // fZoomX/100
    f100DZoomY: double;   // 100/fZoomX
    fHDrawDib: HDRAWDIB;  // direct draw on the screen
    fScrollBars: TScrollStyle;
    fLockPaint: integer;    // 0=paint locked
    fOffX, fOffY: integer;  // start of view (for centered images)
    fExtX, fExtY: integer;  // view extent
    fZZWW, fZZHH: integer;  // size of zoomed bitmap
    fBitmap: TBitmap;
    fBitmapInfoHeader256: TBitmapInfoHeader256; // used by DrawTo and PaintRect
    fDoubleBuffer: boolean;     // always true (not used)
    fAutoFit: boolean;
    fUpdateInvalidate: boolean; // if true (default) Update will call Invalidate
    fAutoStretch: boolean;
    fAutoShrink: boolean;
    // selections
    fSel: boolean;              // True when the selection is active
    fPolySelecting: boolean;    // True on polygonal selection
    fLassoSelecting: boolean;   // True on polygonal and lasso selection
    fRectSelecting: boolean;    // True on rectangular selection
    fCircSelecting: boolean;    // True on circular selection
    fRectResizing: TIEGrip;     // if <>ieNone we are resizing rectangular selection
    fSelectMoving: integer;     // if >-1 selection moving
    fMMoveX, fMMoveY: integer;  // used on fPolySelecting
    fHSX0, fHSY0: integer;              // mouse down coordinates (clipped at bakcground layer)
    fHSX1, fHSY1: integer;              // mouse down coordinates (clipped at current layer)
    fPredX, fPredY: integer;            // last call to MouseMove coordinates
    fPredLX, fPredLY: integer;          // last call to MouseMove coordinates (real coordinates)
    fMouseDownX, fMouseDownY: integer;  // non clipped mouse down coordinates
    fHPolySel: pointer;                 // pointer to the current animated polygon
    fSelectionBase: TIESelectionBase;   // coordinate system for selection (bitmap or client area)
    fSavedSelectionBase: TIESelectionBase;
    fOnSelectionChanging: TNotifyEvent;
    fOnSelectionChange: TNotifyEvent;
    fOnBeforeSelectionChange: TNotifyEvent;
    fOnMouseInSel: TNotifyEvent;
    fSelectionOptions: TIESelectionOptions;
    fStartingPolyCount: integer;    // polycount at MouseDown time
    fSelectionMask: TIEMask;        // selection mask
    fSelectionMaskDepth: integer;   // selection mask depth
    fSelectionIntensity: integer;   // must be 1 if fSelectionMaskDepth is 1, otherwise a value from 0 to 255
    fSelColor1, fSelColor2: TColor; // selection colors

    fGripColor1, fGripColor2: TColor; // grip color
    fGripBrushStyle: TBrushStyle;
    fGripSize: integer;
    fGripShape: TIEGripShape;
    fExtendedGrips: boolean;

    fLyrGripColor1, fLyrGripColor2: TColor;
    fLyrGripBrushStyle: TBrushStyle;
    fLyrGripSize: integer;
    fLyrGripShape: TIEGripShape;

    fChessboardSize: integer;
    fChessboardBrushStyle: TBrushStyle;
    fForceALTkey: boolean;
    fMouseMoveScrolling: boolean; // true if we are inside MouseMoveScroll
    fSavedSelection: TList;       // list of saved selections
    fEnableShiftKey: boolean;     // Enable/disable shift key to add multi selections
    fGradientEndColor: TColor;
    fUpdateLocked: integer;   // 0=unlocked
    // animated polygons
    fAnimPoly: TList;       // animated polygons list (TIEAnimPoly)
    fAnimPolyTimer: TTimer; // animation timer
    // smooth scroll
    fSmoothScrollTimer: TTimer;
    fSmoothScrollDestX, fSmoothScrollDestY: Integer;
    fSmoothScrollValue: Integer;
    // smooth zoom
    fSmoothZoomTimer: TTimer;
    fSmoothZoomDest: Double;
    fSmoothZoomValue: Integer;
    //
    fMouseInteract: TIEMouseInteract;
    fScrollBarsAlwaysVisible: boolean; // true if the scrollbars are always visible
    fVScrollBarParams: TIEScrollBarParams;
    fHScrollBarParams: TIEScrollBarParams;
    fMouseWheelParams: TIEMouseWheelParams;
    flx1, fly1, flx2, fly2: integer;
    // alpha channel
    fEnableAlphaChannel: boolean;
    //
    fIEBitmap: TIEBitmap; // contains also the alpha channel
    // encapsulated components
    fImageEnIO: TImageEnIO;
    fImageEnProc: TImageEnProc;
    // layers
    fLayers: TList;           // list of TIELayer objects
    fLayersCurrent: integer;  // 0=first layer
    fMaxLayerWidth: integer;
    fMaxLayerHeight: integer;
    fLayersAutoUndo: Boolean;
    //
    fLayersSync: boolean;           // true=layers will have same size (default=false)
    fLayersDrawBox: boolean;
    fMovingLayer: integer;          // -1/0 no layers moving, > 0 moving a layer
    fLayerMoved: Boolean;
    fRotatingLayer: integer;        // -1 no layers rotating, >= 0 rotating layer
    fRotatingLayerValue: Double;
    fMovingRotationCenter: integer; // -1 no layers moving rotation center
    fLayerResizing: TIEGrip;        // ieNone no layer grip
    fMovResRotLayerStarted: Boolean;
    // conversion LUT
    fGXScr2Bmp, fGYScr2Bmp: pintegerarray;  // LUT used to convert screen based coordinate to bitmap
    fGXScr2BmpSize, fGYScr2BmpSize: Integer; // size of fGXScr2Bmp and fGYScr2Bmp LUTs
    fXScr2Bmp, fYScr2Bmp: pintegerarray;    // LUT used to convert screen based coordinate to bitmap
    fXScr2BmpSize, fYScr2BmpSize: integer;  // size of fXScr2Bmp and fYScr2Bmp LUTs
    fXBmp2Scr, fYBmp2Scr: pintegerarray;    // LUT used to convert bitmap based coordinate to screen
    fGXBmp2Scr, fGYBmp2Scr: pintegerarray;  // LUT used to convert bitmap based coordinate to screen
    fXBmp2ScrSize, fYBmp2ScrSize: integer;  // size of fXBmp2Scr, fYBmp2Scr LUTs
    //
    fAutoCursors: Boolean; // If true (default) ImageEn handles mouse cursors
    //
    fOnSaveUndo: TIESaveUndoEvent;
    fOnVirtualKey: TIEVirtualKeyEvent;
    fLayerBoxPen: TPen;
    fHighlightedPixelColor: TColor;
    fImageSet : Boolean;  // Becomes true once we set or assign an image
    // Gestures
    fGestures: TIEViewerGestures;
    fGestureStartValue: integer;
    fGestureStartX: integer;
    fGestureStartY: integer;
    fGestureBaseZoom: double;
    fGestureBaseViewX: integer;
    fGestureBaseViewY: integer;
    fGestureBaseRotate: double;
    fGestureBasePosX: integer;
    fGestureBasePosY: integer;
    //
    procedure CalcEdges(var EdgeX, EdgeY: integer; NoVertSB, NoHorizSB: boolean);
    procedure SetBackGround(cl: TColor); override;
    procedure PolyDraw1;
    procedure SetScrollBars(v: TScrollStyle); virtual;
    procedure CreateHandle(); override;
    function GetOptimalZoom(ibitmapwidth, ibitmapheight, iwidth, iheight: integer; bolStretch, bolShrink: Boolean): double;
    // cross platform events
    procedure DoSize;
    procedure DoVertScroll(command: TIEScrollCommand; Position: integer);
    procedure DoHorizScroll(command: TIEScrollCommand; Position: integer);
    procedure DoMouseWheelScroll(Delta: integer; mouseX, mouseY: integer; CtrlPressed: boolean);
    procedure DoDoubleClickEx(Shift: boolean);
    // platform specific events
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Message: TMessage); message WM_HSCROLL;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure WMMouseHWheel(var Message: TMessage); message IEWM_MOUSEHWHEEL;
    function _CreatePalette: HPalette;
    function PaletteChanged(Foreground: Boolean): Boolean; {$ifndef FPC} override; {$endif}
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure IEMUpdate(var Message: TMessage); message IEM_UPDATE;
    procedure IEMProgress(var Message: TMessage); message IEM_PROGRESS;
    procedure IEMFinishWork(var Message: TMessage); message IEM_FINISHWORK;
    procedure CNKEYDOWN(var Message: TMessage); message CN_KEYDOWN;  // we handle CN_ instead of WM_ to be sure key events arrive here
    procedure CNKEYUP(var Message: TMessage); message CN_KEYUP;  // we handle CN_ instead of WM_ to be sure key events arrive here
    procedure WMSYSKEYDOWN(var Message: TMessage); message WM_SYSKEYDOWN;
    procedure WMSYSKEYUP(var Message: TMessage); message WM_SYSKEYUP;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    //
    procedure RestoreCursor;
    procedure ShowSelectionEx(d: boolean);
    procedure HideSelectionEx(dd: boolean);
    function GetIdealComponentWidth: integer;
    function GetIdealComponentHeight: integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function GetPolySel(idx: integer): TPoint;
    function GetPolySelCount: integer;
    procedure SetMouseInteract(v: TIEMouseInteract); virtual;
    procedure AnimPolygonAddPtEx(ap: pointer; x, y: integer);
    procedure AnimPolygonAddBreak(ap: pointer);
    procedure AnimPolygonRemoveLast(ap: pointer);
    procedure AnimPolygonRemoveLastPoint(ap: pointer);
    procedure AnimPolygonMove(ap: pointer; ox, oy: integer; max_x, max_y: integer; CutSel: boolean);
    procedure ViewChange(c: integer); virtual;
    procedure ViewChanging(c: integer; newValue: Double); virtual;
    procedure SetZoomFilter(v: TResampleFilter);
    procedure SubMouseMoveScroll(scx, scy: integer); virtual;
    function MouseMoveScroll: boolean;
    function GetFBitmap: TBitmap; override;
    function GetIEBitmap: TIEBitmap; override;
    procedure SetVisibleSelection(vv: boolean);
    function GetVisibleSelection: boolean;
    function GetPolySelArray: PPointArray;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure AddSelPointEx(x, y: integer);
    procedure AddSelBreakEx;
    procedure SelectEx(x1, y1, x2, y2: integer; Op: TIESelOp; aspectratio: boolean; adjRect: Boolean);
    function GetClientWidth: integer; virtual;
    function GetClientHeight: integer; virtual;
    function HasParentWindow: boolean; virtual;
    procedure SetSelColor1(v: TColor);
    procedure SetSelColor2(v: TColor);
    function GetImageEnIO: TImageEnIO;
    function GetImageEnProc: TImageEnProc;
    procedure SetEnableAlphaChannel(v: boolean);
    procedure SetOnProgress(v: TIEProgressEvent); virtual;
    function GetOnFinishWork: TNotifyEvent; virtual;
    procedure SetOnFinishWork(v: TNotifyEvent); virtual;
    procedure SetOnAcquireBitmap(v: TIEAcquireBitmapEvent); virtual;
    function GetOnAcquireBitmap: TIEAcquireBitmapEvent; virtual;
    function GetOnProgress: TIEProgressEvent; virtual;
    function GetHasAlphaChannel: boolean; override;
    function GetAlphaChannel: TIEBitmap; override;
    function GetIsEmpty: boolean; virtual;
    function GetIsEmpty2: boolean; virtual;
    function RequiresScrollBars: boolean;
    procedure SetLayersCurrent(Value: integer); virtual;
    procedure DoLayersSync;
    procedure SetLegacyBitmap(Value: boolean); virtual;
    procedure CreateCoordConvLUT; virtual;
    procedure CalcPaintCoords; virtual;
    procedure CalcPaintCoordsEx(var XSrc, YSrc, SrcWidth, SrcHeight: integer; var DstWidth, DstHeight: integer; tViewX, tViewY: integer);
    procedure UpdateLimits; virtual;
    function QuantizeViewX(vx: integer): integer;
    function QuantizeViewY(vy: integer): integer;
    procedure HideHorizScrollBar;
    procedure HideVertScrollBar;
    procedure SetSelectionIntensity(value: integer);
    procedure SetLayersSync(value: boolean);
    procedure SetLayersDrawBox(value: boolean);
    procedure DoLayerNotify(layer: integer; event: TIELayerEvent); virtual;
    procedure SetTempCursor(Value: TCursor; bOnMouseDown : Boolean = False);
    function GetCurrentLayer: TIELayer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetFitValueXY(var x, y: Double);
    function IsPointInsideLayer(x, y: Integer; layer: Integer): Boolean;
    procedure OnSmoothSetView(Sender: TObject);
    procedure StopSmoothScroll();
    procedure OnSmoothZoom(Sender: TObject);
    procedure StopSmoothZoom();
    procedure DrawSelectionGrid(x1, y1, x2, y2: Integer); virtual;
    procedure AfterDrawLayer(layerIndex: Integer; DestBitmap: TIEBitmap; const DestRect: TRect); virtual;
    procedure SetVisibleBitmapRect(ARect: TRect);
    function GetVisibleBitmapRect : TRect;

  public

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure PaintToEx(ABitmap: TIEBitmap; ABitmapScanline: ppointerarray; UpdRect: PRect; drawBackground: Boolean; drawGadgets: Boolean); virtual;
    procedure GetRenderRectangles(var xDst, yDst, dxDst, dyDst: integer; var xSrc, ySrc, dxSrc, dySrc: integer);
    procedure GetMaxViewXY(var mx, my: integer);
    procedure SetZoomNoUpdate(v: double); virtual;
    procedure SetZoomXNoUpdate(v: double); virtual;
    procedure SetZoomYNoUpdate(v: double); virtual;
    function GetFitValue: double;
    procedure DrawBackgroundToCanvas(ACanvas: TCanvas; iWidth: Integer = -1; iHeight: Integer = -1);
    procedure MoveContentTo(Destination: TImageEnView);
    procedure SaveState(const FileName: String); overload;
    procedure SaveState(Stream: TStream); overload;
    procedure LoadState(const FileName: String); overload;
    procedure LoadState(Stream: TStream); overload;
    procedure ResetState();

    // Selection
    procedure Select(x1, y1, x2, y2: integer; Op: TIESelOp = iespReplace); virtual;
    procedure SelectMagicWand(x, y: integer; Op: TIESelOp = iespReplace);
    procedure SelectColors(StartColor, FinalColor: TRGB; Op: TIESelOp = iespReplace); overload;
    procedure SelectColors(Color: TRGB; Op: TIESelOp = iespReplace); overload;
    procedure SelectColors(ColorIndex: integer; Op: TIESelOp = iespReplace); overload;
    procedure SelectEllipse(CenterX, CenterY, Width, Height: integer; Op: TIESelOp = iespReplace);
    procedure SelectRoundRect(Left, Top, Right, Bottom, RoundWidth, RoundHeight: integer; Op: TIESelOp = iespReplace);
    procedure SelectCustom;
    procedure MoveSelection(OffsetX, OffsetY: integer);
    property SelectionAspectRatio: double read fSelectionAspectRatio write SetSelectionAspectRatio;
    procedure SetSelectionMarkOuterStyle(Alpha: Integer; Color: TColor);

{!!
<FS>TImageEnView.SelectionAbsWidth

<FM>Declaration<FC>
property SelectionAbsWidth: Integer;

<FM>Description<FN>
Specifies the fixed selection width, when <A TImageEnView.SelectionAspectRatio> is 0.

See also: <A TImageEnView.SelectionAbsHeight>.

<FM>Example<FC>
// we want a fixed selection of 100 x 100 pixels
ImageEnView.SelectionAbsWidth    := 100;
ImageEnView.SelectionAbsHeight   := 100;
ImageEnView.SelectionAspectRatio := 0;
!!}
    property SelectionAbsWidth: integer read fSelectionAbsWidth write fSelectionAbsWidth;

{!!
<FS>TImageEnView.SelectionAbsHeight

<FM>Declaration<FC>
property SelectionAbsHeight: Integer;

<FM>Description<FN>
Specifies the fixed selection height, when <A TImageEnView.SelectionAspectRatio> is 0.

See also: <A TImageEnView.SelectionAbsWidth>.

<FM>Example<FC>
// we want a fixed selection of 100 x 100 pixels
ImageEnView.SelectionAbsWidth    := 100;
ImageEnView.SelectionAbsHeight   := 100;
ImageEnView.SelectionAspectRatio := 0;
!!}
    property SelectionAbsHeight: integer read fSelectionAbsHeight write fSelectionAbsHeight;

{!!
<FS>TImageEnView.AutoFixRotationBorders

<FM>Declaration<FC>
property AutoFixRotationBorders: Boolean;

<FM>Description<FN>
If true (default) ImageEn will automatically call <A TImageEnView.LayersFixBorders> method prior to rotation.
!!}
    property AutoFixRotationBorders: Boolean read fAutoFixRotationBorders write fAutoFixRotationBorders;

    procedure DeSelect;
    property SelX1: integer read GetSelx1;
    property SelY1: integer read GetSely1;
    property SelX2: integer read GetSelx2;
    property SelY2: integer read GetSely2;
    procedure AddSelPoint(x, y: integer); virtual;
    procedure AddSelBreak;
    procedure DelLastSelPoint();
    property PolySel[idx: integer]: TPoint read GetPolySel;
    property PolySelCount: integer read GetPolySelCount;
    property PolySelPoints: PPointArray read GetPolySelArray;
    property VisibleBitmapRect : TRect read GetVisibleBitmapRect write SetVisibleBitmapRect;

    property SmoothScrollValue: Integer read fSmoothScrollValue write SetSmoothScrollValue;

    property SmoothZoomValue: Integer read fSmoothZoomValue write SetSmoothZoomValue;

{!!
<FS>TImageEnView.Selected

<FM>Declaration<FC>
property Selected: boolean;

<FM>Description<FN>
Selected is True if any portion of the image is selected. Selected is True after you have called the Select method. Selected is False after you have called <A TImageEnView.DeSelect> method.
If Selected is True, the properties <A TImageEnView.SelX1>, <A TImageEnView.SelY1>, <A TImageEnView.SelX2>, <A TImageEnView.SelY2> are valid.

Read-only
!!}
    property Selected: boolean read fSel;

{!!
<FS>TImageEnView.SelectedRect

<FM>Declaration<FC>
property SelectedRect: <A TIERectangle>;

<FM>Description<FN>
Returns current selected rectangle. If no selected exists returns the whole image rectangle.
The coordinate has the same dimensions of the bitmap (i.e. it is not affected by <A TImageenView.Zoom> and <A TImageenView.ViewX>, <A TImageenView.ViewY> values).
You can programatically make a selection using the <A TImageEnView.Select> method.

Read-only
!!}
    property SelectedRect: TIERectangle read GetSelectedRect;

    property VisibleSelection: boolean read GetVisibleSelection write SetVisibleSelection;

{!!
<FS>TImageEnView.MagicWandMaxFilter

<FM>Declaration<FC>
property MagicWandMaxFilter: Boolean;

<FM>Description<FN>
Set MagicWandMaxFilter to True (default is False) to apply a maximum filter to the magic wand selection (remove the "black hole").

<FM>See Also<FN>
- <A TImageEnView.MagicWandMode>
- <A TImageEnView.MagicWandTolerance>
- <A TImageEnView.MouseInteract>
!!}
    property MagicWandMaxFilter: boolean read fMagicWandMaxFilter write fMagicWandMaxFilter;

{!!
<FS>TImageEnView.MagicWandTolerance

<FM>Declaration<FC>
property MagicWandTolerance: Integer;

<FM>Description<FN>
MagicWandTolerance specifies the color difference of original point and region point (default 15) when using <L TIEMouseInteractItems>miSelectMagicWand</L>. A low number selects colors of a very close match, whereas a higher value allows much broader color differences.


<FM>See Also<FN>
- <A TImageEnView.MagicWandMaxFilter>
- <A TImageEnView.MagicWandMode>
- <A TImageEnView.MouseInteract>
!!}
    property MagicWandTolerance: integer read fMagicWandTolerance write fMagicWandTolerance;

{!!
<FS>TImageEnView.MagicWandMode

<FM>Declaration<FC>
property MagicWandMode: <A TIEMagicWandMode>;

<FM>Description<FN>

MagicWandMode specifies how magic wand selection <L TIEMagicWandMode>works</L>. Default is iewInclusive.

<FM>See Also<FN>
- <A TImageEnView.MagicWandMaxFilter>
- <A TImageEnView.MagicWandTolerance>
- <A TImageEnView.MouseInteract>
!!}
    property MagicWandMode: TIEMagicWandMode read fMagicWandMode write fMagicWandMode;

    function IsPointInsideSelection(x, y: integer): boolean;

{!!
<FS>TImageEnView.SelectionMask

<FM>Declaration<FC>
property SelectionMask: <A TIEMask>;

<FM>Description<FN>
SelectionMask contains the current selection as a bit mask of 1 bit per pixel (zero is unselected, 1 is selected).

<FM>Example<FC>
// Excludes pixel 100,100 from selection
ImageEnView1.SelectionMask.SetPixel(100, 100, 0);

// To create a new selection using only SelectionMask (this select point 50,50 and 55,55)
ImageEnView1.SelectionMask.SetPixel(50, 50, 1);
ImageEnView1.SelectionMask.SetPixel(55, 55, 1);
ImageEnView1.SelectCustom();

<FM>See Also<FN>
- <A TImageEnView.SelectCustom>
!!}
    property SelectionMask: TIEMask read fSelectionMask;

    procedure InvertSelection;
    procedure SetSelectedAreaAlpha(Alpha: integer);
    procedure SetSelectionGripStyle(GripColor1: TColor=clBlack; GripColor2: TColor=clWhite; GripBrushStyle: TBrushStyle=bsSolid; GripSize: integer=5; ExtendedGrips: boolean=true; Shape: TIEGripShape = iegsCircle);
    procedure GetSelectionGripStyle(var GripColor1: TColor; var GripColor2: TColor; var GripBrushStyle: TBrushStyle; var GripSize: integer; var ExtendedGrips: boolean; var Shape: TIEGripShape);
    procedure SetLayersGripStyle(GripColor1: TColor=clBlack; GripColor2: TColor=clWhite; GripBrushStyle: TBrushStyle=bsSolid; GripSize: integer=5; Shape: TIEGripShape = iegsCircle);
    procedure SetLayersBoxStyle(PenStyle: TPenStyle=psDot; PenMode: TPenMode=pmNot; PenWidth: Integer = 1; PenColor: TColor=clBlack);
    procedure CopySelectionToBitmap(DestBitmap: TBitmap);
    procedure CopySelectionToIEBitmap(DestBitmap: TIEBitmap; FillBackground: Boolean=true);
    procedure EndSelect; virtual;
    procedure SetSelectedPixelsColor(color: TRGB);
    procedure SetAlphaRangePixelsColor(alphaMin, alphaMax: integer; color: TRGB);
    procedure SaveSelectionToStream(Stream: TStream);
    function LoadSelectionFromStream(Stream: TStream; Options: TIERSOptions=iersMoveToAdapt): boolean;
    function MergeSelectionFromStream(Stream: TStream): boolean;
    procedure SaveSelectionToFile(const FileName: String);
    function LoadSelectionFromFile(const FileName: String; Options: TIERSOptions = iersMoveToAdapt): boolean;
    function MergeSelectionFromFile(const FileName: String): Boolean;
    function RestoreSelection(Remove: Boolean=true; Options: TIERSOptions = iersMoveToAdapt): boolean;
    procedure SaveSelection;
    function DiscardSavedSelection: boolean;
    property SavedSelectionsCount: Integer read GetSavedSelectionsCount;

{!!
<FS>TImageEnView.EnableShiftKey

<FM>Declaration<FC>
property EnableShiftKey: Boolean;

<FM>Description<FN>
Enables Shift key functionality (holding down the Shift key to insert multiple selections). The default is true.
!!}
    property EnableShiftKey: boolean read fEnableShiftKey write fEnableShiftKey;

{!!
<FS>TImageEnView.LayersRotationFilter

<FM>Declaration<FC>
property LayersRotationFilter: <A TIEAntialiasMode>;

<FM>Description<FN>
Specifies the rotation filter to use when layers rotation has completed. This is important to optimize the quality of rotated image layers.

Note: You also need to set <A TImageEnView.LayersRotationAntialias> := True;  

<FM>Example<FC>
// Rotate the current layer 45 degrees clockwise at highest quality
ImageEnView1.CurrentLayer.Rotate := -45;
ImageEnView1.LayersRotationAntialias := True;
ImageEnView1.LayersRotationFilter := ierBilinear;
ImageEnView1.LayersRotationUseFilterOnPreview := true;
ImageEnView1.LayersRotationDelayFilterOnPreview := true;

// Allow the user to free rotate layers at highest quality
ImageEnView1.MouseInteract := [miRotateLayers];
ImageEnView1.LayersRotationAntialias := True;
ImageEnView1.LayersRotationFilter := ierBilinear;
ImageEnView1.LayersRotationUseFilterOnPreview := true;
ImageEnView1.LayersRotationDelayFilterOnPreview := true;

<FM>See Also<FN>
- <A TImageEnView.LayersRotationAntialias>
- <A TImageEnView.LayersRotationUseFilterOnPreview>
- <A TImageEnView.LayersRotationDelayFilterOnPreview>
!!}
    property LayersRotationFilter: TIEAntialiasMode read fLayersRotationFilter write fLayersRotationFilter;

{!!
<FS>TImageEnView.LayersRotationAntialias

<FM>Declaration<FC>
property LayersRotationAntialias: Boolean;

<FM>Description<FN>
Enables rotation anti-aliasing when layer rotation has completed (to improve their quality). Use <A TImageEnView.LayersRotationFilter> to specify the anti-aliasing algorithm.

<FM>Example<FC>
// Rotate the current layer 45 degrees clockwise at highest quality
ImageEnView1.CurrentLayer.Rotate := -45;
ImageEnView1.LayersRotationAntialias := True;
ImageEnView1.LayersRotationFilter := ierBilinear;
ImageEnView1.LayersRotationUseFilterOnPreview := true;
ImageEnView1.LayersRotationDelayFilterOnPreview := true;

// Allow the user to free rotate layers at highest quality
ImageEnView1.MouseInteract := [miRotateLayers];
ImageEnView1.LayersRotationAntialias := True;
ImageEnView1.LayersRotationFilter := ierBilinear;
ImageEnView1.LayersRotationUseFilterOnPreview := true;
ImageEnView1.LayersRotationDelayFilterOnPreview := true;

<FM>See Also<FN>
- <A TImageEnView.LayersRotationFilter>
- <A TImageEnView.LayersRotationUseFilterOnPreview>
- <A TImageEnView.LayersRotationDelayFilterOnPreview>
!!}
    property LayersRotationAntialias: Boolean read fLayersRotationAntialias write fLayersRotationAntialias;


{!!
<FS>TImageEnView.LayersRotationUseFilterOnPreview

<FM>Declaration<FC>
property LayersRotationUseFilterOnPreview: Boolean;

<FM>Description<FN>
Displays the user's rotation with the selected <L TImageEnView.LayersRotationFilter>anti-alias effect</L> immediately (i.e. before calling <A TImageEnView.LayersFixRotations>).

Note: Preview of large rotated images at high quality can be slow, so it is recommended tha you also use <A TImageEnView.LayersRotationDelayFilterOnPreview>.

<FM>Example<FC>
// Rotate the current layer 45 degrees clockwise at highest quality
ImageEnView1.CurrentLayer.Rotate := -45;
ImageEnView1.LayersRotationAntialias := True;
ImageEnView1.LayersRotationFilter := ierBilinear;
ImageEnView1.LayersRotationUseFilterOnPreview := true;
ImageEnView1.LayersRotationDelayFilterOnPreview := true;

// Allow the user to free rotate layers at highest quality
ImageEnView1.MouseInteract := [miRotateLayers];
ImageEnView1.LayersRotationAntialias := True;
ImageEnView1.LayersRotationFilter := ierBilinear;
ImageEnView1.LayersRotationUseFilterOnPreview := true;
ImageEnView1.LayersRotationDelayFilterOnPreview := true;

<FM>See Also<FN>
- <A TImageEnView.LayersRotationFilter>
- <A TImageEnView.LayersRotationAntialias>
- <A TImageEnView.LayersRotationDelayFilterOnPreview>
!!}
    property LayersRotationUseFilterOnPreview: boolean read fLayersRotationUseFilterOnPreview write fLayersRotationUseFilterOnPreview;




{!!
<FS>TImageEnView.LayersRotationDelayFilterOnPreview

<FM>Declaration<FC>
property LayersRotationDelayFilterOnPreview: Boolean;

<FM>Description<FN>
Delays the application of the <L TImageEnView.LayersRotationFilter>anti-alias effect</L> if you have enabled <A TImageEnView.LayersRotationUseFilterOnPreview> to improve the displayed quality of rotation.

This will improve performance when rotating large images.   

<FM>Example<FC>
// Rotate the current layer 45 degrees clockwise at highest quality
ImageEnView1.CurrentLayer.Rotate := -45;
ImageEnView1.LayersRotationAntialias := True;
ImageEnView1.LayersRotationFilter := ierBilinear;
ImageEnView1.LayersRotationUseFilterOnPreview := true;
ImageEnView1.LayersRotationDelayFilterOnPreview := true;

// Allow the user to free rotate layers at highest quality
ImageEnView1.MouseInteract := [miRotateLayers];
ImageEnView1.LayersRotationAntialias := True;
ImageEnView1.LayersRotationFilter := ierBilinear;
ImageEnView1.LayersRotationUseFilterOnPreview := true;
ImageEnView1.LayersRotationDelayFilterOnPreview := true;

<FM>See Also<FN>                  
- <A TImageEnView.LayersRotationUseFilterOnPreview>
- <A TImageEnView.LayersRotationFilter>
- <A TImageEnView.LayersRotationAntialias>
!!}
    property LayersRotationDelayFilterOnPreview: boolean read fLayersRotationDelayFilterOnPreview write fLayersRotationDelayFilterOnPreview;

    property SelectionMaskDepth: integer read fSelectionMaskDepth write SetSelectionMaskDepth;
    property SelectionIntensity: integer read fSelectionIntensity write SetSelectionIntensity;
    procedure MakeSelectionFeather(radius: double);
    // Animated polygons
    function AnimPolygonNew(VColor1, VColor2: TColor; VAnimated: boolean; VSizeable: boolean): pointer;
    procedure AnimPolygonDel(ap: pointer);
    procedure AnimPolygonAddPt(ap: pointer; x, y: integer);
    procedure AnimPolygonClear(ap: pointer);
    procedure AnimPolygonEnabled(ap: pointer; Value: Boolean);
    // Polygon process
    procedure CopyFromPolygon(Source: TBitmap; const Polygon: array of TPoint; PolygonLen: integer; const Position: TPoint);
    procedure CopyToPolygon(Dest: TBitmap; const Polygon: array of TPoint; PolygonLen: integer; const Position: TPoint);
    // Display
    procedure Update; override;

{!!
<FS>TImageEnView.UpdateReason

<FM>Declaration<FC>
property UpdateReason: <A TIEUpdateReason>;

<FM>Description<FN>
Advises the reason for the next <A TImageEnView.Update>. This is used to optimize the Update() task.
This property is restored to ieurDefault after Update() has executed.
!!}
    property UpdateReason: TIEUpdateReason read fUpdateReason write fUpdateReason;

    procedure UpdateNoPaint;

{!!
<FS>TImageEnView.OffScreenPaint

<FM>Declaration<FC>
property OffScreenPaint: Boolean;

<FM>Description<FN>
When the component is not visible (i.e. Visible=False) the back buffer <A TImageEnView.BackBuffer> is not painted.
If you need the back buffer to be paint correctly set OffScreenPaint to true.
!!}
    property OffScreenPaint: Boolean read fOffscreenPaint write fOffscreenPaint;

{!!
<FS>TImageEnView.SelectionGridSize

<FM>Declaration<FC>
property SelectionGridSize: Integer;

<FM>Description<FN>
When this is not 1 (Default) a grid is drawn over rectangular selections with the column and row count of SelectionGridSize (e.g. if SelectionGridSize=3 then there will be three columns and rows within the selection box). The color of the grid is specified by <A TIEImageEnGlobalSettings.SelectionGridColor>.

This property can be used in place of <A TImageEnView.SelectionGridWidth> and <A TImageEnView.SelectionGridHeight>.
!!}
    property SelectionGridSize: Integer read GetSelectionGridSize write SetSelectionGridSize;

{!!
<FS>TImageEnView.SelectionGridWidth

<FM>Declaration<FC>
property SelectionGridWidth: Integer;

<FM>Description<FN>
When this is not 1 (Default) a grid is drawn over rectangular selections with the column count of SelectionGridWidth (e.g. if SelectionGridWidth=3 then there will be three columns within the selection box). The color of the grid is specified by <A TIEImageEnGlobalSettings.SelectionGridColor>.

This property can be used in place of <A TImageEnView.SelectionGridSize>.

See also: <A TImageEnView.SelectionGridHeight>.
!!}
    property SelectionGridWidth: Integer read fSelectionGridWidth write fSelectionGridWidth;

{!!
<FS>TImageEnView.SelectionGridHeight

<FM>Declaration<FC>
property SelectionGridHeight: Integer;

<FM>Description<FN>
When this is not 1 (Default) a grid is drawn over rectangular selections with the row count of SelectionGridHeight (e.g. if SelectionGridHeight=3 then there will be three rows within the selection box). The color of the grid is specified by <A TIEImageEnGlobalSettings.SelectionGridColor>.

This property can be used in place of <A TImageEnView.SelectionGridSize>.

See also: <A TImageEnView.SelectionGridWidth>.
!!}
    property SelectionGridHeight: Integer read fSelectionGridHeight write fSelectionGridHeight;

    procedure Fit;
    procedure FitToWidth;
    procedure FitToHeight;
    procedure Stretch;
    function GetIdealZoom: double;
    property ViewX: integer read fViewX write SetViewX;
    property ViewY: integer read fViewY write SetViewY;
    procedure CenterImage;
    procedure SetViewXY(x, y: integer);
    procedure SetViewXYSmooth(x, y: Integer);
    procedure SetZoomSmooth(Zoom: Double);
    property Zoom: double read fZoomX write SetZoom;
    property ZoomX: Double read fZoomX write SetZoomX;
    property ZoomY: Double read fZoomY write SetZoomY;
    procedure ZoomSelection(AspectRatio: Boolean=true);
    procedure ZoomAt(x, y: integer; ZoomVal: double; Center: Boolean=true);
    procedure ZoomIn;
    procedure ZoomOut;
    property ClientWidth read GetClientWidth;
    property ClientHeight read GetClientHeight;
    property IdealComponentWidth: integer read GetIdealComponentWidth;
    property IdealComponentHeight: integer read GetIdealComponentHeight;
    procedure LockPaint; override;
    function UnLockPaint: integer; override;
    function NPUnLockPaint: integer; override;

{!!
<FS>TImageEnView.LockPaintCount

<FM>Declaration<FC>
property LockPaintCount: Integer; (Read-only)

<FM>Description<FN>
Returns lock painting state. 0=no lock, > 0 locking (the image will not be painted).
<A TImageEnView.LockPaint> increases LockPaintCount, <A TImageEnView.UnLockPaint> decreases it.
!!}
    property LockPaintCount: integer read fLockPaint;

    procedure DrawTo(Canvas: TCanvas);

{!!
<FS>TImageEnView.OffsetX

<FM>Declaration<FC>
property OffsetX: integer; (Read-only)

<FM>Description<FN>
Returns the horizontal position where the image has been drawn. If <A TImageEnView.ImageHorizAlignment> is iehLeft, <FC>OffsetX<FN> will be zero, whereas for <FC>iehCenter<FN> it will be (Control Width - Image Width) div 2.

Note: This value only indicates the initial draw position for an image, it is not related to scrolling (unlike <A TImageEnView.ViewX>).

<FM>See Also<FN>
- <A TImageEnView.OffsetY>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.VisibleBitmapRect>
!!}
    property OffsetX: integer read fOffX;

{!!
<FS>TImageEnView.OffsetY

<FM>Declaration<FC>
property OffsetY: integer; (Read-only)

<FM>Description<FN>
Returns the vertical position where the image has been drawn. If <A TImageEnView.ImageVertAlignment> is ievTop, <FC>OffsetY<FN> will be zero, whereas for <FC>ievCenter<FN> it will be (Control Height - Image Height) div 2.

Note: This value only indicates the initial draw position for an image, it is not related to scrolling (unlike <A TImageEnView.ViewY>).

<FM>See Also<FN>
- <A TImageEnView.OffsetX>
- <A TImageEnView.YScr2Bmp>
- <A TImageEnView.VisibleBitmapRect>
!!}
    property OffsetY: integer read fOffY;

{!!
<FS>TImageEnView.ExtentX

<FM>Declaration<FC>
property ExtentX: integer; (Read-only)

<FM>Description<FN>
ExtentX is the width of the area used to show the current image.

<FM>Example<FC>
// Copy the actual view of ImageEnView1 as the current image
ImageEnView2.Bitmap.Width := ImageEnView1.ExtentX;
ImageEnView2.Bitmap.Height := ImageEnView1.ExtentY;
ImageEnView1.DrawTo(ImageEnView2.Bitmap.Canvas);

<FM>See Also<FN>
- <A TImageEnView.ExtentY>
- <A TImageEnView.ViewX>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.VisibleBitmapRect>
!!}
    property ExtentX: integer read fExtX;

{!!
<FS>TImageEnView.ExtentY

<FM>Declaration<FC>
property ExtentY: integer; (Read-only)

<FM>Description<FN>
ExtentY is the height of the area used to show the current image.

<FM>Example<FC>
// Copy the actual view of ImageEnView1 as the current image of ImageEnView2
ImageEnView2.Bitmap.Width := ImageEnView1.ExtentX;
ImageEnView2.Bitmap.Height := ImageEnView1.ExtentY;
ImageEnView1.DrawTo(ImageEnView2.Bitmap.Canvas);

<FM>See Also<FN>
- <A TImageEnView.ExtentX>
- <A TImageEnView.ViewY>
- <A TImageEnView.YScr2Bmp>
- <A TImageEnView.VisibleBitmapRect>
!!}
    property ExtentY: integer read fExtY;

{!!
<FS>TImageEnView.ScrollBarsAlwaysVisible

<FM>Declaration<FC>
property ScrollBarsAlwaysVisible: Boolean;

<FM>Description<FN>
When the ScrollBarsAlwaysVisible property is True, the scrollbars specified in <A TImageEnView.ScrollBars> property will be displayed, even if when not required.

!!}
    property ScrollBarsAlwaysVisible: boolean read GetScrollBarsAlwaysVisible write SetScrollBarsAlwaysVisible default false;

{!!
<FS>TImageEnView.VScrollBarParams

<FM>Declaration<FC>
property VScrollBarParams: <A TIEScrollBarParams>;

<FM>Description<FN>
The VScrollBarParams property allows an application to customize the vertical scrollbar behavior, including tracking (display refresh on mouse dragging), up/down buttons pixel scrolling, pagedown/up pixel scrolling.

<FM>Example<FC>
// disable tracking
ImageEnView1.VScrollBarParams.Tracking := False;
!!}
    property VScrollBarParams: TIEScrollBarParams read fVScrollBarParams;

{!!
<FS>TImageEnView.HScrollBarParams

<FM>Declaration<FC>
property HScrollBarParams: <A TIEScrollBarParams>;

<FM>Description<FN>
The HScrollBarParams property allows an application to customize the horizontal scroll bar behavior, including tracking (display refresh on mouse dragging), left/right buttons pixel scrolling, pageleft/pageright pixel scrolling.

<FM>Example<FC>
// disable tracking
ImageEnView1.HScrollBarParams.Tracking := False;
!!}
    property HScrollBarParams: TIEScrollBarParams read fHScrollBarParams;

    procedure UpdateRect(rclip: TRect);
    procedure PaintRect(const rc: TRect);
    procedure LockUpdate;
    procedure UnLockUpdate;
    procedure UnLockUpdateEx;
    property BackBuffer: TBitmap read GetBackBuffer;
    procedure SetNavigator(nav: TImageEnView; options: TIENavigatorOptions = []);

{!!
<FS>TImageEnView.IsNavigator

<FM>Declaration<FC>
property IsNavigator: Boolean;

<FM>Description<FN>
Returns <FC>true<FN> if this object is used as a navigator of another TImageEnView object. A navigator shows a selection that controls the zoom and scroll of the main control.

See also: <A TImageEnView.SetNavigator>.
!!}
    property IsNavigator: Boolean read fIsNavigator;

    property MinBitmapSize: Integer read fMinBitmapSize write fMinBitmapSize;
    procedure ChangeResolution(NewDPI: Integer; ResampleFilter: TResampleFilter=rfNone);
    // layers
    property LayersCount: integer read GetLayersCount;
    property LayersCurrent: integer read fLayersCurrent write SetLayersCurrent;
    function LayersAdd(): integer; overload;
    function LayersAdd(Width: Integer; Height: Integer; PixelFormat: TIEPixelFormat = ie24RGB; PosX: Integer = 0; PosY: Integer = 0): Integer; overload;
    function LayersAdd(Bitmap: TIEBitmap; DoCopy: Boolean=true): integer; overload;
    function LayersAdd(FileName: WideString; PosX: Integer = 0; PosY: Integer = 0): Integer; overload;
    procedure LayersInsert(Position: integer);
    procedure LayersRemove(idx: integer); virtual;
    procedure LayersClear;
    procedure LayersMove(CurIndex, NewIndex: integer);
    procedure LayersMerge(Layer1, Layer2: integer; RemoveUpperLayer: Boolean = true); overload;
    procedure LayersMerge(LayerList: array of integer); overload;
    procedure LayersMerge(LayerList: TIEArrayOfInteger); overload;
    procedure LayersMergeTo(Layer1, Layer2: integer; Destination: TIEBitmap);
    procedure LayersDrawTo(Destination: TIEBitmap);
    procedure LayersMergeAll;
    procedure LayersCopyToAlpha(DestLayer: Integer);
    function LayersCreateFromAlpha: Integer;
    property SoftCrop: TIESoftCropMode read fSoftCrop write SetSoftCrop;
    property SoftCropValue: Integer read fSoftCropValue write SetSoftCropValue;

{!!
<FS>TImageEnView.LayersRotateStep

<FM>Declaration<FC>
property LayersRotateStep: Integer;

<FM>Description<FN>
Sets the degree of each rotation step when the user rotates a layer and presses the Shift key.
!!}
    property LayersRotateStep: Integer read fLayersRotateStep write fLayersRotateStep;

{!!
<FS>TImageEnView.LayersSelectConstrains

<FM>Declaration<FC>
property LayersSelectConstrains: Boolean;

<FM>Description<FN>
If true (default) selection constraints are active, i.e. whether the layer itself can be selected is controlled at the layer level by <A TIELayer.Selectable>.
!!}
    property LayersSelectConstrains: Boolean read fLayersSelectConstrains write fLayersSelectConstrains;

{!!
<FS>TImageEnView.Layers

<FM>Declaration<FC>
property Layers[idx: Integer]: <A TIELayer>;

<FM>Description<FN>
The Layers property provides access to properties related to a specified <L TIELayer>layer</L>.

<FM>Example<FC>

// Set the opacity of the first layer to 0.5 (50%)
ImageEnView.Layers[0].Opacity := 0.5;
ImageEnView.Update;

// Set the transparency of the first layer to 50
ImageEnView.Layers[0].Transparency := 50;
ImageEnView.Update;

// Hide layer 1
ImageEnView.Layers[1].Visible := False;
ImageEnView.Update;

// Create a magnify layer
ImageEnView.LayersSync := False;
idx := ImageEnView.LayersAdd;
with ImageEnView.Layers[idx] do
begin
  Magnify.Enabled := true;
  Magnify.Rate := 2;  // x2 magnification
  Width := 50;
  Height := 50;
end;
!!}
    property Layers[idx: integer]: TIELayer read GetLayer;

{!!
<FS>TImageEnView.LayersAutoUndo

<FM>Declaration<FC>
property LayersAutoUndo: Boolean;

<FM>Description<FN>
Allows automatic undo of layers modifications by user actions, such as moving and resizing.
!!}
    property LayersAutoUndo: Boolean read fLayersAutoUndo write fLayersAutoUndo;

    property LayersList: TList read fLayers;

{!!
<FS>TImageEnView.LayersResizeAspectRatio

<FM>Declaration<FC>
property LayersResizeAspectRatio: <A TIELayersResizeAspectRatio>;

<FM>Description<FN>
Specifies how and when ImageEn respects the aspect ratio of image layers when resizing. The default value is iearALTKey.
!!}
    property LayersResizeAspectRatio: TIELayersResizeAspectRatio read fLayersResizeAspectRatio write fLayersResizeAspectRatio;

{!!
<FS>TImageEnView.MaxLayerWidth

<FM>Declaration<FC>
property MaxLayerWidth: Integer; (Read-only)

<FM>Description<FN>
MaxLayerWidth returns the maximum width of all layers.
!!}
    property MaxLayerWidth: integer read fMaxLayerWidth;

{!!
<FS>TImageEnView.MaxLayerHeight

<FM>Declaration<FC>
property MaxLayerHeight: Integer; (Read-only)

<FM>Description<FN>
MaxLayerHeight returns the maximum height of all layers.
!!}
    property MaxLayerHeight: integer read fMaxLayerHeight;

    property LayersSync: boolean read fLayersSync write SetLayersSync;
    property LayersDrawBox: boolean read fLayersDrawBox write SetLayersDrawBox;
    function FindLayerAt(x, y: integer; SelectablesOnly: Boolean=true): integer;
    property CurrentLayer: TIELayer read GetCurrentLayer;
    procedure LayersSaveToStream(Stream: TStream; CompressionFormat: TIOFileType = -1);
    procedure LayersSaveToFile(const FileName: String; CompressionFormat: TIOFileType = -1);
    function LayersLoadFromStream(Stream: TStream): Boolean;
    function LayersLoadFromFile(const FileName: String): Boolean;
    function LayersCreateFromSelection: Integer;
    function LayersCreateFromClipboard: Integer;
    function LayersCreateFromFile(const FileName : string) : integer;
    function LayersCreateFromText(const sText : string;
                                  const sFontName : string;
                                  iFontSize : Integer;
                                  cFontColor : TColor;
                                  Style : TFontStyles;
                                  bAddShadow: boolean = False;
                                  iBlurRadius : Integer = 3;
                                  iShadowOffset : Integer = 2;
                                  Angle : Integer = 0;
                                  bAntiAlias : Boolean = true) : integer;
    procedure LayersFixSizes(layer: Integer = -1);
    procedure LayersFixRotations(layer: Integer = -1);
    procedure LayersFixBorders(layer: Integer = -1);
    // Other
    procedure SetExternalBitmap(bmp: TIEBitmap);
    property GradientEndColor: TColor read fGradientEndColor write SetGradientEndColor;
    property MouseCapture;
    procedure Assign(Source: TObject); reintroduce; virtual;
    procedure AssignSelTo(Dest: TPersistent);
    procedure ImageChange; override;
    function YScr2Bmp(y: integer): integer;
    function XScr2Bmp(x: integer): integer;
    function YBmp2Scr(y: integer): integer;
    function XBmp2Scr(x: integer): integer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property SelColor1: TColor read fSelColor1 write SetSelColor1 default clBlack;
    property SelColor2: TColor read fSelColor2 write SetSelColor2 default clWhite;
    procedure Clear;
    procedure Blank;
    property DisplayGrid: boolean read fDisplayGrid write SetDisplayGrid;
    property DisplayGridLyr: integer read fDisplayGridLyr write SetDisplayGridLyr;
    property HighlightedPixel: TPoint read fHighlightedPixel write SetHighlightedPixel;

    property HighlightedPixelColor: TColor read fHighlightedPixelColor write SetHighlightedPixelColor;

    procedure SetInteractionHint(const Text: String; x, y: Integer; const minText: String);

    property ImageHorizAlignment: TIEHAlign read fImageHorizAlignment write SetImageHorizAlignment;
    property ImageVertAlignment: TIEVAlign read fImageVertAlignment write SetImageVertAlignment;

{!!
<FS>TImageEnView.MouseWheelParams

<FM>Declaration<FC>
property MouseWheelParams: <A TIEMouseWheelParams>;

<FM>Description<FN>
Properties to customize the behavior of the mouse wheel.

<FM>Example<FC>
// mouse wheel will scroll image of 15 % of component height
ImageEnView1.MouseWheelParams.Action := iemwVScroll;
ImageEnView1.MouseWheelParams.Variation := iemwPercentage;
ImageEnView1.MouseWheelParams.value := 15;

// mouse wheel will scroll image of 1 pixel
ImageEnView1.MouseWheelParams.Action := iemwVScroll;
ImageEnView1.MouseWheelParams.Variation := iemwAbsolute;
ImageEnView1.MouseWheelParams.value := 1;

<FM>Demo<FN>
\Other\MouseWheel\MouseWheelParams.dpr
!!}
    property MouseWheelParams: TIEMouseWheelParams read fMouseWheelParams;

    procedure SetChessboardStyle(Size: integer = 16; BrushStyle: TBrushStyle=bsSolid);

{!!
<FS>TImageEnView.ForceALTkey

<FM>Declaration<FC>
property ForceALTkey: Boolean;

<FM>Description<FN>
Set ForceALTkey to True to emulate pressing the ALT key.
When this is enabled (or the Alt key is pressed) the aspect ratio is maintained when making or modifying selections (or with vectorial objects for the <A TImageEnVect> component). The default is False.
!!}
    property ForceALTkey: boolean read fForceALTkey write fForceALTkey;

{!!
<FS>TImageEnView.DelayDisplaySelection

<FM>Declaration<FC>
property DelayDisplaySelection: Boolean

<FM>Description<FN>
When True, the selection is displayed with a delay. This allows you to quickly navigate the image (zoom and scroll) and once you have finished navigation the selection is displayed.
!!}
    property DelayDisplaySelection: boolean read fDelayDisplaySelection write fDelayDisplaySelection;

{!!
<FS>TImageEnView.DelayZoomTime

<FM>Declaration<FC>
property DelayZoomTime: Integer;

<FM>Description<FN>
ImageEn has a timer that decrements a counter at each tick (you can set the tick delay using <A TImageEnView.DelayTimer> property). This timer controls the selection animation and the application of filters on scrolling (when <A TImageEnView.DelayZoomFilter> is true).
DelayZoomTime controls only the delay zoom filter application.
The default value for this property is 4 (apply the filter after 4 ticks since last scroll).
!!}
    property DelayZoomTime: integer read fStableReset write fStableReset;

    property DelayTimer: integer read GetDelayTimer write SetDelayTimer;
    procedure CopyToBitmapWithAlpha(Dest: TBitmap; DestX, DestY: integer);
    property UseDrawDibDraw: boolean read fUseDrawDibDraw write fUseDrawDibDraw;
    function GetGripAt(x, y: integer): TIEGrip;
    procedure RemoveAlphaChannel(Merge: boolean = false); override;
    property IsEmpty: boolean read GetIsEmpty;
    property IsEmpty2: boolean read GetIsEmpty2;

{!!
<FS>TImageEnView.AutoCursors

<FM>Declaration<FC>
property AutoCursors: Boolean;

<FM>Description<FN>

Set AutoCursors to False to override ImageEn's automatic mouse cursor handling.

<FM>See Also<FN>
- <A TImageEnView.Cursor>
!!}
    property AutoCursors: Boolean read fAutoCursors write fAutoCursors;

{!!
<FS>TImageEnView.ZoomSelectionAspectRatio

<FM>Declaration<FC>
property ZoomSelectionAspectRatio: Boolean;

<FM>Description<FN>
This property is active when MouseInteract contains miSelectZoom. If true (default) the selected rectangle is adjusted to maintain the aspect ratio. Otherwise (false) the image loses the aspect ratio (ZoomX <> ZoomY and Zoom value is invalid) making it stretched inside the window.
!!}
    property ZoomSelectionAspectRatio: Boolean read fZoomSelectionAspectRatio write fZoomSelectionAspectRatio;

{!!
<FS>TImageEnView.MouseScrollRate

<FM>Declaration<FC>
property MouseScrollRate: Double;

<FM>Description<FN>
If <A TImageEnView.MouseInteract> contains miScroll then MouseScrollRate specifies the relationship between mouse movement and image scrolling. The default is 1, which means that one mouse point is one pixel (at 100%).

<FM>Example<FC>
// set double rate scroll
ImageEnView1.MouseScrollRate := 2;
ImageEnView1.MouseInteract := [miScroll];
!!}
    property MouseScrollRate: Double read fMouseScrollRate write fMouseScrollRate;

    // encapsulated components

{!!
<FS>TImageEnView.IO

<FM>Declaration<FC>
property IO: <A TImageEnIO>;

<FM>Description<FN>

The IO property encapsulates the TImageEnIO component inside TImageEnView (The TImageEnIO component is automatically created the first time you access the IO property).

<A TImageEnIO> provides all functionality for loading and saving images, and other I/O functions.


<FM>Example<FC>
// Load image into the ImageEnView
ImageEnView1.IO.LoadFromFile('C:\Hello.jpeg');

// Retrieve an image from a Twain scanner and display in the ImageEnView
ImageEnView1.IO.Acquire;
!!}
    property IO: TImageEnIO read GetImageEnIO;

{!!
<FS>TImageEnView.Proc

<FM>Declaration<FC>
property Proc: <A TImageEnProc>;

<FM>Description<FN>

The Proc property encapsulates the TImageEnProc component inside TImageEnView (The TImageEnProc component is automatically created the first time you access the IO property).
The TImageEnProc component is created the first time you use Proc property.

<A TImageEnProc> provides functionality for editing and manipulating images, clipboard and analysis.

<FM>Example<FC>
// Invert the colors of the image in the ImageEnView
ImageEnView1.Proc.Negative;
!!}
    property Proc: TImageEnProc read GetImageEnProc;

    // transitions
    property TransitionRunning: boolean read GetTransitionRunning;
    procedure PrepareTransition;
    procedure RunTransition(Effect: TIETransitionType; Duration: integer); overload;
    procedure RunTransition(Effect: TIETransitionType; Duration: integer; PanZoomEffect : TIEPanZoomType; iZoomLevel : Integer); overload;
    procedure RunTransition(Effect: TIETransitionType; Duration: integer; StartRect, EndRect : TRect; bMaintainAspectRatio : Boolean = True); overload;
    procedure AbortTransition;

    {$ifdef IEINCLUDEDEPRECATEDMETHODS}
    // DEPRECATED: Use TImageEnProc.PrepareTransitionBitmaps/CreateTransitionBitmaps
    procedure PrepareTransitionBitmaps(OriginalBitmap, TargetBitmap : TBitmap; Effect : TIETransitionType; iWidth : Integer = -1; iHeight : Integer = -1); overload; {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
    procedure PrepareTransitionBitmaps(OriginalBitmap, TargetBitmap : TBitmap; Effect : TIETransitionType; StartRect, EndRect : TRect; bMaintainAspectRatio : Boolean = True; iWidth : Integer = -1; iHeight : Integer = -1); overload; {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
    procedure CreateTransitionBitmap(TransitionProgress : Single; DestBitmap : TBitmap); {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
    {$endif}

    property TransitionTiming: TIETransitionTiming read GetTransitionTiming write SetTransitionTiming;
    // post frames
    procedure BeginPostFrames(target: TImageEnView; delay: Integer; interval: Integer);
    procedure EndPostFrames(target: TImageEnView);
    // others undocumented
    procedure DrawLayerBox(ABitmap: TBitmap; idx: integer);
    procedure DrawLayerOuter(ABitmap: TBitmap; idx: Integer);
    procedure DrawLayerGrips(ABitmap: TBitmap; idx: integer);
    procedure DrawLayerRotationCenter(ABitmap: TBitmap; idx: integer);
    procedure CalcLayerGripCoords(layeridx: integer; var coords: array of TRect);
    function FindLayerGrip(x, y: integer): TIEGrip;
    function FindLayerGripDist(x, y: integer; var Distance: Double): TIEGrip;

{!!
<FS>TImageEnView.Gestures

<FM>Declaration<FC>
property Gestures: <A TIEViewerGestures>;

<FM>Description<FN>
TImageEnView supports native Windows 7 and Windows 8 gestures, allowing pan, zoom, layer rotation and movement.
ImageEn automatically handles the Windows gesture events, so adding gesture support to your applications requires only that you enable the relevant Gesture property.

<FM>Demo<FN>
Display\Gestures

<FM>Example<FC>
// enable Pan (scroll) and Zoom gestures
ImageEnView1.Gestures.Pan.Enabled := True;
ImageEnView1.Gestures.Zoom.Enabled := True;
!!}
    property Gestures: TIEViewerGestures read fGestures;  // not: cannot stay in published section without an property editor
    
  published

    property LegacyBitmap: boolean read fLegacyBitmap write SetLegacyBitmap default true;

{!!
<FS>TImageEnView.DoubleBuffer

<FM>Declaration<FC>
property DoubleBuffer: Boolean;

<FM>Description<FN>
Not used. Always True.
!!}
    property DoubleBuffer: boolean read fDoubleBuffer write fDoubleBuffer default true; // not used

    property ZoomFilter: TResampleFilter read fZoomFilter write SetZoomFilter default rfNone;
    property ScrollBars: TScrollStyle read fScrollBars write SetScrollBars default ssBoth;

{!!
<FS>TImageEnView.OnViewChange

<FM>Declaration<FC>
property OnViewChange: <A TViewChangeEvent>;

<FM>Description<FN>
Notifies of changes to <A TImageEnView.Zoom>, <A TImageEnView.ViewX> or <A TImageEnView.ViewY>.
!!}
    property OnViewChange: TViewChangeEvent read fOnViewChange write fOnViewChange;

{!!
<FS>TImageEnView.OnImageEnGesture

<FM>Declaration<FC>
property OnImageEnGesture: <A TIEImageEnGestureEvent>;

<FM>Description<FN>
Occurs whenever a gesture is handled by ImageEn.
!!}
    property OnImageEnGesture: TIEImageEnGestureEvent read fOnImageEnGesture write fOnImageEnGesture;

{!!
<FS>TImageEnView.OnFinishSmoothTask

<FM>Declaration<FC>
property OnFinishSmoothTask: <A TIEFinishSmoothTaskEvent>;

<FM>Description<FN>
Notifies that a smooth task has finished (smooth scroll, smooth zoom, etc...).

<FM>See Also<FN>
- <A TImageEnView.SetZoomSmooth>
- <A TImageEnView.SetViewXYSmooth>
!!}
    property OnFinishSmoothTask: TIEFinishSmoothTaskEvent read fOnFinishSmoothTask write fOnFinishSmoothTask;

{!!
<FS>TImageEnView.OnViewChanging

<FM>Declaration<FC>
property OnViewChanging: <A TViewChangeEvent>;

<FM>Description<FN>
Notifies that <A TImageEnView.Zoom>, <A TImageEnView.ViewX> or <A TImageEnView.ViewY> is going to change.
!!}
    property OnViewChanging: TViewChangingEvent read fOnViewChanging write fOnViewChanging;


{!!
<FS>TImageEnView.OnImageChange

<FM>Declaration<FC>
property OnImageChange: TNotifyEvent;

<FM>Description<FN>
The OnImageChange event is called after the image is modified using <A TImageEnProc> (<A TImageEnView.Proc> property) or <A TImageEnIO> (<A TImageEnView.IO> property).

Note: <A TImageEnProc> calls <A TImageEnView.ImageChange> after a modification which actuates OnImageChange.
!!}
    property OnImageChange: TNotifyEvent read fOnImageChange write fOnImageChange;

    property SelectionOptions: TIESelectionOptions read fSelectionOptions write SetSelectionOptions default [iesoAnimated, iesoSizeable, iesoMoveable, iesoCanScroll];
    property Center: boolean read GetCenter write SetCenter default true;
    property MouseInteract: TIEMouseInteract read GetMouseInteract write SetMouseInteract default [];

{!!
<FS>TImageEnView.AutoFit

<FM>Declaration<FC>
property AutoFit: Boolean;

<FM>Description<FN>
If AutoFit is True and the image is updated (<A TImageEnView.Update> method) then <A TImageEnView.Fit> is automatically called to make the image fit within the display area (stretch if it is smaller than the window, shrink if it is larger).

This has the same effect as setting both <A TImageEnView.AutoStretch> and <A TImageEnView.AutoShrink> to true.
!!}
    property AutoFit: boolean read fAutoFit write fAutoFit default false;

{!!
<FS>TImageEnView.SelectionBase

<FM>Declaration<FC>
property SelectionBase: <A TIESelectionBase>;

<FM>Description<FN>
Specifies the meaning of coordinates used when creating a selection.
If SelectionBase is iesbClientArea (default) then all coordinates depend upon the actual zoom and window view/scrolling (i.e. pixels on screen). Otherwise, if SelectionBase is iesbBitmap, then all coordinates refer to bitmap pixels.

Specify this before creating a selection. It has no effect on existing selections.

<FM>Examples<FC>
// Select a 100 pixel square in the top left corner of the bitmap
ImageEnView1.SelectionBase := iesbBitmap;
ImageEnView1.Select(0, 0, 100, 100);

// Select a 100 pixel square that is displayed in the top left corner of the ImageEnView window (100 x 100 pixels of screen area)
ImageEnView1.SelectionBase := iesbClientArea;
ImageEnView1.Select(0, 0, 100, 100);

Note: When ImageEnView1.Zoom = 100, ImageEnView1.ViewX = 0 and ImageEnView1.ViewY = 0, then both the preceeding examples will provide the same result

// Select 20% of the center of the screen
const
  Selection_Size = 0.2; // 20% of screen area
begin
  ImageEnView1.SelectionBase := iesbClientArea;
  ImageEnView1.Select(Trunc((0.5 - Selection_Size / 2) * ImageEnView1.ClientWidth),
                      Trunc((0.5 - Selection_Size / 2) * ImageEnView1.ClientHeight),
                      Trunc((0.5 + Selection_Size / 2) * ImageEnView1.ClientWidth),
                      Trunc((0.5 + Selection_Size / 2) * ImageEnView1.ClientHeight));
end;

<FM>See Also<FN>
- <A TImageEnView.Select>
- <A TImageEnView.XBmp2Scr>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.YBmp2Scr>
- <A TImageEnView.YScr2Bmp>
!!}
    property SelectionBase: TIESelectionBase read fSelectionBase write fSelectionBase default iesbClientArea;

    property BackgroundStyle: TIEBackgroundStyle read fBackgroundStyle write SetBackgroundStyle default iebsSolid;

{!!
<FS>TImageEnView.OnSelectionChange

<FM>Declaration<FC>
property OnSelectionChange: TNotifyEvent;

<FM>Description<FN>
The OnSelectionChange event is called whenever the user finishes a change of the current selection.
Methods such as <A TImageEnView.AddSelPoint> or <A TImageEnView.Select> do not call the OnSelectionChange event.

<FM>Example<FC>
// Real time copy of user selected region to ImageEnView2 component
ImageEnView2.Clear;
ImageEnView2.CopyFromPolygon(ImageEnView2.Bitmap, ImageEnView2.PolySelPoints^, ImageEnView2.PolySelCount, Point(0, 0));

// An enhancement of above example: Display the current user selection as zoomed in ImageEnView2
ImageEnView2.Clear;
ImageEnView2.CopyFromPolygon(ImageEnView2.Bitmap, ImageEnView2.PolySelPoints^, ImageEnView2.PolySelCount, Point(0, 0));
ImageEnView2.Zoom := 200;
!!}
    property OnSelectionChange: TNotifyEvent read fOnSelectionChange write fOnSelectionChange;

{!!
<FS>TImageEnView.OnSelectionChanging

<FM>Declaration<FC>
property OnSelectionChanging: TNotifyEvent;

<FM>Description<FN>
OnSelectionChanging is called whenever while the user is modifying the current selection.
Methods such as <A TImageEnView.AddSelPoint> or <A TImageEnView.Select> do not call the OnSelectionChanging event.
!!}
    property OnSelectionChanging: TNotifyEvent read fOnSelectionChanging write fOnSelectionChanging;

{!!
<FS>TImageEnView.OnMouseInSel

<FM>Declaration<FC>
property OnMouseInSel: TNotifyEvent;

<FM>Description<FN>
OnMouseInSel is called whenever the mouse is inside a selection or over its contours.
!!}
    property OnMouseInSel: TNotifyEvent read fOnMouseInSel write fOnMouseInSel;

{!!
<FS>TImageEnView.DelayZoomFilter

<FM>Declaration<FC>
property DelayZoomFilter: Boolean

<FM>Description<FN>
If DelayZoomFilter is True, the zoom filter (which enhances the display quality is only applied after a delay.
This allows you to quickly navigate the image (zoom and scroll) and once the navigation has finished the image will be displayed at best quality.
!!}
    property DelayZoomFilter: boolean read fDelayZoomFilter write fDelayZoomFilter default false;

{!!
<FS>TImageEnView.OnPaint

<FM>Declaration<FC>
property OnPaint: TNotifyEvent;

<FM>Description<FN>

OnPaint occurs whenever the component is redrawn. Use OnPaint to perform special processing when the component is redrawn.
!!}
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;

    property EnableAlphaChannel: boolean read fEnableAlphaChannel write SetEnableAlphaChannel default true;

{!!
<FS>TImageEnView.OnMouseInResizingGrip

<FM>Declaration<FC>
property OnMouseInResizingGrip: <A TIEMouseInResizingGripEvent>;

<FM>Description<FN>
OnMouseInResizingGrip is called whenever the mouse moves over a grip and tells you which resizing grip is under the mouse position.
!!}
    property OnMouseInResizingGrip: TIEMouseInResizingGripEvent read fOnMouseInResizingGrip write fOnMouseInResizingGrip;

{!!
<FS>TImageEnView.OnZoomIn

<FM>Declaration<FC>
property OnZoomIn: <A TIEZoomEvent>;

<FM>Description<FN>
An OnZoomIn event occurs whenever the user zooms into the current image (increases its display size).
You can programatically customize the zoom value by changing the <FC>NewZoom<FN> parameter.

<FM>Example<FC>
// Max zoom-in of 50%
Procedure Tform1.ImageEnView1ZoomIn(Sender: TObject; var NewZoom: Double);
Begin
  if NewZoom < 50 then
  NewZoom := 50;
End;
!!}
    property OnZoomIn: TIEZoomEvent read fOnZoomIn write fOnZoomIn;

{!!
<FS>TImageEnView.OnZoomOut

<FM>Declaration<FC>
property OnZoomOut: <A TIEZoomEvent>;

<FM>Description<FN>
An OnZoomOut event occurs whenever the user zooms out of the current image (reduces its display size).
You can programatically customize the zoom value by changing the <FC>NewZoom<FN> parameter.

<FM>Example<FC>
// Max zoom-out to 200%
Procedure Tform1.ImageEnView1ZoomOut(Sender: TObject; var NewZoom: Double);
Begin
  if NewZoom > 200 then
  NewZoom := 200;
End;
!!}
    property OnZoomOut: TIEZoomEvent read fOnZoomOut write fOnZoomOut;

{!!
<FS>TImageEnView.OnProgress

<FM>Declaration<FC>
property OnProgress: <A TIEProgressEvent>;

<FM>Description<FN>
The OnProgress event is called when image processing or input/output operations are executed. If you are using it to update a progress bar then you can reset it in the <A TImageEnView.OnFinishWork> event.
!!}
    property OnProgress: TIEProgressEvent read GetOnProgress write SetOnProgress;

    property OnFinishWork: TNotifyEvent read GetOnFinishWork write SetOnFinishWork;

    property OnAcquireBitmap: TIEAcquireBitmapEvent read GetOnAcquireBitmap write SetOnAcquireBitmap;


{!!
<FS>TImageEnView.AutoStretch

<FM>Declaration<FC>
property AutoStretch: Boolean;

<FM>Description<FN>
When AutoStretch is True TImageEnView follows these rules:
- If an image is bigger than TImageEnView window it is displayed 100%;
- If an image is smaller than TImageEnView window it is stretched to fit

<FM>See Also<FN>
- <A TImageEnView.AutoShrink>
- <A TImageEnView.AutoFit>
!!}
    property AutoStretch: boolean read fAutoStretch write fAutoStretch default false;

{!!
<FS>TImageEnView.AutoShrink

<FM>Declaration<FC>
property AutoShrink: Boolean;

<FM>Description<FN>
When AutoShrink is True TImageEnView follows these rules:
- If an image is bigger than TImageEnView window it is Shrink to fit;
- If an image is smaller than TImageEnView window it is displayed 100%

<FM>See Also<FN>
- <A TImageEnView.AutoStretch>
- <A TImageEnView.AutoFit>
!!}
    property AutoShrink: boolean read fAutoShrink write fAutoShrink default false;

{!!
<FS>TImageEnView.OnBeforeSelectionChange

<FM>Declaration<FC>
property OnBeforeSelectionChange: TNotifyEvent;

<FM>Description<FN>
OnBeforeSelectionChange occurs prior to the change of a selection.
!!}
    property OnBeforeSelectionChange: TNotifyEvent read fOnBeforeSelectionChange write fOnBeforeSelectionChange;

    property DrawVersion: boolean read fDrawVersion write SetDrawVersion default false;

{!!
<FS>TImageEnView.OnDrawBackBuffer

<FM>Declaration<FC>
property OnDrawBackBuffer: TNotifyEvent;

<FM>Description<FN>
OnDrawBackBuffer provides access to the back buffer where ImageEn will draw the image (and layers) just before the paint event.
You can draw on the <A TImageEnView.BackBuffer> by handling the OnDrawBackBuffer event to paint onto the Backbuffer canvas.
<A TImageEnView.BackBuffer> is updated whenever <A TImageEnView.Update> is called.

<FM>Example<FC>
Procedure Form1OnDrawBackBuffer(Sender: TObject);
Begin
  // Draw a red line on the back buffer
  With ImageEnView1.BackBuffer.Canvas do
  begin
    Pen.Color := clRed;
    MoveTo( 0, 0 );
    LineTo( 100, 100 );
  End;
End;
!!}
    property OnDrawBackBuffer: TNotifyEvent read fOnDrawBackBuffer write fOnDrawBackBuffer;

{!!
<FS>TImageEnView.OnLayerNotify

<FM>Declaration<FC>
property OnLayerNotify: <A TIELayerNotify>;

<FM>Description<FN>
The OnLayerNotify event occurs whenever a layer is selected, moved or resized.

This event only fires on user actions (not programatic changes).
!!}
    property OnLayerNotify: TIELayerNotify read fOnLayerNotify write fOnLayerNotify;

{!!
<FS>TImageEnView.OnSpecialKey

<FM>Declaration<FC>
property OnSpecialKey: <A TIESpecialKeyEvent>;

<FM>Description<FN>
OnSpecialKey is called whenever a special key is pressed. Special keys are arrows, "Home", "End", etc.

Note: If you have problems receiving OnSpecialKey events, it is recommended that you place the TImageEnView (or its inherited components) on a TPanel instead of TForm.
!!}
    property OnSpecialKey: TIESpecialKeyEvent read fOnSpecialKey write fOnSpecialKey;

    property ImageEnVersion: String read GetImageEnVersion write SetImageEnVersion stored false;

{!!
<FS>TImageEnView.OnDrawBackground

<FM>Declaration<FC>
property OnDrawBackground: <A TIEOnDrawBackground>;

<FM>Description<FN>
OnDrawBackground occurs whenever background needs to be painted.
!!}
    property OnDrawBackground: TIEOnDrawBackground read fOnDrawBackground write fOnDrawBackground;

{!!
<FS>TImageEnView.OnDrawCanvas

<FM>Declaration<FC>
property OnDrawCanvas: <A TIEOnDrawCanvas>;

<FM>Description<FN>
OnDrawCanvas occurs whenever the component canvas is updated.
!!}
    property OnDrawCanvas: TIEOnDrawCanvas read fOnDrawCanvas write fOnDrawCanvas;

    property WallPaper: TPicture read fWallPaper write SetWallPaper;
    property WallPaperStyle: TIEWallPaperStyle read fWallPaperStyle write SetWallPaperStyle default iewoNormal;

    property OnTransitionStop: TNotifyEvent read GetOnTransitionStop write SetOnTransitionStop;
    property OnTransitionStep: TIETransitionStepEvent read GetOnTransitionStep write SetOnTransitionStep;
    property OnTransitionPaint: TIEOnTransitionPaint read GetOnTransitionPaint write SetOnTransitionPaint;

{!!
<FS>TImageEnView.OnDrawLayerBox

<FM>Declaration<FC>
property OnDrawLayerBox: <A TIEDrawLayerBoxEvent>;

<FM>Description<FN>
Occurs when a layer box must be painted. If you handle this event the default behavior is disabled.
<FC>ABitmap<FN> is the destination bitmap and <FC>layer<FN> is the layer index that we are drawing.
Use <A TImageEnView.Layers>[].<A TIELayer.ClientAreaBox> rectangle to get actual rectangle coordinates.

See the ImageEditing\Layers demo for more details.

<FM>Example<FC>
procedure Tfmain.ImageEnView1DrawLayerBox(Sender: TObject;
  ABitmap: TBitmap; layer: Integer);
begin
  // a green line
  with ABitmap.Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 2;
    Pen.mode := pmCopy;
    Pen.Color := clGreen;
    Brush.Style := bsClear;
    with TIELayer(ImageEnView1.Layers[layer]).ClientAreaBox do
      Rectangle(Left-1, Top-1, Right+1, Bottom+1);
  end;
end;
!!}
    property OnDrawLayerBox: TIEDrawLayerBoxEvent read fOnDrawLayerBox write fOnDrawLayerBox;


{!!
<FS>TImageEnView.OnDrawLayer

<FM>Declaration<FC>
property OnDrawLayer: <A TIEDrawLayerEvent>;

<FM>Description<FN>
Occurs immediately after a layer is painted.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Dest<FN></C> <C>The destination bitmap (usually the back buffer)</C> </R>
<R> <C><FC>LayerIndex<FN></C> <C>The layer index that we are drawing</C> </R>
</TABLE>

Use <A TImageEnView.Layers>[].<A TIELayer.ClientAreaBox> rectangle to know actual rectangle coordinates.
!!}
    property OnDrawLayer: TIEDrawLayerEvent read fOnDrawLayer write fOnDrawLayer;


{!!
<FS>TImageEnView.OnDrawLayerGrip

<FM>Declaration<FC>
property OnDrawLayerGrip: <A TIEDrawLayerGrip>;

<FM>Description<FN>
Occurs when a layer grip is painted. If you handle this event the default behavior is disabled.
<FC>ABitmap<FN> is the destination bitmap and layer is the layer index that we are drawing.
<FC>Rect<FN> specifies the actual grip rectangle.

See the ImageEditing\Layers demo for more details.

<FM>Example<FC>
procedure Tfmain.ImageEnView1DrawLayerGrip(Sender: TObject;
  ABitmap: TBitmap; layer, grip: Integer; rect: TRect);
begin
  with ABitmap.Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    Pen.Color := clGreen;
    Brush.Style := bsSolid;
    Brush.Color := $0000FF00;
    with rect do
      Ellipse(Left-1, Top-1, Right+1, Bottom+1);
  end;
end;
!!}
    property OnDrawLayerGrip: TIEDrawLayerGrip read fOnDrawLayerGrip write fOnDrawLayerGrip;


{!!
<FS>TImageEnView.OnDrawPolygon

<FM>Declaration<FC>
property OnDrawPolygon: <A TIEOnDrawPolygon>;

<FM>Description<FN>
OnDrawPolygon occurs whenever a polygon is painted.
!!}
    property OnDrawPolygon: TIEOnDrawPolygon read fOnDrawPolygon write fOnDrawPolygon;

    property OnSaveUndo: TIESaveUndoEvent read GetOnSaveUndo write SetOnSaveUndo;
{$IFDEF IESUPPORTANCHORS}
    property Anchors;
{$ENDIF}
{$IFDEF IEMOUSEWHEELEVENTS}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$ENDIF}
    property DragCursor;

{$IFDEF IEINCLUDEDIRECTSHOW}

{!!
<FS>TImageEnView.OnDShowNewFrame

<FM>Declaration<FC>
property OnDShowNewFrame: TNotifyEvent;

<FM>Description<FN>
OnDShowNewFrame occurs when a new frame is ready (when using the DirectShow functionality of TImageEnView). This event is active when you are using IO.<A TImageEnIO.DShowParams> and <A TIEDirectShow.EnableSampleGrabber> is True.

<FM>Example<FC>
procedure Tfmain.ImageEnView1DShowNewFrame(Sender: TObject);
begin
   // copy current sample to ImageEnView bitmap
   ImageEnView1.IO.DShowParams.GetSample(ImageEnView1.IEBitmap);
   // refresh ImageEnView1
   ImageEnView1.Update;
end;
!!}
    property OnDShowNewFrame: TNotifyEvent read fOnDShowNewFrame write fOnDShowNewFrame;

{!!
<FS>TImageEnView.OnDShowEvent

<FM>Declaration<FC>
property OnDShowEvent: TNotifyEvent;

<FM>Description<FN>
OnDShowEvent occurs when one or more events are ready (when using the DirectShow functionality of TImageEnView).
You should call <A TImageEnView.IO>.<A TImageEnIO.DShowParams>.<A TIEDirectShow.GetEventCode> until it returns false (no more events are available).

<FM>Example<FC>
procedure Tfmain.ImageEnView1DShowEvent;
var
  event: Integer;
begin
  while ImageEnView1.IO.DShowParams.GetEventCode(event) do
    case event of
      IEEC_COMPLETE:
        begin
        ... end of stream!
        end;
    end;
end;
!!}
    property OnDShowEvent: TNotifyEvent read fOnDShowEvent write fOnDShowEvent;

{$ENDIF}


{!!
<FS>TImageEnView.OnSetCursor

<FM>Declaration<FC>
property OnSetCursor: <A TIESetCursorEvent>;

<FM>Description<FN>
OnSetCursor occurs whenever the mouse cursor needs to be changed. You can set your custom cursor by setting the <FC>Cursor<FN> parameter.
This event may be called multiple times for the same cursor shape.
!!}
    property OnSetCursor: TIESetCursorEvent read fOnSetCursor write fOnSetCursor;



{$IFDEF IEINCLUDEMEDIAFOUNDATION}

{!!
<FS>TImageEnView.OnMediaFoundationNotify

<FM>Declaration<FC>
property OnMediaFoundationNotify: <A TIEMediaFoundationNotifyEvent>;

<FM>Description<FN>
Occurs whenever Media Foundation sends a new frame or a notification to TImageEnView.

<FM>Example<FC>
// handler for TImageEnView.OnMediaFoundatioNotify event
procedure TForm1.ImageEnVect1MediaFoundationNotify(Sender, MediaFoundationObject: TObject; NotifyType: TIEMediaFountationNotifyType);
var
  sample: TIEMFReceivedSample;
begin
  if NotifyType = iemfnFRAME then // is this a frame?
  begin
    sample := ImageEnView1.IO.MediaFoundationSourceReader.GetNextSample();  // retrieve frame sample
    try
      sample.DecodeSample(ImageEnView1.IEBitmap); // convert frame sample to bitmap
      ImageEnView1.Update();                      // update TImageEnView to show the new bitmap
    finally
      sample.Free();                              // free the sample
    end;
  end;
end;
!!}
    property OnMediaFoundationNotify: TIEMediaFoundationNotifyEvent read fOnMediaFoundationNotify write fOnMediaFoundationNotify;

{$ENDIF}

{!!
<FS>TImageEnView.OnVirtualKey

<FM>Declaration<FC>
property OnVirtualKey: <A TIEVirtualKeyEvent>;

<FM>Description<FN>
Occurs whenever the component receives WM_KEYDOWN, WM_SYSKEYDOWN, WM_KEYUP or WM_SYSKEYUP message.
!!}
    property OnVirtualKey: TIEVirtualKeyEvent read fOnVirtualKey write fOnVirtualKey;

{!!
<FS>TImageEnView.EnableInteractionHints

<FM>Declaration<FC>
property EnableInteractionHints: Boolean;

<FM>Description<FN>
When true (default) mouse interaction hints are displayed to the user (rotation angle of a layer, position when moving, etc.).
!!}
    property EnableInteractionHints: Boolean read fEnableInteractionHints write fEnableInteractionHints;

    
    // play
    property Playing: boolean read fPlaying write SetPlaying default false;

{!!
<FS>TImageEnView.PlayLoop

<FM>Declaration<FC>
property PlayLoop: boolean;

<FM>Description<FN>
Set PlayLoop to True to continuously loop playback of animated GIF and AVI files (when <A TImageEnView.Playing> is enabled).

!!}
    property PlayLoop: boolean read fPlayLoop write fPlayLoop default true;


    property Cursor: TCursor read fCursor write SetCursor default 1785;
    {$ifdef IEINCLUDEFLATSB}
    property FlatScrollBars: Boolean read fFlatScrollBars write SetFlatScrollBars default False;
    {$endif}
    property Align;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnContextPopup;
    {$ifdef IEHASONGESTURE}
    property OnGesture;
    {$endif}
  end;

// TImageEnView
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


function IETryLYR(Stream: TStream): Boolean;

function _IsRectangle(p: PPointArray; n: integer): boolean;

procedure IEDrawBackground(ComponentState: TComponentState; Canvas: TCanvas; Bitmap: TBitmap; fBackgroundStyle: TIEBackgroundStyle; fBackGround: TColor; DestX, DestY, Width, Height: integer; x1, y1, x2, y2: integer; fChessboardSize: integer; fChessboardBrushStyle: TBrushStyle; fGradientEndColor: TColor; WallPaper: TPicture; WallPaperStyle: TIEWallPaperStyle);

procedure IELayersMerge(Layer1, Layer2, LayerMask: TIELayer; Destination: TIEBitmap; ZoomFilter: TResampleFilter; Background: TColor; RelativePositions: Boolean);



const
  // consts used by <A TImageEnView.LayersMove>
  IEN_Send_To_Back    = -9;
  IEN_Send_Backward   = -8;
  IEN_Bring_Forward   = -7;
  IEN_Bring_To_Front  = -6;
  

procedure IEInitialize_imageenview;
procedure IEFinalize_imageenview;


implementation


uses math, NeurQuant, ievect, iegdiplus, iesettings
{$ifdef IEUSEVCLZLIB}, zlib{$else}, iezlib{$endif}
{$ifdef IEINCLUDEDIRECTSHOW}
  , ieds
{$endif}
{$ifdef IEINCLUDEFLATSB}
  , flatsb
{$endif}
{$IFDEF IEINCLUDEMEDIAFOUNDATION}
  , iemmf
{$ENDIF}
{$ifdef THEMED_BORDERS}
  , Themes
{$endif}
{$ifdef IEVISION}
  , ievision
{$endif}
{$ifdef IEHASTYPES}
  , Types
{$endif}
  ;

{$R IMRES.RES}

{$R-}



type
  // animated polygons item
  // note: this item is replicated in ievect also
  TIEAnimPoly = record
    Poly: PPointArray; // coordinates
    PolyCount: integer; // vertex count
    PolyCapacity: integer; // memory capacity
    Color1: TColor; // Color 1
    Color2: TColor; // Color 2
    //
    Animated: boolean; // Animated
    AniFt: integer; // frame counter
    C1: integer; // DDA counter
    Canvas: TCanvas; // destination canvas
    RX1, RY1, RX2, RY2: integer; // bounds of the polygon
    Enabled: boolean; // if True show the polygon
    Sizeable: boolean; // shows and use resizing grips
    DrawPixelPtr: PRGB; // to replace SetPixel
    DrawPixelBitmap: TBitmap;  // to replace SetPixel
  end;

  PIEAnimPoly = ^TIEAnimPoly;


// Initialize (load) cursors
procedure InitCursors;
const
  imcur = 1777;
  curnum = 37;
  curname: array[1..curnum] of string = (
    'ZOOMMENO', // 1778
    'ZOOMPIU', // 1779
    'CROCE', // 1780
    'GOMMA', // 1781
    'MANO', // 1782
    'PENNA', // 1783
    'CROCE1', // 1784
    'CROCE3', // 1785   // default
    'MOVE2', // 1786
    'MOVEY', // 1787
    'MOVE1', // 1788
    'MOVEX', // 1789
    'MOVE', // 1790
    'CROCE3+', // 1791
    'CROCE3-', // 1792
    'CROCE2', // 1793
    'CROCE2+', // 1794
    'CROCE2-', // 1795
    'CROCE2-', // 1796  (FREE ENTRY)
    '1795', // 1797
    '1796', // 1798
    '1797', // 1799
    '1798', // 1800
    '1799', // 1801
    '1800', // 1802
    '1801', // 1803
    '1802', // 1804
    '1803', // 1805
    '1803', // 1806 (FREE ENTRY)
    '1805', // 1807
    '1806', // 1808
    '1807', // 1809
    '1808', // 1810
    'ROT1', // 1811
    'ROT2', // 1812
    'ROT3', // 1813
    'ROT4'  // 1814
    );
var
  q: integer;
begin
  for q := 1 to curnum do
    Screen.Cursors[imcur + q] := LoadCursor(sysinit.hinstance, PChar(curname[q]));
end;

/////////////////////////////////////////////////////////////////////////////////////

constructor TImageEnView.Create(Owner: TComponent);
begin
  LockUpdate;
  fOffscreenPaint := false;
  fGXScr2Bmp := nil;
  fGYScr2Bmp := nil;
  fGXScr2BmpSize := 0;
  fGYScr2BmpSize := 0;
  fGXBmp2Scr := nil;
  fGYBmp2Scr := nil;
  fXScr2BmpSize := 0;
  fYScr2BmpSize := 0;
  fXBmp2ScrSize := 0;
  fYBmp2ScrSize := 0;
  fXScr2Bmp := nil;
  fYScr2Bmp := nil;
  fXBmp2Scr := nil;
  fYBmp2Scr := nil;
  fBitmapWidth := 0;
  fBitmapHeight := 0;
  fUpdateReason := ieurDefault;
  fWasScreenCursor := crNone;

  fImageEnIO := nil;
  fImageEnProc := nil;
  fLegacyBitmap := true;
  fBitmap := TBitmap.create;
  fBitmap.PixelFormat := pf24bit;
{$IFNDEF OCXVERSION}
  fBitmap.Width := Width;
  fBitmap.Height := Height;
{$ENDIF}

  inherited Create(Owner);

  {$ifdef IEVISION}
  if (csDesigning in ComponentState) then
  begin
    // design mode, unload ievision
    IEFinalize_ievision();
  end;
  {$endif}

  IEGDIPLoadLibrary();

  fIEBitmap := TIEBitmap.Create;
  fIEBitmap.EncapsulateTBitmap(fBitmap, true);
  fMaxLayerWidth := fIEBitmap.Width;
  fMaxLayerHeight := fIEBitmap.Height;
  fUpdateInsideUpdate := false;
  fInsideUpdate := false;
  // layers
  fLayers := TList.Create;
  fLayers.Add(TIELayer.Create(self, fIEBitmap, true));
  with TIELayer(fLayers[0]) do
  begin
    VisibleBox := false;
    Locked := true;
  end;
  fLayersCurrent := 0;

  fDrawPixelBitmap := nil;
  fDrawPixelPtr := nil;

  fOnSetCursor := nil;

  fOldHandle := 0;
  fAutoCursors := assigned(Owner);
  fTransition := nil;
  fTransitionEffect := iettNone;
  fTransitionDuration := 1000;
  fVScrollBarVisible := false;
  fHScrollBarVisible := false;
  fScrollBarsAlwaysVisible := false;
  fRXScroll := 1;
  fRYScroll := 1;
  fSelectionOptions := [iesoAnimated, iesoSizeable, iesoMoveable, iesoCanScroll];
  fOnViewChange := nil;
  fOnFinishSmoothTask := nil;
  fOnViewChanging := nil;
  fOnImageEnGesture := nil;
  fOnImageChange := nil;
  fOnDrawLayer := nil;
  fOnDrawLayerBox := nil;
  fOnDrawLayerGrip := nil;
  fDelayZoomFilter := false;
  fZoomFilter := rfNone;
  fDBToDraw := true;
  fFullUpdateRequest := true;
  ZeroMemory(@fBitmapInfoHeader256, sizeof(TBitmapInfoHeader256));
  fMouseInteract := [];
  fLockPaint := 0;
  fHDrawDib := IEDrawDibOpen;
  {$ifndef IEDOTNETVERSION}
  if Owner <> nil then
  {$endif}
    InitCursors;
  fFirstTimeSetCursor := true;
  fCursor :=  1785;
  fLCursor := fCursor;
  fScrollBars := ssBoth;
  fZoomX := 100;
  fZoomY := 100;
  fZoomD100X := fZoomX / 100;
  f100DZoomX := 100 / fZoomX;
  fZoomD100Y := fZoomY / 100;
  f100DZoomY := 100 / fZoomY;
  Height := 105;
  Width := 105;
  fOffX := 0;
  fOffY := 0;
  fExtX := 0;
  fExtY := 0;
  fImageHorizAlignment := iehCenter;
  fImageVertAlignment  := ievCenter;
  fDoubleBuffer := true;
  SetDPI(IEGlobalSettings().DefaultDPIX, IEGlobalSettings().DefaultDPIY);
  fPolySelecting := False;
  fLassoSelecting := False;
  fRectSelecting := False;
  fCircSelecting := False;
  fRectResizing := ieNone;
  fSelectMoving := -1;
  fSel := false;
  fAnimPoly := TList.Create;
  fAnimPolyTimer := nil;
  fDelayTimer := -20; // maximum 20% of cpu for selections
  fHPolySel := PIEAnimPoly(AnimPolygonNew(clBlack, clWhite, True, True));
  if not (csDesigning in ComponentState) then
  begin
    fTBitmap := TIEBitmap.Create(10, 10, ie24RGB); // back buffer
    fTBitmap.Location := ieTBitmap;
    HideSelectionEx(false);
  end;
  fUpdateInvalidate := true;
  fSelectionBase := iesbClientArea;
  fSavedSelectionBase := fSelectionBase;
  fBackgroundStyle := iebsSolid;
  fOnSelectionChange := nil;
  fOnSelectionChanging := nil;
  fOnBeforeSelectionChange := nil;
  fMagicWandMaxFilter := false;
  fMagicWandMode := iewInclusive;
  fMagicWandTolerance := 15;
  fOnMouseInSel := nil;
  fOnMouseInResizingGrip := nil;
  fOnZoomIn := nil;
  fOnZoomOut := nil;
  fOnVirtualKey := nil;
  fDisplayGrid := false;
  fDisplayGridLyr := -1;
  fVScrollBarParams := TIEScrollBarParams.Create;
  fHScrollBarParams := TIEScrollBarParams.Create;
  fMouseWheelParams := TIEMouseWheelParams.Create(iemwZoom);
  fSelectionMaskDepth := 1;
  fSelectionIntensity := 1;
  fSelectionMask := TIEMask.Create;
  fSelectionMask.AllocateBits(fIEBitmap.Width, fIEBitmap.Height, fSelectionMaskDepth);
  fAniCounter := 0;
  SelColor1 := clBlack;
  SelColor2 := clWhite;

  fGripColor1 := clBlack;
  fGripColor2 := $00BAFFFF;
  fGripBrushStyle := bsSolid;
  fGripSize := 5;
  fGripShape := iegsCircle;
  fExtendedGrips := true;

  fLyrGripColor1 := clBlack;
  fLyrGripColor2 := $00BAFFFF;
  fLyrGripBrushStyle := bsSolid;
  fLyrGripSize := 5;
  fLyrGripShape := iegsCircle;

  fChessboardSize := 16;
  fChessboardBrushStyle := bsSolid;
  fOnPaint := nil;
  fOnProgress := nil;
  fOnFinishWork := nil;
  fOnAcquireBitmap := nil;
  fForceALTkey := false;
  fEnableAlphaChannel := true;
  fDelayDisplaySelection := true;
  fAutoStretch := false;
  fAutoShrink := false;
  fBlockPaint := false;
  fRectMoving := false;
  fMouseMoveScrolling := false;
  fSavedSelection := TList.Create;
  fEnableShiftKey := true;
  fStableReset := 4;
  fStable2Reset := 2;
  fLutLastZoomX := -1;
  fLutLastZoomY := -1;
  fLutLastFRX := -1;
  fLutLastFRY := -1;
  fLutLastMaxLayerWidth := -1;
  fLutLastMaxLayerHeight := -1;
  fGradientEndColor := clBlue;
  fUseDrawDibDraw := false; // for same behavior of old versions set it to True
  fDrawVersion := false;
  UnLockUpdate;
  fOnDrawBackBuffer := nil;
  fLayersSync := false;
  fLayersDrawBox := false;
  fMovingLayer := -1;
  fLayerMoved := false;
  fRotatingLayer := -1;
  fRotatingLayerValue := 0;
  fMovingRotationCenter := -1;
  fLayerResizing := ieNone;
  fMovResRotLayerStarted := false;
  fOnLayerNotify := nil;
  {$IFDEF IEINCLUDEDIRECTSHOW}
  fOnDShowNewFrame := nil;
  fOnDShowEvent := nil;
  {$ENDIF}
  {$IFDEF IEINCLUDEMEDIAFOUNDATION}
  fOnMediaFoundationNotify := nil;
  {$ENDIF}
  fSelectionAspectRatio := -1;
  fSelectionAbsWidth := 100;
  fSelectionAbsHeight := 100;
  fOnSpecialKey := nil;
  fFlatScrollBars := false;
  fNavigator := nil;
  fNavigatorInside := false;
  fIsNavigator := false;
  fMinBitmapSize := 2;
  fOnDrawBackground := nil;
  fOnDrawCanvas := nil;
  {$ifdef IEDOTNETVERSION} // wallpaper created on request when under DOTNET
    fWallPaper := nil;
  {$else}
    fWallPaper := TPicture.Create;
  {$endif}
  fWallPaperStyle := iewoNormal;
  fZoomSelectionAspectRatio := true;
  fMouseScrollRate := 1;
  fVisibleSelection := true;
  fOnDrawPolygon := nil;
  fOnSaveUndo := nil;
  fSoftCrop := iesfNone;
  fSoftCropValue := 60;
  fInteractionHint := '';
  fEnableInteractionHints := true;
  fLayersRotationFilter := ierFast;
  fLayersRotationAntialias := false;
  fLayersRotationUseFilterOnPreview := false;
  fLayersRotationDelayFilterOnPreview := True;
  fLayersRotateStep := 45;
  fLayersSelectConstrains := true;
  fUpdate_MaskCache := -1;
  fSmoothScrollTimer := nil;
  fSmoothScrollValue := 8;
  fSmoothZoomTimer := nil;
  fSmoothZoomValue := 8;
  fHighlightedPixel := Point(-1, -1);
  fAutoFixRotationBorders := true;
  fPostFrames := TList.Create;
  fSelectionGridWidth := 1;
  fSelectionGridHeight := 1;
  fNavigatorOptions := [];
  fNavigatorBackBuffer := nil;
  fNavigatorActualRect := Rect(0, 0, 0, 0);
  fMarkOuterAlpha := -1;
  fMarkOuterColor := clWhite;
  fLayersAutoUndo := false;
  fLayersResizeAspectRatio := iearALTKey;
  fLayerBoxPen := TPen.Create;

  fLayerBoxPen.Style := psDot;
  fLayerBoxPen.Mode := pmNot;
  fLayerBoxPen.Width := 1;

  fHighlightedPixelColor := clRed;

  fPlayTimer := 0;
  fPlayLoop := true;
  fImageSet := False;

  fGestures := TIEViewerGestures.Create();
end;


procedure TImageEnView.CreateHandle();
begin
  inherited;
end;

destructor TImageEnView.Destroy;
var
  i: integer;
begin
  // free post frames
  while fPostFrames.Count>0 do
    RemovePostFrames(0);
  FreeAndNil(fPostFrames);

  FreeAndNil(fGestures);

  FreeAndNil(fSmoothScrollTimer);
  FreeAndNil(fSmoothZoomTimer);

  DoLayersSync;
  FreeAndNil(fTransition);
  if assigned(fNavigator) then
    SetNavigator(nil);
  if assigned(fImageEnIO) then
    FreeAndNil(fImageEnIO);
  if assigned(fImageEnProc) then
    FreeAndNil(fImageEnProc);
  if not (csDesigning in ComponentState) then
  begin
    // free double buffer
    FreeAndNil(fTBitmap);
  end;
  FreeAndNil(fAnimPolyTimer);

  for i := 0 to fAnimPoly.Count-1 do
  begin
    freemem(PIEAnimPoly(fAnimPoly[i])^.Poly); // free vertex array
    freemem(PIEAnimPoly(fAnimPoly[i]));       // free TIEAnimPoly structure
  end;
  FreeAndNil(fAnimPoly);

  IEDrawDibClose(fHDrawDib);
  FreeAndNil(fVScrollBarParams);
  FreeAndNil(fHScrollBarParams);
  FreeAndNil(fMouseWheelParams);
  FreeAndNil(fSelectionMask);
  // free layers (frees also fIEBitmap and fBitmap)
  for i := 0 to fLayers.Count - 1 do
    TIELayer(fLayers[i]).Free;
  FreeAndNil(fLayers);
  // deleted by previous loop
  fBitmap := nil;
  fIEBitmap := nil;

  for i := 0 to fSavedSelection.Count-1 do
    TObject(fSavedSelection[i]).Free;
  FreeAndNil(fSavedSelection);

  if fGXScr2Bmp <> nil then
    freemem(fGXScr2Bmp);
  if fGYScr2Bmp <> nil then
    freemem(fGYScr2Bmp);
  if fGXBmp2Scr <> nil then
    freemem(fGXBmp2Scr);
  if fGYBmp2Scr <> nil then
    freemem(fGYBmp2Scr);
  fGXScr2Bmp := nil;
  fGYScr2Bmp := nil;
  fGXScr2BmpSize := 0;
  fGYScr2BmpSize := 0;
  fGXBmp2Scr := nil;
  fGYBmp2Scr := nil;
  fXScr2BmpSize := 0;
  fYScr2BmpSize := 0;
  fXBmp2ScrSize := 0;
  fYBmp2ScrSize := 0;
  FreeAndNil(fLayerBoxPen);
  FreeAndNil(fWallPaper);
  FreeAndNil(fDrawPixelBitmap);
  if assigned(fNavigatorBackBuffer) then
    fNavigatorBackBuffer.Free;

  IEGDIPUnLoadLibrary();

  inherited;
end;

procedure TImageEnView.SetupAniPolyTimer;
begin
  if not assigned(fAnimPolyTimer) then
  begin
    fAnimPolyTimer := TTimer.Create(nil);
    fAnimPolyTimer.enabled := false;
    fAnimPolyTimer.Interval := 210;
    fAnimPolyTimer.OnTimer := TimerEvent;
  end;
end;

procedure TImageEnView.SetupTransition;
begin
  if not assigned(fTransition) then
    fTransition := TIETransitionEffects.Create(self);
end;

procedure TImageEnView.SetupDrawPixelBitmap;
begin
  if not assigned(fDrawPixelBitmap) then
  begin
    fDrawPixelBitmap := TBitmap.Create;
    fDrawPixelBitmap.Width  := 1;
    fDrawPixelBitmap.Height := 1;
    fDrawPixelBitmap.PixelFormat := pf24bit;
    fDrawPixelPtr := PRGB(fDrawPixelBitmap.Scanline[0]);
  end;
end;

{!!
<FS>TImageEnView.Background

<FM>Declaration<FC>
property Background: TColor;

<FM>Description<FN>
This property sets the background color, which is shown in the unoccupied area of the window (when the current image is smaller than the ImageEnView).
This color is used also in geometric processing (such as rotation) to fill blank areas.

If you change the background color after the component has been created (at runtime) , you won't see the change because the bitmap overlaps the background.
For this reason, to change background color at runtime you must:

- empty Layer1 bitmap:
  ImageEnView.Blank;
  ImageEnView.Background := clRed;

- or fill the bitmap using the color you want:
  ImageEnView.Proc.Fill(clRed);

See also: <A TImageEnView.BackgroundStyle>

<FM>Example<FC>
// Rotate of 30 degrees and fill blank spaces with clBlack color
ImageEnView1.Background := clBlack;
ImageEnView1.Proc.Rotate(30, False);
!!}
procedure TImageEnView.SetBackGround(cl: TColor);
begin
  inherited SetBackGround(cl);
  if csDesigning in ComponentState then
    Clear;
  UpdateReason := ieurComponentStuffChanged;
  Update;
  UpdateReason := ieurComponentStuffChanged;
  ImageChange;
  if (csDesigning in ComponentState) then
    Paint;
end;

procedure TImageEnView.DoSize;
begin
  if assigned(fTransition) and fTransition.Running then
    fTransition.stop;
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

{!!
<FS>TImageEnView.ScrollBars

<FM>Declaration<FC>
property ScrollBars: TScrollType;

<FM>Description<FN>
ScrollBars determines whether the TImageEnView control displays scroll bars (they are only shown if they are needed unless <A TImageEnView.ScrollBarsAlwaysVisible> is true).

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>ssNone<FN></C> <C>The control has no scroll bars</C> </R>
<R> <C><FC>ssHorizontal<FN></C> <C>The control has a single scroll bar on the bottom edge when needed</C> </R>
<R> <C><FC>ssVertical<FN></C> <C>The control has a single scroll bar on the right edge when needed</C> </R>
<R> <C><FC>ssBoth<FN></C> <C>The control has a scroll bar on both the bottom and right edges when needed</C> </R>
</TABLE>
!!}
procedure TImageEnView.SetScrollBars(v: TScrollStyle);
begin
  fScrollBars := v;
  if (fScrollBars <> ssVertical) and (fScrollBars <> ssBoth) then
    IEShowScrollBar(handle, SB_VERT, false, fFlatScrollBars);
  if (fScrollBars <> ssHorizontal) and (fScrollBars <> ssBoth) then
    IEShowScrollBar(handle, SB_HORZ, false, fFlatScrollBars);
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

{!!
<FS>TImageEnView.Zoom

<FM>Declaration<FC>
property Zoom: double;

<FM>Description<FN>
Use the Zoom property to zoom in or out of the image. The image itself is not modified, only its display size changes.
Zoom is expressed as a percentage of its native size, i.e. 100 is normal size, values less than 100 decrease the display size and values greater than 100 increase the display size.

Note: <FC>Zoom<FN> will have no effect if <A TImageEnView.AutoFit>, <A TImageEnView.AutoShrink> or <A TImageEnView.AutoStretch> are enabled.

<FM>See Also<FN>
- <A TImageEnView.ZoomX>
- <A TImageEnView.ZoomY>
- <A TImageEnView.ZoomIn>
- <A TImageEnView.ZoomOut>

<FM>Example<FC>
// Half size: a 200x200 pixel image is displayed at a size of 100x100
ImageEnView.Zoom := 50;

// Double size: a 200x200 pixel image is displayed at a size of 400x400
ImageEnView.Zoom := 200;

// floating point zoom
ImageEnView.Zoom := 30.25;
!!}
procedure TImageEnView.SetZoomNoUpdate(v: double);
var
  zz: double;
  max_x, max_y: integer;
  x, y: integer;
begin
  StopSmoothScroll();

  zz := v / 100;
  x := GetClientWidth shr 1;
  x := trunc(round((x + fViewX - fOffX) * (f100DZoomX)) * zz - x);
  y := GetClientHeight shr 1;
  y := trunc(round((y + fViewY - fOffY) * (f100DZoomY)) * zz - y);

  fZoomX := v;
  fZoomD100X := fZoomX / 100;
  f100DZoomX := 100 / fZoomX;
  fZoomY := v;
  fZoomD100Y := fZoomY / 100;
  f100DZoomY := 100 / fZoomY;

  // 3.0.3: moved here to center image when scrollbars showed the first time
  GetMaxViewXY(max_x, max_y);
  fViewX := ilimit(x, 0, max_x);
  fViewY := ilimit(y, 0, max_y);
end;


procedure TImageEnView.SetZoom(v: double);
begin
  StopSmoothScroll();
  ViewChanging(2, v);
  if (v > 0) and ((v <> fZoomX) or (v<>fZoomY)) then
  begin
    if (fDelayZoomFilter) and (fZoomFilter <> rfNone) and ((fZoomX <> 100) or (fZoomY<>100)) then
      fStable := fStableReset;
    fStable2 := fStable2Reset;
    LockPaint;
    if assigned(fNavigator) then
      fNavigator.LockPaint;
    SetZoomNoUpdate(v);
    UpdateReason := ieurZoomed;
    Update; // initial update
    UpdateReason := ieurZoomed;
    Update; // final update (without this scroll-bars aren't updated well)
    CalcPaintCoords;
    CreateCoordConvLUT; // recalculates coordinate conversion LUT
    NPUnLockPaint;
    if assigned(fNavigator) then
      fNavigator.NPUnLockPaint;
    Paint;
    ViewChange(1);
  end;
end;

procedure TImageEnView.SetZoomXNoUpdate(v: double);
var
  zz: double;
  max_x, max_y: integer;
  x: integer;
begin
  StopSmoothScroll();
  zz := v / 100;
  x := GetClientWidth shr 1;
  x := trunc(round((x + fViewX - fOffX) * (f100DZoomX)) * zz - x);
  GetMaxViewXY(max_x, max_y);
  fViewX := ilimit(x, 0, max_x);
  fZoomX := v;
  fZoomD100X := fZoomX / 100;
  f100DZoomX := 100 / fZoomX;
end;

{!!
<FS>TImageEnView.ZoomX

<FM>Declaration<FC>
property ZoomX: Double;

<FM>Description<FN>
ZoomX specifies the horizontal Zoom. Using this property (and/or ZoomY) you lose the image aspect ratio and some functions, which require aspect ratio, may not work properly.
Setting ZoomX and ZoomY to the same value is equivalent to setting <A TImageEnView.Zoom>.

<FM>See Also<FN>
- <A TImageEnView.Zoom>
- <A TImageEnView.ZoomY>
!!}                
procedure TImageEnView.SetZoomX(v: double);
begin
  StopSmoothScroll();
  ViewChanging(2, v);
  if (v > 0) and (v <> fZoomX) then
  begin
    if (fDelayZoomFilter) and (fZoomFilter <> rfNone) and (fZoomX <> 100) then
      fStable := fStableReset;
    fStable2 := fStable2Reset;
    LockPaint;
    SetZoomXNoUpdate(v);
    UpdateReason := ieurZoomed;
    Update; // initial update
    UpdateReason := ieurZoomed;
    Update; // final update (without this scroll-bars aren't updated well)
    CalcPaintCoords;
    CreateCoordConvLUT; // recalculates coordinate conversion LUT
    NPUnLockPaint;
    Paint;
    ViewChange(1);
  end;
end;

procedure TImageEnView.SetZoomYNoUpdate(v: double);
var
  zz: double;
  max_x, max_y: integer;
  y: integer;
begin
  StopSmoothScroll();
  zz := v / 100;
  y := GetClientHeight shr 1;
  y := trunc(round((y + fViewY - fOffY) * (f100DZoomY)) * zz - y);
  GetMaxViewXY(max_x, max_y);
  fViewY := ilimit(y, 0, max_y);
  fZoomY := v;
  fZoomD100Y := fZoomY / 100;
  f100DZoomY := 100 / fZoomY;
end;

{!!
<FS>TImageEnView.ZoomY

<FM>Declaration<FC>
property ZoomY: Double;

<FM>Description<FN>
ZoomY specifies the vertical zoom. Using this property (and/or ZoomX) you lose the image aspect ratio and some functions, which require aspect ratio, may not work properly.
Setting <A TImageEnView.ZoomX> and ZoomY to the same value is equivalent to setting <A TImageEnView.Zoom>.

<FM>See Also<FN>
- <A TImageEnView.Zoom>
- <A TImageEnView.ZoomX>
!!}
procedure TImageEnView.SetZoomY(v: double);
begin
  StopSmoothScroll();
  ViewChanging(2, v);
  if (v > 0) and (v<>fZoomY) then
  begin
    if (fDelayZoomFilter) and (fZoomFilter <> rfNone) and (fZoomY<>100) then
      fStable := fStableReset;
    fStable2 := fStable2Reset;
    LockPaint;
    SetZoomYNoUpdate(v);
    UpdateReason := ieurZoomed;
    Update; // initial update
    UpdateReason := ieurZoomed;
    Update; // final update (without this scroll-bars aren't updated well)
    CalcPaintCoords;
    CreateCoordConvLUT; // recalculates coordinate conversion LUT
    NPUnLockPaint;
    Paint;
    ViewChange(1);
  end;
end;



// creates a palette for the current bitmap

type
  tpal = record
    palVersion: word;
    palNumEntries: word;
    PaletteEntry: array[0..255] of TPALETTEENTRY;
  end;
  plogpalette = ^tlogpalette;

function TImageEnView._CreatePalette: HPalette;
var
  pa: tpal;
  pp: array[0..255] of TRGB;
  q: integer;
  qt: TIEQuantizer;
begin
  qt := TIEQuantizer.Create(fIEBitmap, pp, 256);
  for q := 0 to 255 do
  begin
    pa.PaletteEntry[q].peRed := pp[q].r;
    pa.PaletteEntry[q].peGreen := pp[q].g;
    pa.PaletteEntry[q].peBlue := pp[q].b;
    pa.PaletteEntry[q].peFlags := 0;
  end;
  pa.palVersion := $300;
  pa.palNumEntries := 256;
  FreeAndNil(qt);
  result := CreatePalette(plogpalette(@pa)^);
end;

{!!
<FS>TImageEnView.IdealComponentWidth

<FM>Declaration<FC>
property IdealComponentWidth: integer; (Read-only)

<FM>Description<FN>
IdealComponentWidth is the width that the TImageEnView component should be to exactly fit the current image.

See also: <A TImageEnView.IdealComponentHeight>

<FM>Example<FC>
// Resize ImageEnView to fully contain Carlotta.jpg
ImageEnView.LoadFromFile('C:\Carlotta.jpg');
ImageEnView1.Width := ImageEnView1.IdealComponentWidth;
ImageEnView1.Height := ImageEnView1.IdealComponentHeight;

!!}
// ideal horizontal component size
function TImageEnView.GetIdealComponentWidth: integer;
var
  z1: integer;
begin
  z1 := trunc(fZoomX * (fMaxLayerWidth / 100)); // new width of the bitmap
  result := z1 + 1;
  if (fScrollBars = ssHorizontal) or (fScrollBars = ssBoth) then
    inc(result, width - GetClientWidth);
end;

// ideal vertical component size

{!!
<FS>TImageEnView.IdealComponentHeight

<FM>Declaration<FC>
property IdealComponentHeight: integer; (Read-only)

<FM>Description<FN>
IdealComponentHeight is the height that the TImageEnView component should be to exactly fit the current image.

See also: <A TImageEnView.IdealComponentWidth>

<FM>Example<FC>
// Resize ImageEnView to fully contains Carlotta.jpg
ImageEnView.LoadFromFile('C:\Carlotta.jpg');
ImageEnView1.Width := ImageEnView1.IdealComponentWidth;
ImageEnView1.Height := ImageEnView1.IdealComponentHeight;

!!}
function TImageEnView.GetIdealComponentHeight: integer;
var
  z2: integer;
begin
  z2 := trunc(fZoomY * (fMaxLayerHeight / 100)); // new height of the bitmap
  result := z2 + 1;
  if (fScrollBars = ssVertical) or (fScrollBars = ssBoth) then
    inc(result, height - GetClientHeight);
end;

procedure TImageEnView.DoMouseWheelScroll(Delta: integer; mouseX, mouseY: integer; CtrlPressed: boolean);
var
  zDelta, nPos, xTmp: integer;
  dPos: double;
  dir: integer;
  mx, my: Integer;
begin
  if (fMouseWheelParams.Action = iemwNone) or (fAutoFit) then
    exit;
  zDelta := Delta;
  if zDelta > 0 then
    dir := -1
  else
    dir := 1;
  if fMouseWheelParams.InvertDirection then
    dir := -1 * dir;
  case fMouseWheelParams.Action of
    iemwVScroll:
      begin
        GetMaxViewXY(mx, my);
        case fMouseWheelParams.Variation of
          iemwAbsolute: // bitmap based
            if CtrlPressed or ((mx>0) and (my = 0)) then
            begin
              // horizontal
              nPos := fViewX + dir * fMouseWheelParams.Value * imax(trunc(fZoomD100X), 1);
              SetViewX(nPos);
            end
            else
            begin
              // vertical
              nPos := fViewY + dir * fMouseWheelParams.Value * imax(trunc(fZoomD100Y), 1);
              SetViewY(nPos);
            end;
          iemwPercentage: // client based
            if CtrlPressed or ((mx>0) and (my = 0))  then
            begin
              // horizontal
              xTmp := imax(round(GetClientWidth * fMouseWheelParams.Value / 100), 1);
              nPos := fViewX + dir * xTmp;
              SetViewX(nPos);
            end
            else
            begin
              // vertical
              xTmp := imax(round(GetClientHeight * fMouseWheelParams.Value / 100), 1);
              nPos := fViewY + dir * xTmp;
              SetViewY(nPos);
            end;
        end;
      end;
    iemwZoom:
      begin
        dPos := fZoomX;
        case fMouseWheelParams.Variation of
          iemwAbsolute:
            dPos := fZoomX + dir * fMouseWheelParams.Value;
          iemwPercentage:
            dPos := fZoomX + imax(round(fZoomX * fMouseWheelParams.Value / 100), 1) * dir;
        end;
        if (dPos > fZoomX) then
          DoZoomIn(dPos);
        if (dPos < fZoomX) then
          DoZoomOut(dPos);
        if fMouseWheelParams.ZoomPosition = iemwCenter then
          SetZoom(dPos)
        else
          ZoomAt(mouseX, mouseY, dPos, false);
      end;
    iemwNavigate:
      begin
        if dir < 0 then
          IO.Seek(ieioSeekPrior)
        else
          IO.Seek(ieioSeekNext);
      end;
  end;
end;

// Position is valid on when command is iescPosition

procedure TImageEnView.DoVertScroll(command: TIEScrollCommand; Position: integer);
var
  nPos: integer;
  mx, my: integer;
begin
  nPos := 0;
  case command of
    iescPosition:
      nPos := Position;
    iescBottom:
      begin
        GetMaxViewXY(mx, my);
        nPos := my;
      end;
    iescTop:
      nPos := 0;
    iescLineDown:
      if fVScrollBarParams.LineStep = -1 then
        nPos := fViewY + imax(trunc(fZoomD100Y), 1)
      else
        nPos := fViewY + fVScrollBarParams.LineStep * imax(trunc(fZoomD100Y), 1);
    iescLineUp:
      if fVScrollBarParams.LineStep = -1 then
        nPos := fViewY - imax(trunc(fZoomD100Y), 1)
      else
        nPos := fViewY - fVScrollBarParams.LineStep * imax(trunc(fZoomD100Y), 1);
    iescPageDown:
      if fVScrollBarParams.PageStep = -1 then
        nPos := fViewY + GetClientHeight
      else
        nPos := fViewY + fVScrollBarParams.PageStep * imax(trunc(fZoomD100Y), 1);
    iescPageUp:
      if fVScrollBarParams.PageStep = -1 then
        nPos := fViewY - GetClientHeight
      else
        nPos := fViewY - fVScrollBarParams.PageStep * imax(trunc(fZoomD100Y), 1);
  end;
  SetViewY(nPos);
end;

// Position is valid on when command is scPosition

procedure TImageEnView.DoHorizScroll(command: TIEScrollCommand; Position: integer);
var
  nPos: integer;
  mx, my: integer;
begin
  nPos := 0;
  case command of
    iescPosition:
      nPos := Position;
    iescBottom:
      begin
        GetMaxViewXY(mx, my);
        nPos := mx;
      end;
    iescTop:
      nPos := 0;
    iescLineDown:
      if fHScrollBarParams.LineStep = -1 then
        nPos := fViewX + imax(trunc(fZoomD100X), 1)
      else
        nPos := fViewX + fHScrollBarParams.LineStep * imax(trunc(fZoomD100X), 1);
    iescLineUp:
      if fHScrollBarParams.LineStep = -1 then
        nPos := fViewX - imax(trunc(fZoomD100X), 1)
      else
        nPos := fViewX - fHScrollBarParams.LineStep * imax(trunc(fZoomD100X), 1);
    iescPageDown:
      if fHScrollBarParams.PageStep = -1 then
        nPos := fViewX + GetClientWidth
      else
        nPos := fViewX + fHScrollBarParams.PageStep * imax(trunc(fZoomD100X), 1);
    iescPageUp:
      if fHScrollBarParams.PageStep = -1 then
        nPos := fViewX - GetClientWidth
      else
        nPos := fViewX - fHScrollBarParams.PageStep * imax(trunc(fZoomD100X), 1);
  end;
  SetViewX(nPos);
end;

procedure TImageEnView.WMVScroll(var Message: TMessage);
var
  cmd: TIEScrollCommand;
  pos: integer;
begin
  inherited;
  pos := 0;
  case Message.WParamLo of
    SB_THUMBTRACK, SB_THUMBPOSITION:
      begin
        if (not fVScrollBarParams.Tracking) and (Message.WParamLo = SB_THUMBTRACK) then
          exit;
        cmd := iescPosition;
        pos := round(Message.WParamHi * fRYScroll); // 3.0.0 ("round" to avoid scroll with a single click)
      end;
    SB_BOTTOM:    cmd := iescBottom;
    SB_TOP:       cmd := iescTop;
    SB_LINEDOWN:  cmd := iescLineDown;
    SB_LINEUP:    cmd := iescLineUp;
    SB_PAGEDOWN:  cmd := iescPageDown;
    SB_PAGEUP:    cmd := iescPageUp;
  else
    begin
      cmd := iescPosition;
      pos := fViewY;
    end;
  end;
  DoVertScroll(cmd, pos);
end;

procedure TImageEnView.WMHScroll(var Message: TMessage);
var
  cmd: TIEScrollCommand;
  pos: integer;
begin
  inherited;
  pos := 0;
  case Message.WParamLo of
    SB_THUMBTRACK, SB_THUMBPOSITION:
      begin
        if (not fHScrollBarParams.Tracking) and (Message.WParamLo = SB_THUMBTRACK) then
          exit;
        cmd := iescPosition;
        pos := round(Message.WParamHi * fRXScroll);  // 3.0.0 ("round" to avoid scroll with a single click)
      end;
    SB_BOTTOM: cmd := iescBottom;
    SB_TOP: cmd := iescTop;
    SB_LINEDOWN: cmd := iescLineDown;
    SB_LINEUP: cmd := iescLineUp;
    SB_PAGEDOWN: cmd := iescPageDown;
    SB_PAGEUP: cmd := iescPageUp;
  else
    begin
      cmd := iescPosition;
      pos := fViewX;
    end;
  end;
  DoHorizScroll(cmd, pos);
end;

procedure TImageEnView.WMMouseWheel(var Message: TMessage);
var
  pt: TPoint;
begin
  inherited;
  pt.x := smallint(Message.LParamLo);
  pt.y := smallint(Message.LParamHi);
  pt := ScreenToClient(pt);
  DoMouseWheelScroll(smallint($FFFF and (Message.wParam shr 16)), pt.x, pt.y, GetKeyState(VK_CONTROL) < 0);
end;

procedure TImageEnView.WMMouseHWheel(var Message: TMessage);
begin
  inherited;
end;

// double click - end of polygonal selection
procedure TImageEnView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  DoDoubleClickEx((MK_SHIFT and Message.Keys) <> 0);
end;

procedure TImageEnView.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  invalidate;
end;

procedure TImageEnView.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  invalidate;
end;

procedure TImageEnView.WMNCPaint(var Message: TMessage);
{$IFDEF THEMED_BORDERS}
var
  R: TRect;
  Details: TThemedElementDetails;
  DC: HDC;
  XEdge, YEdge: Integer;
{$ENDIF}
begin
  inherited;

{$IFDEF THEMED_BORDERS}
  if ThemeServices.ThemesEnabled and (Ctl3D = False) then
  begin
    R := Rect(0, 0, Width, Height);
    DC := GetWindowDC(Handle);
    XEdge := IEGlobalSettings().BorderX;
    YEdge := IEGlobalSettings().BorderY;
    ExcludeClipRect(DC, XEdge, YEdge, Width - XEdge, Height - YEdge);
    try
      Details := ThemeServices.GetElementDetails((teEditTextNormal));
      ThemeServices.DrawParentBackground(Handle, DC, @Details, True);
      ThemeServices.DrawElement(DC, Details, R);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
{$ENDIF}
end;

procedure TImageEnView.WMPaint(var Message: TWMPaint);
begin
  {$IFDEF IEINCLUDEDIRECTSHOW}
  if assigned(fImageEnIO) and assigned(fImageEnIO.DShowParams) and fImageEnIO.DShowParams.RenderVideo and (fImageEnIO.DShowParams.State<>gsStopped) then
    fImageEnIO.DShowParams.RepaintVideo(handle, Canvas.Handle);
  {$ENDIF}
  inherited;
end;

procedure TImageEnView.IEMUpdate(var Message: TMessage);
begin
  Update;
end;

procedure TImageEnView.IEMProgress(var Message: TMessage);
begin
  if assigned(fOnProgress) then
    fOnProgress(self, Message.wParam);
end;

procedure TImageEnView.IEMFinishWork(var Message: TMessage);
begin
  if assigned(fOnFinishWork) then
    fOnFinishWork(self);
end;

procedure TImageEnView.WMSize(var Message: TWMSize);
begin
  inherited;
  if not fInsideUpdate then
    DoSize;
end;

procedure TImageEnView.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

{!!
<FS>TImageEnView.SetViewXY

<FM>Declaration<FC>
procedure SetViewXY(x, y: integer);

<FM>Description<FN>
Sets <A TImageEnView.ViewX> and <A TImageEnView.ViewY> in one step to change the displayed portion of the image (programatic scroll).
!!}
procedure TImageEnView.SetViewXY(x, y: integer);
var
  max_x, max_y: integer;
  lviewx, lviewy: integer;
begin
  ViewChanging(0, x);
  ViewChanging(1, y);
  lviewx := fViewX;
  lviewy := fViewY;
  if (x = fViewX) and (y = fViewY) then
    exit;
  GetMaxViewXY(max_x, max_y);
  fViewX := ilimit(x, 0, max_x);
  fViewY := ilimit(y, 0, max_y);
  if (fViewX = lviewx) and (fViewY = lviewy) then
    exit;
  if fDelayZoomFilter and (fZoomFilter <> rfNone) and ((fZoomX <> 100) or (fZoomY<>100)) then
    fStable := fStableReset;
  fStable2 := fStable2Reset;
  fDBToDraw := true;
  fFullUpdateRequest := true;
  Paint;
  ViewChange(0);
  if fSel then
    AniPolyFunc(self, true);
  if (fScrollBars = ssHorizontal) or (fScrollBars = ssBoth) then
    IESetScrollPos(Handle, SB_HORZ, trunc(fViewX / fRXScroll), true, fFlatScrollBars);
  if (fScrollBars = ssVertical) or (fScrollBars = ssBoth) then
    IESetScrollPos(Handle, SB_VERT, trunc(fViewY / fRYScroll), true, fFlatScrollBars);
end;

{!!
<FS>TImageEnView.ViewX

<FM>Declaration<FC>
property ViewX: integer;

<FM>Description<FN>
ViewX and <A TImageEnView.ViewY> specify the top-left of the image that is visible in the TImageEnView component window. Use ViewX/Y to programatically scroll images bigger than the TImageEnView.

<FM>Example<FC>
ImageEnView1.ViewX := 20;  // View from column 20 of bitmap (i.e. first 20 pixels on the left of the image will no longer be visible)
ImageEnView1.Zoom := 200; // zoom image to 200%
ImageEnView1.ViewX := 20;  // view from column 40 of bitmap (i.e. first 40 pixels on the left of the image will no longer be visible)

<FM>See Also<FN>
- <A TImageEnView.ViewY>
- <A TImageEnView.ExtentX>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.VisibleBitmapRect>
!!}
procedure TImageEnView.SetViewX(v: integer);
var
  max_x, max_y: integer;
  lviewx: Integer;
begin
  ViewChanging(0, v);
  if v = fViewX then
    exit;
  lviewx := fViewX;
  GetMaxViewXY(max_x, max_y);
  fViewX := ilimit(v, 0, max_x);
  if fViewX=lviewx then
    exit;
  if fDelayZoomFilter and (fZoomFilter <> rfNone) and ((fZoomX <> 100) or (fZoomY<>100)) then
    fStable := fStableReset;
  fStable2 := fStable2Reset;
  fDBToDraw := true;
  fFullUpdateRequest := true;
  paint;
  ViewChange(0);
  if fSel then
    AniPolyFunc(self, true);
  if (fScrollBars = ssHorizontal) or (fScrollBars = ssBoth) then
    IESetScrollPos(Handle, SB_HORZ, trunc(fViewX / fRXScroll), true, fFlatScrollBars);
end;

{!!
<FS>TImageEnView.ViewY

<FM>Declaration<FC>
property ViewY: integer;

<FM>Description<FN>
<A TImageEnView.ViewX> and ViewY specify the top-left of the image that is visible in the TImageEnView component window. Use ViewX/Y to programatically scroll images bigger than the TImageEnView.

<FM>Example<FC>
ImageEnView1.ViewX := 20;  // View from column 20 of bitmap (i.e. first 20 pixels on the left of the image will no longer be visible)
ImageEnView1.Zoom := 200; // zoom image to 200%
ImageEnView1.ViewX := 20;  // view from column 40 of bitmap (i.e. first 40 pixels on the left of the image will no longer be visible)

<FM>See Also<FN>
- <A TImageEnView.ViewX>
- <A TImageEnView.ExtentY>
- <A TImageEnView.YScr2Bmp>
- <A TImageEnView.VisibleBitmapRect>
!!}
procedure TImageEnView.SetViewY(v: integer);
var
  max_x, max_y: integer;
  lviewy: Integer;
begin
  ViewChanging(1, v);
  if v = fViewY then
    exit;
  lviewy := fViewY;
  GetMaxViewXY(max_x, max_y);
  fViewY := ilimit(v, 0, max_y);
  if fViewY=lviewy then
    exit;
  if fDelayZoomFilter and (fZoomFilter <> rfNone) and ((fZoomX <> 100) or (fZoomY<>100)) then
    fStable := fStableReset;
  fStable2 := fStable2Reset;
  fDBToDraw := true;
  fFullUpdateRequest := true;
  paint;
  ViewChange(0);
  if fSel then
    AniPolyFunc(self, true);
  if (fScrollBars = ssVertical) or (fScrollBars = ssBoth) then
    IESetScrollPos(Handle, SB_VERT, trunc(fViewY / fRYScroll), true, fFlatScrollBars);
end;

{!!
<FS>TImageEnView.GetMaxViewXY

<FM>Declaration<FC>
procedure GetMaxViewXY(var mx, my: integer);

<FM>Description<FN>
Returns the maximum values for <A TImageEnView.ViewX> and <A TImageEnView.ViewY> (which will fill the mx and my vars). I.e. this is the maximum that the image can be programatically scrolled.

!!}
procedure TImageEnView.GetMaxViewXY(var mx, my: integer);
var
  zx, zy: integer;
begin

  if fZoomX<>fZoomY then
  begin
    zx := trunc(fMaxLayerWidth * fZoomD100X); // new bitmap width (zoom)
    zy := trunc(fMaxLayerHeight * fZoomD100Y); // new bitmap height (zoom)
    if zx > (GetClientWidth + 10) then
      // this avoids to cut last right/bottom pixel
      zx := trunc((fMaxLayerWidth+1) * fZoomD100X); // new bitmap width (zoom)
    if zy > (GetClientHeight + 10) then
      // this avoids to cut last right/bottom pixel
      zy := trunc((fMaxLayerHeight+1) * fZoomD100Y); // new bitmap height (zoom)
    mx := 0;
    my := 0;
    if (zx > 0) and (zy > 0) then
    begin
      mx := zx - GetClientWidth;
      my := zy - GetClientHeight;
      if (mx < 0) or (XScr2Bmp(mx) = 0) then
        mx := 0;
      if (my < 0) or (YScr2Bmp(my) = 0) then
        my := 0;
      if zx<=Width then
        mx := 0;
      if zy<=Height then
        my := 0;
    end;
  end
  else
  begin
    zx := trunc(fMaxLayerWidth * fZoomD100X);  // new bitmap width (zoom)
    zy := trunc(fMaxLayerHeight * fZoomD100Y); // new bitmap height (zoom)
    if ((zx > (GetClientWidth + 10)) or (zy > (GetClientHeight + 10))) and (ScrollBars<>ssNone) then
    begin
      // this avoid to cut last right/bottom pixel
      zx := trunc((fMaxLayerWidth+1) * fZoomD100X);  // new bitmap width (zoom)
      zy := trunc((fMaxLayerHeight+1) * fZoomD100Y); // new bitmap height (zoom)
    end;
    mx := 0;
    my := 0;
    if (zx > 0) and (zy > 0) then
    begin
      mx := zx - GetClientWidth;
      my := zy - GetClientHeight;
      if (mx < 0) or (XScr2Bmp(mx) = 0) then
        mx := 0;
      if (my < 0) or (YScr2Bmp(my) = 0) then
        my := 0;
      if (zx <= Width) and (zy <= Height) then
      begin
        mx := 0;
        my := 0;
      end;
    end;
  end;

end;

function TImageEnView.PaletteChanged(Foreground: Boolean): Boolean;
begin
{$IFNDEF OCXVERSION}
  if assigned(application) and assigned(application.mainform) and assigned(application.mainform.canvas) then
  begin
    if IEDrawDibRealize(fHDrawDib, application.mainform.canvas.handle, false) > 0 then
      invalidate;
  end
  else
    invalidate;
{$ELSE}
  invalidate;
{$ENDIF}
  result := true;
end;

// makes an event OnImageChange
{!!
<FS>TImageEnView.ImageChange

<FM>Declaration<FC>
procedure ImageChange; virtual;

<FM>Description<FN>
The ImageChange method generates an <A TImageEnView.OnImageChange> event.
!!}
procedure TImageEnView.ImageChange;
begin
  inherited;
  if assigned(fOnImageChange) then
    fOnImageChange(self);
  StartPlayTimer;
  if UpdateReason <> ieurComponentStuffChanged then
  begin
    fImageSet := True;
  end;
end;

{!!
<FS>TImageEnView.Clear

<FM>Declaration<FC>
procedure Clear;

<FM>Description<FN>
Clear fills current image with <A TImageEnView.Background> color.

<FM>Example<FC>
ImageEn1.Background := clWhite;
ImageEn1.Clear;

!!}
procedure TImageEnView.Clear;
begin
  fIEBitmap.Fill(BackGround);
  RemoveAlphaChannel(false);
  Update;
end;

{!!
<FS>TImageEnView.Blank

<FM>Declaration<FC>
procedure Blank;

<FM>Description<FN>
Blank sets the image size to 1x1 (Width x Height).


<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('image.tif');  // load image
...
ImageEnView1.Blank;   // free memory

!!}
procedure TImageEnView.Blank;
begin
  StopPlayTimer;
  if assigned(fTransition) and fTransition.Running then
    fTransition.stop;
  fIEBitmap.Width := 1;
  fIEBitmap.Height := 1;
  Clear;
  fImageSet := False;
end;

{!!
<FS>TImageEnView.IsEmpty

<FM>Declaration<FC>
property IsEmpty: Boolean;

<FM>Description<FN>
IsEmpty returns True if the image is empty.
At the moment just checks if the bitmap size is less than 2x2.

<FM>See Also<FN>
- <A TImageenView.IsEmpty2>
!!}
function TImageEnView.GetIsEmpty: boolean;
begin
  result := (fIEBitmap.Width < 2) and (fIEBitmap.Height < 2);
end;

{!!
<FS>TImageEnView.IsEmpty2

<FM>Declaration<FC>
property IsEmpty2 : boolean;

<FM>Description<FN>
IsEmpty returns True if the image is empty, i.e. no image has been assigned or loaded.

<FM>See Also<FN>
- <A TImageenView.IsEmpty>
!!}
function TImageEnView.GetIsEmpty2: boolean;
var
  bBackgroundOnly: Boolean;
  brgb, irgb: TRGB;
begin
  Result := True;
  if fImageSet then
    Result := False
  else
  if (fIEBitmap.Width > 1) and (fIEBitmap.Height > 1) then
  begin
    irgb := fIEBitmap.Pixels[0, 0];
    brgb := TColor2TRGB(TColor(fBackground));
    bBackgroundOnly := (fIEBitmap.Width = Width) and (fIEBitmap.Height = Height) and (irgb.r = brgb.r) and (irgb.g = brgb.g) and (irgb.b = brgb.b);
    Result := bBackgroundOnly;
  end;
end;



function TImageEnView.GetSelectedRect(): TIERectangle;
begin
  if Selected then
    result := IERectangle(SelX1, SelY1, SelX2 - SelX1 +1, SelY2 - SelY1 +1)
  else
    result := IERectangle(0, 0, fIEBitmap.Width, fIEBitmap.Height);
end;


{!!
<FS>TImageEnView.SelX1

<FM>Declaration<FC>
property SelX1: integer; (Read-only)

<FM>Description<FN>
Returns the top-left X (column) coordinate of the selected area.
The value is a coordinate of the Bitmap itself, i.e. it is independent of scrolling (<A TImageenView.ViewX> and <A TImageenView.ViewY>) and <A TImageenView.Zoom> (e.g. you will get the same result whether the zoom is 50% or 100%).

Note:
- The selected area can be specified using <A TImageEnView.Select>
- You can convert the returned value to a screen value using <A TImageEnView.XBmp2Scr>

<FM>See Also<FN>
- <A TImageEnView.Select>
- <A TImageEnView.XBmp2Scr>
- <A TImageEnView.YBmp2Scr>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.YScr2Bmp>
!!}
function TImageEnView.GetSelX1: integer;
begin
  if fPolySelecting or fRectSelecting or fCircSelecting or (fRectResizing <> ieNone) or fRectMoving then
    result := PIEAnimPoly(fHPolySel)^.rx1
  else
    result := fSelectionMask.X1;
end;

{!!
<FS>TImageEnView.SelY1

<FM>Declaration<FC>
property SelY1: integer; (Read-only)

<FM>Description<FN>
Returns the top-left Y (row) coordinate of the selected area.
The value is a coordinate of the Bitmap itself, i.e. it is independent of scrolling (<A TImageenView.ViewX> and <A TImageenView.ViewY>) and <A TImageenView.Zoom> (e.g. you will get the same result whether the zoom is 50% or 100%).

Note:
- The selected area can be specified using <A TImageEnView.Select>
- You can convert the returned value to a screen value using <A TImageEnView.YBmp2Scr>

<FM>See Also<FN>
- <A TImageEnView.Select>
- <A TImageEnView.XBmp2Scr>
- <A TImageEnView.YBmp2Scr>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.YScr2Bmp>
!!}
function TImageEnView.GetSelY1: integer;
begin
  if fPolySelecting or fRectSelecting or fCircSelecting or (fRectResizing <> ieNone) or fRectMoving then
    result := PIEAnimPoly(fHPolySel)^.ry1
  else
    result := fSelectionMask.Y1;
end;

{!!
<FS>TImageEnView.SelX2

<FM>Declaration<FC>
property SelX2: integer; (Read-only)

<FM>Description<FN>
Returns bottom-right X (column) coordinate of the selected area.
The value is a coordinate of the Bitmap itself, i.e. it is independent of scrolling (<A TImageenView.ViewX> and <A TImageenView.ViewY>) and <A TImageenView.Zoom> (e.g. you will get the same result whether the zoom is 50% or 100%).

Note:
- The selected area can be specified using <A TImageEnView.Select>
- You can convert the returned value to a screen value using <A TImageEnView.XBmp2Scr>

<FM>See Also<FN>
- <A TImageEnView.Select>
- <A TImageEnView.XBmp2Scr>
- <A TImageEnView.YBmp2Scr>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.YScr2Bmp>

!!}
function TImageEnView.GetSelX2: integer;
begin
  if fPolySelecting or fRectSelecting or fCircSelecting or (fRectResizing <> ieNone) or fRectMoving then
    result := PIEAnimPoly(fHPolySel)^.rx2 - 1
  else
    result := fSelectionMask.X2;
end;

{!!
<FS>TImageEnView.SelY2

<FM>Declaration<FC>
property SelY2: integer; (Read-only)

<FM>Description<FN>
Returns the bottom-right Y (row) coordinate of the selected area.
The value is a coordinate of the Bitmap itself, i.e. it is independent of scrolling (<A TImageenView.ViewX> and <A TImageenView.ViewY>) and <A TImageenView.Zoom> (e.g. you will get the same result whether the zoom is 50% or 100%).

Note:
- The selected area can be specified using <A TImageEnView.Select>
- You can convert the returned value to a screen value using <A TImageEnView.YBmp2Scr>

<FM>See Also<FN>
- <A TImageEnView.Select>
- <A TImageEnView.XBmp2Scr>
- <A TImageEnView.YBmp2Scr>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.YScr2Bmp>

!!}
function TImageEnView.GetSelY2: integer;
begin
  if fPolySelecting or fRectSelecting or fCircSelecting or (fRectResizing <> ieNone) or fRectMoving then
    result := PIEAnimPoly(fHPolySel)^.ry2 - 1
  else
    result := fSelectionMask.Y2;
end;

{!!
<FS>TImageEnView.XBmp2Scr

<FM>Declaration<FC>
function XBmp2Scr(x: Integer): Integer;

<FM>Description<FN>
The XBmp2Scr and <A TImageEnView.YBmp2Scr> methods convert a bitmap coordinate to the window coordinate (considering the Zoom and View status). x and y are bitmap coordinates.
These methods are useful to select a bitmap region independently from the <A TImageEnView.Zoom>, <A TImageEnView.ViewX> and <A TImageEnView.ViewY> properties.

Note: If you are using multiple layers you should instead use TImageEnView.Layers[].<A TIELayer.ConvXBmp2Scr> method.


<FM>Example<FC>
// Select rect 10,10,100,100 of bitmap
ImageEnView1.Zoom(300);
x1 := XBmp2Scr(10);
y1 := YBmp2Scr(10);
x2 := XBmp2Scr(100);
y2 := YBmp2Scr(100);
ImageEnView1.Select( x1, y1, x2, y2);

// Select rect 10,10,100,100 of the window
ImageEnView1.Select(10, 10, 100, 100)
                                                       
<FM>See Also<FN>
- <A TImageEnView.YBmp2Scr>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.YScr2Bmp>
!!}       
function TImageEnView.XBmp2Scr(x: integer): integer;
var
  xx: integer;
begin
  xx := x - fo1x;
  if (xx < 0) or (xx >= fXBmp2ScrSize) then
    // aproximated value
    result := round(fOffX + (x + round((-QuantizeViewX(fViewX)) * f100DZoomX)) * fZoomD100X)
  else
    result := fOffX + fXBmp2Scr[xx];
end;

{!!
<FS>TImageEnView.YBmp2Scr

<FM>Declaration<FC>
function YBmp2Scr(y: Integer): Integer;

<FM>Description<FN>
The YBmp2Scr and <A TImageEnView.XBmp2Scr> methods convert a bitmap coordinate to the window coordinate (considering the Zoom and View status). x and y are bitmap coordinates.
These methods are useful to select a bitmap region independently from the <A TImageEnView.Zoom>, <A TImageEnView.ViewX> and <A TImageEnView.ViewY> properties.

Note: If you are using multiple layers you should instead use TImageEnView.Layers[].<A TIELayer.ConvYBmp2Scr> method.

<FM>Example<FC>
// Select rect 10, 10, 100, 100 of bitmap
ImageEnView1.Zoom(300);
x1 := XBmp2Scr(10);
y1 := YBmp2Scr(10);
x2 := XBmp2Scr(100);
y2 := YBmp2Scr(100);
ImageEnView1.Select( x1, y1, x2, y2);

// Select rect 10,10,100,100 of the window
ImageEnView1.Select(10, 10, 100, 100)
                
<FM>See Also<FN>
- <A TImageEnView.XBmp2Scr>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.YScr2Bmp>

!!}
function TImageEnView.YBmp2Scr(y: integer): integer;
var
  yy: integer;
begin
  yy := y - fo1y;
  if (yy < 0) or (yy >= fYBmp2ScrSize) then
  begin
    // aproximated value
    result := round(fOffY + (y + round((-QuantizeViewY(fViewY)) * f100DZoomY)) * fZoomD100Y);
  end
  else
    result := fOffY + fYBmp2Scr[yy];
end;

{!!
<FS>TImageEnView.XScr2Bmp

<FM>Declaration<FC>
function XScr2Bmp(x: integer): integer;

<FM>Description<FN>
The XScr2Bmp and <A TImageEnView.YScr2Bmp> methods convert a window coordinate to the corresponding bitmap coordinate (considering <A TImageEnView.Zoom> and <A TImageEnView.ViewX>, <A TImageEnView.ViewY> status).
<FC>x<FN> is a window coordinate.
Returns a bitmap coordinate which can be negative or larger than bitmap size if the window coordinate is beyond the displayed image's boundaries.
If you are using multiple layers please use instead TImageEnView.<A TImageEnView.Layers>[].<A TIELayer.ConvXScr2Bmp> method.

<FM>Example<FC>
// X and Y are MOUSE coordinates
bx := ImageEnView1.XScr2Bmp(X);
by := ImageEnView1.YScr2Bmp(Y);
// now bx and by are Bitmap coordinates (referred to the ImageEnView1.Bitmap)

<FM>See Also<FN>
- <A TImageEnView.YScr2Bmp>
- <A TImageEnView.XBmp2Scr>
- <A TImageEnView.YBmp2Scr>
- <A TImageEnView.VisibleBitmapRect>
!!}
function TImageEnView.XScr2Bmp(x: integer): integer;
var
  xx: integer;
begin
  xx := x - fOffX;
  if (xx < 0) or (xx >= fXScr2BmpSize) then
    // approximated value
    result := trunc((X - fOffX) * f100DZoomX + fo1x)
  else
    result := fXScr2Bmp[xx];
end;

{!!
<FS>TImageEnView.YScr2Bmp

<FM>Declaration<FC>
function YScr2Bmp(y: integer): integer;

<FM>Description<FN>
The XScr2Bmp and <A TImageEnView.YScr2Bmp> methods convert a window coordinate to the corresponding bitmap coordinate (considering <A TImageEnView.Zoom> and <A TImageEnView.ViewX>, <A TImageEnView.ViewY> status).
<FC>y<FN> is a window coordinate.
Returns a bitmap coordinate which can be negative or larger than bitmap size if the window coordinate is beyond the displayed image's boundaries.
If you are using multiple layers please use instead TImageEnView.<A TImageEnView.Layers>[].<A TIELayer.ConvYScr2Bmp> method.

<FM>Example<FC>
// X and Y are MOUSE coordinates
bx := ImageEnView1.XScr2Bmp(X);
by := ImageEnView1.YScr2Bmp(Y);
// now bx and by are Bitmap coordinates (referred to the ImageEnView1.Bitmap)
                
<FM>See Also<FN>
- <A TImageEnView.XScr2Bmp>
- <A TImageEnView.XBmp2Scr>
- <A TImageEnView.YBmp2Scr>
- <A TImageEnView.VisibleBitmapRect>
!!}
function TImageEnView.YScr2Bmp(y: integer): integer;
var
  yy: integer;
begin
  yy := y - fOffY;
  if (yy < 0) or (yy >= fYScr2BmpSize) then
    // approximated value
    result := trunc((Y - fOffY) * f100DZoomY + fo1y)
  else
    result := fYScr2Bmp[yy];
end;

{!!
<FS>TImageEnView.GetIdealZoom

<FM>Declaration<FC>
function TImageEnView.GetIdealZoom: Double;

<FM>Description<FN>
GetIdealZoom returns the best zoom value to stretch the image to fit within the component.

<FM>Example<FC>
// Both the following code snippets will have the same result
ImageEnView.Zoom := ImageEnView.GetIdealZoom;

// or
ImageEnView.Fit;
!!}
function TImageEnView.GetIdealZoom: double;
begin
  //result := GetOptimalZoom(IEBitmap.Width, IEBitmap.Height, Width, Height, true, true);
  Result := dmin(Width / (fMaxLayerWidth / 100), Height / (fMaxLayerHeight / 100));
end;

procedure TImageEnView.CalcEdges(var EdgeX, EdgeY: integer; NoVertSB, NoHorizSB: boolean);
begin
  case fBackgroundStyle of
    iebsCropped:
      begin
        EdgeX := IEGlobalSettings().EdgeX;
        EdgeY := IEGlobalSettings().EdgeY;
      end;
    iebsCropShadow:
      begin
        EdgeX := 5;
        EdgeY := 5;
      end;
    iebsSoftShadow:
      begin
        EdgeX := 9;
        EdgeY := 9;
      end;
  else
    begin
      if NoVertSB then
      begin
        if BorderStyle=bsNone then
          EdgeX := 0
        else
        if Ctl3D then
          EdgeX := IEGlobalSettings().EdgeX
        else
          EdgeX := IEGlobalSettings().BorderX;
      end
      else
        EdgeX := (Width - GetClientWidth) div 2;
      if NoHorizSB then
      begin
        if BorderStyle=bsNone then
          EdgeY := 0
        else
        if Ctl3D then
          EdgeY := IEGlobalSettings().EdgeY
        else
          EdgeY := IEGlobalSettings().BorderY;
      end
      else
        EdgeY := (Height - GetClientHeight) div 2;
    end;
  end;

end;

procedure TImageEnView.GetFitValueXY(var x, y: double);
var
  EdgeX, EdgeY: integer;
begin
  CalcEdges(EdgeX, EdgeY, true, true);
  x := (Width - EdgeX * 2) / (fMaxLayerWidth / 100);
  y := (Height - EdgeY * 2) / (fMaxLayerHeight / 100);
end;

function TImageEnView.GetFitValue: double;
var
  x, y: double;
begin
  GetFitValueXY(x, y);
  Result := dmin(x, y);
end;

{!!
<FS>TImageEnView.Fit

<FM>Declaration<FC>
procedure Fit;

<FM>Description<FN>
This method adjusts <A TImageEnView.Zoom> so that the image fits within the client area of the component (while respecting the aspect ratio) .
!!}
procedure TImageEnView.Fit;
begin
  if (fMaxLayerWidth <> 0) and (fMaxLayerHeight <> 0) then
  begin
    HideHorizScrollBar;
    HideVertScrollBar;
    Zoom := GetFitValue;
  end;
end;

{!!
<FS>TImageEnView.Stretch

<FM>Declaration<FC>
procedure Stretch;

<FM>Description<FN>
This method sets <A TImageEnView.ZoomX> and <A TImageEnView.ZoomY> values to stretch the image to fit within the component borders.
Note: Using this method you will lose the image aspect ratio and some functions, which require aspect ratio, may not work properly.

!!}
procedure TImageEnView.Stretch;
var
  x, y: Double;
begin
  if (fMaxLayerWidth <> 0) and (fMaxLayerHeight <> 0) then
  begin
    HideHorizScrollBar;
    HideVertScrollBar;
    GetFitValueXY(x, y);
    LockPaint;
    ZoomX := x;
    ZoomY := y;
    UnlockPaint;
    HideHorizScrollBar;
    HideVertScrollBar;
  end;
end;

{!!
<FS>TImageEnView.FitToWidth

<FM>Declaration<FC>
procedure FitToWidth;

<FM>Description<FN>
FitToWidth adjust the image display size (<A TImageEnView.Zoom>) so that it fits within the width of the control window.
                
<FM>See Also<FN>
- <A TImageEnView.Fit>
- <A TImageEnView.FitToHeight>
!!}
procedure TImageEnView.FitToWidth;
var
  EdgeX, EdgeY: integer;
begin
  CalcEdges(EdgeX, EdgeY, false, true);
  if (fMaxLayerWidth <> 0) and (fMaxLayerHeight <> 0) then
  begin
    LockPaint;
    Zoom := (Width - EdgeX * 2) / (fMaxLayerWidth / 100);
    Zoom := (GetClientWidth) / (fMaxLayerWidth / 100); // second time needed for scrollbars
    UnLockPaint;
  end;
end;

{!!
<FS>TImageEnView.FitToHeight

<FM>Declaration<FC>
procedure FitToHeight;

<FM>Description<FN>
FitToHeight adjust the image display size (<A TImageEnView.Zoom>) so that it fits within the height of the control window.

<FM>See Also<FN>
- <A TImageEnView.Fit>
- <A TImageEnView.FitToWidth>
!!}
procedure TImageEnView.FitToHeight;
var
  EdgeX, EdgeY: integer;
begin
  CalcEdges(EdgeX, EdgeY, true, false);
  if (fMaxLayerWidth <> 0) and (fMaxLayerHeight <> 0) then
  begin
    LockPaint;
    Zoom := (Height - EdgeY * 2) / (fMaxLayerHeight / 100);
    Zoom := (GetClientHeight) / (fMaxLayerHeight / 100); // second time needed for scrollbars
    UnLockPaint;
  end;
end;

function TImageEnView.GetOptimalZoom(ibitmapwidth, ibitmapheight, iwidth, iheight: integer; bolStretch, bolShrink: Boolean): double;
begin
  result := 100;
  try
    if (iwidth = 0) or (iheight = 0) or (ibitmapwidth = 0) or (ibitmapheight = 0) then
      exit;
    if (iBitmapHeight / iHeight) < (iBitmapWidth / iWidth) then
    begin // width is important
      if iBitmapWidth > iWidth then
      begin
        if bolShrink = False then
          result := 100
        else
          result := GetFitValue;
      end
      else
      begin
        if bolStretch = False then
          result := 100
        else
          result := GetFitValue;
      end;
    end
    else
    begin // height is important
      if iBitmapHeight > iHeight then
      begin
        if bolShrink = False then
          result := 100
        else
          result := GetFitValue;
      end
      else
      begin
        if bolStretch = False then
          result := 100
        else
          result := GetFitValue;
      end;
    end; // if wider than high
  except
  end;
end;

// makes a zoom inside the selected area

{!!
<FS>TImageEnView.ZoomSelection

<FM>Declaration<FC>
procedure ZoomSelection(AspectRatio: Boolean);

<FM>Description<FN>
The ZoomSelection method zooms to a rectangular area specified with Select method.
If AspectRatio is true then the zoom respects the aspect ratio of the image, otherwise the image area is stretched to match the size of the component.


<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.ZoomSelection; // fill client area (if possible) with rectangle 10,10,100,100

!!}
procedure TImageEnView.ZoomSelection(AspectRatio: Boolean);
begin
  ZoomSelectionEx(AspectRatio, false);
end;

procedure TImageEnView.ZoomSelectionEx(AspectRatio: Boolean; FireDoZoomIn: Boolean);
var
  dx, dy: integer;
  x1, y1: integer;
  cx, cy: integer;
  zx, zy: double;
  zm: Double;
begin
  if fSel then
  begin
    dx := Selx2 - Selx1 + 1;
    dy := Sely2 - Sely1 + 1;
    x1 := SelX1;
    y1 := SelY1;
    if (x1 < 0) or (y1 < 0) or (dx < 2) or (dy < 2) then
      exit;
    Deselect;
    zm := dmin(Width / (dx / 100), Height / (dy / 100));  // zoom with aspect ratio
    if FireDoZoomIn then
      DoZoomIn(zm);
    if AspectRatio then
    begin
      Zoom := zm;
    end
    else
    begin
      ZoomX := Width / (dx / 100);
      ZoomY := Height / (dy / 100);
    end;
    zx := ZoomX / 100;
    zy := ZoomY / 100;
    x1 := trunc(x1 * zx);
    dx := trunc(dx * zx);
    y1 := trunc(y1 * zy);
    dy := trunc(dy * zy);
    cy := (GetClientHeight - dy) div 2;
    cx := (GetClientWidth - dx) div 2;
    SetViewXY(x1 - cx, y1 - cy);
  end;
end;


// called by "initialization"
procedure InitImageEnView;
begin
  with IEGlobalSettings().GridPen do
  begin
    Mode := pmNot;
    Style := psSolid;
    Width := 1;
  end;
end;


{!!
<FS>TImageEnView.LockPaint

<FM>Declaration<FC>
procedure LockPaint;

<FM>Description<FN>
The LockPaint method increases the lock counter's value. The ImageEnView window will not be repainted while <A TImageEnView.LockPaintCount> > 0.

See also: <A TImageEnView.UnlockPaint>.

<FM>Example<FC>
ImageEnView1.LockPaint;
ImageEnView1.ViewX := 10;
ImageEnView1.ViewY := 20;
ImageEnView1.Zoom := 300;
ImageEnView1.UnLockPaint;

!!}
// Inc fLockPaint counter
procedure TImageEnView.LockPaint;
begin
  inc(fLockPaint);
end;


{!!
<FS>TImageEnView.UnlockPaint

<FM>Declaration<FC>
function UnlockPaint: integer;

<FM>Description<FN>
Use the UnlockPaint method to decrease the lock counter's value. The ImageEnView window will not be repainted while <A TImageEnView.LockPaintCount> > 0.

If calling UnlockPaint resets <A TImageEnView.LockPaintCount> to zero then the the <A TImageEnView.Update> method is automatically called.

Returns the lock count.

See also: <A TImageEnView.LockPaint>


<FM>Example<FC>
ImageEnView1.LockPaint;
ImageEnView1.ViewX := 10;
ImageEnView1.ViewY := 20;
ImageEnView1.Zoom := 300;
ImageEnView1.UnLockPaint;
!!}
// Dec fLockPaint counter
// return current fLockPaint value (after Dec)
// Call Update if fLockPaint = 0
function TImageEnView.UnLockPaint: integer;
begin
  if fLockPaint > 0 then
    dec(fLockPaint);
  if fLockPaint = 0 then
    Update;
  result := fLockPaint;
end;

// Dec fLockPaint counter
// return current fLockPaint value (after Dec)
function TImageEnView.NPUnLockPaint: integer;
begin
  if fLockPaint > 0 then
    dec(fLockPaint);
  result := fLockPaint;
end;


{!!
<FS>TImageEnView.ZoomAt

<FM>Declaration<FC>
procedure ZoomAt(x, y: integer; ZoomVal: double; Center: Boolean=true);

<FM>Description<FN>
Zooms by the percentage specified by ZoomVal.
If the optional <FC>Center<FN> parameter if False, the zooming center is <FC>x, y<FN>, otherwise (the default) it is the center of image.
!!}
// zoom to ZoomVal at x,y coordinates (client area coordinates)
(*
procedure TImageEnView.ZoomAt(x, y: integer; ZoomVal: double; Center: Boolean);
var
  zz: double;
  bx, by: Integer;
  w2, h2: Integer;
begin
  if assigned(fNavigator) then
    fNavigator.LockPaint;
  ViewChanging(2, x);
  ViewChanging(2, y);
  LockPaint;
  bx := XScr2Bmp(x);
  by := YScr2Bmp(y);

  Zoom := ZoomVal;    // fire ViewChange(1) event
  zz := Zoom / 100;
  w2 := (GetClientWidth div 2);
  h2 := (GetClientHeight div 2);

  if Center then
    SetViewXY(trunc(bx * zz - w2), trunc(by * zz - h2))     // fire ViewChange(0) event
  else
    SetViewXY(trunc(bx*zz - x +Zoom/100), trunc(by*zz - y+Zoom/100)); // fire ViewChange(0) event

  UnLockPaint;

  CalcPaintCoords;
  CreateCoordConvLUT; // recalculates coordinate conversion LUT

  if assigned(fNavigator) then
    fNavigator.NPUnLockPaint;

end;
*)
procedure TImageEnView.ZoomAt(x, y: integer; ZoomVal: double; Center: Boolean);
var
  zz: double;
  bx, by: Integer;
  w2, h2: Integer;
begin
  if assigned(fNavigator) then
    fNavigator.LockPaint;
  ViewChanging(2, x);
  ViewChanging(2, y);
  LockPaint;
  bx := XScr2Bmp(x);
  by := YScr2Bmp(y);

  Zoom := ZoomVal;    // fire ViewChange(1) event
  zz := Zoom / 100;
  w2 := (GetClientWidth div 2);
  h2 := (GetClientHeight div 2);

  if Center then
    SetViewXY(trunc(bx * zz - w2), trunc(by * zz - h2))     // fire ViewChange(0) event
  else
    SetViewXY(trunc(bx*zz - x +Zoom/100), trunc(by*zz - y+Zoom/100)); // fire ViewChange(0) event

  UnLockPaint;

  CalcPaintCoords;
  CreateCoordConvLUT; // recalculates coordinate conversion LUT

  if assigned(fNavigator) then
  begin
    fNavigator.NPUnLockPaint;
    SetNavigatorRect; // 3.0.4 (27/07/2009 14:18)
  end;
end;

{!!
<FS>TImageEnView.ZoomIn

<FM>Declaration<FC>
procedure ZoomIn;

<FM>Description<FN>
Provides a convenient method to handle clicking of a "Zoom In" button.  Each time <FC>ZoomIn<FN> is called it increases the zoom by a logical step, e.g. 50 to 75, 75 to 100, etc.
It also includes "Fit to Window" as one of the steps.

Note: <FC>ZoomIn<FN> will have no effect if <A TImageEnView.AutoFit>, <A TImageEnView.AutoShrink> or <A TImageEnView.AutoStretch> are enabled.

<FM>See Also<FN>
- <A TImageEnView.Zoom>
- <A TImageEnView.ZoomOut>
!!}
procedure TImageEnView.ZoomIn;
begin
  Zoom := GetNextZoomValue(Zoom, True, GetIdealZoom);
end;

{!!
<FS>TImageEnView.ZoomOut

<FM>Declaration<FC>
procedure ZoomOut;

<FM>Description<FN>
Provides as a convenient method to handle clicking of a "Zoom Out" button.  Each time <FC>ZoomOut<FN> is called it decreases the zoom by a logical step, e.g. 100 to 75, 75 to 50, etc.
It also includes "Fit to Window" as one of the steps.

Note: <FC>ZoomOut<FN> will have no effect if <A TImageEnView.AutoFit>, <A TImageEnView.AutoShrink> or <A TImageEnView.AutoStretch> are enabled.

<FM>See Also<FN>
- <A TImageEnView.Zoom>
- <A TImageEnView.ZoomIn>
!!}
procedure TImageEnView.ZoomOut;
begin   
  Zoom := GetNextZoomValue(Zoom, False, GetIdealZoom);
end;

{!!
<FS>TImageEnView.DrawTo

<FM>Declaration<FC>
procedure DrawTo(Canvas: TCanvas);

<FM>Description<FN>
This method draws the current view to the specified Canvas.

Note: The background is not painted, and the image has an up-left alignment.


<FM>Example<FC>
// draw in ImageEn2 all what is displayed in ImageEnView1
ImageEnView1.DrawTo(ImageEnView2.Bitmap.Canvas);
ImageEnView2.Update;

!!}
procedure TImageEnView.DrawTo(Canvas: TCanvas);
begin
  Canvas.CopyRect(Rect(0, 0, frx, fry), fTBitmap.Canvas, Rect(fOffX, fOffY, fOffX + frx, fOffY + fry));
end;

function TImageEnView.HasParentWindow: boolean;
begin
  result := (GetParentForm(self) <> nil) or (ParentWindow <> 0);
end;

{!!
<FS>TImageEnView.UpdateNoPaint

<FM>Declaration<FC>
procedure UpdateNoPaint;

<FM>Description<FN>
The Update method refreshes the TImageEnView component view with the current state of the image.
You must call <A TImageEnView.Update> or UpdateNoPaint() whenever the <A TImageEnView.Bitmap> or <A TImageEnView.IEBitmap> property is modified by your code.
Unlike <A TImageEnView.Update>, UpdateNoPaint doesn't paint the image, just refreshes the bitmap data.
!!}
procedure TImageEnView.UpdateNoPaint;
begin
  try
    fBlockPaint := true;
    Update;
  finally
    fBlockPaint := false;
  end;
end;

function TImageEnView.RequiresScrollBars: boolean;
var
  max_x, max_y: integer;
begin
  GetMaxViewXY(max_x, max_y);
  result := (max_x > 0) or (max_y > 0);
end;

procedure TImageEnView.HideHorizScrollBar;
begin
  if fHScrollBarVisible then
  begin
    fHScrollBarVisible := false;
    IEShowScrollBar(handle, SB_HORZ, false, fFlatScrollBars);
  end;
end;

procedure TImageEnView.HideVertScrollBar;
begin
  if fVScrollBarVisible then
  begin
    fVScrollBarVisible := false;
    IEShowScrollBar(handle, SB_VERT, false, fFlatScrollBars);
  end;
end;

{!!
<FS>TImageEnView.Update

<FM>Declaration<FC>
procedure Update;

<FM>Description<FN>
The Update method refreshes the TImageEnView component view with the current state of the image and redraws the client area.
You must call <A TImageEnView.Update> or UpdateNoPaint() whenever the <A TImageEnView.Bitmap> or <A TImageEnView.IEBitmap> property is modified by your code.
Update is called automatically when an image processing or I/O operation is executed.

<FM>Example<FC>
// Draw a rectangle on the image and show it
ImageEnView1.Bitmap.Canvas.Rectangle(5, 5, 100, 100);
ImageEnView1.Update;
!!}
procedure TImageEnView.Update;
var
  max_x, max_y: integer;
  lClientWidth, lClientHeight: integer;
  bb: boolean;
  EdgeX, EdgeY: integer;
  i: Integer;
  newZoom: double;
begin
  if fUpdateLocked <> 0 then
    exit;
  if (csDesigning in ComponentState) then
    exit;
{$IFDEF IEFIXUPDATE}
  if (ComponentState <> []) and (ComponentState <> [csDesigning]) and (ComponentState <> [csFreeNotification]) then
    exit;
{$ELSE}
  if (ComponentState <> []) and (ComponentState <> [csDesigning]) then
    exit;
{$ENDIF}

  fInsideUpdate := true;

  try
    // empty layer mask caches
    for i := 0 to fLayers.Count-1 do
      if (fUpdate_MaskCache = -1) or (fUpdate_MaskCache=i) then
        FreeAndNil( TIELayer(fLayers[i]).fCachedLayerMask );
    fUpdate_MaskCache := -1;

    // check layers sizes
    DoLayersSync;

    if (fAutoStretch or fAutoShrink or fAutoFit) and (fMaxLayerWidth <> 0) and (fMaxLayerHeight <> 0) then
    begin
      ViewChanging(2, fZoomX);
      case fBackgroundStyle of
        iebsCropped:
          begin
            EdgeX := IEGlobalSettings().EdgeX;
            EdgeY := IEGlobalSettings().EdgeY;
          end;
        iebsCropShadow:
          begin
            EdgeX := 5;
            EdgeY := 5;
          end;
        iebsSoftShadow:
          begin
            EdgeX := 9;
            EdgeY := 9;
          end;
      else
      begin
        EdgeX := (Width - GetClientWidth) div 2;
        EdgeY := (Height - GetClientHeight) div 2;
      end;
      end;

      if (fMaxLayerWidth < 2) and (fMaxLayerHeight < 2) then
        newZoom := 100
      else
        NewZoom := GetOptimalZoom(fMaxLayerWidth, fMaxLayerHeight, (GetClientWidth - EdgeX * 2), (GetClientHeight - EdgeY * 2), fAutoStretch or fautofit, fAutoShrink or fautofit);
      if (NewZoom) < 1 then
        NewZoom := 1;
      SetZoomNoUpdate(newZoom);
      CalcPaintCoords;
      CreateCoordConvLUT; // recalculates coordinate conversion LUT
      ViewChange(1);
      // to avoid flickering
      fStable := 0;
      fStable2 := 0;
    end;

    // check bitmap size changes
    if (fBitmapWidth <> fIEBitmap.Width) or (fBitmapHeight <> fIEBitmap.Height) then
    begin
      fBitmapWidth := fIEBitmap.Width;
      fBitmapHeight := fIEBitmap.Height;

      if (fSelectionMask.Width<>fIEBitmap.Width) or (fSelectionMask.Height<>fIEBitmap.Height) then
      begin
        if fSel then
        begin
          fSelectionMask.Resize(fIEBitmap.Width, fIEBitmap.Height);
          fSelectionMask.SyncFull;
          fSelectionMask.SyncRect;
        end
        else
          fSelectionMask.AllocateBits(fIEBitmap.Width, fIEBitmap.Height, fSelectionMaskDepth);
      end;

      if fIEBitmap.HasAlphaChannel and ((fIEBitmap.AlphaChannel.Width <> fIEBitmap.Width) or (fIEBitmap.AlphaChannel.Height <> fIEBitmap.Height)) then
      begin
        fIEBitmap.AlphaChannel.Allocate(fIEBitmap.Width, fIEBitmap.Height, ie8g);
        fIEBitmap.AlphaChannel.Fill(255);
      end;
    end;

    if not HandleAllocated then
      exit;

    if not (csDesigning in ComponentState) and (HasParentWindow) then
    begin
      // Save client sizes because the scrollbar changes the client area, then we must recall Update (recursive call)
      lClientWidth := GetClientWidth;
      lClientHeight := GetClientHeight;
      //
      bb := false;
      GetMaxViewXY(max_x, max_y);
      if fViewX > max_x then
      begin
        ViewChanging(0, max_x);
        fViewX := max_x;
        bb := true;
      end;
      if fViewY > max_y then
      begin
        ViewChanging(1, max_y);
        fViewY := max_y;
        bb := true;
      end;
      if bb then
        ViewChange(0);
      try
        if HasParentWindow and ((fScrollBars = ssHorizontal) or (fScrollBars = ssBoth)) then
        begin
          if max_x > 0 then
          begin
            // we need horizontal scroll bar
            fHScrollBarVisible := true;
            fRXScroll := (max_x + GetClientWidth) / 65536;
            IESetScrollBar(Handle, SB_HORZ, 0, 65535, trunc(GetClientWidth / fRXScroll), trunc(fViewX / fRXScroll), true, fFlatScrollBars);
            IEEnableScrollBar(Handle, SB_HORZ, ESB_ENABLE_BOTH, fFlatScrollBars);
            IEShowScrollBar(handle, SB_HORZ, true, fFlatScrollBars);
          end
          else
          if fScrollBarsAlwaysVisible then
          begin
            // the scroll bar is always visible
            fHScrollBarVisible := true;
            IEEnableScrollBar(Handle, SB_HORZ, ESB_DISABLE_BOTH, fFlatScrollBars);
            IEShowScrollBar(handle, SB_HORZ, true, fFlatScrollBars);
          end
          else
          begin
            // we don't need the horizontal scroll bar
            HideHorizScrollBar;
          end;
        end;
        if HasParentWindow and ((fScrollBars = ssVertical) or (fScrollBars = ssBoth)) then
        begin
          if max_y > 0 then
          begin
            // we need vertical scroll bar
            fVScrollBarVisible := true;
            fRYScroll := (max_y + GetClientHeight) / 65536;
            IESetScrollBar(Handle, SB_VERT, 0, 65535, trunc(GetClientHeight / fRYScroll), trunc(fViewY / fRYScroll), true, fFlatScrollBars);
            IEEnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH, fFlatScrollBars);
            IEShowScrollBar(handle, SB_VERT, true, fFlatScrollBars);
          end
          else
          if fScrollBarsAlwaysVisible then
          begin
            // the scroll bar is always visible
            fVScrollBarVisible := true;
            IEEnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH, fFlatScrollBars);
            IEShowScrollBar(handle, SB_VERT, true, fFlatScrollBars);
          end
          else
          begin
            // we don't need the vertical scroll bar
            HideVertScrollBar;
          end;
        end;
      except
      end;
      if ((lClientWidth <> GetClientWidth) or (lClientHeight <> GetClientHeight)) and not fUpdateInsideUpdate then
      begin
        fUpdateInsideUpdate := true;
        Update; // recursive, see above
        fUpdateInsideUpdate := false;
      end;
    end;

    UpdateLimits;

    fDBToDraw := true;
    fFullUpdateRequest := true;
    if HasParentWindow and (fBlockPaint = false) and fUpdateInvalidate then
      invalidate;

    if assigned(fNavigator) and (fNavigator.fLockPaint = 0)then
    begin
      if fUpdateReason = ieurDefault then
        fNavigator.Update;
      SetNavigatorRect;
    end;

  finally
    fUpdateReason := ieurDefault;
    fInsideUpdate := false;
  end;
end;

procedure TImageEnView.UpdateLimits;
begin
  fZZWW := round(fMaxLayerWidth * fZoomD100X); // new Bitmap width
  fZZHH := round(fMaxLayerHeight * fZoomD100Y); // new Bitmap height
  fOffX := 0;
  fOffY := 0;
  if not (csDesigning in ComponentState) then
  begin
    fExtX := imin(fZZWW, GetClientWidth); // width of target bitmap
    fExtY := imin(fZZHH, GetClientHeight); // height of target bitmap

    if (fExtx < GetClientWidth) then
      case fImageHorizAlignment of
        //iehLeft:
        iehCenter: fOffX := (GetClientWidth - fExtX) div 2;
        iehRight:  fOffX := (GetClientWidth - fExtX);
      end;
    if (fExty < GetClientHeight) then
      case fImageVertAlignment of
        //ievTop:
        ievCenter: fOffY := (GetClientHeight - fExtY) div 2;
        ievBottom: fOffY := (GetClientHeight - fExtY);
      end;
  end
  else
  begin
    fExtX := imin(fZZWW, Width);
    fExtY := imin(fZZHH, Height);
  end;
end;

{!!
<FS>TImageEnView.Center

<FM>Declaration<FC>
property Center: boolean;

<FM>Description<FN>
If Center is True, the image is displayed in the center of the ImageEnViews client area. Default is True.
This is equivalent to <A TImageEnView.ImageHorizAlignment> = iehCenter and <A TImageEnView.ImageVertAlignment> = ievCenter.

<FM>See Also<FN>
- <A TImageEnView.ImageHorizAlignment>
- <A TImageEnView.ImageVertAlignment>
!!}
procedure TImageEnView.SetCenter(value: boolean);
begin
  if value then
  begin
    ImageHorizAlignment := iehCenter;
    ImageVertAlignment  := ievCenter;
  end
  else
  begin
    ImageHorizAlignment := iehLeft;
    ImageVertAlignment  := ievTop;
  end;
end;


function TImageEnView.GetCenter(): Boolean;
begin
  result := (ImageHorizAlignment = iehCenter) and (ImageVertAlignment = ievCenter);
end;


{!!
<FS>TImageEnView.ImageHorizAlignment

<FM>Declaration<FC>
property ImageHorizAlignment: <A TIEHAlign>;

<FM>Description<FN>
Specifies where to align horizontally the image inside the component. Default is iehCenter.

<FM>See Also<FN>
- <A TImageEnView.ImageVertAlignment>
- <A TImageEnView.Center>
!!}
procedure TImageEnView.SetImageHorizAlignment(value: TIEHAlign);
begin
  if value <> fImageHorizAlignment then
  begin
    fImageHorizAlignment := value;
    UpdateReason := ieurScrolled;
    Update();
  end;
end;


{!!
<FS>TImageEnView.ImageVertAlignment

<FM>Declaration<FC>
property ImageVertAlignment: <A TIEVAlign>;

<FM>Description<FN>
Specifies where to align vertically the image inside the component. Default is ievCenter.

<FM>See Also<FN>
- <A TImageEnView.ImageHorizAlignment>
- <A TImageEnView.Center>
!!}
procedure TImageEnView.SetImageVertAlignment(value: TIEVAlign);
begin
  if value <> fImageVertAlignment then
  begin
    fImageVertAlignment := value;
    UpdateReason := ieurScrolled;
    Update();
  end;
end;


procedure TImageEnView.SetMouseInteract(v: TIEMouseInteract);
begin
  if v <> fMouseInteract then
  begin  
    if fPolySelecting and (miSelectPolygon in fMouseInteract) then
    begin
      AnimPolygonRemoveLastPoint(fHPolySel);
      fSelectionMask.Empty;
      EndSelect;
      fPolySelecting := False;
      invalidate;
    end;

    if (miScroll in v) and not (miScroll in fMouseInteract) then // miScroll
      v := v - [miSelect, miSelectPolygon, miSelectCircle, miSelectZoom, miSelectMagicWand, miSelectLasso, miMoveLayers, miResizeLayers, miRotateLayers]
    else
    if (miSelect in v) and not (miSelect in fMouseInteract) then // miSelect
      v := v - [miSelectPolygon, miSelectCircle, miScroll, miSelectZoom, miSelectMagicWand, miSelectLasso, miMoveLayers, miResizeLayers, miRotateLayers]
    else
    if (miSelectPolygon in v) and not (miSelectPolygon in fMouseInteract) then // miSelectPolygon
      v := v - [miSelect, miScroll, miSelectCircle, miSelectZoom, miZoom, miSelectMagicWand, miSelectLasso, miMoveLayers, miResizeLayers, miRotateLayers]
    else
    if (miSelectCircle in v) and not (miSelectCircle in fMouseInteract) then // miSelectCircle
      v := v - [miSelect, miSelectPolygon, miScroll, miSelectZoom, miSelectMagicWand, miSelectLasso, miMoveLayers, miResizeLayers, miRotateLayers]
    else
    if (miSelectLasso in v) and not (miSelectLasso in fMouseInteract) then // miSelectLasso
      v := v - [miSelect, miSelectPolygon, miScroll, miSelectZoom, miSelectMagicWand, miSelectCircle, miMoveLayers, miResizeLayers, miRotateLayers]
    else
    if (miSelectZoom in v) and not (miSelectZoom in fMouseInteract) then // miSelectZoom
      v := v - [miScroll, miSelect, miSelectCircle, miSelectPolygon, miSelectMagicWand, miSelectLasso, miMoveLayers, miResizeLayers, miRotateLayers]
    else
    if (miZoom in v) and not (miZoom in fMouseInteract) then // miZoom
      v := v - [miSelectPolygon, miSelectMagicWand, miMoveLayers, miResizeLayers, miRotateLayers]
    else
    if (miSelectMagicWand in v) and not (miSelectMagicWand in fMouseInteract) then // miSelectMagicWand
      v := v - [miSelect, miScroll, miSelectCircle, miSelectZoom, miZoom, miSelectPolygon, miSelectLasso, miMoveLayers, miResizeLayers, miRotateLayers]
    else
    if (miRotateLayers in v) then // miRotateLayers
      v := v - [miSelect, miScroll, miSelectCircle, miSelectZoom, miZoom, miSelectPolygon, miSelectLasso, miSelectMagicWand, miResizeLayers]
    else
    if (miMoveLayers in v) then // miMoveLayers
    begin
      v := v - [miSelect, miScroll, miSelectCircle, miSelectZoom, miZoom, miSelectPolygon, miSelectLasso, miSelectMagicWand];
      if miResizeLayers in v then
        v := v-[miRotateLayers];
    end
    else
    if (miResizeLayers in v) then // miResizeLayers
      v := v - [miSelect, miScroll, miSelectCircle, miSelectZoom, miZoom, miSelectPolygon, miSelectLasso, miSelectMagicWand, miRotateLayers];

    if not (miRotateLayers in v) then
      LayersFixRotations  // exiting from rotation mode? Anyway fix rotations.
    else
    if fAutoFixRotationBorders then
      LayersFixBorders;   // entering rotation mode, fix borders

    fMouseInteract := v;

    Update();
  end;
end;

// track mouse move for polygonal selection
procedure TImageEnView.PolyDraw1;
var
  x, y: integer;
begin
  if fPolySelecting and (not fLassoSelecting) then
  begin
    canvas.pen.mode := pmNot;
    canvas.pen.width := 1;
    canvas.pen.style := psSolid;
    with PIEAnimPoly(fHPolySel)^ do
      if PolyCount > 0 then
      begin
        canvas.moveto(fMMoveX, fMMoveY);
        x := CurrentLayer.ConvXBmp2Scr(Poly^[PolyCount - 1].x);
        y := CurrentLayer.ConvYBmp2Scr(Poly^[PolyCount - 1].y);
        canvas.lineto(x, y);
      end;
  end;
end;




procedure TImageEnView.DrawMarkOutNavigator(const rc: TRect);
var
  dx, dy: integer;
begin
  if not assigned(fNavigatorBackBuffer) then
  begin
    fNavigatorBackBuffer := TIEBitmap.Create(GetClientWidth, GetClientHeight, ie24RGB);
    fNavigatorBackBuffer.Location := ieTBitmap;
  end;
  if (fNavigatorBackBuffer.Width<>GetClientWidth) or (fNavigatorBackBuffer.Height<>GetClientHeight) then
    fNavigatorBackBuffer.Resize(GetClientWidth, GetClientHeight);
  with rc do
  begin
    dx := imin(right - left, GetClientWidth);
    dy := imin(bottom - top, GetClientHeight);
    BitBlt(fNavigatorBackBuffer.Canvas.Handle, left, top, dx, dy, fTBitmap.Canvas.Handle, left, top, SRCCOPY);
    with CurrentLayer.DrawingInfo do
      IEDrawGrayedOut(fNavigatorBackBuffer.Canvas, XDst, YDst, WidthDst, HeightDst,
                      fNavigatorActualRect.Left, fNavigatorActualRect.Top,
                      fNavigatorActualRect.Right, fNavigatorActualRect.Bottom);
    BitBlt(Canvas.Handle, left, top, dx, dy, fNavigatorBackBuffer.Canvas.Handle, left, top, SRCCOPY);
  end;
end;



{!!
<FS>TImageEnView.PaintRect

<FM>Declaration<FC>
procedure PaintRect(const rc: TRect);

<FM>Description<FN>
PaintRect repaints only the rectangle <FC>rc<FN> without waiting for the Windows Paint message.

<FM>Example<FC>
// this code paints an image (width=50 height=50) at bitmap coordinates 10,10
ImageEnView1.Bitmap.Canvas.Draw(10, 10, MyBitmap);
// map the bitmap coordinates to view coordinates (rc is a TRect type)
With ImageEnView1 do
   rc := rect( XBmp2Scr(10), YBmp2Scr(10), XBmp2Scr(10+50), YBmp2Scr(10+50) );
ImageEnView1.UpdateRect(rc);  // update internal ImageEn state
ImageEnView1.PaintRect(rc);  // paint the changes on the screen

!!}
procedure TImageEnView.PaintRect(const rc: TRect);
var
  dx, dy: integer;
  ur: TRect;
  {$IFDEF IEDEBUG}
  t1: dword;
  {$ENDIF}
begin
  if (not HasParentWindow) and (not fOffscreenPaint) then
    exit;
  {$IFDEF IEDEBUG}
  t1 := gettickcount;
  {$ENDIF}
  // prepare the back buffer
  if (fTBitmap.Width <> GetClientWidth) or (fTBitmap.Height <> GetClientHeight) then
  begin
    fTBitmap.Allocate(GetClientWidth, GetClientHeight, ie24RGB);
    fDBToDraw := true;
  end;
  // draw on the back buffer
  if fDBToDraw then
  begin
    ur := rc;
    PaintToEx(fTBitmap, fTBitmap.TBitmapScanlines, @ur, true, true);
    if assigned(fOnDrawBackBuffer) then
      fOnDrawBackBuffer(self);
    fDBToDraw := false;
  end;
  // draw the back buffer to the canvas
  with fBitmapInfoHeader256 do
  begin
    biSize := sizeof(TBitmapInfoHeader);
    biWidth := GetClientWidth;
    biHeight := GetClientHeight;
    biPlanes := 1;
    if fTBitmap.PixelFormat = ie1g then
    begin
      biBitCount := 1;
      Palette[1].rgbRed := 255;
      Palette[1].rgbGreen := 255;
      Palette[1].rgbBlue := 255;
    end
    else
      biBitCount := 24;
    biCompression := BI_RGB;
  end;

  {$IFDEF IEINCLUDEDIRECTSHOW}
  if assigned(fImageEnIO) and assigned(fImageEnIO.DShowParams) and fImageEnIO.DShowParams.RenderVideo and (fImageEnIO.DShowParams.State<>gsStopped) then
    with CurrentLayer.DrawingInfo do
      ExcludeClipRect(Canvas.Handle, XDst, YDst, XDst+WidthDst, YDst+HeightDst);
  {$ENDIF}

  with rc do
  begin
    dx := imin(right - left, GetClientWidth);
    dy := imin(bottom - top, GetClientHeight);

    if (fUseDrawDibDraw or (IEGlobalSettings().SystemColors <= 8)) and not IEGlobalSettings().IsRemoteSession then
      // drawdibdraw. Good for dithering in system with <=256 colors
      IEDrawDibDraw(fHDrawDib, Canvas.Handle, left, top, dx, dy, PBitmapInfoHeader(@fBitmapInfoHeader256)^, fTBitmap.Scanline[fTBitmap.Height - 1], left, top, dx, dy, 0)
    else
    begin
      // new mode
      if (IEGlobalSettings().SystemColors < 24) and not IEGlobalSettings().IsRemoteSession then
      begin
        // dithering needed (for example display with 16bit depth need dithering)
        SetStretchBltMode(Canvas.Handle, HALFTONE);
        StretchBlt(Canvas.Handle, left, top, dx, dy, fTBitmap.Canvas.Handle, left, top, dx, dy, SRCCOPY);
      end
      else
      begin
        // no dithering needed (fastest way)
        if fIsNavigator and (ienoMARKOUTER in fNavigatorOptions) then
          DrawMarkOutNavigator(rc)
        else
          BitBlt(Canvas.Handle, left, top, dx, dy, fTBitmap.Canvas.Handle, left, top, SRCCOPY);
      end;
    end;

  end;

{$IFDEF IEDEBUG}
  //OutputDebugString(PAnsiChar('PaintRect: '+IEIntToStr(gettickcount-t1)+' ms'));
{$ENDIF}

  if fPolySelecting then
    PolyDraw1;

  if (fOldHandle<>0) and (fOldHandle<>Handle) then
    IO.RecreatedTImageEnViewHandle;
  fOldHandle := Handle;
end;

procedure TImageEnView.DoDrawVersion;
var
  x, y: integer;
  ss: string;
begin
  with Canvas do
  begin
    Font.Name := 'Arial';
    Font.Height := 13;
    Font.Color := clBlack;
    Brush.style := bsSolid;
    Brush.Color := clWhite;
    ss := 'ImageEn Version ' + string(IEMAINVERSION) + ' - ' + IntToStr(IEMAINDATEDD) + '/' + IntToStr(IEMAINDATEMM) + '/' + IntToStr(IEMAINDATEYY);
    y := ClientHeight - TextHeight(ss);
    x := ClientWidth - TextWidth(ss);
    TextOut(x, y, ss);
  end;
end;

procedure TImageEnView.Paint;
var
  rc: TRect;
  {$IFDEF IEINCLUDEDIRECTSHOW}
  x1, y1, nw, nh: Integer;
  zw, zh: Double;
  {$ENDIF}
begin
  {$ifdef IEDEBUG}
  OutputDebugString('TImageEnView.Paint');
  {$endif}

  try
    if (not HandleAllocated) or ((not HasParentWindow) or (GetClientWidth = 0) or (GetClientHeight = 0)) and not fOffscreenPaint then
      exit; // environment not ready

    if csDesigning in ComponentState then
    begin
      // draws only the background
      IEDrawBackGround(ComponentState, Canvas, nil, fBackgroundStyle, fBackground, 0, 0, Width, Height, 0, 0, 0, 0, fChessboardSize, fChessboardBrushStyle, fGradientEndColor, fWallPaper, fWallPaperStyle);
    end
    else
    begin
      if fLockPaint = 0 then
      begin
        // ready to draw
        if fFullUpdateRequest then
          rc := Rect(0, 0, GetClientWidth, GetClientHeight)
        else
        begin
          rc := Canvas.ClipRect;
          if (rc.Right = 0) or (rc.bottom = 0) then
          begin
            rc.Right := GetClientWidth;
            rc.Bottom := GetClientHeight;
          end;
        end;
        PaintRect(rc);
        if Assigned(fOnDrawCanvas) then
          FOnDrawCanvas(self, Canvas, Rect(0, 0, GetClientWidth, GetClientHeight));
      end;
      fFullUpdateRequest := False;
      if not fRectMoving then
        AniPolyFunc(self, false);
      if assigned(fOnPaint) then
        OnPaint(self);
    end;
  except
    {$IFDEF IEDEBUG}
    on E:Exception do
      OutputDebugString(PAnsiChar('[Paint] ' + e.message + #13#10));
    {$ENDIF}
  end;

  if fDrawVersion then
    DoDrawVersion;

  if (fInteractionHint<>'') and (fEnableInteractionHints) then
    IEDrawHint2(Canvas, fInteractionHintX, fInteractionHintY, fInteractionHint, fInteractionHintMinText);

  {$IFDEF IEINCLUDEDIRECTSHOW}
  // synchronize directshow video rendering with current layer
  if assigned(fImageEnIO) and assigned(fImageEnIO.DShowParams) and fImageEnIO.DShowParams.RenderVideo and (fImageEnIO.DShowParams.State<>gsStopped) then
  begin
    with CurrentLayer.DrawingInfo do
    begin
      fImageEnIO.DShowParams.GetVideoRenderNativeSize(nw, nh);
      if (nw > 0) and (nh > 0) and (CurrentLayer.Bitmap.Width > 0) and (CurrentLayer.Bitmap.Height > 0) then
      begin
        zw := CurrentLayer.Bitmap.Width/nw;
        zh := CurrentLayer.Bitmap.Height/nh;
        x1 := trunc(XSrc/zw);
        y1 := trunc(YSrc/zh);
        nw := trunc(WidthSrc/zw);
        nh := trunc(HeightSrc/zh);
        fImageEnIO.DShowParams.SetVideoRenderRect( Rect(x1, y1, x1+nw, y1+nh), Rect(XDst, YDst, XDst+WidthDst, YDst+HeightDst) );
      end;
    end;
  end;
  {$ENDIF}

  if fAutoCursors and fFirstTimeSetCursor then
  begin
    SetCursor( Cursor );
    fFirstTimeSetCursor := false;
  end;
end;

procedure TImageEnView.TerminatePolySelection(shift: Boolean; removeLastPoint: Boolean);
begin
  if removeLastPoint then
    AnimPolygonRemoveLastPoint(fHPolySel);
  if (not shift) or (not fEnableShiftKey) then
    fSelectionMask.Empty;
  EndSelect;
  UpdateReason := ieurSelectionChanged;
  Update;
  fPolySelecting := False;
  DoSelectionChange;
end;

procedure TImageEnView.DoDoubleClickEx(Shift: boolean);
begin
  if fPolySelecting and (not fLassoSelecting) then
    TerminatePolySelection(Shift, true);
end;

procedure TImageEnView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  {$IFNDEF OCXVERSION}
  if (csReading in ComponentState) or (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if assigned(fIEBitmap) then
    begin
      fIEBitmap.Width := Width;
      fIEBitmap.Height := Height;
      Clear;
    end;
  {$ENDIF}
end;

{!!
<FS>TImageEnView.PolySelPoints

<FM>Declaration<FC>
property PolySelPoints: <A PPointArray>;

<FM>Description<FN>
PolySelPoints returns the current selection as a pointer to an array of TPoint.

<FM>Example<FC>
// Copies current selection to ImageEnView2. Then enhance contrast of ImageEnView2 and copy back to ImageEnView1 (in some selection).
ImageEnView2.CopyFromPolygon(ImageEnView1.Bitmap, ImageEnView2.PolySelPoints^, ImageEnView2.PolySelCount, Point(0, 0));
ImageEnView2.Proc.Contrast(30);
ImageEnView2.CopyToPolygon(ImageEnView1.Bitmap, ImageEnView1.PolySelPoints^, ImageEnView1.PolySelCount, Point(0, 0));
ImageEnView1.Update;
!!}
function TImageEnView.GetPolySelArray: PPointArray;
begin
  result := PIEAnimPoly(fHPolySel)^.Poly;
end;

{!!
<FS>TImageEnView.PolySel


<FM>Declaration<FC>
property PolySel[idx: Integer]: TPoint; (Read-only)

<FM>Description<FN>
Returns the idx point of the current polygonal selection. The point is specified in bitmap coordinates.

See also: <A TImageEnView.PolySelCount>.
!!}
function TImageEnView.GetPolySel(idx: integer): TPoint;
begin
  result := Point(0, 0);
  with PIEAnimPoly(fHPolySel)^ do
    if (idx >= 0) and (idx <= PolyCount) then
    begin
      if Poly^[idx].X = IESELBREAK then
        result := Poly^[idx] // break code
      else
        result := Point(Poly^[idx].X, Poly^[idx].Y);
    end;
end;

{!!
<FS>TImageEnView.PolySelCount

<FM>Declaration<FC>
property PolySelCount: Integer; (Read-only)

<FM>Description<FN>
Returns the points number of the current polygonal selection (size of <A TImageEnView.PolySel> array).

See also: <A TImageEnView.PolySel>.
!!}
function TImageEnView.GetPolySelCount: integer;
begin
  result := PIEAnimPoly(fHPolySel)^.PolyCount;
end;

{!!
<FS>TImageEnView.Deselect

<FM>Declaration<FC>
procedure Deselect;

<FM>Description<FN>
This method removes current selection. After Deselect is called, the <A TImageEnView.Selected> property will be false.
!!}
procedure TImageEnView.DeSelect;
begin
  if fSel then
  begin
    fSel := false;
    HideSelectionEx(true);
  end;
  AnimPolygonClear(fHPolySel);
  fSelectionMask.Empty;
  fSel := false;
  fPolySelecting := false;
  fLassoSelecting := false;
  fRectSelecting := False;
  fCircSelecting := False;
  fRectResizing := ieNone;
  fSelectMoving := -1;
  RestoreCursor;
  UpdateReason := ieurSelectionChanged;
  Update;
end;


{!!
<FS>TImageEnView.Select

<FM>Declaration<FC>
procedure Select(x1, y1, x2, y2: integer; Op: <A TIESelOp> = iespReplace); virtual;

<FM>Description<FN>
This method selects a portion of the image.
The selected area will be outlined with an animated rectangle.
If <A TImageEnView.SelectionBase> is <FC>iesbClientArea<FN> (default) all coordinates depend on actual zoom and scrolling.
If <FC>SelectionBase<FN> is <FC>iesbBitmap<FN>, all coordinates refer to bitmap pixels.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>x1<FN></C> <C>Top-left horizontal position</C> </R>
<R> <C><FC>y1<FN></C> <C>Top-left vertical position</C> </R>
<R> <C><FC>x2<FN></C> <C>Bottom-right horizontal position. Last column not selected</C> </R>
<R> <C><FC>y2<FN></C> <C>Bottom-right vertical position. Last row not selected</C> </R>
<R> <C><FC>Op<FN></C> <C>Selection operation (add or replace selection). If <FC>Op<FN> is <FC>iespReplace<FN>, <FC>Select<FN> replaces all previous selections. If Op is <FC>iespAdd<FN>, <FC>Select<FN> adds a new selection</C> </R>
</TABLE>

<FC>x2, y2<FN> represent the right/bottom margin (not selected). To select an area of 1x1 pixels we need to select using 0,0,1,1 (the column 1 and the row 1 aren't selected).

<FM>Example<FC>
// increase contrast of bitmap rectangle (0,0,49,49)
ImageEnView1.SelectionBase := iesbBitmap;
ImageEnView1.Select(0, 0, 50, 50);
ImageEnView1.Proc.Contrast(10);
ImageEnView1.DeSelect;

<FM>See Also<FN>
- <A TImageEnView.SelectEllipse>
- <A TImageEnView.SelectMagicWand>
- <A TImageEnView.SelectRoundRect>
- <A TImageEnView.InvertSelection>
- <A TImageEnView.Deselect>
!!}
// x2,y2 represent the right/bottom margin (not selected)
// to select an area of 1x1 pixels we need to select using 0,0,1,1 (the column 1 and the
// row 1 aren't selected)
procedure TImageEnView.Select(x1, y1, x2, y2: integer; Op: TIESelOp);
begin
  if fIsNavigator then
    LockPaint;
  SelectEx(x1, y1, x2, y2, Op, false, false);
  if Op = iespReplace then
    fSelectionMask.Empty;
  EndSelect;
  if not fIsNavigator then  // 3.0.3: to speed-up scrolling when navigator has a ZoomFilter
    fDBToDraw := true;
  if fIsNavigator then
  begin
    NPUnLockPaint;
    AniPolyFunc(self, true);
    Paint;
    AniPolyFunc(self, true);
  end
  else
    Paint;
end;

{!!
<FS>TImageEnView.SelectEllipse

<FM>Declaration<FC>
procedure SelectEllipse(CenterX, CenterY, Width, Height: Integer; Op: <A TIESelOp> = iespReplace);

<FM>Description<FN>
SelectEllipse makes an elliptical selection, centered at <FC>CenterX<FN> and <FC>CenterY<FN>, and with size specified by <FC>Width<FN> and <FC>Height<FN>.
<FC>Op<FN> specifies whether to add a new selection (<FC>iespAdd<FN>) or replace the current one (<FC>iespReplace<FN>).

<FM>Example<FC>
ImageEnView.SelectEllipse( 100, 100, 20, 20 );

!!}
procedure TImageEnView.SelectEllipse(CenterX, CenterY, Width, Height: integer; Op: TIESelOp);
var
  dx, dy, p, g, xx, yy: integer;
begin
  if fSelectionBase = iesbClientArea then
  begin
    CenterX := CurrentLayer.ConvXScr2Bmp(CenterX);
    CenterY := CurrentLayer.ConvYScr2Bmp(CenterY);
    Width := trunc(Width * f100DZoomX);
    Height := trunc(Height * f100DZoomY);
  end;
  if (Width <= 0) or (Height <= 0) then
    exit;
  fSel := true;
  if (Op = iespAdd) and (PIEAnimPoly(fHPolySel)^.PolyCount > 0) then
    AnimPolygonAddBreak(fHPolySel)
  else
  begin
    AnimPolygonClear(fHPolySel);
  end;
  dx := Width div 2;
  dy := Height div 2;
  p := trunc(2 * pi * imin(dx, dy)) shr 2;
  if p < 3 then
    p := 3;
  for g := 0 to p - 1 do
  begin
    xx := round(CenterX + cos((2 * pi / p) * g) * dx);
    yy := round(CenterY + sin((2 * pi / p) * g) * dy);
    xx := imax(0, imin(fIEBitmap.Width, xx));
    yy := imax(0, imin(fIEBitmap.Height, yy));
    AnimPolygonAddPtEx(fHPolySel, xx, yy);
  end;
  if Op = iespReplace then
    fSelectionMask.Empty;
  EndSelect;
  fDBToDraw := true;
  Paint;
  ShowSelectionEx(true);
end;

{!!
<FS>TImageEnView.SelectRoundRect

<FM>Declaration<FC>
procedure SelectRoundRect(Left, Top, Right, Bottom, RoundWidth, RoundHeight: integer; Op: <A TIESelOp> = iespReplace);

<FM>Description<FN>
Creates a rounded selection.
<FC>Left, Top, Right, Bottom<FN> are the rectangle coordinates.
<FC>RoundWidth<FN> and <FC>RoundHeight<FN> specify the size of the rounding (in pixels).
<FC>Op<FN> specifies whether to add a new selection (<FC>iespAdd<FN>) or replace the current one (<FC>iespReplace<FN>).

<FM>Example<FC>
ImageEnView1.SelectRoundRect(100, 100, 250, 250, 20, 20);

!!}
procedure TImageEnView.SelectRoundRect(Left, Top, Right, Bottom, RoundWidth, RoundHeight: integer; Op: TIESelOp);
var
  nsteps: Integer;
  ptr: ppointarray;
  i: Integer;
  xrect: TRect;

  procedure AddPt(xx, yy: Integer);
  begin
    xx := imax(0, imin(fIEBitmap.Width, xx));
    yy := imax(0, imin(fIEBitmap.Height, yy));
    AnimPolygonAddPtEx(fHPolySel, xx, yy);
  end;

begin
  if fSelectionBase = iesbClientArea then
  begin
    Left := CurrentLayer.ConvXScr2Bmp(Left);
    Top := CurrentLayer.ConvYScr2Bmp(Top);
    Right := CurrentLayer.ConvXScr2Bmp(Right);
    Bottom := CurrentLayer.ConvYScr2Bmp(Bottom);
    RoundWidth := trunc(RoundWidth * f100DZoomX);
    RoundHeight := trunc(RoundHeight * f100DZoomY);
  end;
  fSel := true;
  if (Op = iespAdd) and (PIEAnimPoly(fHPolySel)^.PolyCount > 0) then
    AnimPolygonAddBreak(fHPolySel)
  else
    AnimPolygonClear(fHPolySel);

  xrect := rect(Left, Top, Right, Bottom);
  nsteps := RoundWidth * RoundHeight;
  getmem(ptr, sizeof(TPoint) * nsteps);
  try
    // top-left round
    IEBezier2D4Controls(Point(xrect.Left, xrect.Top+RoundHeight), xrect.TopLeft, Point(xrect.Left+RoundWidth, xrect.Top), Point(xrect.Left+RoundWidth, xrect.Top), ptr, nsteps);
    for i := 0 to nsteps-1 do
      AddPt( ptr[i].x, ptr[i].y );
    // top-right round
    IEBezier2D4Controls(Point(xrect.Right, xrect.Top+RoundHeight), Point(xrect.Right, xrect.Top), Point(xrect.Right-RoundWidth, xrect.Top), Point(xrect.Right-RoundWidth, xrect.Top), ptr, nsteps);
    for i := nsteps-1 downto 0 do
      AddPt( ptr[i].x, ptr[i].y );
    // bottom-right round
    IEBezier2D4Controls(Point(xrect.Right, xrect.Bottom-RoundHeight), Point(xrect.Right, xrect.Bottom), Point(xrect.Right-RoundWidth, xrect.Bottom), Point(xrect.Right-RoundWidth, xrect.Bottom), ptr, nsteps);
    for i := 0 to nsteps-1 do
      AddPt( ptr[i].x, ptr[i].y );
    // bottom-left round
    IEBezier2D4Controls(Point(xrect.Left, xrect.Bottom-RoundHeight), Point(xrect.Left, xrect.Bottom), Point(xrect.Left+RoundWidth, xrect.Bottom), Point(xrect.Left+RoundWidth, xrect.Bottom), ptr, nsteps);
    for i := nsteps-1 downto 0 do
      AddPt( ptr[i].x, ptr[i].y );
  finally
    freemem(ptr);
  end;

  if Op = iespReplace then
    fSelectionMask.Empty;
  EndSelect;
  fDBToDraw := true;
  Paint;
  ShowSelectionEx(true);
end;

procedure AdjustRectangle(var x1, y1, x2, y2: Integer; Width, Height: Integer);
begin
  if x1<0 then
  begin
    x2 := x2-x1;
    x1 := 0;
  end;
  if y1<0 then
  begin
    y2 := y2-y1;
    y1 := 0;
  end;
  if x2>=Width then
  begin
    x1 := x1-(x2-Width+1);
    x2 := Width-1;
  end;
  if y2>=Height then
  begin
    y1 := y1-(y2-Height+1);
    y2 := Height-1;
  end;
end;

// x2, y2 represent the right/bottom margin (not selected)
// to select an area of 1x1 pixels we need to select using 0, 0, 1, 1 (the column 1 and the
// row 1 aren't selected)
// IT DO NOT CALL "EndSelect"
// if adjRect is true, move the rectangle to make it inside the bitmap
procedure TImageEnView.SelectEx(x1, y1, x2, y2: integer; Op: TIESelOp; aspectratio: boolean; adjRect: Boolean);
begin
  if (x1 = x2) or (y1 = y2) then
  begin
    exit;
  end;
  if fSelectionBase = iesbClientArea then
  begin
    x1 := CurrentLayer.ConvXScr2Bmp(x1);
    y1 := CurrentLayer.ConvYScr2Bmp(y1);
    x2 := CurrentLayer.ConvXScr2Bmp(x2);
    y2 := CurrentLayer.ConvYScr2Bmp(y2);
  end;
  fSel := true;
  if (Op = iespAdd) and (PIEAnimPoly(fHPolySel)^.PolyCount > 0) then
    AnimPolygonAddBreak(fHPolySel)
  else
  begin
    AnimPolygonClear(fHPolySel);
  end;
  if (Op = iespReplace) then
    fSelectionMask.Empty;
  if aspectratio and (fSelectionAspectRatio = -1) then
    y2 := round(x2 - x1 + 1 + y1 - 1)
  else
  if fSelectionAspectRatio > 0 then
  begin
    y2 := y1 + round((x2 - x1) * fSelectionAspectRatio);
    if y2>=fIEBitmap.Height then
      x2 := round( (fIEBitmap.Height-1-y1+x1*fSelectionAspectRatio)/fSelectionAspectRatio );
    if y2<0 then
      x2 := round( (0-y1+x1*fSelectionAspectRatio)/fSelectionAspectRatio );
  end;
  if adjRect then
    AdjustRectangle(x1, y1, x2, y2, fIEBitmap.Width, fIEBitmap.Height);

  OrdCor(x1, y1, x2, y2);

  x1 := imax(0, imin(fIEBitmap.Width - 1, x1));
  y1 := imax(0, imin(fIEBitmap.Height - 1, y1));
  x2 := imax(0, imin(fIEBitmap.Width - 0, x2));
  y2 := imax(0, imin(fIEBitmap.Height - 0, y2));

  AnimPolygonAddPtEx(fHPolySel, x1, y1);
  AnimPolygonAddPtEx(fHPolySel, x2, y1);
  AnimPolygonAddPtEx(fHPolySel, x2, y2);
  AnimPolygonAddPtEx(fHPolySel, x1, y2);
  ShowSelectionEx(true);
end;

{!!
<FS>TImageEnView.SelectCustom

<FM>Declaration<FC>
procedure SelectCustom;

<FM>Description<FN>

Call SelectCustom after you have specified a pixmap of selected pixels of the image.
To select a pixmap use <A TImageEnView.SelectionMask> methods like <A TIEMask.SetPixel>.

<FM>Example<FC>
// only pixels at 10,10 and 15,15 are selected
ImageEnView.SelectionMask.SetPixel(10, 10, 1);
ImageEnView.SelectionMask.SetPixel(15, 15, 1);
ImageEnView.SelectCustom();

<FM>See Also<FN>
- <A TImageEnView.SelectionMask>
!!}
procedure TImageEnView.SelectCustom;
begin
  fSel := not fSelectionMask.IsEmpty;
  ShowSelectionEx(true);
  fDBToDraw := true;
  Paint;
end;

{!!
<FS>TImageEnView.SelectMagicWand

<FM>Declaration<FC>
procedure SelectMagicWand(x, y: Integer; Op: <A TIESelOp>);

<FM>Description<FN>
SelectMagicWand selects an irregular region that has similar colors.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>x<FN></C> <C>Starting horizontal coordiante</C> </R>
<R> <C><FC>y<FN></C> <C>Starting vertical coordinate</C> </R>
<R> <C><FC>Op<FN></C> <C>If <FC>Op<FN> is <FC>iespReplace<FN> the region replaces the existing selection, otherwise if <FC>Op<FN> is <FC>iespAdd<FN>, the region is appended to the existing selection</C> </R>
</TABLE>


<FM>Example<FC>
ImageEnView1.SelectMagicWand(10, 10, iespReplace);
!!}
procedure TImageEnView.SelectMagicWand(x, y: integer; Op: TIESelOp);
var
  pts: ppointarray;
  npt, q: integer;
  xx, yy: integer;
begin
  if fSelectionBase = iesbClientArea then
  begin
    x := CurrentLayer.ConvXScr2Bmp(x);
    y := CurrentLayer.ConvYScr2Bmp(y);
  end;
  if (x < 0) or (x > fIEBitmap.Width) or (y < 0) or (y > fIEBitmap.Height) then
    exit;
  case fMagicWandMode of
    iewInclusive: 
      begin
        pts := _MakeMagicWandPoints(fIEBitmap, x, y, fMagicWandMaxFilter, fMagicWandTolerance, npt);
        if npt > 0 then
        begin
          fSel := true;
          if (Op = iespAdd) and (PIEAnimPoly(fHPolySel)^.PolyCount > 0) then
            AnimPolygonAddBreak(fHPolySel)
          else
          begin
            AnimPolygonClear(fHPolySel);
            fSelectionMask.Empty;
          end;
          for q := 0 to npt - 1 do
          begin
            xx := imin(imax(0, pts^[q].x), fIEBitmap.Width);
            yy := imin(imax(0, pts^[q].y), fIEBitmap.Height);
            AnimPolygonAddPtEx(fHPolySel, xx, yy);
          end;
          freemem(pts);
          if Op = iespReplace then
            fSelectionMask.Empty;
          EndSelect;
          ShowSelectionEx(true);
        end;
      end;
    iewExclusive:
      begin
        if (Op = iespReplace) then
        begin
          AnimPolygonClear(fHPolySel);
          fSelectionMask.Empty;
        end;
        _MakeMagicWandPointsEx(fIEBitmap, x, y, fMagicWandMaxFilter, fMagicWandTolerance, fSelectionMask, fSelectionIntensity);
        fSel := not fSelectionMask.IsEmpty;
        ShowSelectionEx(true);
      end;
    iewGlobal:
      begin
        if (Op = iespReplace) then
        begin
          AnimPolygonClear(fHPolySel);
          fSelectionMask.Empty;
        end;
        _MakeMagicWandPointsEx2(fIEBitmap, x, y, fMagicWandTolerance, fSelectionMask, fSelectionIntensity);
        fSel := not fSelectionMask.IsEmpty;
        ShowSelectionEx(true);
      end;
  end;
  fDBToDraw := true;
  Paint;
end;

{!!
<FS>TImageEnView.SelectColors

<FM>Declaration<FC>
procedure SelectColors(StartColor, FinalColor: <A TRGB>; Op: <A TIESelOp> = iespReplace);
procedure SelectColors(Color: <A TRGB>; Op: <A TIESelOp> = iespReplace);
procedure SelectColors(ColorIndex: integer; Op: <A TIESelOp> = iespReplace);

<FM>Description<FN>
Selects all colors inside the range <FC>StartColor<FN> up to <FC>FinalColor<FN>, or specific color or a color index.
If <FC>Op<FN> is <FC>iespReplace<FN> the region replaces the existing selection, otherwise if <FC>Op<FN> is <FC>iespAdd<FN>, the region is appended to the existing selection.

Third overload accepts <FC>ColorIndex<FN>, which is the index of a color in image colormap: the bitmat pixelformat must be ie8p (palette).

<FM>Example<FC>
// select from 200,200,200 to 255,255,255
ImageEnView1.SelectColors(CreateRGB(200, 200, 200), CreateRGB(255, 255, 255));

// select only color 255,0,0 (pure red)
ImageEnView1.SelectColors(CreateRGB(255, 0, 0));

// select color index 5 of a paletted bitmap (ie8p)
ImageEnView1.SelectColors(5);
!!}
procedure TImageEnView.SelectColors(StartColor, FinalColor: TRGB; Op: TIESelOp);
var
  i, j: integer;
  ww, hh: integer;
  r1, g1, b1, r2, g2, b2: byte;
  p_rgb: PRGB;
begin
  if (Op = iespReplace) then
  begin
    AnimPolygonClear(fHPolySel);
    fSelectionMask.Empty;
  end;

  r1 := StartColor.r;
  g1 := StartColor.g;
  b1 := StartColor.b;

  r2 := FinalColor.r;
  g2 := FinalColor.g;
  b2 := FinalColor.b;

  ww := fIEBitmap.Width;
  hh := fIEBitmap.Height;
  case fIEBitmap.PixelFormat of
    ie24RGB:
      begin
        for i := 0 to hh - 1 do
        begin
          p_rgb := fIEBitmap.ScanLine[i];
          for j := 0 to ww - 1 do
          begin
            with p_rgb^ do
              if (r >= r1) and (g >= g1) and (b >= b1) and (r <= r2) and (g <= g2) and (b <= b2) then
                fSelectionMask.SetPixel(j, i, fSelectionIntensity);
            inc(p_rgb);
          end;
        end;
      end;
    else
    begin
      for i := 0 to hh - 1 do
        for j := 0 to ww - 1 do
          with fIEBitmap.Pixels[j, i] do
            if (r >= r1) and (g >= g1) and (b >= b1) and (r <= r2) and (g <= g2) and (b <= b2) then
              fSelectionMask.SetPixel(j, i, fSelectionIntensity);
    end;
  end;

  fSel := not fSelectionMask.IsEmpty;
  ShowSelectionEx(true);

  fDBToDraw := true;
  Paint;
end;

procedure TImageEnView.SelectColors(Color: TRGB; Op: TIESelOp);
var
  i, j: integer;
  ww, hh: integer;
  rr, gg, bb: byte;
  p_rgb: PRGB;
begin
  if (Op = iespReplace) then
  begin
    AnimPolygonClear(fHPolySel);
    fSelectionMask.Empty;
  end;

  rr := Color.r;
  gg := Color.g;
  bb := Color.b;

  ww := fIEBitmap.Width;
  hh := fIEBitmap.Height;
  case fIEBitmap.PixelFormat of
    ie24RGB:
      begin
        for i := 0 to hh - 1 do
        begin
          p_rgb := fIEBitmap.ScanLine[i];
          for j := 0 to ww - 1 do
          begin
            with p_rgb^ do
              if (r = rr) and (g = gg) and (b = bb) then
                fSelectionMask.SetPixel(j, i, fSelectionIntensity);
            inc(p_rgb);
          end;
        end;
      end;
    else
    begin
      for i := 0 to hh - 1 do
        for j := 0 to ww - 1 do
          with fIEBitmap.Pixels[j, i] do
            if (r = rr) and (g = gg) and (b = bb) then
              fSelectionMask.SetPixel(j, i, fSelectionIntensity);
    end;
  end;

  fSel := not fSelectionMask.IsEmpty;
  ShowSelectionEx(true);

  fDBToDraw := true;
  Paint;
end;

procedure TImageEnView.SelectColors(ColorIndex: integer; Op: TIESelOp);
var
  i, j: integer;
  ww, hh: integer;
begin
  if fIEBitmap.PixelFormat <> ie8p then
    exit;
  if (Op = iespReplace) then
  begin
    AnimPolygonClear(fHPolySel);
    fSelectionMask.Empty;
  end;

  ww := fIEBitmap.Width;
  hh := fIEBitmap.Height;
  for i := 0 to hh - 1 do
    for j := 0 to ww - 1 do
      if fIEBitmap.Pixels_ie8[j, i] = ColorIndex then
          fSelectionMask.SetPixel(j, i, fSelectionIntensity);

  fSel := not fSelectionMask.IsEmpty;
  ShowSelectionEx(true);

  fDBToDraw := true;
  Paint;
end;


procedure AdjustRectWithRatio(var x1, y1, x2, y2: Integer; lx1, ly1, lx2, ly2: Integer; Width, Height: Integer; z: Double);
begin
  z := 1/z;
  if y2>=Height then
    x2 := round( (Height-1-y1+x1*z)/z )
  else
  if y1<0 then
    x1 := round( (-y2+0+x2*z)/z )
  else
  if x2>=Width then
    y2 := round( y1+(Width-1-x1)*z )
  else
  if x1<0 then
    y1 := round( y2-(x2-0)*z );
end;

// Resize selection
// newx, newy: new point in client coords
// return new grip if changed
//
// grip:  0=left-top  1=right-top  2=right-bottom  3=left-bottom
// 4=left side
// 5=right side
// 6=top side
// 7=bottom side
function TImageEnView.SelectResizeEx(grip: TIEGrip; newx, newy: integer; aspectratio: boolean): TIEGrip;
var
  x1, y1, x2, y2, w, h: integer;
  z: double;
begin
  z := 1;
  result := grip;
  with PIEAnimPoly(fHPolySel)^ do
    if _IsRectangle(Poly, PolyCount) then
    begin
      newx := CurrentLayer.Convxscr2bmp(newx);
      newy := CurrentLayer.Convyscr2bmp(newy);
      if aspectratio then
      begin
        w := flx2 - flx1 + 1;
        h := fly2 - fly1 + 1;
        if h = 0 then
          exit;
        if fSelectionAspectRatio>0 then
          z := 1/fSelectionAspectRatio
        else
          z := w / h;
        case grip of
          ieTopLeft: // left-top
            while true do
            begin
              newy := round((-flx2 + newx - 1 + z * fly2 + z) / z);
              if newy >= 0 then
                break;
              inc(newx);
            end;
          ieTopRight: // right-top
            while true do
            begin
              newy := round((-newx + flx1 - 1 + z * fly2) / z);
              if newy >= 0 then
                break;
              dec(newx);
            end;
          ieBottomRight: // right-bottom
            newy := round((newx - flx1 + 1 + z * fly1 - z) / z);
          ieBottomLeft: // left-bottom
            while true do
            begin
              newy := round((flx2 - newx + 1 + fly1 * z + z) / z);
              if newy<=fIEBitmap.Height then
                break;
              inc(newx);
            end;
          ieLeftSide: // left side
            begin
              while true do
              begin
                newy := round((-flx2 + newx - 1 + z * fly2 + z) / z);
                if newy >= 0 then
                  break;
                inc(newx);
              end;
              grip := ieTopLeft;
            end;
          ieRightSide: // right side
            begin
              newy := round((newx - flx1 + 1 + z * fly1 - z) / z);
              grip := ieBottomRight;
            end;
          ieTopSide: // top side
            begin
              while true do
              begin
                newx := round(-z * fly2 + z * newy - z + flx2 + 1);
                if newx >= 0 then
                  break;
                inc(newy);
              end;
              grip := ieTopLeft;
            end;
          ieBottomSide: // bottom side
            begin
              while true do
              begin
                newx := round(z * newy - z * fly1 + z + flx1 - 1);
                if newx<=fIEBitmap.Width then
                  break;
                dec(newy);
              end;
              grip := ieBottomRight;
            end;
        end;
      end;

      if (grip = ieTopRight) or (grip = ieBottomRight) or (grip = ieRightSide) then
        inc(newx);
      if (grip = ieBottomRight) or (grip = ieBottomLeft) or (grip = ieBottomSide) then
        inc(newy);

      x1 := RX1;
      y1 := RY1;
      x2 := RX2;
      y2 := RY2;
      case grip of
        ieTopLeft, ieBottomLeft, ieLeftSide: // left
          begin
            x1 := newx;
            if x1 = x2 then
              dec(x1);
          end;
        ieTopRight, ieBottomRight, ieRightSide: // right
          begin
            x2 := newx;
            if x1 = x2 then
              inc(x2);
          end;
      end;
      case grip of
        ieTopLeft, ieTopRight, ieTopSide: // top
          begin
            y1 := newy;
            if y1 = y2 then
              dec(y1);
          end;
        ieBottomRight, ieBottomLeft, ieBottomSide: // bottom
          begin
            y2 := newy;
            if y1 = y2 then
              inc(y2);
          end;
      end;

      if x1 > x2 then
      begin
        if (result = ieTopLeft) then
          result := ieTopRight // from left-top to right-top
        else
        if (result = ieBottomLeft) then
          result := ieBottomRight // from left-bottom to right-bottom
        else
        if (result = ieTopRight) then
          result := ieTopLeft // from right-top to left-top
        else
        if (result = ieBottomRight) then
          result := ieBottomLeft // from right-bottom to left-bottom
        else
        if (result = ieLeftSide) then
          result := ieRightSide // from left side to right side
        else
        if (result = ieRightSide) then
          result := ieLeftSide; // from right side to left side
      end;
      if y1 > y2 then
      begin
        if (result = ieTopRight) then
          result := ieBottomRight // from right-top to right-bottom
        else
        if (result = ieTopLeft) then
          result := ieBottomLeft // from left-top to left-bottom
        else
        if (result = ieBottomRight) then
          result := ieTopRight // from right-bottom to right-top
        else
        if (result = ieBottomLeft) then
          result := ieTopLeft // from left-bottom to left-top
        else
        if (result = ieTopSide) then
          result := ieBottomSide // from top side to bottom side
        else
        if (result = ieBottomSide) then
          result := ieTopSide; // from bottom side to top side
      end;
      AnimPolygonClear(fHPolySel);
      OrdCor(x1, y1, x2, y2);

      if aspectratio then
        AdjustRectWithRatio(x1, y1, x2, y2, flx1, fly1, flx2, fly2, fIEBitmap.Width, fIEBitmap.Height, z);

      x1 := imax(0, imin(fIEBitmap.Width, x1));
      y1 := imax(0, imin(fIEBitmap.Height, y1));
      x2 := imax(0, imin(fIEBitmap.Width, x2));
      y2 := imax(0, imin(fIEBitmap.Height, y2));
      OrdCor(x1, y1, x2, y2);
      AnimPolygonAddPtEx(fHPolySel, x1, y1);
      AnimPolygonAddPtEx(fHPolySel, x2, y1);
      AnimPolygonAddPtEx(fHPolySel, x2, y2);
      AnimPolygonAddPtEx(fHPolySel, x1, y2);
      ShowSelectionEx(true);
    end;
end;

// Move selection
// fMoving must be 0
// ox, oy are moving offsets
function TImageEnView.SelectMoveEx(fMoving, ox, oy: integer; cutBorders: Boolean): integer;
begin
  result := fMoving;
  with PIEAnimPoly(fHPolySel)^ do
    fRectMoving := _IsRectangle(Poly, PolyCount);
  if not fRectMoving then
  begin
    fSelectionMask.TranslateBitmap(ox, oy, cutBorders); // modify ox and oy
  end
  else
  begin
    fSelectionMask.Empty; // moving a rectangle, full handled by AnimPolygonMove
  end;
  if (ox = 0) and (oy = 0) then
    exit;
  AnimPolygonMove(fHPolySel, ox, oy, fIEBitmap.Width, fIEBitmap.Height, cutBorders);
  AniPolyFunc(self, true);
  if fIsNavigator then
  begin
    Paint;
    AniPolyFunc(self, true);
  end;
end;

{!!
<FS>TImageEnView.MoveSelection

<FM>Declaration<FC>
procedure MoveSelection(OffsetX, OffsetY: Integer);

<FM>Description<FN>
MoveSelection moves the current selection by the specified horizontal and vertical offsets.

<FM>Example<FC>
ImageEnView.MoveSelection(-5, -5);
!!}
procedure TImageEnVIew.MoveSelection(OffsetX, OffsetY: integer);
begin
  SelectMoveEx(0, OffsetX, OffsetY, iesoCutBorders in fSelectionOptions);
  EndSelect;
end;


{!!
<FS>TImageEnView.AddSelPoint

<FM>Declaration<FC>
procedure AddSelPoint(x, y: Integer);

<FM>Description<FN>
AddSelPoint adds a point to the current polygonal selection.
If <A TImageEnView.SelectionBase> is <FC>iesbClientArea<FN> (default), all coordinates depend upon actual zoom and scrolling.
Otherwise, if <FC>SelectionBase<FN> is <FC>iesbBitmap<FN> all coordinates refer to bitmap pixels.

Use <A TImageEnView.EndSelect> to terminate selection by code.
!!}
// add a point to current selection
// To deselect use Deselect
procedure TImageEnView.AddSelPoint(x, y: integer);
begin
  AddSelPointEx(x, y);
  fDBToDraw := true;
  Paint;
end;

// Same of AddSelPoint
// add a point to current selection
// To deselect use Deselect
// DO NOT CALL "EndSelect"
procedure TImageEnView.AddSelPointEx(x, y: integer);
begin
  if fSelectionBase = iesbClientArea then
  begin
    x := CurrentLayer.ConvXScr2Bmp(x);
    y := CurrentLayer.ConvYScr2Bmp(y);
    x := imax(0, imin(fIEBitmap.Width, x));
    y := imax(0, imin(fIEBitmap.Height, y));
  end;
  AnimPolygonAddPt(fHPolySel, x, y);
  ShowSelectionEx(true);
end;


{!!
<FS>TImageEnView.DelLastSelPoint

<FM>Declaration<FC>
procedure DelLastSelPoint();

<FM>Description<FN>
Removes the last inserted selection point.

<FM>Example<FC>
// Pressing ESC removes last polygon point (when MouseInteract = [miSelectPolygon])
procedure TForm1.ImageEnVect1KeyPress(Sender: TObject; var Key: Char);
begin
  if key = #27 then
    ImageEnView1.DelLastSelRegion();
end;
!!}
procedure TImageEnView.DelLastSelPoint();
begin
  if PIEAnimPOly(fHPolySel)^.PolyCount > 1 then
  begin
    AnimPolygonRemoveLastPoint(fHPolySel);
    fDBToDraw := true;
    Paint;
  end;
end;



{!!
<FS>TImageEnView.AddSelBreak

<FM>Declaration<FC>
procedure AddSelBreak;

<FM>Description<FN>
AddSelBreak ends the current selection and begins a new selection.
!!}
// add a "break" to insert a new selection
procedure TImageEnView.AddSelBreak;
begin
  AddSelBreakEx;
end;

// add a "break" to insert a new selection
// DO NOT CALL "EndSelect"
procedure TImageEnView.AddSelBreakEx;
begin
  if PIEAnimPoly(fHPolySel)^.PolyCount > 0 then
    AnimPolygonAddBreak(fHPolySel);
end;

// Show current selection
// d=true : draw now
// d=false : draw at next timer tick
procedure TImageEnView.ShowSelectionEx(d: boolean);
begin
  PIEAnimPoly(fHPolySel)^.Enabled := True;
  if d then
    AniPolyFunc(self, true);
end;

// Hides current selection
// dd=true : remove selection performing a Paint
// dd=false : do not remove selection now
procedure TImageEnView.HideSelectionEx(dd: boolean);
begin
  PIEAnimPoly(fHPolySel)^.Enabled := False;
  if dd and (fLockPaint = 0) then
    Paint;
end;

procedure TImageEnView.SetVisibleSelection(vv: boolean);
begin
  fVisibleSelection := vv;
  if vv then
    ShowSelectionEx(true)
  else
    HideSelectionEx(true);
  UpdateReason := ieurSelectionChanged;
  Update;
end;

{!!
<FS>TImageEnView.VisibleSelection

<FM>Declaration<FC>
property VisibleSelection: Boolean;

<FM>Description<FN>
When true the current selection is displayed.  This property is still valid when the selection is hidden.
!!}
function TImageEnView.GetVisibleSelection: boolean;
begin
  result := fVisibleSelection;
end;

{!!
<FS>TImageEnView.SelectionOptions

<FM>Declaration<FC>
property SelectionOptions: <A TIESelectionOptions>;

<FM>Description<FN>
Provides access to <L TIESelectionOptions>selection behavior options</L>.

Default: SelectionOptions = [iesoAnimated, iesoSizeable, iesoMoveable, iesoCanScroll];
!!}
procedure TImageEnView.SetSelectionOptions(v: TIESelectionOptions);
begin
  fSelectionOptions := v;
  with PIEAnimPoly(fHPolySel)^ do
  begin
    Animated := iesoAnimated in fSelectionOptions;
    Sizeable := iesoSizeable in fSelectionOptions;
  end;
  UpdateReason := ieurSelectionChanged;
  Update;
end;

// reset vertex number of the animated polygon "ap"

{!!
<FS>TImageEnView.AnimPolygonClear

<FM>Declaration<FC>
procedure AnimPolygonClear(ap: pointer);

<FM>Description<FN>
Removes all points of the specified polygon.
!!}
procedure TImageEnView.AnimPolygonClear(ap: pointer);
begin
  with PIEAnimPOly(ap)^ do
  begin
    freemem(Poly);
    GetMem(Poly, 0);
    PolyCount := 0;
    PolyCapacity := 0;
  end;
  if fLockPaint = 0 then
    Paint;
end;

// Removes last added sub-polygon
procedure TImageEnView.AnimPolygonRemoveLast(ap: pointer);
var
  q: integer;
begin
  with PIEAnimPoly(ap)^ do
    for q := PolyCount - 1 downto 0 do
      if (Poly^[q].x = IESELBREAK) or (q = 0) then
      begin
        PolyCount := q;
        if PolyCount = 0 then
          AnimPolygonClear(ap)
        else
        if fLockPaint = 0 then
          Paint;
        break;
      end;
end;

// removes last point
procedure TImageEnView.AnimPolygonRemoveLastPoint(ap: pointer);
begin
  with PIEAnimPoly(ap)^ do
  begin
    dec(PolyCount);
    while (PolyCount > 0) and (Poly^[PolyCount - 1].x = IESELBREAK) do
      dec(PolyCount);
  end;
end;

// Add a "break" in the animated polygon
procedure TImageEnView.AnimPolygonAddBreak(ap: pointer);
var
  tmp: PPointArray;
begin
  with PIEAnimPoly(ap)^ do
  begin
    if PolyCount = PolyCapacity then
    begin
      PolyCapacity := imax(PolyCapacity*2, PolyCount+1);
      GetMem(tmp, sizeof(TPoint) * PolyCapacity);
      if PolyCount > 0 then
      begin
        CopyMemory(tmp, Poly, sizeof(TPoint) * PolyCount);
        FreeMem(Poly);
      end;
      Poly := tmp;
    end;
    if (PolyCount > 0) and (Poly^[PolyCount - 1].x = IESELBREAK) then
      exit;
    Poly^[PolyCount] := Point(IESELBREAK, IESELBREAK);
    inc(PolyCount);
  end;
end;

// add a vertex to animated polygon "ap", doesn't paint
procedure TImageEnView.AnimPolygonAddPtEx(ap: pointer; x, y: integer);
var
  tmp: PPointArray;
begin
  // add a point
  with PIEAnimPoly(ap)^ do
  begin
    if PolyCount = PolyCapacity then
    begin
      PolyCapacity := imax(PolyCapacity*2, PolyCount+1);
      GetMem(tmp, sizeof(TPoint) * PolyCapacity);
      if PolyCount > 0 then
      begin
        CopyMemory(tmp, Poly, sizeof(TPoint) * PolyCount);
        FreeMem(Poly);
      end;
      Poly := tmp;
    end;
    Poly^[PolyCount] := Point(x, y);
    inc(PolyCount);
    // calc rectangle
    if PolyCount = 2 then
    begin
      // only two points
      RX1 := Poly^[0].x;
      RY1 := Poly^[0].y;
      RX2 := Poly^[1].x;
      RY2 := Poly^[1].y;
      OrdCor(RX1, RY1, RX2, RY2);
    end
    else
    if PolyCount > 2 then
    begin
      // more than two points (R__ are already valid)
      RX1 := imin(RX1, x);
      RY1 := imin(RY1, y);
      RX2 := imax(RX2, x);
      RY2 := imax(RY2, y);
    end;
  end;
  fSel := true;
end;

procedure TImageEnView.AnimPolygonEnabled(ap: pointer; Value: Boolean);
begin
  with PIEAnimPoly(ap)^ do
    Enabled := Value;
  AniPolyFunc(self, true);
end;

// move the polygon
procedure TImageEnView.AnimPolygonMove(ap: pointer; ox, oy: integer; max_x, max_y: integer; CutSel: boolean);
var
  q: integer;
  procedure LimitOXOY;
  begin
    with PIEAnimPoly(ap)^ do
    begin
      if (ox + RX1) < 0 then
        dec(ox, (ox + RX1));
      if (ox + RX2) >= max_x then
        dec(ox, (ox + RX2 - max_x + 0));
      if (oy + RY1) < 0 then
        dec(oy, (oy + RY1));
      if (oy + RY2) >= max_y then
        dec(oy, (oy + RY2 - max_y + 0));
    end;
  end;
begin
  if (ox = 0) and (oy = 0) then
    exit;
  if fLockPaint = 0 then
    Paint;
  with PIEAnimPoly(ap)^ do
  begin
    if not CutSel then
    begin
      LimitOXOY;
      if RX1 + ox >= max_x then
        exit;
      if RY1 + oy >= max_y then
        exit;
      if RX2 + ox <= 0 then
        exit;
      if RY2 + oy <= 0 then
        exit;
    end
    else
    begin
      if RX1 + ox >= max_x then
        LimitOXOY;
      if RY1 + oy >= max_y then
        LimitOXOY;
      if RX2 + ox <= 0 then
        LimitOXOY;
      if RY2 + oy <= 0 then
        LimitOXOY;
    end;
    RX1 := ilimit(RX1 + ox, 0, max_x);
    RY1 := ilimit(RY1 + oy, 0, max_y);
    RX2 := ilimit(RX2 + ox, 0, max_x);
    RY2 := ilimit(RY2 + oy, 0, max_y);
    for q := 0 to PolyCount - 1 do
      with Poly^[q] do
        if x <> IESELBREAK then
        begin
          x := ilimit(x + ox, 0, max_x);
          y := ilimit(y + oy, 0, max_y);
        end;
  end;
end;


{!!
<FS>TImageEnView.AnimPolygonAddPt

<FM>Declaration<FC>
procedure AnimPolygonAddPt(ap: pointer; x, y: Integer);

<FM>Description<FN>
Adds a new point to the polygon <FC>ap<FN>.
<FC>x, y<FN> are the point coordinates (bitmap based).
!!}
// add a vertex to the polygon "ap"
// x and y are bitmap based coordinate
procedure TImageEnView.AnimPolygonAddPt(ap: pointer; x, y: integer);
begin
  AnimPolygonAddPtEx(ap, x, y);
  if fLockPaint = 0 then
    Paint;
end;


{!!
<FS>TImageEnView.AnimPolygonNew

<FM>Declaration<FC>
function TImageEnView.AnimPolygonNew(VColor1, VColor2: TColor; VAnimated: Boolean; VSizeable: Boolean): pointer;

<FM>Description<FN>
Creates a new animated polygon.

VColor1 and VColor2 specify the two colors of animation.
Set VAnimated to True to animate the polygon
Set VSizeable to True to allow resizing of the polygon.

Returns an index of the new polygon.

!!}
// Create a new animated polygon
// return the polygon handle
function TImageEnView.AnimPolygonNew(VColor1, VColor2: TColor; VAnimated: boolean; VSizeable: boolean): pointer;
var
  pp: PIEAnimPoly;
begin
  getmem(pp, sizeof(TIEAnimPoly));
  fAnimPoly.Add(pp);
  with pp^ do
  begin
    PolyCount := 0;
    PolyCapacity := 0;
    getmem(Poly, 0);
    Color1 := VColor1;
    Color2 := VColor2;
    Animated := VAnimated;
    Sizeable := VSizeable;
    AniFt := 0;
    if not (csDesigning in ComponentState) then
      Canvas := self.Canvas;
    Enabled := True;
    RX1 := 0;
    RY1 := 0;
    RX2 := 0;
    RY2 := 0;
  end;
  if (fAnimPoly.Count = 1) and not (csDesigning in ComponentState)
      {$ifndef IEDOTNETVERSION} // 3.0.3
      and (assigned(Owner) or (ParentWindow<>0))
      {$endif}
      then
  begin
    SetupDrawPixelBitmap;
    SetupAniPolyTimer;
    fAnimPolyTimer.Enabled := True;
  end;
  pp^.DrawPixelPtr := fDrawPixelPtr;
  pp^.DrawPixelBitmap := fDrawPixelBitmap;
  result := pp;
end;


{!!
<FS>TImageEnView.AnimPolygonDel

<FM>Declaration<FC>
procedure AnimPolygonDel(ap: pointer);

<FM>Description<FN>
Removes the animated polygon <FC>ap<FN> (value returned from <A TImageEnView.AnimPolygonNew>).
!!}
// removes the animated polygon "ap"
procedure TImageEnView.AnimPolygonDel(ap: pointer);
begin
  if ap <> nil then
  begin
    freemem(PIEAnimPoly(ap)^.Poly);
    freemem(PIEAnimPoly(ap));
    fAnimPoly.Delete(fAnimPoly.IndexOf(pointer(ap)));
    if (fAnimPoly.Count = 0) and assigned(fAnimPolyTimer) then
      fAnimPolyTimer.Enabled := False;
    if fLockPaint = 0 then
      Paint;
  end;
end;


// this makes selection animated
procedure TImageEnView.TimerEvent(Sender: TObject);
begin
  AniPolyFunc(Sender, true);
end;

procedure TImageEnView.DoOnDrawPolygon(polygon: Integer; point: Integer; Canvas: TCanvas; x, y: Integer);
begin
  if assigned(fOnDrawPolygon) then
    fOnDrawPolygon(self, polygon, point, Canvas, x, y);
end;

function TImageEnView.GetSelectionGridSize(): Integer;
begin
  result := imax( fSelectionGridWidth, fSelectionGridHeight );
end;

procedure TImageEnView.SetSelectionGridSize(value: Integer);
begin
  fSelectionGridWidth := value;
  fSelectionGridHeight := value;
end;

procedure TImageEnView.DrawSelectionGrid(x1, y1, x2, y2: Integer);
var
  i: Integer;
  p: Integer;
begin
  if (fSelectionGridWidth > 1) or (fSelectionGridHeight > 1) then
  begin
    OrdCor(x1, y1, x2, y2);
    Canvas.Pen.Color := IEGlobalSettings().SelectionGridColor;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Style := psSolid;
    for i := 1 to fSelectionGridHeight-1 do
    begin
      // horizontal lines
      p := trunc( i*(y2-y1)/fSelectionGridHeight );
      Canvas.MoveTo(x1, y1 + p);
      Canvas.LineTo(x2, y1 + p);
    end;
    for i := 1 to fSelectionGridWidth-1 do
    begin
      // vertical lines
      p := trunc( i*(x2-x1)/fSelectionGridWidth );
      Canvas.MoveTo(x1 + p, y1);
      Canvas.LineTo(x1 + p, y2);
    end;
  end;
end;

procedure LineDDAProc(x, y: integer; self: PIEAnimPoly); stdcall;
begin
  with self^ do
  begin
    // Due to SetPixel bug on Vista - on Window Classic theme
    if Assigned(DrawPixelPtr) then
    begin
      if C1 < 4 then
        DrawPixelPtr^ := TColor2TRGB(Color1)
      else
      if C1 < 7 then
        DrawPixelPtr^ := TColor2TRGB(Color2)
      else
      begin // fC1=6
        DrawPixelPtr^ := TColor2TRGB(Color2);
        C1 := -1;
      end;
      Canvas.Draw(x, y, DrawPixelBitmap);
      inc(C1);
    end;
  end;
end;


// Draw the selection
procedure TImageEnView.AniPolyFunc(Sender: TObject; UseLockPaint: boolean);

  procedure DrawGrip(iec: TIECanvas; x1, y1, x2, y2: Integer);
  begin
    case fGripShape of
      iegsBox:
        iec.Rectangle(x1, y1, x2, y2);
      iegsCircle:
        iec.Ellipse(x1, y1, x2, y2);
    end;
  end;

var
  q, p: integer;
  pp: PIEAnimPoly;
  x1, y1, x2, y2: integer;
  lbreak: TPoint;
  grx1, gry1, grx2, gry2: integer;
  xanimated: boolean;
  xtime: Dword;
  invisible: Boolean;
  iec: TIECanvas;
begin
  if (not HasParentWindow) or (not fVisibleSelection) then
    exit;

  xanimated := false;
  for q := 0 to fAnimPoly.Count - 1 do
  begin
    pp := PIEAnimPoly(fAnimPoly[q]);
    with pp^ do
      if Enabled then
      begin

        if Animated then
          xanimated := true;

        if Animated and (not fRectSelecting) and (not fPolySelecting)
          and (not fCircSelecting) and (Sender is TTimer) and (fRectResizing = ieNone) and (fSelectMoving < 0) then
        begin
          inc(AniFt);
          if AniFt >= 10 then
            AniFt := 0;
          inc(fAniCounter);
        end;

        with Canvas.ClipRect do
          invisible := (Left = 0) and (Top = 0) and (Right = 0) and (Bottom = 0);

        if (not invisible) and (fLockPaint = 0) and ((Canvas.LockCount = 0) or (not UseLockPaint)) and (not fMouseMoveScrolling) then
        begin

          // draw grips
          if Sizeable and _IsRectangle(Poly, PolyCount) and (not fRectSelecting) and (fSelectMoving = -1) then
          begin
            iec := TIECanvas.Create(Canvas, true, true);
            with iec do
            begin
              Brush.Style := fGripBrushStyle;
              Brush.Color := fGripColor2;
              Pen.Color := fGripColor1;
              grx1 := 10000000;
              gry1 := grx1;
              gry2 := -1000000;
              grx2 := gry2;
              for p := 0 to PolyCount - 1 do
              begin
                if Poly^[p].x <> IESELBREAK then
                begin
                  x1 := CurrentLayer.ConvXBmp2Scr(Poly^[p].x);
                  y1 := CurrentLayer.ConvYBmp2Scr(Poly^[p].y);
                  DrawGrip(iec, x1 - fGripSize, y1 - fGripSize, x1 + fGripSize, y1 + fGripSize);
                  if x1 < grx1 then
                    grx1 := x1;
                  if y1 < gry1 then
                    gry1 := y1;
                  if x1 > grx2 then
                    grx2 := x1;
                  if y1 > gry2 then
                    gry2 := y1;
                end;
              end;
              if fExtendedGrips then
              begin
                x1 := (grx2 + grx1) div 2;
                y1 := (gry2 + gry1) div 2;
                DrawGrip(iec, x1 - fGripSize, gry1 - fGripSize, x1 + fGripSize, gry1 + fGripSize);
                DrawGrip(iec, grx1 - fGripSize, y1 - fGripSize, grx1 + fGripSize, y1 + fGripSize);
                DrawGrip(iec, grx2 - fGripSize, y1 - fGripSize, grx2 + fGripSize, y1 + fGripSize);
                DrawGrip(iec, x1 - fGripSize, gry2 - fGripSize, x1 + fGripSize, gry2 + fGripSize);
              end;
            end;
            iec.Free;
            DrawSelectionGrid(grx1, gry1, grx2, gry2);
          end; // end draw grips


          if (pp = fHPolySel) and (not fSelectionMask.IsEmpty) and (fRectResizing = ieNone) and (not fRectSelecting)
            and (not fCircSelecting) and (not fPolySelecting) and (not fRectMoving) then
          begin
            // draw selection
            if (not fDelayDisplaySelection) or (fStable2 = 0) then
            begin
              xtime := GetTickCount;
              with CurrentLayer.DrawingInfo do
                fSelectionMask.DrawOutline(Canvas, XDst, YDst, WidthDst, HeightDst, XSrc, YSrc, WidthSrc, HeightSrc, fAniCounter, fSelColor1, fSelColor2, @fNavigatorActualRect);
              xtime := GetTickCount - xtime;
              if (fDelayTimer<0) and (assigned(Owner) or (ParentWindow<>0)) then
              begin
                SetupAniPolyTimer;
                fAnimPolyTimer.Interval := imax(210, trunc(xtime*100/(-fDelayTimer)));
              end;
            end;
          end
          else
          begin
            C1 := AniFt;
            Canvas.pen.mode := pmCopy;
            // draw user polygon or building selection
            if PolyCount > 0 then
            begin
              if _IsRectangle(Poly, PolyCount) then
              begin
                // this is a rectangle
                x1 := CurrentLayer.ConvXBmp2Scr(Poly^[0].x);
                y1 := CurrentLayer.ConvYBmp2Scr(Poly^[0].y);
                x2 := CurrentLayer.ConvXBmp2Scr(Poly^[2].x) - 1;
                y2 := CurrentLayer.ConvYBmp2Scr(Poly^[2].y) - 1;
                fNavigatorActualRect := Rect(x1, y1, x2, y2);
                // 3.0.4: rectangular selection painted here to avoid grips missalignment

                LineDDA(x1, y1, x2, y1, @LineDDAProc, NativeInt(pp));
                DoOnDrawPolygon(q, 1, Canvas, x2, y1);

                LineDDA(x2, y1, x2, y2, @LineDDAProc, NativeInt(pp));
                DoOnDrawPolygon(q, 2, Canvas, x2, y2);

                LineDDA(x2, y2, x1, y2, @LineDDAProc, NativeInt(pp));
                DoOnDrawPolygon(q, 3, Canvas, x1, y2);

                LineDDA(x1, y1, x1, y2, @LineDDAProc, NativeInt(pp));

                if fRectSelecting or fRectMoving then
                  DrawSelectionGrid(x1, y1, x2, y2);

                DoOnDrawPolygon(q, 0, Canvas, x1, y1);
              end
              else
              begin
                // this is a polygon
                lbreak := Poly^[0];
                p := 1;
                while p < PolyCount do
                begin
                  if Poly^[p].x = IESELBREAK then
                  begin
                    // closes sub-polygon
                    x1 := CurrentLayer.ConvXBmp2Scr(Poly^[p - 1].x);
                    y1 := CurrentLayer.ConvYBmp2Scr(Poly^[p - 1].y);
                    x2 := CurrentLayer.ConvXBmp2Scr(lbreak.x);
                    y2 := CurrentLayer.ConvYBmp2Scr(lbreak.y);
                    LineDDA(x1, y1, x2, y2, @LineDDAProc, NativeInt(pp));
                    DoOnDrawPolygon(q, p-1, Canvas, x1, y1);
                    inc(p);
                    lbreak := Poly^[p];
                  end
                  else
                  begin
                    x1 := CurrentLayer.ConvXBmp2Scr(Poly^[p - 1].x);
                    y1 := CurrentLayer.ConvYBmp2Scr(Poly^[p - 1].y);
                    x2 := CurrentLayer.ConvXBmp2Scr(Poly^[p].x);
                    y2 := CurrentLayer.ConvYBmp2Scr(Poly^[p].y);
                    LineDDA(x1, y1, x2, y2, @LineDDAProc, NativeInt(pp));
                    DoOnDrawPolygon(q, p-1, Canvas, x1, y1);
                  end;
                  inc(p);
                end;
                // closes last sub-polygon
                if (not fPolySelecting) then
                begin
                  x1 := CurrentLayer.ConvXBmp2Scr(Poly^[PolyCount - 1].x);
                  y1 := CurrentLayer.ConvYBmp2Scr(Poly^[PolyCount - 1].y);
                  x2 := CurrentLayer.ConvXBmp2Scr(lbreak.x);
                  y2 := CurrentLayer.ConvYBmp2Scr(lbreak.y);
                  LineDDA(x1, y1, x2, y2, @LineDDAProc, NativeInt(pp));
                  DoOnDrawPolygon(q, p-1, Canvas, x1, y1);
                end;
              end;
            end;
          end;
        end;
      end;  // end of polygon "enabled"

  end;

  if (assigned(Owner) or (ParentWindow<>0)) then
  begin
    SetupAniPolyTimer;
    if (fStable = 0) and (fStable2 = 0) and (not xanimated) then
      fAnimPolyTimer.Enabled := false
    else
    if (fAnimPolyTimer.Enabled = false) then
      fAnimPolyTimer.Enabled := true;
  end;

  if fStable > 0 then
  begin
    dec(fStable);
    if fStable = 0 then
      Update;
  end;

  if fStable2 > 0 then
    dec(fStable2);
end;

{!!
<FS>TImageEnView.GetGripAt

<FM>Declaration<FC>
function GetGripAt(x, y: Integer): <A TIEGrip>;

<FM>Description<FN>
Returns the selection grip at mouse position specified by <FC>x, y<FN> (or ieNone if a grip is not found).

!!}
function TImageEnView.GetGripAt(x, y: integer): TIEGrip;
var
  p, x1, y1, x2, y2: integer;
begin
  result := ieNone;
  with PIEAnimPoly(fHPolySel)^ do
    if Enabled and Sizeable and _IsRectangle(Poly, PolyCount) and not fRectSelecting then
    begin
      // try grips
      for p := 0 to PolyCount - 1 do
      begin
        if Poly^[p].x <> IESELBREAK then
        begin
          x1 := CurrentLayer.ConvXBmp2Scr(Poly^[p].x);
          y1 := CurrentLayer.ConvYBmp2Scr(Poly^[p].y);
          if IEPointInRect(x, y, x1 - fGripSize, y1 - fGripSize, x1 + fGripSize, y1 + fGripSize) then
          begin
            result := TIEGrip(p + 1);
            break;
          end;
        end;
      end;
      if result = ieNone then
      begin
        // try sides
        x1 := CurrentLayer.ConvXBmp2Scr(RX1);
        y1 := CurrentLayer.ConvYBmp2Scr(RY1);
        x2 := CurrentLayer.ConvXBmp2Scr(RX2);
        y2 := CurrentLayer.ConvYBmp2Scr(RY2);
        if _DistPoint2Seg(x, y, x1, y1, x1, y2) < 3 then
          result := ieLeftSide // left
        else
        if _DistPoint2Seg(x, y, x2, y1, x2, y2) < 3 then
          result := ieRightSide // right
        else
        if _DistPoint2Seg(x, y, x1, y1, x2, y1) < 3 then
          result := ieTopSide // top
        else
        if _DistPoint2seg(x, y, x1, y2, x2, y2) < 3 then
          result := ieBottomSide; // bottom
      end;
    end;
end;

// Return the grip on position x,y
// ieNone if none or Shift is pressed
function TImageEnView.GetResizingGrip(x, y: integer; Shift: TShiftState): TIEGrip;
begin
  if (ssShift in Shift) then
    result := ieNone
  else
    result := GetGripAt(x, y);
end;

// return 0 if x,y is inside selection and shift is not pressed
// -1 otherwise
function TImageEnView.GetMovingGrip(x, y: integer; Shift: TShiftState): integer;
begin
  result := -1;
  if (not (ssShift in Shift)) and (not fSelectionMask.IsEmpty) and (not fRectSelecting) and (not fPolySelecting) and (not fCircSelecting) and
    fSelectionMask.IsPointInside(CurrentLayer.Convxscr2bmp(x), CurrentLayer.Convyscr2bmp(y)) and (iesoMoveable in fSelectionOptions) then
    result := 0;
end;

procedure TImageEnView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lx, ly, i: integer;
  bDisableNewSelections: Boolean;
  {$IFNDEF OCXVERSION}
  ParForm: TCustomForm;
  {$ENDIF}
begin
  inherited;

{$IFDEF OCXVERSION}
  SetFocus;
{$ELSE}
  ParForm := GetParentForm(Self);
  if (ParForm<>nil) and (ParForm.Visible) and CanFocus then
    SetFocus;
{$ENDIF}

  fSaveZoomFilter := fZoomFilter;

  if (Button = mbRight) and (ssLeft in Shift) then
    // right button pressed
    exit;

  fLayerMoved := false;
  bDisableNewSelections := (iesoDisableNewSelection in fSelectionOptions) and (Selected = True);

  if (Button = mbLeft) or (miZoom in fMouseInteract) or ((Button = mbRight) and (iesoRightButtonSelectLayers in fSelectionOptions)) then
  begin

    if (not (miScroll in fMouseInteract)) and fDelayZoomFilter then
      fZoomFilter := rfNone;

    fSavedSelectionBase := fSelectionBase;
    fSelectionBase := iesbClientArea;

    lx := x;
    ly := y;

    fHSX0 := ilimit(lx, fOffX, fOffX + fExtX);
    fHSY0 := ilimit(ly, fOffY, fOffY + fExtY);

    x := ilimit(lx, CurrentLayer.ClientAreaBox.Left, CurrentLayer.ClientAreaBox.Right+1);
    y := ilimit(ly, CurrentLayer.ClientAreaBox.Top, CurrentLayer.ClientAreaBox.Bottom+1);

    fMouseDownX := lx;
    fMouseDownY := ly;
    fPredLX := lx;
    fPredLY := ly;
    fPredX := x;
    fPredY := y;
    fHSX1 := x;
    fHSY1 := y;
    fHSVX1 := ViewX;
    fHSVY1 := ViewY;
    fMMoveX := x;
    fMMoveY := y;
    fStartingPolyCount := PIEAnimPoly(fHPolySel)^.PolyCount;
    if (Button = mbLeft) and ((miSelectPolygon in fMouseInteract) or (miSelectLasso in fMouseInteract)) then
    begin
      LayersFixRotations;
      DoBeforeSelectionChange;
      fSelectMoving := GetMovingGrip(x, y, Shift);
      if (fSelectMoving = -1) and (bDisableNewSelections = False) then
      begin
        if (ssAlt in Shift) or fForceALTkey then
          with PIEAnimPoly(fHPolySel)^ do
            if (PolyCount > 0) and (Poly^[PolyCount - 1].x <> IESELBREAK) then
              _CastPolySelCC(Poly^[PolyCount - 1].x - fViewX + fOffX, Poly^[PolyCount - 1].y - fViewY + fOffY, x, y);
        if (not fPolySelecting) then
        begin
          if (ssShift in Shift) and (fEnableShiftKey) then
            AddSelBreakEx
          else
          if (iesoDisableOneClickDeselect in fSelectionOptions) = False then
            DeSelect;
        end;
        AddSelPointEx(X, Y);
        fPolySelecting := true;
        fLassoSelecting := miSelectLasso in fMouseInteract;
        DoSelectionChanging;
      end;
    end
    else
    if (Button = mbLeft) and (miSelect in fMouseInteract) then
    begin
      LayersFixRotations;
      DoBeforeSelectionChange;
      fRectResizing := GetResizingGrip(lx, ly, Shift);
      if fRectResizing = ieNone then
        fSelectMoving := GetMovingGrip(lx, ly, Shift);
      if (fRectResizing = ieNone) and (fSelectMoving = -1) and (bDisableNewSelections = False) then
      begin
        // new rectangular selection
        fRectSelecting := True;
        if (fSelectionAspectRatio = 0) then
        begin
          // fixed selection
          MouseSelectRectangle(x, y, Shift);
        end;
        DoSelectionChanging;
      end;
      if fRectResizing <> ieNone then
        with PIEAnimPoly(fHPolySel)^ do
        begin
          flx1 := RX1;
          flx2 := RX2;
          fly1 := RY1;
          fly2 := RY2;
        end;
    end
    else
    if (Button = mbLeft) and (miSelectZoom in fMouseInteract) then
    begin
      fRectSelecting := True;
      DoSelectionChanging;
      if miSelectZoom in fMouseInteract then
        SaveSelection;
    end
    else
    if (Button = mbLeft) and (miSelectCircle in fMouseInteract) then
    begin
      LayersFixRotations;
      DoBeforeSelectionChange;
      fSelectMoving := GetMovingGrip(x, y, Shift);
      if (fSelectMoving = -1) and (bDisableNewSelections = False) then
      begin
        fCircSelecting := True;
        ShowSelectionEx(false);
        if (fSelectionAspectRatio = 0) then
        begin
          // fixed selection
          MouseSelectCircle(x, y, Shift);
        end;
        DoSelectionChanging;
      end;
    end
    else
    if (Button = mbLeft) and (miSelectMagicWand in fMouseInteract) then
    begin
      LayersFixRotations;
      DoBeforeSelectionChange;
      fSelectMoving := GetMovingGrip(x, y, Shift);
    end
    else
    if ((Button = mbLeft) or ((Button=mbRight) and (iesoRightButtonSelectLayers in fSelectionOptions)))
      and (((miMoveLayers in fMouseInteract) or (miResizeLayers in fMouseInteract) or (miRotateLayers in fMouseInteract))) then
    begin
      fMovResRotLayerStarted := false;
      if miResizeLayers in fMouseInteract then
        fLayerResizing := FindLayerGrip(lx, ly)
      else
        fLayerResizing := ieNone;
      if (fLayerResizing <> ieNone) then
      begin
        LayersFixRotations;
        with TIELayer(fLayers[fLayersCurrent]) do
        begin
          fLyrResizingClientAreaBox := ClientAreaBox;
          for i := 0 to 3 do
            fLyrResizingRotatedBox[i] := DrawingInfo.RotatedDest[i];
        end;
        DoLayerNotify(fLayersCurrent, ielBeginResizing);
      end;
      if (fLayerResizing = ieNone) and ((miMoveLayers in fMouseInteract) or (miRotatelayers in fMouseInteract)) then
      begin
        if (miRotateLayers in fMouseInteract) and (FindLayerGrip(lx, ly) = ieRotationCenter) then
          // begin rotation center moving
          fMovingRotationCenter := LayersCurrent
        else
        begin
          if (miRotateLayers in fMouseInteract) and (LayersCurrent > -1) and not IsPointInsideLayer(lx, ly, LayersCurrent) then
          begin
            // start rotating current layer
            fRotatingLayer := LayersCurrent;
            fRotatingLayerValue := CurrentLayer.Rotate;
            if fLayersRotationDelayFilterOnPreview then
              fStable := fStableReset;
          end
          else
          begin
            // begin layer moving
            fMovingLayer := FindLayerAt(lx, ly);
            if fMovingLayer > -1 then
            begin
              SetLayersCurrent(fMovingLayer);
              DoLayerNotify(fLayersCurrent, ielSelected);
              DoLayerNotify(fLayersCurrent, ielBeginMoving);
            end;
            if (fMovingLayer > -1) and (not TIELayer(fLayers[fLayersCurrent]).Locked) then
              SetTempCursor(1790, true);
          end;
        end;
      end;
    end;
    if (Button = mbLeft) and (miScroll in fMouseInteract) and RequiresScrollBars then // warning, not follows the previous if..then...else
      SetTempCursor(1782, True);
    fSelectionBase := fSavedSelectionBase;
  end;
end;

// Called by MouseMoveScroll, when it is scrolled the view (ViewXY)
// of scx and scy pixels.
procedure TImageEnView.SubMouseMoveScroll(scx, scy: integer);
begin
  dec(fMMoveX, scx);
  dec(fMMoveY, scy);
  dec(fPredX, scx);
  dec(fPredY, scy);
  dec(fPredLX, scx);
  dec(fPredLY, scy);
end;

// calls SubMouseMoveScroll method
// scroll if the mouse is out of the component
function TImageEnView.MouseMoveScroll: boolean;
var
  pt: Tpoint;
  scx, scy, lvx, lvy: integer;
begin
  fMouseMoveScrolling := true;
  try
    result := false;
    repeat
      // calc movement
      getcursorpos(pt);
      pt := ScreenToClient(pt);
      if pt.x < fOffX then
        scx := pt.x - fOffX
      else
      if pt.x > (fOffX + fExtX - 1) then
        scx := pt.x - (fOffX + fExtX - 1)
      else
        scx := 0;
      if pt.y < fOffY then
        scy := pt.y - fOffY
      else
      if pt.y > (fOffY + fExtY - 1) then
        scy := pt.y - (fOffY + fExtY - 1)
      else
        scy := 0;
      if (scx = 0) and (scy = 0) then
      begin
        break; // EXIT POINT!
      end;
      lvx := fViewX;
      lvy := fViewY;
      SetViewXY(fViewX + scx, fViewY + scy);
      if (lvx = fViewX) and (lvy = fViewY) then
      begin
        break; // EXIT POINT!
      end;
      result := true;
      dec(lvx, fViewX);
      dec(lvy, fViewY);
      inc(fHSX1, lvx);
      inc(fHSY1, lvy);
      inc(fHSX0, lvx);
      inc(fHSY0, lvy);
      SubMouseMoveScroll(scx, scy);
    until false;
  finally
    fMouseMoveScrolling := false;
  end;
end;

procedure TImageEnView.MouseResizeLayer(clientlx, cliently: integer; AltPressed: Boolean);
var
  w, h, lx, ly: integer;
  layer: TIELayer;
  plx, ply: Integer;
  lr: TIEGrip;
  aspectratio: Boolean;
begin
  layer := CurrentLayer;

  if layer.ResizedWidth = 0 then
    w := layer.Bitmap.Width
  else
    w := layer.ResizedWidth;

  if layer.ResizedHeight = 0 then
    h := layer.Bitmap.Height
  else
    h := layer.ResizedHeight;

  lx  := XScr2Bmp(clientlx);
  ly  := YScr2Bmp(cliently);
  plx := XScr2Bmp(fPredLX);
  ply := YScr2Bmp(fPredLY);

  lr := fLayerResizing;

  case fLayersResizeAspectRatio of
    iearALTKey:
      aspectratio := AltPressed;
    iearAlways:
      aspectratio := true;
    iearAlwaysOnCornerGrip:
      aspectratio := (lr = ieTopLeft) or (lr = ieTopRight) or (lr = ieBottomRight) or (lr = ieBottomLeft);
    else  // iearDisabled (suppress warning)
      aspectratio := false;
  end;

  if aspectratio then
    case lr of
      ieTopLeft: // left-top
        begin
          ly := YScr2Bmp( trunc( (clientlx-fLyrResizingClientAreaBox.Left)*fLayersResizingAR ) +fLyrResizingClientAreaBox.Top );
        end;
      ieTopRight: // right-top
        begin
          ly := YScr2Bmp( trunc( (-clientlx+fLyrResizingClientAreaBox.Right)*fLayersResizingAR ) +fLyrResizingClientAreaBox.Top );
        end;
      ieBottomRight: // right-bottom
        begin
          ly := YScr2Bmp( trunc( (clientlx-fLyrResizingClientAreaBox.Left)*fLayersResizingAR ) +fLyrResizingClientAreaBox.Top );
        end;
      ieBottomLeft: // left-bottom
        begin
          ly := YScr2Bmp( trunc( (-clientlx+fLyrResizingClientAreaBox.Left)*fLayersResizingAR ) +fLyrResizingClientAreaBox.Bottom );
        end;
      ieLeftSide: // left side
        begin
          ly := YScr2Bmp( trunc( (-clientlx+fLyrResizingClientAreaBox.Left)*fLayersResizingAR ) +fLyrResizingClientAreaBox.Bottom );
          lr := ieBottomLeft;
        end;
      ieRightSide: // right side
        begin
          ly := YScr2Bmp( trunc( (clientlx-fLyrResizingClientAreaBox.Left)*fLayersResizingAR ) +fLyrResizingClientAreaBox.Top );
          lr := ieBottomRight;
        end;
      ieTopSide: // top side
        begin
          lx := XScr2Bmp( trunc( (cliently-fLyrResizingClientAreaBox.Top)/fLayersResizingAR ) +fLyrResizingClientAreaBox.Left );
          lr := ieTopLeft;
        end;
      ieBottomSide: // bottom side
        begin
          lx := XScr2Bmp( trunc( (cliently-fLyrResizingClientAreaBox.Top)/fLayersResizingAR ) +fLyrResizingClientAreaBox.Left );
          lr := ieBottomRight;
        end;
      else
    end;

  case lr of
    ieTopLeft: // left-top
      begin
        layer.ResizedWidth  := imax(1, w + (layer.PosX-lx) );
        layer.ResizedHeight := imax(1, h + (layer.PosY-ly));
        if layer.ResizedWidth > 1 then
          layer.PosX := lx;
        if layer.ResizedHeight > 1 then
          layer.PosY := ly;
      end;
    ieTopRight: // right-top
      begin
        layer.ResizedWidth  := imax(1, lx-layer.PosX );
        layer.ResizedHeight := imax(1, h + (layer.PosY-ly));
        if layer.ResizedHeight > 1 then
          layer.PosY := ly;
      end;
    ieBottomRight: // right-bottom
      begin
        //if layer.Rotate = 0 then
        //begin
          layer.ResizedWidth  := imax(1, (lx - XScr2Bmp(fLyrResizingClientAreaBox.Left)));
          layer.ResizedHeight := imax(1, (ly - YScr2Bmp(fLyrResizingClientAreaBox.Top)));
        //end
        //else
        //begin
        //  IERotatePoint(lx, ly, -layer.Rotate, trunc(layer.Width*layer.RotateCenterX), trunc(layer.Height*layer.RotateCenterY));
        //  layer.ResizedWidth  := imax(1, (lx - XScr2Bmp(fLyrResizingRotatedBox[0].X)));
        //  layer.ResizedHeight := imax(1, (ly - YScr2Bmp(fLyrResizingRotatedBox[0].Y)));
        //end;
      end;
    ieBottomLeft: // left-bottom
      begin
        layer.ResizedWidth  := imax(1, w + (layer.PosX-lx) );
        layer.ResizedHeight := imax(1, (ly - YScr2Bmp(fLyrResizingClientAreaBox.Top)));
        if layer.ResizedWidth > 1 then
          layer.PosX := lx;
      end;
    ieLeftSide: // left side
      begin
        layer.ResizedWidth := imax(1, w - (lx - plx));
        if layer.ResizedWidth > 1 then
          layer.PosX := layer.PosX + (lx - plx);
      end;
    ieRightSide: // right side
      begin
        layer.ResizedWidth := imax(1, (lx - XScr2Bmp(fLyrResizingClientAreaBox.Left)));
      end;
    ieTopSide: // top side
      begin
        layer.ResizedHeight := imax(1, h + (layer.PosY-ly));
        if layer.ResizedHeight > 1 then
          layer.PosY := ly;
      end;
    ieBottomSide: // bottom side
      begin
        layer.ResizedHeight := imax(1, h - (-ly + ply));
      end;
    else
      exit;
  end;
  if not aspectratio then
    fLayersResizingAR := layer.Height / layer.Width;
end;

procedure TImageEnView.MouseSelectCircle(x, y: integer; Shift: TShiftState);
var
  dx, dy, p, g: integer;
  xx, yy: integer;
begin
  MouseMoveScroll;
  if (fSelectionAspectRatio = 0) then
  begin
    // fixed selection
    dx := round(fSelectionAbsWidth * fZoomD100X) div 2;
    dy := round(fSelectionAbsHeight * fZoomD100Y) div 2;
    fHSX1 := x;
    fHSY1 := y;
    x := x + dx;
    y := y + dy;
  end;
  if (ssShift in Shift) and (fEnableShiftKey) then
  begin
    if fStartingPolyCount <> PIEAnimPoly(fHPolySel)^.PolyCount then
      AnimPolygonRemoveLast(fHPolySel);
    AddSelBreakEx;
  end
  else
    AnimPolygonClear(fHPolySel);
  if ((ssAlt in Shift) or fForceALTkey) and (fSelectionAspectRatio = -1) then
  begin
    // circle (pressing ALT)
    dx := imax(abs(fHSX1 - X), abs(fHSY1 - Y));
    dy := dx;
  end
  else
  if fSelectionAspectRatio>0 then
  begin
    dx := imax(abs(fHSX1 - X), abs(fHSY1 - Y));
    dy := round(dx * fSelectionAspectRatio);
  end
  else
  begin
    dx := abs(fHSX1 - X);
    dy := abs(fHSY1 - Y);
  end;
  dx := trunc(dx * f100DZoomX);
  dy := trunc(dy * f100DZoomY);
  p := trunc(2 * pi * imin(dx, dy));
  if p < 50 then
    p := 50;
  for g := 0 to p - 1 do
  begin
    xx := round(CurrentLayer.ConvXScr2Bmp(fHSX1) + cos((2 * pi / p) * g) * dx);
    yy := round(CurrentLayer.ConvYScr2Bmp(fHSY1) + sin((2 * pi / p) * g) * dy);
    xx := imax(0, imin(fIEBitmap.Width, xx));
    yy := imax(0, imin(fIEBitmap.Height, yy));
    AnimPolygonAddPtEx(fHPolySel, xx, yy);
  end;
  AniPolyFunc(self, true);
  DoSelectionChanging;
end;

procedure TImageEnView.MouseSelectRectangle(x, y: integer; Shift: TShiftState);
var
  dx, dy: integer;
begin
  MouseMoveScroll;
  if (fSelectionAspectRatio = 0) then
  begin
    dx := round(fSelectionAbsWidth * fZoomD100X) div 2;
    dy := round(fSelectionAbsHeight * fZoomD100Y) div 2;
    fHSx1 := x - dx;
    fHsy1 := y - dy;
    x := x + dx - 1;
    y := y + dy - 1;
  end;
  if (ssShift in Shift) and (fEnableShiftKey) then
  begin
    if fStartingPolyCount <> PIEAnimPoly(fHPolySel)^.PolyCount then
      AnimPolygonRemoveLast(fHPolySel);
    SelectEx(fHSx1, fHSy1, x, y, iespAdd, (ssAlt in Shift) or fForceALTkey, not(iesoSizeable in fSelectionOptions) )
  end
  else
    SelectEx(fHSx1, fHSy1, x, y, iespReplace, (ssAlt in Shift) or fForceALTkey, not(iesoSizeable in fSelectionOptions) );
  DoSelectionChanging;
end;

procedure TImageEnView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lx, ly: integer;
  grip: TIEGrip;
  ii: integer;
  layer: TIELayer;
  cx, cy: Integer;
  dx, dy: Double;
  max_x, max_y: Integer;
  ox, oy: Integer;
begin

  inherited;

  fSavedSelectionBase := fSelectionBase;
  fSelectionBase := iesbClientArea;

  lx := x;
  ly := y;

  x := ilimit(lx, CurrentLayer.ClientAreaBox.Left, CurrentLayer.ClientAreaBox.Right+1);
  y := ilimit(ly, CurrentLayer.ClientAreaBox.Top, CurrentLayer.ClientAreaBox.Bottom+1);

  if MouseCapture then
  begin

    // inside Mouse Capture

    if miScroll in fMouseInteract then
    begin
      // panning
      SetViewXY(fHSVX1 - trunc((lx - fMouseDownX)*fMouseScrollRate), fHSVY1 - trunc((ly - fMouseDownY)*fMouseScrollRate) ); // 3.0.2
    end

    else
    if fRectResizing <> ieNone then
    begin
      // resize rectangular selection
      MouseMoveScroll;
      fRectResizing := SelectResizeEx(fRectResizing, x, y, (ssAlt in Shift) or fForceALTkey or (fSelectionAspectRatio>0));
      DoSelectionChanging;
    end

    else
    if fSelectMoving > -1 then
    begin
      // move selection
      if iesoCanScroll in fSelectionOptions then
        MouseMoveScroll;
      fSelectMoving := SelectMoveEx(fSelectMoving,
                                    CurrentLayer.ConvXScr2Bmp(x)-CurrentLayer.ConvXScr2Bmp(fPredX),
                                    CurrentLayer.ConvYScr2Bmp(y)-CurrentLayer.ConvYScr2Bmp(fPredY),
                                    iesoCutBorders in fSelectionOptions);
      DoSelectionChanging;
    end

    else
    if fRectSelecting then
    begin
      // select rectangle
      MouseSelectRectangle(x, y, Shift);
    end

    else
    if fPolySelecting then
    begin
      // select polygon
      MouseMoveScroll;
      Paint;
      PolyDraw1;
      AnimPolygonAddPtEx(fHPolySel, CurrentLayer.ConvXScr2Bmp(x), CurrentLayer.ConvYScr2Bmp(y));
      AniPolyFunc(self, true);
      DoSelectionChanging;
    end

    else
    if fCircSelecting then
    begin
      // select circle
      MouseSelectCircle(x, y, Shift);
    end

    else
    if (fMovingLayer > -1) then
    begin
      // Moving a layer
      layer := TIELayer(fLayers[fMovingLayer]);
      if not layer.Locked then
      begin
        if not fMovResRotLayerStarted and fLayersAutoUndo then
          Proc.SaveUndo(ieuLayer);
        fMovResRotLayerStarted := true;
        MouseMoveScroll;
        ox := Layer.PosX + (XScr2Bmp(lx) - XScr2Bmp(fPredLX));
        oy := Layer.PosY + (YScr2Bmp(ly) - YScr2Bmp(fPredLY));
        layer.PosX := ox;
        layer.PosY := oy;
        fLayerMoved := (ox<>0) or (oy<>0) or fLayerMoved;
        SetInteractionHint('X='+IntToStr(Layer.PosX)+' Y='+IntToStr(Layer.PosY), lx, ly, 'X=000 Y=000');
        fUpdate_MaskCache := fMovingLayer;
        Update;
        DoLayerNotify(fLayersCurrent, ielMoving);
      end;
    end

    else
    if fMovingRotationCenter > -1 then
    begin
      // moving rotation center
      layer := TIELayer(fLayers[fMovingRotationCenter]);
      layer.RotateCenterX := layer.ConvXScr2Bmp(lx) / layer.Bitmap.Width;
      layer.RotateCenterY := layer.ConvYScr2Bmp(ly) / layer.Bitmap.Height;
      SetInteractionHint('X='+IntToStr(trunc(layer.RotateCenterX*layer.Width)) + ' Y='+IntToStr(trunc(layer.RotateCenterY*layer.Height)), lx, ly, 'X=000 Y=000');
      fUpdate_MaskCache := fMovingRotationCenter;
      Update;
    end

    else
    if (fRotatingLayer > -1) then
    begin
      // Rotating a layer
      layer := TIELayer(fLayers[fRotatingLayer]);
      if not layer.Locked then
      begin
        if not fMovResRotLayerStarted and fLayersAutoUndo then
          Proc.SaveUndo(ieuFullLayer);
        fMovResRotLayerStarted := true;
        dx := ((XScr2Bmp(lx) - XScr2Bmp(fPredLX)))/4;
        dy := ((YScr2Bmp(ly) - YScr2Bmp(fPredLY)))/4;
        cx := CurrentLayer.ConvXBmp2Scr( trunc(CurrentLayer.RotateCenterX * CurrentLayer.Bitmap.Width) );
        cy := CurrentLayer.ConvYBmp2Scr( trunc(CurrentLayer.RotateCenterY * CurrentLayer.Bitmap.Height) );
        if lx>cx then
          dy := -dy;
        if ly<cy then
          dx := -dx;
        fRotatingLayerValue := fRotatingLayerValue+dx+dy;
        if (ssShift in Shift) then
          layer.Rotate := trunc(fRotatingLayerValue/fLayersRotateStep)*fLayersRotateStep
        else
          layer.Rotate := fRotatingLayerValue;
        SetInteractionHint(IEFloatToStrS(layer.Rotate)+'°', lx, ly, '0000°');
        fUpdate_MaskCache := fRotatingLayer; 
        if fLayersRotationDelayFilterOnPreview then
          fStable := fStableReset;
        Update;
        DoLayerNotify(fLayersCurrent, ielRotating);
      end;
    end

    else
    if (fLayerResizing <> ieNone) and (not CurrentLayer.Locked) then
    begin
      // Resizing a layer
      if not fMovResRotLayerStarted and fLayersAutoUndo then
        Proc.SaveUndo(ieuLayer);
      if not fMovResRotLayerStarted then
        fLayersResizingAR := CurrentLayer.Height / CurrentLayer.Width;
      fMovResRotLayerStarted := true;
      MouseMoveScroll;
      MouseResizeLayer(lx, ly, (ssAlt in Shift) or fForceALTkey);
      SetInteractionHint(IntToStr(CurrentLayer.Width)+' x '+IntToStr(CurrentLayer.Height), lx, ly, '0000 x 0000');
      fUpdate_MaskCache := fLayersCurrent;
      Update;
      DoLayerNotify(fLayersCurrent, ielResizing);
    end;

    // out of Mouse Capture

  end
  else
  if fPolySelecting and not (fLassoSelecting) then
  begin
    PolyDraw1;
    if (ssAlt in Shift) or fForceALTkey then
      with PIEAnimPoly(fHPolySel)^ do
        if (PolyCount > 0) and (Poly^[PolyCount - 1].x <> IESELBREAK) then
          _CastPolySelCC(Poly^[PolyCount - 1].x - fViewX + fOffX, Poly^[PolyCount - 1].y - fViewY + fOffY, x, y);
    fMMoveX := X;
    fMMoveY := Y;
    PolyDraw1;
    DoSelectionChanging;
  end
  else
  if (miSelect in fMouseInteract) or (miSelectPolygon in fMouseInteract) or
          (miSelectCircle in fMouseInteract) or
          (miSelectMagicWand in fMouseInteract) or (miSelectLasso in fMouseInteract) then
  begin
    grip := GetResizingGrip(lx, ly, Shift);
    DoMouseInResizingGrip(grip);
    if grip = ieNone then
    begin
      ii := GetMovingGrip(lx, ly, Shift);
      if ii > -1 then
        // moving cursor
        SetTempCursor(1790)
      else
      begin
        // default cursor
        if (ssShift in Shift) and fEnableShiftKey then
        begin
          case cursor of
            1785: SetTempCursor(1791);
            1793: SetTempCursor(1794);
          end;
        end
        else
          RestoreCursor;
      end;
    end
    else
    begin
      // resizing cursors
      case grip of
        ieTopLeft,  ieBottomRight: SetTempCursor(1786);
        ieTopRight, ieBottomLeft:  SetTempCursor(1788);
        ieLeftSide, ieRightSide:   SetTempCursor(1789);
        ieTopSide,  ieBottomSide:  SetTempCursor(1787);
      end;
    end;
  end
  else
  if (miRotateLayers in fMouseInteract) then
  begin
    // layers rotation. Rotate only the currently selected layer.
    if IsPointInsideLayer(lx, ly, LayersCurrent) then
      RestoreCursor
    else
    begin
      cx := CurrentLayer.ConvXBmp2Scr( trunc(CurrentLayer.RotateCenterX * CurrentLayer.Bitmap.Width) );
      cy := CurrentLayer.ConvYBmp2Scr( trunc(CurrentLayer.RotateCenterY * CurrentLayer.Bitmap.Height) );
      if (lx < cx) and (ly < cy) then
        SetTempCursor(1814)
      else
      if (lx < cx) and (ly>cy) then
        SetTempCursor(1811)
      else
      if (lx > cx) and (ly < cy) then
        SetTempCursor(1812)
      else
      if (lx > cx) and (ly > cy) then
        SetTempCursor(1813);
    end;
  end
  else
  if ((miResizeLayers in fMouseInteract) or (miMoveLayers in fMouseInteract)) then
  begin
    grip := ieNone;
    if (miResizeLayers in fMouseInteract) and (not TIELayer(fLayers[fLayersCurrent]).Locked) then
      grip := FindLayerGrip(lx, ly);
    if grip = ieNone then
    begin
      ii := FindLayerAt(lx, ly);
      if (ii > -1) and (not TIELayer(fLayers[ii]).Locked) and (miMoveLayers in fMouseInteract) then
        // moving cursor
        SetTempCursor(1790)
      else
        // default cursor
        RestoreCursor;
    end
    else
    begin
      // resizing cursors
      case grip of
        ieTopLeft, ieBottomRight : SetTempCursor(1786);
        ieTopRight, ieBottomLeft : SetTempCursor(1788);
        ieLeftSide, ieRightSide  : SetTempCursor(1789);
        ieTopSide, ieBottomSide  : SetTempCursor(1787);
      end;
    end;
  end;

  if (miMovingScroll in fMouseInteract) and ((fPredLx<>lx) or (fPredLy<>ly)) and not MouseCapture then
  begin
    GetMaxViewXY(max_x, max_y);
    cx := trunc( (imax(imin(ClientWidth -1, lx), 0)/(ClientWidth )-0.02) * 1.05 * (max_x) );
    cy := trunc( (imax(imin(ClientHeight-1, ly), 0)/(ClientHeight)-0.02) * 1.05 * (max_y) );
    SetViewXYSmooth( cx, cy );
  end;

  if fSelectionMask.IsPointInside(CurrentLayer.ConvXScr2Bmp(x), CurrentLayer.ConvYScr2Bmp(y)) then
    DoMouseInSel;
  fMMoveX := x;
  fMMoveY := y;
  fSelectionBase := fSavedSelectionBase;
  fPredX := x;
  fPredY := y;
  fPredLx := lx;
  fPredLy := ly;
end;

{!!
<FS>TImageEnView.IsPointInsideSelection

<FM>Declaration<FC>
function IsPointInsideSelection(x, y: Integer): Boolean;

<FM>Description<FN>
IsPointInsideSelection returns true if the point specified with <FC>x, y<FN> is inside the current selection.
If <A TImageEnView.SelectionBase> is <FC>iesbClientArea<FN> (default) all coordinates depend on the zoom and image view (scrolling).
!!}
function TImageEnView.IsPointInsideSelection(x, y: integer): boolean;
begin
  if fSelectionBase = iesbClientArea then
  begin
    x := CurrentLayer.ConvXScr2Bmp(x);
    y := CurrentLayer.ConvYScr2Bmp(y);
  end;
  result := fSelectionMask.IsPointInside(x, y);
end;

procedure TImageEnView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  NUMSCALES = 8;
  scales: array[1..NUMSCALES] of integer = (2, 5, 10, 25, 33, 50, 75, 100);
var
  w, sl: integer;
  d: double;
  ix, iy: integer;
  callselectionchange: boolean;
begin
  inherited;
  fInteractionHint := '';
  callselectionchange := false;
  ix := ilimit(x, CurrentLayer.ClientAreaBox.Left, CurrentLayer.ClientAreaBox.Right+1); 
  iy := ilimit(y, CurrentLayer.ClientAreaBox.Top, CurrentLayer.ClientAreaBox.Bottom+1);
  fSavedSelectionBase := fSelectionBase;
  fSelectionBase := iesbClientArea;
  d := 0;
  if (miZoom in fMouseInteract) and (fHSx1 = x) and (fHSy1 = y) and not IsEmpty then
  begin
    if Button = mbLeft then
    begin
      if (miScroll in fMouseInteract) then
        RestoreCursor;
      // zoom-in (MOUSE LEFT)
      if Zoom < 100 then
      begin
        for w := 1 to NUMSCALES do
          if scales[w] > Zoom then
          begin
            d := scales[w];
            break;
          end;
      end
      else
        d := Zoom + 100;
      DoZoomIn(d);
      if (d <> fZoomX) or (d<>fZoomY) then
        ZoomAt(x, y, d, false)
    end
    else
    if (Button = mbRight) then
    begin
      // zoom-out (MOUSE RIGHT)
      if Zoom < 200 then
      begin
        d := 2;
        for w := NUMSCALES downto 1 do
          if scales[w] < Zoom then
          begin
            d := scales[w];
            break;
          end;
      end
      else
        d := Zoom - 100;
      DoZoomOut(d);
      if (d <> fZoomX) or (d<>fZoomY) then
        ZoomAt(x, y, d, false);
    end;
    fPolySelecting := false;
  end
  else
  if (Button = mbLeft) and (miScroll in fMouseInteract) and RequiresScrollBars then
  begin
    // set "view", free by MouseCapture
    SetViewXY(fHSVX1 - trunc((x - fMouseDownX)*fMouseScrollRate), fHSVY1 - trunc((y - fMouseDownY)*fMouseScrollRate));  // 3.0.2
    RestoreCursor;
  end
  else
  if (Button = mbLeft) and (miSelectZoom in fMouseInteract) then
  begin
    ZoomSelectionEx(fZoomSelectionAspectRatio, true);
    fRectSelecting := false;
    RestoreSelection(true, iersMoveToAdapt);
  end
  else
  if (Button = mbLeft) and (miSelect in fMouseInteract) then
  begin
    if (fHSX1 = ix) and (fHSY1 = iy) and (fRectResizing = ieNone) and (fSelectMoving = -1) then
    begin
      if fSel and ((iesoDisableOneClickDeselect in fSelectionOptions) = False) then
      begin
        DeSelect;
        callselectionchange := true;
      end;
    end
    else
    begin
      if fRectSelecting or (fRectResizing <> ieNone) then
      begin
        if (not (ssShift in Shift)) or (not fEnableShiftKey) then
          fSelectionMask.Empty;
        EndSelect;
      end;
      fRectSelecting := false;
      fRectResizing := ieNone;
      fSelectMoving := -1;
      callselectionchange := true;
      if fIsNavigator then
        invalidate
      else
      begin
        UpdateReason := ieurSelectionChanged;
        Update;
      end;
    end;
  end
  else
  if (Button = mbLeft) and (miSelectCircle in fMouseInteract) then
  begin
    if (fHSX1 = ix) and (fHSY1 = iy) and (fSelectMoving = -1) and (fSelectionAspectRatio <> 0) then
    begin
      if (iesoDisableOneClickDeselect in fSelectionOptions) = False then
        DeSelect
    end
    else
    begin
      if fCircSelecting then
      begin
        if (not (ssShift in Shift)) or (not fEnableShiftKey) then
          fSelectionMask.Empty;
        EndSelect;
      end;
      fCircSelecting := False;
      fSelectMoving := -1;
      callselectionchange := true;
      UpdateReason := ieurSelectionChanged;
      Update;
    end;
  end
  else
  if (Button = mbLeft) and (miSelectLasso in fMouseInteract) then
  begin
    if (fHSX1 = ix) and (fHSY1 = iy) and (fSelectMoving = -1) then
    begin
      if (iesoDisableOneClickDeselect in fSelectionOptions) = False then
      DeSelect
    end
    else
    begin
      if fLassoSelecting then
      begin
        if (not (ssShift in Shift)) or (not fEnableShiftKey) then
          fSelectionMask.Empty;
        EndSelect;
      end;
      fLassoSelecting := false;
      fPolySelecting := false;
      fSelectMoving := -1;
      callselectionchange := true;
      UpdateReason := ieurSelectionChanged;
      Update;
    end;
  end
  else
  if (Button = mbLeft) and (miSelectPolygon in fMouseInteract) then
  begin
    if (fSelectMoving > -1) then
    begin
      fSelectMoving := -1;
      callselectionchange := true;
    end;
    UpdateReason := ieurSelectionChanged;
    Update;
  end
  else
  if (Button = mbLeft) and (miSelectMagicWand in fMouseInteract) then
  begin
    if fSelectMoving > -1 then
    begin
      fSelectMoving := -1;
      callselectionchange := true;
    end
    else
    if NOT ((iesoDisableNewSelection in fSelectionOptions) and Selected) then
    begin
      SetTempCursor(crHourGlass);
      if (ssShift in Shift) and fEnableShiftKey then
        SelectMagicWand(x, y, iespAdd)
      else
        SelectMagicWand(x, y, iespReplace);
      RestoreCursor;
      callselectionchange := true;
    end;
    UpdateReason := ieurSelectionChanged;
    Update;
  end;

  if Button = mbLeft then
  begin
    if fRectMoving then
    begin
      fSelectionMask.Empty;
      EndSelect;
      fRectMoving := false;
    end;

    if fMovingLayer > -1 then
    begin
      fMovingLayer := -1;
      RestoreCursor;
      if fLayerMoved then
        DoLayerNotify(fLayersCurrent, ielMoved);
      fLayerMoved := false;
      invalidate;
    end;

    if fRotatingLayer > -1 then
    begin
      fRotatingLayer := -1;
      if (fHSX1 <> ix) or (fHSY1 <> iy) then
        DoLayerNotify(fLayersCurrent, ielRotated);
      invalidate;
    end
    else
    if (fMovingRotationCenter > -1) then
    begin
      fMovingRotationCenter := -1;
    end;
    if (miRotateLayers in fMouseInteract) and (fHSX1=ix) and (fHSY1=iy) then
    begin
      // just select a layer
      sl := FindLayerAt(X, Y);
      if sl>-1 then
      begin
        SetLayersCurrent(sl);
        DoLayerNotify(fLayersCurrent, ielSelected);
      end;
    end;

    if fLayerResizing <> ieNone then
    begin
      fLayerResizing := ieNone;
      if (fHSX1 <> ix) or (fHSY1 <> iy) then
        DoLayerNotify(fLayersCurrent, ielResized);
      Update; // needed to update magnify-resized layer
    end;

    fSelectMoving := -1;
    fRectResizing := ieNone;
    fRectSelecting := false;
    fCircSelecting := false;
    fLassoSelecting := false;

    if callselectionchange then
      DoSelectionChange;
  end;

  if (Button = mbRight) and (iesoRightButtonTerminatePolySelect in fSelectionOptions) then
    TerminatePolySelection(ssShift in Shift, false);

  fZoomFilter := fSaveZoomFilter;
  fSelectionBase := fSavedSelectionBase;
end;

// Called whenever the zoom or viewx/y changes.
// It calls fOnViewChange
procedure TImageEnView.ViewChange(c: integer);
begin
  if assigned(fOnViewChange) then
    fOnViewChange(self, c);

  if assigned(fNavigator) then
    SetNavigatorRect;
end;

procedure TImageEnView.ViewChanging(c: integer; newValue: double);
begin
  if assigned(fOnViewChanging) then
    fOnViewChanging(self, c, newValue);
end;

// draw background on Width and Height area
procedure IEDrawBackground(ComponentState: TComponentState; Canvas: TCanvas; Bitmap: TBitmap; fBackgroundStyle: TIEBackgroundStyle; fBackGround: TColor; DestX, DestY, Width, Height: integer; x1, y1, x2, y2: integer; fChessboardSize: integer; fChessboardBrushStyle: TBrushStyle; fGradientEndColor: TColor; WallPaper: TPicture; WallPaperStyle: TIEWallPaperStyle);
const
  BS: array[iebsSolid..iebsDiagCross] of TBrushStyle = (bsSolid, bsHorizontal, 
    bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
var
  x, y, i1, i2: integer;
  b1, b2: boolean;
  rc: TRect;
  px: PRGB;
  c: array [0..1] of TRGB;
  hh: Integer;
begin
  with Canvas do
    case fBackgroundStyle of
      iebsGradient:
        IEDrawGradient(Rect(DestX, DestY, width + DestX, height + DestY), Canvas.handle, fBackGround, fGradientEndColor, true);
      iebsSolid..iebsDiagCross:
        begin
          Brush.Color := fBackGround;
          Brush.Style := BS[fBackgroundStyle];
          FillRect(rect(DestX, DestY, Width + DestX, Height + DestY));
        end;
      iebsChessBoard, iebsPhotoLike:
        begin
          if fBackgroundStyle = iebsPhotoLike then
          begin
            IEDrawBackground(ComponentState, Canvas, Bitmap, iebsSolid, $B0B0B0, DestX, DestY, Width, Height, x1, y1, x2, y2, fChessboardSize, fChessboardBrushStyle, fGradientEndColor, WallPaper, WallPaperStyle);
            Canvas.Pen.Color := clBlack;
            Canvas.Pen.Mode := pmCopy;
            Canvas.Pen.Width := 1;
            Canvas.Pen.Style := psSolid;
            Canvas.Brush.Style := bsClear;
            Canvas.Rectangle(x1-1, y1-1, x2+1, y2+1);
            DestX := x1;
            DestY := y1;
            Width := x2-x1;
            Height := y2-y1;
          end;
          if (Bitmap<>nil) and (fChessboardBrushStyle=bsSolid) and (Bitmap.PixelFormat=pf24bit) then
          begin
            c[0] := TColor2TRGB(fBackground);
            c[1] := TColor2TRGB(not fBackground);
            b1 := false;
            i2 := 0;
            hh := Bitmap.Height;
            for y := DestY to DestY+Height-1 do
            begin
              b2 := b1;
              if y<0 then
                px := Bitmap.Scanline[0]
              else
              if y>=hh then
                px := Bitmap.Scanline[hh-1]
              else
                px := Bitmap.Scanline[y];
              inc(px, DestX);
              i1 := 0;
              for x := 0 to Width-1 do
              begin
                px^ := c[integer(b2)];
                if i1=fChessboardSize then
                begin
                  b2 := not b2;
                  i1 := 0;
                end;
                inc(i1);
                inc(px);
              end;
              if i2=fChessboardSize then
              begin
                b1 := not b1;
                i2 := 0;
              end;
              inc(i2);
            end;
          end
          else
          begin
            Brush.Style := fChessboardBrushStyle;
            y := DestY;
            b1 := false;
            while y < Height + DestY do
            begin
              b2 := b1;
              b1 := not b1;
              x := DestX;
              while x < Width + DestX do
              begin
                if b2 then
                  Brush.Color := fBackground
                else
                  Brush.Color := not fBackground;
                b2 := not b2;
                FillRect(rect(x, y, imin(x + fChessboardSize, DestX + Width), imin(y + fChessboardSize, DestY + Height)));
                inc(x, fChessboardSize);
              end;
              inc(y, fChessboardSize);
            end;
          end;
        end;
      iebsDiagonals:
        begin
          Brush.Color := fBackGround;
          Brush.Style := bsSolid;
          FillRect(rect(DestX, DestY, Width + DestX, Height + DestY));
          Pen.Color := not fBackground;
          Pen.Style := psSolid;
          moveto(DestX, DestY);
          lineto(Width + DestX, Height + DestY);
          moveto(Width + DestX, DestY);
          lineto(DestX, Height + DestY);
        end;
      iebsCropped:
        begin
          if (csDesigning in ComponentState) then
          begin
            Brush.Color := fBackGround;
            Brush.Style := bsSolid;
            FillRect(rect(DestX, DestY, Width + DestX, Height + DestY));
          end
          else
          begin
            Brush.Color := clBtnFace;
            Brush.Style := bsSolid;
            FillRect(rect(DestX, DestY, Width + DestX, Height + DestY));
            if ((x2 - x1) > 1) and ((y2 - y1) > 1) then
            begin
              rc := rect(x1 - IEGlobalSettings().EdgeX, y1 - IEGlobalSettings().EdgeY, x2 + IEGlobalSettings().EdgeX, y2 + IEGlobalSettings().EdgeY);
              DrawEdge(Canvas.Handle, rc, EDGE_SUNKEN, BF_RECT);
            end;
          end;
        end;
      iebsCropShadow:
        begin
          if (csDesigning in ComponentState) then
          begin
            Brush.Color := fBackGround;
            Brush.Style := bsSolid;
            FillRect(rect(DestX, DestY, Width + DestX, Height + DestY));
          end
          else
          begin
            Brush.Color := fBackGround;
            Brush.Style := bsSolid;
            FillRect(rect(DestX, DestY, Width + DestX, Height + DestY));
            if ((x2 - x1) > 1) and ((y2 - y1) > 1) then
            begin
              IERightShadow(Canvas, Bitmap, x2, y1, x2 + 5, y2 + 3, iestSmooth1, fBackground);
              IEBottomShadow(Canvas, Bitmap, x1, y2, x2 + 4, y2 + 5, iestSmooth1, fBackground);
            end;
          end;
        end;
      iebsSoftShadow:
        begin
          if (csDesigning in ComponentState) then
          begin
            Brush.Color := fBackGround;
            Brush.Style := bsSolid;
            FillRect(rect(DestX, DestY, Width + DestX, Height + DestY));
          end
          else
          begin
            if assigned(Bitmap) then
            begin
              Brush.Color := fBackGround;
              Brush.Style := bsSolid;
              FillRect(rect(DestX, DestY, Width + DestX, Height + DestY));
              if ((x2 - x1) > 1) and ((y2 - y1) > 1) then
                IERectShadow(Bitmap, x1, y1, x2, y2, fBackground);
            end;
          end;
        end;
    end;
  // draw wallpaper
  if assigned(WallPaper) and assigned(WallPaper.Graphic) then
  begin
    case WallPaperStyle of
      iewoNormal:
        begin
          Canvas.Draw(0, 0, WallPaper.Graphic);
        end;
      iewoStretch:
        begin
          Canvas.StretchDraw(rect(0, 0, Width, Height), WallPaper.Graphic);
        end;
      iewoTile:
        begin
          x1 := 0;
          y1 := 0;
          while (y1 < Height) do
          begin
            Canvas.Draw(x1, y1, WallPaper.Graphic);
            inc(x1, WallPaper.Graphic.Width);
            if x1 >= Width then
            begin
              x1 := 0;
              inc(y1, WallPaper.Graphic.Height);
            end;
          end;
        end;
    end;
  end;
end;

// fo1x, fo1y, fo2x, fo2y , frx, fry
procedure TImageEnView.CalcPaintCoordsEx(var XSrc, YSrc, SrcWidth, SrcHeight: integer; var DstWidth, DstHeight: integer; tViewX, tViewY: integer);
var
  rr: double;
begin
  XSrc := 0;
  SrcWidth := 0;
  YSrc := 0;
  SrcHeight := 0;
  if fZZWW <> 0 then
  begin
    rr := fMaxLayerWidth / fZZWW;
    XSrc := round(QuantizeViewX(tViewX) * rr);
    SrcWidth := round(fExtx * rr);
    if (XSrc + SrcWidth) > fMaxLayerWidth then
      dec(SrcWidth);
  end;
  if fZZHH <> 0 then
  begin
    rr := fMaxLayerHeight / fZZHH;
    YSrc := round(QuantizeViewY(tViewY) * rr);
    SrcHeight := round(fExty * rr);
    if (YSrc + SrcHeight) > fMaxLayerHeight then
      dec(SrcHeight);
  end;
  if fZoomX > 100 then
  begin
    DstWidth := trunc(SrcWidth * fZoomD100X);
    if (DstWidth < fExtX) and (XSrc + SrcWidth + 1 <= fMaxLayerWidth) then
    begin
      inc(SrcWidth);
      DstWidth := trunc(SrcWidth * fZoomD100X);
    end;
  end
  else
    DstWidth := fExtX;
  if fZoomY>100 then
  begin
    DstHeight := trunc(SrcHeight * fZoomD100Y);
    if (DstHeight < fExtY) and (YSrc + SrcHeight + 1 <= fMaxLayerHeight) then
    begin
      inc(SrcHeight);
      DstHeight := trunc(SrcHeight * fZoomD100Y);
    end;
  end
  else
    DstHeight := fExtY;
end;

procedure TImageEnView.CalcPaintCoords;
begin
  CalcPaintCoordsEx(fo1x, fo1y, fo2x, fo2y, frx, fry, fViewX, fViewY);
end;

{!!
<FS>TImageEnView.ZoomFilter

<FM>Declaration<FC>
property ZoomFilter: <A TResampleFilter>;

<FM>Description<FN>
ZoomFilter specifies the filter to apply when the image is not displayed at normal size (i.e. <A TImageEnView.Zoom> is not 100). It improves the quality of the image display.
For fastest display the ZoomFilter should be set to <FC>rfNone<FN> (default), but better quality is achieved with a filter such as <FC>rfLancoz3<FN>.

<FM>Example<FC>
// Performs a 400% quality zoom
ImageEnView1.ZoomFilter := rfFastLinear;
ImageEnView1.Zoom := 400;
!!}
procedure TImageEnView.SetZoomFilter(v: TResampleFilter);
begin
  fSaveZoomFilter := v; // used in MouseUp (but also setted in MouseDown)
  if fZoomFilter <> v then
  begin
    fZoomFilter := v;
    Update;
  end;
end;




{!!
<FS>TImageEnView.Assign

<FM>Declaration<FC>        
procedure Assign(Source: TObject); reintroduce; overload;

<FM>Description<FN>
Copy the content of a TBitmap, <A TIEBitmap>, <A TImageEnView> or <A TImageEnVect> to the current control.

<FM>Example<FC>
// Copy content from another TImageEnView, including layers and IO params
ImageEnView1.Assign(ImageEnView2);

// Copy only the image of another TImageEnView
ImageEnView1.Assign(ImageEnView2.IEBitmap);
!!}
procedure TImageEnView.Assign(Source: TObject);
var
  si: TImageEnView;
  i: Integer;
  iLayerID: Integer;
begin
  if Source = nil then
    Clear
  else
  if Source is TImageEnView then
  begin         
    si := (Source as TImageEnView);

    // Disable navigator
    SetNavigator(nil);

    fBackGround := si.fBackGround;
    SetDPI(si.fDPIX, si.fDPIY);

    // IO Params
    IO.Params.Assign( si.IO.Params );

    // Layers
    LayersClear;
    for i := 0 to si.LayersCount - 1 do
    begin
      if i > 0 then // layer 0 already is here, no need to create
        iLayerID := LayersAdd()
      else
        iLayerID := 0;
      Layers[ iLayerID ].Assign( si.Layers[i] );  // here bitmaps are also copied
    end;
    fLayersCurrent := si.fLayersCurrent;

    // Vect objects
    if (Source is TImageEnVect) and (Self is TImageEnVect) then
      (Source as TImageEnVect).CopyAllObjectsTo(Self as TImageEnVect);

    Update;
    ImageChange;

    // Navigator
    if si.IsNavigator then
      SetNavigator(si.fNavigator);
  end
  else
  if Source is TBitmap then
  begin
    fIEBitmap.CopyFromTBitmap(source as TBitmap);
    if (fIEBitmap.PixelFormat <> ie1g) and (fIEBitmap.PixelFormat <> ie24RGB) then
      fIEBitmap.PixelFormat := ie24RGB;
    Update;
    ImageChange;
  end
  else
  if Source is TIEBitmap then
  begin
    fIEBitmap.Assign(Source);
    Update;
    ImageChange;
  end;
end;


// ret True is the polygon is a rectangle
// "n" is the number of vertex
function _IsRectangle(p: PPointArray; n: integer): boolean;
var
  q: integer;
  vv: boolean; // vv=false (x equals)   vv=true (y equals)
begin
  result := false;
  if (n = 4) then
  begin
    if (p^[3].x = p^[0].x) and (p^[3].y <> p^[0].y) then
      vv := true
    else
    if (p^[3].x <> p^[0].x) and (p^[3].y = p^[0].y) then
      vv := false
    else
      exit;
    for q := 0 to 2 do
      if (p^[q].x = p^[q + 1].x) and (p^[q].y <> p^[q + 1].y) and (not vv) then
        vv := true
      else
      if (p^[q].x <> p^[q + 1].x) and (p^[q].y = p^[q + 1].y) and vv then
        vv := false
      else
        exit;
    result := true;
  end;
end;

{!!
<FS>TImageEnView.MouseInteract

<FM>Declaration<FC>
property MouseInteract: <A TIEMouseInteract>;

<FM>Description<FN>
MouseInteract determines which mouse activities are performed when the user interacts with the ImageEnView component with the mouse.

<FM>Example<FC>
// Single left click zoom-in image, single right click zoom-out image, click and
// drag scroll the image (if it is bigger than client area).
ImageEnView1.MouseInteract := [miZoom, miScroll];
!!}
function TImageEnView.GetMouseInteract: TIEMouseInteract;
begin
  result := fMouseInteract;
end;

{!!
<FS>TImageEnView.BackgroundStyle

<FM>Declaration<FC>
property BackgroundStyle: <A TIEBackgroundStyle>;

<FM>Description<FN>
Specifies the style of the image background (the region of the ImageEnView window that is not filled by the image).

<FM>Example<FC>
ImageEnView1.BackgroundStyle := iebsPhotoLike;

<FM>See Also<FN>
- <A TImageEnView.Background>
- <A TImageEnView.GradientEndColor>
- <A TImageEnView.Wallpaper>
- <A TImageEnView.WallpaperStyle>
!!}
procedure TImageEnView.SetBackgroundStyle(v: TIEBackgroundStyle);
begin
  fBackgroundStyle := v;
  invalidate;
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

procedure TImageEnView.DoSelectionChanging;
begin
  SwapSelectionBase;
  if Assigned(fOnSelectionChanging) then
    OnSelectionChanging(self);
  SwapSelectionBase;
end;

procedure TImageEnView.DoSelectionChange;
begin
  if Assigned(fOnSelectionChange) then
    OnSelectionChange(self);
end;

procedure TImageEnView.DoBeforeSelectionChange;
begin
  SwapSelectionBase;
  if Assigned(fOnBeforeSelectionChange) then
    OnBeforeSelectionChange(self);
  SwapSelectionBase;
end;



{!!
<FS>TImageEnView.CopyFromPolygon

<FM>Declaration<FC>
procedure CopyFromPolygon(Source: TBitmap; const Polygon: array of TPoint; PolygonLen: Integer; const Position: TPoint);

<FM>Description<FN>
CopyFromPolygon copies a region (Polygon) of source bitmap to current image.
CopyFromPolygon enlarges current bitmap when needed.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Source<FN></C> <C>Source bitmap</C> </R>
<R> <C><FC>Polygon<FN></C> <C>Array of polygon vertexes (pixel coordinates related to Source bitmap)</C> </R>
<R> <C><FC>PolygonLen<FN></C> <C>Number of vertexes in polygon</C> </R>
<R> <C><FC>Position<FN></C> <C>Destination point (pixel coordinate related to current image). The destination point is the top-left side of the rectangle that encloses source polygon</C> </R>
</TABLE>


<FM>Example<FC>
// Copies current selected area of ImageEnView1 to ImageEnView2 at position 0, 0
ImageEnView2.CopyFromPolygon(ImageEnView1.Bitmap, ImageEnView1.PolySelPoints^, ImageEnView1.PolySelCount, Point(0, 0));

// Copies the rect 0, 0, 100, 100 in ImageEnView2 at position 0, 0
ImageEnView2.CopyFromPolygon(ImageEnView1.Bitmap, [Point(0, 0), Point(100, 0), Point(100, 100), Point(0, 100)], 4, Point(0, 0));

// Copies current selection to ImageEnView2. Then enhance contrast of ImageEnView2 and copy back to ImageEnView1 (in some selection).
ImageEnView2.CopyFromPolygon(ImageEnView1.Bitmap, ImageEnView2.PolySelPoints^, ImageEnView2.PolySelCount, Point(0, 0));
ImageEnView2.Proc.Contrast(30);
ImageEnView2.CopyToPolygon(ImageEnView1.Bitmap, ImageEnView1.PolySelPoints^, ImageEnView1.PolySelCount, Point(0, 0));
ImageEnView1.Update;

!!}
// Copy the polygon Polygon of Source in Position of fBitmap
procedure TImageEnView.CopyFromPolygon(Source: TBitmap; const Polygon: array of TPoint; PolygonLen: integer; const Position: TPoint);
begin
  _CopyPolygonToPoint(Source, @Polygon, PolygonLen, fBitmap, Position);
  Update;
end;


{!!
<FS>TImageEnView.CopyToPolygon

<FM>Declaration<FC>
procedure CopyToPolygon(Dest: TBitmap; const Polygon: array of TPoint; PolygonLen: Integer; const Position: TPoint);

<FM>Description<FN>
CopyToPolygon copies a region (Polygon) of current image inside Dest bitmap.

Polygon is an array of TPoint (pixel coordinates related to Dest bitmap) and PolygonLen is the number of point in Polygon.
Position is the source point (pixel coordinate related to current image). The source point is the top-left side of the rectangle that encloses destination polygon.
CopyToPolygon enlarges destination bitmap when needed.


<FM>Example<FC>
// Copies current selection to ImageEnView2. Then enhance contrast of ImageEnView2 and copy back to ImageEnView1 (in some selection).
ImageEnView2.CopyFromPolygon(ImageEnView1.Bitmap, ImageEnView2.PolySelPoints^, ImageEnView2.PolySelCount, Point(0, 0));
ImageEnView2.Proc.Contrast(30);
ImageEnView2.CopyToPolygon(ImageEnView1.Bitmap, ImageEnView1.PolySelPoints^, ImageEnView1.PolySelCount, Point(0, 0));
ImageEnView1.Update;
!!}
// copy the rect at Position of fBitmap in Polygon of Dest
procedure TImageEnView.CopyToPolygon(Dest: TBitmap; const Polygon: array of TPoint; PolygonLen: integer; const Position: TPoint);
begin
  _CopyPointToPolygon(fBitmap, @Polygon, PolygonLen, Dest, Position);
end;

function TImageEnView.GetScrollBarsAlwaysVisible: boolean;
begin
  result := fScrollBarsAlwaysVisible;
end;

procedure TImageEnView.SetScrollBarsAlwaysVisible(v: boolean);
begin
  fScrollBarsAlwaysVisible := v;
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

{!!
<FS>TImageEnView.DisplayGrid

<FM>Declaration<FC>
property DisplayGrid: Boolean;

<FM>Description<FN>
When DisplayGrid is True and the Zoom is equal to or greater than 400, a grid is drawn. Each grid box is equivalent to a single bitmap pixel.

<FM>See Also<FN>
- <A TIEImageEnGlobalSettings.MinZoomDisplayGrid>
- <A TImageEnView.DisplayGridLyr>
!!}
procedure TImageEnView.SetDisplayGrid(v: boolean);
begin
  fDisplayGrid := v;
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

{!!
<FS>TImageEnView.DisplayGridLyr

<FM>Declaration<FC>
property DisplayGridLyr: Integer;

<FM>Description<FN>
Specifies where to draw the grid.
-1   : current layer (default behavior)
>= 0 : specific layer


<FM>See Also<FN>
- <A TIEImageEnGlobalSettings.MinZoomDisplayGrid>
- <A TImageEnView.DisplayGrid>
!!}
procedure TImageEnView.SetDisplayGridLyr(v: integer);
begin
  fDisplayGridLyr := v;
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

procedure TImageEnView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (ssShift in Shift) and fEnableShiftKey then
  begin
    case cursor of
      1785: SetTempCursor(1791);
      1793: SetTempCursor(1794);
    end;
  end;
end;

procedure TImageEnView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (not (ssShift in Shift)) and fEnableShiftKey then
  begin
    RestoreCursor;
  end;
end;

procedure TImageEnView.KeyPress(var Key: Char);
begin
  inherited;
end;


{!!
<FS>TImageEnView.EndSelect

<FM>Declaration<FC>
procedure EndSelect;

<FM>Description<FN>
EndSelect terminates a selection specified by code using the <A TImageEnView.AddSelPoint> and <A TImageEnView.AddSelBreak> methods.

<FM>Example<FC>
ImageEnView1.AddSelPoint(0, 0);
ImageEnView1.AddSelPoint(100, 100);
ImageEnView1.AddSelPoint(50, 50);
ImageEnView1.EndSelect;
!!}
// finalize selection (update fSelectionMask)
procedure TImageEnView.EndSelect;
begin
  fSelectionMask.DrawPolygon(fSelectionIntensity, PIEAnimPoly(fHPolySel)^.Poly, PIEAnimPoly(fHPolySel)^.PolyCount); // update mask
  if fSelectionMask.IsEmpty then
  begin
    // empty selection
    fSel := false;
    AnimPolygonClear(fHPolySel);
  end;
end;

function TImageEnView.GetClientWidth: integer;
begin
  if fOffscreenPaint then
    result := Width
  else
  if HasParentWindow and HandleAllocated then  // 3.0.1
    result := inherited ClientWidth
  else
    result := 50;
end;

function TImageEnView.GetClientHeight: integer;
begin
  if fOffscreenPaint then
    result := Height
  else
  if HasParentWindow and HandleAllocated then  // 3.0.1
    result := inherited Clientheight
  else
    result := 50;
end;

{!!
<FS>TImageEnView.SelColor1

<FM>Declaration<FC>
property SelColor1: TColor

<FM>Description<FN>
SelColor1 and <A TImageEnView.SelColor2> set the two colors of the animated selection polygon.
!!}
procedure TImageEnView.SetSelColor1(v: TColor);
begin
  PIEAnimPoly(fHPolySel)^.Color1 := v;
  fSelColor1 := v;
end;

{!!
<FS>TImageEnView.SelColor2

<FM>Declaration<FC>
property SelColor2: TColor

<FM>Description<FN>
<A TImageEnView.SelColor1> and SelColor2 set the two colors of the animated selection polygon.
!!}
procedure TImageEnView.SetSelColor2(v: TColor);
begin
  PIEAnimPOly(fHPolySel)^.Color2 := v;
  fSelColor2 := v;
end;

function TImageEnView.GetImageEnIO: TImageEnIO;
begin
  if not assigned(fImageEnIO) then
  begin
    fImageEnIO := TImageEnIO.Create(self);
    fImageEnIO.AttachedImageEn := self;
    fImageEnIO.OnProgress := fOnProgress;
    fImageEnIO.OnFinishWork := fOnFinishWork;
    fImageEnIO.OnAcquireBitmap := fOnAcquireBitmap;
  end;
  result := fImageEnIO;
end;

function TImageEnView.GetImageEnProc: TImageEnProc;
begin
  if not assigned(fImageEnProc) then
  begin
    fImageEnProc := TImageEnProc.Create(self);
    fImageEnProc.AttachedImageEn := self;
    fImageEnProc.OnProgress := fOnProgress;
    fImageEnProc.OnFinishWork := fOnFinishWork;
    fImageEnProc.OnSaveUndo := fOnSaveUndo;
  end;
  result := fImageEnProc;
end;

procedure TImageEnView.SetOnProgress(v: TIEProgressEvent);
begin
  fOnProgress := v;
  if assigned(fImageEnIO) then
    fImageEnIO.OnProgress := v;
  if assigned(fImageEnProc) then
    fImageEnProc.OnProgress := v;
end;

function TImageEnView.GetOnProgress: TIEProgressEvent;
begin
  result := fOnProgress;
end;

{!!
<FS>TImageEnView.OnAcquireBitmap

<FM>Declaration<FC>
property OnAcquireBitmap: <A TIEAcquireBitmapEvent>;

<FM>Description<FN>
Occurs whenever a new bitmap is acquired during an acquisition.

!!}
function TImageEnView.GetOnAcquireBitmap: TIEAcquireBitmapEvent;
begin
  result := fOnAcquireBitmap;
end;

procedure TImageEnView.SetOnAcquireBitmap(v: TIEAcquireBitmapEvent);
begin
  fOnAcquireBitmap := v;
  if assigned(fImageEnIO) then
    fImageEnIO.OnAcquireBitmap := v;
end;

{!!
<FS>TImageEnView.OnFinishWork

<FM>Declaration<FC>
property OnFinishWork: TNotifyEvent;

<FM>Description<FN>
Occurs whenever an image processing or input/output task terminates.
It is always called after <A TImageEnView.OnProgress> so is useful to reset a progress bar.
!!}
function TImageEnView.GetOnFinishWork: TNotifyEvent;
begin
  result := fOnFinishWork;
end;

procedure TImageEnView.SetOnFinishWork(v: TNotifyEvent);
begin
  fOnFinishWork := v;
  if assigned(fImageEnIO) then
    fImageEnIO.OnFinishWork := v;
  if assigned(fImageEnProc) then
    fImageEnProc.OnFinishWork := v;
end;

{!!
<FS>TImageEnView.TransitionRunning

<FM>Declaration<FC>
property TransitionRunning: Boolean;

<FM>Description<FN>
TransitionRunning is True whenever a transition effect is being displayed.

<FM>See Also<FN>
- <A TImageEnView.PrepareTransition>
- <A TImageEnView.RunTransition>
- <A TImageEnView.AbortTransition>
!!}
function TImageEnView.GetTransitionRunning: boolean;
begin
  result := assigned(fTransition) and fTransition.Running;
end;

{!!
<FS>TImageEnView.PrepareTransition

<FM>Declaration<FC>
procedure PrepareTransition;

<FM>Description<FN>
PrepareTransition must be called prior to <L TImageEnView.RunTransition>running</L> a transition effect.
It copies the currently displayed image to an internal buffer, which allows you to change current image (load, update, etc.) before starting the transition with <A TImageEnView.RunTransition>.

<FM>Example<FC>
// Show a transition effect from the image already loaded in AImageEnView to our new image, Pic2.jpg
AImageEnView.PrepareTransition;                     // Prepare the transition
AImageEnView.IO.LoadFromFile('C:\Pic2.jpg');     // Load the new image (though it won't yet display)
AImageEnView.RunTransition(iettRandompoints, 500); // Start display of the transition effect

<FM>See Also<FN>
- <A TImageEnView.RunTransition>
- <A TImageEnView.TransitionRunning>   
- <A TImageEnView.AbortTransition>
!!}
procedure TImageEnView.PrepareTransition;
var
  iebmp: TIEBitmap;
begin
  if (GetClientWidth = 0) or (GetClientHeight = 0) then
    exit;
  SetupTransition;
  fTransition.SetSizes(ClientWidth, ClientHeight);
  iebmp := TIEBitmap.Create;
  try
    iebmp.EncapsulateTBitmap(fTransition.SourceShot, false);
    PaintToEx(iebmp, nil, nil, true, true);
  finally
    iebmp.free;
  end;
end;

{!!
<FS>TImageEnView.RunTransition

<FM>Declaration<FC>
procedure RunTransition(Effect : <A TIETransitionType>; Duration : integer); overload;
procedure RunTransition(Effect : <A TIETransitionType>; Duration : integer; StartRect, EndRect : TRect; bMaintainAspectRatio : Boolean); overload;
procedure RunTransition(Effect : <A TIETransitionType>; Duration : integer; PanZoomEffect : <A TIEPanZoomType>; iZoomLevel : Integer); overload;

<FM>Description<FN>
RunTransition starts the transition using <FC>Effect<FN> and <FC>Duration<FN> parameters.

Effect specifies the effect. Duration specifies the duration of the transition in milliseconds.

If effect is iettPanZoom then use one of the overloaded versions.

Pan-Zoom Overload 1:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>StartRect</C> <C>The starting rectangle of an iettPanZoom transition (specified in bitmap points)</C> </R>
<R> <C>EndRect</C> <C>The ending rectangle of an iettPanZoom transition (specified in bitmap points)</C> </R>
<R> <C>bMaintainAspectRatio</C> <C>StartRect and EndRect will be automatically adjusted to ensure the image appears with the correct aspect ratio</C> </R>
</TABLE>            

Pan-Zoom Overload 2:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>PanZoomEffect</C> <C>One of ImageEn's <L TIEPanZoomType>built-in Pan Zoom effects</L></C> </R>
<R> <C>iZoomLevel</C> <C>If the specified <FC>PanZoomEffect<FN> is zoom type, then this specifies the maximum amount to zoom in, e.g. 20%</C> </R>
</TABLE>

<FM>Example<FC>
// Transition from Image1.jpg to Image2.jpg with "Randon Points" transition
ImageEnView1.IO.LoadFromFile('D:\image1.jpg');               // Load initial image
ImageEnView1.PrepareTransition;                            // Prepare the transition
ImageEnView1.IO.LoadFromFile('D:\image2.jpg');            // Load the next image (but does not display it)
ImageEnView1.RunTransition(iettRandompoints, 500);     // Execute the transition


// Pan from the top-left corner of the image to the bottom-right (Image is 600 x 800)
ImageEnView1.PrepareTransition;
ImageEnView1.IO.LoadFromFile('D:\image.jpg');
ImageEnView1.RunTransition(iettPanZoom,
                           2000,                       // 2 second transition
                           Rect(0, 0, 300, 400),       // Top-left quarter
                           Rect(300, 400, 600, 800),   // Bottom-right quarter
                           True);                      // Automatically adjust starting and ending rect to ensure the transition does not distort the image

// Pan from the top-left corner of the image to the bottom-right
ImageEnView1.PrepareTransition;
ImageEnView1.IO.LoadFromFile('D:\image.jpg');
ImageEnView1.RunTransition(iettPanZoom,                           
                           2000,                       // 2 second transition
                           iepzPanTopLeftToBottomRight,
                           20);

<FM>See Also<FN>
- <A TImageEnView.PrepareTransition>
- <A TImageEnView.TransitionRunning>
- <A TImageEnView.AbortTransition>
!!}
procedure TImageEnView.RunTransition(Effect: TIETransitionType; duration: integer);
var
  iebmp: TIEBitmap;
begin
  if (GetClientWidth = 0) or (GetClientHeight = 0) then
    exit;
  SetupTransition;
  if (duration < 1) then
    duration := 1;
  fTransitionEffect := Effect;
  fTransitionDuration := duration;
  fTransition.Transition := fTransitionEffect;
  fTransition.Duration := fTransitionDuration;
  fTransition.Background := Background;
  iebmp := TIEBitmap.Create;
  try
    iebmp.EncapsulateTBitmap(fTransition.TargetShot, false);
    PaintToEx(iebmp, nil, nil, true, true);
  finally
    iebmp.free;
  end;
  fTransition.FullImage := fIEBitmap;
  fTransition.Run(true);

  // Now adjust the display to match the end position of the transition
  if Effect = iettPanZoom then
    VisibleBitmapRect := fTransition.EndRect;

  fTransitionEffect := iettNone;
end;


procedure TImageEnView.RunTransition(Effect: TIETransitionType; Duration: integer; PanZoomEffect : TIEPanZoomType; iZoomLevel : Integer);
var
  StartRect, EndRect : TRect;
begin
  GetPanZoomEffectStartEndRects(ClientWidth, ClientHeight,
                                fIEBitmap.Width, fIEBitmap.Height,
                                PanZoomEffect,
                                iZoomLevel,
                                StartRect, EndRect);
  RunTransition(Effect, Duration, StartRect, EndRect, True);
end;


procedure TImageEnView.RunTransition(Effect: TIETransitionType; Duration: integer; StartRect, EndRect : TRect; bMaintainAspectRatio : Boolean = True);
begin
  SetupTransition;
  if bMaintainAspectRatio then
  begin
    fTransition.StartRect := AdjustRectToAspectRatio(StartRect);
    fTransition.EndRect   := AdjustRectToAspectRatio(EndRect);
  end
  else
  begin
    fTransition.StartRect := StartRect;
    fTransition.EndRect   := EndRect;
  end;
  RunTransition(Effect, Duration);
end;


{$ifdef IEINCLUDEDEPRECATEDMETHODS}
// DEPRECATED: Use TImageEnProc.PrepareTransitionBitmaps/CreateTransitionBitmaps
procedure TImageEnView.PrepareTransitionBitmaps(OriginalBitmap, TargetBitmap : TBitmap; Effect : TIETransitionType; iWidth : Integer = -1; iHeight : Integer = -1);  {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
begin
  SetupTransition;
  fTransition.Transition := Effect;
  fTransition.FullImage := fIEBitmap;
  fTransition.Background := Background;

  if Effect = iettPanZoom then
  begin
    fIEBitmap.assign(OriginalBitmap);
    fTransition.PrepareBitmap(OriginalBitmap, OriginalBitmap);
  end
  else
  begin
    fTransition.PrepareBitmap(OriginalBitmap, TargetBitmap);
  end;
  
  if iWidth > 0 then
    fTransition.SetSizes(iWidth, iHeight);
end;
{$endif}

{$ifdef IEINCLUDEDEPRECATEDMETHODS}
// DEPRECATED: Use TImageEnProc.PrepareTransitionBitmaps/CreateTransitionBitmaps
procedure TImageEnView.PrepareTransitionBitmaps(OriginalBitmap, TargetBitmap : TBitmap; Effect : TIETransitionType; StartRect, EndRect : TRect; bMaintainAspectRatio : Boolean = True; iWidth : Integer = -1; iHeight : Integer = -1);  {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
begin
  SetupTransition;
  if bMaintainAspectRatio then
  begin
    fTransition.StartRect := AdjustRectToAspectRatio(StartRect);
    fTransition.EndRect   := AdjustRectToAspectRatio(EndRect);
  end
  else
  begin
    fTransition.StartRect := StartRect;
    fTransition.EndRect   := EndRect;
  end;
  PrepareTransitionBitmaps(OriginalBitmap, TargetBitmap, Effect, iWidth, iHeight);
end;
{$endif}

{$ifdef IEINCLUDEDEPRECATEDMETHODS}
// DEPRECATED: Use TImageEnProc.PrepareTransitionBitmaps/CreateTransitionBitmaps 
// TransitionProgress: The percentage that it is has progressed from the start image to the end image (ranging from 0.0 to 100.0)
// ToBitmap: the bitmap to write it to
procedure TImageEnView.CreateTransitionBitmap(TransitionProgress : Single; DestBitmap : TBitmap); {$ifdef IESUPPORTDEPRECATED} deprecated; {$endif}
begin
  SetupTransition;
  fTransition.CreateBitmap(TransitionProgress, DestBitmap);
end;
{$endif}

{!!
<FS>TImageEnView.AbortTransition

<FM>Declaration<FC>
procedure AbortTransition;

<FM>Description<FN>
This method aborts current transition started with <A TImageEnView.RunTransition>.

<FM>See Also<FN>
- <A TImageEnView.PrepareTransition>
- <A TImageEnView.RunTransition>
- <A TImageEnView.TransitionRunning>
!!}
procedure TImageEnView.AbortTransition;
begin
  if assigned(fTransition) then
    fTransition.Stop;
end;

procedure TImageEnView.SetOnTransitionStop(value: TNotifyEvent);
begin
  SetupTransition;
  fTransition.OnTransitionStop := value;
end;

{!!
<FS>TImageEnView.OnTransitionStop

<FM>Declaration<FC>
property OnTransitionStop: TNotifyEvent;

<FM>Description<FN>
Occurs when the transition (that was started by using <A TImageEnView.RunTransition>) has finished its job (i.e. the transition effect has finished).

!!}
function TImageEnView.GetOnTransitionStop: TNotifyEvent;
begin
  if assigned(fTransition) then
    result := fTransition.OnTransitionStop
  else
    result := nil;
end;

procedure TImageEnView.SetOnTransitionPaint(const Value: TIEOnTransitionPaint);
begin
  SetupTransition;
  fTransition.OnTransitionPaint := Value;
end;

procedure TImageEnView.SetOnTransitionStep(value: TIETransitionStepEvent);
begin
  SetupTransition;
  fTransition.OnTransitionStep := value;
end;

{!!
<FS>TImageEnView.OnTransitionStep

<FM>Declaration<FC>
property OnTransitionStep: <A TIETransitionStepEvent>;

<FM>Description<FN>
Occurs prior to the painting of each transition frame. <FC>Step<FN> is a value from 0 to 1024.
!!}
function TImageEnView.GetOnTransitionStep: TIETransitionStepEvent;
begin
  if assigned(fTransition) then
    result := fTransition.OnTransitionStep
  else
    result := nil;
end;

{!!
<FS>TImageEnView.OnTransitionPaint

<FM>Declaration<FC>
property OnTransitionPaint: <A TIEOnTransitionPaint>;

<FM>Description<FN>
Occurs immediately before a new transition frame is painted. <FC>Step<FN> is a value from 0 to 1024.
!!}
function TImageEnView.GetOnTransitionPaint: TIEOnTransitionPaint;
begin
  if fTransition = nil then
    Result := nil
  else
    Result := fTransition.OnTransitionPaint;
end;

procedure TImageEnView.SetTransitionTiming(value: TIETransitionTiming);
begin
  SetupTransition;
  fTransition.Timing := value;
end;

{!!
<FS>TImageEnView.TransitionTiming

<FM>Declaration<FC>
property TransitionTiming: <A TIETransitionTiming>;

<FM>Description<FN>
TransitionTiming provides alternative timing options for transition effect progress. Generally this will be iettLinear (normal progress).
!!}
function TImageEnView.GetTransitionTiming: TIETransitionTiming;
begin
  SetupTransition;
  result := fTransition.Timing;
end;




{!!
<FS>TImageEnView.Playing

<FM>Declaration<FC>
property Playing : boolean;

<FM>Description<FN>
Set Playing to True to animate GIF and AVI files. If <A TImageEnView.PlayLoop> is enabled then the the animation will replay continuously

Note: TImageEnView.Playing loads each frame on demand, which means that animations with short display times may appear jerky. It also cannot be used with LoadFromURL. You may find it more suitable to use TImageEnMView.<A TImageEnMView.Playing> instead which preloads all frames.

!!}
procedure TImageEnView.SetPlaying(const v : boolean);
// Xequte: 13/11/12
begin
  if v = fPlaying then
    exit;

  fPlaying := v;

  if v = False then
    StopPlayTimer
  else
    StartPlayTimer;
end;


// Timer event for fPlayTimer
procedure TImageEnView.WMTimer(var Message: TWMTimer); 
// Xequte: 13/11/12
begin
  StopPlayTimer;

  if IO.Params.ImageIndex >= IO.Params.ImageCount - 1 then
  begin
    IO.Seek(ieioSeekFirst);
    if fPlayLoop = False then
    begin
      // After we have looped back (in case they want to play again)
      Playing := False;
      exit;
    end;
  end
  else
  begin
    IO.Seek(ieioSeekNext);
  end;

  if IO.Aborting = False then
    StartPlayTimer;
end;

// Enable fPlayTimer if the current file is an animated GIF or AVI
procedure TImageEnView.StartPlayTimer;          
// Xequte: 13/11/12
var
  iInterval: Integer;
begin
  StopPlayTimer;
  if fPlaying = False then
    exit;

  iInterval := 0;
  if IO.Params.ImageCount > 1 then
  begin
    iInterval := IO.Params.ImageDelayTime;
    if iInterval < 1 then
      iInterval := Default_GIF_Animation_Delay_MS;
  end;

  if (iInterval > 0) and HasParentWindow then
    fPlayTimer := SetTimer(self.handle, 1, iInterval, nil);
end;


// Reset fPlayTimer
procedure TImageEnView.StopPlayTimer;    
// Xequte: 13/11/12
begin
  // remove timer
  if fPlayTimer <> 0 then
  begin
    KillTimer(self.handle, 1);
    fPlayTimer := 0;
  end;
end;




// Returns an adjusted rect taking into account the aspect ratio of the image and display window
function TImageEnView.AdjustRectToAspectRatio(ARect: TRect): TRect;
begin
  if IsEmpty then
    result := ARect
  else
    result := hyieutils.AdjustRectToAspectRatio(ARect, ClientWidth, ClientHeight, fIEBitmap.Width, fIEBitmap.Height);
end;

{!!
<FS>TImageEnView.AlphaChannel

<FM>Declaration<FC>
property AlphaChannel: <A TIEBitmap>;

<FM>Description<FN>
Some formats like GIF, PNG, PSD, TIFF, ICO, CUR and TGA contain an alpha channel that specifies the image's transparency.
The alpha channel is stored in the AlphaChannel property and in <A TImageEnView.IEBitmap>.<A TIEBitmap.AlphaChannel> property.
!!}
function TImageEnView.GetAlphaChannel: TIEBitmap;
begin
  result := fIEBitmap.AlphaChannel;
end;

{!!
<FS>TImageEnView.HasAlphaChannel

<FM>Declaration<FC>
property HasAlphaChannel: Boolean; (Read-only)

<FM>Description<FN>
The property HasAlphaChannel returns True if the current image has an alpha channel.

!!}
function TImageEnView.GetHasAlphaChannel: boolean;
begin
  result := fIEBitmap.HasAlphaChannel;
end;

{!!
<FS>TImageEnView.InvertSelection

<FM>Declaration<FC>
procedure InvertSelection;

<FM>Description<FN>
InvertSelection changes the selection to everything except the current selection.

<FM>Example<FC>

ImageEnView1.Select(10, 10, 100, 100, iespReplace);  // select box 10,10,100,100
ImageEnView1.InvertSelection;  // select all excluding the box 10,10,100,100

!!}
procedure TImageEnView.InvertSelection;
begin
  fSelectionMask.SyncFull;
  if fSelectionMask.Full then
    Deselect
  else
  begin
    AnimPolygonClear(fHPolySel);
    ShowSelectionEx(true);
    fSelectionMask.Negative(fSelectionIntensity);
    fDBToDraw := true;
    Paint;
  end;
end;

{!!
<FS>TImageEnView.EnableAlphaChannel

<FM>Declaration<FC>
property EnableAlphaChannel: Boolean;

<FM>Description<FN>
If EnableAlphaChannel is True, ImageEn uses the alpha channel to display the image.
Some formats like Gif, Png, Tiff, Ico, Cur and Tga contain an alpha channel that specifies the image's transparency.
The alpha channel is stored in the <A TImageEnView.AlphaChannel> property (a <A TIEBitmap> object) and in TIEBitmap.<A TIEBitmap.AlphaChannel> property.
!!}
procedure TImageEnView.SetEnableAlphaChannel(v: boolean);
begin
  if fEnableAlphaChannel <> v then
  begin
    fEnableAlphaChannel := v;
    Update;
  end;
end;

{!!
<FS>TImageEnView.UpdateRect

<FM>Declaration<FC>
procedure UpdateRect(rclip: TRect);

<FM>Description<FN>
UpdateRect updates the rectangle, rclip. Use this function instead of Update when only a portion of the image has changed.

<FM>Example<FC>
// we assume that Zoom = 100
ImageEnView1.Bitmap.Canvas.Fill(0, 0, 10, 10);
ImageEnView1.UpdateRect(rect(0, 0, 10, 10));
!!}
procedure TImageEnView.UpdateRect(rclip: TRect);
begin
  fDBToDraw := true;
  InvalidateRect(handle, @rclip, false);
end;

{!!
<FS>TImageEnView.SetSelectedAreaAlpha

<FM>Declaration<FC>
procedure SetSelectedAreaAlpha(Alpha: Integer);

<FM>Description<FN>
SetSelectedAreaAlpha sets the specified Alpha (alpha channel, transparency) value for all pixels inside current selection.

To activate alpha channel (transparency), set <A TImageEnView.EnableAlphaChannel> to True.

<FM>Example<FC>
// set transparency to 180 inside 10,10,100,100 rectangle
ImageEnView1.Select(10, 10, 100, 100);
ImageEnVIew1.SetSelectedAreaAlpha(180);
ImageEnView1.io.SaveToFile('C:\image.png');  // save with alpha channel

!!}
procedure TImageEnView.SetSelectedAreaAlpha(Alpha: integer);
var
  y, x: integer;
  palpha: pbyte;
  psel: pbyte;
begin
  if fSelectionMask.IsEmpty then
    // entire image
    GetAlphaChannel.Fill(Alpha)
  else
  begin
    // selected area
    for y := 0 to fSelectionMask.Height - 1 do
    begin
      palpha := fIEBitmap.AlphaChannel.ScanLine[y];
      psel := fSelectionMask.ScanLine[y];
      case fSelectionMask.BitsPerPixel of
        1:
          for x := 0 to fSelectionMask.Width - 1 do
          begin
            if (pbytearray(psel)^[x shr 3] and iebitmask1[x and $7]) <> 0 then
              palpha^ := Alpha;
            inc(palpha);
          end;
        8:
          for x := 0 to fSelectionMask.Width - 1 do
          begin
            if psel^ <> 0 then
              palpha^ := Alpha;
            inc(palpha);
            inc(psel);
          end;
      end;
    end;
    fIEBitmap.AlphaChannel.Full := false;
  end;
  Update;
end;

{!!
<FS>TImageEnView.SetSelectionGripStyle

<FM>Declaration<FC>
procedure SetSelectionGripStyle(GripColor1, GripColor2: TColor; GripBrushStyle: TBrushStyle; GripSize: Integer; ExtendedGrips: Boolean; boolean; Shape: <A TIEGripShape>);

<FM>Description<FN>
SetSelectionGripStyle determines the appearance of selection grips.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>GripColor1<FN></C> <C>grip border color (default clBlack)</C> </R>
<R> <C><FC>GripColor2<FN></C> <C>grip brush color (default clWhite)</C> </R>
<R> <C><FC>GripBrushStyle<FN></C> <C>brush style (default bsSolid)</C> </R>
<R> <C><FC>GripSize<FN></C> <C>size in pixels of the grip (default 5)</C> </R>
<R> <C><FC>ExtendedGrips<FN></C> <C>if true enables grips on border with 8 resizing grips (deault true)</C> </R>
<R> <C><FC>Shape<FN></C> <C>specifies the grip shape</C> </R>
</TABLE>

Use <A TImageEnView.GetSelectionGripStyle> to know current values.

<FM>Example<FC>
ImageEnView1.SetSelectionGripStyle(clWhite, clWhite, bsSolid, 5, true, iegsCircle);
!!}
procedure TImageEnView.SetSelectionGripStyle(GripColor1, GripColor2: TColor; GripBrushStyle: TBrushStyle; GripSize: integer; ExtendedGrips: boolean; Shape: TIEGripShape);
begin
  fGripColor1 := GripColor1;
  fGripColor2 := GripColor2;
  fGripBrushStyle := GripBrushStyle;
  fGripSize := GripSize;
  fGripShape := Shape;
  fExtendedGrips := ExtendedGrips;
end;


{!!
<FS>TImageEnView.GetSelectionGripStyle

<FM>Declaration<FC>
procedure GetSelectionGripStyle(var GripColor1: TColor; var GripColor2: TColor; var GripBrushStyle: TBrushStyle; var GripSize: integer; var ExtendedGrips: boolean; var Shape: TIEGripShape);

<FM>Description<FN>
GetSelectionGripStyle returns properties which determines the appearance of selection grips.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>GripColor1<FN></C> <C>grip border color (default clBlack)</C> </R>
<R> <C><FC>GripColor2<FN></C> <C>grip brush color (default clWhite)</C> </R>
<R> <C><FC>GripBrushStyle<FN></C> <C>brush style (default bsSolid)</C> </R>
<R> <C><FC>GripSize<FN></C> <C>size in pixels of the grip (default 5)</C> </R>
<R> <C><FC>ExtendedGrips<FN></C> <C>if true enables grips on border with 8 resizing grips (deault false)</C> </R>
<R> <C><FC>Shape<FN></C> <C>specifies the grip shape</C> </R>
</TABLE>

Use <A TImageEnView.SetSelectionGripStyle> to set current values.
!!}
procedure TImageEnView.GetSelectionGripStyle(var GripColor1: TColor; var GripColor2: TColor; var GripBrushStyle: TBrushStyle; var GripSize: integer; var ExtendedGrips: boolean; var Shape: TIEGripShape);
begin
  GripColor1 := fGripColor1;
  GripColor2 := fGripColor2;
  GripBrushStyle := fGripBrushStyle;
  GripSize := fGripSize;
  Shape := fGripShape;
  ExtendedGrips := fExtendedGrips;
end;

{!!
<FS>TImageEnView.SetLayersGripStyle

<FM>Declaration<FC>
procedure SetLayersGripStyle(GripColor1, GripColor2: TColor; GripBrushStyle: TBrushStyle; GripSize: Integer; Shape: <A TIEGripShape>);

<FM>Description<FN>
SetLayersGripStyle determines the appearance of layers grips.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>GripColor1<FN></C> <C>grip border color</C> </R>
<R> <C><FC>GripColor2<FN></C> <C>grip brush color</C> </R>
<R> <C><FC>GripBrushStyle<FN></C> <C>brush style</C> </R>
<R> <C><FC>GripSize<FN></C> <C>size in pixels of the grip</C> </R>
<R> <C><FC>Shape<FN></C> <C>specifies the grip shape</C> </R>
</TABLE>

<FM>Example<FC>
ImageEnView1.SetLayersGripStyle(clWhite, clWhite, bsSolid, 5, iegsCircle);
!!}
procedure TImageEnView.SetLayersGripStyle(GripColor1, GripColor2: TColor; GripBrushStyle: TBrushStyle; GripSize: integer; Shape: TIEGripShape);
begin
  fLyrGripColor1 := GripColor1;
  fLyrGripColor2 := GripColor2;
  fLyrGripBrushStyle := GripBrushStyle;
  fLyrGripSize := GripSize;
  fLyrGripShape := Shape;
end;

{!!
<FS>TImageEnView.SetChessboardStyle

<FM>Declaration<FC>
procedure SetChessboardStyle(Size: Integer; BrushStyle: TBrushStyle);

<FM>Description<FN>
SetChessboardStyle specifies the size and brush of the chessboard background (see <A TImageEnView.BackgroundStyle>).

Size specifies the box size in pixels (default 16).
BrushStyle specifies the brush style of the boxes (default bsSolid).

!!}
procedure TImageEnView.SetChessboardStyle(Size: integer; BrushStyle: TBrushStyle);
begin
  fChessboardSize := Size;
  fChessboardBrushStyle := BrushStyle;
end;



{!!
<FS>TImageEnView.CopySelectionToBitmap

<FM>Declaration<FC>
procedure CopySelectionToBitmap(DestBitmap: TBitmap);

<FM>Description<FN>
CopySelectionToBitmap copies the current selection to the specified bitmap.

<FM>Example<FC>
ImageEnView1.CopySelectionToBitmap( ImageEnView2.Bitmap );
!!}
// note: doesn't copy alpha channel
procedure TImageEnView.CopySelectionToBitmap(DestBitmap: TBitmap);
var
  tempbmp: TIEBitmap;
begin
  tempbmp := TIEBitmap.Create;

  if DestBitmap.PixelFormat<>pf1bit then
    DestBitmap.PixelFormat := pf24bit;

  tempbmp.EncapsulateTBitmap(DestBitmap, true);
  CopySelectionToIEBitmap(tempbmp, true);
  FreeAndNil(tempbmp);
end;

{!!
<FS>TImageEnView.CopySelectionToIEBitmap

<FM>Declaration<FC>
procedure CopySelectionToIEBitmap(DestBitmap: <A TIEBitmap>; FillBackground: Boolean);

<FM>Description<FN>
CopySelectionToIEBitmap copies the current selection to the specified bitmap.
If <FC>FillBackground<FN> is true the background is filled using <A TImageEnView.Background> color, otherwise it is filled with image self.

<FM>Example<FC>
ImageEnView1.CopySelectionToIEBitmap( ImageEnView2.IEBitmap );

!!}
procedure TImageEnView.CopySelectionToIEBitmap(DestBitmap: TIEBitmap; FillBackground: Boolean);
begin
  if (fRectResizing <> ieNone) or (fSelectMoving > -1) or fRectSelecting then
  begin
    fSelectionMask.Empty;
    EndSelect;
  end;
  if FillBackground then
    fIEBitmap.CopyWithMask1(DestBitmap, fSelectionMask, fBackground)
  else
    fIEBitmap.CopyWithMask1(DestBitmap, fSelectionMask);
end;


{!!
<FS>TImageEnView.AssignSelTo

<FM>Declaration<FC>
procedure AssignSelTo(Dest: TPersistent);

<FM>Description<FN>
Assigns the selected area to the <FC>Dest<FN> object (can be a <A TImageEnView>, TBitmap or TImage).


<FM>Example<FC>
// Create a triangular selection
ImageEnView1.DeSelect;
ImageEnView1.AddSelPoint(100, 100);
ImageEnView1.AddSelPoint(200, 100);
ImageEnView1.AddSelPoint(150, 50);
ImageEnView1.EndSelect;

// assign selection to ImageEnView2
ImageEnView1.AssignSelTo( ImageEnView2 );

// assign selection to mybitmap (TBitmap)
ImageEnView1.AssignSelTo( mybitmap );

!!}
// Assign selection to Dest
// Dest can be TImageEnView, TBitmap, TImage
// Copy also DPIX, Y and background
procedure TImageEnView.AssignSelTo(Dest: TPersistent);
var
  di: TImageEnView;
  db: tbitmap;
  im: TImage;
begin
  if Dest is TImageEnView then
  begin
    di := Dest as TImageEnView;
    if fSelectionMask.IsEmpty then
      di.IEBitmap.Assign(fIEBItmap)
    else
      CopySelectionToIEBitmap(di.IEBitmap, true);
    di.fBackground := Background;
    di.SetDPI(fDPIx, fDPIy);
    di.Update;
    di.ImageChange;
  end
  else
  if Dest is TBitmap then
  begin
    db := Dest as TBitmap;
    if fSelectionMask.IsEmpty then
      IECopyBitmap(fBitmap, db)
    else
      CopySelectionToBitmap(db);
    db.Modified := true;
  end
  else
  if Dest is TImage then
  begin
    im := Dest as TImage;
    if fSelectionMask.IsEmpty then
      IECopyBitmap(fBitmap, im.picture.bitmap)
    else
      CopySelectionToBitmap(im.picture.bitmap);
    im.picture.bitmap.Modified := true;
  end;
end;



{!!
<FS>TImageEnView.CopyToBitmapWithAlpha

<FM>Declaration<FC>
procedure TImageEnView.CopyToBitmapWithAlpha(Dest: TBitmap; DestX, DestY: Integer);

<FM>Description<FN>
CopyToBitmapWithAlpha copies the current image inside the <FC>Dest<FN> bitmap, at <FC>DestX, DestY<FN> position.
If the image has an alpha channel, CopyToBitmapWithAlpha copies only visible area.

<FM>Example<FC>
ImageEnView1.CopyToBitmapWithAlpha( ImageEnView2.Bitmap, 0, 0);
ImageEnView2.Update;

!!}
// Dest must be pf24bit
procedure TImageEnView.CopyToBitmapWithAlpha(Dest: TBitmap; DestX, DestY: integer);
var
  dummy1: ppointerarray;
  dummy2, dummy3: PInteger;
begin
  if Dest.PixelFormat <> pf24bit then
    Dest.PixelFormat := pf24bit;
  if fIEBitmap.HasAlphaChannel then
  begin
    dummy1 := nil;
    dummy2 := nil;
    dummy3 := nil;
    fIEBitmap.RenderToTBitmap(Dest, dummy1, dummy2, dummy3, nil, DestX, DestY, fIEBitmap.Width, fIEBitmap.Height, 0, 0, fIEBitmap.Width, fIEBitmap.Height, true, false, 255, rfNone, true, ielNormal);
  end;
end;


{!!
<FS>TImageEnView.SetSelectedPixelsColor

<FM>Declaration<FC>
procedure SetSelectedPixelsColor(color: <A TRGB>);

<FM>Description<FN>
SetSelectedPixelsColor sets selected pixels to the specified color.

<FM>Example<FC>
// select all pixels of a similar color to that 0, 0. Then fills all selected pixels with White.
ImageEnView.SelectMagicWand(0, 0, iespReplace);
ImageEnView.SetSelectedPixelsColor( CreateRGB(255, 255, 255) );

!!}
procedure TImageEnView.SetSelectedPixelsColor(color: TRGB);
var
  col, row: integer;
  px: PRGB;
  bitmapWidth, bitmapHeight: Integer;
begin
  bitmapWidth  := fIEBitmap.Width;
  bitmapHeight := fIEBitmap.Height;
  if fIEBitmap.PixelFormat = ie24RGB then
  begin
    for row := 0 to bitmapHeight - 1 do
    begin
      px := fIEBitmap.Scanline[row];
      for col := 0 to bitmapWidth - 1 do
      begin
        if fSelectionMask.IsPointInside(col, row) then
          px^ := color;
        inc(px);
      end;
    end;
    Update;
  end;
end;

{!!
<FS>TImageEnView.SetAlphaRangePixelsColor

<FM>Declaration<FC>
procedure SetAlphaRangePixelsColor(alphaMin, alphaMax: Integer; color: <A TRGB>);

<FM>Description<FN>
SetAlphaRangePixelsColor sets all pixels that have an alpha channel value between alphaMin and alphaMax to the specified color.

<FM>Example<FC>
// this example loads an image with alpha channel, then paints all transparent pixels (0...254 alpha values) using White color, finally saves in a jpeg where we cannot save the alpha channel.
ImageEnView.IO.LoadFromFile('C:\test.png');
ImageEnView.SetAlphaRangePixelsColor(0, 254, CreateRGB(255, 255, 255));
ImageEnView.IO.SaveToFile('C:\output.jpg');

!!}
procedure TImageEnView.SetAlphaRangePixelsColor(alphaMin, alphaMax: integer; color: TRGB);
var
  col, row: integer;
  px: PRGB;
  al: pbyte;
  bitmapWidth, bitmapHeight: Integer;
begin
  if not HasAlphaChannel then
    exit;
  bitmapWidth  := fIEBitmap.Width;
  bitmapHeight := fIEBitmap.Height;
  for row := 0 to bitmapHeight - 1 do
  begin
    px := fIEBitmap.Scanline[row];
    al := fIEBitmap.AlphaChannel.ScanLine[row];
    for col := 0 to bitmapWidth - 1 do
    begin
      if (al^ >= alphaMin) and (al^ <= alphaMax) then
        px^ := color;
      inc(px);
      inc(al);
    end;
  end;
  Update;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Layers

{!!
<FS>TImageEnView.LayersCount

<FM>Declaration<FC>
property LayersCount: Integer;

<FM>Description<FN>
LayersCount returns the number of layers.

!!}
function TImageEnView.GetLayersCount: integer;
begin
  result := fLayers.Count;
end;

function TImageEnView.GetLayer(idx: integer): TIELayer;
begin
  result := TIELayer(fLayers[idx]);
end;

{!!
<FS>TImageEnView.CurrentLayer

<FM>Declaration<FC>
property CurrentLayer: <A TIELayer>;

<FM>Description<FN>
Provides access to the currently active layer.

This is equivalent to:
<FC>ImageEnView.Layers[ ImageEnView.LayersCurrent ]<FN>

See the <A TImageEnView.Layers> property for more info.

<FM>Example<FC>
ImageEnView.LayersAdd;
ImageEnView.CurrentLayer.Transparency := 200;
!!}
function TImageEnView.GetCurrentLayer: TIELayer;
begin
  result := TIELayer(fLayers[fLayersCurrent]);
end;

{!!
<FS>TImageEnView.LayersCurrent

<FM>Declaration<FC>
property LayersCurrent: Integer;

<FM>Description<FN>
Use LayersCurrent to get/set the active layer.
The first layer has the index of 0, the last is <A TImageEnView.LayersCount> - 1.
Making a layer current changes the <A TImageEnView.IEBitmap> and <A TImageEnView.Bitmap> properties, so they point to the current layer (allowing specification of which layer is active for input/output and image processing operations).

<FM>Example<FC>
// load 'first.jpg' in layer 0 and 'second.jpg' in layer 1
ImageEnView1.LayersCurrent := 0;
ImageEnView1.IO.LoadFromFile('C:\first.jpg');
ImageEnView1.LayersCurrent := 1;
ImageEnView1.IO.LoadFromFile('C:\second.jpg');
!!}
procedure TImageEnView.SetLayersCurrent(Value: integer);
begin
  if (Value >= 0) and (Value < fLayers.Count) and (Value <> fLayersCurrent) then
  begin
    // sync current layer (fLayersCurrent = -1 is a temporary state, where no layer is selected)
    if fLayersCurrent > -1 then
    begin
      Layers[fLayersCurrent].Bitmap := fIEBitmap;
      if fSel then
        SaveSelection;
    end;

    // set new layer

    while true do
    begin
      fLayersCurrent := Value;
      if (Layers[Value].Selectable = true) or (Value = 0) or (fLayersSelectConstrains = false) then
        break;
      dec(Value);
    end;

    fIEBitmap := Layers[Value].Bitmap;
    if fIEBitmap.EncapsulatedFromTBitmap then
      fBitmap := fIEBitmap.VclBitmap
    else
      fBitmap := nil;

    if fSel then
      RestoreSelection(true, iersSyncLayers);

    CallBitmapChangeEvents;
    Update;
  end;
end;


{!!
<FS>TImageEnView.LayersAdd

<FM>Declaration<FC>
function LayersAdd(): Integer;
function LayersAdd(Width: Integer; Height: Integer; PixelFormat: <A TIEPixelFormat> = ie24RGB; PosX: Integer = 0; PosY: Integer = 0): Integer;
function LayersAdd(Bitmap: <A TIEBitmap>): integer;
function LayersAdd(FileName: String; PosX: Integer = 0; PosY: Integer = 0): integer;

<FM>Description<FN>
Appends a new layer to the layers list. The new layer will become the current layer.
If the size and pixel format are not specified then the new layer assumes that of the current layer.
You can specify PosX and PosY for the destination position of the layer.
LayersAdd returns the index of the added layer.


<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\first.jpg');    // load 'first' in first layer (or current layer)
ImageEnView1.LayersAdd;   // append a new layer
ImageEnView1.IO.LoadFromFile('C:\second.jpg');   // load 'second.jpg' in the new append layer
!!}
function TImageEnView.LayersAdd(): integer;
begin
  result := fLayers.Add(TIELayer.Create(self, fIEBitmap, false));
  LayersCurrent := result; // this call update
  Clear;
end;

function TImageEnView.LayersAdd(Width: Integer; Height: Integer; PixelFormat: TIEPixelFormat = ie24RGB; PosX: Integer = 0; PosY: Integer = 0): Integer;
begin
  result := fLayers.Add(TIELayer.Create(self, TIEBitmap.Create(Width, Height, PixelFormat), true));
  Layers[result].PosX := PosX;
  Layers[result].PosY := PosY;
  LayersCurrent := result; // this call update
end;

function TImageEnView.LayersAdd(FileName: WideString; PosX: Integer = 0; PosY: Integer = 0): Integer;
begin
  result := LayersAdd;
  IO.LoadFromFile(FileName);
  CurrentLayer.PosX := PosX;
  CurrentLayer.PosY := PosY;
end;

function TImageEnView.LayersAdd(Bitmap: TIEBitmap; DoCopy: Boolean): integer;
var
  bmp: TIEBitmap;
begin
  if DoCopy then
  begin
    bmp := TIEBitmap.Create;
    bmp.Assign( Bitmap );
    result := fLayers.Add(TIELayer.Create(self, bmp, true));
  end
  else
    result := fLayers.Add(TIELayer.Create(self, Bitmap, true));
  LayersCurrent := result; // this call update
end;



{!!
<FS>TImageEnView.LayersInsert

<FM>Declaration<FC>
procedure LayersInsert(Position: Integer);

<FM>Description<FN>
Inserts a new layer into the layers list at the specified <FC>Position<FN>.
The new layer becomes the current one, and assumes the size of other layers and the pixel format of the last one.

!!}
// insert a new layer and return its index
// create the bitmap and nil alpha channel
procedure TImageEnView.LayersInsert(Position: integer);
begin
  // sync current layer
  Layers[fLayersCurrent].Bitmap := fIEBitmap;
  //
  if Layers[Position].IsMask then
    dec(Position);
  fLayers.Insert(Position, TIELayer.Create(self, fIEBitmap, false));
  fLayersCurrent := -1;
  LayersCurrent := Position; // this call update
end;


{!!
<FS>TImageEnView.LayersRemove

<FM>Declaration<FC>
procedure LayersRemove(idx: Integer);

<FM>Description<FN>
Removes the specified layer and frees the related bitmap. At least one layer must be present.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>idx<FN></C> <C>Index of the layer to remove (0 = background/first layer)</C> </R>
</TABLE>

!!}
// one layer musts remain
procedure TImageEnView.LayersRemove(idx: integer);
var
  l: integer;
begin
  if (fLayers.Count > 1) and (idx > -1) and (idx < fLayers.Count) then
  begin // fLayers.Count>1 is not a bug
    // sync current layer
    Layers[fLayersCurrent].Bitmap := fIEBitmap;
    // free idx+1 layer if it is a layer mask
    if (idx<fLayers.Count-1) and (Layers[idx+1].IsMask) then
    begin
      Layers[idx+1].Free;
      fLayers.Delete(idx+1);
    end;
    // free idx layer
    Layers[idx].Free;
    fLayers.Delete(idx);
    // set new current layer
    if (fLayersCurrent = idx) then
    begin
      fLayersCurrent := -1;
      SetLayersCurrent(imin(idx, fLayers.Count - 1));
    end
    else
    if (fLayersCurrent > idx) then
    begin
      l := fLayersCurrent;
      fLayersCurrent := -1;
      SetLayersCurrent(l - 1);
    end;
  end;
end;

{!!
<FS>TImageEnView.LayersClear

<FM>Declaration<FC>
procedure LayersClear;

<FM>Description<FN>
LayersClear removes all layers.

Note: After completion only the background layer will remain (as an ImageEnView always requires at least one layer).
!!}
// remove all layers
procedure TImageEnView.LayersClear;
var
  i: Integer;
begin
  for i := fLayers.Count-1 downto 1 do
  begin
    Layers[i].Free;
    fLayers.Delete(i);
  end;
  fLayersCurrent := -1;
  SetLayersCurrent(0);
  Layers[0].SetDefaults;
  with Layers[0] do
  begin
    VisibleBox := false;
    Locked := true;
  end;
  Clear;
end;

{!!
<FS>TImageEnView.LayersMove

<FM>Declaration<FC>
procedure LayersMove(CurIndex, NewIndex: Integer);

<FM>Description<FN>
Moves the layer at index, <FC>CurIndex<FN>, to the position, <FC>NewIndex<FN>.

NewIndex can be one of the following:
<TABLE>
<R> <C>>= 0</C> <C>The new insertion index</C> </R>
<R> <C>IEN_Send_To_Back</C> <C>The layer will become the new background layer (layer 0)</C> </R>
<R> <C>IEN_Send_Backward</C> <C>The layer will move closer to the background</C> </R>
<R> <C>IEN_Bring_Forward</C> <C>The layer will move closer to the foreground</C> </R>
<R> <C>IEN_Bring_To_Front</C> <C>The layer will become the topmost one (in front of all others)</C> </R>
</TABLE>

<FM>Example<FN>
// Move the current layer forward
ImageEnView1.LayersMove(ImageEnView1.LayersCurrent, IEN_Bring_Forwards);
!!}
procedure TImageEnView.LayersMove(CurIndex, NewIndex: integer);
var
  curLayer, layerMask, movingLayer: TIELayer;
  a, b: Integer;
begin
  case NewIndex of
    IEN_Send_To_Back    : NewIndex := 0;
    IEN_Send_Backward   : NewIndex := CurIndex - 1;
    IEN_Bring_Forward   : NewIndex := CurIndex + 1;
    IEN_Bring_To_Front  : NewIndex := fLayers.Count - 1;
  end;                                          

  if (NewIndex < 0) or (NewIndex >= fLayers.Count) or (CurIndex = NewIndex) then
    exit;
  // sync current layer
  Layers[fLayersCurrent].Bitmap := fIEBitmap;
  // save current layer index and make no current layer
  if fLayersCurrent>-1 then
    curLayer := Layers[fLayersCurrent]
  else
    curLayer := nil;
  fLayersCurrent := -1;
  // check if it has a layer mask
  if (CurIndex<fLayers.Count-1) and (Layers[CurIndex+1].IsMask) then
  begin
    layerMask := Layers[CurIndex+1];
    if NewIndex=CurIndex+1 then
      inc(NewIndex);
  end
  else
    layerMask := nil;
  // move layer
  movingLayer := Layers[CurIndex];
  fLayers.Move(CurIndex, NewIndex);
  // move layer mask if exists
  if layerMask<>nil then
  begin
    a := fLayers.IndexOf(layerMask);
    b := fLayers.IndexOf(movingLayer);
    if b<fLayers.Count-1 then
      inc(b);
    fLayers.Move( a,  b);
  end;
  // set old current layer
  if curLayer<>nil then
    SetLayersCurrent( fLayers.IndexOf(curLayer) );
end;

// make sure all layers have same sizes (if fLayerSync=true)
// synchornize current fBitmap and fAlphaChannel with layers list
procedure TImageEnView.DoLayersSync;
var
  i: integer;
begin
  if fBitmap <> nil then
    fIEBitmap.EncapsulateTBitmap(fBitmap, false); // only sync properties
  // sync current layer
  Layers[fLayersCurrent].Bitmap := fIEBitmap;

  fMaxLayerWidth := Layers[0].Bitmap.Width;
  fMaxLayerHeight := Layers[0].Bitmap.Height;

  if fLayersSync and (fLayers.Count > 1) then
  begin // useful only for more than one layer
    // search for maximum sizes
    for i := 1 to fLayers.Count - 1 do
    begin
      if Layers[i].Bitmap.Width > fMaxLayerWidth then
        fMaxLayerWidth := Layers[i].Bitmap.Width;
      if Layers[i].Bitmap.Height > fMaxLayerHeight then
        fMaxLayerHeight := Layers[i].Bitmap.Height;
    end;
    // set all layers same sizes
    for i := 0 to fLayers.Count - 1 do
    begin
      if (Layers[i].Bitmap.Width <> fMaxLayerWidth) or (Layers[i].Bitmap.Height <> fMaxLayerHeight) then
      begin
        Layers[i].Bitmap.AlphaChannel; // this create alpha channel (requested when resizing layers)
        Layers[i].Bitmap.Resize(fMaxLayerWidth, fMaxLayerHeight, Background, 0, iehLeft, ievTop);
      end;
      if Layers[i].Bitmap.HasAlphaChannel and ((Layers[i].Bitmap.AlphaChannel.Width <> fMaxLayerWidth) or (Layers[i].Bitmap.AlphaChannel.Height <> fMaxLayerHeight)) then
        Layers[i].Bitmap.AlphaChannel.Resize(fMaxLayerWidth, fMaxLayerHeight, 0, 0, iehLeft, ievTop);
    end;
  end;
end;

{!!
<FS>TImageEnView.FindLayerAt

<FM>Declaration<FC>
function FindLayerAt(x, y: Integer; SelectablesOnly: Boolean = true): Integer;

<FM>Description<FN>
FindLayerAt returns the layer index under the position x, y (in client area coordinates).
If SelectablesOnly is true, it won't return any layer which isn't selectable.
Returns -1 if no layer was found.
!!}
function TImageEnView.FindLayerAt(x, y: integer; SelectablesOnly: Boolean): integer;
var
  rect: TRect;
  lyr: TIELayer;
  xx, yy: Integer;
begin
  for result := fLayers.Count - 1 downto 0 do
  begin
    lyr := TIELayer(fLayers[result]);
    rect := lyr.ClientAreaBox;
    if (lyr.Selectable or not fLayersSelectConstrains or not SelectablesOnly) and lyr.Visible and IEPointInRect(x, y, Rect) then
      if (not lyr.Bitmap.HasAlphaChannel) or (iesoSelectTranspLayers in fSelectionOptions) then
        break
      else
      begin
        xx := ilimit( lyr.ConvXScr2Bmp(x), 0, lyr.Bitmap.Width-1 );
        yy := ilimit( lyr.ConvYScr2Bmp(y), 0, lyr.Bitmap.Height-1 );
        if lyr.Bitmap.Alpha[ xx, yy ] >0 then
          break;
      end;
  end;
end;

function TImageEnView.IsPointInsideLayer(x, y: Integer; layer: Integer): Boolean;
var
  lyr: TIELayer;
begin
  result := false;
  if layer>-1 then
  begin
    lyr := TIELayer(fLayers[layer]);
    if lyr.Rotate = 0 then
      with lyr.ClientAreaBox do
        result := lyr.Visible and IEPointInRect(x, y, Left, Top, Right, Bottom)
    else
      result := lyr.Visible and IEISPointInPoly(x, y, lyr.DrawingInfo.RotatedDest);
  end;
end;

function CoCreateGuid(out guid: TGUID): HResult; stdcall; external 'ole32.dll' name 'CoCreateGuid';

constructor TIELayer.Create(Owner: TObject; TemplateBitmap: TIEBitmap; JustAssign: boolean);
var
  bmp: TBitmap;
begin
  inherited Create;
  CoCreateGuid(GUID);
  fOwner := Owner;
  fCachedLayerMask := nil;
  if JustAssign then
  begin
    Bitmap := TemplateBitmap;
  end
  else
  begin
    Bitmap := TIEBitmap.Create;
    if assigned(TemplateBitmap) then
    begin
      if TemplateBitmap.EncapsulatedFromTBitmap then
      begin
        bmp := TBitmap.Create();
        bmp.Width  := 1;
        bmp.Height := 1;
        bmp.PixelFormat := TemplateBitmap.VclBitmap.PixelFormat;
        bmp.Width  := TemplateBitmap.Width;
        bmp.Height := TemplateBitmap.Height;
        Bitmap.EncapsulateTBitmap(bmp, true);
      end
      else
      begin
        Bitmap.Location := TemplateBitmap.Location;
        Bitmap.Allocate(TemplateBitmap.Width, TemplateBitmap.Height, TemplateBitmap.PixelFormat);
      end;
    end;
  end;
  FreeBitmapOnDestroy := true;
  SetDefaults;
end;

procedure TIELayer.SetDefaults;
begin
  Visible := true;
  Transparency := 255; // full opaque
  Opacity := 1.0;      // full opaque
  PosX := 0;
  PosY := 0;
  ClientAreaBox := Rect(0, 0, 0, 0);
  ResizedWidth := 0;
  ResizedHeight := 0;
  Cropped := false;
  VisibleBox := true;
  Selectable := true;
  Locked := false;
  Magnify.Enabled := false;
  Magnify.Rate := 2;
  Magnify.Style := iemRectangle;
  Magnify.Source := iemCanvas;
  Operation := ielNormal;
  UserData := nil;
  UserDataLen := 0;
  IsMask := false;
  DrawOuter := false;
  FillChar(DrawingInfo, sizeof(DrawingInfo), 0);
  Rotate := 0;
  RotateCenterX := 0.5;
  RotateCenterY := 0.5;
end;

destructor TIELayer.Destroy;
begin
  if FreeBitmapOnDestroy then
  begin
    if Bitmap.EncapsulatedFromTBitmap then
    begin
      Bitmap.VclBitmap.Free;
      Bitmap.VclBitmap := nil;
    end;
    FreeAndNil(Bitmap);
  end;
  if UserDataLen>0 then
    freemem(UserData);
  if assigned(fCachedLayerMask) then
    FreeAndNil(fCachedLayerMask);

  inherited Destroy;
end;

{!!
<FS>TIELayer.Assign

<FM>Declaration<FC>
procedure Assign(Source: <A TIELayer>);

<FM>Description<FN>
Copy the content of a layer to the current one.
!!}
procedure TIELayer.Assign(Source: TIELayer);
begin
  ResizedWidth      := Source.ResizedWidth;
  ResizedHeight     := Source.ResizedHeight;
  Bitmap.Assign(Source.Bitmap);
  Visible           := Source.Visible;
  VisibleBox        := Source.VisibleBox;
  Selectable        := Source.Selectable;
  Transparency      := Source.Transparency;
  Opacity           := Source.Opacity;
  PosX              := Source.PosX;
  PosY              := Source.PosY;
  ClientAreaBox     := Source.ClientAreaBox;
  Cropped           := Source.Cropped;
  Locked            := Source.Locked;
  Magnify           := Source.Magnify;
  Operation         := Source.Operation;
  UserData          := Source.UserData;
  UserDataLen       := Source.UserDataLen;
  ResampleFilter    := Source.ResampleFilter;
  UseResampleFilter := Source.UseResampleFilter;
  Name              := Source.Name;
  IsMask            := Source.IsMask;
  DrawOuter         := Source.DrawOuter;
end;


{!!
<FS>TIELayer.Swap

<FM>Declaration<FC>
procedure Swap(OtherLayer: <A TIELayer>);

<FM>Description<FN>
Swaps content of two layers.
!!}
procedure TIELayer.Swap(OtherLayer: TIELayer);
begin
  IESwap(ResizedWidth, OtherLayer.ResizedWidth);
  IESwap(ResizedHeight, OtherLayer.ResizedHeight);
  OtherLayer.Bitmap.SwitchTo(Bitmap);
  IESwap(Visible, OtherLayer.Visible);
  IESwap(VisibleBox, OtherLayer.VisibleBox);
  IESwap(Selectable, OtherLayer.Selectable);
  IESwap(Transparency, OtherLayer.Transparency);
  IESwap(Opacity, OtherLayer.Opacity);
  IESwap(PosX, OtherLayer.PosX);
  IESwap(PosY, OtherLayer.PosY);
  IESwap(Cropped, OtherLayer.Cropped);
  IESwap(Locked, OtherLayer.Locked);
  IESwap(Magnify, OtherLayer.Magnify);
  IESwap(Operation, OtherLayer.Operation);
  IESwap(UserData, OtherLayer.UserData);
  IESwap(UserDataLen, OtherLayer.UserDataLen);
  IESwap(ResampleFilter, OtherLayer.ResampleFilter);
  IESwap(UseResampleFilter, OtherLayer.UseResampleFilter);
  IESwap(Name, OtherLayer.Name);
  IESwap(IsMask, OtherLayer.IsMask);
  IESwap(DrawOuter, OtherLayer.DrawOuter);
  IESwap(FreeBitmapOnDestroy, OtherLayer.FreeBitmapOnDestroy);
  IESwap(GUID, OtherLayer.GUID);

  if assigned(fOwner) and ((fOwner as TImageEnView).fIEBitmap = OtherLayer.Bitmap) then
  begin
    (fOwner as TImageEnView).fIEBitmap := Bitmap;
    if Bitmap.EncapsulatedFromTBitmap then
      (fOwner as TImageEnView).fBitmap := Bitmap.VclBitmap
    else
      (fOwner as TImageEnView).fBitmap := nil;
  end;
end;


procedure TIELayer.SaveInfo(Stream: TStream; SaveBitmap: Boolean);
var
  l, sz: integer;
begin
  sz := 0;
  l := Stream.Position;
  Stream.Write(l, sizeof(integer));
  Stream.Write(GUID, sizeof(TGuid));
  Stream.Write(ResizedWidth, sizeof(ResizedWidth));
  Stream.Write(ResizedHeight, sizeof(ResizedHeight));
  Stream.Write(Visible, sizeof(Visible));
  Stream.Write(VisibleBox, sizeof(VisibleBox));
  Stream.Write(Selectable, sizeof(Selectable));
  Stream.Write(Transparency, sizeof(Transparency));
  Stream.Write(Opacity, sizeof(Opacity));
  Stream.Write(PosX, sizeof(PosX));
  Stream.Write(PosY, sizeof(PosY));
  Stream.Write(ClientAreaBox, sizeof(ClientAreaBox));
  Stream.Write(Cropped, sizeof(Cropped));
  Stream.Write(Locked, sizeof(Locked));
  Stream.Write(Magnify, sizeof(Magnify));
  Stream.Write(Operation, sizeof(Operation));
  Stream.Write(UserDataLen, sizeof(UserDataLen));
  if (UserData <> nil) and (UserDataLen > 0) then
    Stream.Write(pbyte(UserData)^, UserDataLen);
  Stream.Write(ResampleFilter, sizeof(ResampleFilter));
  Stream.Write(UseResampleFilter, sizeof(UseResampleFilter));
  IESaveStringToStream(Stream, Name);
  Stream.Write(IsMask, sizeof(IsMask));
  Stream.Write(DrawOuter, sizeof(DrawOuter));
  Stream.Write(fRotate, sizeof(double));
  Stream.Write(RotateCenterX, sizeof(double));
  Stream.Write(RotateCenterY, sizeof(double));
  Stream.Write(SaveBitmap, sizeof(boolean));
  if SaveBitmap then
    Bitmap.SaveRAWToBufferOrStream(nil, Stream, true);
  sz := Stream.Position - l;
  Stream.Position := l;
  Stream.Write(sz, sizeof(integer));
  Stream.Position := l + sz;
end;

procedure TIELayer.LoadInfo(Stream: TStream);
var
  sz: integer;
  BitmapSaved: boolean;
begin
  Stream.Read(sz, sizeof(sz));
  Stream.Read(GUID, sizeof(TGuid));
  Stream.Read(ResizedWidth, sizeof(ResizedWidth));
  Stream.Read(ResizedHeight, sizeof(ResizedHeight));
  Stream.Read(Visible, sizeof(Visible));
  Stream.Read(VisibleBox, sizeof(VisibleBox));
  Stream.Read(Selectable, sizeof(Selectable));
  Stream.Read(Transparency, sizeof(Transparency));
  Stream.Read(Opacity, sizeof(Opacity));
  Stream.Read(PosX, sizeof(PosX));
  Stream.Read(PosY, sizeof(PosY));
  Stream.Read(ClientAreaBox, sizeof(ClientAreaBox));
  Stream.Read(Cropped, sizeof(Cropped));
  Stream.Read(Locked, sizeof(Locked));
  Stream.Read(Magnify, sizeof(Magnify));
  Stream.Read(Operation, sizeof(Operation));

  if (UserDataLen > 0) and (UserData <> nil) then
  begin
    freemem(UserData);
    UserDataLen := 0;
    UserData    := nil;
  end;

  Stream.Read(UserDataLen, sizeof(UserDataLen));
  if UserDataLen > 0 then
  begin
    getmem(UserData, UserDataLen);
    Stream.Read(pbyte(UserData)^, UserDataLen);
  end;

  Stream.Read(ResampleFilter, sizeof(ResampleFilter));
  Stream.Read(UseResampleFilter, sizeof(UseResampleFilter));
  IELoadStringFromStream(Stream, Name);
  Stream.Read(IsMask, sizeof(IsMask));
  Stream.Read(DrawOuter, sizeof(DrawOuter));
  Stream.Read(fRotate, sizeof(double));
  Stream.Read(RotateCenterX, sizeof(double));
  Stream.Read(RotateCenterY, sizeof(double));
  Stream.Read(BitmapSaved, sizeof(boolean));
  if BitmapSaved then
    Bitmap.LoadRAWFromBufferOrStream(nil, Stream);
end;


function TIELayer.GetWidth: integer;
begin
  if ResizedWidth = 0 then
    result := Bitmap.Width
  else
    result := ResizedWidth;
end;

function TIELayer.GetHeight: integer;
begin
  if ResizedHeight = 0 then
    result := Bitmap.Height
  else
    result := ResizedHeight;
end;

procedure TIELayer.SetRotate(v: Double);
begin
  if (frac(v) = 0) and (trunc(v) mod 360 = 0) then
    fRotate := 0
  else
    fRotate := v;
end;

{!!
<FS>TIELayer.ConvXScr2Bmp

<FM>Declaration<FC>
function ConvXScr2Bmp(x: Integer): Integer;

<FM>Description<FN>
Converts screen coordinates to bitmap coordinates, with consideration to the layer position and size.
!!}
function TIELayer.ConvXScr2Bmp(x: Integer): Integer;
var
  ieview: TImageEnView;
begin
  ieview := (fOwner as TImageEnView);
  if Width <> 0 then
    // note 1.0 required to make operation as double (otherwise overflow may occur)
    result := trunc( 1.0 * ieview.XScr2Bmp(x - trunc(PosX * ieview.fZoomD100X)) * Bitmap.Width / Width )
  else
    result := 0;
end;

{!!
<FS>TIELayer.ConvYScr2Bmp

<FM>Declaration<FC>
function ConvYScr2Bmp(y: Integer): Integer;

<FM>Description<FN>
Converts screen coordinates to bitmap coordinates, with consideration to the layer position and size.
!!}
function TIELayer.ConvYScr2Bmp(y: Integer): Integer;
var
  ieview: TImageEnView;
begin
  ieview := (fOwner as TImageEnView);
  if Height <> 0 then
    // note 1.0 required to make operation as double (otherwise overflow may occur)
    result := trunc( 1.0 * ieview.YScr2Bmp(y - trunc(PosY * ieview.fZoomD100Y)) * Bitmap.Height / Height )
  else
    result := 0;
end;

{!!
<FS>TIELayer.ConvXBmp2Scr

<FM>Declaration<FC>
function ConvXBmp2Scr(x: Integer): Integer;

<FM>Description<FN>
Converts bitmap coordinates to screen coordinates, with consideration to the layer position and size.
!!}
function TIELayer.ConvXBmp2Scr(x: Integer): Integer;
var
  ieview: TImageEnView;
begin
  ieview := (fOwner as TImageEnView);
  result := ieview.XBmp2Scr(trunc(1.0 * x * Width / Bitmap.Width)) + trunc(PosX * ieview.fZoomD100X);  // note 1.0 required to make operation as double (otherwise overflow may occur)
end;

{!!
<FS>TIELayer.ConvYBmp2Scr

<FM>Declaration<FC>
function ConvYBmp2Scr(y: Integer): Integer;

<FM>Description<FN>
Converts bitmap coordinates to screen coordinates, with consideration to the layer position and size.
!!}
function TIELayer.ConvYBmp2Scr(y: Integer): Integer;
var
  ieview: TImageEnView;
begin
  ieview := (fOwner as TImageEnView);
  result := ieview.YBmp2Scr(trunc(1.0 * y * Height / Bitmap.Height)) + trunc(PosY * ieview.fZoomD100Y);  // // note 1.0 required to make operation as double (otherwise overflow may occur)
end;


{!!
<FS>TIELayer.GetLayerMask

<FM>Declaration<FC>
function GetLayerMask(): <A TIELayer>;

<FM>Description<FN>
Returns the layer mask associated to this layer. Returns "nil" if no layer mask exists.
!!}
function TIELayer.GetLayerMask(): TIELayer;
var
  ieview: TImageEnView;
  idx: integer;
begin
  ieview := (fOwner as TImageEnView);
  idx := GetIndex();
  if (idx > - 1) and (idx + 1 < ieview.fLayers.Count) and (ieview.Layers[idx + 1].IsMask) then
    result := ieview.Layers[idx + 1]
  else
    result := nil;
end;


{!!
<FS>TIELayer.GetIndex

<FM>Declaration<FC>
function GetIndex(): integer;

<FM>Description<FN>
Returns the index of this layer inside the <A TImageEnView.Layers> list. Returns -1 if not found.
!!}
// -1 = not found
function TIELayer.GetIndex(): integer;
var
  ieview: TImageEnView;
begin
  ieview := (fOwner as TImageEnView);
  result := ieview.fLayers.IndexOf(self);
end;


{!!
<FS>IELayersMerge

<FM>Declaration<FC>
procedure IELayersMerge(Layer1, Layer2, LayerMask: <A TIELayer>; Destination: <A TIEBitmap>; ZoomFilter: <A TResampleFilter>; Background: TColor; RelativePositions: Boolean);

<FM>Description<FN>
IELayersMerge merges two layers to the destination bitmap.
ZoomFilter specifies the filter to use when layers needs to be stretched.
Background is the color used to fill empty regions.
LayerMask is the upper layer's mask. This can be 'nil' if no layer mask exists.
If RelativePositions = false the output layer should have posx = 0 and posy = 0 because the required space is added to the destination bitmap.

<FM>Example<FC>
// this merges all layers of ImageEnView1 to tempIE (invisible), then saves to jpeg file
var
  tempIE: TImageEnView;
  tempBMP: TIEBitmap;
  i: Integer;
begin
  tempIE := TImageEnView.Create(nil);

  tempIE.Layers[0].Bitmap.Assign( ImageEnView1.Layers[0].Bitmap );

  tempBMP := TIEBitmap.Create;

  for i := 1 to ImageEnView1.LayersCount-1 do
  begin
    IELayersMerge( tempIE.Layers[0], ImageEnView1.Layers[i], nil, tempBMP, rfNone, clBlack );
    tempIE.Layers[0].Bitmap.Assign( tempBMP );
  end;

  tempBMP.free;

  tempIE.BackGround := clBlack;
  tempIE.RemoveAlphaChannel(true);

  tempIE.IO.SaveToFile('output.jpg');

  tempIE.free;
end;
!!}
procedure IELayersMerge(Layer1, Layer2, LayerMask: TIELayer; Destination: TIEBitmap; ZoomFilter: TResampleFilter; Background: TColor; RelativePositions: Boolean);
var
  // l0...is layer 0 (down layer)
  // l1...is layer 1 (up layer)
  // l2...is layer mask of layer 1
  l0bitmap, l1bitmap, l2bitmap: TIEBitmap;
  l0alpha, l1alpha: TIEBitmap;
  l0transp, l1transp: integer;
  l0palpha, l1palpha: pbyte;
  l1transp255, l0transp255: double;
  l1opacity, l0opacity: double;
  l0prgb, l1prgb: PRGB;
  l2pbyte: pbyte;
  l0posx, l1posx, l2posx: integer;
  l0posy, l1posy, l2posy: integer;
  l0row, l1row, l2row: integer;
  l0col, l1col, l2col: integer;
  l1crop: boolean;
  proc0, proc1: TImageEnProc;
  l0width, l1width, l2width: integer;
  l0height, l1height, l2height: integer;
  outbitmap: TIEBitmap;
  outalpha: TIEBitmap;
  outwidth, outheight: Integer;
  row, col: integer;
  SimAlphaRow0, SimAlphaRow1: pbyte;
  ConvRGBRow0, ConvRGBRow1: PRGB;
  pdestalpha: pbyte;
  pdest: PRGB;
  aa, bb, cc, opt1: double;
  maxwidth, maxheight: integer;
  minposx, minposy, maxposx: Integer;
  all0: pbyte;
  allBak: PRGB;
  vv: TRGB;
  op: TIERenderOperation;
  i, a, r, g, b: integer;
begin
  l2Width  := 0;
  l2Height := 0;

  op := Layer2.Operation;

  proc0 := TImageEnProc.Create(nil);
  proc1 := TImageEnProc.Create(nil);

  // layer 0 vars
  l0bitmap := Layer1.Bitmap;
  l0width  := Layer1.Width;
  l0height := Layer1.Height;
  if (l0width <> l0bitmap.Width) or (l0height <> l0bitmap.Height) then
  begin
    // resize the layer
    l0bitmap := TIEBitmap.Create;
    proc0.AttachedIEBitmap := Layer1.Bitmap;
    proc0.ResampleTo(l0bitmap, l0width, l0height, ZoomFilter);
  end;
  l0posx := Layer1.PosX;
  l0posy := Layer1.PosY;
  if l0bitmap.HasAlphaChannel then
    l0alpha := l0bitmap.AlphaChannel
  else
    l0alpha := nil;
  l0transp := Layer1.Transparency;
  l0transp255 := l0transp / 255.0;
  l0opacity := Layer1.Opacity;
  getmem(SimAlphaRow0, l0bitmap.Width);
  fillchar(SimAlphaRow0^, l0bitmap.Width, 255);
  getmem(ConvRGBRow0, l0bitmap.Width * sizeof(TRGB));

  // layer 1 vars
  l1bitmap := Layer2.Bitmap;
  l1width  := Layer2.Width;
  l1height := Layer2.Height;
  l1crop   := Layer2.Cropped and not RelativePositions;
  if (l1width <> l1bitmap.Width) or (l1height <> l1bitmap.Height) then
  begin
    // resize the layer
    l1bitmap := TIEBitmap.Create;
    proc1.AttachedIEBitmap := Layer2.Bitmap;
    proc1.ResampleTo(l1bitmap, l1width, l1height, ZoomFilter);
  end;
  l1posx := Layer2.PosX;
  l1posy := Layer2.PosY;
  if l1bitmap.HasAlphaChannel then
    l1alpha := l1bitmap.AlphaChannel
  else
    l1alpha := nil;
  l1transp := Layer2.Transparency;
  l1transp255 := l1transp / 255.0;
  l1opacity := Layer2.Opacity;
  getmem(SimAlphaRow1, l1bitmap.Width);
  fillchar(SimAlphaRow1^, l1bitmap.Width, 255);
  getmem(ConvRGBRow1, l1bitmap.Width * sizeof(TRGB));

  // layer mask vars (of layer 1)
  if LayerMask <> nil then
  begin
    l2width  := LayerMask.Width;
    l2height := LayerMask.Height;
    if (l2width <> LayerMask.Bitmap.Width) or (l2height <> LayerMask.Bitmap.Height) then
    begin
      // resize the layer
      l2bitmap := TIEBitmap.Create;
      l2bitmap.Allocate(l2width, l2height, ie8g);
      _IEBmpStretchEx(LayerMask.Bitmap, l2bitmap, nil, nil);
    end
    else
      l2bitmap := LayerMask.Bitmap;
    l2posx := LayerMask.PosX;
    l2posy := LayerMask.PosY;
  end
  else
  begin
    l2posx := 0;
    l2posy := 0;
  end;

  // output vars
  if l0posx < 0 then
  begin
    inc(l1posx, -l0posx);
    inc(l2posx, -l0posx);
    l0posx := 0;
  end;
  if l0posy < 0 then
  begin
    inc(l1posy, -l0posy);
    inc(l2posy, -l0posy);
    l0posy := 0;
  end;
  if (l1posx < 0) and not l1crop then
  begin
    inc(l0posx, -l1posx);
    inc(l2posx, -l1posx);
    l1posx := 0;
  end;
  if (l1posy < 0) and not l1crop then
  begin
    inc(l0posy, -l1posy);
    inc(l2posy, -l1posy);
    l1posy := 0;
  end;

  if l1crop then
  begin
    maxwidth  := l0width + l0posx;
    maxheight := l0height + l0posy;
  end
  else
  begin
    maxwidth  := imax(l0width + l0posx, l1width + l1posx);
    maxheight := imax(l0height + l0posy, l1height + l1posy);
  end;

  if RelativePositions and not l1crop then
  begin
    minposx := imin(l0posx, l1posx);
    minposy := imin(l0posy, l1posy);
  end
  else
  begin
    minposx := 0;
    minposy := 0;
  end;

  outwidth  := maxwidth - minposx;
  outheight := maxheight - minposy;

  outbitmap := Destination;
  if (outwidth <> outbitmap.Width) or (outheight <> outbitmap.Height) or (outbitmap.PixelFormat <> ie24RGB) then
    outbitmap.Allocate(outwidth, outheight, ie24RGB);
  outalpha := outbitmap.AlphaChannel; // this creates alpha channel
  outalpha.Full := false;

  getmem(all0, outwidth * 4);
  fillchar(all0^, outwidth * 4, 0);

  // set background row
  vv := TColor2TRGB(Background);
  getmem(allBak, outwidth * 4);
  pdest := allBak;
  for i := 1 to outwidth do
  begin
    pdest^ := vv;
    inc(pdest);
  end;

  // combine images
  for row := minposy to minposy + outheight - 1 do
  begin

    // set layer 0 pointers
    l0row := row - l0posy;
    if (l0row >= 0) and (l0row < l0bitmap.Height) then
    begin
      if assigned(l0alpha) then
        l0palpha := l0alpha.Scanline[l0row]
      else
        l0palpha := SimAlphaRow0;
      if l0bitmap.PixelFormat = ie24RGB then
        l0prgb := l0bitmap.Scanline[l0row]
      else
      begin
        // convert one row of l0bitmap to 24bit
        _ConvRow1To24(l0bitmap.Scanline[l0row], pbyte(ConvRGBRow0), l0Width);
        l0prgb := ConvRGBRow0;
      end;
    end
    else
    begin
      l0palpha := all0;
      l0prgb := allBak;
    end;

    // set layer 1 pointers
    l1row := row - l1posy;
    if (l1row >= 0) and (l1row < l1Height) then
    begin
      if assigned(l1alpha) then
        l1palpha := l1alpha.ScanLine[l1row]
      else
        l1palpha := SimAlphaRow1;
      if l1bitmap.PixelFormat = ie24RGB then
        l1prgb := l1bitmap.Scanline[l1row]
      else
      begin
        // convert one row of l1bitmap to 24bit
        _ConvRow1To24(l1bitmap.Scanline[l1row], pbyte(ConvRGBRow1), l1Width);
        l1prgb := ConvRGBRow1;
      end;
    end
    else
    begin
      l1palpha := all0;
      l1prgb := allBak;
    end;

    // set layer mask pointers
    if LayerMask <> nil then
    begin
      l2row := row - l2posy;
      if (l2row >= 0) and (l2row < l2Height) then
        l2pbyte := l2bitmap.Scanline[l2row]
      else
        l2pbyte := all0;
    end
    else
    begin
      l2pbyte := all0;
    end;

    // set output pointers
    pdest := outbitmap.Scanline[row - minposy];
    pdestalpha := outalpha.Scanline[row - minposy];

    // main loop
    l0col := minposx - l0posx;
    l1col := minposx - l1posx;
    l2col := minposx - l2posx;
    maxposx := minposx + outwidth - 1;
    if l1col > 0 then
    begin
      inc(l1prgb, l1col);
      inc(l1palpha, l1col);
    end;
    if l2col > 0 then
      inc(l2pbyte, l2col);
    for col := minposx to maxposx do
    begin
      // combine pixels
      if (l0col >= 0) and (l1col >= 0) and (l0col < l0Width) and (l1col < l1Width) then
      begin
        if l0palpha^ < l0transp then
          aa := l0palpha^ / 255.0 * l0opacity
        else
          aa := l0transp255 * l0opacity;

        if LayerMask <> nil then
        begin
          if (l2col >= 0) and (l2col < l2Width) then
          begin
            bb := imin(imin(l1palpha^, l2pbyte^), l1transp) / 255 * l1opacity;
            inc(l2pbyte);
          end
          else
            bb := 0.0;
        end
        else
        begin
          if l1palpha^ < l1transp then
            bb := l1palpha^ / 255.0 * l1opacity
          else
            bb := l1transp255 * l1opacity;
        end;

        opt1 := (1.0 - bb) * aa;
        cc := bb + opt1;

        vv := l0prgb^;

        if op = ielNormal then
          vv := l1prgb^
        else
          IEBlend(l1prgb^, vv, op, row);

        if cc <> 0.0 then
        begin
          r := round((bb * vv.r + opt1 * l0prgb^.r) / cc);
          g := round((bb * vv.g + opt1 * l0prgb^.g) / cc);
          b := round((bb * vv.b + opt1 * l0prgb^.b) / cc);

          pdest^.r := blimit(r);
          pdest^.g := blimit(g);
          pdest^.b := blimit(b);
        end
        else
          pdest^ := vv;

        a := round(cc * 255.0);
        pdestalpha^ := ilimit(a, 0, 255);

        inc(l0palpha);
        inc(l0prgb);
        inc(l1palpha);
        inc(l1prgb);
      end
      else
      if (l0col >= 0) and (l0col < l0Width) then
      begin
        // copy only l0bitmap
        pdest^ := l0prgb^;
        pdestalpha^ := trunc(imin(l0palpha^, l0transp) * l0opacity);
        inc(l0palpha);
        inc(l0prgb);
      end
      else
      if (l1col >= 0) and (l1col < l1Width) then
      begin
        // copy only l1bitmap
        pdest^ := l1prgb^;
        if LayerMask <> nil then
        begin
          if (l2col >= 0) and (l2col < l2Width) then
          begin
            pdestalpha^ := trunc(imin(imin(l1palpha^, l2pbyte^), l1transp) * l1opacity);
            inc(l2pbyte);
          end
          else
            pdestalpha^ := 0;
        end
        else
        begin
          if l1palpha^ < l1transp then
            pdestalpha^ := trunc(l1palpha^ * l1opacity)
          else
            pdestalpha^ := trunc(l1transp * l1opacity);
        end;
        inc(l1palpha);
        inc(l1prgb);
      end
      else
      begin
        // transparent area
        pdestalpha^ := 0;
        pdest^ := TColor2TRGB(BackGround);
      end;
      inc(pdest);
      inc(pdestalpha);
      inc(l0col);
      inc(l1col);
      inc(l2col);
    end;
  end;

  freemem(SimAlphaRow0);
  freemem(SimAlphaRow1);
  freemem(ConvRGBRow0);
  freemem(ConvRGBRow1);
  freemem(all0);
  freemem(allBak);
  if proc0.CanUndo then
    proc0.Undo;
  if proc1.CanUndo then
    proc1.Undo;
  FreeAndNil(proc0);
  FreeAndNil(proc1);
  if (LayerMask <> nil) and (LayerMask.Bitmap <> l2bitmap) then
    FreeAndNil(l2bitmap);

  if l0bitmap <> Layer1.Bitmap then
    l0bitmap.Free;
  if l1bitmap <> Layer2.Bitmap then
    l1bitmap.Free;

  // verify if outalpha is full and remove it if not needed
  outalpha.SyncFull;
  if outalpha.Full then
  begin
    // remove alpha
    outbitmap.RemoveAlphaChannel;
  end;

end;


{!!
<FS>TImageEnView.LayersMergeTo

<FM>Declaration<FC>
procedure LayersMergeTo(Layer1, Layer2: integer; Destination: <A TIEBitmap>);

<FM>Description<FN>
LayersMergeTo merges Layer1 and Layer2 into a TIEBitmap object.
LayersMergeTo makes the new image considering <A TImageEnView.Layers>[].<A TIELayer.Transparency> and the bitmap's alpha channels.
The resulting bitmap will always be 24 bit (ie24RGB).

<FM>Example<FC>
// we want to get a background image and then merge over it another image in semitransparency.
ImageEnView.IO.LoadFromFile('C:\background.jpg');
ImageEnView.LayersAdd;
ImageEnView.IO.LoadFromFile('C:\foreground.jpg');
ImageEnView.Layers[1].Transparency := 128; // the second layer has 50% transparency
ImageEnView.LayersMergeTo(0, 1, ImageEnView2.IEBitmap);
ImageEnView2.IO.SaveToFile('C:\output.jpg');
!!}
procedure TImageEnView.LayersMergeTo(Layer1, Layer2: integer; Destination: TIEBitmap);
var
  UpLayer, DownLayer: Integer;
  LayerMask: TIELayer;
begin
  DownLayer := imin(Layer1, Layer2);
  UpLayer   := imax(Layer1, Layer2);
  if (UpLayer < fLayers.Count - 1) and Layers[UpLayer + 1].IsMask then
    LayerMask := Layers[UpLayer + 1]
  else
    LayerMask := nil;
  IELayersMerge(Layers[DownLayer], Layers[UpLayer], LayerMask, Destination, fZoomFilter, fBackground, DownLayer > 0);
end;

{!!
<FS>TImageEnView.LayersMerge

<FM>Declaration<FC>
procedure LayersMerge(Layer1, Layer2: integer; RemoveUpperLayer: Boolean = true);
procedure LayersMerge(LayerList: array of integer);
procedure LayersMerge(LayerList: <A TIEArrayOfInteger>);

<FM>Description<FN>
LayersMerge merges two or more layers into one layer. The new layer has the lesser index.
LayersMerge makes the new layer considering the <A TImageEnView.Layers>.<A TIELayer.Transparency> and the bitmap's alpha channels.

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Layer1<FN></C> <C>Index of the first layer to merge.</C> </R>
<R> <C><FC>Layer2<FN></C> <C>Index of the second layer to merge.</C> </R>
<R> <C><FC>RemoveUpperLayer<FN></C> <C>If <FC>RemoveUpperLayer<FN> is <FC>false<FN>, the upper layer will not be removed.</C> </R>
<R> <C><FC>LayerList<FN></C> <C>An array of layer indexes to remove. The array must be ordered and all layers will be merged into the layer specified by the first index. Empty list means "all layers".</C> </R>
</TABLE>


<FM>Example<FC>
// merge layer 0 and 1
ImageEnView1.LayersMerge([0, 1]);

// merge layer 0, 1 and 2
ImageEnView1.LayersMerge([0, 1, 2]);

// merge all layers
ImageEnView1.LayersMerge([]);

// we want to get a background image and then merge over it another image in semitransparency.
ImageEnView.IO.LoadFromFile('C:\background.jpg');
ImageEnView.LayersAdd;
ImageEnView.IO.LoadFromFile('C:\foreground.jpg');
ImageEnView.Layers[1].Transparency := 128;  // the second layer has 50% transparency
ImageEnView.LayersMerge(0, 1);   // from now we have only one layer
ImageEnView.IO.SaveToFile('C:\output.jpg');
!!}
procedure TImageEnView.LayersMerge(Layer1, Layer2: integer; RemoveUpperLayer: Boolean);
var
  outlayer: TIELayer;
  lamin, lamax: integer;
  oldnav: TImageEnView;
begin
  oldnav := fNavigator;
  SetNavigator(nil);

  lamin := imin(Layer1, Layer2);
  lamax := imax(Layer1, Layer2);

  outlayer := TIELayer.Create(self, Layers[Layer1].Bitmap, false);

  LayersMergeTo(Layer1, Layer2, outlayer.Bitmap);

  if lamin > 0 then
  begin
    outlayer.PosX := imin( Layers[Layer1].PosX, Layers[Layer2].PosX );
    outlayer.PosY := imin( Layers[Layer1].PosY, Layers[Layer2].PosY );
  end;

  outlayer.Locked     := TIELayer(Layers[lamin]).Locked;
  outlayer.VisibleBox := TIELayer(Layers[lamin]).VisibleBox;
  outlayer.Selectable := TIELayer(Layers[lamin]).Selectable;

  // remove old layers
  if RemoveUpperLayer then
  begin
    if (lamax<fLayers.Count - 1) and Layers[lamax + 1].IsMask then
    begin
      // remove layer mask
      Layers[lamax + 1].Free;
      fLayers.Delete(lamax + 1);
    end;
    Layers[lamax].Free;
    fLayers.Delete(lamax);
  end;
  Layers[lamin].Free;
  fLayers.Delete(lamin);
  // insert new one
  fLayersCurrent := -1;
  fLayers.Insert(lamin, outlayer);
  //
  LayersCurrent := lamin; // this calls update

  SetNavigator(oldnav);
end;


procedure TImageEnView.LayersMerge(LayerList: array of integer);
var
  llist: TIEArrayOfInteger;
  i: integer;
begin
  SetLength(llist, length(LayerList));
  for i := 0 to length(LayerList) - 1 do
    llist[i] := LayerList[i];
  LayersMerge(llist);
end;


// indexes in LayerList must be sorted (lower indexes first)
procedure TImageEnView.LayersMerge(LayerList: TIEArrayOfInteger);
var
  i: integer;
  llistLen: integer;
  llist: array of TIELayer;
  oldnav: TImageEnView;
  outLayer: TIELayer;
  firstLastIndex: integer;
begin
  if length(LayerList) = 1 then // unique invalid case (0 = all layers)
    exit;

  oldnav := fNavigator;
  SetNavigator(nil);

  fLayersCurrent := -1;

  // creates a list of pointers to layers (excluding masks)
  if length(LayerList) = 0 then
  begin
    // empty array means All layers
    SetLength(llist, fLayers.Count);
    llistLen := 0;
    for i := 0 to fLayers.Count - 1 do
      if not Layers[i].IsMask then
      begin
        llist[llistLen] := Layers[i];
        inc(llistLen);
      end;
    firstLastIndex := 0;
  end
  else
  begin
    // get list from LayerList
    SetLength(llist, length(LayerList));
    llistLen := 0;
    for i := 0 to length(LayerList) - 1 do
    begin
      if not Layers[LayerList[i]].IsMask then
      begin
        llist[llistLen] := Layers[LayerList[i]];
        inc(llistLen);
      end;
    end;
    firstLastIndex := LayerList[0];
  end;

  if llistLen > 1 then
  begin
    for i := 1 to llistLen - 1 do
    begin

      outLayer := TIELayer.Create(self, llist[0].Bitmap, false);
      IELayersMerge(llist[0], llist[i], llist[i].GetLayerMask(), outLayer.Bitmap, fZoomFilter, fBackground, firstLastIndex > 0);

      if firstLastIndex > 0 then
      begin
        outlayer.PosX := imin( llist[0].PosX, llist[i].PosX );
        outlayer.PosY := imin( llist[0].PosY, llist[i].PosY );
      end;
      outlayer.Locked     := llist[0].Locked;
      outlayer.VisibleBox := llist[0].VisibleBox;
      outlayer.Selectable := llist[0].Selectable;

      fLayers.Insert(firstLastIndex, outlayer);

      fLayers.Remove(llist[0]);
      fLayers.Remove(llist[i].GetLayerMask());
      fLayers.Remove(llist[i]);

      llist[0].Free();
      llist[i].GetLayerMask().Free();
      llist[i].Free();

      llist[0] := outlayer;

    end;
  end;

  LayersCurrent := firstLastIndex; // this call update
  SetNavigator(oldnav);
end;

// Layers
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{!!
<FS>TImageEnView.Bitmap

<FM>Declaration<FC>
property Bitmap: TBitmap;

<FM>Description<FN>
Bitmap contains the image (current layer) to display.
If <A TImageEnView.LegacyBitmap> is <FC>True<FN>, <A TImageEnView.IEBitmap> is just a wrapper for TBitmap that may be accessed using the Bitmap property.
!!}
// after changing sizes using Bitmap, we should call Update to sync fIEBitmap
function TImageEnView.GetFBitmap: TBitmap;
begin
  result := fBitmap;
end;

{!!
<FS>TImageEnView.IEBitmap

<FM>Declaration<FC>
property IEBitmap: <A TIEBitmap>;

<FM>Description<FN>
IEBitmap contains the image (current layer) to display. The object TIEBitmap also contains the alpha channel of the image.

If <A TImageEnView.LegacyBitmap> is True, IEBitmap is just a wrapper for TBitmap that may be accessed using the Bitmap property.

!!}
function TImageEnView.GetIEBitmap: TIEBitmap;
begin
  result := fIEBitmap;
end;

{!!
<FS>TImageEnView.LegacyBitmap

<FM>Declaration<FC>
property LegacyBitmap: Boolean;

<FM>Description<FN>
If LegacyBitmap is <FC>True<FN>, ImageEn uses TBitmap to store the image, otherwise ImageEn uses <A TIEBitmap>.
TIEBitmap handles images using memory mapped file (for large images) or main memory.
This allows handling of large images and to include input/output and image processing in a multi-threaded environment.
Also TIEBitmap supports a larger number of pixel formats (<A TIEPixelFormat>).

Note: LegacyBitmap should be set to true for new applications.
!!}
procedure TImageEnView.SetLegacyBitmap(Value: boolean);
begin
  if Value <> fLegacyBitmap then
  begin
    if fLegacyBitmap = true then
    begin
      // from fBitmap to fIEBitmap
      fIEBitmap.EncapsulatedFromTBitmap := false; // this allows fIEBitmap to full own fBitmap
      fIEBitmap.Location := ieFile;
      fBitmap := nil; // fBitmap freedom by fIEBitmap
    end
    else
    begin
      // from fIEBitmap to fBitmap
      fIEBitmap.Location := ieTBitmap;
      fIEBitmap.EncapsulatedFromTBitmap := true; // this deny fIEBitmap to own fBitmap
      fBitmap := fIEBitmap.VclBitmap;
    end;
    fLegacyBitmap := Value;
    CallBitmapChangeEvents;
    Update;
  end;
end;

// fOffX, fOffY, frx, fry, fo1x, fo1y, fo2x, fo2y must be valid
procedure TImageEnView.PaintSelection(OutBitmap: TBitmap);
begin
  if fVisibleSelection then
  begin

    if (iesoFilled in fSelectionOptions) and PIEAnimPoly(fHPolySel)^.Enabled then
      with CurrentLayer.DrawingInfo do
        fSelectionMask.InvertCanvas(OutBitmap.Canvas, XDst, YDst, WidthDst, HeightDst, XSrc, YSrc, WidthSrc, HeightSrc);

    if (iesoMarkOuter in fSelectionOptions) and PIEAnimPoly(fHPolySel)^.Enabled then
      with CurrentLayer.DrawingInfo do
        fSelectionMask.DrawOuter(OutBitmap, XDst, YDst, WidthDst, HeightDst, XSrc, YSrc, WidthSrc, HeightSrc, fMarkOuterAlpha, fMarkOuterColor);

  end;
end;

// fOffX, fOffY, frx, fry, fo1x, fo1y, fo2x, fo2y must be valid
procedure TImageEnView.PaintGrid(OutBitmap: TBitmap);
var
  x1, y1, x2, y2, y, x: integer;
  dx, dy: double;
  lyr: TIELayer;
begin
  if fDisplayGridLyr = -1 then
    lyr := CurrentLayer
  else
    lyr := Layers[imin(fDisplayGridLyr, LayersCount-1)];

  if fDisplayGrid and lyr.Visible and ((fZoomX >= IEGlobalSettings().MinZoomDisplayGrid) or (fZoomY >= IEGlobalSettings().MinZoomDisplayGrid)) and ((MaxLayerWidth > 1) or (MaxLayerHeight > 1)) then
  begin
    with OutBitmap.Canvas do
    begin
      Pen.Assign(IEGlobalSettings().GridPen);
      y1 := lyr.DrawingInfo.YDst;
      y2 := lyr.DrawingInfo.YDst + lyr.DrawingInfo.HeightDst;
      dx := lyr.DrawingInfo.WidthDst / lyr.DrawingInfo.WidthSrc;
      x := 0;
      while x <= lyr.DrawingInfo.WidthSrc do
      begin
        x1 := round(lyr.DrawingInfo.XDst + x*dx);
        MoveTo(x1, y1);
        LineTo(x1, y2);
        inc(x);
      end;
      x1 := lyr.DrawingInfo.XDst;
      x2 := lyr.DrawingInfo.XDst + lyr.DrawingInfo.WidthDst;
      dy := lyr.DrawingInfo.HeightDst / lyr.DrawingInfo.HeightSrc;
      y := 0;
      while y <= lyr.DrawingInfo.HeightSrc do
      begin
        y1 := round(lyr.DrawingInfo.YDst + y*dy);
        MoveTo(x1, y1);
        LineTo(x2, y1);
        inc(y);
      end;
    end;

    if (fHighlightedPixel.X >= 0) and (fHighlightedPixel.Y >= 0) then
    begin
      x1 := lyr.ConvXBmp2Scr(fHighlightedPixel.X);
      y1 := lyr.ConvYBmp2Scr(fHighlightedPixel.Y);
      x2 := lyr.ConvXBmp2Scr(fHighlightedPixel.X + 1);
      y2 := lyr.ConvYBmp2Scr(fHighlightedPixel.Y + 1);
      with TIECanvas.Create(OutBitmap.Canvas, true, true) do
      begin
        Pen.Color := fHighlightedPixelColor;
        Pen.Width := 2;
        Brush.Style := bsClear;
        RoundRect(x1, y1, x2, y2, 5, 5);
        Free;
      end;
    end;

  end;
end;

{!!
<FS>TImageEnView.GetRenderRectangles

<FM>Declaration<FC>
procedure GetRenderRectangles(var xDst, yDst, dxDst, dyDst: integer; var xSrc, ySrc, dxSrc, dySrc: integer);

<FM>Description<FN>
Returns the rendered rectangle related to the client area and the source rectangle related to the bitmap of current layer.

<FC>xDst, yDst, dxDst, dyDst<FN> : the destination x, y and width, height where the image has been rendered

<FC>xSrc, ySrc, dxSrc, dySrc<FN> : the source x, y and width, height of the source image.

!!}
procedure TImageEnView.GetRenderRectangles(var xDst, yDst, dxDst, dyDst: integer; var xSrc, ySrc, dxSrc, dySrc: integer);
begin
  DoLayersSync; // make sure all layers have same sizes
  CalcPaintCoords;
  CreateCoordConvLUT; // recalculates coordinate conversion LUT
  xDst  := fOffX;
  yDst  := fOffY;
  dxDst := frx;
  dyDst := fry;
  xSrc  := fo1x;
  ySrc  := fo1y;
  dxSrc := fo2x;
  dySrc := fo2y;
end;

// ABitmap must be pf24bit, width=clientwidth height=clientheight
// ABitmapScanline can be "nil"
// If UpdRect=nil updates entire ABitmap width*height
procedure TImageEnView.PaintToEx(ABitmap: TIEBitmap; ABitmapScanline: ppointerarray; UpdRect: PRect; drawBackground: boolean; drawGadgets: boolean);
var
  i, j: integer;
  lBitmap: TIEBitmap;
  IsFirst: boolean;
  Transparency: integer;  // value of TIELayer.Transpacency field
  Opacity: double;        // value of TIELayer.Opacity field
  zf: TResampleFilter;    // effective zoom filter
  XSrc, YSrc, WidthSrc, HeightSrc: integer;
  XDst, YDst, WidthDst, HeightDst: integer;
  dummy2, dummy3: PInteger;
  layer: TIELayer;
  w, h: integer;
  x1, y1, x2, y2: integer;
  x, y: integer;
  layerMask: TIELayer;
  oldAlpha: TIEBitmap;
  newAlpha: TIEBitmap;
  FreeLayerMask: boolean;
  px: PRGB;
  rotbmp, tmpbmp: TIEBitmap;
  lresw, lresh: integer;
  lposx, lposy: integer;

  procedure SubDrawBackground(force: boolean);
  var
    bHandledBG: boolean;
  begin
    if force or
       (layer.PosX <> 0) or
       (layer.PosY <> 0) or
       (layer.Rotate <> 0) or
       (fOffX > 0) or
       (fOffY > 0) or
       (frx < ABitmap.Width) or
       (fry < ABitmap.Height) or
       (fEnableAlphaChannel and lBitmap.HasAlphaChannel and (not lBitmap.AlphaChannel.Full)) or
       (IsFirst and (layer.Operation <> ielNormal)) or 
       (Transparency < 255) or
       (Opacity < 1.0) then
    begin
      if UpdRect <> nil then
      begin
        with UpdRect^ do
          IntersectClipRect(ABitmap.Canvas.Handle, left, top, right + 1, bottom + 1);
      end;
      bHandledBG := false;
      if assigned(fOnDrawBackground) then
        fOnDrawBackground(self, ABitmap.Canvas, Rect(0, 0, ABitmap.Width, ABitmap.Height), bHandledBG);
      if bHandledBG=false then
        IEDrawBackground(ComponentState, ABitmap.Canvas, ABitmap.VclBitmap, fBackgroundStyle, fBackGround, 0, 0, ABitmap.Width, ABitmap.Height, fOffX, fOffY, fOffX + frx, fOffY + fry, fChessboardSize, fChessboardBrushStyle, fGradientEndColor, fWallPaper, fWallPaperStyle);
      SelectClipRgn(ABitmap.canvas.handle, 0);
    end;
  end;

  procedure SetLayerMask;
  var
    pold, pnew: pbyte;
    i, j: integer;
    aw, ah: integer;  // alpha width and height
    mw, mh: integer;  // mask width and height
    v: integer;
    rx, ry: double;
    zx, zy: double;
    mx, my: integer;
    mxarr: pintegerarray;
    msk_p: pbytearray;
    l0width, l0height: integer;
    rmy: integer;
    armx: pintegerarray;
    doloop: boolean;
  begin
    if layerMask = nil then
      exit;
    if layer.fCachedLayerMask<> nil then
    begin
      // cached
      oldAlpha := lBitmap.AlphaChannel;
      lBitmap.AlphaChannel := layer.fCachedLayerMask;
      newAlpha := layer.fCachedLayerMask;
    end
    else
    begin
      // check pixelformat of layer mask
      if layerMask.Bitmap.PixelFormat <> ie8g then
      begin
        layerMask.Bitmap.PixelFormat := ie8g;
        if layerMask.Bitmap.HasAlphaChannel then
          layerMask.Bitmap.RemoveAlphaChannel;  // a layer mask cannot have alpha channel
      end;

      l0width  := Layers[0].Bitmap.Width;
      l0height := Layers[0].Bitmap.Height;
      oldAlpha := lBitmap.AlphaChannel;
      newAlpha := TIEBitmap.Create;
      newAlpha.Allocate(oldAlpha.Width, oldAlpha.Height, ie8g);
      lBitmap.AlphaChannel := newAlpha;
      aw := newAlpha.Width;
      ah := newAlpha.Height;
      mw := layerMask.Bitmap.Width;
      mh := layerMask.Bitmap.Height;
      zx := (layer.Width / layer.Bitmap.Width);
      zy := (layer.Height / layer.Bitmap.Height);
      rx := (layerMask.Width / layerMask.Bitmap.Width);
      ry := (layerMask.Height / layerMask.Bitmap.Height);
      armx := nil;

      getmem(mxarr, sizeof(integer) * aw);
      for j := 0 to aw - 1 do
        mxarr[j] := round((j * zx / rx - layerMask.PosX / rx) + layer.PosX / rx);

      if fSoftCrop = iesfAlphaBlend then
      begin
        getmem(armx, sizeof(integer) * aw);
        for j := 0 to aw - 1 do
        begin
          i := round(mxarr[j] * rx) + layer.PosX;
          if (i < 0) or (i > l0width) then
            armx[j] := 0
          else
            armx[j] := - 1;
        end;
      end;

      for i := 0 to ah - 1 do
      begin
        pold := oldAlpha.Scanline[i];
        pnew := newAlpha.Scanline[i];
        my := round((i * zy / ry - layerMask.PosY / ry) + layer.PosY / ry);
        msk_p := layerMask.Bitmap.Scanline[imin(imax(0, my), layerMask.Bitmap.Height - 1)];
        doloop := true;
        if fSoftCrop = iesfAlphaBlend then
        begin
          rmy := round(my * ry) + layer.PosY;
          if (rmy < 0) or (rmy > l0height) then
          begin
            FillChar(pnew^, aw, fSoftCropValue);
            doloop := false;
          end;
        end;
        if doloop then
        begin
          for j := 0 to aw - 1 do
          begin
            if (fSoftCrop = iesfAlphaBlend) and (armx[j] = 0) then
              v := fSoftCropValue
            else
            begin
              mx := mxarr[j];
              if (my < mh) and (my >= 0) and (mx < mw) and (mx >= 0) then
                v := msk_p[mx]
              else
                v := 0;
            end;
            if pold^ < v then
              pnew^ := pold^
            else
              pnew^ := v;
            inc(pold);
            inc(pnew);
          end;
        end;
      end;
      freemem(mxarr);
      if fSoftCrop = iesfAlphaBlend then
        freemem(armx);
    end;
  end;

  procedure UnSetLayerMask;
  begin
    if layerMask <> nil then
    begin
      layer.bitmap.AlphaChannel := oldAlpha;
      if layer.fCachedLayerMask = nil then
        layer.fCachedLayerMask := newAlpha
      else
      if layer.fCachedLayerMask <> newAlpha then
      begin
        // this should never happen
        FreeAndNil(layer.fCachedLayerMask);
        layer.fCachedLayerMask := newAlpha;
      end;
    end;
  end;

  function AlignYBmp2Scr(v: integer; ref: integer): integer;
  begin
    if YScr2Bmp(v) < ref then
      result := v + 1
    else
    if YScr2Bmp(v) > ref then
      result := v - 1
    else
      result := v;
  end;

  function AlignXBmp2Scr(v: integer; ref: integer): integer;
  begin
    if XScr2Bmp(v) < ref then
      result := v + 1
    else
    if XScr2Bmp(v) > ref then
      result := v - 1
    else
      result := v;
  end;

begin
  DoLayersSync();
  CalcPaintCoords();
  CreateCoordConvLUT(); // recalculates coordinate conversion LUT

  // check for errors in current layer
  if (csDesigning in ComponentState) or (fIEBitmap.Height = 0) or (fIEBitmap.Width = 0) then
    exit; // EXIT!
  if fIEBitmap.HasAlphaChannel and ((fIEBitmap.AlphaChannel.Width <> fIEBitmap.Width) or (fIEBitmap.AlphaChannel.Height <> fIEBitmap.Height)) then
    exit; // EXIT!

  lresw := 0;
  lresh := 0;

  lposx := 0;
  lposy := 0;

  // draw layers
  IsFirst := true;
  for i := 0 to fLayers.Count - 1 do
  begin
    layer := TIELayer(fLayers[i]);
    lBitmap := layer.Bitmap;

    if layer.Visible and (frx <> 0) and (fry <> 0) and ((lBitmap.Width >= fMinBitmapSize) or (lBitmap.Height >= fMinBitmapSize)) then
    begin

      // layer mask and softcropping
      layerMask := nil;
      FreeLayerMask := false;
      if (i < fLayers.Count - 1) and Layers[i + 1].IsMask and (not layer.IsMask) and (layer.Rotate = 0) then
        layerMask := Layers[i + 1];
      if (i > 0) and (layerMask = nil) and (fSoftCrop = iesfAlphaBlend) then
      begin
        FreeLayerMask := true;
        layerMask := TIELayer.Create(self, nil, false);
        layerMask.Bitmap.Allocate(lBitmap.Width, lBitmap.Height, ie8g);
        layerMask.Bitmap.Fill(255);
        layerMask.PosX := layer.PosX;
        layerMask.PosY := layer.PosY;
        layerMask.ResizedWidth  := layer.ResizedWidth;
        layerMask.ResizedHeight := layer.ResizedHeight;
      end;
      SetLayerMask;

      Transparency := layer.Transparency;
      Opacity      := layer.Opacity;
      if IsFirst and drawBackground then
        SubDrawBackground(false);

      if (fStable > 0) or ((layer.ResizedWidth = 0) and (layer.ResizedHeight = 0) and (fZoomX = 100) and (fZoomY = 100)) then
      begin
        zf := rfNone;
      end
      else
      begin
        if layer.UseResampleFilter then
          zf := layer.ResampleFilter
        else
          zf := fZoomFilter;
      end;

      if (fLayersSync or ((fLayers.Count = 1) and
         (layer.ResizedWidth = 0) and (layer.ResizedHeight = 0) and
         (layer.PosX = 0) and (layer.PosY = 0) and (layer.Rotate = 0.0))) then
      begin

        {$IFDEF IEDEBUG}
        OutputDebugString(PAnsiChar('TImageEnView.PaintToEx'));
        OutputDebugString(PAnsiChar(string( '  XDst='+IntToStr(fOffX)+' YDst='+IntToStr(fOffY)+
                                            ' WidthDst='+IntToStr(frx)+' HeightDst='+IntToStr(fry)+
                                            ' XSrc='+IntToStr(fo1x)+' YSrc='+IntToStr(fo1y)+
                                            ' WidthSrc='+IntToStr(fo2x)+' HeightSrc='+IntToStr(fo2y)
          )));
        {$ENDIF}

        lBitmap.RenderToTIEBitmap(ABitmap, ABitmapScanline, pinteger(fXScr2Bmp), pinteger(fYScr2Bmp), UpdRect, fOffX, fOffY, frx, fry, fo1x, fo1y, fo2x, fo2y, fEnableAlphaChannel, (fBackgroundStyle = iebsSolid) and IsFirst, Transparency, zf, true, layer.Operation, Opacity);
        layer.ClientAreaBox := Rect(fOffX, fOffY, fOffX + frx, fOffY + fry);
        layer.DrawingInfo.XDst      := fOffX;
        layer.DrawingInfo.YDst      := fOffY;
        layer.DrawingInfo.WidthDst  := frx;
        layer.DrawingInfo.HeightDst := fry;
        layer.DrawingInfo.XSrc      := fo1x;
        layer.DrawingInfo.YSrc      := fo1y;
        layer.DrawingInfo.WidthSrc  := fo2x;
        layer.DrawingInfo.HeightSrc := fo2y;
      end
      else
      begin

        // rotate
        if layer.Rotate <> 0 then
        begin
          // here ResizedWidth and ResizedHeight should be 0, but anyway here we accept other values
          lresw := layer.ResizedWidth;
          lresh := layer.ResizedHeight;
          lposx := layer.PosX;
          lposy := layer.PosY;
          w := layer.Width;
          h := layer.Height;
          if (w = lbitmap.Width) and (h = lbitmap.Height) then
          begin
            tmpbmp := lbitmap;
          end
          else
          begin
            tmpbmp := TIEBitmap.create(w, h, lbitmap.PixelFormat);
            _IEResampleIEBitmap(lbitmap, tmpbmp, rfNone, nil, nil);
          end;
          tmpbmp.AlphaChannel;
          rotbmp := TIEBitmap.Create();

          if ((fLayersRotationDelayFilterOnPreview = False) or (fStable = 0)) AND
             fLayersRotationUseFilterOnPreview and fLayersRotationAntialias and ((fLayersRotationFilter = ierBilinear) or (fLayersRotationFilter = ierBicubic)) then
          begin
            IEQRotateTo(tmpbmp, rotbmp, layer.Rotate, clBlack, fLayersRotationFilter, nil, nil);
            IEQRotateTo8(tmpbmp.AlphaChannel, rotbmp.AlphaChannel, layer.Rotate, clBlack, fLayersRotationFilter);
          end
          else
          begin
            IEGRotateTo(tmpbmp, rotbmp, layer.Rotate, clBlack, nil, nil);
            IEGRotateTo(tmpbmp.AlphaChannel, rotbmp.AlphaChannel, layer.Rotate, clBlack, nil, nil);
          end;

          rotbmp.AlphaChannel.SyncFull();
          if tmpbmp <> lbitmap then
            tmpbmp.Free();
          lbitmap := rotbmp;
          layer.ResizedWidth  := lbitmap.Width;
          layer.ResizedHeight := lbitmap.Height;
          with layer.Drawinginfo do
          begin
            RotatedDest[0].X := 0;
            RotatedDest[0].Y := 0;
            RotatedDest[1].X := w;
            RotatedDest[1].Y := 0;
            RotatedDest[2].X := w;
            RotatedDest[2].Y := h;
            RotatedDest[3].X := 0;
            RotatedDest[3].Y := h;
            IERotatePoints(RotatedDest, layer.Rotate, trunc(layer.RotateCenterX * w), trunc(layer.RotateCenterY * h));
            layer.PosX := lposx+imin(imin(imin(RotatedDest[0].X, RotatedDest[1].X), RotatedDest[2].X), RotatedDest[3].X);
            layer.PosY := lposy+imin(imin(imin(RotatedDest[0].Y, RotatedDest[1].Y), RotatedDest[2].Y), RotatedDest[3].Y);
            for j := 0 to 3 do
            begin
              RotatedDest[j].X := fOffX + trunc( (lposx+RotatedDest[j].X) * fZoomD100X ) - fViewX;
              RotatedDest[j].Y := fOffY + trunc( (lposy+RotatedDest[j].Y) * fZoomD100Y ) - fViewY;
            end;
          end;
        end
        else
          rotbmp := nil;

        if (i > 0) and (layer.Magnify.Enabled) and (layer.Magnify.Rate >= 1) then
        begin
          // magnify layer
          if (fLayerResizing = ieNone) then // needed because aspectratio-resizing doesn't work with magnify layers
          begin
            if layer.ResizedWidth > 0 then
              lBitmap.Width := layer.ResizedWidth;
            if layer.ResizedHeight > 0 then
              lBitmap.Height := layer.ResizedHeight;
          end;

          w  := trunc(lBitmap.Width / (layer.Magnify.Rate / fZoomD100X));
          h  := trunc(lBitmap.Height / (layer.Magnify.Rate / fZoomD100Y));
          x1 := XBmp2Scr(layer.PosX) + trunc(lBitmap.Width * fZoomD100X / 2);
          y1 := YBmp2Scr(layer.PosY) + trunc(lBitmap.Height * fZoomD100Y / 2);
          x2 := x1 + w;
          y2 := y1 + h;
          x1 := x1 - w div 2;
          y1 := y1 - h div 2;
          x2 := x2 - w div 2;
          y2 := y2 - h div 2;

          case layer.Magnify.Source of
            iemCanvas:
              begin
                // gets all from current canvas (already painted layers and the background)
                lBitmap.Location := ieTBitmap;
                lBitmap.VclBitmap.Canvas.CopyRect(Rect(0, 0, lBitmap.Width, lBitmap.Height), ABitmap.Canvas, Rect(x1, y1, x2, y2));
              end;
            iemBackgroundLayer:
              begin
                // gets image from layer 0
                if lBitmap.PixelFormat <> ie24RGB then
                  lBitmap.PixelFormat := ie24RGB;
                x1 := XScr2Bmp(x1);
                y1 := YScr2Bmp(y1);
                x2 := XScr2Bmp(x2);
                y2 := YScr2Bmp(y2);
                w := x2 - x1 + 1;
                h := y2 - y1 + 1;
                if (y1 >= 0) and (x1 >= 0) and
                   (x1 < Layers[0].Bitmap.Width - w) and (y1 < Layers[0].Bitmap.Height - h) and
                   (w < Layers[0].Bitmap.Width) and (h < Layers[0].Bitmap.Height) then
                begin
                  lBitmap.Fill(0);
                  Layers[0].Bitmap.RenderToTIEBitmapEx(lBitmap, 0, 0, lBitmap.Width, lBitmap.Height, x1, y1, w, h, 255, zf, ielNormal);
                end;
              end;
          end;

          case layer.Magnify.Style of
            iemRectangle:
              lBitmap.AlphaChannel.Fill(255); // do nothing
            iemEllipse:
              begin
                lBitmap.AlphaChannel.Location := ieTBitmap;
                lBitmap.AlphaChannel.Fill(0);
                lBitmap.AlphaChannel.Full := false;
                with lBitmap.AlphaChannel.VclBitmap.Canvas do
                begin
                  Brush.Style := bsSolid;
                  Brush.Color := $01000000 or $FF;
                  Pen.Style   := psSolid;
                  Pen.Mode    := pmCopy;
                  Pen.Color   := $01000000 or $FF;
                  Pen.Width   := 1;
                  Ellipse(0, 0, lBitmap.Width - 1, lBitmap.Height - 1);
                end;
              end;
          end;
        end;  // end of magnify layer support

        XDst := XBmp2Scr(layer.PosX);
        YDst := YBmp2Scr(layer.PosY);

        YDst := AlignYBmp2Scr(YDst, layer.PosY);
        XDst := AlignXBmp2Scr(XDst, layer.PosX);

        w := layer.ResizedWidth;
        if w = 0 then
          w := lbitmap.Width;
        WidthDst := XBmp2Scr(layer.PosX + w);
        WidthDst := AlignXBmp2Scr(WidthDst, layer.PosX + w);
        dec(WidthDst, XDst);

        h := layer.ResizedHeight;
        if h = 0 then
          h := lbitmap.Height;
        HeightDst := YBmp2Scr(layer.PosY + h);
        HeightDst := AlignYBmp2Scr(HeightDst, layer.Posy + h);
        dec(HeightDst, YDst);

        XSrc := 0;
        YSrc := 0;
        WidthSrc  := lbitmap.Width;
        HeightSrc := lbitmap.Height;

        layer.ClientAreaBox := Rect(XDst, YDst, XDst + WidthDst, YDst + HeightDst);

        if layer.Cropped then
        begin
          // cropped
          if XScr2Bmp(XDst) < 0 then
          begin
            XSrc     := - trunc(layer.PosX * (lbitmap.width / w));
            WidthSrc := lbitmap.width - XSrc;
            XDst     := XBmp2Scr(0);
            WidthDst := XBmp2Scr(w + layer.posX) - XDst;
          end;
          if YScr2Bmp(YDst) < 0 then
          begin
            YSrc      := - trunc(layer.PosY * (lbitmap.height / h));
            HeightSrc := lbitmap.height - YSrc;
            YDst      := YBmp2Scr(0);
            HeightDst := YBmp2Scr(h + layer.posY) - YDst;
          end;

          if XScr2Bmp(XDst + WidthDst) > Layers[0].Bitmap.Width then
          begin
            WidthDst := XBmp2Scr(Layers[0].Bitmap.Width) - XDst;
            WidthSrc := trunc(WidthDst* (lbitmap.Width/(layer.ClientAreaBox.Right - layer.ClientAreaBox.Left)));
          end;
          if YScr2Bmp(YDst + HeightDst) > Layers[0].Bitmap.Height then
          begin
            HeightDst := YBmp2Scr(Layers[0].Bitmap.Height) - YDst;
            HeightSrc := trunc(HeightDst * (lbitmap.Height/(layer.ClientAreaBox.Bottom - layer.ClientAreaBox.Top )));
          end;

        end;

        dummy2 := nil;
        dummy3 := nil;

        {$IFDEF IEDEBUG}
        OutputDebugString(PAnsiChar('TImageEnView.PaintToEx'));
        OutputDebugString(PAnsiChar(string( '  XDst='+IntToStr(XDst)+' YDst='+IntToStr(YDst)+
                                            ' WidthDst='+IntToStr(WidthDst)+' HeightDst='+IntToStr(HeightDst)+
                                            ' XSrc='+IntToStr(XSrc)+' YSrc='+IntToStr(YSrc)+
                                            ' WidthSrc='+IntToStr(WidthSrc)+' HeightSrc='+IntToStr(HeightSrc)
          )));
        {$ENDIF}

        lBitmap.RenderToTIEBitmap(ABitmap, ABitmapScanline, dummy2, dummy3, UpdRect, XDst, YDst, WidthDst, HeightDst, XSrc, YSrc, WidthSrc, HeightSrc, fEnableAlphaChannel, (fBackgroundStyle = iebsSolid) and IsFirst, Transparency, zf, true, layer.Operation, Opacity);
        layer.DrawingInfo.XDst := XDst;
        layer.DrawingInfo.YDst := YDst;
        layer.DrawingInfo.WidthDst  := WidthDst;
        layer.DrawingInfo.HeightDst := HeightDst;
        layer.DrawingInfo.XSrc := XSrc;
        layer.DrawingInfo.YSrc := YSrc;
        layer.DrawingInfo.WidthSrc  := WidthSrc;
        layer.DrawingInfo.HeightSrc := HeightSrc;

        if assigned(rotbmp) then
        begin
          FreeAndNil(rotbmp);
          layer.ResizedWidth  := lresw;
          layer.ResizedHeight := lresh;
          layer.PosX := lposx;
          layer.PosY := lposy;
        end;

      end;

      AfterDrawLayer(i, ABitmap, updRect^);

      if drawGadgets then
      begin
        if (layer.DrawOuter) and (i = fLayersCurrent) then
          DrawLayerOuter(ABitmap.VclBitmap, i);
      end;

      if assigned(fOnDrawLayer) then
        fOnDrawLayer(self, ABitmap, i);

      UnSetLayerMask;
      if FreeLayerMask then
      begin
        layerMask.Free;
      end;

      IsFirst := False;
    end; // end of Visible and (frx<>0) and (fry<>0)
  end;

  if (fSoftCrop = iesfGrid) or (fSoftCrop = iesfAdd) then
  begin
    w  := ABitmap.Width;
    h  := ABitmap.Height;
    x1 := Layers[0].DrawingInfo.XDst;
    y1 := Layers[0].DrawingInfo.YDst;
    x2 := x1 + Layers[0].DrawingInfo.WidthDst;
    y2 := y1 + Layers[0].DrawingInfo.HeightDst;
    for y := 0 to h - 1 do
    begin
      px := ABitmap.Scanline[y];
      case fSoftCrop of
        iesfGrid:
          for x := 0 to w - 1 do
          begin
            if (y < y1) or (y > y2) or (x < x1) or (x > x2) then
              if ((x + y) and 1) = 0 then
              with px^ do
              begin
                r := 128;
                g := 128;
                b := 128;
              end;
            inc(px);
          end;
        iesfAdd:
          for x := 0 to w - 1 do
          begin
            if (y < y1) or (y >= y2) or (x < x1) or (x >= x2) then
              with px^ do
              begin
                r := blimit(r + fSoftCropValue);
                g := blimit(g + fSoftCropValue);
                b := blimit(b + fSoftCropValue);
              end;
            inc(px);
          end;
      end;
    end;
  end;

  // check if no layer is displayed
  if IsFirst and drawBackground then
    SubDrawBackground(true);

  if drawGadgets then
  begin
    for i := 0 to fLayers.Count - 1 do
      if (Layers[i].VisibleBox) then
      begin
        if (i = fLayersCurrent) then
        begin
          if (miMoveLayers in fMouseInteract) or (miResizeLayers in fMouseInteract) or (miRotateLayers in fMouseInteract) then
            DrawLayerBox(ABitmap.VclBitmap, i);
          if (miResizeLayers in fMouseInteract) and (not Layers[i].Locked) then
            DrawLayerGrips(ABitmap.VclBitmap, i);
          if (miRotateLayers in fMouseInteract) then
            DrawLayerRotationCenter(ABitmap.VclBitmap, i);
        end
        else
        if fLayersDrawBox then
          DrawLayerBox(ABitmap.VclBitmap, i);
      end;

    // draw selection
    PaintSelection(ABitmap.VclBitmap);

    // draw grid
    PaintGrid(ABitmap.VclBitmap);
  end;

end;


procedure TImageEnView.AfterDrawLayer(layerIndex: Integer; DestBitmap: TIEBitmap; const DestRect: TRect);
begin
  // nothing to do
end;


{!!
<FS>TImageEnView.LayersMergeAll

<FM>Declaration<FC>
procedure LayersMergeAll;

<FM>Description<FN>
Call LayersMergeAll to merge all layers in one step.
This method performs the same task as <A TImageEnView.LayersDrawTo>, but replaces all layers with the merged result.

This method is fast and works with all layer options, so should be used to merge layers before printing or saving.

!!}
procedure TImageEnView.LayersMergeAll();
var
  bmp: TIEBitmap;
begin
  bmp := TIEBitmap.Create(fIEBitmap.Width, fIEBitmap.Height);
  try
    bmp.Fill(Background);
    LayersDrawTo(bmp);
    LayersClear();
    fIEBitmap.Assign(bmp);
  finally
    bmp.free;
  end;
  Update;
end;

{!!
<FS>TImageEnView.LayersDrawTo

<FM>Declaration<FC>
procedure LayersDrawTo(Destination: <A TIEBitmap>; Scale: Double = 1);

<FM>Description<FN>
Merges all layers and draws the result to <FC>Destination<FN> bitmap.
This function should replace a sequence of <A TImageEnView.LayersMerge> calls. The destination bitmap will not have transparency channel.

<FM>Example<FC>
// draws all layers of ImageEnView1 to ImageEnView2
ImageEnView1.LayersDrawTo( ImageEnView2.IEBitmap );
ImageEnView2.Update;

!!}
procedure TImageEnView.LayersDrawTo(Destination: TIEBitmap);
var
  i: Integer;
  mxwidth, mxheight: Integer;
  dstWidth, dstHeight: Integer;
  lzoomx, lzoomy: Double;
  lviewx, lviewy: Integer;
  lMaxLayerWidth, lMaxLayerHeight: Integer;
  negw, negh: Integer;
begin
  // calculates maximum extension and minimum left/top coordinates
  mxwidth  := Layers[0].PosX + Layers[0].Width;
  mxheight := Layers[0].PosY + Layers[0].Height;
  negw := 0;
  negh := 0;
  for i := 0 to LayersCount-1 do
  begin
    if not Layers[i].Cropped then
    begin
      if (mxwidth < Layers[i].PosX+Layers[i].Width) then
        mxwidth := Layers[i].PosX+Layers[i].Width;
      if (mxheight < Layers[i].PosY+Layers[i].Height) then
        mxheight := Layers[i].PosY+Layers[i].Height;
      if Layers[i].PosX < 0 then
        negw := imax(negw, abs(Layers[i].PosX));
      if Layers[i].PosY<0 then
        negh := imax(negh, abs(Layers[i].PosY));
    end;
  end;
  dstWidth  := mxWidth+negw;
  dstHeight := mxHeight+negh;
  Destination.Resize(dstWidth, dstHeight, Background, 255, iehLeft, ievTop);

  // paint layers
  lzoomx := fZoomX;
  lzoomy := fZoomY;
  fZoomX := 100;
  fZoomY := 100;
  fZoomD100X := 1;
  fZoomD100Y := 1;
  f100DZoomX := 1;
  f100DZoomY := 1;
  lviewx := fViewX;
  lviewy := fViewY;
  fViewX := 0;
  fViewY := 0;
  fOffX := negw;
  fOffY := negh;
  fExtX := Layers[0].Bitmap.Width;
  fExtY := Layers[0].Bitmap.Height;
  fZZWW := Layers[0].Bitmap.Width;
  fZZHH := Layers[0].Bitmap.Height;
  lMaxLayerWidth  := fMaxLayerWidth;
  lMaxLayerHeight := fMaxLayerHeight;
  fMaxLayerWidth  := Layers[0].Bitmap.Width;
  fMaxLayerHeight := Layers[0].Bitmap.Height;

  PaintToEx(Destination, nil, nil, false, false);

  fMaxLayerWidth  := lMaxLayerWidth;
  fMaxLayerHeight := lMaxLayerHeight;
  fViewX := lviewx;
  fViewY := lviewy;
  fZoomX := lzoomx;
  fZoomY := lzoomy;
  fZoomD100X := fZoomX / 100;
  f100DZoomX := 100 / fZoomX;
  fZoomD100Y := fZoomY / 100;
  f100DZoomY := 100 / fZoomY;

  fUpdateInvalidate := false;
  Update(); // restores other old values
  fUpdateInvalidate := true;

  CalcPaintCoords();
  CreateCoordConvLUT();
end;


{!!
<FS>TImageEnView.SetLayersBoxStyle

<FM>Declaration<FC>
procedure SetLayersBoxStyle(PenStyle: TPenStyle=psDot; PenMode: TPenMode=pmNot; PenWidth: Integer = 1; PenColor: TColor=clBlack);

<FM>Description<FN>
An alternative way to change selected layer box pen style.

See also: <A TImageEnView.OnDrawLayerBox>
!!}
procedure TImageEnView.SetLayersBoxStyle(PenStyle: TPenStyle; PenMode: TPenMode; PenWidth: Integer; PenColor: TColor);
begin
  fLayerBoxPen.Style := PenStyle;
  fLayerBoxPen.Mode  := PenMode;
  fLayerBoxPen.Width := PenWidth;
  fLayerBoxPen.Color := PenColor;
end;

procedure TImageEnView.DrawLayerBox(ABitmap: TBitmap; idx: integer);
var
  lyr: TIELayer;
begin
  if assigned(fOnDrawLayerBox) then
    fOnDrawLayerBox(self, ABitmap, idx)
  else
    with ABitmap.Canvas do
    begin
      Pen.Assign( fLayerBoxPen );
      Brush.Style := bsClear;
      lyr := TIELayer(fLayers[idx]);
      if lyr.Rotate = 0 then
      begin
        with lyr.ClientAreaBox do
          Rectangle(Left, Top, Right, Bottom);
      end
      else
      begin
        Polygon(lyr.DrawingInfo.RotatedDest);
      end;
    end;
end;

procedure TImageEnView.DrawLayerOuter(ABitmap: TBitmap; idx: integer);
var
  i, j: integer;
  rgb: PRGB;
  bwidth, bheight: integer;
begin
  with ABitmap.Canvas do
  begin
    with TIELayer(fLayers[idx]).ClientAreaBox do
    begin
      bwidth  := ABitmap.Width;
      bheight := ABitmap.Height;
      for i := 0 to bheight - 1 do
      begin
        rgb := ABitmap.Scanline[i];
        for j := 0 to bwidth - 1 do
        begin
          if ((j < Left) or (j > Right) or (i < Top) or (i > Bottom)) and ((((i and 1) = 0) and ((j and 1) = 0)) or (((i and 1) = 1) and ((j and 1) = 1))) then
            with rgb^ do
            begin
              r := 97;
              g := 97;
              b := 97;
            end;
          inc(rgb);
        end;
      end;
    end;
  end;
end;

// a layer must be selected (fLayersCurrent must be >-1)
function TImageEnView.FindLayerGrip(x, y: integer): TIEGrip;
var
  coords: array[0..8] of TRect;
  i: integer;
begin
  result := ieNone;
  if fLayersCurrent > -1 then
  begin
    CalcLayerGripCoords(fLayersCurrent, coords);
    for i := 0 to 8 do
    begin
      with coords[i] do
        if IEPointInRect(x, y, left, top, right, bottom) then
        begin
          result := TIEGrip(i + 1);
          break;
        end;
    end;
  end;
  if not (miRotatelayers in fMouseInteract) and (result = ieRotationCenter) then
    result := ieNone;
end;

function TImageEnView.FindLayerGripDist(x, y: integer; var Distance: Double): TIEGrip;
var
  coords: array[0..8] of TRect;
  i: integer;
  d: Double;
begin
  result := ieNone;
  Distance := 10000000;
  if fLayersCurrent > -1 then
  begin
    CalcLayerGripCoords(fLayersCurrent, coords);
    for i := 0 to 8 do
      with coords[i] do
      begin
        d := sqrt(sqr(x-((Left+Right)/2))+sqr(y-((Top+Bottom)/2)));
        if d<Distance then
        begin
          Distance := d;
          result := TIEGrip(i + 1);
        end;
      end;
  end;
end;


// 0=left-top  1=right-top  2=right-bottom  3=left-bottom
// 4=left side
// 5=right side
// 6=top side
// 7=bottom side
// 8=rotation center grip
procedure TImageEnView.CalcLayerGripCoords(layeridx: integer; var coords: array of TRect);
var
  lyr: TIELayer;
begin
  lyr := TIELayer(fLayers[layeridx]);
  if lyr.Rotate = 0 then
  begin
    with lyr.ClientAreaBox do
    begin
      // left | top
      coords[0].Left := left - fLyrGripSize;
      coords[0].Top := top - fLyrGripSize;
      coords[0].Right := left + fLyrGripSize;
      coords[0].Bottom := top + fLyrGripSize;
      // right | top
      coords[1].Left := right - fLyrGripSize;
      coords[1].Top := top - fLyrGripSize;
      coords[1].Right := right + fLyrGripSize;
      coords[1].Bottom := top + fLyrGripSize;
      // right | bottom
      coords[2].Left := right - fLyrGripSize;
      coords[2].Top := bottom - fLyrGripSize;
      coords[2].Right := right + fLyrGripSize;
      coords[2].Bottom := bottom + fLyrGripSize;
      // left | bottom
      coords[3].Left := left - fLyrGripSize;
      coords[3].Top := bottom - fLyrGripSize;
      coords[3].Right := left + fLyrGripSize;
      coords[3].Bottom := bottom + fLyrGripSize;
      // left | center
      coords[4].Left := left - fLyrGripSize;
      coords[4].Top := ((top + bottom) div 2) - fLyrGripSize;
      coords[4].Right := left + fLyrGripSize;
      coords[4].Bottom := ((top + bottom) div 2) + fLyrGripSize;
      // right | center
      coords[5].Left := right - fLyrGripSize;
      coords[5].Top := ((top + bottom) div 2) - fLyrGripSize;
      coords[5].Right := right + fLyrGripSize;
      coords[5].Bottom := ((top + bottom) div 2) + fLyrGripSize;
      // top | center
      coords[6].Left := ((left + right) div 2) - fLyrGripSize;
      coords[6].Top := top - fLyrGripSize;
      coords[6].Right := ((left + right) div 2) + fLyrGripSize;
      coords[6].Bottom := top + fLyrGripSize;
      // bottom | center
      coords[7].Left := ((left + right) div 2) - fLyrGripSize;
      coords[7].Top := bottom - fLyrGripSize;
      coords[7].Right := ((left + right) div 2) + fLyrGripSize;
      coords[7].Bottom := bottom + fLyrGripSize;
    end;
  end
  else
  begin
    with lyr.DrawingInfo do
    begin
      // left | top
      coords[0].Left := RotatedDest[0].X - fLyrGripSize;
      coords[0].Top := RotatedDest[0].Y - fLyrGripSize;
      coords[0].Right := RotatedDest[0].X + fLyrGripSize;
      coords[0].Bottom := RotatedDest[0].Y + fLyrGripSize;
      // right | top
      coords[1].Left := RotatedDest[1].X - fLyrGripSize;
      coords[1].Top := RotatedDest[1].Y - fLyrGripSize;
      coords[1].Right := RotatedDest[1].X + fLyrGripSize;
      coords[1].Bottom := RotatedDest[1].Y + fLyrGripSize;
      // right | bottom
      coords[2].Left := RotatedDest[2].X - fLyrGripSize;
      coords[2].Top := RotatedDest[2].Y - fLyrGripSize;
      coords[2].Right := RotatedDest[2].X + fLyrGripSize;
      coords[2].Bottom := RotatedDest[2].Y + fLyrGripSize;
      // left | bottom
      coords[3].Left := RotatedDest[3].X - fLyrGripSize;
      coords[3].Top := RotatedDest[3].Y - fLyrGripSize;
      coords[3].Right := RotatedDest[3].X + fLyrGripSize;
      coords[3].Bottom := RotatedDest[3].Y + fLyrGripSize;
      // left | center
      coords[4].Left   := ((RotatedDest[0].X+RotatedDest[3].X) div 2) - fLyrGripSize;
      coords[4].Top    := ((RotatedDest[0].Y+RotatedDest[3].Y) div 2) - fLyrGripSize;
      coords[4].Right  := coords[4].Left + fLyrGripSize*2;
      coords[4].Bottom := coords[4].Top + fLyrGripSize*2;
      // right | center
      coords[5].Left   := ((RotatedDest[1].X+RotatedDest[2].X) div 2) - fLyrGripSize;
      coords[5].Top    := ((RotatedDest[1].Y+RotatedDest[2].Y) div 2) - fLyrGripSize;
      coords[5].Right  := coords[5].Left + fLyrGripSize*2;
      coords[5].Bottom := coords[5].Top + fLyrGripSize*2;
      // top | center
      coords[6].Left   := ((RotatedDest[0].X+RotatedDest[1].X) div 2) - fLyrGripSize;
      coords[6].Top    := ((RotatedDest[0].Y+RotatedDest[1].Y) div 2) - fLyrGripSize;
      coords[6].Right  := coords[6].Left + fLyrGripSize*2;
      coords[6].Bottom := coords[6].Top + fLyrGripSize*2;
      // bottom | center
      coords[7].Left   := ((RotatedDest[2].X+RotatedDest[3].X) div 2) - fLyrGripSize;
      coords[7].Top    := ((RotatedDest[2].Y+RotatedDest[3].Y) div 2) - fLyrGripSize;
      coords[7].Right  := coords[7].Left + fLyrGripSize*2;
      coords[7].Bottom := coords[7].Top + fLyrGripSize*2;
    end;
  end;
  // rotation center
  coords[8].Left := lyr.ConvXBmp2Scr( round(lyr.Bitmap.Width * lyr.RotateCenterX ) ) - fLyrGripSize;
  coords[8].Top := lyr.ConvYBmp2Scr( round(lyr.Bitmap.Height * lyr.RotateCenterY ) ) - fLyrGripSize;
  coords[8].Right  := coords[8].Left + fLyrGripSize*2;
  coords[8].Bottom := coords[8].Top + fLyrGripSize*2;
end;

procedure TImageEnView.DrawLayerGrips(ABitmap: TBitmap; idx: integer);
var
  i: integer;
  coords: array[0..8] of TRect;
  iec: TIECanvas;
begin
  CalcLayerGripCoords(idx, coords);
  if assigned(fOnDrawLayerGrip) then
  begin
    for i := 0 to 8 do
      fOnDrawLayerGrip(self, ABitmap, idx, i, coords[i]);
  end
  else
  begin
    iec := TIECanvas.Create(ABitmap.Canvas, true, true);
    with iec do
    begin
      Pen.Style := psSolid;
      Pen.Mode  := pmCopy;
      Pen.Color := fLyrGripColor1;
      Pen.Width := 1;
      Brush.Style := fLyrGripBrushStyle;
      Brush.Color := fLyrGripColor2;
      for i := 0 to 7 do
        with coords[i] do
          case fLyrGripShape of
            iegsBox:    Rectangle(Left, Top, Right, Bottom);
            iegsCircle: Ellipse(Left, Top, Right, Bottom);
          end;
    end;
    iec.Free;
  end;
end;

procedure TImageEnView.DrawLayerRotationCenter(ABitmap: TBitmap; idx: integer);
var
  coords: array[0..8] of TRect;
  iec: TIECanvas;
begin
  CalcLayerGripCoords(idx, coords);
  iec := TIECanvas.Create(ABitmap.Canvas, true, true);
  with iec do
  begin
    Pen.Style := psSolid;
    Pen.Mode  := pmCopy;
    Pen.Color := fLyrGripColor1;
    Brush.Style := fLyrGripBrushStyle;
    Brush.Color := fLyrGripColor2;
    Ellipse(coords[8]);
  end;
  iec.Free;
end;


{!!
<FS>TImageEnView.SaveSelectionToStream

<FM>Declaration<FC>
procedure SaveSelectionToStream(Stream: TStream);

<FM>Description<FN>
SaveSelectionToStream saves the current selection to the specified stream (just the user selection, not the image content)

<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.SaveSelectionToStream(sel1);
..
sel1.Position := 0;
ImageEnView1.LoadSelectionFromStream(sel1);   // this equates to Select(10, 10, 100, 100)
!!}
procedure TImageEnView.SaveSelectionToStream(Stream: TStream);
begin
  // main sizes
  Stream.Write(fSelectionMask.Width, sizeof(fSelectionMask.Width));
  Stream.Write(fSelectionMask.Height, sizeof(fSelectionMask.Height));
  // TImageEnView
  Stream.Write(PIEAnimPoly(fHPolySel)^.PolyCount, sizeof(PIEAnimPoly(fHPolySel)^.PolyCount));
  if PIEAnimPoly(fHPolySel)^.Poly <> nil then
    Stream.Write(PIEAnimPoly(fHPolySel)^.Poly^, sizeof(TPoint) * PIEAnimPoly(fHPolySel)^.PolyCount);
  Stream.Write(PIEAnimPoly(fHPolySel)^.RX1, sizeof(PIEAnimPoly(fHPolySel)^.RX1));
  Stream.Write(PIEAnimPoly(fHPolySel)^.RY1, sizeof(PIEAnimPoly(fHPolySel)^.RY1));
  Stream.Write(PIEAnimPoly(fHPolySel)^.RX2, sizeof(PIEAnimPoly(fHPolySel)^.RX2));
  Stream.Write(PIEAnimPoly(fHPolySel)^.RY2, sizeof(PIEAnimPoly(fHPolySel)^.RY2));
  Stream.Write(fSel, sizeof(fSel));
  Stream.Write(CurrentLayer.PosX, sizeof(integer));
  Stream.Write(CurrentLayer.PosY, sizeof(integer));
  // TIEMask
  Stream.Write(fSelectionMask.BitsPerPixel, sizeof(fSelectionMask.BitsPerPixel));
  Stream.Write(fSelectionMask.X1, sizeof(fSelectionMask.X1));
  Stream.Write(fSelectionMask.Y1, sizeof(fSelectionMask.Y1));
  Stream.Write(fSelectionMask.X2, sizeof(fSelectionMask.X2));
  Stream.Write(fSelectionMask.Y2, sizeof(fSelectionMask.Y2));
  Stream.Write(fSelectionMask.Full, sizeof(fSelectionMask.Full));
  Stream.Write(fSelectionMask.Bits^, fSelectionMask.Height * fSelectionMask.RowLen);
end;

{!!
<FS>TImageEnView.LoadSelectionFromStream

<FM>Declaration<FC>
function LoadSelectionFromStream(Stream: TStream; Options: <A TIERSOptions> = iersMoveToAdapt): boolean;;

<FM>Description<FN>
LoadSelectionFromStream loads a selection from a stream (saved using <A TImageEnView.SaveSelectionToStream> or <A TImageEnView.SaveSelectionToFile>). Returns False if it fails.
Options specifies how the selection is adapted when it is applied to an image of a different size or position.

Note: To share the same selection among layers you must use iersSyncLayers.

<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.SaveSelectionToStream(sel1);
..
sel1.Position := 0;
ImageEnView1.LoadSelectionFromStream(sel1);   // this equates to Select(10, 10, 100, 100)
!!}
// return false if cannot load the selection (the background bitmap has wrong sizes)
function TImageEnView.LoadSelectionFromStream(Stream: TStream; Options: TIERSOptions): boolean;
var
  w, h, b, x1, y1, x2, y2: integer;
  f: boolean;
  psx, psy: Integer;
  ix, iy: Integer;
begin
  // main sizes
  result := true;
  Stream.Read(w, sizeof(fSelectionMask.Width));
  Stream.Read(h, sizeof(fSelectionMask.Height));

  // TImageEnView
  if PIEAnimPoly(fHPolySel)^.Poly <> nil then
    freemem(PIEAnimPoly(fHPolySel)^.Poly);
  PIEAnimPoly(fHPolySel)^.Poly := nil;
  Stream.Read(PIEAnimPoly(fHPolySel)^.PolyCount, sizeof(PIEAnimPoly(fHPolySel)^.PolyCount));
  getmem(PIEAnimPoly(fHPolySel)^.Poly, sizeof(TPoint) * (PIEAnimPoly(fHPolySel)^.PolyCount));
  PIEAnimPoly(fHPolySel)^.PolyCapacity := PIEAnimPoly(fHPolySel)^.PolyCount;
  if PIEAnimPoly(fHPolySel)^.Poly <> nil then
    Stream.Read(PIEAnimPoly(fHPolySel)^.Poly^, sizeof(TPoint) * PIEAnimPoly(fHPolySel)^.PolyCount);
  Stream.Read(PIEAnimPoly(fHPolySel)^.RX1, sizeof(PIEAnimPoly(fHPolySel)^.RX1));
  Stream.Read(PIEAnimPoly(fHPolySel)^.RY1, sizeof(PIEAnimPoly(fHPolySel)^.RY1));
  Stream.Read(PIEAnimPoly(fHPolySel)^.RX2, sizeof(PIEAnimPoly(fHPolySel)^.RX2));
  Stream.Read(PIEAnimPoly(fHPolySel)^.RY2, sizeof(PIEAnimPoly(fHPolySel)^.RY2));
  Stream.Read(fSel, sizeof(fSel));
  Stream.Read(psx, sizeof(integer));
  Stream.Read(psy, sizeof(integer));
  // TIEMask
  Stream.Read(b, sizeof(fSelectionMask.BitsPerPixel));
  fSelectionMask.AllocateBits(w, h, b);
  Stream.Read(x1, sizeof(fSelectionMask.X1));
  Stream.Read(y1, sizeof(fSelectionMask.Y1));
  Stream.Read(x2, sizeof(fSelectionMask.X2));
  Stream.Read(y2, sizeof(fSelectionMask.Y2));
  fSelectionMask.X1 := x1;
  fSelectionMask.Y1 := y1;
  fSelectionMask.X2 := x2;
  fSelectionMask.Y2 := y2;
  Stream.Read(f, sizeof(fSelectionMask.Full));
  fSelectionMask.Full := f;
  Stream.Read(fSelectionMask.Bits^, fSelectionMask.Height * fSelectionMask.RowLen);

  LockUpdate;

  // sync layers
  if (Options = iersSyncLayers) then
  begin
    AnimPolygonClear(fHPolySel);
    fSelectionMask.Resize(fIEBitmap.Width+imax(0, psx)+w, fIEBitmap.Height+imax(0, psy)+h);
    fSelectionMask.TranslateBitmap(psx, psy, true);
    ix := -CurrentLayer.PosX;
    iy := -CurrentLayer.PosY;
    fSelectionMask.TranslateBitmap(ix, iy, true );
    fSelectionMask.Resize(fIEBitmap.Width, fIEBitmap.Height);
    fSelectionMask.SyncFull;
    fSelectionMask.SyncRect;
  end
  else
  begin

    if ((w <> fIEBitmap.Width) or (h <> fIEBitmap.Height)) then
    begin
      if (not fSelectionMask.IsEmpty) then
      begin
        // try to move the selection
        if (Options = iersMoveToAdapt) and (x2>fIEBitmap.Width) then
        begin
          x1 := fIEBitmap.Width- (fSelectionMask.X2-fSelectionMask.X1+1);
          MoveSelection( -(fSelectionMask.X1-x1), 0 );
        end;
        if (Options = iersMoveToAdapt) and (y2>fIEBitmap.Height) then
        begin
          y1 := fIEBitmap.Height- (fSelectionMask.Y2-fSelectionMask.Y1+1);
          MoveSelection( 0, -(fSelectionMask.Y1-y1) );
        end;
        fSelectionMask.Resize(fIEBitmap.Width, fIEBitmap.Height);
        fSelectionMask.SyncFull;
        fSelectionMask.SyncRect;
      end
      else
        fSelectionMask.Resize(fIEBitmap.Width, fIEBitmap.Height);
    end;

  end;

  if fSelectionMask.IsEmpty then
    Deselect
  else
    fSel := true;

  ShowSelectionEx(true);

  UnLockUpdate;

  Update;

end;

{!!
<FS>TImageEnView.SaveSelectionToFile

<FM>Declaration<FC>
procedure SaveSelectionToFile(const FileName: String);

<FM>Description<FN>
SaveSelectionToFile saves the current selection to the specified file (just the user selection, not the image content).

<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.SaveSelectionToFile('selection1');
..
sel1.Position := 0;
ImageEnView1.LoadSelectionFromFile('selection1');   // this equates to Select(10, 10, 100, 100)
!!}
procedure TImageEnView.SaveSelectionToFile(const FileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveSelectionToStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnView.LoadSelectionFromFile

<FM>Declaration<FC>
function LoadSelectionFromFile(const FileName: String; Options: <A TIERSOptions> = iersMoveToAdapt): boolean;

<FM>Description<FN>
LoadSelectionFromFile loads a selection from a file (saved using <A TImageEnView.SaveSelectionToStream> or <A TImageEnView.SaveSelectionToFile>).
Returns False if it fails.
Options specifies how the selection is adapted when it comes from an image with different size or position.

Note: To share the same selection among layers you must use iersSyncLayers.

<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.SaveSelectionToFile('C:\selection1');
..
sel1.Position := 0;
ImageEnView1.LoadSelectionFromFile('C:\selection1');   // this equates to Select(10, 10, 100, 100)
!!}
function TImageEnView.LoadSelectionFromFile(const FileName: String; Options: TIERSOptions = iersMoveToAdapt): boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := LoadSelectionFromStream(fs, Options);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnView.MergeSelectionFromFile

<FM>Declaration<FC>
function MergeSelectionFromFile(const FileName: String): Boolean;

<FM>Description<FN>
MergeSelectionFromFile loads a selection from a file (saved using <A TImageEnView.SaveSelectionToStream> or <A TImageEnView.SaveSelectionToFile>) merging it with the currently one.
Returns False if it fails.

<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.SaveSelectionToFile('C:\selection1');
..
sel1.Position := 0;
ImageEnView1.MergeSelectionFromFile('C:\selection1');   // this equates to Select(10, 10, 100, 100, iespAdd)
!!}
function TImageEnView.MergeSelectionFromFile(const FileName: String): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := MergeSelectionFromStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnView.MergeSelectionFromStream

<FM>Declaration<FC>
function MergeSelectionFromStream(Stream: TStream): Boolean;

<FM>Description<FN>
MergeSelectionFromStream loads a selection from a stream (saved using <A TImageEnView.SaveSelectionToStream> or <A TImageEnView.SaveSelectionToFile>) merging it with the currently one.
Returns False if it fails.

<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.SaveSelectionToStream(sel1);
..
sel1.Position := 0;
ImageEnView1.MergeSelectionFromStream(sel1);   // this equates to Select(10, 10, 100, 100, iespAdd)
!!}
function TImageEnView.MergeSelectionFromStream(Stream: TStream): boolean;
var
  w, h, b, x1, y1, x2, y2: integer;
  f: boolean;
  msk2: TIEMask;
  src, dst, x, y: Integer;
  psx, psy: Integer;
begin
  // main sizes
  result := true;
  Stream.Read(w, sizeof(fSelectionMask.Width));
  Stream.Read(h, sizeof(fSelectionMask.Height));

  // TImageEnView
  if PIEAnimPoly(fHPolySel)^.Poly <> nil then
    freemem(PIEAnimPoly(fHPolySel)^.Poly);
  PIEAnimPoly(fHPolySel)^.Poly := nil;
  Stream.Read(PIEAnimPoly(fHPolySel)^.PolyCount, sizeof(PIEAnimPoly(fHPolySel)^.PolyCount));
  getmem(PIEAnimPoly(fHPolySel)^.Poly, sizeof(TPoint) * (PIEAnimPoly(fHPolySel)^.PolyCount));
  PIEAnimPoly(fHPolySel)^.PolyCapacity := PIEAnimPoly(fHPolySel)^.PolyCount;
  if PIEAnimPoly(fHPolySel)^.Poly <> nil then
    Stream.Read(PIEAnimPoly(fHPolySel)^.Poly^, sizeof(TPoint) * PIEAnimPoly(fHPolySel)^.PolyCount);
  Stream.Read(PIEAnimPoly(fHPolySel)^.RX1, sizeof(PIEAnimPoly(fHPolySel)^.RX1));
  Stream.Read(PIEAnimPoly(fHPolySel)^.RY1, sizeof(PIEAnimPoly(fHPolySel)^.RY1));
  Stream.Read(PIEAnimPoly(fHPolySel)^.RX2, sizeof(PIEAnimPoly(fHPolySel)^.RX2));
  Stream.Read(PIEAnimPoly(fHPolySel)^.RY2, sizeof(PIEAnimPoly(fHPolySel)^.RY2));
  Stream.Read(fSel, sizeof(fSel));
  Stream.Read(psx, sizeof(integer));
  Stream.Read(psy, sizeof(integer));

  AnimPolygonClear(fHPolySel);

  // TIEMask
  Stream.Read(b, sizeof(fSelectionMask.BitsPerPixel));
  msk2 := TIEMask.Create;
  msk2.AllocateBits(w, h, b);
  Stream.Read(x1, sizeof(fSelectionMask.X1));
  Stream.Read(y1, sizeof(fSelectionMask.Y1));
  Stream.Read(x2, sizeof(fSelectionMask.X2));
  Stream.Read(y2, sizeof(fSelectionMask.Y2));
  msk2.X1 := x1;
  msk2.Y1 := y1;
  msk2.X2 := x2;
  msk2.Y2 := y2;
  Stream.Read(f, sizeof(fSelectionMask.Full));
  msk2.Full := f;
  Stream.Read(msk2.Bits^, msk2.Height * msk2.RowLen);

  for y := y1 to y2 do
    for x := x1 to x2 do
    begin
      src := msk2.GetPixel(x, y);
      dst := fSelectionMask.GetPixel(x, y);
      fSelectionMask.SetPixel(x, y, imax(src, dst) );
    end;
  fSelectionMask.Full := msk2.Full and fSelectionMask.Full;

  SelectCustom;

  FreeAndNil(msk2);

end;


{!!
<FS>TImageEnView.SavedSelectionsCount

<FM>Declaration<FC>
property SavedSelectionsCount: Integer

<FM>Description<FN>
Returns the number of saved selections.
!!}
function TImageEnView.GetSavedSelectionsCount: Integer;
begin
  result := fSavedSelection.Count;
end;

// push current selection in fSavedSelection list

{!!
<FS>TImageEnView.SaveSelection

<FM>Declaration<FC>
procedure SaveSelection;

<FM>Description<FN>
SaveSelection adds the current selection to the stack (selections list).

<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.SaveSelection;
ImageEnView1.Select(200, 200, 150, 150);
ImageEnView1.SaveSelection;
ImageEnView1.Deselect;

ImageEnView1.RestoreSelection;  // reload 200,200,150,150
..
ImageEnView1.RestoreSelection;  // reload 10,10,100,100
!!}
procedure TImageEnView.SaveSelection;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveSelectionToStream(ms);
  fSavedSelection.Add(ms);
end;

// pop a selection from fSavedSelection list

{!!
<FS>TImageEnView.RestoreSelection

<FM>Declaration<FC>
function RestoreSelection(Remove: Boolean=true; Options: <A TIERSOptions> = iersMoveToAdapt): boolean;

<FM>Description<FN>
Restore a saved selection from the selections stack. RestoreSelection returns false when there aren't any selections saved in the stack.

Options specifies how the selection is adapted when it comes from an image with different size or position.

Note: To share the same selection among layers you must use iersSyncLayers.

Remove allows you to remove restored selection from selections stack.

<FM>Example<FC>
ImageEnView1.Select(10, 10, 100, 100);
ImageEnView1.SaveSelection;
ImageEnView1.Select(200, 200, 150, 150);
ImageEnView1.SaveSelection;
ImageEnView1.Deselect;

ImageEnView1.RestoreSelection;  // reload 200,200,150,150
..
ImageEnVIew1.RestoreSelection;  // reload 10,10,100,100
!!}
function TImageEnView.RestoreSelection(Remove: Boolean; Options: TIERSOptions): boolean;
var
  ms: TMemoryStream;
begin
  result := false;
  if fSavedSelection.Count > 0 then
  begin
    ms := fSavedSelection[fSavedSelection.Count - 1];
    ms.Position := 0;
    result := LoadSelectionFromStream(ms, Options);
    if Remove then
    begin
      fSavedSelection.Delete(fSavedSelection.Count - 1);
      FreeAndNil(ms);
    end;
  end;
end;

{!!
<FS>TImageEnView.DiscardSavedSelection

<FM>Declaration<FC>
function DiscardSavedSelection: boolean;

<FM>Description<FN>

DiscardSavedSelection removes the last saved selection from the stack (saved using <A TImageEnView.SaveSelection>).
You can use this method if you don't want to restore the saved selection.

!!}
function TImageEnView.DiscardSavedSelection: boolean;
var
  ms: TMemoryStream;
begin
  result := false;
  if fSavedSelection.Count > 0 then
  begin
    ms := fSavedSelection[fSavedSelection.Count - 1];
    FreeAndNil(ms);
    fSavedSelection.Delete(fSavedSelection.Count - 1);
    result := true;
  end;
end;



{!!
<FS>TImageEnView.RemoveAlphaChannel

<FM>Declaration<FC>
procedure RemoveAlphaChannel(Merge: Boolean=false);

<FM>Description<FN>
The RemoveAlphaChannel procedure removes the alpha channel and its allocated memory. <A TImageEnView.HasAlphaChannel> will be set to false.
If <FC>Merge<FN> is true the image will be merged with the background color using its alpha channel.

<FM>Example<FC>
// remove alpha channel from a gif
ImageEnView.io.LoadFromFile('C:\test.gif');
ImageEnView.RemoveAlphaChannel;

// this add a shadow (that uses the alpha channel) then save it to a jpeg (that cannot support alpha channel)
ImageEnView.Proc.AddSoftShadow(4, 3, 3);
ImageEnView.RemoveAlphaChannel( True );  // merge the alpha channel
ImageEnView.IO.SaveToFile('C:\output.jpg');
!!}
// if merge is true merges the alpha channel with the background color
procedure TImageEnView.RemoveAlphaChannel(Merge: boolean);
begin
  fIEBitmap.RemoveAlphaChannel(Merge, fBackground);
  Update();
end;

function TImageEnView.QuantizeViewX(vx: integer): integer;
begin
  result := vx;
  if result < 0 then
    result := 0;
  if (result < fZZWW) and (fGXBmp2Scr <> nil) and (fXScr2BmpSize > 0) and (result < fGXScr2BmpSize) then
    result := fGXBmp2Scr[fGXScr2Bmp[result]];
end;

function TImageEnView.QuantizeViewY(vy: integer): integer;
begin
  result := vy;
  if result < 0 then
    result := 0;
  if (result < fZZHH) and (fGYBmp2Scr <> nil) and (fYScr2BmpSize > 0) and (result < fGYScr2BmpSize) then
    result := fGYBmp2Scr[fGYScr2Bmp[result]];
end;

procedure TImageEnView.CreateCoordConvLUT;
var
  i, v: integer;
  scrx, scry, bmpx, bmpy: integer;
  qviewx, qviewy: integer;
begin
  if (fLutLastZoomX <> fZoomX) or (fLutLastZoomY<>fZoomY) or (fLutLastFRX <> frx) or (fLutLastFRY <> fry) or (fLutLastMaxLayerWidth <> fMaxLayerWidth) or (fLutLastMaxLayerHeight <> fMaxLayerHeight) then
  begin
    // free olds
    if fGXScr2Bmp <> nil then
      freemem(fGXScr2Bmp);
    if fGYScr2Bmp <> nil then
      freemem(fGYScr2Bmp);
    if fGXBmp2Scr <> nil then
      freemem(fGXBmp2Scr);
    if fGYBmp2Scr <> nil then
      freemem(fGYBmp2Scr);
    fGXScr2Bmp := nil;
    fGYScr2Bmp := nil;
    fGXScr2BmpSize := 0;
    fGYScr2BmpSize := 0;
    fGXBmp2Scr := nil;
    fGYBmp2Scr := nil;
    fXScr2Bmp := nil;
    fYScr2Bmp := nil;
    fXBmp2Scr := nil;
    fYBmp2Scr := nil;
    fXScr2BmpSize := 0;
    fYScr2BmpSize := 0;
    fXBmp2ScrSize := 0;
    fYBmp2ScrSize := 0;
    // check validity
    if (fry = 0) or (frx = 0) then
      exit;
    if (IEBitmap.Width = 0) or (IEBitmap.Height = 0) then
      exit;
    // sizes
    fXScr2BmpSize := frx;
    fYScr2BmpSize := fry;
    fXBmp2ScrSize := trunc(frx * f100DZoomX);
    fYBmp2ScrSize := trunc(fry * f100DZoomY);

    scrx := fZZWW + fXScr2BmpSize + 1;
    scry := fZZHH + fYScr2BmpSize + 1;
    bmpx := trunc(scrx * f100DZoomX) + 1;
    bmpy := trunc(scry * f100DZoomY) + 1;
    fGXScr2BmpSize := (scrx + 1);
    fGYScr2BmpSize := (scry + 1);
    fGXScr2Bmp := allocmem(fGXScr2BmpSize * sizeof(integer));
    fGYScr2Bmp := allocmem(fGYScr2BmpSize * sizeof(integer));
    fGXBmp2Scr := allocmem((bmpx + 1) * sizeof(integer));
    fGYBmp2Scr := allocmem((bmpy + 1) * sizeof(integer));

    if fZoomX > 100 then
    begin
      for i := scrx - 1 downto 0 do
      begin
        v := trunc(i * f100DZoomX);
        fGXBmp2Scr[v] := i;
        fGXScr2Bmp[i] := imin(v, imax(fMaxLayerWidth - 1, 0));
      end;
    end
    else
    begin
      for i := bmpx - 1 downto 0 do
      begin
        v := trunc(i * fZoomD100X);
        fGXBmp2Scr[i] := v;
        fGXScr2Bmp[v] := imin(i, imax(fMaxLayerWidth - 1, 0));
      end;
    end;
    if fZoomY > 100 then
    begin
      for i := scry - 1 downto 0 do
      begin
        v := trunc(i * f100DZoomY);
        fGYBmp2Scr[v] := i;
        fGYScr2Bmp[i] := imin(v, imax(fMaxLayerHeight - 1, 0));
      end;
    end
    else
    begin
      for i := bmpy - 1 downto 0 do
      begin
        v := trunc(i * fZoomD100Y);
        fGYBmp2Scr[i] := v;
        fGYScr2Bmp[v] := imin(i, imax(fMaxLayerHeight - 1, 0));
      end;
    end;

    fLutLastZoomX := fZoomX;
    fLutLastZoomY := fZoomY;
    fLutLastFRX := frx;
    fLutLastFRY := fry;
    fLutLastMaxLayerWidth := fMaxLayerWidth;
    fLutLastMaxLayerHeight := fMaxLayerHeight;
  end;
  if fXScr2BmpSize > 0 then
  begin // check one for all (don't care YScr2BmpSize...)
    qviewx := QuantizeViewX(fViewX);
    qviewy := QuantizeViewY(fViewY);

    fXBmp2Scr := @fGXBmp2Scr[imax(0, fGXScr2Bmp[qViewX] - fo1x)];
    fYBmp2Scr := @fGYBmp2Scr[imax(0, fGYScr2Bmp[qViewY] - fo1y)];
    fXScr2Bmp := @fGXScr2Bmp[qViewX];
    fYScr2Bmp := @fGYScr2Bmp[qViewY];
  end;
end;

{!!
<FS>TImageEnView.Cursor

<FM>Declaration<FC>
property Cursor: TCursor;

<FM>Description<FN>
Specifies the image used for the mouse pointer when it passes over the control.

ImageEn presets:

 1778 <IMG help_images\2.bmp>
 1779 <IMG help_images\3.bmp>
 1780 <IMG help_images\4.bmp>
 1781 <IMG help_images\5.bmp>
 1782 <IMG help_images\6.bmp>
 1783 <IMG help_images\7.bmp>
 1784 <IMG help_images\8.bmp>
 1785 <IMG help_images\9.bmp>
 1786 <IMG help_images\10.bmp>
 1787 <IMG help_images\11.bmp>
 1788 <IMG help_images\12.bmp>
 1789 <IMG help_images\13.bmp>
 1790 <IMG help_images\14.bmp>
 1791 <IMG help_images\15.bmp>
 1792 <IMG help_images\16.bmp>
 1793 <IMG help_images\17.bmp>
 1794 <IMG help_images\18.bmp>
 1795 <IMG help_images\19.bmp>
 1796 <IMG help_images\20.bmp>
 1797 <IMG help_images\21.bmp>
 1798 <IMG help_images\22.bmp>
 1799 <IMG help_images\23.bmp>
 1800 <IMG help_images\24.bmp>
 1801 <IMG help_images\25.bmp>
 1802 <IMG help_images\26.bmp>
 1803 <IMG help_images\27.bmp>
 1804 <IMG help_images\28.bmp>
 1805 <IMG help_images\29.bmp>
 1806 <IMG help_images\30.bmp>
 1807 <IMG help_images\31.bmp>
 1808 <IMG help_images\32.bmp>
 1809 <IMG help_images\33.bmp>
 1810 <IMG help_images\34.bmp>

System:

 -1 <IMG help_images\35.bmp>
 -2 <IMG help_images\36.bmp>
 -3 <IMG help_images\37.bmp>
 -4 <IMG help_images\38.bmp>
 -6 <IMG help_images\39.bmp>
 -7 <IMG help_images\40.bmp>
 -8 <IMG help_images\41.bmp>
 -9 <IMG help_images\42.bmp>
 -10 <IMG help_images\43.bmp>
 -11 <IMG help_images\44.bmp>
 -12 <IMG help_images\45.bmp>
 -13 <IMG help_images\46.bmp>
 -14 <IMG help_images\47.bmp>
 -15 <IMG help_images\48.bmp>
 -16 <IMG help_images\49.bmp>
 -17 <IMG help_images\50.bmp>
 -18 <IMG help_images\51.bmp>
 -19 <IMG help_images\52.bmp>
 -20 <IMG help_images\53.bmp>
 -21 <IMG help_images\54.bmp>
 -22 <IMG help_images\55.bmp>
!!}
procedure TImageEnView.SetCursor(Value: TCursor);
begin
  if assigned(fOnSetCursor) then
    fOnSetCursor(self, Value);
  fLCursor := Value;
  inherited Cursor := Value;
  fCursor := Value;
  //Windows.SetCursor(Screen.Cursors[Value]);
end;

procedure TImageEnView.SetTempCursor(Value: TCursor; bOnMouseDown : Boolean = False);
begin
  if assigned(fOnSetCursor) then
    fOnSetCursor(self, Value);
  if fAutoCursors then
  begin
    inherited Cursor := Value;
    fCursor := Value;

    if bOnMouseDown then
    begin
      // Note: Need special handling for OnMouseDown because a control's cursor cannot be changed on mouse down because it has been captured (until mouse up). So need to change screen's cursor
      fWasScreenCursor := Screen.Cursor;
      Screen.Cursor := Value;
    end;
  end;
end;

// restore the cursor for fLCursor (default cursor)
procedure TImageEnView.RestoreCursor;
begin
  SetTempCursor(fLCursor);
  if fWasScreenCursor <> crNone then
  begin
    Screen.Cursor := fWasScreenCursor;
    fWasScreenCursor := crNone;
  end;
end;

procedure TImageEnView.SetDelayTimer(Value: integer);
begin
  fDelayTimer := Value;
  if (fDelayTimer>0) then
  begin
    SetupAniPolyTimer;
    fAnimPolyTimer.Interval := Value;
  end;
end;

{!!
<FS>TImageEnView.DelayTimer

<FM>Declaration<FC>
property DelayTimer: Integer;

<FM>Description<FN>
ImageEn has a timer that decrements a counter at each tick (you can set the tick delay using DelayTimer property).
This timer controls the selection animation and the application of filters on scrolling (when <A TImageEnView.DelayZoomFilter> is True).
If you set negative values DelayTimer changes its behavior. Negative values represent the maximum CPU time that ImageEn can use to show selections. For example setting:
<FC>ImageEnView.DelayTimer := -10;<FN>
ImageEn will not use much more than 10% of the CPU time.

The default DelayTimer's value is -20 (maximum 20% of the CPU time).

!!}
function TImageEnView.GetDelayTimer: integer;
begin
  result := fDelayTImer;
end;

{!!
<FS>TImageEnView.GradientEndColor

<FM>Declaration<FC>
property GradientEndColor: TColor

<FM>Description<FN>
GradientEndColor specifies the ending color of the gradient when <A TImageEnView.BackgroundStyle> is <FC>iebsGradient<FN>.  The gradient start color is set using <A TImageEnView.Background>. 
!!}
procedure TImageEnView.SetGradientEndColor(Value: TColor);
begin
  fGradientEndColor := Value;
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

{!!
<FS>TImageEnView.CenterImage

<FM>Declaration<FC>
procedure CenterImage;

<FM>Description<FN>
If the image is larger than the component client area, CenterImage will scroll the image so that it is centered.

!!}
procedure TImageEnView.CenterImage;
begin
  SetViewXY(trunc((fIEBitmap.Width*fZoomD100X - ClientWidth)/2), trunc((fIEBitmap.Height*fZoomD100Y - ClientHeight)/2));
end;

{!!
<FS>TImageEnView.LockUpdate

<FM>Declaration<FC>
procedure LockUpdate;

<FM>Description<FN>
LockUpdate disables all calls to the <A TImageEnView.Update> method. LockUpdate and <A TImageEnView.UnlockUpdate> are useful, for example, when resampling multiple layers.

<FM>Example<FC>
ImageEnView1.LockUpdate;
for i := 0 to ImageEnView1.LayersCount-1 do
begin
  ImageEnView1.LayersCurrent := i;
  ImageEnView1.Proc.Resample(300, -1, rfNone); // withtout LockUpdate this call resizes all other layers
end;
ImageEnView1.UnlockUpdate;

!!}
procedure TImageEnView.LockUpdate;
begin
  inc(fUpdateLocked);
end;

{!!
<FS>TImageEnView.UnlockUpdate

<FM>Declaration<FC>
procedure UnlockUpdate;

<FM>Description<FN>
UnLockUpdate re-enables all calls to the <A TImageEnView.Update> method, after a call to <A TImageEnView.LockUpdate>.
LockUpdate and UnLockUpdate are useful, for example, when you need to resample multiple layers.
UnLockUpdate calls <A TImageEnView.Update> when the update can be unlocked.

<FM>Example<FC>
ImageEnView1.LockUpdate;
for i := 0 to ImageEnView1.LayersCount-1 do
begin
  ImageEnView1.LayersCurrent := i;
  ImageEnView1.Proc.Resample(300, -1, rfNone); // without LockUpdate this call resizes all other layers
end;
ImageEnView1.UnlockUpdate;

!!}
procedure TImageEnView.UnLockUpdate;
begin
  UnLockUpdateEx;
  if fUpdateLocked = 0 then
    Update;
end;

{!!
<FS>TImageEnView.UnLockUpdateEx

<FM>Declaration<FC>
procedure UnLockUpdateEx;

<FM>Description<FN>
UnLockUpdateEx re-enables all calls to the <A TImageEnView.Update> method, after a call to <A TImageEnView.LockUpdate>.
LockUpdate and UnLockUpdateEx are useful, for example, when you need to resample multiple layers.
UnLockUpdateEx doesn't call <A TImageEnView.Update> when the update can be unlocked.
!!}
procedure TImageEnView.UnLockUpdateEx;
begin
  if fUpdateLocked>0 then
    dec(fUpdateLocked);
end;

{!!
<FS>TImageEnView.DrawVersion

<FM>Declaration<FC>
property DrawVersion: Boolean;

<FM>Description<FN>
When DrawVersion is True, the ImageEn version and release date are displayed on bottom-left side of the ImageEnView window.
!!}
procedure TImageEnView.SetDrawVersion(v: boolean);
begin
  fDrawVersion := v;
  Paint;
end;

{!!
<FS>TImageEnView.SelectionMaskDepth

<FM>Declaration<FC>
property SelectionMaskDepth: Integer;

<FM>Description<FN>
The SelectionMaskDepth property allows the specifaction of the selection depth in bits.
The default is 1 bit which means a pixel has two states "unselected" (0) or "selected" (1).
If the setting is 8 (8 bit), a pixel can be "unselected" (0), "semi-selected" (1 to 254), or "fully selected" (255). This allows creating soft or feathered selections.

Note: <A TImageEnView.SelectionOptions>=<FC>iesoFilled<FN> is not supported when the SelectionMaskDepth = 8

<FM>Example<FC>
ImageEnView.SelectionMaskDepth := 8;
ImageEnView.SelectionIntensity := 128;
ImageEnView.Select( 10, 10, 100, 100 );
ImageEnView.Proc.Negative;  // the negative is applied at 50 % (128=semi selected)

<IMG help_images\72.bmp>
!!}
procedure TImageEnView.SetSelectionMaskDepth(value: integer);
var
  owidth, oheight: Integer;
begin
  if (value <> fSelectionMaskDepth) and ((value = 1) or (value = 8)) then
  begin
    if fSel then
      Deselect;
    fSelectionMaskDepth := value;
    owidth  := fSelectionMask.Width;
    oheight := fSelectionMask.Height;
    fSelectionMask.AllocateBits(owidth, oheight, fSelectionMaskDepth);
    case value of
      1: fSelectionIntensity := 1;
      8: fSelectionIntensity := 255;
    end;
  end;
end;

{!!
<FS>TImageEnView.SelectionIntensity

<FM>Declaration<FC>
property SelectionIntensity: Integer;

<FM>Description<FN>
The SelectionIntensity property allows the specification of the selection intensity and is valid only when the <A TImageEnView.SelectionMaskDepth> is 8 (8 bit), otherwise it must be 1.
You can assign a value from 0 to 255 which will be applied to the next or current selection.

See also: <A TImageEnView.SelectionMaskDepth>.

<FM>Example<FC>
ImageEnView.SelectionMaskDepth := 8;
ImageEnView.SelectionIntensity := 128;
ImageEnView.Select( 10, 10, 100, 100 );
ImageEnView.Proc.Negative;  // the negative is applied at 50 % (128=semi selected)
!!}
procedure TImageEnView.SetSelectionIntensity(value: integer);
var
  px: pbyte;
  i, j: integer;
begin
  fSelectionIntensity := value;
  if fSel and (fSelectionMask.BitsPerPixel = 8) then
  begin
    // changes current selection
    with fSelectionMask do
    begin
      for i := y1 to y2 do
      begin
        px := Scanline[i];
        inc(px, x1);
        for j := x1 to x2 do
        begin
          if px^ > 0 then
            px^ := value;
          inc(px);
        end;
      end;
    end;
  end;
end;

{!!
<FS>TImageEnView.MakeSelectionFeather

<FM>Declaration<FC>
procedure MakeSelectionFeather(radius: Double);

<FM>Description<FN>
MakeSelectionFeather modifies the current selection by applying a blur effect to make soften the borders.
This property works only if <A TImageEnView.SelectionMaskDepth> is 8.
Radius specifies the feathering intensity (amount of blur).

<FM>Example<FC>
ImageEnView.SelectionMaskDepth := 8;
ImageEnView.Select( 10, 10, 100, 100 );
ImageEnView.MakeSelectionFeather( 4 );

<IMG help_images\73.bmp>
!!}
procedure TImageEnView.MakeSelectionFeather(radius: double);
var
  bmp: TIEBitmap;
  ww, x1, y1, x2, y2: integer;
begin
  if fSel and (fSelectionMask.BitsPerPixel = 8) then
  begin
    AnimPolygonClear(fHPolySel);
    bmp := TIEBitmap.Create;
    bmp.EncapsulateMemory(fSelectionMask.Bits, fSelectionMask.Width, fSelectionmask.Height, ie8g, false);
    ww := imax(trunc(radius * 5), 1);
    x1 := imax(fSelectionMask.X1 - ww, 0);
    y1 := imax(fSelectionMask.Y1 - ww, 0);
    x2 := imin(fSelectionMask.X2 + ww, fSelectionMask.Width - 1);
    y2 := imin(fSelectionMask.Y2 + ww, fSelectionMask.Height - 1);
    _IEGBlurRect8(bmp, x1, y1, x2, y2, radius);
    FreeAndNil(bmp);
    fSelectionMask.SyncRect;
    ShowSelectionEx(true);
    fDBToDraw := true;
    Paint;
  end;
end;

{!!
<FS>TImageEnView.LayersSync

<FM>Declaration<FC>
property LayersSync: Boolean;

<FM>Description<FN>
If LayersSync is true. all layers must have the same size.
When false (the default), layers can have different sizes and can be moved by code or user interaction (see <A TImageEnView.MouseInteract>).
!!}
procedure TImageEnView.SetLayersSync(value: boolean);
begin
  if not fLayersSync then
    exit; // you cannot set LayersSync=true (default), but only LayersSync=false
  if value <> fLayersSync then
  begin
    fLayersSync := value;
    if fLayersSync then
      DoLayersSync;
  end;
end;

{!!
<FS>TImageEnView.LayersDrawBox

<FM>Declaration<FC>
property LayersDrawBox: Boolean;

<FM>Description<FN>
A box is displayed around layers when this property is enabled.
This option has no effect if <A TImageEnView.Layers>[].<A TIELayer.VisibleBox> is set to false.
If Layers[].VisibleBox is true and LayersDrawBox is false, a box is drawn only on the selected layer.
If Layers[].VisibleBox is true and LayersDrawBox is true, a box is drawn on all layers.
!!}
procedure TImageEnView.SetLayersDrawBox(value: boolean);
begin
  if value <> fLayersDrawBox then
  begin
    fLayersDrawBox := value;
    UpdateReason := ieurComponentStuffChanged;
    Update;
  end;
end;

procedure TImageEnView.DoLayerNotify(layer: integer; event: TIELayerEvent);
begin
  SwapSelectionBase;
  if assigned(fOnLayerNotify) then
    fOnLayerNotify(self, layer, event);
  SwapSelectionBase;
end;

{$IFDEF IEINCLUDEDIRECTSHOW}
procedure TImageEnView.DShowNewFrame(var Message: TMessage);
var
  dshow: TIEDirectShow;
begin
  dshow := TIEDirectShow(pointer(Message.LParam));
  dshow.AcceptNextFrame := false;
  if assigned(dshow) and dshow.Connected and assigned(fOnDShowNewFrame) then
    fOnDShowNewFrame(self);
  dshow.AcceptNextFrame := true;
end;

procedure TImageEnView.DShowEvent(var Message: TMessage);
var
  dshow: TIEDirectShow;
  event: integer;
begin
  dshow := TIEDirectShow(pointer(Message.LParam));
  if assigned(dshow) and dshow.Connected then
  begin
    if assigned(fOnDShowEvent) then
      fOnDShowEvent(self)
    else
    begin
      dshow.GetEventCode(event);
      case event of
        IEEC_COMPLETE:
          dshow.EndOfStream := true;
      end;
    end;
  end;
end;
{$ENDIF}


{$IFDEF IEINCLUDEMEDIAFOUNDATION}
procedure TImageEnView.MediaFoundationNotify(var Message: TMessage);
var
  notify: TIEMediaFountationNotifyType;
  mediaFoundation: TObject;
begin
  notify := TIEMediaFountationNotifyType(Message.WParam);
  mediaFoundation := TObject(pointer(Message.LParam));
  if assigned(mediaFoundation) then
  begin
    if assigned(fOnMediaFoundationNotify) then
      fOnMediaFoundationNotify(self, mediaFoundation, notify);
  end;
end;
{$ENDIF}


{!!
<FS>TImageEnView.SelectionAspectRatio

<FM>Declaration<FC>
property SelectionAspectRatio: Double;

<FM>Description<FN>
Specifies the selection aspect ratio.
If SelectionAspectRatio is -1, the aspect ratio is active only when user press the ALT key, and it is automatically calculated.
If SelectionAspectRatio is 0, the size of the selection is fixed and determined by <A TImageEnView.SelectionAbsWidth> and <A TImageEnView.SelectionAbsHeight> properties.
If SelectionAspectRatio is >0, ImageEn maintains the specified aspect.

<FM>Example<FC>
// we want standard behavior
ImageEnView.SelectionAspectRatio := -1;

// we want a fixed selection of 100 x 100 pixels
ImageEnView.SelectionAbsWidth := 100;
ImageEnView.SelectionAbsHeight := 100;
ImageEnView.SelectionAspectRatio := 0;

// we want a fixed aspect ratio of 2 (height = 2 x width)
ImageEnView.SelectionAspectRatio := 2;
!!}
procedure TImageEnView.SetSelectionAspectRatio(value: Double);
begin
  if value<>fSelectionAspectRatio then
  begin
    fSelectionAspectRatio := value;
    if fSelectionAspectRatio = 0 then
      SelectionOptions := SelectionOptions - [iesoSizeable]
    else
      SelectionOptions := SelectionOptions + [iesoSizeable];
  end;
end;

{$ifdef IEINCLUDEFLATSB}
{!!
<FS>TImageEnView.FlatScrollBars

<FM>Declaration<FC>
property FlatScrollBars: Boolean;

<FM>Description<FN>
Specifies whether the component's scroll bars are flat.
ImageEn only supports flat scroll bars if the system comctl32.dll is version 472 or later.
This property must be set at Design Time (has not effect at run-time)
!!}
procedure TImageEnView.SetFlatScrollBars(Value: Boolean);
begin
  if Value<>fFlatScrollBars then
  begin
    fFlatScrollBars := Value;
    if fFlatScrollBars then
    begin
      IESetScrollRange(Handle, SB_HORZ, 0, 65535, false, false);  // Why this? Please ask to Microsoft programmers!
      IESetScrollRange(Handle, SB_VERT, 0, 65535, false, false);  // Why this? Please ask to Microsoft programmers!
      InitializeFlatSB(Handle);
      IEShowScrollBar(handle, SB_HORZ, false, true); // Why this? Please ask to Microsoft programmers!
      IEShowScrollBar(handle, SB_VERT, false, true); // Why this? Please ask to Microsoft programmers!
    end
    else
      UninitializeFlatSB(Handle);
  end;
end;
{$endif}

procedure TImageEnView.SwapSelectionBase;
var
  x: TIESelectionBase;
begin
  x := fSavedSelectionBase;
  fSavedSelectionBase := fSelectionBase;
  fSelectionBase := x;
end;

procedure TImageEnView.DoZoomIn(var NewZoom: double);
begin
  SwapSelectionBase;
  if assigned(fOnZoomIn) then
    fOnZoomIn(self, NewZoom);
  SwapSelectionBase;
end;

procedure TImageEnView.DoZoomOut(var NewZoom: double);
begin
  SwapSelectionBase;
  if assigned(fOnZoomOut) then
    fOnZoomOut(self, NewZoom);
  SwapSelectionBase;
end;

procedure TImageEnView.DoMouseInResizingGrip(Grip: TIEGrip);
begin
  SwapSelectionBase;
  if assigned(fOnMouseInResizingGrip) then
    fOnMouseInResizingGrip(self, Grip);
  SwapSelectionBase;
end;

procedure TImageEnView.DoMouseInSel;
begin
  SwapSelectionBase;
  if assigned(fOnMouseInSel) then
    fOnMouseInSel(self);
  SwapSelectionBase;
end;

procedure TImageEnView.NavigatorSelectionChanging(Sender: TObject);
var
  x, y: Integer;
begin
  if fNavigatorInside then
    exit;
  fNavigatorInside := true;
  x := round( Zoom/100 * fNavigator.SelX1 );
  y := round( Zoom/100 * fNavigator.SelY1 );
  SetViewXY(x, y);
  fNavigatorInside := false;
end;

procedure TImageEnView.NavigatorMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  dPos: double;
  dir: integer;
begin
  if WheelDelta > 0 then
    dir := -1
  else
    dir := 1;
  if fNavigator.fMouseWheelParams.InvertDirection then
    dir := -1 * dir;
  dPos := fZoomX;
  case fNavigator.fMouseWheelParams.Variation of
    iemwAbsolute:
      dPos := fZoomX + dir * fNavigator.fMouseWheelParams.Value;
    iemwPercentage:
      dPos := fZoomX + imax(round(fZoomX * fNavigator.fMouseWheelParams.Value / 100), 1) * dir;
  end;
  if (dPos > fZoomX) then
    DoZoomIn(dPos);
  if (dPos < fZoomX) then
    DoZoomOut(dPos);
  if fNavigator.fMouseWheelParams.ZoomPosition = iemwCenter then
    SetZoom(dPos)
  else
    ZoomAt(MousePos.X, MousePos.Y, dPos, false);
  Handled := true;
end;

procedure TImageEnView.SetNavigatorRect;
var
  x1, y1, x2, y2: Integer;
begin
  if fNavigatorInside or (fLockPaint > 0) then
    exit;
  fNavigatorInside := true;

  Paint;  // this updates offsets
  x1 := XScr2Bmp( OffsetX );
  y1 := YScr2Bmp( OffsetY );
  x2 := XScr2Bmp( OffsetX+ExtentX );
  y2 := YScr2Bmp( OffsetY+ExtentY );

  fNavigator.SelectionAbsWidth  := x2-x1;
  fNavigator.SelectionAbsHeight := y2-y1;
  fNavigator.SelectionAspectRatio := 0;

  fNavigator.Select(x1, y1, x2, y2, iespReplace);

  fNavigatorInside := false;
end;

{!!
<FS>TImageEnView.SetNavigator

<FM>Declaration<FC>
procedure SetNavigator(nav: <A TImageEnView>; options: <A TIENavigatorOptions>);

<FM>Description<FN>
SetNavigator specifies a TImageEnView component which works as navigator of current image. A navigator shows a selection that controls the zoom and scroll of the main control.
Use <FC>options<FN> to fine-tune navigator behavior.

<FM>Demo<FN>
Display\Navigator

<FM>Example<FC>
ImageEnView1.SetNavigator(ImageEnView2, [ienoMouseWheelZoom, ienoMarkOuter]);
ImageEnView1.IO.LoadFromFile('input.jpg');
!!}
procedure TImageEnView.SetNavigator(nav: TImageEnView; options: TIENavigatorOptions);
begin
  if fNavigator<>nil then
  begin
    fNavigator.OnSelectionChanging := nil;
    fNavigator.SetExternalBitmap(nil);
    fNavigator.fIsNavigator := false;
  end;

  fNavigator := nav;

  if fNavigator<>nil then
  begin
    fNavigator.fIsNavigator := true;
    fNavigator.EnableAlphaChannel := EnableAlphaChannel;
    fNavigator.AutoFit := True;
    fNavigator.MouseInteract := [miSelect];
    fNavigator.SelectionBase := iesbBitmap;
    fNavigator.SelectionOptions := [iesoMoveable, iesoCanScroll];
    fNavigator.SetExternalBitmap( IEBitmap );
    fNavigator.fOnSelectionChanging := NavigatorSelectionChanging;
    fNavigator.fNavigatorOptions := options;
    if ienoMOUSEWHEELZOOM in options then
      fNavigator.OnMouseWheel := NavigatorMouseWheel;
    fNavigator.FreeNotification(self);
    SetNavigatorRect;
  end;
end;

procedure TImageEnView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = fNavigator) and (Operation = opRemove) then
  begin
    fNavigator := nil;
  end;
end;

{!!
<FS>TImageEnView.SetExternalBitmap

<FM>Declaration<FC>
procedure SetExternalBitmap(bmp: <A TIEBitmap>);

<FM>Description<FN>
SetExternalBitmap allows you to connect one TImageEnView component to another one and share the same bitmap.
This is useful to view the same image with multiple TImageEnView components, but loading the image only once.

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
ImageEnView2.SetExternalBitmap( ImageEnView1.IEBitmap );

..Both ImageEnView1 and ImageEnView2 now show input.jpg.

!!}
procedure TImageEnView.SetExternalBitmap(bmp: TIEBitmap);
begin
  if fLayersCurrent>-1 then
  begin
    if bmp<>nil then
    begin
      if CurrentLayer.FreeBitmapOnDestroy then
      begin
        if fIEBitmap.EncapsulatedFromTBitmap then
          FreeAndNil(fBitmap);
        FreeAndNil(fIEBitmap);
      end;

      CurrentLayer.Bitmap := bmp;
      CurrentLayer.FreeBitmapOnDestroy := false;

      fIEBitmap := CurrentLayer.Bitmap;
      if fIEBitmap.EncapsulatedFromTBitmap then
        fBitmap := fIEBitmap.VclBitmap
      else
        fBitmap := nil;
      CallBitmapChangeEvents;
      Update;
    end
    else
    begin
      CurrentLayer.FreeBitmapOnDestroy := true;
      fIEBitmap := TIEBitmap.Create;
      fBitmap := nil;
    end;
  end;
end;


{!!
<FS>TImageEnView.ImageEnVersion

<FM>Declaration<FC>
property ImageEnVersion: String;

<FM>Description<FN>
Returns the ImageEn version as a string.

!!}
function TImageEnView.GetImageEnVersion: String;
begin
  result := IEMAINVERSION;
end;

procedure TImageEnView.SetImageEnVersion(Value: String);
begin
  // this is a read-only property, but it must be displayed in object inspector
end;

{$ifdef IEDOTNETVERSION}
procedure TImageEnView.WMContextMenu(var Message: TWMContextMenu);
begin
  // just to remove Delphi default behavior
end;
{$endif}

{!!
<FS>TImageEnView.ChangeResolution

<FM>Declaration<FC>
procedure ChangeResolution(NewDPI: Integer; ResampleFilter: <A TResampleFilter>);

<FM>Description<FN>
Modifies the DPI of the image, resampling it and setting the new DPI values.

Note: You should ensure the source DPI contains valid values before calling ChangeResolution. This method has no effect if DPI is 0.

<FM>Example<FC>
// load input.jpg which has 100 dpi
ImageEnView1.IO.LoadFromFile('C:\input.jpg');
// we want 75 dpi
ImageEnView1.ChangeResolution(75, rfTriangle);
// save back as 75 dpi
ImageEnView1.IO.SaveToFile('C:\output.jpg');

!!}
procedure TImageEnView.ChangeResolution(NewDPI: Integer; ResampleFilter: TResampleFilter);
var
  olddpi: Integer;
  newheight: Integer;
  proc: TImageEnProc;
begin
  if (DPIX = 0) or (DPIY = 0) then
    exit;
  olddpi := DPIY;
  newheight := round(fIEBitmap.Height / olddpi * NewDPI);
  proc := TImageEnProc.CreateFromBitmap(fIEBitmap);
  try
    proc.AutoUndo := False;
    proc.OnProgress := fOnProgress;
    proc.OnFinishWork := fOnFinishWork;
    proc.Resample(-1, newheight, ResampleFilter);
  finally
    FreeAndNil(proc);
  end;
  DPIY := NewDPI;
  DPIX := round(DPIX / olddpi * NewDPI);
  CallBitmapChangeEvents;
  Update;
end;

{!!
<FS>TImageEnView.LayersSaveToFile


<FM>Declaration<FC>
procedure LayersSaveToFile(const FileName: String; CompressionFormat: <A TIOFileType>);

<FM>Description<FN>
LayersSaveToFile saves all layers including the bitmap, position, size, etc. in a proprietary file format. This allows TImageEnView to rebuild a previous saved configuration of layers.
All images will be compressed to the specified format, but the file which contains all layers is an ImageEn custom file format.

If <FC>CompressionFormat<FN> is -1, an internal compressed format is used. This format preserves pixel format and alpha channel.
If <FC>CompressionFormat<FN> is -2, an internal non-compressed format is used. This format preserves pixel format and alpha channel.

Note: <FC>CompressionFormat<FN> cannot be <FC>ioTIFF<FN>.

<FM>Example<FC>
// saves current layers configuration (compress images as jpeg)
ImageEnView1.LayersSaveToFile('C:\layers.lyr', -1);

// loads saved layers
ImageEnView1.LayersLoadFromFile('C:\layers.lyr');

!!}
// warning: ioTIFF not supported
// if compressionFormat = -1 then use internal compressed format (preserves pixelformat and transparency)
// if compressionFormat = -2 then use internal noncompressed format (preserves pixelformat and transparency)
procedure TImageEnView.LayersSaveToFile(const FileName: String; CompressionFormat: TIOFileType);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    LayersSaveToStream(fs, CompressionFormat);
  finally
    FreeAndNil(fs);
  end;
end;

{!!
<FS>TImageEnView.LayersLoadFromFile

<FM>Declaration<FC>
function LayersLoadFromFile(const FileName: String): Boolean;

<FM>Description<FN>
LayersLoadFromFile loads all layers including the bitmap, position, size, etc. from a file saved with <A TImageEnView.LayersSaveToStream> or <A TImageEnView.LayersSaveToFile>.
It allows TImageEnView to rebuild a previous saved configuration of layers.

Returns True on success.

<FM>Example<FC>
// save current layers configuration (compress images as jpeg)
ImageEnView1.LayersSaveToFile('C:\layers.lyr', ioJPEG);

// load saved layers
ImageEnView1.LayersLoadFromFile('C:\layers.lyr');

!!}
function TImageEnView.LayersLoadFromFile(const FileName: String): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := LayersLoadFromStream(fs); // this calls Update
  finally
    FreeAndNil(fs);
  end;
end;

type

TLayerHeader=packed record
  Magic: array [0..12] of AnsiChar;  // must be IELAYERSFILEMAGIC
  Version: Integer;                  // must be IELAYERSFILEVER
  FileFormat: TIOFileType;
  LayersCount: Integer;
end;

const
  IELAYERSFILEVER: integer       = 7;
  IELAYERSFILEMAGIC: AnsiString  = 'IMAGEENLAYERS';
  IEIMAGEENVIEWSTATE: AnsiString = 'IMAGEENVIEWSTATE';

{!!
<FS>TImageEnView.LayersSaveToStream

<FM>Declaration<FC>
procedure LayersSaveToStream(Stream: TStream; CompressionFormat: <A TIOFileType>);

<FM>Description<FN>
LayersSaveToStream saves all layers including the bitmap, position, size, etc. in a proprietary format.
This allows TImageEnView to rebuild a previous saved configuration of layers.
All images will be compressed to the specified format, but the file which contains all layers is an ImageEn custom file format.

If <FC>CompressionFormat<FN> is -1, an internal compressed format is used. This format preserves pixel format and alpha channel.
If <FC>CompressionFormat<FN> is -2, an internal non-compressed format is used. This format preserves pixel format and alpha channel.

Note: <FC>CompressionFormat<FN> cannot be <FC>ioTIFF<FN>.

<FM>Example<FC>
// saves current layers configuration (compress images as jpeg)
ImageEnView1.LayersSaveToFile('C:\layers.lyr', -1);

// loads saved layers                         
ImageEnView1.LayersLoadFromFile('C:\layers.lyr');
!!}
// warning: ioTIFF not supported
// if CompressionFormat = -1 then use internal compressed format (preserves pixelformat and transparency)
// if CompressionFormat = -2 then use internal non-compressed format (preserves pixelformat and transparency)
procedure TImageEnView.LayersSaveToStream(Stream: TStream; CompressionFormat: TIOFileType);
var
  recHead: TLayerHeader;
  i, k: Integer;
  p1, sz: int64;
  p2: TList;
  ll, hh: Dword;
  lcurrent: Integer;
  LZStream: TZCompressionStream;
  ms: TMemoryStream;
  dw: Dword;
  lconstrains: Boolean;
begin

  lcurrent := LayersCurrent;
  lconstrains := LayersSelectConstrains;

  LayersSelectConstrains := false;

  p2 := TList.Create;

  try

    {$ifdef IEINCLUDEPNG}
    if CompressionFormat = ioTIFF then
      CompressionFormat := ioPNG;
    {$endif}

    // prepare header
    recHead.Version := IELAYERSFILEVER;
    recHead.LayersCount := fLayers.Count;
    recHead.FileFormat := CompressionFormat;
    move(IELAYERSFILEMAGIC[1], recHead.Magic[0], length(IELAYERSFILEMAGIC));

    // write header
    Stream.Write(recHead, sizeof(TLayerHeader));

    // write layers data
    for i := 0 to fLayers.Count-1 do
    begin
      Stream.Write(Layers[i].Visible, sizeof(boolean));
      Stream.Write(Layers[i].VisibleBox, sizeof(boolean));
      Stream.Write(Layers[i].Transparency, sizeof(integer));
      Stream.Write(Layers[i].Opacity, sizeof(double));
      Stream.Write(Layers[i].PosX, sizeof(integer));
      Stream.Write(Layers[i].PosY, sizeof(integer));
      Stream.Write(Layers[i].Cropped, sizeof(boolean));
      Stream.Write(Layers[i].Locked, sizeof(boolean));
      Stream.Write(Layers[i].Magnify.Enabled, sizeof(boolean));
      Stream.Write(Layers[i].Magnify.Rate, sizeof(double));
      Stream.Write(Layers[i].Magnify.Style, sizeof(TIEMagnifyStyle));
      Stream.Write(Layers[i].Magnify.Source, sizeof(TIEMagnifySource));  // v.3
      Stream.Write(Layers[i].Operation, sizeof(TIERenderOperation));
      Stream.Write(Layers[i].ResizedWidth, sizeof(integer));
      Stream.Write(Layers[i].ResizedHeight, sizeof(integer));
      Stream.Write(Layers[i].ResampleFilter, sizeof(TResampleFilter));   // v.3
      Stream.Write(Layers[i].UseResampleFilter, sizeof(boolean));        // v.3
      IESaveStringToStream(Stream, Layers[i].Name);                        // v.3
      Stream.Write(Layers[i].UserDataLen, sizeof(integer));              // v.3
      if Layers[i].UserDataLen>0 then
        Stream.Write(pbyte(Layers[i].UserData)^, Layers[i].UserDataLen); // v.3
      Stream.Write(Layers[i].IsMask, sizeof(boolean));                   // v.3
      Stream.Write(Layers[i].Selectable, sizeof(boolean));               // v.4
      Stream.Write(Layers[i].DrawOuter, sizeof(boolean));                // v.6
      k := Stream.Position;
      p2.Add(pointer(k));         // save position of fileposition field
      Stream.Seek(8, soCurrent);  // unknown position (lo and hi)
    end;

    // write images
    for i := 0 to fLayers.Count-1 do
    begin
      p1 := Stream.Position;

      // save image position
      Stream.Position := integer(p2[i]);
      ll := p1 and $FFFFFFFF;
      hh := (p1 shr 32) and $FFFFFFFF;
      Stream.Write(ll, sizeof(dword));
      Stream.Write(hh, sizeof(dword));

      Stream.Position := p1;
      LayersCurrent := i;

      if (CompressionFormat = -1) or (CompressionFormat = -2) then
      begin
        // use internal format (preserves pixelformat and alpha channel). Also this is LZ compressed if CompressionFormat = -1.
        ms := TMemoryStream.Create;
        try
          sz := fIEBitmap.Height*fIEBitmap.Rowlen div 2; // aproximated size
          if fIEBitmap.HasAlphaChannel then
            sz := sz + fIEBitmap.AlphaChannel.Height*fIEBitmap.AlphaChannel.Rowlen;
          if sz < IEGetMemory(true) then
          begin
            try
              ms.Size := sz;
            except
            end;
          end;

          if CompressionFormat = -1 then
          begin
            LZStream := TZCompressionStream.Create(ms, zcDefault, 15);
            try
              fIEBitmap.SaveRAWToBufferOrStream(nil, LZStream, false);
            finally
              FreeAndNil(LZStream);
            end;
          end
          else
            fIEBitmap.SaveRAWToBufferOrStream(nil, ms, true);

          sz := ms.Position;
          dw := sz; Stream.Write(dw, sizeof(dword));  // write compressed stream size
          ms.Position := 0;
          if sz > 0 then
            IECopyFrom(Stream, ms, sz);
        finally
          FreeAndNil(ms);
        end;
      end
      else
      begin
        // use standard image formats
        case fIEBitmap.PixelFormat of
          ie1g:
            begin
              IO.Params.BitsPerSample := 1;
              IO.Params.SamplesPerPixel := 1;
            end;
          ie8g:
            begin
              IO.Params.BitsPerSample := 8;
              IO.Params.SamplesPerPixel := 1;
              IO.Params.JPEG_ColorSpace := ioJPEG_GRAYLEV;
            end;
          else
            begin
              IO.Params.BitsPerSample := 8;
              IO.Params.SamplesPerPixel := 3;
              IO.Params.JPEG_ColorSpace := ioJPEG_YCbCr;
            end;
        end;
        IO.SaveToStream(Stream, CompressionFormat);
      end;
      Stream.Seek(0, soEnd);
    end;

  finally

    p2.Free;

    LayersCurrent := lcurrent;
    LayersSelectConstrains := lconstrains;

  end;

end;

function IETryLYR(Stream: TStream): Boolean;
var
  l: int64;
  recHead: TLayerHeader;
begin
  l := Stream.Position;
  result := true;
  Stream.Read(recHead, sizeof(TLayerHeader));
  if (StrLComp(recHead.Magic, PAnsiChar(IELAYERSFILEMAGIC), length(IELAYERSFILEMAGIC)) <> 0) or (recHead.Version <= 0) then
    result := false;
  Stream.Position := l;
end;

{!!
<FS>TImageEnView.LayersLoadFromStream

<FM>Declaration<FC>
function LayersLoadFromStream(Stream: TStream): Boolean;

<FM>Description<FN>
LayersLoadFromStream loads all layers including the bitmap, position, size, etc. from a file saved using <A TImageEnView.LayersSaveToStream> or <A TImageEnView.LayersSaveToFile>. It allows TImageEnView to rebuild a previous saved layers configuration.

Returns True on success.

<FM>Example<FC>
// Saves current layers configuration (compress images as jpeg)
ImageEnView1.LayersSaveToFile('C:\layers.lyr', ioJPEG);

// load saved layers
ImageEnView1.LayersLoadFromFile('C:\layers.lyr');

!!}
function TImageEnView.LayersLoadFromStream(Stream: TStream): Boolean;
var
  recHead: TLayerHeader;
  i: Integer;
  p1: int64;
  poslo, poshi: Dword;
  lnative: Boolean;
  LZStream: TZDecompressionStream;
  ms: TMemoryStream;
  dw: Dword;
begin
  result := false;

  // read header
  Stream.Read(recHead, sizeof(TLayerHeader));
  if StrLComp(recHead.Magic, PAnsiChar(IELAYERSFILEMAGIC), length(IELAYERSFILEMAGIC)) <> 0 then
    exit;
  if recHead.Version > 0 then
  begin
    for i := fLayers.Count-1 downto 1 do
      LayersRemove(i);

    // read layers data and images
    for i := 0 to recHead.LayersCount-1 do
    begin
      if i>0 then
        LayersAdd;

      Stream.Read(CurrentLayer.Visible, sizeof(boolean));
      Stream.Read(CurrentLayer.VisibleBox, sizeof(boolean));
      Stream.Read(CurrentLayer.Transparency, sizeof(integer));
      if recHead.Version >= 7 then
        Stream.Read(CurrentLayer.Opacity, sizeof(double));
      Stream.Read(CurrentLayer.PosX, sizeof(integer));
      Stream.Read(CurrentLayer.PosY, sizeof(integer));
      Stream.Read(CurrentLayer.Cropped, sizeof(boolean));
      Stream.Read(CurrentLayer.Locked, sizeof(boolean));
      Stream.Read(CurrentLayer.Magnify.Enabled, sizeof(boolean));
      Stream.Read(CurrentLayer.Magnify.Rate, sizeof(double));
      Stream.Read(CurrentLayer.Magnify.Style, sizeof(TIEMagnifyStyle));
      if recHead.Version>=3 then
        Stream.Read(CurrentLayer.Magnify.Source, sizeof(TIEMagnifySource));
      Stream.Read(CurrentLayer.Operation, sizeof(TIERenderOperation));
      Stream.Read(CurrentLayer.ResizedWidth, sizeof(integer));
      Stream.Read(CurrentLayer.ResizedHeight, sizeof(integer));
      if recHead.Version>=3 then
      begin
        Stream.Read(CurrentLayer.ResampleFilter, sizeof(TResampleFilter));
        Stream.Read(CurrentLayer.UseResampleFilter, sizeof(boolean));
        IELoadStringFromStream(Stream, CurrentLayer.Name);

        if (CurrentLayer.UserDataLen > 0) and (CurrentLayer.UserData <> nil) then
        begin
          freemem(CurrentLayer.UserData);
          CurrentLayer.UserDataLen := 0;
          CurrentLayer.UserData    := nil;
        end;

        Stream.Read(CurrentLayer.UserDataLen, sizeof(integer));
        if CurrentLayer.UserDataLen > 0 then
        begin
          getmem(CurrentLayer.UserData, CurrentLayer.UserDataLen);
          Stream.Read(pbyte(CurrentLayer.UserData)^, CurrentLayer.UserDataLen);
        end;
        Stream.Read(CurrentLayer.IsMask, sizeof(boolean));
      end;
      if recHead.Version>=4 then
        Stream.Read(CurrentLayer.Selectable, sizeof(boolean));
      if recHead.Version>=6 then
        Stream.Read(CurrentLayer.DrawOuter, sizeof(boolean));
      CurrentLayer.ClientAreaBox := Rect(0, 0, 0, 0);
      Stream.Read(poslo, sizeof(dword));
      Stream.Read(poshi, sizeof(dword));

      p1 := Stream.Position;
      Stream.Position := poslo+(poshi shl 32);
      LayersCurrent := i;

      if (recHead.FileFormat = -1) or (recHead.FileFormat = -2) then
      begin
        // use internal file format
        Stream.Read(dw, sizeof(dword));  // compressed stream size
        ms := TMemoryStream.Create;
        try
          ms.SetSize(dw); // to speed up loading
          if dw > 0 then
            IECopyFrom(ms, Stream, dw);
          ms.Position := 0;
          if recHead.FileFormat = -1 then
          begin
            LZStream := TZDecompressionStream.Create(ms);
            try
              fIEBitmap.LoadRAWFromBufferOrStream(nil, LZStream);
            finally
              FreeAndNil(LZStream);
            end;
          end
          else
            fIEBitmap.LoadRAWFromBufferOrStream(nil, ms);
        finally
          FreeAndNil(ms);
        end;
      end
      else
      begin
        // use standard file formats
        lnative := IO.NativePixelFormat;
        if CurrentLayer.IsMask then
          IO.NativePixelFormat := true;
        IO.LoadFromStreamFormat(Stream, recHead.FileFormat);
        IO.NativePixelFormat := lnative;
      end;

      Stream.Position := p1;

    end;

    result := true;
  end;
  if assigned(fImageEnIO) then
  begin
    IO.Params.Width :=  Layers[0].Bitmap.Width;
    IO.Params.Height := Layers[0].Bitmap.Height;
    IO.Params.OriginalWidth :=  Layers[0].Bitmap.Width;
    IO.Params.OriginalHeight := Layers[0].Bitmap.Height;
  end;
  Update;
end;

{!!
<FS>TImageEnView.Wallpaper

<FM>Declaration<FC>
property Wallpaper: TPicture;

<FM>Description<FN>
The Wallpaper property allows you to use an image for your background (behind the main image and layers).
Use <A TImageEnView.WallpaperStyle> to specify how to paint the wallpaper.

<FM>Example<FN>
// Tile a bitmap over the background
ImageEnView1.WallPaperStyle := iewoTile
ImageEnView1.WallPaper.LoadFromFile('D:\MyWallpaper.bmp');

// Clear the wallpaper
ImageEnView1.WallPaper := nil;
!!}
procedure TImageEnView.SetWallPaper(Value: TPicture);
begin
  if assigned(Value) then
  begin
    if not assigned(fWallPaper) then
      fWallPaper := TPicture.Create;
    fWallPaper.Assign(Value);
  end
  else
  begin
    // Reset TPicture
    if assigned(fWallPaper) then
      FreeAndNil(fWallPaper);
    fWallPaper := TPicture.Create;
  end;

  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

{!!
<FS>TImageEnView.WallpaperStyle

<FM>Declaration<FC>
property WallpaperStyle: <A TIEWallpaperStyle>;

<FM>Description<FN>
If you are using an image for your background (specified by <A TImageEnView.Wallpaper>), then WallpaperStyle defines how it will be painted.

<FM>Example<FN>
// Tile a bitmap over the background
ImageEnView1.WallPaperStyle := iewoTile
ImageEnView1.WallPaper.LoadFromFile('D:\MyWallpaper.bmp');
!!}
procedure TImageEnView.SetWallPaperStyle(Value: TIEWallPaperStyle);
begin
  if Value <> fWallPaperStyle then
  begin
    fWallPaperStyle := Value;
    UpdateReason := ieurComponentStuffChanged;
    Update;
  end;
end;

procedure TImageEnView.DrawBackgroundToCanvas(ACanvas: TCanvas; iWidth: Integer = -1; iHeight: Integer = -1);
begin
  if (iWidth = -1) and (iHeight = -1) then
  begin
    iWidth  := Width;
    iHeight := Height;
  end;

  // draws only the background
  IEDrawBackGround(ComponentState, ACanvas, nil, fBackgroundStyle, fBackground, 0, 0, iWidth, iHeight, 0, 0, 0, 0, fChessboardSize, fChessboardBrushStyle, fGradientEndColor, fWallPaper, fWallPaperStyle);
end;

{!!
<FS>TImageEnView.VisibleBitmapRect

<FM>Declaration<FC>
property VisibleBitmapRect : TRect;  (read/write)

<FM>Description<FN>
Show a specific area of the current image, or return the area of the image that is shown.

Note: The rectangle passed will be automatically adjusted to ensure the correct aspect ratio.

<FM>Examples<FC>
// Show the central portion of an image
ImageEnView1.VisibleBitmapRect := Rect( 100, 100, 200, 200 );

// Output the visible bitmap in ImageEnView1 to ImageEnView2
ImageEnView2.IEBitmap.Width  := IERectangle( ImageEnView1.VisibleBitmapRect ).Width;
ImageEnView2.IEBitmap.Height := IERectangle( ImageEnView1.VisibleBitmapRect ).Height;
ImageEnView1.IEBitmap.DrawToTIEBitmap( ImageEnView2.IEBitmap, 0, 0, IERectangle( ImageEnView1.VisibleBitmapRect ) );
ImageEnView2.Update;

<FM>See Also<FN>
- <A TImageEnView.ViewX>
- <A TImageEnView.ViewY>
- <A TImageEnView.ExtentX>
- <A TImageEnView.ExtentY>
!!}
function TImageEnView.GetVisibleBitmapRect : TRect;
begin
  Result.Left   := XScr2Bmp( OffsetX );
  Result.Top    := YScr2Bmp( OffsetY );
  Result.Right  := XScr2Bmp( ClientWidth - OffsetX ) - 1;
  Result.Bottom := YScr2Bmp( ClientHeight - OffsetY ) - 1;
end;

procedure TImageEnView.SetVisibleBitmapRect(ARect: TRect);
var
  dZoom: Double;
begin
  LockPaint;
  try
    ARect := AdjustRectToAspectRatio(ARect);
    if (ARect.Bottom-ARect.Top) = 0 then
      exit;
    dZoom := ClientHeight / (ARect.Bottom-ARect.Top);
    Zoom := dZoom * 100;
    ViewX := round(ARect.left * dZoom);
    ViewY := round(ARect.top * dZoom );
  finally
    UnlockPaint;
  end;
end;

{!!
<FS>TImageEnView.LayersCreateFromSelection

<FM>Declaration<FC>
function LayersCreateFromSelection: Integer;

<FM>Description<FN>
Creates a new layer from the current selection. This might be used to copy & paste selections, for example.
Returns the index of the new layer.

<FM>Example<FC>
// select an ellipse
ImageEnView1.SelectEllipse(150, 150, 50, 50);
// copy selected area and create a new layer
ImageEnView1.LayersCreateFromSelection;
// move the new layer
ImageEnView1.CurrentLayer.PosX := 100;
ImageEnView1.CurrentLayer.PosY := 100;
// paste to the background
ImageEnView1.LayersMerge(0, 1);

!!}
function TImageEnView.LayersCreateFromSelection: Integer;
var
  baseLayer: Integer;
begin
  baseLayer := LayersCurrent;
  result := LayersAdd;
  LayersCurrent := baseLayer;
  if Selected then
  begin
    CopySelectionToIEBitmap( Layers[result].Bitmap, (fSelectionMaskDepth = 1) );
    Layers[result].PosX := SelX1 + CurrentLayer.PosX;
    Layers[result].PosY := SelY1 + CurrentLayer.posY;
  end
  else
  begin
    Layers[Result].Bitmap.Assign( CurrentLayer.Bitmap );
    Layers[result].PosX := CurrentLayer.PosX;
    Layers[result].PosY := CurrentLayer.posY;
  end;
  LayersCurrent := result;  // this disables selection
end;

{!!
<FS>TImageEnView.LayersCreateFromClipboard

<FM>Declaration<FC>
function LayersCreateFromClipboard: Integer;

<FM>Description<FN>
Creates a new layer with the content of the clipboard.
Returns the index of the new layer.
!!}
function TImageEnView.LayersCreateFromClipboard: Integer;
begin
  result := LayersAdd;
  Proc.PasteFromClipboard;
end;


{!!
<FS>TImageEnView.LayersCreateFromFile

<FM>Declaration<FC>
function LayersCreateFromFile(const FileName : string) : Integer;

<FM>Description<FN>
Loads an image from a file and adds it as a new layer.
Returns the index of the new layer, or -1 in case of error.
!!}
function TImageEnView.LayersCreateFromFile(const FileName : string) : integer;
begin
  Result := LayersAdd;
  try
    IO.LoadFromFile(Filename);
    if IO.Aborting then
      raise EIEException.create('Load Error');
  except
    LayersRemove(Result);
    Result := -1;
  end;
end;



{!!
<FS>TImageEnView.LayersCreateFromText

<FM>Declaration<FC>
function LayersCreateFromText(const sText : string;
                              const sFontName : string;
                              iFontSize : Integer;
                              cFontColor : TColor;
                              Style : TFontStyles;
                              bAddShadow: boolean = False;
                              iBlurRadius : Integer = 3;
                              iShadowOffset : Integer = 2;
                              Angle : Integer = 0;
                              bAntiAlias : Boolean = true) : integer;

<FM>Description<FN>
Adds a new layer filled with text of the specified font.
If bAddShadow is true then a <L TImageEnProc.AddSoftShadow>soft shadow</L> is added.
Angle is the Escapement value, i.e. -45 will draws text diagonally downwards (i.e. rotated 45 degrees clockwise).

Returns the index of the new layer, or -1 in case of error.


<FM>Example<FN>
// Output the filename as a new layer
ImageEnView1.LayersCreateFromText(ExtractFileName(ImageEnView1.IO.Params.Filename),
                                  'Tahoma', 32, clRed, [fsBold],
                                  True);
!!}
function TImageEnView.LayersCreateFromText(const sText : string;
                                           const sFontName : string;
                                           iFontSize : Integer;
                                           cFontColor : TColor;
                                           Style : TFontStyles;
                                           bAddShadow: boolean = False;
                                           iBlurRadius : Integer = 3;
                                           iShadowOffset : Integer = 2;
                                           Angle : Integer = 0;
                                           bAntiAlias : Boolean = true) : integer;
begin             
  // add a new layer
  Result := LayersAdd;

  // White fill the new layer
  Proc.Fill(CreateRGB(255, 255, 255));

  // Output our text
  Proc.TextOut(0, 0, sText, sFontName, iFontSize, cFontColor, Style, Angle, bAntiAlias);

  // Make the white background transparent
  Proc.SetTransparentColors(CreateRGB(255, 255, 255), CreateRGB(255, 255, 255), 0);

  // Add our shadow
  if bAddShadow then
    Proc.AddSoftShadow(iBlurRadius, iShadowOffset, iShadowOffset);

  // Remove the excess transparency
  Proc.CropTransparentBorder;
end;


{!!
<FS>TImageEnView.BackBuffer

<FM>Declaration<FC>
property BackBuffer: TBitmap;

<FM>Description<FN>
Provides access to the back buffer where ImageEn will draw the image (and layers) prior to the paint event.
You can draw on BackBuffer by handling the <A TImageEnView.OnDrawBackBuffer> event to paint onto the Backbuffer canvas.
BackBuffer is updated whenever <A TImageEnView.Update> is called.

<FM>Example<FC>
Procedure Form1OnDrawBackBuffer(Sender: TObject);
Begin
  With ImageEnView1.BackBuffer.Canvas do
  begin
    Pen.Color := clRed;
    MoveTo( 0, 0 );
    LineTo( 100, 100 );
  End;
End;

!!}
function TImageEnView.GetBackBuffer: TBitmap;
begin
  result := fTBitmap.VclBitmap;
end;

procedure TImageEnView.SetOnSaveUndo(value: TIESaveUndoEvent);
begin
  fOnSaveUndo := value;
  if assigned(fImageEnProc) then
    fImageEnProc.OnSaveUndo := value;
end;

{!!
<FS>TImageEnView.OnSaveUndo

<FM>Declaration<FC>
property OnSaveUndo: <A TIESaveUndoEvent>;

<FM>Description<FN>
OnSaveUndo occurs after <A TImageEnProc.SaveUndo> is called by user action or your code.
<FC>source<FN> is the same parameter used when calling <A TImageEnProc.SaveUndo>.

!!}
function TImageEnView.GetOnSaveUndo: TIESaveUndoEvent;
begin
  result := fOnSaveUndo;
end;

{!!
<FS>TImageEnView.LayersFixSizes

<FM>Declaration<FC>
procedure LayersFixSizes(layer: Integer = -1);

<FM>Description<FN>
Resamples the bitmap of each layer (if layer = -1) or a specified layer (if a valid index is passed) to match the actual layer size.
!!}
// layer = -1 : all layers
procedure TImageEnView.LayersFixSizes(layer: Integer);
var
  lyr: TIELayer;
  i, l: Integer;
  rf: TResampleFilter;
  tempproc: TImageEnProc;
begin
  tempproc := TImageEnProc.Create(nil);
  try
    tempproc.AutoUndo := false;
    l := fLayersCurrent;
    for i := 0 to fLayers.Count-1 do
    begin
      lyr := TIELayer(fLayers[i]);
      if ((lyr.ResizedWidth<>0) or (lyr.ResizedHeight<>0)) and ((layer = -1) or (i=layer)) then
      begin
        LayersCurrent := i;
        if lyr.UseResampleFilter then
          rf := lyr.ResampleFilter
        else
          rf := fZoomFilter;
        tempproc.AttachedIEBitmap := lyr.Bitmap;
        tempproc.Resample(lyr.Width, lyr.Height, rf);
        lyr.ResizedWidth  := 0;
        lyr.ResizedHeight := 0;
      end;
    end;
    LayersCurrent := l;
  finally
    tempproc.Free();
  end;
end;

{!!
<FS>TImageEnView.LayersFixBorders

<FM>Declaration<FC>
procedure LayersFixBorders(layer: Integer = -1);

<FM>Description<FN>
Removes the transparent border around the bitmap, which typically occurs after multiple rotations.
Pass a valid layer index, or -1 to fix all layers
!!}
procedure TImageEnView.LayersFixBorders(layer: Integer = -1);
var
  lyr: TIELayer;
  i: Integer;
  p: TImageEnProc;
begin
  p := TImageEnProc.Create(nil);
  try
    p.AutoUndo := false;
    for i := 0 to fLayers.Count-1 do
    begin
      lyr := TIELayer(fLayers[i]);
      if ((layer = -1) or (i = layer)) then
      begin
        p.AttachedIEBitmap := lyr.Bitmap;
        p.CropTransparentBorder();
      end;
    end;
  finally
    p.Free();
  end;
end;

{!!
<FS>TImageEnView.LayersFixRotations

<FM>Declaration<FC>
procedure LayersFixRotations(layer: Integer = -1);

<FM>Description<FN>
Rotates the bitmap of each layer (if layer = -1) or a specified layer to its actual rotation angle.

Note: This method will also call <A TImageEnView.LayersFixSizes>.
!!}
// layer = -1 : all layers
procedure TImageEnView.LayersFixRotations(layer: Integer);
var
  lyr: TIELayer;
  i, l: Integer;
  pts: array [0..3] of TPoint;
  lposx, lposy, lresw, lresh: Integer;
  r: TRect;
  tempproc: TImageEnProc;
begin
  tempproc := TImageEnProc.Create(nil);
  try
    tempproc.AutoUndo := false;
    tempproc.Background := fBackground;
    l := fLayersCurrent;
    for i := 0 to fLayers.Count-1 do
    begin
      lyr := TIELayer(fLayers[i]);
      if (lyr.Rotate <> 0) and ((layer = -1) or (i = layer)) then
      begin

        LayersFixSizes(i);

        LayersCurrent := i;

        lresw := lyr.Width;
        lresh := lyr.Height;
        pts[0].X := 0;
        pts[0].Y := 0;
        pts[1].X := lyr.Width;
        pts[1].Y := 0;
        pts[2].X := lyr.Width;
        pts[2].Y := lyr.height;
        pts[3].X := 0;
        pts[3].Y := lyr.Height;
        lposx := lyr.PosX;
        lposy := lyr.PosY;
        lyr.Bitmap.AlphaChannel;  // creates alpha channel
        tempproc.AttachedIEBitmap := lyr.Bitmap;
        if fLayersRotationAntialias then
          tempproc.Rotate(lyr.Rotate, fLayersRotationFilter, -1)
        else                                   
          tempproc.Rotate(lyr.Rotate, ierNone, -1);
        IERotatePoints(pts, lyr.Rotate, trunc(lyr.RotateCenterX*lresw), trunc(lyr.RotateCenterY*lresh));
        lyr.PosX := lposx+imin(imin(imin(pts[0].X, pts[1].X), pts[2].X), pts[3].X);
        lyr.PosY := lposy+imin(imin(imin(pts[0].Y, pts[1].Y), pts[2].Y), pts[3].Y);

        lyr.ResizedWidth  := 0;
        lyr.ResizedHeight := 0;

        lyr.Rotate := 0;

        if i = 0 then
        begin
          lyr.PosX := lposx;
          lyr.PosY := lposy;
        end
        else
        begin
          r := IEGetVisibleArea(fIEBitmap, nil, nil);
          tempproc.Crop(r);
          lyr.PosX := lyr.PosX + r.Left;
          lyr.PosY := lyr.posY + r.Top;
        end;

      end;
    end;
    LayersCurrent := l;
  finally
    tempproc.Free();
  end;
end;

{!!
<FS>TImageEnView.SoftCrop

<FM>Declaration<FC>
property SoftCrop: <A TIESoftCropMode>;

<FM>Description<FN>
Specifies the operation to perform when a layer is outside of the background layer (layer 0). I.e. to provide a visual indication to the user.

Portion of layers that are outside the background layer are shown as:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C>iesfNone</C> <C>Normal</C> </R>
<R> <C>iesfAlphaBlend</C> <C>Partially transparent (to the level specified by <A TImageEnView.SoftCropValue>)</C> </R>
<R> <C>iesfGrid</C> <C>Grayed (Grid matrix pattern)</C> </R>
<R> <C>iesfAdd</C> <C>Color shifted/washed out (to the level specified by <A TImageEnView.SoftCropValue>)</C> </R>
</TABLE>


!!}
procedure TImageEnView.SetSoftCrop(v: TIESoftCropMode);
begin
  fSoftCrop := v;
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;

{!!
<FS>TImageEnView.SoftCropValue

<FM>Declaration<FC>
property SoftCropValue: Integer;

<FM>Description<FN>
If <A TImageEnView.SoftCrop> is iesfAlphaBlend then the SoftCropValue specifies the level of transparency from 0 (minimal) to 255 (fully)
If <A TImageEnView.SoftCrop> is iesfAdd then the SoftCropValue specifies the amount of color shift from 0 (minimal) to 255 (extreme)
!!}
procedure TImageEnView.SetSoftCropValue(v: Integer);
begin
  fSoftCropValue := v;
  UpdateReason := ieurComponentStuffChanged;
  Update;
end;


{!!
<FS>TImageEnView.SetInteractionHint

<FM>Declaration<FC>
procedure SetInteractionHint(const Text: String; x, y: Integer; const minText: String);

<FM>Description<FN>
Specify a text hint to appear over the ImageEnView (which will be displayed at the next painting event).

<TABLE>
<R> <H>Parameter</H> <H>Description</H> </R>
<R> <C><FC>Text<FN></C> <C>Text to draw</C> </R>
<R> <C><FC>x<FN></C> <C>Horizontal position</C> </R>
<R> <C><FC>y<FN></C> <C>Vertical position</C> </R>
<R> <C><FC>minText<FN></C> <C>Example (dummy) text to calculate the minimum text size</C> </R>
</TABLE>

<FM>Example<FC>
// display current position in pixels
procedure TForm1.ImageEnVect1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  xx, yy: integer;
begin
  xx := ImageEnView1.XScr2Bmp(X);
  yy := ImageEnView1.yscr2bmp(Y);
  if (xx >= 0) and (xx < ImageEnView1.IEBitmap.Width) and (yy >= 0) and (yy < ImageEnView1.IEBitmap.Height) then
  begin
    ImageEnView1.SetInteractionHint(Format('%d %d', [xx, yy]), X, Y, '0000 0000');
    ImageEnView1.Paint();
  end;
end;
!!}
procedure TImageEnView.SetInteractionHint(const Text: String; x, y: Integer; const minText: String);
begin
  fInteractionHint := Text;
  fInteractionHintX := x;
  fInteractionHintY := y;
  fInteractionHintMinText := minText;
end;


{!!
<FS>TImageEnView.HighlightedPixel

<FM>Declaration<FC>
property HighlightedPixel: TPoint;

<FM>Description<FN>
Specify a pixel within your image and it will be highlighted by a colored box (to draw attention to it).

Set it to (-1, -1) to disable.

<FM>Demo<FN>
Display\ImageComp
!!}
procedure TImageEnView.SetHighlightedPixel(v: TPoint);
var
  x1, y1, x2, y2: Integer;
begin
  if (fHighlightedPixel.X > -1) and (fHighlightedPixel.Y > -1) then
  begin
    x1 := CurrentLayer.ConvXBmp2Scr(fHighlightedPixel.X);
    y1 := CurrentLayer.ConvYBmp2Scr(fHighlightedPixel.Y);
    x2 := CurrentLayer.ConvXBmp2Scr(fHighlightedPixel.X+1);
    y2 := CurrentLayer.ConvYBmp2Scr(fHighlightedPixel.Y+1);

    fHighlightedPixel := Point(-1, -1);
    UpdateRect(Rect(x1-5, y1-5, x2+5, y2+5));
  end;
  fHighlightedPixel := v;
  Paint;
end;

{!!
<FS>TImageEnView.MoveContentTo

<FM>Declaration<FC>
procedure TImageEnView.MoveContentTo(Destination: <A TImageEnView>);

<FM>Description<FN>
Transfers the current image, all layers and input/output parameters to the destination TImageEnView component.

Note:
- This method doesn't copy the images, but transfers pointers to the image buffer
- After the operation the source component will be empty, and all previous images will be removed from "Destination"
- To "Copy" the content to another TImageEnView use the <A TImageEnView.Assign> method

!!}
procedure TImageEnView.MoveContentTo(Destination: TImageEnView);
var
  src_oldnav, dst_oldnav: TImageEnView;
  i: integer;
begin
  if Destination = self then
    exit;

  // disable navigator
  src_oldnav := fNavigator;
  SetNavigator(nil);
  dst_oldnav := Destination.fNavigator;
  Destination.SetNavigator(nil);

  // assign IO params
  Destination.IO.Params.Assign( IO.Params );

  // free destination images
  for i := 0 to Destination.fLayers.Count - 1 do
    TIELayer(Destination.fLayers[i]).Free;
  FreeAndNil(Destination.fLayers);

  // copy pointers to destination
  Destination.fLayers := fLayers;
  Destination.fIEBitmap := fIEBitmap;
  Destination.fBitmap := fBitmap;
  Destination.fLayersCurrent := fLayersCurrent;

  // change destination layers owner
  for i := 0 to Destination.fLayers.Count-1 do
    TIELayer(Destination.fLayers[i]).fOwner := Destination;

  // create empty images in source
  fIEBitmap := TIEBitmap.Create;
  if fLegacyBitmap then
  begin
    fBitmap := TBitmap.create;
    fBitmap.pixelformat := pf24bit;
    fIEBitmap.EncapsulateTBitmap(fBitmap, true);
  end;
  fLayers := TList.Create;
  fLayers.Add(TIELayer.Create(self, fIEBitmap, true));
  with TIELayer(fLayers[0]) do
  begin
    VisibleBox := false;
    Locked := true;
  end;
  fLayersCurrent := 0;

  Destination.CallBitmapChangeEvents;
  CallBitmapChangeEvents;

  Destination.Update;
  Update;

  // enable navigators
  SetNavigator(src_oldnav);
  Destination.SetNavigator(dst_oldnav);
end;


////////////////////////////////////////////////////////////////////////////////////////////////////////
// Smooth scroll

{!!
<FS>TImageEnView.SetViewXYSmooth

<FM>Declaration<FC>
procedure SetViewXYSmooth(x, y: Integer);

<FM>Description<FN>
Repositions the image view (i.e. by setting <A TImageEnView.ViewX> and <A TImageEnView.ViewY>), but does so with smooth scrolling (image will slowly pan to the new location).
To control the smoothness set <A TImageEnView.SmoothScrollValue>.

<FM>See Also<FN>
- <A TImageEnView.OnFinishSmoothTask>
- <A TImageEnView.SmoothScrollValue>
!!}
procedure TImageEnView.SetViewXYSmooth(x, y: Integer);
begin
  if fSmoothScrollValue = 0 then
    SetViewXY(x, y)
  else
  begin
    fSmoothScrollDestX := x;
    fSmoothScrollDestY := y;
    if not assigned(fSmoothScrollTimer) then
    begin
      fSmoothScrollTimer := TTimer.Create(self);
      fSmoothScrollTimer.OnTimer := OnSmoothSetView;
      fSmoothScrollTimer.Interval := 30;
      fSmoothScrollTimer.Enabled := true;
      OnSmoothSetView(self);  // first call done manually
    end;
  end;
end;

{!!
<FS>TImageEnView.SmoothScrollValue

<FM>Declaration<FC>
property SmoothScrollValue: Integer;

<FM>Description<FN>
Specifies the smoothness (speed) of scrolling when <A TImageEnView.SetViewXYSmooth> is called or <A TImageEnView.MouseInteract> contains miMovingScroll.
A large values increases the smoothness. "0" disables smoothing (i.e. so that it acts like <A TImageEnView.SetViewXY>).
Default: 8.
!!}
procedure TImageEnView.SetSmoothScrollValue(v: Integer);
begin
  fSmoothScrollValue := v;
end;

procedure TImageEnView.OnSmoothSetView(Sender: TObject);
var
  x, y: Integer;
  lx, ly: Integer;
begin
  if assigned(fSmoothScrollTimer) then
  begin
    lx := fViewX;
    ly := fViewY;
    x := round(fViewX - (fViewX - fSmoothScrollDestX) / fSmoothScrollValue);
    y := round(fViewY - (fViewY - fSmoothScrollDestY) / fSmoothScrollValue);

    SetViewXY(x, y);

    if ((fViewX = lx) and (fViewY = ly)) or IEIsLeftMouseButtonPressed then
      StopSmoothScroll();
  end;
end;

procedure TImageEnView.StopSmoothScroll();
begin
  FreeAndNil(fSmoothScrollTimer);
  if assigned(fOnFinishSmoothTask) then
    OnFinishSmoothTask(self, iestScroll);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////
// Smooth zoom

{!!
<FS>TImageEnView.SetZoomSmooth

<FM>Declaration<FC>
procedure SetZoomSmooth(Zoom: Double);

<FM>Description<FN>
Sets the image zoom (i.e. by setting <A TImageEnView.Zoom>), but does so with smooth zoom (image will slowly zoom to the new value).
To control the smoothness set <A TImageEnView.SmoothZoomValue>.

<FM>See Also<FN>
- <A TImageEnView.OnFinishSmoothTask>
- <A TImageEnView.SmoothZoomValue>
!!}
procedure TImageEnView.SetZoomSmooth(Zoom: Double);
begin
  if fSmoothZoomValue = 0 then
    self.SetZoom(Zoom)
  else
  begin
    fSmoothZoomDest := Zoom;
    if not assigned(fSmoothZoomTimer) then
    begin
      fSmoothZoomTimer := TTimer.Create(self);
      fSmoothZoomTimer.OnTimer := OnSmoothZoom;
      fSmoothZoomTimer.Interval := 30;
      fSmoothZoomTimer.Enabled := true;
      OnSmoothZoom(self);  // first call done manually
    end;
  end;
end;

{!!
<FS>TImageEnView.SmoothZoomValue

<FM>Declaration<FC>
property SmoothZoomValue: Integer;

<FM>Description<FN>
Specifies the smoothness (speed) of zooming when <A TImageEnView.SetZoomSmooth> is called.
A large values increases the smoothness. "0" disables smoothing (i.e. so that it acts like <A TImageEnView.Zoom>).
Default: 8.
!!}
procedure TImageEnView.SetSmoothZoomValue(v: Integer);
begin
  fSmoothZoomValue := v;
end;

procedure TImageEnView.OnSmoothZoom(Sender: TObject);
var
  delta: double;
begin
  if assigned(fSmoothZoomTimer) then
  begin
    delta := (Zoom - fSmoothZoomDest) / fSmoothZoomValue;
    SetZoom(Zoom - delta);

    if (abs(delta) <= (1 / fSmoothZoomValue)) then
    begin
      SetZoom(fSmoothZoomDest);
      StopSmoothZoom();
    end
    else
    if IEIsLeftMouseButtonPressed then
      StopSmoothZoom();
  end;
end;

procedure TImageEnView.StopSmoothZoom();
begin
  FreeAndNil(fSmoothZoomTimer);
  if assigned(fOnFinishSmoothTask) then
    OnFinishSmoothTask(self, iestZoom);
end;



////////////////////////////////////////////////////////////////////////////////////////////////////////

procedure IEInitialize_imageenview;
begin
  InitImageEnView;
end;

procedure IEFinalize_imageenview;
begin
  if assigned(IEGlobalSettings().GridPen) then
  begin
    IEGlobalSettings().GridPen.Free();
    IEGlobalSettings().GridPen := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
// PostFrames support

type
TIEPostFramesTarget = class
public
  FSource: TImageEnView;
  FTarget: TImageEnView;
  FDelay: integer;    // in ms
  FInterval: integer; // in ms
  FTimer: TTimer;
  FBuffers: TList;    // a list of TIEBitmap objects (Delay/Interval+1)
  FElapsed: integer;  // in ms
  FNextToRead: integer;
  FNextToWrite: integer;
  constructor Create(source, target: TImageEnView; delay: Integer; interval: Integer);
  destructor Destroy; override;
private
  procedure PostFrames(Sender: TObject);
end;

constructor TIEPostFramesTarget.Create(source, target: TImageEnView; delay: Integer; interval: Integer);
var
  i, c: Integer;
begin
  inherited Create;
  FSource   := source;
  FTarget   := target;
  FDelay    := delay;
  FInterval := interval;
  FTimer    := TTimer.Create(nil);
  FTimer.OnTimer := PostFrames;
  FBuffers  := TList.Create;
  c := delay div interval +2;
  for i := 0 to c-1 do
    FBuffers.Add( TIEBitmap.Create );
  FElapsed := 0;
  FTimer.Interval := interval;
  FTimer.Enabled := true;
  FNextToRead  := 0;
  FNextToWrite := 0;
end;

destructor TIEPostFramesTarget.Destroy;
var
  i: Integer;
begin
  FTimer.Free;
  for i := 0 to FBuffers.Count-1 do
    TIEBitmap(FBuffers[i]).Free;
  FBuffers.Free;
  inherited;
end;

procedure TIEPostFramesTarget.PostFrames(Sender: TObject);
begin
  inc(FElapsed, FInterval);
  if FElapsed > FDelay then
  begin
    // move first frame to target viewer
    FTarget.IEBitmap.Assign( TIEBitmap(FBuffers[FNextToRead]) );
    FTarget.Update;
    inc(FNextToRead);
    if FNextToRead = FBuffers.Count-1 then
      FNextToRead := 0;
  end;
  TIEBitmap(FBuffers[FNextToWrite]).Assign( FSource.IEBitmap );
  inc(FNextToWrite);
  if FNextToWrite = FBuffers.Count-1 then
    FNextToWrite := 0;
end;

{!!
<FS>TImageEnView.BeginPostFrames

<FM>Declaration<FC>
procedure BeginPostFrames(target: TImageEnView; delay: Integer; interval: Integer);

<FM>Description<FN>
Sends the current image to a target <A TImageEnView>, after "delay" milliseconds and then after each "interval" milliseconds.
This is useful to display captured frames to another control with delay (See the VideoCapture\DirectShow5 demo).
You can (but don't need to) end frame sending by calling <A TImageEnView.EndPostFrames>.

Multiple calls to BeginPostFrames are possible.

<FM>Example<FC>
// send the current image to ImageEnView2 after 5 seconds, and to ImageEnView3 after 10 seconds, at 50 ms for each frame
ImageEnView1.BeginPostFrames(ImageEnView2, 5000, 50);
ImageEnView1.BeginPostFrames(ImageEnView3, 10000, 50);
!!}
procedure TImageEnView.BeginPostFrames(target: TImageEnView; delay: Integer; interval: Integer);
begin
  // removes if already exists
  EndPostFrames( target );
  // add new
  fPostFrames.Add( TIEPostFramesTarget.Create(self, target, delay, interval) );
end;

procedure TImageEnView.RemovePostFrames(index: Integer);
begin
  if assigned(fPostFrames) and (index>-1) then
  begin
    TIEPostFramesTarget(fPostFrames[index]).Free;
    fPostFrames.Delete(index);
  end;
end;

{!!
<FS>TImageEnView.EndPostFrames

<FM>Declaration<FC>
procedure EndPostFrames(target: TImageEnView);

<FM>Description<FN>
Stops sending of the current image to a target <A TImageEnView> that has been commenced using <A TImageEnView.BeginPostFrames>.

<FM>Example<FC>
ImageEnView1.EndPostFrames(ImageEnView2);
ImageEnView1.EndPostFrames(ImageEnView3);
!!}
procedure TImageEnView.EndPostFrames(target: TImageEnView);
var
  i: Integer;
begin
  for i := 0 to fPostFrames.Count-1 do
    if TIEPostFramesTarget(fPostFrames[i]).FTarget = target then
    begin
      RemovePostFrames( i );
      break;
    end;
end;



// End of PostFrames support
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////


{!!
<FS>TImageEnView.LayersCopyToAlpha

<FM>Declaration<FC>
procedure LayersCopyToAlpha(DestLayer: Integer);

<FM>Description<FN>
Copies the current layer to the alpha channel of the specified destination layer.
This is useful to handle alpha channel of other bitmaps, applying the same image processing algorithms.

See also: <A TImageEnView.LayersCreateFromAlpha>

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\image.jpg');
ImageEnView1.LayersCreateFromAlpha;
ImageEnView1.Proc.BumpMapping( 300, 300, 150, 150, 0, CreateRgb(255, 255, 255));
ImageEnView1.LayersCopyToAlpha(0);
ImageEnView1.LayersRemove(1);
!!}
procedure TImageEnView.LayersCopyToAlpha(DestLayer: Integer);
var
  dst_w, dst_h: Integer;
begin
  dst_w := Layers[DestLayer].Bitmap.Width;
  dst_h := Layers[DestLayer].Bitmap.Height;
  with Layers[DestLayer].Bitmap.AlphaChannel do
  begin
    AssignImage( fIEBitmap );
    Resize(dst_w, dst_h);
    PixelFormat := ie8g;
  end;
  Update;
end;


{!!
<FS>TImageEnView.LayersCreateFromAlpha

<FM>Declaration<FC>
function LayersCreateFromAlpha: Integer;

<FM>Description<FN>
Creates a new layer with the content of current bitmap's alpha channel.
This is useful to handle alpha channel of other bitmaps, applying the same image processing algorithms.

See also: <A TImageEnView.LayersCopyToAlpha>

<FM>Example<FC>
ImageEnView1.IO.LoadFromFile('C:\image.jpg');
ImageEnView1.LayersCreateFromAlpha;
ImageEnView1.Proc.BumpMapping( 300, 300, 150, 150, 0, CreateRgb(255, 255, 255));
ImageEnView1.LayersCopyToAlpha(0);
ImageEnView1.LayersRemove(1);
!!}
function TImageEnView.LayersCreateFromAlpha: Integer;
begin
  result := LayersAdd( fIEBitmap.AlphaChannel );
end;


{!!
<FS>TImageEnView.SetSelectionMarkOuterStyle

<FM>Declaration<FC>
procedure SetSelectionMarkOuterStyle(Alpha: Integer; Color: TColor);

<FM>Description<FN>
When <A TImageEnView.SelectionOptions> is <FC>iesoMarkOuter<FN>, this method specifies how the outer area will be drawn.
If Alpha = -1 (default) a gray grid is displayed.
If Alpha >= 0 and <= 255 a solid color is painted.

<FM>Example<FC>
// Areas outside the selection will be marked with red (with 50% transparency)
ImageEnView1.SelectionOptions := ImageEnView1.SelectionOptions+[iesoMarkOuter];
ImageEnView1.SetSelectionMarkOuterStyle(128, clRed);
!!}
procedure TImageEnView.SetSelectionMarkOuterStyle(Alpha: Integer; Color: TColor);
begin
  fMarkOuterAlpha := Alpha;
  fMarkOuterColor := Color;
end;


{!!
<FS>TImageEnView.SaveState

<FM>Declaration<FC>
procedure SaveState(const FileName: String);
procedure SaveState(Stream: TStream);

<FM>Description<FN>
Saves layers, selection and some other parameters such as Zoom and Scroll position.

See also: <A TImageEnView.LoadState> method.
!!}
procedure TImageEnView.SaveState(const FileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveState(fs);
  finally
    fs.Free;
  end;
end;

procedure TImageEnView.SaveState(Stream: TStream);
var
  ver: Integer;
  vi: Integer;
begin
  // write magic string
  Stream.Write(IEIMAGEENVIEWSTATE[1], length(IEIMAGEENVIEWSTATE));

  // write version
  ver := 2;
  Stream.Write(ver, sizeof(integer));

  // selection
  SaveSelectionToStream(Stream);

  // view and zoom
  Stream.Write(fZoomX, sizeof(fZoomX));
  Stream.Write(fZoomY, sizeof(fZoomY));
  Stream.Write(fViewX, sizeof(fViewX));
  Stream.Write(fViewY, sizeof(fViewY));

  // others
  Stream.Write(fLegacyBitmap, sizeof(boolean));
  Stream.Write(fImageHorizAlignment, sizeof(TIEHAlign));
  Stream.Write(fImageVertAlignment, sizeof(TIEVAlign));
  Stream.Write(fZoomFilter, sizeof(TResampleFilter));
  Stream.Write(fBackgroundStyle, sizeof(TIEBackgroundStyle));
  Stream.Write(fDisplayGrid, sizeof(boolean));
  Stream.Write(fAutoFit, sizeof(boolean));
  Stream.Write(fAutoStretch, sizeof(boolean));
  Stream.Write(fAutoShrink, sizeof(boolean));
  Stream.Write(fSelectionOptions, sizeof(TIESelectionOptions));
  Stream.Write(fMouseInteract, sizeof(TIEMouseInteract));
  Stream.Write(fScrollBarsAlwaysVisible, sizeof(boolean));
  Stream.Write(fEnableAlphaChannel, sizeof(boolean));
  Stream.Write(fScrollBars, sizeof(TScrollStyle));

  // for debugging
  vi := ClientWidth;  Stream.Write(vi, sizeof(integer));
  vi := ClientHeight; Stream.Write(vi, sizeof(integer));
  vi := Width;        Stream.Write(vi, sizeof(integer));
  vi := Height;       Stream.Write(vi, sizeof(integer));

  // LAST: layers
  SaveSelection;
  LayersSaveToStream(Stream);
  RestoreSelection;
end;


{!!
<FS>TImageEnView.LoadState

<FM>Declaration<FC>
procedure LoadState(const FileName: String);
procedure LoadState(Stream: TStream);

<FM>Description<FN>
Loads layers, selection and some other parameters such as Zoom and Scroll position.

See also: <A TImageEnView.SaveState> method.
!!}
procedure TImageEnView.LoadState(const FileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadState(fs);
  finally
    fs.Free;
  end;
end;

procedure TImageEnView.LoadState(Stream: TStream);
var
  ver: Integer;
  magic: AnsiString;
  r_ViewX, r_ViewY: Integer;
  r_ZoomX, r_ZoomY: Double;
  r_LegacyBitmap, r_Center: Boolean;
  r_ZoomFilter: TResampleFilter;
  r_BackgroundStyle: TIEBackgroundStyle;
  r_DisplayGrid: Boolean;
  r_ClientWidth, r_ClientHeight, r_Width, r_Height: Integer;
  r_AutoFit, r_AutoStretch, r_AutoShrink: Boolean;
  r_SelectionOptions: TIESelectionOptions;
  r_MouseInteract: TIEMouseInteract;
  r_ScrollBarsAlwaysVisible, r_EnableAlphaChannel: Boolean;
  r_ScrollBars: TScrollStyle;
  r_ImageHorizAlignment: TIEHAlign;
  r_ImageVertAlignment: TIEVAlign;
begin
  // read magic string
  SetLength(magic, length(IEIMAGEENVIEWSTATE));
  Stream.Read(magic[1], length(IEIMAGEENVIEWSTATE));
  // read version
  Stream.Read(ver, sizeof(integer));

  if (magic = ieIMAGEENVIEWSTATE) and (ver>0) then
  begin
    // selection
    LoadSelectionFromStream(Stream);
    SaveSelection;

    // view and zoom
    Stream.Read(r_ZoomX, sizeof(r_ZoomX));
    Stream.Read(r_ZoomY, sizeof(r_ZoomY));
    Stream.Read(r_ViewX, sizeof(r_ViewX));
    Stream.Read(r_ViewY, sizeof(r_ViewY));

    // others
    Stream.Read(r_LegacyBitmap, sizeof(boolean));
    if ver >= 2 then
    begin
      Stream.Read(r_ImageHorizAlignment, sizeof(TIEHAlign));
      Stream.Read(r_ImageVertAlignment, sizeof(TIEVAlign));
    end
    else
    begin
      Stream.Read(r_Center, sizeof(boolean));
      if r_Center then
      begin
        r_ImageHorizAlignment := iehCenter;
        r_ImageVertAlignment  := ievCenter;
      end
      else
      begin
        r_ImageHorizAlignment := iehLeft;
        r_ImageVertAlignment  := ievTop;
      end;
    end;
    Stream.Read(r_ZoomFilter, sizeof(TResampleFilter));
    Stream.Read(r_BackgroundStyle, sizeof(TIEBackgroundStyle));
    Stream.Read(r_DisplayGrid, sizeof(boolean));
    Stream.Read(r_AutoFit, sizeof(boolean));
    Stream.Read(r_AutoStretch, sizeof(boolean));
    Stream.Read(r_AutoShrink, sizeof(boolean));
    Stream.Read(r_SelectionOptions, sizeof(TIESelectionOptions));
    Stream.Read(r_MouseInteract, sizeof(TIEMouseInteract));
    Stream.Read(r_ScrollBarsAlwaysVisible, sizeof(boolean));
    Stream.Read(r_EnableAlphaChannel, sizeof(boolean));
    Stream.Read(r_ScrollBars, sizeof(TScrollStyle));

    // for debugging
    Stream.Read(r_ClientWidth, sizeof(integer));
    Stream.Read(r_ClientHeight, sizeof(integer));
    Stream.Read(r_Width, sizeof(integer));
    Stream.Read(r_Height, sizeof(integer));

    // LAST: layers
    LayersLoadFromStream(Stream);

    // Settings
    LockPaint;
    LegacyBitmap := r_LegacyBitmap;
    ImageHorizAlignment := r_ImageHorizAlignment;
    ImageVertAlignment  := r_ImageVertAlignment;
    ZoomX := r_ZoomX;
    ZoomY := r_ZoomY;
    ViewX := r_ViewX;
    ViewY := r_ViewY;
    ZoomFilter := r_ZoomFilter;
    BackgroundStyle := r_BackgroundStyle;
    DisplayGrid := r_DisplayGrid;
    AutoFit := r_AutoFit;
    AutoStretch := r_AutoStretch;
    AutoShrink := r_AutoShrink;
    SelectionOptions := r_SelectionOptions;
    MouseInteract := r_MouseInteract;
    ScrollBarsAlwaysVisible := r_ScrollBarsAlwaysVisible;
    EnableAlphaChannel := r_EnableAlphaChannel;
    ScrollBars := r_ScrollBars;

    RestoreSelection;

    UnLockPaint;
  end;
end;


{!!
<FS>TImageEnView.ResetState

<FM>Declaration<FC>
procedure ResetState();

<FM>Description<FN>
Resets some TImageEnView properties to their defaults.

ResetState will remove selection, empty layers, set input/output parameters to their defaults, reset Zoom and Scroll and clear the Undo buffer.

Leaves <A TImageEnView.LegacyBitmap> unchanged.
!!}
procedure TImageEnView.ResetState();
begin
  Playing := False;
  DeSelect();
  LayersClear();
  Blank();
  IO.Params.SetDefaultParams();
  Zoom := 100.0;
  ViewX := 0;
  ViewY := 0;
  ImageHorizAlignment := iehCenter;
  ImageVertAlignment  := ievCenter;
  Proc.ClearAllUndo();
  Proc.ClearAllRedo();
  if LegacyBitmap = false then
    IEBitmap.Location := ieMemory;
  Update();
end;


// this could not fired if the component is on TForm. Putting TImageEnView on TPanel it works.
procedure TImageEnView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
var
  handled: Boolean;
begin
  inherited;
  handled := false;
  if assigned(fOnSpecialKey) then
    fOnSpecialKey(self, Msg.CharCode, KeyDataToShiftState(Msg.KeyData), handled);
  if not handled then
  begin
    {$IFDEF OCXVERSION}
    case msg.CharCode of
      VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END:
        begin
          msg.Result := 1;
          KeyUp(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
        end;
    end;
    {$ENDIF}
  end
  else
    msg.Result := 1;  // handled
end;


procedure TImageEnView.CNKEYDOWN(var Message: TMessage);
begin
  inherited;
  if assigned(fOnVirtualKey) then
    fOnVirtualKey(self, Message.wParam, Message.lParam, true);
end;

procedure TImageEnView.CNKEYUP(var Message: TMessage);
begin
  inherited;
  if assigned(fOnVirtualKey) then
    fOnVirtualKey(self, Message.wParam, Message.lParam, false);
end;

procedure TImageEnView.WMSYSKEYDOWN(var Message: TMessage);
begin
  inherited;
  if assigned(fOnVirtualKey) then
    fOnVirtualKey(self, Message.wParam, Message.lParam, true);
end;

procedure TImageEnView.WMSYSKEYUP(var Message: TMessage);
begin
  inherited;
  if assigned(fOnVirtualKey) then
    fOnVirtualKey(self, Message.wParam, Message.lParam, false);
end;


{!!
<FS>TImageEnView.HighlightedPixelColor
 
<FM>Declaration<FC>
property HighlightedPixelColor: TColor;
 
<FM>Description<FN>
Colored box color.

<FM>See Also<FN>
- <A TImageEnView.HighlightedPixel>
!!}
procedure TImageEnView.SetHighlightedPixelColor(c: TColor);
begin
  fHighlightedPixelColor := c;
end;


procedure TImageEnView.WMGestureNotify(var Msg: TIEWMGestureNotify);
var
  c: integer;
  gc: array of TIEGESTURECONFIG;
begin
  inherited;
  if fGestures.Enabled and IEHasGestures() then
  begin
    IEUnregisterTouchWindow(Handle);
    if fGestures.fPan.Enabled or fGestures.fLayerMove.Enabled then
    begin
      c := length(gc);
      SetLength(gc, c + 1);
      gc[c].dwID    := IEGID_PAN;
      gc[c].dwWant  := IEGC_PAN or IEGC_PAN_WITH_SINGLE_FINGER_VERTICALLY or IEGC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY or IEGC_PAN_WITH_INERTIA;
      gc[c].dwBlock := IEGC_PAN_WITH_GUTTER;
    end;
    if fGestures.fZoom.Enabled then
    begin
      c := length(gc);
      SetLength(gc, c + 1);
      gc[c].dwID    := IEGID_ZOOM;
      gc[c].dwWant  := IEGC_ALLGESTURES;
      gc[c].dwBlock := 0;
    end;
    if fGestures.fLayerRotate.Enabled then
    begin
      c := length(gc);
      SetLength(gc, c + 1);
      gc[c].dwID    := IEGID_ROTATE;
      gc[c].dwWant  := IEGC_ROTATE;
      gc[c].dwBlock := 0;
    end;
    IESetGestureConfig(Handle, 0, length(gc), @gc[0], sizeof(TIEGESTURECONFIG));
  end;
end;


function GID_ROTATE_ANGLE_FROM_ARGUMENT(arg: integer): double;
begin
  result := ((arg / 65535.0) * 4.0 * 3.14159265) - 2.0 * 3.14159265;
end;


procedure TImageEnView.DoImageEnGestureEvent(const GInfo: TIEGESTUREINFO; var Handled: boolean);
var
  Flags: TIEGestureFlags;
  ID: TIEGestureID;
begin
  if assigned(fOnImageEnGesture) then
  begin
    Flags := [];
    if (IEGF_BEGIN and GInfo.dwFlags) <> 0 then
      Flags := Flags + [iegfBegin];
    if (IEGF_INERTIA and GInfo.dwFlags) <> 0 then
      Flags := Flags + [iegfInertia];
    if (IEGF_END and GInfo.dwFlags) <> 0 then
      Flags := Flags + [iegfEnd];
    ID := TIEGestureID(GInfo.dwID - 1);
    fOnImageEnGesture(self, Flags, ID, GInfo.ptsLocation,
                      GInfo.ullArguments and $FFFFFFFF,
                      (GInfo.ullArguments shr 32) and $FFFF,
                      (GInfo.ullArguments shr 48) and $FFFF,
                      (GInfo.ullArguments shr 32),
                      Handled);
  end;
end;


function TImageEnView.PerformRotateSnap(Value: double): double;
var
  i: integer;
begin
  result := Value;
  if fGestures.fLayerRotate.SnapValues then
    for i := 0 to 8 do
      if result >= 0 then
      begin
        if (abs(45 * i - IEGetReferenceAngle(result)) <= fGestures.fLayerRotate.SnapDelta) then
          result := 45 * i;
      end
      else
      begin
        if (abs(-45 * i - IEGetReferenceAngle(result)) <= fGestures.fLayerRotate.SnapDelta) then
          result := - 45 * i;
      end;
end;


function TImageEnView.PerformLayerMoveSnap(Value: double): double;
begin
  result := Value;
  if fGestures.fLayerMove.SnapValues then
  begin
    if abs(result) <= fGestures.fLayerMove.SnapDelta then
      result := 0.0
  end;
end;


function TImageEnView.PerformZoomSnap(Value: double): double;
begin
  result := Value;
  if fGestures.fZoom.SnapValues then
  begin
    if abs(result - 100.0) <= fGestures.fZoom.SnapDelta then
      result := 100;
  end;
end;


procedure TImageEnView.WMGesture(var Msg: TMessage);
var
  gInfo: TIEGESTUREINFO;
  value: integer;
  v: double;
  isBegin: boolean;
  isInertia: boolean;
  handled: boolean;
begin
  Msg.Result := 1;  // not handled
  if fGestures.Enabled and IEHasGestures() then
  begin
    FillChar(gInfo, sizeof(gInfo), 0);
    gInfo.cbSize := sizeof(gInfo);
    if IEGetGestureInfo(Msg.LParam, @gInfo) then
    begin
      try
        handled := false;
        DoImageEnGestureEvent(gInfo, handled);

        if not handled then
        begin

          value     := gInfo.ullArguments and $FFFFFFFF;
          isBegin   := (IEGF_BEGIN and gInfo.dwFlags) <> 0;
          isInertia := (IEGF_INERTIA and gInfo.dwFlags) <> 0;

          case gInfo.dwID of
            IEGID_ZOOM:
              begin
                // begin zoom
                if isBegin then
                begin
                  fGestureStartValue := value;
                  fGestureBaseZoom   := Zoom;
                end
                // perform zoom
                else
                if fGestures.fZoom.Enabled and (not isInertia or fGestures.fZoom.Inertia) then
                begin
                  v := fGestureBaseZoom * (value / fGestureStartValue);
                  v := dmax(v, fGestures.fZoom.Min);
                  v := dmin(v, fGestures.fZoom.Max);
                  v := v * fGestures.fZoom.Multiplier;
                  Zoom := PerformZoomSnap(v);
                  Msg.Result := 0;  // handled
                end
              end;

            IEGID_PAN:
              begin
                // begin pan
                if isBegin then
                begin
                  fGestureStartX := gInfo.ptsLocation.x;
                  fGestureStartY := gInfo.ptsLocation.y;
                  fGestureBaseViewX := ViewX;
                  fGestureBaseViewY := ViewY;
                  fGestureBasePosX := CurrentLayer.PosX;
                  fGestureBasePosY := CurrentLayer.PosY;
                end
                // perform pan
                else
                if fGestures.fPan.Enabled and (not isInertia or fGestures.fPan.Inertia) then
                begin
                  v := fGestureBaseViewX + (fGestureStartX - gInfo.ptsLocation.x);
                  v := dmax(v, fGestures.fPan.BoundingBox.Left);
                  v := dmin(v, fGestures.fPan.BoundingBox.Right);
                  v := v * fGestures.fPan.Multiplier;
                  ViewX := trunc(v);

                  v := fGestureBaseViewY + (fGestureStartY - gInfo.ptsLocation.y);
                  v := dmax(v, fGestures.fPan.BoundingBox.Top);
                  v := dmin(v, fGestures.fPan.BoundingBox.Bottom);
                  v := v * fGestures.fPan.Multiplier;
                  ViewY := trunc(v);

                  Msg.Result := 0;  // handled
                end
                // perform layer move
                else
                if fGestures.fLayerMove.Enabled and (not isInertia or fGestures.fLayerMove.Inertia) then
                begin
                  v := fGestureBasePosX - trunc((fGestureStartX - gInfo.ptsLocation.x) / (ZoomX / 100));
                  v := dmax(v, fGestures.fLayerMove.BoundingBox.Left);
                  v := dmin(v, fGestures.fLayerMove.BoundingBox.Right);
                  v := v * fGestures.fLayerMove.Multiplier;
                  CurrentLayer.PosX := trunc(PerformLayerMoveSnap(v));

                  v := fGestureBasePosY - trunc((fGestureStartY - gInfo.ptsLocation.y) / (ZoomY / 100));
                  v := dmax(v, fGestures.fLayerMove.BoundingBox.Top);
                  v := dmin(v, fGestures.fLayerMove.BoundingBox.Bottom);
                  v := v * fGestures.fLayerMove.Multiplier;
                  CurrentLayer.PosY := trunc(PerformLayerMoveSnap(v));

                  Update();
                  Msg.Result := 0;  // handled
                end;
              end;

            IEGID_ROTATE:
              begin
                // begin rotate
                if isBegin then
                begin
                  fGestureBaseRotate := CurrentLayer.Rotate;
                end
                // perform layer rotate
                else

                if fGestures.fLayerRotate.Enabled and (not isInertia or fGestures.fLayerRotate.Inertia) then
                begin
                  v := fGestureBaseRotate + GID_ROTATE_ANGLE_FROM_ARGUMENT(value) * 180 / PI;
                  v := dmax(v, fGestures.fLayerRotate.Min);
                  v := dmin(v, fGestures.fLayerRotate.Max);
                  v := v * fGestures.fLayerRotate.Multiplier;
                  CurrentLayer.Rotate := PerformRotateSnap(v);
                  Update();
                  Msg.Result := 0;  // handled
                end;
              end;
          end;
        end
        else
        begin
          // handled by user (in OnImageEnGesture event)
          Msg.Result := 0;  // handled
        end;

      finally
        if Msg.Result = 0 then
          IECloseGestureInfoHandle(Msg.LParam);
      end;
    end
  end;
  if Msg.Result <> 0 then
    inherited;
end;


constructor TIEViewerGestures.Create();
begin
  inherited;
  fPan          := TIEGesturePanOptions.Create();
  fZoom         := TIEGestureZoomOptions.Create();
  fLayerRotate  := TIEGestureLayerRotateOptions.Create();
  fLayerMove    := TIEGestureLayerMoveOptions.Create();

  // Pan defaults
  fPan.Enabled := false;
  fPan.Inertia := true;
  fPan.BoundingBox := Rect(Low(integer), Low(integer), High(integer), High(integer));
  fPan.Multiplier := 1.0;
  fPan.SnapValues := false;
  fPan.SnapDelta := 0.0;
  fPan.AddDisables(fLayerMove);

  // Zoom defaults
  fZoom.Enabled := false;
  fZoom.Inertia := true;
  fZoom.Min := 1.0;
  fZoom.Max := 8000.0;
  fZoom.Multiplier := 1.0;
  fZoom.SnapValues := true;
  fZoom.SnapDelta := 5.0;

  // LayerRotate defaults
  fLayerRotate.Enabled := false;
  fLayerRotate.Inertia := false;
  fLayerRotate.Min := - MaxDouble;
  fLayerRotate.Max := MaxDouble;
  fLayerRotate.Multiplier := 1.0;
  fLayerRotate.SnapValues := true;
  fLayerRotate.SnapDelta := 5.0;

  // LayerMove defaults
  fLayerMove.Enabled := false;
  fLayerMove.Inertia := false;
  fLayerMove.BoundingBox := Rect(Low(integer), Low(integer), High(integer), High(integer));
  fLayerMove.Multiplier := 1.0;
  fLayerMove.SnapValues := true;
  fLayerMove.SnapDelta := 5.0;
  fLayerMove.AddDisables(fPan);
end;


destructor TIEViewerGestures.Destroy();
begin
  fPan.Free();
  fZoom.Free();
  fLayerRotate.Free();
  fLayerMove.Free();
  inherited;
end;


function TIEViewerGestures.GetEnabled(): boolean;
begin
  result := fPan.Enabled or fZoom.Enabled or fLayerRotate.Enabled or fLayerMove.Enabled;
end;


procedure TIEGestureOptions.SetEnabled(Value: boolean);
var
  i: integer;
begin
  fEnabled := Value;
  if Value then
    for i := 0 to high(fDisables) do
      fDisables[i].Enabled := false;
end;


procedure TIEGestureOptions.AddDisables(Value: TIEGestureOptions);
var
  l: integer;
begin
  l := length(fDisables);
  SetLength(fDisables, l + 1);
  fDisables[l] := Value;
end;



end.




