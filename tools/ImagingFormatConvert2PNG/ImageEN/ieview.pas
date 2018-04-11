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
File version 1008
*)

unit ieview;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$R-}
{$Q-}


{$I ie.inc}

interface

uses Windows, Messages, SysUtils, Controls, forms, classes, graphics, hyieutils
{$ifdef FPC}
, LCLType, LMessages
{$endif}

;


type

  // called to notify that Bitmap has changed
  TIEBitmapChangeEvent = procedure(Sender: TObject; destroying: boolean) of object;
  PIEBitmapChangeEvent = ^TIEBitmapChangeEvent;

{!!
<FS>TIEView

<FM>Declaration<FC>
TIEView = class(TCustomControl);

<FM>Description<FN>
TIEView is the base abstract class for <A TImageEnView> and <A TImageEnMView>.
!!}
  // Base class for TImageEnView and TImageEnMView
  TIEView = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    fBmpChange: TList; // methods list to call when Bitmap changes
    fOnMouseEnter: TNotifyEvent;
    fOnMouseLeave: TNotifyEvent;

{$IFDEF SupportVclThemes}
    class constructor Create;
    class destructor Destroy;
{$ENDIF}
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetCtl3D: boolean;
    procedure SetCtl3D(v: boolean);
  protected
    fDPIX, fDPIY: integer; // horizontal and vertical dpi
    procedure SetBackGround(cl: TColor); virtual;
    function GetFBitmap: TBitmap; virtual;
    function GetIEBitmap: TIEBitmap; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetAlphaChannel: TIEBitmap; virtual; abstract;
    function GetHasAlphaChannel: boolean; virtual; abstract;
  public
    fBackGround: TColor;
{$IFDEF OCXVERSION}
    ReplyMessageTo: HWND;
{$ENDIF}
    procedure RemoveAlphaChannel(Merge: boolean); virtual; abstract;
    procedure LockPaint; virtual; abstract;
    function UnLockPaint: integer; virtual; abstract;
    function NPUnLockPaint: integer; virtual; abstract;
    procedure SetDPIX(dpiX: integer); virtual;
    procedure SetDPIY(dpiY: integer); virtual;
    procedure SetDPI(dpiX, dpiY: integer); virtual;
    procedure ImageChange; virtual;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property DpiX: integer read fDpiX write SetDPIX;
    property DpiY: integer read fDpiY write SetDPIY;
    property Bitmap: TBitmap read GetFBitmap;
    property IEBitmap: TIEBitmap read GetIEBitmap;
    procedure CallBitmapChangeEvents; virtual;
    function RegisterBitmapChangeEvent(callbackMethod: TIEBitmapChangeEvent): pointer;
    procedure RemoveBitmapChangeEvent(callbackHandle: pointer);
    function GetCanvas: TCanvas;
    property AlphaChannel: TIEBitmap read GetAlphaChannel;
    property HasAlphaChannel: boolean read GetHasAlphaChannel;
    {$IFDEF OCXVERSION}
    procedure WndProc(var Message: TMessage); override;
    {$ENDIF}
  published
    property Background: TColor read fBackGround write SetBackGround;     // NB: No default. Always store!
    property Ctl3D read GetCtl3D write SetCtl3d default true;
    property ParentCtl3D;

{!!
<FS>TIEView.OnMouseEnter

<FM>Declaration<FC>
property OnMouseEnter: TNotifyEvent;

<FM>Description<FN>
OnMouseEnter occurs when the mouse pointer moves over the component.
Write OnMouseEnter event handler to take specific action when the user moves the mouse over the component.
!!}
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;

{!!
<FS>TIEView.OnMouseLeave

<FM>Declaration<FC>
property OnMouseLeave: TNotifyEvent;

<FM>Description<FN>
OnMouseLeave occurs when the mouse pointer moves off of the component.
Write OnMouseLeave event handler to take specific action when the user moves the mouse off the component.
!!}
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
    
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;

    property OnEnter;
    property OnExit;
    property OnResize;

    {$ifdef IEHASTOUCH}
    property Touch;
    {$endif}

  end;



implementation

uses
  {$IFDEF SupportVclThemes} Vcl.Themes, iexThemes, iesettings, {$ENDIF}
  iemview, hyiedefs;

/////////////////////////////////////////////////////////////////////////////////////

procedure TIEView.ImageChange;
begin
  //
end;

/////////////////////////////////////////////////////////////////////////////////////

{$IFDEF SupportVclThemes}
class constructor TIEView.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TIEView, TImageEnStyleHook);
end;
{$ENDIF}

constructor TIEView.Create(Owner: TComponent);
begin
  {$IFDEF OCXVERSION}
  ReplyMessageTo := 0;
  {$ENDIF}
  inherited;

  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls, csReplicatable {$ifdef IEHASNEEDSBORDERPAINT}, csNeedsBorderPaint {$endif}];

  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  fOnMouseEnter := nil;
  fOnMouseLeave := nil;
  controlstyle := controlstyle + [csOpaque];
  fBackground := clBtnFace;
  FBorderStyle := bsSingle;
  fBmpChange := TList.Create;
  Ctl3D := true;
end;

{$IFDEF SupportVclThemes}
class destructor TIEView.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TIEView, TImageEnStyleHook);
end;
{$ENDIF}

destructor TIEView.Destroy;
begin
  while fBmpChange.Count > 0 do
  begin
    if assigned(fBmpChange[0]) then
      PIEBitmapChangeEvent(fBmpChange[0])^(self, true);
    dispose(fBmpChange[0]);
    fBmpChange.Delete(0);
  end;
  FreeAndNil(fBmpChange);

  inherited;
end;

{!!
<FS>TIEView.SetDPI

<FM>Declaration<FC>
procedure SetDPI(dpiX, dpiY: integer);

<FM>Description<FN>
The SetDPI method sets the horizontal (DpiX) and vertical (DpiY) dots per inch of the current image. These values will be saved together with the image.
!!}
procedure TIEView.SetDPI(dpiX, dpiY: integer);
begin
  SetDPIX(dpiX);
  SetDPIY(dpiY);
end;

{!!
<FS>TIEView.DpiX

<FM>Declaration<FC>
property DpiX: integer;

<FM>Description<FN>
DpiX is the number of horizontal dots (pixels) per inch of the image. This value is loaded and saved to file in <A TImageEnIO> component.

!!}
procedure TIEView.SetDPIX(dpiX: integer);
begin
  if dpiX <> 0 then
    fDPIX := dpiX;
end;

{!!
<FS>TIEView.DpiY

<FM>Declaration<FC>
property DpiY: integer;

<FM>Description<FN>
DpiY is the number of vertical dots (pixels) per inch of the image. This value is loaded and saved to file in <A TImageEnIO> component.

!!}
procedure TIEView.SetDPIY(dpiY: integer);
begin
  if dpiY <> 0 then
    fDPIY := dpiY;
end;

function TIEView.GetFBitmap: TBitmap;
begin
  result := nil;
end;

function TIEView.GetIEBitmap: TIEBitmap;
begin
  result := nil;
end;

// Background property corresponds to Color of TControl: they are synchronized
procedure TIEView.SetBackGround(cl: TColor);
begin
  fBackground := cl;
end;

procedure TIEView.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TIEView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

// Adds "callbackMethod" method to the list of methods to call when Bitmap changes (when it points to another object)
function TIEView.RegisterBitmapChangeEvent(callbackMethod: TIEBitmapChangeEvent): pointer;
var
  callbackHandle: PIEBitmapChangeEvent;
begin
  new(callbackHandle);
  callbackHandle^ := callbackMethod;
  fBmpChange.Add(callbackHandle);
  result := callbackHandle;
end;

// Removes method "callbackHandle" from methods to call when Bitmap points to another object
procedure TIEView.RemoveBitmapChangeEvent(callbackHandle: pointer);
var
  i: integer;
begin
  if assigned(fBmpChange) and assigned(callbackHandle) then
  begin
    i := fBmpChange.IndexOf(pointer(callbackHandle));
    if i > -1 then
    begin
      dispose(fBmpChange[i]);
      fBmpChange.Delete(i);
    end;
  end;
end;

// Call registered events using RegisterBitmapChangeEvent()
procedure TIEView.CallBitmapChangeEvents;
var
  q: integer;
begin
  if assigned(fBmpChange) then
    for q := 0 to fBmpChange.Count - 1 do
      if assigned(fBmpChange[q]) then
        PIEBitmapChangeEvent(fBmpChange[q])^(self, false);
end;

function TIEView.GetCtl3D: boolean;
begin
  result := inherited Ctl3D;
end;

procedure TIEView.SetCtl3D(v: boolean);
begin
  {$IFDEF THEMED_BORDERS}
  // Need to disable 3D for theming
  ParentCtl3D := False;
  v := False;
  {$ENDIF THEMED_BORDERS}

  if v <> (inherited Ctl3D) then
  begin
    inherited Ctl3D := v;
    RecreateWnd;
  end;
end;

procedure TIEView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if assigned(fOnMouseEnter) then
    fOnMouseEnter(Self);
end;

procedure TIEView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if assigned(fOnMouseLeave) then
    fOnMouseLeave(Self);
end;

{!!
<FS>TIEView.GetCanvas

<FM>Declaration<FC>
function GetCanvas: TCanvas;

<FM>Description<FN>
The GetCanvas method returns the <A TImageEnView> TCanvas object. Use it when you override Paint method to draw your object to the client area of the component.
!!}
function TIEView.GetCanvas: TCanvas;
begin
  result := Canvas;
end;

{$IFDEF OCXVERSION}
procedure TIEView.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if (ReplyMessageTo <> 0) then
    case Message.Msg of
      WM_MOUSEMOVE,
      WM_ENABLE,
      WM_QUERYDRAGICON,
      WM_COPYDATA,
      WM_HELP,
      WM_CONTEXTMENU,
      WM_KEYDOWN,
      WM_KEYUP,
      WM_SYSKEYDOWN,
      WM_SYSKEYUP,
      WM_KEYLAST,
      WM_ENTERMENULOOP,
      WM_EXITMENULOOP,
      WM_NEXTMENU,
      WM_DROPFILES,
      WM_HOTKEY :
        begin
          PostMessage(ReplyMessageTo, Message.Msg, Message.WParam, Message.LParam);
        end;
    end;
end;
{$ENDIF} // end of OCXVERSION




end.
