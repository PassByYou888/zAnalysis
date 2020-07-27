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

unit iegradientbar;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ImageEnProc, hyiedefs, iegdiplus;

type

{!!
<FS>TIEGradientDir

<FM>Declaration<FC>
}
  TIEGradientDir = (gdHorizontal, gdVertical);
{!!}

  // automatic mouse interactions
{!!
<FS>TIEMouseInteractItemsGr

<FM>Declaration<FC>
}
  TIEMouseInteractItemsGr = ( migDragGrip );
{!!}

{!!
<FS>TIEMouseInteractGr

<FM>Declaration<FC>
type TIEMouseInteractGr = set of <A TIEMouseInteractItemsGr>;
!!}
  TIEMouseInteractGr = set of TIEMouseInteractItemsGr;

{!!
<FS>TIEGradientBar

<FM>Description<FN>
TIEGradientBar component shows a gradient bar, with a grip with which the user can select a color or an index.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TIEGradientBar.BeginColor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGradientBar.Color></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGradientBar.ColorIndex></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIEGradientBar.Direction></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIEGradientBar.EndColor></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TIEGradientBar.MouseInteract></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIEGradientBar.RGB></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TIEGradientBar.OnChange></C> </R>
</TABLE>
!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TIEGradientBar = class(TCustomControl)
  private
    { Private declarations }
    fColorIndex: integer;
    fBeginColor: TColor;
    fEndColor: TColor;
    fGradient: array[0..255] of TColor;
    fDirection: TIEGradientDir;
    fOnChange: TNotifyEvent;
    fMouseInteract: TIEMouseInteractGr;
    procedure SetColorIndex(v: integer);
    procedure SetBeginColor(v: TColor);
    procedure SetEndColor(v: TColor);
    function GetColor: TColor;
    function GetRGB: TRGB;
    procedure SetDirection(v: TIEGradientDir);
    procedure SetMouseInteractGr(v: TIEMouseInteractGr);
    function GetMouseInteractGr: TIEMouseInteractGr;
  protected
    { Protected declarations }
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CalcGradient;
    procedure DrawGrip;
  public
    { Public declarations }
    procedure Paint; override;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property ColorIndex: integer read fColorIndex write SetColorIndex;
    property Color: TColor read GetColor;
    property RGB: TRGB read GetRGB;
  published
    { Published declarations }
    property BeginColor: TColor read fBeginColor write SetBeginColor default clBlack;
    property EndColor: TColor read fEndColor write SetEndColor default clWhite;
    property Direction: TIEGradientDir read fDirection write SetDirection default gdVertical;

{!!
<FS>TIEGradientBar.OnChange

<FM>Declaration<FC>
property OnChange: TNotifyEvent;

<FM>Description<FN>
This event is called when the user selects a color.
!!}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    property MouseInteract: TIEMouseInteractGr read GetMouseInteractGr write SetmouseInteractGr default [migDragGrip];
    property Width default 40;
    property Height default 300;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
  end;

implementation

uses hyieutils;

{$R-}

constructor TIEGradientBar.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  Height := 300;
  Width := 40;
  fColorIndex := 0;
  fBeginColor := clBlack;
  fEndColor := clWhite;
  fDirection := gdVertical;
  fMouseInteract := [migDragGrip];
  CalcGradient;
end;

destructor TIEGradientBar.Destroy;
begin
  inherited;
end;

procedure TIEGradientBar.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TIEGradientBar.WMSize(var Message: TWMSize);
begin
  inherited;
end;

{!!
<FS>TIEGradientBar.ColorIndex

<FM>Declaration<FC>
property ColorIndex: integer;

<FM>Description<FN>
ColorIndex is the color index that user has selected. It can be 0 (<A TIEGradientBar.BeginColor>) up to 255 (<A TIEGradientBar.EndColor>).

!!}
procedure TIEGradientBar.SetColorIndex(v: integer);
begin
  fColorIndex := v;
  invalidate;
end;

{!!
<FS>TIEGradientBar.BeginColor

<FM>Declaration<FC>
property BeginColor: TColor;

<FM>Description<FN>
First color of the gradient bar.
!!}
procedure TIEGradientBar.SetBeginColor(v: TColor);
begin
  fBeginColor := v;
  CalcGradient;
  invalidate;
end;

{!!
<FS>TIEGradientBar.EndColor

<FM>Declaration<FC>
property EndColor: TColor;

<FM>Description<FN>
Final color of the gradient bar.
!!}
procedure TIEGradientBar.SetEndColor(v: TColor);
begin
  fEndColor := v;
  CalcGradient;
  invalidate;
end;

{!!
<FS>TIEGradientBar.Color


<FM>Declaration<FC>
property Color: TColor;

<FM>Description<FN>
The color the user has selected.

Read-only

!!}
function TIEGradientBar.GetColor: TColor;
begin
  result := fGradient[fColorIndex];
end;

{!!
<FS>TIEGradientBar.RGB

<FM>Declaration<FC>
property RGB: <A TRGB>;

<FM>Description<FN>
The color the user has selected.

Read-only

!!}
function TIEGradientBar.GetRGB: TRGB;
begin
  result := TColor2TRGB(fGradient[fColorIndex]);
end;

{!!
<FS>TIEGradientBar.Direction

<FM>Declaration<FC>
property Direction: <A TIEGradientDir>;

<FM>Description<FN>
Direction is gdHorizontal for a horizontal bar or gdVertical for a vertical bar.
!!}
procedure TIEGradientBar.SetDirection(v: TIEGradientDir);
begin
  fDirection := v;
  invalidate;
end;

procedure TIEGradientBar.CalcGradient;
var
  BeginRGB, EndRGB: TRGB;
  ir, ig, ib: double;
  q: integer;
begin
  BeginRGB := TColor2TRGB(fBeginColor);
  EndRGB := TColor2TRGB(fEndColor);
  ir := (-EndRGB.r + BeginRGB.r) / 255;
  ig := (-EndRGB.g + BeginRGB.g) / 255;
  ib := (-EndRGB.b + BeginRGB.b) / 255;
  for q := 0 to 255 do
    fGradient[q] := RGB2TColor(blimit(EndRGB.r + trunc(q * ir)),
      blimit(EndRGB.g + trunc(q * ig)),
      blimit(EndRGB.b + trunc(q * ib)));
end;

procedure TIEGradientBar.Paint;
var
  ii: double;
  q: integer;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Mode := pmCopy;
  case fDirection of
    gdVertical:
      begin
        if ClientHeight < 2 then
          exit;
        ii := 255 / (ClientHeight - 1);
        for q := 0 to ClientHeight - 1 do
        begin
          Canvas.Pen.Color := fGradient[trunc(ii * q)];
          Canvas.MoveTo(0, q);
          Canvas.LineTo(ClientWidth, q);
        end;
      end;
    gdHorizontal:
      begin
        if ClientWidth < 2 then
          exit;
        ii := 255 / (ClientWidth - 1);
        for q := 0 to ClientWidth - 1 do
        begin
          Canvas.Pen.Color := fGradient[trunc(ii * q)];
          Canvas.MoveTo(q, 0);
          Canvas.LineTo(q, ClientHeight);
        end;
      end;
  end;
  if (not (csDesigning in ComponentState)) and (migDragGrip in fMouseInteract) then
    DrawGrip;
end;

// draw grip at fColorIndex position

procedure TIEGradientBar.DrawGrip;
var
  ii: double;
  pp: integer;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Mode := pmNot;
  Canvas.Brush.Style := bsClear;
  case fDirection of
    gdVertical:
      begin
        ii := (ClientHeight - 1) / 255;
        pp := trunc(fColorIndex * ii);
        Canvas.Rectangle(0, pp + 3, clientwidth, pp - 3);
      end;
    gdHorizontal:
      begin
        ii := (ClientWidth - 1) / 255;
        pp := trunc(fColorIndex * ii);
        Canvas.Rectangle(pp - 3, 0, pp + 3, clientheight);
      end;
  end;
end;

// mouse movement

procedure TIEGradientBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  //
  y := ilimit(y, 0, clientheight - 1);
  x := ilimit(x, 0, clientwidth - 1);
  if MouseCapture then
  begin
    if (migDragGrip in fMouseInteract) then
    begin
      DrawGrip;
      case fDirection of
        gdVertical: fColorIndex := trunc(y / (clientheight - 1) * 255);
        gdHorizontal: fColorIndex := trunc(x / (clientwidth - 1) * 255);
      end;
      DrawGrip;
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
  end;
end;

// pressing button mouse

procedure TIEGradientBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  y := ilimit(y, 0, clientheight - 1);
  x := ilimit(x, 0, clientwidth - 1);
  if (Button = mbLeft) then
  begin
    if (migDragGrip in fMouseInteract) then
    begin
      DrawGrip;
      case fDirection of
        gdVertical: fColorIndex := trunc(y / (clientheight - 1) * 255);
        gdHorizontal: fColorIndex := trunc(x / (clientwidth - 1) * 255);
      end;
      DrawGrip;
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
  end;
end;

// mouse up

procedure TIEGradientBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

{!!
<FS>TIEGradientBar.MouseInteract

<FM>Declaration<FC>
property MouseInteract: <A TIEMouseInteractGr>;

<FM>Description<FN>
Specify the automatic interactions with mouse (user).

MouseInteract can be migDragGrip to select a color (or an index).
!!}
procedure TIEGradientBar.SetMouseInteractGr(v: TIEMouseInteractGr);
begin
  fMouseInteract := v;
end;

function TIEGradientBar.GetMouseInteractGr: TIEMouseInteractGr;
begin
  result := fMouseInteract;
end;

end.



