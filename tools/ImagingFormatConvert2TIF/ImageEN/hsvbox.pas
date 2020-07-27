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

unit hsvbox;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ImageEnProc, hyiedefs;

type

{!!
<FS>THSVBox

<FM>Description<FN>
THSVBox allows selection of a color in the HSV (Hue Saturation Value) color space.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.Background></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.BarsDistance></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.Blue></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.Color></C> </R>
<R> <C_IMG_METHOD> <C><A THSVBox.GetColorAt></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.Green></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.Hue></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.HueBarWidth></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.Red></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.Sat></C> </R>
<R> <C_IMG_METHOD> <C><A THSVBox.SetColor></C> </R>
<R> <C_IMG_METHOD> <C><A THSVBox.SetRGB></C> </R>
<R> <C_IMG_PUBLISHED> <C><A THSVBox.Val></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A THSVBox.OnChange></C> </R>
</TABLE>

!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  THSVBox = class(TCustomControl)
  private
    { Private declarations }
    fHue: integer; // from 0 to 359
    fSat: integer; // from 0 to 99
    fVal: integer; // from 0 to 99
    fRed: byte;
    fGreen: byte;
    fBlue: byte;
    bitmap: TBitMap;
    fBackground: TColor;
    fOnChange: TNotifyEvent;
    fMouseSel: integer; // 0=none  1=capture on sat/val  2=capture on hue
    fColor: TColor;
    fHueBarWidth: integer;
    fBarsDistance: integer;
  protected
    { Protected declarations }
    procedure SetHue(h: integer);
    procedure SetSat(s: integer);
    procedure SetVal(v: integer);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure SetBackground(bk: TColor);
    procedure DrawValSat;
    procedure DrawHue;
    procedure DrawGrips;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure XMouseMove(X, Y: integer);
    procedure SetHueBarWidth(v: integer);
    procedure SetBarsDistance(v: integer);
  public
    { Public declarations }
    procedure Paint; override;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure SetRGB(r, g, b: byte);
    procedure SetColor(cl: TColor);
    function GetColorAt(x, y: integer): TColor;
  published
    { Published declarations }

{!!
<FS>THSVBox.Hue

<FM>Declaration<FC>
property Hue: integer;

<FM>Description<FN>
Hue channel of the current color.
!!}
    property Hue: integer read fHue write SetHue default 0; // from 0 to 359

{!!
<FS>THSVBox.Sat

<FM>Declaration<FC>
property Sat: integer;

<FM>Description<FN>
Saturation channel of the current color.
!!}
    property Sat: integer read fSat write SetSat default 0; // from 0 to 99

{!!
<FS>THSVBox.Val

<FM>Declaration<FC>
property Val: integer;

<FM>Description<FN>
Value channel of the current color.
!!}
    property Val: integer read fVal write SetVal default 0; // from 0 to 99

{!!
<FS>THSVBox.Red

<FM>Declaration<FC>
property Red: byte;

<FM>Description<FN>
Red channel of the conversion from current HSV color to RGB.

Read-only
!!}
    property Red: byte read fRed; // WARNING!! MUST BE READ-ONLY!!

{!!
<FS>THSVBox.Green

<FM>Declaration<FC>
property Green: byte;

<FM>Description<FN>
Green channel of the conversion from current HSV color to RGB.

Read-only
!!}
    property Green: byte read fGreen; // WARNING!! MUST BE READ-ONLY!!

{!!
<FS>THSVBox.Blue

<FM>Declaration<FC>
property Blue: byte;

<FM>Description<FN>
Blue channel of the conversion from current HSV color to RGB.

Read-only
!!}
    property Blue: byte read fBlue; // WARNING!! MUST BE READ-ONLY!!

{!!
<FS>THSVBox.Color

<FM>Declaration<FC>
property Color: TColor;

<FM>Description<FN>
Conversion of current HSV color in TColor.

Read-only
!!}
    property Color: TColor read fColor; // ATTN!! IT MUST BE READ-ONLY!!

    property Background: TColor read fBackground write SetBackground default clBtnFace;

{!!
<FS>THSVBox.OnChange

<FM>Declaration<FC>
property OnChange: TNotifyEvent;

<FM>Description<FN>
This event is called whenever a color is changed.
!!}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    property HueBarWidth: integer read fHueBarWidth write SetHueBarWidth default 20;
    property BarsDistance: integer read fBarsDistance write SetBarsDistance default 5;
    property Align;
{$IFDEF IESUPPORTANCHORS}
    property Anchors;
{$ENDIF}
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


/////////////////////////////////////////////////////////////////////////////////////

constructor THSVBox.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  ControlStyle := ControlStyle + [csOpaque, csAcceptsControls];
  fHueBarWidth := 20;
  fBarsDistance := 5;
  fOnChange := nil;
  fBackground := clBtnFace;
  fMouseSel := 0; // no capture
  Height := 105;
  Width := 105;
  fHue := 0;
  fSat := 0;
  fVal := 0;
  fRed := 0;
  fGreen := 0;
  fBlue := 0;
  fColor := 0;
  bitmap := TBitmap.create;
  bitmap.PixelFormat := pf24bit;
  bitmap.Width := width;
  bitmap.Height := height;
  DrawHue;
  DrawValSat;
  DrawGrips;
end;

/////////////////////////////////////////////////////////////////////////////////////

destructor THSVBox.Destroy;
begin
  FreeAndNil(bitmap);
  inherited;
end;

/////////////////////////////////////////////////////////////////////////////////////

procedure THSVBox.Paint;
begin
  canvas.Draw(0, 0, bitmap);
end;

procedure THSVBox.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure THSVBox.WMSize(var Message: TWMSize);
begin
  inherited;
  bitmap.Width := message.Width;
  bitmap.Height := message.Height;
  DrawHue;
  DrawValSat;
  DrawGrips;
  invalidate;
end;

{!!
<FS>THSVBox.Background

<FM>Declaration<FC>
property Background: TColor;

<FM>Description<FN>
Background is the background color.
!!}
procedure THSVBox.SetBackground(bk: TColor);
begin
  fBackground := bk;
  DrawHue;
  DrawValSat;
  DrawGrips;
  invalidate;
end;


// paint box Sat/val. Do not invalidate.
procedure THSVBox.DrawValSat;
var
  SLWIDTH: integer; // width in pixels of Sat/Val box
  v: integer;
  ss, sv: single;
  col, row: integer;
  px: pRGB;
begin
  SLWIDTH := bitmap.width - fBarsDistance - fHueBarWidth;
  if (SLWIDTH > 0) and (bitmap.Height>0) then
  begin
    ss := 100 / SLWIDTH;
    sv := 100 / bitmap.height;
    // paint Sat/Val box
    for row := 0 to bitmap.height - 1 do
    begin
      v := trunc(sv * row);
      px := bitmap.ScanLine[bitmap.height - row - 1];
      for col := 0 to SLWIDTH - 1 do
      begin
        HSV2RGB(px^, fHue, trunc(ss * col), v);
        inc(px);
      end;
    end;
  end;
end;


// paint hue bar and other stuff
procedure THSVBox.DrawHue;
var
  SLWIDTH: integer; // width in pixel of Sat/Val box
  sh: single;
  col, row: integer;
  px: pRGB;
  bk: TRGB;
  fo: TRGB;
begin
  SLWIDTH := bitmap.width - fBarsDistance - fHueBarWidth;
  if (SLWIDTH > 0) and (Bitmap.Height>0) then
  begin
    sh := 360 / bitmap.height;
    // draws hue bar
    bk := TColor2TRGB(fBackground);
    for row := 0 to bitmap.height - 1 do
    begin
      px := bitmap.ScanLine[bitmap.height - row - 1];
      inc(px, SLWIDTH);
      for col := SLWIDTH to SLWIDTH + fBarsDistance - 1 do
      begin
        px^ := bk; // background
        inc(px);
      end;
      HSV2RGB(fo, trunc(sh * row), 99, 99);
      for col := SLWIDTH + fBarsDistance to width - 1 do
      begin
        px^ := fo;
        inc(px);
      end;
    end;
  end;
end;


// Paint grips of Hue Bar and Val/Sat box
procedure THSVBox.DrawGrips;
var
  x, y: integer;
  SLWIDTH: integer; // width in pixels of Sat/Val box
begin
  bitmap.canvas.pen.style := psSolid;
  bitmap.canvas.pen.mode := pmNot;
  bitmap.canvas.Brush.style := bsClear;
  bitmap.canvas.pen.width := 1;
  SLWIDTH := bitmap.width - fBarsDistance - fHueBarWidth;
  if (SLWIDTH > 1) and (bitmap.Height-1<>0) then
  begin
    // hue
    y := round((359 - fHue) / 360 * (bitmap.height - 1));
    bitmap.Canvas.rectangle(SLWIDTH + fBarsDistance, y - 2, width, y + 2);
    // sat/val
    x := round(fSat / 99 * (SLWIDTH - 1));
    y := bitmap.height - round((bitmap.height - 1) * fVal / 99) - 1;
    bitmap.Canvas.rectangle(x - 3, y - 3, x + 3, y + 3);
  end;
end;


procedure THSVBox.XMouseMove(X, Y: integer);
var
  SLWIDTH: integer; // width in pixels of Sat/Val box
  px: TRGB;
begin
  SLWIDTH := bitmap.width - fBarsDistance - fHueBarWidth;
  if SLWIDTH > 1 then
  begin
    if X < SLWIDTH then
    begin
      // box sat/val
      DrawGrips;
      fSat := round(X / (SLWIDTH - 1) * 99);
      fVal := 99 - round(Y / (bitmap.height - 1) * 99);
    end
    else
    if X >= SLWIDTH + fBarsDistance then
    begin
      // hue bar
      DrawGrips;
      fHue := 359 - round(y / (bitmap.height - 1) * 359);
      DrawValSat;
    end;
    HSV2RGB(px, fHue, fSat, fVal);
    fColor := TRGB2TColor(px);
    fRed := px.r;
    fGreen := px.g;
    fBlue := px.b;
    if Assigned(fOnChange) then
      fOnChange(Self);
    DrawGrips;
    paint;
  end;
end;

{!!
<FS>THSVBox.GetColorAt

<FM>Declaration<FC>
function GetColorAt(x, y: integer): TColor;

<FM>Description<FN>
GetColorAt returns the color at component coordinates x, y. Useful in response to MouseMove event.

!!}
function THSVBox.GetColorAt(x, y: integer): TColor;
var
  s, v: integer;
  px: TRGB;
  SLWIDTH: integer;
begin
  result := fColor;
  SLWIDTH := bitmap.width - fBarsDistance - fHueBarWidth;
  if SLWIDTH>1 then
  begin
    if x < SLWIDTH then
    begin
      s := round(X / (SLWIDTH - 1) * 99);
      v := 99 - round(Y / (bitmap.height - 1) * 99);
      HSV2RGB(px, fHue, s, v);
      result := TRGB2TColor(px);
    end
  end;
end;


procedure THSVBox.SetHue(h: integer);
var
  px: TRGB;
begin
  if h < 0 then
    h := 0;
  if h > 359 then
    h := 359;
  DrawGrips;
  fHue := h;
  HSV2RGB(px, fHue, fSat, fVal);
  fColor := TRGB2TColor(px);
  fRed := px.r;
  fGreen := px.g;
  fBlue := px.b;
  DrawValSat;
  DrawGrips;
  paint;
end;


procedure THSVBox.SetSat(s: integer);
var
  px: TRGB;
begin
  if s < 0 then
    s := 0;
  if s > 99 then
    s := 99;
  DrawGrips;
  fSat := s;
  HSV2RGB(px, fHue, fSat, fVal);
  fColor := TRGB2TColor(px);
  fRed := px.r;
  fGreen := px.g;
  fBlue := px.b;
  DrawGrips;
  paint;
end;


procedure THSVBox.SetVal(v: integer);
var
  px: TRGB;
begin
  if v < 0 then
    v := 0;
  if v > 99 then
    v := 99;
  DrawGrips;
  fVal := v;
  HSV2RGB(px, fHue, fSat, fVal);
  fColor := TRGB2TColor(px);
  fRed := px.r;
  fGreen := px.g;
  fBlue := px.b;
  DrawGrips;
  paint;
end;


{!!
<FS>THSVBox.SetRGB

<FM>Declaration<FC>
procedure SetRGB(r, g, b: byte);

<FM>Description<FN>
Sets current color as RGB.

!!}
procedure THSVBox.SetRGB(r, g, b: byte);
var
  px: TRGB;
begin
  DrawGrips;
  fRed := r;
  fGreen := g;
  fBlue := b;
  px := creatergb(fRed, fGreen, fBlue);
  RGB2HSV(px, fHue, fSat, fVal);
  fColor := TRGB2TColor(px);
  DrawValSat;
  DrawGrips;
  paint;
end;


procedure THSVBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  SLWIDTH: integer; // width in pixels of Sat/Val box
begin
  inherited;

  if MouseCapture then
  begin
    SLWIDTH := bitmap.width - fBarsDistance - fHueBarWidth;
    if SLWIDTH > 0 then
    begin
      if x < 0 then
        x := 0;
      if y < 0 then
        y := 0;
      if y >= bitmap.height then
        y := bitmap.height - 1;
      if fMouseSel = 1 then
      begin
        if x >= SLWIDTH then
          x := SLWIDTH - 1;
        XMouseMove(x, y);
      end
      else
      if fMouseSel = 2 then
      begin
        if x < SLWIDTH + fBarsDistance then
          x := SLWIDTH + fBarsDistance
        else
        if x >= bitmap.width then
          x := bitmap.width - 1;
        XMouseMove(x, y);
      end;
    end;
  end;
end;


procedure THSVBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SLWIDTH: integer; // width in pixels of Sat/Val box
begin
  inherited;
  if (Button = mbLeft) then
  begin
    SLWIDTH := bitmap.width - fBarsDistance - fHueBarWidth;
    if SLWIDTH > 0 then
    begin
      if x < SLWIDTH then
      begin
        fMouseSel := 1; // capture val/sat
        XMouseMove(x, y);
      end
      else
      if x >= SLWIDTH + fBarsDistance then
      begin
        fMouseSel := 2; // capture hue
        XMouseMove(x, y);
      end
      else
        fMouseSel := 0; // no capture
    end;
  end;
end;


procedure THSVBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  fMouseSel := 0;
end;


{!!
<FS>THSVBox.SetColor

<FM>Declaration<FC>
procedure SetColor(cl: TColor);

<FM>Description<FN>
Sets current color as TColor.

!!}
procedure THSVBox.SetColor(cl: TColor);
var
  rgb: TRGB;
begin
  DrawGrips;
  rgb := TColor2TRGB(cl);
  fRed := rgb.r;
  fGreen := rgb.g;
  fBlue := rgb.b;
  RGB2HSV(rgb, fHue, fSat, fVal);
  fColor := cl;
  DrawValSat;
  DrawGrips;
  paint;
end;


{!!
<FS>THSVBox.HueBarWidth

<FM>Declaration<FC>
property HueBarWidth: integer;

<FM>Description<FN>
HueBarWidth specifies the width of the Hue bar. Set this value to 0 to remove the Hue bar (right bar).

<FM>Example<FC>
HSVBox1.HueBarWidth := 0;  // removes Hue bar
!!}
procedure THSVBox.SetHueBarWidth(v: integer);
begin
  if v >= 0 then
  begin
    fHueBarWidth := v;
    DrawHue;
    DrawValSat;
    DrawGrips;
    invalidate;
  end;
end;


{!!
<FS>THSVBox.BarsDistance

<FM>Declaration<FC>
property BarsDistance: integer;

<FM>Description<FN>
BarsDistance specifies the distance of Hue bar from color box (left box).

<FM>Example<FC>
HSVBox1.BarsDistance := 0;  // removes distance from color bar and hue bar
!!}
procedure THSVBox.SetBarsDistance(v: integer);
begin
  if v >= 0 then
  begin
    fBarsDistance := v;
    DrawHue;
    DrawValSat;
    DrawGrips;
    invalidate;
  end;
end;


end.


