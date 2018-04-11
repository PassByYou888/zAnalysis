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
File version 1005
*)

unit rulerbox;

{$R-}
{$Q-}

{$I ie.inc}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  {$ifdef IEHASTYPES} Types, {$endif}
  iegdiplus;

const
  IEMAXPOLYGLINES = 10;

type

  TIEPolyg = array[0..IEMAXPOLYGLINES - 1] of TPoint;

{!!
<FS>TGripKind

<FM>Declaration<FC>
}
  TGripKind = (gkTriangle, gkLeftTriangle, gkRightTriangle, gkArrow, gkArrow2);
{!!}

{!!
<FS>TGripsDir

<FM>Declaration<FC>
}
  TGripsDir = (gdUp, gdDown);
{!!}

{!!
<FS>TRulerDir

<FM>Declaration<FC>
}
  TRulerDir = (rdHorizontal, rdVertical);
{!!}

{!!
<FS>TRulerPosChangeEvent

<FM>Declaration<FC>
}
  TRulerPosChangeEvent = procedure(Sender: TObject; Grip: integer) of object;
{!!}

{!!
<FS>TRulerGripClickEvent

<FM>Declaration<FC>
}
  TRulerGripClickEvent = procedure(Sender: TObject; Grip: integer) of object;
{!!}

{!!
<FS>TRulerGripDblClickEvent

<FM>Declaration<FC>
}
  TRulerGripDblClickEvent = procedure(Sender: TObject; Grip: integer) of object;
{!!}

{!!
<FS>TRulerClickEvent

<FM>Declaration<FC>
}
  TRulerClickEvent = procedure(Sender: TObject; ps: double) of object;
{!!}

{!!
<FS>TRulerBox

<FM>Description<FN>
TRulerBox is a ruler. It was included in ImageEn for the histogram equalization preview dialog.

<FM>Methods and Properties<FN>
<TABLE2>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.Background></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.DotPerUnit></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.FitInView></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.Frequency></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.GripBaseDim></C> </R>
<R> <C_IMG_PROPERTY> <C><A TRulerBox.GripsColor></C> </R>
<R> <C_IMG_PROPERTY> <C><A TRulerBox.GripsCount></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.GripsDir></C> </R>
<R> <C_IMG_PROPERTY> <C><A TRulerBox.GripsMax></C> </R>
<R> <C_IMG_PROPERTY> <C><A TRulerBox.GripsMin></C> </R>
<R> <C_IMG_PROPERTY> <C><A TRulerBox.GripsPos></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.GripsKind></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.HexLabels></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.Inverted></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.LabelFreq></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.MaxGripHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TRulerBox.OffsetX></C> </R>
<R> <C_IMG_PROPERTY> <C><A TRulerBox.OffsetY></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.RulerColor></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.RulerDir></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.Ruler></C> </R>
<R> <C_IMG_PROPERTY> <C><A TRulerBox.ScrollRate></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.ViewMax></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.ViewMin></C> </R>
<R> <C_IMG_PUBLISHED> <C><A TRulerBox.ViewPos></C> </R>
</TABLE>

<FM>Events<FN>
<TABLE2>
<R> <C_IMG_EVENT> <C><A TRulerBox.OnRulerClick></C> </R>
<R> <C_IMG_EVENT> <C><A TRulerBox.OnRulerGripClick></C> </R>
<R> <C_IMG_EVENT> <C><A TRulerBox.OnRulerGripDblClick></C> </R>
<R> <C_IMG_EVENT> <C><A TRulerBox.OnRulerPosChange></C> </R>
</TABLE>
!!}
  {$ifdef IEHASPLATFORMATTRIBUTE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$endif}
  TRulerBox = class(TCustomControl)
  private
    { Private declarations }
    Bitmap: TBitmap;
    fBackground: TColor;
    fGrips: TList;          // list of rips
    fGripBaseDim: integer;  // base size of grip (triangles)
    fSelGrip: integer;      // selected grip (-1=none)
    fSelGripSt: integer;    // starting position during mouse-dragging
    fMX1, fMY1: integer;    // initial mouse coordinate during mouse-dragging
    fGripsDir: TGripsDir;   // direction for all grips
    fRuler: boolean;        // ruler + numeric labels
    fViewPos: double;       // starting ruler position
    fDPU: double;           // DotPerUnit - relation between ruler and pixels
    fFrequency: double;     // line frequency (Unit based)
    fLabelFreq: double;     // label frequency (Unit based)
    fRulerColor: TColor;    // ruler color
    fRulerDir: TRulerDir;   // ruler orientation
    fViewMin, fViewMax: double; // Max and Min for fViewPos
    fOnRulerPosChange: TRulerPosChangeEvent; // event
    fOnRulerGripClick: TRulerGripClickEvent; // event
    fOnRulerGripDblClick: TRulerGripDblClickEvent; // event
    fOnRulerClick: TRulerClickEvent; // event
    fFitInView: boolean;    // adjust fDPU from fViewMin and fViewMax
    fHexLabels: boolean;    // if true display labels in hex
    fMaxGripHeight: integer;
    fInverted: boolean;      // ruler is inverted
    fScrollRate: double;     // scroll rate
    fOffsetX, fOffsetY: integer;        // horizontal or vertical offset
    procedure SetBackground(bk: TColor);
    procedure SetGripsCount(v: integer);
    function GetGripsCount: integer;
    procedure SetGripsPos(i: integer; p: double);
    function GetGripsPos(i: integer): double;
    procedure SetGripsCol(i, p: integer);
    function GetGripsCol(i: integer): integer;
    procedure SetGripBaseDim(v: integer);
    procedure SetGripsKind(i: integer; v: TGripKind);
    function GetGripsKind(i: integer): TGripKind;
    procedure SetGripsDir(v: TGripsDir);
    procedure SetRuler(v: boolean);
    procedure SetViewPos(v: double);
    procedure SetDPU(v: double);
    procedure SetFrequency(v: double);
    procedure SetLabelFreq(v: double);
    procedure SetRulerColor(v: TColor);
    procedure SetRulerDir(v: TRulerDir);
    procedure SetGripsMax(i: integer; v: double);
    procedure SetGripsMin(i: integer; v: double);
    function GetGripsMax(i: integer): double;
    function GetGripsMin(i: integer): double;
    procedure SetViewMin(v: double);
    procedure SetViewMax(v: double);
    procedure SetFitInView(v: boolean);
    function GetWidthInUnit: double;
    procedure SetHexLabels(v: boolean);
    procedure SetInverted(v: boolean);
    procedure SetOffsetX(v: integer);
    procedure SetOffsetY(v: integer);
  protected
    { Protected declarations }
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function GetClickedGrip(x, y: integer): integer;
    procedure GetGripPoly(gn: integer; var poly: TIEPolyg);
    procedure GetGripRect(gn: integer; var rc: TRect);
    function GetRulerHeight: integer;
    procedure RepaintGrips;
    procedure RepaintRuler;
    function GetXGripPos(gn: integer): integer;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure AdjustGripLimits(gn: integer);
    procedure AdjustViewLimits;
  public
    { Public declarations }
    procedure Paint; override;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Update; override;
    property GripsPos[g: integer]: double read GetGripsPos write SetGripsPos; default;
    property GripsColor[g: integer]: integer read GetGripsCol write SetGripsCol;
    property GripsKind[g: integer]: TGripKind read GetGripsKind write SetGripsKind;
    property GripsMin[g: integer]: double read GetGripsMin write SetGripsMin;
    property GripsMax[g: integer]: double read GetGripsMax write SetGripsMax;

{!!
<FS>TRulerBox.ScrollRate

<FM>Declaration<FC>
property ScrollRate: double;

<FM>Description<FN>
Specifies the scroll rate used when moving a grip out of ruler borders.
Default is 1.
!!}
    property ScrollRate: double read fScrollRate write fScrollRate;

    property OffsetX: integer read fOffsetX write SetOffsetX;
    property OffsetY: integer read fOffsetY write SetOffsetY;

  published
    { Published declarations }
    property ViewPos: double read fViewPos write SetViewPos;
    property Background: TColor read fBackground write SetBackground default clBtnFace;
    property GripBaseDim: integer read fGripBaseDim write SetGripBaseDim default 12;
    property GripsDir: TGripsDir read fGripsDir write SetGripsDir default gdUp;
    property Ruler: boolean read fRuler write SetRuler default true;
    property DotPerUnit: double read fDPU write SetDPU;
    property Frequency: double read fFrequency write SetFrequency;
    property LabelFreq: double read fLabelFreq write SetLabelFreq;
    property RulerColor: TColor read fRulerColor write SetRulerColor default clBtnFace;
    property RulerDir: TRulerDir read fRulerDir write SetRulerDir default rdHorizontal;
    property ViewMin: double read fViewMin write SetViewMin;
    property ViewMax: double read fViewMax write SetViewMax;

{!!
<FS>TRulerBox.MaxGripHeight

<FM>Declaration<FC>
property MaxGripHeight: integer;

<FM>Description<FN>
Specifies the maximum height of the grip.
!!}
    property MaxGripHeight: integer read fMaxGripHeight write fMaxGripHeight;

{!!
<FS>TRulerBox.OnRulerPosChange

<FM>Declaration<FC>
property OnRulerPosChange: <A TRulerPosChangeEvent>;

<FM>Description<FN>
Occurs when <A TRulerBox.ViewPos> changes.
!!}
    property OnRulerPosChange: TRulerPosChangeEvent read fOnRulerPosChange write fOnRulerPosChange;

{!!
<FS>TRulerBox.OnRulerGripClick

<FM>Declaration<FC>
property OnRulerGripClick: <A TRulerGripClickEvent>;

<FM>Description<FN>
Occurs when the user clicks on a grip.
!!}
    property OnRulerGripClick: TRulerGripClickEvent read fOnRulerGripClick write fOnRulerGripClick;

{!!
<FS>TRulerBox.OnRulerGripDblClick

<FM>Declaration<FC>
property OnRulerGripDblClick: <A TRulerGripDblClickEvent>;

<FM>Description<FN>
Occurs when the user double-click on a grip.
!!}
    property OnRulerGripDblClick: TRulerGripDblClickEvent read fOnRulerGripDblClick write fOnRulerGripDblClick;

{!!
<FS>TRulerBox.OnRulerClick

<FM>Declaration<FC>
property OnRulerClick: <A TRulerClickEvent>;

<FM>Description<FN>
OnRulerClick is called when the user clicks on the ruler.
!!}
    property OnRulerClick: TRulerClickEvent read fOnRulerClick write fOnRulerClick;

{!!
<FS>TRulerBox.Inverted

<FM>Declaration<FC>
property Inverted: boolean;

<FM>Description<FN>
When true the ruler is painted from right to left.
!!}
    property Inverted: boolean read fInverted write SetInverted default false;

    property FitInView: boolean read fFitInView write SetFitInView default false;
    property GripsCount: integer read GetGripsCount write SetGripsCount default 1;
    property HexLabels: boolean read fHexLabels write SetHexLabels default false;
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
    property Width default 300;
    property Height default 40;
    property Font;
  end;

implementation

uses hyieutils, imageenproc;

{$R-}

type

  TRUGrip = record
    Pos: double;      // position
    Color: TColor;    // color
    Kind: TGripKind;  // type
    Min: double;      // min position
    Max: double;      // max position
  end;
  PRUGrip = ^TRUGrip;

const
  // number of points for each grip type
  NumLinesGKind: array[gkTriangle..gkArrow2] of integer = (
    3, // gkTriangle
    3, // gkLeftTriangle
    3, // gkRightTriangle
    7, // gkArrow
    5  // gkArrow2
  );

  DISTRULLAB = 3; // Distance between ruler and labels

constructor TRulerBox.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  IEGDIPLoadLibrary();
  controlstyle := controlstyle + [csOpaque];
  fOffsetX := 0;
  fOffsetY := 0;
  fInverted := false;
  fHexLabels := false;
  fGripsDir := gdUp;
  fBackground := clBtnFace;
  fGripBaseDim := 12;
  fRuler := true;
  fViewPos := 0;
  fDPU := 1;
  fFrequency := 10;
  fLabelFreq := 40;
  fRulerColor := clBtnFace;
  fViewMin := 0;
  fViewMax := 0;
  fMaxGripHeight := 15;
  fOnRulerPosChange := nil;
  fOnRulerGripClick := nil;
  fOnRulerGripDblClick := nil;
  fOnRulerClick := nil;
  fFitInView := false;
  Height := 40;
  Width := 300;
  bitmap := TBitmap.create;
  bitmap.PixelFormat := pf24bit;
  bitmap.Width := Width - fOffsetX;
  bitmap.Height := Height - fOffsetY;
  fGrips := TList.Create;
  SetGripsCount(1);
  fSelGrip := -1;
  fScrollRate := 1.0;
  Update;
end;

destructor TRulerBox.Destroy;
var
  i: integer;
begin
  FreeAndNil(bitmap);
  for i := 0 to fGrips.Count-1 do
    dispose(fGrips[i]);
  FreeAndNil(fGrips);
  IEGDIPUnLoadLibrary();
  inherited;
end;

procedure TRulerBox.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := fBackground;
  Canvas.FillRect(Rect(0, 0, fOffsetX, Height));
  Canvas.FillRect(Rect(0, 0, Width, fOffsetY));
  Canvas.Draw(fOffsetX, fOffsetY, Bitmap);
end;

procedure TRulerBox.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TRulerBox.WMSize(var Message: TWMSize);
begin
  inherited;
  Bitmap.Width := message.Width - fOffsetX;
  Bitmap.Height := message.Height - fOffsetY;
  Update;
end;

{!!
<FS>TRulerBox.Background

<FM>Declaration<FC>
property Background: TColor;

<FM>Description<FN>
Background is the background color.
!!}
procedure TRulerBox.SetBackground(bk: TColor);
begin
  fBackground := bk;
  Update;
end;

procedure TRulerBox.SetGripsCount(v: integer);
var
  ex, q: integer;
begin
  ex := fGrips.Count;
  if v <> ex then
  begin
    if v < ex then
      // free
      for q := ex-1 downto v do
        dispose(fGrips[q]);

    fGrips.Count := v;

    // init
    for q := ex to v - 1 do
    begin
      fGrips[q] := new(PRUGrip);
      // defaults
      PRUGrip(fGrips[q])^.Pos   := 0;
      PRUGrip(fGrips[q])^.Color := fBackground;
      PRUGrip(fGrips[q])^.Kind  := gkTriangle;
      PRUGrip(fGrips[q])^.Min   := fViewMin;
      PRUGrip(fGrips[q])^.Max   := fViewMax;
    end;

    Update;
  end;
end;

{!!
<FS>TRulerBox.GripsCount

<FM>Declaration<FC>
property GripsCount: integer;

<FM>Description<FN>
GripsCount gets/sets number of the grips. 

!!}
function TRulerBox.GetGripsCount: integer;
begin
  result := fGrips.Count;
end;

procedure TRulerBox.SetGripsPos(i: integer; p: double);
begin
  if (i < fGrips.Count) then
  begin
    PRUGrip(fGrips[i])^.Pos := p;
    AdjustGripLimits(i);
    if assigned(fOnRulerPosChange) then
      fOnRulerPosChange(self, i);
    Update;
    Paint;
  end;
end;

{!!
<FS>TRulerBox.GripsPos

<FM>Declaration<FC>
property GripsPos[g: integer]: double;

<FM>Description<FN>
GripsPos is the current position of the grip g.
!!}
function TRulerBox.GetGripsPos(i: integer): double;
begin
  if i < fGrips.Count then
    result := PRUGrip(fGrips[i])^.Pos
  else
    result := 0;
end;

{!!
<FS>TRulerBox.GripsColor

<FM>Declaration<FC>
property GripsColor[g: integer]: integer;

<FM>Description<FN>
GripsColor sets color of the grip g.

!!}
procedure TRulerBox.SetGripsCol(i, p: integer);
begin
  if i < fGrips.Count then
  begin
    PRUGrip(fGrips[i])^.Color := p;
    Update;
  end;
end;

function TRulerBox.GetGripsCol(i: integer): integer;
begin
  if i < fGrips.Count then
    result := PRUGrip(fGrips[i])^.Color
  else
    result := 0;
end;

{!!
<FS>TRulerBox.GripBaseDim

<FM>Declaration<FC>
property GripBaseDim: integer;

<FM>Description<FN>
Base of the grip triangles in pixels.
!!}
procedure TRulerBox.SetGripBaseDim(v: integer);
begin
  fGripBaseDim := v;
  Update;
end;

procedure TRulerBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  dec(X, fOffsetX);
  dec(Y, fOffsetY);
  if (Button = mbLeft) then
  begin
    fSelGrip := GetClickedGrip(x, y);
    fMX1 := X;
    fMY1 := Y;
    if fSelGrip >= 0 then
    begin
      if (ssDouble in Shift) and assigned(fOnRulerGripDblClick) then
        fOnRulerGripDblClick(Self, fSelGrip);
      fSelGripSt := GetXGripPos(fSelGrip);
    end
    else
    begin
      if assigned(fOnRulerClick) then
        fOnRulerClick(self, X / fDPU + fViewPos);
    end;
  end;
end;

procedure TRulerBox.SetGripsKind(i: integer; v: TGripKind);
begin
  if i < fGrips.Count then
    PRUGrip(fGrips[i])^.Kind := v;
  Update;
end;

{!!
<FS>TRulerBox.GripsKind

<FM>Declaration<FC>
property GripsKind[g: integer]: <A TGripKind>;

<FM>Description<FN>
GripsKind is the kind of the grip g (triangle, arrow, etc).

!!}
function TRulerBox.GetGripsKind(i: integer): TGripKind;
begin
  result := gkTriangle; // default
  if i < fGrips.Count then
    result := PRUGrip(fGrips[i])^.Kind;
end;

// return -1 = none
function TRulerBox.GetClickedGrip(x, y: integer): integer;
var
  q: integer;
  rc: TRect;
begin
  result := -1;
  for q := fGrips.Count - 1 downto 0 do
  begin
    GetGripRect(q, rc);
    if PtInRect(rc, Point(x, y)) then
    begin
      result := q;
      break;
    end;
  end;
end;

// returns the rectangle where the grip "gn" is contained.
// Right and bottom borders are incremented by 1 to make them compatible with PtInRect.
procedure TRulerBox.GetGripRect(gn: integer; var rc: TRect);
var
  poly: TIEPolyg;
  q: integer;
begin
  GetGripPoly(gn, poly);
  rc.TopLeft := poly[0];
  rc.BottomRight := poly[0];
  for q := 1 to NumLinesGKind[GripsKind[gn]] - 1 do
  begin
    if poly[q].X < rc.Left then
      rc.Left := poly[q].X;
    if poly[q].X > rc.Right then
      rc.Right := poly[q].X;
    if poly[q].Y < rc.Top then
      rc.Top := poly[q].Y;
    if poly[q].Y > rc.Bottom then
      rc.Bottom := poly[q].Y;
  end;
  inc(rc.Bottom);
  inc(rc.Right);
end;

{!!
<FS>TRulerBox.GripsDir

<FM>Declaration<FC>
property GripsDir: <A TGripsDir>;

<FM>Description<FN>
GripsDir is the direction of the grips (up or down).
!!}
procedure TRulerBox.SetGripsDir(v: TGripsDir);
begin
  fGripsDir := v;
  Update;
end;

{!!
<FS>TRulerBox.Ruler

<FM>Declaration<FC>
property Ruler: boolean;

<FM>Description<FN>
If True, the ruler is shown.
!!}
procedure TRulerBox.SetRuler(v: boolean);
begin
  fRuler := v;
  Update;
end;

// return height of ruler+text
function TRulerBox.GetRulerHeight: integer;
begin
  if fRuler then
  begin
    if fRulerDir = rdHorizontal then
      result := trunc(abs(Font.Height) * 1.7) + DISTRULLAB
    else
      result := Bitmap.Width - fMaxGripHeight;
  end
  else
    result := 0;
end;

procedure TRulerBox.Update;
begin
  if (fFitinView) then
  begin
    if (fViewMax-fViewMin<>0) then
      fDPU := (Bitmap.Width) / (fViewMax - fViewMin);
    AdjustViewLimits;
  end;
  RepaintGrips;
  RepaintRuler;
  invalidate;
end;

{!!
<FS>TRulerBox.ViewPos

<FM>Declaration<FC>
property ViewPos: double;

<FM>Description<FN>
ViewPos is the initial position of viewing.
!!}
procedure TRulerBox.SetViewPos(v: double);
begin
  fViewPos := v;
  AdjustViewLimits;
  Update;
  Paint;
end;

{!!
<FS>TRulerBox.Frequency

<FM>Declaration<FC>
property Frequency: double;

<FM>Description<FN>
Frequency is the number of logical units where the ticks are shown.
!!}
procedure TRulerBox.SetFrequency(v: double);
begin
  fFrequency := v;
  Update;
  Paint;
end;

{!!
<FS>TRulerBox.LabelFreq

<FM>Declaration<FC>
property LabelFreq: double;

<FM>Description<FN>
LabelFreq is the number of logical units where to show the labels.
!!}
procedure TRulerBox.SetLabelFreq(v: double);
begin
  fLabelFreq := v;
  Update;
  Paint;
end;

// returns X position (in pixels) of grip "gn"
function TRulerBox.GetXGripPos(gn: integer): integer;
begin
  result := round(fDPU * (GripsPos[gn] - fViewPos));
  if fInverted then
  begin
    if fRulerDir = rdHorizontal then
      result := (Bitmap.Width) - result
    else
      result := (Bitmap.Height) - result;
  end;
end;

{!!
<FS>TRulerBox.RulerColor

<FM>Declaration<FC>
property RulerColor: TColor;

<FM>Description<FN>
Color of the ruler.
!!}
procedure TRulerBox.SetRulerColor(v: TColor);
begin
  fRulerColor := v;
  Update;
end;

procedure TRulerBox.CMFontChanged(var Message: TMessage);
begin
  Update;
end;

{!!
<FS>TRulerBox.RulerDir

<FM>Declaration<FC>
property RulerDir: <A TRulerDir>;

<FM>Description<FN>
Direction of the ruler (horizontal or vertical).
!!}
procedure TRulerBox.SetRulerDir(v: TRulerDir);
begin
  fRulerDir := v;
  Update;
end;

// returns coordinates of points making the grip
procedure TRulerBox.GetGripPoly(gn: integer; var poly: TIEPolyg);
var
  w, h, b: integer;
begin
  b := 0;
  h := 0;
  if csDesigning in Componentstate then
    w := (fGripBaseDim + 5) * gn
  else
    w := GetXGripPos(gn);
  if fGripsDir = gdUp then
  begin
    if fRulerDir = rdHorizontal then
      h := Bitmap.Height - 1
    else
      h := Bitmap.Width - 1;
    b := 0;
    if fRuler then
      inc(b, GetRulerHeight);
  end;
  if fGripsDir = gdDown then
  begin
    if fRulerDir = rdHorizontal then
      b := Bitmap.Height - 1
    else
      b := Bitmap.Width - 1;
    h := 0;
    if fRuler then
      dec(b, GetRulerHeight);
  end;
  case GripsKind[gn] of
    gkTriangle:
      begin
        poly[0].x := w - (fGripBaseDim div 2);
        poly[0].y := h;
        poly[1].x := w + (fGripBaseDim div 2);
        poly[1].y := h;
        poly[2].x := w;
        poly[2].y := b;
      end;
    gkLeftTriangle:
      begin
        poly[0].x := w - (fGripBaseDim div 2);
        poly[0].y := h;
        poly[1].x := w;
        poly[1].y := h;
        poly[2].x := w;
        poly[2].y := b;
      end;
    gkRightTriangle:
      begin
        poly[0].x := w;
        poly[0].y := h;
        poly[1].x := w + (fGripBaseDim div 2);
        poly[1].y := h;
        poly[2].x := w;
        poly[2].y := b;
      end;
    gkArrow:
      begin
        poly[0].x := w - (fGripBaseDim div 4);
        poly[0].y := h;
        poly[1].x := w + (fGripBaseDim div 4);
        poly[1].y := poly[0].y;
        poly[2].x := poly[1].x;
        if fGripsDir = gdUp then
          poly[2].y := b + ((h - b) div 3)
        else
          poly[2].y := h + ((b - h) div 3) * 2;
        poly[3].x := w + (fGripBaseDim div 2);
        poly[3].y := poly[2].y;
        poly[4].x := w;
        poly[4].y := b;
        poly[5].x := w - (fGripBaseDim div 2);
        poly[5].y := poly[2].y;
        poly[6].x := poly[0].x;
        poly[6].y := poly[2].y;
      end;
    gkArrow2:
      begin
        poly[0].x := w - (fGripBaseDim div 2);
        poly[0].y := h;
        poly[1].x := w + (fGripBaseDim div 2);
        poly[1].y := poly[0].y;
        poly[2].x := poly[1].x;
        if fGripsDir = gdUp then
          poly[2].y := b + (h - b) div 2
        else
          poly[2].y := h + (b - h) div 2;
        poly[3].x := w;
        poly[3].y := b;
        poly[4].x := poly[0].x;
        poly[4].y := poly[2].y;
      end;
  end;
  if fRulerDir = rdVertical then
    for w := 0 to NumLinesGKind[GripsKind[gn]] - 1 do
      iswap(poly[w].x, poly[w].y);
end;

procedure TRulerBox.RepaintGrips;
var
  q, rh: integer;
  poly: TIEPolyg;
  iec: TIECanvas;
begin
  iec := TIECanvas.Create(Bitmap.Canvas, true, true);
  iec.Brush.Color := fBackground;
  rh := GetRulerHeight;
  if fRulerDir = rdHorizontal then
  begin
    if fGripsDir = gdUp then
      iec.Fillrect(rect(0, rh, Bitmap.Width, Bitmap.Height))
    else
    if fgripsDir = gdDown then
      iec.FillRect(rect(0, 0, Bitmap.Width, Bitmap.Height - rh));
  end
  else
  begin
    if fGripsDir = gdUp then
      iec.FillRect(rect(rh, 0, Bitmap.Width, Bitmap.Height))
    else
    if fgripsDir = gdDown then
      iec.FillRect(rect(0, 0, Bitmap.Width - rh, Bitmap.Height));
  end;

  for q := 0 to fGrips.Count - 1 do
  begin
    GetGripPoly(q, poly);
    // paints grip
    iec.Brush.Color := GripsColor[q];
    iec.Pen.Color := clBlack;
    iec.Polygon(slice(poly, NumLinesGKind[GripsKind[q]]));
  end;
  iec.Free;
end;

// fMouseSel 0=none  1=capture on sat/val  2=capture on hue
procedure TRulerBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  dec(X, fOffsetX);
  dec(Y, fOffsetY);
  //
  if fSelGrip >= 0 then
  begin
    if (fMX1 = X) and (fMY1 = Y) and assigned(fOnRulerGripClick) then
      fOnRulerGripClick(self, fSelGrip);
  end;
end;

// adjusts limits of specified grip (looking at Max and Min)
procedure TRulerBox.AdjustGripLimits(gn: integer);
begin
  if GripsMin[gn] < GripsMax[gn] then
  begin
    if GripsPos[gn] < GripsMin[gn] then
      GripsPos[gn] := GripsMin[gn]
    else
    if GripsPos[gn] > GripsMax[gn] then
      GripsPos[gn] := GripsMax[gn];
  end;
end;

// return horizontal size in Units
function TRulerBox.GetWidthInUnit: double;
begin
  result := (Bitmap.Width) / fDPU;
end;

// adjusts limits of fViewPos (looking at fViewMin and fViewMax)
procedure TRulerBox.AdjustViewLimits;
begin
  if fViewMin < fViewMax then
  begin
    if fViewPos < fViewMin then
      fViewPos := fViewMin
    else
    if fViewPos > (fViewMax - GetWidthInUnit) then
      fViewpos := dmax(0, fViewMax - GetWidthInUnit);
  end;
end;

procedure TRulerBox.MouseMove(Shift: TShiftState; X, Y: Integer);
  function ScrollLeft: integer;
  var
    p: TPoint;
  begin
    GetCursorPos(p);
    p := ScreenToClient(p);
    PRUGrip(fGrips[fSelGrip])^.Pos := (fSelGripSt - fMX1) / fDPU + fViewPos - (fScrollRate / fDPU);
    AdjustGripLimits(fSelGrip);
    fViewPos := fViewPos - (fScrollRate / fDPU);
    AdjustViewLimits;
    RepaintGrips;
    RepaintRuler;
    Paint;
    if assigned(fOnRulerPosChange) then
      fOnRulerPosChange(self, fSelGrip);
    result := p.X;
  end;
  function ScrollRight: integer;
  var
    p: TPoint;
  begin
    GetCursorPos(p);
    p := ScreenToClient(p);
    PRUGrip(fGrips[fSelGrip])^.Pos := (fSelGripSt + bitmap.Width - 1 - fMX1) / fDPU + fViewPos + (fScrollRate / fDPU);
    AdjustGripLimits(fSelGrip);
    fViewPos := fViewPos + (fScrollRate / fDPU);
    AdjustViewLimits;
    RepaintGrips;
    RepaintRuler;
    Paint;
    if assigned(fOnRulerPosChange) then
      fOnRulerPosChange(self, fSelGrip);
    result := p.X;
  end;
  function ScrollUp: integer;
  var
    p: TPoint;
  begin
    GetCursorPos(p);
    p := ScreenToClient(p);
    PRUGrip(fGrips[fSelGrip])^.Pos := (fSelGripSt - fMY1) / fDPU + fViewPos - (fScrollRate / fDPU);
    AdjustGripLimits(fSelGrip);
    fViewPos := fViewPos - (fScrollRate / fDPU);
    AdjustViewLimits;
    RepaintGrips;
    RepaintRuler;
    Paint;
    if assigned(fOnRulerPosChange) then
      fOnRulerPosChange(self, fSelGrip);
    result := p.Y;
  end;
  function ScrollDown: integer;
  var
    p: TPoint;
  begin
    GetCursorPos(p);
    p := ScreenToClient(p);
    PRUGrip(fGrips[fSelGrip])^.Pos := (fSelGripSt + Bitmap.Height- 1 - fMY1) / fDPU + fViewPos + (fScrollRate / fDPU);
    AdjustGripLimits(fSelGrip);
    fViewPos := fViewPos + (fScrollRate / fDPU);
    AdjustViewLimits;
    RepaintGrips;
    RepaintRuler;
    Paint;
    if assigned(fOnRulerPosChange) then
      fOnRulerPosChange(self, fSelGrip);
    result := p.Y;
  end;
begin
  inherited;
  //
  dec(X, fOffsetX);
  dec(Y, fOffsetY);
  if MouseCapture then
    if (fSelGrip >= 0) then
    begin
      if fRulerDir = rdHorizontal then
      begin
        if fInverted then
        begin
          if X < 0 then
            while (ScrollRight() < 0) and (GetAsyncKeyState(VK_LBUTTON) <> 0) do begin end
          else
          if X >= Bitmap.Width then
            while (ScrollLeft() >= Bitmap.Width) and (GetAsyncKeyState(VK_LBUTTON) <> 0) do begin end
          else
          begin
            SetGripsPos(fSelGrip, (fSelGripSt + (Bitmap.Width-X) - fMX1) / fDPU + fViewPos);
            Paint;
          end;
        end
        else
        begin
          if X < 0 then
            while (ScrollLeft() < 0) and (GetAsyncKeyState(VK_LBUTTON) <> 0) do begin end
          else
          if X >= Bitmap.Width then
            while (ScrollRight() >= Bitmap.Width) and (GetAsyncKeyState(VK_LBUTTON) <> 0) do begin end
          else
          begin
            SetGripsPos(fSelGrip, (fSelGripSt + X - fMX1) / fDPU + fViewPos);
            Paint;
          end;
        end
      end
      else
      begin
        if fInverted then
        begin
          if Y < 0 then
            while (ScrollDown() < 0) and (GetAsyncKeyState(VK_LBUTTON) <> 0) do begin end
          else
          if Y >= Bitmap.Height then
            while (ScrollUp() >= Bitmap.height) and (GetAsyncKeyState(VK_LBUTTON) <> 0) do begin end
          else
          begin
            SetGripsPos(fSelGrip, (fSelGripSt + (Bitmap.Height-Y) - fMY1) / fDPU + fViewPos);
            Paint;
          end;
        end
        else
        begin
          if Y < 0 then
            while (ScrollUp() < 0) and (GetAsyncKeyState(VK_LBUTTON) <> 0) do begin end
          else
          if Y >= Bitmap.Height then
            while (ScrollDown() >= Bitmap.height) and (GetAsyncKeyState(VK_LBUTTON) <> 0) do begin end
          else
          begin
            SetGripsPos(fSelGrip, (fSelGripSt + Y - fMY1) / fDPU + fViewPos);
            Paint;
          end;
        end
      end;
    end;
end;

procedure TRulerBox.SetGripsMax(i: integer; v: double);
var
  q: integer;
begin
  if (i < fGrips.Count) then
  begin
    PRUGrip(fGrips[i])^.Max := v;
    for q := 0 to fGrips.Count - 1 do
      AdjustGripLimits(q);
    Update;
  end;
end;

procedure TRulerBox.SetGripsMin(i: integer; v: double);
var
  q: integer;
begin
  if (i < fGrips.Count) then
  begin
    PRUGrip(fGrips[i])^.Min := v;
    for q := 0 to fGrips.Count - 1 do
      AdjustGripLimits(q);
    Update;
  end;
end;

{!!
<FS>TRulerBox.GripsMax

<FM>Declaration<FC>
property GripsMax[g: integer]: double;

<FM>Description<FN>
GripsMax is the max value of the grip g.

!!}
function TRulerBox.GetGripsMax(i: integer): double;
begin
  if i < fGrips.Count then
    result := PRUGrip(fGrips[i])^.Max
  else
    result := 0;
end;

{!!
<FS>TRulerBox.GripsMin

<FM>Declaration<FC>
property GripsMin[g: integer]: double;

<FM>Description<FN>
GripsMin is the minimal value of the grip g.

!!}
function TRulerBox.GetGripsMin(i: integer): double;
begin
  if i < fGrips.Count then
    result := PRUGrip(fGrips[i])^.Min
  else
    result := 0;
end;

{!!
<FS>TRulerBox.ViewMin

<FM>Declaration<FC>
property ViewMin: double;

<FM>Description<FN>
Minimal value for ViewPos property.
!!}
procedure TRulerBox.SetViewMin(v: double);
var
  q: integer;
begin
  fViewMin := v;
  AdjustViewLimits;
  for q := 0 to fGrips.Count - 1 do
    PRUGrip(fGrips[q])^.Min := fViewMin;
  Update;
end;

{!!
<FS>TRulerBox.ViewMax

<FM>Declaration<FC>
property ViewMax: double;

<FM>Description<FN>
Maximum value for <A TRulerBox.ViewPos> property.
!!}
procedure TRulerBox.SetViewMax(v: double);
var
  q: integer;
begin
  fViewMax := v;
  AdjustViewLimits;
  for q := 0 to fGrips.Count - 1 do
    PRUGrip(fGrips[q])^.Max := fViewMax;
  Update;
end;

// repaint ruler+text
procedure TRulerBox.RepaintRuler;
var
  z, v: double;   // pos in unit
  x, y: integer;  // pos in pixel
  rl: integer;    // ruler height
  fh: integer;    // font height (abs)
  lb1, lb: string;
  xx, yy: integer;
  LogFont: TLogFontA;
  SaveFont: TFont;
  q, w, e: integer;
begin
  if not fRuler then
    exit;
  Bitmap.Canvas.Brush.Color := fRulerColor;
  rl := GetRulerHeight;
  fh := abs(Font.Height);
  if fRulerDir = rdHorizontal then
  begin
    if fGripsDir = gdDown then
      Bitmap.Canvas.Fillrect(Rect(0, Bitmap.Height - rl, Bitmap.Width, Bitmap.Height))
    else
      Bitmap.Canvas.Fillrect(Rect(0, 0, Bitmap.Width, rl + 1));
  end
  else
  begin
    if fGripsDir = gdDown then
      Bitmap.Canvas.Fillrect(Rect(Bitmap.Width - rl, 0, Bitmap.Width, Bitmap.Height))
    else
      Bitmap.Canvas.Fillrect(Rect(0, 0, rl, Bitmap.Height));
  end;
  // paint
  Bitmap.Canvas.Pen.Color := Font.Color;
  Bitmap.Canvas.Font := Font;
  if fRulerDir = rdVertical then
  begin
    // VERTICAL
    // pepare vertical font
    SaveFont := TFont.Create;
    SaveFont.assign(Bitmap.Canvas.Font);
    GetObject(SaveFont.Handle, sizeof(TLogFontA), @LogFont);
    if fGripsDir = gdDown then
      LogFont.lfEscapement := -900 // -90 degrees
    else
      LogFont.lfEscapement := 900; // 90 degrees
    LogFont.lfPitchAndFamily := FIXED_PITCH or FF_DONTCARE;
    Bitmap.Canvas.Font.Handle := CreateFontIndirectA(LogFont);
    //
    // paint little row
    w := trunc(((Bitmap.Height) / fDPU) / fFrequency); // number of little rows
    for q := 0 to w + 1 do
    begin
      v := ((fViewPos / fFrequency) - trunc(fViewPos / fFrequency)) * fFrequency;
      z := q * fFrequency + fViewPos - v;

      y := round((z - fViewPos) * fDPU);
      if fInverted then
        y := (Bitmap.Height) - y;

      if fGripsDir = gdDown then
      begin // DOWN
        Bitmap.Canvas.MoveTo(Bitmap.Width - rl, y);
        bitmap.Canvas.LineTo(Bitmap.Width - rl + (rl div 4), y);
      end
      else
      begin // UP
        Bitmap.Canvas.MoveTo(rl, y);
        Bitmap.Canvas.LineTo(rl - (rl div 4), y);
      end;
    end;
    // paint long row
    w := trunc((Bitmap.Height / fDPU) / fLabelFreq); // number of long rows
    for q := 0 to w + 1 do
    begin
      v := ((fViewPos / fLabelFreq) - trunc(fViewPos / fLabelFreq)) * fLabelFreq;
      z := q * fLabelFreq + fViewPos - v;
      if fHexLabels then
      begin
        lb1 := IntToHex(trunc(abs(z)), 6);
        // remove zeros
        lb := '0';
        for e := 1 to length(lb1) do
          if lb1[e] <> '0' then
          begin
            lb := Copy(lb1, e, length(lb1));
            break;
          end;
        if z < 0 then
          lb := '-' + lb;
      end
      else
        lb := FloatToStrF(round(z*1000)/1000, ffGeneral, 15, 3);
      y := round((z - fViewPos) * fDPU);

      if fInverted then
        y := Bitmap.Height - y;

      if fGripsDir = gdDown then
      begin // DOWN
        yy := y - (Bitmap.Canvas.TextWidth(lb) div 2);
        Bitmap.Canvas.MoveTo(Bitmap.Width - rl, y);
        bitmap.Canvas.LineTo(Bitmap.Width - rl + (rl div 2), y);
        Bitmap.Canvas.TextOut(Bitmap.Width, yy, lb);
      end
      else
      begin // UP
        yy := y - (Bitmap.Canvas.TextHeight(lb) div 2);
        Bitmap.Canvas.MoveTo(rl, y);
        bitmap.Canvas.LineTo(rl - (rl div 2), y);
        Bitmap.Canvas.TextOut(0, yy, lb);
      end;
    end;
    // free font
    Bitmap.Canvas.Font.Assign(SaveFont);
    FreeAndNil(SaveFont);
  end
  else
  begin
    // HORIZONTAL
    // Paint little line
    w := trunc((Bitmap.Width / fDPU) / fFrequency); // number of little lines
    for q := 0 to w + 1 do
    begin
      v := ((fViewPos / fFrequency) - trunc(fViewPos / fFrequency)) * fFrequency;
      z := q * fFrequency + fViewPos - v;
      
      x := round((z - fViewPos) * fDPU);
      if fInverted then
        x := Bitmap.Width - x;

      if fGripsDir = gdDown then
      begin // DOWN
        Bitmap.Canvas.MoveTo(x, Bitmap.Height - rl);
        bitmap.Canvas.LineTo(x, Bitmap.Height - rl + (rl div 4));
      end
      else
      begin // UP
        Bitmap.Canvas.MoveTo(x, rl);
        bitmap.Canvas.LineTo(x, rl - (rl div 4));
      end;
    end;
    // Paint long line and text
    w := trunc((Bitmap.Width / fDPU) / fLabelFreq); // number of long lines
    for q := 0 to w + 1 do
    begin
      v := ((fViewPos / fLabelFreq) - trunc(fViewPos / fLabelFreq)) * fLabelFreq;
      z := q * fLabelFreq + fViewPos - v;
      if fHexLabels then
      begin
        lb1 := IntToHex(trunc(abs(z)), 6);
        // remove starting zeros
        lb := '0';
        for e := 1 to length(lb1) do
          if lb1[e] <> '0' then
          begin
            lb := Copy(lb1, e, length(lb1));
            break;
          end;
        if z < 0 then
          lb := '-' + lb;
      end
      else
        lb := FloatToStrF(round(z*1000)/1000, ffGeneral, 15, 3);

      x := round((z - fViewPos) * fDPU);
      if fInverted then
        x := Bitmap.Width - x;
      xx := x - (Bitmap.Canvas.TextWidth(lb) div 2);

      if fGripsDir = gdDown then
      begin // DOWN
        Bitmap.Canvas.MoveTo(x, Bitmap.Height - rl);
        Bitmap.Canvas.LineTo(x, Bitmap.Height - rl + (rl div 2));
        Bitmap.Canvas.TextOut(xx, Bitmap.Height - fh, lb);
      end
      else
      begin // UP
        Bitmap.Canvas.MoveTo(x, rl);
        Bitmap.Canvas.LineTo(x, rl - (rl div 2));
        Bitmap.Canvas.TextOut(xx, 0, lb);
      end;
    end;
  end;
end;

{!!
<FS>TRulerBox.FitInView

<FM>Declaration<FC>
property FitInView: boolean;

<FM>Description<FN>
If True, it specifies Automatic adjustment of DotPerUnit to fit ViewMin and ViewMax.

Adjusts DPU property using values of ViewMin and ViewMax.
!!}
procedure TRulerBox.SetFitInView(v: boolean);
begin
  fFitInView := v;
  Update;
end;

{!!
<FS>TRulerBox.DotPerUnit

<FM>Declaration<FC>
property DotPerUnit: double;

<FM>Description<FN>
Pixel for logical unit.
!!}
procedure TRulerBox.SetDPU(v: double);
begin
  if (csDesigning in Componentstate) and fFitInView then
    exit;
  fDPU := v;
  Update;
  Paint;
end;

{!!
<FS>TRulerBox.HexLabels

<FM>Declaration<FC>
property HexLabels: boolean;

<FM>Description<FN>
If HexLabels is True, the labels are shown in hexadecimal notation.
!!}
procedure TRulerBox.SetHexLabels(v: boolean);
begin
  fHexLabels := v;
  Update;
end;

procedure TRulerBox.SetInverted(v: boolean);
begin
  fInverted := v;
  Update;
end;

{!!
<FS>TRulerBox.OffsetX

<FM>Declaration<FC>
property OffsetX: integer;

<FM>Description<FN>
Specifies the horizontal offset where the ruler begins. Default is 0.
!!}
procedure TRulerBox.SetOffsetX(v: integer);
begin
  fOffsetX := v;
  Bitmap.Width := Width - fOffsetX;
  Update();
end;

{!!
<FS>TRulerBox.OffsetY

<FM>Declaration<FC>
property OffsetY: integer;

<FM>Description<FN>
Specifies the vertical offset where the ruler begins. Default is 0.
!!}
procedure TRulerBox.SetOffsetY(v: integer);
begin
  fOffsetY := v;
  Bitmap.Height := Height - fOffsetY;
  Update();
end;


end.




