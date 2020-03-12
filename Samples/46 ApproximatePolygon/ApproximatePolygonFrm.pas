unit ApproximatePolygonFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.ScrollBox, FMX.Memo,

  CoreClasses, PascalStrings, UnicodeMixedLib, Geometry2DUnit, MemoryRaster,
  MemoryStream64, DoStatusIO, zDrawEngine, zDrawEngineInterface_FMX;

type
  TApproximatePolygonForm = class(TForm)
    pb1: TPaintBox;
    pb2: TPaintBox;
    Memo1: TMemo;
    Timer1: TTimer;
    HausdorfDisanceButton: TButton;
    Button1: TButton;
    Button2: TButton;
    CustomHausdorfDisanceButton: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pb1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure pb1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure pb1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure pb1Paint(Sender: TObject; Canvas: TCanvas);
    procedure pb2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure pb2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure pb2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure pb2Paint(Sender: TObject; Canvas: TCanvas);
    procedure HausdorfDisanceButtonClick(Sender: TObject);
    procedure CustomHausdorfDisanceButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    dIntf: TDrawEngineInterface_FMX;
    Down1, Down2: Boolean;
    poly1, poly2: T2DPolygon;
    procedure DoStatusMethod(Text_: SystemString; const ID: Integer);
  public
  end;

var
  ApproximatePolygonForm: TApproximatePolygonForm;

implementation

{$R *.fmx}


procedure TApproximatePolygonForm.Button1Click(Sender: TObject);
begin
  poly2.Assign(poly1);
end;

procedure TApproximatePolygonForm.Button2Click(Sender: TObject);
begin
  poly1.Assign(poly2);
end;

procedure TApproximatePolygonForm.Button3Click(Sender: TObject);
begin
  poly1.SwapData(poly2);
end;

procedure TApproximatePolygonForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  dIntf := TDrawEngineInterface_FMX.Create;
  Down1 := False;
  Down2 := False;
  poly1 := T2DPolygon.Create;
  poly2 := T2DPolygon.Create;
end;

procedure TApproximatePolygonForm.pb1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  DoStatus('重置多边形');
  poly1.Clear;
  Down1 := True;
  poly1.Add(Vec2(X, Y));
end;

procedure TApproximatePolygonForm.pb1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if not Down1 then
      exit;
  poly1.Add(Vec2(X, Y));
end;

procedure TApproximatePolygonForm.pb1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  num: Integer;
begin
  if not Down1 then
      exit;
  Down1 := False;
  poly1.Add(Vec2(X, Y));
  num := poly1.Count;
  poly1.Reduction(15.0);
  DoStatus('优化过的多边形顶点 %d -> %d', [num, poly1.Count]);
end;

procedure TApproximatePolygonForm.pb1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
begin
  dIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, dIntf);
  d.ViewOptions := [voEdge];
  d.EdgeColor := DEColor(1, 0, 0);
  d.FillBox;

  if Down1 then
      d.DrawPL(False, poly1, True, DEColor(0.5, 0.5, 0.5), 2)
  else
      d.DrawPL(False, poly1, True, DEColor(1, 1, 1), 2);

  d.DrawText('在这里用鼠标绘制多边形', 14, DEColor(1, 1, 1), Vec2(0, 0));
  d.Flush;
end;

procedure TApproximatePolygonForm.pb2MouseDown(Sender: TObject; Button:
  TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  DoStatus('重置多边形');
  poly2.Clear;
  Down2 := True;
  poly2.Add(Vec2(X, Y));
end;

procedure TApproximatePolygonForm.pb2MouseMove(Sender: TObject; Shift:
  TShiftState; X, Y: Single);
begin
  if not Down2 then
      exit;
  poly2.Add(Vec2(X, Y));
end;

procedure TApproximatePolygonForm.pb2MouseUp(Sender: TObject; Button:
  TMouseButton; Shift: TShiftState; X, Y: Single);
var
  num: Integer;
begin
  if not Down2 then
      exit;
  Down2 := False;
  poly2.Add(Vec2(X, Y));
  num := poly2.Count;
  poly2.Reduction(15.0);
  DoStatus('优化过的多边形顶点 %d -> %d', [num, poly2.Count]);
end;

procedure TApproximatePolygonForm.pb2Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
begin
  dIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, dIntf);
  d.ViewOptions := [voEdge];
  d.EdgeColor := DEColor(1, 0, 0);
  d.FillBox;

  if Down2 then
      d.DrawPL(False, poly2, True, DEColor(0.5, 0.5, 0.5), 2)
  else
      d.DrawPL(False, poly2, True, DEColor(1, 1, 1), 2);

  d.DrawText('在这里用鼠标绘制多边形', 14, DEColor(1, 1, 1), Vec2(0, 0));
  d.Flush;
end;

procedure TApproximatePolygonForm.HausdorfDisanceButtonClick(Sender: TObject);
begin
  if poly1.Count > 0 then
    if poly2.Count > 0 then
        DoStatus('相似性: %f', [THausdorf.Compute(poly1, poly2, 0, 0.0001)]);
end;

procedure TApproximatePolygonForm.CustomHausdorfDisanceButtonClick(Sender: TObject);
var
  buff: TArrayVec2;
  v2: TVec2;
begin
  if poly1.Count > 0 then
    if poly2.Count > 0 then
      with THausdorf.Create(poly1, poly2, 0, 0.0001) do
        begin
          DoStatus('The distance is reached on the following vectors:');
          buff := HausdorffReached();
          for v2 in buff do
              DoStatus('%f %f', [v2[0], v2[1]]);
          SetLength(buff, 0);

          DoStatus('Hausdorff distance: %f', [HausdorffDistance]);
          DoStatus('The mutual position of the polygons is optimal: %s', [umlBoolToStr(polygonsIsOptimal).Text]);
          Free;
        end;
end;

procedure TApproximatePolygonForm.Timer1Timer(Sender: TObject);
begin
  DoStatus;
  EnginePool.Progress();
  Invalidate;
end;

procedure TApproximatePolygonForm.DoStatusMethod(Text_: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(Text_);
  Memo1.GoToTextEnd;
end;

end.
