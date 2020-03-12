unit RectRotationProjectionFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.ScrollBox, FMX.Memo,

  CoreClasses, PascalStrings, UnicodeMixedLib, Geometry2DUnit, MemoryRaster,
  MemoryStream64, DoStatusIO, zDrawEngine, zDrawEngineInterface_FMX;

type
  TRectRotationProjectionForm = class(TForm)
    Timer1: TTimer;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Memo1: TMemo;
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure PaintBox2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    dIntf: TDrawEngineInterface_FMX;
    destBox: TRectV2; // 目标投影框
    a: TDEFloat;      // 目标投影框的旋转角度
    destPt: TVec2;    // 位于目标投影框中的坐标
    constructor Create(AOwner: TComponent); override;
  end;

var
  RectRotationProjectionForm: TRectRotationProjectionForm;

implementation

{$R *.fmx}


procedure TRectRotationProjectionForm.Timer1Timer(Sender: TObject);
begin
  // 绘图引擎主循环
  a := NormalizeDegAngle(a + EnginePool.Progress() * 5);
  Invalidate;
end;

constructor TRectRotationProjectionForm.Create(AOwner: TComponent);
begin
  inherited;
  dIntf := TDrawEngineInterface_FMX.Create;
  destBox := RectV2(0, 0, 100, 200);
  destPt := Vec2(250, 250);
  a := 15;
end;

procedure TRectRotationProjectionForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  destPt := Vec2(X, Y);
end;

procedure TRectRotationProjectionForm.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
begin
  dIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, dIntf);
  d.ViewOptions := [voEdge];
  d.FillBox();
  d.DrawPoint(destPt, DEColor(1, 0.5, 0, 5), 20, 2);
  d.DrawText('|color(0.5,1,0.5)|在此处移动鼠标正投影', 14, d.ScreenRectV2, DEColor(1, 1, 1), False);
  d.Flush;
end;

procedure TRectRotationProjectionForm.PaintBox2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  d: TDrawEngine;
  r: TRectV2;
  v: TVec2;
begin
  d := DrawPool(Sender);
  r := destBox;
  r := RectAdd(r, Vec2((d.width - RectWidth(r)) * 0.5, (d.Height - RectHeight(r)) * 0.5));

  v := Vec2(X, Y);
  with TV2R4.Init(r, a) do
    if not InHere(Vec2(X, Y)) then
        v := GetNear(v);
  // 反投影
  destPt := RectProjectionRotationSource(r, RectV2(0, 0, 500, 500), a, v);
end;

procedure TRectRotationProjectionForm.PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  r: TRectV2;
begin
  dIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, dIntf);
  d.ViewOptions := [voEdge];
  d.FillBox();
  // 把目标投影框画到中央位置
  r := destBox;
  r := RectAdd(r, Vec2((d.width - RectWidth(r)) * 0.5, (d.Height - RectHeight(r)) * 0.5));
  d.DrawDotLineBox(r, RectCentre(r), a, DEColor(1, 1, 1), 2);
  // 方便区分，正投影我们用函数 RectProjectionRotationDest
  d.DrawPoint(RectProjectionRotationDest(RectV2(0, 0, 500, 500), r, a, destPt), DEColor(1, 0.5, 0, 5), 20, 2);
  // 另一种通用的正投影的函数 RectRotationProjection 也能达到同样的目的
  // d.DrawPoint(RectRotationProjection(RectV2(0, 0, 500, 500), r, 0, a, destPt), DEColor(1, 0.5, 0, 5), 20, 2);
  // 在投影框四周画上记号
  with TV2R4.Init(r, a) do
    begin
      d.DrawText('左上', 14, DEColor(0.5, 1, 0.5), LeftTop, a);
      d.DrawText('右上', 14, DEColor(0.5, 1, 0.5), RightTop, a);
      d.DrawText('右下', 14, DEColor(0.5, 1, 0.5), RightBottom, a);
      d.DrawText('左下', 14, DEColor(0.5, 1, 0.5), LeftBottom, a);
    end;
  d.DrawText(
    '|color(0.5,1,0.5)|在此处移动鼠标反投影||' + #13#10 +
    '严格划分，投影是图形领域，多用于空间到空间之间的转换' + #13#10 +
    '坐标投影是将原框体坐标按尺度和旋转投影到目标框体' + #13#10 +
    '我们可以注意到目标框体在旋转，目标框体尺度和原框体并不相同' + #13#10 +
    '投影的计算和使用，需要建立概念才行，这是公式计算，不是流程' + #13#10 +
    '正投影函数: |color(0.5,1.0,0.5)|RectProjectionRotationDest||' + #13#10 +
    '反投影函数: |color(0.5,1.0,0.5)|RectProjectionRotationSource||' + #13#10 +
    '正反通用投影函数: |color(0.5,1.0,0.5)|RectRotationProjection||' + #13#10 +
    '该Demo投影计算不使用对称矩阵，这样可以更直观的理解投影', 14, d.ScreenRectV2, DEColor(1, 1, 1), False);
  d.Flush;
end;

end.
