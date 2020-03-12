unit RotationFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Layouts,

  CoreClasses, PascalStrings, UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit,
  MemoryRaster, MemoryStream64, DoStatusIO, zDrawEngine,
  zExpression,
  zDrawEngineInterface_FMX, zAI_Common;

type
  TApproximatePolygonForm = class(TForm)
    pb1: TPaintBox;
    pb2: TPaintBox;
    Memo1: TMemo;
    Timer1: TTimer;
    Layout1: TLayout;
    Label1: TLabel;
    sRotateEdit: TEdit;
    Layout2: TLayout;
    Label2: TLabel;
    sOffsetEdit: TEdit;
    Layout3: TLayout;
    Label3: TLabel;
    sScaleEdit: TEdit;
    Layout4: TLayout;
    Label4: TLabel;
    sAxisEdit: TEdit;
    Layout5: TLayout;
    Label5: TLabel;
    dRotateEdit: TEdit;
    Layout6: TLayout;
    Label6: TLabel;
    dOffsetEdit: TEdit;
    Layout7: TLayout;
    Label7: TLabel;
    dScaleEdit: TEdit;
    Layout8: TLayout;
    Label8: TLabel;
    dAxisEdit: TEdit;
    projButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure pb1Paint(Sender: TObject; Canvas: TCanvas);
    procedure pb2Paint(Sender: TObject; Canvas: TCanvas);
    procedure projButtonClick(Sender: TObject);
    procedure sourceOptChange(Sender: TObject);
    procedure TargetOptChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    dIntf: TDrawEngineInterface_FMX;
    bk, sour, dest: TMemoryRaster;
    sour_rect, dest_rect: TV2R4;
    procedure DoStatusMethod(Text_: SystemString; const ID: Integer);
  public
    // raster，光栅
    // angle，旋转角度，0..360，-180-180
    // scale，缩放量，既0..1之间的浮点
    // axis，旋转轴坐标，这里是用尺度来定义，既0..1之间的浮点，这样更符合向量原则
    // offset，偏移量，这里是用尺度来定义，既0..1之间的浮点，这样更符合向量原则
    function RebuildRect(raster: TMemoryRaster; Angle_, scale_: TGeoFloat; rotate_axis_, offset_: TVec2): TV2R4;
    procedure DoProjection;
  end;

var
  ApproximatePolygonForm: TApproximatePolygonForm;

implementation

{$R *.fmx}


procedure TApproximatePolygonForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  dIntf := TDrawEngineInterface_FMX.Create;
  bk := NewRaster();
  bk.SetSize(128, 128);
  FillBlackGrayBackgroundTexture(bk, 32, RColor(0, 0, 0), RColorF(0.5, 0.5, 0.5), RColorF(0.4, 0.4, 0.4));

  sour := NewRasterFromFile(WhereFileFromConfigure('lena.bmp'));
  dest := NewRaster();
  dest.SetSizeR(sour.BoundsRectV2);

  sourceOptChange(nil);
  TargetOptChange(nil);
  DoProjection();
end;

procedure TApproximatePolygonForm.pb1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  box: TV2R4;
begin
  dIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, dIntf);
  d.ViewOptions := [voEdge];
  d.EdgeColor := DEColor(1, 0, 0);

  // 画背景
  d.DrawTile(bk, bk.BoundsRectV2, 1.0);

  // 等尺度绘制后取出绘制目标屏幕的框体，再用sour_rect投影到box
  box := sour_rect.Projection(sour.BoundsRectV20, d.FitDrawPicture(sour, sour.BoundsRectV20, RectEdge(d.ScreenRectV2, -20), 1.0));

  // 把box画出来
  d.DrawDotLineBox(box, DEColor(1, 1, 1, 1), 2);
  with box do
    begin
      d.DrawCross(LeftTop, DEColor(0.5, 0.5, 1.0), 10, 3);
      d.DrawCross(RightTop, DEColor(0.5, 0.5, 1.0), 10, 3);
      d.DrawCross(RightBottom, DEColor(0.5, 0.5, 1.0), 10, 3);
      d.DrawCross(LeftBottom, DEColor(0.5, 0.5, 1.0), 10, 3);
    end;

  d.BeginCaptureShadow(Vec2(2, 2), 1);
  d.DrawText('原图像 |color(1,0,0)|白虚线是投影框体 ||X是4个顶点坐标', 14, DEColor(1, 1, 1), Vec2(5, 5));
  d.EndCaptureShadow;
  d.Flush;
end;

procedure TApproximatePolygonForm.pb2Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  oriBox: TRectV2;
  box: TV2R4;
begin
  dIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, dIntf);
  d.ViewOptions := [voEdge];
  d.EdgeColor := DEColor(1, 0, 0);

  // 画背景
  d.DrawTile(bk, bk.BoundsRectV2, 1.0);

  // 等尺度绘制后取出绘制目标屏幕的框体，再用sour_rect投影到box
  oriBox := d.FitDrawPicture(dest, dest.BoundsRectV20, RectEdge(d.ScreenRectV2, -20), 1.0);
  box := dest_rect.Projection(dest.BoundsRectV20, oriBox);

  // 把box画出来
  d.DrawDotLineBox(box, DEColor(0.5, 0.5, 1, 1), 2);
  with box do
    begin
      d.DrawCross(LeftTop, DEColor(0.5, 0.5, 1.0), 10, 3);
      d.DrawCross(RightTop, DEColor(0.5, 0.5, 1.0), 10, 3);
      d.DrawCross(RightBottom, DEColor(0.5, 0.5, 1.0), 10, 3);
      d.DrawCross(LeftBottom, DEColor(0.5, 0.5, 1.0), 10, 3);
    end;

  // 把原始图片的框画出来
  d.DrawBox(oriBox, DEColor(1, 1, 0, 1), 2);

  d.BeginCaptureShadow(Vec2(2, 2), 1);
  d.DrawText('旋转后的图像 |color(1,1,0)|目标图片为黄框(目标带有alpha) ||蓝线为投影光栅坐标', 14, DEColor(1, 1, 1), Vec2(5, 5));
  d.EndCaptureShadow;
  d.Flush;
end;

procedure TApproximatePolygonForm.sourceOptChange(Sender: TObject);
begin
  sour_rect := RebuildRect(sour,
    EStrToFloat(sRotateEdit.Text, 0),
    EStrToFloat(sScaleEdit.Text, 1.0),
    StrToVec2(sAxisEdit.Text),
    StrToVec2(sOffsetEdit.Text));
end;

procedure TApproximatePolygonForm.TargetOptChange(Sender: TObject);
begin
  dest_rect := RebuildRect(dest,
    EStrToFloat(dRotateEdit.Text, 0),
    EStrToFloat(dScaleEdit.Text, 1.0),
    StrToVec2(dAxisEdit.Text),
    StrToVec2(dOffsetEdit.Text));
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

function TApproximatePolygonForm.RebuildRect(raster: TMemoryRaster; Angle_, scale_: TGeoFloat; rotate_axis_, offset_: TVec2): TV2R4;
begin
  // TV2R4=TV2Rect4，数据结构用4个2D Vector申明，这是由4顶点组成的不规则框体
  // TV2R4.Init等同于构建矩阵
  // TV2R4的换算会比矩阵更快
  // TV2R4的易操作性优于矩阵，操作性=可拆分性，可分析性

  // raster，光栅
  // angle，旋转角度，0..360，-180-180
  // scale，size缩放量，既0..1之间的浮点
  // axis，旋转轴坐标，这里是用尺度来定义，既0..1之间的浮点，这样更符合向量原则
  // offset，偏移量，这里是用尺度来定义，既0..1之间的浮点，这样更符合向量原则
  Result := TV2R4.Init(
    RectMul(RectAdd(raster.BoundsRectV2, Vec2Mul(offset_, raster.Size2D)), scale_), // rect
    Vec2Mul(raster.Size2D, rotate_axis_),                                           // rotation axis
    Angle_                                                                          // rotation angle
    );
end;

procedure TApproximatePolygonForm.DoProjection;
begin
  // 重置目标光栅
  dest.Clear(RColor(0, 0, 0, 0));

  // 投影就是一个函数
  // 光栅投影的计算内核使用三角填充，用两个三角形将一个不规则框体裁开来画
  // 三角填充在dest.Vertex中实现，请自行使用debug追
  // 三角填充会自动支持并行：当填充规模非常大，高于1000分辨率时，Vertex引擎会自动启动并行填充引擎
  // 实测：在8000*8000分辨率的图片填充时，并行技术可以提速高达4-8倍，整个填充过程不会出现的内存申请和释放
  sour.ProjectionTo(dest, sour_rect, dest_rect, True, 1.0);

  // 重置纹理，使他可以在fmx中更新
  dest.NoUsage;
end;

procedure TApproximatePolygonForm.projButtonClick(Sender: TObject);
begin
  DoProjection;
end;

end.
