unit MorphologySegmentationMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls,
  FMX.Controls.Presentation,

  System.IOUtils, System.Threading,

  CoreClasses, UnicodeMixedLib, PascalStrings, Geometry2DUnit, MemoryRaster, MemoryStream64,
  zDrawEngine, zDrawEngineInterface_SlowFMX;

type
  TMorphologySegmentationMainForm = class(TForm)
    segListPB: TPaintBox;
    segPB: TPaintBox;
    Timer1: TTimer;
    Splitter1: TSplitter;
    openButton: TButton;
    OpenDialog1: TOpenDialog;
    ViewGeometryCheckBox: TCheckBox;
    ViewEdgeLinesCheckBox: TCheckBox;
    EdgeLinesCrossCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure openButtonClick(Sender: TObject);
    procedure segListPBPaint(Sender: TObject; Canvas: TCanvas);
    procedure segPBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure segPBPaint(Sender: TObject; Canvas: TCanvas);
    procedure Timer1Timer(Sender: TObject);
    procedure ViewGeometryCheckBoxClick(Sender: TObject);
  private
    drawIntf: TDrawEngineInterface_FMX;
  public
    tex: TMemoryRaster;
    tex_box: TRectV2;
    pickColor: TRColor;
    SegImgList: TMemoryRasterList;
    LastSegBox: TArrayRectV2;

    // 输入的分割器的颜色分类ID
    procedure GetPixelSegClassify(X, Y: Integer; Color: TRColor; var Classify: TMorphologyClassify);

    // 构建分割
    procedure BuildSeg;
  end;

var
  MorphologySegmentationMainForm: TMorphologySegmentationMainForm;

implementation

{$R *.fmx}


procedure TMorphologySegmentationMainForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  tex := NewRasterFromFile(umlCombineFileName(TPath.GetLibraryPath, 'ColorSeg1.bmp'));
  SegImgList := TMemoryRasterList.Create;
  SetLength(LastSegBox, 0);
end;

procedure TMorphologySegmentationMainForm.segListPBPaint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  r: TRectV2;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voEdge];

  d.FillBox(d.ScreenRect, DEColor(0.1, 0.1, 0.1));

  LockObject(SegImgList);
  d.CameraR := d.DrawPicturePackingInScene(SegImgList, 10, Vec2(0, 0), 1.0);
  UnLockObject(SegImgList);

  d.DrawText('像素分割后', 16, d.ScreenRect, DEColor(0.5, 1.0, 0.5), False);
  d.Flush;
end;

procedure TMorphologySegmentationMainForm.segPBMouseUp(Sender: TObject; Button:
  TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pt: TVec2;
  i: Integer;
  c: TRColorEntry;
begin
  LockObject(SegImgList);
  for i := 0 to SegImgList.count - 1 do
      DisposeObject(SegImgList[i]);
  SegImgList.Clear;
  UnLockObject(SegImgList);

  pt := RectProjection(tex_box, tex.BoundsRectV2, Vec2(X, Y));
  if PointInRect(pt, tex.BoundsRectV2) then
    begin
      pickColor := tex.PixelVec[pt];

      // if RColorDistance(pickColor, RColor(0, 0, 0)) < color_threshold then
      // begin
      // DrawPool(segPB).PostScrollText(5, '不能拾取黑色', 24, DEColor(1, 0, 0));
      // exit;
      // end;

      c.BGRA := pickColor;

      DrawPool(segPB).PostScrollText(5, Format('正在分割颜色|color(%d,%d,%d)|(%d,%d,%d)||' + #13#10, [c.r, c.G, c.B, c.r, c.G, c.B]),
        24, DEColor(1.0, 1.0, 1.0));

      BuildSeg;
    end;
end;

procedure TMorphologySegmentationMainForm.segPBPaint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  n: U_String;
  i: Integer;
  c: TRColorEntry;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voEdge];

  d.FillBox(d.ScreenRect, DEColor(0.5, 0.5, 0.5));

  tex_box := d.FitDrawPicture(tex, tex.BoundsRectV2, RectEdge(d.ScreenRect, -20), 1.0);

  for i := 0 to length(LastSegBox) - 1 do
      d.DrawBox(RectTransform(tex.BoundsRectV2, tex_box, LastSegBox[i]), DEColor(1, 0.5, 0.5, 1), 2);

  d.DrawDotLineBox(tex_box, Vec2(0.5, 0.5), 0, DEColor(0.8, 0.1, 0.4), 3);

  d.Flush;
end;

procedure TMorphologySegmentationMainForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

procedure TMorphologySegmentationMainForm.GetPixelSegClassify(X, Y: Integer; Color: TRColor; var Classify: TMorphologyClassify);
var
  i: Integer;
  ID: WORD;
begin
  // 输入的分割器的颜色分类ID
  // Classify给 0 表示这个颜色不必分割，如果大于0的话，分割器会按Classify进行分类
  Classify := 0;

  if RColorDistance(pickColor, RColor(0, 0, 0)) < 0.1 then
      exit;

  if RColorDistance(pickColor, Color) < 0.05 then
      Classify := pickColor;
end;

procedure TMorphologySegmentationMainForm.BuildSeg;
begin
  TComputeThread.RunP(nil, nil, procedure(ThSender: TComputeThread)
    var
      s, test: TMorphologySegmentation;
      first_total: Integer;
      stream: TMemoryStream64;
    begin
      test := TMorphologySegmentation.Create;
      // 分割器的颜色分类接口
      test.OnGetPixelSegClassify := GetPixelSegClassify;
      // 形态分割器在数据输入时是可编程的分类器，这里会涉及到少量工程统计学知识，诸如数据梯度，分布这类基本常识
      // 形态分割器可以选择，形态象素分割，形态数学分割，二值化分割，一共三种分割模型
      // 使用形态分割器，多数都是在经过了重重数据预处理以后，做编程识别的步骤
      // 这一步不可自动化，需要编程基本功支持，数据结构要求高于普通编程
      test.BuildSegmentation(tex);
      // test.RemoveNoise(500);

      // TMorphologySegmentation分割器支持Stream保存
      // 下列程序演示了使用Stream保存TMorphologySegmentation分割器中的复杂数据
      stream := TMemoryStream64.Create;
      test.SaveToStream(stream);
      DisposeObject(test);
      stream.Position := 0;
      test := TMorphologySegmentation.Create;
      test.LoadFromStream(stream);
      DisposeObject(stream);

      // TMorphologySegmentation分割器中的数据都是指针，不会占用很大内存，它们相互关联
      // TMorphologySegmentation分割器有能力复制自身的复杂数据
      s := TMorphologySegmentation.Create;
      s.Assign(test);
      DisposeObject(test);

      // 记录一下首次分割总数
      first_total := s.count;

      // 构建分割以后会出现噪音，这里我们将噪音移出
      // 阈值50表示分割碎片块像素总和如果低于20个
      // s.RemoveNoise(20);

      SetLength(LastSegBox, s.count);

      DelphiParallelFor(0, s.count - 1, procedure(pass: Integer)
        var
          j, k: Integer;
          sp: TMorphologyPool;
          nm: TMemoryRaster;
          geo: T2DPolygonGraph;
          LL: TLinesList;
          L: TLines;
          c: TRColor;
        begin
          sp := s[pass];
          // 将分割图形构建成T2DPolygonGraph，T2DPolygonGraph由包围多边形和塌陷多边形共同组成
          geo := sp.BuildConvolutionGeometry(1.0);
          // geo := sp.BuildGeometry(0.0);
          // 将分割边界构建成多条非闭合线
          LL := sp.BuildLines(1.0);
          LastSegBox[pass] := sp.BoundsRectV2;
          // BuildDatamap方法会将分割数据投影到一个新光栅中
          nm := sp.BuildClipDatamap(RColor(0, 0, 0, 0), RasterAlphaColor(pickColor, $7F));

          nm.OpenAgg;
          nm.Agg.LineWidth := 2;
          if geo <> nil then
            begin
              geo.Transform(-sp.Left, -sp.Top);
              // 将变换后的多边形包画出来
              if ViewGeometryCheckBox.IsChecked then
                  nm.DrawPolygon(geo, RColorF(1, 1, 1), RColorF(0.8, 1.0, 0.8));
              // 画坐标
              if ViewGeometryCheckBox.IsChecked and EdgeLinesCrossCheckBox.IsChecked then
                  nm.DrawPolygonCross(geo, 5, RColorF(1.0, 0, 0), RColorF(1.0, 0.5, 0.5));
              DisposeObject(geo);
            end;

          for j := 0 to LL.count - 1 do
            begin
              L := LL[j];
              // 变换坐标,使它与BuildClipDatamap出来的图像吻合
              L.Transform(-sp.Left, -sp.Top);
              c := RColor(umlRandomRange($7F, $FF), umlRandomRange($7F, $FF), umlRandomRange($7F, $FF), $FF);
              // 把分割边界的非闭合线画出来
              if ViewEdgeLinesCheckBox.IsChecked then
                  nm.DrawPolygonLine(L, c, False);
              // 画坐标
              if ViewEdgeLinesCheckBox.IsChecked and EdgeLinesCrossCheckBox.IsChecked then
                  nm.DrawCrossF(L, 5, c);
              DisposeObject(L);
            end;
          DisposeObject(LL);
          nm.CloseAgg;

          LockObject(SegImgList);
          SegImgList.Add(nm);
          UnLockObject(SegImgList);
        end);

      DrawPool(segPB).PostScrollText(5,
        Format('分割报告:检测到 |s:16,color(1,0,0)|%d|| 个像素图形(包含像素碎片图形),而实际有效只有 |s:16,color(1,0,0)|%d|| 个图形', [first_total, SegImgList.count]),
        20, DEColor(1, 1, 1));
      DisposeObject(s);
    end);
end;

procedure TMorphologySegmentationMainForm.openButtonClick(Sender: TObject);
var
  i: Integer;
begin
  OpenDialog1.Filter := TBitmapCodecManager.GetFilterString;
  if not OpenDialog1.Execute then
      exit;

  DisposeObject(tex);
  tex := NewRasterFromFile(OpenDialog1.FileName);
  SetLength(LastSegBox, 0);
end;

procedure TMorphologySegmentationMainForm.ViewGeometryCheckBoxClick(Sender: TObject);
var
  i: Integer;
begin
  LockObject(SegImgList);
  for i := 0 to SegImgList.count - 1 do
      DisposeObject(SegImgList[i]);
  SegImgList.Clear;
  UnLockObject(SegImgList);

  BuildSeg;
end;

end.
