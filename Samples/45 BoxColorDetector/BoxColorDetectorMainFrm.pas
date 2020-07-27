unit BoxColorDetectorMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,

  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  Geometry2DUnit,
  MemoryRaster, MorphologyExpression,
  zDrawEngine,
  zDrawEngineInterface_SlowFMX;

type
  TBoxColorDetectorMainForm = class(TForm)
    oriImage: TImage;
    outImage: TImage;
    detButton: TButton;
    segmentLineDetButton: TButton;
    SegDetButton: TButton;
    LineDetButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure oriImageClick(Sender: TObject);
    procedure detButtonClick(Sender: TObject);
    procedure SegDetButtonClick(Sender: TObject);
    procedure LineDetButtonClick(Sender: TObject);
    procedure segmentLineDetButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    bk: TMemoryRaster;
    raster: TMemoryRaster;
  end;

var
  BoxColorDetectorMainForm: TBoxColorDetectorMainForm;

implementation

{$R *.fmx}


procedure TBoxColorDetectorMainForm.FormCreate(Sender: TObject);
var
  r: TRect;
  i: Integer;
  x: Integer;
begin
  raster := NewRaster;
  bk := NewRaster;
  BitmapToMemoryBitmap(oriImage.Bitmap, raster);
  bk.Assign(raster);

  CoreClasses.SetMT19937Seed(1);
  for i := 1 to 5 do
    begin
      repeat
        r.Left := umlRandomRange(5, raster.Width - 6);
        r.Right := umlRandomRange(5, raster.Width - 6);
        r.Top := umlRandomRange(5, raster.Height - 6);
        r.Bottom := umlRandomRange(5, raster.Height - 6);
      until (r.Width > 20) and (r.Height > 20);
      raster.DrawRect(RectV2(r), RColorF(1, 1, 1, 1));
    end;
  MemoryBitmapToBitmap(raster, oriImage.Bitmap);
end;

procedure TBoxColorDetectorMainForm.oriImageClick(Sender: TObject);
var
  r: TRect;
  i: Integer;
  x: Integer;
begin
  raster.Assign(bk);
  for i := 1 to 5 do
    begin
      repeat
        r.Left := umlRandomRange(5, raster.Width - 6);
        r.Right := umlRandomRange(5, raster.Width - 6);
        r.Top := umlRandomRange(5, raster.Height - 6);
        r.Bottom := umlRandomRange(5, raster.Height - 6);
      until (r.Width > 20) and (r.Height > 20);
      raster.DrawRect(RectV2(r), RColorF(1, 1, 1, 1));
    end;
  MemoryBitmapToBitmap(raster, oriImage.Bitmap);
end;

procedure TBoxColorDetectorMainForm.detButtonClick(Sender: TObject);
var
  d: TDrawEngine;
  n: U_String;
  dcol: TDEColor;
  MorphMath: TMorphomatics;
  MorphBin: TMorphologyBinaryzation;
  RCLines: TMorphologyRCLines;
  rl: TRectV2List;
  i: Integer;
  r: TRectV2;
begin
  {
    规则框体检测：规则框体是指框体的4个角点都在平行线上
  }
  d := TDrawEngine.Create;
  d.Rasterization.Memory.SetSize(raster.Width, raster.Height, RColor(0, 0, 0));
  d.Rasterization.UsedAgg := True;
  d.SetSize;
  d.ViewOptions := [];

  // 形态学方法，先将YIQ色彩形态的Y空间提炼成数学形态
  MorphMath := raster.BuildMorphomatics(TMorphologyPixel.mpYIQ_Y);

  // 使用阈值法将数学形态提炼成二值化形态
  // Binarization是最简单的一种二值化方法，0.99是判断偏纯白的YIQ.Y值是否等同
  MorphBin := MorphMath.Binarization(0.99);

  // 基于二值化形态进行平行线的检测和构建
  RCLines := TMorphologyRCLines.BuildLines(MorphBin, 10);
  // 根据构造好的平行线数据，计算规则框体
  rl := RCLines.BuildFormulaBox();

  // 把检测结果画出来
  d.DrawText(Format('total box:%d', [rl.Count]), 12, d.ScreenRect, DEColor(1, 1, 1, 1), False);
  for i := 0 to rl.Count - 1 do
    begin
      r := rl[i];
      dcol := RColor2DColor(RandomRColor($7F));
      d.DrawBox(r, dcol, 1);
      n := Format('%d*%d', [RoundWidth(r), RoundHeight(r)]);
      d.DrawLabelBox(n, 16, DEColor(1, 1, 1, 1), r, dcol, 2);
    end;
  d.Flush;

  disposeObject(MorphMath);
  disposeObject(MorphBin);
  disposeObject(rl);
  disposeObject(RCLines);
  MemoryBitmapToBitmap(d.Rasterization.Memory, outImage.Bitmap);
  disposeObject(d);
end;

procedure TBoxColorDetectorMainForm.SegDetButtonClick(Sender: TObject);
var
  d: TDrawEngine;
  n: U_String;
  dcol: TDEColor;
  MorphMath: TMorphomatics;
  MorphBin: TMorphologyBinaryzation;
  RCLines: TMorphologyRCLines;
  rl: TRectV2List;
  i: Integer;
  r: TRectV2;
begin
  {
    线段分割后的框体检测：当一条平行线被另一条平行线相交后，这条平行线就会被视作两根线，同时它也表示4个角点
    简单来说，图像中所有可能的规则框体都会被检测出来，包括重叠的框体
    因为检测出的框体非常多，所以我们需要自行变成对检测结果做过滤处理
    如果你不太会写图像程序，可以直接使用规则框体检测
    所有可能出现的框体，它都能检测出来
  }
  d := TDrawEngine.Create;
  d.Rasterization.Memory.SetSize(raster.Width, raster.Height, RColor(0, 0, 0));
  d.Rasterization.UsedAgg := False;
  d.SetSize;
  d.ViewOptions := [];

  // 形态学方法，先将YIQ色彩形态的Y空间提炼成数学形态
  MorphMath := raster.BuildMorphomatics(TMorphologyPixel.mpYIQ_Y);

  // 使用阈值法将数学形态提炼成二值化形态
  // Binarization是最简单的一种二值化方法，0.99是判断偏纯白的YIQ.Y值是否等同
  MorphBin := MorphMath.Binarization(0.99);

  // 基于二值化形态进行分割线检测和构建
  RCLines := TMorphologyRCLines.BuildIntersectSegment(MorphBin, 10);

  // 根据构造好的分割线数据，计算规则框体
  rl := RCLines.BuildFormulaBox();

  // 把检测结果画出来
  d.DrawText(Format('total box:%d', [rl.Count]), 12, d.ScreenRect, DEColor(1, 1, 1, 1), False);
  for i := 0 to rl.Count - 1 do
    begin
      r := rl[i];
      dcol := RColor2DColor(RandomRColor($7F));
      n := Format('%d*%d', [RoundWidth(r), RoundHeight(r)]);
      d.DrawLabelBox(n, 16, DEColor(1, 1, 1, 1), r, dcol, 2);
    end;
  d.Flush;

  disposeObject(MorphMath);
  disposeObject(MorphBin);
  disposeObject(rl);
  disposeObject(RCLines);
  MemoryBitmapToBitmap(d.Rasterization.Memory, outImage.Bitmap);
  disposeObject(d);
end;

procedure TBoxColorDetectorMainForm.LineDetButtonClick(Sender: TObject);
var
  n: TMemoryRaster;
  MorphMath: TMorphomatics;
  MorphBin: TMorphologyBinaryzation;
  RCLines: TMorphologyRCLines;
  p: PMorphologyRCLine;
  i: Integer;
begin
  {
    RC线是指，Row+column线，就是横线和竖线
    规则框体都是由横线+竖线组成
  }

  // 形态学方法，先将YIQ色彩形态的Y空间提炼成数学形态
  MorphMath := raster.BuildMorphomatics(TMorphologyPixel.mpYIQ_Y);

  // 使用阈值法将数学形态提炼成二值化形态
  // Binarization是最简单的一种二值化方法，0.99是判断偏纯白的YIQ.Y值是否等同
  MorphBin := MorphMath.Binarization(0.99);

  // 基于二值化形态进行RC线检测和构建
  RCLines := TMorphologyRCLines.BuildLines(MorphBin, 10);

  // 将检测到的RC线画出来
  n := NewRaster();
  n.SetSize(raster.Width, raster.Height, RColor(0, 0, 0));
  for i := 0 to RCLines.Count - 1 do
    begin
      p := RCLines[i];
      n.LineF(Vec2(p^.Bp), Vec2(p^.ep), RandomRColor, True, 5, True);
    end;

  disposeObject(MorphMath);
  disposeObject(MorphBin);
  disposeObject(RCLines);
  MemoryBitmapToBitmap(n, outImage.Bitmap);
  disposeObject(n);
end;

procedure TBoxColorDetectorMainForm.segmentLineDetButtonClick(Sender: TObject);
var
  n: TMemoryRaster;
  MorphMath: TMorphomatics;
  MorphBin: TMorphologyBinaryzation;
  RCLines: TMorphologyRCLines;
  p: PMorphologyRCLine;
  i: Integer;
begin
  {
    RC线是指，Row+column线，就是横线和竖线
    规则框体都是由横线+竖线组成
  }

  // 形态学方法，先将YIQ色彩形态的Y空间提炼成数学形态
  MorphMath := raster.BuildMorphomatics(TMorphologyPixel.mpYIQ_Y);

  // 使用阈值法将数学形态提炼成二值化形态
  // Binarization是最简单的一种二值化方法，0.99是判断偏纯白的YIQ.Y值是否等同
  MorphBin := MorphMath.Binarization(0.99);

  // 基于二值化形态进行RC线检测和构建
  RCLines := TMorphologyRCLines.BuildIntersectSegment(MorphBin, 10);

  // 将检测到的RC线画出来
  n := NewRaster();
  n.SetSize(raster.Width, raster.Height, RColor(0, 0, 0));
  for i := 0 to RCLines.Count - 1 do
    begin
      p := RCLines[i];
      n.LineF(Vec2(p^.Bp), Vec2(p^.ep), RandomRColor, True, 5, True);
    end;

  disposeObject(MorphMath);
  disposeObject(MorphBin);
  disposeObject(RCLines);
  MemoryBitmapToBitmap(n, outImage.Bitmap);
  disposeObject(n);
end;

end.
