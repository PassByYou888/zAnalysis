unit GUIAnalysisMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  FMX.TabControl, FMX.Surfaces, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts,

  CoreClasses, MemoryRaster, UnicodeMixedLib, Geometry2DUnit, FastKDTreeS, KM,
  zDrawEngineInterface_SlowFMX;

const
  sceneWidth  = 800;
  sceneHeight = 800;
  RandomCount = 1000;

type
  TGUIAnalysisMainForm = class(TForm)
    Layout1: TLayout;
    KEdit: TEdit;
    Label1: TLabel;
    Button1: TButton;
    LeftImage1: TImage;
    RightImage1: TImage;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pl1: T2DPointList;

    procedure GenerateData(pl: T2DPointList);
    procedure GenerateView(pl: T2DPointList; bmp: TMemoryRaster);
    procedure BuildCluster(pl: T2DPointList; k: Integer; bmp: TMemoryRaster);
  end;

var
  GUIAnalysisMainForm: TGUIAnalysisMainForm;

implementation

{$R *.fmx}


procedure TGUIAnalysisMainForm.Button1Click(Sender: TObject);
var
  mr: TMemoryRaster;
begin
  mr := TMemoryRaster.Create;
  mr.OpenAGG;
  GenerateView(pl1, mr);

  MemoryBitmapToBitmap(mr, LeftImage1.Bitmap);

  BuildCluster(pl1, umlStrtoInt(KEdit.Text, 5), mr);
  MemoryBitmapToBitmap(mr, RightImage1.Bitmap);
  DisposeObject(mr);
end;

procedure TGUIAnalysisMainForm.Button2Click(Sender: TObject);
begin
  GenerateData(pl1);
  Button1Click(nil);
end;

procedure TGUIAnalysisMainForm.FormCreate(Sender: TObject);
begin
  pl1 := T2DPointList.Create;

  GenerateData(pl1);

  Button1Click(nil);
end;

procedure TGUIAnalysisMainForm.FormDestroy(Sender: TObject);
begin
  DisposeObject([pl1]);
end;

procedure TGUIAnalysisMainForm.FormResize(Sender: TObject);
begin
  LeftImage1.Width := Width * 0.5 - 10;
  LeftImage1.UpdateEffects;
end;

procedure TGUIAnalysisMainForm.GenerateData(pl: T2DPointList);
var
  i: Integer;
  pt: T2DPoint;
begin
  pl.Clear;
  for i := 0 to RandomCount - 1 do
    begin
      pt := Make2DPoint(umlRandomRange(20, sceneWidth - 20), umlRandomRange(20, sceneHeight - 20));
      pl.add(pt);
    end;
end;

procedure TGUIAnalysisMainForm.GenerateView(pl: T2DPointList; bmp: TMemoryRaster);
var
  i: Integer;
  pt: T2DPoint;
begin
  bmp.SetSize(sceneWidth, sceneHeight, RasterColor($0, $0, $0, $0));
  for i := 0 to pl.Count - 1 do
    begin
      pt := pl[i]^;
      bmp.FillCircle(pt, 5, RasterColor($FF, $0, $0, $FF));
    end;
end;

procedure TGUIAnalysisMainForm.BuildCluster(pl: T2DPointList; k: Integer; bmp: TMemoryRaster);
var
  k2d: TKDT2DS;
  OutIndex: TDynamicIndexArray;
  arryPl: array of T2DPointList;
  i: Integer;
  pt: T2DPoint;
  ConvexHullPl: T2DPointList;
begin
  k2d := TKDT2DS.Create;
  k2d.BuildKDTreeWithClusterP(pl.Count, k, 1, OutIndex, nil, procedure(const IndexFor: NativeInt; var Source: TKDT2DS.TKDT2DS_Source; const Data: Pointer)
    begin
      Source.Buff[0] := pl[IndexFor]^[0];
      Source.Buff[1] := pl[IndexFor]^[1];
      Source.index := IndexFor;
    end);

  SetLength(arryPl, k);
  for i := 0 to k - 1 do
      arryPl[i] := T2DPointList.Create;

  for i := 0 to Length(OutIndex) - 1 do
    begin
      pt := pl[i]^;
      arryPl[OutIndex[i]].add(pt);
    end;

  for i := 0 to k - 1 do
    begin
      ConvexHullPl := T2DPointList.Create;
      arryPl[i].ConvexHull(ConvexHullPl);
      bmp.DrawPointListLine(ConvexHullPl, RasterColor($0, $0, 0, $FF), True);
      DisposeObject(ConvexHullPl);
    end;

  DisposeObject(k2d);
  for i := 0 to k - 1 do
      DisposeObject(arryPl[i]);

  SetLength(arryPl, 0);
  SetLength(OutIndex, 0);

end;

end.
