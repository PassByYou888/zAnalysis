unit BuildInPictureDriverFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects,

  CoreClasses, PascalStrings,
  ObjectDataManager, MemoryStream64, TextDataEngine, ListEngine, ObjectDataHashItem,
  MemoryRaster, zDrawEngine, zDrawEngineInterface_SlowFMX, MediaCenter,
  Geometry2DUnit,
  PictureViewerInterface;

type
  TBuildInPictureDriverForm = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    d: TDrawEngine;
    dIntf: TDrawEngineInterface_FMX;
    viewIntf: TPictureViewerInterface;
  public
  end;

var
  BuildInPictureDriverForm: TBuildInPictureDriverForm;

implementation

{$R *.fmx}


procedure TBuildInPictureDriverForm.FormCreate(Sender: TObject);
var
  l: TCoreClassList;
  i: Integer;
  p: PHashItemData;
begin
  dIntf := TDrawEngineInterface_FMX.Create;
  dIntf.SetSurface(Canvas, Self);
  d := TDrawEngine.Create;
  d.DrawInterface := dIntf;
  viewIntf := TPictureViewerInterface.Create(d);
  InitGlobalMedia([gmtArt]);

  l := TCoreClassList.Create;
  ArtLibrary.ROOT.GetList(l);
  for i := 0 to l.count - 1 do
    begin
      p := l[i];
      viewIntf.InputPicture(NewRasterFromStream(p^.stream), p^.OriginName, True, False, True);
    end;
  disposeObject(l);
end;

procedure TBuildInPictureDriverForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  dIntf.SetSurface(Canvas, Sender);
  d.DrawInterface := dIntf;
  d.SetSize;
  d.ViewOptions := [];

  viewIntf.DrawEng := d;
  viewIntf.Render;
end;

procedure TBuildInPictureDriverForm.pbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  viewIntf.TapDown(vec2(X, Y));
end;

procedure TBuildInPictureDriverForm.pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  viewIntf.TapMove(vec2(X, Y));
end;

procedure TBuildInPictureDriverForm.pbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  viewIntf.TapUp(vec2(X, Y));
end;

procedure TBuildInPictureDriverForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  Handled := True;
  if WheelDelta > 0 then
      viewIntf.ScaleCamera(1.1)
  else
      viewIntf.ScaleCamera(0.9);
end;

procedure TBuildInPictureDriverForm.Timer1Timer(Sender: TObject);
begin
  Invalidate;
end;

end.
