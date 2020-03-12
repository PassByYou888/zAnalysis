unit HoughRotationDetectAndCalibrateMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.ExtCtrls,

  CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, Geometry2DUnit,
  MemoryRaster, zDrawEngine, zDrawEngineInterface_SlowFMX;

type
  THoughRotationDetectAndCalibrateMainForm = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Memo1: TMemo;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusMethod(Text_: SystemString; const ID: Integer);
  public
    { Public declarations }
  end;

var
  HoughRotationDetectAndCalibrateMainForm: THoughRotationDetectAndCalibrateMainForm;

implementation

{$R *.fmx}


procedure THoughRotationDetectAndCalibrateMainForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
end;

procedure THoughRotationDetectAndCalibrateMainForm.Button1Click(Sender: TObject);
var
  mr: TMemoryRaster;
begin
  mr := NewRaster;
  BitmapToMemoryBitmap(Image1.Bitmap, mr);
  mr.CalibrateRotate(RColor($FF, $FF, $FF));
  MemoryBitmapToBitmap(mr, Image2.Bitmap);
  disposeObject(mr);
end;

procedure THoughRotationDetectAndCalibrateMainForm.DoStatusMethod(
  Text_: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(Text_);
  Memo1.GoToTextEnd;
end;

end.
