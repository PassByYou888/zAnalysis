{ ****************************************************************************** }
{ * FMX canvas Character to Ratermization                                      * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit FMXCharacterMapBuilder;

{$INCLUDE ..\zDefine.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics,

  CoreClasses, ListEngine,
  ObjectData, ObjectDataManager, ItemStream, zExpression,
  MemoryStream64, MemoryRaster, Geometry2DUnit, DoStatusIO, PascalStrings, UPascalStrings,
  UnicodeMixedLib, zDrawEngine, zDrawEngineInterface_SlowFMX;

type
  TCharBox_ = record
    Char_: SystemChar;
    box_: TRect;
  end;

  TArrayCharBox = array of TCharBox_;

function BuildFMXCharacterRaster(fontName_: U_String; fontSize_: Integer; Bold_, Italic_: Boolean; InputBuff: TArrayChar; var boxOutput: TArrayCharBox): TMemoryRaster;

implementation

type
  TFMXFontToRasterFactory = class
  public
    bmp: FMX.Graphics.TBitmap;
    dIntf: TDrawEngineInterface_FMX;
    d: TDrawEngine;
    fontSize: Integer;
    constructor Create(fontName_: string; fontSize_: Integer; Bold_, Italic_: Boolean);
    destructor Destroy; override;
    function MakeCharRaster(Char_: string; var MinRect_: TRect): TMemoryRaster;
  end;

constructor TFMXFontToRasterFactory.Create(fontName_: string; fontSize_: Integer; Bold_, Italic_: Boolean);
begin
  inherited Create;
  fontSize := fontSize_;

  bmp := FMX.Graphics.TBitmap.Create;
  bmp.SetSize(fontSize_ * 2, fontSize_ * 2);

  bmp.Canvas.Font.Family := fontName_;
  bmp.Canvas.Font.Size := fontSize_;
  if Bold_ then
      bmp.Canvas.Font.Style := bmp.Canvas.Font.Style + [TFontStyle.fsBold]
  else
      bmp.Canvas.Font.Style := bmp.Canvas.Font.Style - [TFontStyle.fsBold];

  if Italic_ then
      bmp.Canvas.Font.Style := bmp.Canvas.Font.Style + [TFontStyle.fsItalic]
  else
      bmp.Canvas.Font.Style := bmp.Canvas.Font.Style - [TFontStyle.fsItalic];

  dIntf := TDrawEngineInterface_FMX.Create;
  dIntf.SetSurface(bmp.Canvas, bmp);
  d := TDrawEngine.Create;
  d.DrawInterface := dIntf;
  d.ViewOptions := [];
  d.SetSize;
end;

destructor TFMXFontToRasterFactory.Destroy;
begin
  disposeObject(d);
  disposeObject(dIntf);
  disposeObject(bmp);
  inherited Destroy;
end;

function TFMXFontToRasterFactory.MakeCharRaster(Char_: string; var MinRect_: TRect): TMemoryRaster;
var
  r4: TV2Rect4;
begin
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0));
  d.Flush;
  r4 := d.DrawText(Char_, fontSize, d.ScreenRect, DEColor(1, 1, 1), True);
  MinRect_ := Rect2Rect(r4.BoundRect);
  d.Flush;
  Result := TMemoryRaster.Create;
  BitmapToMemoryBitmap(bmp, Result);
end;

function BuildFMXCharacterRaster(fontName_: U_String; fontSize_: Integer; Bold_, Italic_: Boolean; InputBuff: TArrayChar; var boxOutput: TArrayCharBox): TMemoryRaster;
var
  BmpFactory: TFMXFontToRasterFactory;
  fr: TFontRaster;
  c: SystemChar;
  tmp, nRaster: TMemoryRaster;
  morph: TMorphMath;
  bin: TMorphBin;
  R: TRect;
  box_: TCharBox_;
  boxL: TGenericsList<TCharBox_>;
  i: Integer;
begin
  BmpFactory := TFMXFontToRasterFactory.Create(fontName_, fontSize_, Bold_, Italic_);
  fr := TFontRaster.Create;
  for c in InputBuff do
    begin
      tmp := BmpFactory.MakeCharRaster(c, R);
      morph := tmp.BuildMorphomatics(TMorphPixel.mpGrayscale);
      bin := morph.Binarization(0.5);
      R := bin.BoundsRect;
      if not R.IsEmpty then
        begin
          inc(R.Right);
          inc(R.Bottom);
          nRaster := TRaster.Create;
          nRaster.SetSize(R.Width + 1, R.Height + 1, RColor(0, 0, 0));
          nRaster.Draw(1, 1, R, tmp);
          try
              fr.Add(c, nRaster);
          except
          end;
        end;
      disposeObject(tmp);
      disposeObject(morph);
      disposeObject(bin);
    end;
  fr.Build(fontSize_);
  Result := fr.BuildRaster(False);

  DoStatus('compute character box.');
  boxL := TGenericsList<TCharBox_>.Create;
  for c in InputBuff do
    begin
      if fr.ValidChar(c) then
        begin
          box_.Char_ := c;
          box_.box_ := fr.GetBox(c);
          boxL.Add(box_);
        end;
    end;

  setLength(boxOutput, boxL.Count);
  for i := 0 to boxL.Count - 1 do
      boxOutput[i] := boxL[i];

  DoStatus('font generate done.');
  disposeObject(boxL);
  disposeObject(fr);
  disposeObject(BmpFactory);
end;

end.
