{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
  //    Maintained by Christian-W. Budde (Christian@pcjv.de)                    //
  //    Copyright (c) 2012-2017                                                 //
  //                                                                            //
  //  Based on:                                                                 //
  //    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
  //    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
  //                                                                            //
  //  Original License:                                                         //
  //    Anti-Grain Geometry - Version 2.4 (Public License)                      //
  //    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
  //    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
  //                                                                            //
  //  Permission to copy, use, modify, sell and distribute this software        //
  //  is granted provided this copyright notice appears in all copies.          //
  //  This software is provided "as is" without express or implied              //
  //  warranty, and with no claim as to its suitability for any purpose.        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)
unit AggControl;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggTransAffine,
  AggRasterizerScanLine,
  AggScanline,
  AggRendererScanLine,
  AggRenderScanlines,
  AggVertexSource,
  AggColor32;

type
  TAggCustomAggControl = class(TAggCustomVertexSource)
  private
    FFlipY: Boolean;
    FMatrix: TAggTransAffine;
    function GetScale: Double;
  protected
    FRect: TRectDouble;
    function GetColorPointer(index: Cardinal): PAggColor; virtual; abstract;
  public
    constructor Create(x1, y1, x2, y2: Double; FlipY: Boolean); virtual;
    destructor Destroy; override;

    procedure SetClipBox(x1, y1, x2, y2: Double); overload; virtual;
    procedure SetClipBox(ClipBox: TRectDouble); overload; virtual;

    function InRect(X, Y: Double): Boolean; virtual;

    function OnMouseButtonDown(X, Y: Double): Boolean; virtual;
    function OnMouseButtonUp(X, Y: Double): Boolean; virtual;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; virtual;
    function OnArrowKeys(Left, Right, Down, up: Boolean): Boolean; virtual;

    procedure Transform(Matrix: TAggTransAffine);
    procedure TransformXY(X, Y: PDouble);
    procedure InverseTransformXY(X, Y: PDouble); overload;
    procedure InverseTransformXY(var X, Y: Double); overload;
    procedure NoTransform;

    property ColorPointer[index: Cardinal]: PAggColor read GetColorPointer;
    property Scale: Double read GetScale;
  end;

procedure RenderControl(Ras: TAggRasterizerScanLine; SL: TAggCustomScanLine; R: TAggCustomRendererScanLineSolid; C: TAggCustomAggControl);

implementation


{ TAggCustomAggControl }

constructor TAggCustomAggControl.Create;
begin
  inherited Create;

  FRect.x1 := x1;
  FRect.y1 := y1;
  FRect.x2 := x2;
  FRect.y2 := y2;

  FFlipY := FlipY;

  FMatrix := nil;
end;

destructor TAggCustomAggControl.Destroy;
begin
  inherited;
end;

function TAggCustomAggControl.InRect(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseButtonDown(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean;
begin
  Result := False;
end;

procedure TAggCustomAggControl.SetClipBox(x1, y1, x2, y2: Double);
begin
  SetClipBox(RectDouble(x1, y1, x2, y2));
end;

procedure TAggCustomAggControl.SetClipBox(ClipBox: TRectDouble);
begin
  FRect := ClipBox;
end;

function TAggCustomAggControl.OnArrowKeys(Left, Right, Down, up: Boolean): Boolean;
begin
  Result := False;
end;

procedure TAggCustomAggControl.Transform(Matrix: TAggTransAffine);
begin
  FMatrix := Matrix;
end;

procedure TAggCustomAggControl.NoTransform;
begin
  FMatrix := nil;
end;

procedure TAggCustomAggControl.TransformXY(X, Y: PDouble);
begin
  if FFlipY then
      Y^ := FRect.y1 + FRect.y2 - Y^;

  if FMatrix <> nil then
      FMatrix.Transform(FMatrix, X, Y);
end;

procedure TAggCustomAggControl.InverseTransformXY(X, Y: PDouble);
begin
  if FMatrix <> nil then
      FMatrix.InverseTransform(FMatrix, X, Y);

  if FFlipY then
      Y^ := FRect.y1 + FRect.y2 - Y^;
end;

procedure TAggCustomAggControl.InverseTransformXY(var X, Y: Double);
begin
  InverseTransformXY(@X, @Y);
end;

function TAggCustomAggControl.GetScale: Double;
begin
  if FMatrix <> nil then
      Result := FMatrix.GetScale
  else
      Result := 1.0;
end;

procedure RenderControl(Ras: TAggRasterizerScanLine; SL: TAggCustomScanLine;
  R: TAggCustomRendererScanLineSolid; C: TAggCustomAggControl);
var
  i: Cardinal;
begin
  if C.PathCount > 0 then
    for i := 0 to C.PathCount - 1 do
      begin
        Ras.Reset;
        Ras.AddPath(C, i);

        R.SetColor(C.ColorPointer[i]);

        RenderScanLines(Ras, SL, R);
      end;
end;

end. 
 
