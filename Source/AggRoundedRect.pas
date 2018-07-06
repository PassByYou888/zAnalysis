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
unit AggRoundedRect;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggVertexSource,
  AggArc;

type
  TAggRoundedRect = class(TAggVertexSource)
  private
    FPoint: array [0 .. 1] of TPointDouble;
    FRadius: array [0 .. 3] of TPointDouble;

    FStatus: Cardinal;
    FArc: TAggArc;

    procedure SetApproximationScale(Value: Double);
    function GetApproximationScale: Double;
  public
    constructor Create; overload;
    constructor Create(x1, y1, x2, y2, R: Double); overload;
    destructor Destroy; override;

    procedure Rect(x1, y1, x2, y2: Double); overload;
    procedure Rect(Value: TRectDouble); overload;

    procedure radius(R: Double); overload;
    procedure radius(RX, RY: Double); overload;
    procedure radius(radius: TPointDouble); overload;
    procedure radius(BottomX, BottomY, TopX, TopY: Double); overload;
    procedure radius(Bottom, Top: TPointDouble); overload;
    procedure radius(Rx1, Ry1, Rx2, Ry2, Rx3, Ry3, Rx4, Ry4: Double); overload;
    procedure radius(Radius1, Radius2, Radius3, Radius4: TPointDouble); overload;

    procedure NormalizeRadius;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property ApproximationScale: Double read GetApproximationScale write SetApproximationScale;
  end;

implementation


{ TAggRoundedRect }

constructor TAggRoundedRect.Create;
begin
  FPoint[0] := PointDouble(0);
  FPoint[1] := PointDouble(0);
  FRadius[0] := PointDouble(0);
  FRadius[1] := PointDouble(0);
  FRadius[2] := PointDouble(0);
  FRadius[3] := PointDouble(0);

  FStatus := 0;

  FArc := TAggArc.Create;
end;

constructor TAggRoundedRect.Create(x1, y1, x2, y2, R: Double);
begin
  Create;

  FPoint[0] := PointDouble(x1, y1);
  FPoint[1] := PointDouble(x2, y2);
  FRadius[0] := PointDouble(R);
  FRadius[1] := PointDouble(R);
  FRadius[2] := PointDouble(R);
  FRadius[3] := PointDouble(R);

  if x1 > x2 then
    begin
      FPoint[0].X := x2;
      FPoint[1].X := x1;
    end;

  if y1 > y2 then
    begin
      FPoint[0].Y := y2;
      FPoint[1].Y := y1;
    end;
end;

destructor TAggRoundedRect.Destroy;
begin
  FArc.Free;
  inherited;
end;

procedure TAggRoundedRect.Rect(x1, y1, x2, y2: Double);
begin
  FPoint[0] := PointDouble(x1, y1);
  FPoint[1] := PointDouble(x2, y2);

  if x1 > x2 then
    begin
      FPoint[0].X := x2;
      FPoint[1].X := x1;
    end;

  if y1 > y2 then
    begin
      FPoint[0].Y := y2;
      FPoint[1].Y := y1;
    end;
end;

procedure TAggRoundedRect.Rect(Value: TRectDouble);
begin
  FPoint[0] := PointDouble(Value.x1, Value.y1);
  FPoint[1] := PointDouble(Value.x2, Value.y2);

  if Value.x1 > Value.x2 then
    begin
      FPoint[0].X := Value.x2;
      FPoint[1].X := Value.x1;
    end;

  if Value.y1 > Value.y2 then
    begin
      FPoint[0].Y := Value.y2;
      FPoint[1].Y := Value.y1;
    end;
end;

procedure TAggRoundedRect.radius(R: Double);
begin
  FRadius[0] := PointDouble(R);
  FRadius[1] := PointDouble(R);
  FRadius[2] := PointDouble(R);
  FRadius[3] := PointDouble(R);
end;

procedure TAggRoundedRect.radius(RX, RY: Double);
begin
  FRadius[0] := PointDouble(RX, RY);
  FRadius[1] := PointDouble(RX, RY);
  FRadius[2] := PointDouble(RX, RY);
  FRadius[3] := PointDouble(RX, RY);
end;

procedure TAggRoundedRect.radius(radius: TPointDouble);
begin
  FRadius[0] := radius;
  FRadius[1] := radius;
  FRadius[2] := radius;
  FRadius[3] := radius;
end;

procedure TAggRoundedRect.radius(BottomX, BottomY, TopX, TopY: Double);
begin
  FRadius[0] := PointDouble(BottomX, BottomY);
  FRadius[1] := PointDouble(BottomX, BottomY);
  FRadius[2] := PointDouble(TopX, TopY);
  FRadius[3] := PointDouble(TopX, TopY);
end;

procedure TAggRoundedRect.radius(Bottom, Top: TPointDouble);
begin
  FRadius[0] := Bottom;
  FRadius[1] := Bottom;
  FRadius[2] := Top;
  FRadius[3] := Top;
end;

procedure TAggRoundedRect.radius(Rx1, Ry1, Rx2, Ry2, Rx3, Ry3, Rx4, Ry4: Double);
begin
  FRadius[0] := PointDouble(Rx1, Ry1);
  FRadius[1] := PointDouble(Rx2, Ry2);
  FRadius[2] := PointDouble(Rx3, Ry3);
  FRadius[3] := PointDouble(Rx4, Ry4);
end;

procedure TAggRoundedRect.radius(Radius1, Radius2, Radius3,
  Radius4: TPointDouble);
begin
  FRadius[0] := Radius1;
  FRadius[1] := Radius2;
  FRadius[2] := Radius3;
  FRadius[3] := Radius4;
end;

procedure TAggRoundedRect.NormalizeRadius;
var
  Delta: TPointDouble;
  k, T: Double;
begin
  Delta.X := Abs(FPoint[1].Y - FPoint[0].Y);
  Delta.Y := Abs(FPoint[1].X - FPoint[0].X);

  k := 1.0;
  try
    T := Delta.X / (FRadius[0].X + FRadius[1].X);

    if T < k then
        k := T;
  except
  end;

  try
    T := Delta.X / (FRadius[2].X + FRadius[3].X);

    if T < k then
        k := T;
  except
  end;

  try
    T := Delta.Y / (FRadius[0].Y + FRadius[1].Y);

    if T < k then
        k := T;
  except
  end;

  try
    T := Delta.Y / (FRadius[2].Y + FRadius[3].Y);

    if T < k then
        k := T;
  except
  end;

  if k < 1.0 then
    begin
      FRadius[0].X := FRadius[0].X * k;
      FRadius[0].Y := FRadius[0].Y * k;
      FRadius[1].X := FRadius[1].X * k;
      FRadius[1].Y := FRadius[1].Y * k;
      FRadius[2].X := FRadius[2].X * k;
      FRadius[2].Y := FRadius[2].Y * k;
      FRadius[3].X := FRadius[3].X * k;
      FRadius[3].Y := FRadius[3].Y * k;
    end;
end;

procedure TAggRoundedRect.SetApproximationScale(Value: Double);
begin
  FArc.ApproximationScale := Value;
end;

function TAggRoundedRect.GetApproximationScale: Double;
begin
  Result := FArc.ApproximationScale;
end;

procedure TAggRoundedRect.Rewind(PathID: Cardinal);
begin
  FStatus := 0;
end;

function TAggRoundedRect.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
label
  _1, _2, _3, _4, _5, _6, _7, _8;
begin
  Cmd := CAggPathCmdStop;

  case FStatus of
    0:
      begin
        FArc.Init(FPoint[0].X + FRadius[0].X, FPoint[0].Y + FRadius[0].Y,
          FRadius[0].X, FRadius[0].Y, pi, pi + pi * 0.5);
        FArc.Rewind(0);

        Inc(FStatus);

        goto _1;
      end;

    1:
    _1:
      begin
        Cmd := FArc.Vertex(X, Y);

        if IsStop(Cmd) then
          begin
            Inc(FStatus);
            goto _2;
          end
        else
          begin
            Result := Cmd;
            Exit;
          end;
      end;

    2:
    _2:
      begin
        FArc.Init(FPoint[1].X - FRadius[1].X, FPoint[0].Y + FRadius[1].Y,
          FRadius[1].X, FRadius[1].Y, pi + pi * 0.5, 0.0);
        FArc.Rewind(0);

        Inc(FStatus);
        goto _3;
      end;

    3:
    _3:
      begin
        Cmd := FArc.Vertex(X, Y);

        if IsStop(Cmd) then
          begin
            Inc(FStatus);
            goto _4;
          end
        else
          begin
            Result := CAggPathCmdLineTo;
            Exit;
          end;
      end;

    4:
    _4:
      begin
        FArc.Init(FPoint[1].X - FRadius[2].X, FPoint[1].Y - FRadius[2].Y,
          FRadius[2].X, FRadius[2].Y, 0.0, pi * 0.5);
        FArc.Rewind(0);

        Inc(FStatus);
        goto _5;
      end;

    5:
    _5:
      begin
        Cmd := FArc.Vertex(X, Y);

        if IsStop(Cmd) then
          begin
            Inc(FStatus);
            goto _6;
          end
        else
          begin
            Result := CAggPathCmdLineTo;
            Exit;
          end;
      end;

    6:
    _6:
      begin
        FArc.Init(FPoint[0].X + FRadius[3].X, FPoint[1].Y - FRadius[3].Y,
          FRadius[3].X, FRadius[3].Y, pi * 0.5, pi);
        FArc.Rewind(0);

        Inc(FStatus);
        goto _7;
      end;

    7:
    _7:
      begin
        Cmd := FArc.Vertex(X, Y);

        if IsStop(Cmd) then
          begin
            Inc(FStatus);
            goto _8;
          end
        else
          begin
            Result := CAggPathCmdLineTo;
            Exit;
          end;
      end;

    8:
    _8:
      begin
        Cmd := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;
        Inc(FStatus);
      end;
  end;

  Result := Cmd;
end;

end. 
