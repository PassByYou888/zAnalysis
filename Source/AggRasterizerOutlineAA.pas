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
unit AggRasterizerOutlineAA;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggColor32,
  AggLineAABasics,
  AggVertexSource,
  AggVertexSequence,
  AggRendererOutlineAA,
  AggControl;

type
  // Vertex (x, y) with the distance to the next one. The last vertex has
  // the distance between the last and the first points
  PAggLineAAVertex = ^TAggLineAAVertex;

  TAggLineAAVertex = packed record
    X, Y, Len: Integer;
    procedure Initialize(X, Y: Integer);
  end;

  PAggDrawVars = ^TAggDrawVars;

  TAggDrawVars = packed record
    idx: Cardinal;
    x1, y1, x2, y2: Integer;
    Curr, Next: TAggLineParameters;
    Lcurr, Lnext, Xb1, Yb1, Xb2, Yb2: Integer;
    Flags: Cardinal;
  end;

  TAggRasterizerOutlineAA = class
  private
    FRen: TAggRendererOutline;
    FSourceVertices: TAggVertexSequence;
    FAccurateJoin, FRoundCap: Boolean;
    FStart: TPointInteger;

    function GetAccurateJoin: Boolean;
    function GetRoundCap: Boolean;
    procedure SetAccurateJoin(v: Boolean);
    procedure SetRoundCap(v: Boolean);
  public
    constructor Create(Ren: TAggRendererOutline);
    destructor Destroy; override;

    procedure Renderer(Ren: TAggRendererOutline);

    procedure Draw(DV: PAggDrawVars; Start, stop: Cardinal);

    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);

    procedure MoveToDouble(X, Y: Double);
    procedure LineToDouble(X, Y: Double);

    procedure Render(ClosePolygon: Boolean);

    procedure AddVertex(X, Y: Double; Cmd: Cardinal);
    procedure AddPath(VertexSource: TAggCustomVertexSource; PathID: Cardinal = 0);

    procedure RenderAllPaths(VertexSource: TAggVertexSource; COLORS: PAggColor; PathID: PCardinal; PathCount: Cardinal);

    procedure RenderControl(C: TAggCustomAggControl);

    property RoundCap: Boolean read GetRoundCap write SetRoundCap;
    property AccurateJoin: Boolean read GetAccurateJoin write SetAccurateJoin;
  end;

function CompareDistStart(d: Integer): Boolean;
function CompareDistEnd(d: Integer): Boolean;

function LineAAVertexFuncOperator(This, val: PAggLineAAVertex): Boolean;

implementation


function CompareDistStart;
begin
  Result := d > 0;
end;

function CompareDistEnd;
begin
  Result := d <= 0;
end;

{ TAggLineAAVertex }

procedure TAggLineAAVertex.Initialize(X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
  Len := 0;
end;

function LineAAVertexFuncOperator;
var
  dx, dy: Double;

begin
  dx := val.X - This.X;
  dy := val.Y - This.Y;

  This.Len := Trunc(Sqrt(dx * dx + dy * dy));

  Result := This.Len > (CAggLineSubpixelSize + CAggLineSubpixelSize div 2);
end;

{ TAggRasterizerOutlineAA }

constructor TAggRasterizerOutlineAA.Create(Ren: TAggRendererOutline);
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggLineAAVertex), 6,
    @LineAAVertexFuncOperator);

  FRen := Ren;

  FAccurateJoin := FRen.AccurateJoinOnly;
  FRoundCap := False;

  FStart := PointInteger(0);
end;

destructor TAggRasterizerOutlineAA.Destroy;
begin
  FSourceVertices.Free;
  inherited;
end;

procedure TAggRasterizerOutlineAA.Renderer(Ren: TAggRendererOutline);
begin
  FRen := Ren;
end;

procedure TAggRasterizerOutlineAA.Draw(DV: PAggDrawVars; Start, stop: Cardinal);
var
  i: Cardinal;
  v: PAggLineAAVertex;
begin
  i := Start;

  while i < stop do
    begin
      case DV.Flags of
        0:
          FRen.Line3(@DV.Curr, DV.Xb1, DV.Yb1, DV.Xb2, DV.Yb2);
        1:
          FRen.Line2(@DV.Curr, DV.Xb2, DV.Yb2);
        2:
          FRen.Line1(@DV.Curr, DV.Xb1, DV.Yb1);
        3:
          FRen.Line0(@DV.Curr);
      end;

      DV.x1 := DV.x2;
      DV.y1 := DV.y2;

      DV.Lcurr := DV.Lnext;
      DV.Lnext := PAggLineAAVertex(FSourceVertices[DV.idx]).Len;

      Inc(DV.idx);

      if DV.idx >= FSourceVertices.Size then
          DV.idx := 0;

      v := FSourceVertices[DV.idx];

      DV.x2 := v.X;
      DV.y2 := v.Y;

      DV.Curr := DV.Next;

      DV.Next.Initialize(DV.x1, DV.y1, DV.x2, DV.y2, DV.Lnext);

      DV.Xb1 := DV.Xb2;
      DV.Yb1 := DV.Yb2;

      if FAccurateJoin then
          DV.Flags := 0
      else
        begin
          DV.Flags := DV.Flags shr 1;

          DV.Flags := DV.Flags or
            (Cardinal(DV.Curr.DiagonalQuadrant = DV.Next.DiagonalQuadrant) shl 1);
        end;

      if DV.Flags and 2 = 0 then
          Bisectrix(@DV.Curr, @DV.Next, @DV.Xb2, @DV.Yb2);

      Inc(i)
    end;
end;

procedure TAggRasterizerOutlineAA.SetAccurateJoin;
begin
  if FRen.AccurateJoinOnly then
      FAccurateJoin := True
  else
      FAccurateJoin := v;
end;

function TAggRasterizerOutlineAA.GetAccurateJoin;
begin
  Result := FAccurateJoin;
end;

procedure TAggRasterizerOutlineAA.SetRoundCap;
begin
  FRoundCap := v;
end;

function TAggRasterizerOutlineAA.GetRoundCap;
begin
  Result := FRoundCap;
end;

procedure TAggRasterizerOutlineAA.MoveTo(X, Y: Integer);
var
  VT: TAggLineAAVertex;
begin
  FStart := PointInteger(X, Y);

  VT.Initialize(X, Y);

  FSourceVertices.ModifyLast(@VT);
end;

procedure TAggRasterizerOutlineAA.LineTo(X, Y: Integer);
var
  VT: TAggLineAAVertex;
begin
  VT.Initialize(X, Y);

  FSourceVertices.Add(@VT);
end;

procedure TAggRasterizerOutlineAA.MoveToDouble(X, Y: Double);
begin
  MoveTo(LineCoord(X), LineCoord(Y));
end;

procedure TAggRasterizerOutlineAA.LineToDouble(X, Y: Double);
begin
  LineTo(LineCoord(X), LineCoord(Y));
end;

procedure TAggRasterizerOutlineAA.Render(ClosePolygon: Boolean);
var
  DV: TAggDrawVars;
  v: PAggLineAAVertex;
  x1, y1, x2, y2, Lprev, x3, y3, Lnext: Integer;
  Prev, LP, Lp1, Lp2: TAggLineParameters;
begin
  FSourceVertices.Close(ClosePolygon);

  if ClosePolygon then
    if FSourceVertices.Size >= 3 then
      begin
        DV.idx := 2;

        v := FSourceVertices[FSourceVertices.Size - 1];
        x1 := v.X;
        y1 := v.Y;
        Lprev := v.Len;

        v := FSourceVertices[0];
        x2 := v.X;
        y2 := v.Y;

        DV.Lcurr := v.Len;

        Prev.Initialize(x1, y1, x2, y2, Lprev);

        v := FSourceVertices[1];
        DV.x1 := v.X;
        DV.y1 := v.Y;

        DV.Lnext := v.Len;

        DV.Curr.Initialize(x2, y2, DV.x1, DV.y1, DV.Lcurr);

        v := FSourceVertices[DV.idx];
        DV.x2 := v.X;
        DV.y2 := v.Y;

        DV.Next.Initialize(DV.x1, DV.y1, DV.x2, DV.y2, DV.Lnext);

        DV.Xb1 := 0;
        DV.Yb1 := 0;
        DV.Xb2 := 0;
        DV.Yb2 := 0;

        if FAccurateJoin then
            DV.Flags := 0
        else
            DV.Flags := Cardinal(Prev.DiagonalQuadrant = DV.Curr.DiagonalQuadrant)
            or (Cardinal(DV.Curr.DiagonalQuadrant = DV.Next.DiagonalQuadrant) shl 1);

        if DV.Flags and 1 = 0 then
            Bisectrix(@Prev, @DV.Curr, @DV.Xb1, @DV.Yb1);

        if DV.Flags and 2 = 0 then
            Bisectrix(@DV.Curr, @DV.Next, @DV.Xb2, @DV.Yb2);

        Draw(@DV, 0, FSourceVertices.Size);
      end
    else
  else
    case FSourceVertices.Size of
      2:
        begin
          v := FSourceVertices[0];
          x1 := v.X;
          y1 := v.Y;
          Lprev := v.Len;
          v := FSourceVertices[1];
          x2 := v.X;
          y2 := v.Y;

          LP.Initialize(x1, y1, x2, y2, Lprev);

          if FRoundCap then
              FRen.Semidot(@CompareDistStart, x1, y1, x1 + (y2 - y1),
              y1 - (x2 - x1));

          FRen.Line3(@LP, x1 + (y2 - y1), y1 - (x2 - x1), x2 + (y2 - y1),
            y2 - (x2 - x1));

          if FRoundCap then
              FRen.Semidot(@CompareDistEnd, x2, y2, x2 + (y2 - y1),
              y2 - (x2 - x1));
        end;

      3:
        begin
          v := FSourceVertices[0];
          x1 := v.X;
          y1 := v.Y;
          Lprev := v.Len;
          v := FSourceVertices[1];
          x2 := v.X;
          y2 := v.Y;
          Lnext := v.Len;
          v := FSourceVertices[2];
          x3 := v.X;
          y3 := v.Y;

          Lp1.Initialize(x1, y1, x2, y2, Lprev);
          Lp2.Initialize(x2, y2, x3, y3, Lnext);

          Bisectrix(@Lp1, @Lp2, @DV.Xb1, @DV.Yb1);

          if FRoundCap then
              FRen.Semidot(@CompareDistStart, x1, y1, x1 + (y2 - y1),
              y1 - (x2 - x1));

          FRen.Line3(@Lp1, x1 + (y2 - y1), y1 - (x2 - x1), DV.Xb1, DV.Yb1);

          FRen.Line3(@Lp2, DV.Xb1, DV.Yb1, x3 + (y3 - y2), y3 - (x3 - x2));

          if FRoundCap then
              FRen.Semidot(@CompareDistEnd, x3, y3, x3 + (y3 - y2),
              y3 - (x3 - x2));
        end;

      0, 1:
      else
        begin
          DV.idx := 3;

          v := FSourceVertices[0];
          x1 := v.X;
          y1 := v.Y;
          Lprev := v.Len;

          v := FSourceVertices[1];
          x2 := v.X;
          y2 := v.Y;

          DV.Lcurr := v.Len;

          Prev.Initialize(x1, y1, x2, y2, Lprev);

          v := FSourceVertices[2];
          DV.x1 := v.X;
          DV.y1 := v.Y;

          DV.Lnext := v.Len;

          DV.Curr.Initialize(x2, y2, DV.x1, DV.y1, DV.Lcurr);

          v := FSourceVertices[DV.idx];
          DV.x2 := v.X;
          DV.y2 := v.Y;

          DV.Next.Initialize(DV.x1, DV.y1, DV.x2, DV.y2, DV.Lnext);

          DV.Xb1 := 0;
          DV.Yb1 := 0;
          DV.Xb2 := 0;
          DV.Yb2 := 0;

          if FAccurateJoin then
              DV.Flags := 0
          else
              DV.Flags :=
              Cardinal(Prev.DiagonalQuadrant = DV.Curr.DiagonalQuadrant) or
              (Cardinal(DV.Curr.DiagonalQuadrant = DV.Next.
              DiagonalQuadrant) shl 1);

          if DV.Flags and 1 = 0 then
            begin
              Bisectrix(@Prev, @DV.Curr, @DV.Xb1, @DV.Yb1);
              FRen.Line3(@Prev, x1 + (y2 - y1), y1 - (x2 - x1), DV.Xb1, DV.Yb1);

            end
          else
              FRen.Line1(@Prev, x1 + (y2 - y1), y1 - (x2 - x1));

          if FRoundCap then
              FRen.Semidot(@CompareDistStart, x1, y1, x1 + (y2 - y1),
              y1 - (x2 - x1));

          if DV.Flags and 2 = 0 then
              Bisectrix(@DV.Curr, @DV.Next, @DV.Xb2, @DV.Yb2);

          Draw(@DV, 1, FSourceVertices.Size - 2);

          if DV.Flags and 1 = 0 then
              FRen.Line3(@DV.Curr, DV.Xb1, DV.Yb1,
              DV.Curr.x2 + (DV.Curr.y2 - DV.Curr.y1),
              DV.Curr.y2 - (DV.Curr.x2 - DV.Curr.x1))
          else
              FRen.Line2(@DV.Curr, DV.Curr.x2 + (DV.Curr.y2 - DV.Curr.y1),
              DV.Curr.y2 - (DV.Curr.x2 - DV.Curr.x1));

          if FRoundCap then
              FRen.Semidot(@CompareDistEnd, DV.Curr.x2, DV.Curr.y2,
              DV.Curr.x2 + (DV.Curr.y2 - DV.Curr.y1),
              DV.Curr.y2 - (DV.Curr.x2 - DV.Curr.x1));
        end;
    end;

  FSourceVertices.RemoveAll;
end;

procedure TAggRasterizerOutlineAA.AddVertex;
begin
  if IsMoveTo(Cmd) then
    begin
      Render(False);
      MoveToDouble(X, Y);
    end
  else if IsEndPoly(Cmd) then
    begin
      Render(IsClosed(Cmd));

      if IsClosed(Cmd) then
          MoveTo(FStart.X, FStart.Y);
    end
  else
      LineToDouble(X, Y);
end;

procedure TAggRasterizerOutlineAA.AddPath(VertexSource: TAggCustomVertexSource; PathID: Cardinal = 0);
var
  X, Y: Double;
  Cmd: Cardinal;
begin
  VertexSource.Rewind(PathID);

  Cmd := VertexSource.Vertex(@X, @Y);

  while not IsStop(Cmd) do
    begin
      AddVertex(X, Y, Cmd);

      Cmd := VertexSource.Vertex(@X, @Y);
    end;

  Render(False);
end;

procedure TAggRasterizerOutlineAA.RenderAllPaths;
var
  i: Cardinal;
begin
  for i := 0 to PathCount - 1 do
    begin
      FRen.SetColor(PAggColor(PtrComp(COLORS) + i * SizeOf(TAggColor)));
      AddPath(VertexSource, PCardinal(PtrComp(PathID) + i * SizeOf(Cardinal))^);
    end;
end;

procedure TAggRasterizerOutlineAA.RenderControl;
var
  i: Cardinal;
begin
  for i := 0 to C.PathCount - 1 do
    begin
      FRen.SetColor(C.ColorPointer[i]);
      AddPath(C, i);
    end;
end;

end. 
 
