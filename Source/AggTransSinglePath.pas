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
unit AggTransSinglePath;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggVertexSource,
  AggVertexSequence,
  AggTransAffine;

type
  TAggInternalStatus = (siInitial, siMakingPath, siReady);

  TAggTransSinglePath = class(TAggTransAffine)
  private
    FSourceVertices: TAggVertexSequence;
    FBaseLength, FKIndex: Double;

    FStatus: TAggInternalStatus;

    FPreserveXScale: Boolean;

    procedure SetBaseLength(v: Double);
    procedure SetPreserveXScale(F: Boolean);
    function GetTotalLength: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; virtual;

    procedure MoveTo(X, Y: Double);
    procedure LineTo(X, Y: Double);
    procedure FinalizePath;

    procedure AddPath(Vs: TAggVertexSource; PathID: Cardinal = 0);

    property TotalLength: Double read GetTotalLength;
    property BaseLength: Double read FBaseLength write SetBaseLength;
    property PreserveXScale: Boolean read FPreserveXScale write SetPreserveXScale;
  end;

implementation


procedure SinglePathTransform(This: TAggTransSinglePath; X, Y: PDouble);
var
  Rect: TRectDouble;
  Delta: TPointDouble;
  d, DD: Double;
  i, J, k: Cardinal;
begin
  if This.FStatus = siReady then
    begin
      if This.FBaseLength > 1E-10 then
          X^ := X^ * (PAggVertexDistance(This.FSourceVertices[
          This.FSourceVertices.Size - 1]).Dist / This.FBaseLength);

      Rect.x1 := 0;
      Rect.y1 := 0;
      Delta.X := 1;
      Delta.Y := 1;
      d := 0;
      DD := 1;

      if X^ < 0 then
        begin
          // Extrapolation on the left
          Rect.x1 := PAggVertexDistance(This.FSourceVertices[0]).pos.X;
          Rect.y1 := PAggVertexDistance(This.FSourceVertices[0]).pos.Y;
          Delta.X := PAggVertexDistance(This.FSourceVertices[1]).pos.X - Rect.x1;
          Delta.Y := PAggVertexDistance(This.FSourceVertices[1]).pos.Y - Rect.y1;

          DD := PAggVertexDistance(This.FSourceVertices[1]).Dist -
            PAggVertexDistance(This.FSourceVertices[0]).Dist;

          d := X^;
        end
      else if X^ > PAggVertexDistance(This.FSourceVertices[
        This.FSourceVertices.Size - 1]).Dist then
        begin
          // Extrapolation on the right
          i := This.FSourceVertices.Size - 2;
          J := This.FSourceVertices.Size - 1;

          Rect.x1 := PAggVertexDistance(This.FSourceVertices[J]).pos.X;
          Rect.y1 := PAggVertexDistance(This.FSourceVertices[J]).pos.Y;
          Delta.X := Rect.x1 - PAggVertexDistance(This.FSourceVertices[i]).pos.X;
          Delta.Y := Rect.y1 - PAggVertexDistance(This.FSourceVertices[i]).pos.Y;

          DD := PAggVertexDistance(This.FSourceVertices[J]).Dist -
            PAggVertexDistance(This.FSourceVertices[i]).Dist;

          d := X^ - PAggVertexDistance(This.FSourceVertices[J]).Dist;
        end
      else
        begin
          // Interpolation
          i := 0;
          J := This.FSourceVertices.Size - 1;

          if This.FPreserveXScale then
            begin
              i := 0;

              while J - i > 1 do
                begin
                  k := (i + J) shr 1;

                  if X^ < PAggVertexDistance(This.FSourceVertices[k]).Dist
                  then
                      J := k
                  else
                      i := k;
                end;

              d := PAggVertexDistance(This.FSourceVertices[i]).Dist;
              DD := PAggVertexDistance(This.FSourceVertices[J]).Dist - d;
              d := X^ - d;
            end
          else
            begin
              i := Trunc(X^ * This.FKIndex);
              J := i + 1;

              DD := PAggVertexDistance(This.FSourceVertices[J]).Dist -
                PAggVertexDistance(This.FSourceVertices[i]).Dist;

              d := ((X^ * This.FKIndex) - i) * DD;
            end;

          Rect.x1 := PAggVertexDistance(This.FSourceVertices[i]).pos.X;
          Rect.y1 := PAggVertexDistance(This.FSourceVertices[i]).pos.Y;
          Delta.X := PAggVertexDistance(This.FSourceVertices[J]).pos.X - Rect.x1;
          Delta.Y := PAggVertexDistance(This.FSourceVertices[J]).pos.Y - Rect.y1;
        end;

      DD := 1 / DD;
      Rect.x2 := Rect.x1 + Delta.X * d * DD;
      Rect.y2 := Rect.y1 + Delta.Y * d * DD;
      X^ := Rect.x2 - Y^ * Delta.Y * DD;
      Y^ := Rect.y2 + Y^ * Delta.X * DD;
    end;
end;

{ TAggTransSinglePath }

constructor TAggTransSinglePath.Create;
begin
  inherited Create;

  Transform := @SinglePathTransform;

  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));

  FBaseLength := 0.0;
  FKIndex := 0.0;

  FStatus := siInitial;

  FPreserveXScale := True;
end;

destructor TAggTransSinglePath.Destroy;
begin
  FSourceVertices.Free;
  inherited
end;

procedure TAggTransSinglePath.SetBaseLength(v: Double);
begin
  FBaseLength := v;
end;

procedure TAggTransSinglePath.SetPreserveXScale(F: Boolean);
begin
  FPreserveXScale := F;
end;

procedure TAggTransSinglePath.Reset;
begin
  FSourceVertices.RemoveAll;

  FKIndex := 0.0;
  FStatus := siInitial;
end;

procedure TAggTransSinglePath.MoveTo(X, Y: Double);
var
  VD: TAggVertexDistance;
begin
  if FStatus = siInitial then
    begin
      VD.pos := PointDouble(X, Y);
      VD.Dist := 0;

      FSourceVertices.ModifyLast(@VD);

      FStatus := siMakingPath;
    end
  else
      LineTo(X, Y);
end;

procedure TAggTransSinglePath.LineTo(X, Y: Double);
var
  VD: TAggVertexDistance;
begin
  if FStatus = siMakingPath then
    begin
      VD.pos := PointDouble(X, Y);
      VD.Dist := 0;

      FSourceVertices.Add(@VD);
    end;
end;

procedure TAggTransSinglePath.FinalizePath;
var
  i: Cardinal;
  v: PAggVertexDistance;
  Dist, d: Double;
begin
  if (FStatus = siMakingPath) and (FSourceVertices.Size > 1) then
    begin
      FSourceVertices.Close(False);

      if FSourceVertices.Size > 2 then
        if PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 2]).Dist * 10
          < PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 3]).Dist
        then
          begin
            d := PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 3]).Dist
              + PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 2]).Dist;

            Move(FSourceVertices[FSourceVertices.Size - 1]^,
              FSourceVertices[FSourceVertices.Size - 2]^,
              SizeOf(TAggVertexDistance));

            FSourceVertices.RemoveLast;

            PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 2]).Dist := d;
          end;

      Dist := 0.0;

      for i := 0 to FSourceVertices.Size - 1 do
        begin
          v := FSourceVertices[i];
          d := v.Dist;

          v.Dist := Dist;
          Dist := Dist + d;
        end;

      FKIndex := (FSourceVertices.Size - 1) / Dist;
      FStatus := siReady;
    end;
end;

procedure TAggTransSinglePath.AddPath;
var
  X, Y: Double;
  Cmd: Cardinal;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
    begin
      if IsMoveTo(Cmd) then
          MoveTo(X, Y)
      else if IsVertex(Cmd) then
          LineTo(X, Y);

      Cmd := Vs.Vertex(@X, @Y);
    end;

  FinalizePath;
end;

function TAggTransSinglePath.GetTotalLength;
begin
  if FBaseLength >= 1E-10 then
    begin
      Result := FBaseLength;
      Exit;
    end;

  if FStatus = siReady then
      Result := PAggVertexDistance(FSourceVertices[FSourceVertices.Size - 1]).Dist
  else
      Result := 0.0
end;

end. 
 
