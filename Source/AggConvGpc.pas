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
unit AggConvGpc;

/// /////////////////////////////////////////////////////////////////////////////
// //
// General Polygon Clipper based on the GPC library by Alan Murta            //
// Union, Intersection, XOR, A-B, B-A                                        //
// Contact the author if you intend to use it in commercial applications!    //
// http://www.cs.man.ac.uk/aig/staff/alan/software/                          //
// Alan Murta (email: gpc@cs.man.ac.uk)                                      //
// //
/// /////////////////////////////////////////////////////////////////////////////

interface

{$INCLUDE AggCompiler.inc}


uses
  gpc,
  AggBasics,
  AggArray,
  AggVertexSource;

type
  TAggGpcOp = (goOr, goAnd, goXor, goAMinusB, goBMinusA);

  TAggInternalStatus = (siMoveTo, siLineTo, siStop);

  PAggGpcVertex = ^TGpcVertex;

  PAggContourHeader = ^TAggContourHeader;

  TAggContourHeader = packed record
    NumVertices, HoleFlag: Integer;
    Vertices: PAggGpcVertex;
  end;

  TAggConvGpc = class(TAggVertexSource)
  private
    FSourceA, FSourceB: TAggVertexSource;

    FStatus: TAggInternalStatus;
    FVertex, FContour: Integer;
    FOperation: TAggGpcOp;

    FVertexAccumulator, FContourAccumulator: TAggPodDeque;

    FPolygonA, FPolygonB, FResult: TGpcPolygon;

    // Private
    procedure FreePolygon(p: PGpcPolygon);
    procedure FreeResult;
    procedure FreeGpcData;
    procedure StartContour;
    procedure SetAddVertex(X, Y: Double);
    procedure EndContour(Orientation: Cardinal);
    procedure MakePolygon(p: PGpcPolygon);
    procedure StartExtracting;

    function NextContour: Boolean;
    function NextVertex(X, Y: PDouble): Boolean;

    procedure Add(Src: TAggVertexSource; p: PGpcPolygon);
  public
    constructor Create(A, b: TAggVertexSource; Op: TAggGpcOp = goOr);
    destructor Destroy; override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Operation: TAggGpcOp read FOperation write FOperation;
    property SourceA: TAggVertexSource read FSourceA write FSourceA;
    property SourceB: TAggVertexSource read FSourceB write FSourceB;
  end;

implementation


{ TAggConvGpc }

constructor TAggConvGpc.Create(A, b: TAggVertexSource; Op: TAggGpcOp = goOr);
begin
  FVertexAccumulator := TAggPodDeque.Create(SizeOf(TGpcVertex), 8);
  FContourAccumulator := TAggPodDeque.Create(SizeOf(TAggContourHeader), 6);

  FSourceA := A;
  FSourceB := b;

  FStatus := siMoveTo;
  FVertex := -1;
  FContour := -1;
  FOperation := Op;

  FillChar(FPolygonA, SizeOf(FPolygonA), 0);
  FillChar(FPolygonB, SizeOf(FPolygonB), 0);
  FillChar(FResult, SizeOf(FResult), 0);
end;

destructor TAggConvGpc.Destroy;
begin
  FreeGpcData;

  FVertexAccumulator.Free;
  FContourAccumulator.Free;

  inherited;
end;

procedure TAggConvGpc.Rewind(PathID: Cardinal);
begin
  FreeResult;

  FSourceA.Rewind(PathID);
  FSourceB.Rewind(PathID);

  Add(FSourceA, @FPolygonA);
  Add(FSourceB, @FPolygonB);

  case FOperation of
    goOr:
      GpcPolygonClip(gpc.goUnion, @FPolygonA, @FPolygonB, @FResult);

    goAnd:
      GpcPolygonClip(gpc.goInt, @FPolygonA, @FPolygonB, @FResult);

    goXor:
      GpcPolygonClip(gpc.goXor, @FPolygonA, @FPolygonB, @FResult);

    goAMinusB:
      GpcPolygonClip(gpc.goDiff, @FPolygonA, @FPolygonB, @FResult);

    goBMinusA:
      GpcPolygonClip(gpc.goDiff, @FPolygonB, @FPolygonA, @FResult);
  end;

  StartExtracting;
end;

function TAggConvGpc.Vertex(X, Y: PDouble): Cardinal;
begin
  if FStatus = siMoveTo then
    if NextContour then
      begin
        if NextVertex(X, Y) then
          begin
            FStatus := siLineTo;
            Result := CAggPathCmdMoveTo;

            Exit;
          end;

        FStatus := siStop;
        Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

        Exit;

      end
    else
  else
    begin
      if NextVertex(X, Y) then
        begin
          Result := CAggPathCmdLineTo;

          Exit;

        end
      else
          FStatus := siMoveTo;

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

      Exit;
    end;

  Result := CAggPathCmdStop;
end;

procedure TAggConvGpc.FreePolygon(p: PGpcPolygon);
var
  i: Integer;
begin
  i := 0;

  while i < p.NumContours do
    begin
      AggFreeMem(Pointer(p.Contour[i].Vertex), p.Contour[i].NumVertices *
        SizeOf(TGpcVertex));

      Inc(i);
    end;

  // AggFreeMem(pointer(p.hole ) ,? );
  AggFreeMem(Pointer(p.Contour), p.NumContours * SizeOf(TGpcVertexList));

  FillChar(p^, SizeOf(TGpcPolygon), 0);
end;

procedure TAggConvGpc.FreeResult;
begin
  if FResult.Contour <> nil then
      GpcFreePolygon(@FResult);

  FillChar(FResult, SizeOf(FResult), 0);
end;

procedure TAggConvGpc.FreeGpcData;
begin
  FreePolygon(@FPolygonA);
  FreePolygon(@FPolygonB);
  FreeResult;
end;

procedure TAggConvGpc.StartContour;
var
  h: TAggContourHeader;
begin
  FillChar(h, SizeOf(h), 0);
  FContourAccumulator.Add(@h);
  FVertexAccumulator.RemoveAll;
end;

procedure TAggConvGpc.SetAddVertex(X, Y: Double);
var
  v: TGpcVertex;
begin
  v.X := X;
  v.Y := Y;

  FVertexAccumulator.Add(@v);
end;

procedure TAggConvGpc.EndContour(Orientation: Cardinal);
var
  h: PAggContourHeader;
  d, s: PAggGpcVertex;
  i: Integer;
begin
  if FContourAccumulator.Size <> 0 then
    if FVertexAccumulator.Size > 2 then
      begin
        h := FContourAccumulator[FContourAccumulator.Size - 1];

        h.NumVertices := FVertexAccumulator.Size;
        h.HoleFlag := 0;

        // TO DO: Clarify the "holes"
        // if IsClockwise(orientation ) then h.HoleFlag:=1;

        AggGetMem(Pointer(h.Vertices), h.NumVertices * SizeOf(TGpcVertex));

        d := h.Vertices;

        for i := 0 to h.NumVertices - 1 do
          begin
            s := FVertexAccumulator[i];

            d.X := s.X;
            d.Y := s.Y;

            Inc(PtrComp(d), SizeOf(TGpcVertex));
          end;

      end
    else
        FVertexAccumulator.RemoveLast;
end;

procedure TAggConvGpc.MakePolygon(p: PGpcPolygon);
var
  i: Integer;
  h: PAggContourHeader;

  // ph : PInteger;
  PV: PGpcVertexList;
begin
  FreePolygon(p);

  if FContourAccumulator.Size <> 0 then
    begin
      p.NumContours := FContourAccumulator.Size;

      // TO DO: Clarify the "holes"
      // p.hole = new goInt[p.NumContours];

      p.Hole := nil;

      AggGetMem(Pointer(p.Contour), p.NumContours * SizeOf(TGpcVertexList));

      // ph:=p.hole;
      PV := PGpcVertexList(p.Contour);

      if p.NumContours > 0 then
        for i := 0 to p.NumContours - 1 do
          begin
            h := FContourAccumulator[i];

            // *ph++ = h.HoleFlag;
            PV.NumVertices := h.NumVertices;
            PV.Vertex := PGpcVertexArray(h.Vertices);

            Inc(PtrComp(PV), SizeOf(TGpcVertexList));
          end;
    end;
end;

procedure TAggConvGpc.StartExtracting;
begin
  FStatus := siMoveTo;
  FContour := -1;
  FVertex := -1;
end;

function TAggConvGpc.NextContour;
begin
  Inc(FContour);

  if FContour < FResult.NumContours then
    begin
      FVertex := -1;

      Result := True;

    end
  else
      Result := False;
end;

function TAggConvGpc.NextVertex(X, Y: PDouble): Boolean;
var
  Vlist: PGpcVertexList;
  v: PAggGpcVertex;
begin
  Vlist := @FResult.Contour[FContour];

  Inc(FVertex);

  if FVertex < Vlist.NumVertices then
    begin
      v := @Vlist.Vertex[FVertex];

      X^ := v.X;
      Y^ := v.Y;

      Result := True;

    end
  else
      Result := False;
end;

procedure TAggConvGpc.Add(Src: TAggVertexSource; p: PGpcPolygon);
var
  Cmd, Orientation: Cardinal;
  X, Y, StartX, StartY: Double;
  LineTo: Boolean;
begin
  StartX := 0.0;
  StartY := 0.0;
  LineTo := False;

  Orientation := 0;

  FContourAccumulator.RemoveAll;

  Cmd := Src.Vertex(@X, @Y);

  while not IsStop(Cmd) do
    begin
      if IsVertex(Cmd) then
        begin
          if IsMoveTo(Cmd) then
            begin
              if LineTo then
                begin
                  EndContour(Orientation);

                  Orientation := 0;
                end;

              StartContour;

              StartX := X;
              StartY := Y;
            end;

          SetAddVertex(X, Y);

          LineTo := True;

        end
      else if IsEndPoly(Cmd) then
        begin
          Orientation := GetOrientation(Cmd);

          if LineTo and IsClosed(Cmd) then
              SetAddVertex(StartX, StartY);
        end;

      Cmd := Src.Vertex(@X, @Y);
    end;

  if LineTo then
      EndContour(Orientation);

  MakePolygon(p);
end;

end.   
