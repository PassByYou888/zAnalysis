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

unit gpc;

(*
  ===========================================================================

  Project:   Generic Polygon Clipper

  A new algorithm for calculating the difference, intersection,
  exclusive-or or union of arbitrary Polygon sets.

  File:      GPC.pas
  Author:    Alan Murta (GPC@cs.man.ac.uk)
  CVersion:   2.32 last modify 2.33, by 600585@qq.com
  Date:      17hesTH December 2004

  Copyright: (C) Advanced Interfaces Group,
  University of Manchester.

  This software may be freely copied, modified, and redistributed
  provided that this copyright notice is preserved on all copies.
  The intellectual property rights of the algorithms used reside
  with the University of Manchester Advanced Interfaces Group.

  You may not distribute this software, in whole or in part, as
  part of any commercial product without the express consent of
  the author.

  There is no warranty or other guarantee of fitness of this
  software for any purpose. It is provided solely "as is".

  ===========================================================================

  Ported to Delphi by Richard B. Winston (rbwinst@usgs.gov) Dec. 17, 2008.
  Based in part on a previous port by Stefan Schedel.

  Mar. 18, 2009 Correction submitted by (cesar.aguilar@gmx.net)
*)

interface

{$INCLUDE AggCompiler.inc}

uses CoreClasses, Math;

const
  CVersion            = 'GPC_VERSION "2.32"';
  CGPCEpsilon: Double = 2.2204460492503131E-16; { from float.h }

type
  TGpcOp =   { Set operation type }
    (goDiff, { Difference }
    goInt,   { Intersection }
    goXor,   { Exclusive or }
    goUnion  { Union }
    );

  TGpcVertex = packed record { Polygon Vertex structure }
    X: Double;               { Vertex x component }
    Y: Double;               { Vertex y component }
  end;

  PGpcVertexArray = ^TGpcVertexArray; { Helper Type for indexing }
  TGpcVertexArray = array [0 .. MaxInt div SizeOf(TGpcVertex) - 1] of TGpcVertex;

  PGpcVertexList = ^TGpcVertexList; { Vertex list structure }

  TGpcVertexList = packed record
    NumVertices: Integer;    { Number of vertices in list }
    Vertex: PGpcVertexArray; { Vertex array pointer }
  end;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array [0 .. MaxInt div SizeOf(Integer) - 1] of Integer;

  PGpcVertexListArray = ^TGpcVertexListArray; { Helper Type for indexing }
  TGpcVertexListArray = array [0 .. MaxInt div SizeOf(TGpcVertexList) - 1] of TGpcVertexList;

  PGpcPolygon = ^TGpcPolygon;

  TGpcPolygon = packed record     { Polygon set structure }
    NumContours: Integer;         { Number of contours in Polygon }
    Hole: PIntegerArray;          { Hole / external Contour flags }
    Contour: PGpcVertexListArray; { Contour array pointer }
  end;

  PGpcTristrip = ^TGpcTriStrip; { TriStrip set structure }

  TGpcTriStrip = packed record
    NumStrips: Integer;         { Number of tristrips }
    Strip: PGpcVertexListArray; { TriStrip array pointer }
  end;

procedure GpcAddContour(Polygon: PGpcPolygon; Contour: PGpcVertexList; Hole: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure GpcPolygonClip(SetOperation: TGpcOp; SubjectPolygon: PGpcPolygon; ClipPolygon: PGpcPolygon; ResultPolygon: PGpcPolygon);
procedure GpcTristripClip(Op: TGpcOp; CSubj: PGpcPolygon; CClip: PGpcPolygon; Result: PGpcTristrip);
procedure GpcPolygonToTristrip(s: PGpcPolygon; T: PGpcTristrip); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure GpcFreePolygon(Polygon: PGpcPolygon); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure GpcFreeTristrip(TriStrip: PGpcTristrip); {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation

// ===========================================================================
// Constants
// ===========================================================================

const
  CDoubleMax: Double = MaxDouble;
  CDoubleDig         = 15;
  FFALSE             = 0;
  FTRUE              = 1;
  CLeft              = 0;
  CRight             = 1;
  CAbove             = 0;
  CBelow             = 1;
  CClip              = 0;
  CSubj              = 1;
  CInvertTriStrips   = FFALSE;

  // ===========================================================================
  // Private Data Types
  // ===========================================================================

type
  TVertexType = ( { Edge intersection classes }
    vtNUL,        { Empty non-intersection }
    vtEMX,        { External maximum }
    vtELI,        { External CLeft intermediate }
    vtTED,        { Top edge }
    vtERI,        { External CRight intermediate }
    vtRED,        { CRight edge }
    vtIMM,        { Internal maximum and minimum }
    vtIMN,        { Internal minimum }
    vtEMN,        { External minimum }
    vtEMM,        { External maximum and minimum }
    vtLED,        { CLeft edge }
    vtILI,        { Internal CLeft intermediate }
    vtBED,        { Bottom edge }
    vtIRI,        { Internal CRight intermediate }
    vtIMX,        { Internal maximum }
    vtFUL         { Full non-intersection }
    );

  THorizontalEdgeState = (
    hesNH, { No horizontal edge }
    hesBH, { Bottom horizontal edge }
    hesTH  { Top horizontal edge }
    );

  TBundleState = (UNBUNDLED, BUNDLE_HEAD, BUNDLE_TAIL);

  PPVertexNode = ^PVertexNode;
  PVertexNode  = ^TVertexNode; { Internal Vertex list datatype }

  TVertexNode = packed record
    X: Double;         { X coordinate component }
    Y: Double;         { Y coordinate component }
    Next: PVertexNode; { Pointer to next Vertex in list }
  end;

  PVertexNodeArray = ^TVertexNodeArray; { Helper type for indexing }
  TVertexNodeArray = array [0 .. 1] of PVertexNode;

  PPpolygonNode = ^PPolygonNode;
  PPolygonNode  = ^TPolygonNode;

  TPolygonNode = packed record
    Active: Integer;
    Hole: Integer;
    v: array [0 .. 1] of PVertexNode;
    Next: PPolygonNode;
    Proxy: PPolygonNode;
  end;

  PPEdgeNode = ^PEdgeNode;
  PEdgeNode  = ^TEdgeNode;

  TEdgeNode = packed record
    Vertex: TGpcVertex;                        { Piggy-backed Contour Vertex data }
    Bot: TGpcVertex;                           { Edge lower (x, y) coordinate }
    Top: TGpcVertex;                           { Edge upper (x, y) coordinate }
    XB: Double;                                { Scanbeam bottom x coordinate }
    xt: Double;                                { Scanbeam top x coordinate }
    dx: Double;                                { Change in x for a unit y increase }
    Typ: Integer;                              { CClip / subject edge flag }
    Bundle: array [0 .. 1, 0 .. 1] of Integer; { Bundle edge flags }
    Bside: array [0 .. 1] of Integer;          { Bundle CLeft / CRight indicators }
    Bstate: array [0 .. 1] of TBundleState;    { Edge bundle state }
    Outp: array [0 .. 1] of PPolygonNode;      { Output Polygon / TriStrip pointer }
    Prev: PEdgeNode;                           { Previous edge in the AET }
    Next: PEdgeNode;                           { Next edge in the AET }
    pred: PEdgeNode;                           { Edge connected at the lower end }
    Succ: PEdgeNode;                           { Edge connected at the upper end }
    Next_bound: PEdgeNode;                     { Pointer to next bound in LocalMinimaTable }
  end;

  PPEdgeNodeArray = ^PEdgeNodeArray;
  PEdgeNodeArray  = ^TEdgeNodeArray;
  TEdgeNodeArray  = array [0 .. MaxInt div SizeOf(TEdgeNode) - 1] of TEdgeNode;

  PPLocalMinimaTableNode = ^PLocalMinimaTableNode;
  PLocalMinimaTableNode  = ^TLocalMinimaTableNode;

  TLocalMinimaTableNode = packed record { Local minima table }
    Y: Double;                          { Y coordinate at local minimum }
    FirstBound: PEdgeNode;              { Pointer to bound list }
    Next: PLocalMinimaTableNode;        { Pointer to next local minimum }
  end;

  PPScanBeamTree = ^PScanBeamTree;
  PScanBeamTree  = ^TScanBeamTree;

  TScanBeamTree = packed record { Scanbeam tree }
    Y: Double;                  { Scanbeam node y value }
    Less: PScanBeamTree;        { Pointer to nodes with lower y }
    More: PScanBeamTree;        { Pointer to nodes with higher y }
  end;

  PPIntersectionNode = ^PIntersectionNode;
  PIntersectionNode  = ^TIntersectionNode; { Intersection table }

  TIntersectionNode = packed record
    IE: array [0 .. 1] of PEdgeNode; { Intersecting edge (bundle) pair }
    Point: TGpcVertex;               { Point of intersection }
    Next: PIntersectionNode;         { The next intersection table node }
  end;

  PPSortedEdgeTableNode = ^PSortedEdgeTableNode;
  PSortedEdgeTableNode  = ^TSortedEdgeTableNode; { Sorted edge table }

  TSortedEdgeTableNode = packed record
    edge: PEdgeNode;            { Pointer to AET edge }
    XB: Double;                 { Scanbeam bottom x coordinate }
    xt: Double;                 { Scanbeam top x coordinate }
    dx: Double;                 { Change in x for a unit y increase }
    Prev: PSortedEdgeTableNode; { Previous edge in sorted list }
  end;

  PBoundingBox = ^TBoundingBox;

  TBoundingBox = packed record { Contour axis-aligned bounding box }
    XMin: Double;              { Minimum x coordinate }
    YMin: Double;              { Minimum y coordinate }
    XMax: Double;              { Maximum x coordinate }
    YMax: Double;              { Maximum y coordinate }
  end;

  PBoundingBoxArray = ^TBoundingBoxArray;
  TBoundingBoxArray = array [0 .. MaxInt div SizeOf(TBoundingBox) - 1] of TBoundingBox;

  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array [0 .. MaxInt div SizeOf(Double) - 1] of Double;



  // ===========================================================================
  // C Macros, defined as function for PASCAL
  // ===========================================================================

function EQ(A, b: Double): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  EQ := Abs(A - b) <= CGPCEpsilon
end;

function PREV_INDEX(i, n: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  PREV_INDEX := ((i - 1 + n) mod n);
end;

function NEXT_INDEX(i, n: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  NEXT_INDEX := ((i + 1) mod n);
end;

function OPTIMAL(v: PGpcVertexArray; i, n: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  OPTIMAL := (v[PREV_INDEX(i, n)].Y <> v[i].Y) or
    (v[NEXT_INDEX(i, n)].Y <> v[i].Y);
end;

function FWD_MIN(v: PEdgeNodeArray; i, n: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  FWD_MIN := (v[PREV_INDEX(i, n)].Vertex.Y >= v[i].Vertex.Y) and
    (v[NEXT_INDEX(i, n)].Vertex.Y > v[i].Vertex.Y);
end;

function NOT_FMAX(v: PEdgeNodeArray; i, n: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  NOT_FMAX := (v[NEXT_INDEX(i, n)].Vertex.Y > v[i].Vertex.Y);
end;

function REV_MIN(v: PEdgeNodeArray; i, n: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  REV_MIN := (v[PREV_INDEX(i, n)].Vertex.Y > v[i].Vertex.Y) and
    (v[NEXT_INDEX(i, n)].Vertex.Y >= v[i].Vertex.Y);
end;

function NOT_RMAX(v: PEdgeNodeArray; i, n: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  NOT_RMAX := (v[PREV_INDEX(i, n)].Vertex.Y > v[i].Vertex.Y);
end;

procedure MALLOC(var p: Pointer; b: Integer; s: string); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  GetMem(p, b);
  if (p = nil) and (b <> 0) then
      RaiseInfo('gpc malloc failure: %s', [s]);
end;

procedure AddVertex(var p: PVertexNode; X, Y: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if p = nil then
    begin
      MALLOC(Pointer(p), SizeOf(TVertexNode), 'tristrip vertex creation');
      p.X := X;
      p.Y := Y;
      p.Next := nil;
    end
  else
    { Head further down the list }
      AddVertex(p.Next, X, Y);
end;

procedure Vertex(var E: PEdgeNode; p, s: Integer; var X, Y: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  AddVertex(E.Outp[p].v[s], X, Y);
  Inc(E.Outp[p].Active);
end;

procedure P_EDGE(var d, E: PEdgeNode; p: Integer; var i, J: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  d := E;
  repeat
      d := d.Prev
  until d.Outp[p] <> nil;
  i := d.Bot.X + d.dx * (J - d.Bot.Y);
end;

procedure N_EDGE(var d, E: PEdgeNode; p: Integer; var i, J: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  d := E;
  repeat
      d := d.Next;
  until d.Outp[p] <> nil;
  i := d.Bot.X + d.dx * (J - d.Bot.Y);
end;

procedure Free(var p: Pointer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  FreeMem(p);
  p := nil;
end;

procedure CFree(var p: Pointer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if p <> nil then
      Free(p);
end;



// ===========================================================================
// Global Data
// ===========================================================================

{ Horizontal edge state transitions within scanbeam boundary }
const
  CNextHorizontalEdgeState: array [0 .. 2, 0 .. 5] of THorizontalEdgeState =
  { CAbove     CBelow     CROSS }
  { L   R     L   R     L   R }
  { hesNH } ((hesBH, hesTH, hesTH, hesBH, hesNH, hesNH),
    { hesBH } (hesNH, hesNH, hesNH, hesNH, hesTH, hesTH),
    { hesTH } (hesNH, hesNH, hesNH, hesNH, hesBH, hesBH));




  // ===========================================================================
  // Private Functions
  // ===========================================================================

procedure Reset_it(var It: PIntersectionNode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ITN: PIntersectionNode;
begin
  while (It <> nil) do
    begin
      ITN := It.Next;
      Free(Pointer(It));
      It := ITN;
    end;
end;

procedure ResetLocalMinimaTable(var LocalMinimaTable: PLocalMinimaTableNode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LocalMinimaTableNode: PLocalMinimaTableNode;
begin
  while LocalMinimaTable <> nil do
    begin
      LocalMinimaTableNode := LocalMinimaTable^.Next;
      Free(Pointer(LocalMinimaTable));
      LocalMinimaTable := LocalMinimaTableNode;
    end;
end;

procedure InsertBound(b: PPEdgeNodeArray; E: PEdgeNodeArray); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  Existing_bound: Pointer;
begin
  if b^ = nil then
    begin
      { Link node e to the tail of the list }
      b^ := E;
    end
  else
    begin
      { Do primary sort on the x field }
      if ((E[0].Bot.X < b^[0].Bot.X)) then
        begin
          { Insert a new node mid-list }
          Existing_bound := b^;
          b^ := E;
          b^[0].Next_bound := Existing_bound;
        end
      else
        begin
          if ((E[0].Bot.X = b^[0].Bot.X)) then
            begin
              { Do secondary sort on the dx field }
              if ((E[0].dx < b^[0].dx)) then
                begin
                  { Insert a new node mid-list }
                  Existing_bound := b^;
                  b^ := E;
                  b^[0].Next_bound := Existing_bound;
                end
              else
                begin
                  { Head further down the list }
                  InsertBound(@(b^[0].Next_bound), E);
                end;
            end
          else
            begin
              { Head further down the list }
              InsertBound(@(b^[0].Next_bound), E);
            end;
        end;
    end;
end;

function BoundList(var LocalMinimaTable: PLocalMinimaTableNode; Y: Double): PPEdgeNodeArray; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  Existing_node: PLocalMinimaTableNode;
begin
  if LocalMinimaTable = nil then
    begin
      { Add node onto the tail end of the LocalMinimaTable }
      MALLOC(Pointer(LocalMinimaTable), SizeOf(TLocalMinimaTableNode), 'LMT insertion');
      LocalMinimaTable.Y := Y;
      LocalMinimaTable.FirstBound := nil;
      LocalMinimaTable.Next := nil;
      Result := @LocalMinimaTable.FirstBound;
    end
  else if (Y < LocalMinimaTable.Y) then
    begin
      { Insert a new LocalMinimaTable node before the current node }
      Existing_node := LocalMinimaTable;
      MALLOC(Pointer(LocalMinimaTable), SizeOf(TLocalMinimaTableNode), 'LMT insertion');
      LocalMinimaTable.Y := Y;
      LocalMinimaTable.FirstBound := nil;
      LocalMinimaTable.Next := Existing_node;
      Result := @LocalMinimaTable.FirstBound;
    end
  else if (Y > LocalMinimaTable.Y) then
    { Head further up the LocalMinimaTable }
      Result := BoundList(LocalMinimaTable.Next, Y)
  else
    { Use this existing LocalMinimaTable node }
      Result := @LocalMinimaTable.FirstBound;
end;

procedure AddToScanBeamTree(var Entries: Integer; var SbTree: PScanBeamTree; const Y: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if SbTree = nil then
    begin
      { Add a new tree node here }
      MALLOC(Pointer(SbTree), SizeOf(TScanBeamTree), 'scanbeam tree insertion');
      SbTree.Y := Y;
      SbTree.Less := nil;
      SbTree.More := nil;
      Inc(Entries);
    end
  else
    begin
      if (SbTree.Y > Y) then
        begin
          { Head into the 'less' sub-tree }
          AddToScanBeamTree(Entries, SbTree.Less, Y);
        end
      else
        begin
          if (SbTree.Y < Y) then
            begin
              { Head into the 'more' sub-tree }
              AddToScanBeamTree(Entries, SbTree.More, Y);
            end;
        end;
    end;
end;

procedure BuildScanBeamTree(var Entries: Integer; var Sbt: PDoubleArray; SbTree: PScanBeamTree); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if SbTree.Less <> nil then
      BuildScanBeamTree(Entries, Sbt, SbTree.Less);
  Sbt[Entries] := SbTree.Y;
  Inc(Entries);
  if SbTree.More <> nil then
      BuildScanBeamTree(Entries, Sbt, SbTree.More);
end;

procedure FreeScanBeamTree(var SbTree: PScanBeamTree); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if SbTree <> nil then
    begin
      FreeScanBeamTree(SbTree.Less);
      FreeScanBeamTree(SbTree.More);
      Free(Pointer(SbTree));
    end;
end;

function CountOptimalVertices(C: TGpcVertexList): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i: Integer;
begin
  Result := 0;

  { Ignore non-contributing contours }
  if C.NumVertices > 0 then
    begin
      for i := 0 to C.NumVertices - 1 do
        { Ignore superfluous vertices embedded in horizontal edges }
        if OPTIMAL(C.Vertex, i, C.NumVertices) then
            Inc(Result);
    end;
end;

function BuildLocalMinimaTable(var LocalMinimaTable: PLocalMinimaTableNode; var SbTree: PScanBeamTree; var Sbt_entries: Integer;
  const p: PGpcPolygon; const Typ: Integer; const Op: TGpcOp): PEdgeNodeArray; {$IFDEF INLINE_ASM} inline; {$ENDIF}

var
  C, i, Min, Max, NumEdges, v, NumVertices: Integer;
  TotalVertices, E_index: Integer;
  E, EdgeTable: PEdgeNodeArray;
begin
  TotalVertices := 0;
  E_index := 0;

  for C := 0 to p.NumContours - 1 do
      Inc(TotalVertices, CountOptimalVertices(p.Contour[C]));

  { Create the entire input Polygon edge table in one go }
  MALLOC(Pointer(EdgeTable), TotalVertices * SizeOf(TEdgeNode),
    'edge table creation');

  for C := 0 to p.NumContours - 1 do
    begin
      if p.Contour[C].NumVertices < 0 then
        begin
          { Ignore the non-contributing Contour and repair the Vertex count }
          p.Contour[C].NumVertices := -p.Contour[C].NumVertices;
        end
      else
        begin
          { Perform Contour optimisation }
          NumVertices := 0;
          for i := 0 to p.Contour[C].NumVertices - 1 do
            if (OPTIMAL(p.Contour[C].Vertex, i, p.Contour[C].NumVertices)) then
              begin
                EdgeTable[NumVertices].Vertex.X := p.Contour[C].Vertex[i].X;
                EdgeTable[NumVertices].Vertex.Y := p.Contour[C].Vertex[i].Y;

                { record Vertex in the scanbeam table }
                AddToScanBeamTree(Sbt_entries, SbTree, EdgeTable[NumVertices].Vertex.Y);

                Inc(NumVertices);
              end;

          { Do the Contour forward pass }
          for Min := 0 to NumVertices - 1 do
            begin
              { If a forward local minimum... }
              if FWD_MIN(EdgeTable, Min, NumVertices) then
                begin
                  { Search for the next local maximum... }
                  NumEdges := 1;
                  Max := NEXT_INDEX(Min, NumVertices);
                  while (NOT_FMAX(EdgeTable, Max, NumVertices)) do
                    begin
                      Inc(NumEdges);
                      Max := NEXT_INDEX(Max, NumVertices);
                    end;

                  { Build the next edge list }
                  E := @EdgeTable[E_index];
                  Inc(E_index, NumEdges);
                  v := Min;
                  E[0].Bstate[CBelow] := UNBUNDLED;
                  E[0].Bundle[CBelow][CClip] := FFALSE;
                  E[0].Bundle[CBelow][CSubj] := FFALSE;
                  for i := 0 to NumEdges - 1 do
                    begin
                      E[i].XB := EdgeTable[v].Vertex.X;
                      E[i].Bot.X := EdgeTable[v].Vertex.X;
                      E[i].Bot.Y := EdgeTable[v].Vertex.Y;

                      v := NEXT_INDEX(v, NumVertices);

                      E[i].Top.X := EdgeTable[v].Vertex.X;
                      E[i].Top.Y := EdgeTable[v].Vertex.Y;
                      E[i].dx := (EdgeTable[v].Vertex.X - E[i].Bot.X) /
                        (E[i].Top.Y - E[i].Bot.Y);
                      E[i].Typ := Typ;
                      E[i].Outp[CAbove] := nil;
                      E[i].Outp[CBelow] := nil;
                      E[i].Next := nil;
                      E[i].Prev := nil;
                      if (NumEdges > 1) and (i < (NumEdges - 1)) then
                          E[i].Succ := @E[i + 1]
                      else
                          E[i].Succ := nil;
                      if (NumEdges > 1) and (i > 0) then
                          E[i].pred := @E[i - 1]
                      else
                          E[i].pred := nil;
                      E[i].Next_bound := nil;
                      if Op = goDiff then
                          E[i].Bside[CClip] := CRight
                      else
                          E[i].Bside[CClip] := CLeft;
                      E[i].Bside[CSubj] := CLeft;
                    end;
                  InsertBound(BoundList(LocalMinimaTable, EdgeTable[Min].Vertex.Y), E);
                end;
            end;

          { Do the Contour reverse pass }
          for Min := 0 to NumVertices - 1 do
            begin
              { If a reverse local minimum... }
              if REV_MIN(EdgeTable, Min, NumVertices) then
                begin
                  { Search for the previous local maximum... }
                  NumEdges := 1;
                  Max := PREV_INDEX(Min, NumVertices);
                  while NOT_RMAX(EdgeTable, Max, NumVertices) do
                    begin
                      Inc(NumEdges);
                      Max := PREV_INDEX(Max, NumVertices);
                    end;

                  { Build the previous edge list }
                  E := @EdgeTable[E_index];
                  Inc(E_index, NumEdges);
                  v := Min;
                  E[0].Bstate[CBelow] := UNBUNDLED;
                  E[0].Bundle[CBelow][CClip] := FFALSE;
                  E[0].Bundle[CBelow][CSubj] := FFALSE;
                  for i := 0 to NumEdges - 1 do
                    begin
                      E[i].XB := EdgeTable[v].Vertex.X;
                      E[i].Bot.X := EdgeTable[v].Vertex.X;
                      E[i].Bot.Y := EdgeTable[v].Vertex.Y;

                      v := PREV_INDEX(v, NumVertices);

                      E[i].Top.X := EdgeTable[v].Vertex.X;
                      E[i].Top.Y := EdgeTable[v].Vertex.Y;
                      E[i].dx := (EdgeTable[v].Vertex.X - E[i].Bot.X) /
                        (E[i].Top.Y - E[i].Bot.Y);
                      E[i].Typ := Typ;
                      E[i].Outp[CAbove] := nil;
                      E[i].Outp[CBelow] := nil;
                      E[i].Next := nil;
                      E[i].Prev := nil;
                      if (NumEdges > 1) and (i < (NumEdges - 1)) then
                          E[i].Succ := @E[i + 1]
                      else
                          E[i].Succ := nil;
                      if (NumEdges > 1) and (i > 0) then
                          E[i].pred := @E[i - 1]
                      else
                          E[i].pred := nil;
                      E[i].Next_bound := nil;
                      if Op = goDiff then
                          E[i].Bside[CClip] := CRight
                      else
                          E[i].Bside[CClip] := CLeft;
                      E[i].Bside[CSubj] := CLeft;
                    end;
                  InsertBound(BoundList(LocalMinimaTable, EdgeTable[Min].Vertex.Y), E);
                end;
            end;
        end;
    end;
  Result := EdgeTable;
end;

procedure AddEdgeToAET(var Aet: PEdgeNode; const edge: PEdgeNode; const Prev: PEdgeNode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if Aet = nil then
    begin
      { Append edge onto the tail end of the AET }
      Aet := edge;
      edge.Prev := Prev;
      edge.Next := nil;
    end
  else
    begin
      { Do primary sort on the xb field }
      if (edge.XB < Aet.XB) then
        begin
          { Insert edge here (before the AET edge) }
          edge.Prev := Prev;
          edge.Next := Aet;
          Aet.Prev := edge;
          Aet := edge;
        end
      else
        begin
          if (edge.XB = Aet.XB) then
            begin
              { Do secondary sort on the dx field }
              if (edge.dx < Aet.dx) then
                begin
                  { Insert edge here (before the AET edge) }
                  edge.Prev := Prev;
                  edge.Next := Aet;
                  Aet.Prev := edge;
                  Aet := edge;
                end
              else
                begin
                  { Head further into the AET }
                  AddEdgeToAET(Aet.Next, edge, Aet);
                end;
            end
          else
            begin
              { Head further into the AET }
              AddEdgeToAET(Aet.Next, edge, Aet);
            end;
        end;
    end;
end;

procedure AddIntersection(var It: PIntersectionNode; const Edge0, Edge1: PEdgeNode; const X, Y: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  Existing_node: PIntersectionNode;
begin

  if It = nil then
    begin
      { Append a new node to the tail of the list }
      MALLOC(Pointer(It), SizeOf(TIntersectionNode), 'IT insertion');
      It.IE[0] := Edge0;
      It.IE[1] := Edge1;
      It.Point.X := X;
      It.Point.Y := Y;
      It.Next := nil;
    end
  else
    begin
      if (It.Point.Y > Y) then
        begin
          { Insert a new node mid-list }
          Existing_node := It;
          MALLOC(Pointer(It), SizeOf(TIntersectionNode), 'IT insertion');
          It.IE[0] := Edge0;
          It.IE[1] := Edge1;
          It.Point.X := X;
          It.Point.Y := Y;
          It.Next := Existing_node;
        end
      else
        { Head further down the list }
          AddIntersection(It.Next, Edge0, Edge1, X, Y);
    end;
end;

procedure AddSortedTableEdge(var st: PSortedEdgeTableNode; var It: PIntersectionNode; const edge: PEdgeNode; const dy: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  Existing_node: PSortedEdgeTableNode;
  Den, X, Y, R: Double;
begin
  if st = nil then
    begin
      { Append edge onto the tail end of the ST }
      MALLOC(Pointer(st), SizeOf(TSortedEdgeTableNode), 'ST insertion');
      st.edge := edge;
      st.XB := edge.XB;
      st.xt := edge.xt;
      st.dx := edge.dx;
      st.Prev := nil;
    end
  else
    begin
      Den := (st.xt - st.XB) - (edge.xt - edge.XB);

      { If new edge and ST edge don't cross }
      if ((edge.xt >= st.xt) or (edge.dx = st.dx) or (Abs(Den) <= CGPCEpsilon))
      then
        begin
          { No intersection - insert edge here (before the ST edge) }
          Existing_node := st;
          MALLOC(Pointer(st), SizeOf(TSortedEdgeTableNode), 'ST insertion');
          st.edge := edge;
          st.XB := edge.XB;
          st.xt := edge.xt;
          st.dx := edge.dx;
          st.Prev := Existing_node;
        end
      else
        begin
          { Compute intersection between new edge and ST edge }
          R := (edge.XB - st.XB) / Den;
          X := st.XB + R * (st.xt - st.XB);
          Y := R * dy;

          { Insert the edge pointers and the intersection point in the IT }
          AddIntersection(It, st.edge, edge, X, Y);

          { Head further into the ST }
          AddSortedTableEdge(st.Prev, It, edge, dy);

        end;
    end;
end;

procedure BuildIntersectionTable(var It: PIntersectionNode; const Aet: PEdgeNode; const dy: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  st, Stp: PSortedEdgeTableNode;
  edge: PEdgeNode;
begin

  { Build intersection table for the current scanbeam }
  Reset_it(It);
  st := nil;

  { Process each AET edge }
  edge := Aet;
  while edge <> nil do
    begin
      if (edge.Bstate[CAbove] = BUNDLE_HEAD) or (edge.Bundle[CAbove][CClip] <> 0) or
        (edge.Bundle[CAbove][CSubj] <> 0) then
          AddSortedTableEdge(st, It, edge, dy);
      edge := edge.Next;
    end;

  { Free the sorted edge table }
  while st <> nil do
    begin
      Stp := st.Prev;
      Free(Pointer(st));
      st := Stp;
    end;
end;

function CountContours(Polygon: PPolygonNode): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  NV: Integer;
  v, Nextv: PVertexNode;
begin

  Result := 0;
  while Polygon <> nil do
    begin
      if Polygon.Active <> 0 then
        begin
          { Count the vertices in the current Contour }
          NV := 0;
          v := Polygon.Proxy.v[CLeft];
          while v <> nil do
            begin
              Inc(NV);
              v := v.Next;
            end;

          { record valid Vertex counts in the Active field }
          if (NV > 2) then
            begin
              Polygon.Active := NV;
              Inc(Result);
            end
          else
            begin
              { Invalid Contour: just Free the heap }
              v := Polygon.Proxy.v[CLeft];
              while v <> nil do
                begin
                  Nextv := v.Next;
                  Free(Pointer(v));
                  v := Nextv;
                end;
              Polygon.Active := 0;
            end;
        end;

      Polygon := Polygon.Next;
    end;
end;

procedure AddLeft(const p: PPolygonNode; const X, Y: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  NV: PVertexNode;
begin
  { Create a new Vertex node and set its fields }
  MALLOC(Pointer(NV), SizeOf(TVertexNode), 'vertex node creation');
  NV.X := X;
  NV.Y := Y;

  { Add Vertex nv to the CLeft end of the Polygon's vertex list }
  NV.Next := p.Proxy.v[CLeft];

  { Update Proxy[CLeft] to point to nv }
  p.Proxy.v[CLeft] := NV;
end;

procedure MergeLeft(const p, q: PPolygonNode; List: PPolygonNode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  Target: PPolygonNode;
begin
  { Label Contour as a Hole }
  q.Proxy.Hole := FTRUE;

  if p.Proxy <> q.Proxy then
    begin
      { Assign P's vertex list to the left end of Q's list }
      p.Proxy.v[CRight].Next := q.Proxy.v[CLeft];
      q.Proxy.v[CLeft] := p.Proxy.v[CLeft];

      { Redirect any P->Proxy references to Q->Proxy }
      Target := p.Proxy;
      while List <> nil do
        begin
          if List.Proxy = Target then
            begin
              List.Active := FFALSE;
              List.Proxy := q.Proxy;
            end;
          List := List.Next;
        end;
    end;
end;

procedure AddRight(const p: PPolygonNode; const X, Y: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  NV: PVertexNode;
begin
  { Create a new Vertex node and set its fields }
  MALLOC(Pointer(NV), SizeOf(TVertexNode), 'vertex node creation');
  NV.X := X;
  NV.Y := Y;
  NV.Next := nil;

  { Add Vertex nv to the CRight end of the Polygon's vertex list }
  p.Proxy.v[CRight].Next := NV;

  { Update Proxy.v[CRight] to point to nv }
  p.Proxy.v[CRight] := NV;
end;

procedure MergeRight(const p, q: PPolygonNode; List: PPolygonNode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  Target: PPolygonNode;
begin
  { Label Contour as external }
  q.Proxy.Hole := FFALSE;

  if p.Proxy <> q.Proxy then
    begin
      { Assign P's vertex list to the right end of Q's list }
      q.Proxy.v[CRight].Next := p.Proxy.v[CLeft];
      q.Proxy.v[CRight] := p.Proxy.v[CRight];

      { Redirect any P->Proxy references to Q->Proxy }
      Target := p.Proxy;
      while List <> nil do
        begin
          if List.Proxy = Target then
            begin
              List.Active := FFALSE;
              List.Proxy := q.Proxy;
            end;
          List := List.Next;
        end;
    end;
end;

procedure AddLocalMin(const p: PPpolygonNode; const edge: PEdgeNode; const X, Y: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  NV: PVertexNode;
  Existing_min: PPolygonNode;
begin
  Existing_min := p^;

  MALLOC(Pointer(p^), SizeOf(TPolygonNode), 'polygon node creation');

  { Create a new Vertex node and set its fields }
  MALLOC(Pointer(NV), SizeOf(TVertexNode), 'vertex node creation');
  NV.X := X;
  NV.Y := Y;
  NV.Next := nil;

  { Initialise Proxy to point to p itself }
  p^.Proxy := p^;
  p^.Active := FTRUE;
  p^.Next := Existing_min;

  { Make v[CLeft] and v[CRight] point to new Vertex nv }
  p^.v[CLeft] := NV;
  p^.v[CRight] := NV;

  { Assign Polygon p to the edge }
  edge.Outp[CAbove] := p^;
end;

function CountTristrips(tN: PPolygonNode): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  Result := 0;

  while tN <> nil do
    begin
      if tN.Active > 2 then
          Inc(Result);
      tN := tN.Next;
    end;
end;

procedure NewTristrip(var tN: PPolygonNode; const edge: PEdgeNode; const X, Y: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  if tN = nil then
    begin
      MALLOC(Pointer(tN), SizeOf(TPolygonNode), 'tristrip node creation');
      tN.Next := nil;
      tN.v[CLeft] := nil;
      tN.v[CRight] := nil;
      tN.Active := 1;
      AddVertex(tN.v[CLeft], X, Y);
      edge.Outp[CAbove] := tN;
    end
  else
    { Head further down the list }
      NewTristrip(tN.Next, edge, X, Y);
end;

function CreateContourBoundingBoxes(const p: PGpcPolygon): PBoundingBoxArray; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  C, v: Integer;
begin
  MALLOC(Pointer(Result), p.NumContours * SizeOf(TBoundingBox), 'Bounding box creation');

  { Construct Contour bounding boxes }
  for C := 0 to p.NumContours - 1 do
    begin
      { Initialise bounding box extent }
      Result[C].XMin := CDoubleMax;
      Result[C].YMin := CDoubleMax;
      Result[C].XMax := -CDoubleMax;
      Result[C].YMax := -CDoubleMax;

      for v := 0 to p.Contour[C].NumVertices - 1 do
        begin
          { Adjust bounding Result }
          if (p.Contour[C].Vertex[v].X < Result[C].XMin) then
              Result[C].XMin := p.Contour[C].Vertex[v].X;
          if (p.Contour[C].Vertex[v].Y < Result[C].YMin) then
              Result[C].YMin := p.Contour[C].Vertex[v].Y;
          if (p.Contour[C].Vertex[v].X > Result[C].XMax) then
              Result[C].XMax := p.Contour[C].Vertex[v].X;
          if (p.Contour[C].Vertex[v].Y > Result[C].YMax) then
              Result[C].YMax := p.Contour[C].Vertex[v].Y;
        end;
    end;
end;

procedure MinimaxTest(CSubj: PGpcPolygon; CClip: PGpcPolygon; Op: TGpcOp);
var
  S_bbox, C_bbox: PBoundingBoxArray;
  s, C: Integer;
  OverlapTable: PIntegerArray;
  Overlap: Integer;
begin
  S_bbox := CreateContourBoundingBoxes(CSubj);
  C_bbox := CreateContourBoundingBoxes(CClip);

  MALLOC(Pointer(OverlapTable), CSubj.NumContours * CClip.NumContours *
    SizeOf(Integer), 'overlap table creation');

  { Check all subject Contour bounding boxes against CClip boxes }
  for s := 0 to CSubj.NumContours - 1 do
    for C := 0 to CClip.NumContours - 1 do
        OverlapTable[C * CSubj.NumContours + s] :=
        Integer((not((S_bbox[s].XMax < C_bbox[C].XMin) or
        (S_bbox[s].XMin > C_bbox[C].XMax))) and
        (not((S_bbox[s].YMax < C_bbox[C].YMin) or
        (S_bbox[s].YMin > C_bbox[C].YMax))));

  { For each CClip Contour, search for any subject Contour overlaps }
  for C := 0 to CClip.NumContours - 1 do
    begin
      Overlap := 0;
      s := 0;
      while (Overlap = 0) and (s < CSubj.NumContours) do
        begin
          Overlap := OverlapTable[C * CSubj.NumContours + s];
          Inc(s);
        end;

      if Overlap = 0 then
        { Flag non contributing status by negating Vertex count }
          CClip.Contour[C].NumVertices := -CClip.Contour[C].NumVertices;
    end;

  if (Op = goInt) then
    begin
      { For each subject Contour, search for any CClip Contour overlaps }
      for s := 0 to CSubj.NumContours - 1 do
        begin
          Overlap := 0;
          C := 0;
          while (Overlap = 0) and (C < CClip.NumContours) do
            begin
              Overlap := OverlapTable[C * CSubj.NumContours + s];
              Inc(C);
            end;

          if Overlap = 0 then
            { Flag non contributing status by negating Vertex count }
              CSubj.Contour[s].NumVertices := -CSubj.Contour[s].NumVertices;
        end;
    end;

  Free(Pointer(S_bbox));
  Free(Pointer(C_bbox));
  Free(Pointer(OverlapTable));
end;


// ===========================================================================
// Public Functions
// ===========================================================================

procedure GpcFreePolygon(Polygon: PGpcPolygon);
var
  C: Integer;
begin
  for C := 0 to Polygon.NumContours - 1 do
      CFree(Pointer(Polygon.Contour[C].Vertex));

  CFree(Pointer(Polygon.Hole));
  CFree(Pointer(Polygon.Contour));
  Polygon.NumContours := 0;
end;

procedure GpcAddContour(Polygon: PGpcPolygon; Contour: PGpcVertexList;
  Hole: Integer);
var
  C, v: Integer;
  Extended_hole: PIntegerArray;
  Extended_contour: PGpcVertexListArray;
begin

  { Create an extended Hole array }
  MALLOC(Pointer(Extended_hole), (Polygon.NumContours + 1) * SizeOf(Integer),
    'contour hole addition');

  { Create an extended Contour array }
  MALLOC(Pointer(Extended_contour), (Polygon.NumContours + 1) *
    SizeOf(TGpcVertexList), 'contour addition');

  { Copy the old Contour into the extended Contour array }
  for C := 0 to Polygon.NumContours - 1 do
    begin
      Extended_hole[C] := Polygon.Hole[C];
      Extended_contour[C] := Polygon.Contour[C];
    end;

  { Copy the new Contour onto the end of the extended Contour array }
  C := Polygon.NumContours;
  Extended_hole[C] := Hole;
  Extended_contour[C].NumVertices := Contour.NumVertices;
  MALLOC(Pointer(Extended_contour[C].Vertex), Contour.NumVertices *
    SizeOf(TGpcVertex), 'contour addition');
  for v := 0 to Contour.NumVertices - 1 do
      Extended_contour[C].Vertex[v] := Contour.Vertex[v];

  { Dispose of the old Contour }
  CFree(Pointer(Polygon.Contour));
  CFree(Pointer(Polygon.Hole));

  { Update the Polygon information }
  Inc(Polygon.NumContours);
  Polygon.Hole := Extended_hole;
  Polygon.Contour := Extended_contour;
end;

procedure GpcPolygonClip(SetOperation: TGpcOp; SubjectPolygon: PGpcPolygon;
  ClipPolygon: PGpcPolygon; ResultPolygon: PGpcPolygon);
var
  SbTree: PScanBeamTree;
  It, Intersect: PIntersectionNode;
  edge, PrevEdge, NextEdge, SuccEdge: PEdgeNode;
  E0, e1: PEdgeNode;
  Aet: PEdgeNode;
  C_heap, S_heap: PEdgeNodeArray;
  LocalMinimaTable, LocalMin: PLocalMinimaTableNode;
  OutPoly, p, q, Poly, Npoly, Cf: PPolygonNode;
  Vtx, NV: PVertexNode;
  Horiz: array [0 .. 1] of THorizontalEdgeState;
  Inn, Exists, Parity: array [0 .. 1] of Integer;
  C, v, Contributing, Search, Scanbeam: Integer;
  SbtEntries, _class, BL, BR, TL, tr: Integer;
  Sbt: PDoubleArray;
  XB, Px, Yb, Yt, dy, ix, iy: Double;
begin
  edge := nil;
  SbTree := nil;
  It := nil;
  Aet := nil;
  LocalMinimaTable := nil;
  OutPoly := nil;
  Cf := nil;
  Inn[0] := CLeft;
  Inn[1] := CLeft;
  Exists[0] := CLeft;
  Exists[1] := CLeft;
  Parity[0] := CLeft;
  Parity[1] := CLeft;
  Scanbeam := 0;
  SbtEntries := 0;
  Sbt := nil;
  C_heap := nil;
  S_heap := nil;

  { Test for trivial NULL Result cases }
  if ((SubjectPolygon.NumContours = 0) and (ClipPolygon.NumContours = 0)) or
    ((SubjectPolygon.NumContours = 0) and ((SetOperation = goInt) or
    (SetOperation = goDiff))) or ((ClipPolygon.NumContours = 0) and
    (SetOperation = goInt)) then
    begin
      ResultPolygon.NumContours := 0;
      ResultPolygon.Hole := nil;
      ResultPolygon.Contour := nil;
      Exit;
    end;

  { Identify potentialy contributing contours }
  if (((SetOperation = goInt) or (SetOperation = goDiff)) and
    (SubjectPolygon.NumContours > 0) and (ClipPolygon.NumContours > 0)) then
      MinimaxTest(SubjectPolygon, ClipPolygon, SetOperation);

  { Build LocalMinimaTable }
  if SubjectPolygon.NumContours > 0 then
      S_heap := BuildLocalMinimaTable(LocalMinimaTable, SbTree, SbtEntries, SubjectPolygon, CSubj,
      SetOperation);
  if ClipPolygon.NumContours > 0 then
      C_heap := BuildLocalMinimaTable(LocalMinimaTable, SbTree, SbtEntries, ClipPolygon, CClip,
      SetOperation);

  { Return a NULL Result if no contours contribute }
  if LocalMinimaTable = nil then
    begin
      ResultPolygon.NumContours := 0;
      ResultPolygon.Hole := nil;
      ResultPolygon.Contour := nil;
      ResetLocalMinimaTable(LocalMinimaTable);
      Free(Pointer(S_heap));
      Free(Pointer(C_heap));
      Exit;
    end;

  { Build scanbeam table from scanbeam tree }
  MALLOC(Pointer(Sbt), SbtEntries * SizeOf(Double), 'sbt creation');
  BuildScanBeamTree(Scanbeam, Sbt, SbTree);
  Scanbeam := 0;
  FreeScanBeamTree(SbTree);

  { Allow pointer re-use without causing memory leak }
  if SubjectPolygon = ResultPolygon then
      GpcFreePolygon(SubjectPolygon);
  if ClipPolygon = ResultPolygon then
      GpcFreePolygon(ClipPolygon);

  { Invert CClip Polygon for difference operation }
  if SetOperation = goDiff then
      Parity[CClip] := CRight;

  LocalMin := LocalMinimaTable;

  { Process each scanbeam }
  while (Scanbeam < SbtEntries) do
    begin
      { Set yb and yt to the bottom and top of the scanbeam }
      Yb := Sbt[Scanbeam];
      Inc(Scanbeam);
      if Scanbeam < SbtEntries then
        begin
          Yt := Sbt[Scanbeam];
          dy := Yt - Yb;
        end;

      { === SCANBEAM BOUNDARY PROCESSING ================================ }

      { If LocalMinimaTable node corresponding to yb exists }
      if LocalMin <> nil then
        begin
          if (LocalMin.Y = Yb) then
            begin
              { Add edges starting at this local minimum to the AET }
              edge := LocalMin.FirstBound;
              while edge <> nil do
                begin
                  AddEdgeToAET(Aet, edge, nil);
                  edge := edge.Next_bound;
                end;
              LocalMin := LocalMin.Next;
            end;
        end;

      { Set dummy previous x value }
      Px := -CDoubleMax;

      { Create bundles within AET }
      E0 := Aet;
      e1 := Aet;

      { Set up bundle fields of first edge }
      Aet.Bundle[CAbove][Integer(Aet.Typ <> 0)] := Integer((Aet.Top.Y <> Yb));
      Aet.Bundle[CAbove][Integer(Aet.Typ = 0)] := FFALSE;
      Aet.Bstate[CAbove] := UNBUNDLED;

      NextEdge := Aet.Next;

      while NextEdge <> nil do
        begin
          { Set up bundle fields of next edge }
          NextEdge.Bundle[CAbove][NextEdge.Typ] :=
            Integer((NextEdge.Top.Y <> Yb));
          NextEdge.Bundle[CAbove][Integer(NextEdge.Typ = 0)] := FFALSE;
          NextEdge.Bstate[CAbove] := UNBUNDLED;

          { Bundle edges CAbove the scanbeam boundary if they coincide }
          if NextEdge.Bundle[CAbove][NextEdge.Typ] <> 0 then
            begin
              if (EQ(E0.XB, NextEdge.XB) and EQ(E0.dx, NextEdge.dx) and
                (E0.Top.Y <> Yb)) then
                begin
                  NextEdge.Bundle[CAbove][NextEdge.Typ] := NextEdge.Bundle[CAbove]
                    [NextEdge.Typ] xor E0.Bundle[CAbove][NextEdge.Typ];
                  NextEdge.Bundle[CAbove][Integer(NextEdge.Typ = 0)] :=
                    E0.Bundle[CAbove][Integer(NextEdge.Typ = 0)];
                  NextEdge.Bstate[CAbove] := BUNDLE_HEAD;
                  E0.Bundle[CAbove][CClip] := FFALSE;
                  E0.Bundle[CAbove][CSubj] := FFALSE;
                  E0.Bstate[CAbove] := BUNDLE_TAIL;
                end;
              E0 := NextEdge;
            end;
          NextEdge := NextEdge.Next;
        end;

      Horiz[CClip] := hesNH;
      Horiz[CSubj] := hesNH;

      { Process each edge at this scanbeam boundary }
      edge := Aet;
      while edge <> nil do
        begin
          Exists[CClip] := edge.Bundle[CAbove][CClip] +
            (edge.Bundle[CBelow][CClip] shl 1);
          Exists[CSubj] := edge.Bundle[CAbove][CSubj] +
            (edge.Bundle[CBelow][CSubj] shl 1);

          if (Exists[CClip] <> 0) or (Exists[CSubj] <> 0) then
            begin
              { Set bundle side }
              edge.Bside[CClip] := Parity[CClip];
              edge.Bside[CSubj] := Parity[CSubj];

              { Determine contributing status and quadrant occupancies }
              case SetOperation of
                goDiff, goInt:
                  begin
                    Contributing :=
                      Integer(((Exists[CClip] <> 0) and ((Parity[CSubj] <> 0) or
                      (Horiz[CSubj] <> hesNH))) or ((Exists[CSubj] <> 0) and
                      ((Parity[CClip] <> 0) or (Horiz[CClip] <> hesNH))) or
                      ((Exists[CClip] <> 0) and (Exists[CSubj] <> 0) and
                      (Parity[CClip] = Parity[CSubj])));
                    BR := Integer((Parity[CClip] <> 0) and (Parity[CSubj] <> 0));
                    BL := Integer(((Parity[CClip] xor edge.Bundle[CAbove][CClip]) <> 0)
                      and ((Parity[CSubj] xor edge.Bundle[CAbove][CSubj]) <> 0));
                    tr := Integer(((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH)) <> 0)
                      and ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH)) <> 0));
                    TL := Integer
                      (((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH) xor edge.Bundle
                      [CBelow][CClip]) <> 0) and
                      ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH) xor edge.Bundle
                      [CBelow][CSubj]) <> 0));
                  end;

                goXor:
                  begin
                    Contributing := Integer((Exists[CClip] <> 0) or
                      (Exists[CSubj] <> 0));
                    BR := Integer(Parity[CClip] xor Parity[CSubj]);
                    BL := Integer(((Parity[CClip] xor edge.Bundle[CAbove][CClip]) <> 0)
                      xor ((Parity[CSubj] xor edge.Bundle[CAbove][CSubj]) <> 0));
                    tr := Integer(((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH)) <> 0)
                      xor ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH)) <> 0));
                    TL := Integer
                      (((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH) xor edge.Bundle
                      [CBelow][CClip]) <> 0)
                      xor ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH)
                      xor edge.Bundle[CBelow][CSubj]) <> 0));
                  end;

                goUnion:
                  begin
                    Contributing :=
                      Integer(((Exists[CClip] <> 0) and ((Parity[CSubj] = 0) or
                      (Horiz[CSubj] <> hesNH))) or ((Exists[CSubj] <> 0) and
                      ((Parity[CClip] = 0) or (Horiz[CClip] <> hesNH))) or
                      ((Exists[CClip] <> 0) and (Exists[CSubj] <> 0) and
                      (Parity[CClip] = Parity[CSubj])));

                    BR := Integer((Parity[CClip] <> 0) or (Parity[CSubj] <> 0));
                    BL := Integer(((Parity[CClip] xor edge.Bundle[CAbove][CClip]) <> 0)
                      or ((Parity[CSubj] xor edge.Bundle[CAbove][CSubj]) <> 0));
                    tr := Integer(((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH)) <> 0)
                      or ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH)) <> 0));
                    TL := Integer
                      (((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH) xor edge.Bundle
                      [CBelow][CClip]) <> 0) or
                      ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH) xor edge.Bundle
                      [CBelow][CSubj]) <> 0));
                  end;
              end; { case }

              { Update parity }
              (* parity[CClip] := Integer((parity[CClip] <> 0) xor (edge.bundle[CAbove][CClip] <> 0));
                parity[CSubj] := Integer((parity[CSubj] <> 0) xor (edge.bundle[CAbove][CSubj] <> 0));
              *)
              Parity[CClip] := Parity[CClip] xor edge.Bundle[CAbove][CClip];
              Parity[CSubj] := Parity[CSubj] xor edge.Bundle[CAbove][CSubj];

              { Update horizontal state }
              if Exists[CClip] <> 0 then
                  Horiz[CClip] := CNextHorizontalEdgeState[Integer(Horiz[CClip])
                  ][((Exists[CClip] - 1) shl 1) + Parity[CClip]];
              if Exists[CSubj] <> 0 then
                  Horiz[CSubj] := CNextHorizontalEdgeState[Integer(Horiz[CSubj])
                  ][((Exists[CSubj] - 1) shl 1) + Parity[CSubj]];

              _class := tr + (TL shl 1) + (BR shl 2) + (BL shl 3);

              if Contributing <> 0 then
                begin
                  XB := edge.XB;

                  case TVertexType(_class) of
                    vtEMN, vtIMN:
                      begin
                        AddLocalMin(@OutPoly, edge, XB, Yb);
                        Px := XB;
                        Cf := edge.Outp[CAbove];
                      end;
                    vtERI:
                      begin
                        if (XB <> Px) then
                          begin
                            AddRight(Cf, XB, Yb);
                            Px := XB;
                          end;
                        edge.Outp[CAbove] := Cf;
                        Cf := nil;
                      end;
                    vtELI:
                      begin
                        AddLeft(edge.Outp[CBelow], XB, Yb);
                        Px := XB;
                        Cf := edge.Outp[CBelow];
                      end;
                    vtEMX:
                      begin
                        if (XB <> Px) then
                          begin
                            AddLeft(Cf, XB, Yb);
                            Px := XB;
                          end;
                        MergeRight(Cf, edge.Outp[CBelow], OutPoly);
                        Cf := nil;
                      end;
                    vtILI:
                      begin
                        if (XB <> Px) then
                          begin
                            AddLeft(Cf, XB, Yb);
                            Px := XB;
                          end;
                        edge.Outp[CAbove] := Cf;
                        Cf := nil;
                      end;
                    vtIRI:
                      begin
                        AddRight(edge.Outp[CBelow], XB, Yb);
                        Px := XB;
                        Cf := edge.Outp[CBelow];
                        edge.Outp[CBelow] := nil;
                      end;
                    vtIMX:
                      begin
                        if (XB <> Px) then
                          begin
                            AddRight(Cf, XB, Yb);
                            Px := XB;
                          end;
                        MergeLeft(Cf, edge.Outp[CBelow], OutPoly);
                        Cf := nil;
                        edge.Outp[CBelow] := nil;
                      end;
                    vtIMM:
                      begin
                        if (XB <> Px) then
                          begin
                            AddRight(Cf, XB, Yb);
                            Px := XB;
                          end;
                        MergeLeft(Cf, edge.Outp[CBelow], OutPoly);
                        edge.Outp[CBelow] := nil;
                        AddLocalMin(@OutPoly, edge, XB, Yb);
                        Cf := edge.Outp[CAbove];
                      end;
                    vtEMM:
                      begin
                        if (XB <> Px) then
                          begin
                            AddLeft(Cf, XB, Yb);
                            Px := XB;
                          end;
                        MergeRight(Cf, edge.Outp[CBelow], OutPoly);
                        edge.Outp[CBelow] := nil;
                        AddLocalMin(@OutPoly, edge, XB, Yb);
                        Cf := edge.Outp[CAbove];
                      end;
                    vtLED:
                      begin
                        if (edge.Bot.Y = Yb) then
                            AddLeft(edge.Outp[CBelow], XB, Yb);
                        edge.Outp[CAbove] := edge.Outp[CBelow];
                        Px := XB;
                      end;
                    vtRED:
                      begin
                        if (edge.Bot.Y = Yb) then
                            AddRight(edge.Outp[CBelow], XB, Yb);
                        edge.Outp[CAbove] := edge.Outp[CBelow];
                        Px := XB;
                      end;
                    else
                  end; { End of case }
                end;   { End of contributing conditional }
            end;       { End of edge exists conditional }
          edge := edge.Next;
        end; { End of AET loop }

      { Delete terminating edges from the AET, otherwise compute xt }
      edge := Aet;
      while edge <> nil do
        begin
          if (edge.Top.Y = Yb) then
            begin
              PrevEdge := edge.Prev;
              NextEdge := edge.Next;
              if PrevEdge <> nil then
                  PrevEdge.Next := NextEdge
              else
                  Aet := NextEdge;
              if NextEdge <> nil then
                  NextEdge.Prev := PrevEdge;

              { Copy bundle head state to the adjacent tail edge if required }
              if (edge.Bstate[CBelow] = BUNDLE_HEAD) and (PrevEdge <> nil) then
                begin
                  if PrevEdge.Bstate[CBelow] = BUNDLE_TAIL then
                    begin
                      PrevEdge.Outp[CBelow] := edge.Outp[CBelow];
                      PrevEdge.Bstate[CBelow] := UNBUNDLED;
                      if PrevEdge.Prev <> nil then
                        if PrevEdge.Prev.Bstate[CBelow] = BUNDLE_TAIL then
                            PrevEdge.Bstate[CBelow] := BUNDLE_HEAD;
                    end;
                end;
            end
          else
            begin
              if (edge.Top.Y = Yt) then
                  edge.xt := edge.Top.X
              else
                  edge.xt := edge.Bot.X + edge.dx * (Yt - edge.Bot.Y);
            end;

          edge := edge.Next;
        end;

      if Scanbeam < SbtEntries then
        begin
          { === SCANBEAM INTERIOR PROCESSING ============================== }

          BuildIntersectionTable(It, Aet, dy);

          { Process each node in the intersection table }
          Intersect := It;
          while Intersect <> nil do
            begin
              E0 := Intersect.IE[0];
              e1 := Intersect.IE[1];

              { Only generate output for contributing intersections }
              if ((E0.Bundle[CAbove][CClip] <> 0) or (E0.Bundle[CAbove][CSubj] <> 0)) and
                ((e1.Bundle[CAbove][CClip] <> 0) or (e1.Bundle[CAbove][CSubj] <> 0)) then
                begin
                  p := E0.Outp[CAbove];
                  q := e1.Outp[CAbove];
                  ix := Intersect.Point.X;
                  iy := Intersect.Point.Y + Yb;

                  Inn[CClip] :=
                    Integer(((E0.Bundle[CAbove][CClip] <> 0) and (E0.Bside[CClip] = 0)) or
                    ((e1.Bundle[CAbove][CClip] <> 0) and (e1.Bside[CClip] <> 0)) or
                    ((E0.Bundle[CAbove][CClip] = 0) and (e1.Bundle[CAbove][CClip] = 0) and
                    (E0.Bside[CClip] <> 0) and (e1.Bside[CClip] <> 0)));

                  Inn[CSubj] :=
                    Integer(((E0.Bundle[CAbove][CSubj] <> 0) and (E0.Bside[CSubj] = 0)) or
                    ((e1.Bundle[CAbove][CSubj] <> 0) and (e1.Bside[CSubj] <> 0)) or
                    ((E0.Bundle[CAbove][CSubj] = 0) and (e1.Bundle[CAbove][CSubj] = 0) and
                    (E0.Bside[CSubj] <> 0) and (e1.Bside[CSubj] <> 0)));

                  { Determine quadrant occupancies }
                  case SetOperation of

                    goDiff, goInt:
                      begin
                        tr := Integer((Inn[CClip] <> 0) and (Inn[CSubj] <> 0));
                        TL := Integer(((Inn[CClip] xor e1.Bundle[CAbove][CClip]) <> 0) and
                          ((Inn[CSubj] xor e1.Bundle[CAbove][CSubj]) <> 0));
                        BR := Integer(((Inn[CClip] xor E0.Bundle[CAbove][CClip]) <> 0) and
                          ((Inn[CSubj] xor E0.Bundle[CAbove][CSubj]) <> 0));
                        BL := Integer
                          (((Inn[CClip] xor e1.Bundle[CAbove][CClip] xor E0.Bundle[CAbove]
                          [CClip]) <> 0) and
                          ((Inn[CSubj] xor e1.Bundle[CAbove][CSubj] xor E0.Bundle[CAbove]
                          [CSubj]) <> 0));
                      end;

                    goXor:
                      begin
                        tr := Integer((Inn[CClip] <> 0) xor (Inn[CSubj] <> 0));
                        TL := Integer((Inn[CClip] xor e1.Bundle[CAbove][CClip])
                          xor (Inn[CSubj] xor e1.Bundle[CAbove][CSubj]));
                        BR := Integer((Inn[CClip] xor E0.Bundle[CAbove][CClip])
                          xor (Inn[CSubj] xor E0.Bundle[CAbove][CSubj]));
                        BL := Integer
                          ((Inn[CClip] xor e1.Bundle[CAbove][CClip] xor E0.Bundle[CAbove]
                          [CClip]) xor (Inn[CSubj] xor e1.Bundle[CAbove][CSubj]
                          xor E0.Bundle[CAbove][CSubj]));
                      end;

                    goUnion:
                      begin
                        tr := Integer((Inn[CClip] <> 0) or (Inn[CSubj] <> 0));
                        TL := Integer(((Inn[CClip] xor e1.Bundle[CAbove][CClip]) <> 0) or
                          ((Inn[CSubj] xor e1.Bundle[CAbove][CSubj]) <> 0));
                        BR := Integer(((Inn[CClip] xor E0.Bundle[CAbove][CClip]) <> 0) or
                          ((Inn[CSubj] xor E0.Bundle[CAbove][CSubj]) <> 0));
                        BL := Integer
                          (((Inn[CClip] xor e1.Bundle[CAbove][CClip] xor E0.Bundle[CAbove]
                          [CClip]) <> 0) or
                          ((Inn[CSubj] xor e1.Bundle[CAbove][CSubj] xor E0.Bundle[CAbove]
                          [CSubj]) <> 0));
                      end;
                  end; { case }

                  _class := tr + (TL shl 1) + (BR shl 2) + (BL shl 3);

                  case TVertexType(_class) of
                    vtEMN:
                      begin
                        AddLocalMin(@OutPoly, E0, ix, iy);
                        e1.Outp[CAbove] := E0.Outp[CAbove];
                      end;
                    vtERI:
                      begin
                        if p <> nil then
                          begin
                            AddRight(p, ix, iy);
                            e1.Outp[CAbove] := p;
                            E0.Outp[CAbove] := nil;
                          end;
                      end;
                    vtELI:
                      begin
                        if q <> nil then
                          begin
                            AddLeft(q, ix, iy);
                            E0.Outp[CAbove] := q;
                            e1.Outp[CAbove] := nil;
                          end;
                      end;
                    vtEMX:
                      begin
                        if (p <> nil) and (q <> nil) then
                          begin
                            AddLeft(p, ix, iy);
                            MergeRight(p, q, OutPoly);
                            E0.Outp[CAbove] := nil;
                            e1.Outp[CAbove] := nil;
                          end;
                      end;
                    vtIMN:
                      begin
                        AddLocalMin(@OutPoly, E0, ix, iy);
                        e1.Outp[CAbove] := E0.Outp[CAbove];
                      end;
                    vtILI:
                      begin
                        if p <> nil then
                          begin
                            AddLeft(p, ix, iy);
                            e1.Outp[CAbove] := p;
                            E0.Outp[CAbove] := nil;
                          end;
                      end;
                    vtIRI:
                      begin
                        if q <> nil then
                          begin
                            AddRight(q, ix, iy);
                            E0.Outp[CAbove] := q;
                            e1.Outp[CAbove] := nil;
                          end;
                      end;
                    vtIMX:
                      begin
                        if (p <> nil) and (q <> nil) then
                          begin
                            AddRight(p, ix, iy);
                            MergeLeft(p, q, OutPoly);
                            E0.Outp[CAbove] := nil;
                            e1.Outp[CAbove] := nil;
                          end;
                      end;
                    vtIMM:
                      begin
                        if (p <> nil) and (q <> nil) then
                          begin
                            AddRight(p, ix, iy);
                            MergeLeft(p, q, OutPoly);
                            AddLocalMin(@OutPoly, E0, ix, iy);
                            e1.Outp[CAbove] := E0.Outp[CAbove];
                          end;
                      end;
                    vtEMM:
                      begin
                        if (p <> nil) and (q <> nil) then
                          begin
                            AddLeft(p, ix, iy);
                            MergeRight(p, q, OutPoly);
                            AddLocalMin(@OutPoly, E0, ix, iy);
                            e1.Outp[CAbove] := E0.Outp[CAbove];
                          end;
                      end;
                    else
                  end; { End of case }
                end;   { End of contributing intersection conditional }

              { Swap bundle sides in response to edge crossing }
              if (E0.Bundle[CAbove][CClip] <> 0) then
                  e1.Bside[CClip] := Integer(e1.Bside[CClip] = 0);
              if (e1.Bundle[CAbove][CClip] <> 0) then
                  E0.Bside[CClip] := Integer(E0.Bside[CClip] = 0);
              if (E0.Bundle[CAbove][CSubj] <> 0) then
                  e1.Bside[CSubj] := Integer(e1.Bside[CSubj] = 0);
              if (e1.Bundle[CAbove][CSubj] <> 0) then
                  E0.Bside[CSubj] := Integer(E0.Bside[CSubj] = 0);

              { Swap e0 and e1 bundles in the AET }
              PrevEdge := E0.Prev;
              NextEdge := e1.Next;
              if NextEdge <> nil then
                  NextEdge.Prev := E0;

              if E0.Bstate[CAbove] = BUNDLE_HEAD then
                begin
                  Search := FTRUE;
                  while Search <> 0 do
                    begin
                      PrevEdge := PrevEdge.Prev;
                      if PrevEdge <> nil then
                        begin
                          if PrevEdge.Bstate[CAbove] <> BUNDLE_TAIL then
                              Search := FFALSE;
                        end
                      else
                          Search := FFALSE;
                    end;
                end;
              if PrevEdge = nil then
                begin
                  Aet.Prev := e1;
                  e1.Next := Aet;
                  Aet := E0.Next;
                end
              else
                begin
                  PrevEdge.Next.Prev := e1;
                  e1.Next := PrevEdge.Next;
                  PrevEdge.Next := E0.Next;
                end;
              E0.Next.Prev := PrevEdge;
              e1.Next.Prev := e1;
              E0.Next := NextEdge;

              Intersect := Intersect.Next;
            end; { End of IT loop }

          { Prepare for next scanbeam }
          edge := Aet;
          while edge <> nil do
            begin
              NextEdge := edge.Next;
              SuccEdge := edge.Succ;

              if (edge.Top.Y = Yt) and (SuccEdge <> nil) then
                begin
                  { Replace AET edge by its successor }
                  SuccEdge.Outp[CBelow] := edge.Outp[CAbove];
                  SuccEdge.Bstate[CBelow] := edge.Bstate[CAbove];
                  SuccEdge.Bundle[CBelow][CClip] := edge.Bundle[CAbove][CClip];
                  SuccEdge.Bundle[CBelow][CSubj] := edge.Bundle[CAbove][CSubj];
                  PrevEdge := edge.Prev;
                  if PrevEdge <> nil then
                      PrevEdge.Next := SuccEdge
                  else
                      Aet := SuccEdge;
                  if NextEdge <> nil then
                      NextEdge.Prev := SuccEdge;
                  SuccEdge.Prev := PrevEdge;
                  SuccEdge.Next := NextEdge;
                end
              else
                begin
                  { Update this edge }
                  edge.Outp[CBelow] := edge.Outp[CAbove];
                  edge.Bstate[CBelow] := edge.Bstate[CAbove];
                  edge.Bundle[CBelow][CClip] := edge.Bundle[CAbove][CClip];
                  edge.Bundle[CBelow][CSubj] := edge.Bundle[CAbove][CSubj];
                  edge.XB := edge.xt;
                end;
              edge.Outp[CAbove] := nil;
              edge := NextEdge;
            end;
        end;
    end; { === END OF SCANBEAM PROCESSING ================================== }

  { Generate Result Polygon from OutPoly }
  ResultPolygon.Contour := nil;
  ResultPolygon.Hole := nil;
  ResultPolygon.NumContours := CountContours(OutPoly);
  if ResultPolygon.NumContours > 0 then
    begin
      MALLOC(Pointer(ResultPolygon.Hole), ResultPolygon.NumContours *
        SizeOf(Integer), 'hole flag table creation');
      MALLOC(Pointer(ResultPolygon.Contour), ResultPolygon.NumContours *
        SizeOf(TGpcVertexList), 'contour creation');
      Poly := OutPoly;
      C := 0;

      while Poly <> nil do
        begin
          Npoly := Poly.Next;
          if Poly.Active <> 0 then
            begin
              ResultPolygon.Hole[C] := Poly.Proxy.Hole;
              ResultPolygon.Contour[C].NumVertices := Poly.Active;
              MALLOC(Pointer(ResultPolygon.Contour[C].Vertex),
                ResultPolygon.Contour[C].NumVertices * SizeOf(TGpcVertex),
                'vertex creation');

              v := ResultPolygon.Contour[C].NumVertices - 1;
              Vtx := Poly.Proxy.v[CLeft];
              while Vtx <> nil do
                begin
                  NV := Vtx.Next;
                  ResultPolygon.Contour[C].Vertex[v].X := Vtx.X;
                  ResultPolygon.Contour[C].Vertex[v].Y := Vtx.Y;
                  Free(Pointer(Vtx));
                  Dec(v);
                  Vtx := NV;
                end;
              Inc(C);
            end;
          Free(Pointer(Poly));
          Poly := Npoly;
        end;
    end
  else
    begin
      Poly := OutPoly;
      while Poly <> nil do
        begin
          Npoly := Poly.Next;
          Free(Pointer(Poly));
          Poly := Npoly;
        end;

    end;

  { Tidy up }
  Reset_it(It);
  ResetLocalMinimaTable(LocalMinimaTable);
  Free(Pointer(C_heap));
  Free(Pointer(S_heap));
  Free(Pointer(Sbt));
end;

procedure GpcFreeTristrip(TriStrip: PGpcTristrip);
var
  s: Integer;
begin
  for s := 0 to TriStrip.NumStrips - 1 do
      CFree(Pointer(TriStrip.Strip[s].Vertex));
  CFree(Pointer(TriStrip.Strip));
  TriStrip.NumStrips := 0;
end;

procedure GpcPolygonToTristrip(s: PGpcPolygon; T: PGpcTristrip);
var
  C: TGpcPolygon;
begin
  C.NumContours := 0;
  C.Hole := nil;
  C.Contour := nil;
  GpcTristripClip(goDiff, s, @C, T);
end;

procedure GpcTristripClip(Op: TGpcOp; CSubj: PGpcPolygon; CClip: PGpcPolygon;
  Result: PGpcTristrip);
var
  SbTree: PScanBeamTree;
  It: PIntersectionNode;
  Intersect: PIntersectionNode;
  edge, Prev_edge, Next_edge, Succ_edge, E0, e1: PEdgeNode;
  Aet: PEdgeNode;
  C_heap, S_heap: PEdgeNodeArray;
  Cf: PEdgeNode;
  LocalMinimaTable, Local_min: PLocalMinimaTableNode;
  TList, tN, Tnn, p, q: PPolygonNode;
  lt, Ltn, RT, Rtn: PVertexNode;
  Horiz: array [0 .. 1] of THorizontalEdgeState;
  Cft: TVertexType;
  InArray: array [0 .. 1] of Integer;
  Exists: array [0 .. 1] of Integer;
  Parity: array [0 .. 1] of Integer;
  s, v, Contributing, Search, Scanbeam, Sbt_entries: Integer;
  Vclass, BL, BR, TL, tr: Integer;
  Sbt: PDoubleArray;
  XB, Px, Nx, Yb, Yt, dy, ix, iy: Double;
begin
  SbTree := nil;
  It := nil;
  Aet := nil;
  C_heap := nil;
  S_heap := nil;
  LocalMinimaTable := nil;
  TList := nil;
  Parity[0] := CLeft;
  Parity[1] := CLeft;
  Scanbeam := 0;
  Sbt_entries := 0;
  Sbt := nil;

  // * Test for trivial NULL Result cases */
  if (((CSubj.NumContours = 0) and (CClip.NumContours = 0)) or
    ((CSubj.NumContours = 0) and ((Op = goInt) or (Op = goDiff))) or
    ((CClip.NumContours = 0) and (Op = goInt))) then
    begin
      Result.NumStrips := 0;
      Result.Strip := nil;
      Exit;
    end;

  // * Identify potentialy contributing contours */
  if (((Op = goInt) or (Op = goDiff)) and (CSubj.NumContours > 0) and
    (CClip.NumContours > 0)) then
    begin
      MinimaxTest(CSubj, CClip, Op);
    end;

  // * Build LocalMinimaTable */
  if (CSubj.NumContours > 0) then
      S_heap := BuildLocalMinimaTable(LocalMinimaTable, SbTree, Sbt_entries, CSubj, gpc.CSubj, Op);
  if (CClip.NumContours > 0) then
      C_heap := BuildLocalMinimaTable(LocalMinimaTable, SbTree, Sbt_entries, CClip, gpc.CClip, Op);

  // * Return a NULL Result if no contours contribute */
  if (LocalMinimaTable = nil) then
    begin
      Result.NumStrips := 0;
      Result.Strip := nil;
      ResetLocalMinimaTable(LocalMinimaTable);
      Free(Pointer(S_heap));
      Free(Pointer(C_heap));
      Exit;
    end;

  // * Build scanbeam table from scanbeam tree */
  MALLOC(Pointer(Sbt), Sbt_entries * SizeOf(Double), 'sbt creation');
  BuildScanBeamTree(Scanbeam, Sbt, SbTree);
  Scanbeam := 0;
  FreeScanBeamTree(SbTree);

  // * Invert CClip Polygon for difference operation */
  if (Op = goDiff) then
      Parity[gpc.CClip] := CRight;

  Local_min := LocalMinimaTable;

  // * Process each scanbeam */
  while (Scanbeam < Sbt_entries) do
    begin
      // * Set yb and yt to the bottom and top of the scanbeam */
      Yb := Sbt[Scanbeam];
      Inc(Scanbeam);
      if (Scanbeam < Sbt_entries) then
        begin
          Yt := Sbt[Scanbeam];
          dy := Yt - Yb;
        end;

      // * === SCANBEAM BOUNDARY PROCESSING ================================ */

      // * If LocalMinimaTable node corresponding to yb exists */
      if (Local_min <> nil) then
        begin
          if (Local_min.Y = Yb) then
            begin
              // * Add edges starting at this local minimum to the AET */
              edge := Local_min.FirstBound;
              while edge <> nil do
                begin
                  AddEdgeToAET(Aet, edge, nil);
                  edge := edge.Next_bound;
                end;
              Local_min := Local_min.Next;
            end;
        end;

      // * Set dummy previous x value */
      Px := -CDoubleMax;

      // * Create bundles within AET */
      E0 := Aet;
      e1 := Aet;

      // * Set up bundle fields of first edge */
      Aet.Bundle[CAbove][Aet.Typ] := Ord(Aet.Top.Y <> Yb);
      Aet.Bundle[CAbove][Ord(Aet.Typ = 0)] := FFALSE;
      Aet.Bstate[CAbove] := UNBUNDLED;

      Next_edge := Aet.Next;
      while Next_edge <> nil do
        begin

          // * Set up bundle fields of next edge */
          Next_edge.Bundle[CAbove][Next_edge.Typ] := Ord(Next_edge.Top.Y <> Yb);
          Next_edge.Bundle[CAbove][Ord(Next_edge.Typ = 0)] := FFALSE;
          Next_edge.Bstate[CAbove] := UNBUNDLED;

          // * Bundle edges CAbove the scanbeam boundary if they coincide */
          if (Next_edge.Bundle[CAbove][Next_edge.Typ] <> 0) then
            begin
              if (EQ(E0.XB, Next_edge.XB) and EQ(E0.dx, Next_edge.dx) and
                (E0.Top.Y <> Yb)) then
                begin
                  Next_edge.Bundle[CAbove][Next_edge.Typ] := Next_edge.Bundle[CAbove]
                    [Next_edge.Typ] xor E0.Bundle[CAbove][Next_edge.Typ];
                  Next_edge.Bundle[CAbove][Ord(Next_edge.Typ = 0)] :=
                    E0.Bundle[CAbove][Ord(Next_edge.Typ = 0)];
                  Next_edge.Bstate[CAbove] := BUNDLE_HEAD;
                  E0.Bundle[CAbove][gpc.CClip] := FFALSE;
                  E0.Bundle[CAbove][gpc.CSubj] := FFALSE;
                  E0.Bstate[CAbove] := BUNDLE_TAIL;
                end;
              E0 := Next_edge;
            end;
          Next_edge := Next_edge.Next;
        end;

      Horiz[gpc.CClip] := hesNH;
      Horiz[gpc.CSubj] := hesNH;

      // * Process each edge at this scanbeam boundary */
      edge := Aet;
      while edge <> nil do
        begin
          Exists[gpc.CClip] := edge.Bundle[CAbove][gpc.CClip] +
            (edge.Bundle[CBelow][gpc.CClip] shl 1);
          Exists[gpc.CSubj] := edge.Bundle[CAbove][gpc.CSubj] +
            (edge.Bundle[CBelow][gpc.CSubj] shl 1);

          if ((Exists[gpc.CClip] <> 0) or (Exists[gpc.CSubj] <> 0)) then
            begin
              // * Set bundle side */
              edge.Bside[gpc.CClip] := Parity[gpc.CClip];
              edge.Bside[gpc.CSubj] := Parity[gpc.CSubj];

              // * Determine contributing status and quadrant occupancies */
              case (Op) of

                goDiff, goInt:
                  begin
                    Contributing :=
                      Ord(((Exists[gpc.CClip] <> 0) and ((Parity[gpc.CSubj] <> 0) or
                      (Horiz[gpc.CSubj] <> hesNH))) or ((Exists[gpc.CSubj] <> 0) and
                      ((Parity[gpc.CClip] <> 0) or (Horiz[gpc.CClip] <> hesNH))) or
                      ((Exists[gpc.CClip] <> 0) and (Exists[gpc.CSubj] <> 0) and
                      (Parity[gpc.CClip] = Parity[gpc.CSubj])));
                    BR := (Parity[gpc.CClip]) and (Parity[gpc.CSubj]);
                    BL := (Parity[gpc.CClip] xor edge.Bundle[CAbove][gpc.CClip]) and
                      (Parity[gpc.CSubj] xor edge.Bundle[CAbove][gpc.CSubj]);
                    tr := (Parity[gpc.CClip] xor Ord(Horiz[gpc.CClip] <> hesNH)) and
                      (Parity[gpc.CSubj] xor Ord(Horiz[gpc.CSubj] <> hesNH));
                    TL := (Parity[gpc.CClip] xor (Ord(Horiz[gpc.CClip] <> hesNH)
                      xor edge.Bundle[CBelow][gpc.CClip])) and
                      (Parity[gpc.CSubj] xor (Ord(Horiz[gpc.CSubj] <> hesNH)
                      xor edge.Bundle[CBelow][gpc.CSubj]));
                  end;
                goXor:
                  begin
                    Contributing := Exists[gpc.CClip] or Exists[gpc.CSubj];
                    BR := (Parity[gpc.CClip]) xor (Parity[gpc.CSubj]);
                    BL := (Parity[gpc.CClip] xor edge.Bundle[CAbove][gpc.CClip])
                      xor (Parity[gpc.CSubj] xor edge.Bundle[CAbove][gpc.CSubj]);
                    tr := (Parity[gpc.CClip] xor Ord(Horiz[gpc.CClip] <> hesNH))
                      xor (Parity[gpc.CSubj] xor Ord(Horiz[gpc.CSubj] <> hesNH));
                    TL := (Parity[gpc.CClip] xor (Ord(Horiz[gpc.CClip] <> hesNH)
                      xor edge.Bundle[CBelow][gpc.CClip]))
                      xor (Parity[gpc.CSubj] xor (Ord(Horiz[gpc.CSubj] <> hesNH)
                      xor edge.Bundle[CBelow][gpc.CSubj]));
                  end;
                goUnion:
                  begin
                    Contributing :=
                      Ord(((Exists[gpc.CClip] <> 0) and ((Parity[gpc.CSubj] = 0) or
                      (Horiz[gpc.CSubj] <> hesNH))) or ((Exists[gpc.CSubj] <> 0) and
                      ((Parity[gpc.CClip] = 0) or (Horiz[gpc.CClip] <> hesNH))) or
                      ((Exists[gpc.CClip] <> 0) and (Exists[gpc.CSubj] <> 0) and
                      (Parity[gpc.CClip] = Parity[gpc.CSubj])));
                    BR := (Parity[gpc.CClip]) or (Parity[gpc.CSubj]);
                    BL := (Parity[gpc.CClip] xor edge.Bundle[CAbove][gpc.CClip]) or
                      (Parity[gpc.CSubj] xor edge.Bundle[CAbove][gpc.CSubj]);
                    tr := (Parity[gpc.CClip] xor Ord(Horiz[gpc.CClip] <> hesNH)) or
                      (Parity[gpc.CSubj] xor Ord(Horiz[gpc.CSubj] <> hesNH));
                    TL := (Parity[gpc.CClip] xor (Ord(Horiz[gpc.CClip] <> hesNH)
                      xor edge.Bundle[CBelow][gpc.CClip])) or
                      (Parity[gpc.CSubj] xor (Ord(Horiz[gpc.CSubj] <> hesNH)
                      xor edge.Bundle[CBelow][gpc.CSubj]));
                  end;
              end;

              // * Update parity */
              Parity[gpc.CClip] := Parity[gpc.CClip] xor edge.Bundle[CAbove][gpc.CClip];
              Parity[gpc.CSubj] := Parity[gpc.CSubj] xor edge.Bundle[CAbove][gpc.CSubj];

              // * Update horizontal state */
              if (Exists[gpc.CClip] <> 0) then
                  Horiz[gpc.CClip] := CNextHorizontalEdgeState[Ord(Horiz[gpc.CClip])]
                  [((Exists[gpc.CClip] - 1) shl 1) + Parity[gpc.CClip]];
              if (Exists[gpc.CSubj] <> 0) then
                  Horiz[gpc.CSubj] := CNextHorizontalEdgeState[Ord(Horiz[gpc.CSubj])]
                  [((Exists[gpc.CSubj] - 1) shl 1) + Parity[gpc.CSubj]];

              Vclass := tr + (TL shl 1) + (BR shl 2) + (BL shl 3);

              if (Contributing <> 0) then
                begin
                  XB := edge.XB;

                  case TVertexType(Vclass) of

                    vtEMN:
                      begin
                        NewTristrip(TList, edge, XB, Yb);
                        Cf := edge;
                      end;
                    vtERI:
                      begin
                        edge.Outp[CAbove] := Cf.Outp[CAbove];
                        if (XB <> Cf.XB) then
                            Vertex(edge, CAbove, CRight, XB, Yb);
                        Cf := nil;
                      end;
                    vtELI:
                      begin
                        Vertex(edge, CBelow, CLeft, XB, Yb);
                        edge.Outp[CAbove] := nil;
                        Cf := edge;
                      end;
                    vtEMX:
                      begin
                        if (XB <> Cf.XB) then
                            Vertex(edge, CBelow, CRight, XB, Yb);
                        edge.Outp[CAbove] := nil;
                        Cf := nil;
                      end;
                    vtIMN:
                      begin
                        if (Cft = vtLED) then
                          begin
                            if (Cf.Bot.Y <> Yb) then
                                Vertex(Cf, CBelow, CLeft, Cf.XB, Yb);
                            NewTristrip(TList, Cf, Cf.XB, Yb);
                          end;
                        edge.Outp[CAbove] := Cf.Outp[CAbove];
                        Vertex(edge, CAbove, CRight, XB, Yb);
                      end;
                    vtILI:
                      begin
                        NewTristrip(TList, edge, XB, Yb);
                        Cf := edge;
                        Cft := vtILI;
                      end;
                    vtIRI:
                      begin
                        if (Cft = vtLED) then
                          begin
                            if (Cf.Bot.Y <> Yb) then
                                Vertex(Cf, CBelow, CLeft, Cf.XB, Yb);
                            NewTristrip(TList, Cf, Cf.XB, Yb);
                          end;
                        Vertex(edge, CBelow, CRight, XB, Yb);
                        edge.Outp[CAbove] := nil;
                      end;
                    vtIMX:
                      begin
                        Vertex(edge, CBelow, CLeft, XB, Yb);
                        edge.Outp[CAbove] := nil;
                        Cft := vtIMX;
                      end;
                    vtIMM:
                      begin
                        Vertex(edge, CBelow, CLeft, XB, Yb);
                        edge.Outp[CAbove] := Cf.Outp[CAbove];
                        if (XB <> Cf.XB) then
                            Vertex(Cf, CAbove, CRight, XB, Yb);
                        Cf := edge;
                      end;
                    vtEMM:
                      begin
                        Vertex(edge, CBelow, CRight, XB, Yb);
                        edge.Outp[CAbove] := nil;
                        NewTristrip(TList, edge, XB, Yb);
                        Cf := edge;
                      end;
                    vtLED:
                      begin
                        if (edge.Bot.Y = Yb) then
                            Vertex(edge, CBelow, CLeft, XB, Yb);
                        edge.Outp[CAbove] := edge.Outp[CBelow];
                        Cf := edge;
                        Cft := vtLED;
                      end;
                    vtRED:
                      begin
                        edge.Outp[CAbove] := Cf.Outp[CAbove];
                        if (Cft = vtLED) then
                          begin
                            if (Cf.Bot.Y = Yb) then
                                Vertex(edge, CBelow, CRight, XB, Yb)
                            else if (edge.Bot.Y = Yb) then
                              begin
                                Vertex(Cf, CBelow, CLeft, Cf.XB, Yb);
                                Vertex(edge, CBelow, CRight, XB, Yb);
                              end;
                          end
                        else
                          begin
                            Vertex(edge, CBelow, CRight, XB, Yb);
                            Vertex(edge, CAbove, CRight, XB, Yb);
                          end;
                        Cf := nil;
                      end;
                    // * End of switch */
                  end;
                  // * End of contributing conditional */
                end;
              // * End of edge exists conditional */

            end;
          // * End of AET loop */

          edge := edge.Next
        end;

      // * Delete terminating edges from the AET, otherwise compute xt */
      edge := Aet;
      while edge <> nil do
        begin
          if (edge.Top.Y = Yb) then
            begin
              Prev_edge := edge.Prev;
              Next_edge := edge.Next;
              if (Prev_edge <> nil) then
                  Prev_edge.Next := Next_edge
              else
                  Aet := Next_edge;
              if (Next_edge <> nil) then
                  Next_edge.Prev := Prev_edge;

              // * Copy bundle head state to the adjacent tail edge if required */
              if ((edge.Bstate[CBelow] = BUNDLE_HEAD) and (Prev_edge <> nil)) then
                begin
                  if (Prev_edge.Bstate[CBelow] = BUNDLE_TAIL) then
                    begin
                      Prev_edge.Outp[CBelow] := edge.Outp[CBelow];
                      Prev_edge.Bstate[CBelow] := UNBUNDLED;
                      if (Prev_edge.Prev <> nil) then
                        if (Prev_edge.Prev.Bstate[CBelow] = BUNDLE_TAIL) then
                            Prev_edge.Bstate[CBelow] := BUNDLE_HEAD;
                    end;
                end;
            end
          else
            begin
              if (edge.Top.Y = Yt) then
                  edge.xt := edge.Top.X
              else
                  edge.xt := edge.Bot.X + edge.dx * (Yt - edge.Bot.Y);
            end;

          edge := edge.Next
        end;

      if (Scanbeam < Sbt_entries) then
        begin
          // * === SCANBEAM INTERIOR PROCESSING ============================== */

          BuildIntersectionTable(It, Aet, dy);

          // * Process each node in the intersection table */
          Intersect := It;
          while (Intersect <> nil) do
            begin
              E0 := Intersect.IE[0];
              e1 := Intersect.IE[1];

              // * Only generate output for contributing intersections */
              if (((E0.Bundle[CAbove][gpc.CClip] <> 0) or (E0.Bundle[CAbove][gpc.CSubj] <>
                0)) and ((e1.Bundle[CAbove][gpc.CClip] <> 0) or
                (e1.Bundle[CAbove][gpc.CSubj] <> 0))) then
                begin
                  p := E0.Outp[CAbove];
                  q := e1.Outp[CAbove];
                  ix := Intersect.Point.X;
                  iy := Intersect.Point.Y + Yb;

                  InArray[gpc.CClip] :=
                    Ord(((E0.Bundle[CAbove][gpc.CClip] <> 0) and (E0.Bside[gpc.CClip] = 0))
                    or ((e1.Bundle[CAbove][gpc.CClip] <> 0) and (e1.Bside[gpc.CClip] <> 0))
                    or ((E0.Bundle[CAbove][gpc.CClip] = 0) and
                    (e1.Bundle[CAbove][gpc.CClip] = 0) and (E0.Bside[gpc.CClip] <> 0) and
                    (e1.Bside[gpc.CClip] <> 0)));
                  InArray[gpc.CSubj] :=
                    Ord(((E0.Bundle[CAbove][gpc.CSubj] <> 0) and (E0.Bside[gpc.CSubj] = 0))
                    or ((e1.Bundle[CAbove][gpc.CSubj] <> 0) and (e1.Bside[gpc.CSubj] <> 0))
                    or ((E0.Bundle[CAbove][gpc.CSubj] = 0) and
                    (e1.Bundle[CAbove][gpc.CSubj] = 0) and (E0.Bside[gpc.CSubj] <> 0) and
                    (e1.Bside[gpc.CSubj] <> 0)));

                  // * Determine quadrant occupancies */
                  case (Op) of

                    goDiff, goInt:
                      begin
                        tr := (InArray[gpc.CClip]) and (InArray[gpc.CSubj]);
                        TL := (InArray[gpc.CClip] xor e1.Bundle[CAbove][gpc.CClip]) and
                          (InArray[gpc.CSubj] xor e1.Bundle[CAbove][gpc.CSubj]);
                        BR := (InArray[gpc.CClip] xor E0.Bundle[CAbove][gpc.CClip]) and
                          (InArray[gpc.CSubj] xor E0.Bundle[CAbove][gpc.CSubj]);
                        BL := (InArray[gpc.CClip] xor e1.Bundle[CAbove][gpc.CClip]
                          xor E0.Bundle[CAbove][gpc.CClip]) and
                          (InArray[gpc.CSubj] xor e1.Bundle[CAbove][gpc.CSubj]
                          xor E0.Bundle[CAbove][gpc.CSubj]);

                      end;
                    goXor:
                      begin
                        tr := (InArray[gpc.CClip]) xor (InArray[gpc.CSubj]);
                        TL := (InArray[gpc.CClip] xor e1.Bundle[CAbove][gpc.CClip])
                          xor (InArray[gpc.CSubj] xor e1.Bundle[CAbove][gpc.CSubj]);
                        BR := (InArray[gpc.CClip] xor E0.Bundle[CAbove][gpc.CClip])
                          xor (InArray[gpc.CSubj] xor E0.Bundle[CAbove][gpc.CSubj]);
                        BL := (InArray[gpc.CClip] xor e1.Bundle[CAbove][gpc.CClip]
                          xor E0.Bundle[CAbove][gpc.CClip])
                          xor (InArray[gpc.CSubj] xor e1.Bundle[CAbove][gpc.CSubj]
                          xor E0.Bundle[CAbove][gpc.CSubj]);
                      end;
                    goUnion:
                      begin
                        tr := (InArray[gpc.CClip]) or (InArray[gpc.CSubj]);
                        TL := (InArray[gpc.CClip] xor e1.Bundle[CAbove][gpc.CClip]) or
                          (InArray[gpc.CSubj] xor e1.Bundle[CAbove][gpc.CSubj]);
                        BR := (InArray[gpc.CClip] xor E0.Bundle[CAbove][gpc.CClip]) or
                          (InArray[gpc.CSubj] xor E0.Bundle[CAbove][gpc.CSubj]);
                        BL := (InArray[gpc.CClip] xor e1.Bundle[CAbove][gpc.CClip]
                          xor E0.Bundle[CAbove][gpc.CClip]) or
                          (InArray[gpc.CSubj] xor e1.Bundle[CAbove][gpc.CSubj]
                          xor E0.Bundle[CAbove][gpc.CSubj]);
                      end;
                  end;

                  Vclass := tr + (TL shl 1) + (BR shl 2) + (BL shl 3);

                  case TVertexType(Vclass) of
                    vtEMN:
                      begin
                        NewTristrip(TList, e1, ix, iy);
                        E0.Outp[CAbove] := e1.Outp[CAbove];
                      end;
                    vtERI:
                      begin
                        if (p <> nil) then
                          begin
                            P_EDGE(Prev_edge, E0, CAbove, Px, iy);
                            Vertex(Prev_edge, CAbove, CLeft, Px, iy);
                            Vertex(E0, CAbove, CRight, ix, iy);
                            e1.Outp[CAbove] := E0.Outp[CAbove];
                            E0.Outp[CAbove] := nil;
                          end;
                      end;
                    vtELI:
                      begin
                        if (q <> nil) then
                          begin
                            N_EDGE(Next_edge, e1, CAbove, Nx, iy);
                            Vertex(e1, CAbove, CLeft, ix, iy);
                            Vertex(Next_edge, CAbove, CRight, Nx, iy);
                            E0.Outp[CAbove] := e1.Outp[CAbove];
                            e1.Outp[CAbove] := nil;
                          end
                      end;
                    vtEMX:
                      begin
                        if ((p <> nil) and (q <> nil)) then
                          begin
                            Vertex(E0, CAbove, CLeft, ix, iy);
                            E0.Outp[CAbove] := nil;
                            e1.Outp[CAbove] := nil;
                          end
                      end;
                    vtIMN:
                      begin
                        P_EDGE(Prev_edge, E0, CAbove, Px, iy);
                        Vertex(Prev_edge, CAbove, CLeft, Px, iy);
                        N_EDGE(Next_edge, e1, CAbove, Nx, iy);
                        Vertex(Next_edge, CAbove, CRight, Nx, iy);
                        NewTristrip(TList, Prev_edge, Px, iy);
                        e1.Outp[CAbove] := Prev_edge.Outp[CAbove];
                        Vertex(e1, CAbove, CRight, ix, iy);
                        NewTristrip(TList, E0, ix, iy);
                        Next_edge.Outp[CAbove] := E0.Outp[CAbove];
                        Vertex(Next_edge, CAbove, CRight, Nx, iy);
                      end;
                    vtILI:
                      begin
                        if (p <> nil) then
                          begin
                            Vertex(E0, CAbove, CLeft, ix, iy);
                            N_EDGE(Next_edge, e1, CAbove, Nx, iy);
                            Vertex(Next_edge, CAbove, CRight, Nx, iy);
                            e1.Outp[CAbove] := E0.Outp[CAbove];
                            E0.Outp[CAbove] := nil;
                          end;
                      end;
                    vtIRI:
                      begin
                        if (q <> nil) then
                          begin
                            Vertex(e1, CAbove, CRight, ix, iy);
                            P_EDGE(Prev_edge, E0, CAbove, Px, iy);
                            Vertex(Prev_edge, CAbove, CLeft, Px, iy);
                            E0.Outp[CAbove] := e1.Outp[CAbove];
                            e1.Outp[CAbove] := nil;
                          end;
                      end;
                    vtIMX:
                      begin
                        if ((p <> nil) and (q <> nil)) then
                          begin
                            Vertex(E0, CAbove, CRight, ix, iy);
                            Vertex(e1, CAbove, CLeft, ix, iy);
                            E0.Outp[CAbove] := nil;
                            e1.Outp[CAbove] := nil;
                            P_EDGE(Prev_edge, E0, CAbove, Px, iy);
                            Vertex(Prev_edge, CAbove, CLeft, Px, iy);
                            NewTristrip(TList, Prev_edge, Px, iy);
                            N_EDGE(Next_edge, e1, CAbove, Nx, iy);
                            Vertex(Next_edge, CAbove, CRight, Nx, iy);
                            Next_edge.Outp[CAbove] := Prev_edge.Outp[CAbove];
                            Vertex(Next_edge, CAbove, CRight, Nx, iy);
                          end;
                      end;
                    vtIMM:
                      begin
                        if ((p <> nil) and (q <> nil)) then
                          begin
                            Vertex(E0, CAbove, CRight, ix, iy);
                            Vertex(e1, CAbove, CLeft, ix, iy);
                            P_EDGE(Prev_edge, E0, CAbove, Px, iy);
                            Vertex(Prev_edge, CAbove, CLeft, Px, iy);
                            NewTristrip(TList, Prev_edge, Px, iy);
                            N_EDGE(Next_edge, e1, CAbove, Nx, iy);
                            Vertex(Next_edge, CAbove, CRight, Nx, iy);
                            e1.Outp[CAbove] := Prev_edge.Outp[CAbove];
                            Vertex(e1, CAbove, CRight, ix, iy);
                            NewTristrip(TList, E0, ix, iy);
                            Next_edge.Outp[CAbove] := E0.Outp[CAbove];
                            Vertex(Next_edge, CAbove, CRight, Nx, iy);
                          end;
                      end;
                    vtEMM:
                      begin
                        if ((p <> nil) and (q <> nil)) then
                          begin
                            Vertex(E0, CAbove, CLeft, ix, iy);
                            NewTristrip(TList, e1, ix, iy);
                            E0.Outp[CAbove] := e1.Outp[CAbove];
                          end;
                      end;
                  end; // * End of switch */
                end;   // * End of contributing intersection conditional */

              // * Swap bundle sides in response to edge crossing */
              if (E0.Bundle[CAbove][gpc.CClip] <> 0) then
                  e1.Bside[gpc.CClip] := Ord(e1.Bside[gpc.CClip] = 0);
              if (e1.Bundle[CAbove][gpc.CClip] <> 0) then
                  E0.Bside[gpc.CClip] := Ord(E0.Bside[gpc.CClip] = 0);
              if (E0.Bundle[CAbove][gpc.CSubj] <> 0) then
                  e1.Bside[gpc.CSubj] := Ord(e1.Bside[gpc.CSubj] = 0);
              if (e1.Bundle[CAbove][gpc.CSubj] <> 0) then
                  E0.Bside[gpc.CSubj] := Ord(E0.Bside[gpc.CSubj] = 0);

              // * Swap e0 and e1 bundles in the AET */
              Prev_edge := E0.Prev;
              Next_edge := e1.Next;
              if (e1.Next <> nil) then
                  e1.Next.Prev := E0;

              if (E0.Bstate[CAbove] = BUNDLE_HEAD) then
                begin
                  Search := FTRUE;
                  while (Search <> FFALSE) do
                    begin
                      Prev_edge := Prev_edge.Prev;
                      if (Prev_edge <> nil) then
                        begin
                          if ((Prev_edge.Bundle[CAbove][gpc.CClip] <> 0) or
                            (Prev_edge.Bundle[CAbove][gpc.CSubj] <> 0) or
                            (Prev_edge.Bstate[CAbove] = BUNDLE_HEAD)) then
                              Search := FFALSE;
                        end
                      else
                          Search := FFALSE;
                    end;
                end;
              if (Prev_edge = nil) then
                begin
                  e1.Next := Aet;
                  Aet := E0.Next;
                end
              else
                begin
                  e1.Next := Prev_edge.Next;
                  Prev_edge.Next := E0.Next;
                end;
              E0.Next.Prev := Prev_edge;
              e1.Next.Prev := e1;
              E0.Next := Next_edge;
              Intersect := Intersect.Next;
            end; // * End of IT loop*/

          // * Prepare for next scanbeam */
          edge := Aet;
          while (edge <> nil) do
            begin
              Next_edge := edge.Next;
              Succ_edge := edge.Succ;

              if ((edge.Top.Y = Yt) and (Succ_edge <> nil)) then
                begin
                  // * Replace AET edge by its successor */
                  Succ_edge.Outp[CBelow] := edge.Outp[CAbove];
                  Succ_edge.Bstate[CBelow] := edge.Bstate[CAbove];
                  Succ_edge.Bundle[CBelow][gpc.CClip] := edge.Bundle[CAbove][gpc.CClip];
                  Succ_edge.Bundle[CBelow][gpc.CSubj] := edge.Bundle[CAbove][gpc.CSubj];
                  Prev_edge := edge.Prev;
                  if (Prev_edge <> nil) then
                      Prev_edge.Next := Succ_edge
                  else
                      Aet := Succ_edge;
                  if (Next_edge <> nil) then
                      Next_edge.Prev := Succ_edge;
                  Succ_edge.Prev := Prev_edge;
                  Succ_edge.Next := Next_edge;
                end
              else
                begin
                  // * Update this edge */
                  edge.Outp[CBelow] := edge.Outp[CAbove];
                  edge.Bstate[CBelow] := edge.Bstate[CAbove];
                  edge.Bundle[CBelow][gpc.CClip] := edge.Bundle[CAbove][gpc.CClip];
                  edge.Bundle[CBelow][gpc.CSubj] := edge.Bundle[CAbove][gpc.CSubj];
                  edge.XB := edge.xt;
                end;
              edge.Outp[CAbove] := nil;
              edge := Next_edge;
            end;
        end;
    end; // * === END OF SCANBEAM PROCESSING ================================== */

  // * Generate Result TriStrip from tlist */
  Result.Strip := nil;
  Result.NumStrips := CountTristrips(TList);
  if (Result.NumStrips > 0) then
    begin
      MALLOC(Pointer(Result.Strip), Result.NumStrips * SizeOf(TGpcVertexList),
        'tristrip list creation');

      s := 0;
      tN := TList;
      while (tN <> nil) do
        begin
          Tnn := tN.Next;

          if (tN.Active > 2) then
            begin
              // * Valid TriStrip: copy the vertices and Free the heap */
              Result.Strip[s].NumVertices := tN.Active;
              MALLOC(Pointer(Result.Strip[s].Vertex), tN.Active * SizeOf(TGpcVertex),
                'tristrip creation');
              v := 0;
              if (CInvertTriStrips <> 0) then
                begin
                  lt := tN.v[CRight];
                  RT := tN.v[CLeft];
                end
              else
                begin
                  lt := tN.v[CLeft];
                  RT := tN.v[CRight];
                end;
              while ((lt <> nil) or (RT <> nil)) do
                begin
                  if (lt <> nil) then
                    begin
                      Ltn := lt.Next;
                      Result.Strip[s].Vertex[v].X := lt.X;
                      Result.Strip[s].Vertex[v].Y := lt.Y;
                      Inc(v);
                      Free(Pointer(lt));
                      lt := Ltn;
                    end;
                  if (RT <> nil) then
                    begin
                      Rtn := RT.Next;
                      Result.Strip[s].Vertex[v].X := RT.X;
                      Result.Strip[s].Vertex[v].Y := RT.Y;
                      Inc(v);
                      Free(Pointer(RT));
                      RT := Rtn;
                    end;
                end;
              Inc(s);
            end
          else
            begin
              // * Invalid TriStrip: just Free the heap */
              lt := tN.v[CLeft];
              while (lt <> nil) do
                begin
                  Ltn := lt.Next;
                  Free(Pointer(lt));
                  lt := Ltn
                end;
              RT := tN.v[CRight];
              while (RT <> nil) do
                begin
                  Rtn := RT.Next;
                  Free(Pointer(RT));
                  RT := Rtn
                end;
            end;
          Free(Pointer(tN));

          tN := Tnn;
        end;
    end;

  // * Tidy up */
  Reset_it(It);
  ResetLocalMinimaTable(LocalMinimaTable);
  Free(Pointer(C_heap));
  Free(Pointer(S_heap));
  Free(Pointer(Sbt));
end;

end.   
