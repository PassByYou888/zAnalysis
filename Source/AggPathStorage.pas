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
unit AggPathStorage;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggMath,
  AggBezierArc,
  AggVertexSource,
  AggTransAffine;

// Allocation parameters
const
  CAggBlockShift = 8;
  CAggBlockSize  = 1 shl CAggBlockShift;
  CAggBlockMask  = CAggBlockSize - 1;
  CAggBlockPool  = 256;

type
  TAggPathStorage = class;

  TAggPathStorageVertexSource = class(TAggVertexSource)
  private
    FPath: TAggPathStorage;

    FVertexIndex: Cardinal;
  public
    constructor Create; overload;
    constructor Create(p: TAggPathStorage); overload;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  // A container to store vertices with their flags.
  // A path consists of a number of contours separated with "MoveTo"
  // commands. The path storage can keep and maintain more than one
  // path.
  // To navigate to the beginning of a particular path, use rewind(PathID);
  // Where PathID is what StartNewPath() returns. So, when you call
  // StartNewPath() you need to store its return value somewhere else
  // to navigate to the path afterwards.
  TAggPathStorage = class(TAggVertexSource)
  private
    FTotalVertices, FTotalBlocks, FMaxBlocks: Cardinal;

    FCoordBlocks: PPDouble;
    FCmdBlocks: PPInt8u;

    FIterator: Cardinal;

    // Private
    procedure AllocateBlock(nb: Cardinal);
    function StoragePtrs(Xy_ptr: PPDouble): PInt8u;

    function PerceivePolygonOrientation(Start, stop: Cardinal): Cardinal;

    // Allows you to modify vertex command. The caller must know
    // the index of the vertex.
    function GetCommand(index: Cardinal): Cardinal;
    procedure SetCommand(index, Cmd: Cardinal);
  public
    constructor Create; overload;
    constructor Create(ps: TAggPathStorage); overload;
    destructor Destroy; override;

    procedure RemoveAll; override;

    function LastVertex(X, Y: PDouble): Cardinal;
    function PrevVertex(X, Y: PDouble): Cardinal;

    function LastX: Double;
    function LastY: Double;

    procedure RelativeToAbsolute(X, Y: PDouble);

    procedure MoveTo(X, Y: Double);
    procedure MoveRelative(dx, dy: Double);

    procedure LineTo(X, Y: Double);
    procedure LineRelative(dx, dy: Double);

    procedure HorizontalLineTo(X: Double);
    procedure HorizontalLineRelative(dx: Double);

    procedure VerticalLineTo(Y: Double);
    procedure VerticalLineRelative(dy: Double);

    procedure ArcTo(RX, RY, angle: Double; LargeArcFlag, SweepFlag: Boolean; X, Y: Double); overload;
    procedure ArcRelative(RX, RY, angle: Double; LargeArcFlag, SweepFlag: Boolean; dx, dy: Double); overload;

    procedure Curve3(ControlX, ControlY, tox, ToY: Double); overload;
    procedure Curve3Relative(DeltaControlX, DeltaControlY, DeltaToX, DeltaToY: Double); overload;

    procedure Curve3(tox, ToY: Double); overload;
    procedure Curve3Relative(DeltaToX, DeltaToY: Double); overload;

    procedure Curve4(Control1X, Control1Y, Control2X, Control2Y, tox, ToY: Double); overload;
    procedure Curve4Relative(DeltaControl1X, DeltaControl1Y, DeltaControl2X, DeltaControl2Y, DeltaToX, DeltaToY: Double); overload;

    procedure Curve4(Control2X, Control2Y, tox, ToY: Double); overload;
    procedure Curve4Relative(DeltaControl2X, DeltaControl2Y, DeltaToX, DeltaToY: Double); overload;

    procedure EndPoly(Flags: Cardinal = CAggPathFlagsClose);
    procedure ClosePolygon(Flags: Cardinal = CAggPathFlagsNone);

    procedure AddPoly(Vertices: PPointDouble; Num: Cardinal; SolidPath: Boolean = False; EndFlags: Cardinal = CAggPathFlagsNone);
    procedure AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0; SolidPath: Boolean = True);

    function StartNewPath: Cardinal;

    procedure CopyFrom(ps: TAggPathStorage);

    function SetVertex(index: Cardinal; X, Y: PDouble): Cardinal;
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    // Arrange the orientation of a polygon, all polygons in a path,
    // or in all paths. After calling arrangeOrientations() or
    // ArrangeOrientationsAllPaths(), all the polygons will have
    // the same orientation, i.e. CAggPathFlagsCw or CAggPathFlagsCcw
    function ArrangePolygonOrientation(Start, Orientation: Cardinal): Cardinal;
    function ArrangeOrientations(Start, Orientation: Cardinal): Cardinal;
    procedure ArrangeOrientationsAllPaths(Orientation: Cardinal);

    // Flip all the vertices horizontally or vertically
    procedure FlipX(x1, x2: Double);
    procedure FlipY(y1, y2: Double);

    // This function adds a vertex with its flags directly. Since there's no
    // checking for errors, keeping proper path integrity is the responsibility
    // of the caller. It can be said the function is "not very public".
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;

    // Allows you to modify vertex coordinates. The caller must know
    // the index of the vertex.
    procedure ModifyVertex(index: Cardinal; X, Y: Double);

    // Path Affine Transformations
    procedure Transform(Trans: TAggTransAffine; PathID: Cardinal = 0);
    procedure TransformAllPaths(Trans: TAggTransAffine);

    // from 2.4
    procedure ConcatPath(Vs: TAggVertexSource; PathID: Cardinal = 0);

    procedure InvertPolygon(Start, stop: Cardinal); overload;
    procedure InvertPolygon(Start: Cardinal); overload;

    property TotalVertices: Cardinal read FTotalVertices;
    property Command[index: Cardinal]: Cardinal read GetCommand write SetCommand;
  end;

implementation


{ TAggPathStorageVertexSource }

constructor TAggPathStorageVertexSource.Create;
begin
  FPath := nil;

  FVertexIndex := 0;
end;

constructor TAggPathStorageVertexSource.Create(p: TAggPathStorage);
begin
  FPath := p;

  FVertexIndex := 0;
end;

procedure TAggPathStorageVertexSource.Rewind(PathID: Cardinal);
begin
  FVertexIndex := PathID;
end;

function TAggPathStorageVertexSource.Vertex(X, Y: PDouble): Cardinal;
begin
  if FVertexIndex < FPath.TotalVertices then
    begin
      Result := FPath.SetVertex(FVertexIndex, X, Y);

      Inc(FVertexIndex);
    end
  else
      Result := CAggPathCmdStop;
end;

{ TAggPathStorage }

constructor TAggPathStorage.Create;
begin
  FTotalVertices := 0;
  FTotalBlocks := 0;
  FMaxBlocks := 0;

  FCoordBlocks := nil;
  FCmdBlocks := nil;

  FIterator := 0;
end;

constructor TAggPathStorage.Create(ps: TAggPathStorage);
begin
  FTotalVertices := 0;
  FTotalBlocks := 0;
  FMaxBlocks := 0;

  FCoordBlocks := nil;
  FCmdBlocks := nil;

  FIterator := 0;

  CopyFrom(ps);
end;

destructor TAggPathStorage.Destroy;
var
  CoordBulk: PPDouble;
  DataSize: Cardinal;
begin
  if FTotalBlocks <> 0 then
    begin
      CoordBulk := PPDouble(PtrComp(FCoordBlocks) + (FTotalBlocks - 1) *
        SizeOf(PDouble));

      while FTotalBlocks > 0 do
        begin
          DataSize := (CAggBlockSize * 2 +
            CAggBlockSize div (SizeOf(Double) div SizeOf(Int8u))) * SizeOf(Double);
          AggFreeMem(TPointer32(CoordBulk^).PTR, DataSize);

          Dec(PtrComp(CoordBulk), SizeOf(PDouble));
          Dec(FTotalBlocks);
        end;

      AggFreeMem(Pointer(FCoordBlocks), FMaxBlocks * 2 * SizeOf(PDouble));
    end;

  inherited;
end;

procedure TAggPathStorage.RemoveAll;
begin
  FTotalVertices := 0;
  FIterator := 0;
end;

function TAggPathStorage.LastVertex;
begin
  if FTotalVertices <> 0 then
      Result := SetVertex(FTotalVertices - 1, X, Y)
  else
      Result := CAggPathCmdStop;
end;

function TAggPathStorage.PrevVertex;
begin
  if FTotalVertices > 1 then
      Result := SetVertex(FTotalVertices - 2, X, Y)
  else
      Result := CAggPathCmdStop;
end;

function TAggPathStorage.LastX;
var
  index: Cardinal;
begin
  if FTotalVertices <> 0 then
    begin
      index := FTotalVertices - 1;

      Result := PDouble
        (PtrComp(PPointer32(PtrComp(FCoordBlocks) + (index shr CAggBlockShift) *
        SizeOf(PDouble)).PTR) + ((index and CAggBlockMask) shl 1) *
        SizeOf(Double))^;
    end
  else
      Result := 0.0;
end;

function TAggPathStorage.LastY;
var
  index: Cardinal;
begin
  if FTotalVertices <> 0 then
    begin
      index := FTotalVertices - 1;

      Result := PDouble
        (PtrComp(PPointer32(PtrComp(FCoordBlocks) + (index shr CAggBlockShift) *
        SizeOf(PDouble)).PTR) + (((index and CAggBlockMask) shl 1) + 1) *
        SizeOf(Double))^;
    end
  else
      Result := 0.0;
end;

procedure TAggPathStorage.RelativeToAbsolute;
var
  x2, y2: Double;
begin
  if FTotalVertices <> 0 then
    if IsVertex(SetVertex(FTotalVertices - 1, @x2, @y2)) then
      begin
        X^ := X^ + x2;
        Y^ := Y^ + y2;
      end;
end;

procedure TAggPathStorage.MoveTo;
begin
  AddVertex(X, Y, CAggPathCmdMoveTo);
end;

procedure TAggPathStorage.MoveRelative;
begin
  RelativeToAbsolute(@dx, @dy);
  AddVertex(dx, dy, CAggPathCmdMoveTo);
end;

procedure TAggPathStorage.LineTo;
begin
  AddVertex(X, Y, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.LineRelative;
begin
  RelativeToAbsolute(@dx, @dy);
  AddVertex(dx, dy, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.HorizontalLineTo;
begin
  AddVertex(X, LastY, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.HorizontalLineRelative;
var
  dy: Double;
begin
  dy := 0;

  RelativeToAbsolute(@dx, @dy);
  AddVertex(dx, dy, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.VerticalLineTo;
begin
  AddVertex(LastX, Y, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.VerticalLineRelative;
var
  dx: Double;
begin
  dx := 0;

  RelativeToAbsolute(@dx, @dy);
  AddVertex(dx, dy, CAggPathCmdLineTo);
end;

procedure TAggPathStorage.ArcTo(RX, RY, angle: Double; LargeArcFlag,
  SweepFlag: Boolean; X, Y: Double);
var
  A: TAggBezierArcSvg;
  x0, y0, Epsilon: Double;
begin
  A := nil;

  if (FTotalVertices <> 0) and IsVertex(Command[FTotalVertices - 1]) then
    begin
      Epsilon := 1E-30;

      x0 := 0.0;
      y0 := 0.0;

      LastVertex(@x0, @y0);

      RX := Abs(RX);
      RY := Abs(RY);

      // Ensure radii are valid
      if (RX < Epsilon) or (RY < Epsilon) then
        begin
          LineTo(X, Y);
          Exit;
        end;

      // If the endpoints (x, y) and (x0, y0) are identical, then this
      // is equivalent to omitting the elliptical arc segment entirely.
      if CalculateDistance(x0, y0, X, Y) < Epsilon then
          Exit;

      A := TAggBezierArcSvg.Create(x0, y0, RX, RY, angle, LargeArcFlag,
        SweepFlag, X, Y);

      if A.RadiiOK then
          AddPath(A, 0, True)
      else
          LineTo(X, Y);
    end
  else
      MoveTo(X, Y);

  if A <> nil then
      A.Free;
end;

procedure TAggPathStorage.ArcRelative(RX, RY, angle: Double;
  LargeArcFlag, SweepFlag: Boolean; dx, dy: Double);
begin
  RelativeToAbsolute(@dx, @dy);
  ArcTo(RX, RY, angle, LargeArcFlag, SweepFlag, dx, dy);
end;

procedure TAggPathStorage.Curve3(ControlX, ControlY, tox, ToY: Double);
begin
  AddVertex(ControlX, ControlY, CAggPathCmdCurve3);
  AddVertex(tox, ToY, CAggPathCmdCurve3);
end;

procedure TAggPathStorage.Curve3Relative(DeltaControlX, DeltaControlY,
  DeltaToX, DeltaToY: Double);
begin
  RelativeToAbsolute(@DeltaControlX, @DeltaControlY);
  RelativeToAbsolute(@DeltaToX, @DeltaToY);
  AddVertex(DeltaControlX, DeltaControlY, CAggPathCmdCurve3);
  AddVertex(DeltaToX, DeltaToY, CAggPathCmdCurve3);
end;

procedure TAggPathStorage.Curve3(tox, ToY: Double);
var
  Cmd: Cardinal;
  x0, y0, ControlX, ControlY: Double;
begin
  if IsVertex(LastVertex(@x0, @y0)) then
    begin
      Cmd := PrevVertex(@ControlX, @ControlY);

      if IsCurve(Cmd) then
        begin
          ControlX := x0 + x0 - ControlX;
          ControlY := y0 + y0 - ControlY;
        end
      else
        begin
          ControlX := x0;
          ControlY := y0;
        end;

      Curve3(ControlX, ControlY, tox, ToY);
    end;
end;

procedure TAggPathStorage.Curve3Relative(DeltaToX, DeltaToY: Double);
begin
  RelativeToAbsolute(@DeltaToX, @DeltaToY);
  Curve3(DeltaToX, DeltaToY);
end;

procedure TAggPathStorage.Curve4(Control1X, Control1Y, Control2X, Control2Y,
  tox, ToY: Double);
begin
  AddVertex(Control1X, Control1Y, CAggPathCmdCurve4);
  AddVertex(Control2X, Control2Y, CAggPathCmdCurve4);
  AddVertex(tox, ToY, CAggPathCmdCurve4);
end;

procedure TAggPathStorage.Curve4Relative(DeltaControl1X, DeltaControl1Y,
  DeltaControl2X, DeltaControl2Y, DeltaToX, DeltaToY: Double);
begin
  RelativeToAbsolute(@DeltaControl1X, @DeltaControl1Y);
  RelativeToAbsolute(@DeltaControl2X, @DeltaControl2Y);
  RelativeToAbsolute(@DeltaToX, @DeltaToY);
  AddVertex(DeltaControl1X, DeltaControl1Y, CAggPathCmdCurve4);
  AddVertex(DeltaControl2X, DeltaControl2Y, CAggPathCmdCurve4);
  AddVertex(DeltaToX, DeltaToY, CAggPathCmdCurve4);
end;

procedure TAggPathStorage.Curve4(Control2X, Control2Y, tox, ToY: Double);
var
  Cmd: Cardinal;
  x0, y0, Control1X, Control1Y: Double;
begin
  if IsVertex(LastVertex(@x0, @y0)) then
    begin
      Cmd := PrevVertex(@Control1X, @Control1Y);

      if IsCurve(Cmd) then
        begin
          Control1X := x0 + x0 - Control1X;
          Control1Y := y0 + y0 - Control1Y;
        end
      else
        begin
          Control1X := x0;
          Control1Y := y0;
        end;

      Curve4(Control1X, Control1Y, Control2X, Control2Y, tox, ToY);
    end;
end;

procedure TAggPathStorage.Curve4Relative(DeltaControl2X, DeltaControl2Y, DeltaToX, DeltaToY: Double);
begin
  RelativeToAbsolute(@DeltaControl2X, @DeltaControl2Y);
  RelativeToAbsolute(@DeltaToX, @DeltaToY);

  Curve4(DeltaControl2X, DeltaControl2Y, DeltaToX, DeltaToY);
end;

procedure TAggPathStorage.EndPoly(Flags: Cardinal = CAggPathFlagsClose);
begin
  if FTotalVertices <> 0 then
    if IsVertex(Command[FTotalVertices - 1]) then
        AddVertex(0.0, 0.0, CAggPathCmdEndPoly or Flags);
end;

procedure TAggPathStorage.ClosePolygon(Flags: Cardinal = CAggPathFlagsNone);
begin
  EndPoly(CAggPathFlagsClose or Flags);
end;

procedure TAggPathStorage.AddPoly(Vertices: PPointDouble; Num: Cardinal;
  SolidPath: Boolean = False; EndFlags: Cardinal = CAggPathFlagsNone);
begin
  if Num <> 0 then
    begin
      if not SolidPath then
        begin
          MoveTo(Vertices.X, Vertices.Y);

          Inc(PtrComp(Vertices), 2 * SizeOf(Double));
          Dec(Num);
        end;

      while Num > 0 do
        begin
          LineTo(Vertices.X, Vertices.Y);

          Inc(PtrComp(Vertices), 2 * SizeOf(Double));
          Dec(Num);
        end;

      if EndFlags <> 0 then
          EndPoly(EndFlags);
    end;
end;

procedure TAggPathStorage.AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0;
  SolidPath: Boolean = True);
var
  Cmd: Cardinal;
  X, Y: Double;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
    begin
      if IsMoveTo(Cmd) and SolidPath and (FTotalVertices <> 0) then
          Cmd := CAggPathCmdLineTo;

      AddVertex(X, Y, Cmd);

      Cmd := Vs.Vertex(@X, @Y);
    end;
end;

function TAggPathStorage.StartNewPath: Cardinal;
begin
  if FTotalVertices <> 0 then
    if not IsStop(Command[FTotalVertices - 1]) then
        AddVertex(0.0, 0.0, CAggPathCmdStop);

  Result := FTotalVertices;
end;

procedure TAggPathStorage.CopyFrom(ps: TAggPathStorage);
var
  i, Cmd: Cardinal;
  X, Y: Double;
begin
  RemoveAll;

  for i := 0 to ps.TotalVertices - 1 do
    begin
      Cmd := ps.SetVertex(i, @X, @Y);

      AddVertex(X, Y, Cmd);
    end;
end;

function TAggPathStorage.SetVertex(index: Cardinal; X, Y: PDouble): Cardinal;
var
  nb: Cardinal;
  PV: PDouble;
begin
  nb := index shr CAggBlockShift;

  PV := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) + nb *
    SizeOf(PDouble)).PTR) + ((index and CAggBlockMask) shl 1) * SizeOf(Double));

  X^ := PV^;
  Inc(PtrComp(PV), SizeOf(Double));
  Y^ := PV^;

  Result := PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + nb *
    SizeOf(PInt8u)).PTR) + (index and CAggBlockMask) * SizeOf(Int8u))^;
end;

function TAggPathStorage.GetCommand(index: Cardinal): Cardinal;
begin
  Result := PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) +
    (index shr CAggBlockShift) * SizeOf(PInt8u)).PTR) + (index and CAggBlockMask) *
    SizeOf(Int8u))^;
end;

procedure TAggPathStorage.Rewind(PathID: Cardinal);
begin
  FIterator := PathID;
end;

function TAggPathStorage.Vertex(X, Y: PDouble): Cardinal;
begin
  if FIterator >= FTotalVertices then
      Result := CAggPathCmdStop
  else
    begin
      Result := SetVertex(FIterator, X, Y);

      Inc(FIterator);
    end;
end;

function TAggPathStorage.ArrangePolygonOrientation(Start, Orientation: Cardinal)
  : Cardinal;
var
  Cmd, stop: Cardinal;
begin
  if Orientation = CAggPathFlagsNone then
    begin
      Result := Start;

      Exit;
    end;

  // Skip all non-vertices at the beginning
  while (Start < FTotalVertices) and not IsVertex(Command[Start]) do
      Inc(Start);

  // Skip all insignificant MoveTo
  while (Start + 1 < FTotalVertices) and IsMoveTo(Command[Start]) and
    IsMoveTo(Command[Start + 1]) do
      Inc(Start);

  // Find the last vertex
  stop := Start + 1;

  while (stop < FTotalVertices) and not IsNextPoly(Command[stop]) do
      Inc(stop);

  if stop - Start > 2 then
    if PerceivePolygonOrientation(Start, stop) <> Orientation then
      begin
        // Invert polygon, set orientation flag, and skip all end_poly
        InvertPolygon(Start, stop);

        Cmd := Command[stop];

        while (stop < FTotalVertices) and IsEndPoly(Cmd) do
          begin
            Command[stop] := SetOrientation(Cmd, Orientation);

            Inc(stop);

            Cmd := Command[stop];
          end;
      end;

  Result := stop;
end;

function TAggPathStorage.ArrangeOrientations(Start, Orientation: Cardinal):
  Cardinal;
begin
  if Orientation <> CAggPathFlagsNone then
    while Start < FTotalVertices do
      begin
        Start := ArrangePolygonOrientation(Start, Orientation);

        if IsStop(Command[Start]) then
          begin
            Inc(Start);

            Break;
          end;
      end;

  Result := Start;
end;

procedure TAggPathStorage.ArrangeOrientationsAllPaths(Orientation: Cardinal);
var
  Start: Cardinal;
begin
  if Orientation <> CAggPathFlagsNone then
    begin
      Start := 0;

      while Start < FTotalVertices do
          Start := ArrangeOrientations(Start, Orientation);
    end;
end;

procedure TAggPathStorage.FlipX(x1, x2: Double);
var
  i, Cmd: Cardinal;
  X, Y: Double;
begin
  if FTotalVertices > 0 then
    for i := 0 to FTotalVertices - 1 do
      begin
        Cmd := SetVertex(i, @X, @Y);

        if IsVertex(Cmd) then
            ModifyVertex(i, x2 - X + x1, Y);
      end;
end;

procedure TAggPathStorage.FlipY(y1, y2: Double);
var
  i, Cmd: Cardinal;
  X, Y: Double;
begin
  if FTotalVertices > 0 then
    for i := 0 to FTotalVertices - 1 do
      begin
        Cmd := SetVertex(i, @X, @Y);

        if IsVertex(Cmd) then
            ModifyVertex(i, X, y2 - Y + y1);
      end;
end;

procedure TAggPathStorage.AddVertex(X, Y: Double; Cmd: Cardinal);
var
  CoordPointer: PDouble;
  CmdPointer: PInt8u;
begin
  CoordPointer := nil;

  CmdPointer := StoragePtrs(@CoordPointer);

  CmdPointer^ := Int8u(Cmd);

  CoordPointer^ := X;
  Inc(PtrComp(CoordPointer), SizeOf(Double));
  CoordPointer^ := Y;

  Inc(FTotalVertices);
end;

procedure TAggPathStorage.ModifyVertex(index: Cardinal; X, Y: Double);
var
  PV: PDouble;
begin
  PV := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) +
    (index shr CAggBlockShift) * SizeOf(PDouble)).PTR) +
    ((index and CAggBlockMask) shl 1) * SizeOf(Double));

  PV^ := X;
  Inc(PtrComp(PV), SizeOf(Double));
  PV^ := Y;
end;

procedure TAggPathStorage.SetCommand(index, Cmd: Cardinal);
begin
  PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + (index shr CAggBlockShift) *
    SizeOf(PInt8u)).PTR) + (index and CAggBlockMask) * SizeOf(Int8u))^ :=
    Int8u(Cmd);
end;

procedure TAggPathStorage.Transform(Trans: TAggTransAffine;
  PathID: Cardinal = 0);
var
  X, Y: Double;
  Cmd: Cardinal;
begin
  while PathID < FTotalVertices do
    begin
      Cmd := SetVertex(PathID, @X, @Y);

      if IsStop(Cmd) then
          Break;

      if IsVertex(Cmd) then
        begin
          Trans.Transform(Trans, @X, @Y);
          ModifyVertex(PathID, X, Y);
        end;

      Inc(PathID);
    end;
end;

procedure TAggPathStorage.TransformAllPaths(Trans: TAggTransAffine);
var
  X, Y: Double;
  index: Cardinal;
begin
  index := 0;

  while index < FTotalVertices do
    begin
      if IsVertex(SetVertex(index, @X, @Y)) then
        begin
          Trans.Transform(Trans, @X, @Y);
          ModifyVertex(index, X, Y);
        end;

      Inc(index);
    end;
end;

procedure TAggPathStorage.AllocateBlock(nb: Cardinal);
var
  NewCoords: PPDouble;
  NewCmds: PPInt8u;
  NewSize: Cardinal;
  PTR: PPDouble;
begin
  if nb >= FMaxBlocks then
    begin
      AggGetMem(Pointer(NewCoords), 2 * (FMaxBlocks + CAggBlockPool) *
        SizeOf(PDouble));

      NewCmds := PPInt8u(PtrComp(NewCoords) + (FMaxBlocks + CAggBlockPool)
        * SizeOf(PDouble));

      if FCoordBlocks <> nil then
        begin
          Move(FCoordBlocks^, NewCoords^, FMaxBlocks * SizeOf(PDouble));
          Move(FCmdBlocks^, NewCmds^, FMaxBlocks * SizeOf(PInt8u));

          AggFreeMem(Pointer(FCoordBlocks), 2 * FMaxBlocks * SizeOf(PDouble));
        end;

      FCoordBlocks := NewCoords;
      FCmdBlocks := NewCmds;

      Inc(FMaxBlocks, CAggBlockPool);
    end;

  NewSize := (CAggBlockSize * 2 + CAggBlockSize div
    (SizeOf(Double) div SizeOf(Int8u))) * SizeOf(Double);
  PTR := FCoordBlocks;
  Inc(PTR, nb);
  AggGetMem(Pointer(PTR^), NewSize);

  PPointer32(PtrComp(FCmdBlocks) + nb * SizeOf(PInt8u)).PTR :=
    Pointer(PtrComp(PTR^) + 2 * CAggBlockSize * SizeOf(Double));

  Inc(FTotalBlocks);
end;

function TAggPathStorage.StoragePtrs(Xy_ptr: PPDouble): PInt8u;
var
  NumBlocks: Cardinal;
begin
  NumBlocks := FTotalVertices shr CAggBlockShift;

  if NumBlocks >= FTotalBlocks then
      AllocateBlock(NumBlocks);

  Xy_ptr^ := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) + NumBlocks *
    SizeOf(PDouble)).PTR) + ((FTotalVertices and CAggBlockMask) shl 1) *
    SizeOf(Double));

  Result := PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + NumBlocks *
    SizeOf(PInt8u)).PTR) + (FTotalVertices and CAggBlockMask) *
    SizeOf(Int8u));
end;

function TAggPathStorage.PerceivePolygonOrientation;
var
  np, i: Cardinal;
  Area, x1, y1, x2, y2: Double;
begin
  // Calculate signed area (double area to be exact)
  np := stop - Start;
  Area := 0.0;

  if np > 0 then
    for i := 0 to np - 1 do
      begin
        SetVertex(Start + i, @x1, @y1);
        SetVertex(Start + (i + 1) mod np, @x2, @y2);

        Area := Area + (x1 * y2 - y1 * x2);
      end;

  if Area < 0.0 then
      Result := CAggPathFlagsCw
  else
      Result := CAggPathFlagsCcw;
end;

procedure TAggPathStorage.InvertPolygon(Start, stop: Cardinal);
var
  i, TmpCmd, StartNb, StopNb: Cardinal;
  StartPointer, StopPointer: PDouble;
  TmpXY: Double;
begin
  TmpCmd := Command[Start];

  Dec(stop); // Make "end" inclusive

  // Shift all commands to one position
  i := Start;

  while i < stop do
    begin
      Command[i] := Command[i + 1];

      Inc(i);
    end;

  // Assign starting command to the ending command
  Command[stop] := TmpCmd;

  // Reverse the polygon
  while stop > Start do
    begin
      StartNb := Start shr CAggBlockShift;
      StopNb := stop shr CAggBlockShift;

      StartPointer := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) + StartNb *
        SizeOf(PDouble)).PTR) + ((Start and CAggBlockMask) shl 1) *
        SizeOf(Double));

      StopPointer := PDouble(PtrComp(PPointer32(PtrComp(FCoordBlocks) + StopNb *
        SizeOf(PDouble)).PTR) + ((stop and CAggBlockMask) shl 1) *
        SizeOf(Double));

      TmpXY := StartPointer^;
      StartPointer^ := StopPointer^;
      Inc(PtrComp(StartPointer), SizeOf(Double));
      StopPointer^ := TmpXY;
      Inc(PtrComp(StopPointer), SizeOf(Double));

      TmpXY := StartPointer^;
      StartPointer^ := StopPointer^;
      StopPointer^ := TmpXY;

      TmpCmd := PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + StartNb *
        SizeOf(PInt8u)).PTR) + (Start and CAggBlockMask) * SizeOf(Int8u))^;

      PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + StartNb *
        SizeOf(PInt8u)).PTR) + (Start and CAggBlockMask) * SizeOf(Int8u))^ :=
        PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + StopNb *
        SizeOf(PInt8u)).PTR) + (stop and CAggBlockMask) * SizeOf(Int8u))^;

      PInt8u(PtrComp(PPointer32(PtrComp(FCmdBlocks) + StopNb * SizeOf(PInt8u)
        ).PTR) + (stop and CAggBlockMask) * SizeOf(Int8u))^ := Int8u(TmpCmd);

      Inc(Start);
      Dec(stop);
    end;
end;

procedure TAggPathStorage.InvertPolygon(Start: Cardinal);
var
  stop: Cardinal;
begin
  // Skip all non-vertices at the beginning
  while (Start < FTotalVertices) and not IsVertex(Command[Start]) do
      Inc(Start);

  // Skip all insignificant MoveTo
  while (Start + 1 < FTotalVertices) and IsMoveTo(Command[Start]) and
    IsMoveTo(Command[Start + 1]) do
      Inc(Start);

  // Find the last vertex
  stop := Start + 1;

  while (stop < FTotalVertices) and not IsNextPoly(Command[stop]) do
      Inc(stop);

  InvertPolygon(Start, stop);
end;

procedure TAggPathStorage.ConcatPath(Vs: TAggVertexSource;
  PathID: Cardinal = 0);
var
  X, Y: Double;
  Cmd: Cardinal;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
    begin
      AddVertex(X, Y, Cmd);

      Cmd := Vs.Vertex(@X, @Y);
    end;
end;

end. 
 
