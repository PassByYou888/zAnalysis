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
unit AggRasterizerScanLineAA;
(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  // Class TAggOutlineAA - implementation.                                      //
  //                                                                            //
  // Initially the rendering algorithm was designed by David Turner and the     //
  // other authors of the FreeType library - see the above notice. I nearly     //
  // created a similar Renderer, but still I was far from David's work.         //
  // I completely redesigned the original code and adapted it for Anti-Grain    //
  // ideas. Two functions - RenderLine and RenderHorizontalLine are the core    //
  // of the algorithm - they calculate the exact coverage of each pixel cell    //
  // of the polygon. I left these functions almost as is, because there's       //
  // no way to improve the perfection - hats off to David and his group!        //
  //                                                                            //
  // All other code is very different from the original.                        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggArray,
  AggScanline,
  AggRasterizerScanLine,
  AggVertexSource,
  AggGammaFunctions,
  AggClipLiangBarsky;

const
  CAggAntiAliasingShift = 8;
  CAggAntiAliasingNum   = 1 shl CAggAntiAliasingShift;
  CAggAntiAliasingMask  = CAggAntiAliasingNum - 1;
  CAggAntiAliasing2Num  = CAggAntiAliasingNum * 2;
  CAggAntiAliasing2Mask = CAggAntiAliasing2Num - 1;

  CAggCellBlockShift = 12;
  CAggCellBlockSize  = 1 shl CAggCellBlockShift;
  CAggCellBlockMask  = CAggCellBlockSize - 1;
  CAggCellBlockPool  = 256;
  CAggCellBlockLimit = 1024;

  // These constants determine the subpixel accuracy, to be more precise,
  // the number of bits of the fractional part of the coordinates.
  // The possible coordinate capacity in bits can be calculated by formula:
  // SizeOf(Integer) * 8 - CAggPolyBaseShift * 2, i.e, for 32-bit integers and
  // 8-bits fractional part the capacity is 16 bits or [-32768...32767].
  CAggPolyBaseShift = 8;                       // ----CAggPolyBaseShift
  CAggPolyBaseSize  = 1 shl CAggPolyBaseShift; // ----CAggPolyBaseSize
  CAggPolyBaseMask  = CAggPolyBaseSize - 1;    // ----CAggPolyBaseMask

type
  // A pixel cell. There're no constructors defined and it was done
  // intentionally in order to avoid extra overhead when allocating an
  // array of cells.

  PPAggCellAA = ^PAggCellAA;
  PAggCellAA  = ^TAggCellAA;

  TAggCellAA = packed record
    X, Y, Cover, Area: Integer;
  end;

  // An internal class that implements the main rasterization algorithm.
  // Used in the Rasterizer. Should not be used direcly.

  PAggSortedY = ^TAggSortedY;

  TAggSortedY = packed record
    Start, Num: Cardinal;
  end;

  TAggOutlineAA = class
  private
    FNumBlocks, FMaxBlocks, FCurBlock, FNumCells: Cardinal;

    FCur, FMin, FMax: TPointInteger;

    FSorted: Boolean;

    FCells: PPAggCellAA;
    FCurCellPointer: PAggCellAA;
    FCurCell: TAggCellAA;

    FSortedCells: TAggPodArray;
    FSortedY: TAggPodArray;
  public
    constructor Create;
    destructor Destroy; override;

    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);

    procedure Reset;

    procedure AddCurrentCell;
    procedure SetCurrentCell(X, Y: Integer);

    procedure SortCells;
    function ScanLineNumCells(Y: Cardinal): Cardinal;
    function ScanLineCells(Y: Cardinal): PPAggCellAA;

    procedure RenderLine(x1, y1, x2, y2: Integer);
    procedure RenderHorizontalLine(EY, x1, y1, x2, y2: Integer);

    procedure AllocateBlock;

    property TotalCells: Cardinal read FNumCells;
    property Sorted: Boolean read FSorted;
    property MinX: Integer read FMin.X;
    property MinY: Integer read FMin.Y;
    property MaxX: Integer read FMax.X;
    property MaxY: Integer read FMax.Y;
  end;

  TAggScanLineHitTest = class(TAggCustomScanLine)
  private
    fx: Integer;
    FHit: Boolean;
  protected
    function GetNumSpans: Cardinal; override;
  public
    constructor Create(X: Integer);

    procedure ResetSpans; override;

    procedure Finalize(Y: Integer); override;
    procedure AddCell(X: Integer; Cover: Cardinal); override;
    procedure AddSpan(X: Integer; Len, Cover: Cardinal); override;

    property Hit: Boolean read FHit;
  end;

  // Polygon Rasterizer that is used to render filled polygons with
  // High-quality Anti-Aliasing. Internally, by default, the class uses
  // integer coordinates in format 24.8, i.e. 24 bits for integer part
  // and 8 bits for fractional - see CAggPolyBaseShift. This class can be
  // used in the following  way:
  //
  // 1. SetFillingRule(TAggFillingRule ft) - optional.
  //
  // 2. Gamma() - optional.
  //
  // 3. reset()
  //
  // 4. MoveTo(x, y) / LineTo(x, y) - make the polygon. One can create
  // more than one contour, but each contour must consist of at least 3
  // vertices, i.e. MoveTo(x1, y1); LineTo(x2, y2); LineTo(x3, y3);
  // is the absolute minimum of vertices that define a triangle.
  // The algorithm does not check either the number of vertices nor
  // coincidence of their coordinates, but in the worst case it just
  // won't draw anything.
  // The orger of the vertices (clockwise or counterclockwise)
  // is important when using the non-zero filling rule (frNonZero).
  // In this case the vertex order of all the contours must be the same
  // if you want your intersecting polygons to be without "holes".
  // You actually can use different vertices order. If the contours do not
  // intersect each other the order is not important anyway. If they do,
  // contours with the same vertex order will be rendered without "holes"
  // while the intersecting contours with different orders will have "holes".
  //
  // SetFillingRule() and Gamma() can be called anytime before "sweeping".
  // ------------------------------------------------------------------------
  // TAggFillingRule = (frNonZero ,frEvenOdd );

  TInitialStatus = (siStatusInitial, siStatusLineTo, siStatusClosed);

  TAggRasterizerScanLineAA = class(TAggRasterizerScanLine)
  private
    FOutline: TAggOutlineAA;
    FGamma: array [0 .. CAggAntiAliasingNum - 1] of Integer;

    FFillingRule: TAggFillingRule;
    FClippedStart: TPointInteger;
    FStart, FPrev: TPointInteger;

    FPrevFlags: Cardinal;
    FStatus: TInitialStatus;

    FClipBox: TRectInteger;
    FClipping: Boolean;

    FCurY, FXScale: Integer;

    FAutoClose: Boolean;

    procedure ClosePolygon;
    procedure ClosePolygonNoClip;
  protected
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;

    function GetFillingRule: TAggFillingRule; override;
    procedure SetFillingRule(Value: TAggFillingRule); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; override;
    procedure AutoClose(flag: Boolean);
    procedure SetClipBox(x1, y1, x2, y2: Double); override;
    procedure SetClipBox(Rect: TRectDouble); override;

    procedure Gamma(AGammaFunction: TAggCustomVertexSource); override;
    function ApplyGamma(Cover: Cardinal): Cardinal;

    procedure MoveToNoClip(X, Y: Integer); overload;
    procedure MoveToNoClip(Point: TPointInteger); overload;
    procedure LineToNoClip(X, Y: Integer); overload;
    procedure LineToNoClip(Point: TPointInteger); overload;

    procedure ClipSegment(X, Y: Integer); overload;
    procedure ClipSegment(Point: TPointInteger); overload;

    procedure MoveToDouble(X, Y: Double); overload;
    procedure MoveToDouble(Point: TPointDouble); overload;
    procedure LineToDouble(X, Y: Double); overload;
    procedure LineToDouble(Point: TPointDouble); overload;

    procedure MoveTo(X, Y: Integer); overload;
    procedure MoveTo(Point: TPointInteger); overload;
    procedure LineTo(X, Y: Integer); overload;
    procedure LineTo(Point: TPointInteger); overload;

    procedure Sort; override;
    function RewindScanLines: Boolean; override;
    function SweepScanLine(SL: TAggCustomScanLine): Boolean; override;

    function NavigateScanLine(Y: Integer): Boolean;

    function HitTest(TX, TY: Integer): Boolean; override;

    function CalculateAlpha(Area: Integer): Cardinal;

    procedure AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0); override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;
  end;

function PolyCoord(Value: Double): Integer; overload;
function PolyCoord(Value: TPointDouble): TPointInteger; overload;
function PolyCoord(Value: TRectDouble): TRectInteger; overload;

implementation


function PolyCoord(Value: Double): Integer;
begin
  Result := Trunc(Value * CAggPolyBaseSize);
end;

function PolyCoord(Value: TPointDouble): TPointInteger;
begin
  Result.X := Trunc(Value.X * CAggPolyBaseSize);
  Result.Y := Trunc(Value.Y * CAggPolyBaseSize);
end;

function PolyCoord(Value: TRectDouble): TRectInteger; overload;
begin
  Result.x1 := Trunc(Value.x1 * CAggPolyBaseSize);
  Result.y1 := Trunc(Value.y1 * CAggPolyBaseSize);
  Result.x2 := Trunc(Value.x2 * CAggPolyBaseSize);
  Result.y2 := Trunc(Value.y2 * CAggPolyBaseSize);
end;

{ TAggOutlineAA }

constructor TAggOutlineAA.Create;
begin
  FSortedCells := TAggPodArray.Create(SizeOf(PAggCellAA));
  FSortedY := TAggPodArray.Create(SizeOf(TAggSortedY));

  FNumBlocks := 0;
  FMaxBlocks := 0;
  FCurBlock := 0;
  FNumCells := 0;

  FCur.X := 0;
  FCur.Y := 0;
  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;

  FSorted := False;

  FCells := nil;
  FCurCellPointer := nil;

  with FCurCell do
    begin
      X := $7FFF;
      Y := $7FFF;

      Cover := 0;
      Area := 0;
    end;
end;

destructor TAggOutlineAA.Destroy;
begin
  FSortedCells.Free;
  FSortedY.Free;

  if FNumBlocks > 0 then
    begin
      repeat
        Dec(FNumBlocks);

        AggFreeMem(Pointer(Pointer(PtrComp(FCells) + FNumBlocks *
          SizeOf(PAggCellAA))^), CAggCellBlockSize * SizeOf(TAggCellAA));
      until FNumBlocks = 0;

      AggFreeMem(Pointer(FCells), SizeOf(PAggCellAA) * FMaxBlocks);
    end;

  inherited;
end;

procedure TAggOutlineAA.MoveTo(X, Y: Integer);
begin
  if FSorted then
      Reset;

  // SetCurrentCell(x shr CAggPolyBaseShift ,y shr CAggPolyBaseShift );
  SetCurrentCell(ShrInt32(X, CAggPolyBaseShift), ShrInt32(Y, CAggPolyBaseShift));

  FCur.X := X;
  FCur.Y := Y;
end;

procedure TAggOutlineAA.LineTo(X, Y: Integer);
begin
  RenderLine(FCur.X, FCur.Y, X, Y);

  FCur.X := X;
  FCur.Y := Y;

  FSorted := False;
end;

procedure TAggOutlineAA.Reset;
begin
  FNumCells := 0;
  FCurBlock := 0;

  FCurCell.X := $7FFF;
  FCurCell.Y := $7FFF;
  FCurCell.Cover := 0;
  FCurCell.Area := 0;

  FSorted := False;

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
end;

procedure TAggOutlineAA.AddCurrentCell;
begin
  if (FCurCell.Area or FCurCell.Cover) <> 0 then
    begin
      if (FNumCells and CAggCellBlockMask) = 0 then
        begin
          if FNumBlocks >= CAggCellBlockLimit then
              Exit;

          AllocateBlock;
        end;

      FCurCellPointer^ := FCurCell;

      Inc(PtrComp(FCurCellPointer), SizeOf(TAggCellAA));
      Inc(FNumCells);

      if FCurCell.X < FMin.X then
          FMin.X := FCurCell.X;

      if FCurCell.X > FMax.X then
          FMax.X := FCurCell.X;

      if FCurCell.Y < FMin.Y then
          FMin.Y := FCurCell.Y;

      if FCurCell.Y > FMax.Y then
          FMax.Y := FCurCell.Y;
    end;
end;

procedure TAggOutlineAA.SetCurrentCell(X, Y: Integer);
begin
  if (FCurCell.X <> X) or (FCurCell.Y <> Y) then
    begin
      AddCurrentCell;

      FCurCell.X := X;
      FCurCell.Y := Y;

      FCurCell.Cover := 0;
      FCurCell.Area := 0;
    end;
end;

procedure QuickSortCells(Start: PPAggCellAA; Num: Cardinal);
var
  Len, X: Integer;

  Stack: array [0 .. 79] of PPAggCellAA;
  Limit, Base: PPAggCellAA;
  i, J, Pivot: PPAggCellAA;
  Top: ^PPAggCellAA;
const
  CQSortThreshold = 9;
begin
  Limit := PPAggCellAA(PtrComp(Start) + Num * SizeOf(Pointer));
  Base := Start;
  Top := @Stack[0];

  repeat
    Len := (PtrComp(Limit) - PtrComp(Base)) div SizeOf(Pointer);

    if Len > CQSortThreshold then
      begin
        // we use base + len/2 as the pivot
        Pivot := PPAggCellAA(PtrComp(Base) + (Len div 2) * SizeOf(Pointer));

        SwapPointers(Base, Pivot);

        i := PPAggCellAA(PtrComp(Base) + SizeOf(Pointer));
        J := PPAggCellAA(PtrComp(Limit) - SizeOf(Pointer));

        // now ensure that *i <= *base <= *j
        if J^.X < i^.X then
            SwapPointers(J, i);

        if Base^.X < i^.X then
            SwapPointers(Base, i);

        if J^.X < Base^.X then
            SwapPointers(Base, J);

        repeat
          X := Base^.X;

          Inc(PtrComp(i), SizeOf(PAggCellAA));

          while i^.X < X do
              Inc(PtrComp(i), SizeOf(PAggCellAA));

          Dec(PtrComp(J), SizeOf(PAggCellAA));

          while X < J^.X do
              Dec(PtrComp(J), SizeOf(PAggCellAA));

          if PtrComp(i) > PtrComp(J) then
              Break;

          SwapPointers(i, J);
        until False;

        SwapPointers(Base, J);

        // now, push the largest sub-array
        if (PtrComp(J) - PtrComp(Base)) div SizeOf(Pointer) >
          (PtrComp(Limit) - PtrComp(i)) div SizeOf(Pointer)
        then
          begin
            Top^ := Base;

            Inc(PtrComp(Top), SizeOf(PPAggCellAA));

            Top^ := J;
            Base := i;
          end
        else
          begin
            Top^ := i;

            Inc(PtrComp(Top), SizeOf(PPAggCellAA));

            Top^ := Limit;
            Limit := J;
          end;

        Inc(PtrComp(Top), SizeOf(PPAggCellAA));
      end
    else
      begin
        // the sub-array is small, perform insertion sort
        J := Base;
        i := PPAggCellAA(PtrComp(J) + SizeOf(Pointer));

        while PtrComp(i) < PtrComp(Limit) do
          begin
            while PPAggCellAA(PtrComp(J) + SizeOf(Pointer))^^.X < J^.X do
              begin
                SwapPointers(PPAggCellAA(PtrComp(J) + SizeOf(Pointer)), J);
                if J = Base then
                    Break;

                Dec(J);
              end;

            J := i;

            Inc(PtrComp(i), SizeOf(PAggCellAA));
          end;

        if PtrComp(Top) > PtrComp(@Stack[0]) then
          begin
            Dec(PtrComp(Top), SizeOf(PPAggCellAA));

            Limit := Top^;

            Dec(PtrComp(Top), SizeOf(PPAggCellAA));

            Base := Top^;
          end
        else
            Break;
      end;

  until False;
end;

procedure TAggOutlineAA.SortCells;
var
  nb, i, v, Start: Cardinal;
  CurY, CurMinY: PAggSortedY;

  BlockPtr: PPAggCellAA;
  CellPtr: PAggCellAA;
begin
  // Perform sort only the first time
  if FSorted then
      Exit;

  AddCurrentCell;

  if FNumCells = 0 then
      Exit;

  // Allocate the array of cell pointers
  FSortedCells.Allocate(FNumCells, 16);

  // Allocate and zero the Y array
  FSortedY.Allocate(FMax.Y - FMin.Y + 1, 16);
  FSortedY.Zero;

  // Create the Y-histogram (count the numbers of cells for each Y)
  BlockPtr := FCells;

  nb := FNumCells shr CAggCellBlockShift;

  CurMinY := PAggSortedY(FSortedY.ArrayPointer);
  Dec(CurMinY, FMin.Y);

  while nb > 0 do
    begin
      Dec(nb);

      CellPtr := BlockPtr^;
      Inc(BlockPtr);
      i := CAggCellBlockSize;

      while i > 0 do
        begin
          Dec(i);
          CurY := CurMinY;
          Inc(CurY, CellPtr^.Y);
          Inc(CurY^.Start);
          Inc(CellPtr);
        end;
    end;

  CellPtr := BlockPtr^;

  Inc(BlockPtr);

  i := FNumCells and CAggCellBlockMask;

  while i > 0 do
    begin
      Dec(i);
      CurY := CurMinY;
      Inc(CurY, CellPtr^.Y);
      Inc(CurY^.Start);
      Inc(CellPtr);
    end;

  // Convert the Y-histogram into the array of starting indexes
  Start := 0;

  CurY := PAggSortedY(FSortedY.ArrayPointer);
  for i := 0 to FSortedY.Size - 1 do
    begin
      v := CurY^.Start;

      CurY^.Start := Start;
      Inc(CurY);

      Inc(Start, v);
    end;

  // Fill the cell pointer array sorted by Y
  BlockPtr := FCells;

  nb := FNumCells shr CAggCellBlockShift;

  while nb > 0 do
    begin
      Dec(nb);

      CellPtr := BlockPtr^;

      Inc(BlockPtr);

      i := CAggCellBlockSize;

      while i > 0 do
        begin
          Dec(i);

          CurY := CurMinY;
          Inc(CurY, CellPtr.Y);

          PPointer(PtrComp(FSortedCells.ArrayPointer) +
            Cardinal(CurY.Start + CurY.Num) * FSortedCells.EntrySize)^ := CellPtr;

          Inc(CurY.Num);
          Inc(CellPtr);
        end;
    end;

  CellPtr := BlockPtr^;
  Inc(BlockPtr);
  i := FNumCells and CAggCellBlockMask;

  while i > 0 do
    begin
      Dec(i);

      CurY := CurMinY;
      Inc(CurY, CellPtr.Y);

      PPointer(PtrComp(FSortedCells.ArrayPointer) +
        Cardinal(CurY.Start + CurY.Num) * FSortedCells.EntrySize)^ := CellPtr;

      Inc(CurY.Num);
      Inc(CellPtr);
    end;

  // Finally arrange the X-arrays
  CurY := PAggSortedY(FSortedY.ArrayPointer);
  for i := 0 to FSortedY.Size - 1 do
    begin
      if CurY.Num > 0 then
          QuickSortCells(PPAggCellAA(PtrComp(FSortedCells.ArrayPointer) + CurY.Start
          * FSortedCells.EntrySize), CurY.Num);
      Inc(CurY);
    end;

  FSorted := True;
end;

function TAggOutlineAA.ScanLineNumCells(Y: Cardinal): Cardinal;
begin
  Result := PAggSortedY(PtrComp(FSortedY.ArrayPointer) + Cardinal(Y - FMin.Y) *
    FSortedY.EntrySize).Num;
end;

function TAggOutlineAA.ScanLineCells(Y: Cardinal): PPAggCellAA;
begin
  Result := PPAggCellAA(PtrComp(FSortedCells.ArrayPointer) +
    PAggSortedY(PtrComp(FSortedY.ArrayPointer) + Cardinal(Y - FMin.Y) *
    FSortedY.EntrySize).Start * FSortedCells.EntrySize);
end;

procedure TAggOutlineAA.RenderLine(x1, y1, x2, y2: Integer);
var
  center, Delta: TPointInteger;
  p, EX, Ey1, Ey2, FY1, FY2, Rem, DeltaMod,
    FromX, tox, Lift, DeltaVal, First, Incr, TwoFX, Area: Integer;
const
  CDxLimit = 16384 shl CAggPolyBaseShift;
begin
  Delta.X := x2 - x1;

  if (Delta.X >= CDxLimit) or (Delta.X <= -CDxLimit) then
    begin
      center.X := (x1 + x2) shr 1;
      center.Y := (y1 + y2) shr 1;

      RenderLine(x1, y1, center.X, center.Y);
      RenderLine(center.X, center.Y, x2, y2);
    end;

  Delta.Y := y2 - y1;

  // ey1:=y1 shr CAggPolyBaseShift;
  // ey2:=y2 shr CAggPolyBaseShift;
  Ey1 := ShrInt32(y1, CAggPolyBaseShift);
  Ey2 := ShrInt32(y2, CAggPolyBaseShift);

  FY1 := y1 and CAggPolyBaseMask;
  FY2 := y2 and CAggPolyBaseMask;

  // everything is on a single HorizontalLine
  if Ey1 = Ey2 then
    begin
      RenderHorizontalLine(Ey1, x1, FY1, x2, FY2);
      Exit;
    end;

  // Vertical line - we have to calculate start and end cells,
  // and then - the common values of the area and coverage for
  // all cells of the line. We know exactly there's only one
  // cell, so, we don't have to call render_HorizontalLine().
  Incr := 1;

  if Delta.X = 0 then
    begin
      // Ex := x1 shr CAggPolyBaseShift;
      EX := ShrInt32(x1, CAggPolyBaseShift);

      TwoFX := (x1 - (EX shl CAggPolyBaseShift)) shl 1;
      First := CAggPolyBaseSize;

      if Delta.Y < 0 then
        begin
          First := 0;
          Incr := -1;
        end;

      FromX := x1;

      // RenderHorizontalLine(ey1 ,FromX ,fy1 ,FromX ,first );
      DeltaVal := First - FY1;

      Inc(FCurCell.Cover, DeltaVal);
      Inc(FCurCell.Area, TwoFX * DeltaVal);
      Inc(Ey1, Incr);

      SetCurrentCell(EX, Ey1);

      DeltaVal := First + First - CAggPolyBaseSize;
      Area := TwoFX * DeltaVal;

      while Ey1 <> Ey2 do
        begin
          // RenderHorizontalLine(ey1 ,FromX ,CAggPolyBaseSize - first ,FromX ,first );
          FCurCell.Cover := DeltaVal;
          FCurCell.Area := Area;

          Inc(Ey1, Incr);

          SetCurrentCell(EX, Ey1);
        end;

      // RenderHorizontalLine(ey1, FromX, CAggPolyBaseSize - first, FromX, fy2);
      DeltaVal := FY2 - CAggPolyBaseSize + First;

      Inc(FCurCell.Cover, DeltaVal);
      Inc(FCurCell.Area, TwoFX * DeltaVal);

      Exit;
    end;

  // ok, we have to render several HorizontalLines
  p := (CAggPolyBaseSize - FY1) * Delta.X;
  First := CAggPolyBaseSize;

  if Delta.Y < 0 then
    begin
      p := FY1 * Delta.X;
      First := 0;
      Incr := -1;
      Delta.Y := -Delta.Y;
    end;

  DeltaVal := p div Delta.Y;
  DeltaMod := p mod Delta.Y;

  if DeltaMod < 0 then
    begin
      Dec(DeltaVal);
      Inc(DeltaMod, Delta.Y);
    end;

  FromX := x1 + DeltaVal;

  RenderHorizontalLine(Ey1, x1, FY1, FromX, First);

  Inc(Ey1, Incr);

  // SetCurrentCell(FromX shr CAggPolyBaseShift ,ey1 );
  SetCurrentCell(ShrInt32(FromX, CAggPolyBaseShift), Ey1);

  if Ey1 <> Ey2 then
    begin
      p := CAggPolyBaseSize * Delta.X;
      Lift := p div Delta.Y;
      Rem := p mod Delta.Y;

      if Rem < 0 then
        begin
          Dec(Lift);
          Inc(Rem, Delta.Y);
        end;

      Dec(DeltaMod, Delta.Y);

      while Ey1 <> Ey2 do
        begin
          DeltaVal := Lift;

          Inc(DeltaMod, Rem);

          if DeltaMod >= 0 then
            begin
              Dec(DeltaMod, Delta.Y);
              Inc(DeltaVal);
            end;

          tox := FromX + DeltaVal;

          RenderHorizontalLine(Ey1, FromX, CAggPolyBaseSize - First, tox, First);

          FromX := tox;

          Inc(Ey1, Incr);

          // SetCurrentCell(FromX shr CAggPolyBaseShift ,ey1 );
          SetCurrentCell(ShrInt32(FromX, CAggPolyBaseShift), Ey1);
        end;
    end;

  RenderHorizontalLine(Ey1, FromX, CAggPolyBaseSize - First, x2, FY2);
end;

procedure TAggOutlineAA.RenderHorizontalLine(EY, x1, y1, x2, y2: Integer);
var
  p, deltax, Ex1, Ex2, FX1, FX2: Integer;
  Delta, First, Incr, Lift, DeltaMod, Rem: Integer;
begin
  Ex1 := ShrInt32(x1, CAggPolyBaseShift);
  Ex2 := ShrInt32(x2, CAggPolyBaseShift);

  // trivial case. Happens often
  if y1 = y2 then
    begin
      SetCurrentCell(Ex2, EY);

      Exit;
    end;

  FX1 := x1 and CAggPolyBaseMask;
  FX2 := x2 and CAggPolyBaseMask;

  // everything is located in a single cell.  That is easy!
  if Ex1 = Ex2 then
    begin
      Delta := y2 - y1;

      Inc(FCurCell.Cover, Delta);
      Inc(FCurCell.Area, (FX1 + FX2) * Delta);

      Exit;
    end;

  // ok, we'll have to render a run of adjacent cells on the same
  // HorizontalLine...
  p := (CAggPolyBaseSize - FX1) * (y2 - y1);
  First := CAggPolyBaseSize;
  Incr := 1;
  deltax := x2 - x1;

  if deltax < 0 then
    begin
      p := FX1 * (y2 - y1);
      First := 0;
      Incr := -1;
      deltax := -deltax;
    end;

  Delta := p div deltax;
  DeltaMod := p mod deltax;

  if DeltaMod < 0 then
    begin
      Dec(Delta);
      Inc(DeltaMod, deltax);
    end;

  Inc(FCurCell.Cover, Delta);
  Inc(FCurCell.Area, (FX1 + First) * Delta);

  Inc(Ex1, Incr);

  SetCurrentCell(Ex1, EY);

  Inc(y1, Delta);

  if Ex1 <> Ex2 then
    begin
      p := CAggPolyBaseSize * (y2 - y1 + Delta);
      Lift := p div deltax;
      Rem := p mod deltax;

      if Rem < 0 then
        begin
          Dec(Lift);
          Inc(Rem, deltax);
        end;

      Dec(DeltaMod, deltax);

      while Ex1 <> Ex2 do
        begin
          Delta := Lift;

          Inc(DeltaMod, Rem);

          if DeltaMod >= 0 then
            begin
              Dec(DeltaMod, deltax);
              Inc(Delta);
            end;

          Inc(FCurCell.Cover, Delta);
          Inc(FCurCell.Area, (CAggPolyBaseSize) * Delta);
          Inc(y1, Delta);
          Inc(Ex1, Incr);

          SetCurrentCell(Ex1, EY);
        end;
    end;

  Delta := y2 - y1;

  Inc(FCurCell.Cover, Delta);
  Inc(FCurCell.Area, (FX2 + CAggPolyBaseSize - First) * Delta);
end;

procedure TAggOutlineAA.AllocateBlock;
var
  NewCells: PPAggCellAA;
begin
  if FCurBlock >= FNumBlocks then
    begin
      if FNumBlocks >= FMaxBlocks then
        begin
          AggGetMem(Pointer(NewCells), SizeOf(PAggCellAA) *
            (FMaxBlocks + CAggCellBlockPool));

          if FCells <> nil then
            begin
              Move(FCells^, NewCells^, SizeOf(PAggCellAA) * FMaxBlocks);

              AggFreeMem(Pointer(FCells), SizeOf(PAggCellAA) * FMaxBlocks);
            end;

          FCells := NewCells;

          Inc(FMaxBlocks, CAggCellBlockPool);
        end;

      AggGetMem(Pointer(Pointer(PtrComp(FCells) + FNumBlocks *
        SizeOf(PAggCellAA))^), CAggCellBlockSize * SizeOf(TAggCellAA));

      Inc(FNumBlocks);
    end;

  FCurCellPointer := PPAggCellAA(PtrComp(FCells) + FCurBlock *
    SizeOf(PAggCellAA))^;

  Inc(FCurBlock);
end;

{ TAggScanLineHitTest }

constructor TAggScanLineHitTest.Create;
begin
  fx := X;
  FHit := False;
end;

procedure TAggScanLineHitTest.ResetSpans;
begin
end;

procedure TAggScanLineHitTest.Finalize(Y: Integer);
begin
end;

procedure TAggScanLineHitTest.AddCell(X: Integer; Cover: Cardinal);
begin
  if fx = X then
      FHit := True;
end;

procedure TAggScanLineHitTest.AddSpan(X: Integer; Len, Cover: Cardinal);
begin
  if (fx >= X) and (fx < X + Len) then
      FHit := True;
end;

function TAggScanLineHitTest.GetNumSpans: Cardinal;
begin
  Result := 1;
end;

{ TAggRasterizerScanLineAA }

constructor TAggRasterizerScanLineAA.Create;
var
  i: Integer;
begin
  FOutline := TAggOutlineAA.Create;

  FFillingRule := frNonZero;
  FAutoClose := True;

  FClippedStart := PointInteger(0);
  FStart := PointInteger(0);
  FPrev := PointInteger(0);

  FPrevFlags := 0;
  FStatus := siStatusInitial;
  FClipping := False;

  for i := 0 to CAggAntiAliasingNum - 1 do
      FGamma[i] := i;

  FXScale := 1;
end;

destructor TAggRasterizerScanLineAA.Destroy;
begin
  FOutline.Free;
  inherited;
end;

procedure TAggRasterizerScanLineAA.Reset;
begin
  FOutline.Reset;

  FStatus := siStatusInitial;
end;

procedure TAggRasterizerScanLineAA.SetClipBox(x1, y1, x2, y2: Double);
begin
  Reset;

  FClipBox := PolyCoord(RectDouble(x1, y1, x2, y2));
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineAA.SetClipBox(Rect: TRectDouble);
begin
  Reset;

  FClipBox := PolyCoord(Rect);
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineAA.SetFillingRule(Value: TAggFillingRule);
begin
  FFillingRule := Value;
end;

procedure TAggRasterizerScanLineAA.AutoClose(flag: Boolean);
begin
  FAutoClose := flag;
end;

procedure TAggRasterizerScanLineAA.Gamma(AGammaFunction: TAggCustomVertexSource);
var
  i: Integer;
begin
  for i := 0 to CAggAntiAliasingNum - 1 do
      FGamma[i] := Trunc(AGammaFunction.FuncOperatorGamma(
      i / CAggAntiAliasingMask) * CAggAntiAliasingMask + 0.5);
end;

function TAggRasterizerScanLineAA.ApplyGamma(Cover: Cardinal): Cardinal;
begin
  Result := FGamma[Cover];
end;

procedure TAggRasterizerScanLineAA.MoveToNoClip(X, Y: Integer);
begin
  if (FStatus = siStatusLineTo) and FAutoClose then
      ClosePolygonNoClip;

  FOutline.MoveTo(X * FXScale, Y);

  FClippedStart := PointInteger(X, Y);

  FStatus := siStatusLineTo;
end;

procedure TAggRasterizerScanLineAA.MoveToNoClip(Point: TPointInteger);
begin
  if (FStatus = siStatusLineTo) and FAutoClose then
      ClosePolygonNoClip;

  FOutline.MoveTo(Point.X * FXScale, Point.Y);

  FClippedStart := Point;

  FStatus := siStatusLineTo;
end;

procedure TAggRasterizerScanLineAA.LineToNoClip(X, Y: Integer);
begin
  if FStatus <> siStatusInitial then
    begin
      FOutline.LineTo(X * FXScale, Y);

      FStatus := siStatusLineTo;
    end;
end;

procedure TAggRasterizerScanLineAA.LineToNoClip(Point: TPointInteger);
begin
  if FStatus <> siStatusInitial then
    begin
      FOutline.LineTo(Point.X * FXScale, Point.Y);

      FStatus := siStatusLineTo;
    end;
end;

procedure TAggRasterizerScanLineAA.ClosePolygon;
begin
  if FClipping then
      ClipSegment(FStart);

  if FAutoClose then
      ClosePolygonNoClip;
end;

procedure TAggRasterizerScanLineAA.ClosePolygonNoClip;
begin
  if FStatus = siStatusLineTo then
    begin
      FOutline.LineTo(FClippedStart.X * FXScale, FClippedStart.Y);

      FStatus := siStatusClosed;
    end;
end;

procedure TAggRasterizerScanLineAA.ClipSegment(X, Y: Integer);
var
  Flags, n: Cardinal;

  center: array [0 .. 3] of TPointInteger;
  Pnt: PPointInteger;
begin
  Flags := ClippingFlagsInteger(X, Y, FClipBox);

  if FPrevFlags = Flags then
    if Flags = 0 then
      if FStatus = siStatusInitial then
          MoveToNoClip(X, Y)
      else
          LineToNoClip(X, Y)
    else
  else
    begin
      n := ClipLiangBarskyInteger(FPrev.X, FPrev.Y, X, Y, FClipBox, @center[0]);

      Pnt := @center[0];

      while n > 0 do
        begin
          if FStatus = siStatusInitial then
              MoveToNoClip(Pnt^)
          else
              LineToNoClip(Pnt^);

          Inc(Pnt);
          Dec(n);
        end;
    end;

  FPrevFlags := Flags;
  FPrev := PointInteger(X, Y);
end;

procedure TAggRasterizerScanLineAA.ClipSegment(Point: TPointInteger);
var
  Flags, n: Cardinal;

  center: array [0 .. 3] of TPointInteger;
  Pnt: PPointInteger;
begin
  Flags := ClippingFlagsInteger(Point.X, Point.Y, FClipBox);

  if FPrevFlags = Flags then
    if Flags = 0 then
      if FStatus = siStatusInitial then
          MoveToNoClip(Point)
      else
          LineToNoClip(Point)
    else
  else
    begin
      n := ClipLiangBarskyInteger(FPrev.X, FPrev.Y, Point.X, Point.Y, FClipBox,
        @center[0]);

      Pnt := @center[0].X;

      while n > 0 do
        begin
          if FStatus = siStatusInitial then
              MoveToNoClip(Pnt^)
          else
              LineToNoClip(Pnt^);

          Inc(Pnt);
          Dec(n);
        end;
    end;

  FPrevFlags := Flags;
  FPrev := Point;
end;

procedure TAggRasterizerScanLineAA.MoveToDouble(X, Y: Double);
begin
  MoveTo(PolyCoord(X), PolyCoord(Y));
end;

procedure TAggRasterizerScanLineAA.LineToDouble(X, Y: Double);
begin
  LineTo(PolyCoord(X), PolyCoord(Y));
end;

procedure TAggRasterizerScanLineAA.MoveToDouble(Point: TPointDouble);
begin
  MoveTo(PolyCoord(Point));
end;

procedure TAggRasterizerScanLineAA.LineToDouble(Point: TPointDouble);
begin
  LineTo(PolyCoord(Point));
end;

procedure TAggRasterizerScanLineAA.MoveTo(Point: TPointInteger);
begin
  if FClipping then
    begin
      if FOutline.Sorted then
          Reset;

      if (FStatus = siStatusLineTo) and FAutoClose then
          ClosePolygon;

      FPrev := Point;
      FStart := Point;
      FStatus := siStatusInitial;

      FPrevFlags := ClippingFlagsInteger(Point.X, Point.Y, FClipBox);

      if FPrevFlags = 0 then
          MoveToNoClip(Point);
    end
  else
      MoveToNoClip(Point);
end;

procedure TAggRasterizerScanLineAA.MoveTo(X, Y: Integer);
begin
  if FClipping then
    begin
      if FOutline.Sorted then
          Reset;

      if (FStatus = siStatusLineTo) and FAutoClose then
          ClosePolygon;

      FPrev := PointInteger(X, Y);
      FStart := PointInteger(X, Y);
      FStatus := siStatusInitial;

      FPrevFlags := ClippingFlagsInteger(X, Y, FClipBox);

      if FPrevFlags = 0 then
          MoveToNoClip(X, Y);
    end
  else
      MoveToNoClip(X, Y);
end;

procedure TAggRasterizerScanLineAA.LineTo(Point: TPointInteger);
begin
  if FClipping then
      ClipSegment(Point)
  else
      LineToNoClip(Point);
end;

procedure TAggRasterizerScanLineAA.LineTo(X, Y: Integer);
begin
  if FClipping then
      ClipSegment(X, Y)
  else
      LineToNoClip(X, Y);
end;

procedure TAggRasterizerScanLineAA.Sort;
begin
  FOutline.SortCells;
end;

function TAggRasterizerScanLineAA.RewindScanLines: Boolean;
begin
  if FAutoClose then
      ClosePolygon;

  FOutline.SortCells;

  if FOutline.TotalCells = 0 then
    begin
      Result := False;

      Exit;
    end;

  FCurY := FOutline.MinY;
  Result := True;
end;

function TAggRasterizerScanLineAA.SweepScanLine(SL: TAggCustomScanLine): Boolean;
var
  X, Area: Integer;
  Cover: Integer;
  alpha: Cardinal;
  Cells: PPAggCellAA;

  CurCell: PAggCellAA;
  NumCells: Cardinal;
begin
  repeat
    if FCurY > FOutline.MaxY then
      begin
        Result := False;

        Exit;
      end;

    SL.ResetSpans;

    NumCells := FOutline.ScanLineNumCells(FCurY);
    Cells := FOutline.ScanLineCells(FCurY);

    Cover := 0;

    while NumCells > 0 do
      begin
        CurCell := Cells^;

        X := CurCell.X;
        Area := CurCell.Area;

        Inc(Cover, CurCell.Cover);

        // accumulate all cells with the same X
        Dec(NumCells);

        while NumCells > 0 do
          begin
            Inc(Cells);

            CurCell := Cells^;

            if CurCell.X <> X then
                Break;

            Inc(Area, CurCell.Area);
            Inc(Cover, CurCell.Cover);

            Dec(NumCells);
          end;

        if Area <> 0 then
          begin
            alpha := CalculateAlpha((Cover shl (CAggPolyBaseShift + 1)) - Area);

            if alpha <> 0 then
                SL.AddCell(X, alpha);

            Inc(X);
          end;

        if (NumCells <> 0) and (CurCell.X > X) then
          begin
            alpha := CalculateAlpha(Cover shl (CAggPolyBaseShift + 1));

            if alpha <> 0 then
                SL.AddSpan(X, CurCell.X - X, alpha);
          end;
      end;

    if Boolean(SL.NumSpans) then
        Break;

    Inc(FCurY);
  until False;

  SL.Finalize(FCurY);

  Inc(FCurY);

  Result := True;
end;

function TAggRasterizerScanLineAA.NavigateScanLine(Y: Integer): Boolean;
begin
  if FAutoClose then
      ClosePolygon;

  FOutline.SortCells;

  if (FOutline.TotalCells = 0) or (Y < FOutline.MinY) or
    (Y > FOutline.MaxY) then
    begin
      Result := False;

      Exit;
    end;

  FCurY := Y;
  Result := True;
end;

function TAggRasterizerScanLineAA.HitTest(TX, TY: Integer): Boolean;
var
  SL: TAggScanLineHitTest;
begin
  if not NavigateScanLine(TY) then
    begin
      Result := False;

      Exit;
    end;

  SL := TAggScanLineHitTest.Create(TX);
  try
    SweepScanLine(SL);

    Result := SL.Hit;
  finally
      SL.Free
  end;
end;

function TAggRasterizerScanLineAA.GetMinX;
begin
  Result := FOutline.MinX;
end;

function TAggRasterizerScanLineAA.GetMinY;
begin
  Result := FOutline.MinY;
end;

function TAggRasterizerScanLineAA.GetFillingRule: TAggFillingRule;
begin
  Result := FFillingRule
end;

function TAggRasterizerScanLineAA.GetMaxX;
begin
  Result := FOutline.MaxX;
end;

function TAggRasterizerScanLineAA.GetMaxY;
begin
  Result := FOutline.MaxY;
end;

function TAggRasterizerScanLineAA.CalculateAlpha(Area: Integer): Cardinal;
var
  Cover: System.Integer;
begin
  // 1: cover:=area shr (CAggPolyBaseShift * 2 + 1 - CAggAntiAliasingShift );
  // 2: cover:=round(area / (1 shl (CAggPolyBaseShift * 2 + 1 - CAggAntiAliasingShift ) ) );
  Cover := ShrInt32(Area, CAggPolyBaseShift shl 1 + 1 - CAggAntiAliasingShift);

  if Cover < 0 then
      Cover := -Cover;

  if FFillingRule = frEvenOdd then
    begin
      Cover := Cover and CAggAntiAliasing2Mask;

      if Cover > CAggAntiAliasingNum then
          Cover := CAggAntiAliasing2Num - Cover;
    end;

  if Cover > CAggAntiAliasingMask then
      Cover := CAggAntiAliasingMask;

  Result := FGamma[Cover];
end;

procedure TAggRasterizerScanLineAA.AddPath;
var
  Cmd: Cardinal;
  X, Y: Double;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
    begin
      AddVertex(X, Y, Cmd);

      Cmd := Vs.Vertex(@X, @Y);
    end;
end;

procedure TAggRasterizerScanLineAA.AddVertex;
begin
  if IsClose(Cmd) then
      ClosePolygon
  else if IsMoveTo(Cmd) then
      MoveTo(PolyCoord(X), PolyCoord(Y))
  else if IsVertex(Cmd) then
      LineTo(PolyCoord(X), PolyCoord(Y));
end;

end. 
