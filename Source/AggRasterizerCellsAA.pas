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
unit AggRasterizerCellsAA;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggMath,
  AggArray,
  AggScanline;

const
  CAggCellBlockShift = 12;
  CAggCellBlockSize  = 1 shl CAggCellBlockShift;
  CAggCellBlockMask  = CAggCellBlockSize - 1;
  CAggCellBlockPool  = 256;
  CAggCellBlockLimit = 1024;

type
  // A pixel cell. There're no constructors defined and it was done
  // intentionally in order to avoid extra overhead when allocating an
  // array of cells.
  PPPAggCellStyleAA = ^PPAggCellStyleAA;
  PPAggCellStyleAA  = ^PAggCellStyleAA;
  PAggCellStyleAA   = ^TAggCellStyleAA;

  TAggCellStyleAA = packed record
    X, Y, Cover, Area: Integer;
    Left, Right: Int16;
  public
    procedure Initial;
    procedure Style(C: PAggCellStyleAA);
    function NotEqual(EX, EY: Integer; C: PAggCellStyleAA): Integer;
  end;

  PAggSortedY = ^TAggSortedY;

  TAggSortedY = packed record
    Start, Num: Cardinal;
  end;

  // An internal class that implements the main rasterization algorithm.
  // Used in the Rasterizer. Should not be used direcly.
  TAggRasterizerCellsAA = class
  private
    FNumBlocks, FMaxBlocks, FCurrVlock, FNumCells: Cardinal;

    FCells: PPAggCellStyleAA;

    FCurrentCellPtr: PAggCellStyleAA;
    FSortedCells, FSortedY: TAggPodVector;
    FCurrentCell, FStyleCell: TAggCellStyleAA;

    FMin, FMax: TPointInteger;

    FSorted: Boolean;

    procedure SetCurrentCell(X, Y: Integer);
    procedure AddCurrentCell;
    procedure RenderHorizontalLine(EY, x1, y1, x2, y2: Integer);
    procedure AllocateBlock;

    function GetMinX: Integer;
    function GetMinY: Integer;
    function GetMaxX: Integer;
    function GetMaxY: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    procedure Style(StyleCell: PAggCellStyleAA);
    procedure Line(x1, y1, x2, y2: Integer);

    procedure SortCells;

    function ScanLineNumCells(Y: Cardinal): Cardinal;
    function ScanLineCells(Y: Cardinal): PPAggCellStyleAA;

    property TotalCells: Cardinal read FNumCells;
    property Sorted: Boolean read FSorted;

    property MinX: Integer read GetMinX;
    property MinY: Integer read GetMinY;
    property MaxX: Integer read GetMaxX;
    property MaxY: Integer read GetMaxY;
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

implementation


{ TAggCellStyleAA }

procedure TAggCellStyleAA.Initial;
begin
  X := $7FFFFFFF;
  Y := $7FFFFFFF;
  Cover := 0;
  Area := 0;
  Left := -1;
  Right := -1;
end;

procedure TAggCellStyleAA.Style(C: PAggCellStyleAA);
begin
  Left := C.Left;
  Right := C.Right;
end;

function TAggCellStyleAA.NotEqual(EX, EY: Integer; C: PAggCellStyleAA): Integer;
begin
  Result := (EX - X) or (EY - Y) or (Left - C.Left) or (Right - C.Right);
end;

{ TAggRasterizerCellsAA }

constructor TAggRasterizerCellsAA.Create;
begin
  FNumBlocks := 0;
  FMaxBlocks := 0;
  FCurrVlock := 0;
  FNumCells := 0;

  FCells := nil;
  FCurrentCellPtr := nil;

  FSortedCells := TAggPodVector.Create(SizeOf(PAggCellStyleAA));
  FSortedY := TAggPodVector.Create(SizeOf(TAggSortedY));

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
  FSorted := False;

  FStyleCell.Initial;
  FCurrentCell.Initial;
end;

destructor TAggRasterizerCellsAA.Destroy;
var
  PTR: PPAggCellStyleAA;
begin
  FSortedCells.Free;
  FSortedY.Free;

  if FNumBlocks <> 0 then
    begin
      PTR := PPAggCellStyleAA(PtrComp(FCells) + (FNumBlocks - 1) *
        SizeOf(PAggCellStyleAA));

      while FNumBlocks <> 0 do
        begin
          Dec(FNumBlocks);

          AggFreeMem(Pointer(PTR^), CAggCellBlockSize * SizeOf(TAggCellStyleAA));

          Dec(PtrComp(PTR), SizeOf(PAggCellStyleAA));
        end;

      AggFreeMem(Pointer(FCells), FMaxBlocks * SizeOf(PAggCellStyleAA));
    end;

  inherited;
end;

procedure TAggRasterizerCellsAA.Reset;
begin
  FNumCells := 0;
  FCurrVlock := 0;

  FCurrentCell.Initial;
  FStyleCell.Initial;

  FSorted := False;
  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
end;

procedure TAggRasterizerCellsAA.Style(StyleCell: PAggCellStyleAA);
begin
  FStyleCell.Style(StyleCell);
end;

procedure TAggRasterizerCellsAA.Line(x1, y1, x2, y2: Integer);
const
  CDxLimit = 16384 shl CAggPolySubpixelShift;
var
  dx, dy: Integer;
  center: TPointInteger;
  Ex1, Ex2, Ey1, Ey2, FY1, FY2, EX, TwoFX, Area: Integer;
  FromX, tox, p, Rem, ModValue, Lift, Delta, First, Incr: Integer;
begin
  dx := x2 - x1;

  if (dx >= CDxLimit) or (dx <= -CDxLimit) then
    begin
      center := PointInteger(ShrInt32(x1 + x2, 1), ShrInt32(y1 + y2, 1));

      Line(x1, y1, center.X, center.Y);
      Line(center.X, center.Y, x2, y2);
    end;

  dy := y2 - y1;
  Ex1 := ShrInt32(x1, CAggPolySubpixelShift);
  Ex2 := ShrInt32(x2, CAggPolySubpixelShift);
  Ey1 := ShrInt32(y1, CAggPolySubpixelShift);
  Ey2 := ShrInt32(y2, CAggPolySubpixelShift);
  FY1 := y1 and CAggPolySubpixelMask;
  FY2 := y2 and CAggPolySubpixelMask;

  if Ex1 < FMin.X then
      FMin.X := Ex1;

  if Ex1 > FMax.X then
      FMax.X := Ex1;

  if Ey1 < FMin.Y then
      FMin.Y := Ey1;

  if Ey1 > FMax.Y then
      FMax.Y := Ey1;

  if Ex2 < FMin.X then
      FMin.X := Ex2;

  if Ex2 > FMax.X then
      FMax.X := Ex2;

  if Ey2 < FMin.Y then
      FMin.Y := Ey2;

  if Ey2 > FMax.Y then
      FMax.Y := Ey2;

  SetCurrentCell(Ex1, Ey1);

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

  if dx = 0 then
    begin
      EX := ShrInt32(x1, CAggPolySubpixelShift);
      TwoFX := (x1 - (EX shl CAggPolySubpixelShift)) shl 1;
      First := CAggPolySubpixelScale;

      if dy < 0 then
        begin
          First := 0;
          Incr := -1;
        end;

      FromX := x1;

      // RenderHorizontalLine(ey1 ,FromX ,fy1 ,FromX ,first );
      Delta := First - FY1;

      Inc(FCurrentCell.Cover, Delta);
      Inc(FCurrentCell.Area, TwoFX * Delta);
      Inc(Ey1, Incr);

      SetCurrentCell(EX, Ey1);

      Delta := First + First - CAggPolySubpixelScale;
      Area := TwoFX * Delta;

      while Ey1 <> Ey2 do
        begin
          // RenderHorizontalLine(ey1 ,FromX ,CAggPolySubpixelScale - first ,FromX ,first );
          FCurrentCell.Cover := Delta;
          FCurrentCell.Area := Area;

          Inc(Ey1, Incr);

          SetCurrentCell(EX, Ey1);
        end;

      // RenderHorizontalLine(ey1 ,FromX ,CAggPolySubpixelScale - first ,FromX ,fy2 );
      Delta := FY2 - CAggPolySubpixelScale + First;

      Inc(FCurrentCell.Cover, Delta);
      Inc(FCurrentCell.Area, TwoFX * Delta);

      Exit;
    end;

  // ok, we have to render several HorizontalLines
  p := (CAggPolySubpixelScale - FY1) * dx;
  First := CAggPolySubpixelScale;

  if dy < 0 then
    begin
      p := FY1 * dx;
      First := 0;
      Incr := -1;
      dy := -dy;
    end;

  Delta := p div dy;
  ModValue := p mod dy;

  if ModValue < 0 then
    begin
      Dec(Delta);
      Inc(ModValue, dy);
    end;

  FromX := x1 + Delta;

  RenderHorizontalLine(Ey1, x1, FY1, FromX, First);

  Inc(Ey1, Incr);

  SetCurrentCell(ShrInt32(FromX, CAggPolySubpixelShift), Ey1);

  if Ey1 <> Ey2 then
    begin
      p := CAggPolySubpixelScale * dx;
      Lift := p div dy;
      Rem := p mod dy;

      if Rem < 0 then
        begin
          Dec(Lift);
          Inc(Rem, dy);
        end;

      Dec(ModValue, dy);

      while Ey1 <> Ey2 do
        begin
          Delta := Lift;

          Inc(ModValue, Rem);

          if ModValue >= 0 then
            begin
              Dec(ModValue, dy);
              Inc(Delta);
            end;

          tox := FromX + Delta;

          RenderHorizontalLine(Ey1, FromX, CAggPolySubpixelScale - First, tox, First);

          FromX := tox;

          Inc(Ey1, Incr);

          SetCurrentCell(ShrInt32(FromX, CAggPolySubpixelShift), Ey1);
        end;
    end;

  RenderHorizontalLine(Ey1, FromX, CAggPolySubpixelScale - First, x2, FY2);
end;

function TAggRasterizerCellsAA.GetMinX: Integer;
begin
  Result := FMin.X;
end;

function TAggRasterizerCellsAA.GetMinY: Integer;
begin
  Result := FMin.Y;
end;

function TAggRasterizerCellsAA.GetMaxX: Integer;
begin
  Result := FMax.X;
end;

function TAggRasterizerCellsAA.GetMaxY: Integer;
begin
  Result := FMax.Y;
end;

procedure SwapCells(A, b: Pointer);
var
  Temp: Pointer;

begin
  Temp := Pointer(A^);
  Pointer(A^) := Pointer(b^);
  Pointer(b^) := Temp;
end;

const
  CQSortThreshold = 9;

procedure QuickSortCells(Start: PPAggCellStyleAA; Num: Cardinal);
var
  Stack: array [0 .. 79] of PPAggCellStyleAA;
  Top: PPPAggCellStyleAA;
  Limit, Base: PPAggCellStyleAA;
  Len, X: Integer;
  i, J, Pivot: PPAggCellStyleAA;
begin
  Limit := PPAggCellStyleAA(PtrComp(Start) + Num *
    SizeOf(PAggCellStyleAA));
  Base := Start;
  Top := @Stack[0];

  repeat
    Len := (PtrComp(Limit) - PtrComp(Base)) div SizeOf(PAggCellStyleAA);

    if Len > CQSortThreshold then
      begin
        // we use base + len/2 as the pivot
        Pivot := PPAggCellStyleAA(PtrComp(Base) + (Len div 2) *
          SizeOf(PAggCellStyleAA));

        SwapCells(Base, Pivot);

        i := PPAggCellStyleAA(PtrComp(Base) + SizeOf(PAggCellStyleAA));
        J := PPAggCellStyleAA(PtrComp(Limit) - SizeOf(PAggCellStyleAA));

        // now ensure that *i <= *base <= *j
        if J^^.X < i^^.X then
            SwapCells(i, J);

        if Base^^.X < i^^.X then
            SwapCells(Base, i);

        if J^^.X < Base^^.X then
            SwapCells(Base, J);

        repeat
          X := Base^^.X;

          repeat
              Inc(PtrComp(i), SizeOf(PAggCellStyleAA));
          until i^^.X >= X;

          repeat
              Dec(PtrComp(J), SizeOf(PAggCellStyleAA));
          until X >= J^^.X;

          if PtrComp(i) > PtrComp(J) then
              Break;

          SwapCells(i, J);
        until False;

        SwapCells(Base, J);

        // now, push the largest sub-array
        if PtrComp(J) - PtrComp(Base) > PtrComp(Limit) - PtrComp(i) then
          begin
            Top^ := Base;

            PPPAggCellStyleAA(PtrComp(Top) +
              SizeOf(PPAggCellStyleAA))^ := J;

            Base := i;
          end
        else
          begin
            Top^ := i;

            PPPAggCellStyleAA(PtrComp(Top) + SizeOf(PPAggCellStyleAA))
              ^ := Limit;

            Limit := J;
          end;

        Inc(PtrComp(Top), 2 * SizeOf(PPAggCellStyleAA));
      end
    else
      begin
        // the sub-array is small, perform insertion sort
        J := Base;
        i := PPAggCellStyleAA(PtrComp(J) + SizeOf(PAggCellStyleAA));

        while PtrComp(i) < PtrComp(Limit) do
          begin
            while PPAggCellStyleAA(PtrComp(J) + SizeOf(PAggCellStyleAA))^^.X
              < J^^.X do
              begin
                SwapCells(PPAggCellStyleAA(PtrComp(J) +
                  SizeOf(PAggCellStyleAA)), J);

                if PtrComp(J) = PtrComp(Base) then
                    Break;

                Dec(PtrComp(J), SizeOf(PAggCellStyleAA));
              end;

            J := i;

            Inc(PtrComp(i), SizeOf(PAggCellStyleAA));
          end;

        if PtrComp(Top) > PtrComp(@Stack[0]) then
          begin
            Dec(PtrComp(Top), 2 * SizeOf(PPAggCellStyleAA));

            Base := Top^;
            Limit := PPPAggCellStyleAA
              (PtrComp(Top) + SizeOf(PPAggCellStyleAA))^;
          end
        else
            Break;
      end;
  until False;
end;

procedure TAggRasterizerCellsAA.SortCells;
var
  BlockPointer: PPAggCellStyleAA;
  CellPointer: PAggCellStyleAA;

  nb, i, Start, v: Cardinal;

  CurrentY: PAggSortedY;
begin
  // Perform sort only the first time.
  if FSorted then
      Exit;

  AddCurrentCell;

  FCurrentCell.X := $7FFFFFFF;
  FCurrentCell.Y := $7FFFFFFF;
  FCurrentCell.Cover := 0;
  FCurrentCell.Area := 0;

  if FNumCells = 0 then
      Exit;

  // Allocate the array of cell pointers
  FSortedCells.Allocate(FNumCells, 16);

  // Allocate and zero the Y array
  FSortedY.Allocate(FMax.Y - FMin.Y + 1, 16);
  FSortedY.Zero;

  // Create the Y-histogram (count the numbers of cells for each Y)
  BlockPointer := FCells;

  nb := FNumCells shr CAggCellBlockShift;

  while nb <> 0 do
    begin
      Dec(nb);

      CellPointer := BlockPointer^;

      Inc(PtrComp(BlockPointer), SizeOf(PAggCellStyleAA));

      i := CAggCellBlockSize;

      while i <> 0 do
        begin
          Dec(i);
          Inc(PAggSortedY(FSortedY[CellPointer.Y - FMin.Y]).Start);
          Inc(PtrComp(CellPointer), SizeOf(TAggCellStyleAA));
        end;
    end;

  CellPointer := BlockPointer^;

  Inc(PtrComp(BlockPointer), SizeOf(PAggCellStyleAA));

  i := FNumCells and CAggCellBlockMask;

  while i <> 0 do
    begin
      Dec(i);
      Inc(PAggSortedY(FSortedY[CellPointer.Y - FMin.Y]).Start);
      Inc(PtrComp(CellPointer), SizeOf(TAggCellStyleAA));
    end;

  // Convert the Y-histogram into the array of starting indexes
  Start := 0;
  i := 0;

  while i < FSortedY.Size do
    begin
      v := PAggSortedY(FSortedY[i]).Start;

      PAggSortedY(FSortedY[i]).Start := Start;

      Inc(Start, v);
      Inc(i);
    end;

  // Fill the cell pointer array sorted by Y
  BlockPointer := FCells;

  nb := FNumCells shr CAggCellBlockShift;

  while nb <> 0 do
    begin
      Dec(nb);

      CellPointer := BlockPointer^;

      Inc(PtrComp(BlockPointer), SizeOf(PAggCellStyleAA));

      i := CAggCellBlockSize;

      while i <> 0 do
        begin
          Dec(i);

          CurrentY := PAggSortedY(FSortedY[CellPointer.Y - FMin.Y]);

          PPAggCellStyleAA(FSortedCells[CurrentY.Start +
            CurrentY.Num])^ := CellPointer;

          Inc(CurrentY.Num);
          Inc(PtrComp(CellPointer), SizeOf(TAggCellStyleAA));
        end;
    end;

  CellPointer := BlockPointer^;

  Inc(PtrComp(BlockPointer), SizeOf(PAggCellStyleAA));

  i := FNumCells and CAggCellBlockMask;

  while i <> 0 do
    begin
      Dec(i);

      CurrentY := PAggSortedY(FSortedY[CellPointer.Y - FMin.Y]);

      PPAggCellStyleAA(FSortedCells[CurrentY.Start +
        CurrentY.Num])^ := CellPointer;

      Inc(CurrentY.Num);
      Inc(PtrComp(CellPointer), SizeOf(TAggCellStyleAA));
    end;

  // Finally arrange the X-arrays
  i := 0;

  while i < FSortedY.Size do
    begin
      CurrentY := PAggSortedY(FSortedY[i]);

      if CurrentY.Num <> 0 then
          QuickSortCells(PPAggCellStyleAA(PtrComp(FSortedCells.Data) +
          CurrentY.Start * SizeOf(PAggCellStyleAA)), CurrentY.Num);

      Inc(i);
    end;

  FSorted := True;
end;

function TAggRasterizerCellsAA.ScanLineNumCells(Y: Cardinal): Cardinal;
begin
  Result := PAggSortedY(FSortedY[Y - FMin.Y]).Num;
end;

function TAggRasterizerCellsAA.ScanLineCells(Y: Cardinal): PPAggCellStyleAA;
begin
  Result := PPAggCellStyleAA(PtrComp(FSortedCells.Data) +
    PAggSortedY(FSortedY[Y - FMin.Y]).Start * SizeOf(PAggCellStyleAA));
end;

procedure TAggRasterizerCellsAA.SetCurrentCell(X, Y: Integer);
begin
  if FCurrentCell.NotEqual(X, Y, @FStyleCell) <> 0 then
    begin
      AddCurrentCell;

      FCurrentCell.Style(@FStyleCell);

      FCurrentCell.X := X;
      FCurrentCell.Y := Y;
      FCurrentCell.Cover := 0;
      FCurrentCell.Area := 0;
    end;
end;

procedure TAggRasterizerCellsAA.AddCurrentCell;
begin
  if FCurrentCell.Area or FCurrentCell.Cover <> 0 then
    begin
      if FNumCells and CAggCellBlockMask = 0 then
        begin
          if FNumBlocks >= CAggCellBlockLimit then
              Exit;

          AllocateBlock;
        end;

      FCurrentCellPtr^ := FCurrentCell;

      Inc(PtrComp(FCurrentCellPtr), SizeOf(TAggCellStyleAA));
      Inc(FNumCells);
    end;
end;

procedure TAggRasterizerCellsAA.RenderHorizontalLine(EY, x1, y1, x2, y2: Integer);
var
  Ex1, Ex2, FX1, FX2, Delta, p, First, dx, Incr, Lift, ModValue, Rem: Integer;
begin
  Ex1 := ShrInt32(x1, CAggPolySubpixelShift);
  Ex2 := ShrInt32(x2, CAggPolySubpixelShift);
  FX1 := x1 and CAggPolySubpixelMask;
  FX2 := x2 and CAggPolySubpixelMask;

  // trivial case. Happens often
  if y1 = y2 then
    begin
      SetCurrentCell(Ex2, EY);
      Exit;
    end;

  // everything is located in a single cell.  That is easy!
  if Ex1 = Ex2 then
    begin
      Delta := y2 - y1;

      Inc(FCurrentCell.Cover, Delta);
      Inc(FCurrentCell.Area, (FX1 + FX2) * Delta);

      Exit;
    end;

  // ok, we'll have to render a run of adjacent cells on the same
  // HorizontalLine...
  p := (CAggPolySubpixelScale - FX1) * (y2 - y1);
  First := CAggPolySubpixelScale;
  Incr := 1;

  dx := x2 - x1;

  if dx < 0 then
    begin
      p := FX1 * (y2 - y1);
      First := 0;
      Incr := -1;
      dx := -dx;
    end;

  Delta := p div dx;
  ModValue := p mod dx;

  if ModValue < 0 then
    begin
      Dec(Delta);
      Inc(ModValue, dx);
    end;

  Inc(FCurrentCell.Cover, Delta);
  Inc(FCurrentCell.Area, (FX1 + First) * Delta);
  Inc(Ex1, Incr);

  SetCurrentCell(Ex1, EY);

  Inc(y1, Delta);

  if Ex1 <> Ex2 then
    begin
      p := CAggPolySubpixelScale * (y2 - y1 + Delta);
      Lift := p div dx;
      Rem := p mod dx;

      if Rem < 0 then
        begin
          Dec(Lift);
          Inc(Rem, dx);
        end;

      Dec(ModValue, dx);

      while Ex1 <> Ex2 do
        begin
          Delta := Lift;

          Inc(ModValue, Rem);

          if ModValue >= 0 then
            begin
              Dec(ModValue, dx);
              Inc(Delta);
            end;

          Inc(FCurrentCell.Cover, Delta);
          Inc(FCurrentCell.Area, CAggPolySubpixelScale * Delta);

          Inc(y1, Delta);
          Inc(Ex1, Incr);

          SetCurrentCell(Ex1, EY);
        end;
    end;

  Delta := y2 - y1;

  Inc(FCurrentCell.Cover, Delta);
  Inc(FCurrentCell.Area, (FX2 + CAggPolySubpixelScale - First) * Delta);
end;

procedure TAggRasterizerCellsAA.AllocateBlock;
var
  NewCells: PPAggCellStyleAA;
begin
  if FCurrVlock >= FNumBlocks then
    begin
      if FNumBlocks >= FMaxBlocks then
        begin
          AggGetMem(Pointer(NewCells), (FMaxBlocks + CAggCellBlockPool) *
            SizeOf(PAggCellStyleAA));

          if FCells <> nil then
            begin
              Move(FCells^, NewCells^, FMaxBlocks * SizeOf(PAggCellStyleAA));

              AggFreeMem(Pointer(FCells), FMaxBlocks * SizeOf(PAggCellStyleAA));
            end;

          FCells := NewCells;

          Inc(FMaxBlocks, CAggCellBlockPool);
        end;

      AggGetMem(Pointer(PPAggCellStyleAA(PtrComp(FCells) + FNumBlocks *
        SizeOf(PAggCellStyleAA))^), CAggCellBlockSize * SizeOf(TAggCellStyleAA));

      Inc(FNumBlocks);
    end;

  FCurrentCellPtr := PPAggCellStyleAA(PtrComp(FCells) + FCurrVlock *
    SizeOf(PAggCellStyleAA))^;

  Inc(FCurrVlock);
end;

{ TAggScanLineHitTest }

constructor TAggScanLineHitTest.Create(X: Integer);
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

end. 
