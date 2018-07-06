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
unit AggRenderingBufferDynaRow;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggRenderingBuffer;

type
  // Rendering buffer class with dynamic allocation of the rows.
  // The rows are allocated as needed when requesting for PSpan().
  // The class automatically calculates min_x and max_x for each row.
  // Generally it's more efficient to use this class as a temporary buffer
  // for rendering a few lines and then to blend it with another buffer.
  TAggRenderingBufferDynaRow = class(TAggRenderingBuffer)
  private
    FBuffer: PAggRowDataType;     // Pointers to each row of the buffer
    FAlloc, FByteWidth: Cardinal; // Width in bytes
  public
    constructor Create(width, height, ByteWidth: Cardinal);
    destructor Destroy; override;

    procedure Init(width, height, ByteWidth: Cardinal);

    function GetByteWidth: Cardinal;

    function RowXY(X, Y: Integer; Len: Cardinal): PInt8u; override;
    function Row(Y: Cardinal): PInt8u; override;
  end;

implementation


{ TAggRenderingBufferDynaRow }

// Allocate and clear the buffer
constructor TAggRenderingBufferDynaRow.Create(width, height,
  ByteWidth: Cardinal);
begin
  FAlloc := SizeOf(TAggRowDataType) * height;

  AggGetMem(Pointer(FBuffer), FAlloc);

  FWidth := width;
  FHeight := height;

  FByteWidth := ByteWidth;

  FillChar(FBuffer^, FAlloc, 0);
end;

destructor TAggRenderingBufferDynaRow.Destroy;
begin
  Init(0, 0, 0);
  inherited;
end;

// Allocate and clear the buffer
procedure TAggRenderingBufferDynaRow.Init(width, height, ByteWidth: Cardinal);
var
  i: Cardinal;
begin
  i := 0;

  while i < FHeight do
    begin
      AggFreeMem(Pointer(PAggRowDataType(PtrComp(FBuffer) + i *
        SizeOf(TAggRowDataType)).PTR), FByteWidth);

      Inc(i);
    end;

  AggFreeMem(Pointer(FBuffer), FAlloc);

  FBuffer := nil;

  if (width <> 0) and (height <> 0) then
    begin
      FWidth := width;
      FHeight := height;

      FByteWidth := ByteWidth;

      FAlloc := SizeOf(TAggRowDataType) * height;

      AggGetMem(Pointer(FBuffer), FAlloc);
      FillChar(FBuffer^, FAlloc, 0);
    end;
end;

function TAggRenderingBufferDynaRow.GetByteWidth: Cardinal;
begin
  Result := FByteWidth;
end;

// The main function used for rendering. Returns pointer to the
// pre-allocated Span. Memory for the row is allocated as needed.
function TAggRenderingBufferDynaRow.RowXY(X, Y: Integer; Len: Cardinal): PInt8u;
var
  R: PAggRowDataType;
  p: PInt8u;

  x2: Integer;

begin
  R := PAggRowDataType(PtrComp(FBuffer) + Y * SizeOf(TAggRowDataType));
  x2 := X + Len - 1;

  if R.PTR <> nil then
    begin
      if X < R.x1 then
          R.x1 := X;

      if x2 > R.x2 then
          R.x2 := x2;

    end
  else
    begin
      AggGetMem(Pointer(p), FByteWidth);

      R.PTR := p;
      R.x1 := X;
      R.x2 := x2;

      FillChar(p^, FByteWidth, 0);
    end;

  Result := R.PTR;
end;

function TAggRenderingBufferDynaRow.Row(Y: Cardinal): PInt8u;
begin
  Result := RowXY(0, Y, FWidth);
end;

end. 