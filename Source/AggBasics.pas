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
unit AggBasics;

{$I AggCompiler.inc}

interface

type
  Int8    = ShortInt;
  Int8u   = Byte;
  Int16   = Smallint;
  Int16u  = Word;
  Int32   = Integer;
  Int32u  = Cardinal;
  Int64u  = UInt64;
  PInt8   = ^Int8;
  PInt8u  = ^Int8u;
  PInt16  = ^Int16;
  PInt16u = ^Int16u;
  PInt32  = ^Int32;
  PInt32u = ^Int32u;
  PInt64  = ^Int64;
  PInt64u = ^Int64u;
  PPInt8u = ^PInt8u;

  TCover = Byte;
  PCover = ^TCover;

  PInt8uArray2 = ^TInt8uArray2;
  TInt8uArray2 = array [0 .. 1] of Int8u;

  PAggBytes = ^TAggBytes;
  TAggBytes = array of Byte;

  TInt16uAccess = packed record
    low, high: Int8u;
  end;

  TInt32Access = packed record
    low, high: Int16;
  end;

  TInt32Int8uAccess = packed record
    Values: array [0 .. 3] of Int8u;
  end;

  TInt32uAccess = packed record
    low, high: Int16u;
  end;

  TInt64uAccess = packed record
    low, high: TInt32uAccess;
  end;

  { To achive maximum compatiblity with older code, FPC doesn't change the size
    of predefined data types like integer, longint or Word when changing from
    32 to 64 Bit. However, the size of a pointer is 8 bytes on a 64 bit
    architecture so constructs like longint(pointer(p)) are doomed to crash on
    64 bit architectures. However, to alLow you to write portable code, the
    FPC system unit introduces the types PtrInt and PtrUInt which are signed
    and Cardinal integer data types with the same size as a pointer.

    Keep in mind that the size change of the "pointer" type also affects packed record
    sizes. If you allocate records with fixed sizes, and not with new or with
    getmem (<x>,SizeOf(<x>)), this will have to be fixed. }
  // Pascal Pointer Computation Type
  PtrComp = NativeUInt;

  // Pascal's pointer-in-an-array-access helper structures
  PPointer32 = ^TPointer32;

  TPointer32 = packed record
    case Integer of
      1: (PTR: Pointer);
      2: (Int: NativeUInt);
  end;

  PPDouble = ^PDouble;

  PDoubleArray2 = ^TDoubleArray2;
  TDoubleArray2 = array [0 .. 1] of Double;

  PDoubleArray4 = ^TDoubleArray4;
  TDoubleArray4 = array [0 .. 3] of Double;

  PDoubleArray8 = ^TDoubleArray8;
  TDoubleArray8 = array [0 .. 7] of Double;

  PDoubleArray42 = ^TDoubleArray42;
  TDoubleArray42 = array [0 .. 3, 0 .. 1] of Double;

  PDoubleMatrix4x4 = ^TDoubleMatrix4x4;
  TDoubleMatrix4x4 = array [0 .. 3, 0 .. 3] of Double;

  PDoubleMatrix8x1 = ^TDoubleMatrix8x1;
  TDoubleMatrix8x1 = array [0 .. 7, 0 .. 0] of Double;

  PDoubleMatrix8x8 = ^TDoubleMatrix8x8;
  TDoubleMatrix8x8 = array [0 .. 7, 0 .. 7] of Double;

  PDoubleMatrix2x6 = ^TDoubleMatrix2x6;
  TDoubleMatrix2x6 = array [0 .. 25] of Double;

  TAggGamma = class
  protected
    function GetDir(Value: Cardinal): Cardinal; virtual; abstract;
    function GetInv(Value: Cardinal): Cardinal; virtual; abstract;
  public
    property Dir[Value: Cardinal]: Cardinal read GetDir;
    property Inv[Value: Cardinal]: Cardinal read GetInv;
  end;

  TAggFillingRule = (frNonZero, frEvenOdd);
  TAggLineCap     = (lcButt, lcSquare, lcRound);
  TAggLineJoin    = (ljMiter, ljMiterRevert, ljMiterRound, ljRound, ljBevel);
  TAggInnerJoin   = (ijBevel, ijMiter, ijJag, ijRound);

const
  CAggCoverShift = 8;
  CAggCoverSize  = 1 shl CAggCoverShift;
  CAggCoverMask  = CAggCoverSize - 1;
  CAggCoverNone  = 0;
  CAggCoverFull  = CAggCoverMask;

  // These constants determine the subpixel accuracy, to be more precise,
  // the number of bits of the fractional part of the coordinates.
  // The possible coordinate capacity in bits can be calculated by formula:
  // SizeOf(Integer) * 8 - CAggPolySubpixelShift, i.ECX, for 32-bit integers and
  // 8-bits fractional part the capacity is 24 bits.
  CAggPolySubpixelShift = 8;
  CAggPolySubpixelScale = 1 shl CAggPolySubpixelShift;
  CAggPolySubpixelMask  = CAggPolySubpixelScale - 1;

  // CAggPathCmd enumeration (see flags below)
  CAggPathCmdStop     = 0;
  CAggPathCmdMoveTo   = 1;
  CAggPathCmdLineTo   = 2;
  CAggPathCmdCurve3   = 3;
  CAggPathCmdCurve4   = 4;
  CAggPathCmdCurveN   = 5;
  CAggPathCmdCatrom   = 6;
  CAggPathCmdUbSpline = 7;
  CAggPathCmdEndPoly  = $0F;
  CAggPathCmdMask     = $0F;

  // CAggPathFlags
  CAggPathFlagsNone  = 0;
  CAggPathFlagsCcw   = $10;
  CAggPathFlagsCw    = $20;
  CAggPathFlagsClose = $40;
  CAggPathFlagsMask  = $F0;

  CDeg2Rad: Double = Pi / 180;
  CRad2Deg: Double = 180 / Pi;

type
  PPointDouble = ^TPointDouble;

  TPointDouble = packed record
    X, Y: Double;
  public
    class operator Equal(const Lhs, Rhs: TPointDouble): Boolean;
    class operator NotEqual(const Lhs, Rhs: TPointDouble): Boolean;
  end;

  PPointInteger = ^TPointInteger;

  TPointInteger = packed record
    X, Y: Integer;
  public
    class operator Equal(const Lhs, Rhs: TPointInteger): Boolean;
    class operator NotEqual(const Lhs, Rhs: TPointInteger): Boolean;
  end;

  PRectInteger = ^TRectInteger;

  TRectInteger = packed record
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
{$IFNDEF FPC}
    constructor Create(X1, Y1, X2, Y2: Integer); overload;
    constructor Create(Rect: TRectInteger); overload;
{$ENDIF}
    class operator Equal(const Lhs, Rhs: TRectInteger): Boolean;
    class operator NotEqual(const Lhs, Rhs: TRectInteger): Boolean;
    class operator Add(const Lhs, Rhs: TRectInteger): TRectInteger;
    class operator Subtract(const Lhs, Rhs: TRectInteger): TRectInteger;
    class function Zero: TRectInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF} static;

    procedure Normalize;
    function Clip(var Rect: TRectInteger): Boolean;
    function IsValid: Boolean;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;

    case Integer of
      0: (X1, Y1, X2, Y2: Integer);
      1: (Values: array [0 .. 3] of Integer);
      2: (Point1, Point2: TPointInteger);
      3: (Points: array [0 .. 1] of TPointInteger);
  end;

  PRectDouble = ^TRectDouble;

  TRectDouble = packed record
  private
    function GetCenterX: Double;
    function GetCenterY: Double;
  public
{$IFNDEF FPC}
    constructor Create(X1, Y1, X2, Y2: Double); overload;
    constructor Create(Rect: TRectDouble); overload;
{$ENDIF}
    // operator overloads
    class operator Equal(const Lhs, Rhs: TRectDouble): Boolean;
    class operator NotEqual(const Lhs, Rhs: TRectDouble): Boolean;
    class operator Add(const Lhs, Rhs: TRectDouble): TRectDouble;
    class operator Subtract(const Lhs, Rhs: TRectDouble): TRectDouble;

    class function Zero: TRectDouble; {$IFDEF INLINE_ASM} inline; {$ENDIF} static;

    class operator Implicit(const Value: TRectInteger): TRectDouble;

    procedure Normalize;
    function Clip(R: PRectDouble): Boolean; overload;
    function Clip(var R: TRectDouble): Boolean; overload;
    function IsValid: Boolean;

    property CenterX: Double read GetCenterX;
    property CenterY: Double read GetCenterY;
    case Integer of
      0: (X1, Y1, X2, Y2: Double);
      1: (Values: array [0 .. 3] of Double);
      2: (Point1, Point2: TPointDouble);
      3: (Points: array [0 .. 1] of TPointDouble);
  end;

  PQuadDouble = ^TQuadDouble;

  TQuadDouble = packed record
    case Integer of
      0: (Values: array [0 .. 7] of Double);
      1: (Points: array [0 .. 3] of TPointDouble);
  end;

  TVertex = packed record
    X, Y: Double;
    Cmd: Byte;
  end;

  TCardinalList = class
  protected
    function GetItem(Index: Cardinal): Cardinal; virtual; abstract;
  public
    property Item[index: Cardinal]: Cardinal read GetItem; default;
  end;

function AggGetMem(out Buf: Pointer; Sz: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function AggFreeMem(var Buf: Pointer; Sz: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Deg2Rad(Deg: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Rad2Deg(Rad: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function IntersectRectangles(const R1, R2: TRectInteger): TRectInteger;
function IntersectRectanglesDouble(const R1, R2: TRectDouble): TRectDouble;

function UniteRectangles(const R1, R2: TRectInteger): TRectInteger;
function UniteRectanglesDouble(const R1, R2: TRectDouble): TRectDouble;

function IsVertex(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsDrawing(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsStop(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsMove(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsLineTo(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsMoveTo(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsCurve(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsCurve3(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsCurve4(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsEndPoly(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsClose(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsNextPoly(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsClockwise(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsCounterClockwise(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsOriented(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsClosed(CX: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function GetCloseFlag(CX: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ClearOrientation(CX: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function GetOrientation(CX: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SetOrientation(CX, O: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SwapPointers(A, B: Pointer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IntToDouble(I: Integer): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RandomMinMax(Min, Max: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectInteger(X1, Y1, X2, Y2: Integer): TRectInteger; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectInteger(Point1, Point2: TPointInteger): TRectInteger; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectDouble(X1, Y1, X2, Y2: Double): TRectDouble; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectDouble(Point1, Point2: TPointDouble): TRectDouble; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function PointInteger(X, Y: Integer): TPointInteger; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointInteger(Value: Integer): TPointInteger; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointDouble(X, Y: Double): TPointDouble; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointDouble(Value: Double): TPointDouble; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function PointIntegerOffset(Point: TPointInteger; Value: Integer): TPointInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointDoubleOffset(Point: TPointDouble; Value: Double): TPointDouble; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointIntegerScale(Point: TPointInteger; Value: Integer): TPointInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointDoubleScale(Point: TPointDouble; Value: Double): TPointDouble; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function QuadDouble(RectDouble: TRectDouble): TQuadDouble; {$IFDEF INLINE_ASM} inline; {$ENDIF}

(*
  procedure Srand_(Seed: Integer);
  function Rand_: Integer;

  procedure Srand(Seed: Integer);
  function Rand: Integer;
*)

function EnsureRange(const Value, Min, Max: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function EnsureRange(const Value, Min, Max: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function UnsignedRound(V: Double): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IntegerRound(V: Double): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SaturationIntegerRound(Limit: Integer; V: Double): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// NoP = No Operation. It's the empty function, whose purpose is only for the
// debugging, or for the piece of code where intentionaly nothing is planned
// to be.
procedure NoP;

// SHR for signed integers is differently implemented in pascal compilers
// than in C++ compilers. On the assembler level, C++ is using the SAR and
// pascal is using SHR. That gives completely different Result, when the
// number is negative. We have to be compatible with C++ implementation,
// thus instead of directly using SHR we emulate C++ solution.
function ShrInt8(I, Shift: Int8): Int8; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ShrInt16(I, Shift: Int16): Int16; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ShrInt32(I, Shift: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure Fill32Bit(var X; Count: Cardinal; var Value);

implementation

function AggGetMem(out Buf: Pointer; Sz: Cardinal): Boolean;
begin
  Result := False;
  try
    GetMem(Buf, Sz);
    Result := True;
  except
      Buf := nil;
  end;
end;

function AggFreeMem(var Buf: Pointer; Sz: Cardinal): Boolean;
begin
  if Buf = nil then
      Result := True
  else
    try
      FreeMem(Buf, Sz);
      Buf := nil;
      Result := True;
    except
        Result := False;
    end;
end;

function Deg2Rad(Deg: Double): Double;
begin
  Result := Deg * CDeg2Rad;
end;

function Rad2Deg(Rad: Double): Double;
begin
  Result := Rad * CRad2Deg;
end;

{ TPointDouble }

class operator TPointDouble.Equal(const Lhs, Rhs: TPointDouble): Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TPointDouble.NotEqual(const Lhs, Rhs: TPointDouble): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

{ TPointInteger }

class operator TPointInteger.Equal(const Lhs, Rhs: TPointInteger): Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TPointInteger.NotEqual(const Lhs, Rhs: TPointInteger): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

{ TRectInteger }

{$IFNDEF FPC}


constructor TRectInteger.Create(X1, Y1, X2, Y2: Integer);
begin
  Self.X1 := X1;
  Self.Y1 := Y1;
  Self.X2 := X2;
  Self.Y2 := Y2;
end;

constructor TRectInteger.Create(Rect: TRectInteger);
begin
  Self.X1 := Rect.X1;
  Self.Y1 := Rect.Y1;
  Self.X2 := Rect.X2;
  Self.Y2 := Rect.Y2;
end;
{$ENDIF}


class operator TRectInteger.Equal(const Lhs, Rhs: TRectInteger): Boolean;
begin
  Result := (Lhs.Point1 = Rhs.Point1) and (Lhs.Point2 = Rhs.Point2)
end;

class operator TRectInteger.NotEqual(const Lhs, Rhs: TRectInteger): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TRectInteger.Add(const Lhs, Rhs: TRectInteger): TRectInteger;
begin
  Result.X1 := Lhs.X1 + Rhs.X1;
  Result.Y1 := Lhs.Y1 + Rhs.Y1;
  Result.X2 := Lhs.X2 + Rhs.X2;
  Result.Y2 := Lhs.Y2 + Rhs.Y2;
end;

class operator TRectInteger.Subtract(const Lhs, Rhs: TRectInteger): TRectInteger;
begin
  Result.X1 := Lhs.X1 - Rhs.X1;
  Result.Y1 := Lhs.Y1 - Rhs.Y1;
  Result.X2 := Lhs.X2 - Rhs.X2;
  Result.Y2 := Lhs.Y2 - Rhs.Y2;
end;

class function TRectInteger.Zero: TRectInteger;
begin
  FillChar(Result, 16, 0);
end;

function TRectInteger.Clip(var Rect: TRectInteger): Boolean;
begin
  if X2 > Rect.X2 then
      X2 := Rect.X2;

  if Y2 > Rect.Y2 then
      Y2 := Rect.Y2;

  if X1 < Rect.X1 then
      X1 := Rect.X1;

  if Y1 < Rect.Y1 then
      Y1 := Rect.Y1;

  Result := (X1 <= X2) and (Y1 <= Y2);
end;

function TRectInteger.IsValid: Boolean;
begin
  Result := (X1 <= X2) and (Y1 <= Y2);
end;

procedure TRectInteger.Normalize;
var
  T: Integer;
begin
  if X1 > X2 then
    begin
      T := X1;
      X1 := X2;
      X2 := T;
    end;

  if Y1 > Y2 then
    begin
      T := Y1;
      Y1 := Y2;
      Y2 := T;
    end;
end;

{ TRectDouble }

{$IFNDEF FPC}


constructor TRectDouble.Create(X1, Y1, X2, Y2: Double);
begin
  Self.X1 := X1;
  Self.Y1 := Y1;
  Self.X2 := X2;
  Self.Y2 := Y2;
end;

constructor TRectDouble.Create(Rect: TRectDouble);
begin
  Self.X1 := Rect.X1;
  Self.Y1 := Rect.Y1;
  Self.X2 := Rect.X2;
  Self.Y2 := Rect.Y2;
end;
{$ENDIF}


function TRectDouble.GetCenterX: Double;
begin
  Result := 0.5 * (X1 + X2);
end;

function TRectDouble.GetCenterY: Double;
begin
  Result := 0.5 * (Y1 + Y2);
end;

function TRectInteger.GetHeight: Integer;
begin
  Result := Abs(X2 - X1);
end;

function TRectInteger.GetWidth: Integer;
begin
  Result := Abs(Y2 - Y1);
end;

procedure TRectDouble.Normalize;
var
  T: Double;
begin
  if X1 > X2 then
    begin
      T := X1;
      X1 := X2;
      X2 := T;
    end;

  if Y1 > Y2 then
    begin
      T := Y1;
      Y1 := Y2;
      Y2 := T;
    end;
end;

class operator TRectDouble.Equal(const Lhs, Rhs: TRectDouble): Boolean;
begin
  Result := (Lhs.Point1 = Rhs.Point1) and (Lhs.Point2 = Rhs.Point2)
end;

class operator TRectDouble.NotEqual(const Lhs, Rhs: TRectDouble): Boolean;
begin
  Result := (Lhs.Point1 <> Rhs.Point1) or (Lhs.Point2 <> Rhs.Point2)
end;

class operator TRectDouble.Subtract(const Lhs, Rhs: TRectDouble): TRectDouble;
begin
  Result.X1 := Lhs.X1 - Rhs.X1;
  Result.Y1 := Lhs.Y1 - Rhs.Y1;
  Result.X2 := Lhs.X2 - Rhs.X2;
  Result.Y2 := Lhs.Y2 - Rhs.Y2;
end;

class function TRectDouble.Zero: TRectDouble;
begin
  FillChar(Result, 32, 4);
end;

function TRectDouble.Clip(R: PRectDouble): Boolean;
begin
  if X2 > R.X2 then
      X2 := R.X2;

  if Y2 > R.Y2 then
      Y2 := R.Y2;

  if X1 < R.X1 then
      X1 := R.X1;

  if Y1 < R.Y1 then
      Y1 := R.Y1;

  Result := (X1 <= X2) and (Y1 <= Y2);
end;

class operator TRectDouble.Add(const Lhs, Rhs: TRectDouble): TRectDouble;
begin
  Result.X1 := Lhs.X1 + Rhs.X1;
  Result.Y1 := Lhs.Y1 + Rhs.Y1;
  Result.X2 := Lhs.X2 + Rhs.X2;
  Result.Y2 := Lhs.Y2 + Rhs.Y2;
end;

function TRectDouble.Clip(var R: TRectDouble): Boolean;
begin
  Result := Clip(@R)
end;

class operator TRectDouble.Implicit(const Value: TRectInteger): TRectDouble;
begin
  Result.X1 := Value.X1;
  Result.Y1 := Value.Y1;
  Result.X2 := Value.X2;
  Result.Y2 := Value.Y2;
end;

function TRectDouble.IsValid;
begin
  Result := (X1 <= X2) and (Y1 <= Y2);
end;

{ TVertex }

procedure NormalizeRect(var This: TRectInteger);
var
  T: Integer;
begin
  if This.X1 > This.X2 then
    begin
      T := This.X1;
      This.X1 := This.X2;
      This.X2 := T;
    end;

  if This.Y1 > This.Y2 then
    begin
      T := This.Y1;
      This.Y1 := This.Y2;
      This.Y2 := T;
    end;
end;

procedure NormalizeRectDouble(var This: TRectDouble);
var
  T: Double;
begin
  if This.X1 > This.X2 then
    begin
      T := This.X1;
      This.X1 := This.X2;
      This.X2 := T;
    end;

  if This.Y1 > This.Y2 then
    begin
      T := This.Y1;
      This.Y1 := This.Y2;
      This.Y2 := T;
    end;
end;

function ClipRect(var This: TRectInteger; R: PRectInteger): Boolean;
begin
  if This.X2 > R.X2 then
      This.X2 := R.X2;

  if This.Y2 > R.Y2 then
      This.Y2 := R.Y2;

  if This.X1 < R.X1 then
      This.X1 := R.X1;

  if This.Y1 < R.Y1 then
      This.Y1 := R.Y1;

  Result := (This.X1 <= This.X2) and (This.Y1 <= This.Y2);
end;

function ClipRectDouble(var This: TRectDouble; R: PRectDouble): Boolean;
begin
  if This.X2 > R.X2 then
      This.X2 := R.X2;

  if This.Y2 > R.Y2 then
      This.Y2 := R.Y2;

  if This.X1 < R.X1 then
      This.X1 := R.X1;

  if This.Y1 < R.Y1 then
      This.Y1 := R.Y1;

  Result := (This.X1 <= This.X2) and (This.Y1 <= This.Y2);
end;

function IsValidRect(var This: TRectInteger): Boolean;
begin
  Result := (This.X1 <= This.X2) and (This.Y1 <= This.Y2);
end;

function IsValidRectDouble(var This: TRectDouble): Boolean;
begin
  Result := (This.X1 <= This.X2) and (This.Y1 <= This.Y2);
end;

function IntersectRectangles(const R1, R2: TRectInteger): TRectInteger;
begin
  Result := R1;

  if Result.X2 > R2.X2 then
      Result.X2 := R2.X2;

  if Result.Y2 > R2.Y2 then
      Result.Y2 := R2.Y2;

  if Result.X1 < R2.X1 then
      Result.X1 := R2.X1;

  if Result.Y1 < R2.Y1 then
      Result.Y1 := R2.Y1;
end;

function IntersectRectanglesDouble(const R1, R2: TRectDouble): TRectDouble;
begin
  Result := R1;

  if Result.X2 > R2.X2 then
      Result.X2 := R2.X2;

  if Result.Y2 > R2.Y2 then
      Result.Y2 := R2.Y2;

  if Result.X1 < R2.X1 then
      Result.X1 := R2.X1;

  if Result.Y1 < R2.Y1 then
      Result.Y1 := R2.Y1;
end;

function UniteRectangles(const R1, R2: TRectInteger): TRectInteger;
begin
  Result := R1;

  if Result.X2 < R2.X2 then
      Result.X2 := R2.X2;

  if Result.Y2 < R2.Y2 then
      Result.Y2 := R2.Y2;

  if Result.X1 > R2.X1 then
      Result.X1 := R2.X1;

  if Result.Y1 > R2.Y1 then
      Result.Y1 := R2.Y1;
end;

function UniteRectanglesDouble(const R1, R2: TRectDouble): TRectDouble;
begin
  Result := R1;

  if Result.X2 < R2.X2 then
      Result.X2 := R2.X2;

  if Result.Y2 < R2.Y2 then
      Result.Y2 := R2.Y2;

  if Result.X1 > R2.X1 then
      Result.X1 := R2.X1;

  if Result.Y1 > R2.Y1 then
      Result.Y1 := R2.Y1;
end;

function IsVertex(CX: Cardinal): Boolean;
begin
  Result := (CX >= CAggPathCmdMoveTo) and (CX < CAggPathCmdEndPoly);
end;

function IsDrawing(CX: Cardinal): Boolean;
begin
  Result := (CX >= CAggPathCmdLineTo) and (CX < CAggPathCmdEndPoly);
end;

function IsStop(CX: Cardinal): Boolean;
begin
  Result := (CX = CAggPathCmdStop);
end;

function IsMove(CX: Cardinal): Boolean;
begin
  Result := (CX = CAggPathCmdMoveTo);
end;

function IsLineTo(CX: Cardinal): Boolean;
begin
  Result := (CX = CAggPathCmdLineTo);
end;

function IsMoveTo(CX: Cardinal): Boolean;
begin
  Result := (CX = CAggPathCmdMoveTo);
end;

function IsCurve(CX: Cardinal): Boolean;
begin
  Result := (CX = CAggPathCmdCurve3) or (CX = CAggPathCmdCurve4);
end;

function IsCurve3(CX: Cardinal): Boolean;
begin
  Result := (CX = CAggPathCmdCurve3);
end;

function IsCurve4(CX: Cardinal): Boolean;
begin
  Result := (CX = CAggPathCmdCurve4);
end;

function IsEndPoly(CX: Cardinal): Boolean;
begin
  Result := ((CX and CAggPathCmdMask) = CAggPathCmdEndPoly);
end;

function IsClose(CX: Cardinal): Boolean;
begin
  Result := (CX and not(CAggPathFlagsCw or CAggPathFlagsCcw))
    = (CAggPathCmdEndPoly or CAggPathFlagsClose)
end;

function IsNextPoly(CX: Cardinal): Boolean;
begin
  Result := IsStop(CX) or IsMoveTo(CX) or IsEndPoly(CX);
end;

function IsClockwise(CX: Cardinal): Boolean;
begin
  Result := not((CX and CAggPathFlagsCw) = 0);
end;

function IsCounterClockwise(CX: Cardinal): Boolean;
begin
  Result := not((CX and CAggPathFlagsCcw) = 0);
end;

function IsOriented(CX: Cardinal): Boolean;
begin
  Result := not((CX and (CAggPathFlagsCw or CAggPathFlagsCcw)) = 0);
end;

function IsClosed(CX: Cardinal): Boolean;
begin
  Result := not((CX and CAggPathFlagsClose) = 0);
end;

function GetCloseFlag(CX: Cardinal): Cardinal;
begin
  Result := CX and CAggPathFlagsClose;
end;

function ClearOrientation(CX: Cardinal): Cardinal;
begin
  Result := CX and not(CAggPathFlagsCw or CAggPathFlagsCcw);
end;

function GetOrientation(CX: Cardinal): Cardinal;
begin
  Result := CX and (CAggPathFlagsCw or CAggPathFlagsCcw);
end;

function SetOrientation(CX, O: Cardinal): Cardinal;
begin
  Result := ClearOrientation(CX) or O;
end;

procedure SwapPointers(A, B: Pointer);
var
  Temp: Pointer;
begin
  Temp := PPointer(A)^;
  PPointer(A)^ := PPointer(B)^;
  PPointer(B)^ := Temp;
end;

function IntToDouble(I: Integer): Double;
begin
  Result := I;
end;

function RandomMinMax(Min, Max: Double): Double;
begin
  Result := (Max - Min) * Random + Min;
end;

function RectInteger(X1, Y1, X2, Y2: Integer): TRectInteger;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

function RectInteger(Point1, Point2: TPointInteger): TRectInteger;
begin
  Result.Point1 := Point1;
  Result.Point2 := Point2;
end;

function RectDouble(X1, Y1, X2, Y2: Double): TRectDouble;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

function RectDouble(Point1, Point2: TPointDouble): TRectDouble;
begin
  Result.Point1 := Point1;
  Result.Point2 := Point2;
end;

function PointInteger(X, Y: Integer): TPointInteger;
begin
  Result.X := X;
  Result.Y := Y;
end;

function PointInteger(Value: Integer): TPointInteger;
begin
  Result.X := Value;
  Result.Y := Value;
end;

function PointDouble(X, Y: Double): TPointDouble;
begin
  Result.X := X;
  Result.Y := Y;
end;

function PointDouble(Value: Double): TPointDouble;
begin
  Result.X := Value;
  Result.Y := Value;
end;

function PointIntegerOffset(Point: TPointInteger; Value: Integer)
  : TPointInteger;
begin
  Result.X := Point.X + Value;
  Result.Y := Point.Y + Value;
end;

function PointDoubleOffset(Point: TPointDouble; Value: Double): TPointDouble;
begin
  Result.X := Point.X + Value;
  Result.Y := Point.Y + Value;
end;

function PointIntegerScale(Point: TPointInteger; Value: Integer): TPointInteger;
begin
  Result.X := Point.X * Value;
  Result.Y := Point.Y * Value;
end;

function PointDoubleScale(Point: TPointDouble; Value: Double): TPointDouble;
begin
  Result.X := Point.X * Value;
  Result.Y := Point.Y * Value;
end;

function QuadDouble(RectDouble: TRectDouble): TQuadDouble;
begin
  Result.Points[0] := RectDouble.Point1;
  Result.Values[2] := RectDouble.X2;
  Result.Values[3] := RectDouble.Y1;
  Result.Points[2] := RectDouble.Point2;
  Result.Values[5] := RectDouble.Y2;
  Result.Values[6] := RectDouble.X1;
  Result.Values[7] := RectDouble.Y2;
end;

function EnsureRange(const Value, Min, Max: Integer): Integer;
begin
  Result := Value;
  if Result < Min then
      Result := Min;
  if Result > Max then
      Result := Max;
end;

function EnsureRange(const Value, Min, Max: Double): Double;
begin
  Result := Value;
  if Result < Min then
      Result := Min;
  if Result > Max then
      Result := Max;
end;

function UnsignedRound(V: Double): Cardinal;
begin
  Result := Cardinal(Trunc(V + 0.5));
end;

function IntegerRound(V: Double): Integer;
begin
  if V < 0.0 then
      Result := Integer(Trunc(V - 0.5))
  else
      Result := Integer(Trunc(V + 0.5));
end;

function SaturationIntegerRound(Limit: Integer; V: Double): Integer;
begin
  if V < -Limit then
      Result := -Limit
  else if V > Limit then
      Result := Limit
  else
      Result := IntegerRound(V);
end;

procedure NoP;
begin
end;

function ShrInt8(I, Shift: Int8): Int8;
begin
  Result := I div (1 shl Shift);
end;

function ShrInt16(I, Shift: Int16): Int16;
begin
  Result := I div (1 shl Shift);
end;

function ShrInt32(I, Shift: Integer): Integer;
begin
  Result := I div (1 shl Shift);
end;

procedure Fill32Bit(var X; Count: Cardinal; var Value);
var
  I: Integer;
  P: PIntegerArray;
begin
  P := PIntegerArray(@X);
  for I := Count - 1 downto 0 do
      P[I] := Integer(Value);
end;

end.
