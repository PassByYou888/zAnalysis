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
unit AggBSpline;

interface

{$INCLUDE AggCompiler.inc}

uses
  AggBasics;

type
  // ----------------------------------------------------------------
  // A very simple class of Bi-cubic Spline interpolation.
  // First call init(num, x[], y[]) where num - number of source points,
  // x, y - arrays of X and Y values respectively. Here Y must be a function
  // of X. It means that all the X-coordinates must be arranged in the ascending
  // order.
  // Then call get(x) that calculates a value Y for the respective X.
  // The class supports extrapolation, i.e. you can call get(x) where x is
  // outside the given with init() X-range. Extrapolation is a simple linear
  // function.

  TAggBSpline = class
  private
    FMax, FNum: Integer;
    fx, fy, FSplineBuffer: PDouble;
    FLastIdx: Integer;
  protected
    procedure BSearch(n: Integer; X: PDouble; x0: Double; i: PInteger);

    function ExtrapolationLeft(X: Double): Double;
    function ExtrapolationRight(X: Double): Double;

    function Interpolation(X: Double; i: Integer): Double;
  public
    constructor Create; overload;
    constructor Create(Num: Integer); overload;
    constructor Create(Num: Integer; X, Y: PDouble); overload;
    destructor Destroy; override;

    procedure Init(Max: Integer); overload;
    procedure Init(Num: Integer; X, Y: PDouble); overload;
    procedure Init(Num: Integer; Points: PPointDouble); overload;

    procedure AddPoint(X, Y: Double);
    procedure Prepare;

    function Get(X: Double): Double;
    function GetStateful(X: Double): Double;
  end;

implementation


{ TAggBSpline }

constructor TAggBSpline.Create;
begin
  FMax := 0;
  FNum := 0;

  fx := nil;
  fy := nil;
  FSplineBuffer := nil;

  FLastIdx := -1;
end;

constructor TAggBSpline.Create(Num: Integer);
begin
  Create;

  Init(Num);
end;

constructor TAggBSpline.Create(Num: Integer; X, Y: PDouble);
begin
  Create;

  Init(Num, X, Y);
end;

destructor TAggBSpline.Destroy;
begin
  AggFreeMem(Pointer(FSplineBuffer), FMax * 3 * SizeOf(Double));

  inherited;
end;

procedure TAggBSpline.Init(Max: Integer);
begin
  if (Max > 2) and (Max > FMax) then
  begin
    AggFreeMem(Pointer(FSplineBuffer), FMax * 3 * SizeOf(Double));
    AggGetMem(Pointer(FSplineBuffer), Max * 3 * SizeOf(Double));

    FMax := Max;

    fx := PDouble(PtrComp(FSplineBuffer) + FMax * SizeOf(Double));
    fy := PDouble(PtrComp(FSplineBuffer) + FMax * 2 * SizeOf(Double));
  end;

  FNum := 0;
  FLastIdx := -1;
end;

procedure TAggBSpline.Init(Num: Integer; X, Y: PDouble);
var
  i: Integer;
begin
  if Num > 2 then
  begin
    Init(Num);

    for i := 0 to Num - 1 do
    begin
      AddPoint(X^, Y^);

      Inc(X);
      Inc(Y);
    end;

    Prepare;
  end;

  FLastIdx := -1;
end;

procedure TAggBSpline.Init(Num: Integer; Points: PPointDouble);
var
  i: Integer;
begin
  if Num > 2 then
  begin
    Init(Num);

    for i := 0 to Num - 1 do
    begin
      AddPoint(Points^.X, Points^.Y);

      Inc(Points);
    end;

    Prepare;
  end;

  FLastIdx := -1;
end;

procedure TAggBSpline.AddPoint(X, Y: Double);
begin
  if FNum < FMax then
  begin
    PDouble(PtrComp(fx) + FNum * SizeOf(Double))^ := X;
    PDouble(PtrComp(fy) + FNum * SizeOf(Double))^ := Y;

    Inc(FNum);
  end;
end;

procedure TAggBSpline.Prepare;
var
  i, k, N1, SZ  : Integer;
  Temp, R, s, al: PDouble;
  h, p, d, F, E : Double;
begin
  if FNum > 2 then
  begin
    for k := 0 to FNum - 1 do
      PDouble(PtrComp(FSplineBuffer) + k * SizeOf(Double))^ := 0;

    N1 := 3 * FNum;
    SZ := N1;

    AggGetMem(Pointer(al), N1 * SizeOf(Double));

    Temp := al;

    for k := 0 to N1 - 1 do
      PDouble(PtrComp(Temp) + k * SizeOf(Double))^ := 0;

    R := PDouble(PtrComp(Temp) + FNum * SizeOf(Double));
    s := PDouble(PtrComp(Temp) + FNum * 2 * SizeOf(Double));

    N1 := FNum - 1;
    d := PDouble(PtrComp(fx) + SizeOf(Double))^ - fx^;
    E := (PDouble(PtrComp(fy) + SizeOf(Double))^ - fy^) / d;

    k := 1;

    while k < N1 do
    begin
      h := d;
      d := PDouble(PtrComp(fx) + (k + 1) * SizeOf(Double))^ -
        PDouble(PtrComp(fx) + k * SizeOf(Double))^;
      F := E;
      E := (PDouble(PtrComp(fy) + (k + 1) * SizeOf(Double))^ -
        PDouble(PtrComp(fy) + k * SizeOf(Double))^) / d;

      PDouble(PtrComp(al) + k * SizeOf(Double))^ := d / (d + h);
      PDouble(PtrComp(R) + k * SizeOf(Double))^ :=
        1.0 - PDouble(PtrComp(al) + k * SizeOf(Double))^;
      PDouble(PtrComp(s) + k * SizeOf(Double))^ := 6.0 * (E - F) / (h + d);

      Inc(k);
    end;

    k := 1;

    while k < N1 do
    begin
      p := 1.0 / (PDouble(PtrComp(R) + k * SizeOf(Double))^ *
        PDouble(PtrComp(al) + (k - 1) * SizeOf(Double))^ + 2.0);

      PDouble(PtrComp(al) + k * SizeOf(Double))^ :=
        PDouble(PtrComp(al) + k * SizeOf(Double))^ * -p;

      PDouble(PtrComp(s) + k * SizeOf(Double))^ :=
        (PDouble(PtrComp(s) + k * SizeOf(Double))^ -
        PDouble(PtrComp(R) + k * SizeOf(Double))^ *
        PDouble(PtrComp(s) + (k - 1) * SizeOf(Double))^) * p;

      Inc(k);
    end;

    PDouble(PtrComp(FSplineBuffer) + N1 * SizeOf(Double))^ := 0.0;

    PDouble(PtrComp(al) + (N1 - 1) * SizeOf(Double))^ :=
      PDouble(PtrComp(s) + (N1 - 1) * SizeOf(Double))^;

    PDouble(PtrComp(FSplineBuffer) + (N1 - 1) * SizeOf(Double))^ :=
      PDouble(PtrComp(al) + (N1 - 1) * SizeOf(Double))^;

    k := N1 - 2;
    i := 0;

    while i < FNum - 2 do
    begin
      PDouble(PtrComp(al) + k * SizeOf(Double))^ :=
        PDouble(PtrComp(al) + k * SizeOf(Double))^ *
        PDouble(PtrComp(al) + (k + 1) * SizeOf(Double))^ +
        PDouble(PtrComp(s) + k * SizeOf(Double))^;

      PDouble(PtrComp(FSplineBuffer) + k * SizeOf(Double))^ :=
        PDouble(PtrComp(al) + k * SizeOf(Double))^;

      Inc(i);
      Dec(k);
    end;

    AggFreeMem(Pointer(al), SZ * SizeOf(Double));
  end;

  FLastIdx := -1;
end;

function TAggBSpline.Get(X: Double): Double;
var
  i: Integer;
begin
  if FNum > 2 then
  begin
    // Extrapolation on the left
    if X < fx^ then
    begin
      Result := ExtrapolationLeft(X);

      Exit;
    end;

    // Extrapolation on the right
    if X >= PDouble(PtrComp(fx) + (FNum - 1) * SizeOf(Double))^ then
    begin
      Result := ExtrapolationRight(X);

      Exit;
    end;

    // Interpolation
    BSearch(FNum, fx, X, @i);

    Result := Interpolation(X, i);

    Exit;
  end;

  Result := 0.0;
end;

function TAggBSpline.GetStateful(X: Double): Double;
begin
  if FNum > 2 then
  begin
    // Extrapolation on the left
    if X < fx^ then
    begin
      Result := ExtrapolationLeft(X);

      Exit;
    end;

    // Extrapolation on the right
    if X >= PDouble(PtrComp(fx) + (FNum - 1) * SizeOf(Double))^ then
    begin
      Result := ExtrapolationRight(X);

      Exit;
    end;

    if FLastIdx >= 0 then
    begin
      // Check if x is not in current range
      if (X < PDouble(PtrComp(fx) + FLastIdx * SizeOf(Double))^) or
        (X > PDouble(PtrComp(fx) + (FLastIdx + 1) * SizeOf(Double))^) then
        // Check if x between next points (most probably)
        if (FLastIdx < FNum - 2) and
          (X >= PDouble(PtrComp(fx) + (FLastIdx + 1) * SizeOf(Double))^)
          and (X <= PDouble(PtrComp(fx) + (FLastIdx + 2) *
          SizeOf(Double))^) then
          Inc(FLastIdx)
        else if (FLastIdx > 0) and
          (X >= PDouble(PtrComp(fx) + (FLastIdx - 1) * SizeOf(Double))^)
          and (X <= PDouble(PtrComp(fx) + FLastIdx * SizeOf(Double))^)
        then
          // x is between pevious points
          Dec(FLastIdx)
        else
          // Else perform full search
          BSearch(FNum, fx, X, @FLastIdx);

      Result := Interpolation(X, FLastIdx);

      Exit;
    end
    else
    begin
      // Interpolation
      BSearch(FNum, fx, X, @FLastIdx);

      Result := Interpolation(X, FLastIdx);

      Exit;
    end;
  end;

  Result := 0.0;
end;

procedure TAggBSpline.BSearch(n: Integer; X: PDouble; x0: Double; i: PInteger);
var
  J, k: Integer;
begin
  J := n - 1;
  i^ := 0;

  while J - i^ > 1 do
  begin
    k := ShrInt32(i^ + J, 1);

    if x0 < PDouble(PtrComp(X) + k * SizeOf(Double))^ then
      J := k
    else
      i^ := k;
  end;
end;

function TAggBSpline.ExtrapolationLeft(X: Double): Double;
var
  d: Double;
begin
  d := PDouble(PtrComp(fx) + SizeOf(Double))^ - fx^;

  Result := (-d * PDouble(PtrComp(FSplineBuffer) + SizeOf(Double))^ / 6 +
    (PDouble(PtrComp(fy) + SizeOf(Double))^ - fy^) / d) *
    (X - fx^) + fy^;
end;

function TAggBSpline.ExtrapolationRight(X: Double): Double;
var
  d: Double;
begin
  d := PDouble(PtrComp(fx) + (FNum - 1) * SizeOf(Double))^ -
    PDouble(PtrComp(fx) + (FNum - 2) * SizeOf(Double))^;

  Result := (d * PDouble(PtrComp(FSplineBuffer) + (FNum - 2) * SizeOf(Double))^ / 6 +
    (PDouble(PtrComp(fy) + (FNum - 1) * SizeOf(Double))^ -
    PDouble(PtrComp(fy) + (FNum - 2) * SizeOf(Double))^) / d) *
    (X - PDouble(PtrComp(fx) + (FNum - 1) * SizeOf(Double))^) +
    PDouble(PtrComp(fy) + (FNum - 1) * SizeOf(Double))^;
end;

function TAggBSpline.Interpolation(X: Double; i: Integer): Double;
var
  J: Integer;
  d, h, R, p: Double;
begin
  J := i + 1;
  d := PDouble(PtrComp(fx) + i * SizeOf(Double))^ -
    PDouble(PtrComp(fx) + J * SizeOf(Double))^;
  h := X - PDouble(PtrComp(fx) + J * SizeOf(Double))^;
  R := PDouble(PtrComp(fx) + i * SizeOf(Double))^ - X;
  p := d * d / 6.0;

  Result := (PDouble(PtrComp(FSplineBuffer) + J * SizeOf(Double))^ * R * R * R +
    PDouble(PtrComp(FSplineBuffer) + i * SizeOf(Double))^ * h * h * h) / 6.0 / d +
    ((PDouble(PtrComp(fy) + J * SizeOf(Double))^ - PDouble(PtrComp(FSplineBuffer)
    + J * SizeOf(Double))^ * p) * R +
    (PDouble(PtrComp(fy) + i * SizeOf(Double))^ - PDouble(PtrComp(FSplineBuffer) +
    i * SizeOf(Double))^ * p) * h) / d;
end;

end. 
