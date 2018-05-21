{ ****************************************************************************** }
{ * k-means++ clusterization library  written by QQ 600585@qq.com              * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit KM;

interface

uses CoreClasses;

{$I zDefine.inc}


type
  TKMInt = Integer;
  PKMInt = ^TKMInt;

  TDynamicIndexArray = packed array of TKMInt;
  PDynamicIndexArray = ^TDynamicIndexArray;

  TKMBoolArray = packed array of Boolean;
  PKMBoolArray = ^TKMBoolArray;

  TKMFloat = Double;
  PKMFloat = ^TKMFloat;

  TKMIntegerArray = TDynamicIndexArray;
  PKMIntegerArray = PDynamicIndexArray;

  TKMFloatArray = packed array of TKMFloat;
  PKMFloatArray = ^TKMFloatArray;

  TKMFloat2DArray = packed array of TKMFloatArray;
  PKMFloat2DArray = ^TKMFloat2DArray;

  (*
    k-means++ clusterization
    return state:
    * -3, if task is degenerate (number of distinct points is less than K)
    * -1, if incorrect NPoints/NFeatures/K/Restarts was passed
    *  1, if subroutine finished successfully
  *)
function KMeansCluster(const Source: TKMFloat2DArray; const NVars, k, Restarts: NativeInt; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): ShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation

uses Math, Learn;

procedure ArrayMove(VDst: PKMFloat; const i11, i12: NativeInt; VSrc: PKMFloat; const i21, i22: NativeInt); inline; overload;
var
  i: NativeInt;
begin
  Inc(VDst, i11);
  Inc(VSrc, i21);

  for i := 0 to i12 - i11 do
    begin
      VDst^ := VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure ArrayMove(VDst: PKMFloat; const i11, i12: NativeInt; VSrc: PKMFloat; const i21, i22: NativeInt; const f: TKMFloat); inline; overload;
var
  i: NativeInt;
begin
  Inc(VDst, i11);
  Inc(VSrc, i21);

  for i := 0 to i12 - i11 do
    begin
      VDst^ := f * VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure ArraySub(VDst: PKMFloat; const i11, i12: NativeInt; VSrc: PKMFloat; const i21, i22: NativeInt); inline;
var
  i: NativeInt;
begin
  Inc(VDst, i11);
  Inc(VSrc, i21);

  for i := 0 to i12 - i11 do
    begin
      VDst^ := VDst^ - VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

function ArrayDotProduct(V1: PKMFloat; const i11, i12: NativeInt; V2: PKMFloat; const i21, i22: NativeInt): TKMFloat; inline;
var
  i: NativeInt;
begin
  Inc(V1, i11);
  Inc(V2, i21);

  Result := 0;
  for i := 0 to i12 - i11 do
    begin
      Result := Result + (V1^ * V2^);
      Inc(V1);
      Inc(V2);
    end;
end;

procedure ArrayAdd(VDst: PKMFloat; const i11, i12: NativeInt; VSrc: PKMFloat; const i21, i22: NativeInt); inline;
var
  i: NativeInt;
begin
  Inc(VDst, i11);
  Inc(VSrc, i21);

  for i := 0 to i12 - i11 do
    begin
      VDst^ := VDst^ + VSrc^;
      Inc(VDst);
      Inc(VSrc);
    end;
end;

procedure ArrayMul(VOp: PKMFloat; const I1, I2: NativeInt; const f: TKMFloat); inline;
var
  i: NativeInt;
begin
  Inc(VOp, I1);
  for i := 0 to I2 - I1 do
    begin
      VOp^ := f * VOp^;
      Inc(VOp);
    end;
end;

procedure CopyMatrix(const A: TKMFloat2DArray; const IS1, IS2, JS1, JS2: NativeInt; var B: TKMFloat2DArray; const ID1, ID2, JD1, JD2: NativeInt); inline;
var
  isrc, idst: NativeInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  isrc := IS1;
  while isrc <= IS2 do
    begin
      idst := isrc - IS1 + ID1;
      ArrayMove(@B[idst][0], JD1, JD2, @A[isrc][0], JS1, JS2);
      Inc(isrc);
    end;
end;

procedure CopyAndTranspose(const A: TKMFloat2DArray; const IS1, IS2, JS1, JS2: NativeInt; var B: TKMFloat2DArray; const ID1, ID2, JD1, JD2: NativeInt); inline;
var
  isrc, jdst, i, k: NativeInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  isrc := IS1;
  while isrc <= IS2 do
    begin
      jdst := isrc - IS1 + JD1;
      k := JS1 - ID1;
      for i := ID1 to ID2 do
          B[i, jdst] := A[isrc, i + k];
      Inc(isrc);
    end;
end;

procedure DynamicArrayCopy(const Source: TKMBoolArray; var Output: TKMBoolArray); inline;
var
  i: NativeInt;
  r: TKMBoolArray;
begin
  SetLength(Output, Length(Source));
  for i := low(Source) to high(Source) do
      Output[i] := Source[i];
end;

(* ************************************************************************
  Select center for a new cluster using k-means++ rule
  ************************************************************************ *)
function SelectCenter(const Source: TKMFloat2DArray;
  const NPoints, NVars: NativeInt; var Centers: TKMFloat2DArray; const BusyCenters: TKMBoolArray; const CCnt: NativeInt;
  var D2: TKMFloatArray; var P: TKMFloatArray; var Tmp: TKMFloatArray): Boolean; inline;
var
  NewBusyCenters: TKMBoolArray;
  i: NativeInt;
  j: NativeInt;
  cc: NativeInt;
  v: TKMFloat;
  s: TKMFloat;
begin
  DynamicArrayCopy(BusyCenters, NewBusyCenters);

  Result := True;
  cc := 0;
  while cc <= CCnt - 1 do
    begin
      if not NewBusyCenters[cc] then
        begin
          i := 0;
          while i <= NPoints - 1 do
            begin
              D2[i] := MaxRealNumber;
              j := 0;
              while j <= CCnt - 1 do
                begin
                  if NewBusyCenters[j] then
                    begin
                      ArrayMove(@Tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                      ArraySub(@Tmp[0], 0, NVars - 1, @Centers[j][0], 0, NVars - 1);
                      v := ArrayDotProduct(@Tmp[0], 0, NVars - 1, @Tmp[0], 0, NVars - 1);
                      if v < D2[i] then
                          D2[i] := v;
                    end;
                  Inc(j);
                end;
              Inc(i);
            end;

          // calculate P (non-cumulative)
          s := 0;
          i := 0;
          while i <= NPoints - 1 do
            begin
              s := s + D2[i];
              Inc(i);
            end;
          if s = 0 then
            begin
              Result := False;
              Exit;
            end;
          s := 1 / s;
          ArrayMove(@P[0], 0, NPoints - 1, @D2[0], 0, NPoints - 1, s);

          // choose one of points with probability P
          // random number within (0,1) is generated and
          // inverse empirical CDF is used to randomly choose a point.
          s := 0;
          v := RandomReal();
          i := 0;
          while i <= NPoints - 1 do
            begin
              s := s + P[i];
              if (v <= s) or (i = NPoints - 1) then
                begin
                  ArrayMove(@Centers[cc][0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                  NewBusyCenters[cc] := True;
                  Break;
                end;
              Inc(i);
            end;
        end;
      Inc(cc);
    end;
end;

function KMeansCluster(const Source: TKMFloat2DArray;
  const NVars, k, Restarts: NativeInt; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): ShortInt;
var
  NPoints: NativeInt;
  i: NativeInt;
  j: NativeInt;
  ct: TKMFloat2DArray;
  CTBest: TKMFloat2DArray;
  XYCBest: TKMIntegerArray;
  E: TKMFloat;
  EBest: TKMFloat;
  X: TKMFloatArray;
  Tmp: TKMFloatArray;
  D2: TKMFloatArray;
  P: TKMFloatArray;
  CSizes: TKMIntegerArray;
  CBusy: TKMBoolArray;
  v: TKMFloat;
  CClosest: NativeInt;
  DClosest: TKMFloat;
  WORK: TKMFloatArray;
  WasChanges: Boolean;
  ZeroSizeClusters: Boolean;
  Pass: NativeInt;
begin
  NPoints := Length(Source);

  if (NPoints < k) or (NVars < 1) or (k < 1) or (Restarts < 1) then
    begin
      Result := -1;
      Exit;
    end;

  Result := 1;

  SetLength(ct, k, NVars);
  SetLength(CTBest, k, NVars);
  SetLength(kIndex, NPoints);
  SetLength(XYCBest, NPoints);
  SetLength(D2, NPoints);
  SetLength(P, NPoints);
  SetLength(Tmp, NVars);
  SetLength(CSizes, k);
  SetLength(CBusy, k);
  EBest := MaxRealNumber;
  Pass := 1;
  while Pass <= Restarts do
    begin
      // Select initial centers  using k-means++ algorithm
      // 1. Choose first center at random
      // 2. Choose next centers using their distance from centers already chosen
      //
      // Note that for performance reasons centers are stored in ROWS of CT, not
      // in columns. We'll transpose CT in the end and store it in the KArray.
      i := RandomInteger(NPoints);
      ArrayMove(@ct[0][0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
      CBusy[0] := True;
      i := 1;
      while i <= k - 1 do
        begin
          CBusy[i] := False;
          Inc(i);
        end;
      if not SelectCenter(Source, NPoints, NVars, ct, CBusy, k, D2, P, Tmp) then
        begin
          Result := -3;
          Exit;
        end;

      // Update centers:
      // 2. update center positions
      while True do
        begin
          // fill kIndex with center numbers
          WasChanges := False;
          i := 0;
          while i <= NPoints - 1 do
            begin
              CClosest := -1;
              DClosest := MaxRealNumber;
              j := 0;
              while j <= k - 1 do
                begin
                  ArrayMove(@Tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                  ArraySub(@Tmp[0], 0, NVars - 1, @ct[j][0], 0, NVars - 1);
                  v := ArrayDotProduct(@Tmp[0], 0, NVars - 1, @Tmp[0], 0, NVars - 1);
                  if v < DClosest then
                    begin
                      CClosest := j;
                      DClosest := v;
                    end;
                  Inc(j);
                end;
              if kIndex[i] <> CClosest then
                  WasChanges := True;
              kIndex[i] := CClosest;
              Inc(i);
            end;

          // Update centers
          j := 0;
          while j <= k - 1 do
            begin
              CSizes[j] := 0;
              Inc(j);
            end;
          i := 0;
          while i <= k - 1 do
            begin
              j := 0;
              while j <= NVars - 1 do
                begin
                  ct[i, j] := 0;
                  Inc(j);
                end;
              Inc(i);
            end;
          i := 0;
          while i <= NPoints - 1 do
            begin
              CSizes[kIndex[i]] := CSizes[kIndex[i]] + 1;
              ArrayAdd(@ct[kIndex[i]][0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
              Inc(i);
            end;
          ZeroSizeClusters := False;
          i := 0;
          while i <= k - 1 do
            begin
              CBusy[i] := CSizes[i] <> 0;
              ZeroSizeClusters := ZeroSizeClusters or (CSizes[i] = 0);
              Inc(i);
            end;
          if ZeroSizeClusters then
            begin
              // Some clusters have zero size - rare, but possible.
              // We'll choose new centers for such clusters using k-means++ rule
              // and restart algorithm
              if not SelectCenter(Source, NPoints, NVars, ct, CBusy, k, D2, P, Tmp) then
                begin
                  Result := -3;
                  Exit;
                end;
              Continue;
            end;
          j := 0;
          while j <= k - 1 do
            begin
              v := 1.0 / CSizes[j];
              ArrayMul(@ct[j][0], 0, NVars - 1, v);
              Inc(j);
            end;

          // if nothing has changed during iteration
          if not WasChanges then
              Break;
        end;

      // 3. Calculate E, compare with best centers found so far
      E := 0;
      i := 0;
      while i <= NPoints - 1 do
        begin
          ArrayMove(@Tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
          ArraySub(@Tmp[0], 0, NVars - 1, @ct[kIndex[i]][0], 0, NVars - 1);
          v := ArrayDotProduct(@Tmp[0], 0, NVars - 1, @Tmp[0], 0, NVars - 1);
          E := E + v;
          Inc(i);
        end;
      if E < EBest then
        begin
          // store partition.
          EBest := E;
          CopyMatrix(ct, 0, k - 1, 0, NVars - 1, CTBest, 0, k - 1, 0, NVars - 1);
          i := 0;
          while i <= NPoints - 1 do
            begin
              XYCBest[i] := kIndex[i];
              Inc(i);
            end;
        end;
      Inc(Pass);
    end;

  // Copy and transpose
  SetLength(KArray, NVars - 1 + 1, k - 1 + 1);
  CopyAndTranspose(CTBest, 0, k - 1, 0, NVars - 1, KArray, 0, NVars - 1, 0, k - 1);
  i := 0;
  while i <= NPoints - 1 do
    begin
      kIndex[i] := XYCBest[i];
      Inc(i);
    end;
end;

initialization

finalization

end.
