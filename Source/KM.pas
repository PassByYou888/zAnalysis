{ ****************************************************************************** }
{ * k-means++ clusterization library  written by QQ 600585@qq.com              * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit KM;

interface

uses CoreClasses;

{$INCLUDE zDefine.inc}


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
function KMeansCluster(const Source: TKMFloat2DArray; const NVars, k, Restarts: nativeInt; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): ShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation

uses Math, Learn;

procedure ArrayMove(VDst: PKMFloat; const i11, i12: nativeInt; vSrc: PKMFloat; const i21, i22: nativeInt); inline; overload;
var
  i: nativeInt;
begin
  Inc(VDst, i11);
  Inc(vSrc, i21);

  for i := i12 - i11 downto 0 do
    begin
      VDst^ := vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure ArrayMove(VDst: PKMFloat; const i11, i12: nativeInt; vSrc: PKMFloat; const i21, i22: nativeInt; const F: TKMFloat); inline; overload;
var
  i: nativeInt;
begin
  Inc(VDst, i11);
  Inc(vSrc, i21);

  for i := i12 - i11 downto 0 do
    begin
      VDst^ := F * vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure ArraySub(VDst: PKMFloat; const i11, i12: nativeInt; vSrc: PKMFloat; const i21, i22: nativeInt); inline;
var
  i: nativeInt;
begin
  Inc(VDst, i11);
  Inc(vSrc, i21);

  for i := i12 - i11 downto 0 do
    begin
      VDst^ := VDst^ - vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

function ArrayDotProduct(v1: PKMFloat; const i11, i12: nativeInt; v2: PKMFloat; const i21, i22: nativeInt): TKMFloat; inline;
var
  i: nativeInt;
begin
  Inc(v1, i11);
  Inc(v2, i21);

  Result := 0;
  for i := i12 - i11 downto 0 do
    begin
      Result := Result + (v1^ * v2^);
      Inc(v1);
      Inc(v2);
    end;
end;

procedure ArrayAdd(VDst: PKMFloat; const i11, i12: nativeInt; vSrc: PKMFloat; const i21, i22: nativeInt); inline;
var
  i: nativeInt;
begin
  Inc(VDst, i11);
  Inc(vSrc, i21);

  for i := i12 - i11 downto 0 do
    begin
      VDst^ := VDst^ + vSrc^;
      Inc(VDst);
      Inc(vSrc);
    end;
end;

procedure ArrayMul(VOp: PKMFloat; const i1, i2: nativeInt; const F: TKMFloat); inline;
var
  i: nativeInt;
begin
  Inc(VOp, i1);
  for i := i2 - i1 downto 0 do
    begin
      VOp^ := F * VOp^;
      Inc(VOp);
    end;
end;

procedure CopyMatrix(const A: TKMFloat2DArray; const IS1, IS2, JS1, JS2: nativeInt; var b: TKMFloat2DArray; const ID1, id2, JD1, JD2: nativeInt); inline;
var
  isrc, idst: nativeInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  isrc := IS1;
  while isrc <= IS2 do
    begin
      idst := isrc - IS1 + ID1;
      ArrayMove(@b[idst][0], JD1, JD2, @A[isrc][0], JS1, JS2);
      Inc(isrc);
    end;
end;

procedure CopyAndTranspose(const A: TKMFloat2DArray; const IS1, IS2, JS1, JS2: nativeInt; var b: TKMFloat2DArray; const ID1, id2, JD1, JD2: nativeInt); inline;
var
  isrc, jdst, i, k: nativeInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  isrc := IS1;
  while isrc <= IS2 do
    begin
      jdst := isrc - IS1 + JD1;
      k := JS1 - ID1;
      for i := ID1 to id2 do
          b[i, jdst] := A[isrc, i + k];
      Inc(isrc);
    end;
end;

procedure DynamicArrayCopy(const Source: TKMBoolArray; var output: TKMBoolArray); inline;
var
  i: nativeInt;
  R: TKMBoolArray;
begin
  SetLength(output, length(Source));
  for i := low(Source) to high(Source) do
      output[i] := Source[i];
end;

(* ************************************************************************
  Select center for a new cluster using k-means++ rule
  ************************************************************************ *)
function SelectCenter(const Source: TKMFloat2DArray;
  const NPoints, NVars: nativeInt; var Centers: TKMFloat2DArray; const BusyCenters: TKMBoolArray; const CCnt: nativeInt;
  var d2: TKMFloatArray; var p: TKMFloatArray; var tmp: TKMFloatArray): Boolean; inline;
var
  NewBusyCenters: TKMBoolArray;
  i: nativeInt;
  J: nativeInt;
  CC: nativeInt;
  v: TKMFloat;
  s: TKMFloat;
begin
  DynamicArrayCopy(BusyCenters, NewBusyCenters);

  Result := True;
  CC := 0;
  while CC <= CCnt - 1 do
    begin
      if not NewBusyCenters[CC] then
        begin
          i := 0;
          while i <= NPoints - 1 do
            begin
              d2[i] := MaxRealNumber;
              J := 0;
              while J <= CCnt - 1 do
                begin
                  if NewBusyCenters[J] then
                    begin
                      ArrayMove(@tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                      ArraySub(@tmp[0], 0, NVars - 1, @Centers[J][0], 0, NVars - 1);
                      v := ArrayDotProduct(@tmp[0], 0, NVars - 1, @tmp[0], 0, NVars - 1);
                      if v < d2[i] then
                          d2[i] := v;
                    end;
                  Inc(J);
                end;
              Inc(i);
            end;

          // calculate P (non-cumulative)
          s := 0;
          i := 0;
          while i <= NPoints - 1 do
            begin
              s := s + d2[i];
              Inc(i);
            end;
          if s = 0 then
            begin
              Result := False;
              Exit;
            end;
          s := 1 / s;
          ArrayMove(@p[0], 0, NPoints - 1, @d2[0], 0, NPoints - 1, s);

          // choose one of points with probability P
          // random number within (0,1) is generated and
          // inverse empirical CDF is used to randomly choose a point.
          s := 0;
          v := RandomReal();
          i := 0;
          while i <= NPoints - 1 do
            begin
              s := s + p[i];
              if (v <= s) or (i = NPoints - 1) then
                begin
                  ArrayMove(@Centers[CC][0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                  NewBusyCenters[CC] := True;
                  Break;
                end;
              Inc(i);
            end;
        end;
      Inc(CC);
    end;
end;

function KMeansCluster(const Source: TKMFloat2DArray;
  const NVars, k, Restarts: nativeInt; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): ShortInt;
var
  NPoints: nativeInt;
  i: nativeInt;
  J: nativeInt;
  ct: TKMFloat2DArray;
  CTBest: TKMFloat2DArray;
  XYCBest: TKMIntegerArray;
  E: TKMFloat;
  EBest: TKMFloat;
  X: TKMFloatArray;
  tmp: TKMFloatArray;
  d2: TKMFloatArray;
  p: TKMFloatArray;
  CSizes: TKMIntegerArray;
  CBusy: TKMBoolArray;
  v: TKMFloat;
  CClosest: nativeInt;
  DClosest: TKMFloat;
  Work: TKMFloatArray;
  WasChanges: Boolean;
  ZeroSizeClusters: Boolean;
  pass: nativeInt;
begin
  NPoints := length(Source);

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
  SetLength(d2, NPoints);
  SetLength(p, NPoints);
  SetLength(tmp, NVars);
  SetLength(CSizes, k);
  SetLength(CBusy, k);
  EBest := MaxRealNumber;
  pass := 1;
  while pass <= Restarts do
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
      if not SelectCenter(Source, NPoints, NVars, ct, CBusy, k, d2, p, tmp) then
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
              J := 0;
              while J <= k - 1 do
                begin
                  ArrayMove(@tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                  ArraySub(@tmp[0], 0, NVars - 1, @ct[J][0], 0, NVars - 1);
                  v := ArrayDotProduct(@tmp[0], 0, NVars - 1, @tmp[0], 0, NVars - 1);
                  if v < DClosest then
                    begin
                      CClosest := J;
                      DClosest := v;
                    end;
                  Inc(J);
                end;
              if kIndex[i] <> CClosest then
                  WasChanges := True;
              kIndex[i] := CClosest;
              Inc(i);
            end;

          // Update centers
          J := 0;
          while J <= k - 1 do
            begin
              CSizes[J] := 0;
              Inc(J);
            end;
          i := 0;
          while i <= k - 1 do
            begin
              J := 0;
              while J <= NVars - 1 do
                begin
                  ct[i, J] := 0;
                  Inc(J);
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
              if not SelectCenter(Source, NPoints, NVars, ct, CBusy, k, d2, p, tmp) then
                begin
                  Result := -3;
                  Exit;
                end;
              Continue;
            end;
          J := 0;
          while J <= k - 1 do
            begin
              v := 1.0 / CSizes[J];
              ArrayMul(@ct[J][0], 0, NVars - 1, v);
              Inc(J);
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
          ArrayMove(@tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
          ArraySub(@tmp[0], 0, NVars - 1, @ct[kIndex[i]][0], 0, NVars - 1);
          v := ArrayDotProduct(@tmp[0], 0, NVars - 1, @tmp[0], 0, NVars - 1);
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
      Inc(pass);
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
