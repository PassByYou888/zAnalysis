{ ****************************************************************************** }
{ * fast Histogram of Oriented Gradient support                                * }
{ * create by QQ 600585@qq.com                                                 * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit FastHistogramSpace;

interface

{$I zDefine.inc}


uses CoreClasses, MemoryRaster, Geometry2DUnit, LearnTypes;

type
  THOGTable = class(TCoreClassObject)
  private type
    TItpRec = packed record
      d: array [0 .. 3] of array [0 .. 1] of TLInt;
      w: array [0 .. 3] of TLInt;
    end;

    PItpRec = ^TItpRec;

    TItpRecArray = array of array of TItpRec;
    PItpRecArray = ^TItpRecArray;
  private
    nOriH, nOriF, cSiz, oriF, oriH: TLInt;
    oriHMatrix: TLIMatrix;
    oriFMatrix: TLIMatrix;
    magMatrix: TLMatrix;
    bwMatrix: TLMatrix;
    itpMatrix: TItpRecArray;
  public
    constructor Create(const numOriHalf, numOriFull, cellSize: TLInt);
    destructor Destroy; override;
  end;

  THOG = class(TCoreClassObject)
  public type
    THRec = packed record
      binH, binF, feat: TLVec;
      nOriH, nOriF, dim: TLInt;
      normH: TLFloat;
    end;

    PHRec      = ^THRec;
    THRecArray = array of array of THRec;
    PHRecArray = ^THRecArray;
  private
    nCX, nCY, cSiz, hSizX, hSizY, Width, Height: TLInt;
    dx, dy: TLIMatrix;
    procedure initialHist(const imgWidth, imgHeight, numOriHalf, numOriFull, cellSize: TLInt);
    procedure ComputeRGB_Diff(img: TMemoryRaster; const m: PLMatrix);
    procedure ComputeHistogram(const AntiLight: Boolean; const oriH, oriF: PLIMatrix; const bw: PLMatrix; const itp: THOGTable.PItpRecArray);
  public
    Ori: THRecArray;
    constructor Create(Table: THOGTable; img: TMemoryRaster);
    constructor CreateAntiLight(Table: THOGTable; img: TMemoryRaster);
    destructor Destroy; override;

    procedure BuildViewer(output: TMemoryRaster);
  end;

procedure TestHOG;

implementation

uses
  Math, DoStatusIO,
  {$IFDEF parallel}
  {$IFDEF FPC}
  mtprocs,
  {$ELSE FPC}
  Threading,
  {$ENDIF FPC}
  {$ENDIF parallel}
  SyncObjs, Learn;

const
  NUM_DIFF      = 511;
  NUM_DIFF_DIV2 = 255;

constructor THOGTable.Create(const numOriHalf, numOriFull, cellSize: TLInt);
  function arcTanXY(const x, y: TLFloat): TLInt; inline;
  begin
    Result := Trunc(arctan2(y, x) / pi * 180 + 360);
  end;

var
  intervalFull, intervalHalf: TLFloat;
  y, x: TLInt;
  d2, nearWeightX, farWeightX, nearWeightY, farWeightY: TLInt;
  cellSize2, w: TLInt;
  p: PItpRec;
begin
  inherited Create;
  nOriH := numOriHalf;
  nOriF := numOriFull;
  cSiz := cellSize;
  oriF := 360;
  oriH := 180;

  SetLength(oriFMatrix, NUM_DIFF, NUM_DIFF);
  SetLength(oriHMatrix, NUM_DIFF, NUM_DIFF);
  SetLength(magMatrix, NUM_DIFF, NUM_DIFF);
  SetLength(itpMatrix, cSiz, cSiz);
  SetLength(bwMatrix, cSiz * cSiz + 1, NUM_DIFF * NUM_DIFF);

  intervalFull := oriF / nOriF;
  intervalHalf := oriH / nOriH;
  for y := -NUM_DIFF_DIV2 to NUM_DIFF_DIV2 do
    for x := -NUM_DIFF_DIV2 to NUM_DIFF_DIV2 do
      begin
        oriFMatrix[y + NUM_DIFF_DIV2, x + NUM_DIFF_DIV2] := Trunc(arcTanXY(x, y) mod oriF / intervalFull);
        oriHMatrix[y + NUM_DIFF_DIV2, x + NUM_DIFF_DIV2] := Trunc(arcTanXY(x, y) mod oriH / intervalHalf);
        magMatrix[y + NUM_DIFF_DIV2, x + NUM_DIFF_DIV2] := sqrt(x * x + y * y);
      end;

  d2 := cSiz div 2;
  for y := -d2 to d2 - 1 do
    begin
      for x := -d2 to d2 - 1 do
        begin
          p := @itpMatrix[y + d2, x + d2];
          if (y < 0) and (x < 0) then
            begin
              nearWeightX := cSiz + x + 1;
              farWeightX := cSiz - nearWeightX;
              nearWeightY := cSiz + y + 1;
              farWeightY := cSiz - nearWeightY;
              p^.d[0, 0] := 0;
              p^.d[0, 1] := 0;
              p^.w[0] := nearWeightX * nearWeightY;
              p^.d[1, 0] := 0;
              p^.d[1, 1] := -1;
              p^.w[1] := nearWeightX * farWeightY;
              p^.d[2, 0] := -1;
              p^.d[2, 1] := 0;
              p^.w[2] := farWeightX * nearWeightY;
              p^.d[3, 0] := -1;
              p^.d[3, 1] := -1;
              p^.w[3] := farWeightX * farWeightY;
            end
          else if (y < 0) and (x >= 0) then
            begin
              nearWeightX := cSiz - x;
              farWeightX := cSiz - nearWeightX;
              nearWeightY := cSiz + y + 1;
              farWeightY := cSiz - nearWeightY;
              p^.d[0, 0] := 0;
              p^.d[0, 1] := 0;
              p^.w[0] := nearWeightX * nearWeightY;
              p^.d[1, 0] := 0;
              p^.d[1, 1] := -1;
              p^.w[1] := nearWeightX * farWeightY;
              p^.d[2, 0] := 1;
              p^.d[2, 1] := 0;
              p^.w[2] := farWeightX * nearWeightY;
              p^.d[3, 0] := 1;
              p^.d[3, 1] := -1;
              p^.w[3] := farWeightX * farWeightY;
            end
          else if (y >= 0) and (x >= 0) then
            begin
              nearWeightX := cSiz - x;
              farWeightX := cSiz - nearWeightX;
              nearWeightY := cSiz - y;
              farWeightY := cSiz - nearWeightY;
              p^.d[0, 0] := 0;
              p^.d[0, 1] := 0;
              p^.w[0] := nearWeightX * nearWeightY;
              p^.d[1, 0] := 0;
              p^.d[1, 1] := 1;
              p^.w[1] := nearWeightX * farWeightY;
              p^.d[2, 0] := 1;
              p^.d[2, 1] := 0;
              p^.w[2] := farWeightX * nearWeightY;
              p^.d[3, 0] := 1;
              p^.d[3, 1] := 1;
              p^.w[3] := farWeightX * farWeightY;
            end
          else if (y >= 0) and (x < 0) then
            begin
              nearWeightX := cSiz + x + 1;
              farWeightX := cSiz - nearWeightX;
              nearWeightY := cSiz - y;
              farWeightY := cSiz - nearWeightY;
              p^.d[0, 0] := 0;
              p^.d[0, 1] := 0;
              p^.w[0] := nearWeightX * nearWeightY;
              p^.d[1, 0] := 0;
              p^.d[1, 1] := 1;
              p^.w[1] := nearWeightX * farWeightY;
              p^.d[2, 0] := -1;
              p^.d[2, 1] := 0;
              p^.w[2] := farWeightX * nearWeightY;
              p^.d[3, 0] := -1;
              p^.d[3, 1] := 1;
              p^.w[3] := farWeightX * farWeightY;
            end;
        end;
    end;

  cellSize2 := cSiz * cSiz;

  for w := 0 to cellSize2 do
    for y := -NUM_DIFF_DIV2 to NUM_DIFF_DIV2 do
      for x := -NUM_DIFF_DIV2 to NUM_DIFF_DIV2 do
          bwMatrix[w, (y + NUM_DIFF_DIV2) * NUM_DIFF + (x + NUM_DIFF_DIV2)] := magMatrix[y + NUM_DIFF_DIV2, x + NUM_DIFF_DIV2] * w / cellSize2;
end;

destructor THOGTable.Destroy;
begin
  SetLength(oriFMatrix, 0, 0);
  SetLength(oriHMatrix, 0, 0);
  SetLength(magMatrix, 0, 0);
  SetLength(itpMatrix, 0, 0);
  SetLength(bwMatrix, 0, 0);
  inherited Destroy;
end;

procedure THOG.initialHist(const imgWidth, imgHeight, numOriHalf, numOriFull, cellSize: TLInt);
var
  y, x: TLInt;
begin
  if (imgWidth mod cellSize <> 0) or (imgHeight mod cellSize <> 0) then
    begin
      raiseInfo('image size is not N*cSiz');
      exit;
    end;
  cSiz := cellSize;
  nCX := imgWidth div cSiz;
  nCY := imgHeight div cSiz;
  hSizX := nCX + 2;
  hSizY := nCY + 2;
  Width := imgWidth;
  Height := imgHeight;

  SetLength(Ori, hSizY, hSizX);

  for y := 0 to hSizY - 1 do
    for x := 0 to hSizX - 1 do
      begin
        SetLength(Ori[y, x].binF, numOriFull);
        SetLength(Ori[y, x].binH, numOriHalf);
        Ori[y, x].dim := 4 + (numOriFull + numOriHalf);
        SetLength(Ori[y, x].feat, Ori[y, x].dim);

        Ori[y, x].nOriF := numOriFull;
        Ori[y, x].nOriH := numOriHalf;
        Ori[y, x].normH := 0;
      end;

  SetLength(dx, Height, Width);
  SetLength(dy, Height, Width);
end;

procedure THOG.ComputeRGB_Diff(img: TMemoryRaster; const m: PLMatrix);
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    x, xDiffs, yDiffs, magni: TLInt;
    pcl, pcr, pct, pcb: TRasterColorEntry;
  begin
    for x := 0 to Width - 1 do
      begin
        pcl.RGBA := img[x - 1, pass];
        pcr.RGBA := img[x + 1, pass];
        pct.RGBA := img[x, pass - 1];
        pcb.RGBA := img[x, pass + 1];

        dx[pass, x] := pcl.R - pcr.R;
        dy[pass, x] := pct.R - pcb.R;
        magni := Trunc(m^[dy[pass, x] + NUM_DIFF_DIV2, dx[pass, x] + NUM_DIFF_DIV2]);

        xDiffs := pcl.g - pcr.g;
        yDiffs := pct.g - pcb.g;
        if magni < m^[yDiffs + NUM_DIFF_DIV2, xDiffs + NUM_DIFF_DIV2] then
          begin
            dx[pass, x] := xDiffs;
            dy[pass, x] := yDiffs;
          end;

        xDiffs := pcl.b - pcr.b;
        yDiffs := pct.b - pcb.b;
        if magni < m^[yDiffs + NUM_DIFF_DIV2, xDiffs + NUM_DIFF_DIV2] then
          begin
            dx[pass, x] := xDiffs;
            dy[pass, x] := yDiffs;
          end;
      end;
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor;
  var
    pass, x, xDiffs, yDiffs, magni: TLInt;
    pcl, pcr, pct, pcb: TRasterColorEntry;
  begin
    for pass := 0 to Height - 1 do
      for x := 0 to Width - 1 do
        begin
          pcl.RGBA := img[x - 1, pass];
          pcr.RGBA := img[x + 1, pass];
          pct.RGBA := img[x, pass - 1];
          pcb.RGBA := img[x, pass + 1];

          dx[pass, x] := pcl.R - pcr.R;
          dy[pass, x] := pct.R - pcb.R;
          magni := Trunc(m^[dy[pass, x] + NUM_DIFF_DIV2, dx[pass, x] + NUM_DIFF_DIV2]);

          xDiffs := pcl.g - pcr.g;
          yDiffs := pct.g - pcb.g;
          if magni < m^[yDiffs + NUM_DIFF_DIV2, xDiffs + NUM_DIFF_DIV2] then
            begin
              dx[pass, x] := xDiffs;
              dy[pass, x] := yDiffs;
            end;

          xDiffs := pcl.b - pcr.b;
          yDiffs := pct.b - pcb.b;
          if magni < m^[yDiffs + NUM_DIFF_DIV2, xDiffs + NUM_DIFF_DIV2] then
            begin
              dx[pass, x] := xDiffs;
              dy[pass, x] := yDiffs;
            end;
        end;
  end;
{$ENDIF parallel}


begin
  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Height - 1);
  {$ELSE FPC}
  TParallel.For(0, Height - 1, procedure(pass: Integer)
    var
      x, xDiffs, yDiffs, magni: TLInt;
      pcl, pcr, pct, pcb: TRasterColorEntry;
    begin
      for x := 0 to Width - 1 do
        begin
          pcl.RGBA := img[x - 1, pass];
          pcr.RGBA := img[x + 1, pass];
          pct.RGBA := img[x, pass - 1];
          pcb.RGBA := img[x, pass + 1];

          dx[pass, x] := pcl.R - pcr.R;
          dy[pass, x] := pct.R - pcb.R;
          magni := Trunc(m^[dy[pass, x] + NUM_DIFF_DIV2, dx[pass, x] + NUM_DIFF_DIV2]);

          xDiffs := pcl.g - pcr.g;
          yDiffs := pct.g - pcb.g;
          if magni < m^[yDiffs + NUM_DIFF_DIV2, xDiffs + NUM_DIFF_DIV2] then
            begin
              dx[pass, x] := xDiffs;
              dy[pass, x] := yDiffs;
            end;

          xDiffs := pcl.b - pcr.b;
          yDiffs := pct.b - pcb.b;
          if magni < m^[yDiffs + NUM_DIFF_DIV2, xDiffs + NUM_DIFF_DIV2] then
            begin
              dx[pass, x] := xDiffs;
              dy[pass, x] := yDiffs;
            end;
        end;
    end);
  {$ENDIF FPC}
  {$ELSE parallel}
  DoFor;
  {$ENDIF parallel}
end;

procedure THOG.ComputeHistogram(const AntiLight: Boolean; const oriH, oriF: PLIMatrix; const bw: PLMatrix; const itp: THOGTable.PItpRecArray);
var
  setoff: TLInt;

  {$IFDEF FPC}
  procedure Nested_ParallelFor_Weight(y: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    x, n: TLInt;
    xDiff, yDiff, oriIdxFull, oriIdxHalf: TLInt;
    cellCoorX, cellCoorY, cellY, cellX, dstCellCoorX, dstCellCoorY, weight: TLInt;
    weightedBinValue: TLFloat;
  begin
    for x := 0 to Width - 1 do
      begin
        xDiff := dx[y, x];
        yDiff := dy[y, x];
        oriIdxFull := oriF^[yDiff + NUM_DIFF_DIV2, xDiff + NUM_DIFF_DIV2];
        oriIdxHalf := oriH^[yDiff + NUM_DIFF_DIV2, xDiff + NUM_DIFF_DIV2];
        cellCoorY := y div cSiz + 1;
        cellCoorX := x div cSiz + 1;
        cellY := y - (cellCoorY - 1) * cSiz;
        cellX := x - (cellCoorX - 1) * cSiz;

        for n := 0 to 3 do
          begin
            dstCellCoorY := cellCoorY + itp^[cellY, cellX].d[n, 1];
            dstCellCoorX := cellCoorX + itp^[cellY, cellX].d[n, 0];
            weight := itp^[cellY, cellX].w[n];
            weightedBinValue := bw^[weight, (yDiff + NUM_DIFF_DIV2) * NUM_DIFF + (xDiff + NUM_DIFF_DIV2)];
            LAdd(Ori[dstCellCoorY, dstCellCoorX].binF[oriIdxFull], weightedBinValue);
            LAdd(Ori[dstCellCoorY, dstCellCoorX].binH[oriIdxHalf], weightedBinValue);
          end;
      end;
  end;
  procedure Nested_ParallelFor_normH(cellY: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    cellX, n: TLInt;
    hp: PHRec;
  begin
    for cellX := 0 to hSizX - 1 do
      begin
        hp := @Ori[cellY, cellX];
        for n := 0 to hp^.nOriH - 1 do
            LAdd(hp^.normH, hp^.binH[n mod hp^.nOriF] * hp^.binH[n mod hp^.nOriF]);
      end;
  end;
  procedure Nested_ParallelFor_Ori(cellY: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    cellX, n: TLInt;
    hp: PHRec;
    nw, ns: array [0 .. 3] of TLFloat;
    sum: TLFloat;
  begin
    for cellX := 1 to nCX do
      begin
        hp := @Ori[cellY, cellX];

        nw[0] := Learn.AP_Sqr(hp^.normH + Ori[cellY - 1, cellX].normH + Ori[cellY, cellX - 1].normH + Ori[cellY - 1, cellX - 1].normH);
        nw[1] := Learn.AP_Sqr(hp^.normH + Ori[cellY + 1, cellX].normH + Ori[cellY, cellX - 1].normH + Ori[cellY + 1, cellX - 1].normH);
        nw[2] := Learn.AP_Sqr(hp^.normH + Ori[cellY + 1, cellX].normH + Ori[cellY, cellX + 1].normH + Ori[cellY + 1, cellX + 1].normH);
        nw[3] := Learn.AP_Sqr(hp^.normH + Ori[cellY - 1, cellX].normH + Ori[cellY, cellX + 1].normH + Ori[cellY - 1, cellX + 1].normH);

        for n := 0 to hp^.nOriH - 1 do
          begin
            ns[0] := max(hp^.binH[n] / nw[0], 0.2);
            ns[1] := max(hp^.binH[n] / nw[1], 0.2);
            ns[2] := max(hp^.binH[n] / nw[2], 0.2);
            ns[3] := max(hp^.binH[n] / nw[3], 0.2);
            hp^.feat[n] := ns[0] + ns[1] + ns[2] + ns[3];
            LAdd(hp^.feat[setoff + 0], ns[0]);
            LAdd(hp^.feat[setoff + 1], ns[1]);
            LAdd(hp^.feat[setoff + 2], ns[2]);
            LAdd(hp^.feat[setoff + 3], ns[3]);
          end;

        for n := 0 to hp^.nOriF - 1 do
          begin
            ns[0] := max(hp^.binF[n] / nw[0], 0.2);
            ns[1] := max(hp^.binF[n] / nw[1], 0.2);
            ns[2] := max(hp^.binF[n] / nw[2], 0.2);
            ns[3] := max(hp^.binF[n] / nw[3], 0.2);
            hp^.feat[hp^.nOriH + n] := ns[0] + ns[1] + ns[2] + ns[3];
            LAdd(hp^.feat[setoff + 0], ns[0]);
            LAdd(hp^.feat[setoff + 1], ns[1]);
            LAdd(hp^.feat[setoff + 2], ns[2]);
            LAdd(hp^.feat[setoff + 3], ns[3]);
          end;

        if AntiLight then
          begin
            sum := 0;
            for n := 0 to Length(hp^.feat) - 1 do
                LAdd(sum, hp^.feat[n]);
            for n := 0 to Length(hp^.feat) - 1 do
                hp^.feat[n] := Learn.AP_Sqr(hp^.feat[n] / sum);
          end;
      end;
  end;
{$ENDIF FPC}
{$IFNDEF parallel}
  procedure DoFor_Weight;
  var
    y, x, n: TLInt;
    xDiff, yDiff, oriIdxFull, oriIdxHalf: TLInt;
    cellCoorX, cellCoorY, cellY, cellX, dstCellCoorX, dstCellCoorY, weight: TLInt;
    weightedBinValue: TLFloat;
  begin
    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
        begin
          xDiff := dx[y, x];
          yDiff := dy[y, x];
          oriIdxFull := oriF^[yDiff + NUM_DIFF_DIV2, xDiff + NUM_DIFF_DIV2];
          oriIdxHalf := oriH^[yDiff + NUM_DIFF_DIV2, xDiff + NUM_DIFF_DIV2];
          cellCoorY := y div cSiz + 1;
          cellCoorX := x div cSiz + 1;
          cellY := y - (cellCoorY - 1) * cSiz;
          cellX := x - (cellCoorX - 1) * cSiz;

          for n := 0 to 3 do
            begin
              dstCellCoorY := cellCoorY + itp^[cellY, cellX].d[n, 1];
              dstCellCoorX := cellCoorX + itp^[cellY, cellX].d[n, 0];
              weight := itp^[cellY, cellX].w[n];
              weightedBinValue := bw^[weight, (yDiff + NUM_DIFF_DIV2) * NUM_DIFF + (xDiff + NUM_DIFF_DIV2)];
              LAdd(Ori[dstCellCoorY, dstCellCoorX].binF[oriIdxFull], weightedBinValue);
              LAdd(Ori[dstCellCoorY, dstCellCoorX].binH[oriIdxHalf], weightedBinValue);
            end;
        end;
  end;
  procedure DoFor_normH;
  var
    cellY, cellX, n: TLInt;
    hp: PHRec;
  begin
    for cellY := 0 to hSizY - 1 do
      for cellX := 0 to hSizX - 1 do
        begin
          hp := @Ori[cellY, cellX];
          for n := 0 to hp^.nOriH - 1 do
              LAdd(hp^.normH, hp^.binH[n mod hp^.nOriF] * hp^.binH[n mod hp^.nOriF]);
        end;
  end;
  procedure DoFor_Ori;
  var
    cellY, cellX, n: TLInt;
    hp: PHRec;
    nw, ns: array [0 .. 3] of TLFloat;
    sum: TLFloat;
  begin
    for cellY := 1 to nCY do
      for cellX := 1 to nCX do
        begin
          hp := @Ori[cellY, cellX];

          nw[0] := Learn.AP_Sqr(hp^.normH + Ori[cellY - 1, cellX].normH + Ori[cellY, cellX - 1].normH + Ori[cellY - 1, cellX - 1].normH);
          nw[1] := Learn.AP_Sqr(hp^.normH + Ori[cellY + 1, cellX].normH + Ori[cellY, cellX - 1].normH + Ori[cellY + 1, cellX - 1].normH);
          nw[2] := Learn.AP_Sqr(hp^.normH + Ori[cellY + 1, cellX].normH + Ori[cellY, cellX + 1].normH + Ori[cellY + 1, cellX + 1].normH);
          nw[3] := Learn.AP_Sqr(hp^.normH + Ori[cellY - 1, cellX].normH + Ori[cellY, cellX + 1].normH + Ori[cellY - 1, cellX + 1].normH);

          for n := 0 to hp^.nOriH - 1 do
            begin
              ns[0] := max(hp^.binH[n] / nw[0], 0.2);
              ns[1] := max(hp^.binH[n] / nw[1], 0.2);
              ns[2] := max(hp^.binH[n] / nw[2], 0.2);
              ns[3] := max(hp^.binH[n] / nw[3], 0.2);
              hp^.feat[n] := ns[0] + ns[1] + ns[2] + ns[3];
              LAdd(hp^.feat[setoff + 0], ns[0]);
              LAdd(hp^.feat[setoff + 1], ns[1]);
              LAdd(hp^.feat[setoff + 2], ns[2]);
              LAdd(hp^.feat[setoff + 3], ns[3]);
            end;

          for n := 0 to hp^.nOriF - 1 do
            begin
              ns[0] := max(hp^.binF[n] / nw[0], 0.2);
              ns[1] := max(hp^.binF[n] / nw[1], 0.2);
              ns[2] := max(hp^.binF[n] / nw[2], 0.2);
              ns[3] := max(hp^.binF[n] / nw[3], 0.2);
              hp^.feat[hp^.nOriH + n] := ns[0] + ns[1] + ns[2] + ns[3];
              LAdd(hp^.feat[setoff + 0], ns[0]);
              LAdd(hp^.feat[setoff + 1], ns[1]);
              LAdd(hp^.feat[setoff + 2], ns[2]);
              LAdd(hp^.feat[setoff + 3], ns[3]);
            end;

          if AntiLight then
            begin
              sum := 0;
              for n := 0 to Length(hp^.feat) - 1 do
                  LAdd(sum, hp^.feat[n]);
              for n := 0 to Length(hp^.feat) - 1 do
                  hp^.feat[n] := Learn.AP_Sqr(hp^.feat[n] / sum);
            end;
        end;
  end;
{$ENDIF parallel}


begin
  setoff := Ori[0, 0].nOriH + Ori[0, 0].nOriF;

  {$IFDEF parallel}
  {$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor_Weight, 0, Height - 1);
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor_normH, 0, hSizY - 1);
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor_Ori, 1, nCY - 1);
  {$ELSE FPC}
  TParallel.For(0, Height - 1, procedure(y: Integer)
    var
      x, n: TLInt;
      xDiff, yDiff, oriIdxFull, oriIdxHalf: TLInt;
      cellCoorX, cellCoorY, cellY, cellX, dstCellCoorX, dstCellCoorY, weight: TLInt;
      weightedBinValue: TLFloat;
    begin
      for x := 0 to Width - 1 do
        begin
          xDiff := dx[y, x];
          yDiff := dy[y, x];
          oriIdxFull := oriF^[yDiff + NUM_DIFF_DIV2, xDiff + NUM_DIFF_DIV2];
          oriIdxHalf := oriH^[yDiff + NUM_DIFF_DIV2, xDiff + NUM_DIFF_DIV2];
          cellCoorY := y div cSiz + 1;
          cellCoorX := x div cSiz + 1;
          cellY := y - (cellCoorY - 1) * cSiz;
          cellX := x - (cellCoorX - 1) * cSiz;

          for n := 0 to 3 do
            begin
              dstCellCoorY := cellCoorY + itp^[cellY, cellX].d[n, 1];
              dstCellCoorX := cellCoorX + itp^[cellY, cellX].d[n, 0];
              weight := itp^[cellY, cellX].w[n];
              weightedBinValue := bw^[weight, (yDiff + NUM_DIFF_DIV2) * NUM_DIFF + (xDiff + NUM_DIFF_DIV2)];
              LAdd(Ori[dstCellCoorY, dstCellCoorX].binF[oriIdxFull], weightedBinValue);
              LAdd(Ori[dstCellCoorY, dstCellCoorX].binH[oriIdxHalf], weightedBinValue);
            end;
        end;
    end);
  TParallel.For(0, hSizY - 1, procedure(cellY: Integer)
    var
      cellX, n: TLInt;
      hp: PHRec;
    begin
      for cellX := 0 to hSizX - 1 do
        begin
          hp := @Ori[cellY, cellX];
          for n := 0 to hp^.nOriH - 1 do
              LAdd(hp^.normH, hp^.binH[n mod hp^.nOriF] * hp^.binH[n mod hp^.nOriF]);
        end;
    end);
  TParallel.For(1, nCY - 1, procedure(cellY: Integer)
    var
      cellX, n: TLInt;
      hp: PHRec;
      nw, ns: array [0 .. 3] of TLFloat;
      sum: TLFloat;
    begin
      for cellX := 1 to nCX do
        begin
          hp := @Ori[cellY, cellX];

          nw[0] := Learn.AP_Sqr(hp^.normH + Ori[cellY - 1, cellX].normH + Ori[cellY, cellX - 1].normH + Ori[cellY - 1, cellX - 1].normH);
          nw[1] := Learn.AP_Sqr(hp^.normH + Ori[cellY + 1, cellX].normH + Ori[cellY, cellX - 1].normH + Ori[cellY + 1, cellX - 1].normH);
          nw[2] := Learn.AP_Sqr(hp^.normH + Ori[cellY + 1, cellX].normH + Ori[cellY, cellX + 1].normH + Ori[cellY + 1, cellX + 1].normH);
          nw[3] := Learn.AP_Sqr(hp^.normH + Ori[cellY - 1, cellX].normH + Ori[cellY, cellX + 1].normH + Ori[cellY - 1, cellX + 1].normH);

          for n := 0 to hp^.nOriH - 1 do
            begin
              ns[0] := max(hp^.binH[n] / nw[0], 0.2);
              ns[1] := max(hp^.binH[n] / nw[1], 0.2);
              ns[2] := max(hp^.binH[n] / nw[2], 0.2);
              ns[3] := max(hp^.binH[n] / nw[3], 0.2);
              hp^.feat[n] := ns[0] + ns[1] + ns[2] + ns[3];
              LAdd(hp^.feat[setoff + 0], ns[0]);
              LAdd(hp^.feat[setoff + 1], ns[1]);
              LAdd(hp^.feat[setoff + 2], ns[2]);
              LAdd(hp^.feat[setoff + 3], ns[3]);
            end;

          for n := 0 to hp^.nOriF - 1 do
            begin
              ns[0] := max(hp^.binF[n] / nw[0], 0.2);
              ns[1] := max(hp^.binF[n] / nw[1], 0.2);
              ns[2] := max(hp^.binF[n] / nw[2], 0.2);
              ns[3] := max(hp^.binF[n] / nw[3], 0.2);
              hp^.feat[hp^.nOriH + n] := ns[0] + ns[1] + ns[2] + ns[3];
              LAdd(hp^.feat[setoff + 0], ns[0]);
              LAdd(hp^.feat[setoff + 1], ns[1]);
              LAdd(hp^.feat[setoff + 2], ns[2]);
              LAdd(hp^.feat[setoff + 3], ns[3]);
            end;

          if AntiLight then
            begin
              sum := 0;
              for n := 0 to Length(hp^.feat) - 1 do
                  LAdd(sum, hp^.feat[n]);
              for n := 0 to Length(hp^.feat) - 1 do
                  hp^.feat[n] := Learn.AP_Sqr(hp^.feat[n] / sum);
            end;
        end;
    end);
  {$ENDIF FPC}
  {$ELSE parallel}
  DoFor_Weight;
  DoFor_normH;
  DoFor_Ori;
  {$ENDIF parallel}
end;

constructor THOG.Create(Table: THOGTable; img: TMemoryRaster);
begin
  inherited Create;
  initialHist(
    img.Width - img.Width mod Table.cSiz,
    img.Height - img.Height mod Table.cSiz,
    Table.nOriH, Table.nOriF, Table.cSiz);
  ComputeRGB_Diff(img, @Table.magMatrix);
  ComputeHistogram(False, @Table.oriHMatrix, @Table.oriFMatrix, @Table.bwMatrix, @Table.itpMatrix);
end;

constructor THOG.CreateAntiLight(Table: THOGTable; img: TMemoryRaster);
begin
  inherited Create;
  initialHist(
    img.Width - img.Width mod Table.cSiz,
    img.Height - img.Height mod Table.cSiz,
    Table.nOriH, Table.nOriF, Table.cSiz);
  ComputeRGB_Diff(img, @Table.magMatrix);
  ComputeHistogram(True, @Table.oriHMatrix, @Table.oriFMatrix, @Table.bwMatrix, @Table.itpMatrix);
end;

destructor THOG.Destroy;
var
  y, x: TLInt;
  hp: PHRec;
begin
  for y := 0 to Length(Ori) - 1 do
    for x := 0 to Length(Ori[y]) - 1 do
      begin
        hp := @Ori[y, x];
        SetLength(hp^.binH, 0);
        SetLength(hp^.binF, 0);
        SetLength(hp^.feat, 0);
      end;
  SetLength(Ori, 0, 0);
  SetLength(dx, 0, 0);
  SetLength(dy, 0, 0);
  inherited Destroy;
end;

procedure THOG.BuildViewer(output: TMemoryRaster);
var
  bP, eP, cP, p: TVec2;
  numOri, maxIdx: TLInt;
  maxValue: TLInt;
  pixValue: TLInt;
  cellY, cellX, n: TLInt;
  maxBinValue: TLFloat;
begin
  output.SetSize(cSiz * nCX, cSiz * nCY, RasterColorF(0, 0, 0, 0));

  maxBinValue := -Learn.MaxRealNumber;

  for cellY := 1 to nCY do
    for cellX := 1 to nCX do
      begin
        numOri := Ori[cellY, cellX].nOriF;
        for n := 0 to numOri - 1 do
            maxBinValue := IfThen(Ori[cellY, cellX].binF[n] > maxBinValue, Ori[cellY, cellX].binF[n], maxBinValue);
      end;

  for cellY := 1 to nCY do
    begin
      cP[1] := (cellY - 1) * cSiz + cellY / 2;
      for cellX := 1 to nCX do
        begin
          numOri := Ori[cellY, cellX].nOriF;
          maxIdx := 0;
          maxValue := Trunc(Ori[cellY, cellX].binF[0] / maxBinValue * 4096);
          cP[0] := (cellX - 1) * cSiz + cellX / 2;
          for n := 0 to numOri - 1 do
            begin
              pixValue := Trunc(Ori[cellY, cellX].binF[n] / maxBinValue * 4096);
              if pixValue > maxValue then
                begin
                  maxValue := pixValue;
                  maxIdx := n;
                end
              else
                begin
                  pixValue := LClamp(pixValue, 0, 255);
                  p := PointRotation(cP, cSiz * 0.5, (maxIdx * (360 / numOri)));
                  output.LineF(PointLerpTo(p, cP, cSiz), p, RasterColor(pixValue, 0, 0, pixValue), True);
                end;
            end;
        end;
    end;
end;

procedure TestHOG;
var
  tab: THOGTable;
  hog: THOG;
  img: TMemoryRaster;
  view: TMemoryRaster;
  i: TLInt;
  t: TTimeTick;
begin
  img := NewRasterFromFile('c:\1.bmp');

  tab := THOGTable.Create(18, 36, 8);

  t := GetTimeTick;
  for i := 1 to 100 do
    begin
      hog := THOG.CreateAntiLight(tab, img);
      disposeObject(hog);
    end;
  DoStatus('%dms', [GetTimeTick - t]);

  hog := THOG.CreateAntiLight(tab, img);

  view := TMemoryRaster.Create;
  hog.BuildViewer(view);
  img.Draw(-8, -8, view);
  img.SaveToFile('c:\view.bmp');
  disposeObject(view);

  disposeObject(tab);
  disposeObject(hog);
  disposeObject(img);
end;

end.
