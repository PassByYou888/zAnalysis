{ ****************************************************************************** }
{ * Dynamic KDTree support, writen by QQ 600585@qq.com                         * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit KDTree;

interface

uses CoreClasses, PascalStrings, KM;

{$I zDefine.inc}

type
  TKDTree_VecType = TKMFloat;
  PKDTree_VecType = PKMFloat;

  TKDTree_Vec = TKMFloatArray;
  PKDTree_Vec = PKMFloatArray;

  TKDTree_Source = packed record
    Buff: TKDTree_Vec;
    index: Int64;
  end;

  PKDTree_Source = ^TKDTree_Source;

  TKDTree_DynamicVecBuffer = TKMFloat2DArray;
  PKDTree_DynamicVecBuffer = PKMFloat2DArray;

  TKDTree_SourceBuffer = packed array [0 .. MaxInt div SizeOf(PKDTree_Source) - 1] of PKDTree_Source;
  PKDTree_SourceBuffer = ^TKDTree_SourceBuffer;

  TKDTreeDyanmicSourceBuffer = packed array of PKDTree_Source;
  PKDTreeDyanmicSourceBuffer = ^TKDTreeDyanmicSourceBuffer;

  TKDTreeDyanmicStoreBuffer = packed array of TKDTree_Source;
  PKDTreeDyanmicStoreBuffer = ^TKDTreeDyanmicStoreBuffer;

  PKDTree_Node = ^TKDTree_Node;

  TKDTree_Node = packed record
    Parent, Right, Left: PKDTree_Node;
    vec: PKDTree_Source;
  end;

  TKDTree = class(TCoreClassObject)
  public type
    TKDTree_BuildCall               = procedure(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
    TKDTree_BuildMethod             = procedure(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer) of object;
    {$IFNDEF FPC} TKDTree_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer); {$ENDIF}
  private
    FAxisCount: Integer;
    KDStoreBuff: TKDTreeDyanmicStoreBuffer;
    KDBuff: TKDTreeDyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: packed array of PKDTree_Node;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDTree_SourceBuffer; const PlanCount, Depth: NativeInt): PKDTree_Node;
    function GetData(const index: NativeInt): PKDTree_Source; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    RootNode: PKDTree_Node;

    constructor Create(const Axis: Integer); virtual;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDTreeDyanmicStoreBuffer;
    property SourceP[const index: NativeInt]: PKDTree_Source read GetData; default;
    property AxisCount: Integer read FAxisCount;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildCall); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildMethod); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildProc); {$IFDEF INLINE_ASM} inline; {$ENDIF} {$ENDIF}
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDTree_BuildCall); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDTree_BuildMethod); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDTree_BuildProc); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$ENDIF FPC}
    { search }
    function Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDTree_Node; overload;
    function Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDTree_Node; overload;
    function Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double): PKDTree_Node; overload;
    function Search(const Buff: TKDTree_Vec): PKDTree_Node; overload;
    { parallel support }
    procedure Search(const inBuff: TKDTree_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToFile(fileName: SystemString);
    procedure LoadFromFile(fileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDTree_Node);
    procedure PrintBuffer;

    class function KDTreeVec(const s: SystemString): TKDTree_Vec; overload;
    class function KDTreeVec(const v: TKDTree_Vec): SystemString; overload;
    class function KDTreeDistance(const v1, v2: TKDTree_Vec): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  // debug
procedure Test_KDTree(const Axis: Integer);

implementation

uses
  {$IFDEF FPC}
  mtprocs,
  {$ELSE FPC}
  Threading,
  {$ENDIF FPC}
  TextParsing, UnicodeMixedLib, DoStatusIO;

const
  SaveToken = $9;

function TKDTree.InternalBuildKdTree(const KDSourceBufferPtr: PKDTree_SourceBuffer; const PlanCount, Depth: NativeInt): PKDTree_Node;
  function SortCompare(const p1, p2: PKDTree_Source; const Axis: NativeInt): ShortInt; inline;
  begin
    if p1^.Buff[Axis] = p2^.Buff[Axis] then
      begin
        if p1^.index = p2^.index then
            Result := 0
        else if p1^.index < p2^.index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.Buff[Axis] < p2^.Buff[Axis] then
        Result := -1
    else
        Result := 1;
  end;
  procedure InternalSort(const SortBuffer: PKDTree_SourceBuffer; l, r: NativeInt; const Axis: NativeInt); inline;
  var
    i, j: NativeInt;
    p, t: PKDTree_Source;
  begin
    repeat
      i := l;
      j := r;
      p := SortBuffer^[(l + r) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, Axis) < 0 do
            inc(i);
        while SortCompare(SortBuffer^[j], p, Axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            inc(i);
            Dec(j);
          end;
      until i > j;
      if l < j then
          InternalSort(SortBuffer, l, j, Axis);
      l := i;
    until i >= r;
  end;

var
  m: NativeInt;
  Axis: NativeInt;
  kdBuffPtr: PKDTree_SourceBuffer;
  dynBuff: PKDTreeDyanmicSourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      exit;

  if PlanCount = 1 then
    begin
      New(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      inc(NodeCounter);
    end
  else
    begin
      Axis := Depth mod FAxisCount;
      m := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr[0], 0, PlanCount - 1, Axis);

      New(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[m];

      KDNodes[NodeCounter] := Result;
      inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], m, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[m + 1], PlanCount - (m + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDTree.GetData(const index: NativeInt): PKDTree_Source;
begin
  Result := @KDStoreBuff[index];
end;

constructor TKDTree.Create(const Axis: Integer);
begin
  inherited Create;
  FAxisCount := Axis;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
end;

destructor TKDTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TKDTree.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDTree_Node(KDNodes[i]));
      inc(i);
    end;
  for i := 0 to length(KDStoreBuff) - 1 do
      SetLength(KDStoreBuff[i].Buff, 0);

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDTree.StoreBuffPtr: PKDTreeDyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDTree.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildCall);
var
  i, j: NativeInt;
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
begin
  Clear;

  if PlanCount <= 0 then
      exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].index := i;
      SetLength(KDStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          KDStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, KDStoreBuff[i], Data);
      inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDTree.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildMethod);
var
  i, j: NativeInt;
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
begin
  Clear;

  if PlanCount <= 0 then
      exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].index := i;
      SetLength(KDStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          KDStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, KDStoreBuff[i], Data);
      inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDTree.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildProc);
var
  i, j: NativeInt;
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
begin
  Clear;

  if PlanCount <= 0 then
      exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].index := i;
      SetLength(KDStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          KDStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, KDStoreBuff[i], Data);
      inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


procedure TKDTree.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDTree_BuildCall);
var
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].index := i;
      SetLength(TempStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          TempStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, TempStoreBuff[i], Data);
      inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), FAxisCount);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to FAxisCount - 1 do
        Source[i, j] := TempStoreBuff[i].Buff[j];

  if KMeansCluster(Source, FAxisCount, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].index := i;
          SetLength(KDStoreBuff[i].Buff, AxisCount);
          for j := 0 to FAxisCount - 1 do
              KDStoreBuff[i].Buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].index;

      SetLength(KArray, 0);
    end;

  for i := 0 to length(TempStoreBuff) - 1 do
      SetLength(TempStoreBuff[i].Buff, 0);
  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDTree.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDTree_BuildMethod);
var
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].index := i;
      SetLength(TempStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          TempStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, TempStoreBuff[i], Data);
      inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), FAxisCount);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to FAxisCount - 1 do
        Source[i, j] := TempStoreBuff[i].Buff[j];

  if KMeansCluster(Source, FAxisCount, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].index := i;
          SetLength(KDStoreBuff[i].Buff, AxisCount);
          for j := 0 to FAxisCount - 1 do
              KDStoreBuff[i].Buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].index;

      SetLength(KArray, 0);
    end;

  for i := 0 to length(TempStoreBuff) - 1 do
      SetLength(TempStoreBuff[i].Buff, 0);
  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDTree.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDTree_BuildProc);
var
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].index := i;
      SetLength(TempStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          TempStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, TempStoreBuff[i], Data);
      inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), FAxisCount);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to FAxisCount - 1 do
        Source[i, j] := TempStoreBuff[i].Buff[j];

  if KMeansCluster(Source, FAxisCount, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].index := i;
          SetLength(KDStoreBuff[i].Buff, AxisCount);
          for j := 0 to FAxisCount - 1 do
              KDStoreBuff[i].Buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].index;

      SetLength(KArray, 0);
    end;

  for i := 0 to length(TempStoreBuff) - 1 do
      SetLength(TempStoreBuff[i].Buff, 0);
  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDTree.Search(
  const Buff: TKDTree_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDTree_Node;

var
  NearestNeighbour: PKDTree_Node;

  function FindParentNode(const BuffPtr: PKDTree_Vec; const NodePtr: PKDTree_Node): PKDTree_Node;
  var
    Next: PKDTree_Node;
    Depth, Axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        Axis := Depth mod FAxisCount;
        if BuffPtr^[Axis] > Next^.vec^.Buff[Axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDTree_Node; const BuffPtr: PKDTree_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    Axis: NativeInt;
  begin
    if NodePtr = nil then
        exit;

    inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDTreeDistance(BuffPtr^, NodePtr^.vec^.Buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.index < NearestNeighbour^.vec^.index) then
        NearestNeighbour := NodePtr;

    Axis := Depth mod FAxisCount;
    Dist := NodePtr^.vec^.Buff[Axis] - BuffPtr^[Axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.Buff[Axis] > BuffPtr^[Axis] then
            ScanSubtree(NodePtr^.Left, BuffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, BuffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, BuffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, BuffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const BuffPtr: PKDTree_Vec; const p1, p2: PKDTree_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDTreeDistance(BuffPtr^, p1^.vec^.Buff);
    d2 := KDTreeDistance(BuffPtr^, p2^.vec^.Buff);
    if d1 = d2 then
      begin
        if p1^.vec^.index = p2^.vec^.index then
            Result := 0
        else if p1^.vec^.index < p2^.vec^.index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; l, r: NativeInt; const BuffPtr: PKDTree_Vec);
  var
    i, j: NativeInt;
    p, t: PKDTree_Node;
  begin
    repeat
      i := l;
      j := r;
      p := SortBuffer[(l + r) shr 1];
      repeat
        while SortCompare(BuffPtr, SortBuffer[i], p) < 0 do
            inc(i);
        while SortCompare(BuffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            inc(i);
            Dec(j);
          end;
      until i > j;
      if l < j then
          InternalSort(SortBuffer, l, j, BuffPtr);
      l := i;
    until i >= r;
  end;

var
  Parent: PKDTree_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      exit;
  if Count = 0 then
      exit;
  if length(Buff) <> FAxisCount then
      exit;

  Parent := FindParentNode(@Buff, RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDTreeDistance(Buff, Parent^.vec^.Buff);

  ScanSubtree(RootNode, @Buff, 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @Buff);

      if NearestNodes.Count > 0 then
          Result := PKDTree_Node(NearestNodes[0]);
    end;
end;

function TKDTree.Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDTree_Node;
begin
  Result := Search(Buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDTree.Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double): PKDTree_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(Buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDTree.Search(const Buff: TKDTree_Vec): PKDTree_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(Buff, SearchedDistanceMin, SearchedCounter);
end;

procedure TKDTree.Search(const inBuff: TKDTree_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDTree_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

  {$IFDEF FPC}
  procedure FPC_ParallelFor(Pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDTree_Node;
  begin
    p := Search(inBuffPtr^[Pass]);
    outIndexPtr^[Pass] := p^.vec^.index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  MHGlobalHookEnabled := False;
  try
    {$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
    {$ELSE FPC}
    TParallel.For(Int64(0), Int64(length(inBuff) - 1),
      procedure(Pass: Int64)
      var
        p: PKDTree_Node;
      begin
        p := Search(inBuffPtr^[Pass]);
        outIndexPtr^[Pass] := p^.vec^.index;
      end);
    {$ENDIF FPC}
  finally
      MHGlobalHookEnabled := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDTree_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.index;
    end;
end;
{$ENDIF parallel}


procedure TKDTree.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, id: Integer;
  i: NativeInt;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  id := FAxisCount;

  stream.Write(st, 4);
  stream.Write(id, 4);

  stream.Write(cnt, 8);
  for i := 0 to cnt - 1 do
    begin
      stream.Write(KDStoreBuff[i].Buff[0], FAxisCount * SizeOf(TKDTree_VecType));
      stream.Write(KDStoreBuff[i].index, 8);
    end;
end;

procedure TKDTree.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, id: Integer;
  i: NativeInt;
begin
  Clear;

  stream.Read(st, 4);
  stream.Read(id, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if id <> FAxisCount then
      RaiseInfo('kdtree axis error!');

  stream.Read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  try
    for i := 0 to cnt - 1 do
      begin
        SetLength(KDStoreBuff[i].Buff, FAxisCount);
        stream.Read(KDStoreBuff[i].Buff[0], FAxisCount * SizeOf(TKDTree_VecType));
        stream.Read(KDStoreBuff[i].index, 8);
      end;
  except
    Clear;
    exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      inc(i);
    end;

  RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDTree.SaveToFile(fileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDTree.LoadFromFile(fileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  except
      exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDTree.PrintNodeTree(const NodePtr: PKDTree_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDTree_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.index, KDTreeVec(p^.vec^.Buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDTree.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d: %s ', [KDStoreBuff[i].index, KDTreeVec(KDStoreBuff[i].Buff)]);
end;

class function TKDTree.KDTreeVec(const s: SystemString): TKDTree_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  c, i, j: NativeInt;
begin
  t := TTextParsing.Create(s, tsText, nil);
  c := t.SplitChar(1, ', ', '', SplitOutput);
  if c > 0 then
    begin
      SetLength(Result, c);

      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            inc(j);
          end;
    end;
  DisposeObject(t);
end;

class function TKDTree.KDTreeVec(const v: TKDTree_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to length(v) - 1 do
    begin
      if i > 0 then
          Result := Result + ' ';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDTree.KDTreeDistance(const v1, v2: TKDTree_Vec): Double;
  function KPow(const v: TKDTree_VecType): Double; inline;
  begin
    Result := v * v;
  end;

var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to length(v1) - 1 do
      Result := Result + KPow(v2[i] - v1[i]);
end;

procedure Test_BuildC(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
var
  i: Integer;
begin
  for i := 0 to length(Source.Buff) - 1 do
      Source.Buff[i] := PKDTree_DynamicVecBuffer(Data)^[IndexFor][i];
end;

procedure Test_KDTree(const Axis: Integer);
var
  TKDTree_Test: TKDTree;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultIndex: TDynamicIndexArray;
  KMeanBuildOutIndex: TDynamicIndexArray;
  errored: Boolean;
  TestBuff: TKDTree_DynamicVecBuffer;

begin
  errored := False;
  DoStatusNoLn('test KDTree...');
  t := GetTimeTick;

  TKDTree_Test := TKDTree.Create(Axis);

  DoStatusNoLn('...');
  SetLength(TestBuff, 1000);
  for i := 0 to length(TestBuff) - 1 do
    begin
      SetLength(TestBuff[i], TKDTree_Test.AxisCount);
      for j := 0 to TKDTree_Test.AxisCount - 1 do
          TestBuff[i][j] := i + 1;
    end;

  DoStatusNoLn('...');
  {$IFDEF FPC}
  TKDTree_Test.BuildKDTreeC(length(TestBuff), @TestBuff, @Test_BuildC);
  {$ELSE FPC}
  TKDTree_Test.BuildKDTreeC(length(TestBuff), @TestBuff, Test_BuildC);
  {$ENDIF FPC}
  { parallel search test }
  DoStatusNoLn('...');
  SetLength(TestResultIndex, length(TestBuff));
  TKDTree_Test.Search(TestBuff, TestResultIndex);
  for i := 0 to length(TestResultIndex) - 1 do
    if TKDTree.KDTreeDistance(TestBuff[TestResultIndex[i]], TestBuff[i]) <> 0 then
        errored := True;

  DoStatusNoLn('...');
  TKDTree_Test.Clear;
  { kMean test }
  {$IFDEF FPC}
  TKDTree_Test.BuildKDTreeWithClusterC(length(TestBuff), 10, 1, KMeanBuildOutIndex, @TestBuff, @Test_BuildC);
  {$ELSE FPC}
  TKDTree_Test.BuildKDTreeWithClusterC(length(TestBuff), 10, 1, KMeanBuildOutIndex, @TestBuff, Test_BuildC);
  {$ENDIF FPC}
  { parallel search test }
  TKDTree_Test.Search(TestBuff, TestResultIndex);
  for i := 0 to length(TestResultIndex) - 1 do
    if KMeanBuildOutIndex[i] <> TestResultIndex[i] then
        errored := True;

  for i := 0 to length(TestBuff) - 1 do
      SetLength(TestBuff[i], 0);

  SetLength(TestBuff, 0);
  SetLength(TestResultIndex, 0);
  TKDTree_Test.Clear;

  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDTree_Test);
end;

end.
