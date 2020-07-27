program KDTree_Analysis_EasyConsoleDemo;

{$APPTYPE CONSOLE}

{$R *.res}

{
  该Demo直观的演示了KDTree数据结构以及查找机制
}

uses
  SysUtils,
  CoreClasses,
  DoStatusIO,
  PascalStrings,
  UnicodeMixedLib,
  MemoryStream64,
  FastKDTreeD,
  KM,
  MH_1;

procedure Demo;
var
  i, j: Integer;
  buff: TKDT1DD.TKDT1DD_DynamicVecBuffer;
  outIndex: TKMIntegerArray;
  k1t: TKDT1DD;
  k1r: TKDT1DD.PKDT1DD_Node;
  d: Double;
  SearchedCounter: NativeInt;
  n: string;
  NearestNodes: TCoreClassList;
begin
  { TKDT1DD是指k维空间是1维的Double类型 }
  k1t := TKDT1DD.Create;

  NearestNodes := TCoreClassList.Create;

  { 这一是对10000个数据做k-means++ clusterization分类，分出100个k-means，然后再基于分出的数据进行kdtree查找 }
  { k-means如果分类多了，BuildKDTreeWithCluster将会非常慢 }
  SetLength(buff, 10000);
  for i := 0 to Length(buff) - 1 do
    for j := 0 to KDT1DD_Axis - 1 do
        buff[i][j] := umlRandomRangeD(-3000, 3000);
  k1t.BuildKDTreeWithCluster(buff, 100, 1, outIndex);

  repeat
    DoStatus('wait input (cmd:buff,tree,origin,exit, number:[x,x,x,x])');
    n := '';
    readln(n);
    if n <> '' then
      begin
        if umlMultipleMatch(['buff'], n) then
            k1t.PrintBuffer
        else if umlMultipleMatch(['origin'], n) then
          begin
            for i := 0 to Length(buff) - 1 do
                DoStatus('%d. %s ', [i, TKDT1DD.Vec(buff[i])]);
          end
        else if umlMultipleMatch(['tree'], n) then
            k1t.PrintNodeTree(k1t.RootNode)
        else if SmithWatermanCompare(['exit'], n) > 0.5 then
            break
        else
          begin
            NearestNodes.Clear;
            k1r := k1t.Search(TKDT1DD.Vec(n), d, SearchedCounter, NearestNodes);

            DoStatus(Format('finded total:%d Nearest:%d distance:%f', [SearchedCounter, k1r^.Vec^.index,
              sqrt(TKDT1DD.Distance(TKDT1DD.Vec(n), k1r^.Vec^.buff))]));
            for i := 0 to NearestNodes.Count - 1 do
                DoStatus(Format('index:%d distance:%f',
                [
                TKDT1DD.PKDT1DD_Node(NearestNodes[i])^.Vec^.index,
                sqrt(TKDT1DD.Distance(TKDT1DD.PKDT1DD_Node(NearestNodes[i])^.Vec^.buff, k1r^.Vec^.buff))
                ]));
          end;
      end
    else
        k1t.PrintBuffer;
  until False;

  disposeObject([k1t, NearestNodes]);
end;

begin
  System.ReportMemoryLeaksOnShutdown := True;
  Demo;

end.
