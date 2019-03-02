{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit CampRelationBase;

{$INCLUDE zDefine.inc}

interface

uses UnicodeMixedLib, CoreClasses, PascalStrings;

type
  TRelationType  = (rtPlayer, rtNeutrally, rtAntagonize, rtAlly);
  TRelationTypes = set of TRelationType;

  TCampRelationData = record
    Camp1, Camp2: U_String;
    RelationType: TRelationType;
    Weight: Double;
  end;

  PIDRelationData = ^TCampRelationData;

  TCampRelation = class(TCoreClassObject)
  private
    FIDRelationList: TCoreClassList;
    FDefaultType: TRelationType;
  public
    constructor Create;
    destructor Destroy; override;
    function GetRelationType(ACamp1, ACamp2: U_String): TRelationType;
    function GetRelationWeight(ACamp1, ACamp2: U_String): Double;
    function ExistsRelation(ACamp1, ACamp2: U_String): Boolean;

    procedure Clear;
    procedure ChangeRelation(ACamp1, ACamp2: U_String; ARelationType: TRelationType; AWeight: Double);
    procedure DeleteRelation(ACamp1, ACamp2: U_String);
    procedure Delete(AID: U_String);

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure AsStrings(ATo: TCoreClassStrings);

    property DefaultType: TRelationType read FDefaultType write FDefaultType;
  end;

function RelationTypeToStr(t: TRelationType): U_String;
function StrToRelationType(s: U_String): TRelationType;
function RelationDataToStr(var Data: TCampRelationData): U_String;
function StrToRelationData(Data: U_String): TCampRelationData;

implementation

uses DataFrameEngine, MemoryStream64, DoStatusIO;

function RelationTypeToStr(t: TRelationType): U_String;
begin
  case t of
    rtPlayer: Result := 'Player';
    rtNeutrally: Result := 'Neutrally';
    rtAntagonize: Result := 'Antagonize';
    rtAlly: Result := 'Ally';
    else raise CoreClassException.Create('Unknow Relation Type');
  end;
end;

function StrToRelationType(s: U_String): TRelationType;
begin
  if umlMultipleMatch(True, 'Player', s) then
      Result := rtPlayer
  else if umlMultipleMatch(True, 'Neutrally', s) then
      Result := rtNeutrally
  else if umlMultipleMatch(True, 'Antagonize', s) then
      Result := rtAntagonize
  else if umlMultipleMatch(True, 'Ally', s) then
      Result := rtAlly
  else
      raise CoreClassException.Create('Unknow Relation Type: ' + s.Text);
end;

function RelationDataToStr(var Data: TCampRelationData): U_String;
begin
  Result.Text := Data.Camp1 + ',' + Data.Camp2 + ',' + RelationTypeToStr(Data.RelationType).Text + ',' + umlFloatToStr(Data.Weight);
end;

function StrToRelationData(Data: U_String): TCampRelationData;
var
  n: U_String;
begin
  n := Data;
  Result.Camp1 := umlLowerCase(umlTrimSpace(umlGetFirstStr(n, ',;')));
  n := umlDeleteFirstStr(n, ',;');

  Result.Camp2 := umlLowerCase(umlTrimSpace(umlGetFirstStr(n, ',;')));
  n := umlDeleteFirstStr(n, ',;');

  Result.RelationType := StrToRelationType(umlLowerCase(umlTrimSpace(umlGetFirstStr(n, ',;'))));
  n := umlDeleteFirstStr(n, ',;');

  Result.Weight := umlStrToFloat(umlTrimSpace(n), 0);
end;

constructor TCampRelation.Create;
begin
  inherited Create;
  FIDRelationList := TCoreClassList.Create;
  FDefaultType := rtNeutrally;
end;

destructor TCampRelation.Destroy;
begin
  Clear;
  DisposeObject(FIDRelationList);
  inherited Destroy;
end;

function TCampRelation.GetRelationType(ACamp1, ACamp2: U_String): TRelationType;
var
  i: Integer;
  p: PIDRelationData;
begin
  if ACamp1.Same(ACamp2) then
    begin
      Result := rtAlly;
      Exit;
    end;

  p := nil;
  if FIDRelationList.Count > 0 then
    for i := 0 to FIDRelationList.Count - 1 do
      begin
        with PIDRelationData(FIDRelationList[i])^ do
          begin
            if ((Camp1.Same(ACamp1)) and (Camp2.Same(ACamp2))) or
              ((Camp2.Same(ACamp1)) and (Camp1.Same(ACamp2))) then
              begin
                p := PIDRelationData(FIDRelationList[i]);
                Break;
              end;
          end;
      end;

  if p = nil then
      Result := FDefaultType
  else
      Result := p^.RelationType;
end;

function TCampRelation.GetRelationWeight(ACamp1, ACamp2: U_String): Double;
var
  ANewCamp1, ANewCamp2: U_String;
  i: Integer;
  p: PIDRelationData;
begin
  ANewCamp1 := umlLowerCase(ACamp1);
  ANewCamp2 := umlLowerCase(ACamp2);

  if ANewCamp1 = ANewCamp2 then
    begin
      Result := 0;
      Exit;
    end;

  p := nil;
  if FIDRelationList.Count > 0 then
    for i := 0 to FIDRelationList.Count - 1 do
      begin
        with PIDRelationData(FIDRelationList[i])^ do
          begin
            if ((Camp1 = ANewCamp1) and (Camp2 = ANewCamp2)) or
              ((Camp2 = ANewCamp1) and (Camp1 = ANewCamp2)) then
              begin
                p := PIDRelationData(FIDRelationList[i]);
                Break;
              end;
          end;
      end;

  if p = nil then
      Result := 0
  else
      Result := p^.Weight;
end;

function TCampRelation.ExistsRelation(ACamp1, ACamp2: U_String): Boolean;
var
  ANewCamp1, ANewCamp2: U_String;
  i: Integer;
  p: PIDRelationData;
begin
  ANewCamp1 := umlLowerCase(ACamp1);
  ANewCamp2 := umlLowerCase(ACamp2);

  if ANewCamp1 = ANewCamp2 then
    begin
      Result := True;
      Exit;
    end;

  p := nil;
  if FIDRelationList.Count > 0 then
    for i := 0 to FIDRelationList.Count - 1 do
      begin
        with PIDRelationData(FIDRelationList[i])^ do
          begin
            if ((Camp1 = ANewCamp1) and (Camp2 = ANewCamp2)) or
              ((Camp2 = ANewCamp1) and (Camp1 = ANewCamp2)) then
              begin
                p := PIDRelationData(FIDRelationList[i]);
                Break;
              end;
          end;
      end;

  Result := p <> nil;
end;

procedure TCampRelation.Clear;
begin
  while FIDRelationList.Count > 0 do
    begin
      Dispose(PIDRelationData(FIDRelationList[0]));
      FIDRelationList.Delete(0);
    end;
end;

procedure TCampRelation.ChangeRelation(ACamp1, ACamp2: U_String; ARelationType: TRelationType; AWeight: Double);
var
  ANewCamp1, ANewCamp2: U_String;
  i: Integer;
  p: PIDRelationData;
begin
  p := nil;

  ANewCamp1 := umlLowerCase(ACamp1);
  ANewCamp2 := umlLowerCase(ACamp2);

  if FIDRelationList.Count > 0 then
    for i := 0 to FIDRelationList.Count - 1 do
      begin
        with PIDRelationData(FIDRelationList[i])^ do
          begin
            if ((Camp1 = ANewCamp1) and (Camp2 = ANewCamp2)) or
              ((Camp2 = ANewCamp1) and (Camp1 = ANewCamp2)) then
              begin
                p := PIDRelationData(FIDRelationList[i]);
                Break;
              end;
          end;
      end;

  if p = nil then
    begin
      new(p);
      p^.Camp1 := ANewCamp1;
      p^.Camp2 := ANewCamp2;
      FIDRelationList.Add(p);
    end;

  p^.RelationType := ARelationType;
  p^.Weight := AWeight;
end;

procedure TCampRelation.DeleteRelation(ACamp1, ACamp2: U_String);
var
  ANewCamp1, ANewCamp2: U_String;
  i: Integer;
  p: PIDRelationData;
begin
  ANewCamp1 := umlLowerCase(ACamp1);
  ANewCamp2 := umlLowerCase(ACamp2);
  i := 0;
  while FIDRelationList.Count > i do
    begin
      p := PIDRelationData(FIDRelationList[i]);
      if ((p^.Camp1 = ANewCamp1) and (p^.Camp2 = ANewCamp2)) or
        ((p^.Camp2 = ANewCamp1) and (p^.Camp1 = ANewCamp2)) then
        begin
          Dispose(p);
          FIDRelationList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TCampRelation.Delete(AID: U_String);
var
  ANewID: U_String;
  i: Integer;
  p: PIDRelationData;
begin
  ANewID := umlLowerCase(AID);
  i := 0;
  while FIDRelationList.Count > i do
    begin
      p := PIDRelationData(FIDRelationList[i]);
      if (p^.Camp1 = ANewID) or (p^.Camp2 = ANewID) then
        begin
          Dispose(p);
          FIDRelationList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TCampRelation.SaveToStream(stream: TCoreClassStream);
var
  aData: TCoreClassStrings;
  i: Integer;
  p: PIDRelationData;
begin
  aData := TCoreClassStringList.Create;
  for i := 0 to FIDRelationList.Count - 1 do
    begin
      p := PIDRelationData(FIDRelationList[i]);
      aData.Add(RelationDataToStr(p^).Text);
    end;
  aData.SaveToStream(stream);
  DisposeObject(aData);
end;

procedure TCampRelation.LoadFromStream(stream: TCoreClassStream);
var
  aData: TCoreClassStrings;
  i: Integer;
  p: PIDRelationData;
begin
  Clear;

  aData := TCoreClassStringList.Create;
  aData.LoadFromStream(stream);
  for i := 0 to aData.Count - 1 do
    if (aData[i] <> '') or (not umlMultipleMatch(False, ';*', aData[i])) then
      begin
        new(p);
        p^ := StrToRelationData(aData[i]);
        FIDRelationList.Add(p);
      end;
  DisposeObject(aData);
end;

procedure TCampRelation.AsStrings(ATo: TCoreClassStrings);
var
  i: Integer;
  p: PIDRelationData;
begin
  ATo.Clear;
  for i := 0 to FIDRelationList.Count - 1 do
    begin
      p := PIDRelationData(FIDRelationList[i]);
      umlAddNewStrTo(p^.Camp1, ATo);
      umlAddNewStrTo(p^.Camp2, ATo);
    end;
end;

end. 
 
 
