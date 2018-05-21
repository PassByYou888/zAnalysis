unit WordAnalysisFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.Edit,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.StdCtrls,

  Math,

  CoreClasses, DoStatusIO, Learn, LearnTypes, PascalStrings, TextParsing, ListEngine;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    StringColumn2: TStringColumn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Dict : TListPascalString;
    dHash: THashVariantList;
    lr   : TLearn;
    procedure DoStatusM(AText: SystemString; const ID: Integer);
    procedure LearnMemoText;
    procedure DisplayInputClassifier(AText: TPascalString);
    function SmithWatermanSearch(v: TPascalString; var idx: Integer): TPascalString;
    function GetWordFromVec(v: TLVec): TPascalString;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.DoStatusM(AText: SystemString; const ID: Integer);
begin
  Memo2.Lines.Add(AText);
  Memo2.GoToTextEnd;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  DisplayInputClassifier(Edit1.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusM);
  Dict := TListPascalString.Create;
  dHash := THashVariantList.Create(128);
  lr := nil;

  Button1Click(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  disposeObject(Dict);
  disposeObject(dHash);

  if lr <> nil then
      disposeObject(lr);
end;

procedure TForm1.LearnMemoText;
var
  t    : TTextParsing;
  i, j : Integer;
  vArry: TLVec;
  n    : TPascalString;
begin
  (*
    这里用到的不是真正的词频技术，只是Demo原理
    实际使用时，请自己编写文本向量化程序
  *)

  Dict.Clear;
  dHash.Clear;
  lr.Clear;

  t := TTextParsing.Create(Memo1.Text, tsText, nil);

  Dict.Add('');
  vArry := LVec(5);
  lr.AddMemory(vArry, [0]);

  for i := 0 to t.TokenCount - 1 do
    if t.Tokens[i]^.tokenType = ttAscii then
      begin
        Dict.Add(t.Tokens[i]^.Text);

        if not dHash.Exists(t.Tokens[i]^.Text) then
            dHash[t.Tokens[i]^.Text] := Dict.Count - 1;
      end;

  for i := 1 to Dict.Count - 1 do
    begin
      n := '';
      for j := 0 to 4 do
        begin
          if i + j < Dict.Count then
            begin
              vArry[j] := dHash[Dict[i + j]];

              if n.Len = 0 then
                  n := Dict[i + j]
              else
                  n.Append(' ' + Dict[i + j]);
            end
          else
              vArry[j] := 0;
        end;
      if n.Len > 0 then
        begin
          lr.AddMemory(vArry, [i]);
          DoStatus('学习向量: %s = %s', [LVec(vArry).Text, n.Text]);
        end;
    end;

  disposeObject(t);

  lr.TrainP(10, procedure(const LSender: TLearn; const state: Boolean)
    begin
      if state then
          DoStatus('训练完成');
    end);
end;

function TForm1.GetWordFromVec(v: TLVec): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(v) - 1 do
      Result.Append(#32 + Dict[Round(v[i])]);
end;

function TForm1.SmithWatermanSearch(v: TPascalString; var idx: Integer): TPascalString;
var
  i   : Integer;
  t, k: Double;
begin
  Result := v;
  if Dict.Count = 0 then
      exit;
  Result := Dict[0];
  k := v.SmithWaterman(Dict[0]);
  for i := 1 to Dict.Count - 1 do
    begin
      t := v.SmithWaterman(Dict[i]);
      if t > k then
        begin
          k := t;
          Result := Dict[i];
        end;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if lr <> nil then
      disposeObject(lr);
  lr := TLearn.CreateClassifier(ltKDT, 5);
  LearnMemoText;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if lr <> nil then
      disposeObject(lr);
  lr := TLearn.CreateClassifier(ltForest, 5);
  LearnMemoText;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if lr <> nil then
      disposeObject(lr);
  lr := TLearn.CreateClassifier(ltLBFGS, 5);
  LearnMemoText;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if lr <> nil then
      disposeObject(lr);
  lr := TLearn.CreateClassifier(ltLM, 5);
  LearnMemoText;
end;

procedure TForm1.DisplayInputClassifier(AText: TPascalString);
var
  t        : TTextParsing;
  i, j, idx: Integer;
  vIn, vOut: TLVec;
  n        : TPascalString;
begin
  t := TTextParsing.Create(AText, tsText, nil);
  j := 0;
  vIn := LVec(5);
  n := '';
  for i := 0 to t.TokenCount - 1 do
    if t.Tokens[i]^.tokenType in [ttAscii, ttNumber] then
      begin
        n.Append(#32 + t.Tokens[i]^.Text);
        if dHash.Exists(t.Tokens[i]^.Text) then
            vIn[j] := dHash.GetDefaultValue(t.Tokens[i]^.Text, 0)
        else
          begin
            SmithWatermanSearch(t.Tokens[i]^.Text, idx);
            vIn[j] := idx;
          end;
        inc(j);

        if j >= 5 then
            break;
      end;
  disposeObject(t);

  DoStatus('%s -> [%s] -> %s', [AText.Text, LVec(vIn).Text, GetWordFromVec(vIn).Text]);

  if lr.process(@vIn, @vOut) then
    begin
      StringGrid1.RowCount := length(vOut);
      for i := 0 to length(vOut) - 1 do
        if i < lr.Count then
          begin
            try
              StringGrid1.Cells[0, i] := GetWordFromVec(lr[i]^.m_in);
              StringGrid1.Cells[1, i] := Format('%f', [vOut[i]]);
            except
            end;
          end;

      DoStatus('神经网络判定 %s = %s', [AText.Text, GetWordFromVec(lr[LMaxVecIndex(vOut)]^.m_in).Text]);
    end;
end;

end.
