{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit PropsConsumeBase;

{$INCLUDE zDefine.inc}

interface

uses PropsBase, BioBase, CoreClasses, SpellBase, UnicodeMixedLib;

type
  TProps_Consume = class(TProps)
  private
    FMaxUsed: Integer;
    FUsed: Integer;
    FInfinite: Boolean;
  public
    constructor Create(AOwner: TCoreClassPersistent); override;
    destructor Destroy; override;

    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromStream(stream: TCoreClassStream); override;

    procedure Execute; virtual;

    property MaxUsed: Integer read FMaxUsed write FMaxUsed;
    property Used: Integer read FUsed write FUsed;
    property Infinite: Boolean read FInfinite write FInfinite;
  end;

  TProps_ConsumeOfSpell = class(TProps_Consume)
  private
    FSpell: TSpellBase;
    FSpellRegName: U_String;
  public
    constructor Create(AOwner: TCoreClassPersistent); override;
    destructor Destroy; override;

    procedure Execute; override;

    property Spell: TSpellBase read FSpell;

    property SpellRegName: U_String read FSpellRegName write FSpellRegName;
  end;

implementation

uses DataFrameEngine, MemoryStream64;

constructor TProps_Consume.Create(AOwner: TCoreClassPersistent);
begin
  inherited Create(AOwner);
  FMaxUsed := 1;
  FUsed := 0;
  FInfinite := True;
end;

destructor TProps_Consume.Destroy;
begin
  inherited Destroy;
end;

procedure TProps_Consume.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;

  ms := TMemoryStream64.Create;
  inherited SaveToStream(ms);
  ms.Position := 0;
  df.WriteStream(ms);
  DisposeObject(ms);

  //
  df.WriteInteger(FMaxUsed);
  df.WriteInteger(FUsed);
  df.WriteBool(FInfinite);

  df.EncodeTo(stream);
  DisposeObject(df);
end;

procedure TProps_Consume.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  ms := TMemoryStream64.Create;
  df.Reader.ReadStream(ms);
  ms.Position := 0;
  inherited LoadFromStream(stream);
  DisposeObject(ms);

  //
  FMaxUsed := df.Reader.ReadInteger;
  FUsed := df.Reader.ReadInteger;
  FInfinite := df.Reader.ReadBool;

  DisposeObject(df);
end;

procedure TProps_Consume.Execute;
begin
end;

constructor TProps_ConsumeOfSpell.Create(AOwner: TCoreClassPersistent);
begin
  inherited Create(AOwner);
end;

destructor TProps_ConsumeOfSpell.Destroy;
begin
  inherited Destroy;
end;

procedure TProps_ConsumeOfSpell.Execute;
begin
  inherited Execute;
end;

end. 
 
