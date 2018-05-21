{ ****************************************************************************** }
{ * sound engine                                                               * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }
unit zSound;

{$I zDefine.inc}

interface

uses CoreClasses, MemoryStream64, UnicodeMixedLib,
  ObjectDataManager, LibraryManager, PascalStrings, ListEngine;

type
  TzSound = class(TCoreClassPersistent)
  protected
    FSearchDB             : TCoreClassObject;
    FTempPath             : SystemString;
    FCacheFileList        : THashVariantList;
    FLastPlaySoundFilename: SystemString;

    procedure DoPrepareMusic(filename: SystemString); virtual; abstract;
    procedure DoPlayMusic(filename: SystemString); virtual; abstract;
    procedure DoStopMusic; virtual; abstract;

    procedure DoPrepareAmbient(filename: SystemString); virtual; abstract;
    procedure DoPlayAmbient(filename: SystemString); virtual; abstract;
    procedure DoStopAmbient; virtual; abstract;

    procedure DoPrepareSound(filename: SystemString); virtual; abstract;
    procedure DoPlaySound(filename: SystemString); virtual; abstract;
    procedure DoStopSound(filename: SystemString); virtual; abstract;

    procedure DoStopAll; virtual; abstract;

    function DoIsPlaying(filename: SystemString): Boolean; virtual; abstract;

    function SaveSoundAsLocalFile(filename: SystemString): SystemString; virtual;
    function SoundReadyOk(filename: SystemString): Boolean; virtual;
  public
    constructor Create(ATempPath: SystemString); virtual;
    destructor Destroy; override;

    procedure PrepareMusic(filename: SystemString);
    procedure PlayMusic(filename: SystemString);
    procedure StopMusic;

    procedure PrepareAmbient(filename: SystemString);
    procedure PlayAmbient(filename: SystemString);
    procedure StopAmbient;

    procedure PrepareSound(filename: SystemString);
    procedure PlaySound(filename: SystemString);
    procedure StopSound(filename: SystemString);

    procedure StopAll;

    procedure Progress(deltaTime: Double); virtual;

    property SearchDB: TCoreClassObject read FSearchDB write FSearchDB;
    property LastPlaySoundFilename: SystemString read FLastPlaySoundFilename;
  end;

  TSoundEngineClass = class of TzSound;

var
  DefaultSoundEngineClass: TSoundEngineClass;

implementation

uses MediaCenter;

function TzSound.SaveSoundAsLocalFile(filename: SystemString): SystemString;
begin
  Result := filename;
end;

function TzSound.SoundReadyOk(filename: SystemString): Boolean;
begin
  Result := False;
end;

constructor TzSound.Create(ATempPath: SystemString);
begin
  inherited Create;
  FSearchDB := nil;
  FTempPath := ATempPath;
  FCacheFileList := THashVariantList.Create;
  FLastPlaySoundFilename := '';
end;

destructor TzSound.Destroy;
begin
  DisposeObject(FCacheFileList);
  inherited Destroy;
end;

procedure TzSound.PrepareMusic(filename: SystemString);
begin
  try
    if SoundReadyOk(filename) then
        DoPrepareMusic(filename)
    else
        DoPrepareMusic(SaveSoundAsLocalFile(filename));
  except
  end;
end;

procedure TzSound.PlayMusic(filename: SystemString);
begin
  try
    if SoundReadyOk(filename) then
        DoPlayMusic(filename)
    else
        DoPlayMusic(SaveSoundAsLocalFile(filename));
  except
  end;
end;

procedure TzSound.StopMusic;
begin
  try
      DoStopMusic;
  except
  end;
end;

procedure TzSound.PrepareAmbient(filename: SystemString);
begin
  try
    if SoundReadyOk(filename) then
        DoPrepareAmbient(filename)
    else
        DoPrepareAmbient(SaveSoundAsLocalFile(filename));
  except
  end;
end;

procedure TzSound.PlayAmbient(filename: SystemString);
begin
  try
    if SoundReadyOk(filename) then
        DoPlayAmbient(filename)
    else
        DoPlayAmbient(SaveSoundAsLocalFile(filename));
  except
  end;
end;

procedure TzSound.StopAmbient;
begin
  try
      DoStopAmbient
  except
  end;
end;

procedure TzSound.PrepareSound(filename: SystemString);
begin
  try
    if SoundReadyOk(filename) then
        DoPrepareSound(filename)
    else
        DoPrepareSound(SaveSoundAsLocalFile(filename));
  except
  end;
end;

procedure TzSound.PlaySound(filename: SystemString);
begin
  try
    FLastPlaySoundFilename := filename;
    if SoundReadyOk(filename) then
      begin
        DoPlaySound(filename);
      end
    else
        DoPlaySound(SaveSoundAsLocalFile(filename));
  except
  end;
end;

procedure TzSound.StopSound(filename: SystemString);
begin
  try
    if FCacheFileList.Exists(filename) then
        DoStopSound(FCacheFileList[filename])
    else if SoundReadyOk(filename) then
        DoStopSound(filename);
  except
  end;
end;

procedure TzSound.StopAll;
begin
  try
      DoStopAll;
  except
  end;
end;

procedure TzSound.Progress(deltaTime: Double);
begin
end;

initialization

DefaultSoundEngineClass := TzSound;

end.
