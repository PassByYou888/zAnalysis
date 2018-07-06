{ ****************************************************************************** }
{ * sound engine                                                               * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit zSound;

{$INCLUDE zDefine.inc}

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

    procedure DoPrepareMusic(fileName: SystemString); virtual; abstract;
    procedure DoPlayMusic(fileName: SystemString); virtual; abstract;
    procedure DoStopMusic; virtual; abstract;

    procedure DoPrepareAmbient(fileName: SystemString); virtual; abstract;
    procedure DoPlayAmbient(fileName: SystemString); virtual; abstract;
    procedure DoStopAmbient; virtual; abstract;

    procedure DoPrepareSound(fileName: SystemString); virtual; abstract;
    procedure DoPlaySound(fileName: SystemString); virtual; abstract;
    procedure DoStopSound(fileName: SystemString); virtual; abstract;

    procedure DoStopAll; virtual; abstract;

    function DoIsPlaying(fileName: SystemString): Boolean; virtual; abstract;

    function SaveSoundAsLocalFile(fileName: SystemString): SystemString; virtual;
    function SoundReadyOk(fileName: SystemString): Boolean; virtual;
  public
    constructor Create(ATempPath: SystemString); virtual;
    destructor Destroy; override;

    procedure PrepareMusic(fileName: SystemString);
    procedure PlayMusic(fileName: SystemString);
    procedure StopMusic;

    procedure PrepareAmbient(fileName: SystemString);
    procedure PlayAmbient(fileName: SystemString);
    procedure StopAmbient;

    procedure PrepareSound(fileName: SystemString);
    procedure PlaySound(fileName: SystemString);
    procedure StopSound(fileName: SystemString);

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

function TzSound.SaveSoundAsLocalFile(fileName: SystemString): SystemString;
begin
  Result := fileName;
end;

function TzSound.SoundReadyOk(fileName: SystemString): Boolean;
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

procedure TzSound.PrepareMusic(fileName: SystemString);
begin
  try
    if SoundReadyOk(fileName) then
        DoPrepareMusic(fileName)
    else
        DoPrepareMusic(SaveSoundAsLocalFile(fileName));
  except
  end;
end;

procedure TzSound.PlayMusic(fileName: SystemString);
begin
  try
    if SoundReadyOk(fileName) then
        DoPlayMusic(fileName)
    else
        DoPlayMusic(SaveSoundAsLocalFile(fileName));
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

procedure TzSound.PrepareAmbient(fileName: SystemString);
begin
  try
    if SoundReadyOk(fileName) then
        DoPrepareAmbient(fileName)
    else
        DoPrepareAmbient(SaveSoundAsLocalFile(fileName));
  except
  end;
end;

procedure TzSound.PlayAmbient(fileName: SystemString);
begin
  try
    if SoundReadyOk(fileName) then
        DoPlayAmbient(fileName)
    else
        DoPlayAmbient(SaveSoundAsLocalFile(fileName));
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

procedure TzSound.PrepareSound(fileName: SystemString);
begin
  try
    if SoundReadyOk(fileName) then
        DoPrepareSound(fileName)
    else
        DoPrepareSound(SaveSoundAsLocalFile(fileName));
  except
  end;
end;

procedure TzSound.PlaySound(fileName: SystemString);
begin
  try
    FLastPlaySoundFilename := fileName;
    if SoundReadyOk(fileName) then
      begin
        DoPlaySound(fileName);
      end
    else
        DoPlaySound(SaveSoundAsLocalFile(fileName));
  except
  end;
end;

procedure TzSound.StopSound(fileName: SystemString);
begin
  try
    if FCacheFileList.Exists(fileName) then
        DoStopSound(FCacheFileList[fileName])
    else if SoundReadyOk(fileName) then
        DoStopSound(fileName);
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
 
