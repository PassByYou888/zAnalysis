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
unit BaseRULEClasses;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses;

type
  TRegionBase = class(TCoreClassPersistent)
  public
    UserObject: TCoreClassObject;
    constructor Create;
  end;

  TBattleBase = class(TCoreClassPersistent)
  public
    UserObject: TCoreClassObject;
    constructor Create;
  end;

  TPropsBase = class(TCoreClassPersistent)
  public
    UserObject: TCoreClassObject;
    constructor Create;
  end;

  TPropsBagBase = class(TCoreClassPersistent)
  public
    UserObject: TCoreClassObject;
    constructor Create;
  end;

implementation

constructor TRegionBase.Create;
begin
  inherited Create;
  UserObject := nil;
end;

constructor TBattleBase.Create;
begin
  inherited Create;
  UserObject := nil;
end;

constructor TPropsBase.Create;
begin
  inherited Create;
  UserObject := nil;
end;

constructor TPropsBagBase.Create;
begin
  inherited Create;
  UserObject := nil;
end;

end.
 
 
