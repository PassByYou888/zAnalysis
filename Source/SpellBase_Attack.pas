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
unit SpellBase_Attack;

{$INCLUDE zDefine.inc}

interface

uses SpellBase, BioBase, BioDataModule, NumberBase, PascalStrings, UnicodeMixedLib,
  CoreClasses;

type
  TSpellBase_Attack = class(TSpellBase)
  public
    constructor Create(AOwner: TBioBase); override;
    destructor Destroy; override;

    class function Description: U_String; override;
    class function Name: U_String; override;
    class function Category: U_String; override;
    class function CanInstall: Boolean; override;

    procedure RecalcCooldown;
  end;

  TSpellBase_Attack_Melee = class(TSpellBase_Attack)
  end;

  TSpellBase_Attack_Range = class(TSpellBase_Attack)
  end;

implementation

uses BaseRULEClasses, CoreRULE, MediaCenter;

constructor TSpellBase_Attack.Create(AOwner: TBioBase);
begin
  inherited Create(AOwner);
  RecalcCooldown;
end;

destructor TSpellBase_Attack.Destroy;
begin
  inherited Destroy;
end;

class function TSpellBase_Attack.Description: U_String;
begin

end;

class function TSpellBase_Attack.Name: U_String;
begin

end;

class function TSpellBase_Attack.Category: U_String;
begin

end;

class function TSpellBase_Attack.CanInstall: Boolean;
begin

end;

procedure TSpellBase_Attack.RecalcCooldown;
begin

end;

end.
 
 
