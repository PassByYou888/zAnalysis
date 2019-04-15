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
unit PropsEquipment_PrimaryWeapon;

{$INCLUDE zDefine.inc}

interface

uses PropsBase, BioBase, CoreClasses, SpellBase, BioDataModule,
  PropsEquipmentBase, PropsMaterialUnit;

const
  PrimaryWeaponType: TPropsMaterialTypes = [
    pmtIash,
    pmtIWAXBT, pmtIWAXGR, pmtIWAXHN, pmtIWBLCL, pmtIWBLFH, pmtIWBLFL, pmtIWBLHL,
    pmtIWBLHW, pmtIWBLML, pmtIWBLMS, pmtIWBWLC, pmtIWBWLN, pmtIWBWSC, pmtIWBWSH,
    pmtIWBWSL, pmtIWBWXH, pmtIWBWXL, pmtIWBWXR, pmtIWDBAX, pmtIWDBMA, pmtIWDBQS,
    pmtIWDBSW, pmtIWMGRD, pmtIWMGST, pmtIWMGWN, pmtIWPLHB, pmtIWPLSC, pmtIWPLSS,
    pmtIWSPKA, pmtIWSPKU, pmtIWSPSC, pmtIWSWBS, pmtIWSWDG,
    pmtIWSWGS, pmtIWSWKA, pmtIWSWLS, pmtIWSWRP, pmtIWSWSC, pmtIWSWSS, pmtIWTHAX];

type
  TProps_Equipment_PrimaryWeapon = class(TProps_Equipment)
  private
    FMinAttack: Integer;
    FMaxAttack: Integer;
    FDamageType: TDamageType;
  public
    constructor Create(AOwner: TCoreClassPersistent); override;
    destructor Destroy; override;

    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromStream(stream: TCoreClassStream); override;

    property MinAttack: Integer read FMinAttack write FMinAttack;
    property MaxAttack: Integer read FMaxAttack write FMaxAttack;
    property DamageType: TDamageType read FDamageType write FDamageType;
  end;

implementation

constructor TProps_Equipment_PrimaryWeapon.Create(AOwner: TCoreClassPersistent);
begin
  inherited Create(AOwner);
  FMinAttack := 1;
  FMaxAttack := 1;
end;

destructor TProps_Equipment_PrimaryWeapon.Destroy;
begin
  inherited Destroy;
end;

procedure TProps_Equipment_PrimaryWeapon.SaveToStream(stream: TCoreClassStream);
begin
  inherited SaveToStream(stream);
end;

procedure TProps_Equipment_PrimaryWeapon.LoadFromStream(stream: TCoreClassStream);
begin
  inherited LoadFromStream(stream);
end;

initialization

finalization

end. 
 
