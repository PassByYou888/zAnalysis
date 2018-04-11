(*
Copyright (c) 1998-2014 by Carlotta Calandra. All rights reserved.
Copyright (c) 2011-2014 by Xequte software.

This software comes without express or implied warranty.
In no case shall the author be liable for any damage or unwanted behavior of any
computer hardware and/or software.

Author grants you the right to include the component
in your application, whether COMMERCIAL, SHAREWARE, or FREEWARE.

ImageEn, IEvolution and ImageEn ActiveX may not be included in any
commercial, shareware or freeware libraries or components.

www.ImageEn.com
*)

(*
File version 1009
*)

unit ietwain;


{$R-}
{$Q-}

{$I ie.inc}


{$ifdef IEINCLUDEIEXACQUIRE}


interface

uses
  Windows, Classes, hyieutils;

type

  TW_UINT32 = ULONG;
  TW_UINT16 = Word;
  TW_MEMREF = Pointer;
  TW_INT32 = LongInt;
  TW_INT16 = SmallInt;
  TW_STR32 = array[0..33] of AnsiChar;
  TW_STR255 = array[0..255] of AnsiChar;
  TW_BOOL = WordBool;
  TW_HANDLE = THandle;
  TW_UINT8 = Byte;

  pTW_ONEVALUE = ^TW_ONEVALUE;
  pTW_BOOL = ^TW_BOOL;
  pTW_IMAGEMEMXFER = ^TW_IMAGEMEMXFER;
  pTW_FIX32 = ^TW_FIX32;
  pTW_ENUMERATION = ^TW_ENUMERATION;
  pTW_ARRAY = ^TW_ARRAY;
  pTW_RANGE = ^TW_RANGE;
  pTW_UINT16 = ^TW_UINT16;
  pTW_INT16 = ^TW_INT16;
  pTW_IDENTITY = ^TW_IDENTITY;
  pTW_INT32 = ^TW_INT32;

  TW_RANGE = packed record
    ItemType: TW_UINT16;
    MinValue: TW_UINT32;
    MaxValue: TW_UINT32;
    StepSize: TW_UINT32;
    DefaultValue: TW_UINT32;
    CurrentValue: TW_UINT32;
  end;

  TW_ARRAY = packed record
    ItemType: TW_UINT16;
    NumItems: TW_UINT32;
    ItemList: array[0..1] of TW_UINT8;
  end;

  TW_ENUMERATION = packed record
    ItemType: TW_UINT16;
    NumItems: TW_UINT32;
    CurrentIndex: TW_UINT32;
    DefaultIndex: TW_UINT32;
    ItemList: array[0..1] of TW_UINT8;
  end;

  TW_FIX32 = packed record
    Whole: TW_INT16;
    Frac: TW_UINT16;
  end;

  TW_FRAME = packed record
    Left: TW_FIX32;
    Top: TW_FIX32;
    Right: TW_FIX32;
    Bottom: TW_FIX32;
  end;

  TW_IMAGELAYOUT = packed record
    Frame: TW_FRAME;
    DocumentNumber: TW_UINT32;
    PageNumber: TW_UINT32;
    FrameNumber: TW_UINT32;
  end;

  TW_EVENT = packed record
    pEvent: TW_MEMREF;
    TWMessage: TW_UINT16;
  end;

  TW_SETUPMEMXFER = packed record
    MinBufSize: TW_UINT32;
    MaxBufSize: TW_UINT32;
    Preferred: TW_UINT32;
  end;

  TW_SETUPFILEXFER = packed record
    FileName: TW_STR255;
    Format: TW_UINT16;
    VRefNum: TW_INT16;
  end;

  TW_MEMORY = packed record
    Flags: TW_UINT32;
    Length: TW_UINT32;
    TheMem: TW_MEMREF;
  end;

  TW_IMAGEMEMXFER = packed record
    Compression: TW_UINT16;
    BytesPerRow: TW_UINT32;
    Columns: TW_UINT32;
    Rows: TW_UINT32;
    XOffset: TW_UINT32;
    YOffset: TW_UINT32;
    BytesWritten: TW_UINT32;
    Memory: TW_MEMORY;
  end;

  TW_IMAGEINFO = packed record
    XResolution: TW_FIX32;
    YResolution: TW_FIX32;
    ImageWidth: TW_INT32;
    ImageLength: TW_INT32;
    SamplesPerPixel: TW_INT16;
    BitsPerSample: array[0..7] of TW_INT16;
    BitsPerPixel: TW_INT16;
    Planar: TW_BOOL;
    PixelType: TW_INT16;
    Compression: TW_UINT16;
  end;

  TW_ONEVALUE = packed record
    ItemType: TW_UINT16;
    Item: TW_UINT32;
  end;

  TW_CAPABILITY = packed record
    Cap: TW_UINT16;
    ConType: TW_UINT16;
    hContainer: TW_HANDLE;
  end;

  TW_STATUS = packed record
    ConditionCode: TW_UINT16;
    Reserved: TW_UINT16;
  end;

  TW_PENDINGXFERS = packed record
    Count: TW_UINT16;
    case boolean of
      False: (EOJ: TW_UINT32);
      True: (Reserved: TW_UINT32);
  end;

  TW_USERINTERFACE = packed record
    ShowUI:  TW_BOOL;
    ModalUI: TW_BOOL;
    hParent: TW_HANDLE;
  end;

  TW_VERSION = packed record
    MajorNum: TW_UINT16;
    MinorNum: TW_UINT16;
    Language: TW_UINT16;
    Country: TW_UINT16;
    Info: TW_STR32;
  end;

  TW_IDENTITY = packed record
    Id: TW_UINT32;
    Version: TW_VERSION;
    ProtocolMajor: TW_UINT16;
    ProtocolMinor: TW_UINT16;
    SupportedGroups: TW_UINT32;
    Manufacturer: TW_STR32;
    ProductFamily: TW_STR32;
    ProductName: TW_STR32;
  end;

  TW_CUSTOMDSDATA = packed record
    InfoLength: TW_UINT32;
    hData: TW_HANDLE;
  end;


DSMENTRYPROC = function(pOrigin: pTW_IDENTITY; pDest: pTW_IDENTITY; DG: TW_UINT32; DAT: TW_UINT16; MSG: TW_UINT16; pData: TW_MEMREF): TW_UINT16; stdcall;

TDSMEntryProc = DSMENTRYPROC;

const

  TWON_PROTOCOLMAJOR = 1;
  TWON_PROTOCOLMINOR = 9;
  DG_IMAGE = $0002;
  DG_CONTROL = $0001;


  TWCC_SUCCESS = 0;
  TWCC_BUMMER = 1;
  TWCC_LOWMEMORY = 2;
  TWCC_NODS = 3;
  TWCC_MAXCONNECTIONS = 4;
  TWCC_OPERATIONERROR = 5;
  TWCC_BADCAP = 6;
  TWCC_BADPROTOCOL = 9;
  TWCC_BADVALUE = 10;
  TWCC_SEQERROR = 11;
  TWCC_BADDEST = 12;
  TWCC_CAPUNSUPPORTED = 13;
  TWCC_CAPBADOPERATION = 14;
  TWCC_CAPSEQERROR = 15;
  TWCC_DENIED = 16;
  TWCC_FILEEXISTS = 17;
  TWCC_FILENOTFOUND = 18;
  TWCC_NOTEMPTY = 19;
  TWCC_PAPERJAM = 20;
  TWCC_PAPERDOUBLEFEED = 21;
  TWCC_FILEWRITEERROR = 22;
  TWCC_CHECKDEVICEONLINE = 23;

  TWRC_SUCCESS = 0;
  TWRC_FAILURE = 1;

  DAT_CUSTOMDSDATA = $000c;
  DAT_PARENT = $0004;
  MSG_OPENDSM = $0301;
  MSG_CLOSEDSM = $0302;
  DAT_USERINTERFACE = $0009;
  MSG_DISABLEDS = $0501;
  DAT_IDENTITY = $0003;
  MSG_CLOSEDS = $0402;
  MSG_USERSELECT = $0403;
  DAT_STATUS = $0008;
  MSG_GET = $0001;
  MSG_GETFIRST = $0004;
  TWRC_ENDOFLIST = 7;
  MSG_GETNEXT = $0005;
  MSG_OPENDS = $0401;
  MSG_ENABLEDS = $0502;
  MSG_ENABLEDSUIONLY = $0503;
  TWON_DONTCARE16 = $FFFF;
  DAT_CAPABILITY = $0001;
  TWON_ONEVALUE = 5;
  TWTY_STR255 = $000C;
  MSG_SET = $0006;
  DAT_PENDINGXFERS = $0005;
  MSG_ENDXFER = $0701;
  MSG_RESET = $0007;
  TWTY_BOOL = $0006;
  DAT_IMAGEINFO = $0101;
  DAT_SETUPMEMXFER = $0006;
  TWON_DONTCARE32 = DWORD($FFFFFFFF);
  TWMF_APPOWNS = $1;
  TWMF_HANDLE = $10;
  DAT_IMAGEMEMXFER = $0103;
  TWRC_XFERDONE = 6;
  CAP_CAPTION = $1001;
  TWRC_CANCEL = 3;
  DAT_IMAGENATIVEXFER = $0104;
  DAT_SETUPFILEXFER = $0007;
  TWFF_BMP = 2;
  DAT_IMAGEFILEXFER = $0105;
  MSG_NULL = $0000;
  DAT_EVENT = $0002;
  MSG_PROCESSEVENT = $0601;
  TWRC_DSEVENT = 4;
  TWRC_NOTDSEVENT = 5;
  MSG_XFERREADY = $0101;
  MSG_CLOSEDSREQ = $0102;
  MSG_CLOSEDSOK = $0103;
  DAT_IMAGELAYOUT = $0102;
  TWON_ENUMERATION = 4;
  ICAP_XRESOLUTION = $1118;
  ICAP_YRESOLUTION = $1119;
  TWON_ARRAY = 3;
  TWON_RANGE = 6;
  TWTY_INT16  = $0001;
  TWTY_UINT16 = $0004;
  TWTY_INT32 = $0002;
  TWTY_FIX32 = $0007;
  ICAP_UNITS = $0102;
  TWPF_CHOCOLATE = 0;
  ICAP_PIXELFLAVOR = $111F;
  TWPF_VANILLA = 1;
  ICAP_UNDEFINEDIMAGESIZE = $112D;
  ICAP_CONTRAST = $1103;
  ICAP_BRIGHTNESS = $1101;
  ICAP_THRESHOLD = $1123;
  ICAP_ROTATION = $1121;
  ICAP_XSCALING = $1124;
  ICAP_YSCALING = $1125;
  ICAP_PIXELTYPE = $0101;
  ICAP_BITDEPTH = $112B;
  ICAP_PLANARCHUNKY = $1120;
  TWPC_CHUNKY = 0;
  ICAP_XFERMECH = $0103;
  TWSX_MEMORY = 2;
  CAP_FEEDERENABLED = $1002;
  CAP_AUTOFEED = $1007;
  ICAP_AUTOMATICDESKEW = $1151;
  ICAP_AUTOMATICBORDERDETECTION = $1150;
  ICAP_AUTOBRIGHT = $1100;
  ICAP_AUTOMATICROTATE = $1152;
  ICAP_ORIENTATION = $1110;
  ICAP_SUPPORTEDSIZES = $1122;
  CAP_INDICATORS = $100B;
  CAP_DUPLEXENABLED = $1013;
  ICAP_GAMMA = $1108;
  ICAP_PHYSICALHEIGHT = $1112;
  ICAP_PHYSICALWIDTH = $1111;
  CAP_FEEDERLOADED = $1003;
  CAP_PAPERDETECTABLE = $100D;
  CAP_DUPLEX = $1012;
  TWOR_ROT0 = 0;
  TWOR_PORTRAIT = TWOR_ROT0;
  MSG_GETDEFAULT = $0003;
  TWMF_POINTER = $8;
  ICAP_FILTER = $1106;
  ICAP_HIGHLIGHT = $110A;
  ICAP_SHADOW = $1113;
  TWCP_NONE = 0;
  ICAP_COMPRESSION = $0100;
  ICAP_AUTODISCARDBLANKPAGES = $1134;
  CAP_AUTOSCAN = $1010;
  CAP_AUTOMATICCAPTURE = $101a;
  CAP_XFERCOUNT = $0001;
  CAP_DEVICEONLINE = $100f;
  

  TWLG_DAN      =   0; { Danish }
  TWLG_DUT      =   1; { Dutch }
  TWLG_ENG      =   2; { International English }
  TWLG_FCF      =   3; { French Canadian }
  TWLG_FIN      =   4; { Finnish }
  TWLG_FRN      =   5; { French }
  TWLG_GER      =   6; { German }
  TWLG_ICE      =   7; { Icelandic }
  TWLG_ITN      =   8; { Italian }
  TWLG_NOR      =   9; { Norwegian }
  TWLG_POR      =  10; { Portuguese }
  TWLG_SPA      =  11; { Spanish }
  TWLG_SWE      =  12; { Swedish }
  TWLG_USA      =  13; { U.S. English }
{ Added for 1.8 }
  TWLG_USERLOCALE         =  $FFFF;
  TWLG_AFRIKAANS          =  14;
  TWLG_ALBANIA            =  15;
  TWLG_ARABIC             =  16;
  TWLG_ARABIC_ALGERIA     =  17;
  TWLG_ARABIC_BAHRAIN     =  18;
  TWLG_ARABIC_EGYPT       =  19;
  TWLG_ARABIC_IRAQ        =  20;
  TWLG_ARABIC_JORDAN      =  21;
  TWLG_ARABIC_KUWAIT      =  22;
  TWLG_ARABIC_LEBANON     =  23;
  TWLG_ARABIC_LIBYA       =  24;
  TWLG_ARABIC_MOROCCO     =  25;
  TWLG_ARABIC_OMAN        =  26;
  TWLG_ARABIC_QATAR       =  27;
  TWLG_ARABIC_SAUDIARABIA =  28;
  TWLG_ARABIC_SYRIA       =  29;
  TWLG_ARABIC_TUNISIA     =  30;
  TWLG_ARABIC_UAE         =  31; { United Arabic Emirates }
  TWLG_ARABIC_YEMEN       =  32;
  TWLG_BASQUE             =  33;
  TWLG_BYELORUSSIAN       =  34;
  TWLG_BULGARIAN          =  35;
  TWLG_CATALAN            =  36;
  TWLG_CHINESE            =  37;
  TWLG_CHINESE_HONGKONG   =  38;
  TWLG_CHINESE_PRC        =  39; { People's Republic of China }
  TWLG_CHINESE_SINGAPORE  =  40;
  TWLG_CHINESE_SIMPLIFIED =  41;
  TWLG_CHINESE_TAIWAN     =  42;
  TWLG_CHINESE_TRADITIONAL=  43;
  TWLG_CROATIA            =  44;
  TWLG_CZECH              =  45;
  TWLG_DANISH             =  TWLG_DAN;
  TWLG_DUTCH              =  TWLG_DUT;
  TWLG_DUTCH_BELGIAN      =  46;
  TWLG_ENGLISH            =  TWLG_ENG;
  TWLG_ENGLISH_AUSTRALIAN =  47;
  TWLG_ENGLISH_CANADIAN   =  48;
  TWLG_ENGLISH_IRELAND    =   49;
  TWLG_ENGLISH_NEWZEALAND =  50;
  TWLG_ENGLISH_SOUTHAFRICA=  51;
  TWLG_ENGLISH_UK           =  52;
  TWLG_ENGLISH_USA          =  TWLG_USA;
  TWLG_ESTONIAN             =  53;
  TWLG_FAEROESE             =  54;
  TWLG_FARSI                =  55;
  TWLG_FINNISH              =  TWLG_FIN;
  TWLG_FRENCH               =  TWLG_FRN;
  TWLG_FRENCH_BELGIAN       =  56;
  TWLG_FRENCH_CANADIAN      =  TWLG_FCF;
  TWLG_FRENCH_LUXEMBOURG    =  57;
  TWLG_FRENCH_SWISS         =  58;
  TWLG_GERMAN               =  TWLG_GER;
  TWLG_GERMAN_AUSTRIAN      =  59;
  TWLG_GERMAN_LUXEMBOURG    =  60;
  TWLG_GERMAN_LIECHTENSTEIN =  61;
  TWLG_GERMAN_SWISS         =  62;
  TWLG_GREEK                =  63;
  TWLG_HEBREW               =  64;
  TWLG_HUNGARIAN            =  65;
  TWLG_ICELANDIC            =  TWLG_ICE;
  TWLG_INDONESIAN           =  66;
  TWLG_ITALIAN              =  TWLG_ITN;
  TWLG_ITALIAN_SWISS        =  67;
  TWLG_JAPANESE             =  68;
  TWLG_KOREAN               =  69;
  TWLG_KOREAN_JOHAB         =  70;
  TWLG_LATVIAN              =  71;
  TWLG_LITHUANIAN           =  72;
  TWLG_NORWEGIAN            =  TWLG_NOR;
  TWLG_NORWEGIAN_BOKMAL     =  73;
  TWLG_NORWEGIAN_NYNORSK    =  74;
  TWLG_POLISH               =  75;
  TWLG_PORTUGUESE           =  TWLG_POR;
  TWLG_PORTUGUESE_BRAZIL    =  76;
  TWLG_ROMANIAN             =  77;
  TWLG_RUSSIAN              =  78;
  TWLG_SERBIAN_LATIN        =  79;
  TWLG_SLOVAK               =  80;
  TWLG_SLOVENIAN            =  81;
  TWLG_SPANISH              =  TWLG_SPA;
  TWLG_SPANISH_MEXICAN      =  82;
  TWLG_SPANISH_MODERN       =  83;
  TWLG_SWEDISH              =  TWLG_SWE;
  TWLG_THAI                 =  84;
  TWLG_TURKISH              =  85;
  TWLG_UKRANIAN             =  86;
  TWLG_ASSAMESE             =   87;
  TWLG_BENGALI              =   88;
  TWLG_BIHARI               =   89;
  TWLG_BODO                 =   90;
  TWLG_DOGRI                =   91;
  TWLG_GUJARATI             =   92;
  TWLG_HARYANVI             =   93;
  TWLG_HINDI                =   94;
  TWLG_KANNADA              =   95;
  TWLG_KASHMIRI             =   96;
  TWLG_MALAYALAM            =   97;
  TWLG_MARATHI              =   98;
  TWLG_MARWARI              =   99;
  TWLG_MEGHALAYAN           =   100;
  TWLG_MIZO                 =   101;
  TWLG_NAGA                 =   102;
  TWLG_ORISSI               =   103;
  TWLG_PUNJABI              =   104;
  TWLG_PUSHTU               =   105;
  TWLG_SERBIAN_CYRILLIC     =   106;
  TWLG_SIKKIMI              =   107;
  TWLG_SWEDISH_FINLAND      =   108;
  TWLG_TAMIL                =   109;
  TWLG_TELUGU               =   110;
  TWLG_TRIPURI              =   111;
  TWLG_URDU                 =   112;
  TWLG_VIETNAMESE           =   113;


  TWCY_AFGHANISTAN    =  1001;
  TWCY_ALGERIA        =  213;
  TWCY_AMERICANSAMOA  =  684;
  TWCY_ANDORRA        =  033;
  TWCY_ANGOLA         =  1002;
  TWCY_ANGUILLA       =  8090;
  TWCY_ANTIGUA        =  8091;
  TWCY_ARGENTINA      =   54;
  TWCY_ARUBA          =  297;
  TWCY_ASCENSIONI     =  247;
  TWCY_AUSTRALIA      =   61;
  TWCY_AUSTRIA        =   43;
  TWCY_BAHAMAS        =  8092;
  TWCY_BAHRAIN        =  973;
  TWCY_BANGLADESH     =  880;
  TWCY_BARBADOS       =  8093;
  TWCY_BELGIUM        =   32;
  TWCY_BELIZE         =  501;
  TWCY_BENIN          =  229;
  TWCY_BERMUDA        =  8094;
  TWCY_BHUTAN         =  1003;
  TWCY_BOLIVIA        =  591;
  TWCY_BOTSWANA       =  267;
  TWCY_BRITAIN        =   6;
  TWCY_BRITVIRGINIS   =  8095;
  TWCY_BRAZIL         =   55;
  TWCY_BRUNEI         =  673;
  TWCY_BULGARIA       =  359;
  TWCY_BURKINAFASO    =  1004;
  TWCY_BURMA          =  1005;
  TWCY_BURUNDI        =  1006;
  TWCY_CAMAROON       =  237;
  TWCY_CANADA         =   2;
  TWCY_CAPEVERDEIS    =  238;
  TWCY_CAYMANIS       =  8096;
  TWCY_CENTRALAFREP   =  1007;
  TWCY_CHAD           =  1008;
  TWCY_CHILE          =   56;
  TWCY_CHINA          =   86;
  TWCY_CHRISTMASIS    =  1009;
  TWCY_COCOSIS        =  1009;
  TWCY_COLOMBIA       =   57;
  TWCY_COMOROS        =  1010;
  TWCY_CONGO          =  1011;
  TWCY_COOKIS         =  1012;
  TWCY_COSTARICA      =  506 ;
  TWCY_CUBA           =  005;
  TWCY_CYPRUS         =  357;
  TWCY_CZECHOSLOVAKIA =  42;
  TWCY_DENMARK        =   45;
  TWCY_DJIBOUTI       =  1013;
  TWCY_DOMINICA       =  8097;
  TWCY_DOMINCANREP    =  8098;
  TWCY_EASTERIS       =  1014;
  TWCY_ECUADOR        =  593;
  TWCY_EGYPT          =   20;
  TWCY_ELSALVADOR     =  503;
  TWCY_EQGUINEA       =  1015;
  TWCY_ETHIOPIA       =  251;
  TWCY_FALKLANDIS     =  1016;
  TWCY_FAEROEIS       =  298;
  TWCY_FIJIISLANDS    =  679;
  TWCY_FINLAND        =  358;
  TWCY_FRANCE         =   33;
  TWCY_FRANTILLES     =  596;
  TWCY_FRGUIANA       =  594;
  TWCY_FRPOLYNEISA    =  689;
  TWCY_FUTANAIS       =  1043;
  TWCY_GABON          =  241;
  TWCY_GAMBIA         =  220;
  TWCY_GERMANY        =   49;
  TWCY_GHANA          =  233;
  TWCY_GIBRALTER      =  350;
  TWCY_GREECE         =   30;
  TWCY_GREENLAND      =  299;
  TWCY_GRENADA        =  8099;
  TWCY_GRENEDINES     =  8015;
  TWCY_GUADELOUPE     =  590;
  TWCY_GUAM           =  671;
  TWCY_GUANTANAMOBAY  =  5399;
  TWCY_GUATEMALA      =  502;
  TWCY_GUINEA         =  224;
  TWCY_GUINEABISSAU   =  1017;
  TWCY_GUYANA         =  592;
  TWCY_HAITI          =  509;
  TWCY_HONDURAS       =  504;
  TWCY_HONGKONG       =  852 ;
  TWCY_HUNGARY        =   36;
  TWCY_ICELAND        =  354;
  TWCY_INDIA          =   91;
  TWCY_INDONESIA      =   62;
  TWCY_IRAN           =   98;
  TWCY_IRAQ           =  964;
  TWCY_IRELAND        =  353;
  TWCY_ISRAEL         =  972;
  TWCY_ITALY          =   39;
  TWCY_IVORYCOAST     =  225 ;
  TWCY_JAMAICA        =  8010;
  TWCY_JAPAN          =   81;
  TWCY_JORDAN         =  962;
  TWCY_KENYA          =  254;
  TWCY_KIRIBATI       =  1018;
  TWCY_KOREA          =   82;
  TWCY_KUWAIT         =  965;
  TWCY_LAOS           =  1019;
  TWCY_LEBANON        =  1020;
  TWCY_LIBERIA        =  231;
  TWCY_LIBYA          =  218;
  TWCY_LIECHTENSTEIN  =   41;
  TWCY_LUXENBOURG     =  352;
  TWCY_MACAO          =  853;
  TWCY_MADAGASCAR     =  1021;
  TWCY_MALAWI         =  265;
  TWCY_MALAYSIA       =   60;
  TWCY_MALDIVES       =  960;
  TWCY_MALI           =  1022;
  TWCY_MALTA          =  356;
  TWCY_MARSHALLIS     =  692;
  TWCY_MAURITANIA     =  1023;
  TWCY_MAURITIUS      =  230;
  TWCY_MEXICO         =   3;
  TWCY_MICRONESIA     =  691;
  TWCY_MIQUELON       =  508;
  TWCY_MONACO         =   33;
  TWCY_MONGOLIA       =  1024;
  TWCY_MONTSERRAT     =  8011;
  TWCY_MOROCCO        =  212;
  TWCY_MOZAMBIQUE     =  1025;
  TWCY_NAMIBIA        =  264;
  TWCY_NAURU          =  1026;
  TWCY_NEPAL          =  977;
  TWCY_NETHERLANDS    =   31;
  TWCY_NETHANTILLES   =  599;
  TWCY_NEVIS          =  8012;
  TWCY_NEWCALEDONIA   =  687;
  TWCY_NEWZEALAND     =   64;
  TWCY_NICARAGUA      =  505;
  TWCY_NIGER          =  227;
  TWCY_NIGERIA        =  234;
  TWCY_NIUE           =  1027;
  TWCY_NORFOLKI       =  1028;
  TWCY_NORWAY         =   47;
  TWCY_OMAN           =  968;
  TWCY_PAKISTAN       =   92;
  TWCY_PALAU          =  1029;
  TWCY_PANAMA         =  507;
  TWCY_PARAGUAY       =  595;
  TWCY_PERU           =   51;
  TWCY_PHILLIPPINES   =   63;
  TWCY_PITCAIRNIS     =  1030;
  TWCY_PNEWGUINEA     =  675;
  TWCY_POLAND         =   48;
  TWCY_PORTUGAL       =  351;
  TWCY_QATAR          =  974;
  TWCY_REUNIONI       =  1031;
  TWCY_ROMANIA        =   40;
  TWCY_RWANDA         =  250;
  TWCY_SAIPAN         =  670;
  TWCY_SANMARINO      =   39;
  TWCY_SAOTOME        =  1033;
  TWCY_SAUDIARABIA    =  966;
  TWCY_SENEGAL        =  221;
  TWCY_SEYCHELLESIS   =  1034;
  TWCY_SIERRALEONE    =  1035;
  TWCY_SINGAPORE      =   65;
  TWCY_SOLOMONIS      =  1036;
  TWCY_SOMALI         =  1037;
  TWCY_SOUTHAFRICA    =  27 ;
  TWCY_SPAIN          =   34;
  TWCY_SRILANKA       =   94;
  TWCY_STHELENA       =  1032;
  TWCY_STKITTS        =  8013;
  TWCY_STLUCIA        =  8014;
  TWCY_STPIERRE       =  508;
  TWCY_STVINCENT      =  8015;
  TWCY_SUDAN          =  1038;
  TWCY_SURINAME       =  597;
  TWCY_SWAZILAND      =  268;
  TWCY_SWEDEN         =   46;
  TWCY_SWITZERLAND    =   41;
  TWCY_SYRIA          =  1039;
  TWCY_TAIWAN         =  886;
  TWCY_TANZANIA       =  255;
  TWCY_THAILAND       =   66;
  TWCY_TOBAGO         =  8016;
  TWCY_TOGO           =  228;
  TWCY_TONGAIS        =  676;
  TWCY_TRINIDAD       =  8016;
  TWCY_TUNISIA        =  216;
  TWCY_TURKEY         =   90;
  TWCY_TURKSCAICOS    =  8017;
  TWCY_TUVALU         =  1040;
  TWCY_UGANDA         =  256;
  TWCY_USSR           =   7;
  TWCY_UAEMIRATES     =  971;
  TWCY_UNITEDKINGDOM  =   44;
  TWCY_USA            =   1;
  TWCY_URUGUAY        =  598;
  TWCY_VANUATU        =  1041;
  TWCY_VATICANCITY    =   39;
  TWCY_VENEZUELA      =   58;
  TWCY_WAKE           =  1042;
  TWCY_WALLISIS       =  1043;
  TWCY_WESTERNSAHARA  =  1044;
  TWCY_WESTERNSAMOA   =  1045;
  TWCY_YEMEN          =  1046;
  TWCY_YUGOSLAVIA     =   38;
  TWCY_ZAIRE          =  243;
  TWCY_ZAMBIA         =  260;
  TWCY_ZIMBABWE       =  263;
{ Added for 1.8 }
  TWCY_ALBANIA        =  355;
  TWCY_ARMENIA        =  374;
  TWCY_AZERBAIJAN     =  994;
  TWCY_BELARUS        =  375;
  TWCY_BOSNIAHERZGO   =  387;
  TWCY_CAMBODIA       =  855;
  TWCY_CROATIA        =  385;
  TWCY_CZECHREPUBLIC  =  420;
  TWCY_DIEGOGARCIA    =  246;
  TWCY_ERITREA        =  291;
  TWCY_ESTONIA        =  372;
  TWCY_GEORGIA        =  995;
  TWCY_LATVIA         =  371;
  TWCY_LESOTHO        =  266;
  TWCY_LITHUANIA      =  370;
  TWCY_MACEDONIA      =  389;
  TWCY_MAYOTTEIS      =  269;
  TWCY_MOLDOVA        =  373;
  TWCY_MYANMAR        =  95 ;
  TWCY_NORTHKOREA     =  850;
  TWCY_PUERTORICO     =  787;
  TWCY_RUSSIA         =  7 ;
  TWCY_SERBIA         =  381;
  TWCY_SLOVAKIA       =  421;
  TWCY_SLOVENIA       =  386;
  TWCY_SOUTHKOREA     =  82 ;
  TWCY_UKRAINE        =  380;
  TWCY_USVIRGINIS     =  340;
  TWCY_VIETNAM        =  84 ;



type

{!!
<FS>TIEDRect

<FM>Declaration<FC>
}
  TIEDRect = record
    Left: double;
    Top: double;
    Right: double;
    Bottom: double;
  end;
{!!}


{!!
<FS>TIETWFilter

<FM>Declaration<FC>
TIETWFilter = (ietwUndefined, ietwRed, ietwGreen, ietwBlue, ietwNone, ietwWhite, ietwCyan, ietwMagenta, ietwYellow, ietwBlack);

<FM>Note<FN>
ietwUndefined unset this property, while ietwNone set "none" value.
!!}
  //
  TIETWFilter = (ietwUndefined, ietwRed, ietwGreen, ietwBlue, ietwNone, ietwWhite, ietwCyan, ietwMagenta, ietwYellow, ietwBlack);

  // capabilities of the Twain source
  TIETWSourceCaps = record
    fXResolution: TIEDoubleList;    // X Resolution
    fYResolution: TIEDoubleList;    // Y Resolution
    fXScaling: TIEDoubleList;       // X Scaling
    fYScaling: TIEDoubleList;       // Y Scaling
    fPixelType: TIEIntegerList;     // Pixel type [ 0 : Black & white (1 bit)
                                    //              1 : Gray scale (8 bit)
                                    //              2 : full RGB (24 bit or 48 bit)
                                    //              3 : palette (ImageEn can't support this)
                                    //              4 : CMY (ImageEn can't support this)
                                    //              5 : CMYK (ImageEn can't support this)
                                    //              6 : YUV (ImageEn can't support this)
                                    //              7 : YUVK (ImageEn can't support this)
                                    //              8 : CIEXYZ (ImageEn can't support this) ]
    fBitDepth: TIEIntegerList;      // Bit Depth [ 1...]
    fGamma: double;                 // Gamma
    fPhysicalHeight: double;        // Physical Height
    fPhysicalWidth: double;         // Physical Width
    fFeederEnabled: boolean;        // FeederEnabled
    fOrientation: TIEIntegerList;   // Orientation
    fIndicators: boolean;           // Progress Indicators
    fAcquireFrame: TIEDRect;        // image layout (this isn't a capability...)
    fBufferedTransfer: boolean;     // image transfer method (default is buffered(true))
    fUseMemoryHandle: boolean;       // use Memory Handle instead of memory pointer to transfer images
    fFileTransfer: boolean;         // image transfer by File (if true overlap fBufferedTransfer), default false
    fDuplexEnabled: boolean;        // Duplex
    fAcquireFrameEnabled: boolean;  // make active fAcquireFrame
    fFeederLoaded: boolean;         // Feeder loaded
    fDuplexSupported: boolean;      // Duplex supported
    fPaperDetectable: boolean;      // Paper detectable
    fContrast: TIEDoubleList;       // Contrast
    fBrightness: TIEDoubleList;     // Brightness
    fThreshold: TIEDoubleList;      // Threshold
    fRotation: TIEDoubleList;       // Rotation
    fUndefinedImageSize: boolean;   // undefined image size
    fStandardSize: TIEIntegerList;  // SupportedSizes
    fAutoFeed: boolean;             // enable/disable Autofeed
    fAutoDeskew: boolean;            // enable/disable ICAP_AUTOMATICDESKEW
    fAutoBorderDetection: boolean;   // enable/disable ICAP_AUTOMATICBORDERDETECTION
    fAutoBright: boolean;            // enable/disable ICAP_AUTOBRIGHT
    fAutoRotate: boolean;            // enable/disable ICAP_AUTOMATICROTATE
    fAutoDiscardBlankPages: integer; // enable/disable/set ICAP_AUTODISCARDBLANKPAGES
    fFilter: TIETWFilter;            // set filter
    fHighlight: double;              // ICAP_HIGHLIGHT (-1 = default)
    fShadow: double;                 // ICAP_SHADOW (-1 = default)
    fAcceptedImages: integer;        // CAP_XFERCOUNT (-2 = default)
    fAutoScan: boolean;              // CAP_AUTOSCAN
    fDeviceOnline: boolean;          // CAP_DEVICEONLINE (readonly, default true)
  end;

  // structure for parameters passed to imscan function
  TIETwainShared = record
    hDSMLib: THANDLE;
    DSM_Entry: TDSMEntryProc;
    hproxy: HWND;
  end;
  PIETwainShared = ^TIETwainShared;

{!!
<FS>TIETwainParams

<FM>Description<FN>
The TIETwainParams object contains many properties and methods to control Twain scanners without using the default dialog.

<FM>Twain Communication Methods<FN>
<TABLE2>
<R> <C_IMG_METHOD> <C><A TIETwainParams.Assign></C> </R>
<R> <C_IMG_METHOD> <C><A TIETwainParams.Create></C> </R>
<R> <C_IMG_METHOD> <C><A TIETwainParams.FreeResources></C> </R>
<R> <C_IMG_METHOD> <C><A TIETwainParams.GetDefaultSource></C> </R>
<R> <C_IMG_METHOD> <C><A TIETwainParams.GetFromScanner></C> </R>
<R> <C_IMG_METHOD> <C><A TIETwainParams.SelectSourceByName></C> </R>
<R> <C_IMG_METHOD> <C><A TIETwainParams.SetDefaultParams></C> </R>
<R> <C_IMG_METHOD> <C><A TIETwainParams.Update></C> </R>
</TABLE>

<FM>Twain Properties<FN>
<TABLE2>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AcceptedImages></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AcquireFrameBottom></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AcquireFrameEnabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AcquireFrameLeft></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AcquireFrameRight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AcquireFrameTop></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AppManufacturer></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AppProductFamily></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AppProductName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AppVersionInfo></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AutoBorderDetection></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AutoBright></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AutoDeskew></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AutoDiscardBlankPages></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AutoFeed></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AutoRotate></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.AutoScan></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.BitDepth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Brightness></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.BufferedTransfer></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.CapabilitiesValid></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.CompatibilityMode></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Contrast></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Country></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.DeviceOnline></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.DuplexEnabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.DuplexSupported></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.FeederEnabled></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.FeederLoaded></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.FileTransfer></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Filter></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Gamma></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Highlight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Language></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.LastErrorStr></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.LastError></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.LogFile></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Orientation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.PaperDetectable></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.PhysicalHeight></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.PhysicalWidth></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.PixelType></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.ProgressIndicators></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Rotation></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.SelectedSource></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Shadow></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.ShowSettingsOnly></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.SourceCount></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.SourceName></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.StandardSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.Threshold></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.UndefinedImageSize></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.UseMemoryHandle></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.VisibleDialog></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.XResolution></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.XScaling></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.YResolution></C> </R>
<R> <C_IMG_PROPERTY> <C><A TIETwainParams.YScaling></C> </R>
</TABLE>
!!}
  TIETwainParams = class
  private
    fOwner: TComponent;
    fVisibleDialog: boolean;
    fShowSettingsOnly: boolean;     // only settings will be visible (use MSG_ENABLEDSUIONLY instead of MSG_ENABLEDS)
    fSourceListData: TList;
    fSelectedSource: integer;       // index of fSourceListData
    fSourceListDataValid: boolean;  // false to get capabilities
    fCapabilitiesValid: boolean;    // false to get capabilities
    fCapabilities: TIETWSourceCaps; // filled by imscan
    fAppVersionInfo: AnsiString;
    fAppManufacturer: AnsiString;
    fAppProductFamily: AnsiString;
    fAppProductName: AnsiString;
    fCompatibilityMode: boolean;
    fLanguage: word;
    fCountry: word;
    fSourceSettings: TMemoryStream;
    procedure FillSourceListData;
    procedure FillCapabilities;
    function GetSourceName(idx: integer): AnsiString;
    function GetSourceCount: integer;
    function GetXResolution: TIEDoubleList;
    function GetYResolution: TIEDoubleList;
    function GetXScaling: TIEDoubleList;
    function GetYScaling: TIEDoubleList;
    function GetContrast: TIEDoubleList;
    function GetBrightness: TIEDoubleList;
    function GetThreshold: TIEDoubleList;
    function GetRotation: TIEDoubleList;
    function GetPixelType: TIEIntegerList;
    function GetBitDepth: TIEIntegerList;
    function GetGamma: double;
    function GetPhysicalHeight: double;
    function GetPhysicalWidth: double;
    function GetFeederEnabled: boolean;
    function GetAutoFeed: boolean;
    function GetAutoDeskew: boolean;
    function GetAutoBorderDetection: boolean;
    function GetAutoBright: boolean;
    function GetAutoRotate: boolean;
    function GetAutoDiscardBlankPages: integer;
    function GetFilter: TIETWFilter;
    function GetHighlight: double;
    function GetShadow: double;
    function GetAcceptedImages: integer;
    function GetAutoScan: boolean;
    function GetDeviceOnline: boolean;
    function GetUndefinedImageSize: boolean;
    function GetOrientation: TIEIntegerList;
    function GetStandardSize: TIEIntegerList;
    procedure SetSelectedSource(v: integer);
    function GetIndicators: boolean;
    procedure SetFeederEnabled(v: boolean);
    procedure SetAutoFeed(v: boolean);
    procedure SetAutoDeskew(v: boolean);
    procedure SetAutoBorderDetection(v: boolean);
    procedure SetAutoBright(v: boolean);
    procedure SetAutoRotate(v: boolean);
    procedure SetAutoDiscardBlankPages(v: integer);
    procedure SetFilter(v: TIETWFilter);
    procedure SetHighlight(v: double);
    procedure SetShadow(v: double);
    procedure SetAcceptedImages(v: integer);
    procedure SetAutoScan(v: boolean);
    procedure SetUndefinedImageSize(v: boolean);
    procedure SetIndicators(v: boolean);
    function GetAcquireFrame(idx: integer): double;
    procedure SetAcquireFrame(idx: integer; v: double);
    function GetBufferedTransfer: boolean;
    procedure SetBufferedTransfer(v: boolean);
    function GetUseMemoryHandle: boolean;
    procedure SetUseMemoryHandle(v: boolean);
    function GetFileTransfer: boolean;
    procedure SetFileTransfer(v: boolean);
    procedure SetAppVersionInfo(v: AnsiString);
    procedure SetAppManufacturer(v: AnsiString);
    procedure SetAppProductFamily(v: AnsiString);
    procedure SetAppProductName(v: AnsiString);
    procedure SetDuplexEnabled(v: boolean);
    function GetDuplexEnabled: boolean;
    procedure SetAcquireFrameEnabled(v: boolean);
    function GetAcquireFrameEnabled: boolean;
    function GetFeederLoaded: boolean;
    function GetDuplexSupported: boolean;
    function GetPaperDetectable: boolean;
    procedure SetLogFile(v: string);
    function GetLogFile: string;
  public
    // reserved
    TwainShared: TIETwainShared;
    // General properties

{!!
<FS>TIETwainParams.LastError

<FM>Declaration<FC>
LastError: integer;

<FM>Description<FN>
LastError returns the last error which occurred on scanning.

Allowed values (defined in ietwain unit) are:

  TWCC_SUCCESS           =  0; // It worked!
  TWCC_BUMMER            =  1; // Failure due to unknown causes
  TWCC_LOWMEMORY         =  2; // Not enough memory to perform operation
  TWCC_NODS              =  3; // No Data Source
  TWCC_MAXCONNECTIONS    =  4; // DS is connected to max possible applications
  TWCC_OPERATIONERROR    =  5; // DS or DSM reported error, application shouldn't
  TWCC_BADCAP            =  6; // Unknown capability
  TWCC_BADPROTOCOL       =  9; // Unrecognized MSG DG DAT combination
  TWCC_BADVALUE          =  10; // Data parameter out of range
  TWCC_SEQERROR          =  11; // DG DAT MSG out of expected sequence
  TWCC_BADDEST           =  12; // Unknown destination Application/Source in DSM_Entry
  TWCC_CAPUNSUPPORTED    =  13; // Capability not supported by source
  TWCC_CAPBADOPERATION   =  14; // Operation not supported by capability
  TWCC_CAPSEQERROR       =  15; // Capability has dependancy on other capability
  TWCC_DENIED            =  16; // File System operation is denied (file is protected)
  TWCC_FILEEXISTS        =  17; // Operation failed because file already exists.
  TWCC_FILENOTFOUND      =  18; // File not found
  TWCC_NOTEMPTY          =  19; // Operation failed because directory is not empty
  TWCC_PAPERJAM          =  20; // The feeder is jammed
  TWCC_PAPERDOUBLEFEED   =  21; // The feeder detected multiple pages
  TWCC_FILEWRITEERROR    =  22; // Error writing the file (meant for things like disk full conditions)
  TWCC_CHECKDEVICEONLINE =  23; // The device went offline prior to or during this operation
!!}
    LastError: integer;

{!!
<FS>TIETwainParams.LastErrorStr

<FM>Declaration<FC>
LastErrorStr: AnsiString;

<FM>Description<FN>
LastErrorStr returns the last error which occurred on scanning as a string (See also: LastError).
!!}
    LastErrorStr: AnsiString;

{!!
<FS>TIETwainParams.VisibleDialog

<FM>Declaration<FC>
property VisibleDialog: boolean;

<FM>Description<FN>
If VisibleDialog is True (default), the scanner user interface is enabled when Acquire method is called.

<FM>Example<FC>
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;
!!}
    property VisibleDialog: boolean read fVisibleDialog write fVisibleDialog;

{!!
<FS>TIETwainParams.ShowSettingsOnly

<FM>Declaration<FC>
property ShowSettingsOnly: boolean;

<FM>Description<FN>
If ShowSettingsOnly is True the scanner driver will show settings dialog, without acquire the image.
You could use <A TIETwainParams.SourceSettings> to read scanner settings.

Subsequent calls to <A TImageEnIO.Acquire> will return True if user presses OK (or Save), False otherwise.

<FM>Demo<FN>
ImageAcquisition\TwainStore

<FM>Example<FC>
// first session: show scanner dialog only to set scanner settings
ImageEnView1.IO.TwainParams.ShowSettingsOnly := true;
ImageEnView1.IO.Acquire;
ImageEnView1.IO.TwainParams.SourceSettings.SaveToFile('mysettings.dat');

// another session: use saved settings
ImageEnView1.IO.TwainParams.ShowSettingsOnly := false;
ImageEnView1.IO.TwainParams.VisibleDialog := false;
ImageEnView1.IO.TwainParams.SourceSettings.LoadFromFile('mysettings.dat');
ImageEnView1.IO.Acquire;
!!}
    property ShowSettingsOnly: boolean read fShowSettingsOnly write fShowSettingsOnly;

{!!
<FS>TIETwainParams.SourceSettings

<FM>Declaration<FC>
property SourceSettings: TMemoryStream;

<FM>Description<FN>
SourceSettings contains custom Twain source settings.
This memory stream is filled when <A TIETwainParams.ShowSettingsOnly> is true and you call <A TImageEnIO.Acquire>.
You can also load it from file to provide already saved settings.

<FM>Demo<FN>
ImageAcquisition\TwainStore

<FM>Example<FC>
// first session: show scanner dialog only to set scanner settings
ImageEnView1.IO.TwainParams.ShowSettingsOnly := true;
ImageEnView1.IO.Acquire;
ImageEnView1.IO.TwainParams.SourceSettings.SaveToFile('mysettings.dat');

// another session: use saved settings
ImageEnView1.IO.TwainParams.ShowSettingsOnly := false;
ImageEnView1.IO.TwainParams.VisibleDialog := false;
ImageEnView1.IO.TwainParams.SourceSettings.LoadFromFile('mysettings.dat');
ImageEnView1.IO.Acquire;
!!}
    property SourceSettings: TMemoryStream read fSourceSettings;

    property SourceName[idx: integer]: AnsiString read GetSourceName;
    property SourceCount: integer read GetSourceCount;
    property SelectedSource: integer read fSelectedSource write SetSelectedSource;
    property LogFile: string read GetLogFile write SetLogFile;

{!!
<FS>TIETwainParams.CompatibilityMode

<FM>Declaration<FC>
property CompatibilityMode: boolean;

<FM>Description<FN>
CompatibilityMode disables capability setting and reading (some scanner may crash when you set/get capabilities).
Set this property to true only if you have problems with a specific scanner.
!!}
    property CompatibilityMode: boolean read fCompatibilityMode write fCompatibilityMode;

{!!
<FS>TIETwainParams.CapabilitiesValid

<FM>Declaration<FC>
property CapabilitiesValid: boolean;

<FM>Description<FN>
If true all properties are updated from scanner, otherwise not and you should call <A TIETwainParams.GetFromScanner> to update them.
!!}
    property CapabilitiesValid: boolean read fCapabilitiesValid;

{!!
<FS>TIETwainParams.Language

<FM>Declaration<FC>
property Language: word;

<FM>Description<FN>
Specifies the primary language for the scanner dialogs. Default is TWLG_USERLOCALE.

Note: Versions before 3.0.3 have TWLG_USA for default.

See also: <A Twain language constants>.
!!}
    property Language: word read fLanguage write fLanguage;

{!!
<FS>TIETwainParams.Country

<FM>Declaration<FC>
property Country: word;

<FM>Description<FN>
Specifies the primary country where your application is intended to be distributed. Default is TWLG_USA.

See also: <A Twain country constants>.
!!}
    property Country: word read fCountry write fCountry;

    // Capabilities
    property XResolution: TIEDoubleList read GetXResolution;
    property YResolution: TIEDoubleList read GetYResolution;
    property XScaling: TIEDoubleList read GetXScaling;
    property YScaling: TIEDoubleList read GetYScaling;
    property PixelType: TIEIntegerList read GetPixelType;
    property BitDepth: TIEIntegerList read GetBitDepth;
    property Gamma: double read GetGamma; // readonly
    property PhysicalHeight: double read GetPhysicalHeight; // readonly
    property PhysicalWidth: double read GetPhysicalWidth; // readonly
    property FeederEnabled: boolean read GetFeederEnabled write SetFeederEnabled;
    property AutoFeed: boolean read GetAutoFeed write SetAutoFeed;
    property AutoDeskew: boolean read GetAutoDeskew write SetAutoDeskew;
    property AutoBorderDetection: boolean read GetAutoBorderDetection write SetAutoBorderDetection;
    property AutoBright: boolean read GetAutoBright write SetAutoBright;
    property AutoRotate: boolean read GetAutoRotate write SetAutoRotate;
    property AutoDiscardBlankPages: integer read GetAutoDiscardBlankPages write SetAutoDiscardBlankPages;
    property Filter: TIETWFilter read GetFilter write SetFilter;
    property Highlight: double read GetHighlight write SetHighlight;
    property Shadow: double read GetShadow write SetShadow;
    property AcceptedImages: integer read GetAcceptedImages write SetAcceptedImages;
    property AutoScan: boolean read GetAutoScan write SetAutoScan;
    property DeviceOnline: boolean read GetDeviceOnline;
    property FeederLoaded: boolean read GetFeederLoaded;
    property PaperDetectable: boolean read GetPaperDetectable;
    property Orientation: TIEIntegerList read GetOrientation;
    property ProgressIndicators: boolean read GetIndicators write SetIndicators;
    property AcquireFrameLeft: double index 0 read GetAcquireFrame write SetAcquireFrame;
    property AcquireFrameTop: double index 1 read GetAcquireFrame write SetAcquireFrame;
    property AcquireFrameRight: double index 2 read GetAcquireFrame write SetAcquireFrame;
    property AcquireFrameBottom: double index 3 read GetAcquireFrame write SetAcquireFrame;
    property BufferedTransfer: boolean read GetBufferedTransfer write SetBufferedTransfer;
    property UseMemoryHandle: boolean read GetUseMemoryHandle write SetUseMemoryHandle;
    property FileTransfer: boolean read GetFileTransfer write SetFileTransfer;
    property DuplexEnabled: boolean read GetDuplexEnabled write SetDuplexEnabled;
    property DuplexSupported: boolean read GetDuplexSupported;
    property AcquireFrameEnabled: boolean read GetAcquireFrameEnabled write SetAcquireFrameEnabled;
    property Contrast: TIEDoubleList read GetContrast;
    property Brightness: TIEDoubleList read GetBrightness;
    property Threshold: TIEDoubleList read GetThreshold;
    property Rotation: TIEDoubleList read GetRotation;
    property UndefinedImageSize: boolean read GetUndefinedImageSize write SetUndefinedImageSize;
    property StandardSize: TIEIntegerList read GetStandardSize;
    // Application identification
    property AppVersionInfo: AnsiString read fAppVersionInfo write SetAppVersionInfo;
    property AppManufacturer: AnsiString read fAppManufacturer write SetAppManufacturer;
    property AppProductFamily: AnsiString read fAppProductFamily write SetAppProductFamily;
    property AppProductName: AnsiString read fAppProductName write SetAppProductName;
    //
    constructor Create(Owner: TComponent);
    destructor Destroy; override;
    procedure SetDefaultParams;
    procedure Assign(Source: TIETwainParams);
    function SelectSourceByName(const sn: AnsiString): boolean;
    function GetDefaultSource: integer;
    procedure Update;
    procedure FreeResources;
    function GetFromScanner: boolean;
  end;

const
  // Constants for PixelType
  Twain_PixelType_BW        = 0;
  Twain_PixelType_Grayscale = 1;
  Twain_PixelType_FullRGB   = 2;
  Twain_PixelType_Palette   = 3;
  Twain_PixelType_CMY       = 4;
  Twain_PixelType_CMYK      = 5;
  Twain_PixelType_YUV       = 6;
  Twain_PixelType_YUVK      = 7;
  Twain_PixelType_CIEXYZ    = 8;
                
  // Constants for Orientation
  Twain_Orientation_NoRotate  = 0;
  Twain_Orientation_Rotate90  = 1;
  Twain_Orientation_Rotate180 = 2;
  Twain_Orientation_Rotate270 = 3;

  // Constants for AutoDiscardBlankPages
  Twain_AutoDiscard_Disable      = -2;
  Twain_AutoDiscard_DiscardBlank = -1;

  
var
  iegTwainLogName: string;
  iegTwainLogFile: textfile;


implementation


uses
  imscan, SysUtils, iemio, hyiedefs, imageenproc, ImageEnIO, iexAcquire;



{!!
<FS>TIETwainParams.Create

<FM>Declaration<FC>
constructor Create(Owner: TComponent);
!!}
constructor TIETwainParams.Create(Owner: TComponent);
begin
  inherited Create;
  fOwner := Owner;
  fSourceListDataValid := false;
  fSourceListData := TList.Create;
  fCapabilitiesValid := false;
  TwainShared.hDSMLib := 0;
  TwainShared.DSM_Entry := nil;
  TwainShared.hproxy := 0;
  fSourceSettings := TMemoryStream.Create;
  //
  with fCapabilities do
  begin
    fXResolution := TIEDoubleList.Create;
    fYResolution := TIEDoubleList.Create;
    fXScaling := TIEDoubleList.Create;
    fYScaling := TIEDoubleList.Create;
    fPixelType := TIEIntegerList.Create;
    fBitDepth := TIEIntegerList.Create;
    fOrientation := TIEIntegerList.Create;
    fContrast := TIEDoubleList.Create;
    fBrightness := TIEDoubleList.Create;
    fStandardSize := TIEIntegerList.Create;
    fThreshold := TIEDoubleList.Create;
    fRotation := TIEDoubleList.Create;
  end;
  //
  SetDefaultParams;
end;

destructor TIETwainParams.Destroy;
begin
  IETW_FreeResources(@TwainShared, IEFindHandle(fOwner));
  SetDefaultParams;
  FreeAndNil(fSourceListData);
  FreeAndNil(fSourceSettings);

  with fCapabilities do
  begin
    FreeAndNil(fXResolution);
    FreeAndNil(fYResolution);
    FreeAndNil(fXScaling);
    FreeAndNil(fYScaling);
    FreeAndNil(fPixelType);
    FreeAndNil(fBitDepth);
    FreeAndNil(fOrientation);
    FreeAndNil(fContrast);
    FreeAndNil(fBrightness);
    FreeAndNil(fStandardSize);
    FreeAndNil(fThreshold);
    FreeAndNil(fRotation);
  end;
  //
  inherited;
end;

{!!
<FS>TIETwainParams.FreeResources

<FM>Declaration<FC>
procedure FreeResources;

<FM>Description<FN>
ImageEn keeps the scanner driver open to improve scanning performance. 

Call FreeResources to close and free the scanner driver after a call to <A TImageEnIO.Acquire> method: this degrades performance but can improve stability.
Applications should not call FreeResources because it is executed when the application exits.

!!}
procedure TIETwainParams.FreeResources;
begin
  IETW_FreeResources(@TwainShared, IEFindHandle(fOwner));
  TwainShared.hDSMLib := 0;
  TwainShared.DSM_Entry := nil;
  TwainShared.hproxy := 0;
end;

{!!
<FS>TIETwainParams.GetDefaultSource

<FM>Declaration<FC>
function GetDefaultSource: integer;

<FM>Description<FN>
GetDefaultSource returns the system's default Twain source.
The default ImageEn source has the index 0, but this should be different from the system's default source.
Result is -1 if no Twain sources exist on the system

<FM>Example<FC>
// set system default source as current ImageEn Twain source
ImageEnView1.IO.TwainParams.SelectedSource := ImageEnIO.TwainParams.GetDefaultSource;

!!}
function TIETwainParams.GetDefaultSource: integer;
var
  q, ll: integer;
  sn: AnsiString;
begin
  result := 0;
  sn := IETW_GetDefaultSource(@TwainShared, IEFindHandle(fOwner));
  FillSourceListData;
  ll := length(sn);
  for q := 0 to fSourceListData.Count - 1 do
    if IEUpperCase(IECopy(pTW_IDENTITY(fSourceListData[q])^.ProductName, 1, ll)) = IEUpperCase(sn) then
    begin
      result := q;
      break;
    end;

  // Any sources found?
  if (Result = 0) and (fSourceListData.Count = 0) then
    Result := -1;
end;



{!!
<FS>TIETwainParams.SetDefaultParams

<FM>Declaration<FC>
procedure SetDefaultParams;

<FM>Description<FN>
Restores the Twain settings their default. You can also use this method to refresh the list of available scanners

<FM>Example<FC>
// Force detection of available devices
ImageEnView1.IO.TwainParams.SetDefaultParams();

// Fill ListBox1 with all scanner on the system
for q := 0 to ImageEnView1.IO.TwainParams.SourceCount - 1 do
  ListBox1.Items.Add( ImageEnView1.IO.TwainParams.SourceName[q] );
!!}
// set defaults. Also free fSourceListData items and fCapabilities allocated data
procedure TIETwainParams.SetDefaultParams;
var
  q: integer;
begin
  for q := 0 to fSourceListData.Count - 1 do
    Freemem(fSourceListData[q]);
  fSourceListData.Clear;
  fVisibleDialog := true;
  fShowSettingsOnly := false;
  fCompatibilityMode := false;
  fLanguage := TWLG_USERLOCALE;
  fCountry := TWCY_USA;
  fSelectedSource := 0;
  fSourceListDataValid := false;
  fCapabilitiesValid := false;
  fSourceSettings.Clear;
  // set fCapabilities items
  with fCapabilities do
  begin
    fXResolution.Clear;
    fYResolution.Clear;
    fXScaling.Clear;
    fYScaling.Clear;
    fPixelType.Clear;
    fBitDepth.Clear;
    fContrast.Clear;
    fBrightness.Clear;
    fGamma := 2.2;
    fPhysicalHeight := 0;
    fPhysicalWidth := 0;
    fThreshold.Clear;
    fRotation.Clear;
{$IFDEF IEINCLUDEMULTIVIEW}
    if fOwner is TImageEnMIO then
    begin
      fFeederEnabled := true;
      fDuplexEnabled := true;
      fAutoFeed := true;
    end
    else
    begin
{$ENDIF}
      fFeederEnabled := false;
      fDuplexEnabled := false;
      fAutoFeed := false;
{$IFDEF IEINCLUDEMULTIVIEW}
    end;
{$ENDIF}
    fAutoDeskew := false;
    fAutoBorderDetection := false;
    fAutoBright := false;
    fAutoRotate := false;
    fAutoDiscardBlankPages := Twain_AutoDiscard_Disable; // -2 = disable
    fFilter := ietwUndefined;
    fHighlight := -1;
    fShadow := -1;
    fAcceptedImages := -2;
    fAutoScan := false;
    fDeviceOnline := false;
    fAcquireFrameEnabled := false;
    fOrientation.Clear();
    fIndicators := true;
    fillchar(fAcquireFrame, sizeof(TRect), 0);
    fBufferedTransfer := True;
    fUseMemoryHandle := True;
    fFileTransfer := false;
    fUndefinedImageSize := false;
    fStandardSize.clear;
  end;
  // app identification
  fAppVersionInfo := 'n/a';
  fAppManufacturer := 'n/a';
  fAppProductFamily := 'n/a';
  fAppProductName := 'n/a';
  //
  LastError := 0;
  LastErrorStr := '';
end;

{!!
<FS>TIETwainParams.Assign

<FM>Declaration<FC>
procedure Assign(Source: <A TIETwainParams>);

<FM>Description<FN>
Applications can Assign Twain parameters between <A TImageEnIO> and <A TImageEnMIO> components.

<FM>Example<FC>
ImageEnView1.IO.TwainParams.Assign( ImageEnView2.IO.TwainParams );

TImageEnMView1.MIO.TwainParams.Assign( ImageEnMView2.MIO.TwainParams );
!!}
procedure TIETwainParams.Assign(Source: TIETwainParams);
var
  q: integer;
  pt: pTW_IDENTITY;
begin
  fVisibleDialog := Source.fVisibleDialog;
  fShowSettingsOnly := Source.fShowSettingsOnly;
  fCompatibilityMode := Source.fCompatibilityMode;
  fLanguage := Source.fLanguage;
  fCountry := Source.fCountry;
  fSelectedSource := Source.fSelectedSource;
  fSourceListDataValid := Source.fSourceListDataValid;
  fSourceSettings.LoadFromStream(Source.fSourceSettings);
  // free fSourceListData items
  for q := 0 to fSourceListData.Count - 1 do
    Freemem(fSourceListData[q]);
  fSourceListData.Clear;
  // copy fSourceListData items
  for q := 0 to Source.fSourceListData.Count - 1 do
  begin
    getmem(pt, sizeof(TW_IDENTITY));
    move(Source.fSourceListData[q]^, pt^, sizeof(TW_IDENTITY));
    fSourceListData.Add(pt);
  end;
  // copy fCapabilities (item by item)
  with fCapabilities do
  begin
    fXResolution.assign(Source.fCapabilities.fXResolution);
    fYResolution.assign(Source.fCapabilities.fYResolution);
    fXScaling.assign(Source.fCapabilities.fXScaling);
    fYScaling.assign(Source.fCapabilities.fYScaling);
    fPixelType.assign(Source.fCapabilities.fPixelType);
    fBitDepth.assign(Source.fCapabilities.fBitDepth);
    fGamma := Source.fCapabilities.fGamma;
    fPhysicalHeight := Source.fCapabilities.fPhysicalHeight;
    fPhysicalWidth := Source.fCapabilities.fPhysicalWidth;
    fFeederEnabled := Source.fCapabilities.fFeederEnabled;
    fAutoFeed := Source.fCapabilities.fAutoFeed;
    fAutoDeskew := Source.fCapabilities.fAutoDeskew;
    fAutoBorderDetection := Source.fCapabilities.fAutoBorderDetection;
    fAutoBright := Source.fCapabilities.fAutoBright;
    fAutoRotate := Source.fCapabilities.fAutoRotate;
    fAutoDiscardBlankPages := Source.fCapabilities.fAutoDiscardBlankPages;
    fFilter := Source.fCapabilities.fFilter;
    fHighlight := Source.fCapabilities.fHighlight;
    fShadow := Source.fCapabilities.fShadow;
    fAcceptedImages := Source.fCapabilities.fAcceptedImages;
    fAutoScan := Source.fCapabilities.fAutoScan;
    fDeviceOnline := Source.fCapabilities.fDeviceOnline;
    fDuplexEnabled := Source.fCapabilities.fDuplexEnabled;
    fAcquireFrameEnabled := Source.fCapabilities.fAcquireFrameEnabled;
    fOrientation.assign(Source.fCapabilities.fOrientation);
    fIndicators := Source.fCapabilities.fIndicators;
    fAcquireFrame := Source.fCapabilities.fAcquireFrame;
    fBufferedTransfer := Source.fCapabilities.fBufferedTransfer;
    fUseMemoryHandle := Source.fCapabilities.fUseMemoryHandle;
    fFileTransfer := Source.fCapabilities.fFileTransfer;
    fContrast.assign(Source.fCapabilities.fContrast);
    fBrightness.assign(Source.fCapabilities.fBrightness);
    fThreshold.assign(Source.fCapabilities.fThreshold);
    fRotation.Assign(Source.fCapabilities.fRotation);
    fUndefinedImageSize := Source.fCapabilities.fUndefinedImageSize;
    fStandardSize := Source.fCapabilities.fStandardSize;
  end;
  // copy app id
  fAppVersionInfo := Source.fAppVersionInfo;
  fAppManufacturer := Source.fAppManufacturer;
  fAppProductFamily := Source.fAppProductFamily;
  fAppProductName := Source.fAppProductName;
end;

procedure TIETwainParams.FillSourceListData;
var
  q: integer;
begin
  if not fSourceListDataValid then
  begin
    // free fSourceListData items
    for q := 0 to fSourceListData.Count - 1 do
      Freemem(fSourceListData[q]);
    fSourceListData.Clear;
    // get fSourceListData
    fSourceListDataValid := IETW_GetSourceList(fSourceListData, @TwainShared, IEFindHandle(fOwner));
  end;
end;

{!!
<FS>TIETwainParams.GetFromScanner

<FM>Declaration<FC>
function GetFromScanner: boolean;

<FM>Description<FN>
GetFromScanner fills all properties with scanner parameters. Use this method to update ImageEn parameters with scanner values.

Returns False if cannot connect to the scanner.

<FM>Example<FC>
ImageEnView1.IO.GetFromScanner;
ScannerPixelType := ImageEnView1.IO.TwainParams.PixelType.CurrentValue;

!!}
function TIETwainParams.GetFromScanner: boolean;
begin
  result := IETW_GetCapabilities(self, fCapabilities, false, @TwainShared, IEFindHandle(fOwner));
  fCapabilitiesValid := True;
end;

procedure TIETwainParams.FillCapabilities;
begin
  if not fCapabilitiesValid then
  begin
    IETW_GetCapabilities(self, fCapabilities, false, @TwainShared, IEFindHandle(fOwner));
    fCapabilitiesValid := True;
  end;
end;

{!!
<FS>TIETwainParams.SourceName

<FM>Declaration<FC>
property SourceName[index]: AnsiString;

<FM>Description<FN>
SourceName is the name of the index scanner. This list has <A TIETwainParams.SourceCount> values.

Read-only

<FM>Example<FC>
// Fills the ListBox1 with all scanner names installed on the system
for q := 0 to ImageEnView1.IO.TwainParams.SourceCount-1 do
  ListBox1.Items.Add( ImageEnView1.IO.TwainParams.SourceName[q] );

!!}
function TIETwainParams.GetSourceName(idx: integer): AnsiString;
begin
  FillSourceListData;
  if idx < fSourceListData.Count then
    result := pTW_IDENTITY(fSourceListData[idx])^.ProductName
  else
    result := '';
end;

{!!
<FS>TIETwainParams.SourceCount

<FM>Declaration<FC>
property SourceCount: integer;

<FM>Description<FN>
SourceCount is the count of the installed scanners. It also defines the length of the <A TIETwainParams.SourceName>[] list.

Read-only

<FM>Example<FC>
// Fills the ListBox1 with all scanner names installed on the system
for q := 0 to ImageEnView1.IO.TwainParams.SourceCount-1 do
  ListBox1.Items.Add( ImageEnView1.IO.TwainParams.SourceName[q] );
!!}
function TIETwainParams.GetSourceCount: integer;
begin
  FillSourceListData;
  result := fSourceListData.Count;
end;

{!!
<FS>TIETwainParams.SelectSourceByName

<FM>Declaration<FC>
function SelectSourceByName(const sn: AnsiString): boolean;

<FM>Description<FN>
SelectSourceByName selects the first device that matches left name string with <FC>sn<FN>.
Returns true if device is found.
The list of names is contained in <A TIETwainParams.SourceName>[] list.

<FM>Example<FC>
// Select the second scanner
ImageEnView1.IO.TwainParams.SelectedSource := 1;

// OR Select scanner by name
ImageEnView1.IO.TwainParams.SelectSourceByName('CanoScan FB620');

// OR Select scanner with standard dialog
ImageEnView1.IO.SelectAcquireSource;

// THEN Acquire
ImageEnView1.IO.Acquire;
!!}
function TIETwainParams.SelectSourceByName(const sn: AnsiString): boolean;
var
  q: integer;
  bestMatch: integer;
  bestMatchLen: integer;
  toFind, current: AnsiString;
  minLen: integer;
begin
  result := false;
  FillSourceListData;

  bestMatch := -1;
  toFind := IEUpperCase(sn);

  // search for exact match
  for q := 0 to fSourceListData.Count - 1 do
    if (IEUpperCase(IETrim(AnsiString(pTW_IDENTITY(fSourceListData[q])^.ProductName))) = toFind) then
    begin
      bestMatch := q;
      break;
    end;

  if bestMatch = -1 then
  begin
    // partial match
    bestMatchLen := 0;
    for q := 0 to fSourceListData.Count - 1 do
    begin
      current := IETrim(AnsiString(pTW_IDENTITY(fSourceListData[q])^.ProductName));
      minLen := imin(length(current), length(toFind));
      if (IEUpperCase(IECopy(current, 1, minLen)) = IECopy(toFind, 1, minLen)) and (minLen > bestMatchLen) then
      begin
        bestMatch := q;
        bestMatchLen := minLen;
      end;
    end;
  end;
  if bestMatch <> -1 then
  begin
    if fSelectedSource <> bestMatch then
      SetSelectedSource(bestMatch);
    result := true;
  end;

  // Make Twain the API for subsequent calls to Acquire
  {$IFDEF IEINCLUDEMULTIVIEW}
  If Result and (fOwner is TImageEnMIO) then
    (fOwner as TImageEnMIO).AcquireParams.fSelectedSourceAPI := ieaTwain
  else
  {$ENDIF}
  If Result and (fOwner is TImageEnIO) then
    (fOwner as TImageEnIO).AcquireParams.fSelectedSourceAPI := ieaTwain;
end;

{!!
<FS>TIETwainParams.XResolution

<FM>Declaration<FC>
property XResolution: <A TIEDoubleList>;

<FM>Description<FN>
XResolution is the DPI (Dots per Inch) in the X-axis.

Allowed values can be assigned to XResolution.CurrentValue property. To see which values your scanner supports, consult the XResolution.Items[] array, or XResolution.RangeMin, XResolution.RangeMax and XResolution.RangeStep.

You can also limit scanner user interface allowed values by removing some XResolution.Items[] items or changing XResolution.RangeMin, XResolution.RangeMax and XResolution.RangeStep.

<FM>Example<FC>
// Acquire with 100 DPI (if supported by scanner)
ImageEnView1.IO.YResolution.CurrentValue := 100;
ImageEnView1.IO.XResolution.CurrentValue := 100;
ImageEnView1.IO.VisibleDialog := False;
ImageEnView1.IO.Acquire;

!!}
function TIETwainParams.GetXResolution: TIEDoubleList;
begin
  FillCapabilities;
  result := fCapabilities.fXResolution;
end;

{!!
<FS>TIETwainParams.YResolution

<FM>Declaration<FC>
property YResolution: <A TIEDoubleList>;

<FM>Description<FN>
YResolution is the DPI (Dots per Inch) in the Y-axis.

Allowed values can be assigned to YResolution.CurrentValue property. To see which values your scanner supports, consult the YResolution.Items[] array, or YResolution.RangeMin, YResolution.RangeMax and YResolution.RangeStep.

You can also limit scanner user interface allowed values by removing some YResolution.Items[] items or by changing YResolution.RangeMin, YResolution.RangeMax or YResolution.RangeStep.

<FM>Example<FC>
// Acquire with 100 DPI (if supported by scanner)
ImageEnView1.IO.YResolution.CurrentValue := 100;
ImageEnView1.IO.XResolution.CurrentValue := 100;
ImageEnView1.IO.VisibleDialog := False;
ImageEnView1.IO.Acquire;

!!}
function TIETwainParams.GetYResolution: TIEDoubleList;
begin
  FillCapabilities;
  result := fCapabilities.fYResolution;
end;

{!!
<FS>TIETwainParams.XScaling


<FM>Declaration<FC>
property XScaling: <A TIEDoubleList>;

<FM>Description<FN>
XScaling  is the X-axis scaling value. A value of 1.0 is equivalent to 100% scaling.

Allowed values can be assigned to XScaling.CurrentValue property. To see which values your scanner supports, consult the XScaling.Items[] array, or XScaling.RangeMin, XScaling.RangeMax and XScaling.RangeStep.

You can also limit scanner user interface allowed values by removing some XResolution.Items[] items or by changing XResolution.RangeMin, XResolution.RangeMax and XResolution.RangeStep.

!!}
function TIETwainParams.GetXScaling: TIEDoubleList;
begin
  FillCapabilities;
  result := fCapabilities.fXScaling;
end;

{!!
<FS>TIETwainParams.YScaling

<FM>Declaration<FC>
property YScaling: <A TIEDoubleList>;

<FM>Description<FN>
YScaling is the Y-axis scaling value. A value of 1.0 is equivalent to 100% scaling.

Allowed values can be assigned to the YScaling.CurrentValue property. To see which values your scanner supports, consult the YScaling.Items[] array, or YScaling.RangeMin, YScaling.RangeMax and YScaling.RangeStep.

You can also limit scanner user interface allowed values by removing some YScaling.Items[] items or by changing YResolution.RangeMin, YResolution.RangeMax and YResolution.RangeStep.
!!}
function TIETwainParams.GetYScaling: TIEDoubleList;
begin
  FillCapabilities;
  result := fCapabilities.fYScaling;
end;

{!!
<FS>TIETwainParams.Contrast

<FM>Declaration<FC>
property Contrast: <A TIEDoubleList>;

<FM>Description<FN>
Contrast value. Allowed range is -1000 to +1000.

Allowed values can be assigned to Contrast.CurrentValue property. To see which values your scanner supports consult the Contrast.Items[] array, or Contrast.RangeMin, Contrast.RangeMax and Contrast.RangeStep.
You can also limit scanner user interface allowed values by removing some Contrast.Items[] items or changing Contrast.RangeMin, Contrast.RangeMax or Contrast.RangeStep.

!!}
function TIETwainParams.GetContrast: TIEDoubleList;
begin
  FillCapabilities;
  result := fCapabilities.fContrast;
end;

{!!
<FS>TIETwainParams.Threshold

<FM>Declaration<FC>
property Threshold: <A TIEDoubleList>;

<FM>Description<FN>
Threshold specifies the dividing line between black and white. Allowed values: 0..255.

!!}
function TIETwainParams.GetThreshold: TIEDoubleList;
begin
  FillCapabilities;
  result := fCapabilities.fThreshold;
end;

{!!
<FS>TIETwainParams.Rotation

<FM>Declaration<FC>
property Rotation: <A TIEDoubleList>;

<FM>Description<FN>
Specifies how much the Source can/should rotate the scanned image data prior to transfer. Allowed values are in the range: -360..+360 degrees.
Note: Rotation direction is clockwise

!!}
function TIETwainParams.GetRotation: TIEDoubleList;
begin
  FillCapabilities;
  result := fCapabilities.fRotation;
end;

{!!
<FS>TIETwainParams.Brightness

<FM>Declaration<FC>
property Brightness: <A TIEDoubleList>;

<FM>Description<FN>
Brightness value. Allowed range is -1000 to +1000.

Allowed values can be assigned to Brightness.CurrentValue property. To see which values your scanner supports consult the Brightness.Items[] array, or Brightness.RangeMin, Brightness.RangeMax and Brightness.RangeStep.
You can also limit scanner user interface allowed values removing some Brightness.Items[] items or changing Brightness.RangeMin, Brightness.RangeMax and Brightness.RangeStep.
!!}
function TIETwainParams.GetBrightness: TIEDoubleList;
begin
  FillCapabilities;
  result := fCapabilities.fBrightness;
end;

{!!
<FS>TIETwainParams.PixelType

<FM>Declaration<FC>
property PixelType: <A TIEIntegerList>;

<FM>Description<FN>
PixelType is the type of pixel data that a scanner is capable of acquiring.
Valid values are:

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>Twain_PixelType_BW (0)<FN></C> <C>Black & white (1 bit)</C> </R>
<R> <C><FC>Twain_PixelType_Grayscale(1)<FN></C> <C>Gray scale (8 bit)</C> </R>
<R> <C><FC>Twain_PixelType_FullRGB (2)<FN></C> <C>Full RGB (24 bit)</C> </R>
<R> <C><FC>Twain_PixelType_Palette (3)<FN></C> <C>Palette (ImageEn can't support this)</C> </R>
<R> <C><FC>Twain_PixelType_CMY (4)<FN></C> <C>CMY (ImageEn can't support this)</C> </R>
<R> <C><FC>Twain_PixelType_CMYK (5)<FN></C> <C>CMYK (ImageEn can't support this)</C> </R>
<R> <C><FC>Twain_PixelType_YUV (6)<FN></C> <C>YUV (ImageEn can't support this)</C> </R>
<R> <C><FC>Twain_PixelType_YUVK (7)<FN></C> <C>YUVK (ImageEn can't support this)</C> </R>
<R> <C><FC>Twain_PixelType_CIEXYZ (8)<FN></C> <C>CIEXYZ (ImageEn can't support this)</C> </R>
</TABLE>

Above values can be assigned to PixelType.<A TIEIntegerList.CurrentValue> property. To see which values your scanner supports, consult the PixelType.<A TIEIntegerList.Items>[] array.
You can also limit scanner user interface allowed values by removing some PixelTtype.Items[] items.

<FM>Example<FC>
// Acquires a black/white (1bit) image
ImageEnView1.IO.TwainParams.PixelType.CurrentValue:= Twain_PixelType_BW;
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

// Acquires a gray scale (8bit) image
ImageEnView1.IO.TwainParams.PixelType.CurrentValue := Twain_PixelType_Grayscale;
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

// Acquires a full RGB (24bit) image
ImageEnView1.IO.TwainParams.PixelType.CurrentValue := Twain_PixelType_FullRGB;
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

// Show standard scanner user interface, but allow only black/white image acquisition
ImageEnView1.IO.TwainParams.PixelType.Clear;
ImageEnView1.IO.TwainParams.PixelType.Add(Twain_PixelType_BW);
ImageEnView1.IO.Acquire;
!!}
function TIETwainParams.GetPixelType: TIEIntegerList;
begin
  FillCapabilities;
  result := fCapabilities.fPixelType;
end;

{!!
<FS>TIETwainParams.BitDepth

<FM>Declaration<FC>
property BitDepth: <A TIEIntegerList>;

<FM>Description<FN>
Specifies the bit depth (bits per channel) of the image to scan.

<FM>Example<FC>
// acquire 48 bit (RGB, 16 bit per channel) native pixel format image
ImageEnView1.LegacyBitmap := false;
ImageEnView1.IO.NativePixelFormat := true;
ImageEnView1.IO.TwainParams.BitDepth.CurrentValue := 16;
ImageEnView1.IO.Acquire;

!!}
function TIETwainParams.GetBitDepth: TIEIntegerList;
begin
  FillCapabilities;
  result := fCapabilities.fBitDepth;
end;

{!!
<FS>TIETwainParams.Gamma

<FM>Declaration<FC>
property Gamma: double;

<FM>Description<FN>
Gamma is the gamma value of your scanner.

Read-only
!!}
function TIETwainParams.GetGamma: double;
begin
  FillCapabilities;
  result := fCapabilities.fGamma;
end;

{!!
<FS>TIETwainParams.FeederLoaded

<FM>Declaration<FC>
property FeederLoaded: boolean;

<FM>Description<FN>
Use the FeederLoaded property to reflect whether or not there are documents loaded in the Source's feeder.

<FM>Example<FC>
// use of TImageEnIO (instead of TImageEnMIO) to acquire and save multi pages
while ImageEnView1.IO.TwainParams.FeederLoaded do
begin
  ImageEnView1.IO.Acquire;
  ImageEnView1.IO.SaveToFile('page'+inttostr(count)+'.jpg');
  Inc( count );
end;

!!}
function TIETwainParams.GetFeederLoaded: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fFeederLoaded;
end;

{!!
<FS>TIETwainParams.PaperDetectable

<FM>Declaration<FC>
property PaperDetectable: boolean;

<FM>Description<FN>
If PaperDetectable is True, the scanner is able to detect paper.
!!}
function TIETwainParams.GetPaperDetectable: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fPaperDetectable;
end;

{!!
<FS>TIETwainParams.DuplexSupported

<FM>Declaration<FC>
property DuplexSupported: boolean;

<FM>Description<FN>
If DuplexSupported is True, the scanner can scans both sides of a paper; otherwise the scanner will scan only one side.

<FM>Example<FC>
// enables duplex if supported
If ImageEnMView1.MIO.TwainParams.DuplexSupported then
  ImageEnView1.IO.TwainParams.DuplexEnabled := True;

!!}
function TIETwainParams.GetDuplexSupported: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fDuplexSupported;
end;

{!!
<FS>TIETwainParams.PhysicalHeight

<FM>Declaration<FC>
property PhysicalHeight: double;

<FM>Description<FN>
PhysicalHeight is the maximum physical height (Y-axis) the scanner can acquire (measured in inches).

Read-only
!!}
function TIETwainParams.GetPhysicalHeight: double;
begin
  FillCapabilities;
  result := fCapabilities.fPhysicalHeight;
end;

{!!
<FS>TIETwainParams.PhysicalWidth

<FM>Declaration<FC>
property PhysicalWidth: double;

<FM>Description<FN>
PhysicalWidth is the maximum physical width (X-axis) the scanner can acquire (measured in inches).

Read-only

!!}
function TIETwainParams.GetPhysicalWidth: double;
begin
  FillCapabilities;
  result := fCapabilities.fPhysicalWidth;
end;

{!!
<FS>TIETwainParams.FeederEnabled

<FM>Declaration<FC>
property FeederEnabled: boolean;

<FM>Description<FN>
FeederEnabled enables the feed loader mechanism when present.
Use this property only within <A TImageEnMIO> component to disable the feed loader.

<FM>Example<FC>
// Acquires only one feed
ImageEnMView1.MIO.TwainParams.FeederEnabled := False;
ImageEnMView1.MIO.Acquire;
!!}
function TIETwainParams.GetFeederEnabled: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fFeederEnabled;
end;

{!!
<FS>TIETwainParams.AutoFeed

<FM>Declaration<FC>
property AutoFeed: boolean;

<FM>Description<FN>
If AutoFeed is true, the scanner will automatically feed the next page from the document feeder.
!!}
function TIETwainParams.GetAutoFeed: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fAutoFeed;
end;

{!!
<FS>TIETwainParams.AutoDeskew

<FM>Declaration<FC>
property AutoDeskew: boolean;

<FM>Description<FN>
Turns automatic deskew correction on and off.

!!}
function TIETwainParams.GetAutoDeskew: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fAutoDeskew;
end;

{!!
<FS>TIETwainParams.AutoBorderDetection

<FM>Declaration<FC>
property AutoBorderDetection: boolean;

<FM>Description<FN>
Turns automatic border detection on and off.
!!}
function TIETwainParams.GetAutoBorderDetection: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fAutoBorderDetection;
end;

{!!
<FS>TIETwainParams.AutoBright

<FM>Declaration<FC>
property AutoBright: boolean;

<FM>Description<FN>
TRUE enables and FALSE disables the Source's Auto-brightness function (if any).

!!}
function TIETwainParams.GetAutoBright: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fAutoBright;
end;

{!!
<FS>TIETwainParams.AutoRotate

<FM>Declaration<FC>
property AutoRotate: boolean;

<FM>Description<FN>
When <FC>true<FN> this capability depends on intelligent features within the Source to automatically rotate the image to the correct position.
!!}
function TIETwainParams.GetAutoRotate: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fAutoRotate;
end;

{!!
<FS>TIETwainParams.AutoDiscardBlankPages

<FM>Declaration<FC>
property AutoDiscardBlankPages: integer;

<FM>Description<FN>
Use this capability to have the scanner discard blank images.
Applications never see these images during the scanning session.
Allowed values:

<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>Twain_AutoDiscard_Disable (-2)<FN></C> <C>All images will be delivered to the application even if they are blank</C> </R>
<R> <C><FC>Twain_AutoDiscard_DiscardBlank (-1)<FN></C> <C>The scanner will decide if an image is blank or not and discard as appropriate</C> </R>
<R> <C><FC>>=0<FN></C>
<C>The scanner will use the value as the byte size cutoff point to identify which images to discard.
If the image size is less than or equal to this value, then it will be discarded.
If the image size is greater than this value, then it will be transferred to the application.</C></R>
</TABLE>
!!}
function TIETwainParams.GetAutoDiscardBlankPages: integer;
begin
  FillCapabilities;
  result := fCapabilities.fAutoDiscardBlankPages;
end;


{!!
<FS>TIETwainParams.Filter

<FM>Declaration<FC>
property Filter: <A TIETWFilter>;

<FM>Description<FN>
Describes the color characteristic of the subtractive filter applied to the image data.
ietwUndefined (default) unsets this property, while ietwNone set "none" value.
!!}
function TIETwainParams.GetFilter: TIETWFilter;
begin
  FillCapabilities;
  result := fCapabilities.fFilter;
end;

{!!
<FS>TIETwainParams.Highlight

<FM>Declaration<FC>
property Highlight: double;

<FM>Description<FN>
Specifies which value in an image should be interpreted as the lightest "highlight". All values "lighter" than this value will be clipped to this value.
-1 is the default scanner value (it means "do not change current value").
Allowed values from 0 to 255.
!!}
function TIETwainParams.GetHighlight: double;
begin
  FillCapabilities;
  result := fCapabilities.fHighlight;
end;

{!!
<FS>TIETwainParams.Shadow

<FM>Declaration<FC>
property Shadow: double;

<FM>Description<FN>
Specifies which value in an image should be interpreted as the darkest "shadow". All values "darker" than this value will be clipped to this value.
-1 is the default scanner value (it means "do not change current value").
Allowed values from 0 to 255.
!!}
function TIETwainParams.GetShadow: double;
begin
  FillCapabilities;
  result := fCapabilities.fShadow;
end;

{!!
<FS>TIETwainParams.AcceptedImages

<FM>Declaration<FC>
property AcceptedImages: integer;

<FM>Description<FN>
Set this capability to the number of images you are willing to transfer per session.
-2 = default scanner value
-1 = multiple images
>1 = one o more images
!!}
function TIETwainParams.GetAcceptedImages: integer;
begin
  FillCapabilities;
  result := fCapabilities.fAcceptedImages;
end;

{!!
<FS>TIETwainParams.AutoScan

<FM>Declaration<FC>
property AutoScan: boolean;

<FM>Description<FN>
Enables/Disables auto scan.
This capability is intended to boost the performance of a Source.
The fundamental assumption behind AutoScan is that the device is able to capture the number of images indicated by the value of <A TIETwainParams.AcceptedImages> without waiting for the Application to request the image transfers.
!!}
function TIETwainParams.GetAutoScan: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fAutoScan;
end;

{!!
<FS>TIETwainParams.DeviceOnline

<FM>Declaration<FC>
property DeviceOnline: boolean;

<FM>Description<FN>
If True, the physical hardware (e.g., scanner, digital camera, image database, etc.) that represents the image source is attached, powered on, and communicating.

Warning!! Several (almost all) devices still return "true" when they are off or disconnected.
!!}
function TIETwainParams.GetDeviceOnline: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fDeviceOnline;
end;

{!!
<FS>TIETwainParams.UndefinedImageSize

<FM>Declaration<FC>
property UndefinedImageSize: boolean;

<FM>Description<FN>
UndefinedImageSize enables support for an undefined image size scanner. Default is False.
!!}
function TIETwainParams.GetUndefinedImageSize: boolean;
begin
  // no need FillCapabilities
  result := fCapabilities.fUndefinedImageSize;
end;

{!!
<FS>TIETwainParams.DuplexEnabled

<FM>Declaration<FC>
property DuplexEnabled: boolean;

<FM>Description<FN>
If DuplexEnabled is True, the scanner scans both sides of a paper; otherwise (default), the scanner will scan only one side.
Use this property only within <A TImageEnMIO> component to enable/disable duplex mode.
!!}
function TIETwainParams.GetDuplexEnabled: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fDuplexEnabled;
end;

{!!
<FS>TIETwainParams.AcquireFrameEnabled

<FM>Declaration<FC>
property AcquireFrameEnabled: boolean;

<FM>Description<FN>
If AcquireFrameEnabled is True, it enables the properties <A TIETwainParams.AcquireFrameLeft>, <A TIETwainParams.AcquireFrameRight>, <A TIETwainParams.AcquireFrameTop> and <A TIETwainParams.AcquireFrameBottom>.

When True, some scanners don't allow the user to change the acquisition frame.
Default is False.

!!}
function TIETwainParams.GetAcquireFrameEnabled: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fAcquireFrameEnabled;
end;

{!!
<FS>TIETwainParams.Orientation

<FM>Declaration<FC>
property Orientation: <A TIEIntegerList>;

<FM>Description<FN>
Orientation defines the orientation of the output image. Not all scanners support this capability.
Valid values are:
<TABLE>
<R> <H>Value</H> <H>Description</H> </R>
<R> <C><FC>Twain_Orientation_NoRotate (0)<FN></C> <C>No rotation (Portrait)</C> </R>
<R> <C><FC>Twain_Orientation_Rotate90 (1)<FN></C> <C>Rotate 90°</C> </R>
<R> <C><FC>Twain_Orientation_Rotate180 (2)<FN></C> <C>Rotate 180°</C> </R>
<R> <C><FC>Twain_Orientation_Rotate270 (3)<FN></C> <C>Rotate 270° (Landscape)</C> </R>
</TABLE>
Note: Rotation direction is clockwise

The above values can be assigned to the Orientation.<A TIEIntegerList.CurrentValue> property.
To know which values your scanner supports, consult the Orientation.<A TIEIntegerList.Items>[] array.

<FM>Example<FC>
// Acquire image in landscape orientation (if supported)
ImageEnView1.IO.TwainParams.Orientation.CurrentValue := 3;
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;

// As above, but control if landscape is supported
if ImageEnView1.IO.TwainParams.Orientation.IndexOf(3)=3 then
begin
  ImageEnView1.IO.TwainParams.Orientation.CurrentValue := 3;
  ImageEnView1.IO.TwainParams.VisibleDialog := False;
  ImageEnView1.IO.Acquire;
end
else
  ShowMessage('landscape isn't supported');

!!}
function TIETwainParams.GetOrientation: TIEIntegerList;
begin
  FillCapabilities;
  result := fCapabilities.fOrientation;
end;

{!!
<FS>TIETwainParams.StandardSize

<FM>Declaration<FC>
property StandardSize: <A TIEIntegerList>;

<FM>Description<FN>
StandardSize page size for devices that support fixed frame sizes.
Defined sizes match typical page sizes. This specifies the size(s) the Source can/should use to acquire image data.
Allowed values:

   IETW_NONE
   IETW_A4LETTER
   IETW_B5LETTER
   IETW_USLETTER
   IETW_USLEGAL
   IETW_A5
   IETW_B4
   IETW_B6
   IETW_USLEDGER
   IETW_USEXECUTIVE
   IETW_A3
   IETW_B3
   IETW_A6
   IETW_C4
   IETW_C5
   IETW_C6
   IETW_4A0
   IETW_2A0
   IETW_A0
   IETW_A1
   IETW_A2
   IETW_A4
   IETW_A7
   IETW_A8
   IETW_A9
   IETW_A10
   IETW_ISOB0
   IETW_ISOB1
   IETW_ISOB2
   IETW_ISOB3
   IETW_ISOB4
   IETW_ISOB5
   IETW_ISOB6
   IETW_ISOB7
   IETW_ISOB8
   IETW_ISOB9
   IETW_ISOB10
   IETW_JISB0
   IETW_JISB1
   IETW_JISB2
   IETW_JISB3
   IETW_JISB4
   IETW_JISB5
   IETW_JISB6
   IETW_JISB7
   IETW_JISB8
   IETW_JISB9
   IETW_JISB10
   IETW_C0
   IETW_C1
   IETW_C2
   IETW_C3
   IETW_C7
   IETW_C8
   IETW_C9
   IETW_C10
   IETW_USSTATEMENT
   IETW_BUSINESSCARD

<FM>Example<FC>
ImageEnView1.IO.TwainParams.StandardSize.CurrentValue := IETW_A4LETTER

!!}
function TIETwainParams.GetStandardSize: TIEIntegerList;
begin
  FillCapabilities;
  result := fCapabilities.fStandardSize;
end;

function TIETwainParams.GetIndicators: boolean;
begin
  FillCapabilities;
  result := fCapabilities.fIndicators;
end;

procedure TIETwainParams.SetFeederEnabled(v: boolean);
begin
  fCapabilities.fFeederEnabled := v;
end;

procedure TIETwainParams.SetAutoFeed(v: boolean);
begin
  fCapabilities.fAutoFeed := v;
end;

procedure TIETwainParams.SetAutoDeskew(v: boolean);
begin
  fCapabilities.fAutoDeskew := v;
end;

procedure TIETwainParams.SetAutoBorderDetection(v: boolean);
begin
  fCapabilities.fAutoBorderDetection := v;
end;

procedure TIETwainParams.SetAutoBright(v: boolean);
begin
  fCapabilities.fAutoBright := v;
end;

procedure TIETwainParams.SetAutoRotate(v: boolean);
begin
  fCapabilities.fAutoRotate := v;
end;

procedure TIETwainParams.SetAutoDiscardBlankPages(v: integer);
begin
  fCapabilities.fAutoDiscardBlankPages := v;
end;

procedure TIETwainParams.SetFilter(v: TIETWFilter);
begin
  fCapabilities.fFilter := v;
end;

procedure TIETwainParams.SetHighlight(v: double);
begin
  fCapabilities.fHighlight := v;
end;

procedure TIETwainParams.SetShadow(v: double);
begin
  fCapabilities.fShadow := v;
end;

procedure TIETwainParams.SetAcceptedImages(v: integer);
begin
  fCapabilities.fAcceptedImages := v;
end;

procedure TIETwainParams.SetAutoScan(v: boolean);
begin
  fCapabilities.fAutoScan := v;
end;

procedure TIETwainParams.SetUndefinedImageSize(v: boolean);
begin
  fCapabilities.fUndefinedImageSize := v;
end;

procedure TIETwainParams.SetDuplexEnabled(v: boolean);
begin
  fCapabilities.fDuplexEnabled := v;
end;

procedure TIETwainParams.SetAcquireFrameEnabled(v: boolean);
begin
  fCapabilities.fAcquireFrameEnabled := v;
end;

{!!
<FS>TIETwainParams.ProgressIndicators

<FM>Declaration<FC>
property ProgressIndicators: boolean;

<FM>Description<FN>
If ProgressIndicators is True (default), the scanner will display a progress indicator during acquisition and transfer, regardless of whether the scanner user interface is active.
You can use <A TImageEnIO.OnProgress> event to display your custom progress indicator.
!!}
procedure TIETwainParams.SetIndicators(v: boolean);
begin
  fCapabilities.fIndicators := v;
end;

function TIETwainParams.GetAcquireFrame(idx: integer): double;
begin
  FillCapabilities;
  case idx of
    0: result := fCapabilities.fAcquireFrame.Left;
    1: result := fCapabilities.fAcquireFrame.Top;
    2: result := fCapabilities.fAcquireFrame.Right;
    3: result := fCapabilities.fAcquireFrame.Bottom;
  else
    result := 0;
  end;
end;

{!!
<FS>TIETwainParams.AcquireFrameLeft

<FM>Declaration<FC>
property AcquireFrameLeft: double;

<FM>Description<FN>
AcquireFrameLeft is the left of the rectangle to acquire measured in inches.

<FM>See Also<FN>
- <A TIETwainParams.AcquireFrameTop>
- <A TIETwainParams.AcquireFrameRight>
- <A TIETwainParams.AcquireFrameBottom>

<FM>Example<FC>
// Acquires the 2,2,7,7 rectangle without display scanner dialog
ImageEnView1.IO.TwainParams.AcquireFrameTop := 2;
ImageEnView1.IO.TwainParams.AcquireFrameLeft := 2;
ImageEnView1.IO.TwainParams.AcquireFrameRight := 7;
ImageEnView1.IO.TwainParams.AcquireFrameBottom := 7;
ImageEnView1.IO.TwainParams.AcquireFrameEnabled := true;
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;
!!}
{!!
<FS>TIETwainParams.AcquireFrameBottom

<FM>Declaration<FC>
property AcquireFrameBottom: double;

<FM>Description<FN>
AcquireFrameBottom is the bottom of the rectangle to acquire measured in inches.

<FM>See Also<FN>
- <A TIETwainParams.AcquireFrameLeft>
- <A TIETwainParams.AcquireFrameTop>
- <A TIETwainParams.AcquireFrameRight>

<FM>Example<FC>
// Acquires the 2,2,7,7 rectangle without display scanner dialog
ImageEnView1.IO.TwainParams.AcquireFrameTop := 2;
ImageEnView1.IO.TwainParams.AcquireFrameLeft := 2;
ImageEnView1.IO.TwainParams.AcquireFrameRight := 7;
ImageEnView1.IO.TwainParams.AcquireFrameBottom := 7;
ImageEnView1.IO.TwainParams.AcquireFrameEnabled := true;
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire
!!}
{!!
<FS>TIETwainParams.AcquireFrameRight

<FM>Declaration<FC>
property AcquireFrameRight: double;

<FM>Description<FN>
AcquireFrameRight is the right side of the rectangle to acquire measured in inches.

<FM>See Also<FN>
- <A TIETwainParams.AcquireFrameLeft>
- <A TIETwainParams.AcquireFrameTop>
- <A TIETwainParams.AcquireFrameBottom>

<FM>Example<FC>
// Acquires the 2,2,7,7 rectangle without display scanner dialog
ImageEnView1.IO.TwainParams.AcquireFrameTop := 2;
ImageEnView1.IO.TwainParams.AcquireFrameLeft := 2;
ImageEnView1.IO.TwainParams.AcquireFrameRight := 7;
ImageEnView1.IO.TwainParams.AcquireFrameBottom := 7;
ImageEnView1.IO.TwainParams.AcquireFrameEnabled := true;
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;
!!}
{!!
<FS>TIETwainParams.AcquireFrameTop

<FM>Declaration<FC>
property AcquireFrameTop: double;

<FM>Description<FN>
AcquireFrameTop is the top of the rectangle to acquire measured in inches.

<FM>See Also<FN>
- <A TIETwainParams.AcquireFrameLeft>
- <A TIETwainParams.AcquireFrameRight>
- <A TIETwainParams.AcquireFrameBottom>

<FM>Example<FC>
// Acquires the 2,2,7,7 rectangle without display scanner dialog
ImageEnView1.IO.TwainParams.AcquireFrameTop := 2;
ImageEnView1.IO.TwainParams.AcquireFrameLeft := 2;
ImageEnView1.IO.TwainParams.AcquireFrameRight := 7;
ImageEnView1.IO.TwainParams.AcquireFrameBottom := 7;
ImageEnView1.IO.TwainParams.AcquireFrameEnabled := true;
ImageEnView1.IO.TwainParams.VisibleDialog := False;
ImageEnView1.IO.Acquire;
!!}
procedure TIETwainParams.SetAcquireFrame(idx: integer; v: double);
begin
  case idx of
    0: fCapabilities.fAcquireFrame.Left := v;
    1: fCapabilities.fAcquireFrame.Top := v;
    2: fCapabilities.fAcquireFrame.Right := v;
    3: fCapabilities.fAcquireFrame.Bottom := v;
  end;
end;

{!!
<FS>TIETwainParams.BufferedTransfer

<FM>Declaration<FC>
property BufferedTransfer: boolean;

<FM>Description<FN>
Set the BufferedTransfer property to False if you have problems acquiring an image.
You never use this property under normal circumstances.
!!}
function TIETwainParams.GetBufferedTransfer: boolean;
begin
  result := fCapabilities.fBufferedTransfer;
end;

procedure TIETwainParams.SetBufferedTransfer(v: boolean);
begin
  fCapabilities.fBufferedTransfer := v;
end;

{!!
<FS>TIETwainParams.UseMemoryHandle

<FM>Declaration<FC>
property UseMemoryHandle: boolean;

<FM>Description<FN>
Some scanner's drivers do not support memory handles, so we must pass memory pointers in order to transfer images.
You should try to set this property to False if you have problems scanning documents or images.
!!}
function TIETwainParams.GetUseMemoryHandle: boolean;
begin
  result := fCapabilities.fUseMemoryHandle;
end;

procedure TIETwainParams.SetUseMemoryHandle(v: boolean);
begin
  fCapabilities.fUseMemoryHandle := v;
end;

{!!
<FS>TIETwainParams.FileTransfer

<FM>Declaration<FC>
property FileTransfer: boolean;

<FM>Description<FN>
If FileTransfer true, uses a file transfer to get images from the scanner (regardless of <A TIETwainParams.BufferedTransfer> setting).
There are three ways to get an image from scanner:
1 - native transfer ( set FileTransfer=False and BufferedTransfer=False )
2 - buffered transfer ( set FileTransfer=False and BufferedTransfer=True )
3 - file transfer ( set FileTransfer=True )

FileTransfer is slow but more compatible.

!!}
function TIETwainParams.GetFileTransfer: boolean;
begin
  result := fCapabilities.fFileTransfer;
end;

procedure TIETwainParams.SetFileTransfer(v: boolean);
begin
  fCapabilities.fFileTransfer := v;
end;

{!!
<FS>TIETwainParams.SelectedSource

<FM>Declaration<FC>
property SelectedSource: integer;

<FM>Description<FN>
SelectedSource is an index of SourceName[] list, and defines the currently selected scanner (source).

The default is 0 (first scanner).
This is an alternative method to select a scanner without calling the <A TImageEnIO.SelectAcquireSource> method.

<FM>Example<FC>
// Select the second scanner
ImageEnView1.IO.TwainParams.SelectedSource := 1;

// OR Select scanner by name
ImageEnView1.IO.TwainParams.SelectSourceByName('CanoScan FB620');

// OR Select scanner with standard dialog
ImageEnView1.IO.SelectAcquireSource;

// THEN Acquire
ImageEnView1.IO.Acquire;
!!}
procedure TIETwainParams.SetSelectedSource(v: integer);
begin
  // Make Twain the API for subsequent calls to Acquire
  {$IFDEF IEINCLUDEMULTIVIEW}
  If fOwner is TImageEnMIO then
    (fOwner as TImageEnMIO).AcquireParams.fSelectedSourceAPI := ieaTwain
  else
  {$ENDIF}
  If fOwner is TImageEnIO then
    (fOwner as TImageEnIO).AcquireParams.fSelectedSourceAPI := ieaTwain;
  

  if v <> fSelectedSource then
  begin
    fSelectedSource := v;
    fCapabilitiesValid := false;
    FillCapabilities;
  end;
end;

{!!
<FS>TIETwainParams.Update

<FM>Declaration<FC>
procedure Update;

<FM>Description<FN>
The Update method determines if the current parameters are valid.

For example, if the application assigns a combination of <A TIETwainParams.PixelType> and <A TIETwainParams.YResolution> unsupported by scanner, the Update method restores the PixelType and YResolution value to a value supported by the scanner.

Note: Don't use this method to refresh the list of available devices, use <A TIETwainParams.SetDefaultParams> instead.

<FM>Example<FC>
// Try if RGB and YResolution combination is supported
ImageEnView1.IO.TwainParams.PixelType.CurrentValue := 2;
ImageEnView1.IO.TwainParams.YResolution.CurrentValue := 300;
ImageEnView1.IO.TwainParams.Update;
if (ImageEnView1.IO.TwainParams.PixelType.CurrentValue<>2) or (ImageEnView1.IO.TwainParams.YResolution.CurrentValue<>300) then
  ShowMessage('error!');
!!}
procedure TIETwainParams.Update;
begin
  IETW_GetCapabilities(self, fCapabilities, true, @TwainShared, IEFindHandle(fOwner));
  fCapabilitiesValid := True;
end;

{!!
<FS>TIETwainParams.AppVersionInfo

<FM>Declaration<FC>
property AppVersionInfo: AnsiString;

<FM>Description<FN>
These properties specify the application information communicated to the Twain scanner interface.

!!}
procedure TIETwainParams.SetAppVersionInfo(v: AnsiString);
begin
  fAppVersionInfo := IECopy(v, 1, 32);
end;

{!!
<FS>TIETwainParams.AppManufacturer

<FM>Declaration<FC>
property AppManufacturer: AnsiString;

<FM>Description<FN>
These properties specify the application information communicated to the Twain scanner interface.
!!}
procedure TIETwainParams.SetAppManufacturer(v: AnsiString);
begin
  fAppManufacturer := IECopy(v, 1, 32);
end;

{!!
<FS>TIETwainParams.AppProductfamily

<FM>Declaration<FC>
property AppProductfamily: AnsiString;

<FM>Description<FN>
These properties specify the application information communicated to the Twain scanner interface.
!!}
procedure TIETwainParams.SetAppProductFamily(v: AnsiString);
begin
  fAppProductFamily := IECopy(v, 1, 32);
end;

{!!
<FS>TIETwainParams.AppProductName

<FM>Declaration<FC>
property AppProductName: AnsiString;

<FM>Description<FN>
These properties specify the application information communicated to the Twain scanner interface.
!!}
procedure TIETwainParams.SetAppProductName(v: AnsiString);
begin
  fAppProductName := IECopy(v, 1, 32);
end;

procedure TIETwainParams.SetLogFile(v: string);
begin
  if iegTwainLogName <> '' then
    CloseFile(iegTwainLogFile);
  iegTwainLogName := v;
  if v <> '' then
  begin
    AssignFile(iegTwainLogFile, iegTwainLogName);
    Rewrite(iegTwainLogFile);
    WriteLn(iegTwainLogFile, 'Twain log. Core version '+IEMAINVERSION+' '+IntToStr(IEMAINDATEDD)+' '+IntToStr(IEMAINDATEMM)+' '+IntToStr(IEMAINDATEYY));
  end;
end;

{!!
<FS>TIETwainParams.LogFile

<FM>Declaration<FC>
property LogFile: string;

<FM>Description<FN>
LogFile specifies the file name that will contain the log of the communication between ImageEn and the scanner driver.
Set this property before use scanner related methods or properties.

<FM>Example<FC>
ImageEnView.IO.TwainParams.LogFile := 'C:\twainlog.txt';

!!}
function TIETwainParams.GetLogFile: string;
begin
  result := iegTwainLogName;
end;



{$else} // IEINCLUDEIEXACQUIRE

interface

implementation


{$endif}  // IEINCLUDEIEXACQUIRE

end.
