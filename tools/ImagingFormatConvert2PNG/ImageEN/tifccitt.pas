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
File version 1008
*)

unit tifccitt;

{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, sysutils, classes, hyiedefs;

procedure IEInitialize_tifccitt();
procedure IEFinalize_tifccitt();

function CCITTHuffmanGetLine(dstbuf, srcbuf: pbyte; srcbufLen: integer; Width: integer; FillOrder: integer): integer;
function _CCITTHuffmanGetLine(dstbuf, srcbuf: pbyte; srcbufLen: integer; Width: integer; posb: integer; FillOrder: integer): integer;
function CCITT3_2D_GetLine(dstbuf, srcbuf: pbyte; srcbuflen: integer; Width: integer; predbuf: pbyte; posb: integer; FillOrder: integer; AlignEOL: boolean): integer;
procedure CCITTHuffmanPutLine(rdata: pbyte; wb: integer; fs: TStream; var Aborting: boolean; FillOrder: integer);
procedure CCITTHuffmanPutLineG3(rdata: pbyte; wb: integer; fs: TStream; var bwr: byte; var bwrl: integer; var Aborting: boolean; FillOrder: integer);
procedure CCITTHuffmanPutLineG32D(rdata: pbyte; wb: integer; fs: TStream; var bwr: byte; var bwrl: integer; var predline: pbyte; var Aborting: boolean; FillOrder: integer);
procedure _CCITTHuffmanPutLine(rdata: pbyte; wb: integer; wbuf: pbyte; var wpos: integer; FillOrder: integer);
procedure CCITTHuffmanPutLineG4(rdata: pbyte; wb: integer; fs: TStream; var bwr: byte; var bwrl: integer; var predline: pbyte; var Aborting: boolean; FillOrder: integer);

implementation

uses hyieutils, ImageEnProc;

{$R-}

type

  ppbyte = ^pbyte;

  TT4Entry = record
    dim:  integer;  // code bit count
    code: word;     // code (max 14 bit)
    rl:   integer;  // run length
  end;
  PT4Entry = ^TT4Entry;

const
  G3CODE_INVALID = -1;
  G3CODE_INCOMP  = -2;
  G3CODE_EOL     = -3;
  G3CODE_EOF     = -4;
  bitmask:      array [0..7] of byte = ($80, $40, $20, $10, $08, $04, $02, $01);
  bitclearmask: array [0..7] of byte = ($80, $C0, $E0, $F0, $F8, $FC, $FE, $FF);


const
  T4_2DC: array [0..12] of TT4Entry = (
    (dim: 12; code: 0),   // 000000000000 (12 zeri)   - 0:INVALID
    (dim: 4; code: 1),    // 0001		                  - 1:pass
    (dim: 3; code: 1),    // 001 		                  - 2:horizzontal
    (dim: 7; code: 2),    // 0000010 	                - 3:vertical Vl(3)
    (dim: 6; code: 2),    // 000010 	                - 4:vertical Vl(2)
    (dim: 3; code: 2),    // 010 		                  - 5:vertical Vl(1)
    (dim: 1; code: 1),    // 1 			                  - 6:vertical V(0)
    (dim: 3; code: 3),    // 011 		                  - 7:vertical Vr(1)
    (dim: 6; code: 3),    // 000011 	                - 8:vertical Vr(2)
    (dim: 7; code: 3),    // 0000011 	                - 9:vertical Vr(3)
    (dim: 10; code: 15),  // 0000001111               - 10:extension 2-D (enter in uncompressed mode)
    (dim: 12; code: 15),  // 000000001111             - 11:extension 1-D (enter in uncompressed mode)
    (dim: 12; code: 1)    // 0000 00000001            - 12:EOL
    );

type
  PT4Tree = ^TT4Tree;
  TT4Tree = record
    code: integer;
    childs: array [0..1] of PT4Tree; // if 0 and 1 is nil this is a terminal node
  end;

var
  T4Tree: array [0..1] of TT4Tree;

const
  NUMCODES = 116 + 12;
  T4Codes: array [0..1, 0..NUMCODES - 1] of TT4Entry = ((
    // WHITE CODES
    (dim: 8; code: $35; rl: 0),  // 0011 0101
    (dim: 6; code: $7; rl: 1),   // 0001 11
    (dim: 4; code: $7; rl: 2),   // 0111
    (dim: 4; code: $8; rl: 3),   // 1000
    (dim: 4; code: $B; rl: 4),   // 1011
    (dim: 4; code: $C; rl: 5),   // 1100
    (dim: 4; code: $E; rl: 6),   // 1110
    (dim: 4; code: $F; rl: 7),   // 1111
    (dim: 5; code: $13; rl: 8),  // 1001 1
    (dim: 5; code: $14; rl: 9),  // 1010 0
    (dim: 5; code: $7; rl: 10),  // 0011 1
    (dim: 5; code: $8; rl: 11),  // 0100 0
    (dim: 6; code: $8; rl: 12),  // 0010 00
    (dim: 6; code: $3; rl: 13),  // 0000 11
    (dim: 6; code: $34; rl: 14), // 1101 00
    (dim: 6; code: $35; rl: 15), // 1101 01
    (dim: 6; code: $2A; rl: 16), // 1010 10
    (dim: 6; code: $2B; rl: 17), // 1010 11
    (dim: 7; code: $27; rl: 18), // 0100 111
    (dim: 7; code: $C; rl: 19),  // 0001 100
    (dim: 7; code: $8; rl: 20),  // 0001 000
    (dim: 7; code: $17; rl: 21), // 0010 111
    (dim: 7; code: $3; rl: 22),  // 0000 011
    (dim: 7; code: $4; rl: 23),  // 0000 100
    (dim: 7; code: $28; rl: 24), // 0101 000
    (dim: 7; code: $2B; rl: 25), // 0101 011
    (dim: 7; code: $13; rl: 26), // 0010 011
    (dim: 7; code: $24; rl: 27), // 0100 100
    (dim: 7; code: $18; rl: 28), // 0011 000
    (dim: 8; code: $2; rl: 29),  // 0000 0010
    (dim: 8; code: $3; rl: 30),  // 0000 0011
    (dim: 8; code: $1A; rl: 31), // 0001 1010
    (dim: 8; code: $1B; rl: 32), // 0001 1011
    (dim: 8; code: $12; rl: 33), // 0001 0010
    (dim: 8; code: $13; rl: 34), // 0001 0011
    (dim: 8; code: $14; rl: 35), // 0001 0100
    (dim: 8; code: $15; rl: 36), // 0001 0101
    (dim: 8; code: $16; rl: 37), // 0001 0110
    (dim: 8; code: $17; rl: 38), // 0001 0111
    (dim: 8; code: $28; rl: 39), // 0010 1000
    (dim: 8; code: $29; rl: 40), // 0010 1001
    (dim: 8; code: $2A; rl: 41), // 0010 1010
    (dim: 8; code: $2B; rl: 42), // 0010 1011
    (dim: 8; code: $2C; rl: 43), // 0010 1100
    (dim: 8; code: $2D; rl: 44), // 0010 1101
    (dim: 8; code: $4; rl: 45),  // 0000 0100
    (dim: 8; code: $5; rl: 46),  // 0000 0101
    (dim: 8; code: $A; rl: 47),  // 0000 1010
    (dim: 8; code: $B; rl: 48),  // 0000 1011
    (dim: 8; code: $52; rl: 49), // 0101 0010
    (dim: 8; code: $53; rl: 50), // 0101 0011
    (dim: 8; code: $54; rl: 51), // 0101 0100
    (dim: 8; code: $55; rl: 52), // 0101 0101
    (dim: 8; code: $24; rl: 53), // 0010 0100
    (dim: 8; code: $25; rl: 54), // 0010 0101
    (dim: 8; code: $58; rl: 55), // 0101 1000
    (dim: 8; code: $59; rl: 56), // 0101 1001
    (dim: 8; code: $5A; rl: 57), // 0101 1010
    (dim: 8; code: $5B; rl: 58), // 0101 1011
    (dim: 8; code: $4A; rl: 59), // 0100 1010
    (dim: 8; code: $4B; rl: 60), // 0100 1011
    (dim: 8; code: $32; rl: 61), // 0011 0010
    (dim: 8; code: $33; rl: 62), // 0011 0011
    (dim: 8; code: $34; rl: 63), // 0011 0100
    (dim: 5; code: $1B; rl: 64), // 1101 1
    (dim: 5; code: $12; rl: 128), // 1001 0
    (dim: 6; code: $17; rl: 192), // 0101 11
    (dim: 7; code: $37; rl: 256), // 0110 111
    (dim: 8; code: $36; rl: 320), // 0011 0110
    (dim: 8; code: $37; rl: 384), // 0011 0111
    (dim: 8; code: $64; rl: 448), // 0110 0100
    (dim: 8; code: $65; rl: 512), // 0110 0101
    (dim: 8; code: $68; rl: 576), // 0110 1000
    (dim: 8; code: $67; rl: 640), // 0110 0111
    (dim: 9; code: $CC; rl: 704), // 0110 0110 0
    (dim: 9; code: $CD; rl: 768), // 0110 0110 1
    (dim: 9; code: $D2; rl: 832), // 0110 1001 0
    (dim: 9; code: $D3; rl: 896), // 0110 1001 1
    (dim: 9; code: $D4; rl: 960), // 0110 1010 0
    (dim: 9; code: $D5; rl: 1024), // 0110 1010 1
    (dim: 9; code: $D6; rl: 1088), // 0110 1011 0
    (dim: 9; code: $D7; rl: 1152), // 0110 1011 1
    (dim: 9; code: $D8; rl: 1216), // 0110 1100 0
    (dim: 9; code: $D9; rl: 1280), // 0110 1100 1
    (dim: 9; code: $DA; rl: 1344), // 0110 1101 0
    (dim: 9; code: $DB; rl: 1408), // 0110 1101 1
    (dim: 9; code: $98; rl: 1472), // 0100 1100 0
    (dim: 9; code: $99; rl: 1536), // 0100 1100 1
    (dim: 9; code: $9A; rl: 1600), // 0100 1101 0
    (dim: 6; code: $18; rl: 1664), // 0110 00
    (dim: 9; code: $9B; rl: 1728), // 0100 1101 1
    (dim: 11; code: $8; rl: 1792), // 0000 0001 000
    (dim: 11; code: $C; rl: 1856), // 0000 0001 100
    (dim: 11; code: $D; rl: 1920), // 0000 0001 101
    (dim: 12; code: $12; rl: 1984), // 0000 0001 0010
    (dim: 12; code: $13; rl: 2048), // 0000 0001 0011
    (dim: 12; code: $14; rl: 2112), // 0000 0001 0100
    (dim: 12; code: $15; rl: 2176), // 0000 0001 0101
    (dim: 12; code: $16; rl: 2240), // 0000 0001 0110
    (dim: 12; code: $17; rl: 2304), // 0000 0001 0111
    (dim: 12; code: $1C; rl: 2368), // 0000 0001 1100
    (dim: 12; code: $1D; rl: 2432), // 0000 0001 1101
    (dim: 12; code: $1E; rl: 2496), // 0000 0001 1110
    (dim: 12; code: $1F; rl: 2560), // 0000 0001 1111
    (dim: 12; code: $1; rl: G3CODE_EOL),     // 0000 0000 0001
    (dim: 9; code: $1; rl: G3CODE_INVALID),  // 0000 0000 1
    (dim: 10; code: $1; rl: G3CODE_INVALID), // 0000 0000 01
    (dim: 11; code: $1; rl: G3CODE_INVALID), // 0000 0000 001
    (dim: 13; code: $1; rl: G3CODE_EOL), // 0000000000001
    (dim: 14; code: $1; rl: G3CODE_EOL), // 00000000000001
    (dim: 15; code: $1; rl: G3CODE_EOL), // 000000000000001
    (dim: 16; code: $1; rl: G3CODE_EOL), // 0000000000000001
    (dim: 17; code: $1; rl: G3CODE_EOL), // 00000000000000001
    (dim: 18; code: $1; rl: G3CODE_EOL), // 000000000000000001
    (dim: 19; code: $1; rl: G3CODE_EOL), // 0000000000000000001
    (dim: 20; code: $1; rl: G3CODE_EOL), // 00000000000000000001
    //
    (dim: 21; code: $1; rl: G3CODE_EOL), // 000000000000000000001
    (dim: 22; code: $1; rl: G3CODE_EOL), // 0000000000000000000001
    (dim: 23; code: $1; rl: G3CODE_EOL), // 00000000000000000000001
    (dim: 24; code: $1; rl: G3CODE_EOL), // 000000000000000000000001
    (dim: 25; code: $1; rl: G3CODE_EOL), // 0000000000000000000000001
    (dim: 26; code: $1; rl: G3CODE_EOL), // 00000000000000000000000001
    (dim: 27; code: $1; rl: G3CODE_EOL), // 000000000000000000000000001
    (dim: 28; code: $1; rl: G3CODE_EOL), // 0000000000000000000000000001
    (dim: 29; code: $1; rl: G3CODE_EOL), // 00000000000000000000000000001
    (dim: 30; code: $1; rl: G3CODE_EOL), // 000000000000000000000000000001
    (dim: 31; code: $1; rl: G3CODE_EOL), // 0000000000000000000000000000001
    (dim: 32; code: $1; rl: G3CODE_EOL)  // 00000000000000000000000000000001
    ), (

    // BLACK CODES
    (dim: 10; code: $37; rl: 0), // 0000 1101 11
    (dim: 3; code: $2; rl: 1),   // 010
    (dim: 2; code: $3; rl: 2),   // 11
    (dim: 2; code: $2; rl: 3),   // 10
    (dim: 3; code: $3; rl: 4),   // 011
    (dim: 4; code: $3; rl: 5),   // 0011
    (dim: 4; code: $2; rl: 6),   // 0010
    (dim: 5; code: $3; rl: 7),   // 0001 1
    (dim: 6; code: $5; rl: 8),   // 0001 01
    (dim: 6; code: $4; rl: 9),   // 0001 00
    (dim: 7; code: $4; rl: 10),  // 0000 100
    (dim: 7; code: $5; rl: 11),  // 0000 101
    (dim: 7; code: $7; rl: 12),  // 0000 111
    (dim: 8; code: $4; rl: 13),  // 0000 0100
    (dim: 8; code: $7; rl: 14),  // 0000 0111
    (dim: 9; code: $18; rl: 15), // 0000 1100 0
    (dim: 10; code: $17; rl: 16), // 0000 0101 11
    (dim: 10; code: $18; rl: 17), // 0000 0110 00
    (dim: 10; code: $8; rl: 18),  // 0000 0010 00
    (dim: 11; code: $67; rl: 19), // 0000 1100 111
    (dim: 11; code: $68; rl: 20), // 0000 1101 000
    (dim: 11; code: $6C; rl: 21), // 0000 1101 100
    (dim: 11; code: $37; rl: 22), // 0000 0110 111
    (dim: 11; code: $28; rl: 23), // 0000 0101 000
    (dim: 11; code: $17; rl: 24), // 0000 0010 111
    (dim: 11; code: $18; rl: 25), // 0000 0011 000
    (dim: 12; code: $CA; rl: 26), // 0000 1100 1010
    (dim: 12; code: $CB; rl: 27), // 0000 1100 1011
    (dim: 12; code: $CC; rl: 28), // 0000 1100 1100
    (dim: 12; code: $CD; rl: 29), // 0000 1100 1101
    (dim: 12; code: $68; rl: 30), // 0000 0110 1000
    (dim: 12; code: $69; rl: 31), // 0000 0110 1001
    (dim: 12; code: $6A; rl: 32), // 0000 0110 1010
    (dim: 12; code: $6B; rl: 33), // 0000 0110 1011
    (dim: 12; code: $D2; rl: 34), // 0000 1101 0010
    (dim: 12; code: $D3; rl: 35), // 0000 1101 0011
    (dim: 12; code: $D4; rl: 36), // 0000 1101 0100
    (dim: 12; code: $D5; rl: 37), // 0000 1101 0101
    (dim: 12; code: $D6; rl: 38), // 0000 1101 0110
    (dim: 12; code: $D7; rl: 39), // 0000 1101 0111
    (dim: 12; code: $6C; rl: 40), // 0000 0110 1100
    (dim: 12; code: $6D; rl: 41), // 0000 0110 1101
    (dim: 12; code: $DA; rl: 42), // 0000 1101 1010
    (dim: 12; code: $DB; rl: 43), // 0000 1101 1011
    (dim: 12; code: $54; rl: 44), // 0000 0101 0100
    (dim: 12; code: $55; rl: 45), // 0000 0101 0101
    (dim: 12; code: $56; rl: 46), // 0000 0101 0110
    (dim: 12; code: $57; rl: 47), // 0000 0101 0111
    (dim: 12; code: $64; rl: 48), // 0000 0110 0100
    (dim: 12; code: $65; rl: 49), // 0000 0110 0101
    (dim: 12; code: $52; rl: 50), // 0000 0101 0010
    (dim: 12; code: $53; rl: 51), // 0000 0101 0011
    (dim: 12; code: $24; rl: 52), // 0000 0010 0100
    (dim: 12; code: $37; rl: 53), // 0000 0011 0111
    (dim: 12; code: $38; rl: 54), // 0000 0011 1000
    (dim: 12; code: $27; rl: 55), // 0000 0010 0111
    (dim: 12; code: $28; rl: 56), // 0000 0010 1000
    (dim: 12; code: $58; rl: 57), // 0000 0101 1000
    (dim: 12; code: $59; rl: 58), // 0000 0101 1001
    (dim: 12; code: $2B; rl: 59), // 0000 0010 1011
    (dim: 12; code: $2C; rl: 60), // 0000 0010 1100
    (dim: 12; code: $5A; rl: 61), // 0000 0101 1010
    (dim: 12; code: $66; rl: 62), // 0000 0110 0110
    (dim: 12; code: $67; rl: 63), // 0000 0110 0111
    (dim: 10; code: $F; rl: 64),  // 0000 0011 11
    (dim: 12; code: $C8; rl: 128), // 0000 1100 1000
    (dim: 12; code: $C9; rl: 192), // 0000 1100 1001
    (dim: 12; code: $5B; rl: 256), // 0000 0101 1011
    (dim: 12; code: $33; rl: 320), // 0000 0011 0011
    (dim: 12; code: $34; rl: 384), // 0000 0011 0100
    (dim: 12; code: $35; rl: 448), // 0000 0011 0101
    (dim: 13; code: $6C; rl: 512), // 0000 0011 0110 0
    (dim: 13; code: $6D; rl: 576), // 0000 0011 0110 1
    (dim: 13; code: $4A; rl: 640), // 0000 0010 0101 0
    (dim: 13; code: $4B; rl: 704), // 0000 0010 0101 1
    (dim: 13; code: $4C; rl: 768), // 0000 0010 0110 0
    (dim: 13; code: $4D; rl: 832), // 0000 0010 0110 1
    (dim: 13; code: $72; rl: 896), // 0000 0011 1001 0
    (dim: 13; code: $73; rl: 960), // 0000 0011 1001 1
    (dim: 13; code: $74; rl: 1024), // 0000 0011 1010 0
    (dim: 13; code: $75; rl: 1088), // 0000 0011 1010 1
    (dim: 13; code: $76; rl: 1152), // 0000 0011 1011 0
    (dim: 13; code: $77; rl: 1216), // 0000 0011 1011 1
    (dim: 13; code: $52; rl: 1280), // 0000 0010 1001 0
    (dim: 13; code: $53; rl: 1344), // 0000 0010 1001 1
    (dim: 13; code: $54; rl: 1408), // 0000 0010 1010 0
    (dim: 13; code: $55; rl: 1472), // 0000 0010 1010 1
    (dim: 13; code: $5A; rl: 1536), // 0000 0010 1101 0
    (dim: 13; code: $5B; rl: 1600), // 0000 0010 1101 1
    (dim: 13; code: $64; rl: 1664), // 0000 0011 0010 0
    (dim: 13; code: $65; rl: 1728), // 0000 0011 0010 1
    (dim: 11; code: $8; rl: 1792),  // 0000 0001 000
    (dim: 11; code: $C; rl: 1856),  // 0000 0001 100
    (dim: 11; code: $D; rl: 1920),  // 0000 0001 101
    (dim: 12; code: $12; rl: 1984), // 0000 0001 0010
    (dim: 12; code: $13; rl: 2048), // 0000 0001 0011
    (dim: 12; code: $14; rl: 2112), // 0000 0001 0100
    (dim: 12; code: $15; rl: 2176), // 0000 0001 0101
    (dim: 12; code: $16; rl: 2240), // 0000 0001 0110
    (dim: 12; code: $17; rl: 2304), // 0000 0001 0111
    (dim: 12; code: $1C; rl: 2368), // 0000 0001 1100
    (dim: 12; code: $1D; rl: 2432), // 0000 0001 1101
    (dim: 12; code: $1E; rl: 2496), // 0000 0001 1110
    (dim: 12; code: $1F; rl: 2560), // 0000 0001 1111
    (dim: 12; code: $1; rl: G3CODE_EOL),     // 0000 0000 0001
    (dim: 9; code: $1; rl: G3CODE_INVALID),  // 0000 0000 1
    (dim: 10; code: $1; rl: G3CODE_INVALID), // 0000 0000 01
    (dim: 11; code: $1; rl: G3CODE_INVALID), // 0000 0000 001
    (dim: 13; code: $1; rl: G3CODE_EOL), // 0000000000001
    (dim: 14; code: $1; rl: G3CODE_EOL), // 00000000000001
    (dim: 15; code: $1; rl: G3CODE_EOL), // 000000000000001
    (dim: 16; code: $1; rl: G3CODE_EOL), // 0000000000000001
    (dim: 17; code: $1; rl: G3CODE_EOL), // 00000000000000001
    (dim: 18; code: $1; rl: G3CODE_EOL), // 000000000000000001
    (dim: 19; code: $1; rl: G3CODE_EOL), // 0000000000000000001
    (dim: 20; code: $1; rl: G3CODE_EOL), // 00000000000000000001

    (dim: 21; code: $1; rl: G3CODE_EOL), // 000000000000000000001
    (dim: 22; code: $1; rl: G3CODE_EOL), // 0000000000000000000001
    (dim: 23; code: $1; rl: G3CODE_EOL), // 00000000000000000000001
    (dim: 24; code: $1; rl: G3CODE_EOL), // 000000000000000000000001
    (dim: 25; code: $1; rl: G3CODE_EOL), // 0000000000000000000000001
    (dim: 26; code: $1; rl: G3CODE_EOL), // 00000000000000000000000001
    (dim: 27; code: $1; rl: G3CODE_EOL), // 000000000000000000000000001
    (dim: 28; code: $1; rl: G3CODE_EOL), // 0000000000000000000000000001
    (dim: 29; code: $1; rl: G3CODE_EOL), // 00000000000000000000000000001
    (dim: 30; code: $1; rl: G3CODE_EOL), // 000000000000000000000000000001
    (dim: 31; code: $1; rl: G3CODE_EOL), // 0000000000000000000000000000001
    (dim: 32; code: $1; rl: G3CODE_EOL) // 00000000000000000000000000000001
    ));

  horizcode: TT4Entry = (dim: 3; code: $1); // 001
  passcode:  TT4Entry = (dim: 4; code: $1); // 0001
  vcodes: array [0..6] of TT4Entry = ((dim: 7; code: $3), // 0000 011
    (dim: 6; code: $3),  // 0000 11
    (dim: 3; code: $3),  // 011
    (dim: 1; code: $1),  // 1
    (dim: 3; code: $2),  // 010
    (dim: 6; code: $2),  // 0000 10
    (dim: 7; code: $2)); // 0000 010


function AdjustWithFillOrder(dwo: dword; posb: integer; FillOrder: integer): dword;
begin
  if FillOrder = 1 then
  begin
    // swap "dwo" dword
    {$ifdef IEUSEASM}
    asm
      mov EAX,dwo
      bswap EAX
      mov dwo,EAX
    end;
    {$else}
    dwo := IESwapDWord(dwo);
    {$endif}
    dwo := (dwo shl (posb and 7));
  end
  else
  begin
    // FillOrder=2
    dwo := (dwo shr (posb and 7));
    ReverseBits(dwo);
  end;
  result := dwo;
end;


// find code indexed by posb
// srcbuf: pointer to initial bits
// posb: bit to read from srcbuf (start of code)
// WB: 0=use white code     1=use black code
function FindCode(srcbuf: pbyte; srcbufLen: integer; posb, WB: integer; FillOrder: integer): integer;
var
  ibi: integer;
  dwo: dword;
  q: integer;
  curtree: PT4Tree;
begin
  result := NUMCODES; // invalid code
  ibi := posb shr 3;  // divide by 8 (calculates related byte)
  if ibi < srcbufLen then
  begin
    dwo := pinteger(@pbytearray(srcbuf)^[ibi])^;
    dwo := AdjustWithFillOrder(dwo, posb, FillOrder);
    // find code
    curtree := @(T4Tree[WB]);
    q := 31;
    repeat
      if curtree^.code <> NUMCODES then
      begin
        result := curtree^.code;
        break;
      end;
      curtree := curtree^.childs[ord((dwo and (1 shl q)) <> 0)];
      dec(q);
    until q < 0;
  end
  else
    result := NUMCODES - 1; // wrong position, send EOL
end;


// find MODE that matches to the one pointed by posb
// srcbuf: pointer to start of bits
// posb: bit inside srcbuf to read (start of code)
function FindMode(srcbuf: pbyte; posb: integer; FillOrder: integer): integer;
var
  ibi: integer;
  dwo, tdwo: dword;
  q: integer;
begin
  result := 0; // invalid code
  ibi := posb shr 3; // divide vt 8 (find matching byte)
  dwo := pinteger(@pbytearray(srcbuf)^[ibi])^;
  dwo := AdjustWithFillOrder(dwo, posb, FillOrder);
  // find mode
  for q := 0 to 12 do
  begin
    tdwo := dwo shr (32 - T4_2DC[q].dim);
    if tdwo = T4_2DC[q].code then
    begin
      result := q;
      break;
    end;
  end;
end;


// returns word composed by the number of specified bits (max 32)
function getpels(srcbuf: pbyte; posb: integer; nbit: integer; FillOrder: integer): integer;
var
  ibi: integer;
  dwo: dword;
begin
  ibi := posb shr 3; // divide by 8 (find matching byte)
  dwo := pinteger(@pbytearray(srcbuf)^[ibi])^;
  dwo := AdjustWithFillOrder(dwo, posb, FillOrder);

  result := dwo shr (32 - nbit);
end;


// decode uncompressed code from srcbuf, at posb (bits) position
// length of return code corresponds to the value of code self
function decode_uncomp_code(srcbuf: pbyte; posb: integer; FillOrder: integer): integer;
var
  ibi: integer;
  dwo, tdwo: dword;
  q: integer;
begin
  result := 0;       // invalid code (valid codes start from 1..)
  ibi := posb shr 3; // divide by 8 (find matching code)
  dwo := pinteger(@pbytearray(srcbuf)^[ibi])^;
  dwo := AdjustWithFillOrder(dwo, posb, FillOrder);
  for q := 1 to 11 do
  begin
    tdwo := dwo shr (32 - q);
    if tdwo = 1 then
    begin
      result := q;
      break;
    end;
  end;
end;


// replicates "1"(0) for "rl" times
// destbuf = destination buffer
// posw = bit inside dstbuf (writing begin)
// rl = bits to replicate
// WB = bit to replicate inverted (0 or 1)
// Width = row length
procedure PutCode(dstbuf: pbyte; posw: integer; rl: integer; Width: integer);
var
  q: integer;
  ibi, abi: integer;
begin
  rl := imin(Width - posw, rl);
  q := 0;
  while q < rl do
  begin
    ibi := posw shr 3;
    abi := (rl - q) and $FFF8;
    if ((posw and $7) = 0) and (abi > 0) then
    begin
      fillchar(pbyte(uint64(dstbuf) + ibi)^, (rl - q) shr 3, 0);
      inc(posw, abi);
      inc(q, abi);
    end
    else
    begin
      pbytearray(dstbuf)^[ibi] := pbytearray(dstbuf)^[ibi] and (not bitmask[posw and 7]);
      inc(posw);
      inc(q);
    end;
  end;
end;


// decompress HUFFMAN row
// dstbuf = destination buffer (must be already allocated)
// srcbuf = source buffer
// Width = row size in pixels
// posb = initial bit to read from srcbuf
// ret. next bit to read (posb)
function _CCITTHuffmanGetLine(dstbuf, srcbuf: pbyte; srcbufLen: integer; Width: integer; posb: integer; FillOrder: integer): integer;
var
  c: integer;
  WB: integer;    // 0=white  1=black
  posw: integer;  // next bit to write
  lposb: integer; // original value of posb
  tc: boolean;    // if True terminating-code is missing
begin
  if (Width and $7) <> 0 then
    c := 1
  else
    c := 0;
  fillmemory(dstbuf, (Width shr 3) + c, 255);
  WB := 0; // start with WHITE
  posw := 0;
  lposb := posb; // save posb
  tc := false;
  while (posw < Width) or tc do
  begin
    c := FindCode(srcbuf, srcbufLen, posb, WB, FillOrder);
    if c = NUMCODES then
    begin
      inc(posb);
      inc(lposb);
    end
    else
    begin
      with T4Codes[WB][C] do
      begin
        inc(posb, dim);
        if (rl = G3CODE_EOL) or (rl = G3CODE_EOF) then
        begin
          if (posb - dim) = lposb then
          begin
            continue; // EOL at the beginning of the row, ignore it!
          end;
          // terminate the row
          if WB <> 0 then
            PutCode(dstbuf, posw, Width - posw + 1, Width);
          inc(posw, Width - posw + 1);
          c := 0; // makes true c < 64 (and tc = false), then exit loop
        end
        else
        if rl <> G3CODE_INVALID then
        begin
          if WB <> 0 then
            PutCode(dstbuf, posw, rl, Width);
          inc(posw, rl);
        end;
      end;
      if c < 64 then
      begin
        tc := false;
        if WB = 0 then
          WB := 1
        else
          WB := 0;
      end
      else
        tc := true;
    end;
  end;
  result := posb;
end;


// decompress HUFFMAN row
// dstbuf = destination buffer (must be already allocated)
// srcbuf = source buffer
// Width = row size in pixels
// ret. byte read from srcbuf
// note: the difference between G3FAX1D and HUFFMAN is that in HUFFMAN each row has the size rounded to a Byte
function CCITTHuffmanGetLine(dstbuf, srcbuf: pbyte; srcbufLen: integer; Width: integer; FillOrder: integer): integer;
var
  posb: integer;
begin
  posb := _CCITTHuffmanGetLine(dstbuf, srcbuf, srcbufLen, Width, 0, FillOrder);
  result := posb shr 3; // posb div 8
  if (posb mod 8) > 0 then
    inc(result);
end;


// search for first pixel not equal to CL (0=black <>0=white)
// consider inverted bits inside buf (0=white 1=black)
// ipos = starting position (bits)
// Width = row size in pixels
// ret. pixel position (starting from 0)
function finddiff(buf: pbyte; ipos: integer; Width: integer; CL: integer): integer;
var
  ibi, ibb: integer;
  by: integer;
  db: pbyte;
begin
  while ipos < Width do
  begin
    // calculate bit "ipos"
    ibi := ipos shr 3; // extract byte position
    ibb := ipos and 7; // extract bit position

    if ibb = 0 then
    begin
      db := buf;
      inc(db, ibi);
      if CL = 0 then
      begin
        while (ipos + 32 < Width) and (pdword(db)^ = $FFFFFFFF) do
        begin
          inc(ipos, 32);
          inc(db, 4);
        end;
        while (ipos + 16 < Width) and (pword(db)^ = $FFFF) do
        begin
          inc(ipos, 16);
          inc(db, 2);
        end;
        while (ipos + 8 < Width) and (db^ = $FF) do
        begin
          inc(ipos, 8);
          inc(db);
        end;
      end
      else
      begin
        while (ipos + 32 < Width) and (pdword(db)^ = 0) do
        begin
          inc(ipos, 32);
          inc(db, 4);
        end;
        while (ipos + 16 < Width) and (pword(db)^ = 0) do
        begin
          inc(ipos, 16);
          inc(db, 2);
        end;
        while (ipos + 8 < Width) and (db^ = 0) do
        begin
          inc(ipos, 8);
          inc(db);
        end;
      end;
      if ipos >= Width then
      begin
        ipos := Width;
        break;
      end;
      ibb := 0;
      while ipos < Width do
      begin
        by := db^ and (1 shl (7 - ibb));
        if (cl = by) or ((cl <> 0) and (by <> 0)) then
          break;
        inc(ibb);
        inc(ipos);
      end;
      break;
    end;

    if ipos >= 0 then
      by := pbytearray(buf)^[ibi] and (1 shl (7 - ibb))
    else
      by := 1;
    if (cl = by) or ((cl <> 0) and (by <> 0)) then
      break;
    inc(ipos);
  end;
  result := ipos;
end;


// decompress CCITT 3 - 2D row
// predbuf: previous row (handled automatically)
// posb = initial bit to read from srcbuf
// ret. next bit to read (posb)
// note: dstbuf must be Zero initialized
function CCITT3_2D_GetLine(dstbuf, srcbuf: pbyte; srcbuflen: integer; Width: integer; predbuf: pbyte; posb: integer; FillOrder: integer; AlignEOL: boolean): integer;
var
  c, v, q: integer;
  WB: integer;         // 0 = white  1 = black
  CL: integer;         // current color (0 = black 1 = white)
  lposb: integer;      // original value of posb
  a0, b1, b2: integer; // a0 = was posw
  run1, run2: integer;
  maxlen: integer;
  dim: integer;
begin
  if (Width and $7) <> 0 then
    dim := 1
  else
    dim := 0;
  fillmemory(dstbuf, (Width shr 3) + dim, 255);
  a0 := -1;
  CL := 1;        // start with WHITE
  lposb := posb;  // save posb
  maxlen := srcbuflen shl 3;
  while (a0 < Width) and (posb < maxlen) do
  begin
    c := FindMode(srcbuf, posb, FillOrder);
    dim := T4_2DC[c].dim;
    inc(posb, dim);
    if AlignEOL and (c = 0) then
    begin
      repeat
        v := getpels(srcbuf, posb, 1, FillOrder);
        inc(posb);
        inc(dim);
      until v <> 0;
      c := 12;
    end;
    case c of
      12: // EOF
        begin
          if lposb <> (posb - dim) then
          begin
            // premature row end
            if (CL = 0) and (a0 > -1) then
              PutCode(dstbuf, a0, Width - a0, Width);
            dec(posb, dim);
            a0 := Width;
          end
          else
          begin
            v := getpels(srcbuf, posb, 1, FillOrder);
            inc(posb); // bypass next bit
            if v = 1 then
            begin      // 1D decode
              if a0 < 0 then
                a0 := 0;
              posb := _CCITTHuffmanGetLine(dstbuf, srcbuf, srcbufLen, Width, posb, FillOrder);
              inc(a0, Width);
            end;
          end;
        end;
      1: // PASS
        begin
          b2 := finddiff(predbuf, a0, Width, (CL));
          b1 := finddiff(predbuf, b2, Width, (not CL) and $1);
          b2 := finddiff(predbuf, b1, Width, (CL));
          if a0 < 0 then
            a0 := 0;
          if CL = 0 then
            PutCode(dstbuf, a0, b2 - a0, Width);
          a0 := b2;
        end;
      2: // HORIZONTAL
        begin
          // run1
          WB := (not CL) and $1;
          run1 := 0;
          repeat
            v := FindCode(srcbuf, srcbufLen, posb, WB, FillOrder);
            inc(posb, T4Codes[WB][v].dim);
            inc(run1, T4Codes[WB][v].rl);
          until v < 64;
          // run2
          WB := CL;
          run2 := 0;
          repeat
            v := FindCode(srcbuf, srcbufLen, posb, WB, FillOrder);
            inc(posb, T4Codes[WB][v].dim);
            inc(run2, T4Codes[WB][v].rl);
          until v < 64;
          if (run1 >= 0) and (run2 >= 0) then
          begin
            if a0 < 0 then
              a0 := 0;
            if a0 + run1 > Width then
              run1 := Width - a0;
            if CL = 0 then
              PutCode(dstbuf, a0, run1, Width);
            inc(a0, run1);
            if a0 + run2 > Width then
              run2 := Width - a0;
            if CL <> 0 then
              PutCode(dstbuf, a0, run2, Width);
            inc(a0, run2);
          end;
        end;
      3..9: // VERTICAL
        begin
          b2 := finddiff(predbuf, a0, Width, (CL));
          b1 := finddiff(predbuf, b2, Width, (not CL) and $1);
          inc(b1, c - 6);
          if a0 < 0 then
            a0 := 0;
          if CL = 0 then
          begin
            PutCode(dstbuf, a0, b1 - a0, Width);
            CL := 1;
          end
          else
            CL := 0;
          a0 := b1;
        end;
      10..11: // Not compressed
        begin
          if a0 < 0 then
            a0 := 0;
          repeat
            v := decode_uncomp_code(srcbuf, posb, FillOrder);
            if v = 0 then
              break;
            inc(posb, v);
            case v of
              1..5:
                begin
                  run1 := v;
                  inc(a0, run1);
                end;
              6:
                begin
                  inc(a0, 5);
                end;
              7..11:
                begin
                  run1 := v - 7;
                  inc(a0, run1);
                  CL := getpels(srcbuf, posb, 1, FillOrder);
                  inc(posb);
                end;
            end;
          until (v > 6) or (posb >= maxlen) or (a0 >= Width);
        end;
    end;
  end; // loop w..Width-1
  result := posb;

  q := Width shr 3;
  if (Width and 7) > 0 then
    inc(q);
  CopyMemory(predbuf, dstbuf, q); // copy new row into previous one
end;


// Ret. rposb bit inside rdata buffer
// 0 = false (black) 1 = true (white)
function _GetRowBit(rdata: pbyte; rposb: integer): boolean;
begin
  inc(rdata, rposb shr 3);
  result := (rdata^ and bitmask[rposb and 7]) = 0;
end;


// Write "bl" bits of wo, into position wposb of wbuf
// "wposb" is increased by "bl"
procedure _PutBits(wbuf: pbyte; var wposb: integer; bl: integer; wo: integer; FillOrder: integer);
var
  q, b, r: integer;
  s, bp: pbyte;
begin
  if FillOrder = 2 then
  begin
    q := 0; // bit counter
    s := @wo;
    while q < bl do
    begin
      // get source
      b := bl - q - 1; // bit position in source
      r := pbytearray(s)^[b shr 3] and (1 shl (b and 7));
      // set dest
      bp := pbyte(uint64(wbuf) + (uint64(wposb) shr 3));
      if r <> 0 then
        bp^ := bp^ or (1 shl (wposb and 7))       // set 1
      else
        bp^ := bp^ and not (1 shl (wposb and 7)); // set 0
      inc(wposb);
      inc(q);
    end;
  end
  else
  begin
    if bl > 8 then
    begin
      bswap(pbytearray(@wo)^[0], pbytearray(@wo)^[1]);
      IECopyBits_small(wbuf, pbyte(@wo), wposb, (16 - bl) mod 8, bl, 2147483647);
    end
    else
      IECopyBits_small(wbuf, pbyte(@wo), wposb, 8 - bl, bl, 2147483647);
    inc(wposb, bl);
  end;
end;


// Write Huffman code "rt" of "bw" type (false = black, true = white) into wbuf buffer at wposb position.
// At the end update wposb to point to next bit to write.
// rt can be any value (even greater than Huffman "rl" codes).
procedure _PutRLCode(wbuf: pbyte; var wposb: integer; rt: integer; bwb: boolean; FillOrder: integer);
var
  bw: integer;
  q: integer;
begin
  bw := ord(not bwb); // 1 = false (black)  0 = true (white)
  // write Make-up code
  while rt > 63 do
  begin
    if rt > 2560 then
    begin
      _PutBits(wbuf, wposb, T4Codes[bw, 103].dim, T4Codes[bw, 103].code, FillOrder);
      dec(rt, 2560);
    end
    else
    begin
      q := 63 + (rt shr 6);
      _PutBits(wbuf, wposb, T4Codes[bw, q].dim, T4Codes[bw, q].code, FillOrder);
      dec(rt, T4Codes[bw, q].rl);
    end;
  end;
  // write term code
  _PutBits(wbuf, wposb, T4Codes[bw, rt].dim, T4Codes[bw, rt].code, FillOrder);
end;


// Compress rdata (of wb bits) and save to "fs"
// used for CCITT 1D (Huffman)
procedure CCITTHuffmanPutLine(rdata: pbyte; wb: integer; fs: TStream; var Aborting: boolean; FillOrder: integer);
var
  bwr: byte;
  bwrl: integer;
begin
  bwrl := 0;
  CCITTHuffmanPutLineG3(rdata, wb, fs, bwr, bwrl, Aborting, FillOrder);
  CCITTHuffmanPutLineG3(nil, 0, fs, bwr, bwrl, Aborting, FillOrder); // finalize row (write remain byte)
end;


// Compress rdata (of wb bits) and save it into "fs"
// bwr: remaining byte to write
// bwrl: remainging bits in bwr to write
// Used by G3FAX1D
procedure CCITTHuffmanPutLineG3(rdata: pbyte; wb: integer; fs: TStream; var bwr: byte; var bwrl: integer; var Aborting: boolean; FillOrder: integer);
var
  wposb: integer; // current writing position (in bits)
  rt: integer;
  wbuf: pbyte;    // writing buffer
begin
  getmem(wbuf, imax(4, (wb shr 3) * 12 + 1)); // 12 times one row (+1 of eventually 1bwr)
  try
    wposb := 0;
    if bwrl > 0 then
    begin
      // there are bits to write since last call
      wbuf^ := bwr;
      wposb := bwrl;
    end;
    _CCITTHuffmanPutLine(rdata, wb, wbuf, wposb, FillOrder);
    // write buffer
    rt := wposb shr 3;
    if (wposb and 7) <> 0 then
    begin
      bwrl := wposb - rt * 8;           // bits to write at the next call
      bwr := pbyte(uint64(wbuf) + rt)^; // byte to write at the next call
    end
    else
      bwrl := 0;
    SafeStreamWrite(fs, Aborting, wbuf^, rt);
  finally
    freemem(wbuf);
  end;

  if (wb = 0) and (bwrl > 0) then
  begin
    // finalize, write remain byte
    bwr := bwr and (bitclearmask[bwrl - 1]);
    if FillOrder = 2 then
      ReverseBitsB(bwr);
    SafeStreamWrite(fs, Aborting, bwr, 1);
    bwrl := 0;
  end;
end;


// Compress rdata (of wb bits size) in wbuf
// move wpos (input / output)
procedure _CCITTHuffmanPutLine(rdata: pbyte; wb: integer; wbuf: pbyte; var wpos: integer; FillOrder: integer);
var
  rposb: integer; // current reading position (in bits)
  rt: integer;
  bw: boolean;    // true = white  false = black : next color to process
begin
  bw := true;     // starting with White
  rposb := 0;
  while rposb < wb do
  begin
    rt := 0;
    while (rposb < wb) and (_GetRowBit(rdata, rposb) = bw) do
    begin
      inc(rt);
      inc(rposb);
    end;
    _PutRLCode(wbuf, wpos, rt, bw, FillOrder); // modifies wposb
    bw := not bw;
  end;
end;


function _PIXEL(buf: pbytearray; ix: integer): integer;
begin
  result := ((buf[ix shr 3]) shr (7 - (ix and 7))) and 1;
end;


// bp: row buffer to compress (input)
// rp: previous row buffer (input)
// wbuf: compressed row buffer (output)
// wpos: position of the next bit to write
// bits: "bp" row length (in bits)
// note, bwr and bwrl ares inputs only
// ret: position of the current bit to write (= written bits)
procedure Fax3Encode2DRow(bp: pbytearray; rp: pbytearray; wbuf: pbyte; var wpos: integer; bits: integer; FillOrder: integer);
var
  white: integer;
  a0: integer;
  a1: integer;
  b1: integer;
  a2, b2: integer;
  d, r: integer;
begin
  white := 0;
  a0 := 0;

  if _PIXEL(bp, 0) <> white then
    a1 := 0
  else
    a1 := finddiff(pbyte(bp), 0, bits, (not white) and $1);

  if _PIXEL(rp, 0) <> white then
    b1 := 0
  else
    b1 := finddiff(pbyte(rp), 0, bits, (not white) and $1);

  repeat
    if b1 < bits then
      r := ((rp[b1 shr 3]) shr (7 - (b1 and 7))) and 1
    else
      r := 0;
    b2 := finddiff(pbyte(rp), b1, bits, (not r) and $1);
    if (b2 >= a1) then
    begin
      d := b1 - a1;
      if not ((-3 <= d) and (d <= 3)) then
      begin // horizontal mode
        a2 := finddiff(pbyte(bp), a1, bits, (not _PIXEL(bp, a1)) and $1);
        _PutBits(wbuf, wpos, horizcode.dim, horizcode.code, FillOrder);
        if (a0 + a1 = 0) or (_PIXEL(bp, a0) = white) then
        begin
          _PutRLCode(wbuf, wpos, a1 - a0, true, FillOrder);
          _PutRLCode(wbuf, wpos, a2 - a1, false, FillOrder);
        end
        else
        begin
          _PutRLCode(wbuf, wpos, a1 - a0, false, FillOrder);
          _PutRLCode(wbuf, wpos, a2 - a1, true, FillOrder);
        end;
        a0 := a2;
      end
      else
      begin // vertical mode
        _PutBits(wbuf, wpos, vcodes[d + 3].dim, vcodes[d + 3].code, FillOrder);
        a0 := a1;
      end;
    end
    else
    begin // pass mode
      _PutBits(wbuf, wpos, passcode.dim, passcode.code, FillOrder);
      a0 := b2;
    end;
    if (a0 >= bits) then
      break;
    a1 := finddiff(pbyte(bp), a0, bits, (not _PIXEL(bp, a0)) and $1);
    b1 := finddiff(pbyte(rp), a0, bits, _PIXEL(bp, a0));
    b1 := finddiff(pbyte(rp), b1, bits, (not _PIXEL(bp, a0)) and $1);
  until false;
end;


// Compress rdata (of wb bits) writing to fs
// bwr: remaining byte to write
// bwrl: remaining bits in bwr to write
// predline: pointer to the previous row. Allocated here. This is "nil" for the first row. If rdata = nil where are "finalizing" (then free predline).
// used for G3FAX2D
procedure CCITTHuffmanPutLineG32D(rdata: pbyte; wb: integer; fs: TStream; var bwr: byte; var bwrl: integer; var predline: pbyte; var Aborting: boolean; FillOrder: integer);
var
  wbuf: pbyte;
  wpos, rt, rowlen: integer;
begin
  getmem(wbuf, imax(4, (wb shr 3) * 12 + 1) ); // 12 times one row (+1 of eventually bwr)

  try
    wpos := 0;
    if bwrl > 0 then
    begin
      // there are bits to write since last call
      wbuf^ := bwr;
      wpos := bwrl;
      bwrl := 0;
    end;
    rowlen := (((wb + 31) shr 5) shl 2);
    if predline = nil then
    begin
      // First row
      // send EOL + 1 (first row 1D)
      _PutBits(wbuf, wpos, 12, $1, FillOrder); // write EOF
      _PutBits(wbuf, wpos, 1, $1, FillOrder);  // mark code 1D
      _CCITTHuffmanPutLine(rdata, wb, wbuf, wpos, FillOrder); // coding as 1D
      getmem(predline, rowlen);
      copymemory(predline, rdata, rowlen);
    end
    else
    if rdata <> nil then
    begin
      // Other rows
      // send EOL + 0
      _PutBits(wbuf, wpos, 12, $1, FillOrder); // write EOF
      _PutBits(wbuf, wpos, 1, $0, FillOrder);  // mark code 2D
      Fax3Encode2DRow(pbytearray(rdata), pbytearray(predline), wbuf, wpos, wb, FillOrder);
      copymemory(predline, rdata, rowlen);
    end;
    if rdata = nil then
    begin
      // finalization
      _PutBits(wbuf, wpos, 12, $1, FillOrder); // write EOF
      freemem(predline);
      predline := nil;
    end;
    // write buffer
    rt := wpos shr 3;
    if (wpos and 7) <> 0 then
    begin
      bwrl := wpos - rt * 8;            // number of bits to write at the next call
      bwr := pbyte(uint64(wbuf) + rt)^; // bits to write at the next call
    end;
    SafeStreamWrite(fs, Aborting, wbuf^, rt);

  finally
    freemem(wbuf);
  end;
end;


// Compress rdata (of wb bits) and write into "fs"
// bwr: remaining byte to write
// bwrl: number of remaining bits in bwr to write
// predline: pointer to the previous row. Allocated here. It is nil for the first row. If rdata  =nil where are finalizing (then free predline).
// used for G4FAX
procedure CCITTHuffmanPutLineG4(rdata: pbyte; wb: integer; fs: TStream; var bwr: byte; var bwrl: integer; var predline: pbyte; var Aborting: boolean; FillOrder: integer);
var
  wbuf: pbyte;
  wpos, rt, rowlen: integer;
begin
  getmem(wbuf, imax(4, (wb shr 3) * 12 + 1) ); // 12 times one row (+1 of eventually bwr)

  try
    wpos := 0;
    if bwrl > 0 then
    begin
      // there are bits to write since last call
      wbuf^ := bwr;
      wpos := bwrl;
      bwrl := 0;
    end;
    rowlen := (((wb + 31) shr 5) shl 2);
    if predline = nil then
    begin
      // Prepare for the first row
      getmem(predline, rowlen);
      fillchar(predline^, rowlen, 0); // all white
    end;
    if rdata <> nil then
    begin
      // Other rows
      Fax3Encode2DRow(pbytearray(rdata), pbytearray(predline), wbuf, wpos, wb, FillOrder);
      copymemory(predline, rdata, rowlen);
    end;
    if rdata = nil then
    begin
      // to follow the Standard write BEOF and round to 8 bits
      _PutBits(wbuf, wpos, 12, $1, FillOrder);
      _PutBits(wbuf, wpos, 12, $1, FillOrder);
      if (wpos and 7) <> 0 then
        _PutBits(wbuf, wpos, 8 - (wpos and 7), $0, FillOrder);
      freemem(predline);
      predline := nil;
    end;
    // write buffer
    rt := wpos shr 3;
    if (wpos and 7) <> 0 then
    begin
      bwrl := wpos - rt * 8;            // number of bits to write at the next call
      bwr := pbyte(uint64(wbuf) + rt)^; // bits to write at the next call
    end;
    SafeStreamWrite(fs, Aborting, wbuf^, rt);

  finally
    freemem(wbuf);
  end;
end;



//////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////
// T4 tree builder

procedure AddT4Child(var baseitem: TT4Tree; const code: TT4Entry; codenum: integer);
var
  bb: integer;
  q: integer;
  curitem: PT4Tree;
begin
  curitem := @baseitem;
  for q := code.dim - 1 downto 0 do
  begin
    bb := ord((code.code and (1 shl q)) <> 0); // extract next bit of the code
    if curitem^.childs[bb] = nil then
    begin
      new(curitem^.childs[bb]);
      curitem^.code := NUMCODES;
      curitem^.childs[bb]^.childs[0] := nil;
      curitem^.childs[bb]^.childs[1] := nil;
    end;
    curitem := curitem^.childs[bb];
  end;
  curitem^.code := codenum;
end;


procedure InitT4Tree();
var
  q, w: integer;
begin
  for w := 0 to 1 do
  begin
    T4Tree[w].childs[0] := nil;
    T4Tree[w].childs[1] := nil;
    for q := 0 to NUMCODES - 1 do
      AddT4Child(T4Tree[w], T4Codes[w, q], q);
  end;
end;

procedure FreeT4Tree(item: PT4Tree);
begin
  if item <> nil then
  begin
    FreeT4Tree(item^.childs[0]);
    FreeT4Tree(item^.childs[1]);
    dispose(item);
  end;
end;

// end of T4 tree builder
//////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////


procedure IEInitialize_tifccitt;
begin
  InitT4Tree;
end;

procedure IEFinalize_tifccitt;
begin
  FreeT4Tree(T4Tree[0].childs[0]);
  FreeT4Tree(T4Tree[0].childs[1]);
  FreeT4Tree(T4Tree[1].childs[0]);
  FreeT4Tree(T4Tree[1].childs[1]);
end;


end.
