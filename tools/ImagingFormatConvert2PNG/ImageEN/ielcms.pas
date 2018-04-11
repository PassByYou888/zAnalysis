//
//  Little cms
//  Copyright (C) 1998-2004 Marti Maria
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Version 1.13

(*
File version 1001
*)


unit ielcms;

{$I ie.inc}

interface

{$IFDEF IEINCLUDECMS}

uses Windows, Classes, SysUtils, hyieutils, imageenproc, hyiedefs;

type

  LCMSHANDLE = pointer;
  cmsHPROFILE = LCMSHANDLE;
  cmsHTRANSFORM = LCMSHANDLE;
  PcmsHTRANSFORM = ^cmsHTRANSFORM;

const
  // Pixel types

  PT_ANY = 0;
  PT_GRAY = 3;
  PT_RGB = 4;
  PT_CMY = 5;
  PT_CMYK = 6;
  PT_YCbCr = 7;
  PT_YUV = 8; // Lu'v'
  PT_XYZ = 9;
  PT_Lab = 10;
  PT_YUVK = 11; // Lu'v'K
  PT_HSV = 12;
  PT_HLS = 13;
  PT_Yxy = 14;
  PT_HiFi = 15;
  PT_HiFi7 = 16;
  PT_HiFi8 = 17;

  TYPE_GRAY_8 = ((PT_GRAY shl 16) or (1 shl 3) or (1));
  TYPE_GRAY_8_REV = ((PT_GRAY shl 16) or (1 shl 3) or (1) or (1 shl 13));
  TYPE_GRAY_16 = ((PT_GRAY shl 16) or (1 shl 3) or (2));
  TYPE_GRAY_16_REV = ((PT_GRAY shl 16) or (1 shl 3) or (2) or (1 shl 13));
  TYPE_GRAY_16_SE = ((PT_GRAY shl 16) or (1 shl 3) or (2) or (1 shl 11));
  TYPE_GRAYA_8 = ((PT_GRAY shl 16) or (1 shl 7) or (1 shl 3) or (1));
  TYPE_GRAYA_16 = ((PT_GRAY shl 16) or (1 shl 7) or (1 shl 3) or (2));
  TYPE_GRAYA_16_SE = ((PT_GRAY shl 16) or (1 shl 7) or (1 shl 3) or (2) or (1 shl 11));
  TYPE_GRAYA_8_PLANAR = ((PT_GRAY shl 16) or (1 shl 7) or (1 shl 3) or (1) or (1 shl 12));
  TYPE_GRAYA_16_PLANAR = ((PT_GRAY shl 16) or (1 shl 7) or (1 shl 3) or (2) or (1 shl 12));

  TYPE_RGB_8 = ((PT_RGB shl 16) or (3 shl 3) or (1));
  TYPE_RGB_8_PLANAR = ((PT_RGB shl 16) or (3 shl 3) or (1) or (1 shl 12));
  TYPE_BGR_8 = ((PT_RGB shl 16) or (3 shl 3) or (1) or (1 shl 10));
  TYPE_BGR_8_PLANAR = ((PT_RGB shl 16) or (3 shl 3) or (1) or (1 shl 10) or (1 shl 12));
  TYPE_RGB_16 = ((PT_RGB shl 16) or (3 shl 3) or (2));
  TYPE_RGB_16_PLANAR = ((PT_RGB shl 16) or (3 shl 3) or (2) or (1 shl 12));
  TYPE_RGB_16_SE = ((PT_RGB shl 16) or (3 shl 3) or (2) or (1 shl 11));
  TYPE_BGR_16 = ((PT_RGB shl 16) or (3 shl 3) or (2) or (1 shl 10));
  TYPE_BGR_16_PLANAR = ((PT_RGB shl 16) or (3 shl 3) or (2) or (1 shl 10) or (1 shl 12));
  TYPE_BGR_16_SE = ((PT_RGB shl 16) or (3 shl 3) or (2) or (1 shl 10) or (1 shl 11));

  TYPE_RGBA_8 = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (1));
  TYPE_RGBA_8_PLANAR = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (1) or (1 shl 12));
  TYPE_RGBA_16 = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2));
  TYPE_RGBA_16_PLANAR = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2) or (1 shl 12));
  TYPE_RGBA_16_SE = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2) or (1 shl 11));

  TYPE_ARGB_8 = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (1) or (1 shl 14));
  TYPE_ARGB_16 = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2) or (1 shl 14));

  TYPE_ABGR_8 = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (1) or (1 shl 10));
  TYPE_ABGR_16 = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2) or (1 shl 10));
  TYPE_ABGR_16_PLANAR = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2) or (1 shl 10) or (1 shl 12));
  TYPE_ABGR_16_SE = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2) or (1 shl 10) or (1 shl 11));

  TYPE_BGRA_8 = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (1) or (1 shl 10) or (1 shl 14));
  TYPE_BGRA_16 = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2) or (1 shl 10) or (1 shl 14));
  TYPE_BGRA_16_SE = ((PT_RGB shl 16) or (1 shl 7) or (3 shl 3) or (2) or (1 shl 11) or (1 shl 14));

  TYPE_CMY_8 = ((PT_CMY shl 16) or (3 shl 3) or (1));
  TYPE_CMY_8_PLANAR = ((PT_CMY shl 16) or (3 shl 3) or (1) or (1 shl 12));
  TYPE_CMY_16 = ((PT_CMY shl 16) or (3 shl 3) or (2));
  TYPE_CMY_16_PLANAR = ((PT_CMY shl 16) or (3 shl 3) or (2) or (1 shl 12));
  TYPE_CMY_16_SE = ((PT_CMY shl 16) or (3 shl 3) or (2) or (1 shl 11));

  TYPE_CMYK_8 = ((PT_CMYK shl 16) or (4 shl 3) or (1));
  TYPE_CMYK_8_REV = ((PT_CMYK shl 16) or (4 shl 3) or (1) or (1 shl 13));
  TYPE_YUVK_8 = TYPE_CMYK_8_REV;
  TYPE_CMYK_8_PLANAR = ((PT_CMYK shl 16) or (4 shl 3) or (1) or (1 shl 12));
  TYPE_CMYK_16 = ((PT_CMYK shl 16) or (4 shl 3) or (2));
  TYPE_CMYK_16_REV = ((PT_CMYK shl 16) or (4 shl 3) or (2) or (1 shl 13));
  TYPE_YUVK_16 = TYPE_CMYK_16_REV;
  TYPE_CMYK_16_PLANAR = ((PT_CMYK shl 16) or (4 shl 3) or (2) or (1 shl 12));
  TYPE_CMYK_16_SE = ((PT_CMYK shl 16) or (4 shl 3) or (2) or (1 shl 11));

  TYPE_KYMC_8 = ((PT_CMYK shl 16) or (4 shl 3) or (1) or (1 shl 10));
  TYPE_KYMC_16 = ((PT_CMYK shl 16) or (4 shl 3) or (2) or (1 shl 10));
  TYPE_KYMC_16_SE = ((PT_CMYK shl 16) or (4 shl 3) or (2) or (1 shl 10) or (1 shl 11));

  TYPE_KCMY_8 = ((PT_CMYK shl 16) or (4 shl 3) or (1) or (1 shl 14));
  TYPE_KCMY_8_REV = ((PT_CMYK shl 16) or (4 shl 3) or (1) or (1 shl 13) or (1 shl 14));
  TYPE_KCMY_16 = ((PT_CMYK shl 16) or (4 shl 3) or (2) or (1 shl 14));
  TYPE_KCMY_16_REV = ((PT_CMYK shl 16) or (4 shl 3) or (2) or (1 shl 13) or (1 shl 14));
  TYPE_KCMY_16_SE = ((PT_CMYK shl 16) or (4 shl 3) or (2) or (1 shl 11) or (1 shl 14));

  TYPE_CMYK5_8 = ((5 shl 3) or (1));
  TYPE_CMYK5_16 = ((5 shl 3) or (2));
  TYPE_CMYK5_16_SE = ((5 shl 3) or (2) or (1 shl 11));
  TYPE_KYMC5_8 = ((5 shl 3) or (1) or (1 shl 10));
  TYPE_KYMC5_16 = ((5 shl 3) or (2) or (1 shl 10));
  TYPE_KYMC5_16_SE = ((5 shl 3) or (2) or (1 shl 10) or (1 shl 11));

  TYPE_CMYKcm_8 = ((6 shl 3) or (1));
  TYPE_CMYKcm_8_PLANAR = ((6 shl 3) or (1) or (1 shl 12));
  TYPE_CMYKcm_16 = ((6 shl 3) or (2));
  TYPE_CMYKcm_16_PLANAR = ((6 shl 3) or (2) or (1 shl 12));
  TYPE_CMYKcm_16_SE = ((6 shl 3) or (2) or (1 shl 11));

  TYPE_CMYK7_8 = ((7 shl 3) or (1));
  TYPE_CMYK7_16 = ((7 shl 3) or (2));
  TYPE_CMYK7_16_SE = ((7 shl 3) or (2) or (1 shl 11));
  TYPE_KYMC7_8 = ((7 shl 3) or (1) or (1 shl 10));
  TYPE_KYMC7_16 = ((7 shl 3) or (2) or (1 shl 10));
  TYPE_KYMC7_16_SE = ((7 shl 3) or (2) or (1 shl 10) or (1 shl 11));
  TYPE_CMYK8_8 = ((8 shl 3) or (1));
  TYPE_CMYK8_16 = ((8 shl 3) or (2));
  TYPE_CMYK8_16_SE = ((8 shl 3) or (2) or (1 shl 11));
  TYPE_KYMC8_8 = ((8 shl 3) or (1) or (1 shl 10));
  TYPE_KYMC8_16 = ((8 shl 3) or (2) or (1 shl 10));
  TYPE_KYMC8_16_SE = ((8 shl 3) or (2) or (1 shl 10) or (1 shl 11));
  TYPE_CMYK9_8 = ((9 shl 3) or (1));
  TYPE_CMYK9_16 = ((9 shl 3) or (2));
  TYPE_CMYK9_16_SE = ((9 shl 3) or (2) or (1 shl 11));
  TYPE_KYMC9_8 = ((9 shl 3) or (1) or (1 shl 10));
  TYPE_KYMC9_16 = ((9 shl 3) or (2) or (1 shl 10));
  TYPE_KYMC9_16_SE = ((9 shl 3) or (2) or (1 shl 10) or (1 shl 11));
  TYPE_CMYK10_8 = ((10 shl 3) or (1));
  TYPE_CMYK10_16 = ((10 shl 3) or (2));
  TYPE_CMYK10_16_SE = ((10 shl 3) or (2) or (1 shl 11));
  TYPE_KYMC10_8 = ((10 shl 3) or (1) or (1 shl 10));
  TYPE_KYMC10_16 = ((10 shl 3) or (2) or (1 shl 10));
  TYPE_KYMC10_16_SE = ((10 shl 3) or (2) or (1 shl 10) or (1 shl 11));
  TYPE_CMYK11_8 = ((11 shl 3) or (1));
  TYPE_CMYK11_16 = ((11 shl 3) or (2));
  TYPE_CMYK11_16_SE = ((11 shl 3) or (2) or (1 shl 11));
  TYPE_KYMC11_8 = ((11 shl 3) or (1) or (1 shl 10));
  TYPE_KYMC11_16 = ((11 shl 3) or (2) or (1 shl 10));
  TYPE_KYMC11_16_SE = ((11 shl 3) or (2) or (1 shl 10) or (1 shl 11));
  TYPE_CMYK12_8 = ((12 shl 3) or (1));
  TYPE_CMYK12_16 = ((12 shl 3) or (2));
  TYPE_CMYK12_16_SE = ((12 shl 3) or (2) or (1 shl 11));
  TYPE_KYMC12_8 = ((12 shl 3) or (1) or (1 shl 10));
  TYPE_KYMC12_16 = ((12 shl 3) or (2) or (1 shl 10));
  TYPE_KYMC12_16_SE = ((12 shl 3) or (2) or (1 shl 10) or (1 shl 11));

  TYPE_XYZ_16 = ((PT_XYZ shl 16) or (3 shl 3) or (2));
  TYPE_Lab_8 = ((PT_Lab shl 16) or (3 shl 3) or (1));
  TYPE_ALab_8 = ((PT_Lab shl 16) or (3 shl 3) or (1) or (1 shl 7) or (1 shl 10));
  TYPE_Lab_16 = ((PT_Lab shl 16) or (3 shl 3) or (2));
  TYPE_Yxy_16 = ((PT_Yxy shl 16) or (3 shl 3) or (2));

  TYPE_YCbCr_8 = ((PT_YCbCr shl 16) or (3 shl 3) or (1));
  TYPE_YCbCr_8_PLANAR = ((PT_YCbCr shl 16) or (3 shl 3) or (1) or (1 shl 12));
  TYPE_YCbCr_16 = ((PT_YCbCr shl 16) or (3 shl 3) or (2));
  TYPE_YCbCr_16_PLANAR = ((PT_YCbCr shl 16) or (3 shl 3) or (2) or (1 shl 12));
  TYPE_YCbCr_16_SE = ((PT_YCbCr shl 16) or (3 shl 3) or (2) or (1 shl 11));

  TYPE_YUV_8 = ((PT_YUV shl 16) or (3 shl 3) or (1));
  TYPE_YUV_8_PLANAR = ((PT_YUV shl 16) or (3 shl 3) or (1) or (1 shl 12));
  TYPE_YUV_16 = ((PT_YUV shl 16) or (3 shl 3) or (2));
  TYPE_YUV_16_PLANAR = ((PT_YUV shl 16) or (3 shl 3) or (2) or (1 shl 12));
  TYPE_YUV_16_SE = ((PT_YUV shl 16) or (3 shl 3) or (2) or (1 shl 11));

  TYPE_HLS_8 = ((PT_HLS shl 16) or (3 shl 3) or (1));
  TYPE_HLS_8_PLANAR = ((PT_HLS shl 16) or (3 shl 3) or (1) or (1 shl 12));
  TYPE_HLS_16 = ((PT_HLS shl 16) or (3 shl 3) or (2));
  TYPE_HLS_16_PLANAR = ((PT_HLS shl 16) or (3 shl 3) or (2) or (1 shl 12));
  TYPE_HLS_16_SE = ((PT_HLS shl 16) or (3 shl 3) or (2) or (1 shl 11));

  TYPE_HSV_8 = ((PT_HSV shl 16) or (3 shl 3) or (1));
  TYPE_HSV_8_PLANAR = ((PT_HSV shl 16) or (3 shl 3) or (1) or (1 shl 12));
  TYPE_HSV_16 = ((PT_HSV shl 16) or (3 shl 3) or (2));
  TYPE_HSV_16_PLANAR = ((PT_HSV shl 16) or (3 shl 3) or (2) or (1 shl 12));
  TYPE_HSV_16_SE = ((PT_HSV shl 16) or (3 shl 3) or (2) or (1 shl 11));

  TYPE_NAMED_COLOR_INDEX = ((1 shl 3) or (2));

  TYPE_XYZ_DBL = ((PT_XYZ shl 16) or (3 shl 3) or (0));
  TYPE_Lab_DBL = ((PT_Lab shl 16) or (3 shl 3) or (0));
  TYPE_GRAY_DBL = ((PT_GRAY shl 16) or (1 shl 3) or (0));
  TYPE_RGB_DBL = ((PT_RGB shl 16) or (3 shl 3) or (0));
  TYPE_CMYK_DBL = ((PT_CMYK shl 16) or (4 shl 3) or (0));

  // Intents

  INTENT_PERCEPTUAL = 0;
  INTENT_RELATIVE_COLORIMETRIC = 1;
  INTENT_SATURATION = 2;
  INTENT_ABSOLUTE_COLORIMETRIC = 3;

  cmsFLAGS_NOTPRECALC = $0100;
  cmsFLAGS_NULLTRANSFORM = $0200; // Don't transform anyway
  cmsFLAGS_HIGHRESPRECALC = $0400; // Use more memory to give better accurancy
  cmsFLAGS_LOWRESPRECALC = $0800; // Use less memory to minimize resouces

function IEcmsOpenProfileFromFile(stream: TStream; save: boolean; save8bit: boolean): cmsHPROFILE;
function IEcmsCreateTransform(Input: cmsHPROFILE; InputFormat: DWORD; Output: cmsHPROFILE; OutputFormat: DWORD; Intent: integer; dwFlags: DWORD): cmsHTRANSFORM;
function IEcmsCloseProfile(hProfile: cmsHPROFILE): longbool;
procedure IEcmsDeleteTransform(hTransform: cmsHTRANSFORM);
procedure IEcmsDoTransform(Transform: cmsHTRANSFORM; InputBuffer: pointer; OutputBuffer: pointer; Size: dword);
function IEcmsCreate_sRGBProfile: cmsHPROFILE;
function IEcmsCreateLabProfile(WhitePoint_x, WhitePoint_y, WhitePoint_Y_: double): cmsHPROFILE;
function IEcmsCreateLabProfileD50: cmsHPROFILE;
function IEcmsCreateXYZProfile: cmsHPROFILE;
function IEcmsWhitePointFromTemp(TempK: integer; var WhitePoint_x, WhitePoint_y, WhitePoint_Y_: double): boolean;

implementation

uses math;

{$WARNINGS OFF}

var
  UsedSpace: integer;

const
  MAX_TABLE_TAG = 50;

  D50X = (0.9642);
  D50Y = (1.0);
  D50Z = (0.8249);

  icMagicNumber = $61637370; // 'acsp' */
  icVersionNumber = $02100000; // 2.1.0, BCD */

  icSigChromaticAdaptationTag = $63686164; // 'chad'

  VX = 0;
  VY = 1;
  VZ = 2;

  MAXCHANNELS = 16;

  LUT_V4_OUTPUT_EMULATE_V2 = $10000; // Is a V4 output LUT, emulating V2
  LUT_V4_INPUT_EMULATE_V2 = $20000; // Is a V4 input LUT, emulating V2
  LUT_V2_OUTPUT_EMULATE_V4 = $40000; // Is a V2 output LUT, emulating V4
  LUT_V2_INPUT_EMULATE_V4 = $80000; // Is a V2 input LUT, emulating V4

  // LUT handling

  LUT_HASMATRIX = $0001;
  LUT_HASTL1 = $0002;
  LUT_HASTL2 = $0008;
  LUT_HAS3DGRID = $0010;

  LUT_HASMATRIX3 = $0020; // Matrix + offset for LutAToB
  LUT_HASMATRIX4 = $0040; // Matrix + offset for LutBToA

  LUT_HASTL3 = $0100; // '3' curves for LutAToB
  LUT_HASTL4 = $0200; // '4' curves for LutBToA

  AlarmR: WORD = $8FFF;
  AlarmG: WORD = $8FFF;
  AlarmB: WORD = $8FFF;

  icSigLuvKData = $4C75764B; // 'LuvK'
  icSigHexachromeData = $4D434836; // MCH6
  icSigHeptachromeData = $4D434837; // MCH7
  icSigOctachromeData = $4D434838; // MCH8

  icSiglutAtoBType = $6D414220; // mAB
  icSiglutBtoAType = $6D424120; // mBA

  icSigParametricCurveType = $70617261; // parametric (ICC 4.0)

  cmsFLAGS_NOTCACHE = $0040; // Inhibit 1-pixel cache

  XYZRel = 0;
  LabRel = 1;

  cmsFLAGS_GAMUTCHECK = $1000; // Out of Gamut alarm
  cmsFLAGS_SOFTPROOFING = $4000; // Do softproofing

  icSigChromaticityTag = $6368726D; // As per Addendum 2 to Spec. ICC.1:1998-09

  lcmsSignature = $6C636D73;

  icSigChromaticityType = $6368726D;

  SAMPLER_HASTL1 = LUT_HASTL1;

  SAMPLER_HASTL2 = LUT_HASTL2;

  SAMPLER_INSPECT = $01000000;

  ERR_THERESHOLD = 5;

  cmsFLAGS_MATRIXINPUT = $0001;
  cmsFLAGS_MATRIXOUTPUT = $0002;
  MATSHAPER_INPUT = $0004;

  MATSHAPER_OUTPUT = $0008;
  MATSHAPER_HASINPSHAPER = $0010;

  MATSHAPER_ALLSMELTED = (MATSHAPER_INPUT or MATSHAPER_OUTPUT);

  MATSHAPER_HASMATRIX = $0001;

  MATSHAPER_HASSHAPER = $0002;

  icReflective = $00000000; // Bit pos 0 */
  icTransparency = $00000001; // Bit pos 0 */
  icGlossy = $00000000; // Bit pos 1 */
  icMatte = $00000002; // Bit pos 1 */

  LCMS_BPFLAGS_D50_ADAPTED = $0001;

  PERCEPTUAL_BLACK_X = (0.00336);
  PERCEPTUAL_BLACK_Y = (0.0034731);
  PERCEPTUAL_BLACK_Z = (0.00287);

  LCMS_USED_AS_INPUT = 0;
  LCMS_USED_AS_OUTPUT = 1;
  LCMS_USED_AS_PROOF = 2;

  cmsFLAGS_WHITEBLACKCOMPENSATION = $2000;
  cmsFLAGS_BLACKPOINTCOMPENSATION = cmsFLAGS_WHITEBLACKCOMPENSATION;

  cmsFLAGS_NOPRELINEARIZATION = $0010;

type

  icUInt32Number = dword;
  icInt32Number = integer;
  icSignature = integer;
  icUInt16Number = word;
  icInt8Number = shortint;
  icUInt8Number = byte;

  icUInt64Number = array[0..1] of dword;
  icS15Fixed16Number = icInt32Number;

  Fixed32 = icInt32Number;

type
  icProfileClassSignature = integer;
const
  icSigInputClass = $73636E72; (* 'scnr' *)
  icSigDisplayClass = $6D6E7472; (* 'mntr' *)
  icSigOutputClass = $70727472; (* 'prtr' *)
  icSigLinkClass = $6C696E6B; (* 'link' *)
  icSigAbstractClass = $61627374; (* 'abst' *)
  icSigColorSpaceClass = $73706163; (* 'spac' *)
  icSigNamedColorClass = $6E6D636C; (* 'nmcl' *)
  icMaxEnumClass = $FFFFFFFF;

type
  icColorSpaceSignature = integer;
const
  icSigXYZData = $58595A20; (* 'XYZ ' *)
  icSigLabData = $4C616220; (* 'Lab ' *)
  icSigLuvData = $4C757620; (* 'Luv ' *)
  icSigYCbCrData = $59436272; (* 'YCbr' *)
  icSigYxyData = $59787920; (* 'Yxy ' *)
  icSigRgbData = $52474220; (* 'RGB ' *)
  icSigGrayData = $47524159; (* 'GRAY' *)
  icSigHsvData = $48535620; (* 'HSV ' *)
  icSigHlsData = $484C5320; (* 'HLS ' *)
  icSigCmykData = $434D594B; (* 'CMYK' *)
  icSigCmyData = $434D5920; (* 'CMY ' *)
  icSig2colorData = $32434C52; (* '2CLR' *)
  icSig3colorData = $33434C52; (* '3CLR' *)
  icSig4colorData = $34434C52; (* '4CLR' *)
  icSig5colorData = $35434C52; (* '5CLR' *)
  icSig6colorData = $36434C52; (* '6CLR' *)
  icSig7colorData = $37434C52; (* '7CLR' *)
  icSig8colorData = $38434C52; (* '8CLR' *)
  icSig9colorData = $39434C52; (* '9CLR' *)
  icSig10colorData = $41434C52; (* 'ACLR' *)
  icSig11colorData = $42434C52; (* 'BCLR' *)
  icSig12colorData = $43434C52; (* 'CCLR' *)
  icSig13colorData = $44434C52; (* 'DCLR' *)
  icSig14colorData = $45434C52; (* 'ECLR' *)
  icSig15colorData = $46434C52; (* 'FCLR' *)
  icMaxEnumData = $FFFFFFFF;

type
  icTagSignature = integer;
const
  icSigAToB0Tag = $41324230; (* 'A2B0' *)
  icSigAToB1Tag = $41324231; (* 'A2B1' *)
  icSigAToB2Tag = $41324232; (* 'A2B2' *)
  icSigBlueColorantTag = $6258595A; (* 'bXYZ' *)
  icSigBlueTRCTag = $62545243; (* 'bTRC' *)
  icSigBToA0Tag = $42324130; (* 'B2A0' *)
  icSigBToA1Tag = $42324131; (* 'B2A1' *)
  icSigBToA2Tag = $42324132; (* 'B2A2' *)
  icSigCalibrationDateTimeTag = $63616C74; (* 'calt' *)
  icSigCharTargetTag = $74617267; (* 'targ' *)
  icSigCopyrightTag = $63707274; (* 'cprt' *)
  icSigCrdInfoTag = $63726469; (* 'crdi' *)
  icSigDeviceMfgDescTag = $646D6E64; (* 'dmnd' *)
  icSigDeviceModelDescTag = $646D6464; (* 'dmdd' *)
  icSigGamutTag = $67616D74; (* 'gamt ' *)
  icSigGrayTRCTag = $6B545243; (* 'kTRC' *)
  icSigGreenColorantTag = $6758595A; (* 'gXYZ' *)
  icSigGreenTRCTag = $67545243; (* 'gTRC' *)
  icSigLuminanceTag = $6C756D69; (* 'lumi' *)
  icSigMeasurementTag = $6D656173; (* 'meas' *)
  icSigMediaBlackPointTag = $626B7074; (* 'bkpt' *)
  icSigMediaWhitePointTag = $77747074; (* 'wtpt' *)
  icSigNamedColorTag = $6E636F6C; (* 'ncol'
  * OBSOLETE, use ncl2 *)
  icSigNamedColor2Tag = $6E636C32; (* 'ncl2' *)
  icSigPreview0Tag = $70726530; (* 'pre0' *)
  icSigPreview1Tag = $70726531; (* 'pre1' *)
  icSigPreview2Tag = $70726532; (* 'pre2' *)
  icSigProfileDescriptionTag = $64657363; (* 'desc' *)
  icSigProfileSequenceDescTag = $70736571; (* 'pseq' *)
  icSigPs2CRD0Tag = $70736430; (* 'psd0' *)
  icSigPs2CRD1Tag = $70736431; (* 'psd1' *)
  icSigPs2CRD2Tag = $70736432; (* 'psd2' *)
  icSigPs2CRD3Tag = $70736433; (* 'psd3' *)
  icSigPs2CSATag = $70733273; (* 'ps2s' *)
  icSigPs2RenderingIntentTag = $70733269; (* 'ps2i' *)
  icSigRedColorantTag = $7258595A; (* 'rXYZ' *)
  icSigRedTRCTag = $72545243; (* 'rTRC' *)
  icSigScreeningDescTag = $73637264; (* 'scrd' *)
  icSigScreeningTag = $7363726E; (* 'scrn' *)
  icSigTechnologyTag = $74656368; (* 'tech' *)
  icSigUcrBgTag = $62666420; (* 'bfd ' *)
  icSigViewingCondDescTag = $76756564; (* 'vued' *)
  icSigViewingConditionsTag = $76696577; (* 'view' *)
  icMaxEnumTag = $FFFFFFFF;

type
  PicTagSignature = ^icTagSignature;

  icRenderingIntent = integer;
const
  icPerceptual = 0;
  icRelativeColorimetric = 1;
  icSaturation = 2;
  icAbsoluteColorimetric = 3;
  icMaxEnumIntent = $FFFFFFFF;

type
  icPlatformSignature = integer;

const
  icSigMacintosh = $4150504C; // 'APPL' */
  icSigMicrosoft = $4D534654; // 'MSFT' */
  icSigSolaris = $53554E57; // 'SUNW' */
  icSigSGI = $53474920; // 'SGI ' */
  icSigTaligent = $54474E54; // 'TGNT' */
  icMaxEnumPlatform = $FFFFFFFF;

type
  icTagTypeSignature = integer;
const
  icSigCurveType = $63757276; // 'curv' */
  icSigDataType = $64617461; // 'data' */
  icSigDateTimeType = $6474696D; // 'dtim' */
  icSigLut16Type = $6D667432; // 'mft2' */
  icSigLut8Type = $6D667431; // 'mft1' */
  icSigMeasurementType = $6D656173; // 'meas' */
  icSigNamedColorType = $6E636F6C; // 'ncol'
  //* OBSOLETE; use ncl2 */
  icSigProfileSequenceDescType = $70736571; // 'pseq' */
  icSigS15Fixed16ArrayType = $73663332; // 'sf32' */
  icSigScreeningType = $7363726E; // 'scrn' */
  icSigSignatureType = $73696720; // 'sig ' */
  icSigTextType = $74657874; // 'text' */
  icSigTextDescriptionType = $64657363; // 'desc' */
  icSigU16Fixed16ArrayType = $75663332; // 'uf32' */
  icSigUcrBgType = $62666420; // 'bfd ' */
  icSigUInt16ArrayType = $75693136; // 'ui16' */
  icSigUInt32ArrayType = $75693332; // 'ui32' */
  icSigUInt64ArrayType = $75693634; // 'ui64' */
  icSigUInt8ArrayType = $75693038; // 'ui08' */
  icSigViewingConditionsType = $76696577; // 'view' */
  icSigXYZType = $58595A20; // 'XYZ ' */
  icSigXYZArrayType = $58595A20; // 'XYZ ' */
  icSigNamedColor2Type = $6E636C32; // 'ncl2' */
  icSigCrdInfoType = $63726469; // 'crdi' */
  icMaxEnumType = $FFFFFFFF;

  // technology signature descriptions */
type
  icTechnologySignature = integer;
const
  icSigDigitalCamera = $6463616D; // 'dcam' */
  icSigFilmScanner = $6673636E; // 'fscn' */
  icSigReflectiveScanner = $7273636E; // 'rscn' */
  icSigInkJetPrinter = $696A6574; // 'ijet' */
  icSigThermalWaxPrinter = $74776178; // 'twax' */
  icSigElectrophotographicPrinter = $6570686F; // 'epho' */
  icSigElectrostaticPrinter = $65737461; // 'esta' */
  icSigDyeSublimationPrinter = $64737562; // 'dsub' */
  icSigPhotographicPaperPrinter = $7270686F; // 'rpho' */
  icSigFilmWriter = $6670726E; // 'fprn' */
  icSigVideoMonitor = $7669646D; // 'vidm' */
  icSigVideoCamera = $76696463; // 'vidc' */
  icSigProjectionTelevision = $706A7476; // 'pjtv' */
  icSigCRTDisplay = $43525420; // 'CRT ' */
  icSigPMDisplay = $504D4420; // 'PMD ' */
  icSigAMDisplay = $414D4420; // 'AMD ' */
  icSigPhotoCD = $4B504344; // 'KPCD' */
  icSigPhotoImageSetter = $696D6773; // 'imgs' */
  icSigGravure = $67726176; // 'grav' */
  icSigOffsetLithography = $6F666673; // 'offs' */
  icSigSilkscreen = $73696C6B; // 'silk' */
  icSigFlexography = $666C6578; // 'flex' */
  icMaxEnumTechnology = $FFFFFFFF;

type
  cmsCIEXYZ = packed record
    X: double;
    Y: double;
    Z: double;
  end;
  LPcmsCIEXYZ = ^cmsCIEXYZ;

  VEC3 = packed record
    n: array[0..2] of double;
  end;

  LPVEC3 = ^VEC3;

  MAT3 = packed record
    v: array[0..2] of VEC3;
  end;

  LPMAT3 = ^MAT3;

  icTag = packed record
    sig: icTagSignature;
    offset: icUInt32Number;
    size: icUInt32Number;
  end;

  icDateTimeNumber = packed record
    year: icUInt16Number;
    month: icUInt16Number;
    day: icUInt16Number;
    hours: icUInt16Number;
    minutes: icUInt16Number;
    seconds: icUInt16Number;
  end;
  PicDateTimeNumber = ^icDateTimeNumber;

  icXYZNumber = packed record
    X: icS15Fixed16Number;
    Y: icS15Fixed16Number;
    Z: icS15Fixed16Number;
  end;

  icHeader = packed record
    size: icUInt32Number;
    cmmId: icSignature;
    version: icUInt32Number;
    deviceClass: icProfileClassSignature;
    colorSpace: icColorSpaceSignature;
    pcs: icColorSpaceSignature;
    date: icDateTimeNumber;
    magic: icSignature;
    xplatform: icPlatformSignature;
    flags: icUInt32Number;
    manufacturer: icSignature;
    model: icUInt32Number;
    attributes: icUInt64Number;
    renderingIntent: icUInt32Number;
    illuminant: icXYZNumber;
    creator: icSignature;
    reserved: array[0..43] of icInt8Number;
  end;

  TReadFunction = function(buffer: pointer; size: integer; count: integer; stream: TStream): integer;
  TSeekFunction = function(stream: TStream; offset: int64): longbool;
  TCloseFunction = function(stream: TStream): longbool;
  TTellFunction = function(stream: TStream): int64;
  TWriteFunction = function(stream: TStream; size: integer; Ptr: pointer): longbool;

  LCMSICCPROFILE = packed record
    stream: TStream;
    DeviceClass: icProfileClassSignature;
    ColorSpace: icColorSpaceSignature;
    PCS: icColorSpaceSignature;
    RenderingIntent: icRenderingIntent;
    flags: icUInt32Number;
    Illuminant: cmsCIEXYZ;
    Version: icUInt32Number;
    ChromaticAdaptation: MAT3;
    MediaWhitePoint: cmsCIEXYZ;
    MediaBlackPoint: cmsCIEXYZ;
    ProfileID: array[0..15] of BYTE;
    TagCount: icInt32Number;
    TagNames: array[0..MAX_TABLE_TAG - 1] of icTagSignature;
    TagSizes: array[0..MAX_TABLE_TAG - 1] of integer;
    TagOffsets: array[0..MAX_TABLE_TAG - 1] of integer;
    TagPtrs: array[0..MAX_TABLE_TAG - 1] of pointer;
    PhysicalFile: TStream;
    IsWrite: longbool;
    SaveAs8Bits: longbool;
    Read: TReadFunction;
    Seek: TSeekFunction;
    Close: TCloseFunction;
    Tell: TTellFunction;
    Write: TWriteFunction;
  end;
  LPLCMSICCPROFILE = ^LCMSICCPROFILE;

  icTagBase = packed record
    sig: icTagTypeSignature; // Signature */
    reserved: array[0..3] of icInt8Number; // Reserved, set to 0 */
  end;

  WVEC3 = packed record
    n: array[0..2] of Fixed32;
  end;
  LPWVEC3 = ^WVEC3;

  WMAT3 = packed record
    v: array[0..2] of WVEC3;
  end;
  LPWMAT3 = ^WMAT3;

  _LPcmsTRANSFORM = ^_cmsTRANSFORM;

  _cmsCOLORCALLBACKFN = procedure(Transform: _LPcmsTRANSFORM;
    InputBuffer: pointer;
    OutputBuffer: pointer; Size: dword);

  _cmsFIXFN = function(info: _LPcmsTRANSFORM; ToUnroll: pwordarray; Buffer: pbyte): pbyte;

  _cmsTRANSFN = procedure(Transform: _LPcmsTRANSFORM; xIn: pwordarray; xOut: pwordarray);

  _cmsADJFN = procedure(xIn: pwordarray; xOut: pwordarray; m: LPWMAT3; b: LPWVEC3);

  LPLUT = ^LUT;

  LPL16PARAMS = ^L16PARAMS;

  _cms3DLERP = procedure(Input: pwordarray; Output: pwordarray; LutTable: pwordarray; p: LPL16PARAMS);

  //_lcms_l8opt_struc
  L8PARAMS = packed record
    X0, Y0, Z0: array[0..255] of dword;
    rx, ry, rz: array[0..255] of word;
  end;

  LPL8PARAMS = ^L8PARAMS;

  // _lcms_l16params_struc
  L16PARAMS = packed record

    nSamples: integer;
    nInputs: integer;
    nOutputs: integer;

    Domain: WORD;

    opta1, opta2: integer;
    opta3, opta4: integer;
    opta5, opta6: integer;
    opta7, opta8: integer;

    Interp3D: _cms3DLERP;

    p8: LPL8PARAMS;

  end;

  //struct _lcms_LUT_struc
  LUT = packed record

    wFlags: DWORD;
    Matrix: WMAT3;

    InputChan: dword;
    OutputChan: dword;
    InputEntries: dword;
    OutputEntries: dword;
    cLutPoints: dword;

    L1: array[0..MAXCHANNELS - 1] of PWORD;
    L2: array[0..MAXCHANNELS - 1] of PWORD;

    T: PWORD;
    Tsize: integer;

    In16params: L16PARAMS;
    Out16params: L16PARAMS;
    CLut16params: L16PARAMS;

    Intent: integer;

    Mat3: WMAT3;
    Ofs3: WVEC3;
    L3: array[0..MAXCHANNELS - 1] of PWORD;
    L3params: L16PARAMS;
    L3Entries: dword;

    Mat4: WMAT3;
    Ofs4: WVEC3;
    L4: array[0..MAXCHANNELS - 1] of PWORD;
    L4params: L16PARAMS;
    L4Entries: dword;
  end;

  MATSHAPER = packed record
    dwFlags: DWORD;

    Matrix: WMAT3;

    p16: L16PARAMS;
    L: array[0..2] of PWORD;

    p2_16: L16PARAMS;
    L2: array[0..2] of PWORD;
  end;

  LPMATSHAPER = ^MATSHAPER;

  cmsNAMEDCOLOR = packed record
    Name: array[0..MAX_PATH - 1] of AnsiChar;
    PCS: array[0..2] of WORD;
    DeviceColorant: array[0..MAXCHANNELS - 1] of WORD;
  end;
  LPcmsNAMEDCOLOR = ^cmsNAMEDCOLOR;

  cmsNAMEDCOLORLIST = packed record
    nColors: integer;
    Allocated: integer;
    ColorantCount: integer;
    Prefix: array[0..32] of AnsiChar;
    Suffix: array[0..32] of AnsiChar;
    List: array[0..0] of cmsNAMEDCOLOR;
  end;

  LPcmsNAMEDCOLORLIST = ^cmsNAMEDCOLORLIST;

  // Transformation
  _cmsTRANSFORM = packed record

    InputFormat, OutputFormat: DWORD;

    StrideIn, StrideOut: DWORD;

    Intent, ProofIntent: integer;
    DoGamutCheck: integer;

    InputProfile: cmsHPROFILE;
    OutputProfile: cmsHPROFILE;
    PreviewProfile: cmsHPROFILE;

    EntryColorSpace: icColorSpaceSignature;
    ExitColorSpace: icColorSpaceSignature;

    m1, m2: WMAT3;
    of1, of2: WVEC3;

    xform: _cmsCOLORCALLBACKFN;

    FromInput: _cmsFIXFN;
    FromDevice: _cmsTRANSFN;
    Stage1: _cmsADJFN;
    Stage2: _cmsADJFN;
    ToDevice: _cmsTRANSFN;
    ToOutput: _cmsFIXFN;

    Device2PCS: LPLUT;
    PCS2Device: LPLUT;
    Gamut: LPLUT;
    Preview: LPLUT;

    DeviceLink: LPLUT;

    InMatShaper: LPMATSHAPER;
    OutMatShaper: LPMATSHAPER;
    SmeltMatShaper: LPMATSHAPER;

    Phase1, Phase2, Phase3: integer;

    NamedColorList: LPcmsNAMEDCOLORLIST;

    lInputV4Lab, lOutputV4Lab: longbool;

    CacheIn: array[0..MAXCHANNELS - 1] of word;
    CacheOut: array[0..MAXCHANNELS - 1] of word;

  end;

  _cmstransform_struct = _cmsTRANSFORM;
  P_cmstransform_struct = ^_cmstransform_struct;

  cmsCIELab = packed record

    L: double;
    a: double;
    b: double;

  end;

  LPcmsCIELab = ^cmsCIELab;

  // lut8, input & output tables are always 256 bytes in length */
  icLut8 = packed record
    inputChan: icUInt8Number; // Num of input channels */
    outputChan: icUInt8Number; // Num of output channels */
    clutPoints: icUInt8Number; // Num of grid points */
    pad: icInt8Number;
    e00: icS15Fixed16Number; // e00 in the 3 * 3 */
    e01: icS15Fixed16Number; // e01 in the 3 * 3 */
    e02: icS15Fixed16Number; // e02 in the 3 * 3 */
    e10: icS15Fixed16Number; // e10 in the 3 * 3 */
    e11: icS15Fixed16Number; // e11 in the 3 * 3 */
    e12: icS15Fixed16Number; // e12 in the 3 * 3 */
    e20: icS15Fixed16Number; // e20 in the 3 * 3 */
    e21: icS15Fixed16Number; // e21 in the 3 * 3 */
    e22: icS15Fixed16Number; // e22 in the 3 * 3 */
    data: array[0..0] of icUInt8Number; // Data follows see spec */
    (*
     *  Data that follows is of this form
     *
     *  icUInt8Number       inputTable[inputChan][256];     * The in-table
     *  icUInt8Number       clutTable[icAny];               * The clut
     *  icUInt8Number       outputTable[outputChan][256];   * The out-table
     *)
  end;

  _cmsTestAlign8 = packed record
    a: icS15Fixed16Number;
    b: icUInt8Number;
  end;

  // lut16 */
  icLut16 = packed record
    inputChan: icUInt8Number; // Number of input channels */
    outputChan: icUInt8Number; // Number of output channels */
    clutPoints: icUInt8Number; // Number of grid points */
    pad: icInt8Number; // Padding for byte alignment */
    e00: icS15Fixed16Number; // e00 in the 3 * 3 */
    e01: icS15Fixed16Number; // e01 in the 3 * 3 */
    e02: icS15Fixed16Number; // e02 in the 3 * 3 */
    e10: icS15Fixed16Number; // e10 in the 3 * 3 */
    e11: icS15Fixed16Number; // e11 in the 3 * 3 */
    e12: icS15Fixed16Number; // e12 in the 3 * 3 */
    e20: icS15Fixed16Number; // e20 in the 3 * 3 */
    e21: icS15Fixed16Number; // e21 in the 3 * 3 */
    e22: icS15Fixed16Number; // e22 in the 3 * 3 */
    inputEnt: icUInt16Number; // Num of in-table entries */
    outputEnt: icUInt16Number; // Num of out-table entries */
    data: array[0..0] of icUInt16Number; // Data follows see spec */
    (*
     *  Data that follows is of this form
     *
     *  icUInt16Number      inputTable[inputChan][icAny];   * The in-table
     *  icUInt16Number      clutTable[icAny];               * The clut
     *  icUInt16Number      outputTable[outputChan][icAny]; * The out-table
     *)
  end;

  _cmsTestAlign16 = packed record
    a: icS15Fixed16Number;
    b: icUInt16Number;
  end;

  // icLutAtoB
  icLutAtoB = packed record
    inputChan: icUInt8Number; // Number of input channels
    outputChan: icUInt8Number; // Number of output channels
    pad1: icUInt8Number;
    pad2: icUInt8Number;
    offsetB: icUInt32Number; // Offset to first "B" curve
    offsetMat: icUInt32Number; // Offset to matrix
    offsetM: icUInt32Number; // Offset to first "M" curve
    offsetC: icUInt32Number; // Offset to CLUT
    offsetA: icUInt32Number; // Offset to first "A" curve
    //icUInt8Number     data[icAny];     Data follows see spec for size */
  end;

  GAMMATABLE = packed record

    nEntries: integer;
    GammaTable: array[0..0] of WORD;

  end;

  LPGAMMATABLE = ^GAMMATABLE;

  GAMMATABLEArray = array[0..$EFFFFFF] of LPGAMMATABLE;
  LPGAMMATABLEArray = ^GAMMATABLEArray;

  TPWORDARRAY = array[0..$EFFFFFF] of pword;
  PPWORDARRAY = ^TPWORDARRAY;

  icCLutStruct = packed record
    gridPoints: array[0..15] of icUInt8Number; // Number of grid points in each dimension.
    prec: icUInt8Number; // Precision of data elements in bytes.
    pad1: icUInt8Number;
    pad2: icUInt8Number;
    pad3: icUInt8Number;
    //icUInt8Number     data[icAny];     Data follows see spec for size */
  end;

  // icLutBtoA
  icLutBtoA = packed record
    inputChan: icUInt8Number; // Number of input channels
    outputChan: icUInt8Number; // Number of output channels
    pad1: icUInt8Number;
    pad2: icUInt8Number;
    offsetB: icUInt32Number; // Offset to first "B" curve
    offsetMat: icUInt32Number; // Offset to matrix
    offsetM: icUInt32Number; // Offset to first "M" curve
    offsetC: icUInt32Number; // Offset to CLUT
    offsetA: icUInt32Number; // Offset to first "A" curve
    //icUInt8Number     data[icAny];     Data follows see spec for size */
  end;

  icNamedColor2 = packed record
    vendorFlag: icUInt32Number; // Bottom 16 bits for IC use */
    count: icUInt32Number; // Count of named colors */
    nDeviceCoords: icUInt32Number; // Num of device coordinates */
    prefix: array[0..31] of icInt8Number; // Prefix for each color name */
    suffix: array[0..31] of icInt8Number; // Suffix for each color name */
    data: array[0..0] of icInt8Number; // Named color data follows */
  end;
  PicNamedColor2 = ^icNamedColor2;

  GAMUTCHAIN = packed record

    hForward, hReverse: cmsHTRANSFORM;
  end;

  LPGAMUTCHAIN = ^GAMUTCHAIN;

  cmsCIExyY = packed record

    x: double;
    y_mi: double;
    Y_ma: double;

  end;

  LPcmsCIExyY = ^cmsCIExyY;

  cmsCIExyYTRIPLE = packed record

    Red: cmsCIExyY;
    Green: cmsCIExyY;
    Blue: cmsCIExyY;

  end;

  LPcmsCIExyYTRIPLE = ^cmsCIExyYTRIPLE;

  cmsCIEXYZTRIPLE = packed record

    Red: cmsCIEXYZ;
    Green: cmsCIEXYZ;
    Blue: cmsCIEXYZ;

  end;

  LPcmsCIEXYZTRIPLE = ^cmsCIEXYZTRIPLE;

  cmsPSEQDESC = packed record

    deviceMfg: icSignature;
    deviceModel: icSignature;
    attributes: array[0..1] of icUInt32Number;
    technology: icTechnologySignature;

    Manufacturer: array[0..511] of AnsiChar;
    Model: array[0..511] of AnsiChar;

  end;

  LPcmsPSEQDESC = ^cmsPSEQDESC;

  cmsSEQ = packed record

    n: integer;
    seq: array[0..0] of cmsPSEQDESC;

  end;
  LPcmsSEQ = ^cmsSEQ;

  // Profile sequence structure */
  icDescStruct = packed record
    deviceMfg: icSignature; // Dev Manufacturer */
    deviceModel: icSignature; // Dev Model */
    attributes: icUInt64Number; // Dev attributes */
    technology: icTechnologySignature; // Technology sig */
    data: array[0..0] of icInt8Number; // Desc text follows */
  end;

  _cmsSAMPLER = function(xIn: pwordarray; xOut: pwordarray; Cargo: pointer): integer;

const

  Device2PCS: array[0..3] of icTagSignature = (icSigAToB0Tag, // Perceptual
    icSigAToB1Tag, // Relative colorimetric
    icSigAToB2Tag, // Saturation
    icSigAToB1Tag); // Absolute colorimetric
  // (Relative/WhitePoint)

  PCS2Device: array[0..3] of icTagSignature = (icSigBToA0Tag, // Perceptual
    icSigBToA1Tag, // Relative colorimetric
    icSigBToA2Tag, // Saturation
    icSigBToA1Tag); // Absolute colorimetric
  // (Relative/WhitePoint)
  Preview: array[0..3] of icTagSignature = (icSigPreview0Tag,
    icSigPreview1Tag,
    icSigPreview2Tag,
    icSigPreview1Tag);

  ///////////////////////////////////////////////////////////////////////////////////////////////////

function DOUBLE_TO_FIXED(x: double): Fixed32; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := round(x * 65536 );
end;

function FIXED_TO_DOUBLE(x: Fixed32): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := x / 65536;
end;

function FixedMul(a: Fixed32; b: Fixed32): Fixed32; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  //result := DOUBLE_TO_FIXED(FIXED_TO_DOUBLE(a) * FIXED_TO_DOUBLE(b));
  result := round(((a / 65536) * (b / 65536)) * 65536 );
end;

procedure DSWAP(var a, b: double); {$ifdef IESUPPORTINLINE} inline; {$endif}
var
  tmp: double;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

function IEFileRead(buffer: pointer; size: integer; count: integer; stream: TStream): integer;
begin
  result := stream.Read(pbyte(buffer)^, size * count) div size;
end;

function IEFileSeek(stream: TStream; offset: int64): longbool;
begin
  result := false; // fdv
  stream.Position := offset;
end;

function IEFileClose(stream: TStream): longbool;
begin
  // nothing to do because we work with streams
end;

function IEFileTell(stream: TStream): int64;
begin
  result := stream.position;
end;

function IEFileWrite(stream: TStream; size: integer; Ptr: pointer): longbool;
begin
  result := true; // fdv
  stream.Write(pbyte(Ptr)^, size);
end;

procedure AdjustEndianess16(iepByte: pbytearray); 
var
  tmp: byte;
begin
  tmp := iepByte[0];
  iepByte[0] := iepByte[1];
  iepByte[1] := tmp;
end;

procedure AdjustEndianess32(iepByte: pbyte); 
var
  temp1: byte;
  temp2: byte;
  tp: pbyte;
begin
  temp1 := iepByte^;
  inc(iepByte);
  temp2 := iepByte^;
  inc(iepByte);
  tp := iepByte;
  dec(tp);
  tp^ := iepByte^;
  iepByte^ := temp2;
  inc(iepByte);
  tp := iepByte;
  dec(tp, 3);
  tp^ := iepByte^;
  iepByte^ := temp1;
end;

(*
procedure AdjustEndianess32(iepByte: pbyte);
begin
  pinteger(iepByte)^ := IESwapDWord(pinteger(iepByte)^);
end;
*)


// Initiate a vector (double version)

procedure VEC3init(r: LPVEC3; x, y, z: double); {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  r^.n[VX] := x;
  r^.n[VY] := y;
  r^.n[VZ] := z;
end;

// Identity

procedure MAT3identity(a: LPMAT3); 
begin
  VEC3init(@a^.v[0], 1.0, 0.0, 0.0);
  VEC3init(@a^.v[1], 0.0, 1.0, 0.0);
  VEC3init(@a^.v[2], 0.0, 0.0, 1.0);
end;

// Swap two double vectors

procedure VEC3swap(a: LPVEC3; b: LPVEC3); 
begin
  DSWAP(a^.n[VX], b^.n[VX]);
  DSWAP(a^.n[VY], b^.n[VY]);
  DSWAP(a^.n[VZ], b^.n[VZ]);
end;

// Divide a vector by a constant

procedure VEC3divK(r: LPVEC3; v: LPVEC3; d: double); 
var
  d_inv: double;
begin
  d_inv := 1 / d;
  r^.n[VX] := v^.n[VX] * d_inv;
  r^.n[VY] := v^.n[VY] * d_inv;
  r^.n[VZ] := v^.n[VZ] * d_inv;
end;

// Multiply by a constant

procedure VEC3perK(r: LPVEC3; v: LPVEC3; d: double); 
begin
  r^.n[VX] := v^.n[VX] * d;
  r^.n[VY] := v^.n[VY] * d;
  r^.n[VZ] := v^.n[VZ] * d;
end;

// Minus

procedure VEC3minus(r: LPVEC3; a: LPVEC3; b: LPVEC3);
begin
  r^.n[VX] := a^.n[VX] - b^.n[VX];
  r^.n[VY] := a^.n[VY] - b^.n[VY];
  r^.n[VZ] := a^.n[VZ] - b^.n[VZ];
end;

// Inverse of a matrix b = a^(-1)
// Gauss-Jordan elimination with partial pivoting

function MAT3inverse(a: LPMAT3; b: LPMAT3): integer;
var
  i, j, max: integer;
  temp: VEC3;
begin

  MAT3identity(b);

  for j := 0 to 2 do
  begin
    max := j;
    for i := j + 1 to 2 do
      if (abs(a^.v[i].n[j]) > abs(a^.v[max].n[j])) then
        max := i;

    VEC3swap(@a^.v[max], @a^.v[j]);
    VEC3swap(@b^.v[max], @b^.v[j]);

    if (a^.v[j].n[j] = 0) then
    begin
      result := -1;
      exit;
    end;

    VEC3divK(@b^.v[j], @b^.v[j], a^.v[j].n[j]);
    VEC3divK(@a^.v[j], @a^.v[j], a^.v[j].n[j]);

    for i := 0 to 2 do
      if (i <> j) then
      begin

        VEC3perK(@temp, @b^.v[j], a^.v[i].n[j]);
        VEC3minus(@b^.v[i], @b^.v[i], @temp);

        VEC3perK(@temp, @a^.v[j], a^.v[i].n[j]);
        VEC3minus(@a^.v[i], @a^.v[i], @temp);
      end;
  end;
  result := 1;
end;

function Convert15Fixed16(fix32: icS15Fixed16Number): double;
var
  floater, sign, mid, hack: double;
  Whole, FracPart: integer;
begin
  AdjustEndianess32(@fix32);

  if fix32 < 0 then
    sign := -1
  else
    sign := 1;

  fix32 := abs(fix32);

  Whole := LOWORD(fix32 shr 16);
  FracPart := LOWORD(fix32 and $0000FFFF);

  hack := 65536.0;
  mid := FracPart / hack;
  floater := Whole + mid;

  result := sign * floater;
end;

// Allocate ICC struct. I/O routines are passed through

function ICCAllocStruct(Read: TReadFunction; Seek: TSeekFunction; Tell: TTellFunction; Close: TCloseFunction): LPLCMSICCPROFILE;
var
  Icc: LPLCMSICCPROFILE;
begin
  getmem(Icc, sizeof(LCMSICCPROFILE));
  if (Icc = nil) then
  begin
    result := nil;
    exit;
  end;
  ZeroMemory(Icc, sizeof(LCMSICCPROFILE));

  Icc^.Read := Read;
  Icc^.Seek := Seek;
  Icc^.Tell := Tell;
  Icc^.Close := Close;
  Icc^.Write := nil;

  Icc^.Illuminant.X := D50X;
  Icc^.Illuminant.Y := D50Y;
  Icc^.Illuminant.Z := D50Z;

  Icc^.TagCount := 0;

  result := Icc;
end;

function _cmsCreateProfilePlaceholder: cmsHPROFILE;
begin
  result := cmsHPROFILE(ICCAllocStruct(nil, nil, nil, nil));
end;

const
  D50XYZ: cmsCIEXYZ = (X: D50X; Y: D50Y; Z: D50Z);

function cmsD50_XYZ: LPcmsCIEXYZ; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := @D50XYZ;
end;

procedure NormalizeXYZ(Dest: LPcmsCIEXYZ);
begin
  while (Dest^.X > 2) and
    (Dest^.Y > 2) and
    (Dest^.Z > 2) do
  begin

    Dest^.X := Dest^.X / 10;
    Dest^.Y := Dest^.Y / 10;
    Dest^.Z := Dest^.Z / 10;
  end;
end;

function CreateICCProfileHandler(ICCfile: TStream; Read: TReadFunction; Seek: TSeekFunction; Tell: TTellFunction; Close: TCloseFunction): LPLCMSICCPROFILE;
var
  Icc: LPLCMSICCPROFILE;
  Tag: icTag;
  Header: icHeader;
  TagCount, i: icInt32Number;
begin
  result := nil;
  Icc := ICCAllocStruct(Read, Seek, Tell, Close);
  if (Icc = nil) then
  begin
    result := nil;
    exit;
  end;

  Icc^.stream := ICCfile;
  Icc^.Read(@Header, sizeof(icHeader), 1, ICCfile);

  AdjustEndianess32(@Header.size);
  AdjustEndianess32(@Header.cmmId);
  AdjustEndianess32(@Header.version);
  AdjustEndianess32(@Header.deviceClass);
  AdjustEndianess32(@Header.colorSpace);
  AdjustEndianess32(@Header.pcs);
  AdjustEndianess32(@Header.magic);
  AdjustEndianess32(@Header.flags);
  AdjustEndianess32(@Header.renderingIntent);

  try
    if (Header.magic <> icMagicNumber) then
      exit;
    if (Icc^.Read(@TagCount, sizeof(icInt32Number), 1, ICCfile) <> 1) then
      exit;

    AdjustEndianess32(@TagCount);

    Icc^.DeviceClass := Header.deviceClass;
    Icc^.ColorSpace := Header.colorSpace;
    Icc^.PCS := Header.pcs;
    Icc^.RenderingIntent := icRenderingIntent(Header.renderingIntent);
    Icc^.flags := Header.flags;
    Icc^.Illuminant.X := Convert15Fixed16(Header.illuminant.X);
    Icc^.Illuminant.Y := Convert15Fixed16(Header.illuminant.Y);
    Icc^.Illuminant.Z := Convert15Fixed16(Header.illuminant.Z);
    Icc^.Version := Header.version;

    Icc^.Illuminant := cmsD50_XYZ^;

    CopyMemory(@Icc^.ProfileID[0], @Header.reserved[0], 16);

    NormalizeXYZ(@Icc^.Illuminant);

    Icc^.TagCount := TagCount;
    for i := 0 to TagCount - 1 do
    begin
      Icc^.Read(@Tag, sizeof(icTag), 1, ICCfile);

      AdjustEndianess32(@Tag.offset);
      AdjustEndianess32(@Tag.size);
      AdjustEndianess32(@Tag.sig);

      Icc^.TagNames[i] := Tag.sig;
      Icc^.TagOffsets[i] := Tag.offset;
      Icc^.TagSizes[i] := Tag.size;
    end;

    result := Icc;

  finally
    if result = nil then
    begin
      Icc^.Close(ICCfile);
      freemem(Icc);
    end;
  end;
end;

// Does search for a specific tag in tag dictionary
// Returns position or -1 if tag not found

function SearchTag(Profile: LPLCMSICCPROFILE; sig: icTagSignature): icInt32Number;
var
  i: icInt32Number;
begin
  if (integer(sig) = 0) then
  begin
    result := -1;
    exit;
  end;

  for i := 0 to Profile^.TagCount - 1 do
  begin
    if (sig = Profile^.TagNames[i]) then
    begin
      result := i;
      exit;
    end;
  end;

  result := -1;
end;

function ReadICCXYZ(hProfile: cmsHPROFILE; sig: icTagSignature; Value: LPcmsCIEXYZ; lIsFatal: longbool): integer;
var
  Icc: LPLCMSICCPROFILE;
  Base: icTagBase;
  offset: integer;
  n: integer;
  XYZ: icXYZNumber;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  n := SearchTag(Icc, sig);
  if (n < 0) then
  begin
    result := -1;
    exit;
  end;

  if (Icc^.stream = nil) then
  begin
    CopyMemory(Value, Icc^.TagPtrs[n], Icc^.TagSizes[n]);
    result := Icc^.TagSizes[n];
    exit;
  end;

  offset := Icc^.TagOffsets[n];

  if (Icc^.Seek(Icc^.stream, offset)) then
  begin
    result := -1;
    exit;
  end;

  Icc^.Read(@Base, 1, sizeof(icTagBase), Icc^.stream);
  AdjustEndianess32(@Base.sig);

  case Base.sig of
    icTagTypeSignature($7C3B10C),
      icSigXYZType:
      begin
        Icc^.Read(@XYZ, sizeof(icXYZNumber), 1, Icc^.stream);
        Value^.X := Convert15Fixed16(XYZ.X);
        Value^.Y := Convert15Fixed16(XYZ.Y);
        Value^.Z := Convert15Fixed16(XYZ.Z);
      end;
  else
    if (lIsFatal) then
    begin
      //raise Exception.Create('Bad tag signature '+IEIntToStr(Base.sig)+' found.');
      result := -1;
      exit;
    end;
  end;
  result := 1;
end;

// Read a icSigS15Fixed16ArrayType (currently only a 3x3 matrix)

function ReadICCXYZArray(hProfile: cmsHPROFILE; sig: icTagSignature; v: LPMAT3): integer;
var
  Icc: LPLCMSICCPROFILE;
  Base: icTagBase;
  offset, sz: integer;
  i, n: integer;
  XYZ: array[0..2] of icXYZNumber;
  XYZdbl: array[0..2] of cmsCIEXYZ;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  n := SearchTag(Icc, sig);
  if (n < 0) then
  begin
    result := -1;
    exit;
  end;

  if (Icc^.stream = nil) then
  begin

    CopyMemory(v, Icc^.TagPtrs[n], Icc^.TagSizes[n]);

    result := Icc^.TagSizes[n];
    exit;
  end;

  offset := Icc^.TagOffsets[n];

  if (Icc^.Seek(Icc^.stream, offset)) then
  begin
    result := -1;
    exit;
  end;

  Icc^.Read(@Base, 1, sizeof(icTagBase), Icc^.stream);
  AdjustEndianess32(@Base.sig);

  case (Base.sig) of

    icSigS15Fixed16ArrayType:
      begin

        sz := Icc^.TagSizes[n] div sizeof(icXYZNumber);

        if (sz <> 3) then
        begin
          //cmsSignalError(LCMS_ERRC_ABORTED, "Bad array size of %d entries.", sz);
          result := -1;
          exit;
        end;

        Icc^.Read(@XYZ[0], sizeof(icXYZNumber), 3, Icc^.stream);

        for i := 0 to 2 do
        begin

          XYZdbl[i].X := Convert15Fixed16(XYZ[i].X);
          XYZdbl[i].Y := Convert15Fixed16(XYZ[i].Y);
          XYZdbl[i].Z := Convert15Fixed16(XYZ[i].Z);
        end;

        CopyMemory(v, @XYZdbl[0], 3 * sizeof(cmsCIEXYZ));
      end;

  else
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", Base.sig);
      result := -1;
      exit;
    end;

  end;

  result := sizeof(MAT3);
end;

// linear transform

procedure MAT3eval(r: LPVEC3; a: LPMAT3; v: LPVEC3);
begin
  r^.n[VX] := a^.v[0].n[VX] * v^.n[VX] + a^.v[0].n[VY] * v^.n[VY] + a^.v[0].n[VZ] * v^.n[VZ];
  r^.n[VY] := a^.v[1].n[VX] * v^.n[VX] + a^.v[1].n[VY] * v^.n[VY] + a^.v[1].n[VZ] * v^.n[VZ];
  r^.n[VZ] := a^.v[2].n[VX] * v^.n[VX] + a^.v[2].n[VY] * v^.n[VY] + a^.v[2].n[VZ] * v^.n[VZ];
end;

// Evaluates a XYZ tristimulous across chromatic adaptation matrix

procedure EvalCHRM(Dest: LPcmsCIEXYZ; Chrm: LPMAT3; Src: LPcmsCIEXYZ);
var
  d, s: VEC3;
begin
  s.n[VX] := Src^.X;
  s.n[VY] := Src^.Y;
  s.n[VZ] := Src^.Z;

  MAT3eval(@d, Chrm, @s);

  Dest^.X := d.n[VX];
  Dest^.Y := d.n[VY];
  Dest^.Z := d.n[VZ];
end;

function ROWCOL(a, b: LPMAT3; i, j: integer): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := a^.v[i].n[0] * b^.v[0].n[j] + a^.v[i].n[1] * b^.v[1].n[j] + a^.v[i].n[2] * b^.v[2].n[j]
end;

// Multiply two matrices

procedure MAT3per(r: LPMAT3; a: LPMAT3; b: LPMAT3);
begin
  VEC3init(@r^.v[0], ROWCOL(a, b, 0, 0), ROWCOL(a, b, 0, 1), ROWCOL(a, b, 0, 2));
  VEC3init(@r^.v[1], ROWCOL(a, b, 1, 0), ROWCOL(a, b, 1, 1), ROWCOL(a, b, 1, 2));
  VEC3init(@r^.v[2], ROWCOL(a, b, 2, 0), ROWCOL(a, b, 2, 1), ROWCOL(a, b, 2, 2));
  (*
  with a^.v[0] do
    VEC3init(@r^.v[0], n[0] * b^.v[0].n[0] + n[1] * b^.v[1].n[0] + n[2] * b^.v[2].n[0],
                       n[0] * b^.v[0].n[1] + n[1] * b^.v[1].n[1] + n[2] * b^.v[2].n[1],
                       n[0] * b^.v[0].n[2] + n[1] * b^.v[1].n[2] + n[2] * b^.v[2].n[2]);
  with a^.v[1] do
    VEC3init(@r^.v[1], n[0] * b^.v[0].n[0] + n[1] * b^.v[1].n[0] + n[2] * b^.v[2].n[0],
                       n[0] * b^.v[0].n[1] + n[1] * b^.v[1].n[1] + n[2] * b^.v[2].n[1],
                       n[0] * b^.v[0].n[2] + n[1] * b^.v[1].n[2] + n[2] * b^.v[2].n[2]);
  with a^.v[2] do
    VEC3init(@r^.v[2], n[0] * b^.v[0].n[0] + n[1] * b^.v[1].n[0] + n[2] * b^.v[2].n[0],
                       n[0] * b^.v[0].n[1] + n[1] * b^.v[1].n[1] + n[2] * b^.v[2].n[1],
                       n[0] * b^.v[0].n[2] + n[1] * b^.v[1].n[2] + n[2] * b^.v[2].n[2]);
                       *)
end;

// Compute chromatic adaptation matrix using Chad as cone matrix

procedure ComputeChromaticAdaptation(Conversion: LPMAT3; SourceWhitePoint: LPcmsCIEXYZ; DestWhitePoint: LPcmsCIEXYZ; Chad: LPMAT3);
var
  Chad_Inv: MAT3;
  ConeSourceXYZ, ConeSourceRGB: VEC3;
  ConeDestXYZ, ConeDestRGB: VEC3;
  Cone, Tmp: MAT3;
begin
  Tmp := Chad^;
  MAT3inverse(@Tmp, @Chad_Inv);

  VEC3init(@ConeSourceXYZ, SourceWhitePoint^.X,
    SourceWhitePoint^.Y,
    SourceWhitePoint^.Z);

  VEC3init(@ConeDestXYZ, DestWhitePoint^.X,
    DestWhitePoint^.Y,
    DestWhitePoint^.Z);

  MAT3eval(@ConeSourceRGB, Chad, @ConeSourceXYZ);
  MAT3eval(@ConeDestRGB, Chad, @ConeDestXYZ);

  VEC3init(@Cone.v[0], ConeDestRGB.n[0] / ConeSourceRGB.n[0], 0.0, 0.0);
  VEC3init(@Cone.v[1], 0.0, ConeDestRGB.n[1] / ConeSourceRGB.n[1], 0.0);
  VEC3init(@Cone.v[2], 0.0, 0.0, ConeDestRGB.n[2] / ConeSourceRGB.n[2]);

  MAT3per(@Tmp, @Cone, Chad);
  MAT3per(Conversion, @Chad_Inv, @Tmp);
end;

// Returns the final chrmatic adaptation from illuminant FromIll to Illuminant ToIll
// The cone matrix can be specified in ConeMatrix. If NULL, Bradford is assumed

function cmsAdaptationMatrix(r: LPMAT3; ConeMatrix: LPMAT3; FromIll: LPcmsCIEXYZ; ToIll: LPcmsCIEXYZ): longbool;
const
  LamRigg: MAT3 = (
    v: ((n: (0.8951, 0.2664, -0.1614)),
    (n: (-0.7502, 1.7135, 0.0367)),
    (n: (0.0389, -0.0685, 1.0296)))
    );
begin
  if (ConeMatrix = nil) then
    ConeMatrix := @LamRigg;
  ComputeChromaticAdaptation(r, FromIll, ToIll, ConeMatrix);
  result := true;
end;

procedure ReadCriticalTags(Icc: LPLCMSICCPROFILE);
const
  Brfd: MAT3 = (
    v: ((n: (0.8951, 0.2664, -0.1614)),
    (n: (-0.7502, 1.7135, 0.0367)),
    (n: (0.0389, -0.0685, 1.0296)))
    );
var
  hProfile: cmsHPROFILE;
  ChrmCanonical: MAT3;
begin
  hProfile := cmsHPROFILE(Icc);

  if (Icc^.Version >= $4000000) then
  begin
    if (ReadICCXYZ(hProfile, icSigMediaWhitePointTag, @Icc^.MediaWhitePoint, FALSE) < 0) then
    begin
      Icc^.MediaWhitePoint := cmsD50_XYZ()^;
    end;

    if (ReadICCXYZ(hProfile, icSigMediaBlackPointTag, @Icc^.MediaBlackPoint, FALSE) < 0) then
    begin
      Icc^.MediaBlackPoint.X := 0;
      Icc^.MediaBlackPoint.Y := 0;
      Icc^.MediaBlackPoint.X := 0;
    end;

    NormalizeXYZ(@Icc^.MediaWhitePoint);
    NormalizeXYZ(@Icc^.MediaBlackPoint);

    if (ReadICCXYZArray(hProfile, icTagSignature(icSigChromaticAdaptationTag), @ChrmCanonical) > 0) then
    begin
      MAT3inverse(@ChrmCanonical, @Icc^.ChromaticAdaptation);
    end
    else
    begin
      MAT3identity(@Icc^.ChromaticAdaptation);
    end;

    EvalCHRM(@Icc^.MediaWhitePoint, @Icc^.ChromaticAdaptation, @Icc^.MediaWhitePoint);
    EvalCHRM(@Icc^.MediaBlackPoint, @Icc^.ChromaticAdaptation, @Icc^.MediaBlackPoint);

  end
  else
  begin

    if (ReadICCXYZ(hProfile, icSigMediaWhitePointTag, @Icc^.MediaWhitePoint, FALSE) < 0) then
    begin
      Icc^.MediaWhitePoint := cmsD50_XYZ()^;
    end;

    if (ReadICCXYZ(hProfile, icSigMediaBlackPointTag, @Icc^.MediaBlackPoint, FALSE) < 0) then
    begin
      Icc^.MediaBlackPoint.X := 0;
      Icc^.MediaBlackPoint.Y := 0;
      Icc^.MediaBlackPoint.X := 0;
    end;

    NormalizeXYZ(@Icc^.MediaWhitePoint);
    NormalizeXYZ(@Icc^.MediaBlackPoint);

    cmsAdaptationMatrix(@Icc^.ChromaticAdaptation, @Brfd, @Icc^.Illuminant, @Icc^.MediaWhitePoint);

  end;

end;

// Create profile from disk file

function IEcmsOpenProfileFromFile(stream: TStream; save: boolean; save8bit: boolean): cmsHPROFILE;
var
  NewIcc: LPLCMSICCPROFILE;
  hEmpty: cmsHPROFILE;
begin

  if save then
  begin

    hEmpty := _cmsCreateProfilePlaceholder();
    NewIcc := LPLCMSICCPROFILE(hEmpty);
    NewIcc^.IsWrite := TRUE;
    NewIcc^.PhysicalFile := stream;

    if save8bit then
      NewIcc^.SaveAs8Bits := TRUE;
    result := hEmpty;
    exit;
  end;

  NewIcc := CreateICCProfileHandler(stream, IEFileRead, IEFileSeek, IEFileTell, IEFileClose);
  if (NewIcc = nil) then
  begin
    result := nil;
    exit;
    //raise Exception.Create('Corrupted profile');
  end;

  ReadCriticalTags(NewIcc);

  NewIcc^.PhysicalFile := stream;
  NewIcc^.IsWrite := FALSE;
  result := cmsHPROFILE(NewIcc);
end;

// Auxiliary: allocate transform struct and set to defaults

function AllocEmptyTransform: _LPcmsTRANSFORM;
var
  p: _LPcmsTRANSFORM;
begin

  getmem(p, sizeof(_cmsTRANSFORM));
  if (p = nil) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "cmsCreateTransform: malloc() failed");
    result := nil;
    exit;
  end;
  ZeroMemory(p, sizeof(_cmsTRANSFORM));

  p^.xform := nil;
  p^.Intent := INTENT_PERCEPTUAL;
  p^.ProofIntent := INTENT_ABSOLUTE_COLORIMETRIC;
  p^.DoGamutCheck := 0;
  p^.InputProfile := nil;
  p^.OutputProfile := nil;
  p^.PreviewProfile := nil;
  p^.Preview := nil;
  p^.Gamut := nil;
  p^.DeviceLink := nil;
  p^.InMatShaper := nil;
  p^.OutMatShaper := nil;
  p^.SmeltMatShaper := nil;
  p^.NamedColorList := nil;
  p^.EntryColorSpace := icColorSpaceSignature(0);
  p^.ExitColorSpace := icColorSpaceSignature(0);

  result := p;
end;

function _cmsEndPointsBySpace(Space: icColorSpaceSignature; var White: pword; var Black: pword; nOutputs: pinteger): longbool;
const
  RGBblack: array[0..3] of word = (0, 0, 0, 0);
  RGBwhite: array[0..3] of word = ($FFFF, $FFFF, $FFFF, 0);
  CMYKblack: array[0..3] of word = ($FFFF, $FFFF, $FFFF, $FFFF);
  CMYKwhite: array[0..3] of word = (0, 0, 0, 0);
  LABblack: array[0..3] of word = (0, $8000, $8000, 0);
  LABwhite: array[0..3] of word = ($FF00, $8000, $8000, 0);
  CMYblack: array[0..3] of word = ($FFFF, $FFFF, $FFFF, 0);
  CMYwhite: array[0..3] of word = (0, 0, 0, 0);
begin
  case (Space) of

    icSigRgbData:
      begin
        White := @RGBwhite;
        Black := @RGBblack;
        nOutputs^ := 3;
        result := true;
        exit;
      end;

    icSigLabData:
      begin
        White := @LABwhite;
        Black := @LABblack;
        nOutputs^ := 3;
        result := true;
        exit;
      end;

    icSigCmykData:
      begin
        White := @CMYKwhite;
        Black := @CMYKblack;
        nOutputs^ := 4;
        result := true;
        exit;
      end;

    icSigCmyData:
      begin
        White := @CMYwhite;
        Black := @CMYblack;
        nOutputs^ := 3;
        result := true;
        exit;
      end;
  end;

  result := false;
end;

function _cmsWhiteBySpace(Space: icColorSpaceSignature): PWORD;
const
  Default: array[0..MAXCHANNELS - 1] of word = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
var
  White, Black: pword;
  Dummy: integer;
begin
  White := nil;
  Black := nil;
  if (_cmsEndPointsBySpace(Space, White, Black, @Dummy)) then
    result := White
  else
    result := @Default;
end;

function cmsGetColorSpace(hProfile: cmsHPROFILE): icColorSpaceSignature;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);
  result := Icc^.ColorSpace;
end;

procedure COPY_3CHANS(xto, xfrom: pwordarray); {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  xto[0] := xfrom[0];
  xto[1] := xfrom[1];
  xto[2] := xfrom[2];
end;

function FROM_V2_TO_V4(x: word): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((((x) shl 8) + (x)) + $80) shr 8);
end;

function FROM_V4_TO_V2(x: word): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := ((((x) shl 8) + $80) div 257);
end;

function ToFixedDomain(a: integer): Fixed32; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := a + ((a + $7FFF) div $FFFF);
end;

function FromFixedDomain(a: Fixed32): integer; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := a - ((a + $7FFF) shr 16);
end;

(*
function FixedMul(a: Fixed32; b: Fixed32): Fixed32;
begin
  result := round(((a / 65536) * (b / 65536)) * 65536 );
end;
*)

procedure MAT3evalW(r: LPWVEC3; a: LPWMAT3; v: LPWVEC3);
var
  o1, o2, o3: double;
begin
  with v^ do
  begin
    o1 := n[0] / 65536;
    o2 := n[1] / 65536;
    o3 := n[2] / 65536;
  end;
  with a^.v[0] do
    r^.n[VX] := round(((n[0] / 65536) * o1) * 65536 ) +
                round(((n[1] / 65536) * o2) * 65536 ) +
                round(((n[2] / 65536) * o3) * 65536 );

  with a^.v[1] do
    r^.n[VY] := round(((n[0] / 65536) * o1) * 65536 ) +
                round(((n[1] / 65536) * o2) * 65536 ) +
                round(((n[2] / 65536) * o3) * 65536 );

  with a^.v[2] do
    r^.n[VZ] := round(((n[0] / 65536) * o1) * 65536 ) +
                round(((n[1] / 65536) * o2) * 65536 ) +
                round(((n[2] / 65536) * o3) * 65536 );

  (*
  r^.n[VX] := FixedMul(a^.v[0].n[0], v^.n[0]) +
              FixedMul(a^.v[0].n[1], v^.n[1]) +
              FixedMul(a^.v[0].n[2], v^.n[2]);

  r^.n[VY] := FixedMul(a^.v[1].n[0], v^.n[0]) +
              FixedMul(a^.v[1].n[1], v^.n[1]) +
              FixedMul(a^.v[1].n[2], v^.n[2]);

  r^.n[VZ] := FixedMul(a^.v[2].n[0], v^.n[0]) +
              FixedMul(a^.v[2].n[1], v^.n[1]) +
              FixedMul(a^.v[2].n[2], v^.n[2]);
  *)
end;

function Clamp_RGB(xin: integer): word; 
begin
  if (xin < 0) then
  begin
    result := 0;
    exit;
  end;
  if (xin > $FFFF) then
    result := $FFFF
  else
    result := xin;
end;

function cmsLinearInterpLUT16(Value: WORD; LutTable: pwordarray; p: LPL16PARAMS): word;
var
  y1, y0: double;
  y: double;
  val2, rest: double;
  cell0, cell1: integer;
begin
  if (Value = $FFFF) then
  begin
    result := LutTable[p^.Domain];
    exit;
  end;

  val2 := p^.Domain * (Value / 65535.0);

  cell0 := iefloor(val2);
  cell1 := ieceil(val2);

  rest := val2 - cell0;

  y0 := LutTable[cell0];
  y1 := LutTable[cell1];

  y := y0 + (y1 - y0) * rest;

  result := iefloor(y + 0.5);
end;

procedure cmsEvalLUT(Lut: LPLUT; xIn: pwordarray; xOut: pwordarray);
var
  i: dword;
  StageABC, StageLMN: array[0..MAXCHANNELS - 1] of WORD;
  InVect, OutVect: WVEC3;
begin
  (*
  for i := 0 to Lut ^. InputChan-1 do
    StageABC[i] := xIn[i];

  if (Lut ^.wFlags and LUT_V4_OUTPUT_EMULATE_V2)<>0 then
  begin

    StageABC[0] := FROM_V2_TO_V4(StageABC[0]);
    StageABC[1] := FROM_V2_TO_V4(StageABC[1]);
    StageABC[2] := FROM_V2_TO_V4(StageABC[2]);

  end;

  if (Lut ^.wFlags and LUT_V2_OUTPUT_EMULATE_V4)<>0 then
  begin

      StageABC[0] := FROM_V4_TO_V2(StageABC[0]);
      StageABC[1] := FROM_V4_TO_V2(StageABC[1]);
      StageABC[2] := FROM_V4_TO_V2(StageABC[2]);
  end;

  if (Lut ^. wFlags and LUT_HASMATRIX)<>0 then
  begin

    InVect.n[VX] := ToFixedDomain(StageABC[0]);
    InVect.n[VY] := ToFixedDomain(StageABC[1]);
    InVect.n[VZ] := ToFixedDomain(StageABC[2]);

    MAT3evalW(@OutVect, @Lut ^. Matrix, @InVect);

    StageABC[0] := Clamp_RGB(FromFixedDomain(OutVect.n[VX]));
    StageABC[1] := Clamp_RGB(FromFixedDomain(OutVect.n[VY]));
    StageABC[2] := Clamp_RGB(FromFixedDomain(OutVect.n[VZ]));
  end;

  if (Lut ^. wFlags and LUT_HASTL1)<>0 then
  begin

    for i := 0 to Lut ^. InputChan-1 do
      StageABC[i] := cmsLinearInterpLUT16(StageABC[i], 
                                                   PWordArray(Lut ^. L1[i]), 
                                                   @Lut ^. In16params);
  end;

  if (Lut ^.wFlags and LUT_HASMATRIX3)<>0 then
  begin

    InVect.n[VX] := ToFixedDomain(StageABC[0]);
    InVect.n[VY] := ToFixedDomain(StageABC[1]);
    InVect.n[VZ] := ToFixedDomain(StageABC[2]);

    MAT3evalW(@OutVect, @Lut ^. Mat3, @InVect);

    OutVect.n[VX] := OutVect.n[VX] + Lut ^.Ofs3.n[VX];
    OutVect.n[VY] := OutVect.n[VY] + Lut ^.Ofs3.n[VY];
    OutVect.n[VZ] := OutVect.n[VZ] + Lut ^.Ofs3.n[VZ];

    StageABC[0] := Clamp_RGB(FromFixedDomain(OutVect.n[VX]));
    StageABC[1] := Clamp_RGB(FromFixedDomain(OutVect.n[VY]));
    StageABC[2] := Clamp_RGB(FromFixedDomain(OutVect.n[VZ]));

  end;

  if (Lut ^.wFlags and LUT_HASTL3)<>0 then
  begin

    for i := 0 to Lut ^. InputChan-1 do
      StageABC[i] := cmsLinearInterpLUT16(StageABC[i], 
                                                   pwordarray(Lut ^. L3[i]), 
                                                   @Lut ^. L3params);

  end;

  if (Lut ^. wFlags and LUT_HAS3DGRID)<>0 then
  begin

    Lut ^.CLut16params.Interp3D(@StageABC, @StageLMN, pwordarray(Lut ^. T), @Lut ^. CLut16params);

  end
  else
  begin

    for i := 0 to Lut ^. InputChan-1 do
      StageLMN[i] := StageABC[i];

  end;

  if (Lut ^.wFlags and LUT_HASTL4)<>0 then
  begin

    for i := 0 to Lut ^. OutputChan-1 do
      StageLMN[i] := cmsLinearInterpLUT16(StageLMN[i], 
                                                   pwordarray(Lut ^. L4[i]), 
                                                   @Lut ^. L4params);
  end;

  if (Lut ^.wFlags and LUT_HASMATRIX4)<>0 then
  begin

    InVect.n[VX] := ToFixedDomain(StageLMN[0]);
    InVect.n[VY] := ToFixedDomain(StageLMN[1]);
    InVect.n[VZ] := ToFixedDomain(StageLMN[2]);

    MAT3evalW(@OutVect, @Lut ^. Mat4, @InVect);

    OutVect.n[VX] := OutVect.n[VX]+ Lut ^.Ofs4.n[VX];
    OutVect.n[VY] := OutVect.n[VY]+ Lut ^.Ofs4.n[VY];
    OutVect.n[VZ] := OutVect.n[VZ]+ Lut ^.Ofs4.n[VZ];

    StageLMN[0] := Clamp_RGB(FromFixedDomain(OutVect.n[VX]));
    StageLMN[1] := Clamp_RGB(FromFixedDomain(OutVect.n[VY]));
    StageLMN[2] := Clamp_RGB(FromFixedDomain(OutVect.n[VZ]));

  end;

  if (Lut ^. wFlags and LUT_HASTL2)<>0 then
  begin

    for i := 0 to Lut ^. OutputChan-1 do
      xOut[i] := cmsLinearInterpLUT16(StageLMN[i], 
                                                   pwordarray(Lut ^. L2[i]), 
                                                   @Lut ^. Out16params);
  end
  else
  begin
    for i := 0 to Lut ^. OutputChan-1 do
      xOut[i] := StageLMN[i];
  end;

  if (Lut ^.wFlags and LUT_V4_INPUT_EMULATE_V2)<>0 then
  begin

      xOut[0] := FROM_V4_TO_V2(xOut[0]);
      xOut[1] := FROM_V4_TO_V2(xOut[1]);
      xOut[2] := FROM_V4_TO_V2(xOut[2]);

  end;

  if (Lut ^.wFlags and LUT_V2_INPUT_EMULATE_V4)<>0 then
  begin

      xOut[0] := FROM_V2_TO_V4(xOut[0]);
      xOut[1] := FROM_V2_TO_V4(xOut[1]);
      xOut[2] := FROM_V2_TO_V4(xOut[2]);
  end;
  *)
  with Lut^ do
  begin
    for i := 0 to InputChan - 1 do
      StageABC[i] := xIn[i];

    if (wFlags and LUT_V4_OUTPUT_EMULATE_V2) <> 0 then
    begin

      StageABC[0] := FROM_V2_TO_V4(StageABC[0]);
      StageABC[1] := FROM_V2_TO_V4(StageABC[1]);
      StageABC[2] := FROM_V2_TO_V4(StageABC[2]);

    end;

    if (wFlags and LUT_V2_OUTPUT_EMULATE_V4) <> 0 then
    begin

      StageABC[0] := FROM_V4_TO_V2(StageABC[0]);
      StageABC[1] := FROM_V4_TO_V2(StageABC[1]);
      StageABC[2] := FROM_V4_TO_V2(StageABC[2]);
    end;

    if (wFlags and LUT_HASMATRIX) <> 0 then
    begin

      InVect.n[VX] := ToFixedDomain(StageABC[0]);
      InVect.n[VY] := ToFixedDomain(StageABC[1]);
      InVect.n[VZ] := ToFixedDomain(StageABC[2]);

      MAT3evalW(@OutVect, @Matrix, @InVect);

      StageABC[0] := Clamp_RGB(FromFixedDomain(OutVect.n[VX]));
      StageABC[1] := Clamp_RGB(FromFixedDomain(OutVect.n[VY]));
      StageABC[2] := Clamp_RGB(FromFixedDomain(OutVect.n[VZ]));
    end;

    if (wFlags and LUT_HASTL1) <> 0 then
    begin

      for i := 0 to InputChan - 1 do
        StageABC[i] := cmsLinearInterpLUT16(StageABC[i], 
          PWordArray(L1[i]), 
          @In16params);
    end;

    if (wFlags and LUT_HASMATRIX3) <> 0 then
    begin

      InVect.n[VX] := ToFixedDomain(StageABC[0]);
      InVect.n[VY] := ToFixedDomain(StageABC[1]);
      InVect.n[VZ] := ToFixedDomain(StageABC[2]);

      MAT3evalW(@OutVect, @Mat3, @InVect);

      OutVect.n[VX] := OutVect.n[VX] + Ofs3.n[VX];
      OutVect.n[VY] := OutVect.n[VY] + Ofs3.n[VY];
      OutVect.n[VZ] := OutVect.n[VZ] + Ofs3.n[VZ];

      StageABC[0] := Clamp_RGB(FromFixedDomain(OutVect.n[VX]));
      StageABC[1] := Clamp_RGB(FromFixedDomain(OutVect.n[VY]));
      StageABC[2] := Clamp_RGB(FromFixedDomain(OutVect.n[VZ]));

    end;

    if (wFlags and LUT_HASTL3) <> 0 then
    begin

      for i := 0 to InputChan - 1 do
        StageABC[i] := cmsLinearInterpLUT16(StageABC[i], 
          pwordarray(L3[i]), 
          @L3params);

    end;

    if (wFlags and LUT_HAS3DGRID) <> 0 then
    begin

      CLut16params.Interp3D(@StageABC, @StageLMN, pwordarray(T), @CLut16params);

    end
    else
    begin

      for i := 0 to InputChan - 1 do
        StageLMN[i] := StageABC[i];

    end;

    if (wFlags and LUT_HASTL4) <> 0 then
    begin

      for i := 0 to OutputChan - 1 do
        StageLMN[i] := cmsLinearInterpLUT16(StageLMN[i], 
          pwordarray(L4[i]), 
          @L4params);
    end;

    if (wFlags and LUT_HASMATRIX4) <> 0 then
    begin

      InVect.n[VX] := ToFixedDomain(StageLMN[0]);
      InVect.n[VY] := ToFixedDomain(StageLMN[1]);
      InVect.n[VZ] := ToFixedDomain(StageLMN[2]);

      MAT3evalW(@OutVect, @Mat4, @InVect);

      OutVect.n[VX] := OutVect.n[VX] + Ofs4.n[VX];
      OutVect.n[VY] := OutVect.n[VY] + Ofs4.n[VY];
      OutVect.n[VZ] := OutVect.n[VZ] + Ofs4.n[VZ];

      StageLMN[0] := Clamp_RGB(FromFixedDomain(OutVect.n[VX]));
      StageLMN[1] := Clamp_RGB(FromFixedDomain(OutVect.n[VY]));
      StageLMN[2] := Clamp_RGB(FromFixedDomain(OutVect.n[VZ]));

    end;

    if (wFlags and LUT_HASTL2) <> 0 then
    begin

      for i := 0 to OutputChan - 1 do
        xOut[i] := cmsLinearInterpLUT16(StageLMN[i], 
          pwordarray(L2[i]), 
          @Out16params);
    end
    else
    begin
      for i := 0 to OutputChan - 1 do
        xOut[i] := StageLMN[i];
    end;

    if (wFlags and LUT_V4_INPUT_EMULATE_V2) <> 0 then
    begin

      xOut[0] := FROM_V4_TO_V2(xOut[0]);
      xOut[1] := FROM_V4_TO_V2(xOut[1]);
      xOut[2] := FROM_V4_TO_V2(xOut[2]);

    end;

    if (wFlags and LUT_V2_INPUT_EMULATE_V4) <> 0 then
    begin

      xOut[0] := FROM_V2_TO_V4(xOut[0]);
      xOut[1] := FROM_V2_TO_V4(xOut[1]);
      xOut[2] := FROM_V2_TO_V4(xOut[2]);
    end;
  end;
end;

// This is the "normal" proofing transform

procedure NormalXFORM(p: _LPcmsTRANSFORM; xin: pointer; xout: pointer; Size: dword);
var
  accum: PBYTE;
  output: PBYTE;
  wIn, wOut: array[0..MAXCHANNELS - 1] of WORD;
  wStageABC, wPCS: array[0..2] of WORD;
  wStageLMN: array[0..MAXCHANNELS - 1] of WORD;
  wGamut: array[0..0] of WORD;
  i, n: dword;
  wPreview: array[0..2] of WORD;
begin

  accum := xin;
  output := xout;
  n := Size;

  for i := 0 to n - 1 do
  begin

    accum := p^.FromInput(p, @wIn, accum);

    p^.FromDevice(p, @wIn, @wStageABC);

    if (@p^.Stage1 <> nil) then
    begin

      p^.Stage1(@wStageABC, @wPCS, @p^.m1, @p^.of1);

      if (wPCS[0] = $FFFF) and (wPCS[1] = $FFFF) and (wPCS[2] = $FFFF) then
      begin

        output := p^.ToOutput(_LPcmsTRANSFORM(p), 
          pwordarray(_cmsWhiteBySpace(cmsGetColorSpace(p^.OutputProfile))), 
          output);
        continue;
      end;
    end
    else
      COPY_3CHANS(@wPCS, @wStageABC);

    if (p^.Gamut <> nil) then
    begin

      cmsEvalLUT(p^.Gamut, @wPCS, @wGamut);

      if (wGamut[0] >= 1) then
      begin

        wOut[0] := AlarmR;
        wOut[1] := AlarmG;
        wOut[2] := AlarmB;
        wOut[3] := 0;

        output := p^.ToOutput(_LPcmsTRANSFORM(p), @wOut, output);
        continue;
      end;
    end;

    if (p^.Preview <> nil) then
    begin

      cmsEvalLUT(p^.Preview, @wPCS, @wPreview);
      COPY_3CHANS(@wPCS, @wPreview);
    end;

    if (@p^.Stage2 <> nil) then
    begin
      p^.Stage2(@wPCS, @wStageLMN, @p^.m2, @p^.of2);
      if (wPCS[0] = $FFFF) and (wPCS[1] = $FFFF) and (wPCS[2] = $FFFF) then
      begin

        output := p^.ToOutput(_LPcmsTRANSFORM(p), 
          pwordarray(_cmsWhiteBySpace(cmsGetColorSpace(p^.OutputProfile))), 
          output);

        continue;
      end;

    end
    else
      COPY_3CHANS(@wStageLMN, @wPCS);

    p^.ToDevice(p, @wStageLMN, @wOut);

    output := p^.ToOutput(_LPcmsTRANSFORM(p), @wOut, output);
  end;
end;

function cmsGetDeviceClass(hProfile: cmsHPROFILE): icProfileClassSignature;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);
  result := Icc^.DeviceClass;
end;

(*
function CHANNELS_SH(c: dword): dword;
begin
  result := ((c) shl 3);
end;
*)

(*
function BYTES_SH(b: dword): dword;
begin
  result := (b);
end;
*)

(*
function TYPE_NAMED_COLOR_INDEX: dword;
begin
  result :=   (CHANNELS_SH(1) or BYTES_SH(2));
end;
*)

(*
function T_BYTES(b: dword): dword;
begin
  result := ((b) and 7);
end;
*)

function T_COLORSPACE(s: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((s) shr 16) and 31);
end;

function L2Fix4(L: double): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := round(L * 655.35 );
end;

function ab2Fix4(ab: double): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := round((ab + 128.0) * 257.0 );
end;

procedure cmsFloat2LabEncoded4(wLab: pwordarray; fLab: LPcmsCIELab);
var
  Lab: cmsCIELab;
begin

  Lab.L := fLab^.L;
  Lab.a := fLab^.a;
  Lab.b := fLab^.b;

  if (Lab.L < 0) then
    Lab.L := 0;
  if (Lab.L > 100) then
    Lab.L := 100;

  if (Lab.a < -128) then
    Lab.a := -128;
  if (Lab.a > 127) then
    Lab.a := 127;
  if (Lab.b < -128) then
    Lab.b := -128;
  if (Lab.b > 127) then
    Lab.b := 127;

  wLab[0] := L2Fix4(Lab.L);
  wLab[1] := ab2Fix4(Lab.a);
  wLab[2] := ab2Fix4(Lab.b);
end;

function L2Fix3(L: double): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := round(L * 652.800 );
end;

function ab2Fix3(ab: double): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := round((ab + 128.0) * 256.0 );
end;

procedure cmsFloat2LabEncoded(wLab: pwordarray; fLab: LPcmsCIELab);
var
  Lab: cmsCIELab;
begin

  Lab.L := fLab^.L;
  Lab.a := fLab^.a;
  Lab.b := fLab^.b;

  if (Lab.L < 0) then
    Lab.L := 0;
  if (Lab.L > 100) then
    Lab.L := 100;

  if (Lab.a < -128) then
    Lab.a := -128;
  if (Lab.a > 127.9961) then
    Lab.a := 127.9961;
  if (Lab.b < -128) then
    Lab.b := -128;
  if (Lab.b > 127.9961) then
    Lab.b := 127.9961;

  wLab[0] := L2Fix3(Lab.L);
  wLab[1] := ab2Fix3(Lab.a);
  wLab[2] := ab2Fix3(Lab.b);
end;

// floating point

function UnrollLabDouble(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
begin
  if (info^.lInputV4Lab) then
    cmsFloat2LabEncoded4(wIn, LPcmsCIELab(accum))
  else
    cmsFloat2LabEncoded(wIn, LPcmsCIELab(accum));

  inc(accum, sizeof(cmsCIELab));

  result := accum;
end;

// In XYZ All 3 components are encoded using 1.15 fixed point

function XYZ2Fix(d: double): WORD; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := iefloor(d * 32768.0 + 0.5);
end;

procedure cmsFloat2XYZEncoded(MXYZ: pwordarray; fXYZ: LPcmsCIEXYZ);
var
  xyz: cmsCIEXYZ;
begin

  xyz.X := fXYZ^.X;
  xyz.Y := fXYZ^.Y;
  xyz.Z := fXYZ^.Z;

  if (xyz.Y <= 0) then
  begin

    xyz.X := 0;
    xyz.Y := 0;
    xyz.Z := 0;
  end;

  if (xyz.X > 1.99996) then
    xyz.X := 1.99996;

  if (xyz.X < 0) then
    xyz.X := 0;

  if (xyz.Y > 1.99996) then
    xyz.Y := 1.99996;

  if (xyz.Y < 0) then
    xyz.Y := 0;

  if (xyz.Z > 1.99996) then
    xyz.Z := 1.99996;

  if (xyz.Z < 0) then
    xyz.Z := 0;

  MXYZ[0] := XYZ2Fix(xyz.X);
  MXYZ[1] := XYZ2Fix(xyz.Y);
  MXYZ[2] := XYZ2Fix(xyz.Z);

end;

function UnrollXYZDouble(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
begin
  cmsFloat2XYZEncoded(wIn, LPcmsCIEXYZ(accum));
  inc(accum, sizeof(cmsCIEXYZ));

  result := accum;
end;

function T_CHANNELS(c: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((c) shr 3) and 15);
end;

function T_EXTRA(e: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((e) shr 7) and 7);
end;

type
  tdoublearray = array[0..$EFFFFFF] of double;

  pdoublearray = ^tdoublearray;

  // Remaining cases are between 0..1.0

function UnrollDouble(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
var
  Inks: pdoublearray;
  nChan: integer;
  i: integer;
  v: double;
begin
  Inks := pdoublearray(accum);
  nChan := T_CHANNELS(info^.InputFormat);

  for i := 0 to nChan - 1 do
  begin

    v := iefloor(Inks[i] * 65535.0 + 0.5);

    if (v > 65535.0) then
      v := 65535.0;
    if (v < 0) then
      v := 0;

    wIn[i] := trunc(v);
  end;

  inc(accum, (nChan + T_EXTRA(info^.InputFormat)) * sizeof(double));
  result := accum;
end;

// Inks does come in percentage

function UnrollInkDouble(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
var
  Inks: pdoublearray;
  nChan, i: integer;
  v: double;
begin
  Inks := pdoublearray(accum);
  nChan := T_CHANNELS(info^.InputFormat);

  for i := 0 to nChan - 1 do
  begin

    v := iefloor(Inks[i] * 655.35 + 0.5);

    if (v > 65535.0) then
      v := 65535.0;
    if (v < 0) then
      v := 0;

    wIn[i] := trunc(v);
  end;

  inc(accum, (nChan + T_EXTRA(info^.InputFormat)) * sizeof(double));
  result := accum;
end;

function T_PLANAR(p: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((p) shr 12) and 1);
end;

function RGB_8_TO_16(rgb: word): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := ((((rgb)) shl 8) or (rgb));
end;

function UnrollPlanarBytes(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
var
  nChan: integer;
  i: integer;
  Init: pbyte;
begin
  nChan := T_CHANNELS(info^.InputFormat);
  Init := accum;

  for i := 0 to nChan - 1 do
  begin

    wIn[i] := RGB_8_TO_16(accum^);
    inc(accum, info^.StrideIn);
  end;
  inc(Init);
  result := Init;
end;

function T_ENDIAN16(e: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((e) shr 11) and 1);
end;

function CHANGE_ENDIAN(w: word): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((w) shl 8) or ((w) shr 8));
end;

function UnrollPlanarWordsBigEndian(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
var
  nChan: integer;
  i: integer;
  Init: pbyte;
begin
  nChan := T_CHANNELS(info^.InputFormat);
  Init := accum;

  for i := 0 to nChan - 1 do
  begin

    wIn[i] := CHANGE_ENDIAN(pword(accum)^);
    inc(accum, (info^.StrideIn * 2));
  end;
  inc(Init, 2);
  result := Init;
end;

function UnrollPlanarWords(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
var
  nChan: integer;
  i: integer;
  Init: pbyte;
begin
  nChan := T_CHANNELS(info^.InputFormat);
  Init := accum;

  for i := 0 to nChan - 1 do
  begin

    wIn[i] := pword(accum)^;
    inc(accum, (info^.StrideIn * 2));
  end;

  inc(Init, 2);
  result := Init;
end;

function T_FLAVOR(s: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((s) shr 13) and 1);
end;

function REVERSE_FLAVOR_16(x: word): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (($FFFF - (x)));
end;

function Unroll1ByteReversed(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := REVERSE_FLAVOR_16(RGB_8_TO_16(accum^));
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum);
  result := accum;
end;

// Monochrome duplicates L into RGB for null-transforms

function Unroll1Byte(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := RGB_8_TO_16(accum^);
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum);
  result := accum;
end;

function T_SWAPFIRST(s: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((s) shr 14) and 1);
end;

function Unroll2ByteSwapFirst(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
begin
  wIn[3] := RGB_8_TO_16(accum^);
  inc(accum);
  wIn[2] := RGB_8_TO_16(accum^);
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum);
  result := accum;
end;

// Monochrome + alpha. Alpha is lost

function Unroll2Byte(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
begin
  wIn[2] := RGB_8_TO_16(accum^);
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum);
  wIn[3] := RGB_8_TO_16(accum^);
  inc(accum);
  result := accum;
end;

function T_DOSWAP(e: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((e) shr 10) and 1);
end;

// BRG

function Unroll3BytesSwap(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin

  wIn[2] := RGB_8_TO_16(accum^);
  inc(accum);
  wIn[1] := RGB_8_TO_16(accum^);
  inc(accum);
  wIn[0] := RGB_8_TO_16(accum^);
  inc(accum);

  result := accum;
end;

function Unroll1ByteSkip2(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := RGB_8_TO_16(accum^);
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum);
  inc(accum, 2);
  result := accum;
end;

function Unroll3Bytes(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin

  wIn[0] := RGB_8_TO_16(accum^);
  inc(accum);
  wIn[1] := RGB_8_TO_16(accum^);
  inc(accum);
  wIn[2] := RGB_8_TO_16(accum^);
  inc(accum);

  result := accum;
end;

function Unroll4BytesSwapSwapFirst(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := RGB_8_TO_16(accum^);
  inc(accum); // K
  wIn[1] := RGB_8_TO_16(accum^);
  inc(accum); // Y
  wIn[0] := RGB_8_TO_16(accum^);
  inc(accum); // M
  wIn[3] := RGB_8_TO_16(accum^);
  inc(accum); // C

  result := accum;
end;

function Unroll4BytesSwapFirst(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin

  wIn[3] := RGB_8_TO_16(accum^);
  inc(accum); // K
  wIn[0] := RGB_8_TO_16(accum^);
  inc(accum); // C
  wIn[1] := RGB_8_TO_16(accum^);
  inc(accum); // M
  wIn[2] := RGB_8_TO_16(accum^);
  inc(accum); // Y

  result := accum;
end;

// KYMC

function Unroll4BytesSwap(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[3] := RGB_8_TO_16(accum^);
  inc(accum); // K
  wIn[2] := RGB_8_TO_16(accum^);
  inc(accum); // Y
  wIn[1] := RGB_8_TO_16(accum^);
  inc(accum); // M
  wIn[0] := RGB_8_TO_16(accum^);
  inc(accum); // C

  result := accum;
end;

function REVERSE_FLAVOR_8(x: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (($FF - (x)));
end;

function Unroll4BytesReverse(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
var
  i: word;
begin
  (*
  result := accum;
  wIn[0] := RGB_8_TO_16(($FF - result^));
  inc(result); // C
  wIn[1] := RGB_8_TO_16(($FF - result^));
  inc(result); // M
  wIn[2] := RGB_8_TO_16(($FF - result^));
  inc(result); // Y
  wIn[3] := RGB_8_TO_16(($FF - result^));
  inc(result); // K
  *)

  result := accum;
  i := $FF-result^;
  wIn[0] := ((i shl 8) or i);

  inc(result);
  i := $FF-result^;
  wIn[1] := ((i shl 8) or i);

  inc(result);
  i := $FF-result^;
  wIn[2] := ((i shl 8) or i);

  inc(result);
  i := $FF-result^;
  wIn[3] := ((i shl 8) or i);
  inc(result);

end;

function Unroll4Bytes(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
begin

  wIn[0] := RGB_8_TO_16(accum^);
  inc(accum); // C
  wIn[1] := RGB_8_TO_16(accum^);
  inc(accum); // M
  wIn[2] := RGB_8_TO_16(accum^);
  inc(accum); // Y
  wIn[3] := RGB_8_TO_16(accum^);
  inc(accum); // K

  result := accum;
end;

//result := ((( (rgb)) shl 8)or (rgb));

function UnrollAnyBytes(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
var
  nChan: integer;
  i: integer;
begin

  nChan := (info^.InputFormat shr 3) and 15; //nChan := T_CHANNELS(info ^. InputFormat);

  for i := 0 to nChan - 1 do
  begin

    wIn[i] := (accum^ shl 8) or accum^; // wIn[i] := RGB_8_TO_16(accum^)
    inc(accum);
  end;
  result := accum;
end;

function Unroll1WordBigEndian(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := CHANGE_ENDIAN(PWORD(accum)^);
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum, 2);
  result := accum;
end;

function Unroll1WordReversed(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := REVERSE_FLAVOR_16(PWORD(accum)^);
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum, 2);
  result := accum;
end;

function Unroll1Word(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := PWORD(accum)^;
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum, 2);
  result := accum;
end;

function Unroll2WordBigEndian(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := CHANGE_ENDIAN(PWORD(accum)^);
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum, 2);
  wIn[3] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2);

  result := accum;
end;

function Unroll2WordSwapFirst(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[3] := PWORD(accum)^;
  inc(accum, 2);
  wIn[2] := PWORD(accum)^;
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum, 2);

  result := accum;
end;

function Unroll2Word(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := PWORD(accum)^;
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];
  inc(accum, 2);
  wIn[3] := PWORD(accum)^;
  inc(accum, 2);

  result := accum;
end;

function Unroll3WordsSwapBigEndian(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2);
  wIn[1] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2);
  wIn[0] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2);
  result := accum;
end;

function Unroll3WordsSwap(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := PWORD(accum)^;
  inc(accum, 2);
  wIn[1] := PWORD(accum)^;
  inc(accum, 2);
  wIn[0] := PWORD(accum)^;
  inc(accum, 2);
  result := accum;
end;

function Unroll3WordsBigEndian(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[0] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2);
  wIn[1] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2);
  wIn[2] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2);
  result := accum;
end;

function Unroll3Words(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[0] := PWORD(accum)^;
  inc(accum, 2); // C R
  wIn[1] := PWORD(accum)^;
  inc(accum, 2); // M G
  wIn[2] := PWORD(accum)^;
  inc(accum, 2); // Y B
  result := accum;
end;

// KYMC

function Unroll4WordsSwapBigEndian(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[3] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2); //K
  wIn[2] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2); //Y
  wIn[1] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2); //M
  wIn[0] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2); //C

  result := accum;
end;

function Unroll4WordsSwapSwapFirst(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := PWORD(accum)^;
  inc(accum, 2); // K
  wIn[1] := PWORD(accum)^;
  inc(accum, 2); // Y
  wIn[0] := PWORD(accum)^;
  inc(accum, 2); // M
  wIn[3] := PWORD(accum)^;
  inc(accum, 2); // C

  result := accum;
end;

// KYMC

function Unroll4WordsSwap(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[3] := PWORD(accum)^;
  inc(accum, 2); // K
  wIn[2] := PWORD(accum)^;
  inc(accum, 2); // Y
  wIn[1] := PWORD(accum)^;
  inc(accum, 2); // M
  wIn[0] := PWORD(accum)^;
  inc(accum, 2); // C

  result := accum;
end;

function Unroll1WordSkip3(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[2] := accum^;
  wIn[1] := wIn[2];
  wIn[0] := wIn[1];

  inc(accum, 8);
  result := accum;
end;

function Unroll4WordsBigEndianReverse(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[0] := REVERSE_FLAVOR_16(CHANGE_ENDIAN(PWORD(accum)^));
  inc(accum, 2); //C
  wIn[1] := REVERSE_FLAVOR_16(CHANGE_ENDIAN(PWORD(accum)^));
  inc(accum, 2); //M
  wIn[2] := REVERSE_FLAVOR_16(CHANGE_ENDIAN(PWORD(accum)^));
  inc(accum, 2); //Y
  wIn[3] := REVERSE_FLAVOR_16(CHANGE_ENDIAN(PWORD(accum)^));
  inc(accum, 2); //K

  result := accum;
end;

function Unroll4WordsBigEndian(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[0] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2); //C
  wIn[1] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2); //M
  wIn[2] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2); //Y
  wIn[3] := CHANGE_ENDIAN(PWORD(accum)^);
  inc(accum, 2); //K

  result := accum;
end;

function Unroll4WordsSwapFirst(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[3] := PWORD(accum)^;
  inc(accum, 2); // K
  wIn[0] := PWORD(accum)^;
  inc(accum, 2); // C
  wIn[1] := PWORD(accum)^;
  inc(accum, 2); // M
  wIn[2] := PWORD(accum)^;
  inc(accum, 2); // Y

  result := accum;
end;

function Unroll4WordsReverse(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  wIn[0] := REVERSE_FLAVOR_16(PWORD(accum)^);
  inc(accum, 2); // C
  wIn[1] := REVERSE_FLAVOR_16(PWORD(accum)^);
  inc(accum, 2); // M
  wIn[2] := REVERSE_FLAVOR_16(PWORD(accum)^);
  inc(accum, 2); // Y
  wIn[3] := REVERSE_FLAVOR_16(PWORD(accum)^);
  inc(accum, 2); // K

  result := accum;
end;

function Unroll4Words(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte; 
begin
  result := accum;
  wIn[0] := PWORD(result)^;
  inc(result, 2); // C
  wIn[1] := PWORD(result)^;
  inc(result, 2); // M
  wIn[2] := PWORD(result)^;
  inc(result, 2); // Y
  wIn[3] := PWORD(result)^;
  inc(result, 2); // K

end;

function UnrollAnyWords(info: _LPcmsTRANSFORM; wIn: pwordarray; accum: pbyte): pbyte;
var
  nChan: integer;
  i: integer;
begin
  nChan := T_CHANNELS(info^.InputFormat);

  for i := 0 to nChan - 1 do
  begin

    wIn[i] := PWORD(accum)^;
    inc(accum, 2);
  end;

  result := accum;
end;

//  choose routine from Input identifier

function _cmsIdentifyInputFormat(xform: _LPcmsTRANSFORM; dwInput: DWORD): _cmsFIXFN;
var
  FromInput: _cmsFIXFN;
begin
  FromInput := nil;

  if (xform <> nil) then
  begin

    if (xform^.InputProfile <> nil) then
    begin

      if (cmsGetDeviceClass(xform^.InputProfile) = icSigNamedColorClass) then
      begin

        if (dwInput <> TYPE_NAMED_COLOR_INDEX) then
        begin
          //cmsSignalError(LCMS_ERRC_ABORTED, "Named color needs TYPE_NAMED_COLOR_INDEX");
          result := nil;
          exit;
        end;

      end;

    end;

  end;

  if ((dwInput and 7) = 0) then
  begin

    case (T_COLORSPACE(dwInput)) of

      PT_Lab:
        FromInput := UnrollLabDouble;
      PT_XYZ:
        FromInput := UnrollXYZDouble;

      PT_GRAY,
        PT_RGB,
        PT_YCbCr,
        PT_YUV,
        PT_YUVK,
        PT_HSV,
        PT_HLS,
        PT_Yxy:
        FromInput := UnrollDouble;

    else
      FromInput := UnrollInkDouble;

    end

  end
  else
  if (T_PLANAR(dwInput) <> 0) then
  begin

    case ((dwInput and 7)) of

      1:
        FromInput := UnrollPlanarBytes;

      2:
        if (T_ENDIAN16(dwInput) <> 0) then
          FromInput := UnrollPlanarWordsBigEndian
        else
          FromInput := UnrollPlanarWords;

    end
  end
  else
  begin

    case ((dwInput and 7)) of

      1:

        case (T_CHANNELS(dwInput) + T_EXTRA(dwInput)) of
          1: if (T_FLAVOR(dwInput) <> 0) then
              FromInput := Unroll1ByteReversed
            else
              FromInput := Unroll1Byte;

          2: if (T_SWAPFIRST(dwInput) <> 0) then
              FromInput := Unroll2ByteSwapFirst
            else
              FromInput := Unroll2Byte;

          3: if (T_DOSWAP(dwInput) <> 0) then
              FromInput := Unroll3BytesSwap
            else
            begin
              if (T_EXTRA(dwInput) = 2) then
                FromInput := Unroll1ByteSkip2
              else
                FromInput := Unroll3Bytes;
            end;

          4: if (T_DOSWAP(dwInput) <> 0) then
            begin
              if (T_SWAPFIRST(dwInput) <> 0) then
                FromInput := Unroll4BytesSwapSwapFirst
              else
                FromInput := Unroll4BytesSwap;
            end
            else
            begin
              if (T_SWAPFIRST(dwInput) <> 0) then
                FromInput := Unroll4BytesSwapFirst
              else
              begin
                if ((((dwInput) shr 13) and 1) <> 0) then
                  FromInput := Unroll4BytesReverse
                else
                  FromInput := Unroll4Bytes;
              end;
            end;

          5,
            6,
            7,
            8:
            if (T_DOSWAP(dwInput) = 0) and (T_SWAPFIRST(dwInput) = 0) then
              FromInput := UnrollAnyBytes;

        end;

      2:
        case (T_CHANNELS(dwInput) + T_EXTRA(dwInput)) of
          1: if (T_ENDIAN16(dwInput) <> 0) then
              FromInput := Unroll1WordBigEndian
            else
            if (T_FLAVOR(dwInput) <> 0) then
              FromInput := Unroll1WordReversed
            else
              FromInput := Unroll1Word;

          2: if (T_ENDIAN16(dwInput) <> 0) then
              FromInput := Unroll2WordBigEndian
            else
            begin
              if (T_SWAPFIRST(dwInput) <> 0) then
                FromInput := Unroll2WordSwapFirst
              else
                FromInput := Unroll2Word;
            end;

          3: if (T_DOSWAP(dwInput) <> 0) then
            begin
              if (T_ENDIAN16(dwInput) <> 0) then
                FromInput := Unroll3WordsSwapBigEndian
              else
                FromInput := Unroll3WordsSwap;
            end
            else
            begin
              if (T_ENDIAN16(dwInput) <> 0) then
                FromInput := Unroll3WordsBigEndian
              else
                FromInput := Unroll3Words;
            end;

          4: if (T_DOSWAP(dwInput) <> 0) then
            begin

              if (T_ENDIAN16(dwInput) <> 0) then
                FromInput := Unroll4WordsSwapBigEndian
              else
              begin
                if (T_SWAPFIRST(dwInput) <> 0) then
                  FromInput := Unroll4WordsSwapSwapFirst
                else
                  FromInput := Unroll4WordsSwap;
              end;
            end
            else
            begin

              if (T_EXTRA(dwInput) = 3) then
                FromInput := Unroll1WordSkip3
              else
                if (T_ENDIAN16(dwInput) <> 0) then
                begin

                  if (T_FLAVOR(dwInput) <> 0) then
                    FromInput := Unroll4WordsBigEndianReverse
                  else
                    FromInput := Unroll4WordsBigEndian;
                end
                else
                begin
                  if (T_SWAPFIRST(dwInput) <> 0) then
                    FromInput := Unroll4WordsSwapFirst
                  else
                  begin
                    if ((((dwInput) shr 13) and 1) <> 0) then
                      FromInput := Unroll4WordsReverse
                    else
                      FromInput := Unroll4Words;
                  end;
                end;
            end;

          5,
            6,
            7,
            8:
            if (T_DOSWAP(dwInput) = 0) and (T_SWAPFIRST(dwInput) = 0) then
              FromInput := UnrollAnyWords;

        end;
    end;
  end;

  //if (FromInput=nil) then;
    //cmsSignalError(LCMS_ERRC_ABORTED, "Unknown input format");

  result := FromInput;
end;

function L2float4(v: WORD): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
var
  fix32: Fixed32;
begin
  fix32 := v;
  result := fix32 / 655.35;
end;

function ab2float4(v: WORD): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
var
  fix32: Fixed32;
begin
  fix32 := v;
  result := (fix32 / 257.0) - 128.0;
end;

function L2float3(v: WORD): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
var
  fix32: Fixed32;
begin
  fix32 := v;
  result := fix32 / 652.800;
end;

procedure cmsLabEncoded2Float4(Lab: LPcmsCIELab; wLab: pwordarray);
begin
  Lab^.L := L2float4(wLab[0]);
  Lab^.a := ab2float4(wLab[1]);
  Lab^.b := ab2float4(wLab[2]);
end;

function ab2float3(v: WORD): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
var
  fix32: Fixed32;
begin
  fix32 := v;
  result := (fix32 / 256.0) - 128.0;
end;

procedure cmsLabEncoded2Float(Lab: LPcmsCIELab; wLab: pwordarray); 
begin
  Lab^.L := L2float3(wLab[0]);
  Lab^.a := ab2float3(wLab[1]);
  Lab^.b := ab2float3(wLab[2]);
end;

// Unencoded Float values -- don't try optimize speed

function PackLabDouble(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  if (Info^.lOutputV4Lab) then
    cmsLabEncoded2Float4(LPcmsCIELab(output), wOut)
  else
    cmsLabEncoded2Float(LPcmsCIELab(output), wOut);

  inc(output, sizeof(cmsCIELab));

  result := output;
end;

function XYZ2float(v: WORD): double; 
var
  fix32: Fixed32;
begin
  fix32 := v shl 1;
  result := FIXED_TO_DOUBLE(fix32);
end;

procedure cmsXYZEncoded2Float(fXYZ: LPcmsCIEXYZ; XYZ: pwordarray);
begin

  fXYZ^.X := XYZ2float(XYZ[0]);
  fXYZ^.Y := XYZ2float(XYZ[1]);
  fXYZ^.Z := XYZ2float(XYZ[2]);

end;

function PackXYZDouble(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte; 
begin
  cmsXYZEncoded2Float(LPcmsCIEXYZ(output), wOut);
  inc(output, sizeof(cmsCIEXYZ));
  result := output;
end;

function PackDouble(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  Inks: pdoublearray;
  i, nChan: integer;
begin
  Inks := pdoublearray(output);
  nChan := T_CHANNELS(Info^.OutputFormat);

  for i := 0 to nChan - 1 do
  begin

    Inks[i] := wOut[i] / 65535;
  end;

  inc(output, (nChan + T_EXTRA(Info^.OutputFormat)) * sizeof(double));
  result := output;
end;

function PackInkDouble(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  Inks: pdoublearray;
  i, nChan: integer;
begin
  Inks := pdoublearray(output);
  nChan := T_CHANNELS(Info^.OutputFormat);

  for i := 0 to nChan - 1 do
  begin

    Inks[i] := wOut[i] / 655.35;
  end;

  inc(output, (nChan + T_EXTRA(Info^.OutputFormat)) * sizeof(double));
  result := output;

end;

function RGB_16_TO_8(rgb: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := ((((rgb) * 65281 + 8388608) shr 24) and $FF);
end;

function PackPlanarBytes(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  nChan: integer;
  i: integer;
  Init: pbyte;
begin
  nChan := T_CHANNELS(info^.OutputFormat);
  Init := output;

  for i := 0 to nChan - 1 do
  begin

    output^ := RGB_16_TO_8(wOut[i]);
    inc(output, info^.StrideOut);

  end;
  inc(Init);
  result := Init;
end;

function PackPlanarWords(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  nChan: integer;
  i: integer;
  Init: pbyte;
begin
  nChan := T_CHANNELS(info^.OutputFormat);
  Init := output;

  for i := 0 to nChan - 1 do
  begin

    pword(output)^ := wOut[i];
    inc(output, (info^.StrideOut * sizeof(WORD)));
  end;
  inc(Init, 2);
  result := Init;

end;

function Pack1Byte(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte; 
begin
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  result := output;
end;

function Pack1ByteAndSkip1SwapFirst(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte; 
begin
  inc(output);
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  result := output;
end;

function Pack1ByteAndSkip1(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte; 
begin
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  inc(output);
  result := output;
end;

function Pack3BytesSwap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte; 
begin
  result := output;
  result^ := ((((wOut[2]) * 65281 + 8388608) shr 24) and $FF);
  inc(result);
  result^ := ((((wOut[1]) * 65281 + 8388608) shr 24) and $FF);
  inc(result);
  result^ := ((((wOut[0]) * 65281 + 8388608) shr 24) and $FF);
  inc(result);
end;

function Pack3Bytes(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte; 
begin
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);

  result := output;
end;

function Pack3BytesAndSkip1SwapSwapFirst(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte; 
begin
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  inc(output);

  result := output;
end;

function Pack3BytesAndSkip1SwapFirst(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  inc(output);
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);

  result := output;
end;

function Pack3BytesAndSkip1Swap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);

  result := output;
end;

function Pack3BytesAndSkip1(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);
  inc(output);

  result := output;
end;

function Pack4BytesSwapSwapFirst(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[3]);
  inc(output);

  result := output;
end;

function Pack4BytesSwap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  output^ := RGB_16_TO_8(wOut[3]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);

  result := output;
end;

function Pack4BytesSwapFirst(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  output^ := RGB_16_TO_8(wOut[3]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);

  result := output;
end;

function Pack4BytesReverse(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  output^ := REVERSE_FLAVOR_8(RGB_16_TO_8(wOut[0]));
  inc(output);
  output^ := REVERSE_FLAVOR_8(RGB_16_TO_8(wOut[1]));
  inc(output);
  output^ := REVERSE_FLAVOR_8(RGB_16_TO_8(wOut[2]));
  inc(output);
  output^ := REVERSE_FLAVOR_8(RGB_16_TO_8(wOut[3]));
  inc(output);

  result := output;
end;

function Pack4Bytes(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[3]);
  inc(output);

  result := output;
end;

function Pack6BytesSwap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  output^ := RGB_16_TO_8(wOut[3]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[4]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[5]);
  inc(output);

  result := output;
end;

function Pack6Bytes(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  output^ := RGB_16_TO_8(wOut[0]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[1]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[2]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[3]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[4]);
  inc(output);
  output^ := RGB_16_TO_8(wOut[5]);
  inc(output);

  result := output;
end;

function PackNBytesSwap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  i, nChan: integer;
begin
  nChan := T_CHANNELS(info^.OutputFormat);

  i := nChan - 1;
  while (i >= 0) do
  begin
    output^ := RGB_16_TO_8(wOut[i]);
    inc(output);
    dec(i);
  end;
  result := output;

end;

function PackNBytes(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  nChan, i: integer;
begin
  nChan := T_CHANNELS(info^.OutputFormat);

  for i := 0 to nChan - 1 do
  begin
    output^ := RGB_16_TO_8(wOut[i]);
    inc(output);
  end;
  result := output;
end;

function Pack1WordBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);

  result := output;
end;

function Pack1Word(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[0];
  inc(output, 2);

  result := output;
end;

function Pack1WordAndSkip1BigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 4);

  result := output;
end;

function Pack1WordAndSkip1SwapFirst(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  inc(output, 2);
  pword(output)^ := wOut[0];
  inc(output, 2);

  result := output;
end;

function Pack1WordAndSkip1(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[0];
  inc(output, 4);

  result := output;
end;

function Pack3WordsSwapBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[2]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[1]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);

  result := output;
end;

function Pack3WordsSwap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[2];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[0];
  inc(output, 2);

  result := output;
end;

function Pack3WordsBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[1]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[2]);
  inc(output, 2);

  result := output;
end;

function Pack3Words(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[0];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[2];
  inc(output, 2);

  result := output;
end;

function Pack3WordsAndSkip1SwapBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[2]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[1]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);

  result := output;
end;

function Pack3WordsAndSkip1SwapSwapFirst(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[2];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[0];
  inc(output, 2);
  inc(output, 2);

  result := output;
end;

function Pack3WordsAndSkip1Swap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  inc(output, 2);
  pword(output)^ := wOut[2];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[0];
  inc(output, 2);

  result := output;
end;

function Pack3WordsAndSkip1BigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[1]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[2]);
  inc(output, 2);
  inc(output, 2);

  result := output;
end;

function Pack3WordsAndSkip1(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[0];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[2];
  inc(output, 2);
  inc(output, 2);

  result := output;
end;

function Pack4WordsSwapBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[3]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[2]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[1]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);

  result := output;
end;

function Pack4WordsSwap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[3];
  inc(output, 2);
  pword(output)^ := wOut[2];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[0];
  inc(output, 2);

  result := output;
end;

function Pack4WordsBigEndianReverse(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(REVERSE_FLAVOR_16(wOut[0]));
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(REVERSE_FLAVOR_16(wOut[1]));
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(REVERSE_FLAVOR_16(wOut[2]));
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(REVERSE_FLAVOR_16(wOut[3]));
  inc(output, 2);

  result := output;
end;

function Pack4WordsBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[1]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[2]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[3]);
  inc(output, 2);

  result := output;
end;

function Pack4WordsReverse(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := REVERSE_FLAVOR_16(wOut[0]);
  inc(output, 2);
  pword(output)^ := REVERSE_FLAVOR_16(wOut[1]);
  inc(output, 2);
  pword(output)^ := REVERSE_FLAVOR_16(wOut[2]);
  inc(output, 2);
  pword(output)^ := REVERSE_FLAVOR_16(wOut[3]);
  inc(output, 2);

  result := output;
end;

function Pack4Words(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[0];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[2];
  inc(output, 2);
  pword(output)^ := wOut[3];
  inc(output, 2);

  result := output;
end;

function Pack6WordsSwapBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[3]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[1]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[2]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[4]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[5]);
  inc(output, 2);

  result := output;
end;

function Pack6WordsSwap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[3];
  inc(output, 2);
  pword(output)^ := wOut[0];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[2];
  inc(output, 2);
  pword(output)^ := wOut[4];
  inc(output, 2);
  pword(output)^ := wOut[5];
  inc(output, 2);

  result := output;
end;

function Pack6WordsBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := CHANGE_ENDIAN(wOut[0]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[1]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[2]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[3]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[4]);
  inc(output, 2);
  pword(output)^ := CHANGE_ENDIAN(wOut[5]);
  inc(output, 2);

  result := output;
end;

function Pack6Words(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
begin
  pword(output)^ := wOut[0];
  inc(output, 2);
  pword(output)^ := wOut[1];
  inc(output, 2);
  pword(output)^ := wOut[2];
  inc(output, 2);
  pword(output)^ := wOut[3];
  inc(output, 2);
  pword(output)^ := wOut[4];
  inc(output, 2);
  pword(output)^ := wOut[5];
  inc(output, 2);

  result := output;
end;

function PackNWordsSwapBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  nChan, i: integer;
begin
  nChan := T_CHANNELS(info^.OutputFormat);

  i := nChan - 1;
  while (i >= 0) do
  begin
    pword(output)^ := CHANGE_ENDIAN(wOut[i]);
    inc(output, sizeof(WORD));
    dec(i);
  end;
  result := output;
end;

function PackNWordsSwap(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  nChan, i: integer;
begin
  nChan := T_CHANNELS(info^.OutputFormat);

  i := nChan - 1;
  while (i >= 0) do
  begin
    pword(output)^ := wOut[i];
    inc(output, sizeof(WORD));
    dec(i);
  end;

  result := output;
end;

function PackNWordsBigEndian(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  nChan, i: integer;
begin
  nChan := T_CHANNELS(info^.OutputFormat);

  for i := 0 to nChan - 1 do
  begin
    pword(output)^ := CHANGE_ENDIAN(wOut[i]);
    inc(output, sizeof(WORD));
  end;
  result := output;
end;

function PackNWords(Info: _LPcmsTRANSFORM; wOut: pwordarray; output: pbyte): pbyte;
var
  nChan, i: integer;
begin
  nChan := T_CHANNELS(info^.OutputFormat);

  for i := 0 to nChan - 1 do
  begin
    pword(output)^ := wOut[i];
    inc(output, sizeof(WORD));
  end;

  result := output;
end;

//  choose routine from Input identifier

function _cmsIdentifyOutputFormat(xform: _LPcmsTRANSFORM; dwOutput: DWORD): _cmsFIXFN;
var
  ToOutput: _cmsFIXFN;
begin
  ToOutput := nil;

  if ((dwOutput and 7) = 0) then
  begin

    case (T_COLORSPACE(dwOutput)) of

      PT_Lab:
        ToOutput := PackLabDouble;
      PT_XYZ:
        ToOutput := PackXYZDouble;

      PT_GRAY,
        PT_RGB,
        PT_YCbCr,
        PT_YUV,
        PT_YUVK,
        PT_HSV,
        PT_HLS,
        PT_Yxy:
        ToOutput := PackDouble;

    else
      ToOutput := PackInkDouble;
    end;

  end
  else
  if (T_PLANAR(dwOutput) <> 0) then
  begin

    case ((dwOutput and 7)) of

      1: ToOutput := PackPlanarBytes;

      2: if (T_ENDIAN16(dwOutput) = 0) then
          ToOutput := PackPlanarWords;

    end;
  end
  else
  begin

    case ((dwOutput and 7)) of

      1:
        case (T_CHANNELS(dwOutput)) of
          1:
            begin
              ToOutput := Pack1Byte;
              if (T_EXTRA(dwOutput) = 1) then
              begin
                if (T_SWAPFIRST(dwOutput) <> 0) then
                  ToOutput := Pack1ByteAndSkip1SwapFirst
                else
                  ToOutput := Pack1ByteAndSkip1;
              end;
            end;

          3:
            case (T_EXTRA(dwOutput)) of

              0: if (T_DOSWAP(dwOutput) <> 0) then
                  ToOutput := Pack3BytesSwap
                else
                  ToOutput := Pack3Bytes;

              1: if (T_DOSWAP(dwOutput) <> 0) then
                begin

                  if (T_SWAPFIRST(dwOutput) <> 0) then
                    ToOutput := Pack3BytesAndSkip1SwapSwapFirst
                  else
                    ToOutput := Pack3BytesAndSkip1Swap;
                end
                else
                begin
                  if (T_SWAPFIRST(dwOutput) <> 0) then
                    ToOutput := Pack3BytesAndSkip1SwapFirst
                  else
                    ToOutput := Pack3BytesAndSkip1;
                end;
            end;

          4: if (T_EXTRA(dwOutput) = 0) then
            begin
              if (T_DOSWAP(dwOutput) <> 0) then
              begin
                if (T_SWAPFIRST(dwOutput) <> 0) then
                  ToOutput := Pack4BytesSwapSwapFirst
                else
                  ToOutput := Pack4BytesSwap;
              end
              else
              begin
                if (T_SWAPFIRST(dwOutput) <> 0) then
                  ToOutput := Pack4BytesSwapFirst
                else
                begin

                  if (T_FLAVOR(dwOutput) <> 0) then
                    ToOutput := Pack4BytesReverse
                  else
                    ToOutput := Pack4Bytes;
                end;
              end;
            end;

          6: if (T_EXTRA(dwOutput) = 0) then
            begin

              if (T_DOSWAP(dwOutput) <> 0) then
                ToOutput := Pack6BytesSwap
              else
                ToOutput := Pack6Bytes;
            end;

          5,
            7,
            8,
            9,
            10,
            11,
            12,
            13,
            14,
            15:

            if ((T_EXTRA(dwOutput) = 0) and (T_SWAPFIRST(dwOutput) = 0)) then
            begin

              if (T_DOSWAP(dwOutput) <> 0) then
                ToOutput := PackNBytesSwap
              else
                ToOutput := PackNBytes;
            end;

        end;

      2:

        case (T_CHANNELS(dwOutput)) of

          1:
            begin
              if (T_ENDIAN16(dwOutput) <> 0) then

                ToOutput := Pack1WordBigEndian
              else
                ToOutput := Pack1Word;

              if (T_EXTRA(dwOutput) = 1) then
              begin

                if (T_ENDIAN16(dwOutput) <> 0) then

                  ToOutput := Pack1WordAndSkip1BigEndian
                else
                begin
                  if (T_SWAPFIRST(dwOutput) <> 0) then
                    ToOutput := Pack1WordAndSkip1SwapFirst
                  else
                    ToOutput := Pack1WordAndSkip1;
                end;
              end;
            end;

          3:

            case (T_EXTRA(dwOutput)) of

              0:
                if (T_DOSWAP(dwOutput) <> 0) then
                begin

                  if (T_ENDIAN16(dwOutput) <> 0) then

                    ToOutput := Pack3WordsSwapBigEndian
                  else
                    ToOutput := Pack3WordsSwap;
                end
                else
                begin
                  if (T_ENDIAN16(dwOutput) <> 0) then

                    ToOutput := Pack3WordsBigEndian
                  else
                    ToOutput := Pack3Words;
                end;

              1: if (T_DOSWAP(dwOutput) <> 0) then
                begin

                  if (T_ENDIAN16(dwOutput) <> 0) then

                    ToOutput := Pack3WordsAndSkip1SwapBigEndian
                  else
                  begin
                    if (T_SWAPFIRST(dwOutput) <> 0) then
                      ToOutput := Pack3WordsAndSkip1SwapSwapFirst
                    else
                      ToOutput := Pack3WordsAndSkip1Swap;
                  end;
                end
                else
                begin
                  if (T_ENDIAN16(dwOutput) <> 0) then
                    ToOutput := Pack3WordsAndSkip1BigEndian
                  else
                    ToOutput := Pack3WordsAndSkip1;
                end;
            end;

          4:
            if (T_EXTRA(dwOutput) = 0) then
            begin

              if (T_DOSWAP(dwOutput) <> 0) then
              begin

                if (T_ENDIAN16(dwOutput) <> 0) then
                  ToOutput := Pack4WordsSwapBigEndian
                else
                  ToOutput := Pack4WordsSwap;
              end
              else
              begin

                if (T_ENDIAN16(dwOutput) <> 0) then
                begin

                  if (T_FLAVOR(dwOutput) <> 0) then
                    ToOutput := Pack4WordsBigEndianReverse
                  else
                    ToOutput := Pack4WordsBigEndian;
                end
                else
                begin
                  if (T_FLAVOR(dwOutput) <> 0) then
                    ToOutput := Pack4WordsReverse
                  else
                    ToOutput := Pack4Words;
                end;
              end;
            end;

          6:
            if (T_EXTRA(dwOutput) = 0) then
            begin

              if (T_DOSWAP(dwOutput) <> 0) then
              begin

                if (T_ENDIAN16(dwOutput) <> 0) then
                  ToOutput := Pack6WordsSwapBigEndian
                else
                  ToOutput := Pack6WordsSwap;
              end
              else
              begin

                if (T_ENDIAN16(dwOutput) <> 0) then
                  ToOutput := Pack6WordsBigEndian
                else
                  ToOutput := Pack6Words;
              end;
            end;

          5,
            7,
            8,
            9,
            10,
            11,
            12,
            13,
            14,
            15: if ((T_EXTRA(dwOutput) = 0) and (T_SWAPFIRST(dwOutput) = 0)) then
            begin

              if (T_DOSWAP(dwOutput) <> 0) then
              begin

                if (T_ENDIAN16(dwOutput) <> 0) then
                  ToOutput := PackNWordsSwapBigEndian
                else
                  ToOutput := PackNWordsSwap;
              end
              else
              begin

                if (T_ENDIAN16(dwOutput) <> 0) then
                  ToOutput := PackNWordsBigEndian
                else
                  ToOutput := PackNWords;
              end;
            end;

        end;

    end;
  end;

  //if (not ToOutput)
    //cmsSignalError(LCMS_ERRC_ABORTED, "Unknown output format");

  result := ToOutput;
end;

// Null transformation, only hold channels

procedure NullXFORM(p: _LPcmsTRANSFORM; xin: pointer; xout: pointer; Size: dword);
var
  accum: pbyte;
  output: pbyte;
  wIn: array[0..MAXCHANNELS - 1] of WORD;
  i, n: dword;
begin
  accum := xin;
  output := xout;
  n := Size;

  for i := 0 to n - 1 do
  begin
    accum := p^.FromInput(p, @wIn, accum);
    output := p^.ToOutput(p, @wIn, output);
  end;

end;

procedure cmsFreeLUT(Lut: LPLUT);
var
  i: integer;  // 3.0.1
begin

  if (Lut = nil) then
    exit;

  if (Lut^.T <> nil) then
    freemem(Lut^.T);

  for i := 0 to Lut^.OutputChan - 1 do
  begin
    if (Lut^.L2[i] <> nil) then
      freemem(Lut^.L2[i]);
  end;

  for i := 0 to Lut^.InputChan - 1 do
  begin
    if (Lut^.L1[i] <> nil) then
      freemem(Lut^.L1[i]);
  end;

  if (Lut^.wFlags and LUT_HASTL3) <> 0 then
  begin

    for i := 0 to Lut^.InputChan - 1 do
    begin

      if (Lut^.L3[i] <> nil) then
        freemem(Lut^.L3[i]);
    end;
  end;

  if (Lut^.wFlags and LUT_HASTL4) <> 0 then
  begin

    for i := 0 to Lut^.OutputChan - 1 do
    begin

      if (Lut^.L4[i] <> nil) then
        freemem(Lut^.L4[i]);
    end;
  end;

  if (Lut^.CLut16params.p8 <> nil) then
    freemem(Lut^.CLut16params.p8);

  freemem(Lut);
end;

procedure cmsFreeMatShaper(MatShaper: LPMATSHAPER);
var
  i: integer;
begin
  if (MatShaper = nil) then
    exit;

  for i := 0 to 2 do
  begin
    if (MatShaper^.L[i] <> nil) then
      freemem(MatShaper^.L[i]);
    if (MatShaper^.L2[i] <> nil) then
      freemem(MatShaper^.L2[i]);
  end;

  freemem(MatShaper);
end;

procedure cmsFreeNamedColorList(v: LPcmsNAMEDCOLORLIST);
begin
  if (v = nil) then
  begin
    //cmsSignalError(LCMS_ERRC_RECOVERABLE, "Couldn't free a NULL named color list");
    exit;
  end;
  freemem(v);
end;

procedure IEcmsDeleteTransform(hTransform: cmsHTRANSFORM);
var
  p: _LPcmsTRANSFORM;
begin
  p := _LPcmsTRANSFORM(hTransform);
  if (p^.Device2PCS <> nil) then
    cmsFreeLUT(p^.Device2PCS);
  if (p^.PCS2Device <> nil) then
    cmsFreeLUT(p^.PCS2Device);
  if (p^.Gamut <> nil) then
    cmsFreeLUT(p^.Gamut);
  if (p^.Preview <> nil) then
    cmsFreeLUT(p^.Preview);
  if (p^.DeviceLink <> nil) then
    cmsFreeLUT(p^.DeviceLink);
  if (p^.InMatShaper <> nil) then
    cmsFreeMatShaper(p^.InMatShaper);
  if (p^.OutMatShaper <> nil) then
    cmsFreeMatShaper(p^.OutMatShaper);
  if (p^.SmeltMatShaper <> nil) then
    cmsFreeMatShaper(p^.SmeltMatShaper);
  if (p^.NamedColorList <> nil) then
    cmsFreeNamedColorList(p^.NamedColorList);

  freemem(p);
end;

// Translate from our colorspace to ICC representation

function _cmsICCcolorSpace(OurNotation: integer): icColorSpaceSignature;
begin
  case (OurNotation) of

    1,
      PT_GRAY: result := icSigGrayData;

    2,
      PT_RGB: result := icSigRgbData;

    PT_CMY: result := icSigCmyData;
    PT_CMYK: result := icSigCmykData;
    PT_YCbCr: result := icSigYCbCrData;
    PT_YUV: result := icSigLuvData;
    PT_XYZ: result := icSigXYZData;
    PT_Lab: result := icSigLabData;
    PT_YUVK: result := icColorSpaceSignature(icSigLuvKData);
    PT_HSV: result := icSigHsvData;
    PT_HLS: result := icSigHlsData;
    PT_Yxy: result := icSigYxyData;
    PT_HiFi: result := icColorSpaceSignature(icSigHexachromeData);
    PT_HiFi7: result := icColorSpaceSignature(icSigHeptachromeData);
    PT_HiFi8: result := icColorSpaceSignature(icSigOctachromeData);

  else
    result := icMaxEnumData;
  end;
end;

function cmsGetPCS(hProfile: cmsHPROFILE): icColorSpaceSignature;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);
  result := Icc^.PCS;
end;

// Check colorspace

function IsProperColorSpace(hProfile: cmsHPROFILE; dwFormat: DWORD; lUsePCS: longbool): longbool;
var
  Space: integer;
begin
  Space := T_COLORSPACE(dwFormat);

  if (Space = PT_ANY) then
  begin
    result := true;
    exit;
  end;

  if (lUsePCS) then
    result := (_cmsICCcolorSpace(Space) = cmsGetPCS(hProfile))
  else
    result := (_cmsICCcolorSpace(Space) = cmsGetColorSpace(hProfile));

end;

function cmsAllocLUT: LPLUT;
var
  NewLUT: LPLUT;
begin
  getmem(NewLUT, sizeof(LUT));
  if (NewLUT <> nil) then
    ZeroMemory(NewLUT, sizeof(LUT));
  result := NewLUT;
end;

function DupBlockTab(Org: pointer; size: integer): pointer;
var
  mem: pointer;
begin
  getmem(mem, size);
  CopyMemory(mem, Org, size);
  result := mem;
end;

function cmsDupLUT(Orig: LPLUT): LPLUT;
var
  NewLUT: LPLUT;
  i: integer;
begin
  NewLUT := cmsAllocLUT;

  CopyMemory(NewLUT, Orig, sizeof(LUT));

  for i := 0 to Orig^.InputChan - 1 do
    NewLUT^.L1[i] := PWORD(DupBlockTab(Orig^.L1[i], sizeof(WORD) * Orig^.In16params.nSamples));

  for i := 0 to Orig^.OutputChan - 1 do
    NewLUT^.L2[i] := PWORD(DupBlockTab(Orig^.L2[i], sizeof(WORD) * Orig^.Out16params.nSamples));

  NewLUT^.T := PWORD(DupBlockTab(Orig^.T, Orig^.Tsize));

  result := NewLUT;
end;

function SIZEOF_UINT8_ALIGNED: integer; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (sizeof(_cmsTestAlign8) - sizeof(icS15Fixed16Number));
end;

// Convert to fixed point encoding is 1.0 = 0xFFFF

procedure VEC3toFix(r: LPWVEC3; v: LPVEC3);
begin
  r^.n[VX] := round(v^.n[VX] * 65536 );
  r^.n[VY] := round(v^.n[VY] * 65536 );
  r^.n[VZ] := round(v^.n[VZ] * 65536 );
end;

procedure MAT3toFix(r: LPWMAT3; v: LPMAT3);
begin
  VEC3toFix(@r^.v[0], @v^.v[0]);
  VEC3toFix(@r^.v[1], @v^.v[1]);
  VEC3toFix(@r^.v[2], @v^.v[2]);
end;

// Check id two vectors are the same, allowing tolerance

function RangeCheck(l, h, v: double): longbool; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (v >= l) and (v <= h);
end;

(*
function FIXED_TO_DOUBLE(x: Fixed32): double;
begin
  result := x / 65536;
end;
*)

function VEC3equal(a: LPWVEC3; b: LPWVEC3; Tolerance: double): longbool;
var
  i: integer;
  c: double;
begin

  for i := 0 to 2 do
  begin

    c := a^.n[i] / 65536;
    if (not RangeCheck(c - Tolerance, c + Tolerance, (b^.n[i]/65536) )) then
    begin
      result := FALSE;
      exit;
    end;
  end;
  result := TRUE;
end;

// Check if matrix is Identity. Allow a tolerance as %

function MAT3isIdentity(a: LPWMAT3; Tolerance: double): longbool;
var
  i: integer;
  Idd: MAT3;
  Idf: WMAT3;
begin

  MAT3identity(@Idd);
  MAT3toFix(@Idf, @Idd);

  for i := 0 to 2 do
    if (not VEC3equal(@a^.v[i], @Idf.v[i], Tolerance)) then
    begin
      result := false;
      exit;
    end;

  result := true;
end;

function TO16_TAB(x: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((x) shl 8) or (x));
end;

function _cmsQuantizeVal(i: double; MaxSamples: integer): WORD; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := iefloor(((i * 65535) / (MaxSamples - 1)) + 0.5);
end;

// Is a table linear?

function cmsIsLinear(Table: pwordarray; nEntries: integer): integer;
var
  i, diff: integer;
begin

  for i := 0 to nEntries - 1 do
  begin

    //diff := abs(Table[i] - _cmsQuantizeVal(i, nEntries));
    diff := abs(Table[i] - (iefloor(((i * 65535) / (nEntries - 1)) + 0.5)));

    if (diff > 3) then
    begin
      result := 0;
      exit;
    end;
  end;

  result := 1;
end;

function uipow(a, b: integer): dword;
var
  rv: dword;
begin
  rv := 1;
  while b > 0 do
  begin
    rv := rv * a;
    dec(b);
  end;
  result := rv;
end;

procedure cmsCalcL16Params(nSamples: integer; p: LPL16PARAMS);
begin
  p^.nSamples := nSamples;
  p^.Domain := (nSamples - 1);
  p^.nOutputs := 1;
  p^.nInputs := p^.nOutputs;

end;

function FIXED_TO_INT(x: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := ((x) shr 16);
end;

function FIXED_REST_TO_INT(x: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := ((x) and $FFFF);
end;

(*
function FixedLERP(a, l, h: Fixed32): Fixed32;
begin
  result := round(((((h - l) * a) / 65536) + l) );
end;
*)

function FixedLERP(a, l, h: Fixed32): Fixed32;
var
  dif: double;
begin
  dif := h - l;

  dif := dif * a;
  dif := dif / 65536;
  dif := dif + l;

  result := round(dif);
end;

// Eval gray LUT having only one input channel

procedure Eval1Input(StageABC: pwordarray; StageLMN: pwordarray; LutTable: pwordarray; p16: LPL16PARAMS);
var
  fk: Fixed32;
  k0, k1, rk, xK0, xK1: Fixed32;
  OutChan: integer;
begin

  fk := ToFixedDomain(StageABC[0] * p16^.Domain);
  k0 := FIXED_TO_INT(fk);
  rk := FIXED_REST_TO_INT(fk);

  if StageABC[0] <> $FFFF then
    k1 := k0 + 1
  else
    k1 := k0;

  xK0 := p16^.opta1 * k0;
  xK1 := p16^.opta1 * k1;

  for OutChan := 0 to p16^.nOutputs - 1 do

    StageLMN[OutChan] := FixedLERP(rk, LutTable[xK0 + OutChan], LutTable[xK1 + OutChan]);
end;

// Tetrahedral interpolation, using Sakamoto algorithm.

procedure cmsTetrahedralInterp16(Input: pwordarray; Output: pwordarray; LutTable: pwordarray; p: LPL16PARAMS);
var
  px, py, pz: double;
  x0, y0, z0, x1, y1, z1: integer;
  fx, fy, fz: double;
  c1, c2, c3: double;
  t1, t2: double;
  i1: integer;
  clutPoints, OutChan, TotalOut: integer;
  (*
  function DENS(X, Y, Z: integer): double;
  begin
    result := (LutTable[TotalOut*((Z)+clutPoints*((Y)+clutPoints*(X)))+OutChan])
  end;
  *)
begin

  clutPoints := p^.Domain + 1;
  TotalOut := p^.nOutputs;

  px := (Input[0] * p^.Domain) / 65535;
  py := (Input[1] * p^.Domain) / 65535;
  pz := (Input[2] * p^.Domain) / 65535;

  x0 := iefloor(px);
  fx := (px - x0);
  y0 := iefloor(py);
  fy := (py - y0);
  z0 := iefloor(pz);
  fz := (pz - z0);

  if Input[0] <> $FFFF then
    x1 := x0 + 1
  else
    x1 := x0;

  if Input[1] <> $FFFF then
    y1 := y0 + 1
  else
    y1 := y0;

  if Input[2] <> $FFFF then
    z1 := z0 + 1
  else
    z1 := z0;

  for OutChan := 0 to TotalOut - 1 do
  begin

    if (fx >= fy) and (fy >= fz) then
    begin
      //
      i1 := clutPoints * (x1);
      t1 := (LutTable[TotalOut * ((z0) + clutPoints * ((y0) + i1)) + OutChan]);
      t2 := (LutTable[TotalOut * ((z0) + clutPoints * ((y1) + i1)) + OutChan]);
      c1 := t1 - (LutTable[TotalOut * ((z0) + clutPoints * ((y0) + clutPoints * (x0))) + OutChan]);
      c2 := t2 - t1;
      c3 := (LutTable[TotalOut * ((z1) + clutPoints * ((y1) + i1)) + OutChan]) - t2;
      (*
          c1 := DENS(x1, y0, z0) - DENS(x0, y0, z0);
          c2 := DENS(x1, y1, z0) - DENS(x1, y0, z0);
          c3 := DENS(x1, y1, z1) - DENS(x1, y1, z0);
          *)
    end
    else
    if (fx >= fz) and (fz >= fy) then
    begin
      i1 := clutPoints * (x1);
      t1 := (LutTable[TotalOut * ((z0) + clutPoints * ((y0) + i1)) + OutChan]);
      t2 := (LutTable[TotalOut * ((z1) + clutPoints * ((y0) + i1)) + OutChan]);
      c1 := t1 - (LutTable[TotalOut * ((z0) + clutPoints * ((y0) + clutPoints * (x0))) + OutChan]);
      c2 := (LutTable[TotalOut * ((z1) + clutPoints * ((y1) + i1)) + OutChan]) - t2;
      c3 := t2 - t1;
      (*
            c1 := DENS(x1, y0, z0) - DENS(x0, y0, z0);
            c2 := DENS(x1, y1, z1) - DENS(x1, y0, z1);
            c3 := DENS(x1, y0, z1) - DENS(x1, y0, z0);
            *)
    end
    else
    if (fz >= fx) and (fx >= fy) then
    begin
      t1 := (LutTable[TotalOut * ((Z1) + clutPoints * ((Y0) + clutPoints * (X1))) + OutChan]);
      t2 := (LutTable[TotalOut * ((Z1) + clutPoints * ((Y0) + clutPoints * (X0))) + OutChan]);
      c1 := t1 - t2;
      c2 := (LutTable[TotalOut * ((Z1) + clutPoints * ((Y1) + clutPoints * (X1))) + OutChan]) - t1;
      c3 := t2 - (LutTable[TotalOut * ((Z0) + clutPoints * ((Y0) + clutPoints * (X0))) + OutChan]);
      (*
            c1 := DENS(x1, y0, z1) - DENS(x0, y0, z1);
            c2 := DENS(x1, y1, z1) - DENS(x1, y0, z1);
            c3 := DENS(x0, y0, z1) - DENS(x0, y0, z0);
            *)
    end
    else
    if (fy >= fx) and (fx >= fz) then
    begin
      t1 := (LutTable[TotalOut * ((z0) + clutPoints * ((y1) + clutPoints * (x1))) + OutChan]);
      t2 := (LutTable[TotalOut * ((z0) + clutPoints * ((y1) + clutPoints * (x0))) + OutChan]);
      c1 := t1 - t2;
      c2 := t2 - (LutTable[TotalOut * ((z0) + clutPoints * ((y0) + clutPoints * (x0))) + OutChan]);
      c3 := (LutTable[TotalOut * ((z1) + clutPoints * ((y1) + clutPoints * (x1))) + OutChan]) - t1;
      (*
          c1 := DENS(x1, y1, z0) - DENS(x0, y1, z0);
          c2 := DENS(x0, y1, z0) - DENS(x0, y0, z0);
          c3 := DENS(x1, y1, z1) - DENS(x1, y1, z0);
          *)

    end
    else
    if (fy >= fz) and (fz >= fx) then
    begin
      t1 := (LutTable[TotalOut * ((z1) + clutPoints * ((y1) + clutPoints * (x0))) + OutChan]);
      t2 := (LutTable[TotalOut * ((z0) + clutPoints * ((y1) + clutPoints * (x0))) + OutChan]);
      c1 := (LutTable[TotalOut * ((z1) + clutPoints * ((y1) + clutPoints * (x1))) + OutChan]) - t1;
      c2 := t2 - (LutTable[TotalOut * ((z0) + clutPoints * ((y0) + clutPoints * (x0))) + OutChan]);
      c3 := t1 - t2;
      (*
            c1 := DENS(x1, y1, z1) - DENS(x0, y1, z1);
            c2 := DENS(x0, y1, z0) - DENS(x0, y0, z0);
            c3 := DENS(x0, y1, z1) - DENS(x0, y1, z0);
            *)
    end
    else
    if (fz >= fy) and (fy >= fx) then
    begin
      t1 := (LutTable[TotalOut * ((z1) + clutPoints * ((y1) + clutPoints * (x0))) + OutChan]);
      t2 := (LutTable[TotalOut * ((z1) + clutPoints * ((y0) + clutPoints * (x0))) + OutChan]);
      c1 := (LutTable[TotalOut * ((z1) + clutPoints * ((y1) + clutPoints * (x1))) + OutChan]) - t1;
      c2 := t1 - t2;
      c3 := t2 - (LutTable[TotalOut * ((z0) + clutPoints * ((y0) + clutPoints * (x0))) + OutChan]);
      (*
            c1 := DENS(x1, y1, z1) - DENS(x0, y1, z1);
            c2 := DENS(x0, y1, z1) - DENS(x0, y0, z1);
            c3 := DENS(x0, y0, z1) - DENS(x0, y0, z0);
            *)
    end
    else
    begin
      c3 := 0;
      c2 := 0;
      c1 := 0;
    end;

    Output[OutChan] := iefloor((LutTable[TotalOut * ((Z0) + clutPoints * ((Y0) + clutPoints * (X0))) + OutChan]) + c1 * fx + c2 * fy + c3 * fz + 0.5);
  end;

end;

// Trilinear interpolation (16 bits) - float version

(*
procedure cmsTrilinearInterp16(Input: pwordarray; Output: pwordarray; LutTable: pwordarray; p: LPL16PARAMS);
var
  px, py, pz: double;
  x0, y0, z0, 
  x1, y1, z1: integer;
  clutPoints, TotalOut, OutChan: integer;
  fx, fy, fz, 
  d000, d001, d010, d011, 
  d100, d101, d110, d111, 
  dx00, dx01, dx10, dx11, 
  dxy0, dxy1, dxyz: double;

  function LERP(a, l, h: double): double;
  begin
    result := ((l)+(((h)-(l))*(a)));
  end;
  function DENS(X, Y, Z: integer): double;
  begin
    result := (LutTable[TotalOut*((Z)+clutPoints*((Y)+clutPoints*(X)))+OutChan]);
  end;

begin

  clutPoints := p ^. Domain + 1;
  TotalOut   := p ^. nOutputs;

  px := ( Input[0] * (p^.Domain)) / 65535.0;
  py := ( Input[1] * (p^.Domain)) / 65535.0;
  pz := ( Input[2] * (p^.Domain)) / 65535.0;

  x0 := floor(px); fx := px - x0;
  y0 := floor(py); fy := py - y0;
  z0 := floor(pz); fz := pz - z0;

  if Input[0] <> $FFFF then
    x1 := x0 + 1
  else
    x1 := x0 ;

  if Input[1] <> $FFFF then
    y1 := y0 + 1
  else
    y1 := y0;

  if Input[2] <> $FFFF then
    z1 := z0 + 1
  else
    z1 := z0;

  for OutChan := 0 to TotalOut-1 do begin

    d000 := DENS(x0, y0, z0);
    d001 := DENS(x0, y0, z1);
    d010 := DENS(x0, y1, z0);
    d011 := DENS(x0, y1, z1);

    d100 := DENS(x1, y0, z0);
    d101 := DENS(x1, y0, z1);
    d110 := DENS(x1, y1, z0);
    d111 := DENS(x1, y1, z1);

    dx00 := LERP(fx, d000, d100);
    dx01 := LERP(fx, d001, d101);
    dx10 := LERP(fx, d010, d110);
    dx11 := LERP(fx, d011, d111);

    dxy0 := LERP(fy, dx00, dx10);
    dxy1 := LERP(fy, dx01, dx11);

    dxyz := LERP(fz, dxy0, dxy1);

    Output[OutChan] := floor(dxyz + 0.5);
  end;

end;
*)


procedure cmsTrilinearInterp16(Input: pwordarray; Output: pwordarray;
  LutTable: pwordarray; p: LPL16PARAMS);

var
  OutChan, TotalOut: integer;
  fx, fy, fz: Fixed32;
  rx, ry, rz: WORD;
  x0mi, y0mi, z0mi, x1mi, y1mi, z1mi: integer;
  X0ma, X1ma, Y0ma, Y1ma, Z0ma, Z1ma: integer;
  d000, d001, d010, d011, 
    d100, d101, d110, d111, 
    dx00, dx01, dx10, dx11, 
    dxy0, dxy1, dxyz: integer;

  function DENS(i, j, k: integer): integer;
  begin
    result := (LutTable[(i) + (j) + (k) + OutChan]);
  end;
  function LERP(a, l, h: integer): word;
  begin
    result := l + ((((h - l) * a) + $8000) shr 16);
  end;

begin

  TotalOut := p^.nOutputs;

  fx := ToFixedDomain(Input[0] * p^.Domain);
  x0mi := FIXED_TO_INT(fx);
  rx := FIXED_REST_TO_INT(fx);

  fy := ToFixedDomain(Input[1] * p^.Domain);
  y0mi := FIXED_TO_INT(fy);
  ry := FIXED_REST_TO_INT(fy);

  fz := ToFixedDomain(Input[2] * p^.Domain);
  z0mi := FIXED_TO_INT(fz);
  rz := FIXED_REST_TO_INT(fz);

  if Input[0] <> $FFFF then
    x1mi := x0mi + (1)
  else
    x1mi := x0mi + (0);

  if Input[1] <> $FFFF then
    y1mi := y0mi + (1)
  else
    y1mi := y0mi + (0);

  if Input[2] <> $FFFF then
    z1mi := z0mi + (1)
  else
    z1mi := z0mi + (0);

  Z0ma := p^.opta1 * z0mi;
  Z1ma := p^.opta1 * z1mi;
  Y0ma := p^.opta2 * y0mi;
  Y1ma := p^.opta2 * y1mi;
  X0ma := p^.opta3 * x0mi;
  X1ma := p^.opta3 * x1mi;

  for OutChan := 0 to TotalOut - 1 do
  begin

    d000 := DENS(X0ma, Y0ma, Z0ma);
    d001 := DENS(X0ma, Y0ma, Z1ma);
    d010 := DENS(X0ma, Y1ma, Z0ma);
    d011 := DENS(X0ma, Y1ma, Z1ma);

    d100 := DENS(X1ma, Y0ma, Z0ma);
    d101 := DENS(X1ma, Y0ma, Z1ma);
    d110 := DENS(X1ma, Y1ma, Z0ma);
    d111 := DENS(X1ma, Y1ma, Z1ma);

    dx00 := LERP(rx, d000, d100);
    dx01 := LERP(rx, d001, d101);
    dx10 := LERP(rx, d010, d110);
    dx11 := LERP(rx, d011, d111);

    dxy0 := LERP(ry, dx00, dx10);
    dxy1 := LERP(ry, dx01, dx11);

    dxyz := LERP(rz, dxy0, dxy1);

    Output[OutChan] := dxyz;
  end;

end;

procedure Eval4Inputs(StageABC: pwordarray; StageLMN: pwordarray; LutTable: pwordarray; p16: LPL16PARAMS);
var
  fk: Fixed32;
  k0, rk: Fixed32;
  xK0, xK1: integer;
  T, T1: PWORD;
  i: integer;
  Tmp1, Tmp2: array[0..MAXCHANNELS - 1] of WORD;
begin

  fk := ToFixedDomain(StageABC[0] * p16^.Domain);
  k0 := ((fk) shr 16);
  rk := ((fk) and $FFFF);

  xK0 := p16^.opta4 * k0;

  if StageABC[0] <> $FFFF then
    xK1 := p16^.opta4 * (k0 + 1)
  else
    xK1 := p16^.opta4 * (k0);

  p16^.nInputs := 3;

  T := pword(LutTable);
  inc(T, xK0);

  T1 := PWORD(StageABC);
  inc(T1);
  cmsTetrahedralInterp16(pwordarray(T1), @Tmp1, pwordarray(T), p16);

  T := pword(LutTable);
  inc(T, xK1);

  T1 := PWORD(StageABC);
  inc(T1);
  cmsTetrahedralInterp16(pwordarray(T1), @Tmp2, pwordarray(T), p16);

  p16^.nInputs := 4;
  for i := 0 to p16^.nOutputs - 1 do
    //StageLMN[i] := FixedLERP(rk, Tmp1[i], Tmp2[i]);
    StageLMN[i] := round(((((Tmp2[i] - Tmp1[i]) * rk) / 65536) + Tmp1[i]) );

end;

procedure Eval5Inputs(StageABC: pwordarray; StageLMN: pwordarray; LutTable: pwordarray; p16: LPL16PARAMS);
var
  fk: Fixed32;
  k0, rk: Fixed32;
  xK0, xK1: integer;
  T, T1: PWORD;
  i: integer;
  Tmp1, Tmp2: array[0..MAXCHANNELS - 1] of WORD;
begin

  fk := ToFixedDomain(StageABC[0] * p16^.Domain);
  k0 := FIXED_TO_INT(fk);
  rk := FIXED_REST_TO_INT(fk);

  xK0 := p16^.opta5 * k0;

  if StageABC[0] <> $FFFF then
    xK1 := p16^.opta5 * (k0 + (1))
  else
    xK1 := p16^.opta5 * (k0);

  p16^.nInputs := 4;

  T := pword(LutTable);
  inc(T, xK0);

  T1 := pword(StageABC);
  inc(T1);
  Eval4Inputs(pwordarray(T1), @Tmp1, pwordarray(T), p16);

  T := pword(LutTable);
  inc(T, xK1);

  T1 := pword(StageABC);
  inc(T1);
  Eval4Inputs(pwordarray(T1), @Tmp2, pwordarray(T), p16);

  p16^.nInputs := 5;
  for i := 0 to p16^.nOutputs - 1 do
    StageLMN[i] := FixedLERP(rk, Tmp1[i], Tmp2[i]);

end;

procedure Eval6Inputs(StageABC: pwordarray; StageLMN: pwordarray; LutTable: pwordarray; p16: LPL16PARAMS);
var
  fk: Fixed32;
  k0, rk: Fixed32;
  xK0, xK1: integer;
  T, T1: PWORD;
  i: integer;
  Tmp1, Tmp2: array[0..MAXCHANNELS - 1] of WORD;
begin

  fk := ToFixedDomain(StageABC[0] * p16^.Domain);
  k0 := FIXED_TO_INT(fk);
  rk := FIXED_REST_TO_INT(fk);

  xK0 := p16^.opta6 * k0;

  if StageABC[0] <> $FFFF then
    xK1 := p16^.opta6 * (k0 + (1))
  else
    xK1 := p16^.opta6 * (k0);

  p16^.nInputs := 5;

  T := pword(LutTable);
  inc(T, xK0);

  T1 := pword(StageABC);
  inc(T1);

  Eval5Inputs(pwordarray(T1), @Tmp1, pwordarray(T), p16);

  T := pword(LutTable);
  inc(T, xK1);

  T1 := pword(StageABC);
  inc(T1);

  Eval5Inputs(pwordarray(T1), @Tmp2, pwordarray(T), p16);

  p16^.nInputs := 6;
  for i := 0 to p16^.nOutputs - 1 do
    StageLMN[i] := FixedLERP(rk, Tmp1[i], Tmp2[i]);

end;

procedure Eval7Inputs(StageABC: pwordarray; StageLMN: pwordarray; LutTable: pwordarray; p16: LPL16PARAMS);
var
  fk: Fixed32;
  k0, rk: Fixed32;
  xK0, xK1: integer;
  T, T1: PWORD;
  i: integer;
  Tmp1, Tmp2: array[0..MAXCHANNELS - 1] of WORD;
begin

  fk := ToFixedDomain(StageABC[0] * p16^.Domain);
  k0 := FIXED_TO_INT(fk);
  rk := FIXED_REST_TO_INT(fk);

  xK0 := p16^.opta7 * k0;

  if StageABC[0] <> $FFFF then
    xK1 := p16^.opta7 * (k0 + (1))
  else
    xK1 := p16^.opta7 * (k0);

  p16^.nInputs := 6;

  T := pword(LutTable);
  inc(T, xK0);

  T1 := pword(StageABC);
  inc(T1);

  Eval6Inputs(pwordarray(T1), @Tmp1, pwordarray(T), p16);

  T := pword(LutTable);
  inc(T, xK1);

  T1 := pword(StageABC);
  inc(T1);

  Eval6Inputs(pwordarray(T1), @Tmp2, pwordarray(T), p16);

  p16^.nInputs := 7;
  for i := 0 to p16^.nOutputs - 1 do
    StageLMN[i] := FixedLERP(rk, Tmp1[i], Tmp2[i]);

end;

procedure Eval8Inputs(StageABC: pwordarray; StageLMN: pwordarray; LutTable: pwordarray; p16: LPL16PARAMS);
var
  fk: Fixed32;
  k0, rk: Fixed32;
  xK0, xK1: integer;
  T, T1: PWORD;
  i: integer;
  Tmp1, Tmp2: array[0..MAXCHANNELS - 1] of WORD;
begin

  fk := ToFixedDomain(StageABC[0] * p16^.Domain);
  k0 := FIXED_TO_INT(fk);
  rk := FIXED_REST_TO_INT(fk);

  xK0 := p16^.opta8 * k0;

  if StageABC[0] <> $FFFF then
    xK1 := p16^.opta8 * (k0 + (1))
  else
    xK1 := p16^.opta8 * (k0);

  p16^.nInputs := 7;

  T := pword(LutTable);
  inc(T, xK0);

  T1 := pword(StageABC);
  inc(T1);

  Eval7Inputs(pwordarray(T1), @Tmp1, pwordarray(T), p16);

  T := pword(LutTable);
  inc(T, xK1);

  T1 := pword(StageABC);
  inc(T1);

  Eval7Inputs(pwordarray(T1), @Tmp2, pwordarray(T), p16);

  p16^.nInputs := 8;
  for i := 0 to p16^.nOutputs - 1 do
    StageLMN[i] := FixedLERP(rk, Tmp1[i], Tmp2[i]);

end;

// Fills optimization parameters

procedure cmsCalcCLUT16ParamsEx(nSamples: integer; InputChan: integer; OutputChan: integer; lUseTetrahedral: longbool; p: LPL16PARAMS);
var
  clutPoints: integer;
begin
  cmsCalcL16Params(nSamples, p);

  p^.nInputs := InputChan;
  p^.nOutputs := OutputChan;

  clutPoints := p^.Domain + 1;

  p^.opta1 := p^.nOutputs; // Z
  p^.opta2 := p^.opta1 * clutPoints; // Y
  p^.opta3 := p^.opta2 * clutPoints; // X
  p^.opta4 := p^.opta3 * clutPoints; // Used only in 4 inputs LUT
  p^.opta5 := p^.opta4 * clutPoints;
  p^.opta6 := p^.opta5 * clutPoints; // Used only on 6 inputs LUT
  p^.opta7 := p^.opta6 * clutPoints; // Used only on 7 inputs LUT
  p^.opta8 := p^.opta7 * clutPoints; // Used only on 8 inputs LUT

  case (InputChan) of

    1: // Gray LUT

      p^.Interp3D := Eval1Input;

    3: // RGB et al
      if (lUseTetrahedral) then
        p^.Interp3D := cmsTetrahedralInterp16
      else
        p^.Interp3D := cmsTrilinearInterp16;

    4: // CMYK LUT
      p^.Interp3D := Eval4Inputs;

    5: // 5 Inks
      p^.Interp3D := Eval5Inputs;

    6: // Hexachrome
      p^.Interp3D := Eval6Inputs;

    7: // 7 inks
      p^.Interp3D := Eval7Inputs;

    8: // 8 inks
      p^.Interp3D := Eval8Inputs;

  else
    ;
    //cmsSignalError(LCMS_ERRC_ABORTED, "Unsupported restoration (%d channels)", InputChan);
  end;

end;

procedure cmsCalcCLUT16Params(nSamples: integer; InputChan: integer; OutputChan: integer; p: LPL16PARAMS);
begin
  cmsCalcCLUT16ParamsEx(nSamples, InputChan, OutputChan, FALSE, p);
end;

procedure VEC3fromFix(r: LPVEC3; v: LPWVEC3);
begin
  r^.n[VX] := FIXED_TO_DOUBLE(v^.n[VX]);
  r^.n[VY] := FIXED_TO_DOUBLE(v^.n[VY]);
  r^.n[VZ] := FIXED_TO_DOUBLE(v^.n[VZ]);
end;

procedure MAT3fromFix(r: LPMAT3; v: LPWMAT3);
begin
  VEC3fromFix(@r^.v[0], @v^.v[0]);
  VEC3fromFix(@r^.v[1], @v^.v[1]);
  VEC3fromFix(@r^.v[2], @v^.v[2]);
end;

procedure FixLUT8(Lut: LPLUT; sig: icTagSignature; nTabSize: integer);
var
  Fixup, Original, Result: MAT3;
  PtrW: PWORD;
  i: integer;
begin

  case (sig) of

    icSigBToA0Tag,
      icSigBToA1Tag,
      icSigBToA2Tag,
      icSigGamutTag,
      icSigPreview0Tag,
      icSigPreview1Tag,
      icSigPreview2Tag:
      begin

        VEC3init(@Fixup.v[0], $FFFF / $FF00, 0, 0);
        VEC3init(@Fixup.v[1], 0, $FFFF / $FF00, 0);
        VEC3init(@Fixup.v[2], 0, 0, $FFFF / $FF00);

        MAT3fromFix(@Original, @Lut^.Matrix);
        MAT3per(@Result, @Original, @Fixup);
        MAT3toFix(@Lut^.Matrix, @Result);

        Lut^.wFlags := Lut^.wFlags or LUT_HASMATRIX;
      end;

  else
    begin

      PtrW := Lut^.T;
      for i := 0 to nTabSize - 1 do
      begin

        PtrW^ := PtrW^ and $FF00;
        inc(PtrW);
      end;

    end;
  end;

end;

procedure ReadLUT8(Icc: LPLCMSICCPROFILE; NewLUT: LPLUT; sig: icTagSignature);
var
  LUT8: icLut8;
  Temp: pbytearray;
  nTabSize: integer;
  i, j: dword;
  AllLinear: dword;
  PtrW: pwordarray;
begin

  Icc^.Read(@LUT8, sizeof(icLut8) - SIZEOF_UINT8_ALIGNED, 1, Icc^.stream);

  NewLUT^.wFlags := LUT_HASTL1 or LUT_HASTL2 or LUT_HAS3DGRID;
  NewLUT^.cLutPoints := LUT8.clutPoints;
  NewLUT^.InputChan := LUT8.inputChan;
  NewLUT^.OutputChan := LUT8.outputChan;
  NewLUT^.InputEntries := 256;
  NewLUT^.OutputEntries := 256;

  AdjustEndianess32(PBYTE(@LUT8.e00));
  AdjustEndianess32(PBYTE(@LUT8.e01));
  AdjustEndianess32(PBYTE(@LUT8.e02));
  AdjustEndianess32(PBYTE(@LUT8.e10));
  AdjustEndianess32(PBYTE(@LUT8.e11));
  AdjustEndianess32(PBYTE(@LUT8.e12));
  AdjustEndianess32(PBYTE(@LUT8.e20));
  AdjustEndianess32(PBYTE(@LUT8.e21));
  AdjustEndianess32(PBYTE(@LUT8.e22));

  NewLUT^.Matrix.v[0].n[0] := Fixed32(LUT8.e00);
  NewLUT^.Matrix.v[0].n[1] := Fixed32(LUT8.e01);
  NewLUT^.Matrix.v[0].n[2] := Fixed32(LUT8.e02);
  NewLUT^.Matrix.v[1].n[0] := Fixed32(LUT8.e10);
  NewLUT^.Matrix.v[1].n[1] := Fixed32(LUT8.e11);
  NewLUT^.Matrix.v[1].n[2] := Fixed32(LUT8.e12);
  NewLUT^.Matrix.v[2].n[0] := Fixed32(LUT8.e20);
  NewLUT^.Matrix.v[2].n[1] := Fixed32(LUT8.e21);
  NewLUT^.Matrix.v[2].n[2] := Fixed32(LUT8.e22);

  if (not MAT3isIdentity(@NewLUT^.Matrix, 0.0001)) then
  begin

    NewLUT^.wFlags := NewLUT^.wFlags or LUT_HASMATRIX;
  end;

  getmem(Temp, 256);
  AllLinear := 0;
  for i := 0 to NewLUT^.InputChan - 1 do
  begin

    getmem(PtrW, sizeof(WORD) * 256);
    NewLUT^.L1[i] := pword(PtrW);
    Icc^.Read(Temp, 1, 256, Icc^.stream);
    for j := 0 to 255 do
      PtrW[j] := TO16_TAB(Temp[j]);
    AllLinear := AllLinear + cmsIsLinear(pwordarray(NewLUT^.L1[i]), NewLUT^.InputEntries);
  end;

  if (AllLinear = NewLUT^.InputChan) then
  begin

    NewLUT^.wFlags := NewLUT^.wFlags and (not LUT_HASTL1);
  end;

  freemem(Temp);

  nTabSize := (NewLUT^.OutputChan * uipow(NewLUT^.cLutPoints, NewLUT^.InputChan));

  if (nTabSize > 0) then
  begin

    getmem(PtrW, sizeof(WORD) * nTabSize);
    getmem(Temp, nTabSize);
    Icc^.Read(Temp, 1, nTabSize, Icc^.stream);

    NewLUT^.T := pword(PtrW);
    NewLUT^.Tsize := (nTabSize * sizeof(WORD));

    for i := 0 to nTabSize - 1 do
    begin

      pword(PtrW)^ := TO16_TAB(Temp[i]);
      inc(pword(PtrW));
    end;
    freemem(Temp);
  end
  else
  begin
    NewLUT^.T := nil;
    NewLUT^.Tsize := 0;
    NewLUT^.wFlags := NewLUT^.wFlags and (not LUT_HAS3DGRID);
  end;

  getmem(Temp, 256);
  AllLinear := 0;
  for i := 0 to NewLUT^.OutputChan - 1 do
  begin

    getmem(PtrW, sizeof(WORD) * 256);
    NewLUT^.L2[i] := pword(PtrW);
    Icc^.Read(Temp, 1, 256, Icc^.stream);
    for j := 0 to 255 do
      PtrW[j] := TO16_TAB(Temp[j]);
    AllLinear := AllLinear + cmsIsLinear(pwordarray(NewLUT^.L2[i]), 256);
  end;

  if (AllLinear = NewLUT^.OutputChan) then
  begin

    NewLUT^.wFlags := NewLUT^.wFlags and (not LUT_HASTL2);
  end;

  freemem(Temp);

  cmsCalcL16Params(NewLUT^.InputEntries, @NewLUT^.In16params);
  cmsCalcL16Params(NewLUT^.OutputEntries, @NewLUT^.Out16params);
  cmsCalcCLUT16Params(NewLUT^.cLutPoints, NewLUT^.InputChan,
    NewLUT^.OutputChan,
    @NewLUT^.CLut16params);
  if (Icc^.PCS = icSigLabData) then
    FixLUT8(NewLUT, sig, nTabSize);

end;

function SIZEOF_UINT16_ALIGNED: integer;
begin
  result := (sizeof(_cmsTestAlign16) - sizeof(icS15Fixed16Number));
end;

procedure xswab(from: pointer; xto: pointer; len: integer);
var
  temp: dword;
  n: integer;
  fp, tp: pbyte;

  procedure STEP;
  begin
    temp := fp^;
    inc(fp);
    tp^ := fp^;
    inc(tp);
    inc(fp);
    tp^ := temp;
    inc(tp);
  end;
begin

  n := (len shr 1) + 1;
  fp := from;
  tp := xto;

  dec(n);
  while ((n) and 07) <> 0 do
  begin
    STEP;
    dec(n);
  end;
  n := n shr 3;
  dec(n);
  while (n >= 0) do
  begin

    STEP;
    STEP;
    STEP;
    STEP;
    STEP;
    STEP;
    STEP;
    STEP;
    dec(n);
  end;
end;

// swap bytes in a array of words

procedure AdjustEndianessArray16(p: PWORD; num_words: integer); {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  xswab(p, p, num_words * sizeof(WORD));
end;

procedure ReadLUT16(Icc: LPLCMSICCPROFILE; NewLUT: LPLUT);
var
  LUT16: icLut16;
  nTabSize: integer;
  i: dword;
  AllLinear: dword;
  PtrW: pword;
begin

  Icc^.Read(@LUT16, sizeof(icLut16) - SIZEOF_UINT16_ALIGNED, 1, Icc^.stream);

  NewLUT^.wFlags := LUT_HASTL1 or LUT_HASTL2 or LUT_HAS3DGRID;
  NewLUT^.cLutPoints := LUT16.clutPoints;
  NewLUT^.InputChan := LUT16.inputChan;
  NewLUT^.OutputChan := LUT16.outputChan;

  AdjustEndianess16(PBYTEARRAY(@LUT16.inputEnt));
  AdjustEndianess16(PBYTEARRAY(@LUT16.outputEnt));

  NewLUT^.InputEntries := LUT16.inputEnt;
  NewLUT^.OutputEntries := LUT16.outputEnt;

  AdjustEndianess32(PBYTE(@LUT16.e00));
  AdjustEndianess32(PBYTE(@LUT16.e01));
  AdjustEndianess32(PBYTE(@LUT16.e02));
  AdjustEndianess32(PBYTE(@LUT16.e10));
  AdjustEndianess32(PBYTE(@LUT16.e11));
  AdjustEndianess32(PBYTE(@LUT16.e12));
  AdjustEndianess32(PBYTE(@LUT16.e20));
  AdjustEndianess32(PBYTE(@LUT16.e21));
  AdjustEndianess32(PBYTE(@LUT16.e22));

  NewLUT^.Matrix.v[0].n[0] := Fixed32(LUT16.e00);
  NewLUT^.Matrix.v[0].n[1] := Fixed32(LUT16.e01);
  NewLUT^.Matrix.v[0].n[2] := Fixed32(LUT16.e02);
  NewLUT^.Matrix.v[1].n[0] := Fixed32(LUT16.e10);
  NewLUT^.Matrix.v[1].n[1] := Fixed32(LUT16.e11);
  NewLUT^.Matrix.v[1].n[2] := Fixed32(LUT16.e12);
  NewLUT^.Matrix.v[2].n[0] := Fixed32(LUT16.e20);
  NewLUT^.Matrix.v[2].n[1] := Fixed32(LUT16.e21);
  NewLUT^.Matrix.v[2].n[2] := Fixed32(LUT16.e22);

  if (not MAT3isIdentity(@NewLUT^.Matrix, 0.0001)) then
  begin

    NewLUT^.wFlags := NewLUT^.wFlags or LUT_HASMATRIX;
  end;

  AllLinear := 0;
  for i := 0 to NewLUT^.InputChan - 1 do
  begin

    getmem(PtrW, sizeof(WORD) * NewLUT^.InputEntries);
    NewLUT^.L1[i] := PtrW;
    Icc^.Read(PtrW, sizeof(WORD), NewLUT^.InputEntries, Icc^.stream);
    AdjustEndianessArray16(PtrW, NewLUT^.InputEntries);
    AllLinear := AllLinear + cmsIsLinear(pwordarray(NewLUT^.L1[i]), NewLUT^.InputEntries);
  end;

  if (AllLinear = NewLUT^.InputChan) then
  begin

    NewLUT^.wFlags := NewLUT^.wFlags and (not LUT_HASTL1);
  end;

  nTabSize := (NewLUT^.OutputChan * uipow(NewLUT^.cLutPoints, NewLUT^.InputChan));
  if (nTabSize > 0) then
  begin

    getmem(PtrW, sizeof(WORD) * nTabSize);

    NewLUT^.T := PtrW;
    NewLUT^.Tsize := (nTabSize * sizeof(WORD));

    Icc^.Read(PtrW, sizeof(WORD), nTabSize, Icc^.stream);
    AdjustEndianessArray16(NewLUT^.T, nTabSize);
  end
  else
  begin
    NewLUT^.T := nil;
    NewLUT^.Tsize := 0;
    NewLUT^.wFlags := NewLUT^.wFlags and (not LUT_HAS3DGRID);
  end;

  AllLinear := 0;
  for i := 0 to NewLUT^.OutputChan - 1 do
  begin

    getmem(PtrW, sizeof(WORD) * NewLUT^.OutputEntries);
    NewLUT^.L2[i] := PtrW;
    Icc^.Read(PtrW, sizeof(WORD), NewLUT^.OutputEntries, Icc^.stream);
    AdjustEndianessArray16(PtrW, NewLUT^.OutputEntries);
    AllLinear := AllLinear + cmsIsLinear(pwordarray(NewLUT^.L2[i]), NewLUT^.OutputEntries);
  end;

  if (AllLinear = NewLUT^.OutputChan) then
  begin
    NewLUT^.wFlags := NewLUT^.wFlags and (not LUT_HASTL2);
  end;

  cmsCalcL16Params(NewLUT^.InputEntries, @NewLUT^.In16params);
  cmsCalcL16Params(NewLUT^.OutputEntries, @NewLUT^.Out16params);
  cmsCalcCLUT16Params(NewLUT^.cLutPoints, NewLUT^.InputChan,
    NewLUT^.OutputChan,
    @NewLUT^.CLut16params);
end;

function cmsAllocGamma(nEntries: integer): LPGAMMATABLE;
var
  p: LPGAMMATABLE;
  size: integer;
begin

  size := sizeof(GAMMATABLE) + (sizeof(WORD) * (nEntries - 1));

  getmem(p, size);
  if (p = nil) then
  begin
    result := nil;
    exit;
  end;

  p^.nEntries := nEntries;
  ZeroMemory(@p^.GammaTable, nEntries * sizeof(WORD));

  result := p;
end;

function FGamma(R: double; x: double): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := Power(R, x);
end;

// Build a gamma table based on gamma constant

function cmsBuildGamma(nEntries: integer; Gamma: double): LPGAMMATABLE;
var
  p: LPGAMMATABLE;
  Table: PWORDARRAY;
  i: integer;
  R, Val: double;
begin

  if (nEntries > 65530) then
  begin
    //cmsSignalError(LCMS_ERRC_WARNING, "Couldn't create gammatable of more than 65530 entries; 65530 assumed");
    nEntries := 65530;
  end;

  p := cmsAllocGamma(nEntries);
  if (p = nil) then
  begin
    result := nil;
    exit;
  end;

  Table := @p^.GammaTable;
  if (Gamma = 0.0) then
  begin
    ZeroMemory(Table, nEntries * sizeof(WORD));
    result := p;
    exit;
  end;

  if (Gamma = 1.0) then
  begin

    for i := 0 to nEntries - 1 do
    begin

      Table[i] := _cmsQuantizeVal(i, nEntries);

    end;
    result := p;
    exit;
  end;

  for i := 0 to nEntries - 1 do
  begin
    R := i / (nEntries - 1);
    Val := FGamma(R, Gamma);

    Table[i] := iefloor(Val * 65535 + 0.5);
  end;

  result := p;
end;

// Fixed point conversion

function Convert8Fixed8(fixed8: WORD): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
var
  msb, lsb: BYTE;
begin

  lsb := (fixed8 and $FF);
  msb := ((fixed8 shr 8) and $FF);

  result := (msb + (lsb / 256));
end;

procedure cmsFreeGamma(Gamma: LPGAMMATABLE);
begin
  if (Gamma <> nil) then
    freemem(Gamma);
end;

function cmsBuildParametricGamma(nEntries: integer; xType: integer; Params: pdoublearray): LPGAMMATABLE;
var
  Table: LPGAMMATABLE;
  R, Val, dval, e: double;
  i: integer;
begin

  Table := cmsAllocGamma(nEntries);
  if (Table = nil) then
  begin
    result := nil;
    exit;
  end;

  for i := 0 to nEntries - 1 do
  begin

    R := i / (nEntries - 1);

    case (xType) of

      1:
        Val := Power(R, Params[0]);

      -1:
        Val := Power(R, 1 / Params[0]);

      2:
        if (R >= -Params[2] / Params[1]) then
        begin

          e := Params[1] * R + Params[2];

          if (e > 0) then
            Val := Power(e, Params[0])
          else
            Val := 0;
        end
        else
          Val := 0;

      -2:
        begin

          Val := (Power(R, 1.0 / Params[0]) - Params[2]) / Params[1];
          if (Val < 0) then
            Val := 0;
        end;

      3:
        if (R >= -Params[2] / Params[1]) then
        begin

          e := Params[1] * R + Params[2];
          Val := Power(e, Params[0]) + Params[3];
        end
        else
          Val := Params[3];

      -3:
        if (R >= Params[3]) then
        begin
          e := R - Params[3];
          Val := (Power(e, 1 / Params[0]) - Params[2]) / Params[1];
          if (Val < 0) then
            Val := 0;
        end
        else
        begin
          Val := -Params[2] / Params[1];
        end;

      4:
        if (R >= Params[4]) then
        begin

          e := Params[1] * R + Params[2];
          if (e > 0) then
            Val := Power(e, Params[0])
          else
            Val := 0;
        end
        else
          Val := R * Params[3];

      -4:
        if (R >= Power(Params[1] * Params[4] + Params[2], Params[0])) then
        begin

          Val := (Power(R, 1.0 / Params[0]) - Params[2]) / Params[1];
        end
        else
        begin
          Val := R / Params[3];
        end;

      5:
        if (R >= Params[4]) then
        begin

          e := Params[1] * R + Params[2];
          Val := Power(e, Params[0]) + Params[5];
        end
        else
          Val := R * Params[3] + Params[6];

      -5:

        if (R >= Power(Params[1] * Params[4], Params[0]) + Params[5]) then
        begin

          Val := Power(R - Params[5], 1 / Params[0]) - Params[2] / Params[1];
        end
        else
        begin
          Val := (R - Params[6]) / Params[3];
        end;

    else
      begin
        //cmsSignalError(LCMS_ERRC_ABORTED, "Unsupported parametric curve type=%d", abs(xType)-1);
        cmsFreeGamma(Table);
        result := nil;
        exit;
      end;
    end;

    dval := Val * 65535.0 + 0.5;
    if (dval > 65535) then
      dval := 65535;
    if (dval < 0) then
      dval := 0;

    Table^.GammaTable[i] := iefloor(dval);
  end;

  result := Table;
end;

function ReadCurve(Icc: LPLCMSICCPROFILE): LPGAMMATABLE;
const
  ParamsByType: array[0..4] of integer = (1, 3, 4, 5, 7);
var
  Count: icUInt32Number;
  NewGamma: LPGAMMATABLE;
  Base: icTagBase;
  n: integer;
  SingleGammaFixed: WORD;

  Params: array[0..9] of double;
  Num: icS15Fixed16Number;
  Reserved: icUInt32Number;
  xType: icUInt16Number;
  i: integer;
begin

  Icc^.Read(@Base, 1, sizeof(icTagBase), Icc^.stream);
  AdjustEndianess32(@Base.sig);

  case (Base.sig) of

    icTagTypeSignature($9478EE00),
      icSigCurveType:
      begin
        Icc^.Read(@Count, sizeof(icUInt32Number), 1, Icc^.stream);

        AdjustEndianess32(@Count);

        case (Count) of

          0:
            begin
              NewGamma := cmsAllocGamma(2);
              if (NewGamma = nil) then
              begin
                result := nil;
                exit;
              end;
              pwordarray(@NewGamma^.GammaTable)[0] := 0;
              pwordarray(@NewGamma^.GammaTable)[1] := $FFFF;
              result := NewGamma;
              exit;
            end;

          1:
            begin
              Icc^.Read(@SingleGammaFixed, sizeof(WORD), 1, Icc^.stream);
              AdjustEndianess16(@SingleGammaFixed);
              result := cmsBuildGamma(4096, Convert8Fixed8(SingleGammaFixed));
              exit;
            end;

        else
          begin

            NewGamma := cmsAllocGamma(Count);
            if (NewGamma = nil) then
            begin
              result := nil;
              exit;
            end;

            Icc^.Read(@NewGamma^.GammaTable, sizeof(WORD), Count, Icc^.stream);

            AdjustEndianessArray16(@NewGamma^.GammaTable, Count);

            result := NewGamma;
            exit;
          end;
        end;
      end;

    icTagTypeSignature(icSigParametricCurveType):
      begin

        Icc^.Read(@xType, sizeof(icUInt16Number), 1, Icc^.stream);
        Icc^.Read(@Reserved, sizeof(icUInt16Number), 1, Icc^.stream);

        AdjustEndianess16(@xType);
        if (xType > 5) then
        begin

          //cmsSignalError(LCMS_ERRC_ABORTED, "Unknown parametric curve type '%d' found.", xType);
          result := nil;
          exit;
        end;

        ZeroMemory(@Params, 10 * sizeof(double));
        n := ParamsByType[xType];

        for i := 0 to n - 1 do
        begin
          Num := 0;
          Icc^.Read(@Num, sizeof(icS15Fixed16Number), 1, Icc^.stream);
          Params[i] := Convert15Fixed16(Num);
        end;

        NewGamma := cmsBuildParametricGamma(4096, xType + 1, @Params);
        result := NewGamma;
        exit;
      end;

  else
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature '%lx' found.", Base.sig);
      result := nil;
      exit;
    end;
  end;

end;

function cmsAllocLinearTable(NewLUT: LPLUT; Tables: LPGAMMATABLEArray; nTable: integer): LPLUT;
var
  i: dword;
  PtrW: PWORD;
begin

  case (nTable) of

    1:
      begin
        NewLUT^.wFlags := NewLUT^.wFlags or LUT_HASTL1;
        cmsCalcL16Params(Tables[0]^.nEntries, @NewLUT^.In16params);
        NewLUT^.InputEntries := Tables[0]^.nEntries;

        for i := 0 to NewLUT^.InputChan - 1 do
        begin

          getmem(PtrW, sizeof(WORD) * NewLUT^.InputEntries);
          NewLUT^.L1[i] := PtrW;
          CopyMemory(PtrW, @Tables[i]^.GammaTable, sizeof(WORD) * NewLUT^.InputEntries);
        end;
      end;

    2:
      begin
        NewLUT^.wFlags := NewLUT^.wFlags or LUT_HASTL2;
        cmsCalcL16Params(Tables[0]^.nEntries, @NewLUT^.Out16params);
        NewLUT^.OutputEntries := Tables[0]^.nEntries;
        for i := 0 to NewLUT^.OutputChan - 1 do
        begin

          getmem(PtrW, sizeof(WORD) * NewLUT^.OutputEntries);
          NewLUT^.L2[i] := PtrW;
          CopyMemory(PtrW, @Tables[i]^.GammaTable, sizeof(WORD) * NewLUT^.OutputEntries);
        end;
      end;

    3:
      begin
        NewLUT^.wFlags := NewLUT^.wFlags or LUT_HASTL3;
        cmsCalcL16Params(Tables[0]^.nEntries, @NewLUT^.L3params);
        NewLUT^.L3Entries := Tables[0]^.nEntries;

        for i := 0 to NewLUT^.InputChan - 1 do
        begin

          getmem(PtrW, sizeof(WORD) * NewLUT^.L3Entries);
          NewLUT^.L3[i] := PtrW;
          CopyMemory(PtrW, @Tables[i]^.GammaTable, sizeof(WORD) * NewLUT^.L3Entries);
        end;
      end;

    4:
      begin
        NewLUT^.wFlags := NewLUT^.wFlags or LUT_HASTL4;
        cmsCalcL16Params(Tables[0]^.nEntries, @NewLUT^.L4params);
        NewLUT^.L4Entries := Tables[0]^.nEntries;
        for i := 0 to NewLUT^.OutputChan - 1 do
        begin

          getmem(PtrW, sizeof(WORD) * NewLUT^.L4Entries);
          NewLUT^.L4[i] := PtrW;
          CopyMemory(PtrW, @Tables[i]^.GammaTable, sizeof(WORD) * NewLUT^.L4Entries);
        end;
      end;

  end;

  result := NewLUT;
end;

// Read a set of curves from specific offset

function ReadSetOfCurves(Icc: LPLCMSICCPROFILE; Offset: integer; NewLUT: LPLUT; nLocation: integer): longbool;
var
  Curves: array[0..MAXCHANNELS - 1] of LPGAMMATABLE;
  i, nCurves: dword;
begin

  if (Icc^.Seek(Icc^.stream, Offset)) then
  begin
    result := FALSE;
    exit;
  end;

  if (nLocation = 1) or (nLocation = 3) then

    nCurves := NewLUT^.InputChan
  else
    nCurves := NewLUT^.OutputChan;

  try

  // 3.0.1
  for i := 0 to nCurves - 1 do
    Curves[i] := nil;

  for i := 0 to nCurves - 1 do
  begin
    Curves[i] := ReadCurve(LPLCMSICCPROFILE(Icc));
    // 3.0.2
    if Curves[i]=nil then
    begin
      result := FALSE;
      exit;
    end;
  end;

  cmsAllocLinearTable(NewLUT, @Curves, nLocation);

  result := TRUE;

  finally

    for i := 0 to nCurves - 1 do
      cmsFreeGamma(Curves[i]);

  end;

end;

function cmsSetMatrixLUT4(Lut: LPLUT; M: LPMAT3; off: LPVEC3; dwFlags: DWORD): LPLUT;
const
  Zero: VEC3 = (n: (0, 0, 0));
var
  WMat: WMAT3;
  Woff: WVEC3;
begin

  MAT3toFix(@WMat, M);

  if (off = nil) then
    off := @Zero;

  VEC3toFix(@Woff, off);

  if MAT3isIdentity(@WMat, 0.0001) and (Woff.n[VX] = 0) and (Woff.n[VY] = 0) and (Woff.n[VZ] = 0) then
  begin
    result := Lut;
    exit;
  end;

  case (dwFlags) of

    LUT_HASMATRIX:
      begin
        Lut^.Matrix := WMat;
        Lut^.wFlags := Lut^.wFlags or LUT_HASMATRIX;
      end;

    LUT_HASMATRIX3:
      begin
        Lut^.Mat3 := WMat;
        Lut^.Ofs3 := Woff;
        Lut^.wFlags := Lut^.wFlags or LUT_HASMATRIX3;
      end;

    LUT_HASMATRIX4:
      begin
        Lut^.Mat4 := WMat;
        Lut^.Ofs4 := Woff;
        Lut^.wFlags := Lut^.wFlags or LUT_HASMATRIX4;
      end;

  end;

  result := Lut;
end;

function ReadMatrixOffset(Icc: LPLCMSICCPROFILE; Offset: integer; NewLUT: LPLUT; dwFlags: DWORD): longbool;
var
  All: array[0..11] of icS15Fixed16Number;
  i: integer;
  m: MAT3;
  o: VEC3;
begin

  if (Icc^.Seek(Icc^.stream, Offset)) then
  begin
    result := FALSE;
    exit;
  end;

  Icc^.Read(@All, sizeof(icS15Fixed16Number), 12, Icc^.stream);

  for i := 0 to 11 do
    AdjustEndianess32(@All[i]);

  m.v[0].n[0] := FIXED_TO_DOUBLE(Fixed32(All[0]));
  m.v[0].n[1] := FIXED_TO_DOUBLE(Fixed32(All[1]));
  m.v[0].n[2] := FIXED_TO_DOUBLE(Fixed32(All[2]));
  m.v[1].n[0] := FIXED_TO_DOUBLE(Fixed32(All[3]));
  m.v[1].n[1] := FIXED_TO_DOUBLE(Fixed32(All[4]));
  m.v[1].n[2] := FIXED_TO_DOUBLE(Fixed32(All[5]));
  m.v[2].n[0] := FIXED_TO_DOUBLE(Fixed32(All[6]));
  m.v[2].n[1] := FIXED_TO_DOUBLE(Fixed32(All[7]));
  m.v[2].n[2] := FIXED_TO_DOUBLE(Fixed32(All[8]));

  o.n[0] := FIXED_TO_DOUBLE(Fixed32(All[9]));
  o.n[1] := FIXED_TO_DOUBLE(Fixed32(All[10]));
  o.n[2] := FIXED_TO_DOUBLE(Fixed32(All[11]));

  cmsSetMatrixLUT4(NewLUT, @m, @o, dwFlags);

  result := TRUE;
end;

function cmsAlloc3DGrid(NewLUT: LPLUT; clutPoints: integer; inputChan: integer; outputChan: integer): LPLUT;
var
  nTabSize: DWORD;
begin

  NewLUT^.wFlags := NewLUT^.wFlags or LUT_HAS3DGRID;
  NewLUT^.cLutPoints := clutPoints;
  NewLUT^.InputChan := inputChan;
  NewLUT^.OutputChan := outputChan;

  nTabSize := (NewLUT^.OutputChan * UIpow(NewLUT^.cLutPoints, NewLUT^.InputChan) * sizeof(WORD));

  getmem(NewLUT^.T, nTabSize);
  ZeroMemory(NewLUT^.T, nTabSize);
  NewLUT^.Tsize := nTabSize;

  cmsCalcCLUT16Params(NewLUT^.cLutPoints, NewLUT^.InputChan,
    NewLUT^.OutputChan,
    @NewLUT^.CLut16params);

  result := NewLUT;
end;

function ReadCLUT(Icc: LPLCMSICCPROFILE; Offset: integer; NewLUT: LPLUT): longbool;
var
  CLUT: icCLutStruct;
  v: byte;
  i: dword;
begin

  if (Icc^.Seek(Icc^.stream, Offset)) then
  begin
    result := FALSE;
    exit;
  end;
  Icc^.Read(@CLUT, sizeof(icCLutStruct), 1, Icc^.stream);

  cmsAlloc3DGrid(NewLUT, CLUT.gridPoints[0], NewLUT^.InputChan,
    NewLUT^.OutputChan);

  if (CLUT.prec = 1) then
  begin

    for i := 0 to NewLUT^.Tsize - 1 do
    begin
      Icc^.Read(@v, sizeof(BYTE), 1, Icc^.stream);
      pwordarray(NewLUT^.T)^[i] := TO16_TAB(v);
    end;

  end
  else
  if (CLUT.prec = 2) then
  begin

    Icc^.Read(NewLUT^.T, sizeof(WORD), NewLUT^.Tsize div sizeof(WORD), Icc^.stream);

    AdjustEndianessArray16(NewLUT^.T, NewLUT^.Tsize div sizeof(WORD));
  end
  else
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "Unknow precission of '%d'", CLUT.prec);
    result := FALSE;
    exit;
  end;

  result := TRUE;
end;

function ReadLUT_A2B(Icc: LPLCMSICCPROFILE; NewLUT: LPLUT; BaseOffset: integer; sig: icTagSignature): longbool;
var
  LUT16: icLutAtoB;
begin

  result := true;

  Icc^.Read(@LUT16, sizeof(icLutAtoB), 1, Icc^.stream);

  NewLUT^.InputChan := LUT16.inputChan;
  NewLUT^.OutputChan := LUT16.outputChan;

  AdjustEndianess32(@LUT16.offsetB);
  AdjustEndianess32(@LUT16.offsetMat);
  AdjustEndianess32(@LUT16.offsetM);
  AdjustEndianess32(@LUT16.offsetC);
  AdjustEndianess32(@LUT16.offsetA);

  if (LUT16.offsetB <> 0) then
    result := result and ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetB, NewLUT, 2); // 3.0.2

  if (LUT16.offsetMat <> 0) then
    ReadMatrixOffset(Icc, BaseOffset + LUT16.offsetMat, NewLUT, LUT_HASMATRIX4);

  if (LUT16.offsetM <> 0) then
    result := result and ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetM, NewLUT, 4); // 3.0.2

  if (LUT16.offsetC <> 0) then
    ReadCLUT(Icc, BaseOffset + LUT16.offsetC, NewLUT);

  if (LUT16.offsetA <> 0) then
    result := result and ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetA, NewLUT, 1); // 3.0.2

  if (Icc^.PCS = icSigLabData) then
  begin

    case (sig) of

      icSigAToB0Tag,
        icSigAToB1Tag,
        icSigAToB2Tag,
        icSigGamutTag,
        icSigPreview0Tag,
        icSigPreview1Tag,
        icSigPreview2Tag:

        NewLUT^.wFlags := NewLUT^.wFlags or LUT_V4_INPUT_EMULATE_V2;

    end;
  end;

end;

function ReadLUT_B2A(Icc: LPLCMSICCPROFILE; NewLUT: LPLUT; BaseOffset: integer; sig: icTagSignature): longbool;
var
  LUT16: icLutBtoA;
begin

  Icc^.Read(@LUT16, sizeof(icLutBtoA), 1, Icc^.stream);

  NewLUT^.InputChan := LUT16.inputChan;
  NewLUT^.OutputChan := LUT16.outputChan;

  AdjustEndianess32(@LUT16.offsetB);
  AdjustEndianess32(@LUT16.offsetMat);
  AdjustEndianess32(@LUT16.offsetM);
  AdjustEndianess32(@LUT16.offsetC);
  AdjustEndianess32(@LUT16.offsetA);

  if (LUT16.offsetB <> 0) then
    ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetB, NewLUT, 1);

  if (LUT16.offsetMat <> 0) then
    ReadMatrixOffset(Icc, BaseOffset + LUT16.offsetMat, NewLUT, LUT_HASMATRIX3);

  if (LUT16.offsetM <> 0) then
    ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetM, NewLUT, 3);

  if (LUT16.offsetC <> 0) then
    ReadCLUT(Icc, BaseOffset + LUT16.offsetC, NewLUT);

  if (LUT16.offsetA <> 0) then
    ReadSetOfCurves(Icc, BaseOffset + LUT16.offsetA, NewLUT, 2);

  if (Icc^.PCS = icSigLabData) then
  begin

    case (sig) of

      icSigBToA0Tag,
        icSigBToA1Tag,
        icSigBToA2Tag,
        icSigGamutTag,
        icSigPreview0Tag,
        icSigPreview1Tag,
        icSigPreview2Tag:

        NewLUT^.wFlags := NewLUT^.wFlags or LUT_V4_OUTPUT_EMULATE_V2;
    end;
  end;

  result := TRUE;
end;

// CLUT main reader

function cmsReadICCLut(hProfile: cmsHPROFILE; sig: icTagSignature): LPLUT;
var
  Icc: LPLCMSICCPROFILE;
  Base: icTagBase;
  n: integer;
  offset: integer;
  NewLUT: LPLUT;
begin

  Icc := LPLCMSICCPROFILE(hProfile);

  n := SearchTag(Icc, sig);
  if (n < 0) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "Tag not found");
    result := nil;
    exit;
  end;

  if (Icc^.stream = nil) then
  begin
    result := cmsDupLUT(LPLUT(Icc^.TagPtrs[n]));
    exit;
  end;

  offset := Icc^.TagOffsets[n];

  if (Icc^.Seek(Icc^.stream, offset)) then
  begin
    result := nil;
    exit;
  end;

  Icc^.Read(@Base, sizeof(icTagBase), 1, Icc^.stream);
  AdjustEndianess32(pbyte(@Base.sig));

  NewLUT := cmsAllocLUT();
  if (NewLUT = nil) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "cmsAllocLUT() failed");
    result := nil;
    exit;
  end;

  case (Base.sig) of

    icSigLut8Type: ReadLUT8(Icc, NewLUT, sig);
    icSigLut16Type: ReadLUT16(Icc, NewLUT);

    icTagTypeSignature(icSiglutAtoBType):
      begin
        // 3.0.2
        if not ReadLUT_A2B(Icc, NewLUT, offset, sig) then
        begin
          cmsFreeLUT(NewLUT);
          result := nil;
          exit;
        end;
      end;
    icTagTypeSignature(icSiglutBtoAType): ReadLUT_B2A(Icc, NewLUT, offset, sig);

  else
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature %lx found.", Base.sig);
      cmsFreeLUT(NewLUT);
      result := nil;
      exit;
    end;
  end;

  result := NewLUT;
end;

procedure PrecalculatedXFORM(p: _LPcmsTRANSFORM; xin: pointer; xout: pointer; Size: dword);
var
  accum: pbyte;
  output: pbyte;
  wIn, wOut: array[0..MAXCHANNELS - 1] of WORD;
  i, n: dword;
begin
  accum := xin;
  output := xout;
  n := Size;

  for i := 0 to n - 1 do
  begin

    accum := p^.FromInput(p, @wIn, accum);
    cmsEvalLUT(p^.DeviceLink, @wIn, @wOut);
    output := p^.ToOutput(p, @wOut, output);
  end;
end;

procedure CachedXFORM(p: _LPcmsTRANSFORM; xin: pointer; xout: pointer; Size: dword);
var
  accum: pbyte;
  output: pbyte;
  wIn, wOut: array[0..MAXCHANNELS - 1] of WORD;
  i, n: dword;
begin

  accum := PBYTE(xin);
  output := PBYTE(xout);
  n := Size;

  ZeroMemory(@wIn, sizeof(WORD) * MAXCHANNELS);
  ZeroMemory(@wOut, sizeof(WORD) * MAXCHANNELS);

  for i := 0 to n - 1 do
  begin

    with p^ do
    begin
      accum := FromInput(p, @wIn, accum);

      // it seems that disabling cache it speed up!
      (*
      if (comparemem(@wIn, @p ^.CacheIn, sizeof(WORD) * MAXCHANNELS) ) then
      begin

        CopyMemory(@wOut, @p ^.CacheOut, sizeof(WORD) * MAXCHANNELS);
      end
      else
      begin
      *)

        //CopyMemory(@p ^.CacheIn, @wIn, sizeof(WORD) * MAXCHANNELS);

      cmsEvalLUT(DeviceLink, @wIn, @wOut);

      //CopyMemory(@p ^.CacheOut, @wOut, sizeof(WORD) * MAXCHANNELS);

    //end;

      output := ToOutput(p, @wOut, output);
    end;
  end;
end;

procedure SetPrecalculatedTransform(p: _LPcmsTRANSFORM; dwFlags: DWORD);
begin
  p^.xform := PrecalculatedXFORM;

  if ((dwFlags and cmsFLAGS_NOTCACHE) = 0) then
  begin

    ZeroMemory(@p^.CacheIn, sizeof(WORD) * MAXCHANNELS);
    cmsEvalLUT(p^.DeviceLink, @p^.CacheIn, @p^.CacheOut);
    p^.xform := CachedXFORM;
  end;

end;

// Transform is identified as device-link

function CreateDeviceLinkTransform(p: _LPcmsTRANSFORM; dwFlags: DWORD): cmsHPROFILE;
begin

  if (not IsProperColorSpace(p^.InputProfile, p^.InputFormat, FALSE)) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "Device link is operating on wrong colorspace on input");
    result := nil;
    exit;
  end;

  if (not IsProperColorSpace(p^.InputProfile, p^.OutputFormat, TRUE)) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "Device link is operating on wrong colorspace on output");
    result := nil;
    exit;
  end;

  p^.DeviceLink := cmsReadICCLut(p^.InputProfile, icSigAToB0Tag);

  if (p^.DeviceLink = nil) then
  begin

    //cmsSignalError(LCMS_ERRC_ABORTED, "Noncompliant device-link profile");
    IEcmsDeleteTransform(cmsHTRANSFORM(p));
    result := nil;
    exit;
  end;

  if (p^.PreviewProfile <> nil) then
  begin
    //cmsSignalError(LCMS_ERRC_WARNING, "Proofing not supported on device link transforms");
  end;

  if (p^.OutputProfile <> nil) then
  begin
    //cmsSignalError(LCMS_ERRC_WARNING, "Output profile should be NULL, since this is a device-link transform");
  end;

  p^.Phase1 := -1;
  p^.Phase2 := -1;
  p^.Phase3 := -1;

  SetPrecalculatedTransform(p, dwFlags);

  p^.ExitColorSpace := cmsGetPCS(p^.InputProfile);

  // Precalculated device-link profile is ready
  result := cmsHTRANSFORM(p);
end;

function cmsAllocNamedColorList(n: integer): LPcmsNAMEDCOLORLIST;
var
  size: integer;
  v: LPcmsNAMEDCOLORLIST;
begin
  size := sizeof(cmsNAMEDCOLORLIST) + (n - 1) * sizeof(cmsNAMEDCOLOR);

  getmem(v, size);

  if (v = nil) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "Out of memory creating named color list");
    result := nil;
    exit;
  end;

  ZeroMemory(v, size);

  v^.nColors := n;
  v^.Allocated := n;
  v^.Prefix[0] := #0;
  v^.Suffix[0] := #0;

  result := v;
end;

function CheckHeader(v: LPcmsNAMEDCOLORLIST; nc2: PicNamedColor2): longbool;
begin
  if (v^.Prefix[0] = #0) and (v^.Suffix[0] = #0) and (v^.ColorantCount = 0) then
  begin
    result := TRUE;
    exit;
  end;

  if (strcomp(PAnsiChar(@v^.Prefix), PAnsiChar(@nc2^.prefix)) <> 0) then
  begin
    result := FALSE;
    exit;
  end;
  if (strcomp(PAnsiChar(@v^.Suffix), PAnsiChar(@nc2^.suffix)) <> 0) then
  begin
    result := FALSE;
    exit;
  end;

  result := (v^.ColorantCount = nc2^.nDeviceCoords);
end;

function GrowNamedColorList(v: LPcmsNAMEDCOLORLIST; ByElements: integer): LPcmsNAMEDCOLORLIST;
var
  TheNewList: LPcmsNAMEDCOLORLIST;
  NewElements: integer;
begin
  if (ByElements > v^.Allocated) then
  begin

    if (v^.Allocated = 0) then
      NewElements := 64
    else
      NewElements := v^.Allocated;

    while (ByElements > NewElements) do
      NewElements := NewElements * 2;

    getmem(TheNewList, sizeof(cmsNAMEDCOLORLIST) + (sizeof(cmsNAMEDCOLOR) * NewElements));

    if (TheNewList = nil) then
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "Out of memory reallocating named color list");
      result := nil;
      exit;
    end
    else
    begin
      CopyMemory(TheNewList, v, sizeof(cmsNAMEDCOLORLIST) + (v^.nColors - 1) * sizeof(cmsNAMEDCOLOR));
      TheNewList^.Allocated := NewElements;

      freemem(v);
      result := TheNewList;
      exit;
    end;
  end;

  result := v;
end;

function cmsAppendNamedColor(xform: cmsHTRANSFORM; Name: PAnsiChar; PCS: pwordarray; Colorant: pwordarray): longbool;
var
  v: _LPcmsTRANSFORM;
  List: LPcmsNAMEDCOLORLIST;
  i: integer;
begin
  v := _LPcmsTRANSFORM(xform);

  if (v^.NamedColorList = nil) then
  begin
    result := FALSE;
    exit;
  end;

  v^.NamedColorList := GrowNamedColorList(v^.NamedColorList, v^.NamedColorList^.nColors + 1);

  List := v^.NamedColorList;

  for i := 0 to MAXCHANNELS - 1 do
    List^.List[List^.nColors].DeviceColorant[i] := Colorant[i];

  for i := 0 to 2 do
    List^.List[List^.nColors].PCS[i] := PCS[i];

  strlcopy(List^.List[List^.nColors].Name, Name, MAX_PATH - 1);

  inc(List^.nColors);
  result := TRUE;
end;

function cmsReadICCnamedColorList(xform: cmsHTRANSFORM; hProfile: cmsHPROFILE; sig: icTagSignature): integer;
var
  v: _LPcmsTRANSFORM;
  Icc: LPLCMSICCPROFILE;
  n: integer;
  Base: icTagBase;
  offset: integer;
  size: integer;
  nc2: icNamedColor2;
  i, j: integer;
  PCS: array[0..2] of WORD;
  Colorant: array[0..MAXCHANNELS - 1] of WORD;
  Root: array[0..32] of AnsiChar;
begin
  v := _LPcmsTRANSFORM(xform);

  Icc := LPLCMSICCPROFILE(hProfile);

  n := SearchTag(Icc, sig);
  if (n < 0) then
  begin

    //cmsSignalError(LCMS_ERRC_WARNING, "Named color tag not found");
    result := 0;
    exit;
  end;

  if (Icc^.stream = nil) then
  begin

    size := Icc^.TagSizes[n];

    if (v^.NamedColorList <> nil) then
      cmsFreeNamedColorList(v^.NamedColorList);
    getmem(v^.NamedColorList, size);
    CopyMemory(v^.NamedColorList, Icc^.TagPtrs[n], size);
    result := v^.NamedColorList^.nColors;
    exit;
  end;

  offset := Icc^.TagOffsets[n];

  if (Icc^.Seek(Icc^.stream, offset)) then
  begin
    result := 0;
    exit;
  end;

  Icc^.Read(@Base, 1, sizeof(icTagBase), Icc^.stream);
  AdjustEndianess32(@Base.sig);

  case (Base.sig) of

    icSigNamedColorType:
      begin
        //cmsSignalError(LCMS_ERRC_WARNING, "Ancient named color profiles are not supported.");
        result := 0;
        exit;
      end;

    icSigNamedColor2Type:
      begin

        Icc^.Read(@nc2, sizeof(icNamedColor2) - SIZEOF_UINT8_ALIGNED, 1, Icc^.stream);
        AdjustEndianess32(@nc2.vendorFlag);
        AdjustEndianess32(@nc2.count);
        AdjustEndianess32(@nc2.nDeviceCoords);

        if (not CheckHeader(v^.NamedColorList, @nc2)) then
        begin
          //cmsSignalError(LCMS_ERRC_WARNING, "prefix/suffix/device for named color profiles mismatch.");
        end;

        strlcopy(v^.NamedColorList^.Prefix, @nc2.prefix, 32);
        strlcopy(v^.NamedColorList^.Suffix, @nc2.suffix, 32);
        v^.NamedColorList^.Suffix[32] := #0;
        v^.NamedColorList^.Prefix[32] := v^.NamedColorList^.Suffix[32];

        v^.NamedColorList^.ColorantCount := nc2.nDeviceCoords;

        for i := 0 to nc2.count - 1 do
        begin

          ZeroMemory(@Colorant, sizeof(WORD) * MAXCHANNELS);
          Icc^.Read(@Root, 1, 32, Icc^.stream);
          Icc^.Read(@PCS, 3, sizeof(WORD), Icc^.stream);

          for j := 0 to 2 do
            AdjustEndianess16(@PCS[j]);

          Icc^.Read(@Colorant, sizeof(WORD), nc2.nDeviceCoords, Icc^.stream);

          for j := 0 to nc2.nDeviceCoords - 1 do
            AdjustEndianess16(@Colorant[j]);

          cmsAppendNamedColor(v, Root, @PCS, @Colorant);
        end;

        result := v^.NamedColorList^.nColors;
        exit;
      end;
  else
    begin
      //cmsSignalError(LCMS_ERRC_WARNING, "Bad tag signature '%lx' found.", Base.sig);
      result := 0;
      exit;
    end;
  end;

end;

procedure NC2deviceXform(p: _LPcmsTRANSFORM; xin: pointer; xout: pointer; Size: integer);
var
  accum: pbyte;
  output: pbyte;
  wIn, wOut: array[0..MAXCHANNELS - 1] of WORD;
  i: dword;
begin

  accum := xin;
  output := xout;

  for i := 0 to Size - 1 do
  begin

    accum := p^.FromInput(p, @wIn, accum);
    CopyMemory(@wOut, @(p^.NamedColorList^.List[wIn[0]].DeviceColorant), sizeof(WORD) * MAXCHANNELS);
    output := p^.ToOutput(p, @wOut, output);
  end;

end;

function GetPhase(hProfile: cmsHPROFILE): integer;
begin
  case (cmsGetPCS(hProfile)) of

    icSigXYZData:
      begin
        result := XYZRel;
        exit;
      end;

    icSigLabData:
      begin
        result := LabRel;
        exit;
      end;

  else
    ; //cmsSignalError(LCMS_ERRC_ABORTED, "Invalid PCS");
  end;

  result := XYZRel;
end;

function cmsIsTag(hProfile: cmsHPROFILE; sig: icTagSignature): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  result := SearchTag(Icc, sig) >= 0;

end;

procedure cmsSetDeviceClass(hProfile: cmsHPROFILE; sig: icProfileClassSignature);
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);
  Icc^.DeviceClass := sig;
end;

procedure cmsSetColorSpace(hProfile: cmsHPROFILE; sig: icColorSpaceSignature);
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);
  Icc^.ColorSpace := sig;
end;

procedure cmsSetPCS(hProfile: cmsHPROFILE; pcs: icColorSpaceSignature);
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);
  Icc^.PCS := pcs;
end;

function InitTag(Icc: LPLCMSICCPROFILE; sig: icTagSignature; size: integer; Init: pointer): pointer;
var
  Ptr: pointer;
  i: icInt32Number;
begin
  i := SearchTag(Icc, sig);
  if (i >= 0) then
  begin

    if (Icc^.TagPtrs[i] <> nil) then
      freemem(Icc^.TagPtrs[i]);
  end
  else
  begin
    i := Icc^.TagCount;
    inc(Icc^.TagCount);
  end;

  getmem(Ptr, size);
  CopyMemory(Ptr, Init, size);

  Icc^.TagNames[i] := sig;
  Icc^.TagSizes[i] := size;
  Icc^.TagPtrs[i] := Ptr;

  result := Ptr;
end;

function _cmsAddTextTag(hProfile: cmsHPROFILE; sig: icTagSignature; Text: PAnsiChar): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  InitTag(Icc, sig, IEStrLen(Text) + 1, Text);
  result := TRUE;
end;

procedure cmsSetRenderingIntent(hProfile: cmsHPROFILE; RenderingIntent: integer);
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);
  Icc^.RenderingIntent := icRenderingIntent(RenderingIntent);
end;

function _cmsAddXYZTag(hProfile: cmsHPROFILE; sig: icTagSignature; XYZ: LPcmsCIEXYZ): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  InitTag(Icc, sig, sizeof(cmsCIEXYZ), XYZ);
  result := TRUE;
end;

function SizeOfGammaTab(xIn: LPGAMMATABLE): integer;
begin
  result := sizeof(GAMMATABLE) + (xIn^.nEntries - 1) * sizeof(WORD);
end;

function _cmsAddGammaTag(hProfile: cmsHPROFILE; sig: icTagSignature; TransferFunction: LPGAMMATABLE): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  InitTag(Icc, sig, SizeOfGammaTab(TransferFunction), TransferFunction);
  result := TRUE;
end;

function DupBlock(Icc: LPLCMSICCPROFILE; Block: pointer; size: integer): pointer;
begin
  if (Block <> nil) and (size > 0) then
    result := InitTag(Icc, icTagSignature(0), size, Block)
  else
    result := nil;
end;

function _cmsAddLUTTag(hProfile: cmsHPROFILE; sig: icTagSignature; xlut: pointer): longbool;
var
  Icc: LPLCMSICCPROFILE;
  Orig, Stored: LPLUT;
  i: dword;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  Orig := LPLUT(xlut);
  Stored := LPLUT(InitTag(Icc, icTagSignature(sig), sizeof(LUT), xlut));

  for i := 0 to Orig^.InputChan - 1 do
    Stored^.L1[i] := DupBlock(Icc, Orig^.L1[i], sizeof(WORD) * Orig^.In16params.nSamples);

  for i := 0 to Orig^.OutputChan - 1 do
    Stored^.L2[i] := DupBlock(Icc, Orig^.L2[i], sizeof(WORD) * Orig^.Out16params.nSamples);

  Stored^.T := DupBlock(Icc, Orig^.T, Orig^.Tsize);

  result := TRUE;
end;

function _cmsAddChromaticityTag(hProfile: cmsHPROFILE; sig: icTagSignature; Chrm: LPcmsCIExyYTRIPLE): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  InitTag(Icc, sig, sizeof(cmsCIExyYTRIPLE), Chrm);
  result := TRUE;
end;

function _cmsAddSequenceDescriptionTag(hProfile: cmsHPROFILE; sig: icTagSignature; pseq: LPcmsSEQ): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  InitTag(Icc, sig, sizeof(integer) + pseq^.n * sizeof(cmsPSEQDESC), pseq);
  result := TRUE;

end;

function _cmsAddNamedColorTag(hProfile: cmsHPROFILE; sig: icTagSignature; nc: LPcmsNAMEDCOLORLIST): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  InitTag(Icc, sig, sizeof(cmsNAMEDCOLORLIST) + (nc^.nColors - 1) * sizeof(cmsNAMEDCOLOR), nc);
  result := FALSE;
end;

function cmsAddTag(hProfile: cmsHPROFILE; sig: icTagSignature; Tag: pointer): longbool;
var
  rc: longbool;
begin

  case (sig) of

    icSigCharTargetTag,
      icSigCopyrightTag,
      icSigProfileDescriptionTag,
      icSigDeviceMfgDescTag,
      icSigDeviceModelDescTag:
      rc := _cmsAddTextTag(hProfile, sig, PAnsiChar(Tag));

    icSigRedColorantTag,
      icSigGreenColorantTag,
      icSigBlueColorantTag,
      icSigMediaWhitePointTag,
      icSigMediaBlackPointTag:
      rc := _cmsAddXYZTag(hProfile, sig, LPcmsCIEXYZ(Tag));

    icSigRedTRCTag,
      icSigGreenTRCTag,
      icSigBlueTRCTag,
      icSigGrayTRCTag:
      rc := _cmsAddGammaTag(hProfile, sig, LPGAMMATABLE(Tag));

    icSigAToB0Tag,
      icSigAToB1Tag,
      icSigAToB2Tag,
      icSigBToA0Tag,
      icSigBToA1Tag,
      icSigBToA2Tag,
      icSigGamutTag,
      icSigPreview0Tag,
      icSigPreview1Tag,
      icSigPreview2Tag:
      rc := _cmsAddLUTTag(hProfile, sig, Tag);

    icTagSignature(icSigChromaticityTag):
      rc := _cmsAddChromaticityTag(hProfile, sig, LPcmsCIExyYTRIPLE(Tag));

    icSigProfileSequenceDescTag:
      rc := _cmsAddSequenceDescriptionTag(hProfile, sig, LPcmsSEQ(Tag));

    icSigNamedColor2Tag:
      rc := _cmsAddNamedColorTag(hProfile, sig, LPcmsNAMEDCOLORLIST(Tag));

  else
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "cmsAddTag: Tag '%x' is unsupported", sig);
      result := FALSE;
      exit;
    end;
  end;

  case (sig) of

    icSigMediaWhitePointTag,
      icSigMediaBlackPointTag,
      icTagSignature(icSigChromaticAdaptationTag):

      ReadCriticalTags(LPLCMSICCPROFILE(hProfile));

  end;

  result := rc;

end;

procedure cmsxyY2XYZ(Dest: LPcmsCIEXYZ; Source: LPcmsCIExyY); {$ifdef IESUPPORTINLINE} inline; {$endif}
begin

  Dest^.X := (Source^.x / Source^.y_mi) * Source^.Y_ma;
  Dest^.Y := Source^.Y_ma;
  Dest^.Z := ((1 - Source^.x - Source^.y_mi) / Source^.y_mi) * Source^.Y_ma;
end;

function cmsBuildRGB2XYZtransferMatrix(r: LPMAT3; WhitePt: LPcmsCIExyY; Primrs: LPcmsCIExyYTRIPLE): longbool;
var
  WhitePoint, Coef: VEC3;
  xResult, Primaries: MAT3;
  xn, yn: double;
  xr, yr: double;
  xg, yg: double;
  xb, yb: double;
begin

  xn := WhitePt^.x;
  yn := WhitePt^.y_mi;
  xr := Primrs^.Red.x;
  yr := Primrs^.Red.y_mi;
  xg := Primrs^.Green.x;
  yg := Primrs^.Green.y_mi;
  xb := Primrs^.Blue.x;
  yb := Primrs^.Blue.y_mi;

  VEC3init(@Primaries.v[0], xr, xg, xb);
  VEC3init(@Primaries.v[1], yr, yg, yb);
  VEC3init(@Primaries.v[2], (1 - xr - yr), (1 - xg - yg), (1 - xb - yb));

  if (MAT3inverse(@Primaries, @xResult) = 0) then
  begin
    result := FALSE;
    exit;
  end;

  VEC3init(@WhitePoint, xn / yn, 1.0, (1.0 - xn - yn) / yn);

  MAT3eval(@Coef, @xResult, @WhitePoint);

  VEC3init(@r^.v[0], Coef.n[VX] * xr, Coef.n[VY] * xg, Coef.n[VZ] * xb);
  VEC3init(@r^.v[1], Coef.n[VX] * yr, Coef.n[VY] * yg, Coef.n[VZ] * yb);
  VEC3init(@r^.v[2], Coef.n[VX] * (1.0 - xr - yr), Coef.n[VY] * (1.0 - xg - yg), Coef.n[VZ] * (1.0 - xb - yb));

  result := TRUE;
end;

function TransportValue32(Value: icInt32Number): icInt32Number;
var
  Temp: icInt32Number;
begin
  Temp := Value;

  AdjustEndianess32(@Temp);
  result := Temp;
end;

function TransportValue16(Value: WORD): WORD;
var
  Temp: WORD;
begin
  Temp := Value;

  AdjustEndianess16(@Temp);
  result := Temp;
end;

procedure EncodeDateTime(DateTime: PicDateTimeNumber);
var
  tm: TDateTime;
  year, month, day: word;
  hour, min, sec, msec: word;
begin

  tm := date + time;
  DecodeDate(tm, year, month, day);
  DecodeTime(tm, hour, min, sec, msec);

  DateTime^.year := TransportValue16(year);
  DateTime^.month := TransportValue16(month);
  DateTime^.day := TransportValue16(day);
  DateTime^.hours := TransportValue16(hour);
  DateTime^.minutes := TransportValue16(min);
  DateTime^.seconds := TransportValue16(sec);

end;

function SaveHeader(OutStream: TStream; Icc: LPLCMSICCPROFILE): longbool;
var
  Header: icHeader;
begin

  Header.size := TransportValue32(icInt32Number(UsedSpace));
  Header.cmmId := TransportValue32(lcmsSignature);
  Header.version := TransportValue32(icInt32Number($02300000));
  Header.deviceClass := icProfileClassSignature(TransportValue32(integer(Icc^.DeviceClass)));
  Header.colorSpace := icColorSpaceSignature(TransportValue32(integer(Icc^.ColorSpace)));
  Header.pcs := icColorSpaceSignature(TransportValue32(integer(Icc^.PCS)));

  EncodeDateTime(@Header.date);

  Header.magic := TransportValue32(icMagicNumber);
  Header.xplatform := icPlatformSignature(TransportValue32(integer(icSigMicrosoft)));

  Header.flags := TransportValue32(Icc^.flags);
  Header.manufacturer := TransportValue32(lcmsSignature);
  Header.model := TransportValue32(0);
  Header.attributes[0] := TransportValue32(0);
  Header.attributes[1] := TransportValue32(0);

  Header.renderingIntent := TransportValue32(integer(Icc^.RenderingIntent));

  Header.illuminant.X := TransportValue32(DOUBLE_TO_FIXED(Icc^.Illuminant.X));
  Header.illuminant.Y := TransportValue32(DOUBLE_TO_FIXED(Icc^.Illuminant.Y));
  Header.illuminant.Z := TransportValue32(DOUBLE_TO_FIXED(Icc^.Illuminant.Z));

  Header.creator := TransportValue32(lcmsSignature);

  ZeroMemory(@Header.reserved, sizeof(Header.reserved));

  CopyMemory(@Header.reserved, @Icc^.ProfileID, 16);

  UsedSpace := 0;

  result := Icc^.Write(OutStream, sizeof(icHeader), @Header);
end;

function SaveTagDirectory(OutStream: TStream; Icc: LPLCMSICCPROFILE): longbool;
var
  i: icInt32Number;
  Tag: icTag;
  Count: icInt32Number;
begin
  Count := 0;

  for i := 0 to Icc^.TagCount - 1 do
  begin
    if (Icc^.TagNames[i] <> icTagSignature(0)) then
      inc(Count);
  end;

  Count := TransportValue32(Count);
  if (not Icc^.Write(OutStream, sizeof(icInt32Number), @Count)) then
  begin
    result := FALSE;
    exit;
  end;

  for i := 0 to Icc^.TagCount - 1 do
  begin

    if (Icc^.TagNames[i] = icTagSignature(0)) then
      continue;

    Tag.sig := icTagSignature(TransportValue32(integer(Icc^.TagNames[i])));
    Tag.offset := TransportValue32(icInt32Number(Icc^.TagOffsets[i]));
    Tag.size := TransportValue32(icInt32Number(Icc^.TagSizes[i]));

    if (not Icc^.Write(OutStream, sizeof(icTag), @Tag)) then
    begin
      result := FALSE;
      exit;
    end;
  end;

  result := TRUE;
end;

function ALIGNLONG(x: dword): dword; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := (((x) + 3) and not (3)); // Aligns to DWORD boundary
end;

function SetupBase(OutStream: TStream; sig: icTagTypeSignature; Icc: LPLCMSICCPROFILE): longbool;
var
  Base: icTagBase;
begin
  Base.sig := icTagTypeSignature(TransportValue32(integer(sig)));
  ZeroMemory(@Base.reserved, sizeof(Base.reserved));
  result := Icc^.Write(OutStream, sizeof(icTagBase), @Base);
end;

function SaveDescription(OutStream: TStream; Text: PAnsiChar; Icc: LPLCMSICCPROFILE): longbool;
var
  len, Count, TotalSize, AlignedSize: icUInt32Number;
  Filler: array[0..255] of AnsiChar;
begin

  len := icUInt32Number((IEStrLen(Text) + 1));

  TotalSize := sizeof(icTagBase) + sizeof(icUInt32Number) + len +
    sizeof(icUInt32Number) + sizeof(icUInt32Number) +
    sizeof(icUInt16Number) + sizeof(icUInt8Number) + 67;

  AlignedSize := TotalSize;

  if (not SetupBase(OutStream, icSigTextDescriptionType, Icc)) then
  begin
    result := FALSE;
    exit;
  end;
  AlignedSize := AlignedSize - sizeof(icTagBase);

  Count := TransportValue32(len);
  if (not Icc^.Write(OutStream, sizeof(icUInt32Number), @Count)) then
  begin
    result := FALSE;
    exit;
  end;
  AlignedSize := AlignedSize - sizeof(icUInt32Number);

  if (not Icc^.Write(OutStream, len, pointer(Text))) then
  begin
    result := FALSE;
    exit;
  end;
  AlignedSize := AlignedSize - len;

  ZeroMemory(@Filler, AlignedSize);
  if (not Icc^.Write(OutStream, AlignedSize, @Filler)) then
  begin
    result := FALSE;
    exit;
  end;

  result := TRUE;
end;

function SaveXYZNumber(OutStream: TStream; Value: LPcmsCIEXYZ; Icc: LPLCMSICCPROFILE): longbool;
var
  XYZ: icXYZNumber;
begin

  if (not SetupBase(OutStream, icSigXYZType, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  XYZ.X := TransportValue32(DOUBLE_TO_FIXED(Value^.X));
  XYZ.Y := TransportValue32(DOUBLE_TO_FIXED(Value^.Y));
  XYZ.Z := TransportValue32(DOUBLE_TO_FIXED(Value^.Z));

  result := Icc^.Write(OutStream, sizeof(icXYZNumber), @XYZ);
end;

function SaveGamma(OutStream: TStream; Gamma: LPGAMMATABLE; Icc: LPLCMSICCPROFILE): longbool;
var
  Count: icInt32Number;
  i: integer;
  Val: WORD;
begin

  if (not SetupBase(OutStream, icSigCurveType, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  Count := TransportValue32(Gamma^.nEntries);

  if (not Icc^.Write(OutStream, sizeof(icInt32Number), @Count)) then
  begin
    result := FALSE;
    exit;
  end;

  for i := 0 to Gamma^.nEntries - 1 do
  begin
    Val := TransportValue16(Gamma^.GammaTable[i]);

    if (not Icc^.Write(OutStream, sizeof(WORD), @Val)) then
    begin
      result := FALSE;
      exit;
    end;
  end;

  result := TRUE;
end;

function SaveText(OutStream: TStream; Text: PAnsiChar; Icc: LPLCMSICCPROFILE): longbool;
var
  len: integer;
begin
  len := IEStrLen(Text) + 1;

  if (not SetupBase(OutStream, icSigTextType, Icc)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not Icc^.Write(OutStream, len, pointer(Text))) then
  begin
    result := FALSE;
    exit;
  end;
  result := TRUE;
end;

function SaveOneChromaticity(OutStream: TStream; x, y: double; Icc: LPLCMSICCPROFILE): longbool;
var
  xf, yf: Fixed32;
begin

  xf := TransportValue32(DOUBLE_TO_FIXED(x));
  yf := TransportValue32(DOUBLE_TO_FIXED(y));

  if (not Icc^.Write(OutStream, sizeof(Fixed32), @xf)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not Icc^.Write(OutStream, sizeof(Fixed32), @yf)) then
  begin
    result := FALSE;
    exit;
  end;

  result := TRUE;
end;

function SaveChromaticities(OutStream: TStream; chrm: LPcmsCIExyYTRIPLE; Icc: LPLCMSICCPROFILE): longbool;
var
  nChans, Table: WORD;
begin

  if (not SetupBase(OutStream, icTagTypeSignature(icSigChromaticityType), Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  nChans := TransportValue16(3);
  if (not Icc^.Write(OutStream, sizeof(WORD), @nChans)) then
  begin
    result := FALSE;
    exit;
  end;
  Table := TransportValue16(0);
  if (not Icc^.Write(OutStream, sizeof(WORD), @Table)) then
  begin
    result := FALSE;
    exit;
  end;

  if (not SaveOneChromaticity(OutStream, chrm^.Red.x, chrm^.Red.y_mi, Icc)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not SaveOneChromaticity(OutStream, chrm^.Green.x, chrm^.Green.y_mi, Icc)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not SaveOneChromaticity(OutStream, chrm^.Blue.x, chrm^.Blue.y_mi, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  result := TRUE;
end;

function SaveLUT8(OutStream: TStream; NewLUT: LPLUT; Icc: LPLCMSICCPROFILE): longbool;
var
  LUT8: icLut8;
  i, j: integer;
  nTabSize: integer;
  val: BYTE;
begin

  if (NewLUT^.wFlags and LUT_HASTL1) <> 0 then
  begin

    if (NewLUT^.InputEntries <> 256) then
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "LUT8 needs 256 entries on prelinearization");
      result := FALSE;
      exit;
    end;

  end;

  if (NewLUT^.wFlags and LUT_HASTL2) <> 0 then
  begin

    if (NewLUT^.OutputEntries <> 256) then
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "LUT8 needs 256 entries on postlinearization");
      result := FALSE;
      exit;
    end;
  end;

  if (not SetupBase(OutStream, icSigLut8Type, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  LUT8.clutPoints := icUInt8Number(NewLUT^.cLutPoints);
  LUT8.inputChan := icUInt8Number(NewLUT^.InputChan);
  LUT8.outputChan := icUInt8Number(NewLUT^.OutputChan);

  if (NewLUT^.wFlags and LUT_HASMATRIX) <> 0 then
  begin

    LUT8.e00 := TransportValue32(NewLUT^.Matrix.v[0].n[0]);
    LUT8.e01 := TransportValue32(NewLUT^.Matrix.v[0].n[1]);
    LUT8.e02 := TransportValue32(NewLUT^.Matrix.v[0].n[2]);
    LUT8.e10 := TransportValue32(NewLUT^.Matrix.v[1].n[0]);
    LUT8.e11 := TransportValue32(NewLUT^.Matrix.v[1].n[1]);
    LUT8.e12 := TransportValue32(NewLUT^.Matrix.v[1].n[2]);
    LUT8.e20 := TransportValue32(NewLUT^.Matrix.v[2].n[0]);
    LUT8.e21 := TransportValue32(NewLUT^.Matrix.v[2].n[1]);
    LUT8.e22 := TransportValue32(NewLUT^.Matrix.v[2].n[2]);
  end
  else
  begin

    LUT8.e00 := TransportValue32(DOUBLE_TO_FIXED(1));
    LUT8.e01 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT8.e02 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT8.e10 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT8.e11 := TransportValue32(DOUBLE_TO_FIXED(1));
    LUT8.e12 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT8.e20 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT8.e21 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT8.e22 := TransportValue32(DOUBLE_TO_FIXED(1));
  end;

  Icc^.Write(OutStream, sizeof(icLut8) - SIZEOF_UINT8_ALIGNED, @LUT8);

  for i := 0 to NewLUT^.InputChan - 1 do
  begin

    for j := 0 to 255 do
    begin

      if (NewLUT^.wFlags and LUT_HASTL1) <> 0 then
        val := iefloor(pwordarray(NewLUT^.L1[i])[j] / 257.0 + 0.5)
      else
        val := j;

      Icc^.Write(OutStream, 1, @val);
    end;

  end;

  nTabSize := (NewLUT^.OutputChan * uipow(NewLUT^.cLutPoints,
    NewLUT^.InputChan));

  for j := 0 to nTabSize - 1 do
  begin

    val := iefloor(pwordarray(NewLUT^.T)[j] / 257.0 + 0.5);
    Icc^.Write(OutStream, 1, @val);
  end;

  for i := 0 to NewLUT^.OutputChan - 1 do
  begin

    for j := 0 to 255 do
    begin

      if (NewLUT^.wFlags and LUT_HASTL2) <> 0 then
        val := iefloor(pwordarray(NewLUT^.L2[i])[j] / 257.0 + 0.5)
      else
        val := j;

      Icc^.Write(OutStream, 1, @val);
    end;

  end;

  result := TRUE;
end;

function SaveWordsTable(OutStream: TStream; nEntries: integer; Tab: PWORD; Icc: LPLCMSICCPROFILE): longbool;
var
  nTabSize: integer;
  PtrW: PWORD;
begin
  nTabSize := sizeof(WORD) * nEntries;
  getmem(PtrW, nTabSize);

  if (PtrW = nil) then
  begin
    result := FALSE;
    exit;
  end;
  CopyMemory(PtrW, Tab, nTabSize);
  AdjustEndianessArray16(PtrW, nEntries);
  Icc^.Write(OutStream, nTabSize, PtrW);
  freemem(PtrW);

  result := TRUE;
end;

function SaveLUT(OutStream: TStream; NewLUT: LPLUT; Icc: LPLCMSICCPROFILE): longbool;
const
  NullTbl: array[0..1] of WORD = (0, $FFFF);
var
  LUT16: icLut16;
  i: dword;
  nTabSize: integer;
begin

  if (not SetupBase(OutStream, icSigLut16Type, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  LUT16.clutPoints := icUInt8Number(NewLUT^.cLutPoints);
  LUT16.inputChan := icUInt8Number(NewLUT^.InputChan);
  LUT16.outputChan := icUInt8Number(NewLUT^.OutputChan);

  if (NewLUT^.wFlags and LUT_HASTL1) <> 0 then
    LUT16.inputEnt := TransportValue16((NewLUT^.InputEntries))
  else
    LUT16.inputEnt := TransportValue16((2));

  if (NewLUT^.wFlags and LUT_HASTL2) <> 0 then
    LUT16.outputEnt := TransportValue16((NewLUT^.OutputEntries))
  else
    LUT16.outputEnt := TransportValue16((2));

  if (NewLUT^.wFlags and LUT_HASMATRIX) <> 0 then
  begin

    LUT16.e00 := TransportValue32(NewLUT^.Matrix.v[0].n[0]);
    LUT16.e01 := TransportValue32(NewLUT^.Matrix.v[0].n[1]);
    LUT16.e02 := TransportValue32(NewLUT^.Matrix.v[0].n[2]);
    LUT16.e10 := TransportValue32(NewLUT^.Matrix.v[1].n[0]);
    LUT16.e11 := TransportValue32(NewLUT^.Matrix.v[1].n[1]);
    LUT16.e12 := TransportValue32(NewLUT^.Matrix.v[1].n[2]);
    LUT16.e20 := TransportValue32(NewLUT^.Matrix.v[2].n[0]);
    LUT16.e21 := TransportValue32(NewLUT^.Matrix.v[2].n[1]);
    LUT16.e22 := TransportValue32(NewLUT^.Matrix.v[2].n[2]);
  end
  else
  begin

    LUT16.e00 := TransportValue32(DOUBLE_TO_FIXED(1));
    LUT16.e01 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT16.e02 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT16.e10 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT16.e11 := TransportValue32(DOUBLE_TO_FIXED(1));
    LUT16.e12 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT16.e20 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT16.e21 := TransportValue32(DOUBLE_TO_FIXED(0));
    LUT16.e22 := TransportValue32(DOUBLE_TO_FIXED(1));
  end;

  Icc^.Write(OutStream, sizeof(icLut16) - SIZEOF_UINT16_ALIGNED, @LUT16);

  for i := 0 to NewLUT^.InputChan - 1 do
  begin

    if (NewLUT^.wFlags and LUT_HASTL1) <> 0 then
    begin

      if (not SaveWordsTable(OutStream, NewLUT^.InputEntries, NewLUT^.L1[i], Icc)) then
      begin
        result := FALSE;
        exit;
      end;

    end
    else
      Icc^.Write(OutStream, sizeof(WORD) * 2, @NullTbl);
  end;

  nTabSize := (NewLUT^.OutputChan * uipow(NewLUT^.cLutPoints, NewLUT^.InputChan));

  if (not SaveWordsTable(OutStream, nTabSize, NewLUT^.T, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  for i := 0 to NewLUT^.OutputChan - 1 do
  begin

    if (NewLUT^.wFlags and LUT_HASTL2) <> 0 then
    begin

      if (not SaveWordsTable(OutStream, NewLUT^.OutputEntries, NewLUT^.L2[i], Icc)) then
      begin
        result := FALSE;
        exit;
      end;
    end
    else
      Icc^.Write(OutStream, sizeof(WORD) * 2, @NullTbl);

  end;

  result := TRUE;
end;

function SaveSequenceDescriptionTag(OutStream: TStream; seq: LPcmsSEQ; Icc: LPLCMSICCPROFILE): longbool;
var
  nSeqs: icUInt32Number;
  DescStruct: icDescStruct;
  i, n: integer;
  pseq: LPcmsPSEQDESC;
  sec: LPcmsPSEQDESC;
begin
  n := seq^.n;
  pseq := @seq^.seq;

  if (not SetupBase(OutStream, icSigProfileSequenceDescType, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  nSeqs := TransportValue32(n);

  if (not Icc^.Write(OutStream, sizeof(icUInt32Number), @nSeqs)) then
  begin
    result := FALSE;
    exit;
  end;

  for i := 0 to n - 1 do
  begin

    sec := pseq;
    inc(sec, i);

    DescStruct.deviceMfg := icSignature(icTagTypeSignature(TransportValue32(sec^.deviceMfg)));
    DescStruct.deviceModel := icSignature(icTagTypeSignature(TransportValue32(sec^.deviceModel)));
    DescStruct.technology := icTechnologySignature(TransportValue32(integer(sec^.technology)));
    DescStruct.attributes[0] := TransportValue32(sec^.attributes[0]);
    DescStruct.attributes[1] := TransportValue32(sec^.attributes[1]);

    if (not Icc^.Write(OutStream, sizeof(icDescStruct) - SIZEOF_UINT8_ALIGNED, @DescStruct)) then
    begin
      result := FALSE;
      exit;
    end;

    if (not SaveDescription(OutStream, sec^.Manufacturer, Icc)) then
    begin
      result := FALSE;
      exit;
    end;
    if (not SaveDescription(OutStream, sec^.Model, Icc)) then
    begin
      result := FALSE;
      exit;
    end;
  end;

  result := TRUE;
end;

function SaveNamedColorList(OutStream: TStream; NamedColorList: LPcmsNAMEDCOLORLIST; Icc: LPLCMSICCPROFILE): longbool;
var
  vendorFlag: icUInt32Number; // Bottom 16 bits for IC use
  count: icUInt32Number; // Count of named colors
  nDeviceCoords: icUInt32Number; // Num of device coordinates
  prefix: array[0..31] of icInt8Number; // Prefix for each color name
  suffix: array[0..31] of icInt8Number; // Suffix for each color name
  i: integer;
  PCS: array[0..2] of icUInt16Number;
  Colorant: array[0..MAXCHANNELS - 1] of icUInt16Number;
  root: array[0..31] of icInt8Number;
  Color: LPcmsNAMEDCOLOR;
  j: integer;
begin

  if (not SetupBase(OutStream, icSigNamedColor2Type, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  vendorFlag := TransportValue32(0);
  count := TransportValue32(NamedColorList^.nColors);
  nDeviceCoords := TransportValue32(NamedColorList^.ColorantCount);

  strlcopy(@prefix, NamedColorList^.Prefix, 32);
  strlcopy(@suffix, NamedColorList^.Suffix, 32);

  if (not Icc^.Write(OutStream, sizeof(icUInt32Number), @vendorFlag)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not Icc^.Write(OutStream, sizeof(icUInt32Number), @count)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not Icc^.Write(OutStream, sizeof(icUInt32Number), @nDeviceCoords)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not Icc^.Write(OutStream, 32, @prefix)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not Icc^.Write(OutStream, 32, @suffix)) then
  begin
    result := FALSE;
    exit;
  end;

  for i := 0 to NamedColorList^.nColors - 1 do
  begin

    Color := @NamedColorList^.List;
    inc(Color, i);

    strlcopy(@root, Color^.Name, 32);
    if (not Icc^.Write(OutStream, 32, @root)) then
    begin
      result := FALSE;
      exit;
    end;

    for j := 0 to 2 do
      PCS[j] := TransportValue16(Color^.PCS[j]);

    if (not Icc^.Write(OutStream, 3 * sizeof(icUInt16Number), @PCS)) then
    begin
      result := FALSE;
      exit;
    end;

    for j := 0 to NamedColorList^.ColorantCount - 1 do
      Colorant[j] := TransportValue16(Color^.DeviceColorant[j]);

    if (not Icc^.Write(OutStream, NamedColorList^.ColorantCount * sizeof(icUInt16Number), @Colorant)) then
    begin
      result := FALSE;
      exit;
    end;
  end;

  result := TRUE;
end;

function SaveTags(OutStream: TStream; Icc: LPLCMSICCPROFILE): longbool;
var
  Data: PBYTE;
  i: icInt32Number;
  xBegin: integer;
  AlignedSpace, FillerSize: integer;
  Filler: array[0..19] of BYTE;
begin

  for i := 0 to Icc^.TagCount - 1 do
  begin

    if (Icc^.TagNames[i] = icTagSignature(0)) then
      continue;

    AlignedSpace := ALIGNLONG(UsedSpace);
    FillerSize := AlignedSpace - UsedSpace;

    if (FillerSize > 0) then
    begin

      ZeroMemory(@Filler, 16);
      if (not Icc^.Write(OutStream, FillerSize, @Filler)) then
      begin
        result := FALSE;
        exit;
      end;
    end;

    xBegin := UsedSpace;
    Icc^.TagOffsets[i] := xBegin;
    Data := Icc^.TagPtrs[i];
    if (Data = nil) then
      continue;

    case (Icc^.TagNames[i]) of

      icSigProfileDescriptionTag,
        icSigDeviceMfgDescTag,
        icSigDeviceModelDescTag:
        if (not SaveDescription(OutStream, PAnsiChar(Data), Icc)) then
        begin
          result := FALSE;
          exit;
        end;

      icSigRedColorantTag,
        icSigGreenColorantTag,
        icSigBlueColorantTag,
        icSigMediaWhitePointTag,
        icSigMediaBlackPointTag:
        if (not SaveXYZNumber(OutStream, LPcmsCIEXYZ(Data), Icc)) then
        begin
          result := FALSE;
          exit;
        end;

      icSigRedTRCTag,
        icSigGreenTRCTag,
        icSigBlueTRCTag,
        icSigGrayTRCTag:
        if (not SaveGamma(OutStream, LPGAMMATABLE(Data), Icc)) then
        begin
          result := FALSE;
          exit;
        end;

      icSigCharTargetTag,
        icSigCopyrightTag:
        if (not SaveText(OutStream, PAnsiChar(Data), Icc)) then
        begin
          result := FALSE;
          exit;
        end;

      icTagSignature(icSigChromaticityTag):
        if (not SaveChromaticities(OutStream, LPcmsCIExyYTRIPLE(Data), Icc)) then
        begin
          result := FALSE;
          exit;
        end;

      icSigAToB0Tag,
        icSigAToB1Tag,
        icSigAToB2Tag,
        icSigBToA0Tag,
        icSigBToA1Tag,
        icSigBToA2Tag,
        icSigGamutTag,
        icSigPreview0Tag,
        icSigPreview1Tag,
        icSigPreview2Tag:

        if (Icc^.SaveAs8Bits) then
        begin

          if (not SaveLUT8(OutStream, LPLUT(Data), Icc)) then
          begin
            result := FALSE;
            exit;
          end;
        end
        else
        begin

          if (not SaveLUT(OutStream, LPLUT(Data), Icc)) then
          begin
            result := FALSE;
            exit;
          end;
        end;

      icSigProfileSequenceDescTag:
        if (not SaveSequenceDescriptionTag(OutStream, LPcmsSEQ(Data), Icc)) then
        begin
          result := FALSE;
          exit;
        end;

      icTagSignature(icSigNamedColor2Type):
        if (not SaveNamedColorList(OutStream, LPcmsNAMEDCOLORLIST(Data), Icc)) then
        begin
          result := FALSE;
          exit;
        end;

    else
      begin
        result := FALSE;
        exit;
      end;
    end;

    Icc^.TagSizes[i] := (UsedSpace - xBegin);
  end;

  result := TRUE;
end;

function _cmsSaveProfile(hProfile: cmsHPROFILE; stream: TStream): longbool;
var
  OutStream: TStream;
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  Icc^.Write := @FileWrite;

  if (not SaveHeader(nil, Icc)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not SaveTagDirectory(nil, Icc)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not SaveTags(nil, Icc)) then
  begin
    result := FALSE;
    exit;
  end;

  OutStream := stream;
  if (OutStream = nil) then
  begin
    result := FALSE;
    exit;
  end;

  if (SaveHeader(OutStream, Icc)) and
    (SaveTagDirectory(OutStream, Icc)) and
    (SaveTags(OutStream, Icc)) then
  begin

    //OutStream.Free; // do not free, because we don't control the stream object
    result := true;
  end
  else
  begin

    //OutStream.Free; // do not free, because we don't control the stream object
    //unlink(FileName);
    result := FALSE;
  end;
end;

function IEcmsCloseProfile(hProfile: cmsHPROFILE): longbool;
var
  icco: LPLCMSICCPROFILE;
  xfile: tstream;
  rc: longbool;
  i: icInt32Number;
begin
  icco := LPLCMSICCPROFILE(hProfile);
  rc := TRUE;

  if (icco = nil) then
  begin
    result := FALSE;
    exit;
  end;

  if (icco^.IsWrite) then
  begin

    icco^.IsWrite := FALSE;
    rc := _cmsSaveProfile(hProfile, icco^.PhysicalFile);
  end;

  xfile := icco^.stream;

  if (xfile = nil) then
  begin

    for i := 0 to icco^.TagCount - 1 do
    begin

      if (icco^.TagPtrs[i] <> nil) then
        freemem(icco^.TagPtrs[i]);
    end;

  end
  else
  begin
    icco^.Close(xfile);
  end;

  freemem(icco);

  result := rc;
end;

function cmsAdaptMatrixToD50(r: LPMAT3; SourceWhitePt: LPcmsCIExyY): longbool;
var
  Dn: cmsCIEXYZ;
  Bradford: MAT3;
  Tmp: MAT3;
begin

  cmsxyY2XYZ(@Dn, SourceWhitePt);

  cmsAdaptationMatrix(@Bradford, nil, @Dn, cmsD50_XYZ);

  Tmp := r^;
  MAT3per(r, @Bradford, @Tmp);

  result := TRUE;
end;

function cmsCreateRGBProfile(WhitePoint: LPcmsCIExyY; Primaries: LPcmsCIExyYTRIPLE; TransferFunction: LPGAMMATABLEArray): cmsHPROFILE;
var
  hICC: cmsHPROFILE;
  tmp: cmsCIEXYZ;
  MColorants: MAT3;
  Colorants: cmsCIEXYZTRIPLE;
  MaxWhite: cmsCIExyY;
begin

  hICC := _cmsCreateProfilePlaceholder;
  if (hICC = nil) then
  begin
    result := nil;
    exit;
  end;

  cmsSetDeviceClass(hICC, icSigDisplayClass);
  cmsSetColorSpace(hICC, icSigRgbData);
  cmsSetPCS(hICC, icSigXYZData);
  cmsSetRenderingIntent(hICC, INTENT_PERCEPTUAL);

  cmsAddTag(hICC, icSigDeviceMfgDescTag, PAnsiChar('(lcms internal)'));
  cmsAddTag(hICC, icSigProfileDescriptionTag, PAnsiChar('lcms RGB virtual profile'));
  cmsAddTag(hICC, icSigDeviceModelDescTag, PAnsiChar('rgb built-in'));

  if (WhitePoint <> nil) then
  begin

    cmsxyY2XYZ(@tmp, WhitePoint);
    cmsAddTag(hICC, icSigMediaWhitePointTag, @tmp);
  end;

  if (WhitePoint <> nil) and (Primaries <> nil) then
  begin

    MaxWhite.x := WhitePoint^.x;
    MaxWhite.y_mi := WhitePoint^.y_mi;
    MaxWhite.Y_ma := 1.0;

    if (cmsBuildRGB2XYZtransferMatrix(@MColorants, @MaxWhite, Primaries) = false) then
    begin
      IEcmsCloseProfile(hICC);
      result := nil;
      exit;
    end;

    cmsAdaptMatrixToD50(@MColorants, @MaxWhite);

    Colorants.Red.X := MColorants.v[0].n[0];
    Colorants.Red.Y := MColorants.v[1].n[0];
    Colorants.Red.Z := MColorants.v[2].n[0];

    Colorants.Green.X := MColorants.v[0].n[1];
    Colorants.Green.Y := MColorants.v[1].n[1];
    Colorants.Green.Z := MColorants.v[2].n[1];

    Colorants.Blue.X := MColorants.v[0].n[2];
    Colorants.Blue.Y := MColorants.v[1].n[2];
    Colorants.Blue.Z := MColorants.v[2].n[2];

    cmsAddTag(hICC, icSigRedColorantTag, @Colorants.Red);
    cmsAddTag(hICC, icSigBlueColorantTag, @Colorants.Blue);
    cmsAddTag(hICC, icSigGreenColorantTag, @Colorants.Green);
  end;

  if (TransferFunction <> nil) then
  begin

    cmsAddTag(hICC, icSigRedTRCTag, TransferFunction[0]);
    cmsAddTag(hICC, icSigGreenTRCTag, TransferFunction[1]);
    cmsAddTag(hICC, icSigBlueTRCTag, TransferFunction[2]);
  end;

  if (Primaries <> nil) then
  begin
    cmsAddTag(hICC, icTagSignature(icSigChromaticityTag), Primaries);
  end;

  result := hICC;
end;

procedure cmsXYZ2xyY(Dest: LPcmsCIExyY; Source: LPcmsCIEXYZ);
var
  ISum: double;
begin
  ISum := 1 / (Source^.X + Source^.Y + Source^.Z);

  Dest^.x := (Source^.X) * ISum;
  Dest^.y_mi := (Source^.Y) * ISum;
  Dest^.Y_ma := Source^.Y;
end;

const
  D50xyY: cmsCIExyY = (x: 0; y_mi: 0; Y_ma: 0);

function cmsD50_xyY: LPcmsCIExyY;
begin

  cmsXYZ2xyY(@D50xyY, cmsD50_XYZ);

  result := @D50xyY;
end;

function Create3x3EmptyLUT: LPLUT;
var
  AToB0: LPLUT;
begin
  AToB0 := cmsAllocLUT;
  AToB0^.OutputChan := 3;
  AToB0^.InputChan := AToB0^.OutputChan;

  result := AToB0;
end;

function cmsCreateLabProfile(WhitePoint: LPcmsCIExyY): cmsHPROFILE;
var
  hProfile: cmsHPROFILE;
  Lut: LPLUT;
begin

  if WhitePoint = nil then
    hProfile := cmsCreateRGBProfile(cmsD50_xyY, nil, nil)
  else
    hProfile := cmsCreateRGBProfile(WhitePoint, nil, nil);

  cmsSetDeviceClass(hProfile, icSigAbstractClass);
  cmsSetColorSpace(hProfile, icSigLabData);
  cmsSetPCS(hProfile, icSigLabData);

  cmsAddTag(hProfile, icSigDeviceMfgDescTag, PAnsiChar('(lcms internal)'));
  cmsAddTag(hProfile, icSigProfileDescriptionTag, PAnsiChar('lcms Lab identity'));
  cmsAddTag(hProfile, icSigDeviceModelDescTag, PAnsiChar('Lab built-in'));

  Lut := Create3x3EmptyLUT;
  if (Lut = nil) then
  begin
    result := nil;
    exit;
  end;

  cmsAddTag(hProfile, icSigAToB0Tag, Lut);
  cmsAddTag(hProfile, icSigBToA0Tag, Lut);

  cmsFreeLUT(Lut);

  result := hProfile;
end;

function IEcmsCreateLabProfile(WhitePoint_x, WhitePoint_y, WhitePoint_Y_: double): cmsHPROFILE;
var
  xyY: cmsCIExyY;
begin
  xyY.x := WhitePoint_x;
  xyY.y_mi := WhitePoint_y;
  xyY.Y_ma := WhitePoint_Y_;
  result := cmsCreateLabProfile(@xyY);
end;

function IEcmsCreateLabProfileD50: cmsHPROFILE;
begin
  result := cmsCreateLabProfile(nil);
end;

(*
function COLORSPACE_SH(s: dword): dword;
begin
  result := ((s) shl 16);
end;
*)

(*
function TYPE_Lab_16: dword;
begin
  result := (COLORSPACE_SH(PT_Lab) or CHANNELS_SH(3) or BYTES_SH(2))
end;
*)

function ipow(base: integer; exp: integer): integer;
var
  res: integer;
begin

  res := base;
  dec(exp);
  while (exp <> 0) do
  begin
    res := res * base;
    dec(exp);
  end;

  result := res;
end;

function ComponentOf(n: integer; clut: integer; nColorant: integer): integer;
begin
  if (nColorant <= 0) then
  begin
    result := (n mod clut);
    exit;
  end;

  n := trunc(n / ipow(clut, nColorant) );

  result := (n mod clut);
end;

function cmsReverseLinearInterpLUT16(Value: WORD; LutTable: pwordarray; p: LPL16PARAMS): word;
var
  l: integer;
  r: integer;
  x, res: integer;
  NumZeroes, NumPoles: integer;
  cell0, cell1: integer;
  val2: double;
  y0, y1, x0, x1: double;
  a, b: double;
  ia, ib: integer;
begin
  l := 1;
  r := $10000;
  x := 0;

  NumZeroes := 0;
  while (LutTable[NumZeroes] = 0) and (NumZeroes < p^.Domain) do
    inc(NumZeroes);

  NumPoles := 0;
  while (LutTable[p^.Domain - NumPoles] = $FFFF) and (NumPoles < p^.Domain) do
    inc(NumPoles);

  if (NumZeroes > 1) or (NumPoles > 1) then
  begin

    if (Value = 0) then
    begin
      result := 0;
      exit;
    end;
    if (Value = $FFFF) then
    begin
      result := $FFFF;
      exit;
    end;

    ia := trunc(((NumZeroes - 1) * $FFFF) / p^.Domain );
    ib := trunc(((p^.Domain - NumPoles) * $FFFF) / p^.Domain);

    l := ia - 1;
    r := ib + 1;
  end;

  while (r > l) do
  begin

    x := trunc((l + r) / 2 );

    res := cmsLinearInterpLUT16((x - 1), LutTable, p);

    if (res = Value) then
    begin

      result := (x - 1);
      exit;
    end;

    if (res > Value) then
      r := x - 1
    else
      l := x + 1;
  end;

  val2 := p^.Domain * ((x - 1) / 65535);

  cell0 := iefloor(val2);
  cell1 := ieceil(val2);

  if (cell0 = cell1) then
  begin
    result := x;
    exit;
  end;

  y0 := LutTable[cell0];
  x0 := (65535 * cell0) / p^.Domain;

  y1 := LutTable[cell1];
  x1 := (65535 * cell1) / p^.Domain;

  a := (y1 - y0) / (x1 - x0);
  b := y0 - a * x0;

  if (a = 0) then
  begin
    result := x;
    exit;
  end;

  result := iefloor(((Value - b) / a) + 0.5);

end;

function cmsSample3DGrid(Lut: LPLUT; Sampler: _cmsSAMPLER; Cargo: pointer; dwFlags: DWORD): longbool;
var
  i, tt, nTotalPoints, Colorant, index: integer;
  xIn, xOut: array[0..MAXCHANNELS - 1] of WORD;
begin

  with Lut^ do
  begin
    nTotalPoints := ipow(cLutPoints, InputChan);

    index := 0;
    for i := 0 to nTotalPoints - 1 do
    begin

      for tt := 0 to InputChan - 1 do
      begin

        Colorant := ComponentOf(i, cLutPoints, (InputChan - tt - 1));

        //xIn[tt] := _cmsQuantizeVal(Colorant, Lut^.cLutPoints);
        xIn[tt] := iefloor(((Colorant * 65535) / (cLutPoints - 1)) + 0.5)
      end;

      if (dwFlags and SAMPLER_HASTL1) <> 0 then
      begin

        for tt := 0 to InputChan - 1 do
          xIn[tt] := cmsReverseLinearInterpLUT16(xIn[tt], pwordarray(L1[tt]), @In16params);
      end;

      if (dwFlags and SAMPLER_INSPECT) <> 0 then
      begin

        for tt := 0 to OutputChan - 1 do
          xOut[tt] := pwordarray(T)[index + tt];
      end;

      if (Sampler(@xIn, @xOut, Cargo)) = 0 then
      begin
        result := FALSE;
        exit;
      end;

      if (dwFlags and SAMPLER_INSPECT) = 0 then
      begin

        if (dwFlags and SAMPLER_HASTL2) <> 0 then
        begin

          for tt := 0 to OutputChan - 1 do
            xOut[tt] := cmsReverseLinearInterpLUT16(xOut[tt], 
              pwordarray(L2[tt]), 
              @Out16params);
        end;

        for tt := 0 to OutputChan - 1 do
          pwordarray(T)[index + tt] := xOut[tt];

      end;

      inc(index, OutputChan);

    end;

    result := TRUE;
  end;
  (*
  nTotalPoints := ipow(Lut^.cLutPoints, Lut^.InputChan);

  index := 0;
  for i := 0 to nTotalPoints - 1 do
  begin

    for t := 0 to Lut^.InputChan - 1 do
    begin

      Colorant := ComponentOf(i, Lut^.cLutPoints, (Lut^.InputChan - t - 1));

      //xIn[t] := _cmsQuantizeVal(Colorant, Lut^.cLutPoints);
      xIn[t] := iefloor(((Colorant * 65535) / (Lut^.cLutPoints - 1)) + 0.5)
    end;

    if (dwFlags and SAMPLER_HASTL1) <> 0 then
    begin

      for t := 0 to Lut^.InputChan - 1 do
        xIn[t] := cmsReverseLinearInterpLUT16(xIn[t], pwordarray(Lut^.L1[t]), @Lut^.In16params);
    end;

    if (dwFlags and SAMPLER_INSPECT) <> 0 then
    begin

      for t := 0 to Lut^.OutputChan - 1 do
        xOut[t] := pwordarray(Lut^.T)[index + t];
    end;

    if (Sampler(@xIn, @xOut, Cargo)) = 0 then
    begin
      result := FALSE;
      exit;
    end;

    if (dwFlags and SAMPLER_INSPECT) = 0 then
    begin

      if (dwFlags and SAMPLER_HASTL2) <> 0 then
      begin

        for t := 0 to Lut^.OutputChan - 1 do
          xOut[t] := cmsReverseLinearInterpLUT16(xOut[t], 
            pwordarray(Lut^.L2[t]), 
            @Lut^.Out16params);
      end;

      for t := 0 to Lut^.OutputChan - 1 do
        pwordarray(Lut^.T)[index + t] := xOut[t];

    end;

    inc(index, Lut^.OutputChan);

  end;

  result := TRUE;
  *)
end;

procedure IEcmsDoTransform(Transform: cmsHTRANSFORM; InputBuffer: pointer; OutputBuffer: pointer; Size: dword);
var
  p: _LPcmsTRANSFORM;
begin
  p := _LPcmsTRANSFORM(Transform);

  p^.StrideOut := Size;
  p^.StrideIn := p^.StrideOut;

  p^.xform(p, InputBuffer, OutputBuffer, Size);

end;

function SoftProofSampler(xIn: pwordarray; xOut: pwordarray; Cargo: pointer): integer;
var
  t: LPGAMUTCHAIN;
  Colorant: array[0..MAXCHANNELS - 1] of WORD;
begin
  t := LPGAMUTCHAIN(Cargo);

  IEcmsDoTransform(t^.hForward, xIn, @Colorant, 1);

  IEcmsDoTransform(t^.hReverse, @Colorant, xOut, 1);

  result := 1;
end;

function _cmsComputeSoftProofLUT(hProfile: cmsHPROFILE; nIntent: integer): LPLUT;
var
  hLab: cmsHPROFILE;
  SoftProof: LPLUT;
  dwFormat: DWORD;
  Chain: GAMUTCHAIN;
  //nErrState: integer ;
begin

  ZeroMemory(@Chain, sizeof(GAMUTCHAIN));

  hLab := cmsCreateLabProfile(nil);

  dwFormat := ((4 shl 3) or 2);

  //nErrState := cmsErrorAction(LCMS_ERROR_IGNORE);

  Chain.hForward := IEcmsCreateTransform(hLab, TYPE_Lab_16,
    hProfile, dwFormat,
    nIntent,
    cmsFLAGS_NOTPRECALC);

  Chain.hReverse := IEcmsCreateTransform(hProfile, dwFormat,
    hLab, TYPE_Lab_16,
    INTENT_RELATIVE_COLORIMETRIC,
    cmsFLAGS_NOTPRECALC);

  //cmsErrorAction(nErrState);

  if (Chain.hForward <> nil) and (Chain.hReverse <> nil) then
  begin

    SoftProof := cmsAllocLUT;
    SoftProof := cmsAlloc3DGrid(SoftProof, 33, 3, 3);

    cmsSample3DGrid(SoftProof, SoftProofSampler, @Chain, 0);
  end
  else
    SoftProof := nil;

  if (Chain.hForward <> nil) then
    IEcmsDeleteTransform(Chain.hForward);
  if (Chain.hReverse <> nil) then
    IEcmsDeleteTransform(Chain.hReverse);

  IEcmsCloseProfile(hLab);

  result := SoftProof;
end;

function cmsDeltaE(Lab1: LPcmsCIELab; Lab2: LPcmsCIELab): double;
var
  dL, da, db: double;
begin

  if (Lab1^.L < 0) or (Lab2^.L < 0) then
  begin
    result := 65536;
    exit;
  end;

  if (Lab1^.a < -200) or (Lab1^.a > 200) then
  begin
    result := 65536;
    exit;
  end;
  if (Lab1^.b < -200) or (Lab1^.b > 200) then
  begin
    result := 65536.;
    exit;
  end;

  if (Lab2^.a < -200) or (Lab2^.a > 200) then
  begin
    result := 65536;
    exit;
  end;
  if (Lab2^.b < -200) or (Lab2^.b > 200) then
  begin
    result := 65536;
    exit;
  end;

  if (Lab1^.L = 0) and (Lab2^.L = 0) then
  begin
    result := 0;
    exit;
  end;

  dL := abs(Lab1^.L - Lab2^.L);
  da := abs(Lab1^.a - Lab2^.a);
  db := abs(Lab1^.b - Lab2^.b);

  result := Power(dL * dL + da * da + db * db, 0.5);

end;

function GamutSampler(xIn: pwordarray; xOut: pwordarray; Cargo: pointer): integer;
var
  t: LPGAMUTCHAIN;
  Proof, Check: array[0..MAXCHANNELS - 1] of WORD;
  Proof2, Check2: array[0..MAXCHANNELS - 1] of WORD;
  LabIn1, LabOut1: cmsCIELab;
  LabIn2, LabOut2: cmsCIELab;
  dE1, dE2, ErrorRatio: double;
begin
  t := LPGAMUTCHAIN(Cargo);

  //ErrorRatio := 1.0;

  IEcmsDoTransform(t^.hForward, xIn, @Proof, 1);

  IEcmsDoTransform(t^.hReverse, @Proof, @Check, 1);

  IEcmsDoTransform(t^.hForward, @Check, @Proof2, 1);
  IEcmsDoTransform(t^.hReverse, @Proof2, @Check2, 1);

  if (Check[0] = $FFFF) and (Check[1] = $FFFF) and (Check[2] = $FFFF) then

    xOut[0] := $F000
  else
  begin

    cmsLabEncoded2Float(@LabIn1, xIn);
    cmsLabEncoded2Float(@LabOut1, @Check);

    dE1 := cmsDeltaE(@LabIn1, @LabOut1);

    cmsLabEncoded2Float(@LabIn2, @Check);
    cmsLabEncoded2Float(@LabOut2, @Check2);

    dE2 := cmsDeltaE(@LabIn2, @LabOut2);

    if (dE1 < ERR_THERESHOLD) and (dE2 < ERR_THERESHOLD) then
      xOut[0] := 0
    else
    begin
      if (dE1 < ERR_THERESHOLD) and (dE2 > ERR_THERESHOLD) then
        xOut[0] := 0
      else
      begin
        if (dE1 > ERR_THERESHOLD) and (dE2 < ERR_THERESHOLD) then
          xOut[0] := iefloor((dE1 - ERR_THERESHOLD) + 0.5)
        else
        begin

          if (dE2 = 0.0) then
            ErrorRatio := dE1
          else
            ErrorRatio := dE1 / dE2;

          if (ErrorRatio > ERR_THERESHOLD) then
            xOut[0] := iefloor((ErrorRatio - ERR_THERESHOLD) + 0.5)
          else
            xOut[0] := 0;
        end;
      end;
    end;

  end;

  result := 1;
end;

function _cmsComputeGamutLUT(hProfile: cmsHPROFILE; Intent: integer): LPLUT;
var
  hLab: cmsHPROFILE;
  Gamut: LPLUT;
  dwFormat: DWORD;
  Chain: GAMUTCHAIN;
  //nErrState: integer ;
begin

  ZeroMemory(@Chain, sizeof(GAMUTCHAIN));

  hLab := cmsCreateLabProfile(nil);

  dwFormat := ((4 shl 3) or 2);

  //nErrState := cmsErrorAction(LCMS_ERROR_IGNORE);

  Chain.hForward := IEcmsCreateTransform(hLab, TYPE_Lab_16,
    hProfile, dwFormat,
    Intent,
    cmsFLAGS_NOTPRECALC);

  Chain.hReverse := IEcmsCreateTransform(hProfile, dwFormat,
    hLab, TYPE_Lab_16,
    Intent,
    cmsFLAGS_NOTPRECALC);

  //cmsErrorAction(nErrState);

  if (Chain.hForward <> nil) and (Chain.hReverse <> nil) then
  begin

    Gamut := cmsAllocLUT;
    Gamut := cmsAlloc3DGrid(Gamut, 42, 3, 1);

    cmsSample3DGrid(Gamut, GamutSampler, @Chain, 0);

  end
  else
    Gamut := nil;

  if (Chain.hForward <> nil) then
    IEcmsDeleteTransform(Chain.hForward);
  if (Chain.hReverse <> nil) then
    IEcmsDeleteTransform(Chain.hReverse);

  IEcmsCloseProfile(hLab);

  result := Gamut;
end;

procedure CreateProof(p: _LPcmsTRANSFORM; dwFlags: DWORD; ToTagPtr: PicTagSignature);
var
  ProofTag: icTagSignature;
begin

  if (dwFlags and cmsFLAGS_SOFTPROOFING) <> 0 then
  begin

    p^.Preview := _cmsComputeSoftProofLUT(p^.PreviewProfile, p^.Intent);
    p^.Phase2 := LabRel;

    ToTagPtr^ := PCS2Device[p^.ProofIntent];

    if (p^.Preview = nil) then
    begin

      ProofTag := Preview[p^.Intent];

      if (not cmsIsTag(p^.PreviewProfile, ProofTag)) then
      begin

        ProofTag := Preview[0];
        if (not cmsIsTag(p^.PreviewProfile, ProofTag)) then
          ProofTag := icTagSignature(0);
      end;

      if integer(ProofTag) <> 0 then
      begin

        p^.Preview := cmsReadICCLut(p^.PreviewProfile, ProofTag);
        p^.Phase2 := GetPhase(p^.PreviewProfile);

      end
      else
      begin

        p^.Preview := nil;
        p^.PreviewProfile := nil;
        //cmsSignalError(LCMS_ERRC_WARNING, "Sorry, the proof profile has not previewing capabilities");
      end;
    end;

  end;

  if (dwFlags and cmsFLAGS_GAMUTCHECK) <> 0 then
  begin

    p^.Gamut := _cmsComputeGamutLUT(p^.PreviewProfile, p^.Intent);
    if (p^.Gamut = nil) then
    begin

      if (cmsIsTag(p^.PreviewProfile, icSigGamutTag)) then
      begin

        p^.Gamut := cmsReadICCLut(p^.PreviewProfile, icSigGamutTag);
      end
      else
      begin

        //cmsSignalError(LCMS_ERRC_WARNING, "Sorry, the proof profile has not gamut checking capabilities");
        p^.Gamut := nil;
      end;
    end;

  end;

end;

procedure NC2toPCS(p: _LPcmsTRANSFORM; xIn: pwordarray; xOut: pwordarray);
var
  index: integer;
begin
  index := xIn[0];

  CopyMemory(xOut, @(p^.NamedColorList^.List[index].PCS), 3 * sizeof(WORD));
end;

function cmsLinearInterpFixed(Value1: WORD; LutTable: pwordarray; p: LPL16PARAMS): Fixed32;
var
  y1, y0: Fixed32;
  cell0: integer;
  val3, Value: integer;
begin

  Value := Value1;
  if (Value = $FFFF) then
  begin
    result := LutTable[p^.Domain];
    exit;
  end;

  val3 := p^.Domain * Value;
  val3 := ToFixedDomain(val3);

  cell0 := FIXED_TO_INT(val3);

  y0 := LutTable[cell0];
  y1 := LutTable[cell0 + 1];

  result := y0 + FixedMul((y1 - y0), (val3 and $FFFF));
end;

function Clamp_XYZ(xin: integer): word; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  if (xin < 0) then
  begin
    result := 0;
    exit;
  end;
  if (xin > $FFFF) then
  begin
    result := $FFFF;
    exit;
  end;
  result := xin;
end;

procedure AllSmeltedBehaviour(MatShaper: LPMATSHAPER; xIn: pwordarray; xOut: pwordarray);
var
  tmp: array[0..2] of WORD;
  InVect, OutVect: WVEC3;
begin

  if (MatShaper^.dwFlags and MATSHAPER_HASINPSHAPER) <> 0 then
  begin
    InVect.n[VX] := cmsLinearInterpFixed(xIn[0], pwordarray(MatShaper^.L2[0]), @MatShaper^.p2_16);
    InVect.n[VY] := cmsLinearInterpFixed(xIn[1], pwordarray(MatShaper^.L2[1]), @MatShaper^.p2_16);
    InVect.n[VZ] := cmsLinearInterpFixed(xIn[2], pwordarray(MatShaper^.L2[2]), @MatShaper^.p2_16);
  end
  else
  begin
    InVect.n[VX] := ToFixedDomain(xIn[0]);
    InVect.n[VY] := ToFixedDomain(xIn[1]);
    InVect.n[VZ] := ToFixedDomain(xIn[2]);
  end;

  if (MatShaper^.dwFlags and MATSHAPER_HASMATRIX) <> 0 then
  begin

    MAT3evalW(@OutVect, @MatShaper^.Matrix, @InVect);
  end
  else
  begin

    OutVect.n[VX] := InVect.n[VX];
    OutVect.n[VY] := InVect.n[VY];
    OutVect.n[VZ] := InVect.n[VZ];
  end;

  tmp[0] := Clamp_XYZ(FromFixedDomain(OutVect.n[VX]));
  tmp[1] := Clamp_XYZ(FromFixedDomain(OutVect.n[VY]));
  tmp[2] := Clamp_XYZ(FromFixedDomain(OutVect.n[VZ]));

  if (MatShaper^.dwFlags and MATSHAPER_HASSHAPER) <> 0 then
  begin
    xOut[0] := cmsLinearInterpLUT16(tmp[0], pwordarray(MatShaper^.L[0]), @MatShaper^.p16);
    xOut[1] := cmsLinearInterpLUT16(tmp[1], pwordarray(MatShaper^.L[1]), @MatShaper^.p16);
    xOut[2] := cmsLinearInterpLUT16(tmp[2], pwordarray(MatShaper^.L[2]), @MatShaper^.p16);
  end
  else
  begin
    xOut[0] := tmp[0];
    xOut[1] := tmp[1];
    xOut[2] := tmp[2];
  end;

end;

procedure InputBehaviour(MatShaper: LPMATSHAPER; xIn: pwordarray; xOut: pwordarray);
var
  InVect, OutVect: WVEC3;
begin

  if (MatShaper^.dwFlags and MATSHAPER_HASSHAPER) <> 0 then
  begin
    InVect.n[VX] := cmsLinearInterpFixed(xIn[0], pwordarray(MatShaper^.L[0]), @MatShaper^.p16);
    InVect.n[VY] := cmsLinearInterpFixed(xIn[1], pwordarray(MatShaper^.L[1]), @MatShaper^.p16);
    InVect.n[VZ] := cmsLinearInterpFixed(xIn[2], pwordarray(MatShaper^.L[2]), @MatShaper^.p16);
  end
  else
  begin
    InVect.n[VX] := ToFixedDomain(xIn[0]);
    InVect.n[VY] := ToFixedDomain(xIn[1]);
    InVect.n[VZ] := ToFixedDomain(xIn[2]);
  end;

  if (MatShaper^.dwFlags and MATSHAPER_HASMATRIX) <> 0 then
  begin
    MAT3evalW(@OutVect, @MatShaper^.Matrix, @InVect);
  end
  else
  begin
    OutVect := InVect;
  end;

  xOut[0] := Clamp_XYZ((OutVect.n[VX]) shr 1);
  xOut[1] := Clamp_XYZ((OutVect.n[VY]) shr 1);
  xOut[2] := Clamp_XYZ((OutVect.n[VZ]) shr 1);

end;

procedure OutputBehaviour(MatShaper: LPMATSHAPER; xIn: pwordarray; xOut: pwordarray);
var
  InVect, OutVect: WVEC3;
  i: integer;
begin

  InVect.n[VX] := Fixed32(xIn[0] shl 1);
  InVect.n[VY] := Fixed32(xIn[1] shl 1);
  InVect.n[VZ] := Fixed32(xIn[2] shl 1);

  if (MatShaper^.dwFlags and MATSHAPER_HASMATRIX) <> 0 then
  begin

    MAT3evalW(@OutVect, @MatShaper^.Matrix, @InVect);
  end
  else
  begin
    OutVect := InVect;
  end;

  if (MatShaper^.dwFlags and MATSHAPER_HASSHAPER) <> 0 then
  begin

    for i := 0 to 2 do
    begin

      xOut[i] := cmsLinearInterpLUT16(
        Clamp_RGB(FromFixedDomain(OutVect.n[i])),
        pwordarray(MatShaper^.L[i]),
        @MatShaper^.p16);
    end;
  end
  else
  begin

    xOut[0] := Clamp_RGB(FromFixedDomain(OutVect.n[VX]));
    xOut[1] := Clamp_RGB(FromFixedDomain(OutVect.n[VY]));
    xOut[2] := Clamp_RGB(FromFixedDomain(OutVect.n[VZ]));
  end;

end;

procedure cmsEvalMatShaper(MatShaper: LPMATSHAPER; xIn: pwordarray; xOut: pwordarray);
begin

  if ((MatShaper^.dwFlags and MATSHAPER_ALLSMELTED) = MATSHAPER_ALLSMELTED) then
  begin
    AllSmeltedBehaviour(MatShaper, xIn, xOut);
    exit;
  end;
  if (MatShaper^.dwFlags and MATSHAPER_INPUT) <> 0 then
  begin
    InputBehaviour(MatShaper, xIn, xOut);
    exit;
  end;

  OutputBehaviour(MatShaper, xIn, xOut);
end;

procedure MatrixShaperXFORM(p: _LPcmsTRANSFORM; xin: pointer; xout: pointer; Size: dword);
var
  accum: PBYTE;
  output: PBYTE;
  wIn, wOut: array[0..MAXCHANNELS - 1] of WORD;
  i, n: dword;
begin
  accum := xin;
  output := xout;
  n := Size;

  for i := 0 to n - 1 do
  begin
    accum := p^.FromInput(p, @wIn, accum);
    cmsEvalMatShaper(p^.SmeltMatShaper, @wIn, @wOut);
    output := p^.ToOutput(p, @wOut, output);
  end;
end;

function cmsTakeColorants(Dest: LPcmsCIEXYZTRIPLE; hProfile: cmsHPROFILE): longbool;
begin
  if (ReadICCXYZ(hProfile, icSigRedColorantTag, @Dest^.Red, TRUE) < 0) then
  begin
    result := FALSE;
    exit;
  end;
  if (ReadICCXYZ(hProfile, icSigGreenColorantTag, @Dest^.Green, TRUE) < 0) then
  begin
    result := FALSE;
    exit;
  end;
  if (ReadICCXYZ(hProfile, icSigBlueColorantTag, @Dest^.Blue, TRUE) < 0) then
  begin
    result := FALSE;
    exit;
  end;

  result := TRUE;
end;

function cmsReadICCMatrixRGB2XYZ(r: LPMAT3; hProfile: cmsHPROFILE): longbool;
var
  Primaries: cmsCIEXYZTRIPLE;
begin

  if (not cmsTakeColorants(@Primaries, hProfile)) then
  begin
    result := FALSE;
    exit;
  end;

  VEC3init(@r^.v[0], Primaries.Red.X, Primaries.Green.X, Primaries.Blue.X);
  VEC3init(@r^.v[1], Primaries.Red.Y, Primaries.Green.Y, Primaries.Blue.Y);
  VEC3init(@r^.v[2], Primaries.Red.Z, Primaries.Green.Z, Primaries.Blue.Z);

  result := TRUE;

end;

function cmsDupGamma(xIn: LPGAMMATABLE): LPGAMMATABLE;
var
  Ptr: LPGAMMATABLE;
begin

  Ptr := cmsAllocGamma(xIn^.nEntries);
  if (Ptr = nil) then
  begin
    result := nil;
    exit;
  end;

  CopyMemory(@Ptr^.GammaTable,
    @xIn^.GammaTable,
    xIn^.nEntries * sizeof(WORD));

  result := Ptr;
end;

function cmsReadICCGamma(hProfile: cmsHPROFILE; sig: icTagSignature): LPGAMMATABLE;
var
  Icc: LPLCMSICCPROFILE;
  offset: integer;
  n: integer;
begin

  Icc := LPLCMSICCPROFILE(hProfile);

  n := SearchTag(Icc, sig);
  if (n < 0) then
  begin

    //cmsSignalError(LCMS_ERRC_ABORTED, "Tag not found");
    result := nil;
    exit;
  end;

  if (Icc^.stream = nil) then
  begin

    result := cmsDupGamma(LPGAMMATABLE(Icc^.TagPtrs[n]));
    exit;
  end;

  offset := Icc^.TagOffsets[n];

  if (Icc^.Seek(Icc^.stream, offset)) then
  begin
    result := nil;
    exit;
  end;

  result := ReadCurve(Icc);

end;

function cmsReverseGamma(nResultSamples: integer; InGamma: LPGAMMATABLE): LPGAMMATABLE;
var
  i: integer;
  L16In: L16PARAMS;
  InPtr: PWORD;
  p: LPGAMMATABLE;
  wValIn, wValOut: WORD;
begin

  p := cmsAllocGamma(nResultSamples);
  if (p = nil) then
  begin
    result := nil;
    exit;
  end;

  cmsCalcL16Params(InGamma^.nEntries, @L16In);
  InPtr := @InGamma^.GammaTable;

  for i := 0 to nResultSamples - 1 do
  begin

    //wValIn := _cmsQuantizeVal(i, nResultSamples);
    wValIn := iefloor(((i * 65535) / (nResultSamples - 1)) + 0.5);

    wValOut := cmsReverseLinearInterpLUT16(wValIn, pwordarray(InPtr), @L16In);
    p^.GammaTable[i] := wValOut;
  end;

  result := p;
end;

function ReadCurveReversed(Icc: LPLCMSICCPROFILE): LPGAMMATABLE;
const
  ParamsByType: array[0..4] of integer = (1, 3, 4, 5, 7);
var
  Base: icTagBase;
  NewGamma, ReturnGamma: LPGAMMATABLE;
  Count: icUInt32Number;
  n: integer;
  SingleGammaFixed: WORD;
  Params: array[0..9] of double;
  Num: icS15Fixed16Number;
  Reserved: icUInt32Number;
  xType: icUInt16Number;
  i: integer;
begin

  Icc^.Read(@Base, 1, sizeof(icTagBase), Icc^.stream);
  AdjustEndianess32(@Base.sig);

  case (Base.sig) of

    icTagTypeSignature($9478EE00),
      icSigCurveType:
      begin

        Icc^.Read(@Count, sizeof(icUInt32Number), 1, Icc^.stream);
        AdjustEndianess32(@Count);

        case (Count) of

          0:
            begin
              NewGamma := cmsAllocGamma(2);
              if (NewGamma = nil) then
              begin
                result := nil;
                exit;
              end;
              NewGamma^.GammaTable[0] := 0;
              pwordarray(@NewGamma^.GammaTable)[1] := $FFFF;
              result := NewGamma;
            end;

          1:
            begin
              Icc^.Read(@SingleGammaFixed, sizeof(WORD), 1, Icc^.stream);
              AdjustEndianess16(@SingleGammaFixed);
              result := cmsBuildGamma(4096, 1 / Convert8Fixed8(SingleGammaFixed));
            end;

        else
          begin

            NewGamma := cmsAllocGamma(Count);
            if (NewGamma = nil) then
            begin
              result := nil;
              exit;
            end;

            Icc^.Read(@NewGamma^.GammaTable, sizeof(WORD), Count, Icc^.stream);

            AdjustEndianessArray16(@NewGamma^.GammaTable, Count);

            ReturnGamma := cmsReverseGamma(Count, NewGamma);
            cmsFreeGamma(NewGamma);

            result := ReturnGamma;
          end;
        end;

      end;

    icTagTypeSignature(icSigParametricCurveType):
      begin

        Icc^.Read(@xType, sizeof(icUInt16Number), 1, Icc^.stream);
        Icc^.Read(@Reserved, sizeof(icUInt16Number), 1, Icc^.stream);

        AdjustEndianess16(@xType);
        if (xType > 5) then
        begin

          //cmsSignalError(LCMS_ERRC_ABORTED, "Unknown parametric curve type '%d' found.", Type);
          result := nil;
          exit;
        end;

        ZeroMemory(@Params, 10 * sizeof(double));
        n := ParamsByType[xType];

        for i := 0 to n - 1 do
        begin
          Icc^.Read(@Num, sizeof(icS15Fixed16Number), 1, Icc^.stream);
          Params[i] := Convert15Fixed16(Num);
        end;

        NewGamma := cmsBuildParametricGamma(4096, -(xType + 1), @Params);
        result := NewGamma;
      end;
  else
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "Bad tag signature '%lx' found.", Base.sig);
      result := nil;
      exit;
    end;
  end;

end;

function cmsReadICCGammaReversed(hProfile: cmsHPROFILE; sig: icTagSignature): LPGAMMATABLE;
var
  Icc: LPLCMSICCPROFILE;
  offset: integer;
  n: integer;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  n := SearchTag(Icc, sig);
  if (n < 0) then
  begin

    //cmsSignalError(LCMS_ERRC_ABORTED, "Tag not found");
    result := nil;
    exit;
  end;

  if (Icc^.stream = nil) then
  begin

    result := cmsReverseGamma(256, LPGAMMATABLE(Icc^.TagPtrs[n]));
    exit;
  end;

  offset := Icc^.TagOffsets[n];

  if (Icc^.Seek(Icc^.stream, offset)) then
  begin
    result := nil;
    exit;
  end;

  result := ReadCurveReversed(Icc);
end;

function ComputeTables(Table: LPGAMMATABLEarray; xOut: PPWORDARRAY; p16: LPL16PARAMS): integer;
var
  i, AllLinear: integer;
  PtrW: PWORD;
begin

  cmsCalcL16Params(Table[0]^.nEntries, p16);

  AllLinear := 0;
  for i := 0 to 2 do
  begin

    getmem(PtrW, sizeof(WORD) * p16^.nSamples);

    if (PtrW = nil) then
    begin
      result := -1;
      exit;
    end;

    CopyMemory(PtrW, @Table[i]^.GammaTable, sizeof(WORD) * Table[i]^.nEntries);

    xOut[i] := PtrW;

    inc(AllLinear, cmsIsLinear(pwordarray(PtrW), p16^.nSamples));
  end;

  if (AllLinear <> 3) then
    result := 1
  else
    result := 0;

end;

function cmsAllocMatShaper2(Matrix: LPMAT3; xIn: LPGAMMATABLEArray; xOut: LPGAMMATABLEArray; Behaviour: DWORD): LPMATSHAPER;
var
  NewMatShaper: LPMATSHAPER;
  rc: integer;
begin

  getmem(NewMatShaper, sizeof(MATSHAPER));
  if (NewMatShaper <> nil) then
    ZeroMemory(NewMatShaper, sizeof(MATSHAPER));

  NewMatShaper^.dwFlags := Behaviour and (MATSHAPER_ALLSMELTED);

  MAT3toFix(@NewMatShaper^.Matrix, Matrix);

  if (not MAT3isIdentity(@NewMatShaper^.Matrix, 0.00001)) then
    NewMatShaper^.dwFlags := NewMatShaper^.dwFlags or MATSHAPER_HASMATRIX;

  if (xOut <> nil) then
  begin

    rc := ComputeTables(xOut, @NewMatShaper^.L, @NewMatShaper^.p16);
    if (rc < 0) then
    begin
      cmsFreeMatShaper(NewMatShaper);
      result := nil;
      exit;
    end;
    if (rc = 1) then
      NewMatShaper^.dwFlags := NewMatShaper^.dwFlags or MATSHAPER_HASSHAPER;
  end;

  if (xIn <> nil) then
  begin

    rc := ComputeTables(xIn, @NewMatShaper^.L2, @NewMatShaper^.p2_16);
    if (rc < 0) then
    begin
      cmsFreeMatShaper(NewMatShaper);
      result := nil;
      exit;
    end;
    if (rc = 1) then
      NewMatShaper^.dwFlags := NewMatShaper^.dwFlags or MATSHAPER_HASINPSHAPER;
  end;

  result := NewMatShaper;

end;

procedure cmsFreeGammaTriple(Gamma: LPGAMMATABLEarray);
begin
  cmsFreeGamma(Gamma[0]);
  cmsFreeGamma(Gamma[1]);
  cmsFreeGamma(Gamma[2]);
  Gamma[2] := nil;
  Gamma[1] := Gamma[2];
  Gamma[0] := Gamma[1];
end;

function cmsBuildSmeltMatShaper(p: _LPcmsTRANSFORM): longbool;
var
  From, xTo, ToInv, Transfer: MAT3;
  xIn, InverseOut: array[0..2] of LPGAMMATABLE;
begin

  if (not cmsReadICCMatrixRGB2XYZ(@From, p^.InputProfile)) then
  begin
    result := FALSE;
    exit;
  end;

  if (not cmsReadICCMatrixRGB2XYZ(@xTo, p^.OutputProfile)) then
  begin
    result := FALSE;
    exit;
  end;

  if (MAT3inverse(@xTo, @ToInv) < 0) then
  begin
    result := FALSE;
    exit;
  end;

  MAT3per(@Transfer, @ToInv, @From);

  xIn[0] := cmsReadICCGamma(p^.InputProfile, icSigRedTRCTag);
  xIn[1] := cmsReadICCGamma(p^.InputProfile, icSigGreenTRCTag);
  xIn[2] := cmsReadICCGamma(p^.InputProfile, icSigBlueTRCTag);

  if (xIn[0] = nil) or (xIn[1] = nil) or (xIn[2] = nil) then
  begin
    result := FALSE;
    exit;
  end;

  InverseOut[0] := cmsReadICCGammaReversed(p^.OutputProfile, icSigRedTRCTag);
  InverseOut[1] := cmsReadICCGammaReversed(p^.OutputProfile, icSigGreenTRCTag);
  InverseOut[2] := cmsReadICCGammaReversed(p^.OutputProfile, icSigBlueTRCTag);

  p^.SmeltMatShaper := cmsAllocMatShaper2(@Transfer, @xIn, @InverseOut, MATSHAPER_ALLSMELTED);

  cmsFreeGammaTriple(@xIn);

  cmsFreeGammaTriple(@InverseOut);

  result := (p^.SmeltMatShaper <> nil);
end;

procedure LUTtoPCS(p: _LPcmsTRANSFORM; xIn: pwordarray; xOut: pwordarray);
begin
  cmsEvalLUT(p^.Device2PCS, xIn, xOut);
end;

procedure ShaperMatrixToPCS(p: _LPcmsTRANSFORM; xIn: pwordarray; xOut: pwordarray); {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  cmsEvalMatShaper(p^.InMatShaper, xIn, xOut);
end;

function cmsTakeIluminant(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  Dest^ := Icc^.Illuminant;

  result := true;
end;

function cmsAllocMatShaper(Matrix: LPMAT3; Tables: LPGAMMATABLEarray; Behaviour: DWORD): LPMATSHAPER;
var
  NewMatShaper: LPMATSHAPER;
  i, AllLinear: integer;
  PtrW: PWORD;

begin

  getmem(NewMatShaper, sizeof(MATSHAPER));
  if (NewMatShaper <> nil) then
    ZeroMemory(NewMatShaper, sizeof(MATSHAPER));

  NewMatShaper^.dwFlags := Behaviour and (MATSHAPER_ALLSMELTED);

  MAT3toFix(@NewMatShaper^.Matrix, Matrix);

  if (not MAT3isIdentity(@NewMatShaper^.Matrix, 0.00001)) then
    NewMatShaper^.dwFlags := NewMatShaper^.dwFlags or MATSHAPER_HASMATRIX;

  cmsCalcL16Params(Tables[0]^.nEntries, @NewMatShaper^.p16);

  AllLinear := 0;
  for i := 0 to 2 do
  begin

    getmem(PtrW, sizeof(WORD) * NewMatShaper^.p16.nSamples);

    if (PtrW = nil) then
    begin
      cmsFreeMatShaper(NewMatShaper);
      result := nil;
      exit;
    end;

    CopyMemory(PtrW, @Tables[i]^.GammaTable,
      sizeof(WORD) * Tables[i]^.nEntries);

    NewMatShaper^.L[i] := PtrW;

    inc(AllLinear, cmsIsLinear(pwordarray(PtrW), NewMatShaper^.p16.nSamples));
  end;

  if (AllLinear <> 3) then
    NewMatShaper^.dwFlags := NewMatShaper^.dwFlags or MATSHAPER_HASSHAPER;

  result := NewMatShaper;
end;

function cmsBuildGrayInputMatrixShaper(hProfile: cmsHPROFILE): LPMATSHAPER;
var
  Illuminant: cmsCIEXYZ;
  GrayTRC: LPGAMMATABLE;
  Shapes: array[0..2] of LPGAMMATABLE;
  MatShaper: LPMATSHAPER;
  Scale: MAT3;
begin

  GrayTRC := cmsReadICCGamma(hProfile, icSigGrayTRCTag);
  cmsTakeIluminant(@Illuminant, hProfile);

  Shapes[0] := cmsDupGamma(GrayTRC);
  Shapes[1] := cmsDupGamma(GrayTRC);
  Shapes[2] := cmsDupGamma(GrayTRC);

  if (Shapes[0] = nil) or (Shapes[1] = nil) or (Shapes[2] = nil) then
  begin
    result := nil;
    exit;
  end;

  cmsFreeGamma(GrayTRC);

  VEC3init(@Scale.v[0], Illuminant.X / 3, Illuminant.X / 3, Illuminant.X / 3);
  VEC3init(@Scale.v[1], Illuminant.Y / 3, Illuminant.Y / 3, Illuminant.Y / 3);
  VEC3init(@Scale.v[2], Illuminant.Z / 3, Illuminant.Z / 3, Illuminant.Z / 3);

  MatShaper := cmsAllocMatShaper(@Scale, @Shapes, MATSHAPER_INPUT);
  cmsFreeGammaTriple(@Shapes);
  result := MatShaper;

end;

function cmsBuildInputMatrixShaper(InputProfile: cmsHPROFILE; dwFlags: PDWORD): LPMATSHAPER;
var
  DoubleMat: MAT3;
  Shapes: array[0..2] of LPGAMMATABLE;
  InMatSh: LPMATSHAPER;
begin

  if (cmsGetColorSpace(InputProfile) = icSigGrayData) then
  begin
    if (dwFlags) <> nil then
      dwFlags^ := dwFlags^ or cmsFLAGS_NOTPRECALC;
    result := cmsBuildGrayInputMatrixShaper(InputProfile);
    exit;
  end;

  if (not cmsReadICCMatrixRGB2XYZ(@DoubleMat, InputProfile)) then
  begin
    result := nil;
    exit;
  end;

  Shapes[0] := cmsReadICCGamma(InputProfile, icSigRedTRCTag);
  Shapes[1] := cmsReadICCGamma(InputProfile, icSigGreenTRCTag);
  Shapes[2] := cmsReadICCGamma(InputProfile, icSigBlueTRCTag);

  if (Shapes[0] = nil) or (Shapes[1] = nil) or (Shapes[2] = nil) then
  begin
    result := nil;
    exit;
  end;

  InMatSh := cmsAllocMatShaper(@DoubleMat, @Shapes, MATSHAPER_INPUT);

  cmsFreeGammaTriple(@Shapes);

  result := InMatSh;
end;

procedure PCStoLUT(p: _LPcmsTRANSFORM; xIn: pwordarray; xOut: pwordarray);
begin
  cmsEvalLUT(p^.PCS2Device, xIn, xOut);
end;

procedure PCStoShaperMatrix(p: _LPcmsTRANSFORM; xIn: pwordarray; xOut: pwordarray); {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  cmsEvalMatShaper(p^.OutMatShaper, xIn, xOut);
end;

function cmsBuildGrayOutputMatrixShaper(hProfile: cmsHPROFILE): LPMATSHAPER;
var
  Illuminant: cmsCIEXYZ;
  GrayTRC: LPGAMMATABLE;
  Shapes: array[0..2] of LPGAMMATABLE;
  MatShaper: LPMATSHAPER;
  Scale: MAT3;

begin

  GrayTRC := cmsReadICCGammaReversed(hProfile, icSigGrayTRCTag);
  cmsTakeIluminant(@Illuminant, hProfile);

  Shapes[0] := cmsDupGamma(GrayTRC);
  Shapes[1] := cmsDupGamma(GrayTRC);
  Shapes[2] := cmsDupGamma(GrayTRC);

  if (Shapes[0] = nil) or (Shapes[1] = nil) or (Shapes[2] = nil) then
  begin
    result := nil;
    exit;
  end;

  cmsFreeGamma(GrayTRC);

  VEC3init(@Scale.v[0], 0, 1.0 / Illuminant.Y, 0);
  VEC3init(@Scale.v[1], 0, 1.0 / Illuminant.Y, 0);
  VEC3init(@Scale.v[2], 0, 1.0 / Illuminant.Y, 0);

  MatShaper := cmsAllocMatShaper(@Scale, @Shapes, MATSHAPER_OUTPUT);
  cmsFreeGammaTriple(@Shapes);
  result := MatShaper;

end;

function cmsBuildOutputMatrixShaper(OutputProfile: cmsHPROFILE; dwFlags: PDWORD): LPMATSHAPER;
var
  DoubleMat, DoubleInv: MAT3;
  InverseShapes: array[0..2] of LPGAMMATABLE;
  OutMatSh: LPMATSHAPER;

begin

  if (cmsGetColorSpace(OutputProfile) = icSigGrayData) then
  begin
    if (dwFlags <> nil) then
      dwFlags^ := dwFlags^ or cmsFLAGS_NOTPRECALC;
    result := cmsBuildGrayOutputMatrixShaper(OutputProfile);
    exit;
  end;

  if (not cmsReadICCMatrixRGB2XYZ(@DoubleMat, OutputProfile)) then
  begin
    result := nil;
    exit;
  end;

  if (MAT3inverse(@DoubleMat, @DoubleInv) < 0) then
  begin
    result := nil;
    exit;
  end;

  InverseShapes[0] := cmsReadICCGammaReversed(OutputProfile, icSigRedTRCTag);
  InverseShapes[1] := cmsReadICCGammaReversed(OutputProfile, icSigGreenTRCTag);
  InverseShapes[2] := cmsReadICCGammaReversed(OutputProfile, icSigBlueTRCTag);

  OutMatSh := cmsAllocMatShaper(@DoubleInv, @InverseShapes, MATSHAPER_OUTPUT);

  cmsFreeGammaTriple(@InverseShapes);

  result := OutMatSh;
end;

function PickTransformRoutine(p: _LPcmsTRANSFORM; dwFlagsPtr: PDWORD; FromTagPtr: PicTagSignature; ToTagPtr: PicTagSignature): _LPcmsTRANSFORM;
begin

  if (cmsGetDeviceClass(p^.InputProfile) = icSigNamedColorClass) then
  begin

    p^.FromDevice := NC2toPCS;
  end
  else
  begin

    if (FromTagPtr^ = icTagSignature(0)) and (ToTagPtr^ = icTagSignature(0)) and (p^.PreviewProfile <> nil) and
      (p^.Intent <> INTENT_ABSOLUTE_COLORIMETRIC) and (p^.EntryColorSpace = icSigRgbData) and
      (p^.ExitColorSpace = icSigRgbData) then
    begin

      p^.xform := MatrixShaperXFORM;
      dwFlagsPtr^ := dwFlagsPtr^ or cmsFLAGS_NOTPRECALC;

      if (not cmsBuildSmeltMatShaper(p)) then
      begin
        //cmsSignalError(LCMS_ERRC_ABORTED, "unable to smelt shaper-matrix, required tags missing");
        result := nil;
        exit;
      end;
      p^.Phase3 := XYZRel;
      p^.Phase1 := p^.Phase3;
      result := p;
      exit;

    end;

    if (FromTagPtr^ <> icTagSignature(0)) then
    begin

      p^.FromDevice := LUTtoPCS;
      p^.Device2PCS := cmsReadICCLut(p^.InputProfile, FromTagPtr^);
      if (p^.Device2PCS = nil) then
      begin

        //cmsSignalError(LCMS_ERRC_ABORTED, "profile is unsuitable for input");
        result := nil;
        exit;
      end;

    end
    else
    begin

      p^.FromDevice := ShaperMatrixToPCS;
      p^.InMatShaper := cmsBuildInputMatrixShaper(p^.InputProfile, dwFlagsPtr);

      if (p^.InMatShaper = nil) then
      begin
        //cmsSignalError(LCMS_ERRC_ABORTED, "profile is unsuitable for input");
        result := nil;
        exit;
      end;

      p^.Phase1 := XYZRel;

    end;

  end;

  if (ToTagPtr^ <> icTagSignature(0)) then
  begin

    p^.ToDevice := PCStoLUT;
    p^.PCS2Device := cmsReadICCLut(p^.OutputProfile, ToTagPtr^);
    if (p^.PCS2Device = nil) then
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "profile is unsuitable for output");
      result := nil;
      exit;
    end;

  end
  else
  begin

    p^.ToDevice := PCStoShaperMatrix;
    p^.OutMatShaper := cmsBuildOutputMatrixShaper(p^.OutputProfile, dwFlagsPtr);

    if (p^.OutMatShaper = nil) then
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "profile is unsuitable for output");
      result := nil;
      exit;
    end;
    p^.Phase3 := XYZRel;

  end;

  result := p;
end;

function cmsTakeMediaWhitePoint(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin

  Icc := LPLCMSICCPROFILE(hProfile);

  Dest^ := Icc^.MediaWhitePoint;

  result := TRUE;
end;

function cmsTakeMediaBlackPoint(Dest: LPcmsCIEXYZ; hProfile: cmsHPROFILE): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  Dest^ := Icc^.MediaBlackPoint;

  result := TRUE;
end;

function cmsTakeHeaderFlags(hProfile: cmsHPROFILE): dword;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  result := Icc^.flags;
end;

function cmsReadChromaticAdaptationMatrix(r: LPMAT3; hProfile: cmsHPROFILE): longbool;
var
  Icc: LPLCMSICCPROFILE;
begin

  if (ReadICCXYZArray(hProfile, icTagSignature(icSigChromaticAdaptationTag), r) < 0) then
  begin

    Icc := LPLCMSICCPROFILE(hProfile);

    MAT3identity(r);

    if ((cmsGetDeviceClass(hProfile) = icSigDisplayClass)) or
      ((cmsTakeHeaderFlags(hProfile) and icTransparency) <> 0) then
    begin

      cmsAdaptationMatrix(r, nil, @Icc^.MediaWhitePoint, @Icc^.Illuminant);
    end;
  end;

  result := TRUE;
end;

function cmsGetProfileICCversion(hProfile: cmsHPROFILE): dword;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  result := Icc^.Version;
end;

function cmsAdaptToIlluminant(xResult: LPcmsCIEXYZ;
  SourceWhitePt: LPcmsCIEXYZ;
  Illuminant: LPcmsCIEXYZ;
  Value: LPcmsCIEXYZ): longbool;
var
  Bradford: MAT3;
  xIn, xOut: VEC3;
begin

  cmsAdaptationMatrix(@Bradford, nil, SourceWhitePt, Illuminant);

  VEC3init(@xIn, Value^.X, Value^.Y, Value^.Z);
  MAT3eval(@xOut, @Bradford, @xIn);

  xResult^.X := xOut.n[0];
  xResult^.Y := xOut.n[1];
  xResult^.Z := xOut.n[2];

  result := TRUE;
end;

function GetV4PerceptualBlack(BlackPoint: LPcmsCIEXYZ; hProfile: cmsHPROFILE; dwFlags: DWORD): integer;
var
  D50BlackPoint, MediaWhite: cmsCIEXYZ;
begin
  if (dwFlags and LCMS_BPFLAGS_D50_ADAPTED) <> 0 then
  begin

    BlackPoint^.X := PERCEPTUAL_BLACK_X;
    BlackPoint^.Y := PERCEPTUAL_BLACK_Y;
    BlackPoint^.Z := PERCEPTUAL_BLACK_Z;
  end
  else
  begin

    cmsTakeMediaWhitePoint(@MediaWhite, hProfile);
    D50BlackPoint.X := PERCEPTUAL_BLACK_X;
    D50BlackPoint.Y := PERCEPTUAL_BLACK_Y;
    D50BlackPoint.Z := PERCEPTUAL_BLACK_Z;

    cmsAdaptToIlluminant(BlackPoint, cmsD50_XYZ, @MediaWhite, @D50BlackPoint);
  end;

  result := 1;
end;


function c_frexp(value: extended; eptr: pinteger): extended; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  frexp(value, result, eptr^);
end;


function CubeRoot(x: double): double;
var
  fr, r: double;
  ex, shx: integer;
begin

  fr := c_frexp(x, @ex);
  shx := ex mod 3;

  if (shx > 0) then
    shx := shx - 3;

  ex := (ex - shx) div 3;
  fr := ldexp(fr, shx);

  fr := (((((45.2548339756803022511987494 * fr +
    192.2798368355061050458134625) * fr +
    119.1654824285581628956914143) * fr +
    13.43250139086239872172837314) * fr +
    0.1636161226585754240958355063)
    /
    ((((14.80884093219134573786480845 * fr +
    151.9714051044435648658557668) * fr +
    168.5254414101568283957668343) * fr +
    33.9905941350215598754191872) * fr +
    1.0));
  r := ldexp(fr, ex);

  result := r;
end;

function f(t: double): double; {$ifdef IESUPPORTINLINE} inline; {$endif}
begin

  if (t <= 0.008856) then
    result := 7.787037037037037037037037037037 * t + (16 / 116)
  else
    result := CubeRoot(t);

end;

procedure cmsXYZ2Lab(WhitePoint: LPcmsCIEXYZ; Lab: LPcmsCIELab; xyz: LPcmsCIEXYZ);
var
  fx, fy, fz: double;
begin

  if (xyz^.X = 0) and (xyz^.Y = 0) and (xyz^.Z = 0) then
  begin
    Lab^.L := 0;
    Lab^.a := 0;
    Lab^.b := 0;
    exit;
  end;

  if (WhitePoint = nil) then
    WhitePoint := cmsD50_XYZ;

  fx := f(xyz^.X / WhitePoint^.X);
  fy := f(xyz^.Y / WhitePoint^.Y);
  fz := f(xyz^.Z / WhitePoint^.Z);

  Lab^.L := 116. * fy - 16.;

  Lab^.a := 500. * (fx - fy);
  Lab^.b := 200. * (fy - fz);
end;

function f_1(t: double): double;
var
  tmp: double;
begin

  if (t <= ((7.787 * 0.008856) + (16 / 116))) then
  begin

    tmp := ((t - (16 / 116)) / 7.787037037037037037037037037037);
    if (tmp <= 0.0) then
    begin
      result := 0.0;
      exit;
    end
    else
    begin
      result := tmp;
      exit;
    end;
  end;

  result := t * t * t;
end;

procedure cmsLab2XYZ(WhitePoint: LPcmsCIEXYZ; xyz: LPcmsCIEXYZ; Lab: LPcmsCIELab);
var
  x, y, z: double;
begin

  if (Lab^.L <= 0) then
  begin
    xyz^.X := 0;
    xyz^.Y := 0;
    xyz^.Z := 0;
    exit;
  end;

  if (WhitePoint = nil) then
    WhitePoint := cmsD50_XYZ;

  y := (Lab^.L + 16) / 116.0;
  x := y + 0.002 * Lab^.a;
  z := y - 0.005 * Lab^.b;

  xyz^.X := f_1(x) * WhitePoint^.X;
  xyz^.Y := f_1(y) * WhitePoint^.Y;
  xyz^.Z := f_1(z) * WhitePoint^.Z;

end;

function cmsTakeRenderingIntent(hProfile: cmsHPROFILE): integer;
var
  Icc: LPLCMSICCPROFILE;
begin
  Icc := LPLCMSICCPROFILE(hProfile);

  result := integer(Icc^.RenderingIntent);
end;

function cmsIsIntentSupported(hProfile: cmsHPROFILE;
  Intent: integer; UsedDirection: integer): longbool;
var
  TagTable: PicTagSignature;
begin

  if (cmsGetDeviceClass(hProfile) <> icSigLinkClass) then
  begin

    case (UsedDirection) of

      LCMS_USED_AS_INPUT: TagTable := @Device2PCS;
      LCMS_USED_AS_OUTPUT: TagTable := @PCS2Device;
      LCMS_USED_AS_PROOF: TagTable := @Preview;

    else
      begin
        //cmsSignalError(LCMS_ERRC_ABORTED, "Unexpected direction (%d)", UsedDirection);
        result := FALSE;
        exit;
      end;
    end;

    inc(TagTable, Intent);
    result := cmsIsTag(hProfile, TagTable^);
    exit;
  end;

  result := (cmsTakeRenderingIntent(hProfile) = Intent);
end;

(*
function TYPE_Lab_DBL: dword;
begin
  result :=        (COLORSPACE_SH(PT_Lab) or CHANNELS_SH(3) or BYTES_SH(0));
end;
*)

(*
function TYPE_CMYK_16: dword;
begin
  result :=           (COLORSPACE_SH(PT_CMYK) or CHANNELS_SH(4) or BYTES_SH(2));
end;
*)

function BlackPointUsingPerceptualBlack(BlackPoint: LPcmsCIEXYZ;
  hProfile: cmsHPROFILE;
  dwFlags: DWORD): integer;
var
  hPercLab2CMYK, hRelColCMYK2Lab: cmsHTRANSFORM;
  hLab: cmsHPROFILE;
  LabIn, LabOut: cmsCIELab;
  CMYK: array[0..MAXCHANNELS - 1] of WORD;
  BlackXYZ, MediaWhite: cmsCIEXYZ;

begin

  if (not cmsIsIntentSupported(hProfile, INTENT_PERCEPTUAL, LCMS_USED_AS_INPUT)) then
  begin

    BlackPoint^.Z := 0.0;
    BlackPoint^.Y := BlackPoint^.Z;
    BlackPoint^.X := BlackPoint^.Y;
    result := 0;
    exit;
  end;

  hLab := cmsCreateLabProfile(nil);

  hPercLab2CMYK := IEcmsCreateTransform(hLab, TYPE_Lab_DBL,
    hProfile, TYPE_CMYK_16,
    INTENT_PERCEPTUAL, cmsFLAGS_NOTPRECALC);

  hRelColCMYK2Lab := IEcmsCreateTransform(hProfile, TYPE_CMYK_16,
    hLab, TYPE_Lab_DBL,
    INTENT_RELATIVE_COLORIMETRIC, cmsFLAGS_NOTPRECALC);

  LabIn.b := 0;
  LabIn.a := LabIn.b;
  LabIn.L := LabIn.a;

  IEcmsDoTransform(hPercLab2CMYK, @LabIn, @CMYK, 1);
  IEcmsDoTransform(hRelColCMYK2Lab, @CMYK, @LabOut, 1);

  if (LabOut.L > 50) then
    LabOut.L := 50;
  LabOut.b := 0;
  LabOut.a := LabOut.b;

  IEcmsDeleteTransform(hPercLab2CMYK);
  IEcmsDeleteTransform(hRelColCMYK2Lab);
  IEcmsCloseProfile(hLab);

  cmsLab2XYZ(nil, @BlackXYZ, @LabOut);

  if ((dwFlags and LCMS_BPFLAGS_D50_ADAPTED) = 0) then
  begin
    cmsTakeMediaWhitePoint(@MediaWhite, hProfile);
    cmsAdaptToIlluminant(BlackPoint, cmsD50_XYZ(), @MediaWhite, @BlackXYZ);
  end
  else
    BlackPoint^ := BlackXYZ;

  result := 1;

end;

function BlackPointAsDarkerColorant(hInput: cmsHPROFILE;
  Intent: integer;
  BlackPoint: LPcmsCIEXYZ;
  dwFlags: DWORD): integer;
var
  White, Black: pWORD;
  xform: cmsHTRANSFORM;
  Space: icColorSpaceSignature;
  nChannels: integer;
  dwFormat: DWORD;
  hLab: cmsHPROFILE;
  Lab: cmsCIELab;
  BlackXYZ, MediaWhite: cmsCIEXYZ;
begin

  if (not cmsIsIntentSupported(hInput, Intent, LCMS_USED_AS_INPUT)) then
  begin

    BlackPoint^.Z := 0.0;
    BlackPoint^.Y := BlackPoint^.Z;
    BlackPoint^.X := BlackPoint^.Y;
    result := 0;
    exit;
  end;

  Space := cmsGetColorSpace(hInput);

  if (not _cmsEndPointsBySpace(Space, White, Black, @nChannels)) then
  begin

    BlackPoint^.Z := 0.0;
    BlackPoint^.Y := BlackPoint^.Z;
    BlackPoint^.X := BlackPoint^.Y;
    result := 0;
    exit;
  end;

  dwFormat := (nChannels shl 3) or 2;

  hLab := cmsCreateLabProfile(nil);

  xform := IEcmsCreateTransform(hInput, dwFormat,
    hLab, TYPE_Lab_DBL, Intent, cmsFLAGS_NOTPRECALC);

  IEcmsDoTransform(xform, Black, @Lab, 1);

  Lab.b := 0;
  Lab.a := Lab.b;
  if (Lab.L > 50) then
    Lab.L := 50;

  IEcmsCloseProfile(hLab);
  IEcmsDeleteTransform(xform);

  cmsLab2XYZ(nil, @BlackXYZ, @Lab);

  if (Intent = INTENT_ABSOLUTE_COLORIMETRIC) then
  begin

    BlackPoint^ := BlackXYZ;
  end
  else
  begin

    if ((dwFlags and LCMS_BPFLAGS_D50_ADAPTED) = 0) then
    begin

      cmsTakeMediaWhitePoint(@MediaWhite, hInput);
      cmsAdaptToIlluminant(BlackPoint, cmsD50_XYZ(), @MediaWhite, @BlackXYZ);
    end
    else
      BlackPoint^ := BlackXYZ;
  end;

  result := 1;
end;

function cmsDetectBlackPoint(BlackPoint: LPcmsCIEXYZ; hProfile: cmsHPROFILE; Intent: integer; dwFlags: DWORD): integer;
var
  BlackXYZ, UntrustedBlackPoint, TrustedBlackPoint, MediaWhite: cmsCIEXYZ;
  Lab: cmsCIELab;
begin

  if ((cmsGetProfileICCversion(hProfile) >= $4000000) and
    ((Intent = INTENT_PERCEPTUAL) or (Intent = INTENT_SATURATION))) then
  begin

    result := GetV4PerceptualBlack(BlackPoint, hProfile, dwFlags);
    exit;
  end;

  if (cmsIsTag(hProfile, icSigMediaBlackPointTag) and
    (Intent = INTENT_RELATIVE_COLORIMETRIC)) then
  begin

    cmsTakeMediaBlackPoint(@BlackXYZ, hProfile);
    cmsTakeMediaWhitePoint(@MediaWhite, hProfile);

    cmsAdaptToIlluminant(@UntrustedBlackPoint, @MediaWhite, cmsD50_XYZ, @BlackXYZ);

    cmsXYZ2Lab(nil, @Lab, @UntrustedBlackPoint);
    Lab.b := 0;
    Lab.a := Lab.b;
    if (Lab.L > 50) then
      Lab.L := 50;

    cmsLab2XYZ(nil, @TrustedBlackPoint, @Lab);

    if ((dwFlags and LCMS_BPFLAGS_D50_ADAPTED) = 0) then
      cmsAdaptToIlluminant(BlackPoint, cmsD50_XYZ, @MediaWhite, @TrustedBlackPoint)
    else
      BlackPoint^ := TrustedBlackPoint;
  end;

  if (Intent = INTENT_RELATIVE_COLORIMETRIC) and
    (cmsGetDeviceClass(hProfile) = icSigOutputClass) and
    (cmsGetColorSpace(hProfile) = icSigCmykData) then
  begin
    result := BlackPointUsingPerceptualBlack(BlackPoint, hProfile, dwFlags);
    exit;
  end;

  result := BlackPointAsDarkerColorant(hProfile, Intent, BlackPoint, dwFlags);

end;

procedure Rel2RelStepAbsCoefs(BlackPointIn: LPcmsCIEXYZ;
  WhitePointIn: LPcmsCIEXYZ;
  IlluminantIn: LPcmsCIEXYZ;
  ChromaticAdaptationMatrixIn: LPMAT3;

  BlackPointOut: LPcmsCIEXYZ;
  WhitePointOut: LPcmsCIEXYZ;
  IlluminantOut: LPcmsCIEXYZ;
  ChromaticAdaptationMatrixOut: LPMAT3;

  m: LPMAT3; xof: LPVEC3);

var
  WtPtIn, WtPtInAdapted: VEC3;
  WtPtOut, WtPtOutAdapted: VEC3;
  Scale, m1, m2, m3: MAT3;
begin

  VEC3init(@WtPtIn, WhitePointIn^.X, WhitePointIn^.Y, WhitePointIn^.Z);
  MAT3eval(@WtPtInAdapted, ChromaticAdaptationMatrixIn, @WtPtIn);

  VEC3init(@WtPtOut, WhitePointOut^.X, WhitePointOut^.Y, WhitePointOut^.Z);
  MAT3eval(@WtPtOutAdapted, ChromaticAdaptationMatrixOut, @WtPtOut);

  VEC3init(@Scale.v[0], WtPtInAdapted.n[0] / WtPtOutAdapted.n[0], 0, 0);
  VEC3init(@Scale.v[1], 0, WtPtInAdapted.n[1] / WtPtOutAdapted.n[1], 0);
  VEC3init(@Scale.v[2], 0, 0, WtPtInAdapted.n[2] / WtPtOutAdapted.n[2]);

  m1 := ChromaticAdaptationMatrixIn^;
  MAT3inverse(@m1, @m2);

  MAT3per(@m3, @m2, @Scale);
  MAT3per(m, @m3, ChromaticAdaptationMatrixOut);
  VEC3init(xof, 0.0, 0.0, 0.0);

end;

procedure XYZ2XYZ(xIn: pwordarray; xOut: pwordarray; m: LPWMAT3; xof: LPWVEC3);
var
  a, r: WVEC3;

begin

  a.n[0] := xIn[0] shl 1;
  a.n[1] := xIn[1] shl 1;
  a.n[2] := xIn[2] shl 1;

  MAT3evalW(@r, m, @a);

  xOut[0] := Clamp_XYZ((r.n[VX] + xof^.n[VX]) shr 1);
  xOut[1] := Clamp_XYZ((r.n[VY] + xof^.n[VY]) shr 1);
  xOut[2] := Clamp_XYZ((r.n[VZ] + xof^.n[VZ]) shr 1);
end;

procedure ComputeBlackPointCompensationFactors(BlackPointIn: LPcmsCIEXYZ;
  WhitePointIn: LPcmsCIEXYZ;
  IlluminantIn: LPcmsCIEXYZ;
  BlackPointOut: LPcmsCIEXYZ;
  WhitePointOut: LPcmsCIEXYZ;
  IlluminantOut: LPcmsCIEXYZ;
  m: LPMAT3; xof: LPVEC3);
var
  RelativeBlackPointIn, RelativeBlackPointOut: cmsCIEXYZ;
  ax, ay, az, bx, by, bz, tx, ty, tz: double;

begin

  cmsAdaptToIlluminant(@RelativeBlackPointIn, WhitePointIn, IlluminantIn, BlackPointIn);
  cmsAdaptToIlluminant(@RelativeBlackPointOut, WhitePointOut, IlluminantOut, BlackPointOut);

  tx := RelativeBlackPointIn.X - IlluminantIn^.X;
  ty := RelativeBlackPointIn.Y - IlluminantIn^.Y;
  tz := RelativeBlackPointIn.Z - IlluminantIn^.Z;

  ax := (RelativeBlackPointOut.X - IlluminantOut^.X) / tx;
  ay := (RelativeBlackPointOut.Y - IlluminantOut^.Y) / ty;
  az := (RelativeBlackPointOut.Z - IlluminantOut^.Z) / tz;

  bx := -IlluminantOut^.X * (RelativeBlackPointOut.X - RelativeBlackPointIn.X) / tx;
  by := -IlluminantOut^.Y * (RelativeBlackPointOut.Y - RelativeBlackPointIn.Y) / ty;
  bz := -IlluminantOut^.Z * (RelativeBlackPointOut.Z - RelativeBlackPointIn.Z) / tz;

  MAT3identity(m);

  m^.v[VX].n[0] := ax;
  m^.v[VY].n[1] := ay;
  m^.v[VZ].n[2] := az;

  VEC3init(xof, bx, by, bz);

end;

function Clamp_L(xin: Fixed32): word;
begin
  if (xin = $FFFF) then
  begin
    result := $FFFF;
    exit;
  end;

  if (xin > $FF00) then
  begin
    result := $FF00;
    exit;
  end;
  result := xin;
end;

function ENCODE_AB(x: double): word;  {$ifdef IESUPPORTINLINE} inline; {$endif}
begin
  result := round(((x) + 128.0) * 256.0 );
end;

function Clamp_ab(xin: Fixed32): word;
begin
  if (xin = $FFFF) then
  begin
    result := $FFFF;
    exit;
  end;

  if (xin < 0) then
  begin
    result := ENCODE_AB(-128.0);
    exit;
  end;
  if (xin > $FFFF) then
  begin
    result := ENCODE_AB(+127.9961);
    exit;
  end;
  result := xin;
end;

procedure cmsXYZ2LabEncoded(XYZ: pwordarray; Lab: pwordarray);
var
  X_ma, Y_ma, Z_ma: Fixed32;
  x_mi, y_mi, z_mi, L, a, b: double;
  fx, fy, fz: double;
  wL, wa, wb: Fixed32;
begin

  X_ma := XYZ[0] shl 1;
  Y_ma := XYZ[1] shl 1;
  Z_ma := XYZ[2] shl 1;

  if (X_ma = 0) and (Y_ma = 0) and (Z_ma = 0) then
  begin

    Lab[0] := 0;
    Lab[2] := $8000;
    Lab[1] := Lab[2];
    exit;
  end;

  x_mi := FIXED_TO_DOUBLE(X_ma) / D50X;
  y_mi := FIXED_TO_DOUBLE(Y_ma) / D50Y;
  z_mi := FIXED_TO_DOUBLE(Z_ma) / D50Z;

  fx := f(x_mi);
  fy := f(y_mi);
  fz := f(z_mi);

  L := 116 * fy - 16;

  a := 500 * (fx - fy);
  b := 200 * (fy - fz);

  a := a + 128;
  b := b + 128;

  wL := round(L * 652.800 );
  wa := round(a * 256.0 );
  wb := round(b * 256.0 );

  Lab[0] := Clamp_L(wL);
  Lab[1] := Clamp_ab(wa);
  Lab[2] := Clamp_ab(wb);

end;

procedure XYZ2Lab(xIn: pwordarray; xOut: pwordarray; m: LPWMAT3; xof: LPWVEC3);
var
  XYZ: array[0..2] of WORD;
begin

  XYZ2XYZ(xIn, @XYZ, m, xof);
  cmsXYZ2LabEncoded(@XYZ, xOut);
end;

function FromXYZRelLUT(xAbsolute: integer;
  BlackPointIn: LPcmsCIEXYZ;
  WhitePointIn: LPcmsCIEXYZ;
  IlluminantIn: LPcmsCIEXYZ;
  ChromaticAdaptationMatrixIn: LPMAT3;

  Phase2: integer; BlackPointOut: LPcmsCIEXYZ;
  WhitePointOut: LPcmsCIEXYZ;
  IlluminantOut: LPcmsCIEXYZ;
  ChromaticAdaptationMatrixOut: LPMAT3;

  DoBlackPointCompensation: integer;
  var fn1: _cmsADJFN;
  m: LPMAT3; xof: LPVEC3): integer;

begin
  case (Phase2) of

    XYZRel:

      if (xAbsolute <> 0) then
      begin

        Rel2RelStepAbsCoefs(BlackPointIn,
          WhitePointIn,
          IlluminantIn,
          ChromaticAdaptationMatrixIn,
          BlackPointOut,
          WhitePointOut,
          IlluminantOut,
          ChromaticAdaptationMatrixOut,
          m, xof);
        fn1 := XYZ2XYZ;

      end
      else
      begin

        fn1 := nil;
        if (DoBlackPointCompensation <> 0) then
        begin

          fn1 := XYZ2XYZ;
          ComputeBlackPointCompensationFactors(BlackPointIn,
            WhitePointIn,
            IlluminantIn,
            BlackPointOut,
            WhitePointOut,
            IlluminantOut,
            m, xof);

        end;
      end;

    LabRel:

      if (xAbsolute <> 0) then
      begin

        Rel2RelStepAbsCoefs(BlackPointIn,
          WhitePointIn,
          IlluminantIn,
          ChromaticAdaptationMatrixIn,
          BlackPointOut,
          WhitePointOut,
          IlluminantOut,
          ChromaticAdaptationMatrixOut,
          m, xof);

        fn1 := XYZ2Lab;

      end
      else
      begin

        MAT3identity(m);
        VEC3init(xof, 0, 0, 0);
        fn1 := XYZ2Lab;

        if (DoBlackPointCompensation <> 0) then
        begin

          ComputeBlackPointCompensationFactors(BlackPointIn,
            WhitePointIn,
            IlluminantIn,
            BlackPointOut,
            WhitePointOut,
            IlluminantOut,
            m, xof);
        end;
      end;

  else
    begin
      result := 0;
      exit;
    end;
  end;
  result := 1;
end;

procedure cmsLab2XYZEncoded(Lab: pwordarray; XYZ: pwordarray);
var
  L, a, b: double;
  X_ma, Y_ma, Z_ma, x_mi, y_mi, z_mi: double;

begin

  L := (Lab[0] * 100) / 65280;
  if (L = 0) then
  begin

    XYZ[0] := 0;
    XYZ[1] := 0;
    XYZ[2] := 0;
    exit;
  end;

  a := (Lab[1] / 256.0) - 128;
  b := (Lab[2] / 256.0) - 128;

  y_mi := (L + 16) / 116;
  x_mi := y_mi + 0.002 * a;
  z_mi := y_mi - 0.005 * b;

  X_ma := f_1(x_mi) * D50X;
  Y_ma := f_1(y_mi) * D50Y;
  Z_ma := f_1(z_mi) * D50Z;

  XYZ[0] := Clamp_XYZ(iefloor(X_ma * 32768 + 0.5));
  XYZ[1] := Clamp_XYZ(iefloor(Y_ma * 32768 + 0.5));
  XYZ[2] := Clamp_XYZ(iefloor(Z_ma * 32768 + 0.5));

end;

procedure Lab2XYZ(xIn: pwordarray; xOut: pwordarray; m: LPWMAT3; xof: LPWVEC3);
var
  XYZ: array[0..2] of word;
begin
  cmsLab2XYZEncoded(xIn, @XYZ);
  XYZ2XYZ(@XYZ, xOut, m, xof);
end;

procedure Lab2XYZ2Lab(xIn: pwordarray; xOut: pwordarray; m: LPWMAT3; xof: LPWVEC3);
var
  XYZ, XYZ2: array[0..2] of WORD;
begin
  cmsLab2XYZEncoded(xIn, @XYZ);
  XYZ2XYZ(@XYZ, @XYZ2, m, xof);
  cmsXYZ2LabEncoded(@XYZ2, xOut);
end;

function FromLabRelLUT(xAbsolute: integer;
  BlackPointIn: LPcmsCIEXYZ;
  WhitePointIn: LPcmsCIEXYZ;
  IlluminantIn: LPcmsCIEXYZ;
  ChromaticAdaptationMatrixIn: LPMAT3;

  Phase2: integer; BlackPointOut: LPcmsCIEXYZ;
  WhitePointOut: LPcmsCIEXYZ;
  IlluminantOut: LPcmsCIEXYZ;
  ChromaticAdaptationMatrixOut: LPMAT3;

  DoBlackPointCompensation: integer;
  var fn1: _cmsADJFN;
  m: LPMAT3; xof: LPVEC3): integer;
begin

  case (Phase2) of

    XYZRel:

      if (xAbsolute <> 0) then
      begin

        Rel2RelStepAbsCoefs(BlackPointIn,
          WhitePointIn,
          cmsD50_XYZ(),
          ChromaticAdaptationMatrixIn,
          BlackPointOut,
          WhitePointOut,
          IlluminantOut,
          ChromaticAdaptationMatrixOut,
          m, xof);

        fn1 := Lab2XYZ;

      end
      else
      begin

        fn1 := Lab2XYZ;
        if (DoBlackPointCompensation <> 0) then
        begin

          ComputeBlackPointCompensationFactors(BlackPointIn,
            WhitePointIn,
            IlluminantIn,
            BlackPointOut,
            WhitePointOut,
            IlluminantOut,
            m, xof);

        end;
      end;

    LabRel:

      if (xAbsolute <> 0) then
      begin

        Rel2RelStepAbsCoefs(BlackPointIn,
          WhitePointIn, IlluminantIn,
          ChromaticAdaptationMatrixIn,
          BlackPointOut,
          WhitePointOut, cmsD50_XYZ(),
          ChromaticAdaptationMatrixOut,
          m, xof);
        fn1 := Lab2XYZ2Lab;
      end
      else
      begin

        fn1 := nil;
        if (DoBlackPointCompensation <> 0) then
        begin

          fn1 := Lab2XYZ2Lab;
          ComputeBlackPointCompensationFactors(BlackPointIn,
            WhitePointIn,
            IlluminantIn,
            BlackPointOut,
            WhitePointOut,
            IlluminantOut,
            m, xof);

        end;
      end;

  else
    begin
      result := 0;
      exit;
    end;
  end;

  result := 1;
end;

procedure VEC3initF(r: LPWVEC3; x, y, z: double);
begin
  r^.n[VX] := DOUBLE_TO_FIXED(x);
  r^.n[VY] := DOUBLE_TO_FIXED(y);
  r^.n[VZ] := DOUBLE_TO_FIXED(z);
end;

function IdentityParameters(m: LPWMAT3; xof: LPWVEC3): longbool;
var
  wv0: WVEC3;
begin

  VEC3initF(@wv0, 0, 0, 0);

  if (not MAT3isIdentity(m, 0.00001)) then
  begin
    result := FALSE;
    exit;
  end;
  if (not VEC3equal(xof, @wv0, 0.00001)) then
  begin
    result := FALSE;
    exit;
  end;

  result := TRUE;
end;

function cmsChooseCnvrt(xAbsolute: integer;
  Phase1: integer; BlackPointIn: LPcmsCIEXYZ;
  WhitePointIn: LPcmsCIEXYZ;
  IlluminantIn: LPcmsCIEXYZ;
  ChromaticAdaptationMatrixIn: LPMAT3;

  Phase2: integer; BlackPointOut: LPcmsCIEXYZ;
  WhitePointOut: LPcmsCIEXYZ;
  IlluminantOut: LPcmsCIEXYZ;
  ChromaticAdaptationMatrixOut: LPMAT3;

  DoBlackPointCompensation: integer;
  var fn1: _cmsADJFN;
  wm: LPWMAT3; wof: LPWVEC3): integer;

var
  rc: integer;
  m: MAT3;
  xof: VEC3;

begin

  MAT3identity(@m);
  VEC3init(@xof, 0, 0, 0);

  case (Phase1) of

    XYZRel: rc := FromXYZRelLUT(xAbsolute,
        BlackPointIn,
        WhitePointIn,
        IlluminantIn,
        ChromaticAdaptationMatrixIn,
        Phase2,
        BlackPointOut,
        WhitePointOut,
        IlluminantOut,
        ChromaticAdaptationMatrixOut,
        DoBlackPointCompensation,
        fn1, @m, @xof);

    LabRel: rc := FromLabRelLUT(xAbsolute,
        BlackPointIn,
        WhitePointIn,
        IlluminantIn,
        ChromaticAdaptationMatrixIn,
        Phase2,
        BlackPointOut,
        WhitePointOut,
        IlluminantOut,
        ChromaticAdaptationMatrixOut,
        DoBlackPointCompensation,
        fn1, @m, @xof);

  else
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "(internal) Phase error");
      result := 0;
      exit;
    end;
  end;

  MAT3toFix(wm, @m);
  VEC3toFix(wof, @xof);

  if (@fn1 = @XYZ2XYZ) or (@fn1 = @Lab2XYZ2Lab) then
  begin

    if (IdentityParameters(wm, wof)) then
      fn1 := nil;
  end;

  result := rc;
end;

procedure TakeConversionRoutines(p: _LPcmsTRANSFORM; DoBPC: integer);
var
  BlackPointIn, WhitePointIn, IlluminantIn: cmsCIEXYZ;
  BlackPointOut, WhitePointOut, IlluminantOut: cmsCIEXYZ;
  BlackPointProof, WhitePointProof, IlluminantProof: cmsCIEXYZ;
  ChromaticAdaptationMatrixIn, ChromaticAdaptationMatrixOut: MAT3;
  ChromaticAdaptationMatrixProof: MAT3;

begin

  cmsTakeIluminant(@IlluminantIn, p^.InputProfile);
  cmsTakeMediaWhitePoint(@WhitePointIn, p^.InputProfile);
  cmsTakeMediaBlackPoint(@BlackPointIn, p^.InputProfile);
  cmsReadChromaticAdaptationMatrix(@ChromaticAdaptationMatrixIn, p^.InputProfile);

  cmsTakeIluminant(@IlluminantOut, p^.OutputProfile);
  cmsTakeMediaWhitePoint(@WhitePointOut, p^.OutputProfile);
  cmsTakeMediaBlackPoint(@BlackPointOut, p^.OutputProfile);
  cmsReadChromaticAdaptationMatrix(@ChromaticAdaptationMatrixOut, p^.OutputProfile);

  if (p^.Preview = nil) then
  begin

    if (p^.Intent = INTENT_PERCEPTUAL) or
      (p^.Intent = INTENT_SATURATION) then
    begin

      if ((cmsGetProfileICCversion(p^.InputProfile) >= $4000000) or
        (cmsGetProfileICCversion(p^.OutputProfile) >= $4000000)) then
      begin

        DoBPC := 1;
      end;
    end;

    if (p^.Intent = INTENT_ABSOLUTE_COLORIMETRIC) then
      DoBPC := 0;

    if ((cmsGetDeviceClass(p^.InputProfile) = icSigAbstractClass) or
      (cmsGetDeviceClass(p^.InputProfile) = icSigLinkClass)) then
      DoBPC := 0;

    if ((cmsGetDeviceClass(p^.OutputProfile) = icSigAbstractClass) or
      (cmsGetDeviceClass(p^.OutputProfile) = icSigLinkClass)) then
      DoBPC := 0;

    if (DoBPC <> 0) then
    begin

      cmsDetectBlackPoint(@BlackPointIn, p^.InputProfile, p^.Intent, 0);
      cmsDetectBlackPoint(@BlackPointOut, p^.OutputProfile, p^.Intent, 0);

      if (BlackPointIn.X = BlackPointOut.X) and
        (BlackPointIn.Y = BlackPointOut.Y) and
        (BlackPointIn.Z = BlackPointOut.Z) then
        DoBPC := 0;

    end;

    cmsChooseCnvrt(integer(p^.Intent = INTENT_ABSOLUTE_COLORIMETRIC),

      p^.Phase1,
      @BlackPointIn,
      @WhitePointIn,
      @IlluminantIn,
      @ChromaticAdaptationMatrixIn,

      p^.Phase3,
      @BlackPointOut,
      @WhitePointOut,
      @IlluminantOut,
      @ChromaticAdaptationMatrixOut,

      DoBPC,
      p^.Stage1,
      @p^.m1, @p^.of1);

  end
  else
  begin

    cmsTakeIluminant(@IlluminantProof, p^.PreviewProfile);
    cmsTakeMediaWhitePoint(@WhitePointProof, p^.PreviewProfile);
    cmsTakeMediaBlackPoint(@BlackPointProof, p^.PreviewProfile);
    cmsReadChromaticAdaptationMatrix(@ChromaticAdaptationMatrixProof, p^.PreviewProfile);

    if (DoBPC <> 0) then
    begin

      cmsDetectBlackPoint(@BlackPointProof, p^.PreviewProfile, p^.Intent, 0);
      cmsDetectBlackPoint(@BlackPointIn, p^.InputProfile, p^.Intent, 0);
      cmsDetectBlackPoint(@BlackPointOut, p^.OutputProfile, p^.Intent, 0);

      if (BlackPointIn.X = BlackPointProof.X) and
        (BlackPointIn.Y = BlackPointProof.Y) and
        (BlackPointIn.Z = BlackPointProof.Z) then
        DoBPC := 0;

    end;

    cmsChooseCnvrt(integer(p^.Intent = INTENT_ABSOLUTE_COLORIMETRIC),

      p^.Phase1,
      @BlackPointIn,
      @WhitePointIn,
      @IlluminantIn,
      @ChromaticAdaptationMatrixIn,

      p^.Phase2,
      @BlackPointProof,
      @WhitePointProof,
      @IlluminantProof,
      @ChromaticAdaptationMatrixProof,
      DoBPC,
      p^.Stage1,
      @p^.m1, @p^.of1);

    cmsChooseCnvrt(integer(p^.ProofIntent = INTENT_ABSOLUTE_COLORIMETRIC),

      p^.Phase2,
      @BlackPointProof,
      @WhitePointProof,
      @IlluminantProof,
      @ChromaticAdaptationMatrixProof,

      p^.Phase3,
      @BlackPointOut,
      @WhitePointOut,
      @IlluminantOut,
      @ChromaticAdaptationMatrixOut,
      0,
      p^.Stage2,
      @p^.m2, @p^.of2);
  end;

end;

function _cmsChannelsOf(ColorSpace: icColorSpaceSignature): integer;
begin

  case (ColorSpace) of

    icSigGrayData: result := 1;

    icSig2colorData: result := 2;

    icSigXYZData,
      icSigLabData,
      icSigLuvData,
      icSigYCbCrData,
      icSigYxyData,
      icSigRgbData,
      icSigHsvData,
      icSigHlsData,
      icSigCmyData,
      icSig3colorData: result := 3;

    icColorSpaceSignature(icSigLuvKData),
      icSigCmykData,
      icSig4colorData: result := 4;

    icSig5colorData: result := 5;

    icColorSpaceSignature(icSigHexachromeData),
      icSig6colorData: result := 6;

    icColorSpaceSignature(icSigHeptachromeData),
      icSig7colorData: result := 7;

    icColorSpaceSignature(icSigOctachromeData),
      icSig8colorData: result := 8;

    icSig9colorData: result := 9;
    icSig10colorData: result := 10;
    icSig11colorData: result := 11;
    icSig12colorData: result := 12;
    icSig13colorData: result := 13;
    icSig14colorData: result := 14;
    icSig15colorData: result := 15;

  else
    result := 3;
  end;

end;

function _cmsReasonableGridpointsByColorspace(Colorspace: icColorSpaceSignature; dwFlags: DWORD): integer;
var
  nChannels: integer;
begin

  nChannels := _cmsChannelsOf(Colorspace);

  if (dwFlags and cmsFLAGS_HIGHRESPRECALC) <> 0 then
  begin

    if (nChannels > 4) then
    begin
      result := 7;
      exit;
    end;

    if (nChannels = 4) then
    begin
      result := 23;
      exit;
    end;

    result := 48;
    exit;
  end;

  if (dwFlags and cmsFLAGS_LOWRESPRECALC) <> 0 then
  begin

    if (nChannels > 4) then
    begin
      result := 6;
      exit;
    end;

    if (nChannels = 1) then
    begin
      result := 33;
      exit;
    end;

    result := 17;
    exit;
  end;

  if (nChannels > 4) then
  begin
    result := 7;
    exit;
  end;

  if (nChannels = 4) then
  begin
    result := 17;
    exit;
  end;

  result := 33;

end;

function MostlyLinear(Table: pwordarray; nEntries: integer): integer;
var
  i: integer;
  diff: integer;
begin

  for i := 5 to nEntries - 1 do
  begin

    diff := abs(Table[i] - _cmsQuantizeVal(i, nEntries));
    if (diff > $0300) then
    begin
      result := 0;
      exit;
    end;
  end;

  result := 1;
end;

function IsMonotonic(t: LPGAMMATABLE): longbool;
var
  n: integer;
  i, last: integer;
begin
  n := t^.nEntries;

  last := t^.GammaTable[n - 1];

  for i := n - 2 downto 0 do
  begin

    if (t^.GammaTable[i] > last) then
    begin

      result := FALSE;
      exit;
    end
    else
      last := t^.GammaTable[i];

  end;

  result := TRUE;
end;

procedure SlopeLimiting(Table: pwordarray; nEntries: integer);
var
  At: integer;
  Val, Slope: double;
  i: integer;
begin
  At := iefloor(nEntries * 0.02 + 0.5);

  Val := Table[At];
  Slope := Val / At;

  for i := 0 to At - 1 do
    Table[i] := iefloor(i * Slope + 0.5);

end;

const
  PRELINEARIZATION_POINTS = 4096;

procedure _cmsComputePrelinearizationTablesFromXFORM(h: PcmsHTRANSFORM; nTransforms: integer; Grid: LPLUT);
var
  Trans: array[0..MAXCHANNELS - 1] of LPGAMMATABLE;
  t, i, v: dword;
  j: integer;
  xIn, xOut: array[0..MAXCHANNELS - 1] of WORD;
  lIsSuitable: longBOOL;
  hh: PcmsHTRANSFORM;

begin

  hh := h;

  for t := 0 to Grid^.InputChan - 1 do
    Trans[t] := cmsAllocGamma(PRELINEARIZATION_POINTS);

  for i := 0 to PRELINEARIZATION_POINTS - 1 do
  begin

    h := hh;

    v := _cmsQuantizeVal(i, PRELINEARIZATION_POINTS);

    for t := 0 to Grid^.InputChan - 1 do
      xIn[t] := v;

    IEcmsDoTransform(h^, @xIn, @xOut, 1);
    inc(h);
    for j := 1 to nTransforms - 1 do
    begin
      IEcmsDoTransform(h^, @xOut, @xOut, 1);
      inc(h);
    end;

    for t := 0 to Grid^.InputChan - 1 do
      Trans[t]^.GammaTable[i] := xOut[t];

  end;

  lIsSuitable := TRUE;
  t := 0;
  while (lIsSuitable and (t < Grid^.InputChan)) do
  begin

    if (MostlyLinear(@Trans[t]^.GammaTable, PRELINEARIZATION_POINTS)) <> 0 then
      lIsSuitable := FALSE;

    if (not IsMonotonic(Trans[t])) then
      lIsSuitable := FALSE;

    inc(t);

  end;

  if (lIsSuitable) then
  begin

    for t := 0 to Grid^.InputChan - 1 do
      SlopeLimiting(@Trans[t]^.GammaTable, Trans[t]^.nEntries);
  end;

  if (lIsSuitable) then
    cmsAllocLinearTable(Grid, @Trans, 1);

  for t := 0 to Grid^.InputChan - 1 do
    cmsFreeGamma(Trans[t]);

end;

function XFormSampler(xIn: pwordarray; xOut: pwordarray; Cargo: pointer): integer;
begin
  IEcmsDoTransform(cmsHTRANSFORM(Cargo), xIn, xOut, 1);
  result := 1;
end;

function _cmsPrecalculateDeviceLink(h: cmsHTRANSFORM; dwFlags: DWORD): LPLUT;
var
  p: _LPcmsTRANSFORM;
  Grid: LPLUT;
  nGridPoints: integer;
  dwFormatIn, dwFormatOut: DWORD;
  ChannelsIn, ChannelsOut: integer;
  hOne: array[0..0] of cmsHTRANSFORM;

begin
  p := _LPcmsTRANSFORM(h);

  ChannelsIn := _cmsChannelsOf(p^.EntryColorSpace);
  ChannelsOut := _cmsChannelsOf(p^.ExitColorSpace);

  if (dwFlags and $00FF0000) <> 0 then
  begin

    nGridPoints := (dwFlags shr 16) and $FF;

  end
  else
  begin

    nGridPoints := _cmsReasonableGridpointsByColorspace(p^.EntryColorSpace, dwFlags);
  end;

  Grid := cmsAllocLUT();
  if (Grid = nil) then
  begin
    result := nil;
    exit;
  end;

  Grid := cmsAlloc3DGrid(Grid, nGridPoints, ChannelsIn, ChannelsOut);

  dwFormatIn := ((ChannelsIn shl 3) or 2);
  dwFormatOut := ((ChannelsOut shl 3) or 2);

  p^.FromInput := _cmsIdentifyInputFormat(p, dwFormatIn);
  p^.ToOutput := _cmsIdentifyOutputFormat(p, dwFormatOut);

  if (p^.EntryColorSpace = icSigRgbData) and
    (p^.ExitColorSpace = icSigRgbData) and
    ((dwFlags and cmsFLAGS_NOPRELINEARIZATION) = 0) then
  begin

    hOne[0] := h;

    _cmsComputePrelinearizationTablesFromXFORM(@hOne, 1, Grid);
  end;

  if (not cmsSample3DGrid(Grid, XFormSampler, p, Grid^.wFlags)) then
  begin

    cmsFreeLUT(Grid);
    result := nil;
    exit;
  end;

  result := Grid;
end;

procedure cmsTetrahedralInterp8(Input: pwordarray;
  Output: pwordarray;
  LutTable: pwordarray;
  p: LPL16PARAMS);
var
  r, g, b: integer;
  rx, ry, rz: Fixed32;
  c1, c2, c3, Rest: Fixed32;
  OutChan: integer;
  X0, X1, Y0, Y1, Z0, Z1: Fixed32;
  TotalOut: integer;
  p8: LPL8PARAMS;

  function DENS(i, j, k: Fixed32): Fixed32;
  begin
    result := (LutTable[(i) + (j) + (k) + OutChan])
  end;

begin

  TotalOut := p^.nOutputs;
  p8 := p^.p8;

  r := Input[0] shr 8;
  g := Input[1] shr 8;
  b := Input[2] shr 8;

  X1 := p8^.X0[r];
  X0 := X1;
  Y1 := p8^.Y0[g];
  Y0 := Y1;
  Z1 := p8^.Z0[b];
  Z0 := Z1;

  if (r <> 255) then
    X1 := X1 + p^.opta3;

  if (g <> 255) then
    Y1 := Y1 + p^.opta2;

  if (b <> 255) then
    Z1 := Z1 + p^.opta1;

  rx := p8^.rx[r];
  ry := p8^.ry[g];
  rz := p8^.rz[b];

  for OutChan := 0 to TotalOut - 1 do
  begin

    if (rx >= ry) and (ry >= rz) then
    begin

      c1 := DENS(X1, Y0, Z0) - DENS(X0, Y0, Z0);
      c2 := DENS(X1, Y1, Z0) - DENS(X1, Y0, Z0);
      c3 := DENS(X1, Y1, Z1) - DENS(X1, Y1, Z0);

    end
    else
    if (rx >= rz) and (rz >= ry) then
    begin

      c1 := DENS(X1, Y0, Z0) - DENS(X0, Y0, Z0);
      c2 := DENS(X1, Y1, Z1) - DENS(X1, Y0, Z1);
      c3 := DENS(X1, Y0, Z1) - DENS(X1, Y0, Z0);

    end
    else
    if (rz >= rx) and (rx >= ry) then
    begin

      c1 := DENS(X1, Y0, Z1) - DENS(X0, Y0, Z1);
      c2 := DENS(X1, Y1, Z1) - DENS(X1, Y0, Z1);
      c3 := DENS(X0, Y0, Z1) - DENS(X0, Y0, Z0);

    end
    else
    if (ry >= rx) and (rx >= rz) then
    begin

      c1 := DENS(X1, Y1, Z0) - DENS(X0, Y1, Z0);
      c2 := DENS(X0, Y1, Z0) - DENS(X0, Y0, Z0);
      c3 := DENS(X1, Y1, Z1) - DENS(X1, Y1, Z0);

    end
    else
    if (ry >= rz) and (rz >= rx) then
    begin

      c1 := DENS(X1, Y1, Z1) - DENS(X0, Y1, Z1);
      c2 := DENS(X0, Y1, Z0) - DENS(X0, Y0, Z0);
      c3 := DENS(X0, Y1, Z1) - DENS(X0, Y1, Z0);

    end
    else
    if (rz >= ry) and (ry >= rx) then
    begin

      c1 := DENS(X1, Y1, Z1) - DENS(X0, Y1, Z1);
      c2 := DENS(X0, Y1, Z1) - DENS(X0, Y0, Z1);
      c3 := DENS(X0, Y0, Z1) - DENS(X0, Y0, Z0);

    end
    else
    begin
      c3 := 0;
      c2 := c3;
      c1 := c2;

    end;

    Rest := c1 * rx + c2 * ry + c3 * rz;

    Output[OutChan] := (DENS(X0, Y0, Z0) + ((ToFixedDomain(Rest)) + $8000) shr 16);

  end;

end;

function _cmsBlessLUT8(Lut: LPLUT): LPLUT;
var
  i, j: integer;
  StageABC: array[0..2] of WORD;
  v1, v2, v3: Fixed32;
  p8: LPL8PARAMS;
  p: LPL16PARAMS;
begin
  p := @Lut^.CLut16params;

  getmem(p8, sizeof(L8PARAMS));
  if (p8 = nil) then
  begin
    result := nil;
    exit;
  end;

  for i := 0 to 255 do
  begin

    StageABC[2] := RGB_8_TO_16(i);
    StageABC[1] := StageABC[2];
    StageABC[0] := StageABC[1];

    if (Lut^.wFlags and LUT_HASTL1) <> 0 then
    begin

      for j := 0 to 2 do
        StageABC[i] := cmsLinearInterpLUT16(StageABC[i],
          pwordarray(Lut^.L1[i]),
          @Lut^.In16params);
      Lut^.wFlags := Lut^.wFlags and (not LUT_HASTL1);
    end;

    v1 := ToFixedDomain(StageABC[0] * p^.Domain);
    v2 := ToFixedDomain(StageABC[1] * p^.Domain);
    v3 := ToFixedDomain(StageABC[2] * p^.Domain);

    p8^.X0[i] := p^.opta3 * FIXED_TO_INT(v1);
    p8^.Y0[i] := p^.opta2 * FIXED_TO_INT(v2);
    p8^.Z0[i] := p^.opta1 * FIXED_TO_INT(v3);

    p8^.rx[i] := FIXED_REST_TO_INT(v1);
    p8^.ry[i] := FIXED_REST_TO_INT(v2);
    p8^.rz[i] := FIXED_REST_TO_INT(v3);

  end;

  Lut^.CLut16params.p8 := p8;
  Lut^.CLut16params.Interp3D := cmsTetrahedralInterp8;

  result := Lut;

end;

// Create a transform.

function cmsCreateProofingTransform(InputProfile: cmsHPROFILE; InputFormat: DWORD; OutputProfile: cmsHPROFILE; OutputFormat: DWORD; ProofingProfile: cmsHPROFILE; nIntent: integer; ProofingIntent: integer; dwFlags: DWORD): cmsHTRANSFORM;
var
  p: _LPcmsTRANSFORM;
  FromTag: icTagSignature;
  ToTag: icTagSignature;
  DeviceLink: LPLUT;
begin

  if (nIntent < 0) or (nIntent > 3) or (ProofingIntent < 0) or (ProofingIntent > 3) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "cmsCreateTransform: intent mismatch");
    result := nil;
    exit;
  end;

  p := AllocEmptyTransform;
  if (p = nil) then
  begin
    result := nil;
    exit;
  end;

  p^.xform := NormalXFORM;
  p^.Intent := nIntent;
  p^.ProofIntent := ProofingIntent;
  p^.DoGamutCheck := 0;
  p^.InputProfile := InputProfile;
  p^.OutputProfile := OutputProfile;
  p^.PreviewProfile := ProofingProfile;
  p^.InputFormat := InputFormat;
  p^.OutputFormat := OutputFormat;
  p^.lOutputV4Lab := false;
  p^.lInputV4Lab := p^.lOutputV4Lab;

  p^.FromInput := _cmsIdentifyInputFormat(p, InputFormat);
  p^.ToOutput := _cmsIdentifyOutputFormat(p, OutputFormat);

  if (((dwFlags and cmsFLAGS_NULLTRANSFORM) <> 0) or ((InputProfile = nil) and (OutputProfile = nil))) then
  begin
    p^.xform := NullXFORM;
    result := cmsHTRANSFORM(p);
    exit;
  end;

  if (InputProfile = nil) then
  begin

    //cmsSignalError(LCMS_ERRC_ABORTED, "Input profile cannot be NULL!");
    IEcmsDeleteTransform(cmsHTRANSFORM(p));
    result := nil;
    exit;
  end;

  if (cmsGetDeviceClass(InputProfile) = icSigLinkClass) then
  begin

    result := CreateDeviceLinkTransform(p, dwFlags);
    exit;
  end;

  if (not IsProperColorSpace(InputProfile, InputFormat, FALSE)) then
  begin

    //cmsSignalError(LCMS_ERRC_ABORTED, "Input profile is operating on wrong colorspace");
    IEcmsDeleteTransform(cmsHTRANSFORM(p));
    result := nil;
    exit;
  end;

  p^.EntryColorSpace := cmsGetColorSpace(InputProfile);

  if (cmsGetDeviceClass(InputProfile) = icSigNamedColorClass) then
  begin

    if (p^.NamedColorList = nil) then
      p^.NamedColorList := cmsAllocNamedColorList(0);

    cmsReadICCnamedColorList(p, InputProfile, icSigNamedColor2Tag);

    if (OutputProfile = nil) then
    begin

      p^.ExitColorSpace := p^.EntryColorSpace;
      p^.xform := @NC2deviceXform;
      result := cmsHTRANSFORM(p);
      exit;
    end;

    dwFlags := dwFlags or cmsFLAGS_NOTPRECALC;
  end;

  if (OutputProfile = nil) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "Output profile cannot be NULL!");
    IEcmsDeleteTransform(cmsHTRANSFORM(p));
    result := nil;
    exit;
  end;

  if (not IsProperColorSpace(OutputProfile, OutputFormat, FALSE)) then
  begin
    //cmsSignalError(LCMS_ERRC_ABORTED, "Output profile is operating on wrong colorspace");
    IEcmsDeleteTransform(cmsHTRANSFORM(p));
    result := nil;
    exit;
  end;

  p^.ExitColorSpace := cmsGetColorSpace(OutputProfile);

  if (cmsGetDeviceClass(OutputProfile) = icSigNamedColorClass) then
  begin

    //cmsSignalError(LCMS_ERRC_ABORTED, "Named color profiles are not supported as output");
    IEcmsDeleteTransform(cmsHTRANSFORM(p));
    result := nil;
    exit;
  end;

  p^.Phase1 := GetPhase(InputProfile);
  p^.Phase2 := -1;
  p^.Phase3 := GetPhase(OutputProfile);

  FromTag := Device2PCS[nIntent];
  ToTag := PCS2Device[nIntent];

  if (not cmsIsTag(InputProfile, FromTag)) then
  begin
    FromTag := Device2PCS[0];
    if (not cmsIsTag(InputProfile, FromTag)) then
    begin
      FromTag := icTagSignature(0);
    end;
  end;

  if (ProofingProfile <> nil) then
    CreateProof(p, dwFlags, @ToTag);

  if (not cmsIsTag(OutputProfile, ToTag)) then
  begin

    ToTag := PCS2Device[0];

    if (cmsGetDeviceClass(OutputProfile) = icSigAbstractClass) then
    begin

      if (not cmsIsTag(OutputProfile, ToTag)) then
      begin
        ToTag := icTagSignature(icSigAToB0Tag);
      end;
    end;
    if (not cmsIsTag(OutputProfile, ToTag)) then
      ToTag := icTagSignature(0);
  end;

  if (dwFlags and cmsFLAGS_MATRIXINPUT) <> 0 then
    FromTag := icTagSignature(0);

  if (dwFlags and cmsFLAGS_MATRIXOUTPUT) <> 0 then
    ToTag := icTagSignature(0);

  if (dwFlags and cmsFLAGS_GAMUTCHECK) <> 0 then
  begin

    p^.DoGamutCheck := 1;
    dwFlags := dwFlags or cmsFLAGS_NOTPRECALC;
  end;

  if (PickTransformRoutine(p, @dwFlags, @FromTag, @ToTag) = nil) then
  begin

    IEcmsDeleteTransform(cmsHTRANSFORM(p));
    result := nil;
    exit;

  end;

  TakeConversionRoutines(p, dwFlags and cmsFLAGS_BLACKPOINTCOMPENSATION);

  if ((dwFlags and cmsFLAGS_NOTPRECALC) = 0) then
  begin

    DeviceLink := _cmsPrecalculateDeviceLink(cmsHTRANSFORM(p), dwFlags);

    if (p^.EntryColorSpace = icSigRgbData) or (p^.EntryColorSpace = icSigCmyData) then
    begin

      DeviceLink^.CLut16params.Interp3D := cmsTetrahedralInterp16;
    end;

    if (((InputFormat and 7) = 1) and (T_CHANNELS(InputFormat) = 3)) then
    begin

      DeviceLink := _cmsBlessLUT8(DeviceLink);
      if (DeviceLink = nil) then
      begin
        result := nil;
        exit;
      end;

    end;

    if (DeviceLink <> nil) then
    begin

      p^.DeviceLink := DeviceLink;
    end
    else
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "Cannot precalculate %d->%d channels transform!", T_CHANNELS(InputFormat), T_CHANNELS(OutputFormat));
      IEcmsDeleteTransform(p);
      result := nil;
      exit;
    end;

    SetPrecalculatedTransform(p, dwFlags);

  end;

  p^.FromInput := _cmsIdentifyInputFormat(p, InputFormat);
  p^.ToOutput := _cmsIdentifyOutputFormat(p, OutputFormat);

  result := p;
end;

// Wrapper por simpler non-proofing transforms.

function IEcmsCreateTransform(Input: cmsHPROFILE; InputFormat: DWORD; Output: cmsHPROFILE; OutputFormat: DWORD; Intent: integer; dwFlags: DWORD): cmsHTRANSFORM;
begin
  result := cmsCreateProofingTransform(Input, InputFormat, Output, OutputFormat, nil, Intent, INTENT_ABSOLUTE_COLORIMETRIC, dwFlags);
end;

function cmsWhitePointFromTemp(TempK: integer; WhitePoint: LPcmsCIExyY): longbool;
var
  x, y: double;
  T, T2, T3: double;

begin

  T := TempK;
  T2 := T * T;
  T3 := T2 * T;

  if (T >= 4000) and (T <= 7000) then
  begin
    x := -4.6070 * (1E9 / T3) + 2.9678 * (1E6 / T2) + 0.09911 * (1E3 / T) + 0.244063;
  end
  else
    if (T > 7000.0) and (T <= 25000.0) then
    begin
      x := -2.0064 * (1E9 / T3) + 1.9018 * (1E6 / T2) + 0.24748 * (1E3 / T) + 0.237040;
    end
    else
    begin
      //cmsSignalError(LCMS_ERRC_ABORTED, "cmsWhitePointFromTemp: invalid temp");
      result := FALSE;
      exit;
    end;

  y := -3.000 * (x * x) + 2.870 * x - 0.275;

  WhitePoint^.x := x;
  WhitePoint^.y_mi := y;
  WhitePoint^.Y_ma := 1.0;

  result := TRUE;
end;

function IEcmsWhitePointFromTemp(TempK: integer; var WhitePoint_x, WhitePoint_y, WhitePoint_Y_: double): boolean;
var
  xyY: cmsCIExyY;
begin
  result := cmsWhitePointFromTemp(tempK, @xyY);
  WhitePoint_x := xyY.x;
  WhitePoint_y := xyY.y_mi;
  WhitePoint_Y_ := xyY.Y_ma;
end;

function Build_sRGBGamma: LPGAMMATABLE;
var
  Parameters: array[0..4] of double;
begin
  Parameters[0] := 2.4;
  Parameters[1] := 1. / 1.055;
  Parameters[2] := 0.055 / 1.055;
  Parameters[3] := 1. / 12.92;
  Parameters[4] := 0.04045;

  result := cmsBuildParametricGamma(1024, 4, @Parameters);
end;

function IEcmsCreate_sRGBProfile: cmsHPROFILE;
const
  Rec709Primaries: cmsCIExyYTRIPLE = (
    Red: (x: 0.6400; y_mi: 0.3300; Y_ma: 1.0);
    Green: (x: 0.3000; y_mi: 0.6000; Y_ma: 1.0);
    Blue: (x: 0.1500; y_mi: 0.0600; Y_ma: 1.0)
    );

var
  D65: cmsCIExyY;
  Gamma22: array[0..2] of LPGAMMATABLE;
  hsRGB: cmsHPROFILE;

begin

  cmsWhitePointFromTemp(6504, @D65);
  Gamma22[2] := Build_sRGBGamma();
  Gamma22[1] := Gamma22[2];
  Gamma22[0] := Gamma22[1];

  hsRGB := cmsCreateRGBProfile(@D65, @Rec709Primaries, @Gamma22);
  cmsFreeGamma(Gamma22[0]);

  cmsAddTag(hsRGB, icSigDeviceMfgDescTag, PAnsiChar('(lcms internal)'));
  cmsAddTag(hsRGB, icSigDeviceModelDescTag, PAnsiChar('sRGB built-in'));
  cmsAddTag(hsRGB, icSigProfileDescriptionTag, PAnsiChar('sRGB built-in'));

  result := hsRGB;
end;

function IEcmsCreateXYZProfile: cmsHPROFILE;
var
  hProfile: cmsHPROFILE;
  Lut: LPLUT;
begin

  hProfile := cmsCreateRGBProfile(cmsD50_xyY, nil, nil);

  cmsSetDeviceClass(hProfile, icSigAbstractClass);
  cmsSetColorSpace(hProfile, icSigXYZData);
  cmsSetPCS(hProfile, icSigXYZData);

  cmsAddTag(hProfile, icSigDeviceMfgDescTag, PAnsiChar('(lcms internal)'));
  cmsAddTag(hProfile, icSigProfileDescriptionTag, PAnsiChar('lcms XYZ identity'));
  cmsAddTag(hProfile, icSigDeviceModelDescTag, PAnsiChar('XYZ built-in'));

  Lut := Create3x3EmptyLUT;
  if (Lut = nil) then
  begin
    result := nil;
    exit;
  end;

  cmsAddTag(hProfile, icSigAToB0Tag, Lut);
  cmsAddTag(hProfile, icSigBToA0Tag, Lut);
  cmsAddTag(hProfile, icSigPreview0Tag, Lut);

  cmsFreeLUT(Lut);

  result := hProfile;
end;

{$ELSE} // IEINCLUDECMS

implementation

{$ENDIF}

end.
