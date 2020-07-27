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
File version 1001
*)

unit stdquant;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


{$R-}
{$Q-}

{$I ie.inc}

interface

uses Windows, Graphics, classes, sysutils, imageen, ImageEnProc, hyiedefs;

function CreateMedianCutQuantizer(SrcBitmap: TObject; var ColorMap: array of TRGB; NCol: integer): pointer;
procedure FreeMedianCutQuantizer(mq: pointer);
function MedianCutFindIndex(mq: pointer; const rgb: TRGB): integer;

implementation

uses hyieutils;

{$R-}

const
  ERR_CANCEL = 128;
  ERR_NOMEMORY = 139;
  RedI = 0;
  GreenI = 1;
  BlueI = 2;
  Bits = 5;
  cBits = 8 - Bits;
  ColorMaxI = 1 shl Bits;
  cHistogramm = ColorMaxI * ColorMaxI * ColorMaxI;

type

  TTrueColor = record
    Blue: byte;
    Green: byte;
    Red: Byte;
  end;

  PRGBByteArray = ^TRGBByteArray;
  TRGBByteArray = array[0..32767] of TTrueColor;

  tDMCoSi = record
    pqBMI: TObject;
    cqWid: word;
    cqHei: word;
    czWid: word;
    czHei: word;
  end;

  tMean = array[RedI..BlueI] of double;
  tFreqZeile = array[0..ColorMaxI - 1] of longint;
  tFreqArray = array[RedI..BlueI] of tFreqZeile;
  tLowHigh = array[RedI..BlueI] of integer;
  pBox = ^tBox;
  tBox = record
    WeiVar: double;
    mean: tMean;
    weight: longint;
    Freq: tFreqArray;
    low: tLowHigh;
    high: tLowHigh;
  end;

  pBoxes = ^tBoxes;
  tBoxes = array[0..255] of tBox;
  pHistogramm = ^tHistogramm;
  tHistogramm = array[0..cHistogramm - 1] of longint;

  pRGBmap = ^tRGBmap;
  tRGBmap = array[0..cHistogramm - 1] of byte;

  PGVar = ^TGVar;
  TGVar = record
    pHisto: pHistogramm;
    pBoxArr: pBoxes;
    pMap: pRGBmap;
    cHBRPix: longint;
    cHBRCol: longint;
    cHBROutCol: longint;
    DMCoSi: tDMCoSi;
  end;

  /////////////////////////////////////////////////////////////////////////////

function Histogramm(gvar: PGVar): boolean;
var
  p24: PRGBByteArray;
  h: integer;
  r, g, b: byte;
  y, x: integer;
begin
  p24 := nil;
  with gvar^ do
  begin
    with pBoxArr^[0] do
    begin
      fillchar(Freq[RedI], sizeof(tFreqZeile), #0);
      fillchar(Freq[GreenI], sizeof(tFreqZeile), #0);
      fillchar(Freq[BlueI], sizeof(tFreqZeile), #0);
      for y := 0 to DMCoSi.czHei - 1 do
      begin
        if DMCoSi.pqBMi is TBitmap then
          p24 := (DMCoSi.pqBMi as TBitmap).Scanline[y]
        else
        if DMCoSi.pqBMi is TIEBitmap then
          p24 := (DMCoSi.pqBMi as TIEBitmap).Scanline[y];
        for x := 0 to DMCoSi.czWid - 1 do
        begin
          r := p24[x].Red shr cBits;
          inc(Freq[RedI, r]);
          g := p24[x].Green shr cBits;
          inc(Freq[GreenI, g]);
          b := p24[x].Blue shr cBits;
          inc(Freq[BlueI, b]);
          h := r shl Bits;
          h := (h or g) shl Bits;
          h := h or b;
          inc(pHisto^[h]);
        end;
      end;
    end;
    Result := true;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

procedure BoxStats(gvar: pgvar; var pn: tBox);
var
  mean1, vari1: double;
  hw: double;
  i, col: integer;
begin
  with gvar^ do
  begin
    pn.WeiVar := 0.0;
    if (pn.Weight = 0) then
      exit;
    for col := RedI to BlueI do
    begin
      vari1 := 0.0;
      mean1 := 0.0;
      for i := pn.Low[col] to pn.High[col] - 1 do
      begin
        hw := pn.Freq[col, i];
        hw := hw * i;
        mean1 := mean1 + hw;
        hw := hw * i;
        vari1 := vari1 + hw;
      end;
      pn.Mean[col] := mean1 / pn.Weight;
      hw := pn.mean[col];
      hw := hw * hw * pn.Weight;
      hw := vari1 - hw;
      pn.WeiVar := pn.WeiVar + hw;
    end;
    pn.WeiVar := pn.WeiVar / cHBRPix;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

function FindCutPoint(gvar: pgvar; var pn, nBox1, nBox2: tBox; RGB: byte): boolean;
var
  u, v, max: double;
  hw: double;
  OptWei: longint;
  CurWei: longint;
  myfreq: longint;
  h: integer;
  rOff, gOff: integer;
  i, CutPt: integer;
  maxIdx, minIdx: integer;
  l1, l2, h1, h2: integer;
  b, g, r: byte;
begin
  with gvar^ do
  begin
    Result := false;
    if (pn.Low[RGB] + 1 = pn.High[RGB]) then
      exit;
    MinIdx := round((pn.Mean[RGB] + pn.Low[RGB]) * 0.5);
    MaxIdx := round((pn.Mean[RGB] + pn.High[RGB]) * 0.5);
    CutPt := MinIdx;
    OptWei := pn.Weight;
    CurWei := 0;
    for i := pn.Low[RGB] to MinIdx - 1 do
      CurWei := CurWei + longint(pn.Freq[RGB, i]);
    u := 0.0;
    Max := -1.0;
    for i := MinIdx to MaxIdx do
    begin
      inc(CurWei, pn.Freq[RGB, i]);
      if (CurWei = pn.Weight) then
        break;
      hw := i;
      hw := (hw * pn.Freq[RGB, i]) / pn.Weight;
      u := u + hw;
      hw := pn.Mean[RGB];
      hw := hw - u;
      hw := hw * hw;
      v := CurWei;
      v := (v / (pn.Weight - CurWei)) * hw;
      if (v > max) then
      begin
        max := v;
        CutPt := i;
        OptWei := CurWei;
      end;
    end;
    inc(CutPt);
    Move(pn, nBox1, sizeof(tBox));
    Move(pn, nBox2, sizeof(tBox));
    nBox1.Weight := OptWei;
    nBox2.Weight := nBox2.Weight - OptWei;
    if (nBox1.Weight = 0) or (nBox2.Weight = 0) then
    begin
      exit;
    end;
    nBox1.High[RGB] := CutPt;
    nBox2.Low[RGB] := CutPt;
    fillchar(nBox1.Freq[RedI], sizeof(tFreqZeile), #0);
    fillchar(nBox1.Freq[GreenI], sizeof(tFreqZeile), #0);
    fillchar(nBox1.Freq[BlueI], sizeof(tFreqZeile), #0);
    for r := nBox1.Low[RedI] to nBox1.High[RedI] - 1 do
    begin
      rOff := r shl Bits;
      for g := nBox1.Low[GreenI] to nBox1.High[GreenI] - 1 do
      begin
        gOff := (rOff or g) shl Bits;
        for b := nBox1.Low[BlueI] to nBox1.High[BlueI] - 1 do
        begin
          h := gOff or b;
          myfreq := pHisto^[h];
          if (myfreq <> 0) then
          begin
            inc(nBox1.Freq[RedI, r], myfreq);
            inc(nBox1.Freq[GreenI, g], myfreq);
            inc(nBox1.Freq[BlueI, b], myfreq);
            dec(nBox2.Freq[RedI, r], myfreq);
            dec(nBox2.Freq[GreenI, g], myfreq);
            dec(nBox2.Freq[BlueI, b], myfreq);
          end;
        end;
      end;
    end;
    for r := RedI to BlueI do
    begin
      l1 := ColorMaxI;
      l2 := ColorMaxI;
      h1 := 0;
      h2 := 0;
      for g := 0 to ColorMaxI - 1 do
      begin
        if (nBox1.Freq[r, g] <> 0) then
        begin
          if (g < l1) then
            l1 := g;
          if (g > h1) then
            h1 := g;
        end;
        if (nBox2.Freq[r, g] <> 0) then
        begin
          if (g < l2) then
            l2 := g;
          if (g > h2) then
            h2 := g;
        end;
      end;
      nBox1.Low[r] := l1;
      nBox2.Low[r] := l2;
      nBox1.High[r] := h1 + 1;
      nBox2.High[r] := h2 + 1;
    end;
    BoxStats(gvar, nBox1);
    BoxStats(gvar, nBox2);
    Result := true;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

function CutBox(gvar: pgvar; var pn, nBox1: tBox): boolean;
const
  Hugo = 1.7 * 10308;
var
  i: integer;
  TotVar: array[RedI..BlueI] of double;
  nBoxes: array[RedI..BlueI, 0..1] of tBox;
begin
  with gvar^ do
  begin
    if (pn.WeiVar = 0.0) or (pn.Weight = 0) then
    begin
      pn.WeiVar := 0.0;
      Result := false;
      exit;
    end
    else
      Result := true;
    for i := RedI to BlueI do
    begin
      if (FindCutPoint(gvar, pn, nBoxes[i, 0], nBoxes[i, 1], i)) then
        TotVar[i] := nBoxes[i, 0].WeiVar + nBoxes[i, 1].WeiVar
      else
        TotVar[i] := Hugo;
    end;
    if (TotVar[RedI] < Hugo)
      and (TotVar[RedI] <= TotVar[GreenI])
      and (TotVar[RedI] <= TotVar[BlueI]) then
    begin
      Move((nBoxes[RedI, 0]), pn, sizeof(tBox));
      Move((nBoxes[RedI, 1]), nBox1, sizeof(tBox));
      exit;
    end
    else
    if (TotVar[GreenI] < Hugo)
      and (TotVar[GreenI] <= TotVar[RedI])
      and (TotVar[GreenI] <= TotVar[BlueI]) then
    begin
      Move((nBoxes[GreenI, 0]), pn, sizeof(tBox));
      Move((nBoxes[GreenI, 1]), nBox1, sizeof(tBox));
      exit;
    end
    else
    if (TotVar[BlueI] < Hugo) then
    begin
      Move((nBoxes[BlueI, 0]), pn, sizeof(tBox));
      Move((nBoxes[BlueI, 1]), nBox1, sizeof(tBox));
      exit;
    end;
    pn.WeiVar := 0.0;
    Result := false;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

function CutBoxes(gvar: pgvar): integer;
var
  CurBox, n, i: integer;
  Max: double;
begin
  with gvar^ do
  begin
    with pBoxArr^[0] do
    begin
      Low[RedI] := 0;
      Low[GreenI] := 0;
      Low[BlueI] := 0;
      High[RedI] := ColorMaxI;
      High[GreenI] := ColorMaxI;
      High[BlueI] := ColorMaxI;
      Weight := cHBRPix;
    end;
    BoxStats(gvar, pBoxArr^[0]);
    CurBox := 1;
    while (CurBox < cHBRCol) do
    begin
      n := CurBox;
      max := 0.0;
      for i := 0 to CurBox - 1 do
        with pBoxArr^[i] do
        begin
          if (WeiVar > Max) then
          begin
            Max := WeiVar;
            n := i;
          end;
        end;
      if (n = CurBox) then
        break;
      if (CutBox(gvar, pBoxArr^[n], pBoxArr^[CurBox])) then
        inc(CurBox);
    end;
    Result := CurBox;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

function MakeRGBmap(gvar: pgvar): boolean;
var
  i, p: integer;
  r, g, b: integer;
  rOff, gOff: integer;
begin
  with gvar^ do
  begin
    for i := 0 to cHBROutCol - 1 do
      with pBoxArr^[i] do
      begin
        for r := Low[RedI] to High[RedI] - 1 do
        begin
          rOff := r shl Bits;
          for g := Low[GreenI] to High[GreenI] - 1 do
          begin
            gOff := (rOff or g) shl Bits;
            for b := Low[BlueI] to High[BlueI] - 1 do
            begin
              p := gOff or b;
              pMap^[p] := i;
            end;
          end;
        end;
      end;
    Result := true;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

function GetHBRmem(gvar: pgvar): boolean;
begin
  with gvar^ do
  begin
    GetMem(pHisto, sizeof(tHistogramm));
    GetMem(pBoxArr, sizeof(tBoxes));
    GetMem(pMap, sizeof(tRGBmap));
    fillchar(pHisto^, sizeof(tHistogramm), #0);
    fillchar(pBoxArr^, sizeof(tBoxes), #0);
    fillchar(pMap^, sizeof(tRGBmap), #0);
    Result := true;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

procedure FreeHBRmem(gvar: pgvar);
begin
  with gvar^ do
  begin
    if (pHisto <> nil) then
    begin
      FreeMem(pHisto);
      pHisto := nil;
    end;
    if (pBoxArr <> nil) then
    begin
      FreeMem(pBoxArr);
      pBoxArr := nil;
    end;
    if (pMap <> nil) then
    begin
      FreeMem(pMap);
      pMap := nil;
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////
// SrcBitmap can be TIEBitmap or TBitmap

function CreateMedianCutQuantizer(SrcBitmap: TObject; var ColorMap: array of TRGB; NCol: integer): pointer;
var
  gvar: PGVar;
  i: integer;
begin
  getmem(gvar, sizeof(TGVar));
  with gvar^ do
  begin
    fillchar(DMCoSi, sizeof(tDMCoSi), #0);
    pHisto := nil;
    pBoxArr := nil;
    pMap := nil;
    with DMCoSi do
    begin
      pqBMI := SrcBitmap;
      if pqBMi is TBitmap then
      begin
        with pqBMI as TBitmap do
        begin
          czWid := Width;
          czHei := Height;
        end;
      end
      else
      if pqBMi is TIEBitmap then
      begin
        with pqBMI as TIEBitmap do
        begin
          czWid := Width;
          czHei := Height;
        end;
      end;
      cHBRPix := czWid;
      cHBRPix := cHBRPix * czHei;
      cHBRCol := ncol;
    end;
    GetHBRmem(gvar);
    Histogramm(gvar);
    cHBROutCol := CutBoxes(gvar);
    for i := 0 to cHBROutCol - 1 do
      with pBoxArr^[i] do
      begin
        ColorMap[i].r := round(Mean[RedI]) shl cBits;
        ColorMap[i].g := round(Mean[GreenI]) shl cBits;
        ColorMap[i].b := round(Mean[BlueI]) shl cBits;
      end;
  end;
  MakeRGBmap(gvar);
  result := gvar;
end;

/////////////////////////////////////////////////////////////////////////////

procedure FreeMedianCutQuantizer(mq: pointer);
begin
  with PGVar(mq)^ do
    FreeHBRmem(PGVar(mq));
  freemem(mq);
end;

/////////////////////////////////////////////////////////////////////////////

function MedianCutFindIndex(mq: pointer; const rgb: TRGB): integer;
var
  b, g, r, p: integer;
begin
  with PGVar(mq)^ do
  begin
    r := (rgb.r and $F8) shl (Bits + Bits - cBits);
    g := (rgb.g and $F8) shl (Bits - cBits);
    b := (rgb.b and $F8) shr cBits;
    p := r or g or b;
    result := pmap^[p];
  end;
end;

end.
