{ ****************************************************************************** }
{ * h264Transquant.pas        by qq600585                                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit h264Transquant;

{$I zDefine.inc}
{$POINTERMATH ON}

interface

uses h264Stdint, CoreClasses;

procedure transqt(block: int16_p; const qp: uint8_t; const intra: boolean; const quant_start_idx: uint8_t = 0);
procedure itransqt(block: int16_p; const qp: uint8_t; const quant_start_idx: uint8_t = 0);

procedure transqt_dc_2x2(block: int16_p; const qp: uint8_t);
procedure itransqt_dc_2x2(block: int16_p; const qp: uint8_t);

procedure transqt_dc_4x4(block: int16_p; const qp: uint8_t);
procedure itransqt_dc_4x4(block: int16_p; const qp: uint8_t);

procedure itrans_dc(block: int16_p);

implementation


const
  table_qp_div6: array [0 .. 51] of uint8_t =
    (0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8);
  table_qp_mod6: array [0 .. 51] of uint8_t =
    (0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3);

  { E matrix - scaling factors:
    a^2 = 1
    ab/2 = 2
    b^2/4 = 3
  }
  coef_idx: array [0 .. 15] of uint8_t = (
    1, 2, 1, 2,
    2, 3, 2, 3,
    1, 2, 1, 2,
    2, 3, 2, 3
    );

  // V = Qstep*PF*64 rescaling factor
  // -> LevelScale (8-252)
  table_v_coefs: array [0 .. 5, 1 .. 3] of uint8_t = (
    (10, 13, 16), // 0
    (11, 14, 18),
    (13, 16, 20),
    (14, 18, 23),
    (16, 20, 25),
    (18, 23, 29) // 5
    );

  // (PF/Qstep) mult. factor
  // 1, 2, 3
  // -> LevelScale2 (8-293)
  table_mf_coefs: array [0 .. 5, 1 .. 3] of int16_t = (
    (13107, 8066, 5243), // 0
    (11916, 7490, 4660),
    (10082, 6554, 4194),
    (9362, 5825, 3647),
    (8192, 5243, 3355),
    (7282, 4559, 2893) // 5
    );

type
  matrix_t    = array [0 .. 3, 0 .. 3] of int16_t;
  dc_matrix_t = array [0 .. 1, 0 .. 1] of int16_t;
  dc_matrix_p = ^dc_matrix_t;

var
  resc_factor,
    mult_factor: array [0 .. 5, 0 .. 15] of int16_t;

procedure init_tables;
var
  i, j: uint8_t;
begin
  for i := 0 to 5 do
    begin
      for j := 0 to 15 do
        begin
          mult_factor[i][j] := table_mf_coefs[i, coef_idx[j]];
          resc_factor[i][j] := table_v_coefs[i, coef_idx[j]];
        end;
    end;
end;

// Z = (|W| . MF + f) >> qbits
procedure quant(a: int16_p; const qp: uint8_t; const intra: boolean; const sidx: uint8_t);
var
  i: int32_t;
  f: int32_t;
  qbits: uint8_t;
  mf: int16_p;
begin
  // multiply shift
  qbits := 15 + table_qp_div6[qp];
  // multiply factor
  mf := @mult_factor[table_qp_mod6[qp]];
  // rounding factor
  if intra then
      f := (1 shl qbits) div 3
  else
      f := (1 shl qbits) div 6;

  inc(a, sidx);
  inc(mf, sidx);
  for i := sidx to 15 do
    begin
      if a^ > 0 then
          a^ := (a^ * mf^ + f) shr qbits
      else
          a^ := -((f - a^ * mf^) shr qbits); // fix from x264
      inc(a);
      inc(mf);
    end;
end;

procedure core_4x4(block: int16_p);
var
  m: matrix_t;
  e, f, g, h: array [0 .. 3] of int16_t;
  i: int32_t;
begin
  CopyPtr(block, @m, 16 * 2);

  { aaaa
    bbbb
    cccc
    dddd
  }
  for i := 0 to 3 do
    begin
      e[i] := m[0][i] + m[3][i]; // a + d
      f[i] := m[0][i] - m[3][i]; // a - d
      g[i] := m[1][i] + m[2][i]; // b + c
      h[i] := m[1][i] - m[2][i]; // b - c
    end;

  for i := 0 to 3 do
    begin
      m[0][i] := e[i] + g[i];     // a + b +  c +  d
      m[1][i] := 2 * f[i] + h[i]; // 2a + b -  c - 2d
      m[2][i] := e[i] - g[i];     // a - b -  c +  d
      m[3][i] := f[i] - h[i] * 2; // a -2b + 2c -  d
    end;

  { abcd
    abcd
    abcd
    abcd
  }
  for i := 0 to 3 do
    begin
      e[i] := m[i][0] + m[i][3];
      f[i] := m[i][0] - m[i][3];
      g[i] := m[i][1] + m[i][2];
      h[i] := m[i][1] - m[i][2];
    end;

  for i := 0 to 3 do
    begin
      m[i][0] := e[i] + g[i];
      m[i][1] := 2 * f[i] + h[i];
      m[i][2] := e[i] - g[i];
      m[i][3] := f[i] - h[i] * 2;
    end;

  CopyPtr(@m, block, 16 * 2);
end;

procedure transqt(block: int16_p; const qp: uint8_t; const intra: boolean; const quant_start_idx: uint8_t);
begin
  core_4x4(block);
  quant(block, qp, intra, quant_start_idx);
end;

(* ******************************************************************************
  iHCT + dequant
*)
procedure iquant(a: int16_p; const qp: uint8_t; const sidx: uint8_t);
var
  i: int32_t;
  shift: int32_t;
  mf: int16_p;
begin
  shift := table_qp_div6[qp];
  mf := @resc_factor[table_qp_mod6[qp]];
  inc(a, sidx);
  inc(mf, sidx);
  for i := sidx to 15 do
    begin
      a^ := a^ * mf^ shl shift;
      inc(a);
      inc(mf);
    end;
end;

procedure icore_4x4(block: int16_p);
var
  m: matrix_t;
  e, f, g, h: array [0 .. 3] of int16_t;
  i: int32_t;
begin
  CopyPtr(block, @m, 16 * 2);

  for i := 0 to 3 do
    begin
      e[i] := m[i][0] + m[i][2];
      f[i] := m[i][0] - m[i][2];
      g[i] := m[i][1] + Sar16(m[i][3], 1);
      h[i] := Sar16(m[i][1], 1) - m[i][3];
    end;
  for i := 0 to 3 do
    begin
      m[i][0] := e[i] + g[i];
      m[i][1] := f[i] + h[i];
      m[i][2] := f[i] - h[i];
      m[i][3] := e[i] - g[i];
    end;

  for i := 0 to 3 do
    begin
      e[i] := m[0][i] + m[2][i];
      f[i] := m[0][i] - m[2][i];
      g[i] := m[1][i] + Sar16(m[3][i], 1);
      h[i] := Sar16(m[1][i], 1) - m[3][i];
    end;
  for i := 0 to 3 do
    begin
      m[0][i] := Sar16(e[i] + g[i] + 32, 6); // rescaling
      m[1][i] := Sar16(f[i] + h[i] + 32, 6);
      m[2][i] := Sar16(f[i] - h[i] + 32, 6);
      m[3][i] := Sar16(e[i] - g[i] + 32, 6);
    end;

  CopyPtr(@m, block, 16 * 2);
end;

procedure itransqt(block: int16_p; const qp: uint8_t; const quant_start_idx: uint8_t = 0);
begin
  iquant(block, qp, quant_start_idx);
  icore_4x4(block);
end;

(* ******************************************************************************
  chroma DC
*)
procedure trans_dc_2x2(block: int16_p);
var
  m: dc_matrix_t;
  e, f, g, h: int32_t;
begin
  m := dc_matrix_p(block)^;
  e := m[0, 0] + m[1, 0];
  f := m[0, 0] - m[1, 0];
  g := m[0, 1] + m[1, 1];
  h := m[0, 1] - m[1, 1];
  m[0, 0] := e + g;
  m[0, 1] := e - g;
  m[1, 0] := f + h;
  m[1, 1] := f - h;
  dc_matrix_p(block)^ := m;
end;

procedure quant_dc_2x2(a: int16_p; const qp: uint8_t);
var
  i: int32_t;
  f: int32_t;
  qbits: uint8_t;
  mf: int16_t;
begin
  // multiply factor
  mf := mult_factor[table_qp_mod6[qp], 0];
  // multiply shift
  qbits := 16 + table_qp_div6[qp];
  f := 1 shl (qbits - 1);

  for i := 0 to 3 do
    if a[i] > 0 then
        a[i] := (a[i] * mf + f) shr qbits
    else
        a[i] := -((f - a[i] * mf) shr qbits);
end;

procedure iquant_dc_2x2(a: int16_p; const qp: uint8_t);
var
  i: int32_t;
  shift: int32_t;
  mf: int16_t;
begin
  shift := table_qp_div6[qp] - 1;
  mf := resc_factor[table_qp_mod6[qp], 0];
  if qp >= 6 then
    begin
      for i := 0 to 3 do
          a[i] := a[i] * mf shl shift;
    end
  else
    for i := 0 to 3 do
        a[i] := Sar16(a[i] * mf, 1);
end;

procedure transqt_dc_2x2(block: int16_p; const qp: uint8_t);
begin
  trans_dc_2x2(block);
  quant_dc_2x2(block, qp);
end;

procedure itransqt_dc_2x2(block: int16_p; const qp: uint8_t);
begin
  trans_dc_2x2(block);
  iquant_dc_2x2(block, qp);
end;

procedure itrans_dc(block: int16_p);
var
  dc: int16_t;
  i: int32_t;
begin
  dc := Sar16(block[0] + 32, 6);
  for i := 0 to 15 do
      block[i] := dc;
end;

(* ******************************************************************************
  luma DC 4x4
*)
procedure core_4x4_dc(block: int16_p);
var
  m: matrix_t;
  e, f, g, h: array [0 .. 3] of int16_t;
  i: int32_t;
begin
  CopyPtr(block, @m, 16 * 2);

  for i := 0 to 3 do
    begin
      e[i] := m[0][i] + m[3][i]; // a + d
      f[i] := m[0][i] - m[3][i]; // a - d
      g[i] := m[1][i] + m[2][i]; // b + c
      h[i] := m[1][i] - m[2][i]; // b - c
    end;

  for i := 0 to 3 do
    begin
      m[0][i] := e[i] + g[i]; // a + b + c + d
      m[1][i] := f[i] + h[i]; // a + b - c - d
      m[2][i] := e[i] - g[i]; // a - b - c + d
      m[3][i] := f[i] - h[i]; // a - b + c - d
    end;

  for i := 0 to 3 do
    begin
      e[i] := m[i][0] + m[i][3];
      f[i] := m[i][0] - m[i][3];
      g[i] := m[i][1] + m[i][2];
      h[i] := m[i][1] - m[i][2];
    end;

  for i := 0 to 3 do
    begin
      m[i][0] := e[i] + g[i];
      m[i][1] := f[i] + h[i];
      m[i][2] := e[i] - g[i];
      m[i][3] := f[i] - h[i];
    end;

  CopyPtr(@m, block, 16 * 2);
end;

procedure quant_dc_4x4(a: int16_p; const qp: uint8_t);
var
  i: int32_t;
  f: int32_t;
  qbits: uint8_t;
  mf: int16_p;
begin
  // scale by 2
  for i := 0 to 15 do
    if a[i] > 0 then
        a[i] := (a[i] + 1) div 2
    else
        a[i] := (a[i] - 1) div 2;

  // multiply factor
  mf := @mult_factor[table_qp_mod6[qp]];
  // multiply shift
  qbits := 16 + table_qp_div6[qp];
  f := 1 shl (qbits - 1);

  for i := 0 to 15 do
    if a[i] > 0 then
        a[i] := (a[i] * mf[0] + f) shr qbits
    else
        a[i] := -((f - a[i] * mf[0]) shr qbits);
end;

procedure iquant_dc_4x4(a: int16_p; const qp: uint8_t);
var
  i: int32_t;
  f, shift: int32_t;
  mf: int32_t;
begin
  mf := resc_factor[table_qp_mod6[qp], 0];

  if qp >= 12 then
    begin
      shift := table_qp_div6[qp] - 2;
      for i := 0 to 15 do
          a[i] := a[i] * mf shl shift;
    end
  else
    begin
      shift := 2 - table_qp_div6[qp];
      f := 1 shl (1 - table_qp_div6[qp]);
      for i := 0 to 15 do
          a[i] := Sar16(a[i] * mf + f, shift);
    end;
end;

procedure transqt_dc_4x4(block: int16_p; const qp: uint8_t);
begin
  core_4x4_dc(block);
  quant_dc_4x4(block, qp);
end;

procedure itransqt_dc_4x4(block: int16_p; const qp: uint8_t);
begin
  core_4x4_dc(block);
  iquant_dc_4x4(block, qp);
end;

initialization

init_tables;

end.
