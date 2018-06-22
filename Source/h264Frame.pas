{ ****************************************************************************** }
{ * h264Frame.pas        by qq600585                                           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit h264Frame;

{$I zDefine.inc}
{$POINTERMATH ON}

interface

uses
  sysutils, h264Stdint, h264stats, h264common, h264image, h264pixel, h264util, CoreClasses;

const
  FRAME_PADDING_W = 16;
  FRAME_EDGE_W    = FRAME_PADDING_W div 2;

type
  TFrameManager = class
  private
    listL0: array of TFrame;
    ifree: int32_t;
    procedure GetRef(out f: PFrame; const frame_num: int32_t);
  public
    procedure InsertRef(var f: TFrame);
    procedure SetRefs(var f: TFrame; const frame_num, nrefs: int32_t);
    procedure GetFree(var f: TFrame);
    constructor Create(const ref_count, mb_w, mb_h: int32_t);
    destructor destroy; override;
  end;

procedure frame_new(var frame: TFrame; const mb_width, mb_height: int32_t);
procedure frame_free(var frame: TFrame);

procedure frame_img2frame_copy(var frame: TFrame; const img: TPlanarImage);
procedure frame_paint_edges(var frame: TFrame);
procedure frame_hpel_interpolate(var frame: TFrame);
procedure frame_init;

implementation

procedure TFrameManager.GetRef(out f: PFrame; const frame_num: int32_t);
var
  i: int32_t;
begin
  for i := 0 to Length(listL0) - 1 do
    if listL0[i].num = frame_num then
      begin
        f := @listL0[i];
        exit;
      end;
  RaiseInfo('GetRef - ref not found! %d', [frame_num]);
end;

// insert ref. frame to oldest slot, mark oldest taken slot as free
procedure TFrameManager.InsertRef(var f: TFrame);
var
  i, oldest: int32_t;
begin
  oldest := MaxInt;
  for i := 0 to Length(listL0) - 1 do
    if listL0[i].num < oldest then
      begin
        ifree := i;
        oldest := listL0[i].num;
      end;
  listL0[ifree] := f;

  oldest := MaxInt;
  for i := 0 to Length(listL0) - 1 do
    if listL0[i].num < oldest then
      begin
        ifree := i;
        oldest := listL0[i].num;
      end;
end;

procedure TFrameManager.SetRefs(var f: TFrame; const frame_num, nrefs: int32_t);
var
  i: int32_t;
  t: PFrame;
begin
  for i := 1 to nrefs do
    begin
      GetRef(t, frame_num - i);
      f.refs[i - 1] := t;
    end;
end;

procedure TFrameManager.GetFree(var f: TFrame);
begin
  if ifree = -1 then
      RaiseInfo('GetFree - no free frame!');
  f := listL0[ifree];
  ifree := -1;
end;

constructor TFrameManager.Create(const ref_count, mb_w, mb_h: int32_t);
var
  i: int32_t;
begin
  inherited Create;
  ifree := 0;
  SetLength(listL0, ref_count + 1);
  for i := 0 to ref_count do
    begin
      frame_new(listL0[i], mb_w, mb_h);
      listL0[i].num := -1;
    end;
end;

destructor TFrameManager.destroy;
var
  i: int32_t;
begin
  for i := 0 to Length(listL0) - 1 do
      frame_free(listL0[i]);
  listL0 := nil;
  inherited destroy;
end;

procedure frame_new(var frame: TFrame; const mb_width, mb_height: int32_t);
var
  padded_height, padded_width,
    frame_mem_offset, frame_mem_offset_cr: int32_t; // frame memory to image data start offset
  pfsize, pfsize_cr: int32_t;                       // padded frame luma / chroma plane size
  i: int32_t;
begin
  with frame do
    begin
      mbw := mb_width;
      mbh := mb_height;
      w := mbw * 16;
      h := mbh * 16;
      w_cr := w div 2;
      h_cr := h div 2;
      padded_width := w + FRAME_PADDING_W * 2;
      padded_height := h + FRAME_PADDING_W * 2;
    end;

  frame.pw := padded_width;
  frame.ph := padded_height;
  frame.stride := padded_width;
  frame.stride_c := padded_width div 2;

  pfsize := padded_width * padded_height;
  pfsize_cr := pfsize div 4;
  frame_mem_offset := FRAME_PADDING_W * padded_width + FRAME_PADDING_W;
  frame_mem_offset_cr := FRAME_PADDING_W * padded_width div 4 + FRAME_PADDING_W div 2;

  frame.mbs := fev_malloc(mb_width * mb_height * sizeof(TMacroblock));
  frame.aq_table := fev_malloc(mb_width * mb_height);
  frame.qp := 0;
  frame.qp_avg := 0;
  frame.num := 0;

  frame.frame_mem_offset := frame_mem_offset;
  frame.frame_mem_offset_cr := frame_mem_offset_cr;

  // luma plane
  frame.mem[0] := fev_malloc(pfsize + pfsize_cr * 2);
  frame.plane[0] := frame.mem[0] + frame_mem_offset;
  // chroma planes
  frame.mem[1] := frame.mem[0] + pfsize;
  frame.mem[2] := frame.mem[1] + pfsize_cr;
  frame.plane[1] := frame.mem[1] + frame_mem_offset_cr;
  frame.plane[2] := frame.mem[2] + frame_mem_offset_cr;
  // decoded planes + interpolated planes
  frame.mem[3] := fev_malloc(pfsize * 4 + pfsize_cr * 2);
  frame.plane_dec[0] := frame.mem[3] + frame_mem_offset;
  for i := 0 to 3 do
      frame.luma_mc[i] := frame.plane_dec[0] + pfsize * i;
  for i := 0 to 3 do
      frame.luma_mc_qpel[i] := frame.luma_mc[i];
  frame.luma_mc_qpel[4] := frame.luma_mc[0] + 1;
  frame.luma_mc_qpel[5] := frame.luma_mc[2] + 1;
  frame.luma_mc_qpel[6] := frame.luma_mc[0] + padded_width;
  frame.luma_mc_qpel[7] := frame.luma_mc[1] + padded_width;

  frame.mem[4] := frame.mem[3] + pfsize * 4;
  frame.mem[5] := frame.mem[4] + pfsize_cr;
  frame.plane_dec[1] := frame.mem[4] + frame_mem_offset_cr;
  frame.plane_dec[2] := frame.mem[5] + frame_mem_offset_cr;

  // 4x4 block offsets
  with frame do
    begin
      blk_offset[0] := 0;
      blk_offset[1] := 4;
      blk_offset[2] := 0 + 4 * stride;
      blk_offset[3] := 4 + 4 * stride;

      blk_offset[4] := 8;
      blk_offset[5] := 12;
      blk_offset[6] := 8 + 4 * stride;
      blk_offset[7] := 12 + 4 * stride;

      blk_offset[8] := 0 + 8 * stride;
      blk_offset[9] := 4 + 8 * stride;
      blk_offset[10] := 0 + 12 * stride;
      blk_offset[11] := 4 + 12 * stride;

      blk_offset[12] := 8 + 8 * stride;
      blk_offset[13] := 12 + 8 * stride;
      blk_offset[14] := 8 + 12 * stride;
      blk_offset[15] := 12 + 12 * stride;

      blk_chroma_offset[0] := 0;
      blk_chroma_offset[1] := 4;
      blk_chroma_offset[2] := 0 + 4 * stride_c;
      blk_chroma_offset[3] := 4 + 4 * stride_c;
    end;

  // other
  frame.filter_hv_temp := fev_malloc(padded_width * 2);
  frame.bs_buf := fev_malloc(frame.w * frame.h * 3);

  frame.stats := TFrameStats.Create;
end;

procedure frame_free(var frame: TFrame);
begin
  fev_free(frame.mbs);
  fev_free(frame.aq_table);
  fev_free(frame.mem[0]);
  fev_free(frame.mem[3]);
  fev_free(frame.filter_hv_temp);
  fev_free(frame.bs_buf);
  frame.stats.Free;

  frame.plane[0] := nil;
  frame.plane[1] := nil;
  frame.plane[2] := nil;
  frame.filter_hv_temp := nil;

  frame.stride := 0;
  frame.stride_c := 0;

  frame.w := 0;
  frame.h := 0;
end;

procedure frame_swap(var a, b: TFrame);
var
  t: TFrame;
begin
  t := a;
  a := b;
  b := t;
end;

(* ******************************************************************************
  frame_setup_adapt_q
  adjust mb quant according to variance
*)
procedure frame_setup_adapt_q(var frame: TFrame; pixbuffer: uint8_p; const base_qp: uint8_t);
const
  QP_RANGE  = 10;
  QP_MIN    = 15;
  QP_MAX    = 51;
  VAR_SHIFT = 14;

var
  x, y: int32_t;
  vari: int32_t;
  qp: int32_t;
  pfenc, pix: uint8_p;
  stride, avg: int32_t;

begin
  stride := frame.mbw;
  pix := pixbuffer;

  // derive qp from variance
  avg := 0;
  for y := 0 to (frame.mbh - 1) do
    begin
      pfenc := frame.plane[0] + y * 16 * frame.stride;

      for x := 0 to (frame.mbw - 1) do
        begin
          pixel_load_16x16(pix, pfenc, frame.stride);
          vari := dsp.var_16x16(pix);
          inc(pfenc, 16);
          qp := base_qp - QP_RANGE;
          qp := clip3(QP_MIN, qp + min(vari shr VAR_SHIFT, QP_RANGE * 2), QP_MAX);
          inc(avg, qp);
          frame.aq_table[y * stride + x] := qp;
        end;
    end;

  frame.aq_table[0] := base_qp;
  frame.qp_avg := avg / (frame.mbw * frame.mbh);
end;

{ skopirovanie dat do zarovnanej oblasti pamate spolu s vyplnou na okrajoch
}
procedure paint_edge_vert(src, dst: uint8_p; stride, h: int32_t; const edge_width: uint8_t);
var
  i, j: int32_t;
begin
  for i := 0 to h - 1 do
    begin
      for j := 0 to edge_width - 1 do
          dst[j] := src^;
      inc(dst, stride);
      inc(src, stride);
    end;
end;

procedure paint_edge_horiz(src, dst: uint8_p; stride: int32_t; const edge_width: uint8_t);
var
  i: int32_t;
begin
  for i := 0 to edge_width - 1 do
    begin
      CopyPtr(src, dst, stride);
      inc(dst, stride);
    end;
end;

procedure frame_img2frame_copy(var frame: TFrame; const img: TPlanarImage);
var
  w, h, i, j: int32_t;
  dstride, sstride, edge_width: int32_t;
  s, d: uint8_p;
begin
  w := img.Width;
  h := img.Height;
  // y
  dstride := frame.stride;
  sstride := img.stride;
  d := frame.plane[0];
  s := img.plane[0];
  for i := 0 to h - 1 do
    begin
      CopyPtr(s, d, w);
      inc(s, sstride);
      inc(d, dstride);
    end;
  // u/v
  dstride := frame.stride_c;
  sstride := img.stride_c;
  for j := 1 to 2 do
    begin
      d := frame.plane[j];
      s := img.plane[j];
      for i := 0 to h div 2 - 1 do
        begin
          CopyPtr(s, d, w div 2);
          inc(s, sstride);
          inc(d, dstride);
        end;
    end;

  // fill non-mod16 edges
  if (w and $F) > 0 then
    begin
      edge_width := 16 - (img.Width and $F);
      paint_edge_vert(frame.plane[0] + w - 1, frame.plane[0] + w, frame.stride, frame.h, edge_width);
      for i := 1 to 2 do
          paint_edge_vert(frame.plane[i] + w div 2 - 1, frame.plane[i] + w div 2, frame.stride_c, frame.h_cr, edge_width div 2);
    end;
  if (h and $F) > 0 then
    begin
      edge_width := 16 - (img.Height and $F);
      paint_edge_horiz(frame.plane[0] - 16 + frame.stride * (h - 1), frame.plane[0] - 16 + frame.stride * h, frame.stride, edge_width);
      for i := 1 to 2 do
          paint_edge_horiz(frame.plane[i] - 8 + frame.stride_c * (h div 2 - 1), frame.plane[i] - 8 + frame.stride_c * h div 2, frame.stride_c, edge_width);
    end;
end;

(* ******************************************************************************
  fill plane edges with edge pixel's copies
*)
procedure plane_paint_edges(const p: uint8_p; const w, h, stride, edge_width: int32_t);
begin
  // left/right/up/down
  paint_edge_vert(p, p - edge_width, stride, h, edge_width);
  paint_edge_vert(p + w - 1, p + w, stride, h, edge_width);
  paint_edge_horiz(p - edge_width, (p - edge_width) - edge_width * stride, stride, edge_width);
  paint_edge_horiz(p - edge_width + stride * (h - 1), p - edge_width + stride * h, stride, edge_width);
end;

procedure frame_paint_edges(var frame: TFrame);
var
  i: int32_t;
begin
  plane_paint_edges(frame.plane_dec[0], frame.w, frame.h, frame.stride, 16);
  for i := 1 to 2 do
      plane_paint_edges(frame.plane_dec[i], frame.w_cr, frame.h_cr, frame.stride_c, 8);
end;

(* ******************************************************************************
  h.264 6tap hpel filter
*)
procedure frame_hpel_interpolate(var frame: TFrame);
var
  Width, Height: int32_t;
  stride: int32_t;
  src: uint8_p;
  dst: array [0 .. 2] of uint8_p;
  row: array [-2 .. 3] of uint8_p;
  x, y, i, j: int32_t;
  t: array [-2 .. 3] of int32_t;
  edge_offset: int32_t;

  function clip(i: int32_t): uint8_t; inline;
  begin
    if i > 255 then
        i := 255
    else if i < 0 then
        i := 0;
    result := uint8_t(i);
  end;

begin
  Width := frame.w;
  Height := frame.h;
  stride := frame.stride;
  edge_offset := FRAME_EDGE_W + FRAME_EDGE_W * stride;
  src := frame.plane_dec[0] - edge_offset;
  for i := 0 to 2 do
      dst[i] := frame.luma_mc[i + 1] - edge_offset;

  // horizontal
  for y := 0 to Height - 1 + FRAME_EDGE_W * 2 do
    begin
      for x := 0 to Width - 1 + FRAME_EDGE_W * 2 do
        begin
          i := src[-2 + x] - 5 * src[-1 + x] + 20 * src[0 + x] + 20 * src[1 + x] - 5 * src[2 + x] + src[3 + x] + 16;
          if i < 0 then
              i := 0;
          dst[0][x] := clip(i shr 5);
        end;
      inc(src, stride);
      inc(dst[0], stride);
    end;

  // vertical + hv
  src := frame.plane_dec[0] - edge_offset;
  for i := -2 to 3 do
      row[i] := src + i * stride;

  for y := 0 to Height - 1 + FRAME_EDGE_W * 2 do
    begin
      // fill temps first
      x := -1;
      for j := x - 2 to x + 3 do
          t[j - x] := row[-2, j] - 5 * row[-1, j] + 20 * row[0, j] + 20 * row[1, j] - 5 * row[2, j] + row[3, j];

      for x := 0 to Width - 1 + FRAME_EDGE_W * 2 do
        begin
          // reuse coefs from last run
          for j := -1 to 3 do
              t[j - 1] := t[j];
          // get new vertical intermed
          j := x + 3;
          t[3] := row[-2, j] - 5 * row[-1, j] + 20 * row[0, j] + 20 * row[1, j] - 5 * row[2, j] + row[3, j];
          // vert
          i := t[0] + 16;
          if i < 0 then
              i := 0;
          dst[1][y * stride + x] := clip(i shr 5);
          // vert + horiz
          i := t[-2] - 5 * t[-1] + 20 * t[0] + 20 * t[1] - 5 * t[2] + t[3] + 512;
          if i < 0 then
              i := 0;
          dst[2][y * stride + x] := clip(i shr 10);
        end;

      for i := -2 to 3 do
          inc(row[i], stride);
    end;
end;

(* ******************************************************************************
  frame_init
*)
procedure frame_init;
begin
end;

end.
