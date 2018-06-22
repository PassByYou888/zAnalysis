{ ****************************************************************************** }
{ * h264Motion_est_search.pas        by qq600585                               * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit h264Motion_est_search;

{$I zDefine.inc}

interface

uses
  h264stdint, h264common, h264util, h264motion_comp, h264frame;

type
  { TRegionSearch }

  TRegionSearch = class
  private
    _max_x, _max_y: int32_t;
    _max_x_hpel, _max_y_hpel: int32_t;
    _max_x_qpel, _max_y_qpel: int32_t;
    _last_search_score: int32_t;
    _starting_fpel_mv: TMotionvec;
    MotionCompensator: TMotionCompensation;
    InterCostEval: IInterPredCostEvaluator;
  public
    cur: uint8_p;
    _mbx, _mby: int32_t;

    property LastSearchScore: int32_t read _last_search_score;

    constructor Create(region_width, region_height: int32_t; mc: TMotionCompensation; cost_eval: IInterPredCostEvaluator);
    procedure PickFPelStartingPoint(const fref: PFrame; const predicted_mv_list: TMotionVectorList);
    function SearchFPel(var mb: TMacroblock; const fref: PFrame): TMotionvec;
    function SearchHPel(var mb: TMacroblock; const fref: PFrame): TMotionvec;
    function SearchQPel(var mb: TMacroblock; const fref: PFrame; const satd, chroma_me: boolean): TMotionvec;
  end;

  (* ******************************************************************************
    ****************************************************************************** *)
implementation

type
  // motion search patterns
  TXYOffs      = array [0 .. 1] of int8_t;
  TMEPrecision = (mpFpel, mpHpel, mpQpel);

const
  FPEL_SAD_TRESH                                 = 64;
  ME_RANGES: array [TMEPrecision] of uint8_t     = (16, 4, 4);
  MIN_XY                                         = -FRAME_EDGE_W;
  MIN_XY_HPEL                                    = MIN_XY * 2;
  MIN_XY_QPEL                                    = MIN_XY * 4;
  pt_dia_small: array [0 .. 3] of TXYOffs        = ((0, -1), (0, 1), (-1, 0), (1, 0));
  pt_dia_large: array [0 .. 7] of TXYOffs        = ((0, -2), (0, 2), (-2, 0), (2, 0), (-1, -1), (-1, 1), (1, -1), (1, 1));
  pt_dia_large_sparse: array [0 .. 3] of TXYOffs = ((0, -2), (0, 2), (-2, 0), (2, 0));
  pt_square: array [0 .. 7] of TXYOffs           = ((0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1));

constructor TRegionSearch.Create(region_width, region_height: int32_t; mc: TMotionCompensation; cost_eval: IInterPredCostEvaluator);
var
  edge: int32_t;
begin
  MotionCompensator := mc;
  InterCostEval := cost_eval;

  _starting_fpel_mv := ZERO_MV;
  _last_search_score := MaxInt;

  // max. compensated mb position; we need to subtract the unpainted edge
  edge := FRAME_EDGE_W + 1;
  _max_x := region_width - edge;
  _max_y := region_height - edge;

  edge := FRAME_EDGE_W * 2 + 1;
  _max_x_hpel := region_width * 2 - edge;
  _max_y_hpel := region_height * 2 - edge;

  edge := FRAME_EDGE_W * 4 + 1;
  _max_x_qpel := region_width * 4 - edge;
  _max_y_qpel := region_height * 4 - edge;
end;

(*
  Pick a FPel mv that gives the lowest SAD score from a list of predictors.
  This mv will be the starting point for FPel search.
  input
  fref - frame being searched
  me - ME struct set up for fpel
  predicted_mv_list - list of predicted mvs in QPel units
*)
procedure TRegionSearch.PickFPelStartingPoint(const fref: PFrame; const predicted_mv_list: TMotionVectorList);
var
  i, x, y: int32_t;
  stride: int32_t;
  score: int32_t;
  ref: uint8_p;
  tested_mv: TMotionvec;

begin
  ref := fref^.plane_dec[0];
  stride := fref^.stride;

  // test 0,0
  _last_search_score := dsp.sad_16x16(cur, @ref[stride * _mby + _mbx], stride);
  _starting_fpel_mv := ZERO_MV;

  // test vectors
  for i := 0 to predicted_mv_list.Count - 1 do
    begin
      tested_mv := predicted_mv_list[i] / 4;
      if tested_mv = ZERO_MV then
          continue;

      x := clip3(MIN_XY, _mbx + tested_mv.x, _max_x);
      y := clip3(MIN_XY, _mby + tested_mv.y, _max_y);
      score := dsp.sad_16x16(cur, @ref[stride * y + x], stride);

      if score < _last_search_score then
        begin
          _last_search_score := score;
          _starting_fpel_mv := XYToMVec(x - _mbx, y - _mby);
        end;
    end;
end;

(*
  Fullpel ME search using 8-point diamond pattern
  output
  result - best found vector (in qpel units)
*)
function TRegionSearch.SearchFPel(var mb: TMacroblock; const fref: PFrame): TMotionvec;
var
  ref: uint8_p;
  max_x, max_y: int32_t;
  x, y: int32_t; // currently searched fpel x,y position
  stride: int32_t;
  min_score: int32_t;
  mv, mv_prev_pass: TMotionvec;
  iter: int32_t;
  check_bounds: boolean;
  range: int32_t;
  pixel_range: int32_t;

  procedure check_pattern(const pattern: array of TXYOffs);
  var
    i: int32_t;
    nx, ny: int32_t;
    score: int32_t;
  begin
    if check_bounds then
      if (x - 2 < MIN_XY) or (x + 2 > max_x) or
        (y - 2 < MIN_XY) or (y + 2 > max_y) then
          exit; // use large diamond range
    for i := 0 to Length(pattern) - 1 do
      begin
        nx := x + pattern[i][0];
        ny := y + pattern[i][1];
        score := dsp.sad_16x16(cur, @ref[stride * ny + nx], stride);
        if score < min_score then
          begin
            min_score := score;
            mv := XYToMVec(nx - _mbx, ny - _mby);
          end;
      end;
    x := _mbx + mv.x;
    y := _mby + mv.y;
  end;

begin
  if _last_search_score < FPEL_SAD_TRESH then
    begin
      mb.mv := _starting_fpel_mv * 4;
      result := mb.mv;
      exit;
    end;

  ref := fref^.plane_dec[0];
  stride := fref^.stride;
  max_x := _max_x;
  max_y := _max_y;
  range := ME_RANGES[mpFpel];

  mv := _starting_fpel_mv;
  x := _mbx + mv.x;
  y := _mby + mv.y;
  min_score := _last_search_score;

  iter := 0;
  pixel_range := 2 * range + 1;
  check_bounds := (x - pixel_range < MIN_XY) or (x + pixel_range > max_x) or
    (y - pixel_range < MIN_XY) or (y + pixel_range > max_y);
  repeat
    mv_prev_pass := mv;
    check_pattern(pt_dia_large_sparse);
    inc(iter);
  until (mv = mv_prev_pass) or (iter >= range);
  check_pattern(pt_square);
  _last_search_score := min_score;

  // scale mv to qpel units
  mb.mv := mv * 4;
  result := mb.mv;
end;

(*
  Half-pixel ME search using 4-point diamond pattern
  input
  mb.mv - starting vector in qpel units
  fref^.luma_mc - 4 half-pel interpolated planes
  output
  result - best found vector (in qpel units)
*)
function TRegionSearch.SearchHPel(var mb: TMacroblock; const fref: PFrame): TMotionvec;
var
  ref: array [0 .. 3] of uint8_p;
  max_x, max_y: int32_t;
  mbx, mby,      // macroblock hpel x,y position
  x, y: int32_t; // currently searched hpel x,y position
  stride: int32_t;
  min_score: int32_t;
  mv,
    mv_prev_pass: TMotionvec;
  range: int32_t;
  iter: int32_t;
  check_bounds: boolean;

  procedure check_pattern_hpel();
  var
    i, idx: int32_t;
    nx, ny,
      mcx, mcy: int32_t;
    score: int32_t;
  begin
    if check_bounds then
      if (x - 1 < MIN_XY_HPEL) or (x + 1 > max_x) or
        (y - 1 < MIN_XY_HPEL) or (y + 1 > max_y) then
          exit;
    for i := 0 to 3 do
      begin
        nx := x + pt_dia_small[i][0];
        ny := y + pt_dia_small[i][1];

        mcx := nx div 2;
        mcy := ny div 2;
        idx := ((ny and 1) shl 1) or (nx and 1);
        score := dsp.sad_16x16(cur, ref[idx] + mcy * stride + mcx, stride)
          + InterCostEval.BitCost(XYToMVec(nx - mbx, ny - mby) * 2);

        if score < min_score then
          begin
            min_score := score;
            mv := XYToMVec(nx - mbx, ny - mby);
          end;
      end;
    x := mbx + mv.x;
    y := mby + mv.y;
  end;

begin
  for x := 0 to 3 do
      ref[x] := fref^.luma_mc[x];
  stride := fref^.stride;
  mbx := _mbx * 2;
  mby := _mby * 2;
  max_x := _max_x_hpel;
  max_y := _max_y_hpel;
  range := ME_RANGES[mpHpel];

  // scale to hpel units
  mv := mb.mv / 2;
  x := mbx + mv.x;
  y := mby + mv.y;
  min_score := MaxInt; // we need to include bitcost in score, so reset

  iter := 0;
  check_bounds := (x - range < MIN_XY_HPEL) or (x + range > max_x) or (y - range < MIN_XY_HPEL) or (y + range > max_y);
  repeat
    mv_prev_pass := mv;
    check_pattern_hpel;
    inc(iter);
  until (mv = mv_prev_pass) or (iter >= range);
  _last_search_score := min_score;

  // scale mv to qpel units
  mb.mv := mv * 2;
  result := mb.mv;
end;

(* quarter-pixel motion vector refinement - search using 4-point diamond pattern
  input
  me - ME struct set up for qpel
  h.mb.mv - starting vector in qpel units
  output
  h.mb.mv - best found vector in qpel units
*)
function TRegionSearch.SearchQPel(var mb: TMacroblock; const fref: PFrame; const satd, chroma_me: boolean): TMotionvec;
var
  mbcmp: mbcmp_func_t;
  max_x, max_y: int32_t;
  mbx, mby,      // macroblock qpel x,y position
  x, y: int32_t; // currently searched qpel x,y position
  min_score: int32_t;
  mv,
    mv_prev_pass: TMotionvec;
  range: int32_t;
  iter: int32_t;
  check_bounds: boolean;

  function chroma_score: int32_t;
  begin
    result := dsp.satd_8x8(mb.pixels_c[0], mb.mcomp_c[0], 16);
    inc(result, dsp.satd_8x8(mb.pixels_c[1], mb.mcomp_c[1], 16));
  end;

  procedure check_pattern_qpel;
  var
    i: int32_t;
    nx, ny,
      score: int32_t;
  begin
    if check_bounds then
      if (x - 1 < MIN_XY_QPEL) or (x + 1 > max_x) or (y - 1 < MIN_XY_QPEL) or (y + 1 > max_y) then
          exit;
    for i := 0 to 3 do
      begin
        nx := x + pt_dia_small[i][0];
        ny := y + pt_dia_small[i][1];

        MotionCompensator.CompensateQPelXY(fref, nx, ny, mb.mcomp);
        score := mbcmp(cur, mb.mcomp, 16) + InterCostEval.BitCost(XYToMVec(nx - mbx, ny - mby));

        if chroma_me then
          begin
            MotionCompensator.CompensateChromaQpelXY(fref, nx, ny, mb.mcomp_c[0], mb.mcomp_c[1]);
            inc(score, chroma_score());
          end;

        if score < min_score then
          begin
            min_score := score;
            mv := XYToMVec(nx - mbx, ny - mby);
          end;
      end;
    x := mbx + mv.x;
    y := mby + mv.y;
  end;

begin
  mbx := _mbx * 4;
  mby := _mby * 4;
  max_x := _max_x_qpel;
  max_y := _max_y_qpel;
  if satd then
      mbcmp := dsp.satd_16x16
  else
      mbcmp := dsp.sad_16x16;
  range := ME_RANGES[mpQpel];

  mv := mb.mv;
  x := mbx + mv.x;
  y := mby + mv.y;
  min_score := MaxInt; // reset score, mbcmp may be different

  iter := 0;
  check_bounds := (x - range < MIN_XY_QPEL) or (x + range > max_x) or (y - range < MIN_XY_QPEL) or (y + range > max_y);
  repeat
    mv_prev_pass := mv;
    check_pattern_qpel();
    inc(iter);
  until (mv = mv_prev_pass) or (iter >= range);

  if min_score = MaxInt then
    begin // return valid score if no searches were done (rare cases at the padded edge of a frame)
      MotionCompensator.CompensateQPelXY(fref, x, y, mb.mcomp);
      min_score := mbcmp(cur, mb.mcomp, 16) + InterCostEval.BitCost(mv);
      if chroma_me then
        begin
          MotionCompensator.CompensateChromaQpelXY(fref, x, y, mb.mcomp_c[0], mb.mcomp_c[1]);
          inc(min_score, chroma_score());
        end;
    end;

  _last_search_score := min_score;
  mb.mv := mv;
  result := mv;
end;

end.
