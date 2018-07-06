{ ****************************************************************************** }
{ * h264inter_pred.pas        by qq600585                                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264inter_pred;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Stdint, h264Common, h264Util;

procedure mb_load_mvs(var mb: TMacroblock; const frame: TFrame; const num_ref_frames: int32_t);

implementation

(* ******************************************************************************
  calculate predicted mv, store some mvs as predictors for ME
  mb layout:
  D B C
  A X
*)
procedure mb_load_mvs(var mb: TMacroblock; const frame: TFrame; const num_ref_frames: int32_t);

  function is_avail(const mb: PMacroblock): Boolean; inline;
  begin
    Result := not(mb^.mbtype in [MB_I_4x4, MB_I_16x16]);
  end;

type
  mb_info = record
    avail: Boolean;
    mv: TMotionvec;
    refidx: int32_t;
  end;

var
  mbs: array [0 .. 2] of mb_info; // A, B, C (D)
  T: PMacroblock;
  i: int32_t;
  num_avail: int32_t;
  same_ref_n: int32_t;
  same_ref_i: int32_t;

  procedure assign_mb(var M: mb_info);
  begin
    M.avail := is_avail(T);
    M.mv := T^.mv;
    M.refidx := T^.ref;
    if M.avail then
        Inc(num_avail);
  end;

begin
  if frame.ftype = SLICE_I then
      Exit;

  mb.mv := ZERO_MV;
  num_avail := 0;
  for i := 0 to 2 do
    begin
      mbs[i].avail := False;
      mbs[i].mv := ZERO_MV;
      mbs[i].refidx := 0;
    end;

  // left mb - A
  if mb.X > 0 then
    begin
      T := @frame.mbs[mb.Y * frame.mbw + mb.X - 1];
      assign_mb(mbs[0]);
    end;

  // top mbs - B, C/D
  if mb.Y > 0 then
    begin
      T := @frame.mbs[(mb.Y - 1) * frame.mbw + mb.X];
      assign_mb(mbs[1]);

      if mb.X < frame.mbw - 1 then
          T := @frame.mbs[(mb.Y - 1) * frame.mbw + mb.X + 1] // C
      else
          T := @frame.mbs[(mb.Y - 1) * frame.mbw + mb.X - 1]; // D
      assign_mb(mbs[2]);
    end;

  case num_avail of
    0:
      mb.mvp := ZERO_MV;

    1:
      begin
        // only one mb is available for interpred - use its mv (if the refidx is the same)
        for i := 0 to 2 do
          if mbs[i].avail then
            begin
              mb.mvp := mbs[i].mv;
              if (num_ref_frames > 1) and (mb.Y > 0) and (mbs[i].refidx <> mb.ref) then
                  mb.mvp := ZERO_MV;
            end;
      end;

    2, 3:
      begin
        // mvp = median (a, b ,c)
        mb.mvp.X := Median(mbs[0].mv.X, mbs[1].mv.X, mbs[2].mv.X);
        mb.mvp.Y := Median(mbs[0].mv.Y, mbs[1].mv.Y, mbs[2].mv.Y);

        // only one mb has same refidx - 8.4.1.3.1
        if (num_ref_frames > 1) then
          begin
            same_ref_n := 0;
            same_ref_i := 0;
            for i := 0 to 2 do
              if mbs[i].avail and (mbs[i].refidx = mb.ref) then
                begin
                  Inc(same_ref_n);
                  same_ref_i := i;
                end;
            if same_ref_n = 1 then
                mb.mvp := mbs[same_ref_i].mv;
          end;
      end;

  end;

  // get skip mv from predicted mv (for P_SKIP) - 8.4.1.1
  mb.mv_skip := mb.mvp;
  // frame edge
  if (mb.X = 0) or (mb.Y = 0) then
      mb.mv_skip := ZERO_MV;
  // A
  if mb.X > 0 then
    if mbs[0].avail and (mbs[0].mv = ZERO_MV) and (mbs[0].refidx = 0) then
        mb.mv_skip := ZERO_MV;
  // B
  if mb.Y > 0 then
    if mbs[1].avail and (mbs[1].mv = ZERO_MV) and (mbs[1].refidx = 0) then
        mb.mv_skip := ZERO_MV;
end;

end.  
