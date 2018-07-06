{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
  //    Maintained by Christian-W. Budde (Christian@pcjv.de)                    //
  //    Copyright (c) 2012-2017                                                 //
  //                                                                            //
  //  Based on:                                                                 //
  //    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
  //    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
  //                                                                            //
  //  Original License:                                                         //
  //    Anti-Grain Geometry - Version 2.4 (Public License)                      //
  //    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
  //    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
  //                                                                            //
  //  Permission to copy, use, modify, sell and distribute this software        //
  //  is granted provided this copyright notice appears in all copies.          //
  //  This software is provided "as is" without express or implied              //
  //  warranty, and with no claim as to its suitability for any purpose.        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)
unit AggPixelFormatGray;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggPixelFormat,
  AggColor32,
  AggRenderingBuffer;

procedure PixelFormatGray8(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatGray8Bgr24r(var PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatGray8Bgr24g(var PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatGray8Bgr24b(var PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatGray8Pre(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatGray8PreBgr24r(var PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatGray8PreBgr24g(var PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatGray8PreBgr24b(var PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);

implementation

function Fmt8Row(This: TAggPixelFormatProcessor; X, Y: Integer): TAggRowDataType;
begin
  Result.Initialize(X, This.width - 1,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + X * This.Step + This.Offset));
end;

procedure GrayGammaDirApply(This: TAggPixelFormatProcessor; p: PInt8u);
begin
  p^ := This.Apply.dir[p^];
end;

procedure GrayGammaInvApply(This: TAggPixelFormatProcessor; p: PInt8u);
begin
  p^ := This.Apply.Inv[p^];
end;

procedure GrayForEachPixel(This: TAggPixelFormatProcessor; F: TAggFuncApplyGamma);
var
  Y, Len: Cardinal;
  p: PInt8u;
begin
  Y := 0;

  while Y < This.height do
    begin
      Len := This.width;
      p := PInt8u(PtrComp(This.RenderingBuffer.Row(Y)) + This.Offset);

      repeat
        F(This, p);

        Inc(PtrComp(p), This.Step);
        Dec(Len);

      until Len = 0;

      Inc(Y);
    end;
end;

{$INCLUDE AggPixelFormatGray8.inc }


procedure PixelFormatGray8;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 1, 0);

  PixelFormatProcessor.PixWidth := 1;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8BlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8Bgr24r;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 2);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8BlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8Bgr24g;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 1);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8BlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8Bgr24b;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 0);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8BlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

{$INCLUDE AggPixelFormatGray8Pre.inc }


procedure PixelFormatGray8Pre;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 1, 0);

  PixelFormatProcessor.PixWidth := 1;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8PreBlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8PreBgr24r;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 2);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8PreBlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8PreBgr24g;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 1);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8PreBlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

procedure PixelFormatGray8PreBgr24b;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer, 3, 0);

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Gray8CopyPixel;
  PixelFormatProcessor.BlendPixel := @Gray8PreBlendPixel;

  PixelFormatProcessor.Pixel := @Gray8Pixel;
  PixelFormatProcessor.Row := @Fmt8Row;

  PixelFormatProcessor.CopyHorizontalLine := @Gray8CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Gray8CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Gray8PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Gray8PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Gray8PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Gray8PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Gray8CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Gray8CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Gray8PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Gray8PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Gray8CopyFrom;
  PixelFormatProcessor.BlendFrom := nil; // not defined in aggPixelFormatGray.h

  PixelFormatProcessor.BlendFromColor := @Gray8PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Gray8PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @GrayForEachPixel;
  PixelFormatProcessor.GammaDirApply := @GrayGammaDirApply;
  PixelFormatProcessor.GammaInvApply := @GrayGammaInvApply;
end;

end. 
