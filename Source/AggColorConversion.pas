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
unit AggColorConversion;

interface

{$INCLUDE AggCompiler.inc}

uses
  AggBasics,
  AggColor32,
  AggRenderingBuffer;

type
  CopyRow = procedure(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversion(Dst, Src: TAggRenderingBuffer; CopyRowFunctor: CopyRow);

procedure ColorConversionGray8ToBgr24(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionGray8ToRgb24(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionRgb565ToRgb555(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionBgr24ToRgb24(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionBgr24ToBgr24(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionBgr24ToBgra32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionRgb24ToBgr24(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionRgb24ToBgra32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionBgra32ToArgb32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionAbgr32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionAbgr32ToBgra32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionRgba32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionRgba32ToBgra32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionArgb32ToBgra32(Dst, Src: PInt8u; width: Cardinal);

implementation

procedure ColorConversion(Dst, Src: TAggRenderingBuffer;
  CopyRowFunctor: CopyRow);
var
  Y, width, height: Cardinal;
begin
  width := Src.width;
  height := Src.height;

  if Dst.width < width then
    width := Dst.width;

  if Dst.height < height then
    height := Dst.height;

  if width > 0 then
    for Y := 0 to height - 1 do
      CopyRowFunctor(Dst.Row(Y), Src.Row(Y), width);
end;

procedure ColorConversionGray8ToBgr24(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderBgr.R)^ := Src^;
    PInt8u(PtrComp(Dst) + CAggOrderBgr.g)^ := Src^;
    PInt8u(PtrComp(Dst) + CAggOrderBgr.b)^ := Src^;

    Inc(PtrComp(Dst), 3);
    Inc(PtrComp(Src));
    Dec(width);
  until width = 0;
end;

procedure ColorConversionGray8ToRgb24(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderRgb.R)^ := Src^;
    PInt8u(PtrComp(Dst) + CAggOrderRgb.g)^ := Src^;
    PInt8u(PtrComp(Dst) + CAggOrderRgb.b)^ := Src^;

    Inc(PtrComp(Dst), 3);
    Inc(PtrComp(Src));
    Dec(width);
  until width = 0;
end;

procedure ColorConversionBgr24ToRgb24(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderBgr.R)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgb.R)^;
    PInt8u(PtrComp(Dst) + CAggOrderBgr.g)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgb.g)^;
    PInt8u(PtrComp(Dst) + CAggOrderBgr.b)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgb.b)^;

    Inc(PtrComp(Dst), 3);
    Inc(PtrComp(Src), 3);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionBgr24ToBgr24(Dst, Src: PInt8u; width: Cardinal);
begin
  Move(Src^, Dst^, width * 3);
end;

procedure ColorConversionBgra32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderArgb.R)^ :=
      PInt8u(PtrComp(Src) + CAggOrderBgra.R)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.g)^ :=
      PInt8u(PtrComp(Src) + CAggOrderBgra.g)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.b)^ :=
      PInt8u(PtrComp(Src) + CAggOrderBgra.b)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.A)^ :=
      PInt8u(PtrComp(Src) + CAggOrderBgra.A)^;

    Inc(PtrComp(Dst), 4);
    Inc(PtrComp(Src), 4);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionAbgr32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderArgb.R)^ :=
      PInt8u(PtrComp(Src) + CAggOrderAbgr.R)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.g)^ :=
      PInt8u(PtrComp(Src) + CAggOrderAbgr.g)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.b)^ :=
      PInt8u(PtrComp(Src) + CAggOrderAbgr.b)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.A)^ :=
      PInt8u(PtrComp(Src) + CAggOrderAbgr.A)^;

    Inc(PtrComp(Dst), 4);
    Inc(PtrComp(Src), 4);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionRgba32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderArgb.R)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgba.R)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.g)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgba.g)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.b)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgba.b)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.A)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgba.A)^;

    Inc(PtrComp(Dst), 4);
    Inc(PtrComp(Src), 4);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionBgr24ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderBgr(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderBgr(Src)^.g;
    PAggOrderBgra(Dst)^.R := PAggOrderBgr(Src)^.R;
    PAggOrderBgra(Dst)^.A := $FF;

    Inc(Dst, 4);
    Inc(Src, 3);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionRgb565ToRgb555(Dst, Src: PInt8u; width: Cardinal);
var
  RGB: Integer;
begin
  repeat
    RGB := PInt16u(Src)^;

    PInt16u(Dst)^ := ((RGB shr 1) and $7FE0) or (RGB and $1F);

    Inc(PtrComp(Src), 2);
    Inc(PtrComp(Dst), 2);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionRgb24ToBgr24(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgr(Dst)^.R := PAggOrderRgb(Src)^.R;
    PAggOrderBgr(Dst)^.g := PAggOrderRgb(Src)^.g;
    PAggOrderBgr(Dst)^.b := PAggOrderRgb(Src)^.b;

    Inc(Src, 3);
    Inc(Dst, 3);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionAbgr32ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderAbgr(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderAbgr(Src)^.g;
    PAggOrderBgra(Dst)^.R := PAggOrderAbgr(Src)^.R;
    PAggOrderBgra(Dst)^.A := PAggOrderAbgr(Src)^.A;

    Inc(Src, 4);
    Inc(Dst, 4);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionArgb32ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderArgb(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderArgb(Src)^.g;
    PAggOrderBgra(Dst)^.R := PAggOrderArgb(Src)^.R;
    PAggOrderBgra(Dst)^.A := PAggOrderArgb(Src)^.A;

    Inc(Src, 4);
    Inc(Dst, 4);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionRgba32ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderRgba(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderRgba(Src)^.g;
    PAggOrderBgra(Dst)^.R := PAggOrderRgba(Src)^.R;
    PAggOrderBgra(Dst)^.A := PAggOrderRgba(Src)^.A;

    Inc(Src, 4);
    Inc(Dst, 4);
    Dec(width);
  until width = 0;
end;

procedure ColorConversionRgb24ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderRgb(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderRgb(Src)^.g;
    PAggOrderBgra(Dst)^.R := PAggOrderRgb(Src)^.R;
    PAggOrderBgra(Dst)^.A := $FF;

    Inc(Src, 4);
    Inc(Dst, 4);
    Dec(width);
  until width = 0;
end;

end. 
