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
unit AggRendererMultiClip;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggArray,
  AggColor32,
  AggRenderingBuffer,
  AggRendererBase,
  AggPixelFormat;

type
  TAggRendererMultiClip = class(TAggRendererBase)
  private
    FClip: TAggPodDeque;
    FCurrentClibBoxIndex: Cardinal;
    FBounds: TRectInteger;
  protected
    function GetBoundingXMin: Integer; override;
    function GetBoundingYMin: Integer; override;
    function GetBoundingXMax: Integer; override;
    function GetBoundingYMax: Integer; override;
  public
    constructor Create(PixelFormatProcessor: TAggPixelFormatProcessor; OwnPixelFormatProcessor: Boolean = False); override;
    destructor Destroy; override;

    function GetBoundingClipBox: PRectInteger; virtual;

    procedure FirstClipBox; override;
    function NextClipBox: Boolean; override;

    procedure ResetClipping(Visibility: Boolean); override;

    procedure AddClipBox(x1, y1, x2, y2: Integer); overload;
    procedure AddClipBox(Rect: TRectInteger); overload;

    procedure CopyPixel(X, Y: Integer; C: PAggColor); override;
    procedure BlendPixel(X, Y: Integer; C: PAggColor; Cover: Int8u); override;
    function Pixel(X, Y: Integer): TAggColor; override;

    procedure CopyHorizontalLine(x1, Y, x2: Integer; C: PAggColor); override;
    procedure CopyVerticalLine(X, y1, y2: Integer; C: PAggColor); override;

    procedure BlendHorizontalLine(x1, Y, x2: Integer; C: PAggColor; Cover: Int8u); override;
    procedure BlendVerticalLine(X, y1, y2: Integer; C: PAggColor; Cover: Int8u); override;

    procedure CopyBar(x1, y1, x2, y2: Integer; C: PAggColor); override;
    procedure BlendBar(x1, y1, x2, y2: Integer; C: PAggColor; Cover: Int8u); override;

    procedure BlendSolidHSpan(X, Y, Len: Integer; C: PAggColor; Covers: PInt8u); override;
    procedure BlendSolidVSpan(X, Y, Len: Integer; C: PAggColor; Covers: PInt8u); override;

    procedure CopyColorHSpan(X, Y, Len: Integer; COLORS: PAggColor); override;
    procedure BlendColorHSpan(X, Y, Len: Integer; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull); override;
    procedure BlendColorVSpan(X, Y, Len: Integer; COLORS: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull); override;

    procedure CopyFrom(From: TAggRenderingBuffer; RC: PRectInteger = nil; tox: Integer = 0; ToY: Integer = 0); override;
  end;

implementation


{ TAggRendererMultiClip }

constructor TAggRendererMultiClip.Create(PixelFormatProcessor: TAggPixelFormatProcessor;
  OwnPixelFormatProcessor: Boolean = False);
begin
  inherited Create(PixelFormatProcessor, OwnPixelFormatProcessor);

  FClip := TAggPodDeque.Create(SizeOf(TRectInteger), 4);
  FBounds := RectInteger(GetXMin, GetYMin, GetXMax, GetYMax);

  FCurrentClibBoxIndex := 0;
end;

destructor TAggRendererMultiClip.Destroy;
begin
  FClip.Free;
  inherited;
end;

function TAggRendererMultiClip.GetBoundingClipBox;
begin
  Result := @FBounds;
end;

function TAggRendererMultiClip.GetBoundingXMin;
begin
  Result := FBounds.x1;
end;

function TAggRendererMultiClip.GetBoundingYMin;
begin
  Result := FBounds.y1;
end;

function TAggRendererMultiClip.GetBoundingXMax;
begin
  Result := FBounds.x2;
end;

function TAggRendererMultiClip.GetBoundingYMax;
begin
  Result := FBounds.y2;
end;

procedure TAggRendererMultiClip.FirstClipBox;
var
  CB: PRectInteger;
begin
  FCurrentClibBoxIndex := 0;

  if FClip.Size <> 0 then
    begin
      CB := FClip[0];

      ClipBoxNaked(CB.x1, CB.y1, CB.x2, CB.y2);
    end;
end;

function TAggRendererMultiClip.NextClipBox;
var
  CB: PRectInteger;
begin
  Inc(FCurrentClibBoxIndex);

  if FCurrentClibBoxIndex < FClip.Size then
    begin
      CB := FClip[FCurrentClibBoxIndex];

      ClipBoxNaked(CB.x1, CB.y1, CB.x2, CB.y2);

      Result := True;

      Exit;
    end;

  Result := False;
end;

procedure TAggRendererMultiClip.ResetClipping;
begin
  inherited ResetClipping(Visibility);

  FClip.RemoveAll;

  FCurrentClibBoxIndex := 0;

  FBounds := GetClipBox^;
end;

procedure TAggRendererMultiClip.AddClipBox(x1, y1, x2, y2: Integer);
begin
  AddClipBox(RectInteger(x1, y1, x2, y2));
end;

procedure TAggRendererMultiClip.AddClipBox(Rect: TRectInteger);
var
  RC: TRectInteger;
begin
  Rect.Normalize;
  RC := RectInteger(0, 0, width - 1, height - 1);

  if Rect.Clip(RC) then
    begin
      FClip.Add(@Rect);

      if Rect.x1 < FBounds.x1 then
          FBounds.x1 := Rect.x1;

      if Rect.y1 < FBounds.y1 then
          FBounds.y1 := Rect.y1;

      if Rect.x2 > FBounds.x2 then
          FBounds.x2 := Rect.x2;

      if Rect.y2 > FBounds.y2 then
          FBounds.y2 := Rect.y2;
    end;
end;

procedure TAggRendererMultiClip.CopyPixel(X, Y: Integer; C: PAggColor);
begin
  FirstClipBox;

  repeat
    if InBox(X, Y) then
      begin
        FPixelFormatProcessor.CopyPixel(FPixelFormatProcessor, X, Y, C);

        Break;
      end;
  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendPixel(X, Y: Integer; C: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
    if InBox(X, Y) then
      begin
        FPixelFormatProcessor.BlendPixel(FPixelFormatProcessor, X, Y, C, Cover);

        Break;
      end;
  until not NextClipBox;
end;

function TAggRendererMultiClip.Pixel;
begin
  FirstClipBox;

  repeat
    if InBox(X, Y) then
      begin
        Result := FPixelFormatProcessor.Pixel(FPixelFormatProcessor, X, Y);

        Exit;
      end;
  until not NextClipBox;

  Result.Clear;
end;

procedure TAggRendererMultiClip.CopyHorizontalLine(x1, Y, x2: Integer;
  C: PAggColor);
begin
  FirstClipBox;

  repeat
      inherited CopyHorizontalLine(x1, Y, x2, C);
  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyVerticalLine(X, y1, y2: Integer;
  C: PAggColor);
begin
  FirstClipBox;

  repeat
      inherited CopyVerticalLine(X, y1, y2, C);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendHorizontalLine(x1, Y, x2: Integer;
  C: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
      inherited BlendHorizontalLine(x1, Y, x2, C, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendVerticalLine(X, y1, y2: Integer;
  C: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
      inherited BlendVerticalLine(X, y1, y2, C, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyBar(x1, y1, x2, y2: Integer; C: PAggColor);
begin
  FirstClipBox;

  repeat
      inherited CopyBar(x1, y1, x2, y2, C);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendBar(x1, y1, x2, y2: Integer; C: PAggColor;
  Cover: Int8u);
begin
  FirstClipBox;

  repeat
      inherited BlendBar(x1, y1, x2, y2, C, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendSolidHSpan(X, Y, Len: Integer;
  C: PAggColor; Covers: PInt8u);
begin
  FirstClipBox;

  repeat
      inherited BlendSolidHSpan(X, Y, Len, C, Covers);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendSolidVSpan(X, Y, Len: Integer;
  C: PAggColor; Covers: PInt8u);
begin
  FirstClipBox;

  repeat
      inherited BlendSolidVSpan(X, Y, Len, C, Covers);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyColorHSpan(X, Y, Len: Integer;
  COLORS: PAggColor);
begin
  FirstClipBox;

  repeat
      inherited CopyColorHSpan(X, Y, Len, COLORS);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendColorHSpan(X, Y, Len: Integer;
  COLORS: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FirstClipBox;

  repeat
      inherited BlendColorHSpan(X, Y, Len, COLORS, Covers, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendColorVSpan(X, Y, Len: Integer;
  COLORS: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FirstClipBox;

  repeat
      inherited BlendColorVSpan(X, Y, Len, COLORS, Covers, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyFrom(From: TAggRenderingBuffer;
  RC: PRectInteger = nil; tox: Integer = 0; ToY: Integer = 0);
begin
  FirstClipBox;

  repeat
      inherited CopyFrom(From, RC, tox, ToY);

  until not NextClipBox;
end;

end. 
 
