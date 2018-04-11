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


unit iexCanvasUtils;

(*
File version 1006
*)

{$I ie.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, {$ifdef IEHASTYPES}Types,{$endif} Graphics, Controls, stdctrls;

type
  TXCustomShape = (xcsStar5, xcsStar6, xcsArrowNW, xcsArrowNE, xcsArrowSW, xcsArrowSE, xcsLightningLeft, xcsLightningRight,
                   xcsExplosion, xcsExplosion_2, xcsCross, xcsArrowNW2, xcsArrowNE2, xcsArrowSW2, xcsArrowSE2, xcsHeart, xcsDoubleHeart);

  TWallpaperEffect = (
      wpSolid,                  // A color fill
      wpLeftLine,               // A single line on left
      wpTopLine,                // A single line along top
      wpDoubleLeftLines,        // Two lines on left
      wpDoubleTopLines,         // Two lines along top
      wpDoubleVariedLeftLines,  // Two lines of decreasing widths on left
      wpDoubleVariedTopLines,   // Two lines of decreasing widths along top
      wpTripleLeftLines,        // Three lines on left
      wpTripleTopLines,         // Three lines along top
      wpTripleVariedLeftLines,  // Three lines of decreasing widths on left
      wpTripleVariedTopLines,   // Three lines of decreasing widths along top
      wpLeftDashes,             // Short lines of a uniform sizes on left
      wpTopDashes,              // Short lines of a uniform sizes along top
      wpDoubleLeftDashes,       // Short lines of two sizes on left
      wpDoubleTopDashes,        // Short lines of two sizes along top
      wpTripleLeftDashes,       // Short lines of three sizes on left
      wpTripleTopDashes,        // Short lines of three sizes along top
      wpLeftZigZag,             // Shark tooth pattern on left
      wpTopZigZag,              // Shark tooth pattern along top
      wpLeftGradient,           // Color gradient on left
      wpTopGradient,            // Color gradient along top
      wpHorzPinStripe,          // Horizontal lines across image
      wpVertPinStripe,          // Vertical lines down image
      wpDiagPinStripe,          // Diagonal lines across image
      wpDiagPinStripe2,         // Diagonal lines across image (alternative direction)
      wpCrossHatch,             // Horizontal and vertical lines
      wpDiagCrossHatch,         // Diagonal lines in two directions
      wpHorzStripes,            // Horizontal stripes across image
      wpVertStripes,            // Vertical stripes down image
      wpDiagStripes,            // Diagonal stripes down image  
      wpDiagStripes2,           // Diagonal stripes down image (alternative direction)
      wpCheckers,               // Alternativing blocks of color
      wpDiagCheckers,           // Alternativing diagonal blocks of color
      wpRain,                   // Diagonal dashes
      wpRain2,                  // Diagonal dashes (alternative direction)
      wpRivets,                 // Circles over page
      wpHearts,                 // Hearts over page
      wpStars,                  // Stars over page
      wpLightning,              // Lightning over page
      wpRuledPage);             // Similar to standard ruled page (with red line along side)



  // Draw a TXCustomShape onto a canvas
  procedure DrawCustomShape(ACanvas: TCanvas; AShape: TXCustomShape; iLeft, iTop, iWidth, iHeight: Integer);

  // Create an HRGN of a TXCustomShape
  function CreateCustomShapeRegion(AShape: TXCustomShape; iLeft, iTop, iWidth, iHeight: Integer): Hrgn;

  // Create a bitmap suitable for use as HTML wallpaper (i.e. which can be tiled)
  // Returns a bitmap if succesful which must be freed after usage
  function CreateWallpaperBitmap(Effect : TWallpaperEffect; Color1 : TColor; Color2 : TColor; iSize : Integer = -1; iSpacing : integer = -1; iThickness: Integer = -1) : TBitmap;

  // Fill the canvas by tiling the specified bitmap (i.e. for wallpaper type effects)
  Procedure TileBitmapOntoCanvas(Canvas : TCanvas; iWidth, iHeight : Integer; Bitmap : TBitmap);

implementation

uses
  Math, iesettings, iexHelperFunctions, hyieutils;

type
  TShapeDirection = (_xsdNW, _xsdNE, _xsdSW, _xsdSE);

const
  Cross_Horz_Bar_Vert_Offset = 0.33;



function CreateHeartRegion(iLeft, iTop, iWidth, iHeight: Integer): Hrgn;
const
  Triangle_Start_Point_On_Circle = 0.69;     // start trinagle 62% down the circle
  Top_Circle_Bulge               = 2;        // two pixels bigger
  LHS_Circle_Intersect_Deg       = 233.14;   // Start triangle 1.5% in from edge
  RHS_Circle_Intersect_Deg       = 360 - LHS_Circle_Intersect_Deg; //126.86;
  LHS_Circle_Middle_Touch_Deg    = 83;
  RHS_Circle_Middle_Touch_Deg    = 360 - LHS_Circle_Middle_Touch_Deg;

  Function FindPointOnCircleEdge(Center: Tpoint; Angle: Real; Radius: Word): TPoint;
  Begin
     Result.X := Round(center.x + Radius*cos((angle-90)*pi/180));
     Result.Y := round(center.y + Radius*sin((angle-90)*pi/180));
  End;

var
  rgn1, rgn2, rgn3, rgn4 : Hrgn;
  iCircleSize: Integer;
  poly: array[0..2] of TPoint;
  iCircleRadius: Integer;
  LeftCircleIntersectPt: TPoint;
  RightCircleIntersectPt: TPoint;
  iHalfWidth: Integer;
  iQuarterWidth: Integer;
  iShortSide: Integer;
begin
  // Maintain the Aspect Ratio of the Heart
  if iWidth < iHeight then
    iShortSide := iWidth
  else
    iShortSide := iHeight;
  Inc(iLeft, (iWidth - iShortSide) div 2);
  Inc(iTop, (iHeight - iShortSide) div 2);
  iWidth  := iShortSide;
  iHeight := iShortSide;

  iHalfWidth    := iWidth div 2;
  iQuarterWidth := iWidth div 4;
  iCircleSize   := iHalfWidth + Top_Circle_Bulge;
  iCircleRadius := iCircleSize div 2;

  LeftCircleIntersectPt  := FindPointOnCircleEdge(Point(iQuarterWidth, iQuarterWidth), LHS_Circle_Intersect_Deg, iCircleRadius - 1);
  RightCircleIntersectPt := FindPointOnCircleEdge(Point(iWidth - iQuarterWidth, iQuarterWidth), RHS_Circle_Intersect_Deg, iCircleRadius - 1);
                
  Result :=  CreateEllipticRgn(iLeft, iTop, iWidth, iHeight);

  // Circles
  rgn1 := CreateEllipticRgn(iLeft,
                            iTop,
                            iLeft + iCircleSize,
                            iTop + iCircleSize);
  rgn2 := CreateEllipticRgn(iLeft + iWidth - iCircleSize,
                            iTop,
                            iLeft + iWidth,
                            iTop + iCircleSize);
  CombineRgn(Result, rgn1, rgn2, RGN_OR);

  // Bottom Triangle
  poly[0] := point(iLeft + LeftCircleIntersectPt.X,  iTop + LeftCircleIntersectPt.Y);
  poly[1] := point(iLeft + RightCircleIntersectPt.X, iTop + RightCircleIntersectPt.Y);
  poly[2] := point(iLeft + iHalfWidth, iTop + iHeight);

  rgn3 :=  Windows.CreatePolygonRgn(poly, 3, WINDING);
  CombineRgn(Result, Result, rgn3, RGN_OR);

  // Fill Hole
  rgn4 :=  Windows.CreateRectRgn(iLeft + iCircleRadius, iTop + iCircleRadius,
                                 iLeft + iWidth - iCircleRadius, iTop + iCircleSize);
  CombineRgn(Result, Result, rgn4, RGN_OR);

  DeleteObject(rgn4);
  DeleteObject(rgn3);
  DeleteObject(rgn2);
  DeleteObject(rgn1);
end;

       
function CreateDoubleHeartRegion(iLeft, iTop, iWidth, iHeight: Integer): Hrgn;  
const
  Dbl_Heart_Size = 66;
  Dbl_Heart_Offset = 5;
var
  Rgn2 : Hrgn;
begin
  // Left heart - Higher
  Result := CreateHeartRegion(iLeft,
                              iTop + MulDiv(iHeight, Dbl_Heart_Offset, 100),
                              MulDiv(iWidth, Dbl_Heart_Size, 100),
                              MulDiv(iHeight, Dbl_Heart_Size, 100));

  // Right heart - Lower
  Rgn2   := CreateHeartRegion(iLeft + MulDiv(iWidth, 100 - Dbl_Heart_Size, 100),
                              iTop + MulDiv(iHeight, 100 - Dbl_Heart_Size, 100) - MulDiv(iHeight, Dbl_Heart_Offset, 100),
                              MulDiv(iWidth, Dbl_Heart_Size, 100),
                              MulDiv(iHeight, Dbl_Heart_Size, 100));
  CombineRgn(Result, Result, Rgn2, RGN_OR);
  DeleteObject(Rgn2);
end;



type
  TFloatPoint= record
    x: extended;
    y: extended;
  end;

                                                           
// Create square canvas to maintain the Aspect Ratio of the shape
procedure KeepAspectRatioForDimensions(var iLeft, iTop, iWidth, iHeight: integer);
var
  iShortSide: Integer;
begin
  if iWidth < iHeight then
    iShortSide := iWidth
  else
    iShortSide := iHeight;
  Inc(iLeft, (iWidth - iShortSide) div 2);
  Inc(iTop, (iHeight - iShortSide) div 2);
  iWidth  := iShortSide;
  iHeight := iShortSide;
end;


function Create5PointStarRegion(iLeft, iTop, iWidth, iHeight: Integer): Hrgn;
const
  Star_5_Point    : array[0 .. 10] of TFloatPoint = ((X: 0.5; Y: 0), (X: 0.62; Y: 0.36), (X: 1; Y: 0.36), (X: 0.7; Y: 0.59), (X: 0.81; Y: 0.95), (X: 0.5; Y: 0.73), (X: 0.19; Y: 0.95), (X: 0.31; Y: 0.59), (X: 0; Y: 0.36), (X: 0.39; Y: 0.36), (X: 0.5; Y: 0));
var
  iPointCount:  Integer;
  PointArray: array[0 .. 10] of TPoint;
  i: Integer;
begin
  KeepAspectRatioForDimensions(iLeft, iTop, iWidth, iHeight);

  iPointCount := 1 + High(Star_5_Point) - Low(Star_5_Point);
  for i := 0 to iPointCount - 1 do
  begin
    PointArray[i].x := iLeft + round(Star_5_Point[i].x * (iWidth - 1));
    PointArray[i].y := iTop  + round(Star_5_Point[i].y * (iHeight - 1));
  end;               
  Result :=  Windows.CreatePolygonRgn(PointArray, iPointCount, WINDING);
end;


function Create6PointStarRegion(iLeft, iTop, iWidth, iHeight: Integer): Hrgn;
const
  Star_6_Point: array[0 .. 12] of TFloatPoint = ((X: 0.5; Y: 0), (X: 0.625; Y: 0.25), (X: 0.9; Y: 0.25), (X: 0.75; Y: 0.5), (X: 0.9; Y: 0.75), (X: 0.625; Y: 0.75), (X: 0.5; Y: 1), (X: 0.375; Y: 0.75), (X: 0.1; Y: 0.75), (X: 0.25; Y: 0.5), (X: 0.1; Y: 0.25), (X: 0.375; Y: 0.25), (X: 0.5; Y: 0));
var
  iPointCount: Integer;
  PointArray: array[0 .. 12] of TPoint;
  i: Integer;
begin
  KeepAspectRatioForDimensions(iLeft, iTop, iWidth, iHeight);

  iPointCount := 1 + High(Star_6_Point) - Low(Star_6_Point);
  
  for i := 0 to iPointCount - 1 do
  begin
    PointArray[i].x := iLeft + round(Star_6_Point[i].x * (iWidth - 1));
    PointArray[i].y := iTop  + round(Star_6_Point[i].y * (iHeight - 1));
  end;

  Result :=  Windows.CreatePolygonRgn(PointArray, iPointCount, WINDING);
end;



function CreatePointingArrowRegion(iLeft, iTop, iWidth, iHeight: Integer; ADirection: TShapeDirection): Hrgn;
const
  Arrow_NW: array[0 ..  7] of TFloatPoint = ((X: 0; Y: 0), (X: 0.4; Y: 0), (X: 0.3; Y: 0.1), (X: 1; Y: 0.8), (X: 0.8; Y: 1), (X: 0.1; Y: 0.3), (X: 0; Y: 0.4), (X: 0; Y: 0));
var
  iPointCount: Integer;
  PointArray: array[0 .. 7] of TPoint;
  i: Integer;
  iX, iY: Extended;
begin
  KeepAspectRatioForDimensions(iLeft, iTop, iWidth, iHeight);

  iPointCount := 1 + High(Arrow_NW) - Low(Arrow_NW);
  
  for i := 0 to iPointCount - 1 do
  begin
    // FLIP HORZ
    if ADirection in [_xsdNE, _xsdSE] then
      iX := 1 - Arrow_NW[i].x
    else
      iX := Arrow_NW[i].x;

    // FLIP VERT
    if ADirection in [_xsdSE, _xsdSW] then
      iY := 1 - Arrow_NW[i].y
    else
      iY := Arrow_NW[i].y;

    PointArray[i].x := iLeft + round(iX * (iWidth - 1));
    PointArray[i].y := iTop  + round(iY * (iHeight - 1));
  end;

  Result :=  Windows.CreatePolygonRgn(PointArray, iPointCount, WINDING);
end;


function CreateShootingArrowRegion(iLeft, iTop, iWidth, iHeight: Integer; ADirection: TShapeDirection): Hrgn;
const
  Arrow_NW_2      : array[0 .. 30] of TFloatPoint = ((X: 0.85; Y: 1), (X: 0.8; Y: 0.95), (X: 0.8; Y: 0.85), (X: 0.775; Y: 0.825), (X: 0.775; Y: 0.925), (X: 0.725; Y: 0.875), (X: 0.725; Y: 0.775), (X: 0.7; Y: 0.75), (X: 0.7; Y: 0.85), (X: 0.65; Y: 0.8), (X: 0.65; Y: 0.7), (X: 0.15; Y: 0.2), (X: 0.15; Y: 0.3), (X: 0; Y: 0.15), (X: 0; Y: 0), (X: 0.15; Y: 0), (X: 0.3; Y: 0.15), (X: 0.2; Y: 0.15), (X: 0.7; Y: 0.65), (X: 0.8; Y: 0.65), (X: 0.85; Y: 0.7), (X: 0.75; Y: 0.7), (X: 0.775; Y: 0.725), (X: 0.875; Y: 0.725), (X: 0.925; Y: 0.775), (X: 0.825; Y: 0.775), (X: 0.85; Y: 0.8), (X: 0.95; Y: 0.8), (X: 1; Y: 0.85), (X: 0.85; Y: 0.85), (X: 0.85; Y: 1));
var
  iPointCount: Integer;
  PointArray: array[0 .. 30] of TPoint;
  i: Integer;
  iX, iY: Extended;
begin
  KeepAspectRatioForDimensions(iLeft, iTop, iWidth, iHeight);

  iPointCount := 1 + High(Arrow_NW_2) - Low(Arrow_NW_2);
  
  for i := 0 to iPointCount - 1 do
  begin
    // FLIP HORZ
    if ADirection in [_xsdNE, _xsdSE] then
      iX := 1 - Arrow_NW_2[i].x
    else
      iX := Arrow_NW_2[i].x;

    // FLIP VERT
    if ADirection in [_xsdSE, _xsdSW] then
      iY := 1 - Arrow_NW_2[i].y
    else
      iY := Arrow_NW_2[i].y;

    PointArray[i].x := iLeft + round(iX * (iWidth - 1));
    PointArray[i].y := iTop  + round(iY * (iHeight - 1));
  end;

  Result :=  Windows.CreatePolygonRgn(PointArray, iPointCount, WINDING);
end;



function CreateLightningRegion(iLeft, iTop, iWidth, iHeight: Integer; bLeft: Boolean): Hrgn;
const
  Lightning_Left  : array[0 .. 11] of TFloatPoint = ((X: 1; Y: 0.19), (X: 0.61; Y: 0.01), (X: 0.41; Y: 0.29), (X: 0.49; Y: 0.31), (X: 0.23; Y: 0.55), (X: 0.33; Y: 0.6), (X: 0; Y: 1), (X: 0.51; Y: 0.69), (X: 0.46; Y: 0.65), (X: 0.78; Y: 0.45), (X: 0.66; Y: 0.39), (X: 1; Y: 0.19));
var
  iPointCount: Integer;
  PointArray: array[0 .. 11] of TPoint;
  i: Integer;
  iX: Extended;
  iShortSide: Integer;
begin
  // Maintain the Aspect Ratio
  if iWidth > iHeight then
  begin
    iShortSide := iHeight;
    Inc(iLeft, (iWidth - iShortSide) div 2);
    iWidth  := iShortSide;
  end;

  iPointCount := 1 + High(Lightning_Left) - Low(Lightning_Left);
  
  for i := 0 to iPointCount - 1 do
  begin
    if bLeft then
      iX := Lightning_Left[i].x
    else
      iX := 1 - Lightning_Left[i].x;
   PointArray[i].x := iLeft + round(iX * (iWidth - 1));
   PointArray[i].y := iTop  + round(Lightning_Left[i].y * (iHeight - 1));
  end;

  Result :=  Windows.CreatePolygonRgn(PointArray, iPointCount, WINDING);
end;


function CreateExplosionRegion(iLeft, iTop, iWidth, iHeight: Integer; bReverse: Boolean): Hrgn;
const
  Explosion: array[0 .. 24] of TFloatPoint = ((X: 0.33; Y: 0), (X: 0.5; Y: 0.26), (X: 0.62; Y: 0.11), (X: 0.67; Y: 0.28), (X: 0.99; Y: 0.1), (X: 0.78; Y: 0.35), (X: 1; Y: 0.39), (X: 0.84; Y: 0.54), (X: 0.99; Y: 0.66), (X: 0.75; Y: 0.64), (X: 0.78; Y: 0.81), (X: 0.65; Y: 0.71), (X: 0.61; Y: 1), (X: 0.51; Y: 0.69), (X: 0.38; Y: 0.89), (X: 0.35; Y: 0.66), (X: 0.17; Y: 0.83), (X: 0.22; Y: 0.59), (X: 0; Y: 0.61), (X: 0.19; Y: 0.48), (X: 0.04; Y: 0.37), (X: 0.22; Y: 0.34), (X: 0.16; Y: 0.2), (X: 0.35; Y: 0.25), (X: 0.33; Y: 0.01));
var
  iPointCount: Integer;
  PointArray: array[0 .. 24] of TPoint;
  i: Integer;
  iX, iY: Extended;
begin
  iPointCount := 1 + High(Explosion) - Low(Explosion);
  
  for i := 0 to iPointCount - 1 do
  begin
    if bReverse then
    begin
      iX := 1 - Explosion[i].x;
      iY := 1 - Explosion[i].y;
    end
    else    
    begin
      iX := Explosion[i].x;
      iY := Explosion[i].y;
    end;
    PointArray[i].x := iLeft + round(iX * (iWidth - 1));
    PointArray[i].y := iTop  + round(iY * (iHeight - 1));
  end;

  Result :=  Windows.CreatePolygonRgn(PointArray, iPointCount, WINDING);
end;


function CreateCrossRegion(iLeft, iTop, iWidth, iHeight: Integer; rOffset: Single = Cross_Horz_Bar_Vert_Offset): Hrgn;
const
  Cross_Bar_Width = 0.22;
var
  iShortSide: Integer;
  iBarWidth: Integer;
  iBarLeft: Integer;
  iBarTop: Integer;
  HorzRgn: HRGN;
begin
  if iWidth > Round(iHeight * 0.75)  then
  begin
    iShortSide := Round(iHeight * 0.75);
    Inc(iLeft, (iWidth - iShortSide) div 2);
    iWidth  := iShortSide;
  end;

  iBarWidth := Min(Round(iWidth *0.2), Round(iHeight * Cross_Bar_Width));
  iBarLeft  := iLeft + ((iWidth - iBarWidth) div 2);
  iBarTop   := iTop + Round((iHeight - iBarWidth) * rOffset);

  // Vertical  
  Result :=  Windows.CreateRectRgn(iBarLeft, iTop, iBarLeft + iBarWidth, iTop + iHeight);

  // Horizontal
  HorzRgn :=  Windows.CreateRectRgn(iLeft, iBarTop, iLeft + iWidth, iBarTop + iBarWidth);

  CombineRgn(Result, Result, HorzRgn, RGN_OR);

  DeleteObject(HorzRgn);
end;



// Create an HRGN of a TXCustomShape
function CreateCustomShapeRegion(AShape: TXCustomShape; iLeft, iTop, iWidth, iHeight: Integer): Hrgn;
begin
  case AShape of
    xcsStar5          : Result := Create5PointStarRegion(iLeft, iTop, iWidth, iHeight);
    xcsStar6          : Result := Create6PointStarRegion(iLeft, iTop, iWidth, iHeight);
    xcsArrowNW        : Result := CreatePointingArrowRegion(iLeft, iTop, iWidth, iHeight, _xsdNW);
    xcsArrowNE        : Result := CreatePointingArrowRegion(iLeft, iTop, iWidth, iHeight, _xsdNE);
    xcsArrowSW        : Result := CreatePointingArrowRegion(iLeft, iTop, iWidth, iHeight, _xsdSW);
    xcsArrowSE        : Result := CreatePointingArrowRegion(iLeft, iTop, iWidth, iHeight, _xsdSE);
    xcsLightningLeft  : Result := CreateLightningRegion(iLeft, iTop, iWidth, iHeight, True);
    xcsLightningRight : Result := CreateLightningRegion(iLeft, iTop, iWidth, iHeight, False);
    xcsExplosion      : Result := CreateExplosionRegion(iLeft, iTop, iWidth, iHeight, False);
    xcsExplosion_2    : Result := CreateExplosionRegion(iLeft, iTop, iWidth, iHeight, True);
    xcsCross          : Result := CreateCrossRegion(iLeft, iTop, iWidth, iHeight);
    xcsArrowNW2       : Result := CreateShootingArrowRegion(iLeft, iTop, iWidth, iHeight, _xsdNW);
    xcsArrowNE2       : Result := CreateShootingArrowRegion(iLeft, iTop, iWidth, iHeight, _xsdNE);
    xcsArrowSW2       : Result := CreateShootingArrowRegion(iLeft, iTop, iWidth, iHeight, _xsdSW);
    xcsArrowSE2       : Result := CreateShootingArrowRegion(iLeft, iTop, iWidth, iHeight, _xsdSE);
    xcsHeart          : Result := CreateHeartRegion(iLeft, iTop, iWidth, iHeight);
    xcsDoubleHeart    : Result := CreateDoubleHeartRegion(iLeft, iTop, iWidth, iHeight);
    else                Result := 0;
  end;
end;


// Draw a TXCustomShape onto a canvas
procedure DrawCustomShape(ACanvas: TCanvas; AShape: TXCustomShape; iLeft, iTop, iWidth, iHeight: Integer);
var
  rgnMain: HRGN;
begin
  rgnMain := CreateCustomShapeRegion(AShape, iLeft, iTop, iWidth, iHeight);
  try                                  
    SelectClipRgn(ACanvas.handle, rgnMain);
    ACanvas.FillRect(Rect(iLeft, iTop, iLeft + iWidth, iTop + iHeight));
    SelectClipRgn(ACanvas.handle, 0);
  finally
    DeleteObject(rgnMain);
  end;
end;


// Create a bitmap suitable for use as HTML wallpaper (i.e. which can be tiled)
// Returns a bitmap if succesful which must be freed after usage
function CreateWallpaperBitmap(Effect : TWallpaperEffect; Color1 : TColor; Color2 : TColor; iSize : Integer = -1; iSpacing : integer = -1; iThickness: Integer = -1) : TBitmap;
const
  Full_Page_Wallpaper_Width  = 4000;
  Full_Page_Wallpaper_Height = 3000;
  Small_Width  = 5;
  Small_Height = 5;

  procedure _InitBitmap(iWidth, iHeight : Integer);
  begin
    Result := TBitmap.create;
    Result.PixelFormat := pf24bit;
    Result.Width  := iWidth;
    Result.Height := iHeight;
    Result.Canvas.Brush.Color := Color2;
    Result.Canvas.FillRect(Rect(0, 0, iWidth, iHeight));
  end;

var
  I: Integer;
  AShape: TXCustomShape;
begin
  Result := nil;
  case Effect of

    wpSolid :
        // A color fill
        begin
          // iSize      : Not used
          // iSpacing   : Not used
          // iThickness : Not used

          _InitBitmap(Small_Width, Small_Height);
        end;

    wpLeftLine :
        // A single line on left
        begin
          // iSize      : Not used
          // iSpacing   : Not used
          // iThickness : Width of line (3)

          if iThickness < 1 then
            iThickness := 3;
          _InitBitmap(Full_Page_Wallpaper_Width, 1);
          for I := 0 to iThickness - 1 do
            Result.Canvas.Pixels[I, 0] := Color1;
        end;

    wpTopLine :
        // A single line along top
        begin
          // iSize      : Not used
          // iSpacing   : Not used
          // iThickness : Width of line (3)

          if iThickness < 1 then
            iThickness := 3;            
          _InitBitmap(1, Full_Page_Wallpaper_Height);
          for I := 0 to iThickness - 1 do
            Result.Canvas.Pixels[0, I] := Color1;
        end;

    wpDoubleLeftLines :
        // Two lines on left
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (3 x iThickness)
          // iThickness : Width of line (2)

          if iThickness < 1 then
            iThickness := 2;
          if iSpacing < 1 then
            iSpacing := iThickness * 3;
          _InitBitmap(Full_Page_Wallpaper_Width, 1);
          for I := 0 to iThickness - 1 do
          begin
            Result.Canvas.Pixels[I, 0] := Color1;
            Result.Canvas.Pixels[I + iSpacing, 0] := Color1;
          end;
        end;

    wpDoubleTopLines :
        // Two lines along top
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (3 x iThickness)
          // iThickness : Width of line (2)

          if iThickness < 1 then
            iThickness := 2;
          if iSpacing < 1 then
            iSpacing := iThickness * 3;
          _InitBitmap(1, Full_Page_Wallpaper_Height);
          for I := 0 to iThickness - 1 do
          begin
            Result.Canvas.Pixels[0, I] := Color1;
            Result.Canvas.Pixels[0, I + iSpacing] := Color1;
          end;
        end;

    wpDoubleVariedLeftLines :
        // Two lines of decreasing widths on left
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (3 x iThickness)
          // iThickness : Width of thinnest line (2)

          if iThickness < 1 then
            iThickness := 2;
          if iSpacing < 1 then
            iSpacing := iThickness * 3;
          _InitBitmap(Full_Page_Wallpaper_Width, 1);
          for I := 0 to iThickness - 1 do
          begin
            Result.Canvas.Pixels[I, 0] := Color1;
            Result.Canvas.Pixels[I + iThickness, 0] := Color1;
            Result.Canvas.Pixels[I + iThickness + iSpacing, 0] := Color1;
          end;
        end;

    wpDoubleVariedTopLines :
        // Two lines of decreasing widths along top
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (3 x iThickness)
          // iThickness : Width of thinnest line (2)

          if iThickness < 1 then
            iThickness := 2;
          if iSpacing < 1 then
            iSpacing := iThickness * 3;
          _InitBitmap(1, Full_Page_Wallpaper_Height);
          for I := 0 to iThickness - 1 do
          begin
            Result.Canvas.Pixels[0, I] := Color1;
            Result.Canvas.Pixels[0, I + iThickness] := Color1;
            Result.Canvas.Pixels[0, I + iThickness + iSpacing] := Color1;
          end;
        end;

    wpTripleLeftLines :
        // Three lines on left
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (3 x iThickness)
          // iThickness : Width of line (2)

          if iThickness < 1 then
            iThickness := 2;
          if iSpacing < 1 then
            iSpacing := iThickness * 3;
          _InitBitmap(Full_Page_Wallpaper_Width, 1);
          for I := 0 to iThickness - 1 do
          begin
            Result.Canvas.Pixels[I, 0] := Color1;
            Result.Canvas.Pixels[I + iSpacing, 0] := Color1;
            Result.Canvas.Pixels[I + 2 * iSpacing, 0] := Color1;
          end;
        end;

    wpTripleTopLines :
        // Three lines along top
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (3 x iThickness)
          // iThickness : Width of line (2)

          if iThickness < 1 then
            iThickness := 2;
          if iSpacing < 1 then
            iSpacing := iThickness * 3;
          _InitBitmap(1, Full_Page_Wallpaper_Height);
          for I := 0 to iThickness - 1 do
          begin
            Result.Canvas.Pixels[0, I] := Color1;
            Result.Canvas.Pixels[0, I + iSpacing] := Color1;
            Result.Canvas.Pixels[0, I + 2 * iSpacing] := Color1;
          end;
        end;

    wpTripleVariedLeftLines :
        // Three lines of decreasing widths on left
        begin           
          // iSize      : Not used
          // iSpacing   : Space between lines (3 x iThickness)
          // iThickness : Width of thinnest line (2)

          if iThickness < 1 then
            iThickness := 2;
          if iSpacing < 1 then
            iSpacing := iThickness * 3;
          _InitBitmap(Full_Page_Wallpaper_Width, 1);
          for I := 0 to iThickness - 1 do
          begin
            // Line 1
            Result.Canvas.Pixels[I, 0] := Color1;
            Result.Canvas.Pixels[I + iThickness, 0] := Color1;
            Result.Canvas.Pixels[I + 2 * iThickness, 0] := Color1;

            // Line 2
            Result.Canvas.Pixels[I + 2 * iThickness + iSpacing, 0] := Color1;
            Result.Canvas.Pixels[I + 3 * iThickness + iSpacing, 0] := Color1;

            // Line 3
            Result.Canvas.Pixels[I + 3 * iThickness + 2 * iSpacing, 0] := Color1;
          end;
        end;

    wpTripleVariedTopLines :
        // Three lines of decreasing widths along top
        begin   
          // iSize      : Not used
          // iSpacing   : Not used
          // iThickness : Not used

          if iThickness < 1 then
            iThickness := 2;
          if iSpacing < 1 then
            iSpacing := iThickness * 3;
          _InitBitmap(1, Full_Page_Wallpaper_Height);
          for I := 0 to iThickness - 1 do
          begin
            // Line 1
            Result.Canvas.Pixels[0, I] := Color1;
            Result.Canvas.Pixels[0, I + iThickness] := Color1;
            Result.Canvas.Pixels[0, I + 2 * iThickness] := Color1;

            // Line 2
            Result.Canvas.Pixels[0, I + 2 * iThickness + iSpacing] := Color1;
            Result.Canvas.Pixels[0, I + 3 * iThickness + iSpacing] := Color1;

            // Line 3
            Result.Canvas.Pixels[0, I + 3 * iThickness + 2 * iSpacing] := Color1;
          end;
        end;

    wpLeftDashes :
        // Short lines of a uniform sizes on left
        begin
          // iSize      : Length of dash (18)
          // iSpacing   : Space between dashes (3)
          // iThickness : Thickness of dashes (1)

          if iSize < 1 then
            iSize := 18;
          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 3;
          _InitBitmap(Full_Page_Wallpaper_Width, iSpacing + iThickness);
          Result.Canvas.Pen.Color := Color1;
          for I := 0 to iThickness - 1 do
          begin
            Result.Canvas.MoveTo(0, I);
            Result.Canvas.LineTo(iSize, I);
          end;
        end;

    wpTopDashes :
        // Short lines of a uniform sizes along top
        begin
          // iSize      : Length of dash (18)
          // iSpacing   : Space between dashes (3)
          // iThickness : Thickness of dashes (1)

          if iSize < 1 then
            iSize := 18;
          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 3;
          _InitBitmap(iSpacing + iThickness, Full_Page_Wallpaper_Height);
          Result.Canvas.Pen.Color := Color1;
          for I := 0 to iThickness - 1 do
          begin
            Result.Canvas.MoveTo(I, 0);
            Result.Canvas.LineTo(I, iSize);
          end;
        end;

    wpDoubleLeftDashes :
        // Short lines of two sizes on left
        begin
          // iSize      : Length of longest dash (15)
          // iSpacing   : Space between dashes (3)
          // iThickness : Thickness of dashes (1)

          if iSize < 1 then
            iSize := 15;
          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 3;
          _InitBitmap(Full_Page_Wallpaper_Width, 2 * (iSpacing + iThickness));
          Result.Canvas.Pen.Color := Color1;
          for I := 0 to iThickness - 1 do
          begin
            // Short Line
            Result.Canvas.MoveTo(0, I);
            Result.Canvas.LineTo(MulDiv(iSize, 2, 3), I);

            // Long Line
            Result.Canvas.MoveTo(0, I + iSpacing + iThickness);
            Result.Canvas.LineTo(iSize, I + iSpacing + iThickness);
          end;
        end;

    wpDoubleTopDashes :
        // Short lines of two sizes along top
        begin
          // iSize      : Length of longest dash (15)
          // iSpacing   : Space between dashes (3)
          // iThickness : Thickness of dashes (1)

          if iSize < 1 then
            iSize := 15;
          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 3;
          _InitBitmap(2 * (iSpacing + iThickness), Full_Page_Wallpaper_Height);
          Result.Canvas.Pen.Color := Color1;
          for I := 0 to iThickness - 1 do
          begin
            // Short Line
            Result.Canvas.MoveTo(I, 0);
            Result.Canvas.LineTo(I, MulDiv(iSize, 2, 3));
            
            // Long Line
            Result.Canvas.MoveTo(I + iSpacing + iThickness, 0);
            Result.Canvas.LineTo(I + iSpacing + iThickness, iSize);
          end;
        end;

    wpTripleLeftDashes :
        // Short lines of three sizes on left
        begin
          // iSize      : Length of longest dash (15)
          // iSpacing   : Space between dashes (4)
          // iThickness : Thickness of dashes (1)

          if iSize < 1 then
            iSize := 15;
          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 4;
          _InitBitmap(Full_Page_Wallpaper_Width, 4 * (iSpacing + iThickness));
          Result.Canvas.Pen.Color := Color1;
          for I := 0 to iThickness - 1 do
          begin
            // Short Line
            Result.Canvas.MoveTo(0, I);
            Result.Canvas.LineTo(MulDiv(iSize, 1, 3), I);

            // Middle Line
            Result.Canvas.MoveTo(0, I + iSpacing + iThickness);
            Result.Canvas.LineTo(MulDiv(iSize, 2, 3), I + iSpacing + iThickness);

            // Long Line
            Result.Canvas.MoveTo(0, I + 2 * (iSpacing + iThickness));
            Result.Canvas.LineTo(iSize, I + 2 * (iSpacing + iThickness));

            // Second Middle Line
            Result.Canvas.MoveTo(0, I + 3 * (iSpacing + iThickness));
            Result.Canvas.LineTo(MulDiv(iSize, 2, 3), I + 3 * (iSpacing + iThickness));
          end;
        end;

    wpTripleTopDashes :
        // Short lines of three sizes along top
        begin
          // iSize      : Length of longest dash (15)
          // iSpacing   : Space between dashes (4)
          // iThickness : Thickness of dashes (1)

          if iSize < 1 then
            iSize := 15;
          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 4;
          _InitBitmap(4 * (iSpacing + iThickness), Full_Page_Wallpaper_Height);
          Result.Canvas.Pen.Color := Color1;
          for I := 0 to iThickness - 1 do
          begin
            // Short Line
            Result.Canvas.MoveTo(I, 0);
            Result.Canvas.LineTo(I, MulDiv(iSize, 1, 3));

            // Middle Line
            Result.Canvas.MoveTo(I + iSpacing + iThickness, 0);
            Result.Canvas.LineTo(I + iSpacing + iThickness, MulDiv(iSize, 2, 3));

            // Long Line
            Result.Canvas.MoveTo(I + 2 * (iSpacing + iThickness), 0);
            Result.Canvas.LineTo(I + 2 * (iSpacing + iThickness), iSize);

            // Second Middle Line
            Result.Canvas.MoveTo(I + 3 * (iSpacing + iThickness), 0);
            Result.Canvas.LineTo(I + 3 * (iSpacing + iThickness), MulDiv(iSize, 2, 3));
          end;
        end;

    wpLeftZigZag :
        // Shark tooth pattern on left
        begin
          // iSize      : The solid area of the border (10)
          // iSpacing   : The height of the "peak" (15)
          // iThickness : Not used

          if iSize < 1 then
            iSize := 10;
          if iSpacing < 1 then
            iSpacing := 15;
          _InitBitmap(Full_Page_Wallpaper_Width, 2 * iSpacing);
          Result.Canvas.Pen.Color := Color1;
          for I := 0 to 2 * iSpacing - 1 do
          begin
            Result.Canvas.MoveTo(0, I);
            if I <= iSpacing then
              Result.Canvas.LineTo(iSize + I, I)
            else
              Result.Canvas.LineTo(iSize + 2 * iSpacing - I, I);
          end;
        end;

    wpTopZigZag :
        // Shark tooth pattern along top
        begin
          // iSize      : The solid area of the border (10)
          // iSpacing   : The height of the "peak" (15)
          // iThickness : Not used

          if iSize < 1 then
            iSize := 10;
          if iSpacing < 1 then
            iSpacing := 15;
          // iSize      : Not used
          // iSpacing   : Not used
          // iThickness : Not used
          _InitBitmap(2 * iSpacing, Full_Page_Wallpaper_Height);
          Result.Canvas.Pen.Color := Color1;
          for I := 0 to 2 * iSpacing - 1 do
          begin
            Result.Canvas.MoveTo(I, 0);
            if I <= iSpacing then
              Result.Canvas.LineTo(I, iSize + I)
            else
              Result.Canvas.LineTo(I, iSize + 2 * iSpacing - I);
          end;
        end;

    wpLeftGradient :
        // Color gradient on left
        begin
          // iSize      : Width of gradient area (20)
          // iSpacing   : Not used
          // iThickness : Not used

          if iSize < 1 then
            iSize := 30;
          _InitBitmap(Full_Page_Wallpaper_Width, Small_Height);
          IEDrawGradient(Rect(0, 0, iSize, Result.Height), Result.Canvas.handle, Color1, Color2, False);
        end;

    wpTopGradient :
        // Color gradient along top
        begin
          // iSize      : Width of gradient area (20)
          // iSpacing   : Not used
          // iThickness : Not used

          if iSize < 1 then
            iSize := 30;
          _InitBitmap(Small_Width, Full_Page_Wallpaper_Height);
          IEDrawGradient(Rect(0, 0, Result.Width, iSize), Result.Canvas.handle, Color1, Color2, True);
        end;

    wpHorzPinStripe :
        // Horizontal lines across image
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (10)
          // iThickness : Thickness of lines (1)

          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 10;
          iSize := iSpacing + iThickness;
          _InitBitmap(Small_Width, iSize);
          Result.Canvas.Pen.Color := Color1;
          for I := 1 to iThickness do
          begin
            Result.Canvas.MoveTo(0, iSize - I);
            Result.Canvas.LineTo(Small_Width, iSize - I);
          end;
        end;

    wpVertPinStripe :
        // Vertical lines down image
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (10)
          // iThickness : Thickness of lines (1)

          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 10;
          iSize := iSpacing + iThickness;
          _InitBitmap(iSize, Small_Height);
          Result.Canvas.Pen.Color := Color1;
          for I := 1 to iThickness do
          begin
            Result.Canvas.MoveTo(iSize - I, 0);
            Result.Canvas.LineTo(iSize - I, Small_Height);
          end;
        end;

    wpDiagPinStripe, wpDiagPinStripe2 :
        // Diagonal lines across image
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (20)
          // iThickness : Thickness of lines. Always 1

          iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 20;
          iSize := iSpacing + iThickness;
          _InitBitmap(iSize, iSize);
          Result.Canvas.Pen.Color := Color1;
          Result.Canvas.Pen.Width := iThickness;
          
          if Effect = wpDiagPinStripe then
          begin
            Result.Canvas.MoveTo(0, 0);
            Result.Canvas.LineTo(iSize, iSize);
          end
          else
          begin
            Result.Canvas.MoveTo(iSize - 1, 0);
            Result.Canvas.LineTo(-1, iSize);
          end;
        end;

    wpCrossHatch :
        // Horizontal and vertical lines
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (14)
          // iThickness : Thickness of lines (1)

          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 14;
          iSize := iSpacing + iThickness;
          _InitBitmap(iSize, iSize);
          Result.Canvas.Pen.Color := Color1;
          for I := 1 to iThickness do
          begin
            // Horizontal
            Result.Canvas.MoveTo(0, iSize - I);
            Result.Canvas.LineTo(iSize, iSize - I);

            // Vertical
            Result.Canvas.MoveTo(iSize - I, 0);
            Result.Canvas.LineTo(iSize - I, iSize);
          end;
        end;

    wpDiagCrossHatch :
        // Diagonal lines in two directions
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (25). Always odd
          // iThickness : Thickness of lines. Always 1

          iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 25;
          iSpacing := iSpacing div 2 * 2 + 1; // must be odd
          iSize := iSpacing + iThickness;
          _InitBitmap(iSize, iSize);
          Result.Canvas.Pen.Color := Color1;
          Result.Canvas.Pen.Width := iThickness;
          Result.Canvas.MoveTo(0, 0);
          Result.Canvas.LineTo(iSize, iSize);
          Result.Canvas.MoveTo(iSize, 0);
          Result.Canvas.LineTo(0, iSize);
        end;

    wpHorzStripes :
        // Horizontal stripes across image
        begin
          // iSize      : Same as two * iThickness
          // iSpacing   : Not used
          // iThickness : Width of stripes (60)

          if iThickness < 1 then
            iThickness := iSize div 2;
          if iThickness < 1 then
            iThickness := 60;
          _InitBitmap(iThickness * 2, 1);
          Result.Canvas.Brush.Style := bsSolid;
          Result.Canvas.Brush.Color := Color1;
          Result.Canvas.FillRect(Rect(iThickness, 0, 2 * iThickness, iThickness));
        end;

    wpVertStripes :
        // Vertical stripes down image
        begin
          // iSize      : Same as two * iThickness
          // iSpacing   : Not used
          // iThickness : Width of stripes (60)

          if iThickness < 1 then
            iThickness := iSize div 2;
          if iThickness < 1 then
            iThickness := 60;
          _InitBitmap(1, iThickness * 2);
          Result.Canvas.Brush.Style := bsSolid;
          Result.Canvas.Brush.Color := Color1;
          Result.Canvas.FillRect(Rect(0, iThickness, iThickness, 2 * iThickness));
        end;

    wpDiagStripes, wpDiagStripes2 :
        // Diagonal stripes down image
        begin
          // iSize      : Same as two * iThickness
          // iSpacing   : Not used
          // iThickness : Width of stripes (60)

          if iThickness < 1 then
            iThickness := iSize div 2;
          if iThickness < 1 then
            iThickness := 60;
          _InitBitmap(2 * iThickness, 2 * iThickness);
          Result.Canvas.Pen.Style := psClear;
          Result.Canvas.Brush.Style := bsSolid;
          Result.Canvas.Brush.Color := Color1;
          
          if Effect = wpDiagStripes then
          begin
            Result.Canvas.Polygon([ Point(0, 0),
                                    Point(2 * iThickness, 2 * iThickness),
                                    Point(iThickness, 2 * iThickness),
                                    Point(0, iThickness) ]);
            Result.Canvas.Polygon([ Point(iThickness, 0),
                                    Point(2 * iThickness, 0),
                                    Point(2 * iThickness, iThickness) ]);
          end
          else
          begin
            Result.Canvas.Polygon([ Point(2 * iThickness, 0),
                                    Point(0, 2 * iThickness),
                                    Point(iThickness, 2 * iThickness),
                                    Point(2 * iThickness, iThickness) ]);
            Result.Canvas.Polygon([ Point(iThickness, 0),
                                    Point(0, 0),
                                    Point(0, iThickness) ]);
          end
        end;

    wpCheckers :
        // Alternativing blocks of color
        begin
          // iSize      : Same as two * iThickness
          // iSpacing   : Not used
          // iThickness : Width of stripes (60)

          if iThickness < 1 then
            iThickness := iSize div 2;
          if iThickness < 1 then
            iThickness := 60;
          _InitBitmap(iThickness * 2, iThickness * 2);
          Result.Canvas.Brush.Style := bsSolid;
          Result.Canvas.Brush.Color := Color1;
          Result.Canvas.FillRect(Rect(iThickness, 0, 2 * iThickness, iThickness));
          Result.Canvas.FillRect(Rect(0, iThickness, iThickness, 2 * iThickness));
        end;

    wpDiagCheckers :
        // Alternativing diagonal blocks of color
        begin
          // iSize      : Same as two * iThickness
          // iSpacing   : Not used
          // iThickness : Width of stripes (60)

          if iThickness < 1 then
            iThickness := iSize div 2;
          if iThickness < 1 then
            iThickness := 60;
          _InitBitmap(iThickness * 2, iThickness * 2);
          Result.Canvas.Pen.Style := psSolid;   
          Result.Canvas.Pen.Color := Color1;
          Result.Canvas.Brush.Style := bsSolid;
          Result.Canvas.Brush.Color := Color1;
          Result.Canvas.Polygon([ Point(iThickness, 0),
                                  Point(2 * iThickness, iThickness),
                                  Point(iThickness, 2 * iThickness),
                                  Point(0, iThickness) ]);
        end;

    wpRain, wpRain2 :
        // Diagonal dashes
        begin
          // iSize      : Not used
          // iSpacing   : Space between dashes (20)
          // iThickness : Thickness of dashes (5)

          if iThickness < 1 then
            iThickness := 5;
          if iSpacing < 1 then
            iSpacing := 20;
          iSize := iSpacing + iThickness;
          _InitBitmap(iSize, iSize);
          Result.Canvas.Pen.Color := Color1;
          Result.Canvas.Pen.Width := iThickness;
          if Effect = wpRain then
          begin
            Result.Canvas.MoveTo(MulDiv(iSize, 1, 5), MulDiv(iSize, 1, 5));
            Result.Canvas.LineTo(MulDiv(iSize, 4, 5), MulDiv(iSize, 4, 5));
          end
          else
          begin
            Result.Canvas.MoveTo(MulDiv(iSize, 4, 5), MulDiv(iSize, 1, 5));
            Result.Canvas.LineTo(MulDiv(iSize, 1, 5), MulDiv(iSize, 4, 5));
          end
        end;

      wpRivets :
        // Circles over page 
        begin
          // iSize      : Size of circle, Min: 4 (5)
          // iSpacing   : Spacing between shapes, Min: iSize Div 2 (3)
          // iThickness : not used

          if iSize < 1 then
            iSize := 5;
          if iSize < 4 then
            iSize := 4;
          if iSpacing < 1 then
            iSpacing := 3;
          if iSpacing < iSize div 2 + 1 then
            iSpacing := iSize div 2 + 1;
          _InitBitmap(2 * iSpacing + 2 * iSize , 2 * iSize);
          Result.Canvas.Pen.Style := psClear;
          Result.Canvas.Brush.Style := bsClear;
          Result.Canvas.Brush.Color := Color1;
          Result.Canvas.Ellipse(iSpacing, 0,
                                iSpacing + iSize, iSize);
          Result.Canvas.Ellipse(2 * iSpacing + iSize, iSize,
                                2 * iSpacing + 2 * iSize, 2 * iSize);
        end;

      wpHearts :
        // Hearts over page
        begin
          // iSize      : Size of heart, Min: 5 (78)
          // iSpacing   : Spacing between shapes, Min: iSize Div 2 (5)
          // iThickness : not used

          if iSize < 1 then
            iSize := 78;
          if iSize < 4 then
            iSize := 4;
          if iSpacing < 1 then
            iSpacing := 5;
          if iSpacing < iSize div 2 + 1 then
            iSpacing := iSize div 2 + 1;
          _InitBitmap(2 * iSpacing + 2 * iSize , 2 * iSize);
          Result.Canvas.Pen.Style   := psClear;
          Result.Canvas.Brush.Style := bsClear;
          Result.Canvas.Brush.Color := Color1;
          DrawCustomShape(Result.Canvas, xcsHeart, iSpacing, 0, iSize, iSize);
          DrawCustomShape(Result.Canvas, xcsHeart, 2 * iSpacing + iSize, iSize, iSize, iSize);
        end;

      wpStars ,                 // Stars over page
      wpLightning :             // Lightning over page
        begin
          // iSize      : Size of star/lightning, Min: 5 (50)
          // iSpacing   : Spacing between shapes (10)
          // iThickness : not used

          if iSize < 1 then
            iSize := 50;
          if iSize < 5 then
            iSize := 5;
          if iSpacing < 1 then
            iSpacing := 10;
          _InitBitmap(iSize + iSpacing, iSize + iSpacing);
          Result.Canvas.Pen.Style   := psClear;
          Result.Canvas.Brush.Style := bsClear;
          Result.Canvas.Brush.Color := Color1;
          if Effect = wpStars then
            AShape := xcsStar5
          else
            AShape := xcsLightningLeft;
          DrawCustomShape(Result.Canvas, AShape, iSpacing div 2, iSpacing div 2, iSize, iSize);
        end;

      wpRuledPage:
        // Similar to standard ruled page (with red line along side)
        begin
          // iSize      : Not used
          // iSpacing   : Space between lines (24)
          // iThickness : Thickness of lines (1)

          if iThickness < 1 then
            iThickness := 1;
          if iSpacing < 1 then
            iSpacing := 24;
          iSize := iSpacing + iThickness;
          _InitBitmap(Full_Page_Wallpaper_Width, iSize);
          Result.Canvas.Pen.Color := Color1;
          for I := 1 to iThickness do
          begin
            Result.Canvas.MoveTo(0, iSize - I);
            Result.Canvas.LineTo(Full_Page_Wallpaper_Width, iSize - I);
          end;       
          Result.Canvas.Pen.Color := clRed;
          for I := 1 to iThickness do
          begin
            Result.Canvas.MoveTo(iSpacing * 3 + I, 0);
            Result.Canvas.LineTo(iSpacing * 3 + I, iSize);
          end;  
        end;

  end;
end;

               
// Fill the canvas by tiling the specified bitmap (i.e. for wallpaper type effects)
Procedure TileBitmapOntoCanvas(Canvas : TCanvas; iWidth, iHeight : Integer; Bitmap : TBitmap);
var
  X, Y: LongInt;
begin
  // Avoid stack overflow
  if not assigned(Bitmap) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    raise Exception.create('Invalid Source Bitmap');

  // tile the image onto the  bitmap canvas
  Y := 0;
  while Y < iHeight do
  begin
    X := 0;
    while X < iWidth do
    begin
      Canvas.Draw(X, Y, Bitmap);
      Inc(X, Bitmap.Width);
    end;
    Inc(Y, Bitmap.Height);
  end;
end;

    

{!!
<FS>iexCanvasUtils

<FN>iexCanvasUtils.pas provides helper functions adding images and shapes to TCanvas.

!!}


end.
