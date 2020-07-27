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


{------------------------------------------------------------------------------}
{                                                                              }
{  Some transitions based on TCustomPicShow v4.10 (PSEffect.pas) by:           }
{                                                                              }
{  Kambiz R. Khojasteh                                                         }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

(*
File version 1012
*)

{$I ie.inc}

{$R-}

unit iexTransitions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, hyieutils, ieview, ExtCtrls, hyiedefs;

type


{!!
<FS>TIETransitionType

<FM>Declaration<FC>
}
  TIETransitionType = (iettNone,
                       iettCrossDissolve,                          // Cross Fade
                       iettFadeOut,                                // Fade Out
                       iettFadeIn,                                 // Fade In
                       iettFadeOutIn,                              // Fade Out then In
                       iettLeftRight1,                             // Wipe Left to Right
                       iettLeftRight2,                             // Wipe Left to Right 2
                       iettRightLeft1,                             // Wipe Right to Left
                       iettRightLeft2,                             // Wipe Right to Left 2
                       iettUpDown1,                                // Wipe Top to Bottom
                       iettUpDown2,                                // Wipe Top to Bottom 2
                       iettDownUp1,                                // Wipe Bottom to Top
                       iettDownUp2,                                // Wipe Bottom to Top 2
                       iettFromUpLeft,                             // Slide from Top Left
                       iettFromUpRight,                            // Slide from Top Right
                       iettFromBottomLeft,                         // Slide from Bottom Left
                       iettFromBottomRight,                        // Slide from Bottom Right
                       iettMoveLeftRight1,                         // Push Left to Right
                       iettMoveLeftRight2,                         // Slide Out Left to Right
                       iettMoveRightLeft1,                         // Push Right to Left
                       iettMoveRightLeft2,                         // Slide Out Right to Left
                       iettMoveUpDown1,                            // Push Top to Bottom
                       iettMoveUpDown2,                            // Slide Out Top to Bottom
                       iettMoveDownUp1,                            // Push Bottom to Top
                       iettMoveDownUp2,                            // Slide Out Bottom to Top
                       iettRandomPoints,                           // Random Points
                       iettRandomBoxes,                            // Random Boxes
                       iettCenter1,                                // Wipe Out from Center
                       iettCenter2,                                // Wipe In to Center
                       iettCenterZoom1,                            // Expand Out from Center
                       iettCenterZoom2                             // Expand In to Center

                       {$ifdef IEINCLUDEEXTRATRANSITIONS}
                       ,iettExpandFromLeft,                        // Expand from Left
                       iettExpandFromRight,                        // Expand from Right
                       iettExpandFromTop,                          // Expand from Top
                       iettExpandFromBottom,                       // Expand from Bottom
                       iettExpandFromTopLeft,                      // Expand from Top Left
                       iettExpandFromTopRight,                     // Expand from Top Right
                       iettExpandFromBottomLeft,                   // Expand from Bottom Left
                       iettExpandFromBottomRight,                  // Expand from Bottom Right
                       iettExpandInFromLeft,                       // Expand in from Left
                       iettExpandInFromRight,                      // Expand in from Right
                       iettExpandInFromTop,                        // Expand in from Top
                       iettExpandInFromBottom,                     // Expand in from Bottom
                       iettExpandInToVerticalCenter,               // Expand in to Vertical Center
                       iettExpandInToHorizon,                      // Expand in to Horizon
                       iettExpandInFromSides,                      // Expand in from Sides
                       iettExpandInFromTopAndBottom,               // Expand in from Top and Bottom
                       iettExpandOutFromHorizon,                   // Expand out from Horizon
                       iettExpandOutFromVerticalCenter,            // Expand out from Vertical Center
                       iettWipeFromTopLeft,                        // Wipe from Top Left
                       iettWipeFromTopRight,                       // Wipe from Top Right
                       iettWipeFromBottomLeft,                     // Wipe from Bottom Left
                       iettWipeFromBottomRight,                    // Wipe from Bottom Right
                       iettWipeInFromTopAndBottom,                 // Wipe in from Top and Bottom
                       iettWipeFromHorizon,                        // Wipe from Horizon
                       iettWipeInFromSides,                        // Wipe in from Sides
                       iettWipeOutFromVerticalCenter,              // Wipe out from Vertical Center
                       iettBuildUpFromLeft,                        // Build up from Left
                       iettBuildUpFromRight,                       // Build up from Right
                       iettBuildUpFromTop,                         // Build up from Top
                       iettBuildUpFromBottom,                      // Build up from Bottom
                       iettUnrollFromLeft,                         // Unroll from Left
                       iettUnrollFromRight,                        // Unroll from Right
                       iettUnrollFromTop,                          // Unroll from Top
                       iettUnrollFromBottom,                       // Unroll from Bottom
                       iettSlideInFromLeft,                        // Slide in from Left
                       iettSlideInFromRight,                       // Slide in from Right
                       iettSlideInFromTop,                         // Slide in from Top
                       iettSlideInFromBottom,                      // Slide in from Bottom
                       iettShrinkToTopLeft,                        // Shrink to Top Left
                       iettShrinkToTopRight,                       // Shrink to Top Right
                       iettShrinkToBottomLeft,                     // Shrink to Bottom Left
                       iettShrinkToBottomRight,                    // Shrink to Bottom Right
                       iettShrinkToCenter,                         // Shrink to Center
                       iettQuartersWipeInToCenter,                 // Quarters Wipe in to Center
                       iettQuartersExpandToCenter,                 // Quarters Expand to Center
                       iettQuartersSlideInToCenter,                // Quarters Slide in to Center
                       iettCurvedWipeFromLeft,                     // Curved Wipe from Left
                       iettCurvedWipeFromRight,                    // Curved Wipe from Right
                       iettCurvedWipeFromTop,                      // Curved Wipe from Top
                       iettCurvedWipeFromBottom,                   // Curved Wipe from Bottom
                       iettCurvedWipeFromTopLeft,                  // Curved Wipe from Top Left
                       iettCurvedWipeFromTopRight,                 // Curved Wipe from Top Right
                       iettCurvedWipeFromBottomLeft,               // Curved Wipe from Bottom Left
                       iettCurvedWipeFromBottomRight,              // Curved Wipe from Bottom Right
                       iettBarsInFromLeft,                         // Bars in from Left
                       iettBarsInFromRight,                        // Bars in from Right
                       iettBarsFromTop,                            // Bars from Top
                       iettBarsFromBottom,                         // Bars from Bottom
                       iettBarsLeftThenRight,                      // Bars Left then Right
                       iettBarsRightThenLeft,                      // Bars Right then Left
                       iettBarsTopThenBottom,                      // Bars Top then Bottom
                       iettBarsBottomThenTop,                      // Bars Bottom then Top
                       iettBarsFromBothSides,                      // Bars from both Sides
                       iettBarsFromTopAndBottom,                   // Bars from Top and Bottom
                       iettShreddedFromLeft,                       // Shredded from Left
                       iettShreddedFromRight,                      // Shredded from Right
                       iettShreddedFromTop,                        // Shredded from Top
                       iettShreddedFromBottom,                     // Shredded from Bottom
                       iettShreddedFromTopAndLeft,                 // Shredded from Top and Left
                       iettShreddedFromTopAndRight,                // Shredded from Top and Right
                       iettShreddedFromBottomAndLeft,              // Shredded from Bottom and Left
                       iettShreddedFromBottomAndRight,             // Shredded from Bottom and Right
                       iettShreddedFromHorizonAndLeft,             // Shredded from Horizon and Left
                       iettShreddedFromHorizonAndRight,            // Shredded from Horizon and Right
                       iettShreddedFromTopAndVerticalCenter,       // Shredded from Top and Vertical Center
                       iettShreddedFromBottomAndVerticalCenter,    // Shredded from Bottom and Vertical Center
                       iettShreddedFromCenter,                     // Shredded from Center
                       iettShreddedToCenter,                       // Shredded to Center
                       iettShreddedInToHorizon,                    // Shredded in to Horizon
                       iettShreddedInToVerticalCenter,             // Shredded in to Vertical Center
                       iettShreddedOutFromHorizon,                 // Shredded out from Horizon
                       iettShreddedOutFromVerticalCenter,          // Shredded out from Vertical Center
                       iettExpandingRectangles,                    // Expanding Rectangles
                       iettExpandingTriangles,                     // Expanding Triangles
                       iettExpandingCircles,                       // Expanding Circles
                       iettExpandingDiamonds,                      // Expanding Diamonds
                       iettCircularWipeFromCenter,                 // Circular Wipe from Center
                       iettCircularWipeToCenter,                   // Circular Wipe to Center
                       iettCrisscrossWipeFromTopLeft,              // Crisscross Wipe from Top Left
                       iettCrisscrossWipeFromTopRight,             // Crisscross Wipe from Top Right
                       iettCrisscrossWipeFromBottomLeft,           // Crisscross Wipe from Bottom Left
                       iettCrisscrossWipeFromBottomRight,          // Crisscross Wipe from Bottom Right
                       iettCrisscrossWipeBounceFromTopLeft,        // Crisscross Wipe Bounce from Top Left
                       iettCrisscrossWipeBounceFromTopRight,       // Crisscross Wipe Bounce from Top Right
                       iettCrisscrossWipeBounceFromBottomLeft,     // Crisscross Wipe Bounce from Bottom Left
                       iettCrisscrossWipeBounceFromBottomRight,    // Crisscross Wipe Bounce from Bottom Right
                       iettCrisscrossWipeFromLeftRightAndTop,      // Crisscross Wipe from Left Right and Top
                       iettCrisscrossWipeFromLeftRightAndBottom,   // Crisscross Wipe from Left Right and Bottom
                       iettCrisscrossWipeFromLeftTopAndBottom,     // Crisscross Wipe from Left Top and Bottom
                       iettCrisscrossWipeFromTopLeftRightAndBottom,// Crisscross Wipe from Top Left Right and Bottom
                       iettCrisscrossWipeFromRightTopAndBottom,    // Crisscross Wipe from Right Top and Bottom
                       iettCrisscrossWipeFromBottomLeftTopRight,   // Crisscross Wipe from Bottom Left Top Right
                       iettWipeDiagonalFromTopLeft,                // Wipe diagonal from Top Left
                       iettWipeDiagonalFromTopRight,               // Wipe diagonal from Top Right
                       iettWipeDiagonalFromBottomLeft,             // Wipe diagonal from Bottom Left
                       iettWipeDiagonalFromBottomRight,            // Wipe diagonal from Bottom Right
                       iettDiagonalSweepClockwise,                 // Diagonal Sweep Clockwise
                       iettDiagonalSweepCounterClockwise,          // Diagonal Sweep Counter-Clockwise
                       iettSweepClockwise,                         // Half Sweep Clockwise
                       iettSweepCounterClockwise,                  // Half Sweep Counter-Clockwise
                       iettStarburstClockwiseFromCenter,           // Starburst Clockwise from Center
                       iettStarburstCounterClockwiseFromCenter,    // Starburst Clockwise from Center
                       iettRotationalRectangle,                    // Rotational Rectangle - Clockwise
                       iettRotationalRectangleCounterClockwise,    // Rotational Rectangle - Counter Clockwise
                       iettRotationalStar,                         // Rotational Star - Clockwise
                       iettRotationalStarCounterClockwise,         // Rotational Star - Counter Clockwise
                       iettSpeckledWipeFromLeft,                   // Speckled Wipe from Left
                       iettSpeckledWipeFromRight,                  // Speckled Wipe from Right
                       iettSpeckledWipeFromTop,                    // Speckled Wipe from Top
                       iettSpeckledWipeFromBottom,                 // Speckled Wipe from Bottom
                       iettPushLeftAndSlideOut,                    // Push Left and Slide out
                       iettPushRightAndSlideOut,                   // Push Right and Slide out
                       iettPushUpAndSlideOut,                      // Push up and Slide out
                       iettPushDownAndSlideOut,                    // Push down and Slide out
                       iettPushAndSqueezeLeft,                     // Push and Squeeze Left
                       iettPushAndSqueezeRight,                    // Push and Squeeze Right
                       iettPushAndSqueezeUp,                       // Push and Squeeze up
                       iettPushAndSqueezeDown,                     // Push and Squeeze down
                       iettHorizontalBlinds,                       // Horizontal Blinds
                       iettVerticalBlinds,                         // Vertical Blinds
                       iettUnevenBlindsFromLeft,                   // Uneven Blinds from Left
                       iettUnevenBlindsFromRight,                  // Uneven Blinds from Right
                       iettUnevenBlindsFromTop,                    // Uneven Blinds from Top
                       iettUnevenBlindsFromBottom,                 // Uneven Blinds from Bottom
                       iettRectanglesFromTheLeft,                  // Random Bars from the Left
                       iettRectanglesFromTheRight,                 // Random Bars from the Right
                       iettRectanglesFromTheTop,                   // Random Bars from the Top
                       iettRectanglesFromTheBottom,                // Random Bars from the Bottom
                       iettSpirallingRectangleClockwise,           // Spiralling Rectangle Clockwise
                       iettSpirallingRectangleCounterClockwise,    // Spiralling Rectangle Counter-Clockwise
                       iettArrowWipeFromLeft,                      // Arrow Wipe from Left
                       iettArrowWipeFromRight,                     // Arrow Wipe from Right
                       iettArrowWipeFromTop,                       // Arrow Wipe from Top
                       iettArrowWipeFromBottom,                    // Arrow Wipe from Bottom
                       iettHorizontalBowTieWipe,                   // Horizontal Bow Tie Wipe
                       iettVerticalBowTieWipe,                     // Vertical Bow Tie Wipe
                       iettDiagonalCrossFromCenter,                // Diagonal Cross from Center
                       iettDiagonalCrossToCenter,                  // Diagonal Cross to Center
                       iettZigzagWipeFromHorizon,                  // Zigzag Wipe from Horizon
                       iettZigzagWipeFromVerticalCenter,           // Zigzag Wipe from Vertical Center
                       iettDiamondWipeFromCenter,                  // Diamond Wipe from Center
                       iettDiamondWipeToCenter,                    // Diamond Wipe to Center
                       iettDiagonalBoxWipe,                        // Diagonal Box Wipe
                       iettTriangularWipe,                         // Rotational Triangular Wipe
                       iettRandomBigBoxes,                         // Random Big Boxes
                       iettPageFlip,                               // Page Flip
                       iettPageFlip2,                              // Page Flip 2
                       iettReversePageFlip,                        // Page Flip
                       iettReversePageFlip2,                       // Page Flip 2
                       iettZigzagWipeToHorizon,                    // Zigzag Wipe To Horizon
                       iettZigzagWipeToVerticalCenter,             // Zigzag Wipe To Vertical Center
                       iettRandomHearts,                           // Random Hearts
                       iettRandomStar5s,                           // Random 5 Pointed Stars
                       iettRandomStar6s,                           // Random 6 Pointed Stars
                       iettRandomExplosions,                       // Random Explosions
                       iettExpandingHearts,                        // Expanding Hearts
                       iettExpandingStar5,                         // Expanding 5 Pointed Stars
                       iettExpandingStar6,                         // Expanding 6 Pointed Stars
                       iettExpandingExplosions,                    // Expanding Explosions
                       iettExpandingLightningBolts,                // Expanding Lightning Bolts
                       iettHeartWipeOut,                           // Heart Wipe from Center
                       iettHeartWipeIn,                            // Heart Wipe to Center
                       iettStar5WipeOut,                           // 5 Pointed Star Wipe from Center
                       iettStar5WipeIn,                            // 5 Pointed Star Wipe to Center
                       iettStar6WipeOut,                           // 6 Pointed Star Wipe from Center
                       iettStar6WipeIn,                            // 6 Pointed Star Wipe to Center
                       iettExplosionWipeOut,                       // Explosion Wipe from Center
                       iettExplosionWipeIn,                        // Explosion Wipe to Center
                       iettCrossWipeOut,                           // Cross Wipe from Center
                       iettCrossWipeIn,                            // Cross Wipe to Center
                       iettHeartWipeInAndOut,                      // Heart Wipe In and Out
                       iettStar5WipeInAndOut,                      // 5 Pointed Star Wipe In and Out
                       iettStar6WipeInAndOut,                      // 6 Pointed Star Wipe In and Out
                       iettExplosionWipeInAndOut,                  // Explosion Wipe In and Out
                       iettCubeRotateFromLeft,                     // Cube Rotate from Left
                       iettCubeRotateFromRight,                    // Cube Rotate from Right
                       iettCubeRotateFromTop,                      // Cube Rotate from Top
                       iettCubeRotateFromBottom,                   // Cube Rotate from Bottom
                       iettSoftWipeFromLeft,                       // Soft Wipe from Left
                       iettSoftWipeFromRight,                      // Soft Wipe from Right
                       iettSoftWipeFromTop,                        // Soft Wipe from Top
                       iettSoftWipeFromBottom,                     // Soft Wipe from Bottom
                       iettAngledTwistIn,                          // Twist In
                       iettAngledTwistOut,                         // Twist Out
                       iettMultipleAngledTwistIn,                  // Multiple Twist In
                       iettMultipleAngledTwistOut,                 // Multiple Twist Out
                       iettRandomPuzzlePieces,                     // Random Puzzle Pieces
                       iettPacmanFromLeft   ,                      // Pacman Devours from Left
                       iettPacmanFromRight  ,                      // Pacman Devours from Right
                       iettPacman3Row       ,                      // Pacman Devours Three Rows
                       iettPacman4Row       ,                      // Pacman Devours Four Rows
                       iettPacman2SimRow    ,                      // Two Simultaneous Rows of Pacman
                       iettPacman4SimRow    ,                      // Four Simultaneous Rows of Pacman
                       iettPacman6SimRow    ,                      // Six Simultaneous Rows of Pacman
                       iettFullSweepClockwise,                     // Full Sweep Clockwise
                       iettExpandingSweepClockwise,                // Expanding Sweep Clockwise
                       iettCubeRotateFromLeft2,                    // 3D Cube Rotate from Left
                       iettCubeRotateFromRight2,                   // 3D Cube Rotate from Right
                       iettCubeRotateFromTop2,                     // 3D Cube Rotate from Top
                       iettCubeRotateFromBottom2,                  // 3D Cube Rotate from Bottom
                       iettRandomBoxesWithWord,                    // Random Boxes with Word
                       iettRandomWord,                             // Random Letters of Word
                       iettExpandingWord,                          // Expanding Letters of Word
                       iettWordWipeOut,                            // Word Wipe from Center
                       iettWordWipeIn,                             // Word Wipe to Center
                       iettWordWipeInAndOut,                       // Word Wipe In and Out
                       iettWordHalfSweep,                          // Half Sweep with Word
                       iettWordFullSweep,                          // Full Sweep with Word
                       iettWordFullExpandingSweep                  // Expanding Sweep with Word
                       {$endif}

                       ,iettPanZoom                                // Pan Zoom/Effects
                       );


{//}
const
  // All transitions that can display a word, using the global setting, IEGlobalSettings().WordTransitionParams
  Transitions_Supporting_Word =
      [ {$ifdef IEINCLUDEEXTRATRANSITIONS}
        iettRandomBoxesWithWord,
        iettRandomWord,
        iettExpandingWord,
        iettWordWipeOut,
        iettWordWipeIn,
        iettWordWipeInAndOut,
        iettWordHalfSweep,
        iettWordFullSweep,
        iettWordFullExpandingSweep
        {$endif}
        ];
                    
  // All transitions for which the global setting, IEGlobalSettings().TransitionsDrawAlternative, will modify the transition
  Transitions_Supporting_TransitionsDrawAlternative =
      [ iettLeftRight2                                 ,
        iettRightLeft2                                 ,
        iettUpDown2                                    ,
        iettDownUp2                                    ,
        iettRandomPoints                               ,  
        iettRandomBoxes
        {$ifdef IEINCLUDEEXTRATRANSITIONS}             ,
        iettExpandFromLeft                             ,
        iettExpandFromRight                            ,
        iettExpandFromTop                              ,
        iettExpandFromBottom                           ,
        iettPacmanFromLeft                             ,
        iettPacmanFromRight                            ,
        iettPacman3Row                                 ,
        iettPacman4Row                                 ,
        iettPacman2SimRow                              ,
        iettPacman4SimRow                              ,
        iettPacman6SimRow                              ,
        iettCubeRotateFromLeft                         ,
        iettCubeRotateFromRight                        ,
        iettCubeRotateFromTop                          ,
        iettCubeRotateFromBottom                       ,
        iettCubeRotateFromLeft2                        ,
        iettCubeRotateFromRight2                       ,
        iettCubeRotateFromTop2                         ,
        iettCubeRotateFromBottom2                      ,
        iettPageFlip                                   ,
        iettReversePageFlip                            ,
        iettPageFlip2                                  ,
        iettReversePageFlip2                           ,
        iettSoftWipeFromLeft                           ,
        iettSoftWipeFromRight                          ,
        iettSoftWipeFromTop                            ,
        iettSoftWipeFromBottom                         ,
        iettSweepClockwise                             ,
        iettSweepCounterClockwise                      ,
        iettFullSweepClockwise                         ,
        iettExpandingSweepClockwise                    ,
        iettRandomPuzzlePieces                         ,
        iettRandomBoxesWithWord                        ,
        iettRandomBigBoxes                             ,
        iettTriangularWipe                             ,
        iettShreddedFromLeft                           ,
        iettShreddedFromRight                          ,
        iettShreddedFromTop                            ,
        iettShreddedFromBottom                         ,
        iettShreddedFromTopAndLeft                     ,
        iettShreddedFromTopAndRight                    ,
        iettShreddedFromBottomAndLeft                  ,
        iettShreddedFromBottomAndRight                 ,
        iettShreddedFromHorizonAndLeft                 ,
        iettShreddedFromHorizonAndRight                ,
        iettShreddedFromTopAndVerticalCenter           ,
        iettShreddedFromBottomAndVerticalCenter        ,
        iettShreddedFromCenter                         ,
        iettShreddedToCenter                           ,
        iettShreddedInToHorizon                        ,
        iettShreddedInToVerticalCenter                 ,
        iettShreddedOutFromHorizon                     ,
        iettShreddedOutFromVerticalCenter              ,
        iettBarsInFromLeft                             ,
        iettBarsInFromRight                            ,
        iettBarsFromTop                                ,
        iettBarsFromBottom                             ,
        iettBarsLeftThenRight                          ,
        iettBarsRightThenLeft                          ,
        iettBarsTopThenBottom                          ,
        iettBarsBottomThenTop                          ,
        iettBarsFrombothSides                          ,
        iettBarsFromTopAndBottom                       ,
        iettCrisscrossWipeFromTopLeft                  ,
        iettCrisscrossWipeFromTopRight                 ,
        iettCrisscrossWipeFromBottomLeft               ,
        iettCrisscrossWipeFromBottomRight              ,
        iettCrisscrossWipeBounceFromTopLeft            ,
        iettCrisscrossWipeBounceFromTopRight           ,
        iettCrisscrossWipeBounceFromBottomLeft         ,
        iettCrisscrossWipeBounceFromBottomRight        ,
        iettCrisscrossWipeFromLeftRightAndTop          ,
        iettCrisscrossWipeFromLeftRightAndBottom       ,
        iettCrisscrossWipeFromLeftTopAndBottom         ,
        iettCrisscrossWipeFromTopLeftRightAndBottom    ,
        iettCrisscrossWipeFromRightTopAndBottom        ,
        iettCrisscrossWipeFromBottomLeftTopRight       ,
        iettSpeckledWipeFromLeft                       ,
        iettSpeckledWipeFromRight                      ,
        iettSpeckledWipeFromTop                        ,
        iettSpeckledWipeFromBottom                     ,
        iettExpandingExplosions                        ,
        iettExpandingLightningBolts                    ,
        iettExplosionWipeOut                           ,
        iettExplosionWipeIn                            ,
        iettExplosionWipeInAndOut                      ,
        iettHeartWipeInAndOut                          ,
        iettRandomHearts                               ,
        iettExpandingHearts                            ,
        iettHeartWipeOut                               ,
        iettHeartWipeIn                                ,
        iettExpandingTriangles                         ,
        iettRectanglesFromTheLeft                      ,
        iettRectanglesFromTheRight                     ,
        iettRectanglesFromTheTop                       ,
        iettRectanglesFromTheBottom                    ,
        iettRandomWord                                 ,
        iettExpandingWord                              ,
        iettWordWipeOut                                ,
        iettWordWipeIn                                 ,
        iettWordWipeInAndOut                           ,
        iettZigzagWipeFromHorizon                      ,
        iettZigzagWipeFromVerticalCenter               ,
        iettZigzagWipeToHorizon                        ,
        iettZigzagWipeToVerticalCenter                 ,
        iettWordHalfSweep                              ,
        iettWordFullSweep
        {$endif}
        ];
{//}

{
<FM>Description<FN>
The effects that ImageEn offers that allow you to smoothly transition from one image to another.

Notes:
- The style of many transition effects can be modified using <A TIEImageEnGlobalSettings.TransitionsDrawAlternative>
- iettPanZoom is a special effect that does not transition to another image, but rather zooms in/out or pans to a position within the current image

<TABLE>
<R> <H>Category</H> <H>Transition</H> <H>Description</H> <H>Notes</H></R>
<R> <C><FM>General<FN></C> <C>iettCrossDissolve</C> <C>Cross Fade</C> <C></C> </R>
<R> <C></C> <C>iettFadeOutIn</C> <C>Fade Out then In</C> <C></C> </R>
<R> <C></C> <C>iettPageFlip</C> <C>Page Flip</C> <C></C> </R>
<R> <C></C> <C>iettPageFlip2</C> <C>3D Page Flip</C> <C>May be CPU intensive</C> </R>
<R> <C></C> <C>iettReversePageFlip</C> <C>Reverse Page Flip</C> <C></C> </R>
<R> <C></C> <C>iettReversePageFlip2</C> <C>3D Reverse Page Flip</C> <C>May be CPU intensive</C> </R>
<R> <C></C> <C>iettCubeRotateFromLeft</C> <C>Cube Rotate from Left</C> <C></C> </R>
<R> <C></C> <C>iettCubeRotateFromRight</C> <C>Cube Rotate from Right</C> <C></C> </R>
<R> <C></C> <C>iettCubeRotateFromTop</C> <C>Cube Rotate from Top</C> <C></C> </R>
<R> <C></C> <C>iettCubeRotateFromBottom</C> <C>Cube Rotate from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettCubeRotateFromLeft2</C> <C>3D Cube Rotate from Left</C> <C>May be CPU intensive</C> </R>
<R> <C></C> <C>iettCubeRotateFromRight2</C> <C>3D Cube Rotate from Right</C> <C>May be CPU intensive</C> </R>
<R> <C></C> <C>iettCubeRotateFromTop2</C> <C>3D Cube Rotate from Top</C> <C>May be CPU intensive</C> </R>
<R> <C></C> <C>iettCubeRotateFromBottom2</C> <C>3D Cube Rotate from Bottom</C> <C>May be CPU intensive</C> </R>
<R> <C><FM>Wipes<FN></C> <C>iettLeftRight1</C> <C>Wipe Left to Right</C> <C></C> </R>
<R> <C></C> <C>iettLeftRight2</C> <C>Wipe Left to Right 2</C> <C></C> </R>
<R> <C></C> <C>iettRightLeft1</C> <C>Wipe Right to Left</C> <C></C> </R>
<R> <C></C> <C>iettRightLeft2</C> <C>Wipe Right to Left 2</C> <C></C> </R>
<R> <C></C> <C>iettUpDown1</C> <C>Wipe Top to Bottom</C> <C></C> </R>
<R> <C></C> <C>iettUpDown2</C> <C>Wipe Top to Bottom 2</C> <C></C> </R>
<R> <C></C> <C>iettDownUp1</C> <C>Wipe Bottom to Top</C> <C></C> </R>
<R> <C></C> <C>iettDownUp2</C> <C>Wipe Bottom to Top 2</C> <C></C> </R>
<R> <C></C> <C>iettCenter1</C> <C>Wipe Out from Center</C> <C></C> </R>
<R> <C></C> <C>iettCenter2</C> <C>Wipe In to Center</C> <C></C> </R>
<R> <C></C> <C>iettWipeFromTopLeft</C> <C>Wipe from Top Left</C> <C></C> </R>
<R> <C></C> <C>iettWipeFromTopRight</C> <C>Wipe from Top Right</C> <C></C> </R>
<R> <C></C> <C>iettWipeFromBottomLeft</C> <C>Wipe from Bottom Left</C> <C></C> </R>
<R> <C></C> <C>iettWipeFromBottomRight</C> <C>Wipe from Bottom Right</C> <C></C> </R>
<R> <C></C> <C>iettWipeInFromTopAndBottom</C> <C>Wipe in from Top and Bottom</C> <C></C> </R>
<R> <C></C> <C>iettWipeFromHorizon</C> <C>Wipe from Horizon</C> <C></C> </R>
<R> <C></C> <C>iettWipeInFromSides</C> <C>Wipe in from Sides</C> <C></C> </R>
<R> <C></C> <C>iettWipeOutFromVerticalCenter</C> <C>Wipe out from Vertical Center</C> <C></C> </R>
<R> <C></C> <C>iettQuartersWipeInToCenter</C> <C>Quarters Wipe in to Center</C> <C></C> </R>
<R> <C><FM>Special Wipes<FN></C> <C>iettUnrollFromLeft</C> <C>Unroll from Left</C> <C></C> </R>
<R> <C></C> <C>iettUnrollFromRight</C> <C>Unroll from Right</C> <C></C> </R>
<R> <C></C> <C>iettUnrollFromTop</C> <C>Unroll from Top</C> <C></C> </R>
<R> <C></C> <C>iettUnrollFromBottom</C> <C>Unroll from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettSpeckledWipeFromLeft</C> <C>Speckled Wipe from Left</C> <C></C> </R>
<R> <C></C> <C>iettSpeckledWipeFromRight</C> <C>Speckled Wipe from Right</C> <C></C> </R>
<R> <C></C> <C>iettSpeckledWipeFromTop</C> <C>Speckled Wipe from Top</C> <C></C> </R>
<R> <C></C> <C>iettSpeckledWipeFromBottom</C> <C>Speckled Wipe from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettSoftWipeFromLeft</C> <C>Soft Wipe from Left</C> <C></C> </R>
<R> <C></C> <C>iettSoftWipeFromRight</C> <C>Soft Wipe from Right</C> <C></C> </R>
<R> <C></C> <C>iettSoftWipeFromTop</C> <C>Soft Wipe from Top</C> <C></C> </R>
<R> <C></C> <C>iettSoftWipeFromBottom</C> <C>Soft Wipe from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettPacmanFromLeft</C> <C>Pacman Devours from Left/C> <C></C> </R>
<R> <C></C> <C>iettPacmanFromRight</C> <C>Pacman Devours from Right</C> <C></C> </R>
<R> <C></C> <C>iettPacman3Row</C> <C>Pacman Devours Three Rows</C> <C></C> </R>
<R> <C></C> <C>iettPacman4Row</C> <C>Pacman Devours Four Rows</C> <C></C> </R>
<R> <C></C> <C>iettPacman2SimRow</C> <C>Two Simultaneous Rows of Pacman</C> <C></C> </R>
<R> <C></C> <C>iettPacman4SimRow</C> <C>Four Simultaneous Rows of Pacman</C> <C></C> </R>
<R> <C></C> <C>iettPacman6SimRow</C> <C>Six Simultaneous Rows of Pacman</C> <C></C> </R>
<R> <C><FM>Curved Wipes<FN></C> <C>iettCurvedWipeFromLeft</C> <C>Curved Wipe from Left</C> <C></C> </R>
<R> <C></C> <C>iettCurvedWipeFromRight</C> <C>Curved Wipe from Right</C> <C></C> </R>
<R> <C></C> <C>iettCurvedWipeFromTop</C> <C>Curved Wipe from Top</C> <C></C> </R>
<R> <C></C> <C>iettCurvedWipeFromBottom</C> <C>Curved Wipe from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettCurvedWipeFromTopLeft</C> <C>Curved Wipe from Top Left</C> <C></C> </R>
<R> <C></C> <C>iettCurvedWipeFromTopRight</C> <C>Curved Wipe from Top Right</C> <C></C> </R>
<R> <C></C> <C>iettCurvedWipeFromBottomLeft</C> <C>Curved Wipe from Bottom Left</C> <C></C> </R>
<R> <C></C> <C>iettCurvedWipeFromBottomRight</C> <C>Curved Wipe from Bottom Right</C> <C></C> </R>
<R> <C><FM>Angled Wipes<FN></C> <C>iettWipeDiagonalFromTopLeft</C> <C>Wipe diagonal from Top Left</C> <C></C> </R>
<R> <C></C> <C>iettWipeDiagonalFromTopRight</C> <C>Wipe diagonal from Top Right</C> <C></C> </R>
<R> <C></C> <C>iettWipeDiagonalFromBottomLeft</C> <C>Wipe diagonal from Bottom Left</C> <C></C> </R>
<R> <C></C> <C>iettWipeDiagonalFromBottomRight</C> <C>Wipe diagonal from Bottom Right</C> <C></C> </R>
<R> <C></C> <C>iettArrowWipeFromLeft</C> <C>Arrow Wipe from Left</C> <C></C> </R>
<R> <C></C> <C>iettArrowWipeFromRight</C> <C>Arrow Wipe from Right</C> <C></C> </R>
<R> <C></C> <C>iettArrowWipeFromTop</C> <C>Arrow Wipe from Top</C> <C></C> </R>
<R> <C></C> <C>iettArrowWipeFromBottom</C> <C>Arrow Wipe from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettHorizontalBowTieWipe</C> <C>Horizontal Bow Tie Wipe</C> <C></C> </R>
<R> <C></C> <C>iettVerticalBowTieWipe</C> <C>Vertical Bow Tie Wipe</C> <C></C> </R>
<R> <C></C> <C>iettZigzagWipeFromHorizon</C> <C>Zigzag Wipe from Horizon</C> <C></C> </R>
<R> <C></C> <C>iettZigzagWipeFromVerticalCenter</C> <C>Zigzag Wipe from Vertical Center</C> <C></C> </R>
<R> <C></C> <C>iettTriangularWipe</C> <C>Triangular Wipe</C> <C></C> </R>
<R> <C></C> <C>iettZigzagWipeToHorizon</C> <C>Zigzag Wipe To Horizon</C> <C></C> </R>
<R> <C></C> <C>iettZigzagWipeToVerticalCenter</C> <C>Zigzag Wipe To Vertical Center</C> <C></C> </R>
<R> <C></C> <C>iettAngledTwistIn</C> <C>Twist In</C> <C></C> </R>
<R> <C></C> <C>iettAngledTwistOut</C> <C>Twist Out</C> <C></C> </R>
<R> <C></C> <C>iettMultipleAngledTwistIn</C> <C>Multiple Twist In</C> <C></C> </R>
<R> <C></C> <C>iettMultipleAngledTwistOut</C> <C>Multiple Twist Out</C> <C></C> </R>
<R> <C><FM>Center Wipes<FN></C> <C>iettCircularWipeFromCenter</C> <C>Circular Wipe from Center</C> <C></C> </R>
<R> <C></C> <C>iettCircularWipeToCenter</C> <C>Circular Wipe to Center</C> <C></C> </R>
<R> <C></C> <C>iettDiagonalCrossFromCenter</C> <C>Diagonal Cross from Center</C> <C></C> </R>
<R> <C></C> <C>iettDiagonalCrossToCenter</C> <C>Diagonal Cross to Center</C> <C></C> </R>
<R> <C></C> <C>iettDiamondWipeFromCenter</C> <C>Diamond Wipe from Center</C> <C></C> </R>
<R> <C></C> <C>iettDiamondWipeToCenter</C> <C>Diamond Wipe to Center</C> <C></C> </R>
<R> <C></C> <C>iettHeartWipeOut</C> <C>Heart Wipe from Center</C> <C></C> </R>
<R> <C></C> <C>iettHeartWipeIn</C> <C>Heart Wipe to Center</C> <C></C> </R>
<R> <C></C> <C>iettStar5WipeOut</C> <C>5 Pointed Star Wipe from Center</C> <C></C> </R>
<R> <C></C> <C>iettStar5WipeIn</C> <C>5 Pointed Star Wipe to Center</C> <C></C> </R>
<R> <C></C> <C>iettStar6WipeOut</C> <C>6 Pointed Star Wipe from Center</C> <C></C> </R>
<R> <C></C> <C>iettStar6WipeIn</C> <C>6 Pointed Star Wipe to Center</C> <C></C> </R>
<R> <C></C> <C>iettExplosionWipeOut</C> <C>Explosion Wipe from Center</C> <C></C> </R>
<R> <C></C> <C>iettExplosionWipeIn</C> <C>Explosion Wipe to Center</C> <C></C> </R>
<R> <C></C> <C>iettCrossWipeOut</C> <C>Cross Wipe from Center</C> <C></C> </R>
<R> <C></C> <C>iettCrossWipeIn</C> <C>Cross Wipe to Center</C> <C></C> </R>     
<R> <C><FM>In and Outs<FN></C> <C>iettDiagonalBoxWipe</C> <C>Diamond Wipe In and Out</C> <C></C> </R>
<R> <C></C> <C>iettHeartWipeInAndOut</C> <C>Heart Wipe In and Out</C> <C></C> </R>
<R> <C></C> <C>iettStar5WipeInAndOut</C> <C>5 Pointed Star Wipe In and Out</C> <C></C> </R>
<R> <C></C> <C>iettStar6WipeInAndOut</C> <C>6 Pointed Star Wipe In and Out</C> <C></C> </R>
<R> <C></C> <C>iettExplosionWipeInAndOut</C> <C>Explosion Wipe In and Out</C> <C></C> </R>
<R> <C><FM>Shreds<FN></C> <C>iettShreddedFromLeft</C> <C>Shredded from Left</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromRight</C> <C>Shredded from Right</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromTop</C> <C>Shredded from Top</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromBottom</C> <C>Shredded from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromTopAndLeft</C> <C>Shredded from Top and Left</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromTopAndRight</C> <C>Shredded from Top and Right</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromBottomAndLeft</C> <C>Shredded from Bottom and Left</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromBottomAndRight</C> <C>Shredded from Bottom and Right</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromHorizonAndLeft</C> <C>Shredded from Horizon and Left</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromHorizonAndRight</C> <C>Shredded from Horizon and Right</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromTopAndVerticalCenter</C> <C>Shredded from Top and Vertical Center</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromBottomAndVerticalCenter</C> <C>Shredded from Bottom and Vertical Center</C> <C></C> </R>
<R> <C></C> <C>iettShreddedFromCenter</C> <C>Shredded from Center</C> <C></C> </R>
<R> <C></C> <C>iettShreddedToCenter</C> <C>Shredded to Center</C> <C></C> </R>
<R> <C></C> <C>iettShreddedInToHorizon</C> <C>Shredded in to Horizon</C> <C></C> </R>
<R> <C></C> <C>iettShreddedInToVerticalCenter</C> <C>Shredded in to Vertical Center</C> <C></C> </R>
<R> <C></C> <C>iettShreddedOutFromHorizon</C> <C>Shredded out from Horizon</C> <C></C> </R>
<R> <C></C> <C>iettShreddedOutFromVerticalCenter</C> <C>Shredded out from Vertical Center</C> <C></C> </R>
<R> <C><FM>Bars<FN></C> <C>iettBarsInFromLeft</C> <C>Bars in from Left</C> <C></C> </R>
<R> <C></C> <C>iettBarsInFromRight</C> <C>Bars in from Right</C> <C></C> </R>
<R> <C></C> <C>iettBarsFromTop</C> <C>Bars from Top</C> <C></C> </R>
<R> <C></C> <C>iettBarsFromBottom</C> <C>Bars from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettBarsLeftThenRight</C> <C>Bars Left then Right</C> <C></C> </R>
<R> <C></C> <C>iettBarsRightThenLeft</C> <C>Bars Right then Left</C> <C></C> </R>
<R> <C></C> <C>iettBarsTopThenBottom</C> <C>Bars Top then Bottom</C> <C></C> </R>
<R> <C></C> <C>iettBarsBottomThenTop</C> <C>Bars Bottom then Top</C> <C></C> </R>
<R> <C></C> <C>iettBarsFromBothSides</C> <C>Bars from both Sides</C> <C></C> </R>
<R> <C></C> <C>iettBarsFromTopAndBottom</C> <C>Bars from Top and Bottom</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromTopLeft</C> <C>Crisscross Wipe from Top Left</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromTopRight</C> <C>Crisscross Wipe from Top Right</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromBottomLeft</C> <C>Crisscross Wipe from Bottom Left</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromBottomRight</C> <C>Crisscross Wipe from Bottom Right</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeBounceFromTopLeft</C> <C>Crisscross Wipe Bounce from Top Left</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeBounceFromTopRight</C> <C>Crisscross Wipe Bounce from Top Right</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeBounceFromBottomLeft</C> <C>Crisscross Wipe Bounce from Bottom Left</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeBounceFromBottomRight</C> <C>Crisscross Wipe Bounce from Bottom Right</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromLeftRightAndTop</C> <C>Crisscross Wipe from Left Right and Top</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromLeftRightAndBottom</C> <C>Crisscross Wipe from Left Right and Bottom</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromLeftTopAndBottom</C> <C>Crisscross Wipe from Left Top and Bottom</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromTopLeftRightAndBottom</C> <C>Crisscross Wipe from Top Left Right and Bottom</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromRightTopAndBottom</C> <C>Crisscross Wipe from Right Top and Bottom</C> <C></C> </R>
<R> <C></C> <C>iettCrisscrossWipeFromBottomLeftTopRight</C> <C>Crisscross Wipe from Bottom Left Top Right</C> <C></C> </R>   
<R> <C></C> <C>iettRectanglesFromTheLeft</C> <C>Random Bars from the Left</C> <C></C> </R>
<R> <C></C> <C>iettRectanglesFromTheRight</C> <C>Random Bars from the Right</C> <C></C> </R>
<R> <C></C> <C>iettRectanglesFromTheTop</C> <C>Random Bars from the Top</C> <C></C> </R>
<R> <C></C> <C>iettRectanglesFromTheBottom</C> <C>Random Bars from the Bottom</C> <C></C> </R>
<R> <C><FM>Pushes<FN></C> <C>iettMoveLeftRight1</C> <C>Push Left to Right</C> <C></C> </R>
<R> <C></C> <C>iettMoveRightLeft1</C> <C>Push Right to Left</C> <C></C> </R>
<R> <C></C> <C>iettMoveUpDown1</C> <C>Push Top to Bottom</C> <C></C> </R>
<R> <C></C> <C>iettMoveDownUp1</C> <C>Push Bottom to Top</C> <C></C> </R>
<R> <C></C> <C>iettPushLeftAndSlideOut</C> <C>Push Left and Slide out</C> <C></C> </R>
<R> <C></C> <C>iettPushRightAndSlideOut</C> <C>Push Right and Slide out</C> <C></C> </R>
<R> <C></C> <C>iettPushUpAndSlideOut</C> <C>Push up and Slide out</C> <C></C> </R>
<R> <C></C> <C>iettPushDownAndSlideOut</C> <C>Push down and Slide out</C> <C></C> </R>
<R> <C></C> <C>iettPushAndSqueezeLeft</C> <C>Push and Squeeze Left</C> <C></C> </R>
<R> <C></C> <C>iettPushAndSqueezeRight</C> <C>Push and Squeeze Right</C> <C></C> </R>
<R> <C></C> <C>iettPushAndSqueezeUp</C> <C>Push and Squeeze up</C> <C></C> </R>
<R> <C></C> <C>iettPushAndSqueezeDown</C> <C>Push and Squeeze down</C> <C></C> </R>
<R> <C><FM>Slides<FN></C> <C>iettFromUpLeft</C> <C>Slide from Top Left</C> <C></C> </R>
<R> <C></C> <C>iettFromUpRight</C> <C>Slide from Top Right</C> <C></C> </R>
<R> <C></C> <C>iettFromBottomLeft</C> <C>Slide from Bottom Left</C> <C></C> </R>
<R> <C></C> <C>iettFromBottomRight</C> <C>Slide from Bottom Right</C> <C></C> </R>
<R> <C></C> <C>iettMoveLeftRight2</C> <C>Slide Out Left to Right</C> <C></C> </R>
<R> <C></C> <C>iettMoveRightLeft2</C> <C>Slide Out Right to Left</C> <C></C> </R>
<R> <C></C> <C>iettMoveUpDown2</C> <C>Slide Out Top to Bottom</C> <C></C> </R>
<R> <C></C> <C>iettMoveDownUp2</C> <C>Slide Out Bottom to Top</C> <C></C> </R>
<R> <C></C> <C>iettSlideInFromLeft</C> <C>Slide in from Left</C> <C></C> </R>
<R> <C></C> <C>iettSlideInFromRight</C> <C>Slide in from Right</C> <C></C> </R>
<R> <C></C> <C>iettSlideInFromTop</C> <C>Slide in from Top</C> <C></C> </R>
<R> <C></C> <C>iettSlideInFromBottom</C> <C>Slide in from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettQuartersSlideInToCenter</C> <C>Quarters Slide in to Center</C> <C></C> </R>
<R> <C><FM>Shrinks<FN></C> <C>iettShrinkToTopLeft</C> <C>Shrink to Top Left</C> <C></C> </R>
<R> <C></C> <C>iettShrinkToTopRight</C> <C>Shrink to Top Right</C> <C></C> </R>
<R> <C></C> <C>iettShrinkToBottomLeft</C> <C>Shrink to Bottom Left</C> <C></C> </R>
<R> <C></C> <C>iettShrinkToBottomRight</C> <C>Shrink to Bottom Right</C> <C></C> </R>
<R> <C></C> <C>iettShrinkToCenter</C> <C>Shrink to Center</C> <C></C> </R>
<R> <C><FM>Expands<FN></C> <C>iettCenterZoom1</C> <C>Expand Out from Center</C> <C></C> </R>
<R> <C></C> <C>iettCenterZoom2</C> <C>Expand In to Center</C> <C></C> </R>
<R> <C></C> <C>iettExpandFromLeft</C> <C>Expand from Left</C> <C></C> </R>
<R> <C></C> <C>iettExpandFromRight</C> <C>Expand from Right</C> <C></C> </R>
<R> <C></C> <C>iettExpandFromTop</C> <C>Expand from Top</C> <C></C> </R>
<R> <C></C> <C>iettExpandFromBottom</C> <C>Expand from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettExpandFromTopLeft</C> <C>Expand from Top Left</C> <C></C> </R>
<R> <C></C> <C>iettExpandFromTopRight</C> <C>Expand from Top Right</C> <C></C> </R>
<R> <C></C> <C>iettExpandFromBottomLeft</C> <C>Expand from Bottom Left</C> <C></C> </R>
<R> <C></C> <C>iettExpandFromBottomRight</C> <C>Expand from Bottom Right</C> <C></C> </R>
<R> <C></C> <C>iettExpandInFromLeft</C> <C>Expand in from Left</C> <C></C> </R>
<R> <C></C> <C>iettExpandInFromRight</C> <C>Expand in from Right</C> <C></C> </R>
<R> <C></C> <C>iettExpandInFromTop</C> <C>Expand in from Top</C> <C></C> </R>
<R> <C></C> <C>iettExpandInFromBottom</C> <C>Expand in from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettExpandInToVerticalCenter</C> <C>Expand in to Vertical Center</C> <C></C> </R>
<R> <C></C> <C>iettExpandInToHorizon</C> <C>Expand in to Horizon</C> <C></C> </R>
<R> <C></C> <C>iettExpandInFromSides</C> <C>Expand in from Sides</C> <C></C> </R>
<R> <C></C> <C>iettExpandInFromTopAndBottom</C> <C>Expand in from Top and Bottom</C> <C></C> </R>
<R> <C></C> <C>iettExpandOutFromHorizon</C> <C>Expand out from Horizon</C> <C></C> </R>
<R> <C></C> <C>iettExpandOutFromVerticalCenter</C> <C>Expand out from Vertical Center</C> <C></C> </R>
<R> <C></C> <C>iettQuartersExpandToCenter</C> <C>Quarters Expand to Center</C> <C></C> </R>
<R> <C><FM>Multi-Expanders<FN></C> <C>iettExpandingRectangles</C> <C>Expanding Rectangles</C> <C></C> </R>
<R> <C></C> <C>iettExpandingTriangles</C> <C>Expanding Triangles</C> <C></C> </R>
<R> <C></C> <C>iettExpandingCircles</C> <C>Expanding Circles</C> <C></C> </R>
<R> <C></C> <C>iettExpandingDiamonds</C> <C>Expanding Diamonds</C> <C></C> </R>
<R> <C></C> <C>iettExpandingHearts</C> <C>Expanding Hearts</C> <C></C> </R>
<R> <C></C> <C>iettExpandingStar5</C> <C>Expanding 5 Pointed Stars</C> <C></C> </R>
<R> <C></C> <C>iettExpandingStar6</C> <C>Expanding 6 Pointed Stars</C> <C></C> </R>
<R> <C></C> <C>iettExpandingExplosions</C> <C>Expanding Explosions</C> <C></C> </R>
<R> <C></C> <C>iettExpandingLightningBolts</C> <C>Expanding Lightning Bolts</C> <C></C> </R>
<R> <C><FM>Randoms<FN></C> <C>iettRandomPoints</C> <C>Random Points</C> <C></C> </R>
<R> <C></C> <C>iettRandomBoxes</C> <C>Random Boxes</C> <C></C> </R>
<R> <C></C> <C>iettRandomBigBoxes</C> <C>Random Big Boxes</C> <C></C> </R>
<R> <C></C> <C>iettRandomHearts</C> <C>Random Hearts</C> <C></C> </R>
<R> <C></C> <C>iettRandomStar5s</C> <C>Random 5 Pointed Stars</C> <C></C> </R>
<R> <C></C> <C>iettRandomStar6s</C> <C>Random 6 Pointed Stars</C> <C></C> </R>
<R> <C></C> <C>iettRandomExplosions</C> <C>Random Explosions</C> <C></C> </R>
<R> <C></C> <C>iettRandomPuzzlePieces</C> <C>Random Puzzle Pieces</C> <C></C> </R>
<R> <C><FM>Rotates<FN></C> <C>iettDiagonalSweepClockwise</C> <C>Diagonal Sweep Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettDiagonalSweepCounterClockwise</C> <C>Diagonal Sweep Counter-Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettSweepClockwise</C> <C>Sweep Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettSweepCounterClockwise</C> <C>Sweep Counter-Clockwise</C> <C></C> </R>    
<R> <C></C> <C>iettFullSweepClockwise</C> <C> Full Sweep Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettExpandingSweepClockwise</C> <C>Expanding Sweep Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettStarburstClockwiseFromCenter</C> <C>Starburst Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettStarburstCounterClockwiseFromCenter</C> <C>Starburst Counter-Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettRotationalRectangle</C> <C>Rotational Rectangle Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettRotationalRectangleCounterClockwise</C> <C>Rotational Rectangle Counter-Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettRotationalStar</C> <C>Rotational Star Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettRotationalStarCounterClockwise</C> <C>Rotational Star Counter-Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettSpirallingRectangleClockwise</C> <C>Spiralling Rectangle Clockwise</C> <C></C> </R>
<R> <C></C> <C>iettSpirallingRectangleCounterClockwise</C> <C>Spiralling Rectangle Counter-Clockwise</C> <C></C> </R>
<R> <C><FM>Word Effects<FN></C> <C>iettRandomBoxesWithWord</C> <C>Random Boxes with Word</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams>. Full word is always shown; does not cycle.</C> </R>
<R> <C></C> <C>iettRandomWord</C> <C>Random Letters of Word</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
<R> <C></C> <C>iettExpandingWord</C> <C>Expanding Letters of Word</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
<R> <C></C> <C>iettWordWipeOut</C> <C>Word Wipe from Center</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
<R> <C></C> <C>iettWordWipeIn</C> <C>Word Wipe to Center</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
<R> <C></C> <C>iettWordWipeInAndOut</C> <C>Word Wipe In and Out</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
<R> <C></C> <C>iettWordHalfSweep</C> <C>Half Sweep with Word</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
<R> <C></C> <C>iettWordFullSweep</C> <C>Full Sweep with Word</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
<R> <C></C> <C>iettWordFullExpandingSweep</C> <C>Expanding Sweep with Word</C> <C>Specify word properties using IEGlobalSettings().<A TIEImageEnGlobalSettings.WordTransitionParams></C> </R>
<R> <C><FM>Other<FN></C> <C>iettBuildUpFromLeft</C> <C>Build up from Left</C> <C></C> </R>
<R> <C></C> <C>iettBuildUpFromRight</C> <C>Build up from Right</C> <C></C> </R>
<R> <C></C> <C>iettBuildUpFromTop</C> <C>Build up from Top</C> <C></C> </R>
<R> <C></C> <C>iettBuildUpFromBottom</C> <C>Build up from Bottom</C> <C></C> </R>
<R> <C></C> <C>iettHorizontalBlinds</C> <C>Horizontal Blinds</C> <C></C> </R>
<R> <C></C> <C>iettVerticalBlinds</C> <C>Vertical Blinds</C> <C></C> </R>
<R> <C></C> <C>iettUnevenBlindsFromLeft</C> <C>Uneven Blinds from Left</C> <C></C> </R>
<R> <C></C> <C>iettUnevenBlindsFromRight</C> <C>Uneven Blinds from Right</C> <C></C> </R>
<R> <C></C> <C>iettUnevenBlindsFromTop</C> <C>Uneven Blinds from Top</C> <C></C> </R>
<R> <C></C> <C>iettUnevenBlindsFromBottom</C> <C>Uneven Blinds from Bottom</C> <C></C> </R>
<R> <C><FM>Legacy<FN></C> <C>iettFadeOut</C> <C>Fade Out</C> <C></C> </R>
<R> <C></C> <C>iettFadeIn</C> <C>Fade In</C> <C></C> </R>
<R> <C><FM>Special<FN></C> <C>iettPanZoom</C> <C>Pan and Zoom</C> <C>Special usage: Must specify Start and End points</C> </R>
</TABLE>
!!}

type
{!!
<FS>TIETransitionTiming

<FM>Declaration<FC>
}
  TIETransitionTiming=(iettLinear, iettExponential, iettLogarithmic);
{!!}

{!!
<FS>TIETransitionStepEvent

<FM>Declaration<FC>
}
  TIETransitionStepEvent=procedure(Sender: TObject; step: Integer) of object;
{!!}

  TIETransitionEffects = class;

{!!
<FS>TIEOnTransitionPaint

<FM>Declaration<FC>
}
  TIEOnTransitionPaint = procedure(Sender: TObject; Bitmap: TBitmap; Transition: TIETransitionEffects; Step: Integer) of object;
{!!}


  // transition effects class
  TIETransitionEffects = class
  private
    fSourceShot, fTargetShot, fCurrentView: TBitmap;
    fFullImage : TIEBitmap;
    fSourceShotLines, fTargetShotLines, fCurrentViewLines: PPointerArray;
    fWidth, fHeight: integer;
    fRunning: boolean;
    fOwner: TIEView;
    fTimer: TTimer;
    fStep: integer;
    FFirstStep : Boolean;  // some transitions require initializing
    fAccum1: integer;
    fTransition: TIETransitionType;
    fDuration: integer;    // time in millisecs
    fStartTick: dword;
    fUpdateOnStop: boolean;
    fStartRect: TRect;
    fEndRect: TRect;
    fOnTransitionStop: TNotifyEvent;
    fOnTransitionStep: TIETransitionStepEvent;
    fTiming: TIETransitionTiming;
    fBackground: TColor;
    // Pan Zoom Smoothing
    FMinLeft, FMaxLeft,
    FMinTop, FMaxTop,
    FMinBottom, FMaxBottom,
    FMinRight, FMaxRight: integer;
    fOnTransitionPaint: TIEOnTransitionPaint;
    procedure SetOnTransitionPaint(const Value: TIEOnTransitionPaint);
    procedure MakeTransition(DestCanvas : TCanvas; bExplicitUpdate : Boolean = False);
  protected
    fFrames: integer; // debug counter

    procedure TimerEvent(Sender: TObject);
    //
    procedure CrossDissolve(Step: integer);
    procedure FadeOut(Step: integer);
    procedure FadeIn(Step: integer);
    procedure FadeOutIn(Step: integer);
    procedure LeftRight1(Step: integer);
    procedure LeftRight2(Step: integer);
    procedure RightLeft1(Step: integer);
    procedure RightLeft2(Step: integer);
    procedure UpDown1(Step: integer);
    procedure UpDown2(Step: integer);
    procedure DownUp1(Step: integer);
    procedure DownUp2(Step: integer);
    procedure MoveLeftRight1(Step: integer);
    procedure MoveLeftRight2(Step: integer);
    procedure MoveRightLeft1(Step: integer);
    procedure MoveRightLeft2(Step: integer);
    procedure MoveUpDown1(Step: integer);
    procedure MoveUpDown2(Step: integer);
    procedure MoveDownUp1(Step: integer);
    procedure MoveDownUp2(Step: integer);
    procedure RandomPoints(Step: integer);
    procedure FromUpLeft(Step: integer);
    procedure FromUpRight(Step: integer);
    procedure FromBottomLeft(Step: integer);
    procedure FromBottomRight(Step: integer);
    procedure RandomBoxes(Step: integer);
    procedure Center1(Step: integer);
    procedure Center2(Step: integer);
    procedure CenterZoom1(Step: integer);
    procedure CenterZoom2(Step: integer);
    procedure PanZoom(Step: integer);
    
    {$ifdef IEINCLUDEEXTRATRANSITIONS}
    Procedure CubeRotateDarkenEdges(bVertical, bForward: Boolean; iCurrentPosition, Step : Integer);
    procedure CubeRotateFromLeft(Step: integer);
    procedure CubeRotateFromRight(Step: integer);
    procedure CubeRotateFromTop(Step: integer);
    procedure CubeRotateFromBottom(Step: integer);
    procedure CubeRotateFromLeft3D(Step: integer);
    procedure CubeRotateFromRight3D(Step: integer);
    procedure CubeRotateFromTop3D(Step: integer);
    procedure CubeRotateFromBottom3D(Step: integer);

    procedure PageFlipEffect(Step: Integer; bForward: Boolean);
    procedure PageFlipEffect3D(Step: Integer; bForward: Boolean);
    procedure PageFlipDarkenEdges(iCurrentPosition, Step : Integer; bForward: Boolean);

    procedure SoftWipeVerticalEx(Step: integer; bForward : Boolean);
    procedure SoftWipeHorizontalEx(Step: integer; bForward : Boolean);
    procedure SoftWipeFromLeft(Step: integer);
    procedure SoftWipeFromRight(Step: integer);
    procedure SoftWipeFromTop(Step: integer);
    procedure SoftWipeFromBottom(Step: integer);
    {$endif}

  public
    constructor Create(Owner: TIEView);
    destructor Destroy; override;
    property SourceShot: TBitmap read fSourceShot;
    property TargetShot: TBitmap read fTargetShot;
    property Running: boolean read fRunning;
    procedure Run(UpdateOnStop: boolean);
    procedure Stop;
    property Transition: TIETransitionType read fTransition write fTransition;
    procedure SetSizes(Width, Height: integer);
    property Duration: integer read fDuration write fDuration;
    procedure PrepareBitmap(OriginalBitmap, TargetBitmap: TBitmap);
    procedure CreateBitmap(TransitionProgress : Single; DestBitmap : TBitmap);
    property FullImage : TIEBitmap read fFullImage write fFullImage;
    property StartRect: TRect read fStartRect write fStartRect;
    property EndRect: TRect read fEndRect write fEndRect;
    property OnTransitionStop: TNotifyEvent read fOnTransitionStop write fOnTransitionStop;
    property OnTransitionStep: TIETransitionStepEvent read fOnTransitionStep write fOnTransitionStep;
    property OnTransitionPaint: TIEOnTransitionPaint read fOnTransitionPaint write SetOnTransitionPaint;
    property Timing: TIETransitionTiming read fTiming write fTiming;
    property Background: TColor read fBackground write fBackground;
  end;



type
  TTransitionCategory = (tcGeneral, tcWipes, tcSpecialWipes, tcCurvedWipes, tcAngledWipes, tcCenterWipes, tcInAndOuts,
                         tcShreds, tcBars, tcSlides, tcPushes, tcShrinks, tcExpands, tcMultiExpanders, tcRandoms,
                         tcRotates, tcWordEffects, tcOther, tcUnattractive);
  TEffect = record
    Name : String;                     // Display name for transition
    Category : TTransitionCategory;    // A categorization of the transition
    Overlay : Boolean;                 // If true, the transition is suitable when changing from one image to a modified version of the same image (e.g. Image, then the same image with text applied). It is commonly used to "Reveal" text, like in PowerPoint (e.g. Text fading in on a title page)
  end;

const
  {$ifdef IEINCLUDEEXTRATRANSITIONS}
  MAX_TRANSITIONS = 249;
  {$else}
  MAX_TRANSITIONS = 30;
  {$endif}

  IETransitionList: array[1..MAX_TRANSITIONS] of TEffect = (

  // ADD IMAGEEN TRANSITIONS TO OUR ARRAY SO ALL TRANSITION NAMES CAN BE ACCESSED

    (Name: 'Cross Fade';                                      Category: tcGeneral;           Overlay: True),
    (Name: 'Fade Out';                                        Category: tcUnattractive;      Overlay: False),
    (Name: 'Fade In';                                         Category: tcUnattractive;      Overlay: False),
    (Name: 'Fade Out then In';                                Category: tcGeneral;           Overlay: False),

    (Name: 'Wipe Left to Right';                              Category: tcWipes;             Overlay: True),
    (Name: 'Wipe Left to Right 2';                            Category: tcWipes;             Overlay: True),
    (Name: 'Wipe Right to Left';                              Category: tcWipes;             Overlay: True),
    (Name: 'Wipe Right to Left 2';                            Category: tcWipes;             Overlay: True),
    (Name: 'Wipe Top to Bottom';                              Category: tcWipes;             Overlay: True),
    (Name: 'Wipe Top to Bottom 2';                            Category: tcWipes;             Overlay: True),
    (Name: 'Wipe Bottom to Top';                              Category: tcWipes;             Overlay: True),
    (Name: 'Wipe Bottom to Top 2';                            Category: tcWipes;             Overlay: True),

    (Name: 'Slide from Top Left';                             Category: tcSlides;            Overlay: False),
    (Name: 'Slide from Top Right';                            Category: tcSlides;            Overlay: False),
    (Name: 'Slide from Bottom Left';                          Category: tcSlides;            Overlay: False),
    (Name: 'Slide from Bottom Right';                         Category: tcSlides;            Overlay: False)
    ,
    (Name: 'Push Left to Right';                              Category: tcPushes;            Overlay: False),
    (Name: 'Slide Out Left to Right';                         Category: tcSlides;            Overlay: False),
    (Name: 'Push Right to Left';                              Category: tcPushes;            Overlay: False),
    (Name: 'Slide Out Right to Left';                         Category: tcSlides;            Overlay: False),
    (Name: 'Push Top to Bottom';                              Category: tcPushes;            Overlay: False),
    (Name: 'Slide Out Top to Bottom';                         Category: tcSlides;            Overlay: False),
    (Name: 'Push Bottom to Top';                              Category: tcPushes;            Overlay: False),
    (Name: 'Slide Out Bottom to Top';                         Category: tcSlides;            Overlay: False),

    (Name: 'Random Points';                                   Category: tcRandoms;           Overlay: True),
    (Name: 'Random Boxes';                                    Category: tcRandoms;           Overlay: True),

    (Name: 'Wipe Out from Center';                            Category: tcWipes;             Overlay: True),
    (Name: 'Wipe In to Center';                               Category: tcWipes;             Overlay: True),
    (Name: 'Expand Out from Center';                          Category: tcExpands;           Overlay: False),
    (Name: 'Expand In to Center';                             Category: tcExpands;           Overlay: False)

{$ifdef IEINCLUDEEXTRATRANSITIONS}

     // PS EFFECTS START
  , (Name: 'Expand from Left';	                              Category: tcExpands;           Overlay: False),
    (Name: 'Expand from Right';	                              Category: tcExpands;           Overlay: False),
    (Name: 'Expand from Top';	                              Category: tcExpands;           Overlay: False),
    (Name: 'Expand from Bottom';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand from Top Left';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand from Top Right';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand from Bottom Left';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand from Bottom Right';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand in from Left';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand in from Right';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand in from Top';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand in from Bottom';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand in to Vertical Center';                    Category: tcExpands;           Overlay: False),
    (Name: 'Expand in to Horizon';       	              Category: tcExpands;           Overlay: False),
    (Name: 'Expand in from Sides';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand in from Top and Bottom';	              Category: tcExpands;           Overlay: False),
    (Name: 'Expand out from Horizon';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Expand out from Vertical Center';	              Category: tcExpands;           Overlay: False),

    (Name: 'Wipe from Top Left';	                      Category: tcWipes;             Overlay: True),
    (Name: 'Wipe from Top Right';	                      Category: tcWipes;             Overlay: True),
    (Name: 'Wipe from Bottom Left';	                      Category: tcWipes;             Overlay: True),
    (Name: 'Wipe from Bottom Right';	                      Category: tcWipes;             Overlay: True),
    (Name: 'Wipe in from Top and Bottom';	              Category: tcWipes;             Overlay: True),
    (Name: 'Wipe from Horizon';	                              Category: tcWipes;             Overlay: True),
    (Name: 'Wipe in from Sides';	                      Category: tcWipes;             Overlay: True),
    (Name: 'Wipe out from Vertical Center';	              Category: tcWipes;             Overlay: True),

    (Name: 'Build up from Left';	                      Category: tcOther;             Overlay: False),
    (Name: 'Build up from Right';	                      Category: tcOther;             Overlay: False),
    (Name: 'Build up from Top';	                              Category: tcOther;             Overlay: False),
    (Name: 'Build up from Bottom';	                      Category: tcOther;             Overlay: False),

    (Name: 'Unroll from Left';	                              Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Unroll from Right';	                              Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Unroll from Top';	                              Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Unroll from Bottom';	                      Category: tcSpecialWipes;      Overlay: True),

    (Name: 'Slide in from Left';	                      Category: tcSlides;            Overlay: False),
    (Name: 'Slide in from Right';	                      Category: tcSlides;            Overlay: False),
    (Name: 'Slide in from Top';	                              Category: tcSlides;            Overlay: False),
    (Name: 'Slide in from Bottom';                            Category: tcSlides;            Overlay: False),

    (Name: 'Shrink to Top Left';	                      Category: tcShrinks;           Overlay: False),
    (Name: 'Shrink to Top Right';	                      Category: tcShrinks;           Overlay: False),
    (Name: 'Shrink to Bottom Left';	                      Category: tcShrinks;           Overlay: False),
    (Name: 'Shrink to Bottom Right';	                      Category: tcShrinks;           Overlay: False),
    (Name: 'Shrink to Center';	                              Category: tcShrinks;           Overlay: False),

    (Name: 'Quarters Wipe in to Center';	              Category: tcWipes;             Overlay: True),
    (Name: 'Quarters Expand to Center';	                      Category: tcExpands;           Overlay: False),
    (Name: 'Quarters Slide in to Center';	              Category: tcSlides;            Overlay: False),

    (Name: 'Curved Wipe from Left';	                      Category: tcCurvedWipes;       Overlay: True),
    (Name: 'Curved Wipe from Right';	                      Category: tcCurvedWipes;       Overlay: True),
    (Name: 'Curved Wipe from Top';	                      Category: tcCurvedWipes;       Overlay: True),
    (Name: 'Curved Wipe from Bottom';	                      Category: tcCurvedWipes;       Overlay: True),

    (Name: 'Curved Wipe from Top Left';	                      Category: tcCurvedWipes;       Overlay: True),
    (Name: 'Curved Wipe from Top Right';	              Category: tcCurvedWipes;       Overlay: True),
    (Name: 'Curved Wipe from Bottom Left';	              Category: tcCurvedWipes;       Overlay: True),
    (Name: 'Curved Wipe from Bottom Right';	              Category: tcCurvedWipes;       Overlay: True),

    (Name: 'Bars in from Left';	                              Category: tcBars;              Overlay: True),
    (Name: 'Bars in from Right';	                      Category: tcBars;              Overlay: True),
    (Name: 'Bars from Top';	                              Category: tcBars;              Overlay: True),
    (Name: 'Bars from Bottom';	                              Category: tcBars;              Overlay: True),
    (Name: 'Bars Left then Right';	                      Category: tcBars;              Overlay: True),
    (Name: 'Bars Right then Left';	                      Category: tcBars;              Overlay: True),
    (Name: 'Bars Top then Bottom';	                      Category: tcBars;              Overlay: True),
    (Name: 'Bars Bottom then Top';	                      Category: tcBars;              Overlay: True),
    (Name: 'Bars from both Sides';	                      Category: tcBars;              Overlay: True),
    (Name: 'Bars from Top and Bottom';	                      Category: tcBars;              Overlay: True),

    (Name: 'Shredded from Left';	                      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Right';	                      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Top';	                              Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Bottom';	                      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Top and Left';	              Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Top and Right';	              Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Bottom and Left';	              Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Bottom and Right';	              Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Horizon and Left';	              Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Horizon and Right';	              Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Top and Vertical Center';	      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Bottom and Vertical Center';	      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded from Center';	                      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded to Center';	                      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded in to Horizon';	                      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded in to Vertical Center';                  Category: tcShreds;            Overlay: True),
    (Name: 'Shredded out from Horizon';	                      Category: tcShreds;            Overlay: True),
    (Name: 'Shredded out from Vertical Center';               Category: tcShreds;            Overlay: True),

    (Name: 'Expanding Rectangles';                            Category: tcMultiExpanders;    Overlay: True),
    (Name: 'Expanding Triangles';                             Category: tcMultiExpanders;    Overlay: True),
    (Name: 'Expanding Circles';                               Category: tcMultiExpanders;    Overlay: True),
    (Name: 'Expanding Diamonds';                              Category: tcMultiExpanders;    Overlay: True),

    (Name: 'Circular Wipe from Center';	                      Category: tcCenterWipes;       Overlay: True),
    (Name: 'Circular Wipe to Center';	                      Category: tcCenterWipes;       Overlay: True),

    (Name: 'Crisscross Wipe from Top Left';	              Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Top Right';	              Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Bottom Left';	              Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Bottom Right';	              Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe Bounce from Top Left';	      Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe Bounce from Top Right';	      Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe Bounce from Bottom Left';         Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe Bounce from Bottom Right';	      Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Left Right and Top';	      Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Left Right and Bottom';      Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Left Top and Bottom';	      Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Top Left Right and Bottom';  Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Right Top and Bottom';	      Category: tcBars;              Overlay: True),
    (Name: 'Crisscross Wipe from Bottom Left Top Right';      Category: tcBars;              Overlay: True),

    (Name: 'Wipe diagonal from Top Left';	              Category: tcAngledWipes;       Overlay: True),
    (Name: 'Wipe diagonal from Top Right';	              Category: tcAngledWipes;       Overlay: True),
    (Name: 'Wipe diagonal from Bottom Left';	              Category: tcAngledWipes;       Overlay: True),
    (Name: 'Wipe diagonal from Bottom Right';	              Category: tcAngledWipes;       Overlay: True),

    (Name: 'Diagonal Sweep Clockwise';                        Category: tcRotates;           Overlay: True),
    (Name: 'Diagonal Sweep Counter-Clockwise';                Category: tcRotates;           Overlay: True),
    (Name: 'Half Sweep Clockwise';                            Category: tcRotates;           Overlay: True),
    (Name: 'Half Sweep Counter-Clockwise';                    Category: tcRotates;           Overlay: True),

    (Name: 'Starburst Clockwise';	                      Category: tcRotates;           Overlay: True),
    (Name: 'Starburst Counter-Clockwise';                     Category: tcRotates;           Overlay: True),

    (Name: 'Rotational Rectangle Clockwise';                  Category: tcRotates;           Overlay: False),
    (Name: 'Rotational Rectangle Counter-Clockwise';          Category: tcRotates;           Overlay: False),
    (Name: 'Rotational Star Clockwise';                       Category: tcRotates;           Overlay: True),
    (Name: 'Rotational Star Counter-Clockwise';               Category: tcRotates;           Overlay: True),

    (Name: 'Speckled Wipe from Left';	                      Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Speckled Wipe from Right';	                      Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Speckled Wipe from Top';	                      Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Speckled Wipe from Bottom';	                      Category: tcSpecialWipes;      Overlay: True),

    (Name: 'Push Left and Slide out';	                      Category: tcPushes;            Overlay: False),
    (Name: 'Push Right and Slide out';                        Category: tcPushes;            Overlay: False),
    (Name: 'Push up and Slide out';	                      Category: tcPushes;            Overlay: False),
    (Name: 'Push down and Slide out';	                      Category: tcPushes;            Overlay: False),

    (Name: 'Push and Squeeze Left';                           Category: tcPushes;            Overlay: False),
    (Name: 'Push and Squeeze Right';                          Category: tcPushes;            Overlay: False),
    (Name: 'Push and Squeeze up';                             Category: tcPushes;            Overlay: False),
    (Name: 'Push and Squeeze down';                           Category: tcPushes;            Overlay: False),

    (Name: 'Horizontal Blinds';                               Category: tcOther;             Overlay: True),
    (Name: 'Vertical Blinds';                                 Category: tcOther;             Overlay: True),

    (Name: 'Uneven Blinds from Left';                         Category: tcOther;             Overlay: True),
    (Name: 'Uneven Blinds from Right';                        Category: tcOther;             Overlay: True),
    (Name: 'Uneven Blinds from Top';	                      Category: tcOther;             Overlay: True),
    (Name: 'Uneven Blinds from Bottom';                       Category: tcOther;             Overlay: True),

    (Name: 'Wide Bars from the Left';                         Category: tcBars;              Overlay: True),
    (Name: 'Wide Bars from the Right';                        Category: tcBars;              Overlay: True),
    (Name: 'Wide Bars from the Top';                          Category: tcBars;              Overlay: True),
    (Name: 'Wide Bars from the Bottom';                       Category: tcBars;              Overlay: True),

    (Name: 'Spiralling Rectangle Clockwise';                  Category: tcRotates;           Overlay: False),
    (Name: 'Spiralling Rectangle Counter-Clockwise';          Category: tcRotates;           Overlay: False),

    (Name: 'Arrow Wipe from Left';                            Category: tcAngledWipes;       Overlay: True),
    (Name: 'Arrow Wipe from Right';                           Category: tcAngledWipes;       Overlay: True),
    (Name: 'Arrow Wipe from Top';                             Category: tcAngledWipes;       Overlay: True),
    (Name: 'Arrow Wipe from Bottom';                          Category: tcAngledWipes;       Overlay: True),

    (Name: 'Horizontal Bow Tie Wipe';                         Category: tcAngledWipes;       Overlay: True),
    (Name: 'Vertical Bow Tie Wipe';                           Category: tcAngledWipes;       Overlay: True),

    (Name: 'Diagonal Cross from Center';                      Category: tcCenterWipes;       Overlay: True),
    (Name: 'Diagonal Cross to Center';                        Category: tcCenterWipes;       Overlay: True),

    (Name: 'Zigzag Wipe from Horizon';                        Category: tcAngledWipes;       Overlay: True),
    (Name: 'Zigzag Wipe from Vertical Center';                Category: tcAngledWipes;       Overlay: True),

    (Name: 'Diamond Wipe from Center';                        Category: tcCenterWipes;       Overlay: True),
    (Name: 'Diamond Wipe to Center';                          Category: tcCenterWipes;       Overlay: True),

    (Name: 'Diamond Wipe In and Out';                         Category: tcInAndOuts;         Overlay: True),
    (Name: 'Rotational Triangular Wipe';                      Category: tcRotates;           Overlay: True),

    (Name: 'Random Big Boxes';                                Category: tcRandoms;           Overlay: True),

    (Name: 'Page Flip';                                       Category: tcGeneral;           Overlay: False),
    (Name: '3D Page Flip';                                    Category: tcGeneral;           Overlay: False),
    (Name: 'Reverse Page Flip';                               Category: tcGeneral;           Overlay: False),
    (Name: '3D Reverse Page Flip';                            Category: tcGeneral;           Overlay: False),

    (Name: 'Zigzag Wipe To Horizon';                          Category: tcAngledWipes;       Overlay: True),
    (Name: 'Zigzag Wipe To Vertical Center';                  Category: tcAngledWipes;       Overlay: True),

    (Name: 'Random Hearts';                                   Category: tcRandoms;           Overlay: True),
    (Name: 'Random 5 Pointed Stars';                          Category: tcRandoms;           Overlay: True),
    (Name: 'Random 6 Pointed Stars';                          Category: tcRandoms;           Overlay: True),
    (Name: 'Random Explosions';                               Category: tcRandoms;           Overlay: True),

    (Name: 'Expanding Hearts';                                Category: tcMultiExpanders;    Overlay: True),
    (Name: 'Expanding 5 Pointed Stars';                       Category: tcMultiExpanders;    Overlay: True),
    (Name: 'Expanding 6 Pointed Stars';                       Category: tcMultiExpanders;    Overlay: True),
    (Name: 'Expanding Explosions';                            Category: tcMultiExpanders;    Overlay: True),
    (Name: 'Expanding Lightning Bolts';                       Category: tcMultiExpanders;    Overlay: True),

    (Name: 'Heart Wipe from Center';                          Category: tcCenterWipes;       Overlay: True),
    (Name: 'Heart Wipe to Center';                            Category: tcCenterWipes;       Overlay: True),
    (Name: '5 Pointed Star Wipe from Center';                 Category: tcCenterWipes;       Overlay: True),
    (Name: '5 Pointed Star Wipe to Center';                   Category: tcCenterWipes;       Overlay: True),
    (Name: '6 Pointed Star Wipe from Center';                 Category: tcCenterWipes;       Overlay: True),
    (Name: '6 Pointed Star Wipe to Center';                   Category: tcCenterWipes;       Overlay: True),
    (Name: 'Explosion Wipe from Center';                      Category: tcCenterWipes;       Overlay: True),
    (Name: 'Explosion Wipe to Center';                        Category: tcCenterWipes;       Overlay: True),
    (Name: 'Cross Wipe from Center';                          Category: tcCenterWipes;       Overlay: True),
    (Name: 'Cross Wipe to Center';                            Category: tcCenterWipes;       Overlay: True),

    (Name: 'Heart Wipe In and Out';                           Category: tcInAndOuts;         Overlay: True),
    (Name: '5 Pointed Star Wipe In and Out';                  Category: tcInAndOuts;         Overlay: True),
    (Name: '6 Pointed Star Wipe In and Out';                  Category: tcInAndOuts;         Overlay: True),
    (Name: 'Explosion Wipe In and Out';                       Category: tcInAndOuts;         Overlay: True),

    (Name: 'Cube Rotate from Left';                           Category: tcGeneral;           Overlay: True),
    (Name: 'Cube Rotate from Right';                          Category: tcGeneral;           Overlay: True),
    (Name: 'Cube Rotate from Top';                            Category: tcGeneral;           Overlay: True),
    (Name: 'Cube Rotate from Bottom';                         Category: tcGeneral;           Overlay: True),

    (Name: 'Soft Wipe from Left';                             Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Soft Wipe from Right';                            Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Soft Wipe from Top';                              Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Soft Wipe from Bottom';                           Category: tcSpecialWipes;      Overlay: True),

    (Name: 'Twist In';                                        Category: tcAngledWipes;       Overlay: True),
    (Name: 'Twist Out';                                       Category: tcAngledWipes;       Overlay: True),
    (Name: 'Multiple Twist In';                               Category: tcAngledWipes;       Overlay: True),
    (Name: 'Multiple Twist Out';                              Category: tcAngledWipes;       Overlay: True),

    (Name: 'Random Puzzle Pieces';                            Category: tcRandoms;           Overlay: True),

    (Name: 'Pacman Devours from Left';                        Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Pacman Devours from Right';                       Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Pacman Devours Three Rows';                       Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Pacman Devours Four Rows';                        Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Two Simultaneous Rows of Pacman';                 Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Four Simultaneous Rows of Pacman';                Category: tcSpecialWipes;      Overlay: True),
    (Name: 'Six Simultaneous Rows of Pacman';                 Category: tcSpecialWipes;      Overlay: True),

    (Name: 'Full Sweep Clockwise';                            Category: tcRotates;           Overlay: True),
    (Name: 'Expanding Sweep Clockwise';                       Category: tcRotates;           Overlay: True),

    (Name: '3D Cube Rotate from Left';                        Category: tcGeneral;           Overlay: True),
    (Name: '3D Cube Rotate from Right';                       Category: tcGeneral;           Overlay: True),
    (Name: '3D Cube Rotate from Top';                         Category: tcGeneral;           Overlay: True),
    (Name: '3D Cube Rotate from Bottom';                      Category: tcGeneral;           Overlay: True),

    (Name: 'Random Boxes with Word';                          Category: tcWordEffects;       Overlay: False),
    (Name: 'Random Letters of Word';                          Category: tcWordEffects;       Overlay: False),
    (Name: 'Expanding Letters of Word';                       Category: tcWordEffects;       Overlay: False),
    (Name: 'Word Wipe from Center';                           Category: tcWordEffects;       Overlay: False),
    (Name: 'Word Wipe to Center';                             Category: tcWordEffects;       Overlay: False),
    (Name: 'Word Wipe In and Out';                            Category: tcWordEffects;       Overlay: False),
    (Name: 'Half Sweep with Word';                            Category: tcWordEffects;       Overlay: False),
    (Name: 'Full Sweep with Word';                            Category: tcWordEffects;       Overlay: False),
    (Name: 'Expanding Sweep with Word';                       Category: tcWordEffects;       Overlay: False)
{$endif}
    );


{
THE FOLLOWING EFFECTS FROM CUSTOMPICSHOW WERE NOT USED:

    (Name: 'Wipe from Left';	              Proc: Effect005) : Just like iettLeftRight1
    (Name: 'Wipe from Right';	              Proc: Effect006) : Just like iettRightLeft1
    (Name: 'Wipe from Top';	              Proc: Effect022) : Just like iettDownUp1
    (Name: 'Wipe from Bottom';	              Proc: Effect023) : Just like iettUpDown1
    (Name: 'Slide in from Bottom Right';      Proc: Effect039) : Just like iettFromBottomRight
    (Name: 'Slide in from Top Right';	      Proc: Effect040) : Just like iettFromUpRight
    (Name: 'Slide in from Top Left';	      Proc: Effect041) : Just like iettFromUpLeft
    (Name: 'Slide in from Bottom Left';	      Proc: Effect042) : Just like iettFromBottomLeft
    (Name: 'Expand out from centre';	      Proc: Effect052) : Just like iettCenterZoom1
    (Name: 'Wipe out from centre';	      Proc: Effect053)
    (Name: 'Wipe in to centre';	              Proc: Effect054) : Just like iettCenter2
    (Name: 'Fade';	                      Proc: Effect119) : Just like iettCrossDissolve
    (Name: 'Pivot from Top Left';	      Proc: Effect120) : Requires a 32 bit palette
    (Name: 'Pivot from Bottom Left';	      Proc: Effect121)
    (Name: 'Pivot from Top Right';	      Proc: Effect122)
    (Name: 'Pivot from Bottom Right';	      Proc: Effect123)
    (Name: 'Random squares appear';	      Proc: Effect128) : Just like iettRandomBoxes
    (Name: 'Pixelate';                        Proc: Effect168) : Requires a 32 bit palette
    (Name: 'Dissolve';                        Proc: Effect169) : Just like IE effect
    (Name: 'Random Bars Horizontal';          Proc: Effect170) : Uses TBitmap.ScanLine
    (Name: 'Random Bars Vertical';            Proc: Effect171) : Uses TBitmap.ScanLine
    (Name: 'Channel Mix';                     Proc: Effect172) : Uses TBitmap.ScanLine and 32 bit
    (Name: 'Rectangles from the Left';        Proc: Effect146) : Unattractive
    (Name: 'Rectangles from the Right';       Proc: Effect147) : Unattractive
    (Name: 'Rectangles from the Top';         Proc: Effect148) : Unattractive
    (Name: 'Rectangles from the Bottom';      Proc: Effect149) : Unattractive
}



type
{!!
<FS>TIEPanZoomType

<FM>Declaration<FC>
}
  TIEPanZoomType = (iepzPanTopLeftToBottomRight,
                    iepzPanTopRightToBottomLeft,
                    iepzPanBottomLeftToTopRight,
                    iepzPanBottomRightToTopLeft,

                    iepzPanTopLeftToCenter,
                    iepzPanTopRightToCenter,
                    iepzPanBottomLeftToCenter,
                    iepzPanBottomRightToCenter,

                    iepzPanCenterToTopLeft,
                    iepzPanCenterToTopRight,
                    iepzPanCenterToBottomLeft,
                    iepzPanCenterToBottomRight,

                    iepzPanLeftToRightOrTopToBottom,
                    iepzPanLeftToCenterOrTopToCenter,
                    iepzPanRightToCenterOrBottomToCenter,

                    iepzPanRightToLeftOrBottomToTop,
                    iepzPanCenterToLeftToOrCenterToTop,
                    iepzPanCenterToRightOrCenterToBottom,

                    iepzZoomInToTopLeft,
                    iepzZoomInToTopRight,
                    iepzZoomInToCenter,
                    iepzZoomInToBottomLeft,
                    iepzZoomInToBottomRight,

                    iepzZoomOutFromTopLeft,
                    iepzZoomOutFromTopRight,
                    iepzZoomOutFromCenter,
                    iepzZoomOutFromBottomLeft,
                    iepzZoomOutFromBottomRight,

                    iepzFullZoomInToTopLeft,
                    iepzFullZoomInToTop,
                    iepzFullZoomInToTopRight,

                    iepzFullZoomInToLeft,
                    iepzFullZoomInToCenter,
                    iepzFullZoomInToRight,

                    iepzFullZoomInToBottomLeft,
                    iepzFullZoomInToBottom,
                    iepzFullZoomInToBottomRight,

                    iepzFullZoomOutFromTopLeft,
                    iepzFullZoomOutFromTop,
                    iepzFullZoomOutFromTopRight,

                    iepzFullZoomOutFromLeft,
                    iepzFullZoomOutFromCenter,
                    iepzFullZoomOutFromRight,

                    iepzFullZoomOutFromBottomLeft,
                    iepzFullZoomOutFromBottom,
                    iepzFullZoomOutFromBottomRight); 
{!!}


const
  MAX_PAN_ZOOM_EFFECT_COUNT = 46;
  IEPanZoomEffectNames : array[0 .. MAX_PAN_ZOOM_EFFECT_COUNT - 1] of string = (    
          'Pan from Top Left to Bottom Right',
          'Pan from Top Right to Bottom Left',
          'Pan from Bottom Left to Top Right',
          'Pan from Bottom Right to Top Left',

          'Pan from Top Left to Center',
          'Pan from Top Right to Center',
          'Pan from Bottom Left to Center',
          'Pan from Bottom Right to Center',  

          'Pan from Center to Top Left',
          'Pan from Center to Top Right',
          'Pan from Center to Bottom Left',
          'Pan from Center to Bottom Right',

          'Pan from Left to Right/Top to Bottom',
          'Pan from Left to Center/Top to Center',
          'Pan from Right to Center/Bottom to Center',

          'Pan from Right to Left/Bottom to Top', 
          'Pan from Center to Left/Center to Top',
          'Pan from Center to Right/Center to Bottom',

          'Zoom in to Top Left',
          'Zoom in to Top Right',
          'Zoom in to Center',
          'Zoom in to Bottom Left',
          'Zoom in to Bottom Right',

          'Zoom out from Top Left',
          'Zoom out from Top Right',
          'Zoom out from Center',
          'Zoom out from Bottom Left',
          'Zoom out from Bottom Right',

          'Full Zoom in to Top Left',
          'Full Zoom in to Top',
          'Full Zoom in to Top Right',

          'Full Zoom in to Left',
          'Full Zoom in to Center',
          'Full Zoom in to Right',

          'Full Zoom in to Bottom Left',
          'Full Zoom in to Bottom',
          'Full Zoom in to Bottom Right',

          'Full Zoom out from Top Left',
          'Full Zoom out from Top',
          'Full Zoom out from Top Right',

          'Full Zoom out from Left',
          'Full Zoom out from Center',
          'Full Zoom out from Right',

          'Full Zoom out from Bottom Left',
          'Full Zoom out from Bottom',
          'Full Zoom out from Bottom Right');

  // Convert a TIEPanZoomType effect type to a starting and ending position
  procedure GetPanZoomEffectStartEndRects(iIEClientWidth, iIEClientHeight : Integer;    // ClientWidth and ClientHeight of the display window
                                          iBitmapWidth, iBitmapHeight : Integer;         // Width and Height of the Bitmap
                                          PanZoomEffect : TIEPanZoomType;                // Effect to use
                                          iMaxZoom : Integer;                            // For zoom effects, how much should we zoom in/out, e.g. 20 for 20%
                                          out StartRect: TRect;                          // Will be filled with the first display position
                                          out EndRect: TRect);                           // Will be filled with the last display position


implementation


uses
  imageenproc, iexCanvasUtils, {$IFDEF IEHASTYPES} Types, {$ENDIF} Math, ieSettings, imageenview;



{$ifdef IEINCLUDEEXTRATRANSITIONS}
var
  fTransitionInitialized : Boolean;

const
  xcsIELetter        = TXCustomShape(100);
  xcsIECircularShape = TXCustomShape(101);
  xcsIEDiamondShape  = TXCustomShape(102);


{ TComplexRegion }

type
  TComplexRegion = class(TObject)
  private
    RgnData: PRgnData;
    Capacity: Integer;
    Count: Integer;
    Bounds: TRect;
    Rect: PRect;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddRect(Left, Top, Right, Bottom: Integer);
    function CreateRegion: HRGN;
  end;

constructor TComplexRegion.Create;
begin
  inherited Create;
  Clear;
end;

destructor TComplexRegion.Destroy;
begin
  ReallocMem(RgnData, 0);
  inherited Destroy;
end;

procedure TComplexRegion.Clear;
begin
  ReallocMem(RgnData, 0);
  Count := 0;
  Capacity := 0;
  with Bounds do
  begin
    Left   := +MaxInt;
    Top    := +MaxInt;
    Right  := -MaxInt;
    Bottom := -MaxInt;
  end;
end;

procedure TComplexRegion.AddRect(Left, Top, Right, Bottom: Integer);
begin
  if Count = Capacity then
  begin
    Inc(Capacity, 500);
    ReallocMem(RgnData, SizeOf(TRgnData) + Capacity * SizeOf(TRect));
    Rect := PRect(@(RgnData^.Buffer));
    Inc(Rect, Count);
  end;
  Rect^.Left := Left;
  Rect^.Top := Top;
  Rect^.Right := Right;
  Rect^.Bottom := Bottom;
  Inc(Rect);
  Inc(Count);
  if Bounds.Left > Left then
    Bounds.Left := Left;
  if Bounds.Top > Top then
    Bounds.Top := Top;
  if Bounds.Right < Right then
    Bounds.Right := Right;
  if Bounds.Bottom < Bottom then
    Bounds.Bottom := Bottom;
end;

function TComplexRegion.CreateRegion: HRGN;
begin
  if Assigned(RgnData) then
  begin
    with RgnData^.rdh do
    begin
      dwSize := SizeOf(TRgnDataHeader);
      iType := RDH_RECTANGLES;
      nCount := Count;
      nRgnSize := SizeOf(TRect);
      rcBound := Bounds;
    end;
    Result := ExtCreateRegion(nil, SizeOf(TRgnData) + Count * SizeOf(TRect), RgnData^);
  end
  else
    Result := 0;
end;


{ Helper Functions }

// Blend the specified rect of a canvas with a color
{$ifdef IEHASNOTWINDOWSALPHABLEND}
procedure ColorBlend(const ACanvas: HDC; const ARect: TRect; const ABlendColor: TColor; const ABlendValue: Integer);
begin
end;
{$else}
procedure ColorBlend(const ACanvas: HDC; const ARect: TRect; const ABlendColor: TColor; const ABlendValue: Integer);
var
  DC: HDC;
  Brush: HBRUSH;
  Bitmap: HBITMAP;
  BlendFunction: TBlendFunction;
begin
  DC := CreateCompatibleDC(ACanvas);
  Bitmap := CreateCompatibleBitmap(ACanvas, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  Brush := CreateSolidBrush(ColorToRGB(ABlendColor));
  try
    SelectObject(DC, Bitmap);
    Windows.FillRect(DC, Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top), Brush);
    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.AlphaFormat := 0;
    BlendFunction.SourceConstantAlpha := ABlendValue;
    Windows.AlphaBlend(ACanvas, ARect.Left, ARect.Top,
                       ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, DC, 0, 0,
                       ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, BlendFunction);
  finally
    DeleteObject(Brush);
    DeleteObject(Bitmap);
    DeleteDC(DC);
  end;
end;
{$endif}



// PS to IE Parameter conversion
// W and H remain constant regardless of Progress
// X and Y increase in proportion to progress
procedure CalcParams(const ARect: TRect; Progress: Integer; var W, H, X, Y: Integer);
begin
  W := ARect.Right  - ARect.Left;
  H := ARect.Bottom - ARect.Top;
  if W >= H then
  begin
    X := MulDiv(W, Progress, 100);
    Y := MulDiv(X, H, W);
  end
  else
  begin
    Y := MulDiv(H, Progress, 100);
    X := MulDiv(Y, W, H);
  end;
end;


procedure OutputRgnToCanvas(DestCanvas: TCanvas; W, H : Integer; Rgn: HRGN; Image : TBitmap);
begin
  if Rgn <> 0 then
  try
    SelectClipRgn(DestCanvas.Handle, Rgn);
    BitBlt(DestCanvas.Handle, 0, 0, W, H, Image.Canvas.Handle, 0, 0, SRCCOPY);
    SelectClipRgn(DestCanvas.Handle, 0);
  except
    // UNEXPECTED ERROR
  end;
end;


procedure RotatePoints(var Points: array of TPoint; xOrg, yOrg: Integer; Angle: Extended);
var
  Sin, Cos: Extended;
  xPrime, yPrime: Integer;
  I: Integer;
begin
 SinCos(Angle, Sin, Cos);
 for I := Low(Points) to High(Points) do
   with Points[I] do
   begin
     xPrime := X - xOrg;
     yPrime := Y - yOrg;
     X := Round(xPrime * Cos - yPrime * Sin) + xOrg;
     Y := Round(xPrime * Sin + yPrime * Cos) + yOrg;
   end;
end;

procedure MirrorCopyRect(Canvas: TCanvas; dstRect: TRect; Bitmap: TBitmap; srcRect: TRect; Horz, Vert: Boolean);
var
  T: Integer;
begin
  IntersectRect(srcRect, srcRect, Rect(0, 0, Bitmap.Width, Bitmap.Height));
  if Horz then
  begin
    T := dstRect.Left;
    dstRect.Left := dstRect.Right+1;
    dstRect.Right := T-1;
  end;
  if Vert then
  begin
    T := dstRect.Top;
    dstRect.Top := dstRect.Bottom+1;
    dstRect.Bottom := T-1;
  end;
  SetStretchBltMode(Canvas.Handle, HALFTONE);
  StretchBlt(Canvas.Handle, dstRect.Left, dstRect.Top,
             dstRect.Right - dstRect.Left, dstRect.Bottom - dstRect.Top,
             Bitmap.Canvas.Handle, srcRect.Left, srcRect.Top,
             srcRect.Right - srcRect.Left, srcRect.Bottom - srcRect.Top, SRCCOPY);
end;


function CreateCustomShape(AShape: TXCustomShape; ARect: TRect): Hrgn;
var
  iLeft, iTop, iWidth, iHeight: Integer;
begin
  iLeft   := ARect.Left;
  iTop    := ARect.Top;
  iWidth  := ARect.Right - ARect.Left;
  iHeight := ARect.Bottom - ARect.Top;

  Result := CreateCustomShapeRegion(AShape, iLeft, iTop, iWidth, iHeight);
end;



function CreateBarRgn(X, Y, W, H: Integer; XMode, YMode: Integer): HRGN;
const
  Bar_Count = 50;
var
  X1, Y1, D: Integer;
  ComplexRgn: TComplexRegion;
begin
  D := (Min(W, H) div Bar_Count) + 1;
  if IEGlobalSettings().TransitionsDrawAlternative then
    D := (Min(W, H) div Bar_Count * 4) + 1;

  ComplexRgn := TComplexRegion.Create;
  try
    if X <= W then
      Y1 := 0
    else
      Y1 := D;
    while Y1 < H + D do
    begin
      if X > W then
      begin
        if XMode in [1, 4] then
          ComplexRgn.AddRect(2 * W - X, Y1, W, Y1 + D)
        else
        if XMode in [2, 5] then
          ComplexRgn.AddRect(0, Y1, X - W, Y1 + D);
        ComplexRgn.AddRect(0, Y1 - D, W, Y1);
      end
      else
      begin
        if XMode in [1, 5] then
          ComplexRgn.AddRect(W - X, Y1, W, Y1 + D)
        else
        if XMode in [2, 4] then
          ComplexRgn.AddRect(0, Y1, X, Y1 + D)
        else
        if XMode = 3 then
        begin
          ComplexRgn.AddRect(0, Y1 + D, X, Y1 + D + D);
          ComplexRgn.AddRect(W - X, Y1, W, Y1 + D);
        end;
      end;
      Inc(Y1, 2 * D);
    end;
    if Y <= H then
      X1 := 0
    else
      X1 := D;
    while X1 < W + D do
    begin
      if Y > H then
      begin
        if YMode in [1, 4] then
          ComplexRgn.AddRect(X1, 2 * H - Y, X1 + D, H)
        else
        if YMode in [2, 5] then
          ComplexRgn.AddRect(X1, 0, X1 + D, Y - H);
        ComplexRgn.AddRect(X1 - D, 0, X1, H);
      end
      else
      begin
        if YMode in [1, 5] then
          ComplexRgn.AddRect(X1, H - Y, X1 + D, H)
        else
        if YMode in [2, 4] then
          ComplexRgn.AddRect(X1, 0, X1 + D, Y)
        else
        if YMode = 3 then
        begin
          ComplexRgn.AddRect(X1, H - Y, X1 + D, H);
          ComplexRgn.AddRect(X1 + D, 0, X1 + D + D, Y);
        end;
      end;
      Inc(X1, 2 * D);
    end;
    Result := ComplexRgn.CreateRegion;
  finally
    ComplexRgn.Free;
  end;
end;


function CreatePolygonRgnEx(const Pts: array of Integer): HRGN;
begin
  Result := CreatePolygonRgn(Pts, (High(Pts) - Low(Pts) + 1) shr 1, WINDING);
end;


function CreatePourRgn(X, Y, W, H, XMode, YMode: Integer): HRGN;
const
  Shred_Value = 200;
var
  X1, Y1, mW, mH, WD, HD, N, R, mR, D: Integer;
  ComplexRegion: TComplexRegion;
begin
  ComplexRegion := TComplexRegion.Create;
  try
    D := (Min(W, H) div Shred_Value) + 1;
    if IEGlobalSettings().TransitionsDrawAlternative then
      D := (Min(W, H) div Shred_Value * 2) + 1;

    WD := W mod D;
    HD := H mod D;
    mW := W div 2;
    mH := H div 2;
    if XMode <> 0 then
    begin
      if X < W then
        N := W div 10
      else
        N := 0;
      Y1 := 0;
      while Y1 < H do
      begin
        R := X + (Random(2 * N) - N);
        if XMode = 1 then
          ComplexRegion.AddRect(W - R, Y1, W, Y1 + D + HD)
        else
        if XMode = 2 then
          ComplexRegion.AddRect(0, Y1, R, Y1 + D + HD)
        else
        if XMode = 3 then
        begin
          mR := R div 2;
          ComplexRegion.AddRect(mW - mR, Y1, mW, Y1 + D + HD);
          ComplexRegion.AddRect(mW, Y1, mW + mR, Y1 + D + HD);
        end
        else
        begin
          mR := R div 2;
          ComplexRegion.AddRect(W - mR, Y1, W, Y1 + D + HD);
          ComplexRegion.AddRect(0, Y1, mR, Y1 + D + HD);
        end;
        Inc(Y1, D);
      end;
    end;
    if YMode <> 0 then
    begin
      if Y < H then
        N := H div 10
      else
        N := 0;
      X1 := 0;
      while X1 < W do
      begin
        R := Y + Random(2 * N) - N;
        if YMode = 1 then
          ComplexRegion.AddRect(X1, H - R, X1 + D + WD, H)
        else
        if YMode = 2 then
          ComplexRegion.AddRect(X1, 0, X1 + D + WD, R)
        else
        if YMode = 3 then
        begin
          mR := R div 2;
          ComplexRegion.AddRect(X1, mH - mR, X1 + D + WD, mH);
          ComplexRegion.AddRect(X1, mH, X1 + D + WD, mH + mR);
        end
        else
        begin
          mR := R div 2;
          ComplexRegion.AddRect(X1, H - mR, X1 + D + WD, H);
          ComplexRegion.AddRect(X1, 0, X1 + D + WD, mR);
        end;
        Inc(X1, D);
      end;
    end;
    Result := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
end;



function CreateSwarmRgn(X, Y, W, H, XMode, YMode: Integer): HRGN;
var
  X1, Y1, N, M, I, C, L, S: Integer;
  ComplexRegion: TComplexRegion;
begin
  ComplexRegion := TComplexRegion.Create;
  try
    if XMode <> 0 then
    begin
      if X < W then
        N := W div 10
      else
        N := 0;
      M := N div 20;
      if IEGlobalSettings().TransitionsDrawAlternative then
          M := N div 40;
      if M < 2 then
        M := 2;
      S := M div 2;
      L := N div M;
      C := (3 * N) div (4 * M);
      Y1 := 0;
      while Y1 < H do
      begin
        if XMode = 1 then
        begin
          ComplexRegion.AddRect(W - X, Y1, W, Y1 + M);
          for I := L downto 1 do
            if Random(I) <= Ord(I <= C) then
            begin
              X1 := (W - X) - (I * M);
              ComplexRegion.AddRect(X1, Y1, X1 + M, Y1 + M);
            end;
        end
        else
        begin
          ComplexRegion.AddRect(0, Y1, X, Y1 + M);
          for I := L downto 1 do
            if Random(I) <= Ord(I <= C) then
            begin
              X1 := X + (I * M);
              ComplexRegion.AddRect(X1 - M, Y1, X1, Y1 + M);
            end;
        end;
        Inc(Y1, S);
      end;
    end;
    if YMode <> 0 then
    begin
      if Y < H then
        N := H div 10
      else
        N := 0;
      M := N div 20;   
      if IEGlobalSettings().TransitionsDrawAlternative then
        M := N div 40;
      if M < 2 then
        M := 2;
      S := M div 2;
      L := N div M;
      C := (3 * N) div (4 * M);
      X1 := 0;
      while X1 < W do
      begin
        if YMode = 1 then
        begin
          ComplexRegion.AddRect(X1, H - Y, X1 + M, H);
          for I := L downto 1 do
            if Random(I) <= Ord(I <= C) then
            begin
              Y1 := (H - Y) - (I * M);
              ComplexRegion.AddRect(X1, Y1, X1 + M, Y1 + M);
            end;
        end
        else
        begin
          ComplexRegion.AddRect(X1, 0, X1 + M, Y);
          for I := N div M downto 1 do
            if Random(I) <= Ord(I <= C) then
            begin
              Y1 := Y + (I * M);
              ComplexRegion.AddRect(X1, Y1 - M, X1 + M, Y1);
            end;
        end;
        Inc(X1, S);
      end;
    end;
    Result := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
end;



function CreateSliceRgn(mX, mY, Radius: Integer; StartAngle, EndAngle: Extended; NumPts: Integer; bFromTop : Boolean): HRGN;
var
  Pts, Pt: PPoint;
  Sin, Cos, Delta: Extended;
  I: Integer;
begin
  if NumPts < 1 then
    NumPts := 1;
  GetMem(Pts, (NumPts + 1) * SizeOf(TPoint));
  try
    Pt := Pts;
    Pt.X := mX;
    Pt.Y := mY;
    Delta := (EndAngle - StartAngle) / NumPts;
    for I := 1 to NumPts do
    begin
      Inc(Pt);
      SinCos(StartAngle, Sin, Cos);
      if bFromTop then
      begin
        Pt.X := mX + Round(Radius * Cos);
        Pt.Y := mY + Round(Radius * Sin);
      end
      else   
      begin
        Pt.X := mX - Round(Radius * Cos);
        Pt.Y := mY - Round(Radius * Sin);
      end;
      StartAngle := StartAngle + Delta;
    end;
    Result := CreatePolygonRgn(Pts^, NumPts + 1, WINDING);
  finally
    FreeMem(Pts);
  end;
end;


{ Randomization }

const
  Max_Random_List = 30;
var
  gRandomList : Array [0..Max_Random_List - 1 ] of Integer;

// Generates a randomized index of values between 0 and iMax. Call to initialize before using GetRandomListIndex
procedure InitializeRandomList(iMax: Integer);
var
  I: Integer;
  iDone: Integer;
  iRandom: Integer;
begin
  if iMax > Max_Random_List - 1 then
    raise Exception.create('Too large for random array');

  Randomize;
  for I := 0 to iMax do
    gRandomList[i] := -1;

  iDone := 0;
  while iDone <= iMax do
  begin
    iRandom := Random(iMax + 1);
    if gRandomList[iRandom] = -1 then
    begin
      gRandomList[iRandom] := iDone;
      inc(iDone);
    end;
  end;
end;

// Converts a zero-based value to a random one
function GetRandomListIndex(Index : Integer) : Integer;
begin
  Result := gRandomList[Index];
end;

{ Text Handling }

function BitmapToRegion(Bitmap: TBitmap; iLeft, iTop, iWidth, iHeight: Integer; cTransparentColor : TColor): HRGN;
type
  TPixels24 = array[0..0] of TRGBTriple;
var
  Pixels: Pointer;
  NewRectLeft: Integer;
  X, Y: Integer; 
  ComplexRegion: TComplexRegion;
  HorzScale, VertScale : Double;
  bIsTransparent : Boolean;
  R, G, B : Byte;

  procedure AddRect(LastCol: Boolean = False);
  var
    iBottom: Integer;
  begin
    if LastCol then
      Inc( X );
    iBottom := Y + 1;
    ComplexRegion.AddRect(iLeft + Trunc(NewRectLeft * HorzScale),
                          iTop  + Trunc(Y * VertScale),
                          iLeft + Trunc(X * HorzScale),
                          iTop  + Trunc(iBottom * VertScale));
    NewRectLeft := -1;
  end;
  
begin
  HorzScale := iWidth / Bitmap.Width;
  VertScale := iHeight / Bitmap.Height;
  ComplexRegion := TComplexRegion.Create;
  try                                   
    R := GetRValue(cTransparentColor);
    G := GetGValue(cTransparentColor);
    B := GetBValue(cTransparentColor);

    for Y := 0 to Bitmap.Height - 1 do
    begin
      Pixels := Bitmap.ScanLine[ Y ];
      NewRectLeft := -1;
      for X := 0 to Bitmap.Width - 1 do
      begin           
        bIsTransparent := ( TPixels24( Pixels^ )[ X ].rgbtRed = R ) and
                          ( TPixels24( Pixels^ )[ X ].rgbtGreen = G ) and
                          ( TPixels24( Pixels^ )[ X ].rgbtBlue = B );

        if bIsTransparent then
        begin
          if NewRectLeft >= 0 then
            AddRect;
        end
        else
        begin
          if NewRectLeft = -1 then
            NewRectLeft := X;
          if ( X = Bitmap.Width - 1 ) and ( NewRectLeft >= 0 ) then
            AddRect( True );
        end;
      end;
    end;

    Result := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
end;

function TextToBitmap(const sText : string; iBitmapHeight : Integer; const sFontName : string; cFontColor : TColor; Style : TFontStyles;
                      cBackground : TColor = clWhite; Angle : Integer = 0; bAntiAlias : Boolean = True; bIncludeLeading : Boolean = True
                      ): TBitmap;
var
  LogFont : TLogFont;
  AnExtent : TSize;
  AFontHandle : HFont;
begin
  Result := TBitmap.create;

  Result.Canvas.Font.Name   := sFontName;
  Result.Canvas.Font.Color  := cFontColor;
  Result.Canvas.Font.Style  := Style;

  if bIncludeLeading then
    Result.Canvas.Font.Height := iBitmapHeight
  else
    Result.Canvas.Font.Height := -1 * iBitmapHeight;

  GetObject(Result.Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
  LogFont.lfEscapement  := Angle * 10;
  LogFont.lfOrientation := Angle * 10;
  if bAntiAlias then
    LogFont.lfQuality   := ANTIALIASED_QUALITY
  else
    LogFont.lfQuality   := NONANTIALIASED_QUALITY;
  AFontHandle := CreateFontIndirect(LogFont);
  Result.Canvas.Font.Handle := AFontHandle;

  AnExtent  := Result.Canvas.TextExtent(sText);

  Result.Width  := AnExtent.cx;
  Result.Height := AnExtent.cy;
  Result.Canvas.Brush.Color := cBackground;
  Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));

  Result.Canvas.Brush.Style := bsClear;
  Result.Canvas.TextOut(0, 0, sText);
end;


// iDisplayStart should be 0 if we can start rotating the text immediately
// iDisplayEnd should be 100 if we can continue rotating the text all the way to the end
function CreateRegionFromLetter(ARect: TRect; Progress : Integer; iDisplayStart, iDisplayEnd : Integer;
                                bForceWholeWord : Boolean = False; bIncludeLeading : Boolean = True): Hrgn;
const
  Font_Color = clBlack;
  Transparent_Color = clWhite;
var
  Bitmap: TBitmap;
  iLeft, iTop, iWidth, iHeight: Integer;
  iQuality: Word;
  sText, sLetter: string;
  iLength, iLetter: Integer;
  iDisplayPeriod, iPeriodProgress: Integer;
begin
  if IEGlobalSettings().WordTransitionParams.Word = '' then
    IEGlobalSettings().WordTransitionParams.Word := 'A';
  if IEGlobalSettings().WordTransitionParams.FontName = '' then
    IEGlobalSettings().WordTransitionParams.FontName := 'Arial'; 
  if IEGlobalSettings().WordTransitionParams.Quality < 10 then
    IEGlobalSettings().WordTransitionParams.Quality := 200;

  // GET THE LETTER TO DISPLAY
  sText := IEGlobalSettings().WordTransitionParams.Word;
  iLength := Length(sText);

  // Support small text strings enclosed in quotes
  if (iLength > 1) and (sText[1] = '"') and (sText[iLength] = '"') then
  begin
    Delete(sText, iLength, 1);
    Delete(sText, 1, 1);
    iLength := 1; // Process as if one character
    if sText = '' then
      sText := 'X';
  end;

  // Single letter?
  if bForceWholeWord or (iLength = 1) then
    sLetter := sText
  else
  // In initial "hard to read" period
  if Progress <= iDisplayStart then
    sLetter := sText[1]
  else
  // in ending  "hard to read" period    
  if Progress >= iDisplayEnd then
    sLetter := sText[iLength]
  else
  begin
    // In rotating text period
    iDisplayPeriod := iDisplayEnd - iDisplayStart;
    iPeriodProgress := Progress - iDisplayStart;
    iLetter := Trunc(iPeriodProgress / iDisplayPeriod * iLength) + 1;
    sLetter := sText[iLetter];
  end;


  // GET A BITMAP OF OUR LETTER
  iHeight := ARect.Bottom - ARect.Top;
  iQuality := IEGlobalSettings().WordTransitionParams.Quality;
  if iQuality > iHeight then
    iQuality := iHeight;

  Bitmap := TextToBitmap(sLetter,
                         iQuality,
                         IEGlobalSettings().WordTransitionParams.FontName,
                         Font_Color,
                         IEGlobalSettings().WordTransitionParams.Style,
                         Transparent_Color,
                         0, False,
                         bIncludeLeading);
  try
    Bitmap.PixelFormat := pf24bit;

    // Maintain aspect ratio of Letter
    ARect := GetImageRectWithinArea(Bitmap.Width, Bitmap.Height, ARect);

    iLeft   := ARect.Left;
    iTop    := ARect.Top;           
    iHeight := ARect.Bottom - ARect.Top;
    iWidth  := Round(Bitmap.Width / Bitmap.Height * iHeight);

    // CONVERT THE LETTER BITMAP TO A REGION
    Result := BitmapToRegion(Bitmap, iLeft, iTop, iWidth, iHeight, clWhite);
  finally
    Bitmap.Free;
  end;
end;



{ Transition Effects }
                          

// Slide in from left
procedure SlideInFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, X - W, 0, W, H,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
end;

// Slide in from right
procedure SlideInFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, W - X, 0, W, H,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
end;

// Slide in from top
procedure SlideInFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, 0, Y - H, W, H,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
end;

// Slide in from bottom
procedure SlideInFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, 0, H - Y, W, H,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
end;


// Reveal out from middle
procedure WipeOutFromVerticalCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mWX: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mWX := (W - X) div 2;
  BitBlt(DestCanvas.Handle, mWX, 0, X, H,
         Image.Canvas.Handle, mWX, 0,
         SRCCOPY);
end;

// Reveal in from sides
procedure WipeInFromSidesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mX: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mX := X div 2;
  BitBlt(DestCanvas.Handle, 0, 0, mX, H,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
  BitBlt(DestCanvas.Handle, W - mX, 0, mX, H,
         Image.Canvas.Handle, W - mX, 0,
         SRCCOPY);
end;


// Unroll from left  
// 24bit verson
procedure UnrollFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
  R1, R2: TRect;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  R1 := ARect;
  R2 := ARect;
  R1.Left := X;
  if R1.Left < W div 5 then
   R1.Right := R1.Left + X div 2
  else
  if (R1.Left + W div 5) > W then
   R1.Right := R1.Left + (W - X) div 2
  else
   R1.Right := R1.Left + W div 10;
  R2.Left := R1.Right;
  R2.Right := R2.Left + R1.Right - R1.Left;
  MirrorCopyRect(DestCanvas, R1, Image, R2, True, False);
  R1.Left := 0;
  R1.Right := X;
  R2.Left := 0;
  R2.Right := X;
  DestCanvas.CopyRect(R1, Image.Canvas, R2);
end;

// Unroll from right
// 24bit verson
procedure UnrollFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
  R1, R2: TRect;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  R1 := ARect;
  R2 := ARect;
  R1.Right := W - X;
  if (R1.Right + W div 5) > W then
   R1.Left := R1.Right - X div 2
  else
  if R1.Right < W div 5 then
   R1.Left := R1.Right - (W - X) div 2
  else
   R1.Left := R1.Right - W div 10;
  R2.Right := R1.Left;
  R2.Left := R2.Right - R1.Right + R1.Left;
  MirrorCopyRect(DestCanvas, R1, Image, R2, True, False);
  R1.Left := W - X;
  R1.Right := W;
  R2.Left := W - X;
  R2.Right := W;
  DestCanvas.CopyRect(R1, Image.Canvas, R2);
end;

// Unroll from top
// 24bit verson
procedure UnrollFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
  R1, R2: TRect;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  R1 := ARect;
  R2 := ARect;
  R1.Top := Y;
  if R1.Top < H div 5 then
   R1.Bottom := R1.Top + Y div 2
  else
  if (R1.Top + H div 5) > H then
   R1.Bottom := R1.Top + (H - Y) div 2
  else
   R1.Bottom := R1.Top + H div 10;
  R2.Top := R1.Bottom;
  R2.Bottom := R2.Top + R1.Bottom - R1.Top;
  MirrorCopyRect(DestCanvas, R1, Image, R2, False, True);
  R1.Top := 0;
  R1.Bottom := Y;
  R2.Top := 0;
  R2.Bottom := Y;
  DestCanvas.CopyRect(R1, Image.Canvas, R2);
end;

// Unroll from bottom
// 24bit verson
procedure UnrollFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
  R1, R2: TRect;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  R1 := ARect;
  R2 := ARect;
  R1.Bottom := H - Y;
  if (R1.Bottom + H div 5) > H then
   R1.Top := R1.Bottom - Y div 2
  else
  if R1.Bottom < H div 5 then
   R1.Top := R1.Bottom - (H - Y) div 2
  else
   R1.Top := R1.Bottom - H div 10;
  R2.Bottom := R1.Top;
  R2.Top := R2.Bottom - R1.Bottom + R1.Top;
  MirrorCopyRect(DestCanvas, R1, Image, R2, False, True);
  R1.Top := H - Y;
  R1.Bottom := H;
  R2.Top := H - Y;
  R2.Bottom := H;
  DestCanvas.CopyRect(R1, Image.Canvas, R2);
end;


// Build up from left
procedure BuildUpFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  N: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, W - X, 0, X, H,
         Image.Canvas.Handle, W - X, 0,
         SRCCOPY);
  N := Max((W - X) - (W div 10), 0);
  StretchBlt(DestCanvas.Handle, 0, 0, W - X, H,
             Image.Canvas.Handle, N, 0, W - X - N, H,
             SRCCOPY);
end;

// Build up from right
procedure BuildUpFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  N: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, 0, 0, X, H,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
  N := Min(W - X, W div 10);
  StretchBlt(DestCanvas.Handle, X, 0, W - X, H,
             Image.Canvas.Handle, X, 0, N, H,
             SRCCOPY);
end;

// Expand in from top
procedure BuildUpFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  N: Integer;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, 0, H - Y, W, Y,
         Image.Canvas.Handle, 0, H - Y,
         SRCCOPY);
  N := Max((H - Y) - H div 10, 0);
  StretchBlt(DestCanvas.Handle, 0, 0, W, H - Y,
             Image.Canvas.Handle, 0, N, W, H - Y - N,
             SRCCOPY);
end;

// Expand from bottom
procedure BuildUpFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  N: Integer;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, 0, 0, W, Y,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
  N := Min(H - Y, H div 10);
  StretchBlt(DestCanvas.Handle, 0, Y, W, H - Y,
             Image.Canvas.Handle, 0, Y, W, N,
             SRCCOPY);
end;


// Expand from left
procedure ExpandFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  if IEGlobalSettings().TransitionsDrawAlternative then
    BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);

  StretchBlt(DestCanvas.Handle, 0, 0, X, H,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  if IEGlobalSettings().TransitionsDrawAlternative then
    ColorBlend(DestCanvas.Handle, Rect(X, 0, W, H), clBlack, 100 - SQR(10 - Progress div 10));
end;

// Expand from right
procedure ExpandFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  if IEGlobalSettings().TransitionsDrawAlternative then
    BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);

  StretchBlt(DestCanvas.Handle, W - X, 0, X, H,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  if IEGlobalSettings().TransitionsDrawAlternative then
    ColorBlend(DestCanvas.Handle, Rect(0, 0, W - X, H), clBlack, 100 - SQR(10 - Progress div 10));
end;

// Expand from top
procedure ExpandFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  if IEGlobalSettings().TransitionsDrawAlternative then
    BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);

  StretchBlt(DestCanvas.Handle, 0, 0, W, Y,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  if IEGlobalSettings().TransitionsDrawAlternative then
    ColorBlend(DestCanvas.Handle, Rect(0, Y, W, H), clBlack, 100 - SQR(10 - Progress div 10));
end;

// Expand from bottom
procedure ExpandFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  if IEGlobalSettings().TransitionsDrawAlternative then
    BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);

  StretchBlt(DestCanvas.Handle, 0, H - Y, W, Y,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  if IEGlobalSettings().TransitionsDrawAlternative then
    ColorBlend(DestCanvas.Handle, Rect(0, 0, W, H), clBlack, 100 - SQR(10 - Progress div 10));
end;

// Expand from top left
procedure ExpandFromTopLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, 0, X, Y,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);
end;

// Expand from top right
procedure ExpandFromTopRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, W - X, 0, X, Y,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);
end;

// Expand from bottom left
procedure ExpandFromBottomLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, H - Y, X, Y,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);
end;


// Expand from bottom right
procedure ExpandFromBottomRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, W - X, H - Y, X, Y,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);
end;


// Expand in from left
procedure ExpandInFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, X - W, 0, (2 * W) - X, H,
             Image.Canvas.Handle, W - X, 0, X, H,
             SRCCOPY);
end;

// Expand in from right
procedure ExpandInFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, 0, (2 * W) - X, H,
             Image.Canvas.Handle, 0, 0, X, H,
             SRCCOPY);
end;

// Expand in from top
procedure ExpandInFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, Y - H, W, (2 * H) - Y,
             Image.Canvas.Handle, 0, H - Y, W, Y,
             SRCCOPY);
end;

// Expand in from bottom
procedure ExpandInFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, 0, W, (2 * H) - Y,
             Image.Canvas.Handle, 0, 0, W, Y,
             SRCCOPY);
end;

// Expand in to middle (horiz)
procedure ExpandInToHorizonEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, Y - H, W, (3 * H) - (2 * Y),
             Image.Canvas.Handle, 0, (H - Y) div 2, W, Y,
             SRCCOPY);
end;

// Expand in from top / bottom
procedure ExpandInFromTopAndBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mY, mH: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mY := Y div 2;
  mH := H div 2;
  StretchBlt(DestCanvas.Handle, 0, 0, W, mY,
             Image.Canvas.Handle, 0, 0, W, mH,
             SRCCOPY);
  StretchBlt(DestCanvas.Handle, 0, H - mY, W, mY,
             Image.Canvas.Handle, 0, mH, W, mH,
             SRCCOPY);
end;

// Expand in to middle
procedure ExpandInToVerticalCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, X - W, 0, (3 * W) - (2 * X), H,
             Image.Canvas.Handle, (W - X) div 2, 0, X, H,
             SRCCOPY);
end;

// Expand in from sides
procedure ExpandInFromSidesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mX, mW: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mX := X div 2;
  mW := W div 2;
  StretchBlt(DestCanvas.Handle, 0, 0, mX, H,
             Image.Canvas.Handle, 0, 0, mW, H,
             SRCCOPY);
  StretchBlt(DestCanvas.Handle, W - mX, 0, mX, H,
             Image.Canvas.Handle, mW, 0, mW, H,
             SRCCOPY);
end;


// Expand out from middle (horiz)
procedure ExpandOutFromHorizonEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, (H - Y) div 2, W, Y,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);
end;

// Expand out from middle
procedure ExpandOutFromVerticalCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, (W - X) div 2, 0, X, H,
             Image.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);
end;


// Reveal from middle (horiz)
procedure WipeFromHorizonEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mHY: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mHY := (H - Y) div 2;
  BitBlt(DestCanvas.Handle, 0, mHY, W, Y,
         Image.Canvas.Handle, 0, mHY,
         SRCCOPY);
end;

// Slide in from top / bottom
procedure WipeInFromTopAndBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mY: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mY := Y div 2;
  BitBlt(DestCanvas.Handle, 0, 0, W, mY,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
  BitBlt(DestCanvas.Handle, 0, H - mY, W, mY,
         Image.Canvas.Handle, 0, H - mY,
         SRCCOPY);
end;


// Reveal from top left
procedure WipeFromTopLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, 0, 0, X, Y,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
end;

// Reveal from bottom left
procedure WipeFromBottomLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, 0, H - Y, X, Y,
         Image.Canvas.Handle, 0, H - Y,
         SRCCOPY);
end;

// Reveal from bottom right
procedure WipeFromBottomRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, W - X, H - Y, X, Y,
         Image.Canvas.Handle, W - X, H - Y,
         SRCCOPY);
end;

// Reveal from top right
procedure WipeFromTopRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  BitBlt(DestCanvas.Handle, W - X, 0, X, Y,
         Image.Canvas.Handle, W - X, 0,
         SRCCOPY);
end;

// Appear and Contract to top left
procedure ShrinkToTopLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, 0, (2 * W) - X, (2 * H) - Y,
             Image.Canvas.Handle, 0, 0, X, Y,
             SRCCOPY);
end;

// Appear and Contract to bottom left
procedure ShrinkToBottomLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, 0, Y - H, (2 * W) - X, (2 * H) - Y,
             Image.Canvas.Handle, 0, H - Y, X, Y,
             SRCCOPY);
end;

// Appear and Contract to bottom right
procedure ShrinkToBottomRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, X - W, Y - H, (2 * W) - X, (2 * H) - Y,
             Image.Canvas.Handle, W - X, H - Y, X, Y,
             SRCCOPY);
end;

// Appear and Contract to top right
procedure ShrinkToTopRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, X - W, 0, (2 * W) - X, (2 * H) - Y,
             Image.Canvas.Handle, W - X, 0, X, Y,
             SRCCOPY);
end;

// Appear and Contract to middle
procedure ShrinkToCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  StretchBlt(DestCanvas.Handle, X - W, Y - H, (3 * W) - (2 * X), (3 * H) - (2 * Y),
             Image.Canvas.Handle, (W - X) div 2, (H - Y) div 2, X, Y,
             SRCCOPY);
end;

// Quarters Reveal in to middle
procedure QuartersWipeInToCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mX, mY: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mX := X div 2;
  mY := Y div 2;
  BitBlt(DestCanvas.Handle, 0, 0, mX, mY,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
  BitBlt(DestCanvas.Handle, 0, H - mY, mX, mY,
         Image.Canvas.Handle, 0, H - mY,
         SRCCOPY);
  BitBlt(DestCanvas.Handle, W - mX, H - mY, mX, mY,
         Image.Canvas.Handle, W - mX, H - mY,
         SRCCOPY);
  BitBlt(DestCanvas.Handle, W - mX, 0, mX, mY,
         Image.Canvas.Handle, W - mX, 0,
         SRCCOPY);
end;

// Quarters Expand to middle
procedure QuartersExpandToCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mX, mY, mW, mH: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mX := X div 2;
  mY := Y div 2;
  mW := W div 2;
  mH := H div 2;
  StretchBlt(DestCanvas.Handle, 0, 0, mX, mY,
             Image.Canvas.Handle, 0, 0, mW, mH,
             SRCCOPY);
  StretchBlt(DestCanvas.Handle, 0, H - mY, mX, mY,
             Image.Canvas.Handle, 0, mH, mW, mH,
             SRCCOPY);
  StretchBlt(DestCanvas.Handle, W - mX, H - mY, mX, mY,
             Image.Canvas.Handle, mW, mH, mW, mH,
             SRCCOPY);
  StretchBlt(DestCanvas.Handle, W - mX, 0, mX, mY,
             Image.Canvas.Handle, mW, 0, mW, mH,
             SRCCOPY);
end;

// Quarters Slide in to middle
procedure QuartersSlideInToCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mX, mY, mW, mH: Integer;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mX := X div 2;
  mY := Y div 2;
  mW := W div 2;
  mH := H div 2;
  BitBlt(DestCanvas.Handle, mX - mW, 0, mW, mH,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);
  BitBlt(DestCanvas.Handle, mW, mY - mH, mW, mH,
         Image.Canvas.Handle, mW, 0,
         SRCCOPY);
  BitBlt(DestCanvas.Handle, W - mX, mH, mW, mH,
         Image.Canvas.Handle, mW, mH,
         SRCCOPY);
  BitBlt(DestCanvas.Handle, 0, H - mY, mW, mH,
         Image.Canvas.Handle, 0, mH,
         SRCCOPY);
end;


// Curved Reveal from left
procedure CurvedWipeFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateRoundRectRgn(-2 * W, 0, 2 * X, H + 1, 2 * W, 2 * W);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Curved Reveal from right
procedure CurvedWipeFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateRoundRectRgn(W - 2 * X, 0, 3 * W, H + 1, 2 * W, 2 * W);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Curved Reveal from top
procedure CurvedWipeFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateRoundRectRgn(0, -2 * H, W + 1, 2 * Y, 2 * H, 2 * H);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Curved Reveal from bottom
procedure CurvedWipeFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateRoundRectRgn(0, H - 2 * Y, W + 1, 3 * H, 2 * H, 2 * H);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Curved reveal from top left
procedure CurvedWipeFromTopLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateRoundRectRgn(-W, -H, 3 * X div 2, 3 * Y div 2, 2 * W, 2 * H);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Curved reveal from top right
procedure CurvedWipeFromTopRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateRoundRectRgn(W - 3 * X div 2, -H, 2 * W, 3 * Y div 2, 2 * W, 2 * H);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Curved reveal from bottom left
procedure CurvedWipeFromBottomLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateRoundRectRgn(-W, H - 3 * Y div 2, 3 * X div 2, 2 * H, 2 * W, 2 * H);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Curved reveal from bottom right
procedure CurvedWipeFromBottomRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateRoundRectRgn(W - 3 * X div 2, H - 3 * Y div 2, 2 * W, 2 * H, 2 * W, 2 * H);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


procedure DoWipeEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer;
                       AShape: TXCustomShape; bReverse: Boolean; rScale: Single = 1);
var
  mW, mH: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
  iScaledX, iScaledY: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  mW := W div 2;
  mH := H div 2;

  if bReverse then
  begin
    Rgn := CreateRectRgn(0, 0, W, H);
    if AShape = xcsIECircularShape then
      TmpRgn := CreateRoundRectRgn(X - mW, Y - mH, 3 * mW - X, 3 * mH - Y, 9 * (W - X) div 5, 9 * (H - Y) div 5)
    else
    if (rScale <> 1) or (AShape = xcsIELetter) then
    begin
      iScaledX := Round(rScale * (W - X)) div 2;
      iScaledY := Round(rScale * (H - Y)) div 2;
      if AShape = xcsIELetter then
        TmpRgn := CreateRegionFromLetter(Rect(X - mW - iScaledX, Y - mH - iScaledY, 3 * mW - X + iScaledX, 3 * mH - Y + iScaledY), Progress, 60, 100)
      else
        TmpRgn := CreateCustomShape(AShape, Rect(X - mW - iScaledX, Y - mH - iScaledY, 3 * mW - X + iScaledX, 3 * mH - Y + iScaledY))
    end
    else
      TmpRgn := CreateCustomShape(AShape, Rect(X - mW, Y - mH, 3 * mW - X, 3 * mH - Y));
    CombineRgn(Rgn, Rgn, TmpRgn, RGN_XOR);
    DeleteObject(TmpRgn)
  end
  else
  begin      
    if AShape = xcsIECircularShape then
      Rgn := CreateRoundRectRgn(mW - X, mH - Y, mW + X, mH + Y, 9 * X div 5, 9 * Y div 5)
    else      
    if (rScale <> 1) or (AShape = xcsIELetter) then
    begin
      iScaledX := Round(rScale * X);
      iScaledY := Round(rScale * Y);
      if AShape = xcsIELetter then
        Rgn := CreateRegionFromLetter(Rect(mW - iScaledX, mH - iScaledY, mW + iScaledX, mH + iScaledY), Progress, 0, 60)
      else
        Rgn := CreateCustomShape(AShape, Rect(mW - iScaledX, mH - iScaledY, mW + iScaledX, mH + iScaledY))
    end
    else
      Rgn := CreateCustomShape(AShape, Rect(mW - X, mH - Y, mW + X, mH + Y));
  end;

  try
    if (AShape = xcsCross) or (AShape = xcsIELetter) or (AShape = xcsDoubleHeart) then
      BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);

    OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  finally
    DeleteObject(Rgn);
  end;
end;



// Circular Wipe from Center
procedure CircularWipeFromCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIECircularShape, False);
end;

// Circular Wipe to Center
procedure CircularWipeToCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIECircularShape, True);
end;


// Heart Wipe from Center
procedure HeartWipeOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  if IEGlobalSettings().TransitionsDrawAlternative then
    DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsDoubleHeart, False, 1.3)
  else
    DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsHeart, False, 1.3)
end;

// Heart Wipe to Center
procedure HeartWipeInEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin          
  if IEGlobalSettings().TransitionsDrawAlternative then
    DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsDoubleHeart, True, 1.3)
  else
    DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsHeart, True, 1.3)
end;



// 5 Point Star Wipe from Center
procedure Star5WipeOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar5, False, 1.6);
end;

// 5 Point Star Wipe to Center
procedure Star5WipeInEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar5, True, 1.6);
end;


// 6 Point Star Wipe from Center
procedure Star6WipeOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar6, False, 1.3);
end;

// 6 Point Star Wipe to Center
procedure Star6WipeInEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar6, True, 1.3);
end;


// Word Wipe from Center
procedure WordWipeOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  rSize: Single;
begin
  rSize := 3;
  if IEGlobalSettings().TransitionsDrawAlternative then
    rSize := 6;
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIELetter, False, rSize);
end;

// Word Wipe to Center
procedure WordWipeInEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer); 
var
  rSize: Single;
begin
  rSize := 3;
  if IEGlobalSettings().TransitionsDrawAlternative then
    rSize := 6;
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIELetter, True, rSize);
end;

// Explosion Wipe from Center
procedure ExplosionWipeOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  AShape: TXCustomShape;
begin
  if IEGlobalSettings().TransitionsDrawAlternative then
    AShape := xcsExplosion_2
  else
    AShape := xcsExplosion;
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, AShape, False);
end;

// Explosion Wipe to Center
procedure ExplosionWipeInEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  AShape: TXCustomShape;
begin
  if IEGlobalSettings().TransitionsDrawAlternative then
    AShape := xcsExplosion_2
  else
    AShape := xcsExplosion;
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, AShape, True);
end;


// Cross Wipe from Center
procedure CrossWipeOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsCross, False, 3.6);
end;

// Cross Wipe to Center
procedure CrossWipeInEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWipeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsCross, True, 3.5);
end;   

// Bars in from left
procedure BarsInFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 0, W, H, 2, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bars in from right
procedure BarsInFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 0, W, H, 1, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bars right then left
procedure BarsRightThenLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 0, W, H, 5, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bars left then right
procedure BarsLeftThenRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 0, W, H, 4, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bars from both sides
procedure BarsFromBothSidesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(X, 0, W, H, 3, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;   

// Bars from top
procedure BarsFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(0, 2 * Y, W, H, 0, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bars from bottom
procedure BarsFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(0, 2 * Y, W, H, 0, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bars top then bottom
procedure BarsTopThenBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(0, 2 * Y, W, H, 0, 4);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;    

// Bars from top and bottom
procedure BarsFromTopAndBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(0, Y, W, H, 0, 3);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bars bottom then top
procedure BarsBottomThenTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(0, 2 * Y, W, H, 0, 5);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Criss Cross reveal from top left
procedure CrisscrossWipeFromTopLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 2, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal from top right
procedure CrisscrossWipeFromTopRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 1, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;  

// Criss Cross reveal from bottom left
procedure CrisscrossWipeFromBottomLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 2, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal from bottom right
procedure CrisscrossWipeFromBottomRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 1, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal bounce from top left
procedure CrisscrossWipeBounceFromTopLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 4, 4);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal bounce from bottom left
procedure CrisscrossWipeBounceFromBottomLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 4, 5);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal bounce from top right
procedure CrisscrossWipeBounceFromTopRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 5, 4);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal bounce from bottom right
procedure CrisscrossWipeBounceFromBottomRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 5, 5);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal from right top and bottom
procedure CrisscrossWipeFromRightTopAndBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(X, Y, W, H, 1, 3);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal from left top and bottom
procedure CrisscrossWipeFromLeftTopAndBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(X, Y, W, H, 2, 3);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal from left right and bottom
procedure CrisscrossWipeFromLeftRightAndBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(X, Y, W, H, 3, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal from left right and top
procedure CrisscrossWipeFromLeftRightAndTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(X, Y, W, H, 3, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal from top left right and bottom
procedure CrisscrossWipeFromTopLeftRightAndBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(X, Y, W, H, 3, 3);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Criss Cross reveal from bottom left top right
procedure CrisscrossWipeFromBottomLeftTopRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, 1, 1);
  TmpRgn := CreateBarRgn(2 * X, 2 * Y, W, H, 2, 2);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_AND);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Uneven shred from left
procedure ShreddedFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, 0, W, H, 2, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from right
procedure ShreddedFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, 0, W, H, 1, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;     

// Uneven shred from top
procedure ShreddedFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(0, Y, W, H, 0, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Unven shred from bottom
procedure ShreddedFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(0, Y, W, H, 0, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from top and left
procedure ShreddedFromTopAndLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 2, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from top and right
procedure ShreddedFromTopAndRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 1, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;   

// Uneven shred from bottom and left
procedure ShreddedFromBottomAndLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 2, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from bottom and right
procedure ShreddedFromBottomAndRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 1, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;   

// Uneven shred from horiz and left
procedure ShreddedFromHorizonAndLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 2, 3);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from horiz and right
procedure ShreddedFromHorizonAndRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 1, 3);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from bottom and vert middle
procedure ShreddedFromBottomAndVerticalCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 3, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from top and vert middle
procedure ShreddedFromTopAndVerticalCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 3, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from centre
procedure ShreddedFromCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 3, 3);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred to centre
procedure ShreddedToCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, Y, W, H, 4, 4);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred out from middle (horiz)
procedure ShreddedOutFromVerticalCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, 0, W, H, 3, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred in to middle (horiz)
procedure ShreddedInToVerticalCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(X, 0, W, H, 4, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred from horizon
procedure ShreddedOutFromHorizonEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(0, Y, W, H, 0, 3);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven shred in to horizon
procedure ShreddedInToHorizonEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePourRgn(0, Y, W, H, 0, 4);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Reveal diagonal from top left
procedure WipeDiagonalFromTopLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePolygonRgnEx([0, 0, 2 * X, 0, 0, 2 * Y]);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Reveal diagonal from top right
procedure WipeDiagonalFromTopRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePolygonRgnEx([W, 0, W - 2 * X, 0, W, 2 * Y]);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Reveal diagonal from bottom left
procedure WipeDiagonalFromBottomLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePolygonRgnEx([0, H, 2 * X, H, 0, H - 2 * Y]);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Reveal diagonal from bottom right
procedure WipeDiagonalFromBottomRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePolygonRgnEx([W, H, W - 2 * X, H, W, H - 2 * Y]);
  SelectClipRgn(DestCanvas.Handle, Rgn);
  DeleteObject(Rgn);
  DestCanvas.Draw(0, 0, Image);
  SelectClipRgn(DestCanvas.Handle, 0);
end;

// Diagonal sweep from top left bottom right clockwise
procedure DiagonalSweepClockwiseEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn, TmpRgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePolygonRgnEx([W, 0, 0, 0, W, Y]);
  TmpRgn := CreatePolygonRgnEx([W, H, 0, H, 0, H - Y]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Diagonal sweep from top left bottom right anticlockwise
procedure DiagonalSweepCounterClockwiseEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn, TmpRgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreatePolygonRgnEx([0, H, 0, 0, X, H]);
  TmpRgn := CreatePolygonRgnEx([W, H, W, 0, W - X, 0]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Starburst clockwise from center
procedure StarburstClockwiseFromCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW, mH: Integer;
  Rgn, TmpRgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  Rgn := CreatePolygonRgnEx([mW, mH, 0, 0, X, 0]);
  TmpRgn := CreatePolygonRgnEx([mW, mH, 0, H, 0, H - Y]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([mW, mH, W, H, W - X, H]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([mW, mH, W, 0, W, Y]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Starburst anticlockwise from center
procedure StarburstCounterClockwiseFromCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW, mH: Integer;
  Rgn, TmpRgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  Rgn := CreatePolygonRgnEx([mW, mH, 0, 0, 0, Y]);
  TmpRgn := CreatePolygonRgnEx([mW, mH, 0, H, X, H]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([mW, mH, W, H, W, H - Y]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([mW, mH, W, 0, W - X, 0]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Triangular shred
procedure ExpandingTrianglesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  sW, sH, dW, dH: Integer;
  Rgn, TmpRgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  sW := (W div 10) + 1;
  sH := (H div 10) + 1;
  dW := MulDiv(sW, Progress, 50);
  dH := MulDiv(sH, Progress, 50);
  Rgn := 0;
  X := 0;
  while X < W do
  begin
    Inc(X, sW);
    Y := 0;
    while Y < H + sH do
    begin
      Inc(Y, sH);

      if IEGlobalSettings().TransitionsDrawAlternative then
        TmpRgn := CreatePolygonRgnEx([X - dW, Y + dH,
                                      X, Y - dH,
                                      X + dW, Y + dH])
      else
        TmpRgn := CreatePolygonRgnEx([X - dW, Y - dH,
                                      X, Y + dH,
                                      X + dW, Y - dH]);

      if Rgn <> 0 then
      begin
        CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end
      else
        Rgn := TmpRgn;
      Inc(Y, sH);
    end;
    Inc(X, sW);
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Speckle appear from left
procedure SpeckledWipeFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateSwarmRgn(X, Y, W, H, 2, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;  

// Speckle appear from right
procedure SpeckledWipeFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateSwarmRgn(X, Y, W, H, 1, 0);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Speckle appear from top
procedure SpeckledWipeFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateSwarmRgn(X, Y, W, H, 0, 2);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;  

// Speckle appear from bottom
procedure SpeckledWipeFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  Rgn: HRGN;
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  Rgn := CreateSwarmRgn(X, Y, W, H, 0, 1);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Push and squeeze left
procedure PushAndSqueezeLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  with DestCanvas do
  begin
    StretchBlt(Handle, 0, 0, W - X, H, Handle, 0, 0, W, H, SRCCOPY);
    BitBlt(Handle, W - X, 0, X, H, Image.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

// Push and squeeze right
procedure PushAndSqueezeRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  with DestCanvas do
  begin
    StretchBlt(Handle, X, 0, W - X, H, Handle, 0, 0, W, H, SRCCOPY);
    BitBlt(Handle, 0, 0, X, H, Image.Canvas.Handle, W - X, 0, SRCCOPY);
  end;
end;        

// Push and sqeeze up
procedure PushAndSqueezeUpEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  with DestCanvas do
  begin
    StretchBlt(Handle, 0, 0, W, H - Y, Handle, 0, 0, W, H, SRCCOPY);
    BitBlt(Handle, 0, H - Y, W, Y, Image.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;             

// Push and sqeeze down
procedure PushAndSqueezeDownEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  with DestCanvas do
  begin
    StretchBlt(Handle, 0, Y, W, H - Y, Handle, 0, 0, W, H, SRCCOPY);
    BitBlt(Handle, 0, 0, W, Y, Image.Canvas.Handle, 0, H - Y, SRCCOPY);
  end;
end;


// Push left
procedure PushLeftAndSlideOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  with DestCanvas do
  begin
    BitBlt(Handle, 0, 0, W - X, H,
           Handle, X, 0,
           SRCCOPY);
    BitBlt(Handle, W - X, 0, X, H,
           Image.Canvas.Handle, 0, 0,
           SRCCOPY);
  end;
end;

// Push right
procedure PushRightAndSlideOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  with DestCanvas do
  begin
    BitBlt(Handle, X, 0, W - X, H,
           Handle, 0, 0,
           SRCCOPY);
    BitBlt(Handle, 0, 0, X, H,
           Image.Canvas.Handle, W - X, 0,
           SRCCOPY);
  end;
end;

// Push up
procedure PushUpAndSlideOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  with DestCanvas do
  begin
    BitBlt(Handle, 0, 0, W, H - Y, Handle, 0, Y, SRCCOPY);
    BitBlt(Handle, 0, H - Y, W, Y, Image.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

// Push down
procedure PushDownAndSlideOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  with DestCanvas do
  begin
    BitBlt(Handle, 0, Y, W, H - Y, Handle, 0, 0, SRCCOPY);
    BitBlt(Handle, 0, 0, W, Y, Image.Canvas.Handle, 0, H - Y, SRCCOPY);
  end;
end;


// Blind vertically
procedure VerticalBlindsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  S, D: Integer;
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  S := (W div 40) + 1;
  D := MulDiv(S, Progress, 100);
  ComplexRegion := TComplexRegion.Create;
  try
    X := 0;
    while X < W do
    begin
      Inc(X, S);
      ComplexRegion.AddRect(X - D, 0, X + D, H);
      Inc(X, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Blind horizontally
procedure HorizontalBlindsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  S, D: Integer;
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  S := (H div 40) + 1;
  D := MulDiv(S, Progress, 100);
  ComplexRegion := TComplexRegion.Create;
  try
    Y := 0;
    while Y < H do
    begin
      Inc(Y, S);
      ComplexRegion.AddRect(0, Y - D, W, Y + D);
      Inc(Y, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Uneven blind from left
procedure UnevenBlindsFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  S, D: Integer;
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  if X = 0 then Exit;
  S := X;
  D := MulDiv(S, Progress, 100);
  ComplexRegion := TComplexRegion.Create;
  try
    X := 0;
    while X < W do
    begin
      ComplexRegion.AddRect(X, 0, X + D, H);
      Inc(X, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven blind from right
procedure UnevenBlindsFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  S, D: Integer;
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  if X = 0 then Exit;
  S := X;
  D := MulDiv(S, Progress, 100);
  ComplexRegion := TComplexRegion.Create;
  try
    X := W;
    while X > 0 do
    begin
      ComplexRegion.AddRect(X - D, 0, X, H);
      Dec(X, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven blind from top
procedure UnevenBlindsFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  S, D: Integer;
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  if Y = 0 then Exit;
  S := Y;
  D := MulDiv(S, Progress, 100);
  ComplexRegion := TComplexRegion.Create;
  try
    Y := 0;
    while Y < H do
    begin
      ComplexRegion.AddRect(0, Y, W, Y + D);
      Inc(Y, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Uneven blind from bottom
procedure UnevenBlindsFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  S, D: Integer;
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  if Y = 0 then Exit;
  S := Y;
  D := MulDiv(S, Progress, 100);
  ComplexRegion := TComplexRegion.Create;
  try
    Y := H;
    while Y > 0 do
    begin
      ComplexRegion.AddRect(0, Y - D, W, Y);
      Dec(Y, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;



// Rectangular shred
procedure ExpandingRectanglesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  S, D: Integer;
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  S := (Min(W, H) div 10) + 1;
  D := MulDiv(S, Progress, 100);
  ComplexRegion := TComplexRegion.Create;
  try
    X := 0;
    while X < W do
    begin
      Inc(X, S);
      Y := 0;
      while Y < H do
      begin
        Inc(Y, S);
        ComplexRegion.AddRect(X - D, Y - D, X + D, Y + D);
        Inc(Y, S);
      end;
      Inc(X, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// iDirection is -1 or 1
// bHalfScreen: Turns from the circle and does a full loop
// bAlternativing: If true, it is a full screen rotate, if false, it opens from a central point (bHalfScreen = True)
// bDoubleRotate: If true, rotates twice, cycling first through a black screen
// bIncludeText: Outputs a word using IEGlobalSettings().WordTransitionParams
procedure DoRotationalSweepEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer;
                                  iDirection : Integer; bHalfScreen, bAlternating, bDoubleRotate : Boolean;
                                  bIncludeText : Boolean = False);
const
  Size_As_Percent_Of_Dim = 70; // Text is 70% of width/height

  procedure _DoEffect(AScreen, AImage: TBitmap; iProgress: Integer; bReverse : Boolean = False);
  var
    mX, mY: Integer;
    mPI: Extended;
    Rgn, Rgn2, WrdRgn: HRGN;
    W, H, X, Y: Integer;
  begin
    CalcParams(ARect, iProgress, W, H, X, Y);
    mX := W div 2;
    mY := H div 2;
    mPI := PI / 2;
    if bHalfScreen then
    begin
      Rgn := CreateSliceRgn(mX, mY, Ceil(Hypot(mX, mY)), -mPI, (iDirection * PI * iProgress / 50) - mPI, iProgress, True);
    end
    else
    begin
      Rgn := CreateSliceRgn(mX, mY, Ceil(Hypot(mX, mY)), -mPI, (iDirection * PI * iProgress / 100) - mPI, iProgress div 2, not bReverse);
      if bAlternating then
        Rgn2 := CreateSliceRgn(mX, mY, Ceil(Hypot(mX, mY)), -mPI, (iDirection * PI * iProgress / 100) - mPI, iProgress div 2, False)
      else
        Rgn2 := CreateSliceRgn(mX, mY, Ceil(Hypot(mX, mY)), -mPI, (-1 * iDirection * PI * iProgress / 100) - mPI, iProgress div 2, not bReverse);
      CombineRgn(Rgn, Rgn, Rgn2, RGN_OR);
      DeleteObject(Rgn2);
    end;

    if bIncludeText and (Progress > 50) then
    begin
      WrdRgn := CreateRegionFromLetter(Rect(MulDiv(W, 50 - Size_As_Percent_Of_Dim div 2, 100),
                                            MulDiv(H, 50 - Size_As_Percent_Of_Dim div 2, 100),
                                            MulDiv(W, 50 + Size_As_Percent_Of_Dim div 2, 100),
                                            MulDiv(H, 50 + Size_As_Percent_Of_Dim div 2, 100)),
                                       Progress, 0, 100, True);
      CombineRgn(Rgn, Rgn, WrdRgn, RGN_OR);
      DeleteObject(WrdRgn) ;
    end;

    OutputRgnToCanvas(DestCanvas, W, H, Rgn, AImage);
    DeleteObject(Rgn);
  end;

var
  ABlack: TBitmap;
  bReverse : Boolean;  
  WrdRgn : HRGN;
begin
  if bDoubleRotate = False then
  begin
    _DoEffect(Screen, Image, Progress);
  end
  else
  begin
    ABlack := TBitmap.create;
    try
      ABlack.Width  := Screen.Width;
      ABlack.Height := Screen.Height;
      ABlack.Canvas.Brush.Color := clBlack;
      ABlack.Canvas.FillRect(Rect(0, 0, ABlack.Width, ABlack.Height));
      if Progress < 50 then
      begin
        if bIncludeText  then
        begin
          WrdRgn := CreateRegionFromLetter(Rect(MulDiv(ABlack.Width, 50 - Size_As_Percent_Of_Dim div 2, 100),
                                                MulDiv(ABlack.Height, 50 - Size_As_Percent_Of_Dim div 2, 100),
                                                MulDiv(ABlack.Width, 50 + Size_As_Percent_Of_Dim div 2, 100),
                                                MulDiv(ABlack.Height, 50 + Size_As_Percent_Of_Dim div 2, 100)),
                                           Progress, 0, 100, True);
          OutputRgnToCanvas(ABlack.Canvas, ABlack.Width, ABlack.Height, WrdRgn, Image);
          DeleteObject(WrdRgn) ;
        end;

        _DoEffect(Screen, ABlack, Progress * 2)
      end
      else
      begin
        DestCanvas.Brush.Color := clBlack;
        DestCanvas.FillRect(Rect(0, 0, Screen.Width, Screen.Height));

        bReverse := (bHalfScreen or bAlternating) = False;
        _DoEffect(ABlack, Image, Progress * 2 - 100, bReverse);
      end;
    finally
      ABlack.Free;
    end;
  end;
end;



// Half Sweep clockwise
procedure HalfSweepClockwise(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  bDoubleRotate: Boolean;
begin
  bDoubleRotate := IEGlobalSettings().TransitionsDrawAlternative;
  DoRotationalSweepEffect(DestCanvas, Screen, Image, ARect, Progress, 1, True, False, bDoubleRotate);
end;

// Half Sweep anticlockwise
procedure HalfSweepAntiClockwise(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  bDoubleRotate: Boolean;
begin
  bDoubleRotate := IEGlobalSettings().TransitionsDrawAlternative;
  DoRotationalSweepEffect(DestCanvas, Screen, Image, ARect, Progress, -1, True, False, bDoubleRotate);
end;

// Full Clockwise Sweep
procedure FullSweepClockwise(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  bDoubleRotate: Boolean;
begin
  bDoubleRotate := IEGlobalSettings().TransitionsDrawAlternative;
  DoRotationalSweepEffect(DestCanvas, Screen, Image, ARect, Progress, 1, False, True, bDoubleRotate);
end;

// Full Expanding Sweep clockwise
procedure FullExpandingSweepClockwise(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  bDoubleRotate: Boolean;
begin
  bDoubleRotate := IEGlobalSettings().TransitionsDrawAlternative;
  DoRotationalSweepEffect(DestCanvas, Screen, Image, ARect, Progress, 1, False, False, bDoubleRotate);
end;

// Half Sweep clockwise with Word
procedure WordHalfSweep(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  iDirection : Integer;
begin
  iDirection := 1;
  if IEGlobalSettings().TransitionsDrawAlternative then
    iDirection := -1;
  DoRotationalSweepEffect(DestCanvas, Screen, Image, ARect, Progress, iDirection, True, False, True, True);
end;

// Full Clockwise Sweep with Word
procedure WordFullSweep(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  iDirection : Integer;
begin
  iDirection := 1;
  if IEGlobalSettings().TransitionsDrawAlternative then
    iDirection := -1;
  DoRotationalSweepEffect(DestCanvas, Screen, Image, ARect, Progress, iDirection, False, True, True, True);
end;

// Full Expanding Sweep with Word
procedure WordFullExpandingSweep(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoRotationalSweepEffect(DestCanvas, Screen, Image, ARect, Progress, 1, False, False, True, True);
end;



procedure DoWideBarsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer; bHorz, bReverse : Boolean);
const
  Bar_Count = 12;   // Maximum of 30
var
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
  iBarSize, iMaxBarLength: Integer;
  iProgressBlock: Integer;
  I: Integer;
  iBarIndex: Integer;
  iBarLength: Integer;
  iBarCount: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  iBarCount := Bar_Count;
  if IEGlobalSettings().TransitionsDrawAlternative then
    iBarCount := Bar_Count div 2;

  if bHorz then
  begin
    iBarSize      := H div iBarCount + 1;
    iMaxBarLength := W;
  end
  else
  begin
    iBarSize := W div iBarCount + 1;
    iMaxBarLength := H;
  end;
  iProgressBlock := 100 div iBarCount;
  if iBarSize = 0 then
    exit;

  if fTransitionInitialized = False then
  begin
    InitializeRandomList(iBarCount - 1);
    fTransitionInitialized := True;
  end;

  ComplexRegion := TComplexRegion.Create;
  try
    for I := 0 to iBarCount - 1 do
    begin
      iBarIndex := GetRandomListIndex(I);

      iBarLength := 0;
      if Progress > (I + 1) * iProgressBlock then
        iBarLength := iMaxBarLength
      else
      if Progress > I * iProgressBlock then
        iBarLength := MulDiv(iMaxBarLength, Progress - I * iProgressBlock, iProgressBlock);

      if iBarLength > 0 then
      begin
        if bHorz then
        begin
          if bReverse then
            // RIGHT TO LEFT
            ComplexRegion.AddRect(W - iBarLength, iBarIndex * iBarSize, W, (iBarIndex + 1) * iBarSize)
          else
            // LEFT TO RIGHT
            ComplexRegion.AddRect(0, iBarIndex * iBarSize, iBarLength, (iBarIndex + 1) * iBarSize);
        end
        else
        // VERTICAL
        begin   
          if bReverse then
            // BOTTOM TO TOP
            ComplexRegion.AddRect(iBarIndex * iBarSize, H - iBarLength, (iBarIndex + 1) * iBarSize, H)
          else
            // TOP TO BOTTOM
            ComplexRegion.AddRect(iBarIndex * iBarSize, 0, (iBarIndex + 1) * iBarSize,  iBarLength);
        end;
      end;
    end;

    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
  end;

  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Random Bars from the Left
procedure WideBarsFromLeft(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWideBarsEffect(DestCanvas, Screen, Image, ARect, Progress, True, False);
end;

// Random Bars from the Right
procedure WideBarsFromRight(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWideBarsEffect(DestCanvas, Screen, Image, ARect, Progress, True, True);
end;

// Random Bars from the Top
procedure WideBarsFromTop(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWideBarsEffect(DestCanvas, Screen, Image, ARect, Progress, False, False);
end;

// Random Bars from the Bottom
procedure WideBarsFromBottom(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoWideBarsEffect(DestCanvas, Screen, Image, ARect, Progress, False, True);
end;


procedure DoRotateEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer;
                         var Points: array of TPoint; xOrg, yOrg: Integer; PtCount : Integer;
                         bClockwise: Boolean);
var
  Angle: Extended;
  Rgn: HRGN;
  W, H : Integer;
begin                   
  W := ARect.Right - ARect.Left;
  H := ARect.Bottom - ARect.Top;
  Angle := PI * Progress / 50;
  if bClockwise = False then
    Angle := -Angle;
  RotatePoints(Points, xOrg, yOrg, Angle);
  Rgn := CreatePolygonRgn(Points, PtCount, WINDING);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;



procedure RotationalRectangleEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer;
                                    bClockwise: Boolean);
var
  mW, mH, mX, mY: Integer;
  Pts: array[1..4] of TPoint;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  mX := X div 2;
  mY := Y div 2;
  Pts[1].X := mW - mX;
  Pts[1].Y := mH - mY;
  Pts[2].X := mW + mX;
  Pts[2].Y := mH - mY;
  Pts[3].X := mW + mX;
  Pts[3].Y := mH + mY;
  Pts[4].X := mW - mX;
  Pts[4].Y := mH + mY;

  DoRotateEffect(DestCanvas, Screen, Image, ARect, Progress, Pts, mW, mH, 4, bClockwise);
end;


// Rotational star in center
procedure RotationalStarEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer; bClockwise: Boolean);
var
  mW, mH, mX, mY, dX, dY: Integer;
  Pts: array[1..8] of TPoint;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  mX := X div 2;
  mY := Y div 2;
  dX := 2 * X;
  dY := 2 * Y;
  Pts[1].X := mW - dX;
  Pts[1].Y := mH;
  Pts[2].X := mW - mX;
  Pts[2].Y := mH - mY;
  Pts[3].X := mW;
  Pts[3].Y := mH - dY;
  Pts[4].X := mW + mX;
  Pts[4].Y := mH - mY;
  Pts[5].X := mW + dX;
  Pts[5].Y := mH;
  Pts[6].X := mW + mX;
  Pts[6].Y := mH + mY;
  Pts[7].X := mW;
  Pts[7].Y := mH + dY;
  Pts[8].X := mW - mX;
  Pts[8].Y := mH + mY;
       
  DoRotateEffect(DestCanvas, Screen, Image, ARect, Progress, Pts, mW, mH, 8, bClockwise);
end;


// Spiral rectangle
procedure SpirallingRectangleEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer; bClockwise: Boolean);
var
  mW, mH, mX, mY: Integer;
  Pts: array[1..4] of TPoint;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  mX := X div 2;
  mY := Y div 2;
  Pts[1].X := mW - mX;
  Pts[1].Y := mH - mY;
  Pts[2].X := mW + mX;
  Pts[2].Y := mH - mY;
  Pts[3].X := mW + mX;
  Pts[3].Y := mH + mY;
  Pts[4].X := mW - mX;
  Pts[4].Y := mH + mY;

  DoRotateEffect(DestCanvas, Screen, Image, ARect, Progress, Pts, X, Y, 4, bClockwise);
end;



// Rotational Triangular Wipe
procedure RotationalTriangularWipeEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW, mH: Integer;
  Pts: array[1..3] of TPoint;
  W, H, X, Y: Integer;
  iSize: Integer;
  bClockwise: Boolean;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  iSize := imax(X, Y) div 2;
  mW := W div 2;
  mH := H div 2;

  Pts[1].X := mW;
  Pts[1].Y := mH - iSize;
  Pts[2].X := mW + iSize;
  Pts[2].Y := mH + iSize;
  Pts[3].X := mW - iSize;
  Pts[3].Y := mH + iSize;

  bClockwise := not IEGlobalSettings().TransitionsDrawAlternative;
  DoRotateEffect(DestCanvas, Screen, Image, ARect, Progress, Pts, mW, mH, 3, bClockwise);
end;

procedure DoExpandShapeEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer;
                              AShape: TXCustomShape; rSize: Single);
var
  S, D: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  S := (Min(W, H) div 10) + 1;
  D := MulDiv(Round(rSize * S), Progress, 200);
  Rgn := 0;
  X := 0;
  while X < W do
  begin
    Inc(X, S);
    Y := 0;
    while Y < H do
    begin
      Inc(Y, S);  
      if AShape = xcsIELetter then
        TmpRgn := CreateRegionFromLetter(Rect(X - D, Y - D, X + D, Y + D), Progress, 0, 50)
      else
      if AShape = xcsIECircularShape then
        TmpRgn := CreateEllipticRgn(X - D, Y - D, X + D, Y + D)
      else
        TmpRgn := CreateCustomShape(AShape, Rect(X - D, Y - D, X + D, Y + D));
      if Rgn <> 0 then
      begin
        CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end
      else
        Rgn := TmpRgn;
      Inc(Y, S);
    end;
    Inc(X, S);
  end;

  if Rgn <> 0 then
  try
    if (AShape = xcsIELetter) or (AShape = xcsDoubleHeart) then
      BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);

    OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  finally
    DeleteObject(Rgn);
  end;
end;

// Expanding Circles
procedure ExpandingCirclesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoExpandShapeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIECircularShape, 3);
end;

// Expanding Hearts
procedure ExpandingHeartsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  if IEGlobalSettings().TransitionsDrawAlternative then
    DoExpandShapeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsDoubleHeart, 3)
  else
    DoExpandShapeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsHeart, 3)
end;

// Expanding 5 Point Stars
procedure ExpandingStar5sEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoExpandShapeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar5, 4);
end;

// Expanding 6 Point Stars
procedure ExpandingStar6sEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoExpandShapeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar6, 4);
end;

// Expanding Letters of Word
procedure ExpandingWordEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  rSize: Single;
begin
  rSize := 6;
  if IEGlobalSettings().TransitionsDrawAlternative then
    rSize := 12;

  DoExpandShapeEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIELetter, rSize);
end;

// Expanding Explosions
procedure ExpandingExplosionsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  AShape: TXCustomShape;
begin
  if IEGlobalSettings().TransitionsDrawAlternative then
    AShape := xcsExplosion_2
  else
    AShape := xcsExplosion;
  DoExpandShapeEffect(DestCanvas, Screen, Image, ARect, Progress, AShape, 4);
end;

// Expanding Lightning Bolts
procedure ExpandingLightningBoltsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  AShape: TXCustomShape;
begin
  if IEGlobalSettings().TransitionsDrawAlternative then
    AShape := xcsLightningRight
  else
    AShape := xcsLightningLeft;
  DoExpandShapeEffect(DestCanvas, Screen, Image, ARect, Progress, AShape, 6);
end;


// Reveal V from left
procedure ArrowWipeFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mH: Integer;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mH := H div 2;
  Rgn := CreatePolygonRgnEx([0, mH - Y, 2 * X, mH, 0, mH + Y]);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Reveal V from right
procedure ArrowWipeFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mH: Integer;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mH := H div 2;
  Rgn := CreatePolygonRgnEx([W, mH - Y, W - 2 * X, mH, W, mH + Y]);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Reveal V from top
procedure ArrowWipeFromTopEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW: Integer;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  Rgn := CreatePolygonRgnEx([mW - X, 0, mW, 2 * Y, mW + X, 0]);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Reveal V from bottom
procedure ArrowWipeFromBottomEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW: Integer;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  Rgn := CreatePolygonRgnEx([mW - X, H, mW, H - 2 * Y, mW + X, H]);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bow Tie Horizontal
procedure HorizontalBowTieWipeEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mH: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mH := H div 2;
  Rgn := CreatePolygonRgnEx([0, mH - Y, X, mH, 0, mH + Y]);
  TmpRgn := CreatePolygonRgnEx([W, mH - Y, W - X, mH, W, mH + Y]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Bow Tie Vertical
procedure VerticalBowTieWipeEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  Rgn := CreatePolygonRgnEx([mW - X, 0, mW, 2 * Y, mW + X, 0]);
  TmpRgn := CreatePolygonRgnEx([mW - X, H, mW, H - 2 * Y, mW + X, H]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Diagonal Cross In
procedure DiagonalCrossToCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW, mH, mX, mY: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  mX := X div 2;
  mY := Y div 2;
  Rgn := CreatePolygonRgnEx([0, mH - mY, mX, mH, 0, mH + mY]);
  TmpRgn := CreatePolygonRgnEx([W, mH - mY, W - mX, mH, W, mH + mY]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([mW - mX, 0, mW, mY, mW + mX, 0]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([mW - mX, H, mW, H - mY, mW + mX, H]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Diagonal Cross Out
procedure DiagonalCrossFromCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW, mH, mX, mY: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  X := W - X;
  Y := H - Y;
  mW := W div 2;
  mH := H div 2;
  mX := X div 2;
  mY := Y div 2;
  Rgn := CreateRectRgn(0, 0, W, H);
  TmpRgn := CreatePolygonRgnEx([0, mH - mY, mX, mH, 0, mH + mY]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_XOR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([W, mH - mY, W - mX, mH, W, mH + mY]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_XOR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([mW - mX, 0, mW, mY, mW + mX, 0]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_XOR);
  DeleteObject(TmpRgn);
  TmpRgn := CreatePolygonRgnEx([mW - mX, H, mW, H - mY, mW + mX, H]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_XOR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

procedure DoHorzZigzagEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer; bReverse : Boolean);
var
  mH, mY, sH, sW: Integer;
  YY: array[1..4] of Integer;
  U, D, I, J: Integer;
  Pts: array[1..24] of TPoint;
  Rgn: HRGN;
  W, H, X, Y: Integer;
  iProgress: Integer;
begin
  iProgress := Progress;
  if bReverse then
    iProgress := 100 - Progress;
  CalcParams(ARect, iProgress, W, H, X, Y);

  sH := H div 10;
  sW := W div 10;       
  if IEGlobalSettings().TransitionsDrawAlternative then
  begin
    sH := sH * 2;
    sW := sW * 2;
  end;
  
  mH := H div 2;
  mY := Y div 2 + MulDiv(sH, iProgress, 100);
  YY[1] := mH + sH - mY;
  YY[2] := mH - sH - mY;
  YY[3] := mH + sH + mY;
  YY[4] := mH - sH + mY;
  X := 0;
  U := Low(Pts);
  D := High(Pts);
  for I := 1 to 6 do
  begin
    for J := 1 to 2 do
    begin
      Pts[U].X := X;
      Pts[U].Y := YY[J];
      Inc(U);
      Pts[D].X := X;
      Pts[D].Y := YY[J + 2];
      Dec(D);
      Inc(X, sW);
    end;
  end;

  Rgn := CreatePolygonRgn(Pts, 24, WINDING);
  try
    with DestCanvas do
    begin
      if bReverse then
        BitBlt(Handle, 0, 0, W, H, Image.Canvas.Handle, 0, 0, SRCCOPY);

      SelectClipRgn(Handle, Rgn);
      if bReverse then
        BitBlt(Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY)
      else
        BitBlt(Handle, 0, 0, W, H, Image.Canvas.Handle, 0, 0, SRCCOPY);
      SelectClipRgn(Handle, 0);
    end;
  finally
    DeleteObject(Rgn);
  end;
end;

// Zigzag Wipe from Horizon
procedure ZigzagWipeFromHorizonEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoHorzZigzagEffect(DestCanvas, Screen, Image, ARect, Progress, False);
end;

// Zigzag Wipe To Horizon
procedure ZigzagWipeToHorizonEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoHorzZigzagEffect(DestCanvas, Screen, Image, ARect, Progress, True);    
end;

procedure DoVerticalZigzagEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer; bReverse : Boolean);
var
  mW, mX, sH, sW: Integer;
  XX: array[1..4] of Integer;
  L, R, I, J: Integer;
  Pts: array[1..24] of TPoint;
  Rgn: HRGN;
  W, H, X, Y: Integer;
  iProgress: Integer;
begin
  iProgress := Progress;
  if bReverse then
    iProgress := 100 - Progress;
  CalcParams(ARect, iProgress, W, H, X, Y);

  sH := H div 10;
  sW := W div 10;
  if IEGlobalSettings().TransitionsDrawAlternative then
  begin
    sH := sH * 2;
    sW := sW * 2;
  end;

  mW := W div 2;
  mX := X div 2 + MulDiv(sW, iProgress, 100);

  // LHS
  XX[1] := mW + sW - mX;
  XX[2] := mW - sW - mX;

  // RHS
  XX[3] := mW + sW + mX;
  XX[4] := mW - sW + mX;

  Y := 0;
  L := Low(Pts);
  R := High(Pts);
  for I := 1 to 6 do
  begin
    for J := 1 to 2 do
    begin
      // LHS
      Pts[L].X := XX[J];
      Pts[L].Y := Y;
      Inc(L);

      // RHS
      Pts[R].X := XX[J + 2];
      Pts[R].Y := Y;
      Dec(R);

      Inc(Y, sH);
    end;
  end;

  Rgn := CreatePolygonRgn(Pts, 24, WINDING);
  try
    with DestCanvas do
    begin
      if bReverse then
        BitBlt(Handle, 0, 0, W, H, Image.Canvas.Handle, 0, 0, SRCCOPY);

      SelectClipRgn(Handle, Rgn);
      if bReverse then
        BitBlt(Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY)
      else
        BitBlt(Handle, 0, 0, W, H, Image.Canvas.Handle, 0, 0, SRCCOPY);
      SelectClipRgn(Handle, 0);
    end;
  finally
    DeleteObject(Rgn);
  end;
end;  

// Zigzag Wipe from Vertical Center
procedure ZigzagWipeFromVerticalCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoVerticalZigzagEffect(DestCanvas, Screen, Image, ARect, Progress, False);
end;

// Zigzag Wipe To Vertical Center
procedure ZigzagWipeToVerticalCenter(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);  
begin
  DoVerticalZigzagEffect(DestCanvas, Screen, Image, ARect, Progress, True);   
end;


// Diamond shred
procedure ExpandingDiamondsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  S, D: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  S := (Min(W, H) div 10) + 1;
  D := MulDiv(S, Progress, 50);
  Rgn := 0;
  X := 0;
  while X < W do
  begin
    Inc(X, S);                                             
    Y := 0;
    while Y < H do
    begin
      Inc(Y, S);
      TmpRgn := CreatePolygonRgnEx([X - D, Y, X, Y - D, X + D, Y, X, Y + D]);
      if Rgn <> 0 then
      begin
        CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end
      else
        Rgn := TmpRgn;
      Inc(Y, S);
    end;
    Inc(X, S);
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Reveal diamond out from centre
procedure DiamondWipeFromCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW, mH: Integer;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  Rgn := CreatePolygonRgnEx([mW - X, mH, mW, mH - Y, mW + X, mH, mW, mH + Y]);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Reveal diamond in to centre
procedure DiamondWipeToCenterEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  mW, mH: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  Rgn := CreateRectRgn(0, 0, W, H);
  TmpRgn := CreatePolygonRgnEx([X - mW, mH, mW, Y - mH, 3 * mW - X, mH, mW, 3 * mH - Y]);
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_XOR);
  DeleteObject(TmpRgn);
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

    

procedure DoShapeWipeInAndOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer;
                                    AShape: TXCustomShape; rScale: single = 1);
var
  mW, mH, mX, mY: Integer;
  Rgn, TmpRgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  mW := W div 2;
  mH := H div 2;
  mX := X div 2;
  mY := Y div 2;

  if AShape = xcsIELetter then
  begin
    Rgn := CreateRegionFromLetter(Rect(-1 * Round(rScale * mX), -1 * Round(rScale * mY), 2 * mW + Round(rScale * mX), 2 * mH + Round(rScale * mY)), Progress, 0, 50);
    TmpRgn := CreateRegionFromLetter(Rect(mx, mY, 2 * mW - mX, 2 * mH - mY), Progress, 10, 80);
  end
  else
  if AShape = xcsIEDiamondShape then
  begin
    Rgn := CreatePolygonRgnEx([-mX, mH, mW, -mY, 2 * mW + mX, mH, mW, 2 * mH + mY]);
    TmpRgn := CreatePolygonRgnEx([mX, mH, mW, mY, 2 * mW - mX, mH, mW, 2 * mH - mY]);
  end
  else
  begin
    Rgn := CreateCustomShape(AShape, Rect(-1 * Round(rScale * mX), -1 * Round(rScale * mY), 2 * mW + Round(rScale * mX), 2 * mH + Round(rScale * mY)));
    TmpRgn := CreateCustomShape(AShape, Rect(mx, mY, 2 * mW - mX, 2 * mH - mY));
  end;
  CombineRgn(Rgn, Rgn, TmpRgn, RGN_XOR);
  DeleteObject(TmpRgn);
  
  try
    if (AShape = xcsIELetter) or (AShape = xcsDoubleHeart) then
      BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);

    OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  finally
    DeleteObject(Rgn);
  end;
end;


// Diamond Wipe In and Out
procedure DiagonalBoxWipeEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoShapeWipeInAndOutEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIEDiamondShape);
end;


// Heart Wipe In and Out
procedure HeartWipeInAndOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin                                                                        
  if IEGlobalSettings().TransitionsDrawAlternative then
    DoShapeWipeInAndOutEffect(DestCanvas, Screen, Image, ARect, Progress, xcsDoubleHeart, 1.6)
  else
    DoShapeWipeInAndOutEffect(DestCanvas, Screen, Image, ARect, Progress, xcsHeart, 1.6)
end;

// 5 Point Star Wipe In and Out
procedure Star5WipeInAndOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoShapeWipeInAndOutEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar5, 2.4);
end;

// 6 Point Star Wipe In and Out
procedure Star6WipeInAndOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoShapeWipeInAndOutEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar6, 1.6);
end;

// Word Wipe In and Out
procedure WordWipeInAndOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer); 
var
  rSize: Single;
begin
  rSize := 4.0;
  if IEGlobalSettings().TransitionsDrawAlternative then
    rSize := 8.0;

  DoShapeWipeInAndOutEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIELetter, rSize);
end;


var
  gPacmanLastUpdate : DWORD;
  gPacmanState : Byte;

procedure DoPacmanEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer;
                         iRowCount : Integer; bSimultaneous : Boolean; bReversed : Boolean = False);
const
  State_Duration = 100 {millisec};

  // Pacman states
  Mouth_Open    = 0;
  Mouth_Closing = 1;
  Mouth_Closed  = 2;
  Mouth_Opening = 3;
var
  W, H, X, Y: Integer;
  iSize: Integer;
  Rgn, TmpRgn: HRGN;
  I: Integer;
  iProgressDivs: Integer;
  bDraw: Boolean;

  function GetPacmanRegion(iHPos, iVOffSet : Integer; bReverse : Boolean) : HRgn;
  var
    iMouthPos: Integer; 
    Pts: array[1..3] of TPoint;   
    iX, iY, iPillSize, iSpacing: Integer;
  begin
    if bReversed then
      bReverse := not bReverse;

    iMouthPos := iHPos + iSize div 2;
    if bReverse then
    begin
      iHPos := W - iHPos;
      iMouthPos := iHPos - iSize div 2;
    end;

    // Trail
    if bReverse then
      Result := CreateRectRgn(iHPos, iVOffSet, W, iVOffSet + iSize)
    else
      Result := CreateRectRgn(0, iVOffSet, iHPos, iVOffSet + iSize);

    // Pac body
    TmpRgn := CreateEllipticRgn(iHPos - iSize div 2, iVOffSet, iHPos + iSize div 2, iVOffSet + iSize + 1);
    CombineRgn(Result, Result, TmpRgn, RGN_OR);
    DeleteObject(TmpRgn);

    // Pac Mouth
    Pts[1].X := iHPos;
    Pts[1].Y := iVOffSet + iSize div 2;

    if gPacmanState in [Mouth_Opening, Mouth_Closing] then
    begin
      // Mouth half open
      Pts[2].X := iMouthPos;
      Pts[2].Y := iVOffSet + MulDiv(iSize, 1, 4);
      Pts[3].X := iMouthPos;
      Pts[3].Y := iVOffSet + MulDiv(iSize, 3, 4);
    end
    else
    if gPacmanState = Mouth_Closed then
    begin
      // Mouth almost closed
      Pts[2].X := iMouthPos;
      Pts[2].Y := iVOffSet + MulDiv(iSize, 15, 32);
      Pts[3].X := iMouthPos;
      Pts[3].Y := iVOffSet + MulDiv(iSize, 17, 32);
    end
    else // Mouth_Open
    begin
      // Mouth fully open
      Pts[2].X := iMouthPos;
      Pts[2].Y := iVOffSet - MulDiv(iSize, 1, 4);
      Pts[3].X := iMouthPos;
      Pts[3].Y := iVOffSet + iSize + MulDiv(iSize, 1, 4);
    end;

    TmpRgn := CreatePolygonRgn(Pts, 3, WINDING);
    CombineRgn(Result, Result, TmpRgn, RGN_DIFF);
    DeleteObject(TmpRgn);

    // Previous paths
    if (bSimultaneous = False) and (iVOffSet > 0) then
    begin
      TmpRgn := CreateRectRgn(0, 0, W, iVOffSet);
      CombineRgn(Result, Result, TmpRgn, RGN_OR);
      DeleteObject(TmpRgn);
    end;

    // Draw pills
    if IEGlobalSettings().TransitionsDrawAlternative = False then
    begin
      iY := iVOffSet + iSize div 2;
      iPillSize := iSize div 12;
      iSpacing  := imax(W div 10, iPillSize * 3);
      iX := iSpacing div 2;
      while iX < W do
      begin
        if bReverse then
          bDraw := iX < iMouthPos
        else
          bDraw := iX > iMouthPos;
        if bDraw then
        begin
          TmpRgn := CreateEllipticRgn(iX - iPillSize, iY- iPillSize, iX + iPillSize, iY + iPillsize);
          CombineRgn(Result, Result, TmpRgn, RGN_OR);
          DeleteObject(TmpRgn);
        end;
        inc(iX, iSpacing);
      end;
    end;
  end;

begin
  CalcParams(ARect, Progress, W, H, X, Y);
  iSize := H div iRowCount + 1;

  if fTransitionInitialized = False then
  begin
    gPacmanLastUpdate := GetTickCount;
    gPacmanState := Mouth_Open;
    fTransitionInitialized := True;
  end
  else
  if GetTickCount > gPacmanLastUpdate + State_Duration then
  begin
    inc(gPacmanState);
    if gPacmanState > Mouth_Opening then
      gPacmanState := Mouth_Open;
    gPacmanLastUpdate := GetTickCount;
  end;

  Rgn := 0;
  iProgressDivs := 100 div iRowCount;
  for I := 0 to iRowCount - 1 do
  begin
    if bSimultaneous then
    begin
      TmpRgn := GetPacmanRegion(MulDiv(W, Progress, 100), iSize * I, I Mod 2 = 1);
      if Rgn = 0 then
        Rgn := TmpRgn
      else
      begin
        CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
        DeleteObject(TmpRgn);
      end;
    end
    else
    begin
      if I = iRowCount - 1 then
        bDraw := Progress >= iProgressDivs * (iRowCount - 1)
      else
        bDraw := Progress < (I + 1) * iProgressDivs;

      if bDraw then
      begin
        Rgn := GetPacmanRegion(MulDiv(W, Progress - I * iProgressDivs, iProgressDivs), iSize * I, I Mod 2 = 1);
        Break;
      end;
    end;
  end;
  
  if Rgn <> 0 then
  try
    BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);
    OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  finally
    DeleteObject(Rgn);
  end;
end;


// Pacman Devours from Left
procedure PacmanFromLeftEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoPacmanEffect(DestCanvas, Screen, Image, ARect, Progress, 1, False);
end;

// Pacman Devours from Right
procedure PacmanFromRightEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoPacmanEffect(DestCanvas, Screen, Image, ARect, Progress, 1, False, True);
end;

// Pacman Devours Three Rows
procedure Pacman3RowEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoPacmanEffect(DestCanvas, Screen, Image, ARect, Progress, 3, False);
end;

// Pacman Devours Four Rows
procedure Pacman4RowEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoPacmanEffect(DestCanvas, Screen, Image, ARect, Progress, 4, False);
end;

// Two Simultaneous Rows of Pacman
procedure Pacman2SimRowEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoPacmanEffect(DestCanvas, Screen, Image, ARect, Progress, 2, True);
end;

// Four Simultaneous Rows of Pacman
procedure Pacman4SimRowEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoPacmanEffect(DestCanvas, Screen, Image, ARect, Progress, 4, True);
end;

// Six Simultaneous Rows of Pacman
procedure Pacman6SimRowEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoPacmanEffect(DestCanvas, Screen, Image, ARect, Progress, 6, True);
end;

// Random Puzzle Pieces
procedure RandomPuzzlePiecesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
const
  Box_Value = 8;   // = 5 boxes
var
  S, SavedRandSeed: Integer;
  Rgn, TmpRgn, JRgn: HRGN;
  W, H, X, Y: Integer;
  iJSize, iJOffset: Integer;
  bTopJoiner: Boolean;
  iCol, iRow: Integer;

  procedure _AddJoiner(p1, p2, p3, p4, CombineMode : Integer);
  begin
    JRgn := CreateEllipticRgn(p1, p2, p3, p4);
    CombineRgn(TmpRgn, TmpRgn, JRgn, CombineMode);
    DeleteObject(JRgn);
  end;

begin
  CalcParams(ARect, Progress, W, H, X, Y);
  SavedRandSeed := RandSeed;
  RandSeed := Integer(Image.Handle);
  S := (Min(W, H) div Box_Value) + 1;
  if IEGlobalSettings().TransitionsDrawAlternative then
    S := (Min(W, H) div Box_Value div 2) + 1;

  iJSize := S div 3;
  iJOffset := iJSize div 5;
  Rgn := 0;
  try
    iCol := 0;
    X := 0 - 2 * iJSize;
    while X < W do
    begin
      Inc(X, S);
      Y := 0 - 2 * iJSize;
      iRow := 0;
      while Y < H do
      begin
        Inc(Y, S);
        if Random(100) < Progress then
        begin
          TmpRgn := CreateRectRgn(X - S, Y - S, X + S, Y + S);

          bTopJoiner := iCol mod 2 = 0;
          if iRow mod 2 = 0 then
            bTopJoiner := not bTopJoiner;

          if bTopJoiner then
          begin
            // Top Joiner
            _AddJoiner(X - iJSize, Y - S - iJOffset, X + iJSize, Y - S - iJOffset + 2 * iJSize, RGN_DIFF);    // Poke In

            // Left Joiner
            _AddJoiner(X - S - 2 * iJSize + iJOffset, Y - iJSize, X - S + iJOffset, Y + iJSize, RGN_OR);      // Poke Out

            // Bottom Joiner
            _AddJoiner(X - iJSize, Y + S - 2 * iJSize + iJOffset, X + iJSize, Y + S + iJOffset, RGN_DIFF);    // Poke In

            // Right Joiner
            _AddJoiner(X + S - iJOffset, Y - iJSize, X + S - iJOffset + 2 * iJSize, Y + iJSize, RGN_OR);      // Poke Out
          end
          else  
          begin
            // Top Joiner
            _AddJoiner(X - iJSize, Y - S - 2 * iJSize + iJOffset, X + iJSize, Y - S + iJOffset, RGN_OR);      // Poke Out

            // Left Joiner
            _AddJoiner(X - S - iJOffset, Y - iJSize, X - S - iJOffset + 2 * iJSize, Y + iJSize, RGN_DIFF);    // Poke In

            // Bottom Joiner
            _AddJoiner(X - iJSize, Y + S - iJOffset, X + iJSize, Y + S - iJOffset + 2 * iJSize, RGN_OR);      // Poke Out

            // Right Joiner
            _AddJoiner(X + S - 2 * iJSize + iJOffset, Y - iJSize, X + S + iJOffset, Y + iJSize, RGN_DIFF);    // Poke In
          end;
                                                    
          if Rgn = 0 then
            Rgn := TmpRgn
          else
          begin
            CombineRgn(Rgn, Rgn, TmpRgn, RGN_XOR);
            DeleteObject(TmpRgn)
          end;
        end;
        Inc(Y, S);
        Inc(iRow);
      end;
      Inc(X, S);
      inc(iCol);
    end;
  finally
    RandSeed := SavedRandSeed;
  end;

  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;

// Explosion Wipe In and Out
procedure ExplosionWipeInAndOutEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  AShape: TXCustomShape;
begin
  if IEGlobalSettings().TransitionsDrawAlternative then
    AShape := xcsExplosion_2
  else
    AShape := xcsExplosion;
  DoShapeWipeInAndOutEffect(DestCanvas, Screen, Image, ARect, Progress, AShape, 1.1);
end;


// Random Boxes with Word
procedure RandomBoxesWithWordEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
const
  Box_Value = 400;   // = 201 boxes
  Size_As_Percent_Of_Dim = 90; // Text is 90% of width/height
  Intersection_Point_in_Progress = 60;  // when change from showing word to dissolving word
var
  S, SavedRandSeed: Integer;
  ComplexRegion: TComplexRegion;
  Rgn, TmpRgn: HRGN;
var
  W, H, X, Y: Integer;
  bAdd: Boolean;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  SavedRandSeed := RandSeed;
  RandSeed := Integer(Image.Handle);
  S := (Min(W, H) div Box_Value) + 1;
  if IEGlobalSettings().TransitionsDrawAlternative then
    S := (Min(W, H) div Box_Value div 2) + 1;

  ComplexRegion := TComplexRegion.Create;
  try
    X := 0;
    while X < W do
    begin
      Inc(X, S);
      Y := 0;
      while Y < H do
      begin
        Inc(Y, S);
        if Progress > Intersection_Point_in_Progress then
          bAdd := Random(100) < MulDiv(Progress - Intersection_Point_in_Progress, 100, 100 - Intersection_Point_in_Progress)
        else
          bAdd := Random(100) < MulDiv(Progress, 100, Intersection_Point_in_Progress);
        if bAdd then
          ComplexRegion.AddRect(X - S, Y - S, X + S, Y + S);
        Inc(Y, S);
      end;
      Inc(X, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
    RandSeed := SavedRandSeed;
  end;

  TmpRgn := CreateRegionFromLetter(Rect(MulDiv(W, 50 - Size_As_Percent_Of_Dim div 2, 100),
                                        MulDiv(H, 50 - Size_As_Percent_Of_Dim div 2, 100),
                                        MulDiv(W, 50 + Size_As_Percent_Of_Dim div 2, 100),
                                        MulDiv(H, 50 + Size_As_Percent_Of_Dim div 2, 100)),
                                   Progress, 0, 100, True);
  if Rgn = 0 then
    Rgn := TmpRgn
  else
  begin
    if Progress > Intersection_Point_in_Progress then
      CombineRgn(Rgn, Rgn, TmpRgn, RGN_AND)
    else
      CombineRgn(Rgn, Rgn, TmpRgn, RGN_DIFF);
    DeleteObject(TmpRgn)
  end;
  
  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


// Large boxes at random positions
procedure RandomBigBoxesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
const
  Box_Value = 20;  // = 11 boxes
var
  S, SavedRandSeed: Integer;
  ComplexRegion: TComplexRegion;
  Rgn: HRGN;
  W, H, X, Y: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  SavedRandSeed := RandSeed;
  RandSeed := Integer(Image.Handle);
  S := (Min(W, H) div Box_Value) + 1; 
  if IEGlobalSettings().TransitionsDrawAlternative then
    S := (Min(W, H) div Box_Value div 2) + 1;

  ComplexRegion := TComplexRegion.Create;
  try
    X := 0;
    while X < W do
    begin
      Inc(X, S);
      Y := 0;
      while Y < H do
      begin
        Inc(Y, S);
        if Random(100) < Progress then
          ComplexRegion.AddRect(X - S, Y - S, X + S, Y + S);
        Inc(Y, S);
      end;
      Inc(X, S);
    end;
    Rgn := ComplexRegion.CreateRegion;
  finally
    ComplexRegion.Free;
    RandSeed := SavedRandSeed;
  end;

  OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  DeleteObject(Rgn);
end;


procedure DoRandomShapesEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer;
                               AShape: TXCustomShape; iCount: integer; rCoverage: Single);
var
  iSize, SavedRandSeed: Integer;
  Rgn: HRGN;
  W, H, X, Y: Integer;
  TmpRgn: HRGN;
  iMovementIncrement: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);
  SavedRandSeed := RandSeed;
  RandSeed := Integer(Image.Handle);
  iSize := (Min(W, H) div iCount) + 1;
  iMovementIncrement := Trunc(iSize * rCoverage);
  Rgn := 0;
  try
    X := 0;
    while X < W do
    begin
      Inc(X, iMovementIncrement);
      Y := 0;
      while Y < H do
      begin
        Inc(Y, iMovementIncrement);

        if Random(100) < Progress then
        begin
          if AShape = xcsIELetter then
            TmpRgn := CreateRegionFromLetter(Rect(X - iSize, Y - iSize, X + iSize, Y + iSize), Progress, 0, 50, False, False)
          else
            TmpRgn := CreateCustomShape(AShape, Rect(X - iSize, Y - iSize, X + iSize, Y + iSize));
          if Rgn <> 0 then
          begin
            CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR);
            DeleteObject(TmpRgn);
          end
          else
            Rgn := TmpRgn;
        end;

        Inc(Y, iMovementIncrement);
      end;
      Inc(X, iMovementIncrement);
    end;
  finally
    RandSeed := SavedRandSeed;
  end;

  if Rgn <> 0 then
  try        
    if (AShape = xcsIELetter) and (Length(IEGlobalSettings().WordTransitionParams.Word) > 1) then
      BitBlt(DestCanvas.Handle, 0, 0, W, H, Screen.Canvas.Handle, 0, 0, SRCCOPY);

    OutputRgnToCanvas(DestCanvas, W, H, Rgn, Image);
  finally
    DeleteObject(Rgn);
  end;
end;

// Hearts at random positions
procedure RandomHeartsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin                             
  if IEGlobalSettings().TransitionsDrawAlternative then
    DoRandomShapesEffect(DestCanvas, Screen, Image, ARect, Progress, xcsDoubleHeart, 12, 0.2)
  else
    DoRandomShapesEffect(DestCanvas, Screen, Image, ARect, Progress, xcsHeart, 12, 0.2);
end;

// 5 Point Stars at random positions
procedure RandomStar5sEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoRandomShapesEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar5, 14, 0.4);
end;

// 6 Point Stars at random positions
procedure RandomStar6sEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
begin
  DoRandomShapesEffect(DestCanvas, Screen, Image, ARect, Progress, xcsStar6, 14, 0.4);
end;

// Words of Letter at random positions
procedure RandomWordEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  rSize: Single;
begin
  rSize := 0.4;
  if IEGlobalSettings().TransitionsDrawAlternative then
    rSize := 0.2;                          
  DoRandomShapesEffect(DestCanvas, Screen, Image, ARect, Progress, xcsIELetter, 14, rSize);
end;

// Explosions at random positions
procedure RandomExplosionsEffect(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  AShape: TXCustomShape;
begin
  if Progress mod 2 = 0 then
    AShape := xcsExplosion
  else
    AShape := xcsExplosion_2;
  DoRandomShapesEffect(DestCanvas, Screen, Image, ARect, Progress, AShape, 18, 0.5);
end;
             
// Twist In
procedure AngledTwistIn(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
  Pts: array [0..2] of TPoint;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  {
  PlgBlt requires a pointer to an array of three points that identify three corners of the destination parallelogram
  Pts[0]  : upper-left corner of the source rectangle
  Pts[1]  : upper-right corner
  Pts[2]  : lower-left corner
  Implied : lower-right corner of the source rectangle is mapped to the implicit fourth point in the parallelogram.
  }

  Pts[0] := Point((w - X) div 2, (h - Y) div 2);
  Pts[1] := Point(w, 0);
  Pts[2] := Point(0, H);

  PlgBlt(DestCanvas.Handle, Pts, Image.Canvas.Handle, 0, 0, W, H, 0, 0, 0);
end;

// Twist Out
procedure AngledTwistOut(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer);
var
  W, H, X, Y: Integer;
  Pts: array [0..2] of TPoint;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  {
  PlgBlt requires a pointer to an array of three points that identify three corners of the destination parallelogram
  Pts[0]  : upper-left corner of the source rectangle
  Pts[1]  : upper-right corner
  Pts[2]  : lower-left corner
  Implied : lower-right corner of the source rectangle is mapped to the implicit fourth point in the parallelogram.
  }
         
  BitBlt(DestCanvas.Handle, 0, 0, W, H,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);

  Pts[0] := Point(X div 2, Y div 2);
  Pts[1] := Point(w, 0);
  Pts[2] := Point(0, H);

  PlgBlt(DestCanvas.Handle, Pts, Screen.Canvas.Handle, 0, 0, W, H, 0, 0, 0);
end;


// Multiple Twist In
procedure MultipleAngledTwistIn(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer; Step : Integer);
const
  Horz_Count = 6;
  Vert_Count = 4;
var
  W, H, X, Y: Integer;
  Pts: array [0..2] of TPoint;
  iBoxWidth, iBoxHeight: Integer;
  iBoxSkewPosX, iBoxSkewPosY: Integer;
  iBoxX, iBoxY: Integer;
  iX, iY: Integer;                         
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  iBoxWidth    := Round(W / Horz_Count);
  iBoxHeight   := Round(H / Vert_Count);
  iBoxSkewPosX := (iBoxWidth - MulDiv(iBoxWidth, Step, 1024)) div 2;
  iBoxSkewPosY := (iBoxHeight - MulDiv(iBoxHeight, Step, 1024))  div 2;

  for iY := 0 to Vert_Count - 1 do
    for iX := 0 to Horz_Count - 1 do
    begin           
      {
      PlgBlt requires a pointer to an array of three points that identify three corners of the destination parallelogram
      Pts[0]  : upper-left corner of the source rectangle
      Pts[1]  : upper-right corner
      Pts[2]  : lower-left corner
      Implied : lower-right corner of the source rectangle is mapped to the implicit fourth point in the parallelogram.
      }
      iBoxX := iX * iBoxWidth;
      iBoxY := iY * iBoxHeight;
      Pts[0] := Point(iBoxX + iBoxSkewPosX, iBoxY + iBoxSkewPosY);
      Pts[1] := Point(iBoxX + iBoxWidth, iBoxY - 1);
      Pts[2] := Point(iBoxX - 1, iBoxY + iBoxHeight);
      PlgBlt(DestCanvas.Handle, Pts, Image.Canvas.Handle, iBoxX, iBoxY, iBoxWidth, iBoxHeight, 0, 0, 0);
    end;
end;

// Multiple Twist Out
procedure MultipleAngledTwistOut(DestCanvas: TCanvas; Screen, Image: TBitmap; const ARect: TRect; Progress: Integer; Step : Integer);
const
  Horz_Count = 6;
  Vert_Count = 4;
var
  W, H, X, Y: Integer;
  Pts: array [0..2] of TPoint;
  iBoxWidth, iBoxHeight: Integer;
  iBoxSkewPosX, iBoxSkewPosY: Integer;
  iBoxX, iBoxY: Integer;
  iX, iY: Integer;
begin
  CalcParams(ARect, Progress, W, H, X, Y);

  iBoxWidth    := Round(W / Horz_Count);
  iBoxHeight   := Round(H / Vert_Count);
  iBoxSkewPosX := MulDiv(iBoxWidth, Step, 1024) div 2;
  iBoxSkewPosY := MulDiv(iBoxHeight, Step, 1024)  div 2;

  BitBlt(DestCanvas.Handle, 0, 0, W, H,
         Image.Canvas.Handle, 0, 0,
         SRCCOPY);

  for iY := 0 to Vert_Count - 1 do
    for iX := 0 to Horz_Count - 1 do
    begin
      {
      PlgBlt requires a pointer to an array of three points that identify three corners of the destination parallelogram
      Pts[0]  : upper-left corner of the source rectangle
      Pts[1]  : upper-right corner
      Pts[2]  : lower-left corner
      Implied : lower-right corner of the source rectangle is mapped to the implicit fourth point in the parallelogram.
      }
      iBoxX := iX * iBoxWidth;
      iBoxY := iY * iBoxHeight;
      Pts[0] := Point(iBoxX + iBoxSkewPosX, iBoxY + iBoxSkewPosY);
      Pts[1] := Point(iBoxX + iBoxWidth, iBoxY {- 1});
      Pts[2] := Point(iBoxX {- 1}, iBoxY + iBoxHeight);
      PlgBlt(DestCanvas.Handle, Pts, Screen.Canvas.Handle, iBoxX, iBoxY, iBoxWidth, iBoxHeight, 0, 0, 0);
    end;
end;


// Perform one of the "Extra" transitionEffects
procedure PerformExtraTransitionEffect(Effect : TIETransitionType; ACanvas: TCanvas; ASourceBitmap, ATargetBitmap: TBitmap; const ARect: TRect; iStep: Integer);
var
  iProgress: Integer;
begin
  iProgress := Round(iStep / 1024 * 100);

  case Effect of
    iettExpandFromLeft                          : ExpandFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandFromRight                         : ExpandFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandFromTop                           : ExpandFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandFromBottom                        : ExpandFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandFromTopLeft                       : ExpandFromTopLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandFromTopRight                      : ExpandFromTopRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandFromBottomLeft                    : ExpandFromBottomLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandFromBottomRight                   : ExpandFromBottomRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandInFromLeft                        : ExpandInFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandInFromRight                       : ExpandInFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandInFromTop                         : ExpandInFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandInFromBottom                      : ExpandInFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandInToVerticalCenter                : ExpandInToVerticalCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandInToHorizon                       : ExpandInToHorizonEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandInFromSides                       : ExpandInFromSidesEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandInFromTopAndBottom                : ExpandInFromTopAndBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandOutFromHorizon                    : ExpandOutFromHorizonEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandOutFromVerticalCenter             : ExpandOutFromVerticalCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettWipeFromTopLeft                         : WipeFromTopLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeFromTopRight                        : WipeFromTopRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeFromBottomLeft                      : WipeFromBottomLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeFromBottomRight                     : WipeFromBottomRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeInFromTopAndBottom                  : WipeInFromTopAndBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeFromHorizon                         : WipeFromHorizonEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeInFromSides                         : WipeInFromSidesEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeOutFromVerticalCenter               : WipeOutFromVerticalCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettBuildUpFromLeft                         : BuildUpFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBuildUpFromRight                        : BuildUpFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBuildUpFromTop                          : BuildUpFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBuildUpFromBottom                       : BuildUpFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettUnrollFromLeft                          : UnrollFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettUnrollFromRight                         : UnrollFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettUnrollFromTop                           : UnrollFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettUnrollFromBottom                        : UnrollFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettSlideInFromLeft                         : SlideInFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettSlideInFromRight                        : SlideInFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettSlideInFromTop                          : SlideInFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettSlideInFromBottom                       : SlideInFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettShrinkToTopLeft                         : ShrinkToTopLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShrinkToTopRight                        : ShrinkToTopRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShrinkToBottomLeft                      : ShrinkToBottomLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShrinkToBottomRight                     : ShrinkToBottomRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShrinkToCenter                          : ShrinkToCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettQuartersWipeInToCenter                  : QuartersWipeInToCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettQuartersExpandToCenter                  : QuartersExpandToCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettQuartersSlideInToCenter                 : QuartersSlideInToCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettCurvedWipeFromLeft                      : CurvedWipeFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCurvedWipeFromRight                     : CurvedWipeFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCurvedWipeFromTop                       : CurvedWipeFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCurvedWipeFromBottom                    : CurvedWipeFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCurvedWipeFromTopLeft                   : CurvedWipeFromTopLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCurvedWipeFromTopRight                  : CurvedWipeFromTopRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCurvedWipeFromBottomLeft                : CurvedWipeFromBottomLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCurvedWipeFromBottomRight               : CurvedWipeFromBottomRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettBarsInFromLeft                          : BarsInFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsInFromRight                         : BarsInFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsFromTop                             : BarsFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsFromBottom                          : BarsFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsLeftThenRight                       : BarsLeftThenRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsRightThenLeft                       : BarsRightThenLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsTopThenBottom                       : BarsTopThenBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsBottomThenTop                       : BarsBottomThenTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsFromBothSides                       : BarsFromBothSidesEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettBarsFromTopAndBottom                    : BarsFromTopAndBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettShreddedFromLeft                        : ShreddedFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromRight                       : ShreddedFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromTop                         : ShreddedFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromBottom                      : ShreddedFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromTopAndLeft                  : ShreddedFromTopAndLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromTopAndRight                 : ShreddedFromTopAndRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromBottomAndLeft               : ShreddedFromBottomAndLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromBottomAndRight              : ShreddedFromBottomAndRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromHorizonAndLeft              : ShreddedFromHorizonAndLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromHorizonAndRight             : ShreddedFromHorizonAndRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromTopAndVerticalCenter        : ShreddedFromTopAndVerticalCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromBottomAndVerticalCenter     : ShreddedFromBottomAndVerticalCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedFromCenter                      : ShreddedFromCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedToCenter                        : ShreddedToCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedInToHorizon                     : ShreddedInToHorizonEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedInToVerticalCenter              : ShreddedInToVerticalCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedOutFromHorizon                  : ShreddedOutFromHorizonEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettShreddedOutFromVerticalCenter           : ShreddedOutFromVerticalCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettExpandingRectangles                     : ExpandingRectanglesEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingTriangles                      : ExpandingTrianglesEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingCircles                        : ExpandingCirclesEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingDiamonds                       : ExpandingDiamondsEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettCircularWipeFromCenter                  : CircularWipeFromCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCircularWipeToCenter                    : CircularWipeToCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettCrisscrossWipeFromTopLeft               : CrisscrossWipeFromTopLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromTopRight              : CrisscrossWipeFromTopRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromBottomLeft            : CrisscrossWipeFromBottomLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromBottomRight           : CrisscrossWipeFromBottomRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeBounceFromTopLeft         : CrisscrossWipeBounceFromTopLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeBounceFromTopRight        : CrisscrossWipeBounceFromTopRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeBounceFromBottomLeft      : CrisscrossWipeBounceFromBottomLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeBounceFromBottomRight     : CrisscrossWipeBounceFromBottomRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromLeftRightAndTop       : CrisscrossWipeFromLeftRightAndTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromLeftRightAndBottom    : CrisscrossWipeFromLeftRightAndBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromLeftTopAndBottom      : CrisscrossWipeFromLeftTopAndBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromTopLeftRightAndBottom : CrisscrossWipeFromTopLeftRightAndBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromRightTopAndBottom     : CrisscrossWipeFromRightTopAndBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrisscrossWipeFromBottomLeftTopRight    : CrisscrossWipeFromBottomLeftTopRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettWipeDiagonalFromTopLeft                 : WipeDiagonalFromTopLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeDiagonalFromTopRight                : WipeDiagonalFromTopRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeDiagonalFromBottomLeft              : WipeDiagonalFromBottomLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWipeDiagonalFromBottomRight             : WipeDiagonalFromBottomRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettDiagonalSweepClockwise                  : DiagonalSweepClockwiseEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettDiagonalSweepCounterClockwise           : DiagonalSweepCounterClockwiseEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettSweepClockwise                          : HalfSweepClockwise(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettSweepCounterClockwise                   : HalfSweepAntiClockwise(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettStarburstClockwiseFromCenter            : StarburstClockwiseFromCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettStarburstCounterClockwiseFromCenter     : StarburstCounterClockwiseFromCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettRotationalRectangle                     : RotationalRectangleEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress, True);
    iettRotationalRectangleCounterClockwise     : RotationalRectangleEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress, False);
    iettRotationalStar                          : RotationalStarEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress, True);
    iettRotationalStarCounterClockwise          : RotationalStarEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress, False);

    iettSpeckledWipeFromLeft                    : SpeckledWipeFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettSpeckledWipeFromRight                   : SpeckledWipeFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettSpeckledWipeFromTop                     : SpeckledWipeFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettSpeckledWipeFromBottom                  : SpeckledWipeFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettPushLeftAndSlideOut                     : PushLeftAndSlideOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPushRightAndSlideOut                    : PushRightAndSlideOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPushUpAndSlideOut                       : PushUpAndSlideOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPushDownAndSlideOut                     : PushDownAndSlideOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettPushAndSqueezeLeft                      : PushAndSqueezeLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPushAndSqueezeRight                     : PushAndSqueezeRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPushAndSqueezeUp                        : PushAndSqueezeUpEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPushAndSqueezeDown                      : PushAndSqueezeDownEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettHorizontalBlinds                        : HorizontalBlindsEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettVerticalBlinds                          : VerticalBlindsEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettUnevenBlindsFromLeft                    : UnevenBlindsFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettUnevenBlindsFromRight                   : UnevenBlindsFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettUnevenBlindsFromTop                     : UnevenBlindsFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettUnevenBlindsFromBottom                  : UnevenBlindsFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettRectanglesFromTheLeft                   : WideBarsFromLeft(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettRectanglesFromTheRight                  : WideBarsFromRight(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettRectanglesFromTheTop                    : WideBarsFromTop(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettRectanglesFromTheBottom                 : WideBarsFromBottom(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettSpirallingRectangleClockwise            : SpirallingRectangleEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress, True);
    iettSpirallingRectangleCounterClockwise     : SpirallingRectangleEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress, False);

    iettArrowWipeFromLeft                       : ArrowWipeFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettArrowWipeFromRight                      : ArrowWipeFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettArrowWipeFromTop                        : ArrowWipeFromTopEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettArrowWipeFromBottom                     : ArrowWipeFromBottomEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettHorizontalBowTieWipe                    : HorizontalBowTieWipeEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettVerticalBowTieWipe                      : VerticalBowTieWipeEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettDiagonalCrossFromCenter                 : DiagonalCrossFromCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettDiagonalCrossToCenter                   : DiagonalCrossToCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettZigzagWipeFromHorizon                   : ZigzagWipeFromHorizonEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettZigzagWipeFromVerticalCenter            : ZigzagWipeFromVerticalCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettDiamondWipeFromCenter                   : DiamondWipeFromCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettDiamondWipeToCenter                     : DiamondWipeToCenterEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettDiagonalBoxWipe                         : DiagonalBoxWipeEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettTriangularWipe                          : RotationalTriangularWipeEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettRandomBigBoxes                          : RandomBigBoxesEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettZigzagWipeToHorizon                     : ZigzagWipeToHorizonEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettZigzagWipeToVerticalCenter              : ZigzagWipeToVerticalCenter (ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettRandomHearts                            : RandomHeartsEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettRandomStar5s                            : RandomStar5sEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettRandomStar6s                            : RandomStar6sEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettRandomExplosions                        : RandomExplosionsEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingHearts                         : ExpandingHeartsEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingStar5                          : ExpandingStar5sEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingStar6                          : ExpandingStar6sEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingExplosions                     : ExpandingExplosionsEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingLightningBolts                 : ExpandingLightningBoltsEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettHeartWipeOut                            : HeartWipeOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettHeartWipeIn                             : HeartWipeInEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettStar5WipeOut                            : Star5WipeOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettStar5WipeIn                             : Star5WipeInEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettStar6WipeOut                            : Star6WipeOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettStar6WipeIn                             : Star6WipeInEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExplosionWipeOut                        : ExplosionWipeOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExplosionWipeIn                         : ExplosionWipeInEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrossWipeOut                            : CrossWipeOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettCrossWipeIn                             : CrossWipeInEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettHeartWipeInAndOut                       : HeartWipeInAndOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettStar5WipeInAndOut                       : Star5WipeInAndOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettStar6WipeInAndOut                       : Star6WipeInAndOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExplosionWipeInAndOut                   : ExplosionWipeInAndOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettAngledTwistIn                           : AngledTwistIn(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettAngledTwistOut                          : AngledTwistOut(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettMultipleAngledTwistIn                   : MultipleAngledTwistIn(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress, iStep);
    iettMultipleAngledTwistOut                  : MultipleAngledTwistOut(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress, iStep);
    iettRandomPuzzlePieces                      : RandomPuzzlePiecesEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettPacmanFromLeft                          : PacmanFromLeftEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPacmanFromRight                         : PacmanFromRightEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPacman3Row                              : Pacman3RowEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPacman4Row                              : Pacman4RowEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPacman2SimRow                           : Pacman2SimRowEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPacman4SimRow                           : Pacman4SimRowEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettPacman6SimRow                           : Pacman6SimRowEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettFullSweepClockwise                      : FullSweepClockwise(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingSweepClockwise                 : FullExpandingSweepClockwise(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    iettRandomBoxesWithWord                     : RandomBoxesWithWordEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettRandomWord                              : RandomWordEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettExpandingWord                           : ExpandingWordEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWordWipeOut                             : WordWipeOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWordWipeIn                              : WordWipeInEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWordWipeInAndOut                        : WordWipeInAndOutEffect(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWordHalfSweep                           : WordHalfSweep(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWordFullSweep                           : WordFullSweep(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);
    iettWordFullExpandingSweep                  : WordFullExpandingSweep(ACanvas, ASourceBitmap, ATargetBitmap, ARect, iProgress);

    else raise Exception.create('Unknown Transition!');

  end;
end;

{$endif}


/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
// TIETransitionEffects

//

constructor TIETransitionEffects.Create(Owner: TIEView);
begin
  inherited Create;
  //
  fOwner := Owner;
  fSourceShot := TBitmap.Create;
  fSourceShot.Width := 1;
  fSourceShot.Height := 1;
  fSourceShot.PixelFormat := pf24bit;
  fTargetShot := TBitmap.Create;
  fTargetShot.Width := 1;
  fTargetShot.Height := 1;
  fTargetShot.PixelFormat := pf24bit;
  fCurrentView := TBitmap.Create;
  fCurrentView.Width := 1;
  fCurrentView.Height := 1;
  fCurrentView.PixelFormat := pf24bit;
  fRunning := false;
  fTransition := iettNone;
  fDuration := 1000;
  fSourceShotLines := nil;
  fTargetShotLines := nil;
  fCurrentViewLines := nil;
  fUpdateOnStop := false;
  fTimer := TTimer.Create(fOwner);
  fTimer.Enabled := false;
  fTimer.OnTimer := TimerEvent;
  fTimer.Interval := 1;
  fFullImage := nil;
  fOnTransitionStop := nil;
  fOnTransitionStep := nil;
  fTiming := iettLinear;
end;

destructor TIETransitionEffects.Destroy;
begin
  if fSourceShotLines <> nil then
    freemem(fSourceShotLines);
  if fTargetShotLines <> nil then
    freemem(fTargetShotLines);
  if fCurrentViewLines <> nil then
    freemem(fCurrentViewLines);
  fSourceShotLines := nil;
  fTargetShotLines := nil;
  fCurrentViewLines := nil;
  FreeAndNil(fTimer);
  FreeAndNil(fSourceShot);
  FreeAndNil(fTargetShot);
  FreeAndNil(fCurrentView);
  //
  inherited;
end;

procedure TIETransitionEffects.SetOnTransitionPaint(const Value: TIEOnTransitionPaint);
begin
  fOnTransitionPaint := Value;
end;

procedure TIETransitionEffects.SetSizes(Width, Height: integer);
begin
  Stop;
  fWidth := Width;
  fHeight := Height;
  fCurrentView.Width := fWidth;
  fCurrentView.Height := fHeight;
  fSourceShot.Width := fWidth;
  fSourceShot.Height := fHeight;
  fTargetShot.Width := fWidth;
  fTargetShot.Height := fHeight;
  FMinLeft  := 0;
  FMaxRight := 0;
end;

procedure TIETransitionEffects.Run(UpdateOnStop: boolean);
var
  q: integer;
begin
  if fTransition = iettNone then
    exit;
  if fRunning then
    fOwner.NPUnLockPaint;
  fUpdateOnStop := UpdateOnStop;
  fRunning := true;
  fStep := 0;
  FFirstStep := True;
  fAccum1 := 0;
  fFrames := 0;
  if ((fWidth < 2) and (fHeight < 2)) or (fHeight<>fSourceShot.Height) then
  begin
    fRunning := false;
    exit;
  end;
  fOwner.LockPaint;
  if fSourceShotLines <> nil then
    freemem(fSourceShotLines);
  if fTargetShotLines <> nil then
    freemem(fTargetShotLines);
  if fCurrentViewLines <> nil then
    freemem(fCurrentViewLines);
  getmem(fSourceShotLines, sizeof(pointer) * fHeight);
  getmem(fTargetShotLines, sizeof(pointer) * fHeight);
  getmem(fCurrentViewLines, sizeof(pointer) * fHeight);
  for q := 0 to fHeight - 1 do
  begin
    fSourceShotLines[q] := fSourceShot.Scanline[q];
    fTargetShotLines[q] := fTargetShot.Scanline[q];
    fCurrentViewLines[q] := fCurrentView.Scanline[q];
  end;
  fStartTick := GetTickCount;
  fTimer.Enabled := true;
end;

procedure TIETransitionEffects.PrepareBitmap(OriginalBitmap, TargetBitmap : TBitmap);
var
  q: Integer;
begin
  if fTransition=iettNone then
     exit;
  fRunning := true;
  fStep := 0;
  FFirstStep := True;
  fAccum1 := 0;
  fFrames := 0;

  SetSizes(OriginalBitmap.Width, OriginalBitmap.Height);

  fSourceShot.assign(OriginalBitmap);
  fCurrentView.assign(OriginalBitmap);
  fTargetShot.assign(TargetBitmap);

  if ((fWidth < 2) and (fHeight < 2)) or (fHeight <> fSourceShot.Height) then
  begin
    fRunning := false;
    exit;
  end;

  if fSourceShotLines <> nil then
    freemem(fSourceShotLines);
  if fTargetShotLines <> nil then
    freemem(fTargetShotLines);
  if fCurrentViewLines <> nil then
    freemem(fCurrentViewLines);
  getmem(fSourceShotLines, sizeof(pointer) * fHeight);
  getmem(fTargetShotLines, sizeof(pointer) * fHeight);
  getmem(fCurrentViewLines, sizeof(pointer) * fHeight);
  for q := 0 to fHeight - 1 do
  begin
    fSourceShotLines[q] := fSourceShot.Scanline[q];
    fTargetShotLines[q] := fTargetShot.Scanline[q];
    fCurrentViewLines[q] := fCurrentView.Scanline[q];
  end;
end;

procedure TIETransitionEffects.Stop;
begin
  if not fRunning then
    exit;
  if fTransition = iettNone then
    exit;
  fTimer.Enabled := false;
  fRunning := false;
  fCurrentView.Width := 1;
  fCurrentView.Height := 1;
  fSourceShot.Width := 1;
  fSourceShot.Height := 1;
  fTargetShot.Width := 1;
  fTargetShot.Height := 1;

  if assigned(fOwner) then
  begin
    fOwner.NPUnLockPaint;
    if fUpdateOnStop then
      fOwner.Update;
    if assigned(fOnTransitionStop) then
      fOnTransitionStop(fOwner);
  end;
end;


procedure TIETransitionEffects.MakeTransition(DestCanvas : TCanvas; bExplicitUpdate : Boolean = False);
const
  Transitions_That_Use_CurrentViewLines = [iettRandomPoints         ,
                                           iettFadeIn               ,
                                           iettFadeOut              ,
                                           iettCrossDissolve        ,
                                           iettMoveLeftRight1       ,
                                           iettMoveLeftRight2       ,
                                           iettMoveRightLeft1       ,
                                           iettMoveRightLeft2       ,
                                           iettMoveUpDown1          ,
                                           iettMoveUpDown2          ,
                                           iettMoveDownUp1          ,
                                           iettMoveDownUp2          
                                           {$ifdef IEINCLUDEEXTRATRANSITIONS}
                                          ,iettPageFlip2            ,
                                           iettReversePageFlip2     ,
                                           iettCubeRotateFromLeft   ,
                                           iettCubeRotateFromRight  ,
                                           iettCubeRotateFromTop    ,
                                           iettCubeRotateFromBottom ,
                                           iettSoftWipeFromLeft     ,
                                           iettSoftWipeFromRight    ,
                                           iettSoftWipeFromTop      ,
                                           iettSoftWipeFromBottom   ,
                                           iettCubeRotateFromLeft2  ,
                                           iettCubeRotateFromRight2 ,
                                           iettCubeRotateFromTop2   ,
                                           iettCubeRotateFromBottom2
                                           {$endif}
                                           ];
var
  q: Integer; 
begin
  // make transition
  case fTransition of

    iettCrossDissolve         : CrossDissolve(fStep);
    iettFadeOut               : FadeOut(fStep);
    iettFadeIn                : FadeIn(fStep);
    iettFadeOutIn             : FadeOutIn(fStep);
    iettLeftRight1            : LeftRight1(fStep);
    iettLeftRight2            : LeftRight2(fStep);
    iettRightLeft1            : RightLeft1(fStep);
    iettRightLeft2            : RightLeft2(fStep);
    iettUpDown1               : UpDown1(fStep);
    iettUpDown2               : UpDown2(fStep);
    iettDownUp1               : DownUp1(fStep);
    iettDownUp2               : DownUp2(fStep);
    iettMoveLeftRight1        : MoveLeftRight1(fStep);
    iettMoveLeftRight2        : MoveLeftRight2(fStep);
    iettMoveRightLeft1        : MoveRightLeft1(fStep);
    iettMoveRightLeft2        : MoveRightLeft2(fStep);
    iettMoveUpDown1           : MoveUpDown1(fStep);
    iettMoveUpDown2           : MoveUpDown2(fStep);
    iettMoveDownUp1           : MoveDownUp1(fStep);
    iettMoveDownUp2           : MoveDownUp2(fStep);
    iettRandomPoints          : RandomPoints(fStep);
    iettFromUpLeft            : FromUpLeft(fStep);
    iettFromUpRight           : FromUpRight(fStep);
    iettFromBottomLeft        : FromBottomLeft(fStep);
    iettFromBottomRight       : FromBottomRight(fStep);
    iettRandomBoxes           : RandomBoxes(fStep);
    iettCenter1               : Center1(fStep);
    iettCenter2               : Center2(fStep);
    iettCenterZoom1           : CenterZoom1(fStep);
    iettCenterZoom2           : CenterZoom2(fStep);
    iettPanZoom               : PanZoom(fStep);

    {$ifdef IEINCLUDEEXTRATRANSITIONS}

    iettPageFlip              : PageFlipEffect(FStep, True);
    iettPageFlip2             : PageFlipEffect3D(FStep, True);
    iettReversePageFlip       : PageFlipEffect(FStep, False);
    iettReversePageFlip2      : PageFlipEffect3D(FStep, False);

    iettCubeRotateFromLeft    : CubeRotateFromLeft(FStep);
    iettCubeRotateFromRight   : CubeRotateFromRight(FStep);
    iettCubeRotateFromTop     : CubeRotateFromTop(FStep);
    iettCubeRotateFromBottom  : CubeRotateFromBottom(FStep);

    iettSoftWipeFromLeft      : SoftWipeFromLeft(FStep);
    iettSoftWipeFromRight     : SoftWipeFromRight(FStep);
    iettSoftWipeFromTop       : SoftWipeFromTop(FStep);
    iettSoftWipeFromBottom    : SoftWipeFromBottom(FStep);

    iettCubeRotateFromLeft2   : CubeRotateFromLeft3D(FStep);
    iettCubeRotateFromRight2  : CubeRotateFromRight3D(FStep);
    iettCubeRotateFromTop2    : CubeRotateFromTop3D(FStep);
    iettCubeRotateFromBottom2 : CubeRotateFromBottom3D(FStep);

    else
    begin
      // Support for Extra Transition effects
      if (FStep = 0) or FFirstStep then
      begin
        fCurrentView.assign(fSourceShot);
        fTransitionInitialized := False;
      end;
      
      if (FStep > 0) then
      begin
        SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);
        PerformExtraTransitionEffect(fTransition,
                                     fCurrentView.Canvas,
                                     fSourceShot,
                                     fTargetShot,
                                     Rect(0, 0, fCurrentView.Width, fCurrentView.Height),
                                     FStep);
      end;
    end;
    {$endif}
  end;

  // Force update of fCurrentView
  if bExplicitUpdate and (fTransition in Transitions_That_Use_CurrentViewLines)  then
    for q := 0 to fCurrentView.Height - 1 do
      CopyMemory(fCurrentView.Scanline[q], fCurrentViewLines[q], IEBitmapRowLen(fCurrentView.Width, 24, 32));

  // paint result
  BitBlt(DestCanvas.handle, 0, 0, fWidth, fHeight, fCurrentView.Canvas.Handle, 0, 0, SRCCOPY);
end;


procedure TIETransitionEffects.TimerEvent(Sender: TObject);
var
  elap: dword;
begin
  if not fRunning then
    exit;
  if assigned(fOnTransitionStep) then
    fOnTransitionStep(fOwner, fStep);
  inc(fFrames);

  MakeTransition(fOwner.GetCanvas);

  if Assigned(FOnTransitionPaint) then
    fOnTransitionPaint(fOwner, FCurrentView, Self, fStep);
  //
  elap := GetTickCount - fStartTick;
  if elap >= dword(fDuration) then
  begin
    if FUpdateOnStop and (fTransition<>iettPanZoom) then
    begin
      BitBlt(fOwner.GetCanvas.handle, 0, 0, fWidth, fHeight, fTargetShot.Canvas.Handle, 0, 0, SRCCOPY);
      if Assigned(FOnTransitionPaint) then
        fOnTransitionPaint(fOwner, fTargetShot, Self, 1024);
    end;
    Stop;
  end;

  case fTiming of
    iettLinear: fStep := trunc((elap / fDuration) * 1024);  // linear
    iettExponential: fStep := trunc( exp((elap/fDuration)*5-2.718)/10 * 1024); // exponential
    iettLogarithmic: fStep := trunc( (ln((elap/fDuration)*10)+2.718)/5 * 1024 );  // logarithmic
  end;

  if fStep > 1024 then
    fStep := 1024;
  FFirstStep := False;
end;

// TransitionProgress: The percentage that it is has progressed from the start image to the end image (ranging from 0.0 to 100.0)
procedure TIETransitionEffects.CreateBitmap(TransitionProgress : Single; DestBitmap : TBitmap);   
// NPC: 24/10/11
begin
  // Removed v5.0.7. Already handled by PrepareBitmap. Causes A/V on memory copy
  // if (TransitionProgress = 0) or FFirstStep then
  //   fCurrentView.assign(fSourceShot);

  DestBitmap.width   := fWidth;
  DestBitmap.Height  := fHeight;
  DestBitmap.pixelformat := pf24bit;

  FStep :=  Round(1024 * TransitionProgress / 100);
  if fStep > 1024 then
    fStep := 1024;
  if fStep < 0 then
    fStep := 0;

  inc(fFrames);

  // make transition
  MakeTransition(DestBitmap.Canvas, True);
  FFirstStep := False;
end;

procedure TIETransitionEffects.RandomPoints(Step: integer);  
var
  y, fRowLen: integer;
  px1, px2: PRGBROW;
  r, rr: integer;
  s, q: integer;
begin
  fRowLen := IEBitmapRowLen(fWidth, 24, 32);
  if (fStep = 0) or FFirstStep then
    CopyMemory(fCurrentViewLines[fHeight - 1], fSourceShotLines[fHeight - 1], fRowLen * fHeight);

  if fStep > 0 then
  begin
    q := round(Step * fWidth * (Step / 180) / 1024);
    s := q - fAccum1;
    fAccum1 := q;
    for y := 0 to fHeight - 1 do
    begin
      px1 := fCurrentViewLines[y];
      px2 := fTargetShotLines[y];
      for q := 0 to s - 1 do
      begin
        r := random(fWidth);
        px1^[r].r := px2^[r].r;
        px1^[r].b := px2^[r].b;
        px1^[r].g := px2^[r].g;

        if IEGlobalSettings().TransitionsDrawAlternative then
        begin
          for rr := 1 to 5 do
            if r + rr < fWidth then
            begin
              px1^[r + rr].r := px2^[r + rr].r;
              px1^[r + rr].b := px2^[r + rr].b;
              px1^[r + rr].g := px2^[r + rr].g;
            end;
        end;
      end;
    end;
  end;
end;

// fade from background
procedure TIETransitionEffects.FadeIn(Step: integer);
var
  x, y: integer;
  ppx, ppx3: pRGB;
  c1: integer;
  rr, gg, bb: integer;
begin
  c1 := 1024 - Step;
  with TColor2TRGB(FBackground) do
  begin
    rr := r * c1;
    gg := g * c1;
    bb := b * c1;
  end;
  for y := 0 to fHeight - 1 do
  begin
    ppx := fTargetShotLines[y];
    ppx3 := fCurrentViewLines[y];
    for x := 0 to fWidth - 1 do
    begin
      with ppx3^ do
      begin
        r := (ppx^.r * Step + rr) shr 10;
        g := (ppx^.g * Step + gg) shr 10;
        b := (ppx^.b * Step + bb) shr 10;
      end;
      inc(ppx);
      inc(ppx3);
    end;
  end;
end;

procedure TIETransitionEffects.FadeOutIn(Step: integer);
begin
  if Step < 512 then
    FadeOut(Step * 2)
  else
    FadeIn((Step - 512) * 2);
end;

// dissolve to background
procedure TIETransitionEffects.FadeOut(Step: integer);
var
  x, y: integer;
  ppx, ppx3: pRGB;
  c1: integer;
  rr, gg, bb: integer;
begin
  c1 := 1024 - Step;
  with TColor2TRGB(FBackground) do
  begin
    rr := r * Step;
    gg := g * Step;
    bb := b * Step;
  end;
  for y := 0 to fHeight - 1 do
  begin
    ppx := fSourceShotLines[y];
    ppx3 := fCurrentViewLines[y];
    for x := 0 to fWidth - 1 do
    begin
      with ppx3^ do
      begin
        r := (ppx^.r * c1 + rr) shr 10;
        g := (ppx^.g * c1 + gg) shr 10;
        b := (ppx^.b * c1 + bb) shr 10;
      end;
      inc(ppx);
      inc(ppx3);
    end;
  end;
end;

procedure TIETransitionEffects.CrossDissolve(Step: integer);
var
  x, y: integer;
  ppx, ppx2, ppx3: pRGB;
  c1: integer;
begin
  c1 := 1024 - Step;
  for y := 0 to fHeight - 1 do
  begin
    ppx := fSourceShotLines[y];
    ppx2 := fTargetShotLines[y];
    ppx3 := fCurrentViewLines[y];
    for x := 0 to fWidth - 1 do
    begin
      with ppx3^ do
      begin
        r := (ppx^.r * c1 + ppx2^.r * Step) shr 10;
        g := (ppx^.g * c1 + ppx2^.g * Step) shr 10;
        b := (ppx^.b * c1 + ppx2^.b * Step) shr 10;
      end;
      inc(ppx);
      inc(ppx2);
      inc(ppx3);
    end;
  end;
end;

procedure TIETransitionEffects.LeftRight1(Step: integer);
begin
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, 0, 0, ((Step * fWidth) shr 10), fHeight, fTargetShot.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TIETransitionEffects.LeftRight2(Step: integer);
var
  sx: integer;
begin
  sx := (Step * fWidth) shr 10;
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, 0, 0, sx, fHeight, fTargetShot.Canvas.Handle, 0, 0, SRCCOPY);

  if IEGlobalSettings().TransitionsDrawAlternative then
  begin
    fCurrentView.Canvas.Pen.color := clGray;
    fCurrentView.Canvas.Pen.Width := 2;
  end
  else
  begin
    fCurrentView.Canvas.Pen.color := clRed;
    fCurrentView.Canvas.Pen.Width := 1;
  end;
  fCurrentView.Canvas.MoveTo(sx, 0);
  fCurrentView.canvas.LineTo(sx, fHeight);
end;

procedure TIETransitionEffects.RightLeft1(Step: integer);
var
  sx, sx2: integer;
begin
  sx := round(((1024 - Step) * fWidth) / 1024);
  sx2 := round((Step * fWidth) / 1024);
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, sx, 0, sx2, fHeight, fTargetShot.Canvas.Handle, sx, 0, SRCCOPY);
end;

procedure TIETransitionEffects.RightLeft2(Step: integer);
var
  sx, sx2: integer;
begin
  sx := round(((1024 - Step) * fWidth) / 1024);
  sx2 := round((Step * fWidth) / 1024);
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, sx, 0, sx2, fHeight, fTargetShot.Canvas.Handle, sx, 0, SRCCOPY);

  if IEGlobalSettings().TransitionsDrawAlternative then
  begin
    fCurrentView.Canvas.Pen.color := clGray;
    fCurrentView.Canvas.Pen.Width := 2;
  end
  else
  begin
    fCurrentView.Canvas.Pen.color := clRed;
    fCurrentView.Canvas.Pen.Width := 1;
  end;
  fCurrentView.Canvas.MoveTo(sx, 0);
  fCurrentView.canvas.LineTo(sx, fHeight);
end;

procedure TIETransitionEffects.UpDown1(Step: integer);
var
  sy: integer;
begin
  sy := round((Step * fHeight) / 1024);
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, 0, 0, fWidth, sy, fTargetShot.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TIETransitionEffects.UpDown2(Step: integer);
var
  sy: integer;
begin
  sy := round((Step * fHeight) / 1024);
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, 0, 0, fWidth, sy, fTargetShot.Canvas.Handle, 0, 0, SRCCOPY);

  if IEGlobalSettings().TransitionsDrawAlternative then
  begin
    fCurrentView.Canvas.Pen.color := clGray;
    fCurrentView.Canvas.Pen.Width := 2;
  end
  else
  begin
    fCurrentView.Canvas.Pen.color := clRed;
    fCurrentView.Canvas.Pen.Width := 1;
  end;
  fCurrentView.Canvas.MoveTo(0, sy);
  fCurrentView.Canvas.LineTo(fWidth, sy);
end;

procedure TIETransitionEffects.DownUp1(Step: integer);
var
  sy, sy2: integer;
begin
  sy := round(((1024 - Step) * fHeight) / 1024);
  sy2 := round((Step * fHeight) / 1024);
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, 0, sy, fWidth, sy2, fTargetShot.Canvas.Handle, 0, sy, SRCCOPY);
end;

procedure TIETransitionEffects.DownUp2(Step: integer);
var
  sy, sy2: integer;
begin
  sy := round(((1024 - Step) * fHeight) / 1024);
  sy2 := round((Step * fHeight) / 1024);
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, 0, sy, fWidth, sy2, fTargetShot.Canvas.Handle, 0, sy, SRCCOPY);

  if IEGlobalSettings().TransitionsDrawAlternative then
  begin
    fCurrentView.Canvas.Pen.color := clGray;
    fCurrentView.Canvas.Pen.Width := 2;
  end
  else
  begin
    fCurrentView.Canvas.Pen.color := clRed;
    fCurrentView.Canvas.Pen.Width := 1;
  end;
  fCurrentView.Canvas.MoveTo(0, sy);
  fCurrentView.Canvas.LineTo(fWidth, sy);
end;

procedure TIETransitionEffects.MoveLeftRight1(Step: integer);
var
  sx, y: integer;
  ppx, ppx2: PRGB;
begin
  sx := Step * fWidth shr 10;
  for y := 0 to fHeight - 1 do
  begin
    ppx := fCurrentViewLines[y];
    ppx2 := fTargetShotLines[y];
    inc(ppx2, fWidth - sx);
    CopyMemory(ppx, ppx2, sx * 3);
    inc(ppx, sx);
    CopyMemory(ppx, fSourceShotLines[y], (fWidth - sx) * 3);
  end;
end;

procedure TIETransitionEffects.MoveLeftRight2(Step: integer);
var
  sx, y: integer;
  ppx: PRGB;
begin
  sx := Step * fWidth shr 10;
  for y := 0 to fHeight - 1 do
  begin
    ppx := fCurrentViewLines[y];
    CopyMemory(ppx, fTargetShotLines[y], sx * 3);
    inc(ppx, sx);
    CopyMemory(ppx, fSourceShotLines[y], (fWidth - sx) * 3);
  end;
end;

procedure TIETransitionEffects.MoveRightLeft1(Step: integer);
var
  sx, y: integer;
  ppx, ppx2: PRGB;
begin
  sx := (1024 - Step) * fWidth shr 10;
  for y := 0 to fHeight - 1 do
  begin
    ppx := fCurrentViewLines[y];
    ppx2 := fSourceShotLines[y];
    inc(ppx2, fWidth - sx);
    CopyMemory(ppx, ppx2, sx * 3);
    inc(ppx, sx);
    CopyMemory(ppx, fTargetShotLines[y], (fWidth - sx) * 3);
  end;
end;

procedure TIETransitionEffects.MoveRightLeft2(Step: integer);
var
  sx, y: integer;
  ppx: PRGB;
begin
  sx := (1024 - Step) * fWidth shr 10;
  for y := 0 to fHeight - 1 do
  begin
    ppx := fCurrentViewLines[y];
    CopyMemory(ppx, fSourceShotLines[y], sx * 3);
    inc(ppx, sx);
    CopyMemory(ppx, fTargetShotLines[y], (fWidth - sx) * 3);
  end;
end;

procedure TIETransitionEffects.MoveUpDown1(Step: integer);
var
  sy, fRowLen, y: integer;
begin
  sy := Step * fHeight shr 10;
  fRowLen := IEBitmapRowLen(fWidth, 24, 32);
  for y := 0 to sy - 1 do
    CopyMemory(fCurrentViewLines[y], fTargetShotLines[fHeight - sy + y], fRowLen);
  for y := sy to fHeight - 1 do
    CopyMemory(fCurrentViewLines[y], fSourceShotLines[y - sy], fRowLen);
end;

// source image go away

procedure TIETransitionEffects.MoveUpDown2(Step: integer);
var
  sy, fRowLen, y: integer;
begin
  sy := Step * fHeight shr 10;
  fRowLen := IEBitmapRowLen(fWidth, 24, 32);
  for y := 0 to sy - 1 do
    CopyMemory(fCurrentViewLines[y], fTargetShotLines[y], fRowLen);
  for y := sy to fHeight - 1 do
    CopyMemory(fCurrentViewLines[y], fSourceShotLines[y - sy], fRowLen);
end;

procedure TIETransitionEffects.MoveDownUp1(Step: integer);
var
  sy, fRowLen, y: integer;
begin
  sy := (1024 - Step) * fHeight shr 10;
  fRowLen := IEBitmapRowLen(fWidth, 24, 32);
  for y := 0 to sy - 1 do
    CopyMemory(fCurrentViewLines[y], fSourceShotLines[fHeight - sy + y], fRowLen);
  for y := sy to fHeight - 1 do
    CopyMemory(fCurrentViewLines[y], fTargetShotLines[y - sy], fRowLen);
end;

// source image go away

procedure TIETransitionEffects.MoveDownUp2(Step: integer);
var
  sy, fRowLen, y: integer;
begin
  sy := (1024 - Step) * fHeight shr 10;
  fRowLen := IEBitmapRowLen(fWidth, 24, 32);
  for y := 0 to sy - 1 do
    CopyMemory(fCurrentViewLines[y], fSourceShotLines[fHeight - sy + y], fRowLen);
  for y := sy to fHeight - 1 do
    CopyMemory(fCurrentViewLines[y], fTargetShotLines[y], fRowLen);
end;

procedure TIETransitionEffects.FromUpLeft(Step: integer);
var
  sx, sy: integer;
begin
  sx := (1024 - Step) * fWidth shr 10;
  sy := (1024 - Step) * fHeight shr 10;
  if Step = 0 then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  fCurrentView.Canvas.Draw(-sx, -sy, fTargetShot)
end;

procedure TIETransitionEffects.FromUpRight(Step: integer);
var
  sx, sy: integer;
begin
  sx := (1024 - Step) * fWidth shr 10;
  sy := (1024 - Step) * fHeight shr 10;
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  fCurrentView.Canvas.Draw(sx, -sy, fTargetShot);
end;

procedure TIETransitionEffects.FromBottomLeft(Step: integer);
var
  sx, sy: integer;
begin
  sx := (1024 - Step) * fWidth shr 10;
  sy := (1024 - Step) * fHeight shr 10;
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  fCurrentView.Canvas.Draw(-sx, sy, fTargetShot);
end;

procedure TIETransitionEffects.FromBottomRight(Step: integer);
var
  sx, sy: integer;
begin
  sx := (1024 - Step) * fWidth shr 10;
  sy := (1024 - Step) * fHeight shr 10;
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  fCurrentView.Canvas.Draw(sx, sy, fTargetShot);
end;

procedure TIETransitionEffects.RandomBoxes(Step: integer);
const
  Box_Value = 8;
var
  s, q, x, y: integer;
  iBoxDims: integer;
begin
  iBoxDims := Box_Value;
  if IEGlobalSettings().TransitionsDrawAlternative then
    iBoxDims := 2 * Box_Value;

  if fStep = 0 then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot)
  else
  begin
    q := round(Step * (fWidth * fHeight div (iBoxDims * iBoxDims)) * (Step / 180) / 1024);
    s := q - fAccum1;
    fAccum1 := q;
    for q := 0 to s - 1 do
    begin
      x := random(fWidth) and $FFFFFFF8;
      y := random(fheight) and $FFFFFFF8;
      BitBlt(fCurrentView.Canvas.Handle, x, y, iBoxDims, iBoxDims, fTargetShot.Canvas.Handle, x, y, SRCCOPY);
    end;
  end;
end;

// the new image begin from center
procedure TIETransitionEffects.Center1(Step: integer);
var
  sx, sy: integer;
  sx2, sy2: integer;
  cx, cy: integer;
begin
  cx := fWidth shr 1;
  cy := fheight shr 1;
  sx := (Step * cx) shr 10;
  sx2 := Step * fWidth shr 10;
  sy := (Step * cy) shr 10;
  sy2 := step * fHeight shr 10;
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  BitBlt(fCurrentView.Canvas.Handle, cx - sx, cy - sy, sx2, sy2, fTargetShot.Canvas.Handle, cx - sx, cy - sy, SRCCOPY);
end;

// the old image collapse in the center
procedure TIETransitionEffects.Center2(Step: integer);
var
  sx, sy: integer;
  sx2, sy2: integer;
  cx, cy: integer;
begin
  cx := fWidth shr 1;
  cy := fheight shr 1;
  sx := ((1024 - Step) * cx) shr 10;
  sx2 := (1024 - Step) * fWidth shr 10;
  sy := ((1024 - Step) * cy) shr 10;
  sy2 := (1024 - step) * fHeight shr 10;
  fCurrentView.Canvas.Draw(0, 0, fTargetShot);
  BitBlt(fCurrentView.Canvas.Handle, cx - sx, cy - sy, sx2, sy2, fSourceShot.Canvas.Handle, cx - sx, cy - sy, SRCCOPY);
end;

procedure TIETransitionEffects.CenterZoom1(Step: integer);
var
  sx, sy: integer;
  sx2, sy2: integer;
  cx, cy: integer;
begin
  cx := fWidth shr 1;
  cy := fheight shr 1;
  sx := (Step * cx) shr 10;
  sx2 := Step * fWidth shr 10;
  sy := (Step * cy) shr 10;
  sy2 := step * fHeight shr 10;
  if (Step = 0) or FFirstStep then
    fCurrentView.Canvas.Draw(0, 0, fSourceShot);
  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);
  StretchBlt(fCurrentView.Canvas.Handle, cx - sx, cy - sy, sx2, sy2, fTargetShot.Canvas.Handle, 0, 0, fWidth, fHeight, SRCCOPY);
end;

procedure TIETransitionEffects.CenterZoom2(Step: integer);
var
  sx, sy: integer;
  sx2, sy2: integer;
  cx, cy: integer;
begin
  cx := fWidth shr 1;
  cy := fheight shr 1;
  sx := ((1024 - Step) * cx) shr 10;
  sx2 := (1024 - Step) * fWidth shr 10;
  sy := ((1024 - Step) * cy) shr 10;
  sy2 := (1024 - step) * fHeight shr 10;
  fCurrentView.Canvas.Draw(0, 0, fTargetShot);
  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);
  StretchBlt(fCurrentView.Canvas.Handle, cx - sx, cy - sy, sx2, sy2, fSourceShot.Canvas.Handle, 0, 0, fWidth, fHeight, SRCCOPY);
end;

procedure TIETransitionEffects.PanZoom(Step: integer);
var
  bHasBorders: boolean;
  xdst, ydst, dxdst, dydst: Integer;
  xSrc, ySrc, dxSrc, dySrc: Integer;
  q: double;
  iSetLeft, iSetTop, iSetRight, iSetBottom : Integer;

  // Center the displayed portion of image within the window at the correct aspect ratio
  procedure _AdjustDestPos(iShowImgWidth, iShowImgHeight: Integer;  // height and width of the part of the image currently visible
                           iIEViewWidth, iIEViewHeight: Integer;    // the dimensions of the display window
                           var iResultLeft: Integer;
                           var iResultTop: Integer;
                           var iResultWidth: Integer;
                           var iResultHeight: Integer);
  var
    dResizeRatio: Double;
  begin
    iResultLeft := 0;
    iResultTop := 0;
    iResultWidth := 0;
    iResultHeight := 0;
    try
      if (iShowImgHeight / iIEViewHeight)<(iShowImgWidth / iIEViewWidth) then
      // WIDTH IS IMPORTANT
      begin
        // calulate the amount to increase the object size by
        dResizeRatio := iIEViewWidth/iShowImgWidth;

        // now get the new image dimensions
        iResultHeight := round(iShowImgHeight*dResizeRatio);
        iResultWidth := round(iShowImgWidth*dResizeRatio);
      end
      ELSE
      // HEIGHT IS IMPORTANT
      begin
        // calulate the amount to increase the object size by
        dResizeRatio := iIEViewHeight/iShowImgHeight;

        // now get the new image dimensions
        iResultWidth := round(iShowImgWidth*dResizeRatio);
        iResultHeight := round(iShowImgHeight*dResizeRatio);
      end;

      // center image in hte available area
      iResultLeft := trunc((iIEViewWidth /2)-(iResultWidth /2));
      iResultTop := trunc((iIEViewHeight / 2)-(iResultHeight / 2));

    except
      // DIV BY ZERO ERROR DUE TO NULL IMAGE SIZES
    end;
  end;

  procedure ResetMinMaxVals;
  begin
    FMinLeft := round(xSrc);
    FMaxLeft := round(xSrc);
    FMinTop  := round(ySrc);
    FMaxTop  := round(ySrc);
    FMinRight  := trunc(xSrc + dxSrc);
    FMaxRight  := trunc(xSrc + dxSrc);
    FMinBottom := trunc(ySrc + dySrc);
    FMaxBottom := trunc(ySrc + dySrc);
  end;

  procedure UpdateMinMaxVals;
  begin
    if iSetLeft < FMinLeft then FMinLeft := iSetLeft;
    if iSetLeft > FMaxLeft then FMaxLeft := iSetLeft;
    if iSetTop < FMinTop then FMinTop := iSetTop;
    if iSetTop > FMaxTop then FMaxTop := iSetTop;
    if iSetRight < FMinRight then FMinRight := iSetRight;
    if iSetRight > FMaxRight then FMaxRight := iSetRight;
    if iSetBottom < FMinBottom then FMinBottom := iSetBottom;
    if iSetBottom > FMaxBottom then FMaxBottom := iSetBottom;
  end;

begin
  bHasBorders := false;

  xdst := 0;
  ydst := 0;
  dxdst := fWidth;
  dydst := fHeight;

  q := (startRect.Left-endRect.Left)/1024;
  xSrc := trunc(startRect.Left-q*Step);
  q := (startRect.Top-endRect.Top)/1024;
  ySrc := trunc(startRect.Top-q*Step);
  q := (startRect.Right-endRect.Right)/1024;
  dxSrc := trunc(startRect.Right-q*Step)-xSrc;
  q := (startRect.Bottom-endRect.Bottom)/1024;
  dySrc := trunc(startRect.Bottom-q*Step)-ySrc;

  // Are we showing some border area?
  if xSrc < 0 then
  begin
    xSrc := 0;
    bHasBorders := true;
  end;

  if dxSrc>fFullImage.width then
  begin
    dxSrc := fFullImage.width;
    bHasBorders := true;
  end;

  if xSrc+dxSrc>fFullImage.width then
  begin
    xSrc := fFullImage.width-dxSrc;
    bHasBorders := true;
  end;

  if ySrc < 0 then
  begin
    ySrc := 0;    
    bHasBorders := true;
  end;

  if ySrc+dySrc>fFullImage.Height then
  begin
    ySrc := fFullImage.Height-dySrc;
    bHasBorders := true;
  end;

  if dySrc>fFullImage.height then
  begin
    dySrc := fFullImage.height;
    bHasBorders := true;
  end;

  if bHasBorders then
  begin
    // Center the visible image portion within the window (and maintain the aspect ratio)
    _AdjustDestPos(dxSrc, dySrc, fwidth, fheight, 
                   xdst, ydst, dxdst, dydst);

    // Draw the background
    if assigned(fOwner) and (fOwner is TImageEnView) then
      TImageEnView(fowner).DrawBackgroundToCanvas(fCurrentView.Canvas)
    else
    begin                             
      fCurrentView.Canvas.Brush.color := fBackground;
      fCurrentView.Canvas.FillRect(rect(0, 0, fCurrentView.width, fCurrentView.height));
    end;
  end;

  if (Step = 0) or
     (FFirstStep) or
     (FMinLeft = 0) or
     (FMaxRight = 0) or
     (bHasBorders) then
    ResetMinMaxVals;

  // Smooth pan zoom motion
  if startRect.Left > endRect.Left then
    // Getting Smaller
    iSetLeft := min(FMinLeft, round(xSrc))
  else
    // Getting Larger
    iSetLeft := max(FMaxLeft, round(xSrc));

  if startRect.Top > endRect.Top then
    // Getting Smaller
    iSetTop := min(FMinTop, round(ySrc))
  else
    // Getting Larger
    iSetTop := max(FMaxTop, round(ySrc));

  if startRect.Right > endRect.Right then
    // Getting Smaller
    iSetRight := min(FMinRight, iSetLeft+trunc(dxSrc))
  else
    // Getting Larger
    iSetRight := max(FMaxRight, iSetLeft+trunc(dxSrc));

  if startRect.Bottom > endRect.Bottom then
    // Getting Smaller
    iSetBottom := min(FMinBottom, iSetTop + trunc(dySrc))
  else
    // Getting Larger
    iSetBottom := max(FMaxBottom, iSetTop + trunc(dySrc));

  fFullImage.RenderToTBitmapEx(fCurrentView,
                               round(xdst), round(ydst),
                               trunc(dxdst), trunc(dydst),
                               iSetLeft, iSetTop,
                               iSetRight - iSetLeft, iSetBottom - iSetTop,
                               255,
                               IEGlobalSettings().PanZoomQualityFilter,
                               ielNormal);
  UpdateMinMaxVals;
end;





{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.SoftWipeHorizontalEx(Step: integer; bForward : Boolean);
var
  x, y: integer;
  ppx, ppx2, ppx3: pRGB;
  iTransLevel: integer;
  iBlendEndPos: Integer;
  iDistance: Integer;
  iBlendWidth: Integer;
begin
  if bForward = False then
    Step := 1024 - Step;

  iBlendWidth := FWidth div 8;
  if IEGlobalSettings().TransitionsDrawAlternative then
    iBlendWidth := FWidth div 4;

  iBlendEndPos :=  MulDiv(FWidth + iBlendWidth, Step, 1024);
  for y := 0 to fHeight - 1 do
  begin                        
    if bForward then
    begin                  
      ppx := fSourceShotLines[y];
      ppx2 := fTargetShotLines[y];
    end
    else
    begin                      
      ppx := fTargetShotLines[y];
      ppx2 := fSourceShotLines[y];
    end;
    ppx3 := fCurrentViewLines[y];
    for x := 0 to fWidth - 1 do
    begin
      if X > iBlendEndPos then
        iTransLevel := 0
      else
      begin
        iDistance := iBlendEndPos - X;
        iTransLevel :=  MulDiv(1024, iDistance, iBlendWidth);
        if iTransLevel > 1024 then
          iTransLevel := 1024;
      end;

      with ppx3^ do
      begin
        r := (ppx^.r * (1024 - iTransLevel) + ppx2^.r * iTransLevel) shr 10;
        g := (ppx^.g * (1024 - iTransLevel) + ppx2^.g * iTransLevel) shr 10;
        b := (ppx^.b * (1024 - iTransLevel) + ppx2^.b * iTransLevel) shr 10;
      end;
      inc(ppx);
      inc(ppx2);
      inc(ppx3);
    end;
  end;
end; 
{$endif}        
      

{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.SoftWipeFromRight(Step: integer);
begin
  SoftWipeHorizontalEx(Step, False);
end;
{$endif}
      

{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.SoftWipeFromLeft(Step: integer);
begin
  SoftWipeHorizontalEx(Step, True);
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.SoftWipeVerticalEx(Step: integer; bForward : Boolean);
var
  x, y: integer;
  ppx, ppx2, ppx3: pRGB;
  iTransLevel: integer;
  iBlendEndPos: Integer;
  iDistance: Integer;
  iBlendHeight: Integer;
begin
  if bForward = False then
    Step := 1024 - Step;

  iBlendHeight := FHeight div 8;
  if IEGlobalSettings().TransitionsDrawAlternative then
    iBlendHeight := FHeight div 4;

  iBlendEndPos :=  MulDiv(fHeight + iBlendHeight, Step, 1024);
  for y := 0 to fHeight - 1 do
  begin                        
    if bForward then
    begin                  
      ppx := fSourceShotLines[y];
      ppx2 := fTargetShotLines[y];
    end
    else
    begin                      
      ppx := fTargetShotLines[y];
      ppx2 := fSourceShotLines[y];
    end;
    ppx3 := fCurrentViewLines[y];

    if Y > iBlendEndPos then
      iTransLevel := 0
    else
    begin
      iDistance := iBlendEndPos - Y;
      iTransLevel :=  MulDiv(1024, iDistance, iBlendHeight);
      if iTransLevel > 1024 then
        iTransLevel := 1024;
    end;
             
    for x := 0 to fWidth - 1 do
    begin
      with ppx3^ do
      begin
        r := (ppx^.r * (1024 - iTransLevel) + ppx2^.r * iTransLevel) shr 10;
        g := (ppx^.g * (1024 - iTransLevel) + ppx2^.g * iTransLevel) shr 10;
        b := (ppx^.b * (1024 - iTransLevel) + ppx2^.b * iTransLevel) shr 10;
      end;
      inc(ppx);
      inc(ppx2);
      inc(ppx3);
    end;
  end;
end;
{$endif}
                   

{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.SoftWipeFromTop(Step: integer);
begin
  SoftWipeVerticalEx(Step, True);
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.SoftWipeFromBottom(Step: integer);
begin
  SoftWipeVerticalEx(Step, False);
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.PageFlipDarkenEdges(iCurrentPosition, Step : Integer; bForward: Boolean);
const
  Initial_Darken_Level = 0.25;
  Darkness_Reduction = 0.5;
var
  iX, iY: integer;
  ppx: pRGB;
  rr, gg, bb: integer;
  iFadeStep: integer;
  bDoDarken: Boolean;
begin
  if Step < 512 then
    iFadeStep :=  Round(Step + Round(1024 * Initial_Darken_Level) / Darkness_Reduction)
  else
    iFadeStep :=  Round(1024 - Step + Round(1024 * Initial_Darken_Level) / Darkness_Reduction);

  if iFadeStep > 1024 then
    iFadeStep := 1024;     

  with TColor2TRGB(fBackground) do
  begin
    rr := r * (1024 - iFadeStep);
    gg := g * (1024 - iFadeStep);
    bb := b * (1024 - iFadeStep);
  end;
  for iY := 0 to fHeight - 1 do
  begin
    ppx := fCurrentViewLines[iY];
    for iX := 0 to fWidth - 1 do
    begin
      if Step < 512 then
        bDoDarken := iX > iCurrentPosition
      else
        bDoDarken := iX < iCurrentPosition;
      if bForward = False then
        bDoDarken := not bDoDarken;

      if bDoDarken then
        with ppx^ do
        begin
          r := (ppx^.r * iFadeStep + rr) shr 10;
          g := (ppx^.g * iFadeStep + gg) shr 10;
          b := (ppx^.b * iFadeStep + bb) shr 10;
        end;

      inc(ppx);
    end;
  end;

  fCurrentView.Canvas.MoveTo(iCurrentPosition, 0);
  fCurrentView.Canvas.LineTo(iCurrentPosition, fHeight);
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}       
// Page Flip Transition
procedure TIETransitionEffects.PageFlipEffect(Step: Integer; bForward: Boolean);
var
  W, H, X: Integer;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;

  if (Step = 0) or FFirstStep then
    StretchBlt(fCurrentView.Canvas.Handle, 0, 0, W, H,
               FSourceShot.Canvas.Handle, 0, 0, W, H,
               SRCCOPY);

  if Step = 0 then
    exit;

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  fCurrentView.Canvas.Pen.Color := clBlack;
  fCurrentView.Canvas.Pen.Width := 1;
  fCurrentView.Canvas.Pen.Style := psSolid;

  if bForward then
  begin
    // draw the new image on the RHS
    StretchBlt(fCurrentView.Canvas.Handle, w div 2, 0, w div 2, H,
               FTargetShot.Canvas.Handle, w div 2, 0, W div 2, H,
               SRCCOPY);

    if Step <= 512 then
    begin
      X := MulDiv(W, 512 - Step, 1024);

      // Right Hand fCurrentView.Canvas
      StretchBlt(fCurrentView.Canvas.Handle, w div 2, 0, X, H,
                 FSourceShot.Canvas.Handle, w div 2, 0, W div 2, H,
                 SRCCOPY);

      if IEGlobalSettings().TransitionsDrawAlternative = False then
        PageFlipDarkenEdges(w div 2 + X, Step, True);
    end
    else
    begin                  
      X := MulDiv(W, Step - 512, 512) div 2;

      if IEGlobalSettings().TransitionsDrawAlternative = False then
        StretchBlt(fCurrentView.Canvas.Handle, 0, 0, W div 2, H,
                   FSourceShot.Canvas.Handle, 0, 0, W div 2, H,
                   SRCCOPY);

      // Left Hand side
      StretchBlt(fCurrentView.Canvas.Handle, W div 2 - X, 0, X, H,
                 FTargetShot.Canvas.Handle, 0, 0, W div 2, H,
                 SRCCOPY);

      if IEGlobalSettings().TransitionsDrawAlternative = False then
        PageFlipDarkenEdges(w div 2 - X, Step, True);
    end;
  end
  else
  begin
    // draw the New image on the LHS
    StretchBlt(fCurrentView.Canvas.Handle, 0, 0, w div 2, H,
               FTargetShot.Canvas.Handle, 0, 0, W div 2, H,
               SRCCOPY);

    if Step <= 512 then
    begin
      X := MulDiv(W, Step, 1024);

      // Left Hand side
      StretchBlt(fCurrentView.Canvas.Handle, X, 0, w div 2 - X, H,
                 FSourceShot.Canvas.Handle, 0, 0, W div 2, H,
                 SRCCOPY);

      if IEGlobalSettings().TransitionsDrawAlternative = False then
        PageFlipDarkenEdges(X, Step, False);
    end
    else
    begin
      X := MulDiv(W, Step - 512, 512) div 2;

      // draw the old image on the RHS
      StretchBlt(fCurrentView.Canvas.Handle, w div 2, 0, w div 2, H,
                 FSourceShot.Canvas.Handle, w div 2, 0, W div 2, H,
                 SRCCOPY);

      // Right Hand Side
      StretchBlt(fCurrentView.Canvas.Handle, W div 2, 0, X, H,
                 FTargetShot.Canvas.Handle, w div 2, 0, W div 2, H,
                 SRCCOPY);

      if IEGlobalSettings().TransitionsDrawAlternative = False then
        PageFlipDarkenEdges(w div 2 + X, Step, False);
    end;
  end;
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
// 3D Page Flip Transition
procedure TIETransitionEffects.PageFlipEffect3D(Step: Integer; bForward: Boolean);
const
  Inset_Factor = 10;     // 1/10 the height
var
  W, H, X, iInset: Integer;
  IECurrentView   : TIEBitmap;
  IESourceShotLHS, IESourceShotRHS : TIEBitmap;
  IETargetShotLHS, IETargetShotRHS : TIEBitmap;
  iEdgeInset: Integer;
  iAlpha: Integer;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;

  if (Step = 0) or FFirstStep then
    StretchBlt(fCurrentView.Canvas.Handle, 0, 0, W, H,
               FSourceShot.Canvas.Handle, 0, 0, W, H,
               SRCCOPY);

  if Step = 0 then
    exit;                                 

  if Step <= 512 then
    iInset  := MulDiv(H, Step, 512 * Inset_Factor)
  else
    iInset  := MulDiv(H, 1024 - Step, 512 * Inset_Factor);
  iEdgeInset := iInset div 2;

  fCurrentView.Canvas.Brush.Color := fBackground;
  fCurrentView.Canvas.fillRect(Rect(0, 0, fCurrentView.Width, fCurrentView.height));

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  fCurrentView.Canvas.Pen.Color := clBlack;
  fCurrentView.Canvas.Pen.Width := 1;
  fCurrentView.Canvas.Pen.Style := psSolid;
                                       
  IECurrentView   := TIEBitmap.create;
  IESourceShotLHS := TIEBitmap.create(fSourceShot, Rect(0, 0, W div 2, H));
  IESourceShotRHS := TIEBitmap.create(fSourceShot, Rect(W div 2, 0, W, H));
  IETargetShotLHS := TIEBitmap.create(fTargetShot, Rect(0, 0, W div 2, H));
  IETargetShotRHS := TIEBitmap.create(fTargetShot, Rect(w div 2, 0, W, H));
  try
    IECurrentView.EncapsulateTBitmap(fCurrentView, False);

    if bForward then
    begin
      // FORWARD PAGE FLIP

      // Draw old image on LHS
      IEPerspectiveTransform(IESourceShotLHS,
                             IECurrentView,
                             iEdgeInset, iEdgeInset,                 // Top-Left
                             W div 2, iInset,                        // Top-Right
                             W div 2, H - iInset,                    // Bottom-Right
                             iEdgeInset, H - iEdgeInset,             // Bottom-Left
                             -1, -1, True);

      // Draw new image on the RHS
      IEPerspectiveTransform(IETargetShotRHS,
                             IECurrentView,
                             w div 2, iInset,                        // Top-Left
                             W - iEdgeInset, iEdgeInset,             // Top-Right
                             W - iEdgeInset, H - iEdgeInset,         // Bottom-Right
                             w div 2, H - iInset,                    // Bottom-Left
                             -1, -1, True);

      if Step <= 512 then
      begin
        X := MulDiv(W, 512 - Step, 1024);

        // Draw old image flipping on RHS
        IEPerspectiveTransform(IESourceShotRHS,
                               IECurrentView,
                               w div 2, iInset,                      // Top-Left
                               W div 2 + X, 0,                       // Top-Right
                               W div 2 + X, H ,                      // Bottom-Right
                               w div 2, H - iInset,                  // Bottom-Left
                               -1, -1, True);
      end
      else
      begin
        X := MulDiv(W, Step - 512, 512) div 2;

        // Draw New image flipping on LHS
        IEPerspectiveTransform(IETargetShotLHS,
                               IECurrentView,
                               W div 2 - X, 0,                       // Top-Left
                               W div 2, iInset,                      // Top-Right
                               W div 2, H - iInset,                  // Bottom-Right
                               w div 2 - X, H ,                      // Bottom-Left
                               -1, -1, True);
      end;
    end
    else
    begin
      // REVERSE PAGE FLIP

      // Draw new image on the LHS
      IEPerspectiveTransform(IETargetShotLHS,
                             IECurrentView,
                             iEdgeInset, 0,                          // Top-Left
                             W div 2, iInset,                        // Top-Right
                             W div 2, H - iInset,                    // Bottom-Right
                             iEdgeInset, H,                          // Bottom-Left
                             -1, -1, True);

      // Draw old image on RHS
      IEPerspectiveTransform(IESourceShotRHS,
                             IECurrentView,
                             w div 2, iInset,                        // Top-Left
                             W - iEdgeInset, 0,                      // Top-Right
                             W - iEdgeInset, H,                      // Bottom-Right
                             w div 2, H - iInset,                    // Bottom-Left
                             -1, -1, True);

      if Step <= 512 then
      begin
        X := MulDiv(W, Step, 1024);       

        // Draw old image flipping on LHS
        IEPerspectiveTransform(IESourceShotLHS,
                               IECurrentView,
                               X, 0,                                 // Top-Left
                               W div 2, iInset,                      // Top-Right
                               W div 2, H - iInset,                  // Bottom-Right
                               X, H,                                 // Bottom-Left
                               -1, -1, True);
      end
      else
      begin
        X := MulDiv(W, Step - 512, 512) div 2;   

        // Draw new image flipping on RHS
        IEPerspectiveTransform(IETargetShotRHS,
                               IECurrentView,
                               w div 2, iInset,                      // Top-Left
                               W div 2 + X, 0,                       // Top-Right
                               W div 2 + X, H,                       // Bottom-Right
                               w div 2, H - iInset,                  // Bottom-Left
                               -1, -1, True);
      end;
    end;

  finally
    FreeAndNil(IESourceShotLHS);
    FreeAndNil(IESourceShotRHS);
    FreeAndNil(IETargetShotLHS);
    FreeAndNil(IETargetShotRHS);
    FreeAndNil(IECurrentView);
  end;

  // Darken half shown pages
  if bForward then
  begin
    if Step <= 512 then
      PageFlipDarkenEdges(w div 2 + X, Step, True)
    else
      PageFlipDarkenEdges(w div 2 - X, Step, True);
  end
  else
  begin
    if Step <= 512 then
      PageFlipDarkenEdges(X, Step, False)
    else
      PageFlipDarkenEdges(w div 2 + X, Step, False);
  end;

  // DRAW PAGE CENTER LINE
  
  if IEGlobalSettings().TransitionsDrawAlternative = False then
  begin
    if Step <= 512 then
      iAlpha  := MulDiv(Step, 255, 1024)
    else
      iAlpha  := MulDiv(1024 - Step, 255, 1024);
    ColorBlend(fCurrentView.Canvas.Handle,
               Rect(w div 2, 0, w div 2 + 1, fHeight),
               fBackground,
               iAlpha);
  end;
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateDarkenEdges(bVertical, bForward: Boolean; iCurrentPosition, Step : Integer);
const
  Initial_Darken_Level = 0.15;
  Darkness_Reduction = 0.6;
var
  iX, iY: integer;
  ppx: pRGB;
  rr1, gg1, bb1: integer;
  rr2, gg2, bb2: integer;
  bFirstHalf: Boolean; 
  iFadeStep1, iFadeStep2 : integer;
begin
  if bForward then
  begin
    iFadeStep1 :=  Round(Step + Round(1024 * Initial_Darken_Level) / Darkness_Reduction);
    iFadeStep2 :=  Round(1024 - Step + Round(1024 * Initial_Darken_Level) / Darkness_Reduction);
  end
  else
  begin
    iFadeStep1 :=  Round(1024 - Step + Round(1024 * Initial_Darken_Level) / Darkness_Reduction);
    iFadeStep2 :=  Round(Step + Round(1024 * Initial_Darken_Level) / Darkness_Reduction);
  end;
  
  if iFadeStep1 > 1024 then
    iFadeStep1 := 1024;
  if iFadeStep2 > 1024 then
    iFadeStep2 := 1024;

  with TColor2TRGB(fBackground) do
  begin
    rr1 := r * iFadeStep1;
    gg1 := g * iFadeStep1;
    bb1 := b * iFadeStep1;
  end;
  with TColor2TRGB(fBackground) do
  begin
    rr2 := r * iFadeStep2;
    gg2 := g * iFadeStep2;
    bb2 := b * iFadeStep2;
  end;

  for iY := 0 to fCurrentView.Height - 1 do
  begin
    ppx := fCurrentViewLines[iY];
    for iX := 0 to fCurrentView.Width - 1 do
    begin
      if bVertical then
        bFirstHalf := (iY < iCurrentPosition)
      else                        
        bFirstHalf := (iX < iCurrentPosition);

      if bFirstHalf then
      begin      
        // Darken LHS/Top
        with ppx^ do
        begin
          r := (ppx^.r * iFadeStep1 + rr1) shr 10;
          g := (ppx^.g * iFadeStep1 + gg1) shr 10;
          b := (ppx^.b * iFadeStep1 + bb1) shr 10;
        end
      end
      else
      begin         
        // Darken RHS/Bottom
        with ppx^ do
        begin
          r := (ppx^.r * iFadeStep2 + rr2) shr 10;
          g := (ppx^.g * iFadeStep2 + gg2) shr 10;
          b := (ppx^.b * iFadeStep2 + bb2) shr 10;
        end;
      end;

      inc(ppx);
    end;
  end;
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateFromRight(Step: integer);
var
  W, H, X, Y: Integer;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;
  if W >= H then
    X := W - MulDiv(W, Step, 1024)
  else
  begin
    Y := MulDiv(H, Step, 1024);
    X := W - MulDiv(Y, W, H);
  end;

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  // LHS
  StretchBlt(fCurrentView.Canvas.Handle, 0, 0, X, H,
             fSourceShot.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  // RHS
  StretchBlt(fCurrentView.Canvas.Handle, X, 0, W - X, H,
             fTargetShot.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  // Now fade the half shown sides     
  if IEGlobalSettings().TransitionsDrawAlternative = False then
    CubeRotateDarkenEdges(False, False, X, Step);
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateFromRight3D(Step: integer);
const
  Inset_Factor = 4; // 1/4 of height is inset
var
  W, H, X, Y: Integer;
  iLeftInset, iRightInset: Integer;
  IECurrentView, IESourceShot, IETargetShot: TIEBitmap;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;
  if W >= H then
    X := W - MulDiv(W, Step, 1024)
  else
  begin
    Y := MulDiv(H, Step, 1024);
    X := W - MulDiv(Y, W, H);
  end;

  iLeftInset  := MulDiv(H, Step, 1024 * Inset_Factor);
  iRightInset := MulDiv(H, 1024 - Step, 1024 * Inset_Factor);

  fCurrentView.Canvas.Brush.Color := fBackground;
  fCurrentView.Canvas.fillRect(Rect(0, 0, fCurrentView.Width, fCurrentView.height));      

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  IESourceShot  := TIEBitmap.create;
  IECurrentView := TIEBitmap.create;
  IETargetShot  := TIEBitmap.create;

  try
    IESourceShot.EncapsulateTBitmap(fSourceShot, False);
    IECurrentView.EncapsulateTBitmap(fCurrentView, False);
    IETargetShot.EncapsulateTBitmap(fTargetShot, False);

    // LHS
    IEPerspectiveTransform(IESourceShot,
                           IECurrentView,
                           0, iLeftInset,        // Top-Left
                           X, 0,                 // Top-Right
                           X, H,                 // Bottom-Right
                           0, H - iLeftInset,    // Bottom-Left
                           -1, -1, True);

    // RHS
    IEPerspectiveTransform(IETargetShot,
                           IECurrentView,
                           X, 0,                 // Top-Left
                           W, iRightInset,       // Top-Right
                           W, H - iRightInset,   // Bottom-Right
                           X, H,                 // Bottom-Left
                           -1, -1, True);

  finally
    FreeAndNil(IESourceShot);
    FreeAndNil(IECurrentView);
    FreeAndNil(IETargetShot);
  end;

  // Now fade the half shown sides       
  if IEGlobalSettings().TransitionsDrawAlternative = False then
    CubeRotateDarkenEdges(False, False, X, Step);
end; 
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateFromLeft(Step: integer);
var
  W, H, X, Y: Integer;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;
  if W >= H then
    X := MulDiv(W, Step, 1024)
  else
  begin
    Y := MulDiv(H, Step, 1024);
    X := MulDiv(Y, W, H);
  end;

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  // Draw LHS
  StretchBlt(fCurrentView.Canvas.Handle, 0, 0, X, H,
             fTargetShot.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  // Draw RHS
  StretchBlt(fCurrentView.Canvas.Handle, X, 0, W - X, H,
             fSourceShot.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  // Now fade the half shown sides      
  if IEGlobalSettings().TransitionsDrawAlternative = False then
    CubeRotateDarkenEdges(False, True, X, Step);
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateFromLeft3D(Step: integer);
const
  Inset_Factor = 4; // 1/4 of height is inset
var
  W, H, X, Y: Integer;
  iLeftInset, iRightInset: Integer;
  IECurrentView, IESourceShot, IETargetShot: TIEBitmap;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;
  if W >= H then
    X := MulDiv(W, Step, 1024)
  else
  begin
    Y := MulDiv(H, Step, 1024);
    X := MulDiv(Y, W, H);
  end;

  iLeftInset  := MulDiv(H, 1024 - Step, 1024 * Inset_Factor);
  iRightInset := MulDiv(H, Step, 1024 * Inset_Factor);

  fCurrentView.Canvas.Brush.Color := fBackground;
  fCurrentView.Canvas.fillRect(Rect(0, 0, fCurrentView.Width, fCurrentView.height));      

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  IESourceShot  := TIEBitmap.create;
  IECurrentView := TIEBitmap.create;
  IETargetShot  := TIEBitmap.create;

  try
    IESourceShot.EncapsulateTBitmap(fSourceShot, False);
    IECurrentView.EncapsulateTBitmap(fCurrentView, False);
    IETargetShot.EncapsulateTBitmap(fTargetShot, False);

    // LHS
    IEPerspectiveTransform(IETargetShot,
                           IECurrentView,
                           0, iLeftInset,        // Top-Left
                           X, 0,                 // Top-Right
                           X, H,                 // Bottom-Right
                           0, H - iLeftInset,    // Bottom-Left
                           -1, -1, True);

    // RHS
    IEPerspectiveTransform(IESourceShot,
                           IECurrentView,
                           X, 0,                 // Top-Left
                           W, iRightInset,       // Top-Right
                           W, H - iRightInset,   // Bottom-Right
                           X, H,                 // Bottom-Left
                           -1, -1, True);

  finally
    FreeAndNil(IESourceShot);
    FreeAndNil(IECurrentView);
    FreeAndNil(IETargetShot);
  end;

  // Now fade the half shown sides
  if IEGlobalSettings().TransitionsDrawAlternative = False then
    CubeRotateDarkenEdges(False, True, X, Step);
end; 
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateFromBottom(Step: integer);
var
  W, H, X, Y: Integer;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;

  if W >= H then
  begin
    X := MulDiv(W, Step, 1024);
    Y := H - MulDiv(X, H, W);
  end
  else
  begin
    Y := H - MulDiv(H, Step, 1024);
  end;

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  // Draw Top
  StretchBlt(fCurrentView.Canvas.Handle, 0, 0, W, Y,
             fSourceShot.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);

  // Draw Bottom
  StretchBlt(fCurrentView.Canvas.Handle, 0, Y, W, H - Y,
             fTargetShot.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);


  // Now fade the half shown sides      
  if IEGlobalSettings().TransitionsDrawAlternative = False then
    CubeRotateDarkenEdges(True, False, Y, Step);
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateFromBottom3D(Step: integer);
const
  Inset_Factor = 4; // 1/4 of height is inset
var
  W, H, X, Y: Integer;
  iTopInset, iBottomInset: Integer;
  IECurrentView, IESourceShot, IETargetShot: TIEBitmap;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;
  if W >= H then
  begin
    X := MulDiv(W, Step, 1024);
    Y := H - MulDiv(X, H, W);
  end
  else
  begin
    Y := H - MulDiv(H, Step, 1024);
  end;

  iTopInset  := MulDiv(W, Step, 1024 * Inset_Factor);
  iBottomInset := MulDiv(W, 1024 - Step, 1024 * Inset_Factor);

  fCurrentView.Canvas.Brush.Color := fBackground;
  fCurrentView.Canvas.fillRect(Rect(0, 0, fCurrentView.Width, fCurrentView.height));      

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  IESourceShot  := TIEBitmap.create;
  IECurrentView := TIEBitmap.create;
  IETargetShot  := TIEBitmap.create;

  try
    IESourceShot.EncapsulateTBitmap(fSourceShot, False);
    IECurrentView.EncapsulateTBitmap(fCurrentView, False);
    IETargetShot.EncapsulateTBitmap(fTargetShot, False);

    // Top
    IEPerspectiveTransform(IESourceShot,
                           IECurrentView,
                           iTopInset, 0,       // Top-Left
                           W - iTopInset, 0,   // Top-Right
                           W, Y,               // Bottom-Right
                           0, Y,               // Bottom-Left
                           -1, -1, True);

    // Bottom
    IEPerspectiveTransform(IETargetShot,
                           IECurrentView,
                           0, Y,                // Top-Left
                           W, Y,                // Top-Right
                           W - iBottomInset, H, // Bottom-Right
                           iBottomInset, H,     // Bottom-Left
                           -1, -1, True);

  finally
    FreeAndNil(IESourceShot);
    FreeAndNil(IECurrentView);
    FreeAndNil(IETargetShot);
  end;

  // Now fade the half shown sides   
  if IEGlobalSettings().TransitionsDrawAlternative = False then
    CubeRotateDarkenEdges(True, False, Y, Step);
end; 
{$endif}



{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateFromTop(Step: integer);
var
  W, H, X, Y: Integer;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;

  if W >= H then
  begin
    X := MulDiv(W, Step, 1024);
    Y := MulDiv(X, H, W);
  end
  else
  begin
    Y := MulDiv(H, Step, 1024);
  end;

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  // Draw Top
  StretchBlt(fCurrentView.Canvas.Handle, 0, 0, W, Y,
             fTargetShot.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);
             
  // Draw Bottom
  StretchBlt(fCurrentView.Canvas.Handle, 0, Y, W, H - Y,
             fSourceShot.Canvas.Handle, 0, 0, W, H,
             SRCCOPY);


  // Now fade the half shown sides  
  if IEGlobalSettings().TransitionsDrawAlternative = False then
    CubeRotateDarkenEdges(True, True, Y, Step);
end;
{$endif}


{$ifdef IEINCLUDEEXTRATRANSITIONS}
procedure TIETransitionEffects.CubeRotateFromTop3D(Step: integer);
const
  Inset_Factor = 4; // 1/4 of height is inset
var
  W, H, X, Y: Integer;
  iTopInset, iBottomInset: Integer;
  IECurrentView, IESourceShot, IETargetShot: TIEBitmap;
begin
  W := fCurrentView.Width;
  H := fCurrentView.Height;
  if W >= H then
  begin
    X := MulDiv(W, Step, 1024);
    Y := MulDiv(X, H, W);
  end
  else
  begin
    Y := MulDiv(H, Step, 1024);
  end;

  iTopInset    := MulDiv(W, 1024 - Step, 1024 * Inset_Factor);
  iBottomInset := MulDiv(W, Step, 1024 * Inset_Factor);

  fCurrentView.Canvas.Brush.Color := fBackground;
  fCurrentView.Canvas.fillRect(Rect(0, 0, fCurrentView.Width, fCurrentView.height));      

  SetStretchBltMode(fCurrentView.Canvas.Handle, HALFTONE);

  IESourceShot  := TIEBitmap.create;
  IECurrentView := TIEBitmap.create;
  IETargetShot  := TIEBitmap.create;

  try
    IESourceShot.EncapsulateTBitmap(fSourceShot, False);
    IECurrentView.EncapsulateTBitmap(fCurrentView, False);
    IETargetShot.EncapsulateTBitmap(fTargetShot, False);

    // Top
    IEPerspectiveTransform(IETargetShot,
                           IECurrentView,
                           iTopInset, 0,       // Top-Left
                           W - iTopInset, 0,   // Top-Right
                           W, Y,               // Bottom-Right
                           0, Y,               // Bottom-Left
                           -1, -1, True);

    // Bottom
    IEPerspectiveTransform(IESourceShot,
                           IECurrentView,
                           0, Y,                // Top-Left
                           W, Y,                // Top-Right
                           W - iBottomInset, H, // Bottom-Right
                           iBottomInset, H,     // Bottom-Left
                           -1, -1, True);

  finally
    FreeAndNil(IESourceShot);
    FreeAndNil(IECurrentView);
    FreeAndNil(IETargetShot);
  end;

  // Now fade the half shown sides   
  if IEGlobalSettings().TransitionsDrawAlternative = False then
    CubeRotateDarkenEdges(True, True, Y, Step);
end;

{$endif}

// Convert a TIEPanZoomType effect type to a starting and ending position
procedure GetPanZoomEffectStartEndRects(iIEClientWidth, iIEClientHeight : Integer;    // ClientWidth and ClientHeight of the display window
                                        iBitmapWidth, iBitmapHeight : Integer;        // Width and Height of the Bitmap
                                        PanZoomEffect : TIEPanZoomType;               // Effect to use
                                        iMaxZoom : Integer;                           // For zoom effects, how much should we zoom in/out, e.g. 20 for 20%
                                        out StartRect: TRect;                         // Will be filled with the first display position
                                        out EndRect: TRect);                          // Will be filled with the last display position

  function RectWidth(ARect: Trect): Integer;
  begin
    result := ARect.right - ARect.left;
  end;

  function RectHeight(ARect: Trect): Integer;
  begin
    result := ARect.bottom - ARect.top;
  end;

  function AreSimilar(val1, val2 : Double): boolean;
  begin
    result := abs(val1 - val2) <= 0.01;
  end;

const
  _ipsTopLeft         = 0;
  _ipsTop             = 1;
  _ipsTopRight        = 2;
  _ipsLeft            = 3;
  _ipsCenter          = 4;
  _ipsRight           = 5;
  _ipsBottomLeft      = 6;
  _ipsBottom          = 7;
  _ipsBottomRight     = 8;
  
  Buffer_Zone = 2;  // avoid display of a 1 pixel border  
  Default_Zoom_Level = 20;
var
  TempRect: TRect;
  VisibleImageRect : TRect;  
  BigPic : TPoint;                  // A portion of the image at full width or height of the image
  bCroppedToAspectRatio : Boolean;  // Image has same aspect ratio as display window
  bIsPortraitImage : boolean;       // Original image portrait or landscape?  Use to determine the best way to display some effects

  function _GetRect(iPos : Integer; iWidth, iHeight : Integer): TRect;
  var
    iTop: Integer;
    iLeft: Integer;
   begin

    // HORIZONTAL POS
    case iPos of

      _ipsTopLeft,
      _ipsLeft,
      _ipsBottomLeft  : iLeft := VisibleImageRect.Left;

      _ipsTopRight,
      _ipsRight,
      _ipsBottomRight : iLeft := VisibleImageRect.Left + RectWidth(VisibleImageRect) - iWidth;

      {_ipsTop,
       _ipsCenter,
       _ipsBottom }
      else              iLeft := VisibleImageRect.Left + (RectWidth(VisibleImageRect) - iWidth) div 2;

    end;

    // VERTICAL POS
    case iPos of

      _ipsTopLeft,
      _ipsTop,
      _ipsTopRight    : iTop := VisibleImageRect.Top;

      _ipsBottomLeft,
      _ipsBottom,
      _ipsBottomRight : iTop := VisibleImageRect.Top + RectHeight(VisibleImageRect) - iHeight;

      {_ipsLeft,
       _ipsCenter,
       _ipsRight   }
      else              iTop := VisibleImageRect.Top + (RectHeight(VisibleImageRect) - iHeight) div 2;

    end;

    result := Rect(iLeft, iTop, iLeft + iWidth, iTop + iHeight);
  end;

  function _GetSmallRect(iPos: Integer): Trect;
  begin
    result := _GetRect(iPos,
                       MulDiv(BigPic.X, 100 - iMaxZoom, 100),
                       MulDiv(BigPic.Y, 100 - iMaxZoom, 100));
  end;

  function _GetBigRect(iPos: Integer): Trect;
  begin
    result := _GetRect(iPos, BigPic.X, BigPic.Y);
  end;

  // The rect to display a full image
  function _GetFullImageRect: Trect;
  begin
    result := _GetRect(_ipsCenter, RectWidth(VisibleImageRect), RectHeight(VisibleImageRect));
  end;

  function _GetFullImageSmallRect(iPos: Integer): Trect;
  begin
    result := _GetRect(iPos,
                       MulDiv(RectWidth(VisibleImageRect), 100 - iMaxZoom, 100),
                       MulDiv(RectHeight(VisibleImageRect), 100 - iMaxZoom, 100));
  end;

begin
  VisibleImageRect := Rect(Buffer_Zone, Buffer_Zone, iBitmapWidth - 2 * Buffer_Zone, iBitmapHeight - 2 * Buffer_Zone);
  bCroppedToAspectRatio := AreSimilar(RectWidth(VisibleImageRect) / RectHeight(VisibleImageRect), iIEClientwidth / iIEClientheight); // Image is cropped or matches the output aspect ratio
  bIsPortraitImage := iBitmapHeight > iBitmapWidth;
  if iMaxZoom < 2 then
    iMaxZoom := Default_Zoom_Level;

  if (RectHeight(VisibleImageRect) / iIEClientheight) > (RectWidth(VisibleImageRect) / iIEClientwidth) then
  begin
    // Vertical Slide - PIC IS AS WIDE AS THE IMAGE
    BigPic.X := RectWidth(VisibleImageRect);
    BigPic.Y := round(BigPic.X * iIEClientHeight/iIEClientWidth);
  end
  ELSE
  begin
    // Horizontal Slide  - PIC IS AS HIGH AS THE IMAGE
    BigPic.Y := RectHeight(VisibleImageRect);
    BigPic.X := round(BigPic.Y * iIEClientWidth/iIEClientHeight);
  end;

  case PanZoomEffect of

    iepzPanTopLeftToBottomRight, iepzPanBottomRightToTopLeft:
        begin
          StartRect := _GetSmallRect(_ipsTopLeft);
          EndRect   := _GetSmallRect(_ipsBottomRight);
        end;

    iepzPanTopRightToBottomLeft, iepzPanBottomLeftToTopRight:
        begin
          StartRect := _GetSmallRect(_ipsTopRight);
          EndRect   := _GetSmallRect(_ipsBottomLeft);
        end;

    iepzPanTopLeftToCenter, iepzPanCenterToTopLeft:
        begin
          StartRect := _GetSmallRect(_ipsTopLeft);
          EndRect   := _GetSmallRect(_ipsCenter);
        end;

    iepzPanTopRightToCenter, iepzPanCenterToTopRight:
        begin
          StartRect := _GetSmallRect(_ipsTopRight);
          EndRect   := _GetSmallRect(_ipsCenter);
        end;

    iepzPanBottomLeftToCenter, iepzPanCenterToBottomLeft:
        begin
          StartRect := _GetSmallRect(_ipsBottomLeft);
          EndRect   := _GetSmallRect(_ipsCenter);
        end;

    iepzPanBottomRightToCenter, iepzPanCenterToBottomRight:
        begin
          StartRect := _GetSmallRect(_ipsBottomRight);
          EndRect   := _GetSmallRect(_ipsCenter);
        end;

    iepzPanLeftToRightOrTopToBottom, iepzPanRightToLeftOrBottomToTop:
        begin
          // Output image has same dimensions as the display area?
          if bCroppedToAspectRatio and bIsPortraitImage then
          // Zoomed vertical Slide
          begin
            StartRect := _GetSmallRect(_ipsTop);
            EndRect   := _GetSmallRect(_ipsBottom);
          end
          ELSE
          if bCroppedToAspectRatio and (bIsPortraitImage = false) then
          // Zoomed horizontal Slide
          begin
            StartRect := _GetSmallRect(_ipsLeft);
            EndRect   := _GetSmallRect(_ipsRight);
          end
          ELSE
          if (RectHeight(VisibleImageRect) / iIEClientheight) > (RectWidth(VisibleImageRect) / iIEClientwidth) then
          // Vertical Slide
          begin
            StartRect := _GetBigRect(_ipsTop);
            EndRect   := _GetBigRect(_ipsBottom);
          end
          ELSE
          // Horizontal Slide
          begin
            StartRect := _GetBigRect(_ipsLeft);
            EndRect   := _GetBigRect(_ipsRight);
          end;
        end;

    iepzPanLeftToCenterOrTopToCenter, iepzPanCenterToLeftToOrCenterToTop:
        begin
          // Output image has same dimensions as the display area?
          if bCroppedToAspectRatio and bIsPortraitImage then
          // Zoomed vertical Slide
          begin
            StartRect := _GetSmallRect(_ipsTop);
            EndRect   := _GetSmallRect(_ipsCenter);
          end
          ELSE
          if bCroppedToAspectRatio and (bIsPortraitImage = false) then
          // Zoomed horizontal Slide
          begin
            StartRect := _GetSmallRect(_ipsLeft);
            EndRect   := _GetSmallRect(_ipsCenter);
          end
          ELSE
          if (RectHeight(VisibleImageRect) / iIEClientheight) > (RectWidth(VisibleImageRect) / iIEClientwidth) then
          // Vertical Slide
          begin
            StartRect := _GetBigRect(_ipsTop);
            EndRect   := _GetBigRect(_ipsCenter);
          end                                                             
          ELSE
          // Horizontal Slide
          begin
            StartRect := _GetBigRect(_ipsLeft);
            EndRect   := _GetBigRect(_ipsCenter);
          end;
        end;

    iepzPanRightToCenterOrBottomToCenter, iepzPanCenterToRightOrCenterToBottom:
        begin
          // Output image has same dimensions as the display area?
          if bCroppedToAspectRatio and bIsPortraitImage then
          // Zoomed vertical Slide
          begin
            StartRect := _GetSmallRect(_ipsBottom);
            EndRect   := _GetSmallRect(_ipsCenter);
          end
          ELSE
          if bCroppedToAspectRatio and (bIsPortraitImage = false) then
          // Zoomed horizontal Slide
          begin
            StartRect := _GetSmallRect(_ipsRight);
            EndRect   := _GetSmallRect(_ipsCenter);
          end
          ELSE
          if (RectHeight(VisibleImageRect) / iIEClientheight) > (RectWidth(VisibleImageRect) / iIEClientwidth) then
          // Vertical Slide
          begin
            StartRect := _GetBigRect(_ipsBottom);
            EndRect   := _GetBigRect(_ipsCenter);
          end
          ELSE
          // Horizontal Slide
          begin
            StartRect := _GetBigRect(_ipsRight);
            EndRect   := _GetBigRect(_ipsCenter);
          end;
        end;

    iepzZoomInToTopLeft, iepzZoomOutFromTopLeft:
        begin
          StartRect := _GetBigRect(_ipsTopLeft);
          EndRect   := _GetSmallRect(_ipsTopLeft);
        end;

    iepzZoomInToTopRight, iepzZoomOutFromTopRight:
        begin
          StartRect := _GetBigRect(_ipsTopRight);
          EndRect   := _GetSmallRect(_ipsTopRight);
        end;

    iepzZoomInToBottomLeft, iepzZoomOutFromBottomLeft:
        begin
          StartRect := _GetBigRect(_ipsBottomLeft);
          EndRect   := _GetSmallRect(_ipsBottomLeft);
        end;

    iepzZoomInToBottomRight, iepzZoomOutFromBottomRight:
        begin
          StartRect := _GetBigRect(_ipsBottomRight);
          EndRect   := _GetSmallRect(_ipsBottomRight);
        end;

    iepzZoomInToCenter, iepzZoomOutFromCenter:
        begin
          StartRect := _GetBigRect(_ipsCenter);
          EndRect   := _GetSmallRect(_ipsCenter);
        end;

    iepzFullZoomInToTopLeft, iepzFullZoomOutFromTopLeft:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsTopLeft);
        end;

    iepzFullZoomInToTop, iepzFullZoomOutFromTop:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsTop);
        end;

    iepzFullZoomInToTopRight, iepzFullZoomOutFromTopRight:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsTopRight);
        end;


    iepzFullZoomInToLeft, iepzFullZoomOutFromLeft:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsLeft);
        end;

    iepzFullZoomInToCenter, iepzFullZoomOutFromCenter:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsCenter);
        end;

    iepzFullZoomInToRight, iepzFullZoomOutFromRight:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsRight);
        end;


    iepzFullZoomInToBottomLeft, iepzFullZoomOutFromBottomLeft:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsBottomLeft);
        end;

    iepzFullZoomInToBottom, iepzFullZoomOutFromBottom:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsBottom);
        end;

    iepzFullZoomInToBottomRight, iepzFullZoomOutFromBottomRight:
        begin
          StartRect := _GetFullImageRect;
          EndRect   := _GetFullImageSmallRect(_ipsBottomRight);
        end;

  end;

  // REVERSE THE RECTS?
  if PanZoomEffect in [iepzPanBottomRightToTopLeft,
                       iepzPanBottomLeftToTopRight,
                       iepzPanCenterToTopLeft,
                       iepzPanCenterToTopRight,
                       iepzPanCenterToBottomLeft,
                       iepzPanCenterToBottomRight,
                       iepzPanRightToLeftOrBottomToTop,
                       iepzPanCenterToLeftToOrCenterToTop,
                       iepzPanCenterToRightOrCenterToBottom,
                       iepzZoomOutFromTopLeft,
                       iepzZoomOutFromTopRight,
                       iepzZoomOutFromBottomLeft,
                       iepzZoomOutFromBottomRight,
                       iepzZoomOutFromCenter,
                       iepzFullZoomOutFromTopLeft,
                       iepzFullZoomOutFromTop,
                       iepzFullZoomOutFromTopRight,
                       iepzFullZoomOutFromLeft,
                       iepzFullZoomOutFromCenter,
                       iepzFullZoomOutFromRight,
                       iepzFullZoomOutFromBottomLeft,
                       iepzFullZoomOutFromBottom,
                       iepzFullZoomOutFromBottomRight] then
  begin
    TempRect  := StartRect;
    StartRect := EndRect;
    EndRect   := TempRect;
  end; 
end;


// end of TIETransitionEffects
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////



end.


