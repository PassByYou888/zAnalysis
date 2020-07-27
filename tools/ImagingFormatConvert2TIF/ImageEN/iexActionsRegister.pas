{------------------------------------------------------------------------------}
{                                                                              }
{  TActions for common ImageEn functions                                       }
{                                                                              }
{  Nigel Cross                                                                 }
{  Xequte Software                                                             }
{  nigel@xequte.com                                                            }
{  http://www.xequte.com                                                       }
{                                                                              }
{  © Xequte Software 2011-2014                                                 }
{                                                                              }
{------------------------------------------------------------------------------}


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
File version 1003
*)


unit iexActionsRegister;      

{$I ie.inc}

{$IFDEF IEINCLUDEACTIONS}

interface

  Procedure Register;

implementation

uses
  iexActions, ActnList, iexActionsVect, iexActionsMulti, iexActionsFolder, iesettings
  {$IFDEF DelphiXE3orNewer}
  , System.Classes, System.Actions
  {$ENDIF}
  ;


procedure Register;
begin                          

  // IMAGEENVIEW ACTIONS

  RegisterActions('ImageEnView', [TImageEnViewAutoShrink           ,
                                  TImageEnViewAutoStretch          ,
                                  TImageEnViewBlank                ,
                                  TImageEnViewDeSelect             ,
                                  TImageEnViewFit                  ,
                                  TImageEnViewFitToHeight          ,
                                  TImageEnViewFitToWidth           ,
                                  TImageEnViewEnableAdjustOrientation ,
                                  TImageEnViewPlaying              ,
                                  TImageEnViewPlayLoop             ,
                                  TImageEnViewZoomFullSize         ,
                                  TImageEnViewSetZoom              ,
                                  TImageEnViewZoomIn               ,
                                  TImageEnViewZoomOut
                                  ], nil);

  RegisterActions('ImageEnView Layers', [TImageEnViewLayersAdd                  ,
                                         TImageEnViewLayersMergeAll             ,
                                         TImageEnViewLayersRemoveCurrent        ,
                                         TImageEnViewLayersCreateFromClipboard  ,
                                         {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
                                         TImageEnViewLayersCreateFromFile       ,
                                         {$ENDIF}
                                         TImageEnViewLayersMoveSendToBack       ,
                                         TImageEnViewLayersMoveSendBackward     ,
                                         TImageEnViewLayersMoveBringForward     ,
                                         TImageEnViewLayersMoveBringToFront     ,
                                         TImageEnViewLayersFixBorders
                                         ], nil);

  RegisterActions('ImageEnView Mouse', [TImageEnViewMouseMoveLayers      ,
                                        TImageEnViewMouseMovingScroll    ,
                                        TImageEnViewMouseResizeLayers    ,
                                        TImageEnViewMouseRotateLayers    ,
                                        TImageEnViewMouseScroll          ,
                                        TImageEnViewMouseSelect          ,
                                        TImageEnViewMouseSelectCircle    ,
                                        TImageEnViewMouseSelectLasso     ,
                                        TImageEnViewMouseSelectMagicWand ,
                                        TImageEnViewMouseSelectPolygon   ,
                                        TImageEnViewMouseSelectZoom      ,
                                        TImageEnViewMouseZoom
                                        ], nil);

  RegisterActions('ImageEnView Proc', [TImageEnViewConvertToGray     ,
                                       TImageEnViewCopyToClipboard   ,
                                       {$IFDEF IEINCLUDEDIALOGIP}
                                       TImageEnViewDoAdjustPreviews  ,
                                       TImageEnViewDoEffectPreviews  ,    
                                       TImageEnViewPromptToRotate    ,
                                       TImageEnViewPromptToResize    ,
                                       {$ENDIF}
                                       TImageEnViewFlipHorizontal    ,
                                       TImageEnViewFlipVertical      ,
                                       TImageEnViewHistAutoEqualize  ,
                                       TImageEnViewNegative          ,
                                       TImageEnViewPasteFromClipboard,
                                       TImageEnViewRedo              ,
                                       TImageEnViewRemoveRedEyes     ,
                                       TImageEnViewRotate180         ,
                                       TImageEnViewRotateLeft        ,
                                       TImageEnViewRotateRight       ,
                                       TImageEnViewCrop              ,
                                       TImageEnViewSelCopyToClip     ,
                                       TImageEnViewSelCutToClip      ,
                                       TImageEnViewSharpen           ,
                                       TImageEnViewUndo
                                       ], nil);

  RegisterActions('ImageEnView IO', [{$IFDEF IEINCLUDEIEXACQUIRE}
                                     TImageEnViewAcquire                    ,
                                     TImageEnViewSelectAcquireSource        ,
                                     {$ENDIF}
                                     {$IFDEF IEINCLUDEDIALOGIO}
                                     TImageEnViewDoIOPreviews               ,
                                     {$ENDIF}
                                     {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
                                     TImageEnViewPromptToOpen               ,
                                     TImageEnViewSave                       ,
                                     TImageEnViewPromptToSave               ,
                                     {$ENDIF}
                                     {$IFDEF IEINCLUDEPRINTDIALOGS}
                                     TImageEnViewDoPrintPreviewDialog       ,
                                     TImageEnViewPrintImageFitToPage        ,
                                     TImageEnViewPrintImageNormal           ,
                                     {$ENDIF}
                                     TImageEnViewSeekFirst                  ,
                                     TImageEnViewSeekLast                   ,
                                     TImageEnViewSeekNext                   ,
                                     TImageEnViewSeekPrior
                                     ], nil);



  // IMAGEENVECT ACTIONS
                                     
  RegisterActions('ImageEnVect', [TImageEnVectAutoShrink           ,
                                  TImageEnVectAutoStretch          ,
                                  TImageEnVectClear                ,
                                  TImageEnVectFit                  ,
                                  TImageEnVectFitToHeight          ,
                                  TImageEnVectFitToWidth           ,
                                  TImageEnVectZoomFullSize         ,    
                                  TImageEnVectSetZoom              ,
                                  TImageEnVectZoomIn               ,
                                  TImageEnVectZoomOut              ,
                                  TImageEnVectCopyToClipboard
                                  ], nil);

  RegisterActions('ImageEnVect Mouse', [TImageEnVectMouseMovingScroll           ,
                                        TImageEnVectMouseScroll                 ,
                                        TImageEnVectMouseSelect                 ,
                                        TImageEnVectMouseSelectCircle           ,
                                        TImageEnVectMouseSelectLasso            ,
                                        TImageEnVectMouseSelectMagicWand        ,
                                        TImageEnVectMouseSelectPolygon          ,
                                        TImageEnVectMouseSelectZoom             ,
                                        TImageEnVectMouseZoom                   ,
                                        TImageEnVectMouseVtDragLen              ,
                                        TImageEnVectMouseVtEditPolyline         ,
                                        TImageEnVectMouseVtLineLen              ,
                                        TImageEnVectMouseVtObjectSelect         ,
                                        TImageEnVectMouseVtPutAngle             ,
                                        TImageEnVectMouseVtPutBitmap            ,
                                        TImageEnVectMouseVtPutBox               ,
                                        TImageEnVectMouseVtPutEllipse           ,
                                        TImageEnVectMouseVtPutLine              ,
                                        TImageEnVectMouseVtPutLineLabel         ,
                                        TImageEnVectMouseVtPutMemo              ,
                                        TImageEnVectMouseVtPutPolyLine          ,
                                        TImageEnVectMouseVtPutRuler             ,
                                        TImageEnVectMouseVtPutText              ,
                                        TImageEnVectMouseVtUnStampMode          
                                        ], nil);

  {$IFDEF IEINCLUDEDIALOGIO}
  RegisterActions('ImageEnVect IO', [TImageEnVectDoIOPreviews], nil);
  {$ENDIF}

  {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
  RegisterActions('ImageEnVect IO', [TImageEnVectPromptToOpen               ,
                                     TImageEnVectPromptToSave
                                     ], nil);
  {$ENDIF}

  {$IFDEF IEINCLUDEPRINTDIALOGS}
  RegisterActions('ImageEnVect IO', [TImageEnVectDoPrintPreviewDialog       ,
                                     TImageEnVectPrintImageFitToPage        ,
                                     TImageEnVectPrintImageNormal
                                     ], nil);
  {$ENDIF}

  RegisterActions('ImageEnVect Objects', [TImageEnVectCropImageToObjects     ,
                                          TImageEnVectMergeAllToBackground   ,
                                          TImageEnVectMergeObjToBackground   ,
                                          TImageEnVectObjBringForward        ,
                                          TImageEnVectObjBringToFront        ,
                                          TImageEnVectObjCopyToClipboard     ,
                                          TImageEnVectObjCutToClipboard      ,
                                          TImageEnVectObjPasteFromClipboard  ,
                                          TImageEnVectObjSendBackward        ,
                                          TImageEnVectObjSendToBack          ,
                                          TImageEnVectObjUndo                ,
                                          TImageEnVectRemoveAllObjects       ,
                                          TImageEnVectRemoveObject           ,
                                          TImageEnVectRotateObject180        ,
                                          TImageEnVectRotateObjectLeft       ,
                                          TImageEnVectRotateObjectRight      ,
                                          TImageEnVectSelAllObjects          ,
                                          TImageEnVectUnSelAllObjects
                                          ], nil);


{$IFDEF IEINCLUDEMULTIVIEW}
  // MULTIVIEW ACTIONS

  RegisterActions('ImageEnMView', [TImageEnMViewClear                   ,
                                   TImageEnMViewDeleteImage             ,
                                   TImageEnMViewDeSelect                ,
                                   TImageEnMViewDisplayModeSingle       ,
                                   TImageEnMViewEnableAdjustOrientation ,
                                   TImageEnMViewPlaying                 ,
                                   TImageEnMViewPlayLoop                ,
                                   TImageEnMViewSelectAll               ,
                                   TImageEnMViewSeekFirst               ,
                                   TImageEnMViewSeekLast                ,
                                   TImageEnMViewSeekNext                ,
                                   TImageEnMViewSeekPrior
                                   ], nil);

  RegisterActions('ImageEnMView Proc', [TImageEnMViewCopyToClipboard        ,
                                        TImageEnMViewCutToClipboard         ,
                                        {$IFDEF IEINCLUDEDIALOGIP}
                                        TImageEnMViewDoAdjustPreviews       ,
                                        TImageEnMViewDoEffectPreviews       ,
                                        TImageEnMViewPromptToRotate         ,
                                        TImageEnMViewPromptToResize         ,
                                        {$ENDIF}
                                        TImageEnMViewFlipHorizontal         ,
                                        TImageEnMViewFlipVertical           ,
                                        TImageEnMViewPasteFromClipboard     ,
                                        TImageEnMViewRotate180              ,
                                        TImageEnMViewRotateLeft             ,
                                        TImageEnMViewRotateRight
                                        ], nil);

  {$IFDEF IEINCLUDEIEXACQUIRE}
  RegisterActions('ImageEnMView IO', [TImageEnMViewAcquire                    ,
                                      TImageEnMViewSelectAcquireSource
                                      ], nil);
  {$ENDIF}

  {$IFDEF IEINCLUDEDIALOGIO}
  RegisterActions('ImageEnMView IO', [TImageEnMViewDoIOPreviews               ,
                                      TImageEnMViewDoIOPreviewsSelected
                                      ], nil);
  {$ENDIF}

  {$IFDEF IEINCLUDEOPENSAVEDIALOGS}
  RegisterActions('ImageEnMView IO', [TImageEnMViewPromptToAdd                ,
                                      TImageEnMViewPromptToOpen               ,
                                      TImageEnMViewPromptToSave               ,
                                      TImageEnMViewSave
                                      ], nil);
  {$ENDIF}

  {$IFDEF IEINCLUDEPRINTDIALOGS}
  RegisterActions('ImageEnMView IO', [TImageEnMViewDoPrintPreviewDialog       ,
                                      TImageEnMViewPrintAllThumbnails         ,
                                      TImageEnMViewPrintImageFitToPage        ,
                                      TImageEnMViewPrintImageNormal           ,
                                      TImageEnMViewPrintSelectedThumbnails
                                      ], nil);
  {$ENDIF}

  RegisterActions('ImageEnFolderMView', [TIEFolderMViewPromptForFolder               ,
                                         TIEFolderMViewOpenParentFolder              ,
                                         TIEFolderMViewRefreshFileList               ,  
                                         TIEFolderMViewCreateNewFolder               ,
                                         TIEFolderMViewExecuteSelectedFile           ,
                                         TIEFolderMViewMoveSelectedFilesToFolder     ,
                                         TIEFolderMViewCopySelectedFilesToFolder     ,
                                         TIEFolderMViewDeleteSelectedFilesFromFolder ,
                                         TIEFolderMViewCopySelectedFilesToClipboard  , 
                                         TIEFolderMViewCutSelectedFilesToClipboard   ,
                                         TIEFolderMViewPasteFilesFromClipboard       ,
                                         TIEFolderMViewRenameSelectedFile            ,
                                         TIEFolderMViewPromptForFolder               ,
                                         TIEFolderMViewCreateNewFolder
                                         ], nil);                                    

{$ENDIF}   // {$IFDEF IEINCLUDEMULTIVIEW}}

end;


{$ELSE} // {$IFDEF IEINCLUDEACTIONS}}

interface
implementation

{$ENDIF}

end.
