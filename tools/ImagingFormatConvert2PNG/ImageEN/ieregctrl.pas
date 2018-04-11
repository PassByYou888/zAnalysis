(*
File version 1000
*)

unit ieregctrl;

interface

{$I ie.inc}

procedure Register;

implementation

uses Classes,

  {$ifdef IEUSEDESIGNINTF}
  DesignIntf,
  {$endif}

  {$ifdef IEUSEFILTEDIT}
  FiltEdit,
  {$endif}

  {$ifdef IEUSEDSGNINTF}
  DsgnIntf,
  {$endif}

  ImageEn, HSVBox, ImageEnProc, ImageEnView, ImageEnIO, HistogramBox, RulerBox, IEGradientBar,
  VideoCap, IEVect, IEMView, Iemio, IEOpenSaveDlg, hvideocap, hyieutils, iexFolderMView;

procedure Register;
begin
  {$IFDEF IEINCLUDETIMAGEEN}
  RegisterComponents('ImageEn', [TImageEn]);
  {$ENDIF}
  RegisterComponents('ImageEn', [TImageEnProc]);
  RegisterComponents('ImageEn', [TImageEnView]);
  RegisterComponents('ImageEn', [TImageEnVect]);
  {$IFDEF IEINCLUDEVIDEOCAPTURE}
  RegisterComponents('ImageEn (Legacy)', [TImageEnVideoView]);
  RegisterComponents('ImageEn (Legacy)', [TImageEnVideoCap]);
  {$ENDIF}
  {$IFDEF IEINCLUDEMULTIVIEW}
  RegisterComponents('ImageEn', [TImageEnMIO]);
  RegisterComponents('ImageEn', [TImageEnMView]);  
  RegisterComponents('ImageEn', [TImageEnFolderMView]);
  {$ENDIF}
  RegisterComponents('ImageEn', [TImageEnIO]);
  RegisterComponents('ImageEn', [THistogramBox]);
  RegisterComponents('ImageEn', [TIEGradientBar]);
  RegisterComponents('ImageEn', [THSVBox]);
  RegisterComponents('ImageEn', [TOpenImageEnDialog]);
  RegisterComponents('ImageEn', [TSaveImageEnDialog]);
  RegisterComponents('ImageEn', [TRulerBox]);

  {$ifdef IEREGISTERPROPERTYEDITOR}
  RegisterPropertyEditor(TypeInfo(string), TOpenImageEnDialog, 'Filter', TFilterProperty);
  {$endif}
end;

end.
