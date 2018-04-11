(*
File version 1000
*)

unit ieregdb;

{$R-}
{$Q-}

interface

{$I ie.inc}

procedure Register;

implementation

uses Classes, DBImageEn, dbimageenvect
     {$ifdef IEREGISTERQR}
     , qrimageen, qrdbimageen
     {$endif}
     ;

procedure Register;
begin
  {$IFDEF IEINCLUDEDB}
  RegisterComponents('ImageEn', [TImageEnDBView]);
  RegisterComponents('ImageEn', [TImageEnDBVect]);
  {$ifdef IEREGISTERQR}
  RegisterComponents('ImageEn', [TQRDBImageEn]);
  RegisterComponents('ImageEn', [TQRImageEn]);
  {$endif}
  {$ENDIF}
end;

end.
