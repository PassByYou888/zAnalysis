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
File version 1001
*)

unit iexThemes;

{$R-}
{$Q-}

{$I ie.inc}

{$ifdef IESUPPORTDEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$endif}

{$IFDEF SupportVclThemes}

interface

uses
  Vcl.Forms, Winapi.Messages, Vcl.Controls, Vcl.Themes, Vcl.Graphics, Winapi.Windows;

type
  TImageEnStyleHook = class(TScrollingStyleHook)
  strict private
    procedure UpdateColors;
 strict protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

implementation

uses
  ieview, iemview;

{ TImageEnStyleHook }

constructor TImageEnStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverridePaintNC := True;
  OverrideEraseBkgnd := True;
  UpdateColors;
end;

procedure TImageEnStyleHook.UpdateColors;
const
  ColorStates: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
  FontColorStates: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  TIEView(Control).fBackground := LStyle.GetStyleColor(ColorStates[Control.Enabled]);

  if Control is TImageEnMView then
  begin
    if seFont in Control.StyleElements then
    begin
      TImageEnMView(Control).DefaultTopTextFont   .Color := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled]);
      TImageEnMView(Control).DefaultInfoTextFont  .Color := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled]);
      TImageEnMView(Control).DefaultBottomTextFont.Color := LStyle.GetStyleFontColor(FontColorStates[Control.Enabled]);
    end;
    TImageEnMView(Control).ThumbnailsBackground         := LStyle.GetStyleColor(ColorStates[Control.Enabled]);
    TImageEnMView(Control).ThumbnailsBackgroundSelected := clHighlight;
    TImageEnMView(Control).SelectedFontColor            := clHighlightText;
  end;
end;

procedure TImageEnStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
end;

{$ELSE} // {$ifdef SupportVclThemes}

interface
implementation

{$ENDIF}

end.
