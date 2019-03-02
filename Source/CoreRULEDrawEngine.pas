{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit CoreRULEDrawEngine;

{$INCLUDE zDefine.inc}

interface

uses Geometry2DUnit, CoreClasses, Geometry3DUnit, UnicodeMixedLib,
  zDrawEngine,
  CoreRULE,
  zNavigationScene, zNavigationPass, zNavigationPoly, zNavigationPathFinding,
  MovementEngine;

const
  NavSceneEditor_PointDrawRadius: TDEFloat = 10.0;

type
  TCoreRULEDrawEngine = class;

  TCoreRULEDrawOption = (crdoNavSceneEditor);
  TCoreRULEDrawOptions = set of TCoreRULEDrawOption;

  TNavSceneEditorMode = (nsemScene, nsemPoly, nsemNone);

  TNavSceneEditorPickStyle = (
    nsepsPoly, nsepsPolyIndex,
    nsepsLocationCenter, nsepsLocationEndge,
    nsepsNone);

  TCoreRULEPickStyle = (crpsBIO, crpsNone);

  ICoreRULEDrawEngineNotifyInterface = interface
    procedure BeginSceneEdit(Sender: TCoreRULEDrawEngine);
    procedure BeginPolyEdit(Sender: TCoreRULEDrawEngine);
    procedure DoneEdit(Sender: TCoreRULEDrawEngine);
    procedure PolygonChange(Sender: TCoreRULEDrawEngine; Poly: TPolyManagerChildren);
    procedure LocationChange(Sender: TCoreRULEDrawEngine; Location: TLocation);
  end;

  TCoreRULEDrawEngine = class(TDrawEngine)
  protected
    FRegion: TRegion;
    FCoreRULEDrawOptions: TCoreRULEDrawOptions;
    FNotifyInterface: ICoreRULEDrawEngineNotifyInterface;

    procedure DoFlush; override;
  protected
    // nav scene editor ui
    Done_btn: TDrawEngine_RectButton;
    DrawScene_btn: TDrawEngine_RectButton;
    DrawPoly_btn: TDrawEngine_RectButton;

    NavSceneEditorMode: TNavSceneEditorMode;

    NavScene_PickStyle: TNavSceneEditorPickStyle;
    NavScene_PickPoly: TPolyManagerChildren;
    NavScene_PickPolyIndex: Integer;
    NavScene_PickLocation: TLocation;

    NavScene_CurrentDrawPoly: TVec2List;

    FScenePolyDrawOption: TPolyDrawOption;
    FPolyDrawOption: TPolyDrawOption;
    FPointDrawOption: TPolyDrawOption;
    FSelPolyDrawOption: TPolyDrawOption;

    procedure InitNavSceneEditorUI;
    procedure FreeNavSceneEditorUI;
    procedure PrepareNavSceneEditorUI;

    procedure DrawNavScene(r: TBattleRegion);
    procedure DrawLocation(r: TBattleRegion);
    procedure DrawNavSceneEditor;

    function DoNavSceneEditor_TapDown(x, y: TDEFloat): Boolean;
    function DoNavSceneEditor_TapMove(x, y: TDEFloat): Boolean;
    function DoNavSceneEditor_TapUp(x, y: TDEFloat): Boolean;

    procedure Done_Click(Sender: TDrawEngine_UIBase);
    procedure DrawScene_Click(Sender: TDrawEngine_UIBase);
    procedure DrawPoly_Click(Sender: TDrawEngine_UIBase);
  protected
    function DoTapDown(x, y: TDEFloat): Boolean; override;
    function DoTapMove(x, y: TDEFloat): Boolean; override;
    function DoTapUp(x, y: TDEFloat): Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Region: TRegion read FRegion write FRegion;
    property CoreRULEDrawOptions: TCoreRULEDrawOptions read FCoreRULEDrawOptions write FCoreRULEDrawOptions;
    property NotifyInterface: ICoreRULEDrawEngineNotifyInterface read FNotifyInterface write FNotifyInterface;

    procedure Progress(deltaTime: Double); override;

    procedure DrawMap(r: TMapRegion);
    procedure DrawBackground(r: TBattleRegion);
    procedure DrawBioBefore(r: TBattleRegion);
    procedure DrawBio(r: TBattleRegion);
    procedure DrawBioAfter(r: TBattleRegion);
  end;

implementation

procedure TCoreRULEDrawEngine.DoFlush;
begin
  if FRegion is TMapRegion then
    begin
      DrawMap(FRegion as TMapRegion);
    end
  else if FRegion is TBattleRegion then
    begin
      DrawBackground(FRegion as TBattleRegion);

      if (crdoNavSceneEditor in CoreRULEDrawOptions) then
        begin
          DrawNavScene(FRegion as TBattleRegion);
          DrawLocation(FRegion as TBattleRegion);
        end;

      DrawBioBefore(FRegion as TBattleRegion);
      DrawBio(FRegion as TBattleRegion);
      DrawBioAfter(FRegion as TBattleRegion);
    end;

  DrawNavSceneEditor;
end;

procedure TCoreRULEDrawEngine.InitNavSceneEditorUI;
begin
  Done_btn := TDrawEngine_RectButton.Create(Self);
  Done_btn.Visibled := False;

  DrawScene_btn := TDrawEngine_RectButton.Create(Self);
  DrawScene_btn.Visibled := False;

  DrawPoly_btn := TDrawEngine_RectButton.Create(Self);;
  DrawPoly_btn.Visibled := False;

{$IFDEF FPC}
  Done_btn.OnClick := @Done_Click;
  DrawScene_btn.OnClick := @DrawScene_Click;
  DrawPoly_btn.OnClick := @DrawPoly_Click;
{$ELSE}
  Done_btn.OnClick := Done_Click;
  DrawScene_btn.OnClick := DrawScene_Click;
  DrawPoly_btn.OnClick := DrawPoly_Click;
{$ENDIF}
  NavSceneEditorMode := nsemNone;

  NavScene_PickStyle := nsepsNone;
  NavScene_PickPoly := nil;
  NavScene_PickPolyIndex := -1;
  NavScene_PickLocation := nil;

  NavScene_CurrentDrawPoly := TVec2List.Create;

  with FScenePolyDrawOption do
    begin
      LineColor := DEColor(0.5, 0.8, 0.5, 0.9);
      PointColor := DEColor(0.5, 0.8, 0.5, 0.9);
      LineWidth := 1;
      PointScreenRadius := NavSceneEditor_PointDrawRadius;
    end;

  with FPolyDrawOption do
    begin
      LineColor := DEColor(0.8, 0.5, 0.5, 0.9);
      PointColor := DEColor(0.8, 0.5, 0.5, 0.9);
      LineWidth := 1;
      PointScreenRadius := NavSceneEditor_PointDrawRadius;
    end;

  with FPointDrawOption do
    begin
      LineColor := DEColor(0.5, 0.8, 0.5, 0.9);
      PointColor := DEColor(0.5, 0.8, 0.5, 0.9);
      LineWidth := 1;
      PointScreenRadius := NavSceneEditor_PointDrawRadius;
    end;

  with FSelPolyDrawOption do
    begin
      LineColor := DEColor(1, 1, 1, 0.9);
      PointColor := DEColor(1, 1, 1, 0.9);
      LineWidth := 3;
      PointScreenRadius := NavSceneEditor_PointDrawRadius;
    end;
end;

procedure TCoreRULEDrawEngine.FreeNavSceneEditorUI;
begin
  DisposeObject(NavScene_CurrentDrawPoly);
end;

procedure TCoreRULEDrawEngine.PrepareNavSceneEditorUI;
begin
  if (crdoNavSceneEditor in CoreRULEDrawOptions) and (Region is TBattleRegion) then
    begin
      if NavSceneEditorMode = nsemNone then
        begin
          DrawScene_btn.Button := MakeRect(width - 60, 5, width - 5, 40);
          DrawScene_btn.Text := 'Draw Scene';
          DrawScene_btn.Visibled := True;

          DrawPoly_btn.Button := MakeRect(width - 60, 45, width - 5, 80);
          DrawPoly_btn.Text := 'Draw Poly';
          DrawPoly_btn.Visibled := True;
        end
      else
        begin
          Done_btn.Button := MakeRect(width - 60, 5, width - 5, 40);
          Done_btn.Text := 'Done';
          Done_btn.Visibled := True;
        end;
    end;
end;

procedure TCoreRULEDrawEngine.DrawNavScene(r: TBattleRegion);
var
  i, j: Integer;
  dopt: TPolyDrawOption;
  CM: TPolyPassManager;
  p: TBasePass;
  c: TDEColor;
begin
  if (NavScene_PickStyle = nsepsPoly) and (NavScene_PickPoly = r.NavScene.PolyManager.Scene) then
      dopt := FSelPolyDrawOption
  else
      dopt := FScenePolyDrawOption;

  DrawPolyInScene(r.NavScene.PolyManager.Scene, True, dopt);

  for i := 0 to r.NavScene.PolyManager.Count - 1 do
    begin
      if (NavScene_PickStyle = nsepsPoly) and (NavScene_PickPoly = r.NavScene.PolyManager[i]) then
          dopt := FSelPolyDrawOption
      else
          dopt := FPolyDrawOption;

      DrawPolyInScene(r.NavScene.PolyManager[i], True, dopt);
    end;

  DrawCommand.SetLineWidth(1);
  c := DEColor(0.2, 0.5, 0.2, 0.3);
  CM := r.NavScene.PassManager[10];
  if CM.Count = 0 then
      CM.Rebuild;
  for i := 0 to CM.Count - 1 do
    begin
      p := CM[i];
      for j := 0 to p.Count - 1 do
        if (p[j]^.Enabled) then
            DrawCommand.DrawLine(p.Position, p[j]^.passed.Position, c);
    end;
end;

procedure TCoreRULEDrawEngine.DrawLocation(r: TBattleRegion);
var
  i: Integer;
  L: TLocation;
  t1: TDEVec;
  r2: TDERect;
  c: TDEColor;
begin
  c := DEColor(0.3, 0, 0, 0.6);
  DrawCommand.SetLineWidth(NavSceneEditor_PointDrawRadius * 2);
  for i := 0 to r.LocationCount - 1 do
    begin
      L := r.Location[i];
      t1 := L.Position;

      r2[0][0] := t1[0] - L.radius;
      r2[0][1] := t1[1] - L.radius;
      r2[1][0] := t1[0] + L.radius;
      r2[1][1] := t1[1] + L.radius;

      DrawCommand.DrawEllipse(r2, c);
    end;

  DrawCommand.SetLineWidth(1);
  for i := 0 to r.LocationCount - 1 do
    begin
      L := r.Location[i];
      t1 := L.Position;

      r2[0][0] := t1[0] - NavSceneEditor_PointDrawRadius;
      r2[0][1] := t1[1] - NavSceneEditor_PointDrawRadius;
      r2[1][0] := t1[0] + NavSceneEditor_PointDrawRadius;
      r2[1][1] := t1[1] + NavSceneEditor_PointDrawRadius;

      DrawCommand.FillEllipse(r2, c);
    end;
end;

procedure TCoreRULEDrawEngine.DrawNavSceneEditor;
begin
  AllUINoVisibled;
  PrepareNavSceneEditorUI;

  if (crdoNavSceneEditor in CoreRULEDrawOptions) and (Region is TBattleRegion) then
    case NavSceneEditorMode of
      nsemScene, nsemPoly:
        DrawPLInScene(NavScene_CurrentDrawPoly, False, FPointDrawOption);
    end;
end;

function TCoreRULEDrawEngine.DoNavSceneEditor_TapDown(x, y: TDEFloat): Boolean;
var
  i, j: Integer;
  ScenePt: TDEVec;
  r: TBattleRegion;
  L: TLocation;
  d: Double;
  Poly: TPolyManagerChildren;
  NavScene: TNavigationScene;
begin
  Result := False;
  if (crdoNavSceneEditor in CoreRULEDrawOptions) and (Region is TBattleRegion) then
    begin
      r := Region as TBattleRegion;
      NavScene := r.NavScene;
      ScenePt := ScreenToScene(x, y);
      case NavSceneEditorMode of
        nsemScene, nsemPoly:
          begin
          end;
        nsemNone:
          begin
            NavScene_PickStyle := nsepsNone;
            NavScene_PickPoly := nil;
            NavScene_PickPolyIndex := -1;
            NavScene_PickLocation := nil;

            for i := 0 to r.LocationCount - 1 do
              begin
                L := r.Location[i];
                if PointInCircle(ScenePt, L.Position, NavSceneEditor_PointDrawRadius * Scale) then
                  begin
                    NavScene_PickStyle := nsepsLocationCenter;
                    NavScene_PickLocation := L;
                    Exit;
                  end;

                d := PointDistance(ScenePt, L.Position);
                if (d >= L.radius - NavSceneEditor_PointDrawRadius) and (d <= L.radius + NavSceneEditor_PointDrawRadius) then
                  begin
                    NavScene_PickStyle := nsepsLocationEndge;
                    NavScene_PickLocation := L;
                    Exit;
                  end;
              end;

            if PointInCircle(ScenePt, NavScene.PolyManager.Scene.Position, FScenePolyDrawOption.PointScreenRadius * Scale) then
              begin
                NavScene_PickStyle := nsepsPoly;
                NavScene_PickPoly := NavScene.PolyManager.Scene;
                Exit;
              end;

            for i := 0 to NavScene.PolyManager.Count - 1 do
              begin
                if PointInCircle(ScenePt, NavScene.PolyManager[i].Position, FScenePolyDrawOption.PointScreenRadius * Scale) then
                  begin
                    NavScene_PickStyle := nsepsPoly;
                    NavScene_PickPoly := NavScene.PolyManager[i];
                    Exit;
                  end;
              end;

            for i := 0 to NavScene.PolyManager.Count - 1 do
              begin
                Poly := NavScene.PolyManager[i];
                for j := 0 to Poly.Count - 1 do
                  begin
                    if PointInCircle(ScenePt, Poly.Points[j], FPolyDrawOption.PointScreenRadius * Scale) then
                      begin
                        NavScene_PickStyle := nsepsPolyIndex;
                        NavScene_PickPoly := Poly;
                        NavScene_PickPolyIndex := j;
                        Exit;
                      end;
                  end;
              end;

            Poly := NavScene.PolyManager.Scene;
            for j := 0 to Poly.Count - 1 do
              begin
                if PointInCircle(ScenePt, Poly.Points[j], FPolyDrawOption.PointScreenRadius * Scale) then
                  begin
                    NavScene_PickStyle := nsepsPolyIndex;
                    NavScene_PickPoly := Poly;
                    NavScene_PickPolyIndex := j;
                    Exit;
                  end;
              end;
          end;
      end;
    end;
end;

function TCoreRULEDrawEngine.DoNavSceneEditor_TapMove(x, y: TDEFloat): Boolean;
var
  NavScene: TNavigationScene;
  ScenePt: TDEVec;
begin
  Result := False;
  if (crdoNavSceneEditor in CoreRULEDrawOptions) and (Region is TBattleRegion) then
    begin
      NavScene := TBattleRegion(Region).NavScene;
      ScenePt := ScreenToScene(x, y);
      case NavSceneEditorMode of
        nsemScene, nsemPoly:
          begin
          end;
        nsemNone:
          begin
            case NavScene_PickStyle of
              nsepsPoly: NavScene_PickPoly.Position := ScenePt;
              nsepsPolyIndex: NavScene_PickPoly.Points[NavScene_PickPolyIndex] := ScenePt;
              nsepsLocationCenter:
                begin
                  NavScene_PickLocation.Position := ScenePt;
                  if FNotifyInterface <> nil then
                      FNotifyInterface.LocationChange(Self, NavScene_PickLocation);
                end;
              nsepsLocationEndge:
                begin
                  NavScene_PickLocation.radius := PointDistance(ScenePt, NavScene_PickLocation.Position);
                  if FNotifyInterface <> nil then
                      FNotifyInterface.LocationChange(Self, NavScene_PickLocation);
                end;
            end;
          end;
      end;
    end;
end;

function TCoreRULEDrawEngine.DoNavSceneEditor_TapUp(x, y: TDEFloat): Boolean;
var
  NavScene: TNavigationScene;
  ScenePt: TDEVec;
begin
  Result := False;
  if (crdoNavSceneEditor in CoreRULEDrawOptions) and (Region is TBattleRegion) then
    begin
      NavScene := TBattleRegion(Region).NavScene;
      ScenePt := ScreenToScene(x, y);
      case NavSceneEditorMode of
        nsemScene, nsemPoly: NavScene_CurrentDrawPoly.Add(ScenePt);
        nsemNone:
          begin
            case NavScene_PickStyle of
              nsepsPoly, nsepsPolyIndex:
                begin
                  if FNotifyInterface <> nil then
                      FNotifyInterface.PolygonChange(Self, NavScene_PickPoly);

                  NavScene.RebuildCoordinate;
                  NavScene.RebuildPass;
                  NavScene.ResetCollisionState;
                end;
            end;
            NavScene_PickStyle := nsepsNone;
            NavScene_PickPoly := nil;
            NavScene_PickPolyIndex := -1;
            NavScene_PickLocation := nil;
          end;
      end;
    end;
end;

procedure TCoreRULEDrawEngine.Done_Click(Sender: TDrawEngine_UIBase);
var
  NavScene: TNavigationScene;
begin
  if (Region is TBattleRegion) then
    begin
      NavScene := TBattleRegion(Region).NavScene;
      case NavSceneEditorMode of
        nsemScene:
          begin
            NavScene.SetScene(NavScene_CurrentDrawPoly);
            NavScene.RebuildPass;
            NavScene.ResetCollisionState;
          end;
        nsemPoly:
          begin
            NavScene.AddPolygon(NavScene_CurrentDrawPoly, False);
            NavScene.RebuildPass;
            NavScene.ResetCollisionState;
          end;
      end;
    end;
  NavScene_CurrentDrawPoly.Clear;
  NavSceneEditorMode := nsemNone;

  if FNotifyInterface <> nil then
      FNotifyInterface.DoneEdit(Self);
end;

procedure TCoreRULEDrawEngine.DrawScene_Click(Sender: TDrawEngine_UIBase);
begin
  NavSceneEditorMode := nsemScene;
  if FNotifyInterface <> nil then
      FNotifyInterface.BeginSceneEdit(Self);
end;

procedure TCoreRULEDrawEngine.DrawPoly_Click(Sender: TDrawEngine_UIBase);
begin
  NavSceneEditorMode := nsemPoly;
  if FNotifyInterface <> nil then
      FNotifyInterface.BeginPolyEdit(Self);
end;

function TCoreRULEDrawEngine.DoTapDown(x, y: TDEFloat): Boolean;
begin
  Result := inherited DoTapDown(x, y);
  if Result then
      Exit;
  Result := DoNavSceneEditor_TapDown(x, y);
  if Result then
      Exit;

end;

function TCoreRULEDrawEngine.DoTapMove(x, y: TDEFloat): Boolean;
begin
  Result := inherited DoTapMove(x, y);
  if Result then
      Exit;
  Result := DoNavSceneEditor_TapMove(x, y);
end;

function TCoreRULEDrawEngine.DoTapUp(x, y: TDEFloat): Boolean;
begin
  Result := inherited DoTapUp(x, y);
  if Result then
      Exit;
  Result := DoNavSceneEditor_TapUp(x, y);
end;

constructor TCoreRULEDrawEngine.Create;
begin
  inherited Create;

  FRegion := nil;
  FCoreRULEDrawOptions := [crdoNavSceneEditor];
  FNotifyInterface := nil;

  InitNavSceneEditorUI;
end;

destructor TCoreRULEDrawEngine.Destroy;
begin
  FreeNavSceneEditorUI;
  inherited Destroy;
end;

procedure TCoreRULEDrawEngine.Progress(deltaTime: Double);
begin
  inherited Progress(deltaTime);
end;

procedure TCoreRULEDrawEngine.DrawMap(r: TMapRegion);
begin
end;

procedure TCoreRULEDrawEngine.DrawBackground(r: TBattleRegion);
begin

end;

procedure TCoreRULEDrawEngine.DrawBioBefore(r: TBattleRegion);
begin

end;

procedure TCoreRULEDrawEngine.DrawBio(r: TBattleRegion);
begin

end;

procedure TCoreRULEDrawEngine.DrawBioAfter(r: TBattleRegion);
var
  i: Integer;
  b: TBio;
  t: TDEVec;
  r2: TDERect;
  c: TDEColor;
begin
  // debug time
  DrawCommand.SetLineWidth(1);
  for i := 0 to r.BioCount - 1 do
    begin
      b := r.Bios[i];
      t := SceneToScreen(b.Position);
      r2[0][0] := t[0] - b.radius * Scale;
      r2[0][1] := t[1] - b.radius * Scale;
      r2[1][0] := t[0] + b.radius * Scale;
      r2[1][1] := t[1] + b.radius * Scale;

      c := DEColor(0.9, 0.5, 0.3, 0.9);
      DrawCommand.DrawEllipse(r2, c);
      c := DEColor(0.5, 0.5, 0.9, 0.9);
      DrawCommand.DrawLine(b.Position, PointRotation(b.Position, b.radius * 2 * Scale, FinalAngle4FMX(b.RollAngle)), c);
    end;
end;

end.
