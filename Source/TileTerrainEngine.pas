{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit TileTerrainEngine;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Types, Math, Classes,

  LibraryManager, StreamList, zDrawEngine, MemoryRaster,
  DataFrameEngine, UnicodeMixedLib, CoreClasses,
  ObjectDataManager, Geometry2DUnit, ListEngine, PascalStrings;

type
  TTileDataRec = record
    Tile: U_String;
    x, y: Integer;
  end;

  PTileDataRec = ^TTileDataRec;

  TTileBuffers = array of array of PTileDataRec;
  PTileBuffers = ^TTileBuffers;

const
  _Texture_AllFullClient       = '~FullClient_*.bmp';
  _Texture_AllFullClientFormat = '~FullClient_%d.bmp';

  _Texture_FullClient_1  = '~FullClient_1.bmp';
  _Texture_FullClient_2  = '~FullClient_2.bmp';
  _Texture_FullClient_3  = '~FullClient_3.bmp';
  _Texture_FullClient_4  = '~FullClient_4.bmp';
  _Texture_FullClient_5  = '~FullClient_5.bmp';
  _Texture_FullClient_6  = '~FullClient_6.bmp';
  _Texture_FullClient_7  = '~FullClient_7.bmp';
  _Texture_FullClient_8  = '~FullClient_8.bmp';
  _Texture_FullClient_9  = '~FullClient_9.bmp';
  _Texture_FullClient_10 = '~FullClient_10.bmp';
  _Texture_FullClient_11 = '~FullClient_11.bmp';
  _Texture_FullClient_12 = '~FullClient_12.bmp';
  _Texture_FullClient_13 = '~FullClient_13.bmp';
  _Texture_FullClient_14 = '~FullClient_14.bmp';
  _Texture_FullClient_15 = '~FullClient_15.bmp';
  _Texture_FullClient_16 = '~FullClient_16.bmp';
  _Texture_FullClient_17 = '~FullClient_17.bmp';
  _Texture_FullClient_18 = '~FullClient_18.bmp';

  _Texture_Angle_RightBottom             = '~Angle_RightBottom.bmp';
  _Texture_Angle_LeftBottom              = '~Angle_LeftBottom.bmp';
  _Texture_Angle_RightTop                = '~Angle_RightTop.bmp';
  _Texture_Angle_LeftTop                 = '~Angle_LeftTop.bmp';
  _Texture_EndgeBottom_AngleRightLeft    = '~EndgeBottom_AngleRightLeft.bmp';
  _Texture_EndgeTop_AngleRightLeft       = '~EndgeTop_AngleRightLeft.bmp';
  _Texture_EndgeLeft_AngleTopBottom      = '~EndgeLeft_AngleTopBottom.bmp';
  _Texture_EndgeRight_AngleTopBottom     = '~EndgeRight_AngleTopBottom.bmp';
  _Texture_EndgeLeftBottom_AngleRightTop = '~EndgeLeftBottom_AngleRightTop.bmp';
  _Texture_EndgeRightBottom_AngleLeftTop = '~EndgeRightBottom_AngleLeftTop.bmp';
  _Texture_EndgeLeftTop_AngleRightBottom = '~EndgeLeftTop_AngleRightBottom.bmp';
  _Texture_EndgeRightTop_AngleLeftBottom = '~EndgeRightTop_AngleLeftBottom.bmp';
  _Texture_AngleLeftTop_AngleRightBottom = '~AngleLeftTop_AngleRightBottom.bmp';
  _Texture_AngleLeftBottom_AngleRightTop = '~AngleLeftBottom_AngleRightTop.bmp';

  _MultiTextrueNameSplitText = '|;%'#13#10#0;

function AutoChangeTexture(var _1T: U_String; const _2T: U_String): Boolean;
function Change2Texture(var _1T: U_String; const _2T: U_String): Boolean;
function ChangeAllTexture(var _1T: U_String; const _2T: U_String): Boolean;
function Compare2Texture(_TexName, _NewName: U_String): Boolean;
function Get2TextureGraphics(_Lib: TLibraryManager; _TexName: U_String; _TG: TCoreClassListForObj): Boolean;
function Get2TextureGraphicsAsList(_TexName: U_String; _AsList: TCoreClassStrings): Boolean;
function GetFullClient2(_Lib: TLibraryManager; _TTPrefix: U_String): U_String;
function IsAllFullClient(_TexName, _NewTTPrefix: U_String): Boolean;
function ProcessTerrainTexture(_Lib: TLibraryManager; _TileInfo: U_String; _GR: TMemoryRaster; _X, _Y: Integer): Boolean;
function TextureCount(_TexName, _TexPrefix: U_String): Integer;

function UpdateBottomTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
function UpdateLeftBottomTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
function UpdateLeftTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
function UpdateLeftTopTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
function UpdateRightBottomTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
function UpdateRightTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
function UpdateRightTopTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
function UpdateTopTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;

procedure Update2Texture(_Lib: TLibraryManager; _NewTTPrefix: U_String; var LeftTop, RightTop, LeftBottom, RightBottom: TTileDataRec);
function Update2TerrainTexture(msPtr: PTileBuffers; _Lib: TLibraryManager; _NewTTPrefix: U_String; _Point: TPoint; _Width, _Height: Integer): Boolean;

procedure InternalInitMap(msPtr: PTileBuffers; _MapWidth, _MapHeight: Integer);
procedure GenerateDefaultFullClientTexture(msPtr: PTileBuffers; _DefaultTexturePrefix: U_String);
procedure FreeTileMap(msPtr: PTileBuffers);

procedure PrepareTileCache(_Lib: TLibraryManager; const LibName: SystemString);

procedure LoadTileMapFromStream(msPtr: PTileBuffers; var width, height: Integer; var LibName: SystemString; stream: TCoreClassStream);
procedure SaveTileMapToStream(msPtr: PTileBuffers; const width, height: Integer; const LibName: SystemString; stream: TCoreClassStream);
procedure BuildTileMapAsBitmap(msPtr: PTileBuffers; const width, height: Integer; output: TMemoryRaster); overload;
procedure BuildTileMapAsBitmap(stream: TCoreClassStream; output: TMemoryRaster); overload;
procedure BuildTileMapAsBitmap(MapFile: SystemString; output: TMemoryRaster); overload;
procedure BuildTileMapAsBitmap(MapFile: SystemString; output: TMemoryRaster; MaxSize: Integer); overload;

var
  TileTerrainDefaultBitmapClass: TMemoryRasterClass;

implementation

uses MediaCenter;

function AutoChangeTexture(var _1T: U_String; const _2T: U_String): Boolean;
begin
  if umlMultipleMatch(True, '*' + _Texture_AllFullClient, _2T) then
      Result := ChangeAllTexture(_1T, _2T)
  else
      Result := Change2Texture(_1T, _2T);
end;

function Change2Texture(var _1T: U_String; const _2T: U_String): Boolean;
var
  _TextString, _Prefix: U_String;
begin
  _TextString := _1T;
  _1T := umlGetFirstStr(_TextString, _MultiTextrueNameSplitText);
  _Prefix := '~' + umlGetFirstStr(_2T, '~');
  while umlExistsChar(_TextString, _MultiTextrueNameSplitText) do
    begin
      _TextString := umlDeleteFirstStr(_TextString, _MultiTextrueNameSplitText);
      if not umlMultipleMatch(True, _Prefix + '~*', umlGetFirstStr(_TextString, _MultiTextrueNameSplitText)) then
          _1T := _1T + '|' + umlGetFirstStr(_TextString, _MultiTextrueNameSplitText);
    end;

  if not umlExistsChar(_TextString, _MultiTextrueNameSplitText) then
    begin
      if not umlMultipleMatch(True, umlGetFirstStr(_1T, '~'), _Prefix) then
          _1T := _1T + '|' + _2T;
    end
  else
      _1T := _1T + '|' + _2T;
  Result := True;
end;

function ChangeAllTexture(var _1T: U_String; const _2T: U_String): Boolean;
begin
  _1T := _2T;
  Result := True;
end;

function Compare2Texture(_TexName, _NewName: U_String): Boolean;
begin
  if umlExistsChar(_TexName, _MultiTextrueNameSplitText) then
      Result := umlMultipleMatch(True, _NewName,
      umlGetLastStr(_TexName, _MultiTextrueNameSplitText))
  else
      Result := False;
end;

function Get2TextureGraphics(_Lib: TLibraryManager; _TexName: U_String; _TG: TCoreClassListForObj): Boolean;
var
  _StreamListData: PHashStreamListData;
  _GR: TMemoryRaster;
  _N, _T: U_String;
begin
  _N := _TexName;

  if umlExistsChar(_N, _MultiTextrueNameSplitText) then
    begin
      repeat
        _T := umlGetFirstStr(_N, _MultiTextrueNameSplitText);
        _StreamListData := _Lib.PathItems[_T];
        if _StreamListData <> nil then
          begin
            if _StreamListData^.CustomObject is TMemoryRaster then
              begin
                _TG.Add(_StreamListData^.CustomObject);
              end
            else
              begin
                _GR := TileTerrainDefaultBitmapClass.Create;
                _StreamListData^.stream.SeekStart;
                _GR.LoadFromStream(_StreamListData^.stream);
                _TG.Add(_GR);
                _StreamListData^.ForceFreeCustomObject := True;
                _StreamListData^.CustomObject := _GR;
              end;
          end;
        _N := umlDeleteFirstStr(_N, _MultiTextrueNameSplitText);
      until _N = '';
    end
  else
    begin
      _T := _N;
      _StreamListData := _Lib.PathItems[_T];
      if _StreamListData <> nil then
        begin
          if _StreamListData^.CustomObject is TMemoryRaster then
            begin
              _TG.Add(_StreamListData^.CustomObject);
            end
          else
            begin
              _GR := TileTerrainDefaultBitmapClass.Create;
              _StreamListData^.stream.SeekStart;
              _GR.LoadFromStream(_StreamListData^.stream);
              _TG.Add(_GR);
              _StreamListData^.ForceFreeCustomObject := True;
              _StreamListData^.CustomObject := _GR;
            end;
        end;
    end;

  Result := _TG.Count > 0;
end;

function Get2TextureGraphicsAsList(_TexName: U_String; _AsList: TCoreClassStrings): Boolean;
var
  _N, _T: U_String;
begin
  _AsList.Clear;
  _N := _TexName;

  if umlExistsChar(_N, _MultiTextrueNameSplitText) then
    begin
      repeat
        _T := umlGetFirstStr(_N, _MultiTextrueNameSplitText);
        _AsList.Append(_T);
        _N := umlDeleteFirstStr(_N, _MultiTextrueNameSplitText);
      until _N = '';
    end
  else
    begin
      _T := _N;
      _AsList.Append(_T);
    end;

  Result := _AsList.Count > 0;
end;

function GetFullClient2(_Lib: TLibraryManager; _TTPrefix: U_String): U_String;
begin
  Result := _TTPrefix + Format(_Texture_AllFullClientFormat, [umlRandomRange(1, 18)]);
  if _Lib.PathItems[Result] = nil then
      Result := _TTPrefix + Format(_Texture_AllFullClientFormat, [umlRandomRange(1, 2)]);
end;

function IsAllFullClient(_TexName, _NewTTPrefix: U_String): Boolean;
var
  _FirstN, _N: U_String;
begin
  if umlExistsChar(_TexName, _MultiTextrueNameSplitText) then
    begin
      _N := _TexName;
      _FirstN := umlGetFirstStr(_N, _MultiTextrueNameSplitText);
      Result := umlMultipleMatch(True, _NewTTPrefix + _Texture_AllFullClient, _FirstN);
      while (Result) and (umlExistsChar(_N, _MultiTextrueNameSplitText)) do
        begin
          _N := umlDeleteFirstStr(_N, _MultiTextrueNameSplitText);
          _FirstN := umlGetFirstStr(_N, _MultiTextrueNameSplitText);
          Result := Result and (umlMultipleMatch(True, _NewTTPrefix + '~*', _FirstN));
        end;
    end
  else
      Result := umlMultipleMatch(True, _NewTTPrefix + _Texture_AllFullClient, _TexName);
end;

function ProcessTerrainTexture(_Lib: TLibraryManager; _TileInfo: U_String; _GR: TMemoryRaster; _X, _Y: Integer): Boolean;
var
  _GRList: TCoreClassListForObj;
begin
  Result := False;
  _GRList := TCoreClassListForObj.Create;
  if Get2TextureGraphics(_Lib, _TileInfo, _GRList) then
    begin
      while _GRList.Count > 0 do
        begin
          with TMemoryRaster(_GRList[0]) do
            begin
              DrawMode := dmBlend;
              DrawTo(_GR, _X, _Y);
            end;
          _GRList.Delete(0);
        end;
      Result := True;
    end;
  DisposeObject(_GRList);
end;

function TextureCount(_TexName, _TexPrefix: U_String): Integer;
var
  _N: U_String;
begin
  Result := 0;
  _N := _TexName;
  while _N <> '' do
    begin
      if umlMultipleMatch(True, _TexPrefix + '~*', umlGetFirstStr(_N, _MultiTextrueNameSplitText)) then
          inc(Result);
      _N := umlDeleteFirstStr(_N, _MultiTextrueNameSplitText);
    end;
end;

function UpdateBottomTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec):
  Boolean;
begin
  Result := False;

  if not IsAllFullClient(TileData.Tile, _NewTTPrefix) then
    begin
      if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft);
    end
  else
    begin
      AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
      Result := True;
    end;
end;

function UpdateLeftBottomTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec):
  Boolean;
begin
  Result := False;

  if not IsAllFullClient(TileData.Tile, _NewTTPrefix) then
    begin
      if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop);
          Result := True;
        end
      else
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop);
    end
  else
    begin
      AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
      Result := True;
    end;
end;

function UpdateLeftTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
begin
  Result := False;

  if not IsAllFullClient(TileData.Tile, _NewTTPrefix) then
    begin
      if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom);
    end
  else
    begin
      AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
      Result := True;
    end;
end;

function UpdateLeftTopTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec):
  Boolean;
begin
  Result := False;

  if not IsAllFullClient(TileData.Tile, _NewTTPrefix) then
    begin
      if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom);
    end
  else
    begin
      AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
      Result := True;
    end;
end;

function UpdateRightBottomTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec):
  Boolean;
begin
  Result := False;

  if not IsAllFullClient(TileData.Tile, _NewTTPrefix) then
    begin
      if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop);
    end
  else
    begin
      AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
      Result := True;
    end;
end;

function UpdateRightTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec):
  Boolean;
begin
  Result := False;

  if not IsAllFullClient(TileData.Tile, _NewTTPrefix) then
    begin
      if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom);
    end
  else
    begin
      AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
      Result := True;
    end;
end;

function UpdateRightTopTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec):
  Boolean;
begin
  Result := False;

  if not IsAllFullClient(TileData.Tile, _NewTTPrefix) then
    begin
      if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop);
          Result := True;
        end
      else
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom);
    end
  else
    begin
      AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
      Result := True;
    end;
end;

function UpdateTopTextureName(_Lib: TLibraryManager; _NewTTPrefix: U_String; var TileData: TTileDataRec): Boolean;
begin
  Result := False;

  if not IsAllFullClient(TileData.Tile, _NewTTPrefix) then
    begin
      if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_RightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_Angle_LeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeTop_AngleRightLeft) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeft_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRight_AngleTopBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightTop_AngleLeftBottom) then
        begin
          AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftTop_AngleRightBottom) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeLeftBottom_AngleRightTop);
          Result := True;
        end
      else if Compare2Texture(TileData.Tile, _NewTTPrefix + _Texture_AngleLeftBottom_AngleRightTop) then
        begin
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeRightBottom_AngleLeftTop);
          Result := True;
        end
      else
          AutoChangeTexture(TileData.Tile, _NewTTPrefix + _Texture_EndgeBottom_AngleRightLeft);
    end
  else
    begin
      AutoChangeTexture(TileData.Tile, GetFullClient2(_Lib, _NewTTPrefix));
      Result := True;
    end;
end;

procedure Update2Texture(_Lib: TLibraryManager; _NewTTPrefix: U_String;
  var LeftTop, RightTop, LeftBottom, RightBottom: TTileDataRec);
begin
  UpdateLeftTopTextureName(_Lib, _NewTTPrefix, LeftTop);
  UpdateRightTopTextureName(_Lib, _NewTTPrefix, RightTop);
  UpdateLeftBottomTextureName(_Lib, _NewTTPrefix, LeftBottom);
  UpdateRightBottomTextureName(_Lib, _NewTTPrefix, RightBottom);
end;

function Update2TerrainTexture(msPtr: PTileBuffers; _Lib: TLibraryManager; _NewTTPrefix: U_String;
  _Point: TPoint; _Width, _Height: Integer): Boolean;
var
  Rep_Int_X, Rep_Int_Y: Integer;
  _R: TRect;
  PTileData: PTileDataRec;
begin
  Result := True;
  _R := Rect(_Point.x, _Point.y, _Point.x + _Width, _Point.y + _Height);
  for Rep_Int_X := _R.Left to _R.Right do
    for Rep_Int_Y := _R.Top to _R.Bottom do
      begin
        PTileData := msPtr^[Rep_Int_X][Rep_Int_Y];
        if Rep_Int_X = _R.Left then
          begin
            if Rep_Int_Y = _R.Top then
                Result := UpdateLeftTopTextureName(_Lib, _NewTTPrefix, PTileData^) and Result
            else if Rep_Int_Y = _R.Bottom then
                Result := UpdateLeftBottomTextureName(_Lib, _NewTTPrefix, PTileData^) and Result
            else
                Result := UpdateLeftTextureName(_Lib, _NewTTPrefix, PTileData^) and Result;
          end
        else if Rep_Int_X = _R.Right then
          begin
            if Rep_Int_Y = _R.Top then
                Result := UpdateRightTopTextureName(_Lib, _NewTTPrefix, PTileData^) and Result
            else if Rep_Int_Y = _R.Bottom then
                Result := UpdateRightBottomTextureName(_Lib, _NewTTPrefix, PTileData^) and Result
            else
                Result := UpdateRightTextureName(_Lib, _NewTTPrefix, PTileData^) and Result;
          end
        else
          begin
            if Rep_Int_Y = _R.Top then
                Result := UpdateTopTextureName(_Lib, _NewTTPrefix, PTileData^) and Result
            else if Rep_Int_Y = _R.Bottom then
                Result := UpdateBottomTextureName(_Lib, _NewTTPrefix, PTileData^) and Result
            else
                ChangeAllTexture(PTileData^.Tile, GetFullClient2(_Lib, _NewTTPrefix));
          end;
      end;
end;

procedure InternalInitMap(msPtr: PTileBuffers; _MapWidth, _MapHeight: Integer);
var
  x, y: Integer;
  data_ptr: PTileDataRec;
begin
  SetLength(msPtr^, _MapWidth);
  for x := 0 to _MapWidth - 1 do
    begin
      SetLength(msPtr^[x], _MapHeight);
      for y := 0 to _MapHeight - 1 do
        begin
          new(data_ptr);
          data_ptr^.Tile := '';
          data_ptr^.x := x;
          data_ptr^.y := y;
          msPtr^[x][y] := data_ptr;
        end;
    end;
end;

procedure GenerateDefaultFullClientTexture(msPtr: PTileBuffers; _DefaultTexturePrefix: U_String);
var
  i, j: Integer;
  strlist, newstrlist: TCoreClassStringList;
begin
  strlist := TCoreClassStringList.Create;
  newstrlist := TCoreClassStringList.Create;

  for i := 0 to TileLibrary.Count - 1 do
    begin
      TileLibrary.Items[i].GetOriginNameListFromFilter(_DefaultTexturePrefix + _Texture_AllFullClient, newstrlist);
      strlist.AddStrings(newstrlist);
      newstrlist.Clear;
    end;

  for i := low(msPtr^) to high(msPtr^) do
    for j := low(msPtr^[i]) to high(msPtr^[i]) do
        msPtr^[i][j]^.Tile := strlist[umlRandomRange(0, strlist.Count - 1)];

  DisposeObject(strlist);
  DisposeObject(newstrlist);
end;

procedure FreeTileMap(msPtr: PTileBuffers);
var
  i, j: Integer;
begin
  for i := low(msPtr^) to high(msPtr^) do
    for j := low(msPtr^[i]) to high(msPtr^[i]) do
      begin
        if msPtr^[i][j] <> nil then
          begin
            Dispose(msPtr^[i][j]);
            msPtr^[i][j] := nil;
          end;
      end;

  for i := low(msPtr^) to high(msPtr^) do
      SetLength(msPtr^[i], 0);
  SetLength(msPtr^, 0);
end;

procedure PrepareTileCache(_Lib: TLibraryManager; const LibName: SystemString);
var
  strlist: TCoreClassStringList;
  hs: THashStreamList;
  lst: TCoreClassListForObj;
  i: Integer;
begin
  strlist := TCoreClassStringList.Create;
  hs := _Lib.NameItems[LibName];
  if hs <> nil then
    begin
      hs.GetOriginNameList(strlist);
      lst := TCoreClassListForObj.Create;
      for i := 0 to strlist.Count - 1 do
        begin
          Get2TextureGraphics(_Lib, Format('%s:%s', [hs.Name, strlist[i]]), lst);
          lst.Clear;
        end;
      DisposeObject(lst);
    end;
  DisposeObject(strlist);
end;

procedure LoadTileMapFromStream(msPtr: PTileBuffers; var width, height: Integer; var LibName: SystemString; stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  x, y: Integer;
  p: PTileDataRec;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  width := df.Reader.ReadInteger;
  height := df.Reader.ReadInteger;
  LibName := df.Reader.ReadString;

  SetLength(msPtr^, width);
  for x := 0 to width - 1 do
    begin
      SetLength(msPtr^[x], height);
      for y := 0 to height - 1 do
        begin
          new(p);

          p^.Tile := df.Reader.ReadString;
          p^.x := x;
          p^.y := y;
          msPtr^[x][y] := p;
        end;
    end;
  DisposeObject(df);
end;

procedure SaveTileMapToStream(msPtr: PTileBuffers; const width, height: Integer; const LibName: SystemString; stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  x, y: Integer;
  p: PTileDataRec;
begin
  df := TDataFrameEngine.Create;
  df.WriteInteger(width);
  df.WriteInteger(height);
  df.WriteString(LibName);
  for x := 0 to width - 1 do
    for y := 0 to height - 1 do
      begin
        p := msPtr^[x][y];
        df.WriteString(p^.Tile);
      end;
  df.EncodeAsZLib(stream);
  DisposeObject(df);
end;

procedure BuildTileMapAsBitmap(msPtr: PTileBuffers; const width, height: Integer; output: TMemoryRaster);
var
  i, j: Integer;
  p: PTileDataRec;
begin
  output.SetSize(width * 64, height * 64);

  for i := low(msPtr^) to high(msPtr^) do
    for j := low(msPtr^[i]) to high(msPtr^[i]) do
      begin
        p := msPtr^[i][j];
        ProcessTerrainTexture(TileLibrary, p^.Tile, output, p^.x * 64, p^.y * 64);
      end;
end;

procedure BuildTileMapAsBitmap(stream: TCoreClassStream; output: TMemoryRaster);
var
  buff: TTileBuffers;
  width, height: Integer;
  LibName: SystemString;
begin
  try
    LoadTileMapFromStream(@buff, width, height, LibName, stream);
    BuildTileMapAsBitmap(@buff, width, height, output);
    FreeTileMap(@buff);
  except
  end;
end;

procedure BuildTileMapAsBitmap(MapFile: SystemString; output: TMemoryRaster);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(MapFile, fmOpenRead or fmShareDenyWrite);
  BuildTileMapAsBitmap(fs, output);
  DisposeObject(fs);
end;

procedure BuildTileMapAsBitmap(MapFile: SystemString; output: TMemoryRaster; MaxSize: Integer);
var
  bmp: TMemoryRaster;
  newbmp: TMemoryRaster;
  k: Double;
begin
  if MaxSize > 0 then
    begin
      bmp := TMemoryRaster.Create;
      BuildTileMapAsBitmap(MapFile, bmp);
      if (bmp.width > MaxSize) or (bmp.height > MaxSize) then
        begin
          newbmp := TMemoryRaster.Create;
          k := MaxSize / Max(bmp.width, bmp.height);
          newbmp.ZoomFrom(bmp, Round(k * bmp.width), Round(k * bmp.height));
          bmp.Assign(newbmp);
          DisposeObject(newbmp);
        end;
      output.Assign(bmp);
      DisposeObject(bmp);
    end
  else
      BuildTileMapAsBitmap(MapFile, output);
end;

initialization

TileTerrainDefaultBitmapClass := TMemoryRaster;

finalization

end. 
 
 
