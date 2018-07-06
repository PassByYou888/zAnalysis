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
unit AggConvAdaptorVpgen;

interface

{$INCLUDE AggCompiler.inc}


uses
  AggBasics,
  AggVertexSource,
  AggVpGen,
  AggVpGenSegmentator;

type
  TAggConvAdaptorVpgen = class(TAggVertexSource)
  private
    FSource: TAggCustomVertexSource;
    FStart: TPointDouble;
    FPolyFlags: Cardinal;
    FVertices: Integer;
  protected
    FVpGen: TAggCustomVpgen;
  public
    constructor Create(Source: TAggCustomVertexSource; Gen: TAggCustomVpgen);
    procedure SetSource(Source: TAggVertexSource);
    procedure Rewind(PathID: Cardinal); override;
    property Vpgen: TAggCustomVpgen read FVpGen;
  end;

  TAggConvAdaptorVpgenSegmentator = class(TAggConvAdaptorVpgen)
  private
    function GetVpgenSegmentator: TAggVpgenSegmentator;
  public
    function Vertex(X, Y: PDouble): Cardinal; override;
    property VpGenSegmentator: TAggVpgenSegmentator read GetVpgenSegmentator;
  end;

implementation


{ TAggConvAdaptorVpgen }

constructor TAggConvAdaptorVpgen.Create(Source: TAggCustomVertexSource;
  Gen: TAggCustomVpgen);
begin
  FSource := Source;
  FVpGen := Gen;
  FStart.X := 0;
  FStart.Y := 0;

  FPolyFlags := 0;
  FVertices := 0;
end;

procedure TAggConvAdaptorVpgen.SetSource(Source: TAggVertexSource);
begin
  FSource := Source;
end;

procedure TAggConvAdaptorVpgen.Rewind(PathID: Cardinal);
begin
  FSource.Rewind(PathID);

  TAggCustomVpgen(FVpGen).Reset;

  FStart.X := 0;
  FStart.Y := 0;
  FPolyFlags := 0;
  FVertices := 0;
end;

{ TAggConvAdaptorVpgenSegmentator }

function TAggConvAdaptorVpgenSegmentator.GetVpgenSegmentator: TAggVpgenSegmentator;
begin
  Result := TAggVpgenSegmentator(FVpGen);
end;

function TAggConvAdaptorVpgenSegmentator.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
  TX, TY: Double;
begin
  Cmd := CAggPathCmdStop;

  repeat
    Cmd := FVpGen.Vertex(X, Y);

    if not IsStop(Cmd) then
        Break;

    if (FPolyFlags <> 0) and not VpGenSegmentator.AutoUnclose
    then
      begin
        X^ := 0.0;
        Y^ := 0.0;
        Cmd := FPolyFlags;

        FPolyFlags := 0;

        Break;
      end;

    if FVertices < 0 then
      begin
        if FVertices < -1 then
          begin
            FVertices := 0;

            Result := CAggPathCmdStop;

            Exit;
          end;

        Vpgen.MoveTo(FStart.X, FStart.Y);

        FVertices := 1;

        Continue;
      end;

    Cmd := FSource.Vertex(@TX, @TY);

    if IsVertex(Cmd) then
      if IsMoveTo(Cmd) then
        begin
          if VpGenSegmentator.AutoClose and (FVertices > 2) then
            begin
              Vpgen.LineTo(FStart.X, FStart.Y);

              FPolyFlags := CAggPathCmdEndPoly or CAggPathFlagsClose;
              FStart.X := TX;
              FStart.Y := TY;
              FVertices := -1;

              Continue;
            end;

          Vpgen.MoveTo(TX, TY);

          FStart.X := TX;
          FStart.Y := TY;
          FVertices := 1;
        end
      else
        begin
          Vpgen.LineTo(TX, TY);

          Inc(FVertices);
        end
    else if IsEndPoly(Cmd) then
      begin
        FPolyFlags := Cmd;

        if IsClosed(Cmd) or VpGenSegmentator.AutoClose then
          begin
            if VpGenSegmentator.AutoClose then
                FPolyFlags := FPolyFlags or CAggPathFlagsClose;

            if FVertices > 2 then
                Vpgen.LineTo(FStart.X, FStart.Y);

            FVertices := 0;
          end;
      end
    else
      begin
        // CAggPathCmdStop
        if VpGenSegmentator.AutoClose and (FVertices > 2) then
          begin
            Vpgen.LineTo(FStart.X, FStart.Y);

            FPolyFlags := CAggPathCmdEndPoly or CAggPathFlagsClose;
            FVertices := -2;

            Continue;
          end;

        Break;
      end;
  until False;

  Result := Cmd;
end;

end. 
