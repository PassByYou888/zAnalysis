procedure LearnTest;
const
  test_InLen  = 10;
  test_OutLen = 5;
  test_Depth  = 50;

  RegressionLearnType: set of TLearnType = [
    ltKDT, ltKM,
    ltForest, ltLogit,
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo,
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT];

  ClassifierLearnType: set of TLearnType = [
    ltKDT, ltKM,
    ltForest, ltLogit,
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo,
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT];

  RasterClassifierLearnType: set of TLearnType = [
    ltKDT, ltKM,
    ltForest,
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod];

var
  lt          : TLearnType;
  lr          : TLearn;
  I, J, lv    : TLInt;
  f_In, f_Out : TLVec;
  f_out_list  : TLMatrix;
  Search_Out  : TLIVec;
  m           : TLMatrix;
  mr          : TMemoryRaster;
  RasterArray : array of TMemoryRaster;
  m64         : TMemoryStream64;
  md5_1, md5_2: TMD5;
begin
  // Regression test
  for lv := 0 to 2 do
    for lt in RegressionLearnType do
      begin
        DoStatusNoLn('Regression test %s ..', [CLearnString[lt]]);
        case lv of
          0: lr := TLearn.CreateRegression(lt, test_InLen, test_OutLen);
          1: lr := TLearn.CreateRegression1(lt, test_InLen, test_OutLen);
          else lr := TLearn.CreateRegression2(lt, test_InLen, test_OutLen);
        end;

        SetLength(f_In, lr.InLen);
        SetLength(f_Out, lr.OutLen);

        for J := 0 to test_Depth - 1 do
          begin
            for I := 0 to lr.InLen - 1 do
                f_In[I] := umlRandomRangeD(0, 100);
            for I := 0 to lr.OutLen - 1 do
                f_Out[I] := umlRandomRangeD(0, 10);
            lr.AddMemory(f_In, f_Out);
          end;
        if lr.Train then
          begin
            SetLength(f_out_list, lr.Count);
            for I := 0 to lr.Count - 1 do
              if not lr.process(@lr[I]^.m_in, @f_out_list[I]) then
                  DoStatusNoLn('process error ');

            if lt = ltLBFGS then
              for I := 0 to lr.Count - 1 do
                begin
                  lr.SearchMemoryWithPearson(lr[I]^.m_in);
                  lr.SearchMemoryWithPearson(lr[I]^.m_in, Search_Out);
                  SetLength(Search_Out, 0);

                  lr.SearchMemoryWithSpearman(lr[I]^.m_in);
                  lr.SearchMemoryWithSpearman(lr[I]^.m_in, Search_Out);
                  SetLength(Search_Out, 0);

                  lr.SearchMemoryWithDistance(lr[I]^.m_in);
                  lr.SearchMemoryWithDistance(lr[I]^.m_in, Search_Out);
                  SetLength(Search_Out, 0);
                end;

            m64 := TMemoryStream64.Create;
            lr.SaveToStream(m64);
            md5_1 := umlStreamMD5(m64);
            m64.Position := 0;
            lr.LoadFromStream(m64);
            m64.Clear;
            lr.SaveToStream(m64);
            md5_2 := umlStreamMD5(m64);
            if not umlCompareMD5(md5_1, md5_2) then
                DoStatusNoLn('encode error ');
            DisposeObject(m64);

            for I := 0 to lr.Count - 1 do
              if lr.process(@lr[I]^.m_in, @f_Out) then
                begin
                  if TKDTree.KDTreeDistance(f_Out, f_out_list[I]) <> 0 then
                      DoStatusNoLn('verify data error ');
                end
              else
                  DoStatusNoLn('verify data with process time error ');

            SetLength(f_out_list, 0, 0);
          end
        else
            DoStatusNoLn('train error ');

        DoStatusNoLn;

        SetLength(f_In, 0);
        SetLength(f_Out, 0);

        DisposeObject(lr);
      end;

  // classifier test
  for lv := 0 to 2 do
    for lt in ClassifierLearnType do
      begin
        DoStatusNoLn('Classifier test %s ..', [CLearnString[lt]]);
        case lv of
          0: lr := TLearn.CreateClassifier(lt, test_InLen);
          1: lr := TLearn.CreateClassifier1(lt, test_InLen);
          else lr := TLearn.CreateClassifier2(lt, test_InLen);
        end;

        SetLength(f_In, lr.InLen);
        SetLength(f_Out, lr.OutLen);

        for J := 0 to test_Depth - 1 do
          begin
            for I := 0 to lr.InLen - 1 do
                f_In[I] := umlRandomRangeD(0, 100);
            for I := 0 to lr.OutLen - 1 do
                f_Out[I] := umlRandomRangeD(0, 10);
            lr.AddMemory(f_In, f_Out);
          end;
        if lr.Train then
          begin
            SetLength(f_out_list, lr.Count);
            for I := 0 to lr.Count - 1 do
              if not lr.process(@lr[I]^.m_in, @f_out_list[I]) then
                  DoStatusNoLn('process error ');

            m64 := TMemoryStream64.Create;
            lr.SaveToStream(m64);
            md5_1 := umlStreamMD5(m64);
            m64.Position := 0;
            lr.LoadFromStream(m64);
            m64.Clear;
            lr.SaveToStream(m64);
            md5_2 := umlStreamMD5(m64);
            if not umlCompareMD5(md5_1, md5_2) then
                DoStatusNoLn('encode error ');
            DisposeObject(m64);

            for I := 0 to lr.Count - 1 do
              if lr.process(@lr[I]^.m_in, @f_Out) then
                begin
                  if TKDTree.KDTreeDistance(f_Out, f_out_list[I]) <> 0 then
                      DoStatusNoLn('verify data error ');
                end
              else
                  DoStatusNoLn('process verify error ');

            SetLength(f_out_list, 0, 0);
          end
        else
            DoStatusNoLn('train error ');

        DoStatusNoLn;

        SetLength(f_In, 0);
        SetLength(f_Out, 0);

        DisposeObject(lr);
      end;

  // raster sampler test

  // build random pixel
  SetLength(RasterArray, test_Depth);
  for J := 0 to test_Depth - 1 do
    begin
      RasterArray[J] := TMemoryRaster.Create;
      RasterArray[J].SetSize(test_Depth, test_Depth);
    end;

  for mr in RasterArray do
    begin
      for J := 0 to mr.Height - 1 do
        for I := 0 to mr.Width - 1 do
            mr.Pixel[I, J] := RasterColor(umlRandomRange(0, 255), umlRandomRange(0, 255), umlRandomRange(0, 255), 255);
    end;

  // raster Classifier
  for lt in RasterClassifierLearnType do
    begin
      DoStatusNoLn('raster Classifier test %s ..', [CLearnString[lt]]);
      lr := TLearn.CreatePictureClassifier(lt, Trunc(Sqrt(test_Depth)));

      for J := 0 to test_Depth - 1 do
        begin
          m := MatrixSampler(Trunc(Sqrt(test_Depth)), RasterArray[J]);
          SetLength(f_Out, 1);
          f_Out[0] := J;
          lr.AddMatrix(m, f_Out);
        end;
      if lr.Train then
        begin
          DoStatusNoLn;

          SetLength(Search_Out, test_Depth);
          SetLength(f_out_list, test_Depth);
          for J := 0 to test_Depth - 1 do
            begin
              m := MatrixSampler(Trunc(Sqrt(test_Depth)), RasterArray[J]);

              Search_Out[J] := lr.processMaxIndex(m);
              if Search_Out[J] <> J then
                  DoStatus('raster Classifier %s search matrix problem..', [CLearnString[lt]]);

              Search_Out[J] := lr.SearchMemoryWithDistance(LVec(m, lr.InLen));
              if Search_Out[J] <> J then
                  DoStatus('raster Classifier %s SearchMemoryWithDistance problem..', [CLearnString[lt]]);

              Search_Out[J] := lr.SearchMemoryWithPearson(LVec(m, lr.InLen));
              if Search_Out[J] <> J then
                  DoStatus('raster Classifier %s SearchMemoryWithPearson problem..', [CLearnString[lt]]);

              Search_Out[J] := lr.SearchMemoryWithSpearman(LVec(m, lr.InLen));
              if Search_Out[J] <> J then
                  DoStatus('raster Classifier %s SearchMemoryWithSpearman problem..', [CLearnString[lt]]);
            end;
        end
      else
          DoStatusNoLn('train error ');

      DoStatusNoLn;
      DisposeObject(lr);
    end;

  for J := 0 to test_Depth - 1 do
      DisposeObject(RasterArray[J]);
  SetLength(RasterArray, 0);

  DoStatus('all learn test finish!');
end;
