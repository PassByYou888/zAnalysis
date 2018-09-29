procedure LearnTest;
const
  test_InLen  = 5;
  test_OutLen = 5;
  test_Depth  = 5;

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
  lR          : TLearn;
  i, j, LV    : TLInt;
  f_In, f_Out : TLVec;
  f_out_list  : TLMatrix;
  Search_Out  : TLIVec;
  M           : TLMatrix;
  mr          : TMemoryRaster;
  RasterArray : array of TMemoryRaster;
  m64         : TMemoryStream64;
  md5_1, md5_2: TMD5;
begin
  // Regression test
  for LV := 0 to 2 do
    for lt in RegressionLearnType do
      begin
        DoStatusNoLn('Regression%d test %s ..', [LV, CLearnString[lt]]);
        case LV of
          0: lR := TLearn.CreateRegression(lt, test_InLen, test_OutLen);
          1: lR := TLearn.CreateRegression1(lt, test_InLen, test_OutLen);
          else lR := TLearn.CreateRegression2(lt, test_InLen, test_OutLen);
        end;

        SetLength(f_In, lR.InLen);
        SetLength(f_Out, lR.OutLen);

        for j := 0 to test_Depth - 1 do
          begin
            for i := 0 to lR.InLen - 1 do
                f_In[i] := umlRandomRangeD(0, 100);
            for i := 0 to lR.OutLen - 1 do
                f_Out[i] := umlRandomRangeD(0, 10);
            lR.AddMemory(f_In, f_Out);
          end;
        if lR.Train then
          begin
            SetLength(f_out_list, lR.Count);
            for i := 0 to lR.Count - 1 do
              if not lR.process(@lR[i]^.m_in, @f_out_list[i]) then
                  DoStatusNoLn('process error ');

            if lt = ltLBFGS then
              for i := 0 to lR.Count - 1 do
                begin
                  lR.SearchMemoryWithPearson(lR[i]^.m_in);
                  lR.SearchMemoryWithPearson(lR[i]^.m_in, Search_Out);
                  SetLength(Search_Out, 0);

                  lR.SearchMemoryWithSpearman(lR[i]^.m_in);
                  lR.SearchMemoryWithSpearman(lR[i]^.m_in, Search_Out);
                  SetLength(Search_Out, 0);

                  lR.SearchMemoryWithDistance(lR[i]^.m_in);
                  lR.SearchMemoryWithDistance(lR[i]^.m_in, Search_Out);
                  SetLength(Search_Out, 0);
                end;

            m64 := TMemoryStream64.Create;
            lR.SaveToStream(m64);
            md5_1 := umlStreamMD5(m64);
            m64.Position := 0;
            lR.LoadFromStream(m64);
            m64.Clear;
            lR.SaveToStream(m64);
            md5_2 := umlStreamMD5(m64);
            if not umlCompareMD5(md5_1, md5_2) then
                DoStatusNoLn('encode error ');
            DisposeObject(m64);

            for i := 0 to lR.Count - 1 do
              if lR.process(@lR[i]^.m_in, @f_Out) then
                begin
                  if TKDTree.KDTreeDistance(f_Out, f_out_list[i]) <> 0 then
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

        DisposeObject(lR);
      end;

  // classifier test
  for LV := 0 to 2 do
    for lt in ClassifierLearnType do
      begin
        DoStatusNoLn('Classifier%d test %s ..', [LV, CLearnString[lt]]);
        case LV of
          0: lR := TLearn.CreateClassifier(lt, test_InLen);
          1: lR := TLearn.CreateClassifier1(lt, test_InLen);
          else lR := TLearn.CreateClassifier2(lt, test_InLen);
        end;

        SetLength(f_In, lR.InLen);
        SetLength(f_Out, lR.OutLen);

        for j := 0 to test_Depth - 1 do
          begin
            for i := 0 to lR.InLen - 1 do
                f_In[i] := umlRandomRangeD(0, 100);
            for i := 0 to lR.OutLen - 1 do
                f_Out[i] := umlRandomRangeD(0, 10);
            lR.AddMemory(f_In, f_Out);
          end;
        if lR.Train then
          begin
            SetLength(f_out_list, lR.Count);
            for i := 0 to lR.Count - 1 do
              if not lR.process(@lR[i]^.m_in, @f_out_list[i]) then
                  DoStatusNoLn('process error ');

            m64 := TMemoryStream64.Create;
            lR.SaveToStream(m64);
            md5_1 := umlStreamMD5(m64);
            m64.Position := 0;
            lR.LoadFromStream(m64);
            m64.Clear;
            lR.SaveToStream(m64);
            md5_2 := umlStreamMD5(m64);
            if not umlCompareMD5(md5_1, md5_2) then
                DoStatusNoLn('encode error ');
            DisposeObject(m64);

            for i := 0 to lR.Count - 1 do
              if lR.process(@lR[i]^.m_in, @f_Out) then
                begin
                  if TKDTree.KDTreeDistance(f_Out, f_out_list[i]) <> 0 then
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

        DisposeObject(lR);
      end;

  // raster sampler test

  // build random pixel
  SetLength(RasterArray, test_Depth);
  for j := 0 to test_Depth - 1 do
    begin
      RasterArray[j] := TMemoryRaster.Create;
      RasterArray[j].SetSize(test_Depth, test_Depth);
    end;

  for mr in RasterArray do
    begin
      for j := 0 to mr.height - 1 do
        for i := 0 to mr.width - 1 do
            mr.Pixel[i, j] := RasterColor(umlRandomRange(0, 255), umlRandomRange(0, 255), umlRandomRange(0, 255), 255);
    end;

  // raster Classifier
  for lt in RasterClassifierLearnType do
    begin
      DoStatusNoLn('raster Classifier test %s ..', [CLearnString[lt]]);
      lR := TLearn.CreatePictureClassifier(lt, Trunc(Sqrt(test_Depth)));

      for j := 0 to test_Depth - 1 do
        begin
          M := LMatrixSampler(Trunc(Sqrt(test_Depth)), RasterArray[j]);
          SetLength(f_Out, 1);
          f_Out[0] := j;
          lR.AddMatrix(M, f_Out);
        end;
      if lR.Train then
        begin
          DoStatusNoLn;

          SetLength(Search_Out, test_Depth);
          SetLength(f_out_list, test_Depth);
          for j := 0 to test_Depth - 1 do
            begin
              M := LMatrixSampler(Trunc(Sqrt(test_Depth)), RasterArray[j]);

              Search_Out[j] := lR.processMaxIndex(M);
              if Search_Out[j] <> j then
                  DoStatus('raster Classifier %s search matrix problem..', [CLearnString[lt]]);

              Search_Out[j] := lR.SearchMemoryWithDistance(lvec(M, lR.InLen));
              if Search_Out[j] <> j then
                  DoStatus('raster Classifier %s SearchMemoryWithDistance problem..', [CLearnString[lt]]);

              Search_Out[j] := lR.SearchMemoryWithPearson(lvec(M, lR.InLen));
              if Search_Out[j] <> j then
                  DoStatus('raster Classifier %s SearchMemoryWithPearson problem..', [CLearnString[lt]]);

              Search_Out[j] := lR.SearchMemoryWithSpearman(lvec(M, lR.InLen));
              if Search_Out[j] <> j then
                  DoStatus('raster Classifier %s SearchMemoryWithSpearman problem..', [CLearnString[lt]]);
            end;
        end
      else
          DoStatusNoLn('train error ');

      DoStatusNoLn;
      DisposeObject(lR);
    end;

  for j := 0 to test_Depth - 1 do
      DisposeObject(RasterArray[j]);
  SetLength(RasterArray, 0);

  DoStatus('all learn test finish!');
end; 
 
 
