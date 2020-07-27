object fPreviews: TfPreviews
  Left = 327
  Top = 311
  Width = 548
  Height = 454
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Previews'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 532
    Height = 416
    Align = alClient
  end
  object Label1: TLabel
    Left = 9
    Top = 5
    Width = 37
    Height = 13
    Caption = 'Source:'
    Transparent = True
  end
  object Label2: TLabel
    Left = 233
    Top = 5
    Width = 34
    Height = 13
    Caption = 'Result:'
    Transparent = True
  end
  object OkButton: TBitBtn
    Left = 444
    Top = 21
    Width = 87
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OkButtonClick
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF006600006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0066001EB2311FB13300
      6600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF00660031C24F22B7381AB02D21B437006600FF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF00660047D36D3BCB5E25A83B0066001B
      A92E1DB132006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF006600
      4FD67953DE7F31B54D006600FF00FF006600179D271EAE31006600FF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FF00660041C563006600FF00FFFF00FFFF
      00FFFF00FF00660019AA2B006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF006600149D210066
      00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FF006600006600FF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FF006600006600FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
  end
  object CancelButton: TBitBtn
    Left = 444
    Top = 50
    Width = 87
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF00009A00009AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00009A0000
      9AFF00FFFF00FFFF00FFFF00FFFF00FF00009A174AFD103BF400009AFF00FFFF
      00FFFF00FFFF00FF00009A002CF80030FC00009AFF00FFFF00FFFF00FFFF00FF
      00009A1A47F81A4CFF123BF100009AFF00FFFF00FF00009A012DF60132FF002A
      F300009AFF00FFFF00FFFF00FFFF00FFFF00FF00009A1C47F61B4DFF143EF400
      009A00009A002DF80134FF032BF200009AFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FF00009A1D48F61D50FF103DFB0431FE0132FF002CF600009AFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00009A1A48F913
      42FF0C3CFF0733F600009AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FF00009A214EFC1D4BFF1847FF1743F600009AFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00009A2E5BF92C5FFF22
      4DF8204BF82355FF1B46F600009AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FF00009A3664FA386BFF2D59F400009A00009A224CF42558FF1D49F60000
      9AFF00FFFF00FFFF00FFFF00FFFF00FF00009A4071FA4274FF325DF100009AFF
      00FFFF00FF00009A224DF1275AFF204CF800009AFF00FFFF00FFFF00FFFF00FF
      00009A497AFC3B66F300009AFF00FFFF00FFFF00FFFF00FF00009A2550F42655
      FA00009AFF00FFFF00FFFF00FFFF00FFFF00FF00009A00009AFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FF00009A00009AFF00FFFF00FFFF00FFFF00FFFF00FF
      FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
      FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
      00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
  end
  object PageControl1: TPageControl
    Left = 9
    Top = 173
    Width = 510
    Height = 207
    ActivePage = TabSheet1
    Anchors = []
    HotTrack = True
    TabOrder = 7
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Tag = 1
      Caption = 'Contrast'
      object Label3: TLabel
        Left = 8
        Top = 12
        Width = 46
        Height = 13
        Caption = '&Contrast:'
        FocusControl = Edit1
      end
      object Label22: TLabel
        Left = 8
        Top = 52
        Width = 54
        Height = 13
        Caption = '&Brightness:'
        FocusControl = Edit21
      end
      object Edit1: TEdit
        Left = 74
        Top = 9
        Width = 33
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = Edit1Change
      end
      object TrackBar1: TTrackBar
        Left = 109
        Top = 6
        Width = 345
        Height = 29
        Max = 100
        Min = -100
        Frequency = 10
        TabOrder = 1
        OnChange = TrackBar1Change
      end
      object Edit21: TEdit
        Left = 74
        Top = 49
        Width = 33
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = Edit1Change
      end
      object TrackBar12: TTrackBar
        Left = 109
        Top = 46
        Width = 345
        Height = 29
        Max = 100
        Min = -100
        Frequency = 10
        TabOrder = 3
        OnChange = TrackBar1Change
      end
    end
    object TabSheet5: TTabSheet
      Tag = 5
      Caption = 'HSV'
      object Label10: TLabel
        Left = 8
        Top = 12
        Width = 41
        Height = 13
        Caption = '&Hue (H):'
        FocusControl = Edit19
      end
      object Label11: TLabel
        Left = 8
        Top = 40
        Width = 71
        Height = 13
        Caption = '&Saturation (S):'
        FocusControl = Edit18
      end
      object Label12: TLabel
        Left = 8
        Top = 68
        Width = 47
        Height = 13
        Caption = '&Value (V):'
        FocusControl = Edit17
      end
      object Label13: TLabel
        Left = 143
        Top = 93
        Width = 53
        Height = 13
        Caption = 'Base color:'
        ParentShowHint = False
        ShowHint = True
      end
      object Label14: TLabel
        Left = 288
        Top = 93
        Width = 51
        Height = 13
        Caption = 'New color:'
      end
      object Edit17: TEdit
        Left = 96
        Top = 65
        Width = 33
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = Edit19Change
      end
      object Edit18: TEdit
        Left = 96
        Top = 37
        Width = 33
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = Edit19Change
      end
      object Edit19: TEdit
        Left = 96
        Top = 9
        Width = 33
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = Edit19Change
      end
      object TrackBar9: TTrackBar
        Left = 135
        Top = 6
        Width = 320
        Height = 27
        Max = 180
        Min = -180
        Frequency = 10
        TabOrder = 3
        OnChange = TrackBar9Change
      end
      object TrackBar10: TTrackBar
        Left = 135
        Top = 34
        Width = 320
        Height = 27
        Max = 100
        Min = -100
        Frequency = 10
        TabOrder = 4
        OnChange = TrackBar9Change
      end
      object TrackBar11: TTrackBar
        Left = 135
        Top = 62
        Width = 320
        Height = 27
        Max = 100
        Min = -100
        Frequency = 10
        TabOrder = 5
        OnChange = TrackBar9Change
      end
      object Panel3: TPanel
        Left = 142
        Top = 110
        Width = 97
        Height = 59
        BevelOuter = bvLowered
        TabOrder = 6
        object HSVBox3: THSVBox
          Left = 1
          Top = 1
          Width = 95
          Height = 57
          OnChange = HSVBox3Change
          Align = alClient
        end
      end
      object Panel4: TPanel
        Left = 288
        Top = 110
        Width = 98
        Height = 60
        BevelOuter = bvLowered
        TabOrder = 7
        object HSVBox1: THSVBox
          Left = 1
          Top = 1
          Width = 96
          Height = 58
          OnChange = HSVBox3Change
          Align = alClient
        end
      end
    end
    object TabSheet2: TTabSheet
      Tag = 2
      Caption = 'HSL'
      object Label4: TLabel
        Left = 8
        Top = 12
        Width = 41
        Height = 13
        Caption = '&Hue (H):'
        FocusControl = Edit4
      end
      object Label5: TLabel
        Left = 8
        Top = 44
        Width = 71
        Height = 13
        Caption = '&Saturation (S):'
        FocusControl = Edit2
      end
      object Label6: TLabel
        Left = 8
        Top = 76
        Width = 70
        Height = 13
        Caption = '&Luminosity (L):'
        FocusControl = Edit3
      end
      object Edit3: TEdit
        Left = 105
        Top = 73
        Width = 33
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = Edit4Change
      end
      object Edit2: TEdit
        Left = 105
        Top = 41
        Width = 33
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = Edit4Change
      end
      object Edit4: TEdit
        Left = 105
        Top = 9
        Width = 33
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = Edit4Change
      end
      object TrackBar2: TTrackBar
        Left = 143
        Top = 6
        Width = 312
        Height = 29
        Max = 180
        Min = -180
        Frequency = 10
        TabOrder = 3
        OnChange = TrackBar2Change
      end
      object TrackBar3: TTrackBar
        Left = 143
        Top = 38
        Width = 312
        Height = 29
        Max = 100
        Min = -100
        Frequency = 10
        TabOrder = 4
        OnChange = TrackBar2Change
      end
      object TrackBar5: TTrackBar
        Left = 143
        Top = 70
        Width = 312
        Height = 29
        Max = 100
        Min = -100
        Frequency = 10
        TabOrder = 5
        OnChange = TrackBar2Change
      end
    end
    object TabSheet3: TTabSheet
      Tag = 3
      Caption = 'RGB'
      object Label7: TLabel
        Left = 8
        Top = 12
        Width = 41
        Height = 13
        Caption = '&Red (R):'
        FocusControl = Edit7
      end
      object Label8: TLabel
        Left = 8
        Top = 44
        Width = 51
        Height = 13
        Caption = '&Green (G):'
        FocusControl = Edit6
      end
      object Label9: TLabel
        Left = 8
        Top = 76
        Width = 41
        Height = 13
        Caption = '&Blue (B):'
        FocusControl = Edit5
      end
      object Edit5: TEdit
        Left = 72
        Top = 73
        Width = 33
        Height = 21
        TabOrder = 4
        Text = '0'
        OnChange = Edit7Change
      end
      object Edit6: TEdit
        Left = 72
        Top = 41
        Width = 33
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = Edit7Change
      end
      object Edit7: TEdit
        Left = 72
        Top = 9
        Width = 33
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = Edit7Change
      end
      object TrackBar6: TTrackBar
        Left = 110
        Top = 6
        Width = 333
        Height = 28
        Max = 255
        Min = -255
        Frequency = 10
        TabOrder = 1
        OnChange = TrackBar6Change
      end
      object TrackBar7: TTrackBar
        Left = 110
        Top = 38
        Width = 333
        Height = 28
        Max = 255
        Min = -255
        Frequency = 10
        TabOrder = 3
        OnChange = TrackBar6Change
      end
      object TrackBar8: TTrackBar
        Left = 110
        Top = 70
        Width = 333
        Height = 28
        Max = 255
        Min = -255
        Frequency = 10
        TabOrder = 5
        OnChange = TrackBar6Change
      end
    end
    object TabSheet4: TTabSheet
      Tag = 4
      Caption = 'User filter'
      object GroupBox1: TGroupBox
        Left = 10
        Top = 8
        Width = 359
        Height = 155
        Caption = ' Filter values'
        TabOrder = 0
        object Label15: TLabel
          Left = 208
          Top = 32
          Width = 36
          Height = 13
          Caption = 'Divisor:'
        end
        object Edit8: TEdit
          Left = 10
          Top = 21
          Width = 33
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = Edit8Change
        end
        object Edit9: TEdit
          Tag = 3
          Left = 10
          Top = 48
          Width = 33
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = Edit8Change
        end
        object Edit10: TEdit
          Tag = 6
          Left = 10
          Top = 75
          Width = 33
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = Edit8Change
        end
        object Edit11: TEdit
          Tag = 1
          Left = 77
          Top = 21
          Width = 33
          Height = 21
          TabOrder = 6
          Text = '0'
          OnChange = Edit8Change
        end
        object Edit12: TEdit
          Tag = 4
          Left = 77
          Top = 48
          Width = 33
          Height = 21
          TabOrder = 8
          Text = '1'
          OnChange = Edit8Change
        end
        object Edit13: TEdit
          Tag = 7
          Left = 77
          Top = 75
          Width = 33
          Height = 21
          TabOrder = 10
          Text = '0'
          OnChange = Edit8Change
        end
        object Edit14: TEdit
          Tag = 2
          Left = 144
          Top = 21
          Width = 33
          Height = 21
          TabOrder = 12
          Text = '0'
          OnChange = Edit8Change
        end
        object Edit15: TEdit
          Tag = 5
          Left = 144
          Top = 48
          Width = 33
          Height = 21
          TabOrder = 14
          Text = '0'
          OnChange = Edit8Change
        end
        object Edit16: TEdit
          Tag = 8
          Left = 144
          Top = 75
          Width = 33
          Height = 21
          TabOrder = 16
          Text = '0'
          OnChange = Edit8Change
        end
        object UpDown1: TUpDown
          Left = 43
          Top = 21
          Width = 15
          Height = 21
          Associate = Edit8
          Min = -100
          TabOrder = 1
          Thousands = False
        end
        object UpDown2: TUpDown
          Left = 43
          Top = 48
          Width = 15
          Height = 21
          Associate = Edit9
          Min = -100
          TabOrder = 3
          Thousands = False
        end
        object UpDown3: TUpDown
          Left = 43
          Top = 75
          Width = 15
          Height = 21
          Associate = Edit10
          Min = -100
          TabOrder = 5
          Thousands = False
        end
        object UpDown4: TUpDown
          Left = 110
          Top = 21
          Width = 15
          Height = 21
          Associate = Edit11
          Min = -100
          TabOrder = 7
          Thousands = False
        end
        object UpDown5: TUpDown
          Left = 110
          Top = 48
          Width = 15
          Height = 21
          Associate = Edit12
          Min = -100
          Position = 1
          TabOrder = 9
          Thousands = False
        end
        object UpDown6: TUpDown
          Left = 110
          Top = 75
          Width = 15
          Height = 21
          Associate = Edit13
          Min = -100
          TabOrder = 11
          Thousands = False
        end
        object UpDown7: TUpDown
          Left = 177
          Top = 21
          Width = 15
          Height = 21
          Associate = Edit14
          Min = -100
          TabOrder = 13
          Thousands = False
        end
        object UpDown8: TUpDown
          Left = 177
          Top = 48
          Width = 15
          Height = 21
          Associate = Edit15
          Min = -100
          TabOrder = 15
          Thousands = False
        end
        object UpDown9: TUpDown
          Left = 177
          Top = 75
          Width = 15
          Height = 21
          Associate = Edit16
          Min = -100
          TabOrder = 17
          Thousands = False
        end
        object Button4: TButton
          Left = 10
          Top = 118
          Width = 70
          Height = 21
          Caption = '&Load'
          TabOrder = 20
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 90
          Top = 118
          Width = 70
          Height = 21
          Caption = '&Save'
          TabOrder = 21
          OnClick = Button5Click
        end
        object Edit20: TEdit
          Tag = 9
          Left = 208
          Top = 48
          Width = 33
          Height = 21
          TabOrder = 18
          Text = '0'
          OnChange = Edit8Change
        end
        object UpDown10: TUpDown
          Left = 241
          Top = 48
          Width = 15
          Height = 21
          Associate = Edit20
          Min = -100
          TabOrder = 19
          Thousands = False
        end
      end
      object GroupBox3: TGroupBox
        Left = 380
        Top = 8
        Width = 113
        Height = 155
        Caption = ' Presets'
        TabOrder = 1
        object ListBox1: TListBox
          Left = 8
          Top = 18
          Width = 97
          Height = 127
          ItemHeight = 13
          TabOrder = 0
          OnClick = ListBox1Click
        end
      end
    end
    object TabSheet6: TTabSheet
      Tag = 6
      Caption = 'Equalization'
      object Label16: TLabel
        Left = 8
        Top = 148
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label17: TLabel
        Left = 280
        Top = 148
        Width = 18
        Height = 13
        Caption = '255'
      end
      object Label18: TLabel
        Left = 120
        Top = 148
        Width = 47
        Height = 13
        Caption = 'Threshold'
      end
      object Label19: TLabel
        Left = 120
        Top = 4
        Width = 57
        Height = 13
        Caption = 'Equalization'
      end
      object Label20: TLabel
        Left = 8
        Top = 5
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label21: TLabel
        Left = 280
        Top = 5
        Width = 18
        Height = 13
        Caption = '255'
      end
      object SpeedButton3: TSpeedButton
        Left = 326
        Top = 119
        Width = 73
        Height = 25
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'Equalize'
        OnClick = SpeedButton3Click
      end
      object Panel5: TPanel
        Left = 8
        Top = 20
        Width = 293
        Height = 123
        BevelOuter = bvNone
        BorderStyle = bsSingle
        TabOrder = 0
        object RulerBox2: TRulerBox
          Left = 0
          Top = 0
          Width = 289
          Height = 13
          Background = clSilver
          GripsDir = gdDown
          Ruler = False
          DotPerUnit = 1.133333333333333000
          Frequency = 16.000000000000000000
          LabelFreq = 32.000000000000000000
          RulerColor = clGray
          ViewMax = 255.000000000000000000
          MaxGripHeight = 15
          OnRulerPosChange = RulerBox2RulerPosChange
          FitInView = True
          GripsCount = 2
          Align = alTop
        end
        object RulerBox1: TRulerBox
          Left = 0
          Top = 87
          Width = 289
          Height = 32
          Background = clSilver
          DotPerUnit = 1.133333333333330000
          Frequency = 16.000000000000000000
          LabelFreq = 32.000000000000000000
          RulerColor = clSilver
          ViewMax = 255.000000000000000000
          MaxGripHeight = 15
          OnRulerPosChange = RulerBox1RulerPosChange
          GripsCount = 2
          Align = alBottom
        end
        object HistogramBox1: THistogramBox
          Left = 0
          Top = 14
          Width = 289
          Height = 67
          Background = clSilver
          Labels = []
        end
      end
      object GroupBox4: TGroupBox
        Left = 325
        Top = 14
        Width = 74
        Height = 99
        Caption = ' Histogram '
        TabOrder = 1
        object CheckListBox1: TCheckListBox
          Left = 8
          Top = 21
          Width = 57
          Height = 67
          BorderStyle = bsNone
          Color = clBtnFace
          Ctl3D = True
          ItemHeight = 13
          Items.Strings = (
            'Red'
            'Green'
            'Blue'
            'Gray')
          ParentCtl3D = False
          TabOrder = 0
          OnClick = CheckListBox1Click
        end
      end
    end
    object TabSheet7: TTabSheet
      Tag = 7
      Caption = 'Bump map'
      object Label25: TLabel
        Left = 16
        Top = 144
        Width = 133
        Height = 13
        Caption = 'Source image quantity (%):'
      end
      object GroupBox2: TGroupBox
        Left = 10
        Top = 8
        Width = 483
        Height = 125
        Caption = ' Light '
        TabOrder = 0
        object Label23: TLabel
          Left = 11
          Top = 70
          Width = 32
          Height = 13
          Caption = 'Width:'
        end
        object Label24: TLabel
          Left = 11
          Top = 95
          Width = 35
          Height = 13
          Caption = 'Height:'
        end
        object Label26: TLabel
          Left = 153
          Top = 16
          Width = 29
          Height = 13
          Caption = 'Color:'
        end
        object Label27: TLabel
          Left = 11
          Top = 22
          Width = 23
          Height = 13
          Caption = 'Left:'
        end
        object Label28: TLabel
          Left = 11
          Top = 46
          Width = 22
          Height = 13
          Caption = 'Top:'
        end
        object Panel1: TPanel
          Left = 154
          Top = 34
          Width = 134
          Height = 77
          BevelOuter = bvLowered
          TabOrder = 8
          object HSVBox2: THSVBox
            Left = 1
            Top = 1
            Width = 132
            Height = 75
            OnChange = HSVBox2Change
            Align = alClient
          end
        end
        object Edit22: TEdit
          Left = 70
          Top = 19
          Width = 41
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = Edit22Change
        end
        object UpDown11: TUpDown
          Left = 111
          Top = 19
          Width = 15
          Height = 21
          Associate = Edit22
          Max = 32767
          Increment = 10
          TabOrder = 1
          Thousands = False
        end
        object UpDown12: TUpDown
          Left = 111
          Top = 43
          Width = 15
          Height = 21
          Associate = Edit23
          Max = 32767
          Increment = 10
          TabOrder = 3
          Thousands = False
        end
        object Edit23: TEdit
          Left = 70
          Top = 43
          Width = 41
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = Edit22Change
        end
        object Edit24: TEdit
          Left = 70
          Top = 67
          Width = 41
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = Edit22Change
        end
        object UpDown13: TUpDown
          Left = 111
          Top = 67
          Width = 15
          Height = 21
          Associate = Edit24
          Max = 32767
          Increment = 10
          TabOrder = 5
          Thousands = False
        end
        object Edit25: TEdit
          Left = 70
          Top = 91
          Width = 41
          Height = 21
          TabOrder = 6
          Text = '0'
          OnChange = Edit22Change
        end
        object UpDown14: TUpDown
          Left = 111
          Top = 91
          Width = 15
          Height = 21
          Associate = Edit25
          Max = 32767
          Increment = 10
          TabOrder = 7
          Thousands = False
        end
      end
      object Edit26: TEdit
        Left = 176
        Top = 141
        Width = 32
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = Edit22Change
      end
      object UpDown15: TUpDown
        Left = 208
        Top = 141
        Width = 16
        Height = 21
        Associate = Edit26
        Increment = 10
        TabOrder = 2
        Thousands = False
      end
    end
    object TabSheet8: TTabSheet
      Tag = 8
      Caption = 'Lens'
      object GroupBox5: TGroupBox
        Left = 10
        Top = 8
        Width = 483
        Height = 129
        Caption = ' Lens '
        TabOrder = 0
        object Label29: TLabel
          Left = 11
          Top = 70
          Width = 32
          Height = 13
          Caption = 'Width:'
        end
        object Label30: TLabel
          Left = 11
          Top = 95
          Width = 35
          Height = 13
          Caption = 'Height:'
        end
        object Label32: TLabel
          Left = 11
          Top = 22
          Width = 23
          Height = 13
          Caption = 'Left:'
        end
        object Label33: TLabel
          Left = 11
          Top = 46
          Width = 22
          Height = 13
          Caption = 'Top:'
        end
        object Label31: TLabel
          Left = 153
          Top = 22
          Width = 54
          Height = 13
          Caption = 'Refraction:'
        end
        object Edit27: TEdit
          Left = 73
          Top = 19
          Width = 41
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = Edit27Change
        end
        object UpDown16: TUpDown
          Left = 114
          Top = 19
          Width = 15
          Height = 21
          Associate = Edit27
          Max = 32767
          Increment = 10
          TabOrder = 1
          Thousands = False
        end
        object UpDown17: TUpDown
          Left = 114
          Top = 43
          Width = 15
          Height = 21
          Associate = Edit28
          Max = 32767
          Increment = 10
          TabOrder = 3
          Thousands = False
        end
        object Edit28: TEdit
          Left = 73
          Top = 43
          Width = 41
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = Edit27Change
        end
        object Edit29: TEdit
          Left = 73
          Top = 67
          Width = 41
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = Edit27Change
        end
        object UpDown18: TUpDown
          Left = 114
          Top = 67
          Width = 15
          Height = 21
          Associate = Edit29
          Max = 32767
          Increment = 10
          TabOrder = 5
          Thousands = False
        end
        object Edit30: TEdit
          Left = 73
          Top = 91
          Width = 41
          Height = 21
          TabOrder = 6
          Text = '0'
          OnChange = Edit27Change
        end
        object UpDown19: TUpDown
          Left = 114
          Top = 91
          Width = 15
          Height = 21
          Associate = Edit30
          Max = 32767
          Increment = 10
          TabOrder = 7
          Thousands = False
        end
        object UpDown20: TUpDown
          Left = 256
          Top = 19
          Width = 15
          Height = 21
          Associate = Edit31
          Max = 32767
          TabOrder = 9
          Thousands = False
        end
        object Edit31: TEdit
          Left = 215
          Top = 19
          Width = 41
          Height = 21
          TabOrder = 8
          Text = '0'
          OnChange = Edit27Change
        end
      end
    end
    object TabSheet9: TTabSheet
      Tag = 9
      Caption = 'Wave'
      object GroupBox6: TGroupBox
        Left = 10
        Top = 8
        Width = 483
        Height = 150
        Caption = ' Wave '
        TabOrder = 0
        object Label34: TLabel
          Left = 12
          Top = 22
          Width = 51
          Height = 13
          Caption = 'Amplitude:'
        end
        object Label35: TLabel
          Left = 12
          Top = 54
          Width = 65
          Height = 13
          Caption = 'Wave length:'
        end
        object Label36: TLabel
          Left = 12
          Top = 86
          Width = 33
          Height = 13
          Caption = 'Phase:'
        end
        object Edit32: TEdit
          Left = 128
          Top = 19
          Width = 41
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = Edit32Change
        end
        object UpDown21: TUpDown
          Left = 169
          Top = 19
          Width = 15
          Height = 21
          Associate = Edit32
          Max = 32767
          TabOrder = 1
          Thousands = False
        end
        object UpDown22: TUpDown
          Left = 169
          Top = 51
          Width = 15
          Height = 21
          Associate = Edit33
          Max = 32767
          TabOrder = 3
          Thousands = False
        end
        object Edit33: TEdit
          Left = 128
          Top = 51
          Width = 41
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = Edit32Change
        end
        object Edit34: TEdit
          Left = 128
          Top = 83
          Width = 41
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = Edit32Change
        end
        object UpDown23: TUpDown
          Left = 169
          Top = 83
          Width = 15
          Height = 21
          Associate = Edit34
          Max = 359
          TabOrder = 5
          Thousands = False
        end
        object CheckBox2: TCheckBox
          Left = 10
          Top = 118
          Width = 129
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Reflective'
          TabOrder = 6
          OnClick = Edit32Change
        end
      end
    end
    object TabSheet10: TTabSheet
      Tag = 10
      Caption = 'Morph filters'
      object GroupBox7: TGroupBox
        Left = 10
        Top = 8
        Width = 483
        Height = 121
        Caption = ' Morph filters '
        TabOrder = 0
        object Label37: TLabel
          Left = 8
          Top = 24
          Width = 28
          Height = 13
          Caption = 'Filter:'
        end
        object Label38: TLabel
          Left = 184
          Top = 24
          Width = 63
          Height = 13
          Caption = 'Window size:'
        end
        object ListBox2: TListBox
          Left = 56
          Top = 24
          Width = 113
          Height = 81
          ItemHeight = 13
          TabOrder = 0
          OnClick = Edit35Change
        end
        object Edit35: TEdit
          Left = 178
          Top = 40
          Width = 41
          Height = 21
          TabOrder = 1
          Text = '1'
          OnChange = Edit35Change
        end
        object UpDown24: TUpDown
          Left = 219
          Top = 40
          Width = 15
          Height = 21
          Associate = Edit35
          Min = 1
          Max = 64
          Position = 1
          TabOrder = 2
          Thousands = False
        end
      end
    end
    object tabRotate: TTabSheet
      Caption = 'Rotate'
      object LabelRotate: TLabel
        Left = 8
        Top = 23
        Width = 37
        Height = 13
        Caption = '&Rotate:'
        FocusControl = edtRotate
      end
      object lblFlip: TLabel
        Left = 8
        Top = 68
        Width = 20
        Height = 13
        Caption = 'Flip:'
      end
      object lblBackground: TLabel
        Left = 8
        Top = 128
        Width = 60
        Height = 13
        Caption = 'Background:'
        Visible = False
      end
      object TrackBarRotate: TTrackBar
        Left = 137
        Top = 17
        Width = 312
        Height = 31
        Max = 18000
        Min = -18000
        Frequency = 500
        TabOrder = 1
        OnChange = TrackBarRotateChange
      end
      object edtRotate: TEdit
        Left = 86
        Top = 20
        Width = 52
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = edtRotateChange
      end
      object chkFlipHorz: TCheckBox
        Left = 86
        Top = 68
        Width = 109
        Height = 17
        Caption = 'Horizontally'
        TabOrder = 2
        OnClick = chkFlipHorzClick
      end
      object chkFlipVert: TCheckBox
        Left = 86
        Top = 93
        Width = 109
        Height = 17
        Caption = 'Vertically'
        TabOrder = 3
        OnClick = chkFlipHorzClick
      end
      object pnlBackgroundColor: TPanel
        Left = 86
        Top = 125
        Width = 31
        Height = 20
        Color = clBlack
        ParentBackground = False
        TabOrder = 4
        Visible = False
        OnClick = pnlBackgroundColorClick
      end
    end
    object TabSheet12: TTabSheet
      Caption = 'FFT'
      object GroupBox8: TGroupBox
        Left = 8
        Top = 8
        Width = 233
        Height = 163
        Caption = ' Frequency domain image '
        TabOrder = 0
        object ImageEnView1: TImageEnView
          Left = 10
          Top = 18
          Width = 212
          Height = 135
          Background = clBtnFace
          ParentCtl3D = False
          BorderStyle = bsNone
          MouseInteract = [miZoom, miSelect]
          SelectionBase = iesbBitmap
          EnableInteractionHints = True
          TabOrder = 0
        end
        object pbrFFT: TProgressBar
          Left = 2
          Top = 151
          Width = 229
          Height = 10
          Align = alBottom
          TabOrder = 1
          Visible = False
        end
      end
      object Clear: TButton
        Left = 249
        Top = 13
        Width = 75
        Height = 25
        Caption = 'Clear'
        TabOrder = 1
        OnClick = ClearClick
      end
      object Button7: TButton
        Left = 249
        Top = 45
        Width = 75
        Height = 25
        Caption = 'Reset'
        TabOrder = 2
        OnClick = Button7Click
      end
      object CheckBox1: TCheckBox
        Left = 251
        Top = 85
        Width = 97
        Height = 17
        Caption = 'Gray scale'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = CheckBox1Click
      end
    end
    object tabGamma: TTabSheet
      Caption = 'Gamma Correction'
      object Label39: TLabel
        Left = 8
        Top = 28
        Width = 92
        Height = 13
        Caption = 'Gamma Correction:'
        FocusControl = edtGamma
      end
      object Label41: TLabel
        Left = 461
        Top = 60
        Width = 18
        Height = 13
        Caption = '255'
      end
      object Label42: TLabel
        Left = 111
        Top = 153
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label40: TLabel
        Left = 120
        Top = 61
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label43: TLabel
        Left = 100
        Top = 75
        Width = 18
        Height = 13
        Caption = '255'
      end
      object GroupBox9: TGroupBox
        Left = 8
        Top = 72
        Width = 74
        Height = 83
        Caption = 'Channels'
        TabOrder = 2
        object cbxGamma: TCheckListBox
          Left = 8
          Top = 24
          Width = 57
          Height = 44
          BorderStyle = bsNone
          Color = clBtnFace
          Ctl3D = True
          ItemHeight = 13
          Items.Strings = (
            'Red'
            'Green'
            'Blue')
          ParentCtl3D = False
          TabOrder = 0
          OnClick = cbxGammaClick
        end
      end
      object edtGamma: TEdit
        Left = 123
        Top = 25
        Width = 41
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
        Text = '0'
      end
      object trkGamma: TTrackBar
        Left = 168
        Top = 24
        Width = 313
        Height = 29
        Max = 100
        Min = 1
        Frequency = 10
        Position = 1
        TabOrder = 1
        OnChange = trkGammaChange
      end
      object ImageEnView2: TImageEnView
        Left = 120
        Top = 77
        Width = 361
        Height = 90
        Background = clBtnFace
        Ctl3D = False
        ParentCtl3D = False
        EnableInteractionHints = True
        TabOrder = 3
      end
    end
    object TabSheet14: TTabSheet
      Caption = 'Sharpen'
      object Label44: TLabel
        Left = 8
        Top = 12
        Width = 51
        Height = 13
        Caption = 'Amplitude:'
        FocusControl = Edit36
      end
      object Label45: TLabel
        Left = 8
        Top = 74
        Width = 63
        Height = 13
        Caption = 'Window size:'
      end
      object Edit36: TEdit
        Left = 13
        Top = 34
        Width = 33
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = Edit36Change
      end
      object TrackBar4: TTrackBar
        Left = 47
        Top = 34
        Width = 442
        Height = 28
        Max = 50
        Min = 1
        Position = 1
        TabOrder = 1
        OnChange = TrackBar4Change
      end
      object Edit37: TEdit
        Left = 13
        Top = 92
        Width = 41
        Height = 21
        TabOrder = 2
        Text = '1'
        OnChange = TrackBar4Change
      end
      object UpDown25: TUpDown
        Left = 54
        Top = 92
        Width = 15
        Height = 21
        Associate = Edit37
        Min = 1
        Max = 64
        Position = 1
        TabOrder = 3
        Thousands = False
      end
    end
    object tabResize: TTabSheet
      Caption = 'Resize'
      ImageIndex = 14
      object lblResize: TLabel
        Left = 8
        Top = 23
        Width = 68
        Height = 13
        Caption = 'Resize Image:'
        FocusControl = edtRotate
      end
      object lblWidth: TLabel
        Left = 27
        Top = 58
        Width = 70
        Height = 13
        Caption = 'Width (pixels):'
      end
      object lblHeight: TLabel
        Left = 27
        Top = 90
        Width = 73
        Height = 13
        Caption = 'Height (pixels):'
      end
      object lblCurrentSize: TLabel
        Left = 120
        Top = 31
        Width = 37
        Height = 13
        Caption = 'Current'
      end
      object lblNewSize: TLabel
        Left = 188
        Top = 31
        Width = 21
        Height = 13
        Caption = 'New'
      end
      object lblNewScale: TLabel
        Left = 256
        Top = 31
        Width = 25
        Height = 13
        Caption = 'Scale'
      end
      object lblP1: TLabel
        Left = 312
        Top = 58
        Width = 11
        Height = 13
        Caption = '%'
      end
      object lblP2: TLabel
        Left = 312
        Top = 90
        Width = 11
        Height = 13
        Caption = '%'
      end
      object edtOldWidth: TEdit
        Left = 120
        Top = 53
        Width = 49
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
      end
      object edtOldHeight: TEdit
        Left = 120
        Top = 85
        Width = 49
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 5
      end
      object chkMaintainAR: TCheckBox
        Left = 120
        Top = 118
        Width = 158
        Height = 17
        Caption = 'Maintain Aspect Ratio'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 10
        OnClick = edtNewWidthChange
      end
      object updNewWidth: TUpDown
        Left = 223
        Top = 53
        Width = 16
        Height = 21
        Associate = edtNewWidth
        Min = 1
        Max = 32767
        Increment = 100
        Position = 1
        TabOrder = 2
        Thousands = False
      end
      object edtNewWidth: TEdit
        Left = 188
        Top = 53
        Width = 35
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '1'
        OnChange = edtNewWidthChange
      end
      object updNewHeight: TUpDown
        Left = 223
        Top = 85
        Width = 16
        Height = 21
        Associate = edtNewHeight
        Min = 1
        Max = 32767
        Increment = 100
        Position = 1
        TabOrder = 7
        Thousands = False
      end
      object edtNewHeight: TEdit
        Left = 188
        Top = 85
        Width = 35
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        Text = '1'
        OnChange = edtNewHeightChange
      end
      object edtNewWidthPercent: TEdit
        Left = 256
        Top = 53
        Width = 35
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Text = '1'
        OnChange = edtNewWidthPercentChange
      end
      object updNewWidthPercent: TUpDown
        Left = 291
        Top = 53
        Width = 16
        Height = 21
        Associate = edtNewWidthPercent
        Min = 1
        Max = 10000
        Increment = 5
        Position = 1
        TabOrder = 4
        Thousands = False
      end
      object edtNewHeightPercent: TEdit
        Left = 256
        Top = 85
        Width = 35
        Height = 21
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        Text = '1'
        OnChange = edtNewHeightPercentChange
      end
      object updNewHeightPercent: TUpDown
        Left = 291
        Top = 85
        Width = 16
        Height = 21
        Associate = edtNewHeightPercent
        Min = 1
        Max = 10000
        Increment = 5
        Position = 1
        TabOrder = 9
        Thousands = False
      end
    end
    object tabSoftShadow: TTabSheet
      Caption = 'Soft Shadow'
      ImageIndex = 15
      object lblShadowRadius: TLabel
        Left = 39
        Top = 54
        Width = 36
        Height = 13
        Caption = 'Radius:'
      end
      object lblShadowOffset: TLabel
        Left = 39
        Top = 82
        Width = 35
        Height = 13
        Caption = 'Offset:'
      end
      object lblShadowColor: TLabel
        Left = 39
        Top = 109
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object lblAddSoftShadow: TLabel
        Left = 8
        Top = 23
        Width = 87
        Height = 13
        Caption = 'Add Soft Shadow:'
      end
      object lblShadowPosition: TLabel
        Left = 39
        Top = 137
        Width = 41
        Height = 13
        Caption = 'Position:'
      end
      object edtShadowRadius: TEdit
        Left = 90
        Top = 50
        Width = 33
        Height = 21
        TabOrder = 0
        Text = '3'
        OnChange = SoftShadowControlChange
      end
      object updShadowRadius: TUpDown
        Left = 123
        Top = 50
        Width = 16
        Height = 21
        Associate = edtShadowRadius
        Min = 1
        Position = 3
        TabOrder = 1
      end
      object edtShadowOffset: TEdit
        Left = 90
        Top = 78
        Width = 33
        Height = 21
        TabOrder = 2
        Text = '3'
        OnChange = SoftShadowControlChange
      end
      object updShadowOffset: TUpDown
        Left = 123
        Top = 78
        Width = 16
        Height = 21
        Associate = edtShadowOffset
        Min = -30
        Max = 30
        Position = 3
        TabOrder = 3
      end
      object pnlShadowColor: TPanel
        Left = 90
        Top = 105
        Width = 49
        Height = 20
        Color = clBlack
        ParentBackground = False
        TabOrder = 4
        OnClick = pnlBackgroundColorClick
      end
      object cmbShadowPosition: TComboBox
        Left = 90
        Top = 133
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 5
        OnChange = SoftShadowControlChange
        Items.Strings = (
          'Top Left'
          'Top Right'
          'Bottom Left'
          'Bottom Right'
          'All (Glow)')
      end
    end
  end
  object ResultToSourceButton: TBitBtn
    Left = 207
    Top = 74
    Width = 21
    Height = 25
    Hint = 'Copy Result to Source'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = ResultToSourceButtonClick
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000FAFAFAFAFAFA
      FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA409742409742FAFAFAFAFA
      FAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA
      FAFA40974265DD4A409742FAFAFAFAFAFAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFA
      FAFAFAFAFAFAFAFAFAFAFAFAFAFAFA40974263D9486CE04D409742FAFAFAFAFA
      FAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA40974263
      D94864DF486CE04D409742FAFAFAFAFAFAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFA
      FAFAFAFAFAFAFAFAFA4097425FD44763D94864DF486CE04D409742FAFAFAFAFA
      FAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFAFAFAFAFAFAFA4097425ED44764DF4863
      DF4664DF486CE04D409742FAFAFAFAFAFAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFA
      FAFAFA4097425ED64763DF4664DF4863DF4664DF486CE04D409742FAFAFAFAFA
      FAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFA4097425DCC4763DF4663DF4664DF4863
      DF4664DF486CE04D409742FAFAFAFAFAFAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFA
      4097425DCC4764DF4864DF4864DF4863DF4664DF486CE04D409742FAFAFAFAFA
      FAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFAFAFAFA4097425ECD4864DF4863DF4663
      DF4667E04B6FE051409742FAFAFAFAFAFAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFA
      FAFAFAFAFAFA4097425FD54863DF4664DF486EE04F72E154409742FAFAFAFAFA
      FAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA4097425FD54864
      DF4872E15478E25B409742FAFAFAFAFAFAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFA
      FAFAFAFAFAFAFAFAFAFAFAFA40974266D54C78E25B78E25B409742FAFAFAFAFA
      FAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA40
      974278E25B78E25B409742FAFAFAFAFAFAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFA
      FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA40974278E25B409742FAFAFAFAFA
      FAFAFAFAFAFAFAFFFFFFFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA
      FAFAFAFAFA409742409742FAFAFAFAFAFAFAFAFAFAFAFAFFFFFF}
    Spacing = 0
  end
  object PreviewButton: TBitBtn
    Left = 444
    Top = 102
    Width = 87
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Preview'
    TabOrder = 3
    OnClick = PreviewButtonClick
  end
  object ImageEn1: TImageEnView
    Left = 9
    Top = 21
    Width = 191
    Height = 135
    Cursor = 1782
    Background = clBtnFace
    ParentCtl3D = False
    OnViewChange = ImageEn1ViewChange
    MouseInteract = [miZoom, miScroll]
    EnableInteractionHints = True
    TabOrder = 4
  end
  object ImageEn2: TImageEnView
    Left = 233
    Top = 21
    Width = 191
    Height = 135
    Cursor = 1780
    Background = clBtnFace
    ParentCtl3D = False
    EnableInteractionHints = True
    TabOrder = 6
    OnMouseDown = ImageEn2MouseUp
    OnMouseUp = ImageEn2MouseUp
  end
  object chkLockPreview: TCheckBox
    Left = 9
    Top = 390
    Width = 211
    Height = 17
    Hint = 
      'Automatically updates your preview with the changes you have sel' +
      'ected'
    Anchors = [akLeft, akBottom]
    Caption = 'Lock Preview'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = chkLockPreviewClick
  end
  object ResetButton: TBitBtn
    Left = 444
    Top = 131
    Width = 87
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Reset'
    TabOrder = 5
    OnClick = ResetButtonClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'flt'
    Filter = 'Filter (*.flt)|*.flt'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Title = 'Load filter'
    Left = 256
    Top = 35
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'flt'
    Filter = 'Filter (*.flt)|*.flt'
    Options = [ofHideReadOnly, ofPathMustExist]
    Title = 'Save filter'
    Left = 320
    Top = 34
  end
  object ImageEnProc2: TImageEnProc
    AttachedImageEn = ImageEnView1
    Background = clBtnFace
    OnProgress = ImageEnProc2Progress
    PreviewsParams = [prppShowResetButton, prppHardReset]
    PreviewFont.Charset = DEFAULT_CHARSET
    PreviewFont.Color = clWindowText
    PreviewFont.Height = -11
    PreviewFont.Name = 'MS Sans Serif'
    PreviewFont.Style = []
    Left = 36
    Top = 32
  end
  object tmrUpdatePreview: TTimer
    Enabled = False
    Interval = 250
    OnTimer = tmrUpdatePreviewTimer
    Left = 136
    Top = 72
  end
end
