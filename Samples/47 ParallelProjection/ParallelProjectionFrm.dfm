object ParallelProjectionForm: TParallelProjectionForm
  Left = 0
  Top = 0
  Caption = 'Parallel Projection. create by.qq600585'
  ClientHeight = 908
  ClientWidth = 1463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 41
    Width = 705
    Height = 727
    Align = alLeft
    Stretch = True
    ExplicitHeight = 778
  end
  object Image2: TImage
    Left = 708
    Top = 41
    Width = 755
    Height = 727
    Align = alClient
    Stretch = True
    ExplicitLeft = 416
    ExplicitTop = 48
    ExplicitWidth = 370
    ExplicitHeight = 273
  end
  object leftSplitter: TSplitter
    Left = 705
    Top = 41
    Height = 727
    AutoSnap = False
    ExplicitLeft = 464
    ExplicitTop = 200
    ExplicitHeight = 100
  end
  object Memo: TMemo
    Left = 0
    Top = 768
    Width = 1463
    Height = 140
    Align = alBottom
    Lines.Strings = (
      #20809#26629#24341#25806#20869#26680#40664#35748#25903#25345#24182#34892#21270#25237#24433','#30001#36171#20540#24320#20851' TRasterVertex.Parallel '#25511#21046
      
        #24403' TRasterVertex.Parallel = true '#26102#24182#34892#25237#24433#30340#25191#34892#26465#20214#30001' TRasterVertex.Parall' +
        'elWidthTrigger + TRasterVertex.ParallelHeightTrigger '#35302#21457
      #22312#22810#25968#24773#20917#19979','#25105#20204#19981#38656#35201#25805#24515#36825#20123#24320#20851#21644#26465#20214','#20869#37096#24050#32463#33258#21160#21270#22788#29702#36807#20102
      #39640#28165#22270#20687#24182#34892#25237#24433#30340#36895#24230#21462#20915#20110#20869#23384#39057#29575'+cpu'#39057#29575
      #24403#25105#20204#25237#24433#39640#28165#22270#20687','#31995#32479#20250#33258#21160#21551#21160#24182#34892#21270#25237#24433','#23567#22270#29255#21017#30452#25509#20018#34892#26041#24335#25237#24433
      'by.qq600585')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1463
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 344
    ExplicitTop = 256
    ExplicitWidth = 185
    object ProjButton: TButton
      Left = 8
      Top = 10
      Width = 89
      Height = 25
      Caption = #24182#34892#21270#25237#24433
      TabOrder = 0
      OnClick = ProjButtonClick
    end
    object CheckBox_bilinear_sampling: TCheckBox
      Left = 104
      Top = 16
      Width = 105
      Height = 17
      Caption = #22235#32447#24615#37319#26679
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 424
    Top = 256
  end
end
