object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #22522#20110'ZDB'#30340#25968#25454#37319#26679#19982#20998#26512
  ClientHeight = 591
  ClientWidth = 745
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object CreateRandDataButton: TButton
    Left = 24
    Top = 8
    Width = 169
    Height = 33
    Caption = #21019#24314'50'#26465#38543#26426#25968#25454
    TabOrder = 0
    OnClick = CreateRandDataButtonClick
  end
  object Memo1: TMemo
    Left = 24
    Top = 96
    Width = 681
    Height = 473
    Lines.Strings = (
      #27492'Demo'#28436#31034#20102#22914#20309#20174'ZDB'#26412#22320#36827#34892#37319#26679
      ''
      #37319#26679#27493#39588#65306
      '1'#65292#23558#25968#25454#36880#26465#37319#26679#25104#22810#32500#21160#24577#25968#32452#65292#37319#26679#37117#20351#29992'ZDB'#20869#37096#30340#26597#35810#20989#25968
      '2'#65292#32479#35745#26426#22120#20154#37117#26159#22522#20110'KDTree'#23545#25968#25454#36827#34892#32447#24615#21270#23398#20064
      '3'#65292#25512#29702#26426#22120#20154#26159#23398#20064#20840#37096#30340#37319#26679#20869#23481#65292#25968#25454#36234#22810#65292#25512#29702#26426#22120#20154#36234#32874#26126
      ''
      #32479#35745#65306
      #22522#20110'KDTree'#23545#25968#25454#36880#26465#37319#26679#23436#25104#21518#65292#36827#34892#32447#24615#21270#24402#31867
      #28982#21518#20877#25171#21360#21508#20010#24402#31867
      ''
      #25512#29702#65306
      #25512#29702#20351#29992#30340'LBFGS'#31639#27861#65288'BFGS'#26159'4'#20010#20316#32773#30340#33521#25991#39318#23383#27597#65292'L'#26159#20195#34920#20248#21270#65289
      'LBFGS'#31639#27861#35831#33258#34892#23398#20064
      'LBFGS'#38656#35201#22823#37327#30693#35782#24211#65292#23398#20064#25968#25454#26465#30446#36234#22810#65292#25512#29702#20063#23601#36234#32874#26126
      ''
      'by.qq600585'
      '2018-4')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 24
    Top = 48
    Width = 121
    Height = 33
    Caption = #32479#35745':'#27599#26376#28040#36153#33021#21147
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 151
    Top = 48
    Width = 121
    Height = 33
    Caption = #32479#35745':'#23384#27454#19982#28040#36153
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 278
    Top = 48
    Width = 121
    Height = 33
    Caption = #32479#35745':'#24180#40836#27573
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 405
    Top = 48
    Width = 121
    Height = 33
    Caption = #32479#35745':'#32508#21512
    TabOrder = 5
    OnClick = Button5Click
  end
  object Edit1: TLabeledEdit
    Left = 355
    Top = 14
    Width = 59
    Height = 21
    EditLabel.Width = 40
    EditLabel.Height = 13
    EditLabel.Caption = #24180#40836#27573':'
    LabelPosition = lpLeft
    TabOrder = 6
    Text = '20.5'
  end
  object Button6: TButton
    Left = 420
    Top = 8
    Width = 75
    Height = 33
    Caption = #25512#29702
    TabOrder = 7
    OnClick = Button6Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 224
    Top = 224
  end
end
