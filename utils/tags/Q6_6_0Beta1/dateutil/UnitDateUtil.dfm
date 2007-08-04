object FormDateUtil: TFormDateUtil
  Left = 192
  Top = 107
  Width = 217
  Height = 323
  Caption = 'DateUtil'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDay: TLabel
    Left = 80
    Top = 28
    Width = 22
    Height = 13
    Caption = 'Day:'
  end
  object LabelMonth: TLabel
    Left = 80
    Top = 60
    Width = 33
    Height = 13
    Caption = 'Month:'
  end
  object LabelYear: TLabel
    Left = 80
    Top = 92
    Width = 25
    Height = 13
    Caption = 'Year:'
  end
  object LabelResult: TLabel
    Left = 10
    Top = 160
    Width = 94
    Height = 13
    Caption = 'TDateTime number:'
  end
  object TextDay: TEdit
    Left = 120
    Top = 20
    Width = 32
    Height = 21
    MaxLength = 2
    TabOrder = 0
    OnChange = TextDayChange
  end
  object TextMonth: TEdit
    Left = 120
    Top = 52
    Width = 32
    Height = 21
    MaxLength = 2
    TabOrder = 1
    OnChange = TextMonthChange
  end
  object TextYear: TEdit
    Left = 120
    Top = 84
    Width = 32
    Height = 21
    MaxLength = 4
    TabOrder = 2
    OnChange = TextYearChange
  end
  object TextResult: TEdit
    Left = 120
    Top = 160
    Width = 64
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object ButtonExit: TButton
    Left = 24
    Top = 224
    Width = 161
    Height = 49
    Caption = 'Exit'
    TabOrder = 4
    OnClick = ButtonExitClick
  end
end
