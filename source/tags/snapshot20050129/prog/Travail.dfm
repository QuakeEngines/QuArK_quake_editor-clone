object FormTravail: TFormTravail
  Left = 200
  Top = 104
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 129
  ClientWidth = 297
  Color = clActiveCaption
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 113
    TabOrder = 0
    object LabelProgress: TLabel
      Left = 8
      Top = 11
      Width = 265
      Height = 33
      Alignment = taCenter
      AutoSize = False
      WordWrap = True
    end
    object Label1: TLabel
      Left = 2
      Top = 41
      Width = 32
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0%'
    end
    object ProgressBar1: TProgressBar
      Left = 40
      Top = 40
      Width = 217
      Height = 15
      Min = 0
      Max = 100
      TabOrder = 0
    end
    object ButtonStop: TPanel
      Left = 168
      Top = 81
      Width = 73
      Height = 23
      Caption = 'Interrupt'
      ParentColor = True
      TabOrder = 1
    end
  end
end
