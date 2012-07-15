object FQTextBoxForm: TFQTextBoxForm
  Left = 310
  Top = 309
  AutoScroll = False
  ClientHeight = 213
  ClientWidth = 472
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 297
    Height = 13
    AutoSize = False
    Caption = 'Label1'
  end
  object Image1: TImage
    Left = 24
    Top = 168
    Width = 32
    Height = 32
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 457
    Height = 153
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button1: TButton
    Left = 170
    Top = 168
    Width = 137
    Height = 41
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
end
