object AutoUpdater: TAutoUpdater
  Left = 152
  Top = 123
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'QuArK - Online Update'
  ClientHeight = 316
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 24
    Top = 8
    Width = 445
    Height = 26
    Caption = 
      'One or more updates of QuArK were found online. Please select wh' +
      'ich should be downloaded and installed.'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 256
    Top = 48
    Width = 217
    Height = 201
    Caption = 'Package Information'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 201
      Height = 177
      AutoSize = False
      Caption = 'Description'
      WordWrap = True
    end
  end
  object OKBtn: TButton
    Left = 120
    Top = 264
    Width = 105
    Height = 41
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 272
    Top = 264
    Width = 105
    Height = 41
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelBtnClick
  end
  object CheckListBox1: TCheckListBox
    Left = 24
    Top = 48
    Width = 217
    Height = 201
    ItemHeight = 13
    TabOrder = 3
    OnClick = CheckListBox1Click
  end
end
