object AutoUpdateInstaller: TAutoUpdateInstaller
  Left = 152
  Top = 123
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'QuArK - Online Update Installer'
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
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 152
    Top = 80
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object StopBtn: TButton
    Left = 192
    Top = 264
    Width = 105
    Height = 41
    Cancel = True
    Caption = 'Stop'
    TabOrder = 0
    OnClick = StopBtnClick
  end
  object pgbInstall: TProgressBar
    Left = 8
    Top = 160
    Width = 473
    Height = 49
    Step = 1
    TabOrder = 1
  end
end
