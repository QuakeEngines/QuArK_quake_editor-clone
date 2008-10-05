object HelpPopup: THelpPopup
  Left = 50
  Top = 430
  Width = 304
  Height = 245
  Hint = 'Push me or press F1 to access infobase page'
  BorderStyle = bsSizeToolWin
  Caption = 'HelpPopup'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = True
  OnClick = FormClicked
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 296
    Height = 180
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clInfoText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    WantReturns = False
    OnClick = FormClicked
  end
  object Button1: TButton
    Left = 224
    Top = 198
    Width = 51
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'InfoBase'
    TabOrder = 1
    OnClick = OkBtnClick
  end
  object ActionList1: TActionList
    Left = 192
    Top = 96
  end
end
