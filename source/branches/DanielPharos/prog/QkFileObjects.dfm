object QForm1: TQForm1
  Left = 845
  Top = 669
  Width = 435
  Height = 295
  Caption = '(new)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object topdock: TDock97
    Left = 0
    Top = 0
    Width = 427
    Height = 13
    BoundLines = [blBottom]
    Color = 12632264
    object MenuToolbar: TToolbar97
      Left = 0
      Top = 0
      Width = 21
      Height = 12
      Caption = 'menu'
      CloseButton = False
      DefaultDock = topdock
      DockedTo = topdock
      DockPos = 0
      Visible = False
    end
  end
  object leftdock: TDock97
    Left = 0
    Top = 13
    Width = 9
    Height = 246
    BoundLines = [blRight]
    Color = 12632264
    Position = dpLeft
  end
  object rightdock: TDock97
    Left = 418
    Top = 13
    Width = 9
    Height = 246
    BoundLines = [blLeft]
    Color = 12632264
    Position = dpRight
  end
  object bottomdock: TDock97
    Left = 0
    Top = 259
    Width = 427
    Height = 9
    BoundLines = [blTop]
    Color = 12632264
    Position = dpBottom
  end
end
