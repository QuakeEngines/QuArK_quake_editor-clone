object ConsoleForm: TConsoleForm
  Left = 200
  Top = 104
  Width = 435
  Height = 300
  Color = clBlack
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnResize = Panel1Resize
  PixelsPerInch = 96
  TextHeight = 13
  object Display: TCursorScrollBox
    Left = 0
    Top = 0
    Width = 427
    Height = 252
    Cursor = crIBeam
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    TabOrder = 0
    OnMouseDown = DisplayMouseDown
    OnMouseMove = DisplayMouseMove
    OnMouseUp = DisplayMouseUp
    DisplayHPos = 0
    DisplayVPos = 0
    OnPaint = DisplayPaint
  end
  object Notebook1: TNotebook
    Left = 0
    Top = 252
    Width = 427
    Height = 21
    Align = alBottom
    Color = clBtnFace
    PageIndex = 1
    ParentColor = False
    TabOrder = 1
    object TPage
      Left = 0
      Top = 0
      Caption = 'Run'
      object ToolbarButton971: TToolbarButton97
        Left = 403
        Top = 0
        Width = 23
        Height = 21
        Enabled = False
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000CE0E0000D80E00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FF00FFFF
          00FFFFFF00FF00FFFF00FFFFFF00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFFFF00FF00FFFFFF00FF00FFFFFF00FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FF
          00FFFFFF00FF00FFFFFF00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FFFFFFFFFF00FF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FFFF00FFFF00FFFF00FF
          FFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FFFFFFFFFF00FF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FF
          00FFFFFF00FF00FFFFFF00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFFFF00FF00FFFFFF00FF00FFFFFF00FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FF00FFFF
          00FFFFFF00FF00FFFF00FFFFFF00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFF00FF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        OnClick = ToolbarButton971Click
      end
      object ComboBox1: TComboBox
        Left = 0
        Top = 0
        Width = 402
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Edit'
      object EnterComboBox1: TEnterComboBox
        Left = 0
        Top = 0
        Width = 427
        Height = 23
        Color = clBlack
        Font.Color = clWhite
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 15
        ParentFont = False
        TabOrder = 0
        Color2 = clGreen
        OnAccept = EnterEdit1Accept
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 32
    OnTimer = Timer1Timer
    Left = 250
    Top = 90
  end
end
