object UndoDlg: TUndoDlg
  Left = 98
  Top = 197
  Width = 221
  Height = 247
  HelpContext = 415
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsSizeToolWin
  Caption = 'Undo / Redo'
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 213
    Height = 25
    Align = alTop
    Style = bsRaised
  end
  object TLabel
    Left = 9
    Top = 6
    Width = 67
    Height = 13
    Caption = 'Modified files :'
  end
  object Panel1: TPanel
    Left = 0
    Top = 25
    Width = 213
    Height = 64
    Align = alTop
    BorderWidth = 1
    Color = clGreen
    TabOrder = 0
    object Label1: TLabel
      Left = 2
      Top = 2
      Width = 209
      Height = 14
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Redo from :'
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
    object ListBox1: TListBox
      Tag = 1
      Left = 2
      Top = 16
      Width = 209
      Height = 46
      Align = alClient
      ExtendedSelect = False
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
      OnClick = ListBox1Click
      OnMouseDown = ListBoxMouseDown
      OnMouseMove = ListBoxMouseMove
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 89
    Width = 213
    Height = 131
    Align = alClient
    BorderWidth = 1
    Caption = 'Panel2'
    Color = clMaroon
    TabOrder = 1
    object Label2: TLabel
      Left = 2
      Top = 2
      Width = 209
      Height = 15
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Undo up to :'
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
    object ListBox2: TListBox
      Tag = -1
      Left = 2
      Top = 17
      Width = 209
      Height = 112
      Align = alClient
      ExtendedSelect = False
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
      OnClick = ListBox2Click
      OnMouseDown = ListBoxMouseDown
      OnMouseMove = ListBoxMouseMove
    end
  end
  object ComboBox1: TComboBox
    Left = 84
    Top = 2
    Width = 127
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnClick = ComboBox1Click
  end
end
