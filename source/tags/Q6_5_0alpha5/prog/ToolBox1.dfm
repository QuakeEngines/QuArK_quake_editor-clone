object ToolBoxForm: TToolBoxForm
  Left = 172
  Top = 117
  Width = 473
  Height = 312
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object bottomdock: TDock97
    Left = 0
    Top = 276
    Width = 465
    Height = 9
    BoundLines = [blTop]
    Color = 12632264
    Position = dpBottom
  end
  object PanelBig: TPanel
    Left = 9
    Top = 25
    Width = 447
    Height = 251
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 447
      Height = 251
      Align = alClient
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
    end
  end
  object topdock: TDock97
    Left = 0
    Top = 0
    Width = 465
    Height = 25
    BoundLines = [blBottom]
    Color = 12632264
    object Toolbar972: TToolbar97
      Left = 0
      Top = 0
      Width = 233
      Height = 24
      Caption = 'menu'
      CloseButton = False
      DockedTo = topdock
      DockPos = 0
      object Window1: TToolbarButton97
        Left = 117
        Top = 2
        Width = 64
        Height = 20
        DropdownArrow = False
        DropdownMenu = Form1.WindowMenu
        Caption = '&Toolboxes'
      end
      object TopBtn: TToolbarButton97
        Left = 181
        Top = 2
        Width = 50
        Height = 20
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'Top'
        Glyph.Data = {
          06010000424D060100000000000076000000280000000B000000120000000100
          04000000000090000000CE0E0000D80E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEE0EEEEE0
          0000EEEE030EEEE00000EEE0BB30EEE00000EE0BBBB30EE00000E0BBBBBB30E0
          00000000BBB000000000EEE0BBB0EEE00000EEE0BBB0EEE00000EEE0BBB0EEE0
          0000EEE0BBA0EEE00000EEE0AAA0EEE00000EEE0AAA0EEE000000000AAA00000
          0000E0FAAAAAA0E00000EE0FAAAA0EE00000EEE0FAA0EEE00000EEEE0F0EEEE0
          0000EEEEE0EEEEE00000}
      end
      object Edit1: TToolbarButton97
        Left = 81
        Top = 2
        Width = 36
        Height = 20
        DropdownArrow = False
        DropdownMenu = Form1.EditMenu
        Caption = '&Edit'
      end
      object Folder1: TToolbarButton97
        Left = 11
        Top = 2
        Width = 70
        Height = 20
        DropdownArrow = False
        DropdownMenu = FolderMenu
        Caption = '&Folders'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000CE0E0000D80E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00999999999999
          9999900000000000000988888888888888098FB7B7B7B7B7B8098F7B7B7B7B7B
          78098FB7B7B7B7B7B8098F7B7B7B7B7B78098FB7B7B7B7B7B8098F7B7B7B7B7B
          78098FB7B7B7B7B8B88B8FFFFFFFF8B8B8BB87B7B7B7888BBB88987B7B780BBB
          BBBB998888899B8BBB889999999998B8B8B8999999999B88B8BB}
      end
    end
  end
  object rightdock: TDock97
    Left = 456
    Top = 25
    Width = 9
    Height = 251
    BoundLines = [blLeft]
    Color = 12632264
    Position = dpRight
  end
  object leftdock: TDock97
    Left = 0
    Top = 25
    Width = 9
    Height = 251
    BoundLines = [blRight]
    Color = 12632264
    Position = dpLeft
  end
  object FolderMenu: TPopupMenu
    OnPopup = Folder1Click
    Left = 377
    Top = 217
    object Newfolder1: TMenuItem
      Tag = 1179471444
      Caption = 'New &main folder...'
      OnClick = FolderMacroClick
    end
    object Newsubfolder1: TMenuItem
      Tag = 1179864660
      Caption = 'New &sub-folder'
      ShortCut = 16454
      OnClick = FolderMacroClick
    end
    object Deletefolder1: TMenuItem
      Tag = 4605012
      Caption = '&Delete folder'
      OnClick = FolderMacroClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object EditDescription1: TMenuItem
      Caption = 'Item &Description...'
      ShortCut = 16452
      OnClick = EditDescription1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Reloadfolders1: TMenuItem
      Tag = 1279611476
      Caption = '&Reload folders'
      OnClick = FolderMacroClick
    end
    object Addons1: TMenuItem
      Tag = 1329873985
      Caption = '&Add-ons...'
      OnClick = FolderMacroClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CloseToolbox1: TMenuItem
      Caption = '&Close Toolbox'
      ShortCut = 16465
      OnClick = CloseToolbox1Click
    end
  end
end
