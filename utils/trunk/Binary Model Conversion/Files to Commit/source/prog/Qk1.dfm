object Form1: TForm1
  Left = 149
  Top = 101
  Width = 500
  Height = 350
  Caption = 'QuArK Explorer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object leftdock: TDock97
    Left = 0
    Top = 25
    Width = 9
    Height = 289
    BoundLines = [blRight]
    Color = 12632264
    Position = dpLeft
  end
  object rightdock: TDock97
    Left = 483
    Top = 25
    Width = 9
    Height = 289
    BoundLines = [blLeft]
    Color = 12632264
    Position = dpRight
  end
  object bottomdock: TDock97
    Left = 0
    Top = 314
    Width = 492
    Height = 9
    BoundLines = [blTop]
    Color = 12632264
    Position = dpBottom
  end
  object Panel3: TPanel
    Left = 9
    Top = 25
    Width = 474
    Height = 289
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 474
      Height = 289
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object topdock: TDock97
    Left = 0
    Top = 0
    Width = 492
    Height = 25
    BoundLines = [blBottom]
    Color = 12632264
    object ToolbarMenu1: TToolbar97
      Left = 0
      Top = 0
      Width = 172
      Height = 24
      Caption = 'menu'
      CloseButton = False
      DefaultDock = topdock
      DockedTo = topdock
      DockPos = 0
      object Edit1: TToolbarButton97
        Left = 47
        Top = 2
        Width = 36
        Height = 20
        DropdownArrow = False
        DropdownMenu = EditMenu
        Caption = '&Edit'
      end
      object File1: TToolbarButton97
        Left = 11
        Top = 2
        Width = 36
        Height = 20
        DropdownArrow = False
        DropdownMenu = FileMenu
        Caption = '&File'
      end
      object Window1: TToolbarButton97
        Left = 83
        Top = 2
        Width = 64
        Height = 20
        DropdownArrow = False
        DropdownMenu = WindowMenu
        Caption = '&Toolboxes'
      end
      object Help1: TToolbarButton97
        Left = 147
        Top = 2
        Width = 23
        Height = 20
        DropdownArrow = False
        DropdownMenu = HelpMenu
        Caption = '&?'
      end
    end
    object ToolbarMenu2: TToolbar97
      Left = 431
      Top = 0
      Width = 61
      Height = 24
      Caption = 'menu 2'
      DefaultDock = topdock
      DockedTo = topdock
      DockPos = 431
      object Games1: TToolbarButton97
        Left = 11
        Top = 2
        Width = 48
        Height = 20
        DropdownArrow = False
        DropdownMenu = GamesMenu
        Caption = '&Games'
      end
    end
  end
  object FileMenu: TPopupMenu
    OnPopup = File1Click
    Left = 120
    Top = 208
    object News1: TMenuItem
      OnClick = News1Click
    end
    object Open1: TMenuItem
      Caption = '&Open...'
      ShortCut = 16463
      OnClick = Open1Click
    end
    object MdlImportFrom1: TMenuItem
      Caption = 'Model Importers'
      object mdlimpempty1: TMenuItem
        Caption = '(mdlimpempty)'
        Enabled = False
      end
    end
    object ConvertFrom1: TMenuItem
      Caption = 'Convert From'
      object empty1: TMenuItem
        Caption = '(empty)'
        Enabled = False
      end
    end
    object Save1: TMenuItem
      Tag = 1
      Caption = '&Save'
      ShortCut = 16467
      OnClick = Save1Click
    end
    object Saveinnewentry1: TMenuItem
      Caption = 'Save in QuArK &Explorer'
      OnClick = Saveinnewentry1Click
    end
    object Saveasfile1: TMenuItem
      Tag = 2
      Caption = 'Save &as file...'
      OnClick = Save1Click
    end
    object SSep1: TMenuItem
      Caption = '-'
    end
    object Saveall1: TMenuItem
      Caption = 'Save a&ll'
      OnClick = Saveall1Click
    end
    object Saveentryasfile1: TMenuItem
      Caption = 'Save object as &file...'
      OnClick = Saveentryasfile1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Close1: TMenuItem
      Caption = '&Close'
      ShortCut = 16465
      OnClick = Close1Click
    end
    object FileSep1: TMenuItem
      Caption = '-'
    end
    object N5: TMenuItem
      Caption = '-'
      OnClick = RecentFileClick
    end
    object N6: TMenuItem
      Caption = '-'
      OnClick = RecentFileClick
    end
    object N7: TMenuItem
      Caption = '-'
      OnClick = RecentFileClick
    end
    object N8: TMenuItem
      Caption = '-'
      OnClick = RecentFileClick
    end
    object N9: TMenuItem
      Caption = '-'
      OnClick = RecentFileClick
    end
  end
  object EditMenu: TPopupMenu
    OnPopup = Edit1Click
    Left = 184
    Top = 208
    object Undo1: TMenuItem
      Tag = 1329876565
      ShortCut = 16474
      OnClick = EditMacroClick
    end
    object Redo1: TMenuItem
      Tag = 1329874258
      ShortCut = 24666
      OnClick = EditMacroClick
    end
    object UndoRedo1: TMenuItem
      Tag = 1146246477
      Caption = 'U&ndo / Redo...'
      OnClick = EditMacroClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Tag = 4098
      Caption = '&Cut'
      ShortCut = 16472
      OnClick = EditMenuItemClick
    end
    object Copy1: TMenuItem
      Tag = 4100
      Caption = 'Cop&y'
      ShortCut = 16451
      OnClick = EditMenuItemClick
    end
    object Paste1: TMenuItem
      Tag = 64
      Caption = '&Paste'
      ShortCut = 16470
      OnClick = EditMenuItemClick
    end
    object Delete1: TMenuItem
      Tag = 4112
      Caption = '&Delete'
      ShortCut = 16473
      OnClick = EditMenuItemClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Copyas1: TMenuItem
      Caption = 'Copy object &as'
      object N2: TMenuItem
        Caption = '-'
      end
    end
    object PasteObj1: TMenuItem
      Caption = 'Paste &special...'
      OnClick = PasteObj1Click
    end
    object Importfromfile1: TMenuItem
      Caption = '&Import files'
      OnClick = Importfromfile1Click
      object Importfiles1: TMenuItem
        Caption = '&Import (copy) files...'
        OnClick = Makefilelink1Click
      end
      object Makefilelinks1: TMenuItem
        Caption = '&Make file links...'
        OnClick = Makefilelink1Click
      end
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object OpenSel1: TMenuItem
      Tag = 32
      Caption = '&Open selection...'
      Default = True
      OnClick = EditMenuItemClick
    end
    object ExtEdit1: TMenuItem
      Tag = 1163155525
      Caption = 'E&xternal editor'
      OnClick = EditMacroClick
    end
    object Properties1: TMenuItem
      Tag = 1347375696
      Caption = 'Object p&roperties...'
      GroupIndex = 7
      OnClick = EditMacroClick
    end
    object N11: TMenuItem
      Caption = '-'
      GroupIndex = 7
    end
    object Configuration1: TMenuItem
      Tag = 1145521731
      Caption = 'Confi&guration...'
      GroupIndex = 7
      OnClick = EditMacroClick
    end
  end
  object WindowMenu: TPopupMenu
    OnPopup = WindowMenuPopup
    Left = 248
    Top = 208
    object TbList1: TMenuItem
      Caption = '-'
      GroupIndex = 7
    end
    object MainWindow1: TMenuItem
      Caption = '&Main window'
      GroupIndex = 7
      RadioItem = True
      OnClick = MainWindow1Click
    end
    object WinList1: TMenuItem
      Caption = '-'
      GroupIndex = 7
    end
    object Minimize1: TMenuItem
      Caption = 'Minimi&ze window'
      GroupIndex = 7
      OnClick = Minimize1Click
    end
  end
  object GamesMenu: TPopupMenu
    OnPopup = Games1Click
    Left = 392
    Top = 208
    object GameSep1: TMenuItem
      Caption = '-'
    end
    object Go1: TMenuItem
      Caption = '&Go'
    end
    object N13: TMenuItem
      Caption = '-'
    end
    object Options2: TMenuItem
      Caption = '&Configuration...'
      OnClick = Options2Click
    end
    object Addons1: TMenuItem
      Caption = '&Add-ons...'
      OnClick = Addons1Click
    end
    object Outputdirectories1: TMenuItem
      Caption = 'O&utput directories...'
      OnClick = Outputdirectories1Click
    end
  end
  object ObjMenu: TPopupMenu
    Left = 49
    Top = 209
    object ObjSep1: TMenuItem
      Caption = '-'
    end
    object Cut2: TMenuItem
      Tag = 4098
      Caption = '&Cut'
      ShortCut = 16472
      OnClick = EditMenuItemClick
    end
    object Copy2: TMenuItem
      Tag = 4100
      Caption = 'Co&py'
      ShortCut = 16451
      OnClick = EditMenuItemClick
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object OpenSel2: TMenuItem
      Tag = 32
      Caption = '&Open...'
      Default = True
      OnClick = EditMenuItemClick
    end
    object Properties2: TMenuItem
      Tag = 1347375696
      Caption = 'P&roperties...'
      OnClick = EditMacroClick
    end
  end
  object HelpMenu: TPopupMenu
    Left = 321
    Top = 209
    object Viewconsole1: TMenuItem
      Caption = '&View console...'
      OnClick = Viewconsole1Click
    end
    object About1: TMenuItem
      Caption = '&About...'
      OnClick = About1Click
    end
  end
end
