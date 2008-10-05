inherited QSprForm: TQSprForm
  Left = 748
  Top = 549
  Width = 532
  Height = 415
  Caption = 'QSprForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited topdock: TDock97
    Width = 524
  end
  inherited leftdock: TDock97
    Height = 359
  end
  inherited rightdock: TDock97
    Left = 515
    Height = 359
  end
  inherited bottomdock: TDock97
    Top = 372
    Width = 524
  end
  object Panel1: TPanel
    Left = 9
    Top = 13
    Width = 506
    Height = 359
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 4
    object ListView1: TListView
      Left = 0
      Top = 101
      Width = 506
      Height = 258
      Align = alClient
      Columns = <
        item
          Caption = 'Frame Number:'
          Width = 86
        end
        item
          Caption = 'Frame Name / Type:'
          Width = 110
        end
        item
          Caption = 'X Origin:'
          Width = 52
        end
        item
          Caption = 'Y Origin:'
          Width = 52
        end
        item
          Caption = 'Width:'
          Width = 43
        end
        item
          Caption = 'Height:'
          Width = 46
        end
        item
          Caption = 'Image Date Size:'
          Width = 93
        end>
      ReadOnly = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object QSplitter1: TQSplitter
      Left = 0
      Top = 98
      Width = 506
      Height = 3
      Cursor = crVSplit
      Align = alTop
      OnResized = QSplitter1Resized
      Orientation = soHorizontal
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 506
      Height = 98
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 2
      object Label1: TLabel
        Left = 59
        Top = 10
        Width = 27
        Height = 13
        Caption = 'Type:'
      end
      object Label2: TLabel
        Left = 20
        Top = 34
        Width = 66
        Height = 13
        Caption = 'Texture Type:'
      end
      object ComboBox2: TComboBox
        Left = 96
        Top = 12
        Width = 153
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = ComboBox2Change
        Items.Strings = (
          'SPR_VP_PARALLEL_UPRIGHT'
          'SPR_FACING_UPRIGHT'
          'SPR_VP_PARALLEL'
          'SPR_ORIENTED'
          'SPR_VP_PARALLEL_ORIENTED')
      end
      object ComboBox3: TComboBox
        Left = 96
        Top = 36
        Width = 153
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = ComboBox3Change
        Items.Strings = (
          'SPR_NORMAL'
          'SPR_ADDITIVE'
          'SPR_INDEXALPHA'
          'SPR_ALPHTEST')
      end
      object Panel3: TPanel
        Left = 1
        Top = 65
        Width = 504
        Height = 32
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object ffw: TToolbarButton97
          Tag = 100
          Left = 89
          Top = 5
          Width = 24
          Height = 24
          Hint = 'Go to last frame'
          Glyph.Data = {
            42010000424D4201000000000000760000002800000014000000110000000100
            040000000000CC00000000000000000000001000000010000000000000000000
            BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007FFFFFFFFFFF
            FFFFFFF700008777777777777777777F00008777777777777777777F00008777
            777777777777777F00008777707777707777777F00008777700777700777777F
            00008777700077700077777F00008777700007700007777F0000877770000070
            0000777F00008777700007700007777F00008777700077700077777F00008777
            700777700777777F00008777707777707777777F00008777777777777777777F
            00008777777777777777777F00008777777777777777777F0000788888888888
            888888870000}
          ParentShowHint = False
          ShowHint = True
          OnClick = playClick
        end
        object play: TToolbarButton97
          Tag = 1
          Left = 65
          Top = 5
          Width = 24
          Height = 24
          Hint = 'Go to next frame'
          DisplayMode = dmGlyphOnly
          Glyph.Data = {
            42010000424D4201000000000000760000002800000014000000110000000100
            040000000000CC00000000000000000000001000000010000000000000000000
            BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007FFFFFFFFFFF
            FFFFFFF700008777777777777777777F00008777777777777777777F00008777
            777077777777777F00008777777007777777777F00008777777000777777777F
            00008777777000077777777F00008777777000007777777F0000877777700000
            0777777F00008777777000007777777F00008777777000077777777F00008777
            777000777777777F00008777777007777777777F00008777777077777777777F
            00008777777777777777777F00008777777777777777777F0000788888888888
            888888870000}
          Opaque = False
          ParentShowHint = False
          ShowHint = True
          Spacing = 0
          OnClick = playClick
        end
        object back: TToolbarButton97
          Tag = -1
          Left = 41
          Top = 5
          Width = 24
          Height = 24
          Hint = 'Go to pervious frame'
          Glyph.Data = {
            42010000424D4201000000000000760000002800000014000000110000000100
            040000000000CC00000000000000000000001000000010000000000000000000
            BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007FFFFFFFFFFF
            FFFFFFF700008777777777777777777F00008777777777777777777F00008777
            777777770777777F00008777777777700777777F00008777777777000777777F
            00008777777770000777777F00008777777700000777777F0000877777700000
            0777777F00008777777700000777777F00008777777770000777777F00008777
            777777000777777F00008777777777700777777F00008777777777770777777F
            00008777777777777777777F00008777777777777777777F0000788888888888
            888888870000}
          ParentShowHint = False
          ShowHint = True
          OnClick = playClick
        end
        object rew: TToolbarButton97
          Tag = -100
          Left = 17
          Top = 5
          Width = 24
          Height = 24
          Hint = 'Go to first frame'
          Glyph.Data = {
            42010000424D4201000000000000760000002800000014000000110000000100
            040000000000CC00000000000000000000001000000010000000000000000000
            BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007FFFFFFFFFFF
            FFFFFFF700008777777777777777777F00008777777777777777777F00008777
            777777777777777F00008777777707777707777F00008777777007777007777F
            00008777770007770007777F00008777700007700007777F0000877700000700
            0007777F00008777700007700007777F00008777770007770007777F00008777
            777007777007777F00008777777707777707777F00008777777777777777777F
            00008777777777777777777F00008777777777777777777F0000788888888888
            888888870000}
          ParentShowHint = False
          ShowHint = True
          OnClick = playClick
        end
        object Bevel1: TBevel
          Left = 0
          Top = 0
          Width = 504
          Height = 2
          Align = alTop
          Shape = bsTopLine
        end
        object Label3: TLabel
          Left = 120
          Top = 10
          Width = 55
          Height = 13
          Caption = 'Frame 1 / 1'
        end
      end
    end
  end
end
