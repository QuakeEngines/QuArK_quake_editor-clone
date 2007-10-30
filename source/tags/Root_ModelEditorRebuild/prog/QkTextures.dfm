inherited FQTexture: TFQTexture
  Left = 105
  Top = 262
  Width = 523
  Color = clBlack
  PixelsPerInch = 96
  TextHeight = 13
  inherited topdock: TDock97
    Width = 515
  end
  inherited rightdock: TDock97
    Left = 506
  end
  inherited bottomdock: TDock97
    Width = 515
  end
  object Panel1: TPanel
    Left = 9
    Top = 13
    Width = 497
    Height = 246
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object PaintPanel1: TPaintPanel
      Left = 0
      Top = 0
      Width = 497
      Height = 183
      OnPaint = PaintPanel1Paint
      Align = alClient
      ParentColor = True
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 0
      Top = 204
      Width = 497
      Height = 42
      Align = alBottom
      BevelOuter = bvLowered
      TabOrder = 1
      Visible = False
      object Label1: TLabel
        Left = 304
        Top = 2
        Width = 146
        Height = 13
        Caption = 'Next texture in anim. sequence'
      end
      object Label2: TLabel
        Left = 152
        Top = 2
        Width = 25
        Height = 13
        Caption = 'Flags'
      end
      object Label3: TLabel
        Left = 88
        Top = 2
        Width = 42
        Height = 13
        Caption = 'Contents'
      end
      object Label4: TLabel
        Left = 216
        Top = 2
        Width = 27
        Height = 13
        Caption = 'Value'
      end
      object BtnFlags: TToolbarButton97
        Left = 271
        Top = 12
        Width = 26
        Height = 26
        Glyph.Data = {
          06010000424D0601000000000000760000002800000010000000120000000100
          04000000000090000000CE0E0000D80E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEE0000EEEEE
          00EEE2220FF00000A0EE22220F2FF0AAA0EEE220FF8F8F00A0EEEE00F7FF7F8F
          00EEE030FFF7FF8F8F0EE0BB00FFF7FF7F0E0FBBBB00FFF7FF0E00FB8BB30FFF
          F00EEE008BBBB00FF00EEEE0BBBBBB300A0EEEE0BBB38BBB300EEE0BBBB30FBB
          0AA0EE0BBB30A00F0AA0E0BBBB30AAA0AAA0E0FBB30AAAAAAAA0EE00FB0AAAA0
          0000EEEE0000000EEEEE}
        OnClick = BtnFlagsClick
      end
      object Label5: TLabel
        Left = 14
        Top = 2
        Width = 22
        Height = 13
        Caption = 'Path'
      end
      object Anim: TEnterEdit
        Left = 312
        Top = 16
        Width = 113
        Height = 21
        TabOrder = 4
        OnAccept = ContentsAccept
      end
      object Contents: TEnterEdit
        Left = 79
        Top = 16
        Width = 60
        Height = 21
        TabOrder = 1
        OnAccept = ContentsAccept
      end
      object Flags: TEnterEdit
        Left = 143
        Top = 16
        Width = 60
        Height = 21
        TabOrder = 2
        OnAccept = ContentsAccept
      end
      object Value: TEnterEdit
        Left = 207
        Top = 16
        Width = 60
        Height = 21
        TabOrder = 3
        OnAccept = ContentsAccept
      end
      object Path: TEnterEdit
        Left = 6
        Top = 16
        Width = 65
        Height = 21
        TabOrder = 0
        OnAccept = ContentsAccept
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 183
      Width = 497
      Height = 21
      Align = alBottom
      BevelOuter = bvLowered
      TabOrder = 2
      Visible = False
      object Label6: TLabel
        Left = 8
        Top = 4
        Width = 32
        Height = 13
        Caption = 'Link to'
      end
      object Label7: TLabel
        Left = 160
        Top = 4
        Width = 51
        Height = 13
        Caption = 'in directory'
      end
      object Label8: TLabel
        Left = 296
        Top = 4
        Width = 41
        Height = 13
        Caption = 'in maps/'
      end
      object Label9: TLabel
        Left = 422
        Top = 4
        Width = 20
        Height = 13
        Caption = '.bsp'
      end
      object LinkTo: TEnterEdit
        Left = 47
        Top = 0
        Width = 106
        Height = 21
        TabOrder = 0
        OnAccept = LinkToAccept
      end
      object BaseDir: TEnterEdit
        Left = 218
        Top = 0
        Width = 66
        Height = 21
        TabOrder = 1
        OnAccept = BaseDirAccept
      end
      object SrcBsp: TEnterEdit
        Left = 340
        Top = 0
        Width = 80
        Height = 21
        TabOrder = 2
        OnAccept = SrcBspAccept
      end
    end
  end
end
