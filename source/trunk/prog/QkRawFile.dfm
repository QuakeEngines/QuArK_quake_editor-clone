inherited FQRawFile: TFQRawFile
  Top = 146
  Height = 363
  PixelsPerInch = 96
  TextHeight = 13
  inherited leftdock: TDock97
    Height = 314
  end
  inherited rightdock: TDock97
    Height = 314
  end
  inherited bottomdock: TDock97
    Top = 327
  end
  object Panel1: TPanel
    Left = 9
    Top = 13
    Width = 409
    Height = 314
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 4
    OnResize = Panel1Resize
    object GroupBox1: TGroupBox
      Left = 2
      Top = 253
      Width = 405
      Height = 59
      Align = alBottom
      Caption = 'Play'
      TabOrder = 0
      object BackBtn: TToolbarButton97
        Left = 74
        Top = 20
        Width = 28
        Height = 28
        Glyph.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          01000000000040000000C40E0000C40E0000000000000000000000000000FFFF
          FF00FFFF0000FFFF00003F7E00003E7C00003C78000038700000306000002040
          00002040000030600000387000003C7800003E7C00003F7E0000FFFF0000FFFF
          0000}
        OnClick = BackBtnClick
      end
      object StopBtn: TToolbarButton97
        Left = 42
        Top = 20
        Width = 28
        Height = 28
        Glyph.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          01000000000040000000C40E0000C40E0000000000000000000000000000FFFF
          FF00FFFF0000FFFF0000FFFF0000E0070000E0070000E0070000E0070000E007
          0000E0070000E0070000E0070000E0070000E0070000FFFF0000FFFF0000FFFF
          0000}
        OnClick = StopBtnClick
      end
      object PlayBtn: TToolbarButton97
        Left = 14
        Top = 20
        Width = 28
        Height = 28
        Glyph.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          01000000000040000000C40E0000C40E0000000000000000000000000000FFFF
          FF00F3FF0000F1FF0000F0FF0000F07F0000F03F0000F01F0000F00F0000F007
          0000F0070000F00F0000F01F0000F03F0000F07F0000F0FF0000F1FF0000F3FF
          0000}
        OnClick = PlayBtnClick
      end
      object TrackBar1: TTrackBar
        Left = 107
        Top = 17
        Width = 288
        Height = 40
        Orientation = trHorizontal
        Frequency = 1
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 0
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = TrackBar1Change
      end
    end
    object Box1: TPaintPanel
      Left = 2
      Top = 2
      Width = 405
      Height = 251
      OnPaint = Box1Paint
      Align = alClient
      Color = clInactiveCaption
      TabOrder = 1
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 357
    Top = 201
  end
end
