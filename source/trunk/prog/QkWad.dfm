inherited FQWad: TFQWad
  Color = clBlack
  PixelsPerInch = 96
  TextHeight = 13
  inherited ListView1: TListView
    Color = clBlack
    Font.Color = clGray
    LargeImages = ImageList1
    ParentFont = False
    ViewStyle = vsIcon
  end
  object ImageList1: TImageList
    Height = 64
    Masked = False
    Width = 64
    Left = 320
    Top = 208
  end
  object TimerAnimation: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerAnimationTimer
    Left = 280
    Top = 208
  end
end
