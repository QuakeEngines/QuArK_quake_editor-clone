inherited FQImages: TFQImages
  Left = 126
  Top = 208
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 9
    Top = 13
    Width = 409
    Height = 239
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clBlack
    TabOrder = 4
    object Panel2: TPanel
      Left = 0
      Top = 194
      Width = 405
      Height = 41
      Align = alBottom
      BevelInner = bvLowered
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 11
        Width = 26
        Height = 13
        Caption = 'Size :'
      end
      object EditSize: TEnterEdit
        Left = 40
        Top = 8
        Width = 73
        Height = 21
        TabOrder = 0
        OnAccept = EditSizeAccept
      end
      object Format8bits: TRadioButton
        Left = 136
        Top = 4
        Width = 137
        Height = 17
        Caption = '8-bit image with palette'
        TabOrder = 1
        OnClick = Format8bitsClick
      end
      object Format24bits: TRadioButton
        Left = 136
        Top = 21
        Width = 137
        Height = 17
        Caption = '24-bit true color image'
        TabOrder = 2
        OnClick = Format24bitsClick
      end
      object AlphaCB: TCheckBox
        Left = 288
        Top = 11
        Width = 153
        Height = 17
        Caption = 'Alpha map (transparency)'
        TabOrder = 3
        OnClick = AlphaCBClick
      end
    end
  end
end
