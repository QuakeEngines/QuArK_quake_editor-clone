inherited FQMdl: TFQMdl
  Left = 287
  Top = 232
  PixelsPerInch = 96
  TextHeight = 13
  inherited leftdock: TDock97
    Top = 51
    Height = 208
  end
  inherited rightdock: TDock97
    Top = 51
    Height = 208
  end
  object Panel1: TPanel
    Left = 0
    Top = 13
    Width = 427
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    Color = clInactiveCaption
    TabOrder = 4
    object Label1: TLabel
      Left = 10
      Top = 1
      Width = 5
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clInactiveCaptionText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 17
      Top = 17
      Width = 51
      Height = 13
      Caption = 'File name :'
    end
    object Button1: TButton
      Left = 216
      Top = 6
      Width = 89
      Height = 25
      Caption = 'Open model'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = Button1Click
    end
    object EnterEdit1: TEnterEdit
      Left = 72
      Top = 14
      Width = 121
      Height = 21
      TabOrder = 1
      OnAccept = EnterEdit1Accept
    end
  end
  object Panel2: TPanel
    Left = 9
    Top = 51
    Width = 409
    Height = 208
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 5
  end
end
