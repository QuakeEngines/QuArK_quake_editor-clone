inherited FQQuakeC: TFQQuakeC
  Color = clWindow
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel8: TPanel [0]
    Left = 9
    Top = 13
    Width = 409
    Height = 246
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    Caption = 'Panel8'
    TabOrder = 0
    object LabelErreur: TLabel
      Left = 2
      Top = 230
      Width = 405
      Height = 14
      Align = alBottom
      AutoSize = False
      Color = clRed
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object CodeEditor: TSyntaxMemo
      Left = 2
      Top = 2
      Width = 405
      Height = 228
      Align = alClient
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      HideSelection = False
      Lines.Strings = (
        'CodeEditor')
      ParentFont = False
      TabOrder = 0
      OnChange = CodeEditorChange
      OnFormatLine = CodeEditorFormatLine
    end
  end
end
