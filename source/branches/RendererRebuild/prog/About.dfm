object AboutBox: TAboutBox
  Left = 153
  Top = 130
  HelpContext = -1
  ActiveControl = Edit1
  BorderStyle = bsDialog
  ClientHeight = 352
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 737
    Height = 337
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object Image1: TImage
      Left = 8
      Top = 8
      Width = 374
      Height = 297
    end
    object Bevel1: TBevel
      Left = 392
      Top = 8
      Width = 17
      Height = 321
      Shape = bsLeftLine
    end
    object ProgramIcon: TImage
      Left = 412
      Top = 29
      Width = 32
      Height = 32
      Stretch = True
      IsControl = True
    end
    object ProductName1: TLabel
      Left = 492
      Top = 27
      Width = 17
      Height = 13
      Caption = 'Qu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object ProductName2: TLabel
      Left = 508
      Top = 27
      Width = 22
      Height = 13
      Caption = 'ake'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ProductName3: TLabel
      Left = 536
      Top = 27
      Width = 13
      Height = 13
      Caption = 'Ar'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ProductName4: TLabel
      Left = 548
      Top = 27
      Width = 16
      Height = 13
      Caption = 'my'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ProductName5: TLabel
      Left = 570
      Top = 27
      Width = 9
      Height = 13
      Caption = 'K'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ProductName6: TLabel
      Left = 577
      Top = 27
      Width = 22
      Height = 13
      Caption = 'nife'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Version: TLabel
      Left = 461
      Top = 45
      Width = 172
      Height = 20
      Alignment = taCenter
      AutoSize = False
      IsControl = True
    end
    object Label1: TLabel
      Left = 28
      Top = 314
      Width = 329
      Height = 11
      Caption = 
        'Logo designed by leonard "paniq" ritter - graphics and icons by ' +
        'gryphon and paniq'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object WebsiteAddress: TLabel
      Left = 472
      Top = 256
      Width = 2
      Height = 11
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Copyright: TLabel
      Left = 408
      Top = 65
      Width = 3
      Height = 13
    end
    object UsedCompilerLabel: TLabel
      Left = 454
      Top = 7
      Width = 2
      Height = 11
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object RepositoryAddress: TLabel
      Left = 472
      Top = 272
      Width = 2
      Height = 11
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 408
      Top = 272
      Width = 53
      Height = 13
      Caption = 'Repository:'
    end
    object Label2: TLabel
      Left = 408
      Top = 256
      Width = 56
      Height = 13
      Caption = 'HomePage:'
    end
    object Label4: TLabel
      Left = 408
      Top = 288
      Width = 120
      Height = 13
      Caption = 'QuArK Resource Forums:'
    end
    object ForumAddress: TLabel
      Left = 536
      Top = 288
      Width = 2
      Height = 11
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Memo1: TMemo
      Left = 408
      Top = 83
      Width = 313
      Height = 166
      ParentColor = True
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object OKButton: TButton
    Left = 511
    Top = 312
    Width = 75
    Height = 24
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object Edit1: TEdit
    Left = 64
    Top = 440
    Width = 265
    Height = 21
    TabOrder = 2
  end
end
