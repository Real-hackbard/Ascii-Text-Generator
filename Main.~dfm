object Form1: TForm1
  Left = 439
  Top = 217
  Width = 548
  Height = 459
  Caption = 'ASCii Text Generator 1.0'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblFontSize: TLabel
    Left = 504
    Top = 383
    Width = 6
    Height = 13
    Alignment = taCenter
    Caption = '8'
    Layout = tlCenter
  end
  object memTexte: TMemo
    Left = 112
    Top = 8
    Width = 121
    Height = 65
    Lines.Strings = (
      'Hello'
      'World')
    TabOrder = 0
    OnChange = memTexteChange
  end
  object memInfosFont: TMemo
    Left = 240
    Top = 8
    Width = 281
    Height = 65
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'memTexte')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object lstFonts: TListBox
    Left = 8
    Top = 8
    Width = 94
    Height = 369
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 2
    OnClick = lstFontsClick
  end
  object memResultat: TMemo
    Left = 112
    Top = 80
    Width = 409
    Height = 289
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'memTexte')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object btnCopy: TButton
    Left = 8
    Top = 380
    Width = 180
    Height = 23
    Caption = 'Copy to Clipboard'
    TabOrder = 4
    OnClick = btnCopyClick
  end
  object trkFontSize: TTrackBar
    Tag = 1
    Left = 200
    Top = 376
    Width = 297
    Height = 34
    Max = 20
    Min = 2
    PageSize = 1
    Position = 8
    TabOrder = 5
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = trkFontSizeChange
  end
end
