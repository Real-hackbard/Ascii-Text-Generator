object Form1: TForm1
  Left = 439
  Top = 217
  Caption = 'ASCii Text Generator'
  ClientHeight = 565
  ClientWidth = 718
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 546
    Width = 718
    Height = 19
    Panels = <
      item
        Text = 'Font Count :'
        Width = 75
      end
      item
        Width = 50
      end
      item
        Text = 'Selected :'
        Width = 65
      end
      item
        Text = '3D ASCII'
        Width = 200
      end
      item
        Text = 'Font Name :'
        Width = 75
      end
      item
        Text = 'Courier New'
        Width = 50
      end>
    ExplicitLeft = -1
    ExplicitWidth = 543
  end
  object Panel1: TPanel
    Left = 137
    Top = 0
    Width = 581
    Height = 546
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 152
    ExplicitTop = -6
    ExplicitWidth = 437
    object Memo1: TMemo
      Left = 1
      Top = 121
      Width = 579
      Height = 343
      TabStop = False
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'memTexte')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      ExplicitLeft = 198
      ExplicitTop = 198
      ExplicitWidth = 177
      ExplicitHeight = 125
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 579
      Height = 120
      Align = alTop
      TabOrder = 1
      ExplicitLeft = 49
      ExplicitTop = 33
      ExplicitWidth = 367
      object memTexte: TMemo
        Left = 1
        Top = 1
        Width = 394
        Height = 118
        TabStop = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'Hello'
          'World')
        ParentFont = False
        TabOrder = 0
        OnChange = memTexteChange
        ExplicitLeft = 57
        ExplicitTop = 18
        ExplicitWidth = 176
        ExplicitHeight = 101
      end
      object memInfosFont: TMemo
        Left = 395
        Top = 1
        Width = 183
        Height = 118
        TabStop = False
        Align = alRight
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = []
        Lines.Strings = (
          'memTexte')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
        ExplicitLeft = 183
        ExplicitTop = 18
        ExplicitHeight = 101
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 464
      Width = 579
      Height = 81
      Align = alBottom
      TabOrder = 2
      ExplicitLeft = 6
      ExplicitTop = 459
      object Label1: TLabel
        Left = 37
        Top = 22
        Width = 26
        Height = 13
        Caption = 'Size :'
      end
      object Shape1: TShape
        Left = 69
        Top = 45
        Width = 18
        Height = 22
        Cursor = crHandPoint
        OnMouseDown = Shape1MouseDown
      end
      object Label2: TLabel
        Left = 8
        Top = 48
        Width = 55
        Height = 13
        Caption = 'BGR Color :'
      end
      object btnCopy: TButton
        Left = 442
        Top = 44
        Width = 61
        Height = 25
        Caption = 'Copy'
        TabOrder = 0
        TabStop = False
        OnClick = btnCopyClick
      end
      object Button1: TButton
        Left = 509
        Top = 44
        Width = 60
        Height = 25
        Caption = 'Image'
        TabOrder = 1
        TabStop = False
        OnClick = Button1Click
      end
      object CheckBox1: TCheckBox
        Left = 126
        Top = 47
        Width = 82
        Height = 17
        TabStop = False
        Caption = 'Invert Image'
        TabOrder = 2
      end
      object SpinEdit1: TSpinEdit
        Left = 69
        Top = 17
        Width = 41
        Height = 22
        TabStop = False
        MaxLength = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = SpinEdit1Change
      end
      object Button2: TButton
        Left = 376
        Top = 44
        Width = 60
        Height = 25
        Caption = 'Remove #'
        TabOrder = 4
        TabStop = False
        OnClick = Button2Click
      end
      object Button4: TButton
        Left = 310
        Top = 44
        Width = 60
        Height = 25
        Caption = 'Font'
        TabOrder = 5
        TabStop = False
        OnClick = Button4Click
      end
      object CheckBox2: TCheckBox
        Left = 126
        Top = 21
        Width = 178
        Height = 17
        TabStop = False
        Caption = 'Transparent PNG (Color is Black)'
        TabOrder = 6
      end
    end
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 137
    Height = 546
    TabStop = False
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 2
    OnClick = ListBox1Click
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Left = 169
    Top = 168
  end
  object ColorDialog1: TColorDialog
    Left = 257
    Top = 168
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      'Bitmap (*.BMP)|*.bmp|Jpeg (*.JPG; *.JPEG()|*.jpg|Portable Networ' +
      'k Graphic (*.PNG)|*.png|Graphic for Intercharge (*.GIF)|*.gif'
    Left = 353
    Top = 168
  end
end
