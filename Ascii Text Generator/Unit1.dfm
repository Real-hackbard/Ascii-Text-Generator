object Form1: TForm1
  Left = 439
  Top = 217
  Caption = 'ASCii Text Generator'
  ClientHeight = 589
  ClientWidth = 828
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 570
    Width = 828
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
    ExplicitTop = 569
    ExplicitWidth = 824
  end
  object Panel1: TPanel
    Left = 161
    Top = 0
    Width = 667
    Height = 570
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 663
    ExplicitHeight = 569
    object Memo1: TMemo
      Left = 0
      Top = 120
      Width = 667
      Height = 450
      TabStop = False
      Align = alClient
      BorderStyle = bsNone
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      ExplicitWidth = 663
      ExplicitHeight = 449
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 667
      Height = 120
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 663
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 340
        Height = 120
        TabStop = False
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
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
        OnChange = Memo2Change
        ExplicitWidth = 336
      end
      object Memo3: TMemo
        Left = 340
        Top = 0
        Width = 327
        Height = 120
        TabStop = False
        Align = alRight
        BevelOuter = bvNone
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 161
    Height = 570
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitHeight = 569
    DesignSize = (
      161
      570)
    object Label3: TLabel
      Left = 13
      Top = 349
      Width = 48
      Height = 13
      Caption = 'Font list:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ListBox1: TListBox
      Left = 10
      Top = 368
      Width = 135
      Height = 185
      TabStop = False
      Anchors = [akLeft, akTop, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
      OnClick = ListBox1Click
      ExplicitHeight = 184
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 161
      Height = 329
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Image1: TImage
        Left = 8
        Top = 20
        Width = 21
        Height = 19
        Picture.Data = {
          07544269746D617036030000424D360300000000000036000000280000001000
          000010000000010018000000000000030000120B0000120B0000000000000000
          0000FF00FF314B62AC7D7EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF5084B20F6FE1325F8CB87E7AFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FF32A0FE37A1FF106FE2325F8BB67D79FF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF37A4FE379FFF0E6DDE355F
          89BB7F79FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FF37A4FE359EFF0F6FDE35608BA67B7FFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF38A5FE329D
          FF156DCE444F5BFF00FF9C6B65AF887BAF887EAA8075FF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FF3BABFFA1CAE7AD8679A98373E0CFB1FFFFDA
          FFFFDDFCF8CFCCB29FA1746BFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFC0917DFCE9ACFFFFCCFFFFCFFFFFD0FFFFDEFFFFFAE3D3D1996965FF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB08978FAD192FEF4C2FFFFD0
          FFFFDAFFFFF6FFFFFCFFFFFCB69384FF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFB08978FEDA97EDB478FBEEBBFFFFD3FFFFDCFFFFF4FFFFF4FFFFE2E9DDBCA6
          7B73FF00FFFF00FFFF00FFFF00FFFF00FFB18A78FFDE99E9A167F4D199FEFCCC
          FFFFD5FFFFDAFFFFDCFFFFD7EFE6C5A97E75FF00FFFF00FFFF00FFFF00FFFF00
          FFAA7F73FAE0A4F0B778EEBA7BF6DDA6FEFBCCFFFFD3FFFFD1FFFFD7D9C5A7A3
          756CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEB293FFFEDDF4D1A5EEBA7B
          F2C78FF8E1ABFCF0BAFCFACAA3776FFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFA1746BE1D4D3FFFEEEF7CC8CF0B473F7C788FCE3A5C2A088A5776CFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF986865BA9587EAD7A4
          EAD59EE0C097A5776CA5776CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFA77E70A98073A4786EFF00FFFF00FFFF00FFFF
          00FF}
        Transparent = True
      end
      object Label1: TLabel
        Left = 40
        Top = 96
        Width = 26
        Height = 13
        Caption = 'Size :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label4: TLabel
        Left = 47
        Top = 124
        Width = 19
        Height = 13
        Caption = 'Bit :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 10
        Top = 157
        Width = 90
        Height = 13
        Caption = 'Move Text H/V :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 52
        Top = 179
        Width = 14
        Height = 13
        Caption = 'H :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 53
        Top = 207
        Width = 13
        Height = 13
        Caption = 'V :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 15
        Top = 242
        Width = 51
        Height = 13
        Caption = 'Replace :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label8: TLabel
        Left = 8
        Top = 296
        Width = 30
        Height = 13
        Caption = 'Char :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 70
        Top = 296
        Width = 16
        Height = 13
        Caption = '<>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label10: TLabel
        Left = 71
        Top = 269
        Width = 18
        Height = 13
        Caption = 'All :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Edit1: TEdit
        Left = 31
        Top = 20
        Width = 114
        Height = 19
        TabStop = False
        Ctl3D = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 0
        Text = 'search...'
        OnChange = Edit1Change
        OnClick = Edit1Click
      end
      object SpinEdit1: TSpinEdit
        Left = 72
        Top = 93
        Width = 73
        Height = 22
        TabStop = False
        MaxLength = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = SpinEdit1Change
      end
      object Button2: TButton
        Left = 58
        Top = 50
        Width = 25
        Height = 27
        Hint = 'Remove hash tags from text'
        Caption = '#'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        TabStop = False
        OnClick = Button2Click
      end
      object ComboBox2: TComboBox
        Left = 72
        Top = 121
        Width = 73
        Height = 21
        Style = csDropDownList
        ItemIndex = 4
        TabOrder = 3
        TabStop = False
        Text = '24 bit'
        Items.Strings = (
          '1 bit'
          '4 bit'
          '8 bit'
          '16 bit'
          '24 bit'
          '32 bit')
      end
      object BitBtn1: TBitBtn
        Left = 27
        Top = 50
        Width = 25
        Height = 27
        Hint = 'Copy ASCII text'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFB88989B88989B88989B88989B88989B88989B88989B889
          89B88989B88989FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB88989FEFEFDFE
          FEFEFEFEFDFEFEFDFEFEFDFEFEFDFEFEFDFEFEFDB88989FF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFB88989FEFBF8B27E73B27E73B27E73B27E73B27E73B27E
          73FEFBF8B88989FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB88989FEF8F3FE
          FAF6FEF8F3FEF8F3FEF8F3FEF8F3FEF8F3FEF8F3B88989FF00FFB88989B88989
          B88989B88989B88989B88989FEF6EDB27E73B27E73B27E73B27E73B27E73B27E
          73FEF6EDB88989FF00FFB88989FEFEFDFEFEFEFEFEFDFEFEFDB88989FEF3E8FF
          F4EAFEF3E8FEF3E8FEF3E8FEF3E8FEF3E8FEF3E8B88989FF00FFB88989FEFBF8
          B27E73B27E73B27E73B88989FFF0E3B27E73B27E73B27E73B27E73B27E73B27E
          73FFF0E3B88989FF00FFB88989FEF8F3FEFAF6FEF8F3FEF8F3B88989FFEDDDFF
          EDDDFFEDDDFFEDDDFFEDDDE9D5C9E7D6C9D7C5BAB88989FF00FFB88989FEF6ED
          B27E73B27E73B27E73B88989FFEBD8FFEAD7FFEBD8FFEBD8FFEBD8C4AAA7C5AB
          A8CDB5B0CD9999FF00FFB88989FEF3E8FFF4EAFEF3E8FEF3E8B88989FFE8D2FF
          E8D2FFE8D2FFE8D2FBE4CFC6ACA9FEFEFECD9999FF00FFFF00FFB88989FFF0E3
          B27E73B27E73B27E73B88989FFE6CFFFE6CFFFE6CFFFE6CFE9CFBFD2BAB4CD99
          99FF00FFFF00FFFF00FFB88989FFEDDDFFEDDDFFEDDDFFEDDDB88989B88989B8
          8989B88989B88989B88989CD9999FF00FFFF00FFFF00FFFF00FFB88989FFEBD8
          FFEAD7FFEBD8FFEBD8FFEBD8C4AAA7C5ABA8CDB5B0CD9999FF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFB88989FFE8D2FFE8D2FFE8D2FFE8D2FBE4CFC6ACA9FE
          FEFECD9999FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB88989FFE6CF
          FFE6CFFFE6CFFFE6CFE9CFBFD2BAB4CD9999FF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFB88989B88989B88989B88989B88989B88989CD9999FF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        TabStop = False
        OnClick = BitBtn1Click
      end
      object BitBtn2: TBitBtn
        Left = 89
        Top = 50
        Width = 25
        Height = 27
        Hint = 'Add text margin on right side for image'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FF044906055B09066C0C066C0C055E0A044C06FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF05600905600908911309B01809
          B31A09B31909B11907961405680C05680CFF00FFFF00FFFF00FFFF00FFFF00FF
          0A6A150A7F150BB61C09B91A08B41807B21609B31909B41909B81A09B91A0783
          10044D06FF00FFFF00FFFF00FF0B6A150F852216BD3411B7270BB21C07B11608
          B11709B21909B21909B21909B41909BA1A07841006670CFF00FFFF00FF0B6A15
          20BE491BBD4014B7300AB21F28BC36DFF5E1EEFAEF63CE6D09B21909B21909B3
          1909BA1A06670CFF00FF0872101B9A3A2AC65B1DBB450EB4250BB31B11B4219A
          DFA0FFFFFFF7FDF85ACB6509B21909B21909B81A089413045D090872102AB65B
          2CC56522BD4D0FB4220AB21A0CB31C0AB2198DDB95FDFEFDF6FCF758CB6309B2
          1909B51A08AB17045D090F821C37C26C33C76CCDF1DAC9EFD3C7EED0C8EFD2C5
          EED0C7EECFF8FDF9FFFFFFF2FBF36FD27908B41909B31905650B138D2358CC83
          42C977FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFEFDFFFFFFFFFFFFBCEA
          C10AB41A09B319066D0D0F911D6FD2935FD38D6DD49572D69971D69872D69964
          D28C92DFA8FBFEFBFFFFFFACE5B82EBF4C11B82B08B11905610A0F911D67CC83
          9BE5BA38C67030C36938C56F38C56F70D697E8F8EEFFFFFF9FE2B120BD481AB9
          3E10BA2908A31705610AFF00FF25AE39BCEDD282DBA428C0632FC26753CD82F7
          FDF9FFFFFF9CE2B222BC4B1DBA4118B73614C0300A8517FF00FFFF00FF25AE39
          71D28CD2F4E180DAA336C46D39C56FBCECCEABE6C22DC26324BE5623BC4D1FC1
          4616AE340A8517FF00FFFF00FFFF00FF25AE3984D89FDBF7EAAFE8C66BD49352
          CC8144C97849CA7B48CB7839CB6A21B6490F7C1FFF00FFFF00FFFF00FFFF00FF
          FF00FF25AE3925AE39ADE8C5CCF2DEBAEDD1A6E7C291E2B364D4922FB1572FB1
          57FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF32B74E25AE3925
          AE3925AE3925AE3924A342FF00FFFF00FFFF00FFFF00FFFF00FF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        TabStop = False
        OnClick = BitBtn2Click
      end
      object BitBtn3: TBitBtn
        Left = 120
        Top = 50
        Width = 25
        Height = 27
        Hint = 'Remove text margin on right side'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FF013002014103025104025104014303013302FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF014503014503037808039C0B03
          9F0C039F0C039D0C027E09014D04014D04FF00FFFF00FFFF00FFFF00FFFF00FF
          034F0903650904A30D03A60C03A00B029E0A039F0C03A00C03A50C03A60C0269
          06013402FF00FFFF00FFFF00FF044F09066B110AAB1F07A415049E0D029D0A03
          9D0A039E0C039E0C039E0C03A00C03A70C026A06024C04FF00FFFF00FF044F09
          10AC300DAB2809A41C039E0F48C052E9F8EAD5F2D816AA20039E0C039E0C039F
          0C03A70C024C04FF00FF0357060D822317B6410EA92D05A01341BD4BF4FCF6FF
          FFFF82D58907A010039E0C039E0C039E0C03A50C037B0801420303570617A341
          18B54A11AB3441BD4EF3FBF4FCFEFC75D07D039E0C039E0C039E0C039E0C039E
          0C03A10C03960A01420306680D21B1511EB75170D392F0FBF3FFFFFFF7FCF8B7
          E9C5B4E7BDADE5B2ADE5B2AFE5B4B1E6B603A00C039F0C014A040874123EBD69
          2ABA5CBAEACCFFFFFFFFFFFFFCFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FF03A00C039F0C02520506780E54C57A44C6742CBA5CA4E3BCFFFFFFFAFEFB82
          D9A03EBF5E41C05B41BF5B41BF5841BF5707A518039D0C01460306780E4CBD69
          83DDA722B6551DB24F95DEB0FFFFFFE2F7EA4FC67511AB340FAA300FAA2E0CA6
          2706A716038C0A014603FF00FF139923AAE7C568D08E16AF481CB14D8EDCABFF
          FFFFF4FCF72DB85310A9310EA7290BA42009AF1C036B0AFF00FFFF00FF139923
          56C573C5F0D866CF8C20B4521CB24F95DEB0ABE6C11FB44E13AC3C12AA340FB0
          2D0A991F036B0AFF00FFFF00FFFF00FF1399236ACC88D0F4E39AE1B650C77A38
          BD672CBA5D30BB602FBC5D23BC4F11A33006620FFF00FFFF00FFFF00FFFF00FF
          FF00FF13992313992398E1B5BDEED4A7E7C490E0B178D99F49C7791B9D3D1B9D
          3DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1DA43513992313
          9923139923139923138C2AFF00FFFF00FFFF00FFFF00FFFF00FF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        TabStop = False
        OnClick = BitBtn3Click
      end
      object BitBtn4: TBitBtn
        Left = 120
        Top = 266
        Width = 25
        Height = 21
        Hint = 'This replaces all characters in the ASCII text.'
        Glyph.Data = {
          76020000424D760200000000000036000000280000000C0000000C0000000100
          20000000000040020000120B0000120B00000000000000000000FFFFFF00FFFF
          FF00FFFFFF00E0EDE51F3BA05DBBECF3EF13FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FCFBFD0358AD6FA700A52BFF4BAA
          6BB4F5F6F60BFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF0082BB907D01A42CFE00BD41FF02A52DFD89BF9576FFFEFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00A1CBAF5E04A733FB00C844FF00CC49FF00C7
          45FF09A22EF6ADD2B652FFFFFF00FFFFFF00FFFFFF00FFFFFF00C9E5D03619B3
          45E600CB42FF00D048FF02C03DFD00D649FF00CD49FF0AAE41F5CCE1D334FFFF
          FF00FFFFFF00FFFFFF0035B25CC400C835FF00DB50FF0FC143F187C89B780AC7
          3FF500DA4BFF00CD42FF36B157CAE8F0EA17FFFFFF00FFFFFF009FD3AF5D0AC3
          42F500CA38FF80C69680FFFCFF0088CB997800CF3CFF00DD4EFF00CB39FF56BA
          71A9F1F3F20EFFFFFF00FFFEFF0069CA88984BBE6FB4F9F6F906FFFFFF00FCF7
          FB044BBD71B400DE44FF00E752FF00C73AFF7EC59582FFFDFF00FFFFFF00E6F1
          EA19E5F0EA1AFFFFFF00FFFFFF00FFFFFF00E8EEEC1733BB57CC00E548FF00E1
          4FFF03BF3DFCA6D9B557FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00D4E5D92B18C248E700EF54FF00DE40FF30B65CC8FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A6D2B55900D0
          40FF1ECE53E1CFE3D430FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFCFF0071C68E8884CE9E76FFFEFF00}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        TabStop = False
        OnClick = BitBtn4Click
      end
      object Edit2: TEdit
        Left = 95
        Top = 266
        Width = 20
        Height = 21
        TabStop = False
        MaxLength = 1
        TabOrder = 8
        Text = '*'
      end
      object BitBtn5: TBitBtn
        Left = 120
        Top = 239
        Width = 25
        Height = 21
        Hint = 'Undo'
        Glyph.Data = {
          06030000424D060300000000000036000000280000000F0000000F0000000100
          180000000000D0020000C30E0000C30E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFF9ECE9EFD3CCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFDFCFDF0D5CEF1B9A0E1A58BE2
          E1E1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF
          F2E5E3ECBDAAF7C09FFFE0C1E9B99EC1A29AF7F6F6FEFCFCFEFEFEFFFFFFFFFF
          FFFFFFFFFFFFFF000000FAF5F5F0CABDF1B797FCCDA9FFDBBAFFDBBEFCD8BFF2
          CBB5F5CEBBEDC1B2E2C9C1F0DDD8F9FAFAFFFFFFFFFFFF000000EEB59FFBC69F
          FFD3AEFFD4B3FFD7B9FFDABFFFE1C8FFE7D4FFECDBFFF5EAFFF4EDF0D4CCE0BC
          B2F1EAE9FFFFFF000000E19376FEC59EFFD8B3FFD6B3FFD8B8FFDBC0FFE1C9FF
          E7D5FFEEDEFFF3E9FFFBF7FFFFFFFBDFCBDAAA95F0EEEE000000D59E90DF5D37
          F7A985FFD5B3FFDCBEFFDDC2FED9C0FDD5BBFEDECBFEE9DCFEF6F2FFFFFDFFEB
          D3F0AD86D3B3A9000000F2F0EFCB9283D26241F1906AFDC8A8FFE3CAFDCEB2F9
          B48BF7AD85FAAE89FDB99BFDCEB9FED9BDF8AC83C89E91000000FFFFFFF9FEFF
          DACAC5CE856FE78762FEBE9CF6BB9DB88B7BBFAEA7C69D8FDB9277F2946FFCA8
          83F4956EC59C8F000000FFFFFFFFFFFFFFFFFFEFF2F3D2B0A6DD9273E8A27BC2
          A59CFEFFFFFDFFFFD8CCCBD46948F28760F68B65C69E93000000FFFFFFFFFFFF
          FFFFFFFFFFFFFDFFFFE4DDDBC3A196D0C9C6FFFFFFF8E4DEE8A289DE6742E46C
          48F68E68C8A297000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFF
          FFFFF3F2F3E6AD93EE8F66E2724EF9916ADB7F5FC0B0AB000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFF0E3A488F9976FF99B75D880
          61AF8F86EDF1F2000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFF9FDFEE3AE9BD4886AB7897ABEB3B1F0F4F5FFFFFF000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECEAEAD2D1D2EFF4F5FFFF
          FFFFFFFFFFFFFF000000}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
        TabStop = False
        OnClick = BitBtn5Click
      end
      object Edit3: TEdit
        Left = 44
        Top = 293
        Width = 20
        Height = 21
        TabStop = False
        MaxLength = 1
        TabOrder = 10
        Text = '\'
      end
      object Edit4: TEdit
        Left = 94
        Top = 293
        Width = 20
        Height = 21
        TabStop = False
        MaxLength = 1
        TabOrder = 11
        Text = '*'
      end
      object BitBtn6: TBitBtn
        Left = 120
        Top = 293
        Width = 25
        Height = 21
        Hint = 'This replaces all characters with the first one specified.'
        Glyph.Data = {
          76020000424D760200000000000036000000280000000C0000000C0000000100
          20000000000040020000120B0000120B00000000000000000000FFFFFF00FFFF
          FF00FFFFFF00E0EDE51F3BA05DBBECF3EF13FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FCFBFD0358AD6FA700A52BFF4BAA
          6BB4F5F6F60BFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF0082BB907D01A42CFE00BD41FF02A52DFD89BF9576FFFEFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00A1CBAF5E04A733FB00C844FF00CC49FF00C7
          45FF09A22EF6ADD2B652FFFFFF00FFFFFF00FFFFFF00FFFFFF00C9E5D03619B3
          45E600CB42FF00D048FF02C03DFD00D649FF00CD49FF0AAE41F5CCE1D334FFFF
          FF00FFFFFF00FFFFFF0035B25CC400C835FF00DB50FF0FC143F187C89B780AC7
          3FF500DA4BFF00CD42FF36B157CAE8F0EA17FFFFFF00FFFFFF009FD3AF5D0AC3
          42F500CA38FF80C69680FFFCFF0088CB997800CF3CFF00DD4EFF00CB39FF56BA
          71A9F1F3F20EFFFFFF00FFFEFF0069CA88984BBE6FB4F9F6F906FFFFFF00FCF7
          FB044BBD71B400DE44FF00E752FF00C73AFF7EC59582FFFDFF00FFFFFF00E6F1
          EA19E5F0EA1AFFFFFF00FFFFFF00FFFFFF00E8EEEC1733BB57CC00E548FF00E1
          4FFF03BF3DFCA6D9B557FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00D4E5D92B18C248E700EF54FF00DE40FF30B65CC8FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A6D2B55900D0
          40FF1ECE53E1CFE3D430FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFCFF0071C68E8884CE9E76FFFEFF00}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
        TabStop = False
        OnClick = BitBtn6Click
      end
      object SpinEdit2: TSpinEdit
        Left = 72
        Top = 176
        Width = 73
        Height = 22
        TabStop = False
        MaxValue = 1500
        MinValue = 0
        TabOrder = 13
        Value = 0
        OnChange = SpinEdit2Change
      end
      object SpinEdit3: TSpinEdit
        Left = 72
        Top = 204
        Width = 73
        Height = 22
        TabStop = False
        MaxValue = 1500
        MinValue = 0
        TabOrder = 14
        Value = 0
        OnChange = SpinEdit3Change
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Left = 225
    Top = 160
  end
  object ColorDialog1: TColorDialog
    Left = 313
    Top = 160
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      'Bitmap (*.BMP)|*.bmp|Jpeg (*.JPG; *.JPEG)|*.jpg|Portable Network' +
      ' Graphic (*.PNG)|*.png|Graphic for Intercharge (*.GIF)|*.gif'
    Left = 409
    Top = 160
  end
  object MainMenu1: TMainMenu
    Left = 497
    Top = 160
    object File1: TMenuItem
      Caption = 'File'
      object ExportPicture1: TMenuItem
        Caption = 'Export Picture..'
        OnClick = ExportPicture1Click
      end
      object ransparent1: TMenuItem
        AutoCheck = True
        Caption = 'Transparent'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Font1: TMenuItem
        Caption = 'Font'
        OnClick = Font1Click
      end
      object Remove1: TMenuItem
        Caption = 'Remove (#)'
        OnClick = Remove1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object BackgroundColor1: TMenuItem
        Caption = 'Background Color'
        OnClick = BackgroundColor1Click
      end
      object Invert1: TMenuItem
        AutoCheck = True
        Caption = 'Invert'
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object FontInformation1: TMenuItem
        AutoCheck = True
        Caption = 'Font Information'
        Checked = True
        OnClick = FontInformation1Click
      end
      object Panel3: TMenuItem
        AutoCheck = True
        Caption = 'Panel'
        Checked = True
        OnClick = Panel3Click
      end
    end
    object N2: TMenuItem
      Caption = '?'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
end
