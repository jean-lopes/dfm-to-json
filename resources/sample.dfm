object Form1: TForm1
  Left = 192
  Top = 107
  Width = 463
  Height = 224
  Cursor = crHourGlass
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 240
    Top = 104
    Width = 97
    Height = 89
    Picture.Data = {
      07544269746D617036550000424D365500000000000036000000280000005500
      000055000000010018000000000000550000C40E0000C40E0000000000000000
    }
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 217
    Height = 73
    Color = 8454143
    ItemHeight = 13
    Items.Strings = 
      ( 'this is string.' 
      + 'and another one...'
      + 'and another one...'
      + 'now with apostrophe '''#13#10
      + 'even with control characters '#7#9'mixed in the string'
      )
    TabOrder = 1
  end
  object DBGrid1: TDBGrid
    Left = 240
    Top = 8
    Width = 209
    Height = 89
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        Visible = True
      end
      item
        Expanded = False
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 8
    Top = 104
    Width = 145
    Height = 57
    TabOrder = 2
    object CheckBox1: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'CheckBox2'
      TabOrder = 1
    end
  end
end
