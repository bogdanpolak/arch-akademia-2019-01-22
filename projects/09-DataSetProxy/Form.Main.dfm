object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 318
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 292
    Top = 0
    Width = 5
    Height = 318
    ExplicitLeft = 191
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 286
    Height = 312
    Align = alLeft
    Caption = 'GroupBox1'
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 276
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitWidth = 161
    end
    object ListBox1: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 80
      Width = 276
      Height = 227
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
      OnClick = ListBox1Click
      ExplicitLeft = 21
      ExplicitTop = 119
      ExplicitWidth = 161
      ExplicitHeight = 242
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 49
      Width = 276
      Height = 25
      Align = alTop
      Caption = 'Button2'
      TabOrder = 2
      OnClick = Button2Click
      ExplicitLeft = 104
      ExplicitTop = 144
      ExplicitWidth = 75
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Books')
    Left = 392
    Top = 24
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 392
    Top = 80
  end
end
