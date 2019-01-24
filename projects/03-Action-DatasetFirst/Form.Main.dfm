object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 404
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    545
    404)
  PixelsPerInch = 96
  TextHeight = 13
  object DBNavigator1: TDBNavigator
    Left = 79
    Top = 88
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object DBEdit1: TDBEdit
    Left = 8
    Top = 119
    Width = 240
    Height = 21
    DataField = 'CompanyName'
    DataSource = DataSource1
    TabOrder = 2
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 146
    Width = 529
    Height = 250
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 8
    Top = 88
    Width = 65
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 352
    Top = 8
    Width = 185
    Height = 132
    Caption = 'GroupBox1'
    TabOrder = 4
    object btnDataSourceRemove: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 175
      Height = 25
      Align = alTop
      Caption = 'btnDataSourceRemove'
      TabOrder = 0
      OnClick = btnDataSourceRemoveClick
    end
    object btnDataSourceCreate: TButton
      AlignWithMargins = True
      Left = 5
      Top = 49
      Width = 175
      Height = 25
      Align = alTop
      Caption = 'btnDataSourceCreate'
      TabOrder = 1
      OnClick = btnDataSourceCreateClick
    end
    object btnBindAction: TButton
      AlignWithMargins = True
      Left = 5
      Top = 80
      Width = 175
      Height = 25
      Align = alTop
      Caption = 'btnBindAction'
      TabOrder = 2
      OnClick = btnBindActionClick
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    LoginPrompt = False
    Left = 88
    Top = 8
  end
  object fdqCustomers: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM Customers')
    Left = 168
    Top = 8
  end
  object tmrReady: TTimer
    Interval = 1
    OnTimer = tmrReadyTimer
    Left = 16
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = fdqCustomers
    Left = 248
    Top = 8
  end
end
