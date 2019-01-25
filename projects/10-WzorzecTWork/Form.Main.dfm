object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 286
  ClientWidth = 415
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
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 265
    Height = 280
    Align = alLeft
    Caption = 'GroupBox1'
    TabOrder = 0
    object lblResults: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 80
      Width = 255
      Height = 13
      Align = alTop
      Caption = 'lblResults'
      WordWrap = True
      ExplicitWidth = 45
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 255
      Height = 25
      Align = alTop
      Caption = 'Button1'
      TabOrder = 0
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 49
      Width = 255
      Height = 25
      Align = alTop
      Caption = 'Button2'
      TabOrder = 1
    end
  end
end
