object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 353
  ClientWidth = 508
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 248
    Top = 192
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object tmrAnimate: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrAnimateTimer
    Left = 32
    Top = 16
  end
end
