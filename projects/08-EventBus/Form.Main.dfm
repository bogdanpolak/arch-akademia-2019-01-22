object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 343
  ClientWidth = 225
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
    Width = 219
    Height = 337
    Align = alClient
    Caption = 'GroupBox1'
    Padding.Top = 10
    TabOrder = 0
    ExplicitLeft = -2
    ExplicitTop = -2
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 61
      Width = 209
      Height = 3
      Margins.Top = 5
      Margins.Bottom = 5
      Align = alTop
      ExplicitLeft = 2
      ExplicitTop = 56
      ExplicitWidth = 173
    end
    object Bevel2: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 132
      Width = 209
      Height = 3
      Margins.Top = 5
      Margins.Bottom = 5
      Align = alTop
      ExplicitLeft = 3
      ExplicitTop = 161
      ExplicitWidth = 167
    end
    object Bevel3: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 168
      Width = 209
      Height = 3
      Margins.Top = 5
      Margins.Bottom = 5
      Align = alTop
      ExplicitLeft = 7
      ExplicitWidth = 194
    end
    object Bevel4: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 209
      Width = 209
      Height = 3
      Margins.Top = 5
      Margins.Bottom = 5
      Align = alTop
      ExplicitLeft = 7
      ExplicitTop = 222
      ExplicitWidth = 194
    end
    object btnPostMessage1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 99
      Width = 209
      Height = 25
      Align = alTop
      Caption = 'btnPostMessage1'
      TabOrder = 2
      OnClick = btnPostMessage1Click
    end
    object btnShowSubscribers: TButton
      AlignWithMargins = True
      Left = 5
      Top = 28
      Width = 209
      Height = 25
      Align = alTop
      Caption = 'btnShowSubscribers'
      TabOrder = 0
      OnClick = btnShowSubscribersClick
    end
    object Edit1: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 72
      Width = 209
      Height = 21
      Align = alTop
      TabOrder = 1
      Text = 'Wiadomo'#347#263' z okna g'#322#243'wnego'
      OnDblClick = Edit1DblClick
    end
    object chkFastAnimataion: TCheckBox
      AlignWithMargins = True
      Left = 17
      Top = 143
      Width = 197
      Height = 17
      Margins.Left = 15
      Align = alTop
      Caption = 'chkFastAnimataion (Message2)'
      TabOrder = 3
      OnClick = chkFastAnimataionClick
    end
    object btnExit: TButton
      AlignWithMargins = True
      Left = 5
      Top = 307
      Width = 209
      Height = 25
      Align = alBottom
      Caption = 'Exit'
      TabOrder = 6
      OnClick = btnExitClick
    end
    object ColorBox1: TColorBox
      AlignWithMargins = True
      Left = 17
      Top = 179
      Width = 185
      Height = 22
      Margins.Left = 15
      Margins.Right = 15
      Align = alTop
      TabOrder = 4
      OnChange = ColorBox1Change
    end
    object GridPanel1: TGridPanel
      Left = 2
      Top = 217
      Width = 215
      Height = 33
      Align = alTop
      Caption = 'GridPanel1'
      ColumnCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = btnPause
          Row = 0
        end
        item
          Column = 1
          Control = btnAnimate
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      ShowCaption = False
      TabOrder = 5
      object btnPause: TButton
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 100
        Height = 25
        Align = alClient
        Caption = 'btnPause'
        TabOrder = 0
        OnClick = btnPauseClick
      end
      object btnAnimate: TButton
        AlignWithMargins = True
        Left = 110
        Top = 4
        Width = 101
        Height = 25
        Align = alClient
        Caption = 'btnAnimate'
        TabOrder = 1
        OnClick = btnAnimateClick
      end
    end
  end
  object tmrReady: TTimer
    Interval = 1
    OnTimer = tmrReadyTimer
    Left = 16
    Top = 256
  end
end
