object DataModMain: TDataModMain
  OldCreateOrder = False
  Height = 207
  Width = 352
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Books')
    Connected = True
    LoginPrompt = False
    Left = 32
    Top = 8
  end
  object dsBooks: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM Books')
    Left = 112
    Top = 8
  end
  object dsReaders: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM Readers')
    Left = 112
    Top = 64
  end
  object dsReports: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM Reports')
    Left = 112
    Top = 120
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 208
    Top = 8
  end
end
