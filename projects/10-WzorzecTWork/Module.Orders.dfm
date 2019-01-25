object ModuleOrders: TModuleOrders
  OldCreateOrder = False
  Height = 150
  Width = 308
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    LoginPrompt = False
    Left = 52
    Top = 22
  end
  object fdqOrders: TFDQuery
    Connection = FDConnection1
    Left = 52
    Top = 78
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 168
    Top = 48
  end
end
