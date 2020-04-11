object DataSet_Model: TDataSet_Model
  OldCreateOrder = False
  Height = 174
  Width = 267
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=D:\998.Test.Programacion\MVVM\Prototipo.0\Demos\Data\bd' +
        '.db'
      'DriverID=SQLite')
    Connected = True
    Left = 24
    Top = 16
  end
  object FDTable1: TFDTable
    Connection = FDConnection1
    UpdateOptions.UpdateTableName = 'Personas'
    TableName = 'Personas'
    Left = 24
    Top = 80
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 144
    Top = 16
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 144
    Top = 88
  end
end
