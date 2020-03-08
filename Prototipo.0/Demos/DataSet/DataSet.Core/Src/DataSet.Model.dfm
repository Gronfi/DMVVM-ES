object DataSet_Model: TDataSet_Model
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object cdsSource: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 24
    Top = 16
  end
end
