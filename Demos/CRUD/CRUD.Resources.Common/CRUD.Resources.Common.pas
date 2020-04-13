unit CRUD.Resources.Common;

interface

uses
  MVVM.Interfaces.Architectural,
  DataSet.Interfaces,
  DataSet.Model;

var
  Modelo        : TDataSet_Model;
  VistaModelo   : IDataSet_ViewModel;
  VistaTabla    : IView<IDataSet_ViewModel>;

implementation

end.
