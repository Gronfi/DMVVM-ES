program App.CSV.FMX;

uses
  System.StartUpCopy,
  System.Classes,
  FMX.Forms,

  MVVM.Core,
  MVVM.Utils,
  MVVM.Services.Platform.FMX,
  MVVM.Bindings.LiveBindings,
  MVVM.Bindings,
  MVVM.Interfaces,
  CSV.Interfaces,
  CSV.Model,
  CSV.ViewModel,
  CSV.View,
  CSV.RecursosEjercicio in '..\CSV.Resources.Common\CSV.RecursosEjercicio.pas';

{$R *.res}

begin
  Application.Initialize;
  InitializePlatform;

  MVVMCore.DefaultBindingStrategyName := 'LIVEBINDINGS';
  MVVMCore.DefaultViewPlatform        := 'WINDOWS_DESKTOP';

  MVVMCore.InitializationDone;

  Modelo      := TCSVFile_Model.Create;

  VistaModelo := TCSVFile_ViewModel.Create;
  VistaModelo.SetModel(Modelo);

  Vista := MVVMCore.ViewsProvider.CreateView<ICSVFile_ViewModel>(MVVMCore.DefaultViewPlatform, ICSVFile_View_NAME, nil, VistaModelo);
  Application.MainForm := TfrmCSV(Vista.GetAsObject);

  Utils.ShowView(Vista);

  Application.Run;
end.
