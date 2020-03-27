program App.CSV.FMX;

uses
  System.StartUpCopy,
  FMX.Forms,

  MVVM.Core,
  MVVM.Services.Platform.FMX,
  MVVM.Bindings.LiveBindings,
  MVVM.Bindings in 'Source\MVVM.Bindings.pas',
  MVVM.Interfaces in 'Source\MVVM.Interfaces.pas',
  CSV.Interfaces in 'CSV.Interfaces.pas',
  CSV.Model in 'CSV.Model.pas',
  CSV.ViewModel in 'CSV.ViewModel.pas',
  CSV.View in 'CSV.View.pas' {frmCSV},
  CSV.RecursosEjercicio in '..\CSV.Resources.Common\CSV.RecursosEjercicio.pas';

{$R *.res}

begin
  Application.Initialize;
  InitializePlatform;

  MVVMCore.InitializationDone;
  MVVMCore.DefaultBindingStrategyName := 'LIVEBINDINGS';

  Modelo      := TCSVFile_Model.Create;

  VistaModelo := TCSVFile_ViewModel.Create;
  VistaModelo.SetModel(Modelo);

  Vista := TfrmCSV.Create(Application);
  Application.MainForm := TfrmCSV(Vista);
  Vista.InitView(VistaModelo);
  TfrmCSV(Vista).Show;

  Application.Run;
end.
