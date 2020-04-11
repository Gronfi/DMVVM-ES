program App.DataSet.FMX;

uses
  System.StartUpCopy,
  FMX.Forms,

  MVVM.Core,
  MVVM.Services.Platform.FMX,

  MVVM.Bindings.LiveBindings, //especial
  MVVM.Binding.LiveBindings.Controls.FMX,

  MVVM.Bindings in 'Source\MVVM.Bindings.pas',
  MVVM.Interfaces in 'Source\MVVM.Interfaces.pas',
  DataSet.Interfaces in 'DataSet.Interfaces.pas',
  DataSet.Model in 'DataSet.Model.pas',
  DataSet.ViewModel in 'DataSet.ViewModel.pas',
  DataSet.View.Desktop in 'DataSet.View.Desktop.pas' {frmDataSetDesktop},
  DataSet.Resources.Common in '..\DataSet.Resources.Common\DataSet.Resources.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  InitializePlatform;

  MVVMCore.InitializationDone;
  MVVMCore.DefaultBindingStrategyName := 'LIVEBINDINGS';

  Modelo          := TDataSet_Model.Create;
  Modelo.FileName := '..\..\..\Demos\Data\biolife.xml';

  VistaModelo := TDataSet_ViewModel.Create;
  VistaModelo.SetModel(Modelo);

  Vista := TfrmDataSetDesktop.Create(Application);
  Application.MainForm := TfrmDataSetDesktop(Vista);

  Vista.InitView(VistaModelo);
  TfrmDataSetDesktop(Vista).Show;

  Application.Run;
end.
