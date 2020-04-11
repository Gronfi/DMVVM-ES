program App.CSV.FMX.NoPackages;

uses
  System.StartUpCopy,
  FMX.Forms,
  CSV.Interfaces in '..\CSV.Core\Src\CSV.Interfaces.pas',
  CSV.Model in '..\CSV.Core\Src\CSV.Model.pas',
  CSV.ViewModel in '..\CSV.Core\Src\CSV.ViewModel.pas',
  CSV.RecursosEjercicio in '..\CSV.Resources.Common\CSV.RecursosEjercicio.pas',
  CSV.View in '..\CSV.View.FMX\Src\CSV.View.pas' {frmCSV},
  MVVM.Bindings.Collections in '..\..\..\Source\MVVM.Core\Src\MVVM.Bindings.Collections.pas',
  MVVM.Bindings.Commands in '..\..\..\Source\MVVM.Core\Src\MVVM.Bindings.Commands.pas',
  MVVM.Bindings in '..\..\..\Source\MVVM.Core\Src\MVVM.Bindings.pas',
  MVVM.Core in '..\..\..\Source\MVVM.Core\Src\MVVM.Core.pas',
  MVVM.Interfaces in '..\..\..\Source\MVVM.Core\Src\MVVM.Interfaces.pas',
  MVVM.Observable in '..\..\..\Source\MVVM.Core\Src\MVVM.Observable.pas',
  MVVM.Types in '..\..\..\Source\MVVM.Core\Src\MVVM.Types.pas',
  MVVM.Utils in '..\..\..\Source\MVVM.Core\Src\MVVM.Utils.pas',
  MVVM.ViewFactory in '..\..\..\Source\MVVM.Core\Src\MVVM.ViewFactory.pas',
  MVVM.Controls.Platform.FMX in '..\..\..\Source\MVVM.FMX\Src\MVVM.Controls.Platform.FMX.pas',
  MVVM.Services.Platform.FMX in '..\..\..\Source\MVVM.FMX\Src\MVVM.Services.Platform.FMX.pas',
  MVVM.Views.Platform.FMX in '..\..\..\Source\MVVM.FMX\Src\MVVM.Views.Platform.FMX.pas',
  MVVM.Bindings.LiveBindings.EnumerableAdapter in '..\..\..\Source\MVVM.Binding.Strategies\LiveBindings\Src\MVVM.Bindings.LiveBindings.EnumerableAdapter.pas',
  MVVM.Bindings.LiveBindings in '..\..\..\Source\MVVM.Binding.Strategies\LiveBindings\Src\MVVM.Bindings.LiveBindings.pas',
  MVVM.Binding.LiveBindings.Controls.FMX in '..\..\..\Source\MVVM.Binding.Strategies\LiveBindings.FMX\Src\MVVM.Binding.LiveBindings.Controls.FMX.pas',
  MVVM.Attributes in '..\..\..\Source\MVVM.Core\Src\MVVM.Attributes.pas',
  MVVM.Interfaces.Architectural in '..\..\..\Source\MVVM.Core\Src\MVVM.Interfaces.Architectural.pas';
{$R *.res}

begin
  Application.Initialize;
  InitializePlatform;

  ReportMemoryLeaksOnShutdown := True;

  MVVMCore.DefaultBindingStrategyName := 'LIVEBINDINGS';
  MVVMCore.DefaultViewPlatform        := 'WINDOWS_DESKTOP';

  MVVMCore.InitializationDone;
  MVVMCore.DefaultBindingStrategyName := 'LIVEBINDINGS';

  Modelo      := TCSVFile_Model.Create;

  VistaModelo := TCSVFile_ViewModel.Create;
  VistaModelo.SetModel(Modelo);

  Vista := MVVMCore.ViewsProvider.CreateView<ICSVFile_ViewModel>(MVVMCore.DefaultViewPlatform, ICSVFile_View_NAME, nil, VistaModelo);
  Application.MainForm := TfrmCSV(Vista.GetAsObject);

  Utils.ShowView(Vista);

  Application.Run;
end.
