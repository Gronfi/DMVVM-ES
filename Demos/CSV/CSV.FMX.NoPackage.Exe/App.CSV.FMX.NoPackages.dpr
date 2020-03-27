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
  MVVM.Classes in '..\..\..\Source\MVVM.Core\Src\MVVM.Classes.pas',
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
  MVVM.Binding.LiveBindings.Controls.FMX in '..\..\..\Source\MVVM.Binding.Strategies\LiveBindings.FMX\Src\MVVM.Binding.LiveBindings.Controls.FMX.pas';

//  MVVM.Bindings.Collections in '..\..\..\Source\MVVM.Core\Src\MVVM.Bindings.Collections.pas'
//  MVVM.Bindings.Commands in '..\..\..\Source\MVVM.Core\Src\MVVM.Bindings.Commands.pas';

//  CSV.Interfaces in 'CSV.Interfaces.pas',
//  CSV.Model in 'CSV.Model.pas',
//  CSV.ViewModel in 'CSV.ViewModel.pas',
//  CSV.View in 'CSV.View.pas' {frmCSV},
//  CSV.RecursosEjercicio in '..\CSV.Resources.Common\CSV.RecursosEjercicio.pas';
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

  Application.CreateForm(TfrmCSV, frmCSV);
  Application.Run;
end.
