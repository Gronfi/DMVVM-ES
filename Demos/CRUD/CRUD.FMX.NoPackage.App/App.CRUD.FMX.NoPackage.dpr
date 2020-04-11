program App.CRUD.FMX.NoPackage;

uses
  System.StartUpCopy,
  FMX.Forms,
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
  MVVM.Interfaces.Architectural in '..\..\..\Source\MVVM.Core\Src\MVVM.Interfaces.Architectural.pas',
  DataSet.Interfaces in '..\CRUD.Core\Src\DataSet.Interfaces.pas',
  DataSet.Model in '..\CRUD.Core\Src\DataSet.Model.pas',
  DataSet.ViewModel in '..\CRUD.Core\Src\DataSet.ViewModel.pas',
  Personas.View.Desktop in '..\CRUD.FMX\Src\Personas.View.Desktop.pas' {frmPersonasDesktop},
  Personas.Resources.Common in '..\DataSet.Resources.Common\Personas.Resources.Common.pas',
  uMain in '..\CRUD.FMX.App\Src\uMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  InitializePlatform;

  MVVMCore.DefaultBindingStrategyName := 'LIVEBINDINGS';
  MVVMCore.DefaultViewPlatform        := 'WINDOWS_DESKTOP';

  MVVMCore.InitializationDone;

  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
