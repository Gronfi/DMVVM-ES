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
  CRUD.Resources.Common in '..\CRUD.Resources.Common\CRUD.Resources.Common.pas',
  uMain in '..\CRUD.FMX.App\Src\uMain.pas' {frmMain},
  Coches.View.Desktop.Grid in '..\CRUD.FMX\Src\Coches.View.Desktop.Grid.pas' {frmCochesDesktop},
  New.Coche.View.Desktop in '..\CRUD.FMX\Src\New.Coche.View.Desktop.pas' {frmNewCoche},
  New.Persona.View.Desktop in '..\CRUD.FMX\Src\New.Persona.View.Desktop.pas' {frmNewPersona},
  Update.Coche.View.Desktop in '..\CRUD.FMX\Src\Update.Coche.View.Desktop.pas' {frmUpdateCoche},
  Update.Persona.View.Desktop in '..\CRUD.FMX\Src\Update.Persona.View.Desktop.pas' {frmUpdatePersona},
  DataSet.Types in '..\CRUD.Core\Src\DataSet.Types.pas',
  MVVM.CommandFactory in '..\..\..\Source\MVVM.Core\Src\MVVM.CommandFactory.pas',
  MVVM.Rtti in '..\..\..\Source\MVVM.Core\Src\MVVM.Rtti.pas';

{$R *.res}

begin
  Application.Initialize;
  InitializePlatform;

  MVVMCore.DefaultBindingStrategyName := 'LIVEBINDINGS';
  MVVMCore.DefaultViewPlatform        := 'WINDOWS_DESKTOP';

  MVVMCore.InitializationDone;

  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmNewPersona, frmNewPersona);
  Application.CreateForm(TfrmUpdateCoche, frmUpdateCoche);
  Application.Run;
end.
