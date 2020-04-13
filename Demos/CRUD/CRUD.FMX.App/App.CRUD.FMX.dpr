program App.CRUD.FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MVVM.Core,
  MVVM.Utils,
  MVVM.Services.Platform.FMX,
  MVVM.Bindings.LiveBindings,
  MVVM.Binding.LiveBindings.Controls.FMX,
  MVVM.Bindings,
  MVVM.Interfaces,
  DataSet.Interfaces,
  DataSet.Model,
  DataSet.ViewModel,
  Personas.View.Desktop {frmDataSetDesktop},
  CRUD.Resources.Common in '..\CRUD.Resources.Common\CRUD.Resources.Common.pas',
  uMain in 'Src\uMain.pas' {frmMain};

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
