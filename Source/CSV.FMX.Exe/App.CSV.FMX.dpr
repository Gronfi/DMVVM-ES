program App.CSV.FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
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

  Modelo      := TCSVFile_Model.Create;

  VistaModelo := TCSVFile_ViewModel.Create;
  VistaModelo.SetModel(Modelo);

  Vista := TfrmCSV.Create(Application);
  Application.MainForm := TfrmCSV(Vista);
  Vista.AddViewModel(VistaModelo);
  TfrmCSV(Vista).Show;

  Application.Run;
end.
