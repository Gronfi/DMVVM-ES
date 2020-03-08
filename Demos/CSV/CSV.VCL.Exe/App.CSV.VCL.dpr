program App.CSV.VCL;

uses
  Vcl.Forms,

  MVVM.Servicios.Platform.VCL,
  MVVM.Bindings in 'Source\MVVM.Bindings.pas',
  MVVM.Interfaces in 'Source\MVVM.Interfaces.pas',
  MVVM.Classes in 'Source\MVVM.Classes.pas',
  CSV.Interfaces in 'CSV.Interfaces.pas',
  CSV.Model in 'CSV.Model.pas',
  CSV.ViewModel in 'CSV.ViewModel.pas',
  CSV.View in 'CSV.View.pas' {frmCSV},
  CSV.RecursosEjercicio in '..\CSV.Resources.Common\CSV.RecursosEjercicio.pas',
  Unit1 in 'Src\Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  InitializePlatform;

  Application.MainFormOnTaskbar := True;

  Modelo      := TCSVFile_Model.Create;

  VistaModelo := TCSVFile_ViewModel.Create;
  VistaModelo.SetModel(Modelo);

  Vista := TfrmCSV.Create(Application);
  Vista.AddViewModel(VistaModelo);
  TfrmCSV(Vista).Show;

  // Truco para que quede elegante (de momento), crear form principal
  Application.CreateForm(TForm1, Form1);
  // Fin de truco

  Application.Run;
end.
