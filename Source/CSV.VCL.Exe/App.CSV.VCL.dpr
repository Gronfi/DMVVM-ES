program App.CSV.VCL;

uses
  Vcl.Forms,
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
  Application.MainFormOnTaskbar := True;

  Modelo      := TCSVFile_Model.Create;

  VistaModelo := TCSVFile_ViewModel.Create;
  VistaModelo.SetModel(Modelo);

  Vista := TfrmCSV.Create(Application);
  Vista.AddViewModel(VistaModelo);
  TfrmCSV(Vista).Show;

  // Truco para que quede elegante, crear form principal
  Application.CreateForm(TForm1, Form1);
  //Application.ShowMainForm := False;
  // Fin de truco

  Application.Run;
end.
