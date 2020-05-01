unit CSV.Core.ViewModel.DUnitX;

interface

uses
  DUnitX.TestFramework,

  CSV.Interfaces;

type
  [TestFixture]
  TCSVViewModelTests = class(TObject)
  private
    FModelo: ICSVFile_Model;
    FViewModel: ICSVFile_ViewModel;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure SetFileName;
    [Test]
    procedure IsValidFile;
    [Test]
    procedure IsNotValidFile;
    [Test]
    [TestCase('Procesa-OK','..\..\Data.Examples\TestOK.csv,True')]
    [TestCase('Procesa-NoOK','..\..\Data.Examples\TestFalla.csv,False')]
    procedure Procesamiento(const AFile: String; const AResultado: Boolean);
    [Test]
    [TestCase('Procesa-P-OK','..\..\Data.Examples\TestOK.csv,True')]
    [TestCase('Procesa-P-NoOK','..\..\Data.Examples\TestFalla.csv,False')]
    procedure ProcesamientoParalelo(const AFile: String; const AResultado: Boolean);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,

  //MVVM.Servicios.Platform.VCL,
  CSV.Model, CSV.ViewModel;

{ TCSVViewModelTests }


procedure TCSVViewModelTests.IsNotValidFile;
const
  CName = '.\Testdsfsdfs.csv';
begin
  FViewModel.FileName := CName;
  Assert.IsFalse(FViewModel.IsValidFile);
end;

procedure TCSVViewModelTests.IsValidFile;
const
  CName = '.\Test.csv';
begin
  TFile.Delete(CName);
  TFile.WriteAllLines(CName, []);
  FViewModel.FileName := CName;
  Assert.IsTrue(FViewModel.IsValidFile);
end;

procedure TCSVViewModelTests.Procesamiento(const AFile: String; const AResultado: Boolean);
var
  LRes: Boolean;
begin
  TDUnitX.CurrentRunner.Log(TLogLevel.Information, 'Fichero a chequear: ' + AFile);
  FViewModel.FileName := AFile;
  //LRes := FViewModel.ProcesarFicheroCSV;
  TDUnitX.CurrentRunner.Log(TLogLevel.Information, 'Resultado del chequeo: ' + LRes.ToString);
  Assert.AreEqual(AResultado, LRes);
end;

procedure TCSVViewModelTests.ProcesamientoParalelo(const AFile: String; const AResultado: Boolean);
var
  LRes: Boolean;
begin
  TDUnitX.CurrentRunner.Log(TLogLevel.Information, 'Fichero a chequear: ' + AFile);
  FViewModel.FileName := AFile;
//  LRes := FViewModel.ProcesarFicheroCSV_Parallel;
  TDUnitX.CurrentRunner.Log(TLogLevel.Information, 'Resultado del chequeo: ' + LRes.ToString);
  Assert.AreEqual(AResultado, LRes);
end;

procedure TCSVViewModelTests.SetFileName;
const
  CName = '.\Test.csv';
begin
  FViewModel.FileName := CName;
  Assert.AreEqual(CName, FModelo.FileName);
end;

procedure TCSVViewModelTests.Setup;
begin
  //InitializePlatform;
  FModelo    := TCSVFile_Model.Create;
  FViewModel := TCSVFile_ViewModel.Create;
  FViewModel.SetModel(FModelo);
end;

procedure TCSVViewModelTests.TearDown;
begin
  FViewModel := nil;
  FModelo    := nil;
end;

initialization
  TDUnitX.RegisterTestFixture(TCSVViewModelTests);

end.
