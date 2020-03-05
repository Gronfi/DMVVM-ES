unit CSV.Core.Model.DUnitX;

interface
uses
  DUnitX.TestFramework,

  CSV.Interfaces;

type

  [TestFixture]
  TCSVModelTests = class(TObject)
  private
    FModelo: ICSVFile_Model;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure SetFileName;
    [Test]
    procedure ExistsFile;
    [Test]
    procedure NotExistsFile;
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

  CSV.Model;

procedure TCSVModelTests.Setup;
begin
  FModelo := TCSVFile_Model.Create;
end;

procedure TCSVModelTests.TearDown;
begin
  FModelo := nil;
end;

procedure TCSVModelTests.ExistsFile;
const
  CName = '.\Test.csv';
begin
  TFile.Delete(CName);
  TFile.WriteAllLines(CName, []);
  FModelo.FileName := CName;
  Assert.IsTrue(FModelo.IsPathOk);
end;

procedure TCSVModelTests.NotExistsFile;
const
  CName = '.\Testdsfsdfs.csv';
begin
  FModelo.FileName := CName;
  Assert.IsFalse(FModelo.IsPathOk);
end;

procedure TCSVModelTests.Procesamiento(const AFile: String; const AResultado: Boolean);
var
  LRes: Boolean;
begin
  TDUnitX.CurrentRunner.Log(TLogLevel.Information, 'Fichero a chequear: ' + AFile);
  FModelo.FileName := AFile;
  LRes := FModelo.ProcesarFicheroCSV;
  TDUnitX.CurrentRunner.Status('Resultado del chequeo: ' + LRes.ToString);
  Assert.AreEqual(AResultado, LRes);
end;

procedure TCSVModelTests.ProcesamientoParalelo(const AFile: String; const AResultado: Boolean);
var
  LRes: Boolean;
begin
  TDUnitX.CurrentRunner.Log(TLogLevel.Information, 'Fichero a chequear: ' + AFile);
  FModelo.FileName := AFile;
  LRes := FModelo.ProcesarFicheroCSV_Parallel;
  TDUnitX.CurrentRunner.Status('Resultado del chequeo: ' + LRes.ToString);
  Assert.AreEqual(AResultado, LRes);
end;

procedure TCSVModelTests.SetFileName;
const
  CName = 'c:\temp\Test.csv';
begin
  FModelo.FileName := CName;
  Assert.AreEqual(FModelo.FileName, CName);
end;

initialization
  TDUnitX.RegisterTestFixture(TCSVModelTests);
end.
