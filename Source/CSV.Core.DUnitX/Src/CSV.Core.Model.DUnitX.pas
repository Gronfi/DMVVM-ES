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
    procedure Test2(const AValue1, AValue2: Integer);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure SetFileName;
    [Test]
    //[TestCase('Test-Existe','.\test.csv', 'True')]
    //[TestCase('Test-NoExiste','3,4')]
    procedure ExistsFile;
    [Test]
    procedure NotExistsFile;
    [Test]
    [TestCase('Procesa-OK','..\..\Data.Examples\TestOK.csv,True')]
    [TestCase('Procesa-NoOK','..\..\Data.Examples\TestFalla.csv,False')]
    procedure Procesamiento(const AFile: String; const AResultado: Boolean);
  end;

implementation

uses
  System.IOUtils,

  CSV.Model, CSV.ViewModel;

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
  Assert.AreEqual(AResultado, LRes);
end;

procedure TCSVModelTests.SetFileName;
const
  CName = 'c:\temp\Test.csv';
begin
  FModelo.FileName := CName;
  Assert.AreEqual(FModelo.FileName, CName);
end;

procedure TCSVModelTests.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TCSVModelTests);
end.
