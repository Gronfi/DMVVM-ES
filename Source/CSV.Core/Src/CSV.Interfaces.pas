unit CSV.Interfaces;

interface

uses
  System.Classes,

  Spring,
  MVVM.Interfaces,
  MVVM.Bindings;

type

  ICSVFile_Model = Interface(IModel)
  ['{8B9E3855-735B-4641-AA49-8C20E5F6759A}']
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsPathOK: Boolean;

    function LoadFile: TStrings;

    function ProcesarFicheroCSV: Boolean;
    function ProcesarFicheroCSV_Parallel: Boolean;

    property IsPathOk: Boolean read GetIsPathOK;
    property FileName: String read GetFileName write SetFileName;
  end;

  TFinProcesamiento = procedure(const AData: String) of Object;

  ICSVFile_ViewModel = Interface(IViewModel<ICSVFile_Model>)
  ['{A3EA9B78-2144-4AE3-98CE-6EF522DCDBF7}']
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;

    function GetIsValidFile: Boolean;
    procedure SetIsValidFile(const AValue: Boolean);

    function ProcesarFicheroCSV: Boolean;
    function ProcesarFicheroCSV_Parallel: Boolean;

    property IsValidFile: Boolean read GetIsValidFile write SetIsValidFile;
    property FileName: String read GetFileName write SetFileName;
    property OnProcesamientoFinalizado: IEvent<TFinProcesamiento> read GetOnProcesamientoFinalizado;
  end;

  ICSVFile_View = Interface(IView<ICSVFile_Model>)
  ['{A4D5834F-FF1C-4044-BA22-9BCE213241D1}']
  end;

implementation

end.
