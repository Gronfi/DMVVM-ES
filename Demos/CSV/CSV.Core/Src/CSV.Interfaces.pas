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
    function GetProgresoProcesamiento: Integer;

    function LoadFile: TStrings;

    function ProcesarFicheroCSV: Boolean;
    function ProcesarFicheroCSV_Parallel: Boolean;

    property IsPathOk: Boolean read GetIsPathOK;
    property FileName: String read GetFileName write SetFileName;
    property ProgresoProcesamiento: Integer read GetProgresoProcesamiento;
  end;

  TFinProcesamiento = procedure(const AData: String) of Object;
  TProgresoProcesamiento = procedure(const AData: Integer) of Object;

  ICSVFile_ViewModel = Interface(IViewModel<ICSVFile_Model>)
  ['{A3EA9B78-2144-4AE3-98CE-6EF522DCDBF7}']
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetProgresoProcesamiento: Integer;

    function GetOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;
    function GetOnProgresoProcesamiento: IEvent<TProgresoProcesamiento>;

    function GetIsValidFile: Boolean;

    procedure ProcesarFicheroCSV;
    procedure ProcesarFicheroCSV_Parallel;

    procedure CreateNewView;

    property IsValidFile: Boolean read GetIsValidFile;
    property FileName: String read GetFileName write SetFileName;
    property ProgresoProcesamiento: Integer read GetProgresoProcesamiento;

    property OnProcesamientoFinalizado: IEvent<TFinProcesamiento> read GetOnProcesamientoFinalizado;
    property OnProgresoProcesamiento: IEvent<TProgresoProcesamiento> read GetOnProgresoProcesamiento;
  end;

const
  ICSVFile_View_NAME = 'ICSVFile_View';

type

  ICSVFile_View = Interface(IView<ICSVFile_ViewModel>)
  ['{A4D5834F-FF1C-4044-BA22-9BCE213241D1}']
  end;

  ICSVFile_ViewForm = Interface(IViewForm<ICSVFile_ViewModel>)
  ['{96836BC6-C410-4610-84CD-4926D32F79E5}']
  end;

implementation

end.
