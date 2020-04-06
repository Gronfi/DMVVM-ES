unit CSV.Interfaces;

interface

uses
  System.Classes,

  Spring,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.Bindings;

type
  TProgresoProcesamiento = procedure(const AData: Integer) of Object;

  ICSVFile_Model = Interface(IModel)
  ['{8B9E3855-735B-4641-AA49-8C20E5F6759A}']
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsPathOK: Boolean;
    function GetProgresoProcesamiento: Integer;
    function GetOnProgresoProcesamiento: IEvent<TProgresoProcesamiento>;
    function GetOnPropertyChanged: IEvent<TNotifySomethingChangedEvent>;

    function LoadFile: TStrings;

    function ProcesarFicheroCSV: Boolean;
    function ProcesarFicheroCSV_Parallel: Boolean;

    property IsPathOk: Boolean read GetIsPathOK;
    property FileName: String read GetFileName write SetFileName;
    property ProgresoProcesamiento: Integer read GetProgresoProcesamiento;
    property OnPropertyChanged: IEvent<TNotifySomethingChangedEvent> read GetOnPropertyChanged;
    property OnProgresoProcesamiento: IEvent<TProgresoProcesamiento> read GetOnProgresoProcesamiento;
  end;

  TFinProcesamiento = procedure(const AData: String) of Object;

  ICSVFile_ViewModel = Interface(IViewModel)
  ['{A3EA9B78-2144-4AE3-98CE-6EF522DCDBF7}']
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetProgresoProcesamiento: Integer;

    function GetOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;
    function GetOnProgresoProcesamiento: IEvent<TProgresoProcesamiento>;

    function GetIsValidFile: Boolean;

    procedure SetModel(AModel: ICSVFile_Model);

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

implementation

end.
