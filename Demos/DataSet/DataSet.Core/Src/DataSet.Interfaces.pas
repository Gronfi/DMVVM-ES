unit DataSet.Interfaces;

interface

uses
  System.Classes,
  Data.DB,

  Spring,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.Bindings;

type

  IDataSetFile_Model = Interface(IModel)
  ['{913E8A51-3BC9-48EC-851D-1025442CAB3D}']
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsPathOK: Boolean;
    function GetDataSet: TDataSet;

    procedure Open;

    property DataSet: TDataSet read GetDataSet;
    property IsPathOk: Boolean read GetIsPathOK;
    property FileName: String read GetFileName write SetFileName;
  end;

  IDataSetFile_ViewModel = Interface(IViewModel<IDataSetFile_Model>)
  ['{452FF42A-9FE0-49D0-A3D1-DE6422E1202B}']
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsValidFile: Boolean;
    procedure SetIsValidFile(const AValue: Boolean);
    function GetDataSet: TDataSet;

    procedure AbrirDataSet;

    property DataSet: TDataSet read GetDataSet;
    property IsValidFile: Boolean read GetIsValidFile write SetIsValidFile;
    property FileName: String read GetFileName write SetFileName;
  end;

  IDataSetFile_View = Interface(IView<IDataSetFile_ViewModel>)
  ['{9D920DB3-9BD7-4712-8451-B3216A711C1F}']
    procedure RefreshData;
  end;

implementation

end.
