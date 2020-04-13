unit DataSet.Interfaces;

interface

uses
  System.Rtti,
  System.Classes,
  Data.DB,

  Spring,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.Bindings,

  DataSet.Types,
  DataSet.Model;

type
  IDataSet_ViewModel = Interface(IViewModel)
  ['{452FF42A-9FE0-49D0-A3D1-DE6422E1202B}']
    function GetDataSet: TDataSet;

    function GetIsOpen: Boolean;

    function GetTableName: String;
    procedure SetTableName(const ATableName: string);

    function GetNewRowView: string;
    procedure SetNewRowView(const AViewName: string);

    function GetUpdateRowView: string;
    procedure SetUpdateRowView(const AViewName: string);

    procedure SetModel(AModel: TDataSet_Model);

    procedure MakeGetRows;
    function GetRows(const AFields: TFieldsToGet): TFieldConverters;
    procedure DeleteActiveRow;
    procedure MakeAppend;
    procedure AppendRow(const AFields: TFieldConverters);
    procedure MakeUpdate;
    procedure UpdateActiveRow(const AFields: TFieldConverters);

    property NewRowView: string read GetNewRowView write SetNewRowView;
    property UpdateRowView: string read GetUpdateRowView write SetUpdateRowView;
    property TableName: string read GetTableName write SetTableName;
    property IsOpen: Boolean read GetIsOpen;
    property DataSet: TDataSet read GetDataSet;
  end;

implementation

end.
