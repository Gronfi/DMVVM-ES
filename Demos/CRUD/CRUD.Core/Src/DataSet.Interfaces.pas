unit DataSet.Interfaces;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Classes,
  Data.DB,

  Spring,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.Bindings,
  MVVM.Types,

  DataSet.Types,
  DataSet.Model;

type
  IDataSet_ViewModel = Interface(IViewModel)
    ['{452FF42A-9FE0-49D0-A3D1-DE6422E1202B}']
    function GetDataSet: TDataSet;

    function GetProcMakeGetRows:  TExecuteMethod;
    function GetProcDeleteActiveRow:  TExecuteMethod;
    function GetProcMakeAppend:  TExecuteMethod;
    function GetProcMakeUpdate:  TExecuteMethod;

    function GetIsOpen: TCanExecuteMethod;

    function GetTableName: String;
    procedure SetTableName(const ATableName: string);

    function GetNewRowView: string;
    procedure SetNewRowView(const AViewName: string);

    function GetUpdateRowView: string;
    procedure SetUpdateRowView(const AViewName: string);

    function GetTableIndex: string;
    procedure SetTableIndex(const ATableIndex: string);

    procedure SetModel(AModel: TDataSet_Model);

    procedure CloseDataSet;
    procedure OpenDataSet;

    //procedure MakeGetRows;
    function GetRows(const AFields: TFieldsToGet): TFieldConverters;
    //procedure DeleteActiveRow;
    //procedure MakeAppend;
    procedure AppendRow(const AFields: TFieldConverters);
    //procedure MakeUpdate;
    procedure UpdateActiveRow(const AFields: TFieldConverters);

    property NewRowView: string read GetNewRowView write SetNewRowView;
    property UpdateRowView: string read GetUpdateRowView write SetUpdateRowView;
    property TableName: string read GetTableName write SetTableName;
    property TableIndex: string read GetTableIndex write SetTableIndex;
    property DataSet: TDataSet read GetDataSet;

    property IsOpen: TCanExecuteMethod read GetIsOpen;

    property DoMakeGetRows: TExecuteMethod read GetProcMakeGetRows;
    property DoDeleteActiveRow: TExecuteMethod read GetProcDeleteActiveRow;
    property DoMakeAppend: TExecuteMethod read GetProcMakeAppend;
    property DoMakeUpdate: TExecuteMethod read GetProcMakeUpdate;
  end;

implementation

end.
