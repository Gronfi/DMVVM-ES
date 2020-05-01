unit Coche.Interfaces;

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

  Coche.Types,

  DataSet.Model;

type

  ICoches_ViewModel = Interface(IViewModel)
    ['{6AAA74B3-325A-4986-8556-3D703B090364}']
    function GetDataSet: TDataSet;

    function GetProcMakeGetRows:  TExecuteMethod;
    function GetProcDeleteActiveRow:  TExecuteMethod;
    function GetProcMakeAppend:  TExecuteMethod;
    function GetProcMakeUpdate:  TExecuteMethod;

    function GetIsOpen: TCanExecuteMethod;

    function GetActiveRow: RCoche;
    procedure AppendRow(const AData: RCoche);
    procedure UpdateActiveRow(const AData: RCoche);

    property DataSet: TDataSet read GetDataSet;
    property IsOpen: TCanExecuteMethod read GetIsOpen;

    property DoMakeGetRows: TExecuteMethod read GetProcMakeGetRows;
    property DoDeleteActiveRow: TExecuteMethod read GetProcDeleteActiveRow;
    property DoMakeAppend: TExecuteMethod read GetProcMakeAppend;
    property DoMakeUpdate: TExecuteMethod read GetProcMakeUpdate;
  end;

  INewCoche_ViewModel = Interface(IViewModel)
    ['{DECCB720-73A7-4F02-ABD9-5CD6F7995428}']
    function GetSetData: TExecuteMethod<RCoche>;

    procedure SetData(const AData: RCoche);

    function GetOnDataSelected: IEvent<TNotify_Coche>;

    property OnDataSelected: IEvent<TNotify_Coche> read GetOnDataSelected;
    property DoSetData: TExecuteMethod<RCoche> read GetSetData;
  end;

  IUpdateCoche_ViewModel = Interface(IViewModel)
    ['{2F6ADB68-A4E0-43E2-8C0A-1BBE2FE1C1C7}']
    function GetSetData: TExecuteMethod<RCoche>;

    function GetData: RCoche;
    procedure SetData(const AData: RCoche);

    function GetOnDataSelected: IEvent<TNotify_Coche>;

    property OnDataSelected: IEvent<TNotify_Coche> read GetOnDataSelected;
    property DoSetData: TExecuteMethod<RCoche> read GetSetData;
    property Data: RCoche read GetData write SetData;
  end;

implementation

end.
