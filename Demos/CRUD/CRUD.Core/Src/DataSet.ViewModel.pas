unit DataSet.ViewModel;

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB,

  Spring,
  Spring.Collections,

  DataSet.Interfaces,
  DataSet.Model,
  DataSet.Types,

  MVVM.Types,
  MVVM.Attributes,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.Bindings;

type

{$RTTI EXPLICIT METHODS([vcPublic, vcProtected])}

  [ViewModel_Implements(IDataSet_ViewModel)]
  TDataSet_ViewModel = class(TViewModel, IDataSet_ViewModel)
  private
    FModel: TDataSet_Model;
    FTableName: string;
    FTableIndex: string;
    FNewRowView: string;
    FUpdateRowView: string;
  protected
    function GetProcMakeGetRows:  TExecuteMethod;
    function GetProcDeleteActiveRow:  TExecuteMethod;
    function GetProcMakeAppend:  TExecuteMethod;
    function GetProcMakeUpdate:  TExecuteMethod;

    function GetModel: TDataSet_Model;

    function GetDataSet: TDataSet;

    function GetTableName: String;
    procedure SetTableName(const ATableName: string);

    function GetTableIndex: string;
    procedure SetTableIndex(const ATableIndex: string);

    function GetNewRowView: string;
    procedure SetNewRowView(const AViewName: string);

    function GetUpdateRowView: string;
    procedure SetUpdateRowView(const AViewName: string);

    function GetIsOpen: TCanExecuteMethod;
    function GetIsClosed: TCanExecuteMethod;

    procedure MakeGetRows;
    procedure DeleteActiveRow;
    procedure MakeAppend;
    procedure MakeUpdate;
  public
    function IsDataSetOpen: Boolean;
    function IsDataSetClosed: Boolean;

    procedure SetupViewModel; override;

    procedure SetModel(AModel: TDataSet_Model);

    [ActionMember(CLOSE_DATASET, OnExecute, '')]
    procedure CloseDataSet;

    [ActionMember(OPEN_DATASET, OnExecute, '')]
    procedure OpenDataSet;

    function GetRows(const AFields: TFieldsToGet): TFieldConverters;
    procedure AppendRow(const AFields: TFieldConverters);
    procedure UpdateActiveRow(const AFields: TFieldConverters);

    property NewRowView: string read GetNewRowView write SetNewRowView;
    property UpdateRowView: string read GetUpdateRowView write SetUpdateRowView;
    property TableName: string read GetTableName write SetTableName;
    property TableIndex: string read GetTableIndex write SetTableIndex;
    property DataSet: TDataSet read GetDataSet;
    property Model: TDataSet_Model read GetModel;

    [ActionMember(DATASET_IS_OPEN, OnUpdate, '')]
    property IsOpen: TCanExecuteMethod read GetIsOpen;
    [ActionMember(DATASET_IS_CLOSED, OnUpdate, '')]
    property IsClosed: TCanExecuteMethod read GetIsClosed;

    [ActionMember(GET_ROWS, OnExecute, '')]
    property DoMakeGetRows: TExecuteMethod read GetProcMakeGetRows;
    [ActionMember(DELETE_ROW, OnExecute, '')]
    property DoDeleteActiveRow: TExecuteMethod read GetProcDeleteActiveRow;
    [ActionMember(APPEND_ROW, OnExecute, '')]
    property DoMakeAppend: TExecuteMethod read GetProcMakeAppend;
    [ActionMember(UPDATE_ROW, OnExecute, '')]
    property DoMakeUpdate: TExecuteMethod read GetProcMakeUpdate;
  end;

implementation

uses
  System.Rtti,
  System.Threading,
  System.Diagnostics,
  System.UITypes,

  MVVM.Utils,
  MVVM.Core;

{ TDataSetFile_ViewModel }

procedure TDataSet_ViewModel.OpenDataSet;
begin
  if not FModel.IsOpen then
    FModel.Open;
end;

procedure TDataSet_ViewModel.AppendRow(const AFields: TFieldConverters);
begin
  FModel.AppendRow(AFields);
end;

procedure TDataSet_ViewModel.CloseDataSet;
begin
  if FModel.IsOpen then
    FModel.Close
end;

procedure TDataSet_ViewModel.DeleteActiveRow;
begin
  if not FModel.IsOpen then
  begin
    MVVMCore.PlatformServices.MessageDlg('Warning (delete)', 'The dataset is closed');
    Exit;
  end;
  FModel.DataSet.Delete;
end;

function TDataSet_ViewModel.GetDataSet: TDataSet;
begin
  Result := FModel.DataSet;
end;

function TDataSet_ViewModel.GetIsClosed: TCanExecuteMethod;
begin
  Result := IsDataSetClosed;
end;

function TDataSet_ViewModel.GetIsOpen: TCanExecuteMethod;
begin
  Result := IsDataSetOpen;
end;

function TDataSet_ViewModel.GetModel: TDataSet_Model;
begin
  Result := FModel
end;

function TDataSet_ViewModel.GetNewRowView: string;
begin
  Result := FNewRowView
end;

function TDataSet_ViewModel.GetProcDeleteActiveRow: TExecuteMethod;
begin
  Result := DeleteActiveRow;
end;

function TDataSet_ViewModel.GetProcMakeAppend: TExecuteMethod;
begin
  Result := MakeAppend
end;

function TDataSet_ViewModel.GetProcMakeGetRows: TExecuteMethod;
begin
  Result := MakeGetRows;
end;

function TDataSet_ViewModel.GetProcMakeUpdate: TExecuteMethod;
begin
  Result := MakeUpdate
end;

function TDataSet_ViewModel.GetRows(const AFields: TFieldsToGet): TFieldConverters;
begin
  Result := FModel.GetRows(AFields);
end;

function TDataSet_ViewModel.GetTableIndex: string;
begin
  Result := FTableIndex
end;

function TDataSet_ViewModel.GetTableName: String;
begin
  Result := FTableName;
end;

function TDataSet_ViewModel.GetUpdateRowView: string;
begin
  Result := FUpdateRowView
end;

function TDataSet_ViewModel.IsDataSetClosed: Boolean;
begin
  Result := not IsDataSetOpen
end;

function TDataSet_ViewModel.IsDataSetOpen: Boolean;
begin
  Result := FModel.IsOpen
end;

procedure TDataSet_ViewModel.MakeAppend;
var
  LView: IView<IDataSet_ViewModel>;
begin
  OpenDataSet;

  LView := Utils.ShowModalView<IDataSet_ViewModel>(Self, FNewRowView,
    procedure(AResult: TModalResult)
    begin;
    end, MVVMCore.DefaultViewPlatform);
end;

procedure TDataSet_ViewModel.MakeGetRows;
begin
  if not FModel.IsOpen then
    FModel.Open
  else
    FModel.DataSet.Refresh;
end;

procedure TDataSet_ViewModel.MakeUpdate;
var
  LView: IView<IDataSet_ViewModel>;
begin
  if not FModel.IsOpen then
  begin
    MVVMCore.PlatformServices.MessageDlg('Warning (update)', 'The dataset is not opened');
    Exit;
  end;

  LView := Utils.ShowModalView<IDataSet_ViewModel>(Self, FUpdateRowView,
    procedure(AResult: TModalResult)
    begin;
    end, MVVMCore.DefaultViewPlatform);
end;

procedure TDataSet_ViewModel.SetModel(AModel: TDataSet_Model);
begin
  if FModel <> AModel then
  begin
    FModel := AModel;
    SetupViewModel;
  end;
end;

procedure TDataSet_ViewModel.SetNewRowView(const AViewName: string);
begin
  FNewRowView := AViewName
end;

procedure TDataSet_ViewModel.SetTableIndex(const ATableIndex: string);
begin
  if FTableIndex <> ATableIndex then
  begin
    FTableIndex       := ATableIndex;
    FModel.TableIndex := FTableIndex;
  end;
end;

procedure TDataSet_ViewModel.SetTableName(const ATableName: string);
begin
  if FTableName <> ATableName then
  begin
    FTableName       := ATableName;
    FModel.TableName := FTableName;
  end;
end;

procedure TDataSet_ViewModel.SetUpdateRowView(const AViewName: string);
begin
  FUpdateRowView := AViewName
end;

procedure TDataSet_ViewModel.SetupViewModel;
begin
  if not FTableName.IsEmpty then
  begin
    FModel.TableName  := FTableName;
    FModel.TableIndex := FTableIndex;
    FModel.Open;
  end;
end;

procedure TDataSet_ViewModel.UpdateActiveRow(const AFields: TFieldConverters);
begin
  FModel.UpdateActiveRow(AFields);
end;

initialization

TDataSet_ViewModel.ClassName; // as there should be no implicit create, we must do this so the rtti info of the class is included in the final exe

end.
