unit Coches.VM;

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB,

  Spring,
  Spring.Collections,

  MVVM.Types,
  MVVM.Attributes,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.Bindings,

  Coche.Interfaces,
  Coche.Types,

  DataSet.Model,
  DataSet.Types;

type

  [ViewModel_Implements(ICoches_ViewModel)]
  TCoches_ViewModel = class(TViewModel, ICoches_ViewModel)
  private
    FModel: TdmDataSet;
  protected
    function GetProcMakeGetRows    :  TExecuteMethod;
    function GetProcDeleteActiveRow:  TExecuteMethod;
    function GetProcMakeAppend     :  TExecuteMethod;
    function GetProcMakeUpdate     :  TExecuteMethod;

    function GetModel: TdmDataSet;

    function GetDataSet: TDataSet;

    function GetIsOpen: TCanExecuteMethod;
    function IsDataSetOpen: Boolean;

    procedure MakeGetRows;
    procedure DeleteActiveRow;
    procedure MakeAppend;
    procedure MakeUpdate;

    procedure CloseDataSet;
    procedure OpenDataSet;

    procedure OnNewDataSelected(const AData: RCoche);
    procedure OnUpdateDataSelected(const AData: RCoche);

    procedure AfterConstruction; override;
  public
    procedure SetupViewModel; override;

    function GetActiveRow: RCoche;
    procedure AppendRow(const AData: RCoche);
    procedure UpdateActiveRow(const AData: RCoche);

    property DataSet: TDataSet read GetDataSet;
    property Model: TdmDataSet read GetModel;

    property IsOpen: TCanExecuteMethod read GetIsOpen;

    property DoMakeGetRows: TExecuteMethod read GetProcMakeGetRows;
    property DoDeleteActiveRow: TExecuteMethod read GetProcDeleteActiveRow;
    property DoMakeAppend: TExecuteMethod read GetProcMakeAppend;
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

procedure TCoches_ViewModel.OnNewDataSelected(const AData: RCoche);
begin
  AppendRow(AData);
end;

procedure TCoches_ViewModel.OnUpdateDataSelected(const AData: RCoche);
begin
  UpdateActiveRow(AData);
end;

procedure TCoches_ViewModel.OpenDataSet;
begin
  if not FModel.IsOpen then
    FModel.Open;
end;

procedure TCoches_ViewModel.AfterConstruction;
begin
  SetupViewModel;
end;

procedure TCoches_ViewModel.AppendRow(const AData: RCoche);
var
  LData: TFieldConverters;
begin
  LData.AddData('ID', AData.ID);
  LData.AddData('NOMBRE', AData.Nombre);
  LData.AddData('IMAGEN', AData.Imagen);
  LData.AddData('DUEÑO', AData.Dueño);
  FModel.AppendRow(LData);
end;

procedure TCoches_ViewModel.CloseDataSet;
begin
  if FModel.IsOpen then
    FModel.Close;
end;

procedure TCoches_ViewModel.DeleteActiveRow;
begin
  if not FModel.IsOpen then
  begin
    MVVMCore.PlatformServices.MessageDlg('Warning (delete)', 'The dataset is closed');
    Exit;
  end;
  FModel.DataSet.Delete;
end;

function TCoches_ViewModel.GetActiveRow: RCoche;
begin

end;

function TCoches_ViewModel.GetDataSet: TDataSet;
begin
  Result := FModel.DataSet;
end;

function TCoches_ViewModel.GetIsOpen: TCanExecuteMethod;
begin
  Result := IsDataSetOpen;
end;

function TCoches_ViewModel.GetModel: TdmDataSet;
begin
  Result := FModel
end;

function TCoches_ViewModel.GetProcDeleteActiveRow: TExecuteMethod;
begin
  Result := DeleteActiveRow;
end;

function TCoches_ViewModel.GetProcMakeAppend: TExecuteMethod;
begin
  Result := MakeAppend
end;

function TCoches_ViewModel.GetProcMakeGetRows: TExecuteMethod;
begin
  Result := MakeGetRows;
end;

function TCoches_ViewModel.GetProcMakeUpdate: TExecuteMethod;
begin
  Result := MakeUpdate
end;

function TCoches_ViewModel.IsDataSetOpen: Boolean;
begin
  Result := FModel.IsOpen
end;

procedure TCoches_ViewModel.MakeAppend;
var
  LView: IView<INewCoche_ViewModel>;
  LVM  : INewCoche_ViewModel;
begin
  OpenDataSet;

  LVM   := MVVMCore.IoC.ViewModelProvider<INewCoche_ViewModel>;
  LVM.OnDataSelected.Add(OnNewDataSelected);
  LView := Utils.ShowModalView<INewCoche_ViewModel>(LVM, 'Coche.New',
                                                    procedure(AResult: TModalResult)
                                                    begin;
                                                    end, MVVMCore.DefaultViewPlatform);
end;

procedure TCoches_ViewModel.MakeGetRows;
begin
  if not FModel.IsOpen then
    FModel.Open
  else
    FModel.DataSet.Refresh;
end;

procedure TCoches_ViewModel.MakeUpdate;
var
  LView: IView<IUpdateCoche_ViewModel>;
  LVM  : IUpdateCoche_ViewModel;
begin
  if not FModel.IsOpen then
  begin
    MVVMCore.PlatformServices.MessageDlg('Warning (update)', 'The dataset is not opened');
    Exit;
  end;

  LVM   := MVVMCore.IoC.ViewModelProvider<IUpdateCoche_ViewModel>;
  LVM.OnDataSelected.Add(OnUpdateDataSelected);
  LView := Utils.ShowModalView<IUpdateCoche_ViewModel>(LVM, 'Coche.Update',
                                                    procedure(AResult: TModalResult)
                                                    begin;
                                                    end, MVVMCore.DefaultViewPlatform);
end;

procedure TCoches_ViewModel.SetupViewModel;
begin
  FModel.TableName  := 'Coches';
end;

procedure TCoches_ViewModel.UpdateActiveRow(const AData: RCoche);
var
  LData: TFieldConverters;
begin
  LData.AddData('ID', AData.ID);
  LData.AddData('NOMBRE', AData.Nombre);
  LData.AddData('IMAGEN', AData.Imagen);
  LData.AddData('DUEÑO', AData.Dueño);
  FModel.UpdateActiveRow(LData);
end;

initialization

TCoches_ViewModel.ClassName; // as there should be no implicit create, we must do this so the rtti info of the class is included in the final exe

end.
