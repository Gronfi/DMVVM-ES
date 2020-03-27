unit DataSet.ViewModel;

interface

uses

  System.Classes,
  Data.DB,

  Spring,
  Spring.Collections,

  DataSet.Interfaces,

  MVVM.Interfaces,
  MVVM.Bindings;

type
  TDataSet_ViewModel = class(TInterfacedObject, IDataSetFile_ViewModel, INotifyChangedProperty)
  private
    FManager                  : IStrategyEventedObject;
    FModelo                   : IDataSetFile_Model;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetModel: IDataSetFile_Model;

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;

    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    procedure SetIsValidFile(const AValue: Boolean);

    function GetDataSet: TDataSet;

    procedure OnModelNotifyChanged(const ASender: TObject; const APropertyName: String);

    procedure Notify(const APropertyName: string = '');
  public
    constructor Create;
    destructor Destroy; override;

    function GetAsObject: TObject;

    procedure SetupViewModel;
    procedure SetModel(AModel: IDataSetFile_Model);

    function GetIsValidFile: Boolean;

    procedure AbrirDataSet;

    property DataSet: TDataSet read GetDataSet;
    property FileName: String read GetFileName write SetFileName;
    property IsValidFile: Boolean read GetIsValidFile write SetIsValidFile;

    property Model: IDataSetFile_Model read GetModel;

    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent read GetOnPropertyChangedEvent;
  end;

implementation

uses
  System.SysUtils,
  System.Threading,
  System.Diagnostics,

  MVVM.Core,
  MVVM.Types,

  DataSet.Model;

{ TDataSetFile_ViewModel }

procedure TDataSet_ViewModel.AbrirDataSet;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  FModelo.Open;
end;

constructor TDataSet_ViewModel.Create;
begin
  inherited;
end;

destructor TDataSet_ViewModel.Destroy;
begin
  inherited;
end;

function TDataSet_ViewModel.GetAsObject: TObject;
begin
  Result := Self
end;

function TDataSet_ViewModel.GetDataSet: TDataSet;
begin
  Result := FModelo.DataSet;
end;

function TDataSet_ViewModel.GetFileName: String;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Result := FModelo.FileName;
end;

function TDataSet_ViewModel.GetIsValidFile: Boolean;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Result := FModelo.IsPathOk;
end;

function TDataSet_ViewModel.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TDataSet_ViewModel.GetModel: IDataSetFile_Model;
begin
  Result := FModelo
end;

function TDataSet_ViewModel.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Result := Manager.OnPropertyChangedEvent;
end;

procedure TDataSet_ViewModel.Notify(const APropertyName: string);
begin
  Manager.NotifyPropertyChanged(Self, APropertyName);
end;

procedure TDataSet_ViewModel.OnModelNotifyChanged(const ASender: TObject; const APropertyName: String);
begin
(*
  case Utils.StringToCaseSelect(APropertyName, ['ProgresoProcesamiento', 'FileName', 'IsPathOK']) of
    0:
      begin
        FOnProgresoProcesamiento.Invoke(FModelo.ProgresoProcesamiento);
      end;
    1:
      begin
        Notify('FileName');
        Notify('IsValidFile');
      end;
    2:
      begin
        Notify('IsValidFile');
      end;
  end;
*)
end;

procedure TDataSet_ViewModel.SetFileName(const AFileName: String);
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  if FModelo.FileName <> AFileName then
  begin
    FModelo.FileName := AFileName;
    Notify('FileName');
    Notify('IsValidFile');
  end;
end;

procedure TDataSet_ViewModel.SetIsValidFile(const AValue: Boolean);
begin
  Notify('IsValidFile');
end;

procedure TDataSet_ViewModel.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

procedure TDataSet_ViewModel.SetModel(AModel: IDataSetFile_Model);
begin
  if FModelo <> AModel then
  begin
    FModelo := AModel;
    Notify;
    SetupViewModel;
  end;
end;

procedure TDataSet_ViewModel.SetupViewModel;
var
  [weak] LObservable: INotifyChangedProperty;
begin
  //Bindings
  Manager.BindingStrategy.Bind(TDataSet_Model(FModelo), 'IsPathOK', Self, 'IsValidFile', EBindDirection.OneWay);
  if Supports(FModelo, INotifyChangedProperty, LObservable) then
  begin
    LObservable.Manager.OnPropertyChangedEvent.Add(OnModelNotifyChanged);
  end;
end;

end.
