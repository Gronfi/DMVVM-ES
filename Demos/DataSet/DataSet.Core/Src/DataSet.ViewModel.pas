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
  TDataSet_ViewModel = class(TInterfacedObject, IDataSetFile_ViewModel)
  private
    FBinder                   : TBindingHelper_V2;
    FModelo                   : IDataSetFile_Model;
  protected
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsValidFile: Boolean;
    procedure SetIsValidFile(const AValue: Boolean);

    function GetDataSet: TDataSet;

    procedure Notify(const APropertyName: string = '');
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetModel(AModel: IDataSetFile_Model);

    procedure AbrirDataSet;

    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;
    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;

    property DataSet: TDataSet read GetDataSet;
    property FileName: String read GetFileName write SetFileName;
    property IsValidFile: Boolean read GetIsValidFile write SetIsValidFile;
  end;

implementation

uses
  System.SysUtils,
  System.Threading,
  System.Diagnostics,

  MVVM.Core;

{ TDataSetFile_ViewModel }

procedure TDataSet_ViewModel.Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string);
begin
  FBinder.Bind(Self, AProperty, ABindToObject, ABindToProperty);
end;

procedure TDataSet_ViewModel.AbrirDataSet;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  if not FModelo.DataSet.Active then
    FModelo.Open;
end;

procedure TDataSet_ViewModel.Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string);
begin
//  FBinder.Bind(ASrcAlias, ASrcFormatedExpression, ABindToObject, ADstAlias, ADstFormatedExpression);
end;

procedure TDataSet_ViewModel.BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression: string);
begin
//  FBinder.BindReverse(ABindObject, ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression);
end;

procedure TDataSet_ViewModel.BindReverse(const ABindObject: TObject; const AProperty, ABindToProperty: string);
begin
//  FBinder.BindReverse(ABindObject, AProperty, ABindToProperty);
end;

constructor TDataSet_ViewModel.Create;
var
  I: Integer;
begin
  inherited;
  FBinder := TBindingHelper_V2.Create(Self);
end;

destructor TDataSet_ViewModel.Destroy;
begin
  FBinder.Free;
  inherited;
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

procedure TDataSet_ViewModel.Notify(const APropertyName: string);
begin
  FBinder.Notify(Self, APropertyName);
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

procedure TDataSet_ViewModel.SetModel(AModel: IDataSetFile_Model);
begin
  if FModelo <> AModel then
  begin
    FModelo := AModel;
    //Bindings
    FModelo.Bind('IsPathOK', Self, 'IsValidFile');
    //
    Notify;
  end;
end;

end.
