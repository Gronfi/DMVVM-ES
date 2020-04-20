unit DataSet.Model;

interface

uses
  System.SysUtils, Data.DB,

  FireDac.DApt,
  FireDac.Comp.Client,

  DataSet.Types;

type
  TDataSet_Model = class
  private
    { Private declarations }
    FTableName: string;
    FTableIndex: string;
    FDTable1: TFDTable;
  protected
    function GetDataSet: TDataSet;
    function GetIsOpen: Boolean;
    function GetRowCount: Integer;

    procedure SetTableName(const ATableName: String);
    procedure SetConnection(AConnection: TFDConnection);
    procedure SetTableIndex(const ATableIndex: string);
  public
    { Public declarations }
    constructor Create; overload;
    constructor Create(const ATableName: string; AConnection: TFDConnection = nil); overload;
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    function GetRows(const AFields: TFieldsToGet): TFieldConverters;
    procedure AppendRow(const AFields: TFieldConverters);
    procedure UpdateActiveRow(const AFields: TFieldConverters);

    procedure Edit;
    procedure Append;
    procedure Post;
    procedure Cancel;
    procedure Delete;

    property RowCount: Integer read GetRowCount;
    property TableIndex: string read FTableIndex write SetTableIndex;
    property Connection: TFDConnection write SetConnection;
    property TableName: string read FTableName write SetTableName;
    property IsOpen: Boolean read GetIsOpen;
    property DataSet: TDataSet read GetDataSet;
  end;

implementation

uses
  System.IOUtils,

  Spring,

  MVVM.Core;

{ TDataSet_Model }
procedure TDataSet_Model.AppendRow(const AFields: TFieldConverters);
var
  I: Integer;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsBrowse], 'Wrong state for AppendRow');
  FDTable1.Append;
  try
    for I := Low(AFields) to High(AFields) do
    begin
      FDTable1.FieldByName(AFields[I].FieldName).AssignValue(AFields[I].FieldValue.AsVarRec);
    end;
    FDTable1.Post;
  except
    on E: Exception do
    begin
      FDTable1.Cancel;
      raise;
    end;
  end;
end;

procedure TDataSet_Model.Cancel;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsEdit, TDataSetState.dsInsert], 'Wrong state for Cancel');
  FDTable1.Cancel;
end;

procedure TDataSet_Model.Close;
begin
  FDTable1.Active := False;
end;

constructor TDataSet_Model.Create(const ATableName: string; AConnection: TFDConnection);
begin
  Create;
  if Assigned(AConnection) then
    FDTable1.Connection := AConnection;
  TableName             := ATableName;
end;

constructor TDataSet_Model.Create;
begin
  inherited Create;
  FDTable1           := TFDTable.Create(nil);
  FDTable1.TableName := '';
end;

procedure TDataSet_Model.Delete;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsBrowse], 'Wrong state for Delete');
  FDTable1.Delete
end;

destructor TDataSet_Model.Destroy;
begin
  FDTable1.Free;
  inherited;
end;

procedure TDataSet_Model.Edit;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsEdit, TDataSetState.dsInsert], 'Wrong state for Edit');
  FDTable1.Edit;
end;

function TDataSet_Model.GetDataSet: TDataSet;
begin
  Result := FDTable1;
end;

function TDataSet_Model.GetIsOpen: Boolean;
begin
  Result := FDTable1.Active
end;

function TDataSet_Model.GetRowCount: Integer;
begin
  Result := FDTable1.RecordCount;
end;

function TDataSet_Model.GetRows(const AFields: TFieldsToGet): TFieldConverters;
var
  I: Integer;
  LObject: TObject;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsBrowse], 'Wrong state for GetRows');
  for I := Low(AFields) to High(AFields) do
  begin
    case FDTable1.FieldByName(AFields[I].FieldName).DataType of
      ftGuid:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsGuid);
      ftString, ftFixedChar, ftWideString, ftFixedWideChar:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsString);
      ftAutoInc, ftInteger:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsInteger);
      ftLongWord:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsLongWord);
      ftShortint:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsInteger);
      ftByte:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsInteger);
      ftSmallInt:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsInteger);
      ftWord:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsInteger);
      ftBoolean:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsBoolean);
      ftFloat, ftCurrency:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsFloat);
      ftMemo, ftWideMemo:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsWideString);
      ftBlob:
        begin
          if AFields[I].isBitmap then
          begin
            LObject := MVVMCore.PlatformServices.LoadBitmap(DataSet.FieldByName(AFields[I].FieldName).AsBytes);
            Result.AddData(AFields[I].FieldName, LObject);
          end
          else
            Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsWideString);
        end;
      ftInterface:
        begin
          //
        end;
      ftIDispatch:
        begin
          //
        end;
      ftGraphic: // por validar
        begin
          LObject := MVVMCore.PlatformServices.LoadBitmap(DataSet.FieldByName(AFields[I].FieldName).AsBytes);
          Result.AddData(AFields[I].FieldName, LObject);
        end;
      ftVariant:
        Result.AddData(AFields[I].FieldName, TValue.FromVariant(FDTable1.FieldByName(AFields[I].FieldName).AsVariant));
      ftDate, ftTime, ftDateTime:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsDateTime);
      ftFMTBCD:
        begin
          //
        end;
      ftBCD:
        begin
          //
        end;
      ftBytes, ftVarBytes:
        begin
          //
        end;
      ftLargeInt:
        Result.AddData(AFields[I].FieldName, FDTable1.FieldByName(AFields[I].FieldName).AsLargeInt);
    end;
  end;
end;

procedure TDataSet_Model.Open;
begin
  FDTable1.Active := True;
end;

procedure TDataSet_Model.Post;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsEdit, TDataSetState.dsInsert], 'Wrong state for post');
  FDTable1.Post;
end;

procedure TDataSet_Model.SetConnection(AConnection: TFDConnection);
begin
  FDTable1.Connection := AConnection;
end;

procedure TDataSet_Model.SetTableIndex(const ATableIndex: string);
begin
  FDTable1.IndexFieldNames := ATableIndex;
end;

procedure TDataSet_Model.SetTableName(const ATableName: String);
var
  LWasOpen: Boolean;
begin
  if FTableName <> ATableName then
  begin
    LWasOpen := IsOpen;
    if LWasOpen then
      Close;
    FTableName         := ATableName;
    FDTable1.TableName := FTableName;
    if LWasOpen then
      Open;
  end;
end;

procedure TDataSet_Model.UpdateActiveRow(const AFields: TFieldConverters);
var
  I: Integer;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsBrowse], 'Wrong state for UpdateActiveRow');
  FDTable1.Edit;
  try
    for I := Low(AFields) to High(AFields) do
    begin
      FDTable1.FieldByName(AFields[I].FieldName).AssignValue(AFields[I].FieldValue.AsVarRec);
    end;
    FDTable1.Post;
  except
    on E: Exception do
    begin
      FDTable1.Cancel;
      raise;
    end;
  end;
end;

procedure TDataSet_Model.Append;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsBrowse], 'Wrong state for Append');
  FDTable1.Append
end;

end.
