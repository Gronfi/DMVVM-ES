unit DataSet.Model;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  DataSet.Types;

type
  TdmDataSet = class(TDataModule)
    FDTable1: TFDTable;
  private
    { Private declarations }
    FTableName: string;

    function GetDataSet: TDataSet;
    function GetIsOpen: Boolean;
    procedure SetConnection(const AConnection: TFDConnection);

    procedure SetTableName(const ATableName: String);
  public
    { Public declarations }
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

    property Connection: TFDConnection write SetConnection;
    property DataSet: TDataSet read GetDataSet;
    property TableName: string read FTableName write SetTableName;
    property IsOpen: Boolean read GetIsOpen;
  end;

var
  dmDataSet: TdmDataSet;

implementation

uses
  Spring,

  MVVM.Core;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ TdmCoches }

procedure TdmDataSet.Append;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsBrowse], 'Wrong state for Append');
  FDTable1.Append
end;

procedure TdmDataSet.AppendRow(const AFields: TFieldConverters);
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

procedure TdmDataSet.Cancel;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsEdit, TDataSetState.dsInsert], 'Wrong state for Cancel');
  FDTable1.Cancel;
end;

procedure TdmDataSet.Close;
begin
  FDTable1.Active := False;
end;

procedure TdmDataSet.Delete;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsBrowse], 'Wrong state for Delete');
  FDTable1.Delete
end;

procedure TdmDataSet.Edit;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsEdit, TDataSetState.dsInsert], 'Wrong state for Edit');
  FDTable1.Edit;
end;

function TdmDataSet.GetDataSet: TDataSet;
begin
  Result := FDTable1
end;

function TdmDataSet.GetIsOpen: Boolean;
begin
  Result := FDTable1.Active
end;

function TdmDataSet.GetRows(const AFields: TFieldsToGet): TFieldConverters;
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

procedure TdmDataSet.Open;
begin
  FDTable1.Active := True;
end;

procedure TdmDataSet.Post;
begin
  Spring.Guard.CheckTrue(FDTable1.State in [TDataSetState.dsEdit, TDataSetState.dsInsert], 'Wrong state for post');
  FDTable1.Post;
end;

procedure TdmDataSet.SetConnection(const AConnection: TFDConnection);
begin
  FDTable1.Connection := AConnection;
end;

procedure TdmDataSet.SetTableName(const ATableName: String);
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

procedure TdmDataSet.UpdateActiveRow(const AFields: TFieldConverters);
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

end.
