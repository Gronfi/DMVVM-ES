unit DataSet.Model;

interface

uses
  System.SysUtils, Data.DB,

  FireDac.DApt,
  FireDAC.Comp.Client;
type
  TDataSet_Model = class
  private
    { Private declarations }
    FTableName: string;
    FDTable1: TFDTable;
  protected
    function GetDataSet: TDataSet;
    function GetIsOpen: Boolean;
    function GetRowCount: Integer;

    procedure SetTableName(const ATableName: String);
    procedure SetConnection(AConnection: TFDConnection);
  public
    { Public declarations }
    constructor Create; overload;
    constructor Create(const ATableName: string; AConnection: TFDConnection = nil); overload;
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    property RowCount: Integer read GetRowCount;
    property Connection: TFDConnection write SetConnection;
    property TableName: string read FTableName write SetTableName;
    property IsOpen: Boolean read GetIsOpen;
    property DataSet: TDataSet read GetDataSet;
  end;

implementation

uses
  System.IOUtils;

{ TDataSet_Model }
procedure TDataSet_Model.Close;
begin
  FDTable1.Active := False;
end;

constructor TDataSet_Model.Create(const ATableName: string; AConnection: TFDConnection);
begin
  Create;
  if Assigned(AConnection) then
    FDTable1.Connection := AConnection;
  TableName := ATableName;
end;

constructor TDataSet_Model.Create;
begin
  inherited Create;
  FDTable1 := TFDTable.Create(nil);
  FDTable1.TableName := '';
end;

destructor TDataSet_Model.Destroy;
begin
  FDTable1.Free;
  inherited;
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

procedure TDataSet_Model.Open;
begin
  FDTable1.Active := True;
end;

procedure TDataSet_Model.SetConnection(AConnection: TFDConnection);
begin
  FDTable1.Connection := AConnection;
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
    FTableName := ATableName;
    FDTable1.TableName := FTableName;
    if LWasOpen then
      Open;
  end;
end;

end.
