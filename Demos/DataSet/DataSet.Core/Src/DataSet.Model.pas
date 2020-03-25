unit DataSet.Model;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient,

  Spring,
  Spring.Collections,

  DataSet.Interfaces,

  MVVM.Interfaces,
  MVVM.Bindings;

type
  TDataSet_Model = class(TDataModule, IDataSetFile_Model, IModel, INotifyChangedProperty)
    cdsSource: TClientDataSet;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FManager: IStrategyEventedObject;
    FFileName: String;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;

    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsPathOK: Boolean;
    function GetDataSet: TDataSet;

    procedure Notify(const APropertyName: string = '');
  public
    { Public declarations }
    constructor Create; overload;

    function GetAsObject: TObject;

    procedure Open;

    property DataSet: TDataSet read GetDataSet;
    property IsPathOk: Boolean read GetIsPathOK;
    property FileName: String read GetFileName write SetFileName;

    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent read GetOnPropertyChangedEvent;
  end;

var
  DataSet_Model: TDataSet_Model;

implementation

uses
  System.IOUtils;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TDataSet_Model.DataModuleDestroy(Sender: TObject);
begin
  FManager := nil;
  inherited;
end;

constructor TDataSet_Model.Create;
begin
  inherited Create(nil);
end;

procedure TDataSet_Model.DataModuleCreate(Sender: TObject);
begin
  inherited;
end;

{ TdmDataSet }

function TDataSet_Model.GetAsObject: TObject;
begin
  Result := Self
end;

function TDataSet_Model.GetDataSet: TDataSet;
begin
  Result := cdsSource;
end;

function TDataSet_Model.GetFileName: String;
begin
  Result := FFileName
end;

function TDataSet_Model.GetIsPathOK: Boolean;
begin
  Result := TFile.Exists(FFileName);
end;

function TDataSet_Model.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TDataSet_Model.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Result := Manager.OnPropertyChangedEvent;
end;

procedure TDataSet_Model.Notify(const APropertyName: string);
begin
  Manager.NotifyPropertyChanged(Self, APropertyName);
end;

procedure TDataSet_Model.Open;
begin
  if IsPathOk then
  begin
    cdsSource.LoadFromFile(FFileName);
    cdsSource.Active := True;
  end
  else raise Exception.Create('El fichero no existe');
end;

procedure TDataSet_Model.SetFileName(const AFileName: String);
begin
  if FFileName <> AFileName then
  begin
    FFileName := AFileName;
    Notify('FileName');
    Notify('IsPathOK');
  end;
end;

procedure TDataSet_Model.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

end.
