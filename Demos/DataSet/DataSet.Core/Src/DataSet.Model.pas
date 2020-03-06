unit DataSet.Model;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient,

  Spring,
  Spring.Collections,

  DataSet.Interfaces,
  MVVM.Bindings;

type
  TDataSet_Model = class(TDataModule, IDataSetFile_Model)
    cdsSource: TClientDataSet;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FFileName: String;
    FBinder  : TBindingHelper_V2;
  protected
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsPathOK: Boolean;
    function GetDataSet: TDataSet;

    procedure Notify(const APropertyName: string = '');
  public
    { Public declarations }
    constructor Create; overload;

    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;
    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;

    procedure Open;

    property DataSet: TDataSet read GetDataSet;
    property IsPathOk: Boolean read GetIsPathOK;
    property FileName: String read GetFileName write SetFileName;
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
  FBinder.Free;
  inherited;
end;

procedure TDataSet_Model.Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string);
begin
    FBinder.Bind(Self, AProperty, ABindToObject, ABindToProperty);
end;

procedure TDataSet_Model.Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string);
begin
//  FBinder.Bind(ASrcAlias, ASrcFormatedExpression, ABindToObject, ADstAlias, ADstFormatedExpression);
end;

procedure TDataSet_Model.BindReverse(const ABindObject: TObject; const AProperty, ABindToProperty: string);
begin
//  FBinder.BindReverse(ABindObject, AProperty, ABindToProperty);
end;

procedure TDataSet_Model.BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression: string);
begin
//  FBinder.BindReverse(ABindObject, ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression);
end;

constructor TDataSet_Model.Create;
begin
  inherited Create(nil);
end;

procedure TDataSet_Model.DataModuleCreate(Sender: TObject);
begin
  inherited;
  FBinder := TBindingHelper_V2.Create(Self);
end;

{ TdmDataSet }

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

procedure TDataSet_Model.Notify(const APropertyName: string);
begin
  FBinder.Notify(APropertyName);
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

end.
