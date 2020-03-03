unit CSV.ViewModel;

interface

uses
  System.Classes,

  Spring,
  Spring.Collections,

  CSV.Interfaces,
  MVVM.Interfaces,
  MVVM.Bindings;

type
  TCSVFile_ViewModel = class(TInterfacedObject, ICSVFile_ViewModel)
  private
    FBinder                   : TBindingHelper;
    FModelo                   : ICSVFile_Model;
    FOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;

  protected
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;

    function GetIsValidFile: Boolean;
    procedure SetIsValidFile(const AValue: Boolean);

    procedure Notify(const APropertyName: string = '');
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetModel(AModel: ICSVFile_Model);

    function ProcesarFicheroCSV: Boolean;
    function ProcesarFicheroCSV_Parallel: Boolean;

    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;
    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;

    property FileName: String read GetFileName write SetFileName;
    property IsValidFile: Boolean read GetIsValidFile write SetIsValidFile;
    property OnDatosProcesamientoFinalizado: IEvent<TFinProcesamiento> read GetOnProcesamientoFinalizado;
  end;

implementation

uses
  System.SysUtils,
  System.Threading,
  System.Diagnostics,

  MVVM.Core;

{ TCSVFile_ViewModel }

procedure TCSVFile_ViewModel.Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string);
begin
  FBinder.Bind(AProperty, ABindToObject, ABindToProperty);
end;

procedure TCSVFile_ViewModel.Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string);
begin
  FBinder.Bind(ASrcAlias, ASrcFormatedExpression, ABindToObject, ADstAlias, ADstFormatedExpression);
end;

procedure TCSVFile_ViewModel.BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression: string);
begin
  FBinder.BindReverse(ABindObject, ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression);
end;

procedure TCSVFile_ViewModel.BindReverse(const ABindObject: TObject; const AProperty, ABindToProperty: string);
begin
  FBinder.BindReverse(ABindObject, AProperty, ABindToProperty);
end;

constructor TCSVFile_ViewModel.Create;
var
  I: Integer;
begin
  inherited;
  FBinder := TBindingHelper.Create(Self);

  FOnProcesamientoFinalizado := TBindingHelper.CreateEvent<TFinProcesamiento>;
end;

destructor TCSVFile_ViewModel.Destroy;
begin
  FBinder.Free;
  inherited;
end;

function TCSVFile_ViewModel.GetFileName: String;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Result := FModelo.FileName;
end;

function TCSVFile_ViewModel.GetIsValidFile: Boolean;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Result := FModelo.IsPathOk;
end;

function TCSVFile_ViewModel.GetOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;
begin
  Result := FOnProcesamientoFinalizado;
end;

procedure TCSVFile_ViewModel.Notify(const APropertyName: string);
begin
  FBinder.Notify(APropertyName);
end;

function TCSVFile_ViewModel.ProcesarFicheroCSV: Boolean;
var
  LTiming: TStopwatch;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Guard.CheckTrue(FModelo.IsPathOk, 'El fichero no existe: ' + FModelo.FileName);
  if MVVMCore.ServicioDialogo.MessageDlg('Estas seguro?', 'Test') then
  begin
    LTiming := TStopwatch.Create;
    LTiming.Start;
    Result := FModelo.ProcesarFicheroCSV;
    FOnProcesamientoFinalizado.Invoke('Fichero ' + FModelo.FileName + ' procesado (normal) en ' + LTiming.ElapsedMilliseconds.ToString + ' msg');
  end;
end;

function TCSVFile_ViewModel.ProcesarFicheroCSV_Parallel: Boolean;
var
  LTiming  : TStopwatch;
  LFromFile: TStrings;
  LRes     : TParallel.TLoopResult;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Guard.CheckTrue(FModelo.IsPathOk, 'El fichero no existe: ' + FModelo.FileName);
  if MVVMCore.ServicioDialogo.MessageDlg('Estas seguro?', 'Test') then
  begin
    LTiming := TStopwatch.Create;
    LTiming.Start;
    Result := FModelo.ProcesarFicheroCSV_Parallel;
    FOnProcesamientoFinalizado.Invoke('Fichero ' + FModelo.FileName + ' procesado (Paralelo) en ' + LTiming.ElapsedMilliseconds.ToString + ' msg');
  end;
end;

procedure TCSVFile_ViewModel.SetFileName(const AFileName: String);
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  if FModelo.FileName <> AFileName then
  begin
    FModelo.FileName := AFileName;
    Notify('FileName');
    Notify('IsValidFile');
  end;
end;

procedure TCSVFile_ViewModel.SetIsValidFile(const AValue: Boolean);
begin
  Notify('IsValidFile');
end;

procedure TCSVFile_ViewModel.SetModel(AModel: ICSVFile_Model);
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
