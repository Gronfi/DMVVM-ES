unit CSV.ViewModel;

interface

uses

  System.Classes,

  Spring,
  Spring.Collections,

  CSV.Interfaces,

  MVVM.Observable,
  MVVM.Interfaces,
  MVVM.Bindings;

type
  TCSVFile_ViewModel = class(TObservable, ICSVFile_ViewModel)
  private
    FModelo                   : ICSVFile_Model;
    FOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;
    FOnProgresoProcesamiento  : IEvent<TProgresoProcesamiento>;
  protected
    function GetModel: ICSVFile_Model;

    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetProgresoProcesamiento: Integer;

    procedure OnModelNotifyChanged(const ASender: TObject; const APropertyName: String);

    function GetOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;
    function GetOnProgresoProcesamiento: IEvent<TProgresoProcesamiento>;

    procedure Notify(const APropertyName: string = '');
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetupViewModel;
    procedure SetModel(AModel: ICSVFile_Model);

    function GetAsObject: TObject;

    procedure ProcesarFicheroCSV;
    procedure ProcesarFicheroCSV_Parallel;

    procedure CreateNewView;

    function GetIsValidFile: Boolean;

    property FileName: String read GetFileName write SetFileName;
    property IsValidFile: Boolean read GetIsValidFile;
    property ProgresoProcesamiento: Integer read GetProgresoProcesamiento;

    property OnDatosProcesamientoFinalizado: IEvent<TFinProcesamiento> read GetOnProcesamientoFinalizado;
    property OnProgresoProcesamiento: IEvent<TProgresoProcesamiento> read GetOnProgresoProcesamiento;

    property Model: ICSVFile_Model read GetModel;
  end;

implementation

uses
  System.SysUtils,
  System.Threading,
  System.Diagnostics,

  MVVM.ViewFactory,
  MVVM.Utils,
  MVVM.Core;

{ TCSVFile_ViewModel }

constructor TCSVFile_ViewModel.Create;
var
  I: Integer;
begin
  inherited;

  FOnProcesamientoFinalizado := Utils.CreateEvent<TFinProcesamiento>;
  FOnProgresoProcesamiento   := Utils.CreateEvent<TProgresoProcesamiento>;
end;

procedure TCSVFile_ViewModel.CreateNewView;
var
  LVista: IViewForm<ICSVFile_ViewModel>;
begin
  LVista := TViewFactory.CreateView<ICSVFile_ViewModel>(ICSVFile_View_NAME, nil, Self) as IViewForm<ICSVFile_ViewModel>;
  LVista.Execute;
end;

destructor TCSVFile_ViewModel.Destroy;
begin
  FOnProcesamientoFinalizado := nil;
  FOnProgresoProcesamiento   := nil;
  inherited;
end;

function TCSVFile_ViewModel.GetAsObject: TObject;
begin
  Result := Self
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

function TCSVFile_ViewModel.GetModel: ICSVFile_Model;
begin
  Result := FModelo
end;

function TCSVFile_ViewModel.GetOnProcesamientoFinalizado: IEvent<TFinProcesamiento>;
begin
  Result := FOnProcesamientoFinalizado;
end;

function TCSVFile_ViewModel.GetOnProgresoProcesamiento: IEvent<TProgresoProcesamiento>;
begin
  Result := FOnProgresoProcesamiento;
end;

function TCSVFile_ViewModel.GetProgresoProcesamiento: Integer;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Result := FModelo.ProgresoProcesamiento;
end;

procedure TCSVFile_ViewModel.Notify(const APropertyName: string);
begin
  Manager.BindingStrategy.Notify(Self, APropertyName);
end;

procedure TCSVFile_ViewModel.ProcesarFicheroCSV;
var
  LTiming: TStopwatch;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Guard.CheckTrue(FModelo.IsPathOk, 'El fichero no existe: ' + FModelo.FileName);
  if MVVMCore.PlatformServices.MessageDlg('Estas seguro?', 'Test') then
  begin
    LTiming := TStopwatch.Create;
    LTiming.Start;
    FModelo.ProcesarFicheroCSV;
    FOnProcesamientoFinalizado.Invoke('Fichero ' + FModelo.FileName + ' procesado (normal) en ' + LTiming.ElapsedMilliseconds.ToString + ' msg');
  end;
end;

procedure TCSVFile_ViewModel.ProcesarFicheroCSV_Parallel;
var
  LTiming  : TStopwatch;
  LFromFile: TStrings;
  LRes     : TParallel.TLoopResult;
begin
  Guard.CheckNotNull(FModelo, 'Modelo no asignado');
  Guard.CheckTrue(FModelo.IsPathOk, 'El fichero no existe: ' + FModelo.FileName);
  if MVVMCore.PlatformServices.MessageDlg('Estas seguro?', 'Test') then
  begin
    LTiming := TStopwatch.Create;
    LTiming.Start;
    FModelo.ProcesarFicheroCSV_Parallel;
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

procedure TCSVFile_ViewModel.SetModel(AModel: ICSVFile_Model);
begin
  if FModelo <> AModel then
  begin
    FModelo := AModel;
    Notify;
  end;
end;

procedure TCSVFile_ViewModel.OnModelNotifyChanged(const ASender: TObject; const APropertyName: String);
begin
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
end;

procedure TCSVFile_ViewModel.SetupViewModel;
var
  LObservable: INotifyChangedProperty;
begin
  //Bindings
  if Supports(FModelo, INotifyChangedProperty, LObservable) then
  begin
    LObservable.Manager.OnPropertyChangedEvent.Add(OnModelNotifyChanged);
  end;
end;

end.
