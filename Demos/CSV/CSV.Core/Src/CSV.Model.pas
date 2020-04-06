unit CSV.Model;

interface

uses
  System.Classes,

  Spring,
  Spring.Collections,

  CSV.Interfaces,

  MVVM.Interfaces,
  MVVM.Observable,
  MVVM.Bindings;

type
  TCSVFile_Model = class(TObservable, ICSVFile_Model)
  const
    SEPARADOR = ';';
  private
    FFileName: String;
    FProgreso: Integer;
    FOnProgresoProcesamiento  : IEvent<TProgresoProcesamiento>;

    FTest1, FTest2, FTest3: IList<String>;
  protected
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsPathOK: Boolean;
    function GetProgresoProcesamiento: Integer;
    function GetOnProgresoProcesamiento: IEvent<TProgresoProcesamiento>;
    function GetOnPropertyChanged: IEvent<TNotifySomethingChangedEvent>;

    function ProcesarRow(const ARowNo: Integer; const AData: String): Boolean;
    function ValidarDato(const ARowNo, AColumn: Integer; const AData: String): Boolean;

    procedure Notify(const APropertyName: string = '');
  public
    constructor Create;
    destructor Destroy; override;


    function GetAsObject: TObject;
    function LoadFile: TStrings;

    function ProcesarFicheroCSV: Boolean;
    function ProcesarFicheroCSV_Parallel: Boolean;

    property IsPathOk: Boolean read GetIsPathOK;
    property FileName: String read GetFileName write SetFileName;
    property ProgresoProcesamiento: Integer read GetProgresoProcesamiento;
    property OnPropertyChanged: IEvent<TNotifySomethingChangedEvent> read GetOnPropertyChanged;
    property OnProgresoProcesamiento: IEvent<TProgresoProcesamiento> read GetOnProgresoProcesamiento;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,

  System.Threading,

  MVVM.Utils;

{ TCSVFile }

constructor TCSVFile_Model.Create;
var
  I: Integer;
begin
  inherited;

  FOnProgresoProcesamiento := Utils.CreateEvent<TProgresoProcesamiento>;

  FTest1:= TCollections.CreateList<String>;
  FTest2:= TCollections.CreateList<String>;
  FTest3:= TCollections.CreateList<String>;
  for I := 5 to 30000 do
    FTest1.Add(I.ToString);
  for I := 6 to 50000 do
    FTest2.Add(I.ToString);
  for I := 7 to 80000 do
    FTest3.Add(I.ToString);
end;

destructor TCSVFile_Model.Destroy;
begin
  inherited;
end;

function TCSVFile_Model.GetAsObject: TObject;
begin
  Result := Self
end;

function TCSVFile_Model.GetFileName: String;
begin
  Result := FFileName;
end;

function TCSVFile_Model.GetIsPathOK: Boolean;
begin
  Result := TFile.Exists(FFileName);
end;

function TCSVFile_Model.GetOnProgresoProcesamiento: IEvent<TProgresoProcesamiento>;
begin
  Result := FOnProgresoProcesamiento;
end;

function TCSVFile_Model.GetOnPropertyChanged: IEvent<TNotifySomethingChangedEvent>;
begin
  Result := Manager.OnPropertyChangedEvent
end;

function TCSVFile_Model.GetProgresoProcesamiento: Integer;
begin
  Result := FProgreso;
end;

function TCSVFile_Model.LoadFile: TStrings;
begin
  Result := TStringList.Create;
  //Carga del fichero
  Result.BeginUpdate;
  try
    Result.LoadFromFile(FFileName);
  finally
    Result.EndUpdate;
  end;
end;

procedure TCSVFile_Model.Notify(const APropertyName: string);
begin
  Manager.NotifyPropertyChanged(Self, APropertyName);
end;

function TCSVFile_Model.ProcesarFicheroCSV: Boolean;
var
  I             : Integer;
  LFromFile     : TStrings;
  LPaso         : Integer;
begin
  Result    := True;
  LFromFile := LoadFile;
  try
    FProgreso := 0;
    //Procesamiento de Rows
    for I := 0 to LFromFile.Count - 1 do
    begin
      if not ProcesarRow(I, LFromFile[I]) then
        Exit(False);
      LPaso := ((I + 1) * 100) DIV LFromFile.Count;
      if (LPaso <> FProgreso) then
      begin
        FProgreso := LPaso;
        Notify('ProgresoProcesamiento');
        FOnProgresoProcesamiento.Invoke(FProgreso);
      end;
    end;
  finally
    LFromFile.Free;
  end;
end;

function TCSVFile_Model.ProcesarFicheroCSV_Parallel: Boolean;
var
  LFromFile: TStrings;
  LRes     : TParallel.TLoopResult;
begin
  Result  := True;
  LFromFile := LoadFile;
  try
    //Procesamiento de Rows
    TThreadPool.Current.SetMaxWorkerThreads(100);
    TThreadPool.Current.SetMinWorkerThreads(25);
    LRes := TParallel.for(0, LFromFile.Count - 1, procedure(I: Integer; ALoopState: TParallel.TLoopState)
            begin
              if ALoopState.Stopped then
                Exit;
              if not ProcesarRow(I, LFromFile[I]) then
              begin
                ALoopState.Stop;
              end;
            end);
    if not LRes.Completed then
      Result := False;
  finally
    LFromFile.Free;
  end;
end;

function TCSVFile_Model.ProcesarRow(const ARowNo: Integer;
  const AData: String): Boolean;
var
  LDatos  : TArray<String>;
  I, LCnt : Integer;
begin
  LDatos := AData.Split([SEPARADOR]);
  LCnt   := Length(LDatos); //se podría chequear aqui ya si el numero de campos no es el esperado
  for I := 0 to LCnt - 1 do
  begin
    Result := ValidarDato(ARowNo, I, LDatos[I]);
    if not Result then
      Exit; //ha fallado!!!
  end;

end;

procedure TCSVFile_Model.SetFileName(const AFileName: String);
begin
  if FFileName <> AFileName then
  begin
    FFileName := AFileName;
    Notify('FileName');
    Notify('IsPathOK');
  end;
end;

function TCSVFile_Model.ValidarDato(const ARowNo, AColumn: Integer;
  const AData: String): Boolean;
begin
  //algo aleatorio simplemente de prueba
  Result := not((ARowNo in [3, 59, 100]) and (AColumn in [6..10]));
  if not Result then Exit;
  //calculos tontos varios
  if FTest1.Contains(AData) then
    Exit(False);
  if FTest2.Contains(AData) then
    Exit(False);
  if FTest3.Contains(AData) then
    Exit(False);
  if AData = 'FallaSiempre' then
    Exit(False);
  Sleep(10);
end;

end.
