unit CSV.Model;

interface

uses
  System.Classes,

  Spring,
  Spring.Collections,

  CSV.Interfaces,
  MVVM.Bindings;

type
  TCSVFile_Model = class(TInterfacedObject, ICSVFile_Model)
  const
    SEPARADOR = ';';
  private
    FBinder  : TBindingHelper;
    FFileName: String;

    FTest1, FTest2, FTest3: IList<String>;
  protected
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);

    function GetIsPathOK: Boolean;

    function ProcesarRow(const ARowNo: Integer; const AData: String): Boolean;
    function ValidarDato(const ARowNo, AColumn: Integer; const AData: String): Boolean;

    procedure Notify(const APropertyName: string = '');
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFile: TStrings;

    function ProcesarFicheroCSV: Boolean;
    function ProcesarFicheroCSV_Parallel: Boolean;

    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;
    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;

    property IsPathOk: Boolean read GetIsPathOK;
    property FileName: String read GetFileName write SetFileName;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,

  System.Threading;

{ TCSVFile }

procedure TCSVFile_Model.Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string);
begin
  FBinder.Bind(AProperty, ABindToObject, ABindToProperty);
end;

procedure TCSVFile_Model.Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string);
begin
  FBinder.Bind(ASrcAlias, ASrcFormatedExpression, ABindToObject, ADstAlias, ADstFormatedExpression);
end;

procedure TCSVFile_Model.BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression: string);
begin
  FBinder.BindReverse(ABindObject, ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression);
end;

procedure TCSVFile_Model.BindReverse(const ABindObject: TObject; const AProperty, ABindToProperty: string);
begin
  FBinder.BindReverse(ABindObject, AProperty, ABindToProperty);
end;

constructor TCSVFile_Model.Create;
var
  I: Integer;
begin
  inherited;
  FBinder := TBindingHelper.Create(Self);

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
  FBinder.Free;
  inherited;
end;

function TCSVFile_Model.GetFileName: String;
begin
  Result := FFileName;
end;

function TCSVFile_Model.GetIsPathOK: Boolean;
begin
  Result := TFile.Exists(FFileName);
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
  FBinder.Notify(APropertyName);
end;

function TCSVFile_Model.ProcesarFicheroCSV: Boolean;
var
  I        : Integer;
  LFromFile: TStrings;
begin
  Result    := True;
  LFromFile := LoadFile;
  try
    //Procesamiento de Rows
    for I := 0 to LFromFile.Count - 1 do
    begin
      if not ProcesarRow(I, LFromFile[I]) then
        Exit(False);
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

end;

end.
