unit MVVM.Messages.Engine;

//{$DEFINE BUG_ACTIVO}
{.$DEFINE PROBANDO}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,

  Spring,
  Spring.Collections,

  MVVM.Interfaces;
  //UHS.Helpers.TThreadedQueue,
  //UHS.Threads,
  //UHS.Interfaces;

type

  { Forward Declarations }

  TMessage               = class;
  TUhsTipoMensaje        = class of TMessage;
  TUhsThreadBaseMensajes = class;
  TUhsThreadMensajes     = class;
  TPoolMensajes          = class;
  TTipoThreadMensajes    = class of TUhsThreadMensajes;

  //EMensajeListenerNoThreadMensajesDefinido = class(EUhsException);
  //EMensajePoolThreadTypeMismatch           = class(EUhsException);

{$REGION 'TMessage'}
  TMessage = class abstract(TInterfacedObject, IMessage)
  private
    FDateTime: TDateTime;
  protected
    function GetCreationDateTime: TDateTime;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Queue; virtual;

    class function GetMensajeType: TUhsTipoMensaje; inline;

    property CreationDateTime: TDateTime read GetCreationDateTime;
  end;
{$ENDREGION}

{$REGION 'TMessageListener'}

  TMessageListener = class abstract(TInterfacedObject, IMessageListener)
  private
    FCallUIThread    : Boolean;
    FThreadMensajes  : TUhsThreadMensajes;
    FPoolMensajes    : TPoolMensajes;
    //FTypeRestriction : TMensajeTypeRestriction;
    FRegistrationMode: TMensajeRegistrationMode;
    FRegistrado      : Boolean;
    FFilterCondition : TListenerFilter;

    function GetIsMainThreadListener: Boolean;
    function GetThreadMensajes: TUhsThreadMensajes;
    //function GetTypeRestriction: TMensajeTypeRestriction;

    procedure SetIsMainThreadListener(const ACallUIThread: Boolean);
    //procedure SetTypeRestriction(const ATypeRestriction: TMensajeTypeRestriction);

    function GetListenerFilter: TListenerFilter;
    procedure SetListenerFilter(const AFilter: TListenerFilter);

  protected
    //function GetDefaultCallUIThread: Boolean; virtual;
    //function GetDefaultTypeRestriction: TMensajeTypeRestriction; virtual;
  public
    procedure AfterConstruction; override;

    constructor Create(const ThreadMensajes: TUhsThreadMensajes; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload; virtual;
    constructor Create(const PoolThreadsMensajes: TPoolMensajes; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload; virtual;

    destructor Destroy; override;

    function GetConditionsMatch(AMessage: IMessage): Boolean; virtual;

    function GetMensajeClass: TClass; virtual; abstract;

    procedure Register;
    procedure UnRegister;

    procedure OnNewMessage(AMensaje: IMessage); virtual;

    property IsMainThreadListener: Boolean read GetIsMainThreadListener write SetIsMainThreadListener;
    property FilterCondition: TListenerFilter read GetListenerFilter write SetListenerFilter;
    //property EventThread: TUhsThreadMensajes read GetThreadMensajes;
    //property TypeRestriction: TMensajeTypeRestriction read GetTypeRestriction write SetTypeRestriction;
  end;

  TListenerMessage<T: TMessage> = class abstract(TMessageListener)
  private type
    TEventCallbackUnbound   = procedure(AMensaje: IMessage);
    TEventCallbackOfObject  = procedure(AMensaje: IMessage) of Object;
    TEventCallbackAnonymous = reference to procedure(AMensaje: IMessage);
  private
    FListaProcesadores: IList<IProcesadorMensaje>;
    FOnMensaje        : IEvento<TNotifyNuevoMensaje>;
    FOnEventUnbound   : TEventCallbackUnbound;
    FOnEventOfObject  : TEventCallbackOfObject;
    FOnEventAnonymous : TEventCallbackAnonymous;
  protected
    function GetOnMensaje: IEvento<TNotifyNuevoMensaje>;
    procedure MensajeNuevo(AMensaje: IMensaje); override; final;
  public
    constructor Create(const AThreadMensajes: TUhsThreadMensajes; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); overload; override;
    constructor Create(const AThreadMensajes: TUhsThreadMensajes; const AOnEventCallback: TEventCallbackUnbound; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload;
    constructor Create(const AThreadMensajes: TUhsThreadMensajes; const AOnEventCallback: TEventCallbackOfObject; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload;
    constructor Create(const AThreadMensajes: TUhsThreadMensajes; const AOnEventCallback: TEventCallbackAnonymous; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload;

    constructor Create(const APoolThreadMensajes: TPoolMensajes; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); overload; override;
    constructor Create(const APoolThreadMensajes: TPoolMensajes; const AOnEventCallback: TEventCallbackUnbound; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload;
    constructor Create(const APoolThreadMensajes: TPoolMensajes; const AOnEventCallback: TEventCallbackOfObject; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload;
    constructor Create(const APoolThreadMensajes: TPoolMensajes; const AOnEventCallback: TEventCallbackAnonymous; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload;

    destructor Destroy; override;
    function GetMensajeClass: TClass; override; final;

    procedure AddProcesador(AProcesador: IProcesadorMensaje);
    procedure RemoveProcesador(AProcesador: IProcesadorMensaje);

    property OnMensaje: IEvento<TNotifyNuevoMensaje> read GetOnMensaje;
  end;

{$ENDREGION}
{$REGION 'TUhsThreadBaseMensajes'}

  TUhsThreadBaseMensajes = class abstract(TUhsThread)
  const
    CTE_INITIAL_QUEUE_SIZE = 10;
    CTE_PUSH_TIMEOUT       = 100;
  private
    FPerformance      : TPerformanceCounter;
    FSaliendo         : Boolean;
    FNoEventosTratados: Int64;

    //FMensajes             : TUhsThreadedQueue<IMensaje>;
    FMensajes             : TUhsThreadedQueue;

    FSincronizador        : IReadWriteSync;

    procedure AdquireWrite;
    procedure ReleaseWrite;
    procedure AdquireRead;
    procedure ReleaseRead;

    procedure ProcessMsgQueued(AMsg: IMensaje);
    procedure ProcessEvents;

    function ObtenerSiguienteMsg(out AQueueSize: Integer; var AMsg: IMensaje): TWaitResult;
  protected
    procedure Tick(const ADelta, AStartTime: Double); override;

    function GetInitialThreadState: TUhsThreadState; override;
    function GetDefaultEventRateAverageOver: Cardinal; virtual;
    function GetDefaultPauseDelay: Double; virtual;

    function GetNoEventosTratados: Int64;

    function GetEventRate: Double;
    function GetEventRateAverage: Double;
    function GetEventRateAverageOver: Cardinal;

    procedure ProcesarMensaje(AMensaje: IMensaje); virtual; abstract;
    function GetPauseOnNoEvent: Boolean; virtual;
    function GetWakeOnEvent: Boolean; virtual;

  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    procedure AddMensaje(AMensaje: IMensaje); virtual;

    property NoEventosTratados: Int64 read GetNoEventosTratados;
    property EventRate: Double read GetEventRate;
    property EventRateAverage: Double read GetEventRateAverage;
    property EventRateAverageOver: Cardinal read GetEventRateAverageOver;
  end;

{$ENDREGION}
{$REGION 'TUhsThreadMensajes'}

  TUhsThreadMensajes = class(TUhsThreadBaseMensajes)
  private
    FListeners       : IList<IMensajeListener>;

    FSincronizadorListeners       : IReadWriteSync;

    FRegistrationMode: TMensajeRegistrationMode;

    FPool: TPoolMensajes;

    function GetListenersCount: Integer;
    constructor Create(const APoolMensajes: TPoolMensajes; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload; virtual;
  protected
    procedure ProcesarMensaje(AMensaje: IMensaje); override;
    procedure Tick(const ADelta, AStartTime: Double); override;

    procedure InitializeListeners; virtual;
    procedure FinalizeListeners; virtual;

    function GetEventRelevant(AMensaje: IMensaje): Boolean; virtual;
  public
    procedure AfterConstruction; override;

    constructor Create(const AName: string; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure RegisterListener(AMensajesListener: IMensajeListener);
    procedure UnregisterListener(AMensajesListener: IMensajeListener);
    property ListenersCount: Integer read GetListenersCount;

    procedure Register;
    procedure UnRegister;
  end;
{$ENDREGION}
{$REGION 'TPoolMensajes'}

  TPoolMensajes = class abstract(TUhsThreadBaseMensajes)
  private
    FSincronizador    : IReadWriteSync;
    FThreadsMensajes  : IList<TUhsThreadMensajes>;
    FRegistrationMode : TMensajeRegistrationMode;
    FThreadCount      : Integer;
    FThreadCountTarget: Integer;

    procedure AddThreadMensajes(const AThreadMensajes: TUhsThreadMensajes);
    procedure RemoveThreadMensajes(const AThreadMensajes: TUhsThreadMensajes);

    procedure CreateThreads;
    procedure DestroyThreads;

    function GetThreadCount: Integer;
    procedure SetThreadCount(const AThreadCount: Integer);

    procedure AdquireWrite;
    procedure ReleaseWrite;
    procedure AdquireRead;
    procedure ReleaseRead;
  protected
    function GetMensajeThreadType: TTipoThreadMensajes; virtual; abstract;

    procedure Tick(const ADelta, AStartTime: Double); override;

    procedure ProcesarMensaje(AMensaje: IMensaje); override;
    /// <summary><c>Called only when there is at least one viable Event Thread in the Pool.</c></summary>
    procedure PoolMensaje(AMensaje: IMensaje); virtual;
  public
    constructor Create(const AThreadCount: Integer; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic); reintroduce;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Register;
    procedure UnRegister;

    procedure RegisterListener(AMensajesListener: IMensajeListener);
    procedure UnregisterListener(AMensajesListener: IMensajeListener);

    property ThreadCount: Integer read GetThreadCount write SetThreadCount;
  end;
{$ENDREGION}
{$REGION 'TPoolMensajes<T>'}

  TPoolMensajes<T: TUhsThreadMensajes> = class(TPoolMensajes)
  protected
    function GetMensajeThreadType: TTipoThreadMensajes; override; final;
  end;
{$ENDREGION}

  TPoolMensajes_GENERAL = class(TPoolMensajes<TUhsThreadMensajes>);
  TPoolMensajes_GENERAL_OneThread = class(TPoolMensajes<TUhsThreadMensajes>);

{$REGION 'TMensajesScheduler'}

  TMensajesScheduler = class(TUhsThread)
  const
    CTE_INITIAL_QUEUE_SIZE = 10;
    CTE_PUSH_TIMEOUT       = 100;
  private
    //FMensajes     : TUhsThreadedQueue<IMensaje>;
    FMensajes     : TUhsThreadedQueue;
    FCacheMensajes: IList<IMensaje>;
    FNextEventTime: Double;
    FSaliendo     : Boolean;
  protected
    function Checks: Boolean;
    procedure LanzarProcesamiento;

    function GetInitialThreadState: TUhsThreadState; override;
    procedure Tick(const ADelta, AStartTime: Double); override;

    function ObtenerSiguienteMsg(out AQueueSize: Integer; var AMsg: IMensaje): TWaitResult; overload;
    function ObtenerSiguienteMsg(out AQueueSize: Integer; var AMsg: IMensaje; const ATimeOut: Cardinal): TWaitResult; overload;
    function PublicarDato(AMsg: IMensaje; var ASize: Integer): TWaitResult;
  public
    constructor Create(const AName: string); reintroduce;
    destructor Destroy; override;
    procedure ScheduleMensaje(AMensaje: IMensaje); inline;
  end;
{$ENDREGION}

{$REGION 'FrMensajes'}

  FrMensajes = record
  private
    class var FScheduler                   : TMensajesScheduler;
    class var FSincronizadorThreadsMensajes: IReadWriteSync;
    class var FSincronizadorPools          : IReadWriteSync;
    class var FThreadsMensajes             : IList<TUhsThreadMensajes>;
    class var FPools                       : IList<TPoolMensajes>;
    class var FTerminando                  : Boolean;

    class var FDiccionarioProcesadoresRegistrados: IDictionary<string, string>;

    class var FColeccionRetroConvertidores: IDictionary<String, FuncionRetroConversion>;
    class var FColeccionProConvertidores  : IList<FuncionProConversion>;

    class procedure CreateIni; static;
    class procedure DestroyIni; static;

  public
    //Registro de convertidores de mensajes: convierten IMensaje --> String
    class procedure RegistrarRetroConvertidor(const ATipoMensaje: TUhsTipoMensaje; const AConversor: FuncionRetroConversion); static;
    class procedure EliminarRetroConvertidor(const ATipoMensaje: TUhsTipoMensaje); static;
    class function RetroConvertirMensaje(AMensaje: IMensaje): String; static;

    //Registro de convertidores de mensajes: convierten String --> IMensaje
    class procedure RegistrarProConvertidor(const AConversor: FuncionProConversion); static;
    class procedure EliminarProConvertidor(const AConversor: FuncionProConversion); static;
    class function ProConvertirMensaje(const AMensaje: String): IMensaje; static;

    //Registra un procesador de mensajería
    class procedure RegistrarProcesador(const Descripcion, DescriptorClase: string); static;
    class function GetProcesadoresRegistrados: IDictionary<string, string>; static;

    //Registra un pool de mensajes
    class procedure RegisterPool(const APoolMensajes: TPoolMensajes); static;
    class procedure UnregisterPool(const APoolMensajes: TPoolMensajes); static;

    //Registra un thread de mensajes
    class procedure RegisterThreadMensajes(const AThreadMensajes: TUhsThreadMensajes); static;
    class procedure UnregisterThreadMensajes(const AThreadMensajes: TUhsThreadMensajes); static;

    //Se pasa al motor de eventos un mensaje a transmitir (utilizará todos los métodos de transmisión posibles)
    class procedure QueueEvent(AMensaje: IMensaje); static;

    //Se pasa al motor de eventos un mensaje a transmitir (utilizará solo los pools)
    class procedure QueueInPools(AMensaje: IMensaje); static;

    //Se pasa al motor de eventos un mensaje a transmitir (utilizará solo los Threads de mensajería)
    class procedure QueueInThreads(AMensaje: IMensaje); static;
  end;
{$ENDREGION}


{$REGION 'Mensaje String Raw'}

  RDato_StringRaw = record
  public
    Fecha    : TDateTime;
    RawString: string;

    procedure init(const ARawString: string; const AFecha: TDateTime);
  end;

  TMensaje_StringRaw = class(TMessage)
  public
    Data: RDato_StringRaw;
  end;

  TMensaje_StringRaw_Listener = class(TUhsMensajeListener<TMensaje_StringRaw>);

{$ENDREGION}

implementation

uses
  System.Generics.Defaults,
  System.Generics.Collections;

{$REGION 'TMessage'}

constructor TMessage.Create;
begin
  inherited Create;
  FDateTime := Now;
end;

destructor TMessage.Destroy;
begin
  inherited Destroy;
end;

function TMessage.GetCreationDateTime: TDateTime;
begin
  Result := FDatetime;
end;

class function TMessage.GetMensajeType: TUhsTipoMensaje;
begin
  Result := TUhsTipoMensaje(Self);
end;

procedure TMessage.Queue;
begin
  FrMensajes.QueueEvent(Self);
end;

{$ENDREGION}
{$REGION 'TUhsMensajeListener'}

procedure TUhsMensajeListener.AfterConstruction;
begin
  inherited;
  if FRegistrationMode = mrmAutomatic then
    Register;
end;

procedure TUhsMensajeListener.CargarDeJSON(NodoJson_IN: TJSONObject);
begin
  //
end;

constructor TUhsMensajeListener.Create(const PoolThreadsMensajes: TPoolMensajes; const ARegistrationMode: TMensajeRegistrationMode);
begin
  inherited Create;
  Self.FCallUIThread   := GetDefaultCallUIThread;
  Self.FExpiraEn       := GetDefaultExpireAfter;
  FRegistrado          := False;
  Self.FLastProcessed  := 0;
  Self.FNewestOnly     := GetDefaultNewestEventOnly;
  Self.FThreadMensajes := nil;
  Self.FPoolMensajes   := PoolThreadsMensajes;
  if Self.FPoolMensajes = nil then
    raise EMensajeListenerNoThreadMensajesDefinido.Create('Los Listeners de mensajes DEBEN tener un Pool(*)/Thread de mensajes asignado.');
  FRegistrationMode := ARegistrationMode;
end;

constructor TUhsMensajeListener.Create(const ThreadMensajes: TUhsThreadMensajes; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic);
begin
  inherited Create;
  Self.FCallUIThread   := GetDefaultCallUIThread;
  Self.FExpiraEn       := GetDefaultExpireAfter;
  FRegistrado          := False;
  Self.FLastProcessed  := 0;
  Self.FNewestOnly     := GetDefaultNewestEventOnly;
  Self.FThreadMensajes := ThreadMensajes;
  Self.FPoolMensajes   := nil;
  if Self.FThreadMensajes = nil then
    raise EMensajeListenerNoThreadMensajesDefinido.Create('Los Listeners de mensajes DEBEN tener un Pool/Thread(*) de mensajes asignado.');
  FRegistrationMode := ARegistrationMode;
end;

destructor TUhsMensajeListener.Destroy;
begin
  UnRegister;
  inherited;
end;

procedure TUhsMensajeListener.FormatoJSON(NodoJson_IN: TJSONObject);
begin
  //
end;

function TUhsMensajeListener.FormatoJSON: TJSONObject;
begin
  //
end;

function TUhsMensajeListener.GetCallUIThread: Boolean;
begin
  AdquireRead;
  try
    Result := Self.FCallUIThread
  finally
    ReleaseRead;
  end;
end;

function TUhsMensajeListener.GetConditionsMatch(Mensaje: IMensaje): Boolean;
begin
  if Assigned(FFilterCondition) then
    Result := FFilterCondition(Mensaje)
  else Result := True
end;

function TUhsMensajeListener.GetDefaultCallUIThread: Boolean;
begin
  Result := False;
end;

function TUhsMensajeListener.GetDefaultExpireAfter: Double;
begin
  Result := 0;
end;

function TUhsMensajeListener.GetDefaultNewestEventOnly: Boolean;
begin
  Result := False;
end;

function TUhsMensajeListener.GetDefaultTypeRestriction: TMensajeTypeRestriction;
begin
  Result := mtrAllowDescendants;
end;

function TUhsMensajeListener.GetExpiraEn: Double;
begin
  AdquireRead;
  try
    Result := FExpiraEn;
  finally
    ReleaseRead;
  end;
end;

function TUhsMensajeListener.GetFiltroListener: TFiltroListener;
begin
  Result := FFilterCondition
end;

function TUhsMensajeListener.GetLastProcessed: Double;
begin
  AdquireRead;
  try
    Result := FLastProcessed;
  finally
    ReleaseRead;
  end;
end;

function TUhsMensajeListener.GetNewestOnly: Boolean;
begin
  AdquireRead;
  try
    Result := FNewestOnly;
  finally
    ReleaseRead;
  end;
end;

function TUhsMensajeListener.GetThreadMensajes: TUhsThreadMensajes;
begin
  Result := Self.FThreadMensajes
end;

function TUhsMensajeListener.GetTypeRestriction: TMensajeTypeRestriction;
begin
  AdquireRead;
  try
    Result := FTypeRestriction;
  finally
    ReleaseRead;
  end;
end;

procedure TUhsMensajeListener.MensajeNuevo(AMensaje: IMensaje);
begin
  Self.FLastProcessed := AMensaje.DispatchTime;
end;

procedure TUhsMensajeListener.Register;
begin
  if FRegistrado then Exit;
  if Self.FThreadMensajes <> nil then
  begin
    Self.FThreadMensajes.RegisterListener(Self);
    FRegistrado := True;
  end
  else if Self.FPoolMensajes <> nil then
  begin
    Self.FPoolMensajes.RegisterListener(Self);
    FRegistrado := True;
  end;
end;

procedure TUhsMensajeListener.SetCallUIThread(const ACallUIThread: Boolean);
begin
  Self.FCallUIThread := ACallUIThread;
end;

procedure TUhsMensajeListener.SetExpiraEn(const AExpireAfter: Double);
begin
  AdquireWrite;
  try
    FExpiraEn := AExpireAfter;
  finally
    ReleaseWrite;
  end;
end;

procedure TUhsMensajeListener.SetFiltroListener(const Value: TFiltroListener);
begin
  FFilterCondition := Value
end;

procedure TUhsMensajeListener.SetNewestOnly(const ANewestOnly: Boolean);
begin
  AdquireWrite;
  try
    FNewestOnly := ANewestOnly;
  finally
    ReleaseWrite;
  end;
end;

procedure TUhsMensajeListener.SetTypeRestriction(const ATypeRestriction: TMensajeTypeRestriction);
begin
  AdquireWrite;
  try
    FTypeRestriction := ATypeRestriction;
  finally
    ReleaseWrite;
  end;
end;

procedure TUhsMensajeListener.UnRegister;
begin
  if FRegistrado then
  begin
    if Self.FThreadMensajes <> nil then
      Self.FThreadMensajes.UnregisterListener(Self)
    else if Self.FPoolMensajes <> nil then
      Self.FPoolMensajes.UnregisterListener(Self);
    FRegistrado := False;
  end;
end;
{$ENDREGION}
{$REGION 'TUhsThreadBaseMensajes'}

procedure TUhsThreadBaseMensajes.AdquireRead;
begin
  Self.FSincronizador.BeginRead;
end;

procedure TUhsThreadBaseMensajes.AdquireWrite;
begin
  Self.FSincronizador.BeginWrite;
end;

constructor TUhsThreadBaseMensajes.Create(const AName: string);
begin
  inherited Create(AName);
  Self.FPerformance           := TPerformanceCounter.Create(GetDefaultEventRateAverageOver);
  Self.FSincronizador         := TMREWSync.Create;

  //Self.FMensajes := TUhsThreadedQueue<IMensaje>.Create(CTE_INITIAL_QUEUE_SIZE, CTE_PUSH_TIMEOUT, FrMath.INFINITO);
  Self.FMensajes := TUhsThreadedQueue.Create(CTE_INITIAL_QUEUE_SIZE, CTE_PUSH_TIMEOUT, FrMath.INFINITO);

  FNoEventosTratados := 0;

  Self.TickRateLimit := 1;
  Self.ThreadState   := TUhsThreadState.tsPaused;
end;

destructor TUhsThreadBaseMensajes.Destroy;
begin
  FSaliendo := True;
  Self.FMensajes.Free;
  inherited Destroy;
  FSincronizador := nil;
  FPerformance.Destroy;
end;

function TUhsThreadBaseMensajes.GetDefaultEventRateAverageOver: Cardinal;
begin
  Result := 10;
end;

function TUhsThreadBaseMensajes.GetDefaultPauseDelay: Double;
begin
  Result := 0.25;
end;

function TUhsThreadBaseMensajes.GetEventRate: Double;
begin
  Result := FPerformance.InstantRate;
end;

function TUhsThreadBaseMensajes.GetEventRateAverage: Double;
begin
  Result := FPerformance.AverageRate;
end;

function TUhsThreadBaseMensajes.GetEventRateAverageOver: Cardinal;
begin
  Result := FPerformance.AverageOver;
end;

function TUhsThreadBaseMensajes.GetInitialThreadState: TUhsThreadState;
begin
  Result := tsPaused;
end;

function TUhsThreadBaseMensajes.GetNoEventosTratados: Int64;
begin
  Result := FNoEventosTratados
end;

function TUhsThreadBaseMensajes.GetPauseOnNoEvent: Boolean;
begin
  Result := True; // By default, we want to make the Thread sleep when there are no Events left to process
end;

function TUhsThreadBaseMensajes.GetWakeOnEvent: Boolean;
begin
  Result := True; // By default, we want to wake the Thread when an Event is added to the Queue/Stack
end;

function TUhsThreadBaseMensajes.ObtenerSiguienteMsg(out AQueueSize: Integer; var AMsg: IMensaje): TWaitResult;
var
  LI   : IInterface;
  LData: TValue;
begin
  Result := FMensajes.PopItem(AQueueSize, LData);
  if Result = TWaitResult.wrSignaled then
  begin
    LI   := LData.AsInterface;
    AMsg := LI as IMensaje;
  end;
end;

procedure TUhsThreadBaseMensajes.ProcessEvents;
var
  LRes                 : TWaitResult;
  LSize                : Integer;
  LMsg                 : IMensaje;
begin
  while not(Terminated) do
  begin
    repeat
      LRes := ObtenerSiguienteMsg(LSize, LMsg);
      case LRes of
        wrSignaled:
          begin
            if (not FSaliendo) then
            begin
              try
                ProcessMsgQueued(LMsg);
{$IFDEF PROBANDO}
                FrBase.LogInfo('<TUhsThreadBaseMensajes.ProcessEvents> Received msg in thread: ' + TThread.CurrentThread.ThreadID.ToString + ' - ' + LSize.ToString);
{$ENDIF}
              except
                on E:Exception do
                begin
                  FrBase.LogError('<TUhsThreadBaseMensajes.ProcessEvents> ' + TThread.CurrentThread.ThreadID.ToString + ' - ' + LSize.ToString + ' - Error: ' + E.Message);
                end;
              end;
            end
            else
            begin
              Exit;
            end;
          end;
        wrAbandoned:
          begin
            Exit;
          end;
      end;
    until (LSize = 0) or (LRes = TWaitResult.wrTimeout);
  end;
end;

procedure TUhsThreadBaseMensajes.ProcessMsgQueued(AMsg: IMensaje);
var
  LProcessStarted: Double;
begin
  if (AMsg.State <> msCancelled) and (not AMsg.HasExpired) then
  begin
    LProcessStarted := FrPlataforma.GetReferenceTime;
    ProcesarMensaje(AMsg);
    Inc(FNoEventosTratados);
    FPerformance.RecordSample(FrPlataforma.GetReferenceTime - LProcessStarted);
  end
  else FrBase.LogError('<TUhsThreadBaseMensajes.ProcessMsgQueued> El mensaje se desecha (cancelado o expirado) !!!');
end;

procedure TUhsThreadBaseMensajes.AddMensaje(AMensaje: IMensaje);
var
  LSize: Integer;
  LRes : TWaitResult;
  LData: TValue;
begin
  if not Assigned(AMensaje) then
  begin
    FrBase.LogError('<TUhsThreadBaseMensajes.AddMensaje>');
    Exit;
  end;
{$IFDEF PROBANDO}
  FrBase.LogInfo('<TPoolMensajes.AddMensaje>');
{$ENDIF}
  repeat
    LData:= TValue.From<IMensaje>(AMensaje);
    LRes := FMensajes.PushItem(LData, LSize);
    case LRes of
      wrTimeout:
        begin
          FMensajes.Grow(LSize);
          FrBase.LogWarning('<TUhsThreadBaseMensajes.AddMensaje> GROW!!!');
          if FSaliendo then Exit;
        end;
    end;
  until LRes = TWaitResult.wrSignaled;
end;

procedure TUhsThreadBaseMensajes.ReleaseRead;
begin
  FSincronizador.EndRead;
end;

procedure TUhsThreadBaseMensajes.ReleaseWrite;
begin
  FSincronizador.EndWrite;
end;

procedure TUhsThreadBaseMensajes.Tick(const ADelta, AStartTime: Double);
begin
end;
{$ENDREGION}
{$REGION 'TUhsThreadMensajes'}

procedure TUhsThreadMensajes.AfterConstruction;
begin
  inherited;
  if FRegistrationMode = mrmAutomatic then
    Register;
end;

constructor TUhsThreadMensajes.Create(const AName: string; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic);
begin
  inherited Create(FrBase.iif<String>(AName.IsEmpty, QualifiedClassName, AName));
  FPool             := nil;
  FRegistrationMode := ARegistrationMode;
  FListeners        := TCollections.CreateList<IMensajeListener>;

  FSincronizadorListeners        := TMREWSync.Create;

  InitializeListeners;
  Wake;
end;

constructor TUhsThreadMensajes.Create(const APoolMensajes: TPoolMensajes; const ARegistrationMode: TMensajeRegistrationMode);
begin
  Create('', ARegistrationMode);
  FPool := APoolMensajes;
end;

destructor TUhsThreadMensajes.Destroy;
begin
  UnRegister;
  FinalizeListeners;
  FListeners        := nil;

  FSincronizadorListeners        := nil;
  inherited Destroy;
end;

procedure TUhsThreadMensajes.FinalizeListeners;
var
  LListener: IMensajeListener;
  LLista   : IList<IMensajeListener>;
begin
  // Nada de momento
  LLista := TCollections.CreateList<IMensajeListener>;
  FSincronizadorListeners.BeginRead;
  try
    for LListener in FListeners do
      LLista.Add(LListener)
  finally
    FSincronizadorListeners.EndRead;
  end;
  for LListener in LLista do
    LListener.UnRegister;
end;

function TUhsThreadMensajes.GetEventRelevant(AMensaje: IMensaje): Boolean;
begin
  Result := True;
end;

function TUhsThreadMensajes.GetListenersCount: Integer;
begin
  FSincronizadorListeners.BeginRead;
  try
    Result := FListeners.Count;
  finally
    FSincronizadorListeners.EndRead;
  end;
end;

procedure TUhsThreadMensajes.InitializeListeners;
begin
  // Nada
end;

procedure TUhsThreadMensajes.ProcesarMensaje(AMensaje: IMensaje);
var
  I  : Integer;
{$IFDEF PROBANDO}
  L  : Boolean;
{$ENDIF}
begin
{$IFDEF PROBANDO}
  L := False;
{$ENDIF}
  FSincronizadorListeners.BeginRead;
  try
    for I := 0 to FListeners.Count - 1 do
    begin
      if (AMensaje.State <> msCancelled) and (not AMensaje.HasExpired) then
      begin
        if (((FListeners[I].GetTypeRestriction = mtrAllowDescendants) and (AMensaje is FListeners[I].GetMensajeClass)) or ((FListeners[I].GetTypeRestriction = mtrDefinedTypeOnly) and (AMensaje.GetAsObject.ClassType = FListeners[I].GetMensajeClass))) and ((FListeners[I].ExpiraEn = 0) or (FrPlataforma.GetReferenceTime < (AMensaje.DispatchTime + AMensaje.ExpiraEn))) and (((FListeners[I].NewestOnly) and (AMensaje.DispatchTime > FListeners[I].LastProcessed)) or (not FListeners[I].NewestOnly)) and
          (FListeners[I].GetConditionsMatch(AMensaje)) then
        begin
          try
{$IFDEF PROBANDO}
            L := True;
{$ENDIF}
            FListeners[I].MensajeNuevo(AMensaje);
{$IFDEF PROBANDO}
            FrBase.LogInfo('<TUhsThreadMensajes.ProcesarMensaje> Msg entregado a listener - ' + I.ToString + ' - ' + FListeners[I].GetAsObject.QualifiedClassName + ' - Type: ' + AMensaje.GetAsObject.QualifiedClassName);
{$ENDIF}
          except
            on E: Exception do
            begin
              FrDiagnosticos.IdeDebugMsg('PETEEEEEEEEEEEEEE: ' + FListeners[I].GetAsObject.QualifiedClassName + ' - Error: ' + E.Message);
              FrBase.LogError('<Motor eventos> Error al invocar al listener: ' + FListeners[I].GetAsObject.QualifiedClassName + ' - Error: ' + E.Message);
              if AMensaje = nil then
                FrBase.LogError('<Motor eventos> el mensaje es NIL!!!')
              else FrBase.LogError('<Motor eventos> tipo mensaje que ha provocado el error: ' + AMensaje.GetAsObject.QualifiedClassName);
            end;
          end;
        end
      end
      else begin
             FrBase.LogError('<TUhsThreadMensajes.ProcesarMensaje> Msg cancelado o expirado!!! ' + AMensaje.GetAsObject.QualifiedClassName);
             Break;
           end;
    end;
{$IFDEF PROBANDO}
    if not L then
    begin
      FrBase.LogError('<TUhsThreadMensajes.ProcesarMensaje> Msg no entregado a listeners!!! - ' + FListeners.Count.ToString + ' - ' + AMensaje.GetAsObject.QualifiedClassName);
    end;
{$ENDIF}
  finally
    FSincronizadorListeners.EndRead;
  end;
end;

procedure TUhsThreadMensajes.Register;
begin
  if FPool <> nil then
    FPool.AddThreadMensajes(Self)
  else
    FrMensajes.RegisterThreadMensajes(Self);
end;

procedure TUhsThreadMensajes.RegisterListener(AMensajesListener: IMensajeListener);
begin
  FSincronizadorListeners.BeginWrite;
  try
    if (not FListeners.Contains(AMensajesListener)) then
      FListeners.Add(AMensajesListener);
  finally
    FSincronizadorListeners.EndWrite;
  end;
end;

procedure TUhsThreadMensajes.Tick(const ADelta, AStartTime: Double);
begin;
  ProcessEvents;
end;

procedure TUhsThreadMensajes.UnRegister;
begin
  if FPool <> nil then
    FPool.RemoveThreadMensajes(Self)
  else
    FrMensajes.UnregisterThreadMensajes(Self);
end;

procedure TUhsThreadMensajes.UnregisterListener(AMensajesListener: IMensajeListener);
begin
  FSincronizadorListeners.BeginWrite;
  try
    if FListeners.Contains(AMensajesListener) then
      FListeners.remove(AMensajesListener);
  finally
    FSincronizadorListeners.EndWrite;
  end;
end;
{$ENDREGION}
{$REGION 'FrMensajes' }

class procedure FrMensajes.CreateIni;
begin
  FScheduler       := TMensajesScheduler.Create('TMensajesScheduler');
  FThreadsMensajes := TCollections.CreateList<TUhsThreadMensajes>;
  FPools           := TCollections.CreateList<TPoolMensajes>;
  FTerminando      := False;

  FSincronizadorThreadsMensajes := TMREWSync.Create;
  FSincronizadorPools           := TMREWSync.Create;

  FColeccionRetroConvertidores        := TCollections.CreateDictionary<string, FuncionRetroConversion>;
  FColeccionProConvertidores          := TCollections.CreateList<FuncionProConversion>;
  FDiccionarioProcesadoresRegistrados := TCollections.CreateDictionary<string, string>;
end;

class procedure FrMensajes.DestroyIni;
begin
  FTerminando                  := True;
  FColeccionRetroConvertidores := nil;
  FColeccionProConvertidores   := nil;

  FSincronizadorThreadsMensajes := nil;
  FSincronizadorPools           := nil;

  FDiccionarioProcesadoresRegistrados := nil;

  FPools           := nil;
  FThreadsMensajes := nil;

  FScheduler.Destroy;
end;

class procedure FrMensajes.EliminarProConvertidor(const AConversor: FuncionProConversion);
begin
  FrMensajes.FColeccionProConvertidores.Remove(AConversor);
end;

class procedure FrMensajes.EliminarRetroConvertidor(const ATipoMensaje: TUhsTipoMensaje);
begin
  FrMensajes.FColeccionRetroConvertidores.Remove(ATipoMensaje.QualifiedClassName)
end;

class function FrMensajes.ProConvertirMensaje(const AMensaje: String): IMensaje;
var
  Conversor: FuncionProConversion;
begin
  Result := nil;
  for Conversor in FrMensajes.FColeccionProConvertidores do
  begin
    Result := Conversor(AMensaje);
    if Result <> nil then
    begin
      Exit;
    end;
  end;
  if Result = nil then
  begin
    Result := TMensaje_StringRaw.Create;
    TMensaje_StringRaw(Result).Data.init(AMensaje, Now);
  end
end;

class procedure FrMensajes.RegistrarProcesador(const Descripcion, DescriptorClase: string);
begin
  FrMensajes.FDiccionarioProcesadoresRegistrados.AddOrSetValue(Descripcion, DescriptorClase);
end;

class function FrMensajes.GetProcesadoresRegistrados: IDictionary<string, string>;
var
  Datos: TPair<string, string>;
begin
  Result := TCollections.CreateDictionary<string, string>;
  for Datos in FrMensajes.FDiccionarioProcesadoresRegistrados do
    Result.AddOrSetValue(Datos.Key, Datos.value);
end;

class procedure FrMensajes.QueueEvent(AMensaje: IMensaje);
begin
  if FTerminando then
    Exit;
  //si hay que distribuir el mensaje con un delay temporal
  if AMensaje.DispatchAfter > 0 then
  begin
    AMensaje.DispatchAt := (AMensaje.DispatchTime + AMensaje.DispatchAfter);
    AMensaje.State      := msScheduled;
    FScheduler.ScheduleMensaje(AMensaje);
  end
  else
  begin
    if TMensajeTarget.mdThreads in AMensaje.DispatchTargets then
      QueueInThreads(AMensaje);
    if TMensajeTarget.mdPools in AMensaje.DispatchTargets then
      QueueInPools(AMensaje);
  end;
end;

class procedure FrMensajes.QueueInPools(AMensaje: IMensaje);
var
  I: Integer;
begin
{$IFDEF PROBANDO}
  FrBase.LogInfo('<FrMensajes.QueueInPools>');
{$ENDIF}
  if FTerminando then
    Exit;
  FSincronizadorPools.BeginRead;
  try
    for I := 0 to FPools.Count - 1 do
    begin
{$IFDEF PROBANDO}
      FrBase.LogInfo('<FrMensajes.QueueInPools> 1: ' + FPools[I].QualifiedClassName);
{$ENDIF}
      FPools[I].AddMensaje(AMensaje);
    end;
  finally
    FSincronizadorPools.EndRead;
  end;
end;

class procedure FrMensajes.QueueInThreads(AMensaje: IMensaje);
var
  I: Integer;
begin
  if FTerminando then
    Exit;
  FSincronizadorThreadsMensajes.BeginRead;
  try
    for I := 0 to FThreadsMensajes.Count - 1 do
      if FThreadsMensajes[I].GetEventRelevant(AMensaje) then
        FThreadsMensajes[I].AddMensaje(AMensaje);
  finally
    FSincronizadorThreadsMensajes.EndRead;
  end;
end;

class procedure FrMensajes.RegisterPool(const APoolMensajes: TPoolMensajes);
begin
  FSincronizadorPools.BeginWrite;
  try
    if (not FPools.Contains(APoolMensajes)) then
      FPools.Add(APoolMensajes);
  finally
    FSincronizadorPools.EndWrite
  end;
end;

class procedure FrMensajes.RegisterThreadMensajes(const AThreadMensajes: TUhsThreadMensajes);
begin
  if FTerminando then
    Exit;

  FSincronizadorThreadsMensajes.BeginWrite;
  try
    if (not FThreadsMensajes.Contains(AThreadMensajes)) then
      FThreadsMensajes.Add(AThreadMensajes);
  finally
    FSincronizadorThreadsMensajes.EndWrite;
  end;
end;

class procedure FrMensajes.RegistrarProConvertidor(const AConversor: FuncionProConversion);
begin
  FrMensajes.FColeccionProConvertidores.Add(AConversor);
end;

class procedure FrMensajes.RegistrarRetroConvertidor(const ATipoMensaje: TUhsTipoMensaje; const AConversor: FuncionRetroConversion);
begin
  FrMensajes.FColeccionRetroConvertidores.AddOrSetValue(ATipoMensaje.QualifiedClassName, AConversor);
end;

class function FrMensajes.RetroConvertirMensaje(AMensaje: IMensaje): String;
var
  Conversor: FuncionRetroConversion;
begin
  Result := '';
  if AMensaje.GetAsObject is TMensaje_StringRaw then
  begin
    Result := TMensaje_StringRaw(AMensaje).Data.RawString;
  end
  else
  begin
    if FrMensajes.FColeccionRetroConvertidores.TryGetValue(AMensaje.GetAsObject.QualifiedClassName, Conversor) then
      Result := Conversor(AMensaje);
  end;
end;

class procedure FrMensajes.UnregisterThreadMensajes(const AThreadMensajes: TUhsThreadMensajes);
var
  LIndex: Integer;
begin
  if FTerminando then
    Exit;
  FSincronizadorThreadsMensajes.BeginWrite;
  try
    LIndex := FThreadsMensajes.IndexOf(AThreadMensajes);
    if LIndex > -1 then
      FThreadsMensajes.Delete(LIndex);
  finally
    FSincronizadorThreadsMensajes.EndWrite;
  end;
end;

class procedure FrMensajes.UnregisterPool(const APoolMensajes: TPoolMensajes);
var
  LIndex: Integer;
begin
  FSincronizadorPools.BeginWrite;
  try
    LIndex := FPools.IndexOf(APoolMensajes);
    if LIndex > -1 then
      FPools.Delete(LIndex);
  finally
    FSincronizadorPools.EndWrite;
  end;
end;
{$ENDREGION}

{$REGION 'TUhsMensajeListener<T>'}

procedure TUhsMensajeListener<T>.AddProcesador(AProcesador: IProcesadorMensaje);
begin
  FListaProcesadores.Add(AProcesador)
end;

constructor TUhsMensajeListener<T>.Create(const AThreadMensajes: TUhsThreadMensajes; const AOnEventCallback: TEventCallbackUnbound; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic);
begin
  inherited Create(AThreadMensajes, ARegistrationMode);
  FOnEventUnbound    := AOnEventCallback;
  FListaProcesadores := TCollections.CreateList<IProcesadorMensaje>;
  FOnMensaje         := FrBase.CreateEventoBase<TNotifyNuevoMensaje>;
end;

constructor TUhsMensajeListener<T>.Create(const AThreadMensajes: TUhsThreadMensajes; const AOnEventCallback: TEventCallbackOfObject; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic);
begin
  inherited Create(AThreadMensajes, ARegistrationMode);
  FOnEventOfObject   := AOnEventCallback;
  FListaProcesadores := TCollections.CreateList<IProcesadorMensaje>;
  FOnMensaje         := FrBase.CreateEventoBase<TNotifyNuevoMensaje>;
end;

constructor TUhsMensajeListener<T>.Create(const AThreadMensajes: TUhsThreadMensajes; const AOnEventCallback: TEventCallbackAnonymous; const ARegistrationMode: TMensajeRegistrationMode = mrmAutomatic);
begin
  inherited Create(AThreadMensajes, ARegistrationMode);
  FOnEventAnonymous  := AOnEventCallback;
  FListaProcesadores := TCollections.CreateList<IProcesadorMensaje>;
  FOnMensaje         := FrBase.CreateEventoBase<TNotifyNuevoMensaje>;
end;

constructor TUhsMensajeListener<T>.Create(const AThreadMensajes: TUhsThreadMensajes; const ARegistrationMode: TMensajeRegistrationMode);
begin
  inherited Create(AThreadMensajes, ARegistrationMode);
  FListaProcesadores := TCollections.CreateList<IProcesadorMensaje>;
  FOnMensaje         := FrBase.CreateEventoBase<TNotifyNuevoMensaje>;
end;

constructor TUhsMensajeListener<T>.Create(const APoolThreadMensajes: TPoolMensajes; const AOnEventCallback: TEventCallbackUnbound; const ARegistrationMode: TMensajeRegistrationMode);
begin
  inherited Create(APoolThreadMensajes, ARegistrationMode);
  FOnEventUnbound    := AOnEventCallback;
  FListaProcesadores := TCollections.CreateList<IProcesadorMensaje>;
  FOnMensaje         := FrBase.CreateEventoBase<TNotifyNuevoMensaje>;
end;

constructor TUhsMensajeListener<T>.Create(const APoolThreadMensajes: TPoolMensajes; const ARegistrationMode: TMensajeRegistrationMode);
begin
  inherited Create(APoolThreadMensajes, ARegistrationMode);
  FListaProcesadores := TCollections.CreateList<IProcesadorMensaje>;
  FOnMensaje         := FrBase.CreateEventoBase<TNotifyNuevoMensaje>;
end;

constructor TUhsMensajeListener<T>.Create(const APoolThreadMensajes: TPoolMensajes; const AOnEventCallback: TEventCallbackAnonymous; const ARegistrationMode: TMensajeRegistrationMode);
begin
  inherited Create(APoolThreadMensajes, ARegistrationMode);
  FOnEventAnonymous  := AOnEventCallback;
  FListaProcesadores := TCollections.CreateList<IProcesadorMensaje>;
  FOnMensaje         := FrBase.CreateEventoBase<TNotifyNuevoMensaje>;
end;

constructor TUhsMensajeListener<T>.Create(const APoolThreadMensajes: TPoolMensajes; const AOnEventCallback: TEventCallbackOfObject; const ARegistrationMode: TMensajeRegistrationMode);
begin
  inherited Create(APoolThreadMensajes, ARegistrationMode);
  FOnEventOfObject   := AOnEventCallback;
  FListaProcesadores := TCollections.CreateList<IProcesadorMensaje>;
  FOnMensaje         := FrBase.CreateEventoBase<TNotifyNuevoMensaje>;
end;

destructor TUhsMensajeListener<T>.Destroy;
begin
  FListaProcesadores := nil;
  FOnMensaje         := nil;
  inherited;
end;

function TUhsMensajeListener<T>.GetMensajeClass: TClass;
begin
  Result := T;
end;

function TUhsMensajeListener<T>.GetOnMensaje: IEvento<TNotifyNuevoMensaje>;
begin
  Result := FOnMensaje
end;

procedure TUhsMensajeListener<T>.MensajeNuevo(AMensaje: IMensaje);
begin
  inherited MensajeNuevo(AMensaje);
  if Assigned(FOnEventUnbound) then
    FOnEventUnbound(AMensaje)
  else if Assigned(FOnEventOfObject) then
    FOnEventOfObject(AMensaje)
  else if Assigned(FOnEventAnonymous) then
    FOnEventAnonymous(AMensaje);
  FOnMensaje.Invoke(AMensaje);
end;

procedure TUhsMensajeListener<T>.RemoveProcesador(AProcesador: IProcesadorMensaje);
begin
  if FListaProcesadores.Contains(AProcesador) then
    FListaProcesadores.Remove(AProcesador)
end;

{$ENDREGION}
{$REGION 'TMensajesScheduler'}

function TMensajesScheduler.Checks: Boolean;
begin
  // Pull once to prevent unnecessary locking
  if FSaliendo then Exit;

  if FCacheMensajes.Count > 0 then
  begin
    if FCacheMensajes[0].DispatchAt <= FrPlataforma.GetReferenceTime then // Is it time to dispatch this Event yet?
    begin
      FCacheMensajes[0].DispatchAfter := 0;
      FCacheMensajes[0].DispatchTime  := FrPlataforma.GetReferenceTime;
      FrMensajes.QueueEvent(FCacheMensajes[0]); // ... Dispatch to the queue...
      FCacheMensajes.Delete(0); // ... and delete the entry from the Schedule
    end
  end
end;

constructor TMensajesScheduler.Create;
begin
  inherited;
  FNextEventTime := 0;
  //FMensajes      := TUhsThreadedQueue<IMensaje>.Create(CTE_INITIAL_QUEUE_SIZE, CTE_PUSH_TIMEOUT, FrMath.INFINITO);
  FMensajes      := TUhsThreadedQueue.Create(CTE_INITIAL_QUEUE_SIZE, CTE_PUSH_TIMEOUT, FrMath.INFINITO);
  FCacheMensajes := TCollections.CreateList<IMensaje>(TComparison<IMensaje>(
    function(ALeft, ARight: IMensaje): Integer
    begin
      Result := 0;
      if (ALeft.DispatchAt < ARight.DispatchAt) then
        Exit(-1)
      else if (ALeft.DispatchAt > ARight.DispatchAt) then
        Exit(1);
    end));
  FSaliendo      := False;
  Wake;
end;

destructor TMensajesScheduler.Destroy;
begin
  Self.ThreadState := TUhsThreadState.tsPaused;
  FSaliendo := True;
  FMensajes.Free;
  FCacheMensajes := nil;
  inherited;
end;

function TMensajesScheduler.GetInitialThreadState: TUhsThreadState;
begin
  Result := tsPaused;
end;

procedure TMensajesScheduler.LanzarProcesamiento;
var
  LMsg                 : IMensaje;
  LMsgEspera           : IMensaje;
  LRes                 : TWaitResult;
  LSize                : Integer;
begin
  while not(FSaliendo) do
  begin
    // Nuevas tareas
    repeat
      if FSaliendo then Exit;
      case FCacheMensajes.Count of
        0:
          begin
            LRes := ObtenerSiguienteMsg(LSize, LMsg);
          end
      else
        begin
          LMsgEspera          := FCacheMensajes[0];
          LRes                := ObtenerSiguienteMsg(LSize, LMsg, Trunc(LMsgEspera.DispatchAt - FrPlataforma.GetReferenceTime));
        end;
      end;
      case LRes of
        wrSignaled:
          begin
            if (not FSaliendo) then
            begin
              FCacheMensajes.Add(LMsg);
            end
            else
            begin
              Exit;
            end;
          end;
        wrAbandoned:
          begin
            Break;
          end;
      end;
    until (LSize = 0) or (LRes = TWaitResult.wrTimeout);
    Checks;
  end;
end;

function TMensajesScheduler.PublicarDato(AMsg: IMensaje; var ASize: Integer): TWaitResult;
var
  LDato: TValue;
begin
  LDato := TValue.From<IMensaje>(AMsg);
  Result := FMensajes.PushItem(LDato, ASize);
end;

function TMensajesScheduler.ObtenerSiguienteMsg(out AQueueSize: Integer; var AMsg: IMensaje; const ATimeOut: Cardinal): TWaitResult;
var
  LData: TValue;
begin
  Result := FMensajes.PopItem(AQueueSize, LData, ATimeOut);
  if Result = TWaitResult.wrSignaled then
    AMsg   := LData.ToType<IMensaje>;
end;

function TMensajesScheduler.ObtenerSiguienteMsg(out AQueueSize: Integer; var AMsg: IMensaje): TWaitResult;
var
  LData: TValue;
begin
  Result := FMensajes.PopItem(AQueueSize, LData);
  if Result = TWaitResult.wrSignaled then
    AMsg   := LData.ToType<IMensaje>;
end;

procedure TMensajesScheduler.ScheduleMensaje(AMensaje: IMensaje);
var
  LSize: Integer;
  LRes : TWaitResult;
begin
  repeat
    LRes := PublicarDato(AMensaje, LSize);
    case LRes of
      wrTimeout:
        begin
          FMensajes.Grow(LSize);
          if FSaliendo then Exit;
        end;
    end;
  until LRes = TWaitResult.wrSignaled;
end;

procedure TMensajesScheduler.Tick(const ADelta, AStartTime: Double);
begin
  LanzarProcesamiento;
end;
{$ENDREGION}
{$REGION 'TPoolMensajes'}

procedure TPoolMensajes.AdquireRead;
begin
  FSincronizador.BeginRead;
end;

procedure TPoolMensajes.AdquireWrite;
begin
  FSincronizador.BeginWrite;
end;

procedure TPoolMensajes.AddThreadMensajes(const AThreadMensajes: TUhsThreadMensajes);
begin
  if not(AThreadMensajes is GetMensajeThreadType) then
    raise EMensajePoolThreadTypeMismatch.CreateFmt('Event Pool quiere Threads de Mensajes del tipo "%s", pero se ha intentado registrar un thread de mensajes del tipo "%s"', [GetMensajeThreadType.ClassName, AThreadMensajes.ClassName]);
  AdquireWrite;
  try
    if (not FThreadsMensajes.Contains(AThreadMensajes)) then
      FThreadsMensajes.Add(AThreadMensajes);
  finally
    ReleaseWrite;
  end;
end;

procedure TPoolMensajes.AfterConstruction;
begin
  inherited;
  CreateThreads;
  if FRegistrationMode = mrmAutomatic then
    Register;
end;

constructor TPoolMensajes.Create(const AThreadCount: Integer; const ARegistrationMode: TMensajeRegistrationMode);
begin
  inherited Create(QualifiedClassName);
  FThreadCount       := AThreadCount;
  FThreadCountTarget := AThreadCount;
  FRegistrationMode  := ARegistrationMode;
  FThreadsMensajes   := TCollections.CreateList<TUhsThreadMensajes>;
  FSincronizador     := TMREWSync.Create;
  Wake;
end;

procedure TPoolMensajes.CreateThreads;
var
  I: Integer;
begin
  for I := 1 to FThreadCount do
    GetMensajeThreadType.Create(Self, mrmAutomatic);
end;

destructor TPoolMensajes.Destroy;
begin
  DestroyThreads;
  FThreadsMensajes := nil;
  FSincronizador   := nil;
  inherited;
end;

procedure TPoolMensajes.DestroyThreads;
var
  I, LCount: Integer;
begin
  Lock;
  try
    FThreadCount       := 0;
    FThreadCountTarget := 0;
    AdquireWrite;
    try
      LCount := FThreadsMensajes.Count;
      for I  := LCount - 1 downto 0 do
        FThreadsMensajes[I].Free;
    finally
      ReleaseWrite;
    end;
  finally
    Unlock;
  end;
end;

function TPoolMensajes.GetThreadCount: Integer;
begin
  Lock;
  try
    Result := FThreadCount;
  finally
    Unlock;
  end;
end;

function Comparador_TUhsThreadMensajes(const DatoI, DatoD: TUhsThreadMensajes): Integer;
var
  LThisThreadEventCountI, LThisThreadEventCountD                        : Int64;
  LThisThreadScoreI, LThisThreadScoreD: Double;
begin
  Result := 0;
  if (DatoI = DatoD) then
    Exit(0);
  LThisThreadEventCountI := DatoI.NoEventosTratados;
  LThisThreadEventCountD := DatoD.NoEventosTratados;
  if DatoI.EventRateAverage > 0 then
    LThisThreadScoreI := LThisThreadEventCountI / DatoI.EventRateAverage
  else
    LThisThreadScoreI := LThisThreadEventCountI / 60;
  if DatoD.EventRateAverage > 0 then
    LThisThreadScoreD := LThisThreadEventCountD / DatoD.EventRateAverage
  else
    LThisThreadScoreD := LThisThreadEventCountD / 60;
  //como mucho que haya una distancia de 20 msg tratados entre threads del pool
  if Abs(LThisThreadEventCountI - LThisThreadEventCountD) >= 20 then
  begin
    if LThisThreadEventCountI < LThisThreadEventCountD then
      Exit(-1);
    if LThisThreadEventCountI > LThisThreadEventCountD then
      Exit(1);
    end;
  //FrDiagnosticos.IdeDebugMsgConFechaHora('<comparer> ' + DatoI.ThreadID.ToString + ' - ' + LThisThreadScoreI.ToString + ' - ' + LThisThreadEventCountI.ToString + ' - ' + DatoI.EventRateAverage.ToString);
  //FrDiagnosticos.IdeDebugMsgConFechaHora('<comparer> ' + DatoD.ThreadID.ToString + ' - ' + LThisThreadScoreD.ToString + ' - ' + LThisThreadEventCountD.ToString + ' - ' + DatoD.EventRateAverage.ToString);
  if LThisThreadScoreI < LThisThreadScoreD then
    Exit(-1);
  if LThisThreadScoreI > LThisThreadScoreD then
    Exit(1);
  if LThisThreadEventCountI < LThisThreadEventCountD then
    Exit(-1);
  if LThisThreadEventCountI > LThisThreadEventCountD then
    Exit(1);
end;

procedure TPoolMensajes.PoolMensaje(AMensaje: IMensaje);
var
  LFinal                                    : IEnumerable<TUhsThreadMensajes>;
  LData                                     : IList<TUhsThreadMensajes>;
  LSeleccion                                : TUhsThreadMensajes;
begin
  AdquireRead;
  try
{$IFDEF PROBANDO}
    FrBase.LogInfo('<TPoolMensajes.PoolMensaje> Count: ' + FThreadsMensajes.Count.ToString + ' - ' + Self.QualifiedClassName);
{$ENDIF}
    LData := TCollections.CreateList<TUhsThreadMensajes>;
    LData.AddRange(FThreadsMensajes.ToArray);
    LData.Sort(Comparador_TUhsThreadMensajes);
    //FThreadsMensajes.Sort(Comparador_TUhsThreadMensajes);
{$IFDEF PROBANDO}
    FrBase.LogInfo('<TPoolMensajes.PoolMensaje> Count (*): ' + LData.Count.ToString + ' - ' + Self.QualifiedClassName);
{$ENDIF}
    LFinal := LData.Where(function (const AData: TUhsThreadMensajes): Boolean
                                   begin
                                     Result := AData.GetEventRelevant(AMensaje);
                                   end);
{$IFDEF PROBANDO}
    FrBase.LogInfo('<TPoolMensajes.PoolMensaje> LData.Count: ' + LFinal.Count.ToString + ' - ' + Self.QualifiedClassName);
{$ENDIF}
    if LFinal.Count <> 0 then
    begin
{$IFDEF PROBANDO}
      FrBase.LogInfo('<TPoolMensajes.PoolMensaje> Asignado al thread - ' + Self.QualifiedClassName);
{$ENDIF}
      LSeleccion := LFinal.First;
      LSeleccion.AddMensaje(AMensaje);
    end
    else FrBase.LogError('<TPoolMensajes.PoolMensaje> ATENCION! Ningun thread seleccionado!!! ' + Self.QualifiedClassName);
  finally
    ReleaseRead;
  end;
end;

procedure TPoolMensajes.ProcesarMensaje(AMensaje: IMensaje);
begin
  if FThreadsMensajes.Count > 0 then
    PoolMensaje(AMensaje);
end;

procedure TPoolMensajes.Register;
begin
  FrMensajes.RegisterPool(Self);
end;

procedure TPoolMensajes.RegisterListener(AMensajesListener: IMensajeListener);
var
  I: Integer;
begin
  AdquireRead;
  try
    for I := 0 to FThreadsMensajes.Count - 1 do
    begin
      FThreadsMensajes[I].RegisterListener(AMensajesListener);
    end;
  finally
    ReleaseRead;
  end;
end;

procedure TPoolMensajes.ReleaseRead;
begin
  FSincronizador.EndRead
end;

procedure TPoolMensajes.ReleaseWrite;
begin
  FSincronizador.EndWrite
end;

procedure TPoolMensajes.RemoveThreadMensajes(const AThreadMensajes: TUhsThreadMensajes);
var
  LIndex: Integer;
begin
  AdquireWrite;
  try
    LIndex := FThreadsMensajes.IndexOf(AThreadMensajes);
    if LIndex > -1 then
      FThreadsMensajes.Delete(LIndex);
  finally
    ReleaseWrite;
  end;
end;

procedure TPoolMensajes.SetThreadCount(const AThreadCount: Integer);
begin
  Lock;
  try
    FThreadCountTarget := AThreadCount;
  finally
    Unlock;
  end;
end;

procedure TPoolMensajes.Tick(const ADelta, AStartTime: Double);
begin
  ProcessEvents;
end;

procedure TPoolMensajes.UnRegister;
begin
  FrMensajes.UnregisterPool(Self);
end;

procedure TPoolMensajes.UnregisterListener(AMensajesListener: IMensajeListener);
var
  I: Integer;
begin
  AdquireRead;
  try
    for I := 0 to FThreadsMensajes.Count - 1 do
    begin
      FThreadsMensajes[I].UnregisterListener(AMensajesListener);
    end;
  finally
    ReleaseRead;
  end;
end;

{$ENDREGION}
{$REGION 'TPoolMensajes<T>'}

function TPoolMensajes<T>.GetMensajeThreadType: TTipoThreadMensajes;
begin
  Result := T;
end;
{$ENDREGION}
{ RDato_StringRaw }

procedure RDato_StringRaw.init(const ARawString: string; const AFecha: TDateTime);
begin
  RawString := ARawString;
  Fecha     := AFecha;
end;

initialization

FrBase.ContenedorGlobal.RegisterType<TPoolMensajes_GENERAL>.DelegateTo(
  function: TPoolMensajes_GENERAL
  begin
    Result := TPoolMensajes_GENERAL.Create(FrBase.iif<Integer>((TThread.ProcessorCount > 2), 2, TThread.ProcessorCount));
  end).AsSingleton;

FrBase.ContenedorGlobal.RegisterType<TPoolMensajes_GENERAL_OneThread>.DelegateTo(
  function: TPoolMensajes_GENERAL_OneThread
  begin
    Result := TPoolMensajes_GENERAL_OneThread.Create(1)
  end).AsSingleton;

FrMensajes.CreateIni;

finalization

FrMensajes.DestroyIni;

end.
