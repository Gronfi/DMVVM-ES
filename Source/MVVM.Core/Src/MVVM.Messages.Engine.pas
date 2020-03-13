unit MVVM.Messages.Engine;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,

  Spring,
  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types;

type

  { Forward Declarations }
  TMessage                  = class;
  TMessageClass             = class of TMessage;
  TThreadMessageHandlerBase = class;
  TThreadMessageHandler     = class;
  TMessageChannel           = class;
  TThreadMessageHandlerType = class of TThreadMessageHandler;

{$REGION 'TMessage'}
  TMessage = class abstract(TInterfacedObject, IMessage)
  private
    FCreationDateTime: TDateTime;
  protected
    function GetCreationDateTime: TDateTime;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Queue; virtual;
    function GetAsObject: TObject;

    class function GetMensajeType: TMessageClass; inline;

    property CreationDateTime: TDateTime read GetCreationDateTime;
  end;
{$ENDREGION}

{$REGION 'TMessageListener'}
  TMessageListener = class abstract(TInterfacedObject, IMessageListener, IObject)
  private
    FIsCodeToExecuteInUIMainThread : Boolean;
    FThreadMensajes                : TThreadMessageHandler;
    FPoolMensajes                  : TMessageChannel;
    FTypeRestriction               : EMessageTypeRestriction;
    FFilterCondition               : TListenerFilter;

    function GetIsCodeToExecuteInUIMainThread: Boolean;
    procedure SetIsCodeToExecuteInUIMainThread(const AValue: Boolean);

    function GetThreadMensajes: TThreadMessageHandler;

    function GetTypeRestriction: EMessageTypeRestriction;
    procedure SetTypeRestriction(const ATypeRestriction: EMessageTypeRestriction);

    function GetListenerFilter: TListenerFilter;
    procedure SetListenerFilter(const AFilter: TListenerFilter);

  protected
    function GetDefaultTypeRestriction: EMessageTypeRestriction; virtual;
  public
    procedure AfterConstruction; override;

    constructor Create(const AThreadMensajes: TThreadMessageHandler; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); reintroduce; overload; virtual;
    constructor Create(const APoolThreadsMensajes: TMessageChannel; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); reintroduce; overload; virtual;

    destructor Destroy; override;

    function GetConditionsMatch(AMessage: IMessage): Boolean; virtual;

    function GetMensajeClass: TClass; virtual; abstract;

    procedure Register;
    procedure UnRegister;

    function GetAsObject: TObject;

    procedure NewMessage(AMessage: IMessage); virtual;

    property IsCodeToExecuteInUIMainThread: Boolean read GetIsCodeToExecuteInUIMainThread write SetIsCodeToExecuteInUIMainThread;
    property FilterCondition: TListenerFilter read GetListenerFilter write SetListenerFilter;
    property TypeRestriction: EMessageTypeRestriction read GetTypeRestriction write SetTypeRestriction;
  end;

  TMessageListener<T: TMessage> = class abstract(TMessageListener)
  private
    FOnMessage: IEvent<TNotifyMessage>;
  protected
    function GetOnMessage: IEvent<TNotifyMessage>;
    procedure NewMessage(AMessage: IMessage); override; final;
  public
    constructor Create(const AThreadMensajes: TThreadMessageHandler; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload; override;
    constructor Create(const APoolThreadsMensajes: TMessageChannel; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload; override;
    destructor Destroy; override;

    function GetMensajeClass: TClass; override; final;

    property OnMessage: IEvent<TNotifyMessage> read GetOnMessage;
  end;

  TMessageListenerModel<T: TMessage; K: IModel> = class(TMessageListener<T>)
  private
    FModel: IModel;
  public
    constructor Create(AModel: IModel; const AThreadMensajes: TThreadMessageHandler; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload;
    constructor Create(AModel: IModel; const APoolThreadsMensajes: TMessageChannel; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload;
  end;

  TMessageListenerViewModel<T: TMessage; K: IViewModel> = class(TMessageListener<T>)
  private
    FViewModel: IViewModel;
  public
    constructor Create(AViewModel: IViewModel; const AThreadMensajes: TThreadMessageHandler; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload;
    constructor Create(AViewModel: IViewModel; const APoolThreadsMensajes: TMessageChannel; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload;
  end;

{$ENDREGION}

{$REGION 'TThreadedQueue<T>'}
  TThreadedQueue<T> = class
  private
    FQueue: array of T;
    FQueueSize, FQueueOffset: Integer;
    FQueueUnLock,
    FQueueNotEmpty,
    FQueueNotFull: TObject;
    FQueueLock: TObject;
    FShutDown: Boolean;
    FPushTimeout, FPopTimeout: Cardinal;
    FTotalItemsPushed, FTotalItemsPopped: Cardinal;

  public
    constructor Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
    destructor Destroy; override;

    procedure Grow(ADelta: Integer);
    function PushItem(AItem: T): TWaitResult; overload;
    function PushItem(AItem: T; var AQueueSize: Integer): TWaitResult; overload;
    function PopItem: T; overload;
    function PopItem(var AQueueSize: Integer): T; overload;
    function PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult; overload;
    function PopItem(var AItem: T): TWaitResult; overload;
    procedure DoShutDown;

    function PushItem(AItem: T; var AQueueSize: Integer; const ATimeout: Cardinal): TWaitResult; overload;
    function PopItem(var AQueueSize: Integer; var AItem: T; const ATimeout: Cardinal): TWaitResult; overload;

    property QueueSize: Integer read FQueueSize;
    property ShutDown: Boolean read FShutDown;
    property TotalItemsPushed: Cardinal read FTotalItemsPushed;
    property TotalItemsPopped: Cardinal read FTotalItemsPopped;
  end;
{$ENDREGION}

{$REGION 'TThreadMessageHandlerBase'}
  TThreadMessageHandlerBase = class abstract(TThread)
  const
    CTE_INITIAL_QUEUE_SIZE = 10;
    CTE_PUSH_TIMEOUT       = 100;
  private
    //FPerformance      : TPerformanceCounter;
    FSynchronizer : IReadWriteSync;
    FMessageCount : Int64;
    FMessages     : TThreadedQueue<IMessage>;

    procedure AdquireWrite;
    procedure ReleaseWrite;
    procedure AdquireRead;
    procedure ReleaseRead;

    procedure ProcessQueuedMessage(AMessage: IMessage);
    procedure ProcessMessages;

    function GetNextMessage(out AQueueSize: Integer; var AMessage: IMessage): TWaitResult;
  protected
    procedure Execute; override;

    function GetNoEventosTratados: Int64;

    procedure ProcessMessage(AMessage: IMessage); virtual; abstract;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;

    procedure AddMessage(AMessage: IMessage); virtual;

    property NoEventosTratados: Int64 read GetNoEventosTratados;
  end;

{$ENDREGION}

{$REGION 'TThreadMessageHandler'}
  TThreadMessageHandler = class(TThreadMessageHandlerBase)
  private
    FListeners             : IList<IMessageListener>;
    FSynchronizerListeners : IReadWriteSync;
    FPool                  : TMessageChannel;
  protected
    procedure ProcessMessage(AMessage: IMessage); override;

    procedure InitializeListeners; virtual;
    procedure FinalizeListeners; virtual;

    function GetListenersCount: Integer;
    function GetEventRelevant(AMessage: IMessage): Boolean; virtual;
  public
    procedure AfterConstruction; override;

    constructor Create; overload; override;
    constructor Create(const APoolMensajes: TMessageChannel); overload;
    destructor Destroy; override;

    procedure RegisterListener(AMessageListener: IMessageListener);
    procedure UnregisterListener(AMessageListener: IMessageListener);
    property ListenersCount: Integer read GetListenersCount;

    procedure Register;
    procedure UnRegister;
  end;
{$ENDREGION}

{$REGION 'TMessageChannel'}
  TMessageChannel = class abstract(TThreadMessageHandlerBase)
  private
    FSynchronizer    :  IReadWriteSync;
    FThreadsMensajes  : IList<TThreadMessageHandler>;
    //FRegistrationMode : TMensajeRegistrationMode;
    FThreadCount      : Integer;
    FThreadCountTarget: Integer;

    procedure AddThreadMensajes(const AThreadMensajes: TThreadMessageHandler);
    procedure RemoveThreadMensajes(const AThreadMensajes: TThreadMessageHandler);

    procedure CreateThreads;
    procedure DestroyThreads;

    function GetThreadCount: Integer;
    procedure SetThreadCount(const AThreadCount: Integer);

    procedure AdquireWrite;
    procedure ReleaseWrite;
    procedure AdquireRead;
    procedure ReleaseRead;
  protected
    function GetMensajeThreadType: TThreadMessageHandlerType; virtual; abstract;

    procedure ProcessMessage(AMessage: IMessage); override;
    /// <summary><c>Called only when there is at least one viable Event Thread in the Pool.</c></summary>
    procedure PoolMessage(AMessage: IMessage); virtual;
  public
    constructor Create(const AThreadCount: Integer); reintroduce;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Register;
    procedure UnRegister;

    procedure RegisterListener(AMessageListener: IMessageListener);
    procedure UnregisterListener(AMessageListener: IMessageListener);

    property ThreadCount: Integer read GetThreadCount write SetThreadCount;
  end;
{$ENDREGION}

{$REGION 'TMessageChannel<T>'}
  TMessageChannel<T: TThreadMessageHandler> = class(TMessageChannel)
  protected
    function GetMensajeThreadType: TThreadMessageHandlerType; override; final;
  end;
{$ENDREGION}

  TMessageChannel_GENERAL = class(TMessageChannel<TThreadMessageHandler>);
  TMessageChannel_GENERAL_OneThread = class(TMessageChannel<TThreadMessageHandler>);

{$REGION 'Messages'}
  Messages = record
  private
    class var FSynchronizerThreadsMensajes : IReadWriteSync;
    class var FSynchronizerPools           : IReadWriteSync;
    class var FThreadsMensajes             : IList<TThreadMessageHandler>;
    class var FPools                       : IList<TMessageChannel>;
    class var FTerminando                  : Boolean;

    class procedure CreateIni; static;
    class procedure DestroyIni; static;

    class procedure QueueInThreads(AMessage: IMessage); static;
    class procedure QueueInPools(AMessage: IMessage); static;
  public
    //Registra un pool de mensajes
    class procedure RegisterPool(const APoolMensajes: TMessageChannel); static;
    class procedure UnregisterPool(const APoolMensajes: TMessageChannel); static;

    //Registra un thread de mensajes
    class procedure RegisterThreadMensajes(const AThreadMensajes: TThreadMessageHandler); static;
    class procedure UnregisterThreadMensajes(const AThreadMensajes: TThreadMessageHandler); static;

    //Se pasa al motor de eventos un mensaje a transmitir (utilizará todos los métodos de transmisión posibles)
    class procedure QueueMessage(AMessage: IMessage); static;
  end;
{$ENDREGION}

{$REGION 'TMessage_Generic'}
  TMessage_Generic<T> = class(TMessage)
  public
    Data: T;
  end;

  TMessageListener_Generic<T> = class(TMessageListener<TMessage_Generic<T>>);
{$ENDREGION}

implementation

uses
  System.Generics.Defaults,
  System.Generics.Collections,

  MVVM.Core,
  MVVM.Utils;

{$REGION 'TThreadedQueueHelper<T>'}

constructor TThreadedQueue<T>.Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
begin
  inherited Create;
  SetLength(FQueue, AQueueDepth);
  FQueueLock := TObject.Create;
  FQueueNotEmpty := TObject.Create;
  FQueueUnLock   := TObject.Create;
  FQueueNotFull := TObject.Create;
  FPushTimeout := PushTimeout;
  FPopTimeout := PopTimeout;
end;

destructor TThreadedQueue<T>.Destroy;
begin
  DoShutDown;
  FQueueNotFull.Free;
  FQueueNotEmpty.Free;
  FQueueUnLock.Free;
  FQueueLock.Free;
  inherited;
end;

procedure TThreadedQueue<T>.Grow(ADelta: Integer);
var
  LQueue: array of T;
  LTmp  : Integer;
  LCnt  : Integer;
  I     : Integer;
begin
  TMonitor.Enter(FQueueLock);
  try
    LTmp := Length(FQueue);
    if FQueueSize > 0 then
    begin
      SetLength(LQueue, LTmp);
      LCnt := 0;
      repeat
        LQueue[LCnt] := FQueue[(FQueueOffset + LCnt) mod LTmp];
        Inc(LCnt);
      until LCnt = FQueueSize;

      for I := 0 to FQueueSize - 1 do
        FQueue[I] := LQueue[I];
      for I := FQueueSize to LTmp - 1 do
        FQueue[I] := default(T);

      FQueueOffset := 0;
    end;

    LTmp := Length(FQueue) + ADelta;
    SetLength(FQueue, LTmp);
  finally
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.PulseAll(FQueueNotFull);
end;

function TThreadedQueue<T>.PopItem: T;
var
  LQueueSize: Integer;
begin
  PopItem(LQueueSize, Result);
end;

function TThreadedQueue<T>.PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult;
begin
  AItem := Default(T);

  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = 0) and not FShutDown do
      if not TMonitor.Wait(FQueueNotEmpty, FQueueLock, FPopTimeout) then
        Result := wrTimeout;

    if (FShutDown and (FQueueSize = 0)) or (Result <> wrSignaled) then
      Exit;

    AItem := FQueue[FQueueOffset];

    FQueue[FQueueOffset] := Default(T);

    Dec(FQueueSize);
    Inc(FQueueOffset);
    Inc(FTotalItemsPopped);

    if FQueueOffset = Length(FQueue) then
      FQueueOffset := 0;
  finally
    AQueueSize := FQueueSize;
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.Pulse(FQueueNotFull);
end;

function TThreadedQueue<T>.PopItem(var AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PopItem(LQueueSize, AItem);
end;

function TThreadedQueue<T>.PopItem(var AQueueSize: Integer): T;
begin
  PopItem(AQueueSize, Result);
end;

function TThreadedQueue<T>.PushItem(AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PushItem(AItem, LQueueSize);
end;

function TThreadedQueue<T>.PushItem(AItem: T; var AQueueSize: Integer): TWaitResult;
begin
  Result := Pushitem(AItem, AqueueSize, FPushTimeOut);
end;

procedure TThreadedQueue<T>.DoShutDown;
begin
  TMonitor.Enter(FQueueLock);
  try
    FShutDown := True;
  finally
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.PulseAll(FQueueNotFull);
  TMonitor.PulseAll(FQueueNotEmpty);
end;

function TThreadedQueue<T>.PushItem(AItem: T; var AQueueSize: Integer; const ATimeout: Cardinal): TWaitResult;
begin
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = Length(FQueue)) and not FShutDown do
      if not TMonitor.Wait(FQueueNotFull, FQueueLock, ATimeout) then
        Result := wrTimeout;

    if FShutDown or (Result <> wrSignaled) then
      Exit;

    FQueue[(FQueueOffset + FQueueSize) mod Length(FQueue)] := AItem;

    Inc(FQueueSize);
    Inc(FTotalItemsPushed);
  finally
    AQueueSize := FQueueSize;
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.Pulse(FQueueNotEmpty);
end;

function TThreadedQueue<T>.PopItem(var AQueueSize: Integer; var AItem: T; const ATimeout: Cardinal): TWaitResult;
begin
  AItem := Default(T);
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = 0) and not FShutDown do
      if not TMonitor.Wait(FQueueNotEmpty, FQueueLock, ATimeout) then
        Result := wrTimeout;

    if (FShutDown and (FQueueSize = 0)) or (Result <> wrSignaled) then
      Exit;

    AItem := FQueue[FQueueOffset];

    FQueue[FQueueOffset] := Default(T);

    Dec(FQueueSize);
    Inc(FQueueOffset);
    Inc(FTotalItemsPopped);

    if FQueueOffset = Length(FQueue) then
      FQueueOffset := 0;
  finally
    AQueueSize := FQueueSize;
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.Pulse(FQueueNotFull);
end;
{$ENDREGION}

{$REGION 'TMessage'}

constructor TMessage.Create;
begin
  inherited Create;
  FCreationDateTime := Now;
end;

destructor TMessage.Destroy;
begin
  inherited Destroy;
end;

function TMessage.GetAsObject: TObject;
begin
  Result := Self;
end;

function TMessage.GetCreationDateTime: TDateTime;
begin
  Result := FCreationDateTime;
end;

class function TMessage.GetMensajeType: TMessageClass;
begin
  Result := TMessageClass(Self);
end;

procedure TMessage.Queue;
begin
  //FrMensajes.QueueEvent(Self);
end;
{$ENDREGION}

{$REGION 'TMessageListener'}
procedure TMessageListener.AfterConstruction;
begin
  inherited;
  Register;
end;

constructor TMessageListener.Create(const APoolThreadsMensajes: TMessageChannel; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants);
begin
  Guard.CheckNotNull(APoolThreadsMensajes, 'The listener must have a Pool assigned');
  inherited Create;
  FIsCodeToExecuteInUIMainThread := ACodeExecutesInMainUIThread;
  FThreadMensajes                := nil;
  FPoolMensajes                  := APoolThreadsMensajes;
  FFilterCondition               := AFilterCondition;
end;

constructor TMessageListener.Create(const AThreadMensajes: TThreadMessageHandler; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants);
begin
  Guard.CheckNotNull(AThreadMensajes, 'The listener must have a Thread assigned');
  inherited Create;
  FIsCodeToExecuteInUIMainThread := ACodeExecutesInMainUIThread;
  FThreadMensajes           := AThreadMensajes;
  FPoolMensajes             := nil;
  FFilterCondition               := AFilterCondition;
end;

destructor TMessageListener.Destroy;
begin
  UnRegister;
  inherited;
end;

function TMessageListener.GetIsCodeToExecuteInUIMainThread: Boolean;
begin
  Result := FIsCodeToExecuteInUIMainThread
end;

function TMessageListener.GetAsObject: TObject;
begin
  Result := Self
end;

function TMessageListener.GetConditionsMatch(AMessage: IMessage): Boolean;
begin
  if Assigned(FFilterCondition) then
    Result := FFilterCondition(AMessage)
  else Result := True
end;

function TMessageListener.GetDefaultTypeRestriction: EMessageTypeRestriction;
begin
  Result := mtrAllowDescendants;
end;

function TMessageListener.GetListenerFilter: TListenerFilter;
begin
  Result := FFilterCondition
end;

function TMessageListener.GetThreadMensajes: TThreadMessageHandler;
begin
  Result := FThreadMensajes
end;

function TMessageListener.GetTypeRestriction: EMessageTypeRestriction;
begin
  Result := FTypeRestriction;
end;

procedure TMessageListener.NewMessage(AMessage: IMessage);
begin
  //
end;

procedure TMessageListener.Register;
begin
  if FThreadMensajes <> nil then
  begin
    FThreadMensajes.RegisterListener(Self);
  end
  else if FPoolMensajes <> nil then
       begin
         FPoolMensajes.RegisterListener(Self);
       end;
end;

procedure TMessageListener.SetIsCodeToExecuteInUIMainThread(const AValue: Boolean);
begin
  FIsCodeToExecuteInUIMainThread := AValue;
end;

procedure TMessageListener.SetListenerFilter(const AFilter: TListenerFilter);
begin
  FFilterCondition := AFilter
end;

procedure TMessageListener.SetTypeRestriction(const ATypeRestriction: EMessageTypeRestriction);
begin
  FTypeRestriction := ATypeRestriction;
end;

procedure TMessageListener.UnRegister;
begin
  if FThreadMensajes <> nil then
    FThreadMensajes.UnregisterListener(Self)
  else if FPoolMensajes <> nil then
         FPoolMensajes.UnregisterListener(Self);
end;
{$ENDREGION}

{$REGION 'TMessageListener<T>'}
constructor TMessageListener<T>.Create(const APoolThreadsMensajes: TMessageChannel; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  inherited;
  FOnMessage := Utils.CreateEvent<TNotifyMessage>;
end;

constructor TMessageListener<T>.Create(const AThreadMensajes: TThreadMessageHandler; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  inherited;
  FOnMessage := Utils.CreateEvent<TNotifyMessage>;
end;

destructor TMessageListener<T>.Destroy;
begin
  FOnMessage := nil;
  inherited;
end;

function TMessageListener<T>.GetMensajeClass: TClass;
begin
  Result := T;
end;

function TMessageListener<T>.GetOnMessage: IEvent<TNotifyMessage>;
begin
  Result := FOnMessage
end;

procedure TMessageListener<T>.NewMessage(AMessage: IMessage);
begin
  if FIsCodeToExecuteInUIMainThread then
  begin
    if MVVMCore.PlatformServices.IsMainThreadUI then
      MVVMCore.DelegateExecution<IMessage>(AMessage,
                                           procedure (AAMessage: IMessage)
                                           begin
                                             FOnMessage.Invoke(AAMessage)
                                           end, EDelegatedExecutionMode.medQueue)
    else FOnMessage.Invoke(AMessage);
  end
  else FOnMessage.Invoke(AMessage);
end;

{$ENDREGION}

{$REGION 'TThreadMessageHandlerBase'}

procedure TThreadMessageHandlerBase.AdquireRead;
begin
  FSynchronizer.BeginRead;
end;

procedure TThreadMessageHandlerBase.AdquireWrite;
begin
  FSynchronizer.BeginWrite;
end;

constructor TThreadMessageHandlerBase.Create;
begin
  inherited Create(False);
  FSynchronizer := TMREWSync.Create;
  FMessages     := TThreadedQueue<IMessage>.Create(CTE_INITIAL_QUEUE_SIZE, CTE_PUSH_TIMEOUT, Cardinal.MaxValue);
  FMessageCount := 0;
end;

destructor TThreadMessageHandlerBase.Destroy;
begin
  Terminate;
  WaitFor;
  FMessages.Free;
  FSynchronizer := nil;
  inherited Destroy;
end;

function TThreadMessageHandlerBase.GetNoEventosTratados: Int64;
begin
  Result := FMessageCount
end;

function TThreadMessageHandlerBase.GetNextMessage(out AQueueSize: Integer; var AMessage: IMessage): TWaitResult;
//var
//  LI   : IInterface;
//  LData: TValue;
begin
  Result := FMessages.PopItem(AQueueSize, AMessage);
//  if Result = TWaitResult.wrSignaled then
//  begin
//    LI       := LData.AsInterface;
//    AMessage := LI as IMessage;
//  end;
end;

procedure TThreadMessageHandlerBase.ProcessMessages;
var
  LRes                 : TWaitResult;
  LSize                : Integer;
  LMsg                 : IMessage;
begin
  while not(Terminated) do
  begin
    repeat
      LRes := GetNextMessage(LSize, LMsg);
      case LRes of
        wrSignaled:
          begin
            if not Terminated then
            begin
              try
                ProcessQueuedMessage(LMsg);
              except
                on E:Exception do
                begin
                  Utils.IdeDebugMsg('Exception at <TThreadMessageHandlerBase.ProcessEvents> ' + TThread.CurrentThread.ThreadID.ToString + ' - ' + LSize.ToString + ' - Error: ' + E.Message);
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

procedure TThreadMessageHandlerBase.ProcessQueuedMessage(AMessage: IMessage);
var
  LProcessStarted: Double;
begin
  //LProcessStarted := FrPlataforma.GetReferenceTime;
  ProcessMessage(AMessage);
  Inc(FMessageCount);
  //FPerformance.RecordSample(FrPlataforma.GetReferenceTime - LProcessStarted);
end;

procedure TThreadMessageHandlerBase.AddMessage(AMessage: IMessage);
var
  LSize: Integer;
  LRes : TWaitResult;
//  LData: TValue;
begin
  Guard.CheckNotNull(AMessage, 'The message can not be nil');
  repeat
    //LData:= TValue.From<IMessage>(AMessage);
    LRes := FMessages.PushItem(AMessage, LSize);
    case LRes of
      wrTimeout:
        begin
          FMessages.Grow(LSize);
          if Terminated then Exit;
        end;
    end;
  until LRes = TWaitResult.wrSignaled;
end;

procedure TThreadMessageHandlerBase.ReleaseRead;
begin
  FSynchronizer.EndRead;
end;

procedure TThreadMessageHandlerBase.ReleaseWrite;
begin
  FSynchronizer.EndWrite;
end;

procedure TThreadMessageHandlerBase.Execute;
begin
  ProcessMessages;
end;
{$ENDREGION}

{$REGION 'TThreadMessageHandler'}
procedure TThreadMessageHandler.AfterConstruction;
begin
  inherited;
  Register;
end;

constructor TThreadMessageHandler.Create;
begin
  inherited Create;
  FSynchronizerListeners:= TMREWSync.Create;
  FPool                 := nil;
  FListeners            := TCollections.CreateList<IMessageListener>;
  InitializeListeners;
end;

constructor TThreadMessageHandler.Create(const APoolMensajes: TMessageChannel);
begin
  Create;
  FPool := APoolMensajes;
end;

destructor TThreadMessageHandler.Destroy;
begin
  UnRegister;
  FinalizeListeners;
  FListeners        := nil;

  FSynchronizerListeners        := nil;
  inherited Destroy;
end;

procedure TThreadMessageHandler.FinalizeListeners;
var
  LListener: IMessageListener;
  LLista   : IList<IMessageListener>;
begin
  // Nada de momento
  LLista := TCollections.CreateList<IMessageListener>;
  FSynchronizerListeners.BeginRead;
  try
    for LListener in FListeners do
      LLista.Add(LListener)
  finally
    FSynchronizerListeners.EndRead;
  end;
  for LListener in LLista do
    LListener.UnRegister;
end;

function TThreadMessageHandler.GetEventRelevant(AMessage: IMessage): Boolean;
begin
  Result := True;
end;

function TThreadMessageHandler.GetListenersCount: Integer;
begin
  FSynchronizerListeners.BeginRead;
  try
    Result := FListeners.Count;
  finally
    FSynchronizerListeners.EndRead;
  end;
end;

procedure TThreadMessageHandler.InitializeListeners;
begin
  // Nada
end;

procedure TThreadMessageHandler.ProcessMessage(AMessage: IMessage);
var
  I  : Integer;
begin
  FSynchronizerListeners.BeginRead;
  try
    for I := 0 to FListeners.Count - 1 do
    begin
      if (((FListeners[I].TypeRestriction = mtrAllowDescendants) and (AMessage is FListeners[I].GetMensajeClass)) or ((FListeners[I].GetTypeRestriction = mtrDefinedTypeOnly) and (AMessage.GetAsObject.ClassType = FListeners[I].GetMensajeClass))) and
        (FListeners[I].GetConditionsMatch(AMessage)) then
      begin
        try
          FListeners[I].NewMessage(AMessage);
        except
          on E: Exception do
          begin
            Utils.IdeDebugMsg('Exception executing the listener: ' + FListeners[I].GetAsObject.QualifiedClassName + ' - Error: ' + E.Message);
          end;
        end;
      end;
    end;
  finally
    FSynchronizerListeners.EndRead;
  end;
end;

procedure TThreadMessageHandler.Register;
begin
  if FPool <> nil then
    FPool.AddThreadMensajes(Self)
  else
    Messages.RegisterThreadMensajes(Self);
end;

procedure TThreadMessageHandler.RegisterListener(AMessageListener: IMessageListener);
begin
  FSynchronizerListeners.BeginWrite;
  try
    if (not FListeners.Contains(AMessageListener)) then
      FListeners.Add(AMessageListener);
  finally
    FSynchronizerListeners.EndWrite;
  end;
end;

procedure TThreadMessageHandler.UnRegister;
begin
  if FPool <> nil then
    FPool.RemoveThreadMensajes(Self)
  else
    Messages.UnregisterThreadMensajes(Self);
end;

procedure TThreadMessageHandler.UnregisterListener(AMessageListener: IMessageListener);
begin
  FSynchronizerListeners.BeginWrite;
  try
    if FListeners.Contains(AMessageListener) then
      FListeners.remove(AMessageListener);
  finally
    FSynchronizerListeners.EndWrite;
  end;
end;
{$ENDREGION}

{$REGION 'Messages' }
class procedure Messages.CreateIni;
begin
  FThreadsMensajes := TCollections.CreateList<TThreadMessageHandler>;
  FPools           := TCollections.CreateList<TMessageChannel>;
  FTerminando      := False;

  FSynchronizerThreadsMensajes := TMREWSync.Create;
  FSynchronizerPools           := TMREWSync.Create;
end;

class procedure Messages.DestroyIni;
begin
  FTerminando                  := True;

  FSynchronizerThreadsMensajes := nil;
  FSynchronizerPools           := nil;

  FPools           := nil;
  FThreadsMensajes := nil;
end;

class procedure Messages.QueueMessage(AMessage: IMessage);
begin
  QueueInPools(AMessage);
end;

class procedure Messages.QueueInPools(AMessage: IMessage);
var
  I: Integer;
begin
  FSynchronizerPools.BeginRead;
  try
    for I := 0 to FPools.Count - 1 do
    begin
      FPools[I].AddMessage(AMessage);
    end;
  finally
    FSynchronizerPools.EndRead;
  end;
end;

class procedure Messages.QueueInThreads(AMessage: IMessage);
var
  I: Integer;
begin
  FSynchronizerThreadsMensajes.BeginRead;
  try
    for I := 0 to FThreadsMensajes.Count - 1 do
      if FThreadsMensajes[I].GetEventRelevant(AMessage) then
        FThreadsMensajes[I].AddMessage(AMessage);
  finally
    FSynchronizerThreadsMensajes.EndRead;
  end;
end;

class procedure Messages.RegisterPool(const APoolMensajes: TMessageChannel);
begin
  FSynchronizerPools.BeginWrite;
  try
    if (not FPools.Contains(APoolMensajes)) then
      FPools.Add(APoolMensajes);
  finally
    FSynchronizerPools.EndWrite
  end;
end;

class procedure Messages.RegisterThreadMensajes(const AThreadMensajes: TThreadMessageHandler);
begin
  if FTerminando then
    Exit;

  FSynchronizerThreadsMensajes.BeginWrite;
  try
    if (not FThreadsMensajes.Contains(AThreadMensajes)) then
      FThreadsMensajes.Add(AThreadMensajes);
  finally
    FSynchronizerThreadsMensajes.EndWrite;
  end;
end;

class procedure Messages.UnregisterThreadMensajes(const AThreadMensajes: TThreadMessageHandler);
var
  LIndex: Integer;
begin
  FSynchronizerThreadsMensajes.BeginWrite;
  try
    LIndex := FThreadsMensajes.IndexOf(AThreadMensajes);
    if LIndex > -1 then
      FThreadsMensajes.Delete(LIndex);
  finally
    FSynchronizerThreadsMensajes.EndWrite;
  end;
end;

class procedure Messages.UnregisterPool(const APoolMensajes: TMessageChannel);
var
  LIndex: Integer;
begin
  FSynchronizerPools.BeginWrite;
  try
    LIndex := FPools.IndexOf(APoolMensajes);
    if LIndex > -1 then
      FPools.Delete(LIndex);
  finally
    FSynchronizerPools.EndWrite;
  end;
end;
{$ENDREGION}

{$REGION 'TMessageChannel'}
procedure TMessageChannel.AdquireRead;
begin
  FSynchronizer.BeginRead;
end;

procedure TMessageChannel.AdquireWrite;
begin
  FSynchronizer.BeginWrite;
end;

procedure TMessageChannel.AddThreadMensajes(const AThreadMensajes: TThreadMessageHandler);
begin
  if not(AThreadMensajes is GetMensajeThreadType) then
    raise Exception.CreateFmt('Event Pool quiere Threads de Mensajes del tipo "%s", pero se ha intentado registrar un thread de mensajes del tipo "%s"', [GetMensajeThreadType.ClassName, AThreadMensajes.ClassName]);
  AdquireWrite;
  try
    if (not FThreadsMensajes.Contains(AThreadMensajes)) then
      FThreadsMensajes.Add(AThreadMensajes);
  finally
    ReleaseWrite;
  end;
end;

procedure TMessageChannel.AfterConstruction;
begin
  inherited;
  CreateThreads;
  Register;
end;

constructor TMessageChannel.Create(const AThreadCount: Integer);
begin
  inherited Create;
  FThreadCount       := AThreadCount;
  FThreadCountTarget := AThreadCount;
  FThreadsMensajes   := TCollections.CreateList<TThreadMessageHandler>;
  FSynchronizer      := TMREWSync.Create;
end;

procedure TMessageChannel.CreateThreads;
var
  I: Integer;
begin
  for I := 1 to FThreadCount do
    GetMensajeThreadType.Create(Self);
end;

destructor TMessageChannel.Destroy;
begin
  DestroyThreads;
  FThreadsMensajes := nil;
  FSynchronizer   := nil;
  inherited;
end;

procedure TMessageChannel.DestroyThreads;
var
  I, LCount: Integer;
begin
  AdquireWrite;
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
    ReleaseWrite;
  end;
end;

function TMessageChannel.GetThreadCount: Integer;
begin
  AdquireRead;
  try
    Result := FThreadCount;
  finally
    ReleaseRead;
  end;
end;

function Comparador_TThreadMessageHandler(const DatoI, DatoD: TThreadMessageHandler): Integer;
var
  LThisThreadEventCountI, LThisThreadEventCountD: Int64;
begin
  Result := 0;
  if (DatoI = DatoD) then
    Exit(0);
  LThisThreadEventCountI := DatoI.NoEventosTratados;
  LThisThreadEventCountD := DatoD.NoEventosTratados;
  if LThisThreadEventCountI < LThisThreadEventCountD then
    Exit(-1);
  if LThisThreadEventCountI > LThisThreadEventCountD then
    Exit(1);
end;

procedure TMessageChannel.PoolMessage(AMessage: IMessage);
var
  LFinal                                    : IEnumerable<TThreadMessageHandler>;
  LData                                     : IList<TThreadMessageHandler>;
  LSeleccion                                : TThreadMessageHandler;
begin
  AdquireRead;
  try
    LData := TCollections.CreateList<TThreadMessageHandler>;
    LData.AddRange(FThreadsMensajes.ToArray);
    LData.Sort(Comparador_TThreadMessageHandler);
    LFinal := LData.Where(function (const AData: TThreadMessageHandler): Boolean
                                   begin
                                     Result := AData.GetEventRelevant(AMessage);
                                   end);
    if LFinal.Count <> 0 then
    begin
      LSeleccion := LFinal.First;
      LSeleccion.AddMessage(AMessage);
    end
    else begin
           Utils.IdeDebugMsg('No thread selected! ' + QualifiedClassName);
         end;
  finally
    ReleaseRead;
  end;
end;

procedure TMessageChannel.ProcessMessage(AMessage: IMessage);
begin
  if FThreadsMensajes.Count > 0 then
    PoolMessage(AMessage);
end;

procedure TMessageChannel.Register;
begin
  Messages.RegisterPool(Self);
end;

procedure TMessageChannel.RegisterListener(AMessageListener: IMessageListener);
var
  I: Integer;
begin
  AdquireRead;
  try
    for I := 0 to FThreadsMensajes.Count - 1 do
    begin
      FThreadsMensajes[I].RegisterListener(AMessageListener);
    end;
  finally
    ReleaseRead;
  end;
end;

procedure TMessageChannel.ReleaseRead;
begin
  FSynchronizer.EndRead
end;

procedure TMessageChannel.ReleaseWrite;
begin
  FSynchronizer.EndWrite
end;

procedure TMessageChannel.RemoveThreadMensajes(const AThreadMensajes: TThreadMessageHandler);
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

procedure TMessageChannel.SetThreadCount(const AThreadCount: Integer);
begin
  AdquireWrite;
  try
    FThreadCountTarget := AThreadCount;
  finally
    ReleaseWrite;
  end;
end;

procedure TMessageChannel.UnRegister;
begin
  Messages.UnregisterPool(Self);
end;

procedure TMessageChannel.UnregisterListener(AMessageListener: IMessageListener);
var
  I: Integer;
begin
  AdquireRead;
  try
    for I := 0 to FThreadsMensajes.Count - 1 do
    begin
      FThreadsMensajes[I].UnregisterListener(AMessageListener);
    end;
  finally
    ReleaseRead;
  end;
end;

{$ENDREGION}

{$REGION 'TMessageChannel<T>'}
function TMessageChannel<T>.GetMensajeThreadType: TThreadMessageHandlerType;
begin
  Result := T;
end;
{$ENDREGION}

{$REGION 'TMessageListenerModel<T, K>'}
constructor TMessageListenerModel<T, K>.Create(AModel: IModel; const APoolThreadsMensajes: TMessageChannel; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  FModel := AModel;
  Create(APoolThreadsMensajes, AFilterCondition, ACodeExecutesInMainUIThread, ATypeRestriction);
end;

constructor TMessageListenerModel<T, K>.Create(AModel: IModel; const AThreadMensajes: TThreadMessageHandler; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  FModel := AModel;
  Create(AThreadMensajes, AFilterCondition, ACodeExecutesInMainUIThread, ATypeRestriction);
end;
{$ENDREGION}

{$REGION 'TMessageListenerViewModel<T, K>'}
constructor TMessageListenerViewModel<T, K>.Create(AViewModel: IViewModel; const APoolThreadsMensajes: TMessageChannel; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  FViewModel := AViewModel;
  Create(APoolThreadsMensajes, AFilterCondition, ACodeExecutesInMainUIThread, ATypeRestriction);
end;

constructor TMessageListenerViewModel<T, K>.Create(AViewModel: IViewModel; const AThreadMensajes: TThreadMessageHandler; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  FViewModel := AViewModel;
  Create(AThreadMensajes, AFilterCondition, ACodeExecutesInMainUIThread, ATypeRestriction);
end;
{$ENDREGION}

initialization

MVVMCore.Container.RegisterType<TMessageChannel_GENERAL>(
  function: TMessageChannel_GENERAL
  begin
    Result := TMessageChannel_GENERAL.Create(Utils.iif<Integer>((TThread.ProcessorCount > 2), 2, TThread.ProcessorCount));
  end).AsSingleton;

MVVMCore.Container.RegisterType<TMessageChannel_GENERAL_OneThread>(
  function: TMessageChannel_GENERAL_OneThread
  begin
    Result := TMessageChannel_GENERAL_OneThread.Create(1)
  end).AsSingleton;

Messages.CreateIni;

finalization

Messages.DestroyIni;

end.
