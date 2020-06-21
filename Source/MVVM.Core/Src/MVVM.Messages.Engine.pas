unit MVVM.Messages.Engine;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,

  Spring,
  Spring.Collections,

  MVVM.Patched.ThreadedQueue,
  MVVM.Messages.Engine.Scheduler,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.Types,

  MVVM.Messages.Engine.Interfaces;

const
  MAX_DEFAULT_POOLED_THREADS = 4;

type
  { Forward Declarations }
  TMessage                  = class;
  TThreadMessageHandlerBase = class;
  TThreadMessageHandler     = class;
  TMessageChannel           = class;
  TThreadMessageHandlerType = class of TThreadMessageHandler;

{$REGION 'TMessage'}

  TMessage = class abstract(TInterfacedObject, IMessage)
  private
    FCreationDateTime: TDateTime;
    FSender          : TObject;
  protected
    function GetCreationDateTime: TDateTime;
    function GetSender: TObject;
  public
    constructor Create; reintroduce; overload;
    constructor Create(ASender: TObject); overload;
    destructor Destroy; override;

    procedure Post; virtual;
    procedure Schedule(const AMilisecondsToExecute: Int64); overload; virtual;
    procedure Schedule(const ADateTimeWhenExecute: TDateTime); overload; virtual;

    function GetAsObject: TObject;

    property CreationDateTime: TDateTime read GetCreationDateTime;
    property Sender: TObject read GetSender;
  end;
{$ENDREGION}
{$REGION 'TMessageListener'}

  TMessageListener = class abstract(TInterfacedObject, IMessageListener, IObject)
  private
    FRegistered                   : Boolean;
    FIsCodeToExecuteInUIMainThread: Boolean;
    FChannel                      : TMessageChannel;
    FTypeRestriction              : EMessageTypeRestriction;
    FFilterCondition              : TListenerFilter;
    FEnabled                      : Boolean;

    function GetIsCodeToExecuteInUIMainThread: Boolean;
    procedure SetIsCodeToExecuteInUIMainThread(const AValue: Boolean);

    function GetTypeRestriction: EMessageTypeRestriction;
    procedure SetTypeRestriction(const ATypeRestriction: EMessageTypeRestriction);

    function GetListenerFilter: TListenerFilter;
    procedure SetListenerFilter(const AFilter: TListenerFilter);

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);

  protected
    function GetDefaultTypeRestriction: EMessageTypeRestriction; virtual;
    function GetDefaultEnabled: Boolean; virtual;
  public
    procedure AfterConstruction; override;

    constructor Create(const AChannel: TMessageChannel = nil; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); reintroduce;
      overload; virtual;

    destructor Destroy; override;

    function GetConditionsMatch(AMessage: IMessage): Boolean; virtual;

    function GetMessajeClass: TClass; virtual; abstract;

    procedure Register;
    procedure UnRegister;

    function GetAsObject: TObject;

    procedure DoOnNewMessage(AMessage: IMessage); virtual;

    property IsCodeToExecuteInUIMainThread: Boolean read GetIsCodeToExecuteInUIMainThread write SetIsCodeToExecuteInUIMainThread;
    property FilterCondition: TListenerFilter read GetListenerFilter write SetListenerFilter;
    property TypeRestriction: EMessageTypeRestriction read GetTypeRestriction write SetTypeRestriction;
    property Enabled        : Boolean read GetEnabled write SetEnabled;
  end;

  TMessageListener<T: IMessage> = class abstract(TMessageListener, IMessageListener<T>)
  private
    FOnMessage: IEvent<TNotifyMessage>;
  protected
    function GetOnMessage: IEvent<TNotifyMessage>;
    procedure DoOnNewMessage(AMessage: IMessage); override; final;
  public
    constructor Create(const AChannel: TMessageChannel = nil; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False; const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload; override;
    destructor Destroy; override;

    function GetMessajeClass: TClass; override; final;

    property OnMessage: IEvent<TNotifyMessage> read GetOnMessage;
  end;

{$ENDREGION}
{$REGION 'TThreadMessageHandlerBase'}

  TThreadMessageHandlerBase = class abstract(TThread)
  const
    CTE_INITIAL_QUEUE_SIZE = 10;
    CTE_PUSH_TIMEOUT       = 100;
  private
    FSynchronizer: IReadWriteSync;
    FMessageCount: Int64;
    FMessages    : TThreadedQueue<IMessage>;

    procedure AdquireWrite;
    procedure ReleaseWrite;
    procedure AdquireRead;
    procedure ReleaseRead;

    procedure ProcessQueuedMessage(AMessage: IMessage);
    procedure ProcessMessages;

    function GetNextMessage(out AQueueSize: Integer; var AMessage: IMessage): TWaitResult;
  protected
    procedure Execute; override;

    function GetProcessedMessageCount: Int64;

    procedure ProcessMessage(AMessage: IMessage); virtual; abstract;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;

    procedure AddMessage(AMessage: IMessage); virtual;

    property ProcessedMessageCount: Int64 read GetProcessedMessageCount;
  end;

{$ENDREGION}
{$REGION 'TThreadMessageHandler'}

  TThreadMessageHandler = class(TThreadMessageHandlerBase)
  private
    FListeners            : IList<IMessageListener>;
    FSynchronizerListeners: IReadWriteSync;
    FChannel              : TMessageChannel;
  protected
    procedure ProcessMessage(AMessage: IMessage); override;

    procedure InitializeListeners; virtual;
    procedure FinalizeListeners; virtual;

    function GetListenersCount: Integer;
    function GetEventRelevant(AMessage: IMessage): Boolean; virtual;
  public
    procedure AfterConstruction; override;

    constructor Create; overload; override;
    constructor Create(const AChannel: TMessageChannel); overload;
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
    FSynchronizer     : IReadWriteSync;
    FThreadsMessajes  : IList<TThreadMessageHandler>;
    FExecutors        : IList<TThreadMessageHandler>;
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
    function GetMessajeThreadType: TThreadMessageHandlerType; virtual; abstract;

    procedure ProcessMessage(AMessage: IMessage); override;
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
    function GetMessajeThreadType: TThreadMessageHandlerType; override; final;
  end;
{$ENDREGION}

  TMessageChannel_Main                = class(TMessageChannel<TThreadMessageHandler>);
  TMessageChannel_Main_SingleThreaded = class(TMessageChannel<TThreadMessageHandler>);

{$REGION 'MessageBus'}
  EMessageDeploymentKind = (mdkFifo, mdkPooled);

  MessageBus = record
  private
    class var FScheduler             : TMessagesScheduler;
    class var FSynchronizerChannels  : IReadWriteSync;
    class var FChannels              : IList<TMessageChannel>;
    class var FMessageDeploymentKind : EMessageDeploymentKind;

    class procedure CreateIni; static;
    class procedure DestroyIni; static;

    class procedure QueueInchannels(AMessage: IMessage); static;
  public
    class procedure RegisterChannel(const AChannel: TMessageChannel); static;
    class procedure UnregisterChannel(const AChannel: TMessageChannel); static;

    class procedure QueueMessage(AMessage: IMessage); static;

    class property MessageDeploymentKind : EMessageDeploymentKind read FMessageDeploymentKind write FMessageDeploymentKind;
    class property Scheduler: TMessagesScheduler read FScheduler;
  end;
{$ENDREGION}
{$REGION 'TMessage_Generic'}

  TMessage_Generic<T> = class(TMessage)
  public
    Data: T;
  end;

{$ENDREGION}
{$REGION 'TMessage_Base_ViewModel'}

  TMessageListenerViewModel<T: TMessage> = class(TMessageListener<T>)
  private
    FViewModel: IViewModel;
  protected
    function GetConditionsMatch(AMessage: IMessage): Boolean; override;
  public
    constructor Create(AViewModel: IViewModel; const AChannel: TMessageChannel = nil; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False;
      const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload;
  end;

{$ENDREGION}

implementation

uses
  System.Generics.Defaults,

  MVVM.Core,
  MVVM.Utils;

{$REGION 'TMessage'}

constructor TMessage.Create;
begin
  inherited Create;
  FCreationDateTime := Now;
  FSender           := nil;
end;

constructor TMessage.Create(ASender: TObject);
begin
  Create;
  FSender := ASender;
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

function TMessage.GetSender: TObject;
begin
  Result := FSender;
end;

procedure TMessage.Post;
begin
  MessageBus.QueueMessage(Self)
end;

procedure TMessage.Schedule(const ADateTimeWhenExecute: TDateTime);
begin
  MessageBus.Scheduler.ScheduleMessage(Self, ADateTimeWhenExecute);
end;

procedure TMessage.Schedule(const AMilisecondsToExecute: Int64);
begin
  MessageBus.Scheduler.ScheduleMessage(Self, AMilisecondsToExecute);
end;

{$ENDREGION}
{$REGION 'TMessageListener'}

procedure TMessageListener.AfterConstruction;
begin
  inherited;
  Register;
end;

constructor TMessageListener.Create(const AChannel: TMessageChannel; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  if AChannel = nil then
  begin
    case MessageBus.FMessageDeploymentKind of
      EMessageDeploymentKind.mdkFifo:
        begin
          FChannel := MVVMCore.IoC.Resolve<TMessageChannel_Main_SingleThreaded>;
        end;
      EMessageDeploymentKind.mdkPooled:
        begin
          FChannel := MVVMCore.IoC.Resolve<TMessageChannel_Main>;
        end;
    end;
  end
  else
    FChannel := AChannel;
  inherited Create;
  FEnabled                       := GetDefaultEnabled;
  FIsCodeToExecuteInUIMainThread := ACodeExecutesInMainUIThread;
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
  else
    Result := True
end;

function TMessageListener.GetDefaultEnabled: Boolean;
begin
  Result := True;
end;

function TMessageListener.GetDefaultTypeRestriction: EMessageTypeRestriction;
begin
  Result := mtrAllowDescendants;
end;

function TMessageListener.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMessageListener.GetListenerFilter: TListenerFilter;
begin
  Result := FFilterCondition
end;

function TMessageListener.GetTypeRestriction: EMessageTypeRestriction;
begin
  Result := FTypeRestriction;
end;

procedure TMessageListener.DoOnNewMessage(AMessage: IMessage);
begin
  //
end;

procedure TMessageListener.Register;
begin
  FChannel.RegisterListener(Self);
end;

procedure TMessageListener.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
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
  FChannel.UnregisterListener(Self);
end;
{$ENDREGION}
{$REGION 'TMessageListener<T>'}

constructor TMessageListener<T>.Create(const AChannel: TMessageChannel; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  inherited;
  FOnMessage := Utils.CreateEvent<TNotifyMessage>;
end;

destructor TMessageListener<T>.Destroy;
begin
  FOnMessage := nil;
  inherited;
end;

function TMessageListener<T>.GetMessajeClass: TClass;
begin
  Result := PTypeInfo(TypeInfo(T))^.TypeData.ClassType;
end;

function TMessageListener<T>.GetOnMessage: IEvent<TNotifyMessage>;
begin
  Result := FOnMessage
end;

procedure TMessageListener<T>.DoOnNewMessage(AMessage: IMessage);
begin
  if FIsCodeToExecuteInUIMainThread then
  begin
    if not MVVMCore.PlatformServices.IsMainThreadUI then
      MVVMCore.DelegateExecution<IMessage>(AMessage,
        procedure(AAMessage: IMessage)
        begin
          FOnMessage.Invoke(AAMessage)
        end, EDelegatedExecutionMode.medQueue)
    else
      FOnMessage.Invoke(AMessage);
  end
  else
    FOnMessage.Invoke(AMessage);
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
  FMessages.DoShutDown;
  WaitFor;
  FMessages.Free;
  FSynchronizer := nil;
  inherited Destroy;
end;

function TThreadMessageHandlerBase.GetProcessedMessageCount: Int64;
begin
  Result := FMessageCount
end;

function TThreadMessageHandlerBase.GetNextMessage(out AQueueSize: Integer; var AMessage: IMessage): TWaitResult;
begin
  Result := FMessages.PopItem(AQueueSize, AMessage);
end;

procedure TThreadMessageHandlerBase.ProcessMessages;
var
  LRes : TWaitResult;
  LSize: Integer;
  LMsg : IMessage;
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
                on E: Exception do
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
begin
  ProcessMessage(AMessage);
end;

procedure TThreadMessageHandlerBase.AddMessage(AMessage: IMessage);
var
  LSize: Integer;
  LRes : TWaitResult;
begin
  repeat
    LRes := FMessages.PushItem(AMessage, LSize);
    case LRes of
      wrTimeout:
        begin
          FMessages.Grow(LSize);
          if Terminated then
            Exit;
        end;
    end;
  until LRes = TWaitResult.wrSignaled;
  Inc(FMessageCount);
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
  FSynchronizerListeners := TMREWSync.Create;
  FChannel               := nil;
  FListeners             := TCollections.CreateList<IMessageListener>;
  InitializeListeners;
end;

constructor TThreadMessageHandler.Create(const AChannel: TMessageChannel);
begin
  Create;
  FChannel := AChannel;
end;

destructor TThreadMessageHandler.Destroy;
begin
  UnRegister;
  FinalizeListeners;
  FListeners := nil;

  FSynchronizerListeners := nil;
  inherited Destroy;
end;

procedure TThreadMessageHandler.FinalizeListeners;
var
  LListener: IMessageListener;
  LList    : IList<IMessageListener>;
begin
  LList := TCollections.CreateList<IMessageListener>;
  FSynchronizerListeners.BeginRead;
  try
    LList.AddRange(FListeners.ToArray);
  finally
    FSynchronizerListeners.EndRead;
  end;
  for LListener in LList do
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
  //
end;

procedure TThreadMessageHandler.ProcessMessage(AMessage: IMessage);
var
  I: Integer;
begin
  FSynchronizerListeners.BeginRead;
  try
    for I := 0 to FListeners.Count - 1 do
    begin
      if (FListeners[I].Enabled) and (((FListeners[I].TypeRestriction = mtrAllowDescendants) and (AMessage is FListeners[I].GetMessajeClass)) or ((FListeners[I].GetTypeRestriction = mtrDefinedTypeOnly) and (AMessage.GetAsObject.ClassType = FListeners[I].GetMessajeClass))) and
        (FListeners[I].GetConditionsMatch(AMessage)) then
      begin
        try
          FListeners[I].DoOnNewMessage(AMessage);
        except
          on E: Exception do
          begin
            Utils.IdeDebugMsg('Exception executing the listener: ' + FListeners[I].GetAsObject.QualifiedClassName + ' - Error: ' + E.Message);
            Utils.IdeDebugMsg('Exception message class type: ' + AMessage.GetAsObject.QualifiedClassName);
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
  FChannel.AddThreadMensajes(Self)
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
  FChannel.RemoveThreadMensajes(Self)
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

class procedure MessageBus.CreateIni;
begin
  FChannels             := TCollections.CreateList<TMessageChannel>;
  FSynchronizerChannels := TMREWSync.Create;
  FMessageDeploymentKind:= EMessageDeploymentKind.mdkPooled;
  FScheduler            := TMessagesScheduler.Create;
end;

class procedure MessageBus.DestroyIni;
begin
  FScheduler.Destroy;
  FSynchronizerChannels := nil;
  FChannels             := nil;
end;

class procedure MessageBus.QueueMessage(AMessage: IMessage);
begin
  Guard.CheckNotNull(AMessage, 'The message can not be nil');
  QueueInchannels(AMessage);
end;

class procedure MessageBus.QueueInchannels(AMessage: IMessage);
var
  I: Integer;
begin
  FSynchronizerChannels.BeginRead;
  try
    for I := 0 to FChannels.Count - 1 do
    begin
      FChannels[I].AddMessage(AMessage);
    end;
  finally
    FSynchronizerChannels.EndRead;
  end;
end;

class procedure MessageBus.RegisterChannel(const AChannel: TMessageChannel);
begin
  FSynchronizerChannels.BeginWrite;
  try
    if (not FChannels.Contains(AChannel)) then
      FChannels.Add(AChannel);
  finally
    FSynchronizerChannels.EndWrite
  end;
end;

class procedure MessageBus.UnregisterChannel(const AChannel: TMessageChannel);
var
  LIndex: Integer;
begin
  FSynchronizerChannels.BeginWrite;
  try
    LIndex := FChannels.IndexOf(AChannel);
    if LIndex > -1 then
      FChannels.Delete(LIndex);
  finally
    FSynchronizerChannels.EndWrite;
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
  if not(AThreadMensajes is GetMessajeThreadType) then
    raise Exception.CreateFmt('Event Pool quiere Threads de Mensajes del tipo "%s", pero se ha intentado registrar un thread de mensajes del tipo "%s"', [GetMessajeThreadType.ClassName, AThreadMensajes.ClassName]);
  AdquireWrite;
  try
    if (not FThreadsMessajes.Contains(AThreadMensajes)) then
      FThreadsMessajes.Add(AThreadMensajes);
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
  FThreadsMessajes   := TCollections.CreateList<TThreadMessageHandler>;
  FExecutors         := TCollections.CreateList<TThreadMessageHandler>;
  FSynchronizer      := TMREWSync.Create;
end;

procedure TMessageChannel.CreateThreads;
var
  I: Integer;
begin
  for I := 1 to FThreadCount do
    GetMessajeThreadType.Create(Self);
end;

destructor TMessageChannel.Destroy;
begin
  DestroyThreads;
  FThreadsMessajes := nil;
  FExecutors       := nil;
  FSynchronizer    := nil;
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
      LCount := FThreadsMessajes.Count;
      for I  := LCount - 1 downto 0 do
        FThreadsMessajes[I].Free;
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
  LThisThreadEventCountI := DatoI.ProcessedMessageCount;
  LThisThreadEventCountD := DatoD.ProcessedMessageCount;
  if LThisThreadEventCountI < LThisThreadEventCountD then
    Exit(-1);
  if LThisThreadEventCountI > LThisThreadEventCountD then
    Exit(1);
end;

procedure TMessageChannel.PoolMessage(AMessage: IMessage);
var
  LSelected: TThreadMessageHandler;
begin
  AdquireRead;
  try
    FExecutors.AddRange(FThreadsMessajes.ToArray);
    FExecutors.Sort(Comparador_TThreadMessageHandler);
    if FExecutors.Count <> 0 then
    begin
      LSelected := FExecutors.First;
      LSelected.AddMessage(AMessage);
      FExecutors.Clear;
    end;
  finally
    ReleaseRead;
  end;
end;

procedure TMessageChannel.ProcessMessage(AMessage: IMessage);
begin
  if FThreadsMessajes.Count > 0 then
    PoolMessage(AMessage);
end;

procedure TMessageChannel.Register;
begin
  MessageBus.RegisterChannel(Self);
end;

procedure TMessageChannel.RegisterListener(AMessageListener: IMessageListener);
var
  I: Integer;
begin
  AdquireRead;
  try
    for I := 0 to FThreadsMessajes.Count - 1 do
    begin
      FThreadsMessajes[I].RegisterListener(AMessageListener);
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
    LIndex := FThreadsMessajes.IndexOf(AThreadMensajes);
    if LIndex > -1 then
      FThreadsMessajes.Delete(LIndex);
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
  MessageBus.UnregisterChannel(Self);
end;

procedure TMessageChannel.UnregisterListener(AMessageListener: IMessageListener);
var
  I: Integer;
begin
  AdquireRead;
  try
    for I := 0 to FThreadsMessajes.Count - 1 do
    begin
      FThreadsMessajes[I].UnregisterListener(AMessageListener);
    end;
  finally
    ReleaseRead;
  end;
end;
{$ENDREGION}
{$REGION 'TMessageChannel<T>'}

function TMessageChannel<T>.GetMessajeThreadType: TThreadMessageHandlerType;
begin
  Result := T;
end;
{$ENDREGION}
{$REGION 'TMessageListenerViewModel<T>'}

constructor TMessageListenerViewModel<T>.Create(AViewModel: IViewModel; const AChannel: TMessageChannel; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  FViewModel := AViewModel;
  Create(AChannel, AFilterCondition, ACodeExecutesInMainUIThread, ATypeRestriction);
end;
{$ENDREGION}

function TMessageListenerViewModel<T>.GetConditionsMatch(AMessage: IMessage): Boolean;
begin
  if (AMessage.Sender <> FViewModel.GetAsObject) then
    Exit(False);
  Result := inherited GetConditionsMatch(AMessage)
end;

initialization

MVVMCore.IoC.RegisterType<TMessageChannel_Main>(
  function: TMessageChannel_Main
  begin
    Result := TMessageChannel_Main.Create(Utils.iif<Integer>((TThread.ProcessorCount > MAX_DEFAULT_POOLED_THREADS), MAX_DEFAULT_POOLED_THREADS, TThread.ProcessorCount));
  end).AsSingleton;

MVVMCore.IoC.RegisterType<TMessageChannel_Main_SingleThreaded>(
  function: TMessageChannel_Main_SingleThreaded
  begin
    Result := TMessageChannel_Main_SingleThreaded.Create(1)
  end).AsSingleton;

MessageBus.CreateIni;

finalization

MessageBus.DestroyIni;

end.
