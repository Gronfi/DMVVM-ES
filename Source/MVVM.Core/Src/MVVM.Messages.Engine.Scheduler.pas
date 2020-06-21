unit MVVM.Messages.Engine.Scheduler;

interface

uses
  System.Classes,
  System.SyncObjs,
  System.Generics.Defaults,

  Spring.Collections,

  MVVM.Patched.ThreadedQueue,
  MVVM.Messages.Engine.Interfaces;

type

  TMessagesScheduler = class sealed(TThread)
  const
    CTE_INITIAL_QUEUE_SIZE = 10;
    CTE_PUSH_TIMEOUT       = 100;
  private
    FIndex              : Int64;
    FSC                 : TCriticalSection;
    FTaskQueue          : TThreadedQueue<ISchedulerTask>;
    FTaskList           : IList<ISchedulerTask>;
    FComparer           : IComparer<ISchedulerTask>;

    FTaskListToRemove: IList<ISchedulerTask>;

    procedure Execute; override;

    function Checks: Boolean;
    procedure RecalcScheduling;
    function GetIndex: Int64;

    procedure DoScheduling;

    function GetNewTask(out AQueueSize: Integer; out ATask: ISchedulerTask): TWaitResult; overload;
    function GetNewTask(out AQueueSize: Integer; out ATask: ISchedulerTask; const ATimeOut: Cardinal): TWaitResult; overload;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ScheduleMessage(AMessage: IMessage; const AWhenPassedNextMilliseconds: Int64); overload;
    procedure ScheduleMessage(AMessage: IMessage; const AWhenPassedNextDateTime: TDateTime); overload;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,

  MVVM.Utils,
  MVVM.Core,
  MVVM.Interfaces;

type

  TSchedulerTask = class sealed(TInterfacedObject, ISchedulerTask)
  private
    FTaskID                     : Int64;
    FTimeStampCreation          : Int64;
    FTimeStampAwake             : Int64;
    FMilisecondsToAwake         : Int64;
    FIsDone                     : Boolean;
    FMessage                    : IMessage;

    function GetTaskID: Int64;
    function GetMilisecondsToAwake: Int64;

    procedure CalculateAwakeTime;
    procedure Notify;
  public
    constructor Create(const ATaskID: Int64; const AEllapsedMilisecondsToExecute: Int64; AMessage: IMessage); overload;
    destructor Destroy; override;

    function IsDone: Boolean;
    function CheckAndNotify: Boolean;

    property TaskID: Int64 read GetTaskID;
    property MilisecondsToAwake: Int64 read GetMilisecondsToAwake;
  end;

{ TMessagesScheduler }

function TMessagesScheduler.Checks: Boolean;
var
  LTask: ISchedulerTask;
begin
  Result := False;
  if FTaskList.Count <> 0 then
  begin
    FTaskListToRemove.Clear;
    for LTask in FTaskList do
    begin
      if LTask.CheckAndNotify then
      begin
        Result := True;
      end;
      if LTask.IsDone then
      begin
        Result := True;
        FTaskListToRemove.Add(LTask);
      end;
    end;
    for LTask in FTaskListToRemove do
    begin
      FTaskList.Remove(LTask);
    end;
    FTaskListToRemove.Clear;
    if (Result) then
    begin
      if FTaskList.Count = 0 then
      begin
        Result := False;
      end
    end;
  end;
end;

constructor TMessagesScheduler.Create;
begin
  inherited Create(False);
  FSC                       := TCriticalSection.Create;
  FIndex                    := 0;
  FTaskQueue                := TThreadedQueue<ISchedulerTask>.Create(CTE_INITIAL_QUEUE_SIZE, CTE_PUSH_TIMEOUT, INFINITE);
  FTaskList                 := TCollections.CreateList<ISchedulerTask>;
  FTaskListToRemove         := TCollections.CreateList<ISchedulerTask>;
  FComparer                 := TComparerSchedulerTask.Create;
end;

destructor TMessagesScheduler.Destroy;
begin
  Terminate;
  FTaskQueue.DoShutDown;
  WaitFor;
  FComparer              := nil;
  FTaskList              := nil;
  FTaskListToRemove      := nil;
  FTaskQueue.Destroy;
  FSC.Destroy;
  inherited;
end;

procedure TMessagesScheduler.Execute;
begin
  DoScheduling;
end;

function TMessagesScheduler.GetIndex: Int64;
begin
  FSC.Enter;
  try
    Inc(FIndex);
    Result := FIndex;
  finally
    FSC.Leave;
  end;
end;

function TMessagesScheduler.GetNewTask(out AQueueSize: Integer; out ATask: ISchedulerTask): TWaitResult;
begin
  Result := FTaskQueue.PopItem(AQueueSize, ATask);
end;

function TMessagesScheduler.GetNewTask(out AQueueSize: Integer; out ATask: ISchedulerTask; const ATimeOut: Cardinal): TWaitResult;
begin
  Result := FTaskQueue.PopItem(AQueueSize, ATask, ATimeOut);
end;

procedure TMessagesScheduler.DoScheduling;
var
  LTarea               : ISchedulerTask;
  LNoTarea             : Int64;
  LTimeToSleep         : Int64;
  LWaitingTask         : ISchedulerTask;
  LRes                 : TWaitResult;
  LSize                : Integer;
  LRecalcChecks        : Boolean;
begin
  LRecalcChecks := False;
  while not(Terminated) do
  begin
    repeat
      case FTaskList.Count of
        0:
          begin
            LRes := GetNewTask(LSize, LTarea);
          end
      else
        begin
          LWaitingTask  := FTaskList[0];
          LTimeToSleep  := LWaitingTask.MiliSecondsToAwake;
          LWaitingTask  := nil;
          if LTimeToSleep <= 0 then
          begin
            if Checks then
            begin
              RecalcScheduling;
            end;
            Continue;
          end
          else begin
                 LRecalcChecks := True;
                 LRes := GetNewTask(LSize, LTarea, LTimeToSleep);
               end;
        end;
      end;
      case LRes of
        wrSignaled:
          begin
            if (not Terminated) then
            begin
              FTaskList.Add(LTarea);
              RecalcScheduling;
              LRecalcChecks := True;
              LTarea        := nil;
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
    if LRecalcChecks then
    begin
      if Checks then
      begin
        RecalcScheduling;
      end;
      LRecalcChecks := False;
    end;
  end;
end;

procedure TMessagesScheduler.RecalcScheduling;
var
  LComparison: TComparison<ISchedulerTask>;
begin
  if (FTaskList.Count > 1) then
  begin
    FTaskList.Sort(FComparer);
  end;
end;

procedure TMessagesScheduler.ScheduleMessage(AMessage: IMessage; const AWhenPassedNextDateTime: TDateTime);
var
  LPassedMiliseconds: Int64;
begin
  LPassedMiliseconds := MilliSecondsBetween(Now, AWhenPassedNextDateTime);
  if (LPassedMiliseconds <= 0) then
    AMessage.Post
  else ScheduleMessage(AMessage, LPassedMiliseconds);
end;

procedure TMessagesScheduler.ScheduleMessage(AMessage: IMessage; const AWhenPassedNextMilliseconds: Int64);
var
  LTask: ISchedulerTask;
  LSize: Integer;
  LRes : TWaitResult;
begin
  LTask  := TSchedulerTask.Create(GetIndex, AWhenPassedNextMilliseconds, AMessage);
  repeat
    LRes := FTaskQueue.PushItem(LTask, LSize);
    case LRes of
      wrTimeout:
        begin
          FTaskQueue.Grow(LSize);
          if Terminated then Exit;
        end;
    end;
  until LRes = TWaitResult.wrSignaled;
end;

{ TSchedulerTask }

procedure TSchedulerTask.CalculateAwakeTime;
begin
  FTimeStampAwake := FTimeStampCreation + FMilisecondsToAwake;
end;

function TSchedulerTask.CheckAndNotify: Boolean;
var
  LElapsed: Int64;
begin
  Result := False;
  if not(FIsDone) then
  begin
    LElapsed := MVVMCore.PlatformServices.ElapsedMiliseconds;
    FIsDone := (LElapsed >= FTimeStampAwake);
    Result  := FIsDone;
    if FIsDone then
      Notify;
  end;
end;

constructor TSchedulerTask.Create(const ATaskID: Int64; const AEllapsedMilisecondsToExecute: Int64; AMessage: IMessage);
begin
  inherited Create;
  FTaskID             := ATaskID;
  FTimeStampCreation   := MVVMCore.PlatformServices.ElapsedMiliseconds;
  FMilisecondsToAwake := AEllapsedMilisecondsToExecute;
  FMessage            := AMessage;
  FIsDone             := False;
  CalculateAwakeTime;
end;

destructor TSchedulerTask.Destroy;
begin
  FMessage := nil;
  inherited;
end;

function TSchedulerTask.GetTaskID: Int64;
begin
  Result := FTaskID;
end;

function TSchedulerTask.GetMilisecondsToAwake: Int64;
begin
  FMilisecondsToAwake := FTimeStampAwake - MVVMCore.PlatformServices.ElapsedMiliseconds;
  Result              := FMilisecondsToAwake;
end;

function TSchedulerTask.IsDone: Boolean;
begin
  Result := FIsDone
end;

procedure TSchedulerTask.Notify;
begin
  FMessage.Post;
end;

end.
