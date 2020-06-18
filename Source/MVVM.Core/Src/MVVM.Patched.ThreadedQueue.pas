unit MVVM.Patched.ThreadedQueue;

interface

uses
  System.Types,
  System.Generics.Collections;

type

  TThreadedQueue<T> = class
  private
    FQueue: array of T;
    FQueueNotEmpty,
    FQueueNotFull: TObject;
    FQueueLock: TObject;
    FTotalItemsPushed, FTotalItemsPopped: UInt64;
    FQueueSize, FQueueOffset: Integer;
    FPushTimeout, FPopTimeout: Cardinal;
    FShutDown: Boolean;
{$IF Defined(CPU32BITS)}
    function GetTotalItemsPopped: UInt64;
    function GetTotalItemsPushed: UInt64;
{$ENDIF CPU32BITS}
  public
    constructor Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
    destructor Destroy; override;

    procedure Grow(ADelta: Integer);
    function PushItem(const AItem: T): TWaitResult; overload;
    function PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult; overload;
    function PushItem(const AItem: T; var AQueueSize: Integer; const ATimeOut: Cardinal): TWaitResult; overload;
    function PopItem: T; overload;
    function PopItem(var AQueueSize: Integer): T; overload;
    function PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult; overload;
    function PopItem(var AQueueSize: Integer; var AItem: T; const ATimeOut: Cardinal): TWaitResult; overload;
    function PopItem(var AItem: T): TWaitResult; overload;
    procedure DoShutDown;

    property QueueSize: Integer read FQueueSize;
    property ShutDown: Boolean read FShutDown;
{$IF Defined(CPU32BITS)}
    property TotalItemsPushed: UInt64 read GetTotalItemsPushed;
    property TotalItemsPopped: UInt64 read GetTotalItemsPopped;
{$ELSE}
    property TotalItemsPushed: UInt64 read FTotalItemsPushed;
    property TotalItemsPopped: UInt64 read FTotalItemsPopped;
{$ENDIF CPU32BITS}
  end;

implementation

{ TThreadedQueue<T> }

function TThreadedQueue<T>.PopItem(var AQueueSize: Integer; var AItem: T; const ATimeOut: Cardinal): TWaitResult;
begin
  AItem := Default(T);
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = 0) and not FShutDown do
      if not TMonitor.Wait(FQueueNotEmpty, FQueueLock, ATimeOut) then
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

{ TThreadedQueue<T> }

constructor TThreadedQueue<T>.Create(AQueueDepth: Integer = 10; PushTimeout: Cardinal = INFINITE; PopTimeout: Cardinal = INFINITE);
begin
  inherited Create;
  SetLength(FQueue, AQueueDepth);
  FQueueLock := TObject.Create;
  FQueueNotEmpty := TObject.Create;
  FQueueNotFull := TObject.Create;
  FPushTimeout := PushTimeout;
  FPopTimeout := PopTimeout;
end;

destructor TThreadedQueue<T>.Destroy;
begin
  DoShutDown;
  FQueueNotFull.Free;
  FQueueNotEmpty.Free;
  FQueueLock.Free;
  inherited;
end;

{$IF Defined(CPU32BITS)}
function TThreadedQueue<T>.GetTotalItemsPopped: UInt64;
begin
  TMonitor.Enter(FQueueLock);
  try
    Result := FTotalItemsPopped;
  finally
    TMonitor.Exit(FQueueLock);
  end;
end;

function TThreadedQueue<T>.GetTotalItemsPushed: UInt64;
begin
  TMonitor.Enter(FQueueLock);
  try
    Result := FTotalItemsPushed;
  finally
    TMonitor.Exit(FQueueLock);
  end;
end;
{$ENDIF CPU32BITS}

procedure TThreadedQueue<T>.Grow(ADelta: Integer);
var
  Ind, PartialLength, OldLength, NewLength: Integer;
begin
  TMonitor.Enter(FQueueLock);
  try
    OldLength := Length(FQueue);
    NewLength := OldLength + ADelta;
    if ADelta < 0 then
    begin
      if FQueueSize > NewLength then
        ErrorArgumentOutOfRange
      else if FQueueOffset <> 0 then
      begin
        if (NewLength <= FQueueOffset) then
        begin
          for Ind := FQueueSize - 1 downto 0 do
          begin
            FQueue[Ind] := FQueue[(FQueueOffset + Ind) mod OldLength];
            FQueue[(FQueueOffset + Ind) mod OldLength] := Default(T);
          end;
          FQueueOffset := 0;
        end
        else if (NewLength <= FQueueOffset + FQueueSize - 1) then
        begin
          for Ind := 0 to FQueueSize - 1 do
          begin
            FQueue[Ind] := FQueue[(FQueueOffset + Ind) mod OldLength];
            FQueue[(FQueueOffset + Ind) mod OldLength] := Default(T);
          end;
          FQueueOffset := 0;
        end;
      end;
      SetLength(FQueue, NewLength);
    end
    else if ADelta > 0 then
    begin
      SetLength(FQueue, NewLength);
      PartialLength := OldLength - FQueueOffset;
      if FQueueSize > PartialLength then
      begin
        for Ind := OldLength - 1 downto FQueueOffset do
        begin
          FQueue[Ind + ADelta] := FQueue[Ind];
          FQueue[Ind] := Default(T);
        end;
        FQueueOffset := NewLength - PartialLength;
      end
    end;
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

function TThreadedQueue<T>.PushItem(const AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PushItem(AItem, LQueueSize);
end;

function TThreadedQueue<T>.PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult;
begin
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = Length(FQueue)) and not FShutDown do
      if not TMonitor.Wait(FQueueNotFull, FQueueLock, FPushTimeout) then
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

function TThreadedQueue<T>.PushItem(const AItem: T; var AQueueSize: Integer; const ATimeOut: Cardinal): TWaitResult;
begin
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = Length(FQueue)) and not FShutDown do
      if not TMonitor.Wait(FQueueNotFull, FQueueLock, ATimeOut) then
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

end.
