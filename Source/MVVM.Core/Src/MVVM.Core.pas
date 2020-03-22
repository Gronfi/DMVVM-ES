unit MVVM.Core;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Container,

  MVVM.Interfaces,
  MVVM.Types;

type
  MVVMCore = record
  private
  class var
    FPlatformServicesClass: TPlatformServicesClass;
    FContainer: TContainer;
    FDefaultBindingStrategyName: String;
    FSynchronizer: IReadWriteSync;
    class constructor CreateC;
    class destructor DestroyC;

    class function GetDefaultBindingStrategyName: String; static;
    class procedure SetDefaultBindingStrategyName(const AStrategyName: String); static;
  public
    class procedure RegisterPlatformServices(AServicioClass
      : TPlatformServicesClass); static;
    class function PlatformServices: IPlatformServices; static;
    class function Container: TContainer; static;

    class procedure InitializationDone; static;

    class procedure DelegateExecution<T>(AData: T; AProc: TProc<T>;
      AExecutionMode: EDelegatedExecutionMode); overload; static;

    class property DefaultBindingStrategyName: String read GetDefaultBindingStrategyName
      write SetDefaultBindingStrategyName;
  end;

implementation

uses
  System.Classes,
  System.Threading;

{ MVVM }

class function MVVMCore.Container: TContainer;
begin
  Result := FContainer;
end;

class constructor MVVMCore.CreateC;
begin
  FContainer := TContainer.Create;
  FSynchronizer := TMREWSync.Create;
end;

class function MVVMCore.GetDefaultBindingStrategyName: String;
begin
  FSynchronizer.BeginRead;
  try
    Result := FDefaultBindingStrategyName;
  finally
    FSynchronizer.EndRead;
  end;
end;

class procedure MVVMCore.DelegateExecution<T>(AData: T; AProc: TProc<T>;
  AExecutionMode: EDelegatedExecutionMode);
begin
  case AExecutionMode of
    medQueue:
      begin
        TThread.Queue(TThread.CurrentThread,
          procedure
          begin
            AProc(AData);
          end);
      end;
    medSynchronize:
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            AProc(AData)
          end);
      end;
    medNewTask:
      begin
        TTask.Create(
          procedure
          begin
            AProc(AData)
          end).Start;
      end;
    medNormal:
      begin
        AProc(AData);
      end;
  end;
end;

class destructor MVVMCore.DestroyC;
begin
  FContainer.Free;
end;

class procedure MVVMCore.InitializationDone;
begin
  FContainer.Build;
end;

class procedure MVVMCore.RegisterPlatformServices(AServicioClass
  : TPlatformServicesClass);
begin
  FPlatformServicesClass := AServicioClass;
end;

class procedure MVVMCore.SetDefaultBindingStrategyName(const AStrategyName: String);
begin
  FSynchronizer.BeginWrite;
  try
    FDefaultBindingStrategyName := AStrategyName;
  finally
    FSynchronizer.EndWrite;
  end;
end;

class function MVVMCore.PlatformServices: IPlatformServices;
begin
  Spring.Guard.CheckNotNull(FPlatformServicesClass,
    'No hay registrado ningun servicio para la plataforma');
  Result := FPlatformServicesClass.Create;
end;

end.
