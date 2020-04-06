unit MVVM.Core;

interface

uses
  System.SysUtils,

  Spring,
  Spring.Container,

  MVVM.Bindings,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.ViewFactory,
  MVVM.Types;

type
  MVVMCore = record
  private
  class var
    FPlatformServicesClass: TPlatformServicesClass;
    FContainer: TContainer;
    FDefaultBindingStrategyName: String;
    FDefaultViewPlatform: string;
    FSynchronizer: IReadWriteSync;
    class constructor CreateC;
    class destructor DestroyC;

    class function GetDefaultBindingStrategyName: String; static;
    class procedure SetDefaultBindingStrategyName(const AStrategyName: String); static;
    class function GetDefaultViewPlatform: String; static;
    class procedure SetDefaultViewPlatform(const APlatform: String); static;

    class procedure AutoRegister; static;
  public
    class procedure RegisterPlatformServices(AServicioClass: TPlatformServicesClass); static;
    class function PlatformServices: IPlatformServices; static;
    class function Container: TContainer; static;

    class function EnableBinding(AObject: TObject): Boolean; static;
    class function DisableBinding(AObject: TObject): Boolean; static;

    class procedure InitializationDone; static;

    class function ViewsProvider: TViewFactoryClass; static;

    class procedure DelegateExecution(AProc: TProc; AExecutionMode: EDelegatedExecutionMode); overload; static;
    class procedure DelegateExecution<T>(AData: T; AProc: TProc<T>; AExecutionMode: EDelegatedExecutionMode); overload; static;

    class function DefaultBindingStrategy: IBindingStrategy; static;

    class property DefaultBindingStrategyName: String read GetDefaultBindingStrategyName write SetDefaultBindingStrategyName;
    class property DefaultViewPlatform: String read GetDefaultViewPlatform write SetDefaultViewPlatform;
  end;

implementation

uses
  System.Classes,
  System.Threading,
  System.RTTI,

  MVVM.Attributes,
  MVVM.Utils;

{ MVVM }

class procedure MVVMCore.AutoRegister;
var
  Ctx  : TRttiContext;
  Typ  : TRttiType;
  TypI : TRttiInterfaceType ;
  LAttr: TCustomAttribute;
  LVMA : View_For_ViewModel;
  LVMI : ViewModel_Implements;
  LInstanceType: TRttiInstanceType;
  LInterfaces: TArray<TRttiInterfaceType>;
  I : Integer;
begin
  Ctx := TRttiContext.Create;
  try
    for Typ in Ctx.GetTypes do
    begin
      // Loop for attributes
      for LAttr in Typ.GetAttributes do
      begin
        //Utils.IdeDebugMsg('Atributo: ' + LAttr.QualifiedClassName);
        case Utils.AttributeToCaseSelect(LAttr, [View_For_ViewModel, ViewModel_Implements]) of
          0: // View_For_ViewModel
            begin
              LVMA := LAttr as View_For_ViewModel;
              if not Typ.IsInstance then Continue;
              LInstanceType := Typ.AsInstance;
              TViewFactory.Register(LInstanceType, LVMA.ViewAlias, LVMA.Platform);
            end;
          1: //ViewModel_Implements
            begin
              LVMI := LAttr as ViewModel_Implements;
              if Typ.IsInstance then
              begin
                LInstanceType := Typ.AsInstance;
                LInterfaces   := LInstanceType.GetImplementedInterfaces;
                if Length(LInterfaces) > 0 then
                begin
                  for I := Low(LInterfaces) to High(LInterfaces) do
                  begin
                    if LInterfaces[I].GUID = LVMI.VMInterface then
                    begin
                      case LVMI.InstanceType of
                        EInstanceType.itSingleton:
                          begin
                            MVVMCore.Container.RegisterType(LInstanceType.Handle).Implements(LInterfaces[I].Handle).AsSingleton;
                          end;
                        EInstanceType.itNewInstance:
                          begin
                            MVVMCore.Container.RegisterType(LInstanceType.Handle).Implements(LInterfaces[I].Handle);
                          end;
                      end;
                      Break;
                    end;
                  end;
                end;
              end;
            end;
        end;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

class function MVVMCore.Container: TContainer;
begin
  Result := FContainer;
end;

class constructor MVVMCore.CreateC;
begin
  FContainer    := TContainer.Create;
  FSynchronizer := TMREWSync.Create;
  FDefaultBindingStrategyName := '';
  FDefaultViewPlatform        := '';
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

class function MVVMCore.GetDefaultViewPlatform: String;
begin
  FSynchronizer.BeginRead;
  try
    Result := FDefaultViewPlatform;
  finally
    FSynchronizer.EndRead;
  end;
end;

class function MVVMCore.DefaultBindingStrategy: IBindingStrategy;
begin
  Result := TBindingManager.GetDefaultRegisteredBindingStrategy
end;

class procedure MVVMCore.DelegateExecution(AProc: TProc; AExecutionMode: EDelegatedExecutionMode);
begin
  case AExecutionMode of
    medQueue:
      begin
        TThread.Queue(TThread.CurrentThread,
          procedure
          begin
            AProc;
          end);
      end;
    medSynchronize:
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            AProc
          end);
      end;
    medNewTask:
      begin
        TTask.Create(
          procedure
          begin
            AProc
          end).Start;
      end;
    medNormal:
      begin
        AProc;
      end;
  end;
end;

class procedure MVVMCore.DelegateExecution<T>(AData: T; AProc: TProc<T>; AExecutionMode: EDelegatedExecutionMode);
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

class function MVVMCore.DisableBinding(AObject: TObject): Boolean;
var
  [weak] LBinding: IBindable;
begin
  if Supports(AObject, IBindable, LBinding) then
    LBinding.Binding.Enabled := False;
end;

class function MVVMCore.EnableBinding(AObject: TObject): Boolean;
var
  [weak] LBinding: IBindable;
begin
  if Supports(AObject, IBindable, LBinding) then
    LBinding.Binding.Enabled := True;
end;

class procedure MVVMCore.InitializationDone;
begin
  AutoRegister;
  FContainer.Build;
end;

class procedure MVVMCore.RegisterPlatformServices(AServicioClass: TPlatformServicesClass);
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

class procedure MVVMCore.SetDefaultViewPlatform(const APlatform: String);
begin
  FSynchronizer.BeginWrite;
  try
    FDefaultViewPlatform := APlatform;
  finally
    FSynchronizer.EndWrite;
  end;
end;

class function MVVMCore.PlatformServices: IPlatformServices;
begin
  Spring.Guard.CheckNotNull(FPlatformServicesClass, 'No hay registrado ningun servicio para la plataforma');
  Result := FPlatformServicesClass.Create;
end;

class function MVVMCore.ViewsProvider: TViewFactoryClass;
begin
  Result := TViewFactory;
end;

end.
