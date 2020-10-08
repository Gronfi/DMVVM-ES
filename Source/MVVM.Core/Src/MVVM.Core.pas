unit MVVM.Core;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,

  Spring,
  Spring.Container,

  MVVM.Bindings,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.ViewFactory,
  MVVM.Types;

type
  TContainerHelper = class helper for TContainer
  public
    function ViewModelProvider<T: IViewModel>: T;
    procedure RegisterViewModel(AClass, AInterface: PTypeInfo; const AIsSingleton: Boolean);
  end;

  MVVMCore = record
  private
  class var
    FPlatformServicesClass: TPlatformServicesClass;
    FContainer: TContainer;
    FDefaultBindingStrategyName: String;
    FDefaultViewPlatform: string;
    FSynchronizer: TLightweightMREW;

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

    class function IoC: TContainer; static;

    class function EnableBinding(AObject: TObject): Boolean; static;
    class function DisableBinding(AObject: TObject): Boolean; static;

    class procedure InitializationDone; static;

    class function ViewsProvider: TViewFactoryClass; static;
    //class procedure DetectCommandsAndPrepare(AView: TComponent); static;

    class procedure DelegateExecution(AProc: TProc; AExecutionMode: EDelegatedExecutionMode); overload; static;
    class procedure DelegateExecution<T>(AData: T; AProc: TProc<T>; AExecutionMode: EDelegatedExecutionMode); overload; static;

    class function DefaultBindingStrategy: IBindingStrategy; static;

    class property DefaultBindingStrategyName: String read GetDefaultBindingStrategyName write SetDefaultBindingStrategyName;
    class property DefaultViewPlatform: String read GetDefaultViewPlatform write SetDefaultViewPlatform;
  end;

implementation

uses
  System.Threading,
  System.RTTI,
  System.Actions,
  System.TypInfo,

  MVVM.CommandFactory,
  MVVM.Attributes,
  MVVM.Utils;

{ TContainerHelper }

procedure TContainerHelper.RegisterViewModel(AClass, AInterface: PTypeInfo; const AIsSingleton: Boolean);
begin
  case AIsSingleton of
    True:
      begin
        RegisterType(AClass).Implements(AInterface).AsSingleton;
      end;
    False:
      begin
        RegisterType(AClass).Implements(AInterface);
      end;
  end;
end;

function TContainerHelper.ViewModelProvider<T>: T;
begin
  Result := Resolve<T>;
end;

{ MVVM }

class procedure MVVMCore.AutoRegister;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  LMethod: TRttiMethod;
  LAttr: TCustomAttribute;
  LVMA: View_For_ViewModel;
  LVMI: ViewModel_Implements;
  LInstanceType: TRttiInstanceType;
  LInterfaces: TArray<TRttiInterfaceType>;
  I: Integer;
begin
  Ctx := TRttiContext.Create;
  try
    for Typ in Ctx.GetTypes do
    begin
      // Loop for class attributes
      for LAttr in Typ.GetAttributes do
      begin
        // Utils.IdeDebugMsg('Atributo: ' + LAttr.QualifiedClassName);
        case Utils.AttributeToCaseSelect(LAttr, [View_For_ViewModel, ViewModel_Implements]) of
          0: // View_For_ViewModel
            begin
              LVMA := LAttr as View_For_ViewModel;
              if not Typ.IsInstance then
                Continue;
              LInstanceType := Typ.AsInstance;
              TViewFactory.Register(LInstanceType, LVMA.ViewAlias, LVMA.Platform);
            end;
          1: // ViewModel_Implements
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
                            MVVMCore.IoC.RegisterViewModel(LInstanceType.Handle, LInterfaces[I].Handle, True);
                            //MVVMCore.IoC.RegisterType(LInstanceType.Handle).Implements(LInterfaces[I].Handle).AsSingleton;
                          end;
                        EInstanceType.itNewInstance:
                          begin
                            MVVMCore.IoC.RegisterViewModel(LInstanceType.Handle, LInterfaces[I].Handle, False);
                            //MVVMCore.IoC.RegisterType(LInstanceType.Handle).Implements(LInterfaces[I].Handle);
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

//class procedure MVVMCore.DetectCommandsAndPrepare(AView: TComponent);
//var
//  Ctx: TRttiContext;
//  Typ: TRttiType;
//  LAttr: TCustomAttribute;
//  LVMA: View_For_ViewModel;
//  LVMI: ViewModel_Implements;
//  LInstanceType: TRttiInstanceType;
//  LInterfaces: TArray<TRttiInterfaceType>;
//  I: Integer;
//  LField: TRttiField;
//  LCommand: Command;
//  LTypeData: PTypeData;
//  LExecute: RActionMember;
//  LCanExecute: RActionMember;
//  LParams: RActionMember;
//  LBindable: IBindableAction;
//begin
//  if not Supports(AView, IBindableAction, LBindable) then
//    Exit;
//  Ctx := TRttiContext.Create;
//  try
//    Typ           := ctx.GetType(AView.ClassType);
//    LInstanceType := ctx.GetType(AView.ClassType) as TRttiInstanceType;
//    // Loop the Fields
//    for LField in Typ.GetFields do
//    begin
//      if (LField.FieldType.TypeKind = tkClass) then
//      begin
//        LTypeData := GetTypeData(LField.FieldType.Handle);
//        if (not LTypeData^.ClassType.InheritsFrom(TContainedAction)) then //only an action is allowed
//          Continue;
//        // Loop for attributes
//        for LAttr in Typ.GetAttributes do
//        begin
//          case Utils.AttributeToCaseSelect(LAttr, [Command]) of
//            0:
//              begin
//                LCommand := LAttr as Command;
//                LExecute := TCommandsFactory.GetActionMember(LCommand.ExecuteName);
//                //LBindable.Bind(LExecute.Method.Invoke(AView, []) as TExecuteMethod);
//              end;
//          end;
//        end;
//      end;
//    end;
//  finally
//
//  end;
//end;

class function MVVMCore.IoC: TContainer;
begin
  Result := FContainer;
end;

class constructor MVVMCore.CreateC;
begin
  FContainer                  := TContainer.Create;
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
  [weak]
  LBinding: IBindable;
begin
  Result := False;
  if Supports(AObject, IBindable, LBinding) then
  begin
    LBinding.Binding.Enabled := False;
    Result                   := True;
  end;
end;

class function MVVMCore.EnableBinding(AObject: TObject): Boolean;
var
  [weak]
  LBinding: IBindable;
begin
  Result := False;
  if Supports(AObject, IBindable, LBinding) then
  begin
    LBinding.Binding.Enabled := True;
    Result                   := True;
  end;
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
