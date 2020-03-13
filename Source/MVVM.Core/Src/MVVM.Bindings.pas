unit MVVM.Bindings;

interface

uses
  System.Classes,
  Generics.Collections,
  System.RTTI,
  Data.DB,

  Spring,
  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types,

  MVVM.Messages.Engine;

type
{$REGION 'TBindingManager'}
  TBindingManager = class
  private
    class var
      FDiccionarioEstrategiasBinding: IDictionary<String, TClass_BindingStrategyBase>;
  private
    var
      FObject                            : TObject;
      FDiccionarioEstrategias            : IDictionary<String, IBindingStrategy>;
  protected
    class constructor CreateC;
    class destructor DestroyC;

    function ChequeoIntegridadSeleccionBinding(const ABindingStrategy: String): IBindingStrategy;
  public
    constructor Create(AObject: TObject); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
                   const ATarget: TObject; const ATargetPropertyPath: String;
                   const ADirection: EBindDirection = EBindDirection.OneWay;
                   const AFlags: EBindFlags = [];
                   const AValueConverterClass: TBindingValueConverterClass = nil;
                   const ABindingStrategy: String = '';
                   const AExtraParams: TBindExtraParams = []); overload;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String;
               const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String;
               const AFlags: EBindFlags = [];
               const ABindingStrategy: String = '';
               const AExtraParams: TBindExtraParams = []); overload;
    procedure BindCollection<T: Class>(const ACollection: TEnumerable<TObject>;
                             const ATarget: ICollectionViewProvider;
                             const ATemplate: TDataTemplateClass;
                             const ABindingStrategy: String = '');
    procedure BindDataSet(const ADataSet: TDataSet;
                             const ATarget: ICollectionViewProvider;
                             const ATemplate: TDataTemplateClass;
                             const ABindingStrategy: String = '');
    procedure BindAction(const AAction: IBindableAction;
                         const AExecute: TExecuteMethod;
                         const ACanExecute: TCanExecuteMethod = nil;
                         const ABindingStrategy: String = ''); overload; inline;

    procedure Notify(const AObject: TObject; const APropertyName: String); overload; virtual;
    procedure Notify(const AObject: TObject; const APropertiesNames: TArray<String>); overload; virtual;

    class procedure RegisterBindingStrategy(const AEstrategia: String; ABindingStrategyClass: TClass_BindingStrategyBase);
  end;
{$ENDREGION}

  TMessage_Object_Destroyed = class(TMessage)

  end;

  TMessageChannel_OBJECT_DESTROYED = class(TMessageChannel<TThreadMessageHandler>);

implementation

uses
  System.SysUtils,

  MVVM.Core;

{ TBindingManager }

procedure TBindingManager.Bind(const ASource: TObject; const ASourcePropertyPath: String;
                                 const ATarget: TObject; const ATargetPropertyPath: String;
                                 const ADirection: EBindDirection;
                                 const AFlags: EBindFlags;
                                 const AValueConverterClass: TBindingValueConverterClass;
                                 const ABindingStrategy: String;
                                 const AExtraParams: TBindExtraParams);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(ABindingStrategy);
  LEstrategia.Bind(ASource,
                   ASourcePropertyPath, ATarget,
                   ATargetPropertyPath, ADirection,
                   AFlags,
                   AValueConverterClass,
                   AExtraParams);
end;

procedure TBindingManager.Bind(const ASources: TSourcePairArray; const ASourceExpresion: String;
                                 const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String;
                                 const AFlags: EBindFlags;
                                 const ABindingStrategy: String;
                                 const AExtraParams: TBindExtraParams);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(ABindingStrategy);
  LEstrategia.Bind(ASources, ASourceExpresion,
                   ATarget, ATargetAlias, ATargetPropertyPath,
                   AFlags,
                   AExtraParams);
end;

procedure TBindingManager.BindAction(const AAction: IBindableAction; const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(ABindingStrategy);
  LEstrategia.BindAction(AAction, AExecute, ACanExecute);
end;

procedure TBindingManager.BindCollection<T>(const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(ABindingStrategy);
  LEstrategia.BindCollection(TypeInfo(T), ACollection, ATarget, ATemplate);
end;

procedure TBindingManager.BindDataSet(const ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(ABindingStrategy);
  LEstrategia.BindDataSet(ADataSet, ATarget, ATemplate);
end;

function TBindingManager.ChequeoIntegridadSeleccionBinding(const ABindingStrategy: String): IBindingStrategy;
var
  LMetodo    : String;
begin
  if ABindingStrategy.IsEmpty then
    LMetodo := MVVMCore.DefaultBindingStrategy
  else LMetodo := ABindingStrategy;
  // Integridad
  Guard.CheckTrue(FDiccionarioEstrategiasBinding.ContainsKey(LMetodo), 'Estrategia de binding no registrada: ' + LMetodo);
  if not FDiccionarioEstrategias.TryGetValue(LMetodo, Result) then
  begin
    Result := FDiccionarioEstrategiasBinding[LMetodo].Create;
    FDiccionarioEstrategias.AddOrSetValue(LMetodo, Result);
  end;
end;

constructor TBindingManager.Create(AObject: TObject);
begin
  Create;
  FObject                            := AObject;
end;

constructor TBindingManager.Create;
begin
  inherited Create;
  FDiccionarioEstrategias := TCollections.CreateDictionary<String, IBindingStrategy>;
end;

class constructor TBindingManager.CreateC;
begin
  FDiccionarioEstrategiasBinding := TCollections.CreateDictionary<String, TClass_BindingStrategyBase>;
end;

destructor TBindingManager.Destroy;
begin
  //FDiccionarioNotificacionEstrategias := nil;
  inherited;
end;

class destructor TBindingManager.DestroyC;
begin
  FDiccionarioEstrategiasBinding := nil;
end;

(*
function TBindingManager.GetEstrategiaPorDefecto: String;
begin
  if not FEstrategiaPorDefecto.IsEmpty then
    Exit(FEstrategiaPorDefecto);
  {$IFDEF MSWINDOWS}
    Result := CBasicStrategies[EBasicStrategies._LIVEBINDINGS];
  {$ENDIF}
  {$IFDEF LINUX}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._RTTI];
  {$ENDIF}
  {$IFDEF ANDROID}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._RTTI];
  {$ENDIF}
  {$IFDEF POSIX}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._RTTI];
  {$ENDIF}
  {$IFDEF MACOS}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._RTTI];
  {$ENDIF}
end;
*)

procedure TBindingManager.Notify(const AObject: TObject; const APropertiesNames: TArray<String>);
var
  LEstrategia: String;
begin
  for LEstrategia in FDiccionarioEstrategias.Keys do
    FDiccionarioEstrategias[LEstrategia].Notify(AObject, APropertiesNames);
end;

procedure TBindingManager.Notify(const AObject: TObject; const APropertyName: String);
var
  LEstrategia: String;
begin
  for LEstrategia in FDiccionarioEstrategias.Keys do
    FDiccionarioEstrategias[LEstrategia].Notify(AObject, APropertyName);
end;

class procedure TBindingManager.RegisterBindingStrategy(const AEstrategia: String; ABindingStrategyClass: TClass_BindingStrategyBase);
begin
  FDiccionarioEstrategiasBinding.AddOrSetValue(AEstrategia, ABindingStrategyClass);
end;

(*
procedure TBindingManager.SetEstrategiaPorDefecto(const AValue: String);
begin
  if FEstrategiaPorDefecto <> AValue then
  begin
    FEstrategiaPorDefecto := AValue;
  end;
end;
*)

end.
