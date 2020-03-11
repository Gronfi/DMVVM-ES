unit MVVM.Bindings;

interface

uses
  System.Classes,
  Generics.Collections,
  //System.Bindings.Expression, System.Bindings.Helper,
  System.RTTI,

  Spring,
  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types;

type
  TBindingManager = class(TInterfacedObject, IDataBinder)
  private
    class var
      FDiccionarioEstrategiasBinding: IDictionary<String, TClass_BindingStrategyBase>;
  private
    var
      FObject                            : TObject;
      FDiccionarioEstrategias            : IDictionary<String, IBindingStrategy>;
      //FEstrategiaPorDefecto              : String;
  protected
    class constructor CreateC;
    class destructor DestroyC;

    function ChequeoIntegridadSeleccionBinding(const ABindingStrategy: String): IBindingStrategy;
//    function GetEstrategiaPorDefecto: String; virtual;
//    procedure SetEstrategiaPorDefecto(const AValue: String); virtual;
  public
    constructor Create(AObject: TObject); virtual;
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
    procedure BindCollection(const ACollection: TEnumerable<TObject>;
                             const ATarget: ICollectionViewProvider;
                             const ATemplate: TDataTemplateClass;
                             const ABindingStrategy: String = '');
    procedure BindAction(const AAction: IBindableAction;
                         const AExecute: TExecuteMethod;
                         const ACanExecute: TCanExecuteMethod = nil;
                         const ABindingStrategy: String = ''); overload; inline;

    procedure Notify(const AObject: TObject; const APropertyName: String); overload; virtual;
    procedure Notify(const AObject: TObject; const APropertiesNames: TArray<String>); overload; virtual;

    //property EstrategiaPorDefecto: String read GetEstrategiaPorDefecto write SetEstrategiaPorDefecto;
    class procedure RegistrarBindingStrategy(const AEstrategia: String; ABindingStrategyClass: TClass_BindingStrategyBase);
  end;

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

procedure TBindingManager.BindCollection(const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(ABindingStrategy);
  LEstrategia.BindCollection(ACollection, ATarget, ATemplate);
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
  inherited Create;
  FObject                            := AObject;
  //FDiccionarioNotificacionEstrategias:= TCollections.CreateDictionary<String, IList<String>>;
  FDiccionarioEstrategias            := TCollections.CreateDictionary<String, IBindingStrategy>;
  //FEstrategiaPorDefecto              := '';
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

class procedure TBindingManager.RegistrarBindingStrategy(const AEstrategia: String; ABindingStrategyClass: TClass_BindingStrategyBase);
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
