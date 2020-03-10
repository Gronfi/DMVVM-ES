unit MVVM.Bindings;

interface

uses
  System.Classes,
  Generics.Collections,
  System.Bindings.Expression, System.Bindings.Helper,
  System.RTTI,

  Spring,
  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types;

type
  TBindingStrategyBase = class abstract(TInterfacedObject, IBindingStrategy)
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Start; virtual;

    procedure Notify(const AObject: TObject; const APropertyName: String = ''); overload; virtual; abstract;
    procedure Notify(const AObject: TObject; const APropertiesNames: TArray<String>); overload; virtual;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
      const ATarget: TObject; const ATargetPropertyPath: String;
      const ADirection: EBindDirection = EBindDirection.OneWay;
      const AFlags: EBindFlags = [];
      const AValueConverterClass: TBindingValueConverterClass = nil;
      const AExtraParams: TBindExtraParams = []); overload; virtual; abstract;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String;
                   const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String;
                   const AFlags: EBindFlags = [];
                   const AExtraParams: TBindExtraParams = []); overload; virtual; abstract;
    procedure BindCollection<T: class>(const ACollection: TEnumerable<T>;
                                       const ATarget: ICollectionViewProvider;
                                       const ATemplate: TDataTemplateClass); virtual; abstract;
    procedure BindAction(const AAction: IBindableAction;
                     const AExecute: TExecuteMethod;
                     const ACanExecute: TCanExecuteMethod = nil); overload; virtual; abstract;

    procedure ClearBindings; virtual; abstract;
  end;

  TClass_BindingStrategyBase = class of TBindingStrategyBase;

  TEstrategia_LiveBindings = class(TBindingStrategyBase)
  protected
    type
      TExpressionList = TObjectList<TBindingExpression>;
  private
    FBindings: TExpressionList;
  protected
    property Bindings: TExpressionList read FBindings;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Notify(const AObject: TObject; const APropertyName: string = ''); overload; override;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
      const ATarget: TObject; const ATargetPropertyPath: String;
      const ADirection: EBindDirection = EBindDirection.OneWay;
      const AFlags: EBindFlags = [];
      const AValueConverterClass: TBindingValueConverterClass = nil;
      const AExtraParams: TBindExtraParams = []); overload; override;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String;
                   const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String;
                   const AFlags: EBindFlags = [];
                   const AExtraParams: TBindExtraParams = []); overload; override;
    procedure BindCollection<T: class>(const ACollection: TEnumerable<T>;
                                       const ATarget: ICollectionViewProvider;
                                       const ATemplate: TDataTemplateClass); override;
    procedure BindAction(const AAction: IBindableAction;
                     const AExecute: TExecuteMethod;
                     const ACanExecute: TCanExecuteMethod = nil); overload; override;
    procedure ClearBindings; override;
  end;

type

  TBindingHelper_V2 = class(TInterfacedObject, IDataBinder)
  private
    class var
      FDiccionarioEstrategiasBinding: IDictionary<String, TClass_BindingStrategyBase>;
  private
    var
      FObject                            : TObject;
      FDiccionarioEstrategias            : IDictionary<String, IBindingStrategy>;
      FEstrategiaPorDefecto              : String;
  protected
    class constructor CreateC;
    class destructor DestroyC;

    function ChequeoIntegridadSeleccionBinding(const ABindingStrategy: String): IBindingStrategy;
    function GetEstrategiaPorDefecto: String; virtual;
    procedure SetEstrategiaPorDefecto(const AValue: String); virtual;
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
    procedure BindCollection<T: class>(const ACollection: TEnumerable<T>;
                                       const ATarget: ICollectionViewProvider;
                                       const ATemplate: TDataTemplateClass;
                                       const ABindingStrategy: String = '');
    procedure BindAction(const AAction: IBindableAction;
                         const AExecute: TExecuteMethod;
                         const ACanExecute: TCanExecuteMethod = nil;
                         const ABindingStrategy: String = ''); overload; inline;

    procedure Notify(const AObject: TObject; const APropertyName: String); overload; virtual;
    procedure Notify(const AObject: TObject; const APropertiesNames: TArray<String>); overload; virtual;

    (*
    procedure BindCollection<T: class>(const ACollection: IEnumerable<T>;
      const ATarget: IgoCollectionViewProvider;
      const ATemplate: TgoDataTemplateClass;
      const ABindingStrategy: String = '');

    procedure BindAction(const AAction: IgoBindableAction;
      const AExecute: TgoExecuteMethod;
      const ACanExecute: TgoCanExecuteMethod = nil;
      const ABindingStrategy: String = ''); overload; inline;
    procedure BindAction(const AAction: IgoBindableAction;
      const AExecute: TgoExecuteMethod<Integer>;
      const ACanExecute: TgoCanExecuteMethod = nil;
      const ABindingStrategy: String = ''); overload; inline;
    *)
    property EstrategiaPorDefecto: String read GetEstrategiaPorDefecto write SetEstrategiaPorDefecto;
    class procedure RegistrarBindingStrategy(const AEstrategia: String; ABindingStrategyClass: TClass_BindingStrategyBase);
  end;

  (*
  TBindingHelper = class
  protected
    type
      TExpressionList = TObjectList<TBindingExpression>;
  private
    FObject: TObject;
    FBindings: TExpressionList;
  protected
    property Bindings: TExpressionList read FBindings;
  public
    constructor Create(AObject: TObject); virtual;
    destructor Destroy; override;

    procedure Notify(const APropertyName: string = '');

    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;

    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;

    class function CreateEvent<T>: IEvent<T>; static;

    procedure ClearBindings;
  end;
  *)

implementation

uses
  System.Bindings.EvalProtocol,
  System.SysUtils;

(*
constructor TBindingHelper.Create(AObject: TObject);
begin
  inherited Create;
  FObject   := AObject;
  FBindings := TExpressionList.Create(false {AOwnsObjects});
end;

class function TBindingHelper.CreateEvent<T>: IEvent<T>;
var
  E: Event<T>;
begin
  Result := E;
end;

destructor TBindingHelper.Destroy;
begin
  ClearBindings;
  FBindings.Free;
  inherited;
end;

procedure TBindingHelper.ClearBindings;
var
  i: TBindingExpression;
begin
  for i in FBindings do
    TBindings.RemoveBinding(i);
  FBindings.Clear;
end;

procedure TBindingHelper.Notify(const APropertyName: string);
begin
  TBindings.Notify(FObject, APropertyName);
end;

procedure TBindingHelper.Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string);
begin
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(FObject, 'src')])],
      'src.' + AProperty,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(ABindToObject, 'dst')])],
      'dst.' + ABindToProperty,
      nil, nil, [coNotifyOutput, coEvaluate]));
end;

procedure TBindingHelper.Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string);
begin
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(FObject, ASrcAlias)])],
      ASrcFormatedExpression,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(ABindToObject, ADstAlias)])],
      ADstFormatedExpression,
      nil, nil, [coNotifyOutput, coEvaluate]));
end;

procedure TBindingHelper.BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression: string);
begin
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(ABindObject, ASrcAlias)])],
      ASrcFormatedExpression,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(FObject, ADstAlias)])],
      ADstFormatedExpression,
      nil, nil, [coNotifyOutput, coEvaluate]));
end;

procedure TBindingHelper.BindReverse(const ABindObject: TObject; const AProperty, ABindToProperty: string);
begin
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(ABindObject, 'src')])],
      'src.' + AProperty,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(FObject, 'dst')])],
      'dst.' + ABindToProperty,
      nil, nil, [coNotifyOutput, coEvaluate]));
end;
*)

{ TBindingHelper_V2 }

procedure TBindingHelper_V2.Bind(const ASource: TObject; const ASourcePropertyPath: String;
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

procedure TBindingHelper_V2.Bind(const ASources: TSourcePairArray; const ASourceExpresion: String;
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

procedure TBindingHelper_V2.BindAction(const AAction: IBindableAction; const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(ABindingStrategy);
  LEstrategia.BindAction(AAction, AExecute, ACanExecute);
end;

procedure TBindingHelper_V2.BindCollection<T>(const ACollection: TEnumerable<T>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(ABindingStrategy);
  LEstrategia.BindCollection<T>(ACollection, ATarget, ATemplate);
end;

function TBindingHelper_V2.ChequeoIntegridadSeleccionBinding(const ABindingStrategy: String): IBindingStrategy;
var
  LMetodo    : String;
begin
  if ABindingStrategy.IsEmpty then
    LMetodo := GetEstrategiaPorDefecto
  else LMetodo := ABindingStrategy;
  // Integridad
  Guard.CheckTrue(FDiccionarioEstrategiasBinding.ContainsKey(LMetodo), 'Estrategia de binding no registrada: ' + LMetodo);
  if not FDiccionarioEstrategias.TryGetValue(LMetodo, Result) then
  begin
    Result := FDiccionarioEstrategiasBinding[LMetodo].Create;
    FDiccionarioEstrategias.AddOrSetValue(LMetodo, Result);
  end;
end;

constructor TBindingHelper_V2.Create(AObject: TObject);
begin
  inherited Create;
  FObject                            := AObject;
  //FDiccionarioNotificacionEstrategias:= TCollections.CreateDictionary<String, IList<String>>;
  FDiccionarioEstrategias            := TCollections.CreateDictionary<String, IBindingStrategy>;
  FEstrategiaPorDefecto              := '';
end;

class constructor TBindingHelper_V2.CreateC;
begin
  FDiccionarioEstrategiasBinding := TCollections.CreateDictionary<String, TClass_BindingStrategyBase>;
end;

destructor TBindingHelper_V2.Destroy;
begin
  //FDiccionarioNotificacionEstrategias := nil;
  inherited;
end;

class destructor TBindingHelper_V2.DestroyC;
begin
  FDiccionarioEstrategiasBinding := nil;
end;

function TBindingHelper_V2.GetEstrategiaPorDefecto: String;
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

procedure TBindingHelper_V2.Notify(const AObject: TObject; const APropertiesNames: TArray<String>);
var
  LEstrategia: String;
begin
  for LEstrategia in FDiccionarioEstrategias.Keys do
    FDiccionarioEstrategias[LEstrategia].Notify(AObject, APropertiesNames);
end;

procedure TBindingHelper_V2.Notify(const AObject: TObject; const APropertyName: String);
var
  LEstrategia: String;
begin
  for LEstrategia in FDiccionarioEstrategias.Keys do
    FDiccionarioEstrategias[LEstrategia].Notify(AObject, APropertyName);
end;

class procedure TBindingHelper_V2.RegistrarBindingStrategy(const AEstrategia: String; ABindingStrategyClass: TClass_BindingStrategyBase);
begin
  FDiccionarioEstrategiasBinding.AddOrSetValue(AEstrategia, ABindingStrategyClass);
end;

procedure TBindingHelper_V2.SetEstrategiaPorDefecto(const AValue: String);
begin
  if FEstrategiaPorDefecto <> AValue then
  begin
    FEstrategiaPorDefecto := AValue;
  end;
end;

{ TEstrategia_LiveBindings }

procedure TEstrategia_LiveBindings.Bind(const ASource: TObject; const ASourcePropertyPath: String;
                                        const ATarget: TObject; const ATargetPropertyPath: String;
                                        const ADirection: EBindDirection;
                                        const AFlags: EBindFlags;
                                        const AValueConverterClass: TBindingValueConverterClass;
                                        const AExtraParams: TBindExtraParams);
var
  LSrcProperty, LDstProperty: String;
  lAssocInput, lAssocOutput : IScope;
  lManaged                  : TBindingExpression;
  LOptions                  : TBindings.TCreateOptions;
  LNotifyPropertyChanged       : INotifyPropertyChanged;
  LNotifyPropertyChangeTracking: INotifyPropertyChangeTracking;
begin
  LOptions := [coNotifyOutput];
  if not(EBindFlag.DontApply in AFlags) then
    LOptions := LOptions + [coEvaluate];

  LAssocInput := TBindings.CreateAssociationScope([Associate(ASource,'Src')]);
  lAssocOutput:= TBindings.CreateAssociationScope([Associate(ATarget, 'Dst')]);
  if not(EBindFlag.UsesExpressions in AFlags) then
  begin
    LSrcProperty:= 'Src.'+ ASourcePropertyPath;
    LDstProperty:= 'Dst.'+ ATargetPropertyPath;
  end
  else begin
         LSrcProperty:= ASourcePropertyPath;
         LDstProperty:= ATargetPropertyPath;
       end;
  LManaged    := TBindings.CreateManagedBinding(
                                                [LAssocInput], LSrcProperty,
                                                [LAssocOutput], LDstProperty,
                                                nil,
                                                nil,
                                                LOptions);
  FBindings.Add(LManaged);

  // Settings especiales segun interfaces
  if Supports(ASource, INotifyPropertyChanged, LNotifyPropertyChanged) then
  begin
    LNotifyPropertyChanged.BindingStrategy := Self;
  end;
  if Supports(ASource, INotifyPropertyChangeTracking, LNotifyPropertyChangeTracking) then
  begin
    LNotifyPropertyChangeTracking.BindingStrategy := Self;
  end;

  if ADirection = EBindDirection.TwoWay then
  begin
    LAssocInput := TBindings.CreateAssociationScope([Associate(ATarget,'Src')]);
    lAssocOutput:= TBindings.CreateAssociationScope([Associate(ASource, 'Dst')]);
    if not(EBindFlag.UsesExpressions in AFlags) then
    begin
      LSrcProperty:= 'Src.'+ ATargetPropertyPath;
      LDstProperty:= 'Dst.'+ ASourcePropertyPath;
    end
    else begin
           LSrcProperty:= ATargetPropertyPath;
           LDstProperty:= ASourcePropertyPath;
         end;
    LManaged    := TBindings.CreateManagedBinding(
                                                  [LAssocInput], LSrcProperty,
                                                  [LAssocOutput], LDstProperty,
                                                  nil,
                                                  nil,
                                                  LOptions);
    FBindings.Add(LManaged);

    // Settings especiales segun interfaces
    if Supports(ATarget, INotifyPropertyChanged, LNotifyPropertyChanged) then
    begin
      LNotifyPropertyChanged.BindingStrategy := Self;
    end;
    if Supports(ATarget, INotifyPropertyChangeTracking, LNotifyPropertyChangeTracking) then
    begin
      LNotifyPropertyChangeTracking.BindingStrategy := Self;
    end;
  end;
end;

procedure TEstrategia_LiveBindings.Bind(const ASources: TSourcePairArray;
  const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String;
  const ATargetPropertyPath: String; const AFlags: EBindFlags;
  const AExtraParams: TBindExtraParams);
var
  LSrcProperty, LDstProperty: String;
  lAssocInput, lAssocOutput : IScope;
  lManaged                  : TBindingExpression;
  LOptions                  : TBindings.TCreateOptions;
  LArrayAsociacion          : array of TBindingAssociation;
  I, LCnt                   : Integer;
  LNotifyPropertyChanged       : INotifyPropertyChanged;
  LNotifyPropertyChangeTracking: INotifyPropertyChangeTracking;
begin
  LOptions := [coNotifyOutput];
  if not(DontApply in AFlags) then
    LOptions := LOptions + [coEvaluate];
  LCnt := Length(ASources);
  SetLength(LArrayAsociacion, LCnt);
  for I := 0 to LCnt - 1 do
  begin
    LArrayAsociacion[I] := Associate(ASources[I].Source,ASources[I].Alias);
    // Settings especiales segun interfaces
    if Supports(ASources[I].Source, INotifyPropertyChanged, LNotifyPropertyChanged) then
    begin
      LNotifyPropertyChanged.BindingStrategy := Self;
    end;
    if Supports(ASources[I].Source, INotifyPropertyChangeTracking, LNotifyPropertyChangeTracking) then
    begin
      LNotifyPropertyChangeTracking.BindingStrategy := Self;
    end;
  end;

  LAssocInput := TBindings.CreateAssociationScope(LArrayAsociacion);
  lAssocOutput:= TBindings.CreateAssociationScope([Associate(ATarget, ATargetAlias)]);
  LSrcProperty:= ASourceExpresion;
  LDstProperty:= ATargetPropertyPath;
  LManaged    := TBindings.CreateManagedBinding(
                                                [LAssocInput], LSrcProperty,
                                                [LAssocOutput], LDstProperty,
                                                nil,
                                                nil,
                                                LOptions);
  FBindings.Add(LManaged);
end;

procedure TEstrategia_LiveBindings.BindAction(const AAction: IBindableAction; const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod);
begin
  Guard.CheckNotNull(AAction, '<BindAction> (Param=AAction) no puede ser null');
  AAction.Bind(AExecute, ACanExecute);
end;

procedure TEstrategia_LiveBindings.BindCollection<T>(const ACollection: TEnumerable<T>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
begin
  // DAVID
end;

procedure TEstrategia_LiveBindings.ClearBindings;
var
  i: TBindingExpression;
begin
  for i in FBindings do
    TBindings.RemoveBinding(i);
  FBindings.Clear;
end;

constructor TEstrategia_LiveBindings.Create;
begin
  inherited;
  FBindings := TExpressionList.Create(false {AOwnsObjects});
end;

destructor TEstrategia_LiveBindings.Destroy;
begin
  ClearBindings;
  FBindings.Free;
  inherited;
end;

procedure TEstrategia_LiveBindings.Notify(const AObject: TObject; const APropertyName: string);
begin
  TBindings.Notify(AObject, APropertyName);
end;

{ TBindingStrategyBase }

constructor TBindingStrategyBase.Create;
begin
  inherited;
end;

destructor TBindingStrategyBase.Destroy;
begin
  inherited;
end;

procedure TBindingStrategyBase.Notify(const AObject: TObject; const APropertiesNames: TArray<String>);
var
  LValue: String;
begin
  for LValue in APropertiesNames do
    Notify(AObject, LValue);
end;

procedure TBindingStrategyBase.Start;
begin
  ;
end;

initialization

TBindingHelper_V2.RegistrarBindingStrategy(CBasicStrategies[EBasicStrategies._LIVEBINDINGS], TEstrategia_LiveBindings);

end.
