unit MVVM.Bindings.LiveBindings;

interface

uses
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  System.Bindings.Expression, System.Bindings.Helper, System.Bindings.EvalProtocol, System.Bindings.Outputs,
  System.RTTI,
  Data.DB,

  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types,
  MVVM.Bindings;

const
  STRATEGY_NAME = 'LIVEBINDINGS';

type
  TStrategy_LiveBindings = class(TBindingStrategyBase)
  private type
    TInternalBindindExpression = class(TComponent)
    private
      FSynchronizer    : IReadWriteSync;
      FExpression      : TBindingExpression;
      FTrackedInstances: ISet<Pointer>;
      FListener        : TMessageListener_TMessage_Object_Destroyed;

      procedure SetFreeNotification(const AInstance: TObject);
      procedure RemoveFreeNotification(const AInstance: TObject);
      procedure HandleFreeEvent(const ASender, AInstance: TObject);
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure OnObjectDestroyed(AMessage: IMessage);
    public
      constructor Create(const InputScopes: array of IScope;
                         const BindExprStr: string;
                         const OutputScopes: array of IScope;
                         const OutputExpr: string;
                         const OutputConverter: IValueRefConverter;
                         Manager: TBindingManager = nil;
                         Options: TBindings.TCreateOptions = [coNotifyOutput]); reintroduce;
      destructor Destroy; override;

      property Expression: TBindingExpression read FExpression;
    end;
  private
    class var
      FObjectListLinkers   : IDictionary<TClass, TProc<PTypeInfo, TComponent, TEnumerable<TObject>>>;
      FObjectDataSetLinkers: IDictionary<TClass, TProc<TDataSet, TComponent>>;
  protected
    type
      TExpressionList = TObjectList<TInternalBindindExpression>;
  private
    var
      FBindings: TExpressionList;

    class constructor CreateC;
    class destructor DestroyC;
  protected
    function InternalBindCollection(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>): Boolean;
    function InternalBindDataSet(ADataSet: TDataSet; AComponent: TComponent): Boolean;
    property Bindings: TExpressionList read FBindings;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OnObjectDestroyed(AMessage: IMessage);

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
    procedure BindCollection(AServiceType: PTypeInfo;
                             const ACollection: TEnumerable<TObject>;
                             const ATarget: ICollectionViewProvider;
                             const ATemplate: TDataTemplateClass); override;
    procedure BindDataSet(const ADataSet: TDataSet;
                             const ATarget: ICollectionViewProvider;
                             const ATemplate: TDataTemplateClass); override;
    procedure BindAction(const AAction: IBindableAction;
                     const AExecute: TExecuteMethod;
                     const ACanExecute: TCanExecuteMethod = nil); overload; override;
    procedure ClearBindings; override;

    class procedure RegisterClassObjectListCollectionBinder(const AClass: TClass; AProcedure: TProc<PTypeInfo, TComponent, TEnumerable<TObject>>); static;
    class procedure RegisterClassDataSetCollectionBinder(const AClass: TClass; AProcedure: TProc<TDataSet, TComponent>); static;
  end;

implementation

uses
  MVVM.Core,

  Spring;

{ TEstrategia_LiveBindings }

procedure TStrategy_LiveBindings.Bind(const ASource: TObject; const ASourcePropertyPath: String;
                                        const ATarget: TObject; const ATargetPropertyPath: String;
                                        const ADirection: EBindDirection;
                                        const AFlags: EBindFlags;
                                        const AValueConverterClass: TBindingValueConverterClass;
                                        const AExtraParams: TBindExtraParams);
var
  LSrcProperty, LDstProperty: String;
  lAssocInput, lAssocOutput : IScope;
  lManaged                  : TInternalBindindExpression;
  LOptions                  : TBindings.TCreateOptions;
  LNotifyPropertyChanged       : MVVM.Interfaces.INotifyPropertyChanged;
  LNotifyPropertyChangeTracking: MVVM.Interfaces.INotifyPropertyChangeTracking;
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
  LManaged:= TInternalBindindExpression.Create([LAssocInput], LSrcProperty,
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
    LManaged := TInternalBindindExpression.Create(
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

procedure TStrategy_LiveBindings.Bind(const ASources: TSourcePairArray;
  const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String;
  const ATargetPropertyPath: String; const AFlags: EBindFlags;
  const AExtraParams: TBindExtraParams);
var
  LSrcProperty, LDstProperty: String;
  lAssocInput, lAssocOutput : IScope;
  lManaged                  : TInternalBindindExpression;
  LOptions                  : TBindings.TCreateOptions;
  LArrayAsociacion          : array of TBindingAssociation;
  I, LCnt                   : Integer;
  LNotifyPropertyChanged       : MVVM.Interfaces.INotifyPropertyChanged;
  LNotifyPropertyChangeTracking: MVVM.Interfaces.INotifyPropertyChangeTracking;
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
  LManaged    := TInternalBindindExpression.Create(
                                                [LAssocInput], LSrcProperty,
                                                [LAssocOutput], LDstProperty,
                                                nil,
                                                nil,
                                                LOptions);
  FBindings.Add(LManaged);
end;

procedure TStrategy_LiveBindings.BindAction(const AAction: IBindableAction; const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod);
begin
  Guard.CheckNotNull(AAction, '<BindAction> (Param=AAction) no puede ser null');
  AAction.Bind(AExecute, ACanExecute);
end;

procedure TStrategy_LiveBindings.BindCollection(AServiceType: PTypeInfo; const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
var
  LView: ICollectionView;
begin
  Guard.CheckNotNull(ATarget, '<BindCollection> (Param=ATarget) no puede ser null');
  Guard.CheckNotNull(ATarget, '<BindCollection> (Param=ATemplate) no puede ser null');

  LView := ATarget.GetCollectionView;
  if (LView = nil) then
    raise EBindError.CreateFmt('Function %s.GetCollectionView cannot return nil', [TObject(ATarget).QualifiedClassName]);

  LView.Template := ATemplate;
  LView.Source   := TCollectionSource(ACollection);

  if InternalBindCollection(AServiceType, LView.Component, ACollection) then
    raise EBindError.CreateFmt('Component %s has not registered a binder', [TObject(ATarget).QualifiedClassName]);
end;

procedure TStrategy_LiveBindings.BindDataSet(const ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
var
  LView: ICollectionView;
begin
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ADataSet) no puede ser null');
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ATarget) no puede ser null');
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ATemplate) no puede ser null');

  LView := ATarget.GetCollectionView;
  if (LView = nil) then
    raise EBindError.CreateFmt('Function %s.GetCollectionView cannot return nil', [TObject(ATarget).QualifiedClassName]);

  LView.Template := ATemplate;

  if InternalBindDataSet(ADataSet, LView.Component) then
    raise EBindError.CreateFmt('Component %s has not registered a binder', [TObject(ATarget).QualifiedClassName]);
  //LAdapterBindSource := TAdapterBindSource.Create(nil); //DAVID
  //LAdapterBindSource.
end;

// DAVID, liberacion
procedure TStrategy_LiveBindings.ClearBindings;
var
  i: TInternalBindindExpression;
begin
  for i in FBindings do
  begin
    TBindings.RemoveBinding(i.Expression);
  end;
  FBindings.Clear;
end;

constructor TStrategy_LiveBindings.Create;
begin
  inherited;
  FBindings := TExpressionList.Create(false {AOwnsObjects});
end;

class constructor TStrategy_LiveBindings.CreateC;
begin
  FObjectListLinkers    := TCollections.CreateDictionary<TClass, TProc<PTypeInfo, TComponent, TEnumerable<TObject>>>;
  FObjectDataSetLinkers := TCollections.CreateDictionary<TClass, TProc<TDataSet, TComponent>>;
end;

destructor TStrategy_LiveBindings.Destroy;
begin
  ClearBindings;
  FBindings.Free;
  inherited;
end;

class destructor TStrategy_LiveBindings.DestroyC;
begin
  FObjectListLinkers    := nil;
  FObjectDataSetLinkers := nil;
end;

function TStrategy_LiveBindings.InternalBindCollection(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>): Boolean;
var
  LProc : TProc<PTypeInfo, TComponent, TEnumerable<TObject>>;
  LClass: TClass;
begin
  Result := False;
  for LClass in FObjectListLinkers.Keys do
  begin
    if AComponent.ClassType.InheritsFrom(LClass) then
    begin
      LProc(AServiceType, AComponent, ACollection);
      Exit(True);
    end;
  end;
end;

function TStrategy_LiveBindings.InternalBindDataSet(ADataSet: TDataSet; AComponent: TComponent): Boolean;
var
  LProc : TProc<TDataSet, TComponent>;
  LClass: TClass;
begin
  Result := False;
  for LClass in FObjectDataSetLinkers.Keys do
  begin
    if AComponent.ClassType.InheritsFrom(LClass) then
    begin
      LProc(ADataSet, AComponent);
      Exit(True);
    end;
  end;
end;

procedure TStrategy_LiveBindings.Notify(const AObject: TObject; const APropertyName: string);
begin
  TBindings.Notify(AObject, APropertyName);
end;

procedure TStrategy_LiveBindings.OnObjectDestroyed(AMessage: IMessage);
begin
  //
end;

class procedure TStrategy_LiveBindings.RegisterClassDataSetCollectionBinder(const AClass: TClass; AProcedure: TProc<TDataSet, TComponent>);
begin
  FObjectDataSetLinkers.AddOrSetValue(AClass, AProcedure);
end;

class procedure TStrategy_LiveBindings.RegisterClassObjectListCollectionBinder(const AClass: TClass; AProcedure: TProc<PTypeInfo, TComponent, TEnumerable<TObject>>);
begin
  FObjectListLinkers.AddOrSetValue(AClass, AProcedure);
end;

{ TStrategy_LiveBindings.TInternalBindindExpression }

constructor TStrategy_LiveBindings.TInternalBindindExpression.Create(
                                                                    const InputScopes: array of IScope; const BindExprStr: string;
                                                                    const OutputScopes: array of IScope; const OutputExpr: string;
                                                                    const OutputConverter: IValueRefConverter;
                                                                    Manager: TBindingManager;
                                                                    Options: TBindings.TCreateOptions);
var
  FChannel: TMessageChannel_OBJECT_DESTROYED;
begin
  inherited Create(nil);
  FSynchronizer     := TMREWSync.Create;
  FTrackedInstances := TCollections.CreateSet<Pointer>;

  FChannel          := MVVMCore.Container.Resolve<TMessageChannel_OBJECT_DESTROYED>;
  FListener         := TMessageListener_TMessage_Object_Destroyed.Create(FChannel);
  FListener.FilterCondition := function (AMessage: IMessage): Boolean
                               begin
                                 FSynchronizer.BeginRead;
                                 try
                                   Result := FTrackedInstances.Contains(TMessage_Object_Destroyed(AMessage).ObjectDestroyed)
                                 finally
                                   FSynchronizer.EndRead;
                                 end;
                               end;

  FExpression       := TBindings.CreateManagedBinding(
                                InputScopes, BindExprStr,
                                OutputScopes, OutputExpr,
                                nil,
                                nil,
                                Options);
end;

destructor TStrategy_LiveBindings.TInternalBindindExpression.Destroy;
var
  P       : Pointer;
  Instance: TObject;
begin
  if (FTrackedInstances <> nil) then
  begin
    for P in FTrackedInstances do
    begin
      Instance := TObject(P);
      RemoveFreeNotification(Instance);
    end;
    FTrackedInstances := nil;
  end;
  inherited;
end;

procedure TStrategy_LiveBindings.TInternalBindindExpression.HandleFreeEvent(const ASender, AInstance: TObject);
begin
  FSynchronizer.BeginWrite;
  try
    FTrackedInstances.Remove(AInstance);
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TStrategy_LiveBindings.TInternalBindindExpression.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    HandleFreeEvent(AComponent, AComponent);
end;

procedure TStrategy_LiveBindings.TInternalBindindExpression.OnObjectDestroyed(AMessage: IMessage);
begin
  FSynchronizer.BeginWrite;
  try
    FTrackedInstances.Remove(TMessage_Object_Destroyed(AMessage).ObjectDestroyed);
  finally
    FSynchronizer.EndWrite;
  end;

end;

procedure TStrategy_LiveBindings.TInternalBindindExpression.RemoveFreeNotification(const AInstance: TObject);
begin
  if (AInstance is TComponent) then
    TComponent(AInstance).RemoveFreeNotification(Self);
end;

procedure TStrategy_LiveBindings.TInternalBindindExpression.SetFreeNotification(const AInstance: TObject);
var
  NotifyFree: INotifyFree;
begin
  if (AInstance is TComponent) then
  begin
    FSynchronizer.BeginWrite;
    try
      FTrackedInstances.Add(AInstance);
    finally
      FSynchronizer.EndWrite;
    end;
    TComponent(AInstance).FreeNotification(Self);
  end
  else if Supports(AInstance, INotifyFree, NotifyFree) then
  begin
    FSynchronizer.BeginWrite;
    try
      FTrackedInstances.Add(AInstance);
    finally
      FSynchronizer.EndWrite;
    end;
  end;
end;

initialization

TBindingManager.RegisterBindingStrategy(STRATEGY_NAME, TStrategy_LiveBindings);

end.
