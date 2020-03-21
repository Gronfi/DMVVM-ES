unit MVVM.Bindings.LiveBindings;

{$HPPEMIT 'MVVM.Bindings.LiveBindings'}

interface

uses
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  System.Bindings.Expression, System.Bindings.Helper,
  System.Bindings.EvalProtocol, System.Bindings.Outputs,
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
    TInternalBindindExpression = class(TBindingDefault)
    private
      FExpression: TBindingExpression;
    public
      constructor Create(ABindingStrategy: IBindingStrategy;
        const InputScopes: array of IScope; const BindExprStr: string;
        const OutputScopes: array of IScope; const OutputExpr: string;
        const OutputConverter: IValueRefConverter;
        Manager: TBindingManager = nil;
        Options: TBindings.TCreateOptions = [coNotifyOutput]); reintroduce;
      destructor Destroy; override;

      property Expression: TBindingExpression read FExpression;
    end;
  private
  class var
    FObjectListLinkers
      : IDictionary<TClass, TProc<PTypeInfo, TComponent, TEnumerable<TObject>>>;
    FObjectDataSetLinkers: IDictionary<TClass, TProc<TDataSet, TComponent>>;
  protected type
    TExpressionList = TObjectList<TBindingBase>;
  private
  var
    FEnabled : Boolean;
    FBindings: TExpressionList;

    class constructor CreateC;
    class destructor DestroyC;
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(const AValue: Boolean); override;

    function InternalBindCollection(AServiceType: PTypeInfo;
      AComponent: TComponent; ACollection: TEnumerable<TObject>): Boolean;
    function InternalBindDataSet(ADataSet: TDataSet;
      AComponent: TComponent): Boolean;
    property Bindings: TExpressionList read FBindings;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddBinding(ABinding: TBindingBase); override;
    function BindsCount: Integer; override;
    procedure ClearBindings; override;

    function GetPlatformBindActionCommandType: TBindingCommandClass; override;

    procedure Notify(const AObject: TObject; const APropertyName: string = '');
      overload; override;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
      const ATarget: TObject; const ATargetPropertyPath: String;
      const ADirection: EBindDirection = EBindDirection.OneWay;
      const AFlags: EBindFlags = [];
      const AValueConverterClass: TBindingValueConverterClass = nil;
      const AExtraParams: TBindExtraParams = []); overload; override;
    procedure Bind(const ASources: TSourcePairArray;
      const ASourceExpresion: String; const ATarget: TObject;
      const ATargetAlias: String; const ATargetPropertyPath: String;
      const AFlags: EBindFlags = []; const AExtraParams: TBindExtraParams = []);
      overload; override;
    procedure BindCollection(AServiceType: PTypeInfo;
      const ACollection: TEnumerable<TObject>;
      const ATarget: ICollectionViewProvider;
      const ATemplate: TDataTemplateClass); override;
    procedure BindDataSet(const ADataSet: TDataSet;
      const ATarget: ICollectionViewProvider;
      const ATemplate: TDataTemplateClass); override;
    procedure BindAction(AAction: IBindableAction); overload; override;

    class procedure RegisterClassObjectListCollectionBinder
      (const AClass: TClass; AProcedure: TProc < PTypeInfo, TComponent,
      TEnumerable < TObject >> ); static;
    class procedure RegisterClassDataSetCollectionBinder(const AClass: TClass;
      AProcedure: TProc<TDataSet, TComponent>); static;

    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

implementation

uses
  MVVM.Core,

  Spring;

{ TEstrategia_LiveBindings }

procedure TStrategy_LiveBindings.Bind(const ASource: TObject;
  const ASourcePropertyPath: String; const ATarget: TObject;
  const ATargetPropertyPath: String; const ADirection: EBindDirection;
  const AFlags: EBindFlags; const AValueConverterClass
  : TBindingValueConverterClass; const AExtraParams: TBindExtraParams);
var
  LSrcProperty, LDstProperty: String;
  lAssocInput, lAssocOutput: IScope;
  lManaged: TInternalBindindExpression;
  LOptions: TBindings.TCreateOptions;
  LNotifyPropertyChanged: INotifyChangedProperty;
  LNotifyPropertyChangeTracking: INotifyPropertyTrackingChanged;
begin
  LOptions := [coNotifyOutput];
  if not(EBindFlag.DontApply in AFlags) then
    LOptions := LOptions + [coEvaluate];

  lAssocInput := TBindings.CreateAssociationScope([Associate(ASource, 'Src')]);
  lAssocOutput := TBindings.CreateAssociationScope([Associate(ATarget, 'Dst')]);
  if not(EBindFlag.UsesExpressions in AFlags) then
  begin
    LSrcProperty := 'Src.' + ASourcePropertyPath;
    LDstProperty := 'Dst.' + ATargetPropertyPath;
  end
  else
  begin
    LSrcProperty := ASourcePropertyPath;
    LDstProperty := ATargetPropertyPath;
  end;
  lManaged := TInternalBindindExpression.Create(Self, [lAssocInput],
    LSrcProperty, [lAssocOutput], LDstProperty, nil, nil, LOptions);
  // Asociacion de evento de fin

  lManaged.SetManager(ASource);
  lManaged.SetFreeNotification(ASource);
  lManaged.SetManager(ATarget);
  lManaged.SetFreeNotification(ATarget);
  AddBinding(lManaged);
  if ADirection = EBindDirection.TwoWay then
  begin
    lAssocInput := TBindings.CreateAssociationScope
      ([Associate(ATarget, 'Src')]);
    lAssocOutput := TBindings.CreateAssociationScope
      ([Associate(ASource, 'Dst')]);
    if not(EBindFlag.UsesExpressions in AFlags) then
    begin
      LSrcProperty := 'Src.' + ATargetPropertyPath;
      LDstProperty := 'Dst.' + ASourcePropertyPath;
    end
    else
    begin
      LSrcProperty := ATargetPropertyPath;
      LDstProperty := ASourcePropertyPath;
    end;
    lManaged := TInternalBindindExpression.Create(Self, [lAssocInput],
      LSrcProperty, [lAssocOutput], LDstProperty, nil, nil, LOptions);
    // Asociacion de evento de fin
    lManaged.SetManager(ASource);
    lManaged.SetFreeNotification(ASource);
    lManaged.SetManager(ATarget);
    lManaged.SetFreeNotification(ATarget);
    AddBinding(lManaged);
  end;
end;

procedure TStrategy_LiveBindings.AddBinding(ABinding: TBindingBase);
begin
  AdquireWrite;
  try
    FBindings.Add(ABinding)
  finally
    ReleaseWrite
  end;

end;

procedure TStrategy_LiveBindings.Bind(const ASources: TSourcePairArray;
  const ASourceExpresion: String; const ATarget: TObject;
  const ATargetAlias: String; const ATargetPropertyPath: String;
  const AFlags: EBindFlags; const AExtraParams: TBindExtraParams);
var
  LSrcProperty, LDstProperty: String;
  lAssocInput, lAssocOutput: IScope;
  lManaged: TInternalBindindExpression;
  LOptions: TBindings.TCreateOptions;
  LArrayAsociacion: array of TBindingAssociation;
  I, LCnt: Integer;
  // LNotifyPropertyChanged       : MVVM.Interfaces.INotifyChangedProperty;
  // LNotifyPropertyChangeTracking: MVVM.Interfaces.INotifyPropertyChangeTracking;
begin
  LOptions := [coNotifyOutput];
  if not(DontApply in AFlags) then
    LOptions := LOptions + [coEvaluate];
  LCnt := Length(ASources);
  SetLength(LArrayAsociacion, LCnt);
  for I := 0 to LCnt - 1 do
  begin
    LArrayAsociacion[I] := Associate(ASources[I].Source, ASources[I].Alias);
    // Settings especiales segun interfaces
    // if Supports(ASources[I].Source, INotifyChangedProperty, LNotifyPropertyChanged) then
    // begin
    // LNotifyPropertyChanged.BindingStrategy := Self;
    // end;
    // if Supports(ASources[I].Source, INotifyPropertyChangeTracking, LNotifyPropertyChangeTracking) then
    // begin
    // LNotifyPropertyChangeTracking.BindingStrategy := Self;
    // end;
  end;

  lAssocInput := TBindings.CreateAssociationScope(LArrayAsociacion);
  lAssocOutput := TBindings.CreateAssociationScope
    ([Associate(ATarget, ATargetAlias)]);
  LSrcProperty := ASourceExpresion;
  LDstProperty := ATargetPropertyPath;
  lManaged := TInternalBindindExpression.Create(Self, [lAssocInput],
    LSrcProperty, [lAssocOutput], LDstProperty, nil, nil, LOptions);
  // asociacion de eventos de fin
  for I := 0 to LCnt - 1 do
  begin
    lManaged.SetFreeNotification(ASources[I].Source);
  end;
  lManaged.SetManager(ATarget);
  lManaged.SetFreeNotification(ATarget);
  FBindings.Add(lManaged);
end;

procedure TStrategy_LiveBindings.BindAction(AAction: IBindableAction);
var
  LCommandClass: TBindingCommandClass;
begin
  Guard.CheckNotNull(AAction, '<BindAction> (Param=AAction) cannot be null');

end;

procedure TStrategy_LiveBindings.BindCollection(AServiceType: PTypeInfo;
  const ACollection: TEnumerable<TObject>;
  const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
var
  LView: ICollectionView;
begin
  Guard.CheckNotNull(ATarget,
    '<BindCollection> (Param=ATarget) cannot be null');
  Guard.CheckNotNull(ATarget,
    '<BindCollection> (Param=ATemplate) cannot be null');

  LView := ATarget.GetCollectionView;
  if (LView = nil) then
    raise EBindError.CreateFmt
      ('Function %s.GetCollectionView cannot return nil',
      [TObject(ATarget).QualifiedClassName]);

  LView.Template := ATemplate;
  LView.Source := TCollectionSource(ACollection);

  if InternalBindCollection(AServiceType, LView.Component, ACollection) then
    raise EBindError.CreateFmt('Component %s has not registered a binder',
      [TObject(ATarget).QualifiedClassName]);
end;

procedure TStrategy_LiveBindings.BindDataSet(const ADataSet: TDataSet;
  const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
var
  LView: ICollectionView;
begin
  Guard.CheckNotNull(ATarget,
    '<BindDataSet> (Param=ADataSet) cannot be null');
  Guard.CheckNotNull(ATarget,
    '<BindDataSet> (Param=ATarget) cannot be null');
  Guard.CheckNotNull(ATarget,
    '<BindDataSet> (Param=ATemplate) cannot be null');

  LView := ATarget.GetCollectionView;
  if (LView = nil) then
    raise EBindError.CreateFmt
      ('Function %s.GetCollectionView cannot return nil',
      [TObject(ATarget).QualifiedClassName]);

  LView.Template := ATemplate;

  if InternalBindDataSet(ADataSet, LView.Component) then
    raise EBindError.CreateFmt('Component %s has not registered a binder',
      [TObject(ATarget).QualifiedClassName]);
  // LAdapterBindSource := TAdapterBindSource.Create(nil); //DAVID
  // LAdapterBindSource.
end;

function TStrategy_LiveBindings.BindsCount: Integer;
begin
  AdquireRead;
  try
    Result := FBindings.Count
  finally
    ReleaseRead;
  end;
end;

// DAVID, liberacion
procedure TStrategy_LiveBindings.ClearBindings;
var
  I: TBindingBase;
begin
  AdquireWrite;
  try
    for I in FBindings do
    begin
      I.Free;
      // TBindings.RemoveBinding(i.Expression);
    end;
    FBindings.Clear;
  finally
    ReleaseWrite;
  end;
end;

constructor TStrategy_LiveBindings.Create;
begin
  inherited;
  FBindings := TExpressionList.Create(false { AOwnsObjects } );
end;

class constructor TStrategy_LiveBindings.CreateC;
begin
  FObjectListLinkers := TCollections.CreateDictionary<TClass,
    TProc<PTypeInfo, TComponent, TEnumerable<TObject>>>;
  FObjectDataSetLinkers := TCollections.CreateDictionary<TClass,
    TProc<TDataSet, TComponent>>;
end;

destructor TStrategy_LiveBindings.Destroy;
begin
  ClearBindings;
  FBindings.Free;
  inherited;
end;

class destructor TStrategy_LiveBindings.DestroyC;
begin
  FObjectListLinkers := nil;
  FObjectDataSetLinkers := nil;
end;

function TStrategy_LiveBindings.GetEnabled: Boolean;
begin
  FS
end;

function TStrategy_LiveBindings.InternalBindCollection(AServiceType: PTypeInfo;
  AComponent: TComponent; ACollection: TEnumerable<TObject>): Boolean;
var
  LClass: TClass;
begin
  Result := false;
  for LClass in FObjectListLinkers.Keys do
  begin
    if AComponent.ClassType.InheritsFrom(LClass) then
    begin
      FObjectListLinkers[LClass](AServiceType, AComponent, ACollection);
      Exit(True);
    end;
  end;
end;

function TStrategy_LiveBindings.InternalBindDataSet(ADataSet: TDataSet;
  AComponent: TComponent): Boolean;
var
//  LProc: TProc<TDataSet, TComponent>;
  LClass: TClass;
begin
  Result := false;
  for LClass in FObjectDataSetLinkers.Keys do
  begin
    if AComponent.ClassType.InheritsFrom(LClass) then
    begin
      FObjectDataSetLinkers[LClass](ADataSet, AComponent);
      Exit(True);
    end;
  end;
end;

procedure TStrategy_LiveBindings.Notify(const AObject: TObject;
  const APropertyName: string);
begin
  TBindings.Notify(AObject, APropertyName);
end;

class procedure TStrategy_LiveBindings.RegisterClassDataSetCollectionBinder
  (const AClass: TClass; AProcedure: TProc<TDataSet, TComponent>);
begin
  FObjectDataSetLinkers.AddOrSetValue(AClass, AProcedure);
end;

class procedure TStrategy_LiveBindings.RegisterClassObjectListCollectionBinder
  (const AClass: TClass; AProcedure: TProc < PTypeInfo, TComponent,
  TEnumerable < TObject >> );
begin
  FObjectListLinkers.AddOrSetValue(AClass, AProcedure);
end;

procedure TStrategy_LiveBindings.SetEnabled(const AValue: Boolean);
begin
  inherited;

end;

{ TStrategy_LiveBindings.TInternalBindindExpression }

constructor TStrategy_LiveBindings.TInternalBindindExpression.Create
  (ABindingStrategy: IBindingStrategy; const InputScopes: array of IScope;
  const BindExprStr: string; const OutputScopes: array of IScope;
  const OutputExpr: string; const OutputConverter: IValueRefConverter;
  Manager: TBindingManager; Options: TBindings.TCreateOptions);
begin
  inherited Create(ABindingStrategy);
  FExpression := TBindings.CreateManagedBinding(InputScopes, BindExprStr,
    OutputScopes, OutputExpr, nil, nil, Options);
end;

destructor TStrategy_LiveBindings.TInternalBindindExpression.Destroy;
begin
  TBindings.RemoveBinding(FExpression);
  FExpression.Free;
  inherited;
end;

initialization

TBindingManager.RegisterBindingStrategy(STRATEGY_NAME, TStrategy_LiveBindings);

end.
