unit MVVM.Bindings.LiveBindings;

interface

uses
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  System.Bindings.Expression, System.Bindings.Helper,
  System.RTTI,

  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types,
  MVVM.Bindings;

const
  ESTRATEGIA_NAME = 'LIVEBINDINGS';

type
  TEstrategia_LiveBindings = class(TBindingStrategyBase)
  private
    class var
      FObjectListLinkers   : IDictionary<TClass, TProc<PTypeInfo, TComponent, TEnumerable<TObject>>>;
      FObjectDataSetLinkers: IDictionary<TClass, TProc<TComponent>>;
  protected
    type
      TExpressionList = TObjectList<TBindingExpression>;
  private
    var
      FBindings: TExpressionList;

    class constructor CreateC;
    class destructor DestroyC;
  protected
    function InternalBindCollection(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>): Boolean;
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
    procedure BindCollection(AServiceType: PTypeInfo;
                             const ACollection: TEnumerable<TObject>;
                             const ATarget: ICollectionViewProvider;
                             const ATemplate: TDataTemplateClass); override;
    procedure BindAction(const AAction: IBindableAction;
                     const AExecute: TExecuteMethod;
                     const ACanExecute: TCanExecuteMethod = nil); overload; override;
    procedure ClearBindings; override;

    class procedure RegisterClassObjectListCollectionBinder(const AClass: TClass; AProcedure: TProc<PTypeInfo, TComponent, TEnumerable<TObject>>); static;
    class procedure RegisterClassDataSetCollectionBinder(const AClass: TClass; AProcedure: TProc<TComponent>); static;
  end;

implementation

uses
  System.Bindings.EvalProtocol,
  //Data.Bind.ObjectScope,

  Spring;

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

procedure TEstrategia_LiveBindings.BindCollection(AServiceType: PTypeInfo; const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
var
  LView             : ICollectionView;
  //LAdapterBindSource: TAdapterBindSource;
begin
  Assert(Assigned(ATarget));
  Assert(Assigned(ATemplate));

  LView := ATarget.GetCollectionView;
  if (LView = nil) then
    raise EBindError.CreateFmt('Function %s.GetCollectionView cannot return nil', [TObject(ATarget).QualifiedClassName]);

  LView.Template := ATemplate;
  LView.Source   := TCollectionSource(ACollection);

  if InternalBindCollection(AServiceType, LView.Component, ACollection) then
    raise EBindError.CreateFmt('Component %s has not registered a binder', [TObject(ATarget).QualifiedClassName]);
  //LAdapterBindSource := TAdapterBindSource.Create(nil); //DAVID
  //LAdapterBindSource.
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

class constructor TEstrategia_LiveBindings.CreateC;
begin
  FObjectListLinkers    := TCollections.CreateDictionary<TClass, TProc<PTypeInfo, TComponent, TEnumerable<TObject>>>;
  FObjectDataSetLinkers := TCollections.CreateDictionary<TClass, TProc<TComponent>>;
end;

destructor TEstrategia_LiveBindings.Destroy;
begin
  ClearBindings;
  FBindings.Free;
  inherited;
end;

class destructor TEstrategia_LiveBindings.DestroyC;
begin
  FObjectListLinkers    := nil;
  FObjectDataSetLinkers := nil;
end;

function TEstrategia_LiveBindings.InternalBindCollection(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>): Boolean;
var
  LProc: TProc<PTypeInfo, TComponent, TEnumerable<TObject>>;
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

procedure TEstrategia_LiveBindings.Notify(const AObject: TObject; const APropertyName: string);
begin
  TBindings.Notify(AObject, APropertyName);
end;

class procedure TEstrategia_LiveBindings.RegisterClassDataSetCollectionBinder(const AClass: TClass; AProcedure: TProc<TComponent>);
begin
  FObjectDataSetLinkers.AddOrSetValue(AClass, AProcedure);
end;

class procedure TEstrategia_LiveBindings.RegisterClassObjectListCollectionBinder(const AClass: TClass; AProcedure: TProc<PTypeInfo, TComponent, TEnumerable<TObject>>);
begin
  FObjectListLinkers.AddOrSetValue(AClass, AProcedure);
end;

initialization

TBindingManager.RegistrarBindingStrategy(ESTRATEGIA_NAME, TEstrategia_LiveBindings);

end.
