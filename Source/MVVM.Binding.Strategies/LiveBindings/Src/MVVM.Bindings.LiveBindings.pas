unit MVVM.Bindings.LiveBindings;

interface

uses
  System.Generics.Collections,
  System.Bindings.Expression, System.Bindings.Helper,
  System.RTTI,

  MVVM.Interfaces,
  MVVM.Types,
  MVVM.Bindings;

const
  ESTRATEGIA_NAME = 'LIVEBINDINGS';

type
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
    procedure BindCollection(const ACollection: TEnumerable<TObject>;
                             const ATarget: ICollectionViewProvider;
                             const ATemplate: TDataTemplateClass); override;
    procedure BindAction(const AAction: IBindableAction;
                     const AExecute: TExecuteMethod;
                     const ACanExecute: TCanExecuteMethod = nil); overload; override;
    procedure ClearBindings; override;
  end;

implementation

uses
  System.SysUtils,
  System.Bindings.EvalProtocol,
  Data.Bind.ObjectScope,

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

procedure TEstrategia_LiveBindings.BindCollection(const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
var
  LView             : ICollectionView;
  LAdapterBindSource: TAdapterBindSource;
  //LLink             : TLinkGridToDatasource;
begin
  Assert(Assigned(ATarget));
  Assert(Assigned(ATemplate));

  LView := ATarget.GetCollectionView;
  if (LView = nil) then
    raise EBindError.CreateFmt('Function %s.GetCollectionView cannot return nil', [TObject(ATarget).QualifiedClassName]);

  LView.Template := ATemplate;
  LView.Source   := TCollectionSource(ACollection);

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

initialization

TBindingManager.RegistrarBindingStrategy(ESTRATEGIA_NAME, TEstrategia_LiveBindings);

end.
