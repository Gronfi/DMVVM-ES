unit MVVM.Bindings.LiveBindings;

interface

uses
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  System.RTTI,

  System.Bindings.Expression, System.Bindings.Helper,
  System.Bindings.EvalProtocol, System.Bindings.Outputs,


  Data.DB,

  System.Actions,

  Spring.Collections,

  MVVM.Bindings.Commands,
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
      constructor Create(ABindingStrategy: IBindingStrategy; const InputScopes: array of IScope; const BindExprStr: string; const OutputScopes: array of IScope; const OutputExpr: string; const OutputConverter: IValueRefConverter; Manager: TBindingManager = nil; Options: TBindings.TCreateOptions = [coNotifyOutput]); reintroduce;
      destructor Destroy; override;

      function IsObjectInBinding(const AObject: TObject): Boolean; override;
      procedure OnBindingAssignedValueEvent(AssignValueRec: TBindingAssignValueRec; const AValue: TValue);

      property Expression: TBindingExpression read FExpression;
    end;
  private
  class var
    FObjectListLinkers: IDictionary<TClass, TProc<PTypeInfo, TComponent, TEnumerable<TObject>>>;
    FObjectDataSetLinkers: IDictionary<TClass, TProc<TDataSet, TComponent>>;
  private
  var
    FEnabled: Boolean;

    class constructor CreateC;
    class destructor DestroyC;
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(const AValue: Boolean); override;

    procedure DoEnableAll;
    procedure DoDisableAll;

    function ExistBindingCommandActionFor(AObject: TContainedAction; out ACommand: IBinding): Boolean;

    function InternalBindCollection(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>): Boolean;
    function InternalBindDataSet(ADataSet: TDataSet; AComponent: TComponent): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    // function GetPlatformBindActionCommandType: TBindingCommandClass; override;

    procedure Notify(const AObject: TObject; const APropertyName: string = ''); overload; override;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String; const ATarget: TObject; const ATargetPropertyPath: String; const ADirection: EBindDirection = EBindDirection.OneWay; const AFlags: EBindFlags = []; const AValueConverterClass: TValueConverterClass = nil; const AExtraParams: TBindExtraParams = []); overload; override;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String; const AFlags: EBindFlags = []; const AExtraParams: TBindExtraParams = []); overload; override;
    procedure BindCollection(AServiceType: PTypeInfo; const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass); override;

    // DataSets
    procedure BindDataSet(const ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass = nil); override;
    function ClearDataSetBindingFromComponent(ATarget: TComponent): Boolean;

    // - Components
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String); overload; override;
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const AValueConverterClass: TValueConverterClass); overload; override;
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const ACustomFormat: String); overload; override;

    // -- Combobox
    procedure BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const AOnlyFillValues: Boolean = True); overload; override;
    procedure BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomFormat: String; const AOnlyFillValues: Boolean = True); overload; override;

    // -- ListBox
    procedure BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomDisplayExpression: string; const AOnlyFillValues: Boolean = True); overload; override;
    procedure BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListBoxConversionData; const AOnlyFillValues: Boolean = True); overload; override;

    // -- ListView
    procedure BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomDisplayExpression: String; const AOnlyFillValues: Boolean = True); overload; override;
    procedure BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListViewConversionData; const AOnlyFillValues: Boolean = True); overload; override;
    procedure BindDataSetAppendFieldToListView(ADataSet: TDataSet; ATarget: TComponent; const ALink: TListViewConversionData; const AOnlyFillValues: Boolean = True); overload; override;

    // -- Grids
    procedure BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent); overload; override;
    procedure BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent; const AColumnLinks: array of TGridColumnTemplate); overload; override;

    // Actions
    procedure BindAction(AAction: IBindableAction); overload; override;

    // Unbinds
    procedure Unbind(const ASource: TObject); override;

    class procedure RegisterClassObjectListCollectionBinder(const AClass: TClass; AProcedure: TProc < PTypeInfo, TComponent, TEnumerable < TObject >> ); static;
    class procedure RegisterClassDataSetCollectionBinder(const AClass: TClass; AProcedure: TProc<TDataSet, TComponent>); static;

    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

implementation

uses
  Data.Bind.Grid,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.Controls,
  Data.Bind.ObjectScope,

  MVVM.Utils,

  Spring;

{ TStrategy_LiveBindings }

procedure TStrategy_LiveBindings.Bind(const ASource: TObject; const ASourcePropertyPath: String; const ATarget: TObject; const ATargetPropertyPath: String; const ADirection: EBindDirection; const AFlags: EBindFlags; const AValueConverterClass: TValueConverterClass; const AExtraParams: TBindExtraParams);
var
  LSrcProperty, LDstProperty: String;
  lAssocInput, lAssocOutput: IScope;
  lManaged1, lManaged2: IBindingDefault;
  LToAdd1, LToAdd2: IBinding;
  LOptions: TBindings.TCreateOptions;
begin
  LOptions := [coNotifyOutput];
  if not(EBindFlag.DontApply in AFlags) then
    LOptions := LOptions + [coEvaluate];

  lAssocInput  := TBindings.CreateAssociationScope([Associate(ASource, 'Src')]);
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
  LToAdd1 := TInternalBindindExpression.Create(Self, [lAssocInput], LSrcProperty, [lAssocOutput], LDstProperty, nil, nil, LOptions);
  Supports(LToAdd1, IBindingDefault, lManaged1);

  // Free Event
  lManaged1.SetManager(ASource);
  lManaged1.SetFreeNotification(ASource);
  lManaged1.SetManager(ATarget);
  lManaged1.SetFreeNotification(ATarget);
  AddBinding(LToAdd1);
  // Bidirectional?
  if ADirection = EBindDirection.TwoWay then
  begin
    lAssocInput  := TBindings.CreateAssociationScope([Associate(ATarget, 'Src')]);
    lAssocOutput := TBindings.CreateAssociationScope([Associate(ASource, 'Dst')]);
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
    LToAdd2 := TInternalBindindExpression.Create(Self, [lAssocInput], LSrcProperty, [lAssocOutput], LDstProperty, nil, nil, LOptions);
    Supports(LToAdd2, IBindingDefault, lManaged2);
    // Free Event
    lManaged2.SetManager(ASource);
    lManaged2.SetFreeNotification(ASource);
    lManaged2.SetManager(ATarget);
    lManaged2.SetFreeNotification(ATarget);
    AddBinding(LToAdd2);
  end;
end;

procedure TStrategy_LiveBindings.Bind(const ASources: TSourcePairArray; const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String; const AFlags: EBindFlags; const AExtraParams: TBindExtraParams);
var
  LSrcProperty, LDstProperty: String;
  lAssocInput, lAssocOutput: IScope;
  lManaged: IBindingDefault;
  LToAdd: IBinding;
  LOptions: TBindings.TCreateOptions;
  LArrayAsociacion: array of TBindingAssociation;
  I, LCnt: Integer;
begin
  LOptions := [coNotifyOutput];
  if not(DontApply in AFlags) then
    LOptions := LOptions + [coEvaluate];
  LCnt       := Length(ASources);
  SetLength(LArrayAsociacion, LCnt);
  for I := 0 to LCnt - 1 do
  begin
    LArrayAsociacion[I] := Associate(ASources[I].Source, ASources[I].Alias);
  end;

  lAssocInput  := TBindings.CreateAssociationScope(LArrayAsociacion);
  lAssocOutput := TBindings.CreateAssociationScope([Associate(ATarget, ATargetAlias)]);
  LSrcProperty := ASourceExpresion;
  LDstProperty := ATargetPropertyPath;
  LToAdd       := TInternalBindindExpression.Create(Self, [lAssocInput], LSrcProperty, [lAssocOutput], LDstProperty, nil, nil, LOptions);
  Supports(LToAdd, IBindingDefault, lManaged);
  // asociacion de eventos de fin
  for I := 0 to LCnt - 1 do
  begin
    lManaged.SetFreeNotification(ASources[I].Source);
  end;
  lManaged.SetManager(ATarget);
  lManaged.SetFreeNotification(ATarget);
  FBindings.Add(LToAdd);
end;

procedure TStrategy_LiveBindings.BindAction(AAction: IBindableAction);
var
  LCommand: IBinding;
  LObject: TContainedAction;
begin
  Guard.CheckNotNull(AAction, '<BindAction> (Param=AAction) cannot be null');
  LObject := AAction as TContainedAction;
  // if already Exists the binded object we reuse the command
  if not ExistBindingCommandActionFor(LObject, LCommand) then
    LCommand      := TBindingCommandAction.Create(LObject);
  AAction.Binding := LCommand;
  AddBinding(LCommand);
end;

procedure TStrategy_LiveBindings.BindCollection(AServiceType: PTypeInfo; const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
var
  LView: ICollectionView;
begin
  Guard.CheckNotNull(ATarget, '<BindCollection> (Param=ATarget) cannot be null');
  Guard.CheckNotNull(ATarget, '<BindCollection> (Param=ATemplate) cannot be null');

  LView := ATarget.GetCollectionView;
  if (LView = nil) then
    raise EBindError.CreateFmt('Function %s.GetCollectionView cannot return nil', [TObject(ATarget).QualifiedClassName]);

  LView.Template := ATemplate;
  LView.Source   := TCollectionSource(ACollection);

  if not InternalBindCollection(AServiceType, LView.Component, ACollection) then
    raise EBindError.CreateFmt('Component %s has not registered a binder', [TObject(ATarget).QualifiedClassName]);
end;

procedure TStrategy_LiveBindings.BindDataSet(const ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
var
  LView: ICollectionView;
begin
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ADataSet) cannot be null');
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ATarget) cannot be null');
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ATemplate) cannot be null');

  LView := ATarget.GetCollectionView;
  if (LView = nil) then
    raise EBindError.CreateFmt('Function %s.GetCollectionView cannot return nil', [TObject(ATarget).QualifiedClassName]);

  LView.Template := ATemplate;

  if not InternalBindDataSet(ADataSet, LView.Component) then
    raise EBindError.CreateFmt('Component %s has not registered a binder', [TObject(ATarget).QualifiedClassName]);
end;

procedure TStrategy_LiveBindings.BindDataSetAppendFieldToListView(ADataSet: TDataSet; ATarget: TComponent; const ALink: TListViewConversionData; const AOnlyFillValues: Boolean);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I, J   : Integer;
  LFound : Boolean;
  LItem  : TCollectionItem;
  LItem1: TFormatExpressionItem;
begin
  //si hay link lo usamos
  LFiller := nil;
  LLinker := nil;
  LFound := False;
  for I := 0 to ATarget.ComponentCount - 1 do
  begin
    if (ATarget.Components[I] is TBindSourceDB) then
    begin
      for J := 0 to ATarget.Components[I].ComponentCount - 1 do
      begin
        if (ATarget.Components[I].Components[J] is TLinkFillControlToField) then
        begin
          LFiller := ATarget.Components[I].Components[J] as TLinkFillControlToField;
          LFiller.Active := False;
          LFound := True;
          Break;
        end;
        if (ATarget.Components[I].Components[J] is TLinkListControlToField) then
        begin
          LLinker := ATarget.Components[I].Components[J] as TLinkListControlToField;
          LLinker.Active := False;
          LFound := True;
          Break;
        end;
      end;
      if LFound then
        break;
    end;
  end;
  if Assigned(LFiller) then
  begin
    LItem1 := LFiller.FillExpressions.AddExpression;
    LItem1.SourceMemberName  := ALink.DataSetField;
    LItem1.ControlMemberName := ALink.ListViewField;
    if not ALink.CustomFormat.IsEmpty then
      LItem1.CustomFormat := ALink.CustomFormat;
    LFiller.Active := True;
    Exit;
  end;
  if Assigned(LLinker) then
  begin
    LItem1 := LLinker.FillExpressions.AddExpression;
    LItem1.SourceMemberName  := ALink.DataSetField;
    LItem1.ControlMemberName := ALink.ListViewField;
    if not ALink.CustomFormat.IsEmpty then
      LItem1.CustomFormat := ALink.CustomFormat;
    LLinker.Active := True;
  end;
end;

procedure TStrategy_LiveBindings.BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String);
var
  LinkPropertyToField: TLinkPropertyToField;
  LSource: TBindSourceDB;
begin
  LSource                               := TBindSourceDB.Create(nil); //TO-DO
  LSource.DataSet                       := ADataSet;
  LinkPropertyToField                   := TLinkPropertyToField.Create(nil); //TO-DO
  LinkPropertyToField.Category          := 'Quick Bindings';
  LinkPropertyToField.DataSource        := LSource;
  LinkPropertyToField.FieldName         := AFieldName;
  LinkPropertyToField.Component         := ATarget;
  LinkPropertyToField.ComponentProperty := ATargetPropertyPath;
  LinkPropertyToField.Active            := True;
end;

procedure TStrategy_LiveBindings.BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const AValueConverterClass: TValueConverterClass);
begin
  raise Exception.Create('Method not suitable for this kind of binding (LiveBindings)');
end;

procedure TStrategy_LiveBindings.BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath, ACustomFormat: String);
var
  LinkPropertyToField: TLinkPropertyToField;
  LSource: TBindSourceDB;
begin
  LSource                               := TBindSourceDB.Create(nil); //TO-DO
  LSource.DataSet                       := ADataSet;
  LinkPropertyToField                   := TLinkPropertyToField.Create(nil); //TO-DO
  LinkPropertyToField.Category          := 'Quick Bindings';
  LinkPropertyToField.DataSource        := LSource;
  LinkPropertyToField.FieldName         := AFieldName;
  LinkPropertyToField.Component         := ATarget;
  LinkPropertyToField.CustomFormat      := ACustomFormat;
  LinkPropertyToField.ComponentProperty := ATargetPropertyPath;
  LinkPropertyToField.Active            := True;
end;

procedure TStrategy_LiveBindings.BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField, ACustomFormat: String; const AOnlyFillValues: Boolean);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I      : Integer;
begin
  for I := 0 to ATarget.ComponentCount - 1 do
  begin
    if (ATarget.Components[I] is TBindSourceDB) then
    begin
      ATarget.Components[I].Free;
      Break;
    end;
  end;

  LSource          := TBindSourceDB.Create(ATarget);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := ATarget;
    LFiller.Track    := True;

    LFiller.FillDataSource := LSource;
    LFiller.FillDisplayFieldName := AField;
    if not ACustomFormat.IsEmpty then
      LFiller.FillDisplayCustomFormat := ACustomFormat;
    LFiller.AutoFill := True;
    LFiller.Active   := True;
  end
  else begin
         LLinker          := TLinkListControlToField.Create(LSource);
         LLinker.Category := 'Quick Bindings';
         LLinker.Control  := ATarget;

         LLinker.DataSource := LSource;
         LLinker.FieldName  := AField;
         if not ACustomFormat.IsEmpty then
           LLinker.CustomFormat := ACustomFormat;
         LLinker.Active   := True;
       end;
end;

procedure TStrategy_LiveBindings.BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const AOnlyFillValues: Boolean);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I      : Integer;
begin
  for I := 0 to ATarget.ComponentCount - 1 do
  begin
    if (ATarget.Components[I] is TBindSourceDB) then
    begin
      ATarget.Components[I].Free;
      Break;
    end;
  end;

  LSource          := TBindSourceDB.Create(ATarget);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := ATarget;
    LFiller.Track    := True;

    LFiller.FillDataSource := LSource;
    LFiller.FillDisplayFieldName := AField;
    LFiller.AutoFill := True;
    LFiller.Active   := True;
  end
  else begin
         LLinker          := TLinkListControlToField.Create(LSource);
         LLinker.Category := 'Quick Bindings';
         LLinker.Control  := ATarget;

         LLinker.DataSource := LSource;
         LLinker.FieldName  := AField;
         LLinker.Active   := True;
       end;
end;

procedure TStrategy_LiveBindings.BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent; const AColumnLinks: array of TGridColumnTemplate);
var
  LGridLinker: TLinkGridToDataSource;
  LColumnLinker: TLinkGridToDataSourceColumn;
  LSource: TBindSourceDB;
  I, J: Integer;
  LExisteS, LExisteL: Boolean;
begin
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ADataSet) cannot be null');
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ATarget) cannot be null');
  // mirar si existe o no un linkado previo
  LExisteS := False;
  LExisteL := False;
  for I    := 0 to ATarget.ComponentCount - 1 do
  begin
    if (ATarget.Components[I] is TBindSourceDB) then
    begin
      LExisteS := True;
      LSource  := ATarget.Components[I] as TBindSourceDB;
      for J    := 0 to ATarget.Components[I].ComponentCount - 1 do
      begin
        if (ATarget.Components[I].Components[J] is TLinkGridToDataSource) then
        begin
          LGridLinker        := ATarget.Components[I].Components[J] as TLinkGridToDataSource;
          LGridLinker.Active := False;
          LExisteL           := True;
          Break;
        end;
      end;
      if LExisteL then
        Break;
    end;
  end;
  if not LExisteS then
  begin
    LSource         := TBindSourceDB.Create(ATarget);
    LSource.DataSet := ADataSet;
  end;
  if not LExisteL then
  begin
    LGridLinker             := TLinkGridToDataSource.Create(LSource);
    LGridLinker.Category    := 'Quick Bindings';
    LGridLinker.GridControl := ATarget;
    LGridLinker.DataSource  := LSource;
  end;
  for I := Low(AColumnLinks) to High(AColumnLinks) do
  begin
    LColumnLinker              := LGridLinker.Columns.Add;
    LColumnLinker.MemberName   := AColumnLinks[I].DataSetField;
    LColumnLinker.Header       := AColumnLinks[I].HeaderText;
    LColumnLinker.ReadOnly     := AColumnLinks[I].ReadOnly;
    LColumnLinker.Width        := AColumnLinks[I].Width;
    LColumnLinker.Visible      := True;
    LColumnLinker.CustomFormat := AColumnLinks[I].CustomFormat;
    LColumnLinker.CustomParse  := AColumnLinks[I].CustomParse;
    LColumnLinker.ColumnStyle  := AColumnLinks[I].ColumnStyle;
  end;
  LGridLinker.Active := True;
end;

procedure TStrategy_LiveBindings.BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListBoxConversionData; const AOnlyFillValues: Boolean);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I      : Integer;
  LItem  : TCollectionItem;
  LItem1: TFormatExpressionItem;
begin
  ClearDataSetBindingFromComponent(ATarget);

  LSource          := TBindSourceDB.Create(ATarget);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := ATarget;
    LFiller.Track    := True;

    LFiller.FillDataSource := LSource;

    for I := Low(ALinks) to High(ALinks) do
    begin
      LItem1 := LFiller.FillExpressions.AddExpression;
      LItem1.SourceMemberName  := ALinks[I].DataSetField;
      LItem1.ControlMemberName := ALinks[I].ListBoxField;
      if not ALinks[I].CustomFormat.IsEmpty then
        LItem1.CustomFormat := ALinks[I].CustomFormat;
    end;
    LFiller.AutoFill := True;
    LFiller.Active   := True;
  end
  else begin
         LLinker          := TLinkListControlToField.Create(LSource);
         LLinker.Category := 'Quick Bindings';
         LLinker.Control  := ATarget;

         LLinker.DataSource := LSource;
         //LLinker.FieldName  := AField;

         for I := Low(ALinks) to High(ALinks) do
         begin
           LItem1 := LLinker.FillExpressions.AddExpression;
           LItem1.SourceMemberName  := ALinks[I].DataSetField;
           LItem1.ControlMemberName := ALinks[I].ListBoxField;
           if not ALinks[I].CustomFormat.IsEmpty then
             LItem1.CustomFormat := ALinks[I].CustomFormat;
         end;
         LLinker.Active   := True;
       end;
end;

procedure TStrategy_LiveBindings.BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const AField, ACustomDisplayExpression: String; const AOnlyFillValues: Boolean);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I      : Integer;
begin
  ClearDataSetBindingFromComponent(ATarget);
  LSource          := TBindSourceDB.Create(ATarget);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := ATarget;
    LFiller.Track    := True;

    LFiller.FillDataSource := LSource;
    LFiller.FillDisplayFieldName := AField;
    if not ACustomDisplayExpression.IsEmpty then
      LFiller.FillDisplayCustomFormat := ACustomDisplayExpression;
    LFiller.AutoFill := True;
    LFiller.Active   := True;
  end
  else begin
         LLinker          := TLinkListControlToField.Create(LSource);
         LLinker.Category := 'Quick Bindings';
         LLinker.Control  := ATarget;

         LLinker.DataSource := LSource;
         LLinker.FieldName  := AField;
         if not ACustomDisplayExpression.IsEmpty then
           LLinker.CustomFormat := ACustomDisplayExpression;
         LLinker.Active   := True;
       end;
end;

procedure TStrategy_LiveBindings.BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListViewConversionData; const AOnlyFillValues: Boolean);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I      : Integer;
  LItem  : TCollectionItem;
  LItem1: TFormatExpressionItem;
begin
  ClearDataSetBindingFromComponent(ATarget);

  LSource          := TBindSourceDB.Create(ATarget);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := ATarget;
    LFiller.Track    := True;

    LFiller.FillDataSource := LSource;

    for I := Low(ALinks) to High(ALinks) do
    begin
      LItem1 := LFiller.FillExpressions.AddExpression;
      LItem1.SourceMemberName  := ALinks[I].DataSetField;
      LItem1.ControlMemberName := ALinks[I].ListViewField;
      if not ALinks[I].CustomFormat.IsEmpty then
        LItem1.CustomFormat := ALinks[I].CustomFormat;
    end;

    LFiller.AutoFill := True;
    LFiller.Active   := True;
  end
  else begin
         LLinker          := TLinkListControlToField.Create(LSource);
         LLinker.Category := 'Quick Bindings';
         LLinker.DataSource := LSource;

         for I := Low(ALinks) to High(ALinks) do
         begin
           if ALinks[I].IsKeyField then
           begin
             LLinker.FieldName    := ALinks[I].DataSetField;
             LLinker.CustomFormat := ALinks[I].CustomFormat;
             Break;
           end
         end;
         LLinker.Control := ATarget;
         for I := Low(ALinks) to High(ALinks) do
         begin
           if not ALinks[I].IsKeyField then
           begin
             LItem1 := LLinker.FillExpressions.AddExpression;
             LItem1.SourceMemberName  := ALinks[I].DataSetField;
             LItem1.ControlMemberName := ALinks[I].ListViewField;
             if not ALinks[I].CustomFormat.IsEmpty then
               LItem1.CustomFormat := ALinks[I].CustomFormat;
          end;
         end;

         LLinker.Active   := True;
       end;
end;

procedure TStrategy_LiveBindings.BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const AField, ACustomDisplayExpression: string; const AOnlyFillValues: Boolean);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
begin
  ClearDataSetBindingFromComponent(ATarget);

  LSource          := TBindSourceDB.Create(ATarget);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := ATarget;
    LFiller.Track    := True;

    LFiller.FillDataSource := LSource;
    LFiller.FillDisplayFieldName := AField;
    if not ACustomDisplayExpression.IsEmpty then
      LFiller.FillDisplayCustomFormat := ACustomDisplayExpression;
    LFiller.AutoFill := True;
    LFiller.Active   := True;
  end
  else begin
         LLinker          := TLinkListControlToField.Create(LSource);
         LLinker.Category := 'Quick Bindings';
         LLinker.Control  := ATarget;

         LLinker.DataSource := LSource;
         LLinker.FieldName  := AField;
         if not ACustomDisplayExpression.IsEmpty then
           LLinker.CustomFormat := ACustomDisplayExpression;
         LLinker.Active   := True;
       end;
end;

procedure TStrategy_LiveBindings.BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent);
var
  LLinker: TLinkGridToDataSource;
  LSource: TBindSourceDB;
begin
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ADataSet) cannot be null');
  Guard.CheckNotNull(ATarget, '<BindDataSet> (Param=ATarget) cannot be null');

  ClearDataSetBindingFromComponent(ATarget);

  LSource             := TBindSourceDB.Create(ATarget);
  LSource.DataSet     := ADataSet;
  LLinker             := TLinkGridToDataSource.Create(LSource);
  LLinker.Category    := 'Quick Bindings';
  LLinker.GridControl := ATarget;

  LLinker.DataSource := LSource;
  LLinker.Active     := True;
end;

function TStrategy_LiveBindings.ClearDataSetBindingFromComponent(ATarget: TComponent): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I  := 0 to ATarget.ComponentCount - 1 do
  begin
    if (ATarget.Components[I] is TBindSourceDB) then
    begin
      ATarget.Components[I].Free;
      Exit(True);
    end;
  end;
end;

constructor TStrategy_LiveBindings.Create;
begin
  inherited;
end;

class constructor TStrategy_LiveBindings.CreateC;
begin
  FObjectListLinkers    := TCollections.CreateDictionary<TClass, TProc<PTypeInfo, TComponent, TEnumerable<TObject>>>;
  FObjectDataSetLinkers := TCollections.CreateDictionary<TClass, TProc<TDataSet, TComponent>>;
end;

destructor TStrategy_LiveBindings.Destroy;
begin
  ClearBindings;
  inherited;
end;

class destructor TStrategy_LiveBindings.DestroyC;
begin
  FObjectListLinkers    := nil;
  FObjectDataSetLinkers := nil;
end;

procedure TStrategy_LiveBindings.DoDisableAll;
var
  I: IBinding; // TBindingBase
begin
  AdquireWrite;
  try
    for I in FBindings do
    begin
      I.Enabled := False;
    end;
  finally
    ReleaseWrite;
  end;
end;

procedure TStrategy_LiveBindings.DoEnableAll;
var
  I: IBinding; // TBindingBase
begin
  AdquireWrite;
  try
    for I in FBindings do
    begin
      I.Enabled := True;
    end;
  finally
    ReleaseWrite;
  end;
end;

function TStrategy_LiveBindings.ExistBindingCommandActionFor(AObject: TContainedAction; out ACommand: IBinding): Boolean;
var
  I: IBinding;
  LData: TBindingCommandAction;
begin
  Result := False;
  AdquireRead;
  try
    for I in FBindings do
    begin
      LData := I as TBindingCommandAction;
      if LData.Command = AObject then
        Exit(True);
    end;
  finally
    ReleaseRead;
  end;

end;

function TStrategy_LiveBindings.GetEnabled: Boolean;
begin
  Result := FEnabled
end;

// function TStrategy_LiveBindings.GetPlatformBindActionCommandType: TBindingCommandClass;
// begin
//
// end;

function TStrategy_LiveBindings.InternalBindCollection(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>): Boolean;
var
  LClass: TClass;
begin
  Result := False;
  for LClass in FObjectListLinkers.Keys do
  begin
    if AComponent.ClassType.InheritsFrom(LClass) then
    begin
      FObjectListLinkers[LClass](AServiceType, AComponent, ACollection);
      Exit(True);
    end;
  end;
end;

function TStrategy_LiveBindings.InternalBindDataSet(ADataSet: TDataSet; AComponent: TComponent): Boolean;
var
  // LProc: TProc<TDataSet, TComponent>;
  LClass: TClass;
begin
  Result := False;
  for LClass in FObjectDataSetLinkers.Keys do
  begin
    if AComponent.ClassType.InheritsFrom(LClass) then
    begin
      FObjectDataSetLinkers[LClass](ADataSet, AComponent);
      Exit(True);
    end;
  end;
end;

procedure TStrategy_LiveBindings.Notify(const AObject: TObject; const APropertyName: string);
begin
  TBindings.Notify(AObject, APropertyName);
end;

class procedure TStrategy_LiveBindings.RegisterClassDataSetCollectionBinder(const AClass: TClass; AProcedure: TProc<TDataSet, TComponent>);
begin
  FObjectDataSetLinkers[AClass] := AProcedure;
end;

class procedure TStrategy_LiveBindings.RegisterClassObjectListCollectionBinder(const AClass: TClass; AProcedure: TProc < PTypeInfo, TComponent, TEnumerable < TObject >> );
begin
  FObjectListLinkers[AClass] := AProcedure;
end;

procedure TStrategy_LiveBindings.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    case FEnabled of
      True:
        begin
          DoEnableAll;
        end;
      False:
        begin
          DoDisableAll;
        end;
    end;
  end;
end;

procedure TStrategy_LiveBindings.Unbind(const ASource: TObject);
var
  LBindingsToRemove: IEnumerable<IBinding>;
  // LBinding: IBinding;
begin
  AdquireWrite;
  try
    LBindingsToRemove := FBindings.Where(
      function(const ABinding: IBinding): Boolean // TBindingBase
      begin
        Result := ABinding.IsObjectInBinding(ASource)
      end);
    FBindings.RemoveRange(LBindingsToRemove.ToArray);
    // FBindings.RemoveAll(function (const ABinding: TBindingBase): Boolean
    // begin
    // Result := ABinding.IsObjectInBinding(ASource)
    // end
    // );
  finally
    ReleaseWrite
  end;
  // for LBinding in LBindingsToRemove do
  // LBinding.Free;
end;

{ TStrategy_LiveBindings.TInternalBindindExpression }

constructor TStrategy_LiveBindings.TInternalBindindExpression.Create(ABindingStrategy: IBindingStrategy; const InputScopes: array of IScope; const BindExprStr: string; const OutputScopes: array of IScope; const OutputExpr: string; const OutputConverter: IValueRefConverter; Manager: TBindingManager; Options: TBindings.TCreateOptions);
begin
  inherited Create(ABindingStrategy);
  FExpression                      := TBindings.CreateManagedBinding(InputScopes, BindExprStr, OutputScopes, OutputExpr, nil, nil, Options);
  FExpression.OnAssignedValueEvent := OnBindingAssignedValueEvent;
end;

destructor TStrategy_LiveBindings.TInternalBindindExpression.Destroy;
begin
  //Utils.IdeDebugMsg('<TStrategy_LiveBindings.TInternalBindindExpression.Destroy> ID: ' + ID);
  TBindings.RemoveBinding(FExpression);
  // FExpression.Free;
  inherited;
end;

function TStrategy_LiveBindings.TInternalBindindExpression.IsObjectInBinding(const AObject: TObject): Boolean;
begin
  Result := FExpression.Associations.ContainsKey(AObject);
end;

procedure TStrategy_LiveBindings.TInternalBindindExpression.OnBindingAssignedValueEvent(AssignValueRec: TBindingAssignValueRec; const AValue: TValue);
begin
  DoOnBindingAssignedValue(AssignValueRec.OutObj, AssignValueRec.OutProp, AValue);
end;

initialization

TBindingManager.RegisterBindingStrategy(STRATEGY_NAME, TStrategy_LiveBindings);

end.
