unit MVVM.Types;

interface

uses
  System.SysUtils,
  System.RTTI,
  System.Generics.Collections;

// type
// EBasicStrategies = (_RTTI,_LIVEBINDINGS);
//
// const
// CBasicStrategies : array[EBasicStrategies] of String = ('RTTI', 'LIVEBINDINGS');

type
  // Bindings
  EBindDirection = (
    { Data flows in one direction, from the source (eg a view model) to a target
      (eg a control). }
    OneWay,

    { Data flows two ways. A change to the source results in a change to the
      target and vice versa. }
    TwoWay);

  EBindFlag = (
    { If the text used for properties is an expression to evaluate, and not directly an expression }
    UsesExpressions,

    { If you want to get notified while a property of the source object
      is changing (for example, for each character added to a TEdit).
      The source object must implement the IgoNotifyPropertyChangeTracking
      interface to support this. }
    SourceTracking,

    { If you want to get notified while a property of the target object
      is changing (for example, for each character added to a TEdit).
      The target object must implement the IgoNotifyPropertyChangeTracking
      interface to support this. }
    TargetTracking,

    { Normally, when creating a binding, the initial value of the source
      property will be applied to given target property. If you don't want this,
      then you can specify this flag.
      A reason you want to do this is if you have a one-way binding from the
      View to the ViewModel, but you don't want to update the View Model with
      the current value in the View. }
    DontApply);

  EBindFlags = set of EBindFlag;

  RSourcePair = record
  public
    Source: TObject;
    Alias: String;
  end;

  TSourcePairArray = TArray<RSourcePair>;

  TBindExtraParams = TArray<TPair<String, String>>;

// Commands-Actions
  { The type of method to invoke when an IBindableAction is executed. }
  TExecuteRttiMethod = TRttiMethod;
  TExecuteAnonymous = TProc;
  TExecuteMethod = procedure of object;
  TExecuteMethod<T> = procedure(const AArg: T) of object;
  TCanExecuteRttiMethod = TRttiMethod;

  TParam<T> = function: T of Object;
  TParamRtti = TRttiMethod;

  { The type of method to invoke to check whether an IBindableAction can be
    executed. The Enabled property of the action will be set to the result of
    this function. }
  TCanExecuteMethod = function: boolean of object;

  //Action members
  EActionMemberType = (OnExecute, OnUpdate, OnAsyncExecutionFinished, OnParams);

  ExceptionActionMemberTypeDuplicated = class(Exception);
  ExceptionActionMemberTypeError = class(Exception);
  ExceptionActionMemberNotFound = class(Exception);
  ExceptionActionMemberNameCannotBeEmpty = class(Exception);

  RActionMember = record
    public
      Name: String;
      Caption: String;
      MemberType: EActionMemberType;
      Method: TRttiMethod;
      Owner: TObject;
  end;

  RCommand = record
    public
      ExecuteName: String;
      CanExecuteName: String;
      ParamsName: String;
      Field: TRttiField;
      Owner: TObject;
  end;

  TCollectionChangedAction = (
    { An item was added or inserted into the collection.
      The following properties of the associated TgoCollectionChangedEventArgs
      are valid: Item, ItemIndex. }
    Add,

    { An item was removed from the collection.
      The following properties of the associated TgoCollectionChangedEventArgs
      are valid: ItemIndex. }
    Delete,

    { The property of an item in the collection was modified.
      This action is only fired if the items in the collection implement the
      IgoPropertyChangedEvent interface.
      The following properties of the associated TgoCollectionChangedEventArgs
      are valid: Item, PropertyName. }
    ItemChange,

    { The collection was cleared.
      None of the properties of the associated TgoCollectionChangedEventArgs
      are valid. }
    Clear,

    { The items in the collection were rearranged (for example, sorted).
      None of the properties of the associated TgoCollectionChangedEventArgs
      are valid. }
    Rearrange);

  { The arguments passed to an ICollectionChangedEvent. }
  TCollectionChangedEventArgs = record
{$REGION 'Internal Declarations'}
  private
    FAction: TCollectionChangedAction;
    FItem: TObject;
    FItemIndex: Integer;
    FPropertyName: String;
{$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AAction: TCollectionChangedAction; const AItem: TObject; const AItemIndex: Integer; const APropertyName: String);

    { The action that caused the event. The availability of the other properties
      depends on this action. }
    property Action: TCollectionChangedAction read FAction;

    { The item in the collection to which the action applies.
      Is only valid for the Add and ItemChange actions.
      Will be nil for all other actions. }
    property Item: TObject read FItem;

    { The index of the item in the collection to which the action applies.
      Is only valid for the Add and Delete actions.
      Will be -1 for all other actions. }
    property ItemIndex: Integer read FItemIndex;

    { The name of the property of Item whose value has changed.
      Is only valid for the ItemChange action.
      Will be an empty string for all other actions. }
    property PropertyName: String read FPropertyName;
  end;

  EMessageTypeRestriction = (mtrAllowDescendants, mtrDefinedTypeOnly);

  EDelegatedExecutionMode = (medQueue, medSynchronize, medNewTask, medNormal);

  EBindError = class(Exception);

{$REGION 'TValueConverter'}

  TValueConverter = class abstract
  public
    { Converts a property value from a data binding source to a property value
      of for the data binding target.

      This method gets called for OneWay data bindings from a source to a
      target.

      Parameters:
      ASource: the property value of the data binding source.

      Returns:
      ASource converted for the target binding. }
    class function ConvertSourceToTarget(const ASource: TValue): TValue; virtual; abstract;

    { Converts a property value from a data binding target to a property value
      for the data binding source.

      This method gets called for TwoWay data bindings when the data flows in
      the opposite direction (from target to source).

      Parameters:
      ATarget: the property value of the data binding target.

      Returns:
      ATarget converted for the source binding.

      This method is optional. It returns ATarget by default. }
    class function ConvertTargetToSource(const ATarget: TValue): TValue; virtual;
  end;

  TValueConverterClass = class of TValueConverter;
{$ENDREGION}
{$REGION 'TDataTemplate'}

  { Abstract base template class that defines the mapping between properties of
    each item in a collection and the corresponding item in the view.
    For example, if the collection contains objects of type TCustomer, than you
    can create a mapping between the customer name and the item title in the
    view (by overriding the GetTitle method).
    If the view is a TListBox for example, then the item title will
    be assigned to the TListBoxItem.Text property.

    You can pass the template to the TgoDataBinder.BindCollection method. }
  TDataTemplate = class abstract
  public
    { Must be overridden to return the title of a given object.
      This title will be used to fill the Text property of items in a TListBox
      or TListView.

      Parameters:
      AItem: the object whose title to get. You need to typecast it to the
      type of the objects in the collection (as passed to
      TgoDataBinder.BindCollection).

      Returns:
      The title for this object. Should not be an empty string. }
    class function GetTitle(const AItem: TObject): String; virtual; abstract;

    { Returns some details of a given object.
      These details will be used to fill the Details property of items in a
      TListBox or TListView.

      Parameters:
      AItem: the object whose details to get. You need to typecast it to the
      type of the objects in the collection (as passed to
      TgoDataBinder.BindCollection).

      Returns:
      The details for this object.

      Returns an empty string by default. }
    class function GetDetail(const AItem: TObject): String; virtual;

    { Returns the index of an image that represents a given object.
      This index will be used to fill the ImageIndex property of items in a
      TListBox or TListView.

      Parameters:
      AItem: the object whose image index to get. You need to typecast it to
      the type of the objects in the collection (as passed to
      TgoDataBinder.BindCollection).

      Returns:
      The image index for this object, or -1 if there is no image associated
      with the object.

      Returns -1 by default. }
    class function GetID(const AItem: TObject): Integer; virtual; abstract;
    class function GetImageIndex(const AItem: TObject): Integer; virtual;
    class function GetStyle(const AItem: TObject): string; virtual;
    class function GetParent(const AItem: TObject): TObject; virtual; abstract;
    class function GetChildren(const AItem: TObject): TList<TObject>; virtual; abstract;
  end;

  TDataTemplateClass = class of TDataTemplate;
{$ENDREGION}
{$REGION 'TListBoxConversionData'}
  TListBoxConversionData = record
  public
    DataSetField: String;
    ListBoxField: String;
    CustomFormat: String;

    constructor Create(const ADataSetField: String; const AListBoxField: String; const ACustomFormat: String = '');
  end;
{$ENDREGION}
{$REGION 'TListViewConversionData'}
  TListViewConversionData = record
  public
    DataSetField : String;
    ListViewField: String;
    CustomFormat : String;
    IsKeyField   : Boolean;

    constructor Create(const ADataSetField: String; const AListViewField: String; const ACustomFormat: String = ''; const AIsKeyField: Boolean = False);
  end;
{$ENDREGION}
{$REGION 'TGridColumnTemplate'}
  TGridColumnTemplate = record
  public
    DataSetField: String;
    HeaderText: String;
    ReadOnly: boolean;
    Width: Integer;
    CustomFormat: String;
    CustomParse: String;
    ColumnStyle: String; // Posible values: ProgressColumn, CheckColumn, TimeColumn, DateColumn, PopupColumn, ImageColumn, CurrencyColumn, FloatColumn, IntegerColumn, GlyphColumn

    constructor Create(const ADataSetField: String; const AHeaderText: String; const AReadOnly: boolean; const AWidth: Integer; const ACustomFormat: String; const ACustomParse: String; const AColumnStyle: String);
  end;
{$ENDREGION}

implementation

{ TValueConverter }

class function TValueConverter.ConvertTargetToSource(const ATarget: TValue): TValue;
begin
  Result := ATarget;
end;

{ TCollectionChangedEventArgs }

constructor TCollectionChangedEventArgs.Create(const AAction: TCollectionChangedAction; const AItem: TObject; const AItemIndex: Integer; const APropertyName: String);
begin
  FAction       := AAction;
  FItem         := AItem;
  FItemIndex    := AItemIndex;
  FPropertyName := APropertyName;
end;

{ TDataTemplate }

class function TDataTemplate.GetDetail(const AItem: TObject): String;
begin
  Result := '';
end;

class function TDataTemplate.GetImageIndex(const AItem: TObject): Integer;
begin
  Result := -1;
end;

class function TDataTemplate.GetStyle(const AItem: TObject): string;
begin
  Result := '';
end;

{ TGridColumnTemplate }

constructor TGridColumnTemplate.Create(const ADataSetField, AHeaderText: String; const AReadOnly: boolean; const AWidth: Integer; const ACustomFormat, ACustomParse, AColumnStyle: String);
begin
  DataSetField := ADataSetField;
  HeaderText   := AHeaderText;
  ReadOnly     := AReadOnly;
  Width        := AWidth;
  CustomFormat := ACustomFormat;
  CustomParse  := ACustomParse;
  ColumnStyle  := AColumnStyle;
end;

{ TListBoxConversionData }
constructor TListBoxConversionData.Create(const ADataSetField, AListBoxField, ACustomFormat: String);
begin
  DataSetField := ADataSetField;
  ListBoxField := AListBoxField;
  CustomFormat := ACustomFormat;
end;

{ TListViewConversionData }

constructor TListViewConversionData.Create(const ADataSetField, AListViewField, ACustomFormat: String; const AIsKeyField: Boolean);
begin
  DataSetField  := ADataSetField;
  ListViewField := AListViewField;
  CustomFormat  := ACustomFormat;
  IsKeyField    := AIsKeyField;
end;

end.
