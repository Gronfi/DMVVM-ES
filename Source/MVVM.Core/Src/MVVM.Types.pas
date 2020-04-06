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
  //Bindings
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

  TBindingValueConverter = class abstract
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

  RSourcePair = record
  public
    Source: TObject;
    Alias: String;
  end;

  TSourcePairArray = TArray<RSourcePair>;

  TBindingValueConverterClass = class of TBindingValueConverter;

  TBindExtraParams = TArray<TPair<String, String>>;

  { Acciones Bindables } // DAVID: no me gusta como queda por las interfaces

  { The type of method to invoke when an IBindableAction is executed. }
  TExecuteRttiMethod = TRttiMethod;
  TExecuteAnonymous = TProc;
  TExecuteMethod = procedure of object;
  // TExecuteMethod = procedure of Object;

  { The type of method to invoke to check whether an IBindableAction can be
    executed. The Enabled property of the action will be set to the result of
    this function. }
  TCanExecuteMethod = function: boolean of object;
  // TCanExecuteMethod = function:Boolean of object;

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

implementation

{ TBindingValueConverter }

class function TBindingValueConverter.ConvertTargetToSource(const ATarget: TValue): TValue;
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

end.
