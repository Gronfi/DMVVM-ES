unit MVVM.Types;

interface

uses
  System.RTTI,
  System.Generics.Collections;

type
  EEstrategiasBasicas = (_RTTI,_LIVEBINDINGS);

const
  CEstrategiasBasicas : array[EEstrategiasBasicas] of String = ('RTTI', 'LIVEBINDINGS');

type
  EBindDirection = (
    { Data flows in one direction, from the source (eg a view model) to a target
      (eg a control). }
    OneWay,

    { Data flows two ways. A change to the source results in a change to the
      target and vice versa. }
    TwoWay);

  EBindFlag = (
    { If the text used for properties is an expression to evaluate, and not directly an expression}
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
      Alias : String;
  end;

  TSourcePairArray = TArray<RSourcePair>;

  TBindingValueConverterClass = class of TBindingValueConverter;

  TBindExtraParams = TArray<TPair<String, String>>;

  { Acciones Bindables }
  { The type of method to invoke when an IBindableAction is executed. }
  TExecuteMethod = procedure of Object;
  TExecuteMethod<T> = procedure(const AArg: T) of Object;

  { The type of method to invoke to check whether an IBindableAction can be
    executed. The Enabled property of the action will be set to the result of
    this function. }
  TCanExecuteMethod = function: Boolean of object;

implementation

{ TBindingValueConverter }

class function TBindingValueConverter.ConvertTargetToSource(const ATarget: TValue): TValue;
begin
  Result := ATarget;
end;

end.
