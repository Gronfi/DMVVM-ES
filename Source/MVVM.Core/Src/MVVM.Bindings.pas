unit MVVM.Bindings;

interface

uses
  Generics.Collections, System.Bindings.Expression, System.Bindings.Helper,
  System.RTTI,
  Spring,
  Spring.Collections;

type

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

implementation

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

end.
