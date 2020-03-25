unit MVVM.Bindings;

interface

uses
  System.Classes,
  Generics.Collections,
  System.RTTI,
  Data.DB,

  Spring,
  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types;

type
{$REGION 'TBindingManager'}
  TBindingManager = class
  private
    class var FDictionaryBindingStrategies: IDictionary<String, TClass_BindingStrategyBase>;
  private
  var
    FObject: TObject;
    FDictionaryStrategies: IDictionary<String, IBindingStrategy>;
  protected
    class constructor CreateC;
    class destructor DestroyC;

    function GetSelectedBindingOrDefault(const ABindingStrategy: String = ''): IBindingStrategy;
  public
    constructor Create(AObject: TObject); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String; const ATarget: TObject; const ATargetPropertyPath: String; const ADirection: EBindDirection = EBindDirection.OneWay; const AFlags: EBindFlags = []; const AValueConverterClass: TBindingValueConverterClass = nil; const ABindingStrategy: String = '';
      const AExtraParams: TBindExtraParams = []); overload;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String; const AFlags: EBindFlags = []; const ABindingStrategy: String = ''; const AExtraParams: TBindExtraParams = []); overload;
    procedure BindCollection<T: Class>(const ACollection: TEnumerable<T>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String = '');
    procedure BindDataSet(const ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass = nil; const ABindingStrategy: String = '');

    procedure BindAction(AAction: IBindableAction; const ABindingStrategy: String = ''); overload;

    procedure Notify(const AObject: TObject; const APropertyName: String); overload; virtual;
    procedure Notify(const AObject: TObject; const APropertiesNames: TArray<String>); overload; virtual;

    class procedure RegisterBindingStrategy(const ABindingStrategy: String; ABindingStrategyClass: TClass_BindingStrategyBase); static;
    class function GetRegisteredBindingStrategiesClasses: IReadOnlyDictionary<String, TClass_BindingStrategyBase>; static;
    class function GetDefaultRegisteredBindingStrategy: IBindingStrategy; static;
  end;
{$ENDREGION}

implementation

uses
  System.SysUtils,

  MVVM.Utils,
  MVVM.Core;

{ TBindingManager }

procedure TBindingManager.Bind(const ASource: TObject; const ASourcePropertyPath: String; const ATarget: TObject; const ATargetPropertyPath: String; const ADirection: EBindDirection; const AFlags: EBindFlags; const AValueConverterClass: TBindingValueConverterClass; const ABindingStrategy: String; const AExtraParams: TBindExtraParams);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.Bind(ASource, ASourcePropertyPath, ATarget, ATargetPropertyPath, ADirection, AFlags, AValueConverterClass, AExtraParams);
end;

procedure TBindingManager.Bind(const ASources: TSourcePairArray; const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String; const AFlags: EBindFlags; const ABindingStrategy: String; const AExtraParams: TBindExtraParams);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.Bind(ASources, ASourceExpresion, ATarget, ATargetAlias, ATargetPropertyPath, AFlags, AExtraParams);
end;

procedure TBindingManager.BindAction(AAction: IBindableAction; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindAction(AAction);
end;

procedure TBindingManager.BindCollection<T>(const ACollection: TEnumerable<T>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindCollection(TypeInfo(T), TEnumerable<TObject>(ACollection), ATarget, ATemplate);
end;

procedure TBindingManager.BindDataSet(const ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSet(ADataSet, ATarget, ATemplate);
end;

function TBindingManager.GetSelectedBindingOrDefault(const ABindingStrategy: String): IBindingStrategy;
var
  LMetodo: String;
begin
  if ABindingStrategy.IsEmpty then
    LMetodo := MVVMCore.DefaultBindingStrategyName
  else
    LMetodo := ABindingStrategy;
  Guard.CheckTrue(FDictionaryBindingStrategies.ContainsKey(LMetodo), 'Binding Strategy not registered: ' + LMetodo);
  if not FDictionaryStrategies.TryGetValue(LMetodo, Result) then
  begin
    Result := FDictionaryBindingStrategies[LMetodo].Create;
    FDictionaryStrategies.AddOrSetValue(LMetodo, Result);
  end;
end;

constructor TBindingManager.Create(AObject: TObject);
begin
  Create;
  FObject := AObject;
end;

constructor TBindingManager.Create;
begin
  inherited Create;
  FDictionaryStrategies := TCollections.CreateDictionary<String, IBindingStrategy>;
end;

class constructor TBindingManager.CreateC;
begin
  FDictionaryBindingStrategies := TCollections.CreateDictionary<String, TClass_BindingStrategyBase>;
end;

destructor TBindingManager.Destroy;
begin
  FDictionaryStrategies := nil;
  inherited;
end;

class destructor TBindingManager.DestroyC;
begin
  FDictionaryBindingStrategies := nil;
end;

procedure TBindingManager.Notify(const AObject: TObject; const APropertiesNames: TArray<String>);
var
  LEstrategia: String;
begin
  for LEstrategia in FDictionaryStrategies.Keys do
    FDictionaryStrategies[LEstrategia].Notify(AObject, APropertiesNames);
end;

procedure TBindingManager.Notify(const AObject: TObject; const APropertyName: String);
var
  LEstrategia: String;
begin
  for LEstrategia in FDictionaryStrategies.Keys do
    FDictionaryStrategies[LEstrategia].Notify(AObject, APropertyName);
end;

class function TBindingManager.GetDefaultRegisteredBindingStrategy: IBindingStrategy;
var
  LStrategy: String;
begin
  LStrategy := MVVMCore.DefaultBindingStrategyName;
  if LStrategy.IsEmpty then
    raise Exception.Create('Default binding strategy <name> not assigned');
  Guard.CheckTrue(FDictionaryBindingStrategies.ContainsKey(LStrategy), 'Binding Strategy not registered: ' + LStrategy);
  Result := FDictionaryBindingStrategies[LStrategy].Create;
end;

class function TBindingManager.GetRegisteredBindingStrategiesClasses: IReadOnlyDictionary<String, TClass_BindingStrategyBase>;
begin
  Result := FDictionaryBindingStrategies.AsReadOnly;
end;

class procedure TBindingManager.RegisterBindingStrategy(const ABindingStrategy: String; ABindingStrategyClass: TClass_BindingStrategyBase);
begin
  FDictionaryBindingStrategies.AddOrSetValue(ABindingStrategy, ABindingStrategyClass);
end;

end.
