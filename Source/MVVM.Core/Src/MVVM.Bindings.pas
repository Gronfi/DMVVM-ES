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

    // Bindings
    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String; const ATarget: TObject; const ATargetPropertyPath: String; const ADirection: EBindDirection = EBindDirection.OneWay; const AFlags: EBindFlags = []; const AValueConverterClass: TValueConverterClass = nil; const ABindingStrategy: String = '';
      const AExtraParams: TBindExtraParams = []); overload;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String; const AFlags: EBindFlags = []; const ABindingStrategy: String = ''; const AExtraParams: TBindExtraParams = []); overload;
    // Collections
    procedure BindCollection<T: Class>(const ACollection: TEnumerable<T>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String = '');
    // DataSets
    procedure BindDataSet(ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass = nil; const ABindingStrategy: String = '');

    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const ABindingStrategy: String = ''); overload;
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const AValueConverterClass: TValueConverterClass; const ABindingStrategy: String = ''); overload;
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const ACustomFormat: String; const ABindingStrategy: String); overload;

    // -- ComboBox
    procedure BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const AOnlyFillValues: Boolean = True; const ABindingStrategy: String = ''); overload;
// ACustomFormat: %s + ' - ' + DataSet.Category.Text
//        where ---> %s : the default text of the selected field (AField) in the dataset
//                   DataSet : references to the dataset
//                   DataSet.Category : references to the value of that fieldname in the dataset in the selected row
// AOnlyFillValues --> True: the combobox is only filled with values
//                     False: the combobox is synchronized with the dataset, if you select a row in the combobox the dataset is moved to the selected row
    procedure BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomFormat: String; const AOnlyFillValues: Boolean = True; const ABindingStrategy: String = ''); overload;
    // -- ListBox
    // '%s + '' - '' + DataSet.Category.Text'
    procedure BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomDisplayExpression: string; const AOnlyFillValues: Boolean = True; const ABindingStrategy: String = ''); overload;
//    [
//      TListBoxConversionData.Create('Common_Name', 'Text'),
//      TListBoxConversionData.Create('Category', TBinder.StyledFieldOfComponent('resolution')),
//      TListBoxConversionData.Create('Species No', TBinder.StyledFieldOfComponent('depth')),
//      TListBoxConversionData.Create('Graphic', 'Bitmap')
//    ]
    procedure BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListBoxConversionData; const AOnlyFillValues: Boolean = True; const ABindingStrategy: String = ''); overload;
    // -- ListView
    procedure BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomDisplayExpression: String = ''; const AOnlyFillValues: Boolean = True; const ABindingStrategy: String = ''); overload;
    procedure BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListViewConversionData; const AOnlyFillValues: Boolean = True; const ABindingStrategy: String = ''); overload;
    procedure BindDataSetAppendFieldToListView(ADataSet: TDataSet; ATarget: TComponent; const ALink: TListViewConversionData; const AOnlyFillValues: Boolean = True; const ABindingStrategy: String = ''); overload;
    // -- Grid
    procedure BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent; const ABindingStrategy: String = ''); overload; // basic link
    procedure BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent; const AColumnLinks: array of TGridColumnTemplate; const ABindingStrategy: String = ''); overload;
    // Actions
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

procedure TBindingManager.Bind(const ASources: TSourcePairArray; const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String; const AFlags: EBindFlags; const ABindingStrategy: String; const AExtraParams: TBindExtraParams);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.Bind(ASources, ASourceExpresion, ATarget, ATargetAlias, ATargetPropertyPath, AFlags, AExtraParams);
end;

procedure TBindingManager.Bind(const ASource: TObject; const ASourcePropertyPath: String; const ATarget: TObject; const ATargetPropertyPath: String; const ADirection: EBindDirection; const AFlags: EBindFlags; const AValueConverterClass: TValueConverterClass; const ABindingStrategy: String; const AExtraParams: TBindExtraParams);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.Bind(ASource, ASourcePropertyPath, ATarget, ATargetPropertyPath, ADirection, AFlags, AValueConverterClass, AExtraParams);
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

procedure TBindingManager.BindDataSet(ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSet(ADataSet, ATarget, ATemplate);
end;

procedure TBindingManager.BindDataSetAppendFieldToListView(ADataSet: TDataSet; ATarget: TComponent; const ALink: TListViewConversionData; const AOnlyFillValues: Boolean; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetAppendFieldToListView(ADataSet, ATarget, ALink, AOnlyFillValues);
end;

procedure TBindingManager.BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath, ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetFieldToProperty(ADataSet, AFieldName, ATarget, ATargetPropertyPath);
end;

procedure TBindingManager.BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const AValueConverterClass: TValueConverterClass; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetFieldToProperty(ADataSet, AFieldName, ATarget, ATargetPropertyPath, AValueConverterClass);
end;

procedure TBindingManager.BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath, ACustomFormat, ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetFieldToProperty(ADataSet, AFieldName, ATarget, ATargetPropertyPath, ACustomFormat);
end;

procedure TBindingManager.BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomFormat: String; const AOnlyFillValues: Boolean; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetToCombobox(ADataSet, ATarget, AField, ACustomFormat, AOnlyFillValues);
end;

procedure TBindingManager.BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const AOnlyFillValues: Boolean; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetToCombobox(ADataSet, ATarget, AField, AOnlyFillValues);
end;

procedure TBindingManager.BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent; const AColumnLinks: array of TGridColumnTemplate; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetToGrid(ADataSet, ATarget, AColumnLinks);
end;

procedure TBindingManager.BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListBoxConversionData; const AOnlyFillValues: Boolean; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetToListBox(ADataSet, ATarget, ALinks, AOnlyFillValues);
end;

procedure TBindingManager.BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const AField, ACustomDisplayExpression: String; const AOnlyFillValues: Boolean; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetToListView(ADataSet, ATarget, AField, ACustomDisplayExpression, AOnlyFillValues);
end;

procedure TBindingManager.BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListViewConversionData; const AOnlyFillValues: Boolean; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetToListView(ADataSet, ATarget, ALinks, AOnlyFillValues);
end;

procedure TBindingManager.BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const AField, ACustomDisplayExpression: String; const AOnlyFillValues: Boolean; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetToListBox(ADataSet, ATarget, AField, ACustomDisplayExpression, AOnlyFillValues);
end;

procedure TBindingManager.BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent; const ABindingStrategy: String);
var
  LEstrategia: IBindingStrategy;
begin
  LEstrategia := GetSelectedBindingOrDefault(ABindingStrategy);
  LEstrategia.BindDataSetToGrid(ADataSet, ATarget);
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
    Result                         := FDictionaryBindingStrategies[LMetodo].Create;
    FDictionaryStrategies[LMetodo] := Result;
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
  LProc : TProc;
begin
  LProc := procedure
           var
             LEstrategia: String;
           begin
             for LEstrategia in FDictionaryStrategies.Keys do
               FDictionaryStrategies[LEstrategia].Notify(AObject, APropertiesNames);
           end;
  if MVVMCore.PlatformServices.IsMainThreadUI then
  begin
    LProc();
  end
  else begin
         MVVMCore.DelegateExecution(LProc, EDelegatedExecutionMode.medQueue);
       end;
end;

procedure TBindingManager.Notify(const AObject: TObject; const APropertyName: String);
var
  LProc : TProc;
begin
  LProc := procedure
           var
             LEstrategia: String;
           begin
             for LEstrategia in FDictionaryStrategies.Keys do
               FDictionaryStrategies[LEstrategia].Notify(AObject, APropertyName);
           end;
  if MVVMCore.PlatformServices.IsMainThreadUI then
  begin
    LProc();
  end
  else begin
         MVVMCore.DelegateExecution(LProc, EDelegatedExecutionMode.medQueue);
       end;
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
  FDictionaryBindingStrategies[ABindingStrategy] := ABindingStrategyClass;
end;

end.
