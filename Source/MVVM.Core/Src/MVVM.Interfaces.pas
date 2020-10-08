unit MVVM.Interfaces;

interface

uses
  System.RTTI,
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.UITypes,
  Data.DB,

  Spring,
  Spring.Collections,

  MVVM.Types;

type
  // Platform Services Interfaces
{$REGION 'PLATFORM'}
{$REGION 'IPlatformServices'}
  IPlatformServices = interface
    ['{95F9A402-2D01-48E5-A38B-9A6202FF5F59}']
    function CreatePlatformEmptyForm: TComponent;
    procedure AssignParent(AChild, AParent: TComponent);
    function MessageDlg(const ATitle: string; const AText: String): Boolean;
    procedure ShowFormView(AComponent: TComponent);
    procedure ShowModalFormView(AComponent: TComponent; const AResultProc: TProc<TModalResult>);
    function IsMainThreadUI: Boolean;
    function LoadBitmap(const AFileName: String): TObject; overload;
    function LoadBitmap(const AStream: TStream): TObject; overload;
    function LoadBitmap(const AData: System.SysUtils.TBytes): TObject; overload;
    function LoadBitmap(const AMemory: Pointer; const ASize: Integer): TObject; overload;
    function ElapsedMiliseconds: Int64;
    function ElapsedTicks: Int64;
    function GetTimeStamp: Int64;
    function Elapsed: TTimeSpan;
    function GetReferenceTime: Double;
  end;
{$ENDREGION}
{$REGION 'TPlatformServicesBase'}

  TPlatformServicesBase = class abstract(TInterfacedObject, IPlatformServices)
  public
    function CreatePlatformEmptyForm: TComponent; virtual; abstract;
    procedure AssignParent(AChild, AParent: TComponent); virtual; abstract;
    function MessageDlg(const ATitulo: string; const ATexto: String): Boolean; virtual; abstract;
    procedure ShowFormView(AComponent: TComponent); virtual; abstract;
    procedure ShowModalFormView(AComponent: TComponent; const AResultProc: TProc<TModalResult>); virtual; abstract;
    function IsMainThreadUI: Boolean; virtual; abstract;
    function LoadBitmap(const AFileName: String): TObject; overload; virtual; abstract;
    function LoadBitmap(const AStream: TStream): TObject; overload; virtual; abstract;
    function LoadBitmap(const AData: System.SysUtils.TBytes): TObject; overload; virtual; abstract;
    function LoadBitmap(const AMemory: Pointer; const ASize: Integer): TObject; overload; virtual; abstract;
    function ElapsedMiliseconds: Int64; virtual; abstract;
    function ElapsedTicks: Int64; virtual; abstract;
    function GetTimeStamp: Int64; virtual; abstract;
    function Elapsed: TTimeSpan; virtual; abstract;
    function GetReferenceTime: Double; virtual; abstract;
  end;
{$ENDREGION}

  TPlatformServicesClass = class of TPlatformServicesBase;
{$ENDREGION 'PLATFORM'}
  // Returns the object interface
{$REGION 'IObject'}

  IObject = interface
    ['{61A3454D-3B58-4CDE-83AE-4C3E73732977}']
    function GetAsObject: TObject;
  end;
{$ENDREGION}

  // Bindings
  IBindingStrategy = interface;
  ICollectionViewProvider = interface;
  IBindableAction = interface;

  // Strategy Based Interface
{$REGION 'IStrategyBased'}

  IStrategyBased = interface
    ['{3F645E49-CD84-430C-982F-2B2ADC128203}']
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(ABindingStrategy: IBindingStrategy);

    property BindingStrategy: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION}

  IStrategyEventedObject = interface;

  // Strategy and MultiCastEvents Based Interface
{$REGION 'IStrategyEventedBased'}

  IStrategyEventedBased = interface
    ['{72613457-8EBA-47F0-B277-47F66FD4A427}']
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    property Manager: IStrategyEventedObject read GetManager write SetManager;
  end;
{$ENDREGION}
  // Free notification interface
{$REGION 'INotifyFree'}

  TNotifyFreeObjectEvent = procedure(const ASender, AInstance: TObject) of Object;

  IFreeEvent = IEvent<TNotifyFreeObjectEvent>;

  INotifyFree = interface(IStrategyEventedBased)
    ['{6512F0E1-FB06-4056-8BF2-735EB05A60AC}']
    function GetOnFreeEvent: IFreeEvent;

    property OnFreeEvent: IFreeEvent read GetOnFreeEvent;
  end;
{$ENDREGION}
{$REGION 'INotifyPropertyChanged'}

  TNotifySomethingChangedEvent = procedure(const ASender: TObject; const AData: String) of Object;

  IChangedPropertyEvent = IEvent<TNotifySomethingChangedEvent>;

  INotifyChangedProperty = interface(IStrategyEventedBased)
    ['{9201E57B-98C2-4724-9D03-84E7BF15CDAE}']
    function GetOnPropertyChangedEvent: IChangedPropertyEvent;

    property OnPropertyChangedEvent: IChangedPropertyEvent read GetOnPropertyChangedEvent;
  end;
{$ENDREGION}
{$REGION 'INotifyPropertyChangeTracking'}

  IPropertyChangedTrackingEvent = IEvent<TNotifySomethingChangedEvent>;

  INotifyPropertyTrackingChanged = interface(IStrategyEventedBased)
    ['{70345AF0-199C-4E75-A7BE-5C4929E82620}']
    function GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;

    property OnPropertyChangedTrackingEvent: IChangedPropertyEvent read GetOnPropertyChangedTrackingEvent;
  end;
{$ENDREGION}

  TCollectionSource = TEnumerable<TObject>;

  TCollectionChangedEvent = procedure(const ASender: TObject; const AArgs: TCollectionChangedEventArgs) of Object;
  IChangedCollectionEvent = IEvent<TCollectionChangedEvent>;

{$REGION 'INotifyCollectionChanged'}

  INotifyCollectionChanged = interface(IStrategyEventedBased)
    ['{1DF02979-CEAF-4783-BEE9-2500700E6604}']
    function GetOnCollectionChangedEvent: IChangedCollectionEvent;

    property OnCollectionChangedEvent: IChangedCollectionEvent read GetOnCollectionChangedEvent;
  end;
{$ENDREGION}
{$REGION 'IStrategyEventedObject'}

  IStrategyEventedObject = interface(IStrategyBased)
    ['{CB3FA6A8-371A-481A-AD49-790692843777}']
    function GetOnFreeEvent: IFreeEvent;
    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
    function GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
    function GetOnCollectionChangedEvent: IChangedCollectionEvent;

    function IsAssignedFreeEvent: Boolean;
    function IsAssignedPropertyChangedEvent: Boolean;
    function IsAssignedPropertyChangedTrackingEvent: Boolean;
    function IsAssignedCollectionChangedEvent: Boolean;

    procedure NotifyPropertyChanged(const ASender: TObject; const APropertyName: String);
    procedure NotifyPropertyTrackingChanged(const ASender: TObject; const APropertyName: String);
    procedure NotifyFreeEvent(const ASender, AInstance: TObject);

    property OnFreeEvent: IFreeEvent read GetOnFreeEvent;
    property OnPropertyChangedEvent: IChangedPropertyEvent read GetOnPropertyChangedEvent;
    property OnPropertyChangedTrackingEvent: IChangedPropertyEvent read GetOnPropertyChangedTrackingEvent;
    property OnCollectionChangedEvent: IChangedCollectionEvent read GetOnCollectionChangedEvent;
  end;
{$ENDREGION}
{$REGION 'TStrategyEventedObject'}

  TStrategyEventedObject = class(TInterfacedObject, IStrategyEventedObject)
  protected
    FObject: TObject;
    FBindingStrategy: IBindingStrategy;
    FOnFreeEvent: IFreeEvent;
    FOnPropertyChangedEvent: IChangedPropertyEvent;
    FOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
    FOnCollectionChangedEvent: IChangedCollectionEvent;

    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(ABindingStrategy: IBindingStrategy);

    procedure CheckObjectHasFreeInterface;
    procedure CheckObjectHasPropertyChangedInterface;
    procedure CheckObjectHasPropertyChangedTrackingInterface;
    procedure CheckObjectHasCollectionChangedInterface;

    function GetOnFreeEvent: IFreeEvent;
    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
    function GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
    function GetOnCollectionChangedEvent: IChangedCollectionEvent;

    function IsAssignedFreeEvent: Boolean;
    function IsAssignedPropertyChangedEvent: Boolean;
    function IsAssignedPropertyChangedTrackingEvent: Boolean;
    function IsAssignedCollectionChangedEvent: Boolean;
  public
    constructor Create(AObject: TObject); overload;
    constructor Create(ABindingStrategy: IBindingStrategy; AObject: TObject); overload;
    destructor Destroy; override;

    procedure NotifyPropertyChanged(const ASender: TObject; const APropertyName: String);
    procedure NotifyPropertyTrackingChanged(const ASender: TObject; const APropertyName: String);
    procedure NotifyFreeEvent(const ASender, AInstance: TObject);

    property OnFreeEvent: IFreeEvent read GetOnFreeEvent;
    property OnPropertyChangedEvent: IChangedPropertyEvent read GetOnPropertyChangedEvent;
    property OnPropertyChangedTrackingEvent: IChangedPropertyEvent read GetOnPropertyChangedTrackingEvent;
    property OnCollectionChangedEvent: IChangedCollectionEvent read GetOnCollectionChangedEvent;

    property BindingStrategy: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION}
{$REGION 'IBINDING'}

  TOnBindingAssignedValueEvent = procedure(const AObject: TObject; const AProperty: String; const AValue: TValue) of object;

  IBinding = interface
    ['{13B534D5-094F-4DF3-AE62-C2BD8B703215}']
    function GetID: string;

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);

    function GetOnBindingAssignedValueEvent: TOnBindingAssignedValueEvent;
    procedure SetOnBindingAssignedValueEvent(AValue: TOnBindingAssignedValueEvent);

    procedure DoOnBindingAssignedValue(const AObject: TObject; const AProperty: String; const AValue: TValue);

    procedure DoEnabled;
    procedure DoDisabled;

    function IsObjectInBinding(const AObject: TObject): Boolean;

    property ID: string read GetID;
    property OnBindingAssignedValueEvent: TOnBindingAssignedValueEvent read GetOnBindingAssignedValueEvent write SetOnBindingAssignedValueEvent;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  TBindingBase = class abstract(TInterfacedObject, IBinding, IInterface) // TComponent
  const
    ENABLED_STATE                   = 0;
    DISABLED_ACTIONS_APPLY_ON_VALUE = 1;
  protected
    FID: string;
    FEnabled: Integer;
    FSynchronizer: TLightweightMREW;
    FOnBindingAssignedValueEvent: TOnBindingAssignedValueEvent;

    function GetID: string;

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);

    procedure DoEnabled; virtual;
    procedure DoDisabled; virtual;

    function GetOnBindingAssignedValueEvent: TOnBindingAssignedValueEvent;
    procedure SetOnBindingAssignedValueEvent(AValue: TOnBindingAssignedValueEvent);

    procedure DoOnBindingAssignedValue(const AObject: TObject; const AProperty: String; const AValue: TValue);

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function IsObjectInBinding(const AObject: TObject): Boolean; virtual;

    property ID: string read GetID;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  IBindingDefault = interface(IBinding)
    ['{8E5E2B20-86CF-48D5-99C2-6E9416AD1BE9}']
    function GetBindingStrategy: IBindingStrategy;

    function CheckPropertyChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean = True): Boolean;
    function CheckPropertyTrackingChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean = True): Boolean;
    function CheckFreeChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean = True): Boolean;

    procedure HandlePropertyChanged(const ASender: TObject; const APropertyName: String);
    procedure HandleLeafPropertyChanged(const ASender: TObject; const APropertyName: String);
    procedure HandleFreeEvent(const ASender, AInstance: TObject);

    procedure SetFreeNotification(const AInstance: TObject);
    procedure RemoveFreeNotification(const AInstance: TObject);

    procedure SetManager(const AInstance: TObject);

    property BindingStrategy: IBindingStrategy read GetBindingStrategy;
  end;

  TProcFreeNotify = procedure(AComponent: TComponent) of object;

  TFreeProxyComponent = class(TComponent)
  protected
    FProc: TProcFreeNotify;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure SubscribeToObject(const AObject: TObject);
    procedure UnSubscribeFromObject(const AObject: TObject);

    property DoOnFreeNotification: TProcFreeNotify read FProc write FProc;
  end;

  TBindingDefault = class abstract(TBindingBase, IBindingDefault)
  protected
    FInternalComponent: TFreeProxyComponent;
    [weak]
    FBindingStrategy: IBindingStrategy;
    FTrackedInstances: ISet<Pointer>;

    function GetBindingStrategy: IBindingStrategy;

    procedure Notification(AComponent: TComponent);
  public
    constructor Create(ABindingStrategy: IBindingStrategy); overload;
    destructor Destroy; override;

    function CheckPropertyChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean = True): Boolean;
    function CheckPropertyTrackingChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean = True): Boolean;
    function CheckFreeChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean = True): Boolean;

    procedure HandlePropertyChanged(const ASender: TObject; const APropertyName: String); virtual;
    procedure HandleLeafPropertyChanged(const ASender: TObject; const APropertyName: String); virtual;
    procedure HandleFreeEvent(const ASender, AInstance: TObject); virtual;

    procedure SetFreeNotification(const AInstance: TObject); virtual;
    procedure RemoveFreeNotification(const AInstance: TObject); virtual;

    procedure SetManager(const AInstance: TObject); virtual;

    property BindingStrategy: IBindingStrategy read GetBindingStrategy;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  IBindingCommand = interface(IBinding)
    ['{0E689BEC-3ECA-4D02-B90F-3651D114DC4F}']
    procedure Execute;
    function CanExecute: Boolean;
  end;

  TBindingCommandBase = class abstract(TBindingBase, IBindingCommand)
  protected
    FCanExecute: TCanExecuteMethod;
  public
    function CanExecute: Boolean; virtual;
    procedure Execute; virtual; abstract;
  end;

  TBindingCommandBase<T> = class abstract(TBindingCommandBase)
  protected
    FCommand: T;
    procedure SetCommand(const AValue: T);
    function GetCommand: T;
  public
    constructor Create(ACommand: T; ACanExecute: TCanExecuteMethod = nil); overload;
    property Command: T read GetCommand write SetCommand;
  end;

  TBindingCommandClass = class of TBindingCommandBase;

{$ENDREGION}
  { A view of items in a collection. Is uses by controls that present a
    collection of items, such as TListBox and TListView. These controls will
    implement the IgoCollectionViewProvider interface, that provides an object
    that implements this interface.

    Implementors should use the abstract TgoCollectionView class in the
    Grijjy.Mvvm.DataBinding.Collections unit as a base for their views. }

{$REGION 'ICollectionViewProvider'}

  ICollectionView = interface
    ['{FB28F410-1707-497B-BD1E-67C218E9EB42}']
{$REGION 'Internal Declarations'}
    function GetSource: TCollectionSource;
    procedure SetSource(AValue: TCollectionSource);
    function GetTemplate: TDataTemplateClass;
    procedure SetTemplate(const AValue: TDataTemplateClass);
    function GetComponent: TComponent;
{$ENDREGION 'Internal Declarations'}
    { The collection to show in the view. This can be any class derived from
      TEnumerable<T>, as long is T is a class type. You must typecast it to
      TgoCollectionSource to set the property.

      When you need to get notified when the collection changes, use a
      collection that implements the IgoNotifyCollectionChanged interface, such
      as TgoObservableCollection<T>.

      (In technical terms: TList<TPerson> is covariant, meaning that it is
      convertible to TList<TObject> if TPerson is a class. However, Delphi
      does not support covariance (and contravariance) with generics, so you
      need to typecast to TgoCollectionSource yourself.) }
    property Source: TCollectionSource read GetSource write SetSource;

    { The class that is used as a template to map items in the collection to
      properties of items in the view. }
    property Template: TDataTemplateClass read GetTemplate write SetTemplate;
    property Component: TComponent read GetComponent;
  end;

  ICollectionViewProvider = interface
    ['{22F1E2A9-0078-4401-BA80-C8EFFEE091EA}']
    function GetCollectionView: ICollectionView;
  end;
{$ENDREGION}
{$REGION 'IBindable'}

  IBindable = interface
    ['{74F1EA86-FCFC-49AD-AB53-DCEBF476CB3B}']
    function GetBinding: IBinding;
    procedure SetBinding(ABinding: IBinding);

    property Binding: IBinding read GetBinding write SetBinding;
  end;
{$ENDREGION}
{$REGION 'IBindableAction'}
  TCanExecuteChangedEvent = procedure(const ASender: TObject; const AEnabled: Boolean) of Object;
  ICanExecuteChangedEvent = IEvent<TCanExecuteChangedEvent>;

  IBindableAction = interface(IBindable)
    ['{43A86FDB-96E2-47E4-B636-933430EFDD81}']
     function GetCanExecuteChanged: ICanExecuteChangedEvent;

     function GetAsyncExecution: Boolean;
     procedure SetAsyncExecution(const AValue: Boolean);

     procedure CancelAsyncExecution;

     procedure Bind(const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod = nil); overload;
     procedure Bind(const AExecute: TExecuteAnonymous; const ACanExecute: TCanExecuteMethod = nil); overload;
     procedure Bind(const AExecute: TExecuteRttiMethod; const AExecuteObj: TObject;
                   const ACanExecute: TCanExecuteRttiMethod = nil; const ACanExecuteObj: TObject = nil;
                   const AParams: TParamRtti = nil; const AParamObj: TObject = nil); overload;

     property OnCanExecuteChanged: ICanExecuteChangedEvent read GetCanExecuteChanged;
     property AsyncExecution: Boolean read GetAsyncExecution write SetAsyncExecution;
  end;
{$ENDREGION}
{$REGION 'IBindingStrategy'}

  TBindingList = IList<IBinding>;

  IBindingStrategy = interface
    ['{84676E39-0351-4F3E-AB66-814E022014BD}']
    procedure Start;

    procedure AdquireRead;
    procedure ReleaseRead;
    procedure AdquireWrite;
    procedure ReleaseWrite;

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);

    function GetBindings: TBindingList;

    procedure Notify(const ASource: TObject; const APropertyName: String = ''); overload;
    procedure Notify(const ASource: TObject; const APropertiesNames: TArray<String>); overload;

    procedure AddBinding(ABinding: IBinding);
    procedure RemoveBinding(ABinding: IBinding);
    function BindsCount: Integer;
    procedure ClearBindings;

    // function GetPlatformBindActionCommandType: TBindingCommandClass;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String; const ATarget: TObject; const ATargetPropertyPath: String; const ADirection: EBindDirection = EBindDirection.OneWay; const AFlags: EBindFlags = []; const AValueConverterClass: TValueConverterClass = nil; const AExtraParams: TBindExtraParams = []); overload;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String; const AFlags: EBindFlags = []; const AExtraParams: TBindExtraParams = []); overload;
    procedure BindCollection(AServiceType: PTypeInfo; const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass);
    // DataSet
    procedure BindDataSet(const ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass = nil);
    // -- Combobox
    procedure BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const AOnlyFillValues: Boolean = True); overload;
    procedure BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomFormat: String; const AOnlyFillValues: Boolean = True); overload;
    // -- ListBox
    procedure BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomDisplayExpression: string; const AOnlyFillValues: Boolean = True); overload;
    procedure BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListBoxConversionData; const AOnlyFillValues: Boolean = True); overload;
    // -- ListView
    procedure BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomDisplayExpression: String; const AOnlyFillValues: Boolean = True); overload;
    procedure BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListViewConversionData; const AOnlyFillValues: Boolean = True); overload;
    procedure BindDataSetAppendFieldToListView(ADataSet: TDataSet; ATarget: TComponent; const ALink: TListViewConversionData; const AOnlyFillValues: Boolean = True); overload;
    // -- Grid
    procedure BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent); overload; // basic link
    procedure BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent; const AColumnLinks: array of TGridColumnTemplate); overload;
    // -- Component
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String); overload;
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const AValueConverterClass: TValueConverterClass); overload;
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const ACustomFormat: String); overload;

    procedure BindAction(AAction: IBindableAction); overload;

    procedure Unbind(const ASource: TObject); overload;

    property Bindings: TBindingList read GetBindings;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;
{$ENDREGION}
{$REGION 'TBindingStrategyBase'}

  TBindingStrategyBase = class abstract(TInterfacedObject, IBindingStrategy)
  protected
    FBindings: TBindingList;
    FSynchronizer: TLightweightMREW;

    function GetEnabled: Boolean; virtual; abstract;
    procedure SetEnabled(const AValue: Boolean); virtual; abstract;

    function GetBindings: TBindingList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Start; virtual;

    procedure AdquireRead;
    procedure ReleaseRead;
    procedure AdquireWrite;
    procedure ReleaseWrite;

    procedure Notify(const AObject: TObject; const APropertyName: String = ''); overload; virtual; abstract;
    procedure Notify(const AObject: TObject; const APropertiesNames: TArray<String>); overload; virtual;

    procedure AddBinding(ABinding: IBinding); virtual;
    procedure RemoveBinding(ABinding: IBinding); virtual;
    procedure ClearBindings; virtual;
    function BindsCount: Integer; virtual;

    // function GetPlatformBindActionCommandType: TBindingCommandClass; virtual; abstract;
    // Bindings
    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String; const ATarget: TObject; const ATargetPropertyPath: String; const ADirection: EBindDirection = EBindDirection.OneWay; const AFlags: EBindFlags = []; const AValueConverterClass: TValueConverterClass = nil; const AExtraParams: TBindExtraParams = []); overload; virtual; abstract;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String; const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String; const AFlags: EBindFlags = []; const AExtraParams: TBindExtraParams = []); overload; virtual; abstract;
    // Collections
    procedure BindCollection(AServiceType: PTypeInfo; const ACollection: TEnumerable<TObject>; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass); virtual; abstract;

    // DataSets
    procedure BindDataSet(const ADataSet: TDataSet; const ATarget: ICollectionViewProvider; const ATemplate: TDataTemplateClass = nil); virtual; abstract;

    // -- Combobox
    procedure BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const AOnlyFillValues: Boolean = True); overload; virtual; abstract;
    procedure BindDataSetToCombobox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomFormat: String; const AOnlyFillValues: Boolean = True); overload; virtual; abstract;
    // -- ListBox
    procedure BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomDisplayExpression: string; const AOnlyFillValues: Boolean = True); overload; virtual; abstract;
    procedure BindDataSetToListBox(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListBoxConversionData; const AOnlyFillValues: Boolean = True); overload; virtual; abstract;
    // -- ListView
    procedure BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const AField: String; const ACustomDisplayExpression: String; const AOnlyFillValues: Boolean = True); overload; virtual; abstract;
    procedure BindDataSetToListView(ADataSet: TDataSet; ATarget: TComponent; const ALinks: array of TListViewConversionData; const AOnlyFillValues: Boolean = True); overload; virtual; abstract;
    procedure BindDataSetAppendFieldToListView(ADataSet: TDataSet; ATarget: TComponent; const ALink: TListViewConversionData; const AOnlyFillValues: Boolean = True); overload; virtual; abstract;
    // -- Grid
    procedure BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent); overload; virtual; abstract;
    procedure BindDataSetToGrid(ADataSet: TDataSet; ATarget: TComponent; const AColumnLinks: array of TGridColumnTemplate); overload; virtual; abstract;
    // -- Component
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String); overload; virtual; abstract;
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const AValueConverterClass: TValueConverterClass); overload; virtual; abstract;
    procedure BindDataSetFieldToProperty(ADataSet: TDataSet; const AFieldName: String; const ATarget: TComponent; const ATargetPropertyPath: String; const ACustomFormat: String); overload; virtual; abstract;
    // Actions
    procedure BindAction(AAction: IBindableAction); overload; virtual; abstract;

    procedure Unbind(const ASource: TObject); overload; virtual; abstract;

    property Bindings: TBindingList read GetBindings;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  TClass_BindingStrategyBase = class of TBindingStrategyBase;
{$ENDREGION}

implementation

uses
  MVVM.Core,
  MVVM.Utils;

{ TBindingStrategyBase }

procedure TBindingStrategyBase.AddBinding(ABinding: IBinding);
begin
  AdquireWrite;
  try
    //Utils.IdeDebugMsg('<TBindingStrategyBase.AddBinding> ID: ' + ABinding.ID);
    FBindings.Add(ABinding);
  finally
    ReleaseWrite
  end;
end;

procedure TBindingStrategyBase.AdquireRead;
begin
  FSynchronizer.BeginRead;
end;

procedure TBindingStrategyBase.AdquireWrite;
begin
  FSynchronizer.BeginWrite;
end;

function TBindingStrategyBase.BindsCount: Integer;
begin
  AdquireRead;
  try
    Result := FBindings.Count
  finally
    ReleaseRead;
  end;
end;

procedure TBindingStrategyBase.ClearBindings;
begin
  AdquireWrite;
  try
    FBindings.Clear;
  finally
    ReleaseWrite;
  end;
end;

constructor TBindingStrategyBase.Create;
begin
  inherited;
  FBindings     := TCollections.CreateList<IBinding>;
end;

destructor TBindingStrategyBase.Destroy;
begin
  ClearBindings;
  FBindings     := nil;
  inherited;
end;

function TBindingStrategyBase.GetBindings: TBindingList;
begin
  Result := FBindings;
end;

procedure TBindingStrategyBase.Notify(const AObject: TObject; const APropertiesNames: TArray<String>);
var
  LValue: String;
begin
  for LValue in APropertiesNames do
    Notify(AObject, LValue);
end;

procedure TBindingStrategyBase.ReleaseRead;
begin
  FSynchronizer.EndRead;
end;

procedure TBindingStrategyBase.ReleaseWrite;
begin
  FSynchronizer.EndWrite;
end;

procedure TBindingStrategyBase.RemoveBinding(ABinding: IBinding);
begin
  //Utils.IdeDebugMsg('<TBindingStrategyBase.RemoveBinding> ID: ' + ABinding.ID);
  AdquireWrite;
  try
    FBindings.RemoveAll(
      function(const AABinding: IBinding): Boolean
      begin
        Result := ABinding.ID = AABinding.ID;
      end);
  finally
    ReleaseWrite
  end;
end;

procedure TBindingStrategyBase.Start;
begin;
end;

{ TBindingBase }

constructor TBindingBase.Create;
begin
  inherited Create;
  FID           := GUIDToString(TGuid.NewGuid);
  FEnabled      := ENABLED_STATE;
end;

destructor TBindingBase.Destroy;
begin
  inherited;
end;

procedure TBindingBase.DoEnabled;
begin
  //
end;

procedure TBindingBase.DoOnBindingAssignedValue(const AObject: TObject; const AProperty: String; const AValue: TValue);
begin
  if Assigned(FOnBindingAssignedValueEvent) then
    FOnBindingAssignedValueEvent(AObject, AProperty, AValue);
end;

procedure TBindingBase.DoDisabled;
begin
  //
end;

function TBindingBase.GetEnabled: Boolean;
begin
  FSynchronizer.BeginRead;
  try
    Result := (FEnabled = ENABLED_STATE)
  finally
    FSynchronizer.EndRead
  end;
end;

function TBindingBase.GetID: string;
begin
  Result := FID;
end;

function TBindingBase.GetOnBindingAssignedValueEvent: TOnBindingAssignedValueEvent;
begin
  Result := FOnBindingAssignedValueEvent
end;

function TBindingBase.IsObjectInBinding(const AObject: TObject): Boolean;
begin
  Result := False;
end;

procedure TBindingBase.SetEnabled(const AValue: Boolean);
begin
  FSynchronizer.BeginWrite;
  try
    case AValue of
      True:
        begin
          if (FEnabled > ENABLED_STATE) then
          begin
            Dec(FEnabled);
            if (FEnabled = ENABLED_STATE) then
              DoEnabled;
          end;
        end;
      False:
        begin
          Inc(FEnabled);
          if (FEnabled = DISABLED_ACTIONS_APPLY_ON_VALUE) then // only once
          begin
            DoDisabled;
          end;
        end;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TBindingBase.SetOnBindingAssignedValueEvent(AValue: TOnBindingAssignedValueEvent);
begin
  FOnBindingAssignedValueEvent := AValue;
end;

{ TBindingDefault }
function TBindingDefault.CheckFreeChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean): Boolean;
begin
  Result := True;
  if not Supports(AObject, INotifyFree) then
  begin
    Result := False;
    if ARaiseExceptionIfFalse then
      raise Exception.Create('<INotifyFree> Interface not supported by object of class ' + AObject.QualifiedClassName);
  end;
end;

function TBindingDefault.CheckPropertyChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean): Boolean;
begin
  Result := True;
  if not Supports(AObject, INotifyChangedProperty) then
  begin
    Result := False;
    if ARaiseExceptionIfFalse then
      raise Exception.Create('<INotifyChangedProperty> Interface not supported by object of class ' + AObject.QualifiedClassName);
  end;
end;

function TBindingDefault.CheckPropertyTrackingChangedNotificationSupport(AObject: TObject; const ARaiseExceptionIfFalse: Boolean): Boolean;
begin
  Result := True;
  if not Supports(AObject, INotifyPropertyTrackingChanged) then
  begin
    Result := False;
    if ARaiseExceptionIfFalse then
      raise Exception.Create('<IPropertyChangedTrackingEvent> Interface not supported by object of class ' + AObject.QualifiedClassName);
  end;
end;

constructor TBindingDefault.Create(ABindingStrategy: IBindingStrategy);
begin
  inherited Create;
  FInternalComponent                      := TFreeProxyComponent.Create(nil);
  FInternalComponent.DoOnFreeNotification := Notification;
  FTrackedInstances                       := TCollections.CreateSet<Pointer>;
  FBindingStrategy                        := ABindingStrategy;
end;

destructor TBindingDefault.Destroy;
var
  P: Pointer;
  Instance: TObject;
begin
  if (FTrackedInstances <> nil) then
  begin
    for P in FTrackedInstances do
    begin
      Instance := TObject(P);
      RemoveFreeNotification(Instance);
    end;
    FTrackedInstances := nil;
  end;
  FInternalComponent.Free;
  FBindingStrategy  := nil;
  FTrackedInstances := nil;
  inherited;
end;

function TBindingDefault.GetBindingStrategy: IBindingStrategy;
begin
  Result := FBindingStrategy
end;

// En teoría si llegamos aquí hay que destruir este binding, pues una parte del binding se ha destruido
procedure TBindingDefault.HandleFreeEvent(const ASender, AInstance: TObject);
begin
  FSynchronizer.BeginWrite;
  try
    FTrackedInstances.Remove(AInstance);
  finally
    FSynchronizer.EndWrite;
  end;
  if Assigned(FBindingStrategy) then
  begin
    FBindingStrategy.RemoveBinding(Self);
    FBindingStrategy := nil;
  end;
end;

procedure TBindingDefault.HandleLeafPropertyChanged(const ASender: TObject; const APropertyName: String);
begin
  //
end;

procedure TBindingDefault.HandlePropertyChanged(const ASender: TObject; const APropertyName: String);
begin
  //
end;

procedure TBindingDefault.Notification(AComponent: TComponent);
begin
  // inherited;
  // if (Operation = opRemove) then
  // begin
  // Utils.IdeDebugMsg('<TBindingDefault.Notification> Name: ' + AComponent.Name);
  HandleFreeEvent(AComponent, AComponent);
  // end;
end;

procedure TBindingDefault.RemoveFreeNotification(const AInstance: TObject);
begin
  if (AInstance is TComponent) then
    FInternalComponent.UnSubscribeFromObject(AInstance);
  // TComponent(AInstance).RemoveFreeNotification(Self);
end;

procedure TBindingDefault.SetFreeNotification(const AInstance: TObject);
var
  [weak]
  LNotifyFree: INotifyFree;
begin
  if (AInstance is TComponent) then
  begin
    FSynchronizer.BeginWrite;
    try
      FTrackedInstances.Add(AInstance);
    finally
      FSynchronizer.EndWrite;
    end;
    FInternalComponent.SubscribeToObject(AInstance);
    // TComponent(AInstance).FreeNotification(Self);
  end
  else if Supports(AInstance, INotifyFree, LNotifyFree) then
  begin
    FSynchronizer.BeginWrite;
    try
      FTrackedInstances.Add(AInstance);
      LNotifyFree.OnFreeEvent.Add(HandleFreeEvent);
    finally
      FSynchronizer.EndWrite;
    end;
  end;
end;

procedure TBindingDefault.SetManager(const AInstance: TObject);
var
  [weak]
  LNotifyProperty: INotifyChangedProperty;
  [weak]
  LNotifyPropertyTracking: INotifyPropertyTrackingChanged;
  [weak]
  LNotifyCollection: INotifyCollectionChanged;
  LManager: IStrategyEventedObject;
begin
  if Supports(AInstance, INotifyChangedProperty, LNotifyProperty) then
  begin
    LManager := LNotifyProperty.Manager;
    if LManager = nil then
    begin
      LManager                := TStrategyEventedObject.Create(AInstance);
      LNotifyProperty.Manager := LManager;
    end;
    LManager.BindingStrategy := FBindingStrategy;
  end;
  if Supports(AInstance, INotifyPropertyTrackingChanged, LNotifyPropertyTracking) then
  begin
    LManager := LNotifyPropertyTracking.Manager;
    if LManager = nil then
    begin
      LManager                        := TStrategyEventedObject.Create(AInstance);
      LNotifyPropertyTracking.Manager := LManager;
    end;
    LManager.BindingStrategy := FBindingStrategy;
  end;
  if Supports(AInstance, INotifyCollectionChanged, LNotifyCollection) then
  begin
    LManager := LNotifyCollection.Manager;
    if LManager = nil then
    begin
      LManager                  := TStrategyEventedObject.Create(AInstance);
      LNotifyCollection.Manager := LManager;
    end;
    LManager.BindingStrategy := FBindingStrategy;
  end;
end;

{ TBindingCommandBase<T> }

constructor TBindingCommandBase<T>.Create(ACommand: T; ACanExecute: TCanExecuteMethod);
begin
  inherited Create;
  FCommand    := ACommand;
  FCanExecute := ACanExecute;
end;

function TBindingCommandBase<T>.GetCommand: T;
begin
  Result := FCommand
end;

procedure TBindingCommandBase<T>.SetCommand(const AValue: T);
begin
  FCommand := AValue;
end;

{ TStrategyEventedObject }

procedure TStrategyEventedObject.CheckObjectHasCollectionChangedInterface;
begin
  Guard.CheckTrue(Supports(FObject, INotifyCollectionChanged));
end;

procedure TStrategyEventedObject.CheckObjectHasFreeInterface;
begin
  Guard.CheckTrue(Supports(FObject, INotifyFree));
end;

procedure TStrategyEventedObject.CheckObjectHasPropertyChangedInterface;
begin
  Guard.CheckTrue(Supports(FObject, INotifyChangedProperty));
end;

procedure TStrategyEventedObject.CheckObjectHasPropertyChangedTrackingInterface;
begin
  Guard.CheckTrue(Supports(FObject, INotifyPropertyTrackingChanged));
end;

constructor TStrategyEventedObject.Create(AObject: TObject);
begin
  Guard.CheckNotNull(AObject, '(Param=AObject) is null');
  inherited Create;
  FObject          := AObject;
  FBindingStrategy := MVVMCore.DefaultBindingStrategy;
end;

constructor TStrategyEventedObject.Create(ABindingStrategy: IBindingStrategy; AObject: TObject);
begin
  Guard.CheckNotNull(ABindingStrategy, '(Param=ABindingStrategy) is null');
  Guard.CheckNotNull(AObject, '(Param=AObject) is null');
  inherited Create;
  FObject          := AObject;
  FBindingStrategy := ABindingStrategy;
end;

destructor TStrategyEventedObject.Destroy;
begin
  FBindingStrategy := nil;
  inherited;
end;

function TStrategyEventedObject.GetBindingStrategy: IBindingStrategy;
begin
  Result := FBindingStrategy
end;

function TStrategyEventedObject.GetOnCollectionChangedEvent: IChangedCollectionEvent;
begin
  if (FOnCollectionChangedEvent = nil) then
  begin
    CheckObjectHasCollectionChangedInterface;
    FOnCollectionChangedEvent := Utils.CreateEvent<TCollectionChangedEvent>;
  end;
  Result := FOnCollectionChangedEvent;
end;

function TStrategyEventedObject.GetOnFreeEvent: IFreeEvent;
begin
  if (FOnFreeEvent = nil) then
  begin
    CheckObjectHasFreeInterface;
    FOnFreeEvent := Utils.CreateEvent<TNotifyFreeObjectEvent>;
  end;
  Result := FOnFreeEvent;
end;

function TStrategyEventedObject.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  if (FOnPropertyChangedEvent = nil) then
  begin
    CheckObjectHasPropertyChangedInterface;
    FOnPropertyChangedEvent := Utils.CreateEvent<TNotifySomethingChangedEvent>;
  end;
  Result := FOnPropertyChangedEvent;
end;

function TStrategyEventedObject.GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
begin
  if (FOnPropertyChangedTrackingEvent = nil) then
  begin
    CheckObjectHasPropertyChangedTrackingInterface;
    FOnPropertyChangedTrackingEvent := Utils.CreateEvent<TNotifySomethingChangedEvent>;
  end;
  Result := FOnPropertyChangedTrackingEvent;
end;

function TStrategyEventedObject.IsAssignedCollectionChangedEvent: Boolean;
begin
  Result := Assigned(FOnCollectionChangedEvent)
end;

function TStrategyEventedObject.IsAssignedFreeEvent: Boolean;
begin
  Result := Assigned(FOnFreeEvent)
end;

function TStrategyEventedObject.IsAssignedPropertyChangedEvent: Boolean;
begin
  Result := Assigned(FOnPropertyChangedEvent)
end;

function TStrategyEventedObject.IsAssignedPropertyChangedTrackingEvent: Boolean;
begin
  Result := Assigned(FOnPropertyChangedTrackingEvent)
end;

procedure TStrategyEventedObject.NotifyFreeEvent(const ASender, AInstance: TObject);
begin
  if IsAssignedFreeEvent then
    FOnFreeEvent.Invoke(ASender, AInstance);
end;

procedure TStrategyEventedObject.NotifyPropertyChanged(const ASender: TObject; const APropertyName: String);
begin
  if IsAssignedPropertyChangedEvent then
    FOnPropertyChangedEvent.Invoke(ASender, APropertyName);
end;

procedure TStrategyEventedObject.NotifyPropertyTrackingChanged(const ASender: TObject; const APropertyName: String);
begin
  if IsAssignedPropertyChangedTrackingEvent then
    FOnPropertyChangedTrackingEvent.Invoke(ASender, APropertyName);
end;

procedure TStrategyEventedObject.SetBindingStrategy(ABindingStrategy: IBindingStrategy);
begin
  FBindingStrategy := ABindingStrategy;
end;

{ TBindingCommandBase }

function TBindingCommandBase.CanExecute: Boolean;
begin
  Result := Enabled;
  if Result then
  begin
    if Assigned(FCanExecute) then
      Result := FCanExecute;
  end;
end;

{ TFreeProxyComponent }

procedure TFreeProxyComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    //Utils.IdeDebugMsg('<TBindingDefault.Notification> Name: ' + AComponent.Name);
    FProc(AComponent);
  end;
end;

procedure TFreeProxyComponent.SubscribeToObject(const AObject: TObject);
begin
  TComponent(AObject).FreeNotification(Self);
end;

procedure TFreeProxyComponent.UnSubscribeFromObject(const AObject: TObject);
begin
  TComponent(AObject).RemoveFreeNotification(Self);
end;

end.
