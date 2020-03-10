unit MVVM.Interfaces;

interface

uses
  System.SysUtils,
  System.RTTI,
  System.Bindings.Expression, System.Bindings.Helper,
  System.Generics.Collections,
  System.UITypes,

  Spring,

  MVVM.Types;

type
  IPlatformServices = interface
    ['{95F9A402-2D01-48E5-A38B-9A6202FF5F59}']
    function MessageDlg(const ATitle: string; const AText: String): Boolean;
    function IsMainThreadUI: Boolean;
  end;

  IObject = interface
    ['{61A3454D-3B58-4CDE-83AE-4C3E73732977}']
    function GetAsObject: TObject;
  end;

  IMessage = interface(IObject)
    ['{8C6AE8E2-B18D-41B4-AAED-88CF3B110F1D}']
    function GetCreationDateTime: TDateTime;
    procedure Queue;

    property CreationDateTime: TDateTime read GetCreationDateTime;
  end;

  TNotifyMessage = procedure (AMessage: IMessage) of Object;

  TListenerFilter = reference to function (AMessage: IMessage): Boolean;

  IMessageListener = interface(IObject)
    ['{ABC992B0-4CB4-470A-BDCE-EBE6651C84DD}']
    function GetIsCodeToExecuteInUIMainThread: boolean;
    procedure SetIsCodeToExecuteInUIMainThread(const AValue: boolean);

    function GetTypeRestriction: EMessageTypeRestriction;
    procedure SetTypeRestriction(const ATypeRestriction: EMessageTypeRestriction);

    function GetListenerFilter: TListenerFilter;
    procedure SetListenerFilter(const AFilter: TListenerFilter);

    function GetMensajeClass: TClass;

    function GetConditionsMatch(AMessage: IMessage): boolean;

    procedure Register;
    procedure UnRegister;

    procedure NewMessage(AMessage: IMessage);

    property FilterCondition: TListenerFilter read GetListenerFilter write SetListenerFilter;
    property IsCodeToExecuteInUIMainThread: boolean read GetIsCodeToExecuteInUIMainThread write SetIsCodeToExecuteInUIMainThread;
    property TypeRestriction: EMessageTypeRestriction read GetTypeRestriction write SetTypeRestriction;
  end;

  INotificationChanged = interface
    ['{DC4EF24C-660A-46EE-8404-4ECF67CF7287}']
  end;

  IEventNotificationChanged<T:INotificationChanged> = interface(IEvent<T>)
  end;

  IEventNotificationChanged = IEventNotificationChanged<INotificationChanged>;

  IBindingStrategy = interface;

  INotifyPropertyChanged = interface
    ['{9201E57B-98C2-4724-9D03-84E7BF15CDAE}']
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(ABindingStrategy: IBindingStrategy);

    property BindingStrategy: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

  INotifyPropertyChangeTracking = interface
    ['{70345AF0-199C-4E75-A7BE-5C4929E82620}']
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(ABindingStrategy: IBindingStrategy);

    property BindingStrategy: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

  ICollectionSource = IEnumerable<TObject>;

  //ICollectionChangedEvent = IgoMultiCastEvent<TgoCollectionChangedEventArgs>;

  INotifyCollectionChanged = interface
  ['{1E5FE0CA-B5B2-4F07-B881-FC99971716C6}']
    { Gets the multi-cast event that must be fired when a collection or an
      item in a collection has changed.

      Returns:
        The event. Callers can subscribe to or unsubscribe from this event.

      To support notifications of changes to individual items, the items in the
      collection implement the IgoPropertyChangedEvent interface.

      The implementor must invoke this event when the collection or an item
      in the collection has changed. }
    //function GetCollectionChangedEvent: ICollectionChangedEvent; DAVID
  end;

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
    class function GetImageIndex(const AItem: TObject): Integer; virtual;

    class function GetStyle(const AItem: TObject): string; virtual;
  end;

  TDataTemplateClass = class of TDataTemplate;

    { A view of items in a collection. Is uses by controls that present a
    collection of items, such as TListBox and TListView. These controls will
    implement the IgoCollectionViewProvider interface, that provides an object
    that implements this interface.

    Implementors should use the abstract TgoCollectionView class in the
    Grijjy.Mvvm.DataBinding.Collections unit as a base for their views. }
  ICollectionView = interface
  ['{FB28F410-1707-497B-BD1E-67C218E9EB42}']
    {$REGION 'Internal Declarations'}
    function GetSource: ICollectionSource;
    procedure SetSource(AValue: ICollectionSource);
    function GetTemplate: TDataTemplateClass;
    procedure SetTemplate(const AValue: TDataTemplateClass);
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
    property Source: ICollectionSource read GetSource write SetSource;

    { The class that is used as a template to map items in the collection to
      properties of items in the view. }
    property Template: TDataTemplateClass read GetTemplate write SetTemplate;
  end;

  ICollectionViewProvider = interface
  ['{22F1E2A9-0078-4401-BA80-C8EFFEE091EA}']
    function GetCollectionView: ICollectionView;
  end;

  IBindableAction = interface
  ['{43A86FDB-96E2-47E4-B636-933430EFDD81}']
    procedure Bind(const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod = nil; const ABindingStrategy: String = ''); overload;
  end;

  TBindingOptions = TBindings.TCreateOptions;

  IBindingStrategy = interface
    ['{84676E39-0351-4F3E-AB66-814E022014BD}']
    procedure Start;

    procedure Notify(const ASource: TObject; const APropertyName: String = ''); overload;
    procedure Notify(const ASource: TObject; const APropertiesNames: TArray<String>); overload;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
      const ATarget: TObject; const ATargetPropertyPath: String;
      const ADirection: EBindDirection = EBindDirection.OneWay;
      const AFlags: EBindFlags = [];
      const AValueConverterClass: TBindingValueConverterClass = nil;
      const AExtraParams: TBindExtraParams = []); overload;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String;
                   const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String;
                   const AFlags: EBindFlags = [];
                   const AExtraParams: TBindExtraParams = []); overload;
    procedure BindCollection<T: class>(const ACollection: TEnumerable<T>;
                                       const ATarget: ICollectionViewProvider;
                                       const ATemplate: TDataTemplateClass);
    procedure BindAction(const AAction: IBindableAction;
                     const AExecute: TExecuteMethod;
                     const ACanExecute: TCanExecuteMethod = nil); overload;

    procedure ClearBindings;
  end;

  IDataBinder = interface
    ['{E880F234-ED85-4594-9DA5-869100B95F8B}']
    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
                   const ATarget: TObject; const ATargetPropertyPath: String;
                   const ADirection: EBindDirection = EBindDirection.OneWay;
                   const AFlags: EBindFlags = [];
                   const AValueConverterClass: TBindingValueConverterClass = nil;
                   const ABindingStrategy: String = '';
                   const AExtraParams: TBindExtraParams = []); overload;
    procedure Bind(const ASources: TSourcePairArray; const ASourceExpresion: String;
                   const ATarget: TObject; const ATargetAlias: String; const ATargetPropertyPath: String;
                   const AFlags: EBindFlags = [];
                   const ABindingStrategy: String = '';
                   const AExtraParams: TBindExtraParams = []); overload;
    procedure BindCollection<T: class>(const ACollection: TEnumerable<T>;
                                       const ATarget: ICollectionViewProvider;
                                       const ATemplate: TDataTemplateClass;
                                       const ABindingStrategy: String = '');

    procedure BindAction(const AAction: IBindableAction;
                         const AExecute: TExecuteMethod;
                         const ACanExecute: TCanExecuteMethod = nil;
                         const ABindingStrategy: String = ''); overload;

    procedure Notify(const AObject: TObject; const APropertyName: String); overload;
    procedure Notify(const AObject: TObject; const APropertiesNames: TArray<String>); overload;
  end;

  IBinder = interface
    ['{AA80417A-B7B8-4867-A310-89BDAB7FEEDD}']
    function GetDataBinder: IDataBinder;

    property DataBinder: IDataBinder read GetDataBinder;
  end;

  IModel = interface
    ['{28C9B05B-A5F5-49E1-913E-2AB10F9FB8F3}']
//    procedure Notify(const APropertyName: string = '');
//    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
//    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
//    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;
//    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;
  end;

  IViewModel = interface
    ['{37E13CBF-FDB2-4C6B-948A-7D5F7A6D0AC5}']
//    procedure Notify(const APropertyName: string = '');
//    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
//    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
//    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;
//    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure SetupViewModel;
  end;

  IViewModel<T:IModel> = interface(IVIewModel)
    ['{2B47A54F-4C87-4C17-9D68-8848F4A0555A}']
    function GetModel: T;
    procedure SetModel(AModel: T);

    property Model: T read GetModel;
  end;

  IView = interface(IBinder)
    ['{44055F6F-42A8-43DD-B393-1CC700B8C7F8}']
    procedure SetupView;
  end;

  IView<T:IViewModel> = interface
    ['{BF036A8C-6302-482C-BD7B-DED350D255F9}']
    function GetViewModel: T;
    procedure AddViewModel(AViewModel: IViewModel);

    property ViewModel: T read GetViewModel;
  end;

  IViewForm<T:IViewModel> = interface(IView<T>)
    procedure ExecuteModal(const AResultProc: TProc<TModalResult>);
  end;

implementation

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

end.
