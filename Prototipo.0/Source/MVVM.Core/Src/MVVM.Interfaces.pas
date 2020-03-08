unit MVVM.Interfaces;

interface

uses
  Spring,
  System.Bindings.Expression, System.Bindings.Helper;

type
  INotificationChanged = interface
    ['{DC4EF24C-660A-46EE-8404-4ECF67CF7287}']
  end;

  IEventoNotificationChanged<T:INotificationChanged> = interface(IEvent<T>)
  end;

  IEventNotificationChanged = IEventoNotificationChanged<INotificationChanged>;

  INotificationPropertyChanged = interface(INotificationChanged)
    ['{9201E57B-98C2-4724-9D03-84E7BF15CDAE}']
  end;

  TOpcionesBinding = TBindings.TCreateOptions;

  IModel = interface
    ['{28C9B05B-A5F5-49E1-913E-2AB10F9FB8F3}']
    procedure Notify(const APropertyName: string = '');
    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;
    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;
  end;

  IViewModel = interface
    ['{37E13CBF-FDB2-4C6B-948A-7D5F7A6D0AC5}']
    procedure Notify(const APropertyName: string = '');
    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;
    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;
  end;

  IViewModel<T:IModel> = interface(IVIewModel)
    ['{2B47A54F-4C87-4C17-9D68-8848F4A0555A}']
    procedure SetModel(AModel: T);
  end;

  IView<T:IModel> = interface
    ['{BF036A8C-6302-482C-BD7B-DED350D255F9}']
    procedure AddViewModel(AViewModel: IViewModel<T>);
    procedure RemoveViewModel(AViewModel: IViewModel<T>);
  end;

implementation

end.
