unit MVVM.Interfaces.Architectural;

interface

uses
  System.SysUtils,
  System.UITypes,

  MVVM.Interfaces,
  MVVM.Observable;

type
{$REGION 'MVVM-Layers'}
  IModel = interface(IObject)
    ['{28C9B05B-A5F5-49E1-913E-2AB10F9FB8F3}']
  end;

  IViewModel = interface
    ['{37E13CBF-FDB2-4C6B-948A-7D5F7A6D0AC5}']
    procedure SetupViewModel;
  end;

  TViewModel = class abstract(TObservable, IViewModel)
  public
    procedure SetupViewModel; virtual; abstract;
  end;

  TViewModelClass = class of TViewModel;

  IView = interface(IObject)
    ['{44055F6F-42A8-43DD-B393-1CC700B8C7F8}']
    procedure SetupView;
  end;

  IView<T: IViewModel> = interface(IView)
    ['{BF4DF63F-88A6-4971-A1D1-EF03620FA4C2}']
    function GetViewModel: T;

    procedure InitView(AViewModel: T);

    property ViewModel: T read GetViewModel;
  end;

  IView<T: IViewModel; K: TViewModel> = interface(IView<T>)
    ['{BF036A8C-6302-482C-BD7B-DED350D255F9}']
    function GetVM_AsObject: K;
    property ViewModel_AsObject: K read GetVM_AsObject;
  end;

  IViewForm<T: IViewModel> = interface(IView<T>)
    ['{16407011-00BD-4BCA-9453-1D3F4E1C5DE1}']
    procedure Execute;
    procedure ExecuteModal(const AResultProc: TProc<TModalResult>);
  end;
{$ENDREGION}

implementation

end.
