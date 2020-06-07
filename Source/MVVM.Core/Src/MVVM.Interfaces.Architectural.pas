unit MVVM.Interfaces.Architectural;

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,

  MVVM.CommandFactory,
  MVVM.Interfaces,
  MVVM.Observable;

type
{$REGION 'MVVM-Layers'}
  IModel = interface(IObject)
    ['{28C9B05B-A5F5-49E1-913E-2AB10F9FB8F3}']
  end;

  IViewModel = interface(IObject)
    ['{37E13CBF-FDB2-4C6B-948A-7D5F7A6D0AC5}']
    procedure BindCommands(const AView: TComponent);
    procedure SetupViewModel;
  end;

  TViewModel = class abstract(TObservable, IViewModel, INotifyChangedProperty, INotifyFree)
  private
    FCommandsFactory: TCommandsFactory;
  protected
    procedure AfterConstruction; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure BindCommands(const AView: TComponent);
    procedure SetupViewModel; virtual; abstract;

    function GetAsObject: TObject;
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

  IViewForm<T: IViewModel> = interface(IView<T>)
    ['{16407011-00BD-4BCA-9453-1D3F4E1C5DE1}']
    procedure Execute;
    procedure ExecuteModal(const AResultProc: TProc<TModalResult>);
  end;
{$ENDREGION}

implementation

uses
  MVVM.Utils;

{ TViewModel }

procedure TViewModel.BindCommands(const AView: TComponent);
begin
  //Utils.IdeDebugMsg('<TViewModel.BindCommands>');
  FCommandsFactory.LoadCommandsAndActionsFrom(AView);
  FCommandsFactory.BindView(AView);
end;

constructor TViewModel.Create;
begin
  //Utils.IdeDebugMsg('<TViewModel.Create>');
  inherited;
  FCommandsFactory := TCommandsFactory.Create;
end;

destructor TViewModel.Destroy;
begin
  FCommandsFactory.Free;
  inherited;
end;

function TViewModel.GetAsObject: TObject;
begin
  Result := Self
end;

procedure TViewModel.AfterConstruction;
begin
  inherited;
  FCommandsFactory.LoadCommandsAndActionsFrom(Self);
end;

end.
