unit MVVM.ViewFactory;

interface

uses
  System.UITypes,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  MVVM.Core,
  MVVM.Interfaces;

type
  { A factory for creating views based on their names.
    This allows for loose coupling between view models and views, since the
    view model can create a view without knowing its actual type.

    All views (except the main form) should implement the IView interface and
    register the view class by calling TViewFactory.Register (usually in the
    Initialization section of the unit).

    View models can create a view by calling TViewFactory.CreateView. }
  TViewFactory = class // abstract
  {$REGION 'Internal Declarations'}
  private class var
    FRegisteredViews: TDictionary<String, TComponentClass>;
  public
    class destructor Destroy;
  {$ENDREGION 'Internal Declarations'}
  public
    { Registers a view class with the factory.

      Parameters:
        AViewClass: the class of the view (such as a form) to register.
          AViewClass MUST implement the IView interface.
        AViewName: the name to register the view under.

      Raises:
        EInvalidOperation if AViewClass does not implement IView.

      This method should be called once for each view type, usually in the
      Initialization section of the unit of that view. }
    class procedure Register(const AViewClass: TComponentClass; const AViewName: String); static;

    { Creates a view using a previously registered view class.

      Parameters:
        TVM: type parameter with the type of view model used by the view.
        AViewName: the name of the view. This must be one of the names
          previously registered using the Register method. The name determines
          the type of view that is created.
        AOwner: the owner for the view. This is passed as the standard AOwner
          parameter of the view class.
        AViewModel: the view model for the the view. This method will call
          IView.Initialize to associate the view model with the view.
        AOwnsViewModel: (optional) whether the view becomes owner of the view
          model. If True (the default), then the view model will automatically
          be freed when the view is freed.

      Returns:
        A newly created view that represents AViewName.

      Raises:
        EListError if there is no view registered with the given AViewName. }
    class function CreateView<TVM: IViewModel>(const AViewName: String; const AOwner: TComponent; AViewModel: TVM): IView<TVM>; static;
  end;

type
  { Internal class used to manage lifetimes of views. }
  TViewProxy<TVM: IViewModel> = class(TInterfacedObject, IView, IView<TVM>)
  {$REGION 'Internal Declarations'}
  private type
    TViewFreeListener = class(TComponent)
    strict private
      FActualView: IView<TVM>;
      [unsafe] FActualViewObject: TComponent;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(const AView: IView<TVM>); reintroduce;
      destructor Destroy; override;

      property ActualView: IView<TVM> read FActualView;
    end;
  private
    FViewFreeListener: TViewFreeListener;
  protected
    function GetViewModel: TVM;
    { IView }
    procedure ExecuteModal(const AResultProc: TProc<TModalResult>);
  protected
    { IView<TVM> }
    procedure InitView(AViewModel: TVM);
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AActualView: IView<TVM>);
    destructor Destroy; override;

    function GetAsObject: TObject;

    procedure SetupView;
    property ViewModel: TVM read GetViewModel;
  end;

implementation

uses
  System.Generics.Defaults;

{ TViewProxy<TVM> }

constructor TViewProxy<TVM>.Create(const AActualView: IView<TVM>);
begin
  Assert(Assigned(AActualView));
  inherited Create;
  { Executing a view may free it (for example, when the view is a form and its
    CloseAction is set to caFree). When that happens, the IView interface
    will contain an invalid reference, and you may get an access violation when
    it goes out of scope. To avoid this, we use a view proxy. This proxy
    subscribes to a free notification of the view, so it can set the IView
    interface to nil BEFORE the view is destroyed. }
  FViewFreeListener := TViewFreeListener.Create(AActualView);
end;

destructor TViewProxy<TVM>.Destroy;
begin
  FViewFreeListener.Free;
  inherited;
end;

procedure TViewProxy<TVM>.ExecuteModal(const AResultProc: TProc<TModalResult>);
var
  LViewForm: IViewForm<TVM>;
begin
  if Assigned(FViewFreeListener.ActualView) then
  begin
    if Supports(FViewFreeListener.ActualView, IViewForm<TVM>, LViewForm) then
      LViewForm.ExecuteModal(AResultProc);
  end;
end;

function TViewProxy<TVM>.GetAsObject: TObject;
begin
  Result := Self
end;

function TViewProxy<TVM>.GetViewModel: TVM;
begin
  if Assigned(FViewFreeListener.ActualView) then
    Result := FViewFreeListener.ActualView.ViewModel
  else Result := nil;
end;

procedure TViewProxy<TVM>.InitView(AViewModel: TVM);
begin
  if Assigned(FViewFreeListener.ActualView) then
    FViewFreeListener.ActualView.InitView(AViewModel);
end;

procedure TViewProxy<TVM>.SetupView;
begin
  if Assigned(FViewFreeListener.ActualView) then
    FViewFreeListener.ActualView.SetupView;
end;

{ TViewProxy<TVM>.TViewFreeListener }

constructor TViewProxy<TVM>.TViewFreeListener.Create(const AView: IView<TVM>);
var
  Instance: TObject;
begin
  inherited Create(nil);
  FActualView := AView;
  Instance := TObject(AView);
  if (Instance is TComponent) then
  begin
    FActualViewObject := TComponent(Instance);
    FActualViewObject.FreeNotification(Self);
  end;
end;

destructor TViewProxy<TVM>.TViewFreeListener.Destroy;
begin
  if (FActualViewObject <> nil) then
    FActualViewObject.RemoveFreeNotification(Self);
  inherited;
end;

procedure TViewProxy<TVM>.TViewFreeListener.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FActualViewObject) and (Operation = opRemove) then
  begin
    FActualView := nil;
    FActualViewObject := nil;
  end;
end;

{ TViewFactory }

class function TViewFactory.CreateView<TVM>(const AViewName: String; const AOwner: TComponent; AViewModel: TVM): IView<TVM>;
var
  ViewClass: TComponentClass;
  ViewComp: TComponent;
  View: IView<TVM>;
begin
  try
    ViewClass := nil;
    if Assigned(FRegisteredViews) then
      FRegisteredViews.TryGetValue(AViewName, ViewClass);

    if (ViewClass = nil) then
      raise EListError.CreateFmt('Cannot create view. View "%s" is not registered.', [AViewName]);

    ViewComp := ViewClass.Create(AOwner);
    View := ViewComp as IView<TVM>;
    Result := TViewProxy<TVM>.Create(View);
    Result.InitView(AViewModel);
  except
    AViewModel := nil;
    raise;
  end;
end;

class destructor TViewFactory.Destroy;
begin
  FreeAndNil(FRegisteredViews);
end;

class procedure TViewFactory.Register(const AViewClass: TComponentClass; const AViewName: String);
begin
  if (not Supports(AViewClass, IView)) then
    raise EInvalidOperation.CreateFmt('View class %s must implement the IView interface',
      [AViewClass.ClassName]);

  if (FRegisteredViews = nil) then
    FRegisteredViews := TDictionary<String, TComponentClass>.Create(
      TIStringComparer.Ordinal);

  FRegisteredViews.AddOrSetValue(AViewName, AViewClass);
end;

end.
