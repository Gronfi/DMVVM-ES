unit MVVM.Views.Platform.FMX;

{$HPPEMIT 'MVVM.Views.Platform.FMX'}

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Forms,
  FMX.Objects,

  MVVM.Interfaces.Architectural,
  MVVM.Core,
  MVVM.Bindings;

type
  TFrameView<T: IViewModel; K: TViewModel> = class abstract(TFrame, IView, IView<T>, IView<T,K>)
{$REGION 'Internal Declarations'}
  private
    FBinder: TBindingManager;
    FViewModel: T;
  protected
    function GetViewModel: T;
    function GetVM_AsObject: K;
{$ENDREGION 'Internal Declarations'}
  protected
    procedure SetupView; virtual;
  public
    procedure InitView(AViewModel: T);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetAsObject: TObject;

    property ViewModel: T read GetViewModel;
    property ViewModel_AsObject: K read GetVM_AsObject;

    property Binder: TBindingManager read FBinder;
  end;

  TFormView<T: IViewModel; K: TViewModel> = class(TForm, IView, IView<T>, IViewForm<T>, IView<T,K>)
{$REGION 'Internal Declarations'}
  private
{$IFNDEF MOBILE}
    FPrevForm: TCommonCustomForm;
    FOverlay: TRectangle;
{$ENDIF}
    FBinder: TBindingManager;
    FViewModel: T;
    FGrayOutPreviousForm: Boolean;
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;
    function GetViewModel: T;
    function GetVM_AsObject: K;
{$ENDREGION 'Internal Declarations'}
  protected
    procedure SetupView; virtual;
  public
    procedure InitView(AViewModel: T);
    procedure Execute;
    procedure ExecuteModal(const AResultProc: TProc<TModalResult>);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetAsObject: TObject;

    property GrayOutPreviousForm: Boolean read FGrayOutPreviousForm write FGrayOutPreviousForm;
    property ViewModel: T read GetViewModel;
    property ViewModel_AsObject: K read GetVM_AsObject;
    property Binder: TBindingManager read FBinder;
  end;

implementation

uses
  FMX.Graphics,

  Spring;

{ TFormView<T> }

constructor TFormView<T,K>.Create(AOwner: TComponent);
begin
{$IFNDEF MOBILE}
  FPrevForm := Screen.ActiveForm;
{$ENDIF}
  inherited;
  FBinder              := TBindingManager.Create;
  FGrayOutPreviousForm := True;
end;

destructor TFormView<T,K>.Destroy;
begin
  FBinder.Free;
  FViewModel := nil;
  inherited;
end;

procedure TFormView<T,K>.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caFree;
  inherited;
  { When ModelResult = mrNone, the user closed the form by clicking the X
    button or some other means.
    When using ShowModal, this would NOT result in a callback to the
    anonymous ResultProc. So we set ModalResult to mrCancel in this case, which
    results in a callback, }
  if (ModalResult = mrNone) then
    ModalResult := mrCancel;
{$IFNDEF MOBILE}
  if Assigned(FPrevForm) and (FGrayOutPreviousForm) then
  begin
    FPrevForm.RemoveObject(FOverlay);
    FOverlay.DisposeOf;
  end;
{$ENDIF}
end;

procedure TFormView<T,K>.Execute;
begin
  Show;
end;

procedure TFormView<T,K>.ExecuteModal(const AResultProc: TProc<TModalResult>);
{$IFNDEF ANDROID}
var
  MR: TModalResult;
{$ENDIF}
{$IFNDEF MOBILE}
var
  PrevForm: TCommonCustomForm;
{$ENDIF}
begin
{$IFNDEF MOBILE}
  PrevForm := Screen.ActiveForm;
  if (PrevForm <> nil) then
    FPrevForm := PrevForm;
  if Assigned(FPrevForm) and (FGrayOutPreviousForm) then
  begin
    { Add overlay to prevent users from interacting with the previous form. }
    FOverlay := TRectangle.Create(FPrevForm);
    FOverlay.SetBounds(0, 0, 9999, 9999);
    FOverlay.Stroke.Kind := TBrushKind.None;
    FOverlay.Fill.Color  := $50000000;
    FPrevForm.AddObject(FOverlay);
  end;
{$ENDIF}
  ShowModal(AResultProc);
end;

function TFormView<T,K>.GetAsObject: TObject;
begin
  Result := Self
end;

function TFormView<T,K>.GetViewModel: T;
begin
  Result := TValue.From<IViewModel>(FViewModel).AsType<T>;
end;

function TFormView<T, K>.GetVM_AsObject: K;
begin
  Result := TValue.From<IViewModel>(FViewModel).AsType<K>;
end;

procedure TFormView<T,K>.InitView(AViewModel: T);
begin
  Guard.CheckNotNull(AViewModel, 'The viewmodel cannot be nil');
  Guard.CheckTrue(FViewModel = nil, 'The viewmodel is already assigned');
  FViewModel := AViewModel;
  SetupView;
end;

procedure TFormView<T,K>.SetupView;
begin
  //
end;

{ TFrameView<T> }

constructor TFrameView<T,K>.Create(AOwner: TComponent);
begin
  inherited;
  FBinder := TBindingManager.Create;
end;

destructor TFrameView<T,K>.Destroy;
begin
  FBinder.Free;
  inherited;
end;

function TFrameView<T,K>.GetAsObject: TObject;
begin
  Result := Self
end;

function TFrameView<T,K>.GetViewModel: T;
//var
//  LValue: TValue;
begin
  Result := TValue.From<IViewModel>(FViewModel).AsType<T>;
  //Result := LValue.AsType<T>;
end;

function TFrameView<T, K>.GetVM_AsObject: K;
//var
//  LValue: TValue;
begin
  Result := TValue.From<IViewModel>(FViewModel).AsType<K>;
//  Result := LValue.AsType<K>;
end;

procedure TFrameView<T,K>.InitView(AViewModel: T);
begin
  Guard.CheckNotNull(AViewModel, 'The viewmodel cannot be nil');
  Guard.CheckTrue(FViewModel = nil, 'The viewmodel is already assigned');
  FViewModel := AViewModel;
  SetupView;
end;

procedure TFrameView<T,K>.SetupView;
begin
  //
end;

end.
