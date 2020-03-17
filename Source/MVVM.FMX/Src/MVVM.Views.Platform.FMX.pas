unit MVVM.Views.Platform.FMX;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Forms,
  FMX.Objects,

  MVVM.Interfaces,
  MVVM.Core,
  MVVM.Bindings;

type
  TFrameView<T: IViewModel> = class(TFrame, IView, IView<T>)
{$REGION 'Internal Declarations'}
  private
    FBinder: TBindingManager;
    FViewModel: T;
  protected
    function GetViewModel: T; virtual;
{$ENDREGION 'Internal Declarations'}
  protected
    procedure SetupView; virtual;

    { The view model associated with the view }
    property ViewModel: T read GetViewModel;
  public
    procedure InitView(AViewModel: T);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Binder: TBindingManager read FBinder;
  end;

  TFormView<T: IViewModel> = class(TForm, IView, IView<T>, IViewForm<T>)
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
    function GetViewModel: T; virtual;
{$ENDREGION 'Internal Declarations'}
  protected
    procedure SetupView; virtual;

    { The view model associated with the view }
    property ViewModel: T read GetViewModel;
  public
    procedure InitView(AViewModel: T);
    procedure Execute;
    procedure ExecuteModal(const AResultProc: TProc<TModalResult>);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property GrayOutPreviousForm: Boolean read FGrayOutPreviousForm
      write FGrayOutPreviousForm;

    property Binder: TBindingManager read FBinder;
  end;

implementation

uses
  FMX.Graphics,

  Spring;

{ TFormView<T> }

constructor TFormView<T>.Create(AOwner: TComponent);
begin
{$IFNDEF MOBILE}
  FPrevForm := Screen.ActiveForm;
{$ENDIF}
  inherited;
  FBinder := TBindingManager.Create;
  FGrayOutPreviousForm := True;
end;

destructor TFormView<T>.Destroy;
begin
  FBinder.Free;
  FViewModel := nil;
  inherited;
end;

procedure TFormView<T>.DoClose(var CloseAction: TCloseAction);
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

procedure TFormView<T>.Execute;
begin
  Show;
end;

procedure TFormView<T>.ExecuteModal(const AResultProc: TProc<TModalResult>);
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
    FOverlay.Fill.Color := $50000000;
    FPrevForm.AddObject(FOverlay);
  end;
{$ENDIF}
  ShowModal(AResultProc);
end;

function TFormView<T>.GetViewModel: T;
begin
  Result := FViewModel;
end;

procedure TFormView<T>.InitView(AViewModel: T);
begin
  Guard.CheckNotNull(AViewModel, 'The viewmodel cannot be nil');
  Guard.CheckTrue(FViewModel = nil, 'The viewmodel is already assigned');
  FViewModel := AViewModel;
  SetupView;
end;

procedure TFormView<T>.SetupView;
begin
  //
end;

{ TFrameView<T> }

constructor TFrameView<T>.Create(AOwner: TComponent);
begin
  inherited;
  FBinder := TBindingManager.Create;
end;

destructor TFrameView<T>.Destroy;
begin
  FBinder.Free;
  inherited;
end;

function TFrameView<T>.GetViewModel: T;
begin
  Result := FViewModel;
end;

procedure TFrameView<T>.InitView(AViewModel: T);
begin
  Guard.CheckNotNull(AViewModel, 'The viewmodel cannot be nil');
  Guard.CheckTrue(FViewModel = nil, 'The viewmodel is already assigned');
  FViewModel := AViewModel;
  SetupView;
end;

procedure TFrameView<T>.SetupView;
begin
  //
end;

end.
