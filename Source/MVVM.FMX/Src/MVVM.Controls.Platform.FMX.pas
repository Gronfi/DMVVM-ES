unit MVVM.Controls.Platform.FMX;

interface

uses
  System.Rtti,
  System.Classes,
  FMX.Controls.Model,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Memo,
  FMX.ComboEdit,
  FMX.Colors,
  FMX.DateTimeCtrls,
  FMX.SpinBox,
  FMX.NumberBox,
  FMX.ListBox,
  FMX.ListView,
  FMX.Objects,
  FMX.ActnList,

  MVVM.Interfaces,
  MVVM.Types;

type
{$REGION 'FMX.StdCtrls'}
  { TCheckBox with support for light-weight two-way data binding.
    Supports property changed notifications for: IsChecked }
  TCheckBox = class(FMX.StdCtrls.TCheckBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FOrigOnChange     : TNotifyEvent;
    FEstrategiaBinding: IEstrategiaBinding;
  private
    procedure HandleOnChange(Sender: TObject);
  protected
    procedure Loaded; override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;

  { TTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TTrackBar = class(FMX.StdCtrls.TTrackBar, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    procedure DoChanged; override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;

  { TSwitch with support for light-weight two-way data binding.
    Supports property changed notifications for: IsChecked }
  TSwitch = class(FMX.StdCtrls.TSwitch, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    procedure DoSwitch; override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;

  { TArcDial with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TArcDial = class(FMX.StdCtrls.TArcDial, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    procedure AfterChangedProc(Sender: TObject); override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;
{$ENDREGION 'FMX.StdCtrls'}

{$REGION 'FMX.ActnList'}
  TAction = class(FMX.ActnList.TAction, IBindableAction)
  {$REGION 'Internal Declarations'}
  private
    FExecute   : TExecuteMethod;
    FCanExecute: TCanExecuteMethod;
  {$ENDREGION 'Internal Declarations'}
  public
    { IBindableAction }
    procedure Bind(const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod = nil; const AEstrategiaBinding: String = ''); overload;
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;
{$ENDREGION 'FMX.ActnList'}

{$REGION 'FMX.Edit'}
type
  { TEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Text
    Supports property changing notifications for: Text }
  TEdit = class(FMX.Edit.TEdit, INotifyPropertyChanged, INotifyPropertyChangeTracking)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;
{$ENDREGION 'FMX.Edit'}

{$REGION 'FMX.Memo'}
type
  { TMemo with support for light-weight two-way data binding.
    Supports property changed notifications for: Text
    Supports property changing notifications for: Text }
  TMemo = class(FMX.Memo.TMemo, INotifyPropertyChanged, INotifyPropertyChangeTracking)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;
{$ENDREGION 'FMX.Memo'}

{$REGION 'FMX.ComboEdit'}
type
  { TComboEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Text
    Supports property changing notifications for: Text }
  TComboEdit = class(FMX.ComboEdit.TComboEdit, INotifyPropertyChanged, INotifyPropertyChangeTracking)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;
{$ENDREGION 'FMX.ComboEdit'}

implementation

type
  TBindableEditModel = class(TCustomEditModel)
  protected
    procedure DoChange; override;
    procedure DoChangeTracking; override;
  end;

  TBindableMemoModel = class(TCustomMemoModel)
  protected
    procedure DoChange; override;
    procedure DoChangeTracking; override;
  end;

  TBindableComboEditModel = class(TComboEditModel)
  protected
    procedure DoChange; override;
    procedure DoChangeTracking; override;
  end;


{ TAction }

procedure TAction.Bind(const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod; const AEstrategiaBinding: String);
begin
  //le da igual la estrategia de binding, hace siempre lo mismo
  FExecute    := AExecute;
  FCanExecute := ACanExecute;
end;

constructor TAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := False;
end;

function TAction.Execute: Boolean;
begin
  Result := inherited Execute;
  if (Supported) and (not Suspended) and (Enabled) then
  begin
    if Assigned(FExecute) then
      FExecute();
  end;
end;

function TAction.Update: Boolean;
begin
  Result := inherited Update;
  if (Supported) and Assigned(FCanExecute) then
    Enabled := FCanExecute();
end;

{ TCheckBox }

function TCheckBox.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TCheckBox.HandleOnChange(Sender: TObject);
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'IsChecked');

  if Assigned(FOrigOnChange) then
    FOrigOnChange(Sender);
end;

procedure TCheckBox.Loaded;
begin
  inherited;
  FOrigOnChange := OnChange;
  OnChange      := HandleOnChange;
end;

procedure TCheckBox.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TTrackBar }

procedure TTrackBar.DoChanged;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Value');
  inherited;
end;

function TTrackBar.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TTrackBar.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TSwitch }

procedure TSwitch.DoSwitch;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'IsChecked');
  inherited;
end;

function TSwitch.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TSwitch.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TArcDial }

procedure TArcDial.AfterChangedProc(Sender: TObject);
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Value');
  inherited;
end;

function TArcDial.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TArcDial.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TEdit }

function TEdit.DefineModelClass: TDataModelClass;
begin
  Result := TBindableEditModel;
end;

function TEdit.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TEdit.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TBindableEditModel }

procedure TBindableEditModel.DoChange;
var
  Owner: TComponent;
  LEdit: TEdit absolute Owner;
begin
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TEdit);
    if Assigned(LEdit.FEstrategiaBinding) then
      LEdit.FEstrategiaBinding.Notify(LEdit, 'Text');
  end;
  inherited;
end;

procedure TBindableEditModel.DoChangeTracking;
var
  Owner: TComponent;
  LEdit: TEdit absolute Owner;
begin
  inherited;
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TEdit);
    if Assigned(LEdit.FEstrategiaBinding) then
      LEdit.FEstrategiaBinding.Notify(LEdit, 'Text');
  end;
end;

{ TMemo }

function TMemo.DefineModelClass: TDataModelClass;
begin
  Result := TBindableMemoModel;
end;

function TMemo.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TMemo.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TBindableMemoModel }

procedure TBindableMemoModel.DoChange;
var
  Owner: TComponent;
  LMemo: TMemo absolute Owner;
begin
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TMemo);
    if Assigned(LMemo.FEstrategiaBinding) then
      LMemo.FEstrategiaBinding.Notify(LMemo, 'Text');
  end;
  inherited;
end;

procedure TBindableMemoModel.DoChangeTracking;
var
  Owner: TComponent;
  LMemo: TMemo absolute Owner;
begin
  inherited;
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TMemo);
    if Assigned(LMemo.FEstrategiaBinding) then
      LMemo.FEstrategiaBinding.Notify(LMemo, 'Text');
  end;
end;

{ TComboEdit }

function TComboEdit.DefineModelClass: TDataModelClass;
begin
  Result := TBindableComboEditModel;
end;

function TComboEdit.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TComboEdit.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TBindableComboEditModel }

procedure TBindableComboEditModel.DoChange;
var
  Owner: TComponent;
  LComboEdit: TComboEdit absolute Owner;
begin
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TComboEdit);
    if Assigned(LComboEdit.FEstrategiaBinding) then
      LComboEdit.FEstrategiaBinding.Notify(LComboEdit, 'Text');
  end;
  inherited;
end;

procedure TBindableComboEditModel.DoChangeTracking;
var
  Owner: TComponent;
  LComboEdit: TComboEdit absolute Owner;
begin
  inherited;
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TComboEdit);
    if Assigned(LComboEdit.FEstrategiaBinding) then
      LComboEdit.FEstrategiaBinding.Notify(LComboEdit, 'Text');
  end;
end;

end.
