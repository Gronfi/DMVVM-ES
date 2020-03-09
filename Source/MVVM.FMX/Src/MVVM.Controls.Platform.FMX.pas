unit MVVM.Controls.Platform.FMX;

interface

uses
  System.Rtti,
  System.Classes,

  FMX.Forms,
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

{$REGION 'FMX.Colors'}
type
  { TColorPanel with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TColorPanel = class(FMX.Colors.TColorPanel, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FOrigOnChange: TNotifyEvent;
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

type
  { TComboColorBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TComboColorBox = class(FMX.Colors.TComboColorBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    procedure DoColorChange(Sender: TObject); override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;

type
  { TColorListBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TColorListBox = class(FMX.Colors.TColorListBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    procedure DoChange; override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;

type
  { TColorComboBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TColorComboBox = class(FMX.Colors.TColorComboBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    procedure DoChange; override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;

type
  { THueTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  THueTrackBar = class(FMX.Colors.THueTrackBar, INotifyPropertyChanged)
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

type
  { TAlphaTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TAlphaTrackBar = class(FMX.Colors.TAlphaTrackBar, INotifyPropertyChanged)
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

type
  { TBWTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TBWTrackBar = class(FMX.Colors.TBWTrackBar, INotifyPropertyChanged)
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
{$ENDREGION 'FMX.Colors'}

{$REGION 'FMX.DateTimeCtrls'}
type
  { TTimeEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Time }
  TTimeEdit = class(FMX.DateTimeCtrls.TTimeEdit, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    procedure DoDateTimeChanged; override;
    procedure HandlerPickerDateTimeChanged(Sender: TObject; const ADate: TDateTime); override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;

type
  { TDateEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Date }
  TDateEdit = class(FMX.DateTimeCtrls.TDateEdit, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
  protected
    procedure DoDateTimeChanged; override;
    procedure HandlerPickerDateTimeChanged(Sender: TObject; const ADate: TDateTime); override;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;
{$ENDREGION 'FMX.DateTimeCtrls'}

{$REGION 'FMX.SpinBox'}
type
  { TSpinBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TSpinBox = class(FMX.SpinBox.TSpinBox, INotifyPropertyChanged)
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
{$ENDREGION 'FMX.SpinBox'}

{$REGION 'FMX.NumberBox'}
type
  { TNumberBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TNumberBox = class(FMX.NumberBox.TNumberBox, INotifyPropertyChanged)
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
{$ENDREGION 'FMX.NumberBox'}

{$REGION 'FMX.ListBox'}
type
  { TListBox with support for light-weight two-way data binding.
    Supports property changed notifications for: ItemIndex, Selected, SelectedItem.
    NOTE: When used with data binding, the TListBoxItem.Data property is used
          to store the associated object. }
  TListBox = class(FMX.ListBox.TListBox, ICollectionViewProvider, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IEstrategiaBinding;
    FView: ICollectionView;

    function GetSelectedItem: TObject; inline;
    procedure SetSelectedItem(const Value: TObject);
  private
    procedure DoSelectionChanged;
    function FindListBoxItem(const AItem: TObject): Integer;
  protected
    procedure DoChange; override;
  protected
    { IgoCollectionViewProvider }
    function GetCollectionView: ICollectionView;
  protected
    function GetEstrategiaBinding: IEstrategiaBinding;
    procedure SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
  {$ENDREGION 'Internal Declarations'}
  public
    { Destructor }
    destructor Destroy; override;

    procedure Clear; override;

    { The object that is associated with the selected item, or nil if there is
      no item selected or there is no object associated with the selected item.
      The associated object is the object in the TListBoxItem.Data property. }
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property EstrategiaBinding: IEstrategiaBinding read GetEstrategiaBinding write SetEstrategiaBinding;
  end;
{$ENDREGION 'FMX.ListBox'}


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

  TBindableSpinBoxModel = class(TSpinBoxModel)
  protected
    procedure DoChange; override;
  end;

  TBindableNumberBoxModel = class(TNumberBoxModel)
  protected
    procedure DoChange; override;
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

{ TColorPanel }

function TColorPanel.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TColorPanel.HandleOnChange(Sender: TObject);
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Color');

  if Assigned(FOrigOnChange) then
    FOrigOnChange(Sender);
end;

procedure TColorPanel.Loaded;
begin
  inherited;
  FOrigOnChange := OnChange;
  OnChange      := HandleOnChange;
end;

procedure TColorPanel.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TComboColorBox }

procedure TComboColorBox.DoColorChange(Sender: TObject);
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Color');
  inherited;
end;

function TComboColorBox.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TComboColorBox.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TColorListBox }

procedure TColorListBox.DoChange;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Color');
  inherited;
end;

function TColorListBox.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TColorListBox.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TColorComboBox }

procedure TColorComboBox.DoChange;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Color');
  inherited;
end;

function TColorComboBox.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TColorComboBox.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ THueTrackBar }

procedure THueTrackBar.DoChanged;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Value');
  inherited;
end;

function THueTrackBar.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure THueTrackBar.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TAlphaTrackBar }

procedure TAlphaTrackBar.DoChanged;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Value');
  inherited;
end;

function TAlphaTrackBar.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TAlphaTrackBar.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TBWTrackBar }

procedure TBWTrackBar.DoChanged;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Value');
  inherited;
end;

function TBWTrackBar.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TBWTrackBar.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TTimeEdit }

procedure TTimeEdit.DoDateTimeChanged;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Time');
  inherited;
end;

function TTimeEdit.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TTimeEdit.HandlerPickerDateTimeChanged(Sender: TObject; const ADate: TDateTime);
begin
  { This method can be called from an OS specific picker (on iOS and Android).
    FMX does not protect those with the usual try..except block.
    So we do that here to make sure the application doesn't crash when an
    exception occurs in this method. }
  try
    inherited;
  except
    Application.HandleException(Self);
  end;
end;

procedure TTimeEdit.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TDateEdit }

procedure TDateEdit.DoDateTimeChanged;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Date');
  inherited;
end;

function TDateEdit.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TDateEdit.HandlerPickerDateTimeChanged(Sender: TObject; const ADate: TDateTime);
begin
  { This method can be called from an OS specific picker (on iOS and Android).
    FMX does not protect those with the usual try..except block.
    So we do that here to make sure the application doesn't crash when an
    exception occurs in this method. }
  try
    inherited;
  except
    Application.HandleException(Self);
  end;
end;

procedure TDateEdit.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TSpinBox }

function TSpinBox.DefineModelClass: TDataModelClass;
begin
  Result := TBindableSpinBoxModel;
end;

function TSpinBox.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TSpinBox.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TBindableSpinBoxModel }

procedure TBindableSpinBoxModel.DoChange;
var
  Owner   : TComponent;
  LSpinBox: TSpinBox absolute Owner;
begin
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TSpinBox);
    if Assigned(LSpinBox.FEstrategiaBinding) then
      LSpinBox.FEstrategiaBinding.Notify(LSpinBox, 'Value');
  end;
  inherited;
end;

{ TNumberBox }

function TNumberBox.DefineModelClass: TDataModelClass;
begin
  Result := TBindableNumberBoxModel;
end;

function TNumberBox.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

procedure TNumberBox.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TBindableNumberBoxModel }

procedure TBindableNumberBoxModel.DoChange;
var
  Owner: TComponent;
  LNumberBox: TNumberBox absolute Owner;
begin
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TNumberBox);
    if Assigned(LNumberBox.FEstrategiaBinding) then
      LNumberBox.FEstrategiaBinding.Notify(LNumberBox, 'Value');
  end;
  inherited;
end;

{ TListBox }

procedure TListBox.Clear;
begin
  if (Count > 0) then
  begin
    inherited;
    DoSelectionChanged;
  end;
end;

destructor TListBox.Destroy;
begin
  FView := nil;
  inherited;
end;

procedure TListBox.DoChange;
begin
  DoSelectionChanged;
  inherited;
end;

procedure TListBox.DoSelectionChanged;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, ['ItemIndex', 'Selected', 'SelectedItem']);
end;

function TListBox.FindListBoxItem(const AItem: TObject): Integer;
var
  Item: TListBoxItem;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Item := ItemByIndex(I);
    if (Item.Data = AItem) then
      Exit(I);
  end;

  Result := -1;
end;

function TListBox.GetCollectionView: ICollectionView;
begin
  if (FView = nil) then
    FView := TListBoxCollectionView.Create(Self);
  Result := FView;
end;

function TListBox.GetEstrategiaBinding: IEstrategiaBinding;
begin
  Result := FEstrategiaBinding
end;

function TListBox.GetSelectedItem: TObject;
var
  Sel: TListBoxItem;
begin
  Sel := Selected;
  if Assigned(Sel) then
    Result := Sel.Data
  else
    Result := nil;
end;

procedure TListBox.SetEstrategiaBinding(AEstrategiaBinding: IEstrategiaBinding);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

procedure TListBox.SetSelectedItem(const Value: TObject);
begin
  ItemIndex := FindListBoxItem(Value);
end;

end.
