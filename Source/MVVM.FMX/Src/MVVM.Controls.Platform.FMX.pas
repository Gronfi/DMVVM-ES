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
  FMX.TreeView,

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
    FEstrategiaBinding: IBindingStrategy;
  private
    procedure HandleOnChange(Sender: TObject);
  protected
    procedure Loaded; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

  { TTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TTrackBar = class(FMX.StdCtrls.TTrackBar, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoChanged; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

  { TSwitch with support for light-weight two-way data binding.
    Supports property changed notifications for: IsChecked }
  TSwitch = class(FMX.StdCtrls.TSwitch, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoSwitch; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

  { TArcDial with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TArcDial = class(FMX.StdCtrls.TArcDial, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure AfterChangedProc(Sender: TObject); override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
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
    FEstrategiaBinding: IBindingStrategy;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
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
    FEstrategiaBinding: IBindingStrategy;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
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
    FEstrategiaBinding: IBindingStrategy;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
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
    FEstrategiaBinding: IBindingStrategy;
  private
    procedure HandleOnChange(Sender: TObject);
  protected
    procedure Loaded; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

type
  { TComboColorBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TComboColorBox = class(FMX.Colors.TComboColorBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoColorChange(Sender: TObject); override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

type
  { TColorListBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TColorListBox = class(FMX.Colors.TColorListBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoChange; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

type
  { TColorComboBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TColorComboBox = class(FMX.Colors.TColorComboBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoChange; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

type
  { THueTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  THueTrackBar = class(FMX.Colors.THueTrackBar, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoChanged; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

type
  { TAlphaTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TAlphaTrackBar = class(FMX.Colors.TAlphaTrackBar, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoChanged; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

type
  { TBWTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TBWTrackBar = class(FMX.Colors.TBWTrackBar, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoChanged; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION 'FMX.Colors'}

{$REGION 'FMX.DateTimeCtrls'}
type
  { TTimeEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Time }
  TTimeEdit = class(FMX.DateTimeCtrls.TTimeEdit, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoDateTimeChanged; override;
    procedure HandlerPickerDateTimeChanged(Sender: TObject; const ADate: TDateTime); override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

type
  { TDateEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Date }
  TDateEdit = class(FMX.DateTimeCtrls.TDateEdit, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoDateTimeChanged; override;
    procedure HandlerPickerDateTimeChanged(Sender: TObject; const ADate: TDateTime); override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION 'FMX.DateTimeCtrls'}

{$REGION 'FMX.SpinBox'}
type
  { TSpinBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TSpinBox = class(FMX.SpinBox.TSpinBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION 'FMX.SpinBox'}

{$REGION 'FMX.NumberBox'}
type
  { TNumberBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TNumberBox = class(FMX.NumberBox.TNumberBox, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
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
    FEstrategiaBinding: IBindingStrategy;
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
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    { Destructor }
    destructor Destroy; override;

    procedure Clear; override;

    { The object that is associated with the selected item, or nil if there is
      no item selected or there is no object associated with the selected item.
      The associated object is the object in the TListBoxItem.Data property. }
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION 'FMX.ListBox'}

{$REGION 'FMX.ListView'}
  { TListView with support for light-weight two-way data binding.
    Supports property changed notifications for: ItemIndex, Selected, SelectedItem
    NOTE: When used with data binding, the TListViewItem.Tag property is used
          to store the associated object. This is an unsafe reference, so you
          must make sure that the associated objects are available for the
          lifetime of the list view. }
  TListView = class(FMX.ListView.TListView, ICollectionViewProvider, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
    FView: ICollectionView;
    function GetSelectedItem: TObject; inline;
    procedure SetSelectedItem(const Value: TObject);
  private
    function FindListViewItem(const AItem: TObject): Integer;
  protected
    procedure DoChange; override;
  protected
    { IgoCollectionViewProvider }
    function GetCollectionView: ICollectionView;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    { Destructor }
    destructor Destroy; override;

    { The object that is associated with the selected item, or nil if there is
      no item selected or there is no object associated with the selected item.
      The associated object is the object in the TListViewItem.Tag property.
      This is an unsafe reference, so you must make sure that the associated
      objects are available for the lifetime of the list view. }
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION 'FMX.ListView'}

{$REGION 'FMX.Objects'}
type
  { TImage with support for light-weight two-way data binding.
    Supports property changed notifications for: Bitmap }
  TImage = class(FMX.Objects.TImage, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
    procedure DoChanged; override;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  {$ENDREGION 'Internal Declarations'}
  public
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION 'FMX.Objects'}

{$REGION 'FMX.TreeView'}
type
  TTreeView = class(Fmx.TreeView.TTreeView, ICollectionViewProvider, INotifyPropertyChanged)
  {$REGION 'Internal Declarations'}
  private
    FEstrategiaBinding: IBindingStrategy;
    FView: ICollectionView;
    FOnChanged_ : TNotifyEvent;
    function GetSelectedNode: TObject; inline;
    procedure SetSelectedNode(const Value: TObject);
  private
    function FindTreeNode(const AItem: TObject): TTreeViewItem;
  protected
    procedure Loaded; override;
    procedure Change( Sender: TObject);
    procedure DoSelectNode(Node: TTreeViewItem; Selected: Boolean);
  protected
    { IgoCollectionViewProvider }
    function GetCollectionView: ICollectionView;
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
  protected
  {$ENDREGION 'Internal Declarations'}
  public
    { Destructor }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The object that is associated with the selected item, or nil if there is
      no item selected or there is no object associated with the selected item.
      The associated object is the object in the TListItem.Data property. }
    property SelectedNode: TObject read GetSelectedNode write SetSelectedNode;
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION 'FMX.TreeView'}

{$REGION 'FMX.ComboBox'}

  { TComboBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Text
    Supports property changing notifications for: Text
    NOTE: Both PropertyChanged and PropertyChangeTracking notifications are
    fired for each individual keypress. }
  TComboBox = class(Fmx.ListBox.TComboBox, INotifyPropertyChanged,  ICollectionViewProvider, INotifyPropertyChangeTracking)
  {$REGION 'Internal Declarations'}
  private
    FView             : ICollectionView;
    FEstrategiaBinding: IBindingStrategy;

    FSelectedItem: TObject;
    function GetSelectedItem: TObject; inline;
    procedure SetSelectedItem(const Value: TObject);
  protected
    FChanged_: TNotifyEvent;
    procedure Loaded; override;
    procedure Change (Sender: TObject);
  protected
    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);

    function GetCollectionView: ICollectionView;
  {$ENDREGION 'Internal Declarations'}
  public
    { Destructor }
    destructor Destroy; override;
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;
{$ENDREGION 'FMX.ComboBox'}

implementation

uses
  FMX.Forms,
  FMX.Graphics,
  FMX.ListView.Appearances,

  MVVM.Bindings.Collections;

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

  TListBoxCollectionView = class(TCollectionView)
  private
    [weak] FListBox: TListBox;
  private
    procedure UpdateListBoxItem(const AListBoxItem: TListBoxItem;
      const AItem: TObject);
  protected
    procedure ClearItemsInView; override;
    procedure BeginUpdateView; override;
    procedure EndUpdateView; override;
    procedure AddItemToView(const AItem: TObject); override;
    procedure DeleteItemFromView(const AItemIndex: Integer); override;
    procedure UpdateItemInView(const AItem: TObject;
      const APropertyName: String); override;
    procedure UpdateAllItemsInView; override;

    function GetComponent: TComponent; override;
  public
    constructor Create(const AListBox: TListBox);
  end;

  TListViewCollectionView = class(TCollectionView)
  private
    [weak] FListView: TListView;
  private
    procedure UpdateListViewItem(const AListViewItem: TListViewItem; const AItem: TObject);
  protected
    procedure ClearItemsInView; override;
    procedure BeginUpdateView; override;
    procedure EndUpdateView; override;
    procedure AddItemToView(const AItem: TObject); override;
    procedure DeleteItemFromView(const AItemIndex: Integer); override;
    procedure UpdateItemInView(const AItem: TObject; const APropertyName: String); override;
    procedure UpdateAllItemsInView; override;

    function GetComponent: TComponent; override;
  public
    constructor Create(const AListView: TListView);
  end;

  TTreeViewCollectionView = class(TCollectionView)
  private
    FTreeView: TTreeView;
  private
    procedure UpdateTreeNode(const ATreeViewItem: TTreeViewItem; const AItem: TObject);
  protected
    procedure ClearItemsInView; override;
    procedure BeginUpdateView; override;
    procedure EndUpdateView; override;
    procedure AddItemToView(const AItem: TObject); override;
    procedure DeleteItemFromView(const AItemIndex: Integer); override;
    procedure UpdateItemInView(const AItem: TObject;
      const APropertyName: String); override;
    procedure UpdateAllItemsInView; override;

    function GetComponent: TComponent; override;
  public
    constructor Create(const ATreeView: TTreeView);
  end;

  TComboBoxCollectionView = class(TCollectionView)
  private
    FComboBox: TComboBox;
  protected
    procedure ClearItemsInView; override;
    procedure BeginUpdateView; override;
    procedure EndUpdateView; override;
    procedure AddItemToView(const AItem: TObject); override;
    procedure DeleteItemFromView(const AItemIndex: Integer); override;
    procedure UpdateItemInView(const AItem: TObject;
      const APropertyName: String); override;
    procedure UpdateAllItemsInView; override;

    function GetComponent: TComponent; override;
  public
    constructor Create(const AComboBox: TComboBox);
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

function TCheckBox.GetBindingStrategy: IBindingStrategy;
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

procedure TCheckBox.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TTrackBar.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TTrackBar.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TSwitch.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TSwitch.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TArcDial.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TArcDial.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TEdit }

function TEdit.DefineModelClass: TDataModelClass;
begin
  Result := TBindableEditModel;
end;

function TEdit.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TEdit.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TMemo.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TMemo.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TComboEdit.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TComboEdit.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TColorPanel.GetBindingStrategy: IBindingStrategy;
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

procedure TColorPanel.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TComboColorBox.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TComboColorBox.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TColorListBox.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TColorListBox.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TColorComboBox.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TColorComboBox.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function THueTrackBar.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure THueTrackBar.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TAlphaTrackBar.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TAlphaTrackBar.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TBWTrackBar.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TBWTrackBar.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TTimeEdit.GetBindingStrategy: IBindingStrategy;
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

procedure TTimeEdit.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TDateEdit.GetBindingStrategy: IBindingStrategy;
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

procedure TDateEdit.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TSpinBox }

function TSpinBox.DefineModelClass: TDataModelClass;
begin
  Result := TBindableSpinBoxModel;
end;

function TSpinBox.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TSpinBox.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TNumberBox.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TNumberBox.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
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

function TListBox.GetBindingStrategy: IBindingStrategy;
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

procedure TListBox.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

procedure TListBox.SetSelectedItem(const Value: TObject);
begin
  ItemIndex := FindListBoxItem(Value);
end;

{ TListBoxCollectionView }

procedure TListBoxCollectionView.AddItemToView(const AItem: TObject);
var
  ListBox: TListBox;
  ListBoxItem: TListBoxItem;
begin
  ListBox := FListBox; // Strong reference
  if Assigned(ListBox) then
  begin
    ListBoxItem := TListBoxItem.Create(ListBox);
    UpdateListBoxItem(ListBoxItem, AItem);
    ListBox.AddObject(ListBoxItem);
  end;
end;

procedure TListBoxCollectionView.BeginUpdateView;
var
  ListBox: TListBox;
begin
  ListBox := FListBox; // Strong reference
  if Assigned(ListBox) then
    ListBox.BeginUpdate;
end;

procedure TListBoxCollectionView.ClearItemsInView;
var
  ListBox: TListBox;
begin
  ListBox := FListBox; // Strong reference
  if Assigned(ListBox) then
    ListBox.Clear;
end;

constructor TListBoxCollectionView.Create(const AListBox: TListBox);
begin
  Assert(Assigned(AListBox));
  inherited Create;
  FListBox := AListBox;
end;

procedure TListBoxCollectionView.DeleteItemFromView(const AItemIndex: Integer);
var
  ListBox: TListBox;
begin
  ListBox := FListBox; // Strong reference
  if Assigned(ListBox) then
    ListBox.Items.Delete(AItemIndex);
end;

procedure TListBoxCollectionView.EndUpdateView;
var
  ListBox: TListBox;
begin
  ListBox := FListBox; // Strong reference
  if Assigned(ListBox) then
    ListBox.EndUpdate;
end;

function TListBoxCollectionView.GetComponent: TComponent;
begin
  Result := FListBox;
end;

procedure TListBoxCollectionView.UpdateAllItemsInView;
var
  ListBox: TListBox;
  Item: TObject;
  ListBoxItem: TListBoxItem;
  Index: Integer;
begin
  ListBox := FListBox; // Strong reference
  if Assigned(ListBox) then
  begin
    Index := 0;
    for Item in Source do
    begin
      ListBoxItem := ListBox.ItemByIndex(Index);
      UpdateListBoxItem(ListBoxItem, Item);
      Inc(Index);
    end;
  end;
end;

procedure TListBoxCollectionView.UpdateItemInView(const AItem: TObject; const APropertyName: String);
var
  ListBox: TListBox;
  Index: Integer;
begin
  ListBox := FListBox; // Strong reference
  if Assigned(ListBox) then
  begin
    Index := ListBox.FindListBoxItem(AItem);
    if (Index >= 0) then
      UpdateListBoxItem(ListBox.ItemByIndex(Index), AItem);
  end;
end;

procedure TListBoxCollectionView.UpdateListBoxItem(const AListBoxItem: TListBoxItem; const AItem: TObject);
begin
  AListBoxItem.ItemData.Text   := Template.GetTitle(AItem);
  AListBoxItem.ItemData.Detail := Template.GetDetail(AItem);
  AListBoxItem.ImageIndex      := Template.GetImageIndex(AItem);
  AListBoxItem.Data            := AItem;
  AListBoxItem.StyleLookup     := Template.GetStyle(AItem);
end;

{ TListView }

destructor TListView.Destroy;
begin
  FView := nil;
  inherited;
end;

procedure TListView.DoChange;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, ['ItemIndex', 'Selected', 'SelectedItem']);
end;

function TListView.FindListViewItem(const AItem: TObject): Integer;
var
  I: Integer;
  Item: TListViewItem;
begin
  for I := 0 to ItemCount - 1 do
  begin
    Item := Items[I];
    if (Item.Tag = NativeInt(AItem)) then
      Exit(I);
  end;
  Result := -1;
end;

function TListView.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

function TListView.GetCollectionView: ICollectionView;
begin
  if (FView = nil) then
    FView := TListViewCollectionView.Create(Self);
  Result := FView;
end;

function TListView.GetSelectedItem: TObject;
var
  Sel: TListViewItem;
begin
  Sel := TListViewItem(Selected);
  if Assigned(Sel) then
    Result := TObject(Sel.Tag)
  else
    Result := nil;
end;

procedure TListView.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

procedure TListView.SetSelectedItem(const Value: TObject);
begin
  ItemIndex := FindListViewItem(Value);
end;

{ TListViewCollectionView }

procedure TListViewCollectionView.AddItemToView(const AItem: TObject);
var
  ListView: TListView;
  ListViewItem: TListViewItem;
begin
  ListView := FListView; // Strong reference
  if Assigned(ListView) then
  begin
    ListViewItem := ListView.Items.Add;
    UpdateListViewItem(ListViewItem, AItem);
  end;
end;

procedure TListViewCollectionView.BeginUpdateView;
var
  ListView: TListView;
begin
  ListView := FListView; // Strong reference
  if Assigned(ListView) then
    ListView.BeginUpdate;
end;

procedure TListViewCollectionView.ClearItemsInView;
var
  ListView: TListView;
begin
  ListView := FListView; // Strong reference
  if Assigned(ListView) then
    ListView.Items.Clear;
end;

constructor TListViewCollectionView.Create(const AListView: TListView);
begin
  Assert(Assigned(AListView));
  inherited Create;
  FListView := AListView;
end;

procedure TListViewCollectionView.DeleteItemFromView(const AItemIndex: Integer);
var
  ListView: TListView;
begin
  ListView := FListView; // Strong reference
  if Assigned(ListView) then
    ListView.DeleteItem(AItemIndex);
end;

procedure TListViewCollectionView.EndUpdateView;
var
  ListView: TListView;
begin
  ListView := FListView; // Strong reference
  if Assigned(ListView) then
    ListView.EndUpdate;
end;

function TListViewCollectionView.GetComponent: TComponent;
begin
  Result := FListView;
end;

procedure TListViewCollectionView.UpdateAllItemsInView;
var
  ListView: TListView;
  Item: TObject;
  ListViewItem: TListViewItem;
  Index: Integer;
begin
  ListView := FListView; // Strong reference
  if Assigned(ListView) then
  begin
    Index := 0;
    for Item in Source do
    begin
      ListViewItem := ListView.Items[Index];
      UpdateListViewItem(ListViewItem, Item);
      Inc(Index);
    end;
  end;
end;

procedure TListViewCollectionView.UpdateItemInView(const AItem: TObject; const APropertyName: String);
var
  ListView: TListView;
  Index: Integer;
begin
  ListView := FListView; // Strong reference
  if Assigned(ListView) then
  begin
    Index := ListView.FindListViewItem(AItem);
    if (Index >= 0) then
      UpdateListViewItem(ListView.Items[Index], AItem);
  end;
end;

procedure TListViewCollectionView.UpdateListViewItem(const AListViewItem: TListViewItem; const AItem: TObject);
begin
  AListViewItem.Text := Template.GetTitle(AItem);
  AListViewItem.Detail := Template.GetDetail(AItem);
  AListViewItem.ImageIndex := Template.GetImageIndex(AItem);
  AListViewItem.Tag := NativeInt(AItem);
end;

{ TImage }

procedure TImage.DoChanged;
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Bitmap');
  inherited;
end;

function TImage.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

procedure TImage.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

{ TTreeView }

procedure TTreeView.Change(Sender: TObject);
var
  Node: TTreeViewItem;
  tV: TTreeView;
begin
  tv := (Sender as TTreeView);
  if assigned( tv.selected) then
  begin
    Node := (Sender as TTreeView).selected;
    if Assigned( FOnChanged_) then
      FOnChanged_( node);
    DoSelectNode( Node, True);
  end;
end;

constructor TTreeView.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TTreeView.Destroy;
begin
  FView := nil;
  inherited;
end;

procedure TTreeView.DoSelectNode(Node: TTreeViewItem; Selected: Boolean);
begin
  if Selected and Assigned(FEstrategiaBinding) then
  begin
    FEstrategiaBinding.Notify(Self, ['Selected', 'SelectedNode']);
  end;
  inherited;
end;

function TTreeView.FindTreeNode(const AItem: TObject): TTreeViewItem;
var
  node: TTreeViewItem;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    node := Items[i];
    if node.TagObject = AItem then
      Result := node;
  end;
end;

function TTreeView.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

function TTreeView.GetCollectionView: ICollectionView;
begin
  if (FView = nil) then
    FView := TTreeViewCollectionView.Create(Self);

  Result := FView;
end;

function TTreeView.GetSelectedNode: TObject;
var
  Sel: TTreeViewItem;
begin
  Sel := Selected;
  if (Sel = nil) then
    Result := nil
  else
    Result := Sel.TagObject;
end;

procedure TTreeView.Loaded;
begin
  inherited;
  FOnChanged_ := OnChange;
  OnChange := Change;
end;

procedure TTreeView.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

procedure TTreeView.SetSelectedNode(const Value: TObject);
begin
  Selected := FindTreeNode(Value);
end;

{ TComboBox }

procedure TComboBox.Change(Sender: TObject);
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, 'Text');

  SetSelectedItem(Items.Objects[ItemIndex]);
  inherited;
end;

destructor TComboBox.Destroy;
begin
  FView := nil;
  inherited;
end;

function TComboBox.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding
end;

function TComboBox.GetCollectionView: ICollectionView;
begin
  if (FView = nil) then
    FView := TComboBoxCollectionView.Create(Self);

  Result := FView;
end;

function TComboBox.GetSelectedItem: TObject;
begin
  Result := Items.Objects[ItemIndex];
end;

procedure TComboBox.Loaded;
begin
  inherited;
  FChanged_ := onChange;
  onChange  := Change;
end;

procedure TComboBox.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

procedure TComboBox.SetSelectedItem(const Value: TObject);
begin
  if FSelectedItem <> Value then
  begin
    FSelectedItem := Value;

    if Assigned(FEstrategiaBinding) then
      FEstrategiaBinding.Notify(Self, 'SelectedItem');
  end;
end;

{ TComboBoxCollectionView }

procedure TComboBoxCollectionView.AddItemToView(const AItem: TObject);
var
  sItem: string;
begin
  if Assigned(FComboBox) then
  begin
    sItem := Template.GetTitle(AItem);
    FComboBox.Items.AddObject( sItem, AItem);
  end;
end;

procedure TComboBoxCollectionView.BeginUpdateView;
begin
  if Assigned(FComboBox) then
  begin
    FComboBox.Items.BeginUpdate;
  end;
end;

procedure TComboBoxCollectionView.ClearItemsInView;
begin
  if Assigned(FComboBox) then
  begin
    FComboBox.Items.Clear;
  end;
end;

constructor TComboBoxCollectionView.Create(const AComboBox: TComboBox);
begin
  Assert(Assigned(AComboBox));
  inherited Create;
  FComboBox := AComboBox;
end;

procedure TComboBoxCollectionView.DeleteItemFromView(const AItemIndex: Integer);
begin
  if Assigned(FComboBox) then
  begin
    FComboBox.Items.Delete( AItemIndex);
  end;
end;

procedure TComboBoxCollectionView.EndUpdateView;
begin
  if Assigned(FComboBox) then
  begin
    FComboBox.Items.EndUpdate;
  end;
end;

function TComboBoxCollectionView.GetComponent: TComponent;
begin
  Result := FComboBox
end;

procedure TComboBoxCollectionView.UpdateAllItemsInView;
var
  Item: TObject;
  sItem: string;
  Index: Integer;
begin
  if Assigned(FComboBox) then
  begin
    Index := 0;
    for Item in Source do
    begin
      sItem := FComboBox.Items.Strings[index];
      FComboBox.Items.Strings[Index] := Template.GetTitle( Item);
      FComboBox.Items.Objects[Index] := Item;
      Inc(Index);
    end;
  end;
end;

procedure TComboBoxCollectionView.UpdateItemInView(const AItem: TObject; const APropertyName: String);
var
  Index: Integer;
begin
  if Assigned(FComboBox) then
  begin
    Index := FComboBox.Items.IndexOfObject( AItem);
     if (Index >= 0) then
     begin
       FComboBox.Items.Strings[Index] := Template.GetTitle( AItem);
       FComboBox.Items.Objects[Index] := AItem;
     end;
  end;
end;

{ TTreeViewCollectionView }

procedure TTreeViewCollectionView.AddItemToView(const AItem: TObject);
var
  Item: TTreeViewItem;

  procedure Branch( Parent: TTreeViewItem; AItem: TObject );
  var
    Item: TTreeViewItem;
    obj: TObject;
  begin
    for obj in Template.GetChildren( AItem) do
    begin
      Item := TTreeViewItem.Create( FTreeView );
      Item.Parent := Parent;
      Item.Text := Template.GetTitle( obj);
      UpdateTreeNode(Item, obj);
      Branch( Item, obj);
    end;
  end;

begin
  Item := TTreeViewItem.Create( FTreeView );
  Item.Parent := FTreeView;
  Item.Text := Template.GetTitle( AItem);
  UpdateTreeNode(Item, AItem);
  Branch( Item, AItem);
end;

procedure TTreeViewCollectionView.BeginUpdateView;
begin
  FTreeView.BeginUpdate;
end;

procedure TTreeViewCollectionView.ClearItemsInView;
var
  //Item: TTreeViewItem;
  i: Integer;
begin
  for i := 0 to FtreeView.Count -1 do
  begin
    FtreeView.Items[ i].Free;
  end;
end;

constructor TTreeViewCollectionView.Create(const ATreeView: TTreeView);
begin
  Assert(Assigned(ATreeView));
  inherited Create;
  FTreeView := ATreeView;
end;

procedure TTreeViewCollectionView.DeleteItemFromView(const AItemIndex: Integer);
begin
  FTreeView.Items[ AItemIndex].Free;
end;

procedure TTreeViewCollectionView.EndUpdateView;
begin
  FTreeView.EndUpdate;
end;

function TTreeViewCollectionView.GetComponent: TComponent;
begin
  Result := FTreeView
end;

procedure TTreeViewCollectionView.UpdateAllItemsInView;
var
  Index: Integer;
  Item: TObject;
  Node: TTreeViewItem;
begin
  Index := 0;
  for Item in Source do
  begin
    Node := FTreeView.Items[ Index];
    UpdateTreeNode( Node, Item);
    Inc(Index);
  end;
end;

procedure TTreeViewCollectionView.UpdateItemInView(const AItem: TObject;
  const APropertyName: String);
var
  Node: TTreeViewItem;
begin
  Node := FTreeView.FindTreeNode(AItem);
  if (Node <> nil) then
    UpdateTreeNode(Node, AItem);
end;

procedure TTreeViewCollectionView.UpdateTreeNode(
  const ATreeViewItem: TTreeViewItem; const AItem: TObject);
begin
  ATreeViewItem.TagObject := TObject(AItem);
end;

end.
