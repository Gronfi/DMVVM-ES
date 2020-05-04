unit MVVM.Controls.Platform.FMX;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Classes,
  System.Threading,

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
  FMX.Grid,

  MVVM.Interfaces,
  MVVM.Types;

type
{$REGION 'FMX.StdCtrls'}
  { TCheckBox with support for light-weight two-way data binding.
    Supports property changed notifications for: IsChecked }
  TCheckBox = class(FMX.StdCtrls.TCheckBox, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FOrigOnChange: TNotifyEvent;
    FManager: IStrategyEventedObject;
    procedure HandleOnChange(Sender: TObject);
  protected
    procedure Loaded; override;

    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

  { TTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TTrackBar = class(FMX.StdCtrls.TTrackBar, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    procedure DoChanged; override;
    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

  { TSwitch with support for light-weight two-way data binding.
    Supports property changed notifications for: IsChecked }
  TSwitch = class(FMX.StdCtrls.TSwitch, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    procedure DoSwitch; override;
    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

  { TArcDial with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TArcDial = class(FMX.StdCtrls.TArcDial, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    procedure AfterChangedProc(Sender: TObject); override;
    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
{$ENDREGION 'FMX.StdCtrls'}
{$REGION 'FMX.ActnList'}

  TAction = class(FMX.ActnList.TAction, IBindableAction, IBindable)
{$REGION 'Internal Declarations'}
  private
    FBinding: IBinding;
    FExecute: TExecuteMethod;
    FCanExecute: TCanExecuteMethod;
    FAnomExecute: TExecuteAnonymous;

    FRttiUsage            : Boolean;
    FRttiExecute          : TExecuteRttiMethod;
    FRttiExecuteObj       : TObject;
    FRttiCanExecute       : TCanExecuteRttiMethod;
    FRttiCanExecuteObj    : TObject;
    FRttiParam            : TParamRtti;
    FRttiParamObj         : TObject;
    FRttiAfterExecuteDo   : TCanExecuteRttiMethod;
    FRttiAfterExecuteDoObj: TObject;

    FAfterExecuteDo: TProc<Boolean>;
    FTask          : ITask;

    FAsyncExecution: Boolean;
    FTimeOut       : Integer;

    FLastCanExecute: Boolean;
    FCanExecuteChangedEvent: ICanExecuteChangedEvent;
  protected
    procedure DoExecuteAnonymous;
    procedure DoExecuteRtti;

    function GetBinding: IBinding;
    procedure SetBinding(ABinding: IBinding);

    function GetAsyncExecution: Boolean;
     procedure SetAsyncExecution(const AValue: Boolean);

    function GetCanExecuteChanged: ICanExecuteChangedEvent;
{$ENDREGION 'Internal Declarations'}
  public
    procedure CancelAsyncExecution;

    { IBindableAction }
    procedure Bind(const AExecute: TExecuteMethod;
                   const ACanExecute: TCanExecuteMethod = nil); overload;
    procedure Bind<T>(const AExecute: TExecuteMethod<T>; const AParam: TParam<T>;
                      const ACanExecute: TCanExecuteMethod = nil); overload;
    procedure Bind(const AExecute: TExecuteAnonymous;
                   const ACanExecute: TCanExecuteMethod = nil); overload;
    procedure Bind(const AExecute: TExecuteRttiMethod; const AExecuteObj: TObject;
                   const ACanExecute: TCanExecuteRttiMethod = nil; const ACanExecuteObj: TObject = nil;
                   const AParams: TParamRtti = nil; const AParamObj: TObject = nil); overload;

    procedure BindAsync(const AExecute: TExecuteMethod;
                        const ACanExecute: TCanExecuteMethod = nil;
                        const AfterExecuteDo: TProc<Boolean> = nil;
                        const ATimeOut: Integer = Integer.MaxValue); overload;
    procedure BindAsync<T>(const AExecute: TExecuteMethod<T>; const AParam: TParam<T>;
                           const ACanExecute: TCanExecuteMethod = nil;
                           const AfterExecuteDo: TProc<Boolean> = nil;
                           const ATimeOut: Integer = Integer.MaxValue); overload;
    procedure BindAsync(const AExecute: TExecuteAnonymous;
                        const ACanExecute: TCanExecuteMethod = nil;
                        const AfterExecuteDo: TProc<Boolean> = nil;
                        const ATimeOut: Integer = Integer.MaxValue); overload;
    procedure BindAsync(const AExecute: TExecuteRttiMethod;
                        const ACanExecute: TCanExecuteMethod = nil;
                        const AfterExecuteDo: TProc<Boolean> = nil;
                        const ATimeOut: Integer = Integer.MaxValue); overload;
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;

    property Binding: IBinding read GetBinding write SetBinding;
    property OnCanExecuteChanged: ICanExecuteChangedEvent read GetCanExecuteChanged;
    property AsyncExecution: Boolean read GetAsyncExecution write SetAsyncExecution;
  end;
{$ENDREGION 'FMX.ActnList'}
{$REGION 'FMX.Edit'}

type
  { TEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Text
    Supports property changing notifications for: Text }
  TEdit = class(FMX.Edit.TEdit, INotifyChangedProperty,
    INotifyPropertyTrackingChanged)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    function DefineModelClass: TDataModelClass; override;

    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
    function GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
    property OnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent
      read GetOnPropertyChangedTrackingEvent;
  end;
{$ENDREGION 'FMX.Edit'}
{$REGION 'FMX.Memo'}

type
  { TMemo with support for light-weight two-way data binding.
    Supports property changed notifications for: Text
    Supports property changing notifications for: Text }
  TMemo = class(FMX.Memo.TMemo, INotifyChangedProperty,
    INotifyPropertyTrackingChanged)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    function DefineModelClass: TDataModelClass; override;

    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
    function GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
    property OnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent
      read GetOnPropertyChangedTrackingEvent;
  end;
{$ENDREGION 'FMX.Memo'}
{$REGION 'FMX.ComboEdit'}

type
  { TComboEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Text
    Supports property changing notifications for: Text }
  TComboEdit = class(FMX.ComboEdit.TComboEdit, INotifyChangedProperty,
    INotifyPropertyTrackingChanged)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    function DefineModelClass: TDataModelClass; override;

    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
    function GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
    property OnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent
      read GetOnPropertyChangedTrackingEvent;
  end;
{$ENDREGION 'FMX.ComboEdit'}
{$REGION 'FMX.Colors'}

type
  { TColorPanel with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TColorPanel = class(FMX.Colors.TColorPanel, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FOrigOnChange: TNotifyEvent;
    FManager: IStrategyEventedObject;
  private
    procedure HandleOnChange(Sender: TObject);

    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
  protected
    procedure Loaded; override;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

type
  { TComboColorBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TComboColorBox = class(FMX.Colors.TComboColorBox, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoColorChange(Sender: TObject); override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

type
  { TColorListBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TColorListBox = class(FMX.Colors.TColorListBox, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoChange; override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

type
  { TColorComboBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Color }
  TColorComboBox = class(FMX.Colors.TColorComboBox, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoChange; override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

type
  { THueTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  THueTrackBar = class(FMX.Colors.THueTrackBar, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoChanged; override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

type
  { TAlphaTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TAlphaTrackBar = class(FMX.Colors.TAlphaTrackBar, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoChanged; override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

type
  { TBWTrackBar with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TBWTrackBar = class(FMX.Colors.TBWTrackBar, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoChanged; override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
{$ENDREGION 'FMX.Colors'}
{$REGION 'FMX.DateTimeCtrls'}

type
  { TTimeEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Time }
  TTimeEdit = class(FMX.DateTimeCtrls.TTimeEdit, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoDateTimeChanged; override;
    procedure HandlerPickerDateTimeChanged(Sender: TObject;
      const ADate: TDateTime); override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;

type
  { TDateEdit with support for light-weight two-way data binding.
    Supports property changed notifications for: Date }
  TDateEdit = class(FMX.DateTimeCtrls.TDateEdit, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoDateTimeChanged; override;
    procedure HandlerPickerDateTimeChanged(Sender: TObject;
      const ADate: TDateTime); override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
{$ENDREGION 'FMX.DateTimeCtrls'}
{$REGION 'FMX.SpinBox'}

type
  { TSpinBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TSpinBox = class(FMX.SpinBox.TSpinBox, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
{$ENDREGION 'FMX.SpinBox'}
{$REGION 'FMX.NumberBox'}

type
  { TNumberBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Value }
  TNumberBox = class(FMX.NumberBox.TNumberBox, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    function DefineModelClass: TDataModelClass; override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
{$ENDREGION 'FMX.NumberBox'}

{$REGION 'FMX.ListBox'}
type
  { TListBox with support for light-weight two-way data binding.
    Supports property changed notifications for: ItemIndex, Selected, SelectedItem.
    NOTE: When used with data binding, the TListBoxItem.Data property is used
    to store the associated object. }
  TListBox = class(FMX.ListBox.TListBox, ICollectionViewProvider,
    INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
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
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    { Destructor }
    destructor Destroy; override;

    procedure Clear; override;

    { The object that is associated with the selected item, or nil if there is
      no item selected or there is no object associated with the selected item.
      The associated object is the object in the TListBoxItem.Data property. }
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;

    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
{$ENDREGION 'FMX.ListBox'}
{$REGION 'FMX.ListView'}

  { TListView with support for light-weight two-way data binding.
    Supports property changed notifications for: ItemIndex, Selected, SelectedItem
    NOTE: When used with data binding, the TListViewItem.Tag property is used
    to store the associated object. This is an unsafe reference, so you
    must make sure that the associated objects are available for the
    lifetime of the list view. }
(*
  TListView = class(FMX.ListView.TListView, ICollectionViewProvider,
    INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
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
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
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
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
*)
{$ENDREGION 'FMX.ListView'}
{$REGION 'FMX.Objects'}

type
  { TImage with support for light-weight two-way data binding.
    Supports property changed notifications for: Bitmap }
  TImage = class(FMX.Objects.TImage, INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
  protected
    procedure DoChanged; override;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
{$ENDREGION 'Internal Declarations'}
  public
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
{$ENDREGION 'FMX.Objects'}
{$REGION 'FMX.TreeView'}

type
  TTreeView = class(FMX.TreeView.TTreeView, ICollectionViewProvider,
    INotifyChangedProperty)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
    FView: ICollectionView;
    FOnChanged_: TNotifyEvent;
    function GetSelectedNode: TObject; inline;
    procedure SetSelectedNode(const Value: TObject);
  private
    function FindTreeNode(const AItem: TObject): TTreeViewItem;
  protected
    procedure Loaded; override;
    procedure Change(Sender: TObject);
    procedure DoSelectNode(Node: TTreeViewItem; Selected: Boolean);
  protected
    { IgoCollectionViewProvider }
    function GetCollectionView: ICollectionView;
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
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
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
  end;
{$ENDREGION 'FMX.TreeView'}
{$REGION 'FMX.ComboBox'}

  { TComboBox with support for light-weight two-way data binding.
    Supports property changed notifications for: Text
    Supports property changing notifications for: Text
    NOTE: Both PropertyChanged and PropertyChangeTracking notifications are
    fired for each individual keypress. }
  TComboBox = class(FMX.ListBox.TComboBox, INotifyChangedProperty,
    ICollectionViewProvider, INotifyPropertyTrackingChanged)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
    FView: ICollectionView;

    FSelectedItem: TObject;
    function GetSelectedItem: TObject; inline;
    procedure SetSelectedItem(const Value: TObject);
  protected
    FChanged_: TNotifyEvent;
    procedure Loaded; override;
    procedure Change(Sender: TObject);
  protected
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
    function GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;

    function GetCollectionView: ICollectionView;
{$ENDREGION 'Internal Declarations'}
  public
    { Destructor }
    destructor Destroy; override;
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property Manager: IStrategyEventedObject read GetManager write SetManager;
    property OnPropertyChangedEvent: IChangedPropertyEvent
      read GetOnPropertyChangedEvent;
    property OnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent
      read GetOnPropertyChangedTrackingEvent;
  end;
{$ENDREGION 'FMX.ComboBox'}

{$REGION 'TGrid'}
(*  TGrid = class(FMX.Grid.TGrid)

  end;
*)
{$ENDREGION}

implementation

uses
  FMX.Forms,
  FMX.Graphics,
  FMX.ListView.Appearances,

  Spring,

  MVVM.Utils,
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
    [weak]
    FListBox: TListBox;
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

(*
  TListViewCollectionView = class(TCollectionView)
  private
    [weak]
    FListView: TListView;
  private
    procedure UpdateListViewItem(const AListViewItem: TListViewItem;
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
    constructor Create(const AListView: TListView);
  end;
*)

  TTreeViewCollectionView = class(TCollectionView)
  private
    FTreeView: TTreeView;
  private
    procedure UpdateTreeNode(const ATreeViewItem: TTreeViewItem;
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

procedure TAction.Bind(const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod);
begin
  FExecute    := AExecute;
  FCanExecute := ACanExecute;
  FRttiUsage  := False;
end;

procedure TAction.Bind(const AExecute: TExecuteRttiMethod; const AExecuteObj: TObject;
                       const ACanExecute: TCanExecuteRttiMethod; const ACanExecuteObj: TObject;
                       const AParams: TParamRtti; const AParamObj: TObject);
begin
  FExecute           := DoExecuteRtti;
  FRttiExecuteObj    := AExecuteObj;
  FRttiExecute       := AExecute;
  FRttiCanExecuteObj := ACanExecuteObj;
  FRttiCanExecute    := ACanExecute;
  FRttiParam         := AParams;
  FRttiParamObj      := AParamObj;
  FRttiUsage         := True;
end;

procedure TAction.Bind(const AExecute: TExecuteAnonymous; const ACanExecute: TCanExecuteMethod);
begin
  FExecute    := DoExecuteAnonymous;
  FAnomExecute:= AExecute;
  FCanExecute := ACanExecute;
  FRttiUsage  := False;
end;

procedure TAction.Bind<T>(const AExecute: TExecuteMethod<T>; const AParam: TParam<T>; const ACanExecute: TCanExecuteMethod);
begin
  FExecute     := DoExecuteAnonymous;
  FAnomExecute := procedure
                  begin
                    AExecute(AParam);
                  end;
  FCanExecute  := ACanExecute;
  FRttiUsage   := False;
end;

procedure TAction.BindAsync(const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod; const AfterExecuteDo: TProc<Boolean>; const ATimeOut: Integer);
begin
  FAsyncExecution := True;
  FTimeOut := ATimeOut;
  FExecute    := AExecute;
  FCanExecute := ACanExecute;
end;

procedure TAction.BindAsync(const AExecute: TExecuteAnonymous; const ACanExecute: TCanExecuteMethod; const AfterExecuteDo: TProc<Boolean>; const ATimeOut: Integer);
begin
  FAsyncExecution := True;
  FTimeOut    := ATimeOut;
  FExecute    := DoExecuteAnonymous;
  FAnomExecute:= AExecute;
  FCanExecute := ACanExecute;
end;

procedure TAction.BindAsync(const AExecute: TExecuteRttiMethod; const ACanExecute: TCanExecuteMethod; const AfterExecuteDo: TProc<Boolean>; const ATimeOut: Integer);
begin
  FAsyncExecution := True;
  FTimeOut := ATimeOut;
  FExecute    := DoExecuteRtti;
  FRttiExecute:= AExecute;
  FCanExecute := ACanExecute;
end;

procedure TAction.BindAsync<T>(const AExecute: TExecuteMethod<T>; const AParam: TParam<T>; const ACanExecute: TCanExecuteMethod; const AfterExecuteDo: TProc<Boolean>; const ATimeOut: Integer);
begin
  FAsyncExecution := True;
  FTimeOut := ATimeOut;
  FExecute     := DoExecuteAnonymous;
  FAnomExecute := procedure
                  begin
                    AExecute(AParam);
                  end;
  FCanExecute  := ACanExecute;
end;

procedure TAction.CancelAsyncExecution;
begin
  if Assigned(FTask) then
    FTask.Cancel;
end;

constructor TAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := False;
  FLastCanExecute    := True;

  FRttiUsage      := False;
  FAsyncExecution := False;
end;

procedure TAction.DoExecuteAnonymous;
begin
  if Assigned(FAnomExecute) then
    FAnomExecute();
end;

procedure TAction.DoExecuteRtti;
var
  LProc: TExecuteMethod;
  LValue: TValue;
  LRes: Boolean;
begin
  if Assigned(FRttiExecute) then
  begin
    if Assigned(FRttiParam) then
      FRttiExecute.Invoke(fRttiExecuteObj, [FRttiParam.Invoke(FRttiParamObj, [])])
    else begin
           LValue := FRttiExecute.Invoke(fRttiExecuteObj, []);
           if not LValue.IsObject then
           begin
             LProc  := LValue.AsType<TExecuteMethod>();
             LProc;
           end;
         end;
  end;
end;

function TAction.Execute: Boolean;
begin
  Result := inherited Execute;
  if (Supported) and (not Suspended) and (Enabled) then
  begin
    if Assigned(FExecute) then
    begin
      case FAsyncExecution of
        True:
          begin
            TThread.CreateAnonymousThread(
              procedure
              var
                LRes : Boolean;
              begin
                FTask := TTask.Create(FExecute).Start;
                LRes  := FTask.Wait(FTimeOut);
                FTask := nil;
                TThread.Queue(nil,
                  procedure
                  begin
                    FAfterExecuteDo(LRes);
                  end);
              end).Start;
          end;
        False:
          begin
            FExecute();
          end;
      end;
    end;
  end;
end;

function TAction.GetAsyncExecution: Boolean;
begin
  Result := FAsyncExecution
end;

function TAction.GetBinding: IBinding;
begin
  Result := FBinding;
end;

function TAction.GetCanExecuteChanged: ICanExecuteChangedEvent;
begin
  if not Assigned(FCanExecuteChangedEvent) then
    FCanExecuteChangedEvent := Utils.CreateEvent<TCanExecuteChangedEvent>;
  Result := FCanExecuteChangedEvent;
end;

procedure TAction.SetAsyncExecution(const AValue: Boolean);
begin
  FAsyncExecution := AValue
end;

procedure TAction.SetBinding(ABinding: IBinding);
begin
  FBinding := ABinding;
end;

function TAction.Update: Boolean;
var
  LFunc : TCanExecuteMethod;
  LValue: TValue;
  LEnabled: Boolean;
begin
  Result := inherited Update;
  if (Supported) then
  begin
    case FRttiUsage of
      True:
        begin
         if Assigned(FRttiCanExecute) then
         begin
           LValue := FRttiCanExecute.Invoke(FRttiCanExecuteObj, []);
           if not LValue.TryAsType<Boolean>(LEnabled) then
           begin
             LFunc  := LValue.AsType<TCanExecuteMethod>();
             Enabled:= LFunc;
           end
           else Enabled := LEnabled;
         end;
        end;
      False:
        begin
          if Assigned(FCanExecute) then
            Enabled := FCanExecute();
        end;
    end;
  end;
  if Enabled <> FLastCanExecute then
  begin
    FLastCanExecute := Enabled;
    if Assigned(FCanExecuteChangedEvent) then
      FCanExecuteChangedEvent.Invoke(Self, FLastCanExecute);
  end;
end;

{ TCheckBox }

function TCheckBox.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TCheckBox.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TCheckBox.HandleOnChange(Sender: TObject);
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'IsChecked');
    FManager.BindingStrategy.Notify(Self, 'IsChecked');
  end;
  if Assigned(FOrigOnChange) then
    FOrigOnChange(Sender);
end;

procedure TCheckBox.Loaded;
begin
  inherited;
  FOrigOnChange := OnChange;
  OnChange := HandleOnChange;
end;

procedure TCheckBox.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TTrackBar }

procedure TTrackBar.DoChanged;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Value');
    FManager.BindingStrategy.Notify(Self, 'Value');
  end;
  inherited;
end;

function TTrackBar.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TTrackBar.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TTrackBar.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TSwitch }

procedure TSwitch.DoSwitch;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'IsChecked');
    FManager.BindingStrategy.Notify(Self, 'IsChecked');
  end;
  inherited;
end;

function TSwitch.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TSwitch.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TSwitch.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TArcDial }

procedure TArcDial.AfterChangedProc(Sender: TObject);
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Value');
    FManager.BindingStrategy.Notify(Self, 'Value');
  end;
  inherited;
end;

function TArcDial.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TArcDial.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TArcDial.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TEdit }

function TEdit.DefineModelClass: TDataModelClass;
begin
  Result := TBindableEditModel;
end;

function TEdit.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TEdit.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

function TEdit.GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedTrackingEvent;
end;

procedure TEdit.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
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
    if Assigned(LEdit.FManager) then
    begin
      if LEdit.FManager.IsAssignedPropertyChangedEvent then
        LEdit.FManager.OnPropertyChangedEvent.Invoke(LEdit, 'Text');
      LEdit.FManager.BindingStrategy.Notify(LEdit, 'Text');
    end;
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
    if Assigned(LEdit.FManager) then
    begin
      if LEdit.FManager.IsAssignedPropertyChangedTrackingEvent then
        LEdit.FManager.OnPropertyChangedTrackingEvent.Invoke(LEdit, 'Text');
      LEdit.FManager.BindingStrategy.Notify(LEdit, 'Text');
    end;
  end;
end;

{ TMemo }

function TMemo.DefineModelClass: TDataModelClass;
begin
  Result := TBindableMemoModel;
end;

function TMemo.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TMemo.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

function TMemo.GetOnPropertyChangedTrackingEvent: IPropertyChangedTrackingEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedTrackingEvent;
end;

procedure TMemo.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
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
    if Assigned(LMemo.FManager) then
    begin
      if LMemo.FManager.IsAssignedPropertyChangedEvent then
        LMemo.FManager.OnPropertyChangedEvent.Invoke(LMemo, 'Text');
      LMemo.FManager.BindingStrategy.Notify(LMemo, 'Text');
    end;
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
    if Assigned(LMemo.FManager) then
    begin
      if LMemo.FManager.IsAssignedPropertyChangedTrackingEvent then
        LMemo.FManager.OnPropertyChangedTrackingEvent.Invoke(LMemo, 'Text');
      LMemo.FManager.BindingStrategy.Notify(LMemo, 'Text');
    end;
  end;
end;

{ TComboEdit }

function TComboEdit.DefineModelClass: TDataModelClass;
begin
  Result := TBindableComboEditModel;
end;

function TComboEdit.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TComboEdit.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

function TComboEdit.GetOnPropertyChangedTrackingEvent
  : IPropertyChangedTrackingEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedTrackingEvent;
end;

procedure TComboEdit.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
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
    if Assigned(LComboEdit.FManager) then
    begin
      if LComboEdit.FManager.IsAssignedPropertyChangedEvent then
        LComboEdit.FManager.OnPropertyChangedEvent.Invoke(LComboEdit, 'Text');
      LComboEdit.FManager.BindingStrategy.Notify(LComboEdit, 'Text');
    end;
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
    if Assigned(LComboEdit.FManager) then
    begin
      if LComboEdit.FManager.IsAssignedPropertyChangedTrackingEvent then
        LComboEdit.FManager.OnPropertyChangedTrackingEvent.Invoke(LComboEdit, 'Text');
      LComboEdit.FManager.BindingStrategy.Notify(LComboEdit, 'Text');
    end;
  end;
end;

{ TColorPanel }

function TColorPanel.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TColorPanel.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TColorPanel.HandleOnChange(Sender: TObject);
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Color');
    FManager.BindingStrategy.Notify(Self, 'Color');
  end;
  if Assigned(FOrigOnChange) then
    FOrigOnChange(Sender);
end;

procedure TColorPanel.Loaded;
begin
  inherited;
  FOrigOnChange := OnChange;
  OnChange := HandleOnChange;
end;

procedure TColorPanel.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TComboColorBox }

procedure TComboColorBox.DoColorChange(Sender: TObject);
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Color');
    FManager.BindingStrategy.Notify(Self, 'Color');
  end;
  inherited;
end;

function TComboColorBox.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TComboColorBox.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TComboColorBox.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TColorListBox }

procedure TColorListBox.DoChange;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Color');
    FManager.BindingStrategy.Notify(Self, 'Color');
  end;
  inherited;
end;

function TColorListBox.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TColorListBox.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TColorListBox.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TColorComboBox }

procedure TColorComboBox.DoChange;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Color');
    FManager.BindingStrategy.Notify(Self, 'Color');
  end;
  inherited;
end;

function TColorComboBox.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TColorComboBox.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TColorComboBox.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ THueTrackBar }

procedure THueTrackBar.DoChanged;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Value');
    FManager.BindingStrategy.Notify(Self, 'Value');
  end;
  inherited;
end;

function THueTrackBar.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function THueTrackBar.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure THueTrackBar.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TAlphaTrackBar }

procedure TAlphaTrackBar.DoChanged;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Value');
    FManager.BindingStrategy.Notify(Self, 'Value');
  end;
  inherited;
end;

function TAlphaTrackBar.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TAlphaTrackBar.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TAlphaTrackBar.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TBWTrackBar }

procedure TBWTrackBar.DoChanged;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Value');
    FManager.BindingStrategy.Notify(Self, 'Value');
  end;
  inherited;
end;

function TBWTrackBar.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TBWTrackBar.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TBWTrackBar.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TTimeEdit }

procedure TTimeEdit.DoDateTimeChanged;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Time');
    FManager.BindingStrategy.Notify(Self, 'Time');
  end;
  inherited;
end;

function TTimeEdit.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TTimeEdit.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TTimeEdit.HandlerPickerDateTimeChanged(Sender: TObject;
  const ADate: TDateTime);
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

procedure TTimeEdit.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TDateEdit }

procedure TDateEdit.DoDateTimeChanged;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Date');
    FManager.BindingStrategy.Notify(Self, 'Date');
  end;
  inherited;
end;

function TDateEdit.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TDateEdit.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TDateEdit.HandlerPickerDateTimeChanged(Sender: TObject;
  const ADate: TDateTime);
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

procedure TDateEdit.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TSpinBox }

function TSpinBox.DefineModelClass: TDataModelClass;
begin
  Result := TBindableSpinBoxModel;
end;

function TSpinBox.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TSpinBox.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TSpinBox.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TBindableSpinBoxModel }

procedure TBindableSpinBoxModel.DoChange;
var
  Owner: TComponent;
  LSpinBox: TSpinBox absolute Owner;
begin
  Owner := Self.Owner; // Strong reference
  if (Owner <> nil) then
  begin
    Assert(Owner is TSpinBox);
    if Assigned(LSpinBox.FManager) then
    begin
      if LSpinBox.FManager.IsAssignedPropertyChangedEvent then
        LSpinBox.FManager.OnPropertyChangedEvent.Invoke(Self, 'Value');
      LSpinBox.FManager.BindingStrategy.Notify(Self, 'Value');
    end;
  end;
  inherited;
end;

{ TNumberBox }

function TNumberBox.DefineModelClass: TDataModelClass;
begin
  Result := TBindableNumberBoxModel;
end;

function TNumberBox.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TNumberBox.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TNumberBox.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
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
    if Assigned(LNumberBox.FManager) then
    begin
      if LNumberBox.FManager.IsAssignedPropertyChangedEvent then
        LNumberBox.FManager.OnPropertyChangedEvent.Invoke(Self, 'Value');
      LNumberBox.FManager.BindingStrategy.Notify(Self, 'Value');
    end;
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
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
    begin
      FManager.OnPropertyChangedEvent.Invoke(Self, 'ItemIndex');
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Selected');
      FManager.OnPropertyChangedEvent.Invoke(Self, 'SelectedItem');
    end;
    FManager.BindingStrategy.Notify(Self, ['ItemIndex', 'Selected',
      'SelectedItem']);
  end;
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

function TListBox.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TListBox.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
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

procedure TListBox.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
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

procedure TListBoxCollectionView.UpdateItemInView(const AItem: TObject;
  const APropertyName: String);
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

procedure TListBoxCollectionView.UpdateListBoxItem(const AListBoxItem
  : TListBoxItem; const AItem: TObject);
begin
  AListBoxItem.ItemData.Text := Template.GetTitle(AItem);
  AListBoxItem.ItemData.Detail := Template.GetDetail(AItem);
  AListBoxItem.ImageIndex := Template.GetImageIndex(AItem);
  AListBoxItem.Data := AItem;
  AListBoxItem.StyleLookup := Template.GetStyle(AItem);
end;
{ TListView }
(*
destructor TListView.Destroy;
begin
  FView := nil;
  inherited;
end;

procedure TListView.DoChange;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
    begin
      FManager.OnPropertyChangedEvent.Invoke(Self, 'ItemIndex');
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Selected');
      FManager.OnPropertyChangedEvent.Invoke(Self, 'SelectedItem');
    end;
    FManager.BindingStrategy.Notify(Self, ['ItemIndex', 'Selected',
      'SelectedItem']);
  end;
  inherited;
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

function TListView.GetCollectionView: ICollectionView;
begin
  if (FView = nil) then
    FView := TListViewCollectionView.Create(Self);
  Result := FView;
end;

function TListView.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TListView.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
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

procedure TListView.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
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

procedure TListViewCollectionView.UpdateItemInView(const AItem: TObject;
  const APropertyName: String);
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

procedure TListViewCollectionView.UpdateListViewItem(const AListViewItem
  : TListViewItem; const AItem: TObject);
begin
  AListViewItem.Text := Template.GetTitle(AItem);
  AListViewItem.Detail := Template.GetDetail(AItem);
  AListViewItem.ImageIndex := Template.GetImageIndex(AItem);
  AListViewItem.Tag := NativeInt(AItem);
end;
*)
{ TImage }

procedure TImage.DoChanged;
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
    begin
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Bitmap');
    end;
    FManager.BindingStrategy.Notify(Self, 'Bitmap');
  end;
  inherited;
end;

function TImage.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TImage.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

procedure TImage.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TTreeView }

procedure TTreeView.Change(Sender: TObject);
var
  Node: TTreeViewItem;
  tV: TTreeView;
begin
  tV := (Sender as TTreeView);
  if Assigned(tV.Selected) then
  begin
    Node := (Sender as TTreeView).Selected;
    if Assigned(FOnChanged_) then
      FOnChanged_(Node);
    DoSelectNode(Node, True);
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
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
    begin
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Selected');
      FManager.OnPropertyChangedEvent.Invoke(Self, 'SelectedItem');
    end;
    FManager.BindingStrategy.Notify(Self, ['Selected', 'SelectedItem']);
  end;
  inherited;
end;

function TTreeView.FindTreeNode(const AItem: TObject): TTreeViewItem;
var
  Node: TTreeViewItem;
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Node := Items[I];
    if Node.TagObject = AItem then
      Result := Node;
  end;
end;

function TTreeView.GetCollectionView: ICollectionView;
begin
  if (FView = nil) then
    FView := TTreeViewCollectionView.Create(Self);

  Result := FView;
end;

function TTreeView.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TTreeView.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
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

procedure TTreeView.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

procedure TTreeView.SetSelectedNode(const Value: TObject);
begin
  Selected := FindTreeNode(Value);
end;

{ TComboBox }

procedure TComboBox.Change(Sender: TObject);
begin
  if Assigned(FManager) then
  begin
    if FManager.IsAssignedPropertyChangedEvent then
      FManager.OnPropertyChangedEvent.Invoke(Self, 'Text');
    if FManager.IsAssignedPropertyChangedTrackingEvent then
      FManager.OnPropertyChangedTrackingEvent.Invoke(Self, 'Text');
    FManager.BindingStrategy.Notify(Self, 'Text');
  end;

  SetSelectedItem(Items.Objects[ItemIndex]);
  inherited;
end;

destructor TComboBox.Destroy;
begin
  FView := nil;
  inherited;
end;

function TComboBox.GetCollectionView: ICollectionView;
begin
  if (FView = nil) then
    FView := TComboBoxCollectionView.Create(Self);

  Result := FView;
end;

function TComboBox.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result := FManager;
end;

function TComboBox.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedEvent;
end;

function TComboBox.GetOnPropertyChangedTrackingEvent
  : IPropertyChangedTrackingEvent;
begin
  Guard.CheckNotNull(FManager, 'Manager is null');
  Result := FManager.OnPropertyChangedTrackingEvent;
end;

function TComboBox.GetSelectedItem: TObject;
begin
  Result := Items.Objects[ItemIndex];
end;

procedure TComboBox.Loaded;
begin
  inherited;
  FChanged_ := OnChange;
  OnChange := Change;
end;

procedure TComboBox.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

procedure TComboBox.SetSelectedItem(const Value: TObject);
begin
  if FSelectedItem <> Value then
  begin
    FSelectedItem := Value;

    if Assigned(FManager) then
    begin
      if FManager.IsAssignedPropertyChangedEvent then
        FManager.OnPropertyChangedEvent.Invoke(Self, 'SelectedItem');
      FManager.BindingStrategy.Notify(Self, 'SelectedItem');
    end;
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
    FComboBox.Items.AddObject(sItem, AItem);
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
    FComboBox.Items.Delete(AItemIndex);
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
      FComboBox.Items.Strings[Index] := Template.GetTitle(Item);
      FComboBox.Items.Objects[Index] := Item;
      Inc(Index);
    end;
  end;
end;

procedure TComboBoxCollectionView.UpdateItemInView(const AItem: TObject;
  const APropertyName: String);
var
  Index: Integer;
begin
  if Assigned(FComboBox) then
  begin
    Index := FComboBox.Items.IndexOfObject(AItem);
    if (Index >= 0) then
    begin
      FComboBox.Items.Strings[Index] := Template.GetTitle(AItem);
      FComboBox.Items.Objects[Index] := AItem;
    end;
  end;
end;

{ TTreeViewCollectionView }

procedure TTreeViewCollectionView.AddItemToView(const AItem: TObject);
var
  Item: TTreeViewItem;

  procedure Branch(Parent: TTreeViewItem; AItem: TObject);
  var
    Item: TTreeViewItem;
    obj: TObject;
  begin
    for obj in Template.GetChildren(AItem) do
    begin
      Item := TTreeViewItem.Create(FTreeView);
      Item.Parent := Parent;
      Item.Text := Template.GetTitle(obj);
      UpdateTreeNode(Item, obj);
      Branch(Item, obj);
    end;
  end;

begin
  Item := TTreeViewItem.Create(FTreeView);
  Item.Parent := FTreeView;
  Item.Text := Template.GetTitle(AItem);
  UpdateTreeNode(Item, AItem);
  Branch(Item, AItem);
end;

procedure TTreeViewCollectionView.BeginUpdateView;
begin
  FTreeView.BeginUpdate;
end;

procedure TTreeViewCollectionView.ClearItemsInView;
var
  I: Integer;
begin
  for I := 0 to FTreeView.Count - 1 do
  begin
    FTreeView.Items[I].Free;
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
  FTreeView.Items[AItemIndex].Free;
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
    Node := FTreeView.Items[Index];
    UpdateTreeNode(Node, Item);
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

procedure TTreeViewCollectionView.UpdateTreeNode(const ATreeViewItem
  : TTreeViewItem; const AItem: TObject);
begin
  ATreeViewItem.TagObject := TObject(AItem);
end;

end.
