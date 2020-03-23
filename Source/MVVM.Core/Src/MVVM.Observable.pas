unit MVVM.Observable;

interface

uses
  System.Types,
  System.Generics.Collections,
  System.Generics.Defaults,

  Spring,

  MVVM.Types,
  MVVM.Interfaces;

type
  TObservable = class abstract(TInterfacedObject, INotifyChangedProperty, INotifyFree)
  protected
    FManager: IStrategyEventedObject;
{$REGION 'Internal Declarations'}
    procedure NotifyFree;

    function GetOnFreeEvent: IFreeEvent;
    function GetOnPropertyChangedEvent: IChangedPropertyEvent;

    procedure PropertyChanged(const APropertyName: String); virtual;

    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);
{$ENDREGION 'Internal Declarations'}
  public
    constructor Create(AManager: IStrategyEventedObject); overload;
    { The destructor also sends a free notification to its subscribers. }
    destructor Destroy; override;

    property OnFreeEvent: IFreeEvent read GetOnFreeEvent;
    property OnPropertyChangedEvent: IChangedPropertyEvent read GetOnPropertyChangedEvent;

    property Manager: IStrategyEventedObject read GetManager write SetManager;
  end;

  TObservableCollection<T: class> = class(TEnumerable<T>, INotifyChangedProperty, INotifyCollectionChanged)
{$REGION 'Internal Declarations'}
  private
    FManager: IStrategyEventedObject;
    FList: TObjectList<T>;

    // FOnPropertyChanged: IgoPropertyChangedEvent;
    // FOnCollectionChanged: IgoCollectionChangedEvent;

    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetItem(const AIndex: Integer): T; inline;
    procedure SetItem(const AIndex: Integer; const Value: T); inline;
  private
    function GetManager: IStrategyEventedObject;
    procedure SetManager(AManager: IStrategyEventedObject);

    function GetOnPropertyChangedEvent: IChangedPropertyEvent;
    function GetOnCollectionChangedEvent: IChangedCollectionEvent;

    procedure DoItemPropertyChanged(const ASender: TObject; const APropertyName: String);
    procedure DoPropertyChanged(const APropertyName: String);
    procedure DoCollectionChanged(const AAction: TCollectionChangedAction; const AItem: TObject = nil; const AItemIndex: Integer = -1; const APropertyName: String = '');
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

{$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AOwnsObjects: Boolean = False);
    destructor Destroy; override;
  public
    { TEnumerable<T> }
    { Copies the elements in the collection to a dynamic array }
    function ToArray: TArray<T>; override; final;
    { Allow <tt>for..in</tt> enumeration of the collection. }
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    { Checks whether the collection contains a given item.
      This method performs a O(n) linear search and uses the collection's
      comparer to check for equality. For a faster check, use BinarySearch.
      Parameters:
      AItem: The item to check.
      Returns:
      True if the collection contains AValue. }
    function Contains(const AItem: T): Boolean; inline;
    { Returns the index of a given item or -1 if not found.
      This method performs a O(n) linear search and uses the collection's
      comparer to check for equality. For a faster check, use BinarySearch.
      Parameters:
      AItem: The item to find. }
    function IndexOf(const AItem: T): Integer; inline;
    { Returns the last index of a given item or -1 if not found.
      This method performs a O(n) backwards linear search and uses the
      collection's comparer to check for equality. For a faster check, use
      BinarySearch.
      Parameters:
      AItem: The item to find. }
    function LastIndexOf(const AItem: T): Integer; inline;
    { Returns the index of a given item or -1 if not found.
      This method performs a O(n) linear search and uses the collection's
      comparer to check for equality. For a faster check, use BinarySearch.
      Parameters:
      AItem: The item to find.
      ADirection: Whether to search forwards or backwards. }
    function IndexOfItem(const AItem: T; const ADirection: TDirection): Integer; inline;
    { Performs a binary search for a given item. This requires that the
      collection is sorted. This is an O(log n) operation that uses the default
      comparer to check for equality.
      Parameters:
      AItem: The item to find.
      AIndex: is set to the index of AItem if found. If not found, it is set
      to the index of the first entry larger than AItem.
      Returns:
      Whether the collection contains the item. }
    function BinarySearch(const AItem: T; out AIndex: Integer): Boolean; overload; inline;
    { Performs a binary search for a given item. This requires that the
      collection is sorted. This is an O(log n) operation that uses the given
      comparer to check for equality.
      Parameters:
      AItem: The item to find.
      AIndex: is set to the index of AItem if found. If not found, it is set
      to the index of the first entry larger than AItem.
      AComparer: the comparer to use to check for equality.
      Returns:
      Whether the collection contains the item. }
    function BinarySearch(const AItem: T; out AIndex: Integer; const AComparer: IComparer<T>): Boolean; overload; inline;
    { Returns the first item in the collection. }
    function First: T; inline;
    { Returns the last item in the collection. }
    function Last: T; inline;
    { Clears the collection }
    procedure Clear;
    { Adds an item to the end of the collection.
      Parameters:
      AItem: the item to add.
      Returns:
      The index of the added item. }
    function Add(const AItem: T): Integer;
    { Adds a range of items to the end of the collection.
      Parameters:
      AItems: an array of items to add. }
    procedure AddRange(const AItems: array of T); overload;
    { Adds the items of another collection to the end of the collection.
      Parameters:
      ACollection: the collection containing the items to add. Can be any
      class that descends from TEnumerable<T>. }
    procedure AddRange(const ACollection: TEnumerable<T>); overload;
    { Inserts an item into the collection.
      Parameters:
      AIndex: the index in the collection to insert the item. The item will be
      inserted before AIndex. Set to 0 to insert at the beginning to the
      collection. Set to Count to add to the end of the collection.
      AItem: the item to insert. }
    procedure Insert(const AIndex: Integer; const AItem: T);
    { Inserts a range of items into the collection.
      Parameters:
      AIndex: the index in the collection to insert the items. The items will
      be inserted before AIndex. Set to 0 to insert at the beginning to the
      collection. Set to Count to add to the end of the collection.
      AItems: the items to insert. }
    procedure InsertRange(const AIndex: Integer; const AItems: array of T); overload;
    { Inserts the items from another collection into the collection.
      Parameters:
      AIndex: the index in the collection to insert the items. The items will
      be inserted before AIndex. Set to 0 to insert at the beginning to the
      collection. Set to Count to add to the end of the collection.
      ACollection: the collection containing the items to insert. Can be any
      class that descends from TEnumerable<T>. }
    procedure InsertRange(const AIndex: Integer; const ACollection: TEnumerable<T>); overload;
    { Deletes an item from the collection.
      Parameters:
      AIndex: the index of the item to delete }
    procedure Delete(const AIndex: Integer);
    { Deletes a range of items from the collection.
      Parameters:
      AIndex: the index of the first item to delete
      ACount: the number of items to delete }
    procedure DeleteRange(const AIndex, ACount: Integer);
    { Removes an item from the collection.
      Parameters:
      AItem: the item to remove. It this collection does not contain this
      item, nothing happens.
      Returns:
      The index of the removed item, or -1 of the collection does not contain
      AItem.
      If the collection contains multiple items with the same value, only the
      first item is removed. }
    function Remove(const AItem: T): Integer;
    { Reverses the order of the elements in the collection. }
    procedure Reverse;
    { Sort the collection using the default comparer for the element type }
    procedure Sort; overload;
    { Sort the collection using a custom comparer.
      Parameters:
      AComparer: the comparer to use to sort the collection. }
    procedure Sort(const AComparer: IComparer<T>); overload;
    { Trims excess memory used by the collection. To improve performance and
      reduce memory reallocations, the collection usually contains space for
      more items than are actually stored in this collection. That is,
      Capacity >= Count. Call this method free that excess memory. You can do
      this when you are done filling the collection to free memory. }
    procedure TrimExcess; inline;
    { The number of items in the collection }
    property Count: Integer read GetCount;
    { The items in the collection }
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
    { The number of reserved items in the collection. Is >= Count to improve
      performance by reducing memory reallocations. }
    property Capacity: Integer read GetCapacity;

    property OnPropertyChangedEvent: IChangedPropertyEvent read GetOnPropertyChangedEvent;
    property OncollectionChangedEvent: IChangedCollectionEvent read GetOnCollectionChangedEvent;

    property Manager: IStrategyEventedObject read GetManager write SetManager;

  end;

implementation

uses
  System.SysUtils,

  MVVM.Utils,
  MVVM.Bindings;

{ TObservable }

constructor TObservable.Create(AManager: IStrategyEventedObject);
begin
  inherited Create;
  FManager := AManager;
end;

destructor TObservable.Destroy;
begin
  NotifyFree;
  inherited;
end;

function TObservable.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result     := FManager;
end;

function TObservable.GetOnFreeEvent: IFreeEvent;
begin
  Result := Manager.OnFreeEvent;
end;

function TObservable.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Result := Manager.OnPropertyChangedEvent;
end;

procedure TObservable.NotifyFree;
begin
  if Manager.IsAssignedFreeEvent then
    Manager.OnFreeEvent.Invoke(Self, Self);
end;

procedure TObservable.PropertyChanged(const APropertyName: String);
begin
  if Manager.IsAssignedPropertyChangedEvent then
    Manager.OnPropertyChangedEvent.Invoke(Self, APropertyName);
  if Assigned(Manager.BindingStrategy) then
    Manager.BindingStrategy.Notify(Self, APropertyName);
end;

procedure TObservable.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager;
end;

{ TObservableCollection<T> }

function TObservableCollection<T>.Add(const AItem: T): Integer;
var
  [weak]
  LPropertyChanged: INotifyChangedProperty;
begin
  Result := FList.Add(AItem);
  if Supports(AItem, INotifyChangedProperty, LPropertyChanged) then
    LPropertyChanged.OnPropertyChangedEvent.Add(DoItemPropertyChanged);
  DoCollectionChanged(TCollectionChangedAction.Add, AItem, Result);
  DoPropertyChanged('Count');
end;

procedure TObservableCollection<T>.AddRange(const ACollection: TEnumerable<T>);
var
  Index: Integer;
  Item: T;
  [weak]
  NPC: INotifyChangedProperty;
begin
  Index := FList.Count;
  FList.AddRange(ACollection);

  for Item in ACollection do
  begin
    if Supports(Item, INotifyChangedProperty, NPC) then
      NPC.OnPropertyChangedEvent.Add(DoItemPropertyChanged);

    DoCollectionChanged(TCollectionChangedAction.Add, Item, Index);
    Inc(Index);
  end;

  if Assigned(ACollection) then
    DoPropertyChanged('Count');
end;

procedure TObservableCollection<T>.AddRange(const AItems: array of T);
var
  I, Index: Integer;
  Item: T;
  [weak]
  NPC: INotifyChangedProperty;
begin
  Index := FList.Count;
  FList.AddRange(AItems);

  for I := 0 to Length(AItems) - 1 do
  begin
    Item := AItems[I];
    if Supports(Item, INotifyChangedProperty, NPC) then
      NPC.OnPropertyChangedEvent.Add(DoItemPropertyChanged);

    DoCollectionChanged(TCollectionChangedAction.Add, Item, Index);
    Inc(Index);
  end;

  if (Length(AItems) > 0) then
    DoPropertyChanged('Count');
end;

function TObservableCollection<T>.BinarySearch(const AItem: T; out AIndex: Integer; const AComparer: IComparer<T>): Boolean;
begin
  Result := FList.BinarySearch(AItem, AIndex, AComparer);
end;

function TObservableCollection<T>.BinarySearch(const AItem: T; out AIndex: Integer): Boolean;
begin
  Result := FList.BinarySearch(AItem, AIndex);
end;

procedure TObservableCollection<T>.Clear;
var
  I: Integer;
  Item: T;
  [weak]
  NPC: INotifyChangedProperty;
begin
  if (FList.Count > 0) then
  begin
    for I := 0 to FList.Count - 1 do
    begin
      Item := FList[I];
      if Supports(Item, INotifyChangedProperty, NPC) then
      begin
        NPC.OnPropertyChangedEvent.Remove(DoItemPropertyChanged);
        NPC := nil;
      end;
    end;
    FList.Clear;
    DoCollectionChanged(TCollectionChangedAction.Clear);
    DoPropertyChanged('Count');
  end;
end;

function TObservableCollection<T>.Contains(const AItem: T): Boolean;
begin
  Result := FList.Contains(AItem)
end;

constructor TObservableCollection<T>.Create(const AOwnsObjects: Boolean);
begin
  inherited Create;
  FList             := TObjectList<T>.Create;
  FList.OwnsObjects := AOwnsObjects;
end;

procedure TObservableCollection<T>.Delete(const AIndex: Integer);
var
  Item: T;
  [weak]
  NPC: INotifyChangedProperty;
begin
  Item := FList[AIndex];
  if Supports(Item, INotifyChangedProperty, NPC) then
  begin
    NPC.OnPropertyChangedEvent.Remove(DoItemPropertyChanged);

    { Set NPC to nil BEFORE removing item from list. The list owns the item, so
      deleting it frees the item, and NPC would be invalid and an Access
      Violation would happen when NPC would be cleaned up as it goes out of
      scope. }
    NPC := nil;
  end;

  FList.Delete(AIndex);
  DoCollectionChanged(TCollectionChangedAction.Delete, nil, AIndex);
  DoPropertyChanged('Count');
end;

procedure TObservableCollection<T>.DeleteRange(const AIndex, ACount: Integer);
var
  I: Integer;
  Item: T;
  [weak]
  NPC: INotifyChangedProperty;
begin
  for I := AIndex to AIndex + ACount - 1 do
  begin
    Item := FList[I];
    if Supports(Item, INotifyChangedProperty, NPC) then
    begin
      NPC.OnPropertyChangedEvent.Remove(DoItemPropertyChanged);
      NPC := nil;
    end;
  end;

  FList.DeleteRange(AIndex, ACount);
  for I := AIndex to AIndex + ACount - 1 do
    DoCollectionChanged(TCollectionChangedAction.Delete, nil, I);

  if (ACount > 0) then
    DoPropertyChanged('Count');
end;

destructor TObservableCollection<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TObservableCollection<T>.DoCollectionChanged(const AAction: TCollectionChangedAction; const AItem: TObject; const AItemIndex: Integer; const APropertyName: String);
var
  Args: TCollectionChangedEventArgs;
begin
  if Manager.IsAssignedCollectionChangedEvent then
  begin
    Args := TCollectionChangedEventArgs.Create(AAction, AItem, AItemIndex, APropertyName);
    Manager.OncollectionChangedEvent.Invoke(Self, Args);
  end;
end;

function TObservableCollection<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := FList.GetEnumerator;
end;

procedure TObservableCollection<T>.DoItemPropertyChanged(const ASender: TObject; const APropertyName: String);
begin
  DoCollectionChanged(TCollectionChangedAction.ItemChange, ASender, -1, APropertyName);
end;

procedure TObservableCollection<T>.DoPropertyChanged(const APropertyName: String);
begin
  if Manager.IsAssignedPropertyChangedEvent then
    Manager.OnPropertyChangedEvent.Invoke(Self, APropertyName);
  if Assigned(Manager.BindingStrategy) then
    Manager.BindingStrategy.Notify(Self, APropertyName);
end;

function TObservableCollection<T>.First: T;
begin
  Result := FList.First;
end;

function TObservableCollection<T>.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TObservableCollection<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TObservableCollection<T>.GetItem(const AIndex: Integer): T;
begin
  Result := FList[AIndex];
end;

function TObservableCollection<T>.GetManager: IStrategyEventedObject;
begin
  if (FManager = nil) then
    FManager := TStrategyEventedObject.Create(Self);
  Result     := FManager;
end;

function TObservableCollection<T>.GetOnCollectionChangedEvent: IChangedCollectionEvent;
begin
  Result := Manager.OncollectionChangedEvent;
end;

function TObservableCollection<T>.GetOnPropertyChangedEvent: IChangedPropertyEvent;
begin
  Result := Manager.OnPropertyChangedEvent;
end;

function TObservableCollection<T>.IndexOf(const AItem: T): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

function TObservableCollection<T>.IndexOfItem(const AItem: T; const ADirection: TDirection): Integer;
begin
  Result := FList.IndexOfItem(AItem, ADirection);
end;

procedure TObservableCollection<T>.Insert(const AIndex: Integer; const AItem: T);
var
  [weak]
  NPC: INotifyChangedProperty;
begin
  FList.Insert(AIndex, AItem);
  if Supports(AItem, INotifyChangedProperty, NPC) then
    NPC.OnPropertyChangedEvent.Add(DoItemPropertyChanged);
  DoCollectionChanged(TCollectionChangedAction.Add, AItem, AIndex);
  DoPropertyChanged('Count');
end;

procedure TObservableCollection<T>.InsertRange(const AIndex: Integer; const ACollection: TEnumerable<T>);
var
  Index: Integer;
  Item: T;
  [weak]
  NPC: INotifyChangedProperty;
begin
  Index := AIndex;
  FList.InsertRange(AIndex, ACollection);

  for Item in ACollection do
  begin
    if Supports(Item, INotifyChangedProperty, NPC) then
      NPC.OnPropertyChangedEvent.Add(DoItemPropertyChanged);

    DoCollectionChanged(TCollectionChangedAction.Add, Item, Index);
    Inc(Index);
  end;

  if Assigned(ACollection) then
    DoPropertyChanged('Count');
end;

procedure TObservableCollection<T>.InsertRange(const AIndex: Integer; const AItems: array of T);
var
  I, Index: Integer;
  Item: T;
  [weak]
  NPC: INotifyChangedProperty;
begin
  Index := AIndex;
  FList.InsertRange(AIndex, AItems);

  for I := 0 to Length(AItems) - 1 do
  begin
    Item := AItems[I];
    if Supports(Item, INotifyChangedProperty, NPC) then
      NPC.OnPropertyChangedEvent.Add(DoItemPropertyChanged);

    DoCollectionChanged(TCollectionChangedAction.Add, Item, Index);
    Inc(Index);
  end;

  if (Length(AItems) > 0) then
    DoPropertyChanged('Count');
end;

function TObservableCollection<T>.Last: T;
begin
  Result := FList.Last;
end;

function TObservableCollection<T>.LastIndexOf(const AItem: T): Integer;
begin
  Result := FList.LastIndexOf(AItem);
end;

function TObservableCollection<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TObservableCollection<T>.Remove(const AItem: T): Integer;
var
  [weak]
  NPC: INotifyChangedProperty;
begin
  if Supports(AItem, INotifyChangedProperty, NPC) then
  begin
    NPC.OnPropertyChangedEvent.Remove(DoItemPropertyChanged);
    NPC := nil;
  end;

  Result := FList.Remove(AItem);

  if (Result >= 0) then
  begin
    DoCollectionChanged(TCollectionChangedAction.Delete, nil, Result);
    DoPropertyChanged('Count');
  end;
end;

procedure TObservableCollection<T>.Reverse;
begin
  FList.Reverse;
  DoCollectionChanged(TCollectionChangedAction.Rearrange);
end;

procedure TObservableCollection<T>.SetItem(const AIndex: Integer; const Value: T);
begin
  FList[AIndex] := Value;
end;

procedure TObservableCollection<T>.SetManager(AManager: IStrategyEventedObject);
begin
  FManager := AManager
end;

procedure TObservableCollection<T>.Sort(const AComparer: IComparer<T>);
begin
  FList.Sort(AComparer);
  DoCollectionChanged(TCollectionChangedAction.Rearrange);
end;

procedure TObservableCollection<T>.Sort;
begin
  FList.Sort;
  DoCollectionChanged(TCollectionChangedAction.Rearrange);
end;

function TObservableCollection<T>.ToArray: TArray<T>;
begin
  Result := FList.ToArray;
end;

procedure TObservableCollection<T>.TrimExcess;
begin
  FList.TrimExcess;
end;

function TObservableCollection<T>._AddRef: Integer;
begin
  Result := -1;
end;

function TObservableCollection<T>._Release: Integer;
begin
  Result := -1;
end;

end.
