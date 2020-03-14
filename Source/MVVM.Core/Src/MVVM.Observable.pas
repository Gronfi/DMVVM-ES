unit MVVM.Observable;

interface

uses
  System.Generics.Collections,

  MVVM.Interfaces;

type
  TObservable = class abstract(TInterfacedObject, INotifyPropertyChanged, INotifyFree)
  private
    FEstrategiaBinding: IBindingStrategy;
  protected
  {$REGION 'Internal Declarations'}
    procedure PropertyChanged(const APropertyName: String); virtual;

    function GetBindingStrategy: IBindingStrategy;
    procedure SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);

    procedure NotifyFree; virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    { The destructor also sends a free notification to its subscribers. }
    destructor Destroy; override;
    property EstrategiaBinding: IBindingStrategy read GetBindingStrategy write SetBindingStrategy;
  end;

  TObservableCollection<T: class> = class(TEnumerable<T>, INotifyPropertyChanged, INotifyCollectionChanged)
  {$REGION 'Internal Declarations'}
  private
    FList: TObjectList<T>;
    FOnPropertyChanged: IgoPropertyChangedEvent;
    FOnCollectionChanged: IgoCollectionChangedEvent;
    function GetCapacity: Integer; inline;
    function GetCount: Integer; inline;
    function GetItem(const AIndex: Integer): T; inline;
    procedure SetItem(const AIndex: Integer; const Value: T); inline;
  private
    procedure DoItemPropertyChanged(const ASender: TObject;
      const APropertyName: String);
    procedure DoPropertyChanged(const APropertyName: String);
    procedure DoCollectionChanged(const AAction: TgoCollectionChangedAction;
      const AItem: TObject = nil; const AItemIndex: Integer = -1;
      const APropertyName: String = '');
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    { IgoNotifyPropertyChanged }
    function GetPropertyChangedEvent: IgoPropertyChangedEvent;
  protected
    { IgoNotifyCollectionChanged }
    function GetCollectionChangedEvent: IgoCollectionChangedEvent;
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
    function BinarySearch(const AItem: T; out AIndex: Integer;
      const AComparer: IComparer<T>): Boolean; overload; inline;

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
  end;

implementation

uses
  MVVM.Bindings;

{ TObservable }

destructor TObservable.Destroy;
begin
  NotifyFree;
  inherited;
end;

function TObservable.GetBindingStrategy: IBindingStrategy;
begin
  Result := FEstrategiaBinding;
end;

procedure TObservable.NotifyFree;
var
  LMsg: IMessage;
begin
  LMsg := TMessage_Object_Destroyed.Create(Self);
  LMsg.Queue;
end;

procedure TObservable.PropertyChanged(const APropertyName: String);
begin
  if Assigned(FEstrategiaBinding) then
    FEstrategiaBinding.Notify(Self, APropertyName);
end;

procedure TObservable.SetBindingStrategy(AEstrategiaBinding: IBindingStrategy);
begin
  if FEstrategiaBinding <> AEstrategiaBinding then
    FEstrategiaBinding := AEstrategiaBinding;
end;

end.
