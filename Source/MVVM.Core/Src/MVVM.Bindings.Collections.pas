unit MVVM.Bindings.Collections;

interface

uses
  MVVM.Interfaces,
  MVVM.Types;

type
{ Abstract base class for views that implement the IgoCollectionView
    interface.

    NOTE: This class implements an interface but is not reference counted. }
  TCollectionView = class abstract(TInterfacedObject, ICollectionView)
  {$REGION 'Internal Declarations'}
  private
    FSource  : ICollectionSource; // Reference
    FTemplate: TDataTemplateClass;
  private
    procedure AddItemsToView;
    procedure UpdateItemsInView;
  private
    procedure CollectionChanged(const ASender: TObject; const AArg: TCollectionChangedEventArgs);
  protected
    { IgoCollectionView }
    function GetSource: ICollectionSource;
    procedure SetSource(AValue: ICollectionSource);
    function GetTemplate: TDataTemplateClass;
    procedure SetTemplate(const AValue: TDataTemplateClass);
  {$ENDREGION 'Internal Declarations'}
  protected
    { Must be overridden to clear all items in the view.
      For example, when used with a TListBox, it would call the TListBox.Clear
      method. }
    procedure ClearItemsInView; virtual; abstract;

    { Must be overridden to inform the view that a batch of updates will follow.
      For example, when used with a TListBox, it would call the
      TListBox.BeginUpdate method. }
    procedure BeginUpdateView; virtual; abstract;

    { Must be overridden to inform the view that a batch of updates has ended.
      For example, when used with a TListBox, it would call the
      TListBox.EndUpdate method. }
    procedure EndUpdateView; virtual; abstract;

    { Must be overridden to add an item to the view.

      Parameters:
        AItem: the item to add to the view. The type of the item is the same as
          the type of the objects in the Source collection.

      For example, when used with a TListBox, it would create a TListBoxItem
      object, associate it with AItem, and add it to the list box. }
    procedure AddItemToView(const AItem: TObject); virtual; abstract;

    { Must be overridden to delete an item from the view.

      Parameters:
        AItemIndex: the index of the item to delete.

      For example, when used with a TListBox, it would delete item AItemIndex
      from the list box. }
    procedure DeleteItemFromView(const AItemIndex: Integer); virtual; abstract;

    { Must be overridden to update an item in the view.

      Parameters:
        AItem: the item that has changed. The type of the item is the same as
          the type of the objects in the Source collection.
        APropertyName: the name of the property of AItem that has changed.

      For example, when used with a TListBox, it would find the TListBoxItem
      associated with AItem and change one of its properties. }
    procedure UpdateItemInView(const AItem: TObject; const APropertyName: String); virtual; abstract;

    { Must be overridden to update all items in the view with new data.
      This method is called when the order of the items in the source collection
      has changed (for example, by sorting).

      The number of items is unchanged, so the view doesn't have to add or
      delete items. For example, when used with a TListBox, the list box would
      update all existing TListBoxItem objects with the properties from the
      corresponding items in the source collection. It also needs to
      re-associate each TListBoxItem with the corresponding item in the
      collection. }
    procedure UpdateAllItemsInView; virtual; abstract;
  public
    { The collection to show in the view. This can be any class derived from
      TEnumerable<T>, as long is T is a class type. You must typecast it to
      TgoCollectionSource to set the property.

      (In technical terms: TList<TPerson> is convariant, meaning that it is
      convertible to TList<TObject> if TPerson is a class. However, Delphi
      does not support covariance (and contravariance) with generics, so you
      need to typecast to TgoCollectionSource yourself.) }
    property Source: ICollectionSource read FSource write SetSource;

    { The class that is used as a template to map items in the collection to
      properties of items in the view. }
    property Template: TDataTemplateClass read FTemplate write FTemplate;
  end;

implementation

uses
  System.SysUtils;

{ TCollectionView }

procedure TCollectionView.AddItemsToView;
var
  Item: TObject;
begin
  if (FSource = nil) then
    Exit;

  BeginUpdateView;
  try
    for Item in Source do
      AddItemToView(Item);
  finally
    EndUpdateView;
  end;
end;

procedure TCollectionView.CollectionChanged(const ASender: TObject; const AArg: TCollectionChangedEventArgs);
begin
  if (FTemplate = nil) then
    Exit;

  case AArg.Action of
    TCollectionChangedAction.Add:
      AddItemToView(AArg.Item);

    TCollectionChangedAction.Delete:
      DeleteItemFromView(AArg.ItemIndex);

    TCollectionChangedAction.ItemChange:
      UpdateItemInView(AArg.Item, AArg.PropertyName);

    TCollectionChangedAction.Clear:
      ClearItemsInView;

    TCollectionChangedAction.Rearrange:
      begin
        BeginUpdateView;
        try
          UpdateAllItemsInView;
        finally
          EndUpdateView;
        end;
      end;
  end;
end;

function TCollectionView.GetSource: ICollectionSource;
begin
  Result := FSource;
end;

function TCollectionView.GetTemplate: TDataTemplateClass;
begin
  Result := FTemplate;
end;

procedure TCollectionView.SetSource(AValue: ICollectionSource);
//var
//  NCC: INotifyCollectionChanged;
begin
  if (AValue <> FSource) then
  begin
    { Unsubscribe from collection changed event of old source. }
//    if Assigned(FSource) and (Supports(FSource, INotifyCollectionChanged, NCC)) then
//      NCC.GetCollectionChangedEvent.Remove(CollectionChanged);

    FSource := AValue;
    if Assigned(FTemplate) then
    begin
      ClearItemsInView;
      AddItemsToView;
    end;

    { Subscribe to collection changed event of new source. }
//    if Assigned(FSource) and (Supports(FSource, IgoNotifyCollectionChanged, NCC)) then
//      NCC.GetCollectionChangedEvent.Add(CollectionChanged);
  end;
end;

procedure TCollectionView.SetTemplate(const AValue: TDataTemplateClass);
var
  PrevTemplate: TDataTemplateClass;
begin
  if (AValue <> FTemplate) then
  begin
    PrevTemplate := FTemplate;
    FTemplate := AValue;
    if Assigned(FTemplate) and Assigned(FSource) then
    begin
      if Assigned(PrevTemplate) then
        UpdateItemsInView
      else
      begin
        ClearItemsInView;
        AddItemsToView;
      end;
    end;
  end;
end;

procedure TCollectionView.UpdateItemsInView;
begin
  Assert(False);
end;

end.
